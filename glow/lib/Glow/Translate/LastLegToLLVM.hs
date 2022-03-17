module Glow.Translate.LastLegToLLVM where

import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as LT
import qualified Glow.Ast.LastLeg as Ast
import Glow.Prelude
import LLVM.AST
import LLVM.AST.AddrSpace
import LLVM.IRBuilder

translateModule :: MonadModuleBuilder m => Ast.Module -> m ()
translateModule m = do
  for_ (M.toList (Ast.modExterns m)) $ \(v, ty) ->
    -- TODO: this returns an Operand; store that somewhere so we
    -- can refer to it in generated code? Or maybe we'll just
    -- coordinate the name we pass in ourselves.
    declareExtern v ty
  pure () -- TODO: translate the actual code.

declareExtern :: MonadModuleBuilder m => Ast.Var -> Ast.Type -> m Operand
declareExtern v ty = case translateType ty of
  FunctionType {argumentTypes = args, resultType = ret, isVarArg = False} ->
    extern (varToName v) args ret
  _ ->
    error "TODO: extern non-functions."

varToName :: Ast.Var -> Name
varToName (Ast.Var txt) = mkName $ LT.unpack txt

translateType :: Ast.Type -> Type
translateType ty = case ty of
  Ast.TTuple tys ->
    ptr (tuple (map translateType tys))
  Ast.TFunc fTy ->
    -- A function is a pair of (function pointer, closure pointer)
    tuple
      [ mkFunctionType
          opaquePtr
          (Ast.ftParams fTy)
          (Ast.ftRetType fTy),
        opaquePtr
      ]
  Ast.TFPtr fPtrTy ->
    -- A function pointer takes its closure as the first argument. TODO:
    -- this doesn't match up with the TFunc case, where it's an opaque
    -- pointer -- conceptually that's correct, since in TFunc it's basically
    -- an exisistential, but not sure if we'll hit type errors from LLVM?
    -- will have to revist.
    let fTy = Ast.fptFuncType fPtrTy
     in mkFunctionType
          (translateType (Ast.TTuple (Ast.fptCaptures fPtrTy)))
          (Ast.ftParams fTy)
          (Ast.ftRetType fTy)

mkFunctionType :: Type -> [Ast.Type] -> Ast.Type -> Type
mkFunctionType captures args ret =
  FunctionType
    { isVarArg = False,
      argumentTypes = captures : map translateType args,
      resultType = translateType ret
    }

tuple :: [Type] -> Type
tuple elts =
  StructureType
    { isPacked = False,
      elementTypes = elts
    }

ptr :: Type -> Type
ptr ty =
  PointerType
    { pointerReferent = ty,
      pointerAddrSpace = AddrSpace 0
    }

opaquePtr :: Type
opaquePtr = ptr (IntegerType 8)
