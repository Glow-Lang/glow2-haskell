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
          (map translateType $ Ast.ftParams fTy)
          (translateType $ Ast.ftRetType fTy),
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
          (translateType $ Ast.TTuple $ Ast.fptCaptures fPtrTy)
          (map translateType $ Ast.ftParams fTy)
          (translateType $ Ast.ftRetType fTy)

-- | Construct an LLVM type for a function pointer, given its (already
-- translated) capture list, parameter list, and return type. That is,
-- @'mkFunctionType' captures args ret@ constructs an LLVM type corresponding
-- to a function pointer with @captures@ as its closure, @args@ as its
-- argument list, and @ret@ as its return type.
mkFunctionType :: Type -> [Type] -> Type -> Type
mkFunctionType captures args ret =
  FunctionType
    { isVarArg = False,
      argumentTypes = captures : args,
      resultType = ret
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
