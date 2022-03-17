module Glow.Translate.LastLegToLLVM where

import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as LT
import qualified Glow.Ast.LastLeg as Ast
import Glow.Prelude
import LLVM.AST
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
declareExtern v ty = case ty of
  Ast.TFPtr fPtrTy -> declareExternFPtr v fPtrTy
  Ast.TFunc fTy -> declareExternFunc v fTy
  _ -> error $ "TODO: declareExtern with type = " <> show ty

declareExternFPtr :: MonadModuleBuilder m => Ast.Var -> Ast.FPtrType -> m Operand
declareExternFPtr _ _fPtrTy = undefined

declareExternFunc :: MonadModuleBuilder m => Ast.Var -> Ast.FuncType -> m Operand
declareExternFunc v fTy
  | Ast.ftEffectful fTy = error "TODO: extern effectful functions"
  | otherwise =
      extern
        (varToName v)
        (map translateType (Ast.ftParams fTy))
        (translateType (Ast.ftRetType fTy))

varToName :: Ast.Var -> Name
varToName (Ast.Var txt) = mkName $ LT.unpack txt

translateType = undefined
