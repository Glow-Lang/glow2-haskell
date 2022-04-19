{-# LANGUAGE LambdaCase #-}

-- | Translate Glow.Gerbil.Types Type to Glow.Ast.
module Glow.Translate.GerbilTypeToLastLeg where

import Data.ByteString (ByteString)
import qualified Glow.Ast.LastLeg as GAL
import Glow.Gerbil.Types as GGT
import Glow.Prelude

translateType :: GGT.Type -> GAL.Type
translateType = \case
  TyArrow ins out ->
    -- ftEffectful is False because it's not actually about effects,
    -- it's about transaction boundaries or something like that?
    GAL.TFunc (GAL.FuncType (translateType <$> ins) (translateType out) False)
  TyName name -> translateTypeName name
  TyNameSubtype name _ -> translateTypeName name
  TyTuple elements -> GAL.TTuple (translateType <$> elements)
  TyUnknown bs -> error ("Glow.Translate.GerbilTypeToLastLeg.translateType: unknown type " <> show bs)

-- TODO:
-- - [ ] type:var
-- - [ ] type:app
-- - [ ] type:record

translateTypeName :: ByteString -> GAL.Type
translateTypeName name =
  error ("TODO: Glow.Translate.GerbilTypeToLastLeg.translateTypeName: " <> show name)
