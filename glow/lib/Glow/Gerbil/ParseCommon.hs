{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Glow.Gerbil.ParseCommon -- (pattern Builtin, pattern Pair)
where

import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Glow.Gerbil.Types as Glow
import Glow.Prelude
import Text.SExpression as SExpr

pattern Builtin :: String -> [SExpr] -> SExpr
pattern Builtin head tail = List (Atom head : tail)

pattern Pair :: String -> String -> SExpr
pattern Pair fst snd = List [Atom fst, Atom snd]

------------------------------------------------------------

parseTypeTable :: SExpr -> Map ByteString Type
parseTypeTable = parseTable parseType

parseTable :: (SExpr -> a) -> SExpr -> Map ByteString a
parseTable p (List (Atom "hash" : kvs)) = Map.fromList $ map (parseKV p) kvs
parseTable _ sexp = error $ "parseTable: S-expression is not a hash map: " <> show sexp

parseKV :: (SExpr -> a) -> SExpr -> (ByteString, a)
parseKV p (List [Atom k, v]) = (bs8pack k, p v)
parseKV p sexp =
  ( bs8pack $ "parseKV: S-expression is not a key-value pair: " <> show sexp,
    (p (List []))
  )

parseType :: SExpr -> Type
parseType (List [Atom "type:arrow", List (Atom "@list" : params), result]) =
  TyArrow (map parseType params) (parseType result)
parseType (List [Atom "type:name", List [Atom "quote", Atom name]]) =
  TyName (bs8pack name)
parseType (List [Atom "type:name-subtype", List [Atom "quote", Atom name], typ]) =
  TyNameSubtype (bs8pack name) (parseType typ)
parseType (List [Atom "type:tuple", List (Atom "@list" : elts)]) =
  TyTuple (map parseType elts)
parseType sexp =
  TyUnknown (bs8pack $ show sexp)

parseAssetMap :: [SExpr] -> AssetMap
parseAssetMap = Map.fromList . map parseField
  where
    parseField (Builtin name [Atom amountName]) = (bs8pack name, var amountName)
    parseField field = error $ "Malformed field in asset map: " <> show field

parseExpression :: SExpr -> Expression
parseExpression = \case
  Builtin "expect-published" [variableName] ->
    ExpectPublished (bs8pack $ parseName variableName)
  Builtin "@app" [Atom "isValidSignature", Atom roleName, Atom digestVariableName, Atom signatureVariableName] ->
    IsValidSignature (var roleName) (var digestVariableName) (var signatureVariableName)
  Builtin "sign" [Atom _variableName] ->
    NoOp
  unknown ->
    error $ "Unknown expression in contract body: " <> show unknown

var :: String -> GlowValueRef
var = Variable . bs8pack

bs8pack :: String -> ByteString
bs8pack = WrappedByteString . LBS8.pack

bs8unpack :: ByteString -> String
bs8unpack = LBS8.unpack . toLBS

parseName :: SExpr -> String
parseName = \case
  Atom name ->
    name
  List [Atom "quote", Atom name] ->
    name
  unknown ->
    error $ "Invalid name expression: " <> show unknown

parseTrivialExpression :: SExpr -> GlowValueRef
parseTrivialExpression = \case
  Atom name            -> var name
  List [Atom "@tuple"] -> Explicit Unit
  unknown              ->
    error $ "Unknown expression in trivial-expression position: " <> show unknown
