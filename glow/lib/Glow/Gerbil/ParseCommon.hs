{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Glow.Gerbil.ParseCommon where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Glow.Ast.Common (Id(..), Constant(..), TrivExpr(..), cInteger)
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
parseTable p (List (Atom "hash" : kvs)) = Map.fromList $ mapMaybe (parseKV p) kvs
parseTable _ sexp = error $ "parseTable: S-expression is not a hash map: " <> show sexp

parseKV :: (SExpr -> a) -> SExpr -> Maybe (ByteString, a)
parseKV p (List [Atom k, v]) = Just (BS8.pack k, p v)
parseKV _ (List [_, _]) = Nothing
parseKV _ sexp = error $ "parseKV: S-expression is not a key-value pair: " <> show sexp

parseQuoteName :: SExpr -> ByteString
parseQuoteName (List [Atom "quote", Atom name]) = BS8.pack name
parseQuoteName sexp = error $ "parseQuoteAtom: S-expression is not a quoted atom: " <> show sexp

parseType :: SExpr -> Type
parseType (List [Atom "type:arrow", List (Atom "@list" : params), result]) =
  TyArrow (map parseType params) (parseType result)
parseType (List [Atom "type:name", List [Atom "quote", Atom name]]) =
  TyName (BS8.pack name)
parseType (List [Atom "type:name-subtype", List [Atom "quote", Atom name], typ]) =
  TyNameSubtype (BS8.pack name) (parseType typ)
parseType (List [Atom "type:tuple", List (Atom "@list" : elts)]) =
  TyTuple (map parseType elts)
parseType (List [Atom "type:var", List [Atom "quote", Atom name]]) =
  TyVar (BS8.pack name)
parseType (List [Atom "type:app", fun, List (Atom "@list" : args)]) =
  TyApp (parseType fun) (parseType <$> args)
parseType (List [Atom "type:record", List (Atom "symdict" : entries)]) =
  TyRecord (Map.fromList [(BS8.pack k, parseType v) | List [Atom k, v] <- entries])
parseType sexp =
  TyUnknown (BS8.pack $ show sexp)

parseVariant :: SExpr -> Variant
parseVariant (List (Atom name : fields)) = Variant (BS8.pack name) (parseType <$> fields)
parseVariant sexp = error $ "parseVariant: S-expression is not a datatype variant: " <> show sexp

parseAssetMap :: [SExpr] -> AssetMap
parseAssetMap = Map.fromList . map parseField
  where
    parseField (Builtin name [Atom amountName]) = (BS8.pack name, var amountName)
    parseField field = error $ "Malformed field in asset map: " <> show field

parseExpression :: SExpr -> Expression
parseExpression = \case
  Builtin "expect-published" [Builtin "quote" [variableName]] ->
    ExpectPublished (BS8.pack $ parseName variableName)
  Builtin "@app" (fun : args) ->
    AppExpr (parseTrivialExpression fun) (parseTrivialExpression <$> args)
  Builtin "==" [a, b] ->
    EqlExpr (parseTrivialExpression a) (parseTrivialExpression b)
  Builtin "sign" [arg] ->
    Sign (parseTrivialExpression arg)
  Builtin "digest" args ->
    Digest (parseTrivialExpression <$> args)
  Builtin "input" [typ, tag] ->
    Input (parseType typ) (parseTrivialExpression tag)
  u@(Builtin "@tuple" []) -> TrvExpr (parseTrivialExpression u)
  v@(Number _) -> TrvExpr (parseTrivialExpression v)
  s@(String _) -> TrvExpr (parseTrivialExpression s)
  x@(Atom _) -> TrvExpr (parseTrivialExpression x)
  unknown ->
    error $ "Unknown expression in contract body: " <> show unknown

var :: String -> TrivExpr
var = TrexVar . Id . BS8.pack

parseName :: SExpr -> String
parseName = \case
  Atom name ->
    name
  List [Atom "quote", Atom name] ->
    name
  unknown ->
    error $ "Invalid name expression: " <> show unknown

parseTrivialExpression :: SExpr -> TrivExpr
parseTrivialExpression = \case
  Atom name -> var name
  List [Atom "@tuple"] -> TrexConst CUnit
  Bool b -> TrexConst (CBool b)
  Number n -> TrexConst (cInteger n)
  String s -> TrexConst (CByteString (BS8.pack s))
  unknown ->
    error $ "Unknown expression in trivial-expression position: " <> show unknown

parsePattern :: SExpr -> Pattern
parsePattern = \case
  Bool b -> ValPat (CBool b)
  Number n -> ValPat (cInteger n)
  unknown -> error $ "Unknown switch pattern: " <> show unknown
