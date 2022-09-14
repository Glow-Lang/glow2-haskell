{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Parser for the output of the Scheme implementation's `anf`
-- phase.
module Glow.Gerbil.ParseAnf where

import qualified Data.ByteString.Char8 as BS8
import Glow.Ast.Common (Id (..))
import Glow.Gerbil.ParseCommon
import Glow.Gerbil.Types as Glow
import Glow.Prelude
import Text.SExpression as SExpr

parseModule :: SExpr -> [AnfStatement]
parseModule = \case
  List (Atom "@module" : Pair _startLabel _endLabel : statements) ->
    parseStatement <$> statements
  unknown ->
    error $ "Invalid module format: " <> show unknown

-- TODO: We should be able to autogen these from JSON
parseStatement :: SExpr -> AnfStatement
parseStatement = \case
  Builtin "@label" [Atom name] ->
    Label $ Id (BS8.pack name)
  Builtin "@debug-label" [Atom name] ->
    DebugLabel $ Id (BS8.pack name)
  Builtin "deftype" [Atom name, typeDefinition] ->
    DefineType (Id (BS8.pack name)) [] (parseType typeDefinition)
  Builtin "deftype" [List (Atom name : typeVariables), typeDefinition] ->
    DefineType (Id (BS8.pack name)) (parseQuoteId <$> typeVariables) (parseType typeDefinition)
  Builtin "defdata" (Atom name : variants) ->
    DefineDatatype (Id (BS8.pack name)) [] (parseVariant <$> variants)
  Builtin "defdata" (List (Atom name : typeVariables) : variants) ->
    DefineDatatype (Id (BS8.pack name)) (parseQuoteId <$> typeVariables) (parseVariant <$> variants)
  Builtin
    "def"
    [ Atom contractName,
      Builtin
        "@make-interaction"
        ( List
            [ Builtin
                "@record"
                [ Builtin "participants" [Builtin "@list" participantNames],
                  Builtin "assets" [Builtin "@list" assetNames]
                  ]
              ]
            : List argumentNames
            : Pair _startLabel _endLabel
            : body
          )
      ] ->
      DefineInteraction
        (Id (BS8.pack contractName))
        AnfInteractionDef
          { aidParticipantNames = Id . BS8.pack . parseName <$> participantNames,
            aidAssetNames = Id . BS8.pack . parseName <$> assetNames,
            aidArgumentNames = Id . BS8.pack . parseName <$> argumentNames,
            aidBody = parseStatement <$> body
          }
  Builtin "def" [Atom variableName, Builtin "λ" (List argNames : body)] ->
    DefineFunction (Id (BS8.pack variableName)) (Id . BS8.pack . parseName <$> argNames) (parseStatement <$> body)
  Builtin "def" [Atom variableName, sexpr] ->
    Define (Id (BS8.pack variableName)) (parseExpression sexpr)
  Builtin "ignore!" [sexpr] ->
    Ignore (parseExpression sexpr)
  Builtin "return" [sexpr] ->
    Return (parseExpression sexpr)
  Builtin "@" [Atom roleName, s] ->
    AtParticipant (Id (BS8.pack roleName)) (parseStatement s)
  Builtin "set-participant" [roleName] ->
    SetParticipant (Id . BS8.pack $ parseName roleName)
  Builtin "publish!" (Atom roleName : vars) ->
    Publish (Id (BS8.pack roleName)) (Id . BS8.pack <$> (parseName <$> vars))
  Builtin "deposit!" [Atom roleName, Builtin "@record" amounts] ->
    Deposit (Id (BS8.pack roleName)) (parseAssetMap amounts)
  Builtin "withdraw!" [Atom roleName, Builtin "@record" amounts] ->
    Withdraw (Id (BS8.pack roleName)) (parseAssetMap amounts)
  Builtin "require!" [Atom variableName] ->
    Require $ var variableName
  Builtin "assert!" [Atom variableName] ->
    Require $ var variableName
  Builtin "switch" (argumentExpression : cases) ->
    Switch (parseTrivialExpression argumentExpression) (parseSwitchCase <$> cases)
  unknown ->
    error $ "Unknown statement in contract body: " <> show unknown

parseSwitchCase :: SExpr -> (Pat, [AnfStatement])
parseSwitchCase = \case
  List (pat : body) -> (parsePattern pat, parseStatement <$> body)
  unknown -> error $ "expected a pattern and body in a switch case: " <> show unknown
