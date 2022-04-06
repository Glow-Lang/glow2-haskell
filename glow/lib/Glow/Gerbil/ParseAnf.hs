{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Parser for the output of the Scheme implementation's `anf`
-- phase.
module Glow.Gerbil.ParseAnf where

import qualified Data.ByteString.Lazy.Char8 as LBS8
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
    Label $ LBS8.pack name
  Builtin "@debug-label" [Atom name] ->
    DebugLabel $ LBS8.pack name
  Builtin "deftype" [Atom _name, _typeDefinition] ->
    error "monomorphic type not supported"
  Builtin "deftype" [List (Atom _name : _typeVariables), _typeDefinition] ->
    error "polymorphic type not supported"
  Builtin "defdata" [Atom _name, _datatypeDefinition] ->
    error "monomorphic datatype not supported"
  Builtin "defdata" [List (Atom _name : _typeVariables), _datatypeDefinition] ->
    error "polymorphic datatype not supported"
  Builtin
    "def"
    [ Atom _contractName,
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
        AnfInteractionDef
          { aidParticipantNames = LBS8.pack . parseName <$> participantNames,
            aidAssetNames = LBS8.pack . parseName <$> assetNames,
            aidArgumentNames = LBS8.pack . parseName <$> argumentNames,
            aidBody = parseStatement <$> body
          }
  Builtin "def" [Atom variableName, Builtin "λ" (List argNames : body)] ->
    DefineFunction (LBS8.pack variableName) (LBS8.pack . parseName <$> argNames) (parseStatement <$> body)
  Builtin "def" [Atom variableName, sexpr] ->
    Define (LBS8.pack variableName) (parseExpression sexpr)
  Builtin "ignore!" [sexpr] ->
    Ignore (parseExpression sexpr)
  Builtin "return" [sexpr] ->
    Return (parseExpression sexpr)
  Builtin "@" [Atom roleName, s] ->
    AtParticipant (var roleName) (parseStatement s)
  Builtin "set-participant" [roleName] ->
    SetParticipant (var $ parseName roleName)
  Builtin "publish!" (Atom roleName : vars) ->
    Publish (var roleName) (var <$> (parseName <$> vars))
  Builtin "deposit!" [Atom roleName, Builtin "@record" amounts] ->
    Deposit (var roleName) (parseAssetMap amounts)
  Builtin "withdraw!" [Atom roleName, Builtin "@record" amounts] ->
    Withdraw (var roleName) (parseAssetMap amounts)
  Builtin "require!" [Atom variableName] ->
    Require $ var variableName
  Builtin "assert!" [Atom variableName] ->
    Require $ var variableName
  Builtin "switch" (argumentExpression : cases) ->
    Switch (parseTrivialExpression argumentExpression) (parseSwitchCase <$> cases)
  unknown ->
    error $ "Unknown statement in contract body: " <> show unknown

parseSwitchCase :: SExpr -> (Pattern, [AnfStatement])
parseSwitchCase = \case
  List (pat : body) -> (parsePattern pat, parseStatement <$> body)
  unknown -> error $ "expected a pattern and body in a switch case: " <> show unknown
