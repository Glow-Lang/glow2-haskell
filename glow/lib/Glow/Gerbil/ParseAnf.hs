{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Parser for the output of the Scheme implementation's `anf`
-- phase.
module Glow.Gerbil.ParseAnf where

import Glow.Gerbil.Types as Glow
import Glow.Gerbil.ParseCommon
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
    Label $ bs8pack name
  Builtin "@debug-label" [Atom name] ->
    DebugLabel $ bs8pack name
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
          { aidParticipantNames = bs8pack . parseName <$> participantNames,
            aidAssetNames = bs8pack . parseName <$> assetNames,
            aidArgumentNames = bs8pack . parseName <$> argumentNames,
            aidBody = parseStatement <$> body
          }
  Builtin "def" [Atom variableName, Builtin "λ" (Atom argName : body)] ->
    DefineFunction (bs8pack variableName) (bs8pack argName) (parseStatement <$> body)
  Builtin "def" [Atom variableName, sexpr] ->
    Define (bs8pack variableName) (parseExpression sexpr)
  Builtin "ignore!" [sexpr] ->
    Ignore (parseExpression sexpr)
  Builtin "return" [sexpr] ->
    Return (parseTrivialExpression sexpr)
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
  Builtin "switch" (Atom _argumentExpression : _patterns) ->
    error "switch statements are not supported"
  unknown ->
    error $ "Unknown statement in contract body: " <> show unknown
