{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | Parser for the output of the Scheme implementation's `project`
-- phase.
module Glow.Gerbil.ParseProject where

import Control.Monad.State
import qualified Data.ByteString.Base64 as B16
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Either (fromRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Glow.Gerbil.Client.Types
  ( CreateParams (..),
    MoveParams (..),
    RawCreateParams (..),
    RawMoveParams (..),
  )
import Glow.Gerbil.ParseCommon
import Glow.Gerbil.Types as Glow
import Glow.Prelude
import Prettyprinter
import Text.Megaparsec hiding (Label, State)
import Text.SExpression as SExpr
import Prelude (init, last, lookup)

-- TODO tests for toplevel parsers
-- TODO handle / propagate error case
parseRawCreateParams :: RawCreateParams -> CreateParams
parseRawCreateParams rawParams =
  let contractSexpr = fromRight (List []) $ parse (parseSExpr def) "" (source rawParams)
      initialVarSexpr = fromRight (List []) $ parse (parseSExpr def) "" (initialVariableMap rawParams)
      participants' = _participants program
      arguments' = parseVariableMap Map.empty initialVarSexpr
      participantKeyMap = Map.fromList $ map (getKeyMaps arguments') participants'
      consensusProgram = _consensusProgram program
      program = extractPrograms (parseModule contractSexpr)
   in -- TODO do we need/even support participant contracts???
      CreateParams
        { datatypes = Map.empty, -- TODO properly handle datatypes in contract header
          participants = participantKeyMap, -- participants' -- TODO pass in public keys, extract from participants
          arguments = arguments', -- includes participants
          contract = consensusProgram,
          timeoutLength = rawTimeoutLength rawParams
        }
  where
    getKeyMaps m p = (p, pubKey)
      where
        -- TODO: Properly handle erroring states
        pubKey = case fromMaybe (error "missing key!") (Map.lookup p m) of
          PubKey pk -> pk
          _ -> error "not a public key!"

parseRawMoveParams :: RawMoveParams -> MoveParams
parseRawMoveParams rawParams =
  let rawVariableMapSexpr = fromRight (List []) $ parse (parseSExpr def) "" (rawVariableMap rawParams)
      variableMap' = parseVariableMap Map.empty rawVariableMapSexpr
   in MoveParams
        { variableMap = variableMap',
          entryPoint = rawEntryPoint rawParams
        }

-- NOTE: to test:
-- > cabal v2-repl
-- > import Parser
-- > parseCommandPath "./contract/test/assets/project.sexp"
parseCommandPath :: FilePath -> IO ()
parseCommandPath filePath = do
  contractSource <- readFile filePath
  putStrLn $ case parse (parseSExpr def) filePath contractSource of
    Right contractSexpr ->
      let program = extractPrograms (parseModule contractSexpr)
       in show $
            "Participants:" <+> prettyList (show <$> _participants program) <> line
              <> "Arguments:" <+> prettyList (show <$> _arguments program)
              <> line
              <> line
              <> "Consensus program:" <+> line
              <> prettyContract (_consensusProgram program)
              <> line
              <> vsep (fmap (\(participant, contract) -> pretty (BS8.unpack participant) <> " program:" <+> line <> prettyContract contract) (Map.toList $ _participantPrograms program))
    Left err ->
      errorBundlePretty err

parseCommandDebug :: IO ()
parseCommandDebug = do
  filePath : _ <- getArgs
  contractSource <- readFile filePath
  putStrLn $ case parse (parseSExpr def) filePath contractSource of
    Right contractSexpr ->
      let program = extractPrograms (parseModule contractSexpr)
       in show $
            "Participants:" <+> prettyList (show <$> _participants program) <> line
              <> "Arguments:" <+> prettyList (show <$> _arguments program)
              <> line
              <> line
              <> "Consensus program:" <+> line
              <> prettyContract (_consensusProgram program)
              <> line
              <> vsep (fmap (\(participant, contract) -> pretty (BS8.unpack participant) <> " program:" <+> line <> prettyContract contract) (Map.toList $ _participantPrograms program))
    Left err ->
      errorBundlePretty err

prettyContract :: GlowProjectContract -> Doc ann
prettyContract glowContract =
  indent 2 $
    vsep $
      ( \(k, (stmts, maybeExit)) ->
          "->" <+> viaShow k <> line
            <> indent 4 (vsep (viaShow <$> stmts))
            <> line
            <> maybe "" (\exit -> "<-" <+> viaShow exit) maybeExit
      )
        <$> Map.toList glowContract

parseModule :: SExpr -> [ProjectStatement]
parseModule = \case
  List (Atom "@module" : Pair _startLabel _endLabel : statements) ->
    parseStatement <$> statements
  unknown ->
    error $ "Invalid module format: " <> show unknown

-- TODO: We should be able to autogen these from JSON
parseStatement :: SExpr -> ProjectStatement
parseStatement = \case
  Builtin "@label" [Atom name] ->
    Label $ BS8.pack name
  Builtin "@debug-label" [Atom name] ->
    DebugLabel $ BS8.pack name
  Builtin "deftype" [Atom name, typeDefinition] ->
    DefineType (BS8.pack name) [] (parseType typeDefinition)
  Builtin "deftype" [List (Atom name : typeVariables), typeDefinition] ->
    DefineType (BS8.pack name) (parseQuoteName <$> typeVariables) (parseType typeDefinition)
  Builtin "defdata" (Atom name : variants) ->
    DefineDatatype (BS8.pack name) [] (parseVariant <$> variants)
  Builtin "defdata" (List (Atom name : typeVariables) : variants) ->
    DefineDatatype (BS8.pack name) (parseQuoteName <$> typeVariables) (parseVariant <$> variants)
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
            : interactions
          )
      ] ->
      DefineInteraction
        (BS8.pack contractName)
        ProjectInteractionDef
          { pidParticipantNames = BS8.pack . parseName <$> participantNames,
            pidAssetNames = BS8.pack . parseName <$> assetNames,
            pidArgumentNames = BS8.pack . parseName <$> argumentNames,
            pidInteractions = parseInteraction <$> interactions
          }
  Builtin "def" [Atom variableName, Builtin "λ" (List argNames : Pair _startLabel _endLabel : body)] ->
    DefineFunction (BS8.pack variableName) (BS8.pack . parseName <$> argNames) (parseStatement <$> body)
  Builtin "def" [Atom variableName, sexpr] ->
    Define (BS8.pack variableName) (parseExpression sexpr)
  Builtin "ignore!" [sexpr] ->
    Ignore (parseExpression sexpr)
  Builtin "return" [sexpr] ->
    Return (parseExpression sexpr)
  Builtin "set-participant" [roleName] ->
    SetParticipant (var $ parseName roleName)
  Builtin "expect-deposited" [Builtin "@record" amounts] ->
    Deposit (var "ACTIVE") (parseAssetMap amounts)
  Builtin "expect-withdrawn" [Atom roleName, Builtin "@record" amounts] ->
    Withdraw (var roleName) (parseAssetMap amounts)
  Builtin "add-to-publish" _ ->
    Require $ Explicit (Boolean True)
  Builtin "add-to-deposit" [Builtin "@record" amounts] ->
    Deposit (var "ACTIVE") (parseAssetMap amounts)
  Builtin "consensus:withdraw" [Atom roleName, Builtin "@record" amounts] ->
    Withdraw (var roleName) (parseAssetMap amounts)
  -- FIXME Compiler to plutus IR should separate consensus from participant statements.
  Builtin "participant:withdraw" [Atom roleName, Builtin "@record" amounts] ->
    Withdraw (var roleName) (parseAssetMap amounts)
  -- NOTE: Does not seem to be used in the latest project.sexp output
  -- FIXME: Make sure this is not used and cleanup
  -- Builtin "add-to-withdraw" [Atom roleName, Atom amountName] ->
  --   AddToWithdraw (var roleName) (var amountName)

  Builtin "require!" [Atom variableName] ->
    Require $ var variableName
  Builtin "assert!" [Atom variableName] ->
    Require $ var variableName
  Builtin "switch" (argumentExpression : cases) ->
    Switch (parseTrivialExpression argumentExpression) (parseSwitchCase <$> cases)
  unknown ->
    error $ "Unknown statement in contract body: " <> show unknown

parseSwitchCase :: SExpr -> (Pattern, [ProjectStatement])
parseSwitchCase = \case
  List (pat : body) -> (parsePattern pat, parseStatement <$> body)
  unknown -> error $ "expected a pattern and body in a switch case: " <> show unknown

parseInteraction :: SExpr -> (ByteString, [ProjectStatement])
parseInteraction = \case
  Builtin participantName statements ->
    (BS8.pack participantName, parseStatement <$> statements)
  List (Bool False : statements) ->
    (BS8.pack "consensus", parseStatement <$> statements)
  unknown ->
    error $ "Invalid participant interaction expression: " <> show unknown

parseVariableMap :: DatatypeMap -> SExpr -> VariableMap
parseVariableMap _datatypes = \case
  List pairs ->
    Map.fromList $ parsePair <$> pairs
  unknown ->
    error $ "Invalid map expression: " <> show unknown
  where
    parsePair = \case
      ConsList [Atom varName] varGlowValue ->
        let value = case varGlowValue of
              Number number ->
                Integer number
              SExpr.String string ->
                Glow.ByteString (BS8.pack string)
              Bool bool ->
                Boolean bool
              List [Atom cons, SExpr.String val] ->
                parseDatatype cons val
              unknown ->
                error $ "Invalid variable value: " <> show unknown
         in (BS8.pack varName, value)
      List [Atom varName, Atom cons, SExpr.String val] ->
        (BS8.pack varName, parseDatatype cons val)
      unknown ->
        error $ "Invalid pair expression: " <> show unknown

    parseDatatype "signature" rawSignature =
      case B16.decode (BS8.pack rawSignature) of
        Right signature ->
          Signature (LedgerSignature signature)
        Left err ->
          error $ "Invalid signature value: " <> err
    parseDatatype "pub-key" rawPubKey =
      PubKey (fromString rawPubKey)
    parseDatatype cons _rawVal =
      error $ "Unknown constructor: " <> cons

data GlowProgram = GlowProgram
  { _participants :: [ByteString],
    _arguments :: [ByteString],
    _consensusProgram :: GlowProjectContract,
    _participantPrograms :: Map ByteString GlowProjectContract
  }
  deriving (Show)

extractPrograms :: [ProjectStatement] -> GlowProgram
extractPrograms statements =
  execState (traverse processHeaderStatement statements) initialState
  where
    initialState =
      GlowProgram
        { _participants = [],
          _arguments = [],
          _consensusProgram = Map.empty,
          _participantPrograms = Map.empty
        }

    processHeaderStatement = \case
      DefineInteraction
        _contractName
        ProjectInteractionDef
          { pidParticipantNames = participants,
            pidArgumentNames = arguments,
            pidAssetNames = _, -- TODO: do something with assets.
            pidInteractions = interactions
          } -> do
          modify $ \program -> program {_participants = participants, _arguments = arguments}
          let consensusProgram = processProgram "consensus" interactions
          let participantPrograms = (\participant -> processProgram participant interactions) <$> participants
          modify $ \program ->
            program
              { _consensusProgram = snd consensusProgram,
                _participantPrograms = Map.fromList participantPrograms
              }
      _ ->
        pure ()

    processProgram name interactions =
      case lookup name interactions of
        Just consensusStatements ->
          let (_, _, result) = execState (traverse processBodyStatement consensusStatements) (Nothing, "begin0", Map.empty)
           in (name, result)
        Nothing ->
          error $ "Contract is missing " <> BS8.unpack name <> " code."

    processBodyStatement = \case
      SetParticipant newParticipant ->
        setParticipant newParticipant
      stmt ->
        addStatement stmt

    setParticipant :: GlowValueRef -> State (Maybe GlowValueRef, ExecutionPoint, Map ExecutionPoint ([ProjectStatement], Maybe ExecutionPoint)) ()
    setParticipant newParticipant =
      modify $ \cur@(curParticipant, curLabel, contract) ->
        if curParticipant == Just newParticipant
          then cur
          else case Map.lookup curLabel contract of
            Just (stmts, Nothing) ->
              case last stmts of
                Label lastLabel ->
                  let newContract =
                        contract
                          & Map.insert curLabel (init stmts, Just lastLabel)
                          & Map.insert lastLabel ([SetParticipant newParticipant], Nothing)
                   in (Just newParticipant, lastLabel, newContract)
                _ ->
                  error "Change of participant with no preceding label."
            Just (_, Just _) ->
              error "Invalid transition state"
            Nothing ->
              (Just newParticipant, curLabel, contract & Map.insert curLabel ([SetParticipant newParticipant], Nothing))

    addStatement :: ProjectStatement -> State (Maybe GlowValueRef, ExecutionPoint, Map ExecutionPoint ([ProjectStatement], Maybe ExecutionPoint)) ()
    addStatement stmt =
      modify $ \(curParticipant, curLabel, contract) ->
        let newContract = case Map.lookup curLabel contract of
              Just (stmts, exitPoints) ->
                Map.insert curLabel (stmts <> [stmt], exitPoints) contract
              Nothing ->
                contract
         in (curParticipant, curLabel, newContract)
