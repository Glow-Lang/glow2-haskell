{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Glow.Translate.LiftedFunctionsToBlockParam where

import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter, runWriterT, tell)
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Glow.Ast.BlockParamPassing as BP
import Glow.Ast.Classes (getMeta)
import Glow.Ast.Common
import qualified Glow.Ast.LiftedFunctions as LF
import Glow.Fresh (UnusedTable, fresh)
import Glow.Prelude

translateModule :: MonadState UnusedTable m => LF.Module (Set Id) -> m (BP.Module ())
translateModule (LF.Module stmts) = go stmts emptyBPModule
  where
    -- TODO(cleanup): this should basically just be a fold, but can't be
    -- because of the monadic effect. There's probably a less manual way
    -- to do this though.
    go [] m = pure m
    go (s : ss) m = addTopStmt m s >>= go ss

emptyBPModule :: BP.Module ()
emptyBPModule =
  BP.Module
    { BP.pTypes = Map.empty,
      BP.pInteractions = Map.empty,
      BP.pFuncs = Map.empty,
      BP.pInitBody =
        BP.Body
          { BP.bdyBlocks = Map.empty,
            BP.bdyStartBlock = moduleInitId
          }
    }

moduleInitId :: Id
moduleInitId = "$_module_init"

addTopStmt :: MonadState UnusedTable m => BP.Module () -> LF.TopStmt (Set Id) -> m (BP.Module ())
addTopStmt module_ = \case
  LF.TsBodyStmt _BodyStmt -> error "TODO: addTopStmt/BodyStmt"
  LF.TsDefType _meta _name _params _typ -> error "TODO: addTopStmt/DefType"
  LF.TsDefData _meta _name _params _variants -> error "TODO: addTopStmt/DefData"
  LF.TsDefInteraction _meta name interactionDef -> do
    interaction <- translateInteractionDef interactionDef
    pure
      module_
        { BP.pInteractions =
            Map.insert
              name
              interaction
              (BP.pInteractions module_)
        }
  LF.TsDefLambda _meta _participant _name _lambda -> undefined

translateInteractionDef ::
  MonadState UnusedTable m =>
  LF.InteractionDef (Set Id) ->
  m (BP.InteractionDef ())
translateInteractionDef LF.InteractionDef {..} = do
  body' <- translateBody idBody
  pure BP.InteractionDef {idBody = body', ..}

translateBody :: MonadState UnusedTable m => [LF.BodyStmt (Set Id)] -> m (BP.Body ())
translateBody stmts = do
  (startBlockId, blocks) <- runWriterT $ do
    (blockId, block) <- translateBlock ContReturn stmts
    tell [(blockId, block)]
    pure blockId
  pure
    BP.Body
      { BP.bdyStartBlock = startBlockId,
        BP.bdyBlocks = Map.fromList $ DList.toList blocks
      }

type MonadBlockGen m =
  ( MonadState UnusedTable m,
    MonadWriter (DList (Id, BP.Block ())) m
  )

translateBlock ::
  MonadBlockGen m =>
  Continuation ->
  [LF.BodyStmt (Set Id)] ->
  m (Id, BP.Block ())
translateBlock cc stmts = do
  blockName <- Id <$> fresh "block"
  (stmts', br) <- translateBodyStmts cc blockName stmts
  pure
    ( blockName,
      BP.Block
        { blkPartInfo = error "TODO: participant info",
          blkParams = Set.toList (LF.stmtsFreeVars stmts),
          blkStmts = stmts',
          blkBranch = br
        }
    )

-- | A 'Continuation' describes the destination the end of a block should jump to
data Continuation
  = -- | We should jump to another block. The first argument is the name of the block,
    -- and the second argument is a list of variables to pass to the block. The result
    -- of the code jumping to the block should be prepended to the list as the first
    -- argument.
    ContId Id [Id]
  | -- | We should return from the overall function.
    ContReturn

translateBodyStmts ::
  MonadBlockGen m =>
  Continuation ->
  Id ->
  [LF.BodyStmt (Set Id)] ->
  m ([BP.BodyStmt ()], BP.Branch ())
translateBodyStmts cc blockName = \case
  [] ->
    -- No more statements; just jump to the continuation.
    pure
      ( [],
        case cc of
          ContReturn -> BP.BrReturn () (LF.ExTuple [])
          ContId target args ->
            BP.BrJump
              ()
              BP.JumpTarget
                { jtName = target,
                  jtArgs = [TrexVar v | v <- args]
                }
      )
  LF.BsSwitch _ sw : stmts -> do
    -- A switch is a branch, so we need to split into multiple blocks. First,
    -- emit a block for the switch's continuation:
    contBlock@(contBlockId, BP.Block {blkParams = params}) <- translateBlock cc stmts
    tell [contBlock]
    -- Now translate the switch itself, handing it the appropriate continuation:
    br <- translateSwitch (ContId contBlockId params) sw
    pure ([], br)
  LF.BsPartStmt _ part stmt : stmts ->
    let rewrap = BP.BsPartStmt () part
     in case stmt of
          LF.PsLabel {} -> translateBodyStmts cc blockName stmts
          LF.PsDebugLabel {} -> translateBodyStmts cc blockName stmts
          LF.PsIgnore _ e -> translateLinear (rewrap (BP.PsIgnore () e)) stmts
          LF.PsDef _ v e -> translateLinear (rewrap (BP.PsDef () v e)) stmts
          LF.PsAssert _ e -> translateLinear (rewrap (BP.PsAssert () e)) stmts
          LF.PsRequire _ e -> translateLinear (rewrap (BP.PsRequire () e)) stmts
          LF.PsReturn _ e -> pure ([], BP.BrReturn () e)
          LF.PsSwitch meta sw ->
            -- Upgrade the switch to one for body statements, then re-use the logic for
            -- BsSwitch.
            let sw' = fmap (\s -> LF.BsPartStmt (getMeta s) part s) sw
             in translateBodyStmts cc blockName (LF.BsSwitch meta sw' : stmts)
  -- No control flow for these; just translate the one statement and recurse:
  LF.BsWithdraw _ part assets : stmts -> translateLinear (BP.BsWithdraw () part assets) stmts
  LF.BsDeposit _ part assets : stmts -> translateLinear (BP.BsDeposit () part assets) stmts
  -- FIXME(isd): double check understanding of part/var arguments.
  LF.BsPublish _ part var : stmts -> translateLinear (BP.BsPublish () part var) stmts
  where
    translateLinear stmt stmts = do
      (stmts', br) <- translateBodyStmts cc blockName stmts
      pure (stmt : stmts', br)

-- | @'translateSwitch' sw translateBranch@ translates the switch statement @sw@.
-- The return value is the branch instruction for the translated switch.
translateSwitch ::
  MonadBlockGen m =>
  Continuation ->
  LF.Switch (Set Id) (LF.BodyStmt (Set Id)) ->
  m (BP.Branch ())
translateSwitch cc sw = do
  cases <- for (LF.swBranches sw) $ \(pat, stmts) -> do
    (blockId, block) <- translateBlock cc stmts
    tell [(blockId, block)]
    pure
      BP.SwitchCase
        { scMatchValue = pat,
          scTarget =
            BP.JumpTarget
              { jtName = blockId,
                jtArgs = [TrexVar v | v <- BP.blkParams block]
              }
        }
  pure $ BP.BrSwitch () (LF.swArg sw) cases Nothing
