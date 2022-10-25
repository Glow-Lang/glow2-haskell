-- |
-- Module      : Glow.Ast.BlockParamPassing
-- Description : Blocks with parameters, jumps pass arguments
--
-- An intermediate layer between the 'Glow.Ast.LiftedFunctions' layer
-- and the 'Glow.Ast.LowLevel' layer, which breaks up the bodies and
-- switch statements into 'Block's with parameters and 'JumpTarget's
-- that pass arguments for those.
module Glow.Ast.BlockParamPassing where

import Data.Map.Strict (Map)
import Glow.Ast.Common
import Glow.Ast.LiftedFunctions (Expr)
import Glow.Gerbil.Types (Pat, Record, Type, Variant)
import Glow.Prelude

-- |
-- A 'Module' has type definitions, interaction definitions,
-- function definitions, and an initilization body.
-- The initialization body has the top-level variable
-- definitions.
--
-- In most glow programs, there will be a single interaction
-- definition that acts as the "entry point", but for a one
-- used as a library module there may be multiple interaction
-- definitions.
data Module a = Module
  { pTypes :: Map Id (TypeDef a),
    pInteractions :: Map Id (InteractionDef a),
    pFuncs :: Map Id (FuncDef a),
    pInitBody :: Body a
  }
  deriving (Show, Read, Eq)

-- |
-- A 'Body' has a control-flow graph of blocks with a starting
-- block label.
data Body a = Body
  { bdyBlocks :: Map Id (Block a),
    bdyStartBlock :: Id
  }
  deriving (Show, Read, Eq)

-- |
-- A 'Block' is either public or private.
--
--  - If public, the 'blkPartInfo' has 'BpiPublic' with the
--    set of active participants, and the 'blkStmts' must only
--    have those active participants in the 'BsPartStmt's,
--    deposits, and publish statements inside.
--
--  - If private, the 'blkPartInfo' has 'BpiPrivate' with the
--    participant it's private to and an eventual destination.
--    The 'blkStmts' of a private block must only be
--    'BsPartStmt's with 'Just' the participant it's private
--    to, and each target in the 'blkBranch' must either go to
--    the eventual destination, or go another private block
--    for the same participant and the same eventual
--    destination.
data Block a = Block
  { blkPartInfo :: BlockParticipantInfo,
    blkParams :: [Id],
    -- | if the block is private this should only have BsPartStmt with just that participant
    blkStmts :: [BodyStmt a],
    blkBranch :: Branch a
  }
  deriving (Show, Read, Eq)

data BlockParticipantInfo
  = -- | Public with a set of active participants
    BpiPublic [Id]
  | -- | Private to a single participant, with a single eventual public destination block
    BpiPrivate Id EventualDestination
  deriving (Show, Read, Eq)

data EventualDestination
  = -- | Eventual return for participant-private functions only
    EpdReturn
  | -- | Eventual jump to the given public block label
    EpdJump Id
  deriving (Show, Read, Eq)

data Branch a
  = BrReturn a Expr
  | BrJump a JumpTarget
  | BrSwitch a TrivExpr [SwitchCase] (Maybe JumpTarget)
  deriving (Show, Read, Eq)

data JumpTarget = JumpTarget
  { jtName :: Id,
    jtArgs :: [TrivExpr]
  }
  deriving (Show, Read, Eq)

data SwitchCase = SwitchCase
  { scMatchValue :: Pat,
    scTarget :: JumpTarget
  }
  deriving (Show, Read, Eq)

data TypeDef a
  = TdDefType a [Id] Type
  | TdDefData a [Id] [Variant]
  deriving (Show, Read, Eq)

data InteractionDef a = InteractionDef
  { idParticipants :: [Id],
    idAssets :: [Id],
    idParams :: [Id],
    idBody :: Body a
  }
  deriving (Show, Read, Eq)

data FuncDef a = FuncDef
  { fdPart :: (Maybe Id),
    fdCaptures :: [Id],
    fdParams :: [Id],
    fdBody :: Body a
  }
  deriving (Show, Read, Eq)

data BodyStmt a
  = BsPartStmt a (Maybe Id) (PartStmt a)
  | BsWithdraw a Id (Record TrivExpr)
  | BsDeposit a Id (Record TrivExpr)
  | BsPublish a Id Id
  deriving (Show, Read, Eq)

data PartStmt a
  = PsDef a Id Expr
  | PsIgnore a Expr
  | PsRequire a TrivExpr
  | PsAssert a TrivExpr
  deriving (Show, Read, Eq)
