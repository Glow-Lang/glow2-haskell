module Glow.Ast.BlockParamPassing where

import Data.Map.Strict (Map)
import Glow.Ast.Common
import Glow.Gerbil.Types (Record, Type, Variant, Pat)
import Glow.Ast.LiftedFunctions (Expr)
import Glow.Prelude

{- |
A 'Module' has type definitions, interaction definitions,
function definitions, and an initilization body.
The initialization body has the top-level variable
definitions.

In most glow programs, there will be a single interaction
definition that acts as the "entry point", but for a one
used as a library module there may be multiple interaction
definitions.
-}
data Module = Module 
  { pTypes :: Map Id TypeDef,
    pInteractions :: Map Id InteractionDef,
    pFuncs :: Map Id FuncDef,
    pInitBody :: Body
  }
  deriving (Show, Read, Eq)

{- |
A 'Body' has a control-flow graph of blocks with a starting
block label.
-}
data Body = Body
  { bdyBlocks :: Map Id Block,
    bdyStartBlock :: Id
  }
  deriving (Show, Read, Eq)

{- |
A 'Block' is either public or private.

 - If public, the 'blkPartInfo' has 'BpiPublic' with the
   set of active participants, and the 'blkStmts' must only
   have those active participants in the 'BsPartStmt's,
   deposits, and publish statements inside.

 - If private, the 'blkPartInfo' has 'BpiPrivate' with the
   participant it's private to and an eventual destination.
   The 'blkStmts' of a private block must only be
   'BsPartStmt's with 'Just' the participant it's private
   to, and each target in the 'blkBranch' must either go to
   the eventual destination, or go another private block
   for the same participant and the same eventual
   destination.
-}
data Block = Block
  { blkPartInfo :: BlockParticipantInfo,
    blkParams :: [Id],
    blkStmts :: [BodyStmt], -- ^ if the block is private this should only have BsPartStmt with just that participant
    blkBranch :: Branch
  }
  deriving (Show, Read, Eq)

data BlockParticipantInfo
  = BpiPublic [Id] -- ^ Public with a set of active participants
  | BpiPrivate (Id, EventualDestination) -- ^ Private to a single participant, with a single eventual public destination block
  deriving (Show, Read, Eq)

data EventualDestination
  = EpdReturn -- ^ Eventual return for participant-private functions only
  | EpdJump Id -- ^ Eventual jump to the given public block label
  deriving (Show, Read, Eq)

data Branch
  = BrReturn Expr
  | BrJump JumpTarget
  | BrSwitch TrivExpr SwitchCase (Maybe JumpTarget)
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

data TypeDef
  = TdDefType [Id] Type
  | TdDefData [Id] [Variant]
  deriving (Show, Read, Eq)

data InteractionDef = InteractionDef
  { idParticipants :: [Id],
    idAssets :: [Id],
    idParams :: [Id],
    idBody :: Body
  }
  deriving (Show, Read, Eq)

data FuncDef = FuncDef
  { fdPart :: (Maybe Id),
    fdCaptures :: [Id],
    fdParams :: [Id],
    fdBody :: Body
  }
  deriving (Show, Read, Eq)

data BodyStmt
  = BsPartStmt (Maybe Id) PartStmt
  | BsWithdraw Id (Record TrivExpr)
  | BsDeposit Id (Record TrivExpr)
  | BsPublish Id Id
  deriving (Show, Read, Eq)

data PartStmt
  = PsDef Id Expr
  | PsIgnore Expr
  | PsRequire TrivExpr
  | PsAssert TrivExpr
  deriving (Show, Read, Eq)
