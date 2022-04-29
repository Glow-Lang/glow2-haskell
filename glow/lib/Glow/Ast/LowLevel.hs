module Glow.Ast.LowLevel where

import Data.Map.Strict (Map)
import qualified Data.Set as S
import qualified Data.Text.Lazy as LT
import Glow.Ast.Common
import Glow.Prelude

newtype Name = Name LT.Text
  deriving (Show, Read, Eq, Ord)

data Program a = Program
  { pInfo :: a,
    -- | Name of function that is the entry point/"main" function for the program:
    pEntryPoint :: Name,
    -- | Name of a function run *before* the entry point, responsible for initializing
    -- global variables:
    pInitFn :: Name,
    pDefs :: Map Name (Definition a)
  }
  deriving (Show, Read, Eq)

data Definition a
  = DefFunc a (FuncDef a)
  | DefVar a (VarDef a)
  deriving (Show, Read, Eq)

data VarDef a
  = VarDef a (S.Set VarProperty)
  deriving (Show, Read, Eq)

data VarProperty
  = VpPersisent
  | VpMerkleizable
  | VpInteractionLocal
  | VpParameter
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data FuncDef a = FuncDef
  { fdParams :: ParamList a,
    fdResult :: Result a,
    fdBlocks :: Map Name (Block a),
    fdStartBlock :: Name
  }
  deriving (Show, Read, Eq)

type Param a = (Id, Type a)

type ParamList a = [(Id, Type a)]

type Result a = (Id, Type a)

data Type a
  = TyFunc a [Type a] (Type a)
  | TyInt a IntType
  | TyBool a
  | TyArray a (Type a)
  | TyPtr a (Type a)
  | TyTuple a [Type a]
  deriving (Show, Read, Eq)

data Block a = Block
  { blkParams :: ParamList a,
    blkStmts :: [Stmt a],
    blkBranch :: Branch a
  }
  deriving (Show, Read, Eq)

data Stmt a
  = StLet a Id (ValueStmt a)
  | StIgnore a (VoidStmt a)
  deriving (Show, Read, Eq)

data ValueStmt a
  = ValStCall (CallStmt a)
  | ValStEval (Expr a)
  deriving (Show, Read, Eq)

data VoidStmt a
  = VoidStCall (CallStmt a)
  | VoidStStore (Type a) (Expr a) (Expr a)
  deriving (Show, Read, Eq)

data CallStmt a
  = CsCall a Name [Expr a]
  | CsCallPtr a (Expr a) [Expr a]
  deriving (Show, Read, Eq)

data Branch a
  = BrReturn a (Expr a)
  | BrJump a (JumpTarget a)
  | BrSwitch a (Expr a) [SwitchCase a] (Maybe (JumpTarget a))
  deriving (Show, Read, Eq)

data SwitchCase a = SwitchCase
  { scMatchValue :: Constant,
    scTarget :: JumpTarget a
  }
  deriving (Show, Read, Eq)

data JumpTarget a = JumpTarget
  { jtName :: Name,
    jtArgs :: [Expr a]
  }
  deriving (Show, Read, Eq)

data Expr a
  = ExVar a Id
  | ExConst a Constant
  | ExFuncPtr a Name
  | ExApplyOp a (Op a) [Expr a]
  deriving (Show, Read, Eq)

data Op a
  = OpAdd
  | OpSub
  | OpMul
  | OpOr
  | OpAnd
  | OpLoad (Type a)
  deriving (Show, Read, Eq)
