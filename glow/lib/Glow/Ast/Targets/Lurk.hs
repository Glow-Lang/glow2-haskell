{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Ast for Lurk; see https://github.com/lurk-lang/lurk/blob/master/spec/v0-1.md
--
-- For types with a type parameter, the parameter is used to attach misc
-- metadata to the node (e.g. debuginfo).
module Glow.Ast.Targets.Lurk where

import Data.Text.Lazy (Text)
import Glow.Prelude
import Text.SExpression (SExpr)

-- | A Lurk expression. The @a@ type parameter can be used to attach
-- misc. metadata (e.g. debuginfo).
data Expr a
  = ExNil a
  | ExT a
  | ExIf a (Expr a) (Expr a) (Expr a)
  | ExLambda a [Symbol] (Expr a)
  | ExLet a (Let a)
  | ExLetRec a (Let a)
  | ExBinary a BinOp (Expr a) (Expr a)
  | ExUnary a UnaryOp (Expr a)
  | ExBegin a [Expr a] (Expr a)
  | ExCurrentEnv a
  | ExFieldElem a Int
  | -- | @eval@ with an optional environment
    ExEval a (Expr a) (Maybe (Expr a))
  | ExSymbol a Symbol
  | ExApply a (Expr a) [Expr a]
  | ExQuote a SExpr
  deriving (Show, Read, Eq)

-- | A binary operatory
data BinOp
  = BOpCons
  | BOpPlus
  | BOpMinus
  | BOpTimes
  | BOpDiv
  | -- | lurk @=@
    BOpNumEq
  | -- | lurk @eq@
    BOpPtrEq
  deriving (Show, Read, Eq)

-- | A unary operator
data UnaryOp
  = UOpCar
  | UOpCdr
  | UOpEmit
  deriving (Show, Read, Eq)

-- | A let binding. Wether this is a @let@ or a @letrec@ depends on
-- in which constructor of 'Expr' it is contained.
data Let a = Let
  { letBindings :: [Binding a],
    letBody :: Expr a
  }
  deriving (Show, Read, Eq)

-- | A binding, as in a let expression.
data Binding a = Binding
  { bInfo :: a,
    bKey :: Symbol,
    bVal :: Expr a
  }
  deriving (Show, Read, Eq)

-- | A variable
newtype Symbol = Symbol {symText :: Text}
  deriving (Show, Read, Eq, Ord, IsString)
