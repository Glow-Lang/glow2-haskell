-- Glow Ast.
module Glow.Ast where

import Glow.Prelude

import Numeric.Natural (Natural)

import qualified Data.Text.Lazy as LT

type Str = LT.Text

-- | A glow statement
data Stmt
    = StExpr Expr
    | StFunDef Function
    | StValDef Symbol Expr
    deriving(Show, Read, Eq)

-- | A glow expression
data Expr
    = ExUnit
    | ExIdent Symbol
    | ExLiteral Literal
    -- | ExUnary UnaryOp Expr
    | ExBinary BinOp Expr Expr
    deriving(Show, Read, Eq)

{-
data UnaryOp
    deriving(Show, Read, Eq)
-}

data BinOp
    = BinOpAdd
    | BinOpSub
    | BinOpMul
    | BinOpDiv
    | BinOpMod
    deriving(Show, Read, Eq)

data Function = Function
    { fdName   :: Maybe Symbol
    , fdParams :: [Symbol]
    , fdBody   :: [Stmt]
    }
    deriving(Show, Read, Eq)

data Literal
    = LitBool !Bool
    | LitNat !Natural
    | LitStr LT.Text
    | LitFn Function
    deriving(Show, Read, Eq)

newtype Symbol = Symbol LT.Text
    deriving(Show, Read, Eq, Ord)
