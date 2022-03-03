{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

-- | AST fo  Glow's surface syntax.
module Glow.Ast.SurfaceGADT where

import qualified Data.Text.Lazy as LT
import Glow.Prelude
import Numeric.Natural (Natural)

import qualified Glow.Ast.Surface as S

type Str = LT.Text

type Program a = [Stmt a]


-- | A glow statement
data Stmt a
  = StExpr a (Expr a)
  | StLet a Symbol (Expr a)
  deriving (Show, Read, Eq , Functor , Foldable)

-- | A glow expression
data Expr a
  = ExUnit a
  | ExIdent a Symbol
  | ExLiteral a (Literal a)
  | -- | ExUnary UnaryOp Expr
    ExBinary a (BinOp a) (Expr a) (Expr a)
  | ExLambda a (Function a)
  | ExRecord a [(Symbol, Expr a)]
  | ExBody a [(Stmt a)] (Expr a) -- { x; y; z }
  deriving (Show, Read, Eq, Functor, Foldable)

{-
data UnaryOp
    deriving(Show, Read, Eq)
-}

data BinOp a
  = BinOpAdd a
  | BinOpSub a
  | BinOpMul a
  | BinOpDiv a
  | BinOpMod a
  deriving (Show, Read, Eq, Functor, Foldable)

data Function a = Function
  { fParams :: [Param a],
    fBody :: Expr a
  }
  deriving (Show, Read, Eq, Functor, Foldable)

data Param a = Param
  { pName :: Symbol ,
    pType :: Maybe (Type a)
  }
  deriving (Show, Read, Eq, Functor, Foldable)

data Type a
  = TyIdent a Symbol
  deriving (Show, Read, Eq, Functor, Foldable)

data Literal a
  = LitBool a !Bool
  | LitNat a !Natural
  | LitStr a LT.Text
  | LitFn a (Function a)
  deriving (Show, Read, Eq, Functor, Foldable)

newtype Symbol = Symbol LT.Text
  deriving (Show, Read, Eq, Ord, IsString)
