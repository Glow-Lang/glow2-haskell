{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | AST fo  Glow's surface syntax.
module Glow.Ast.SurfaceGrammar where

import qualified Data.Text.Lazy as LT
import Glow.Prelude
import Numeric.Natural (Natural)

import qualified Glow.Ast.Surface as S

data NodeKind = Program | Stmt | Expr | Symbol


type family ToSurface (nk :: NodeKind) :: * where
  ToSurface Program = S.Program
  ToSurface Stmt = S.Stmt
  ToSurface Expr = S.Expr
  ToSurface Symbol = S.Symbol

data ToSurface' (nk :: NodeKind) = ToSurface' (ToSurface nk)

toSS :: ToSurface' nk -> ToSurface nk
toSS (ToSurface' x) = x 
  
class AST (nk :: NodeKind) (a :: *) where
  toSurface' :: a -> ToSurface' nk

-- toSurface :: AST nk a => a -> ToSurface nk
-- toSurface = ?
  
type Str = LT.Text

data List (nk :: NodeKind) where
  Nil :: List nk
  Cons :: AST nk a => a -> List nk -> List nk

toSurfaceList' :: List nk -> [ToSurface' nk]
toSurfaceList' = undefined

instance AST Program (List Stmt) where
  toSurface' x = ToSurface' (map toSS (toSurfaceList' x))

instance AST Expr a => AST Stmt a where
  toSurface' x = ToSurface' (S.StExpr (toSS ((toSurface' x) :: ToSurface' Expr)))



    
data StLet = forall a b . (AST Symbol a , AST Expr b) => StLet a b  
instance AST Stmt StLet where







  
-- -- | A glow statement
-- data Stmt
--   = StExpr Expr
--   | StLet Symbol Expr
--   deriving (Show, Read, Eq)

-- -- | A glow expression
-- data Expr
--   = ExUnit
--   | ExIdent Symbol
--   | ExLiteral Literal
--   | -- | ExUnary UnaryOp Expr
--     ExBinary BinOp Expr Expr
--   | ExLambda Function
--   | ExRecord [(Symbol, Expr)]
--   | ExBody [Stmt] Expr -- { x; y; z }
--   deriving (Show, Read, Eq)

-- {-
-- data UnaryOp
--     deriving(Show, Read, Eq)
-- -}

-- data BinOp
--   = BinOpAdd
--   | BinOpSub
--   | BinOpMul
--   | BinOpDiv
--   | BinOpMod
--   deriving (Show, Read, Eq)

-- data Function = Function
--   { fParams :: [Param],
--     fBody :: Expr
--   }
--   deriving (Show, Read, Eq)

-- data Param = Param
--   { pName :: Symbol,
--     pType :: Maybe Type
--   }
--   deriving (Show, Read, Eq)

-- data Type
--   = TyIdent Symbol
--   deriving (Show, Read, Eq)

-- data Literal
--   = LitBool !Bool
--   | LitNat !Natural
--   | LitStr LT.Text
--   | LitFn Function
--   deriving (Show, Read, Eq)

-- newtype Symbol = Symbol LT.Text
--   deriving (Show, Read, Eq, Ord, IsString)
