{-# LANGUAGE LambdaCase #-}

-- | An Ordering for SExprs
module Glow.Gerbil.OrdSExpr where

import Glow.Prelude
import Text.SExpression as SExpr

newtype OrdSExpr = OrdSExpr SExpr
  deriving (Eq, Show)

instance Ord OrdSExpr where
  compare (OrdSExpr x) (OrdSExpr y) = case (x, y) of
    -- Normalizing Cases
    (ConsList [] a, b) -> compare (OrdSExpr a) (OrdSExpr b)
    (a, ConsList [] b) -> compare (OrdSExpr a) (OrdSExpr b)
    (ConsList (a1 : a2 : a3) a4, b) -> compare (OrdSExpr (ConsList [a1] (ConsList (a2 : a3) a4))) (OrdSExpr b)
    (a, ConsList (b1 : b2 : b3) b4) -> compare (OrdSExpr a) (OrdSExpr (ConsList [b1] (ConsList (b2 : b3) b4)))
    (List (a1 : a2), b) -> compare (OrdSExpr (ConsList [a1] (List a2))) (OrdSExpr b)
    (a, List (b1 : b2)) -> compare (OrdSExpr a) (OrdSExpr (ConsList [b1] (List b2)))
    -- Ordering Cases
    (Atom a, Atom b) -> compare a b
    (Atom _, _) -> LT
    (_, Atom _) -> GT
    (Bool a, Bool b) -> compare a b
    (Bool _, _) -> LT
    (_, Bool _) -> GT
    (Number a, Number b) -> compare a b
    (Number _, _) -> LT
    (_, Number _) -> GT
    (String a, String b) -> compare a b
    (String _, _) -> LT
    (_, String _) -> GT
    (List [], List []) -> EQ
    (List [], _) -> LT
    (_, List []) -> GT
    (ConsList [a1] a2, ConsList [b1] b2) -> compare (OrdSExpr a1) (OrdSExpr b1) <> compare (OrdSExpr a2) (OrdSExpr b2)
