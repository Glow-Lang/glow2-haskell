-- | This module defines Ast nodes that appear in more than one Ast/IR.
module Glow.Ast.Common where

import qualified Data.Text.Lazy as LT
import Glow.Prelude

-- | A variable
newtype Var = Var LT.Text
  deriving (Show, Read, Eq, Ord)

data IntType = IntType
  { itSigned :: !Bool,
    itNumBits :: !Integer
  }
  deriving (Show, Read, Eq)
