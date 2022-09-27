module Glow.Ast.Classes where

class HasMeta f where
  getMeta :: f a -> a
  setMeta :: a -> f a -> f a
