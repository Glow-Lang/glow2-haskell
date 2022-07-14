{-# LANGUAGE LambdaCase #-}
module Glow.Runtime.Interaction.BuiltIns where

import Glow.Ast.Common
import Glow.Gerbil.Types
import Prelude
import Data.ByteString as BS
import Data.Bits



evalBuiltIn :: BS.ByteString -> [GlowValue] -> Either String GlowValue
evalBuiltIn = \case
  -- "randomUInt256" -> \(Integer x : _) -> Right (Integer x )
  "+" -> \[Integer x , Integer y] -> Right (Integer (x + y))
  "*" -> \[Integer x , Integer y] -> Right (Integer (x * y))
  "bitwise-xor" -> \[Integer x , Integer y] -> Right (Integer (xor x y))
  "bitwise-and" -> \[Integer x , Integer y] -> Right (Integer (x .&. y))
  i -> \_ -> Left ((show i) ++ " - builtIn not implemented!") 
