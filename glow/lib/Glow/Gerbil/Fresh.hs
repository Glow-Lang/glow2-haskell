{-# LANGUAGE LambdaCase #-}

module Glow.Gerbil.Fresh where

import Control.Monad.State (State, get, put)
import Data.ByteString.Char8 as BS8 (ByteString, pack, unpack, spanEnd)
import Data.List (init)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Glow.Prelude
import Numeric.Natural
import Text.Read (readMaybe)
import Text.SExpression

type BSN = (ByteString, Maybe Natural)

type UnusedList = [Natural]

type UnusedTable = Map ByteString UnusedList

bsToBsn :: ByteString -> BSN
bsToBsn s = case BS8.spanEnd isDigit s of
  (a, "") -> (a, Nothing)
  (a, b) -> (a, readMaybe (BS8.unpack b))

bsnToBs :: BSN -> ByteString
bsnToBs (a, Nothing) = a
bsnToBs (a, Just b) = a <> BS8.pack (show b)

useUnused :: UnusedList -> Maybe Natural -> (Maybe Natural, UnusedList)
useUnused [] Nothing = (Nothing, [0])
useUnused [] (Just n) = (Just n, init [0..n] <> [n+1])
useUnused [x] Nothing = (Just x, [x+1])
useUnused [x] (Just n) = if (n <= x)
                         then (Just x, [x+1])
                         else (Just n, init [x..n] <> [n+1])
useUnused (x : rst) Nothing = (Just x, rst)
useUnused (x : rst) (Just n) = if (n <= x)
                               then (Just x, rst)
                               else (Just n, removeUnused (x : rst) (Just n))

removeUnused :: UnusedList -> Maybe Natural -> UnusedList
removeUnused ul n = snd (useUnused ul n)

fresh :: ByteString -> State UnusedTable ByteString
fresh bs = do
  let (s, n) = bsToBsn bs
  ut <- get
  let ul = Map.findWithDefault [] s ut
  let (n2, ul2) = useUnused ul n
  put (Map.insert s ul2 ut)
  pure (bsnToBs (s, n2))

markUsed :: ByteString -> State UnusedTable ()
markUsed bs = fresh bs *> pure ()

markAtomsUsed :: SExpr -> State UnusedTable ()
markAtomsUsed = \case
  Atom bs -> markUsed (BS8.pack bs)
  List l -> traverse_ markAtomsUsed l
  ConsList l e -> traverse_ markAtomsUsed l *> markAtomsUsed e
  _ -> pure ()
