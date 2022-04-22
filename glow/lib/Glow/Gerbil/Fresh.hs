{-# LANGUAGE LambdaCase #-}

module Glow.Gerbil.Fresh where

import Control.Monad.State (State, get, put)
import Data.ByteString.Char8 as BS8 (ByteString, pack, unpack, spanEnd)
import Data.List (init, delete)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Glow.Ast.Common (Id(..))
import Glow.Prelude
import Numeric.Natural
import Text.Read (readMaybe)
import Text.SExpression

type BSN = (ByteString, Maybe Natural)

type UnusedNats = Maybe ([Natural], Natural)
-- Where the nats in the list are in ascending order,
-- and the snd nat is greater than everything in the list.
-- Nothing means everything is available including the non-prefixed name
-- Just (list, onward) means the nats in `list` are available and everything
-- from `onward` onward is available.

type UnusedTable = Map ByteString UnusedNats

bsToBsn :: ByteString -> BSN
-- Ignore leading zeros, treat a0 and a00 the same,
-- so if there is a use of a00, conservatively avoid a0,
-- instead move on to a1.
bsToBsn s = case BS8.spanEnd isDigit s of
  (a, "") -> (a, Nothing)
  (a, b) -> (a, readMaybe (BS8.unpack b))

bsnToBs :: BSN -> ByteString
bsnToBs (a, Nothing) = a
bsnToBs (a, Just b) = a <> BS8.pack (show b)

useUnused :: Maybe Natural -> UnusedNats -> (Maybe Natural, UnusedNats)
useUnused Nothing Nothing = (Nothing, Just ([], 0))
useUnused Nothing (Just ([], x)) = (Just x, Just ([], x+1))
useUnused Nothing (Just (x : rst, y)) = (Just x, Just (rst, y))
useUnused (Just n) Nothing = (Just n, Just (init [0..n], n+1))
useUnused (Just n) (Just (lst, x)) =
  if (x <= n)
  then (Just n, Just (lst <> init [x..n], n+1))
  else if elem n lst
  then (Just n, Just (delete n lst, x))
  else useUnused Nothing (Just (lst, x))

fresh :: ByteString -> State UnusedTable ByteString
fresh bs = do
  let (s, n) = bsToBsn bs
  ut <- get
  let ul = Map.findWithDefault Nothing s ut
  let (n2, ul2) = useUnused n ul
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

freshId :: Id -> State UnusedTable Id
freshId (Id x) = Id <$> fresh x
