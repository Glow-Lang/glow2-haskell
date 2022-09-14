{-# LANGUAGE LambdaCase #-}

module Glow.Gerbil.Fresh where

import Control.Monad.State (State, get, put)
import Data.ByteString.Char8 as BS8 (ByteString, pack, spanEnd, unpack)
import Data.List (delete, init)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Glow.Ast.Common (Id (..))
import Glow.Prelude
import Numeric.Natural
import Text.Read (readMaybe)
import Text.SExpression

-- |
-- A 'BSN' is a pair of a non-suffixed name and a possible suffix natural.
--
-- Example: The name @m8@ as a 'BSN' would be @("m", Just 8)@.
type BSN = (ByteString, Maybe Natural)

-- |
-- An 'UnusedNats' is one of:
--
--  - @Nothing@ means everything is available including the non-suffixed name.
--  - @Just (list :: [Natural], onward :: Natural)@
--    where the nats in the @list@ are in ascending order,
--    and the @onward@ nat is greater than everything in the @list@,
--    means the nats in @list@ are available and everything
--    from @onward@ onward is available.
--
-- Example: If the numbers 4, 6, and 7 are used, then the 'UnusedNats' are
-- represented as @Just ([0, 1, 2, 3, 5], 8)@.
type UnusedNats = Maybe ([Natural], Natural)

-- |
-- An 'UnusedTable' is a map from non-prefixed names to 'UnusedNats'.
-- The actual unused names can be represented as 'BSN' pairs made from
-- each of the keys and their associated unused nats.
--
-- Example: If the names @a1@, @a4@, and @m8@ are used, then the 'UnusedTable' is
-- @Map.fromList ([("a", Just ([0, 2, 3], 5)), ("m", Just ([0..7], 9))])@.
type UnusedTable = Map ByteString UnusedNats

-- |
-- Converts a name into its 'BSN' representation.
--
-- Example: @'bsToBsn' "m8"@ produces @("m", Just 8)@.
--
-- Ignore leading zeros, treat @a7@ and @a007@ the same,
-- so if there is a use of @a007@, conservatively avoid @a7@,
-- instead move on to @a8@ if that's the next unused nat.
bsToBsn :: ByteString -> BSN
bsToBsn s = case BS8.spanEnd isDigit s of
  (a, "") -> (a, Nothing)
  (a, b) -> (a, readMaybe (BS8.unpack b))

-- |
-- Converts a 'BSN' into a normalized name.
--
-- Example: @'bsnToBs' ("m", Just 8)@ produces @"m8"@.
--
-- While 'bsToBsn' is the left-inverse of 'bsnToBs', the
-- reverse is not true since 'bsToBsn' ignores leading zeros.
-- Instead, @'bsnToBs' ('bsToBsn' "a007")@ produces @"a7"@, a
-- normalized name without the leading zeros.
bsnToBs :: BSN -> ByteString
bsnToBs (a, Nothing) = a
bsnToBs (a, Just b) = a <> BS8.pack (show b)

-- |
-- Uses up the possible nat suffix from the 'UnusedNats', producing
-- that suffix if unused, or the first unused suffix otherwise.
--
-- Examples:
--
-- @'useUnused' (Just 1) Nothing@ produces @(Just 1, Just ([0], 2))@,
--
-- @'useUnused' (Just 1) (Just ([0], 2))@ produces @(Just 0, Just ([], 2))@,
--
-- @'useUnused' (Just 1) (Just ([], 2))@ produces @(Just 2, Just ([], 3))@,
--
-- and so on.
useUnused :: Maybe Natural -> UnusedNats -> (Maybe Natural, UnusedNats)
useUnused Nothing Nothing = (Nothing, Just ([], 0))
useUnused Nothing (Just ([], x)) = (Just x, Just ([], x + 1))
useUnused Nothing (Just (x : rst, y)) = (Just x, Just (rst, y))
useUnused (Just n) Nothing = (Just n, Just (init [0 .. n], n + 1))
useUnused (Just n) (Just (lst, x)) =
  if (x <= n)
    then (Just n, Just (lst <> init [x .. n], n + 1))
    else
      if elem n lst
        then (Just n, Just (delete n lst, x))
        else useUnused Nothing (Just (lst, x))

-- |
-- Produces a name that hasn't been used in the 'UnusedTable',
-- which may be either the original name if that hasn't been used yet,
-- or a modified name with a different nat suffix.
fresh :: ByteString -> State UnusedTable ByteString
fresh bs = do
  let (s, n) = bsToBsn bs
  ut <- get
  let ul = Map.findWithDefault Nothing s ut
  let (n2, ul2) = useUnused n ul
  put (Map.insert s ul2 ut)
  pure (bsnToBs (s, n2))

-- |
-- Marks the given name as used in the 'UnusedTable'.
markUsed :: ByteString -> State UnusedTable ()
markUsed bs = fresh bs *> pure ()

-- |
-- Marks all atoms in the given 'SExpr' as used in the 'UnusedTable'.
markAtomsUsed :: SExpr -> State UnusedTable ()
markAtomsUsed = \case
  Atom bs -> markUsed (BS8.pack bs)
  List l -> traverse_ markAtomsUsed l
  ConsList l e -> traverse_ markAtomsUsed l *> markAtomsUsed e
  _ -> pure ()

-- |
-- Produces a name 'Id' that hasn't been used in the 'UnusedTable',
-- which may be either the original name if that hasn't been used yet,
-- or a modified name with a different nat suffix.
freshId :: Id -> State UnusedTable Id
freshId (Id x) = Id <$> fresh x
