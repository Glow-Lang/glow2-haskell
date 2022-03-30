
module Glow.Gerbil.Fresh where

import Control.Monad
import Control.Monad.State (State, get, put)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Glow.Gerbil.ParseCommon (bs8pack)
import Glow.Gerbil.Types (ByteString(..))
import Glow.Prelude
import Numeric.Natural
import Text.Read (readMaybe)

type BSN = (ByteString, Maybe Natural)

type UnusedList = [Natural]

type UnusedTable = Map ByteString UnusedList

bsToBsn :: ByteString -> BSN
bsToBsn s = case LBS8.spanEnd isDigit (toLBS s) of
  (a, "") -> (WrappedByteString a, Nothing)
  (a, b) -> (WrappedByteString a, readMaybe (LBS8.unpack b))

bsnToBs :: BSN -> ByteString
bsnToBs (a, Nothing) = a
bsnToBs (a, Just b) = a <> bs8pack (show b)

useUnused :: UnusedList -> Maybe Natural -> (Maybe Natural, UnusedList)
useUnused [] Nothing = (Nothing, [0])
useUnused [] (Just n) = (Just n, [0..(n-1)] <> [n+1])
useUnused [x] Nothing = (Just x, [x+1])
useUnused [x] (Just n) = if (n <= x)
                         then (Just x, [x+1])
                         else (Just n, [x..(n-1)] <> [n+1])
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
  return (bsnToBs (s, n2))

genTemps :: [a] -> State UnusedTable [ByteString]
genTemps l = mapM genTemp l

genTemp :: a -> State UnusedTable ByteString
genTemp _ = fresh "tmp"

