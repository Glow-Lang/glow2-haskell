-- | Custom prelude module; sadly Haskell's standard prelude
-- is far from perfect, so we fuss with the standard imports
-- here.
--
-- Right now this just a minor tweak of the custom prelude that @zenhack
-- uses for his own projects, but we can fuss with it for our own needs
-- as necessary.
module Glow.Prelude
  ( module Zhp,
  )
where

-- We don't do much with file IO, and the name Handle is used for our own
-- abstractions, so hide the name of that type.
import Zhp hiding (Handle)
