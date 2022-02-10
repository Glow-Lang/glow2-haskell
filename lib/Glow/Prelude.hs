-- | Custom prelude module; sadly Haskell's standard prelude
-- is far from perfect, so we fuss with the standard imports
-- here.
--
-- Right now this just re-exports the custom prelude that @zenhack
-- uses for his own projects, but we can fuss with it for our own
-- needs as necessary.
module Glow.Prelude
    ( module Zhp
    ) where

import Zhp
