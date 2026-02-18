{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Fluid.Types
    ( FluidType(..)
    , FluidCell(..)
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))

data FluidType = Ocean | Lake | River | Lava
    deriving (Show, Eq)
instance NFData FluidType where
    rnf Ocean = ()
    rnf Lake  = ()
    rnf River = ()
    rnf Lava  = ()

-- | Per-column fluid info, stored in LoadedChunk.
--   Only present for tiles that have fluid above them.
data FluidCell = FluidCell
    { fcType    ∷ !FluidType   -- ^ What kind of fluid
    , fcSurface ∷ !Int         -- ^ Z-level of the fluid surface
    } deriving (Show, Eq)
instance NFData FluidCell where
    rnf (FluidCell t s) = rnf t `seq` rnf s
