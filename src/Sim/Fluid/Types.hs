{-# LANGUAGE Strict, UnicodeSyntax #-}
module Sim.Fluid.Types
    ( ActiveFluidCell(..)
    , volumePerLevel
    , volumeToSurface
    , fluidCellToActive
    , activeToFluidCell
    ) where

import UPrelude
import World.Fluid.Types (FluidType(..), FluidCell(..))

-- | Volume-tracking fluid cell for active (player-modified) chunks.
data ActiveFluidCell = ActiveFluidCell
    { afcType    ∷ !FluidType
    , afcVolume  ∷ !Word16     -- ^ water units (volumePerLevel per z-level)
    , afcFlowDir ∷ !Word8      -- ^ packed 2-bit per cardinal: outflow directions
                               --   bit 0 = N, bit 1 = E, bit 2 = S, bit 3 = W
    } deriving (Show, Eq)

-- | Volume units per z-level of water depth.
volumePerLevel ∷ Int
volumePerLevel = 7

-- | Derive water surface z-level from terrain height and volume.
volumeToSurface ∷ Int → Word16 → Int
volumeToSurface terrainZ vol
    | vol ≡ 0   = terrainZ
    | otherwise = terrainZ + ((fromIntegral vol + volumePerLevel - 1) `div` volumePerLevel)

-- | Convert a passive FluidCell to an active one given terrain height.
fluidCellToActive ∷ Int → FluidCell → Maybe ActiveFluidCell
fluidCellToActive terrainZ fc =
    let depth = max 0 (fcSurface fc - terrainZ)
        vol   = fromIntegral (depth * volumePerLevel)
    in if vol ≡ 0 ∧ depth ≡ 0
       then Nothing
       else Just ActiveFluidCell
            { afcType    = fcType fc
            , afcVolume  = vol
            , afcFlowDir = 0
            }

-- | Convert an active cell back to a passive FluidCell.
activeToFluidCell ∷ Int → ActiveFluidCell → Maybe FluidCell
activeToFluidCell terrainZ afc
    | afcVolume afc ≡ 0 = Nothing
    | otherwise = Just FluidCell
        { fcType    = afcType afc
        , fcSurface = volumeToSurface terrainZ (afcVolume afc)
        }
