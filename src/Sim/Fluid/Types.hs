{-# LANGUAGE Strict #-}
module Sim.Fluid.Types
    ( ActiveFluidCell(..)
    , clampFluidVolume
    , exactSurfaceOf
    , surfaceCeilZOf
    , fluidCellToActive
    , activeToFluidCell
    , derivePassiveFluid
    , clearTouchedCells
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Fluid.Types (FluidType(..), FluidCell(..), fluidVolumeOverTerrain)
import World.Fluid.Exact (exactSurfaceOfZ, exactSurfaceCeilZ)

-- | Volume-tracking fluid cell for active (player-modified) chunks.
data ActiveFluidCell = ActiveFluidCell
    { afcType    ∷ !FluidType
    , afcVolume  ∷ !Word16     -- ^ exact fluid units standing over this
                               --   cell's own terrain top
                               --   ('World.Fluid.Exact.fluidUnitsPerZ'
                               --   units per z-level)
    , afcFlowDir ∷ !Word8      -- ^ packed 2-bit per cardinal: outflow directions
                               --   bit 0 = N, bit 1 = E, bit 2 = S, bit 3 = W
    } deriving (Show, Eq)

-- | Narrow a unit count to the 'Word16' an active cell carries.
--
--   The conversion domain is explicit (#2520 requirement 2): a negative
--   count is 0, and every count the type can represent —
--   @0 .. 65535@, i.e. 8191 whole z of depth, deeper than any world is
--   tall — narrows EXACTLY, which is what makes activation an identity.
--   A deeper column than that saturates at the bound rather than
--   wrapping to a near-empty cell; that case is a documented clip, not
--   an exact activation, and it is unreachable from generated or saved
--   world heights.
clampFluidVolume ∷ Int → Word16
clampFluidVolume n = fromIntegral (max 0 (min n maxVolume))
  where maxVolume = fromIntegral (maxBound ∷ Word16) ∷ Int
{-# INLINE clampFluidVolume #-}

-- | The EXACT absolute surface of an active cell: its terrain top on
--   the exact plane plus every unit standing over it. This is the value
--   gravity, seam and waterfall pressure compare; a difference of two
--   of them is already measured in fluid units.
exactSurfaceOf ∷ Int → Word16 → Int
exactSurfaceOf terrainZ vol = exactSurfaceOfZ terrainZ + fromIntegral vol
{-# INLINE exactSurfaceOf #-}

-- | Compatibility view of 'exactSurfaceOf': the lowest whole z at or
--   above an active cell's exact surface. A dry cell reads as its own
--   terrain top, and any positive volume under one z reads as one
--   level — the predicate every whole-z consumer of an active cell
--   (the reaction's water surface, D-5) had before #2520.
surfaceCeilZOf ∷ Int → Word16 → Int
surfaceCeilZOf terrainZ vol = exactSurfaceCeilZ (exactSurfaceOf terrainZ vol)
{-# INLINE surfaceCeilZOf #-}

-- | Convert a passive FluidCell to an active one given terrain height.
--
--   EXACT (#2520 requirement 4): the active cell takes precisely the
--   units the passive cell's exact surface stands above the terrain, so
--   a one-unit cell activates as one unit rather than as a whole level.
--
--   A cell whose exact surface is at or BELOW its terrain holds no
--   volume and takes no slot in the active grid. It is not erased —
--   'derivePassiveFluid' preserves it, type and exact plane intact,
--   until DFL-5 repairs generated channel terrain (D-11).
fluidCellToActive ∷ Int → FluidCell → Maybe ActiveFluidCell
fluidCellToActive terrainZ fc
    | units ≤ 0 = Nothing
    | otherwise = Just ActiveFluidCell
        { afcType    = fcType fc
        , afcVolume  = clampFluidVolume units
        , afcFlowDir = 0
        }
  where units = fluidVolumeOverTerrain terrainZ fc

-- | Convert an active cell back to a passive FluidCell.
--
--   EXACT: every remaining unit is written to the exact plane, so
--   deactivation and save can never round a partial cell up into a full
--   one. A cell holding nothing is dry.
activeToFluidCell ∷ Int → ActiveFluidCell → Maybe FluidCell
activeToFluidCell terrainZ afc
    | afcVolume afc ≡ 0 = Nothing
    | otherwise = Just FluidCell
        { fcType         = afcType afc
        , fcExactSurface = exactSurfaceOf terrainZ (afcVolume afc)
        }

-- | Derive a chunk's passive fluid map from its active volume grid,
--   against the passive map the chunk already holds.
--
--   THE one place the active → passive direction is written (#2520
--   requirement 4/5): the per-tick derivation, the seam re-derivation,
--   equilibrium deactivation and the "Sim.Thread" writeback all call
--   it, so none of them can round or drop differently from the others.
--
--   An occupied active cell writes its exact surface. An EMPTY one
--   normally writes dry — a cell drained by transfer, dry-out or
--   annihilation IS dry, and must not be resurrected from the stale
--   passive map. The single exception is the sub-terrain cell
--   activation never took up: its prior passive cell stands at or below
--   its terrain, so it holds no volume the active grid could have
--   carried, and it crosses through unchanged with its type and its
--   exact plane (requirement 5). That test is on the PRIOR cell's own
--   height, so an ordinary positive-volume cell can never qualify.
--
--   The @prior@ map must describe the location as it stands NOW, not as
--   it stood before whatever is being derived. A sub-terrain cell holds
--   no volume, which makes it an ordinary EMPTY destination a neighbour
--   may fill — and fluid that arrives there can be drained or
--   annihilated again before the same tick ends, leaving the slot empty
--   a second time with the location's identity genuinely changed.
--   A stale @prior@ would restore the old cell over that, so a caller
--   deriving MID-TICK runs its prior through 'clearTouchedCells' first.
--   A caller deriving BETWEEN ticks — the writeback, deactivation —
--   already holds a map the last tick corrected.
derivePassiveFluid ∷ VU.Vector Int                  -- ^ terrain tops
                   → V.Vector (Maybe FluidCell)     -- ^ prior passive map
                   → V.Vector (Maybe ActiveFluidCell)
                   → V.Vector (Maybe FluidCell)
derivePassiveFluid terrainV prior active =
    V.imap (\idx mafc →
        let terrainZ = terrainV VU.! idx
        in case mafc of
            Just afc | afcVolume afc ≢ 0 → activeToFluidCell terrainZ afc
            _ → case prior V.!? idx of
                Just (Just fc) | fluidVolumeOverTerrain terrainZ fc ≡ 0 → Just fc
                _ → Nothing
        ) active

-- | Forget the prior passive cell at every index the tick actually
--   wrote fluid into.
--
--   'derivePassiveFluid' preserves a sub-terrain cell on the strength of
--   the prior map alone, which cannot tell an UNTOUCHED slot from one
--   that was filled and emptied again within the same tick. The tick
--   records the difference as it happens — an arrival is the only way a
--   preserved slot can stop being untouched, since a cell holding
--   nothing can be neither drained nor reacted with — and this applies
--   that record, so a location the tick really changed derives as the
--   tick left it rather than as it began.
clearTouchedCells ∷ VU.Vector Bool → V.Vector (Maybe FluidCell)
                  → V.Vector (Maybe FluidCell)
clearTouchedCells touched prior
    | not (VU.or touched) = prior
    | otherwise = V.imap
        (\idx cell → if touched VU.! idx then Nothing else cell) prior
