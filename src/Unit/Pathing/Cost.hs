{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Tile-step cost function for unit pathing.
--
-- The pathing system is "continuous omnidirectional movement on a grid
-- cost map" — units move in ℝ², but the grid tells us what it costs to
-- traverse different terrain. This module is the cost map.
--
-- `stepCost` returns `Maybe Float`:
--   * `Nothing`     — the destination is impassable. The mover must
--                     find another way.
--   * `Just n`      — total cost of moving from src to dst. Composed of
--                     horizontal Euclidean distance plus terrain
--                     modifiers (climb, fall, fluid penalty).
--
-- The cost is a SCALAR — it has the units of distance, scaled by terrain
-- factors. The mover uses it for two things:
--   * Filter: any `Nothing` step is rejected.
--   * Threshold: if a step costs more than `replanCostThreshold`, the
--                mover triggers local A* to see if a flatter detour
--                exists. This is what makes units skirt cliffs instead
--                of climbing them when going around is cheaper.
--
-- Future extension points (left as TODOs in the code, NOT plumbed yet):
--   * Material modifier (sand slower than rock, etc.)
--   * Weather modifier (snow/rain slowing units)
--   * Per-unit modifier (heavy armor slower, light units faster)
--
-- These can be added by widening the function signature; call sites
-- pass placeholder modifiers of 1.0 today.
module Unit.Pathing.Cost
    ( stepCost
    , lookupTerrainZ
    , lookupFluidType
    , lookupSlopeAt
    , isCliffStep
    -- * Tunables (constants for now; future: load from config)
    , climbFactor
    , fallFactor
    , riverPenalty
    , lakePenalty
    , replanCostThreshold
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Bits (testBit)
import Data.Word (Word8)
import World.Types (WorldTileData(..), LoadedChunk(..), columnIndex, lookupChunk)
import World.Chunk.Types (ColumnTiles(..))
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Generate (globalToChunk)

-- * Tunables
--
-- These are placeholder values to start with. Tune after seeing units
-- actually navigate. They live here so Phase D's threshold trigger
-- imports from the same place as the cost function itself.

-- | Cost added per +1 z of climb. A 1-z step costs ~`climbFactor`
--   units on top of the horizontal distance. Tuned high enough that
--   units prefer flat detours; low enough that they'll climb when
--   nothing else is available.
climbFactor ∷ Float
climbFactor = 10.0

-- | Base for fall cost. `fallCost(-dz) = fallFactor ** (negate dz)`,
--   i.e. exponential in drop height. A 1-z drop is `fallFactor`; a
--   3-z drop is `fallFactor^3`. Units will jump small drops freely
--   but treat cliffs as expensive enough to avoid unless desperate.
fallFactor ∷ Float
fallFactor = 3.0

-- | Cost added for stepping into a river/lake tile. River and lake
--   are wadeable — high cost but not impassable. Ocean and lava are
--   `Nothing`.
riverPenalty ∷ Float
riverPenalty = 8.0

-- | Lake-crossing penalty. Slightly higher than rivers since lakes
--   are usually deeper.
lakePenalty ∷ Float
lakePenalty = 12.0

-- | If a greedy step's cost exceeds this, the mover triggers a local
--   A* to look for a flatter alternative. Lower = more replanning
--   (units pickier about route), higher = greedier (units climb
--   anything in their direct line).
replanCostThreshold ∷ Float
replanCostThreshold = 5.0

-- * Cost function

-- | Cost of a single step from source tile to destination tile, given
--   the current world state. Used by both greedy movement (one step
--   at a time) and A* (over a search frontier).
--
--   Both coordinates are global tile coordinates. The function looks
--   up the destination tile's terrain z and fluid type to compute the
--   modifiers. The source tile only contributes its terrain z (for
--   the climb/fall delta).
--
--   If the destination tile lies in an unloaded chunk, returns
--   `Nothing` — units can't path into unloaded territory. This
--   enforces the "movement clamped to loaded chunks" rule
--   bottom-up.
stepCost ∷ WorldTileData → (Int, Int) → (Int, Int) → Maybe Float
stepCost wtd (sgx, sgy) (dgx, dgy) = do
    srcZ <- lookupTerrainZ wtd sgx sgy
    dstZ <- lookupTerrainZ wtd dgx dgy
    let fluidAtDst = lookupFluidType wtd dgx dgy
    fluidCost <- fluidPenalty fluidAtDst
    let dx        = fromIntegral (dgx - sgx) ∷ Float
        dy        = fromIntegral (dgy - sgy) ∷ Float
        horizD    = sqrt (dx * dx + dy * dy)
        dz        = dstZ - srcZ
        climb     = if dz > 0
                    then climbFactor * fromIntegral dz
                    else 0
        fall      = if dz < 0
                    then fallFactor ** fromIntegral (negate dz)
                    else 0
    pure $! horizD + climb + fall + fluidCost

-- | Map fluid type at the destination to a step-cost modifier.
--   Ocean and lava are non-traversable (`Nothing`); rivers and lakes
--   are wadeable at a penalty. Dry tiles add 0.
fluidPenalty ∷ Maybe FluidType → Maybe Float
fluidPenalty Nothing       = Just 0
fluidPenalty (Just Ocean)  = Nothing
fluidPenalty (Just Lava)   = Nothing
fluidPenalty (Just River)  = Just riverPenalty
fluidPenalty (Just Lake)   = Just lakePenalty

-- * World lookups

-- | Read the terrain surface z at a global tile coord.
--   Returns `Nothing` if the containing chunk isn't loaded.
lookupTerrainZ ∷ WorldTileData → Int → Int → Maybe Int
lookupTerrainZ wtd gx gy =
    let (chunkCoord, (lx, ly)) = globalToChunk gx gy
    in case lookupChunk chunkCoord wtd of
        Nothing → Nothing
        Just lc → Just (lcTerrainSurfaceMap lc VU.! columnIndex lx ly)

-- | Read the slope bits at the given (gx, gy, z). Slope bits encode
--   which cardinal-direction neighbors are exactly 1 z below this
--   tile and walkable as a ramp: bit 0 = N, bit 1 = E, bit 2 = S,
--   bit 3 = W. A slope bit of 0 means this tile is a flat top — the
--   walls dropping away to neighbors are cliffs, not ramps.
--   Returns Nothing if the chunk is unloaded or z is out of the
--   column's stored range.
lookupSlopeAt ∷ WorldTileData → Int → Int → Int → Maybe Word8
lookupSlopeAt wtd gx gy z =
    let (chunkCoord, (lx, ly)) = globalToChunk gx gy
    in case lookupChunk chunkCoord wtd of
        Nothing → Nothing
        Just lc → case lcTiles lc V.!? columnIndex lx ly of
            Nothing  → Nothing
            Just col →
                let i = z - ctStartZ col
                in if i ≥ 0 ∧ i < VU.length (ctSlopes col)
                   then Just (ctSlopes col VU.! i)
                   else Nothing

-- | Is the step from src to dst a cliff that needs climbing (vs a
--   walkable slope)?
--
--   Rule:
--     * dz ≤ 0  → not a climb at all.
--     * dz ≥ 2  → always a cliff (slopes are only 1z deltas by
--                 construction; anything taller is vertical rock).
--     * dz = 1  → walkable ramp iff dst has a slope bit pointing
--                 back toward src (i.e. dst's slope was rendered as
--                 tapering down to src). Otherwise it's a cliff.
--
--   Diagonal cardinal-direction steps fall through to "cliff" since
--   slope bits only cover cardinal directions — units can't ramp up
--   diagonally on a corner, they'd need to climb the corner block.
isCliffStep
    ∷ WorldTileData
    → (Int, Int)    -- src (gx, gy)
    → (Int, Int)    -- dst (gx, gy)
    → Int           -- src tile's terrain z
    → Int           -- dst tile's terrain z
    → Bool
isCliffStep wtd (sgx, sgy) (dgx, dgy) srcZ dstZ
    | dz ≤ 0   = False
    | dz ≥ 2   = True
    | otherwise = case lookupSlopeAt wtd dgx dgy dstZ of
        Nothing     → True              -- no slope info → assume cliff
        Just slope  →
            let -- Direction from dst back toward src (where the ramp
                -- would face if there were one). bit N = src is at
                -- (dgx, dgy-1), etc.
                wantsBit
                    | sgy < dgy = 0   -- src is N of dst → need bit N
                    | sgy > dgy = 2   -- src is S of dst → need bit S
                    | sgx > dgx = 1   -- src is E of dst → need bit E
                    | sgx < dgx = 3   -- src is W of dst → need bit W
                    | otherwise = -1  -- same tile (shouldn't happen)
            in wantsBit < 0 ∨ not (testBit slope wantsBit)
  where
    dz = dstZ - srcZ

-- | Read the fluid type at a global tile coord, if any.
--   Returns `Nothing` for tiles in unloaded chunks AND for dry tiles
--   in loaded chunks. Callers needing to distinguish these two cases
--   can pair with `lookupTerrainZ` (which returns `Nothing` only for
--   unloaded chunks).
lookupFluidType ∷ WorldTileData → Int → Int → Maybe FluidType
lookupFluidType wtd gx gy =
    let (chunkCoord, (lx, ly)) = globalToChunk gx gy
    in case lookupChunk chunkCoord wtd of
        Nothing → Nothing
        Just lc → case lcFluidMap lc V.! columnIndex lx ly of
            Just fc → Just (fcType fc)
            Nothing → Nothing
