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
-- Material modifier (issue #312): the destination tile's surface
-- material scales the horizontal step cost via `materialFactor` —
-- loose/soft ground (sand, silt, mud) costs more than firm rock, so A*
-- prefers firmer routes. The same factor drives the actual move SPEED
-- at the call site (`Unit.Thread.Movement`), since the greedy stepper
-- reads `stepCost` only for its replan trigger, not for step length.
--
-- Future extension points (left as TODOs in the code, NOT plumbed yet):
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
    , lookupSurfaceMaterial
    , materialFactor
    , materialDetour
    , isCliffStep
    -- * Tunables (loaded from config/pathing.yaml; see "Unit.Pathing.Config")
    , PathingConfig(..)
    , defaultPathingConfig
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
import World.Material (MaterialRegistry, MaterialId(..), getMaterialProps, mpMoveCost)
import World.Generate (globalToChunk)
import Unit.Pathing.Config (PathingConfig(..), defaultPathingConfig)

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
stepCost ∷ PathingConfig → MaterialRegistry → WorldTileData → (Int, Int) → (Int, Int) → Maybe Float
stepCost pc reg wtd (sgx, sgy) (dgx, dgy) = do
    srcZ <- lookupTerrainZ wtd sgx sgy
    dstZ <- lookupTerrainZ wtd dgx dgy
    let fluidAtDst = lookupFluidType wtd dgx dgy
    fluidCost <- fluidPenalty pc fluidAtDst
    -- No-corner-cutting: a diagonal step grazes the two axis-aligned
    -- neighbours of the source tile. Reject the step if either neighbour
    -- is impassable (Ocean/Lava/unloaded chunk), so a mover can't slip
    -- diagonally past a wall. This rule lives HERE — in the single cost
    -- function both the greedy stepper and A* consult — so the two
    -- planners can never disagree about it. (They used to: A* allowed
    -- the corner-cut, the stepper rejected it, and a unit aimed at a
    -- diagonal corner replanned the same rejected path forever, frozen
    -- in its walk animation.)
    let isDiagonal = sgx ≢ dgx ∧ sgy ≢ dgy
        cornerBlocked = isDiagonal
                      ∧ not (passable pc wtd dgx sgy ∧ passable pc wtd sgx dgy)
    if cornerBlocked
      then Nothing
      else
        let dx     = fromIntegral (dgx - sgx) ∷ Float
            dy     = fromIntegral (dgy - sgy) ∷ Float
            -- Horizontal distance scaled by the destination tile's
            -- surface material: loose/soft ground (sand, mud) costs more
            -- per tile than firm rock, so A* routes prefer firmer ground
            -- (#312). Climb/fall/fluid terms are unaffected — they're
            -- vertical/hydraulic, not a function of the ground's looseness.
            horizD = sqrt (dx * dx + dy * dy) * materialFactor reg wtd dgx dgy
            dz     = dstZ - srcZ
            -- Upward: a walkable ramp (isCliffStep False) costs only the
            -- gentle rampFactor; a true cliff costs the steep climbFactor.
            -- This is the #3 fix — the planner used to charge climbFactor
            -- for ramps too, so units detoured around slopes they could
            -- just walk up.
            climb  = if dz > 0
                     then if isCliffStep wtd (sgx, sgy) (dgx, dgy) srcZ dstZ
                          then pcClimbFactor pc * fromIntegral dz
                          else pcRampFactor  pc * fromIntegral dz
                     else 0
            -- Downward: a single step is a free walk-off; a real fall
            -- (drop ≥ pcFallTriggerDrop) costs an exponential in height so
            -- the planner avoids damaging drops.
            fall   = if negate dz ≥ pcFallTriggerDrop pc
                     then pcFallFactor pc ** fromIntegral (negate dz)
                     else 0
        in pure $! horizD + climb + fall + fluidCost

-- | Is a single tile traversable on its own merits? True iff its chunk
--   is loaded (terrain z resolves) and its fluid isn't a non-wadeable
--   barrier (Ocean/Lava). Used by the no-corner-cutting gate in
--   `stepCost` to test the two axis-neighbours a diagonal step grazes.
passable ∷ PathingConfig → WorldTileData → Int → Int → Bool
passable pc wtd gx gy =
    case lookupTerrainZ wtd gx gy of
        Nothing → False
        Just _  → case fluidPenalty pc (lookupFluidType wtd gx gy) of
            Just _  → True
            Nothing → False

-- | Map fluid type at the destination to a step-cost modifier.
--   Ocean and lava are non-traversable (`Nothing`); rivers and lakes
--   are wadeable at a penalty. Dry tiles add 0.
fluidPenalty ∷ PathingConfig → Maybe FluidType → Maybe Float
fluidPenalty _  Nothing       = Just 0
fluidPenalty _  (Just Ocean)  = Nothing
fluidPenalty _  (Just Lava)   = Nothing
fluidPenalty pc (Just River)  = Just (pcRiverPenalty pc)
fluidPenalty pc (Just Lake)   = Just (pcLakePenalty pc)

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

-- | Read the surface (top-of-column) material id at a global tile coord.
--   This is the material a unit walks ON — the same top-of-column entry
--   the dump reports as @matId@. Returns Nothing if the chunk is
--   unloaded or the column is empty.
lookupSurfaceMaterial ∷ WorldTileData → Int → Int → Maybe Word8
lookupSurfaceMaterial wtd gx gy =
    let (chunkCoord, (lx, ly)) = globalToChunk gx gy
    in case lookupChunk chunkCoord wtd of
        Nothing → Nothing
        Just lc → case lcTiles lc V.!? columnIndex lx ly of
            Nothing  → Nothing
            Just col → let mats = ctMats col
                       in if VU.null mats
                          then Nothing
                          else Just (VU.last mats)

-- | Movement-cost multiplier of a tile's surface material (#312).
--   1.0 for firm ground, an unknown material, or an unloaded/empty
--   column (so missing data is a no-op); >1.0 for loose, soft terrain
--   authored via @move_cost@ in @data/materials/*.yaml@. Clamped to a
--   positive minimum so a stray @move_cost: 0@ can't zero out the
--   horizontal cost or divide-by-zero the speed scaling at the call site.
materialFactor ∷ MaterialRegistry → WorldTileData → Int → Int → Float
materialFactor reg wtd gx gy =
    case lookupSurfaceMaterial wtd gx gy of
        Nothing  → 1.0
        Just mid → max 0.1 (mpMoveCost (getMaterialProps reg (MaterialId mid)))

-- | Should the greedy mover run a local A* detour-check when stepping
--   from @src@ onto @dst@? True when the destination ground is enough
--   softer than the current ground — its `materialFactor` exceeds the
--   source's by at least `pcMaterialReplanMargin` — that a firmer route
--   might be worth finding.
--
--   This exists because material costs are deliberately MILD (sand 1.5,
--   mud 1.8), far below `pcReplanCostThreshold` (5.0). Without it a
--   greedy mover — which only replans when a single step exceeds that
--   threshold — would walk straight through soft ground every time,
--   slowing but never skirting it (#312). The trigger fires only on a
--   firm→soft *rising edge*, so a unit already on soft ground (the
--   common case — most of the world is soft soil) doesn't re-run A*
--   every step. A* itself still decides whether to actually detour: it
--   skirts the soft patch only when a firmer route is genuinely cheaper,
--   otherwise it returns the straight line.
materialDetour
    ∷ PathingConfig → MaterialRegistry → WorldTileData
    → (Int, Int)    -- src (gx, gy)
    → (Int, Int)    -- dst (gx, gy)
    → Bool
materialDetour pc reg wtd (sgx, sgy) (dgx, dgy) =
    materialFactor reg wtd dgx dgy - materialFactor reg wtd sgx sgy
        ≥ pcMaterialReplanMargin pc

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
    -- Diagonal 1-z step: always a cliff. Slope bits are cardinal-only,
    -- so there's no such thing as a diagonal ramp — you'd have to climb
    -- the corner block. (Without this, the cardinal slope-bit check
    -- below picks ONE axis's bit and can wrongly read a diagonal corner
    -- as a walkable ramp, making the unit step up a wall it shouldn't.)
    | sgx ≢ dgx ∧ sgy ≢ dgy = True
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
