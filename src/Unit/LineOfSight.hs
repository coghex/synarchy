{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Per-unit line-of-sight. A unit's visible tile set is the
-- intersection of:
--   * a circular radius (perception * 6 tiles),
--   * a 120° cone centered on the unit's facing direction,
--   * line-of-sight against terrain Z (a hill in the way blocks vision).
--
-- Used by the AI to discover water sources, locate allies, and (in the
-- future) drive fog of war.
module Unit.LineOfSight
    ( unitVisibleTiles
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..))
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))
import Unit.Direction (Direction(..))
import World.Types (WorldManager(..), WorldState(..))
import World.Chunk.Types (LoadedChunk(..), columnIndex)
import World.Tile.Types (lookupChunk, WorldTileData(..))
import World.Generate.Coordinates (globalToChunk)

-- | All tiles the unit can currently see. Empty list if the unit
--   doesn't exist or no world is visible. Includes the unit's own
--   tile (always visible).
unitVisibleTiles ∷ EngineEnv → UnitId → IO [(Int, Int)]
unitVisibleTiles env uid = do
    um ← readIORef (unitManagerRef env)
    case HM.lookup uid (umInstances um) of
        Nothing → return []
        Just inst → do
            wm ← readIORef (worldManagerRef env)
            case wmVisible wm of
                [] → return []
                (pageId:_) → case lookup pageId (wmWorlds wm) of
                    Nothing → return []
                    Just ws → do
                        wtd ← readIORef (wsTilesRef ws)
                        let perception = HM.lookupDefault 1.0 "perception"
                                            (uiStats inst)
                            radius = max 1 (floor (perception * 6) ∷ Int)
                            ux = floor (uiGridX inst) ∷ Int
                            uy = floor (uiGridY inst) ∷ Int
                            uz = uiGridZ inst
                            -- Eye Z is one tile above the unit's footing.
                            eyeZ = fromIntegral (uz + 1) ∷ Double
                            (fx, fy) = facingVector (uiFacing inst)

                            inFOV (dx, dy)
                              | dx ≡ 0 ∧ dy ≡ 0 = True   -- own tile
                              | otherwise =
                                  let dd = dx * dx + dy * dy
                                  in dd ≤ radius * radius
                                     ∧ inCone fx fy dx dy

                            visible =
                                [ (ux + dx, uy + dy)
                                | dx ← [-radius .. radius]
                                , dy ← [-radius .. radius]
                                , inFOV (dx, dy)
                                , (dx ≡ 0 ∧ dy ≡ 0)
                                  ∨ notBlocked wtd ux uy eyeZ
                                              (ux + dx) (uy + dy)
                                ]
                        return visible

-- | Direction → unit-length screen-space heading. Y grows south, so
--   DirS is (0, +1).
facingVector ∷ Direction → (Double, Double)
facingVector DirN  = ( 0.0      , -1.0)
facingVector DirS  = ( 0.0      ,  1.0)
facingVector DirE  = ( 1.0      ,  0.0)
facingVector DirW  = (-1.0      ,  0.0)
facingVector DirNE = ( 0.7071068, -0.7071068)
facingVector DirNW = (-0.7071068, -0.7071068)
facingVector DirSE = ( 0.7071068,  0.7071068)
facingVector DirSW = (-0.7071068,  0.7071068)

-- | Is offset (dx, dy) inside the 120° cone centered on (fx, fy)?
--   Half-angle 60°, cos(60°) = 0.5. We compare dot² to 0.25 × |v|² to
--   avoid a sqrt; the dot ≥ 0 check rules out the back hemisphere
--   (where squaring would flip the sign).
inCone ∷ Double → Double → Int → Int → Bool
inCone fx fy dx dy =
    let dxF = fromIntegral dx
        dyF = fromIntegral dy
        dot = dxF * fx + dyF * fy
        lenSq = dxF * dxF + dyF * dyF
    in dot ≥ 0 ∧ dot * dot ≥ 0.25 * lenSq

-- | Cast a line from (ux, uy) at eyeZ to (gx, gy)'s surface; True if
--   no intermediate tile's terrain top rises above the line's height
--   at that horizontal step. Endpoints excluded — the source can't
--   block itself, and the target's own elevation IS the line's
--   terminus (not an occluder).
notBlocked ∷ WorldTileData → Int → Int → Double → Int → Int → Bool
notBlocked wtd ux uy eyeZ gx gy =
    let targetZ = case tileTerrainZ wtd gx gy of
            Just z  → fromIntegral z
            Nothing → 0     -- chunk not loaded → assume flat (arena)
        steps = max (abs (gx - ux)) (abs (gy - uy))
        n     = fromIntegral steps ∷ Double
    in steps ≤ 1 ∨ all (unblockedStep wtd ux uy gx gy eyeZ targetZ n) [1 .. steps - 1]

unblockedStep
    ∷ WorldTileData → Int → Int → Int → Int → Double → Double → Double
    → Int → Bool
unblockedStep wtd ux uy gx gy eyeZ targetZ n i =
    let t = fromIntegral i / n ∷ Double
        interpX = fromIntegral ux + t * fromIntegral (gx - ux)
        interpY = fromIntegral uy + t * fromIntegral (gy - uy)
        tx = floor interpX
        ty = floor interpY
        lineZ = eyeZ + t * (targetZ - eyeZ)
        groundZ = case tileTerrainZ wtd tx ty of
            Just z  → fromIntegral z
            Nothing → 0
    in groundZ ≤ lineZ

tileTerrainZ ∷ WorldTileData → Int → Int → Maybe Int
tileTerrainZ wtd gx gy =
    let (coord, (lx, ly)) = globalToChunk gx gy
    in case lookupChunk coord wtd of
        Nothing → Nothing
        Just lc → Just (lcTerrainSurfaceMap lc VU.! columnIndex lx ly)
