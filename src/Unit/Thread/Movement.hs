{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Per-tick movement for units.
--
-- Continuous motion in ℝ². Each tick:
--   * If `usLocalPath` is non-empty, head toward its first waypoint.
--     Pop the waypoint on arrival.
--   * Otherwise (greedy mode), head toward `usTarget`.
--   * Before stepping into a new tile, ask `stepCost`:
--       Nothing                → replan
--       cost > replan threshold (greedy mode only) → replan
--       cost ≤ threshold       → step
--
-- Replan = `localAStar` from current tile to the final target's
-- tile, storing the result as continuous tile-center waypoints in
-- `usLocalPath`. If A* finds no path at all, the unit stays idle this
-- tick and tries again next tick — the "never gives up" rule.
module Unit.Thread.Movement
    ( tickAllMovement
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Engine.Core.State (EngineEnv(..))
import World.Types (WorldManager(..), WorldState(..))
import World.Tile.Types (WorldTileData(..))
import Unit.Sim.Types
import Unit.Pathing.Cost (stepCost, lookupTerrainZ, replanCostThreshold)
import Unit.Pathing.AStar (localAStar, defaultMaxRadius)

-- | Distance below which the unit is considered arrived at a
--   waypoint or target. Larger than one tick of motion (≈ 0.066) so
--   the unit can't tick past a sub-goal and start oscillating.
arrivalEpsilon ∷ Float
arrivalEpsilon = 0.1

-- | Advance all units with active move targets, using cost-aware
--   greedy movement + local A* replan. World tile data is snapshotted
--   once per tick so all units in this batch see the same world.
tickAllMovement ∷ Double → EngineEnv → IORef UnitThreadState → IO ()
tickAllMovement dt env utsRef = do
    mWtd ← snapshotVisibleWorldTiles env
    now  ← realToFrac <$> getPOSIXTime
    uts ← readIORef utsRef
    let simStates  = utsSimStates uts
        simStates' = HM.map (tickUnit now dt mWtd) simStates
    writeIORef utsRef (uts { utsSimStates = simStates' })

snapshotVisibleWorldTiles ∷ EngineEnv → IO (Maybe WorldTileData)
snapshotVisibleWorldTiles env = do
    wm ← readIORef (worldManagerRef env)
    case wmVisible wm of
        []          → pure Nothing
        (pageId:_)  → case lookup pageId (wmWorlds wm) of
            Nothing → pure Nothing
            Just ws → Just <$> readIORef (wsTilesRef ws)

tickUnit ∷ Double → Double → Maybe WorldTileData → UnitSimState → UnitSimState
tickUnit now dt mWtd us =
    let us1 = handleReviveExpiry now us
    in case usTarget us1 of
        Nothing → us1
        Just mt →
            let subGoal = case usLocalPath us1 of
                    (p : _) → p
                    []      → (mtTargetX mt, mtTargetY mt)
            in stepTowardSubGoal dt mWtd us1 mt subGoal

-- | If the unit is Reviving and the revive's anim duration has
--   elapsed, snap back to Idle. Otherwise leave the state alone.
handleReviveExpiry ∷ Double → UnitSimState → UnitSimState
handleReviveExpiry now us = case usReviveUntil us of
    Just t | usState us ≡ Reviving ∧ now ≥ t →
        us { usState = Idle, usReviveUntil = Nothing }
    _ → us

-- | Try to advance toward `subGoal`. If we arrive, pop the waypoint
--   (or clear the final target). Otherwise, take one step.
stepTowardSubGoal
    ∷ Double
    → Maybe WorldTileData
    → UnitSimState
    → MoveTarget
    → (Float, Float)
    → UnitSimState
stepTowardSubGoal dt mWtd us mt (gx, gy) =
    let dx   = gx - usRealX us
        dy   = gy - usRealY us
        dist = sqrt (dx * dx + dy * dy)
        step = mtSpeed mt * realToFrac dt
    in if dist ≤ max step arrivalEpsilon
       then arriveAtSubGoal us mt (gx, gy) mWtd
       else moveToward us mt mWtd dx dy dist step

-- | Snap to the sub-goal. If we arrived at the final target (no more
--   waypoints, sub-goal is the target), clear the target. Otherwise
--   pop the first waypoint and continue next tick.
arriveAtSubGoal
    ∷ UnitSimState
    → MoveTarget
    → (Float, Float)
    → Maybe WorldTileData
    → UnitSimState
arriveAtSubGoal us mt (gx, gy) mWtd =
    let z   = lookupZ mWtd (floor gx) (floor gy) (usGridZ us)
        us' = us { usRealX = gx, usRealY = gy, usGridZ = z }
    in case usLocalPath us' of
        (_ : rest) →
            -- Popped a waypoint. If there are more, continue along the
            -- path; otherwise resume greedy heading toward the final
            -- target (unless we're already there).
            let arrivedAtFinal =
                    abs (gx - mtTargetX mt) < arrivalEpsilon
                    ∧ abs (gy - mtTargetY mt) < arrivalEpsilon
            in if null rest ∧ arrivedAtFinal
               then us' { usLocalPath = []
                        , usTarget    = Nothing
                        , usState     = Idle
                        }
               else us' { usLocalPath = rest
                        , usState     = Walking
                        }
        [] →
            -- Greedy mode: subGoal was the final target, so we've arrived.
            us' { usTarget = Nothing, usState = Idle }

-- | Step one tick toward the sub-goal. Cost-check first; on block or
--   high-cost (greedy mode only) trigger replan.
moveToward
    ∷ UnitSimState
    → MoveTarget
    → Maybe WorldTileData
    → Float    -- dx
    → Float    -- dy
    → Float    -- distance to sub-goal
    → Float    -- step length this tick
    → UnitSimState
moveToward us mt mWtd dx dy dist step =
    let nx   = dx / dist
        ny   = dy / dist
        newX = usRealX us + nx * step
        newY = usRealY us + ny * step
        srcTile = (floor (usRealX us), floor (usRealY us))
        dstTile = (floor newX, floor newY)
        mCost
            | srcTile ≡ dstTile = Just 0  -- sub-tile motion, no boundary cross
            | otherwise = case mWtd of
                Just wtd → stepCost wtd srcTile dstTile
                Nothing  → Just 0  -- no world snapshot: don't block
        followingPath = not (null (usLocalPath us))
    in case mCost of
        Nothing →
            replan us mt mWtd srcTile
        Just c | not followingPath ∧ c > replanCostThreshold →
            replan us mt mWtd srcTile
        Just _ →
            let (dgx, dgy) = dstTile
                newZ       = lookupZ mWtd dgx dgy (usGridZ us)
            in us { usRealX  = newX
                  , usRealY  = newY
                  , usGridZ  = newZ
                  , usFacing = vectorToDirection nx ny
                  , usState  = Walking
                  }

-- | Run local A* from `srcTile` to the final target's tile. Store
--   the resulting waypoints (in tile-center continuous coords) in
--   `usLocalPath`. If A* makes no progress (empty path), the unit
--   sits this tick — `usTarget` is preserved so the next tick can
--   try again ("never gives up").
replan
    ∷ UnitSimState
    → MoveTarget
    → Maybe WorldTileData
    → (Int, Int)
    → UnitSimState
replan us mt mWtd srcTile =
    let finalTile = (floor (mtTargetX mt), floor (mtTargetY mt))
        tilePath = case mWtd of
            Just wtd → localAStar wtd srcTile finalTile defaultMaxRadius
            Nothing  → []
        wps = map tileCenter tilePath
    in if null wps
       then us { usLocalPath = [], usState = Idle }
       else us { usLocalPath = wps, usState  = Walking }

tileCenter ∷ (Int, Int) → (Float, Float)
tileCenter (gx, gy) = (fromIntegral gx + 0.5, fromIntegral gy + 0.5)

lookupZ ∷ Maybe WorldTileData → Int → Int → Int → Int
lookupZ mWtd gx gy fallback = case mWtd of
    Just wtd → case lookupTerrainZ wtd gx gy of
        Just z  → z
        Nothing → fallback
    Nothing → fallback

-- | Map a tile-grid velocity vector to one of 8 compass directions.
--
-- The sprite labels are aligned with the iso projection's compass, not
-- the raw grid axes:
--   DirS  = tile-grid (+x, +y)  — projects to screen-down
--   DirSE = tile-grid (+x,  0)  — projects to screen-lower-right
--   DirE  = tile-grid (+x, -y)  — projects to screen-right
--   DirNE = tile-grid ( 0, -y)
--   DirN  = tile-grid (-x, -y)
--   DirNW = tile-grid (-x,  0)
--   DirW  = tile-grid (-x, +y)
--   DirSW = tile-grid ( 0, +y)
-- That makes the result in tile-grid frame, which is what the renderer
-- expects — `Unit.Render.resolveTexture` then applies `cameraRotSteps`
-- to keep the on-screen direction consistent through camera rotations.
vectorToDirection ∷ Float → Float → Direction
vectorToDirection nx ny
    | norm < 22.5   = DirSE
    | norm < 67.5   = DirS
    | norm < 112.5  = DirSW
    | norm < 157.5  = DirW
    | norm < 202.5  = DirNW
    | norm < 247.5  = DirN
    | norm < 292.5  = DirNE
    | norm < 337.5  = DirE
    | otherwise     = DirSE
  where
    deg  = atan2 ny nx * (180.0 / pi)
    norm = if deg < 0 then deg + 360 else deg
