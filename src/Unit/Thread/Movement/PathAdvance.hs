{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Per-unit step advance toward the current target/waypoint:
--   cost-aware greedy stepping with local A* replan, gait selection,
--   and the top-level per-unit tick (tickUnit) that stitches the timer
--   expiries + climb/fall Z-interpolation + stepping together.
module Unit.Thread.Movement.PathAdvance
    ( tickUnit
    , snapshotVisibleWorldTiles
    ) where

import UPrelude
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..))
import World.Types (WorldManager(..), WorldState(..))
import World.Tile.Types (WorldTileData(..))
import Unit.Sim.Types
import Unit.Pathing.Cost (stepCost, lookupTerrainZ, isCliffStep
                         , materialFactor, materialDetour
                         , slopeGrade, slopeSpeedFactor, PathingConfig(..))
import Unit.Pathing.AStar (localAStar, defaultMaxRadius)
import World.Material (MaterialRegistry)
import Unit.Thread.Movement.Types (UnitMoveStats(..), vectorToDirection)
import Unit.Thread.Movement.Climb (tickClimbZ, tickPullup, startClimb)
import Unit.Thread.Movement.Fall (tickFallZ, startFall)
import Unit.Thread.Movement.Timers
    (handleGetUp, handleTransitionExpiry, handlePickupExpiry
    , handleEatExpiry, handleDrinkExpiry)

-- | Distance below which the unit is considered arrived at a
--   waypoint or target. Larger than one tick of motion (≈ 0.066) so
--   the unit can't tick past a sub-goal and start oscillating.
arrivalEpsilon ∷ Float
arrivalEpsilon = 0.1

-- | KNOWN FOLLOW-UP (#797 audit clause): like the pre-#797 LOS code,
--   this snapshots wmVisible's HEAD page rather than each unit's own
--   uiPage — a unit on a secondary visible page paths against the
--   ACTIVE page's terrain. Left unfixed here deliberately: this feeds
--   the batched PER-TICK movement update for every moving unit, so a
--   per-unit-page fix is materially larger in scope than #797's LOS
--   change (a per-caller WorldTileData lookup, not a single shared
--   snapshot). Tracked as a deferred follow-up, not fixed in #797.
snapshotVisibleWorldTiles ∷ EngineEnv → IO (Maybe WorldTileData)
snapshotVisibleWorldTiles env = do
    wm ← readIORef (worldManagerRef env)
    case wmVisible wm of
        []          → pure Nothing
        (pageId:_)  → case lookup pageId (wmWorlds wm) of
            Nothing → pure Nothing
            Just ws → Just <$> readIORef (wsTilesRef ws)

tickUnit ∷ PathingConfig → MaterialRegistry → Double → Double → Maybe WorldTileData
         → UnitMoveStats
         → UnitSimState → UnitSimState
tickUnit pc reg now dt mWtd stats us =
    let us1 = handleGetUp now
            $ handleTransitionExpiry now
            $ handlePickupExpiry now
            $ handleEatExpiry now
            $ handleDrinkExpiry now us
        -- Climb-Z interpolation (Phase 1, Standing→Climbing) +
        -- Fall-Z interpolation (Standing→Falling, descent path).
        -- Climb lerps usRealZ upward; fall lerps it downward. Both
        -- pin xy at the start position for the whole transition.
        -- handleTransitionExpiry handles the landing snaps + the
        -- post-fall outcome routing (walk / collapse / kill).
        us2' = tickPullup now stats (tickFallZ now (tickClimbZ now stats us1))
        -- Clear last tick's slope grade (#375) so a unit that stops
        -- stepping (arrived, climbing, drinking, ...) doesn't keep
        -- reporting stale uphill exertion to the stamina drain. The
        -- stepping path below stamps the fresh value. Conditional so
        -- the common flat/idle case doesn't allocate a record update.
        us2 = if usMoveGrade us2' ≡ 0 then us2' else us2' { usMoveGrade = 0 }
    in case usState us2 of
        -- Stationary anim states block movement.
        Drinking            → us2
        Eating              → us2
        Picking             → us2
        TransitioningTo _   → us2
        _ → case usTarget us2 of
            Nothing → us2
            Just mt →
                let subGoal = case usLocalPath us2 of
                        (p : _) → p
                        []      → (mtTargetX mt, mtTargetY mt)
                in stepTowardSubGoal pc reg now dt mWtd stats us2 mt subGoal

-- | Try to advance toward `subGoal`. If we arrive, pop the waypoint
--   (or clear the final target). Otherwise, take one step.
stepTowardSubGoal
    ∷ PathingConfig
    → MaterialRegistry
    → Double
    → Double
    → Maybe WorldTileData
    → UnitMoveStats
    → UnitSimState
    → MoveTarget
    → (Float, Float)
    → UnitSimState
stepTowardSubGoal pc reg now dt mWtd stats us mt (gx, gy) =
    let dx   = gx - usRealX us
        dy   = gy - usRealY us
        dist = sqrt (dx * dx + dy * dy)
        -- A crawling unit (legs maimed) is capped to a crawl regardless of
        -- the commanded speed — it drags itself along the ground.
        effSpeed = if usPose us ≡ Crawling
                   then min (mtSpeed mt) crawlSpeed
                   else mtSpeed mt
        -- Surface-material slowdown (#312): the ground under the unit's
        -- feet divides its speed — loose/soft terrain (sand, silt, mud)
        -- has move_cost > 1.0 and so is crossed slower than firm rock.
        -- The greedy stepper reads stepCost only for its replan trigger,
        -- so the speed effect must be applied to the step length HERE
        -- (the same factor stepCost folds into the planned route cost).
        matSlow = case mWtd of
            Just wtd → materialFactor reg wtd (floor (usRealX us)) (floor (usRealY us))
            Nothing  → 1.0
        -- Slope grade under the unit's feet (#375): walking up a ramp's
        -- fall line scales speed down (steeper heading = slower),
        -- downhill up slightly. Same call-site pattern as the material
        -- factor above — routing already charges pcRampFactor for the
        -- climb; this makes the traversal itself cost time. The grade is
        -- stamped onto the sim state so the Lua stamina drain can tax
        -- sustained uphill travel (getInfo's moveGrade).
        grade = case mWtd of
            Just wtd | dist > 1e-6 →
                slopeGrade wtd (floor (usRealX us)) (floor (usRealY us))
                           (usGridZ us) (dx / dist, dy / dist)
            _ → 0
        step = (effSpeed * slopeSpeedFactor pc grade / matSlow)
             * realToFrac dt
    in if dist ≤ max step arrivalEpsilon
       then arriveAtSubGoal stats us mt (gx, gy) mWtd
       else moveToward pc reg now stats (us { usMoveGrade = grade })
                       mt mWtd dx dy dist step

-- | Top speed (tiles/sec) of a unit dragging itself along on a maimed
--   body. Slow enough to read as a crawl; the injury speed-multiplier
--   already applied at command time stacks on top.
crawlSpeed ∷ Float
crawlSpeed = 0.7

-- | Snap to the sub-goal. If we arrived at the final target (no more
--   waypoints, sub-goal is the target), clear the target. Otherwise
--   pop the first waypoint and continue next tick.
arriveAtSubGoal
    ∷ UnitMoveStats
    → UnitSimState
    → MoveTarget
    → (Float, Float)
    → Maybe WorldTileData
    → UnitSimState
arriveAtSubGoal stats us mt (gx, gy) mWtd =
    let z   = lookupZ mWtd (floor gx) (floor gy) (usGridZ us)
        us' = us { usRealX = gx, usRealY = gy
                 , usGridZ = z, usRealZ = fromIntegral z }
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
                        , usState     = gaitForPose (usPose us') stats mt
                        }
        [] →
            -- Greedy mode: subGoal was the final target, so we've arrived.
            us' { usTarget = Nothing, usState = Idle }

-- | Step one tick toward the sub-goal. Cost-check first; on block or
--   high-cost (greedy mode only) trigger replan. If the next tile
--   crossing is a cliff (Z step that has no slope ramp), initiate a
--   climb transition instead of taking the step.
moveToward
    ∷ PathingConfig
    → MaterialRegistry
    → Double             -- now (game time, for transition expiry)
    → UnitMoveStats
    → UnitSimState
    → MoveTarget
    → Maybe WorldTileData
    → Float    -- dx
    → Float    -- dy
    → Float    -- distance to sub-goal
    → Float    -- step length this tick
    → UnitSimState
moveToward pc reg now stats us mt mWtd dx dy dist step =
    let nx   = dx / dist
        ny   = dy / dist
        newX = usRealX us + nx * step
        newY = usRealY us + ny * step
        srcTile = (floor (usRealX us), floor (usRealY us))
        dstTile = (floor newX, floor newY)
        -- stepCost enforces the no-corner-cutting rule itself (a
        -- diagonal step grazing an impassable axis-neighbour returns
        -- Nothing), so the greedy stepper and A* agree by construction.
        mCost
            | srcTile ≡ dstTile = Just 0  -- sub-tile motion, no boundary cross
            | otherwise = case mWtd of
                Just wtd → stepCost pc reg wtd srcTile dstTile
                Nothing  → Just 0  -- no world snapshot: don't block
        followingPath = not (null (usLocalPath us))
        -- Soft-ground detour trigger (#312). Material step costs are mild
        -- (sand 1.5, mud 1.8) — far below pcReplanCostThreshold — so the
        -- cost-based replan above never fires for them. This fires a local
        -- A* check when the unit steps onto soft ground; A* skirts the
        -- patch only if a firmer route is cheaper. It re-fires as the unit
        -- crosses a wide soft field (so a firmer route beyond the first
        -- bounded-A* horizon is eventually found), but stays cheap because
        -- it's consulted only in greedy mode — once a local path is set
        -- the unit follows it (no replan) until it ends, so this costs
        -- ~one A* per path-length of soft travel, not one per step.
        matEdge = srcTile ≢ dstTile ∧ case mWtd of
            Just wtd → materialDetour pc reg wtd dstTile
            Nothing  → False
        -- Cliff and fall detection: only meaningful when actually
        -- crossing a tile boundary. The pathfinder already rejects
        -- most cliffs via replanCostThreshold, but when the unit
        -- must climb (or drop into a fall) there's no alternative.
        --
        --   * mCliff: dz > 0 + no walkable slope ⇒ start climb.
        --   * mFall:  dz ≤ -fallTriggerDz       ⇒ start fall. Smaller
        --             drops (dz = -1) walk off normally; the engine's
        --             usual Z-snap path handles them silently.
        mCliff = case mWtd of
            Just wtd | srcTile ≢ dstTile →
                case (lookupTerrainZ wtd (fst srcTile) (snd srcTile),
                      lookupTerrainZ wtd (fst dstTile) (snd dstTile)) of
                    (Just sz, Just dz)
                        | isCliffStep wtd srcTile dstTile sz dz →
                            Just (sz, dz)
                    _ → Nothing
            _ → Nothing
        mFall = case mWtd of
            Just wtd | srcTile ≢ dstTile →
                case (lookupTerrainZ wtd (fst srcTile) (snd srcTile),
                      lookupTerrainZ wtd (fst dstTile) (snd dstTile)) of
                    (Just sz, Just dz)
                        | sz - dz ≥ pcFallTriggerDrop pc → Just (sz, dz)
                    _ → Nothing
            _ → Nothing
    in case mCost of
        Nothing →
            replan pc reg us mt mWtd srcTile
        Just c | not followingPath ∧ (c > pcReplanCostThreshold pc ∨ matEdge) →
            replan pc reg us mt mWtd srcTile
        Just _ → case (mCliff, mFall) of
            (Just (srcZ, dstZ), _) →
                -- Face the CLIFF, not the unit's walking sub-step.
                -- A unit angling into the cliff (e.g. mostly east,
                -- a bit south) would otherwise face DirS while
                -- climbing an east-facing cliff — the climb anim
                -- would render perpendicular. The cliff direction
                -- is the tile-grid delta from source to dest tile.
                let (sx, sy)   = srcTile
                    (dgx, dgy) = dstTile
                    cliffDx    = fromIntegral (dgx - sx) ∷ Float
                    cliffDy    = fromIntegral (dgy - sy) ∷ Float
                in startClimb now stats us (dstTile, dstZ) srcZ
                              (cliffDx, cliffDy)
            (_, Just (srcZ, dstZ)) →
                -- Fall: same facing logic as climb, but the unit
                -- launches into the air rather than grabbing rock.
                let (sx, sy)   = srcTile
                    (dgx, dgy) = dstTile
                    fallDx     = fromIntegral (dgx - sx) ∷ Float
                    fallDy     = fromIntegral (dgy - sy) ∷ Float
                in startFall now us (dstTile, dstZ) srcZ
                             (fallDx, fallDy)
            _ →
                let (dgx, dgy) = dstTile
                    newZ       = lookupZ mWtd dgx dgy (usGridZ us)
                in us { usRealX  = newX
                      , usRealY  = newY
                      , usGridZ  = newZ
                      , usRealZ  = fromIntegral newZ
                      , usFacing = vectorToDirection nx ny
                      , usState  = gaitForPose (usPose us) stats mt
                      }

-- | Walking vs Running gait, by whether the commanded speed crosses the
--   unit's run-anim threshold (def.run_threshold × def.max_speed). This
--   is the fix for "units never run": the per-tick movement update used
--   to hard-code Walking, clobbering the Running activity set at command
--   time. Now the gait is re-derived from speed every tick so it sticks.
gaitFor ∷ UnitMoveStats → MoveTarget → UnitActivity
gaitFor stats mt
    | mtSpeed mt > umsRunThreshold stats = Running
    | otherwise                          = Walking

-- | Gait that respects the pose: a Crawling unit is always Walking-gait
--   (there is only a crawling-walk anim, no crawling-run), so a unit told
--   to move fast still renders crawling rather than falling back.
gaitForPose ∷ Pose → UnitMoveStats → MoveTarget → UnitActivity
gaitForPose Crawling _ _ = Walking
gaitForPose _ stats mt   = gaitFor stats mt

-- | Run local A* from `srcTile` to the final target's tile. Store
--   the resulting waypoints (in tile-center continuous coords) in
--   `usLocalPath`. If A* makes no progress (empty path), the unit
--   sits this tick — `usTarget` is preserved so the next tick can
--   try again ("never gives up").
replan
    ∷ PathingConfig
    → MaterialRegistry
    → UnitSimState
    → MoveTarget
    → Maybe WorldTileData
    → (Int, Int)
    → UnitSimState
replan pc reg us mt mWtd srcTile =
    let finalTile = (floor (mtTargetX mt), floor (mtTargetY mt))
        tilePath = case mWtd of
            Just wtd → localAStar pc reg wtd srcTile finalTile defaultMaxRadius
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
