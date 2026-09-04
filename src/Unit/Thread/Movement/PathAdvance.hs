{-# LANGUAGE Strict #-}
-- | Per-unit step advance toward the current target/waypoint:
--   cost-aware greedy stepping with local A* replan, gait selection,
--   and the top-level per-unit tick (tickUnit) that stitches the timer
--   expiries + climb/fall Z-interpolation + stepping together.
module Unit.Thread.Movement.PathAdvance
    ( tickUnit
    , snapshotOwnedWorldTiles
    , TerrainSnapshots
    , MoveWorld(..)
    , moveWorldFor
    , maxProtectedStep
    , rawStepLength
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (readIORef)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import World.Types (WorldManager(..), WorldState(..))
import World.Page.Types (WorldPageId(..))
import World.Tile.Types (WorldTileData(..))
import Unit.Sim.Types
import Unit.Pathing.Cost (stepCostUnder, lookupTerrainZ, isCliffStep
                         , isDamagingDrop
                         , materialFactor, materialDetour
                         , slopeGrade, slopeSpeedFactor, PathingConfig(..))
import Unit.Pathing.AStar (localAStarUnder, defaultMaxRadius)
import World.Material (MaterialRegistry)
import Unit.Thread.Movement.Types (UnitMoveStats(..), vectorToDirection)
import Unit.Thread.Movement.Climb (tickClimbZ, tickPullup, startClimb)
import Unit.Thread.Movement.Fall (tickFallZ, startFall)
import Unit.Thread.Movement.Timers
    (handleGetUp, handleTransitionExpiry, handlePickupExpiry
    , handleEatExpiry, handleDrinkExpiry)

-- | The raw distance one tick advances a mover, before the
--   protected-step clamp: the commanded speed, scaled by the slope grade
--   under the unit's feet and DIVIDED by that ground's
--   'Unit.Pathing.Cost.materialFactor', over @dt@ seconds.
--
--   Split out of 'stepTowardSubGoal' so the runtime half of the
--   @move_cost@ domain (#1734) can be asserted against the exact
--   arithmetic the mover runs rather than a restatement of it. Note what
--   that arithmetic does and does not promise: with a positive finite
--   @effSpeed@ and @dt@, a FINITE POSITIVE @matSlow@ is what keeps the
--   result positive — but a caller-supplied zero or invalid speed or
--   @dt@ zeroes it independently of any material, which is why the
--   material invariant is stated over the factor, not over every tick.
rawStepLength ∷ PathingConfig → Float → Float → Float → Double → Float
rawStepLength pc effSpeed grade matSlow dt =
    (effSpeed * slopeSpeedFactor pc grade / matSlow) * realToFrac dt

-- | Distance below which the unit is considered arrived at a
--   waypoint or target. Larger than one tick of motion (≈ 0.066) so
--   the unit can't tick past a sub-goal and start oscillating.
arrivalEpsilon ∷ Float
arrivalEpsilon = 0.1

-- | The terrain this movement batch paths against, keyed by the page it
--   came from: one 'WorldTileData' per page some mover in the batch OWNS
--   (#1593). A page missing from the map — unloaded, or simply owned by
--   nobody moving this tick — yields no terrain at all rather than
--   another world's heightmap.
type TerrainSnapshots = HM.HashMap WorldPageId WorldTileData

-- | Read each requested page's tiles ONCE, so every mover in one batch
--   sees a single consistent view of its own page (#1593 requirement 3).
--
--   Resolution is through @wmWorlds@, never @wmVisible@: a mover's own
--   page is its terrain whether or not that page is currently visible,
--   and a page absent from @wmWorlds@ contributes no entry. Nothing here
--   consults or changes visibility.
snapshotOwnedWorldTiles ∷ WorldSimCapability → HS.HashSet WorldPageId
                        → IO TerrainSnapshots
snapshotOwnedWorldTiles wsc pages
  | HS.null pages = pure HM.empty
  | otherwise = do
      wm ← readIORef (wsWorldManagerRef wsc)
      HM.fromList <$> sequence
          [ (,) pageId <$> readIORef (wsTilesRef ws)
          | (pageId, ws) ← wmWorlds wm
          , HS.member pageId pages ]

-- | What the per-unit stepping path knows about the terrain it is
--   pathing against: the tiles, plus whether they are VERIFIED to be
--   this mover's own page.
data MoveWorld = MoveWorld
    { mwTiles   ∷ !(Maybe WorldTileData)
    , mwOwnPage ∷ !Bool
      -- ^ True exactly when 'mwTiles' holds the mover's own page's
      --   terrain. Since #1593 the two move together — a mover is only
      --   ever handed its OWN page's tiles — so a missing page, an
      --   unloaded page and a mover with no instance all read as
      --   @(Nothing, False)@, which is what a 'FallProhibited' request
      --   fails closed on.
    }

-- | Build the per-unit view of the batch's terrain snapshots: the mover's
--   OWN page's tiles, or nothing at all.
--
--   The mover's page is 'Nothing' when the unit has no instance in the
--   manager (a sim state outliving its unit) — which is exactly a case a
--   protected request must not path through, and since #1593 a case that
--   gets no terrain rather than the active page's.
moveWorldFor ∷ TerrainSnapshots → Maybe WorldPageId → MoveWorld
moveWorldFor snaps mMoverPage =
    let mTiles = mMoverPage ⌦ \pid → HM.lookup pid snaps
    in MoveWorld { mwTiles = mTiles, mwOwnPage = isJust mTiles }

tickUnit ∷ PathingConfig → MaterialRegistry → Double → Double → MoveWorld
         → UnitMoveStats
         → UnitSimState → UnitSimState
tickUnit pc reg now dt mw stats us =
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
            -- Fail-closed terrain gate (#1217): a hazard-PROTECTED
            -- request may only advance against terrain verified to be
            -- this mover's own page. Anything else — no snapshot, no
            -- visible page, an unresolvable page, or another page's
            -- heightmap — abandons the request so the ambient AI
            -- resamples later, rather than judging cliffs against the
            -- wrong world. Fall-permitted requests are untouched.
            Just mt
                | mtHazard mt ≡ FallProhibited ∧ not (mwOwnPage mw) →
                    abandonTarget us2
            Just mt →
                let subGoal = case usLocalPath us2 of
                        (p : _) → p
                        []      → (mtTargetX mt, mtTargetY mt)
                in stepTowardSubGoal pc reg now dt mw stats us2 mt subGoal

-- | Drop the in-flight request entirely: no target, no local path,
--   Idle. The terminal state of a 'FallProhibited' request that can make
--   no safe progress — deliberately NOT the fall-permitted "never gives
--   up" behavior (which keeps the target and retries every tick), so an
--   ambient mover is free to pick a different destination on a later
--   thought tick instead of replanning an unreachable one forever.
abandonTarget ∷ UnitSimState → UnitSimState
abandonTarget us = us { usTarget = Nothing, usLocalPath = [], usState = Idle }

-- | Try to advance toward `subGoal`. If we arrive, pop the waypoint
--   (or clear the final target). Otherwise, take one step.
stepTowardSubGoal
    ∷ PathingConfig
    → MaterialRegistry
    → Double
    → Double
    → MoveWorld
    → UnitMoveStats
    → UnitSimState
    → MoveTarget
    → (Float, Float)
    → UnitSimState
stepTowardSubGoal pc reg now dt mw stats us mt (gx, gy) =
    let mWtd = mwTiles mw
        dx   = gx - usRealX us
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
        rawStep = rawStepLength pc effSpeed grade matSlow dt
        -- A protected tick may not span more than one tile boundary —
        -- see `maxProtectedStep`. Fall-permitted movement keeps its exact
        -- uncapped speed.
        step = case mtHazard mt of
            FallPermitted  → rawStep
            -- Bound the MAGNITUDE, not just the upper end: a large
            -- negative step spans just as many tiles (backwards) as a
            -- large positive one, and this clamp is the only thing
            -- standing between one and a skipped tile boundary. Since
            -- #2290 both the `unit.moveTo` ingress and the
            -- `UnitMoveTo` handler refuse a negative speed, so no
            -- SPEED can produce one any more — but `rawStepLength`
            -- also multiplies by a grade and a material factor, so the
            -- bound stays two-sided rather than resting on a caller's
            -- domain. A non-finite step refuses to move
            -- at all, the fail-closed posture the rest of the policy
            -- takes — with the isNaN test FIRST, because every comparison
            -- against NaN is False and a bare clamp chain would launder it
            -- straight through (the same reasoning as
            -- `Unit.Pathing.Cost.clampStepCost`).
            FallProhibited
                | isNaN rawStep → 0
                | otherwise     → max (negate maxProtectedStep)
                                      (min maxProtectedStep rawStep)
        -- Arrival SNAPS x/y and re-grounds z at the sub-goal without
        -- consulting the cost function at all, so a sub-goal within
        -- `max step arrivalEpsilon` on the far side of a tile boundary
        -- is crossed by the snap rather than by a step. For a protected
        -- request that is a third way over a damaging drop, past both
        -- the greedy stepper and A* (#1217, review round 1): a wander
        -- target sampled just over a ledge is reached at 7.95 → 8.04 and
        -- the unit lands at the bottom with no fall and no check.
        --
        -- So the snap is validated by the SAME function every other
        -- crossing goes through — and ONLY when the request is
        -- protected, because a fall-permitted arrival has never consulted
        -- it and must stay byte-for-byte what it was. Missing or
        -- unreadable terrain fails closed here for the same reason it
        -- does everywhere else a protected request touches it.
        srcTile = (floor (usRealX us), floor (usRealY us))
        goalTile = (floor gx, floor gy) ∷ (Int, Int)
        snapBlocked = mtHazard mt ≡ FallProhibited
                    ∧ srcTile ≢ goalTile
                    ∧ isNothing (do wtd ← mWtd
                                    stepCostUnder FallProhibited pc reg wtd
                                                  srcTile goalTile)
    in if dist ≤ max step arrivalEpsilon
       then if snapBlocked
            -- No safe way onto the sub-goal's tile. Replan from here;
            -- if A* can't make safe progress either, it terminates the
            -- request (see `replan`) so the ambient AI resamples.
            then replan pc reg (us { usMoveGrade = grade }) mt mw srcTile
            else arriveAtSubGoal stats us mt (gx, gy) mWtd
       else moveToward pc reg now stats (us { usMoveGrade = grade })
                       mt mw dx dy dist step

-- | Ceiling on a hazard-PROTECTED request's per-tick displacement, in
--   tiles (#1217, review round 2).
--
--   `dt` is an uncapped wall-clock delta ("Unit.Thread"'s @unitTick@) and
--   @unit.moveTo@ takes an uncapped speed, so one tick's motion can span
--   several tiles. Both the greedy check and the arrival snap validate
--   only the tick's START and END tiles, so a multi-tile span could step
--   clean over an intermediate damaging drop and land on a below-trigger
--   one: z=10 across an intermediate z=8 onto a z=9 tile reads as an
--   ordinary 1-z walk-off, and the unit crosses the 2-z drop untouched.
--
--   Rather than marching the crossed boundaries, a protected tick simply
--   cannot span more than one. A displacement strictly under 1 tile moves
--   each axis' `floor` by at most 1, so the destination tile is always
--   8-connected to the source — which makes the SINGLE `stepCostUnder`
--   check the COMPLETE check, and keeps the mover's step model identical
--   to the one A* plans in. 0.9 bounds each axis component, since
--   @|Δx| = |nx| * step ≤ step@.
--
--   The cost is that a protected request commanded faster than ~0.9
--   tiles per tick travels at that cap instead. Ambient wander is a
--   meander and never approaches it; a fall-permitted request is
--   unaffected either way.
maxProtectedStep ∷ Float
maxProtectedStep = 0.9

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
    → MoveWorld
    → Float    -- dx
    → Float    -- dy
    → Float    -- distance to sub-goal
    → Float    -- step length this tick
    → UnitSimState
moveToward pc reg now stats us mt mw dx dy dist step =
    let mWtd = mwTiles mw
        nx   = dx / dist
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
                -- The request's own hazard policy (#1217) reaches the
                -- greedy stepper and A* through the SAME cost function,
                -- so a protected route can't lose its protection by
                -- being planned one way rather than the other.
                Just wtd → stepCostUnder (mtHazard mt) pc reg wtd srcTile dstTile
                -- No world snapshot: a fall-permitted move doesn't block
                -- (its historical behavior). A protected move can't get
                -- here — tickUnit abandons an unverified snapshot before
                -- stepping — but refuse rather than step blind.
                Nothing  → case mtHazard mt of
                    FallPermitted  → Just 0
                    FallProhibited → Nothing
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
                        | isDamagingDrop pc sz dz → Just (sz, dz)
                    _ → Nothing
            _ → Nothing
    in case mCost of
        Nothing →
            replan pc reg us mt mw srcTile
        Just c | not followingPath ∧ (c > pcReplanCostThreshold pc ∨ matEdge) →
            replan pc reg us mt mw srcTile
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
--   `usLocalPath`. If A* makes no progress (empty path), a
--   fall-permitted request sits this tick — `usTarget` is preserved so
--   the next tick can try again ("never gives up").
--
--   A 'FallProhibited' request TERMINATES instead (#1217): "no safe
--   progress" is a real answer for ambient movement, and holding an
--   unreachable target would replan it every movement tick forever
--   while the ambient AI, seeing a still-active move, never resamples.
replan
    ∷ PathingConfig
    → MaterialRegistry
    → UnitSimState
    → MoveTarget
    → MoveWorld
    → (Int, Int)
    → UnitSimState
replan pc reg us mt mw srcTile =
    let finalTile = (floor (mtTargetX mt), floor (mtTargetY mt))
        tilePath = case mwTiles mw of
            Just wtd → localAStarUnder (mtHazard mt) pc reg wtd srcTile
                                       finalTile defaultMaxRadius
            Nothing  → []
        wps = map tileCenter tilePath
    in if null wps
       then case mtHazard mt of
                FallPermitted  → us { usLocalPath = [], usState = Idle }
                FallProhibited → abandonTarget us
       else us { usLocalPath = wps, usState  = Walking }

tileCenter ∷ (Int, Int) → (Float, Float)
tileCenter (gx, gy) = (fromIntegral gx + 0.5, fromIntegral gy + 0.5)

lookupZ ∷ Maybe WorldTileData → Int → Int → Int → Int
lookupZ mWtd gx gy fallback = case mWtd of
    Just wtd → case lookupTerrainZ wtd gx gy of
        Just z  → z
        Nothing → fallback
    Nothing → fallback
