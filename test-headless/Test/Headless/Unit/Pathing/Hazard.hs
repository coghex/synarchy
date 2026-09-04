{-# LANGUAGE Strict #-}
-- | Pure tests for the per-request movement hazard policy (#1217).
--
--   Three layers, all engine-free:
--
--     * the COST layer — 'stepCostUnder' at the configured
--       @fall_trigger_drop@ boundary, and the A* search that consults it;
--     * the TICK layer — 'tickUnit' on handcrafted terrain, proving a
--       protected request never launches a fall, terminates when it can
--       make no safe progress, and fails closed on terrain that isn't
--       verified to be the mover's own page, while a fall-permitted
--       request keeps its "never gives up" behavior there. Since #1593 a
--       mover is only ever handed its OWN page's tiles, so "another
--       page's terrain" and "no terrain" are the same input; the
--       PER-PAGE resolution that produces it is
--       'Test.Headless.Unit.SimPageOwnership';
--     * the WIRING layer — source guards proving every shipped aimless
--       mover (acolyte and technomule @wander@, @bear_wander@,
--       @squirrel_wander@) selects the ONE shared mechanism, and that the
--       mental-state / flee call sites keep the default.
module Test.Headless.Unit.Pathing.Hazard (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..), chunkSize)
import World.Tile.Types (WorldTileData(..))
import World.Fluid.Types (emptyIceMap)
import World.Flora.Types (emptyFloraChunkData)
import World.Page.Types (WorldPageId(..))
import Structure.Types (emptyChunkStructures)
import World.Material (MaterialRegistry, emptyMaterialRegistry)
import Unit.Pathing.Cost
import Unit.Pathing.AStar (localAStar, localAStarUnder, defaultMaxRadius)
import Unit.Sim.Types
import Unit.Thread.Movement.PathAdvance
    (tickUnit, moveWorldFor, TerrainSnapshots, MoveWorld(..)
    , maxProtectedStep)
import Unit.Thread.Movement.Types (UnitMoveStats(..), defaultMoveStats)

-- ---------------------------------------------------------------------
-- Terrain fixtures
-- ---------------------------------------------------------------------

-- | A single origin chunk whose terrain z is set per local tile.
zChunk ∷ ((Int, Int) → Int) → LoadedChunk
zChunk f =
    let area  = chunkSize * chunkSize
        terrV = VU.generate area $ \i →
            f (i `mod` chunkSize, i `div` chunkSize)
    in LoadedChunk
        { lcCoord             = ChunkCoord 0 0
        , lcTiles             = V.empty
        , lcSurfaceMap        = terrV
        , lcTerrainSurfaceMap = terrV
        , lcFluidMap          = V.replicate area Nothing
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.empty
        , lcWaterTableMap     = VU.empty
        , lcMagma             = Nothing
        , lcStructures        = emptyChunkStructures
        }

worldWith ∷ LoadedChunk → WorldTileData
worldWith lc = WorldTileData
    { wtdChunks = HM.singleton (ChunkCoord 0 0) lc, wtdMaxChunks = 1 }

-- | A north–south ridge: everything at x ≤ 7 sits on a plateau at
--   @high@, everything east of it lies at @low@. Crossing x = 7 → x = 8
--   anywhere is a @high - low@ drop, and there is no way around it
--   inside the chunk.
ridgeWorld ∷ Int → Int → WorldTileData
ridgeWorld high low = worldWith $ zChunk $ \(lx, _) →
    if lx ≤ 7 then high else low

-- | A one-tile SHELF at x = 8: the plateau (x ≤ 7) is at z = 10, the
--   shelf itself is at z = 8, and everything east of it sits at z = 9.
--
--   The point is the ENDPOINT ILLUSION: 10 → 9 is a below-trigger
--   walk-off, so a check that only looks at a tick's start and end tiles
--   waves the crossing through, while the real path drops 2 z onto the
--   shelf first.
shelfWorld ∷ WorldTileData
shelfWorld = worldWith $ zChunk $ \(lx, _) → case compare lx 8 of
    LT → 10
    EQ → 8
    GT → 9

-- | A diagonal-corner fixture. The plateau (x ≤ 7) is at z = 10, the
--   graze tile (8, 3) is at @via@, and the diagonal's own destination
--   (8, 4) is at 9 — only 1 z down, below the trigger, and so invisible
--   to any check that looks at the diagonal's ENDPOINTS alone.
--
--   Only @via@ varies, so each case below isolates exactly one edge of
--   the two-step route the diagonal's continuous path really takes.
cornerWorldVia ∷ Int → WorldTileData
cornerWorldVia via = worldWith $ zChunk $ \(lx, ly) →
    if lx ≤ 7 then 10 else if ly ≤ 3 then via else 9

-- | Uniformly flat ground at z = 10: no drop anywhere, so both policies
--   must agree on every route across it.
flatWorld ∷ WorldTileData
flatWorld = worldWith (zChunk (const 10))

pc ∷ PathingConfig
pc = defaultPathingConfig

-- | The same tunables with a normalized @fall_trigger_drop@ of 1, so a
--   SINGLE-z drop is a damaging fall. The shipped default (2) is only
--   the shipped default — the policy must key on the CONFIGURED value.
pcTrigger1 ∷ PathingConfig
pcTrigger1 = pc { pcFallTriggerDrop = 1 }

reg ∷ MaterialRegistry
reg = emptyMaterialRegistry

-- ---------------------------------------------------------------------
-- Sim-state fixture
-- ---------------------------------------------------------------------

-- | A standing unit at the tile centre of (gx, gy) on the plateau,
--   holding one move request.
moverAt ∷ (Float, Float) → Int → MoveTarget → UnitSimState
moverAt (x, y) z mt = UnitSimState
    { usRealX = x, usRealY = y, usGridZ = z, usRealZ = fromIntegral z
    , usTarget = Just mt
    , usPose = Standing, usState = Walking, usFacing = DirE
    , usLocalPath = []
    , usDrinkUntil = Nothing, usEatUntil = Nothing, usPickupUntil = Nothing
    , usTransitionUntil = Nothing, usTransitionStride = 1
    , usPostTransition = []
    , usClimbFromTile = Nothing, usClimbToTile = Nothing
    , usClimbStartTime = Nothing, usClimbSlipAt = Nothing
    , usFallFromTile = Nothing, usFallToTile = Nothing
    , usPendingClimbXP = 0, usGetUpAt = Nothing, usPendingFallDrop = Nothing
    , usJumpApex = Nothing, usMoveGrade = 0
    }

stats ∷ UnitMoveStats
stats = defaultMoveStats

pageA, pageB ∷ WorldPageId
pageA = WorldPageId "page-a"
pageB = WorldPageId "page-b"

-- | A batch that snapshotted exactly one page.
snapshotOf ∷ WorldPageId → WorldTileData → TerrainSnapshots
snapshotOf = HM.singleton

-- | Own-page terrain: the batch snapshotted this mover's page.
ownPageWorld ∷ WorldTileData → MoveWorld
ownPageWorld wtd = moveWorldFor (snapshotOf pageA wtd) (Just pageA)

-- | Run the movement tick repeatedly at a fixed dt, collecting each
--   intermediate state. Deterministic — 'tickUnit' is pure.
runTicks ∷ PathingConfig → MoveWorld → Int → UnitSimState → [UnitSimState]
runTicks cfg mw = runTicksAt cfg mw 0.1

-- | 'runTicks' at an explicit tick delta, so a test can hand the mover
--   the large @dt@ a stalled or resumed process really does produce.
runTicksAt ∷ PathingConfig → MoveWorld → Double → Int → UnitSimState
           → [UnitSimState]
runTicksAt cfg mw dt n us0 = go n 0 us0 []
  where
    go 0 _ us acc = reverse (us : acc)
    go k t us acc =
        let t'  = t + dt
            us' = tickUnit cfg reg t' dt mw stats us
        in go (k - 1 ∷ Int) t' us' (us : acc)

isFalling ∷ UnitSimState → Bool
isFalling us = usState us ≡ TransitioningTo Falling

-- | Read a shipped script; the suite runs from the repository root.
readScript ∷ FilePath → IO Text
readScript = TIO.readFile

-- ---------------------------------------------------------------------

spec ∷ Spec
spec = do
  describe "wander hazard policy" $ do

    describe "stepCostUnder at the configured fall boundary" $ do
        it "rejects a step whose drop reaches fall_trigger_drop" $
            -- x = 7 → x = 8 crosses the ridge: a 4-z drop, default
            -- trigger 2.
            stepCostUnder FallProhibited pc reg (ridgeWorld 10 6) (7, 3) (8, 3)
                `shouldBe` Nothing

        it "allows the same step when falls are permitted" $
            case stepCostUnder FallPermitted pc reg (ridgeWorld 10 6) (7, 3) (8, 3) of
                Nothing → expectationFailure "fall-permitted step was rejected"
                Just c  → c `shouldSatisfy` (> 0)

        it "allows a drop strictly below the trigger under both policies" $ do
            -- A 1-z drop against the shipped trigger of 2 is an ordinary
            -- walk-off, protected or not.
            let w = ridgeWorld 10 9
            stepCostUnder FallProhibited pc reg w (7, 3) (8, 3)
                `shouldBe` stepCostUnder FallPermitted pc reg w (7, 3) (8, 3)
            stepCostUnder FallProhibited pc reg w (7, 3) (8, 3)
                `shouldSatisfy` isJust

        it "rejects a drop exactly AT the trigger" $
            stepCostUnder FallProhibited pc reg (ridgeWorld 10 8) (7, 3) (8, 3)
                `shouldBe` Nothing

        it "keys on the CONFIGURED trigger, not the shipped default" $ do
            -- The same single-z drop the shipped default allows is a
            -- damaging fall once fall_trigger_drop is 1.
            let w = ridgeWorld 10 9
            stepCostUnder FallProhibited pcTrigger1 reg w (7, 3) (8, 3)
                `shouldBe` Nothing
            stepCostUnder FallPermitted pcTrigger1 reg w (7, 3) (8, 3)
                `shouldSatisfy` isJust

        it "never blocks an ascent, however steep" $
            -- Protection is about damaging DROPS only; climbing the same
            -- ridge from below stays priced, not forbidden.
            stepCostUnder FallProhibited pc reg (ridgeWorld 10 6) (8, 3) (7, 3)
                `shouldSatisfy` isJust

        it "rejects a protected DIAGONAL whose graze tile is a drop ONTO" $ do
            -- (7,3) → (8,4) descends only 1 z, but the step's continuous
            -- path clips (8,3), which is 4 z DOWN from the source.
            stepCostUnder FallProhibited pc reg (cornerWorldVia 6) (7, 3) (8, 4)
                `shouldBe` Nothing
            -- The same diagonal stays priced, not forbidden, by default.
            stepCostUnder FallPermitted pc reg (cornerWorldVia 6) (7, 3) (8, 4)
                `shouldSatisfy` isJust

        it "rejects a protected DIAGONAL whose graze tile is a drop OFF" $ do
            -- The edge the first-edge-only rule missed: the graze tile is
            -- 1 z ABOVE the source, so both the direct descent (10 → 9)
            -- and the descent onto it (10 → 11) look harmless — while the
            -- real 2-z drop is 11 → 9, on the way OFF it.
            stepCostUnder FallProhibited pc reg (cornerWorldVia 11) (7, 3) (8, 4)
                `shouldBe` Nothing
            stepCostUnder FallPermitted pc reg (cornerWorldVia 11) (7, 3) (8, 4)
                `shouldSatisfy` isJust

        it "allows a protected DIAGONAL whose graze tiles are both clean" $ do
            -- Both two-step routes descend 1 z at most, so the diagonal is
            -- ordinary movement — the rule refuses hazards, not diagonals.
            stepCostUnder FallProhibited pc reg (cornerWorldVia 10) (7, 3) (8, 4)
                `shouldSatisfy` isJust
            stepCostUnder FallProhibited pc reg flatWorld (2, 2) (3, 3)
                `shouldSatisfy` isJust

        it "isDamagingDrop is the one classification both sides use" $ do
            isDamagingDrop pc 10 8 `shouldBe` True    -- exactly the trigger
            isDamagingDrop pc 10 9 `shouldBe` False   -- one below it
            isDamagingDrop pcTrigger1 10 9 `shouldBe` True
            isDamagingDrop pc 8 10 `shouldBe` False   -- an ascent

        it "leaves the default policy byte-identical to the old function" $ do
            -- Requirement 5: existing callers' cost semantics are
            -- unchanged. Compare across the whole ridge neighbourhood,
            -- flat steps, diagonals, drops and climbs alike.
            let w = ridgeWorld 10 6
                steps = [ ((sx, sy), (dx, dy))
                        | sx ← [5 .. 9], sy ← [2 .. 4]
                        , dx ← [5 .. 9], dy ← [2 .. 4]
                        , (sx, sy) ≢ (dx, dy) ]
            map (\(a, b) → stepCostUnder FallPermitted pc reg w a b) steps
                `shouldBe` map (\(a, b) → stepCost pc reg w a b) steps

    describe "localAStarUnder" $ do
        it "produces no route across a ridge that only a fall could cross" $ do
            -- Unsafe-only route: the plateau ends at x = 7 and the target
            -- is at x = 11, four z below. A protected search must not
            -- reach it.
            let path = localAStarUnder FallProhibited pc reg (ridgeWorld 10 6)
                                       (5, 3) (11, 3) defaultMaxRadius
            path `shouldSatisfy` all (\(x, _) → x ≤ 7)
            last' path `shouldNotBe` Just (11, 3)

        it "still routes there when falls are permitted" $
            localAStarUnder FallPermitted pc reg (ridgeWorld 10 6)
                            (5, 3) (11, 3) defaultMaxRadius
                `shouldSatisfy` elem (11, 3)

        it "leaves the unprotected route untouched (default == today)" $ do
            let w = ridgeWorld 10 6
            localAStarUnder FallPermitted pc reg w (5, 3) (11, 3) defaultMaxRadius
                `shouldBe` localAStar pc reg w (5, 3) (11, 3) defaultMaxRadius

        it "routes normally on safe ground under either policy" $ do
            localAStarUnder FallProhibited pc reg flatWorld (2, 2) (6, 2) defaultMaxRadius
                `shouldBe` localAStar pc reg flatWorld (2, 2) (6, 2) defaultMaxRadius
            localAStarUnder FallProhibited pc reg flatWorld (2, 2) (6, 2) defaultMaxRadius
                `shouldSatisfy` elem (6, 2)

    describe "the movement tick" $ do
        let ridge = ridgeWorld 10 6
            mw    = ownPageWorld ridge
            -- Standing one tile short of the edge, aimed straight across
            -- it.
            start p = moverAt (7.5, 3.5) 10 (MoveTarget 11.5 3.5 1.0 p)

        it "never launches a fall on a protected request" $ do
            let states = runTicks pc mw 200 (start FallProhibited)
            states `shouldSatisfy` not . any isFalling
            map usGridZ states `shouldSatisfy` all (≡ 10)

        it "does launch one on the SAME terrain when falls are permitted" $
            runTicks pc mw 200 (start FallPermitted)
                `shouldSatisfy` any isFalling

        it "terminates a protected request that can make no safe progress" $ do
            -- "Never gives up" is the fall-permitted behavior; an ambient
            -- request must clear its target so the AI resamples.
            let states = runTicks pc mw 200 (start FallProhibited)
            usTarget (last states) `shouldBe` Nothing

        it "keeps a fall-permitted request's target while it replans" $ do
            -- The contrast case: a permitted request walled in by
            -- impassable terrain holds its target (pre-#1217 behavior).
            -- Aim at an unloaded chunk so no route exists at all.
            let us = moverAt (3.5, 3.5) 10 (MoveTarget 3.5 60.5 1.0 FallPermitted)
                states = runTicks pc (ownPageWorld flatWorld) 40 us
            usTarget (last states) `shouldSatisfy` isJust

        it "walks a protected request over a below-trigger step" $ do
            -- Requirement 4 at the tick level: a 1-z walk-off under the
            -- shipped trigger of 2 is ordinary movement, and the unit
            -- actually crosses onto the lower ground.
            let w  = ridgeWorld 10 9
                us = moverAt (7.5, 3.5) 10 (MoveTarget 10.5 3.5 1.0 FallProhibited)
                states = runTicks pc (ownPageWorld w) 200 us
            states `shouldSatisfy` not . any isFalling
            usRealX (last states) `shouldSatisfy` (> 8.0)

        -- Review round 1: the arrival branch snaps x/y AND re-grounds z
        -- without consulting the cost function, so a sub-goal within
        -- `max step arrivalEpsilon` across a tile boundary used to be
        -- crossed by the snap rather than by a step — a third route over
        -- a damaging drop, past both the greedy stepper and A*.
        let atEdge p = moverAt (7.95, 3.5) 10 (MoveTarget 8.04 3.5 1.0 p)

        it "never SNAPS a protected request across a damaging drop" $ do
            -- 0.09 tiles from its target, so this tick takes the arrival
            -- branch, not the stepping one.
            let us' = tickUnit pc reg 0.1 0.1 mw stats (atEdge FallProhibited)
            usRealX us' `shouldSatisfy` (< 8.0)
            usGridZ us' `shouldBe` 10
            -- Nowhere safe to go from here, so the request terminates
            -- rather than retrying the same blocked snap every tick.
            usTarget us' `shouldBe` Nothing

        it "still snaps a fall-permitted arrival exactly as it always did" $ do
            -- The pre-#1217 arrival behavior, deliberately untouched: the
            -- snap ignores the cost function entirely for a permitted
            -- request, which is what makes the case above a policy
            -- decision rather than a general repair.
            let us' = tickUnit pc reg 0.1 0.1 mw stats (atEdge FallPermitted)
            usRealX us' `shouldSatisfy` (> 8.0)
            usGridZ us' `shouldBe` 6

        it "snaps a protected arrival over a BELOW-trigger drop" $ do
            -- Requirement 4 on the arrival path too: the guard keys on
            -- the same damaging-drop classification, so an ordinary
            -- walk-off still arrives.
            let shallow = ownPageWorld (ridgeWorld 10 9)
                us' = tickUnit pc reg 0.1 0.1 shallow stats
                                (atEdge FallProhibited)
            usRealX us' `shouldSatisfy` (> 8.0)
            usGridZ us' `shouldBe` 9

        -- Review round 2: `dt` is an uncapped wall-clock delta and
        -- `unit.moveTo` takes an uncapped speed, so one tick's motion can
        -- span several tiles — and both the greedy check and the arrival
        -- snap look only at the tick's start and end tiles.
        let shelf = ownPageWorld shelfWorld
            -- 3 tiles/s over a 2-second tick: 6 tiles of raw travel,
            -- clean over the shelf, landing on a below-trigger tile.
            sprinter p = moverAt (7.5, 3.5) 10 (MoveTarget 13.5 3.5 3.0 p)

        it "never steps a protected request OVER an intermediate drop" $ do
            let states = runTicksAt pc shelf 2.0 40 (sprinter FallProhibited)
            -- Never reaches the shelf or the ground beyond it...
            map usGridZ states `shouldSatisfy` all (≡ 10)
            -- ...and never crosses onto them either, however long the
            -- tick. The endpoint-only check would have let the very first
            -- tick land at x ≈ 13.5 on z = 9.
            map usRealX states `shouldSatisfy` all (< 8.0)
            states `shouldSatisfy` not . any isFalling

        it "bounds a protected tick's displacement in BOTH directions" $ do
            -- The cap has to bound the MAGNITUDE: a large negative
            -- step spans just as many tiles backwards as a positive one
            -- spans forwards. Since #2290 no negative SPEED survives
            -- either the `unit.moveTo` ingress or the `UnitMoveTo`
            -- handler, so this drives the clamp directly rather than
            -- through a verb that would now refuse it — the step is a
            -- product of speed, grade and material factor, and the cap
            -- must bound the product.
            --
            -- The assertion is the invariant the single-boundary argument
            -- actually rests on — a displacement STRICTLY under one tile
            -- moves `floor` by at most 1 — rather than the cap's own
            -- value, which the accumulate-then-subtract below reproduces
            -- only to within Float rounding (0.9000001). That headroom
            -- under 1.0 is why `maxProtectedStep` is 0.9 and not 0.99.
            let far sp = moverAt (5.5, 3.5) 10 (MoveTarget 13.5 3.5 sp FallProhibited)
                movedWith sp =
                    abs (usRealX (last (runTicksAt pc shelf 2.0 1 (far sp))) - 5.5)
            -- 3 tiles/s over a 2-second tick is 6 tiles of raw travel.
            movedWith 3.0    `shouldSatisfy` (< 1.0)
            movedWith (-3.0) `shouldSatisfy` (< 1.0)
            -- ...and the cap itself is what does the bounding.
            maxProtectedStep `shouldSatisfy` (< 1.0)

        it "refuses to move a protected request on a non-finite step" $ do
            -- NaN compares False against everything, so a bare clamp chain
            -- would launder it through into `floor` on the far side.
            let nanSpeed = 0 / 0 ∷ Float
                us' = tickUnit pc reg 0.1 0.1 shelf stats
                        (moverAt (5.5, 3.5) 10
                            (MoveTarget 13.5 3.5 nanSpeed FallProhibited))
            usRealX us' `shouldBe` 5.5
            usGridZ us' `shouldBe` 10

        it "leaves a fall-permitted high-speed tick uncapped" $ do
            -- The contrast that makes the cap a POLICY rather than a
            -- global change: the same tick under the default crosses in
            -- one go, exactly as it does today.
            let states = runTicksAt pc shelf 2.0 3 (sprinter FallPermitted)
            map usRealX states `shouldSatisfy` any (> 8.0)

        it "abandons a protected request when the terrain is another page" $ do
            let wrongPage = moveWorldFor (snapshotOf pageB ridge) (Just pageA)
                us' = tickUnit pc reg 0.1 0.1 wrongPage stats (start FallProhibited)
            usTarget us' `shouldBe` Nothing
            usState us' `shouldBe` Idle

        it "abandons a protected request when there is no snapshot at all" $ do
            let noTerrain = moveWorldFor HM.empty (Just pageA)
                us' = tickUnit pc reg 0.1 0.1 noTerrain stats (start FallProhibited)
            usTarget us' `shouldBe` Nothing

        it "abandons a protected request when the mover has no page" $ do
            -- A sim state outliving its unit instance: no page to resolve
            -- terrain from, so fail closed.
            let noMover = moveWorldFor (snapshotOf pageA ridge) Nothing
                us' = tickUnit pc reg 0.1 0.1 noMover stats (start FallProhibited)
            usTarget us' `shouldBe` Nothing

        it "keeps a fall-permitted request alive with no terrain at all" $ do
            -- Since #1593 a mover is only ever handed its OWN page's
            -- tiles, so the wrong-page case IS the no-terrain case. A
            -- fall-permitted request keeps its "never gives up" behavior
            -- there — only protected requests abandon.
            let wrongPage = moveWorldFor (snapshotOf pageB ridge) (Just pageA)
                us' = tickUnit pc reg 0.1 0.1 wrongPage stats (start FallPermitted)
            usTarget us' `shouldSatisfy` isJust
            mwTiles wrongPage `shouldSatisfy` isNothing

        it "moveWorldFor hands over the mover's OWN page and nothing else" $ do
            mwOwnPage (moveWorldFor (snapshotOf pageA ridge) (Just pageA))
                `shouldBe` True
            mwOwnPage (moveWorldFor (snapshotOf pageB ridge) (Just pageA))
                `shouldBe` False
            mwOwnPage (moveWorldFor HM.empty (Just pageA)) `shouldBe` False
            mwOwnPage (moveWorldFor (snapshotOf pageA ridge) Nothing)
                `shouldBe` False
            -- The tiles track the flag exactly: no unverified terrain is
            -- ever handed out for a mover to path against.
            mwTiles (moveWorldFor (snapshotOf pageB ridge) (Just pageA))
                `shouldSatisfy` isNothing
            mwTiles (moveWorldFor (snapshotOf pageA ridge) Nothing)
                `shouldSatisfy` isNothing
            -- A batch holding BOTH pages still gives each mover its own.
            let both = HM.fromList [(pageA, flatWorld), (pageB, ridge)]
            mwOwnPage (moveWorldFor both (Just pageB)) `shouldBe` True
            mwTiles (moveWorldFor both (Just pageA)) `shouldSatisfy` isJust

    describe "which movers select the shared mechanism" $ do
        -- Source guards (#1217 requirement 2 / the review's integration
        -- clause): protection is chosen by CALLER CONTEXT, and there is
        -- exactly ONE place the policy token is spelled.
        it "scripts/ambient_movement.lua is the only place the token lives" $ do
            let files = [ "scripts/unit_ai.lua", "scripts/unit_ai_needs.lua"
                        , "scripts/unit_ai_mental.lua", "scripts/bear_ai.lua"
                        , "scripts/red_squirrel_ai.lua" ]
            bodies ← traverse readScript files
            zip files bodies `shouldSatisfy`
                all (\(_, b) → not (T.isInfixOf "avoid_falls" b))
            shared ← readScript "scripts/ambient_movement.lua"
            shared `shouldSatisfy` T.isInfixOf "avoid_falls"

        it "acolyte and technomule wander register the protected execute" $ do
            body ← readScript "scripts/unit_ai.lua"
            let wanderLines =
                    [ l | l ← T.lines body
                        , T.isInfixOf "name = \"wander\"" l ]
            length wanderLines `shouldBe` 2
            wanderLines `shouldSatisfy`
                all (T.isInfixOf "needs.ambientWanderExecute")

        it "bear and squirrel ambient wander use the shared mechanism" $ do
            bear ← readScript "scripts/bear_ai.lua"
            sq   ← readScript "scripts/red_squirrel_ai.lua"
            bear `shouldSatisfy` T.isInfixOf "ambient.wanderTo"
            sq   `shouldSatisfy` T.isInfixOf "ambient.wanderTo"

        it "mental-state and flee movement keep the default policy" $ do
            -- Panic, lash-out idling, delirium and mental breaks all
            -- reuse needs.wanderExecute; none may reach the protected
            -- variant, and flee's own moveTo passes no policy token.
            body ← readScript "scripts/unit_ai_mental.lua"
            body `shouldSatisfy` T.isInfixOf "needs.wanderExecute"
            body `shouldSatisfy` (not . T.isInfixOf "ambientWanderExecute")
            body `shouldSatisfy` (not . T.isInfixOf "ambient.")

        it "the default wanderExecute is still exported for those callers" $ do
            body ← readScript "scripts/unit_ai_needs.lua"
            body `shouldSatisfy` T.isInfixOf "M.wanderExecute"
            body `shouldSatisfy` T.isInfixOf "M.ambientWanderExecute"

last' ∷ [a] → Maybe a
last' [] = Nothing
last' xs = Just (last xs)
