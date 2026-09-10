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
--     * the RESIDUAL-TIME layer (#2473) — 'tickUnit' driven with
--       controlled paths and elapsed schedules, proving that reaching a
--       waypoint is charged its own distance and the rest of the tick
--       continues from there;
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
import World.Chunk.Types
    (ChunkCoord(..), LoadedChunk(..), ColumnTiles(..), chunkSize)
import World.Tile.Types (WorldTileData(..))
import World.Fluid.Types (emptyIceMap)
import World.Flora.Types (emptyFloraChunkData)
import World.Page.Types (WorldPageId(..))
import Structure.Types (emptyChunkStructures)
import World.Material
    (MaterialRegistry, MaterialProps(..), emptyMaterialRegistry
    , defaultMaterialProps, registerMaterial)
import Unit.Pathing.Cost
import Unit.Pathing.AStar (localAStar, localAStarUnder, defaultMaxRadius)
import Unit.Sim.Types
import Unit.Thread.Movement.PathAdvance
    (tickUnit, moveWorldFor, TerrainSnapshots, MoveWorld(..)
    , maxProtectedStep, arrivalTolerance, maxWaypointContinuations)
import Unit.Thread.Movement.Types
    (UnitMoveStats(..), defaultMoveStats, vectorToDirection)

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

-- | A one-chunk world at a uniform z = 5 whose columns carry REAL
--   'ColumnTiles' (#2473).
--
--   'zChunk' above leaves @lcTiles@ empty, so 'materialFactor' reads
--   1.0 and 'slopeGrade' reads 0 on every tile of it — which would make
--   a "the continuation resampled the terrain" assertion vacuous. Here
--   @column lx@ gives each column's (surface material id, slope bits) by
--   local x, which is all these cases need to vary.
tiledWorld ∷ (Int → (Word8, Word8)) → WorldTileData
tiledWorld column =
    let area = chunkSize * chunkSize
        cols = V.generate area $ \i →
            let (mid, slope) = column (i `mod` chunkSize)
            in ColumnTiles { ctStartZ = 5
                           , ctMats   = VU.singleton mid
                           , ctSlopes = VU.singleton slope
                           , ctVeg    = VU.singleton 0 }
    in worldWith (zChunk (const 5)) { lcTiles = cols }

-- | Four @move_cost@ classes: 1 is firm ground, 2 is exactly twice as
--   slow, 4 is twenty times slower (slow enough that a continued segment
--   stays well under the protected ceiling), and 3 is slow enough that a
--   real positive step lands under the Float resolution of the unit's own
--   position. Registered directly rather than through the YAML loader —
--   these cases are about the mover, not the authoring domain (#1734 owns
--   that boundary).
moveCosts ∷ MaterialRegistry
moveCosts =
    registerMaterial 1 (defaultMaterialProps { mpMoveCost = 1.0 })
        (registerMaterial 2 (defaultMaterialProps { mpMoveCost = 2.0 })
            (registerMaterial 3 (defaultMaterialProps { mpMoveCost = 1.0e7 })
                (registerMaterial 4 (defaultMaterialProps { mpMoveCost = 20.0 })
                    emptyMaterialRegistry)))

-- | Firm ground west of x = 2, the slow material from x = 2 on: a
--   boundary a continuation STARTING at x = 2 must resample across.
slowEastWorld ∷ WorldTileData
slowEastWorld = tiledWorld (\lx → (if lx < 2 then 1 else 2, 0))

-- | The same boundary at x = 1, which is where the reviewer's
--   partition-dependence counterexample puts it.
slowFromOneWorld ∷ WorldTileData
slowFromOneWorld = tiledWorld (\lx → (if lx < 1 then 1 else 2, 0))

-- | Firm ground west of x = 6, twenty-times-slower ground from x = 6:
--   slow enough that a continued segment starting there stays under the
--   protected ceiling, so the ceiling cannot mask a mis-billed arrival.
slowFromSixWorld ∷ WorldTileData
slowFromSixWorld = tiledWorld (\lx → (if lx < 6 then 1 else 4, 0))

-- | A downhill column at x = 1 and near-immobilising ground everywhere
--   else, so a continuation off the slope takes a real positive step that
--   is nonetheless too small to change the unit's Float position.
crawlOffSlopeWorld ∷ WorldTileData
crawlOffSlopeWorld = tiledWorld (\lx → if lx ≡ 1 then (1, 2) else (3, 0))

-- | Slope bit 1 marks the EAST neighbour as one z below, so an
--   east-bound unit standing on such a column is heading straight down
--   its fall line (grade -1). Only local x = 1 carries it, so a segment
--   starting there and a segment starting at x = 2 read different
--   grades.
slopeAtOneWorld ∷ WorldTileData
slopeAtOneWorld = tiledWorld (\lx → (1, if lx ≡ 1 then 2 else 0))

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

-- | 'moverAt' already following a local path.
moverOn ∷ (Float, Float) → Int → MoveTarget → [(Float, Float)]
        → UnitSimState
moverOn p z mt wps = (moverAt p z mt) { usLocalPath = wps }

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
        -- without consulting the cost function, so a sub-goal the step
        -- REACHES across a tile boundary used to be crossed by the snap
        -- rather than by a step — a third route over a damaging drop,
        -- past both the greedy stepper and A*. (Before #2473 the
        -- arrival predicate was `dist ≤ max step 0.1`, so a sub-goal
        -- within a tenth of a tile qualified even when the step fell
        -- short of it; the case below is inside both.)
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

  describe "Movement carries residual time across waypoints" $ do

    -- No terrain at all, so the effective speed is constant and
    -- requirement 1's partition independence applies in full.
    let noWorld = MoveWorld Nothing False
        flat    = ownPageWorld flatWorld
        dropRidge  = ownPageWorld (ridgeWorld 10 6)
        cliffRidge = ownPageWorld (ridgeWorld 6 10)
        -- Run a whole elapsed schedule, one tick per entry, advancing
        -- game time by each tick's own delta.
        runSchedule mw dts us0 = go 0 us0 dts
          where
            go _ us []         = us
            go t us (dt : rest) =
                let t' = t + dt
                in go t' (tickUnit pc reg t' dt mw stats us) rest

    describe "the reproduction from the finding" $ do
        -- Start at 0.4, speed 1 tile/s, waypoints at 0.5, 1.5 and 2.5,
        -- target 2.5. One second of game time, three partitions of it.
        let start = moverOn (0.4, 0.5) 0 (MoveTarget 2.5 0.5 1.0 FallPermitted)
                            [(0.5, 0.5), (1.5, 0.5), (2.5, 0.5)]
            endX dts = usRealX (runSchedule noWorld dts start)

        it "ends the same second at the same place however it is split" $
            -- Before this repair: 1.25, 1.4000002 and 1.4499997. The
            -- coarse partition threw away the remainder of the tick that
            -- reached the first waypoint; the fine one was HANDED 0.05
            -- tiles by the old arrival slack.
            mapM_ (\dts → abs (endX dts - 1.4)
                              `shouldSatisfy` (< arrivalTolerance))
                  [replicate 4 0.25, replicate 10 0.10, replicate 20 0.05]

        it "gives a sub-tolerance step no distance its time did not buy" $ do
            -- The 0.05-tile step used to snap a full 0.1 tiles at the
            -- first waypoint, and 1.45 is what that bought.
            endX (replicate 20 0.05) `shouldSatisfy` (< 1.41)
            -- ...while the coarse partition no longer loses the 0.15
            -- tiles the discarded remainder used to cost it.
            endX (replicate 4 0.25) `shouldSatisfy` (> 1.39)

    it "crosses several waypoints inside one tick and finishes the target" $ do
        let us  = moverOn (0.5, 0.5) 0 (MoveTarget 3.5 0.5 10.0 FallPermitted)
                          [(1.5, 0.5), (2.5, 0.5), (3.5, 0.5)]
            us' = tickUnit pc reg 1.0 1.0 noWorld stats us
        usRealX us'     `shouldBe` 3.5
        usLocalPath us' `shouldBe` []
        usTarget us'    `shouldBe` Nothing
        usState us'     `shouldBe` Idle

    describe "a last waypoint near, but distinct from, the target" $ do
        -- `arriveAtSubGoal` used to clear the target on any waypoint
        -- within 0.1 PER AXIS of it, which handed the unit the last
        -- tenth of a tile for free and reported it arrived somewhere it
        -- was not.
        let short = moverOn (1.5, 0.5) 0 (MoveTarget 2.05 0.5 1.0 FallPermitted)
                            [(2.0, 0.5)]

        it "continues toward the real target instead of clearing it" $ do
            let us' = tickUnit pc reg 1.0 1.0 noWorld stats short
            usRealX us'  `shouldBe` 2.05
            usTarget us' `shouldBe` Nothing
            usState us'  `shouldBe` Idle

        it "does not clear a DIAGONALLY near final target" $ do
            -- Offset by 0.9 of the tolerance on each axis: inside a
            -- per-axis pair of comparisons, but 1.27 times the tolerance
            -- away radially, so the last leg must still be travelled and
            -- charged.
            let off = 0.9 * arrivalTolerance
                fx  = 2.0 + off
                fy  = 2.0 + off
                us  = moverOn (1.5, 2.0) 0 (MoveTarget fx fy 1.0 FallPermitted)
                              [(2.0, 2.0)]
                us' = tickUnit pc reg 1.0 1.0 noWorld stats us
            (usRealX us', usRealY us') `shouldBe` (fx, fy)
            usTarget us' `shouldBe` Nothing
            usState us'  `shouldBe` Idle

        it "charges the last leg rather than snapping it" $ do
            -- A budget that reaches the waypoint with only 0.02 tiles of
            -- travel left over gets 0.02 tiles, not the whole 0.05.
            let us' = tickUnit pc reg 0.52 0.52 noWorld stats short
            usRealX us'  `shouldSatisfy` (\x → x > 2.0 ∧ x < 2.05)
            usTarget us' `shouldSatisfy` isJust

    it "resamples the material factor at the waypoint it continues from" $ do
        -- Firm ground to x = 2, twice-as-slow ground beyond. The first
        -- half-second buys the 0.5 tiles to the waypoint; the second
        -- starts ON the slow tile and buys 0.25, not another 0.5.
        let us  = moverOn (1.5, 0.5) 5 (MoveTarget 4.0 0.5 1.0 FallPermitted)
                          [(2.0, 0.5), (4.0, 0.5)]
            us' = tickUnit pc moveCosts 1.0 1.0
                           (ownPageWorld slowEastWorld) stats us
        usRealX us' `shouldBe` 2.25

    it "bills a cap-active protected arrival at its effective SPEED" $ do
        -- Speed 6 over a 1 s tick is 6 tiles of raw travel, so the 0.9
        -- ceiling clamps the first segment's step. The waypoint at x = 6
        -- is 0.5 tiles away, and 0.5 tiles at 6 tiles/s costs 1/12 s, not
        -- the 5/9 s that billing against the clamped 0.9 would charge.
        -- The continuation then crosses twenty-times-slower ground, where
        -- the ceiling never binds again, so the mis-billing is visible in
        -- the distance rather than hidden by the clamp.
        let us  = moverOn (5.5, 0.5) 5 (MoveTarget 12.5 0.5 6.0 FallProhibited)
                          [(6.0, 0.5), (12.5, 0.5)]
            us' = tickUnit pc moveCosts 1.0 1.0
                           (ownPageWorld slowFromSixWorld) stats us
            -- 1 - 0.5/6 seconds left, at 6/20 tiles per second.
            expected = 6.0 + 0.3 * (1 - 0.5 / 6)
        abs (usRealX us' - realToFrac expected)
            `shouldSatisfy` (< arrivalTolerance)
        -- Billing against the clamped step would have left 1 - 0.5/0.9
        -- seconds and stopped short, near x = 6.13.
        usRealX us' `shouldSatisfy` (> 6.2)
        -- ...and the ceiling still bounds the whole tick's path length,
        -- which is what makes this a billing fix and not a relaxed cap.
        (0.5 + abs (usRealX us' - 6.0))
            `shouldSatisfy` (≤ maxProtectedStep + arrivalTolerance)

    describe "usMoveGrade across a multi-segment tick" $ do
        -- Only local x = 1 slopes, so the segment that starts there and
        -- the segment that starts at x = 2 read different grades.
        let slopeMw = ownPageWorld slopeAtOneWorld

        it "names the last segment that consumed movement time" $ do
            let us  = moverOn (1.5, 0.5) 5 (MoveTarget 4.0 0.5 1.0 FallPermitted)
                              [(2.0, 0.5), (4.0, 0.5)]
                us' = tickUnit pc reg 1.0 1.0 slopeMw stats us
            -- The downhill first segment arrived; the flat continuation
            -- spent the rest of the tick, so ITS grade is the one that
            -- stands.
            usMoveGrade us' `shouldBe` 0

        it "is not overwritten by a trailing segment that spends nothing" $ do
            -- A repeated waypoint costs no time, so the downhill
            -- segment's grade survives it.
            let us  = moverOn (1.5, 0.5) 5 (MoveTarget 2.0 0.5 1.0 FallPermitted)
                              [(2.0, 0.5), (2.0, 0.5)]
                us' = tickUnit pc reg 1.0 1.0 slopeMw stats us
            usTarget us'    `shouldBe` Nothing
            usMoveGrade us' `shouldBe` (-1)

        it "belongs to a step too small to change the position" $ do
            -- The continuation off the slope takes a real positive step
            -- of about 5e-8 tiles — the whole remaining budget on ground
            -- with move_cost 1e7 — which rounds away at x = 2 (a Float
            -- there resolves to ≈ 2.4e-7). It spent the time, so the
            -- grade is ITS grade; reading consumption off the coordinates
            -- would hand the downhill segment's -1 back.
            let us  = moverOn (1.5, 0.5) 5 (MoveTarget 4.0 0.5 1.0 FallPermitted)
                              [(2.0, 0.5), (4.0, 0.5)]
                us' = tickUnit pc moveCosts 1.0 1.0
                               (ownPageWorld crawlOffSlopeWorld) stats us
            usRealX us'     `shouldBe` 2.0
            usMoveGrade us' `shouldBe` 0

        it "is zero when no segment consumed movement time" $ do
            let us  = moverOn (1.5, 0.5) 5
                              (MoveTarget 4.0 0.5 (0 / 0) FallPermitted)
                              [(2.0, 0.5), (4.0, 0.5)]
                us' = tickUnit pc reg 1.0 1.0 slopeMw stats us
            usMoveGrade us' `shouldBe` 0

    it "bounds a protected tick's whole PATH LENGTH, turns included" $ do
        -- Out to 5.9, back to 5.5, out again — 0.8 tiles of path for
        -- zero net displacement. An endpoint-measured ceiling would
        -- believe nothing had been spent; a ceiling reset at each
        -- waypoint would believe the same. Either lets the tick finish
        -- the route; the real one runs out 0.3 tiles short.
        let us  = moverOn (5.5, 3.5) 10 (MoveTarget 5.9 3.5 10.0 FallProhibited)
                          [(5.9, 3.5), (5.5, 3.5), (5.9, 3.5)]
            us' = tickUnit pc reg 1.0 1.0 flat stats us
            -- Collinear on x, so the path length is exactly this.
            travelled = 0.4 + 0.4 + abs (usRealX us' - 5.5)
        travelled       `shouldSatisfy` (≤ maxProtectedStep + arrivalTolerance)
        usTarget us'    `shouldSatisfy` isJust
        usRealX us'     `shouldSatisfy` (< 5.9)
        usLocalPath us' `shouldBe` [(5.9, 3.5)]

    it "terminates on repeated zero-length waypoints and keeps the route" $ do
        -- 70 coincident waypoints: each costs no time, so only the
        -- continuation bound can end the tick.
        let us = moverOn (5.5, 3.5) 10 (MoveTarget 20.5 3.5 1.0 FallPermitted)
                         (replicate 70 (5.5, 3.5) ⧺ [(6.5, 3.5)])
            t1 = tickUnit pc reg 1.0 1.0 flat stats us
        length (usLocalPath t1)
            `shouldBe` (71 - (maxWaypointContinuations + 1))
        usRealX t1   `shouldBe` 5.5
        usTarget t1  `shouldSatisfy` isJust
        -- The unspent second is DROPPED, not banked: the next tick moves
        -- what its own second buys at 1 tile/s and no more.
        let t2 = tickUnit pc reg 2.0 1.0 flat stats t1
        abs (usRealX t2 - usRealX t1)
            `shouldSatisfy` (≤ 1.0 + arrivalTolerance)

    describe "an invalid effective step near a waypoint" $ do
        -- 0.05 tiles short: inside the OLD 0.1 arrival slack, far
        -- outside the floating-point tolerance. Every one of these used
        -- to snap, pop and clear.
        let near sp h = moverOn (5.5, 3.5) 10 (MoveTarget 5.55 3.5 sp h)
                                [(5.55, 3.5)]
            outcome sp h =
                let us' = tickUnit pc reg 0.1 0.1 flat stats (near sp h)
                in (usRealX us', usLocalPath us', isJust (usTarget us'))

        it "does not move, snap, pop or clear" $
            mapM_ (\(sp, h) → outcome sp h
                      `shouldBe` (5.5, [(5.55, 3.5)], True))
                  [ (0,      FallPermitted), (0,      FallProhibited)
                  , (0 / 0,  FallPermitted), (0 / 0,  FallProhibited)
                  , (1 / 0,  FallPermitted), (1 / 0,  FallProhibited)
                  , (-1 / 0, FallPermitted), (-1 / 0, FallProhibited) ]

        it "still completes an arrival the unit is already standing on" $ do
            -- Zero speed is in-domain (an exhausted or fully encumbered
            -- unit legitimately commands 0) and #2204's clock can hand a
            -- tick dt = 0, so the refusal must not strand a unit ON its
            -- own target.
            let onTarget sp dt = tickUnit pc reg dt dt flat stats
                    (moverAt (5.5, 3.5) 10 (MoveTarget 5.5 3.5 sp FallPermitted))
            usTarget (onTarget 0 0.1)   `shouldBe` Nothing
            usState  (onTarget 0 0.1)   `shouldBe` Idle
            usTarget (onTarget 1.0 0)   `shouldBe` Nothing
            usState  (onTarget 1.0 0)   `shouldBe` Idle

        it "still validates a protected snap it is standing on" $ do
            -- Free of time cost is not free of the hazard check: the
            -- sub-goal is 2e-5 tiles away and over the ridge.
            let us' = tickUnit pc reg 0.1 0.1 dropRidge stats
                          (moverAt (7.99999, 3.5) 10
                              (MoveTarget 8.00001 3.5 0 FallProhibited))
            usRealX us'  `shouldSatisfy` (< 8.0)
            usGridZ us'  `shouldBe` 10
            usTarget us' `shouldBe` Nothing

    describe "ordinary movement across a terrain boundary" $ do
        -- #2473's explicit out-of-scope: a step that reaches no waypoint
        -- still samples material once, at its start tile.
        let straight = moverOn (0.75, 0.5) 5
                               (MoveTarget 8.5 0.5 2.0 FallPermitted)
                               [(8.5, 0.5)]
            slowMw = ownPageWorld slowFromOneWorld
            tickOf t dt = tickUnit pc moveCosts t dt slowMw stats

        it "produces exactly the state the pre-#2473 mover produced" $
            -- Only the fields an ordinary step writes have moved — the
            -- position and the facing it derives from the step — and
            -- 0.25 s at 2 tiles/s on firm ground is 0.5 tiles, because
            -- the boundary at x = 1 does not shorten it.
            tickOf 0.25 0.25 straight
                `shouldBe` straight { usRealX  = 1.25
                                    , usFacing = vectorToDirection 1 0 }

        it "stays partition-dependent there, deliberately" $
            -- Halving the tick resamples at x = 1 and so travels less.
            -- This is the case requirement 1 excludes, not a regression.
            usRealX (tickOf 0.25 0.125 (tickOf 0.125 0.125 straight))
                `shouldBe` 1.125

    describe "a continued segment meeting a hazard" $ do
        -- Arrive at 7.6 with budget to spare, then step at the ridge at
        -- x = 8 with a waypoint still ahead, so the continuation reaches
        -- the cliff/fall checks rather than the greedy cost threshold.
        let crossing h z = moverOn (7.2, 3.5) z (MoveTarget 11.5 3.5 1.0 h)
                                   [(7.6, 3.5), (11.5, 3.5)]

        it "launches a fall when the request permits one" $ do
            let us' = tickUnit pc reg 1.0 1.0 dropRidge stats
                          (crossing FallPermitted 10)
            usState us' `shouldBe` TransitioningTo Falling

        it "enters the climb transition at a cliff" $ do
            let us' = tickUnit pc reg 1.0 1.0 cliffRidge stats
                          (crossing FallPermitted 6)
            usState us' `shouldBe` TransitioningTo Climbing

        it "replans a protected continuation as a fresh step would" $ do
            let continued = tickUnit pc reg 1.0 1.0 dropRidge stats
                                (crossing FallProhibited 10)
                -- The same segment, run as a FRESH tick from the
                -- waypoint on the budget the continuation had left:
                -- 0.4 tiles at 1 tile/s leaves 0.6 s of the second.
                fresh = tickUnit pc reg 1.0 0.6 dropRidge stats
                            (moverOn (7.6, 3.5) 10
                                (MoveTarget 11.5 3.5 1.0 FallProhibited)
                                [(11.5, 3.5)])
            usTarget continued `shouldBe` Nothing
            usTarget fresh     `shouldBe` Nothing
            usState continued  `shouldBe` usState fresh
            usRealX continued  `shouldBe` usRealX fresh
            continued          `shouldSatisfy` (not . isFalling)

    it "keeps the fall-permitted arrival bypass" $ do
        -- The same case as `still snaps a fall-permitted arrival exactly
        -- as it always did`, restated because the continuation loop now
        -- runs the arrival branch several times in one tick.
        let us' = tickUnit pc reg 0.1 0.1 dropRidge stats
                      (moverAt (7.95, 3.5) 10
                          (MoveTarget 8.04 3.5 1.0 FallPermitted))
        usRealX us' `shouldSatisfy` (> 8.0)
        usGridZ us' `shouldBe` 6

last' ∷ [a] → Maybe a
last' [] = Nothing
last' xs = Just (last xs)
