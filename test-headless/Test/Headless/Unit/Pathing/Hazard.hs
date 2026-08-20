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
--       request on the SAME terrain behaves exactly as it does today;
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
    (tickUnit, moveWorldFor, TerrainSnapshot(..), MoveWorld(..))
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

-- | Own-page terrain: the batch snapshot is this mover's page.
ownPageWorld ∷ WorldTileData → MoveWorld
ownPageWorld wtd = moveWorldFor (Just (TerrainSnapshot pageA wtd)) (Just pageA)

-- | Run the movement tick repeatedly at a fixed dt, collecting each
--   intermediate state. Deterministic — 'tickUnit' is pure.
runTicks ∷ PathingConfig → MoveWorld → Int → UnitSimState → [UnitSimState]
runTicks cfg mw n us0 = go n 0 us0 []
  where
    go 0 _ us acc = reverse (us : acc)
    go k t us acc =
        let t'  = t + 0.1 ∷ Double
            us' = tickUnit cfg reg t' 0.1 mw stats us
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

        it "abandons a protected request when the terrain is another page" $ do
            let wrongPage = moveWorldFor (Just (TerrainSnapshot pageB ridge))
                                         (Just pageA)
                us' = tickUnit pc reg 0.1 0.1 wrongPage stats (start FallProhibited)
            usTarget us' `shouldBe` Nothing
            usState us' `shouldBe` Idle

        it "abandons a protected request when there is no snapshot at all" $ do
            let noTerrain = moveWorldFor Nothing (Just pageA)
                us' = tickUnit pc reg 0.1 0.1 noTerrain stats (start FallProhibited)
            usTarget us' `shouldBe` Nothing

        it "abandons a protected request when the mover has no page" $ do
            -- A sim state outliving its unit instance: nothing to verify
            -- the snapshot against, so fail closed.
            let noMover = moveWorldFor (Just (TerrainSnapshot pageA ridge)) Nothing
                us' = tickUnit pc reg 0.1 0.1 noMover stats (start FallProhibited)
            usTarget us' `shouldBe` Nothing

        it "leaves a fall-permitted request alone on unverified terrain" $ do
            -- The #797 secondary-page defect is NOT lifted for ordinary
            -- movement; only protected requests fail closed.
            let wrongPage = moveWorldFor (Just (TerrainSnapshot pageB ridge))
                                         (Just pageA)
                us' = tickUnit pc reg 0.1 0.1 wrongPage stats (start FallPermitted)
            usTarget us' `shouldSatisfy` isJust

        it "moveWorldFor verifies the page it was snapshotted from" $ do
            mwOwnPage (moveWorldFor (Just (TerrainSnapshot pageA ridge)) (Just pageA))
                `shouldBe` True
            mwOwnPage (moveWorldFor (Just (TerrainSnapshot pageB ridge)) (Just pageA))
                `shouldBe` False
            mwOwnPage (moveWorldFor Nothing (Just pageA)) `shouldBe` False
            mwOwnPage (moveWorldFor (Just (TerrainSnapshot pageA ridge)) Nothing)
                `shouldBe` False

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
