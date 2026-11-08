{-# LANGUAGE OverloadedStrings #-}
-- | Ongoing bleeding through the PRODUCTION tick path (#2941): the
--   wound tick ('Combat.Wounds.Tick.tickAllWounds') credits a unit's
--   trail accumulator and the movement tick
--   ('Unit.Thread.Movement.tickAllMovement') spends it on trail marks
--   and pool layers, both driven here against a real headless engine
--   environment and its real blood store.
--
--   This is what the retired end-to-end bleeding-trail probe was the only gate
--   for beyond the pure arithmetic in "Test.Headless.Blood.Trail":
--
--   * the CLOCK SOURCE. The accumulator's cadence runs on the unpaused
--     'gameTimeRef' ('wsGameTimeRef' on the movement tick), never on the
--     world calendar that @world.setTimeScale@ speeds up. Two scenes with
--     the same bleeding, the same movement and the same elapsed game time
--     but calendars running at 1x and 5x must lay the same marks and the
--     same layers, stamped with game time.
--   * CLOT PROGRESSION. A wound that starts unclotted bleeds and emits,
--     natural clotting then drives its external bleed to zero, the
--     accumulator clears at once and nothing is emitted afterwards
--     however long the unit keeps moving or dwelling (the pool case stops
--     below the layer bound, so saturation cannot explain the stop).
--   * the wiring examples that follow from having the harness: an
--     internal-only wound stays dry, walk-then-stop hands one accumulator
--     from trail to pool, a collapsed bleeder pools, adjacent bleeders keep
--     separate bounded clusters attributed to their own unit, and the kill
--     command ends a trail.
--
--   What this deliberately does NOT cover (the accepted loss recorded in
--   @docs/archive/ongoing_bleeding_probe_retirement_2941.md@): the two ticks here are
--   called in lockstep by the test, not scheduled by the live unit and
--   combat threads, and no Lua @unit.injure@ / @unit.moveTo@ is involved.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "ongoing bleeding through the production tick"'@.
module Test.Headless.Blood.OngoingBleed (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef'
                  , atomicModifyIORef')
import System.Random (mkStdGen)
import Blood.Pool (PoolThresholds(..), defaultPoolThresholds)
import Blood.Trail (TrailThresholds(..), defaultTrailThresholds)
import Blood.Types
    (BloodDecal(..), BloodStore(..), allDecals)
import Combat.Wounds (externalBleedRateFor)
import Combat.Wounds.Tick (tickAllWounds)
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Test.Headless.Unit.SimPageOwnership (minimalDef, mkUnit, baseSimState, tilesAt)
import Unit.Sim.Types
    (UnitSimState(..), UnitThreadState(..), emptyUnitThreadState)
import Unit.Thread.Command.Pose (handleUnitKillCommand)
import Unit.Thread.Movement (tickAllMovement)
import Unit.Types
    ( TrailState(..), UnitId(..), UnitInstance(..), UnitManager(..)
    , Wound(..), bloodMassRatio, emptyUnitManager )
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState )
import World.Time.Types (WorldTime(..), preciseWorldTime)

spec ∷ Spec
spec = describe "ongoing bleeding through the production tick (#2941)" $ do
    clockSourceSpec
    clotProgressionSpec
    wiringSpec

-----------------------------------------------------------------------
-- Scene
-----------------------------------------------------------------------

page ∷ WorldPageId
page = WorldPageId "ongoing_bleed"

uidA, uidB ∷ UnitId
uidA = UnitId 1
uidB = UnitId 2

-- | Game time every scene starts at.
t0 ∷ Double
t0 = 1000

-- | The step both ticks take, in seconds.
stepDt ∷ Double
stepDt = 0.1

-- | A live engine environment with one loaded page holding a flat chunk
--   (so the movement tick has terrain), the given bleeders, a world
--   calendar running at @scale@ game-minutes per real second, and a sim
--   state per bleeder.
data Scene = Scene
    { scEnv      ∷ EngineEnv
    , scWs       ∷ WorldState
    , scUts      ∷ IORef UnitThreadState
    , scScale    ∷ Float
    , scCalendar ∷ IORef Double
    }

type Pos = (Float, Float)

newScene ∷ Float → [(UnitId, UnitInstance, Pos)] → IO Scene
newScene scale units = do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    ws ← emptyWorldState
    writeIORef (wsTilesRef ws) (tilesAt 0)
    writeIORef (wsTimeScaleRef ws) scale
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(page, ws)], wmVisible = [page] }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" minimalDef
        , umInstances = HM.fromList
            [ (u, inst { uiGridX = x, uiGridY = y }) | (u, inst, (x, y)) ← units ] }
    writeIORef (gameTimeRef env) t0
    writeIORef (statRNGRef env) (mkStdGen 20261108)
    utsRef ← newIORef emptyUnitThreadState
        { utsSimStates = HM.fromList
            [ (u, baseSimState { usRealX = x, usRealY = y })
            | (u, _, (x, y)) ← units ] }
    cal ← newIORef 0
    pure (Scene env ws utsRef scale cal)

-- | One lockstep step: advance the game clock and the world calendar (the
--   latter @scale@ times faster, as @world.setTimeScale@ does), put each
--   unit at its next position, run the wound tick then the movement tick,
--   then mirror the new positions into the unit manager the way
--   @publishToRender@ does (the movement tick measures its step against
--   that mirror).
stepScene ∷ Scene → [(UnitId, Pos)] → IO ()
stepScene sc positions = do
    let env = scEnv sc
    modifyIORef' (gameTimeRef env) (+ stepDt)
    modifyIORef' (scCalendar sc) (+ stepDt * realToFrac (scScale sc))
    cal ← readIORef (scCalendar sc)
    let mins = floor cal ∷ Int
    writeIORef (wsTimeRef (scWs sc))
        (preciseWorldTime (WorldTime ((mins `div` 60) `mod` 24) (mins `mod` 60)))
    atomicModifyIORef' (scUts sc) $ \uts →
        ( uts { utsSimStates = foldl'
                  (\m (u, (x, y)) →
                      HM.adjust (\s → s { usRealX = x, usRealY = y }) u m)
                  (utsSimStates uts) positions }
        , () )
    tickAllWounds env (realToFrac stepDt)
    tickAllMovement stepDt env (scUts sc)
    modifyIORef' (unitManagerRef env) $ \um →
        um { umInstances = foldl'
                 (\m (u, (x, y)) →
                     HM.adjust (\i → i { uiGridX = x, uiGridY = y }) u m)
                 (umInstances um) positions }

decalsOf ∷ Scene → IO [BloodDecal]
decalsOf sc = allDecals ∘ bstDecals ⊚ readIORef (wsBloodStoreRef (scWs sc))

instOf ∷ Scene → UnitId → IO UnitInstance
instOf sc u = do
    um ← readIORef (unitManagerRef (scEnv sc))
    maybe (expectationFailure "unit vanished" ≫ error "unreachable") pure
        (HM.lookup u (umInstances um))

trailOf ∷ Scene → UnitId → IO (Maybe TrailState)
trailOf sc u = uiTrailState ⊚ instOf sc u

externalRate ∷ Scene → UnitId → IO Float
externalRate sc u = externalBleedRateFor minimalDef ⊚ instOf sc u

-- | A clean slash of the given severity, bandaged not at all (open).
slash ∷ Float → Wound
slash sev = Wound
    { woundPart = "torso", woundKind = "slash", woundSeverity = sev
    , woundAt = 0, woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0
    , woundDressing = "", woundInfection = 0.0, woundClean = True
    , woundInfectionType = "", woundNecrosis = 0.0 }

internal ∷ Float → Wound
internal sev = (slash sev) { woundKind = "internal" }

-- | A bleeder with plenty of blood in hand, so it outlives every example.
bleeder ∷ [Wound] → UnitInstance
bleeder ws = (mkUnit page ws) { uiBlood = 70 * bloodMassRatio }

-- | A back-and-forth walk along x at @speed@ tiles per second, from the
--   second @t@ since the start; stays inside the fixture chunk.
patrolAt ∷ Float → Float → Float → Pos
patrolAt y speed t =
    let d = speed * t
        leg = d - 20 * fromIntegral (floor (d / 20) ∷ Int)
    in (2 + (if leg < 10 then leg else 20 - leg), y)

-- | Seconds elapsed after @n@ steps.
secs ∷ Int → Float
secs n = fromIntegral n * realToFrac stepDt

-----------------------------------------------------------------------
-- Clock source
-----------------------------------------------------------------------

clockSourceSpec ∷ Spec
clockSourceSpec = describe "the emission clock is the unpaused game clock" $ do

    it "a stationary bleeder lays the same pool layers, stamped with game \
       \time, under a 1x and a 5x world calendar" $ do
        let dwellSteps = 90                       -- 9 game-seconds
            dwellSecs = secs dwellSteps
            run scale = do
                sc ← newScene scale [(uidA, bleeder [slash 0.2], (4.5, 4.5))]
                forM_ [1 .. dwellSteps] $ \_ → stepScene sc [(uidA, (4.5, 4.5))]
                (,) ⊚ decalsOf sc ⊛ readIORef (scCalendar sc)
        (slow, calSlow) ← run 1
        (fast, calFast) ← run 5
        -- the calendars really did run apart (else this proves nothing)
        calFast `shouldSatisfy` (> 4 * calSlow)
        -- same density; cadence, not the calendar, sets the pace
        length fast `shouldBe` length slow
        let expected = floor (realToFrac dwellSecs / ptMinCadence defaultPoolThresholds) ∷ Int
        length slow `shouldSatisfy` (\n → abs (n - expected) ≤ 1)
        -- every layer is stamped with game time inside the dwell window
        forM_ (slow ⧺ fast) $ \d → do
            bdeCreatedAt d `shouldSatisfy` (> t0)
            bdeCreatedAt d `shouldSatisfy` (≤ t0 + realToFrac dwellSecs + 1e-6)
        map bdeCreatedAt fast `shouldBe` map bdeCreatedAt slow

    it "a walking bleeder lays the same trail marks, stamped with game time, \
       \under a 1x and a 5x world calendar" $ do
        let walkSteps = 100                       -- 10 game-seconds, 5 tiles/s
            walkSecs = secs walkSteps
            run scale = do
                sc ← newScene scale
                    [(uidA, bleeder [slash 0.2], patrolAt 6.5 5 0)]
                forM_ [1 .. walkSteps] $ \n →
                    stepScene sc [(uidA, patrolAt 6.5 5 (secs n))]
                (,) ⊚ decalsOf sc ⊛ readIORef (scCalendar sc)
        (slow, calSlow) ← run 1
        (fast, calFast) ← run 5
        calFast `shouldSatisfy` (> 4 * calSlow)
        length fast `shouldBe` length slow
        -- at 5 tiles/s the distance gate opens every other step, so the
        -- 0.5 s cadence is the binding gate: ~2 marks per game-second
        let expected = floor (realToFrac walkSecs / ttMinCadence defaultTrailThresholds) ∷ Int
        length slow `shouldSatisfy` (\n → abs (n - expected) ≤ 2)
        forM_ (slow ⧺ fast) $ \d → do
            bdeCreatedAt d `shouldSatisfy` (> t0)
            bdeCreatedAt d `shouldSatisfy` (≤ t0 + realToFrac walkSecs + 1e-6)
        map bdeCreatedAt fast `shouldBe` map bdeCreatedAt slow

-----------------------------------------------------------------------
-- Clot progression
-----------------------------------------------------------------------

clotProgressionSpec ∷ Spec
clotProgressionSpec = describe "natural clot progression ends the emission" $ do

    it "stops a walking bleeder's trail the moment the external bleed reaches \
       \zero, and nothing is emitted afterwards however far it keeps walking" $ do
        -- sev 0.15 untreated slash: self-clots in ~25 game-seconds
        -- (Combat.Wounds.Constants.clotBaseRate calibration).
        sc ← newScene 1 [(uidA, bleeder [slash 0.15], patrolAt 6.5 2 0)]
        let walk n = stepScene sc [(uidA, patrolAt 6.5 2 (secs n))]
        -- fresh and bleeding: the unit emits marks
        forM_ [1 .. 60] walk
        early ← length ⊚ decalsOf sc
        early `shouldSatisfy` (> 0)
        rate0 ← externalRate sc uidA
        rate0 `shouldSatisfy` (> 0)
        clots0 ← map woundClot ∘ uiWounds ⊚ instOf sc uidA
        clots0 `shouldSatisfy` (\cs → not (null cs) ∧ all (< 1) cs)
        trail0 ← trailOf sc uidA
        trail0 `shouldSatisfy` (≢ Nothing)
        -- walk until natural clotting zeroes the external bleed
        let untilClotted n
                | n > 700 = pure Nothing
                | otherwise = do
                    walk n
                    r ← externalRate sc uidA
                    if r ≤ 0 then pure (Just n) else untilClotted (n + 1)
        mClotted ← untilClotted 61
        n0 ← maybe (expectationFailure "the wound never clotted shut in 70 s" ≫ pure 0)
                   pure mClotted
        -- the accumulator is gone at once, and the marks stop where they were
        trailAtClot ← trailOf sc uidA
        trailAtClot `shouldBe` Nothing
        atClot ← length ⊚ decalsOf sc
        atClot `shouldSatisfy` (≥ early)
        -- ten more seconds of walking: still nothing, and the unit really moved
        let more = [n0 + 1 .. n0 + 100]
        forM_ more walk
        late ← length ⊚ decalsOf sc
        late `shouldBe` atClot
        trailLate ← trailOf sc uidA
        trailLate `shouldBe` Nothing
        let path = [ fst (patrolAt 6.5 2 (secs n)) | n ← n0 : more ]
        sum (zipWith (\a b → abs (b - a)) path (drop 1 path)) `shouldSatisfy` (> 15)

    it "stops a stationary bleeder's pool below the layer bound when the \
       \external bleed reaches zero, and no layer is laid afterwards" $ do
        sc ← newScene 1 [(uidA, bleeder [slash 0.1], (4.5, 4.5))]
        let dwell = stepScene sc [(uidA, (4.5, 4.5))]
            layersNow = maybe 0 tsClusterLayers ⊚ trailOf sc uidA
            loop n peak
                | n > 900 = pure Nothing
                | otherwise = do
                    dwell
                    r ← externalRate sc uidA
                    l ← layersNow
                    let peak' = max peak l
                    if r ≤ 0 then pure (Just peak') else loop (n + 1 ∷ Int) peak'
        rate0 ← externalRate sc uidA
        rate0 `shouldSatisfy` (> 0)
        mPeak ← loop 1 0
        peak ← maybe (expectationFailure "the wound never clotted shut in 90 s" ≫ pure 0)
                     pure mPeak
        -- it pooled, and the clot (not the bound) is what stopped it
        peak `shouldSatisfy` (≥ 1)
        peak `shouldSatisfy` (< ptMaxLayers defaultPoolThresholds)
        trailAtClot ← trailOf sc uidA
        trailAtClot `shouldBe` Nothing
        atClot ← length ⊚ decalsOf sc
        atClot `shouldBe` peak
        forM_ [1 .. 100 ∷ Int] $ \_ → dwell
        late ← length ⊚ decalsOf sc
        late `shouldBe` atClot
        trailLate ← trailOf sc uidA
        trailLate `shouldBe` Nothing

-----------------------------------------------------------------------
-- Wiring of the movement tick onto the accumulator
-----------------------------------------------------------------------

wiringSpec ∷ Spec
wiringSpec = describe "the movement tick spends the accumulator" $ do

    it "a walking bleeder's trail marks are spread along the route, at least \
       \the documented distance apart, and bounded by it" $ do
        let route = 13 ∷ Float
            speed = 1 ∷ Float
            walkSteps = 130
        sc ← newScene 1 [(uidA, bleeder [slash 0.1], (2, 6.5))]
        forM_ [1 .. walkSteps] $ \n →
            stepScene sc [(uidA, (2 + speed * secs n, 6.5))]
        ds ← decalsOf sc
        -- the pool at the far end is not part of the route
        let xs = [ bdeX d | d ← ds, bdeX d < 2 + route - 0.5 ]
            gaps = zipWith (\a b → abs (b - a)) xs (drop 1 xs)
        length xs `shouldSatisfy` (\n → n ≥ 1 ∧ n ≤ floor route + 2)
        (maximum xs - minimum xs) `shouldSatisfy` (> ttMinDistance defaultTrailThresholds)
        gaps `shouldSatisfy` all (≥ ttMinDistance defaultTrailThresholds - 1e-3)

    it "an internal-only wound lays no marks while the unit walks, though the \
       \unit is bleeding internally" $ do
        sc ← newScene 1 [(uidA, bleeder [internal 0.6], patrolAt 6.5 2 0)]
        forM_ [1 .. 200] $ \n → stepScene sc [(uidA, patrolAt 6.5 2 (secs n))]
        decalsOf sc `shouldReturn` []
        trailOf sc uidA `shouldReturn` Nothing
        inst ← instOf sc uidA
        uiBlood inst `shouldSatisfy` (< 70 * bloodMassRatio)

    it "a walk-then-stop hands ONE accumulator from trail marks along the \
       \route to a pool under the stopped unit" $ do
        let walkSteps = 80                        -- 8 tiles at 1 tile/s
            stopAt = (2 + secs walkSteps, 8.5) ∷ Pos
        sc ← newScene 1 [(uidA, bleeder [slash 0.15], (2, 8.5))]
        forM_ [1 .. walkSteps] $ \n → stepScene sc [(uidA, (2 + secs n, 8.5))]
        routeMarks ← decalsOf sc
        length routeMarks `shouldSatisfy` (≥ 2)
        forM_ [1 .. 120 ∷ Int] $ \_ → stepScene sc [(uidA, stopAt)]
        ds ← decalsOf sc
        Just ts ← trailOf sc uidA
        tsClusterLayers ts `shouldSatisfy` (> 1)
        let radius = ptJitterRadius defaultPoolThresholds
            near d = sqrt ((bdeX d - fst stopAt) ^ (2 ∷ Int)
                         + (bdeY d - snd stopAt) ^ (2 ∷ Int)) ≤ radius + 1e-3
            pool = filter near ds
        length pool `shouldSatisfy` (> length routeMarks - length (filter near routeMarks))
        length pool `shouldBe` tsClusterLayers ts + length (filter near routeMarks)
        case tsClusterAnchor ts of
            Nothing → expectationFailure "no cluster anchored at the stop"
            Just (ax, ay) →
                sqrt ((ax - fst stopAt) ^ (2 ∷ Int) + (ay - snd stopAt) ^ (2 ∷ Int))
                    `shouldSatisfy` (≤ radius + 1e-3)

    it "two adjacent bleeders, one of them collapsed, grow separate bounded \
       \clusters, each attributed to its own unit" $ do
        let posA = (4.5, 4.5) ∷ Pos
            posB = (6.5, 4.5) ∷ Pos         -- farther apart than the cluster radius
            collapsed = (bleeder [slash 0.2]) { uiPose = "collapsed" }
        sc ← newScene 1 [(uidA, bleeder [slash 0.2], posA), (uidB, collapsed, posB)]
        -- long enough for both to reach the bound (they clot afterwards, which
        -- clears the accumulator, so the peak layer count is what is read)
        let layers u = maybe 0 tsClusterLayers ⊚ trailOf sc u
            loop ∷ Int → Int → Int → IO (Int, Int)
            loop 0 pa pb = pure (pa, pb)
            loop k pa pb = do
                stepScene sc [(uidA, posA), (uidB, posB)]
                la ← layers uidA
                lb ← layers uidB
                loop (k - 1) (max pa la) (max pb lb)
        (peakA, peakB) ← loop 300 0 0
        ds ← decalsOf sc
        let bound = ptMaxLayers defaultPoolThresholds
            mine u = [ d | d ← ds, bdeSourceUnit d ≡ Just u ]
        peakA `shouldBe` bound
        peakB `shouldBe` bound
        length (mine uidA) `shouldBe` bound
        length (mine uidB) `shouldBe` bound
        length ds `shouldBe` 2 * bound
        let radius = ptJitterRadius defaultPoolThresholds
            within (cx, cy) d = sqrt ((bdeX d - cx) ^ (2 ∷ Int) + (bdeY d - cy) ^ (2 ∷ Int))
                                    ≤ radius + 1e-3
        forM_ (mine uidA) $ \d → d `shouldSatisfy` within posA
        forM_ (mine uidB) $ \d → d `shouldSatisfy` within posB
        -- the two clusters are separate piles, farther apart than the
        -- cluster radius
        let (ax, ay) = posA
            (bx, by) = posB
        sqrt ((ax - bx) ^ (2 ∷ Int) + (ay - by) ^ (2 ∷ Int))
            `shouldSatisfy` (> ptClusterRadius defaultPoolThresholds)

    it "the kill command ends the trail: the accumulator clears and a \
       \corpse that keeps being moved lays no further marks" $ do
        sc ← newScene 1 [(uidA, bleeder [slash 0.2], patrolAt 6.5 2 0)]
        let walk n = stepScene sc [(uidA, patrolAt 6.5 2 (secs n))]
        forM_ [1 .. 40] walk
        before ← length ⊚ decalsOf sc
        before `shouldSatisfy` (> 0)
        trailOf sc uidA >>= (`shouldSatisfy` (≢ Nothing))
        handleUnitKillCommand (scEnv sc) (scUts sc) uidA
        trailOf sc uidA `shouldReturn` Nothing
        forM_ [41 .. 140] walk
        length ⊚ decalsOf sc >>= (`shouldBe` before)
        trailOf sc uidA `shouldReturn` Nothing
