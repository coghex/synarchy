{-# LANGUAGE Strict, OverloadedStrings #-}
-- | Blood modes through the shipped definitions, the real combat wound tick,
--   the facade's transitions, the real motion path and the Lua consumers
--   (#2837, S10; I1-I3, T-S10-2, T-S10-3, T-S10-4).
--
--   Everything here drives the REAL machinery over a live headless engine:
--   'tickAllWounds' (the combat thread's one atomic fold), @physiology.setMode@
--   (the engine's atomic transition), 'handleUnitMoveToCommand' (the unit
--   thread's movement multiplier) and the shipped Lua (@unit_resources@,
--   @unit_resource_tick.checkRevive@). Nothing is rolled: every input is a
--   literal.
module Test.Headless.Unit.PhysiologyBloodModes.Modes (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Combat.Wounds (tickAllWounds, tickOneUnitWith)
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState)
import Infection.Types (emptyInfectionManager)
import qualified System.Random as Random
import Test.Headless.Unit.PhysiologyFixture (readShippedPhysiologyRegistry)
import Test.Headless.Unit.PhysiologyModes.Harness
import Test.Headless.Unit.PhysiologySpeciesProfiles (placeUnits, withShipped)
import Unit.Blood (bloodView)
import Unit.Command.Types (UnitCommand(..))
import Unit.Physiology
import Unit.Physiology.Transition
import Unit.Physiology.Types
import Unit.Sim.Types
import Unit.Thread.Command.Motion (handleUnitMoveToCommand)
import Unit.Types

spec ∷ Spec
spec = describe "blood modes through the real engine" $ do
    defaultsSpec
    interfaceSpec
    serialSpec
    transitionSpec
    movementSpec
    consumerSpec

-----------------------------------------------------------------------
-- Fixtures
-----------------------------------------------------------------------

-- | A unit whose blood is the system under test: every other system is
--   Nominal (the exempt tier) so no pool is read and nothing else drains.
bloodUnit ∷ Int → Maybe PhysioMode → Float → [Wound] → UnitSpec
bloodUnit uid pin vol ws = (unitSpec uid "acolyte" (Just "exempt"))
    { usBlood = Just vol, usWounds = ws
    , usOverrides = ("cardio", PhysioNominal)
                  : maybe [] (\m → [("blood", m)]) pin }

slash ∷ Float → Wound
slash sev = Wound
    { woundPart = "torso", woundKind = "slash", woundSeverity = sev
    , woundAt = 0, woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0
    , woundDressing = "", woundInfection = 0.0, woundClean = True
    , woundInfectionType = "", woundNecrosis = 0.0 }

maxBlood ∷ Float
maxBlood = 5.25

-- | Tick the real combat wound fold @n@ times.
ticks ∷ EngineEnv → Int → Float → IO ()
ticks env n dt = replicateM_ n (tickAllWounds env dt)

bloodOf ∷ EngineEnv → Int → IO Float
bloodOf env uid = uiBlood <$> readInstance env uid

setModeText ∷ LuaBackendState → Int → Text → Text → Text → IO Text
setModeText ls uid system mode values = luaText ls $
    "local ok, ch = __physiology.setMode(" <> tshow uid <> ", '" <> system
    <> "', '" <> mode <> "'" <> (if T.null values then "" else ", " <> values)
    <> "); return tostring(ok) .. '|' .. tostring(ch)"

-----------------------------------------------------------------------
-- Every default row through the shipped definitions, and explicit overrides
-----------------------------------------------------------------------

defaultsSpec ∷ Spec
defaultsSpec = describe "defaults and explicit overrides (I1)" $ do
    it "resolves Simulated for every shipped bodied species and Unsupported for \
       \the three bodyless ones, from the real definitions" $
        withShipped $ \(env, ls) → do
            let bodied = ["acolyte", "bear_brown", "technomule", "red_squirrel", "nomad_primitive"]
                bodyless = ["tiller", "white_tailed_deer", "unknown_unit"]
            forM_ (zip [1 ..] (bodied ⧺ bodyless)) $ \(uid, name) → do
                placeUnits env [(uid, name, Nothing, [], [])]
                luaText ls ("return tostring(unit.getPhysioMode(" <> tshow uid <> ", 'blood'))")
                    `shouldReturn` (if name `elem` bodied then "simulated" else "nil")

    it "reads a Nominal override's full volume, fraction 1.0 and live bleed \
       \rate, where the stored volume is a stale partial" $
        withScene [bloodUnit 1 (Just PhysioNominal) 1.0 [slash 0.5]] $ \(_, ls) → do
            readOwned ls 1 "blood" `shouldReturn` "5.25|nominal"
            readOwned ls 1 "blood_fraction" `shouldReturn` "1|nominal"
            luaNum ls "unit.getBlood(1).current" >>= near 1e-5 (realToFrac maxBlood)
            luaNum ls "unit.getBlood(1).bleedRate" >>= near 1e-5 0.3

    it "debits no volume, recovers none and raises no collapse or \
       \exsanguination over 600 tick-seconds of real ticks, while the wound \
       \keeps clotting; the Simulated twin drains" $
        withScene [ bloodUnit 1 (Just PhysioNominal) 1.0 [slash 0.5]
                  , bloodUnit 2 (Just PhysioSimulated) maxBlood [slash 0.5] ] $ \(env, _) → do
            ticks env 600 1.0
            bloodOf env 1 `shouldReturn` 1.0
            bloodOf env 2 >>= (`shouldSatisfy` (< maxBlood - 0.5))
            drainCommands env >>= \cs → [ () | UnitCollapse (UnitId 1) ← cs ]
                `shouldBe` []
            i ← readInstance env 1
            map woundClot (uiWounds i) `shouldSatisfy` all (> 0.5)

-----------------------------------------------------------------------
-- I2: the interface edges against healthy stubs
-----------------------------------------------------------------------

interfaceSpec ∷ Spec
interfaceSpec = describe "interface equivalence (I2)" $ do
    it "blood -> cardio: a Nominal blood (a stale low stored volume) drives \
       \cardio identically to a Simulated blood at its full maximum over 600 \
       \ticks, within 1e-6, while a bled Simulated control does not" $
        withScene
            [ (bloodUnit 1 (Just PhysioNominal) 1.0 []) { usOverrides =
                  [("blood", PhysioNominal), ("cardio", PhysioSimulated)] }
            , (bloodUnit 2 (Just PhysioSimulated) maxBlood []) { usOverrides =
                  [("blood", PhysioSimulated), ("cardio", PhysioSimulated)] }
            , (bloodUnit 3 (Just PhysioSimulated) (0.4 * maxBlood) []) { usOverrides =
                  [("blood", PhysioSimulated), ("cardio", PhysioSimulated)] } ] $
            \(_, ls) → do
                out ← luaText ls $ T.unlines
                    [ "local worst, finite = 0, true"
                    , "for _ = 1, 600 do"
                    , "  __resources.update(0.1)"
                    , "  for _, k in ipairs({ 'heart_rate', 'blood_oxygen', 'circulation' }) do"
                    , "    local a, b = unit.readPhysio(1, k), unit.readPhysio(2, k)"
                    , "    if a ~= a or b ~= b then finite = false end"
                    , "    worst = math.max(worst, math.abs(a - b))"
                    , "  end"
                    , "end"
                    , "return tostring(finite) .. '|' .. string.format('%.3g', worst)" ]
                let (finite, rest) = T.breakOn "|" out
                finite `shouldBe` "true"
                case reads (T.unpack (T.drop 1 rest)) of
                    [(v, "")] → (v ∷ Double) `shouldSatisfy` (< 1e-6)
                    _ → expectationFailure ("not a number: " <> T.unpack rest)
                luaNum ls "unit.readPhysio(3, 'heart_rate')" >>= \h → h `shouldSatisfy` (> 90)

    it "nutrition -> blood: Simulated blood recovery over a Nominal nutrition \
       \and hydration matches a complete healthy Simulated stub over 600 \
       \tick-seconds, within 1e-6, and is blocked by actual bleeding" $
        withScene
            [ (bloodUnit 1 (Just PhysioSimulated) (0.5 * maxBlood) []) { usOverrides =
                  [("cardio", PhysioNominal), ("blood", PhysioSimulated)] }
            , (bloodUnit 2 (Just PhysioSimulated) (0.5 * maxBlood) []) { usOverrides =
                  [ ("cardio", PhysioNominal), ("blood", PhysioSimulated)
                  , ("nutrition", PhysioSimulated), ("hydration", PhysioSimulated) ] }
            , bloodUnit 3 (Just PhysioSimulated) (0.5 * maxBlood)
                  [ (slash 0.3) { woundKind = "internal" } ] ] $ \(env, _) → do
            ticks env 600 1.0
            a ← bloodOf env 1
            b ← bloodOf env 2
            c ← bloodOf env 3
            abs (a - b) `shouldSatisfy` (< 1e-6)
            -- a real climb (about 0.00085 L per tick-second), not a constant
            a `shouldSatisfy` (> 0.5 * maxBlood + 0.4)
            -- the internal bleeding drained and nothing recovered
            c `shouldSatisfy` (< 0.5 * maxBlood)

-----------------------------------------------------------------------
-- T-S10-2: transitions and the real tick equal a valid serial order
-----------------------------------------------------------------------

-- | One operation of an interleaving.
data Op = Tick | ToNominal | ToSimulated deriving (Eq, Show)

-- | The unit as a pure model sees it: volume, pin and wounds.
type Model = (Float, PhysioOverrides, [Wound])

-- | Apply an operation to a model through the SAME pure functions the engine
--   applies atomically (the wound fold, the transition plan).
applyOp ∷ PhysiologyRegistry → UnitDef → UnitInstance → Op → UnitInstance
applyOp reg def inst op = case op of
    Tick →
        let (i, _, _) = tickOneUnitWith reg 0 def 1.0 emptyInfectionManager Nothing
                            (Random.mkStdGen 7) inst False
        in i
    ToNominal → transition PhysioNominal
    ToSimulated → transition PhysioSimulated
  where
    transition m =
        let ctx = PhysioContext
                { pcRegistry = reg, pcPhysiology = udPhysiology def
                , pcOverrides = uiPhysioOverrides inst, pcNow = 0
                , pcStats = uiStats inst, pcModifiers = uiModifiers inst
                , pcBlood = bloodView def inst }
        in case planSetMode ctx "blood" m Nothing of
            Left _ → inst
            Right t → inst { uiStats = trStats t
                           , uiBlood = fromMaybe (uiBlood inst) (trBlood t)
                           , uiPhysioOverrides = trOverrides t }

observe ∷ UnitInstance → Model
observe i = (uiBlood i, uiPhysioOverrides i, uiWounds i)

-- | Every interleaving of two op sequences, order inside each preserved.
interleave ∷ [α] → [α] → [[α]]
interleave [] ys = [ys]
interleave xs [] = [xs]
interleave (x : xs) (y : ys) = map (x :) (interleave xs (y : ys))
                            ⧺ map (y :) (interleave (x : xs) ys)

serialSpec ∷ Spec
serialSpec = beforeAll readShippedPhysiologyRegistry $
    describe "interleavings of transitions and the wound tick (T-S10-2)" $ do
    it "every program order of tick / Nominal / Simulated through the REAL \
       \tick and the facade equals the pure serial order" $ \reg →
        forM_ (interleave [Tick, Tick, Tick] [ToNominal, ToSimulated]) $ \ops →
            withScene [bloodUnit 1 (Just PhysioSimulated) maxBlood [slash 0.4]] $ \(env, ls) → do
                um0 ← readIORef (unitManagerRef env)
                let def = fromMaybe (error "no def") (HM.lookup "acolyte" (umDefs um0))
                    inst0 = fromMaybe (error "no unit") (HM.lookup (UnitId 1) (umInstances um0))
                    expected = observe (foldl' (applyOp reg def) inst0 ops)
                forM_ ops $ \op → case op of
                    Tick → tickAllWounds env 1.0
                    ToNominal → void (setModeText ls 1 "blood" "nominal" "")
                    ToSimulated → void (setModeText ls 1 "blood" "simulated" "")
                got ← observe <$> readInstance env 1
                (ops, got) `shouldBe` (ops, expected)

    it "a tick racing transitions on another thread always lands on some \
       \serial order: no Simulated drain is lost, and a Nominal unit's volume \
       \is its maximum in EVERY coherent state" $ \reg → do
        withScene [bloodUnit 1 (Just PhysioSimulated) maxBlood [slash 0.4]] $ \(env, ls) → do
            um0 ← readIORef (unitManagerRef env)
            let def = fromMaybe (error "no def") (HM.lookup "acolyte" (umDefs um0))
                inst0 = fromMaybe (error "no unit") (HM.lookup (UnitId 1) (umInstances um0))
                serial ops = observe (foldl' (applyOp reg def) inst0 ops)
                valid = map serial (interleave [Tick, Tick, Tick, Tick] [ToNominal, ToSimulated])
            bad ← newIORef (0 ∷ Int)
            done ← newEmptyMVar
            _ ← forkIO (replicateM_ 4 (tickAllWounds env 1.0) ≫ putMVar done ())
            -- sample coherent states while the tick thread runs
            let sample = do
                    i ← readInstance env 1
                    let nominalNow = HM.lookup "blood" (uiPhysioOverrides i) ≡ Just PhysioNominal
                    when (nominalNow ∧ abs (uiBlood i - maxBlood) > 1e-6) $
                        modifyIORef' bad (+ 1)
            _ ← setModeText ls 1 "blood" "nominal" "" ; sample
            threadDelay 200
            _ ← setModeText ls 1 "blood" "simulated" "" ; sample
            takeMVar done
            readIORef bad `shouldReturn` 0
            final ← observe <$> readInstance env 1
            final `shouldSatisfy` (`elem` valid)

-----------------------------------------------------------------------
-- T-S10-3: seeds, values and refusals
-----------------------------------------------------------------------

transitionSpec ∷ Spec
transitionSpec = describe "transitions (T-S10-3)" $ do
    it "stores uiBlood = base body mass x 0.075 on Nominal -> Simulated, \
       \matching getBlood's maximum, under a body-mass modifier, and writes \
       \the maximum on the way back to Nominal" $
        withScene
            [ (bloodUnit 1 (Just PhysioNominal) 1.0 [])
                { usModifiers = [("body_mass", [StatModifier 0 "spec" Nothing 1.0])] } ] $
            \(env, ls) → do
                setModeText ls 1 "blood" "simulated" "" `shouldReturn` "true|true"
                bloodOf env 1 >>= near' maxBlood
                luaNum ls "unit.getBlood(1).max" >>= near 1e-5 (realToFrac maxBlood)
                i ← readInstance env 1
                HM.member "blood" (uiStats i) `shouldBe` False
                -- partial, then back to Nominal: the canonical maximum
                modifyInstance env 1 (\u → u { uiBlood = 2.0 })
                setModeText ls 1 "blood" "nominal" "" `shouldReturn` "true|true"
                bloodOf env 1 >>= near' maxBlood

    it "seeds a caller value within the maximum exactly, refuses a value \
       \above it, a negative one, one on a non-transition and every dead, \
       \unknown or Unsupported call, each changing nothing" $ do
        withScene [bloodUnit 1 (Just PhysioNominal) 1.0 []] $ \(env, ls) → do
            before ← readInstance env 1
            forM_ ["{ blood = 9 }", "{ blood = -1 }", "{ nonesuch = 1 }"] $ \v → do
                r ← setModeText ls 1 "blood" "simulated" v
                T.isPrefixOf "nil|" r `shouldBe` True
            r ← setModeText ls 1 "blood" "nominal" "{ blood = 1 }"
            T.isPrefixOf "nil|" r `shouldBe` True
            after ← readInstance env 1
            (uiBlood after, uiStats after, uiPhysioOverrides after)
                `shouldBe` (uiBlood before, uiStats before, uiPhysioOverrides before)
            setModeText ls 1 "blood" "simulated" "{ blood = 2.5 }" `shouldReturn` "true|true"
            bloodOf env 1 >>= near' 2.5
            -- a Simulated value write
            setModeText ls 1 "blood" "simulated" "{ blood = 1.25 }" `shouldReturn` "true|false"
            bloodOf env 1 >>= near' 1.25
        withScene [ (bloodUnit 1 Nothing maxBlood []) { usPose = "dead" }
                  , (unitSpec 2 "white_tailed_deer" Nothing) { usBlood = Just 0 } ] $
            \(env, ls) → do
                before ← mapM (readInstance env) [1, 2]
                forM_ [1, 2 ∷ Int] $ \u → do
                    r ← setModeText ls u "blood" "nominal" ""
                    T.isPrefixOf "nil|" r `shouldBe` True
                r ← setModeText ls 1 "nonesuch" "nominal" ""
                T.isPrefixOf "nil|" r `shouldBe` True
                after ← mapM (readInstance env) [1, 2]
                map uiBlood after `shouldBe` map uiBlood before
                map uiPhysioOverrides after `shouldBe` map uiPhysioOverrides before

    it "refuses a seed whose base body mass is non-finite or negative, whole, and \
       \mutates nothing" $
        forM_ [1 / 0, 0 / 0, -70] $ \mass →
            withScene [ bloodUnit 1 (Just PhysioNominal) 1.0 [] ] $ \(env, ls) → do
                -- the mass goes bad AFTER the spawn (a spawn with one is refused)
                modifyInstance env 1 $ \i →
                    i { uiStats = HM.insert "body_mass" mass (uiStats i) }
                before ← bloodOf env 1
                res ← setModeText ls 1 "blood" "simulated" ""
                T.isPrefixOf "nil|" res `shouldBe` True
                bloodOf env 1 >>= (`shouldBe` before)
                -- still pinned Nominal: a second attempt is refused the same way
                setModeText ls 1 "blood" "simulated" "" >>= (`shouldBe` res)

    forM_ [("+100 %", 1.0), ("-100 %", -1.0), ("-50 %", -0.5)] $ \(label, pct) →
        it ("ignores a " <> label <> " modifier carried under the key blood: the \
            \seed and a caller value are stored directly, the transition is \
            \never refused for it, and reads ignore it") $
            withScene [ (bloodUnit 1 (Just PhysioNominal) 1.0 [])
                          { usModifiers = [("blood", [StatModifier 0 "spec" Nothing pct])] } ] $
                \(env, ls) → do
                    setModeText ls 1 "blood" "simulated" "" `shouldReturn` "true|true"
                    bloodOf env 1 >>= near' maxBlood
                    readOwned ls 1 "blood" `shouldReturn` "5.25|simulated"
                    setModeText ls 1 "blood" "simulated" "{ blood = 2.5 }"
                        `shouldReturn` "true|false"
                    bloodOf env 1 >>= near' 2.5
                    readOwned ls 1 "blood" `shouldReturn` "2.5|simulated"
                    -- and a Nominal -> Simulated seed with a caller value
                    setModeText ls 1 "blood" "nominal" "" `shouldReturn` "true|true"
                    setModeText ls 1 "blood" "simulated" "{ blood = 1.25 }"
                        `shouldReturn` "true|true"
                    bloodOf env 1 >>= near' 1.25

    it "records a pin equal to the default as intent and runs the hook only \
       \on an effective change" $
        withScene [bloodUnit 1 Nothing maxBlood []] $ \(_, ls) → do
            _ ← luaText ls "_G.__hooks = 0; __physiology.onModeChanged('spec_blood', \
                           \'blood', function() __hooks = __hooks + 1 end); return true"
            setModeText ls 1 "blood" "simulated" "" `shouldReturn` "true|false"
            luaText ls "return tostring(unit.getPhysioOverride(1, 'blood'))"
                `shouldReturn` "simulated"
            luaText ls "return tostring(__hooks)" `shouldReturn` "0"
            setModeText ls 1 "blood" "nominal" "" `shouldReturn` "true|true"
            luaText ls "return tostring(__hooks)" `shouldReturn` "1"
            luaText ls "return tostring(select(2, __physiology.clearMode(1, 'blood')))"
                `shouldReturn` "true"
            luaText ls "return tostring(__hooks)" `shouldReturn` "2"
  where near' want got = abs (got - want) `shouldSatisfy` (< 1e-5)

-----------------------------------------------------------------------
-- T-S10-4: the real motion path
-----------------------------------------------------------------------

-- | The unit thread's state for the units, each standing and idle.
freshSim ∷ UnitSimState
freshSim = UnitSimState
    { usRealX = 10, usRealY = 10, usGridZ = 0, usRealZ = 0
    , usTarget = Nothing, usPose = Standing, usState = Idle, usFacing = DirS
    , usLocalPath = []
    , usDrinkUntil = Nothing, usEatUntil = Nothing, usPickupUntil = Nothing
    , usTransitionUntil = Nothing, usTransitionStride = 1
    , usPostTransition = []
    , usClimbFromTile = Nothing, usClimbToTile = Nothing
    , usClimbStartTime = Nothing, usClimbSlipAt = Nothing
    , usFallFromTile = Nothing, usFallToTile = Nothing
    , usPendingClimbXP = 0, usGetUpAt = Nothing, usPendingFallDrop = Nothing
    , usJumpApex = Nothing, usMoveGrade = 0 }

-- | The speed a move command commits for a unit through the REAL handler.
commandedSpeed ∷ EngineEnv → Int → Float → IO Float
commandedSpeed env uid speed = do
    utsRef ← newIORef emptyUnitThreadState
        { utsSimStates = HM.singleton (UnitId (fromIntegral uid)) freshSim }
    handleUnitMoveToCommand env utsRef (UnitId (fromIntegral uid)) 12 12 speed FallPermitted
    uts ← readIORef (utsRef ∷ IORef UnitThreadState)
    pure $ case HM.lookup (UnitId (fromIntegral uid)) (utsSimStates uts) ≫= usTarget of
        Just mt → mtSpeed mt
        Nothing → -1

-- | The scene's definition with a leg and a torso part, so wounds slow it.
withLimbs ∷ EngineEnv → IO ()
withLimbs env = modifyIORef' (unitManagerRef env) $ \um →
    um { umDefs = HM.map (\d → d { udBodyParts = [ part "torso" False True, part "leg" True False ] })
                         (umDefs um) }
  where
    part pid loco balance = BodyPart
        { bpId = pid, bpName = pid, bpParent = Nothing
        , bpVital = False, bpAreaWeight = 1.0, bpTacticalValue = 0.5
        , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 1
        , bpLayers = [], bpTargetable = True, bpDepth = 0.0
        , bpAffectsLocomotion = loco, bpAffectsBalance = balance }

movementSpec ∷ Spec
movementSpec = describe "the movement multiplier (T-S10-4)" $
    it "reads blood 1.0 for a Nominal unit whose stored volume is stale and \
       \partial and whose body mass has since grown, where an identical \
       \Simulated unit is slowed by its low blood; a leg wound slows both" $
        withScene
            [ (bloodUnit 1 (Just PhysioNominal) 0.5 []) { usStats = [("body_mass", 200)] }
            , (bloodUnit 2 (Just PhysioSimulated) 0.5 []) { usStats = [("body_mass", 200)] }
            , (bloodUnit 3 (Just PhysioNominal) 0.5 [(slash 0.5) { woundPart = "leg", woundBandage = 0 }])
                  { usStats = [("body_mass", 200)] }
            , (bloodUnit 4 (Just PhysioSimulated) 0.5 [(slash 0.5) { woundPart = "leg", woundBandage = 0 }])
                  { usStats = [("body_mass", 200)] } ] $ \(env, _) → do
            withLimbs env
            [s1, s2, s3, s4] ← mapM (\u → commandedSpeed env u 2.0) [1, 2, 3, 4]
            -- Nominal: no blood term at all
            s1 `shouldSatisfy` (\x → abs (x - 2.0) < 1e-5)
            -- Simulated at 0.5 / (200 x 0.075 = 15 L) = 3.3 %: 0.5 + 0.5 x 0.0333
            s2 `shouldSatisfy` (\x → abs (x - 2.0 * (0.5 + 0.5 * (0.5 / 15))) < 1e-4)
            -- the leg wound: (1 - 0.5 x 1.2) = 0.4 in both, blood term only in Simulated
            s3 `shouldSatisfy` (\x → abs (x - 2.0 * 0.4) < 1e-4)
            s4 `shouldSatisfy` (\x → abs (x - 2.0 * 0.4 * (0.5 + 0.5 * (0.5 / 15))) < 1e-4)

-----------------------------------------------------------------------
-- The Lua consumers
-----------------------------------------------------------------------

consumerSpec ∷ Spec
consumerSpec = describe "getBlood's Lua consumers" $
    it "lets a collapsed Nominal unit with a stale low stored volume pass the \
       \50 % revive gate where the identical Simulated unit stays down" $
        withScene
            [ (bloodUnit 1 (Just PhysioNominal) 1.0 []) { usPose = "collapsed" }
            , (bloodUnit 2 (Just PhysioSimulated) 1.0 []) { usPose = "collapsed" } ] $
            \(env, ls) → do
                _ ← drainCommands env
                _ ← luaText ls "local rt = require('scripts.unit_resource_tick'); \
                               \rt.checkRevive(1, {}); rt.checkRevive(2, {}); return true"
                cs ← drainCommands env
                [ u | UnitRevive u ← cs ] `shouldBe` [UnitId 1]
