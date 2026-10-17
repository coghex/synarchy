{-# LANGUAGE Strict, OverloadedStrings #-}
-- | Transitions and persistence of the cardio mode (#2836; design D-3, D-5,
--   D-6, I5, I6): the facade's atomic transitions over the real engine
--   verbs, and the durable state through the REAL session codec and the
--   real load gate.
--
--   No wire type is added: heart rate and blood oxygen ride @uiStats@ and
--   the cardio override rides the units-v4 override map. What is established
--   here is that transitions seed the effective healthy value
--   (modifier-aware, D-5 inversion) and scrub on the way out, that every
--   refusal leaves the unit exactly as it was, that the derived circulation
--   is fresh the instant a transition returns, that the integrators land
--   their EFFECTIVE value under retained modifiers, and that durable partial
--   values and pins round-trip exactly while a legacy stored circulation is
--   scrubbed once.
module Test.Headless.Unit.PhysiologyCardioCirculationModes.Persistence (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Engine.Core.State (EngineEnv)
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Unit.PhysiologyFixture (readShippedPhysiologyRegistry)
import Test.Headless.Unit.PhysiologyModes.Harness
import Test.Headless.Unit.PhysiologySpeciesProfiles
    ( codecRoundTrip, gate, onSavePage, pageSnapshotOf, placeUnits
    , shippedDefs, withShipped )
import Unit.Physiology.Types
import Unit.Types
import World.Save.Physiology (PhysiologyNormalization(..))
import World.Save.Snapshot (PageSnapshot(..))
import World.Save.Types (UnitInstanceSnapshot(..), UnitSnapshot(..))

spec ∷ Spec
spec = describe "transitions and persistence" $ do
    seedSpec
    refusalSpec
    intentSpec
    freshSpec
    invariantSpec
    tickSpec
    persistenceSpec

-----------------------------------------------------------------------
-- Fixtures
-----------------------------------------------------------------------

-- | Cardio's (owned key, healthy value) pairs.
cardioKeys ∷ [(Text, Double)]
cardioKeys = [("heart_rate", 70), ("blood_oxygen", 1)]

-- | An acolyte-config unit with cardio Nominal (a pin) and every other system
--   Nominal, carrying one percentage modifier on an OWNED key (+50 % is 0.5,
--   -100 % is -1.0).
modded ∷ Text → Float → UnitSpec
modded key pct = (unitSpec 1 "acolyte" (Just "exempt"))
    { usOverrides = [("cardio", PhysioNominal)]
    , usModifiers = [(key, [StatModifier 0 "spec" Nothing pct])] }

-- | Every system Simulated.
simulated ∷ UnitSpec
simulated = unitSpec 1 "acolyte" (Just "rich")

-- | The cardio keys a unit stores and every override it pins, as the engine
--   holds them now.
snapshot ∷ EngineEnv → Int → IO ([(Text, Float)], [(Text, PhysioMode)])
snapshot env uid = do
    i ← readInstance env uid
    pure ( [ (k, v) | (k, _) ← cardioKeys, Just v ← [HM.lookup k (uiStats i)] ]
         , HM.toList (uiPhysioOverrides i) )

-- | The facade's answer as @ok|changed@ or @nil|reason@.
setModeText ∷ LuaBackendState → Text → Text → Text → IO Text
setModeText ls system mode values = luaText ls $
    "local ok, ch = __physiology.setMode(1, '" <> system <> "', '" <> mode
    <> "'" <> (if T.null values then "" else ", " <> values)
    <> "); return tostring(ok) .. '|' .. tostring(ch)"

shouldRefuse ∷ IO Text → Expectation
shouldRefuse act = act >>= (`shouldSatisfy` T.isPrefixOf "nil|")

readNum ∷ LuaBackendState → Int → Text → IO Double
readNum ls uid key = luaNum ls ("unit.readPhysio(" <> tshow uid <> ", '" <> key <> "')")

-----------------------------------------------------------------------
-- Seeds and scrubs (modifier-aware, D-5)
-----------------------------------------------------------------------

seedSpec ∷ Spec
seedSpec = describe "Nominal -> Simulated seeds the effective healthy value" $
    forM_ cardioKeys $ \(key, healthy) →
        forM_ [("+50 %", 0.5), ("-50 %", -0.5), ("none", 0)] $ \(label, pct) →
            it (T.unpack key <> " with a " <> label <> " modifier on it: the \
                \effective value is the healthy value, the stored base is its \
                \inversion, the modifier is kept, and Nominal ignored it") $
                withScene [modded key pct] $ \(env, ls) → do
                    -- Nominal first: the modifier is retained but ignored
                    readNum ls 1 key >>= near 1e-5 healthy
                    setModeText ls "cardio" "simulated" "" `shouldReturn` "true|true"
                    readNum ls 1 key >>= near 1e-5 healthy
                    luaNum ls ("unit.getStatBase(1, '" <> key <> "')")
                        >>= near 1e-5 (healthy / (1 + realToFrac pct))
                    i ← readInstance env 1
                    HM.size (uiModifiers i) `shouldBe` 1
                    -- and back: scrubbed, the modifier still retained and
                    -- still ignored
                    setModeText ls "cardio" "nominal" "" `shouldReturn` "true|true"
                    i' ← readInstance env 1
                    forM_ (map fst cardioKeys) $ \k →
                        (k, HM.member k (uiStats i')) `shouldBe` (k, False)
                    uiModifiers i' `shouldBe` uiModifiers i
                    readNum ls 1 key >>= near 1e-5 healthy

-----------------------------------------------------------------------
-- Refusals change nothing
-----------------------------------------------------------------------

refusalSpec ∷ Spec
refusalSpec = describe "refusals are atomic" $ do
    forM_ cardioKeys $ \(key, _) →
        it ("a -100 % modifier on " <> T.unpack key <> " refuses the seed of \
            \cardio and leaves the unit exactly as it was") $
            withScene [modded key (-1.0)] $ \(env, ls) → do
                before ← snapshot env 1
                iBefore ← readInstance env 1
                shouldRefuse (setModeText ls "cardio" "simulated" "")
                snapshot env 1 `shouldReturn` before
                iAfter ← readInstance env 1
                uiModifiers iAfter `shouldBe` uiModifiers iBefore
                uiStats iAfter `shouldBe` uiStats iBefore
                luaText ls "return tostring(unit.getPhysioMode(1, 'cardio'))"
                    `shouldReturn` "nominal"

    it "refuses an unknown system, a dead unit and a bodyless unit without any \
       \partial change" $ do
        withScene [simulated { usPose = "dead" }] $ \(env, ls) → do
            before ← snapshot env 1
            shouldRefuse (setModeText ls "cardio" "nominal" "")
            snapshot env 1 `shouldReturn` before
        withScene [simulated] $ \(env, ls) → do
            before ← snapshot env 1
            shouldRefuse (setModeText ls "nonesuch" "nominal" "")
            snapshot env 1 `shouldReturn` before
        withScene [unitSpec 1 "white_tailed_deer" Nothing] $ \(env, ls) → do
            before ← snapshot env 1
            shouldRefuse (setModeText ls "cardio" "simulated" "")
            luaText ls "return tostring(unit.getPhysioMode(1, 'cardio'))"
                `shouldReturn` "nil"
            snapshot env 1 `shouldReturn` before

-----------------------------------------------------------------------
-- Intent
-----------------------------------------------------------------------

intentSpec ∷ Spec
intentSpec = describe "set always pins intent; hooks run only on an effective change" $
    it "records a pin equal to the default, fires no hook for it, and clears \
       \it back to the default" $
        withScene [simulated] $ \(env, ls) → do
            _ ← luaText ls "_G.__hooks = 0; \
                           \__physiology.onModeChanged('spec_probe', 'cardio', \
                           \function() __hooks = __hooks + 1 end); return true"
            let hooks = luaText ls "return tostring(__hooks)"
                override = luaText ls "return tostring(unit.getPhysioOverride(1, 'cardio'))"
            before ← snapshot env 1
            -- a set equal to the effective mode: pinned, nothing else happens
            setModeText ls "cardio" "simulated" "" `shouldReturn` "true|false"
            override `shouldReturn` "simulated"
            hooks `shouldReturn` "0"
            fst <$> snapshot env 1 `shouldReturn` fst before
            -- clearing the pin that equals the default changes nothing either
            luaText ls "return tostring(select(2, __physiology.clearMode(1, 'cardio')))"
                `shouldReturn` "false"
            override `shouldReturn` "nil"
            hooks `shouldReturn` "0"
            -- an effective change fires the hook once, in both directions
            setModeText ls "cardio" "nominal" "" `shouldReturn` "true|true"
            hooks `shouldReturn` "1"
            override `shouldReturn` "nominal"
            luaText ls "return tostring(select(2, __physiology.clearMode(1, 'cardio')))"
                `shouldReturn` "true"
            hooks `shouldReturn` "2"
            readNum ls 1 "heart_rate" >>= near 1e-6 70
            -- another system's change is not this hook's event
            setModeText ls "sleep" "nominal" "" `shouldReturn` "true|true"
            hooks `shouldReturn` "2"

-----------------------------------------------------------------------
-- Derived reads are fresh before a transition returns
-----------------------------------------------------------------------

freshSpec ∷ Spec
freshSpec = describe "the derived circulation is fresh before a transition returns" $
    it "drops the slow heart's factor the moment cardio goes Nominal, and \
       \reseeds the healthy heart when it returns, with no tick, and refuses \
       \a raw circulation write in either mode" $
        withScene [simulated { usBlood = Just (0.5 * 5.25)
                             , usStats = [("heart_rate", 35)] }] $ \(env, ls) → do
            -- 1.1 x 0.5 x (35 / 70)
            readNum ls 1 "circulation" >>= near 1e-5 0.275
            luaText ls "return tostring(unit.setStat(1, 'circulation', 0.9))"
                `shouldReturn` "false"
            setModeText ls "cardio" "nominal" "" `shouldReturn` "true|true"
            readNum ls 1 "circulation" >>= near 1e-5 0.55
            luaText ls "return tostring(unit.setStat(1, 'circulation', 0.9))"
                `shouldReturn` "false"
            setModeText ls "cardio" "simulated" "" `shouldReturn` "true|true"
            readNum ls 1 "heart_rate" >>= near 1e-6 70
            readNum ls 1 "circulation" >>= near 1e-5 0.55
            i ← readInstance env 1
            HM.member "circulation" (uiStats i) `shouldBe` False

-----------------------------------------------------------------------
-- A Simulated owned key that is missing is an invariant error
-----------------------------------------------------------------------

invariantSpec ∷ Spec
invariantSpec = describe "missing Simulated owned state is an invariant error" $
    forM_ (map fst cardioKeys) $ \key →
        it ("raises, never falls back, when " <> T.unpack key <> " is gone") $
            withScene [simulated] $ \(env, ls) → do
                modifyInstance env 1 (\u → u { uiStats = HM.delete key (uiStats u) })
                luaText ls ("return tostring((pcall(unit.readPhysio, 1, '" <> key <> "')))")
                    `shouldReturn` "false"
                -- the derived circulation reads the heart rate, so it cannot
                -- be computed without one either
                when (key ≡ "heart_rate") $
                    luaText ls "return tostring((pcall(unit.readPhysio, 1, 'circulation')))"
                        `shouldReturn` "false"

-----------------------------------------------------------------------
-- The integrators land their EFFECTIVE value under retained modifiers
-----------------------------------------------------------------------

-- | A running, Simulated-cardio unit with one percentage modifier on @key@
--   unless @pct@ is 0, at a blood level that moves oxygen too. Units of one
--   scene share the definition, so the mode is the unit's pin.
ticking ∷ Int → Text → Float → UnitSpec
ticking uid key pct = (unitSpec uid "acolyte" (Just "exempt"))
    { usOverrides = [("cardio", PhysioSimulated)]
    , usActivity = "running"
    , usBlood = Just (0.4 * 5.25)
    , usModifiers = [ (key, [StatModifier 0 "spec" Nothing pct]) | pct ≢ 0 ] }

tickSpec ∷ Spec
tickSpec = describe "the cardio tick lands its EFFECTIVE value under retained modifiers" $
    forM_ cardioKeys $ \(key, healthy) →
        it ("a +50 % and a -50 % modifier on " <> T.unpack key <> " follow \
            \exactly the trajectory of an unmodified twin through real \
            \resource updates, and the seed survives the first tick") $
            withScene [ticking 1 key 0.5, ticking 2 key (-0.5), ticking 3 key 0] $
                \(env, ls) → do
                    forM_ [1, 2, 3 ∷ Int] $ \u → readNum ls u key >>= near 1e-4 healthy
                    runSeconds ls 0.1 0.1
                    -- one 0.1 s tick moves the integrators by a few hundredths
                    -- at most; an undone seed would read 1.5x or 0.5x
                    forM_ [1, 2, 3 ∷ Int] $ \u →
                        readNum ls u key >>= near (0.05 * healthy) healthy
                    forM_ [30, 90] $ \seconds → do
                        runSeconds ls seconds 0.1
                        twin ← readNum ls 3 key
                        forM_ [1, 2 ∷ Int] $ \u → readNum ls u key >>= near 2e-3 twin
                    -- the keys really moved: this is not comparing constants
                    readNum ls 3 "heart_rate" >>= \hr → hr `shouldSatisfy` (> 100)
                    forM_ [1, 2 ∷ Int] $ \u → do
                        i ← readInstance env u
                        HM.size (uiModifiers i) `shouldBe` 1
                    scriptErrors ls `shouldReturn` ""

-----------------------------------------------------------------------
-- Persistence (I6)
-----------------------------------------------------------------------

persistenceSpec ∷ Spec
persistenceSpec = describe "persistence through the real codec and load gate" $ do
    it "round-trips partial durable heart rate and oxygen and every pin, \
       \including one equal to the default, exactly, with no normalization, \
       \and a second pass is a no-op" $
        withShipped $ \(env, _) → do
            reg ← readShippedPhysiologyRegistry
            defs ← shippedDefs env
            placeUnits env
                [ (1, "acolyte", Nothing, []
                  , [("heart_rate", 88.5), ("blood_oxygen", 0.42)])
                , (2, "nomad_primitive", Nothing, [("cardio", PhysioNominal)], [])
                , (3, "red_squirrel", Nothing, [("cardio", PhysioSimulated)]
                  , [("heart_rate", 61)]) ]
            onSavePage env [1, 2, 3]
            before ← mapM (readInstance env) [1, 2, 3]
            pg ← pageSnapshotOf env ≫= codecRoundTrip
            (us, notes) ← gate reg defs (pgsUnits pg)
            notes `shouldBe` []
            let unitOf i = HM.lookup (UnitId i) (usnInstances us)
            case (unitOf 1, unitOf 2, unitOf 3) of
                (Just a, Just n, Just s) → do
                    HM.lookup "heart_rate" (uisStats a) `shouldBe` Just 88.5
                    HM.lookup "blood_oxygen" (uisStats a) `shouldBe` Just 0.42
                    uisStats a `shouldBe` uiStats (before !! 0)
                    forM_ (map fst cardioKeys) $ \k →
                        HM.member k (uisStats n) `shouldBe` False
                    uisPhysioOverrides n
                        `shouldBe` HM.fromList [("cardio", PhysioNominal)]
                    uisStats n `shouldBe` uiStats (before !! 1)
                    -- a pin EQUAL to the species default is intent, kept
                    uisPhysioOverrides s
                        `shouldBe` HM.fromList [("cardio", PhysioSimulated)]
                    HM.lookup "heart_rate" (uisStats s) `shouldBe` Just 61
                    uisStats s `shouldBe` uiStats (before !! 2)
                _ → expectationFailure "a unit vanished"
            (us', notes') ← gate reg defs us
            (us', notes') `shouldBe` (us, [])

    it "scrubs a legacy stored circulation in every mode, logged once, and a \
       \re-save and reload performs zero further repairs; a bodyless unit's \
       \integrated heart rate and oxygen go with it" $
        withShipped $ \(env, _) → do
            reg ← readShippedPhysiologyRegistry
            defs ← shippedDefs env
            placeUnits env
                [ (1, "acolyte", Nothing, [], [])
                , (2, "acolyte", Nothing, [("cardio", PhysioNominal)], [])
                , (3, "white_tailed_deer", Nothing, [], []) ]
            onSavePage env [1, 2, 3]
            -- an older build stored circulation for everyone, and integrated
            -- a heart rate and oxygen for the bodyless unit
            forM_ [1, 2, 3] $ \u → modifyInstance env u $ \i → i
                { uiStats = HM.insert "circulation" 0.3 (uiStats i) }
            modifyInstance env 3 $ \i → i
                { uiStats = HM.insert "heart_rate" 190
                          (HM.insert "blood_oxygen" 0.31 (uiStats i)) }
            pg ← pageSnapshotOf env ≫= codecRoundTrip
            (us1, notes1) ← gate reg defs (pgsUnits pg)
            length notes1 `shouldBe` 3
            let named = [ T.unwords ns | PhysiologyNormalization _ _ _ ns ← notes1 ]
            forM_ named $ \n → T.isInfixOf "circulation" n `shouldBe` True
            -- only the bodyless unit also loses its integrators
            length [ () | n ← named, T.isInfixOf "heart_rate" n ] `shouldBe` 1
            forM_ [1, 2, 3] $ \u → case HM.lookup (UnitId u) (usnInstances us1) of
                Nothing → expectationFailure "the unit vanished"
                Just i → HM.member "circulation" (uisStats i) `shouldBe` False
            case HM.lookup (UnitId 3) (usnInstances us1) of
                Just i → forM_ ["heart_rate", "blood_oxygen"] $ \k →
                    HM.member k (uisStats i) `shouldBe` False
                Nothing → expectationFailure "the bodyless unit vanished"
            (us2, notes2) ← gate reg defs us1
            (us2, notes2) `shouldBe` (us1, [])
            back ← codecRoundTrip pg { pgsUnits = us2 }
            (us3, notes3) ← gate reg defs (pgsUnits back)
            (us3, notes3) `shouldBe` (us2, [])

    it "computes the canonical circulation before the first paused observation \
       \without storing it" $
        withScene [simulated { usBlood = Just (0.5 * 5.25) }] $ \(env, ls) → do
            _ ← luaText ls "__resources.onSaveLoaded({ 1 }, {}); return true"
            readNum ls 1 "circulation" >>= near 1e-5 0.55
            i ← readInstance env 1
            HM.member "circulation" (uiStats i) `shouldBe` False
