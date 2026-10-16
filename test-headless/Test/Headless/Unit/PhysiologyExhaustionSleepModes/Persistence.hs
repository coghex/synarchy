{-# LANGUAGE Strict, OverloadedStrings #-}
-- | Transitions and persistence of the exhaustion and sleep modes (#2835;
--   design D-3, D-5, D-6, I5, I6): the facade's atomic transitions over the
--   real engine verbs, and the durable state through the REAL session codec
--   and the real load gate.
--
--   No wire type is added: both pools ride @uiStats@ and their overrides
--   ride the units-v4 override map. What is established here is that
--   transitions seed the FULL effective value (modifier-aware, D-5
--   inversion) and scrub on the way out, that every refusal leaves the unit
--   exactly as it was, that mode-change hooks and transient resets fire
--   only on what actually changed, and that durable partial pools and pins
--   round-trip exactly while a profile change is normalized once.
module Test.Headless.Unit.PhysiologyExhaustionSleepModes.Persistence (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Engine.Core.State (EngineEnv)
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Unit.PhysiologyFixture
    (profilePhysiology, readShippedPhysiologyRegistry)
import Test.Headless.Unit.PhysiologyModes.Harness
import Test.Headless.Unit.PhysiologySpeciesProfiles
    ( codecRoundTrip, gate, nonStoredKeys, onSavePage, pageSnapshotOf
    , placeUnits, shippedDefs, withShipped )
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
    contributionSpec
    resetSpec
    persistenceSpec

-----------------------------------------------------------------------
-- Fixtures
-----------------------------------------------------------------------

-- | Both systems' (system, owned key) pairs.
systemKeys ∷ [(Text, Text)]
systemKeys = [("exhaustion", "exhaustion"), ("sleep", "sleep_pressure")]

-- | An endurance-1 acolyte-config unit with both systems Nominal, carrying
--   one percentage modifier on an OWNED key (+50 % is 0.5, -100 % is -1.0).
modded ∷ Text → Float → UnitSpec
modded key pct = (unitSpec 1 "acolyte" (Just "exempt"))
    { usModes = [("exhaustion", PhysioNominal), ("sleep", PhysioNominal)]
    , usModifiers = [(key, [StatModifier 0 "spec" Nothing pct])] }

-- | Both systems Simulated.
simulated ∷ UnitSpec
simulated = unitSpec 1 "acolyte" (Just "rich")

-- | The two pools a unit stores and every override it pins, as the engine
--   holds them now.
snapshot ∷ EngineEnv → Int → IO ([(Text, Float)], [(Text, PhysioMode)])
snapshot env uid = do
    i ← readInstance env uid
    pure ( [ (k, v) | k ← map snd systemKeys, Just v ← [HM.lookup k (uiStats i)] ]
         , HM.toList (uiPhysioOverrides i) )

-- | The facade's answer as @ok|changed@ or @nil|reason@.
setModeText ∷ LuaBackendState → Text → Text → Text → IO Text
setModeText ls system mode values = luaText ls $
    "local ok, ch = __physiology.setMode(1, '" <> system <> "', '" <> mode
    <> "'" <> (if T.null values then "" else ", " <> values)
    <> "); return tostring(ok) .. '|' .. tostring(ch)"

-- | The facade answered @nil|reason@.
shouldRefuse ∷ IO Text → Expectation
shouldRefuse act = act >>= (`shouldSatisfy` T.isPrefixOf "nil|")

-----------------------------------------------------------------------
-- Seeds and scrubs (modifier-aware, D-5)
-----------------------------------------------------------------------

seedSpec ∷ Spec
seedSpec = describe "Nominal -> Simulated seeds the full effective value" $ do
    forM_ systemKeys $ \(sid, key) →
        forM_ [("+50 %", 0.5, 10 / 1.5), ("-50 %", -0.5, 20), ("none", 0, 10)] $
            \(label, pct, base) →
                it (T.unpack sid <> " with a " <> label <> " modifier on "
                    <> T.unpack key <> ": effective value is the healthy value, \
                    \the stored base is its inversion, the modifier is kept") $
                    withScene [modded key pct] $ \(env, ls) → do
                        -- Nominal first: the modifier is retained but ignored
                        luaNum ls ("unit.readPhysio(1, '" <> key <> "')")
                            >>= near 1e-4 10
                        setModeText ls sid "simulated" "" `shouldReturn` "true|true"
                        luaNum ls ("unit.readPhysio(1, '" <> key <> "')")
                            >>= near 1e-4 10
                        luaNum ls ("unit.getStatBase(1, '" <> key <> "')")
                            >>= near 1e-4 base
                        i ← readInstance env 1
                        HM.size (uiModifiers i) `shouldBe` 1
                        -- and back: scrubbed, the modifier still retained and
                        -- still ignored
                        setModeText ls sid "nominal" "" `shouldReturn` "true|true"
                        i' ← readInstance env 1
                        HM.member key (uiStats i') `shouldBe` False
                        uiModifiers i' `shouldBe` uiModifiers i
                        luaNum ls ("unit.readPhysio(1, '" <> key <> "')")
                            >>= near 1e-4 10

    it "seeds explicit effective targets, capped at the healthy value" $
        withScene [modded "exhaustion" 0] $ \(env, ls) → do
            setModeText ls "exhaustion" "simulated" "{ exhaustion = 4 }"
                `shouldReturn` "true|true"
            luaNum ls "unit.readPhysio(1, 'exhaustion')" >>= near 1e-6 4
            -- a target above the healthy value is refused and changes nothing
            before ← snapshot env 1
            shouldRefuse (setModeText ls "exhaustion" "simulated" "{ exhaustion = 11 }")
            snapshot env 1 `shouldReturn` before

-----------------------------------------------------------------------
-- Refusals change nothing
-----------------------------------------------------------------------

refusalSpec ∷ Spec
refusalSpec = describe "refusals are atomic" $ do
    forM_ systemKeys $ \(sid, key) →
        it ("a -100 % modifier on " <> T.unpack key <> " refuses the seed of "
            <> T.unpack sid <> " and leaves the unit exactly as it was") $
            withScene [modded key (-1.0)] $ \(env, ls) → do
                before ← snapshot env 1
                iBefore ← readInstance env 1
                shouldRefuse (setModeText ls sid "simulated" "")
                snapshot env 1 `shouldReturn` before
                iAfter ← readInstance env 1
                uiModifiers iAfter `shouldBe` uiModifiers iBefore
                luaText ls ("return tostring(unit.getPhysioMode(1, '" <> sid <> "'))")
                    `shouldReturn` "nominal"

    it "refuses an unknown system, a dead unit and a bodyless unit without \
       \any partial change" $ do
        withScene [simulated { usPose = "dead" }] $ \(env, ls) → do
            before ← snapshot env 1
            shouldRefuse (setModeText ls "sleep" "nominal" "")
            snapshot env 1 `shouldReturn` before
        withScene [simulated] $ \(env, ls) → do
            before ← snapshot env 1
            shouldRefuse (setModeText ls "nonesuch" "nominal" "")
            snapshot env 1 `shouldReturn` before
        withScene [unitSpec 1 "white_tailed_deer" Nothing] $ \(env, ls) → do
            before ← snapshot env 1
            forM_ (map fst systemKeys) $ \sid → do
                shouldRefuse (setModeText ls sid "simulated" "")
                luaText ls ("return tostring(unit.getPhysioMode(1, '" <> sid <> "'))")
                    `shouldReturn` "nil"
            snapshot env 1 `shouldReturn` before

-----------------------------------------------------------------------
-- Intent
-----------------------------------------------------------------------

intentSpec ∷ Spec
intentSpec = describe "set always pins intent; hooks run only on an effective change" $
    it "records a pin equal to the default, fires no hook for it, and clears it \
       \back to the default" $
        withScene [simulated] $ \(env, ls) → do
            _ ← luaText ls "_G.__hooks = 0; \
                           \__physiology.onModeChanged('spec_probe', 'sleep', \
                           \function() __hooks = __hooks + 1 end); return true"
            let hooks = luaText ls "return tostring(__hooks)"
                override = luaText ls "return tostring(unit.getPhysioOverride(1, 'sleep'))"
            before ← snapshot env 1
            -- a set equal to the effective mode: pinned, nothing else happens
            setModeText ls "sleep" "simulated" "" `shouldReturn` "true|false"
            override `shouldReturn` "simulated"
            hooks `shouldReturn` "0"
            fst <$> snapshot env 1 `shouldReturn` fst before
            -- clearing the pin that equals the default changes nothing either
            luaText ls "return tostring(select(2, __physiology.clearMode(1, 'sleep')))"
                `shouldReturn` "false"
            override `shouldReturn` "nil"
            hooks `shouldReturn` "0"
            -- an effective change fires the hook once, in both directions
            setModeText ls "sleep" "nominal" "" `shouldReturn` "true|true"
            hooks `shouldReturn` "1"
            override `shouldReturn` "nominal"
            luaText ls "return tostring(select(2, __physiology.clearMode(1, 'sleep')))"
                `shouldReturn` "true"
            hooks `shouldReturn` "2"
            luaNum ls "unit.readPhysio(1, 'sleep_pressure')" >>= near 1e-6 10
            -- the other system's change is not this hook's event
            setModeText ls "exhaustion" "nominal" "" `shouldReturn` "true|true"
            hooks `shouldReturn` "2"

-----------------------------------------------------------------------
-- Contributions are EFFECTIVE deltas
-----------------------------------------------------------------------

-- | A facade contribution (@credit@ \/ @debit@) to the unit's key, as the
--   amount it reports applied.
contribute ∷ LuaBackendState → Text → Text → Double → IO Double
contribute ls fn key amount = luaNum ls $
    "__physiology." <> fn <> "(1, '" <> key <> "', " <> tshow amount <> ")"

effectiveOf ∷ LuaBackendState → Text → IO Double
effectiveOf ls key = luaNum ls ("unit.readPhysio(1, '" <> key <> "')")

contributionSpec ∷ Spec
contributionSpec = describe "contributions to a Simulated pool are EFFECTIVE deltas under \
                            \its retained modifiers" $ do
    forM_ systemKeys $ \(sid, key) →
        forM_ [("+50 %", 0.5), ("-50 %", -0.5), ("none", 0)] $ \(label, pct) →
            it (T.unpack sid <> " with a " <> label <> " modifier on "
                <> T.unpack key <> ": a debit and a credit move the effective \
                \value by exactly their amount, clamped to [0, maximum], and \
                \the modifier is kept") $
                withScene [modded key pct] $ \(env, ls) → do
                    setModeText ls sid "simulated" "" `shouldReturn` "true|true"
                    effectiveOf ls key >>= near 1e-4 10
                    contribute ls "debit" key 1 >>= near 1e-4 1
                    effectiveOf ls key >>= near 1e-4 9
                    contribute ls "credit" key 0.5 >>= near 1e-4 0.5
                    effectiveOf ls key >>= near 1e-4 9.5
                    -- more than the room left: applied is what landed
                    contribute ls "credit" key 5 >>= near 1e-4 0.5
                    effectiveOf ls key >>= near 1e-4 10
                    -- more than the pool holds: floored at zero
                    contribute ls "debit" key 50 >>= near 1e-4 10
                    effectiveOf ls key >>= near 1e-4 0
                    i ← readInstance env 1
                    HM.size (uiModifiers i) `shouldBe` 1

    it "credits an effective 5 under -50 % to 6, not 3" $
        withScene [modded "exhaustion" (-0.5)] $ \(_, ls) → do
            setModeText ls "exhaustion" "simulated" "{ exhaustion = 5 }"
                `shouldReturn` "true|true"
            effectiveOf ls "exhaustion" >>= near 1e-4 5
            contribute ls "credit" "exhaustion" 1 >>= near 1e-4 1
            effectiveOf ls "exhaustion" >>= near 1e-4 6

    it "honours a flat modifier and ignores an expired one, by the engine's \
       \activity rule" $
        withScene [modded "sleep_pressure" 0] $ \(env, ls) → do
            setModeText ls "sleep" "simulated" "{ sleep_pressure = 3 }"
                `shouldReturn` "true|true"
            -- a permanent flat +2 and a +50 % that lasts ten game seconds
            _ ← luaText ls "unit.addModifier(1, 'sleep_pressure', 2, 'flat', nil, 0); \
                           \unit.addModifier(1, 'sleep_pressure', 0, 'temp', 10, 0.5); \
                           \return true"
            effectiveOf ls "sleep_pressure" >>= near 1e-4 ((3 + 2) * 1.5)
            -- a debit of 3 lands exactly: base = 4.5 / 1.5 - 2 = 1
            contribute ls "debit" "sleep_pressure" 3 >>= near 1e-4 3
            effectiveOf ls "sleep_pressure" >>= near 1e-4 4.5
            luaNum ls "unit.getStatBase(1, 'sleep_pressure')" >>= near 1e-4 1
            -- past the temporary modifier's expiry only the flat +2 is active:
            -- the pool reads 1 + 2 = 3, and a debit of 1 solves base 0
            writeGameTime env 20
            effectiveOf ls "sleep_pressure" >>= near 1e-4 3
            contribute ls "debit" "sleep_pressure" 1 >>= near 1e-4 1
            effectiveOf ls "sleep_pressure" >>= near 1e-4 2
            luaNum ls "unit.getStatBase(1, 'sleep_pressure')" >>= near 1e-4 0

    it "writes nothing when no representable base exists (a multiplier that is \
       \not positive), and reports no change" $
        withScene [modded "exhaustion" 0] $ \(env, ls) → do
            setModeText ls "exhaustion" "simulated" "" `shouldReturn` "true|true"
            _ ← luaText ls "unit.addModifier(1, 'exhaustion', 0, 'void', nil, -1.0); \
                           \return true"
            before ← snapshot env 1
            contribute ls "debit" "exhaustion" 1 >>= near 1e-9 0
            contribute ls "credit" "exhaustion" 1 >>= near 1e-9 0
            snapshot env 1 `shouldReturn` before

-----------------------------------------------------------------------
-- Transient resets
-----------------------------------------------------------------------

resetSpec ∷ Spec
resetSpec = describe "carry remainders and alert debounce reset only for what changed" $
    it "resets the changed system's pool on a mode change or a value write, \
       \and nothing for an unchanged pin or another system's change" $
        withScene [simulated] $ \(_, ls) → do
            _ ← luaText ls $ T.unlines
                [ "_G.__forgot = {}"
                , "local carry = require('scripts.unit_resource_carry')"
                , "local alerts = require('scripts.unit_resource_alerts')"
                , "local cf, af = carry.forget, alerts.forget"
                , "carry.forget = function(uid, key)"
                , "  __forgot[#__forgot + 1] = 'c:' .. tostring(key); return cf(uid, key) end"
                , "alerts.forget = function(uid, key)"
                , "  __forgot[#__forgot + 1] = 'a:' .. tostring(key); return af(uid, key) end"
                , "return true" ]
            let forgot = luaText ls
                    "local o = {}; for _, k in ipairs(__forgot) do \
                    \if k:find('exhaustion') or k:find('sleep_pressure') then \
                    \o[#o + 1] = k end end; \
                    \__forgot = {}; table.sort(o); return table.concat(o, ',')"
            -- an unchanged pin resets nothing
            _ ← setModeText ls "sleep" "simulated" ""
            forgot `shouldReturn` ""
            -- a value write (no mode change) resets that pool's remainder
            -- and debounce only
            _ ← setModeText ls "sleep" "simulated" "{ sleep_pressure = 4 }"
            forgot `shouldReturn` "a:sleep_pressure,c:sleep_pressure"
            -- an effective change resets the changed system's pool only
            _ ← setModeText ls "exhaustion" "nominal" ""
            forgot `shouldReturn` "a:exhaustion,c:exhaustion"
            _ ← setModeText ls "sleep" "nominal" ""
            forgot `shouldReturn` "a:sleep_pressure,c:sleep_pressure"

-----------------------------------------------------------------------
-- Persistence (I6)
-----------------------------------------------------------------------

persistenceSpec ∷ Spec
persistenceSpec = describe "persistence through the real codec and load gate" $ do
    it "round-trips partial durable pools and a pinned Simulated squirrel \
       \exactly, with no normalization, and a second pass is a no-op" $
        withShipped $ \(env, _) → do
            reg ← readShippedPhysiologyRegistry
            defs ← shippedDefs env
            placeUnits env
                [ (1, "acolyte", Nothing, []
                  , [("exhaustion", 3.5), ("sleep_pressure", 2.25)])
                , (2, "red_squirrel", Nothing, [("sleep", PhysioSimulated)]
                  , [("sleep_pressure", 4.5)]) ]
            onSavePage env [1, 2]
            before1 ← readInstance env 1
            before2 ← readInstance env 2
            pg ← pageSnapshotOf env ≫= codecRoundTrip
            (us, notes) ← gate reg defs (pgsUnits pg)
            notes `shouldBe` []
            let unitOf i = HM.lookup (UnitId i) (usnInstances us)
            case (unitOf 1, unitOf 2) of
                (Just a, Just s) → do
                    HM.lookup "exhaustion" (uisStats a) `shouldBe` Just 3.5
                    HM.lookup "sleep_pressure" (uisStats a) `shouldBe` Just 2.25
                    uisStats a `shouldBe` uiStats before1
                    HM.lookup "sleep_pressure" (uisStats s) `shouldBe` Just 4.5
                    uisPhysioOverrides s
                        `shouldBe` HM.fromList [("sleep", PhysioSimulated)]
                    uisStats s `shouldBe` uiStats before2
                _ → expectationFailure "a unit vanished"
            (us', notes') ← gate reg defs us
            (us', notes') `shouldBe` (us, [])

    it "normalizes a unit saved under another profile once: owned keys of a \
       \Nominal system are scrubbed, logged once, and a re-save and reload \
       \performs zero further repairs" $
        withShipped $ \(env, _) → do
            reg ← readShippedPhysiologyRegistry
            defs ← shippedDefs env
            rich ← profilePhysiology "rich"
            -- saved while exhaustion and sleep were Simulated; the shipped
            -- technomule resolves both Nominal
            placeUnits env [(1, "technomule", Just rich, [], [])]
            onSavePage env [1]
            before ← readInstance env 1
            forM_ ["exhaustion", "sleep_pressure"] $ \k →
                HM.member k (uiStats before) `shouldBe` True
            pg ← pageSnapshotOf env ≫= codecRoundTrip
            (us1, notes1) ← gate reg defs (pgsUnits pg)
            length notes1 `shouldBe` 1
            case HM.lookup (UnitId 1) (usnInstances us1) of
                Nothing → expectationFailure "the unit vanished"
                Just u → do
                    forM_ (nonStoredKeys reg "exhaustion" ⧺ nonStoredKeys reg "sleep") $
                        \k → HM.member k (uisStats u) `shouldBe` False
                    uisPhysioOverrides u `shouldBe` HM.empty
            -- the repair note names the keys it removed
            let named = [ n | PhysiologyNormalization _ _ _ ns ← notes1, n ← ns ]
            forM_ ["exhaustion", "sleep_pressure"] $ \k →
                (k, any (T.isInfixOf k) named) `shouldBe` (k, True)
            (us2, notes2) ← gate reg defs us1
            (us2, notes2) `shouldBe` (us1, [])
            back ← codecRoundTrip pg { pgsUnits = us2 }
            (us3, notes3) ← gate reg defs (pgsUnits back)
            (us3, notes3) `shouldBe` (us2, [])
