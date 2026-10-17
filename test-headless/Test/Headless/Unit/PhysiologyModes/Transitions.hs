{-# LANGUAGE Strict, OverloadedStrings #-}
-- | Mode transitions and the write boundary of the "physiology stat modes"
--   group (#2831; design D-3, D-5, D-5a, D-7; I5, T-5 to T-8, T-10): the
--   engine verbs behind @physiology.setMode@ \/ @clearMode@, the seeding
--   and scrub rules, the atomic refusals, the raw-write gate, the change
--   hooks, derived freshness and the bodyless (Unsupported) case.
--
--   Everything runs the REAL @unit.*@ verbs and the shipped
--   @scripts\/physiology.lua@ over a live headless engine. A refusal is
--   asserted by reading the unit back ('snapshot'), never by trusting the
--   verb's return value.
module Test.Headless.Unit.PhysiologyModes.Transitions (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Engine.Core.State (EngineEnv)
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Unit.PhysiologyFixture
    (exemptPhysiology, profilePhysiology, richPhysiology)
import Test.Headless.Unit.PhysiologyModes.Harness
import Unit.Command.Types (UnitCommand(..))
import Unit.Types

-- | The 0.3 kg squirrel of the @reduced@ profile: hydration and nutrition
--   Nominal, salt and thermo Simulated.
squirrel ∷ UnitSpec
squirrel = (unitSpec 1 "red_squirrel" (Just "reduced")) { usBody = squirrelBody }

-- | The squirrel under @simple@: only thermo Simulated.
simpleSquirrel ∷ UnitSpec
simpleSquirrel = squirrel { usProfile = Just "simple" }

-- | The all-Nominal unit (@exempt@) at the reference mass.
nomad ∷ UnitSpec
nomad = unitSpec 1 "nomad_primitive" (Just "exempt")

-- | A bodyless definition: every system Unsupported.
bodyless ∷ UnitSpec
bodyless = unitSpec 1 "white_tailed_deer" Nothing

-- | The reference acolyte, every system Simulated.
acolyte ∷ UnitSpec
acolyte = unitSpec 1 "acolyte" (Just "rich")

-- | Everything a unit stores for physiology, plus its override map, as the
--   engine holds it right now.
snapshot ∷ EngineEnv → Int → IO ([(Text, Float)], [(Text, PhysioMode)])
snapshot env uid = do
    i ← readInstance env uid
    pure ( [ (k, v) | k ← nominalKeys, Just v ← [HM.lookup k (uiStats i)] ]
         , HM.toList (uiPhysioOverrides i) )

-- | @setMode@ through the facade, as @ok|changed@ or @nil|reason@.
setMode ∷ LuaBackendState → Int → Text → Text → IO Text
setMode ls uid sys mode = luaText ls $
    "local ok, ch = __physiology.setMode(" <> tshow uid <> ", '" <> sys
    <> "', '" <> mode <> "'); return tostring(ok) .. '|' .. tostring(ch)"

-- | @setMode@ with a Lua values table literal.
setModeWith ∷ LuaBackendState → Int → Text → Text → Text → IO Text
setModeWith ls uid sys mode values = luaText ls $
    "local ok, ch = __physiology.setMode(" <> tshow uid <> ", '" <> sys
    <> "', '" <> mode <> "', " <> values
    <> "); return tostring(ok) .. '|' .. tostring(ch)"

clearMode ∷ LuaBackendState → Int → Text → IO Text
clearMode ls uid sys = luaText ls $
    "local ok, ch = __physiology.clearMode(" <> tshow uid <> ", '" <> sys
    <> "'); return tostring(ok) .. '|' .. tostring(ch)"

-- | A refusal: the verb answers nil and the unit is exactly as it was.
refuses ∷ EngineEnv → LuaBackendState → Int → IO Text → Expectation
refuses env _ uid attempt = do
    before ← snapshot env uid
    out ← attempt
    ("nil|" `T.isPrefixOf` out) `shouldBe` True
    after ← snapshot env uid
    after `shouldBe` before

spec ∷ Spec
spec = describe "transitions and the write boundary" $ do
    it "keeps the pure profile fixtures equal to the shipped profiles' expansions" $ do
        profilePhysiology "rich" `shouldReturn` richPhysiology
        profilePhysiology "exempt" `shouldReturn` exemptPhysiology
    seeding
    overrides
    refusals
    modifiers
    writeBoundary
    hooks
    derivedFreshness
    contributions
    unsupported

-----------------------------------------------------------------------
-- Seeding and scrubbing (D-5)
-----------------------------------------------------------------------

seeding ∷ Spec
seeding = describe "seeding and scrubbing" $ do

    it "gives a unit exactly its Simulated systems' keys at the start" $
        withScene [squirrel] $ \(env, _) → do
            (keys, ovr) ← snapshot env 1
            map fst keys `shouldBe` ["core_temp", "salt", "salt_store"]
            ovr `shouldBe` []

    it "seeds hydration to its effective maximum and removes it on the way back" $
        withScene [squirrel] $ \(env, ls) → do
            setMode ls 1 "hydration" "simulated" `shouldReturn` "true|true"
            (keys, _) ← snapshot env 1
            lookup "hydration" keys `shouldBe` Just 0.18       -- 0.6 L/kg
            readOwned ls 1 "hydration" `shouldReturn` "0.18|simulated"
            setMode ls 1 "hydration" "nominal" `shouldReturn` "true|true"
            (keys', _) ← snapshot env 1
            lookup "hydration" keys' `shouldBe` Nothing
            readOwned ls 1 "hydration" `shouldReturn` "0.18|nominal"

    it "seeds the stomach and the calorie store to their effective maxima" $
        withScene [squirrel] $ \(env, ls) → do
            setMode ls 1 "nutrition" "simulated" `shouldReturn` "true|true"
            (keys, _) ← snapshot env 1
            lookup "hunger" keys `shouldBe` Just 3             -- 10 kcal/kg
            lookup "calories" keys `shouldBe` Just 6           -- 20 kcal/kg

    it "seeds salt to the pool its body water supports, so the concentration reads 1.0" $
        withScene [simpleSquirrel] $ \(env, ls) → do
            -- body water at half: hydration Simulated, set by value
            _ ← setModeWith ls 1 "hydration" "simulated" "{ hydration = 0.09 }"
            setMode ls 1 "salt" "simulated" `shouldReturn` "true|true"
            (keys, _) ← snapshot env 1
            lookup "salt" keys `shouldBe` Just 0.15           -- 0.5 x maxSalt 0.3
            lookup "salt_store" keys `shouldBe` Just 0.6       -- storeCap 2 x maxSalt
            readOwned ls 1 "salt_conc" `shouldReturn` "1|simulated"

    it "seeds core temperature at 37 and its meters at 0, and scrubs all three on leaving" $
        withScene [nomad] $ \(env, ls) → do
            setMode ls 1 "thermo" "simulated" `shouldReturn` "true|true"
            (keys, _) ← snapshot env 1
            lookup "core_temp" keys `shouldBe` Just 37
            lookup "hypothermia" keys `shouldBe` Just 0
            lookup "hyperthermia" keys `shouldBe` Just 0
            setMode ls 1 "thermo" "nominal" `shouldReturn` "true|true"
            (keys', _) ← snapshot env 1
            keys' `shouldBe` []

    it "writes the caller's EFFECTIVE targets when values are given" $
        withScene [squirrel] $ \(env, ls) → do
            setModeWith ls 1 "hydration" "simulated" "{ hydration = 0.045 }"
                `shouldReturn` "true|true"
            (keys, _) ← snapshot env 1
            lookup "hydration" keys `shouldBe` Just 0.045
            -- an in-mode value write changes the value and nothing else
            setModeWith ls 1 "hydration" "simulated" "{ hydration = 0.09 }"
                `shouldReturn` "true|false"
            (keys', _) ← snapshot env 1
            lookup "hydration" keys' `shouldBe` Just 0.09

-----------------------------------------------------------------------
-- Override intent (D-3)
-----------------------------------------------------------------------

overrides ∷ Spec
overrides = describe "override intent" $ do

    it "records a set equal to the species default as intent, with no effective change" $
        withScene [squirrel] $ \(env, ls) → do
            before ← snapshot env 1
            setMode ls 1 "hydration" "nominal" `shouldReturn` "true|false"
            (keys, ovr) ← snapshot env 1
            keys `shouldBe` fst before
            ovr `shouldBe` [("hydration", PhysioNominal)]
            luaText ls "return unit.getPhysioOverride(1, 'hydration')"
                `shouldReturn` "nominal"

    it "removes the override on every clear, a clear that changes nothing included" $
        withScene [squirrel] $ \(env, ls) → do
            _ ← setMode ls 1 "hydration" "nominal"
            clearMode ls 1 "hydration" `shouldReturn` "true|false"
            (_, ovr) ← snapshot env 1
            ovr `shouldBe` []
            -- clearing a unit that never had one is a no-op success
            clearMode ls 1 "hydration" `shouldReturn` "true|false"

    it "follows the species default again after a clear, seeding or scrubbing as needed" $
        withScene [squirrel] $ \(env, ls) → do
            _ ← setMode ls 1 "hydration" "simulated"
            (keys, _) ← snapshot env 1
            lookup "hydration" keys `shouldSatisfy` isJust
            clearMode ls 1 "hydration" `shouldReturn` "true|true"
            (keys', ovr) ← snapshot env 1
            lookup "hydration" keys' `shouldBe` Nothing
            ovr `shouldBe` []

    it "lets an override beat the species default in BOTH directions" $
        withScene [squirrel] $ \(env, ls) → do
            -- salt defaults Simulated under reduced; override it Nominal
            setMode ls 1 "salt" "nominal" `shouldReturn` "true|true"
            (keys, _) ← snapshot env 1
            lookup "salt" keys `shouldBe` Nothing
            luaText ls "return unit.getPhysioMode(1, 'salt')" `shouldReturn` "nominal"
            clearMode ls 1 "salt" `shouldReturn` "true|true"
            luaText ls "return unit.getPhysioMode(1, 'salt')" `shouldReturn` "simulated"

-----------------------------------------------------------------------
-- Atomic refusals (D-5)
-----------------------------------------------------------------------

refusals ∷ Spec
refusals = describe "atomic refusals" $ do

    it "refuses an unknown system, an unknown mode and a missing unit, changing nothing" $
        withScene [squirrel] $ \(env, ls) → do
            refuses env ls 1 (setMode ls 1 "plumbing" "simulated")
            refuses env ls 1 (luaText ls
                "local ok, why = unit.setPhysioMode(1, 'hydration', 'sideways'); \
                \return tostring(ok) .. '|' .. tostring(why)")
            refuses env ls 1 (luaText ls
                "local ok, why = unit.setPhysioMode(99, 'hydration', 'simulated'); \
                \return tostring(ok) .. '|' .. tostring(why)")

    it "refuses a DEAD unit" $
        withScene [squirrel { usPose = "dead" }] $ \(env, ls) →
            refuses env ls 1 (setMode ls 1 "hydration" "simulated")

    it "refuses a system the definition does not support, on set and on clear" $
        withScene [bodyless] $ \(env, ls) → do
            refuses env ls 1 (setMode ls 1 "hydration" "simulated")
            refuses env ls 1 (setMode ls 1 "hydration" "nominal")
            refuses env ls 1 (clearMode ls 1 "hydration")
            luaText ls "return tostring(unit.getPhysioMode(1, 'hydration'))"
                `shouldReturn` "nil"

    it "refuses values that are not a valid target, leaving mode, override and keys exactly as they were" $
        withScene [squirrel] $ \(env, ls) → forM_
            [ "{ hydration = 0.19 }"            -- above the maximum
            , "{ hydration = -0.01 }"           -- negative
            , "{ hydration = 0/0 }"             -- not a number
            , "{ hydration = math.huge }"       -- infinite
            , "{ hydration = '0.09' }"          -- a numeric string is not a number
            , "{ salt = 0.1 }"                  -- a key the system does not own
            , "{ hypothermia = 0 }"             -- a meter
            , "{ salt_conc = 1 }" ] $ \values →  -- a derived key
            refuses env ls 1 (setModeWith ls 1 "hydration" "simulated" values)

    it "refuses values for a system that stays Nominal, and a values argument that is not a table" $
        withScene [squirrel] $ \(env, ls) → do
            refuses env ls 1 (setModeWith ls 1 "hydration" "nominal" "{ hydration = 0.1 }")
            refuses env ls 1 (setModeWith ls 1 "hydration" "simulated" "'0.1'")

    it "refuses Simulated when the species has no resource parameters and the registry has none either" $
        withScene [squirrel] $ \(env, ls) → do
            _ ← luaText ls
                "local p = require('scripts.unit_resource_params'); \
                \_G.__saved = p.DEFAULTS.hydration; p.DEFAULTS.hydration = nil; \
                \return true"
            refuses env ls 1 (setMode ls 1 "hydration" "simulated")
            why ← luaText ls
                "local ok, why = __physiology.setMode(1, 'hydration', 'simulated'); \
                \return tostring(why)"
            ("no resource parameters" `T.isInfixOf` why) `shouldBe` True
            _ ← luaText ls
                "require('scripts.unit_resource_params').DEFAULTS.hydration = \
                \__saved; return true"
            -- and with the default restored the same call succeeds
            setMode ls 1 "hydration" "simulated" `shouldReturn` "true|true"

-----------------------------------------------------------------------
-- Modifiers (T-6)
-----------------------------------------------------------------------

modifiers ∷ Spec
modifiers = describe "modifiers" $ do

    it "applies a modifier on a Simulated pool to its read and ignores it under Nominal" $
        withScene [ acolyte { usId = 1, usStats = [("hydration", 42)]
                            , usModifiers = [("hydration", [StatModifier 0 "t" Nothing 0.5])] }
                  , nomad { usId = 2
                          , usModifiers = [("hydration", [StatModifier (-10) "t" Nothing 0])] } ] $
            \(_, ls) → do
                -- Simulated: base 42 x 1.5
                readOwned ls 1 "hydration" `shouldReturn` "63|simulated"
                -- Nominal: the healthy value, the modifier on the key ignored
                readOwned ls 2 "hydration" `shouldReturn` "42|nominal"

    it "moves the Nominal healthy value when the modifier is on its INPUT" $
        withScene [ nomad { usModifiers = [("max_hydration", [StatModifier 8 "t" Nothing 0])] } ] $
            \(_, ls) → readOwned ls 1 "hydration" `shouldReturn` "50|nominal"

    it "seeds a retained percent modifier so the EFFECTIVE salt is the target and the concentration reads 1.0" $
        withScene [ simpleSquirrel
                      { usModifiers = [("salt", [StatModifier 0 "t" Nothing 0.5])] } ] $
            \(env, ls) → do
                _ ← setModeWith ls 1 "hydration" "simulated" "{ hydration = 0.09 }"
                setMode ls 1 "salt" "simulated" `shouldReturn` "true|true"
                (keys, _) ← snapshot env 1
                -- effective 0.5 x maxSalt 0.3 = 0.15, so the BASE is 0.15 / 1.5
                maybe 0 realToFrac (lookup "salt" keys) `near'` (0.15 / 1.5)
                readOwned ls 1 "salt" `shouldReturn` "0.15|simulated"
                readOwned ls 1 "salt_conc" `shouldReturn` "1|simulated"

    it "refuses the whole transition when the multiplier leaves no representable base" $
        withScene [ simpleSquirrel
                      { usModifiers = [("salt", [StatModifier 0 "t" Nothing (-1.0)])] } ] $
            \(env, ls) → do
                refuses env ls 1 (setMode ls 1 "salt" "simulated")
                luaText ls "return tostring(unit.getPhysioOverride(1, 'salt'))"
                    `shouldReturn` "nil"

    it "pushes a caller's values target through the same inversion" $
        withScene [ simpleSquirrel
                      { usModifiers = [("hydration", [StatModifier 0.02 "t" Nothing 1.0])] } ] $
            \(env, ls) → do
                -- effective (b + 0.02) x 2 = 0.1  =>  b = 0.03
                setModeWith ls 1 "hydration" "simulated" "{ hydration = 0.1 }"
                    `shouldReturn` "true|true"
                (keys, _) ← snapshot env 1
                maybe 0 realToFrac (lookup "hydration" keys) `near'` 0.03
                readOwned ls 1 "hydration" `shouldReturn` "0.1|simulated"
                -- a target that needs a negative base is refused
                refuses env ls 1 (setModeWith ls 1 "hydration" "simulated" "{ hydration = 0.01 }")
  where
    near' ∷ Double → Double → Expectation
    near' got want = near 1e-6 want got

-----------------------------------------------------------------------
-- The write boundary (D-7, T-7)
-----------------------------------------------------------------------

writeBoundary ∷ Spec
writeBoundary = describe "the raw write boundary" $ do

    it "refuses a raw write of every owned, derived and meter key of a Nominal system, leaving them absent" $
        withScene [nomad] $ \(env, ls) → do
            forM_ nominalKeys $ \k →
                luaText ls ("return tostring(unit.setStat(1, '" <> k <> "', 5))")
                    `shouldReturn` "false"
            (keys, _) ← snapshot env 1
            keys `shouldBe` []

    it "keeps every Nominal key absent to every read verb" $
        withScene [nomad] $ \(_, ls) → do
            forM_ nominalKeys $ \k → do
                luaText ls ("return tostring(unit.getStat(1, '" <> k <> "'))")
                    `shouldReturn` "nil"
                luaText ls ("return tostring(unit.getStatBase(1, '" <> k <> "'))")
                    `shouldReturn` "nil"
            luaText ls
                "local n = 0; for k in pairs(unit.getAllStats(1)) do \
                \for _, held in ipairs({ 'core_temp', 'salt', 'salt_store', \
                \'salt_conc', 'hydration', 'hunger', 'calories', 'hypothermia', \
                \'hyperthermia', 'salt_imbalance' }) do \
                \if k == held then n = n + 1 end end end; return n"
                `shouldReturn` "0"

    it "refuses a derived key in BOTH modes and allows a Simulated key's debug write" $
        withScene [acolyte] $ \(env, ls) → do
            luaText ls "return tostring(unit.setStat(1, 'salt_conc', 1))"
                `shouldReturn` "false"
            luaText ls "return tostring(unit.setStat(1, 'salt', 5))" `shouldReturn` "true"
            (keys, _) ← snapshot env 1
            lookup "salt" keys `shouldBe` Just 5
            lookup "salt_conc" keys `shouldBe` Nothing

    it "refuses a raw write of an Unsupported system's keys" $
        withScene [bodyless] $ \(env, ls) → do
            forM_ nominalKeys $ \k →
                luaText ls ("return tostring(unit.setStat(1, '" <> k <> "', 5))")
                    `shouldReturn` "false"
            (keys, _) ← snapshot env 1
            keys `shouldBe` []

    it "leaves the unowned keys writable in every mode" $
        withScene [nomad] $ \(_, ls) → do
            luaText ls "return tostring(unit.setStat(1, 'caffeine', 0.3))"
                `shouldReturn` "true"
            luaText ls "return tostring(unit.setStat(1, 'tissue_recovering', 1))"
                `shouldReturn` "true"

-----------------------------------------------------------------------
-- Hooks, latches, revive and debounce (D-5)
-----------------------------------------------------------------------

hooks ∷ Spec
hooks = describe "change hooks and transient state" $ do

    it "runs the change hooks on an effective change only" $
        withScene [squirrel] $ \(_, ls) → do
            _ ← luaText ls
                "_G.__calls = {}; __physiology.onModeChanged('probe', 'hydration', \
                \function(uid, sys, from, to) \
                \__calls[#__calls + 1] = sys .. ':' .. from .. '>' .. to end); \
                \return true"
            _ ← setMode ls 1 "hydration" "nominal"        -- equal to the default
            _ ← setMode ls 1 "hydration" "simulated"      -- effective change
            _ ← setModeWith ls 1 "hydration" "simulated" "{ hydration = 0.1 }"  -- value write
            _ ← setMode ls 1 "salt" "nominal"             -- another system
            luaText ls "return table.concat(__calls, ',')"
                `shouldReturn` "hydration:nominal>simulated"

    it "cancels a latched source-drinking sequence when hydration changes mode" $
        withScene [acolyte] $ \(_, ls) → do
            out ← luaText ls
                "local ai = require('scripts.unit_ai'); \
                \require('scripts.unit_ai_source_phase'); \
                \local s = { sourcePhase = 'drinking', sourcePhaseAt = 0 }; \
                \ai.getState = function() return s end; \
                \__physiology.setMode(1, 'hydration', 'nominal'); \
                \return tostring(s.sourcePhase)"
            out `shouldBe` "nil"

    it "stands a unit back up that only a Simulated pool was holding down, once the pool is Nominal" $
        withScene [acolyte { usPose = "collapsed", usStats = [("hydration", 4)]
                           , usModes = [("salt", PhysioNominal)] }] $ \(env, ls) → do
            -- Simulated hydration at ~10 % of max: below the 0.5 revive gate
            _ ← killed env
            runSeconds ls 1 0.1
            cmds ← collapsedOrRevived env
            cmds `shouldNotContain` ["revive"]
            setMode ls 1 "hydration" "nominal" `shouldReturn` "true|true"
            _ ← killed env
            runSeconds ls 1 0.1
            collapsedOrRevived env >>= (`shouldContain` ["revive"])

    it "leaves the durable brain integrators alone when it refreshes the caches, whatever modifiers they carry" $
        withScene [acolyte { usStats = [ ("mood", 0.5), ("emotional_pain", 0.2), ("caffeine", 0.1) ]
                           , usModifiers = [ ("mood", [StatModifier 0.1 "t" Nothing 0])
                                           , ("emotional_pain", [StatModifier 0.05 "t" Nothing 0])
                                           , ("caffeine", [StatModifier 0.2 "t" Nothing 0]) ] }] $
            \(_, ls) → do
                let bases = luaText ls
                        "return string.format('%.6f %.6f %.6f', unit.getStatBase(1, 'mood'), \
                        \unit.getStatBase(1, 'emotional_pain'), unit.getStatBase(1, 'caffeine'))"
                before ← bases
                _ ← setMode ls 1 "thermo" "nominal"         -- a transition refreshes the caches
                _ ← setModeWith ls 1 "hydration" "simulated" "{ hydration = 30 }"  -- so does a value write
                _ ← luaText ls "__resources.onSaveLoaded({ 1 }, {}); return true"   -- and a load
                bases `shouldReturn` before
                -- the cache itself still reads the EFFECTIVE mood
                luaNum ls "unit.getStat(1, 'state_of_mind')" >>= \v →
                    v `shouldSatisfy` (< 0.7)

    it "rebuilds state_of_mind even for a unit that has never ticked (no mood yet), without storing a mood" $ do
        -- through a value write ...
        withScene [acolyte] $ \(_, ls) → do
            luaText ls "return tostring(unit.getStat(1, 'mood'))" `shouldReturn` "nil"
            _ ← setModeWith ls 1 "thermo" "simulated" "{ core_temp = 41 }"
            luaNum ls "unit.getStat(1, 'state_of_mind')" >>= \v → v `shouldSatisfy` (< 0.6)
            luaText ls "return tostring(unit.getStat(1, 'mood'))" `shouldReturn` "nil"
        -- ... and through a load of a stale cache
        withScene [acolyte { usStats = [ ("core_temp", 41), ("state_of_mind", 1.0)
                                       , ("consciousness", 1.0) ] }] $ \(_, ls) → do
            _ ← luaText ls "__resources.onSaveLoaded({ 1 }, {}); return true"
            luaNum ls "unit.getStat(1, 'state_of_mind')" >>= \v → v `shouldSatisfy` (< 0.6)
            luaText ls "return tostring(unit.getStat(1, 'mood'))" `shouldReturn` "nil"

    it "treats an EMPTY values table as no value write: nothing is reset" $ do
        let finalLean calls = withScene [acolyte { usModes = [("salt", PhysioNominal)] }] $
                \(_, ls) → do
                    _ ← luaText ls $
                        "for _ = 1, 600 do __resources.update(0.1); " <> calls <> " end; return true"
                    luaNum ls "unit.getStatBase(1, 'lean_mass')"
        control ← finalLean ""
        noop ← finalLean "__physiology.setMode(1, 'nutrition', 'simulated', {})"
        noop `shouldBe` control

    it "leaves another system's carry remainder alone, and an unchanged pin resets nothing" $ do
        let finalHydration pins = withScene [acolyte { usModes = [("salt", PhysioNominal)] }] $
                \(_, ls) → do
                    _ ← luaText ls $
                        "for _ = 1, 600 do __resources.update(0.1); " <> pins <> " end; return true"
                    luaNum ls "unit.getStatBase(1, 'hydration')"
        control ← finalHydration ""
        -- pinning salt Nominal while it already is Nominal changes nothing
        pinned ← finalHydration "__physiology.setMode(1, 'salt', 'nominal')"
        -- clearing an override that is not there changes nothing either
        cleared ← finalHydration "__physiology.clearMode(1, 'salt')"
        pinned `shouldBe` control
        cleared `shouldBe` control
        -- an effective change of ANOTHER system leaves hydration's remainder alone
        saltFlip ← finalHydration
            "if _ % 100 == 0 then __physiology.setMode(1, 'salt', 'simulated'); \
            \__physiology.setMode(1, 'salt', 'nominal') end"
        saltFlip `shouldBe` control

    it "does not re-arm an alert on an unchanged pin" $
        withScene [acolyte { usStats = [("hydration", 4)]
                           , usModes = [("salt", PhysioNominal)] }] $ \(_, ls) → do
            _ ← luaText ls
                "_G.__warnings = 0; local emit = engine.emitEventForUnit; \
                \engine.emitEventForUnit = function(kind, msg, ...) \
                \if kind == 'survival_warning' then __warnings = __warnings + 1 end \
                \return emit(kind, msg, ...) end; return true"
            runSeconds ls 1 0.1
            luaText ls "return __warnings" `shouldReturn` "1"
            _ ← setMode ls 1 "salt" "nominal"               -- unchanged
            _ ← setMode ls 1 "hydration" "simulated"        -- unchanged
            runSeconds ls 1 0.1
            luaText ls "return __warnings" `shouldReturn` "1"

    it "re-arms the dehydration alert after a value write, because the debounce state is cleared" $
        withScene [acolyte { usStats = [("hydration", 4)]
                           , usModes = [("salt", PhysioNominal)] }] $ \(_, ls) → do
            _ ← luaText ls
                "_G.__warnings = 0; local emit = engine.emitEventForUnit; \
                \engine.emitEventForUnit = function(kind, msg, ...) \
                \if kind == 'survival_warning' then __warnings = __warnings + 1 end \
                \return emit(kind, msg, ...) end; return true"
            runSeconds ls 1 0.1
            luaText ls "return __warnings" `shouldReturn` "1"
            runSeconds ls 1 0.1
            luaText ls "return __warnings" `shouldReturn` "1"       -- debounced
            _ ← setModeWith ls 1 "hydration" "simulated" "{ hydration = 4 }"
            runSeconds ls 1 0.1
            luaText ls "return __warnings" `shouldReturn` "2"

-- | The commands a tick queued, as @revive@ \/ @collapse@ \/ @kill@ tags.
collapsedOrRevived ∷ EngineEnv → IO [Text]
collapsedOrRevived env = do
    cmds ← drainCommands env
    pure [ tag c | c ← cmds, tag c ≢ "" ]
  where
    tag (UnitRevive _)   = "revive"
    tag (UnitCollapse _) = "collapse"
    tag (UnitKill _)     = "kill"
    tag _                = ""

-----------------------------------------------------------------------
-- Derived freshness (D-5a, T-8)
-----------------------------------------------------------------------

derivedFreshness ∷ Spec
derivedFreshness = describe "derived and cached reads" $ do

    it "reads salt_conc coherently, with no tick between, after each state change" $
        withScene [ squirrel ] $ \(env, ls) → do
            let conc = luaNum ls "unit.readPhysio(1, 'salt_conc')"
                formula = luaNum ls
                    "(unit.readPhysio(1, 'salt') / unit.getStat(1, 'body_mass')) \
                    \/ math.max(0.05, math.min(1, \
                    \(unit.readPhysio(1, 'hydration') or unit.readPhysio(1, 'max_hydration')) \
                    \/ unit.getStat(1, 'max_hydration')))"
            -- hydration Nominal -> Simulated at half water with salt Simulated
            _ ← setModeWith ls 1 "hydration" "simulated" "{ hydration = 0.09 }"
            (conc >>= near 1e-5 2.0)
            -- a raw debugging write of the Simulated pool
            _ ← luaText ls "unit.setStat(1, 'salt', 0.06); return true"
            c1 ← conc
            f1 ← formula
            near 1e-5 f1 c1
            -- a contribution through the facade
            _ ← luaText ls "__physiology.debit(1, 'salt', 0.01); return true"
            c2 ← conc
            f2 ← formula
            near 1e-5 f2 c2
            c2 `shouldSatisfy` (< c1)
            -- the maximum salt follows body_mass at once ...
            _ ← luaText ls "unit.setStat(1, 'body_mass', 0.4); return true"
            c3 ← conc
            f3 ← formula
            near 1e-5 f3 c3
            c3 `shouldSatisfy` (< c2)
            -- ... and a body recompute that moves the water maximum too
            _ ← luaText ls "unit.recomputeBody(1); return true"
            c4 ← conc
            f4 ← formula
            near 1e-5 f4 c4
            c4 `shouldSatisfy` (> c3)
            ignore env

    it "reads a salt modifier's EXPIRY on the next read, with no tick and no setter" $
        withScene [ squirrel { usModifiers =
                      [("salt", [StatModifier 0 "t" (Just 10) 1.0])] } ] $ \(env, ls) → do
            -- the unit's salt base is the seed; an active +100% doubles it
            before ← luaNum ls "unit.readPhysio(1, 'salt_conc')"
            writeGameTime env 11
            after ← luaNum ls "unit.readPhysio(1, 'salt_conc')"
            near 1e-5 (before / 2) after

    it "reads a fresh circulation and refreshes consciousness before a transition returns" $
        withScene [ acolyte { usStats = [ ("core_temp", 31), ("consciousness", 0.25) ] } ] $ \(_, ls) → do
            -- the cold core (5 degrees under the vasoconstriction onset)
            -- drags the derived circulation to 0.7, with no tick
            luaNum ls "unit.readPhysio(1, 'circulation')" >>= \c →
                c `shouldSatisfy` (\x → abs (x - 0.7) < 1e-4)
            _ ← setMode ls 1 "thermo" "nominal"
            -- no tick has run: circulation (derived, never stored) reads the
            -- healthy core at once, and the brain cache is refreshed
            luaNum ls "unit.readPhysio(1, 'circulation')" >>= \c →
                c `shouldSatisfy` (> 0.9)
            luaNum ls "unit.getStat(1, 'consciousness')" >>= \c →
                c `shouldSatisfy` (> 0.99)
  where
    ignore _ = pure ()

-----------------------------------------------------------------------
-- The facade's contributions into a SIMULATED receiver
-----------------------------------------------------------------------

contributions ∷ Spec
contributions = describe "facade contributions into a Simulated system" $ do

    it "credits hydration clamped to its maximum and reports what it applied" $
        withScene [acolyte { usStats = [("hydration", 40)] }] $ \(env, ls) → do
            luaNum ls "__physiology.credit(1, 'hydration', 1000)" >>= near 1e-4 2
            (keys, _) ← snapshot env 1
            lookup "hydration" keys `shouldBe` Just 42

    it "debits salt and hydration floored at zero" $
        withScene [acolyte] $ \(env, ls) → do
            luaNum ls "__physiology.debit(1, 'salt', 1000)" >>= near 1e-3 70
            luaNum ls "__physiology.debit(1, 'hydration', 1000)" >>= near 1e-3 42
            (keys, _) ← snapshot env 1
            (lookup "salt" keys, lookup "hydration" keys) `shouldBe` (Just 0, Just 0)

    it "caps a meal's salt at the reserve capacity and discards the overflow" $
        withScene [acolyte { usStats = [("salt_store", 130)] }] $ \(env, ls) → do
            _ ← luaText ls "require('scripts.salts').mealSalt(1); \
                           \require('scripts.salts').mealSalt(1); return true"
            (keys, _) ← snapshot env 1
            lookup "salt_store" keys `shouldBe` Just 140

    it "answers nil, reason for a key with no contribution rule and writes nothing" $
        withScene [acolyte] $ \(env, ls) → do
            before ← snapshot env 1
            luaText ls "local r, why = __physiology.credit(1, 'core_temp', 1); \
                       \return tostring(r) .. '|' .. tostring(why)"
                `shouldReturn` "nil|no contribution rule for core_temp"
            snapshot env 1 `shouldReturn` before

-----------------------------------------------------------------------
-- Unsupported (bodyless) units
-----------------------------------------------------------------------

unsupported ∷ Spec
unsupported = describe "a bodyless unit (every system Unsupported)" $ do

    it "holds no physiology key, ticks without error or NaN, and reads its declared substitutes" $
        withScene [bodyless { usStats = [("height", 1.0)] }] $ \(env, ls) → do
            climate ls (-20) 0.5
            runSeconds ls 30 0.1
            scriptErrors ls `shouldReturn` ""
            (keys, ovr) ← snapshot env 1
            keys `shouldBe` []
            ovr `shouldBe` []
            -- the dependents read the Nominal substitutes: no NaN anywhere
            forM_ ["circulation", "consciousness", "heart_rate"] $ \k →
                luaText ls ("return __f(unit.getStat(1, '" <> k <> "'))")
                    >>= \v → (v `shouldNotBe` "nan")
            readOwned ls 1 "core_temp" `shouldReturn` "nil|nil"
            readOwned ls 1 "hydration" `shouldReturn` "nil|nil"
            luaText ls "return tostring(unit.getPhysioMode(1, 'thermo'))"
                `shouldReturn` "nil"
            killed env `shouldReturn` []

    it "gives the facade's contributions nothing to change" $
        withScene [bodyless] $ \(env, ls) → do
            luaText ls "return tostring(__physiology.credit(1, 'hydration', 5))"
                `shouldReturn` "0"
            luaText ls "return tostring(__physiology.debit(1, 'salt', 5))"
                `shouldReturn` "0"
            (keys, _) ← snapshot env 1
            keys `shouldBe` []
