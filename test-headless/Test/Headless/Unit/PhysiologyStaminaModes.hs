{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The "physiology stamina modes" gate (#2834, S8a of the physiology
--   stat-modes epic #2827): the acceptance of stamina as a registry system,
--   the effective stamina commit, the one maximum rule and the SD-20 combat
--   engagement, run through the SHIPPED Haskell (combat admission, 'Wear',
--   'Damage', the engine verbs, the real save codec and load gate) and the
--   SHIPPED Lua (@scripts\/physiology.lua@, @scripts\/unit_resources.lua@ and
--   everything it ticks) over a real headless engine.
--
--   One module per contract area, each named for the design's cases
--   (@docs\/designs\/physiology_stat_modes_design.md@ S8):
--
--     * this module: the resolver and declarations, the maximum (T-S8-6)
--       and the commit boundary arithmetic (T-S8-7, T-S8-8), neutrality;
--     * "Test.Headless.Unit.PhysiologyStaminaModes.Combat": T-S8-1 to
--       T-S8-4, strikes through the real admission commit;
--     * "Test.Headless.Unit.PhysiologyStaminaModes.Tuning": T-S8-5, the two
--       deterministic nomad cases;
--     * "Test.Headless.Unit.PhysiologyStaminaModes.Persistence": pending,
--       applied and pinned state across the real codec, legacy scrubbing
--       and malformed markers.
--
--   Nothing is rolled: every input is a literal, and the cases that need a
--   random stream pin it. Run just this gate: @cabal test
--   synarchy-test-headless --test-options='--match "physiology stamina
--   modes"'@.
module Test.Headless.Unit.PhysiologyStaminaModes (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (modifyIORef', readIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState)
import qualified Test.Headless.Unit.PhysiologyStaminaModes.Combat as Combat
import qualified Test.Headless.Unit.PhysiologyStaminaModes.Persistence as Persistence
import qualified Test.Headless.Unit.PhysiologyStaminaModes.Tuning as Tuning
import Test.Headless.Unit.PhysiologyFixture (readShippedPhysiologyRegistry)
import Test.Headless.Unit.PhysiologyModes.Harness
import Combat.Resolution.Common (maxStaminaFor)
import Combat.Resolution.Stamina (staminaModeFor)
import Combat.Resolution.Wear (spendStrikeCost)
import Combat.Types (AttackMode(..))
import Unit.Command.Types (UnitCommand(..))
import Unit.Physiology
import Unit.Physiology.Types
import Unit.Stats (effectiveStat)
import Unit.Types

spec ∷ Spec
spec = describe "physiology stamina modes" $ do
    declarationSpec
    resolverSpec
    maximumSpec
    commitSpec
    neutralitySpec
    Combat.spec
    Tuning.spec
    Persistence.spec

-----------------------------------------------------------------------
-- Fixtures
-----------------------------------------------------------------------

-- | An endurance-1 unit whose ONLY simulated-or-not system is stamina:
--   every other system is Nominal (exempt), thermo included, so nothing
--   shivers or drains. It carries no explicit @max_stamina@ and no
--   @stamina@ pool of its own: the maximum resolves from @endurance@ and a
--   Simulated pool is the one the seeding rule creates.
staminaUnit ∷ Int → PhysioMode → UnitSpec
staminaUnit uid mode = (unitSpec uid "stamina_unit" (Just "exempt"))
    { usModes = [("stamina", mode)]
    , usDropStats = ["max_stamina", "stamina"] }

-- | A modifier on a stat: flat delta and percent, never expiring.
modOf ∷ Float → Float → StatModifier
modOf delta percent = StatModifier delta "spec" Nothing percent

-- | The stored stamina base, as the engine holds it.
storedStamina ∷ EngineEnv → Int → IO (Maybe Float)
storedStamina env uid = HM.lookup "stamina" ∘ uiStats <$> readInstance env uid

-- | The stored stamina base or a failing expectation.
storedBase ∷ EngineEnv → Int → IO Float
storedBase env uid = storedStamina env uid ≫= \case
    Just v  → pure v
    Nothing → do
        expectationFailure "no stored stamina"
        pure 0

-- | @unit.commitStamina@ as @before|after|maximum|status@, or @nil|reason@.
commitText ∷ LuaBackendState → Int → Double → IO Text
commitText ls uid delta = luaText ls $ T.concat
    [ "local r, why = unit.commitStamina(", tshow uid, ", ", tshow delta, "); "
    , "if not r then return 'nil|' .. tostring(why) end; "
    , "return __f(r.before) .. '|' .. __f(r.after) .. '|' .. __f(r.maximum) "
    , ".. '|' .. r.status" ]

-- | The effective stamina through the read verb.
effRead ∷ LuaBackendState → Int → IO Text
effRead ls uid = readOwned ls uid "stamina"

-- | One strike's charge, through the PRODUCTION cost function and the
--   engine's own registry, in one unit-manager update.
strike ∷ EngineEnv → AttackMode → Int → IO ()
strike env mode uid = do
    reg ← readIORef (physiologyRegistryRef env)
    now ← readIORef (gameTimeRef env)
    modifyIORef' (unitManagerRef env)
        (spendStrikeCost reg now mode (fromIntegral uid))

-- | The mode the engine's resolver gives stamina for a unit now.
modeNow ∷ EngineEnv → Int → IO ResolvedMode
modeNow env uid = do
    reg ← readIORef (physiologyRegistryRef env)
    inst ← readInstance env uid
    um ← readIORef (unitManagerRef env)
    pure (staminaModeFor reg (definitionOf um inst) inst)
  where
    definitionOf um inst = maybe noUnitPhysiology udPhysiology
        (HM.lookup (uiDefName inst) (umDefs um))

-----------------------------------------------------------------------
-- Declarations: the registry entry, the profile columns, the validation
-----------------------------------------------------------------------

declarationSpec ∷ Spec
declarationSpec = describe "the stamina system and its declarations" $ do
    it "is a registry system requiring a body, owning stamina with no meters, \
       \and supplying stamina fraction 1.0 under Nominal" $ do
        reg ← readShippedPhysiologyRegistry
        case registrySystem reg "stamina" of
            Nothing → expectationFailure "no stamina system"
            Just sys → do
                psRequires sys `shouldBe` ["body"]
                map okKey (psOwns sys) `shouldBe` ["stamina"]
                psMeters sys `shouldBe` []
                psDerived sys `shouldBe` []
                [ (ieName e, ieSubstitute e) | e ← psProvides sys ]
                    `shouldBe` [("stamina_fraction", SubValue (QLit 1.0))]
                -- every effect is absent while stamina is Nominal
                [ ieSubstitute e | e ← psEffects sys ]
                    `shouldBe` replicate (length (psEffects sys)) SubNone
                map ieName (psEffects sys)
                    `shouldBe` ["collapse", "kill_on_zero", "strike_cost"]
                map okHealthy (psOwns sys) `shouldBe` [HealthyMaxStamina]

    it "gives every profile a stamina mode, and leaves stance unowned" $ do
        reg ← readShippedPhysiologyRegistry
        forM_ [("rich", PhysioSimulated), ("reduced", PhysioSimulated)
              , ("simple", PhysioSimulated), ("exempt", PhysioNominal)] $
            \(p, m) →
                (Map.lookup "stamina" ∘ ppModes <$> registryProfile reg p)
                    `shouldBe` Just (Just m)
        classifyKey reg "stance" `shouldBe` Nothing
        classifyKey reg "exhaustion" `shouldBe` Nothing
        classifyKey reg "sleep_pressure" `shouldBe` Nothing
        classifyKey reg "max_stamina" `shouldBe` Nothing

    it "validates an engagement declaration: the system exists, is stamina \
       \(the verb that applies it), is Nominal by default and takes simulated" $ do
        reg ← readShippedPhysiologyRegistry
        let resolve profile entries engage =
                resolveUnitPhysiology reg "u" True profile entries engage
        fmap upEngagement (resolve (Just "exempt") [("stamina", "nominal")]
                                   [("stamina", "simulated")])
            `shouldBe` Right (HM.fromList [("stamina", PhysioSimulated)])
        fmap upEngagement (resolve (Just "exempt") [] [])
            `shouldBe` Right HM.empty
        -- an unknown system, a system the verb cannot apply, an invalid
        -- mode, and a system that is not Nominal by default
        fst <$> leftOf (resolve (Just "exempt") [] [("ghost", "simulated")])
            `shouldBe` Just "unknown physiology system"
        fst <$> leftOf (resolve (Just "exempt") [] [("hydration", "simulated")])
            `shouldBe` Just "unsupported physiology engagement system"
        fst <$> leftOf (resolve (Just "exempt") [] [("stamina", "nominal")])
            `shouldBe` Just "invalid physiology engagement mode"
        fst <$> leftOf (resolve (Just "exempt") [] [("stamina", "5")])
            `shouldBe` Just "invalid physiology engagement mode"
        fst <$> leftOf (resolve (Just "rich") [] [("stamina", "simulated")])
            `shouldBe` Just "physiology engagement system is not Nominal by default"

    it "never lets a definition template the engagement marker or a raw write \
       \create it" $ do
        reg ← readShippedPhysiologyRegistry
        rejectPhysiologyTemplates reg "u" ["combat_engaged"] []
            `shouldBe` Just ( "physiology key in unit template"
                            , "u: stats.combat_engaged" )
        rejectPhysiologyTemplates reg "u" [] ["combat_engaged"]
            `shouldBe` Just ( "physiology key in unit template"
                            , "u: skills.combat_engaged" )
        rejectPhysiologyTemplates reg "u" ["stamina"] []
            `shouldBe` Just ( "physiology key in unit template"
                            , "u: stats.stamina" )
        let up = plainUnitPhysiology True Nothing
                     (HM.fromList [("stamina", PhysioSimulated)])
        rawWriteAllowed reg up False HM.empty "combat_engaged" `shouldBe` False
        rawWriteAllowed reg up True HM.empty "combat_engaged" `shouldBe` False
        rawWriteAllowed reg up False HM.empty "stamina" `shouldBe` True
        rawWriteAllowed reg up False
            (HM.fromList [("stamina", PhysioNominal)]) "stamina"
            `shouldBe` False
  where
    leftOf (Left e)  = Just e
    leftOf (Right _) = Nothing

-----------------------------------------------------------------------
-- The resolver's precedence
-----------------------------------------------------------------------

resolverSpec ∷ Spec
resolverSpec = describe "the mode resolver and the engagement marker" $ do
    it "resolves override, else the engagement default only once the marker \
       \is 2, else the species default" $ do
        reg ← readShippedPhysiologyRegistry
        sys ← maybe (expectationFailure "no stamina system" ≫ pure (error "unreachable"))
                    pure (registrySystem reg "stamina")
        let declares = (plainUnitPhysiology True Nothing
                            (HM.fromList [("stamina", PhysioNominal)]))
                           { upEngagement = HM.fromList [("stamina", PhysioSimulated)] }
            plain = plainUnitPhysiology True Nothing
                        (HM.fromList [("stamina", PhysioNominal)])
            ctxOf up marker ovr = PhysioContext
                { pcRegistry = reg, pcPhysiology = up
                , pcOverrides = HM.fromList ovr, pcNow = 0
                , pcStats = HM.fromList (("endurance", 1) : marker)
                , pcModifiers = HM.empty }
            mode up marker ovr = modeOf (ctxOf up marker ovr) sys
        -- absent and pending are not an engagement: the species default
        mode declares [] [] `shouldBe` ModeNominal
        mode declares [("combat_engaged", 1)] [] `shouldBe` ModeNominal
        -- applied: the engagement default
        mode declares [("combat_engaged", 2)] [] `shouldBe` ModeSimulated
        -- an explicit override wins, pending or applied
        mode declares [("combat_engaged", 2)] [("stamina", PhysioNominal)]
            `shouldBe` ModeNominal
        mode declares [("combat_engaged", 1)] [("stamina", PhysioSimulated)]
            `shouldBe` ModeSimulated
        mode declares [] [("stamina", PhysioSimulated)] `shouldBe` ModeSimulated
        -- a marker on a species that does not declare the trigger has no effect
        mode plain [("combat_engaged", 2)] [] `shouldBe` ModeNominal
        mode plain [("combat_engaged", 1)] [] `shouldBe` ModeNominal
        -- a bodyless unit stays Unsupported whatever else is stored
        modeOf (ctxOf noUnitPhysiology [("combat_engaged", 2)]
                       [("stamina", PhysioSimulated)]) sys
            `shouldBe` ModeUnsupported

    it "does not infer a mode from a stored stamina key" $
        withScene [ (staminaUnit 1 PhysioNominal) ] $ \(env, _) → do
            modifyInstance env 1 $ \i →
                i { uiStats = HM.insert "stamina" 3 (uiStats i) }
            modeNow env 1 `shouldReturn` ModeNominal
            -- and the Nominal read stays the healthy maximum
            storedStamina env 1 `shouldReturn` Just 3

    it "refuses unit.setStat on the engagement marker, in either state, and \
       \leaves the unit as it was" $
        withScene [ (staminaUnit 1 PhysioSimulated) ] $ \(env, ls) → do
            before ← uiStats <$> readInstance env 1
            luaText ls "return tostring(unit.setStat(1, 'combat_engaged', 2))"
                `shouldReturn` "false"
            luaText ls "return tostring(unit.setStat(1, 'combat_engaged', 1))"
                `shouldReturn` "false"
            uiStats <$> readInstance env 1 `shouldReturn` before

-----------------------------------------------------------------------
-- T-S8-6: one maximum in every consumer
-----------------------------------------------------------------------

-- | Lua's @stats.get(uid, 'max_stamina')@, the verb, and Haskell's
--   'maxStaminaFor' at the same game time, for one unit.
maxReads ∷ EngineEnv → LuaBackendState → Int → IO (Text, Text, Float)
maxReads env ls uid = do
    viaStats ← luaText ls $ "return __f(require('scripts.unit_stats')\
                            \.get(" <> tshow uid <> ", 'max_stamina'))"
    viaVerb ← luaText ls $ "return __f(unit.getMaxStamina(" <> tshow uid <> "))"
    now ← readIORef (gameTimeRef env)
    inst ← readInstance env uid
    pure (viaStats, viaVerb, maxStaminaFor now inst)

maximumSpec ∷ Spec
maximumSpec = describe "one maximum in every consumer (T-S8-6)" $ do
    it "an endurance-1 unit reads 10 in stats.get, unit.getMaxStamina and \
       \maxStaminaFor, and the Nominal healthy read and the seed target are \
       \the same value" $
        withScene [ staminaUnit 1 PhysioNominal ] $ \(env, ls) → do
            maxReads env ls 1 `shouldReturn` ("10", "10", 10)
            effRead ls 1 `shouldReturn` "10|nominal"
            commitText ls 1 0 `shouldReturn` "nil|nominal"
            storedStamina env 1 `shouldReturn` Nothing
            luaText ls "return tostring(__physiology.setMode(1, 'stamina', 'simulated'))"
                `shouldReturn` "true"
            storedStamina env 1 `shouldReturn` Just 10
            commitText ls 1 0 `shouldReturn` "10|10|10|ok"

    it "an innate +50% max_stamina modifier moves the derived maximum to 15 \
       \everywhere, with no stored max_stamina" $
        withScene [ (staminaUnit 1 PhysioNominal)
                      { usModifiers = [("max_stamina", [modOf 0 0.5])] } ] $
            \(env, ls) → do
                maxReads env ls 1 `shouldReturn` ("15", "15", 15)
                HM.member "max_stamina" ∘ uiStats <$> readInstance env 1
                    `shouldReturn` False
                effRead ls 1 `shouldReturn` "15|nominal"
                luaText ls "return tostring(__physiology.setMode(1, 'stamina', 'simulated'))"
                    `shouldReturn` "true"
                -- the modifier is on the MAXIMUM, not the pool: the seed target
                -- is the effective maximum and the pool carries no modifier
                storedStamina env 1 `shouldReturn` Just 15
                effRead ls 1 `shouldReturn` "15|simulated"
                commitText ls 1 0 `shouldReturn` "15|15|15|ok"
                commitText ls 1 1 `shouldReturn` "15|15|15|ok"       -- clamped
                commitText ls 1 (-3.75) `shouldReturn` "15|11.25|15|ok"

    it "derived maximum modifiers compose with endurance modifiers, and an \
       \explicit max_stamina keeps its precedence over both" $ do
        -- endurance 1 +1 flat = 2 → 20, then +50% on max_stamina = 30
        withScene [ (staminaUnit 1 PhysioNominal)
                      { usModifiers = [ ("endurance", [modOf 1 0])
                                      , ("max_stamina", [modOf 0 0.5]) ] } ] $
            \(env, ls) → maxReads env ls 1 `shouldReturn` ("30", "30", 30)
        -- a flat max_stamina modifier applies to the derived base too
        withScene [ (staminaUnit 1 PhysioNominal)
                      { usModifiers = [("max_stamina", [modOf 5 0])] } ] $
            \(env, ls) → maxReads env ls 1 `shouldReturn` ("15", "15", 15)
        -- explicit attribute: its own modifiers apply, endurance's do not
        withScene [ (staminaUnit 1 PhysioNominal)
                      { usStats = [("max_stamina", 40)]
                      , usDropStats = ["stamina"]
                      , usModifiers = [ ("endurance", [modOf 9 0])
                                      , ("max_stamina", [modOf 0 0.25]) ] } ] $
            \(env, ls) → maxReads env ls 1 `shouldReturn` ("50", "50", 50)

    it "resolves modifier expiry at the captured game time: a modifier is \
       \active iff now < its expiry, in every consumer" $
        withScene [ (staminaUnit 1 PhysioNominal)
                      { usModifiers = [ ("max_stamina"
                                        , [StatModifier 0 "e" (Just 100) 0.5]) ] } ] $
            \(env, ls) → do
                writeGameTime env 99.5
                maxReads env ls 1 `shouldReturn` ("15", "15", 15)
                writeGameTime env 100
                maxReads env ls 1 `shouldReturn` ("10", "10", 10)
                writeGameTime env 100.5
                maxReads env ls 1 `shouldReturn` ("10", "10", 10)

    it "keeps unit.getMaxStamina pure and answers nil where there is no \
       \maximum: a missing unit, and a unit with no endurance" $
        withScene [ (staminaUnit 1 PhysioNominal) { usDropStats = ["max_stamina", "stamina", "endurance"] } ] $
            \(env, ls) → do
                luaText ls "return __f(unit.getMaxStamina(1))" `shouldReturn` "nil"
                luaText ls "return __f(unit.getMaxStamina(99))" `shouldReturn` "nil"
                luaText ls "return __f(require('scripts.unit_stats').get(1, 'max_stamina'))"
                    `shouldReturn` "nil"
                before ← uiStats <$> readInstance env 1
                _ ← luaText ls "return unit.getMaxStamina(1)"
                uiStats <$> readInstance env 1 `shouldReturn` before

-----------------------------------------------------------------------
-- T-S8-7 and T-S8-8: the commit boundary arithmetic
-----------------------------------------------------------------------

commitSpec ∷ Spec
commitSpec = describe "the effective stamina commit" $ do
    it "T-S8-7: a retained -50% modifier seeds base 20 for effective 10; a \
       \zero commit and a full-pool regen stay effective 10; a heavy strike \
       \reports 10 → 7.5; a shiver debit of 0.1 reports an effective decrease of 0.1" $
        withScene [ (staminaUnit 1 PhysioNominal)
                      { usModifiers = [("stamina", [modOf 0 (-0.5)])] } ] $
            \(env, ls) → do
                -- Nominal ignores the modifier on the key itself
                effRead ls 1 `shouldReturn` "10|nominal"
                luaText ls "return tostring(__physiology.setMode(1, 'stamina', 'simulated'))"
                    `shouldReturn` "true"
                storedBase env 1 `shouldReturn` 20
                effRead ls 1 `shouldReturn` "10|simulated"
                commitText ls 1 0 `shouldReturn` "10|10|10|ok"
                effRead ls 1 `shouldReturn` "10|simulated"
                -- full-pool regen: idle, regen clipped at the maximum
                runSeconds ls 1 0.1
                effRead ls 1 `shouldReturn` "10|simulated"
                storedBase env 1 `shouldReturn` 20
                -- a heavy strike (0.25 of the effective 10)
                strike env Heavy 1
                effRead ls 1 `shouldReturn` "7.5|simulated"
                storedBase env 1 `shouldReturn` 15
                -- the shiver debit goes through the facade's relative commit
                applied ← luaNum ls "__physiology.debit(1, 'stamina', 0.1)"
                near 1e-5 0.1 applied
                eff1 ← luaNum ls "unit.readPhysio(1, 'stamina')"
                near 1e-5 7.4 eff1
                out ← commitText ls 1 (-0.1)
                out `shouldSatisfy` T.isSuffixOf "|10|ok"

    it "T-S8-7: with +50% regen never pushes the effective value above the \
       \maximum, and with -100% the transition is refused while an existing \
       \Simulated pool honestly reads zero" $ do
        withScene [ (staminaUnit 1 PhysioNominal)
                      { usModifiers = [("stamina", [modOf 0 0.5])] } ] $
            \(env, ls) → do
                luaText ls "return tostring(__physiology.setMode(1, 'stamina', 'simulated'))"
                    `shouldReturn` "true"
                effRead ls 1 `shouldReturn` "10|simulated"
                ex ← track ls 1 5 0.1 ["stamina"]
                exMax (ex HM.! "stamina") `shouldSatisfy` (≤ 10 + 1e-4)
                exMin (ex HM.! "stamina") `shouldSatisfy` (≥ 10 - 1e-4)
                _ ← storedBase env 1
                pure ()
        withScene [ (staminaUnit 1 PhysioNominal)
                      { usModifiers = [("stamina", [modOf 0 (-1)])] } ] $
            \(env, ls) → do
                before ← uiStats <$> readInstance env 1
                luaText ls "local ok, why = __physiology.setMode(1, 'stamina', 'simulated'); \
                           \return tostring(ok)"
                    `shouldReturn` "nil"
                uiStats <$> readInstance env 1 `shouldReturn` before
                uiPhysioOverrides <$> readInstance env 1 `shouldReturn` HM.empty
        withScene [ staminaUnit 1 PhysioSimulated ] $ \(env, ls) → do
            -- an already-Simulated pool: honest zero, never a refusal
            modifyInstance env 1 $ \i →
                i { uiModifiers = HM.insert "stamina" [modOf 0 (-1)] (uiModifiers i) }
            effRead ls 1 `shouldReturn` "0|simulated"

    it "T-S8-8: b = 0, flat -10, percent -2 (m = -1) reads effective 10, and a \
       \heavy-sized -2.5 stores base 2.5 and reports 10 → 7.5" $
        withScene [ (staminaUnit 1 PhysioSimulated) ] $ \(env, ls) → do
            modifyInstance env 1 $ \i → i
                { uiStats = HM.insert "stamina" 0 (uiStats i)
                , uiModifiers = HM.insert "stamina" [modOf (-10) (-2)] (uiModifiers i) }
            effRead ls 1 `shouldReturn` "10|simulated"
            commitText ls 1 (-2.5) `shouldReturn` "10|7.5|10|ok"
            storedBase env 1 `shouldReturn` 2.5
            effRead ls 1 `shouldReturn` "7.5|simulated"

    it "T-S8-8: a flat floor above the maximum stores base 0, reports the \
       \honest 15 as floored, and fires no collapse or death; a seed is refused" $ do
        withScene [ (staminaUnit 1 PhysioSimulated) ] $ \(env, ls) → do
            modifyInstance env 1 $ \i → i
                { uiModifiers = HM.insert "stamina" [modOf 15 0] (uiModifiers i) }
            commitText ls 1 (-2.5) `shouldReturn` "25|15|10|floored"
            storedBase env 1 `shouldReturn` 0
            effRead ls 1 `shouldReturn` "15|simulated"
            commitText ls 1 0 `shouldReturn` "15|15|10|floored"
            -- a pass at the floored pool consequences: neither collapse nor kill
            _ ← drainCommands env
            runSeconds ls 1 0.1
            cmds ← drainCommands env
            length (filter isKillOrCollapse cmds) `shouldBe` 0
        withScene [ (staminaUnit 1 PhysioNominal)
                      { usModifiers = [("stamina", [modOf 15 0])] } ] $
            \(env, ls) → do
                before ← uiStats <$> readInstance env 1
                luaText ls "return tostring(__physiology.setMode(1, 'stamina', 'simulated'))"
                    `shouldReturn` "nil"
                uiStats <$> readInstance env 1 `shouldReturn` before

    it "T-S8-8: m = 0 reports zero as zeroMultiplier and leaves the base" $
        withScene [ (staminaUnit 1 PhysioSimulated) ] $ \(env, ls) → do
            modifyInstance env 1 $ \i → i
                { uiStats = HM.insert "stamina" 6 (uiStats i)
                , uiModifiers = HM.insert "stamina" [modOf 0 (-1)] (uiModifiers i) }
            commitText ls 1 (-2.5) `shouldReturn` "0|0|10|zeroMultiplier"
            storedBase env 1 `shouldReturn` 6
            effRead ls 1 `shouldReturn` "0|simulated"

    it "T-S8-8: an already-Simulated commit may return ok at base 0 for target \
       \10 (flat -10, percent -2), but the Nominal-to-Simulated transition refuses" $ do
        withScene [ staminaUnit 1 PhysioSimulated ] $ \(env, ls) → do
            modifyInstance env 1 $ \i → i
                { uiStats = HM.insert "stamina" 0 (uiStats i)
                , uiModifiers = HM.insert "stamina" [modOf (-10) (-2)] (uiModifiers i) }
            commitText ls 1 0 `shouldReturn` "10|10|10|ok"
            storedBase env 1 `shouldReturn` 0
        withScene [ (staminaUnit 1 PhysioNominal)
                      { usModifiers = [("stamina", [modOf (-10) (-2)])] } ] $
            \(env, ls) → do
                before ← uiStats <$> readInstance env 1
                luaText ls "return tostring(__physiology.setMode(1, 'stamina', 'simulated'))"
                    `shouldReturn` "nil"
                uiStats <$> readInstance env 1 `shouldReturn` before
                uiPhysioOverrides <$> readInstance env 1 `shouldReturn` HM.empty

    it "reports every value as an effective read of what is stored, across a \
       \modifier sweep, and never differs from an independent effectiveStat" $
        forM_ [ ([], 4.0), ([modOf 2 0], 5.0), ([modOf 0 0.5], 6.0)
              , ([modOf (-1) 0.25], 8.0), ([modOf 0 (-0.25), modOf 1 0], 7.0) ]
            $ \(mods, start) →
                withScene [ staminaUnit 1 PhysioSimulated ] $ \(env, ls) → do
                    modifyInstance env 1 $ \i → i
                        { uiStats = HM.insert "stamina" start (uiStats i)
                        , uiModifiers = HM.insert "stamina" mods (uiModifiers i) }
                    forM_ [-3, 0.5, -0.25, 4, -9, 20] $ \delta → do
                        beforeBase ← storedBase env 1
                        out ← commitText ls 1 delta
                        afterBase ← storedBase env 1
                        let effOf b = effectiveStat 0 b mods
                            parts = T.splitOn "|" out
                        case parts of
                            [b, a, m, _] → do
                                reads' b `shouldSatisfy` nearTo (effOf beforeBase)
                                reads' a `shouldSatisfy` nearTo (effOf afterBase)
                                reads' m `shouldBe` 10
                            _ → expectationFailure ("unexpected commit: " <> T.unpack out)
  where
    isKillOrCollapse c = case c of
        UnitKill _     → True
        UnitCollapse _ → True
        _              → False
    reads' t = case reads (T.unpack t) of
        [(v, "")] → v ∷ Float
        _ → 0 / 0
    nearTo want got = abs (got - want) < 1e-4

-----------------------------------------------------------------------
-- Neutrality
-----------------------------------------------------------------------

neutralitySpec ∷ Spec
neutralitySpec = describe "stamina neutrality" $ do
    it "a Nominal unit reads stamina fraction 1.0, holds no stamina key through \
       \ticks and keeps stance unowned and always on" $
        withScene [ staminaUnit 1 PhysioNominal ] $ \(env, ls) → do
            luaText ls "return __f(__physiology.fraction(1, 'stamina', 'max_stamina'))"
                `shouldReturn` "1"
            runSeconds ls 5 0.1
            storedStamina env 1 `shouldReturn` Nothing
            effRead ls 1 `shouldReturn` "10|nominal"
            -- the stance cost of a strike is spent in every mode
            strike env Heavy 1
            storedStamina env 1 `shouldReturn` Nothing
            HM.lookup "stance" ∘ uiStats <$> readInstance env 1
                `shouldReturn` Just 0.5

    it "a Simulated unit keeps healthy standalone behavior: idle regen to the \
       \maximum, fractions in 0..1" $
        withScene [ staminaUnit 1 PhysioSimulated ] $ \(env, ls) → do
            strike env Heavy 1
            effRead ls 1 `shouldReturn` "7.5|simulated"
            ex ← track ls 1 20 0.1 ["stamina"]
            exMin (ex HM.! "stamina") `shouldSatisfy` (≥ 7.5 - 1e-4)
            exMax (ex HM.! "stamina") `shouldSatisfy` (≤ 10 + 1e-4)
            -- 20 s of idle regen at 0.5/s restores a 2.5 deficit
            effRead ls 1 `shouldReturn` "10|simulated"

    it "stamina → brain: a Nominal pool reads fraction 1.0 to the brain, a \
       \Simulated half pool a lower concentration" $ do
        half ← withScene [ staminaUnit 1 PhysioSimulated ] $ \(env, ls) → do
            modifyInstance env 1 $ \i →
                i { uiStats = HM.insert "stamina" 5 (uiStats i) }
            luaNum ls "require('scripts.brain').staminaFrac(1)"
        full ← withScene [ staminaUnit 1 PhysioNominal ] $ \(_, ls) →
            luaNum ls "require('scripts.brain').staminaFrac(1)"
        near 1e-9 1 full
        near 1e-6 0.5 half

    it "nutrition → stamina: the fat-floor organ failure drains a Simulated \
       \pool and does nothing to a Nominal one, while Simulated nutrition's \
       \lean-floor starvation kill stays active" $ do
        -- fat floor: 2% of the 70 kg frame
        let starving st = (staminaUnit 1 st)
                { usProfile = Just "exempt"
                , usModes = [("stamina", st), ("nutrition", PhysioSimulated)]
                , usStats = [("fat_mass", 1.0), ("lean_mass", 69.0)] }
        simDrop ← withScene [ starving PhysioSimulated ] $ \(env, ls) → do
            runSeconds ls 5 0.1
            maybe 0 realToFrac <$> storedStamina env 1 ∷ IO Double
        simDrop `shouldSatisfy` (< 9.0)
        withScene [ starving PhysioNominal ] $ \(env, ls) → do
            runSeconds ls 5 0.1
            storedStamina env 1 `shouldReturn` Nothing
            effRead ls 1 `shouldReturn` "10|nominal"
