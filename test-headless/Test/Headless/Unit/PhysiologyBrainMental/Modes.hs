{-# LANGUAGE Strict, OverloadedStrings #-}
-- | Profiles, explicit Nominal overrides and persistence of the brain and the
--   mind (#2838, S11a): I1/I3 and the persistence/presentation acceptance.
--   Both systems default Simulated for every supported shipped species in
--   this slice (S11b applies SD-23), a bodyless definition is Unsupported,
--   and the Nominal machinery works through EXPLICIT overrides only.
module Test.Headless.Unit.PhysiologyBrainMental.Modes (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Headless.Unit.PhysiologyFixture (readShippedPhysiologyRegistry)
import Test.Headless.Unit.PhysiologyModes.Harness
import Test.Headless.Unit.PhysiologySpeciesProfiles
    ( codecRoundTrip, gate, onSavePage, pageSnapshotOf, placeUnits
    , shippedDefs, withShipped )
import Unit.Physiology
import Unit.Physiology.Types
import Unit.Types
import World.Save.Types (UnitInstanceSnapshot(..), UnitSnapshot(..))
import World.Save.Snapshot (PageSnapshot(..))

spec ∷ Spec
spec = describe "profiles, overrides and persistence" $ do
    profileSpec
    nominalSpec
    healthySpec
    persistenceSpec
    pinsSpec

mindKeys ∷ [Text]
mindKeys =
    [ "mood", "emotional_pain", "caffeine", "mental_state", "mental_since"
    , "mental_until", "mental_next_roll_at", "mental_cooldown_until"
    , "mental_high_since", "mental_break_behavior", "thought_next_at" ]

derivedKeys ∷ [Text]
derivedKeys = ["consciousness", "concentration", "state_of_mind"]

-----------------------------------------------------------------------
-- I1/I3: profile coverage
-----------------------------------------------------------------------

profileSpec ∷ Spec
profileSpec = describe "profile coverage" $ do
    it "puts consciousness and mind Simulated in every profile, and resolves a \
       \bodyless definition Unsupported for both" $ do
        reg ← readShippedPhysiologyRegistry
        forM_ (prProfiles reg) $ \p →
            forM_ ["consciousness", "mind"] $ \sid →
                (ppId p, Map.lookup sid (ppModes p))
                    `shouldBe` (ppId p, Just PhysioSimulated)
        forM_ ["consciousness", "mind"] $ \sid →
            fmap (\s → resolveMode s noUnitPhysiology False Nothing)
                 (registrySystem reg sid)
                `shouldBe` Just ModeUnsupported

    it "resolves both systems Simulated for every supported shipped species and \
       \Unsupported for the bodyless ones, by the real resolver" $
        withShipped $ \(env, _) → do
            reg ← readShippedPhysiologyRegistry
            defs ← shippedDefs env
            forM_ (HM.toList defs) $ \(name, def) →
                forM_ ["consciousness", "mind"] $ \sid → do
                    let got = fmap (\s → resolveMode s (udPhysiology def) False Nothing)
                                   (registrySystem reg sid)
                        want | upBodyAuthored (udPhysiology def) = ModeSimulated
                             | otherwise                         = ModeUnsupported
                    (name, sid, got) `shouldBe` (name, sid, Just want)

-----------------------------------------------------------------------
-- Explicit Nominal overrides
-----------------------------------------------------------------------

nominalSpec ∷ Spec
nominalSpec = describe "explicit Nominal overrides" $ do
    it "supply the declared interfaces with the keys and derived caches absent" $
        withScene [ (unitSpec 1 "acolyte" (Just "rich"))
                      { usOverrides = [("consciousness", PhysioNominal), ("mind", PhysioNominal)]
                      , usStats = [("blood_oxygen", 0.4)] } ] $ \(env, ls) → do
            -- consciousness Nominal: the fully lucid unit, whatever its oxygen
            readOwned ls 1 "consciousness" `shouldReturn` "1|nominal"
            readOwned ls 1 "concentration" `shouldReturn` "1|nominal"
            -- mind Nominal: mood 1, emotional pain 0, caffeine 0, STABLE; the
            -- state of mind is the live consciousness (here Nominal: 1)
            readOwned ls 1 "mood" `shouldReturn` "1|nominal"
            readOwned ls 1 "emotional_pain" `shouldReturn` "0|nominal"
            readOwned ls 1 "caffeine" `shouldReturn` "0|nominal"
            readOwned ls 1 "mental_state" `shouldReturn` "0|nominal"
            readOwned ls 1 "state_of_mind" `shouldReturn` "1|nominal"
            i ← readInstance env 1
            forM_ (mindKeys ⧺ derivedKeys) $ \k → HM.member k (uiStats i) `shouldBe` False

    it "read a Nominal mind's state of mind as the LIVE consciousness" $
        withScene [ (unitSpec 1 "acolyte" (Just "rich"))
                      { usOverrides = [("mind", PhysioNominal)]
                      , usStats = [("blood_oxygen", 0.65)] } ] $ \(_, ls) → do
            luaNum ls "unit.readPhysio(1, 'consciousness')" >>= near 1e-5 0.5
            luaNum ls "unit.readPhysio(1, 'state_of_mind')" >>= near 1e-5 0.5

    it "gate the owned effects: a Nominal consciousness never collapses a hypoxic \
       \unit from physiology, and a concussion still knocks it out" $ do
        withScene [ (unitSpec 1 "acolyte" (Just "rich"))
                      { usOverrides = [("consciousness", PhysioNominal)]
                      , usStats = [("blood_oxygen", 0.4)] } ] $ \(env, ls) → do
            _ ← drainCommands env
            runSeconds ls 0.1 0.1
            collapses env `shouldReturn` []
        withScene [ (unitSpec 1 "acolyte" (Just "rich"))
                      { usOverrides = [("consciousness", PhysioNominal)]
                      , usWounds = [concussion] } ] $ \(env, ls) → do
            _ ← drainCommands env
            runSeconds ls 0.1 0.1
            collapses env >>= (`shouldContain` [UnitId 1])

concussion ∷ Wound
concussion = Wound
    { woundPart = "head", woundKind = "concussion", woundSeverity = 0.6
    , woundAt = 0, woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0
    , woundDressing = "", woundInfection = 0.0, woundClean = True
    , woundInfectionType = "", woundNecrosis = 0.0 }

-----------------------------------------------------------------------
-- Healthy standalone cases over 600 seconds
-----------------------------------------------------------------------

healthySpec ∷ Spec
healthySpec = describe "healthy standalone units over 600 seconds" $
    forM_ [("acolyte", "rich"), ("bear_brown", "reduced")] $ \(def, profile) →
        it (T.unpack def <> ": stays lucid, content and unbroken, with the existing \
                         \thought cadence and no retuning") $
            withScene [ unitSpec 1 def (Just profile) ] $ \(env, ls) → do
                _ ← luaText ls
                    "_G.__n = 0; local emit = thought.emit; \
                    \thought.emit = function(...) __n = __n + 1; return emit(...) end; \
                    \require('scripts.thoughts').loadCatalogue(); return true"
                _ ← drainCommands env
                ex ← trackWith ls 1 600 1.0 ["cons", "mood", "state_of_mind", "mental_state"]
                    "function(step, t) _G.__t = t end"
                exMin (ex HM.! "cons") `shouldSatisfy` (> 0.999)
                exMin (ex HM.! "mood") `shouldSatisfy` (> 0.75)
                exMin (ex HM.! "state_of_mind") `shouldSatisfy` (> 0.75)
                exMax (ex HM.! "mental_state") `shouldBe` 0
                forM_ ex $ \e → exNaN e `shouldBe` 0
                -- thoughts fire at most every 30 game seconds: 600 s holds at
                -- most 20 (what is eligible depends on the species' state)
                n ← luaNum ls "__n"
                n `shouldSatisfy` (\v → v ≥ 1 ∧ v ≤ 25)
                collapses env `shouldReturn` []
                scriptErrors ls `shouldReturn` ""

-----------------------------------------------------------------------
-- Persistence through the real codec and gate
-----------------------------------------------------------------------

persistenceSpec ∷ Spec
persistenceSpec = describe "persistence" $ do
    it "round-trips mid-episode durable keys and pins exactly through the real \
       \codec with no normalization, and stores no derived key in any mode" $
        withShipped $ \(env, _) → do
            reg ← readShippedPhysiologyRegistry
            defs ← shippedDefs env
            placeUnits env
                [ (1, "acolyte", Nothing, [], [])
                , (2, "acolyte", Nothing, [("mind", PhysioNominal)], [])
                , (3, "acolyte", Nothing, [("consciousness", PhysioNominal)], []) ]
            modifyInstance env 1 $ \i → i { uiStats = foldr (uncurry HM.insert) (uiStats i)
                [ ("mood", 0.37), ("emotional_pain", 0.21), ("caffeine", 0.5)
                , ("mental_state", 2), ("mental_since", 40), ("mental_until", 170)
                , ("mental_next_roll_at", 48), ("mental_cooldown_until", 0)
                , ("mental_high_since", 0), ("mental_break_behavior", 3)
                , ("thought_next_at", 66) ] }
            onSavePage env [1, 2, 3]
            pg ← pageSnapshotOf env ≫= codecRoundTrip
            (us, notes) ← gate reg defs (pgsUnits pg)
            notes `shouldBe` []
            usnInstances us `shouldBe` usnInstances (pgsUnits pg)
            forM_ (HM.elems (usnInstances us)) $ \u →
                forM_ derivedKeys $ \k → HM.member k (uisStats u) `shouldBe` False
            -- the Nominal mind holds none of the mind keys; the pin is kept
            let u2 = usnInstances us HM.! UnitId 2
            forM_ mindKeys $ \k → HM.member k (uisStats u2) `shouldBe` False
            HM.lookup "mind" (uisPhysioOverrides u2) `shouldBe` Just PhysioNominal
            -- the episode survives verbatim
            let u1 = usnInstances us HM.! UnitId 1
            forM_ [("mental_state", 2), ("mental_until", 170), ("mental_break_behavior", 3)] $
                \(k, v) → HM.lookup k (uisStats u1) `shouldBe` Just v

    it "reads the canonical derived values on the first paused observation, with \
       \no tick and no stored cache, for both modes" $
        withScene [ (unitSpec 1 "acolyte" (Just "rich")) { usStats = [("blood_oxygen", 0.65)] }
                  , (unitSpec 2 "acolyte" (Just "rich"))
                      { usOverrides = [("consciousness", PhysioNominal)]
                      , usStats = [("blood_oxygen", 0.65)] } ] $ \(env, ls) → do
            _ ← luaText ls "__resources.onSaveLoaded({ 1, 2 }, {}); return true"
            luaNum ls "unit.readPhysio(1, 'consciousness')" >>= near 1e-5 0.5
            luaNum ls "unit.readPhysio(2, 'consciousness')" >>= near 1e-9 1
            i1 ← readInstance env 1
            forM_ derivedKeys $ \k → HM.member k (uiStats i1) `shouldBe` False

-----------------------------------------------------------------------
-- Pins between the data, the Lua and the Haskell
-----------------------------------------------------------------------

pinsSpec ∷ Spec
pinsSpec = describe "the constants the registry and the rules share with the Lua" $
    it "pins MENTAL_CHECK_INTERVAL to mental.TUNE.CHECK_INTERVAL, the EUPHORIC \
       \code to the rule's, and the euphoria bonus to the rule alone" $
        withScene [ unitSpec 1 "acolyte" (Just "rich") ] $ \(_, ls) → do
            reg ← readShippedPhysiologyRegistry
            fmap realToFrac (Map.lookup "MENTAL_CHECK_INTERVAL" (prConstants reg))
                `shouldBe` Just (8.0 ∷ Double)
            luaNum ls "require('scripts.mental_state').TUNE.CHECK_INTERVAL" >>= near 1e-9 8
            luaNum ls "require('scripts.mental_state').EUPHORIC" >>= near 1e-9 3
            luaText ls "return tostring(require('scripts.mental_state').TUNE\
                       \.EUPHORIA_CONCENTRATION_BONUS)" `shouldReturn` "nil"
