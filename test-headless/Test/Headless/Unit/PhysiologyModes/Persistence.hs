{-# LANGUAGE Strict, OverloadedStrings #-}
-- | Persistence of the "physiology stat modes" group (#2831; design D-6,
--   I6, T-10, T-11): the deterministic load normalization, the round trips
--   through the production codec, and the persisted/rebuilt classification
--   those rest on.
--
--   __Two halves.__ The pure half drives 'physiologyLoadGate' — the exact
--   function the load gate calls before anything is staged — table-driven
--   over D-6's cases (b), (c), (d), (f), (g), (h), (i) and (j), plus the
--   T-10 default-change cases and idempotence. The engine half builds units
--   with the seeding rule spawn uses, ticks them through the SHIPPED Lua,
--   takes them through the real session codec and back, and compares what
--   comes out: durable values and overrides bit-identical, the rebuilt
--   caches equal to their canonical recomputation, nothing normalized under
--   unchanged content.
module Test.Headless.Unit.PhysiologyModes.Persistence (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sort)
import Data.IORef (readIORef, writeIORef)
import Engine.Core.State (EngineEnv(..))
import Test.Headless.Unit.PhysiologyFixture
    ( exemptPhysiology, profilePhysiology, readShippedPhysiologyRegistry
    , richPhysiology )
import Test.Headless.Unit.PhysiologyModes.Harness
import Test.Headless.World.Save.Components.Fixture
    ( minimalGlobals, minimalPage, minimalSimState, minimalUnitInstance
    , page1 )
import Unit.Physiology.Types
import Unit.Types
import World.Page.Types (WorldPageId)
import Equipment.Types (emptyEquipmentClassManager)
import Infection.Types (emptyInfectionManager)
import Item.Types (emptyItemManager)
import World.Save.Envelope (decodeSessionEnvelope, encodeSessionSnapshot)
import World.Save.Physiology
    ( PhysiologyNormalization(..), physiologyLoadGate, rebuiltCacheStats
    , withoutRebuiltCaches )
import World.Save.Snapshot
    ( PageSnapshot(..), SessionSnapshot(..), captureSessionSnapshot )
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotSaveMetadata)
import World.Save.Types
    ( UnitInstanceSnapshot(..), UnitSnapshot(..), fromUnitSnapshot, toUnitSnapshot )

-----------------------------------------------------------------------
-- Pure fixtures
-----------------------------------------------------------------------

-- | A saved unit of the named definition with the given stats and
--   overrides.
saved ∷ Text → [(Text, Float)] → [(Text, PhysioMode)] → UnitInstanceSnapshot
saved def stats overrides = (minimalUnitInstance [])
    { uisDefName = def, uisStats = HM.fromList stats
    , uisPhysioOverrides = HM.fromList overrides }

-- | The 70 kg reference body's unowned inputs (frame 70 kg, no owned keys).
body70 ∷ [(Text, Float)]
body70 = bodyStats acolyteBody

-- | The same body for a unit whose stamina is Nominal (the exempt tier):
--   a Nominal system holds no @stamina@ key, so a canonical save has none.
bodyNominalStamina ∷ [(Text, Float)]
bodyNominalStamina = filter ((≢ "stamina") ∘ fst) body70

-- | The Simulated keys a rich 70 kg acolyte is saved with.
seeds70 ∷ [(Text, Float)]
seeds70 =
    [ ("hydration", 42), ("hunger", 700), ("calories", 1400), ("salt", 70)
    , ("salt_store", 140), ("core_temp", 37) ] ⧺ poolSeeds

-- | The exhaustion and sleep pools (#2835) of an endurance-1 unit, whose
--   Simulated systems save them at the full healthy value (endurance x 10).
poolSeeds ∷ [(Text, Float)]
poolSeeds = [("exhaustion", 10), ("sleep_pressure", 10)]

defsOf ∷ [(Text, UnitPhysiology)] → HM.HashMap Text UnitPhysiology
defsOf = HM.fromList

pageOf ∷ [(Word32, UnitInstanceSnapshot)] → [(WorldPageId, UnitSnapshot)]
pageOf us = [(page1, UnitSnapshot
    { usnInstances = HM.fromList [ (UnitId n, u) | (n, u) ← us ]
    , usnNextId = 100 })]

-- | Run the gate over one unit (id 1) and answer the normalized unit and
--   its notes, or the rejection messages.
gate1 ∷ PhysiologyRegistry → [(Text, UnitPhysiology)] → UnitInstanceSnapshot
      → Either [Text] (UnitInstanceSnapshot, [Text])
gate1 reg defs u = case physiologyLoadGate reg (defsOf defs) 0 (pageOf [(1, u)]) of
    Left msgs → Left msgs
    Right ([(_, us)], notes) → case HM.lookup (UnitId 1) (usnInstances us) of
        Just u' → Right (u', concatMap pnNotes notes)
        Nothing → Left ["unit vanished"]
    Right _ → Left ["unexpected page shape"]

statOf' ∷ UnitInstanceSnapshot → Text → Maybe Float
statOf' u k = HM.lookup k (uisStats u)

shouldNormalize ∷ Either [Text] (UnitInstanceSnapshot, [Text])
                → ((UnitInstanceSnapshot, [Text]) → Expectation) → Expectation
shouldNormalize (Left msgs) _ = expectationFailure ("rejected: " <> show msgs)
shouldNormalize (Right r) check = check r

shouldReject ∷ Either [Text] a → [Text] → Expectation
shouldReject (Right _) _ = expectationFailure "expected a rejection"
shouldReject (Left msgs) needles =
    forM_ needles $ \n →
        (n, any (n `T.isInfixOf`) msgs) `shouldBe` (n, True)

spec ∷ Spec
spec = describe "persistence of the physiology modes" $ do
    beforeAll readShippedPhysiologyRegistry $ do
        canonicalSpec
        normalizationSpec
        policySpec
    engineSpec

-----------------------------------------------------------------------
-- Unchanged content: zero normalizations
-----------------------------------------------------------------------

canonicalSpec ∷ SpecWith PhysiologyRegistry
canonicalSpec = describe "unchanged content" $
    it "normalizes a canonical save zero times and returns it untouched, \
       \for every profile and a bodyless unit" $ \reg → do
        reduced ← profilePhysiology "reduced"
        let squirrelBodyStats = bodyStats squirrelBody
            units =
                [ ("acolyte", richPhysiology, saved "acolyte" (body70 ⧺ seeds70) [])
                , ("red_squirrel", reduced
                  , saved "red_squirrel" (squirrelBodyStats ⧺
                        [ ("salt", 0.3), ("salt_store", 0.6), ("core_temp", 37) ]
                        ⧺ poolSeeds) [])
                , ("nomad_primitive", exemptPhysiology
                  , saved "nomad_primitive" bodyNominalStamina [])
                , ("white_tailed_deer", noUnitPhysiology
                  , saved "white_tailed_deer" [("height", 1)] []) ]
        forM_ units $ \(name, up, u) → do
            gate1 reg [(name, up)] u `shouldBe` Right (u, [])

-----------------------------------------------------------------------
-- D-6, case by case
-----------------------------------------------------------------------

normalizationSpec ∷ SpecWith PhysiologyRegistry
normalizationSpec = describe "D-6 normalization" $ do

    describe "(b) mode and key disagreement" $ do
        it "scrubs the keys a Nominal or Unsupported system holds, logging them" $ \reg → do
            reduced ← profilePhysiology "reduced"
            let u = saved "red_squirrel"
                    (bodyStats squirrelBody ⧺
                        [ ("salt", 0.3), ("salt_store", 0.6), ("core_temp", 37)
                        , ("hydration", 0.1), ("hunger", 2), ("calories", 4)
                        , ("hypothermia", 0.2) ] ⧺ poolSeeds) []
            shouldNormalize (gate1 reg [("red_squirrel", reduced)] u) $ \(u', notes) → do
                forM_ ["hydration", "hunger", "calories"] $ \k →
                    statOf' u' k `shouldBe` Nothing
                -- hypothermia belongs to thermo (Simulated) and stays
                statOf' u' "hypothermia" `shouldBe` Just 0.2
                statOf' u' "salt" `shouldBe` Just 0.3
                length notes `shouldBe` 2          -- hydration and nutrition
                all (any (`T.isInfixOf` T.unwords notes)) [["hydration"], ["nutrition"]]
                    `shouldBe` True

        it "scrubs a bodyless unit's legacy physiology keys once, then leaves it alone" $ \reg → do
            let u = saved "white_tailed_deer"
                    [("height", 1), ("core_temp", 37), ("hydration", 5), ("salt_conc", 1)] []
            shouldNormalize (gate1 reg [("white_tailed_deer", noUnitPhysiology)] u) $
                \(u', notes) → do
                    HM.keys (uisStats u') `shouldBe` ["height"]
                    length notes `shouldBe` 3     -- thermo, hydration, and salt's derived key
                    gate1 reg [("white_tailed_deer", noUnitPhysiology)] u'
                        `shouldBe` Right (u', [])

        it "seeds only the MISSING keys of a Simulated system and keeps present values exactly" $ \reg → do
            -- a pre-#2761 save: salt stored, no reserve
            let u = saved "acolyte" (body70 ⧺ filter ((≢ "salt_store") ∘ fst) seeds70 ⧺ [])
                    []
                u2 = u { uisStats = HM.insert "salt" 33 (uisStats u) }
            shouldNormalize (gate1 reg [("acolyte", richPhysiology)] u2) $ \(u', notes) → do
                statOf' u' "salt" `shouldBe` Just 33              -- exactly as stored
                statOf' u' "salt_store" `shouldBe` Just 140       -- the full reserve
                length notes `shouldBe` 1

        it "seeds hydration before salt when one pass seeds both, so salt starts at its water" $ \reg → do
            let u = saved "acolyte" body70 []                     -- no owned key at all
            shouldNormalize (gate1 reg [("acolyte", richPhysiology)] u) $ \(u', _) → do
                statOf' u' "hydration" `shouldBe` Just 42
                statOf' u' "salt" `shouldBe` Just 70              -- maxSalt x water fraction 1.0
                statOf' u' "salt_store" `shouldBe` Just 140
                statOf' u' "core_temp" `shouldBe` Just 37
                statOf' u' "hunger" `shouldBe` Just 700
                statOf' u' "calories" `shouldBe` Just 1400

    describe "(c) renamed and retired systems" $ do
        let withDeclarations reg = reg
                { prRetired = ["legacy_water"]
                , prSystems = [ if psId s ≡ "hydration"
                                  then s { psRenamedFrom = ["thirst"] } else s
                              | s ← prSystems reg ] }

        it "remaps an override for a renamed system, and drops one for a retired system" $ \reg0 → do
            let reg = withDeclarations reg0
                u = saved "acolyte" (body70 ⧺ seeds70)
                    [("thirst", PhysioNominal), ("legacy_water", PhysioSimulated)]
            shouldNormalize (gate1 reg [("acolyte", richPhysiology)] u) $ \(u', notes) → do
                HM.toList (uisPhysioOverrides u') `shouldBe` [("hydration", PhysioNominal)]
                length notes `shouldSatisfy` (>= 2)
                -- hydration is now Nominal by the remapped override: its key goes
                statOf' u' "hydration" `shouldBe` Nothing

        it "lets a successor's own override win over the renamed one" $ \reg0 → do
            let reg = withDeclarations reg0
                u = saved "acolyte" (body70 ⧺ seeds70)
                    [("thirst", PhysioNominal), ("hydration", PhysioSimulated)]
            shouldNormalize (gate1 reg [("acolyte", richPhysiology)] u) $ \(u', _) →
                HM.toList (uisPhysioOverrides u') `shouldBe` [("hydration", PhysioSimulated)]

    describe "recognized aliases are resolved before any mode-sensitive validation" $ do
        let renamed reg = reg
                { prSystems = [ if psId sys ≡ "thermo"
                                  then sys { psRenamedFrom = ["temperature"] } else sys
                              | sys ← prSystems reg ] }
        it "rejects a non-finite value the REMAPPED override makes Simulated" $ \reg0 → do
            -- thermo defaults Nominal (exempt); the saved override names the old id
            let reg = renamed reg0
                u = saved "nomad_primitive" (body70 ⧺ [("core_temp", 1 / 0)])
                    [("temperature", PhysioSimulated)]
            gate1 reg [("nomad_primitive", exemptPhysiology)] u `shouldReject` ["core_temp"]

        it "does not reject a value the REMAPPED Nominal override scrubs" $ \reg0 → do
            -- thermo defaults Simulated (rich); the old id's override pins it Nominal
            let reg = renamed reg0
                u = saved "acolyte" (body70 ⧺ filter ((≢ "core_temp") ∘ fst) seeds70 ⧺ [("core_temp", 1 / 0)])
                    [("temperature", PhysioNominal)]
            shouldNormalize (gate1 reg [("acolyte", richPhysiology)] u) $ \(u', _) → do
                statOf' u' "core_temp" `shouldBe` Nothing
                HM.toList (uisPhysioOverrides u') `shouldBe` [("thermo", PhysioNominal)]

    describe "(d) an undeclared system" $
        it "still rejects the whole load" $ \reg → do
            let u = saved "acolyte" (body70 ⧺ seeds70) [("plumbing", PhysioSimulated)]
            gate1 reg [("acolyte", richPhysiology)] u `shouldReject` ["plumbing"]

    describe "(f) a Simulated override for a system the definition no longer supports" $
        it "rejects, naming the unit and the system; a Nominal one is only intent and stays" $ \reg → do
            let u = saved "white_tailed_deer" [("height", 1)] [("hydration", PhysioSimulated)]
            gate1 reg [("white_tailed_deer", noUnitPhysiology)] u
                `shouldReject` ["unit #1", "white_tailed_deer", "hydration"]
            let n = saved "white_tailed_deer" [("height", 1)] [("hydration", PhysioNominal)]
            shouldNormalize (gate1 reg [("white_tailed_deer", noUnitPhysiology)] n) $
                \(n', _) → HM.toList (uisPhysioOverrides n') `shouldBe` [("hydration", PhysioNominal)]

    describe "(g) an invalid seed or upkeep input" $ do
        it "rejects a Simulated unit whose missing key cannot be seeded: absent, zero or non-finite maximum" $ \reg → do
            forM_ [ ("no maximum", filter ((≢ "max_hydration") ∘ fst) body70)
                  , ("zero maximum", ("max_hydration", 0) : filter ((≢ "max_hydration") ∘ fst) body70)
                  , ("NaN maximum", ("max_hydration", 0 / 0) : filter ((≢ "max_hydration") ∘ fst) body70) ] $
                \(label, stats) →
                    ( label
                    , either (const True) (const False)
                        (gate1 reg [("acolyte", richPhysiology)]
                            (saved "acolyte" (filter ((≢ "hydration") ∘ fst) stats) []))
                    ) `shouldBe` (label, True)
            gate1 reg [("acolyte", richPhysiology)]
                (saved "acolyte" (filter ((≢ "max_hydration") ∘ fst) body70) [])
                `shouldReject` ["unit #1", "acolyte", "hydration", "max_hydration"]

        it "rejects a Nominal-nutrition unit whose upkeep frame is zero, NaN, or missing with no height" $ \reg → do
            forM_ [ ("zero frame", ("frame_mass", 0) : filter ((≢ "frame_mass") ∘ fst) body70)
                  , ("NaN frame", ("frame_mass", 0 / 0) : filter ((≢ "frame_mass") ∘ fst) body70)
                  , ("no frame, no height"
                    , filter (\(k, _) → k ∉ ["frame_mass", "height"]) body70) ] $
                \(label, stats) →
                    ( label
                    , either (const True) (const False)
                        (gate1 reg [("nomad_primitive", exemptPhysiology)]
                            (saved "nomad_primitive" stats []))
                    ) `shouldBe` (label, True)
            gate1 reg [("nomad_primitive", exemptPhysiology)]
                (saved "nomad_primitive" (("frame_mass", 0) : filter ((≢ "frame_mass") ∘ fst) body70) [])
                `shouldReject` ["unit #1", "nomad_primitive", "nutrition", "frame_mass"]

    describe "(h) a non-finite Simulated value" $
        it "still rejects" $ \reg →
            gate1 reg [("acolyte", richPhysiology)]
                (saved "acolyte" (body70 ⧺ filter ((≢ "salt") ∘ fst) seeds70 ⧺ [("salt", 0 / 0)]) [])
                `shouldReject` ["salt"]

    describe "(i) a legacy stored salt_conc" $
        it "is scrubbed in every mode and logged" $ \reg → do
            reduced ← profilePhysiology "reduced"
            forM_ [ ("rich", "acolyte", richPhysiology, body70 ⧺ seeds70)
                  , ("exempt", "nomad_primitive", exemptPhysiology, bodyNominalStamina)
                  , ("reduced", "red_squirrel", reduced
                    , bodyStats squirrelBody ⧺ [("salt", 0.3), ("salt_store", 0.6), ("core_temp", 37)]
                        ⧺ poolSeeds) ] $
                \(label, name, up, stats) →
                    shouldNormalize (gate1 reg [(name, up)] (saved name (("salt_conc", 0.123) : stats) [])) $
                        \(u', notes) → do
                            (label, statOf' u' "salt_conc") `shouldBe` (label, Nothing)
                            (label, length notes) `shouldBe` (label, 1)

    describe "(j) a pre-frame unit" $
        it "gets frame_mass = 22 x height^2 and keeps its legacy floors exactly; a second load changes nothing" $ \reg → do
            let h = 1.8 ∷ Float
                preFrame = saved "acolyte"
                    ( [ ("height", h), ("body_mass", 70), ("fat_mass", 14), ("lean_mass", 56)
                      , ("endurance", 1), ("stamina", 10) ]
                      ⧺ seeds70 ) []
            shouldNormalize (gate1 reg [("acolyte", richPhysiology)] preFrame) $ \(u', notes) → do
                let frame = fromMaybe (-1) (statOf' u' "frame_mass")
                abs (frame - 22 * h * h) `shouldSatisfy` (< 1e-4)
                -- the legacy floors (0.44 h^2 fat, 4.4 h^2 lean) are the frame floors
                abs (0.02 * frame - 0.44 * h * h) `shouldSatisfy` (< 1e-5)
                abs (0.20 * frame - 4.4 * h * h) `shouldSatisfy` (< 1e-4)
                length notes `shouldBe` 1
                gate1 reg [("acolyte", richPhysiology)] u' `shouldBe` Right (u', [])

    describe "idempotence" $
        it "a compounded legacy unit normalizes once and a second pass finds nothing" $ \reg → do
            let u = saved "acolyte"
                    ( [ ("height", 1.8), ("body_mass", 70), ("fat_mass", 14), ("lean_mass", 56)
                      , ("max_hydration", 42), ("max_hunger", 700), ("max_calories", 1400)
                      , ("endurance", 1), ("stamina", 10)
                      , ("salt", 30), ("salt_conc", 0.4), ("core_temp", 37) ] ) []
            shouldNormalize (gate1 reg [("acolyte", richPhysiology)] u) $ \(u', notes) → do
                notes `shouldSatisfy` (not ∘ null)
                gate1 reg [("acolyte", richPhysiology)] u' `shouldBe` Right (u', [])

-----------------------------------------------------------------------
-- T-10: defaults and pins across a save and a load
-----------------------------------------------------------------------

policySpec ∷ SpecWith PhysiologyRegistry
policySpec = describe "T-10 overrides across save and load" $ do

    it "makes an UNPINNED unit follow a changed species default: reduced -> rich seeds, rich -> exempt scrubs" $ \reg → do
        reduced ← profilePhysiology "reduced"
        let wasReduced = saved "red_squirrel"
                (bodyStats squirrelBody ⧺ [("salt", 0.3), ("salt_store", 0.6), ("core_temp", 37)]
                    ⧺ poolSeeds) []
        shouldNormalize (gate1 reg [("red_squirrel", richPhysiology)] wasReduced) $ \(u', _) → do
            statOf' u' "hydration" `shouldSatisfy` isJust
            statOf' u' "calories" `shouldSatisfy` isJust
            uisPhysioOverrides u' `shouldBe` HM.empty
        let wasRich = saved "acolyte" (body70 ⧺ seeds70) []
        shouldNormalize (gate1 reg [("acolyte", exemptPhysiology)] wasRich) $ \(u', _) →
            forM_ ["hydration", "salt", "core_temp", "calories"] $ \k →
                statOf' u' k `shouldBe` Nothing
        -- (reduced is only a fixture for the above)
        reduced `seq` pure ()

    it "lets a PIN survive a changed default, in either direction" $ \reg → do
        -- pinned Nominal under a Simulated default: its keys stay scrubbed
        let pinned = saved "acolyte" (body70 ⧺ filter ((≢ "hydration") ∘ fst) seeds70)
                [("hydration", PhysioNominal)]
        gate1 reg [("acolyte", richPhysiology)] pinned `shouldBe` Right (pinned, [])
        -- pinned Simulated under a Nominal default: its keys stay
        let pinnedSim = saved "nomad_primitive" (bodyNominalStamina ⧺ [("hydration", 42)])
                [("hydration", PhysioSimulated)]
        gate1 reg [("nomad_primitive", exemptPhysiology)] pinnedSim `shouldBe` Right (pinnedSim, [])

    it "keeps an override EQUAL to the species default exactly, as intent" $ \reg → do
        let sameAsDefault = saved "acolyte" (body70 ⧺ seeds70) [("hydration", PhysioSimulated)]
        gate1 reg [("acolyte", richPhysiology)] sameAsDefault
            `shouldBe` Right (sameAsDefault, [])

-----------------------------------------------------------------------
-- Engine and the production codec
-----------------------------------------------------------------------

-- | Put the scene's units on the page a save captures (the harness ticks
--   them on its own active page; a save snapshots one named page).
onSavePage ∷ EngineEnv → IO ()
onSavePage env = forM_ [1, 2, 3] $ \i → modifyInstance env i (\u → u { uiPage = page1 })

-- | The page snapshot of the engine's live units.
pageSnapshotOf ∷ EngineEnv → IO PageSnapshot
pageSnapshotOf env = do
    um ← readIORef (unitManagerRef env)
    let snap = toUnitSnapshot page1 um
    pure (minimalPage page1)
        { pgsUnits = snap
        , pgsUnitSimStates = HM.fromList [ (uid, minimalSimState) | uid ← HM.keys (usnInstances snap) ] }

-- | Through the REAL session codec and back.
codecRoundTrip ∷ PageSnapshot → IO PageSnapshot
codecRoundTrip pg = case captureSessionSnapshot minimalGlobals [pg] of
    Left errs → do
        expectationFailure (show errs)
        pure pg
    Right snap →
        let meta = snapshotSaveMetadata
                (SaveRequestMeta { srmSlotName = "slot", srmTimestamp = "ts"
                                 , srmAutosave = False }) snap
        in case decodeSessionEnvelope HS.empty HS.empty (encodeSessionSnapshot meta snap []) of
            Left err → do
                expectationFailure (show err)
                pure pg
            Right (_, back, _, _) → case HM.lookup page1 (snapPages back) of
                Just p → pure p
                Nothing → do
                    expectationFailure "page missing after the round trip"
                    pure pg

engineSpec ∷ Spec
engineSpec = describe "through the real codec and the shipped Lua" $ do

    it "excludes from the save comparison exactly the caches the inventory classifies serialized-but-ignored, and nothing durable" $ do
        inventory ← TIO.readFile "docs/persistence_state_inventory.md"
        let rows = [ l | l ← T.lines inventory
                       , "Rebuild (serialized-but-ignored)" `T.isInfixOf` l ]
            named = concat [ T.splitOn "`, `" (T.dropAround (≡ '`') firstCell)
                           | l ← rows
                           , (_ : firstCell : _) ← [T.splitOn "|" l]
                           , let _ = firstCell ]
            clean = map (T.strip ∘ T.filter (≢ '`')) named
        sort clean `shouldBe` sort rebuiltCacheStats
        -- and the erasure leaves a durable stat and an override alone
        let u = saved "acolyte" [("hydration", 21), ("consciousness", 0.5), ("concentration", 0.7)]
                    [("hydration", PhysioSimulated)]
            snapOf us = (minimalPage page1) { pgsUnits = us }
            sess = captureSessionSnapshot minimalGlobals [snapOf (UnitSnapshot (HM.fromList [(UnitId 1, u)]) 10)]
        case sess of
            Left errs → expectationFailure (show errs)
            Right sn → case HM.lookup page1 (snapPages (withoutRebuiltCaches sn)) of
                Just pg → fmap (\i → (uisStats i, uisPhysioOverrides i))
                              (HM.lookup (UnitId 1) (usnInstances (pgsUnits pg)))
                    `shouldBe` Just (HM.fromList [("hydration", 21)], HM.fromList [("hydration", PhysioSimulated)])
                Nothing → expectationFailure "page missing"

    it "round-trips ticked units bit-identically, normalizes nothing, and rebuilds the caches canonically \
       \(saved right after a post-salt hydration drain and a body recompute)" $ do
        reg ← readShippedPhysiologyRegistry
        withScene [ unitSpec 1 "acolyte" (Just "rich")
                  , (unitSpec 2 "red_squirrel" (Just "reduced")) { usBody = squirrelBody }
                  , unitSpec 3 "nomad_primitive" (Just "exempt") ] $ \(env, ls) → do
            climate ls 25 0.4
            -- hydration drains AFTER salts and the brain tick inside each pass,
            -- and surplus regrowth recomputes the body: the stored caches are
            -- stale at the instant of the save
            runSeconds ls 20 0.1
            onSavePage env
            before ← readIORef (unitManagerRef env)
            pg ← pageSnapshotOf env
            back ← codecRoundTrip pg
            -- durable values and overrides: exactly what was saved
            usnInstances (pgsUnits back) `shouldBe` usnInstances (pgsUnits pg)
            -- the gate normalizes nothing under unchanged content
            let defs = HM.map udPhysiology (umDefs before)
            physiologyLoadGate reg defs 0 [(page1, pgsUnits back)]
                `shouldBe` Right ([(page1, pgsUnits back)], [])
            -- caches: load the ORIGINAL and the ROUND-TRIPPED unit through the
            -- load hook; both rebuild to the same canonical values
            let cacheKeys = [ "circulation", "consciousness", "concentration"
                            , "state_of_mind", "strength", "strength_body"
                            , "max_hydration", "max_hunger", "max_calories"
                            , "carrying_capacity", "heart_rate" ]
                readCaches uid = forM cacheKeys $ \k → luaNum ls
                    ("unit.getStat(" <> tshow uid <> ", '" <> k <> "') or -1")
                rebuild = do
                    _ ← luaText ls "__resources.onSaveLoaded({ 1, 2, 3 }, {}); return true"
                    mapM readCaches [1, 2, 3 ∷ Int]
            writeIORef (unitManagerRef env) before
            fromOriginal ← rebuild
            -- the round-tripped units, restored the way load restores them
            let (restored, orphans, _, _, _) = fromUnitSnapshot page1 (umDefs before)
                    emptyInfectionManager emptyEquipmentClassManager
                    emptyItemManager (pgsUnits back)
            orphans `shouldBe` []
            writeIORef (unitManagerRef env) restored
            fromBack ← rebuild
            fromBack `shouldBe` fromOriginal

    it "reads a restored nomad's stale survival state as healthy before the first frame, \
       \because the gate scrubs a Nominal unit's stored keys (#2754's case, moved here)" $ do
        reg ← readShippedPhysiologyRegistry
        withScene [unitSpec 1 "nomad_primitive" (Just "exempt")] $ \(env, ls) → do
            onSavePage env
            um ← readIORef (unitManagerRef env)
            let stale = HM.fromList
                    [ ("core_temp", 43), ("salt", 5), ("salt_conc", 0.2), ("hyperthermia", 0.9)
                    , ("hypothermia", 0.3), ("salt_imbalance", 0.9), ("hydration", 0)
                    , ("consciousness", 0.1), ("state_of_mind", 0.1), ("sepsis", 0.4) ]
                snap = toUnitSnapshot page1 um
                snapStale = snap { usnInstances = HM.map
                    (\u → u { uisStats = HM.union stale (uisStats u) }) (usnInstances snap) }
                defs = HM.map udPhysiology (umDefs um)
            case physiologyLoadGate reg defs 0 [(page1, snapStale)] of
                Left msgs → expectationFailure (show msgs)
                Right ([(_, us)], notes) → do
                    length notes `shouldBe` 1
                    forM_ (HM.elems (usnInstances us)) $ \u → do
                        forM_ nominalKeys $ \k → HM.lookup k (uisStats u) `shouldBe` Nothing
                        -- an injury meter and the cache are not survival physiology
                        HM.lookup "sepsis" (uisStats u) `shouldBe` Just 0.4
                    -- load it, run the load hook and read: healthy, no tick yet
                    let (restored, _, _, _, _) = fromUnitSnapshot page1 (umDefs um)
                            emptyInfectionManager emptyEquipmentClassManager
                            emptyItemManager us
                    writeIORef (unitManagerRef env) restored
                    _ ← luaText ls "__resources.onSaveLoaded({ 1 }, {}); return true"
                    readOwned ls 1 "core_temp" `shouldReturn` "37|nominal"
                    readOwned ls 1 "salt_conc" `shouldReturn` "1|nominal"
                    luaNum ls "unit.getStat(1, 'consciousness')" >>= near 1e-9 1
                Right _ → expectationFailure "unexpected page shape"
