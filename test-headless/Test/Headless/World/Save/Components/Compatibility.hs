{-# LANGUAGE ScopedTypeVariables #-}
-- | The compatibility owner of the "save components" gate (issue
--   #760, split out under #2043): pre-#1233 item payloads, the frozen
--   entity/worldgen/item DTOs, the tracked component fixture, the B1
--   migration, and 'World.Page.Types.WorldPageId' serialization
--   compatibility. Pure -- no engine, no IO; every
--   'World.Save.Component.Session.SessionSnapshot' here is a synthetic
--   literal and the tracked bytes are an inline hex literal.
--
--   Composed by the facade 'Test.Headless.World.Save.Components', which
--   is the only module @test-headless/Spec.hs@ registers.
module Test.Headless.World.Save.Components.Compatibility
    (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import qualified Data.Text as T
import Numeric (readHex)

import qualified Data.HashSet as HS
import World.Save.Envelope
import World.Save.Compat.MetadataV1 (SaveMetadataV1(..))
import World.Save.Envelope.Codec (encodeEnvelope)
import World.Save.Envelope.Types (defaultEnvelopeLimits, ComponentId(..))
import World.Save.Component.Types
import World.Flora.Reference (FloraRef(..))
import World.Flora.Types (FloraId(..))
import World.Save.Component.Page
import World.Save.Component.Entities
import World.Save.Component.Knowledge
    ( containerKnowledgeCodec, ContainerKnowledgeDTO(..)
    , ContainerKnowledgeDTOv1(..), PageContainerKnowledgeDTOv1(..)
    , PageContainerKnowledgeDTO(..), ContainerRecordDTO(..)
    , toContainerRecordDTOv1 )
import World.Save.Compat.SessionV90
import Language.Generated.Types (LanguageProvenance(..))
import World.Save.Reference (SamePageRef(..))
import World.Save.Snapshot
import Test.Headless.Location.Fixture (expectGeometry)
import Location.Types (emptyLocationRegistry)
import World.Save.Types
    ( SaveMetadata(..), BuildingInstanceSnapshot(..)
    , UnitInstanceSnapshot(..), MissingDefRef(..), renderMissingDefRef
    , MissingItemDefRef(..), MissingRecipeRef(..)
    , MissingBillOutputItemRef(..), MissingConstructDefRef(..)
    , renderMissingItemDefRef, renderMissingRecipeRef
    , renderMissingBillOutputItemRef, renderMissingConstructDefRef
    , MissingMaterialRef(..), renderMissingMaterialRef
    , MissingFloraRef(..), renderMissingFloraRef, MissingLocationRef(..)
    , renderMissingLocationRef, MissingInfectionRef(..)
    , renderMissingInfectionRef, resolveLegacyLocationParams )
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..), WorldIdentity(..))
import Item.Ground (emptyGroundItems, GroundItems(..), GroundItem(..))
import Item.Types (ItemInstance(..))
import World.Flora.Harvest (emptyFloraHarvests)
import World.Construct.Attempt (firstConstructAttemptId)
import World.Time.Types (CalendarConfig(..))
import World.Weather.Types
    ( ClimateState(..), ClimateGrid(..), ClimateCoord(..), RegionClimate(..)
    , SeasonalClimate(..), OceanGrid(..), OceanCell(..), OceanCurrent(..)
    , AtmoGrid(..), PressureSystem(..), PressureType(..), SurfaceType(..)
    , SurfaceBudget(..), initClimateState
    , defaultRegionClimate, emptyOceanGrid, emptyAtmoGrid )
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Unit.Sim.Types
    (UnitSimState(..), MoveTarget(..), Pose(..), UnitActivity(..)
    , MoveHazardPolicy(..))
import Unit.Direction (Direction(..))
import Building.Knowledge (ContainerRecord(..))
import Test.Headless.World.Save.Components.Fixture

spec ∷ Spec
spec = do
    -- #1233 appended physical values (external bulk + optional internal
    -- storage capacities) to the recursive item tree, which FOUR
    -- independently versioned components carry. Each therefore bumped and
    -- froze its previous tree, and each must still decode a real
    -- NON-EMPTY historical payload: a component whose migration were
    -- wired to the wrong DTO, or forgotten, would only show up on a
    -- payload that actually holds items.
    --
    -- These drive each component's REAL 'ccDecode' at the OLD version
    -- over bytes encoded from the frozen v1 tree, rather than calling the
    -- migration function directly — so the version dispatch, the frozen
    -- layout and the migration are all exercised together, exactly as a
    -- shipped save exercises them.
    describe "pre-#1233 item payloads still decode (all four components)" $ do
        let expectDecode label = either
                (\e → do expectationFailure
                             (label ⧺ ": " ⧺ T.unpack (renderComponentError e))
                         pure Nothing)
                (pure ∘ Just)

        it "world-activity accepts v1 and v2, and a ground item's whole \
           \contents tree decodes with its physical values absent" $ do
            let legacy = WorldActivityDTOv2
                    [ (toActivityV2 page1)
                        { pad2GroundItems = toGroundItemsDTOv1
                            (GroundItems 2 (HM.singleton 1
                                (GroundItem richItem 3.5 4.5))) } ]
                bytes = S.encode legacy
            ccInputVers worldActivityCodec `shouldBe` [1, 2, 3, 4, 5, 6]
            forM_ [1, 2] $ \ver → do
                mv ← expectDecode ("v" ⧺ show ver)
                          (ccDecode worldActivityCodec ver bytes)
                case mv of
                    Nothing → pure ()
                    Just (WorldActivityDTO slices) → do
                        let gs = concatMap
                                   (HM.elems ∘ gisiItems ∘ padGroundItems)
                                   slices
                        map giiX gs `shouldBe` [3.5]
                        -- Every level of the recursive tree, not just the
                        -- root: three items in richItem's kit-in-kit.
                        concatMap (physicals ∘ giiInst) gs `shouldBe`
                            replicate 3 (Nothing, Nothing)
                        -- The rest of the item survives untouched.
                        map (fromItemInstanceDTO ∘ giiInst) gs `shouldBe`
                            [stripPhysicals richItem]

        it "buildings accepts v1, migrating BOTH a delivered-materials \
           \item and a loose-storage item" $ do
            let inst = (minimalBuildingInstance [richItem])
                    { bisMaterialsDelivered =
                        HM.singleton "steel_bar" [richItem] }
                bytes = S.encode (BuildingsDTOv1
                    [ PageBuildingsDTOv1 page1
                        (HM.singleton (BuildingId 1)
                            (toBuildingInstanceDTOv1 inst)) ])
            ccInputVers buildingsCodec `shouldBe` [1, 2]
            mv ← expectDecode "buildings v1"
                     (ccDecode buildingsCodec 1 bytes)
            case mv of
                Nothing → pure ()
                Just (BuildingsDTO slices) → do
                    let insts = concatMap (HM.elems ∘ pbInstances) slices
                    concatMap (concatMap physicals ∘ bidStorage) insts
                        `shouldBe` replicate 3 (Nothing, Nothing)
                    concatMap (concatMap physicals ∘ concat ∘ HM.elems
                               ∘ bidMaterialsDelivered) insts
                        `shouldBe` replicate 3 (Nothing, Nothing)
                    map fromBuildingInstanceDTO insts `shouldBe`
                        [ inst { bisStorage = [stripPhysicals richItem]
                               , bisMaterialsDelivered = HM.singleton
                                   "steel_bar" [stripPhysicals richItem] } ]

        it "units accepts v1, migrating inventory, equipment AND \
           \accessories" $ do
            let inst = (minimalUnitInstance [richItem])
                    { uisEquipped = HM.singleton "head" richItem
                    , uisAccessories = [richItem] }
                bytes = S.encode (UnitsDTOv1
                    [ PageUnitsDTOv1 page1
                        (HM.singleton (UnitId 1)
                            (toUnitInstanceDTOv1 inst)) ])
            ccInputVers unitsCodec `shouldBe` [1, 2]
            mv ← expectDecode "units v1" (ccDecode unitsCodec 1 bytes)
            case mv of
                Nothing → pure ()
                Just (UnitsDTO slices) → do
                    let insts = concatMap (HM.elems ∘ puInstances) slices
                        allItems u = uidInventory u
                                       ⧺ HM.elems (uidEquipped u)
                                       ⧺ uidAccessories u
                    -- Three containers × three nesting levels.
                    concatMap (concatMap physicals ∘ allItems) insts
                        `shouldBe` replicate 9 (Nothing, Nothing)
                    map fromUnitInstanceDTO insts `shouldBe`
                        [ inst { uisInventory = [stripPhysicals richItem]
                               , uisEquipped = HM.singleton "head"
                                   (stripPhysicals richItem)
                               , uisAccessories =
                                   [stripPhysicals richItem] } ]

        it "container-knowledge accepts v1 — being OPTIONAL governs \
           \ABSENCE, not migration of a payload that IS present" $ do
            let rec' = ContainerRecord
                    { crItems = [richItem], crStoredWeight = 12.5
                    , crRevealedAt = 99.25 }
                bytes = S.encode (ContainerKnowledgeDTOv1
                    [ PageContainerKnowledgeDTOv1 page1
                        (HM.singleton (BuildingId 1)
                            (toContainerRecordDTOv1 rec')) ])
            ccInputVers containerKnowledgeCodec `shouldBe` [1, 2]
            mv ← expectDecode "container-knowledge v1"
                     (ccDecode containerKnowledgeCodec 1 bytes)
            case mv of
                Nothing → pure ()
                Just (ContainerKnowledgeDTO slices) → do
                    let recs = concatMap (HM.elems ∘ pckRecords) slices
                    concatMap (concatMap physicals ∘ crdItems) recs
                        `shouldBe` replicate 3 (Nothing, Nothing)
                    -- The remembered scalars are observations and must
                    -- not be re-derived.
                    map crdStoredWeight recs `shouldBe` [12.5]
                    map crdRevealedAt recs `shouldBe` [99.25]

        it "a CURRENT-version payload round-trips the physical values \
           \for real, so absence is the migration's answer and not the \
           \codec's" $ do
            -- The counterpart to the four cases above: if the current
            -- shape silently dropped bulk/storage, every one of them
            -- would still pass.
            let bytes = S.encode (WorldActivityDTO
                    [ (toActivity page1)
                        { padGroundItems = toGroundItemsDTO
                            (GroundItems 2 (HM.singleton 1
                                (GroundItem richItem 3.5 4.5))) } ])
            mv ← expectDecode "world-activity v3"
                     (ccDecode worldActivityCodec 3 bytes)
            case mv of
                Nothing → pure ()
                Just (WorldActivityDTO slices) →
                    map (fromItemInstanceDTO ∘ giiInst)
                        (concatMap (HM.elems ∘ gisiItems ∘ padGroundItems)
                                   slices)
                        `shouldBe` [richItem]

    describe "frozen entity DTOs (requirement 4)" $ do
        -- The mutable runtime STATE records (UnitSimState, CraftBill,
        -- PowerNode) are never embedded directly; each has a distinct,
        -- component-owned DTO with an explicit field-by-field conversion.
        -- These prove the conversion is lossless (identity) on a
        -- non-trivial value, so a change to the live record surfaces as a
        -- compile error in the conversion, never as silent v1 byte drift.
        it "UnitSimStateDTO round-trips a non-default sim state" $
            fromUnitSimStateDTO (toUnitSimStateDTO richSimState)
                `shouldBe` richSimState

        it "BillQueueDTO round-trips a non-empty craft-bill queue" $
            fromBillQueueDTO (toBillQueueDTO richBills) `shouldBe` richBills

        it "NodeRegistryDTO round-trips a non-empty power-node registry" $
            fromNodeRegistryDTO (toNodeRegistryDTO richNodes) `shouldBe` richNodes

        it "migrates an unambiguous v1 unit-sim page slice into the typed \
           \current shape (issue #764 round-3 review: psSim's map KEY is a \
           \same-page cross-component reference to its owning unit, typed \
           \the same way as a bill's station or a node's host building)" $ do
            let v1 = PageSimDTOv1
                    { ps1PageId = page1
                    , ps1Sim = HM.singleton (UnitId 5)
                        (toUnitSimStateDTOv1 richSimState) }
                v2 = migratePageSimDTOv1 v1
            psPageId v2 `shouldBe` page1
            HM.keys (psSim v2) `shouldBe` [SamePageRef (UnitId 5)]
            HM.lookup (SamePageRef (UnitId 5)) (psSim v2)
                `shouldBe` Just (migrateUnitSimStateDTOv1
                                    (toUnitSimStateDTOv1 richSimState))

        it "migrateUnitSimDTOv1 migrates every page slice in a v1 payload" $
            let v1 = UnitSimDTOv1
                    [ PageSimDTOv1 page1 (HM.singleton (UnitId 5)
                        (toUnitSimStateDTOv1 richSimState))
                    , PageSimDTOv1 page2 HM.empty ]
                UnitSimDTO v2Pages = migrateUnitSimDTOv1 v1
            in map psPageId v2Pages `shouldBe` [page1, page2]

        -- #1217: the unit-sim component is at v3, and BOTH older shapes
        -- must land on the fall-permitted default a pre-#1217 target was
        -- written under. richSimState's own target is FallProhibited, so
        -- a migration that simply carried the current policy through
        -- (rather than defaulting) would pass the round-trip test above
        -- and fail these.
        it "migrates a v1 unit-sim target to the fall-permitted default" $
            let v1 = PageSimDTOv1 page1 (HM.singleton (UnitId 5)
                        (toUnitSimStateDTOv1 richSimState))
                migrated = HM.lookup (SamePageRef (UnitId 5))
                                     (psSim (migratePageSimDTOv1 v1))
            in hazardOf migrated `shouldBe` Just FallPermitted

        it "migrates a v2 unit-sim page slice, defaulting its target's \
           \hazard policy to fall-permitted" $ do
            let v2 = PageSimDTOv2
                    { ps2PageId = page1
                    , ps2Sim = HM.singleton (SamePageRef (UnitId 5))
                        (toUnitSimStateDTOv1 richSimState) }
                v3 = migratePageSimDTOv2 v2
            psPageId v3 `shouldBe` page1
            HM.keys (psSim v3) `shouldBe` [SamePageRef (UnitId 5)]
            let migrated = HM.lookup (SamePageRef (UnitId 5)) (psSim v3)
            hazardOf migrated `shouldBe` Just FallPermitted
            -- Everything else carries across untouched.
            fmap simLocalPath migrated `shouldBe` Just (usLocalPath richSimState)

        it "migrateUnitSimDTOv2 migrates every page slice in a v2 payload" $
            let v2 = UnitSimDTOv2
                    [ PageSimDTOv2 page1 (HM.singleton (SamePageRef (UnitId 5))
                        (toUnitSimStateDTOv1 richSimState))
                    , PageSimDTOv2 page2 HM.empty ]
                UnitSimDTO v3Pages = migrateUnitSimDTOv2 v2
            in map psPageId v3Pages `shouldBe` [page1, page2]

        it "a v1 move target loses nothing but its (absent) policy" $
            let current = toUnitSimStateDTO richSimState
                back    = migrateUnitSimStateDTOv1 (toUnitSimStateDTOv1 richSimState)
                permitted = fmap (\t → t { mvtHazard = FallPermitted })
                                 (simTarget current)
            in back `shouldBe` current { simTarget = permitted }

    describe "frozen worldgen + item DTOs (boundary rule, review round 6)" $ do
        -- The nested worldgen config/state records and the recursive
        -- ItemInstance are no longer embedded live: each has a frozen DTO
        -- with an explicit conversion. These prove those conversions are
        -- lossless (identity) on non-trivial values, so a live-record field
        -- change surfaces as a compile error in a conversion rather than as
        -- silent v1 byte drift.
        it "ItemInstanceDTO round-trips a recursive (kit-in-kit) item" $
            fromItemInstanceDTO (toItemInstanceDTO richItem) `shouldBe` richItem

        it "GroundItemDTO round-trips a ground item carrying a recursive item" $
            let g = GroundItem richItem 3.5 4.5
            in fromGroundItemDTO (toGroundItemDTO g) `shouldBe` g

        it "WorldGenParamsDTO round-trips a populated worldgen config/climate \
           \tree (frozen nested records, no live embedding)" $
            fromWorldGenParamsDTO (toWorldGenParamsDTO richGenParams)
                `shouldBe` richGenParams

        it "a planted nested climate/calendar value survives the DTO round \
           \trip (recursion is lossless, not merely structural)" $ do
            let gp' = fromWorldGenParamsDTO (toWorldGenParamsDTO richGenParams)
            ccDaysPerMonth (wgpCalender gp') `shouldBe` 40
            csGlobalTemp (wgpClimateState gp') `shouldBe` 14.5
            (rcHumidity <$> HM.lookup (ClimateCoord 1 2)
                (cgRegions (csClimate (wgpClimateState gp'))))
                `shouldBe` Just 0.42

    describe "frozen tracked fixture" $
        it "decodes a frozen, tracked multi-component byte fixture -- not \
           \merely this test's own encoder output -- proving the component \
           \envelope round-trips from real stored bytes" $ do
            let bytes = hexDecode trackedComponentFixtureHex
            -- #2021: these bytes carry @"metadata"@ v2 and @world-pages@
            -- v1, both of which predate generated-world identity, so
            -- their migrations leave it absent on BOTH sides rather than
            -- inventing one. The expectation is the live value with the
            -- ids stripped, for the same reason the language-provenance
            -- expectation below strips those: a drop anywhere ELSE still
            -- fails the comparison.
            decodeSaveEnvelopeMetadata HS.empty bytes
                `shouldBe` Right (withoutGeneratedWorldIds richMeta)
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Left err → expectationFailure (T.unpack err)
                Right (meta, snap, _luaComponents, isMigrated) → do
                    meta `shouldBe` withoutGeneratedWorldIds richMeta
                    -- These are pre-#911 bytes (@world-pages@ v1), so
                    -- their pages come back with the old per-chunk
                    -- location flags PENDING rather than an instance
                    -- table -- exactly the state the load path then
                    -- resolves against the location registry before
                    -- publication. Applying that same resolution here
                    -- (this fixture places no locations, so the registry
                    -- is empty) is what makes the comparison against the
                    -- current snapshot shape meaningful rather than
                    -- vacuous.
                    -- They also predate #1092, so their identities come
                    -- back with language provenance ABSENT, never
                    -- inferred -- which is why the expectation strips it
                    -- from the current in-memory snapshot rather than the
                    -- fixture being re-cut. This is requirement 3 proven
                    -- against REAL historical bytes.
                    resolveFixturePages snap
                        `shouldBe` withoutPageGeneratedIds
                                       (withoutLanguageProvenance richSnapshot)
                    languageProvenanceOf snap page1 `shouldBe` Nothing
                    isMigrated `shouldBe` False

    -- | Issue #766 (save-overhaul C4) completes what #760's acceptance
    --   explicitly deferred: "preserve a fixture for the transitional
    --   payload only if it is deliberately supported by an explicit
    --   migration; otherwise document and test its intentional
    --   incompatibility." A real B1-era envelope (metadata + a single
    --   required @"session"@ component, no gameplay components at all)
    --   is now RECOGNIZED and migrated, both for full decode
    --   ('decodeSessionEnvelope') and metadata-only listing
    --   ('decodeSaveEnvelopeMetadata', what 'World.Save.Serialize.listSaves'
    --   calls) — a B1-era save is listable and loadable again, reporting
    --   its true, then-authoritative metadata unchanged.
    --   See "Test.Headless.World.Save.Compat" for the frozen fixture
    --   coverage (a real B1 envelope recovered from git history).
    describe "B1 -> current migration (issue #766, save-overhaul C4)" $ do
        it "a hand-built B1-shaped envelope (metadata + a required \
           \'session' component) migrates and lists under the current \
           \build rather than being rejected as unknown" $ do
            let b1Meta = minimalSaveMetadataV90
                b1Specs =
                    [ (metadataComponentId, legacyMetadataComponentVersion, True
                      , S.encode minimalSaveMetadataV1)
                    , (ComponentId "session", 90, True
                      , S.encode minimalSaveDataV90) ]
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion b1Specs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            decodeSaveEnvelopeMetadata HS.empty bytes `shouldBe` Right b1Meta
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Left err → expectationFailure (T.unpack err)
                Right (meta, snap, luaComponents, isMigrated) → do
                    meta `shouldBe` b1Meta
                    luaComponents `shouldBe` []
                    isMigrated `shouldBe` True
                    snapActivePage snap `shouldBe` WorldPageId "main_world"

        it "refuses to migrate a B1 session carrying non-empty legacy Lua \
           \module state, rather than silently discarding it (requirement \
           \7: the pre-#761 Lua deserializer that could interpret it is \
           \gone, so there is no honest translation left)" $ do
            let luaSd = minimalSaveDataV90
                    { sd90LuaModules = HM.singleton "unit_ai" "opaque legacy blob" }
            case migrateSessionV90 minimalSaveMetadataV90 luaSd of
                Right _   → expectationFailure
                    "expected non-empty legacy Lua state to be rejected"
                Left errs → do
                    errs `shouldSatisfy` (not . null)
                    map cePhase errs `shouldSatisfy` all (≡ MigratePhase)

    -- #1091: 'WorldPageId' grew a field label so the nine hand-written
    -- @where unWorldPageId (WorldPageId t) = t@ clauses in
    -- 'World.Save.Types' (plus the two named accessors) could go. That
    -- is a pure cleanup ONLY if three observable things are unchanged:
    -- every render string, the stock 'Show' representation the save
    -- validation diagnostics and 'Sim.Thread' print through 'tshow',
    -- and the wire bytes 'deriving newtype Serialize' produces.
    describe "WorldPageId accessor cleanup (#1091)" $ do
        -- The expected strings below are written out in full rather
        -- than rebuilt from the record's own fields, so a change to
        -- either side of a render function fails this gate.
        it "renderMissingDefRef is unchanged" $
            renderMissingDefRef MissingDefRef
                { mdrKind = "building", mdrPage = page1
                , mdrEntity = 7, mdrDefName = "ghost_building" }
                `shouldBe`
                "building #7 on page 'page1' references unknown \
                \definition 'ghost_building'"

        it "renderMissingItemDefRef is unchanged" $
            renderMissingItemDefRef MissingItemDefRef
                { midrSource = "unit inventory", midrPage = page1
                , midrItemId = 42, midrDefName = "ghost_item" }
                `shouldBe`
                "unit inventory item #42 on page 'page1' references \
                \unknown item definition 'ghost_item'"

        it "renderMissingRecipeRef is unchanged" $
            renderMissingRecipeRef MissingRecipeRef
                { mrrPage = page2, mrrBillId = 3, mrrRecipe = "ghost_recipe" }
                `shouldBe`
                "craft bill #3 on page 'page2' references unknown \
                \recipe 'ghost_recipe'"

        it "renderMissingBillOutputItemRef is unchanged" $
            renderMissingBillOutputItemRef MissingBillOutputItemRef
                { mbirPage = page1, mbirBillId = 9, mbirDefName = "ghost_output" }
                `shouldBe`
                "craft bill #9 on page 'page1' references unknown \
                \output item definition 'ghost_output'"

        it "renderMissingConstructDefRef is unchanged" $
            renderMissingConstructDefRef MissingConstructDefRef
                { mcdPage = page1, mcdTile = (1, 2), mcdDefName = "ghost_bldg" }
                `shouldBe`
                "construct designation at (1,2) on page 'page1' \
                \references unknown building definition 'ghost_bldg'"

        it "renderMissingMaterialRef is unchanged" $
            renderMissingMaterialRef MissingMaterialRef
                { mmrSource = "edit log", mmrPage = page2
                , mmrCoord = (-3, 4), mmrMatId = 200 }
                `shouldBe`
                "edit log at (-3,4) on page 'page2' references unknown \
                \material id 200"

        it "renderMissingFloraRef names the AUTHORED SPECIES a save \
           \recorded (#2243)" $
            renderMissingFloraRef MissingFloraRef
                { mfrSource = "crop plot", mfrPage = page1
                , mfrCoord = (5, -6)
                , mfrSpecies = FloraByName "moonpetal" }
                `shouldBe`
                "crop plot at (5,-6) on page 'page1' references unknown \
                \species 'moonpetal'"

        it "renderMissingFloraRef falls back to the ORDINAL for a \
           \pre-name payload, never to an invented name (#2243, D-2)" $
            renderMissingFloraRef MissingFloraRef
                { mfrSource = "plant designation", mfrPage = page1
                , mfrCoord = (5, -6)
                , mfrSpecies = FloraByLegacyId (FloraId 77) }
                `shouldBe`
                "plant designation at (5,-6) on page 'page1' references \
                \unknown legacy species id 77"

        it "renderMissingLocationRef is unchanged" $
            renderMissingLocationRef MissingLocationRef
                { mlrPage = page2, mlrCoord = (0, 1), mlrLocId = "ghost_loc" }
                `shouldBe`
                "location overlay chunk (0,1) on page 'page2' references \
                \unknown location id 'ghost_loc'"

        it "renderMissingInfectionRef is unchanged" $
            renderMissingInfectionRef MissingInfectionRef
                { mirPage = page1, mirUnitId = 12
                , mirWoundPart = "left_arm", mirInfType = "ghost_rot" }
                `shouldBe`
                "unit #12 wound (left_arm) on page 'page1' references \
                \unknown infection id 'ghost_rot'"

        -- The field label is deliberately NOT reflected in 'Show': the
        -- record-syntax derivation would print
        -- @WorldPageId {unWorldPageId = "page1"}@, and page ids reach
        -- diagnostics through 'tshow'.
        it "Show still prints the unlabelled constructor application" $ do
            show page1 `shouldBe` "WorldPageId \"page1\""
            showsPrec 11 page1 "" `shouldBe` "(WorldPageId \"page1\")"

        it "the accessor reads back exactly what the constructor wrapped" $
            unWorldPageId (WorldPageId "main_world") `shouldBe` "main_world"

        -- Requirement 4: a field label changes nothing about what
        -- @deriving newtype Serialize@ derives, which is why no save
        -- version moved with this cleanup.
        it "encodes byte-identically to the underlying Text" $
            S.encode (WorldPageId "main_world")
                `shouldBe` S.encode ("main_world" ∷ Text)

-- Helpers -----------------------------------------------------------

-- | The hazard policy of a decoded sim state's in-flight target, if any
--   (#1217). Spelled once so the migration cases below read as
--   assertions rather than as nested Maybe plumbing.
hazardOf ∷ Maybe UnitSimStateDTO → Maybe MoveHazardPolicy
hazardOf mSim = fmap mvtHazard (simTarget =<< mSim)

-- | And the SAME values again as the frozen v1 @"metadata"@ component
--   payload (#913). A hand-built B1 envelope must carry METADATA v1 as
--   well as its v90 session: a real B1 file was written while the
--   metadata component was still at v1, and the B1 recognizer pins that
--   historical version deliberately
--   ('World.Save.Envelope.legacyMetadataComponentVersion'), so a
--   metadata bump can never stop this build recognizing its own frozen
--   baseline. Decoding it must yield 'minimalSaveMetadataV90' exactly —
--   including @smAutosave = False@, the documented "legacy saves are
--   manual saves" answer 'migrateSaveMetadataV1' supplies.
minimalSaveMetadataV1 ∷ SaveMetadataV1
minimalSaveMetadataV1 = SaveMetadataV1
    { sm1Name = "b1-hand-built", sm1Seed = wgpSeed defaultGP
    , sm1WorldSize = wgpWorldSize defaultGP, sm1PlateCount = wgpPlateCount defaultGP
    , sm1Timestamp = "2026-07-16T00:00:00.000000Z"
    , sm1WorldName = Nothing, sm1WorldGloss = Nothing }

-- | A sim state with distinctive values in a spread of fields (incl.
--   the nested MoveTarget, an enum, and the climb/fall tuples) so a
--   dropped or mis-mapped field in the DTO conversion would show up.
richSimState ∷ UnitSimState
richSimState = minimalSimState
    { usRealX = 3.5, usRealY = -2.25, usGridZ = 4, usRealZ = 4.5
    , usTarget = Just (MoveTarget 9 10 1.5 FallProhibited)
    , usPose = Climbing, usState = Running, usFacing = DirNE
    , usLocalPath = [(1,2),(3,4)]
    , usDrinkUntil = Just 12.5, usTransitionStride = 2
    , usPostTransition = [Crawling, Standing]
    , usClimbFromTile = Just (1,2,3), usClimbToTile = Just (4,5,6)
    , usPendingClimbXP = 0.75, usPendingFallDrop = Just 2
    , usJumpApex = Just 1.25, usMoveGrade = 0.5 }

-- #1233 pre-#1233-payload helpers ------------------------------------

-- | An empty CURRENT activity slice for a page — the shape
-- 'worldActivityCodec' writes.
toActivity ∷ WorldPageId → PageActivityDTO
toActivity pid = PageActivityDTO
    { padPageId        = pid
    , padConstructNextAttempt = firstConstructAttemptId
    , padMine          = HM.empty
    , padConstruct     = HM.empty
    , padChop          = HM.empty
    , padTill          = HM.empty
    , padPlant         = HM.empty
    , padFloraHarvests = emptyFloraHarvests
    , padCropPlots     = HM.empty
    , padGroundItems   = toGroundItemsDTO emptyGroundItems
    , padSpoilPiles    = HM.empty
    , padPendingChop   = HM.empty
    , padPendingHarvests = HM.empty
    }

-- | The same at the FROZEN pre-#1233 layout (@world-activity@ v1/v2).
toActivityV2 ∷ WorldPageId → PageActivityDTOv2
toActivityV2 pid = PageActivityDTOv2
    { pad2PageId        = pid
    , pad2Mine          = HM.empty
    , pad2Construct     = HM.empty
    , pad2Chop          = HM.empty
    , pad2Till          = HM.empty
    , pad2Plant         = HM.empty
    , pad2FloraHarvests = HM.empty
    , pad2CropPlots     = HM.empty
    , pad2GroundItems   = toGroundItemsDTOv1 emptyGroundItems
    , pad2SpoilPiles    = HM.empty
    }

-- | Every level of a decoded item tree's physical values, root first —
--   so an assertion covers the WHOLE recursion rather than only the item
--   a migration happened to touch at the top.
physicals ∷ ItemInstanceDTO → [(Maybe Float, Maybe ItemStorageDTO)]
physicals d =
    (itdBulk d, itdStorage d) : concatMap physicals (itdContents d)

-- | The same live item with its physical values cleared everywhere — what
--   a migrated pre-#1233 instance must equal, field for field.
stripPhysicals ∷ ItemInstance → ItemInstance
stripPhysicals i = i
    { iiBulk = Nothing, iiStorage = Nothing
    , iiContents = map stripPhysicals (iiContents i) }

-- | 'WorldGenParams' with distinctive values planted across the newly
--   FROZEN nested worldgen records (a non-default calendar; a climate
--   state carrying a populated region, ocean cell, named current,
--   pressure system, and surface budget) so a mis-mapped field in ANY
--   of the recursive climate/config DTO conversions is observable.
--   'canon' reaches the manual-Serialize fixpoint (see its note) — the
--   volcano ctx is rebuilt from seed/size/plates, which these edits leave
--   untouched, so the planted climate/calendar values survive it.
richClimate ∷ ClimateState
richClimate = (initClimateState 64)
    { csGlobalCO2  = 1.3, csGlobalTemp = 14.5, csSolarConst = 0.98
    , csClimate = ClimateGrid
        (HM.singleton (ClimateCoord 1 2)
            defaultRegionClimate { rcHumidity = 0.42
                                 , rcAirTemp  = SeasonalClimate 20 5 }) 4
    , csOcean = emptyOceanGrid
        { ogCells = HM.singleton (ClimateCoord 0 1)
            (OceanCell (SeasonalClimate 18 12) 34.5 200 1.1 0.3 0.2 0.05)
        , ogCurrents = [OceanCurrent "Gyre" [ClimateCoord 0 0] True 0.6] }
    , csAtmo = emptyAtmoGrid
        { agSystems = [PressureSystem (ClimateCoord 2 2) HighPressure 3 0.4] }
    , csSurface = HM.singleton (ClimateCoord 1 1)
        (SurfaceBudget SurfDesert 0.35 (-0.2) 0.1 0.0) }

richGenParams ∷ WorldGenParams
richGenParams = canon defaultWorldGenParams
    { wgpSeed        = 777
    , wgpCalender    = CalendarConfig 40 10 20 50
    , wgpClimateState = richClimate }

hexDecode ∷ String → BS.ByteString
hexDecode = BS.pack . go
  where
    go (a:b:rest) = case readHex [a,b] of
        ((v,_):_) → v : go rest
        []        → error ("hexDecode: not a hex byte: " <> [a,b])
    go _          = []

-- | The load path's own pre-#911 location resolution
--   ('World.Save.Types.resolveLegacyLocations'), applied to every page
--   of a decoded snapshot.
resolveFixturePages ∷ SessionSnapshot → SessionSnapshot
resolveFixturePages snap = snap
    { snapPages = HM.map resolvePage (snapPages snap) }
  where
    resolvePage p = p
        { pgsGenParams = expectGeometry
            (resolveLegacyLocationParams emptyLocationRegistry
                                         (pgsGenParams p)) }

-- | The expectation for a fixture whose bytes predate #1092: identical
--   to the live snapshot except that no identity carries language
--   provenance. Written as a transformation of the SAME expected value
--   (rather than a second hand-maintained fixture) so a drop anywhere
--   ELSE in the page still fails the comparison.
withoutLanguageProvenance ∷ SessionSnapshot → SessionSnapshot
withoutLanguageProvenance snap = snap
    { snapPages = HM.map stripPage (snapPages snap) }
  where
    stripPage p = p
        { pgsIdentity = (\i → i { wiLanguage = Nothing }) <$> pgsIdentity p }

-- | The expectation for a fixture whose bytes predate #2021: identical
--   except that the save declares no generated-world ids. A pre-v3
--   @"metadata"@ payload migrates with an EMPTY inventory, never an
--   invented one.
withoutGeneratedWorldIds ∷ SaveMetadata → SaveMetadata
withoutGeneratedWorldIds meta = meta { smGeneratedWorldIds = [] }

-- | The page-side half of the same expectation: a pre-v9 @world-pages@
--   payload's pages carry NO generated-world id, and load staging is
--   what mints one — so the decoded snapshot legitimately differs from
--   the live one in exactly this field and no other.
withoutPageGeneratedIds ∷ SessionSnapshot → SessionSnapshot
withoutPageGeneratedIds snap = snap
    { snapPages = HM.map (\p → p { pgsGeneratedId = Nothing })
                         (snapPages snap) }

languageProvenanceOf
    ∷ SessionSnapshot → WorldPageId → Maybe LanguageProvenance
languageProvenanceOf snap pid =
    wiLanguage =≪ (pgsIdentity =≪ HM.lookup pid (snapPages snap))

trackedComponentFixtureHex ∷ String
trackedComponentFixtureHex =
    "53595241000000010000000000000212000000000000000b000000000000\
    \00096275696c64696e677300000001010000000000000000000000000000\
    \00973dafc93879ea3b82000000000000000c636f72652d73657373696f6e\
    \00000001010000000000000097000000000000005574d3010096cbbe2b00\
    \0000000000000b63726166742d62696c6c73000000010100000000000000\
    \ec000000000000003abeec8f6ff4c58c2600000000000000086d65746164\
    \617461000000010100000000000001260000000000000051cda064806651\
    \992f000000000000000b706f7765722d6e6f646573000000010100000000\
    \00000177000000000000003abeec8f6ff4c58c26000000000000000f7465\
    \78747572652d70616c65747465000000010100000000000001b100000000\
    \0000001088201fb960ff64650000000000000008756e69742d73696d0000\
    \00010100000000000001c1000000000000007b81797b8874157310000000\
    \0000000005756e6974730000000101000000000000023c00000000000000\
    \f9fc6ed2ffd1c79265000000000000000e776f726c642d61637469766974\
    \790000000101000000000000033500000000000000c2251087e70708d624\
    \000000000000000b776f726c642d65646974730000000101000000000000\
    \03f700000000000000321ed7627acac89064000000000000000b776f726c\
    \642d70616765730000000101000000000000042900000000000004f8b7df\
    \1cb66e09260ff8327e7c13afca0300000000000000020000000000000005\
    \7061676531000000000000000100000001000000000000000d746573745f\
    \6275696c64696e6700000000000000000000000000000000000000000000\
    \000000000000000000000000000000000001000000000000000100000000\
    \0000000042c8000000000000000000000000000000000000000000000000\
    \000570616765320000000000000000404500000000000000000000000000\
    \640000000a0000000a000000000000000570616765310000000000000001\
    \00000000000000057061676531010000000000000005706167653140e000\
    \004100000040400000030000000000000002000000000000000570616765\
    \310000000000000000000000010000000000000005706167653200000000\
    \00000000000000010000000000000004736c6f74000000000001e2400000\
    \000000000080000000000000000a00000000000000027473010000000000\
    \00000a5269636820576f726c640100000000000000076120676c6f737300\
    \000000000000020000000000000005706167653100000000000000000000\
    \000100000000000000057061676532000000000000000000000001000000\
    \000000000000000000000000000000000000000002000000000000000570\
    \616765310000000000000001000000010000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000005706167653200000000000000000000000000000002000000000000\
    \000570616765310000000000000001000000010000000000000009746573\
    \745f756e69743f8000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000000000000469646c6500000000\
    \000000087374616e64696e67000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000000000000040a0000000000000\
    \000000000000000000000005706167653200000000000000000000000000\
    \000002000000000000000570616765310000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000570616765320000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000200000000000000057061676531\
    \000000000000000000000000000000057061676532000000000000000000\
    \0000000000000200000000000000057061676531000000000001e2400000\
    \000000000080000000000000000a0000000000000000000000000000001e\
    \000000000000000c0000000000000018000000000000003c3ecccccd3f00\
    \0000000000000000001c0000000000000000000000000000000000000080\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000010000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000323f8000003e99999a3f3333333fc000003f80\
    \00003f0000003f8333330000000000000000000000000000002000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \3f800000000000003f8000003f3333333fa0000000000000000000060000\
    \000000000016000000000000000c3f8000003f8000003f80000000000000\
    \000000010000000000000002000000000000000100000000000000030000\
    \000000000001000000000000000300000000000000010000000000000003\
    \000000000000000000000000000000000000000000000000000000000000\
    \00000000000000000000000000000000000c000000000000000000000000\
    \00000001000000000000000100000000000000010001000000000000000a\
    \5269636820576f726c640100000000000000076120676c6f737300000000\
    \000000057061676532000000000000002a00000000000000800000000000\
    \00000a0000000000000000000000000000001e000000000000000c000000\
    \0000000018000000000000003c3ecccccd3f000000000000000000001c00\
    \000000000000000000000000000000000000800000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000100\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \323f8000003e99999a3f3333333fc000003f8000003f0000003f83333300\
    \000000000000000000000000000020000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000003f800000000000003f8000\
    \003f3333333fa00000000000000000000600000000000000160000000000\
    \00000c3f8000003f8000003f800000000000000000000100000000000000\
    \020000000000000001000000000000000300000000000000010000000000\
    \000003000000000000000100000000000000030000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \0000000000000c0000000000000000000000000000000100000000000000\
    \0100000000000000010000"
