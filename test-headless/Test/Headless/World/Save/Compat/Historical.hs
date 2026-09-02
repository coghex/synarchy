-- | The historical-DTO family of the "save migrations" gate (issue
--   #766, save-overhaul C4; split out by #2094): the frozen B1/v90
--   session contract — proved against the REAL tracked B1 bytes in
--   "Test.Headless.World.Save.Compat.B1Fixture", never this suite's
--   own encoder output — and every historical @world-pages@ shape from
--   v1 to v8 (language provenance #1092, stored location and river
--   names #1101/#1102, etymology sources #1104, the retired discovery
--   margin #1230, and encounters #916). Pure — no engine, no IO.
--
--   Each describe group is exported on its own so the aggregate
--   ("Test.Headless.World.Save.Compat") can sequence it among the other
--   families' groups in the order the suite has always run in; this
--   module registers nothing itself. The frozen page-core fixtures in
--   every historical shape live here because only this family decodes
--   them.
module Test.Headless.World.Save.Compat.Historical
    ( frozenV90Spec
    , languageProvenanceSpec
    ) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Serialize as S
import qualified Data.Text as T

import World.Save.Envelope (decodeSaveEnvelopeMetadata, decodeSessionEnvelope)
import World.Save.Component.Types (ComponentError(..))
import World.Save.Compat.SessionV90
import World.Save.Types (SaveMetadata(..))
import Location.Bounds (AbsBounds(..))
import World.Chunk.Types (ChunkCoord(..))
import Location.Instance
    ( LocationEncounter(..), LocationEncounterOccupant(..)
    , LocationInstance(..), LocationInstances(..), LocationInstanceId(..)
    , LocationLifecycle(..), LocationSignificantItem(..), instancesToList )
import World.Save.Snapshot (SessionSnapshot(..), PageSnapshot(..))
import World.Save.Component.Page
    ( fromWorldGenParamsDTOv1, toWorldGenParamsDTOv1, toWorldGenParamsDTO
    , toWorldGenParamsDTOv2
    , PageCoreDTO(..), WorldPagesDTO(..), PageCoreDTOv1(..)
    , WorldPagesDTOv1(..), PageCoreDTOv2(..), WorldPagesDTOv2(..)
    , PageCoreDTOv3(..), WorldPagesDTOv3(..)
    , toWorldGenParamsDTOv3
    , PageCoreDTOv4(..), WorldPagesDTOv4(..)
    , toWorldGenParamsDTOv4
    , PageCoreDTOv5(..), WorldPagesDTOv5(..)
    , PageCoreDTOv6(..), WorldPagesDTOv6(..)
    , PageCoreDTOv7(..), WorldPagesDTOv7(..)
    , PageCoreDTOv8(..), WorldPagesDTOv8(..)
    , WorldGenParamsDTOv5(..), toWorldGenParamsDTOv5
    , toWorldGenParamsDTOv6
    , WorldGenParamsDTOv7(..), toWorldGenParamsDTOv7
    , WorldPages(..), WorldIdentityDTO(..), WorldIdentityDTOv1(..)
    , WorldIdentityDTOv2(..)
    , LanguageProvenanceDTO(..), toEtymologySourceDTO, basePageSnapshots
    , migrateWorldPagesV1, migrateWorldPagesV2, migrateWorldPagesV3
    , migrateWorldPagesV4, migrateWorldPagesV5, migrateWorldPagesV6
    , migrateWorldPagesV7, migrateWorldPagesV8 )
import World.Save.Component.WorldGen
    ( LocationInstanceDTOv3(..), LocationInstancesDTOv3(..)
    , LocationInstanceDTOv5(..), LocationInstancesDTOv5(..)
    , LocationEncounterDTOv1(..)
    , toLocationInstancesDTOv3, toLocationInstancesDTOv5, toRiverNamesDTO )
import Language.Etymology.Source (EtymologySource(..))
import Language.Semantic.Types (ConceptId(..), NameExpr(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import Language.Generated.Types
    (LanguageProvenance(..), LangSeed(..), GeneratorVersion(..))
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Base (GeoFeatureId(..))
import World.River.Naming (RiverName(..), RiverNames(..), riverNamesToList)
import World.Page.Types (WorldPageId(..), WorldIdentity(..))
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Craft.Bills (BillId(..), BillMode(..))
import World.Save.Component.Entities (BillQueueDTOv1(..), CraftBillDTOv1(..))
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)
import Test.Headless.World.Save.Compat.B1Fixture
    (fixtureBytes, extractSessionPayload, minimalSaveMetadataForExtra)

-- | The frozen B1/v90 session contract (issue #766, save-overhaul C4),
--   proved against the REAL tracked B1 bytes.
frozenV90Spec ∷ Spec
frozenV90Spec =
    describe "frozen v90 DTO (issue #766, save-overhaul C4)" $ do
        it "decodes the real, tracked B1 envelope fixture's metadata \
           \component (not merely this test's own encoder output)" $
            decodeSaveEnvelopeMetadata HS.empty fixtureBytes `shouldBe` Right
                SaveMetadata
                    { smName = "envelope_test_save", smSeed = 42
                    , smWorldSize = 64, smPlateCount = 3
                    , smTimestamp = "2026-07-16T00:00:00.000000Z"
                    , smWorldName = Just "Test World"
                    , smWorldGloss = Just "a fixture world", smAutosave = False
                    -- #2021: a v1 metadata payload predates generated
                    -- world identity; the migration leaves it empty
                    -- rather than inventing an id the file never had.
                    , smGeneratedWorldIds = []
                    }

        it "decodes the real, tracked B1 envelope fixture's session \
           \component into the frozen SaveDataV90 shape — proving byte \
           \compatibility with a genuine pre-#760 save, not a value this \
           \test itself encoded" $
            case decodeSessionV90 (extractSessionPayload fixtureBytes) of
                Left err → expectationFailure (show err)
                Right sd → do
                    -- Round-17 review: sd90Metadata was previously never
                    -- asserted at all -- its own decoded VALUE is unused
                    -- by migrateSessionV90 (the real metadata comes from
                    -- the envelope's separate "metadata" component
                    -- instead), so a positional wire-layout regression on
                    -- SaveMetadataV90's frozen shape would have decoded
                    -- successfully into garbage with nothing to notice.
                    sd90Metadata sd `shouldBe` SaveMetadataV90
                        { sm90Name = "envelope_test_save", sm90Seed = 42
                        , sm90WorldSize = 64, sm90PlateCount = 3
                        , sm90Timestamp = "2026-07-16T00:00:00.000000Z"
                        , sm90WorldName = Just "Test World"
                        , sm90WorldGloss = Just "a fixture world"
                        }
                    sd90ActivePage sd `shouldBe` WorldPageId "main_world"
                    sd90VisiblePages sd `shouldBe` [WorldPageId "main_world"]
                    sd90EnginePaused sd `shouldBe` True
                    sd90LuaModules sd `shouldBe` HM.empty
                    map wp90PageId (sd90Worlds sd)
                        `shouldBe` [WorldPageId "main_world"]
                    case sd90Worlds sd of
                        (p:_) → do
                            let gp = fromWorldGenParamsDTOv1 (wp90GenParams p)
                            wgpSeed gp `shouldBe` 42
                            wgpWorldSize gp `shouldBe` 128
                            wgpPlateCount gp `shouldBe` 10
                        [] → expectationFailure "expected one page"

        it "the full migration path correctly rejects this specific \
           \fixture's own pre-existing metadata/gameplay mismatch \
           \(requirement 12 applies uniformly to a migrated legacy \
           \session — this fixture's manifest metadata was never \
           \required to agree with its gameplay gen params, since B1 \
           \predates that check entirely)" $
            case decodeSessionEnvelope HS.empty HS.empty fixtureBytes of
                Right _   → expectationFailure
                    "expected the pre-existing metadata/gameplay mismatch \
                    \to be rejected"
                Left msg  → msg `shouldSatisfy` T.isInfixOf "disagrees"

        it "migrateSessionV90 rejects a B1 save with DUPLICATE page ids \
           \(round-14 review) -- basePageSnapshots' HashMap.fromList over \
           \the raw page list would otherwise silently COLLAPSE two \
           \same-id pages into one before any CROSS-component check ever \
           \saw the duplication (validateSessionSnapshot only ever \
           \inspects the already-collapsed map), so only a component-\
           \local validator running on the raw page list first can catch \
           \it -- exactly the 'silently collapsed' failure mode this \
           \round's review named" $
            case decodeSessionV90 (extractSessionPayload fixtureBytes) of
                Left err → expectationFailure (show err)
                Right sd → do
                    let duplicated = sd { sd90Worlds = sd90Worlds sd ⧺ sd90Worlds sd }
                    case migrateSessionV90 minimalSaveMetadataForExtra duplicated of
                        Right _    → expectationFailure
                            "expected duplicate page ids to be rejected, \
                            \not silently collapsed into one page"
                        Left errs → any (T.isInfixOf "duplicate page id" . ceMessage) errs
                            `shouldBe` True

        it "migrateSessionV90 rejects a B1 save whose craft-bill queue \
           \carries a bill id AT OR ABOVE that page's own allocator \
           \(round-14 review) -- validateCraftBills' allocator/key-\
           \identity check previously never ran anywhere on the B1 path" $
            case decodeSessionV90 (extractSessionPayload fixtureBytes) of
                Left err → expectationFailure (show err)
                Right sd → case sd90Worlds sd of
                    []      → expectationFailure "expected at least one page"
                    (p : ps) → do
                        let malformedBill = CraftBillDTOv1
                                { bil1Id = BillId 5, bil1Station = BuildingId 1
                                , bil1Recipe = "forge_steel_dagger"
                                , bil1Remaining = 1, bil1Claimant = Nothing
                                , bil1ClaimedAt = 0, bil1Progress = 0
                                , bil1Seq = 0, bil1Paused = False
                                , bil1Working = False, bil1Mode = FixedCount
                                , bil1Target = 0, bil1OutputItem = "" }
                            malformedQueue = BillQueueDTOv1
                                { bq1Bills = HM.singleton (BillId 5) malformedBill
                                , bq1NextId = 0 }
                            malformedPage = p { wp90CraftBills = malformedQueue }
                            malformed = sd { sd90Worlds = malformedPage : ps }
                        case migrateSessionV90 minimalSaveMetadataForExtra malformed of
                            Right _    → expectationFailure
                                "expected a craft-bill id at/above its \
                                \page's own allocator to be rejected"
                            Left errs →
                                any (T.isInfixOf "not below the page's bill \
                                                  \allocator" . ceMessage) errs
                                    `shouldBe` True

-- | #1092: world-pages became v3 when WorldIdentity gained its
--   optional language provenance. Every HISTORICAL shape must decode
--   with that provenance ABSENT (#915's precedent) while carrying its
--   name and gloss across byte-exact — a world named before
--   provenance was recorded genuinely has no recoverable language,
--   and inventing one would attach a false etymology to a real world.
languageProvenanceSpec ∷ Spec
languageProvenanceSpec =
    describe "language provenance across historical page shapes \
             \(issue #1092)" $ do
        let identityOf pages pid =
                pgsIdentity =≪ HM.lookup (WorldPageId pid) (wpBase pages)
            -- A page's placed locations in id order (#1101).
            instancesOf pages pid = maybe []
                (instancesToList . wgpLocationInstances . pgsGenParams)
                (HM.lookup (WorldPageId pid) (wpBase pages))
            riversOf pages pid = maybe []
                (riverNamesToList . wgpRiverNames . pgsGenParams)
                (HM.lookup (WorldPageId pid) (wpBase pages))
            -- Decode the REAL tracked B1 bytes, optionally adjust the
            -- single page they carry, migrate, and hand the resulting
            -- PageSnapshot to the assertion.
            withMigratedV90 meta adjust k =
                case decodeSessionV90 (extractSessionPayload fixtureBytes) of
                    Left err → expectationFailure (show err)
                    Right sd →
                        let sd' = sd { sd90Worlds = map adjust (sd90Worlds sd) }
                        in case migrateSessionV90 meta sd' of
                            Left errs →
                                expectationFailure (show (map ceMessage errs))
                            Right snap →
                                case HM.lookup (WorldPageId "main_world")
                                         (snapPages snap) of
                                    Nothing → expectationFailure
                                        "expected the main_world page"
                                    Just p  → k p

        it "a frozen v1 page core decodes its identity with NO language \
           \provenance" $ do
            let dto = WorldPagesDTOv1 [legacyPageCoreV1]
            case S.decode (S.encode dto) ∷ Either String WorldPagesDTOv1 of
                Left err  → expectationFailure err
                Right dto' → do
                    let ident = identityOf (migrateWorldPagesV1 dto') "legacy_page"
                    (wiName <$> ident) `shouldBe` Just "Legacy World"
                    (wiGloss =≪ ident) `shouldBe` Just "an old gloss"
                    (wiLanguage =≪ ident) `shouldBe` Nothing

        it "a frozen v2 page core decodes its identity with NO language \
           \provenance" $ do
            let dto = WorldPagesDTOv2 [legacyPageCoreV2]
            case S.decode (S.encode dto) ∷ Either String WorldPagesDTOv2 of
                Left err  → expectationFailure err
                Right dto' → do
                    let ident = identityOf (migrateWorldPagesV2 dto') "legacy_page"
                    (wiName <$> ident) `shouldBe` Just "Legacy World"
                    (wiGloss =≪ ident) `shouldBe` Just "an old gloss"
                    (wiLanguage =≪ ident) `shouldBe` Nothing

        it "a frozen v3 page core's already-named location keeps its EXACT \
           \stored name and gains no gloss -- an existing location is \
           \never renamed into the world's language by the upgrade" $ do
            let dto = WorldPagesDTOv3 [legacyPageCoreV3]
            case S.decode (S.encode dto) ∷ Either String WorldPagesDTOv3 of
                Left err  → expectationFailure err
                Right dto' → do
                    let insts = instancesOf (migrateWorldPagesV3 dto')
                                    "legacy_page"
                    map liDisplayName insts `shouldBe` ["Small Ruin"]
                    map liGloss insts `shouldBe` [Nothing]
                    -- The page's own identity DOES declare a language,
                    -- so "no gloss" here is the write-once rule, not an
                    -- absent provenance quietly doing the work.
                    (wiLanguage =≪ identityOf (migrateWorldPagesV3 dto')
                                       "legacy_page")
                        `shouldBe` Just (LanguageProvenance
                                            (LangSeed 0xABCDEF0123456789)
                                            (GeneratorVersion 1))

        it "a frozen v4 page core comes back with NO river names, while \
           \its already-named location keeps its EXACT stored name and \
           \gloss -- a save written before rivers were named never \
           \acquires them, and nothing else regresses to do it" $ do
            let dto = WorldPagesDTOv4 [legacyPageCoreV4]
            case S.decode (S.encode dto) ∷ Either String WorldPagesDTOv4 of
                Left err  → expectationFailure err
                Right dto' → do
                    riversOf (migrateWorldPagesV4 dto') "legacy_page"
                        `shouldBe` []
                    let insts = instancesOf (migrateWorldPagesV4 dto')
                                    "legacy_page"
                    map liDisplayName insts `shouldBe` ["Vashenkoro"]
                    map liGloss insts `shouldBe` [Just "Ashen Keep"]
                    -- The page's own identity DOES declare a language,
                    -- so "no river names" here is the write-once rule
                    -- (#1102 requirements 5 and 6), not an absent
                    -- provenance quietly doing the work.
                    (wiLanguage =≪ identityOf (migrateWorldPagesV4 dto')
                                       "legacy_page")
                        `shouldBe` Just (LanguageProvenance
                                            (LangSeed 0xABCDEF0123456789)
                                            (GeneratorVersion 1))

        it "a frozen v6 page core carrying a NONZERO discovery margin \
           \migrates through the canonical value preserving every other instance field \
           \EXACTLY -- allocator, id, definition, chunk, anchor, bounds, \
           \name, gloss, etymology, lifecycle and contents-spawned -- \
           \while the margin itself has no live counterpart left to \
           \land in (#1230 requirement 11)" $ do
            let dto = WorldPagesDTOv6 [legacyPageCoreV6]
            -- The bytes really do carry the margin: assert it on the
            -- ENCODED shape first, so "it is gone afterwards" is a
            -- migration outcome rather than a field that was never set.
            map lid3DiscoveryMargin (HM.elems (lisd3ById richInstancesV6))
                `shouldBe` [6]
            case S.decode (S.encode dto) ∷ Either String WorldPagesDTOv6 of
                Left err  → expectationFailure err
                Right dto' → do
                    let pages = migrateWorldPagesV6 dto'
                        insts = instancesOf pages "legacy_page"
                    -- Whole-record equality against the live fixture:
                    -- 'LocationInstance's derived Eq covers EVERY field,
                    -- so this catches a dropped, defaulted, reordered or
                    -- swapped one — including any margin that had
                    -- survived onto the live record, which no longer has
                    -- anywhere to put one.
                    insts `shouldBe` HM.elems (lisById richInstances)
                    -- Spelled out as well, because a whole-record
                    -- comparison reports "not equal" without saying
                    -- which durable identity moved.
                    map liId insts        `shouldBe` [LocationInstanceId 4]
                    map liDefId insts     `shouldBe` ["ruin_small"]
                    map liChunk insts     `shouldBe` [ChunkCoord 2 3]
                    map liAnchor insts    `shouldBe` [(80, 112)]
                    map liBounds insts    `shouldBe` [AbsBounds 78 110 82 114]
                    map liDisplayName insts `shouldBe` ["Vashenkoro"]
                    map liGloss insts     `shouldBe` [Just "Ashen Keep"]
                    map liEtymology insts `shouldBe` [Just keepSource]
                    map liLifecycle insts `shouldBe` [LifecycleCleared]
                    map liContentsSpawned insts `shouldBe` [True]
                    -- The page-local ALLOCATOR rides across too: a
                    -- migration that rebuilt the table from its values
                    -- would reset this to "one past the highest id" (5)
                    -- and hand a later placement an id this save had
                    -- already retired.
                    map (lisNextId ∘ wgpLocationInstances ∘ pgsGenParams)
                        (HM.elems (wpBase pages))
                        `shouldBe` [7]
                    -- Nothing ELSE about the page moved either: its
                    -- identity, its etymology source, and its river
                    -- names all survive the instance-shape change.
                    (wiName <$> identityOf pages "legacy_page")
                        `shouldBe` Just "Legacy World"
                    (wiEtymology =≪ identityOf pages "legacy_page")
                        `shouldBe` Just keepSource
                    map (rvnDisplayName ∘ snd) (riversOf pages "legacy_page")
                        `shouldBe` ["Vashendral"]
                    map (rvnEtymology ∘ snd) (riversOf pages "legacy_page")
                        `shouldBe` [Just ashenRiverSource]

        it "a frozen pre-#916 v7 page preserves every stored location \
           \field and gains NO encounter during the v7-to-v8 migration" $ do
            let dto = WorldPagesDTOv7 [legacyPageCoreV7]
            case S.decode (S.encode dto) ∷ Either String WorldPagesDTOv7 of
                Left err → expectationFailure err
                Right dto' → do
                    let pages = migrateWorldPagesV7 dto'
                        insts = instancesOf pages "legacy_page"
                    insts `shouldBe` HM.elems (lisById richInstances)
                    map liEncounter insts `shouldBe` [Nothing]
                    map (lisNextId ∘ wgpLocationInstances ∘ pgsGenParams)
                        (HM.elems (wpBase pages)) `shouldBe` [7]

        it "a frozen pre-#917 v8 page preserves every stored location \
           \field, lifts each encounter's clearance notice onto the \
           \instance, and gains NO significant obligations" $ do
            let dto = WorldPagesDTOv8 [legacyPageCoreV8]
            case S.decode (S.encode dto) ∷ Either String WorldPagesDTOv8 of
                Left err → expectationFailure err
                Right dto' → do
                    let pages = migrateWorldPagesV8 dto'
                        insts = L.sortOn liId (instancesOf pages "legacy_page")
                        expected = liEncounter
                            =≪ listToMaybe (HM.elems
                                    (lisById defeatedInstances))
                    map liId insts `shouldBe`
                        [LocationInstanceId 4, LocationInstanceId 5]
                    -- Every stored location field rides across…
                    map liDefId insts `shouldBe` ["ruin_small", "ruin_small"]
                    map liDisplayName insts
                        `shouldBe` ["Vashenkoro", "Vashenkoro"]
                    map liGloss insts
                        `shouldBe` [Just "Ashen Keep", Just "Ashen Keep"]
                    map liEtymology insts
                        `shouldBe` [Just keepSource, Just keepSource]
                    map liContentsSpawned insts `shouldBe` [True, True]
                    map liLifecycle insts
                        `shouldBe` [LifecycleCleared, LifecycleUnknown]
                    -- …and so does the encounter, minus exactly the one
                    -- field #917 moved: every other value is identical
                    -- on both instances.
                    map liEncounter insts `shouldBe` [expected, expected]
                    -- The notice lands on the INSTANCE, per instance —
                    -- so the announced one does not announce again, and
                    -- the deferred one has not lost its pending notice.
                    map liClearEventEmitted insts `shouldBe` [True, False]
                    -- And nothing invents an obligation from today's
                    -- YAML: a materialized world owes no item it never
                    -- spawned, which is what would otherwise make an
                    -- already-cleared location permanently unclearable.
                    map liSignificant insts `shouldBe` [[], []]
                    map (lisNextId ∘ wgpLocationInstances ∘ pgsGenParams)
                        (HM.elems (wpBase pages)) `shouldBe` [7]

        it "the CURRENT v9 page core round-trips a location's significant \
           \obligations -- taken and untaken, each bound to its own \
           \physical item identity -- plus the instance-level clearance \
           \notice, so the v8 absences above are a real decode outcome \
           \and not fields nothing ever writes" $ do
            let dto = WorldPagesDTO [currentPageCoreSignificant]
            case S.decode (S.encode dto) ∷ Either String WorldPagesDTO of
                Left err → expectationFailure err
                Right dto' → do
                    let insts = instancesOf (basePageSnapshots dto')
                                    "legacy_page"
                    map liSignificant insts `shouldBe`
                        [ [ LocationSignificantItem 1 "processing_unit"
                                (Just 6101) True
                          , LocationSignificantItem 2 "processing_unit"
                                (Just 6102) False
                          ] ]
                    map liClearEventEmitted insts `shouldBe` [True]

        it "a frozen v5 page core comes back with NO etymology source on \
           \its page identity, its location, or its river, while every \
           \name and gloss it stored survives EXACTLY -- a save written \
           \before names recorded their expressions never acquires one, \
           \and none is inferred from the text that is there" $ do
            let dto = WorldPagesDTOv5 [legacyPageCoreV5]
            case S.decode (S.encode dto) ∷ Either String WorldPagesDTOv5 of
                Left err  → expectationFailure err
                Right dto' → do
                    let pages = migrateWorldPagesV5 dto'
                        ident = identityOf pages "legacy_page"
                        insts = instancesOf pages "legacy_page"
                    -- Everything the v5 shape DID store rides across.
                    (wiName <$> ident)  `shouldBe` Just "Legacy World"
                    (wiGloss =≪ ident)  `shouldBe` Just "an old gloss"
                    map liDisplayName insts `shouldBe` ["Vashenkoro"]
                    map liGloss insts       `shouldBe` [Just "Ashen Keep"]
                    map (rvnDisplayName ∘ snd) (riversOf pages "legacy_page")
                        `shouldBe` ["Vashendral"]
                    map (rvnGloss ∘ snd) (riversOf pages "legacy_page")
                        `shouldBe` [Just "Ashen River"]
                    -- And all three etymology sources are ABSENT.
                    (wiEtymology =≪ ident) `shouldBe` Nothing
                    map liEtymology insts  `shouldBe` [Nothing]
                    map (rvnEtymology ∘ snd) (riversOf pages "legacy_page")
                        `shouldBe` [Nothing]
                    -- The page's own identity DOES declare a language,
                    -- so "no etymology" here is #1104 requirement 1's
                    -- honest absence, not an absent provenance quietly
                    -- doing the work.
                    (wiLanguage =≪ ident)
                        `shouldBe` Just (LanguageProvenance
                                            (LangSeed 0xABCDEF0123456789)
                                            (GeneratorVersion 1))

        it "the CURRENT v8 page core round-trips an etymology source on \
           \its page identity, its location, AND its river -- so the v5 \
           \absences above are real decode outcomes, not fields nothing \
           \ever writes" $ do
            let dto = WorldPagesDTO [currentPageCoreEtymology]
            case S.decode (S.encode dto) ∷ Either String WorldPagesDTO of
                Left err  → expectationFailure err
                Right dto' → do
                    let pages = basePageSnapshots dto'
                        ident = identityOf pages "legacy_page"
                    (wiEtymology =≪ ident) `shouldBe` Just worldNameSource
                    map liEtymology (instancesOf pages "legacy_page")
                        `shouldBe` [Just keepSource]
                    map (rvnEtymology ∘ snd) (riversOf pages "legacy_page")
                        `shouldBe` [Just ashenRiverSource]

        it "the CURRENT v8 page core round-trips a river's name AND gloss, \
           \keyed by its feature id -- so the v4 absence above is a real \
           \decode outcome, not a table that is always empty" $ do
            let dto = WorldPagesDTO [currentPageCoreRivers]
            case S.decode (S.encode dto) ∷ Either String WorldPagesDTO of
                Left err  → expectationFailure err
                Right dto' →
                    riversOf (basePageSnapshots dto') "legacy_page"
                        `shouldBe` [ (GeoFeatureId 3
                                     , RiverName "Vashendral"
                                           (Just "Ashen River")
                                           (Just ashenRiverSource)) ]

        it "the CURRENT v8 page core round-trips a location's name AND \
           \gloss -- so the v3 absence above is a real decode outcome, \
           \not a field that is always Nothing" $ do
            let dto = WorldPagesDTO [currentPageCoreNamed]
            case S.decode (S.encode dto) ∷ Either String WorldPagesDTO of
                Left err  → expectationFailure err
                Right dto' → do
                    let insts = instancesOf (basePageSnapshots dto')
                                    "legacy_page"
                    map liDisplayName insts `shouldBe` ["Vashenkoro"]
                    map liGloss insts `shouldBe` [Just "Ashen Keep"]

        it "the CURRENT v8 page core round-trips a complete ruin encounter \
           \including typed occupant identity, home, policy, status and \
           \one-shot feedback latches" $ do
            let dto = WorldPagesDTO [currentPageCoreEncounter]
            case S.decode (S.encode dto) ∷ Either String WorldPagesDTO of
                Left err → expectationFailure err
                Right dto' →
                    map liEncounter
                        (instancesOf (basePageSnapshots dto') "legacy_page")
                    `shouldBe` map liEncounter
                        (HM.elems (lisById encounterInstances))

        it "the CURRENT v8 page core round-trips a present provenance -- \
           \so the two absences above are a real decode outcome, not a \
           \field that is always Nothing" $ do
            let dto = WorldPagesDTO [currentPageCore]
            case S.decode (S.encode dto) ∷ Either String WorldPagesDTO of
                Left err  → expectationFailure err
                Right dto' → do
                    let ident = identityOf (basePageSnapshots dto') "legacy_page"
                    (wiName <$> ident) `shouldBe` Just "Legacy World"
                    (wiLanguage =≪ ident) `shouldBe`
                        Just (LanguageProvenance (LangSeed 0xABCDEF0123456789)
                                  (GeneratorVersion 1))

        it "the real, tracked B1 (v90) fixture's own page carries NO \
           \identity at all, and migrates that way" $
            -- The premise for the mutation below: this fixture predates
            -- #707, so its page identity is genuinely absent -- asserting
            -- provenance on it as-is would prove nothing about the
            -- migration, only about an empty field.
            withMigratedV90 minimalSaveMetadataForExtra id $
                \p → pgsIdentity p `shouldBe` Nothing

        it "a v90 page carrying a legacy identity migrates with its name \
           \and gloss exact and NO language provenance" $
            -- Same mutate-the-real-fixture style the duplicate-page and
            -- malformed-bill checks above use: the bytes are real B1
            -- bytes, with one frozen field set to the case under test.
            -- The manifest metadata must name the same world, or the
            -- migration's own metadata/gameplay agreement check (#766
            -- requirement 12) rejects the session before identity ever
            -- reaches a snapshot.
            withMigratedV90
                    minimalSaveMetadataForExtra
                        { smWorldName = Just "Legacy World"
                        , smWorldGloss = Just "an old gloss" }
                    (\p → p { wp90Identity = Just legacyIdentityDTO }) $
                \p → do
                    (wiName <$> pgsIdentity p) `shouldBe` Just "Legacy World"
                    (wiGloss =≪ pgsIdentity p) `shouldBe` Just "an old gloss"
                    (wiLanguage =≪ pgsIdentity p) `shouldBe` Nothing

-- | #1092 fixtures: the same page core in each of the three page-core
--   shapes, differing ONLY in identity. The pre-#1092 shapes carry the
--   frozen name/gloss-only identity; the current one carries a
--   provenance-bearing identity whose seed is above @2^63-1@, which a
--   narrowed carrier would mangle.
legacyIdentityDTO ∷ WorldIdentityDTOv1
legacyIdentityDTO = WorldIdentityDTOv1 "Legacy World" (Just "an old gloss")

legacyPageCoreV1 ∷ PageCoreDTOv1
legacyPageCoreV1 = PageCoreDTOv1
    { pc1PageId     = WorldPageId "legacy_page"
    , pc1GenParams  = toWorldGenParamsDTOv1 defaultWorldGenParams
    , pc1CameraX    = 1, pc1CameraY = 2
    , pc1TimeHour   = 12, pc1TimeMinute = 30
    , pc1DateYear   = 1, pc1DateMonth = 2, pc1DateDay = 3
    , pc1MapMode    = ZMDefault
    , pc1Identity   = Just legacyIdentityDTO
    }

legacyPageCoreV2 ∷ PageCoreDTOv2
legacyPageCoreV2 = PageCoreDTOv2
    { pc2PageId     = WorldPageId "legacy_page"
    , pc2GenParams  = toWorldGenParamsDTOv2 defaultWorldGenParams
    , pc2CameraX    = 1, pc2CameraY = 2
    , pc2TimeHour   = 12, pc2TimeMinute = 30
    , pc2DateYear   = 1, pc2DateMonth = 2, pc2DateDay = 3
    , pc2MapMode    = ZMDefault
    , pc2Identity   = Just legacyIdentityDTO
    }

-- | #1101 fixture: a pre-#1101 (@world-pages@ v3) page core carrying a
--   real placed location. Its gen params are the frozen pre-#1101
--   shape, so the instance genuinely has nowhere to store a gloss —
--   which is what makes the v3 migration's "keeps its stored name,
--   gains no gloss" contract a real decode outcome rather than a field
--   that was never written.
legacyPageCoreV3 ∷ PageCoreDTOv3
legacyPageCoreV3 = PageCoreDTOv3
    { pc3PageId     = WorldPageId "legacy_page"
    , pc3GenParams  = toWorldGenParamsDTOv2 defaultWorldGenParams
                          { wgpLocationInstances = legacyNamedInstances }
    , pc3CameraX    = 1, pc3CameraY = 2
    , pc3TimeHour   = 12, pc3TimeMinute = 30
    , pc3DateYear   = 1, pc3DateMonth = 2, pc3DateDay = 3
    , pc3MapMode    = ZMDefault
    , pc3Identity   = Just (WorldIdentityDTOv2 "Legacy World"
                               (Just "an old gloss")
                               (Just (LanguageProvenanceDTO
                                          0xABCDEF0123456789 1)))
    }

-- | One already-named placed location, as a pre-#1101 save holds it:
--   an ordinary 'ldLabel' name (nothing before #1101 could produce any
--   other kind) and no gloss field at all.
legacyNamedInstances ∷ LocationInstances
legacyNamedInstances = LocationInstances
    { lisNextId        = 2
    , lisById          = HM.singleton (LocationInstanceId 1) LocationInstance
        { liId              = LocationInstanceId 1
        , liDefId           = "ruin_small"
        , liChunk           = ChunkCoord 2 3
        , liAnchor          = (80, 112)
        , liBounds          = AbsBounds 78 110 82 114
        , liDisplayName     = "Small Ruin"
        , liGloss           = Nothing
        , liEtymology       = Nothing
        , liLifecycle       = LifecycleDiscovered
        , liContentsSpawned = True
        , liEncounter       = Nothing
        , liSignificant     = []
        , liClearEventEmitted = False
        }
    , lisPendingLegacy = Nothing
    }

-- | #1101: the current page core carrying a location named in the
--   page's own language, gloss and all.
-- | #1102 fixture: a pre-#1102 (@world-pages@ v4) page core whose gen
--   params are the frozen pre-#1102 shape, so the page genuinely has
--   nowhere to store a river name — which is what makes the v4
--   migration's "no river names, everything else exact" contract a real
--   decode outcome rather than a field that was never written. Its
--   location is already named IN a language, so the migration is also
--   shown not to disturb what #1101 stored.
legacyPageCoreV4 ∷ PageCoreDTOv4
legacyPageCoreV4 = PageCoreDTOv4
    { pc4PageId     = WorldPageId "legacy_page"
    , pc4GenParams  = toWorldGenParamsDTOv3 defaultWorldGenParams
                          { wgpLocationInstances = namedLocationInstances }
    , pc4CameraX    = 1, pc4CameraY = 2
    , pc4TimeHour   = 12, pc4TimeMinute = 30
    , pc4DateYear   = 1, pc4DateMonth = 2, pc4DateDay = 3
    , pc4MapMode    = ZMDefault
    , pc4Identity   = Just (WorldIdentityDTOv2 "Legacy World"
                               (Just "an old gloss")
                               (Just (LanguageProvenanceDTO
                                          0xABCDEF0123456789 1)))
    }

-- | 'legacyNamedInstances' with a generated name and gloss, as #1101
--   stores one. Shared by the v4 fixture above and the current-shape
--   one below.
namedLocationInstances ∷ LocationInstances
namedLocationInstances = legacyNamedInstances
    { lisById = HM.map (\i → i { liDisplayName = "Vashenkoro"
                               , liGloss       = Just "Ashen Keep" })
                       (lisById legacyNamedInstances) }

-- | The current page core carrying one NAMED river, so the v4 fixture's
--   empty table above is a decode outcome rather than a shape that can
--   never hold anything.
currentPageCoreRivers ∷ PageCoreDTO
currentPageCoreRivers = currentPageCore
    { pcGenParams = toWorldGenParamsDTO defaultWorldGenParams
        { wgpRiverNames = RiverNames (HM.singleton (GeoFeatureId 3)
              (RiverName "Vashendral" (Just "Ashen River")
                   (Just ashenRiverSource))) }
    }

currentPageCoreNamed ∷ PageCoreDTO
currentPageCoreNamed = currentPageCore
    { pcGenParams = toWorldGenParamsDTO defaultWorldGenParams
        { wgpLocationInstances = namedLocationInstances }
    }

currentPageCoreEncounter ∷ PageCoreDTO
currentPageCoreEncounter = currentPageCore
    { pcGenParams = toWorldGenParamsDTO defaultWorldGenParams
        { wgpLocationInstances = encounterInstances }
    }

-- | #1230 fixture: one fully-populated placed location, as a
--   @world-pages@ v6 save holds it. Every field the migration must
--   carry across is set to something a default could not produce — a
--   nondefault id and allocator, a definition id, an off-origin chunk
--   and anchor, real bounds, a GENERATED name with a gloss and the
--   etymology source that explains it, a lifecycle past the initial
--   one, and a raised contents-spawned flag — so "preserved exactly"
--   below is a real decode outcome rather than a comparison of
--   defaults.
richInstances ∷ LocationInstances
richInstances = LocationInstances
    { lisNextId        = 7
    , lisById          = HM.singleton (LocationInstanceId 4) LocationInstance
        { liId              = LocationInstanceId 4
        , liDefId           = "ruin_small"
        , liChunk           = ChunkCoord 2 3
        , liAnchor          = (80, 112)
        , liBounds          = AbsBounds 78 110 82 114
        , liDisplayName     = "Vashenkoro"
        , liGloss           = Just "Ashen Keep"
        , liEtymology       = Just keepSource
        , liLifecycle       = LifecycleCleared
        , liContentsSpawned = True
        , liEncounter       = Nothing
        , liSignificant     = []
        , liClearEventEmitted = False
        }
    , lisPendingLegacy = Nothing
    }

encounterInstances ∷ LocationInstances
encounterInstances = richInstances
    { lisById = HM.map (\inst → inst
        { liLifecycle = LifecycleActive
        , liEncounter = Just LocationEncounter
            { leRolledCount = 2
            , leOccupants =
                [ LocationEncounterOccupant (UnitId 41) (79.5, 111.0)
                    True False
                , LocationEncounterOccupant (UnitId 42) (81.0, 113.5)
                    False True
                ]
            , leRosterComplete = True
            , leDeathOnlyClearance = True
            , leActivated = True
            , leEpisodeActive = True
            , leAggressionAnnounced = True
            , leDisengageAnnounced = False
            , leCleared = False
            }
        }) (lisById richInstances)
    }

-- | The pre-#917 shape a v8 payload really holds: #916's encounter,
--   with its clearance-notice flag still nested inside it. Two
--   instances, one announced and one not, so the migration cannot pass
--   by mapping both to the same value.
--
--   Built through 'toLocationInstancesDTOv5', which is the frozen
--   encoder — that is the only way to produce genuine v8 bytes now that
--   the live record has no such field.
legacyV8Instances ∷ LocationInstancesDTOv5
legacyV8Instances = LocationInstancesDTOv5
    { lisd5NextId = 7
    , lisd5ById   = HM.fromList
        [ (LocationInstanceId 4, v8Encoded True  LifecycleCleared)
        , (LocationInstanceId 5, v8Encoded False LifecycleUnknown)
        ]
    }
  where
    -- @announced@ is the v8 @leClearEventEmitted@ this fixture is about.
    v8Encoded announced lifecycle =
        let base = case HM.elems (lisd5ById
                        (toLocationInstancesDTOv5 defeatedInstances)) of
                       (one:_) → one
                       [] → error "v8 fixture has no instance"
        in base { lid5Id        = if announced then LocationInstanceId 4
                                               else LocationInstanceId 5
                , lid5Lifecycle = lifecycle
                , lid5Encounter =
                    (\e → e { led1ClearEventEmitted = announced })
                        <$> lid5Encounter base
                }

-- | 'richInstances' with a DEFEATED death-only encounter — the state a
--   deferred clearance notice can actually be owed from.
defeatedInstances ∷ LocationInstances
defeatedInstances = encounterInstances
    { lisById = HM.map (\inst → inst
        { liEncounter = (\e → e { leCleared = True
                                , leEpisodeActive = False }) <$> liEncounter inst
        }) (lisById encounterInstances)
    }

-- | The immediate pre-#917 current shape: identical page data over the
--   frozen location DTO that has no significant obligations and keeps
--   the clearance notice inside its encounter.
legacyPageCoreV8 ∷ PageCoreDTOv8
legacyPageCoreV8 = PageCoreDTOv8
    { pc8PageId = WorldPageId "legacy_page"
    , pc8GenParams = (toWorldGenParamsDTOv7 defaultWorldGenParams)
        { gp7LocationInstances = legacyV8Instances }
    , pc8CameraX = 1, pc8CameraY = 2
    , pc8TimeHour = 12, pc8TimeMinute = 30
    , pc8DateYear = 1, pc8DateMonth = 2, pc8DateDay = 3
    , pc8MapMode = ZMDefault
    , pc8Identity = Just (WorldIdentityDTO "Legacy World"
        (Just "an old gloss")
        (Just (LanguageProvenanceDTO 0xABCDEF0123456789 1))
        (Just (toEtymologySourceDTO keepSource)))
    }

-- | #917: a CURRENT-shape page whose location owes two significant
--   items in DIFFERENT states — one taken, one spawned and still
--   untaken — plus a spent clearance notice. Without this the v8
--   migration's "no obligations" assertion would only be proving that a
--   field nothing writes comes back empty.
significantInstances ∷ LocationInstances
significantInstances = richInstances
    { lisById = HM.map (\inst → inst
        { liSignificant =
            [ LocationSignificantItem 1 "processing_unit" (Just 6101) True
            , LocationSignificantItem 2 "processing_unit" (Just 6102) False
            ]
        , liClearEventEmitted = True
        }) (lisById richInstances)
    }

currentPageCoreSignificant ∷ PageCoreDTO
currentPageCoreSignificant = currentPageCore
    { pcGenParams = toWorldGenParamsDTO defaultWorldGenParams
        { wgpLocationInstances = significantInstances } }

-- | 'richInstances' encoded into the FROZEN v6 wire shape carrying a
--   NONZERO discovery margin (#1230 requirement 11). The margin is
--   stamped on explicitly rather than taken from the live record,
--   because the live record no longer has one — which is exactly the
--   thing under test: these bytes really do carry a 6 that the current
--   shape has nowhere to put.
richInstancesV6 ∷ LocationInstancesDTOv3
richInstancesV6 =
    let base = toLocationInstancesDTOv3 richInstances
    in base { lisd3ById = HM.map (\d → d { lid3DiscoveryMargin = 6 })
                                 (lisd3ById base) }

-- | A pre-#1230 (@world-pages@ v6) page core over those instances.
legacyPageCoreV6 ∷ PageCoreDTOv6
legacyPageCoreV6 = PageCoreDTOv6
    { pc6PageId     = WorldPageId "legacy_page"
    , pc6GenParams  = (toWorldGenParamsDTOv5 defaultWorldGenParams)
                          { gp5LocationInstances = richInstancesV6
                          , gp5RiverNames = toRiverNamesDTO (RiverNames
                              (HM.singleton (GeoFeatureId 3)
                                  (RiverName "Vashendral" (Just "Ashen River")
                                      (Just ashenRiverSource)))) }
    , pc6CameraX    = 1, pc6CameraY = 2
    , pc6TimeHour   = 12, pc6TimeMinute = 30
    , pc6DateYear   = 1, pc6DateMonth = 2, pc6DateDay = 3
    , pc6MapMode    = ZMDefault
    , pc6Identity   = Just (WorldIdentityDTO "Legacy World"
                               (Just "an old gloss")
                               (Just (LanguageProvenanceDTO
                                          0xABCDEF0123456789 1))
                               (Just (toEtymologySourceDTO keepSource)))
    }

-- | The immediate pre-#916 current shape: identical page data over the
--   frozen location-instance DTO with no encounter field.
legacyPageCoreV7 ∷ PageCoreDTOv7
legacyPageCoreV7 = PageCoreDTOv7
    { pc7PageId = WorldPageId "legacy_page"
    , pc7GenParams = toWorldGenParamsDTOv6 defaultWorldGenParams
        { wgpLocationInstances = richInstances }
    , pc7CameraX = 1, pc7CameraY = 2
    , pc7TimeHour = 12, pc7TimeMinute = 30
    , pc7DateYear = 1, pc7DateMonth = 2, pc7DateDay = 3
    , pc7MapMode = ZMDefault
    , pc7Identity = Just (WorldIdentityDTO "Legacy World"
        (Just "an old gloss")
        (Just (LanguageProvenanceDTO 0xABCDEF0123456789 1))
        (Just (toEtymologySourceDTO keepSource)))
    }

-- | #1104 fixture: a pre-#1104 (@world-pages@ v5) page core whose
--   identity, gen params, location instance, and river name are all the
--   frozen pre-etymology shapes — so the page genuinely has nowhere to
--   store an expression, which is what makes the v5 migration's "no
--   etymology source anywhere, everything else exact" contract a real
--   decode outcome rather than three fields that were never written.
legacyPageCoreV5 ∷ PageCoreDTOv5
legacyPageCoreV5 = PageCoreDTOv5
    { pc5PageId     = WorldPageId "legacy_page"
    , pc5GenParams  = toWorldGenParamsDTOv4 defaultWorldGenParams
                          { wgpLocationInstances = namedLocationInstances
                          , wgpRiverNames = RiverNames
                              (HM.singleton (GeoFeatureId 3)
                                  (RiverName "Vashendral"
                                      (Just "Ashen River") Nothing)) }
    , pc5CameraX    = 1, pc5CameraY = 2
    , pc5TimeHour   = 12, pc5TimeMinute = 30
    , pc5DateYear   = 1, pc5DateMonth = 2, pc5DateDay = 3
    , pc5MapMode    = ZMDefault
    , pc5Identity   = Just (WorldIdentityDTOv2 "Legacy World"
                               (Just "an old gloss")
                               (Just (LanguageProvenanceDTO
                                          0xABCDEF0123456789 1)))
    }

-- | The current page core carrying an etymology source on all three of
--   the things that can hold one.
currentPageCoreEtymology ∷ PageCoreDTO
currentPageCoreEtymology = currentPageCore
    { pcGenParams = toWorldGenParamsDTO defaultWorldGenParams
        { wgpLocationInstances = namedLocationInstances
            { lisById = HM.map (\i → i { liEtymology = Just keepSource })
                               (lisById namedLocationInstances) }
        , wgpRiverNames = RiverNames (HM.singleton (GeoFeatureId 3)
              (RiverName "Vashendral" (Just "Ashen River")
                  (Just ashenRiverSource))) }
    , pcIdentity  = Just (WorldIdentityDTO "Legacy World"
                              (Just "an old gloss")
                              (Just (LanguageProvenanceDTO
                                         0xABCDEF0123456789 1))
                              (Just (toEtymologySourceDTO worldNameSource)))
    }

-- | The world-name and location expressions the current-shape fixture
--   above stores. Distinct from each other and from
--   'ashenRiverSource', so a decode that collapsed all three onto one
--   value could not pass.
worldNameSource ∷ EtymologySource
worldNameSource = EtymologySource
    { esExpr     = Bare (ConceptId "LAND")
    , esLanguage = LanguageProvenance (LangSeed 0xABCDEF0123456789)
                                      (GeneratorVersion 1)
    }

keepSource ∷ EtymologySource
keepSource = EtymologySource
    { esExpr     = Modifier (ConceptId "ASH") (ConceptId "KEEP")
    , esLanguage = LanguageProvenance (LangSeed 0xABCDEF0123456789)
                                      (GeneratorVersion 1)
    }

currentPageCore ∷ PageCoreDTO
currentPageCore = PageCoreDTO
    { pcPageId     = WorldPageId "legacy_page"
    , pcGenParams  = toWorldGenParamsDTO defaultWorldGenParams
    , pcCameraX    = 1, pcCameraY = 2
    , pcTimeHour   = 12, pcTimeMinute = 30
    , pcDateYear   = 1, pcDateMonth = 2, pcDateDay = 3
    , pcMapMode    = ZMDefault
    , pcIdentity   = Just (WorldIdentityDTO "Legacy World" (Just "an old gloss")
                               (Just (LanguageProvenanceDTO
                                          0xABCDEF0123456789 1))
                               Nothing)
    , pcGeneratedId = Just (fixtureGeneratedWorldIdForPage
                                (WorldPageId "legacy_page"))
    }


-- | #1104: the etymology source the current-shape fixtures attach, so a
--   round trip through the CURRENT wire shape is shown to carry one —
--   which is what makes the v5 migration's "no source" a real decode
--   outcome rather than a field nothing ever writes.
ashenRiverSource ∷ EtymologySource
ashenRiverSource = EtymologySource
    { esExpr     = Modifier (ConceptId "ASH") (ConceptId "RIVER")
    , esLanguage = LanguageProvenance (LangSeed 0xABCDEF0123456789)
                                      (GeneratorVersion 1)
    }

