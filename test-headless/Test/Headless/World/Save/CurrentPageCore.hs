-- | Current page-core persistence assertions preserved from the retired
-- historical DTO family by PSP-5.
module Test.Headless.World.Save.CurrentPageCore (spec) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import Test.Hspec

import Gameplay.Tags.Index (emptyCategoryIndex)
import Language.Etymology.Source (EtymologySource(..))
import Language.Generated.Types (LanguageProvenance(..), LangSeed(..), GeneratorVersion(..))
import Language.Semantic.Types (ConceptId(..), NameExpr(..))
import Location.Bounds (AbsBounds(..))
import Location.Instance
    ( LocationEncounter(..), LocationEncounterOccupant(..)
    , LocationInstance(..), LocationInstances(..), LocationInstanceId(..)
    , LocationLifecycle(..), LocationSignificantItem(..), instancesToList )
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)
import Unit.Types (UnitId(..))
import World.Base (GeoFeatureId(..))
import World.Chunk.Types (ChunkCoord(..))
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldIdentity(..), WorldPageId(..))
import World.River.Naming (RiverName(..), RiverNames(..), riverNamesToList)
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Save.Component.Page
    ( LanguageProvenanceDTO(..), PageCoreDTO(..), WorldIdentityDTO(..)
    , WorldPages(..), WorldPagesDTO(..), basePageSnapshots, toWorldGenParamsDTO )
import World.Save.Component.WorldGen (toEtymologySourceDTO)
import World.Save.Snapshot (PageSnapshot(..))

spec ∷ Spec
spec = currentPageCoreSpec

currentPageCoreSpec ∷ Spec
currentPageCoreSpec = describe "current page-core persistence" $ do
    it "round-trips significant obligations and the clearance notice" $ do
        let dto = WorldPagesDTO [currentPageCoreSignificant]
        case S.decode (S.encode dto) ∷ Either String WorldPagesDTO of
            Left err → expectationFailure err
            Right dto' → do
                let insts = currentInstancesOf (basePageSnapshots dto') "legacy_page"
                map liSignificant insts `shouldBe`
                    [ [ LocationSignificantItem 1 "processing_unit" (Just 6101) True
                      , LocationSignificantItem 2 "processing_unit" (Just 6102) False ] ]
                map liClearEventEmitted insts `shouldBe` [True]

    it "round-trips distinct page, location and river etymology sources" $ do
        let dto = WorldPagesDTO [currentPageCoreEtymology]
        case S.decode (S.encode dto) ∷ Either String WorldPagesDTO of
            Left err → expectationFailure err
            Right dto' → do
                let pages = basePageSnapshots dto'
                    ident = currentIdentityOf pages "legacy_page"
                (wiEtymology =≪ ident) `shouldBe` Just worldNameSource
                map liEtymology (currentInstancesOf pages "legacy_page") `shouldBe`
                    [Just keepSource]
                map (rvnEtymology ∘ snd) (currentRiversOf pages "legacy_page")
                    `shouldBe` [Just ashenRiverSource]

    it "round-trips the named river, gloss and etymology" $ do
        let dto = WorldPagesDTO [currentPageCoreRivers]
        case S.decode (S.encode dto) ∷ Either String WorldPagesDTO of
            Left err → expectationFailure err
            Right dto' → currentRiversOf (basePageSnapshots dto') "legacy_page"
                `shouldBe` [ (GeoFeatureId 3, RiverName "Vashendral"
                                (Just "Ashen River") (Just ashenRiverSource)) ]

    it "round-trips the location's authored name and gloss" $ do
        let dto = WorldPagesDTO [currentPageCoreNamed]
        case S.decode (S.encode dto) ∷ Either String WorldPagesDTO of
            Left err → expectationFailure err
            Right dto' → do
                let insts = currentInstancesOf (basePageSnapshots dto') "legacy_page"
                map liDisplayName insts `shouldBe` ["Vashenkoro"]
                map liGloss insts `shouldBe` [Just "Ashen Keep"]

    it "round-trips a complete ruin encounter with its typed occupant state" $ do
        let dto = WorldPagesDTO [currentPageCoreEncounter]
        case S.decode (S.encode dto) ∷ Either String WorldPagesDTO of
            Left err → expectationFailure err
            Right dto' → map liEncounter
                    (currentInstancesOf (basePageSnapshots dto') "legacy_page")
                `shouldBe` map liEncounter (HM.elems (lisById encounterInstances))

    it "round-trips a nonzero sub-minute clock remainder" $ do
        let dto = WorldPagesDTO [currentPageCore]
        case S.decode (S.encode dto) ∷ Either String WorldPagesDTO of
            Left err → expectationFailure err
            Right dto' → map pgsTimeRemainder
                    (HM.elems (wpBase (basePageSnapshots dto'))) `shouldBe` [0.25]

    it "round-trips present language provenance" $ do
        let dto = WorldPagesDTO [currentPageCore]
        case S.decode (S.encode dto) ∷ Either String WorldPagesDTO of
            Left err → expectationFailure err
            Right dto' → (wiLanguage =≪ currentIdentityOf
                    (basePageSnapshots dto') "legacy_page")
                `shouldBe` Just (LanguageProvenance
                    (LangSeed 0xABCDEF0123456789) (GeneratorVersion 1))

currentIdentityOf ∷ WorldPages → Text → Maybe WorldIdentity
currentIdentityOf pages pid = pgsIdentity =≪ HM.lookup (WorldPageId pid) (wpBase pages)

currentInstancesOf ∷ WorldPages → Text → [LocationInstance]
currentInstancesOf pages pid = maybe []
    (instancesToList ∘ wgpLocationInstances ∘ pgsGenParams)
    (HM.lookup (WorldPageId pid) (wpBase pages))

currentRiversOf ∷ WorldPages → Text → [(GeoFeatureId, RiverName)]
currentRiversOf pages pid = maybe []
    (riverNamesToList ∘ wgpRiverNames ∘ pgsGenParams)
    (HM.lookup (WorldPageId pid) (wpBase pages))

currentPageCoreRivers ∷ PageCoreDTO
currentPageCoreRivers = currentPageCore
    { pcGenParams = toWorldGenParamsDTO defaultWorldGenParams
        { wgpRiverNames = RiverNames (HM.singleton (GeoFeatureId 3)
              (RiverName "Vashendral" (Just "Ashen River") (Just ashenRiverSource))) }
    }

currentPageCoreNamed ∷ PageCoreDTO
currentPageCoreNamed = currentPageCore
    { pcGenParams = toWorldGenParamsDTO defaultWorldGenParams
        { wgpLocationInstances = namedInstances } }

currentPageCoreEncounter ∷ PageCoreDTO
currentPageCoreEncounter = currentPageCore
    { pcGenParams = toWorldGenParamsDTO defaultWorldGenParams
        { wgpLocationInstances = encounterInstances } }

currentPageCoreSignificant ∷ PageCoreDTO
currentPageCoreSignificant = currentPageCore
    { pcGenParams = toWorldGenParamsDTO defaultWorldGenParams
        { wgpLocationInstances = significantInstances } }

currentPageCoreEtymology ∷ PageCoreDTO
currentPageCoreEtymology = currentPageCore
    { pcGenParams = toWorldGenParamsDTO defaultWorldGenParams
        { wgpLocationInstances = namedInstances
            { lisById = HM.map (\i → i { liEtymology = Just keepSource })
                               (lisById namedInstances) }
        , wgpRiverNames = RiverNames (HM.singleton (GeoFeatureId 3)
              (RiverName "Vashendral" (Just "Ashen River") (Just ashenRiverSource))) }
    , pcIdentity = Just (WorldIdentityDTO "Legacy World" (Just "an old gloss")
        (Just (LanguageProvenanceDTO 0xABCDEF0123456789 1))
        (Just (toEtymologySourceDTO worldNameSource)))
    }

currentPageCore ∷ PageCoreDTO
currentPageCore = PageCoreDTO
    { pcPageId = WorldPageId "legacy_page"
    , pcGenParams = toWorldGenParamsDTO defaultWorldGenParams
    , pcCameraX = 1, pcCameraY = 2
    , pcTimeHour = 12, pcTimeMinute = 30, pcTimeRemainder = 0.25
    , pcDateYear = 1, pcDateMonth = 2, pcDateDay = 3
    , pcMapMode = ZMDefault
    , pcIdentity = Just (WorldIdentityDTO "Legacy World" (Just "an old gloss")
        (Just (LanguageProvenanceDTO 0xABCDEF0123456789 1)) Nothing)
    , pcGeneratedId = Just (fixtureGeneratedWorldIdForPage (WorldPageId "legacy_page"))
    , pcLocationTags = [], pcArenaBounds = Nothing, pcLocationScenarioRefs = []
    }

namedInstances ∷ LocationInstances
namedInstances = emptyInstances
    { lisById = HM.singleton (LocationInstanceId 4) location
        { liDisplayName = "Vashenkoro", liGloss = Just "Ashen Keep" } }

significantInstances ∷ LocationInstances
significantInstances = emptyInstances
    { lisById = HM.singleton (LocationInstanceId 4) location
        { liSignificant =
            [ LocationSignificantItem 1 "processing_unit" (Just 6101) True
            , LocationSignificantItem 2 "processing_unit" (Just 6102) False ]
        , liClearEventEmitted = True } }

encounterInstances ∷ LocationInstances
encounterInstances = emptyInstances
    { lisById = HM.singleton (LocationInstanceId 4) location
        { liDisplayName = "Vashenkoro", liGloss = Just "Ashen Keep"
        , liLifecycle = LifecycleActive
        , liEncounter = Just LocationEncounter
            { leRolledCount = 2
            , leOccupants =
                [ LocationEncounterOccupant (UnitId 41) (79.5, 111.0) True False
                , LocationEncounterOccupant (UnitId 42) (81.0, 113.5) False True ]
            , leRosterComplete = True, leDeathOnlyClearance = True
            , leActivated = True, leEpisodeActive = True
            , leAggressionAnnounced = True, leDisengageAnnounced = False
            , leCleared = False } } }

emptyInstances ∷ LocationInstances
emptyInstances = LocationInstances
    { lisNextId = 7, lisById = HM.empty, lisPendingLegacy = Nothing
    , lisScenarioRefs = mempty, lisTags = emptyCategoryIndex }

location ∷ LocationInstance
location = LocationInstance
    { liId = LocationInstanceId 4, liDefId = "ruin_small"
    , liChunk = ChunkCoord 2 3, liAnchor = (80, 112)
    , liBounds = AbsBounds 78 110 82 114, liDisplayName = "Ruin"
    , liGloss = Nothing, liEtymology = Just keepSource
    , liLifecycle = LifecycleCleared, liContentsSpawned = True
    , liEncounter = Nothing, liSignificant = [], liContainers = []
    , liClearEventEmitted = False }

worldNameSource ∷ EtymologySource
worldNameSource = EtymologySource (Bare (ConceptId "LAND")) provenance

keepSource ∷ EtymologySource
keepSource = EtymologySource (Modifier (ConceptId "ASH") (ConceptId "KEEP")) provenance

ashenRiverSource ∷ EtymologySource
ashenRiverSource = EtymologySource (Modifier (ConceptId "ASH") (ConceptId "RIVER")) provenance

provenance ∷ LanguageProvenance
provenance = LanguageProvenance (LangSeed 0xABCDEF0123456789) (GeneratorVersion 1)
