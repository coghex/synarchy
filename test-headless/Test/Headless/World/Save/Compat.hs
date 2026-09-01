{-# LANGUAGE ScopedTypeVariables #-}
-- | The "save migrations" gate (issue #766, save-overhaul C4): proves
--   "World.Save.Compat.SessionV90"'s frozen DTO tree decodes REAL,
--   historical B1 envelope bytes — not merely this test's own encoder
--   output. Pure — no engine, no IO.
--
--   'trackedB1EnvelopeFixtureHex' below is byte-for-byte the SAME
--   fixture 'Test.Headless.World.Save.Envelope' tracked before #760
--   replaced it (commit 988c2727, "Introduce the tagged, checksummed
--   save envelope (#759, save-overhaul B1)") — recovered from git
--   history per the compatibility manifest's provenance field
--   (@docs/save_compat/manifest.json@, fixture id @b1-initial-session@).
--   It encodes a real @"session"@ component wrapping a genuine v90
--   'World.Save.Types.SaveData' value (single page @"main_world"@, seed
--   42, no entities) alongside a @"metadata"@ component whose OWN values
--   were hand-picked by that test's author independently of the actual
--   gameplay gen params (world size 64 / plate count 3, vs. the page's
--   REAL 'World.Generate.Types.defaultWorldGenParams' — world size 128 /
--   plate count 10) — B1 predates requirement 12's manifest/gameplay
--   agreement check entirely, so this is not a defect in the fixture,
--   just a pre-existing inconsistency requirement 12 (correctly)
--   still catches on migration. 'decodeSessionV90' alone (no
--   cross-validation) is what proves byte-compatibility; the full
--   'decodeSessionEnvelope' path is exercised separately in
--   "Test.Headless.World.Save.Components" against a self-consistent
--   hand-built fixture.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "save migrations"'@.
module Test.Headless.World.Save.Compat (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?), (.!=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Serialize as S
import qualified Data.Text as T
import Numeric (readHex)

import World.Save.Envelope
    ( decodeSaveEnvelopeMetadata, decodeSessionEnvelope, encodeSessionSnapshot
    , metadataComponentId, metadataComponentVersion
    , legacyMetadataComponentVersion, currentEnvelopeVersion
    , foreignOptionalComponentIds, LuaComponentSpec(..)
    , decodeSessionEnvelopeClassified, generationFailureProgress
    , LoadProgress(..) )
import World.Save.Serialize (loadPhaseFor)
import Engine.Load.Status (LoadPhase(..))
import World.Save.Envelope.Codec
    (decodeEnvelope, encodeEnvelope, dePayloads, deManifest)
import World.Save.Envelope.Types
    (defaultEnvelopeLimits, ComponentId(..), emComponents, cdId, cdVersion, cdRequired)
import World.Save.Component.Types
    (ComponentError(..), ComponentPhase(..), coreSessionComponentId
    , worldPagesComponentId)
import World.Save.Compat.SessionV90
import World.Save.Compat.MetadataV1 (SaveMetadataV1(..))
import World.Save.Types
    ( SaveMetadata(..), BuildingSnapshot(..), UnitSnapshot(..)
    , BuildingInstanceSnapshot(..), UnitInstanceSnapshot(..)
    , resolveLegacyLocationParams )
import Test.Headless.Location.Fixture (expectGeometry)
import Location.Types (emptyLocationRegistry)
import Location.Bounds (AbsBounds(..))
import World.Chunk.Types (ChunkCoord(..))
import Location.Instance
    ( LocationEncounter(..), LocationEncounterOccupant(..)
    , LocationInstance(..), LocationInstances(..), LocationInstanceId(..)
    , LocationLifecycle(..), instancesToList )
import World.Save.Snapshot
    (SessionSnapshot(..), PageSnapshot(..), LiveCameraSnapshot(..))
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
    , WorldGenParamsDTOv5(..), toWorldGenParamsDTOv5
    , toWorldGenParamsDTOv6
    , WorldPages(..), WorldIdentityDTO(..), WorldIdentityDTOv1(..)
    , WorldIdentityDTOv2(..)
    , LanguageProvenanceDTO(..), toEtymologySourceDTO, basePageSnapshots
    , migrateWorldPagesV1, migrateWorldPagesV2, migrateWorldPagesV3
    , migrateWorldPagesV4, migrateWorldPagesV5, migrateWorldPagesV6
    , migrateWorldPagesV7 )
import World.Save.Component.WorldGen
    ( LocationInstanceDTOv3(..), LocationInstancesDTOv3(..)
    , toLocationInstancesDTOv3, toRiverNamesDTO )
import Language.Etymology.Source (EtymologySource(..))
import Language.Etymology
    (EtymologyResult(..), Etymology(..), decomposeName, etyTokenText)
import Language.Semantic.Types (Catalogue, ConceptId(..), NameExpr(..))
import Language.Semantic.Catalogue ( conceptCataloguePath
                                   , conceptOrdinalPath, loadCatalogue )
import World.Render.Zoom.Types (ZoomMapMode(..))
import Language.Generated.Types
    (LanguageProvenance(..), LangSeed(..), GeneratorVersion(..))
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Base (GeoFeatureId(..))
import World.River.Naming
    (RiverName(..), RiverNames(..), riverNamesToList, emptyRiverNames)
import World.Page.Types (WorldPageId(..), WorldIdentity(..))
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Unit.Sim.Types (UnitSimState(..), MoveTarget(..), MoveHazardPolicy(..))
import Craft.Bills (CraftBills(..), CraftBill(..), BillId(..), BillMode(..))
import World.Save.Component.Entities (BillQueueDTOv1(..), CraftBillDTOv1(..))
import Power.Types (PowerNodes(..), PowerNode(..), PowerNodeId(..))
import Item.Ground (GroundItems(..))
import Item.Types (ItemInstance(..))
import Building.Knowledge
    (ContainerKnowledge(..), ContainerRecord(..), emptyContainerKnowledge)
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)
import World.Page.GeneratedId (newGeneratedWorldId)

hexDecode ∷ String → BS.ByteString
hexDecode = BS.pack . go
  where
    go (a:b:rest) = case readHex [a,b] of
        ((v,_):_) → v : go rest
        []        → error ("hexDecode: not a hex byte: " <> [a,b])
    go _          = []

-- | Extract the raw @"session"@ component payload from a structurally-
--   valid legacy envelope, bypassing 'World.Save.Envelope''s own
--   cross-validated migration entirely — exactly what this gate needs
--   to test 'decodeSessionV90' in isolation.
extractSessionPayload ∷ BS.ByteString → BS.ByteString
extractSessionPayload bytes =
    case decodeEnvelope defaultEnvelopeLimits 1
             (HS.fromList [ComponentId "metadata", ComponentId "session"])
             (HS.fromList [ComponentId "metadata", ComponentId "session"])
             bytes of
        Left err → error ("test setup: " <> show err)
        Right decoded → case HM.lookup (ComponentId "session")
                                (dePayloads decoded) of
            Just p  → p
            Nothing → error "test setup: session payload missing"

-- Manifest / canonical-summary parsing (requirement 14: the blocking
-- audit must actually decode/migrate/validate a declared fixture, not
-- merely check its checksum -- tools/save_compat_audit.py's own docstring
-- names this hspec gate as where that real cross-check lives, since only
-- Haskell can run the codec).

data ManifestFixtureRef = ManifestFixtureRef
    { mfrPath            ∷ !FilePath
    , mfrKind            ∷ !Text
    , mfrSha256          ∷ !(Maybe Text)
    , mfrExpectedSummary ∷ !(Maybe FilePath)
    }

instance Aeson.FromJSON ManifestFixtureRef where
    parseJSON = Aeson.withObject "fixture" $ \o → ManifestFixtureRef
        <$> o .: "path" <*> o .: "kind" <*> o .:? "sha256"
        <*> o .:? "expectedCanonicalSummary"

newtype ManifestBaseline = ManifestBaseline
    { mbFixtures ∷ [ManifestFixtureRef] }

instance Aeson.FromJSON ManifestBaseline where
    parseJSON = Aeson.withObject "baseline" $ \o →
        ManifestBaseline <$> o .: "fixtures"

newtype Manifest = Manifest { mBaselines ∷ [ManifestBaseline] }

instance Aeson.FromJSON Manifest where
    parseJSON = Aeson.withObject "manifest" $ \o → Manifest <$> o .: "baselines"

data ExpectedMeta = ExpectedMeta
    { emSeed ∷ !Word64, emWorldSize ∷ !Int, emPlateCount ∷ !Int
    , emWorldName ∷ !(Maybe Text), emWorldGloss ∷ !(Maybe Text)
    }

instance Aeson.FromJSON ExpectedMeta where
    parseJSON = Aeson.withObject "metadata" $ \o → ExpectedMeta
        <$> o .: "seed" <*> o .: "worldSize" <*> o .: "plateCount"
        <*> o .: "worldName" <*> o .: "worldGloss"

data ExpectedCamera = ExpectedCamera
    { ecOwnerPage ∷ !(Maybe Text), ecX ∷ !Float, ecY ∷ !Float
    , ecZoom ∷ !Float, ecFacing ∷ !Text
    }

instance Aeson.FromJSON ExpectedCamera where
    parseJSON = Aeson.withObject "camera" $ \o → ExpectedCamera
        <$> o .: "ownerPage" <*> o .: "x" <*> o .: "y"
        <*> o .: "zoom" <*> o .: "facing"

-- | Entity-level canonical values (round-3 review): the aggregate
--   counts above prove nothing about a migration that maps a valid
--   entity/job/reference to the WRONG value -- re-encode/fresh-decode
--   equivalence only proves the ALREADY-PRODUCED snapshot is self-
--   consistent, not that it's the value the fixture's own real content
--   actually means. These optional lists (default @[]@ via '.:?'/'.!=',
--   never required -- b1's fixture has no entities at all) let a
--   fixture's expected-summary pin down specific entities by id and
--   compare their real field values, not just how many exist.
data ExpectedBuilding = ExpectedBuilding
    { ebId ∷ !Word32, ebDefName ∷ !Text, ebAnchorX ∷ !Int, ebAnchorY ∷ !Int
    , ebGridZ ∷ !Int, ebBuildProgress ∷ !Float
    }

instance Aeson.FromJSON ExpectedBuilding where
    parseJSON = Aeson.withObject "building" $ \o → ExpectedBuilding
        <$> o .: "id" <*> o .: "defName" <*> o .: "anchorX" <*> o .: "anchorY"
        <*> o .: "gridZ" <*> o .: "buildProgress"

-- | An inventory/storage item's canonical values (round-7 review:
--   "canonical fixture validation records only groundItemCount... a
--   migration that drops or mis-maps a real item can pass"). Recurses
--   into 'eiiContents' so an ITEM-container (a first-aid kit) and its
--   nested contents are both checked, not just the outer instance.
data ExpectedItemInstance = ExpectedItemInstance
    { eiiDefName ∷ !Text, eiiInstanceId ∷ !Word64, eiiCurrentFill ∷ !Float
    , eiiQuality ∷ !Float, eiiCondition ∷ !Float, eiiWeight ∷ !Float
    , eiiContents ∷ ![ExpectedItemInstance]
    }

instance Aeson.FromJSON ExpectedItemInstance where
    parseJSON = Aeson.withObject "item" $ \o → ExpectedItemInstance
        <$> o .: "defName" <*> o .: "instanceId" <*> o .: "currentFill"
        <*> o .: "quality" <*> o .: "condition" <*> o .: "weight"
        <*> o .:? "contents" .!= []

data ExpectedUnit = ExpectedUnit
    { euId ∷ !Word32, euDefName ∷ !Text, euGridX ∷ !Float, euGridY ∷ !Float
    , euGridZ ∷ !Int, euFacing ∷ !Text, euActivity ∷ !Text, euPose ∷ !Text
    , euInventory ∷ ![ExpectedItemInstance]
    }

instance Aeson.FromJSON ExpectedUnit where
    parseJSON = Aeson.withObject "unit" $ \o → ExpectedUnit
        <$> o .: "id" <*> o .: "defName" <*> o .: "gridX" <*> o .: "gridY"
        <*> o .: "gridZ" <*> o .: "facing" <*> o .: "activity" <*> o .: "pose"
        <*> o .:? "inventory" .!= []

data ExpectedUnitSimState = ExpectedUnitSimState
    { eusUnitId ∷ !Word32, eusRealX ∷ !Float, eusRealY ∷ !Float
    , eusGridZ ∷ !Int, eusPose ∷ !Text, eusState ∷ !Text, eusFacing ∷ !Text
    }

instance Aeson.FromJSON ExpectedUnitSimState where
    parseJSON = Aeson.withObject "unitSimState" $ \o → ExpectedUnitSimState
        <$> o .: "unitId" <*> o .: "realX" <*> o .: "realY" <*> o .: "gridZ"
        <*> o .: "pose" <*> o .: "state" <*> o .: "facing"

data ExpectedCraftBill = ExpectedCraftBill
    { ecbId ∷ !Word32, ecbStation ∷ !Word32, ecbRecipe ∷ !Text
    , ecbRemaining ∷ !Int, ecbClaimant ∷ !(Maybe Word32), ecbMode ∷ !Text
    }

instance Aeson.FromJSON ExpectedCraftBill where
    parseJSON = Aeson.withObject "craftBill" $ \o → ExpectedCraftBill
        <$> o .: "id" <*> o .: "station" <*> o .: "recipe"
        <*> o .: "remaining" <*> o .: "claimant" <*> o .: "mode"

data ExpectedPowerNode = ExpectedPowerNode
    { epnId ∷ !Word32, epnBuilding ∷ !Word32, epnRole ∷ !Text
    , epnPeakWatts ∷ !Float, epnCapacityWh ∷ !Float, epnStoredWh ∷ !Float
    }

instance Aeson.FromJSON ExpectedPowerNode where
    parseJSON = Aeson.withObject "powerNode" $ \o → ExpectedPowerNode
        <$> o .: "id" <*> o .: "building" <*> o .: "role"
        <*> o .: "peakWatts" <*> o .: "capacityWh" <*> o .: "storedWh"

data ExpectedPage = ExpectedPage
    { epPageId ∷ !Text, epBuildingCount ∷ !Int, epUnitCount ∷ !Int
    , epUnitSimStateCount ∷ !Int, epCraftBillCount ∷ !Int
    , epPowerNodeCount ∷ !Int, epGroundItemCount ∷ !Int
    , epTimeHour ∷ !Int, epTimeMinute ∷ !Int
    , epDateYear ∷ !Int, epDateMonth ∷ !Int, epDateDay ∷ !Int
    , epMapMode ∷ !Text
    , epBuildings ∷ ![ExpectedBuilding]
    , epUnits ∷ ![ExpectedUnit]
    , epUnitSimStates ∷ ![ExpectedUnitSimState]
    , epCraftBills ∷ ![ExpectedCraftBill]
    , epPowerNodes ∷ ![ExpectedPowerNode]
    }

instance Aeson.FromJSON ExpectedPage where
    parseJSON = Aeson.withObject "page" $ \o → ExpectedPage
        <$> o .: "pageId" <*> o .: "buildingCount" <*> o .: "unitCount"
        <*> o .: "unitSimStateCount" <*> o .: "craftBillCount"
        <*> o .: "powerNodeCount" <*> o .: "groundItemCount"
        <*> o .: "timeHour" <*> o .: "timeMinute"
        <*> o .: "dateYear" <*> o .: "dateMonth" <*> o .: "dateDay"
        <*> o .: "mapMode"
        <*> o .:? "buildings" .!= []
        <*> o .:? "units" .!= []
        <*> o .:? "unitSimStates" .!= []
        <*> o .:? "craftBills" .!= []
        <*> o .:? "powerNodes" .!= []

data ExpectedSummary = ExpectedSummary
    { esMeta ∷ !ExpectedMeta
    , esGameTime ∷ !Double
    , esNextItemId ∷ !Word64
    , esNextBuildingId ∷ !Word32
    , esNextUnitId ∷ !Word32
    , esCamera ∷ !ExpectedCamera
    , esActivePage ∷ !Text
    , esVisiblePages ∷ ![Text]
    , esPages ∷ ![ExpectedPage]
    , esLuaComponentCount ∷ !Int
    , esIsMigratedLegacyBaseline ∷ !Bool
    }

instance Aeson.FromJSON ExpectedSummary where
    parseJSON = Aeson.withObject "expected summary" $ \o → ExpectedSummary
        <$> o .: "metadata" <*> o .: "gameTime" <*> o .: "nextItemId"
        <*> o .: "nextBuildingId" <*> o .: "nextUnitId" <*> o .: "camera"
        <*> o .: "activePage" <*> o .: "visiblePages"
        <*> o .: "pages" <*> o .: "luaComponentCount"
        <*> o .: "isMigratedLegacyBaseline"

decodeJSONFile ∷ Aeson.FromJSON a ⇒ FilePath → IO (Either String a)
decodeJSONFile path = Aeson.eitherDecode <$> BSL.readFile path

-- | Compare one real 'ItemInstance' against its declared canonical
--   values, recursing into 'iiContents' (round-7 review) so a
--   migration that maps a valid item to the wrong def, quantity, or
--   nested-container placement is caught -- not just "an item exists".
checkItemInstance ∷ ItemInstance → ExpectedItemInstance → Expectation
checkItemInstance actual expected = do
    iiDefName actual `shouldBe` eiiDefName expected
    iiInstanceId actual `shouldBe` eiiInstanceId expected
    iiCurrentFill actual `shouldBe` eiiCurrentFill expected
    iiQuality actual `shouldBe` eiiQuality expected
    iiCondition actual `shouldBe` eiiCondition expected
    iiWeight actual `shouldBe` eiiWeight expected
    length (iiContents actual) `shouldBe` length (eiiContents expected)
    forM_ (zip (iiContents actual) (eiiContents expected))
          (uncurry checkItemInstance)

spec ∷ Spec
spec = do
    describe "manifest-declared fixtures decode and migrate to their \
             \expected canonical result (requirement 14)" $ do
        it "every complete-session fixture with a tracked checksum \
           \matches docs/save_compat/manifest.json's own expected \
           \canonical summary" $ do
            manifestResult ← decodeJSONFile "docs/save_compat/manifest.json"
            manifest ← either
                (\err → expectationFailure ("manifest parse failed: " <> err)
                        >> fail "unreachable") pure manifestResult
            let checkable =
                    [ f | b ← mBaselines manifest, f ← mbFixtures b
                    , mfrKind f ≡ "complete-session"
                    , Just _ ← [mfrSha256 f] ]
            null checkable `shouldBe` False
            forM_ checkable $ \fixture → do
                bytes ← BS.readFile (mfrPath fixture)
                summaryPath ← maybe
                    (fail (mfrPath fixture <> ": complete-session fixture \
                                             \has no expectedCanonicalSummary"))
                    pure (mfrExpectedSummary fixture)
                summaryResult ← decodeJSONFile summaryPath
                expected ← either
                    (\err → expectationFailure (summaryPath <> ": " <> err)
                            >> fail "unreachable") pure summaryResult
                -- The real, live Lua registry always knows/requires
                -- exactly these two modules -- a fixture may legitimately
                -- carry them (the #764 baseline's does, so its real-engine
                -- probe counterpart can load it too), so this reader must
                -- recognize them the same way the real engine does. The
                -- legacy B1 fallback ignores these arguments entirely (it
                -- decodes its own hardcoded {metadata, session} pair), so
                -- this has no effect on that path.
                let luaNames = HS.fromList ["unit_ai", "building_spawn"]
                case decodeSessionEnvelope luaNames luaNames bytes of
                    Left err → expectationFailure
                        (mfrPath fixture <> ": " <> T.unpack err)
                    Right (rawMeta, snap, luaComponents, isMigrated) → do
                        let em = esMeta expected
                        smSeed rawMeta `shouldBe` emSeed em
                        smWorldSize rawMeta `shouldBe` emWorldSize em
                        smPlateCount rawMeta `shouldBe` emPlateCount em
                        smWorldName rawMeta `shouldBe` emWorldName em
                        smWorldGloss rawMeta `shouldBe` emWorldGloss em
                        snapActivePage snap `shouldBe` WorldPageId (esActivePage expected)
                        snapVisiblePages snap
                            `shouldBe` map WorldPageId (esVisiblePages expected)
                        length luaComponents `shouldBe` esLuaComponentCount expected
                        isMigrated `shouldBe` esIsMigratedLegacyBaseline expected

                        snapGameTime snap `shouldBe` esGameTime expected
                        snapNextItemId snap `shouldBe` esNextItemId expected
                        snapNextBuildingId snap `shouldBe` esNextBuildingId expected
                        snapNextUnitId snap `shouldBe` esNextUnitId expected
                        let cam = snapLiveCamera snap
                            ec = esCamera expected
                        lcsOwnerPage cam `shouldBe` fmap WorldPageId (ecOwnerPage ec)
                        lcsX cam `shouldBe` ecX ec
                        lcsY cam `shouldBe` ecY ec
                        lcsZoom cam `shouldBe` ecZoom ec
                        T.pack (show (lcsFacing cam)) `shouldBe` ecFacing ec

                        -- Round-9 review: the loop below only proved every
                        -- EXPECTED page exists -- a migration that creates
                        -- or retains an extra, undeclared page would pass
                        -- it (and the self-reencode equality check, which
                        -- only proves the migrated snapshot is consistent
                        -- with ITSELF) silently. Compare the exact set of
                        -- migrated page ids against the exact set the
                        -- fixture declares, so a hidden extra (or missing)
                        -- page is caught here, not just individually
                        -- present-or-absent ones.
                        HS.fromList (HM.keys (snapPages snap))
                            `shouldBe` HS.fromList
                                (map (WorldPageId . epPageId) (esPages expected))

                        forM_ (esPages expected) $ \ep →
                            case HM.lookup (WorldPageId (epPageId ep)) (snapPages snap) of
                                Nothing → expectationFailure
                                    (T.unpack (epPageId ep) <> ": page missing \
                                              \from migrated snapshot")
                                Just page → do
                                    HM.size (bsnInstances (pgsBuildings page))
                                        `shouldBe` epBuildingCount ep
                                    HM.size (usnInstances (pgsUnits page))
                                        `shouldBe` epUnitCount ep
                                    HM.size (pgsUnitSimStates page)
                                        `shouldBe` epUnitSimStateCount ep
                                    HM.size (cbsBills (pgsCraftBills page))
                                        `shouldBe` epCraftBillCount ep
                                    HM.size (pnsNodes (pgsPowerNodes page))
                                        `shouldBe` epPowerNodeCount ep
                                    HM.size (gisItems (pgsGroundItems page))
                                        `shouldBe` epGroundItemCount ep
                                    pgsTimeHour page `shouldBe` epTimeHour ep
                                    pgsTimeMinute page `shouldBe` epTimeMinute ep
                                    pgsDateYear page `shouldBe` epDateYear ep
                                    pgsDateMonth page `shouldBe` epDateMonth ep
                                    pgsDateDay page `shouldBe` epDateDay ep
                                    T.pack (show (pgsMapMode page))
                                        `shouldBe` epMapMode ep

                                    -- Entity-level values (round-3 review):
                                    -- an aggregate count can't catch a
                                    -- migration that maps a valid entity/
                                    -- job/reference to the WRONG value --
                                    -- look each declared entity up by its
                                    -- OWN id and compare real field values,
                                    -- not merely "one exists".
                                    forM_ (epBuildings ep) $ \eb →
                                        case HM.lookup (BuildingId (ebId eb))
                                                 (bsnInstances (pgsBuildings page)) of
                                            Nothing → expectationFailure
                                                ("building #" <> show (ebId eb)
                                                 <> " missing from migrated page")
                                            Just b → do
                                                bisDefName b `shouldBe` ebDefName eb
                                                bisAnchorX b `shouldBe` ebAnchorX eb
                                                bisAnchorY b `shouldBe` ebAnchorY eb
                                                bisGridZ b `shouldBe` ebGridZ eb
                                                bisBuildProgress b `shouldBe` ebBuildProgress eb

                                    forM_ (epUnits ep) $ \eu →
                                        case HM.lookup (UnitId (euId eu))
                                                 (usnInstances (pgsUnits page)) of
                                            Nothing → expectationFailure
                                                ("unit #" <> show (euId eu)
                                                 <> " missing from migrated page")
                                            Just u → do
                                                uisDefName u `shouldBe` euDefName eu
                                                uisGridX u `shouldBe` euGridX eu
                                                uisGridY u `shouldBe` euGridY eu
                                                uisGridZ u `shouldBe` euGridZ eu
                                                T.pack (show (uisFacing u)) `shouldBe` euFacing eu
                                                uisActivity u `shouldBe` euActivity eu
                                                uisPose u `shouldBe` euPose eu
                                                length (uisInventory u)
                                                    `shouldBe` length (euInventory eu)
                                                forM_ (euInventory eu) $ \eii →
                                                    case L.find
                                                             (\i → iiInstanceId i ≡ eiiInstanceId eii)
                                                             (uisInventory u) of
                                                        Nothing → expectationFailure
                                                            ("unit #" <> show (euId eu)
                                                             <> ": inventory item instance #"
                                                             <> show (eiiInstanceId eii)
                                                             <> " missing from migrated inventory")
                                                        Just item → checkItemInstance item eii

                                    forM_ (epUnitSimStates ep) $ \eus →
                                        case HM.lookup (UnitId (eusUnitId eus))
                                                 (pgsUnitSimStates page) of
                                            Nothing → expectationFailure
                                                ("unit-sim state for unit #"
                                                 <> show (eusUnitId eus)
                                                 <> " missing from migrated page")
                                            Just s → do
                                                usRealX s `shouldBe` eusRealX eus
                                                usRealY s `shouldBe` eusRealY eus
                                                usGridZ s `shouldBe` eusGridZ eus
                                                T.pack (show (usPose s)) `shouldBe` eusPose eus
                                                T.pack (show (usState s)) `shouldBe` eusState eus
                                                T.pack (show (usFacing s)) `shouldBe` eusFacing eus

                                    forM_ (epCraftBills ep) $ \ecb →
                                        case HM.lookup (BillId (ecbId ecb))
                                                 (cbsBills (pgsCraftBills page)) of
                                            Nothing → expectationFailure
                                                ("craft bill #" <> show (ecbId ecb)
                                                 <> " missing from migrated page")
                                            Just b → do
                                                unBuildingId (cbStation b) `shouldBe` ecbStation ecb
                                                cbRecipe b `shouldBe` ecbRecipe ecb
                                                cbRemaining b `shouldBe` ecbRemaining ecb
                                                fmap unUnitId (cbClaimant b) `shouldBe` ecbClaimant ecb
                                                T.pack (show (cbMode b)) `shouldBe` ecbMode ecb

                                    forM_ (epPowerNodes ep) $ \epn →
                                        case HM.lookup (PowerNodeId (epnId epn))
                                                 (pnsNodes (pgsPowerNodes page)) of
                                            Nothing → expectationFailure
                                                ("power node #" <> show (epnId epn)
                                                 <> " missing from migrated page")
                                            Just n → do
                                                unBuildingId (pnBuilding n) `shouldBe` epnBuilding epn
                                                T.pack (show (pnRole n)) `shouldBe` epnRole epn
                                                pnPeakWatts n `shouldBe` epnPeakWatts epn
                                                pnCapacityWh n `shouldBe` epnCapacityWh epn
                                                pnStoredWh n `shouldBe` epnStoredWh epn

                        -- Re-encode/fresh-decode equivalence: the migrated
                        -- snapshot, run back through the SAME current-format
                        -- encoder and decoder every ordinary save/load uses,
                        -- must reproduce itself exactly. This is what proves
                        -- the migration produced a FULLY faithful modern
                        -- snapshot (every field the derived Eq instance
                        -- covers), not merely one that happens to match the
                        -- handful of fields spot-checked above.
                        -- Required is exactly whatever Lua components THIS
                        -- decode actually reported (never the hardcoded
                        -- luaNames): a migrated B1 session carries none
                        -- (there is no live Lua VM in this pure hspec gate
                        -- to supply fresh unit_ai/building_spawn state the
                        -- way a real engine's next save always would), and
                        -- requiring them here would be testing an
                        -- impossible-in-pure-Haskell scenario, not a real
                        -- gap -- Test.Headless.World.Save.Compat.hs's own
                        -- probe counterpart already proves the real-engine
                        -- round trip in that case.
                        -- A pre-#911 fixture decodes with its old
                        -- per-chunk location flags PENDING; the load path
                        -- resolves them against the location registry
                        -- before publication, so the next save writes
                        -- resolved instances. Applying that same
                        -- resolution first is what makes this an
                        -- equivalence check on the shape production
                        -- actually re-encodes (these fixtures place no
                        -- locations, so the registry is empty).
                        -- #2021, on exactly the same terms: a pre-v9
                        -- fixture decodes with every page's
                        -- generated-world id ABSENT, and transactional
                        -- load staging mints a fresh one per page before
                        -- anything can be saved again -- so a re-encode
                        -- of the raw migrated value is a shape no real
                        -- save ever writes. Staging it here (and
                        -- deriving the metadata inventory from the
                        -- staged pages, as the next save's own metadata
                        -- is derived) keeps the equivalence honest.
                        (meta, resolved) ← stageGeneratedWorldIds
                            (rawMeta, resolveSnapshotLocations snap)
                        let reencoded =
                                encodeSessionSnapshot meta resolved luaComponents
                            actualLuaNames =
                                HS.fromList (map lcsId luaComponents)
                        case decodeSessionEnvelope luaNames actualLuaNames reencoded of
                            Left err → expectationFailure
                                (mfrPath fixture <> ": re-encode/fresh-decode \
                                                     \equivalence: " <> T.unpack err)
                            Right (meta', snap', _, _) → do
                                meta' `shouldBe` meta
                                snap' `shouldBe` resolved

        it "the manifest-driven canonical check's page-set comparison \
           \genuinely distinguishes an incomplete/extra page set from the \
           \real migrated one (round-9 review) -- the per-expected-page \
           \lookup loop above only proves every EXPECTED page exists; it \
           \never proved the migrated snapshot carries NO extra, \
           \undeclared page, so a migration that creates or retains one \
           \would previously have passed silently" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/c3-typed-reference-v1-multipage.bin"
            let luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelope luaNames luaNames bytes of
                Left err → expectationFailure
                    ("expected the tracked multipage fixture to migrate \
                     \cleanly: " <> T.unpack err)
                Right (_, snap, _, _) → do
                    let realPageIds = HS.fromList (HM.keys (snapPages snap))
                        incompleteExpected = HS.fromList [WorldPageId "page1"]
                    -- The real fixture genuinely carries a second page
                    -- ("page2") this incomplete set omits -- proving the
                    -- exact-set comparison this test relies on would
                    -- actually have caught that extra page as a mismatch,
                    -- not silently passed it the way the old
                    -- every-expected-page-exists-only loop did.
                    realPageIds `shouldNotBe` incompleteExpected
                    HS.member (WorldPageId "page2") realPageIds `shouldBe` True

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

    -- #1092: world-pages became v3 when WorldIdentity gained its
    -- optional language provenance. Every HISTORICAL shape must decode
    -- with that provenance ABSENT (#915's precedent) while carrying its
    -- name and gloss across byte-exact — a world named before
    -- provenance was recorded genuinely has no recoverable language,
    -- and inventing one would attach a false etymology to a real world.
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

    -- #1087: "container-knowledge" is the FIRST optional gameplay
    -- component, so this pair covers both sides of what that means for
    -- compatibility — an older tracked baseline that legitimately lacks
    -- it, and the new baseline that carries a genuinely POPULATED one.
    -- The second is what stops the tracked fixture from silently
    -- regressing to the empty default it would still decode as (the
    -- exact way #915's first fixture revision went wrong).
    describe "container knowledge across baselines (issue #1087)" $ do
        let luaNames = HS.fromList ["unit_ai", "building_spawn"]
            withFixture path k = do
                bytes ← BS.readFile path
                case decodeSessionEnvelope luaNames luaNames bytes of
                    Left err → expectationFailure
                        (path <> " did not decode: " <> T.unpack err)
                    Right (_, snap, _, _) → k
                        [ pgsContainerKnowledge p
                        | p ← HM.elems (snapPages snap) ]

        it "a tracked baseline written BEFORE the component existed \
           \decodes with every page's knowledge empty -- every container \
           \never-inspected, nothing inferred from live storage" $
            withFixture
                "test-headless/data/save-compat/g1-language-provenance.bin" $
                \ks → do
                    ks `shouldNotBe` []
                    ks `shouldSatisfy` all (≡ emptyContainerKnowledge)

        it "the tracked current-version baseline carries a genuinely \
           \NON-EMPTY knowledge payload, with a remembered item -- so \
           \this coverage can never silently decay into the same empty \
           \default the pre-#1087 baselines already prove" $
            withFixture
                "test-headless/data/save-compat/h1-container-knowledge.bin" $
                \ks → do
                    let records = concatMap (HM.elems ∘ ckRecords) ks
                    length records `shouldSatisfy` (> 0)
                    concatMap crItems records `shouldSatisfy` (not ∘ null)
                    map iiDefName (concatMap crItems records)
                        `shouldBe` ["bandage"]
                    map crStoredWeight records `shouldSatisfy` all (> 0)

    -- #1102's mirror of the pair above, and for the same reason: the
    -- new per-page river-name table decodes EMPTY for every baseline
    -- written before it existed, which is indistinguishable from a
    -- table that silently lost its contents unless some tracked fixture
    -- carries a genuinely POPULATED one.
    describe "river names across baselines (issue #1102)" $ do
        let luaNames = HS.fromList ["unit_ai", "building_spawn"]
            withSession path k = do
                bytes ← BS.readFile path
                case decodeSessionEnvelope luaNames luaNames bytes of
                    Left err → expectationFailure
                        (path <> " did not decode: " <> T.unpack err)
                    Right (_, snap, _, _) → k (HM.elems (snapPages snap))

            withRivers path k = do
                bytes ← BS.readFile path
                case decodeSessionEnvelope luaNames luaNames bytes of
                    Left err → expectationFailure
                        (path <> " did not decode: " <> T.unpack err)
                    Right (_, snap, _, _) → k
                        [ wgpRiverNames (pgsGenParams p)
                        | p ← HM.elems (snapPages snap) ]

        it "a tracked baseline written BEFORE rivers were named decodes \
           \with every page's river-name table empty -- no name is ever \
           \inferred for a world that was saved without one" $
            withRivers
                "test-headless/data/save-compat/i1-location-language-names.bin" $
                \ts → do
                    ts `shouldNotBe` []
                    ts `shouldSatisfy` all (≡ emptyRiverNames)

        it "the tracked current-version baseline carries genuinely NAMED \
           \rivers -- every entry a non-empty native name with a \
           \non-empty English gloss, so this coverage can never silently \
           \decay into the same empty default the pre-#1102 baselines \
           \already prove" $
            withRivers
                "test-headless/data/save-compat/j1-river-language-names.bin" $
                \ts → do
                    let named = concatMap riverNamesToList ts
                    length named `shouldSatisfy` (> 1)
                    map (rvnDisplayName ∘ snd) named
                        `shouldSatisfy` all (not ∘ T.null)
                    map (rvnGloss ∘ snd) named `shouldSatisfy` all
                        (maybe False (not ∘ T.null))

        it "a tracked baseline written BEFORE names recorded their \
           \expressions decodes with NO etymology source anywhere -- not \
           \on the page identity, not on a location, not on a river -- \
           \even though those names and glosses are all still there, so \
           \nothing is ever inferred from the text that survived" $
            withSession
                "test-headless/data/save-compat/j1-river-language-names.bin" $
                \pages → do
                    pages `shouldNotBe` []
                    -- The names ARE present, which is what makes the
                    -- absent sources below a real decode outcome rather
                    -- than an empty page proving nothing.
                    [ n | p ← pages, Just n ← [pgsIdentity p] ]
                        `shouldSatisfy` (not ∘ null)
                    [ () | p ← pages, Just i ← [pgsIdentity p]
                         , Just _ ← [wiEtymology i] ]
                        `shouldBe` []
                    [ () | p ← pages
                         , li ← instancesToList
                                    (wgpLocationInstances (pgsGenParams p))
                         , Just _ ← [liEtymology li] ]
                        `shouldBe` []
                    [ () | p ← pages
                         , (_, rn) ← riverNamesToList
                                         (wgpRiverNames (pgsGenParams p))
                         , Just _ ← [rvnEtymology rn] ]
                        `shouldBe` []

        it "the tracked current-version baseline carries a genuine \
           \etymology source on its page identity AND on every river it \
           \named -- each one reconstructing the very name it is stored \
           \beside, so this coverage can never silently decay into the \
           \same absent default the pre-#1104 baselines already prove" $
            withSession
                "test-headless/data/save-compat/k1-name-etymology.bin" $
                \pages → do
                    let idents = [ i | p ← pages, Just i ← [pgsIdentity p] ]
                        rivers = [ rn | p ← pages
                                 , (_, rn) ← riverNamesToList
                                       (wgpRiverNames (pgsGenParams p)) ]
                    -- The world's own name.
                    map wiEtymology idents `shouldSatisfy` any isJust
                    -- Every river the page named.
                    rivers `shouldSatisfy` (not ∘ null)
                    map rvnEtymology rivers `shouldSatisfy` all isJust
                    -- And each source really explains the name it sits
                    -- beside: rendered through its OWN recorded
                    -- language, the surface tokens reproduce the stored
                    -- text exactly. A source that had been defaulted,
                    -- swapped between entries, or carried across from
                    -- another page could not satisfy this.
                    cat ← loadRealCatalogue
                    forM_ idents $ \i → forM_ (wiEtymology i) $ \_ →
                        decomposeName cat (wiName i) (wiGloss i)
                                      (wiEtymology i)
                            `shouldSatisfy` isAvailableFor (wiName i)
                    forM_ rivers $ \rn →
                        decomposeName cat (rvnDisplayName rn) (rvnGloss rn)
                                      (rvnEtymology rn)
                            `shouldSatisfy` isAvailableFor (rvnDisplayName rn)

    describe "movement hazard policy across baselines (issue #1217)" $ do
        -- The unit-sim component went to v3 so a move target could carry
        -- the request's damaging-drop policy. These two cases are the
        -- pair: an OLD baseline must default to fall-permitted (the
        -- behavior its bytes were written under), and the CURRENT one
        -- must carry a genuinely FallProhibited target, so this coverage
        -- can never decay into asserting the default on both sides.
        let withTargets path k = do
                bytes ← BS.readFile path
                case decodeSessionEnvelope luaNames luaNames bytes of
                    Left err → expectationFailure
                        (path <> " did not decode: " <> T.unpack err)
                    Right (_, snap, _, _) → k
                        [ mt | p ← HM.elems (snapPages snap)
                             , ss ← HM.elems (pgsUnitSimStates p)
                             , Just mt ← [usTarget ss] ]
            luaNames = HS.fromList ["unit_ai", "building_spawn"]

        it "a tracked baseline written BEFORE the hazard policy decodes \
           \with every in-flight move target fall-permitted -- an old \
           \save's routes keep behaving exactly as they did, and no \
           \policy is ever inferred from a speed or a destination" $
            withTargets
                "test-headless/data/save-compat/l1-order-stall-budget.bin" $
                \mts → do
                    mts `shouldNotBe` []
                    map mtHazard mts `shouldSatisfy` all (≡ FallPermitted)

        it "the tracked current-version baseline carries a genuinely \
           \FALL-PROHIBITED in-flight target, so this coverage can never \
           \silently decay into the same permitted default the pre-#1217 \
           \baselines already prove" $
            withTargets
                "test-headless/data/save-compat/o1-wander-hazard-policy.bin" $
                \mts → do
                    mts `shouldNotBe` []
                    map mtHazard mts `shouldSatisfy` all (≡ FallProhibited)

    describe "unknown optional data in a legacy envelope (requirement 9)" $ do
        it "refuses to migrate a legacy envelope carrying an extra \
           \optional component beyond {metadata, session}, rather than \
           \silently dropping it" $ do
            let extraSpecs =
                    [ (metadataComponentId, legacyMetadataComponentVersion, True
                      , S.encode minimalSaveMetadataV1ForExtra)
                    , (ComponentId "session", sessionComponentVersion, True
                      , extractSessionPayload fixtureBytes)
                    , (ComponentId "future-thing", 1, False, BS.pack [9, 9, 9])
                    ]
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion extraSpecs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Right _   → expectationFailure
                    "expected the extra optional component to be rejected, \
                    \not silently dropped"
                Left msg  → msg `shouldSatisfy` T.isInfixOf "future-thing"

        it "the overwrite guard recognizes a legacy {metadata, session} \
           \envelope as carrying NO foreign data (session itself is a \
           \recognized, migratable shape, not foreign)" $ do
            let plainSpecs =
                    [ (metadataComponentId, legacyMetadataComponentVersion, True
                      , S.encode minimalSaveMetadataV1ForExtra)
                    , (ComponentId "session", sessionComponentVersion, True
                      , extractSessionPayload fixtureBytes)
                    ]
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion plainSpecs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            foreignOptionalComponentIds HS.empty bytes `shouldBe` []

        it "the overwrite guard DOES flag a legacy envelope's genuinely \
           \extra optional component as foreign data" $ do
            let extraSpecs =
                    [ (metadataComponentId, legacyMetadataComponentVersion, True
                      , S.encode minimalSaveMetadataV1ForExtra)
                    , (ComponentId "session", sessionComponentVersion, True
                      , extractSessionPayload fixtureBytes)
                    , (ComponentId "future-thing", 1, False, BS.pack [9, 9, 9])
                    ]
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion extraSpecs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            foreignOptionalComponentIds HS.empty bytes
                `shouldBe` [ComponentId "future-thing"]

        it "the overwrite guard does NOT exempt an id merely spelled \
           \\"session\" when the envelope ISN'T the exact {metadata, \
           \session} legacy shape (round-4 review) -- a modern-shaped \
           \envelope carrying an unrelated optional component that \
           \happens to be named \"session\" is genuinely foreign, and \
           \exempting it just because of that name would silently drop \
           \it on the next save" $ do
            let modernShapedWithSessionNamedExtra =
                    [ (metadataComponentId, metadataComponentVersion, True
                      , S.encode minimalSaveMetadataForExtra)
                    , (ComponentId "world-pages", 1, True, BS.pack [1, 2, 3])
                    , (ComponentId "session", 1, False, BS.pack [4, 5, 6])
                    ]
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion modernShapedWithSessionNamedExtra of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            foreignOptionalComponentIds HS.empty bytes
                `shouldBe` [ComponentId "session"]

        it "refuses to migrate an envelope shaped {metadata, session} \
           \whose \"session\" descriptor is marked OPTIONAL, not \
           \required (round-7 review) -- a genuine B1 envelope's writer \
           \always marks BOTH descriptors required; an envelope that \
           \merely matches the id set and version but not the required \
           \flag is not the real frozen shape, and must not be silently \
           \migrated as if it were" $ do
            let optionalSessionSpecs =
                    [ (metadataComponentId, legacyMetadataComponentVersion, True
                      , S.encode minimalSaveMetadataV1ForExtra)
                    , (ComponentId "session", sessionComponentVersion, False
                      , extractSessionPayload fixtureBytes)
                    ]
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion optionalSessionSpecs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Right _  → expectationFailure
                    "expected an envelope with an OPTIONAL session \
                    \descriptor to be rejected, not migrated"
                Left msg → msg `shouldSatisfy` T.isInfixOf "required"

        it "the overwrite guard does NOT exempt \"session\" when its OWN \
           \descriptor is marked optional (round-7 review) -- otherwise \
           \this exact envelope shape would be treated as \"no foreign \
           \data\" and get silently overwritten on the next save, \
           \discarding whatever the optional session payload actually \
           \was" $ do
            let optionalSessionSpecs =
                    [ (metadataComponentId, legacyMetadataComponentVersion, True
                      , S.encode minimalSaveMetadataV1ForExtra)
                    , (ComponentId "session", sessionComponentVersion, False
                      , extractSessionPayload fixtureBytes)
                    ]
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion optionalSessionSpecs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            foreignOptionalComponentIds HS.empty bytes
                `shouldBe` [ComponentId "session"]

        it "the overwrite guard does NOT exempt \"session\" when it is \
           \\"metadata\" (not \"session\") whose descriptor is marked \
           \optional (round-9 review) -- decodeLegacyStructureAndMetadata \
           \checks BOTH descriptors' required flag, so an envelope with a \
           \perfectly exact, required \"session\" alongside an OPTIONAL \
           \\"metadata\" is not real B1 shape either, and the guard must \
           \independently reach that same conclusion rather than exempt \
           \\"session\" merely because IT happens to be exact" $ do
            let optionalMetadataSpecs =
                    [ (metadataComponentId, legacyMetadataComponentVersion, False
                      , S.encode minimalSaveMetadataV1ForExtra)
                    , (ComponentId "session", sessionComponentVersion, True
                      , extractSessionPayload fixtureBytes)
                    ]
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion optionalMetadataSpecs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Right _  → expectationFailure
                    "expected an envelope with an OPTIONAL metadata \
                    \descriptor to be rejected, not migrated"
                Left msg → msg `shouldSatisfy` T.isInfixOf "required"
            foreignOptionalComponentIds HS.empty bytes
                `shouldBe` [ComponentId "session"]

        it "the overwrite guard does NOT exempt \"lua-state\" merely \
           \because the envelope alongside it LOOKS B1-shaped (round-10 \
           \review) -- an envelope {metadata required v1, session \
           \required v90, lua-state optional v1} is neither genuine B1 \
           \(B1 never carries \"lua-state\") nor genuine B2 (B2 never \
           \carries \"session\"), so \"lua-state\" must be reported as \
           \foreign data, not silently exempted just because it always \
           \rides along in the shared known-set the INITIAL decode needs" $ do
            let extraLuaStateSpecs =
                    [ (metadataComponentId, legacyMetadataComponentVersion, True
                      , S.encode minimalSaveMetadataV1ForExtra)
                    , (ComponentId "session", sessionComponentVersion, True
                      , extractSessionPayload fixtureBytes)
                    , (ComponentId "lua-state", 1, False, BS.empty)
                    ]
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion extraLuaStateSpecs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Right _  → expectationFailure
                    "expected an envelope carrying \"lua-state\" alongside \
                    \{metadata, session} to be rejected -- it is neither \
                    \the exact B1 nor the exact B2 shape"
                Left _   → pure ()
            foreignOptionalComponentIds HS.empty bytes
                `shouldBe` [ComponentId "lua-state"]

    describe "the #760-era (\"B2\") fallback (issue #766 requirement 3, \
             \round-7 review)" $ do
        it "migrates the real, tracked B2-shaped fixture (empty lua-state \
           \blob), and the overwrite guard recognizes it as carrying no \
           \foreign data" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            let luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelope luaNames luaNames bytes of
                Left err → expectationFailure
                    ("expected the B2 fixture to migrate cleanly: "
                     <> T.unpack err)
                Right (_, _, luaComponents, isMigrated) → do
                    isMigrated `shouldBe` True
                    luaComponents `shouldBe` []
            foreignOptionalComponentIds HS.empty bytes `shouldBe` []

        -- Issue #1919, review round 1. Both legacy fallbacks run REAL
        -- component machinery (B1's decodeSessionV90/migrateSessionV90,
        -- B2's assembleSnapshot), so their failures carry
        -- 'ComponentPhase's just as the modern path's do. Those phases
        -- used to survive only because the load-status layer
        -- substring-matched them back out of the rendered text; now they
        -- must be transported structurally through
        -- 'decodeSessionEnvelopeClassified' or 'failedAtPhase' silently
        -- regresses for exactly these saves.
        it "carries a B2 assembly failure's COMPONENT phases through the \
           \classified path, not a flattened envelope-level guess" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            -- An empty page set: every component still decodes, and only
            -- validation/assembly rejects it -- the phases the pre-#1919
            -- substring parser reported as LoadComponentsMigrated.
            let tampered = replaceB2ComponentSpec bytes worldPagesComponentId
                               (versionOfB2Component bytes worldPagesComponentId)
                               True
                               (S.encode (WorldPagesDTO []))
                luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelopeClassified luaNames luaNames tampered of
                Right _ → expectationFailure
                    "expected an empty B2 page set to be refused"
                Left failure → do
                    let progress = generationFailureProgress failure
                    case progress of
                        ReachedComponents phases →
                            phases `shouldSatisfy`
                                all (\ph → ph ≡ ValidatePhase ∨ ph ≡ AssemblePhase)
                        other → expectationFailure
                            ("expected component progress, got " <> show other)
                    loadPhaseFor progress `shouldBe` LoadComponentsMigrated

        it "carries a B2 per-component DECODE failure's phase through the \
           \classified path" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            let tampered = replaceB2ComponentSpec bytes coreSessionComponentId
                               999 True
                               (payloadOfB2Component bytes coreSessionComponentId)
                luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelopeClassified luaNames luaNames tampered of
                Right _ → expectationFailure
                    "expected an unsupported core-session version to be refused"
                Left failure → do
                    generationFailureProgress failure
                        `shouldBe` ReachedComponents [DecodePhase]
                    loadPhaseFor (generationFailureProgress failure)
                        `shouldBe` LoadEnvelopeValidated

        it "still reports a genuinely NON-component B2 failure at the \
           \envelope level -- a malformed lua-state blob never reached a \
           \component phase, so it must not borrow one" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            let tampered = replaceB2LuaStateSpec bytes 1 True (BS.pack [1, 2, 3])
                luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelopeClassified luaNames luaNames tampered of
                Right _ → expectationFailure
                    "expected a malformed lua-state blob to be refused"
                Left failure → do
                    generationFailureProgress failure `shouldBe` ReachedEnvelope
                    loadPhaseFor (generationFailureProgress failure)
                        `shouldBe` LoadEnvelopeValidated

        it "refuses to migrate a B2-shaped envelope whose \"lua-state\" \
           \blob decodes to a WELL-FORMED but NON-EMPTY HashMap Text Text \
           \(round-18 review: the real pre-#761 sdLuaModules/ \
           \snapLuaModules shape, not a hand-wavy 'non-empty bytes' stand-\
           \in) -- the pre-#761 Lua deserializer that could interpret it \
           \was removed, so it cannot be honestly migrated, mirroring \
           \migrateSessionV90's identical policy for B1's own legacy Lua \
           \blob" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            let realNonEmptyMap =
                    HM.fromList [("unit_ai", "some real persisted AI state")]
                tampered = replaceB2LuaStateSpec bytes 1 True (S.encode realNonEmptyMap)
                luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelope luaNames luaNames tampered of
                Right _  → expectationFailure
                    "expected a non-empty lua-state map to be refused"
                Left msg → msg `shouldSatisfy` T.isInfixOf "lua-state"

        it "refuses to migrate a B2-shaped envelope whose \"lua-state\" \
           \blob is genuinely MALFORMED -- not a valid HashMap Text Text \
           \at all (round-18 review: distinct from the well-formed-but-\
           \non-empty case above; malformed bytes must be refused as \
           \malformed, never silently treated as an acceptable empty \
           \state)" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            let tampered = replaceB2LuaStateSpec bytes 1 True (BS.pack [1, 2, 3])
                luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelope luaNames luaNames tampered of
                Right _  → expectationFailure
                    "expected a malformed lua-state blob to be refused"
                Left msg → msg `shouldSatisfy` T.isInfixOf "lua-state"

        it "migrates a B2-shaped envelope whose \"lua-state\" blob is the \
           \REAL cereal-encoded empty HashMap Text Text (round-18 review: \
           \8 bytes -- a Word64 zero length-prefix -- NOT a literal zero-\
           \byte BS.empty payload, which a genuine #760 writer's cereal \
           \encoder never actually produces for an empty map)" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            let realEmptyMap = HM.empty ∷ HM.HashMap Text Text
                tampered = replaceB2LuaStateSpec bytes 1 True (S.encode realEmptyMap)
                luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelope luaNames luaNames tampered of
                Left err → expectationFailure
                    ("expected the real cereal-encoded empty map to migrate "
                     <> "cleanly: " <> T.unpack err)
                Right (_, _, luaComponents, isMigrated) → do
                    isMigrated `shouldBe` True
                    luaComponents `shouldBe` []

        it "refuses to migrate a B2-shaped envelope whose \"lua-state\" \
           \descriptor is marked OPTIONAL, not required -- mirrors the B1 \
           \fallback's identical precision (round-7 review): a genuine \
           \#760 writer always marked it required" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            let tampered = replaceB2LuaStateSpec bytes 1 False BS.empty
                luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelope luaNames luaNames tampered of
                Right _  → expectationFailure
                    "expected an optional lua-state descriptor to be refused"
                Left msg → msg `shouldSatisfy` T.isInfixOf "required"

        it "refuses to migrate a B2-shaped envelope whose \"lua-state\" \
           \descriptor claims a schema version OTHER than the one genuine \
           \#760 writers always used, even though it is required and \
           \EMPTY -- round-8 review: an unsupported/future lua-state \
           \schema must not be silently accepted (and then re-saved \
           \without ever recording that unknown version) just because it \
           \happens to share the required flag and an empty payload with \
           \the recognized v1 shape" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            let tampered = replaceB2LuaStateSpec bytes 2 True BS.empty
                luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelope luaNames luaNames tampered of
                Right _  → expectationFailure
                    "expected a wrong-version (v2) lua-state descriptor to \
                    \be refused rather than treated as the known v1 shape"
                Left msg → msg `shouldSatisfy` T.isInfixOf "lua-state"
            -- The overwrite guard must independently reach the same
            -- conclusion: this is NOT the recognized B2 shape, so
            -- "lua-state" is ordinary foreign data, not exempted.
            foreignOptionalComponentIds HS.empty tampered
                `shouldBe` [ComponentId "lua-state"]

        it "refuses to migrate a B2-shaped envelope whose \"core-session\" \
           \descriptor (a Haskell component OTHER than \"lua-state\") is \
           \marked OPTIONAL -- round-9 review: decodeB2StructureAndMetadata \
           \checks EVERY id in the B2 set for required, not merely \
           \\"lua-state\", and the overwrite guard must reach the \
           \identical conclusion rather than exempt the whole shape just \
           \because \"lua-state\" itself is exact" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            let tampered = replaceB2ComponentSpec bytes
                    (ComponentId "core-session") 1 False
                    (payloadOfB2Component bytes (ComponentId "core-session"))
                luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelope luaNames luaNames tampered of
                Right _  → expectationFailure
                    "expected an optional \"core-session\" descriptor to \
                    \be refused, not treated as the genuine B2 shape"
                Left msg → msg `shouldSatisfy` T.isInfixOf "required"
            foreignOptionalComponentIds HS.empty tampered
                `shouldNotBe` []

-- | Rebuild the tracked B2 fixture's envelope with ONE component's
--   (version, required, payload) replaced -- every OTHER component's
--   id/version/required/payload carried over verbatim from the real
--   fixture -- so a test can exercise exactly one tampered descriptor at
--   a time against otherwise-genuine bytes.
replaceB2ComponentSpec
    ∷ BS.ByteString → ComponentId → Word32 → Bool → BS.ByteString
    → BS.ByteString
replaceB2ComponentSpec bytes targetCid ver req payload =
    case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
             knownAllB2Ids HS.empty bytes of
        Left e → error ("test setup: replaceB2ComponentSpec: decode: " <> show e)
        Right decoded →
            let otherSpecs =
                    [ (cdId d, cdVersion d, cdRequired d, payloadFor decoded (cdId d))
                    | d ← emComponents (deManifest decoded)
                    , cdId d ≢ targetCid ]
                newSpecs = otherSpecs ⧺ [(targetCid, ver, req, payload)]
            in case encodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion newSpecs of
                Right b → b
                Left e  → error ("test setup: replaceB2ComponentSpec: encode: " <> show e)
  where
    payloadFor decoded cid = HM.lookupDefault
        (error ("test setup: payload missing for " <> show cid)) cid
        (dePayloads decoded)

-- | The tracked B2 fixture's own already-encoded payload for one
--   component id, unchanged -- so a test tampering with only that
--   component's (version, required) flags can carry its real payload
--   forward verbatim rather than fabricate one.
payloadOfB2Component ∷ BS.ByteString → ComponentId → BS.ByteString
payloadOfB2Component bytes cid =
    case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
             knownAllB2Ids HS.empty bytes of
        Left e → error ("test setup: payloadOfB2Component: decode: " <> show e)
        Right decoded → HM.lookupDefault
            (error ("test setup: payload missing for " <> show cid)) cid
            (dePayloads decoded)

-- | The tracked B2 fixture's own declared schema version for one
--   component id -- so a test replacing that component's PAYLOAD keeps
--   its real historical version rather than hard-coding a number that
--   would silently drift into an unsupported-version test instead.
versionOfB2Component ∷ BS.ByteString → ComponentId → Word32
versionOfB2Component bytes cid =
    case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
             knownAllB2Ids HS.empty bytes of
        Left e → error ("test setup: versionOfB2Component: decode: " <> show e)
        Right decoded → case findDesc decoded of
            Just v  → v
            Nothing → error ("test setup: descriptor missing for " <> show cid)
  where
    findDesc decoded =
        listToMaybe [ cdVersion d | d ← emComponents (deManifest decoded)
                                  , cdId d ≡ cid ]

-- | The exact id set the tracked B2 fixture carries -- see its own
--   manifest entry's components[] list.
knownAllB2Ids ∷ HS.HashSet ComponentId
knownAllB2Ids = HS.fromList
    [ ComponentId "metadata", ComponentId "core-session"
    , ComponentId "texture-palette", ComponentId "world-pages"
    , ComponentId "world-edits", ComponentId "world-activity"
    , ComponentId "buildings", ComponentId "units"
    , ComponentId "unit-sim", ComponentId "craft-bills"
    , ComponentId "power-nodes", ComponentId "lua-state" ]

-- | 'replaceB2ComponentSpec' specialized to "lua-state", preserved as its
--   own name since every existing lua-state-focused test reads more
--   clearly calling it directly.
replaceB2LuaStateSpec
    ∷ BS.ByteString → Word32 → Bool → BS.ByteString → BS.ByteString
replaceB2LuaStateSpec bytes = replaceB2ComponentSpec bytes (ComponentId "lua-state")

-- | A metadata value that agrees with the extracted fixture session's own
--   gameplay gen params (seed 42 / world size 128 / plate count 10 — see
--   the frozen v90 DTO test above), used by the requirement-9 tests: they
--   are not testing requirement 12's metadata-agreement check, so must
--   not trip over it.
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
            , leClearEventEmitted = False
            }
        }) (lisById richInstances)
    }

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

-- | The production concept catalogue, read from disk. The tracked
--   fixture's sources reference real concept ids, so only the real
--   catalogue can decompose them — a hand-built stub would prove
--   nothing about the file the engine actually ships.
loadRealCatalogue ∷ IO Catalogue
loadRealCatalogue = do
    eCat ← loadCatalogue conceptCataloguePath conceptOrdinalPath
    case eCat of
        Right cat → pure cat
        Left err  → error ("test setup: catalogue: " <> show err)

-- | Whether a decomposition succeeded AND its surface tokens
--   concatenate back to the exact stored name — the #1104 requirement 3
--   property, asserted here against real saved bytes rather than a
--   constructed value.
isAvailableFor ∷ Text → EtymologyResult → Bool
isAvailableFor stored (EtyAvailable ety) =
    etyName ety ≡ stored
    ∧ T.concat (map etyTokenText (etyTokens ety)) ≡ stored
isAvailableFor _ (EtyUnavailable _) = False

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

minimalSaveMetadataForExtra ∷ SaveMetadata
minimalSaveMetadataForExtra = SaveMetadata
    { smName = "extra-test", smSeed = 42, smWorldSize = 128, smPlateCount = 10
    , smTimestamp = "2026-07-16T00:00:00.000000Z"
    , smWorldName = Nothing, smWorldGloss = Nothing, smAutosave = False
    , smGeneratedWorldIds = []
    }

-- | The SAME values in the frozen v1 metadata shape (#913). A hand-built
--   LEGACY envelope must carry v1 metadata, not the current one: a real
--   B1 file was written while metadata was still at v1, and the B1
--   recognizer pins that historical version deliberately
--   ('World.Save.Envelope.legacyMetadataComponentVersion') so a metadata
--   bump can never stop this build recognizing its own frozen baseline.
minimalSaveMetadataV1ForExtra ∷ SaveMetadataV1
minimalSaveMetadataV1ForExtra = SaveMetadataV1
    { sm1Name = "extra-test", sm1Seed = 42, sm1WorldSize = 128
    , sm1PlateCount = 10
    , sm1Timestamp = "2026-07-16T00:00:00.000000Z"
    , sm1WorldName = Nothing, sm1WorldGloss = Nothing
    }

-- | Byte-for-byte the SAME fixture 'Test.Headless.World.Save.Envelope'
--   tracked immediately after #759 landed (commit 988c2727), before #760
--   replaced it — see the module haddock for provenance and exactly what
--   it encodes. Never regenerate this from current code: HEAD's codec
--   can no longer produce a single-@"session"@-component envelope at
--   all (that is the whole point of this fixture).
fixtureBytes ∷ BS.ByteString
fixtureBytes = hexDecode trackedB1EnvelopeFixtureHex

trackedB1EnvelopeFixtureHex ∷ String
trackedB1EnvelopeFixtureHex =
    "535952410000000100000000000000610000000000000002000000000000\
    \00086d657461646174610000000101000000000000000000000000000000\
    \80b6ce951fb0e97917000000000000000773657373696f6e0000005a0100\
    \0000000000008000000000000003e3e5f920542dab08fab95b839d58d4e5\
    \290000000000000012656e76656c6f70655f746573745f73617665000000\
    \000000002a00000000000000400000000000000003000000000000001b32\
    \3032362d30372d31365430303a30303a30302e3030303030305a01000000\
    \000000000a5465737420576f726c6401000000000000000f612066697874\
    \75726520776f726c640000000000000012656e76656c6f70655f74657374\
    \5f73617665000000000000002a0000000000000040000000000000000300\
    \0000000000001b323032362d30372d31365430303a30303a30302e303030\
    \3030305a01000000000000000a5465737420576f726c6401000000000000\
    \000f61206669787475726520776f726c6400000000000000000100000000\
    \000000000000000000000000000000000000000000000000000000010000\
    \00000000000a6d61696e5f776f726c640000000000000001000000000000\
    \000a6d61696e5f776f726c640000000000000001000000000000000a6d61\
    \696e5f776f726c64000000000000002a0000000000000080000000000000\
    \000a0000000000000000000000000000001e000000000000000c00000000\
    \00000018000000000000003c3ecccccd3f000000000000000000001c0000\
    \000000000000000000000000000000000080000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000010000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000032\
    \3f8000003e99999a3f3333333fc000003f8000003f0000003f8333330000\
    \000000000000000000000000002000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000003f800000000000003f800000\
    \3f3333333fa0000000000000000000060000000000000016000000000000\
    \000c3f8000003f8000003f80000000000000000000010000000000000002\
    \000000000000000100000000000000030000000000000001000000000000\
    \000300000000000000010000000000000003000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000003f80\
    \000000000000000000000c00000000000000000000000000000001000000\
    \000000000100000000000000013f80000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000010000000000000000000000\
    \010000000000000000000000000000000000000000000000000000000000\
    \000000000000010000000000000000000000010000000000000000000000\
    \0000000000000000000000000000"

-- | The load path's own pre-#911 location resolution
--   ('World.Save.Types.resolveLegacyLocationParams'), applied to every
--   page of a decoded snapshot.
-- | #2021: mint a fresh 'GeneratedWorldId' for every page that has
--   none, and rebuild the metadata inventory from the result — the two
--   things transactional load staging and the following save do, in
--   that order, to a session decoded from a pre-v9 payload.
--
--   The manifest gate re-encodes a migrated fixture and fresh-decodes
--   it, asserting the two agree. Without this step that re-encode would
--   write a v9 @world-pages@ payload carrying no ids at all — a shape no
--   real save can produce (every live page has an id) and one the reader
--   correctly refuses. Applying staging first is the same move
--   'resolveSnapshotLocations' already makes for #911's pending
--   locations: compare against the shape production actually writes.
stageGeneratedWorldIds
    ∷ (SaveMetadata, SessionSnapshot) → IO (SaveMetadata, SessionSnapshot)
stageGeneratedWorldIds (meta, snap) = do
    staged ← traverse stagePage (snapPages snap)
    let ids = L.sort (L.nub [ gid | p ← HM.elems staged
                                  , Just gid ← [pgsGeneratedId p] ])
    pure ( meta { smGeneratedWorldIds = ids }
         , snap { snapPages = staged } )
  where
    stagePage p = case pgsGeneratedId p of
        Just _  → pure p
        Nothing → do
            gid ← newGeneratedWorldId
            pure p { pgsGeneratedId = Just gid }

resolveSnapshotLocations ∷ SessionSnapshot → SessionSnapshot
resolveSnapshotLocations snap = snap
    { snapPages = HM.map resolvePage (snapPages snap) }
  where
    resolvePage p = p
        { pgsGenParams = expectGeometry
            (resolveLegacyLocationParams emptyLocationRegistry
                                         (pgsGenParams p)) }
