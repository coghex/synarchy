-- | The tracked-baseline family of the "save migrations" gate (issue
--   #766, save-overhaul C4; split out by #2094): every contract that
--   reads a fixture @docs/save_compat/manifest.json@ declares and
--   proves something about the value it migrates to — the
--   manifest-driven canonical-summary check, and the per-baseline
--   container-knowledge (#1087), river-name and etymology (#1102,
--   #1104) and movement-hazard (#1217) pairs. Pure — no engine; the
--   only IO is read-only access to the tracked fixtures, the manifest,
--   and the shipped concept catalogue.
--
--   Each describe group is exported on its own so the aggregate
--   ("Test.Headless.World.Save.Compat") can sequence it among the other
--   families' groups in the order the suite has always run in; this
--   module registers nothing itself. The manifest model, the expected
--   canonical-summary schema (@tools/save_compat_audit_codec.py@'s
--   @GHCI_DUMP_SUMMARY_TEMPLATE@ mirrors it by hand) and the
--   staging helpers live here because only this family reads them.
module Test.Headless.World.Save.Compat.Baselines
    ( manifestFixturesSpec
    , containerKnowledgeSpec
    , riverNamesSpec
    , movementHazardSpec
    ) where

import UPrelude
import Test.Hspec
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?), (.!=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Text as T

import World.Save.Envelope
    (decodeSessionEnvelope, encodeSessionSnapshot, LuaComponentSpec(..))
import World.Save.Types
    ( SaveMetadata(..), BuildingSnapshot(..), UnitSnapshot(..)
    , BuildingInstanceSnapshot(..), UnitInstanceSnapshot(..)
    , resolveLegacyLocationParams )
import Test.Headless.Location.Fixture (expectGeometry)
import Location.Types (emptyLocationRegistry)
import Location.Instance (LocationInstance(..), instancesToList)
import World.Save.Snapshot
    (SessionSnapshot(..), PageSnapshot(..), LiveCameraSnapshot(..))
import Language.Etymology
    (EtymologyResult(..), Etymology(..), decomposeName, etyTokenText)
import Language.Semantic.Types (Catalogue)
import Language.Semantic.Catalogue ( conceptCataloguePath
                                   , conceptOrdinalPath, loadCatalogue )
import World.Generate.Types (WorldGenParams(..))
import World.River.Naming (RiverName(..), riverNamesToList, emptyRiverNames)
import World.Page.Types (WorldPageId(..), WorldIdentity(..))
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Unit.Sim.Types (UnitSimState(..), MoveTarget(..), MoveHazardPolicy(..))
import Craft.Bills (CraftBills(..), CraftBill(..), BillId(..))
import Power.Types (PowerNodes(..), PowerNode(..), PowerNodeId(..))
import Item.Ground (GroundItems(..))
import Item.Types (ItemInstance(..))
import Building.Knowledge
    (ContainerKnowledge(..), ContainerRecord(..), emptyContainerKnowledge)
import World.Page.GeneratedId (newGeneratedWorldId)

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

-- | Requirement 14: every manifest-declared complete-session fixture
--   decodes, migrates and re-encodes to exactly the canonical summary
--   it declares.
manifestFixturesSpec ∷ Spec
manifestFixturesSpec =
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

-- | #1087: "container-knowledge" is the FIRST optional gameplay
--   component, so this pair covers both sides of what that means for
--   compatibility — an older tracked baseline that legitimately lacks
--   it, and the new baseline that carries a genuinely POPULATED one.
--   The second is what stops the tracked fixture from silently
--   regressing to the empty default it would still decode as (the
--   exact way #915's first fixture revision went wrong).
containerKnowledgeSpec ∷ Spec
containerKnowledgeSpec =
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

-- | #1102's mirror of the pair above, and for the same reason: the
--   new per-page river-name table decodes EMPTY for every baseline
--   written before it existed, which is indistinguishable from a
--   table that silently lost its contents unless some tracked fixture
--   carries a genuinely POPULATED one.
riverNamesSpec ∷ Spec
riverNamesSpec =
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

-- | #1217: the unit-sim component's move-target hazard policy across
--   a pre-policy baseline and the current one.
movementHazardSpec ∷ Spec
movementHazardSpec =
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


-- | The load path's own pre-#911 location resolution
--   ('World.Save.Types.resolveLegacyLocationParams'), applied to every
--   page of a decoded snapshot.
resolveSnapshotLocations ∷ SessionSnapshot → SessionSnapshot
resolveSnapshotLocations snap = snap
    { snapPages = HM.map resolvePage (snapPages snap) }
  where
    resolvePage p = p
        { pgsGenParams = expectGeometry
            (resolveLegacyLocationParams emptyLocationRegistry
                                         (pgsGenParams p)) }
