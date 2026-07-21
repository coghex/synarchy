{-# LANGUAGE UnicodeSyntax, OverloadedStrings, ScopedTypeVariables #-}
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
    , metadataComponentId, metadataComponentVersion, currentEnvelopeVersion
    , foreignOptionalComponentIds )
import World.Save.Envelope.Codec
    (decodeEnvelope, encodeEnvelope, dePayloads, deManifest)
import World.Save.Envelope.Types
    (defaultEnvelopeLimits, ComponentId(..), emComponents, cdId, cdVersion, cdRequired)
import World.Save.Component.Types (ComponentError(..))
import World.Save.Compat.SessionV90
import World.Save.Types
    ( SaveMetadata(..), BuildingSnapshot(..), UnitSnapshot(..)
    , BuildingInstanceSnapshot(..), UnitInstanceSnapshot(..) )
import World.Save.Snapshot
    (SessionSnapshot(..), PageSnapshot(..), LiveCameraSnapshot(..))
import World.Save.Component.Page (fromWorldGenParamsDTO)
import World.Generate.Types (WorldGenParams(..))
import World.Page.Types (WorldPageId(..))
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Unit.Sim.Types (UnitSimState(..))
import Craft.Bills (CraftBills(..), CraftBill(..), BillId(..), BillMode(..))
import World.Save.Component.Entities (BillQueueDTOv1(..), CraftBillDTOv1(..))
import Power.Types (PowerNodes(..), PowerNode(..), PowerNodeId(..))
import Item.Ground (GroundItems(..))
import Item.Types (ItemInstance(..))

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

data ManifestBaseline = ManifestBaseline
    { mbId ∷ !Text, mbFixtures ∷ ![ManifestFixtureRef] }

instance Aeson.FromJSON ManifestBaseline where
    parseJSON = Aeson.withObject "baseline" $ \o →
        ManifestBaseline <$> o .: "id" <*> o .: "fixtures"

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
                    Right (meta, snap, luaComponents, isMigrated) → do
                        let em = esMeta expected
                        smSeed meta `shouldBe` emSeed em
                        smWorldSize meta `shouldBe` emWorldSize em
                        smPlateCount meta `shouldBe` emPlateCount em
                        smWorldName meta `shouldBe` emWorldName em
                        smWorldGloss meta `shouldBe` emWorldGloss em
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
                        let reencoded = encodeSessionSnapshot meta snap luaComponents
                            actualLuaNames =
                                HS.fromList [ n | (n, _, _, _) ← luaComponents ]
                        case decodeSessionEnvelope luaNames actualLuaNames reencoded of
                            Left err → expectationFailure
                                (mfrPath fixture <> ": re-encode/fresh-decode \
                                                     \equivalence: " <> T.unpack err)
                            Right (meta', snap', _, _) → do
                                meta' `shouldBe` meta
                                snap' `shouldBe` snap

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
                    , smWorldGloss = Just "a fixture world"
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
                            let gp = fromWorldGenParamsDTO (wp90GenParams p)
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

    describe "unknown optional data in a legacy envelope (requirement 9)" $ do
        it "refuses to migrate a legacy envelope carrying an extra \
           \optional component beyond {metadata, session}, rather than \
           \silently dropping it" $ do
            let extraSpecs =
                    [ (metadataComponentId, metadataComponentVersion, True
                      , S.encode minimalSaveMetadataForExtra)
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
                    [ (metadataComponentId, metadataComponentVersion, True
                      , S.encode minimalSaveMetadataForExtra)
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
                    [ (metadataComponentId, metadataComponentVersion, True
                      , S.encode minimalSaveMetadataForExtra)
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
                    [ (metadataComponentId, metadataComponentVersion, True
                      , S.encode minimalSaveMetadataForExtra)
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
                    [ (metadataComponentId, metadataComponentVersion, True
                      , S.encode minimalSaveMetadataForExtra)
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
                    [ (metadataComponentId, metadataComponentVersion, False
                      , S.encode minimalSaveMetadataForExtra)
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
                    [ (metadataComponentId, metadataComponentVersion, True
                      , S.encode minimalSaveMetadataForExtra)
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
minimalSaveMetadataForExtra ∷ SaveMetadata
minimalSaveMetadataForExtra = SaveMetadata
    { smName = "extra-test", smSeed = 42, smWorldSize = 128, smPlateCount = 10
    , smTimestamp = "2026-07-16T00:00:00.000000Z"
    , smWorldName = Nothing, smWorldGloss = Nothing
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
