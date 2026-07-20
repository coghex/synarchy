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
import Data.Aeson ((.:), (.:?))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Serialize as S
import qualified Data.Text as T
import Numeric (readHex)

import World.Save.Envelope
    ( decodeSaveEnvelopeMetadata, decodeSessionEnvelope, encodeSessionSnapshot
    , metadataComponentId, metadataComponentVersion, currentEnvelopeVersion
    , foreignOptionalComponentIds )
import World.Save.Envelope.Codec (decodeEnvelope, encodeEnvelope, dePayloads)
import World.Save.Envelope.Types (defaultEnvelopeLimits, ComponentId(..))
import World.Save.Compat.SessionV90
import World.Save.Types
    ( SaveMetadata(..), BuildingSnapshot(..), UnitSnapshot(..) )
import World.Save.Snapshot
    (SessionSnapshot(..), PageSnapshot(..), LiveCameraSnapshot(..))
import World.Save.Component.Page (fromWorldGenParamsDTO)
import World.Generate.Types (WorldGenParams(..))
import World.Page.Types (WorldPageId(..))
import Craft.Bills (CraftBills(..))
import Power.Types (PowerNodes(..))
import Item.Ground (GroundItems(..))

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

data ExpectedPage = ExpectedPage
    { epPageId ∷ !Text, epBuildingCount ∷ !Int, epUnitCount ∷ !Int
    , epUnitSimStateCount ∷ !Int, epCraftBillCount ∷ !Int
    , epPowerNodeCount ∷ !Int, epGroundItemCount ∷ !Int
    , epTimeHour ∷ !Int, epTimeMinute ∷ !Int
    , epDateYear ∷ !Int, epDateMonth ∷ !Int, epDateDay ∷ !Int
    , epMapMode ∷ !Text
    }

instance Aeson.FromJSON ExpectedPage where
    parseJSON = Aeson.withObject "page" $ \o → ExpectedPage
        <$> o .: "pageId" <*> o .: "buildingCount" <*> o .: "unitCount"
        <*> o .: "unitSimStateCount" <*> o .: "craftBillCount"
        <*> o .: "powerNodeCount" <*> o .: "groundItemCount"
        <*> o .: "timeHour" <*> o .: "timeMinute"
        <*> o .: "dateYear" <*> o .: "dateMonth" <*> o .: "dateDay"
        <*> o .: "mapMode"

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

spec ∷ Spec
spec = do
    describe "manifest-declared fixtures decode and migrate to their \
             \expected canonical result (requirement 14)" $
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
