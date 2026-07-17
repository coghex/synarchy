{-# LANGUAGE UnicodeSyntax, OverloadedStrings, ScopedTypeVariables #-}
-- | The "save components" gate (issue #760, save-overhaul B2): the
--   Haskell-owned persistence component split that replaced B1's single
--   transitional @"session"@ payload. Pure — no engine, no IO. Every
--   'SessionSnapshot' below is a synthetic literal, the same pattern
--   'Test.Headless.Save.Snapshot' uses one layer up.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "save components"'@.
module Test.Headless.World.Save.Components (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import qualified Data.Text as T
import Numeric (readHex)

import qualified Data.HashSet as HS
import World.Save.Envelope
import World.Save.Envelope.Codec (encodeEnvelope, decodeEnvelope, deManifest)
import World.Save.Envelope.Types
    (defaultEnvelopeLimits, ComponentId(..), EnvelopeManifest(..)
    , ComponentDescriptor(..))
import World.Save.Component
import World.Save.Component.Types
import World.Save.Component.Session
import World.Save.Component.Page
import World.Save.Component.Entities
import World.Save.Snapshot
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotSaveMetadata)
import World.Save.Types
    ( SaveMetadata(..), BuildingSnapshot(..), BuildingInstanceSnapshot(..)
    , UnitSnapshot(..), UnitInstanceSnapshot(..) )
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..), WorldIdentity(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Structure.Palette (emptyTexPalette)
import Item.Ground (emptyGroundItems)
import Item.Types (ItemInstance)
import World.Spoil.Types (emptySpoilPiles)
import World.Flora.Harvest (emptyFloraHarvests)
import World.Flora.CropPlot (emptyCropPlots)
import World.Edit.Types (emptyWorldEdits)
import Craft.Bills (emptyCraftBills)
import Power.Types (emptyPowerNodes)
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Unit.Sim.Types (UnitSimState(..), Pose(..), UnitActivity(..))
import Unit.Direction (Direction(..))

-- ---------------------------------------------------------------------
-- Fixtures (mirror Test.Headless.Save.Snapshot's minimal* pattern)
-- ---------------------------------------------------------------------

page1, page2 ∷ WorldPageId
page1 = WorldPageId "page1"
page2 = WorldPageId "page2"

-- | 'WorldGenParams''s manual cereal instance DERIVES a few nested
--   fields (e.g. the volcanism config's own seed/world-size) from
--   'wgpSeed'/'wgpWorldSize' on decode rather than storing them, so a
--   hand-built default whose nested seeds don't already agree is not a
--   serialize fixpoint. Real gen params (produced by worldgen) always
--   agree; here we reach that fixpoint explicitly by one decode∘encode,
--   so a full-equality snapshot round trip is meaningful. This changes
--   only the in-memory value — the ENCODED bytes are identical either
--   way (the derived fields are never written), so the frozen fixture
--   below stays valid.
canon ∷ WorldGenParams → WorldGenParams
canon gp = case S.decode (S.encode gp) of
    Right gp' → gp'
    Left err  → error ("canon: " <> err)

defaultGP ∷ WorldGenParams
defaultGP = canon defaultWorldGenParams

minimalPage ∷ WorldPageId → PageSnapshot
minimalPage pid = PageSnapshot
    { pgsPageId       = pid
    , pgsGenParams    = defaultGP
    , pgsCameraX      = 0
    , pgsCameraY      = 0
    , pgsTimeHour     = 12
    , pgsTimeMinute   = 0
    , pgsDateYear     = 1
    , pgsDateMonth    = 1
    , pgsDateDay      = 1
    , pgsMapMode      = ZMDefault
    , pgsEdits        = emptyWorldEdits
    , pgsMineDesignations      = HM.empty
    , pgsConstructDesignations = HM.empty
    , pgsGroundItems  = emptyGroundItems
    , pgsSpoilPiles   = emptySpoilPiles
    , pgsBuildings    = BuildingSnapshot { bsnInstances = HM.empty, bsnNextId = 1 }
    , pgsUnits        = UnitSnapshot { usnInstances = HM.empty, usnNextId = 1 }
    , pgsUnitSimStates = HM.empty
    , pgsFloraHarvests = emptyFloraHarvests
    , pgsChopDesignations = HM.empty
    , pgsCraftBills   = emptyCraftBills
    , pgsPowerNodes   = emptyPowerNodes
    , pgsTillDesignations = HM.empty
    , pgsCropPlots    = emptyCropPlots
    , pgsPlantDesignations = HM.empty
    , pgsIdentity     = Nothing
    }

-- | A page carrying a distinctive seed + identity + one building, one
--   unit, one sim state — enough that a round trip that dropped a slice
--   would be observable.
richPage ∷ WorldPageId → PageSnapshot
richPage pid = (minimalPage pid)
    { pgsGenParams = canon (defaultWorldGenParams { wgpSeed = 123456 })
    , pgsIdentity  = Just (WorldIdentity "Rich World" (Just "a gloss"))
    , pgsBuildings = BuildingSnapshot
        { bsnInstances = HM.singleton (BuildingId 1) (minimalBuildingInstance [])
        , bsnNextId = 5 }
    , pgsUnits = UnitSnapshot
        { usnInstances = HM.singleton (UnitId 1) (minimalUnitInstance [])
        , usnNextId = 5 }
    , pgsUnitSimStates = HM.singleton (UnitId 1) minimalSimState
    }

minimalGlobals ∷ SessionGlobals
minimalGlobals = SessionGlobals
    { sgGameTime       = 42
    , sgTexPalette     = emptyTexPalette
    , sgNextItemId     = 1
    , sgNextBuildingId = 10
    , sgNextUnitId     = 10
    , sgLuaModules     = HM.singleton "pause" "blob"
    , sgActivePage     = page1
    , sgVisiblePages   = [page1]
    , sgLiveCamera     = LiveCameraSnapshot
        { lcsOwnerPage = Just page1
        , lcsX = 7, lcsY = 8, lcsZoom = 3, lcsFacing = FaceEast }
    }

minimalUnitInstance ∷ [ItemInstance] → UnitInstanceSnapshot
minimalUnitInstance inv = UnitInstanceSnapshot
    { uisDefName = "test_unit", uisBaseWidth = 1, uisGridX = 0, uisGridY = 0
    , uisGridZ = 0, uisFacing = DirS, uisCurrentAnim = "", uisAnimStart = 0
    , uisAnimReverse = False, uisActivity = "idle", uisPose = "standing"
    , uisAnimStride = 0, uisStats = HM.empty, uisModifiers = HM.empty
    , uisSkills = HM.empty, uisKnowledge = HM.empty, uisInventory = inv
    , uisEquipped = HM.empty, uisAccessories = [], uisFactionId = ""
    , uisWounds = [], uisScars = [], uisImmuneResponse = 0
    , uisImmunities = HM.empty, uisBlood = 5, uisName = "" }

minimalBuildingInstance ∷ [ItemInstance] → BuildingInstanceSnapshot
minimalBuildingInstance storage = BuildingInstanceSnapshot
    { bisDefName = "test_building", bisAnchorX = 0, bisAnchorY = 0
    , bisGridZ = 0, bisSpawnedAt = 0, bisTileW = 1, bisTileH = 1
    , bisSpawnRemaining = 0, bisBuildProgress = 100
    , bisMaterialsDelivered = HM.empty, bisStorage = storage }

minimalSimState ∷ UnitSimState
minimalSimState = UnitSimState
    { usRealX = 0, usRealY = 0, usGridZ = 0, usRealZ = 0
    , usTarget = Nothing, usPose = Standing, usState = Idle, usFacing = DirS
    , usLocalPath = []
    , usDrinkUntil = Nothing, usEatUntil = Nothing, usPickupUntil = Nothing
    , usTransitionUntil = Nothing, usTransitionStride = 0, usPostTransition = []
    , usClimbFromTile = Nothing, usClimbToTile = Nothing, usClimbStartTime = Nothing
    , usClimbSlipAt = Nothing, usFallFromTile = Nothing, usFallToTile = Nothing
    , usPendingClimbXP = 0, usGetUpAt = Nothing, usPendingFallDrop = Nothing
    , usJumpApex = Nothing, usMoveGrade = 0 }

-- A valid, captured multi-page snapshot + its metadata.
richSnapshot ∷ SessionSnapshot
richSnapshot = case captureSessionSnapshot
        minimalGlobals { sgNextItemId = 100 }
        [richPage page1, minimalPage page2] of
    Right s   → s
    Left errs → error ("richSnapshot invalid: " <> show errs)

richMeta ∷ SaveMetadata
richMeta = snapshotSaveMetadata
    (SaveRequestMeta { srmSlotName = "slot", srmTimestamp = "ts" }) richSnapshot

encodeRich ∷ BS.ByteString
encodeRich = encodeSessionSnapshot richMeta richSnapshot

pageCore ∷ WorldPageId → PageCoreDTO
pageCore pid = PageCoreDTO
    { pcPageId = pid, pcGenParams = defaultGP
    , pcCameraX = 0, pcCameraY = 0, pcTimeHour = 0, pcTimeMinute = 0
    , pcDateYear = 1, pcDateMonth = 1, pcDateDay = 1, pcMapMode = ZMDefault
    , pcIdentity = Nothing }

hexDecode ∷ String → BS.ByteString
hexDecode = BS.pack . go
  where
    go (a:b:rest) = case readHex [a,b] of
        ((v,_):_) → v : go rest
        []        → error ("hexDecode: not a hex byte: " <> [a,b])
    go _          = []

isLeft ∷ Either a b → Bool
isLeft (Left _) = True
isLeft _        = False

spec ∷ Spec
spec = do
    describe "registry contract" $ do
        it "the authoritative registry is structurally well-formed \
           \(no duplicate ids, deps resolve, no cycles)" $
            registryStaticErrors `shouldBe` []

        it "topologically orders every component after its dependencies" $
            case dependencyOrder saveComponentRegistry of
                Left cyc → expectationFailure ("unexpected cycle: " <> show cyc)
                Right ordered → do
                    let ids = map rcId ordered
                        before a b = case (elemIndex' a ids, elemIndex' b ids) of
                            (Just i, Just j) → i < j
                            _                → False
                    -- world-pages precedes everything that depends on it
                    before worldPagesComponentId coreSessionComponentId
                        `shouldBe` True
                    before unitsComponentId unitSimComponentId `shouldBe` True
                    before buildingsComponentId craftBillsComponentId
                        `shouldBe` True

        it "rejects a dependency cycle in the registry" $ do
            let a = stubComponent (ComponentId "a") [ComponentId "b"]
                b = stubComponent (ComponentId "b") [ComponentId "a"]
            isLeft (dependencyOrder [a, b]) `shouldBe` True

        it "every gameplay component is required (none safely defaultable, \
           \requirement 7)" $
            all rcRequired saveComponentRegistry `shouldBe` True

    describe "per-component codecs" $ do
        it "each component round-trips its own slice of the snapshot" $ do
            let check enc dec = case dec (enc richSnapshot) of
                    Right _  → pure () ∷ IO ()
                    Left e   → expectationFailure (T.unpack (renderComponentError e))
            check (ccEncode coreSessionCodec)   (ccDecode coreSessionCodec 1)
            check (ccEncode worldPagesCodec)    (ccDecode worldPagesCodec 1)
            check (ccEncode buildingsCodec)     (ccDecode buildingsCodec 1)
            check (ccEncode unitsCodec)         (ccDecode unitsCodec 1)
            check (ccEncode unitSimCodec)       (ccDecode unitSimCodec 1)
            check (ccEncode craftBillsCodec)    (ccDecode craftBillsCodec 1)
            check (ccEncode powerNodesCodec)    (ccDecode powerNodesCodec 1)
            check (ccEncode worldEditsCodec)    (ccDecode worldEditsCodec 1)
            check (ccEncode worldActivityCodec) (ccDecode worldActivityCodec 1)
            check (ccEncode texPaletteCodec)    (ccDecode texPaletteCodec 1)
            check (ccEncode luaStateCodec)      (ccDecode luaStateCodec 1)

        it "declares a stable id and current version of 1" $ do
            ccId coreSessionCodec `shouldBe` coreSessionComponentId
            ccVersion coreSessionCodec `shouldBe` 1
            ccVersion worldPagesCodec `shouldBe` 1

        it "rejects a NEWER unsupported version, naming the phase" $
            case ccDecode worldPagesCodec 999 (ccEncode worldPagesCodec richSnapshot) of
                Left e  → do
                    cePhase e `shouldBe` DecodePhase
                    ceVersion e `shouldBe` 999
                Right _ → expectationFailure "expected version rejection"

        it "rejects an OLDER unsupported version" $
            ccDecode buildingsCodec 0 (ccEncode buildingsCodec richSnapshot)
                `shouldSatisfy` isLeftC

        it "rejects a truncated / malformed payload" $
            case ccDecode coreSessionCodec 1 (BS.pack [1,2,3]) of
                Left e  → cePhase e `shouldBe` DecodePhase
                Right _ → expectationFailure "expected malformed-payload rejection"

        it "world-pages self-validates a duplicate page id (component-local \
           \invariant)" $ do
            let dup = WorldPagesDTO [pageCore page1, pageCore page1]
            ccValidate worldPagesCodec dup `shouldSatisfy` (not . null)

        it "world-pages self-validates an empty page set" $
            ccValidate worldPagesCodec (WorldPagesDTO []) `shouldSatisfy` (not . null)

        it "converts snapshot ↔ DTO with no live-state reads: the world \
           \seed survives the round trip (a meaningful seed stays present, \
           \requirement 10)" $
            case ccDecode worldPagesCodec 1 (ccEncode worldPagesCodec richSnapshot) of
                Right (WorldPagesDTO ps) →
                    [ wgpSeed (pcGenParams p) | p ← ps, pcPageId p ≡ page1 ]
                        `shouldBe` [123456]
                Left e → expectationFailure (T.unpack (renderComponentError e))

    describe "page-scoping (requirement 8)" $ do
        it "rejects a page-scoped slice set missing a page the authority \
           \declares" $ do
            let base = basePageSnapshots (WorldPagesDTO [pageCore page1, pageCore page2])
                bad  = BuildingsDTO
                    [ PageBuildingsDTO page1
                        (BuildingSnapshot HM.empty 1) ]  -- page2 missing
            applyBuildings bad base `shouldSatisfy` isLeft

        it "rejects a page-scoped slice for a page the authority does NOT \
           \declare" $ do
            let base = basePageSnapshots (WorldPagesDTO [pageCore page1])
                bad  = BuildingsDTO
                    [ PageBuildingsDTO page1 (BuildingSnapshot HM.empty 1)
                    , PageBuildingsDTO page2 (BuildingSnapshot HM.empty 1) ]
            applyBuildings bad base `shouldSatisfy` isLeft

        it "accepts a slice set matching the authority exactly" $ do
            let base = basePageSnapshots (WorldPagesDTO [pageCore page1, pageCore page2])
                ok   = BuildingsDTO
                    [ PageBuildingsDTO page1 (BuildingSnapshot HM.empty 1)
                    , PageBuildingsDTO page2 (BuildingSnapshot HM.empty 1) ]
            applyBuildings ok base `shouldSatisfy` (not . isLeft)

    describe "production envelope (encode ↔ decode)" $ do
        it "round-trips a complete multi-page session through \
           \encodeSessionSnapshot / decodeSessionEnvelope, reconstructing \
           \the EXACT snapshot" $
            case decodeSessionEnvelope encodeRich of
                Left err → expectationFailure (T.unpack err)
                Right (meta, snap) → do
                    meta `shouldBe` richMeta
                    snap `shouldBe` richSnapshot

        it "inspects metadata WITHOUT decoding gameplay" $
            decodeSaveEnvelopeMetadata encodeRich `shouldBe` Right richMeta

        it "writes exactly the documented component set and NO transitional \
           \monolithic session component" $
            case decodeEnvelopeIds encodeRich of
                Left err → expectationFailure (T.unpack err)
                Right ids → do
                    let expected = [ metadataComponentId, coreSessionComponentId
                                   , worldPagesComponentId, buildingsComponentId
                                   , unitsComponentId, unitSimComponentId
                                   , craftBillsComponentId, powerNodesComponentId
                                   , worldEditsComponentId, worldActivityComponentId
                                   , texPaletteComponentId, luaStateComponentId ]
                    ids `shouldMatchList` expected
                    (ComponentId "session" `elem` ids) `shouldBe` False

    describe "assembly cross-validation (requirement 6/9/12)" $ do
        it "rejects a manifest/gameplay metadata mismatch" $ do
            let wrongMeta = richMeta { smSeed = 999999 }
                bytes = encodeSessionSnapshot wrongMeta richSnapshot
            case decodeSessionEnvelope bytes of
                Left msg → msg `shouldSatisfy` T.isInfixOf "seed"
                Right _  → expectationFailure "expected a metadata mismatch rejection"

        it "rejects an orphaned unit sim state (a sim owner with no unit)" $ do
            let orphanPage = (minimalPage page1)
                    { pgsUnitSimStates = HM.singleton (UnitId 77) minimalSimState }
                snap = buildSessionSnapshot minimalGlobals [orphanPage]
                meta = snapshotSaveMetadata
                         (SaveRequestMeta "s" "t") snap
                bytes = encodeSessionSnapshot meta snap
            decodeSessionEnvelope bytes `shouldSatisfy` isLeft

        it "rejects an allocator collision (a building id at/above the \
           \allocator)" $ do
            let badPage = (minimalPage page1)
                    { pgsBuildings = BuildingSnapshot
                        (HM.singleton (BuildingId 50) (minimalBuildingInstance []))
                        51 }
                snap = buildSessionSnapshot
                         minimalGlobals { sgNextBuildingId = 50 } [badPage]
                meta = snapshotSaveMetadata (SaveRequestMeta "s" "t") snap
                bytes = encodeSessionSnapshot meta snap
            decodeSessionEnvelope bytes `shouldSatisfy` isLeft

        it "rejects a missing active-page reference" $ do
            let snap = buildSessionSnapshot
                         minimalGlobals { sgActivePage = page2 } [minimalPage page1]
                meta = snapshotSaveMetadata (SaveRequestMeta "s" "t") snap
                bytes = encodeSessionSnapshot meta snap
            decodeSessionEnvelope bytes `shouldSatisfy` isLeft

        it "one malformed component prevents ANY partial snapshot result \
           \(all-or-nothing)" $ do
            let good = encodeComponentSpecs richSnapshot
                tampered = [ if cid ≡ buildingsComponentId
                               then (cid, ver, req, BS.pack [9,9,9])
                               else s
                           | s@(cid, ver, req, _) ← good ]
                specs = (metadataComponentId, metadataComponentVersion, True
                        , S.encode richMeta) : tampered
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion specs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            case decodeSessionEnvelope bytes of
                Left msg → msg `shouldSatisfy` T.isInfixOf "buildings"
                Right _  → expectationFailure
                    "expected a malformed component to fail the whole decode"

    describe "frozen tracked fixture" $
        it "decodes a frozen, tracked multi-component byte fixture -- not \
           \merely this test's own encoder output -- proving the component \
           \envelope round-trips from real stored bytes" $ do
            let bytes = hexDecode trackedComponentFixtureHex
            decodeSaveEnvelopeMetadata bytes `shouldBe` Right richMeta
            case decodeSessionEnvelope bytes of
                Left err → expectationFailure (T.unpack err)
                Right (meta, snap) → do
                    meta `shouldBe` richMeta
                    snap `shouldBe` richSnapshot

    -- | B2's acceptance criteria: "preserve a fixture for the
    --   transitional payload only if it is deliberately supported by an
    --   explicit migration; otherwise document and test its intentional
    --   incompatibility." B2 does not add a migration (out of scope,
    --   also see the module's B1->B2 note above @knownComponentIds@ in
    --   "World.Save.Envelope") -- a real save written by B1-era
    --   'master' (a single required @"session"@ component, no gameplay
    --   components at all) is INTENTIONALLY no longer loadable once B2
    --   lands. Both entry points reject it identically, with the SAME
    --   structural reason (an unknown REQUIRED component): full-session
    --   decode ('decodeSessionEnvelope') AND metadata-only inspection
    --   ('decodeSaveEnvelopeMetadata', what 'World.Save.Serialize.listSaves'
    --   calls) both run the envelope's structural validation first, so
    --   a B1-era save also can't be listed under B2 -- there is no
    --   partial "still shows up in the save browser" case to preserve.
    describe "B1 -> B2 intentional incompatibility (no migration, by design)" $
        it "rejects a real B1-shaped envelope (metadata + a required, \
           \now-unknown 'session' component) as an unknown required \
           \component, both for full decode and metadata-only listing" $ do
            let b1Specs =
                    [ (metadataComponentId, metadataComponentVersion, True
                      , S.encode richMeta)
                    , (ComponentId "session", 90, True, BS.pack [1,2,3]) ]
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion b1Specs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            case decodeSaveEnvelopeMetadata bytes of
                Right _  → expectationFailure
                    "a B1-era save must not be listable under B2 either"
                Left msg → msg `shouldSatisfy` T.isInfixOf "session"
            case decodeSessionEnvelope bytes of
                Right _   → expectationFailure
                    "a B1-era save must not silently decode under B2"
                Left msg  → msg `shouldSatisfy` T.isInfixOf "session"

-- Helpers -----------------------------------------------------------

isLeftC ∷ Either ComponentError a → Bool
isLeftC (Left _) = True
isLeftC _        = False

elemIndex' ∷ Eq a ⇒ a → [a] → Maybe Int
elemIndex' x = go 0
  where go _ [] = Nothing
        go i (y:ys) | x ≡ y = Just i
                    | otherwise = go (i+1) ys

-- | A dummy registered component for cycle testing (its codec bodies are
--   never exercised — dependencyOrder only reads id + deps).
stubComponent ∷ ComponentId → [ComponentId] → RegisteredComponent
stubComponent cid deps = RegisteredComponent
    { rcId = cid, rcVersion = 1, rcInputVers = [1], rcRequired = True
    , rcDeps = deps, rcEncode = const BS.empty, rcCheck = \_ _ → [] }

-- | The component ids actually present in an encoded envelope's
--   manifest — a genuine structural read, so a stray @"session"@
--   component (or a missing gameplay one) would show up.
decodeEnvelopeIds ∷ BS.ByteString → Either Text [ComponentId]
decodeEnvelopeIds bytes =
    let known = HS.insert metadataComponentId componentKnownIds
    in case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
                known known bytes of
        Left err → Left (T.pack (show err))
        Right de → Right (map cdId (emComponents (deManifest de)))

trackedComponentFixtureHex ∷ String
trackedComponentFixtureHex =
    "53595241000000010000000000000240000000000000000c0000000000\
    \0000096275696c64696e67730000000101000000000000000000000000\
    \0000009f58f31c0481a2e438000000000000000c636f72652d73657373\
    \696f6e0000000101000000000000009f000000000000005574d3010096\
    \cbbe2b000000000000000b63726166742d62696c6c7300000001010000\
    \0000000000f4000000000000003abeec8f6ff4c58c2600000000000000\
    \096c75612d73746174650000000101000000000000012e000000000000\
    \002110322bc35ed5a50a00000000000000086d65746164617461000000\
    \0101000000000000014f0000000000000051cda064806651992f000000\
    \000000000b706f7765722d6e6f646573000000010100000000000001a0\
    \000000000000003abeec8f6ff4c58c26000000000000000f7465787475\
    \72652d70616c65747465000000010100000000000001da000000000000\
    \001088201fb960ff64650000000000000008756e69742d73696d000000\
    \010100000000000001ea000000000000007b81797b8874157310000000\
    \0000000005756e69747300000001010000000000000265000000000000\
    \010188a3dd8704d05847000000000000000e776f726c642d6163746976\
    \6974790000000101000000000000036600000000000000c2251087e707\
    \08d624000000000000000b776f726c642d656469747300000001010000\
    \00000000042800000000000000321ed7627acac8906400000000000000\
    \0b776f726c642d70616765730000000101000000000000045a00000000\
    \000004f8b7df1cb66e09260fbae4a5b30bef8a2c000000000000000200\
    \0000000000000570616765310000000000000001000000010000000000\
    \00000d746573745f6275696c64696e6700000000000000000000000000\
    \0000000000000000000000000000000000000000000000000000010000\
    \000000000001000000000000000042c800000000000000000000000000\
    \0000000000000000050000000000000005706167653200000000000000\
    \0000000001404500000000000000000000000000640000000a0000000a\
    \0000000000000005706167653100000000000000010000000000000005\
    \7061676531010000000000000005706167653140e00000410000004040\
    \0000030000000000000002000000000000000570616765310000000000\
    \0000000000000100000000000000057061676532000000000000000000\
    \0000010000000000000001000000000000000570617573650000000000\
    \000004626c6f620000000000000004736c6f74000000000001e2400000\
    \000000000080000000000000000a000000000000000274730100000000\
    \0000000a5269636820576f726c640100000000000000076120676c6f73\
    \7300000000000000020000000000000005706167653100000000000000\
    \0000000001000000000000000570616765320000000000000000000000\
    \0100000000000000000000000000000000000000000000000200000000\
    \0000000570616765310000000000000001000000010000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000005706167653200000000000000000000000000\
    \0000020000000000000005706167653100000000000000010000000100\
    \00000000000009746573745f756e69743f800000000000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \0000000469646c6500000000000000087374616e64696e670000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \000000000000000040a000000000000000000000000000050000000000\
    \0000057061676532000000000000000000000001000000000000000200\
    \0000000000000570616765310000000000000000000000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000570616765320000000000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000000200000000000000057061\
    \6765310000000000000000000000000000000570616765320000000000\
    \0000000000000000000002000000000000000570616765310000000000\
    \01e2400000000000000080000000000000000a00000000000000000000\
    \00000000001e000000000000000c000000000000001800000000000000\
    \3c3ecccccd3f000000000000000000001c000000000000000000000000\
    \0000000000000080000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000000100000000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \00000000000000000000000000000000000000000000000000323f8000\
    \003e99999a3f3333333fc000003f8000003f0000003f83333300000000\
    \0000000000000000000000200000000000000000000000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000003f800000000000003f8000\
    \003f3333333fa000000000000000000006000000000000001600000000\
    \0000000c3f8000003f8000003f80000000000000000000010000000000\
    \0000020000000000000001000000000000000300000000000000010000\
    \0000000000030000000000000001000000000000000300000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \00000000000000000000000c0000000000000000000000000000000100\
    \0000000000000100000000000000010001000000000000000a52696368\
    \20576f726c640100000000000000076120676c6f737300000000000000\
    \057061676532000000000000002a000000000000008000000000000000\
    \0a0000000000000000000000000000001e000000000000000c00000000\
    \00000018000000000000003c3ecccccd3f000000000000000000001c00\
    \0000000000000000000000000000000000008000000000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \0100000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \00000000000000323f8000003e99999a3f3333333fc000003f8000003f\
    \0000003f83333300000000000000000000000000000020000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000000000000000000000000000\
    \3f800000000000003f8000003f3333333fa00000000000000000000600\
    \00000000000016000000000000000c3f8000003f8000003f8000000000\
    \0000000000010000000000000002000000000000000100000000000000\
    \0300000000000000010000000000000003000000000000000100000000\
    \0000000300000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000c000000000000\
    \00000000000000000001000000000000000100000000000000010000"
