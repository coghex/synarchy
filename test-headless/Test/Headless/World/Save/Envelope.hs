{-# LANGUAGE UnicodeSyntax, OverloadedStrings, ScopedTypeVariables #-}
-- | The "save envelope" gate (issue #759, save-overhaul B1): the
--   tagged, checksummed component container that replaced the flat
--   positional save format. No engine, no IO — every test here is pure,
--   the same pattern 'Test.Headless.Save.Snapshot' uses for the layer
--   below this one.
--
--   Two layers are exercised:
--
--   - "World.Save.Envelope.Codec" directly, with small SYNTHETIC
--     component specs — every structural/limit/corruption rejection
--     path, canonical ordering, and round-tripping, none of which needs
--     a real 'SaveData'.
--   - "World.Save.Envelope" (the production wiring), with a real,
--     minimal 'SaveData'/'SaveMetadata' pair — metadata-only
--     inspection, the transitional session component, component-version
--     rejection, and the pre-#759 clean break.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "save envelope"'@.
module Test.Headless.World.Save.Envelope (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Serialize as S
import qualified Data.Text as T
import Numeric (readHex)

import World.Save.Envelope.Types
import World.Save.Envelope.Codec
import World.Save.Envelope
    ( currentEnvelopeVersion, metadataComponentId, sessionComponentId
    , metadataComponentVersion, sessionComponentVersion
    , encodeSaveEnvelope, decodeSaveEnvelope, decodeSaveEnvelopeMetadata
    )
import World.Save.Types
    ( SaveData(..), SaveMetadata(..), WorldPageSave(..)
    , BuildingSnapshot(..), UnitSnapshot(..) )
import World.Generate.Types (defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Tool.Types (ToolMode(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Structure.Palette (emptyTexPalette)
import Item.Ground (emptyGroundItems)
import World.Spoil.Types (emptySpoilPiles)
import World.Flora.Harvest (emptyFloraHarvests)
import World.Flora.CropPlot (emptyCropPlots)
import World.Edit.Types (emptyWorldEdits)
import Craft.Bills (emptyCraftBills)
import Power.Types (emptyPowerNodes)

-- ---------------------------------------------------------------------
-- Test helpers
-- ---------------------------------------------------------------------

-- | Encode, asserting the encode itself succeeds (it always should for
--   these tests' well-formed specs) so a setup bug fails loudly and
--   distinctly from the actual assertion under test.
unsafeEncode ∷ EnvelopeLimits → Word32 → [ComponentSpec] → BS.ByteString
unsafeEncode limits ver specs = case encodeEnvelope limits ver specs of
    Right bytes → bytes
    Left err    → error ("test setup: encodeEnvelope failed: " <> show err)

-- | Flip one byte (XOR 0xFF) at an index — the only way to corrupt a
--   checksum-covered region without disturbing the file's length.
flipByteAt ∷ Int → BS.ByteString → BS.ByteString
flipByteAt idx bs =
    BS.take idx bs
        <> BS.singleton (BS.index bs idx `xor` 0xFF)
        <> BS.drop (idx + 1) bs

hexDecode ∷ String → BS.ByteString
hexDecode = BS.pack . go
  where
    go (a:b:rest) = case readHex [a,b] of
        ((v,_):_) → v : go rest
        []        → error ("hexDecode: not a hex byte: " <> [a,b])
    go _          = []

isManifestTooLarge ∷ Either EnvelopeError a → Bool
isManifestTooLarge (Left (ManifestTooLarge _)) = True
isManifestTooLarge _                           = False

-- ---------------------------------------------------------------------
-- A minimal, otherwise-valid SaveData -- mirrors
-- Test.Headless.Save.Snapshot's minimalPage/minimalGlobals pattern, one
-- level up (a full legacy SaveData rather than a SessionSnapshot),
-- since the "session" component carries exactly this shape unchanged.
-- ---------------------------------------------------------------------

minimalSaveMetadata ∷ SaveMetadata
minimalSaveMetadata = SaveMetadata
    { smName       = "envelope_test_save"
    , smSeed       = 42
    , smWorldSize  = 64
    , smPlateCount = 3
    , smTimestamp  = "2026-07-16T00:00:00.000000Z"
    , smWorldName  = Just "Test World"
    , smWorldGloss = Just "a fixture world"
    }

minimalWorldPageSave ∷ WorldPageSave
minimalWorldPageSave = WorldPageSave
    { wpsPageId       = WorldPageId "main_world"
    , wpsGenParams    = defaultWorldGenParams
    , wpsCameraX      = 0
    , wpsCameraY      = 0
    , wpsCameraZoom   = 1
    , wpsCameraFacing = FaceSouth
    , wpsTimeHour     = 12
    , wpsTimeMinute   = 0
    , wpsDateYear     = 1
    , wpsDateMonth    = 1
    , wpsDateDay      = 1
    , wpsTimeScale    = 1
    , wpsMapMode      = ZMDefault
    , wpsToolMode     = DefaultTool
    , wpsEdits        = emptyWorldEdits
    , wpsMineDesignations      = HM.empty
    , wpsConstructDesignations = HM.empty
    , wpsGroundItems  = emptyGroundItems
    , wpsSpoilPiles   = emptySpoilPiles
    , wpsBuildings    = BuildingSnapshot { bsnInstances = HM.empty, bsnNextId = 1 }
    , wpsUnits        = UnitSnapshot { usnInstances = HM.empty, usnNextId = 1 }
    , wpsUnitSimStates = HM.empty
    , wpsFloraHarvests = emptyFloraHarvests
    , wpsChopDesignations = HM.empty
    , wpsCraftBills   = emptyCraftBills
    , wpsPowerNodes   = emptyPowerNodes
    , wpsTillDesignations = HM.empty
    , wpsCropPlots    = emptyCropPlots
    , wpsPlantDesignations = HM.empty
    , wpsIdentity     = Nothing
    }

minimalSaveData ∷ SaveData
minimalSaveData = SaveData
    { sdMetadata           = minimalSaveMetadata
    , sdGameTime           = 0
    , sdEnginePaused       = True
    , sdLuaModules         = HM.empty
    , sdTexPalette         = emptyTexPalette
    , sdNextItemInstanceId = 1
    , sdActivePage         = WorldPageId "main_world"
    , sdVisiblePages       = [WorldPageId "main_world"]
    , sdWorlds             = [minimalWorldPageSave]
    }

spec ∷ Spec
spec = do
    describe "round trips" $ do
        it "round-trips an empty envelope" $
            case decodeEnvelope defaultEnvelopeLimits 1 HS.empty HS.empty
                    (unsafeEncode defaultEnvelopeLimits 1 []) of
                Right decoded → dePayloads decoded `shouldBe` HM.empty
                Left err      → expectationFailure (show err)

        it "round-trips a multi-component envelope, preserving every \
           \payload's exact bytes" $ do
            let specs = [ (ComponentId "alpha", 3, True,  BS.pack [1,2,3])
                        , (ComponentId "beta",  7, False, BS.pack [4,5,6,7])
                        , (ComponentId "gamma", 1, True,  BS.empty)
                        ]
                known = HS.fromList (map (\(c,_,_,_) → c) specs)
                bytes = unsafeEncode defaultEnvelopeLimits 1 specs
            case decodeEnvelope defaultEnvelopeLimits 1 known known bytes of
                Right decoded → do
                    HM.lookup (ComponentId "alpha") (dePayloads decoded)
                        `shouldBe` Just (BS.pack [1,2,3])
                    HM.lookup (ComponentId "beta") (dePayloads decoded)
                        `shouldBe` Just (BS.pack [4,5,6,7])
                    HM.lookup (ComponentId "gamma") (dePayloads decoded)
                        `shouldBe` Just BS.empty
                Left err → expectationFailure (show err)

    describe "canonical ordering / determinism" $ do
        it "lays out components in ascending component-id order \
           \regardless of input order" $ do
            let specs = [ (ComponentId "zzz", 1, True, BS.pack [1])
                        , (ComponentId "aaa", 1, True, BS.pack [2])
                        , (ComponentId "mmm", 1, True, BS.pack [3])
                        ]
                known = HS.fromList (map (\(c,_,_,_) → c) specs)
                bytes = unsafeEncode defaultEnvelopeLimits 1 specs
            case decodeEnvelope defaultEnvelopeLimits 1 known known bytes of
                Right decoded → map cdId (emComponents (deManifest decoded))
                    `shouldBe` [ComponentId "aaa", ComponentId "mmm", ComponentId "zzz"]
                Left err → expectationFailure (show err)

        it "encodes identical inputs identically regardless of input \
           \list order (no gameplay determinism implied or required)" $ do
            let specsA = [ (ComponentId "one", 1, True,  BS.pack [1,2])
                         , (ComponentId "two", 2, False, BS.pack [3,4,5])
                         ]
                specsB = reverse specsA
            unsafeEncode defaultEnvelopeLimits 1 specsA
                `shouldBe` unsafeEncode defaultEnvelopeLimits 1 specsB

    describe "required/optional component contract" $ do
        it "accepts a known required component alongside a known \
           \optional one" $ do
            let specs = [ (ComponentId "req", 1, True,  BS.pack [1,2,3])
                        , (ComponentId "opt", 1, False, BS.pack [4,5])
                        ]
                known = HS.fromList [ComponentId "req", ComponentId "opt"]
                bytes = unsafeEncode defaultEnvelopeLimits 1 specs
            case decodeEnvelope defaultEnvelopeLimits 1 known
                    (HS.singleton (ComponentId "req")) bytes of
                Right decoded → HM.keys (dePayloads decoded)
                    `shouldMatchList` [ComponentId "req", ComponentId "opt"]
                Left err → expectationFailure (show err)

        it "skips an unknown OPTIONAL component without losing framing \
           \alignment" $ do
            let specs = [ (ComponentId "req", 1, True,  BS.pack [1,2,3])
                        , (ComponentId "mystery", 1, False, BS.pack [9,9])
                        ]
                bytes = unsafeEncode defaultEnvelopeLimits 1 specs
                -- "mystery" is deliberately absent from the known set.
                known = HS.fromList [ComponentId "req"]
            case decodeEnvelope defaultEnvelopeLimits 1 known
                    (HS.singleton (ComponentId "req")) bytes of
                Right decoded → HM.lookup (ComponentId "req") (dePayloads decoded)
                    `shouldBe` Just (BS.pack [1,2,3])
                Left err → expectationFailure (show err)

        it "rejects an unknown REQUIRED component" $ do
            let specs = [ (ComponentId "req", 1, True, BS.pack [1,2,3])
                        , (ComponentId "mystery", 1, True, BS.pack [9,9])
                        ]
                bytes = unsafeEncode defaultEnvelopeLimits 1 specs
                known = HS.fromList [ComponentId "req"]
            decodeEnvelope defaultEnvelopeLimits 1 known
                    (HS.singleton (ComponentId "req")) bytes
                `shouldBe` Left (UnknownRequiredComponent (ComponentId "mystery"))

        it "rejects a save missing a component THIS READER requires, \
           \even though the writer never marked any component required \
           \at all -- a writer's 'optional' can't override a reader's \
           \own hard requirement" $ do
            let specs = [ (ComponentId "other", 1, False, BS.pack [1,2,3]) ]
                bytes = unsafeEncode defaultEnvelopeLimits 1 specs
                known = HS.fromList [ComponentId "other", ComponentId "req"]
            decodeEnvelope defaultEnvelopeLimits 1 known
                    (HS.singleton (ComponentId "req")) bytes
                `shouldBe` Left (MissingRequiredComponent (ComponentId "req"))

    describe "structural corruption" $ do
        it "rejects a header truncated below 16 bytes" $
            decodeEnvelope defaultEnvelopeLimits 1 HS.empty HS.empty
                    (BS.pack [1,2,3])
                `shouldBe` Left TruncatedHeader

        it "rejects bad magic" $ do
            let bytes = unsafeEncode defaultEnvelopeLimits 1
                    [(ComponentId "a", 1, True, BS.pack [1,2,3])]
                corrupted = BS.cons 0x00 (BS.drop 1 bytes)
            decodeEnvelope defaultEnvelopeLimits 1
                    (HS.fromList [ComponentId "a"]) HS.empty corrupted
                `shouldBe` Left BadMagic

        it "rejects an unsupported envelope version" $ do
            let bytes = unsafeEncode defaultEnvelopeLimits 99
                    [(ComponentId "a", 1, True, BS.pack [1,2,3])]
            decodeEnvelope defaultEnvelopeLimits 1
                    (HS.fromList [ComponentId "a"]) HS.empty bytes
                `shouldBe` Left (UnsupportedEnvelopeVersion 99)

        it "rejects a manifest truncated short of its declared length" $ do
            let bytes = unsafeEncode defaultEnvelopeLimits 1
                    [(ComponentId "a", 1, True, BS.pack [1..10])]
                shortened = BS.take 18 bytes
            decodeEnvelope defaultEnvelopeLimits 1
                    (HS.fromList [ComponentId "a"]) HS.empty shortened
                `shouldBe` Left ManifestTruncated

        it "rejects a corrupted manifest (checksum mismatch)" $ do
            let bytes = unsafeEncode defaultEnvelopeLimits 1
                    [(ComponentId "a", 1, True, BS.pack [1,2,3])]
                -- byte 16 is the first byte of the manifest, right after
                -- the fixed 16-byte header.
                flipped = flipByteAt 16 bytes
            decodeEnvelope defaultEnvelopeLimits 1
                    (HS.fromList [ComponentId "a"]) HS.empty flipped
                `shouldBe` Left ManifestChecksumMismatch

        it "rejects duplicate component identifiers" $ do
            let mk off len = ComponentDescriptor
                    { cdId = ComponentId "dup", cdVersion = 1, cdRequired = True
                    , cdOffset = off, cdLength = len
                    , cdChecksum = fnv1a64 (BS.replicate (fromIntegral len) 0)
                    }
                manifest = EnvelopeManifest [mk 0 4, mk 4 4]
                bytes = rawEnvelope 1 manifest (BS.replicate 8 0)
            decodeEnvelope defaultEnvelopeLimits 1
                    (HS.fromList [ComponentId "dup"]) HS.empty bytes
                `shouldBe` Left (DuplicateComponentId (ComponentId "dup"))

        it "rejects overlapping payload declarations" $ do
            let descA = ComponentDescriptor
                    { cdId = ComponentId "a", cdVersion = 1, cdRequired = True
                    , cdOffset = 0, cdLength = 10
                    , cdChecksum = fnv1a64 (BS.replicate 10 1) }
                descB = ComponentDescriptor
                    { cdId = ComponentId "b", cdVersion = 1, cdRequired = True
                    , cdOffset = 5, cdLength = 10
                    , cdChecksum = fnv1a64 (BS.replicate 10 2) }
                manifest = EnvelopeManifest [descA, descB]
                bytes = rawEnvelope 1 manifest (BS.replicate 15 1)
                known = HS.fromList [ComponentId "a", ComponentId "b"]
            decodeEnvelope defaultEnvelopeLimits 1 known HS.empty bytes
                `shouldBe` Left (OverlappingComponents (ComponentId "a") (ComponentId "b"))

        it "rejects a payload truncated short of its declared length" $ do
            let desc = ComponentDescriptor
                    { cdId = ComponentId "a", cdVersion = 1, cdRequired = True
                    , cdOffset = 0, cdLength = 100, cdChecksum = 0 }
                manifest = EnvelopeManifest [desc]
                bytes = rawEnvelope 1 manifest (BS.replicate 10 0)
            decodeEnvelope defaultEnvelopeLimits 1
                    (HS.fromList [ComponentId "a"]) HS.empty bytes
                `shouldBe` Left (TruncatedPayload (ComponentId "a"))

        it "safely rejects an offset+length pair that would overflow \
           \Word64 arithmetic, with no wraparound false-pass" $ do
            let desc = ComponentDescriptor
                    { cdId = ComponentId "a", cdVersion = 1, cdRequired = True
                    , cdOffset = maxBound - 5, cdLength = 100, cdChecksum = 0 }
                manifest = EnvelopeManifest [desc]
                bytes = rawEnvelope 1 manifest (BS.replicate 10 0)
            decodeEnvelope defaultEnvelopeLimits 1
                    (HS.fromList [ComponentId "a"]) HS.empty bytes
                `shouldBe` Left (TruncatedPayload (ComponentId "a"))

        it "rejects forbidden trailing bytes past the last declared \
           \payload" $ do
            let bytes = unsafeEncode defaultEnvelopeLimits 1
                    [(ComponentId "a", 1, True, BS.pack [1,2,3])]
                withGarbage = bytes <> BS.pack [9,9,9]
            decodeEnvelope defaultEnvelopeLimits 1
                    (HS.fromList [ComponentId "a"]) HS.empty withGarbage
                `shouldBe` Left (TrailingBytes 3)

        it "rejects a corrupted component payload (checksum mismatch)" $ do
            let bytes = unsafeEncode defaultEnvelopeLimits 1
                    [(ComponentId "a", 1, True, BS.pack [1,2,3,4,5])]
                flipped = flipByteAt (BS.length bytes - 1) bytes
            decodeEnvelope defaultEnvelopeLimits 1
                    (HS.fromList [ComponentId "a"]) HS.empty flipped
                `shouldBe` Left (ComponentChecksumMismatch (ComponentId "a"))

    describe "documented allocation limits" $ do
        it "rejects a manifest exceeding elMaxManifestBytes" $ do
            let bytes = unsafeEncode defaultEnvelopeLimits 1
                    [(ComponentId "a", 1, True, BS.pack [1,2,3,4,5])]
                tight = defaultEnvelopeLimits { elMaxManifestBytes = 2 }
            decodeEnvelope tight 1 (HS.fromList [ComponentId "a"]) HS.empty bytes
                `shouldSatisfy` isManifestTooLarge

        it "rejects more components than elMaxComponents" $ do
            let ids = [ ComponentId (T.pack ("c" <> show i)) | i ← [1..5 ∷ Int] ]
                specs = [ (cid, 1, True, BS.pack [1]) | cid ← ids ]
                bytes = unsafeEncode defaultEnvelopeLimits 1 specs
                tight = defaultEnvelopeLimits { elMaxComponents = 2 }
            decodeEnvelope tight 1 (HS.fromList ids) HS.empty bytes
                `shouldBe` Left (TooManyComponents 5)

        it "rejects a component id longer than elMaxComponentIdLength" $ do
            let longId = ComponentId (T.replicate 10 "x")
                bytes = unsafeEncode defaultEnvelopeLimits 1
                    [(longId, 1, True, BS.pack [1,2,3])]
                tight = defaultEnvelopeLimits { elMaxComponentIdLength = 5 }
            decodeEnvelope tight 1 (HS.fromList [longId]) HS.empty bytes
                `shouldBe` Left (ComponentIdTooLong longId)

        it "rejects a single payload exceeding elMaxPayloadBytes" $ do
            let bytes = unsafeEncode defaultEnvelopeLimits 1
                    [(ComponentId "a", 1, True, BS.replicate 20 0)]
                tight = defaultEnvelopeLimits { elMaxPayloadBytes = 10 }
            decodeEnvelope tight 1 (HS.fromList [ComponentId "a"]) HS.empty bytes
                `shouldBe` Left (PayloadTooLarge (ComponentId "a") 20)

        it "rejects a total declared payload size exceeding \
           \elMaxTotalPayloadBytes" $ do
            let specs = [ (ComponentId "a", 1, True, BS.replicate 10 0)
                        , (ComponentId "b", 1, True, BS.replicate 10 0)
                        ]
                bytes = unsafeEncode defaultEnvelopeLimits 1 specs
                tight = defaultEnvelopeLimits { elMaxTotalPayloadBytes = 15 }
                known = HS.fromList [ComponentId "a", ComponentId "b"]
            decodeEnvelope tight 1 known HS.empty bytes
                `shouldBe` Left (TotalPayloadTooLarge 20)

    describe "production wiring (World.Save.Envelope)" $ do
        it "round-trips a real SaveData through \
           \encodeSaveEnvelope/decodeSaveEnvelope" $
            case encodeSaveEnvelope defaultEnvelopeLimits minimalSaveData of
                Left err → expectationFailure (T.unpack err)
                Right bytes → case decodeSaveEnvelope bytes of
                    Left err → expectationFailure (T.unpack err)
                    Right sd → do
                        sdMetadata sd `shouldBe` sdMetadata minimalSaveData
                        sdGameTime sd `shouldBe` sdGameTime minimalSaveData
                        sdActivePage sd `shouldBe` sdActivePage minimalSaveData
                        sdVisiblePages sd `shouldBe` sdVisiblePages minimalSaveData
                        map wpsPageId (sdWorlds sd)
                            `shouldBe` map wpsPageId (sdWorlds minimalSaveData)

        it "inspects save metadata WITHOUT decoding the (much larger) \
           \gameplay component -- proven by giving the session component \
           \a structurally/checksum-valid but semantically undecodable \
           \payload: decodeSaveEnvelopeMetadata still succeeds, while \
           \decodeSaveEnvelope (which DOES interpret it) fails" $ do
            let metaBytes = S.encode minimalSaveMetadata
                specs = [ (metadataComponentId, metadataComponentVersion, True
                          , metaBytes)
                        , (sessionComponentId, sessionComponentVersion, True
                          , BS.pack [9,9,9])
                        ]
                bytes = unsafeEncode defaultEnvelopeLimits currentEnvelopeVersion
                            specs
            decodeSaveEnvelopeMetadata bytes `shouldBe` Right minimalSaveMetadata
            case decodeSaveEnvelope bytes of
                Left _  → pure ()
                Right _ → expectationFailure
                    "expected decodeSaveEnvelope to fail on an undecodable \
                    \session payload"

        it "rejects an unsupported session component schema version" $ do
            let metaBytes = S.encode minimalSaveMetadata
                specs = [ (metadataComponentId, metadataComponentVersion, True
                          , metaBytes)
                        , (sessionComponentId, 999999, True
                          , S.encode minimalSaveData)
                        ]
                bytes = unsafeEncode defaultEnvelopeLimits currentEnvelopeVersion
                            specs
            case decodeSaveEnvelope bytes of
                Left msg → msg `shouldSatisfy`
                    T.isInfixOf "expected session component"
                Right _  → expectationFailure
                    "expected a session-component version mismatch rejection"

        it "rejects a pre-#759 flat-format file with a clear \
           \incompatibility diagnostic (the v82-and-earlier clean \
           \break) -- no heuristic positional decoding is attempted" $ do
            -- Pre-#759 saves were [magic][8-byte cereal Int
            -- version][SaveData body], no manifest at all. cereal's
            -- 'Serialize Int' encodes as a big-endian Int64 (confirmed
            -- against the installed cereal-0.5.8.3 source), so the
            -- SECOND 4-byte field of any such file -- read as this
            -- envelope's Word32 version field -- is always the Int64's
            -- HIGH 4 bytes, zero for every real historical save version
            -- (all small positive numbers). That alone is enough to
            -- reject it.
            let legacyBytes = encodeW32 envelopeMagic
                             <> S.encode (91 ∷ Int)
                             <> BS.replicate 64 0
            case decodeSaveEnvelope legacyBytes of
                Left msg → msg `shouldSatisfy`
                    T.isInfixOf "UnsupportedEnvelopeVersion"
                Right _  → expectationFailure
                    "expected the legacy flat-format file to be rejected"

        it "decodes a frozen, tracked byte fixture -- not merely an \
           \encoder's output from within this same test -- proving \
           \metadata inspection and complete decode both work from \
           \real stored bytes" $ do
            let bytes = hexDecode trackedEnvelopeFixtureHex
            decodeSaveEnvelopeMetadata bytes
                `shouldBe` Right minimalSaveMetadata
            case decodeSaveEnvelope bytes of
                Left err → expectationFailure (T.unpack err)
                Right sd → do
                    sdMetadata sd `shouldBe` minimalSaveMetadata
                    sdActivePage sd `shouldBe` sdActivePage minimalSaveData
                    map wpsPageId (sdWorlds sd)
                        `shouldBe` map wpsPageId (sdWorlds minimalSaveData)

-- | Build raw envelope bytes directly from a manifest + payload-section
--   bytes, bypassing 'encodeEnvelope''s own validation entirely. The
--   only way to construct a deliberately INVALID envelope (duplicate
--   ids, overlapping ranges) for negative decode tests.
rawEnvelope ∷ Word32 → EnvelopeManifest → BS.ByteString → BS.ByteString
rawEnvelope ver manifest payloadSection =
    let manifestBytes = S.encode manifest
        header = encodeW32 envelopeMagic <> encodeW32 ver
               <> encodeW64 (fromIntegral (BS.length manifestBytes))
    in header <> manifestBytes <> encodeW64 (fnv1a64 manifestBytes)
           <> payloadSection

-- | Frozen bytes of a real envelope encoding 'minimalSaveMetadata' /
--   'minimalSaveData' -- captured once via 'encodeSaveEnvelope' and
--   hex-dumped, NOT produced by calling the encoder in the decode test
--   above. Regenerate by printing
--   @either (const "") (concatMap (\\b -> ...) . BS.unpack)
--     (encodeSaveEnvelope defaultEnvelopeLimits minimalSaveData)@
--   (or any hex dump of that 'Right' value) if 'minimalSaveData' or the
--   envelope format ever change.
trackedEnvelopeFixtureHex ∷ String
trackedEnvelopeFixtureHex =
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
