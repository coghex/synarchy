{-# LANGUAGE UnicodeSyntax, OverloadedStrings, ScopedTypeVariables #-}
-- | The generic "save envelope" codec gate (issue #759, save-overhaul
--   B1): the tagged, checksummed component container that replaced the
--   flat positional save format. No engine, no IO — every test here is
--   pure and drives "World.Save.Envelope.Codec" directly with small
--   SYNTHETIC component specs, exercising every structural / limit /
--   corruption rejection path, canonical ordering, and round-tripping,
--   none of which needs a real gameplay payload.
--
--   The PRODUCTION wiring (the real multi-component save envelope,
--   #760's component split) is gated separately by
--   'Test.Headless.World.Save.Components'.
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

import World.Save.Envelope.Types
import World.Save.Envelope.Codec

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

isManifestTooLarge ∷ Either EnvelopeError a → Bool
isManifestTooLarge (Left (ManifestTooLarge _)) = True
isManifestTooLarge _                           = False

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
