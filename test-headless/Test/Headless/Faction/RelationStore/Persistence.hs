{-# LANGUAGE Strict #-}
-- | The codec half of the "Faction relation store" gate (#2548): the
--   @"relation-causes"@ component through the REAL production envelope
--   ('World.Save.Serialize'-level 'encodeSessionSnapshot' /
--   'decodeSessionEnvelope'), never through a validator in isolation —
--   "absent" is decided at the manifest level by 'registerComponent', so
--   only real envelope bytes can show it. Pure: no engine, no IO.
--
--   Every malformed case rewrites ONE component of an envelope this
--   module really encoded, so a failure names exactly the component the
--   case damaged.
module Test.Headless.Faction.RelationStore.Persistence
    ( spec
    , withoutComponent
    , rewriteComponent
    ) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (permutations)
import qualified Data.Serialize as S
import qualified Data.Text as T
import Test.Headless.Faction.RelationStore.Fixture
import Test.Headless.World.Save.Components.Fixture
    (minimalGlobals, minimalPage, page1)
import Unit.Faction.RelationStore
import World.Save.Component
    (componentKnownIds, saveComponentRegistry)
import World.Save.Component.RelationCauses
import World.Save.Component.Types
import World.Save.Envelope
import World.Save.Envelope.Codec
    (DecodedEnvelope(..), decodeEnvelope, encodeEnvelope)
import World.Save.Envelope.Types
    ( ComponentDescriptor(..), ComponentId, EnvelopeManifest(..)
    , defaultEnvelopeLimits )
import World.Save.Snapshot
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotSaveMetadata)

-- | A valid one-page session carrying @store@.
snapshotWith ∷ RelationCauseStore → SessionSnapshot
snapshotWith store =
    case captureSessionSnapshot minimalGlobals { sgRelationCauses = store }
                                [minimalPage page1] of
        Right s   → s
        Left errs → error ("relation snapshot invalid: " <> show errs)

encodeStore ∷ RelationCauseStore → BS.ByteString
encodeStore store = encodeSessionSnapshot meta snap []
  where
    snap = snapshotWith store
    meta = snapshotSaveMetadata (SaveRequestMeta "relcause" "ts" False) snap

-- | The store a real envelope decodes to, or the load's failure text.
decodedStore ∷ BS.ByteString → Either Text RelationCauseStore
decodedStore bytes = case decodeSessionEnvelope HS.empty HS.empty bytes of
    Left err → Left err
    Right (_, snap, _, _) → Right (snapRelationCauses snap)

allIds ∷ HS.HashSet ComponentId
allIds = HS.insert metadataComponentId componentKnownIds

-- | Rewrite one component of an encoded envelope: @f version payload@
--   gives its replacement, or 'Nothing' to drop it from the manifest
--   entirely. Every other component is carried through untouched.
rewriteComponent
    ∷ ComponentId → (Word32 → BS.ByteString → Maybe (Word32, BS.ByteString))
    → BS.ByteString → BS.ByteString
rewriteComponent target f bytes =
    case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
             allIds HS.empty bytes of
        Left err → error ("rewriteComponent: decode: " <> show err)
        Right de →
            let specs = [ (cdId d, ver, cdRequired d, payload)
                        | d ← emComponents (deManifest de)
                        , Just raw ← [HM.lookup (cdId d) (dePayloads de)]
                        , Just (ver, payload) ←
                            [ if cdId d ≡ target
                                then f (cdVersion d) raw
                                else Just (cdVersion d, raw) ] ]
            in case encodeEnvelope defaultEnvelopeLimits
                        currentEnvelopeVersion specs of
                Left err  → error ("rewriteComponent: encode: " <> show err)
                Right out → out

withoutComponent ∷ ComponentId → BS.ByteString → BS.ByteString
withoutComponent cid = rewriteComponent cid (\_ _ → Nothing)

-- | Replace the payload (at the current version) with an encoded DTO.
withPayload ∷ RelationCausesDTO → BS.ByteString → BS.ByteString
withPayload dto = rewriteComponent relationCausesComponentId
    (\ver _ → Just (ver, S.encode dto))

spec ∷ Spec
spec = describe "the relation-causes component" $ do
    it "is registered at v1, optional, with no dependency" $
        [ (rcId c, rcVersion c, rcRequired c, rcDeps c)
        | c ← saveComponentRegistry, rcId c ≡ relationCausesComponentId ]
            `shouldBe` [(relationCausesComponentId, 1, False, [])]

    it "round-trips every owner, both tags and the relation value of \
       \several owners and pairs through the real envelope" $
        decodedStore (encodeStore sampleStore) `shouldBe` Right sampleStore

    it "a present EMPTY store round-trips to the empty store" $
        decodedStore (encodeStore emptyRelationCauseStore)
            `shouldBe` Right emptyRelationCauseStore

    it "is order-independent on the way OUT: every insertion order of \
       \the same causes encodes the same payload (all 120 permutations)" $ do
        let payload cs = ccEncode relationCausesCodec
                (snapshotWith (relationCauseStoreFromList cs))
            canonical = payload sampleCauses
        forM_ (permutations sampleCauses) $ \cs →
            payload cs `shouldBe` canonical

    it "is order-independent on the way IN: every ordering of the \
       \payload's own list decodes to the same store (all 120)" $ do
        let CausesList dtos = payloadOf sampleStore
            base = encodeStore sampleStore
        length dtos `shouldBe` 5
        forM_ (permutations dtos) $ \perm →
            decodedStore (withPayload (RelationCausesDTO perm) base)
                `shouldBe` Right sampleStore

    it "writes the component into a real save, and an ABSENT payload \
       \restores the empty store — the honest default of every save that \
       \predates it — leaving the rest of the session exactly as decoded" $ do
        let bytes = encodeStore sampleStore
            stripped = withoutComponent relationCausesComponentId bytes
        decodedStore stripped `shouldBe` Right emptyRelationCauseStore
        -- Non-vacuity: the unstripped bytes really carry the causes, and
        -- the strip removed nothing else.
        decodedStore bytes `shouldBe` Right sampleStore
        case ( decodeSessionEnvelope HS.empty HS.empty stripped
             , decodeSessionEnvelope HS.empty HS.empty
                 (encodeStore emptyRelationCauseStore) ) of
            (Right (_, a, _, _), Right (_, b, _, _)) → a `shouldBe` b
            _ → expectationFailure "stripped envelope did not decode"

    describe "a PRESENT payload that is broken still fails the load like \
             \a required component's" $ do
        let bytes = encodeStore sampleStore
            failsNaming needle r = case r of
                Left msg → do
                    msg `shouldSatisfy` T.isInfixOf "relation-causes"
                    msg `shouldSatisfy` T.isInfixOf needle
                Right s → expectationFailure
                    ("loaded anyway with " <> show (relationCauseStoreOverlay s))

        it "garbage bytes" $
            failsNaming ""
                (decodedStore (rewriteComponent relationCausesComponentId
                    (\v _ → Just (v, "not-a-relation-causes-payload")) bytes))

        it "a truncated payload" $
            failsNaming ""
                (decodedStore (rewriteComponent relationCausesComponentId
                    (\v raw → Just (v, BS.take (BS.length raw - 3) raw))
                    bytes))

        it "an unsupported NEWER version, and the never-written version 0" $
            forM_ [2, 999, 0] $ \ver →
                failsNaming ""
                    (decodedStore (rewriteComponent relationCausesComponentId
                        (\_ raw → Just (ver, raw)) bytes))

        it "a malformed SOURCE or TARGET tag — rejected, never silently \
           \dropped as a cause that vanishes" $
            forM_ [ "", " ", "two words", "tab\there" ] $ \bad → do
                failsNaming "malformed source"
                    (decodedStore (withPayload (RelationCausesDTO
                        [RelationCauseDTO "o" bad "ok" RvdHostile]) bytes))
                failsNaming "malformed target"
                    (decodedStore (withPayload (RelationCausesDTO
                        [RelationCauseDTO "o" "ok" bad RvdHostile]) bytes))

        it "the same (owner, source, target) twice — the in-memory merge \
           \would otherwise invent a value nobody wrote" $
            failsNaming "appears 2 times"
                (decodedStore (withPayload (RelationCausesDTO
                    [ RelationCauseDTO "o" "a" "b" RvdAlly
                    , RelationCauseDTO "o" "a" "b" RvdHostile ]) bytes))

        it "an unknown relation byte (the DTO is a closed enum)" $
            failsNaming ""
                (decodedStore (rewriteComponent relationCausesComponentId
                    (\v _ → Just (v, wireOne "o" "a" "b" 3)) bytes))

    describe "the wire format is a frozen DTO, not FactionRelation" $ do
        it "pins the byte layout: owner, source, target as length-prefixed \
           \text, then one relation byte (ally 0, neutral 1, hostile 2)" $
            forM_ [(0, RvdAlly), (1, RvdNeutral), (2, RvdHostile)] $
              \(b, v) →
                S.decode (wireOne "o" "a" "b" b)
                    `shouldBe` Right (RelationCausesDTO
                        [RelationCauseDTO "o" "a" "b" v])

        it "encodes each relation as that single byte" $ do
            S.encode RvdAlly    `shouldBe` BS.pack [0]
            S.encode RvdNeutral `shouldBe` BS.pack [1]
            S.encode RvdHostile `shouldBe` BS.pack [2]
  where
    payloadOf store = CausesList (rcsdCauses (decodeDTO store))
    decodeDTO store =
        case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
                 allIds HS.empty (encodeStore store) of
            Left err → error (show err)
            Right de → case HM.lookup relationCausesComponentId
                              (dePayloads de) of
                Nothing → error "component absent"
                Just raw → either error id (S.decode raw)

-- | A list wrapper only so the pattern binding above reads as one.
newtype CausesList = CausesList [RelationCauseDTO]

-- | One cause as the exact bytes a v1 payload carries: a one-element
--   list (u64 count), three length-prefixed texts, one relation byte.
wireOne ∷ Text → Text → Text → Word8 → BS.ByteString
wireOne o s t b = BS.concat
    [ be64 1, text o, text s, text t, BS.pack [b] ]
  where
    be64 n = BS.pack [ fromIntegral (n `div` (256 ^ i) `mod` (256 ∷ Int))
                     | i ← [7, 6 .. 0 ∷ Int] ]
    text x = let u = BC.pack (T.unpack x) in be64 (BS.length u) <> u
