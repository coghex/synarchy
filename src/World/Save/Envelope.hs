{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Ties the generic tagged-envelope codec ("World.Save.Envelope.Codec")
--   to this codebase's two concrete components (issue #759,
--   save-overhaul B1):
--
--   - @"metadata"@ — a small, required component carrying exactly
--     'SaveMetadata' (name/seed/size/plates/timestamp/worldName/
--     worldGloss), so 'World.Save.Serialize.listSaves' can discover and
--     display every save WITHOUT decoding the (much larger) gameplay
--     payload (requirement 4).
--   - @"session"@ — a required, TRANSITIONAL component carrying the
--     complete, unchanged 'SaveData' (requirement 13). B2 replaces this
--     one component with several independently-versioned Haskell
--     components; nothing here anticipates that split.
--
--   Both components are required by construction — there is no
--   coherent save missing either — so both this reader's own
--   requirement AND the writer's own 'cdRequired' declaration agree.
module World.Save.Envelope
    ( currentEnvelopeVersion
    , metadataComponentId
    , sessionComponentId
    , metadataComponentVersion
    , sessionComponentVersion
    , encodeSaveEnvelope
    , encodeSaveData
    , decodeSaveEnvelope
    , decodeSaveEnvelopeMetadata
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Serialize as S
import qualified Data.Text as T
import World.Save.Envelope.Types
import World.Save.Envelope.Codec
import World.Save.Types (SaveData(..), SaveMetadata, currentSaveVersion)

-- | The v_new envelope-framing generation (requirement 3): bumped only
--   when the FRAMING contract itself changes incompatibly — never
--   merely because a carried component's own schema version changes.
currentEnvelopeVersion ∷ Word32
currentEnvelopeVersion = 1

metadataComponentId ∷ ComponentId
metadataComponentId = ComponentId "metadata"

sessionComponentId ∷ ComponentId
sessionComponentId = ComponentId "session"

-- | The "metadata" component's own schema version — a small,
--   independent counter (requirement 3: envelope/component versioning
--   stay separate). Bump this, not 'currentEnvelopeVersion', if
--   'SaveMetadata''s shape ever changes.
metadataComponentVersion ∷ Word32
metadataComponentVersion = 1

-- | The "session" component's schema version is the SAME counter
--   'SaveData'/'WorldPageSave' have always used — this transitional
--   component carries that positional format byte-for-byte (requirement
--   13), so its version identity doesn't change just because it now
--   rides inside an envelope instead of being the whole file.
sessionComponentVersion ∷ Word32
sessionComponentVersion = fromIntegral currentSaveVersion

-- | Every component id this codebase knows how to interpret.
knownComponentIds ∷ HS.HashSet ComponentId
knownComponentIds = HS.fromList [metadataComponentId, sessionComponentId]

-- | Both components are a hard requirement of THIS reader regardless
--   of what a writer's own 'cdRequired' flag says (requirement 6) —
--   there is no such thing as a coherent save missing either.
readerRequiredComponentIds ∷ HS.HashSet ComponentId
readerRequiredComponentIds = knownComponentIds

renderEnvelopeError ∷ EnvelopeError → Text
renderEnvelopeError = T.pack . show

-- | Encode a 'SaveData' into a validated, checksummed envelope. Pure —
--   'Left' means @limits@ were exceeded (an oversized manifest/
--   component/total), never a thrown exception.
encodeSaveEnvelope ∷ EnvelopeLimits → SaveData → Either Text BS.ByteString
encodeSaveEnvelope limits sd =
    let metaBytes    = S.encode (sdMetadata sd)
        sessionBytes = S.encode sd
        specs = [ (metadataComponentId, metadataComponentVersion, True, metaBytes)
                , (sessionComponentId, sessionComponentVersion, True, sessionBytes)
                ]
    in either (Left . renderEnvelopeError) Right
              (encodeEnvelope limits currentEnvelopeVersion specs)

-- | Total wrapper matching the pre-#759 'encodeSaveData' signature:
--   'World.Thread.Command.Save.WriteWorld' forces this via 'evaluate'
--   BEFORE releasing the #758 capture-lock barrier, relying on forcing
--   a strict 'BS.ByteString' to WHNF fully realizing every byte —
--   which in turn forces cereal's traversal of the ENTIRE 'SaveData'.
--   Returning an 'Either' directly from THIS function would stop that
--   forcing at the 'Left'/'Right' constructor without ever touching
--   the ByteString sitting inside it; the 'case' below still fully
--   forces 'encodeSaveEnvelope''s result to pick a branch, so the
--   invariant survives unchanged. A 'Left' here means our own generous
--   'defaultEnvelopeLimits' were exceeded — effectively unreachable for
--   any real save — so 'error' (caught as a capture failure exactly
--   like any other unexpected exception at that call site) is the
--   right response, not a silent 'Either' plumbed through call sites
--   that have never needed to handle one.
encodeSaveData ∷ SaveData → BS.ByteString
encodeSaveData sd = case encodeSaveEnvelope defaultEnvelopeLimits sd of
    Right bytes → bytes
    Left err    → error ("encodeSaveData: " <> T.unpack err)

-- | Decode + validate the envelope structure, then decode ONLY the
--   "metadata" component — the "session" component's (much larger)
--   payload is never even cereal-decoded (requirement 4: "Metadata
--   inspection without gameplay decoding"). Its own schema version is
--   checked exactly like the "session" component's (below) — a
--   structurally valid envelope declaring an unsupported metadata
--   schema version is rejected before 'S.decode' ever runs, not
--   trusted merely because its bytes happen to still parse as the
--   CURRENT 'SaveMetadata' shape.
decodeSaveEnvelopeMetadata ∷ BS.ByteString → Either Text SaveMetadata
decodeSaveEnvelopeMetadata bytes = do
    decoded ← decodeValidatedEnvelope bytes
    decodeComponent "metadata" metadataComponentId metadataComponentVersion decoded

-- | Decode + validate the envelope structure, then decode the
--   "session" component into the (unchanged) 'SaveData' shape existing
--   callers already expect. Also validates "metadata"'s own schema
--   version (though its CONTENT is never needed here) — a save
--   incompatible enough to fail 'decodeSaveEnvelopeMetadata' must not
--   silently succeed loading merely because loading never touches that
--   component's payload.
decodeSaveEnvelope ∷ BS.ByteString → Either Text SaveData
decodeSaveEnvelope bytes = do
    decoded ← decodeValidatedEnvelope bytes
    checkComponentVersion "metadata" metadataComponentId
                          metadataComponentVersion decoded
    decodeComponent "session" sessionComponentId sessionComponentVersion decoded

decodeValidatedEnvelope ∷ BS.ByteString → Either Text DecodedEnvelope
decodeValidatedEnvelope =
    either (Left . renderEnvelopeError) Right
        . decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
                          knownComponentIds readerRequiredComponentIds

-- | Look up one component's descriptor in an already
--   structurally-validated envelope and reject a schema-version
--   mismatch (this codebase's per-component compatibility contract,
--   requirement 3) — without needing that component's payload at all.
--   The "descriptor missing" case is unreachable in practice: both
--   component ids are in 'readerRequiredComponentIds', so
--   'decodeValidatedEnvelope' already refused any envelope missing it.
checkComponentVersion ∷ Text → ComponentId → Word32 → DecodedEnvelope
                     → Either Text ()
checkComponentVersion label cid expectedVersion decoded = do
    desc ← maybe (Left (label <> " component descriptor missing \
                              \(unreachable — already required)")) Right
                 (findDescriptor cid (deManifest decoded))
    when (cdVersion desc ≢ expectedVersion) $
        Left ("Save format incompatible: expected " <> label
              <> " component v" <> T.pack (show expectedVersion)
              <> ", got v" <> T.pack (show (cdVersion desc)))

-- | 'checkComponentVersion' plus the payload lookup + cereal decode.
--   The "payload missing" case is unreachable for the same reason as
--   above.
decodeComponent ∷ S.Serialize a
                ⇒ Text → ComponentId → Word32 → DecodedEnvelope
                → Either Text a
decodeComponent label cid expectedVersion decoded = do
    checkComponentVersion label cid expectedVersion decoded
    payload ← maybe (Left (label <> " component payload missing \
                                 \(unreachable — already required)")) Right
                    (HM.lookup cid (dePayloads decoded))
    either (Left . (("Failed to decode " <> label <> " component: ") <>)
                 . T.pack)
           Right (S.decode payload)

findDescriptor ∷ ComponentId → EnvelopeManifest → Maybe ComponentDescriptor
findDescriptor cid manifest =
    listToMaybe [ d | d ← emComponents manifest, cdId d ≡ cid ]
