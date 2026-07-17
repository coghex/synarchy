{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Ties the generic tagged-envelope codec ("World.Save.Envelope.Codec")
--   to this codebase's concrete save components (issue #760,
--   save-overhaul B2 — replacing #759 B1's single transitional
--   @"session"@ component):
--
--   - @"metadata"@ — a small, required component carrying exactly
--     'SaveMetadata' (name/seed/size/plates/timestamp/worldName/
--     worldGloss), so 'World.Save.Serialize.listSaves' can display every
--     save WITHOUT decoding the gameplay payload (requirement 12).
--   - the full Haskell-owned gameplay component set
--     ("World.Save.Component.saveComponentRegistry") — @"core-session"@,
--     @"texture-palette"@, @"lua-state"@, @"world-pages"@, @"world-edits"@,
--     @"world-activity"@, @"buildings"@, @"units"@, @"unit-sim"@,
--     @"craft-bills"@, @"power-nodes"@ — each independently versioned,
--     converted to/from "World.Save.Snapshot"'s 'SessionSnapshot'.
--
--   The envelope's own framing version ('currentEnvelopeVersion') is
--   independent of every component's schema version — the framing
--   contract has not changed for B2, so it stays as B1 assigned it (1);
--   component evolution now uses component versions, not a global save
--   bump (requirement 15). 'World.Save.Types.currentSaveVersion' is
--   likewise untouched by this change — it now only versions the
--   transitional 'SaveData'/'WorldPageSave' bridge shape used by the
--   load path, no longer any wire contract.
module World.Save.Envelope
    ( currentEnvelopeVersion
    , metadataComponentId
    , metadataComponentVersion
    , encodeSessionSnapshot
    , decodeSessionEnvelope
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
import World.Save.Types (SaveMetadata)
import World.Save.Snapshot (SessionSnapshot)
import World.Save.Component
    (componentKnownIds, componentRequiredIds
    , encodeComponentSpecs, assembleSnapshot)
import World.Save.Component.Types
    (ComponentError, renderComponentError, metadataComponentId)

-- | The envelope-framing generation (requirement 15): bumped only when
--   the FRAMING contract itself changes incompatibly — never merely
--   because a carried component's own schema version changes. B2 does
--   not change the framing, so this stays at B1's assigned 1.
currentEnvelopeVersion ∷ Word32
currentEnvelopeVersion = 1

-- | The @"metadata"@ component's own schema version — a small,
--   independent counter (requirement 3). Bump this, not
--   'currentEnvelopeVersion', if 'SaveMetadata''s shape ever changes.
metadataComponentVersion ∷ Word32
metadataComponentVersion = 1

-- | Every component id this reader knows how to interpret: the metadata
--   component plus every registered gameplay component.
knownComponentIds ∷ HS.HashSet ComponentId
knownComponentIds = HS.insert metadataComponentId componentKnownIds

-- | Every component this reader HARD-requires regardless of a writer's
--   own 'cdRequired' flag (requirement 6/7): metadata + every required
--   gameplay component (all of them, currently).
readerRequiredComponentIds ∷ HS.HashSet ComponentId
readerRequiredComponentIds = HS.insert metadataComponentId componentRequiredIds

renderEnvelopeError ∷ EnvelopeError → Text
renderEnvelopeError = T.pack . show

renderComponentErrors ∷ [ComponentError] → Text
renderComponentErrors = T.intercalate "; " . map renderComponentError

-- | Encode a fully-captured, already-validated 'SessionSnapshot' plus
--   its manifest 'SaveMetadata' into a checksummed component envelope.
--   Total (not 'Either') so the caller
--   ('World.Thread.Command.Save.WriteWorld') can force a strict
--   'BS.ByteString' to WHNF and, in doing so, force every component's
--   full cereal traversal BEFORE releasing the #757 barrier (a deferred
--   exploding thunk buried in any component surfaces right here, as a
--   capture failure, not later during the disk write). A 'Left' from the
--   codec means our own generous 'defaultEnvelopeLimits' were exceeded —
--   effectively unreachable for a real save — so 'error' (caught as a
--   capture failure at that call site) is the right response.
encodeSessionSnapshot ∷ SaveMetadata → SessionSnapshot → BS.ByteString
encodeSessionSnapshot meta snap =
    let metaSpec = (metadataComponentId, metadataComponentVersion, True
                   , S.encode meta)
        specs    = metaSpec : encodeComponentSpecs snap
    in case encodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion specs of
        Right bytes → bytes
        Left err    → error ("encodeSessionSnapshot: "
                             <> T.unpack (renderEnvelopeError err))

-- | Decode + validate the envelope structure, then decode ONLY the
--   @"metadata"@ component (requirement 12: metadata inspection without
--   gameplay decoding). Its own schema version is checked before
--   'S.decode' ever runs.
decodeSaveEnvelopeMetadata ∷ BS.ByteString → Either Text SaveMetadata
decodeSaveEnvelopeMetadata bytes = do
    decoded ← decodeValidatedEnvelope bytes
    decodeMetadataComponent decoded

-- | Decode + validate the envelope, decode the metadata component, then
--   reconstruct the complete, cross-validated 'SessionSnapshot' from all
--   gameplay components (requirement 6). Returns both so the caller has
--   the authoritative metadata (name/timestamp) alongside the gameplay
--   state.
decodeSessionEnvelope ∷ BS.ByteString → Either Text (SaveMetadata, SessionSnapshot)
decodeSessionEnvelope bytes = do
    decoded ← decodeValidatedEnvelope bytes
    meta    ← decodeMetadataComponent decoded
    snap    ← either (Left . renderComponentErrors) Right
                     (assembleSnapshot meta decoded)
    pure (meta, snap)

decodeValidatedEnvelope ∷ BS.ByteString → Either Text DecodedEnvelope
decodeValidatedEnvelope =
    either (Left . renderEnvelopeError) Right
        . decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
                          knownComponentIds readerRequiredComponentIds

-- | Decode the @"metadata"@ component, rejecting an unsupported schema
--   version before touching its bytes. The descriptor/payload "missing"
--   cases are unreachable — metadata is in 'readerRequiredComponentIds'.
decodeMetadataComponent ∷ DecodedEnvelope → Either Text SaveMetadata
decodeMetadataComponent decoded = do
    desc ← maybe (Left "metadata component descriptor missing \
                        \(unreachable — already required)") Right
                 (findDescriptor metadataComponentId (deManifest decoded))
    when (cdVersion desc ≢ metadataComponentVersion) $
        Left ("Save format incompatible: expected metadata component v"
              <> T.pack (show metadataComponentVersion) <> ", got v"
              <> T.pack (show (cdVersion desc)))
    payload ← maybe (Left "metadata component payload missing \
                           \(unreachable — already required)") Right
                    (HM.lookup metadataComponentId (dePayloads decoded))
    either (Left . (("Failed to decode metadata component: ") <>) . T.pack)
           Right (S.decode payload)

findDescriptor ∷ ComponentId → EnvelopeManifest → Maybe ComponentDescriptor
findDescriptor cid manifest =
    listToMaybe [ d | d ← emComponents manifest, cdId d ≡ cid ]
