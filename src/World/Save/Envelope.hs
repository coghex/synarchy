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
--
--   INTENTIONAL INCOMPATIBILITY: a save written by B1-era code (a
--   single required @"session"@ component, no gameplay components) can
--   no longer be loaded once this module lands — @"session"@ is
--   dropped from 'knownComponentIds', so 'decodeSessionEnvelope' fails
--   it as an unknown required component (requirement 13: retire the
--   transitional payload; #760's acceptance explicitly permits this in
--   place of a migration, provided it is documented and tested — see
--   "Test.Headless.World.Save.Components"'s \"B1 -> B2 intentional
--   incompatibility\" case). A real migration from the transitional
--   shape is out of scope for B2; #760's Related section assigns
--   long-lived compatibility fixtures/migrations to a future C4.
--   Metadata-only reads ('decodeSaveEnvelopeMetadata', i.e.
--   'World.Save.Serialize.listSaves') validate the SAME envelope
--   structure before ever touching the metadata payload, so a B1-era
--   save also can't be listed under B2 — it disappears from the save
--   browser entirely rather than appearing but failing to load.
module World.Save.Envelope
    ( currentEnvelopeVersion
    , metadataComponentId
    , metadataComponentVersion
    , encodeSessionSnapshot
    , decodeSessionEnvelope
    , decodeSaveEnvelopeMetadata
    , GenerationFailure(..)
    , renderGenerationFailure
    , isRecoverableEnvelopeError
    , decodeSessionEnvelopeClassified
    , decodeSaveEnvelopeMetadataClassified
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

-- | Load-time generation-validity classification (issue #762, storage-
--   overhaul C1): whether a structurally-invalid file is safe to treat as
--   RECOVERABLE STORAGE CORRUPTION — eligible for previous-generation
--   fallback ('World.Save.Storage.selectLoadGeneration') — versus a
--   well-formed-but-semantically-INCOMPATIBLE file, which must be
--   reported directly and never silently rolled back to an older
--   generation (issue #762 requirement 7: "Do not silently fall back
--   merely because a structurally valid authoritative generation has
--   unsupported component versions, fails gameplay/content validation, or
--   cannot be migrated").
data GenerationFailure
    = GenerationCorrupt !Text
        -- ^ Absent, truncated, bad framing, or a checksum failure — the
        --   kind of damage an interrupted publish can actually leave
        --   behind. Safe to fall back to a previous generation.
    | GenerationIncompatible !Text
        -- ^ The bytes are structurally coherent (checksums agree) but
        --   this build cannot interpret them as a valid session: an
        --   unsupported envelope/component version, an unknown/missing
        --   required component, or an assembly/content-validation
        --   failure. Never a fallback trigger — reporting the real
        --   compatibility problem beats silently rolling back progress.
    deriving (Show, Eq)

renderGenerationFailure ∷ GenerationFailure → Text
renderGenerationFailure (GenerationCorrupt t)      = t
renderGenerationFailure (GenerationIncompatible t) = t

-- | Classifies exactly why 'decodeEnvelope' failed. 'True' ⇒ the failure
--   is the shape routine storage corruption takes (missing/truncated/
--   malformed framing, or any checksum mismatch) — everything
--   'decodeEnvelope' can detect BEFORE trusting a single component byte,
--   i.e. before any question of schema compatibility even arises. 'False'
--   ⇒ the envelope is internally consistent (every checksum agreed) but
--   declares a version/component set this reader doesn't recognise —
--   only a foreign (older/future/other-build) file produces these, never
--   this build's own interrupted write, so treating them as fallback
--   triggers would mask a genuine compatibility problem instead of
--   recovering from damage.
isRecoverableEnvelopeError ∷ EnvelopeError → Bool
isRecoverableEnvelopeError e = case e of
    TruncatedHeader              → True
    BadMagic                     → True
    ManifestTooLarge _           → True
    ManifestTruncated            → True
    ManifestMalformed _          → True
    ManifestChecksumMismatch     → True
    TooManyComponents _          → True
    ComponentIdTooLong _         → True
    DuplicateComponentId _       → True
    PayloadTooLarge _ _          → True
    TotalPayloadTooLarge _       → True
    OverlappingComponents _ _    → True
    TruncatedPayload _           → True
    ComponentChecksumMismatch _  → True
    TrailingBytes _              → True
    UnsupportedEnvelopeVersion _ → False
    UnknownRequiredComponent _   → False
    MissingRequiredComponent _   → False
    UnsupportedComponentVersion _ _ → False
    ComponentDecodeError _ _         → False

-- | Shared entry point for both classified decoders below: validate the
--   envelope structure, classifying a failure per 'GenerationFailure'.
decodeClassifiedEnvelope ∷ BS.ByteString → Either GenerationFailure DecodedEnvelope
decodeClassifiedEnvelope bytes =
    case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
                         knownComponentIds readerRequiredComponentIds bytes of
        Left err
          | isRecoverableEnvelopeError err
                      → Left (GenerationCorrupt (renderEnvelopeError err))
          | otherwise → Left (GenerationIncompatible (renderEnvelopeError err))
        Right decoded → Right decoded

-- | 'decodeSaveEnvelopeMetadata', classified per 'GenerationFailure' —
--   the metadata-only read the storage layer's load-source selection
--   uses so it never has to decode a whole gameplay payload just to
--   decide whether the authoritative file is even worth trying.
decodeSaveEnvelopeMetadataClassified
    ∷ BS.ByteString → Either GenerationFailure SaveMetadata
decodeSaveEnvelopeMetadataClassified bytes = do
    decoded ← decodeClassifiedEnvelope bytes
    either (Left . GenerationIncompatible) Right (decodeMetadataComponent decoded)

-- | 'decodeSessionEnvelope', classified per 'GenerationFailure' — the
--   full decode 'World.Save.Storage.selectLoadGeneration' uses to decide,
--   for one candidate file, whether a load failure is eligible for
--   previous-generation fallback (requirement 7). A component-level
--   decode/migrate/validate/assemble failure (an 'assembleSnapshot'
--   'ComponentError', e.g. an unsupported component schema version or a
--   cross-component invariant violation) always classifies as
--   'GenerationIncompatible': by the time 'assembleSnapshot' runs, every
--   component's own per-payload checksum has already passed inside
--   'decodeEnvelope', so a failure here reflects a genuine schema/content
--   mismatch, never routine bit-level corruption.
decodeSessionEnvelopeClassified
    ∷ BS.ByteString → Either GenerationFailure (SaveMetadata, SessionSnapshot)
decodeSessionEnvelopeClassified bytes = do
    decoded ← decodeClassifiedEnvelope bytes
    meta ← either (Left . GenerationIncompatible) Right
                  (decodeMetadataComponent decoded)
    snap ← either (Left . GenerationIncompatible . renderComponentErrors)
                  Right (assembleSnapshot meta decoded)
    pure (meta, snap)

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
