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
--     @"texture-palette"@, @"world-pages"@, @"world-edits"@,
--     @"world-activity"@, @"buildings"@, @"units"@, @"unit-sim"@,
--     @"craft-bills"@, @"power-nodes"@ — each independently versioned,
--     converted to/from "World.Save.Snapshot"'s 'SessionSnapshot'.
--   - a DYNAMIC set of Lua-owned components (issue #761, save-overhaul
--     B3), one per module registered with
--     @scripts/lib/save_modules.lua@, each riding under the reserved
--     @"lua.<module>"@ id ("World.Save.Component.Types.luaComponentPrefix")
--     in the SAME manifest namespace as the Haskell set above. Unlike
--     the Haskell set, this module has no static knowledge of which Lua
--     ids exist — every function below that touches them takes the
--     CURRENT Lua registry's bare (unprefixed) names as an explicit
--     parameter, gathered by the caller
--     ("Engine.Scripting.Lua.API.Save", which alone has Lua access) via
--     @saveModules.describeAll()@ before encoding/decoding. This mirrors
--     exactly how the Haskell side's OWN "current build's registry"
--     already governs known/required ids — a foreign id this build
--     doesn't recognise (Haskell OR Lua) is an incompatibility, not
--     silently ignored.
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
    , LuaComponentSpec
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
    (ComponentError, renderComponentError, metadataComponentId
    , luaComponentPrefix)

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

-- | One Lua-owned component: its bare (unprefixed) registry id, schema
--   version, the writer's own required/optional declaration, and its
--   already-canonically-encoded payload bytes (see
--   @scripts/lib/data_codec.lua@) — the same shape
--   "World.Save.Envelope.Codec.ComponentSpec" uses for the Haskell set,
--   minus the id-namespacing this module owns (issue #761).
type LuaComponentSpec = (Text, Word32, Bool, BS.ByteString)

luaComponentId ∷ Text → ComponentId
luaComponentId name = ComponentId (luaComponentPrefix <> name)

-- | Every component id this reader knows how to interpret: the metadata
--   component, every registered Haskell gameplay component, and every
--   NAME the caller reports as currently Lua-registered (prefixed into
--   the reserved @"lua."@ namespace).
knownComponentIds ∷ HS.HashSet Text → HS.HashSet ComponentId
knownComponentIds luaNames =
    HS.insert metadataComponentId componentKnownIds
        `HS.union` HS.map luaComponentId luaNames

-- | Every component this reader HARD-requires regardless of a writer's
--   own 'cdRequired' flag (requirement 6/7): metadata + every required
--   Haskell gameplay component + every NAME the caller reports as
--   currently required in the live Lua registry.
readerRequiredComponentIds ∷ HS.HashSet Text → HS.HashSet ComponentId
readerRequiredComponentIds luaRequiredNames =
    HS.insert metadataComponentId componentRequiredIds
        `HS.union` HS.map luaComponentId luaRequiredNames

renderEnvelopeError ∷ EnvelopeError → Text
renderEnvelopeError = T.pack . show

renderComponentErrors ∷ [ComponentError] → Text
renderComponentErrors = T.intercalate "; " . map renderComponentError

-- | Encode a fully-captured, already-validated 'SessionSnapshot' plus
--   its manifest 'SaveMetadata' and every currently-registered Lua
--   component's already-encoded payload into a checksummed component
--   envelope. Total (not 'Either') so the caller
--   ('World.Thread.Command.Save.WriteWorld') can force a strict
--   'BS.ByteString' to WHNF and, in doing so, force every component's
--   full cereal traversal BEFORE releasing the #757 barrier (a deferred
--   exploding thunk buried in any component surfaces right here, as a
--   capture failure, not later during the disk write). A 'Left' from the
--   codec means our own generous 'defaultEnvelopeLimits' were exceeded,
--   OR a Lua component id collided with another spec (structurally
--   unreachable given the reserved @"lua."@ prefix, but still reported
--   rather than silently dropped) — either way 'error' (caught as a
--   capture failure at that call site) is the right response.
encodeSessionSnapshot
    ∷ SaveMetadata → SessionSnapshot → [LuaComponentSpec] → BS.ByteString
encodeSessionSnapshot meta snap luaSpecs =
    let metaSpec   = (metadataComponentId, metadataComponentVersion, True
                     , S.encode meta)
        luaEnvSpecs = [ (luaComponentId name, ver, req, payload)
                      | (name, ver, req, payload) ← luaSpecs ]
        specs = metaSpec : encodeComponentSpecs snap ⧺ luaEnvSpecs
    in case encodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion specs of
        Right bytes → bytes
        Left err    → error ("encodeSessionSnapshot: "
                             <> T.unpack (renderEnvelopeError err))

-- | Decode + validate the envelope structure, then decode ONLY the
--   @"metadata"@ component (requirement 12: metadata inspection without
--   gameplay decoding). Its own schema version is checked before
--   'S.decode' ever runs. @luaKnownNames@ is the current Lua registry's
--   bare ids (requirement: an unrecognised Lua-required component must
--   not make a save unlistable, so this needs the SAME known-id
--   widening the full decode gets — never any Lua REQUIRED-ness, since
--   listing never demands a Lua component be present).
decodeSaveEnvelopeMetadata
    ∷ HS.HashSet Text → BS.ByteString → Either Text SaveMetadata
decodeSaveEnvelopeMetadata luaKnownNames bytes = do
    decoded ← decodeValidatedEnvelope luaKnownNames HS.empty bytes
    decodeMetadataComponent decoded

-- | Decode + validate the envelope, decode the metadata component, then
--   reconstruct the complete, cross-validated 'SessionSnapshot' from all
--   Haskell gameplay components (requirement 6), returning it alongside
--   every present Lua component's raw (name, version, payload) —
--   Haskell never interprets a Lua component's bytes itself; the caller
--   hands these to @saveModules.prepareLoad@/@applyAll@.
--   @luaKnownNames@/@luaRequiredNames@ are the CURRENT Lua registry's
--   ids (gathered via @saveModules.describeAll()@ before this call).
decodeSessionEnvelope
    ∷ HS.HashSet Text → HS.HashSet Text → BS.ByteString
    → Either Text (SaveMetadata, SessionSnapshot, [LuaComponentSpec])
decodeSessionEnvelope luaKnownNames luaRequiredNames bytes = do
    decoded ← decodeValidatedEnvelope luaKnownNames luaRequiredNames bytes
    meta    ← decodeMetadataComponent decoded
    snap    ← either (Left . renderComponentErrors) Right
                     (assembleSnapshot meta decoded)
    pure (meta, snap, extractLuaComponents decoded)

-- | Every component in the decoded envelope whose id carries the
--   reserved @"lua."@ prefix, with that prefix stripped back to the bare
--   registry name Lua itself uses.
extractLuaComponents ∷ DecodedEnvelope → [LuaComponentSpec]
extractLuaComponents de =
    [ (name, cdVersion d, cdRequired d, payload)
    | d ← emComponents (deManifest de)
    , Just name ← [T.stripPrefix luaComponentPrefix (cidText (cdId d))]
    , Just payload ← [HM.lookup (cdId d) (dePayloads de)]
    ]
  where cidText (ComponentId t) = t

decodeValidatedEnvelope
    ∷ HS.HashSet Text → HS.HashSet Text → BS.ByteString
    → Either Text DecodedEnvelope
decodeValidatedEnvelope luaKnownNames luaRequiredNames =
    either (Left . renderEnvelopeError) Right
        . decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
                          (knownComponentIds luaKnownNames)
                          (readerRequiredComponentIds luaRequiredNames)

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
decodeClassifiedEnvelope
    ∷ HS.HashSet Text → HS.HashSet Text → BS.ByteString
    → Either GenerationFailure DecodedEnvelope
decodeClassifiedEnvelope luaKnownNames luaRequiredNames bytes =
    case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
                         (knownComponentIds luaKnownNames)
                         (readerRequiredComponentIds luaRequiredNames) bytes of
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
    ∷ HS.HashSet Text → BS.ByteString → Either GenerationFailure SaveMetadata
decodeSaveEnvelopeMetadataClassified luaKnownNames bytes = do
    decoded ← decodeClassifiedEnvelope luaKnownNames HS.empty bytes
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
    ∷ HS.HashSet Text → HS.HashSet Text → BS.ByteString
    → Either GenerationFailure (SaveMetadata, SessionSnapshot, [LuaComponentSpec])
decodeSessionEnvelopeClassified luaKnownNames luaRequiredNames bytes = do
    decoded ← decodeClassifiedEnvelope luaKnownNames luaRequiredNames bytes
    meta ← either (Left . GenerationIncompatible) Right
                  (decodeMetadataComponent decoded)
    snap ← either (Left . GenerationIncompatible . renderComponentErrors)
                  Right (assembleSnapshot meta decoded)
    pure (meta, snap, extractLuaComponents decoded)

-- | Decode the @"metadata"@ component, rejecting an unsupported schema
--   version before touching its bytes. The descriptor/payload "missing"
--   cases are unreachable — metadata is always in the reader's required
--   set.
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
