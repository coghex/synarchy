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
--   B1 COMPATIBILITY (issue #766, save-overhaul C4): a save written by
--   B1-era code (a single required, now-unknown @"session"@ component,
--   no gameplay components) is no longer decodable through the modern,
--   registry-driven path above — @"session"@ was dropped from
--   'knownComponentIds' by #760 and never re-added — but every decode
--   entry point below (full decode AND metadata-only listing) falls
--   back to 'decodeLegacySessionEnvelope' when the modern attempt fails,
--   recognizing exactly that shape and migrating it through
--   "World.Save.Compat.SessionV90" into a fully-validated modern
--   'SessionSnapshot'. This is the migration #760's own acceptance
--   explicitly deferred to "a future C4" rather than a permanent
--   incompatibility. A genuinely modern envelope's decode path and error
--   text are completely unchanged — the legacy fallback only ever
--   triggers after the modern attempt has already failed, and only ever
--   succeeds for a manifest that is exactly @{metadata, session}@, which
--   this build never writes.
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
    , foreignOptionalComponentIds
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
import World.Save.Compat.SessionV90
    ( sessionComponentId, sessionComponentVersion
    , decodeSessionV90, migrateSessionV90 )

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
decodeSaveEnvelopeMetadata luaKnownNames bytes =
    case decodeValidatedEnvelope luaKnownNames HS.empty bytes
             >>= decodeMetadataComponent of
        Right meta     → Right meta
        Left modernErr → case tryLegacyMetadataFallbacks bytes of
            NotLegacyShaped         → Left modernErr
            LegacyShapedButFailed e → Left e
            LegacyDecoded meta      → Right meta

-- | Decode + validate the envelope, decode the metadata component, then
--   reconstruct the complete, cross-validated 'SessionSnapshot' from all
--   Haskell gameplay components (requirement 6), returning it alongside
--   every present Lua component's raw (name, version, payload) —
--   Haskell never interprets a Lua component's bytes itself; the caller
--   hands these to @saveModules.prepareLoad@/@applyAll@. The trailing
--   'Bool' is 'True' iff this generation was reconstructed via the
--   legacy pre-#760 B1 migration (issue #766) rather than the modern
--   registry-driven path — in which case @[LuaComponentSpec]@ is always
--   empty (B1 predates every Lua-owned persistent component), and the
--   caller must supply each currently-required Lua module's own
--   empty-state default instead of treating it as missing
--   (@saveModules.prepareLoad@'s @isMigratingLegacyBaseline@ parameter).
--   @luaKnownNames@/@luaRequiredNames@ are the CURRENT Lua registry's
--   ids (gathered via @saveModules.describeAll()@ before this call).
decodeSessionEnvelope
    ∷ HS.HashSet Text → HS.HashSet Text → BS.ByteString
    → Either Text (SaveMetadata, SessionSnapshot, [LuaComponentSpec], Bool)
decodeSessionEnvelope luaKnownNames luaRequiredNames bytes =
    case decodeModern of
        Right result   → Right result
        Left modernErr → case tryLegacyEnvelopeFallbacks bytes of
            NotLegacyShaped         → Left modernErr
            LegacyShapedButFailed e → Left e
            LegacyDecoded (meta, snap) → Right (meta, snap, [], True)
  where
    decodeModern = do
        decoded ← decodeValidatedEnvelope luaKnownNames luaRequiredNames bytes
        meta    ← decodeMetadataComponent decoded
        snap    ← either (Left . renderComponentErrors) Right
                         (assembleSnapshot meta decoded)
        pure (meta, snap, extractLuaComponents decoded, False)

-- | The outcome of attempting the legacy B1 fallback (issue #766,
--   save-overhaul C4). Distinguishes "this envelope was never B1-shaped
--   at all" (silently defer to the modern attempt's own error — a
--   genuinely modern file with a real problem must keep reporting ITS
--   OWN, more specific error, e.g. \"missing required component X\",
--   never the less helpful \"unknown required component X\" the legacy
--   attempt's narrower known-id set would otherwise produce) from "this
--   IS a structurally-recognized B1 envelope, but migrating it failed
--   for a real reason" (report THAT failure — once the manifest is
--   confirmed to be exactly @{metadata, session}@, any later failure is
--   the genuine, actionable reason, not noise to discard).
data LegacyDecodeResult a
    = NotLegacyShaped
    | LegacyShapedButFailed !Text
    | LegacyDecoded !a

-- | Recognize and migrate a pre-#760 B1 envelope (issue #766,
--   save-overhaul C4): a single required @"session"@ component wrapping
--   the frozen v90 'World.Save.Compat.SessionV90.SaveDataV90' shape,
--   alongside the ordinary @"metadata"@ component. Tried ONLY after the
--   modern registry-driven decode has already failed (every call site
--   above), so a genuinely modern envelope's error text and behaviour
--   are completely unchanged (requirement 6: migration never
--   participates in the normal path) — this only ever recognizes a
--   structurally-valid envelope whose manifest is exactly
--   @{metadata, session}@, which a current build never writes.
decodeLegacySessionEnvelope
    ∷ BS.ByteString → LegacyDecodeResult (SaveMetadata, SessionSnapshot)
decodeLegacySessionEnvelope bytes =
    case decodeLegacyStructureAndMetadata bytes of
        Nothing              → NotLegacyShaped
        Just (Left e)        → LegacyShapedButFailed e
        Just (Right (decoded, meta)) →
            either LegacyShapedButFailed LegacyDecoded $ do
                payload ← maybe (Left "legacy session component payload missing")
                                Right
                                (HM.lookup sessionComponentId (dePayloads decoded))
                sd   ← either (Left . renderComponentError) Right
                              (decodeSessionV90 payload)
                snap ← either (Left . renderComponentErrors) Right
                              (migrateSessionV90 meta sd)
                pure (meta, snap)

-- | Metadata-only counterpart to 'decodeLegacySessionEnvelope'
--   (requirement 12's "listing never decodes gameplay" contract applies
--   to the legacy fallback too): validate the SAME legacy envelope
--   structure and return the @"metadata"@ component alone, WITHOUT
--   decoding or migrating @"session"@ — a B1-era save is listable even
--   when its gameplay migration would separately fail (e.g. a
--   manifest/gameplay mismatch requirement 12 only started enforcing
--   after B1), exactly mirroring how the modern format's own metadata
--   listing never runs 'assembleSnapshot'.
decodeLegacySessionMetadata ∷ BS.ByteString → LegacyDecodeResult SaveMetadata
decodeLegacySessionMetadata bytes =
    case decodeLegacyStructureAndMetadata bytes of
        Nothing               → NotLegacyShaped
        Just (Left e)         → LegacyShapedButFailed e
        Just (Right (_, meta)) → LegacyDecoded meta

-- | Shared structural step for both legacy entry points above: validate
--   the envelope against exactly @{metadata, session}@ AND confirm both
--   descriptors' own flags/versions match the ONE frozen B1 shape this
--   build recognizes (round-7 review) — not merely that those two ids
--   are the only ones PRESENT. 'decodeEnvelope' only ever refuses a
--   descriptor marked @cdRequired@ that ISN'T in the known-id set, or an
--   id in the reader's OWN required set that's altogether absent — it
--   never checks whether a PRESENT descriptor's own @cdRequired@ flag
--   matches what a genuine writer would have marked. So an envelope
--   shaped @{metadata (required), session (OPTIONAL, v90)}@ would
--   otherwise satisfy the exact-id-set check below and get migrated as
--   if it were the real, always-required B1 shape — silently replacing
--   an unsupported/foreign optional payload rather than retaining or
--   refusing it (requirement 9), since 'foreignOptionalComponentIds'
--   would ALSO recognize it as "known" via the very same shape check.
--   'Nothing' means the envelope's manifest itself is not @{metadata,
--   session}@ (or is otherwise structurally invalid under that known/
--   required set) — not a B1 envelope at all, so the caller must defer
--   entirely to the modern attempt's own error. A 'Just' — structurally
--   confirmed B1-shaped, both descriptors' flags/versions matching —
--   carries either the metadata (component-version check passed) or the
--   specific reason it didn't; either way, this is no longer "maybe not
--   legacy", so the caller must surface it.
decodeLegacyStructureAndMetadata
    ∷ BS.ByteString → Maybe (Either Text (DecodedEnvelope, SaveMetadata))
decodeLegacyStructureAndMetadata bytes =
    case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
             b1LegacyIds b1LegacyIds bytes of
        Left _ → Nothing
        Right decoded → Just $ do
            -- Requirement 9: 'decodeEnvelope' tolerates an unknown OPTIONAL
            -- component structurally (that is what makes forward
            -- compatibility possible at all), but a genuine B1 envelope
            -- never carried anything beyond these two -- an extra
            -- component, known or not, required or not, means this is NOT
            -- the frozen B1 shape (tampering, corruption, or a foreign
            -- format this build genuinely does not understand), so it
            -- must be reported rather than silently migrated with that
            -- extra payload dropped on the floor.
            let present = presentComponentIds decoded
            when (present ≢ b1LegacyIds) $
                Left ("legacy envelope does not carry exactly the frozen \
                      \B1 component set {metadata, session} (found: "
                      <> T.intercalate ", " (map cidText (HS.toList present))
                      <> ") -- refusing to migrate rather than silently \
                         \drop the extra component")
            metaDesc ← maybe (Left "legacy metadata component descriptor missing")
                              Right
                              (findDescriptor metadataComponentId (deManifest decoded))
            sessionDesc ← maybe (Left "legacy session component descriptor missing")
                                 Right
                                 (findDescriptor sessionComponentId (deManifest decoded))
            -- Both descriptors must be marked REQUIRED, exactly as this
            -- build's own (never-written-again) B1 writer always marked
            -- them -- an OPTIONAL "session" (or "metadata") is not the
            -- real frozen shape, even though its id and version match.
            when (not (cdRequired metaDesc) ∨ not (cdRequired sessionDesc)) $
                Left "legacy envelope's metadata/session descriptors are \
                     \not both marked required, as the real frozen B1 \
                     \shape always does -- refusing to migrate rather \
                     \than treat an optional payload as the required one"
            when (cdVersion metaDesc ≢ metadataComponentVersion) $
                Left ("Save format incompatible: expected legacy metadata \
                      \component v" <> T.pack (show metadataComponentVersion)
                      <> ", got v" <> T.pack (show (cdVersion metaDesc)))
            when (cdVersion sessionDesc ≢ sessionComponentVersion) $
                Left ("Save format incompatible: expected legacy session \
                      \component v" <> T.pack (show sessionComponentVersion)
                      <> ", got v" <> T.pack (show (cdVersion sessionDesc)))
            meta ← decodeMetadataComponent decoded
            pure (decoded, meta)
  where
    cidText (ComponentId t) = t

-- | Every component id actually present in a decoded envelope's manifest.
presentComponentIds ∷ DecodedEnvelope → HS.HashSet ComponentId
presentComponentIds decoded =
    HS.fromList (map cdId (emComponents (deManifest decoded)))

-- | The exact frozen B1 component-id set: precisely @{metadata, session}@,
--   no more, no less. Hoisted to top level (round-9 review) so
--   'decodeLegacyStructureAndMetadata' and 'foreignOptionalComponentIds'
--   share the one definition rather than risk two copies drifting apart.
b1LegacyIds ∷ HS.HashSet ComponentId
b1LegacyIds = HS.fromList [metadataComponentId, sessionComponentId]

-- | Whether a decoded envelope's descriptor for the given component id is
--   present, marked REQUIRED, and at EXACTLY the given schema version.
--   The one per-descriptor precision check every legacy-shape recognizer
--   needs (round-9 review): shared by the migrating decoders AND the
--   overwrite guard so the two can never independently reimplement a
--   PARTIAL version of it and drift apart again, as happened in rounds
--   7-9 (the guard mirrored the check for "session"/"lua-state" but
--   missed "metadata" in B1 and every non-"lua-state" descriptor in B2).
descriptorIsExactlyRequiredAt
    ∷ DecodedEnvelope → ComponentId → Word32 → Bool
descriptorIsExactlyRequiredAt decoded cid ver =
    maybe False (\d → cdRequired d ∧ cdVersion d ≡ ver)
          (findDescriptor cid (deManifest decoded))

-- | Whether a decoded envelope's descriptor for the given component id is
--   present and marked REQUIRED, at any version -- used for B2's Haskell
--   components besides "lua-state", whose own historical-version
--   acceptance is validated separately by 'assembleSnapshot' rather than
--   at this structural-shape level.
descriptorIsRequired ∷ DecodedEnvelope → ComponentId → Bool
descriptorIsRequired decoded cid =
    maybe False cdRequired (findDescriptor cid (deManifest decoded))

-- | The pre-#761 single opaque Lua persistence blob (issue #766,
--   requirement 3's "#760" baseline): every registered Lua module's
--   state serialized together into one component, before #761 split it
--   into independently-versioned per-module components
--   ("lua.unit_ai"/"lua.building_spawn" etc.).
luaStateComponentId ∷ ComponentId
luaStateComponentId = ComponentId "lua-state"

-- | The only schema version a genuine #760-era writer ever produced for
--   the opaque blob above (round-8 review): this fallback only knows how
--   to interpret THIS version's meaning of "empty payload migrates
--   cleanly, non-empty is refused" -- an envelope claiming some OTHER
--   version is not the recognized #760 shape at all (a hypothetical
--   future schema change to the opaque blob could reuse the same
--   required flag and an empty payload while meaning something entirely
--   different), so it must not be silently accepted and re-saved without
--   that unknown version ever being recorded again.
luaStateComponentVersion ∷ Word32
luaStateComponentVersion = 1

-- | The exact #760-era ("B2") component-id set: "metadata" plus every
--   Haskell gameplay component plus the single opaque "lua-state" blob.
--   Hoisted to top level (round-9 review) so 'decodeB2StructureAndMetadata'
--   and 'foreignOptionalComponentIds' share the one definition rather than
--   risk two copies drifting apart.
b2Ids ∷ HS.HashSet ComponentId
b2Ids = HS.insert metadataComponentId
            (HS.insert luaStateComponentId componentKnownIds)

-- | Shared structural step for the "B2" (#760-era) fallback: an
--   envelope whose component set is EXACTLY the modern Haskell registry
--   ("metadata" plus every @saveComponentRegistry@ entry -- core-
--   session/texture-palette/world-pages/world-edits/world-activity/
--   buildings/units/unit-sim/craft-bills/power-nodes, each already
--   decodable at its historical input version via the SAME
--   ccInputVers-dispatched codecs the modern path uses) plus the single
--   opaque @"lua-state"@ blob -- i.e. Haskell ALREADY split per-
--   component (#760), but Lua NOT YET split into its own versioned
--   components (#761 hadn't landed yet). Every descriptor must be
--   marked required, matching the real #760 writer -- an optional one
--   is not that genuine historical shape (mirrors the B1 fallback's
--   identical precision, round-7 review). The @"lua-state"@ descriptor's
--   own version must ALSO match 'luaStateComponentVersion' exactly
--   (round-8 review): the Haskell components are decoded through the
--   real ccInputVers dispatch, which already validates their own
--   historical version, but the opaque Lua blob is never actually
--   interpreted, only checked for emptiness below -- so an envelope
--   claiming some OTHER lua-state schema version must not be silently
--   accepted as if it meant the same "empty migrates, non-empty
--   refuses" contract this build knows, and then re-saved without that
--   unknown version ever being recorded again. 'Nothing' means defer
--   entirely to whatever error the caller already has (this envelope
--   is not #760-shaped at all); a 'Just' is no longer "maybe", so the
--   caller must surface it.
decodeB2StructureAndMetadata
    ∷ BS.ByteString → Maybe (Either Text (DecodedEnvelope, SaveMetadata))
decodeB2StructureAndMetadata bytes =
    case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
             b2Ids b2Ids bytes of
        Left _ → Nothing
        Right decoded →
            let present = presentComponentIds decoded
            in if present ≢ b2Ids then Nothing else Just $ do
                forM_ (HS.toList b2Ids) $ \cid → do
                    desc ← maybe (Left (cidText cid <> " descriptor missing"))
                                 Right (findDescriptor cid (deManifest decoded))
                    when (not (cdRequired desc)) $
                        Left (cidText cid <> " is not marked required, as \
                             \the real #760 shape always does -- refusing \
                             \to migrate rather than treat an optional \
                             \payload as the required one")
                -- Every id's required flag is already confirmed by the
                -- loop above (lua-state included) -- this only remains to
                -- reject a version other than the one genuine #760
                -- writers always used (round-8 review).
                luaStateDesc ← maybe (Left "lua-state descriptor missing")
                                     Right
                                     (findDescriptor luaStateComponentId
                                                      (deManifest decoded))
                when (cdVersion luaStateDesc ≢ luaStateComponentVersion) $
                    Left ("Save format incompatible: expected legacy \
                          \\"lua-state\" component v"
                          <> T.pack (show luaStateComponentVersion)
                          <> ", got v" <> T.pack (show (cdVersion luaStateDesc)))
                meta ← decodeMetadataComponent decoded
                pure (decoded, meta)
  where
    cidText (ComponentId t) = t

-- | Metadata-only counterpart to 'decodeB2SessionEnvelope', mirroring
--   'decodeLegacySessionMetadata''s identical contract for the B1 case:
--   a #760-era save is listable without decoding/migrating its (opaque,
--   possibly-unmigratable) Lua state.
decodeB2SessionMetadata ∷ BS.ByteString → LegacyDecodeResult SaveMetadata
decodeB2SessionMetadata bytes =
    case decodeB2StructureAndMetadata bytes of
        Nothing                → NotLegacyShaped
        Just (Left e)          → LegacyShapedButFailed e
        Just (Right (_, meta)) → LegacyDecoded meta

-- | Recognize and migrate a #760-era envelope (issue #766, requirement
--   3): reuses 'assembleSnapshot' UNCHANGED for every Haskell component
--   (they are already the modern per-component registry, historical
--   input versions and all), then handles the one genuinely legacy
--   piece -- the opaque @"lua-state"@ blob -- the SAME way
--   'migrateSessionV90' already handles B1's own legacy Lua blob map: an
--   EMPTY payload (the common real case -- most #760-era saves predate
--   any meaningfully persisted Lua state) migrates cleanly, defaulting
--   every current Lua module via the engine's existing
--   @isMigratingLegacyBaseline@ mechanism (the trailing 'True' this
--   returns, exactly like a migrated B1 session, tells the caller no
--   live Lua components were decoded). A NON-EMPTY blob cannot be
--   honestly migrated -- the pre-#761 Lua deserializer that could
--   interpret it was removed -- and is refused rather than silently
--   discarded.
decodeB2SessionEnvelope
    ∷ BS.ByteString → LegacyDecodeResult (SaveMetadata, SessionSnapshot)
decodeB2SessionEnvelope bytes =
    case decodeB2StructureAndMetadata bytes of
        Nothing → NotLegacyShaped
        Just (Left e) → LegacyShapedButFailed e
        Just (Right (decoded, meta)) →
            either LegacyShapedButFailed LegacyDecoded $ do
                snap ← either (Left . renderComponentErrors) Right
                              (assembleSnapshot meta decoded)
                luaStatePayload ←
                    maybe (Left "lua-state component payload missing") Right
                          (HM.lookup luaStateComponentId (dePayloads decoded))
                when (not (BS.null luaStatePayload)) $
                    Left "legacy save carries a non-empty pre-#761 opaque \
                         \\"lua-state\" blob that this build can no longer \
                         \interpret (the pre-#761 Lua deserializer was \
                         \removed) -- refusing to migrate rather than \
                         \silently discard persisted Lua state"
                pure (meta, snap)

-- | Try the B1 legacy fallback, then the B2 one, in that order --
--   shared by every decode entry point below so a fallback chain can
--   never drift out of sync between them.
tryLegacyEnvelopeFallbacks
    ∷ BS.ByteString → LegacyDecodeResult (SaveMetadata, SessionSnapshot)
tryLegacyEnvelopeFallbacks bytes = case decodeLegacySessionEnvelope bytes of
    NotLegacyShaped → decodeB2SessionEnvelope bytes
    other           → other

tryLegacyMetadataFallbacks
    ∷ BS.ByteString → LegacyDecodeResult SaveMetadata
tryLegacyMetadataFallbacks bytes = case decodeLegacySessionMetadata bytes of
    NotLegacyShaped → decodeB2SessionMetadata bytes
    other           → other

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

-- | Requirement 9 (issue #766, save-overhaul C4): every component id
--   present in a structurally-valid envelope that this reader does not
--   recognize — always OPTIONAL ones, since an unknown REQUIRED
--   component already fails 'decodeEnvelope' itself
--   ('UnknownRequiredComponent'), which this function reports as "no
--   foreign data" (an empty list) rather than attempt to peek past a
--   structurally-rejected envelope. @luaKnownNames@ widens the known set
--   the SAME way every other decode entry point does.
--
--   The legacy @"session"@ id is included in the set passed to
--   'decodeEnvelope' itself so a genuine B1-shaped generation's OWN
--   REQUIRED session/metadata pair doesn't itself trip
--   'UnknownRequiredComponent' and refuse EVERY legacy session's first
--   re-save (the scenario this function exists for). But @"session"@ is
--   only actually EXEMPTED from the returned foreign-id list when the
--   decoded envelope's OTHER present ids contain no genuine MODERN
--   gameplay component (round-4 review) — @"session"@ may legitimately
--   appear alongside @"metadata"@ and/or genuinely-unrecognized optional
--   data (that's exactly the B1-plus-extra-optional-data scenario this
--   requirement protects), but if it appears alongside an ACTUAL modern
--   component (e.g. @"world-pages"@), this is NOT the recognized legacy
--   shape at all — a foreign/hand-crafted file could still construct
--   one even though this build's own writer never would — so it must be
--   treated as ordinary unrecognized data like any other id, not
--   silently exempted just because it happens to share that name.
--   Otherwise a genuinely unknown optional component that merely
--   happens to be NAMED @"session"@ inside a modern-shaped save would
--   pass this guard and be silently dropped on the very next save.
--
--   Round-7 review: presence of the id alone is not enough either — a
--   @"session"@ descriptor marked OPTIONAL (or at the wrong version)
--   is NOT the real, always-required frozen B1 shape ('decodeEnvelope'
--   never checks a PRESENT descriptor's own @cdRequired@ flag against
--   what a genuine writer would mark, only whether an id claiming
--   @cdRequired@ is unknown, or a reader-required id is altogether
--   missing) — so this also cross-checks the descriptor's own required
--   flag and version, mirroring 'decodeLegacyStructureAndMetadata'
--   exactly, so the two functions can never disagree about what counts
--   as "genuinely B1-shaped".
--
--   Round-7 review: the SAME reasoning applies to the #760-era ("B2")
--   shape 'decodeB2SessionEnvelope' recognizes — its own opaque
--   @"lua-state"@ id must ALSO be exempted here (with the SAME
--   exact-shape/required-flag precision), or a freshly-migrated #760
--   session's very first re-save would refuse, believing "lua-state"
--   is foreign data about to be lost, even though the migration already
--   proved it can be honestly handled (or refused the load outright, in
--   which case this function is never reached for that generation).
--
--   Round-8 review: the required-flag check alone still isn't the full
--   "genuinely #760-shaped" precision — @decodeB2StructureAndMetadata@
--   now also requires @"lua-state"@'s OWN descriptor version to match
--   'luaStateComponentVersion' exactly (not merely a version this reader
--   happens to already know how to decode, since the opaque blob is
--   never actually interpreted, only checked for emptiness). This guard
--   applies the identical version check, so a hand-crafted or genuinely
--   future envelope claiming some OTHER lua-state schema version can
--   never be exempted here even though the migration itself already
--   refuses to decode it — the two must never disagree about what counts
--   as "genuinely B2-shaped", exactly as B1's pairing already ensures.
--
--   Round-9 review: rounds 7-8 each mirrored only ONE descriptor's
--   precision into this guard (session's for B1, lua-state's for B2),
--   still leaving OTHER descriptors in the same shape unchecked — a
--   @{metadata OPTIONAL, session required v90}@ envelope satisfied
--   @looksLikeB1Shape@ even though 'decodeLegacyStructureAndMetadata'
--   itself would refuse it (metadata's own required flag matters too),
--   and a B2 envelope with any non-"lua-state" Haskell descriptor
--   marked OPTIONAL satisfied @looksLikeB2Shape@ even though
--   'decodeB2StructureAndMetadata' checks EVERY id in @b2Ids@ for
--   'cdRequired', not just "lua-state". This guard now calls the SAME
--   shared 'descriptorIsExactlyRequiredAt'/'descriptorIsRequired'
--   helpers the decoders themselves use, over every relevant id, so a
--   future precision fix to either shape's recognition can never again
--   land in only one of the two places.
foreignOptionalComponentIds ∷ HS.HashSet Text → BS.ByteString → [ComponentId]
foreignOptionalComponentIds luaKnownNames bytes =
    case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
             knownIdsForDecode HS.empty bytes of
        Left _        → []
        Right decoded →
            let presentIds = presentComponentIds decoded

                otherThanSession = HS.delete sessionComponentId presentIds
                hasModernComponentBesidesSession =
                    any (\i → i ≢ metadataComponentId ∧ HS.member i modernKnownIds)
                        (HS.toList otherThanSession)
                looksLikeB1Shape =
                    HS.member sessionComponentId presentIds
                        ∧ not hasModernComponentBesidesSession
                        ∧ descriptorIsExactlyRequiredAt decoded
                              metadataComponentId metadataComponentVersion
                        ∧ descriptorIsExactlyRequiredAt decoded
                              sessionComponentId sessionComponentVersion

                looksLikeB2Shape =
                    presentIds ≡ b2Ids
                        ∧ all (descriptorIsRequired decoded) (HS.toList b2Ids)
                        ∧ descriptorIsExactlyRequiredAt decoded
                              luaStateComponentId luaStateComponentVersion

                effectiveKnownIds
                    | looksLikeB1Shape = knownIdsForDecode
                    | looksLikeB2Shape = HS.insert luaStateComponentId modernKnownIds
                    | otherwise        = modernKnownIds
            in [ cdId d | d ← emComponents (deManifest decoded)
               , not (HS.member (cdId d) effectiveKnownIds) ]
  where
    modernKnownIds    = knownComponentIds luaKnownNames
    knownIdsForDecode =
        HS.insert sessionComponentId (HS.insert luaStateComponentId modernKnownIds)

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
decodeSaveEnvelopeMetadataClassified luaKnownNames bytes =
    case decodeModern of
        Right meta     → Right meta
        Left modernErr → case tryLegacyMetadataFallbacks bytes of
            NotLegacyShaped         → Left modernErr
            LegacyShapedButFailed e → Left (GenerationIncompatible e)
            LegacyDecoded meta      → Right meta
  where
    decodeModern = do
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
    → Either GenerationFailure
             (SaveMetadata, SessionSnapshot, [LuaComponentSpec], Bool)
decodeSessionEnvelopeClassified luaKnownNames luaRequiredNames bytes =
    case decodeModern of
        Right result   → Right result
        Left modernErr → case tryLegacyEnvelopeFallbacks bytes of
            NotLegacyShaped            → Left modernErr
            LegacyShapedButFailed e    → Left (GenerationIncompatible e)
            LegacyDecoded (meta, snap) → Right (meta, snap, [], True)
  where
    decodeModern = do
        decoded ← decodeClassifiedEnvelope luaKnownNames luaRequiredNames bytes
        meta ← either (Left . GenerationIncompatible) Right
                      (decodeMetadataComponent decoded)
        snap ← either (Left . GenerationIncompatible . renderComponentErrors)
                      Right (assembleSnapshot meta decoded)
        pure (meta, snap, extractLuaComponents decoded, False)

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
