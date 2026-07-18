{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | The component-persistence machinery (issue #760, save-overhaul B2):
--   the per-component codec contract, the type-erased registry entry,
--   the structured error every decode/migrate/validate phase reports,
--   and the stable component identifiers.
--
--   This module is deliberately CONTENT-FREE — it knows the SHAPE of a
--   component (id, version, required/optional, dependencies, an
--   encoder, a version-dispatched decoder, a validator) but nothing
--   about any specific gameplay slice. The concrete components live in
--   "World.Save.Component.Session"/".Page"/".Entities"; the authoritative
--   registry + cross-component assembly is "World.Save.Component". Both
--   import THIS module, so this one must not import them (no cycle).
--
--   Every component's on-disk bytes are the frozen wire contract
--   (requirement 4): a 'ComponentCodec's DTO is encoded positionally by
--   cereal and, once shipped at a given version, is never edited in
--   place — a new schema goes in a NEW version, decoded by adding that
--   version to 'ccInputVers' and dispatching on it in 'ccDecode'. The
--   canonical type @a@ a codec decodes INTO is the migration target
--   ("World.Save.Snapshot"'s slices), kept separate from the versioned
--   DTO so ordinary snapshot evolution never touches saved bytes.
--
--   === Frozen-DTO boundary rule (governs every component, present or future)
--
--   A component's DTO must never embed a LIVE gameplay record directly,
--   because adding / dropping / reordering a field on that live record
--   would silently change a shipped component's bytes. For any type
--   reachable from a component's DTO, decide FREEZE vs. LEAF as follows —
--   and recurse the decision into a frozen type's own fields, so the
--   boundary is transitive (a shallow wrapper that re-embeds a live nested
--   type does NOT satisfy this rule):
--
--   FREEZE it (mirror it with a component-owned DTO + an explicit
--   field-by-field @to…@/@from…@ conversion) iff EITHER
--
--     (1) it has its own live-manager identity — it is mutated in place
--         elsewhere in the engine as gameplay-runtime state, imported from
--         a non-Save module whose purpose is holding live mutable state; OR
--     (2) it could plausibly gain / lose / reorder fields for reasons
--         unrelated to save compatibility.
--
--   REUSE it as a LEAF (no wrapper) iff EITHER
--
--     (a) it is a content-reference id / coordinate / append-only enum
--         with no independent mutable identity — e.g. 'MaterialId',
--         @FloraId@, @ChunkCoord@, @ClimateCoord@, @Pose@, @Direction@,
--         @ZoomMapMode@; OR
--     (b) it is ITSELF a live type that carries its OWN in-source
--         documented positional-save-schema discipline (per-field
--         save-version annotations + an explicit stable-field-order
--         contract), e.g. @GeoTimeline@ — safe by a DIFFERENT,
--         already-established mechanism, not a gap.
--
--   NOTE (review round 6): the "World.Save.Types" positional entity
--   snapshots (@BuildingInstanceSnapshot@ / @UnitInstanceSnapshot@) do
--   NOT qualify as leaves, even though @currentSaveVersion@ governs them
--   in the legacy bridge. They directly carry mutable @ItemInstance@
--   values (and, on units, live @StatModifier@ / @Wound@ / @Scar@
--   records), so a v1 @"buildings"@/@"units"@ component payload could
--   drift from an unrelated change to any of those WITHOUT the
--   component's own version dispatch noticing — riding on the older
--   global-version mechanism is not good enough for the new component
--   contract. They are therefore FROZEN
--   ('World.Save.Component.Entities.BuildingInstanceDTO' /
--   'UnitInstanceDTO'), like every other reachable live record.
--
--   Do not gold-plate: stop at leaves. The concrete components apply this
--   rule in "World.Save.Component.WorldGen" (the worldgen-params tree),
--   "World.Save.Component.Page" (world edits / designations / ground
--   items), and "World.Save.Component.Entities" (unit-sim / craft-bills /
--   power-nodes, plus the frozen building/unit instance DTOs and their
--   nested item / stat-modifier / wound / scar records — review round 6).
module World.Save.Component.Types
    ( ComponentPhase(..)
    , ComponentError(..)
    , renderComponentError
    , ComponentCodec(..)
    , RegisteredComponent(..)
    , registerComponent
    , serializeCodec
    , decodeComponentValue
    , findDescriptor
    , applyPageSlices
      -- * Stable component identifiers
    , metadataComponentId
    , coreSessionComponentId
    , texPaletteComponentId
    , luaStateComponentId
    , worldPagesComponentId
    , worldEditsComponentId
    , worldActivityComponentId
    , buildingsComponentId
    , unitsComponentId
    , unitSimComponentId
    , craftBillsComponentId
    , powerNodesComponentId
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import qualified Data.Text as T
import World.Save.Envelope.Types
    (ComponentId(..), ComponentDescriptor(..), EnvelopeManifest(..))
import World.Save.Envelope.Codec (DecodedEnvelope(..))
import qualified Data.HashSet as HS
import qualified Data.List as L
import World.Page.Types (WorldPageId)
import World.Save.Snapshot (SessionSnapshot, PageSnapshot)

-- | Which phase of turning bytes into a validated snapshot slice failed
--   (requirement 6: an error names the failing phase, not just that
--   something broke).
data ComponentPhase
    = DecodePhase    -- ^ cereal decode, or an unsupported encoded version
    | MigratePhase   -- ^ turning a decoded DTO into its canonical shape
    | ValidatePhase  -- ^ a component-local invariant
    | AssemblePhase  -- ^ a cross-component / whole-session invariant
    deriving (Show, Eq)

-- | One failure, naming the component, the encoded version being
--   handled, the phase, and a human message (requirement 6).
data ComponentError = ComponentError
    { ceComponent ∷ !ComponentId
    , ceVersion   ∷ !Word32
    , cePhase     ∷ !ComponentPhase
    , ceMessage   ∷ !Text
    } deriving (Show, Eq)

renderComponentError ∷ ComponentError → Text
renderComponentError e =
    "[" <> cidText (ceComponent e) <> " v" <> T.pack (show (ceVersion e))
        <> " " <> T.pack (show (cePhase e)) <> "] " <> ceMessage e
  where cidText (ComponentId t) = t

-- | Everything one component needs (requirement 3): a stable id, a
--   current schema version, the versions it can DECODE, its
--   required/optional status, its declared dependencies, an encoder
--   from the session snapshot, a version-dispatched decoder into the
--   canonical type @a@, and a component-local validator over that
--   canonical value.
data ComponentCodec a = ComponentCodec
    { ccId        ∷ !ComponentId
    , ccVersion   ∷ !Word32
    , ccInputVers ∷ ![Word32]
      -- ^ Encoded versions this reader accepts. For B2 (the first split)
      --   this is just @[ccVersion]@; a future migration adds an older
      --   version and dispatches on it in 'ccDecode' (requirement 3).
    , ccRequired  ∷ !Bool
    , ccDeps      ∷ ![ComponentId]
    , ccEncode    ∷ SessionSnapshot → BS.ByteString
    , ccDecode    ∷ Word32 → BS.ByteString → Either ComponentError a
    , ccValidate  ∷ a → [ComponentError]
    }

-- | The type-erased registry entry. Erasing @a@ lets one heterogeneous
--   list drive every uniform pass — encode from a snapshot, decode +
--   self-validate each present component ('rcDecodeErrors'), AND fold each
--   component's decoded contribution back onto the in-progress snapshot
--   during assembly ('rcApply'). Because 'rcApply' is a MANDATORY field,
--   a component cannot be added to 'World.Save.Component.saveComponentRegistry'
--   without also providing its assembly step: a registered-but-unassembled
--   component (written to disk + required by the reader, yet silently
--   ignored on load) is therefore structurally impossible. Every field is
--   built from the same concrete 'ComponentCodec', so a component's
--   encode / decode / validate / assemble contributions can never drift
--   apart (they share the exact same 'ccDecode'/'ccValidate').
data RegisteredComponent = RegisteredComponent
    { rcId           ∷ !ComponentId
    , rcVersion      ∷ !Word32
    , rcInputVers    ∷ ![Word32]
    , rcRequired     ∷ !Bool
    , rcDeps         ∷ ![ComponentId]
    , rcEncode       ∷ SessionSnapshot → BS.ByteString
    , rcDecodeErrors ∷ DecodedEnvelope → [ComponentError]
      -- ^ Decode + component-local-validate this component from a
      --   structurally-valid envelope, returning EVERY failure (empty ⇒
      --   the component is well-formed). Drives assembly's all-or-nothing
      --   decode pass without needing the concrete decoded value.
    , rcApply        ∷ DecodedEnvelope → SessionSnapshot
                       → Either [ComponentError] SessionSnapshot
      -- ^ Decode this component (again, from the already-validated
      --   envelope) and FOLD its contribution onto the in-progress
      --   snapshot. This is what makes the registry authoritative for
      --   assembly (requirement 6): 'World.Save.Component.assembleSnapshot'
      --   iterates the registry calling this, rather than hard-coding a
      --   separate, drift-prone apply sequence.
    }

-- | Register a component: build every 'RegisteredComponent' field from its
--   concrete 'ComponentCodec' plus its assembly fold. The fold receives
--   the component's ACTUAL encoded version, its decoded+validated value,
--   and the in-progress snapshot, and returns the snapshot with this
--   component's contribution folded in (or the assembly errors it found —
--   e.g. a page-set mismatch). Requiring the fold here is the structural
--   guarantee that registration and assembly cannot diverge.
registerComponent
    ∷ ComponentCodec a
    → (Word32 → a → SessionSnapshot → Either [ComponentError] SessionSnapshot)
    → RegisteredComponent
registerComponent cc fold = RegisteredComponent
    { rcId           = ccId cc
    , rcVersion      = ccVersion cc
    , rcInputVers    = ccInputVers cc
    , rcRequired     = ccRequired cc
    , rcDeps         = ccDeps cc
    , rcEncode       = ccEncode cc
    , rcDecodeErrors = \de → either id (const []) (decodeComponentValue cc de)
    , rcApply        = \de snap → do
        a ← decodeComponentValue cc de
        fold (encodedVersionOf cc de) a snap
    }

-- | The component's ACTUAL encoded version, read from the decoded
--   manifest descriptor (present whenever the component decoded), falling
--   back to the codec's current version if somehow absent.
encodedVersionOf ∷ ComponentCodec a → DecodedEnvelope → Word32
encodedVersionOf cc de =
    maybe (ccVersion cc) cdVersion (findDescriptor (ccId cc) (deManifest de))

-- | Build a codec whose CURRENT version cereal-decodes straight into a
--   versioned DTO @d@, then migrates that DTO into the canonical type
--   @a@ (for B2 the migration is usually the identity, @d ≡ a@ — but
--   the seam exists so a future version can slot in). Handles the two
--   universal decode failures uniformly: an unsupported encoded version
--   (naming it), and a truncated/malformed payload (cereal's own
--   error), both as 'DecodePhase' errors.
serializeCodec
    ∷ S.Serialize d
    ⇒ ComponentId → Word32 → Bool → [ComponentId]
    → (SessionSnapshot → d)
    → (Word32 → d → Either ComponentError a)
    → (a → [ComponentError])
    → ComponentCodec a
serializeCodec cid ver req deps toDTO migrate validate = ComponentCodec
    { ccId        = cid
    , ccVersion   = ver
    , ccInputVers = [ver]
    , ccRequired  = req
    , ccDeps      = deps
    , ccEncode    = \snap → S.encode (toDTO snap)
    , ccDecode    = \v bytes →
        if v ∉ [ver]
          then Left (ComponentError cid v DecodePhase
                       ("unsupported schema version (reader supports v"
                        <> T.pack (show ver) <> ")"))
          else case S.decode bytes of
                 Left err → Left (ComponentError cid v DecodePhase
                                    ("malformed payload: " <> T.pack err))
                 Right d  → migrate v d
    , ccValidate  = validate
    }
  where x ∉ xs = not (x ∈ xs)
        x ∈ xs = x `elem` xs

-- | Pull one component's typed, self-validated value out of an
--   already-structurally-valid 'DecodedEnvelope' (assembly path). The
--   "missing" cases are unreachable for a required component — the
--   envelope codec already refused any envelope lacking it — but are
--   reported (not partial) rather than crashing, so an OPTIONAL absentee
--   or a direct misuse fails cleanly.
decodeComponentValue
    ∷ ComponentCodec a → DecodedEnvelope → Either [ComponentError] a
decodeComponentValue cc de =
    case findDescriptor (ccId cc) (deManifest de) of
        Nothing → Left [ComponentError (ccId cc) 0 AssemblePhase
                          "component descriptor missing"]
        Just desc →
            let ver = cdVersion desc in
            case HM.lookup (ccId cc) (dePayloads de) of
                Nothing → Left [ComponentError (ccId cc) ver AssemblePhase
                                  "component payload missing"]
                Just payload → case ccDecode cc ver payload of
                    Left e  → Left [e]
                    Right a → case ccValidate cc a of
                        [] → Right a
                        es → Left es

findDescriptor ∷ ComponentId → EnvelopeManifest → Maybe ComponentDescriptor
findDescriptor cid manifest =
    listToMaybe [ d | d ← emComponents manifest, cdId d ≡ cid ]

-- | Apply one page-scoped component's slices onto the base page map
--   (assembly). Enforces requirement 8's page-scoping contract: the
--   slice set's page ids must EXACTLY match the authoritative page set
--   (@base@'s keys, established by @"world-pages"@) — a slice for an
--   unknown page, or a page with no slice, fails as an 'AssemblePhase'
--   error naming @cid@. On success each page's snapshot is updated
--   through @writeSlice@.
--
--   @ver@ is the component's ACTUAL encoded version (the descriptor's
--   'cdVersion', threaded in by the caller) so a page-set-mismatch error
--   reports the true version, not a placeholder (requirement 6).
applyPageSlices
    ∷ ComponentId
    → Word32
    → (slice → WorldPageId)
    → (slice → PageSnapshot → PageSnapshot)
    → [slice]
    → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyPageSlices cid ver pageIdOf writeSlice slices base =
    let sliceIds = HS.fromList (map pageIdOf slices)
        baseIds  = HM.keysSet base
        extra    = HS.toList (HS.difference sliceIds baseIds)
        missing  = HS.toList (HS.difference baseIds sliceIds)
        dupes    = [ pid | (pid, n) ← HM.toList
                              (HM.fromListWith (+)
                                 [ (pageIdOf s, 1 ∷ Int) | s ← slices ])
                         , n > 1 ]
        errs = concat
            [ [ mkErr ("slice for unknown page " <> showPid p) | p ← extra ]
            , [ mkErr ("no slice for page " <> showPid p) | p ← missing ]
            , [ mkErr ("duplicate slice for page " <> showPid p) | p ← dupes ]
            ]
    in if not (null errs)
         then Left errs
         else Right (L.foldl' (\m s → HM.adjust (writeSlice s) (pageIdOf s) m)
                              base slices)
  where
    mkErr = ComponentError cid ver AssemblePhase
    showPid p = T.pack (show p)

-- Stable component identifiers -------------------------------------

metadataComponentId    ∷ ComponentId
metadataComponentId    = ComponentId "metadata"
coreSessionComponentId ∷ ComponentId
coreSessionComponentId = ComponentId "core-session"
texPaletteComponentId  ∷ ComponentId
texPaletteComponentId  = ComponentId "texture-palette"
luaStateComponentId    ∷ ComponentId
luaStateComponentId    = ComponentId "lua-state"
worldPagesComponentId  ∷ ComponentId
worldPagesComponentId  = ComponentId "world-pages"
worldEditsComponentId  ∷ ComponentId
worldEditsComponentId  = ComponentId "world-edits"
worldActivityComponentId ∷ ComponentId
worldActivityComponentId = ComponentId "world-activity"
buildingsComponentId   ∷ ComponentId
buildingsComponentId   = ComponentId "buildings"
unitsComponentId       ∷ ComponentId
unitsComponentId       = ComponentId "units"
unitSimComponentId     ∷ ComponentId
unitSimComponentId     = ComponentId "unit-sim"
craftBillsComponentId  ∷ ComponentId
craftBillsComponentId  = ComponentId "craft-bills"
powerNodesComponentId  ∷ ComponentId
powerNodesComponentId  = ComponentId "power-nodes"
