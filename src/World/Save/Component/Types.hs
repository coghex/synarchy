{-# LANGUAGE Strict #-}
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
--   place — a new schema goes in a NEW version: the old DTO type is
--   frozen and moved to 'csOlderVersions' via 'atVersion', and the new
--   one becomes 'csVersion'/'csEncode'/'csDecode' (issue #1093). The
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
--   NOTE: the "World.Save.Types" positional entity
--   snapshots (@BuildingInstanceSnapshot@ / @UnitInstanceSnapshot@) do
--   NOT qualify as leaves, even though @currentSaveVersion@ governs them
--   in the legacy bridge. They directly carry mutable @ItemInstance@
--   values (and, on units, live @StatModifier@ / @Wound@ / @Scar@
--   records), so a v1 @"buildings"@/@"units"@ component payload could
--   drift from an unrelated change to any of those WITHOUT the
--   component's own version dispatch noticing — riding on the older
--   global-version mechanism is not good enough for the new component
--   contract. They are therefore FROZEN
--   ('World.Save.Component.EntitySnapshots.BuildingInstanceDTO' /
--   'UnitInstanceDTO'), like every other reachable live record.
--
--   Do not gold-plate: stop at leaves. The concrete components apply this
--   rule in "World.Save.Component.WorldGen" (the worldgen-params tree),
--   the three page-scoped owners behind "World.Save.Component.Page"
--   (world edits / designations / ground items), and
--   "World.Save.Component.Entities" (unit-sim / craft-bills /
--   power-nodes, plus the frozen building/unit instance DTOs and their
--   nested item / stat-modifier / wound / scar records).
--
--   "World.Save.Component.WorldGen" is a FAÇADE, not a declaration site
--   (#2098). It declares nothing and re-exports one worldgen DTO graph
--   from four owner modules, each holding a family that evolves on its
--   own schedule, in dependency order:
--   "World.Save.Component.WorldGenClimate" (the generation and climate
--   leaves), "World.Save.Component.WorldGenNaming" (generated names,
--   etymology, bounds, encounters, and every location and river-name
--   shape, current and historical),
--   "World.Save.Component.WorldGenCurrent" (the currently written
--   'WorldGenParamsDTO') and "World.Save.Component.WorldGenHistory"
--   (every decode-only @WorldGenParamsDTOv1@ … @WorldGenParamsDTOv6@).
--   Downstream modules import the façade; the owners exist so a wire
--   fact has one home, not so consumers have to choose one.
--
--   "World.Save.Component.Entities" is the same shape (#2150): a
--   FAÇADE that declares nothing and re-exports the five entity
--   components from three sibling owners —
--   "World.Save.Component.EntitySnapshots" (@"buildings"@ and
--   @"units"@: the frozen instance DTOs and the stat-modifier / wound
--   / scar records they nest), "World.Save.Component.EntitySimulation"
--   (@"unit-sim"@: three live versions of the per-unit simulation
--   state) and "World.Save.Component.EntitySystems" (@"craft-bills"@
--   and @"power-nodes"@: the two page-scoped registries and their
--   allocator validators).
--
--   These paragraphs name the owners; the RULE they implement is still
--   stated here and nowhere else.
module World.Save.Component.Types
    ( ComponentPhase(..)
    , ComponentError(..)
    , renderComponentError
    , ComponentCodec(..)
    , ComponentFold
    , RegisteredComponent(..)
    , registerComponent
    , ComponentSpec(..)
    , ComponentVersion(..)
    , atVersion
    , olderVersionTableError
    , componentCodec
    , findDescriptor
    , applyPageSlices
      -- * Stable component identifiers
    , metadataComponentId
    , coreSessionComponentId
    , texPaletteComponentId
    , luaComponentPrefix
    , worldPagesComponentId
    , worldEditsComponentId
    , worldActivityComponentId
    , buildingsComponentId
    , unitsComponentId
    , unitSimComponentId
    , craftBillsComponentId
    , powerNodesComponentId
    , containerKnowledgeComponentId
    , transferOrdersComponentId
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
    "[" <> cidText (ceComponent e) <> " v" <> tshow (ceVersion e)
        <> " " <> tshow (cePhase e) <> "] " <> ceMessage e
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
      -- ^ Encoded versions this reader accepts, ascending — the
      --   registry/audit-visible projection of what 'ccDecode' actually
      --   dispatches on. Built through 'componentCodec' both come from
      --   the SAME version declarations ('csVersion' + 'csOlderVersions'),
      --   so this list cannot advertise a version the decoder would
      --   reject, nor omit one it accepts (issue #1093).
    , ccRequired  ∷ !Bool
    , ccDeps      ∷ ![ComponentId]
    , ccEncode    ∷ SessionSnapshot → BS.ByteString
    , ccDecode    ∷ Word32 → BS.ByteString → Either ComponentError a
    , ccValidate  ∷ a → [ComponentError]
    }

-- | What one prepared component contributes to assembly: a fold from
--   the in-progress snapshot to the snapshot with this component's
--   ALREADY-DECODED contribution written in (or the assembly errors it
--   found — e.g. a page-set mismatch).
--
--   The decoded value is captured in this closure rather than re-derived,
--   which is what makes 'rcPrepare' the single decode of a component's
--   payload (issue #1919).
type ComponentFold =
    SessionSnapshot → Either [ComponentError] SessionSnapshot

-- | The type-erased registry entry. Erasing @a@ lets one heterogeneous
--   list drive every uniform pass — encode from a snapshot, and decode +
--   self-validate + prepare each present component's assembly fold
--   ('rcPrepare'). Because 'rcPrepare' is a MANDATORY field built from a
--   MANDATORY fold argument to 'registerComponent', a component cannot be
--   added to 'World.Save.Component.saveComponentRegistry' without also
--   providing its assembly step: a registered-but-unassembled component
--   (written to disk + required by the reader, yet silently ignored on
--   load) is therefore structurally impossible. Every field is built from
--   the same concrete 'ComponentCodec', so a component's encode / decode /
--   validate / assemble contributions can never drift apart (they share
--   the exact same 'ccDecode'/'ccValidate').
data RegisteredComponent = RegisteredComponent
    { rcId           ∷ !ComponentId
    , rcVersion      ∷ !Word32
    , rcInputVers    ∷ ![Word32]
    , rcRequired     ∷ !Bool
    , rcDeps         ∷ ![ComponentId]
    , rcEncode       ∷ SessionSnapshot → BS.ByteString
    , rcPrepare      ∷ DecodedEnvelope
                       → Either [ComponentError] ComponentFold
      -- ^ Decode + component-local-validate this component from a
      --   structurally-valid envelope EXACTLY ONCE, returning either
      --   EVERY failure it found or the 'ComponentFold' that writes the
      --   value just decoded onto the in-progress snapshot (issue #1919).
      --
      --   This one field replaced a @rcDecodeErrors@/@rcApply@ pair that
      --   each ran their own 'decodeComponentValue', so a load in which
      --   every component decoded successfully paid for a second full
      --   cereal decode of every registered payload. The all-or-nothing
      --   contract that pair existed to serve is unchanged and needs no
      --   second decode: 'World.Save.Component.assembleSnapshot' calls
      --   this once per registered component, collects every 'Left'
      --   before folding anything, and only then runs the 'Right' folds
      --   in dependency order. The registry stays type-erased — @a@
      --   never escapes this closure — which is what still lets ONE
      --   heterogeneous list drive every uniform pass.
    }

-- | Register a component: build every 'RegisteredComponent' field from its
--   concrete 'ComponentCodec' plus its assembly fold. The fold receives
--   the component's ACTUAL encoded version, its decoded+validated value,
--   and the in-progress snapshot, and returns the snapshot with this
--   component's contribution folded in (or the assembly errors it found —
--   e.g. a page-set mismatch). Requiring the fold here is the structural
--   guarantee that registration and assembly cannot diverge.
--
--   An OPTIONAL component (@ccRequired = False@, first used by #1087's
--   @"container-knowledge"@ and joined by #1246's @"transfer-orders"@)
--   that is entirely ABSENT from the envelope
--   contributes nothing and reports nothing: it prepares to the identity
--   fold WITHOUT decoding anything at all, leaving whatever default the
--   foundational components already installed. That is decided HERE,
--   once, from 'ccRequired' — not per component — so "optional" can
--   never mean two different things in two places. Note the distinction the
--   'componentAbsent' guard draws: absence is checked at the MANIFEST
--   level, so a component that IS declared but whose payload is
--   malformed, truncated, or encoded at an unsupported version still
--   fails exactly as a required one would.
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
    , rcPrepare      = \de →
        if optionalAndAbsent de then Right (\snap → Right snap)
        else do
            a ← decodeComponentValue cc de
            Right (fold (encodedVersionOf cc de) a)
    }
  where
    optionalAndAbsent de =
        not (ccRequired cc) ∧ componentAbsent (ccId cc) de

-- | Is this component entirely absent from the decoded envelope? Both
--   halves must be missing — a descriptor with no payload (or vice
--   versa) is a malformed envelope, not an absent optional component,
--   and must keep reporting as the failure it is.
componentAbsent ∷ ComponentId → DecodedEnvelope → Bool
componentAbsent cid de =
    isNothing (findDescriptor cid (deManifest de))
      ∧ not (HM.member cid (dePayloads de))

-- | The component's ACTUAL encoded version, read from the decoded
--   manifest descriptor (present whenever the component decoded), falling
--   back to the codec's current version if somehow absent.
encodedVersionOf ∷ ComponentCodec a → DecodedEnvelope → Word32
encodedVersionOf cc de =
    maybe (ccVersion cc) cdVersion (findDescriptor (ccId cc) (deManifest de))

-- | ONE accepted encoded version of a component: the version number,
--   plus how bytes encoded at it become the canonical type @a@.
--
--   Built only by 'atVersion', which closes over the frozen DTO type
--   THAT version's bytes cereal-decode through. That closure is what
--   lets a single declaration list carry a DIFFERENT frozen DTO per
--   version — the thing widening 'ccInputVers' alone could never
--   express, since one @Serialize d@ constraint would decode an old
--   payload as the current DTO. The version number stays inspectable, so
--   'componentCodec' derives 'ccInputVers', the decode dispatch, AND the
--   unsupported-version message from these same declarations rather than
--   from a separately-maintained list that could drift out of step
--   (issue #1093).
data ComponentVersion a = ComponentVersion
    { cvVersion ∷ !Word32
    , cvDecode  ∷ ComponentId → BS.ByteString → Either ComponentError a
      -- ^ The component id is supplied by 'componentCodec' (from
      --   'csComponent') rather than restated per version, so a
      --   malformed-payload error can never name the wrong component.
    }

-- | Declare that encoded version @ver@ decodes through the frozen DTO
--   @d@, which @build@ turns into the canonical type @a@. Every real
--   migration in this codebase is total (it re-shapes a decoded DTO —
--   e.g. wrapping bare ids as typed references), so this seam is total
--   too: the previous helper's @Word32 → d → Either ComponentError a@
--   was passed @(\\_ d → Right d)@ by every single call site, and a
--   second dead seam is worth less than the one it replaces. A decode
--   that can genuinely fail on well-formed bytes belongs in 'ccValidate'
--   (a 'ValidatePhase' error naming what was wrong), and a truly
--   fallible migration would add its own variant HERE when one first
--   exists.
atVersion ∷ S.Serialize d ⇒ Word32 → (d → a) → ComponentVersion a
atVersion ver build = ComponentVersion
    { cvVersion = ver
    , cvDecode  = \cid bytes → case S.decode bytes of
        Left err → Left (ComponentError cid ver DecodePhase
                           ("malformed payload: " <> T.pack err))
        Right d  → Right (build d)
    }

-- | Everything 'componentCodec' needs, as NAMED fields: a call site
--   states which number is the schema version and which flag is
--   required/optional instead of relying on positional order
--   (issue #1093).
--
--   @d@ is the CURRENT version's frozen DTO — the one thing encoding and
--   current-version decoding must agree on, so it is named once here and
--   shared by 'csEncode'/'csDecode'. Older accepted versions each bring
--   their OWN frozen DTO through 'csOlderVersions'.
data ComponentSpec d a = ComponentSpec
    { csComponent     ∷ !ComponentId
    , csVersion       ∷ !Word32
      -- ^ The current schema version: what 'csEncode' writes, and the
      --   newest version this reader accepts. Declared ONCE — 'ccVersion',
      --   this reader's current-version dispatch, and its entry in
      --   'ccInputVers' all come from here.
    , csRequired      ∷ !Bool
      -- ^ @True@ ⇒ a save lacking this component is a failure; @False@ ⇒
      --   an absent payload is a legitimate default (see
      --   'registerComponent').
    , csDeps          ∷ ![ComponentId]
    , csEncode        ∷ SessionSnapshot → d
      -- ^ Snapshot → the CURRENT version's frozen DTO. 'componentCodec'
      --   adds the cereal encode, so no component spells out its own
      --   wire step.
    , csDecode        ∷ d → a
      -- ^ The current version's DTO → the canonical decoded type. Often
      --   'id' (the DTO IS the canonical type).
    , csOlderVersions ∷ ![ComponentVersion a]
      -- ^ Every OLDER encoded version this reader still accepts, each
      --   built by 'atVersion' with its own frozen DTO type. Empty for a
      --   component that has never evolved — the degenerate case of the
      --   same mechanism, not a different one.
      --
      --   Every entry must be STRICTLY OLDER than 'csVersion' and appear
      --   AT MOST ONCE. That is not a convention: 'componentCodec'
      --   ENFORCES it (issue #1275) and is the authoritative boundary
      --   for the rule — see 'olderVersionTableError'.
    , csValidate      ∷ a → [ComponentError]
    }

-- | The FIRST way a declared version table breaks the 'csOlderVersions'
--   contract, in declaration order, or 'Nothing' when the table is
--   well-formed (issue #1275).
--
--   Two independent rules, both structural rather than stylistic,
--   because 'componentCodec' turns the declarations into a SORTED
--   association list dispatched by first-match 'lookup':
--
--     * every entry is STRICTLY OLDER than the current version. An
--       entry EQUAL to it is shadowed by the real current decoder
--       ('L.sortOn' is stable and the current version is prepended), so
--       its frozen DTO is silently never reached; an entry GREATER than
--       it is accepted and advertised as though it were history, which
--       is a reader claiming to understand bytes no writer has ever
--       produced.
--     * no version is declared TWICE — the second decoder for a
--       repeated version is unreachable through 'lookup', so a
--       migration can be replaced by an older one without a single
--       type error.
--
--   Both are invisible at the type level (the version is an ordinary
--   'Word32' argument to 'atVersion'), so nothing but this check stands
--   between a mis-typed declaration and a reader that silently decodes
--   through the wrong frozen DTO.
olderVersionTableError ∷ ComponentId → Word32 → [Word32] → Maybe Text
olderVersionTableError (ComponentId cid) current = go HS.empty
  where
    go _    []       = Nothing
    go seen (v : vs)
        | v ≡ current      = Just (violation v "is the CURRENT version, not an older one")
        | v > current      = Just (violation v "is NEWER than the current version")
        | HS.member v seen = Just (violation v "is declared more than once")
        | otherwise        = go (HS.insert v seen) vs
    violation v what =
        "save component \"" <> cid <> "\": csOlderVersions entry v"
          <> tshow v <> " " <> what
          <> " (csVersion is v" <> tshow current
          <> "). Every entry must be strictly older than csVersion and "
          <> "appear at most once, or the sorted dispatch table silently "
          <> "shadows a decoder (issue #1275)."

-- | Build a component's codec from its 'ComponentSpec', handling the two
--   universal decode failures uniformly so no component hand-writes
--   either: an unsupported encoded version (naming every version this
--   reader DOES accept), and a truncated/malformed payload (cereal's own
--   error), both as 'DecodePhase' errors.
--
--   'ccInputVers', the decode dispatch, and the unsupported-version
--   message are all derived from ONE table — the current version plus
--   'csOlderVersions', sorted ascending — so a reader cannot accept a
--   version it has no decoder for, or report an accepted set that
--   disagrees with what it actually dispatches on.
--
--   This is also the AUTHORITATIVE boundary for the 'csOlderVersions'
--   contract (issue #1275): a table with a duplicate, the current
--   version, or a future version is rejected HERE, naming the component
--   and the offending version, before any 'ComponentCodec' exists. That
--   placement is the point — a malformed table cannot reach a live
--   dispatch table at all, rather than reaching one and being reported
--   afterwards. Everything downstream ('tools/save_compat_audit.py'
--   parsing the same declarations, the registered-codec invariants in
--   the @save components@ hspec group) observes the SAME rule and is
--   documented defense-in-depth, not a second source of truth.
componentCodec ∷ S.Serialize d ⇒ ComponentSpec d a → ComponentCodec a
componentCodec spec =
    case olderVersionTableError cid (csVersion spec) declaredOlder of
      Just problem → error (T.unpack problem)
      Nothing      → ComponentCodec
        { ccId        = cid
        , ccVersion   = csVersion spec
        , ccInputVers = map cvVersion accepted
        , ccRequired  = csRequired spec
        , ccDeps      = csDeps spec
        , ccEncode    = \snap → S.encode (csEncode spec snap)
        , ccDecode    = \v bytes → case lookup v dispatch of
            Just decode → decode cid bytes
            Nothing     → Left (ComponentError cid v DecodePhase
                                  ("unsupported schema version (reader supports "
                                   <> T.intercalate ", " renderedVersions <> ")"))
        , ccValidate  = csValidate spec
        }
  where
    cid           = csComponent spec
    declaredOlder = map cvVersion (csOlderVersions spec)
    accepted = L.sortOn cvVersion
                   (atVersion (csVersion spec) (csDecode spec)
                      : csOlderVersions spec)
    dispatch = [ (cvVersion cv, cvDecode cv) | cv ← accepted ]
    renderedVersions =
        [ "v" <> tshow (cvVersion cv) | cv ← accepted ]

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
    showPid p = tshow p

-- Stable component identifiers -------------------------------------

metadataComponentId    ∷ ComponentId
metadataComponentId    = ComponentId "metadata"
coreSessionComponentId ∷ ComponentId
coreSessionComponentId = ComponentId "core-session"
texPaletteComponentId  ∷ ComponentId
texPaletteComponentId  = ComponentId "texture-palette"

-- | Reserved namespace prefix for every dynamically-registered Lua save
--   component (issue #761, save-overhaul B3): a Lua module registered
--   as @"unit_ai"@ rides in the SAME envelope manifest namespace as
--   every Haskell-owned component (#760) under the id
--   @"lua.unit_ai"@ — disjoint from any Haskell component id by
--   construction (none of them carry a @.@), so a cross-language id
--   collision can only ever be a same-prefix Lua/Lua collision, which
--   'World.Save.Envelope.Codec.encodeEnvelope'/'decodeEnvelope' already
--   reject structurally as a 'DuplicateComponentId'. Lua's own registry
--   ids are the bare, unprefixed name (@scripts/lib/save_modules.lua@'s
--   @saveModules.register(id, ...)@) — only the Haskell-side glue
--   ("Engine.Scripting.Lua.API.Save") ever applies this prefix, so it
--   lives in exactly one place.
luaComponentPrefix ∷ Text
luaComponentPrefix = "lua."

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

-- | #1087: the player's last-known container contents. The FIRST of the
--   two OPTIONAL Haskell-owned gameplay components — see
--   "World.Save.Component.Knowledge"'s header for why a baseline that
--   predates the feature must be allowed to carry no payload at all,
--   and what an absent payload means.
containerKnowledgeComponentId ∷ ComponentId
containerKnowledgeComponentId = ComponentId "container-knowledge"

-- | #1246: the per-page queue of durable transfer orders. The SECOND
--   OPTIONAL Haskell-owned gameplay component — see
--   "World.Save.Component.Transfer"'s header for the justification
--   @docs\/persistence_contract.md@ §5 requires of each one, and note
--   that @lua.tutorial_progress@ is already an optional component too;
--   this pair is the optional set of the STATIC Haskell registry
--   specifically, not of the envelope as a whole.
transferOrdersComponentId ∷ ComponentId
transferOrdersComponentId = ComponentId "transfer-orders"
