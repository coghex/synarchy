{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | The authoritative save-component registry and cross-component
--   assembly (issue #760, save-overhaul B2). This is the ONE place that
--   knows the complete set of Haskell-owned gameplay components, their
--   dependency graph, how to encode them from a 'SessionSnapshot', and
--   how to reassemble a validated 'SessionSnapshot' back out of a decoded
--   envelope — replacing #759 B1's single transitional @"session"@
--   component that wrapped the whole legacy 'SaveData'.
--
--   The registry ('saveComponentRegistry') drives the uniform passes:
--   encoding every component from a snapshot, and the reader's
--   known/required id sets for the envelope codec. 'registryStaticErrors'
--   is the build-time contract on the registry ITSELF — no duplicate
--   ids, every declared dependency names a registered component, and no
--   dependency cycles (requirement 6) — surfaced as an hspec assertion.
--
--   'assembleSnapshot' is the load-time reconstruction: decode +
--   component-local-validate every component WITHOUT touching live state
--   (requirement 6), assemble the full immutable snapshot only once all
--   required components decode, then run the cross-component invariants
--   ("World.Save.Snapshot"'s 'validateSessionSnapshot' — page-set/
--   allocator/orphan checks — plus the manifest-metadata agreement check,
--   requirement 12). Any single failure yields no partial snapshot at all
--   (all-or-nothing), reporting every failure with its component id,
--   encoded version, and phase.
module World.Save.Component
    ( saveComponentRegistry
    , componentKnownIds
    , componentRequiredIds
    , encodeComponentSpecs
    , registryStaticErrors
    , assembleSnapshot
    , dependencyOrder
    , capComponentErrors
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Text as T
import World.Save.Envelope.Types (ComponentId(..))
import World.Save.Envelope.Codec (DecodedEnvelope(..))
import World.Save.Types (SaveMetadata(..))
import World.Save.Snapshot
    ( SessionSnapshot(..), LiveCameraSnapshot(..), PageSnapshot(..)
    , validateSessionSnapshot, structureEditPaletteErrors )
import World.Generate.Types (WorldGenParams(..))
import World.Page.Types (WorldIdentity(..), WorldPageId(..))
import Structure.Palette (emptyTexPalette)
import Engine.Graphics.Camera (CameraFacing(..))
import World.Save.Component.Types
import World.Save.Component.Session
import World.Save.Component.Page
import World.Save.Component.Entities
import World.Save.Integrity
    (IntegrityError(..), sessionIntegrityErrors, integrityErrorCap)

-- | Every Haskell-owned gameplay component, in a stable declaration
--   order. The @"metadata"@ component is NOT here — it is owned by
--   "World.Save.Envelope" (it carries listing metadata, not gameplay
--   state, and must be readable without decoding any of these).
-- | Each entry pairs a component's concrete codec with its ASSEMBLY FOLD
--   (the snapshot contribution). 'registerComponent' makes the fold a
--   mandatory part of registration, so this ONE list is authoritative for
--   BOTH encoding and reassembly — a component here is necessarily applied
--   on load, never merely written-and-ignored (the blocker this closes).
--   The folds:
--
--   - global components ('core-session'/'texture-palette') write the
--     session-wide scalars/camera/palette (Lua-owned state is NOT here
--     at all since #761 — see "World.Save.Component.Types"'s
--     'luaComponentPrefix' haddock);
--   - 'world-pages' installs the base 'PageSnapshot' map every page-scoped
--     component then writes onto;
--   - each page-scoped component overwrites its own slice of every page
--     via its @apply…@ helper, reporting a page-set mismatch as an
--     assembly error.
--
--   'buildings'/'units' read the GLOBAL id allocator out of the snapshot
--   ('snapNextBuildingId'/'snapNextUnitId'), which 'core-session' installs
--   — so they declare 'core-session' as a dependency (see their codecs),
--   and 'dependencyOrder' guarantees 'core-session' folds first.
saveComponentRegistry ∷ [RegisteredComponent]
saveComponentRegistry =
    [ registerComponent coreSessionCodec
        (\_ d snap → Right (applyCoreSession d snap))
    , registerComponent texPaletteCodec
        (\_ d snap → Right (coreSessionTexPalette d snap))
    , registerComponent worldPagesCodec
        (\_ d snap → Right snap { snapPages = basePageSnapshots d })
    , registerComponent worldEditsCodec
        (\ver d snap → onPages snap (applyWorldEdits ver d))
    , registerComponent worldActivityCodec
        (\ver d snap → onPages snap (applyWorldActivity ver d))
    , registerComponent buildingsCodec
        (\ver d snap → onPages snap (applyBuildings ver (snapNextBuildingId snap) d))
    , registerComponent unitsCodec
        (\ver d snap → onPages snap (applyUnits ver (snapNextUnitId snap) d))
    , registerComponent unitSimCodec
        (\ver d snap → onPages snap (applyUnitSim ver d))
    , registerComponent craftBillsCodec
        (\ver d snap → onPages snap (applyCraftBills ver d))
    , registerComponent powerNodesCodec
        (\ver d snap → onPages snap (applyPowerNodes ver d))
    ]
  where
    onPages snap f = (\pages → snap { snapPages = pages }) ⊚ f (snapPages snap)

-- | The ids this reader knows how to interpret (gameplay only — the
--   envelope layer adds @"metadata"@).
componentKnownIds ∷ HS.HashSet ComponentId
componentKnownIds = HS.fromList (map rcId saveComponentRegistry)

-- | The ids this reader hard-requires (every gameplay component is
--   required — none is safely defaultable, requirement 7).
componentRequiredIds ∷ HS.HashSet ComponentId
componentRequiredIds =
    HS.fromList [ rcId c | c ← saveComponentRegistry, rcRequired c ]

-- | Encode every gameplay component from a snapshot into envelope specs
--   (id, version, required, payload bytes).
encodeComponentSpecs
    ∷ SessionSnapshot → [(ComponentId, Word32, Bool, BS.ByteString)]
encodeComponentSpecs snap =
    [ (rcId c, rcVersion c, rcRequired c, rcEncode c snap)
    | c ← saveComponentRegistry ]

-- Static registry contract -----------------------------------------

-- | Build-time invariants on the registry itself (requirement 6): no
--   duplicate ids, every dependency resolves to a registered component,
--   and no dependency cycles. Empty list ⇒ the registry is well-formed.
registryStaticErrors ∷ [Text]
registryStaticErrors = concat [dupErrors, unknownDepErrors, cycleErrors]
  where
    ids       = map rcId saveComponentRegistry
    idSet     = HS.fromList ids
    dupErrors =
        [ "duplicate component id " <> cidText cid
        | (cid, n) ← HM.toList (HM.fromListWith (+) [ (i, 1 ∷ Int) | i ← ids ])
        , n > 1 ]
    unknownDepErrors =
        [ "component " <> cidText (rcId c) <> " depends on unregistered "
            <> cidText d
        | c ← saveComponentRegistry, d ← rcDeps c, not (HS.member d idSet) ]
    cycleErrors = case dependencyOrder saveComponentRegistry of
        Left cyc → [ "dependency cycle: "
                     <> T.intercalate " -> " (map cidText cyc) ]
        Right _  → []
    cidText (ComponentId t) = t

-- | Topologically order the registry by dependency (a component appears
--   after everything it depends on), Kahn-style. 'Left' names the cycle
--   remainder. Dependencies on unregistered ids are ignored here
--   (reported separately by 'registryStaticErrors') so this only ever
--   reports a genuine cycle.
dependencyOrder
    ∷ [RegisteredComponent] → Either [ComponentId] [RegisteredComponent]
dependencyOrder comps = go [] comps
  where
    byId = HM.fromList [ (rcId c, c) | c ← comps ]
    knownDeps c = [ d | d ← rcDeps c, HM.member d byId ]
    go done []        = Right (reverse done)
    go done remaining =
        case filter ready remaining of
            []    → Left (map rcId remaining)  -- nothing emittable ⇒ cycle
            (c:_) → go (c : done) (L.deleteBy sameId c remaining)
      where
        emitted  = HS.fromList (map rcId done)
        ready c  = all (`HS.member` emitted) (knownDeps c)
    sameId a b = rcId a ≡ rcId b

-- Assembly ----------------------------------------------------------

-- | Reconstruct a fully validated 'SessionSnapshot' from a decoded
--   envelope plus the already-decoded manifest 'SaveMetadata'
--   (requirement 6/12). All-or-nothing: any decode / migrate / validate
--   / cross-component failure returns EVERY failure and no snapshot.
--
--   Every step is REGISTRY-DRIVEN (requirement 6): there is no separate
--   hard-coded list of components to apply that could drift from
--   'saveComponentRegistry'. Phase 1 asks each registered component for
--   its decode/self-validate errors ('rcDecodeErrors'); phase 2 folds
--   each component's contribution onto a skeleton via its mandatory
--   'rcApply', in dependency order so @"world-pages"@ installs the base
--   page map before any page-scoped component writes onto it, and
--   @"core-session"@ installs the global id allocators before
--   @"buildings"@/@"units"@ read them. Because 'rcApply' is required to
--   register, a component cannot be written+required yet silently skipped
--   on load.
assembleSnapshot
    ∷ SaveMetadata → DecodedEnvelope → Either [ComponentError] SessionSnapshot
assembleSnapshot meta de = do
    -- Dependency order (a genuine registry cycle is already rejected by
    -- registryStaticErrors' hspec assertion; guarded here defensively).
    ordered ← case dependencyOrder saveComponentRegistry of
        Left cyc → Left [ ComponentError (headOr coreSessionComponentId cyc) 0
                            AssemblePhase
                            ("dependency cycle: "
                             <> T.intercalate " -> " (map cidText cyc)) ]
        Right o  → Right o
    -- 1. Decode + component-local-validate EVERY component (no live state),
    --    collecting all failures (all-or-nothing). Round-3 review: capped
    --    +sorted the SAME way every other boundary error list now is — a
    --    corrupted/adversarial envelope can carry an unbounded number of
    --    per-component decode failures just as easily as a cross-component
    --    one.
    let decodeErrs = concatMap (`rcDecodeErrors` de) saveComponentRegistry
    if not (null decodeErrs) then Left (capComponentErrors decodeErrs) else do
        -- 2. Fold each component's contribution onto the skeleton, in
        --    dependency order. Page-set-mismatch errors are independent per
        --    component (they all check against the SAME base @"world-pages"@
        --    installs first), so accumulate them all rather than
        --    short-circuiting. Foundational folds (world-pages/core-session/
        --    tex/lua) never error, so a failed page component leaves the
        --    base intact for the next component's own check.
        let (applyErrs, snap) = L.foldl' step ([], skeleton) ordered
            step (es, s) rc = case rcApply rc de s of
                Left e   → (es <> e, s)
                Right s' → (es, s')
        if not (null applyErrs) then Left (capComponentErrors applyErrs) else do
            -- 3. Cross-component invariants (requirement 6/9/12).
            --    'structureEditPaletteErrors' is called here rather than
            --    folded into 'validateSessionSnapshot' itself, since THIS
            --    call site is load-only (every component has already
            --    cereal-decoded into fully concrete values) — see its
            --    haddock in "World.Save.Snapshot" for why that distinction
            --    matters (the capture-path "full-encode forcing" contract).
            --
            --    Round-3 review: the 'sessionIntegrityErrors' portion used
            --    to be capped via 'capIntegrityErrors' BEFORE joining the
            --    other three raw sources, which then went through
            --    'capComponentErrors' a second time — a double cap that
            --    could badly under-report how many findings were actually
            --    omitted (e.g. an inner cap already dropping 100 of them
            --    down to a single trailer note, which the OUTER cap could
            --    then itself report as "1 additional... omitted", losing
            --    the true count entirely). Feed every source in RAW and
            --    UNCAPPED and let the single outer 'capComponentErrors'
            --    sort+cap+trailer the complete, real combined list exactly
            --    once.
            let crossErrs = capComponentErrors $
                            map snapErr (validateSessionSnapshot snap)
                            ++ map snapErr (structureEditPaletteErrors snap)
                            ++ metadataErrors meta snap
                            ++ map integrityErr (sessionIntegrityErrors snap)
            if null crossErrs then Right snap else Left crossErrs
  where
    cidText (ComponentId t) = t
    headOr d []      = d
    headOr _ (x : _) = x
    -- An empty starting snapshot. EVERY field is overwritten by a REQUIRED
    -- component's fold (globals by @"core-session"@, palette/Lua by their
    -- own components, pages by @"world-pages"@), all of which are present
    -- and decoded by the time the fold runs, so none of these placeholders
    -- survives into a successfully assembled snapshot.
    skeleton = SessionSnapshot
        { snapGameTime       = 0
        , snapTexPalette     = emptyTexPalette
        , snapNextItemId     = 0
        , snapNextBuildingId = 0
        , snapNextUnitId     = 0
        , snapActivePage     = WorldPageId ""
        , snapVisiblePages   = []
        , snapLiveCamera     = LiveCameraSnapshot
            { lcsOwnerPage = Nothing, lcsX = 0, lcsY = 0
            , lcsZoom = 1, lcsFacing = FaceSouth }
        , snapPages          = HM.empty
        }

-- | Lift a whole-session snapshot invariant failure into a component
--   error attributed to @"core-session"@ (the component owning the
--   cross-cutting relationships those invariants check).
snapErr ∷ Show e ⇒ e → ComponentError
snapErr e = ComponentError coreSessionComponentId 1 AssemblePhase
                (T.pack (show e))

-- | Lift a "World.Save.Integrity" structural finding (issue #764,
--   save-overhaul C3) into the existing per-component error shape —
--   attributed to the component the finding actually names (a
--   craft-bill/power-node wrong-page violation), at 'AssemblePhase'
--   (a cross-component check, same phase every other whole-session
--   invariant above reports at).
integrityErr ∷ IntegrityError → ComponentError
integrityErr e = ComponentError (ieComponent e) (ieVersion e) AssemblePhase
    (iePath e <> ": " <> ieCode e <> ": " <> ieMessage e)

-- | Sort + cap the FULL cross-component error list uniformly (round-2
--   review, issue #764). Before this, only the
--   'sessionIntegrityErrors' portion went through 'capIntegrityErrors'
--   (sorted, capped at 'integrityErrorCap') before landing in
--   'crossErrs' — 'validateSessionSnapshot'/'structureEditPaletteErrors'/
--   'metadataErrors' fed in raw, so the FINAL combined list a caller
--   actually sees was only partially deterministic-and-bounded, not
--   uniformly so (requirement 10: "never an arbitrary first hash-map
--   entry, always capped"). This applies the SAME cap to the complete,
--   already-concatenated list, appending one trailer 'ComponentError'
--   (attributed to @"core-session"@, the same cross-component-invariant
--   owner every other whole-session assembly error above already uses)
--   naming how many were omitted — mirroring 'renderIntegrityReport''s
--   own never-silently-truncate contract at this outer boundary too.
capComponentErrors ∷ [ComponentError] → [ComponentError]
capComponentErrors errs =
    let sorted  = L.sortOn sortKey errs
        total   = length sorted
        capped  = take integrityErrorCap sorted
        omitted = max 0 (total - length capped)
    in capped ++
        [ ComponentError coreSessionComponentId 1 AssemblePhase
            (T.pack (show omitted) <> " additional component finding(s) \
             \omitted (see World.Save.Integrity.integrityErrorCap)")
        | omitted > 0 ]
  where
    sortKey e = ( cidText (ceComponent e), ceVersion e
                , T.pack (show (cePhase e)), ceMessage e )
    cidText (ComponentId t) = t

-- | Requirement 12: the manifest metadata must agree with the
--   authoritative gameplay components. A mismatch invalidates the save
--   for full load (listing may still read metadata alone).
metadataErrors ∷ SaveMetadata → SessionSnapshot → [ComponentError]
metadataErrors meta snap =
    case activePage of
        Nothing → []  -- unreachable: validateSessionSnapshot already required it
        Just p  ->
            let gp = pgsGenParams p
                nm = wiName <$> pgsIdentity p
                gl = pgsIdentity p >>= wiGloss
            in concat
                [ disagree "seed" (smSeed meta) (wgpSeed gp)
                , disagree "world size" (smWorldSize meta) (wgpWorldSize gp)
                , disagree "plate count" (smPlateCount meta) (wgpPlateCount gp)
                , disagree "world name" (smWorldName meta) nm
                , disagree "world gloss" (smWorldGloss meta) gl
                ]
  where
    activePage = HM.lookup (snapActivePage snap) (snapPages snap)
    disagree ∷ (Eq a, Show a) ⇒ Text → a → a → [ComponentError]
    disagree label a b
        | a ≡ b     = []
        | otherwise = [ ComponentError (ComponentId "metadata") 1 AssemblePhase
                          ("manifest " <> label <> " (" <> T.pack (show a)
                           <> ") disagrees with gameplay (" <> T.pack (show b)
                           <> ")") ]
