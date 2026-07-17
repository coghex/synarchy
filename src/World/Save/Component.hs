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
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Text as T
import World.Save.Envelope.Types (ComponentId(..), ComponentDescriptor(..))
import World.Save.Envelope.Codec (DecodedEnvelope(..))
import World.Save.Types (SaveMetadata(..))
import World.Save.Snapshot
    ( SessionSnapshot(..), LiveCameraSnapshot(..), PageSnapshot(..)
    , validateSessionSnapshot )
import World.Generate.Types (WorldGenParams(..))
import World.Page.Types (WorldIdentity(..))
import World.Save.Component.Types
import World.Save.Component.Session
import World.Save.Component.Page
import World.Save.Component.Entities

-- | Every Haskell-owned gameplay component, in a stable declaration
--   order. The @"metadata"@ component is NOT here — it is owned by
--   "World.Save.Envelope" (it carries listing metadata, not gameplay
--   state, and must be readable without decoding any of these).
saveComponentRegistry ∷ [RegisteredComponent]
saveComponentRegistry =
    [ registerComponent coreSessionCodec
    , registerComponent texPaletteCodec
    , registerComponent luaStateCodec
    , registerComponent worldPagesCodec
    , registerComponent worldEditsCodec
    , registerComponent worldActivityCodec
    , registerComponent buildingsCodec
    , registerComponent unitsCodec
    , registerComponent unitSimCodec
    , registerComponent craftBillsCodec
    , registerComponent powerNodesCodec
    ]

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
assembleSnapshot
    ∷ SaveMetadata → DecodedEnvelope → Either [ComponentError] SessionSnapshot
assembleSnapshot meta de = do
    -- 1. Decode + component-local-validate each component (no live state).
    let eCore     = decodeComponentValue coreSessionCodec de
        eTex      = decodeComponentValue texPaletteCodec de
        eLua      = decodeComponentValue luaStateCodec de
        ePages    = decodeComponentValue worldPagesCodec de
        eEdits    = decodeComponentValue worldEditsCodec de
        eActivity = decodeComponentValue worldActivityCodec de
        eBuild    = decodeComponentValue buildingsCodec de
        eUnits    = decodeComponentValue unitsCodec de
        eSim      = decodeComponentValue unitSimCodec de
        eCraft    = decodeComponentValue craftBillsCodec de
        ePower    = decodeComponentValue powerNodesCodec de
        decodeErrs = concat
            [ lefts' eCore, lefts' eTex, lefts' eLua, lefts' ePages
            , lefts' eEdits, lefts' eActivity, lefts' eBuild, lefts' eUnits
            , lefts' eSim, lefts' eCraft, lefts' ePower ]
    if not (null decodeErrs) then Left decodeErrs else do
        core     ← eCore
        tex      ← eTex
        lua      ← eLua
        pagesDTO ← ePages
        edits    ← eEdits
        activity ← eActivity
        build    ← eBuild
        units    ← eUnits
        sim      ← eSim
        craft    ← eCraft
        power    ← ePower
        -- 2. Assemble page snapshots (page-set consistency, requirement 8).
        let base = basePageSnapshots pagesDTO
            -- The building/unit id allocators are global, owned once by
            -- @"core-session"@ (requirement 9); the per-page building/
            -- unit slices carry no allocator copy, so refill each page's
            -- reconstructed snapshot from these single counters.
            bNextId = csNextBuildingId core
            uNextId = csNextUnitId core
            -- Each page-scoped component's REAL encoded version, read
            -- from the decoded manifest descriptor (requirement 6: a
            -- page-set-mismatch error names the true version, not a
            -- placeholder). The descriptor is always present here — the
            -- component decoded successfully just above — so the
            -- fallback to the codec's current version is unreachable.
            verOf ∷ ComponentCodec a → Word32
            verOf cc = maybe (ccVersion cc) cdVersion
                             (findDescriptor (ccId cc) (deManifest de))
            editsVer    = verOf worldEditsCodec
            activityVer = verOf worldActivityCodec
            buildVer    = verOf buildingsCodec
            unitsVer    = verOf unitsCodec
            simVer      = verOf unitSimCodec
            craftVer    = verOf craftBillsCodec
            powerVer    = verOf powerNodesCodec
            applyErrs = concat
                [ leftsOf (applyWorldEdits editsVer edits base)
                , leftsOf (applyWorldActivity activityVer activity base)
                , leftsOf (applyBuildings buildVer bNextId build base)
                , leftsOf (applyUnits unitsVer uNextId units base)
                , leftsOf (applyUnitSim simVer sim base)
                , leftsOf (applyCraftBills craftVer craft base)
                , leftsOf (applyPowerNodes powerVer power base)
                ]
        if not (null applyErrs) then Left applyErrs else do
            pages ←   applyWorldEdits editsVer edits base
                  >>= applyWorldActivity activityVer activity
                  >>= applyBuildings buildVer bNextId build
                  >>= applyUnits unitsVer uNextId units
                  >>= applyUnitSim simVer sim
                  >>= applyCraftBills craftVer craft
                  >>= applyPowerNodes powerVer power
            -- 3. Build the full immutable snapshot from the globals + pages.
            let cam  = csLiveCamera core
                snap = SessionSnapshot
                    { snapGameTime       = csGameTime core
                    , snapTexPalette     = tpdPalette tex
                    , snapNextItemId     = csNextItemId core
                    , snapNextBuildingId = csNextBuildingId core
                    , snapNextUnitId     = csNextUnitId core
                    , snapLuaModules     = lsdModules lua
                    , snapActivePage     = csActivePage core
                    , snapVisiblePages   = csVisiblePages core
                    , snapLiveCamera     = LiveCameraSnapshot
                        { lcsOwnerPage = lcdOwner cam
                        , lcsX = lcdX cam, lcsY = lcdY cam
                        , lcsZoom = lcdZoom cam, lcsFacing = lcdFacing cam }
                    , snapPages          = pages
                    }
            -- 4. Cross-component invariants (requirement 6/9/12).
            let crossErrs = map snapErr (validateSessionSnapshot snap)
                            ++ metadataErrors meta snap
            if null crossErrs then Right snap else Left crossErrs

lefts' ∷ Either [ComponentError] a → [ComponentError]
lefts' (Left es) = es
lefts' (Right _) = []

leftsOf ∷ Either [ComponentError] a → [ComponentError]
leftsOf = lefts'

-- | Lift a whole-session snapshot invariant failure into a component
--   error attributed to @"core-session"@ (the component owning the
--   cross-cutting relationships those invariants check).
snapErr ∷ Show e ⇒ e → ComponentError
snapErr e = ComponentError coreSessionComponentId 1 AssemblePhase
                (T.pack (show e))

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
