{-# LANGUAGE Strict #-}
-- | The shared save/load integrity graph (issue #764, save-overhaul
--   C3): one diagnostic vocabulary ('IntegrityError') and one set of
--   checks, run at BOTH boundaries requirement 7 names —
--   'sessionIntegrityErrors' from "World.Save.Snapshot"'s
--   'World.Save.Snapshot.captureSessionSnapshot' (pre-save) and
--   "World.Save.Component"'s 'World.Save.Component.assembleSnapshot'
--   (pre-load) — over the exact same 'SessionSnapshot'.
--
--   === What lives here vs. what stays where it is
--
--   This module does NOT re-implement every check "World.Save.Snapshot"
--   and "World.Save.Component.Entities" already run (duplicate page ids,
--   item/building/unit allocator bounds, orphaned unit-sim state,
--   page-set/active/visible consistency, per-page bill/power-node
--   allocator + map-key/embedded-id agreement) — those are already
--   correct, already tested, and moving them would only add regression
--   risk for no behavioural gain. What's genuinely NEW here, closing
--   gaps the #764 issue text calls out that no existing check covers:
--
--   - 'sessionIntegrityErrors': a craft bill's station / claimant and a
--     power node's host building are validated for KIND (structural —
--     'BuildingId' and 'UnitId' are distinct Haskell types, so a
--     wrong-kind Haskell reference cannot even be constructed) and PAGE
--     (a target that exists on a DIFFERENT page than the record
--     referencing it is a genuine "wrong-page" violation; a target
--     absent from the WHOLE session stays the documented, tolerated gap
--     — see "World.Save.Snapshot"'s haddock — neither boundary hard-
--     fails on it).
--   - 'luaReferenceErrors': cross-validates every reference a Lua save
--     component declares via its @references()@ hook (requirement 8's
--     "Lua AI targets, claims, deliveries, and nested references") — a
--     capability #761 defined the hook for but never wired to any real
--     target set (see "Engine.Scripting.Lua.API.Save").
--
--   The existing gameplay-content-definition ladder
--   ("World.Save.Types"'s @missingDefReferences@ family, driven from
--   "Engine.Scripting.Lua.API.Save"'s @continueLoad@) is deliberately
--   NOT folded into this module's Haskell types either — 9 already-
--   working, already-tested validators against 9 different IO-loaded
--   registries, rewritten onto one generic traversal, would be a large
--   rewrite of working code for a vocabulary-only gain. They already
--   report through the SAME load-rejection gate ('continueLoad's
--   @allMissing@/@allMessages@) the new checks in this module report
--   through, which is what requirement 7's "same integrity rules at
--   both boundaries" cashes out to operationally: one gate, one
--   rejection message, not two independently-decided ones.
module World.Save.Integrity
    ( IntegrityError(..)
    , IntegrityReport(..)
    , integrityErrorCap
    , capIntegrityErrors
    , renderIntegrityError
    , renderIntegrityReport
    , sessionIntegrityErrors
    , sessionIntegrityWarnings
    , OrderRef(..)
    , OrderRefTarget(..)
    , transferOrderRefs
    , PageEntities(..)
    , pageEntitiesFrom
    , danglingOrderRefErrors
    , KnownEntities(..)
    , buildKnownEntities
    , loadReconcileContextFrom
    , LuaRefEdge(..)
    , luaReferenceErrors
    , refEdgeError
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Text as T
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Data.Int (Int64)
import Craft.Bills (CraftBills(..), CraftBill(..), BillId(..))
import Power.Types (PowerNodes(..), PowerNode(..), PowerNodeId(..))
import Unit.Transfer
    (TransferBatch(..), TransferEndpoint(..), QueuedTransfer(..)
    , TransferItemRef(..))
import Unit.Transfer.Orders
    (TransferOrder(..), TransferOrderId(..), TransferOrders, transferOrderList)
import World.Page.Types (WorldPageId(..))
import World.Save.Envelope.Types (ComponentId(..))
import World.Save.Component.Types
    ( craftBillsComponentId, powerNodesComponentId
    , buildingsComponentId, unitsComponentId
    , transferOrdersComponentId, worldPagesComponentId, ccVersion )
import World.Save.Component.Page (worldPagesCodec)
import World.Save.Payload
    (LuaRefEdge(..), LoadReconcileContext(..))
import World.Save.Reference (RefKind(..), RefScope(..), refKindText)
import World.Save.Snapshot
    ( SessionSnapshot(..), PageSnapshot(..), allItemInstanceIds )
import World.Save.Types
    ( BuildingSnapshot(..), UnitSnapshot(..), ItemWalkOrder(..)
    , pageItemContainers, flattenItemInstances )
import Item.Types (ItemInstance(..))
import Item.Ground (GroundItems(..), GroundItem(..))
import Location.Instance
    ( LocationEncounter(..), LocationEncounterOccupant(..)
    , LocationSignificantItem(..)
    , LocationInstance(..), LocationInstanceId(..), instancesToList )
import World.Generate.Types (WorldGenParams(..))

-- | One structured integrity finding (requirement 10): which component
--   + schema version + data path produced it, what kind of reference is
--   involved, the offending value, the scope that was expected vs. what
--   was actually found, a stable machine-readable code, and a
--   human-readable message.
data IntegrityError = IntegrityError
    { ieComponent     ∷ !ComponentId
    , ieVersion       ∷ !Word32
    , iePath          ∷ !Text
    , ieRefKind       ∷ !RefKind
    , ieRefValue      ∷ !Text
    , ieExpectedScope ∷ !Text
    , ieActual        ∷ !Text
    , ieCode          ∷ !Text
    , ieMessage       ∷ !Text
    } deriving (Show, Eq)

renderIntegrityError ∷ IntegrityError → Text
renderIntegrityError e =
    "[" <> cidText (ieComponent e) <> " v" <> tshow (ieVersion e)
        <> " " <> iePath e <> "] " <> ieCode e <> ": " <> ieMessage e
  where cidText (ComponentId t) = t

-- | Every safely discoverable error, deterministically ordered and
--   capped (requirement 10) — never "stop at an arbitrary first
--   hash-map entry".
data IntegrityReport = IntegrityReport
    { irErrors  ∷ ![IntegrityError]
    , irTotal   ∷ !Int
    , irOmitted ∷ !Int
    } deriving (Show, Eq)

-- | Generous but finite — a corrupted or adversarial save could
--   otherwise produce an unbounded diagnostic list.
integrityErrorCap ∷ Int
integrityErrorCap = 500

-- | Sort by (component, path, ref value, code) — every field that makes
--   two distinct findings distinguishable — then truncate, reporting
--   how many were omitted.
capIntegrityErrors ∷ [IntegrityError] → IntegrityReport
capIntegrityErrors errs =
    let sorted = L.sortOn sortKey errs
        total  = length sorted
        capped = take integrityErrorCap sorted
    in IntegrityReport
        { irErrors  = capped
        , irTotal   = total
        , irOmitted = max 0 (total - length capped)
        }
  where
    sortKey e = ( cidText (ieComponent e), iePath e
                , ieRefValue e, ieCode e )
    cidText (ComponentId t) = t

-- | Every retained finding's rendered text, plus (requirement 10: never
--   silently truncate) one trailing line naming how many additional
--   findings were omitted by the cap. This is what a caller should
--   actually surface — never the raw, unsorted, uncapped error list.
renderIntegrityReport ∷ IntegrityReport → [Text]
renderIntegrityReport report =
    map renderIntegrityError (irErrors report)
    ++ [ tshow (irOmitted report) <> " additional integrity \
        \finding(s) omitted (see World.Save.Integrity.integrityErrorCap)"
       | irOmitted report > 0 ]

-- | The generic same-page/global/permitted-cross-page decision
--   (requirement 4). Given the scope a field declares, the page the
--   referencing record lives on, and EVERY page the target actually
--   resolved on (empty when absent from the whole session), decide
--   whether this is a hard violation:
--
--   - Absent everywhere → 'Nothing' (never a hard error at this layer
--     — the documented, tolerated gap; see the module haddock).
--   - Resolves on MORE THAN ONE page → 'Nothing' here specifically —
--     that is a duplicate-identity violation
--     ('duplicateGlobalIdErrors' reports it, with a stable
--     @"duplicate-identity"@ code), and firing a second, arbitrarily-
--     page-chosen "wrong-page" verdict on top of it would be redundant
--     and potentially non-deterministic depending on iteration order.
--   - 'ScopeGlobal' → any single resolved page is fine.
--   - 'ScopeSamePage' → the one resolved page must be exactly the
--     source page; any other single page is a genuine wrong-page
--     violation.
--   - 'ScopeCrossPage' → explicitly permitted; any single resolved
--     page is fine (no shipped field uses this scope yet — see
--     "World.Save.Reference"'s 'World.Save.Reference.CrossPageRef'
--     haddock — exercised directly by this module's test suite).
refEdgeError
    ∷ ComponentId → Word32 → Text → RefKind → RefScope
    → WorldPageId → [WorldPageId] → Text
    → Maybe IntegrityError
refEdgeError cid ver path kind scope sourcePage foundPages val =
    case (scope, foundPages) of
        (_, [])                                  → Nothing
        (_, _ : _ : _)                            → Nothing
        (ScopeGlobal, [_])                        → Nothing
        (ScopeCrossPage, [_])                     → Nothing
        (ScopeSamePage, [p]) | p ≡ sourcePage      → Nothing
                              | otherwise           → Just (mkErr p)
  where
    mkErr p = IntegrityError
        { ieComponent = cid, ieVersion = ver, iePath = path
        , ieRefKind = kind, ieRefValue = val
        , ieExpectedScope = "same page ('" <> unWorldPageId sourcePage <> "')"
        , ieActual = "found on page '" <> unWorldPageId p <> "'"
        , ieCode = "wrong-page"
        , ieMessage = refKindText kind <> " " <> val
            <> " referenced from page '" <> unWorldPageId sourcePage
            <> "' resolves only on page '" <> unWorldPageId p <> "'"
        }

-- | Every NEW structural integrity check this issue adds over a fully
--   assembled 'SessionSnapshot' — see the module haddock for what's
--   deliberately left to the existing checks instead. Both
--   'World.Save.Snapshot.captureSessionSnapshot' (pre-save) and
--   "World.Save.Component".'World.Save.Component.assembleSnapshot'
--   (pre-load) call this over the SAME snapshot shape, satisfying
--   requirement 7 for everything checkable without an IO-loaded content
--   registry or Lua-owned state (those two stay at their existing,
--   necessarily-IO-bound call sites — see "Engine.Scripting.Lua.API.Save").
sessionIntegrityErrors ∷ SessionSnapshot → [IntegrityError]
sessionIntegrityErrors snap = concat
    [ duplicateGlobalIdErrors snap
    , billStationErrors, billClaimantErrors, nodeBuildingErrors
    , locationOccupantErrors
    , significantProvenanceErrors snap
    , orderRefErrors snap
    ]
  where
    pages = snapPages snap

    -- Every page a building/unit id resolves on, sorted for determinism
    -- — more than one entry means a duplicate global identity, which
    -- 'refEdgeError' deliberately treats as "don't ALSO guess a
    -- wrong-page verdict" (see 'duplicateGlobalIdErrors', the check
    -- that actually reports the duplicate).
    buildingPages ∷ BuildingId → [WorldPageId]
    buildingPages bid = L.sort
        [ pid | (pid, page) ← HM.toList pages
              , HM.member bid (bsnInstances (pgsBuildings page)) ]

    unitPages ∷ UnitId → [WorldPageId]
    unitPages uid = L.sort
        [ pid | (pid, page) ← HM.toList pages
              , HM.member uid (usnInstances (pgsUnits page)) ]

    billStationErrors =
        [ err
        | (pid, page) ← HM.toList pages
        , (bid, bill) ← HM.toList (cbsBills (pgsCraftBills page))
        , let path = "craft-bills[page=" <> unWorldPageId pid <> ",bill="
                     <> tshow (unBillId bid) <> "].station"
        , Just err ← [ refEdgeError craftBillsComponentId 2 path RefBuilding
                         ScopeSamePage pid (buildingPages (cbStation bill))
                         (tshow (unBuildingId (cbStation bill))) ]
        ]

    billClaimantErrors =
        [ err
        | (pid, page) ← HM.toList pages
        , (bid, bill) ← HM.toList (cbsBills (pgsCraftBills page))
        , Just uid ← [ cbClaimant bill ]
        , let path = "craft-bills[page=" <> unWorldPageId pid <> ",bill="
                     <> tshow (unBillId bid) <> "].claimant"
        , Just err ← [ refEdgeError craftBillsComponentId 2 path RefUnit
                         ScopeSamePage pid (unitPages uid)
                         (tshow (unUnitId uid)) ]
        ]

    nodeBuildingErrors =
        [ err
        | (pid, page) ← HM.toList pages
        , (nid, node) ← HM.toList (pnsNodes (pgsPowerNodes page))
        , let path = "power-nodes[page=" <> unWorldPageId pid <> ",node="
                     <> tshow (unPowerNodeId nid) <> "].building"
        , Just err ← [ refEdgeError powerNodesComponentId 2 path RefBuilding
                         ScopeSamePage pid (buildingPages (pnBuilding node))
                         (tshow (unBuildingId (pnBuilding node))) ]
        ]

    locationOccupantErrors =
        [ err
        | (pid, page) ← HM.toList pages
        , inst ← instancesToList
            (wgpLocationInstances (pgsGenParams page))
        , encounter ← maybeToList (liEncounter inst)
        , (index, occupant) ← zip [(0 ∷ Int) ..] (leOccupants encounter)
        , let uid = leoUnitId occupant
              path = "world-pages[page=" <> unWorldPageId pid
                  <> "].locations[" <> tshow (unLocationInstanceId (liId inst))
                  <> "].encounter.occupants[" <> tshow index <> "].unit"
        , Just err ← [ refEdgeError worldPagesComponentId
                         worldPagesVersion path RefUnit
                         ScopeSamePage pid (unitPages uid)
                         (tshow (unUnitId uid)) ]
        ]

-- Transfer orders (#1246) -------------------------------------------

-- | What ONE reference a durable transfer order carries points AT. Kept
--   as a small closed sum rather than three parallel enumerations so the
--   hard (wrong-page) and tolerated (dangling) checks below cannot drift
--   apart: both walk the SAME 'transferOrderRefs' list and differ only
--   in how they resolve it.
data OrderRefTarget
    = OrderRefUnit     !UnitId
    | OrderRefBuilding !BuildingId
    | OrderRefItem     !Int64
      -- ^ An 'Unit.Transfer.TransferItemRef.tirInstanceId', held SIGNED
      --   exactly as the live ref holds it. A value that is negative or
      --   0 can never name a live instance, so it simply resolves
      --   nowhere and is reported as dangling — never coerced into a
      --   large 'Word64' that might collide with a real id.
    deriving (Show, Eq)

-- | One durable reference an order carries, with the data path, kind
--   and rendered value a diagnostic needs.
data OrderRef = OrderRef
    { orfPath   ∷ !Text
    , orfKind   ∷ !RefKind
    , orfValue  ∷ !Text
    , orfTarget ∷ !OrderRefTarget
    } deriving (Show, Eq)

-- | Every durable reference one page's transfer orders carry: each
--   order's acting unit, both of its endpoints, and every requested
--   item instance — the four things #1246 requirement 5 puts into the
--   integrity graph.
--
--   THE single enumeration. 'orderRefErrors' (wrong-page, fatal) and
--   'danglingOrderRefErrors' (absent, tolerated) both consume it, and
--   the load boundary consumes it a third time through
--   'pageEntitiesFrom', so a reference kind added to an order is
--   checked everywhere from one edit.
transferOrderRefs ∷ WorldPageId → TransferOrders → [OrderRef]
transferOrderRefs pid orders = concatMap orderRefs (transferOrderList orders)
  where
    orderRefs o =
        let at field = "transfer-orders[page=" <> unWorldPageId pid <> ",order="
                       <> tshow (unTransferOrderId (troId o)) <> "]."
                       <> field
        in endpointRef (at "unit") (EndpointUnit (troUnit o))
           ⧺ endpointRef (at "source") (tbSource (troBatch o))
           ⧺ endpointRef (at "destination") (tbDestination (troBatch o))
           ⧺ [ OrderRef
                 { orfPath   = at ("entries[" <> tshow i <> "].instance")
                 , orfKind   = RefItemInstance
                 , orfValue  = tshow iid
                 , orfTarget = OrderRefItem iid }
             | (i, entry) ← zip [(0 ∷ Int) ..] (tbEntries (troBatch o))
             , let iid = tirInstanceId (qtItem entry) ]
    endpointRef path e = case e of
        EndpointUnit uid →
            [ OrderRef path RefUnit (tshow (unUnitId uid))
                       (OrderRefUnit uid) ]
        EndpointBuilding bid →
            [ OrderRef path RefBuilding (tshow (unBuildingId bid))
                       (OrderRefBuilding bid) ]

-- | The live identities resolvable on ONE page, as the transfer-order
--   reference checks need them.
data PageEntities = PageEntities
    { peUnits     ∷ !(HS.HashSet UnitId)
    , peBuildings ∷ !(HS.HashSet BuildingId)
    , peItems     ∷ !(HS.HashSet Word64)
    , peGroundItems ∷ !(HM.HashMap Word64 Text)
      -- ^ The page's ground items, each mapped to its own
      --   'Item.Types.iiDefName' (#917). Kept beside 'peItems' rather
      --   than derived from it because a significant item's provenance
      --   asks two questions the whole-page set cannot answer: an
      --   untaken obligation must still be LYING on the ground —
      --   finding its item in a unit's inventory or a building's
      --   storage is a contradiction, since it cannot be there without
      --   having been picked up, which is the one thing that latches
      --   @taken@ — and the item lying there must BE the thing the
      --   obligation says is owed, which needs the def name.
      --
      --   TOP-LEVEL ONLY: each ground entry's own @giInst@, never
      --   recursed through 'Item.Types.iiContents'. That is the whole
      --   point rather than an omission.
      --   'Engine.Scripting.Lua.API.Items.Ground.pickupGroundOnPage'
      --   removes a GROUND-MAP entry and latches @iiInstanceId
      --   (giInst gi)@ — the outer item — so an id that exists only
      --   inside a container on the ground is not pickable as its own
      --   ground item and can never latch its obligation. Admitting it
      --   here would pass a save whose obligation is permanently
      --   undischargeable, which is exactly the state this check
      --   exists to refuse. 'peItems' still flattens, because the
      --   question IT answers — does this id exist anywhere on the
      --   page — is a different one.
    } deriving (Show, Eq)

-- | Build a page's resolvable identities from its three item-bearing
--   projections plus its unit/building instance maps.
--
--   Parameterised over the projections rather than over a page type for
--   the same reason 'World.Save.Types.pageItemContainers' is: the two
--   page shapes ('World.Save.Snapshot.PageSnapshot' and the transitional
--   'World.Save.Types.WorldPageSave' bridge) carry identically-typed
--   fields, so the pre-save boundary and the load boundary
--   ("World.Load.Stage") resolve an order's references through ONE
--   implementation instead of two that could disagree about what counts
--   as present.
pageEntitiesFrom
    ∷ (page → GroundItems)
    → (page → UnitSnapshot)
    → (page → BuildingSnapshot)
    → page
    → PageEntities
pageEntitiesFrom groundOf unitsOf buildingsOf page = PageEntities
    { peUnits     = HM.keysSet (usnInstances (unitsOf page))
    , peBuildings = HM.keysSet (bsnInstances (buildingsOf page))
    , peItems     = HS.fromList
        [ iiInstanceId i
        | (_, insts) ← pageItemContainers ItemsGroundFirst
                           groundOf unitsOf buildingsOf page
        , inst ← insts
        , i    ← flattenItemInstances inst ]
    , peGroundItems = HM.fromList
        [ (iiInstanceId inst, iiDefName inst)
        | inst ← map giInst (HM.elems (gisItems (groundOf page))) ]
    }

resolvesOn ∷ PageEntities → OrderRefTarget → Bool
resolvesOn pe t = case t of
    OrderRefUnit uid     → HS.member uid (peUnits pe)
    OrderRefBuilding bid → HS.member bid (peBuildings pe)
    OrderRefItem iid     → iid > 0 ∧ HS.member (fromIntegral iid) (peItems pe)

-- | Every WRONG-PAGE transfer-order reference: a target that exists, but
--   on a page other than the order's own. Fatal at both boundaries, like
--   a craft bill's wrong-page station — an order can only ever be
--   carried out within one page, so a cross-page target is never
--   legitimate. A target absent from the WHOLE session is deliberately
--   NOT reported here; see 'danglingOrderRefErrors'.
orderRefErrors ∷ SessionSnapshot → [IntegrityError]
orderRefErrors snap =
    [ err
    | (pid, page) ← HM.toList (snapPages snap)
    , r ← transferOrderRefs pid (pgsTransferOrders page)
    , Just err ← [ refEdgeError transferOrdersComponentId 1 (orfPath r)
                     (orfKind r) ScopeSamePage pid
                     (sessionPagesOf entitiesByPage (orfTarget r)) (orfValue r) ]
    ]
  where entitiesByPage = snapshotPageEntities snap

-- | Every page's resolvable identities, built ONCE per traversal. A
--   page's item-instance set is a full recursive walk of its ground /
--   inventory / storage containers, so deriving it per REFERENCE (an
--   order can carry many, and every one is resolved against every page)
--   would make an ordinary save quadratic in the session's item count
--   for no gain.
snapshotPageEntities ∷ SessionSnapshot → HM.HashMap WorldPageId PageEntities
snapshotPageEntities =
    HM.map (pageEntitiesFrom pgsGroundItems pgsUnits pgsBuildings) ∘ snapPages

-- | Every page a transfer-order target resolves on, sorted (the input
--   'refEdgeError' expects: empty ⇒ absent everywhere, one entry ⇒ a
--   definite page, several ⇒ a duplicate identity reported elsewhere).
sessionPagesOf
    ∷ HM.HashMap WorldPageId PageEntities → OrderRefTarget → [WorldPageId]
sessionPagesOf entitiesByPage t = L.sort
    [ pid | (pid, pe) ← HM.toList entitiesByPage, resolvesOn pe t ]

-- | Every DANGLING transfer-order reference on one page: a target absent
--   from the identities @entities@ names. Non-blocking BY CONSTRUCTION —
--   this is a separate function from 'sessionIntegrityErrors' precisely
--   because every finding that one returns aborts the save (see
--   'World.Thread.Command.Save.WriteWorld') or the load (see
--   'World.Save.Component.assembleSnapshot'), and #1246 requirement 5
--   wants a demolished destination or a dead carrier REPORTED while the
--   order itself is retained and both boundaries still succeed. Callers
--   log these; nothing may fail on them.
danglingOrderRefErrors
    ∷ WorldPageId → PageEntities → TransferOrders → [IntegrityError]
danglingOrderRefErrors pid entities orders =
    [ IntegrityError
        { ieComponent     = transferOrdersComponentId
        , ieVersion       = 1
        , iePath          = orfPath r
        , ieRefKind       = orfKind r
        , ieRefValue      = orfValue r
        , ieExpectedScope = "same page ('" <> unWorldPageId pid <> "')"
        , ieActual        = "not found in the loaded session"
        , ieCode          = "dangling-reference"
        , ieMessage       = refKindText (orfKind r) <> " " <> orfValue r
            <> " referenced by a transfer order on page '" <> unWorldPageId pid
            <> "' does not resolve (tolerated: the order is retained)"
        }
    | r ← transferOrderRefs pid orders
    , not (resolvesOn entities (orfTarget r))
    ]

-- Guaranteed significant contents (#917) -----------------------------

-- | One placed location's guaranteed significant-item obligation,
--   flattened with the page it belongs to and the data path a
--   diagnostic needs. THE single enumeration: the hard rules below and
--   the tolerated warning both walk it, so a change to what an
--   obligation carries is checked everywhere from one edit — the same
--   discipline 'transferOrderRefs' holds for orders.
significantRefs
    ∷ SessionSnapshot → [(WorldPageId, LocationInstance, LocationSignificantItem, Text)]
significantRefs snap =
    [ (pid, inst, entry, path)
    | (pid, page) ← L.sortOn fst (HM.toList (snapPages snap))
    , inst ← instancesToList (wgpLocationInstances (pgsGenParams page))
    , entry ← liSignificant inst
    , let path = "world-pages[page=" <> unWorldPageId pid
              <> "].locations[" <> tshow (unLocationInstanceId (liId inst))
              <> "].significant[" <> tshow (lsiSlot entry) <> "].item"
    ]

-- | The BLOCKING provenance rules for guaranteed significant items
--   (#917). Two, and both are contradictions rather than mere absences
--   — which is why they abort the boundary while a missing item only
--   warns:
--
--   * an UNTAKEN obligation's item must, if it resolves at all, be
--     lying on the GROUND of the obligation's OWN page. Resolving on
--     another page means the durable @(page, instance)@ provenance is
--     wrong; resolving in a unit's inventory or a building's storage
--     means the item was picked up while its latch says it was not,
--     which would let a location stay unclearable forever with its
--     reward already in someone's pack. Once TAKEN, no rule at all: the
--     item may sit anywhere or have been consumed or destroyed, which
--     #917 requirement 3 explicitly allows.
--   * one physical item identity is owned by at most ONE obligation
--     across the whole session. Item instance ids come from a GLOBAL
--     allocator, so two obligations naming one id can never be two real
--     items — and a single pickup would latch a location whose own item
--     was never taken.
--     ('Location.Instance.locationSignificantItemErrors' catches the
--     page-local case at component decode; this is the session-wide
--     one, which no single component can see.)
--   * a bound item identity is BELOW the session's item-id cursor —
--     the only ids the monotonic allocator can have minted. Unlike
--     every rule above, this one applies to a TAKEN obligation too:
--     "taken" excuses an item from resolving anywhere, so an id the
--     allocator never reached is indistinguishable from a consumed one
--     to a resolution check, and would satisfy clearance with nothing
--     ever spawned or picked up. The cursor lives in @core-session@,
--     so this is again session-wide by necessity.
significantProvenanceErrors ∷ SessionSnapshot → [IntegrityError]
significantProvenanceErrors snap =
    resolutionErrors ⧺ ownershipErrors ⧺ allocatorErrors
  where
    entitiesByPage = snapshotPageEntities snap
    refs = significantRefs snap

    resolutionErrors =
        [ IntegrityError
            { ieComponent     = worldPagesComponentId
            , ieVersion       = worldPagesVersion
            , iePath          = path
            , ieRefKind       = RefItemInstance
            , ieRefValue      = tshow itemId
            , ieExpectedScope = "a ground '" <> lsiItemDefName entry
                <> "' on the owning page ('" <> unWorldPageId pid
                <> "') while untaken"
            , ieActual        = actual
            , ieCode          = "wrong-scope-reference"
            , ieMessage       = "untaken significant item " <> tshow itemId
                <> " owed by location #"
                <> tshow (unLocationInstanceId (liId inst))
                <> " on page '" <> unWorldPageId pid <> "' " <> actual
            }
        | (pid, inst, entry, path) ← refs
        , not (lsiTaken entry)
        , Just itemId ← [lsiInstanceId entry]
        , Just actual ← [misresolution pid (lsiItemDefName entry) itemId]
        ]

    -- 'Nothing' when the item is where an untaken obligation requires
    -- AND is the thing it says is owed, or absent from the session
    -- entirely (which 'significantDanglingWarnings' reports and
    -- tolerates). 'Just' names the contradiction.
    misresolution pid owedDef itemId = case onOwnGround of
        -- Lying on the owning page's ground, as required — but is it
        -- the RIGHT item? A binding that named the wrong definition
        -- would otherwise let picking that item up latch the slot and
        -- clear the location with the guaranteed one still on the
        -- floor. The registration boundary refuses such a binding
        -- ('Location.Instance.registerLocationSignificantSpawn'); this
        -- is the save-side half, for a payload that never went through
        -- it.
        Just actualDef
            | actualDef ≡ owedDef → Nothing
            | otherwise → Just ("is a '" <> actualDef
                <> "' lying on that page's ground, not the '" <> owedDef
                <> "' the obligation names")
        Nothing → case pagesHolding of
            [] → Nothing
            ps | pid `elem` ps →
                   Just "is held in an inventory, in storage, or nested \
                        \inside a container on that page"
               | otherwise →
                   Just ("resolves on page(s) "
                       <> T.intercalate ", " (map unWorldPageId ps))
      where
        onOwnGround = HM.lookup itemId ∘ peGroundItems
                          =≪ HM.lookup pid entitiesByPage
        pagesHolding = L.sort
            [ p | (p, pe) ← HM.toList entitiesByPage
                , HS.member itemId (peItems pe) ]

    ownershipErrors =
        [ IntegrityError
            { ieComponent     = worldPagesComponentId
            , ieVersion       = worldPagesVersion
            , iePath          = "item-instance#" <> tshow itemId
            , ieRefKind       = RefItemInstance
            , ieRefValue      = tshow itemId
            , ieExpectedScope = "owned by at most one significant \
                                \obligation (item ids are one global \
                                \allocator)"
            , ieActual        = "claimed by " <> ownersText
            , ieCode          = "duplicate-identity"
            , ieMessage       = "significant item " <> tshow itemId
                <> " is owed by more than one location: " <> ownersText
            }
        | (itemId, owners) ← L.sortOn fst (HM.toList (HM.fromListWith (flip (⧺))
            [ ( itemId
              , [ unWorldPageId pid <> "#"
                    <> tshow (unLocationInstanceId (liId inst))
                    <> " slot " <> tshow (lsiSlot entry) ] )
            | (pid, inst, entry, _) ← refs
            , Just itemId ← [lsiInstanceId entry] ]))
        , length owners > 1
        , let ownersText = T.intercalate ", " owners
        ]

    -- An id the monotonic allocator could not yet have minted names no
    -- item that ever existed. 'itemAllocatorErrors' already refuses a
    -- live 'ItemInstance' at or above the cursor, but an obligation is
    -- not an item: its id is a bare reference, so a payload can carry
    -- one the allocator never reached and nothing else in the session
    -- has to agree with it.
    --
    -- This is the one provenance rule that must NOT skip a taken
    -- obligation. Every other rule can, because a taken item is
    -- legitimately allowed to be anywhere or gone — but "gone" is
    -- exactly what an id above the cursor looks like to a
    -- resolution-based check, so a forged @lsiTaken = True@ paired with
    -- @snapNextItemId@ or higher would resolve nowhere, draw at most a
    -- tolerated dangling warning, and then satisfy
    -- 'Location.Instance.significantRecovered' — clearing a location
    -- with no spawn and no pickup ever having happened. The lower bound
    -- (0, the never-minted sentinel) is component decode's
    -- ('Location.Instance.significantEntryErrors'); this is the upper
    -- one, which no single component can check because the cursor lives
    -- in @core-session@.
    allocatorErrors =
        [ IntegrityError
            { ieComponent     = worldPagesComponentId
            , ieVersion       = worldPagesVersion
            , iePath          = path
            , ieRefKind       = RefItemInstance
            , ieRefValue      = tshow itemId
            , ieExpectedScope = "an item identity the global allocator "
                <> "has actually minted (below the saved cursor "
                <> tshow cursor <> ")"
            , ieActual        = "at or above that cursor, so no such "
                <> "item was ever created"
            , ieCode          = "unmintable-identity"
            , ieMessage       = "significant item " <> tshow itemId
                <> " owed by location #"
                <> tshow (unLocationInstanceId (liId inst))
                <> " on page '" <> unWorldPageId pid
                <> "' is at or above the session's item-id cursor ("
                <> tshow cursor <> "), so the allocator never minted it"
            }
        | let cursor = snapNextItemId snap
        , (pid, inst, entry, path) ← refs
        , Just itemId ← [lsiInstanceId entry]
        , itemId ≥ cursor
        ]

-- | The TOLERATED half of #917's provenance rules: an untaken
--   obligation whose item is absent from the whole session. Reported,
--   never fatal — the obligation stays untaken and the location stays
--   unclearable, which is the honest outcome and exactly what
--   requirement 9 asks for ("a missing referenced runtime entity must
--   not be mistaken for a taken item or a satisfied condition").
--   Deliberately a warning rather than an error for the same reason a
--   dangling transfer-order target is: the state is recoverable and
--   refusing the whole save would lose far more than it protects.
significantDanglingWarnings ∷ SessionSnapshot → [IntegrityError]
significantDanglingWarnings snap =
    [ IntegrityError
        { ieComponent     = worldPagesComponentId
        , ieVersion       = worldPagesVersion
        , iePath          = path
        , ieRefKind       = RefItemInstance
        , ieRefValue      = tshow itemId
        , ieExpectedScope = "same page ('" <> unWorldPageId pid <> "')"
        , ieActual        = "not found in the loaded session"
        , ieCode          = "dangling-reference"
        , ieMessage       = "significant item " <> tshow itemId
            <> " owed by location #"
            <> tshow (unLocationInstanceId (liId inst)) <> " on page '"
            <> unWorldPageId pid <> "' does not resolve (tolerated: the \
               \obligation stays untaken)"
        }
    | (pid, inst, entry, path) ← significantRefs snap
    , not (lsiTaken entry)
    , Just itemId ← [lsiInstanceId entry]
    , not (any (HS.member itemId ∘ peItems)
               (HM.elems (snapshotPageEntities snap)))
    ]

-- | The NON-BLOCKING half of the session integrity graph (#1246): every
--   finding that must be surfaced as a diagnostic and must never fail a
--   boundary. Deliberately a sibling of 'sessionIntegrityErrors' rather
--   than a flag on 'IntegrityError' — the two are consumed at different
--   severities by different code, and a severity field would make it one
--   forgotten @filter@ away from a tolerated dangling order rejecting an
--   otherwise-valid save.
--
--   Today this is exactly the dangling transfer-order references above.
--   Craft bills' and power nodes' equally-tolerated dangling
--   station/host references are deliberately NOT folded in: they are
--   silently tolerated today (issues #758/#763) and reporting them here
--   would change what an existing, unrelated save logs.
sessionIntegrityWarnings ∷ SessionSnapshot → [IntegrityError]
sessionIntegrityWarnings snap =
    orderWarnings ⧺ locationWarnings ⧺ significantDanglingWarnings snap
  where entitiesByPage = snapshotPageEntities snap
        orderWarnings =
            [ e
            | (pid, page) ← HM.toList (snapPages snap)
            , Just pe ← [HM.lookup pid entitiesByPage]
            , e ← danglingOrderRefErrors pid pe (pgsTransferOrders page)
            ]
        locationWarnings =
            [ IntegrityError
                { ieComponent = worldPagesComponentId
                , ieVersion = worldPagesVersion
                , iePath = path
                , ieRefKind = RefUnit
                , ieRefValue = tshow (unUnitId uid)
                , ieExpectedScope = "same page ('" <> unWorldPageId pid <> "')"
                , ieActual = "absent from the session"
                , ieCode = "dangling-reference"
                , ieMessage = "encounter occupant unit '"
                    <> tshow (unUnitId uid)
                    <> "' does not resolve (tolerated: roster membership is retained)"
                }
            | (pid, page) ← HM.toList (snapPages snap)
            , inst ← instancesToList
                (wgpLocationInstances (pgsGenParams page))
            , encounter ← maybeToList (liEncounter inst)
            , (index, occupant) ← zip [(0 ∷ Int) ..] (leOccupants encounter)
            , let uid = leoUnitId occupant
                  path = "world-pages[page=" <> unWorldPageId pid
                      <> "].locations["
                      <> tshow (unLocationInstanceId (liId inst))
                      <> "].encounter.occupants[" <> tshow index <> "].unit"
            , null (sessionPagesOf entitiesByPage (OrderRefUnit uid))
            ]

-- | A 'UnitId'/'BuildingId' is a GLOBAL allocator (one counter for the
--   whole session, see "World.Save.Snapshot"'s 'SessionGlobals'
--   haddock) — the SAME numeric id existing in more than one page's
--   instance map is therefore never legitimate, unlike 'BillId'/
--   'PowerNodeId' (genuinely per-page allocators, where the same
--   number on two pages is normal and already excluded from this check).
duplicateGlobalIdErrors ∷ SessionSnapshot → [IntegrityError]
duplicateGlobalIdErrors snap = concat
    [ dupsFor RefBuilding buildingsComponentId
        [ (pid, unBuildingId bid)
        | (pid, page) ← HM.toList (snapPages snap)
        , bid ← HM.keys (bsnInstances (pgsBuildings page)) ]
    , dupsFor RefUnit unitsComponentId
        [ (pid, unUnitId uid)
        | (pid, page) ← HM.toList (snapPages snap)
        , uid ← HM.keys (usnInstances (pgsUnits page)) ]
    ]
  where
    dupsFor ∷ RefKind → ComponentId → [(WorldPageId, Word32)] → [IntegrityError]
    dupsFor kind cid entries =
        [ IntegrityError
            { ieComponent = cid, ieVersion = 1
            , iePath = refKindText kind <> "#" <> tshow val
            , ieRefKind = kind, ieRefValue = tshow val
            , ieExpectedScope = "globally unique identity (one allocator \
                                 \for the whole session)"
            , ieActual = "present on pages " <> pagesText
            , ieCode = "duplicate-identity"
            , ieMessage = refKindText kind <> " " <> tshow val
                <> " exists on multiple pages: " <> pagesText
            }
        | (val, ps) ← HM.toList (HM.fromListWith (++)
                          [ (v, [pid]) | (pid, v) ← entries ])
        , length ps > 1
        , let pagesText = T.intercalate ", " (map unWorldPageId (L.sort ps))
        ]

-- Lua reference validation --------------------------------------------

-- | Id sets every Lua-declared reference (requirement 8's "Lua AI
--   targets, claims, deliveries, and nested references") is checked for
--   existence against. @keUnits@/@keBuildings@/@keItemInstances@ are
--   session-wide — every registered Lua component declares
--   @scope = "global"@ today (#761), matching @scrubStaleRefs@'s own
--   global survivor-set reconciliation, AND 'UnitId'/'BuildingId'/item-
--   instance ids are genuinely GLOBAL allocators (one counter for the
--   whole session — see "World.Save.Snapshot"). @keBillsByPage@/
--   @keGroundItemsByPage@ are deliberately PER-PAGE instead: 'BillId'/
--   ground-item ids are per-page allocators (the same number
--   legitimately names two different real entities on two different
--   pages), so resolving them session-wide would let a reference meant
--   for one page's (missing) bill silently "resolve" against an
--   unrelated bill of the same number on another page — a false
--   negative that would mask a genuine dangling reference. Resolution
--   for these two kinds goes through @keUnitPage@ (the owning unit's
--   page, threaded from the reference edge's own @owner@ — see
--   'LuaRefEdge').
--   @keLocationsByPage@ (#915) is per-page for the same reason, but
--   resolved differently: a per-unit location memory names its own page
--   explicitly on the wire ('lrePage'), rather than borrowing the owning
--   unit's, because the page is part of the reference's durable identity
--   @(WorldPageId, LocationInstanceId)@ (#911) and must survive
--   independently of where the remembering unit happens to be.
data KnownEntities = KnownEntities
    { keUnits             ∷ !(HS.HashSet Int)
    , keBuildings         ∷ !(HS.HashSet Int)
    , keBillsByPage       ∷ !(HM.HashMap WorldPageId (HS.HashSet Int))
    , keItemInstances     ∷ !(HS.HashSet Int)
    , keGroundItemsByPage ∷ !(HM.HashMap WorldPageId (HS.HashSet Int))
    , keLocationsByPage   ∷ !(HM.HashMap WorldPageId (HS.HashSet Int))
    , keUnitPage          ∷ !(HM.HashMap Int WorldPageId)
    , keNextUnitId        ∷ !Int
    , keNextBuildingId    ∷ !Int
    , keNextItemId        ∷ !Int
    } deriving (Show, Eq)

buildKnownEntities ∷ SessionSnapshot → KnownEntities
buildKnownEntities snap = KnownEntities
    { keUnits = HS.fromList
        [ fromIntegral (unUnitId uid)
        | page ← pages, uid ← HM.keys (usnInstances (pgsUnits page)) ]
    , keBuildings = HS.fromList
        [ fromIntegral (unBuildingId bid)
        | page ← pages, bid ← HM.keys (bsnInstances (pgsBuildings page)) ]
    , keBillsByPage = HM.fromList
        [ (pid, HS.fromList
              [ fromIntegral (unBillId bid)
              | bid ← HM.keys (cbsBills (pgsCraftBills page)) ])
        | (pid, page) ← HM.toList (snapPages snap) ]
    , keItemInstances = HS.fromList
        (map fromIntegral (allItemInstanceIds snap))
    , keGroundItemsByPage = HM.fromList
        [ (pid, HS.fromList (HM.keys (gisItems (pgsGroundItems page))))
        | (pid, page) ← HM.toList (snapPages snap) ]
    , keLocationsByPage = HM.fromList
        [ (pid, HS.fromList
              [ unLocationInstanceId (liId inst)
              | inst ← instancesToList
                    (wgpLocationInstances (pgsGenParams page)) ])
        | (pid, page) ← HM.toList (snapPages snap) ]
    , keUnitPage = HM.fromList
        [ (fromIntegral (unUnitId uid), pid)
        | (pid, page) ← HM.toList (snapPages snap)
        , uid ← HM.keys (usnInstances (pgsUnits page)) ]
    , keNextUnitId     = fromIntegral (snapNextUnitId snap)
    , keNextBuildingId = fromIntegral (snapNextBuildingId snap)
    , keNextItemId     = fromIntegral (snapNextItemId snap)
    }
  where pages = HM.elems (snapPages snap)

-- | Project the three identity scopes Lua's post-load reconciliation
--   needs out of the same 'KnownEntities' the reference-edge
--   cross-validator already resolves against (issue #1589).
--
--   Deriving the Lua-facing context from 'KnownEntities' rather than
--   rebuilding it from a session is the point: 'luaEdgeResolves' above
--   decides at LOAD time whether an edge resolves, and
--   @scripts/unit_ai_reconcile.lua@ decides at RECONCILE time whether
--   the very same edge should be cleared. Two derivations of "which
--   entities exist, and on which page" could disagree; one cannot.
--
--   Units and buildings are deliberately omitted — 'onSaveLoaded'
--   already receives both survivor arrays positionally.
loadReconcileContextFrom ∷ KnownEntities → LoadReconcileContext
loadReconcileContextFrom ke = LoadReconcileContext
    { lrcItemInstances     = HS.toList (keItemInstances ke)
    , lrcUnitPages         = [ (uid, pid)
                             | (uid, WorldPageId pid) ← HM.toList (keUnitPage ke) ]
    , lrcBillsByPage       = byPage (keBillsByPage ke)
    , lrcGroundItemsByPage = byPage (keGroundItemsByPage ke)
    }
  where
    byPage m = [ (pid, HS.toList ids)
               | (WorldPageId pid, ids) ← HM.toList m ]

-- 'LuaRefEdge' — the edge record this module's checks consume — is
-- defined in the leaf module "World.Save.Payload" and re-exported here,
-- so the HsLua reader that builds it
-- ("Engine.Scripting.Lua.API.Save.Bridge") and the world-thread command
-- that transports it ("World.Command.Types") can name the SAME record
-- without importing this module's own snapshot/envelope dependencies
-- (issue #1103). Its full haddock lives with the definition.

-- | Does this edge resolve against the known entity sets? An unknown
--   @kind@ string is not this function's problem to catch (a
--   registration-time vocabulary mismatch — see
--   @tools/persistence_inventory_audit.py@'s reference-kind check) and
--   is treated as trivially resolving rather than manufacturing a false
--   positive.
--
--   @craft_bill@/@ground_item@ resolve against the OWNING unit's page
--   only (per-page allocators — see 'KnownEntities' haddock): with no
--   owner, or an owner that itself doesn't resolve to a live unit, the
--   edge is reported as not resolving rather than falling back to a
--   session-wide (and therefore potentially wrong-page) match.
--
--   @location_instance@ (#915) resolves against the edge's OWN declared
--   page ('lrePage') — a page missing from the restored session, or an
--   edge that declares none at all, is reported as not resolving rather
--   than matched session-wide, which is what stops two pages' equally-
--   numbered instance ids from aliasing.
luaEdgeResolves ∷ KnownEntities → LuaRefEdge → Bool
luaEdgeResolves ke e = case lreKind e of
    "unit"              → HS.member (lreId e) (keUnits ke)
    "building"          → HS.member (lreId e) (keBuildings ke)
    "item_instance"     → HS.member (lreId e) (keItemInstances ke)
    "craft_bill"        → resolvesOnOwnerPage (keBillsByPage ke)
    "ground_item"       → resolvesOnOwnerPage (keGroundItemsByPage ke)
    "location_instance" → resolvesOnDeclaredPage (keLocationsByPage ke)
    _                   → True
  where
    resolvesOnOwnerPage byPage =
        case lreOwner e ⌦ (`HM.lookup` keUnitPage ke) of
            Nothing  → False
            Just pid → maybe False (HS.member (lreId e)) (HM.lookup pid byPage)
    resolvesOnDeclaredPage byPage =
        case lrePage e of
            Nothing   → False
            Just page → maybe False (HS.member (lreId e))
                              (HM.lookup (WorldPageId page) byPage)

-- | An id at/above the relevant GLOBAL allocator can never have
--   belonged to a real entity (requirement 8's "allocators that could
--   reuse a persisted identity") — distinguished from an ordinary
--   dangling reference (a legitimately-existing target that died before
--   the save boundary) with its own code, even though — like every Lua
--   reference check — neither ever blocks a load (requirement 8/11: the
--   #761-established tolerated-dangling-reference contract; see the
--   module haddock). Only checked for the three GLOBALLY-allocated
--   kinds; bill/ground-item allocators are PER-PAGE and a bare Lua
--   reference carries no page to check against.
luaEdgeExceedsAllocator ∷ KnownEntities → LuaRefEdge → Bool
luaEdgeExceedsAllocator ke e = case lreKind e of
    "unit"          → lreId e ≥ keNextUnitId ke
    "building"      → lreId e ≥ keNextBuildingId ke
    "item_instance" → lreId e ≥ keNextItemId ke
    _               → False

-- | Every Lua-declared reference that does not resolve, as a diagnostic
--   (requirement 16: exposed to headless diagnostics / load completion
--   text) — NEVER a load-blocking failure, matching the #761-established
--   tolerated-dangling-reference contract this module documents above.
--   @componentVersions@ (issue #764) maps a Lua
--   component id to the schema version its edges were collected against
--   (the save side's just-snapshotted payload version; the load side's
--   just-decoded payload version — see the two call sites in
--   "Engine.Scripting.Lua.API.Save" and
--   "World.Thread.Command.Save.WriteWorld") — an id with no entry
--   reports version 0 rather than crashing, since a genuinely unknown
--   component id is itself surfaced by the registry-static checks
--   elsewhere, not by this diagnostic.
luaReferenceErrors
    ∷ HM.HashMap Text Word32 → KnownEntities → [LuaRefEdge] → [IntegrityError]
luaReferenceErrors componentVersions ke edges =
    [ IntegrityError
        { ieComponent = ComponentId ("lua." <> lreComponent e)
        , ieVersion = HM.lookupDefault 0 (lreComponent e) componentVersions
        , iePath = path
        , ieRefKind = luaKind (lreKind e)
        , ieRefValue = refValue
        , ieExpectedScope = if lreKind e ≡ "location_instance"
                              then locationScopeText e
                              else scopeText (lreKind e)
        , ieActual = "not found in the loaded session"
        , ieCode = code
        , ieMessage = "lua component '" <> lreComponent e <> "' " <> path
            <> " references " <> lreKind e <> " " <> refValue
            <> " which does not resolve (tolerated: cleared at reconcile time)"
        }
    | e ← edges
    , not (luaEdgeResolves ke e)
    , let code = if luaEdgeExceedsAllocator ke e
                   then "ref-exceeds-allocator" else "dangling-reference"
    , let path = if T.null (lrePath e)
                   then lreKind e <> "#" <> tshow (lreId e)
                   else lrePath e
    -- A per-page id alone is not a usable diagnostic value for a kind
    -- whose durable identity is (page, id) — a bare "3" names nothing
    -- an operator could look up. #915's location memories therefore
    -- report "page=<pid>,id=<n>", so both halves of the identity appear
    -- in the rendered finding, not only the path.
    , let refValue = case (lreKind e, lrePage e) of
            ("location_instance", Just page) →
                "page=" <> page <> ",id=" <> tshow (lreId e)
            _ → tshow (lreId e)
    ]
  where
    luaKind k = case k of
        "unit"              → RefUnit
        "building"          → RefBuilding
        "craft_bill"        → RefBill
        "item_instance"     → RefItemInstance
        "ground_item"       → RefGroundItem
        "location_instance" → RefLocationInstance
        _                   → RefUnit
    -- craft_bill/ground_item are PER-PAGE allocators, resolved against
    -- the owning unit's page specifically (see 'luaEdgeResolves') — the
    -- expected scope text says so, rather than claiming "global" for a
    -- reference that was never checked session-wide. location_instance
    -- is per-page too, but resolved against the page the EDGE declares,
    -- so its text names that page when the edge carries one.
    scopeText k = case k of
        "craft_bill"  → "owning unit's page (per-page allocator)"
        "ground_item" → "owning unit's page (per-page allocator)"
        _             → "global (session-wide allocator)"
    locationScopeText e = case lrePage e of
        Just page → "world page '" <> page <> "' (per-page allocator)"
        Nothing   → "the reference's own declared world page \
                     \(per-page allocator; none declared)"

-- | The @world-pages@ schema version every diagnostic above attributes
--   itself to, read off the codec that actually writes the component
--   rather than restated as a literal. Two consecutive bumps (#2021's
--   v9, #917's v10) each had to hand-edit five literals here that
--   nothing tied to the codec, and the second one missed them.
worldPagesVersion ∷ Word32
worldPagesVersion = ccVersion worldPagesCodec
