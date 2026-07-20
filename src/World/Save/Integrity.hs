{-# LANGUAGE Strict, UnicodeSyntax #-}
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
    , KnownEntities(..)
    , buildKnownEntities
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
import Craft.Bills (CraftBills(..), CraftBill(..), BillId(..))
import Power.Types (PowerNodes(..), PowerNode(..), PowerNodeId(..))
import World.Page.Types (WorldPageId(..))
import World.Save.Envelope.Types (ComponentId(..))
import World.Save.Component.Types
    ( craftBillsComponentId, powerNodesComponentId
    , buildingsComponentId, unitsComponentId )
import World.Save.Reference (RefKind(..), RefScope(..), refKindText)
import World.Save.Snapshot
    ( SessionSnapshot(..), PageSnapshot(..), allItemInstanceIds )
import World.Save.Types (BuildingSnapshot(..), UnitSnapshot(..))
import Item.Ground (GroundItems(..))

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
    "[" <> cidText (ieComponent e) <> " v" <> T.pack (show (ieVersion e))
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
    ++ [ T.pack (show (irOmitted report)) <> " additional integrity \
        \finding(s) omitted (see World.Save.Integrity.integrityErrorCap)"
       | irOmitted report > 0 ]

pidText ∷ WorldPageId → Text
pidText (WorldPageId t) = t

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
        , ieExpectedScope = "same page ('" <> pidText sourcePage <> "')"
        , ieActual = "found on page '" <> pidText p <> "'"
        , ieCode = "wrong-page"
        , ieMessage = refKindText kind <> " " <> val
            <> " referenced from page '" <> pidText sourcePage
            <> "' resolves only on page '" <> pidText p <> "'"
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
        , let path = "craft-bills[page=" <> pidText pid <> ",bill="
                     <> T.pack (show (unBillId bid)) <> "].station"
        , Just err ← [ refEdgeError craftBillsComponentId 2 path RefBuilding
                         ScopeSamePage pid (buildingPages (cbStation bill))
                         (T.pack (show (unBuildingId (cbStation bill)))) ]
        ]

    billClaimantErrors =
        [ err
        | (pid, page) ← HM.toList pages
        , (bid, bill) ← HM.toList (cbsBills (pgsCraftBills page))
        , Just uid ← [ cbClaimant bill ]
        , let path = "craft-bills[page=" <> pidText pid <> ",bill="
                     <> T.pack (show (unBillId bid)) <> "].claimant"
        , Just err ← [ refEdgeError craftBillsComponentId 2 path RefUnit
                         ScopeSamePage pid (unitPages uid)
                         (T.pack (show (unUnitId uid))) ]
        ]

    nodeBuildingErrors =
        [ err
        | (pid, page) ← HM.toList pages
        , (nid, node) ← HM.toList (pnsNodes (pgsPowerNodes page))
        , let path = "power-nodes[page=" <> pidText pid <> ",node="
                     <> T.pack (show (unPowerNodeId nid)) <> "].building"
        , Just err ← [ refEdgeError powerNodesComponentId 2 path RefBuilding
                         ScopeSamePage pid (buildingPages (pnBuilding node))
                         (T.pack (show (unBuildingId (pnBuilding node)))) ]
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
            , iePath = refKindText kind <> "#" <> T.pack (show val)
            , ieRefKind = kind, ieRefValue = T.pack (show val)
            , ieExpectedScope = "globally unique identity (one allocator \
                                 \for the whole session)"
            , ieActual = "present on pages " <> pagesText
            , ieCode = "duplicate-identity"
            , ieMessage = refKindText kind <> " " <> T.pack (show val)
                <> " exists on multiple pages: " <> pagesText
            }
        | (val, ps) ← HM.toList (HM.fromListWith (++)
                          [ (v, [pid]) | (pid, v) ← entries ])
        , length ps > 1
        , let pagesText = T.intercalate ", " (map pidText (L.sort ps))
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
data KnownEntities = KnownEntities
    { keUnits             ∷ !(HS.HashSet Int)
    , keBuildings         ∷ !(HS.HashSet Int)
    , keBillsByPage       ∷ !(HM.HashMap WorldPageId (HS.HashSet Int))
    , keItemInstances     ∷ !(HS.HashSet Int)
    , keGroundItemsByPage ∷ !(HM.HashMap WorldPageId (HS.HashSet Int))
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
    , keUnitPage = HM.fromList
        [ (fromIntegral (unUnitId uid), pid)
        | (pid, page) ← HM.toList (snapPages snap)
        , uid ← HM.keys (usnInstances (pgsUnits page)) ]
    , keNextUnitId     = fromIntegral (snapNextUnitId snap)
    , keNextBuildingId = fromIntegral (snapNextBuildingId snap)
    , keNextItemId     = fromIntegral (snapNextItemId snap)
    }
  where pages = HM.elems (snapPages snap)

-- | One reference a Lua save component's @references()@ hook reported —
--   the raw @{kind=.., id=..}@ shape, plus which Lua component it came
--   from (for diagnostic attribution), the OWNING unit id when the hook
--   supplied one (@lreOwner@ — every @unitAiReferences@ entry is emitted
--   from inside a per-unit loop, so it always has one;
--   @buildingSpawnReferences@ entries never need one, since its
--   "unit"/"building" kinds resolve session-wide regardless), and
--   (round-2 review, issue #764) the actual field path this edge came
--   from (@lrePath@ — e.g. @"unit[7].attackTargetUid"@,
--   @"building[12].lastUid"@), in the SAME dotted-path style
--   'refEdgeError' already uses for Haskell-side findings — see
--   @unit_ai_save_refs.lua@'s @unitAiReferences@ and
--   @building_spawn.lua@'s @buildingSpawnReferences@, which build it.
data LuaRefEdge = LuaRefEdge
    { lreComponent ∷ !Text
    , lreKind      ∷ !Text
    , lreId        ∷ !Int
    , lreOwner     ∷ !(Maybe Int)
    , lrePath      ∷ !Text
    } deriving (Show, Eq)

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
luaEdgeResolves ∷ KnownEntities → LuaRefEdge → Bool
luaEdgeResolves ke e = case lreKind e of
    "unit"          → HS.member (lreId e) (keUnits ke)
    "building"      → HS.member (lreId e) (keBuildings ke)
    "item_instance" → HS.member (lreId e) (keItemInstances ke)
    "craft_bill"    → resolvesOnOwnerPage (keBillsByPage ke)
    "ground_item"   → resolvesOnOwnerPage (keGroundItemsByPage ke)
    _               → True
  where
    resolvesOnOwnerPage byPage =
        case lreOwner e ⌦ (`HM.lookup` keUnitPage ke) of
            Nothing  → False
            Just pid → maybe False (HS.member (lreId e)) (HM.lookup pid byPage)

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
--   @componentVersions@ (round-2 review, issue #764) maps a Lua
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
        , ieRefValue = T.pack (show (lreId e))
        , ieExpectedScope = scopeText (lreKind e)
        , ieActual = "not found in the loaded session"
        , ieCode = code
        , ieMessage = "lua component '" <> lreComponent e <> "' " <> path
            <> " references " <> lreKind e <> " " <> T.pack (show (lreId e))
            <> " which does not resolve (tolerated: cleared at reconcile time)"
        }
    | e ← edges
    , not (luaEdgeResolves ke e)
    , let code = if luaEdgeExceedsAllocator ke e
                   then "ref-exceeds-allocator" else "dangling-reference"
    , let path = if T.null (lrePath e)
                   then lreKind e <> "#" <> T.pack (show (lreId e))
                   else lrePath e
    ]
  where
    luaKind k = case k of
        "unit"          → RefUnit
        "building"      → RefBuilding
        "craft_bill"    → RefBill
        "item_instance" → RefItemInstance
        "ground_item"   → RefGroundItem
        _               → RefUnit
    -- craft_bill/ground_item are PER-PAGE allocators, resolved against
    -- the owning unit's page specifically (see 'luaEdgeResolves') — the
    -- expected scope text says so, rather than claiming "global" for a
    -- reference that was never checked session-wide.
    scopeText k = case k of
        "craft_bill"  → "owning unit's page (per-page allocator)"
        "ground_item" → "owning unit's page (per-page allocator)"
        _             → "global (session-wide allocator)"
