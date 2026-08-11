# Unified player-managed item transfers design

This document is the durable design authority for one transfer system that
behaves identically no matter which UI gesture reaches it. It transcribes the
design signed off on epic #1013, corrected against what the landed foundations
actually shipped, and gives the arc a processing cursor it never had — its
remaining six slices existed only as an unnumbered work order in the epic body.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [x] EPIC. Unified player-managed item transfers — [#1013]
- [ ] UIT-1. Add the container-window manager for any endpoint kind
- [ ] UIT-2. Add Mode B, the persisted order-at-a-distance transfer
- [ ] UIT-3. Add Mode A, the escort transfer session
- [ ] UIT-4. Extend escort transfers to unit-to-unit two-sided holds
- [ ] UIT-5. Handle cancellation, obstruction, stale items, and failed commits
- [ ] UIT-6. Gate the unified transfer system end to end

**Slice-ID mapping.** Epic #1013's work order names these C1, C2, C3, C4, C5 and
D1. This document renumbers them `UIT-1` … `UIT-6` because a slice called `D1`
would collide with the `D-N` decision identifiers the document contract uses.
The mapping is C1→UIT-1, C2→UIT-2, C3→UIT-3, C4→UIT-4, C5→UIT-5, D1→UIT-6.

## Epic contract

- **Goal:** One transfer system that looks and behaves identically no matter
  which UI gesture reaches it, with every container type interchangeable as an
  endpoint.
- **Done when:** A player can move an exact item instance in either direction
  between any two of an acolyte, a technomule, and a built storage building;
  both interaction modes reach the same commit policy; batches partially succeed
  with a report; container contents display as last-known and persist; Mode B
  orders survive save/load while a Mode A session does not; one widget renders
  every container view; and the existing AI fetch, deliver, repair, medic and
  expedition transfer paths remain green.
- **Users and operators:** Players organizing colony and expedition inventory;
  maintainers of transfer policy, container knowledge, and the shared item-list
  widget.
- **Arc label:** `ui`

## Current state and evidence

The foundations landed; the interaction layer did not, and was never filed.

- **A1 (#1000, closed)** established the queued transaction foundation: the
  `TransferScene` projection, create-time and commit-time revalidation, the
  structured `TransferReason` vocabulary, order-preserving rollback via
  `tpIndex`, the recursive `itemTotalWeight` capacity measure, and the
  footprint-aware Chebyshev ≤ 1 proximity rule.
- **A2 (#1085, closed)** applied all five of its planned corrections to A1,
  verified at HEAD: `trSource ∷ TransferEndpoint` gives both ends one endpoint
  type (`src/Unit/Transfer.hs:158`); `trQuantity` is gone entirely, replaced by
  instance sets; the `transfer_receiver` data marker is deleted and eligibility
  is `faction.isPlayerCommandable`; `TransferBatch` (`:301`) carries a
  partial-batch outcome; and the endpoint-info verb was both enriched **and
  renamed** — it is now `unit.transferEndpointInfo`
  (`Engine/Scripting/Lua/API/Register/Unit.hs:73`), not
  `unit.transferReceiverInfo`.
- **A3 (#1087, closed)** added the player-global stale container-knowledge layer
  as the first OPTIONAL save component, `container-knowledge`, keyed by
  `BuildingId` (`World/Save/Component/Knowledge.hs`). An absent payload reads as
  never-inspected, never as known-empty.
- **C0 (#1088, closed)** extracted `scripts/ui/item_list.lua` from the three
  duplicated panels, which shrank from 792/499/435 lines to **506/398/185**.
  `scripts/item_contents_panel.lua` now `require`s the widget rather than
  carrying its own rows, and `scripts/cargo_inventory_panel.lua:399` confirms its
  tab strip moved to the shared `scripts/ui/tabbar`. `truncateToWidth`
  consolidation was only partly in C0's scope — `item_list.lua:172` delegates to
  the shared `textWrap.truncateToWidth`, and open #1157 explicitly sequences
  itself against #1088 to own the remaining copies.
- **Nothing after C0 exists.** C1 through C5 and D1 are named in #1013's work
  order without issue numbers, and no open issue covers them. The epic therefore
  reads as stalled when it is merely unfiled.
- **The player-facing paths those slices retire are still live**, correctly:
  "Store in \<cargo\>" (`scripts/unit_info_v2_context_menu.lua:234`) and
  "Withdraw with \<unit\>" (`scripts/cargo_inventory_panel.lua:436`). #1013
  removes each only once its replacement exists.
- **B1's single-unit rule is unfinished work that the code now defends.**
  `scripts/transfer_session.lua:158` still reads
  `if not selectedUids or #selectedUids ~= 1 then return nil end`, and the
  comment at `:155-156` asserts that behavior as intended — *"Transfer is
  OMITTED rather than disabled for a multi-unit selection."* That directly
  contradicts D-8, which #1013 signed off and which explicitly supersedes it.
  See D-11.

## Desired experience

Every container type is interchangeable: an acolyte, a technomule, and a built
storage building are all just endpoints, and every combination works in both
directions. Where a near-identical implementation already exists, it is
generalized rather than copied.

The workflow splits into two genuinely different interaction modes that share
one commit policy:

| | **Mode A — escort** | **Mode B — order at a distance** |
|---|---|---|
| Entry | right-click a container or friendly unit → **Transfer** | a container window is open and a unit is selected → right-click an item row |
| Order of events | walk **first**, then choose items | choose items **first**, then walk |
| Panels | two, flanking the pair, mutually avoiding | one floating container window + the existing unit-info HUD panel |
| Commit | immediate, repeatedly, while adjacent | once, on arrival |
| The unit is | held at the container until a window closes | free — this is a job, like construct or pickup |
| Camera | snaps to centre the pair | untouched |
| Persisted | no (transient session) | yes (a durable order) |

Mode B is A1's `QueuedTransfer` lifecycle (`queued → in_transit →
ready_to_commit → completed`, revalidated at commit because the world moved
during the walk). Mode A is its degenerate case: the unit is already adjacent,
so each request is created and committed in the same instant. One policy, two
schedules.

## Scope

### In scope

- A container-window manager that renders any endpoint kind, enforces
  one-window-at-a-time, and distinguishes stale from live.
- Both interaction modes over the shared commit policy.
- Batch operations (1 / N / all) in both directions, partially succeeding with a
  report.
- Unit-to-unit transfers, including the two-sided hold that only that case needs.
- Cancellation, obstruction, stale items, and failed commits.
- Retiring the two superseded player-facing paths once their replacements exist.

### Out of scope

- Drag-and-drop transfer surfaces (the session entry point stays reusable).
- Generalized logistics routing, hauling jobs, or stockpile zones.
- Ground piles and item-containers as transfer **endpoints** — rendering only
  (D-5). Portable item-containers become endpoints in the separate
  portable-loot-containers arc, whose PLC-7 depends on UIT-1 and UIT-2.
- Storage-capacity balancing.
- Per-unit knowledge of container contents — knowledge is player-global (D-2).
- Changing AI transfer policy or the lax AI verbs (D-7).
- Multi-unit transfer to one receiver in a single session.

## Design

### Container knowledge

The player sees a container's **last-known** contents, not a live read — the
same cartographic-versus-experiential split as #911/#915 for locations, but
deliberately simpler because knowledge here is player-global rather than
per-unit.

- A container the player has never interacted with reads as never-inspected, not
  as empty.
- **What reveals contents:** a completed item movement into or out of the
  container by a player-commandable unit — either mode, including the AI's own
  fetch/deliver/logistics hauling — or a Mode A session opening on it.
- **Proximity alone does not reveal.** Walking past a sealed crate does not
  inventory it.
- **Capacity is always known** (the player built it, or it is the unit's own
  stat). Only contents and stored weight go stale.
- Knowledge persists. A contents cache that reset on load would be worse than
  none.
- **Buildings only.** Unit endpoints always report live: a unit knows what it is
  carrying, and only friendly units are reachable as endpoints anyway. The
  governing rule is *an entity knows its own contents; a container must be
  inspected.*

Stale knowledge and A1's commit-time revalidation compose for free: a Mode B
"retrieve the hammer" issued against a snapshot where the hammer is gone fails
as `ReasonBecameStale` carrying cause `ReasonInstanceMissing`.

### What retires, and when

Both are player-facing paths superseded by the unified system, and each is
removed only once its replacement exists:

- **"Store in \<cargo\>"** (`scripts/unit_info_v2_context_menu.lua:234`) — one
  entry per adjacent built cargo, calling the lax `unit.depositToCargo`
  immediately. It is **promoted rather than deleted**: same gesture, same menu
  location, but its target resolution changes from "each adjacent built cargo"
  to "the open container window", and its action from an immediate lax deposit
  to a queued, contract-checked, persisted order. That is Mode B, and it drops
  the adjacency requirement.
- **"Withdraw with \<unit\>"** (`scripts/cargo_inventory_panel.lua:436`, calling
  `unit.withdrawFromCargo`) — superseded by retrieve.

### Infrastructure to build on

`UI.placePopup` is the one placement algorithm for floating content, and
`scripts/ui/reserved_regions.lua` (`avoidReserved`, `findEscapes`,
`maxAvailableWidth`) already keeps popups clear of reserved rectangles —
panel-versus-panel avoidance is that same machinery with the sibling panel as
the reserved rect.

`item_contents_panel.lua`'s coarser group-by-defName rule is **deliberate** and
documented at `src/Engine/Scripting/Lua/API/Units/Inventory.hs:490-497`; the
shared widget must not force the finer stack key onto it.

The AI order in UIT-2 should copy #920's `commandPickup` shape: a durable order,
capacity gated at command time **and** again on arrival, and a stall timeout
that resets on closest approach rather than a total-trip budget. Without a real
AI order the wander tick walks the unit away mid-transfer.

## Decisions

D-1 through D-10 are transcribed verbatim in substance from epic #1013's
"Signed-off design decisions", which states: *do not re-litigate these when
drafting children.*

### D-1. Partial batches

Twelve items into a hold with room for eight stores eight and reports what did
not fit. All-or-nothing would force the player to count and re-select, and A1's
per-request atomicity already provides the guarantee that matters: no single
item ever half-moves.

### D-2. Contents are genuinely stale

Refreshed by the reveal rule in the Design section — not a live read that merely
skips an adjacency check. Knowledge is player-global, not per-unit.

### D-3. All orders persist

Mode B orders survive save/load. The Mode A session stays transient; it already
registers a reset hook.

### D-4. Camera snap on Mode A only

Never on a Mode B order or a plain container inspect. This may change later; it
is not load-bearing.

### D-5. Endpoint scope is building storage and unit inventory

The unified widget additionally *renders* item-container contents (so
`item_contents_panel.lua` collapses into it) and could later render ground
piles, but neither is a transfer endpoint in this arc.

### D-6. Equipped and accessory items stay non-transferable

The player unequips first, as A1 has it.

### D-7. The lax AI verbs survive untouched

`unit.transferItemToUnit`, `unit.transferItemToBuilding`,
`unit.depositToCargo`, and `unit.withdrawFromCargo` keep their documented laxity
(no capacity check unit-to-unit, no adjacency check at all) because the AI
fetch, deliver, repair, and medic ladders depend on exactly that — see
`Unit.Transfer`'s module header. Only the *player-facing* paths retire.

### D-8. Nearest-of-N, with a tiebreak

Multiple selected units are allowed; the nearest goes. Exact distance ties break
on lowest uid so two equidistant acolytes cannot race.

### D-9. One container window at a time

Broken only by a Mode A session, which owns two panels.

### D-10. Building-to-building is expressible in the contract

Even though no UI gesture in this arc reaches it. A symmetric endpoint model
costs less code than one that carves the case out, and the exclusion would have
to be re-litigated the moment a hauling or stockpile feature wants it. Children
must not add a special-case refusal for it.

### D-11. B1's single-unit rule is unfinished work, not settled intent

`scripts/transfer_session.lua:158` still hard-requires exactly one selected unit,
and the comment at `:155-156` asserts that as deliberate. **D-8 supersedes both.**
UIT-3 replaces the rule with nearest-of-N plus the lowest-uid tiebreak and
corrects that comment.

*Rationale:* D-8 was signed off with a specific failure in mind — two
equidistant acolytes racing — and #920's `commandPickup` already establishes
nearest-of-N elsewhere in the codebase, so the pattern is not novel here.

*Consequence:* the stale comment is actively misleading. A solver reading only
the code would conclude the single-unit rule is intended and leave it. Correcting
it is part of UIT-3, not optional cleanup.

## Open questions

None currently blocking. Every decision above carries prior signoff from epic
#1013 except D-11, which was signed off on 2026-08-11.

## Verification strategy

- Preserve the focused pure coverage A1 and A2 established for the transfer
  contract: endpoint symmetry, instance-set requests, create-time and
  commit-time revalidation, `tpIndex` order-preserving rollback, and the
  recursive `itemTotalWeight` capacity measure.
- Extend the container-knowledge coverage from A3 for the window manager's
  stale-versus-live rendering and the reveal rule, including that proximity
  alone does not reveal.
- Verify Mode B orders survive save/load and a Mode A session does not, through
  the repository's persistence inventory and save-compatibility gates.
- Verify the partial-batch contract at the boundary: twelve into room for eight
  stores eight, reports the remainder, and half-moves nothing.
- Verify nearest-of-N selection and the lowest-uid tiebreak deterministically,
  since D-11 replaces a rule the current code documents as intentional.
- Keep the existing AI fetch, deliver, repair, medic, and expedition transfer
  probes green — D-7 means none of their verbs change.
- Use offscreen/manual verification only for the panel geometry, camera snap,
  and coupled-close behavior that cannot be established headlessly.

## Delivery plan

### UIT-1. Add the container-window manager for any endpoint kind

- **Outcome:** One floating container window renders any endpoint kind, enforces
  one-at-a-time, and shows last-known versus live contents honestly.
- **Scope:** The window manager over `scripts/ui/item_list.lua`; endpoint-kind
  dispatch; the one-window rule; stale-versus-live presentation with an "as of…"
  age from `container-knowledge`'s `revealedAt`; and placement through
  `UI.placePopup` and `reserved_regions`.
- **Phase:** 1 — window foundation
- **Depends on:** `none` (C0 #1088 and A3 #1087 both landed)
- **Ordering:** `can land first`
- **Relevant decisions:** D-2, D-5, D-9
- **Acceptance signals:** Every endpoint kind renders through the one widget; a
  second window cannot open; a never-inspected container reads as such rather
  than as empty; capacity always shows while contents may be stale; and
  item-container contents render without becoming a transfer endpoint.
- **Out of scope:** Any transfer gesture, order, or commit.
- **Open questions:** None

### UIT-2. Add Mode B, the persisted order-at-a-distance transfer

- **Outcome:** A player selects a unit, right-clicks an item row, and the unit
  walks over and commits once on arrival — surviving save/load.
- **Scope:** The persisted deferred store/retrieve order and its AI order,
  modeled on #920's `commandPickup`; commit-time revalidation; the partial-batch
  outcome; and promoting "Store in \<cargo\>" to this path.
- **Phase:** 2 — deferred orders
- **Depends on:** `UIT-1`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-3, D-7, D-10
- **Acceptance signals:** An order survives save/load and commits on arrival; a
  snapshot that went stale fails as `ReasonBecameStale`; twelve items into room
  for eight stores eight and reports the rest; the lax AI verbs are unchanged;
  and the wander tick cannot walk the unit away mid-order.
- **Out of scope:** The escort session, camera snap, and unit-to-unit holds.
- **Open questions:** None

### UIT-3. Add Mode A, the escort transfer session

- **Outcome:** A player right-clicks a container with units selected, the nearest
  walks over, and two flanking panels commit transfers immediately while adjacent.
- **Scope:** The escort session; camera snap; flanking mutually-avoiding panels;
  coupled close that releases the unit; **nearest-of-N with the lowest-uid
  tiebreak, replacing `transfer_session.lua`'s single-unit rule and correcting
  its comment**; and the unit hold and release.
- **Phase:** 3 — escort
- **Depends on:** `UIT-1`, `UIT-2`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-4, D-8, D-9, D-11
- **Acceptance signals:** Multiple selected units are accepted and the nearest
  goes; an exact distance tie breaks on lowest uid deterministically; closing
  either panel closes both and releases the unit; the camera snaps on Mode A only;
  the session does not persist; and `transfer_session.lua:155-158`'s rule and
  comment both reflect D-8.
- **Out of scope:** Unit-to-unit two-sided holds.
- **Open questions:** None

### UIT-4. Extend escort transfers to unit-to-unit two-sided holds

- **Outcome:** Acolyte-to-acolyte and mule-to-acolyte escort transfers work, with
  both endpoints held.
- **Scope:** The two-sided hold and its release, since unit-to-unit is the only
  case where **both** endpoints can walk away.
- **Phase:** 4 — unit-to-unit
- **Depends on:** `UIT-3`
- **Ordering:** `critical path`
- **Relevant decisions:** D-6, D-8, D-10
- **Acceptance signals:** Both units hold for the session and both release on
  close; equipped and accessory items remain non-transferable; and every
  direction combination commits under the same policy.
- **Out of scope:** Multi-unit transfer to one receiver in a single session.
- **Open questions:** None

### UIT-5. Handle cancellation, obstruction, stale items, and failed commits

- **Outcome:** Every way a transfer can fail resolves predictably, leaving no
  half-moved item and no stranded held unit.
- **Scope:** Explicit cancellation; an obstructed or unreachable endpoint; items
  that vanished between snapshot and commit; and commit failures across both
  modes and both endpoint kinds.
- **Phase:** 5 — failure handling
- **Depends on:** `UIT-2`, `UIT-3`, `UIT-4`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3
- **Acceptance signals:** A cancelled order releases its unit and mutates
  nothing; an unreachable endpoint reports rather than retrying forever; a stale
  instance fails with the structured reason and cause; and no failure path leaves
  an item half-moved or a unit held indefinitely.
- **Out of scope:** New failure vocabulary beyond A1's `TransferReason` set.
- **Open questions:** None

### UIT-6. Gate the unified transfer system end to end

- **Outcome:** One scenario proves both modes, both endpoint kinds, both
  directions, batches, knowledge staleness, and persistence.
- **Scope:** Acceptance coverage across the arc, probe registration, and the
  load-bearing documentation updates the retirements imply.
- **Phase:** 6 — integration gate
- **Depends on:** `UIT-5`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1 through D-11
- **Acceptance signals:** An exact instance moves in both directions between all
  three endpoint kinds; both modes reach the same commit policy; batches
  partially succeed with a report; contents read last-known and refresh on a
  revealing interaction; Mode B orders survive save/load while a Mode A session
  does not; one widget renders every view; and the AI fetch, deliver, repair,
  medic and expedition paths remain green.
- **Out of scope:** Balance gates and unrelated probe sweeps.
- **Open questions:** None

## Source notes

Epic #1013's body remains the origin record for D-1 through D-10 and for the
"Corrections to landed work" narrative. This document supersedes it as the
design authority and corrects four references that went stale after A2 and C0
landed: the endpoint-info verb is `unit.transferEndpointInfo` rather than
`unit.transferReceiverInfo`; the three panels are 506/398/185 lines rather than
792/499/435; "Store in \<cargo\>" is at `unit_info_v2_context_menu.lua:234`
rather than `:228`; and "Withdraw with \<unit\>" is at
`cargo_inventory_panel.lua:436` rather than `:725`.
