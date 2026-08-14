# Unified player-managed item transfers design

This document is the durable design authority for one transfer system that
behaves identically no matter which UI gesture reaches it. It transcribes the
design signed off on epic #1013, corrected against what the landed foundations
actually shipped, and gives the arc a processing cursor it never had — its
remaining six slices existed only as an unnumbered work order in the epic body.

Design state: `ready for issue processing`

> **2026-08-11 — D-9 amended, D-12/D-13 added, and the arc split from 6 slices
> to 12.** The one-window rule is now per nesting LEVEL rather than global.
> UIT-1 → 1A/1B/1C, UIT-2 → 2A/2B/2C, UIT-3 → 3A/3B, UIT-5 → 5A/5B; UIT-4 and
> UIT-6 are single concerns and were left whole.
>
> Two findings drove the shape. **The transfer core is an unwired policy
> library** — `QueuedTransfer`, `TransferBatch` and `TransferScene` exist only in
> `src/Unit/Transfer.hs` and its Lua wrapper, nothing stores them, and no save
> component carries them — so Mode B needs order state, persistence, an executor
> and a gesture, which are four separable things rather than one. And **UIT-5's
> vocabulary already exists** (`TransferCancelled`, `TransferFailed`, all ten
> `TransferReason` values), so it splits by MODE rather than by failure kind:
> splitting by kind would have produced slices that all edit the same two files.
>
> Two slices now need no predecessor at all — **UIT-3A** (fixes landed code) and
> **UIT-2A** (engine-side, independent of the window) — so three slices are
> workable in parallel with UIT-1A rather than one.

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [x] EPIC. Unified player-managed item transfers — [#1013]
- [x] UIT-1A. Generalize the container window to any endpoint kind — [#1234]
- [x] UIT-1B. Render last-known container contents with an age indicator — [#1237]
- [x] UIT-1C. Add the nested container-window stack — [#1238]
- [x] UIT-3A. Select the nearest of several units instead of requiring exactly one — [#1239]
- [x] UIT-2A. Give transfer orders durable state and persistence — [#1246]
- [x] UIT-2B. Execute a transfer order as a unit job that commits on arrival — [#1247]
- [x] UIT-2C. Promote "Store in cargo" to a queued order-at-a-distance — [#1249]
- [x] UIT-3B. Add the escort transfer session — [#1250]
- [x] UIT-4. Extend escort transfers to unit-to-unit two-sided holds — [#1251]
- [x] UIT-5A. Handle Mode B order failures — [#1253]
- [x] UIT-5B. Handle Mode A session failures — [#1254]
- [x] UIT-6. Gate the unified transfer system end to end — [#1255]

**Slice-ID mapping.** Epic #1013's work order names these C1, C2, C3, C4, C5 and
D1. This document renumbers them `UIT-1` … `UIT-6` because a slice called `D1`
would collide with the `D-N` decision identifiers the document contract uses.
The mapping is C1→UIT-1A/1B/1C, C2→UIT-2A/2B/2C, C3→UIT-3A/3B, C4→UIT-4,
C5→UIT-5A/5B, D1→UIT-6.

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
- **B1's single-unit rule is gone (UIT-3A, #1239, landed).**
  `scripts/transfer_session.lua`'s `resolveSource` now implements D-8's
  nearest-of-N: a multi-unit selection is allowed, the nearest eligible
  candidate to the endpoint's own `gridX`/`gridY` becomes the source, exact
  distance ties break on lowest uid, and distance is measured in the target's
  local u-alias frame (`world.localizeTile`, #1175). Zero eligible candidates
  still omits "Transfer" rather than disabling it. The comments in
  `transfer_session.lua` and both `init_context_menu.lua` call sites that
  asserted the single-unit rule as intended were corrected with it. See D-11
  for why that comment was misleading rather than authoritative.

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

- A container-window manager that renders any endpoint kind, enforces one window
  per nesting level, and distinguishes stale from live.
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

### D-9. One container window per nesting level

As filed on #1013 this read *"one container window at a time, broken only by a
Mode A session, which owns two panels."* **Amended 2026-08-11:** the rule is per
nesting **level**, not global.

- Opening a container that lives **inside** an open container pushes a new
  level rather than replacing the current one; the nesting path is remembered.
- Two container windows at the **same** level may never be shown together.
- When a deeper level opens, the shallower one stays **visible but
  unclickable**, and closing the deeper level returns interactivity to it.

*Consequence:* UIT-1 owns a window **stack** with a remembered nesting path, not
a single window slot.

*This is existing machinery, not new work.* It is #742's modal boundary:
`UI.setPageInputExclusive` is registered, and a `LayerModal` page defaults to
input-exclusive, so a child level simply KEEPS that default and the parent below
it becomes non-interactive while still painting. `scripts/popup.lua:796-801` is
the inverse precedent — it explicitly opts OUT because notification cards are
stacking-only. Container levels want the default.

Mode A's two flanking panels remain the one exception: they are a single level
owning two panels.

### D-12. Generalize `cargo_inventory_panel`, do not add a fourth panel

`scripts/cargo_inventory_panel.lua` already is a container-window manager in
everything but name — `openFor(bid, mx, my)`, `reopenWithTab`, `closeIfOpen`,
`isOpen`, `showRowMenu(item)` — just hardwired to `BuildingId`. UIT-1
generalizes it to any endpoint kind rather than building a new manager beside
it.

*Rationale:* #1013's Vision states it directly — *"where a near-identical
implementation already exists, it is generalized rather than copied."*

*Consequence:* the diff lands in the live building-storage UI, so a regression
is immediately player-visible and UIT-1's acceptance must prove the existing
building path still behaves.

### D-13. Fold `item_contents_panel.lua` into the window manager in UIT-1

Item-container contents are rendered by the same window manager from the start,
rather than left in a separate panel until the portable-container arc needs them.

> **2026-08-12 (#1234):** the absorption lands in **UIT-1C**, not UIT-1A. The
> 6→12 slice split above put the nested window stack and modal exclusivity in
> UIT-1C, and folding a second panel into the manager is that same concern:
> UIT-1A generalizes the manager's own API from a building id to an endpoint
> identity and explicitly leaves `scripts/item_contents_panel.lua` untouched, so
> a regression in either half stays attributable. The decision itself is
> unchanged — the panel is still folded in during UIT-1, and item-containers
> still become renderable levels without becoming endpoints. Only the slice that
> does it moved. UIT-1C's own delivery entry already records this.

*Rationale:* D-5 already says the widget renders item-container contents "so
`item_contents_panel.lua` collapses into it", and leaving it separate would ship
a D-9 that is demonstrably false — today `cargo_inventory_panel` and
`item_contents_panel` are independent modules with independent lifecycles and
can both be open at once.

*Consequence:* UIT-1 touches a HUD entry point (`scripts/hud.lua:228`), and
item-containers become renderable levels in the stack **without** becoming
transfer endpoints — D-5's exclusion is unchanged.

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

### UIT-1A. Generalize the container window to any endpoint kind

- **Outcome:** One window manager opens for a building or a unit endpoint, with
  the existing building-storage experience unchanged.
- **Scope:** Generalize `scripts/cargo_inventory_panel.lua` from `BuildingId` to
  an endpoint identity (D-12) — `openFor`, `reopenWithTab`, `closeIfOpen`,
  `isOpen`, `showRowMenu` all become endpoint-kind agnostic; unit endpoints read
  through `unit.transferEndpointInfo`; placement stays on `UI.placePopup` and
  `reserved_regions`.
- **Phase:** 1 — window foundation
- **Depends on:** `none` (C0 #1088 and A3 #1087 both landed)
- **Ordering:** `can land first`
- **Relevant decisions:** D-5, D-12
- **Acceptance signals:** A building endpoint renders exactly as it does today,
  including tabs, capacity and stored weight; a unit endpoint renders through the
  same manager and widget; no fourth panel is introduced; and contents are still
  read live, so this slice changes structure without changing what the player
  sees for a building.
- **Out of scope:** Stale contents and the age indicator (UIT-1B), nesting
  (UIT-1C), and any transfer gesture.
- **Open questions:** None

### UIT-1B. Render last-known container contents with an age indicator

- **Outcome:** A container shows what the player last observed, with an "as of…"
  age, instead of live truth.
- **Scope:** Consume `building.getContainerKnowledge` — its `state`, `items`,
  `storedWeight` and `revealedAt` — as the window's contents source; render
  never-inspected distinctly from known-empty; keep capacity live and always
  known; and apply the reveal rule so a completed movement by a player-
  commandable unit refreshes the snapshot while proximity alone does not.
- **Phase:** 1 — window foundation
- **Depends on:** `UIT-1A`
- **Ordering:** `critical path` — UIT-1C consumes its knowledge rendering
  (amended 2026-08-11)
- **Relevant decisions:** D-2, D-5
- **Acceptance signals:** A never-inspected container reads as such and never as
  empty; a known-empty one reads as empty; capacity always shows even when
  contents are unknown; the age is derived from `revealedAt` on the same game
  clock `unit.getInfo`'s `animStart` uses; walking past does not reveal; a
  completed movement does; and a unit endpoint still reports live, since a unit
  knows its own contents.
- **Out of scope:** Any write to the knowledge layer beyond what A3 already
  records, per-unit knowledge, and nesting.
- **Open questions:** None

### UIT-1C. Add the nested container-window stack

> **2026-08-11 — filed as [#1238].** Amended at processing time with user
> signoff: building-side nesting included (remembered nested contents from the
> parent's snapshot, read-only engine surface), same-level rule is
> replace-not-refuse, and the slice now depends on UIT-1B as well as UIT-1A.

- **Outcome:** Opening a container inside an open container pushes a level, and
  only the deepest level is interactive.
- **Scope:** A remembered nesting stack (D-9); a deeper level input-exclusive on
  `LayerModal` with shallower levels painted but unclickable; closing a level
  restoring its parent's interactivity; replacing on a second open at the SAME
  level (opening B closes A and any deeper levels); absorbing
  `scripts/item_contents_panel.lua` with its
  `scripts/hud.lua:228` entry point so an item-container is a level rather than a
  rival window (D-13); and exposing a building-stored container's remembered
  nested contents (read-only) so it opens as a level from the parent's own
  knowledge snapshot.
- **Phase:** 1 — window foundation
- **Depends on:** `UIT-1A`, `UIT-1B`
- **Ordering:** `critical path`
- **Relevant decisions:** D-5, D-9, D-13
- **Acceptance signals:** A second window at the same level cannot open; opening
  a nested container leaves the parent visible and unclickable; closing it
  restores the parent; the nesting path is remembered across levels;
  `item_contents_panel.lua` no longer owns a window lifecycle; and an
  item-container renders as a level without becoming a transfer endpoint (D-5
  unchanged).
- **Out of scope:** Mode A's two flanking panels, which are one level owning two
  panels; any transfer gesture.
- **Open questions:** None

### UIT-3A. Select the nearest of several units instead of requiring exactly one

> **2026-08-11 — filed as [#1239].** Amended at processing time with user
> signoff: distance is measured seam-aware in the target's local alias frame
> (`world.localizeTile`), per the #1175 selection-gate rule.

- **Outcome:** The existing Transfer gesture accepts a multi-unit selection and
  sends the nearest, breaking exact ties on lowest uid.
- **Scope:** Replace `scripts/transfer_session.lua`'s
  `#selectedUids ~= 1` rule with nearest-of-N plus the lowest-uid tiebreak, and
  correct the comment at `:155-156` that currently asserts the single-unit
  behavior as deliberate.
- **Phase:** 2 — selection policy
- **Depends on:** `none`
- **Ordering:** `not on the critical path` — fixes landed code and can land at
  any time
- **Relevant decisions:** D-8, D-11
- **Acceptance signals:** A multi-unit selection offers Transfer where it is
  currently omitted; the nearest selected unit is chosen; an exact distance tie
  resolves to the lowest uid deterministically; a self-transfer is still
  excluded; and the misleading comment no longer contradicts D-8.
- **Out of scope:** The escort session itself, and any change to what happens
  after the source is resolved.
- **Open questions:** None

### UIT-2A. Give transfer orders durable state and persistence

> **2026-08-11 — filed as [#1246].** Amended at processing time with user
> signoff: the save component is OPTIONAL (absent = "no orders queued"), the
> second optional component after `container-knowledge`.

- **Outcome:** A queued transfer order exists as live state and survives
  save/load.
- **Scope:** A live owner for `QueuedTransfer`/`TransferBatch`, which today
  exist only as types in `src/Unit/Transfer.hs` with nothing storing them; a new
  save component for them following `docs/persistence_contract.md`'s
  component-version and frozen-DTO rules; and the reset behavior that keeps a
  Mode A session transient while a Mode B order persists (D-3).
- **Phase:** 3 — order foundation
- **Depends on:** `none`
- **Ordering:** `can land first` — engine-side, independent of the window
- **Relevant decisions:** D-1, D-3, D-10
- **Acceptance signals:** An order created programmatically survives a save/load
  round trip with its state, endpoints and exact instance ids intact; a Mode A
  session does not survive; the component is absent-tolerant or required by an
  explicit, documented choice; and the persistence-inventory and
  save-compatibility audits pass.
- **Out of scope:** Anything that creates or executes an order — no unit job, no
  player gesture, no UI.
- **Open questions:** None

### UIT-2B. Execute a transfer order as a unit job that commits on arrival

- **Outcome:** A queued order makes its unit walk to the endpoint and commit
  once, revalidated on arrival.
- **Scope:** The unit job that drives an order through `queued → in_transit →
  ready_to_commit → completed`, modeled on #920's `commandPickup`: capacity
  gated at command time AND again on arrival, and a stall timeout that resets on
  closest approach rather than a total-trip budget. Commit-time revalidation and
  the partial-batch outcome are A1/A2's and are consumed, not rebuilt. Creating
  an order is range-INDEPENDENT (its endpoints are not adjacent yet by
  definition) while preserving `unit.checkTransfer`/`unit.commitTransfer`
  unchanged. Also the BASELINE self-termination and durable bookkeeping for the
  two ways an order provably cannot finish: a carrier that stalls short of a
  counterpart still present terminalizes its pending entries as
  `failed/out_of_range`, and a counterpart that vanished as
  `failed/became_stale` with `source_missing`/`receiver_missing` as its cause
  and no player warning.
- **Phase:** 3 — order foundation
- **Depends on:** `UIT-2A`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-3, D-7
- **Acceptance signals:** An order drives a real walk and commits exactly once on
  arrival; the wander tick cannot take the unit away mid-order; twelve items into
  room for eight commits eight and reports the rest; a snapshot that went stale
  fails as `ReasonBecameStale`; and the lax AI verbs are untouched.
- **Out of scope:** The player gesture that creates an order; explicit
  cancellation, pruning of terminal orders, and any failure surfacing richer
  than the warnings above — all UIT-5A's.
- **Open questions:** None

### UIT-2C. Promote "Store in cargo" to a queued order-at-a-distance

> **2026-08-11 — filed as [#1249].** Amended at processing time with user
> signoff: Mode B row menus ship 1-and-all batch granularity; the fuller
> 1/N/all menu remains Mode A's (UIT-3B).

- **Outcome:** The player right-clicks an item row with a unit selected and gets
  a queued, persisted order instead of an immediate lax deposit.
- **Scope:** Repoint `scripts/unit_info_v2_context_menu.lua:234`'s "Store in
  \<cargo\>" from "each adjacent built cargo" to "the open container window",
  and its action from `unit.depositToCargo` to a queued order — which also drops
  its adjacency requirement; retire "Withdraw with \<unit\>"
  (`scripts/cargo_inventory_panel.lua:436`) in favour of retrieve; and the
  right-click hooks on container rows.
- **Phase:** 3 — order foundation
- **Depends on:** `UIT-1A`, `UIT-2B`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-7, D-9
- **Acceptance signals:** The gesture creates a persisted order rather than a lax
  deposit; it works without adjacency; both retired paths are gone and their
  replacements reachable; and the lax AI verbs still exist untouched for the AI
  ladders that depend on them.
- **Out of scope:** The escort session, and Mode B failure handling.
- **Open questions:** None

### UIT-3B. Add the escort transfer session

> **2026-08-11 — filed as [#1250].** Amended at processing time with user
> signoff: session row menus ship 1-and-all (the epic's "Store N" is
> deferred); unit destinations open sessions source-held only until UIT-4;
> and UIT-1C joins the dependencies — the session level integrates with the
> real window stack.

- **Outcome:** Right-clicking a container with units selected walks the nearest
  over and opens two flanking panels that commit immediately while adjacent.
- **Scope:** The escort session lifecycle; camera snap centring the pair; two
  mutually-avoiding framebuffer-clamped panels via `UI.placePopup` and
  `reserved_regions`; coupled close that releases the unit; and the unit hold.
- **Phase:** 4 — escort
- **Depends on:** `UIT-1A`, `UIT-1C`, `UIT-2C`, `UIT-3A`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-4, D-9
- **Acceptance signals:** The nearest selected unit walks and is held; two panels
  open flanking the pair without overlapping; closing either closes both and
  releases the unit; the camera snaps on Mode A only; the session does not
  persist; and the two panels count as ONE nesting level (D-9's stated
  exception).
- **Out of scope:** Unit-to-unit two-sided holds, and session failure handling.
- **Open questions:** None

### UIT-4. Extend escort transfers to unit-to-unit two-sided holds

> **2026-08-11 — filed as [#1251].** Amended at processing time with user
> signoff: the target's hold begins at session creation — it stops and waits
> during the source's approach, preempting autonomous work like any player
> order.

- **Outcome:** Acolyte-to-acolyte and mule-to-acolyte escort transfers work, with
  both endpoints held.
- **Scope:** The two-sided hold and its release, since unit-to-unit is the only
  case where **both** endpoints can walk away.
- **Phase:** 5 — unit-to-unit
- **Depends on:** `UIT-3B`
- **Ordering:** `critical path`
- **Relevant decisions:** D-6, D-8, D-10
- **Acceptance signals:** Both units hold for the session and both release on
  close; equipped and accessory items remain non-transferable; and every
  direction combination commits under the same policy.
- **Out of scope:** Multi-unit transfer to one receiver in a single session.
- **Open questions:** None

### UIT-5A. Handle Mode B order failures

> **2026-08-11 — filed as [#1253].** Amended at processing time with user
> signoff: explicit cancellation lives on the unit context menu, and terminal
> orders are pruned from the persisted store once their outcome is surfaced.

- **Outcome:** Every way a queued order can fail resolves predictably, leaving no
  half-moved item and no unit stuck on a dead order.
- **Scope:** Explicit cancellation of a queued order; pruning terminal orders
  from the persisted store once their outcome is surfaced; and richer policy and
  surfacing on top of UIT-2B's baseline — which already self-terminates an
  unreachable endpoint and a vanished counterpart, records the structured reason
  and cause, and warns on the reachable-but-unreached case. What is left here is
  everything beyond that floor: a retry or re-route policy for an obstructed
  endpoint, and how a failed order is presented rather than merely logged. The
  states and reasons already exist — `TransferCancelled`, `TransferFailed`, and
  all ten `TransferReason` values — so this slice handles and surfaces them
  rather than inventing vocabulary.
- **Phase:** 6 — failure handling
- **Depends on:** `UIT-2B`, `UIT-2C`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3
- **Acceptance signals:** A cancelled order releases its unit and mutates
  nothing; a terminal order is pruned once its outcome has been surfaced; and
  the failures UIT-2B already terminalises are presented to the player rather
  than only recorded. (That an unreachable endpoint gives up rather than looping,
  that a stale instance fails with the structured reason and cause, and that no
  failure path leaves an item half-moved are UIT-2B's own gates.)
- **Out of scope:** Session failures (UIT-5B) and new failure vocabulary.
- **Open questions:** None

### UIT-5B. Handle Mode A session failures

> **2026-08-11 — filed as [#1254].** Amended at processing time with user
> signoff: a new player command to a held unit ends the session cleanly and
> proceeds — player intent wins.

- **Outcome:** An escort session that is interrupted ends cleanly, releasing
  every held unit.
- **Scope:** An endpoint that disappears or dies mid-session; a held unit that
  becomes uncommandable; interruption by another gesture; and the coupled close
  path under each, including the two-sided hold from UIT-4.
- **Phase:** 6 — failure handling
- **Depends on:** `UIT-3B`, `UIT-4`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-9
- **Acceptance signals:** No interruption leaves a unit held indefinitely or a
  panel orphaned; both holds release in the unit-to-unit case; and the nesting
  stack is left consistent after an abnormal close.
- **Out of scope:** Queued-order failures (UIT-5A).
- **Open questions:** None

### UIT-6. Gate the unified transfer system end to end

- **Outcome:** One scenario proves both modes, both endpoint kinds, both
  directions, batches, knowledge staleness, and persistence.
- **Scope:** Acceptance coverage across the arc, probe registration, and the
  load-bearing documentation updates the retirements imply.
- **Phase:** 6 — integration gate
- **Depends on:** `UIT-5A`, `UIT-5B`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1 through D-13
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
