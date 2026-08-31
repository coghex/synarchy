# Epic Review Findings: Epic #1013 — A. Unified player-managed item transfers

This report records the current review of completed epic #1013 against its
reconciled 17-child scope: #1000, #1014, #1085, #1087, #1088, #1234, #1237,
#1238, #1239, #1246, #1247, #1249, #1250, #1251, #1253, #1254, and #1255.
All 17 children are closed. At reviewed snapshot `master@a7b296ea4`, the
transfer policy, both player modes, persistence, container knowledge, nested
window stack, and integrated gate compose as intended on the covered
acolyte/technomule paths. Master later advanced through unrelated changes that
did not touch the reviewed transfer implementation, tests, or steering docs.
Two new current mistakes remain: one executor-eligibility gap and one
steering-document drift concern.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [ ] ER-1. Mode B can queue an order for a carrier whose species cannot execute it
- [ ] ER-2. Current transfer steering docs still describe the pre-delivery arc

## 1. Transfer composition

### ER-1. Mode B can queue an order for a carrier whose species cannot execute it

> **Captured note:** Epic #1013's Mode B gesture (#1249) and executor (#1247)
> disagree about which player-commandable units can carry a durable transfer
> order. The Mode A review found and closed the equivalent gap for
> `escort_transfer`; the queued-order path still has it.

**Verification:** `FactionDebug` is player-commandable, and the debug overlay
spawns any armed unit definition—including `bear_brown` or `red_squirrel`—in
that faction. Mode B Store accepts the displayed unit directly; Retrieve calls
the shared resolver without its optional action-capability filter. The callback
then creates and stores the order before checking whether the carrier's species
registered `transfer_order`. Only acolyte and technomule register that action;
the bear and squirrel action lists do not. Their AI tick therefore never scores
or executes the queued order, so the valid durable order remains pending until
the player notices and cancels it. Existing Mode B coverage uses acolytes and
does not exercise this composition boundary.

**Evidence:**

- `src/Unit/Faction.hs:156` — both player and debug factions are
  player-commandable.
- `scripts/init_mouse.lua:203` — the debug overlay spawns the selected unit
  definition with faction `debug`, making the unsupported-species case
  reachable through shipped UI.
- `scripts/transfer_gestures.lua:188` — Store forwards the unit-info uid as the
  executor without checking `transfer_order`; Retrieve calls `resolveSource`
  without a required action at line 218.
- `scripts/transfer_session.lua:511` — the shared resolver already supports a
  `requiredAction` filter and explicitly records that Mode B omits it, leaving
  the queued executor question unresolved.
- `scripts/unit_ai_transfer.lua:385` — `commandTransferOrder` stores an accepted
  order and schedules a new decision without checking the species action
  inventory.
- `scripts/unit_ai.lua:145` — `transfer_order` is absent from the universal
  action list; it is registered only in the acolyte and technomule lists at
  lines 209 and 233.
- `scripts/bear_ai.lua:295` and `scripts/red_squirrel_ai.lua:222` — both
  commandable-when-debug-spawned species register action lists without
  `transfer_order`.
- `test-headless/Test/Headless/UI/TransferGestures.hs:55` — Mode B's real-order
  fixture uses acolyte carriers and only a non-commandable wildlife control, so
  it cannot expose the commandable-but-actionless case.

**Handoff context:**

- **Current behavior:** Mode B offers and accepts Store/Retrieve for a
  debug-faction bear or squirrel, persists the order, and never dispatches the
  action that would walk and commit it.
- **Expected behavior:** A player-facing transfer order is queued only when its
  acting unit can execute `transfer_order`, or the action is made universal for
  every species the player can command. The reusable command boundary must not
  accept work its dispatcher can never run.
- **Scope and constraints:** Preserve #1249's nearest-of-N ordering, omission
  rather than a disabled gesture, exact-instance batches, and the distinct Mode
  A source/target rules. Keep the Haskell transfer contract endpoint-generic;
  this is an AI action-inventory/ingress alignment concern, not a new endpoint
  def allowlist.
- **Verification target:** Extend the Mode B headless gesture/AI coverage with
  a `FactionDebug` bear and squirrel while the real action registry is loaded.
  Prove the UI/command boundary either omits/refuses without storing an order,
  or—if `transfer_order` becomes universal—ticks the chosen carrier through
  arrival and exactly-once commit. Cover both Store's direct executor and
  Retrieve's shared resolver.
- **Deduplication:** All-state tracker searches for transfer + bear, squirrel,
  commandable species, unsupported source, Mode B source action, action
  registry, and queued-order species returned no owning issue or PR beyond the
  historical Mode A fix in PR #1350. The primary and docs-worktree report
  corpora contain no pending finding for this Mode B gap.
- **Remaining uncertainty:** The safe correction boundary—capability-gating the
  two player ingresses, rejecting inside `commandTransferOrder`, or making the
  action universal—needs implementation-time review against future
  player-commandable species. The failure itself and its shipped debug-overlay
  reachability are confirmed.

## 2. Steering-document accuracy

### ER-2. Current transfer steering docs still describe the pre-delivery arc

> **Captured note:** Epic #1013's durable design authority contradicts both
> its own completed processing checklist and the current repository, while the
> active portable-container design still calls #1013 open. Maintainers reading
> either document's explicitly named "Current state and evidence" section are
> directed back to the state before the transfer delivery slices landed.

**Verification:** The unified-transfer document's checklist records every UIT
slice through the integrated gate as completed, but its metadata still says
the design is ready for issue processing and its following current-state
section says the interaction layer was never filed, nothing after C0 exists,
and the two old player paths are still live. It also calls #1157 open even
though that issue closed on 2026-08-15. Current code and the passing Mode B
tests prove the old Store/Withdraw paths have been retired in favor of durable
gestures. Open downstream epic #1231's live body correctly records #1013 as
complete and unblocked, but `docs/portable_loot_containers.md`'s current-state
section still calls it open and unfinished.

**Evidence:**

- `docs/unified_item_transfers.md:9` — the document remains marked `ready for
  issue processing` after every processing entry completed.
- `docs/unified_item_transfers.md:33` — the processing checklist records all 12
  UIT delivery slices through #1255 as completed.
- `docs/unified_item_transfers.md:72` — the section labeled "Current state and
  evidence" says the interaction layer did not land and was never filed.
- `docs/unified_item_transfers.md:100` — the same section calls closed issue
  #1157 open.
- `docs/unified_item_transfers.md:102` — it says nothing after C0 exists and
  that the epic is stalled/unfiled.
- `docs/unified_item_transfers.md:105` — it says the retired immediate Store and
  Withdraw paths remain live, although #1249 replaced them and current
  `Transfer context menu (Mode B Store/Retrieve gestures, #1249)` coverage
  asserts their absence.
- `docs/portable_loot_containers.md:99` — the active downstream design's current
  state calls #1013 open and says it still owns the remaining unit/building
  window and movement modes; live epic #1231 now says those foundations are
  complete and no longer block PLC-9.

**Handoff context:**

- **Current behavior:** The completed arc's own design authority gives mutually
  exclusive completion states and stale source pointers, and an active
  downstream design treats the completed upstream arc as unfinished.
- **Expected behavior:** Preserve the dated decision/amendment history, but make
  current metadata and present-state chapters accurately summarize the
  completed 17-child arc, its current implementations and gates, its now-met
  PLC-9 prerequisite, and any still-valid deferrals.
- **Scope and constraints:** This is a documentation correction only. Do not
  rewrite signed-off historical blockquotes or change the completed processing
  markers; replace stale present-tense claims and source locations with current
  architecture references.
- **Verification target:** Both designs' current-state prose, #1013's checklist
  and metadata, live epic #1231, current player paths, and
  `docs/engine_contracts.md` §Player transfers agree after the edits; searches
  for the quoted stale claims return no current steering-document hit.
- **Deduplication:** Exact-phrase and semantic all-state tracker searches for
  the post-C0/stalled/player-path claims and a search of the primary and
  docs-worktree report corpora found no owning issue or pending report.
- **Remaining uncertainty:** None.
