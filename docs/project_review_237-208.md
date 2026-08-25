# Project Review Findings: PRs #237–#208

These entries record focused evidence from the senior review of the next twelve merged PRs in first-parent order — #237, #232, #235, #233, #234, #231, #228, #230, #227, #226, #212, and #208 — for later one-at-a-time disposition. The window contains exactly those twelve PR merges and no unrelated direct commits.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

PR #237's tile/chunk source tags, #235's debug-overlay lifecycle, #233/#234/#231's HUD-owned panel teardown, #228's active-world HUD repaint, #230's item-container existence guard, and #212's coherent cursor-info arbitration all retain their contracts in the current centralized descendants. PR #227 remains output-neutral after the geology split. PR #226's page-slice merge has been deliberately superseded by the transactional whole-session load contract, which now stages and publishes every saved page together. The focused `World.CursorInfo` suite passed 7/7 and `World.Slope.slopeBit` passed 14/14. The latter proves the bank rule only with an in-chunk neighbour and does not cover the seam case below. A direct Lua semantic check also confirmed that `-1` takes `building_spawn`'s success branch. No graphical/offscreen session, concurrency harness, full suite, world check, or `make ci` was run. Three non-duplicate concerns remain.

## Status

- [x] PRR-1. Soft terrain bank slopes ignore wet neighbours across chunk seams — [#1685]
- [ ] PRR-2. A portal tick can outlive its active-page snapshot
- [ ] PRR-3. Portal spawn failures are truthy and consume the roster

## 1. Cross-chunk slope ownership

### [#1685] PRR-1. Soft terrain bank slopes ignore wet neighbours across chunk seams

> **Captured note:** Soft terrain at a chunk edge can still slope into a wet neighbour because the bank rule only sees the current chunk's fluid map.

**Verification:** Verified structurally; not reproduced in a rendered world. PR #232 made terrain-neighbour lookup, load/evict recomputation, and seam wrapping work across chunks, while promising to preserve the dry-bank rule. `slopeBit` still consults fluid occupancy only when the neighbour normalizes back into the source chunk; every adjacent-chunk neighbour is classified dry. A soft dry surface exactly one level above a loaded wet cell across the boundary therefore contributes a slope bit and takes the ordinary terrace path. The later hard-rock work added a real cross-chunk fluid lookup, but passes it only to `rockJaggedSlope`, explicitly leaving this older soft-terrace behavior unchanged.

**Evidence:**

- `src/World/Slope/Compute.hs:161-198` — dry terrain qualifies on an exact-one drop, but `hasFluid` is read from the local `fluidMap` only when `neighborCoord == coord`; the out-of-chunk branch is hard-coded `False`. Thus an adjacent-chunk wet cell cannot veto the dry slope bit.
- `src/World/Slope/Compute.hs:150-159` — soft dry terrain consumes `rawSlope` directly through `applyRoughness`. Only the hard-rock branch receives the separately calculated `wetN`/`wetE`/`wetS`/`wetW` masks.
- `src/World/Slope/Compute.hs:226-244` and `src/World/Slope/Recompute.hs:79-94` — the current tree already has a seam-aware `fluidNeighborLookup`, and the recompute supplies it, but that lookup feeds the hard-rock neighbour masks rather than `slopeBit`'s soft bank rule.
- `test-headless/Test/Headless/World/Render/SlopeBit.hs:48-63,84-90` — the bank regression test fixes `home` at chunk `(0,0)` and checks neighbour `(6,5)`, so its wet neighbour is necessarily in the same `fluidMap`. The focused suite passed all 14 examples without exercising an out-of-chunk wet cell.
- Commit `2347bdc7` (“enforce dry-rock bank rule across chunk seams”) documents the same visibility gap for the hard-rock path and says that `slopeBit`'s pre-existing in-chunk-only terrace behavior is deliberately unchanged. That makes the surviving asymmetry explicit rather than inferred from a refactor.
- Closed issues #222/#224 and PRs #232/#284 own the wet waterfall-lip and hard-rock cases respectively; tracker and pending-report searches found no current owner for a soft bank dipping into water only at a chunk boundary. Closed #26 and the pending water-render concern in `docs/project_review_835-822.md` concern water side/surface rendering, not the dry terrain slope bit.

**Handoff context:**

- **Current behavior:** An in-chunk wet neighbour vetoes a dry terrace slope. The same wet cell one coordinate across a loaded chunk boundary is treated as dry by `slopeBit`; if its terrain surface is exactly one level lower and the bank material is below the hardness threshold, the dry bank may render sloped into the river, lake, or sea. Recompute-on-load makes this stable, but does not correct the classification.
- **Expected behavior:** The dry-bank rule should be topology-independent: equivalent loaded neighbour terrain/fluid data produces the same slope decision whether the edge is inside a chunk, between ordinary chunks, or across the wrapped seam.
- **Scope and constraints:** Surfaced from PR #232 / issue #222 and the later #224 hard-rock review. Preserve wet waterfall-lip slopes, the exact-one dry terrace rule, hard-rock jagged masks, canonical U wrapping, load-and-evict convergence, and the post-recompute dig/construction override restore. `ctSlopes` remains derived render state; this should not alter the save schema.
- **Remaining uncertainty:** A generated or synthetic two-chunk visual fixture should confirm how often a soft bank lands exactly one terrain level above a wet neighbour and whether roughness ever masks the visible dip. The branch error is direct, but the player-visible frequency was not measured.

## 2. Active-page action ownership

### PRR-2. A portal tick can outlive its active-page snapshot

> **Captured note:** The active-page building list is a snapshot; a portal can finish its tick after its page has been hidden and spawn into that hidden page.

**Verification:** Partially verified. The cross-thread interleaving is admitted by PR #208's own follow-up commit and remains possible in the current call graph, but it was not forced with a scheduling barrier. The PR correctly stopped a page switch from routing a unit into the *new* active world by passing the building's own page to `unit.spawn`. It did not revalidate that the building is still on the active page before committing the tick. Because the explicit-page spawn accepts any live page, a page that becomes hidden after `getActiveIds` can still consume one roster entry, spawn a unit into itself, and issue that unit's move order off-view.

**Evidence:**

- `scripts/building_spawn.lua:571-580` — `update` snapshots `getActiveBuildingIds()` once, then performs separate global `building.getInfo`, portal, and construction calls for every returned id. There is no active-page check after the snapshot.
- `src/Engine/Scripting/Lua/API/Buildings/Query.hs:198-216` — `building.getActiveIds()` reads `worldManagerRef` and returns a Lua array for the page active at that instant. The result carries no page generation/epoch that later operations can validate.
- `src/Engine/Scripting/Lua/API/World/Lifecycle.hs:504-525` — `world.show` and `world.hide` enqueue commands to the world thread. That thread can change `wmVisible` after the Lua-side list read while the remainder of `buildingSpawn.update` is still making independent API calls.
- `src/Engine/Scripting/Lua/API/Buildings/Query.hs:43-92` — `building.getInfo` is global and continues returning the building and its owning page after the page becomes hidden, so it does not close the stale-list window.
- `scripts/building_spawn.lua:140-187` — once cooldown/roster checks pass, `tickOne` intentionally passes `info.page` to `unit.spawn`. This guarantees correct destination ownership, but it also lets the action succeed after that page ceases to be active.
- `src/Engine/Scripting/Lua/API/Units/Spawn.hs:113-139` — an explicit page argument is accepted whenever its page remains in `wmWorlds`; visibility or active-page membership is not required. A normal hide leaves the world live, so the stale tick is not rejected.
- PR #208's second commit explicitly identifies the page-switch interval between the active-page scan and spawn. Its stable-page verification proves explicit page ownership and surface-Z selection, not that a formerly active portal stops ticking. Tracker searches found only closed #196 and unrelated remote-portal issues; no open issue or pending report owns this residual window.

**Handoff context:**

- **Current behavior:** Most ticks enumerate only the active page. If visibility changes after enumeration, a roster-ready portal from the old page can still finish one tick; explicit page binding turns the original wrong-world spawn into a correctly owned but off-view spawn, consumes the roster count, and initializes AI movement in the hidden world.
- **Expected behavior:** The active-only simulation boundary promised by #196 should hold at action commit, not just list acquisition. A building that is no longer eligible for the active-page tick must not consume spawn state or create a unit because it appeared in a stale snapshot; explicit page binding must still prevent wrong-world placement for eligible actions.
- **Scope and constraints:** Preserve global entity-id uniqueness, explicit building-page ownership, target-page surface-Z lookup, spawn retry/cooldown behavior, asynchronous world-thread lifecycle commands, and normal hidden-world persistence. Do not solve this by reverting to active-page destination lookup or by holding a shared lock across Lua execution.
- **Remaining uncertainty:** The race window is narrow and was not reproduced under ordinary scheduling. A deterministic fixture should pause after `building.getActiveIds`, apply `WorldHide`/`WorldShow` on the world thread, then resume a roster-ready portal tick and assert no roster consumption or unit creation on either page. The worker-construction path may self-suppress after a switch because its worker query is active-page-scoped; the portal path has no equivalent late eligibility check.

### PRR-3. Portal spawn failures are truthy and consume the roster

> **Captured note:** `unit.spawn` returns `-1` on failure, but the portal tests only `if not newUid`, so its failure path is unreachable for the API's defined sentinel.

**Verification:** Verified structurally and with the installed Lua interpreter. `unit.spawn` returns the number `-1` for every synchronous rejection, while Lua treats every number, including zero and negative numbers, as truthy. Running the portal's exact predicate with `newUid = -1` printed `success-branch:-1`. The caller consequently skips its warning/backoff path, runs success-only side effects against id `-1`, decrements the building's roster, and records the invalid id as its last spawned unit.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Units/Spawn.hs:47-48,94-97,127-133,185-186` — the public contract says “unit ID or -1,” missing name/definition and missing target-world branches return `-1`, and the result is always pushed to Lua as a number rather than `nil`/`false`.
- `scripts/building_spawn.lua:180-214` — the portal checks only `if not newUid`. That branch contains all retry throttling, warning, and “do NOT consumeSpawn” behavior, but numeric `-1` cannot enter it.
- `scripts/building_spawn.lua:216-238` — after a rejected spawn, the code attempts starting-item grants and an AI move for `-1`, calls `building.consumeSpawn`, stores `s.lastUid = -1`, resets the failure counter, and logs a successful spawn.
- `scripts/unit_ai_core.lua:284-295` — `unitAi.commandMove(-1, ...)` unconditionally calls `ensureState`, so the false success also creates an AI-state row for a nonexistent negative unit id before forcing its decision timer.
- `scripts/building_spawn.lua:299-330,371-377` — the component's persisted reference validator requires ids to be positive, yet snapshot wrapping preserves `s.lastUid = -1`. If a save captures the still-live portal in this state, its required `building_spawn` payload can no longer satisfy its own schema.
- `scripts/locations.lua:388-400` is the correct sibling caller: it explicitly tests `uid == -1`, confirming that `-1`, not nil, is the established Lua API failure contract.
- Closed issue #309 and PR #314 describe and test a nil-returning `unit.spawn` stub, so their rate-limit harness exercises a failure representation the production API never emits. PR #208 inherited this mismatch while adding explicit-page rejection as another `-1` path. Searches found no open issue or pending report for the sentinel mismatch.

**Handoff context:**

- **Current behavior:** A portal's synchronous spawn rejection is treated as success. It consumes one roster slot without creating a unit, records negative identity in Lua state, skips the intended failure log/backoff counter, and can repeat after `previousUnitCleared` observes that unit `-1` does not exist. A save made before a later successful spawn overwrites that identity may also fail component validation.
- **Expected behavior:** Every defined `unit.spawn` failure representation must enter the portal's failure path: no item/AI work, no roster consumption, no invalid `lastUid`, and retries governed by the existing throttle/suppression policy. Production and test doubles must share the same return contract.
- **Scope and constraints:** Surfaced at PR #208's explicit-page spawn boundary and confirmed by the later #309/#314 failure handling. Preserve positive unit ids, asynchronous successful spawn behavior, the explicit owning-page argument, roster/cooldown persistence, and sibling callers that already compare against `-1`. Audit the small set of Lua callers before changing the API-wide sentinel so a local portal correction does not silently invert another consumer.
- **Remaining uncertainty:** Shipped portal roster definitions are registered and a merely hidden page is still live, so ordinary play rarely reaches the synchronous failure branches. A focused harness should use an invalid def and a destroyed explicit page, assert the exact Lua return, then prove roster/state/save behavior. The downstream save rejection was traced through the component validator but not driven through a real save transaction.
