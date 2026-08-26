# Project Review Findings: PRs #1642–#1631

These entries record focused evidence from the senior review of the next twelve merged PRs in merge-time order — #1642, #1641, #1640, #1639, #1638, #1637, #1636, #1635, #1633, #1624, #1634, and #1631 — for later one-at-a-time disposition. The linked issues, PR descriptions, commit messages, merged diffs, and current descendants were inspected newest-first. There were no direct first-parent commits in the same landing interval.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]` reviewed and deliberately never to be filed · `[deferred]` blocked on a concrete precondition

PRs #1642, #1641, #1640, #1638, #1636, #1635, #1633, #1624, #1634, and #1631 retain their intended contracts in the current tree. In particular, the possible late-commit concern in #1624 was excluded after tracing selection-changing producers through the serialized Lua/world queues and running the 61-example page-binding gate: the pending projection rejects a change queued ahead of the bound spawn, and the world-thread commit orders changes queued after it. Focused auto-harvest, coffee-gesture, page-binding, and unit-AI reconciliation suites all passed, but the first two suites omit the failure boundaries recorded below. No batch defect was found to have been repaired by a later merge, and all-state tracker plus pending-report searches found no existing owner for either current defect.

## Status

- [ ] PRR-1. Auto-harvest cannot collect the last ripe plant's yields
- [ ] PRR-2. A raced coffee drain still grants the sip's effects

## 1. Auto-harvest collection arbitration

### PRR-1. Auto-harvest cannot collect the last ripe plant's yields

> **Captured note:** Make PR #1639's `collecting` phase independently eligible for arbitration. The action currently asks for another harvestable plant before it can execute the already-completed harvest's one-item-per-tick collection work.

**Verification:** Verified against current code and with a deterministic live-module Lua reproduction. One ripe plant returned two ground-item ids. The completion tick left `harvestPhase="collecting"` and two ids in `harvestLoot`; after the plant disappeared, the next production-style utility/execute tick printed `-inf`, left both fields unchanged, and picked up zero items. The focused `skill-scaled auto-harvest` suite passed 17 examples, but its collection assertion deliberately calls `execute` directly after acknowledging that utility has nothing left to score.

**Evidence:**

- `scripts/unit_ai_harvest.lua:110-125` — `utility` always calls `world.findHarvestableFlora`; when the just-harvested plant was the last ripe one, it returns `-math.huge` before considering `harvestPhase` or `harvestLoot`.
- `scripts/unit_ai_harvest.lua:128-140` — only `execute` advances the collecting phase and removes one ground-item id per tick, so a score that cannot win also prevents this cleanup from running.
- `scripts/unit_ai_harvest.lua:207-220` — a successful harvest removes the target, stores every yielded gid, enters `collecting`, and clears the target/progress. That state therefore immediately depends on a fresh target search that the completed plant can no longer satisfy.
- `scripts/unit_ai.lua:336-375` — the dispatcher executes only the highest action whose utility is strictly greater than its `-math.huge` baseline. An auto-harvest score of `-math.huge` is never selected merely because it has pending collection state.
- `test-headless/Test/Headless/Lua/UnitAiHarvest.hs:180-185` and `:250-259` — the fixture says collection runs “on execute alone” because utility has nothing left to score, then uses `execOnly()` for all three collection ticks. It never proves that normal arbitration reaches those calls when no second ripe plant exists.
- Issue #1582 requirement 4 required the existing exact-yield, one-item-per-tick collection behavior to remain intact. The implementation preserves the execute body but disconnects it from the ordinary one-plant lifecycle.

**Handoff context:**

- **Current behavior:** After a unit finishes the last currently ripe plant, its yielded ground items remain in `harvestLoot` and on the ground, but auto-harvest cannot execute the collecting branch. Collection resumes only if another harvestable plant later makes the same action win; without one, the phase can remain stranded indefinitely.
- **Expected behavior:** Pending collection work is eligible independently of finding a new plant, consumes exactly one recorded yield per AI tick through the existing exact-gid pickup path, and clears the phase/list on exhaustion or failed pickup. Target acquisition and skill-scaled picking remain unchanged outside that phase.
- **Scope and constraints:** Surfaced in PR #1639 / issue #1582. Preserve skill-derived work duration, interruption accounting, exact yield ids, one-item-per-tick pickup, farming XP once per completed harvest, and the existing transient-save policy for picking progress. Do not require or preselect another plant merely to finish the completed harvest.
- **Verification target:** Replace the direct `execOnly()` oracle with a normal arbitration scenario containing exactly one ripe plant that yields at least two gids. After the harvest removes that plant, successive thought ticks must pick one gid each, then clear `harvestPhase`/`harvestLoot`; assert no second harvestable plant was introduced and retain the existing 17 focused examples.
- **Deduplication:** All-state tracker searches for `harvestLoot`, auto-harvest collecting, and ripe-plant collection found only closed source issue #1582 and persistence issue #1589. Pending findings-report searches found the older farming-skill defect that became #1582 and the separate post-load `harvestLoot` scrub, but no owner for this arbitration failure.
- **Remaining uncertainty:** None. The test's direct-execute bypass and the live-module reproduction expose the same missing path.

## 2. Exact-instance coffee consumption

### PRR-2. A raced coffee drain still grants the sip's effects

> **Captured note:** Make PR #1637's exact-instance sip conditional on the amount the engine actually drains. `applySip` currently commits every stat and animation side effect before calling the atomic fill mutation, then ignores its `nil`, zero, or short-drain result.

**Verification:** Verified structurally and with a deterministic production-module Lua reproduction. The stub returned one eligible full coffee pot from `unit.getInventory`, then made `unit.modifyItemFillById` return `nil`, modeling the instance disappearing between the independent inventory snapshot and atomic mutation. `consumable.drinkInstance` still returned success, set hydration to 3.75, caffeine to 0.25, mood to 0.575, and called `unit.drink` despite draining nothing. The focused `Player coffee drink gesture` suite passed 13 examples; its stale cases mutate the inventory before the callback's revalidation and never exercise a change between that read and the later drain.

**Evidence:**

- `scripts/consumable_gestures.lua:125-143` — the callback rechecks commandability and delegates exact-instance revalidation plus mutation to `consumable.drinkInstance`; its comment promises a failed instance check changes no fill or stat.
- `scripts/consumable.lua:112-128` — `eligibleInstance` obtains a standalone `unit.getInventory` snapshot and returns the matching item table. This is not the later mutation's transaction.
- `src/Engine/Scripting/Lua/API/Units/Inventory.hs:537-556` — `unit.getInventory` reads `unitManagerRef` and returns copied item data, ending the read before Lua computes the sip and calls another API.
- `scripts/consumable.lua:133-173` — `applySip` derives the dose from that snapshot, applies hydration, caffeine, and mood first, then calls `unit.modifyItemFillById` and `unit.drink` without checking the mutation result.
- `src/Engine/Scripting/Lua/API/Units/Equipment.hs:57-95` — the exact-instance mutation is the authoritative atomic operation and explicitly returns the actual clamped delta, or `nil` when the unit or instance no longer exists. The caller discards precisely the value needed to know whether a sip occurred.
- `test-headless/Test/Headless/UI/ConsumableGesture.hs:480-525` — every stale-menu case removes/empties the item or changes activity before firing the callback. These prove revalidation of an already-stale menu, not the concurrent boundary between `eligibleInstance` and `modifyItemFillById`.
- Issue #1580 requirements 5 and 7 require a stale invocation to change no fill or stats and only the chosen exact instance to be drained. A missing or short atomic drain currently violates both the no-effect and amount-correspondence halves.

**Handoff context:**

- **Current behavior:** If the selected coffee instance disappears, empties, or loses part of its fill after `eligibleInstance` snapshots it but before the atomic fill update, the player can receive effects for the snapshot's full sip while the actual drain is nil, zero, or smaller. The function still reports success and queues the drink animation.
- **Expected behavior:** No hydration, caffeine, mood, success result, or drink animation is committed unless the exact instance atomically supplies a positive amount, and every effect is calculated from the amount actually removed. A stale disappearance produces the same no-mutation refusal promised for an item already stale when the callback begins.
- **Scope and constraints:** Surfaced in PR #1637 / issue #1580. Preserve exact-instance identity, quality/temperature scaling, clamping, the legacy `drink(uid, defName)` selection policy, idle/player-commandable gesture gates, and the merged-row submenu. The correction needs an honest transaction boundary; merely adding a third pre-drain inventory read leaves the same window.
- **Verification target:** Add a focused API/module case in which the chosen item changes between the eligibility snapshot and the authoritative mutation. Assert a missing/zero drain changes no stats and plays no drink animation; assert a clamped short drain applies effects proportional to the returned amount. Retain the current disappeared-before-callback, emptied-before-callback, busy-unit, and normal exact-instance cases.
- **Deduplication:** All-state tracker searches for coffee, stale drink, and `modifyItemFillById` found closed implementation issue #1580 and closed canteen issue #1220, whose scope is wrong-instance selection rather than this read/mutate race. Pending project/findings-report searches found no current owner.
- **Remaining uncertainty:** The reproduction forces the authoritative mutation result rather than scheduling two real engine threads at an exact instruction boundary. The independently completed `getInventory` read, later atomic mutation, and ignored documented result establish the failure without relying on timing probability.
