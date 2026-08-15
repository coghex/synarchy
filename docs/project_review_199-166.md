# Project Review Findings: PRs #199–#166

These entries record focused evidence from the senior review of the next twelve merged PRs in first-parent order — #199, #179, #192, #187, #171, #181, #174, #169, #170, #163, #168, and #166 — for later one-at-a-time disposition. The window contains exactly those twelve PR merges and no unrelated direct commits.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

PR #199's live-instance cargo measure passed its real headless probe (90 full canteens, 198 kg stored against a 200 kg capacity). PR #179's zero-size projection sweep passed all 16 focused `Render.ViewportGuard` examples. PR #192's right-click swallowing remains subsumed by the current pointer-policy router, whose 33 focused `UI.InputOwnership` examples passed. PR #171's focus-loss reset now includes deferred UI clicks, pending control activations, and consumed control-focus keys; all 30 focused `UI.ControlActivation` examples passed. PR #181's item-selection exclusion, PR #169's debug-overlay gate, PR #170's exit-time mine-anchor cancellation, PR #168's shutdown disarm, and PR #166's boxless tooltip geometry remain present; the focused tooltip suite passed 5/5. No graphical/offscreen session, full suite, world check, or `make ci` was run. Three non-duplicate concerns remain.

## Status

- [ ] PRR-1. Unit Info clears other selections even when its captured target is stale
- [ ] PRR-2. HUD hover keeps mutating the world behind modal gameplay screens
- [ ] PRR-3. Main-menu sorting discards the engine's deterministic save-timestamp tiebreak

## 1. Deferred context-menu selection

### PRR-1. Unit Info clears other selections even when its captured target is stale

> **Captured note:** PR #187 made Unit Info clear the building and ground-item domains after `unit.select`, but it ignores the selection call's failure result; a target that disappears while its menu is open therefore clears valid selections without selecting the requested unit.

**Verification:** Reproduced through the current loaded Lua modules with only hit-testing and menu presentation replaced to capture the production callback. A missing uid made the real `unit.select(999999)` return `false`, but invoking the captured Unit Info callback still called both deselection functions (`claimed=true select=false buildingClears=1 itemClears=1`). This is a live scheduling case rather than invalid API input: the context menu captures `targetUid` when it opens, the modal does not pause simulation, and unit destruction atomically removes the target before the deferred row callback runs.

**Evidence:**

- `scripts/init_context_menu.lua:92-116` — `tryUnitMenu` captures the hit-tested uid in a closure; its Info row calls `unit.select(targetUid)` and then unconditionally calls `building.deselect()` and `item.deselect()` without inspecting the boolean result.
- `src/Engine/Scripting/Lua/API/Units/Selection.hs:39-52` — `unit.select` explicitly returns success/failure. `Unit.Selection.selectUnit` rejects a uid that is missing or no longer belongs to the active page (`src/Unit/Selection.hs:31-48`) and leaves the old unit selection unchanged.
- `scripts/ui/context_menu.lua:366-396,614-633` — the rows and their closures remain stored on a visible modal page until a later click; the click path hides the menu and only then invokes the captured callback. Nothing in this lifecycle pauses the unit thread or revalidates a row's target.
- `src/Unit/Thread/Command/Lifecycle.hs:25-35` — `UnitDestroy` independently removes a unit from `umInstances` and selection. A combat/despawn result can therefore invalidate the captured uid while the menu is waiting for input.
- `src/Engine/Scripting/Lua/API/Buildings/Selection.hs:60-65` and `src/Engine/Scripting/Lua/API/Items/Render.hs:72-80` — the two calls made after the failed unit selection really do clear their respective selection domains; neither is conditional on a successful replacement.
- PR #187's merge commit (`87d418b5`) introduced the mutual-exclusion cleanup for issue #178. Tracker and pending-report searches found no owner for the stale-target failure branch.

**Handoff context:**

- **Current behavior:** If the target still exists, Info correctly replaces unit selection and clears incompatible building/item ownership. If it disappears or becomes ineligible before the row click, `unit.select` returns false, any prior unit selection remains, and building/item selection is still erased; the requested Info action can therefore leave the old unit visible or no information selected at all.
- **Expected behavior:** Clear the other selection domains only after the requested unit selection succeeds. A stale Info row should otherwise close as an unavailable/no-op action (or report that the target is gone) without destroying unrelated current selection.
- **Scope and constraints:** Surfaced from PR #187 / issue #178. Preserve one-domain-at-a-time HUD ownership for every successful unit selection, active-page filtering, modal input ownership, and asynchronous unit destruction. The narrow branch can use the boolean `unit.select` already returns; it does not require keeping a dead target alive or freezing simulation while a menu is open.
- **Remaining uncertainty:** The callback was captured from the real module and its real selection result was exercised, but a unit was not killed by combat while a rendered context menu was open. A focused regression should open Info on a live unit, destroy it before activation, click the row through `cm.handleItemClick`, and assert the prior building/item/unit selection policy explicitly.

## 2. Modal hover ownership

### PRR-2. HUD hover keeps mutating the world behind modal gameplay screens

> **Captured note:** PR #174 gates hover only on `hud.visible`; pause and keep-world Settings deliberately leave that flag true while the canonical gameplay-input predicate is false, so moving over modal UI still pushes cursor hover into the underlying world.

**Verification:** Reproduced in the engine's loaded Lua VM. With `currentMenu="world_view"`, `pauseMenu.visible=true`, a visible zoomed-in HUD, and deterministic mouse/world hooks around the real `hud.update`, `uiManager.isGameplayInputActive()` returned `false` while the update still called `world.setWorldCursorHover("prr_hover",123,456)` (`active=false seen=prr_hover:123,456`). The same structural gap applies to keep-world Settings: that transition changes `currentMenu` to `settings` but intentionally skips `hud.hide()`.

**Evidence:**

- `scripts/hud.lua:1163-1183` — the update returns only when `hud.visible` is false, then unconditionally reconciles and pushes zoom/world hover from the current mouse position. It does not consult `isGameplayInputActive`.
- `scripts/hud.lua:965-983` — the adjacent click path already documents that `hud.visible` is insufficient for pause and keep-world Settings and combines it with `uiManager.isGameplayInputActive()`. The hover path did not receive the same ownership correction.
- `scripts/ui_manager.lua:88-124` — `isGameplayInputActive` is false for a visible pause menu, a non-gameplay current menu, or any exclusive modal boundary. It is the shared input-ownership gate for gameplay keys, clicks, and scroll.
- `scripts/pause_menu.lua:363-384` — opening Pause creates/shows its own modal without calling `hud.hide`, so the visible HUD keeps ticking while gameplay input is inactive.
- `scripts/ui_manager_menu.lua:31-45` — game-to-Settings sets `keepWorld` and deliberately skips both `worldView.hide()` and `hud.hide()`. `scripts/ui_manager_boot.lua:395-409` continues calling `hud.update` on every UI-manager tick regardless of the current menu.
- PR #174's first commit (`5aa95692`) states that cursor hover must not keep mutating behind open menus, but implemented visibility as the sole authority. Its follow-up (`19265b00`) discovered the broader overlay case for clicks; the same distinction remains absent from hover. Tracker and pending-report searches found no current hover/modal owner.

**Handoff context:**

- **Current behavior:** Fully hidden HUDs stop hover updates, but Pause and keep-world Settings keep translating modal-screen cursor movement into zoom/tile hover state for the world underneath. The render cursor and designation previews can keep following a pointer currently owned by modal UI even though every gameplay action route reports inactive.
- **Expected behavior:** World hover should update only while the HUD both is visible and owns active gameplay pointer context. Pause, Settings, or another exclusive modal should not mutate the underlying world's cursor hover from modal-owned pointer positions.
- **Scope and constraints:** Surfaced from PR #174 / issue #153. Preserve `currentView` across hide/show, the visible world backdrop used by Settings/Pause, LayerDebug's deliberate pass-through behavior, and the existing gameplay-input predicate's modal semantics. This is an update-path ownership gate, not a request to hide the world or block debug UI.
- **Remaining uncertainty:** The deterministic check observed the real `hud.update` branch but replaced the mouse getter, view reconciler, and final world call to avoid needing a GPU render cursor. A graphical regression should open Pause and keep-world Settings, move across the modal, and assert the per-page cursor hover/preview remains stable; the processor should also decide whether a purely visual hover behind a transparent/pass-through modal is intentionally allowed and therefore needs a narrower exclusive-modal test.

## 3. Save-list ordering

### PRR-3. Main-menu sorting discards the engine's deterministic save-timestamp tiebreak

> **Captured note:** PR #163 normalized legacy timestamps and the engine sorts equal timestamps by slot name, but `main_menu.lua` sorts the already-ordered list again using timestamp alone; Lua's unstable sort can change which tied legacy slot becomes Continue.

**Verification:** Reproduced in the engine's own embedded Lua VM. Sorting the already canonical name-ascending rows `a,b,c,d`, all with the same timestamp, through the exact main-menu comparator produced `a,c,b,d`. The first row happened to remain `a` in this four-row case, but the comparator has no equality tiebreak and other mixed/equal layouts can move any tied group implementation-dependently. Equal valid rows are not hypothetical: issue #98's original reproduction created multiple second-precision saves in one second, and PR #163's compatibility pass deliberately normalizes every such legacy timestamp to the same fixed-width microsecond string.

**Evidence:**

- `src/World/Save/Serialize.hs:273-276,307-314` — `listSaves` declares and implements one canonical order: timestamp descending, then slot name ascending.
- `src/World/Save/Serialize.hs:435-455` — every parseable legacy/millisecond/microsecond timestamp is normalized before sorting. Two old `…32Z` saves become the identical `…32.000000Z`, so the name fallback remains necessary even after canonicalization.
- `scripts/main_menu.lua:60-67` — `engine.listSaves()` is immediately re-sorted with only `a.timestamp > b.timestamp`; `latestSave` (the Continue target) is then chosen from the resulting first row.
- `src/Engine/Scripting/Lua/API/Save.hs:368-400` — new saves within one running process are forced at least one microsecond apart, so the residual concentrates in legacy/imported saves, malformed pass-through timestamps, and cross-process wall-clock collisions rather than the original same-process path.
- PR #163's review follow-up (`19b47805`) explicitly noted that the Haskell consumer had a name fallback while `main_menu.lua` had none. The final normalization commit (`18a4e92d`) made legacy strings comparable but left equal instants equal. The current embedded-VM check confirmed the second sort does not preserve input order.
- Tracker and pending-report searches found no issue owning the main-menu loss of the engine's tiebreak.

**Handoff context:**

- **Current behavior:** `engine.listSaves()` returns a deterministic order, but main-menu construction can permute rows sharing a timestamp and then derives Continue from that new order. The load browser receives the same mutated `mainMenu.saves` list, so its tied-row display order can drift too.
- **Expected behavior:** The main menu should consume the engine's canonical order unchanged, or repeat the same timestamp-descending/name-ascending comparator. When historical metadata cannot reveal true sub-second recency, the documented deterministic fallback must survive through Continue and the browser.
- **Scope and constraints:** Surfaced from PR #163 / issue #98. Preserve fixed-width normalization, monotonic timestamps for new same-process saves, newest-first ordering, autosave metadata, and slot name as identity. Do not infer unavailable historical creation order; consistency with `listSaves` is the narrow contract.
- **Remaining uncertainty:** Lua does not promise a stable `table.sort`; the exact permutation can vary by embedded Lua implementation/version, which is part of the defect rather than a mitigation. A focused Lua test should feed tied and mixed timestamps through `mainMenu.buildMenuItems`, assert the complete engine order is preserved, and assert `latestSave` uses the same canonical first row. The processor may classify this lower severity because true recency among legacy ties is unknowable.
