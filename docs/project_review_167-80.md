# Project Review Findings: PRs #167–#80

These entries record focused evidence from the senior review of the next twelve merged PRs in first-parent order — #167, #165, #164, #162, #155, #121, #113, #112, #109, #77, #83, and #80 — for later one-at-a-time disposition. The window contains exactly those twelve PR merges and no unrelated direct commits.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

PR #167's shell `quit`/`exit` commands still call the real lifecycle transition, now protected by the later monotonic startup handshake. PR #165's missing-Lua-component reset has been superseded by the transactional, versioned `prepareLoad`/`applyAll` component protocol and its extensive focused suite. PR #162's displayed sharpness mirrors the current combat formula (`base * 100 / clamped wear`), and PR #155's ground-item weight now reaches the recursive `itemTotalWeight` calculation. PR #80's dump waits perform a final boundary poll, keep fatal text on stderr, and abort before JSON output on timeout. No graphical/offscreen session, forced two-queue scheduler, full headless suite, world check, or `make ci` was run. A direct Lua execution reproduced the notification-pause speed loss, a second reproduced the Defaults omission, and the checked-in settings-revert harness failed before reaching its assertions. Seven non-duplicate concerns remain; the final teardown concern is deliberately retained as lower-confidence code-health debt for the processor to verify.

## Status

- [x] PRR-1. Per-world units still move, re-ground, and infect against another page's environment — [#1593]
- [ ] PRR-2. A queued fluid writeback can overtake a live edit's sim re-seed
- [ ] PRR-3. Notification pauses discard the player's non-default world speed on resume
- [ ] PRR-4. Build placement does not bind pick, validation, and commit to one world page
- [ ] PRR-5. Settings Defaults preserves the live tooltip timing values instead of defaulting them
- [ ] PRR-6. The settings Revert regression harness no longer reaches its assertions
- [ ] PRR-7. Exit-to-menu teardown leaves per-entity Lua state accumulating across new sessions

## 1. Unit simulation page ownership

### [#1593] PRR-1. Per-world units still move, re-ground, and infect against another page's environment

> **Captured note:** PRs #109/#113 gave units an owning page and moved boundary reads onto the canonical active-world resolver, but several internal simulation paths still choose one active/first-visible environment for every unit. A unit on another visible page can path through the wrong terrain, a teleport or terrain edit can snap it to another page's surface, and wound infection uses the active page's climate rather than the unit's own.

**Verification:** Verified structurally in the current descendants; no multi-world movement or infection fixture was run. The movement and re-ground modules now label two of these branches as known deferred follow-ups to #797, but there is no open issue or pending report that owns either one. The wound tick has the same ownership error without that annotation: it reads one active climate before folding every globally stored `UnitInstance`, then samples each unit's coordinates in that one climate. Reordering `wmVisible` can therefore change secondary-page movement/surface results, while changing the active page can change infection onset/type for a unit whose `uiPage` did not change.

**Evidence:**

- `src/Unit/Thread/Movement.hs:70-92` snapshots one `Maybe WorldTileData` before ticking the entire global unit-state map. No page accompanies the snapshot or is selected inside the per-unit fold.
- `src/Unit/Thread/Movement/PathAdvance.hs:36-51` explicitly says `snapshotVisibleWorldTiles` reads `wmVisible`'s head, so a secondary-page unit paths against the active page's terrain. The comment calls this a deferred #797 follow-up, but supplies no tracker owner.
- `src/Unit/Thread/Command/Lifecycle.hs:128-153` likewise scans visible pages in order for `lookupSurfaceZ`. `handleUnitTeleportCommand` has a uid whose `uiPage` could choose the correct surface; `UnitReGround` lacks page identity altogether, so a terrain edit on one page can snap globally matched units using whichever visible page provides the first loaded chunk at those coordinates.
- `src/Combat/Wounds/Tick.hs:75-114` resolves one `activeWorldStateFrom`, extracts its climate/world size, then folds every `umInstances` entry and calls `lookupLocalClimate` only with that unit's numeric coordinates. It never consults `uiPage inst`.
- `src/Unit/Types/Instance.hs:29` and `src/Unit/Types/Manager.hs:59-68` show the necessary owning-page identity and page-filter helpers already exist. Rendering and selection use them, while these simulation consumers do not.
- Issue #797 fixed the analogous LOS/awareness paths and its canonical review named movement and re-ground as explicit adjacent follow-ups. That issue is closed, and searches for secondary-page movement, re-ground, infection climate, and owning-page simulation found no open tracker or pending-report owner.

**Handoff context:**

- **Current behavior:** Every unit movement tick receives the head visible page's tiles. Teleport/re-ground surface lookup takes the first visible page with a loaded chunk. Every wound tick receives the active page's climate. Single-page play is correct, but simultaneously loaded/visible pages can cross-contaminate these decisions.
- **Expected behavior:** Each unit-owned simulation reads terrain, surface height, climate, size, and other spatial inputs from `uiPage`. A missing, destroyed, or hidden owning page follows an explicit inactive/failure policy rather than falling back to an unrelated page.
- **Scope and constraints:** Surfaced from PR #109's canonical resolver and PR #113's per-world units (epic #101), with the movement/re-ground omissions later documented by #797. Preserve one batch snapshot per page where useful, global id uniqueness, hidden-page policy, save/load page ownership, fall/climb timers, infection RNG determinism, and single-world results. The terrain edit command chain may need to carry `WorldPageId` into `UnitReGround`.
- **Remaining uncertainty:** The three branches may deserve separate issues because movement batching, command signatures, and wound climate have different owners and tests. The processor should first build two synthetic pages with deliberately opposed terrain elevations/climates, place one unit on each, reorder visibility, and decide the intended simulation policy for hidden pages before splitting the work.

## 2. Edit/writeback ordering

### PRR-2. A queued fluid writeback can overtake a live edit's sim re-seed

> **Captured note:** PR #112 made live terrain/fluid edits re-seed the sim, but the authoritative edit and the re-seed travel in opposite queues. A sim batch computed from the old chunk can already be waiting behind the edit on the world queue, apply afterward, and overwrite the fresh terrain/fluid before the sim thread consumes `SimChunkEdited`.

**Verification:** Partially verified from the two-thread queue topology; the interleaving was not forced. The individual operations are correct in isolation, but FIFO ordering applies only within each queue. The world thread writes the edit and then enqueues a post-edit snapshot to the sim queue. Independently, the sim thread can enqueue a pre-edit `WorldApplyFluids` batch to the world queue. If the world queue order is edit → old batch, the old batch overwrites the just-edited sim-owned fields. The later re-seed makes eventual recovery possible after another sim tick, but pausing between those steps, or saving while the overwritten world state is authoritative, can extend or persist the wrong result.

**Evidence:**

- `src/World/Thread/Command/Edit/Fluid.hs:33-51` writes the edited chunk to `wsTilesRef`, appends its edit, and only then calls `syncEditToSim`. Terrain delete/set-cell follow the same ordering at `src/World/Thread/Command/Edit/Terrain.hs:72-78,233-238`.
- `src/World/Thread/Command/Edit/Sync.hs:20-30` does not synchronize or acknowledge; it only enqueues `SimChunkEdited` onto the independent sim queue with a copied post-edit fluid/terrain snapshot.
- `src/Sim/Thread.hs:136-145,365-391` ticks independently and enqueues dirty `WorldApplyFluids` batches onto the world queue. A batch contains concrete old fluid, terrain, rendered surface, and side-decoration values, not a generation/epoch that can be rejected after a newer edit.
- `src/World/Thread/Command.hs:242-268` applies every batch for a still-live page unconditionally. `applyOneWriteback` overwrites the entire chunk's sim-owned fluid/terrain/surface/side-decoration fields and performs no edit-generation comparison.
- `src/Sim/Thread.hs:234-272` eventually consumes `SimChunkEdited`, re-seeds and activates the chunk, but that only repairs the sim-side copy. A new world-side writeback is produced on a later unpaused tick, not as part of consuming the command.
- Issue #60 / PR #112 specifically promised that stale sim output could not overwrite a live edit. Tracker and pending-report searches for stale writeback ordering, edit epochs, and sim re-seed races found no current owner.

**Handoff context:**

- **Current behavior:** An old sim result can land after the live edit because the edit→sim and sim→world messages have no common causal sequence. The edit remains in `wsEditsRef`, so reload/replay may reconstruct it, while the live `wsTilesRef` temporarily or persistently reflects the older sim fields.
- **Expected behavior:** Once a live edit commits, no writeback derived from an earlier chunk generation can overwrite it. Re-seed and subsequent writeback ordering should be explicit, observable, and safe across pause/save boundaries.
- **Scope and constraints:** Surfaced from PR #112 / issue #60. Preserve the world thread as sole `wsTilesRef` writer, page-tagged sim state, active-neighbour activation, fast-settle acknowledgements, edit-log replay, and save-barrier quiescence. Candidate mechanisms include per-page/chunk generations on both commands, an acknowledgement fence, or applying/reconciling the queued edit at the world-writeback boundary; do not introduce cross-thread direct tile writes.
- **Remaining uncertainty:** Frequency and save impact require a deterministic scheduler fixture. Hold a pre-edit dirty batch, commit `WeSetFluidTile` or a terrain edit, deliver the old batch, pause before the next tick, and inspect both `wsTilesRef` and a captured save; then release `SimChunkEdited` and prove the intended convergence. The processor should close this as no-issue if existing queue/barrier ordering not visible in these local functions makes that sequence impossible.

## 3. Notification pause speed

### PRR-3. Notification pauses discard the player's non-default world speed on resume

> **Captured note:** PR #83 preserved the selected time scale across a normal pause cycle, but notification categories bypass `pause.set(true)` and write the engine flag directly. Resuming through Space therefore restores `pause.prevTimeScale`—normally 1.0—rather than the live speed that was active when the notification paused the game.

**Verification:** Reproduced by executing the current `scripts/pause.lua` with its real branch logic and minimal engine/world bindings. Starting from `engine.isPaused() == true`, live world scale `10`, and the module's normal `prevTimeScale == 1.0`, `pause.toggle()` produced `paused=false`, scale `1.0`, `prevTimeScale=1.0`. This exactly models `Engine.PlayerEvent.Emit` setting the flag directly while leaving the per-page scale live. The module contains a healing branch, but it runs only when asked to pause an already-paused game; ordinary resume never captures the live speed first.

**Evidence:**

- `src/Engine/PlayerEvent/Emit.hs:128-139` implements `ccPause` by writing `enginePausedRef = True` directly. It neither calls Lua nor snapshots/zeroes the page's time scale.
- `scripts/pause.lua:46-68` explicitly recognizes notification auto-pause and can heal the split only when `pause.set(true)` is called while the flag is already true. That is not the normal player resume path.
- `scripts/pause.lua:85-103` handles `pause.set(false)` by applying the flag and writing `pause.prevTimeScale` to the active page. It never reads the still-live current scale on this branch.
- `scripts/init_keys.lua:94-99` routes Space to `pause.toggle`, and `pause.toggle` negates the authoritative engine flag. A notification popup is pass-through rather than an exclusive modal, so the key reaches this path.
- `scripts/pause.lua:8-11,93-100` documents the public promise that a selected fast-forward scale survives a pause cycle. PR #83 / issue #42 established the pause/time-scale consistency contract; later notification pauses created another direct flag writer without completing that handshake.
- Tracker and pending-report searches for notification pause speed, `prevTimeScale`, and non-default resume found no owner. Existing save/pause work concerns transaction safety and deliberate post-load default speed, not a live notification pause.

**Handoff context:**

- **Current behavior:** At 10× (or any non-default speed), a pause-triggering notification freezes simulation through the engine flag but leaves the stored page scale at 10. Pressing Space resumes at the stale global default 1×. Calling the pause action once more before resuming happens to run the healing branch and preserves 10×, but that extra step is not an intuitive contract.
- **Expected behavior:** Every live-session pause source captures the active page's chosen scale exactly once, and every corresponding resume restores it. Save-load remains free to use its separately documented “resume at default speed” policy.
- **Scope and constraints:** Surfaced from PR #83 / issue #42. Preserve `enginePausedRef` as the authoritative simulation gate, load-in-flight unpause rejection, player-intent generation used by autosave, pass-through notification cards, and per-page time scales. Avoid relying on a Lua callback that can be missed during teardown; a source-aware pause transition or an engine-to-Lua notification may be safer.
- **Remaining uncertainty:** The direct module execution proves the state transition, but a real `ccPause` event was not emitted in a running world. A focused integration test should set 10×, emit a configured pause category, press the real Space route, and assert the active page returns to 10× while load publication still resumes at its mandated 1×.

## 4. Build-placement page transaction

### PRR-4. Build placement does not bind pick, validation, and commit to one world page

> **Captured note:** PR #77 made the click use a synchronous live tile pick, but the pick returns coordinates without page identity. `building.canPlaceAt` and `building.spawn` each resolve “active” again, and `canPlaceAt` itself reads the active page and visible tile snapshot separately. A visibility switch can validate one page's building/location metadata against another page's terrain, or place the click into a page other than the one that was hit-tested.

**Verification:** Partially verified from current call boundaries; the narrow page-switch window was not forced. `world.show`/`world.hide` are world-thread commands and can change `wmVisible` between Lua API calls. More importantly, `buildingCanPlaceAtFn` first captures `(pid, ws)` through `activeWorldPageFrom`, then makes a second world-manager read through `snapshotVisibleWorldTiles`; those two values have no shared generation or identity check. Thus even a single validation call can combine page A's building instances, location instances, and world size with page B's tile data.

**Evidence:**

- `scripts/build_tool.lua:953-988` calls `world.pickTile`, then `building.canPlaceAt`, then later power/remote/spawn/designation helpers as separate operations. Only the integer `(gx, gy)` crosses those boundaries.
- `src/Engine/Scripting/Lua/API/WorldQuery/Pick.hs:97-138` resolves the current head of `wmVisible`, performs the hit-test, and returns only `gx, gy,z`. Its own `world.pickChunk` sibling takes an explicit page id because multiple visible pages exist (`:149-163`).
- `src/Engine/Scripting/Lua/API/Buildings/Spawn.hs:125-161` captures `mActive` at line 137, then independently calls `snapshotVisibleWorldTiles` at line 145. Page ownership for occupancy/location/world-size comes from the first read; terrain comes from the second.
- `src/Engine/Scripting/Lua/API/Buildings/Spawn.hs:46-95` shows the commit API already supports an explicit page id that pins validation and spawn. The build tool's `commitStartingPlacement` omits it (`scripts/build_tool.lua:918-920`), and ordinary validation exposes no corresponding page argument.
- `scripts/build_tool.lua:1028-1035,1061-1066` uses the HUD's explicit `worldId` for construction designations, demonstrating that the tool already has a page owner, but its pick/validation and instant building paths do not carry it consistently.
- Related `PRR-2` in `docs/project_review_237-208.md` owns a portal tick that finishes after its active-page roster snapshot; it does not own screen picking or mixed-page building validation/commit. Tracker and pending-report searches found no owner for this placement transaction.

**Handoff context:**

- **Current behavior:** Under a page switch, the tile under the click can come from page A, validation can mix A/B state, and the eventual spawn can resolve page B. At identical coordinates this may silently place a building in the newly active world; with differing terrain/location bounds it may show a false valid/invalid result or a ghost at another surface height.
- **Expected behavior:** One placement attempt captures a page id/generation with the screen pick and uses that exact page for canonicalization, occupancy, location bounds, terrain, power-item ownership, and commit. If the page ceases to be eligible before commit, the action rejects instead of retargeting.
- **Scope and constraints:** Surfaced from PR #77 / issue #66, with the page dimension introduced by PRs #109/#113. Preserve synchronous click hit-testing, stale-hover rejection, explicit hidden-page spawns for location content, remote-settlement confirmation, construction designations, seam canonicalization, and active-page UI policy. Prefer a page-bearing pick/result or an explicit page parameter through validation and commit rather than several fresh “active” reads.
- **Remaining uncertainty:** Normal UI transitions cancel placement, so the race most likely requires an already-queued page lifecycle command, debug/multi-visible setup, or another asynchronous switch. A deterministic test should suspend after pick, switch/reorder visible pages with deliberately different terrain and location bounds, then resume validation/commit and assert rejection or page-A-only placement.

## 5. Tooltip defaults

### PRR-5. Settings Defaults preserves the live tooltip timing values instead of defaulting them

> **Captured note:** PR #164 added saved snapshots so Back can restore tooltip dwell/hint delays, but the Settings Defaults path reads those two fields from the live engine after loading factory video config. Pressing Defaults therefore treats the player's current tooltip timings as the new defaults and leaves them unchanged.

**Verification:** Reproduced by executing the current `settings.data.loadDefaults` with the engine returning factory video values and live tooltip values `777/888`. After the real function completed, both `data.current` and the setter-observed engine values remained `777/888`. The factory action reset the other video/autosave fields, but no default tooltip source was consulted.

**Evidence:**

- `scripts/settings/data.lua:258-266` declares `loadDefaults` as loading factory defaults and obtains ten fields from `engine.loadDefaultConfig`.
- `scripts/settings/data.lua:289-306` assigns every returned video field, but assigns tooltip dwell/hint by calling the live `engine.getTooltipDwellMs()` / `getTooltipHintDelayMs()`. Those are exactly the mutable preview values PR #164 learned not to trust for Revert.
- `scripts/settings/data.lua:308-328` writes those unchanged values back to the engine, snapshots them as the saved tooltip targets, resets pending state, and separately loads the real default autosave config. There is no later tooltip-default correction.
- `scripts/settings/data.lua:588-645` correctly uses the saved snapshots for Back/Revert; the product bug is specifically that Defaults overwrites those snapshots with live customized values.
- `src/Engine/Scripting/Lua/API/Config.hs:102-122` shows `engine.loadDefaultConfig` owns only `VideoConfig` and returns no tooltip fields. The Defaults caller therefore needs another authoritative default source or those settings need to join the default config schema.
- PR #164 / issue #108 addressed live-preview self-revert but did not test Defaults. Tracker and pending-report searches for tooltip Defaults and factory dwell/hint values found no owner.

**Handoff context:**

- **Current behavior:** Change tooltip dwell/hint, then press Defaults: resolution, windowing, scale, brightness, filtering, and autosave reset, while tooltip timings stay on the customized live values. Back after that also targets those customized values because Defaults refreshed the snapshots.
- **Expected behavior:** Defaults resets every setting presented by the Settings UI, including both tooltip timing fields, to one documented factory source and updates current/pending/saved snapshots coherently.
- **Scope and constraints:** Surfaced from PR #164 / issue #108. Preserve live preview, Back-to-last-saved behavior, Save snapshot refresh, default-video and default-autosave separation, scale fan-out, and config migration. Avoid hard-coding a second `400` if an authoritative default record/config can own both fields.
- **Remaining uncertainty:** The synthetic execution used the production function with stubbed engine calls, not a rendered button click. A focused Settings test should set non-default live values, invoke `settingsMenu.onDefaults`, inspect the rebuilt widgets and engine getters, then Back/Save to specify whether Defaults is immediately persisted or only applied pending the Save action.

## 6. Revert regression harness drift

### PRR-6. The settings Revert regression harness no longer reaches its assertions

> **Captured note:** PR #164 checked in `tools/test_settings_revert.lua` as the regression oracle for its live-preview fix, but later Settings growth added required autosave engine calls without updating the harness. The documented command now crashes in `data.reload()` before testing dwell, hint, save-then-revert, or brightness.

**Verification:** Reproduced directly on current master with the documented repository-root invocation (using the installed compatible `lua` interpreter): `lua tools/test_settings_revert.lua` exited 1 at `scripts/settings/data.lua:145`, `attempt to call a nil value (field 'getSaveConfig')`. No `PASS`/`FAIL` assertion line ran. The harness stubs the old video/tooltip surface but not `getSaveConfig`, `getDefaultSaveConfig`, or `setSaveConfig` introduced when Settings gained autosave.

**Evidence:**

- `tools/test_settings_revert.lua:1-12` calls itself the offline regression harness for issue #108, gives a repository-root command, and defines PASS/FAIL as the expected result.
- `tools/test_settings_revert.lua:22-46` supplies the old engine double. It stops after video/tooltip setters and lacks all three current save-config functions.
- `tools/test_settings_revert.lua:59-88` begins every scenario through `data.reload` or later `data.save`, so the first missing autosave method prevents all four intended assertions from executing.
- `scripts/settings/data.lua:138-224` makes autosave a real Settings family; `reload` calls `reloadSave`, `save` calls `saveSaveConfig`, Revert calls `revertSave`, and Defaults calls `loadDefaultSaveConfig`. A current unit double must model that surface.
- The current Hspec Settings tests stub `data.loadDefaults`/`revert` for layout fan-out and do not execute this standalone harness's issue-#108 scenarios end to end. The broken tool is not listed as a default CI gate, so it can remain red unnoticed.
- Tracker and pending-report searches for `test_settings_revert.lua`, its missing `getSaveConfig`, and the offline harness found no owner.

**Handoff context:**

- **Current behavior:** The one artifact named as PR #164's regression check cannot distinguish the original self-revert bug from correct behavior. A developer following its header gets an unrelated missing-stub exception.
- **Expected behavior:** The documented command runs against the current Settings contract and deterministically checks dwell, hint, save-then-revert, and defined brightness. Prefer moving the cases into the normal headless Lua suite if that gives them a maintained engine fixture and CI visibility.
- **Scope and constraints:** Surfaced from PR #164 / issue #108, with drift introduced by later autosave Settings integration. Preserve an offline-seconds feedback loop or replace the tool with an equally cheap focused Hspec group; do not make the harness write real local config files or notify a live autosave scheduler.
- **Remaining uncertainty:** The installed command was `lua` rather than the header's `luajit`, but the failure is an ordinary nil field in the mocked engine and is interpreter-independent. The processor should decide whether this standalone tool still has value or should be retired after porting every assertion.

## 7. New-session Lua state teardown

### PRR-7. Exit-to-menu teardown leaves per-entity Lua state accumulating across new sessions

> **Captured note:** PR #121 made `world.destroyAll` clear Haskell unit/building managers, but the long-lived `unit_ai` and `building_spawn` singletons are only pruned on save-load or engine shutdown. Repeated Exit to Menu → New Game cycles preserve dead per-unit/per-building rows for the life of the process even though snapshots later filter them out.

**Verification:** Verified structurally as retained memory/state; no long-running multi-session measurement was made. Both Lua modules deliberately keep table identity under `package.loaded`. Their comments admit dead rows can accumulate and protect persistence by filtering only live ids at snapshot time. `world.destroyAll` queues Haskell `UnitClearAll`/`BuildingClearAll`, but sends no Lua reset broadcast; creating a new generated world resets tutorial state only. Global next-id counters are preserved, which reduces immediate identity-collision risk, so the confirmed effect is unbounded stale table growth and unnecessary scan/storage pressure rather than a demonstrated wrong-unit action.

**Evidence:**

- `src/World/Thread/Command/Basic.hs:75-104` clears world/sim state and queues only `UnitClearAll` and `BuildingClearAll`. No engine-to-Lua lifecycle event accompanies the teardown.
- `scripts/pause_menu.lua:317-349` calls `world.destroyAll`, manually clears a few known transient tools/sessions, and returns to the main menu. It does not call either module's shutdown/prune hook.
- `scripts/unit_ai_core.lua:62-63` stores `aiState` on the shared `package.loaded` singleton. `scripts/unit_ai.lua:432-447` prunes it only in `onSaveLoaded`, while `:493-497` clears it only during script shutdown.
- `scripts/unit_ai_save.lua:291-307` explicitly says `aiState` accumulates entries and never drops them when a unit is destroyed; snapshot filters live ids so the stale rows do not enter a save.
- `scripts/building_spawn.lua:75-76` similarly stores `state` on its singleton. Its snapshot comment at `:437-450` says the table can retain entries for destroyed buildings, and its only wholesale clears are `onSaveLoaded` (`:503-517`) and shutdown (`:584-586`).
- `scripts/world_manager.lua:136-156` identifies generated-world creation as a new session and resets tutorial progress/presentation, but no generic new-session reset reaches the other registered/per-entity Lua owners. Tracker and pending-report searches found no issue owning the retained AI/spawn rows across ordinary new-game cycles.

**Handoff context:**

- **Current behavior:** A normal new game empties the Haskell managers, so old rows become unreachable to active id iteration, but the Lua tables retain every state record until a save-load reconcile or application shutdown. Repeated sessions monotonically grow them; diagnostic/debug access to the singleton can also still observe the stale rows.
- **Expected behavior:** A new-session boundary clears every session-owned Lua table whose Haskell owners were destroyed, through one explicit lifecycle hook/inventory rather than a growing list of `pcall(require(...).clear)` calls in `pause_menu`.
- **Scope and constraints:** Surfaced from PR #121 / epic #101 group D, after later persistent Lua components made the session boundary broader. Preserve package-loaded table identity, save-load rollback/application hooks, monotonic entity ids if intentional, tutorial reset semantics, and queue-ordered Haskell teardown. Do not invoke full script shutdown if that unregisters components or destroys process-lifetime content registries.
- **Remaining uncertainty:** Severity depends on real session counts and row sizes, and monotonic ids prevent the easiest stale-state misattribution. A focused test should create many AI/building rows, call the real exit/new-session path, and inspect singleton counts plus heap growth. The processor may reasonably classify this as no-issue if process-lifetime retention is explicitly accepted, bounded in practice, and documented; otherwise it likely wants a general `onNewSession`/reset-owner registry rather than two local patches.
