# Project Review Findings: PRs #211–#200

These entries record focused evidence from the senior review of the next twelve merged PRs in first-parent order — #211, #210, #209, #188, #207, #206, #205, #204, #203, #202, #201, and #200 — for later one-at-a-time disposition. The window contains exactly those twelve PR merges and no unrelated direct commits.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

PR #211's click/box-select domain exclusion, #210's ordinary gameplay-key gate, #206's unconditional tile/chunk Escape clear, #205's pointer-policy-aware middle-click swallowing, #203's right-click focus release, and #202's repeatable disabled-hand drop all retain their intended contracts in the current descendants. PR #201's surviving alert-debounce cache now resets through the no-payload reset-hook contract. PR #200's original order-preserving rollback remains present, and the newer strict transfer surface passed all 40 focused `Unit transfer Lua API` examples. The focused `UI.InputOwnership` suite passed 33/33, including its middle-click and modal-Escape cases. Three short headless debug sessions additionally reproduced an off-page unit selection surviving the Escape predicate, a cross-page legacy cargo deposit succeeding, and a failed `structure.place` growing the persistent texture palette. No graphical/offscreen session, forced chunk-eviction race, full suite, world check, or `make ci` was run. Four non-duplicate concerns remain.

## Status

- [x] PRR-1. Escape leaves a hidden page's unit selection alive — [#1672]
- [ ] PRR-2. Unit AI can combine actor and building snapshots from different pages
- [ ] PRR-3. Chunk eviction can turn an accepted structure placement into a staged phantom
- [ ] PRR-4. Rejected structure placements still grow the persistent texture palette

## 1. Selection cancellation ownership

### [#1672] PRR-1. Escape leaves a hidden page's unit selection alive

> **Captured note:** Escape applies the exact off-page guard that PR #207 removed for buildings to the globally stored unit selection, so a hidden page's selected unit can survive and reappear.

**Verification:** Reproduced headless and verified structurally. The unit manager stores one global `umSelected` set, but `unit.getSelected()` filters that set to the active page. `init_keys` calls the global `unit.deselectAll()` only when that filtered read is non-empty. In a two-arena session, unit #1 was selected on `prr_a`; after hiding it and showing `prr_b`, `unit.getSelected()` returned zero, the Escape predicate skipped `deselectAll`, and showing `prr_a` again returned the same one selected unit. The adjacent building/item code is already unconditional for precisely this reason.

**Evidence:**

- `scripts/init_keys.lua:148-164` — the comment explains that an active-page-filtered building read must not guard a global clear, then immediately guards the unit clear with `#unit.getSelected() > 0`. Building and item clears are unconditional.
- `src/Unit/Selection.hs:34-48,56-60,78-84` — selection is stored globally in `umSelected`; `getSelected` filters it through `onActivePage`, while `clearSelection` empties the underlying set without a page predicate.
- `src/Engine/Scripting/Lua/API/Units/Selection.hs:54-66` — `unit.deselectAll()` always invokes the global clear, whereas `unit.getSelected()` exposes the filtered read. There is no safety or cost reason to guard the former with the latter.
- `scripts/ui/view_teardown.lua:262-284` — lifecycle teardown has explicit ground-item and global-building selection entries but no corresponding unit-selection entry, so hiding a page does not independently close this hole.
- PR #207's second commit (`efc352e4`) records the same bug in its first patch: `building.getSelected()`/`item.getSelected()` were active-page-filtered while `deselect()` was global, so guarded clears stranded selections until the page was shown again. The correction did not apply the same reasoning to the pre-existing unit guard immediately above it.
- Focused headless reproduction: select unit #1 on `prr_a` (`getSelected` count 1), hide A/show `prr_b` (count 0), run the production Escape predicate (count 0, no clear), then hide B/show A (count 1). Tracker and pending-report searches found no current owner for this unit-specific residual.

**Handoff context:**

- **Current behavior:** Escape clears units selected on the current page, plus every building/item selection globally. If `umSelected` points at a live unit on a hidden page, the filtered read makes Escape skip the unit clear and that selection becomes visible again when its page is shown.
- **Expected behavior:** The final gameplay Escape cleanup should clear the global unit-selection domain just as unconditionally as the global building-selection domain. A cancellation gesture must not depend on whether the selected entity is currently reportable through a page-filtered read.
- **Scope and constraints:** Surfaced from PR #207 / issue #177. Preserve active-page filtering for selection reads and new selection attempts, multi-unit Shift selection, menu/modal Escape ordering, and per-world cursor ownership. `unit.deselectAll()` is already idempotent and global; no selection storage redesign is required for the narrow fix.
- **Remaining uncertainty:** The direct predicate was driven through the real Lua/Haskell selection APIs, but not through a synthetic physical Escape key event with a rendered HUD. A focused regression should create two pages, keep the first unit selected across the switch, invoke the real `game.onKeyDown("Escape")`, and assert the raw global selection remains empty after switching back.

## 2. Active-page action ownership

### PRR-2. Unit AI can combine actor and building snapshots from different pages

> **Captured note:** `unit.getAllIds()` and `building.getActiveIds()` independently resolve “active”; a page switch between them can pair an old-page actor with a new-page building, and the legacy transfer verbs accept that cross-page pair.

**Verification:** Partially reproduced. A scheduling barrier was not added inside `unitAi.update`, but the interleaving is present in the current call graph and the unsafe commit behavior was reproduced directly. `unitAi.update` snapshots active-page unit ids, then target finders later take a separate active-page building snapshot. `world.show`/`hide` are asynchronous world-thread commands, so the active page can change between those HsLua calls while global `unit.getInfo`/`building.getInfo` keep both entities resolvable. In a two-arena headless session, an acolyte owned by `prr_a` and a cargo hold owned by `prr_b` were passed to the production `unit.depositToCargo`; it returned `true`, reduced the A unit's inventory from 8 to 7, and grew the B building's storage from 0 to 1.

**Evidence:**

- `scripts/unit_ai.lua:467-489` — each AI update snapshots `unit.getAllIds()` once and then performs many independent API calls while ticking every returned uid. The snapshot does not carry the page it was resolved against.
- `src/Engine/Scripting/Lua/API/Units/List.hs:37-49,65-72` — `unit.getAllIds()` reads the active page at that instant and returns bare global ids. `unit.getInfo` later resolves a supplied id directly from the global manager (`:136-152`) and therefore continues to expose an old-page actor after a switch.
- `scripts/unit_ai_logistics.lua:47-75,78-109` and `scripts/unit_ai_deliver.lua:59-68` — storage, nearby-build, and delivery targeting call `building.getActiveIds()` later, during the old snapshot's per-unit work. Their comments assume “active-world buildings” necessarily match the actors, but no shared snapshot or owning-page comparison enforces that assumption.
- `src/Engine/Scripting/Lua/API/Buildings/Query.hs:198-216` — `building.getActiveIds()` performs its own independent active-page read and returns bare ids. `building.getInfo` is global and exposes the instance's `page` (`:43-91`), but the AI target finders do not compare it with an actor page.
- `src/Engine/Scripting/Lua/API/World/Lifecycle.hs:504-528` — `world.show` and `world.hide` only enqueue commands. The world thread can change `wmVisible` between any two Lua API calls in the AI update.
- `src/Engine/Scripting/Lua/API/Units/Cargo.hs:189-256` — the legacy deposit checks only that the uid and bid exist, capacity fits, and the named item exists. It never compares `uiPage` with `biPage` or rechecks adjacency before committing. The withdrawal path has the same shape at `:293-344`; build-material delivery similarly pairs global ids without page validation at `:40-90`.
- `src/Engine/Scripting/Lua/API/Units/Transfer.hs:698-705` — the newer strict transfer surface already defines the desired invariant explicitly: `inReach` requires both identical pages and footprint distance ≤ 1. Its focused 40-example suite passed, but the AI still calls the older cargo/build-material verbs.
- Focused headless reproduction used explicit page ownership: unit #1 on `prr_a`, cargo building #1 on `prr_b`, one added `steel_plate`; `unit.depositToCargo(1,1,"steel_plate")` returned true and moved one exact instance across pages. PR #209's own verification only compared `getActiveIds()` after settled page switches. The related PRR-2 in `docs/project_review_237-208.md` concerns an already-enumerated portal completing a spawn into its own now-hidden page; it does not own this mixed actor/target snapshot or cross-page inventory commit.

**Handoff context:**

- **Current behavior:** Ordinarily both lists resolve the same active page. If the page changes after the actor list but before a building finder, old-page units can measure and cache new-page building coordinates. When numeric coordinates happen to be adjacent, legacy deposit/delivery can immediately teleport item instances across worlds; otherwise the old-page unit may receive movement toward coordinates meaningful only on the new page.
- **Expected behavior:** One AI action must bind actors, targets, and commit-time mutations to one page generation. A stale actor snapshot should either finish only against its own owning page or fail eligibility after a page switch, and every inventory/build-material mutator should reject cross-page endpoint pairs regardless of caller correctness.
- **Scope and constraints:** Surfaced from PR #209 / issue #197 and the still-registered PR #200 legacy transfer surfaces. Preserve active-only AI simulation, global unique entity ids, independent world and Lua threads, exact-instance/order-preserving rollback, and the newer strict transfer API's page-plus-adjacency policy. Avoid holding a global lock across Lua action selection; an explicit page token/actor-owned query or commit-time ownership check is sufficient.
- **Remaining uncertainty:** The precise page-switch interval was not forced inside the AI loop, so frequency is unknown. A deterministic test should pause after `unit.getAllIds`, apply hide/show on the world thread, resume storage and delivery actions, and assert no movement, claim, inventory, material-delivery, or AI-state mutation crosses pages. The same audit should cover crafting cargo fetches, which independently call `building.getActiveIds()`.

## 3. Structure staging acknowledgement

### PRR-3. Chunk eviction can turn an accepted structure placement into a staged phantom

> **Captured note:** The Lua side checks that a chunk is loaded before staging, but the world thread checks again later and can drop the queued edit after an intervening eviction; no acknowledgement removes the staged piece.

**Verification:** Verified structurally; the scheduling window was not forced. PR #188's fourth review correctly stopped an already-unloaded target from entering the stage. The current implementation still separates that residency check from the authoritative world-thread commit. Chunk loading/eviction writes the same `wsTilesRef` independently between those steps. If the target is present at the Lua read but absent when `WorldSetStructure` is handled, the handler logs and returns without appending `WeSetStructure`; the earlier stage entry remains the first result consulted by `floorZAt`/`hasAt`. No success/failure acknowledgement or world-thread cleanup exists for the stage key.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Structure.hs:125-153` — the Lua thread reads `wsTilesRef`, accepts `Just _`, writes `wsStructureStageRef`, then enqueues `WorldSetStructure` as three separate operations. The comment treats the first residency result as if it guaranteed what the later handler will do.
- `src/World/Thread/ChunkLoading.hs:99-110` — a chunk-load pass atomically inserts new chunks and evicts distant chunks from `wsTilesRef`. That can land before an already-prepared Lua placement reaches its authoritative handler.
- `src/World/Thread/Command/Edit/Structure.hs:33-55` — the world thread re-reads `wsTilesRef`; `Nothing` only logs “Chunk not loaded” and performs neither `applyEdit` nor `appendEdit`. It does not remove or reject the corresponding staged key.
- `src/Engine/Scripting/Lua/API/Structure.hs:384-400` — structure reads consult `wsStructureStageRef` first and return a staged piece without checking that an authoritative overlay/edit exists. The phantom therefore makes `floorZAt`, `hasAt`, `getAt`, and the union-based `count` report a placement the world dropped.
- Repository-wide `wsStructureStageRef` references show writes only in `structure.place`, `clear`, and `clearAll`, plus reads in the structure queries and initialization. There is no per-command acknowledgement, successful-commit drain, failed-commit cleanup, or generation check.
- `docs/persistence_contract.md:230-235` classifies the stage as excluded in-flight state. A save barrier may drain the queued command, but if the handler dropped it the live phantom is still absent from the edit log and disappears on the next load, making the same API query change across a save/load round trip.
- PR #188's fourth commit (`8206ff20`) explicitly names the invariant: staging an edit the world thread drops leaves a phantom. Its loaded-at-call-time probe does not cover eviction between the two residency checks. Tracker and pending-report searches found no current owner for this residual time-of-check/time-of-use window.

**Handoff context:**

- **Current behavior:** `structure.place` can return true and satisfy same-call builder reads. If its chunk leaves the resident set before the world command commits, no authoritative structure or edit is created, but the per-world stage continues reporting the piece until an explicit clear or session replacement.
- **Expected behavior:** Every accepted staged placement must eventually become one authoritative edit, or be explicitly rejected and removed from staging. Chunk residency must be decided at the commit owner; a failed commit cannot leave a query-visible write-ahead entry.
- **Scope and constraints:** Surfaced from PR #188 / issue #68. Preserve Lua same-call read-your-writes for floor→post→wall building, per-world stage isolation, canonical wrapped coordinates, ordered world-edit replay, and the rule that new structures cannot be authored into an unvalidated unloaded chunk. The world thread remains the sole `wsTilesRef`/edit-log commit owner.
- **Remaining uncertainty:** Ordinary queue latency is small and no deterministic eviction barrier currently exists. A regression fixture should stage at the edge of the resident radius, pause before `WorldSetStructure`, move/load far enough to evict the target, resume, and assert the result is either an authoritative persisted edit or a cleaned failure — never stage-only. It should then save/load to confirm the observable result is stable.

## 4. Failed-placement side effects

### PRR-4. Rejected structure placements still grow the persistent texture palette

> **Captured note:** `structure.place` advertises “false and does nothing” for a missing world or unloaded chunk, but it interns and registers both texture paths before discovering either rejection.

**Verification:** Reproduced headless and verified through the save boundary. With no active world and an empty palette, calling `structure.place(10000,10000,"floor",0,0,0,"assets/prr_unloaded_tex.png","assets/prr_unloaded_face.png")` returned `false`; `structure.paletteCount()` nevertheless changed from 0 to 2. The two runtime handles were also registered, so `unresolvedPaletteIds()` remained empty. The palette is a required persistent session component, which means a failed placement changes future save bytes and consumes stable ids despite creating no structure edit.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Structure.hs:70-74` — the public comment explicitly promises `false (and does nothing)` when there is no active world or the target chunk is unloaded.
- `src/Engine/Scripting/Lua/API/Structure.hs:102-117` — both paths are interned into `rhTexPaletteRef` and both ids/handles inserted into `rhTexPaletteHandlesRef` before `resolveStructurePage` runs.
- `src/Engine/Scripting/Lua/API/Structure.hs:117-155` — only after those mutations does the function discover a missing page (`Nothing`) or unloaded chunk (`lookupChunk` returns `Nothing`) and return false. There is no rollback of either palette mutation.
- `src/World/Save/Component/Session.hs:126-150` — `texture-palette` freezes and persists the allocator plus every `(path,id)` pair from the live `TexPalette`; unused entries are not pruned against structure edits.
- `src/World/Save/Snapshot.hs:255-286` — integrity validation checks only the forward direction (every edit's ids exist in the palette). It deliberately does not reject or remove palette entries referenced by no edit, so the failed-call residue survives a valid save.
- `src/World/Load/Publish.hs:118-122` — a load restores the entire persisted palette, then clears only the session-local handle table. The unused path/id allocation is therefore durable and must be re-resolved like real structure assets after load.
- Focused headless reproduction printed `before=0 result=false after=2 unresolved=0`. This is independent of the eviction race in PRR-3: a settled, immediately rejected call is enough. Tracker and pending-report searches found no issue owning failed-placement palette mutation.

**Handoff context:**

- **Current behavior:** Every call with syntactically valid paths allocates palette ids before target validation. Repeated rejected placements with distinct paths monotonically bloat the required palette component and alter future id allocation, even though callers correctly see `false` and record no accepted world action.
- **Expected behavior:** Target/page/chunk validation should precede durable palette mutation, or the entire palette/stage/queue operation should commit atomically. A false return must leave the palette allocator, path maps, handle map, stage, and edit log unchanged.
- **Scope and constraints:** Surfaced from PR #188 / issue #68. Preserve stable palette ids for successful structure edits, immediate render-handle registration, explicit hidden-page placement, canonical coordinate resolution, and load-time palette integrity. Texture loading performed by the Lua caller is outside this API transaction; only `structure.place`'s own palette bookkeeping needs failure atomicity.
- **Remaining uncertainty:** The reproduction used no active world, while unloaded-chunk and unknown-explicit-page failures follow the same post-intern branches by inspection. A focused API test should exercise all three rejection modes and snapshot the full `TexPalette`/handle maps before and after; a save round trip should prove no unused paths appear in the encoded component.
