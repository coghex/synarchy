# Project Review Findings: PRs #279–#261

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #279, #278, #272, #271, #260, #269, #268, #270, #266, #267, #263, and #261 — for later one-at-a-time disposition. The first-parent window contains exactly those twelve PR merges and no direct commits.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

PR #279's original soil-shedding omission was closed by the later erosion-credit work; #278's portrait loader still works, while its handle-zero collision is already owned by `project_review_292-281.md` PRR-1; #272's generated names survive the current component save codec and its focused tests pass; #271's load-time structural rebind was superseded by current cache-generation invalidation and stable texture handles; #268, #266, and #267 now clear or hide their state through the centralized view lifecycle; #263's live click pick remains sound, while the direct tile-selection arming asymmetry is already recorded in `project_review_823-789.md` PRR-1; and #261's seed-1337 terrain spike is absent from current baselines. Focused checks passed for generated-name selection (5/5) and the audit self-test (35/35). A current seed-12321 scan found no naturally occurring enclosed dry tile matching the lake-audit blind spot, but a synthetic mixed-surface enclosure reproduced it. No full headless suite, graphical session, worldgen full tier, baseline capture, behavior probe, or `make ci` was run. Four non-duplicate concerns remain; the final three are deliberately retained as structural or latent risks even where current shipped data did not reproduce a player-visible symptom.

## Status

- [x] PRR-1. Popup and event-log coordinates can navigate the wrong world page — [#1588]
- [ ] PRR-2. Dismiss-all cannot clear queued popups before bootstrap
- [ ] PRR-3. The lake-hole audit ignores higher surrounding lake surfaces
- [ ] PRR-4. Nested item identity collapses distinct child-instance state

## 1. Page-scoped popup navigation

### [#1588] PRR-1. Popup and event-log coordinates can navigate the wrong world page

> **Captured note:** Carry the originating world-page identity with every popup coordinate and enforce it when a card or event-log row is activated. A historical coordinate must not be interpreted in whichever page happens to be active later.

**Verification:** Verified structurally. Engine events retain an optional source page in the event store, but the live-popup message has no page field and event-log replay drops the stored page before rebuilding a popup. The popup's coordinate link then calls `camera.goToTile` against the active page without checking whether it is the page that produced the event. Current discovery emission suppresses coordinates for a page that is already inactive, but neither that emission-time guard nor HUD teardown protects a stored event that is activated after a later page transition.

**Evidence:**

- Issue #37 / PR #270 introduced queued notification cards and clickable coordinate lines. The issue required coordinates to pan the camera, but did not define ownership when the engine has more than one world page.
- `src/Engine/PlayerEvent.hs:22-55` stores `peCoords` and an independent optional `peSourcePage`. Its source-page comment explicitly distinguishes events from the active page from coordinates that belong to another page.
- `src/Engine/PlayerEvent/Emit.hs:107-137` records the source page in the persistent event entry, then constructs `LuaShowPopup` with only type, title, message, and coordinates. The page identity is lost on the immediate popup path.
- `src/Engine/Scripting/Lua/Types.hs:291-299` gives `LuaShowPopup` no page/world field, so Lua cannot validate the coordinate even if the Haskell producer knew its owner.
- `src/Engine/Scripting/Lua/API/PlayerEvent.hs:88-145` does expose both `coords` and `page` when the event log queries stored entries. `scripts/event_log.lua:671-700` nevertheless passes only `ev.coords` to `popup.onShowPopup` when replaying one, dropping `ev.page` a second time.
- `scripts/popup.lua:684-718` makes the location line clickable and directly calls `camera.goToTile(target.x, target.y)`. It neither compares a source page with `world.getActiveWorldId()` nor switches pages before interpreting the coordinate.
- `src/World/Thread/Discovery.hs:43-91` emits discovery coordinates only while their page is active and tags the stored event with that page. This prevents an already-background page from producing a live jump, but the event ring outlives the active-page choice and can be opened later.
- `scripts/ui/view_teardown.lua:225-236` hides the event-log view and dismisses current cards during a HUD transition; it does not clear the session event ring. A later log opening can therefore replay the old page's coordinate into the new page.
- Unit events are generally emitted from the active-page unit inventory, so their immediate path is less exposed, but their stored coordinate has the same historical-page ambiguity after a transition.
- Tracker and pending-report searches for popup coordinates, event-log page identity, and cross-world camera jumps found no owner. The older direct-selection finding concerns arming and stale clicks, not interpretation of a valid coordinate from another page.

**Handoff context:**

- **Current behavior:** Clicking a live card or replayed event with coordinates always pans the active world. If the event came from a formerly active page, the same numeric `(x, y)` is interpreted in an unrelated world's coordinate space. The player can be taken to an arbitrary tile while the UI presents the navigation as the event's location.
- **Expected behavior:** A navigable event carries a durable world/page id from emission through storage, Lua delivery, replay, and click activation. Activation either switches to that page through the supported world-selection path before panning, or refuses the jump with a clear unavailable-location state when the page no longer exists. Events without a world owner remain non-navigable or explicitly active-page-relative.
- **Scope and constraints:** Surfaced from PR #270 / issue #37. Preserve popup queuing, event-ring persistence, current card formatting, main-menu/system events that have no world page, and the rule that worker threads do not mutate the active-page selection. Prefer one shared navigation helper for immediate cards and event-log replay so the two paths cannot drop different metadata.
- **Remaining uncertainty:** This review did not run a multi-page graphical click reproduction. The metadata loss and active-page camera call are direct; the exact transition through which a player encounters it depends on the current multi-world/test-arena entry points and whether a particular event type exposes coordinates.

## 2. Pre-bootstrap popup queue teardown

### PRR-2. Dismiss-all cannot clear queued popups before bootstrap

> **Captured note:** Make popup teardown clear pending notifications even when no card is active and the popup UI has not bootstrapped. Queue lifetime must follow the same view/session boundary as active cards rather than leaking an old menu or world event into the next gameplay bootstrap.

**Verification:** Verified as a reachable state-machine asymmetry; the visible delayed-card symptom was not exercised in an offscreen session. Popups accept and queue events before their gameplay UI is bootstrapped. `dismissAll` intends to clear both active cards and the pending queue, but returns immediately when the active-card list is empty, which is necessarily true in that pre-bootstrap state. A later gameplay bootstrap drains the untouched queue.

**Evidence:**

- PR #270 added both a pre-display queue and `dismissAll` lifecycle cleanup. The combination creates a third state — queued but no active cards — that the cleanup's early return does not cover.
- `scripts/popup.lua:660-677` makes `drainQueue` a no-op until `popup.bootstrapped` is true. `scripts/popup.lua:761-786` still appends incoming notifications to `popup.queue`, so pre-bootstrap delivery is explicitly supported rather than rejected.
- `scripts/popup.lua:943-949` returns from `dismissAll` when `#popup.active == 0`. The queue-clearing assignment occurs only after that return, making it unreachable for a queue-only state.
- `scripts/ui_manager_boot.lua:274-289` bootstraps popups and the event log lazily from `ensureGameplayUI`; normal startup's `finishStartupBoot` path initializes the main menu without ensuring those gameplay surfaces. The queue-only state therefore exists during ordinary startup, not only during a test stub.
- `scripts/main_menu.lua:414-423` can emit a `save_load` player event when a synchronous load request is rejected while the user remains on the menu. At that point the notification can enter the unbootstrapped queue with no active card.
- When the player later enters a world, `ensureGameplayUI` bootstraps the popup module and `drainQueue` presents the retained event. Any intervening teardown call to `dismissAll` would have claimed success while leaving that queue intact.
- The event-log store has an explicit session/history role; the popup queue does not. Keeping a record for later inspection therefore does not require displaying a stale transient card after the view boundary.
- Tracker and pending-report searches for pre-bootstrap popup queues, queue-only dismissal, and delayed startup notifications found no owner.

**Handoff context:**

- **Current behavior:** `dismissAll` clears queued notifications only as a side effect of dismissing at least one active card. Before bootstrap there cannot be an active card, so queued notifications survive the cleanup and appear when a later gameplay bootstrap drains them.
- **Expected behavior:** Teardown always empties the pending queue, independently of bootstrap state or active-card count, and dismisses active cards when present. If some notification classes are intentionally durable across bootstrap, that policy is explicit and separate from the transient popup queue rather than an accidental early-return consequence.
- **Scope and constraints:** Surfaced from PR #270 / issue #37. Preserve lazy gameplay UI bootstrap, bounded active-card count, event-log persistence, card dismissal animation where UI elements exist, and safe calls before handles/pages have been created. A Lua regression can cover a queue-only module state without requiring GPU rendering.
- **Remaining uncertainty:** The exact main-menu-to-game delayed card was inferred from the synchronous load-error emission and lazy bootstrap and was not observed visually. It remains possible that product intent is to deliver every queued pre-bootstrap event later, but that would conflict with `dismissAll`'s existing queue-clearing branch and should be documented and tested rather than depend on whether an active card happens to exist.

## 3. Mixed-surface lake-hole audit

### PRR-3. The lake-hole audit ignores higher surrounding lake surfaces

> **Captured note:** Define lake-hole and submerged-bump geometry against all relevant surrounding water surfaces, not only the lowest one. A dry enclosed tile that sits below one or more adjacent lake surfaces should not evade the audit merely because another adjacent lake has a lower surface.

**Verification:** Reproduced with a focused synthetic grid, but not found in the current six-seed quick-style sample inspected during this review. A dry center at terrain 11 surrounded by lake surfaces `[10, 12, 12, 12]` is below three adjacent water surfaces, yet both `check_lake_hole` and `check_submerged_bump` return no finding because they compare the terrain only with `min(surrounding surfaces)`. Current baseline evidence shows that adjacent lake cells can have unequal surfaces, so the premise is real even though the exact enclosed-dry-cell arrangement was not located in seed 12321.

**Evidence:**

- Issue #21 / PR #269 introduced `LAKE_HOLE`, `SUBMERGED_BUMP`, and water-cliff audits to distinguish genuine terrain defects from water-level geometry.
- `tools/world_audit.py:358-396` gathers the surrounding lake surfaces, selects their minimum, and reports a lake hole only when the dry terrain is below that minimum. One low neighbouring lake therefore suppresses evidence that the same dry tile lies below every higher neighbour.
- The submerged-bump classifier immediately below uses the same lowest-surface reference, so it does not recover the mixed-surface case under another category.
- A five-tile synthetic enclosure with center terrain 11 and four lake surfaces 10, 12, 12, and 12 produced `[]` from both classifiers. Raising the lowest neighbour above 11 makes the classification appear, confirming that the minimum is the sole suppressing condition.
- Current seed-12321 baseline data contains 2,767 `WATER_WATER_CLIFF` findings, including 98 representative higher-side cells classified as lake. These examples establish that non-flat lake-to-lake boundaries exist in accepted output; the audit cannot assume every surrounding lake shares one surface.
- A direct scan of current seed 12321 found zero fully lake-surrounded dry tiles below the minimum, below any neighbour, or specifically between the minimum and maximum. That narrows current observed impact but does not validate the predicate for other seeds or future generator changes.
- `python3 tools/test_audit.py` passed all 35 groups. Its lake-related threshold and summary checks do not exercise a mixed set of surrounding lake surfaces against the classifier, so the blind spot is compatible with the green self-test.
- Tracker and pending-report searches for mixed lake surfaces, minimum surrounding water level, and enclosed dry lake cells found no owner.

**Handoff context:**

- **Current behavior:** The audit treats the lowest adjacent lake surface as the only submergence threshold. In a mixed-surface enclosure it can declare a dry tile harmless even when water from higher neighbouring lake cells geometrically stands above that terrain.
- **Expected behavior:** The classifier reflects the intended physical invariant explicitly: compare per edge, use the highest relevant connected surface, or first prove/group which surrounding cells belong to one hydrologic surface. A focused test fixes the expected category for mixed lower/equal/higher neighbours and prevents threshold summaries from masking classifier gaps.
- **Scope and constraints:** Surfaced from PR #269 / issue #21. This is an audit correction, not authorization to change worldgen output or baselines. Preserve the distinction between lake holes, submerged bumps, legitimate shore slopes, and cross-body water cliffs; consult `docs/hydrology_pipeline.md` before deciding whether adjacent unequal lakes should be treated as one body.
- **Remaining uncertainty:** No current baseline occurrence of the exact topology was found, and an unequal adjacent lake surface may sometimes denote separate bodies whose levels should not be combined. The processor should validate hydrologic connectivity and scan more seeds before choosing severity or the precise predicate. The synthetic case proves only that the current minimum-based test cannot express the broader per-neighbour invariant.

## 4. Nested item-instance identity

### PRR-4. Nested item identity collapses distinct child-instance state

> **Captured note:** Preserve all gameplay-relevant child-instance state when a container contributes to an item row's identity, and do not summarize nested contents solely by definition name. Containers whose children differ in quality, weight, temperature, sharpness, fill, or condition must not be treated as interchangeable or expose one arbitrary representative as the whole group.

**Verification:** Verified structurally against the supported item schema; no currently shipped container was found that manifests every collision. The recursive contents signature used by the UI grouping key omits child quality, weight, and temperature. The Lua contents API is coarser still: it groups children only by definition name and retains one representative's selected values. Current first-aid-kit data duplicates mainly stateless supplies and has only one copy of each condition-bearing tool, so the authored data does not yet make the defect conspicuous.

**Evidence:**

- Issue #67 / PR #260 introduced unique item-instance ids and container-aware inventory behavior. Current UI grouping deliberately permits interchangeable instances to share a row while actions target a representative id, making the equality key a correctness boundary.
- `src/Item/Types.hs:218-287` defines per-instance quality, condition, weight, sharpness, contents, id, and temperature/fill state. These fields can affect display, transfer, use, spoilage, or later simulation independently of the child definition name.
- `src/Item/Types.hs:289-308` builds `itemContentsSig` from child definition name, fill, condition, sharpness, and the recursive contents signature. It omits child quality, realized weight, and temperature, so two parent containers can receive the same contents key despite materially distinct children.
- `scripts/ui/item_list.lua:85-106` states that instances sharing its key are interchangeable. The outer row includes the parent item's weight plus `contentsKey`, but equal total parent weight cannot identify how weight or other state is distributed among nested children, and it does not restore omitted quality or temperature.
- `src/Engine/Scripting/Lua/API/Units/Inventory.hs:460-535` implements `unit.getItemContents` by grouping all direct children by `defName`. It returns a count with one representative's fill, condition, and weight and omits quality, sharpness, temperature, and child id, so a Lua caller cannot distinguish or select heterogeneous children in one definition group.
- `data/items/first_aid_kit.yaml:12-21` is the current authored container. Its repeated bandages, gauze, and wraps do not carry the omitted state in their current definitions, while tweezers and scissors have condition rolls but count one each. That explains why ordinary inventory display does not yet expose a collision.
- `data/items/medical_supplies.yaml:54-70` shows that nested-capable item definitions can already have rolled condition. `src/Unit/Thread/Command/Spawn.hs:329-371` recursively materializes default contents through the normal item-spec roll path, so the schema does not restrict future containers to homogeneous stateless children.
- Open issue #1238 plans a nested container-window stack and explicitly preserves the current definition-name grouping. It is related implementation context, not an owner for the equality/state-loss problem; carrying the current grouping forward would make the ambiguity user-facing once nested contents become directly actionable.
- Tracker and pending-report searches found no issue that owns complete nested-item equality or heterogeneous same-definition child exposure. Related issue #1013 concerns other inventory identity behavior and does not define this contract.

**Handoff context:**

- **Current behavior:** Parent containers can group into one supposedly interchangeable UI row even when their nested children differ in omitted instance fields. When Lua asks for contents, same-definition children collapse into one count and one representative state, preventing faithful display or selection of the actual instances.
- **Expected behavior:** The recursive grouping signature includes every field that makes two child instances non-interchangeable for current gameplay, or the UI groups only after a narrower explicit equivalence proof. The contents API returns distinguishable subgroups or individual ids for heterogeneous children, while truly identical stacks can remain compact.
- **Scope and constraints:** Surfaced from PR #260 / issue #67. Coordinate with open #1238 before freezing a nested-container API. Preserve stable top-level item ids, compact presentation of genuinely identical supplies, recursive containers, save compatibility, deterministic row ordering, and existing Lua callers that expect aggregate counts. Avoid making volatile display-only fields churn groups unless they affect an available action or promised identity.
- **Remaining uncertainty:** The practical symptom is latent in current shipped data, and the processor must decide which of quality, realized weight, temperature, condition, sharpness, and fill belong to the interchangeability contract. Some omitted fields may be intentionally aggregated today. The concrete loss of distinguishability is nevertheless present in both the signature and Lua projection, and upcoming nested interaction makes that policy decision time-sensitive.
