# Project Review Findings: PRs #873–#860

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #873, #872, #871, #870, #869, #867, #868, #866, #863, #862, #861, and #860 — plus the three direct first-parent commits (`16038c9d`, `034fd733`, and `7fdb6a84`) in the same window, for later one-at-a-time disposition.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

PRs #873 and #870 retain their codec/apply crash-recovery contracts in the current tree; #871's visual/interactive-bounds implementation remains shared and internally consistent; #872 and #869's contract/compatibility suites still exercise the current registries and tracked fixtures; #868's typed-reference graph has since been extended without exposing a separate current defect in this review; and #862, #861, and #860's persistence layers include the later #1200/#1203/#1204 repairs already recorded in `docs/save_load_findings.md`. Those concerns are not duplicated here. No separate current concern was confirmed for PR #867 or for the three direct commits. In particular, the apparently omitted `World.Slope` path in `16038c9d` was investigated and rejected as a finding: the quick world check deliberately drives bare `--dump`, whose slope layer is opt-in and outside its baselines.

## Status

- [x] PRR-1. Whole-session load assigns the active page's zoom atlas to every page — [#1670]
- [x] PRR-2. A no-payload load retains the replaced session's transient GPU textures — [no-issue]
- [x] PRR-3. Defaults rebuilds drop keyboard control focus on both configuration screens — [#1671]

## 1. Per-page zoom-atlas ownership

### [#1670] PRR-1. Whole-session load assigns the active page's zoom atlas to every page

> **Captured note:** Keep a loaded zoom atlas attached only to the page whose zoom cache and pixels produced it. The whole-session loader currently stages one atlas for the active page, then writes that atlas metadata into every staged `WorldState`, including pages whose independently built caches describe different worlds.

**Verification:** Verified structurally. Every non-arena page receives its own zoom cache, but only the active page produces atlas pixels. `StagedSession` collapses that result to one optional atlas, publish pairs it with every loaded page, and the render upload writes the resulting `ZoomAtlasInfo` into every target state. The zoom renderer then indexes each page's own cache against the active page's atlas dimensions and texture.

**Evidence:**

- Issue #763 / PR #863 make the load an exact whole-session, multi-page replacement. A save records the full visible-page set at `src/World/Thread/Command/Save/WriteWorld.hs:217-220`, and publication restores every requested visible page at `src/World/Load/Publish.hs:155-178`.
- `src/World/Load/Stage.hs:117-125` stages every saved page while identifying one active page. `:327-342` builds and stores a separate `wsZoomCacheRef` for every generated page, but creates `mZoomAtlasVal` only when that page is active.
- `src/World/Load/Types.hs:60-73` gives the complete `StagedSession` only one `ssZoomAtlas`, not an atlas associated with a `StagedPage` or page id. `src/World/Load/Stage.hs:163-187` selects the one non-`Nothing` result and stores it in that singular field.
- `src/World/Load/Publish.hs:131-138` explicitly sets `targetStates = map spWorldState (ssPages staged)` and attaches the active page's one atlas payload to that complete list.
- `src/Engine/Scripting/Lua/Message/WorldTexture.hs:267-293` derives one texture handle, width, height, and `chunksPerRow` from the payload, then writes that identical `ZoomAtlasInfo` into every target state's `wsZoomAtlasRef`.
- `src/World/Render/Zoom/Quads.hs:52-58` renders every `wmVisible` page. `:67-83` reads each page's own `wsZoomCacheRef` together with its assigned `wsZoomAtlasRef`.
- `src/World/Render/Zoom/Bake.hs:104-132` uses the cache entry's index to compute a row and column from the assigned atlas's dimensions and `chunksPerRow`. An inactive page therefore samples the active world's pixels; when its cache is longer or differently shaped, its UV calculation can also extend beyond the layout that texture actually contains.
- The ordinary one-page initialization path gets the ownership right: `src/World/Thread/Command/Init.hs:270-287` pairs a newly generated atlas with `[worldState]`, the sole page whose cache produced it.
- `test-headless/Test/Headless/World/Identity.hs:249-345` stages a real multi-page save and verifies ids/identity, but never examines per-page zoom-atlas ownership. The focused no-preview publication example passed during this review; it likewise checks only preview-generation invalidation, not atlas/cache pairing. Tracker and findings-report searches found no existing owner for this mismatch.

**Handoff context:**

- **Current behavior:** Loading a save with an active generated page and another visible generated page gives both pages the active world's atlas. At zoom-map scale, the inactive page's cache bakes quads against pixels and layout metadata belonging to the wrong world.
- **Expected behavior:** Only the exact page whose cache produced an atlas receives it. Other pages may retain the existing per-material fallback, or staging/upload may carry one independently owned atlas per page; no `WorldState` may receive an atlas built from another page's cache.
- **Scope and constraints:** Surfaced in PR #863 / issue #763. Preserve exact saved page ids and visibility, atomic publication, the no-TOCTOU rule of capturing target `WorldState`s at enqueue time, render-thread-only Vulkan ownership, and the valid no-atlas fallback. Runtime texture handles remain rebuilt state, never persisted.
- **Remaining uncertainty:** This review did not launch a graphical multi-visible-world save. The bad cache/atlas association is direct in the current data flow, while the exact visual severity depends on the two pages' generation parameters and which portions overlap in the shared camera.

## 2. No-payload load teardown

### [no-issue] PRR-2. A no-payload load retains the replaced session's transient GPU textures

> **Disposition:** No issue — verified but inconsequential. Publication's `Just`-only writes (`src/World/Load/Publish.hs:137-138`, `:152-153`) do leave the replaced session's preview and zoom-atlas textures registered, but nothing renders from `previewTexture`/`zoomAtlasTexture` — their only readers are the disposal sites in `Engine/Scripting/Lua/Message/WorldTexture.hs` and `Engine/Loop/Shutdown.hs:80-81` — at most one generation of each is retained rather than accumulating, and the path is reachable only through a save whose ACTIVE page is an arena (`src/World/Load/Stage.hs:311-312`), which `CLAUDE.md` already directs developers not to create.

> **Captured note:** Give a successful load an explicit render-thread teardown result when the replacement session has no zoom atlas or preview. Treating `Nothing` as "perform no write" leaves both the old single-slot payload and the already-uploaded texture alive even though issue #763 requires old-session resources to be disposed after commit.

**Verification:** Verified structurally, with bounded impact. A real arena-page load produces neither payload. Publication invalidates preview delivery, but it neither clears the old handoff slots nor asks the render owner to dispose the current preview/atlas textures. Those textures are destroyed only when a later non-empty upload replaces them or at engine shutdown, so this is retained stale GPU state rather than an unbounded allocation on every arena reload.

**Evidence:**

- Issue #763 requirement 15 says failure before commit must leave the old session intact, then after commit the loader must dispose old worlds, staged caches, Lua state, and other resources. PR #863's module header likewise describes post-commit teardown at `src/World/Load/Publish.hs:15-17`.
- `src/World/Load/Stage.hs:290-325` is the real arena reconstruction branch and returns `Nothing` for both the atlas and preview.
- `src/World/Load/Publish.hs:131-153` writes `zoomAtlasDataRef` and `worldPreviewRef` only inside `forM_` over a `Just`. On `Nothing`, it leaves any older pending payload in the slot. The unconditional preview-generation bump prevents stale Lua delivery, but does not reclaim the upload or its texture.
- `src/Engine/Scripting/Lua/Message/WorldTexture.hs:65-71` and `:186-192` consume the two handoff slots. If an old payload remained pending, the main thread can still upload it after the new session published; the atlas's captured target states keep it away from the new worlds, but the resulting GPU texture is immediately obsolete.
- `src/Engine/Scripting/Lua/Message/WorldTexture.hs:136-143` disposes the prior preview only while installing a new preview. `:258-265` does the same for the atlas. There is no no-replacement branch that unregisters the bindless slot, runs the cleanup, and sets the corresponding `GraphicsState` field to `Nothing`.
- `src/Engine/Loop/Shutdown.hs:72-79` is the only remaining cleanup path when no later generated world supplies a replacement.
- `src/World/Load/Publish.hs:239-296` clears a broad set of old-session transient input/UI/event state, but not these render-owned handoffs or `GraphicsState` textures.
- The focused `publishStagedSession invalidates in-flight preview uploads` test passed with one example during this review. `test-headless/Test/Headless/World/Identity.hs:431-481` deliberately forces `ssPreview = Nothing` and asserts only that `worldPreviewGenerationRef` advanced; it does not seed/assert `worldPreviewRef`, `previewTexture`, `zoomAtlasDataRef`, or `zoomAtlasTexture`. Tracker and report searches found no existing owner for the teardown gap.

**Handoff context:**

- **Current behavior:** Loading an arena/no-preview session can leave the replaced session's preview and zoom-atlas GPU allocations registered until some later generated-world upload or process shutdown. A not-yet-consumed old payload may even be uploaded after publication, only for its Lua announcement or old-world targets to be discarded.
- **Expected behavior:** Once publication succeeds, `Nothing` means the replacement owns no texture of that kind: pending old data is cleared and the render thread unregisters/disposes the old transient texture, leaving the relevant `GraphicsState` field empty. A failed pre-commit load still leaves the old texture untouched.
- **Scope and constraints:** Surfaced in PR #863 / issue #763. Marshal cleanup to the main/render owner; never call Vulkan from the world thread. Preserve the preview generation delivery gate and atlas target-state anti-TOCTOU design. This is separate from PRR-1: correct per-page ownership is needed when payloads exist, while explicit teardown is needed when none exists.
- **Remaining uncertainty:** No live Vulkan allocation measurement was taken. The code proves retention and the issue contract requires cleanup, but the practical cost is bounded to the current preview and atlas generations rather than accumulating one fresh pair on every no-payload load.

## 3. Defaults-action keyboard focus

### [#1671] PRR-3. Defaults rebuilds drop keyboard control focus on both configuration screens

> **Captured note:** Snapshot keyboard control focus by name before the Settings and Create World Defaults actions destroy their pages, then restore it after the rebuilt page is shown. Resetting pending configuration is intentional; clearing the user's keyboard navigation position is a separate side effect of page recreation.

**Verification:** Verified structurally; whether Defaults is intended as an exception to the general focus-preservation contract remains a product decision. Both Defaults handlers rebuild their whole page without the focus wrapper used by their resize paths. Page/element deletion unconditionally clears `upmControlFocus`, so keyboard activation of Defaults ends with no control focused even though a same-named Defaults control exists on the rebuilt visible page.

**Evidence:**

- Issue #748 / PR #866 require eligible focus and keyboard reachability to survive responsive rebuilds. Current repository guidance broadens that into the rule at `CLAUDE.md:391-393`: use `responsive.snapshotControlFocusName()` / `restoreControlFocusName()` around any destroy-and-recreate, after the page is re-shown.
- `scripts/settings_menu.lua:155-180` runs `data.loadDefaults()`, calls `settingsMenu.createUI()`, and re-shows the page without snapshotting/restoring control focus. `:342-386` shows that `createUI` destroys the owned controls and deletes the old page.
- `scripts/create_world_menu.lua:1059-1092` deliberately resets pending generation values, then calls `createUI({ preserveState = false })` and re-shows the page with no control-focus handoff. `:351-360` correctly distinguishes semantic data reset from ordinary state preservation, but the caller still does not preserve the independent control-focus identity; the rebuild deletes its page at `:431`.
- `src/UI/Manager/Core.hs:17-39` clears `upmControlFocus` whenever the focused element is deleted. The outcome is immediate and intentional hygiene for dead handles; restoration must therefore be performed by the rebuilding screen.
- `scripts/ui/responsive.lua:93-119` provides the by-name snapshot/restore mechanism. Both screens already use it correctly in their `onFramebufferResize` paths (`scripts/settings_menu.lua:1062-1066`; `scripts/create_world_menu.lua:1174-1179`).
- The focused `keyboard control focus (#745) survives a resize rebuild` group passed with five examples during this review. Its Settings and Create World cases at `test-headless/Test/Headless/UI/ResponsiveMenus.hs:1590-1631` exercise only `onFramebufferResize`. The Defaults tests at `:1360-1415` assert scale fan-out behavior, not focus after the rebuild.
- Tracker and findings-report searches found no separate issue for Defaults-action focus loss.

**Handoff context:**

- **Current behavior:** A keyboard user can Tab to Defaults and activate it with Enter/Space; the action recreates the screen and clears control focus. The next Enter/Space/arrow has no target, and Tab restarts traversal instead of continuing from the rebuilt Defaults control.
- **Expected behavior:** The Defaults action still resets all configuration state it owns, but restores keyboard control focus to the matching new control (or to an explicitly documented deterministic fallback) once the replacement page is visible.
- **Scope and constraints:** Surfaced in PR #866 / issue #748. Cover both Settings and Create World, preserve their deliberately different pending-state reset semantics, do not restore stale element handles, and do not re-fire the Defaults callback during restoration.
- **Remaining uncertainty:** PR #866's summary describes Defaults as a semantic fresh reset for pending values, so the processor should confirm that focus was not intentionally included in that reset. The current general rebuild guidance, the existing resize implementation, and the concrete no-focus outcome all favor preserving it.
