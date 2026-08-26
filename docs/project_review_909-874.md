# Project Review Findings: PRs #909–#874

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #909, #908, #906, #905, #904, #903, #902, #881, #880, #879, #875, and #874 — plus the two direct first-parent commits (`cb147c8a` and `7400009c`) in the same window, for later one-at-a-time disposition.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

The four capability refactors (#909, #906, #905, and #902) retain live-container projections and passed the focused capability checks; #904's review-round bleeding-trail defects are repaired in the current tree; the #880 and #875 module splits preserve their moved definitions; and #879's probe isolation remains scoped to its test harness. No separate current concern is recorded for those PRs or the two direct commits. The EngineEnv 83-versus-84 documentation mismatch encountered in this window is already PRR-4 in `docs/project_review_938-910.md` and is not duplicated here.

## Status

- [x] PRR-1. Persisted borderless mode boots as a decorated window while reporting borderless — [#1731]
- [ ] PRR-2. A fullscreen boot forgets its configured windowed geometry — [deferred]: behind #1731
- [x] PRR-3. Stale simple-preview completions can create unbounded texture aliases — [no-issue]
- [ ] PRR-4. NaN XP can escape the mental-effectiveness clamps and corrupt combat and crafting

## 1. Persisted borderless startup

### [#1731] PRR-1. Persisted borderless mode boots as a decorated window while reporting borderless

> **Captured note:** Apply the persisted borderless request during graphical startup, or immediately after window creation, and keep the reported mode synchronized with what GLFW actually applied. PR #908 deliberately repaired later mode transitions without closing the already-documented startup mismatch.

**Verification:** Verified structurally and explicitly pinned by the focused regression suite. A saved `borderless` value is reported back through the video-config API, but startup collapses every non-fullscreen value to an ordinary decorated window and records the applied state as `Windowed` until a later mode request runs.

**Evidence:**

- Issue #907 / PR #908 scoped its repair to mode-transition geometry and called the configured-borderless startup behavior an adjacent bug rather than part of that fix.
- `src/Engine/Core/Defaults.hs:42-48` — `defaultWindowConfig` maps only `Fullscreen` to the GLFW fullscreen flag. `Borderless` and `Windowed` both become `wcFullscreen = False`.
- `src/Engine/Graphics/Window/GLFW.hs:69-100` — startup creates an ordinary decorated window and special-cases only fullscreen attachment. There is no borderless decoration/monitor application in the creation path.
- `app/App/Graphical.hs:62-75` — the graphical boot reads configuration, creates the window, and enters the main loop without queuing or applying a startup `LuaSetWindowMode` request.
- `src/Engine/Graphics/Window/GLFW.hs:110-116` — applied-state initialization records every successfully created non-fullscreen window as `Windowed`; its comment explicitly says borderless was never requested from this path.
- `src/Engine/Core/State.hs:570-586` documents the same mismatch: borderless configuration currently starts as a plain decorated window.
- `src/Engine/Scripting/Lua/API/Config.hs:37-51` — `getVideoConfig` returns the persisted `vcWindowMode`, so Lua can report `borderless` while the applied `WindowState` truthfully says `Windowed`.
- `test-headless/Test/Headless/Graphics/WindowMode.hs:224-231` deliberately expects a borderless-configured boot to remain applied `Windowed` until its first explicit request. `tools/video_window_check.py:80-86` and `:324-341` likewise avoid geometry assertions from non-windowed configured starts because requested and applied startup state can disagree.
- The complete `--match "Graphics.WindowMode"` group passed with 24 examples during this review. Tracker searches found only closed #907 and no separate open owner for the startup mismatch.

**Handoff context:**

- **Current behavior:** A user who saves borderless mode and restarts sees an ordinary decorated window, while configuration-facing UI continues to report borderless. Reapplying the same setting is what finally invokes the real borderless transition.
- **Expected behavior:** Startup applies the persisted borderless request, or queues it at the first safe render-thread point, and configuration/applied state do not present contradictory modes after boot.
- **Scope and constraints:** Surfaced in PR #908 / issue #907. Cover both graphical creation paths that share the window initializer, preserve MainRender ownership of GLFW mutations, fullscreen graceful fallback, window-size notifications, and the geometry cache repaired by #908.
- **Remaining uncertainty:** Repository policy precluded launching an interactive window during this audit, but the creation branch, its state initialization, the regression expectation, and the source comments independently establish the mismatch.

## 2. First exit from startup fullscreen

### [deferred] PRR-2. A fullscreen boot forgets its configured windowed geometry

> **Deferred:** Blocked on sequencing behind #1731 — both repairs must seed the
> windowed-geometry cache from the ordinary window `Engine.Graphics.Window.GLFW.createWindow`
> owns *before* any mode attachment, in the same region (`src/Engine/Graphics/Window/GLFW.hs:84-118`,
> whose existing samples are taken after `setFullscreen` and so capture fullscreen geometry).
> Clears when #1731's PR merges: re-read that seeding — if it is unconditional, this finding is
> already fixed and closes as `[no-issue]`; if it is gated on the borderless branch alone, file
> the fullscreen case then.

> **Captured note:** Seed the windowed-geometry cache from the ordinary window that startup creates immediately before a successful fullscreen attachment. PR #908 makes subsequent transitions restore the cache correctly, but the first `Fullscreen -> Windowed` transition still restores the hard-coded 800×600 fallback rather than the configured or pre-fullscreen geometry.

**Verification:** Verified structurally, with the intended fallback semantics left for the processor to settle. The cache is initialized before GLFW creation to `(100,100,800,600)` and the successful fullscreen startup path never replaces it, even though it briefly owns an ordinary window at the configured dimensions.

**Evidence:**

- `src/Engine/Core/Init.hs:177-189` — startup loads the persisted video dimensions into `windowSizeRef`, but initializes `windowStateRef` independently with `defaultWindowState`.
- `src/Engine/Core/State.hs:563-568` — that default state hard-codes the cached windowed position to `(100,100)` and size to `800×600`.
- `src/Engine/Core/Defaults.hs:42-48` and `src/Engine/Graphics/Window/GLFW.hs:69-79` — the initial ordinary window is created at the configured width and height before any fullscreen attachment.
- `src/Engine/Graphics/Window/GLFW.hs:84-116` — successful fullscreen setup records `Fullscreen` but never samples that ordinary window's position or size into the windowed cache. Failed setup truthfully records `Windowed`, which is a separate, intentionally inert case.
- `src/Engine/Scripting/Lua/Message/Video.hs:139-145` — the first later `Windowed` request restores `wsWindowedPos` and `wsWindowedSize` verbatim, so a successful startup-fullscreen session receives the hard-coded fallback.
- `src/Engine/Graphics/Config.hs:131-146` — the default configured dimensions are themselves 800×600. That coincidence masks the size loss unless the persisted configuration uses a custom resolution.
- `test-headless/Test/Headless/Graphics/WindowMode.hs:242-249` accepts the successful-fullscreen boot with only a fallback cache and does not assert that the cache represents the just-created window. The full focused group passed with 24 examples during this review.
- Tracker searches for fullscreen-startup and first-windowed-transition geometry found closed #907 but no separate owner for this initialization gap.

**Handoff context:**

- **Current behavior:** With custom persisted dimensions and a successful fullscreen boot, the first switch to windowed teleports to 800×600 at `(100,100)`. Only a later windowed-to-fullscreen transition has real windowed geometry available to cache.
- **Expected behavior:** A successful startup fullscreen transition preserves meaningful session geometry for its first exit — preferably the position and size of the ordinary window GLFW just created, or at minimum the configured dimensions if position is deliberately unspecified.
- **Scope and constraints:** Surfaced in PR #908 / issue #907. Preserve the repaired later round trip, failed-fullscreen truthfulness, no GLFW calls outside the render owner, and the distinction between requested configuration and applied state.
- **Remaining uncertainty:** The repository does not explicitly promise that `vcWidth`/`vcHeight` are the future windowed fallback while starting fullscreen, and it persists no window position. The code-level fallback loss is certain; the processor should settle whether the pre-attachment OS geometry or configured dimensions are the intended authority.

## 3. Simple-preview texture request identity

### [no-issue] PRR-3. Stale simple-preview completions can create unbounded texture aliases

> **Disposition:** No issue — the mechanism is real (`requestTexture` at
> `scripts/preview_manager.lua:136-146` never caches the path, and the callback at `:497`
> drops any completion whose handle is not the current `pendingHandle`), but its cost is a
> few bookkeeping entries per lost race in a developer-only preview session that already
> documents never unloading textures as an accepted trade-off (`:25-28`). No second image
> upload, no display defect, and the handle table's 65,536-entry ceiling — whose silent
> overflow is separately owned by #1699 — is unreachable by hand. The one-line consistency
> fix (cache at request time, as `acquireTexture` at `:126-132` already does) remains
> welcome inside any future preview change; it does not warrant its own issue.

> **Captured note:** Track simple-browser texture requests by path independently of the currently selected row. A stale completion should populate the loaded-path cache without changing the displayed sprite; otherwise revisiting an abandoned selection allocates another handle alias for the same already-loaded texture every time the callback loses the selection race.

**Verification:** Verified structurally, with impact narrowed to handle/refcount bookkeeping rather than repeated atlas uploads. The preview retains only one pending handle, discards every older completion without caching its path, and calls `engine.loadTexture` again on a later cache miss. The engine deduplicates the underlying loaded path but creates a fresh handle-to-slot alias and increments its reference count for each request.

**Evidence:**

- Issue #886 / PR #903 introduced the simple preview browser, its lazy texture cache, and the asynchronous handle callback being reviewed.
- `scripts/preview_manager.lua:88-102` — the manager has a cache only for completed path entries and one `pendingTextureHandle`/path pair for the current selection; it has no per-path in-flight request table.
- `scripts/preview_manager.lua:127-147` — every cache miss calls `engine.loadTexture`, overwrites the one pending identity, and writes the cache only from the later application path.
- `scripts/preview_manager.lua:460-489` — a texture callback is processed only when its handle equals the current pending handle. A completion from a previously selected row is ignored entirely rather than cached without display.
- Consequently, `A -> B` before A completes discards A's completion; revisiting A is another cache miss and another `loadTexture`. Alternating away before each A callback can repeat the sequence for the lifetime of the preview session.
- `src/Engine/Asset/Manager.hs:54-57` allocates a monotonically fresh `TextureHandle` for each request. `src/Engine/Scripting/Lua/Message/Texture.hs:169-194` recognizes a path that is already loaded and routes it through the cached-alias path rather than uploading it again.
- `src/Engine/Scripting/Lua/Message/Texture.hs:88-126` — `duplicateCachedTextureHandle` inserts the fresh handle-to-slot mapping, marks it ready, increments the atlas entry's `taRefCount`, and adds another size-map entry. Thus the leak is alias/table/refcount growth over the session, not duplicate image storage.
- `scripts/preview_manager.lua:520-550` clears Lua-side state at shutdown but has no texture-unload verb for these abandoned aliases; engine process teardown eventually reclaims the session.
- `tools/preview_probe.py:276-296` waits for the initial row to finish, clicks one other row, and waits again. It never drives `A -> B` before A's callback and then revisits A. Its loaded-path checks also collapse aliases by path, so they cannot observe multiple handles for one texture.
- `luac -p scripts/preview_manager.lua scripts/ui/asset_browser.lua` passed during this review. Tracker and findings-report searches found no existing owner for the stale-callback cache gap.

**Handoff context:**

- **Current behavior:** Rapid selection correctly avoids displaying a stale image, but it forgets that the abandoned path finished loading. Revisit requests allocate new aliases and increment the shared slot's reference count; repeating the race grows handle-indexed engine tables until preview shutdown.
- **Expected behavior:** Each path has at most one request per preview session. Any successful completion records the reusable loaded result for its own path, while only a completion matching the current selection is allowed to change the displayed sprite.
- **Scope and constraints:** Surfaced in PR #903 / issue #886. Limit the repair to the simple list/item preview handshake; unit and building preview acquisition cache their handles immediately and have a different lifecycle. Preserve async size readiness, lazy loading, the stale-display guard, and resize rebuilds that do not re-fire selection callbacks.
- **Remaining uncertainty:** The GUI timing window may be short on a warm cache and was not exercised interactively. The code path is deterministic under delayed or reordered callbacks, and the accumulated resource is bookkeeping around a shared atlas slot rather than repeated GPU texture memory.

## 4. Non-finite mental effectiveness

### PRR-4. NaN XP can escape the mental-effectiveness clamps and corrupt combat and crafting

> **Captured note:** Reject or sanitize non-finite values at the stat/XP boundary and guarantee that mental effectiveness and output quality are finite. Haskell's ordinary comparison-based `clamp` does not contain NaN, so one accepted NaN concentration value defeats issue #353's promised bounds and reaches both combat decisions and persisted crafted-item quality.

**Verification:** Verified directly against the current library. Evaluating `applySkillXP 1 NaN`, the mental-effectiveness formula, `applyMentalQuality`, and `hitChance` produced NaN throughout. The ordinary shipped AI paths supply finite values, so the confirmed ingress is a scripting/debug call to `unit.addXP` (or already-corrupt persisted state), not normal concentration updates.

**Evidence:**

- Issue #353 / PR #874 require final mental effectiveness to remain in its documented band and final item quality to remain within 0–100.
- `src/Engine/Scripting/Lua/API/Units/Stats.hs:172-216` — public `unit.addXP` accepts an arbitrary Lua number, performs no `isNaN`/finite validation, and writes `applySkillXP`'s result into the chosen skill or stat. A concentration stat can therefore receive NaN through this boundary once it exists.
- `src/Unit/Stats.hs:48-60` — `applySkillXP level xp = level + xp / ...` has no non-finite guard, so NaN XP produces a NaN level.
- `src/UPrelude.hs:82-87` — the project `clamp` uses only `<` and `>` comparisons. Both are false for NaN, so the supposedly bounded value passes through unchanged.
- `src/Combat/Resolution/Common.hs:69-91` claims a final effectiveness band of 0.75–1.10, but its inner and outer clamps both preserve NaN concentration.
- `src/Combat/Resolution.hs:190-228` — a NaN hit probability makes `roll > pHit` false, so the attack continues, while a NaN dodge probability makes `dodgeRoll < pDodge` false, so active dodge does not trigger.
- `scripts/unit_ai_craft.lua:443-450` multiplies craft progress by the returned effectiveness. `src/Engine/Scripting/Lua/API/Craft/Execute.hs:203-229` applies the same effectiveness to every output, and `src/Craft/Execute.hs:48-58` uses the NaN-permissive clamp before persisting `iiQuality`.
- Review-time library evaluation printed `(NaN,NaN,NaN,NaN)` for the four stages above. A separate check confirmed `max 0 NaN` becomes `0`, so `unit.setStat` is not the demonstrated ingress; the unguarded `addXP` path is.
- The complete `--match "Mental effectiveness"` group passed with 28 examples. `test-headless/Test/Headless/Combat/MentalEffectiveness.hs:115-132` covers finite values below, within, and above the boundaries but has no non-finite case.
- Tracker and findings-report searches found closed #353 and no issue owning non-finite XP/stat propagation through mental effectiveness. Issue #319 concerns unrelated debug JSON handling.

**Handoff context:**

- **Current behavior:** An injected NaN XP value for concentration, or a corrupt persisted NaN concentration, yields NaN mental effectiveness. Combat comparisons then bias toward a landed attack with no active dodge, craft progress becomes NaN, and completed item quality can persist as NaN rather than remaining in 0–100.
- **Expected behavior:** Public numerical mutation and persistence boundaries reject or normalize non-finite values, `mentalEffectiveness` always returns a finite value in its stated band, and crafted output quality is always finite and bounded to 0–100.
- **Scope and constraints:** Surfaced while reviewing PR #874 / issue #353. Prefer a shared mutation/load validation boundary if one exists, while preserving every finite formula, missing-stat defaults, debug override semantics, craft-completion atomicity, and combat RNG consumption.
- **Remaining uncertainty:** Current shipped Lua callers add finite XP to ordinary skills, and normal brain updates write concentration through the `max 0` setter. Production reachability therefore depends on the public scripting/debug surface or malformed saved state rather than the ordinary gameplay loop; the broken numerical invariant itself is directly demonstrated.
