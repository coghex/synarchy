# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

Deep per-issue history (review-round narratives, verification stories) was trimmed from this
file on 2026-07-23 — see `docs/history/claude_md_2026-07-23_pretrim.md`, git history, and the
referenced issues/PRs when you need the full story behind a contract stated here.

## Build Commands

- **Build:** `cabal build all` (does NOT build test suites — use `cabal build synarchy-test-headless` explicitly)
- **Run:** `cabal run synarchy`
- **Run tests:** see **Testing Tiers** below — pick the cheapest tier that covers the change; don't run the gates as an iteration loop
- **Pre-push gate:** `make ci` runs the exact checks CI runs (`.github/workflows/ci.yml`): warning-clean (`-Werror`) build of library/exe + both test suites, the headless hspec suite, `test_audit.py`, the Lua/Haskell module-budget guards, the persistence-inventory / EngineEnv-capability / save-compat audits (each with its own self-test), and `world_check.py --quick`. Uses the prod profile and your warm `dist-newstyle`; restores any `cabal.project.local` on exit (`tools/ci-local.sh`). It is NOT an iteration loop and must not be run automatically before opening a PR — only on an explicit user request for full local CI validation.
- **Debug output:** `ENGINE_DEBUG=Vulkan,Graphics,...` environment variable

## Testing Tiers

Worldgen is the entire cost of the test stack (~10 s per w64 generation;
every non-worldgen test is milliseconds). The tiers keep iteration in
seconds and the expensive gates at the end.

1. **Iteration (seconds–1 min).** Targeted hspec:
   `cabal test synarchy-test-headless --test-options='--match "<describe name>"'`.
   Worldgen-output sanity: `python3 tools/world_check.py --quick` (6 seeds, <1 min).
2. **Before reporting done — select only relevant checks.** A targeted
   `--match` describe exercising the changed behavior, plus the focused
   probe for the affected subsystem when one exists. Run
   `world_check.py --quick` only for worldgen-output changes; the
   persistence inventory audit only when its root owners/registry or
   inventory docs change; the EngineEnv capability audit only when
   `EngineEnv`'s field set or `docs/engineenv_capability_inventory.md`
   changes; a module-budget guard only when changing a capped module;
   `test_audit.py` only when changing `world_audit.py`/`world_check.py`.
   Do NOT run the whole headless suite, the 21-seed world check, or
   `make ci` by default — CI is the full-suite authority.
3. **Worldgen-OUTPUT changes only (full tier).**
   `SYNARCHY_FULL_TESTS=1 cabal test synarchy-test-headless` (+~25 s),
   then re-capture baselines `python3 tools/world_baseline.py` (~7 min)
   and re-run world_check. Remember the save-version bump.
4. **Behavior probes — opt-in, not a default gate.** ~55 headless
   `tools/*_probe.py` scripts each boot a real engine and gate one
   system — see `tools/README.md` and the subsystem table below. Run the
   ones relevant to what you touched, or `python3 tools/run_probes.py
   --only <substrings> [--jobs N]` (bare run = full sweep, tens of
   minutes). `python3 tools/ci_probes.py --status` is the authoritative
   list of every probe's CI eligibility (CI-eligible vs manual-only with
   reason) — never trust a prose list of probe names. The path→probe map
   for CI's blocking, path-selective PR probe gate lives in
   `tools/ci_probes.py` (a change there re-runs its `--self-test`);
   promoting a probe to the gate = move its key from
   `MANUAL_ONLY_REASONS` to `CI_ELIGIBLE` after proving it
   deterministic, broad, and cheap.

Baselines (`tools/baselines/`) are **tracked in git**: a fresh
clone/worktree runs world_check directly, and a tier-3 re-capture lands
in the PR diff. Don't edit baseline JSON by hand — regenerate with
`world_baseline.py`. CI runs on every PR/push to master on Linux with
`-Werror` (headless suite always blocking; some steps path-selective on
PRs). Worldgen output is bit-identical across macOS/aarch64 and
Linux/x86_64, so baselines are platform-agnostic; a worldgen-output PR
that skips its tier-3 rebaseline fails CI.

Conventions that keep this fast — don't undo them:
- hspec worldgen specs **share generated worlds** via
  `Test.Headless.Harness.sharedWorld env seed size plates` (one engine,
  booted in `Spec.hs`). A spec that mutates its world must `WorldInit` a
  private page. New read-only specs reuse the canonical `42 64 3` world
  unless they need specific geography.
- `world_check.py` dumps each seed **once**; pass `--runs 3` only when
  chasing a suspected race.
- Don't add per-spec `WorldInit`s of worlds that already exist in the
  suite, and don't grow the baseline seed list without tagging the quick
  tier accordingly.

**Do NOT use `-f dev` for routine work.** Full prod rebuild ~1.5 min
(parallelized via `ghc-options: -j` in `cabal.project` — NOT cabal's
`semaphore:` jobserver, which deadlocks under concurrent worktree
builds, #471), and flag-profile switches force one. The `dev` flag
(Vulkan validation layers, ASan on macOS, `ENGINE_DEBUG` plumbing) is
only for actively chasing graphics/memory bugs — give it its own build
dir so flipping back is free: `cabal build -f dev --builddir=dist-dev`
(every run/test in that profile needs the same pair). Production builds
use `-O2 -optc-O3`.

The executable is built with `-rtsopts` (baked-in default `-N -A128M`) —
append `+RTS -s` etc. at run time without a rebuild. Cost-centre
profiling: `cabal build exe:synarchy --enable-profiling -f profile
--builddir=dist-prof`, run with `+RTS -N1 -p -RTS` — **`-N1` is
mandatory** (the profiled RTS segfaults under the sparked worldgen
parallelism), and drive it via `--headless` + `world.waitForInit`, never
`--dump` (its watchdog can force-kill mid-profile and truncate the
`.prof`). Full recipe: `docs/history/worldgen_timeline_profile_2026-07.md`.

## Language & Conventions

- **Haskell with GHC2024**, cabal 3.16
- **NoImplicitPrelude** is enabled globally — all modules import `UPrelude` instead
- **UnicodeSyntax** is enabled globally — code uses `∷` for type signatures, `→` for arrows, `⇒` for constraints, `∀` for forall

### Unicode operators defined in UPrelude

| Operator | Meaning | Standard equivalent |
|----------|---------|-------------------|
| `⌃` | Bitwise AND | `.&.` |
| `⌄` | Bitwise OR | `.\|.` |
| `⊚` | fmap | `<$>` |
| `⌦` | bind | `>>=` |
| `⌫` | reverse bind | `=<<` |
| `⚟` / `⚞` | const replace | `<$` / `$>` |
| `⊘` | filepath join | `</>` |
| `⊙` | filepath extension | `<.>` |
| `≫=` / `=≪` | monadic bind (from Control.Monad.Unicode) | `>>=` / `=<<` |
| `≡` | equality (from Prelude.Unicode) | `==` |
| `∧` / `∨` | logical and/or (from Prelude.Unicode) | `&&` / `\|\|` |

## Architecture

### Data pattern: Base/Types split
Modules are split into `Base.hs` and `Types.hs` files. Base files have **no local dependencies** (only external packages). Types files import from other project modules freely. This prevents circular imports.

### Core monad: EngineM
`Engine.Core.Monad` defines `EngineM ε σ α` — a continuation-passing-style monad transformer with environment (ε via Reader), mutable state (σ via State), IO, error handling, and logging. Most engine code runs in this monad.

`Engine.Core.State`'s `EngineEnv` is one shared record reachable from
any thread — a state the capability-split epic (#537) is narrowing.
`docs/engineenv_capability_inventory.md` (#876) is the authoritative
capability/thread/lifecycle ownership inventory for every field — read
it before adding a field, changing which thread touches one, or
changing its lifecycle; `tools/engine_env_capability_audit.py` (in CI
and `make ci`) fails if a classification drifts from the live record.

**Capability records (#889, E1 landed):** each capability gets its own
`Engine.Core.Capability.<Name>` module exporting one `<Name>Capability`
record (fields sharing the SAME live `IORef`/queue handles `EngineEnv`
already carries — a projection, never a copy) plus a total
`to<Name>Capability ∷ EngineEnv → <Name>Capability`. One-way only
(never reassembled back into an `EngineEnv`); no capability module
imports its own consumers; don't introduce a record before the
migration issue that actually narrows a real consumer to it. `EngineM`
stays hard-wired to `MonadReader EngineEnv` (no capability typeclass
layer), so a narrowed module's own public API is typically two layers:
primitives taking the capability explicitly, plus thin `MonadReader
EngineEnv` wrappers preserving existing call sites (see
`Engine.Core.Log.Monad`/`Engine.Core.Capability.Core`) — narrowing the
*module's own field access* is the goal, not rewriting every caller.
The same audit also enforces a production-only (`src/`+`app/`, `test/`
exempt) full-access ratchet: importing `Engine.Core.State` with
`EngineEnv(..)` or as a bare import (either shape, regardless of
`qualified`/`as`/multiline) is unrestricted access, allowed only for
SS6.1's hard-coded permanent allowlist or SS6.2's checked-in,
strict/shrink-only temporary ceiling (both mirrored as constants in
`tools/engine_env_capability_audit.py`) — a module gaining unrestricted
access fails the audit even if SS6.2 is also edited to document it;
only growing the checked-in ceiling admits one.

### Threading model
The engine uses multiple worker threads communicating via STM (TVar, queues):
- **Main thread:** Vulkan render loop (`app/Main.hs` → `Engine.Loop`)
- **Input thread:** GLFW input handling (`Engine.Input.Thread`)
- **Lua scripting thread:** Runs Lua scripts (`Engine.Scripting.Lua.Thread`)
- **World thread:** Procedural generation and simulation (`World.Thread`)
- **Unit thread:** Actor/unit management (`Unit.Thread`)

`Engine.Input.Thread` (#787) is a thin lifecycle facade; queue draining
and routing live in `Engine.Input.Thread.Dispatch`, per-domain dispatch
in `.Keyboard`/`.Char`/`.Mouse`/`.Scroll` — each capped at 500 lines by
`tools/haskell_module_budget.py` (CI + `make ci`).

### Graphics pipeline
Vulkan-based renderer with GLFW windowing. Key subsystems:
- **Bindless textures:** `Engine.Graphics.Vulkan.Texture.*` — slot-based texture management
- **Batch rendering:** `Engine.Scene.Batch.*` — sprite and text batching
- **Scene graph:** `Engine.Scene.Graph` / `Engine.Scene.Manager`

### World generation
Procedural world with geological simulation in `World/`:
- `World.Generate` — terrain generation, chunk creation
- `World.Geology` — tectonic plates, erosion, volcanism, timeline evolution
- `World.Hydrology` — rivers, glaciers, lakes
- `World.Fluid` — ocean/river/lake/lava fluid simulation
- `World.Flora` — vegetation placement
- Chunk-based with zoom-level LOD system (`World.Render.Zoom.*`, `World.ZoomMap`)

### Lua scripting
`Engine.Scripting.Lua.*` provides a Lua API for game logic. Lua scripts
live in the repo-root `scripts/` directory; `engine.loadScript` paths
are relative to the repo root. The API modules in
`Engine.Scripting.Lua.API.*` expose engine functionality to Lua.

`scripts/unit_ai.lua` (#538) is the unit-AI entry/orchestration module
only (singleton registration, tunables/action-registry wiring, per-unit
dispatch, engine lifecycle). Every domain's utility/execute bodies live
in `scripts/unit_ai_*.lua` submodules, each capped at 500 lines
(`tools/lua_module_budget.py`, in CI and `make ci`). Shared plumbing is
in `unit_ai_core.lua`; the inventory→ground→mule materials-sourcing
ladder in `unit_ai_fetch.lua`. Submodules reach the shared singleton via
`package.loaded["scripts.unit_ai"]`; public API functions stay attached
to the `unitAi` table from whichever submodule owns them.

### UI system
`UI.*` handles focus management, text input, and UI rendering; layout
and behavior are driven from Lua. Regression suites:
`Test.Headless.UI.*` (InputOwnership, Clipping, PopupPlacement,
InteractiveBounds, ResponsiveMenus, ResponsiveGameplay). Contracts:

**Text coordinates:** `UI.TextBuffer`/`UI.getCursor`/`UI.setCursor` use
zero-based Unicode code-point offsets. Lua strings are UTF-8 byte
arrays — editable widgets must use `scripts/ui/utf8_safe.lua`, never
`#text` or byte-based `string.sub`.

**Layers + modal boundary (#742):** pages live on six `UILayer`s,
painted bottom-to-top `LayerHUD < LayerOverlay < LayerMenu < LayerModal
< LayerTooltip < LayerDebug`; `uiLayerBand` is the single paint-order
source of truth shared by hit-testing and rendering. Whether a page
BLOCKS pointer input is the separate per-page `upInputExclusive` flag
(`UI.InputOwnership`) — `LayerModal` defaults exclusive, everything
else pass-through. The topmost visible exclusive page owns the modal
boundary: input that misses every control on or above it is consumed
(empty modal space blocks). Stacking-only modal pages opt out via
`UI.setPageInputExclusive(page, false)` (e.g. `popup.lua` cards).
`LayerDebug` (shell; F8 overlay, which hit-tests itself via a parallel
`tryClaimClick`) is pass-through above any modal. `UI.isInputBlocked()`
reflects the boundary; `ui_manager.lua`'s `isGameplayInputActive()`
folds it in so gameplay handlers go inert behind a modal; Escape's
dismiss cascade (`init_keys.lua`) deliberately runs before that gate.
Raw handlers that iterate widget instances outside `routePointer` use
`UI.isPageInScope(pageHandle)`.

**Per-element input policies (#743):** three independent policies —
fires a click callback, blocks pointer (`UI.setPointerBlocking`),
captures scroll (`UI.setScrollCapture`); query via
`UI.isPointerBlocking`/`isScrollCapturing`. A click callback still
implies pointer-blocking by default; a blocking element with no
relevant callback consumes the press (`RouteBlocked`), across all three
buttons. Wheel routing (`routeScroll`) picks the topmost in-scope
scroll-capturing surface via the same `topHitBy` paint-order walk —
never the click machinery.

**Scroll dispatch (#744):** plain and Shift wheel go through the
IDENTICAL pipeline (`Engine.Input.Thread.Scroll`): a capturing element
wins first (`LuaUIScrollEvent`, carrying the Shift flag), else a
visible modal boundary consumes, and only past both does Shift select
z-slice vs camera zoom. Don't reintroduce `UI.isInputBlocked()`
self-gates in the Lua handlers — the engine decides once, upstream.

**Control activation + keyboard focus (#745):** a press on a discrete
control records `UI.ControlActivation.PendingActivation` (firing
`LuaUIPressBeginEvent`); the release re-runs `routePointer` and only
activates if it still resolves to the same element. Interruptions
reverted before release are caught by epochs: global `upmPageEpoch`
(bumped ONLY by `hidePage`/`showPage`) + per-element `ueRouteEpoch`
(bumped by `setVisible`/`setClickable`/detach on THAT element, only on
a real value change); `PendingActivation` snapshots the pressed
element's and every ancestor's epoch and cancels on mismatch. Unrelated
sibling/child churn (hover highlights, focus-ring attach) must never
cancel an activation — that constraint shaped this design; don't
"simplify" it back to a global counter. Sliders/scrollbar thumbs opt
out via `UI.setDragActivation` (fire-on-press + drag). Keyboard CONTROL
focus (`upmControlFocus`, `UI.FocusNavigation`) is independent of text
focus: Tab/Shift+Tab traverse in-scope focusables (a modal traps
traversal like pointers; `LayerDebug` stays reachable), Enter/Space
fire the real `LuaUIClickEvent`, arrows step `ueSteppable` controls
(`LuaUIStepEvent`); consumed keys are withheld from `inpKeyStates`.
`UI.getElementInfo`'s `focused` stays text-only; control focus reports
as `controlFocused`.

**Clipping + popup placement (#747):** `UI.setClipChildren(el, true)`
clips DESCENDANTS to the container's live bounds (overflow:hidden;
nested clips intersect; recomputed fresh, nothing cached).
`UI.Clipping.effectiveClip` is the ONE helper both rendering
(`clipQuadUV` — partial quads, not all-or-nothing culling) and
hit-testing (`UI.Manager.Query.isPointInElement`) consult, so paint and
hit-test can't drift. Floating root-mounted content (dropdown lists,
context menus) is unaffected — clipping walks real ancestors only.
`UI.placePopup(anchorX, anchorY, anchorW, anchorH, contentW, contentH,
direction)` (`"below"/"above"/"right"/"left"/"anchored"`) is the one
placement algorithm for floating content (pass the FULL interactive
size incl. scrollbar); `UI.fitVisibleRows` backs oversized-list row
reduction. Tooltips keep their own separate cursor-relative clamp.

**Interactive bounds (#749):** three rects per element — LOGICAL
(`uePosition`+`ueSize`), VISUAL (overflow-expanded render rect), and
INTERACTIVE (what all hit-testing uses,
`UI.InteractiveBounds.interactiveRect`). A box opts its visible border
into interaction via `UI.setInteractiveOverflow`; overflow alone never
enlarges a target. Overflow is clamped: non-finite → 0, astronomically
large → capped, inverting → zero-extent, non-hittable AND
non-rendering. `UI.getElementInfo` adds `interactiveOverflow` +
`interactiveBounds` (`x/y/width/height` stay content bounds).

**Responsive lifecycle (#748 menus / #750 gameplay):**
`scripts/ui/responsive.lua` owns the supported envelope — bands
(inclusive): framebuffer height 600-900 @ 0.5-1x UI scale, 901-1200 @
0.75-2x, 1201-1600 @ 1-3x, 1601-2160 @ 1.5-4x; formal minimum 800x600.
`responsive.classify` is introspection only — out-of-envelope
combinations degrade best-effort (never crash, never invalid geometry,
fixed actions stay reachable), typically via `math.max(20, ...)` floors
and `math.min(panelW, fbW)` caps. Menu screens register via
`responsive.register(name, mod)` + `responsive.notifyResize(w, h)`
(0x0-minimize-guarded; re-notify with the SAME size = scale-only
change). Gameplay surfaces stay OFF that registry: they're reached
either through `ui_manager_boot.lua`'s manual forward or the engine's
automatic `broadcastToModules` resize — registering a broadcast-reached
module DOUBLE-FIRES it every resize. Scale-only changes reach gameplay
via `uiManager.notifyGameplayRescale` (called from Settings
Apply/Save/Back/Defaults when the scale actually changed).

Rules that keep resizes correct — follow them for any new screen/panel:
- A geometry rebuild must preserve state a semantic re-entry may reset:
  pending settings edits, scroll offsets, in-progress text
  (textbox/randbox/dropdown raw filter text all have
  `snapshotPage`/`restoreAll`), selected tabs, open-panel targets.
  `hud.createUI()` snapshots each world-page panel's "open for" state
  before the `view_teardown.lua` `"resize"` sweep and reopens via each
  panel's real entry point (`reopenWithTab`/`reopenWithState`); restores
  must not re-fire `onChange`/`onSelect` (use the widgets' `silent`
  params, `toggle.restoreSlotIdentity`, `list.setSelectedIndex` — never
  `selectItem`).
- Keyboard control focus survives rebuilds by NAME:
  `responsive.snapshotControlFocusName()`/`restoreControlFocusName()`
  around any destroy+recreate; restore only after pages are re-shown.
- Fixed-size widgets fit via a LOCAL effective uiscale
  (`responsive.fitScale` against the reserved column/row/panel width) —
  the recurring pattern for dropdowns, tab bars, buttons, and labels
  (row labels reserve a `LABEL_COLUMN_FRACTION` 0.35 column). Shrink a
  box's font together with its box, never separately.
- Panels sized as `BASE * uiscale` must cap width/height to the
  framebuffer, and their content must derive from the panel's REAL
  bounds (`panel.getContentBounds()`), never an independently
  recomputed value that can drift. `scripts/ui/reserved_regions.lua`
  (pure) keeps popups clear of toolbar clusters
  (`hud.getToolbarRects()`, `avoidReserved`, `maxAvailableWidth`,
  `maxRightAnchoredWidth`, `findEscapes`).
- zIndex ACCUMULATES through the parent chain (`elementPaintKey` sums
  up `ueParent`) — leave wrapper/viewport elements at zIndex 0.
- Resize ordering: hud rebuilds first; dependent surfaces (`popup`,
  `unit_info_v2`) expose a separate `reflow()` called after it so they
  never read stale hud geometry.

Headless UI tests use a bare Lua backend + synthetic texture/font
handles (`engine.getTextWidth` returns 0 there — stub it when a test
needs real measurement); the shared fixture wipes `package.loaded`
between cases. The full `ui_manager` boot never runs headless (it gates
on `fontsReady`, which needs a GPU font atlas) — use `--offscreen` for
end-to-end UI verification.

## Project Layout

- `src/` — Library source (360+ modules)
- `app/Main.hs` — Executable entry point (draw loop)
- `test/` — hspec unit tests (engine core and Vulkan primitives)
- `cbits/` — C code (stb_truetype font rasterization, Lua debug FFI)
- `config/` — YAML config: tracked `*_default.yaml` templates +
  gitignored `*.local.yaml` runtime state (see "Config state" below)
- `data/` — Game data YAML (materials, vegetation, flora, units)
- `assets/` — Images and graphical resources
- `scripts/` — Lua scripts for game logic

## Resource Root

Every runtime resource family (`scripts/`, `assets/`, `data/`,
`config/`) is loaded by cwd-relative paths. The executable resolves ONE
resource root at startup (`App.ResourceRoot`, #636) and chdirs into it.
Precedence: `--resource-root <path>` flag > `SYNARCHY_ROOT` env var >
cwd. Running from the repo root needs nothing; launching the built
binary elsewhere needs one of:

```bash
$(cabal list-bin exe:synarchy) --headless --port 9008 --resource-root ~/work/synarchy
SYNARCHY_ROOT=~/work/synarchy $(cabal list-bin exe:synarchy) --dump
```

The root is validated before dispatch (missing root/family exits 1 with
a clear error). The chdir means relative OUTPUT paths (`saves/`, config
saves) also land under the resource root. Gate:
`tools/resource_root_probe.py` (manual-only).

## Headless Mode & Debug Console

Headless mode: no GPU, no window, no focus stealing — for automated
testing, scripted worldgen, and agent workflows.

### Tips for agents (read first)

- **NEVER launch `cabal run synarchy` / `cabal run exe:synarchy` without `--dump`, `--headless`, or `--offscreen`** — otherwise it opens a graphical window that steals the user's focus (`--offscreen` uses the GPU but creates no window, so it is safe). **`--preview` (below) is NOT in this safe list** — it always opens a real window (no offscreen variant exists), so never launch it yourself even transiently; a bad target rejects before boot, but a valid one steals focus like the graphical path
- **Prefer `--dump` for testing** — self-contained, no TCP, JSON to stdout, implies headless
- If you must use `--headless`, use `--port 9008` (or another non-8008 port) — 8008 may be the user's graphical instance
- **NEVER use `pkill -f synarchy`** — it kills the user's GUI. Shut down your own instance with `echo 'engine.quit()' | nc -w 2 localhost 9008`, or track your PID (`HPID=$!`) and `kill $HPID`. If a port is busy with a stale instance: `lsof -ti:9008 | xargs kill`
- **worldSize 256** generates in ~2 minutes; 512 takes much longer
- **Prefer `loadChunksInRegion` + `waitForChunks` over camera movement** for bulk tile loading
- `world.show("name")` must be called before tile/chunk queries work
- The debug console is **single-line only** — use semicolons: `local r=world.getRivers(); return #r`
- Table return values auto-serialize to JSON

### Starting headless

```bash
cabal run exe:synarchy -- --headless --port 9008 > /tmp/engine.log 2>&1 &
# Wait for the debug server (prints "READY port=NNNN" to stdout)
until grep -q "READY" /tmp/engine.log 2>/dev/null; do sleep 0.2; done
```

### Offscreen render mode (#650)

`--offscreen` is the third boot mode: **GPU on, window off** — the full
Vulkan pipeline into offscreen images, no GLFW window/swapchain. Unlike
`--headless`, the REAL UI flow runs (loading screen → menus → HUD),
`debug.captureScreenshot` works, and `input.*` injection (#644) drives
the UI; multiple instances run concurrently on distinct ports.

```bash
cabal run exe:synarchy -- --offscreen --port 9018 --size 1280x720 > /tmp/off.log 2>&1 &
until grep -q "READY" /tmp/off.log 2>/dev/null; do sleep 0.2; done
echo "return debug.captureScreenshot('/tmp/shot.png')" | nc -w 10 localhost 9018
echo "return input.click(640, 260)" | nc -w 5 localhost 9018
echo 'engine.quit()' | nc -w 2 localhost 9018
```

Frames pace on a fixed ~60 fps sleep; window-requiring video settings
no-op with a warning. Gate: `tools/offscreen_probe.py` (manual-only,
`needs-gpu`) — locate click targets via the `ui.dumpWidgets` oracle,
never hardcoded coordinates.

### Preview mode: asset browser (#632 Phase 1, #886 Phase 2, epic #427)

`--preview <category>[/<item>]` is a fourth, structurally distinct boot
mode (`App.Preview`, `BootPreview`): a real GLFW window + Vulkan, but no
world/unit/sim/combat thread, booting straight to
`scripts/preview_manager.lua` instead of the normal ~25-script menu/HUD
set — for eyeballing a texture without booting a game session. **It
always opens a real window** (see the warning above) — there is no
offscreen/headless variant, so treat it exactly like the graphical path.

Canonical category contract (`App.Cli.classifyPreviewCategory`) — the
unknown-category error message lists exactly this set, no compatibility
aliases:

- **Simple** (a flat, recursively-browsable asset folder — bare
  `--preview icons` lists every texture under `assets/textures/icons/`):
  `icons`, `items`, `ui`, `world`.
- **Grouped** (one named entry per item — a bare grouped category prints
  "select a specific ..." and exits without booting; you must give
  `--preview <category>/<item>`): `units`, `flora`, `buildings`,
  `structures`.
- `equipment`, `hud`, `facemap`, `utility`, `vegetation` are NOT exposed
  (no top-level `assets/textures/` directory of that name, or — for
  `hud`, which lives under `ui/hud` — folded into `ui`'s recursive
  simple-category listing instead).

Simple-category behavior (`Engine.Preview.Discovery`, pre-boot; the
in-engine browser is `scripts/ui/asset_browser.lua` + `scripts/ui/list.lua`):

- **Bare category** (`--preview icons`): a scrollable left-hand list of
  every texture found recursively under the category root, labeled by
  its category-relative path with `/` separators and the file extension
  INCLUDED (e.g. `skill/climbing.png`) — sorted lexicographically. The
  first entry auto-selects; its texture renders in the main panel,
  nearest-neighbour scaled (`previewManager.init` forces
  `engine.setTextureFilter("nearest")` live-session-only — never assumed
  from the default video config, which a user's persisted
  `config/video.local.yaml` can override to `"linear"`), fit to the panel
  with aspect ratio preserved. Click a row to select it; wheel-scroll the
  list. A resize (the preview window is resizable) reflows the panel/list
  bounds while preserving the current selection and scroll offset
  (`previewManager.onFramebufferResize`).
  A label displayed here is ALWAYS a valid item target for the form below
  — discovery and item resolution apply the identical extension rule, so
  they can never disagree.
- **Focused item** (`--preview icons/skill/climbing.png`): shows only
  that one texture, no list. A nonexistent item, a directory, an absolute
  path, or a path containing `..` (including a symlink escape) all reject
  **before ever creating a window** — same pre-boot exit code convention
  as the unknown-category/missing-target errors below.
- A grouped category's item form (`--preview units/acolyte`) still only
  gets the Phase 1 placeholder-label boot (its own real browsing is
  #887/#888) — this canonical contract only makes grouped *classification*
  final, not its browsing implementation.
- Trimmed loading: preview mode loads only its font, the list widget's
  own chrome textures (`assets/textures/ui/{highlight,scroll*}.png`,
  loaded once, list-mode only), and textures within the requested
  category — never `data/*.yaml` gameplay catalogs, unrelated world/HUD
  texture sets, or the normal script set.
- Debug-console introspection: `require("scripts.preview_manager").dump()`
  (self-registered into `package.loaded` the same way `unit_ai.lua`/
  `debug.lua` are, despite being `engine.loadScript`-loaded, not
  `require`d) reports `mode` (`"list"`/`"item"`/`"placeholder"`), `state`
  (`"loading"`/`"ready"`/`"empty"`), the current `selected` entry, and in
  list mode `entryCount`, `scrollOffset`, and per-visible-row interactive
  bounds/handles (`rows`, `scripts/ui/list.lua`'s existing F3 dump
  contract) — enough to drive real `input.click`/`input.scroll` against a
  located row without ever hardcoding a screen coordinate.

Gates: `tools/preview_cli_probe.py` (CI-eligible, no boot at all — every
check above the "always opens a real window" line) and
`tools/preview_probe.py` (manual-only, `needs-gpu` — the real-boot
browser checks: discovery/selection/scroll/resize via the dump, forced
nearest filtering, and trimmed loading verified against
`engine.getLoadedTexturePaths()` — `Engine.Asset`'s `apAssetPaths`,
populated by `engine.loadTexture`'s own Haskell handler regardless of
Lua caller, so this is the engine's own authoritative loaded-texture
record, not previewManager's self-reported bookkeeping — plus the
grouped-category placeholder boot). Focused hspec coverage for
the pure discovery/labeling/ordering/containment logic:
`cabal test synarchy-test-headless --test-options='--match "Preview.Discovery"'`.

### Dump mode (no TCP, JSON to stdout)

```bash
cabal run exe:synarchy -- --dump > world.json 2>/dev/null
cabal run exe:synarchy -- --dump=terrain,ice --seed 42 --worldSize 32 --region -2,-2,2,2 > ice.json 2>/dev/null
# --plates is the canonical tectonic-plate-count flag (--ages is a legacy alias)
cabal run exe:synarchy -- --dump --seed 1337 --worldSize 256 --plates 5 --region -5,-5,5,5 > world.json 2>gen.log
```

**Layers:** `terrain` (or `elevation`), `material`, `fluid`, `ice`,
`ore` (the default five). `slope` is **opt-in only** so a bare `--dump`
stays byte-identical to historical output (baselines/audits drive it).
Region coordinates are **chunk coords**. Per-tile fields:

| Field | Layer | Description |
|-------|-------|-------------|
| `x`, `y`, `v` | always | Global tile coords and v-axis (gx+gy) |
| `terrainZ`, `surfaceZ` | terrain | Raw terrain and max(terrain, fluid) |
| `matId` | material | Top surface material ID |
| `fluidType`, `fluidSurf` | fluid | "ocean"/"lake"/"river"/"lava" or null |
| `iceSurf`, `iceMode` | ice | Ice surface Z and "basin"/"drape" or null |
| `oreId`, `oreTopZ`, `oreCount` | ore | Topmost ore band in the column (null/0 if none) |
| `slope`, `hardness` | slope | Slope bitmask (bit0=N,1=E,2=S,3=W; 0=flat) + surface hardness |
| `glacierZone`, `beyondGlacier` | always | World boundary flags |

`python3 tools/ore_report.py` for cross-seed ore statistics.

### Debug console (TCP)

Single-line Lua via netcat; return values auto-serialize (tables → JSON).

```bash
echo 'return world.getInitProgress()' | nc -w 2 localhost 9008
```

### World generation workflow

```bash
# world.init(pageId, seed, worldSize, plateCount[, displayName[, gloss]])
# The optional identity (#707) is display text, immutable per page,
# persisted in saves, independent of pageId and save-slot name;
# world.getIdentity(pageId) reads it; engine.listSaves() exposes
# worldName/worldGloss.
echo 'world.init("test", 42, 256, 5)' | nc -w 2 localhost 9008
# Block until done (preferred; timeout in seconds)…
echo 'return world.waitForInit(300)' | nc -w 300 localhost 9008
# …or poll: phase 0=idle,1=setup,2=chunks,3=done
echo 'return world.getInitProgress()' | nc -w 2 localhost 9008
# Activate for queries (required before chunk/tile queries)
echo 'world.show("test")' | nc -w 2 localhost 9008
```

### Query API (returns JSON)

```bash
echo 'return world.getRivers()' | nc -w 5 localhost 9008          # rivers with segments
echo 'return world.getChunkInfo(cx, cy)' | nc -w 2 localhost 9008
echo 'return world.getTerrainAt(gx, gy)' | nc -w 2 localhost 9008 # surfaceZ, terrainSurfaceZ
echo 'return world.getSlopeAt(gx, gy)' | nc -w 2 localhost 9008   # slope bitmask
echo 'return world.getVegAt(gx, gy)' | nc -w 2 localhost 9008     # vegetation id
echo 'return world.isPlantable(gx, gy)' | nc -w 2 localhost 9008  # tilled-soil contract (#333)
echo 'return world.getFluidAt(gx, gy)' | nc -w 2 localhost 9008
echo 'return world.getSurfaceAt(gx, gy)' | nc -w 2 localhost 9008
echo 'return world.getAreaFluid(gx, gy, radius)' | nc -w 5 localhost 9008  # max radius 64
echo 'return world.loadChunksInRegion(cx1, cy1, cx2, cy2)' | nc -w 5 localhost 9008
echo 'return world.waitForChunks(120)' | nc -w 120 localhost 9008
echo 'return camera.getPosition()' | nc -w 2 localhost 9008
echo 'camera.goToTile(gx, gy)' | nc -w 2 localhost 9008
echo 'engine.quit()' | nc -w 2 localhost 9008                     # shutdown
```

### Subsystem probes & domain contracts

Each area below has a turnkey `tools/*_probe.py` gate (real headless
engine, pass/fail checks). `tools/README.md` lists all ~55;
`ci_probes.py --status` gives CI eligibility. Durable contracts to know
before touching each area:

- **Unit/combat animations** — no pixels headless, but
  `unit.getInfo(uid)` returns `currentAnim`/`animStart` (unit thread
  runs headless); poll over time to verify animation timelines. Gate:
  `combat_anim_probe.py`. Drive by hand: load
  `scripts/unit_stats.lua` + `unit_resources` + `unit_ai`, then
  `require('scripts.unit_ai').commandAttack(atk,tgt)`.
- **Movement** — `scripts/movement_arena.lua` builds obstacle courses
  on a flat `world.initArena` world via the tile-edit API
  (`world.addTile`/`deleteTile`/`setFluidTile`/`setSlope` — `setSlope`
  is the ONLY way to make a step walkable). Gate: `movement_probe.py`
  (neutralises the unit_ai wander tick so `moveTo` is the only
  steering). `startFall` clears the move target on landing — fall
  checks assert the fall + landing z, not arrival.
- **Construction (#95/#96)** — `construction.*` designations +
  construct_job AI (claim → source materials → progress → place →
  stake); build costs in `data/structure_packs/*.yaml` `build:` blocks.
  Gate: `construction_probe.py` (stake phase runs LAST).
- **Roles (#265)** — DERIVED labels, never assigned: highest work skill
  ≥ 30 (+5 switch hysteresis). Roles multiply work-action ENTRY
  utilities only (on-role ×1.4, off-role ×0.7) — never the 6.0
  in-progress locks, never survival/combat/orders. `unitAi.getRole`.
  Gate: `role_probe.py`.
- **Crafting (#325/#326/#329/#343/#795)** — recipes in
  `data/recipes/*.yaml` (station tag, inputs, optional
  fuel/knowledge/skill, work, outputs, optional `power_draw`).
  `craft.execute(uid, recipeId)` is station-blind (tests/console);
  `craft.executeAt(uid, recipeId, bid[, billId])` needs a Built station
  offering the operation with the unit adjacent (Chebyshev ≤ 1).
  Bills (`Craft.Bills`, per-page, engine-side atomic claims, persisted)
  have three modes: fixed count, repeat-forever, until-stock
  (`craft.addBill(bid, recipeId[, count[, untilTarget]])`; until-stock
  re-checks LIVE ground stock via `unit_ai_fetch.untilStockSatisfied` —
  the same formula the crafting panel uses, so they can't disagree).
  Skill-tagged recipes derive output quality from the crafter, then
  shift by live mental effectiveness (±10) — tests asserting quality
  must pin the neutral-effectiveness precondition (#878). Gates:
  `craft_probe.py`, `craft_bill_probe.py`.
- **Power (#358-#361, #590/#591)** — solar/battery nodes are
  item-consuming placements (`power.placeNode` via
  `buildTool.commitPlacement`); networks (wire 4-adjacency +
  nodes/consumers) are recomputed fresh every tick — only battery
  `storedWh` persists. Solar follows the sun angle and
  `world.setTimeScale`. Electrical load lives on the RECIPE
  (`power_draw`), not the building (`power_drain` exists only for
  hypothetical always-on devices; no shipped building sets it): a bill
  draws only while claimed AND `cbWorking` (set at the walking→working
  transition, cleared on exit/release/complete).
  `power.isStationPoweredForRecipe(bid, recipeId[, billId])` is the
  gating query — pass the bill's own id so its already-registered draw
  isn't double-counted, while other consumers still sum. Gates:
  `power_probe.py`, `power_workshop_probe.py`, `machine_shop_probe.py`;
  pure algorithm in `Test.Headless.Power.Network`.
- **Farming (#331-#336, growth #332, tilling #333)** — flora growth is
  DERIVED state from the advancing calendar (nothing per-instance in
  saves; `world.getDate`/`setDate`, `world.getFloraGrowthAt`). Fruiting
  windows gate bare food-harvest calls only; tagged calls (chop's
  `"wood"`) skip the window, and chop-claim keys on
  `regrowthRemaining`+`tags`, not `harvestable`. Tilling: `till.*`
  mirrors `chop.*`; completion writes `world.setVegAt` (edit-log —
  survives eviction/saves); consumers must use `world.isPlantable`,
  never compare `getVegAt` to raw id 77. Gates:
  `flora_growth_probe.py` (registers a max-tolerance `probe_berry`
  species), `till_probe.py`.
- **Location discovery (#780)** — persisted per-page one-way flag,
  flipped when a `uiFactionId == "player"` unit enters the def's
  `discovery_margin` halo; ticks for EVERY loaded page, independent of
  pause; emits exactly one `location_discovery` event (hidden-page
  discoveries omit clickable coords). `world.listPlacedLocations()`.
  Independent of the stamped/contents-spawned flags. Gates:
  `location_content_probe.py`; hspec `--match "Location discovery"`.
- **Logging streams** — event log: `engine.getEventLog()`, emit via
  `engine.emitEvent(cat,text)` / `emitEventAt` /
  `emitEventForUnit(cat,text,uid[,gx,gy])`; a category lands only if
  its notifications YAML has `log: true`. Combat:
  `combat.drainEvents()`. Injury (NON-combat only — falls, hazards,
  wound deaths): `injury.drainEvents()`. These are DRAINED streams —
  don't drain manually in a test while the panel script is loaded, or
  you'll race it. Gate: `injury_log_probe.py`.
- **Config state (#638/#786)** — settings save to gitignored
  `config/*.local.yaml`; boot falls back to tracked `*_default.yaml`
  (notifications self-materializes from
  `data/notification_categories.yaml`). The tracked legacy
  `video.yaml`/`keybinds.yaml`/`notifications.yaml` exist ONLY as a
  one-time migration source: `Engine.Core.Init.migrateLegacyConfig`
  copies a legacy file to the local path iff the local file is absent
  AND the legacy file decodes against the real target schema; failures
  fall back to defaults and never touch a valid local file. Gates:
  `config_state_probe.py`, `config_migration_probe.py`; hspec
  `--match "config"`.

## Save / Load

**Persistence contract:** [`docs/persistence_contract.md`](docs/persistence_contract.md)
is the authoritative contract for what a save represents and how every
piece of engine/Lua state is classified;
[`docs/persistence_state_inventory.md`](docs/persistence_state_inventory.md)
is the field-by-field classification, enforced by
`tools/persistence_inventory_audit.py` (in `make ci`/CI — fails when a
new root state owner, Lua save module, component, or typed reference
kind lacks a classification/coverage row). Read the contract before
adding state to `EngineEnv`, `WorldState`, `World.Save.Types`, or
`scripts/lib/save_modules.lua`'s registry.

```bash
echo 'engine.saveWorld("test", "my_save"); return "saved"' | nc -w 2 localhost 9008
echo 'engine.loadSave("my_save"); return "queued"' | nc -w 2 localhost 9008
# loadSave only ACCEPTS synchronously (#763) — poll engine.getLoadStatus()
# for phase == "LoadPublished" (or "LoadFailed") before touching anything.
# Loaded pages keep their saved ids (no main_world remap) —
# world.getActiveWorldId() to find the active one.
echo 'return engine.getLoadStatus()' | nc -w 2 localhost 9008
# Loads come up paused: engine.setPaused(false) (in-game: scripts.pause,
# which also restores the time scale).
```

Budget ~15 s after a 128-world load before querying tiles — chunks
queue progressively after `LoadPublished`.

**What's preserved:** gen-params + camera + time + climate + river
flow, edited tiles (chunks regen + edits replay), buildings (with
spawn-roster countdown), units (stats/modifiers/skills/inventory/sim
state), Lua AI memory, pause state. **Not preserved by design (load
policy):** selection, build-tool placement mode, active toolbar tool
(always default tool post-load; HUD resets via the `onSaveLoaded`
broadcast), and time scale (always 1). Older schema versions are
rejected with "expected vN, got vM".

**Enum schema policy:** `Direction`, `Pose`, `UnitActivity` (and any
enum serialized via `Generic Serialize`) are positional by constructor
tag — **append-only**. Inserting/reordering silently corrupts saves;
anything beyond appending requires a `currentSaveVersion` bump.

**Architecture (persistence-overhaul epic #756-#768, landed):**
- `World.Save.Snapshot.SessionSnapshot` is the immutable, validated
  in-memory capture (pure `captureSessionSnapshot`) — NOT the wire
  format. The save barrier (`Engine.Save.Barrier`) quiesces every
  state-owner thread, releases its capture lock only after the encode
  is forced (`evaluate`), and reports the outcome only after the disk
  write resolves.
- On-disk `world.synworld` is a tagged, checksummed component ENVELOPE
  (`World.Save.Envelope`): FNV-1a-checksummed manifest + independently
  versioned components (`core-session`, `world-pages`, `world-edits`,
  `world-activity`, `buildings`, `units`, `unit-sim`, `craft-bills`,
  `power-nodes`, `texture-palette`, `metadata`, plus dynamic
  `lua.<module>` components). Registry:
  `World.Save.Component.saveComponentRegistry`. Component evolution =
  per-component schema version bumps + explicit migrations from frozen
  vN DTOs — NOT a global save-version bump. `currentSaveVersion`
  (`src/World/Save/Types.hs`) now versions only the transitional
  in-memory load bridge (`SaveData`) and is bumped freely — don't trust
  any number written in docs. `listSaves` decodes only the `metadata`
  component (never gameplay payloads). Pre-envelope flat saves are a
  clean break (rejected), and `world_gen.yaml` no longer exists.
- Lua-owned state persists via `scripts/lib/save_modules.lua`
  (`saveModules.register(id, spec)` — versioned
  snapshot/decode/validate/apply, dependency-ordered, `required` vs
  optional-with-`default`; `registerResetHook` for non-durable modules)
  with canonical data-only payloads from `scripts/lib/data_codec.lua`
  (decoding never executes code). A required component's failure aborts
  the whole save/load.
- Disk I/O goes ONLY through `World.Save.Storage.publishGeneration` — a
  write-fsync-revalidate-rotate transaction keeping a
  `world.synworld.prev` recovery generation. A corrupt authoritative
  file falls back to `.prev` (loudly logged; `recovered` flag in
  `engine.listSaves()`); an INCOMPATIBLE one reports directly with no
  fallback. Symlinked slot dirs/files are refused. Failures name their
  `StoragePhase` through `engine.getSaveStatus()`.
- `engine.loadSave` is a whole-session TRANSACTION
  (`World.Load.Stage`/`Publish`): stage the entire replacement session
  against fresh values, swap in one quiesced window. A load REPLACES
  the complete session — live pages not in the save do not survive.
  Save and load mutually exclude for their whole duration; a failed
  load leaves the old session unchanged and paused (pause is a one-way
  ratchet per attempt). `engine.getLoadStatus()` exposes the 12-phase
  lifecycle.
- Typed persistent references (`World.Save.Reference`:
  `SamePageRef`/`CrossPageRef` newtypes; Lua `{__ref=kind, id=N}`
  wrapping in `unit_ai_save_refs.lua`/`building_spawn.lua`) feed the
  shared integrity graph (`World.Save.Integrity`), run at both save and
  load boundaries — wrong-PAGE targets are hard errors; DANGLING
  targets are tolerated, non-blocking diagnostics (a demolished
  station's lingering bill is gameplay, not corruption). NB: ground-item
  ids are ZERO-based; every other allocator starts at 1.

**Key gates:** pure hspec — `--match "persistence contract"` (full
representative session through the real codec, every field via derived
`Eq`), `--match "persistence reference integrity"`, `--match "Lua
persistence components"`, `--match "save envelope"` / `"save
components"` / `"atomic save storage"`. Probes —
`persistence_contract_probe.py` (CI-eligible smoke: three real
fresh-process save→load→save cycles compared via
`tools/persistence_snapshot.compare_session_files`),
`persistence_contract_sweep.py` (manual full sweep; runs the 12
cross-referenced persistence probes on isolated resource roots),
`save_barrier_probe.py`, `save_storage_probe.py`,
`transactional_load_probe.py`, `persistence_integrity_probe.py`,
`multiworld_save_probe.py`. NB #365: a save containing an arena page
hangs the world thread on load — never use arenas as a save-test page.

## AI Asset Generation

Textures (flora, units, buildings, tiles) can be generated via the PixelLab MCP server.
**Read `docs/asset_generation.md` before generating** — it has the validated pipelines
(skeleton-freeze masks for multi-stage flora, character/state/animation flow for units),
the raw v2 API parameters the MCP tools hide, and the gotchas that waste hours if rediscovered
(soft freezes, broken `color_image`, base64-in-shell corruption, real ETAs).

## Platform Notes

- Tested primarily on macOS; works on Linux with minor adjustments
- macOS: GLFW produces unavoidable junk on stdout
- macOS builds get `-DDARWIN` cpp flag and address sanitizer in dev mode
