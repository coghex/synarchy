# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

Deep per-issue history (review-round narratives, verification stories) was trimmed from this
file on 2026-07-23 — see `docs/history/claude_md_2026-07-23_pretrim.md`, git history, and the
referenced issues/PRs when you need the full story behind a contract stated here.

## Build Commands

- **Build:** `cabal build all` (does NOT build test suites — use `cabal build synarchy-test-headless` explicitly)
- **Run:** `cabal run synarchy`
- **Run tests:** see **Testing Tiers** below — pick the cheapest tier that covers the change; don't run the gates as an iteration loop
- **Pre-push gate:** `make ci` runs the exact checks CI runs (`.github/workflows/ci.yml`): warning-clean (`-Werror`) build of library/exe + both test suites, the headless hspec suite, `test_audit.py`, the Lua/Haskell module-budget guards, the Unicode-operator audit, the persistence-inventory / EngineEnv-capability / save-compat / enum-append-only / cabal-library-module-inventory / material-id / findings-report-status audits (each with its own self-test), the unit-asset inventory gate (`test_pack_atlas.py` + `pack_atlas.py --validate-only --strict`), and `world_check.py --quick`. Uses the prod profile and your warm `dist-newstyle`. `-Werror` is checked into `synarchy.cabal`'s warning policy (not injected by this gate), so `tools/ci-local.sh` only scopes a temporary `-fforce-recomp` via `cabal.project.local`, restored on exit. It is NOT an iteration loop and must not be run automatically before opening a PR — only on an explicit user request for full local CI validation.
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
   `test_audit.py` only when changing `world_audit.py`/`world_check.py`;
   `findings_report_audit.py` only when editing a findings report;
   the unit-asset inventory gate (`test_pack_atlas.py` +
   `pack_atlas.py --validate-only --strict`, ~1 s) when touching
   `assets/textures/units/`, `data/units/`, or the unit-YAML /
   preview / registration decoders.
   Do NOT run the whole headless suite, the 21-seed world check, or
   `make ci` by default — CI is the full-suite authority.

**Module-budget scope:** the 500-line Haskell/Lua limits are per-split
ratchets, enforced only for module families explicitly listed in the relevant
budget tool. They are not a tree-wide size policy. For a structural split with
no explicit budget entry, extract the cohesive, correctness-relevant boundary
first even if the facade remains above 500 lines; record a later pass rather
than forcing unrelated `EngineEnv`/capability refactoring just to hit 500.
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
`world_baseline.py`. CI runs on every PR/push to master on Linux, where
`synarchy.cabal`'s checked-in `-Werror` policy applies the same as any
build (headless suite always blocking; some steps path-selective on
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
| `≢` | inequality (from Prelude.Unicode) | `/=` |
| `∧` / `∨` | logical and/or (from Prelude.Unicode) | `&&` / `\|\|` |

Five of these are **enforced**: `.&.`, `.\|.`, `>>=`, `==`, and `/=` must
not appear as Haskell operators in `src/`/`app/` outside
`tools/unicode_operator_audit.py`'s short, explicit exemption list
(`src/UPrelude.hs`'s own definitions; `ShaderCode.hs`'s quasiquoted
GLSL; the `Eq`/`Monad` instance method names, which must stay ASCII) —
see issue #1005 / `docs/code_health_findings.md` CH-49. `fmap`'s two
spellings, `<$>` and `⊚`, are a deliberate exception: **both are kept**,
picked per call site by readability, not enforced either way.

## Architecture

### Data pattern: Base/Types split
Modules are split into `Base.hs` and `Types.hs` files. Base files have **no local dependencies** (only external packages). Types files import from other project modules freely. This prevents circular imports.

### Core monad: EngineM
`Engine.Core.Monad` defines `EngineM σ α` — a continuation-passing-style monad with a concrete `EngineEnv` Reader environment, concrete `EngineState` mutable state, IO, error handling, and logging. Its two type parameters are `σ` (the continuation result) and `α` (the value); neither the environment nor the state is a parameter. Most engine code runs in this monad.

`Engine.Core.State`'s `EngineEnv` is one shared record (83 fields)
reachable from any thread. The capability-split epic (#537) that
narrowed it is **complete** (#889–#899).
`docs/engineenv_capability_inventory.md` (#876) is the authoritative
capability/thread/lifecycle ownership inventory for every field — read
it before adding a field, changing which thread touches one, or
changing its lifecycle; `tools/engine_env_capability_audit.py` (in CI
and `make ci`) fails if a classification drifts from the live record.
**Before adding any state, read its §6.4 post-flip procedure** — it
leads with the case that resolves most of them: the state doesn't
belong on `EngineEnv` at all (`WorldState`, a manager, `EngineState`,
or a local), and needs no new field.

**Capability records (#889–#899, epic complete):** each capability gets
its own `Engine.Core.Capability.<Name>` module exporting one
`<Name>Capability` record plus a total `to<Name>Capability ∷ EngineEnv
→ <Name>Capability` projection.
`docs/engineenv_capability_inventory.md` §2.1's canonical convention
block is the one authoritative statement of the naming/placement,
one-way-projection, shared-live-container, no-back-import,
no-record-ahead-of-need, and thread-private-split rules — read it
before adding a capability record rather than inferring the shape from
an existing one. `EngineM` stays hard-wired to `MonadReader EngineEnv`
(no capability typeclass layer), so a narrowed module's own public API
is typically two layers: primitives taking the capability explicitly,
plus thin `MonadReader EngineEnv` wrappers preserving existing call
sites (see `Engine.Core.Log.Monad`/`Engine.Core.Capability.Core`) —
narrowing the *module's own field access* is the goal, not rewriting
every caller. There are **eight capability identifiers and thirteen
record/view types** (§2.1's table): five capabilities are split, four
of them by the thread-private-split rule above (§3.1).

The same audit enforces a production-only (`src/`+`app/`, `test/`
exempt) full-access boundary: importing `Engine.Core.State` with
`EngineEnv(..)` or as a bare import (either shape, regardless of
`qualified`/`as`/multiline) is unrestricted access. Since #899 (E8)
that is allowed **only** for §6.1's hard-coded permanent allowlist —
the 24 genuine whole-session orchestration boundaries (the definer and
constructor, the monad carrier, per-profile boot wire-up, the main
loop, Lua dispatch, and the save/load transaction). §6.2's temporary
ceiling is **empty**, and it is shrink-only, so "add the field now,
narrow it later" no longer exists: a module gaining unrestricted access
fails the audit even if §6.2 is also edited to document it. The audit
additionally parses §6.1 itself and requires its documented set to
equal the checked-in `PERMANENT_DEFINER`/`PERMANENT_IMPORTERS`
constants, with a real justification on every row — so neither the doc
nor the constants can admit a permanent importer alone. §6.4(c)/(d)
govern the two escape hatches (a ninth capability; a new §6.1 module);
both need explicit maintainer approval and synchronized doc + constant
+ self-test changes.

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
- `World.Hydrology` — per-Age geological hydrology: flow accumulation,
  river/glacier carving, and the subsurface water table
- `World.Fluid` — global identification of the FINAL rivers, lakes, ocean,
  seabed and ice. The main identifiers read the stitched, settled terrain,
  but `Ocean` and `IceLevel` are prepared earlier from pre-stitch grids
  (`docs/hydrology_pipeline.md` §5). Not runtime simulation: fluid only
  moves in `Sim.Thread` / `Sim.Fluid.Active`
- `World.Flora` — vegetation placement
- Chunk-based with zoom-level LOD system (`World.Render.Zoom.*`, `World.ZoomMap`)

[`docs/hydrology_pipeline.md`](docs/hydrology_pipeline.md) is the
namespace-ownership map for water: the five pipeline stages in order, which
namespace owns each, the two distinct river-carving mechanisms, where ocean
and lake logic live, and a "where does X live?" index. Read it before adding
river, lake, ocean, ice, or water-table logic — the namespaces do not divide
the way their names suggest.

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
`#text` or byte-based `string.sub`. Since #1187 the debug console's own
input line (`scripts/shell.lua`) holds the same contract: `cursorPos`
and `inputScrollOffset` are code-point offsets into `inputBuffer`, and
that includes the derived paths — tab completion (`longestCommonPrefix`
snaps its byte-wise agreement point back to a character boundary, since
two candidates can agree on part of one emoji), the ghost hint, and the
scroll/measure walk. The Delete key arrives as `onTextDelete`
(`LuaTextDelete`), not `onDelete`. `config/shell_history.txt` is the one
buffer ingress that isn't engine-delivered text, so a line that isn't
valid UTF-8 is dropped at load. Gate: hspec `--match "Lua.ShellInput"`.

**Text display (#1159):** the same rule binds read-only DISPLAY paths —
wrapping, truncation, and any other per-character walk advances one code
point at a time, or non-ASCII text renders as mojibake and gets measured
once per byte. Lua PATTERNS are byte-oriented too, so `gmatch(".")` is a
byte loop, not a character loop. Pixel-width wrapping goes through
`scripts/ui/text_wrap.lua` — `byCharacter` (the debug console) and
`byWord` (word wrap with a character hard-break; all three log panels) —
rather than a fourth private copy. Unlike `utf8_safe`, it never raises on
malformed UTF-8 and never drops a byte: display paths wrap whatever an
arbitrary Lua value stringified to. Gate: hspec
`--match "Lua.TextWrapping"`.

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

## Working-tree discipline

**`~/work/synarchy` is the PRIMARY checkout and must be left CLEAN.** The PR
drainer fast-forwards it after every merge, which means it autostashes whatever
uncommitted work it finds there and restores it afterwards. A restore that
CONFLICTS leaves unmerged entries in the index, and every later drainer pass
then refuses to run — post-merge cleanup wedges until a human resolves it. That
happened four times (2026-08-01, 08-03, 08-05, 08-09), every single time on
`docs/code_health_findings.md`, because report-processing writes long-lived
uncommitted edits into the one file that merged PRs also rewrite.

So: **any file you write into the repo but do not commit belongs in the docs
worktree, never the primary checkout.** That covers report annotation
(`/process-report`), findings documents, design-doc drafts, and anything else a
workflow leaves sitting for review. Resolve it by BRANCH — never hard-code the
path, and never assume the current directory is right:

```bash
DOCS_WT="$(git worktree list --porcelain \
  | awk '/^worktree /{p=substr($0,10)} /^branch refs\/heads\/docs-wip$/{print p; exit}')"
[ -n "$DOCS_WT" ] || { DOCS_WT=~/work/synarchy-docs
                       git worktree add "$DOCS_WT" -b docs-wip origin/master; }
```

Docs land on master by direct push, not a PR. Landing ONE document while others
are still being written is the normal case, so the rebase must tolerate a dirty
tree — a plain `git rebase` aborts with "cannot rebase: You have unstaged
changes" and strands the landing:

**Use `tools/docs_land.sh`, not a hand-rolled sequence:**

```bash
tools/docs_land.sh -m "Commit subject" docs/foo.md [docs/bar.md …]
tools/docs_land.sh -n -m "…" docs/foo.md      # dry run
tools/docs_land.sh -f -m "…" docs/foo.md      # proceed despite the risk warning
```

It resolves the worktree by branch, commits ONLY the paths you name, skips the
rebase entirely when master has not moved, judges success by `rev-list` rather
than push output, and fast-forwards the primary checkout only when it is clean.

Its reason to exist is the pre-flight check: it refuses, before committing or
stashing anything, when a file that is dirty here but NOT being landed has also
changed on master. That combination is exactly what makes a rebase autostash
conflict — and it is what actually happens, repeatedly, on
`docs/code_health_findings.md`, which merged PRs rewrite constantly.

The equivalent by hand, if you ever need it:

```bash
cd "$DOCS_WT" && git add -- <paths> && git commit -m "…" \
  && git fetch origin && git rebase --autostash origin/master \
  && git push origin docs-wip:master
```

`--autostash` is required there, not decorative. Should ITS restore conflict,
the damage is confined to this worktree and surfaces immediately in front of
you — it cannot wedge the drainer, which is the whole point of doing the work
here.

**`docs-wip` is not a feature branch.** It tracks `origin/master` and lands by
direct push, so it is a second working copy of master rather than something that
accumulates and merges later. Uncommitted work can sit in it indefinitely
without the drainer ever seeing it; that is its whole job.

A bare `git push` from this worktree fails safe: `docs-wip` tracks
`origin/master`, and the differing name makes `push.default=simple` refuse. Use
the explicit refspec above.

That push prints `Cannot update this protected ref` and
`N of N required status checks are expected` and then **succeeds anyway** under
admin bypass — judge it by
`git rev-list --left-right --count HEAD...origin/master`, not by the warning.

Exempt, because they either create their own worktree or must operate on the
primary checkout: `solve`, `pr-revise`, `repair`, the read-only `pr-review` /
`pr-rereview` / `issue-review` reviewers, `drain-prs`, `janitor`, `finalize`.

## Findings-report field ownership

A findings report (`docs/code_health_findings.md` and its siblings) is written
by two independent lanes, and they own DIFFERENT FIELDS of the same entry.

**The report-processing lane (`/process-report`) exclusively owns an entry's
status fields:** the checklist checkbox, the trailing checklist marker, and the
heading marker (`[#N]`, `[#N, <note>]`, `[no-issue]`, `[deferred]`, or none).
It is the only lane that may add, remove, or change any of the three, and it
changes them together in one edit.

**An implementation PR may add to or update a finding's narrative body, and
nothing else.** Landing the fix for a finding does not disposition it — a PR
that marks the entry it resolves has answered a question the processing lane
had not asked yet. Say what changed in the body if it helps; leave the box, the
checklist marker, and the heading marker exactly as you found them.

That split is not stylistic. The two lanes had already drifted an entry in each
direction — `82607204` marked CH-126's heading while landing the fix and left
its checklist bare, `89b015d3` marked CH-73's checklist and left its heading
bare — and each drift re-files merged work, because the processor selects a
bare-headed finding as unprocessed and the "headings win, correct the
checklist" tie-break then unchecks a finding an issue already resolved. The
cost lands on other people's PRs too:
`.github/workflows/review-gate.yml:106-116` strips `reviewed:approve` when a
push touches a file an open PR also owns, so every master-side report edit
costs an open PR that touches the report its approval.

`tools/findings_report_audit.py` (CI + `make ci`, with its own
`tools/test_findings_report_audit.py` self-test) fails when a CH item's heading
marker and checklist marker disagree, and when the two sides do not declare the
same set of CH numbers exactly once each. It audits AGREEMENT only — whether a
marker is the right one, and whether the box matches its terminality, stay the
processing lane's judgement.

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

### CLI flags per boot mode

`app/Main.hs` selects exactly one of six boot modes from argv, in this
precedence when more than one selector is present:
`--language-report` > `--dump` > `--preview` > `--offscreen` >
`--headless` > graphical (the default, no selector needed). Every
ancillary flag below is honoured only by the mode(s) listed — passing it
to any other mode exits 1 before any engine, window, or server starts,
naming both the flag and the selected mode (CH-58). `--resource-root
<path>` (or `SYNARCHY_ROOT`) is the one global flag: it applies to and
is validated before every mode.

| Flag | Honoured by |
|---|---|
| `--seed`, `--worldSize`, `--plates` (alias `--ages`), `--region` | `--dump` |
| `--size` | `--offscreen` |
| `--seeds` | `--language-report` |
| `--arena` | `--headless`, `--offscreen`, graphical |
| `--port` | `--headless`, `--offscreen`, `--preview`, graphical |

The rejection table lives in `app/Main.hs`'s `incompatibleFlagTable`;
`tools/preview_cli_probe.py` is the no-boot gate covering it.

**A present-but-malformed value is an error, not a default (#1191).** In
a mode that honours it, `--seed`/`--worldSize`/`--plates`/`--ages`/
`--port` that isn't a whole number, a `--size` that isn't `WxH` with both
dimensions positive, a `--dump=` selection naming an unknown layer, and
an empty selection or empty segment (`--dump=`, `--dump=terrain,`) each
exit 1 pre-boot naming the flag and the offending token. **Omitting** a
flag still keeps its documented default — only a value the user actually
typed can fail. Validation runs after the mode-compatibility rejection
above (which keeps its priority: a malformed `--seed` given to
`--headless` still reports as unsupported in headless mode) and before
every mode-specific early exit, regardless of whether the selected mode
would consume the value. `--region` is deliberately excluded — its
identical silent default is `docs/code_health_findings.md` CH-67,
sequenced after #1081. Gates: hspec `--match "App.Cli"`,
`tools/preview_cli_probe.py`.

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

**The console is required in `--headless`/`--offscreen` (#1190).** They
have no window, so it is their only interactive control surface: if the
listener can't start — an occupied or unbindable port, or `--port 0`
(issue #46's "no TCP listener at all" sentinel, which belongs to
`--dump` alone) — the boot ABORTS. It exits non-zero, prints no `READY`
marker, names the mode / effective port / cause on stderr, and tears
down what it had already built (the pre-thread Lua state, plus
offscreen's input worker), each cleanup step announcing itself on
stderr. So the wait loop above fails fast instead of hanging forever on
a live process with no reachable `engine.quit()`. `--dump`,
`--graphical` and `--preview` keep their existing tolerance unchanged,
port-0 behavior included. The per-mode decision is
`Engine.Scripting.Lua.DebugServer.debugConsolePolicy`, keyed on
`EngineConfig`'s `ecBootMode` — `ecHeadless` can't tell dump from
headless and is `False` for offscreen. Gates: hspec
`--match "debug-console listener policy"`,
`tools/debug_console_boot_probe.py` (CI-eligible).

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

### Preview mode: asset browser (#632/#886/#887/#888, epic #427)

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
  `--preview <category>/<item>`, e.g. `--preview units/acolyte`,
  `--preview buildings/acolyte_portal`, `--preview flora/scots_pine`,
  `--preview structures/wire`): `units`, `flora`, `buildings`,
  `structures`. An item is exactly ONE contained, non-symlinked direct
  child DIRECTORY of the category root
  (`Engine.Preview.Discovery.resolveItemDir`, shared by all four): an
  unknown name, a name with path structure or `.`/`..`/absolute
  traversal, a symlinked directory, and a FILE where a directory was
  expected (`flora/unknown_flora.png`) all exit 1 **before a window
  exists**.
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
- **`flora/<name>` and `structures/<name>` reuse this exact browser**
  (#888), rooted at the ITEM's folder instead of the category root: they
  are flat sets of static PNGs (stage textures, piece sprites), so they
  deliberately have no viewer of their own — same list, same first-entry
  default, same static preview, `mode == "list"` in the dump. Anything
  beyond routing the resolved folder into `discoverEntries` means the
  routing is wrong, not the reuse. (Assembled-structure visualization
  and animated flora stay epic-deferred.)
- Trimmed loading: preview mode loads only its font, the list widget's
  own chrome textures (`assets/textures/ui/{highlight,scroll*}.png`,
  loaded once, list-mode only), and textures within the requested
  category/item — never `data/*.yaml` gameplay catalogs. There are
  exactly TWO exceptions, both a single file for the requested item:
  the units viewer's `data/units/<name>.yaml` and the buildings
  viewer's `data/buildings/<name>.yaml`.
- Debug-console introspection: `require("scripts.preview_manager").dump()`
  (self-registered into `package.loaded` the same way `unit_ai.lua`/
  `debug.lua` are, despite being `engine.loadScript`-loaded, not
  `require`d) reports `mode`
  (`"list"`/`"item"`/`"unit"`/`"building"` — #632's `"placeholder"` is
  GONE as of #888, every canonical category now dispatching to real
  behavior), `state`
  (`"loading"`/`"ready"`/`"empty"`), the current `selected` entry, and in
  list mode the FULL ordered `entries` list (not just its `entryCount`
  — a probe needs the complete list to catch an omission/substitution
  anywhere past the visible/selected rows), `scrollOffset`, and
  per-visible-row interactive bounds/handles (`rows`,
  `scripts/ui/list.lua`'s existing F3 dump contract) — enough to drive
  real `input.click`/`input.scroll` against a located row without ever
  hardcoding a screen coordinate.

Units viewer (`--preview units/<name>`, #887; `Engine.Preview.Unit`
pre-boot + `scripts/ui/unit_animation_view.lua` in-engine):

- **The filesystem is authoritative.**
  `assets/textures/units/<name>/animations/` decides which animations
  exist, which directions each has, and the `frame_NNN.png` order
  (NUMERIC, so an unpadded `frame_10` can't sort before `frame_2`).
  `data/units/<name>.yaml` only AUGMENTS a matching animation with
  `fps`/`loop`/`flip`. Since #1257 every shipped animation folder IS
  declared, in one of two forms (see **Unit asset inventory** below):
  `tiller`, `unknown_unit` and `white_tailed_deer` carry `asset_units:`
  files, and `acolyte/pushing_idle`, `bear_brown/roar` and
  `technomule/hit_react` gained ordinary entries. The viewer's
  missing-metadata fallback is retained for uncommitted local content
  and falls back to the SAME values `UnitYamlAnim`'s decoder defaults
  to: `fps=8`, `loop=true`, `flip=false`.
- **Ordering + default selection:** animations sort case-sensitively by
  exact directory name (the same `Ord`-on-the-label rule
  `Engine.Preview.Discovery.sortEntries` uses); `idle` is selected when
  present, else the first entry in that order, direction south.
- **Directions:** the game's own `S, SW, W, NW, N, NE, E, SE` order. A
  directly authored direction ALWAYS wins; W/SW/NW mirror SE/E/NE only
  when flipping is permitted. With no YAML entry the viewer INFERS
  mirroring for exactly the canonical five-direction layout
  `{S, SE, E, NE, N}` — any other stored set leaves its missing
  directions unavailable rather than inventing them or falling back to
  another unit's textures. A mirrored cell renders genuinely mirrored
  via `UI.setSpriteFlipX` (#887's `ussFlipX`, applied to the CLIPPED UV
  slice — flipping before clipping would sample the wrong slice; #1259
  generalized that reflection to the sprite's own source sub-rect).
- **Playback:** ONE clock per selected animation. Every direction
  computes its own index from the SAME elapsed value against its OWN
  frame count, so unequal per-direction frame counts (four checked-in
  acolyte animations have them) stay phase-aligned. Selecting a
  different ANIMATION resets the clock; enlarging a different DIRECTION
  does not. Non-loop end-of-clip HOLDS the last frame — the same clamp
  `Unit.Render.pickFrame` applies in game. The frame index comes from a
  wall clock, so the script tick rate only affects smoothness.
- **Reflow:** a resize preserves the selected animation, selected
  direction, list scroll offset, AND playback phase.
- **Pre-boot rejection:** `units/<name>` must be exactly one contained,
  non-symlinked direct child of `assets/textures/units` holding a
  non-symlinked `animations/` subtree. An unknown unit, a name with path
  structure or `.`/`..`/absolute traversal, a symlinked unit directory
  OR symlinked `animations/` root, and a unit with no animations all
  exit 1 before a window exists. Both symlink levels matter —
  `doesDirectoryExist` follows links, so a real unit directory with a
  symlinked `animations/` would otherwise browse and load another
  tree's assets, breaking trimmed loading. A missing YAML is NOT a
  rejection.
- **Dump extension:** unit mode adds `unit`, the animation `entries`
  list (each with `fps`/`loop`/`flip`/`thumb`/`directionCount`),
  `defaultAnim`, and `playback` — current `animation`, `direction`,
  `mirrored`, `sourceDirection`, `frameIndex`, effective `fps`/`loop`,
  plus a per-direction `directions` array carrying each cell's own
  mirrored flag, source, frame index, and interactive bounds/handle
  (enough to locate and click a real cell without a hardcoded
  coordinate).

Buildings viewer (`--preview buildings/<name>`, #888;
`Engine.Preview.Building` pre-boot +
`scripts/ui/building_asset_view.lua` in-engine):

- **The filesystem is authoritative**, the same split the units viewer
  uses. The building's own folder decides which entries exist and, in an
  animation directory, the numeric `frame_NNN.png` order;
  `data/buildings/<name>.yaml` only AUGMENTS a matched animation with
  `fps`/`loop` and supplies the default-selection hints. A missing,
  malformed, or unmatched YAML never rejects a valid asset folder
  (`dungeon_1` has no YAML at all; `cargo_hold_S`/`furnace` ship a
  `demolish/` folder no YAML mentions).
- **One list, both kinds.** A recognized animation directory is ONE
  entry labeled by its directory name; every other directory is
  descended into so its textures surface as ordinary item-relative
  statics (`dungeon_1/damaged/floor.png`) rather than being played as
  one clip or silently lost. Ordering is the single label-lexicographic
  rule the rest of the browser uses, across both kinds together.
- **A directory is an animation** iff a YAML animation's declared frame
  paths live in it, OR every `.png` in it follows the numbered-frame
  convention (`frame_000.png`, `frame_10.png`, `frame-3.png`).
- **YAML association is by CONTENT, never by equal names.**
  `acolyte_portal.yaml` names its animations `portal-appear`/
  `portal-idle` while the directories are `appear/`/`idle/`, so a
  directory is matched through the frame paths its animation declares.
- **Default selection ladder:** `state_animations.built`'s animation
  (resolved that same way — selected label `idle`, not `portal-idle`),
  else the def's own `sprite` when it names a discovered static, else
  `default.png`, else the first entry. `dungeon_1` (no YAML, no
  `default.png`) lands on the last rung.
- **Playback defaults are `fps=8`, `loop=false`** — `BuildingYamlAnim`'s
  own, NOT the units viewer's `loop=true`. One wall clock per selected
  animation, reset on a real selection change but preserved across a
  resize; non-loop end-of-clip HOLDS the last frame. A STATIC selection
  has no playback at all.
- **Dump extension:** building mode adds `building`, the ordered
  `entries` list (each with `kind` `"animation"`/`"static"`, `animated`,
  `fps`, `loop`, `frameCount`), `defaultEntry`, `selected`,
  `scrollOffset`, per-visible-row `rows` bounds/handles, and — for an
  animation selection ONLY — `playback` (`entry`, `frameIndex`,
  `frameCount`, effective `fps`/`loop`, `ready`).

Gates: `tools/preview_cli_probe.py` (CI-eligible, no boot at all — every
check above the "always opens a real window" line, units and
flora/buildings/structures item rejections included) and
`tools/preview_probe.py` (manual-only, `needs-gpu`, ~15 window boots —
the real-boot browser checks: discovery/selection/scroll/resize via the
dump, forced nearest filtering, the whole units and buildings viewers
above, flora/structures dispatching into the shared simple browser, a
final every-canonical-category no-placeholder sweep, and trimmed loading
verified against `engine.getLoadedTexturePaths()` — `Engine.Asset`'s
`apAssetPaths`, populated by `engine.loadTexture`'s own Haskell handler
regardless of Lua caller, so this is the engine's own authoritative
loaded-texture record, not previewManager's self-reported bookkeeping).
Focused hspec coverage for the pure
discovery/labeling/ordering/containment logic:
`cabal test synarchy-test-headless --test-options='--match "Preview.Discovery"'`,
`--match "Preview.UnitAnimation"`, and `--match "Preview.Building"`.

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
| `waterTableZ` | terrain | Finalized per-tile water-table z from the chunk's own map (climate baseline, fluid/shoreline-adjusted) |
| `waterTableSummer`, `waterTableWinter` | terrain | Seasonal water-table z-levels for the tile, bilinearly interpolated from the climate model |
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
# world.init(pageId, seed, worldSize, plateCount
#           [, displayName[, gloss[, languageSeed[, languageVersion]]]])
# The optional identity (#707) is display text, immutable per page,
# persisted in saves, independent of pageId and save-slot name;
# world.getIdentity(pageId) reads it; engine.listSaves() exposes
# worldName/worldGloss. A name supplied with no languageSeed is a
# CUSTOM name and has NO language provenance (#1092) —
# world.getLanguageProvenance(pageId) returns nil for it, and
# { seed = "<decimal string>", version = N } only for an identity built
# through the generated-name path (the seed is a STRING: a Word64 has
# no lossless Lua number). languageSeed (#1101) is that path: it states
# that displayName/gloss were RENDERED from that language, and is what
# makes the page's placed locations named in the same one. It is a
# decimal string; languageVersion defaults to the current generator.
# Provenance is never inferred: with no displayName there is no
# identity to attach it to, and a malformed seed or an unconstructible
# version is refused with a warning, leaving an ordinary custom name.
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
- **Tile-coordinate frame at the U seam (#1175)** — chunks are STORED
  u-wrapped, so one physical tile has two names near the seam. ONE
  contract, stated in full on `World.Render.HitTest`: `pickWorldTile`
  and every Lua caller it backs (`world.pickTile`/`pickPos`/
  `getHoverTile`/`getHoverPos`) report CANONICAL coords, and the
  fractional hover position takes the SAME whole-tile shift as the
  integer tile; designation maps (mine/chop/till/plant/construct) store
  canonical keys; every point read, mutation and cancellation accepts
  any alias and returns canonical — including the verbs a worker
  FINISHES a job with (`world.getDigInfoAt`/`digTile`, `harvestFlora`,
  `setVegAt`, `plantCropAt`/`plantRowCropAt`, `structure.place`/`hasAt`/
  `floorZAt`/`clear`, and `building.spawn`/`canPlaceAt` — whose footprint
  walk resolves each tile, since a footprint is stepped off its anchor
  and straddles the seam even from a canonical one), which is what lets a
  job coord persisted by a pre-#1175 save run to completion with no
  migration. Rectangles are the exception that
  makes it work: canonical is a STORAGE frame, not a geometry one — two
  adjacent tiles across the seam sit a whole world apart in it — so a
  drag's second endpoint is re-expressed in the anchor's local alias
  frame (`localizeTileToAnchor`, shared by
  `World.Thread.Command.Cursor.Common.designateRect`, the
  `CursorQuads` previews, and Lua via `world.localizeTile` for
  `build_tool.lua`'s wire snap / occupancy scan) BEFORE any clamp or
  `min`/`max`, with canonicalisation per enumerated tile at
  lookup/storage only. A job-SELECTION range gate needs that frame too —
  a seam-side job measures a world away in canonical coords and would
  never be claimed — so `construction.getPendingJobs` reports `lx`/`ly`
  beside the canonical `x`/`y`, and `unit_ai_construct.lua` measures with
  those. Canonicalising one end alone was MEASURED worse
  than the old seam-blind behaviour; don't do it. Away from the seam,
  and in arena / non-wrapping worlds, every step is the identity.
  Persistence: `world-activity` v2 (same bytes, canonical-key
  invariant); a v1 payload is re-keyed on load. The init QUEUE
  (`world.loadChunksInRegion`, `World.Load.Stage`'s saved-camera radius,
  world init) is wrapped at the drain, so every loader stores canonically
  — before that, a seam-crossing region generated a SECOND chunk for one
  physical place and canonical readers resolved to whichever the camera
  loader had put there. Seam VISIBILITY is the
  separate axis #1176 owns — which tile a pixel NAMES is this contract,
  where that tile is DRAWN is `bestWrapOffset`'s facing-aware `(x, y)`
  offset; both hold at all four facings. Gates: hspec
  `--match "World.Render.PickSeam"`, `--match "World.DesignationSeam"`.
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
- **Player transfers + orders (#1000/#1085/#1246/#1247)** — ONE policy
  (`src/Unit/Transfer.hs`, pure) decides whether exact item instances may
  move between two endpoints (a unit inventory or a built building's loose
  storage, on BOTH sides; direction is DERIVED from the pair). Proximity is
  Chebyshev ≤ 1 between the occupied RECTANGLES, capacity weighs the actual
  instance, a batch is ordered and reports per-item outcomes, and no item
  ever half-moves. The lax AI verbs
  (`unit.transferItemToUnit`/`transferItemToBuilding`/`depositToCargo`/
  `withdrawFromCargo`) are a SEPARATE, deliberately unchecked path the
  fetch/repair/medic ladders depend on — never route AI work through the
  strict one. A durable ORDER (#1246's per-page store, `wsTransferOrdersRef`)
  adds distance: `unit.createTransferOrder` validates with adjacency DEFERRED
  (`ReachPolicy`) — same page still required — because the endpoints are not
  adjacent yet and the create-time capacity gate must be reached anyway;
  `unit.checkTransfer`/`commitTransfer` keep requiring adjacency, unchanged.
  `scripts/unit_ai_transfer.lua` then walks the ACTING unit (recorded beside
  the endpoint pair, so a building→building order has no approach and this
  executor skips it) at comfort pace, holding a 7.5 in-progress lock, and
  ARRIVAL IS THE COMMIT: `unit.commitTransferOrder` re-validates atomically,
  so a refusal there is recorded as `became_stale` carrying the real reason as
  its cause, and only `ready_to_commit` entries are ever submitted — a
  create-time refusal is never retried. The stall timer is a STALL timer
  (60 s of ELIGIBLE time, reset on every new closest approach), never a
  trip budget. Terminal orders STAY in the store — exactly-once comes from
  the lifecycle, not from deletion, and pruning is #1253's. Gates: hspec
  `--match "Unit transfer"` (contract + both Lua surfaces),
  `tools/transfer_order_probe.py` (manual-only).
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
  isn't double-counted, while other consumers still sum.
  **A node's LIFETIME is its building's** (#1206): `BuildingDestroy`
  retires the node in the same live transaction that removes the
  instance (`Power.Live.retirePowerNodeEverywhere`, resolving the
  session-global `BuildingId` across every live page — the
  `forgetContainerEverywhere` pattern), so a demolition never reaches
  the save. That is NOT load-time pruning: the #758/#763 tolerance
  stands, and a save already carrying a dangling node still restores it
  verbatim. Retirement is a delete, never a compaction — surviving ids
  are untouched and `pnsNextId` keeps advancing, so a retired id is
  never handed to a later placement. There is deliberately no public
  `power.removeNode`. Gates: `power_probe.py`, `power_workshop_probe.py`,
  `machine_shop_probe.py`, hspec `--match "power node demolition"`;
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
- **Location instances (#911)** — a placed location is a persisted
  per-page record (`Location.Instance`) keyed by a stable
  `LocationInstanceId` (from 1), allocated at PLACEMENT time in the
  deterministic overlay's `overlayToList` order — never at stamp time,
  never from hashmap order — so ids survive save/load and chunk
  eviction. It stores its definition id, anchor, resolved absolute
  bounds (#777), discovery margin, display name AND optional English
  gloss (#1101 — see below), one-time content-spawn flag
  (#90), and lifecycle `unknown → hinted → discovered → active →
  cleared → depleted`. Consumers read the STORED values, never
  re-derive them from the live registry. `wgpLocationStamped` stays
  chunk-keyed (#424) and was untouched. Transitions are one-way
  (`promoteLifecycle` refuses backward AND same-state), which is what
  makes discovery fire exactly one event. Nothing drives an instance
  past `discovered` yet; `hinted` is deliberately unreachable (every
  location is cartographically visible for now — it is reserved for a
  future information-revealed class, don't delete it). Queries:
  `world.listPlacedLocations([pageId])` (extended, not repurposed —
  `id` is still the DEFINITION id; `instance_id`/`lifecycle`/`name`/
  `contents_spawned` are new), `world.getLocationInstance(id[, pageId])`,
  `world.setLocationLifecycle(id, name[, pageId])`,
  `world.markLocationContentsSpawnedById(id[, pageId])`. The
  coordinate-addressed `hasSpawnedLocationContents`/
  `markLocationContentsSpawned` remain compatibility wrappers resolving
  to the chunk's first instance. Persistence: `world-pages` (v6 since
  #1104; v5 since #1102; v4 since #1101; v3 since #1092; #911 introduced
  its v2), with a
  frozen v1 DTO whose per-chunk flags decode PENDING and are resolved
  against the location registry at the load path's content-validation
  stage (`resolveLegacyLocations`) before publication. Gates: hspec
  `--match "Location instance identity"`, `location_content_probe.py`.
- **Location naming (#1101)** — a placed instance's `name` is rendered
  in its PAGE's own generated language, resolved from the identity's
  #1092 provenance, and its `gloss` is the same `NameExpr`'s English
  reading. Which concepts a location may draw on is DATA
  (`ldNaming`: two ordered, nonempty `heads`/`modifiers` concept-id
  pools on the definition, validated against `data/language/concepts.yaml`
  when the file loads — an unknown id or a missing lexical form rejects
  the whole file rather than degrading to `ldLabel`); the engine has no
  `ldType`→concept mapping. The expression is always
  `Modifier modifier head`, chosen deterministically from the
  instance's own stable `liId` (plus the language seed/version and the
  def id), never from hashmap order. Names are WRITE-ONCE (#708
  principle 5): rendered at instance creation
  (`Location.Instance.newLocationInstance`, the only writer) and read
  thereafter — no load, migration, or definition edit re-derives one, so
  a location placed before this landed keeps its label forever. A page
  with NO provenance (a custom-named world, a pre-#1092 save) falls back
  to `ldLabel` with the `gloss` key ABSENT; absence is never papered
  over by inventing a language. Gate: hspec `--match "Location naming"`,
  plus `location_content_probe.py` phase 5.
- **River identity + naming (#1102)** — `world.getRivers()` gives every
  river an `id`: the `GeoFeatureId` the timeline already allocated, so
  the durable identity is `(WorldPageId, GeoFeatureId)` (feature ids
  restart at zero per timeline, and the query only ever reads the ACTIVE
  page). `World.River.Identity` is the ONE place events are paired with
  features — compaction re-emits exactly one event per active river
  feature in `gtFeatures` order, and the pairing is CHECKED against
  source/mouth/flow before it is trusted, so a violated invariant yields
  no id rather than a wrong one. A named river also carries `name` and
  `gloss`; both keys are ABSENT (nil) otherwise. Names live in a
  per-page `wgpRiverNames` table keyed by `GeoFeatureId` — deliberately
  NOT on `PersistentFeature`, whose `GeoTimeline` is a positionally
  serialized worldgen-OUTPUT schema; naming moves no terrain and no
  baseline. The expression is `Modifier modifier head` over a NARROW
  in-code head pool (`riverHeadConcepts`: `RIVER` — added to the
  catalogue by this issue — plus `FORD`, `CROSSING`, `BAY`, `VALE`,
  `HOLLOW`) and a WIDE modifier pool (every catalogue concept with a
  modifier form): the asymmetry is what makes a head morpheme recur
  across a map's rivers and in the world's own name. Rivers have no
  definition file to author pools on, which is why these are code and
  not data. WRITE-ONCE (#708 principle 5): `buildRiverNames` at world
  init is the only writer, so growing the catalogue never re-renders a
  stored name even though `assignLanguageRoots` re-resolves collisions
  over the whole concept set. A page with no provenance has an EMPTY
  table — ids still work. `Language.Naming` holds the machinery both
  this and #1101 use. `Language.Suggest` (#1106) resolves the same
  profile + roots + catalogue triple itself and is the one remaining
  copy — fold it in rather than adding a fourth. A river's stored
  `rvnEtymology` (#1104) is what lets its name be decomposed.
  Gates: hspec `--match "River naming"` / `--match "River identity"`,
  the shared-world identity specs under `--match "Location overlay"`,
  and `tools/river_naming_probe.py`.
- **Name etymology (#1104)** — a generated name can be decomposed into
  its roots and meanings. What makes that possible is a small optional
  `EtymologySource` (the originating `NameExpr` plus the
  `LanguageProvenance` that rendered it) persisted beside the name on all
  three carriers: `wiEtymology`, `liEtymology`, `rvnEtymology`. A
  precomputed morpheme list is deliberately NOT stored — the presentation
  is reconstructed on query. `Language.Generated.Render` now produces an
  ordered token TRACE and `renderNative` IS its concatenation, so
  "concatenating the trace reproduces the stored name" holds by
  construction; `Language.Generated.Boundary.joinMorphemesTrace` is the
  one implementation both views of a boundary share. `Language.Etymology`
  re-renders from the source and CHECKS the result against the
  authoritative stored text before showing any of it — a mismatch (a
  tampered name, a source from another language, a historical version
  this build renders differently) reports unavailable rather than
  explaining the wrong word. Morpheme identity is
  `(LanguageProvenance, ConceptId)` — never spelling — so #1096's bound
  form and its free root are ONE morpheme while two languages'
  homographs, and the SAME seed under two generator versions, are not.
  Capitalization is a surface-POSITION effect: the leading token carries
  it, every canonical free spelling stays the unmarked lowercase root.
  A source is additionally
  required to belong to the PAGE's own recorded language
  (`decomposeEntityName`): the surface check proves an expression renders
  to the stored text under ITS OWN language, so a stale or foreign source
  that happens to reproduce those letters would otherwise pass while
  attributing every morpheme — and every recurrence link — to a language
  the world does not have. A page with no provenance admits no source at
  all. `world.getEtymology(kind[, id][, pageId])` feeds world/location/river
  adapters into that one path; an unavailable reply still carries the
  stored name so the UI can keep showing it. Recurrence is computed on
  demand from the ACTIVE page — current world + `LifecycleDiscovered`-or-
  later locations + ONLY the river being inspected (a world or location
  target admits no river at all), the inspected entity excluded from its
  own links, entries exposing nothing but an entity kind and an
  already-visible name. There is no session history. The optional
  `pageId` names the TARGET only (#1265) and never widens that set:
  omitted, target and recurrence are both `resolveActiveWorld`'s page; a
  live INACTIVE page resolves the target there — its stored name, gloss,
  source and page-language validation all that page's — while candidates
  still come only from the active page, so no inactive name is ever a
  recurrence entry; a page that does not exist is the unchanged
  `available=false`/`no_entity`. With no visible page, recurrence follows
  `resolveActiveWorld` exactly, head-of-`wmWorlds` fallback included, and
  substitutes nothing when that resolves to `Nothing` — a missing
  ingredient on the RECURRENCE page (no active page, no gen params)
  leaves an explicitly selected target's result intact with recurrence
  empty, never downgrading it. That crossing is what makes
  self-exclusion PAGE-QUALIFIED: every page's world entry is
  `("world", Nothing)` and location ids are page-local, so comparing kind
  and id alone would silently drop the active page's own world name, or
  an equal-numbered active location, from an inactive target's links.
  A river target on another page admits no river at all — the inspected
  river is not on the active page, and its `GeoFeatureId` re-resolved
  there is a different river. `world.getRiverAt`
  is the minimal selected-segment→identity resolution (channel
  containment, nearest wins, no global river list). The expression
  travels the whole Create World chain — `world.suggestName`'s `expr` →
  `name_suggest` → `generation` → `world_view` → `world_manager` →
  `world.init`'s 9th argument — and is cleared with the gloss and
  provenance the moment the player edits the name. UI:
  `scripts/etymology_panel.lua` is the ONE panel all three entry points
  open, hosted by `scripts/name_plate.lua` on `hud.global_page` (NOT
  `world_page` — a world's name is not a zoomed-in concern, and a plate
  on a band-swapped page is unhittable in the zoom map). Persistence:
  `world-pages` v6, with `PageCoreDTOv5`/`WorldGenParamsDTOv4`/
  `WorldIdentityDTOv2`/`LocationInstanceDTOv2`/`RiverNameDTOv1` frozen —
  every historical shape decodes with the source ABSENT, never inferred.
  Gates: hspec `--match "Language etymology"` / `--match "Etymology
  panel"`, `tools/etymology_probe.py` (manual-only, `needs-gpu`).
- **Location discovery (#780)** — a one-way lifecycle promotion to
  `discovered`, fired when a `uiFactionId == "player"` unit enters the
  instance's `discovery_margin` halo; ticks for EVERY loaded page,
  independent of pause; emits exactly one `location_discovery` event
  (hidden-page discoveries omit clickable coords). Independent of the
  stamped/contents-spawned flags. Gates:
  `location_content_probe.py`, `location_embark_probe.py`; hspec
  `--match "Location discovery"` / `--match "Location map icons"`.
- **Per-unit location knowledge (#915)** — the EXPERIENTIAL layer beside
  that CARTOGRAPHIC one, and neither derives from the other: global
  lifecycle = "the player has mapped it", `aiState[uid].knownLocations`
  = "this acolyte knows where it is". Keyed by the durable
  `(page, instance id)` pair — dedup is by IDENTITY, never by distance
  (don't copy `knownWaterSources`' 6-tile rule across; two locations are
  never the same location). Both layers come from ONE containment
  enumeration in `Location.Discovery` (`findDiscoveries` /
  `findAwareness`), so they cannot drift; awareness additionally reports
  EVERY qualifying unit and ignores lifecycle, so a unit arriving at an
  already-mapped ruin still learns it. `world.getLocationAwareness()`
  walks every loaded page; `scripts/unit_ai.lua` ingests it BEFORE its
  pause guard, mirroring the discovery tick's pause independence.
  Persisted via `lua.unit_ai` v4 as typed
  `{__ref="location_instance", page, id, x, y}` entries (v1-v3 decode
  with the field ABSENT — never inferred from discovery); a memory whose
  `(page, id)` is missing from the restored session is a non-blocking
  diagnostic, scrubbed at reconcile time. Sharing over the radio and
  radio range are deliberately deferred. Gates: hspec `--match "unit
  location knowledge"`, `location_content_probe.py`.
- **Expedition retrieval (#920)** — recovering an item from a remote
  location uses ONLY the direct-RTS verbs a player already has
  (`unitAi.commandPickup` → `unitAi.commandMove` home → adjacent
  `unit.depositToCargo`); the design doc forbids a caravan/logistics
  interface until direct retrieval proves inadequate. `commandPickup`
  gates capacity at COMMAND time (refuses, returns false, emits a
  player-visible `unit_warning` naming carrier and item, sets no
  `pickupOrder`) AND still on ARRIVAL, both measuring
  `unit.getCarryingWeight` against the ground instance's live
  `item.listGround().weight` — keep both; the load changes en route. A
  completed pickup emits a `unit_event` naming the item, tagged with
  the carrier's uid — that is the surface answering "who has it".
  `pickup_timeout` and `TASK_TIMEOUT_SEC` are STALL timers, not
  total-trip budgets: they reset on a new closest approach, so a
  long-but-progressing leg completes while an unreachable target still
  gives up. Don't restore the from-`issuedAt`/`startedAt` shape — it
  capped ordered retrieval at ~21 tiles and ordered moves at ~42. Since
  #1291 they are also spent in ELIGIBLE time only
  (`scripts/unit_ai_stall.lua`, which owns both the accounting and
  `maintainTask`; `unit_ai_core` re-exports it): an interval another
  action won (the #306 ladder's eating/drinking/refill/combat/
  `treat_ally`, or a `forage` that walks the unit AWAY) or one the AI
  never ticked through at all (collapse, an engine animation, a mental
  break, a load boundary — seen here as a gap longer than
  `MAX_CHARGED_INTERVAL`) costs a pending order nothing, however long
  it lasts. The budget still ACCUMULATES across interruptions rather
  than restarting after one, so eligible non-progress still expires on
  schedule and no order becomes immortal. That state
  (`stalledFor`/`stallSeenAt` on the order) rides `lua.unit_ai` v5; a
  v1–v4 order carries the old absolute `progressAt` instead and is
  seeded from it on its first tick, so it expires exactly when it
  would have. Gates: `expedition_retrieval_probe.py` (manual-only),
  hspec `--match "commanded order stall budget"`.
- **The expedition loop (#923)** — the arc's shipped slice is
  **prepare → travel → discover → extract → return → invest**, run as
  ONE session by `tools/expedition_loop_probe.py` (manual-only,
  fixed-seed, ~15 min, two engine boots). `docs/expedition_gameplay_loop.md`
  remains the design authority; step 9's original combat encounter and
  guaranteed progression reward are deliberately deferred (#916/#917),
  so "invest" here means the recovered loot is banked in colony storage
  and is afterwards ordinary colony stock — not a completed project.
  Contracts the gate pins, and which new expedition work must not break:
  the colony comes from a real `acolyte_portal` and its OWN roster
  (`scripts/building_spawn.lua`), never hand-spawned units; the expected
  end lifecycle is `discovered` with contents spawned exactly once
  (nothing in the game drives an instance further — a gate that called
  `world.setLocationLifecycle` would be asserting its own writes); the
  extraction target is whichever def the ruin's own loot rolls produced,
  never a staged item; and every durable identity is re-checked in a
  FRESH PROCESS — `(page, instance id)` lifecycle, per-unit
  `knownLocations`, the exact completed objective-ID set, and the
  recovered item's instance id / definition / mutable properties /
  storage ownership. The gate also runs an **unprepared control**: a
  second traveller sharing ONE identical leg with the first — mustered
  to a single staging tile and held there by the PAUSE, then same verb,
  same destination, same paused window, same seeded hunger deficit —
  measured once BOTH are
  inside the ruin's halo, differing only in FOOD (the canteen is left
  full on both: a dry one puts `refill_canteen` at its 7.5 peak, above
  `follow_command`, and the control then abandons the leg to walk to
  the water the scout radioed about — a behavioural difference, not the
  supply being measured), and which must end
  measurably worse off. That is what makes the scenario prove
  preparation matters rather than prove a walk succeeds. Six conditions
  keep the comparison honest, and weakening any one of them quietly
  turns the control into theatre: `find_water` is retired and
  `forage_max_fraction` disabled for the session (#94's emergency ladder
  has its own gate, `foraging_probe.py`); BOTH travellers are shed to
  inside their carrying capacity first (an over-encumbered acolyte
  crawls, its order stall-times-out and it never arrives —
  `docs/expedition_survival_calibration.md` E1); the control is given NO
  retrieval target of its own, because a ruin can roll food and a
  control that eats what it finds destroys the measurement; the travel
  VERB matches, since `commandMove` walks at `movement_speed.ordered` =
  comfort × 1.15 while `pickup_ground` walks at comfort (so the
  retrieval order is issued only after the measurement); the ORIGINS are
  equalised as a PLACE and not merely a distance, because hunger drains
  with time on the road and route shape is time — a radial band is
  satisfied anywhere on a circle, so the check asserts separation as
  well as distance spread, verified with the SIMULATION STOPPED (a
  completed move order does not hold position, E3, and
  **`unit.setFrozen` is not a hold at all**: `uiFrozen` only makes
  `publishToRender` skip the sim-derived update, so a "frozen" unit
  keeps walking while `unit.getInfo` reports where it was when the flag
  went up — use `engine.setPaused` when you need a unit to actually
  stay put, and re-read positions after pausing); and the observation
  point is both
  travellers inside the halo in ONE COHERENT SNAPSHOT — a single paired
  read revalidated with the simulation STOPPED, since two separate
  `unit.getInfo` round trips let the sim run in between and a pair that
  was never inside together can satisfy them, and since a unit that
  finishes its move reverts to wander and can drift back out while the
  other is still walking. The eating itself is
  watched live as a real `eat_from_inventory` action, so the delta is
  attributed to a mechanism rather than inferred from a number two
  differently-massed acolytes could reach by other routes. The gated
  metric is FOOD (stomach fraction), matching what
  `docs/expedition_survival_calibration.md` measured actually goes live
  on a trip this length; water is reported as evidence, not gated.
  **Don't "fix" that by seeding a thirst deficit** — `scripts/salts.lua`
  derives blood salt concentration as saltFrac/hydrationFrac and
  `scripts/brain.lua` folds it straight into consciousness, so a unit
  dehydrated far enough to prefer drinking over its orders is knocked
  unconscious by the electrolyte imbalance, and scaling the `salt` pool
  down to compensate just moves the blackout to the first meal's salt
  bolus (`salts.mealSalt` restores 0.30 of max_salt per feed). Both were
  observed live while building the gate.
- **Testing blood decals headless (#603 epic, #604/#606/#607/#788/#882/#883)**
  — full architecture record: `docs/blood_decals.md`. Five hspec
  groups (`test-headless/Test/Headless/Blood/`), each independently
  targetable via `--match`: `Blood.Types` (texture-pool/decal-store FIFO
  + matching), `Blood.Texture` (deterministic pixel generation),
  `Blood.Impact` (wound → one-shot mark mapping), `Blood.Trail`
  (ongoing-bleeding gating/conservation/partition-invariance math —
  includes `Blood.Pool` coverage: stationary pooling arbitration,
  layer bound, placement), and `Blood.Teardown` (GPU-dispose queue
  plumbing, no device). Turnkey probes: `blood_decal_probe.py`
  (texture reuse/eviction/render), `blood_impact_probe.py` (wound-to-
  mark mapping), `bleeding_trail_probe.py` (#882/#883 moving trail +
  stationary pooling), and the needs-GPU
  `blood_gpu_lifecycle_probe.py` (#788 upload/dispose against a real
  device — manual-only). **Transience contract**: blood is transient
  BY DESIGN — `wsBloodStoreRef` and every unit's
  `Unit.Types.Trail.TrailState` are deliberately never persisted, and
  a loaded session always starts with no decals and no active
  trail/pool accumulators, even one that was saved with plenty of
  both (closed issue #884 is the specification for reversing this,
  should it ever be revisited). A test asserting a mark or an
  accumulator survives a save/load round trip is testing for behavior
  this engine deliberately does not have.
- **Logging streams** — event log: `engine.getEventLog()`, emit via
  `engine.emitEvent(cat,text)` / `emitEventAt` /
  `emitEventForUnit(cat,text,uid[,gx,gy])`; a category lands only if
  its notifications YAML has `log: true`. Combat:
  `combat.drainEvents()`. Injury (NON-combat only — falls, hazards,
  wound deaths): `injury.drainEvents()`. These are DRAINED streams —
  don't drain manually in a test while the panel script is loaded, or
  you'll race it. Gate: `injury_log_probe.py`.
- **Autosave (#913)** — OFF by default (`config/save_default.yaml`,
  overlaid key-by-key with `config/save.local.yaml`; Settings → General
  edits it). `scripts/autosave.lua` owns the WALL-CLOCK interval and
  fires only when `uiManager.isGameplayView()` is true — a deadline
  reached in a menu / with no world / during a save or load is SKIPPED
  silently (no request, no failure event, nothing queued), and menus
  never suspend or reset the cadence. Slots are the reserved
  `autosave-<n>` family, `autosave-1` newest; ownership is the durable
  `smAutosave` metadata flag (`"metadata"` component v2, v1 payloads
  migrate to manual via `World.Save.Compat.MetadataV1`), NEVER the
  name — a manual save (directory OR pre-#762 flat file) squatting on
  one of those names fails the attempt through `save_load` with nothing
  rotated. PUBLISH FIRST, ROTATE SECOND: every autosave is written to
  the reserved `autosave-incoming` staging slot and the family only
  ages down once that transaction reports success, so a failed
  autosave can never have discarded or renumbered a generation; a
  staged generation left by a crash is rotated in by the next cycle. The
  rotation is ordered the same way — the oldest is RETIRED by rename and
  only deleted once every other move succeeded — so an interrupted
  rotation leaves a partially shifted family, never a shorter one. The
  shift plan is DERIVED from what's on disk (retire only when the family
  is genuinely full; then walk down from the first free index), which is
  what makes a resumed rotation land the interrupted generations where
  they belong instead of ageing a second one out. A SUCCESSFUL
  autosave restores the pre-request pause + visible time scale, but
  only if `playerIntentGenRef` still matches — an `MVar` that doubles
  as the mutex, so the comparison and the writes are one critical
  section with those verbs: any `engine.setPaused` /
  `world.setTimeScale` during the window means the player wins. A
  FAILED one stays paused and zero-scaled (the existing ratchet — the
  acceptance step zeroes the visible clock too, so a failure BEFORE the
  world thread's own capture can't leave a half-paused world), every
  terminal failure of an accepted save reports through `save_load`, and
  the success event's own pause (if the category is configured for it)
  is authoritative over the restore. Gate: `autosave_probe.py`
  (manual-only).
- **Config state (#638/#786)** — settings save to gitignored
  `config/*.local.yaml`; boot falls back to tracked `*_default.yaml`
  (notifications self-materializes from
  `data/notification_categories.yaml`; `save` (#913) resolves as an
  explicit KEY-LEVEL overlay instead — a sparse local file keeps every
  tracked default it doesn't mention). The tracked legacy
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
# for phase == "LoadPublished" (or "LoadFailed", or #1204's
# "LoadReconciliationFailed") before touching anything.
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

**Autosave (#913):** interval autosaves ride the SAME transaction —
they only add a request-time `AutosaveRequest` (pre-request pause,
visible time scale, player-intent generation) plus the durable
`smAutosave` classification `engine.listSaves()` exposes. See the
subsystem table above for the full contract.

**Enum schema policy:** `Direction`, `Pose`, `UnitActivity` (and any
enum serialized via `Generic Serialize`) are positional by constructor
tag — **append-only**. Inserting/reordering silently corrupts saves;
anything beyond appending requires a `currentSaveVersion` bump. A
constructor's own FIELDS are positional too, so reordering them or
changing one field's serialized type corrupts saves the same way while
moving no tag (#1270).

Enforced mechanically since #1145 by `tools/enum_append_only_audit.py`
(in CI and `make ci`, with its own `--self-test`), which is the
authority on which types are guarded and why — read its module
docstring before adding, moving, or changing one. It guards **every**
`data` declaration under `src/`/`app/` that derives `Serialize` through
`Generic` and has two or more constructors — a deliberate superset of
"reachable from a save component", currently 37 types (32 of them
reachable from a save-wire DTO today), so a type that becomes persisted
later was already guarded the day its instance was derived.
`docs/save_compat/enum_baseline.json` is the golden constructor list —
module-qualified, each constructor recording its name and its ordered
PAYLOAD signature (`arity` is that payload's length) — plus the
save-wire attribution captured with it; it is GENERATED end to end, so
don't hand-edit it — a pure append ratchets it with `--update-baseline`,
and anything else is a wire-format break the audit refuses to record. A
payload slot is the field's declared type, normalized (strictness
markers, `{-# UNPACK #-}`, layout, `::`/`∷` and the parentheses a `!`
forces are all erased; field order and type structure are not), with the
selector kept for a record alternative — which is what makes swapping
two same-typed record fields visible, and means a selector rename
reports too. An incompatible change's output names every component and
historical shape that carries the type, with the reachability path —
including for a type that was renamed or deleted, which is read back
from the recorded attribution because there is nothing left to walk.
Since #1270 this audit is the one exhaustive gate owning payload drift
inside a multi-constructor sum; single-constructor record field order
stays the frozen-DTO boundary's and `tools/save_compat_audit.py`'s.

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
  `power-nodes`, `texture-palette`, `metadata`, the two OPTIONAL
  `container-knowledge` (#1087) and `transfer-orders` (#1246), plus
  dynamic `lua.<module>` components). Registry:
  `World.Save.Component.saveComponentRegistry`. Every gameplay
  component is REQUIRED except those two, each of whose absence has an
  honest default ("no container has ever been inspected"; "no transfer
  order is queued") — see `docs/persistence_contract.md` §5 before
  declaring a third one.
  Component evolution =
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
  lifecycle, plus #1204's 13th terminal phase
  `LoadReconciliationFailed`: publication SUCCEEDED but a Lua
  `onSaveLoaded` callback raised, so the live session is incompletely
  reconciled. It is a THIRD terminal disposition, not a flavour of
  either existing one — every poller must treat it as terminal (its
  outcome is non-nil, so `loadInProgress` is already false) and as
  UNSUCCESSFUL, and it deliberately leaves `failedAtPhase` unset
  because that field's presence promises the old session survived
  unchanged, which a post-publish failure cannot. The outcome
  aggregates every failing module; `reconciliationFailures` carries the
  per-module `{module, error}` breakdown. Callback isolation is
  unchanged — the broadcast still attempts every module.
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

## Unit asset inventory

`python3 tools/pack_atlas.py --validate-only --strict` is the
authoritative, enforceable inventory of unit ANIMATION art (#1257).
Discovery is **filesystem-first**: it walks every PNG under
`assets/textures/units/<unit>/animations/<animation>/<direction>/` and
checks the declarations against it, never the other way round — the
YAML-first version it replaced simply never looked at the three asset
trees that had no YAML. The current corpus is 7 unit trees, 116
animations, 4,620 frames, and strict validation exits 0 with zero
warnings. **Every committed animation PNG is owned by exactly one
animation-frame declaration; there is no directory or glob exemption
mechanism.** Adding an undeclared frame fails the gate.

**Two declaration forms live under `data/units/`,** and which top-level
key an entry sits under is the entire runtime distinction:

- `units:` — a gameplay unit. `Engine.Asset.YamlUnits.loadUnitYaml`
  returns these, so they register, load textures, list, and spawn.
  `name` and `sprite` are mandatory.
- `asset_units:` — an ASSET-ONLY unit: `tiller`, `unknown_unit`,
  `white_tailed_deer`. Declares exactly `name` + `animations` — a
  WHITELIST, so an unknown key fails as surely as a gameplay one, and
  BOTH decoders enforce it (Aeson ignores keys a parser doesn't ask for,
  so `UnitYamlAssetDef` checks the key set explicitly; a silently
  accepted `sprite:` would decode fine and then be skipped by
  `loadUnitYaml`, looking exactly like a unit that failed to register). `loadUnitYaml` never
  returns one — `loadUnitYamlAssets` does — so nothing registers,
  textures, lists, or spawns it. `unknown_unit`'s hard-coded
  missing-texture fallback (`Engine.Scripting.Lua.API.Units.List`) is
  untouched by its declaration. Promotion to a runtime definition is
  **#1261's** decision, deliberately not this phase's.

A file may hold either key or both; a file holding NEITHER is refused
rather than decoded as zero units (that is what a mistyped top-level key
looks like), and so is a key present with an explicit `null` — aeson's
`.:?` reads that as absent, so the engine's own decoder refuses the
file, and accepting it in the gate would leave CI green while startup
logged a parse failure. Three decoders share the shape:
`Engine.Asset.YamlUnits.UnitYamlFile`, `Engine.Preview.Unit`'s
`UnitAnimMetaFile` (which reads both, since the preview never
registers anything), and `tools/pack_atlas.py`. Animation and direction
keys are strings, never coerced — YAML resolves an unquoted `123:` to an
int whose `str()` would look like a valid identifier.

Enforced invariants — a unit identifier is one lowercase `[a-z0-9_]+`
path component; an animation identifier is the same, plus ONE narrowly
matched approved exception, `<lowercase>_RH_<lowercase>`, for the
documented asymmetric-weapon animations (so `attack_heavy_RH_dagger`
passes while `AnyThing`, `attack_heavy_RH_Dagger` and `attack_LH_dagger`
do not); frames are `frame_NNN.png` with exactly three digits, so
`frame_1.png` and `frame_0002.png` are rejected rather than read as
another spelling of an index; a declared path is
relative, `..`-free, symlink-free, and resolves inside its EXACT
`<unit>/animations/<animation>/<direction>/` directory, so cross-unit,
cross-animation and cross-direction references are each named as such;
`flip: true` declares exactly the canonical five authored directions and
`flip: false` exactly all eight; per direction, indices start at 0,
ASCEND in the order they are declared (playback walks the declared list,
so a contiguous-but-shuffled list plays out of sequence while every
set-based check still passes), and have no gaps or duplicates, while
different directions of one animation may hold different counts; `fps`
is a positive number that survives the engine's 32-bit `Float`, and
`loop` a boolean, rejected rather than coerced when they are not. The
`fps` guards stack because a positivity test alone is not enough:
PyYAML resolves `.nan`/`.inf` to real floats (`nan <= 0` is False like
every NaN comparison, and infinity really is greater); a Python int has
unbounded precision, so a thousand-digit `fps:` is valid YAML that makes
`math.isfinite` RAISE rather than answer; and `1.0e+100`/`1.0e-100` fit
a 64-bit double but land in `UnitYamlAnim`'s single-precision field as
infinity and zero. No symlink may appear anywhere in the walk — unit
directory, `animations/` root, animation directory, direction directory,
or frame — so nothing can be linked past the inventory: a symlinked
entry is an ERROR, never a skipped one, or a linked tree would evade the
inventory while its frames still ship. A `--unit` naming neither a
declaration nor an asset tree exits non-zero rather than reporting a
clean run of an empty inventory.

**"Duplicate" means duplicate ANIMATION-FRAME claims only.** Reusing an
animation frame as a unit's `sprite`, a `directional_sprites` entry, or
its `portrait` is deliberately legal (20 shipped references do this) and
is never reported.

**The INVENTORY gate validates paths and structure, never file
CONTENTS.** For a unit with no compiled index it establishes that each
declared frame exists and is a regular file and asserts nothing about
what is inside it — not that it decodes, not its pixel dimensions, not
that one animation's frames agree on a size. That boundary is
deliberate: validating a real binary format there is its own work with
its own cost, tracked as **#1311**. The COMPILER below necessarily
decodes, so an animation that is actually compiled does get those
checks.

**Scope is `animations/` — deliberately not the whole unit tree.**
`assets/textures/units/unknown_unit/rotations/*.png` and the per-unit
`portrait.png` files are referenced from hard-coded Haskell or from
non-animation YAML fields; they are outside this inventory.

**Deleting art needs the owner's explicit confirmation** (`#1257` R4):
present an exact path-level classification first. #1257 itself deleted
nothing — all 695 previously-unowned paths were retained and declared.

## Unit animation atlas compiler

`python3 tools/pack_atlas.py --compile [--unit <name>]` is the same
tool's other half (#1258, `docs/texture_infrastructure.md` TEX-2). It
compiles the validated declarations into DERIVED artifacts; source PNG
frames stay the editable artwork (D-1) and unit YAML stays the only
hand-edited semantic authority (D-11). Runtime sampling is TEX-3's
(#1259, below) and KTX2 encoding TEX-5's (which does not exist yet).

Output is **one atlas per ANIMATION** (D-2),
`assets/textures/units/<unit>/atlas/<animation>.png`, beside a generated
`assets/textures/units/<unit>/atlas/index.json`. That directory is a
SIBLING of `animations/`, which is what keeps generated artifacts
outside the filesystem-first inventory walk.

- **Rows** are the AUTHORED directions in `ATLAS_DIRECTION_ORDER` — the
  engine's own `Unit.Direction` order `S, SW, W, NW, N, NE, E, SE` — so
  `flip: true` yields five rows and `flip: false` eight (D-4). Each row
  index is nevertheless recorded explicitly, so nothing downstream
  re-derives the order.
- **Columns** are the animation's maximum authored frame count.
  Unequal per-direction lengths are real (D-5): the index records each
  direction's TRUE count, shorter rows are rectangularized with
  transparent RGBA8 zero cells, and no padding cell is addressable as a
  frame — `frame_count` is the sole frame authority.
- **Cells are exact integers**: frame `c` of row `r` is at
  `(c * cell_width, r * cell_height)`. Every frame of one animation must
  decode to that same size; a mismatch is a compile error, never an
  implicit rescale (D-6 — nothing here resamples or blends). Each cell
  is a byte-for-byte copy of its source frame's canonical decoded RGBA8
  samples, alpha included — decoded SAMPLES, not PNG file bytes.
- **The index** carries a `schema_version` (the format TEX-3 parses)
  separately from `tool_version` (this compiler's revision), a
  documented `direction_order`, and per animation its storage format
  and path, atlas/cell dimensions, columns, rows, per-direction row and
  frame count, `flip`/`fps`/`loop` as the engine will hold them (`fps`
  narrowed to 32-bit), and two `sha256` digests: a PER-ANIMATION
  `source_digest` over that animation's own declarations and decoded
  pixels, and an `atlas_digest` over the atlas's decoded CONTENT rather
  than its file bytes. Per-animation is the point — one animation's
  edit must not invalidate an unrelated atlas (D-12). Nobody hand-edits
  this file.
- **Determinism and locality.** A clean rebuild under an unchanged
  toolchain is byte-identical. An incremental run compares each
  artifact against what it would generate and writes only on a real
  difference, so an animation edit rewrites that atlas and its unit
  index and nothing else. An mtime-only touch changes nothing — the
  digest is over content. Obsolete atlases are removed from the unit's
  own `atlas/` directory and nowhere else.
- **`--validate-only` is index-aware.** A unit with NO index is valid
  (every shipped unit is, until TEX-4 begins production tracking).
  Where one exists it is REGENERATED from the sources and compared, so
  a stale digest, a hand-edited or non-canonically serialized index, a
  missing indexed atlas and tampered pixels all report — and a tampered
  index cannot certify a tampered atlas, because the comparison is
  against a fresh regeneration rather than the numbers the file carries
  about itself. `--compile --check` reports what is out of date and
  writes nothing. Compilation refuses outright on an inventory that
  does not validate.
- **No production atlases are committed yet** (#1258 requirement 7).
  D-12's tracking gate is the acolyte pilot, TEX-4.

**Dependencies are pinned in `tools/requirements-assets.txt`** (PyYAML +
Pillow), which `.github/ci/Dockerfile` installs verbatim — the pins are
spelled in the Dockerfile rather than COPYed because the image tag is
that file's own hash, and `test_pack_atlas.py` fails if the two drift.
Pillow is imported lazily, so the inventory gate on an index-free
corpus still runs without it. Validation never needs the exact pinned
toolchain: every recorded digest is over decoded RGBA8, so any Pillow
verifies a committed atlas.

Gates: `python3 tools/test_pack_atlas.py` (fixture-based, isolated temp
trees, never touching the shipped assets; every negative case asserts a
nonzero exit AND a diagnostic naming the real problem, a rule that
tightens gets a positive case pinning the other direction, and each
compiler SCENARIO asserts on real emitted pixels, the index document,
or which files a second run actually wrote) and the
strict run above. Both run unconditionally in `make ci`
and post-merge master CI, and path-selectively on PRs via
`tools/ci_expensive_gates.py --gate unit-assets`. hspec:
`--match "Asset.UnitInventory"`.

## Unit animation atlas runtime

The engine can load and sample those compiled artifacts (#1259,
`docs/texture_infrastructure.md` TEX-3). **No shipped unit uses it yet**
— production migration is TEX-4 (#1260) — so on today's asset tree every
animation still loads its per-frame textures and nothing about loading
changed.

**Storage is a SUM, so no animation is half-migrated.**
`Unit.Types.Def.Animation` carries an `aStorage` of
`Unit.Atlas.Types.AnimStorage`: `StorageLegacy` (one texture handle per
frame, each its own whole image) or `StorageAtlas` (one compiled image
per animation, one handle, one bindless slot; each frame a UV cell).
D-10's "exactly one resident representation" and requirement 6's "never
mixed within one animation" are then unrepresentable rather than merely
enforced. Read frames through the module's storage-neutral accessors —
`storageFrameCount` / `storageFrameCounts` / `storageMaxFrameCount` /
`storageSampleAt` — never by matching the constructor. Buildings reuse
the same `Animation` and are never compiled, so `Building.Render` goes
through `storageLegacyFrames`, which answers `Nothing` for an atlas.

**Mode selection is the compiled index, and failure is failure.** A
unit's `atlas/index.json` is the explicit declaration of which
animations are atlas-backed; an animation it does not name loads legacy.
`Unit.Atlas.Load.loadUnitAtlasIndex` reads, parses, decodes, and
verifies EVERY declared atlas before `loadUnitYaml` allocates one handle
or queues one upload, and `Unit.Atlas.Index.planUnitAtlasStorage` then
adds the YAML-staleness half. A missing, stale, unsupported, or
malformed index does NOT fall back to legacy frames: the whole unit
definition is refused, with the unit, animation, and artifact named. No
partial registration. Only an ABSENT `atlas/` directory means legacy —
a directory that exists WITHOUT its index is an incomplete compiled
artifact and rejects, since falling back there would serve the per-frame
path while compiled PNGs sit beside it.

Validation runs in three passes, cheapest first, stopping at the first
failure. **(1)** The index parses and is structurally sound: supported
`schema_version` and `digest_algorithm`, the unit's own identity,
duplicate animation names, containment of `atlas_path` inside that
unit's `atlas/` directory, positive geometry, every reachable cell lying
inside the sheet, unique and in-range direction rows, real frame counts
bounded by row capacity, a positive finite `fps`. **(2)** It still
describes what the unit YAML declares: animation set, `fps`/`loop`/
`flip`, direction set, per-direction frame counts, columns. **(3)** Each
atlas decodes to the image the index describes (dimensions plus
`atlas_digest` over decoded RGBA8), AND every declared SOURCE frame
decodes to exactly the pixels its atlas cell holds.

Pass 3 is what catches a source PNG repainted while its compiled atlas
and index were left in place — the atlas stays internally consistent and
its own digest still matches, so nothing short of reading the source art
sees it. Passes 2 and 3 together verify every input the compiler's
`source_digest` is taken over, **directly rather than by recomputing the
digest**: that verifies the property the digest is a proxy for, localizes
a failure to one direction and one frame, and avoids reproducing the
compiler's field encoding (including `repr()` of a Python float, whose
decimal formatting diverges from Haskell's at exponent extremes — a
parity bug there would REJECT valid art). Reading source frames at load
is a migration-phase cost: the legacy path is still live and every unit
still ships its frames. TEX-6, which removes source loading, is where
this becomes the compile-time gate's alone.

**`pickFrame` returns a `FrameSample`, and its arithmetic is FROZEN**
(D-3). Every consumer reads the stable handle (#286 — never a slot), the
frame's UV endpoints WITHIN that handle's image, the frame's pixel
dimensions when the storage knows them, and the mirror flag. The only
storage-dependent step is the per-direction frame COUNT, which for an
atlas is the index's REAL count and never the padded column count, so
padding is unreachable by construction (D-5).

**Cell dimensions size everything.** `frameDimensions` is the one funnel:
an atlas sample answers from its cell, a legacy sample falls through to
`rvTextureSizeRef` as it always has. Nothing may measure an atlas
handle's whole-image entry where it means a frame. That includes hit
testing, which since #1259 sizes from the SAME `pickFrame` sample the
renderer draws (`Unit.HitTest.unitHitRect`, shared by click and box
selection) rather than the static T-pose it used before.

**Mirroring reflects across the frame's own sub-rect**, never the whole
image — with atlases, `1-u` lands in a different cell. This is #887's
flip-the-clipped-slice rule generalized: it now governs every sample,
not just the preview's. `UI.Render.renderSpriteBatch` takes the sprite's
source sub-rect (`ussUV`, set from Lua by `UI.setSpriteUV`) and mirrors
as `u' = su0 + su1 - u`; a whole-image sprite is the unchanged `1-u`.
Anything DISPLAYING a unit's live frame must use `unit.getFrameSample`
(handle + UV + flip + cell size), not `unit.getFrameTexture`, which
cannot describe an atlas frame and would draw the whole sheet — and must
publish it with `UI.setSpriteFrame`, which lands texture, sub-rect and
mirror in ONE manager transition. The render thread reads the manager
concurrently, so separate setters leave a window where the new atlas
handle is paired with the previous frame's rect.

Atlas slots are registered PINNED to the nearest sampler with one mip
level (D-6), so a runtime `setTextureFilter` toggle cannot start
bilinearly resampling unit art — which on a sheet would additionally
bleed neighbouring cells across every frame edge. The upload path's
path cache is therefore **policy-aware**: `apAssetPaths` is keyed by
path alone while a slot's sampler was fixed by whichever policy first
uploaded it, so a cache hit is taken only when the canonical texture's
pinned-ness matches the request (`cacheEntryReusable` against
`btsPinned`) and otherwise re-uploads into its own slot. Both directions
matter — an atlas inheriting an ordinary slot would stop being nearest,
and an ordinary texture inheriting a pinned one would be stuck on a
filter it never asked for. Cell UVs sit on exact
cell EDGES with no half-texel inset: unit art is nearest and pixel-
snapped, so a fragment centre lands inside its cell, and an inset would
shift the sampled texels and break pixel-identity with the legacy path.

Gates: hspec `--match "pickFrame"` (the whole logical-choice matrix run
against BOTH storage modes from one table) and `--match "Unit.Atlas"`
(index parsing/validation, the digest against `pack_atlas.py`'s own
reference values, mode selection, and the real consumer geometry —
`unitToQuad`'s vertices, `unitHitRect`, `renderSpriteBatch`, a
texel-level comparison of an atlas cell against its legacy frame with
the mirrored case included, the pinned-nearest survival of a global
filter toggle through `planFilterRebind`, the cache's policy awareness,
and a real on-disk fixture tree driven through `loadUnitAtlasIndexIn`).
`--match "the real unit registration boundary"` drives
`registerUnitDefs` — what `loadUnitYamlFn` delegates to — against a live
headless engine, a real asset pool and a real Lua→engine queue, and
asserts on the messages actually queued and the definitions actually
published: one atlas upload per selected animation with its own handle,
no per-frame textures for an atlas-backed one, an unselected animation
still legacy, and a rejected index queueing nothing and publishing
nothing.

## AI Asset Generation

**Art is tracked work, exactly like code.** A texture, icon, sprite, or
animation that does not exist yet is a first-class blocker, not a detail to
route around: it gets its own issue and its own PR, and the project owner signs
off on every texture before it lands. An issue whose work needs missing art
NAMES it as a blocker in the body rather than resolving it in advance, so the
solver hits it deliberately.

**Stopping is the default.** When you reach an art blocker, STOP and return to
the owner with the exact list of missing assets and what each is for. They will
either supply the file themselves or direct you to generate it via PixelLab.
Unless the owner has ALREADY stated which method they want for that specific
asset, assume neither — assume they want to stop and think about it. Never
choose between those yourself, never ship a `wtNoTexture` or reused-sprite
placeholder as if the work were done, and never quietly narrow a slice to avoid
the art.

Textures (flora, units, buildings, tiles) can be generated via the PixelLab MCP server.
**Read `docs/asset_generation.md` before generating** — it has the validated pipelines
(skeleton-freeze masks for multi-stage flora, character/state/animation flow for units),
the raw v2 API parameters the MCP tools hide, and the gotchas that waste hours if rediscovered
(soft freezes, broken `color_image`, base64-in-shell corruption, real ETAs).

## Platform Notes

- Tested primarily on macOS; works on Linux with minor adjustments
- macOS: GLFW produces unavoidable junk on stdout
- macOS builds get `-DDARWIN` cpp flag and address sanitizer in dev mode
