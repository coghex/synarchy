# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

This file carries the rules that prevent damage: what you must not undo,
and which gate proves it. The layer below — the as-built mechanics behind
those rules — lives in
[`docs/engine_contracts.md`](docs/engine_contracts.md), which the sections
here point at by name. Read the relevant section there before changing
code in the area it covers; every contract in it is enforced by the gate
its entry names, which is why it could move out of the always-loaded file.

Two trims have shrunk this file, both archived verbatim: deep per-issue
history (review-round narratives, verification stories) on 2026-07-23
(`docs/history/claude_md_2026-07-23_pretrim.md`), and the 2026-08-18 pass
that removed verbosity and extracted `docs/engine_contracts.md`
(`docs/history/claude_md_2026-08-18_pretrim.md`). Consult those snapshots,
git history, or the referenced issues/PRs when you need the full story
behind a contract stated tersely here.

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
   probe for the affected subsystem when one exists. Run each of these
   ONLY when its own inputs changed:

   | Gate | Run it when you changed |
   |---|---|
   | `world_check.py --quick` | worldgen output |
   | persistence inventory audit | its root owners/registry or inventory docs |
   | EngineEnv capability audit | `EngineEnv`'s field set or `docs/engineenv_capability_inventory.md` |
   | a module-budget guard | a capped module |
   | `test_audit.py` | `world_audit.py` / `world_check.py` |
   | `test_run_probes.py` (~15 s, GPU-free) | `run_probes.py` |
   | `test_persistence_contract_sweep.py` (pure, no engine, <1 s) | `persistence_contract_sweep.py`'s `SELECTABLE_CROSS_REFERENCED_PROBE_KEYS` or `run_probes.PROBES` |
   | `findings_report_audit.py` | a findings report |
   | unit-asset gate (`test_pack_atlas.py` + `pack_atlas.py --validate-only --strict`, ~2 s) | `assets/textures/units/` (source frames or generated `atlas/`), `data/units/`, `tools/unit_texture_budget.json`, `src/Unit/Atlas/`, or the unit-YAML / preview / registration decoders |

   Do NOT run the whole headless suite, the 21-seed world check, or
   `make ci` by default — CI is the full-suite authority.
3. **Worldgen-OUTPUT changes only (full tier).**
   `SYNARCHY_FULL_TESTS=1 cabal test synarchy-test-headless` (+~25 s),
   then re-capture baselines `python3 tools/world_baseline.py` (~7 min)
   and re-run world_check. Remember the save-version bump.
4. **Behavior probes — opt-in, not a default gate.** ~85 headless
   `tools/*_probe.py` scripts each boot a real engine and gate one
   system — see `tools/README.md` and **Subsystem probes & domain
   contracts** below. Run the ones relevant to what you touched, or
   `python3 tools/run_probes.py
   --only <substrings> [--jobs N]` (bare run = full sweep, tens of
   minutes). `python3 tools/ci_probes.py --status` is the authoritative
   list of every probe's CI eligibility — never trust a prose list of
   probe names. The path→probe map for CI's blocking, path-selective PR
   probe gate lives in `tools/ci_probes.py` (a change there re-runs its
   `--self-test`); promoting a probe to the gate = move its key from
   `MANUAL_ONLY_REASONS` to `CI_ELIGIBLE` after proving it
   deterministic, broad, and cheap.

**Module-budget scope:** the 500-line Haskell/Lua limits are per-split
ratchets, enforced only for module families explicitly listed in the
relevant budget tool. They are not a tree-wide size policy. For a
structural split with no explicit budget entry, extract the cohesive,
correctness-relevant boundary first even if the facade remains above 500
lines; record a later pass rather than forcing unrelated
`EngineEnv`/capability refactoring just to hit 500.

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
`Engine.Core.Monad` defines `EngineM σ α` — a continuation-passing-style
monad with a concrete `EngineEnv` Reader environment, concrete
`EngineState` mutable state, IO, error handling, and logging. Its two type
parameters are `σ` (the continuation result) and `α` (the value); neither
the environment nor the state is a parameter. Most engine code runs in
this monad.

`Engine.Core.State`'s `EngineEnv` is one shared record (87 fields)
reachable from any thread. The capability-split epic (#537/#889–#899) that
narrowed it is **complete**.
[`docs/engineenv_capability_inventory.md`](docs/engineenv_capability_inventory.md)
(#876) is the authoritative capability/thread/lifecycle ownership
inventory for every field, and `tools/engine_env_capability_audit.py` (CI
+ `make ci`) fails if a classification drifts from the live record.

**Before adding any state, read that doc's §6.4 post-flip procedure** —
it leads with the case that resolves most of them: the state doesn't
belong on `EngineEnv` at all (`WorldState`, a manager, `EngineState`, or
a local), and needs no new field. Before adding a capability record, read
§2.1's canonical convention block rather than inferring the shape from an
existing one — it is the one authoritative statement of the
naming/placement, one-way-projection, shared-live-container,
no-back-import, no-record-ahead-of-need and thread-private-split rules.
Currently **eight capability identifiers and thirteen record/view types**
(§2.1's table): five capabilities are split, four by the thread-private
rule (§3.1).

Each capability lives in its own `Engine.Core.Capability.<Name>` module
exporting one `<Name>Capability` record plus a total `to<Name>Capability
∷ EngineEnv → <Name>Capability` projection. `EngineM` stays hard-wired to
`MonadReader EngineEnv` (no capability typeclass layer), so a narrowed
module's public API is typically two layers: primitives taking the
capability explicitly, plus thin `MonadReader EngineEnv` wrappers
preserving existing call sites (see
`Engine.Core.Log.Monad`/`Engine.Core.Capability.Core`). Narrowing the
*module's own field access* is the goal, not rewriting every caller.

**Full access is a closed allowlist.** The same audit treats importing
`Engine.Core.State` with `EngineEnv(..)` or as a bare import (either
shape, regardless of `qualified`/`as`/multiline) as unrestricted access,
production-only (`src/`+`app/`; `test/` exempt). Since #899 that is
allowed **only** for §6.1's hard-coded permanent allowlist — the 24
genuine whole-session orchestration boundaries (the definer and
constructor, the monad carrier, per-profile boot wire-up, the main loop,
Lua dispatch, the save/load transaction). §6.2's temporary ceiling is
**empty** and shrink-only, so "add the field now, narrow it later" no
longer exists: a module gaining unrestricted access fails the audit even
if §6.2 is edited to document it. The audit also parses §6.1 and requires
its documented set to equal the checked-in
`PERMANENT_DEFINER`/`PERMANENT_IMPORTERS` constants with a real
justification per row, so neither the doc nor the constants can admit a
permanent importer alone. §6.4(c)/(d) govern the two escape hatches (a
ninth capability; a new §6.1 module) — both need explicit maintainer
approval and synchronized doc + constant + self-test changes.

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

**`math.random` is GAMEPLAY's stream (#1330).** A Lua state has exactly
one, and eleven modules draw from it (AI cadence, thoughts, mental
state, wildlife, sleep, water scanning, location rolls). Its entropy is
established once per state by `Lua.openlibs` in
`Engine.Scripting.Lua.Thread.createLuaBackendState`, before
`scripts/init.lua` loads — so **nothing under `scripts/` may call
`math.randomseed`**: reseeding replaces per-state entropy (clock AND
state address) with the caller's choice, and two engines launched in
the same second then share one simulation. `scripts/ui/randbox.lua` did
exactly that, and also spent eight gameplay draws per suggested world
seed, so clicking randomize shifted every later simulation decision.
Non-gameplay code that needs random values keeps its OWN stream —
`scripts/ui/random.lua` (SplitMix64, seeded from the same time+address
recipe Lua's own auto-seed uses) is the UI widget kit's. Gate: hspec
`--match "random stream ownership"`, which pairs behavioural isolation
and per-instance-entropy cases with the two source guards.

### UI system
`UI.*` handles focus management, text input, and UI rendering; layout
and behavior are driven from Lua. Regression suites:
`Test.Headless.UI.*` (InputOwnership, Clipping, PopupPlacement,
InteractiveBounds, ResponsiveMenus, ResponsiveGameplay). Contracts:

**Text coordinates:** `UI.TextBuffer`/`UI.getCursor`/`UI.setCursor` use
zero-based Unicode code-point offsets. Lua strings are UTF-8 byte
arrays — editable widgets must use `scripts/ui/utf8_safe.lua`, never
`#text` or byte-based `string.sub`. The debug console's own input line
(`scripts/shell.lua`) holds the same contract, including the derived
contract: `cursorPos` and `inputScrollOffset` are code-point offsets into
`inputBuffer`, and that includes the derived paths — tab completion
(`longestCommonPrefix` snaps its byte-wise
agreement point back to a character boundary, since two candidates can
agree on part of one emoji), the ghost hint, and the scroll/measure walk.
The Delete key arrives as `onTextDelete` (`LuaTextDelete`), not
`onDelete`. `config/shell_history.txt` is the one buffer ingress that
isn't engine-delivered text, so a line that isn't valid UTF-8 is dropped
at load. Gate: hspec `--match "Lua.ShellInput"`.

**Text display (#1159):** the same rule binds read-only DISPLAY paths —
wrapping, truncation, and any other per-character walk advances one code
point at a time, or non-ASCII text renders as mojibake and gets measured
once per byte. Lua PATTERNS are byte-oriented too, so `gmatch(".")` is a
byte loop. Pixel-width wrapping goes through `scripts/ui/text_wrap.lua` —
`byCharacter` (the debug console) and `byWord` (all three log panels) —
rather than a fourth private copy. Unlike `utf8_safe`, it never raises on
malformed UTF-8 and never drops a byte. Gate: hspec
`--match "Lua.TextWrapping"`.

**Layers + modal boundary (#742):** pages live on six `UILayer`s,
painted bottom-to-top `LayerHUD < LayerOverlay < LayerMenu < LayerModal
< LayerTooltip < LayerDebug`; `uiLayerBand` is the single paint-order
source of truth shared by hit-testing and rendering. Whether a page
BLOCKS pointer input is the separate per-page `upInputExclusive` flag —
`LayerModal` defaults exclusive, everything else pass-through. The
topmost visible exclusive page owns the modal boundary: input that misses
every control on or above it is consumed (empty modal space blocks).
Stacking-only modal pages opt out via `UI.setPageInputExclusive(page,
false)`. `LayerDebug` is pass-through above any modal.
`UI.isInputBlocked()` reflects the boundary; `ui_manager.lua`'s
`isGameplayInputActive()` folds it in; Escape's dismiss cascade
(`init_keys.lua`) deliberately runs before that gate. Raw handlers that
iterate widget instances outside `routePointer` use
`UI.isPageInScope(pageHandle)`.

**Per-element input policies (#743):** three independent policies —
fires a click callback, blocks pointer (`UI.setPointerBlocking`),
captures scroll (`UI.setScrollCapture`); query via
`UI.isPointerBlocking`/`isScrollCapturing`. A click callback still implies
pointer-blocking by default; a blocking element with no relevant callback
consumes the press (`RouteBlocked`) across all three buttons. Wheel
routing (`routeScroll`) picks the topmost in-scope scroll-capturing
surface via the same `topHitBy` paint-order walk — never the click
machinery.

**Scroll dispatch (#744):** plain and Shift wheel go through the
IDENTICAL pipeline (`Engine.Input.Thread.Scroll`): a capturing element
wins first (`LuaUIScrollEvent`, carrying the Shift flag), else a visible
modal boundary consumes, and only past both does Shift select z-slice vs
camera zoom. Don't reintroduce `UI.isInputBlocked()` self-gates in the
Lua handlers — the engine decides once, upstream.

**Control activation + keyboard focus (#745):** a press on a discrete
control records `UI.ControlActivation.PendingActivation` (firing
`LuaUIPressBeginEvent`); the release re-runs `routePointer` and only
activates if it still resolves to the same element. Interruptions
reverted before release are caught by epochs: global `upmPageEpoch`
(bumped ONLY by `hidePage`/`showPage`) + per-element `ueRouteEpoch`
(bumped by `setVisible`/`setClickable`/detach on THAT element, only on a
real value change); `PendingActivation` snapshots the pressed element's
and every ancestor's epoch and cancels on mismatch. Unrelated
sibling/child churn (hover highlights, focus-ring attach) must never
cancel an activation — that constraint shaped this design; don't
"simplify" it back to a global counter. Sliders/scrollbar thumbs opt out
via `UI.setDragActivation`. Keyboard CONTROL focus (`upmControlFocus`,
`UI.FocusNavigation`) is independent of text focus: Tab/Shift+Tab
traverse in-scope focusables (a modal traps traversal like pointers;
`LayerDebug` stays reachable), Enter/Space fire the real
`LuaUIClickEvent`, arrows step `ueSteppable` controls (`LuaUIStepEvent`);
consumed keys are
withheld from `inpKeyStates`. `UI.getElementInfo`'s `focused` stays
text-only; control focus reports as `controlFocused`.

**Clipping + popup placement (#747):** `UI.setClipChildren(el, true)`
clips DESCENDANTS to the container's live bounds (overflow:hidden; nested
clips intersect; recomputed fresh, nothing cached).
`UI.Clipping.effectiveClip` is the ONE helper both rendering (`clipQuadUV`
— partial quads, not all-or-nothing culling) and hit-testing
(`UI.Manager.Query.isPointInElement`) consult, so paint and hit-test
can't drift. Floating root-mounted content is unaffected — clipping walks
real ancestors only. `UI.placePopup(anchorX, anchorY, anchorW, anchorH,
contentW, contentH, direction)` (`"below"/"above"/"right"/"left"/
"anchored"`) is the one placement algorithm for floating content (pass
the FULL interactive size incl. scrollbar); `UI.fitVisibleRows` backs
oversized-list row reduction. Tooltips keep their own cursor-relative
clamp.

**Interactive bounds (#749):** three rects per element — LOGICAL
(`uePosition`+`ueSize`), VISUAL (overflow-expanded render rect), and
INTERACTIVE (what all hit-testing uses,
`UI.InteractiveBounds.interactiveRect`). A box opts its visible border
into interaction via `UI.setInteractiveOverflow`; overflow alone never
enlarges a target. Overflow is clamped: non-finite → 0, astronomically
large → capped, inverting → zero-extent, non-hittable AND non-rendering.
`UI.getElementInfo` adds `interactiveOverflow` + `interactiveBounds`
(`x/y/width/height` stay content bounds).

**Container window stack (#1238/#1250):** `scripts/cargo_inventory_panel.lua`
is THE container window and owns an ordered STACK of levels, not one
popup. Level 1 is an endpoint (a storage building or a unit); a container
row inside level N opens level N+1, and the nesting PATH is remembered.
Two windows never coexist at one level: opening container B where A is
open REPLACES A and discards every deeper level, and an EXTERNAL request
always targets the base. Only the DEEPEST level is interactive, and
nothing enforces that by hand — a level past the base gets its own
`LayerModal` page, so #742's boundary makes every shallower level
painted-but-unclickable. The base level keeps its non-modal behaviour on
`hud.world_page`. Escape closes ONE level per press.

Four level kinds — `endpoint`, `unitItem` (LIVE), `buildingItem` (the
player's REMEMBERED contents, never a live storage read or a knowledge
write) and `escort` (#1250's Mode A pair). The two item kinds descend by
EXACT INSTANCE IDENTITY, and a path that stops resolving closes that
level AND every level below it rather than retargeting a same-def
sibling. An item-container level is RENDER-ONLY (D-5): inspection only,
so a building row keeps its Retrieve gestures and merely GAINS "Contents"
(the "Withdraw with <unit>" entry this sentence used to name was retired
by #1249 — see the transfer-system entry below).
`scripts/item_contents_panel.lua` and
`scripts/transfer_session_panels.lua` supply level kinds and own no
window lifecycle at all — no page, no panel, no singleton, no
`setup()`/`update()`. A level owns one or more PANES and remains the unit
of NESTING, modality, teardown and restore, which is what makes two
flanking panels ONE level. The stack is transient session UI:
`hud.createUI()` snapshots and restores the whole thing across a resize,
and `uiManager.onSaveLoaded` drops it.

Pane semantics, the load-bearing `paneWidgetName` rule (control focus is
restored BY NAME, so two panes sharing one name return focus to the wrong
one), and the teardown REASONS — `"layout"` being the one that does not
fire `onClose`, which is what lets an escort session survive a resize —
are in `docs/engine_contracts.md` §Container window stack. Gates: hspec
`--match "container window stack"` / `"Container knowledge"` /
`"Nested item contents"` / `"Item list widget"`, plus
`tools/item_list_widget_probe.py` (manual-only, `needs-gpu`).

**Responsive lifecycle (#748 menus / #750 gameplay):**
`scripts/ui/responsive.lua` owns the supported envelope — bands
(inclusive): framebuffer height 600-900 @ 0.5-1x UI scale, 901-1200 @
0.75-2x, 1201-1600 @ 1-3x, 1601-2160 @ 1.5-4x; formal minimum 800x600.
`responsive.classify` is introspection only — out-of-envelope
combinations degrade best-effort (never crash, never invalid geometry,
fixed actions stay reachable), typically via `math.max(20, ...)` floors
and `math.min(panelW, fbW)` caps. Menu screens register via
`responsive.register(name, mod)` + `responsive.notifyResize(w, h)`
(0x0-minimize-guarded; re-notify with the SAME size = scale-only change).
Gameplay surfaces stay OFF that registry: they're reached either through
`ui_manager_boot.lua`'s manual forward or the engine's automatic
`broadcastToModules` resize — registering a broadcast-reached module
DOUBLE-FIRES it. Scale-only changes reach gameplay via
`uiManager.notifyGameplayRescale`.

Rules that keep resizes correct — follow them for any new screen/panel:
- A geometry rebuild must preserve state a semantic re-entry may reset:
  pending settings edits, scroll offsets, in-progress text, selected
  tabs, open-panel targets. `hud.createUI()` snapshots each world-page
  panel's "open for" state before the `view_teardown.lua` `"resize"`
  sweep and reopens via each panel's real entry point; restores must not
  re-fire `onChange`/`onSelect` (use the widgets' `silent` params,
  `toggle.restoreSlotIdentity`, `list.setSelectedIndex` — never
  `selectItem`). A surface with NESTING restores the whole nesting path.
- Keyboard control focus survives rebuilds by NAME:
  `responsive.snapshotControlFocusName()`/`restoreControlFocusName()`
  around any destroy+recreate; restore only after pages are re-shown.
- Fixed-size widgets fit via a LOCAL effective uiscale
  (`responsive.fitScale` against the reserved column/row/panel width);
  row labels reserve a `LABEL_COLUMN_FRACTION` 0.35 column. Shrink a
  box's font together with its box, never separately.
- Panels sized as `BASE * uiscale` must cap width/height to the
  framebuffer, and their content must derive from the panel's REAL bounds
  (`panel.getContentBounds()`), never an independently recomputed value
  that can drift. `scripts/ui/reserved_regions.lua` (pure) keeps popups
  clear of toolbar clusters (`hud.getToolbarRects()`, `avoidReserved`,
  `maxAvailableWidth`, `maxRightAnchoredWidth`, `findEscapes`).
- zIndex ACCUMULATES through the parent chain (`elementPaintKey` sums up
  `ueParent`) — leave wrapper/viewport elements at zIndex 0.
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

- `src/` — Library source (~730 modules)
- `app/Main.hs` — Executable entry point (draw loop)
- `test/` — hspec unit tests (engine core and Vulkan primitives)
- `cbits/` — C code (stb_truetype font rasterization, Lua debug FFI)
- `config/` — YAML config: tracked `*_default.yaml` templates +
  gitignored `*.local.yaml` runtime state (see "Config state" below)
- `data/` — Game data YAML (materials, vegetation, flora, units)
- `assets/` — Images and graphical resources
- `scripts/` — Lua scripts for game logic

## Working-tree discipline

**`~/work/synarchy` is the PRIMARY checkout and must be left CLEAN.** The
PR drainer fast-forwards it after every merge, autostashing whatever
uncommitted work it finds and restoring it afterwards. A restore that
CONFLICTS leaves unmerged entries in the index, and every later drainer
pass then refuses to run until a human resolves it. That happened four
times in 2026-08, every time on `docs/code_health_findings.md`, because
report-processing writes long-lived uncommitted edits into the one file
that merged PRs also rewrite.

So: **any file you write into the repo but do not commit belongs in the
docs worktree, never the primary checkout** — report annotation
(`/process-report`), findings documents, design-doc drafts, anything a
workflow leaves sitting for review. Resolve it by BRANCH — never
hard-code the path, never assume the current directory is right:

```bash
DOCS_WT="$(git worktree list --porcelain \
  | awk '/^worktree /{p=substr($0,10)} /^branch refs\/heads\/docs-wip$/{print p; exit}')"
[ -n "$DOCS_WT" ] || { DOCS_WT=~/work/synarchy-docs
                       git worktree add "$DOCS_WT" -b docs-wip origin/master; }
```

Docs land on master by direct push, not a PR. **Use `tools/docs_land.sh`,
not a hand-rolled sequence:**

```bash
tools/docs_land.sh -m "Commit subject" docs/foo.md [docs/bar.md …]
tools/docs_land.sh -n -m "…" docs/foo.md      # dry run
tools/docs_land.sh -f -m "…" docs/foo.md      # proceed despite the risk warning
```

It resolves the worktree by branch, commits ONLY the paths you name,
skips the rebase when master has not moved, judges success by `rev-list`
rather than push output, and fast-forwards the primary checkout only when
it is clean. Its reason to exist is the pre-flight check: it refuses,
before committing or stashing anything, when a file that is dirty here
but NOT being landed has also changed on master — exactly the combination
that makes a rebase autostash conflict, and exactly what keeps happening
on `docs/code_health_findings.md`. Landing ONE document while others are
still being written is the normal case, so the rebase must tolerate a
dirty tree; a plain `git rebase` aborts with "cannot rebase: You have
unstaged changes" and strands the landing. By hand, if ever needed:

```bash
cd "$DOCS_WT" && git add -- <paths> && git commit -m "…" \
  && git fetch origin && git rebase --autostash origin/master \
  && git push origin docs-wip:master
```

`--autostash` is required there, not decorative: a conflicting restore is
confined to this worktree instead of wedging the drainer. **`docs-wip` is
not a feature branch** — it tracks `origin/master` and lands by direct
push. That push prints `Cannot update this protected ref` and then
**succeeds anyway** under admin bypass, so judge it by `git rev-list
--left-right --count HEAD...origin/master`, never the warning. Details:
`docs/engine_contracts.md` §Docs landing.

Exempt, because they either create their own worktree or must operate on
the primary checkout: `solve`, `pr-revise`, `repair`, the read-only
`pr-review` / `pr-rereview` / `issue-review` reviewers, `drain-prs`,
`janitor`, `finalize`.

## Findings-report field ownership

A findings report (`docs/code_health_findings.md` and its siblings) is
written by two independent lanes, and they own DIFFERENT FIELDS of the
same entry.

**The report-processing lane (`/process-report`) exclusively owns an
entry's status fields:** the checklist checkbox, the trailing checklist
marker, and the heading marker (`[#N]`, `[#N, <note>]`, `[no-issue]`,
`[deferred]`, or none). It is the only lane that may add, remove, or
change any of the three, and it changes them together in one edit.

**An implementation PR may add to or update a finding's narrative body,
and nothing else.** Landing the fix for a finding does not disposition it
— a PR that marks the entry it resolves has answered a question the
processing lane had not asked yet. Say what changed in the body if it
helps; leave the box, the checklist marker, and the heading marker
exactly as you found them.

That split is not stylistic: the two lanes had already drifted an entry in
each direction, and each drift re-files merged work. The mechanism, and
why a master-side report edit costs an open PR its approval, are in
`docs/engine_contracts.md` §Findings-report lane split.

`tools/findings_report_audit.py` (CI + `make ci`, with its own
`tools/test_findings_report_audit.py` self-test)
fails when a CH item's heading marker and checklist marker disagree, and
when the two sides do not declare the same set of CH numbers exactly once
each. It audits AGREEMENT only — whether a marker is the right one, and
whether the box matches its terminality, stay the processing lane's
judgement.

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
a mode that honours it, a non-numeric
`--seed`/`--worldSize`/`--plates`/`--ages`/`--port`, a `--size` that isn't
`WxH` with both dimensions positive, and a `--dump=` selection that is
empty or names an unknown layer each exit 1 pre-boot naming the flag and
the offending token. **Omitting** a flag still keeps its documented
default — only a value the user actually typed can fail. `--region` is
deliberately excluded (`docs/code_health_findings.md` CH-67). Ordering
against the mode-compatibility rejection, and the full token rules, are in
`docs/engine_contracts.md` §CLI value validation. Gates: hspec
`--match "App.Cli"`, `tools/preview_cli_probe.py`.

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
unknown-category error lists exactly this set, no compatibility aliases:

- **Simple** (a flat, recursively-browsable asset folder): `icons`,
  `items`, `ui`, `world`.
- **Grouped** (one named entry per item — a bare grouped category prints
  "select a specific ..." and exits without booting): `units`, `flora`,
  `buildings`, `structures`.
- `equipment`, `hud`, `facemap`, `utility`, `vegetation` are NOT exposed
  (no top-level directory of that name, or — for `hud` — folded into
  `ui`'s recursive listing).

**Pre-boot rejection is the load-bearing rule** (`Engine.Preview.Discovery`
/ `.Unit` / `.Building`; `resolveItemDir` shared by all four grouped
categories): an unknown name, a name with path structure or
`.`/`..`/absolute traversal, a symlinked directory, and a FILE where a
directory was expected all exit 1 **before a window exists**. For
`units/<name>`, BOTH symlink levels matter — `doesDirectoryExist` follows
links, so a real unit directory with a symlinked `animations/` would
otherwise browse another tree's assets and break trimmed loading.

**Trimmed loading:** only its font, the list widget's own chrome textures
(loaded once, list-mode only), and textures within the requested
category/item — never `data/*.yaml` gameplay catalogs. Exactly TWO
exceptions, each a single file for the requested item: the units viewer's
`data/units/<name>.yaml` and the buildings viewer's
`data/buildings/<name>.yaml`.

**Shared browser** (in-engine: `scripts/ui/asset_browser.lua` +
`scripts/ui/list.lua`): a bare simple category lists every texture found
recursively under the root, labeled by its category-relative path with the
extension INCLUDED (`skill/climbing.png`), sorted lexicographically; the
first entry auto-selects and renders nearest-neighbour scaled.
`previewManager.init` forces `engine.setTextureFilter("nearest")`
live-session-only — never assumed from the video config, which a user's
persisted `video.local.yaml` can override. A label displayed here is
ALWAYS a valid item target: discovery and item resolution apply the
identical extension rule. **`flora/<name>` and `structures/<name>` reuse
this exact browser** (#888) rooted at the ITEM's folder, so anything
beyond routing the resolved folder into `discoverEntries` means the
routing is wrong, not the reuse. Selection/scroll/resize behavior:
`docs/engine_contracts.md` §Preview mode.

**Units viewer** (`--preview units/<name>`, #887/#1261;
`scripts/ui/unit_animation_view.lua`) — **`data/units/<name>.yaml` and
its compiled `atlas/index.json` decide everything.** The viewer samples
the compiled atlas through the same loader (`Unit.Atlas.Yaml.resolveUnitAtlases`)
and the same frozen cell arithmetic (`Unit.Atlas.Types.atlasCellUV`) the
game uses — a preview frame being a texture plus a sub-rect plus its cell
size (`PreviewFrame`), and a rejected target a pre-boot `UnitFocusError`;
a preview-only decoder would miss the
regressions the viewer exists to catch. A rejected, missing,
animation-less or uncompiled index is a PRE-BOOT failure, never a quiet
fall back to source frames, and an animation folder present on disk but
absent from the YAML is EXCLUDED from the browse list.

**Buildings viewer** (`--preview buildings/<name>`, #888;
`scripts/ui/building_asset_view.lua`) — **the filesystem is
authoritative**, and `data/buildings/<name>.yaml` only AUGMENTS a matched
animation with `fps`/`loop` plus default-selection hints; a missing,
malformed or unmatched YAML never rejects a valid asset folder. YAML
association is by CONTENT, never by equal names. Playback defaults are
`fps=8`, `loop=false` — NOT the units viewer's `loop=true`.

Ordering, direction mirroring, the playback-clock rules, both
default-selection ladders, and the full `previewManager.dump()` field
contract (which is what lets a probe click a located row instead of a
hardcoded coordinate) are in `docs/engine_contracts.md` §Preview mode.

Gates: `tools/preview_cli_probe.py` (CI-eligible, no boot at all — every
rejection above) and `tools/preview_probe.py` (manual-only, `needs-gpu`,
~15 window boots — discovery/selection/scroll/resize via the dump, forced
nearest filtering, both viewers, flora/structures dispatching into the
shared browser, and trimmed loading verified against
`engine.getLoadedTexturePaths()`, which is `Engine.Asset`'s
`apAssetPaths` populated by `engine.loadTexture`'s own Haskell handler —
the engine's authoritative record, not previewManager's self-reported
bookkeeping). Pure discovery/labeling/ordering/containment logic: hspec
`--match "Preview.Discovery"` / `"Preview.UnitAnimation"` /
`"Preview.Building"`.

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
engine, pass/fail checks). `tools/README.md` lists all ~85;
`ci_probes.py --status` gives CI eligibility. Durable contracts to know
before touching each area:

- **Unit/combat animations** — no pixels headless, but
  `unit.getInfo(uid)` returns `currentAnim`/`animStart` (unit thread runs
  headless); poll over time to verify timelines. Gate:
  `combat_anim_probe.py`. Drive by hand: load `scripts/unit_stats.lua` +
  `unit_resources` + `unit_ai`, then
  `require('scripts.unit_ai').commandAttack(atk,tgt)`.
- **Movement** — `scripts/movement_arena.lua` builds obstacle courses on
  a flat `world.initArena` world via the tile-edit API
  (`world.addTile`/`deleteTile`/`setFluidTile`/`setSlope` — `setSlope` is
  the ONLY way to make a step walkable). `startFall` clears the move
  target on landing, so fall checks assert the fall + landing z, not
  arrival. Gate: `movement_probe.py` (neutralises the unit_ai wander tick
  so `moveTo` is the only steering).
- **Tile-coordinate frame at the U seam (#1175/#1230)** — chunks are
  STORED u-wrapped, so one physical tile has two names near the seam. ONE
  contract, stated in full on `World.Render.HitTest`: picking
  (`pickWorldTile` and every Lua caller it backs —
  `world.pickTile`/`pickPos`/`getHoverTile`/`getHoverPos`),
  designation maps, and every point read / mutation / cancellation —
  including the verbs a worker FINISHES a job with
  (`world.getDigInfoAt`/`digTile`, `harvestFlora`, `setVegAt`,
  `plantCropAt`/`plantRowCropAt`, `structure.place`/`hasAt`/`floorZAt`/
  `clear`, and `building.spawn`/`canPlaceAt`, whose footprint walk
  resolves each tile) — use CANONICAL coords
  and accept any alias, so pre-#1175 saved job coords need no migration.
  RECTANGLES are the exception: canonical is a STORAGE frame, not a
  geometry one, so a drag's second endpoint is re-expressed in the
  anchor's local alias frame (`localizeTileToAnchor`, shared by
  `World.Thread.Command.Cursor.Common.designateRect` and the `CursorQuads`
  previews; Lua `world.localizeTile` for `build_tool.lua`'s wire snap /
  occupancy scan) BEFORE any clamp/`min`/`max`, canonicalising per
  enumerated tile at lookup/storage only. Job-SELECTION ranges need that
  frame too — `construction.getPendingJobs` reports `lx`/`ly` beside
  canonical `x`/`y`, and `unit_ai_construct.lua` measures with those.
  Canonicalising one end alone MEASURED worse than
  seam-blind behaviour; don't. Terrain LOOKUPS take the same frame:
  `World.Tile.Types.lookupChunk` wraps nothing, so any consumer must
  `wrapChunkCoordU` first — `Unit.LineOfSight.tileTerrainZ` now does — a
  miss reads as "not loaded → assume flat", which for occlusion
  means "nothing blocks". The chunk-init queue is wrapped at the drain.
  Where a tile is DRAWN is the separate `bestWrapOffset` axis (#1176).
  Away from the seam, and in arenas, every step is the identity.
  `world-activity` v1/v2 payloads are re-keyed on load. Gates: hspec
  `--match "World.Render.PickSeam"` / `"World.DesignationSeam"` /
  `"a seam-frame unit"`.
- **Construction (#95/#96)** — `construction.*` designations +
  construct_job AI (claim → source materials → progress → place →
  stake); build costs in `data/structure_packs/*.yaml` `build:` blocks.
  Gate: `construction_probe.py` (stake phase runs LAST).
- **Roles (#265)** — DERIVED labels, never assigned: highest work skill
  ≥ 30 (+5 switch hysteresis). Roles multiply work-action ENTRY utilities
  only (on-role ×1.4, off-role ×0.7) — never the 6.0 in-progress locks,
  never survival/combat/orders. `unitAi.getRole`. Gate: `role_probe.py`.
- **Crafting (#325/#326/#329/#343/#795)** — recipes in
  `data/recipes/*.yaml` (station tag, inputs, optional
  fuel/knowledge/skill, work, outputs, optional `power_draw`).
  `craft.execute(uid, recipeId)` is station-blind (tests/console);
  `craft.executeAt(uid, recipeId, bid[, billId])` needs a Built station
  offering the operation with the unit adjacent (Chebyshev ≤ 1). Bills
  (`Craft.Bills`, per-page, engine-side atomic claims, persisted) have
  three modes: fixed count, repeat-forever, until-stock — the last
  re-checks LIVE ground stock via `unit_ai_fetch.untilStockSatisfied`,
  the same formula the crafting panel uses. Skill-tagged recipes derive
  output quality from the crafter, then shift by live mental
  effectiveness (±10), so quality assertions must pin the
  neutral-effectiveness precondition (#878). Gates: `craft_probe.py`,
  `craft_bill_probe.py`.
- **Player transfers + orders (#1000/#1085/#1246-#1255)** — design
  authority: [`docs/unified_item_transfers.md`](docs/unified_item_transfers.md).
  ONE pure policy (`src/Unit/Transfer.hs`) decides whether exact item
  instances may move between two endpoints (a unit inventory or a built
  building's loose storage, on BOTH sides; direction DERIVED from the
  pair): Chebyshev ≤ 1 between occupied RECTANGLES, capacity weighs the
  actual instance, batches are ordered and report per-item outcomes, and
  no item ever half-moves. The lax AI verbs
  (`transferItemToUnit`/`transferItemToBuilding`/`depositToCargo`/
  `withdrawFromCargo`) are a SEPARATE, deliberately unchecked path the
  fetch/repair/medic ladders depend on — never route AI work through the
  strict one, and never delete them.

  **TWO player modes, ONE commit policy.** Mode B queues a durable order
  and Mode A commits on the spot, but both build the IDENTICAL request
  and both reach `checkTransfer`/`commitTransfer` — so an exact-instance
  identity, a partial batch and a capacity or proximity refusal mean the
  same thing in either. The player-facing IMMEDIATE paths retired with
  #1249 and must not come back: the adjacent-cargo "Store in <cargo>"
  enumeration (`unit.depositToCargo`) and the container window's
  "Withdraw with <unit>" (`unit.withdrawFromCargo`, plus its disabled
  "select an adjacent unit first" placeholder) are now the Store /
  Retrieve gestures, and NEITHER requires adjacency. Only the PLAYER
  paths retired; the verbs themselves stay registered for the AI (D-7).

  **Contents are REMEMBERED, never live (D-2).** A container window
  renders the player's last observation, so refreshing it is a RULE, not
  a read. Exactly four things reveal (`Building.Knowledge.Live`): a
  completed transfer commit into or out of the container, the lax AI
  cargo verbs, a Mode A session OPENING on it
  (`building.refreshContainerKnowledge` — that transition is its only
  caller in the game), and the first completion of a storage-capable
  building, which seeds KNOWN-EMPTY because the player watched it go up.
  Walking past, selecting, right-clicking and opening the window reveal
  NOTHING, and every unit-driven reveal is gated on
  `isPlayerCommandable`, so a non-player unit's withdrawal leaves the
  record stale on purpose. Knowledge is player-global, never per-unit.

  The three player-facing modes — durable ORDERS (#1246/#1247/#1253, where
  arrival is the commit and every ending surfaces once then prunes), Mode
  B queued GESTURES (#1249, Store/Retrieve 1-and-all, neither requiring
  adjacency), and Mode A ESCORT (#1250/#1251, walk first then choose, with
  a two-sided hold) — are specified in `docs/engine_contracts.md`
  §Player transfers. Read it before touching an executor, a gesture's
  eligibility, or a teardown path: the rules about what is OMITTED rather
  than disabled, which timer is a stall rather than a trip budget, and why
  `escort_hold` is auto-prepended to every species are all load-bearing.

  Gates: hspec `--match "Unit transfer"` / `"Transfer context menu"` /
  `"durable transfer orders survive"` / `"Container knowledge"`;
  `tools/transfer_order_probe.py` and `tools/item_list_widget_probe.py`
  (manual-only; the latter owns the real-AI proof that a MOVING target
  is preempted and then stays put for the whole approach), and — the
  arc's INTEGRATED gate — `tools/unified_transfer_probe.py` (#1255,
  manual-only `needs-gpu`): one fixed-seed session with independently
  reported stages proving an exact instance moves both ways between all
  three endpoint classes through BOTH modes, plus the partial batch, the
  reveal rule, one widget rendering every container view, and a Mode B
  order surviving a fresh-process reload while a Mode A session does
  not.
- **Power (#358-#361, #590/#591, #1206)** — solar/battery nodes are
  item-consuming placements (`power.placeNode` via
  `buildTool.commitPlacement`); networks (wire 4-adjacency +
  nodes/consumers) are recomputed fresh every tick — only battery
  `storedWh` persists. Solar follows the sun angle and
  `world.setTimeScale`. Electrical load lives on the RECIPE
  (`power_draw`), not the building (`power_drain` exists only for
  hypothetical always-on devices; no shipped building sets it): a bill
  draws only while claimed AND `cbWorking`.
  `power.isStationPoweredForRecipe(bid, recipeId[, billId])` is the
  gating query — pass the bill's own id so its already-registered draw
  isn't double-counted while other consumers still sum. A node's LIFETIME
  is its building's: `BuildingDestroy` retires it in the same live
  transaction that removes the instance
  (`Power.Live.retirePowerNodeEverywhere`, resolving the session-global
  `BuildingId` across every live page — the `forgetContainerEverywhere`
  pattern), so a demolition never reaches the save. That is NOT load-time
  pruning — a save already carrying a
  dangling node still restores it verbatim. Retirement is a delete, never
  a compaction: `pnsNextId` keeps advancing and a retired id is never
  reissued. There is deliberately no public `power.removeNode`. Gates:
  `power_probe.py`, `power_workshop_probe.py`, `machine_shop_probe.py`,
  hspec `--match "power node demolition"`; pure algorithm in
  `Test.Headless.Power.Network`.
- **Farming (#331-#336)** — flora growth is DERIVED state from the
  advancing calendar (nothing per-instance in saves;
  `world.getDate`/`setDate`, `world.getFloraGrowthAt`). Fruiting windows
  gate bare food-harvest calls only; tagged calls (chop's `"wood"`) skip
  the window, and chop-claim keys on `regrowthRemaining`+`tags`, not
  `harvestable`. Tilling: `till.*` mirrors `chop.*`; completion writes
  `world.setVegAt` (edit-log — survives eviction/saves); consumers must
  use `world.isPlantable`, never compare `getVegAt` to raw id 77. Gates:
  `flora_growth_probe.py` (registers a max-tolerance `probe_berry`
  species), `till_probe.py`.
- **Location instances (#911)** — a placed location is a persisted
  per-page record (`Location.Instance`) keyed by a stable
  `LocationInstanceId` (from 1), allocated at PLACEMENT time in the
  deterministic overlay's `overlayToList` order — never at stamp time,
  never from hashmap order — so ids survive save/load and chunk eviction.
  It stores definition id, anchor, resolved absolute bounds, display name
  + optional gloss, a one-time content-spawn flag, and lifecycle
  `unknown → hinted → discovered → active → cleared → depleted`.
  Consumers read the STORED values, never re-derive from the live
  registry. `wgpLocationStamped` stays chunk-keyed (#424). Transitions
  are one-way (`promoteLifecycle` refuses backward AND same-state), which
  is what makes discovery fire exactly one event. Nothing drives an
  instance past `discovered`; `hinted` is deliberately unreachable but
  must NOT be deleted (the enum is positionally serialized and
  append-only). Queries: `world.listPlacedLocations([pageId])` (extended,
  not repurposed — `id` is still the DEFINITION id),
  `getLocationInstance`, `setLocationLifecycle`,
  `markLocationContentsSpawnedById` (`instance_id`/`lifecycle`/`name`/
  `contents_spawned` are the new fields); the coordinate-addressed
  `hasSpawnedLocationContents`/`markLocationContentsSpawned` remain
  compatibility wrappers resolving to the chunk's first
  instance. Persistence: `world-pages` v7, with a frozen v1 DTO whose
  per-chunk flags decode PENDING and resolve against the registry at the
  load path's content-validation stage (`resolveLegacyLocations`). Gates: hspec
  `--match "Location instance identity"`, `location_content_probe.py`.
- **Location + river naming, etymology (#1101/#1102/#1104)** — a placed
  instance's `name` is rendered in its PAGE's own generated language
  (from the identity's #1092 provenance); `gloss` is the same
  `NameExpr`'s English reading. A LOCATION's concept pools are DATA
  (`ldNaming`'s ordered, nonempty `heads`/`modifiers`, validated against
  `data/language/concepts.yaml` at load — an unknown id rejects the whole
  file rather than degrading to `ldLabel`); the engine has no
  `ldType`→concept mapping. RIVERS have no definition file, so their
  pools are in code (`riverHeadConcepts`: `RIVER`, `FORD`, `CROSSING`,
  `BAY`, `VALE`, `HOLLOW` — a NARROW head pool against a WIDE modifier
  pool of every catalogue concept with a modifier form, which is what
  makes a head morpheme recur across a
  map and in the world's own name). The expression is always `Modifier
  modifier head`, chosen deterministically from the entity's own stable
  id plus the language seed/version, never from hashmap order. Names are
  WRITE-ONCE (#708 principle 5): rendered by the single writer at
  creation (`newLocationInstance`; `buildRiverNames` at world init) and
  read thereafter, so growing the catalogue never re-renders one. A page
  with NO provenance falls back to `ldLabel` with `gloss` ABSENT / an
  EMPTY river-name table — absence is never papered over by inventing a
  language.
  River identity is `(WorldPageId, GeoFeatureId)`, reusing the id the
  timeline already allocated. `World.River.Identity` is the ONE place
  events are paired with features, and the pairing is CHECKED against
  source/mouth/flow before it is trusted — a violated invariant yields no
  id rather than a wrong one. Names live in a per-page `wgpRiverNames`
  keyed by `GeoFeatureId`, deliberately NOT on `PersistentFeature`
  (whose `GeoTimeline` is positionally serialized worldgen OUTPUT).
  Etymology (#1104): an optional `EtymologySource` (originating `NameExpr`
  + the `LanguageProvenance` that rendered it) is persisted beside the
  name on all three carriers; a precomputed morpheme list deliberately is
  not. `Language.Etymology` re-renders and CHECKS against the stored text
  before showing any of it, and the source must belong to the PAGE's own
  recorded language. `world.getEtymology(kind[, id][, pageId])` is the
  one path; `pageId` names the TARGET only (#1265) and never widens the
  recurrence set, which is always the ACTIVE page — so self-exclusion is
  PAGE-QUALIFIED. `world.getRiverAt` is the minimal
  selected-segment→identity resolution. `Language.Suggest` (#1106) is the
  one remaining copy of the profile+roots+catalogue resolution — fold it
  in rather than adding a fourth. UI: `scripts/etymology_panel.lua` is
  the ONE panel all three entry points open, hosted by
  `scripts/name_plate.lua` on `hud.global_page` (NOT `world_page` — a
  plate on a band-swapped page is unhittable in the zoom map). The token
  trace, morpheme identity, capitalization, the recurrence rules and the
  frozen DTOs are in `docs/engine_contracts.md` §Name etymology. Gates:
  hspec `--match "Location naming"` / `"River naming"` /
  `"River identity"` / `"Language etymology"` / `"Etymology panel"`;
  `river_naming_probe.py`, `location_content_probe.py`,
  `etymology_probe.py` (manual-only, `needs-gpu`).

- **Location discovery (#780, sight-based since #1230)** — a one-way
  promotion to `discovered`, fired when a `uiFactionId == "player"` unit
  SEES the location: its visible-tile set intersects the instance's
  stored `liBounds`, seam-aware, one tile being enough. The
  `discovery_margin` halo is GONE from YAML, def, instance, Lua and wire;
  `bounds` is the only location footprint left. Sight is
  `Unit.LineOfSight.visibleTilesOnPage` — the SAME calculation
  `unit.getVisibleTiles` runs (perception radius scaled by the page-local
  `nightPerceptionFactor`, 120° facing cone, terrain-Z occlusion) minus
  that query's `wmVisible` gate, which keeps reveal working on a
  loaded-but-hidden page while `unitVisibleTiles` still reports `[]`
  there. Terrain, clock and world size come from the
  RESOLVED page's own refs, never `activeWorldSizeChunks`. Ticks for
  EVERY loaded page, independent of pause; emits exactly one
  `location_discovery` event. A night-scaled radius is intentionally
  shorter, so any distance-sensitive expectation over
  `unit.getVisibleTiles` (`scripts/unit_ai_water.lua`'s `scanForWater`,
  `tools/tutorial_probe.py`'s `sees_water`) must pin the clock. Gates:
  `location_content_probe.py`, `location_embark_probe.py`; hspec
  `--match "Location discovery"` / `"Location map icons"` /
  `"Unit.LineOfSight"`.
- **Location map icons (#781/#1230)** — a definition declares ONE
  optional `map_icon` (its TYPE icon). All six lifecycle constructors map
  explicitly (`World.Render.Zoom.Icons.locationIconAppearance`):
  `unknown`/`hinted` draw the ONE shared `location_unknown.png`
  (registered once under `locationUnknownIconTextureName`,
  independently of every definition) so the zoom map never
  leaks WHAT is there before a unit has seen it; `discovered`/`active`
  draw the def's own `map_icon`; `cleared`/`depleted` draw that SAME
  bitmap with darkened RGB (`clearedIconTint`), the zoom-fade alpha
  preserved exactly in all six. That dark tint is an explicit, enumerated
  exception to the no-tinting rule (`docs/expedition_gameplay_loop.md`
  D-16), confined to the icon quad's own `Vec4`. A def with no `map_icon`
  places no annotation. Asset gate:
  `tools/location_map_icon_asset_check.py`.
- **Per-unit location knowledge (#915)** — the EXPERIENTIAL layer beside
  that CARTOGRAPHIC one, and neither derives from the other: global
  lifecycle = "the player has mapped it", `aiState[uid].knownLocations` =
  "this acolyte knows where it is". Keyed by the durable `(page, instance
  id)` pair — dedup is by IDENTITY, never by distance (don't copy
  `knownWaterSources`' 6-tile rule across; two locations are never the
  same location). Both layers come from ONE containment enumeration in
  `Location.Discovery` (`findDiscoveries`/`findAwareness`), so they
  cannot drift; awareness additionally reports EVERY qualifying unit and
  ignores lifecycle, so a unit arriving at an already-mapped ruin still
  learns it. `world.getLocationAwareness()` walks every loaded page;
  `scripts/unit_ai.lua` ingests it BEFORE its pause guard. Persisted via
  `lua.unit_ai` v4 as typed `{__ref="location_instance", …}` entries
  (v1-v3 decode with the field ABSENT, never inferred from discovery); a
  memory whose `(page, id)` is missing is a non-blocking diagnostic,
  scrubbed at reconcile. Radio sharing/range deliberately deferred.
  Gates: hspec `--match "unit location knowledge"`,
  `location_content_probe.py`.
- **Expedition retrieval (#920)** — recovering a remote item uses ONLY
  the direct-RTS verbs a player already has (`unitAi.commandPickup` →
  `commandMove` home → adjacent `unit.depositToCargo`);
  `docs/expedition_gameplay_loop.md` forbids a caravan/logistics
  interface until direct retrieval proves inadequate. That last step is
  the LAX verb, not a player gesture. `commandPickup` gates capacity at
  COMMAND time (refuses, returns false, emits `unit_warning`, sets no
  `pickupOrder`) AND again on ARRIVAL, both measuring
  `unit.getCarryingWeight` against the ground instance's live weight —
  keep both; the load changes en route. A completed pickup emits a
  `unit_event` tagged with the carrier's uid.
  `pickup_timeout`/`TASK_TIMEOUT_SEC` are STALL timers, not trip budgets:
  they reset on a new closest approach. Don't restore the
  from-`issuedAt`/`startedAt` shape — it capped ordered retrieval at ~21
  tiles. Since
  #1291 they are spent in ELIGIBLE time only (`unit_ai_stall.lua`, which
  owns the accounting and `maintainTask`): an interval another action
  won (the #306 ladder's eating/drinking/refill/combat/`treat_ally`, or a
  `forage` that walks the unit AWAY), or one the AI never ticked through
  at all (collapse, an engine
  animation, a mental break, a load boundary — seen as a gap longer than
  `MAX_CHARGED_INTERVAL`), costs a pending order nothing, while the
  budget still ACCUMULATES across interruptions so no order becomes
  immortal. That state (`stalledFor`/`stallSeenAt` on the order) rides
  `lua.unit_ai` v5; a v1–v4 order carries the
  old absolute `progressAt` and is seeded from it on its first tick.
  Gates: `expedition_retrieval_probe.py` (manual-only), hspec
  `--match "commanded order stall budget"`.
- **The expedition loop (#923)** — the shipped slice is **prepare →
  travel → discover → extract → return → invest**, run as ONE session by
  `tools/expedition_loop_probe.py` (manual-only, fixed-seed, ~15 min,
  two engine boots). `docs/expedition_gameplay_loop.md` is the design
  authority; step 9's combat encounter and progression reward are
  deferred (#916/#917), so "invest" means the loot is banked as ordinary
  colony stock. Contracts the gate pins: the colony comes from a real
  `acolyte_portal` and its OWN roster, never hand-spawned units; the
  expected end lifecycle is `discovered` with contents spawned exactly
  once (a gate calling `setLocationLifecycle` would be asserting its own
  writes); the extraction target is whichever def the ruin's loot rolls
  produced; and every durable identity is re-checked in a FRESH PROCESS.
  It also runs an **unprepared control** — a second traveller sharing ONE
  identical leg, differing only in FOOD — which must end measurably worse
  off, which is what makes the scenario prove preparation matters rather
  than prove a walk succeeds. Six conditions keep that comparison honest
  and weakening any one turns the control into theatre; they are
  enumerated, with the two live-observed physiology traps (don't seed a
  thirst deficit; `unit.setFrozen` is not a hold) in
  `docs/engine_contracts.md` §The expedition loop. Read it before editing
  this gate.
- **Blood decals (#603 epic)** — architecture record:
  [`docs/blood_decals.md`](docs/blood_decals.md). Five `--match`-able
  hspec groups under `test-headless/Test/Headless/Blood/`:
  `Blood.Types`, `Blood.Texture`, `Blood.Impact`, `Blood.Trail`
  (includes `Blood.Pool`), `Blood.Teardown`. Probes:
  `blood_decal_probe.py`, `blood_impact_probe.py`,
  `bleeding_trail_probe.py`, and the needs-GPU
  `blood_gpu_lifecycle_probe.py` (manual-only). **Transience contract**:
  blood is transient BY DESIGN — `wsBloodStoreRef` and every unit's
  `TrailState` are deliberately never persisted, and a loaded session
  always starts with no decals and no accumulators. A test asserting a
  mark survives a save/load round trip is testing for behavior this
  engine deliberately does not have (closed issue #884 is the spec for
  reversing it).
- **Logging streams** — event log: `engine.getEventLog()`, emit via
  `engine.emitEvent(cat,text)` / `emitEventAt` /
  `emitEventForUnit(cat,text,uid[,gx,gy])`; a category lands only if its
  notifications YAML has `log: true`. Combat: `combat.drainEvents()`.
  Injury (NON-combat only — falls, hazards, wound deaths):
  `injury.drainEvents()`. These are DRAINED streams — don't drain
  manually in a test while the panel script is loaded, or you'll race it.
  Gate: `injury_log_probe.py`.
- **Autosave (#913)** — OFF by default (`config/save_default.yaml`
  overlaid key-by-key with `config/save.local.yaml`; Settings → General
  edits it). `scripts/autosave.lua` owns the WALL-CLOCK interval and
  fires only when `uiManager.isGameplayView()` — a deadline reached in a
  menu / with no world / mid save-or-load is SKIPPED silently, and menus
  never suspend or reset the cadence. Slots are the reserved
  `autosave-<n>` family, `autosave-1` newest; ownership is the durable
  `smAutosave` metadata flag (`"metadata"` v2; v1 payloads migrate to
  manual via `World.Save.Compat.MetadataV1`),
  NEVER the name — a manual save squatting on one of those names fails
  the attempt through `save_load` with nothing rotated. PUBLISH FIRST,
  ROTATE SECOND, and the
  rotation is itself ordered so an interruption leaves a partially
  shifted family rather than a shorter one; the staging slot, the
  retire-by-rename ordering, the DERIVED shift plan and the
  `playerIntentGenRef` mutex are in `docs/engine_contracts.md` §Autosave.
  A FAILED autosave stays paused and zero-scaled. Gate:
  `autosave_probe.py` (manual-only).
- **Config state (#638/#786)** — settings save to gitignored
  `config/*.local.yaml`; boot falls back to tracked `*_default.yaml`
  (notifications self-materializes from
  `data/notification_categories.yaml`; `save` resolves as an explicit
  KEY-LEVEL overlay instead, so a sparse local file keeps every tracked
  default it doesn't mention). The tracked legacy
  `video.yaml`/`keybinds.yaml`/`notifications.yaml` exist ONLY as a
  one-time migration source: `Engine.Core.Init.migrateLegacyConfig`
  copies a legacy file to the local path iff the local file is absent AND
  the legacy file decodes against the real target schema; failures fall
  back to defaults and never touch a valid local file.

  **A headless spec that drives a production path which WRITES `config/`
  must wrap `Test.Headless.Harness.Isolation.withIsolatedResourceRoot`
  AROUND `withHeadlessEngine`** (#1357, enforcing #1266's "tests never
  modify, truncate or regenerate the developer's `config/*.local.yaml`").
  It points the process cwd at a scratch root that symlinks every
  top-level checkout entry but owns a real COPY of `config/` — the one
  family production code writes into — so every cwd-relative write lands
  in a temp dir. Outside, never inside: engine init is itself a writer
  (`migrateLegacyConfig`, the notification-overrides materializer), so a
  fixture that intervened after the engine came up would already be too
  late. The checkout is only ever READ, so no crash can leave developer
  state half-restored. Two properties keep the fixture from deleting the
  wrong thing: the root is created FRESH and EXCLUSIVELY per invocation
  under a random name via `createDirectory` (a fixed path could already
  hold a symlink, and `doesDirectoryExist` follows one, so teardown would
  enumerate and recursively delete the TARGET's children), and "am I
  isolated?" is `isInsideIsolatedResourceRoot` — fixture-owned state
  checked against the real cwd, never a marker file, which any same-named
  file on disk could forge into skipping isolation entirely. The two
  suites that need it (`UI.ResponsiveMenus`, `UI.ResponsiveGameplay`,
  both reaching the write-through `settingsMenu.onDefaults()`) each carry
  a one-line in-suite guard asserting they run under it, because every
  other assertion in them passed while the developer's bindings were
  being replaced.

  Gates: `config_state_probe.py`, `config_migration_probe.py`; hspec
  `--match "config"`, `--match "Settings Defaults keybind persistence"`
  (the isolation boundary itself, plus the player-facing Defaults
  write-through it must not weaken).

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
# for phase == "LoadPublished" (or "LoadFailed", or "LoadReconciliationFailed")
# before touching anything. Loaded pages keep their saved ids (no
# main_world remap) — world.getActiveWorldId() finds the active one.
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
(always default post-load; HUD resets via the `onSaveLoaded` broadcast),
and time scale (always 1). Older schema versions are rejected with
"expected vN, got vM".

**Autosave (#913):** interval autosaves ride the SAME transaction — they
only add a request-time `AutosaveRequest` (pre-request pause, visible time
scale, player-intent generation) plus the durable `smAutosave`
classification `engine.listSaves()` exposes. Full contract: **Subsystem probes
& domain contracts** above, and `docs/engine_contracts.md` §Autosave.

**Enum schema policy:** `Direction`, `Pose`, `UnitActivity` (and any
enum serialized via `Generic Serialize`) are positional by constructor
tag — **append-only**. Inserting/reordering silently corrupts saves;
anything beyond appending requires a `currentSaveVersion` bump. A
constructor's own FIELDS are positional too, so reordering them or
changing one field's serialized type corrupts saves the same way while
moving no tag (#1270).

Enforced since #1145 by `tools/enum_append_only_audit.py` (CI + `make
ci`, with its own `--self-test`), which is the authority on which types
are guarded and why — read its module docstring before adding, moving, or
changing one. It guards **every** `data` declaration under `src/`/`app/`
that derives `Serialize` through `Generic` and has two or more
constructors — a deliberate superset of "reachable from a save
component" (43 types today, which the audit prints on every run) — so a
type that becomes persisted later was already guarded the day its instance
was derived.
`docs/save_compat/enum_baseline.json` is the GENERATED golden constructor
list; don't hand-edit it — a pure append ratchets it with
`--update-baseline`, and anything else is a wire-format break the audit
refuses to record. How a payload slot is normalized, what the recorded
attribution buys for a renamed or deleted type, and the split against
`tools/save_compat_audit.py` are in `docs/engine_contracts.md`
§Enum append-only audit.

**Architecture (persistence-overhaul epic #756-#768, landed):**
- `World.Save.Snapshot.SessionSnapshot` is the immutable, validated
  in-memory capture (pure `captureSessionSnapshot`) — NOT the wire
  format. The save barrier (`Engine.Save.Barrier`) quiesces every
  state-owner thread, releases its capture lock only after the encode is
  forced (`evaluate`), and reports the outcome only after the disk write
  resolves.
- On-disk `world.synworld` is a tagged, checksummed component ENVELOPE
  (`World.Save.Envelope`): FNV-1a-checksummed manifest + independently
  versioned components (`core-session`, `world-pages`, `world-edits`,
  `world-activity`, `buildings`, `units`, `unit-sim`, `craft-bills`,
  `power-nodes`, `texture-palette`, `metadata`, the two OPTIONAL
  `container-knowledge` and `transfer-orders`, plus dynamic
  `lua.<module>` components). Registry:
  `World.Save.Component.saveComponentRegistry`. Every gameplay component
  is REQUIRED except those two, each of whose absence has an honest
  default — see `docs/persistence_contract.md` §5 before declaring a
  third. Component evolution = per-component schema version bumps +
  explicit migrations from frozen vN DTOs, NOT a global save-version
  bump. `currentSaveVersion` now versions only the transitional in-memory
  load bridge (`SaveData`) and is bumped freely — don't trust any number
  written in docs. `listSaves` decodes only the `metadata` component.
  Pre-envelope flat saves are a clean break (rejected), and
  `world_gen.yaml` no longer exists.
- Lua-owned state persists via `scripts/lib/save_modules.lua`
  (`saveModules.register(id, spec)` — versioned
  snapshot/decode/validate/apply, dependency-ordered, `required` vs
  optional-with-`default`; `registerResetHook` for non-durable modules),
  with canonical data-only payloads from `scripts/lib/data_codec.lua`
  (decoding never executes code). A required component's failure aborts
  the whole save/load.
- Disk I/O goes ONLY through `World.Save.Storage.publishGeneration` — a
  write-fsync-revalidate-rotate transaction keeping a
  `world.synworld.prev` recovery generation, with `.prev` fallback on
  corruption (`recovered` in `engine.listSaves()`) but never on an
  INCOMPATIBLE file. Symlinked slot dirs/files are refused.
- `engine.loadSave` is a whole-session TRANSACTION
  (`World.Load.Stage`/`Publish`): stage the entire replacement session
  against fresh values, swap in one quiesced window. A load REPLACES the
  complete session — live pages not in the save do not survive. Save and
  load mutually exclude; a failed load leaves the old session unchanged
  and paused (a one-way ratchet per attempt).
- Typed persistent references (`World.Save.Reference`:
  `SamePageRef`/`CrossPageRef`; Lua `{__ref=kind, id=N}` in
  `unit_ai_save_refs.lua`/`building_spawn.lua`) feed the shared integrity
  graph (`World.Save.Integrity`) at both save and load boundaries —
  wrong-PAGE targets are hard errors; DANGLING targets are tolerated,
  non-blocking diagnostics. NB: ground-item ids are ZERO-based; every
  other allocator starts at 1.

The 13-phase load lifecycle, `LoadReconciliationFailed`'s terminal
semantics, and the `StoragePhase` reporting are in
`docs/engine_contracts.md` §Save/load transaction.

**Key gates:** pure hspec — `--match "persistence contract"` (full
representative session through the real codec, every field via derived
`Eq`), `--match "persistence reference integrity"`, `--match "Lua
persistence components"`, `--match "save envelope"` / `"save components"`
/ `"atomic save storage"`. Probes — `persistence_contract_probe.py`
(CI-eligible smoke: three real fresh-process save→load→save cycles
compared via `tools/persistence_snapshot.compare_session_files`),
`persistence_contract_sweep.py` (manual full sweep on isolated resource
roots), `save_barrier_probe.py`, `save_storage_probe.py`,
`transactional_load_probe.py`, `persistence_integrity_probe.py`,
`multiworld_save_probe.py`. NB #365: a save containing an arena page
hangs the world thread on load — never use arenas as a save-test page.

## Unit animation art: inventory, compiler, budgets, runtime

Design authority: [`docs/texture_infrastructure.md`](docs/texture_infrastructure.md)
(TEX-2…TEX-7, decisions D-1…D-12, and **Measured results** — the
before/after numbers and the budget threshold's derivation).
Fresh-checkout workflow (regenerate, validate, inspect, review, recover)
and D-7's restart-to-reload rule:
[`docs/asset_generation.md`](docs/asset_generation.md).

Source PNG frames stay the editable artwork (D-1); unit YAML stays the
only hand-edited semantic authority (D-11); everything under a unit's
`atlas/` directory is DERIVED and nobody hand-edits it. `atlas/` is a
SIBLING of `animations/`, which keeps generated artifacts outside the
filesystem-first inventory walk.

**One command covers all four concerns:**
`python3 tools/pack_atlas.py --validate-only --strict` runs the art
inventory, the compiler's freshness comparison, AND the two budgets. Add
`--compile [--unit <name>]` to regenerate, or `--compile --check` to
report staleness without writing. Deps are pinned in
`tools/requirements-assets.txt` (PyYAML + Pillow), spelled again in
`.github/ci/Dockerfile` because the image tag is that file's own hash —
`test_pack_atlas.py` fails if the two drift. Pillow is load-bearing for
VALIDATION too: an absent decoder is one loud error naming the install
command, never a silent skip.

### Inventory (#1257, #1311)

Discovery is **filesystem-first**: it walks every PNG under
`assets/textures/units/<unit>/animations/<animation>/<direction>/` and
checks the declarations against it, never the other way round. Corpus: 7
unit trees, 116 animations, 4,620 frames; strict validation exits 0 with
zero warnings. **Every committed animation PNG is owned by exactly one
animation-frame declaration; there is no directory or glob exemption
mechanism.** Scope is `animations/` — non-animation unit textures
(`sprite`, `directional_sprites`, `portrait`,
`unknown_unit/rotations/*.png`) are existence-checked only.
**"Duplicate" means duplicate ANIMATION-FRAME claims only** — reusing an
animation frame as a `sprite`, `directional_sprites` entry, or `portrait`
is deliberately legal (20 shipped references do this).

Two declaration forms live under `data/units/`, and the top-level key is
the entire runtime distinction:

- `units:` — a gameplay unit. `Engine.Asset.YamlUnits.loadUnitYaml`
  returns these, so they register, load textures, list, and spawn.
  `name` and `sprite` are mandatory.
- `asset_units:` — asset-only: exactly `name` + `animations`, as a
  WHITELIST enforced by BOTH decoders (Aeson ignores keys a parser
  doesn't ask for, so `UnitYamlAssetDef` checks the key set explicitly; a
  silently accepted `sprite:` would decode fine, be skipped by
  `loadUnitYaml`, and look exactly like a unit that failed to register).
  `loadUnitYamlAssets` returns them; nothing registers, textures, lists,
  or spawns them. **NO shipped file uses this form since #1261** — it
  stays supported and fixture-tested.

A file may hold either key or both; a file holding NEITHER is refused
rather than decoded as zero units (that is what a mistyped top-level key
looks like), and so is a key present with an explicit `null` (aeson's
`.:?` reads that as absent, so accepting it would leave CI green while
startup logged a parse failure). Three decoders share the shape:
`UnitYamlFile`, `Engine.Preview.Unit`'s `UnitAnimMetaFile`, and
`pack_atlas.py`. Animation/direction keys are strings, never coerced —
YAML resolves an unquoted `123:` to an int whose `str()` looks like a
valid identifier.

Structural invariants, and the three independent CONTENT checks every
declared frame is put through (#1311), are enumerated in
`docs/engine_contracts.md` §Unit animation art — read it before adding,
relaxing or "simplifying" a rule there. In outline: identifiers are one
lowercase `[a-z0-9_]+` component (plus the one approved
`<lowercase>_RH_<lowercase>` asymmetric-weapon form); frames are
`frame_NNN.png` with exactly three digits; declared paths are relative,
`..`-free, symlink-free and resolve inside their EXACT direction
directory; `flip` decides five authored directions or all eight; indices
start at 0, ASCEND in declared order, and have no gaps or duplicates,
while counts may differ per direction; `fps`/`loop` are rejected rather
than coerced; and no symlink may appear anywhere in the walk. Contents
are decoded, CRC-checked and framing-checked — three checks because each
has a fixture the other two accept, so do not fold them into one. Content
findings are ERRORS in plain `--validate-only`; `--strict` only promotes
warnings. Pillow is load-bearing for validation, so an absent decoder is
one loud error, never a silent skip.

**Deleting art needs the owner's explicit confirmation** (#1257 R4):
present an exact path-level classification first. #1257 deleted nothing —
all 695 previously-unowned paths were retained and declared.

### Compiler (#1258, TEX-2)

Output is **one atlas per ANIMATION** (D-2),
`assets/textures/units/<unit>/atlas/<animation>.png`, beside a generated
`atlas/index.json`.

- **Rows** are the AUTHORED directions in `ATLAS_DIRECTION_ORDER` — the
  engine's own `Unit.Direction` order `S, SW, W, NW, N, NE, E, SE` — five for
  `flip: true`, eight for `flip: false` (D-4), each row index recorded
  explicitly so nothing downstream re-derives the order.
- **Columns** are the max authored frame count. Unequal per-direction
  lengths are real (D-5): the index records each direction's TRUE count,
  shorter rows are padded with transparent RGBA8 zero cells, and no
  padding cell is addressable — `frame_count` is the sole authority.
- **Cells are exact integers**: frame `c` of row `r` at
  `(c*cell_width, r*cell_height)`. A size mismatch is a compile error,
  never an implicit rescale (D-6). Each cell is a byte-for-byte copy of
  its source frame's decoded RGBA8 SAMPLES, alpha included.
- **The index** carries `schema_version` (the format the runtime parses)
  separately from `tool_version`, a documented `direction_order`, and per
  animation its storage format and path, atlas/cell dimensions, columns,
  rows, per-direction row and frame count, `flip`/`fps`/`loop` as the
  engine will hold them (`fps` narrowed to 32-bit), and two `sha256`
  digests: a PER-ANIMATION `source_digest` over that animation's own
  declarations and decoded pixels, and an `atlas_digest` over the atlas's
  decoded CONTENT rather than its file bytes. Per-animation is the point
  — one animation's edit must not invalidate an unrelated atlas (D-12).
- **Determinism and locality.** A clean rebuild under an unchanged
  toolchain is byte-identical; an incremental run writes only on a real
  content difference (an mtime-only touch changes nothing); obsolete
  atlases are removed from that unit's `atlas/` and nowhere else.
- **`--validate-only` is index-aware.** A unit with NO index is valid to
  THIS tool (an uncompiled tree is a legitimate working-copy state) but
  not to the ENGINE. Where an index exists it is REGENERATED from sources
  and compared, so a stale digest, a hand-edited index, a missing atlas
  and tampered pixels all report — and a tampered index cannot certify a
  tampered atlas. Compilation refuses outright on an invalid inventory.

**Every shipped unit's atlases ARE committed** — 116 PNGs + seven
`index.json`, tracked, so a fresh checkout runs with no packer step.
Against D-12's 2x on-disk ceiling (animation sources only): 6.93 MiB of
sources → 4.88 MiB of atlases = **0.70x**, so the
choose-a-distribution-strategy clause is not reached.

Gates: `python3 tools/test_pack_atlas.py` (fixture-based, isolated temp
trees, never touching shipped assets) plus the strict run. Both run
unconditionally in `make ci` and post-merge CI, path-selectively on PRs
via `tools/ci_expensive_gates.py --gate unit-assets`. hspec:
`--match "Asset.UnitInventory"`.

### Budgets (#1262, TEX-7)

`tools/unit_texture_budget.json` is the SINGLE machine-readable source
for two independent budgets. It is hand-edited policy, and a missing or
malformed one is a hard error — never a skipped check that would print a
clean run while enforcing nothing.

- **Images and bindless slots — a hard ERROR.** At most one resident
  image and one bindless registration per COMPILED ANIMATION (D-2), the
  bound derived from each unit's own generated index rather than a frozen
  roster total, so it keeps holding as animations are added. This is what
  makes a reintroduced per-frame registration fail automatically.
  Non-animation textures are excluded BY CONSTRUCTION (outside `atlas/`,
  named by no index), not by an exemption list.
- **Resident memory — a WARNING, so `--strict` is what blocks.** Decoded
  RGBA8 footprint summed over the WHOLE tracked roster
  (`scripts/startup_loader.lua` feeds every `data/units/*.yaml` to the
  loader at boot, so all of it is resident regardless of what spawns),
  compared as `measured × roster_growth_factor > threshold` (strict `>`).
  Currently 101.60 MiB measured, 203.19 MiB projected at 2.0x, against a
  384 MiB threshold **confirmed by the project owner on 2026-08-16** —
  raising it is the owner's call, not a maintenance edit. A breach IS
  D-10's precondition for resuming deferred TEX-5 (KTX2 atlas loading). A
  single-unit `--unit` run deliberately does NOT evaluate this one.

Not to be confused with D-12's on-disk guardrail above: that is
repository size, this is resident memory, measured independently.

### Runtime (#1259/#1260/#1261, TEX-3/TEX-6)

**Every shipped unit uses the compiled path, and there is no other way
for a unit animation to load.** The per-frame representation and its
loader are GONE from the tree, not merely unused.

**Storage is a named SUM with one constructor, so no animation is
half-migrated.** `Unit.Types.Def.Animation` carries an `aStorage` of
`Unit.Atlas.Types.AnimStorage`, now exactly `StorageAtlas` — D-10's
"exactly one resident representation, never mixed within one animation"
is unrepresentable rather than merely enforced, and the named type stays
the seam a later representation would be added at (though TEX-5's KTX2
slots in behind `AtlasStorageFormat` instead). Read frames through the
storage-neutral accessors — `storageFrameCount` / `storageFrameCounts` /
`storageMaxFrameCount` / `storageSampleAt` — never by matching the
constructor. **Buildings are not on this type at all**: they were the
other consumer and are never compiled (D-8), so they live on their own
`Building.Types.BuildingAnimation` — same fields, same per-direction
`DirS`-keyed frame map, byte-for-byte the behaviour they had.

**The index is the whole answer, and failure is failure.**
`Unit.Atlas.Load.loadUnitAtlasIndex` reads, parses, decodes and verifies
EVERY declared atlas before `loadUnitYaml` allocates one handle or queues
one upload; `Unit.Atlas.Index.planUnitAtlasStorage` adds the
YAML-staleness half, including reverse coverage. A missing, incomplete,
stale, unsupported or malformed index refuses the whole unit definition,
naming unit, animation and artifact — no partial registration, and
nothing to fall back to (an ABSENT `atlas/` rejects as surely as a
directory without its index; only a unit declaring NO animations needs no
artifacts). Validation runs in three passes, cheapest first, and BOTH
recorded digests are verified — the three passes, what each digest
catches, and the `pythonFloatRepr` pinning are in
`docs/engine_contracts.md` §Unit animation atlas runtime. Read it before
touching index parsing, the digests, or the upload cache.

**`pickFrame` returns a `FrameSample`, and its arithmetic is FROZEN**
(D-3): the stable handle (#286 — never a slot), the frame's UV endpoints
within that handle's image, the frame's pixel dimensions when the storage
knows them, and the mirror flag. The only storage-dependent step is the
per-direction frame COUNT, which is the index's REAL count and never the
padded column count, so padding is unreachable by construction (D-5).
Non-rendering consumers of a clip's LENGTH read the real counts too:
`Unit.Thread.Command.Pose`'s four pose-transition durations and
`unit.getAnimDuration` (which `scripts/unit_ai_combat_attack.lua`
consumes for attack timing) both go through `storageMaxFrameCount`.

**Cell dimensions size everything.** `frameDimensions` is the one funnel:
an atlas sample answers from its cell, a whole-image sample (the direct
default/directional sprite a T-pose falls back to) falls through to
`rvTextureSizeRef`. Nothing may measure an atlas handle's whole-image
entry where it means a frame — including hit testing, which sizes from
the SAME `pickFrame` sample the renderer draws (`Unit.HitTest.unitHitRect`,
shared by click and box selection).

**Mirroring reflects across the frame's own sub-rect**, never the whole
image — with atlases, `1-u` lands in a different cell.
`UI.Render.renderSpriteBatch` takes the sprite's source sub-rect
(`ussUV`, set by `UI.setSpriteUV`) and mirrors as `u' = su0 + su1 - u`; a
whole-image sprite is the unchanged `1-u`. Anything DISPLAYING a unit's
live frame must use `unit.getFrameSample`, not `unit.getFrameTexture`
(which cannot describe an atlas frame and would draw the whole sheet),
and must publish it with `UI.setSpriteFrame`, which lands texture,
sub-rect and mirror in ONE manager transition — the render thread reads
the manager concurrently, so separate setters leave a window pairing the
new handle with the previous frame's rect.

Atlas slots are registered PINNED to the nearest sampler with one mip
level (D-6), so a runtime `setTextureFilter` toggle cannot start
bilinearly resampling unit art. The upload path's path cache is therefore
policy-aware, and cell UVs sit on exact cell EDGES with no half-texel
inset — see `docs/engine_contracts.md` §Unit animation atlas runtime.

Gates: hspec `--match "pickFrame"` (the whole logical-choice matrix from
one table, each case checked against `expectedChoice` — a restatement of
the documented rule written independently, so an edit to either side
fails), `--match "Unit.Atlas"` (index parsing/validation, the digest
against `pack_atlas.py`'s reference values, mode selection, and real
consumer geometry — `unitToQuad`, `unitHitRect`, `renderSpriteBatch`, a
texel-level atlas-cell-vs-source comparison with the mirrored case, the
pinned-nearest survival of a global filter toggle, the cache's policy
awareness, and a real on-disk fixture tree), and `--match "the real unit
registration boundary"` (drives `registerUnitDefs` against a live
headless engine, real asset pool and real Lua→engine queue, asserting on
the messages actually queued: one atlas upload and one published
`Animation` per animation, no per-frame textures, a rejected index
queueing and publishing nothing, and all SEVEN shipped units through the
PRODUCTION resolver against real YAML/index/art). Roster-wide headless
evidence: `tools/combat_anim_probe.py` (`--roster-only` for the storage
half), which reads the texture-NAME registry (`engine.getTextureHandle`)
rather than `engine.getLoadedTexturePaths()` — the latter is written only
inside the device branch of the batch upload handler and so is EMPTY
headless, where a probe built on it would pass vacuously.

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

That document also owns the OTHER half, which is a separate job and the
one most sessions actually need: **Unit animation atlases: the compiled
runtime path** covers regenerating, validating, inspecting and reviewing
compiled atlases from a fresh checkout, recovery for stale artifacts and
budget breaches, and D-7's restart-to-reload rule. Compiling tracked
source frames invents nothing and needs no external service — do not
confuse it with generating new artwork, which is tracked work with its
own issue, its own PR, and the owner's signoff.

## Platform Notes

- Tested primarily on macOS; works on Linux with minor adjustments
- macOS: GLFW produces unavoidable junk on stdout
- macOS builds get `-DDARWIN` cpp flag and address sanitizer in dev mode
