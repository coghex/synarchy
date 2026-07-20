# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

- **Build:** `cabal build all` (does NOT build test suites — use `cabal build synarchy-test-headless` explicitly)
- **Run:** `cabal run synarchy`
- **Run tests:** see **Testing Tiers** below — pick the cheapest tier that covers the change; don't run the gates as an iteration loop
- **Pre-push gate:** `make ci` runs the exact checks CI runs (`.github/workflows/ci.yml`) — warning-clean (`-Werror`) build of the library/exe + both test suites, the headless hspec suite, `test_audit.py`, `tools/lua_module_budget.py`, `tools/haskell_module_budget.py`, `tools/persistence_inventory_audit.py` (+ its own `tools/test_persistence_inventory_audit.py`), and `world_check.py --quick` — so a green `make ci` predicts a green CI. It uses the default prod profile and your warm `dist-newstyle`, and restores any existing `cabal.project.local` on exit (see `tools/ci-local.sh`). It is not an iteration loop and agents must not run it automatically before opening a PR; run it only when the user explicitly requests a full local CI/pre-push validation.
- **Debug output:** Set `ENGINE_DEBUG=Vulkan,Graphics,etc...` environment variable

## Testing Tiers

Worldgen is the entire cost of the test stack (~10 s per w64 generation;
every non-worldgen test is milliseconds). The tiers exist so iteration
stays in seconds and the expensive gates run once, at the end.

1. **Iteration (seconds–1 min).** Targeted hspec:
   `cabal test synarchy-test-headless --test-options='--match "<describe name>"'`.
   For worldgen-output sanity: `python3 tools/world_check.py --quick`
   (6 tagged seeds × 1 dump, <1 min).
2. **Before reporting done — select only relevant checks.** Start with a
   targeted headless describe (`--match`) that exercises the changed behavior
   and add the focused probe named by the affected subsystem when one exists.
   Run `world_check.py --quick` only for worldgen-output changes; run the
   persistence inventory audit only when its root owners/registry or inventory
   docs change; run a module-budget guard only when changing one of its capped
   modules; and run `test_audit.py` only when changing `world_audit.py` or
   `world_check.py`. Do not run the whole headless suite, the 21-seed world
   check, or `make ci` by default. CI remains the full-suite authority; use
   `make ci` locally only on an explicit user request.
3. **Worldgen-OUTPUT changes only (full tier).**
   `SYNARCHY_FULL_TESTS=1 cabal test synarchy-test-headless` (adds the
   w128 volcano exposure case, +~25 s), then re-capture baselines
   `python3 tools/world_baseline.py` (~7 min) and re-run world_check.
   Remember the save-version bump.
4. **Behavior probes — opt-in, not a default gate.** ~55 headless
   `tools/*_probe.py` scripts each boot a real engine and gate one
   specific system/bug (combat anim, movement, construction, saves,
   physiology, ...) — see `tools/README.md` for the full list. They're
   slow (each pays its own engine-boot cost, more when it generates a
   real world) and aren't run as part of tiers 1–3. Run the ones
   relevant to what you touched, or `python3 tools/run_probes.py
   --only <substrings>` (bare `run_probes.py` runs all of them — low
   tens of minutes, only for a deliberate full sweep). Add `--jobs N`
   (#531) to run up to N probes CONCURRENTLY — each its own engine on a
   unique port — cutting a full sweep's wall-time to ~total/N (bounded
   by the slowest single probe); failures are re-run SOLO afterward
   since parallel contention is what a retry needs to escape. The CI
   gate runs `--jobs 2` (#671, revising #531's original jobs-1 call —
   the solo retry absorbs contention flakes); if CI probe retries turn
   chronic, dropping the gate back to `--jobs 1` is the intended lever.

Baselines (`tools/baselines/`) are **tracked in git** (#421): a fresh
clone/worktree can run world_check directly, and a tier-3 re-capture
lands in the PR diff where reviewers can see the intended drift. Don't
edit baseline JSON by hand — regenerate with `world_baseline.py`.

CI (`.github/workflows/ci.yml`, #436) runs on every PR and push to
master, on Linux: full build with `-Werror` (the #435 warning-clean
state is enforced, dependency warnings excluded), the headless suite,
`test_audit.py`, `tools/lua_module_budget.py`
(#538/#541/#545 — fails if a Lua module split to stay reviewable grows
back past its agreed line budget), `tools/haskell_module_budget.py`
(#787 — the same guard for the Haskell input-thread facade split), and
`world_check --quick` — all blocking when selected. The graphical test-suite
build and `world_check --quick` are path-selective on PRs (the full headless
suite is always blocking); both always run on pushes to master. Worldgen output
proved bit-identical between macOS/aarch64 (where baselines are captured) and Linux/x86_64, so the
tracked baselines are platform-agnostic; a worldgen-output PR that
skips its tier-3 rebaseline fails CI.

On PRs, CI additionally runs a **path-selective, blocking behavior-probe
gate** (#530): `tools/ci_probes.py` diffs the PR against its base and
picks which behavior probes to run — only the ones relevant to the
changed files, the full curated set for a core/unclassified change
(fail-safe: anything the mapping can't classify runs everything), and
**zero** probes for docs/assets-only changes or paths whose probes are
manual-only. Selected probes run via
`run_probes.py --only ... --retries 1 --jobs 2` (probes run pairwise on
the 4-vCPU runner, #671; a failed probe re-runs SOLO once before failing
the PR, absorbing both back-to-back and parallel-engine contention
flakes). Only a **curated CI-eligible smoke subset** is
gated — deterministic probes that are broad and cheap enough for a default
PR gate. Deliberately NOT gated, and left to the manual `run_probes.py`
full run: flaky probes (AI-reaction/
arbitration timing the slower, variable-speed Linux CI runner
destabilizes run-to-run, which within-run retry can't fix), slow/
worldgen-heavy or scenario-heavy ones, narrowly targeted regression
probes, and probes that fail on master today for content reasons. Run
`python3 tools/ci_probes.py --status` (#540) for the
authoritative, always-current list of every registered probe's CI
eligibility — CI-eligible, or manual-only with its reason category
(`flaky`, `base-failing`, `slow/worldgen-heavy`, `scenario-heavy`,
`targeted`, `needs-gpu`, or `unclassified` for a probe simply not yet
reviewed for promotion — never trust a comment enumerating probe
names, they drift).
Growing the eligible set is a follow-up: prove a probe is deterministic,
broad enough, and cheap enough for the blocking gate, then move its key
from `MANUAL_ONLY_REASONS` to `CI_ELIGIBLE` in `tools/ci_probes.py`.
The path→probe map lives in `tools/ci_probes.py`;
a change there re-runs its `--self-test` (a blocking CI step of its
own, which also enforces that every registered probe is classified as
either CI-eligible or manual-only-with-a-reason — no probe can go
unclassified silently).

Conventions that keep this fast — don't undo them:
- hspec worldgen specs **share generated worlds** via
  `Test.Headless.Harness.sharedWorld env seed size plates` (one engine
  for all of them, booted in `Spec.hs`). A spec that mutates its world
  (destroy, edits) must `WorldInit` a private page instead. New
  read-only specs should reuse the canonical `42 64 3` world unless
  they need specific geography.
- `world_check.py` dumps each seed **once**; determinism is pinned by
  the hspec determinism test and at baseline capture. Pass `--runs 3`
  only when chasing a suspected race.
- Don't add new per-spec `WorldInit`s of worlds that already exist in
  the suite, and don't grow the baseline seed list without tagging the
  quick tier accordingly.

**Do NOT use `-f dev` for routine work.** The project is 360+ modules; a full prod rebuild takes ~1.5 minutes (parallelized via `ghc-options: -j` in `cabal.project` — NOT cabal's `semaphore:` jobserver, which deadlocks under concurrent worktree builds, #471), and flag-profile switches force one. The default (prod) profile is what the test suite, dump tool, and binaries are expected to run under, and what every code change should be validated against.

The `dev` flag enables Vulkan validation layers, address sanitizer on macOS, and `ENGINE_DEBUG` plumbing. Reach for it only when actively chasing a graphics or memory bug. When you do, give it its own build dir so the two profiles' artifacts coexist and flipping back is free: `cabal build -f dev --builddir=dist-dev` (every `cabal run`/`test` in that profile needs the same `-f dev --builddir=dist-dev` pair; plain commands keep using the prod `dist-newstyle`). Production builds use `-O2 -optc-O3`.

The executable is built with `-rtsopts`, so RTS behavior can be inspected and tuned at run time without a rebuild — e.g. append `+RTS -s` to a `--dump` run for a GC/allocation summary on stderr, or experiment with `-N<n>` / `-A<size>`. The baked-in default remains `-N -A128M`.

Cost-centre profiling uses the `profile` flag (`-fprof-late` on top of the prod `-O2`): `cabal build exe:synarchy --enable-profiling -f profile --builddir=dist-prof`, then run with `+RTS -p -RTS`. **Must add `-N1`** (`+RTS -N1 -p -RTS`) — the baked-in default `-N` (multi-capability) segfaults the GHC 9.12.2 profiled RTS when combined with this codebase's `parListChunk`-sparked worldgen parallelism (crash inside `pushCostCentre`, see `docs/history/worldgen_timeline_profile_2026-07.md`). `-N1` also loses the ~6.9× parallel speedup on top of profiling's own overhead, so a profiled generation run takes minutes, not seconds. **Don't drive it with `--dump`** — its `waitForInit` watchdog has a hardcoded budget sized for normal prod throughput and will fire mid-`buildTimeline`, at which point `runDump`'s timeout path force-kills every thread (`shutdownThread`'s fixed 10s grace, then `killThread`) and can truncate the profile without any obvious sign in the output. Use `--headless` instead: its main loop has no watchdog of its own, so `world.init` + `world.waitForInit(<seconds>)` over the debug-console TCP lets *you* pick the timeout (re-issue with a fresh budget if it elapses — the engine keeps working regardless), and `engine.quit()` once it reports done triggers a normal, non-killed shutdown that writes a trustworthy final `.prof`. Full recipe in `docs/history/worldgen_timeline_profile_2026-07.md`.

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

### Threading model
The engine uses multiple worker threads communicating via STM (TVar, queues):
- **Main thread:** Vulkan render loop (`app/Main.hs` → `Engine.Loop`)
- **Input thread:** GLFW input handling (`Engine.Input.Thread`)
- **Lua scripting thread:** Runs Lua scripts (`Engine.Scripting.Lua.Thread`)
- **World thread:** Procedural generation and simulation (`World.Thread`)
- **Unit thread:** Actor/unit management (`Unit.Thread`)

`Engine.Input.Thread` (#787) is a thin thread-lifecycle facade —
`startInputThread` + the `runInputLoop` drain/sleep tick. Queue
draining and top-level per-event routing live in
`Engine.Input.Thread.Dispatch` (re-exported through the facade so
`processInputs`/`processInput` resolve the same either way); per-domain
dispatch lives in `Engine.Input.Thread.Keyboard`,
`Engine.Input.Thread.Char`, `Engine.Input.Thread.Mouse`, and
`Engine.Input.Thread.Scroll`. Each file is capped at 500 physical
lines (`tools/haskell_module_budget.py`, wired into CI and `make ci`).

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
`Engine.Scripting.Lua.*` provides a Lua API for game logic. Lua scripts live in the repo-root `scripts/` directory (UI, menus, HUD, world management); `engine.loadScript` paths are relative to the repo root (e.g. `engine.loadScript("scripts/ui_manager.lua")`). The API modules in `Engine.Scripting.Lua.API.*` expose engine functionality to Lua.

`scripts/unit_ai.lua` (#538) is the unit-AI entry/orchestration module
only — the self-registering singleton, the tunables/action-registry
wiring, the per-unit dispatch loop (`tickOne`), and the
init/update/shutdown/onSaveLoaded engine lifecycle. Every domain's
utility/execute bodies live in `scripts/unit_ai_*.lua` submodules
(needs, water, combat + combat_attack, notify, deliver, logistics,
construct, craft, dig, chop, farm — till/plant/harvest, repair, pickup,
medic), each required by the orchestrator and capped at 500 physical
lines (`tools/lua_module_budget.py`, wired into CI and `make ci`).
Shared plumbing (per-unit AI state, the goal layer, distance/footprint
geometry, water-source memory) lives in `scripts/unit_ai_core.lua`; the
inventory→ground→mule materials-sourcing ladder shared by
deliver/construct/craft/repair lives in `scripts/unit_ai_fetch.lua`.
Submodules reach the shared `unitAi` singleton via
`package.loaded["scripts.unit_ai"]` (valid once required from
`unit_ai.lua`, which registers it first) — the same self-registration
pattern the header comment there describes. Public API functions
(`commandMove`, `commandAttack`, `getState`, `till`/`plant`/`harvest`,
repair's priority-flag queries, ...) stay attached to the `unitAi`
table from whichever submodule owns them, exactly as before the split;
only internal action utility/execute/onExit functions move to a
plain `return M` module table, imported by the orchestrator's action
registry.

### UI system
`UI.*` handles focus management, text input, and UI rendering. UI layout and behavior is driven from Lua scripts.

Editable text uses one coordinate contract across Haskell and Lua:
`UI.TextBuffer`, `UI.getCursor`, and `UI.setCursor` use zero-based Unicode
code-point offsets. Lua strings remain UTF-8 byte arrays, so editable widgets
must use `scripts/ui/utf8_safe.lua` for lengths, cursor-prefix measurement,
and slicing rather than `#text` or byte-based `string.sub`. Combining marks
are separate code points; grapheme-cluster navigation is not implied.

Pages (`UI.Manager.Page`) live on one of six `UILayer`s (`UI.Types`),
paint bottom-to-top: `LayerHUD < LayerOverlay < LayerMenu < LayerModal
< LayerTooltip < LayerDebug` (`uiLayerBand` is the single paint-order
source of truth both hit-testing and rendering share). Layer alone only
decides paint/hit-test order; whether a page actually **blocks** pointer
input from reaching whatever paints below it is the separate per-page
`upInputExclusive` flag (#742, `UI.InputOwnership`). A `LayerModal` page
is input-exclusive by default; every other layer defaults pass-through.
When a visible exclusive page exists, it's the topmost one that owns
the **modal boundary**: pointer input (left/right-click, wheel) that
misses every owned control on or above that page is consumed rather
than falling through to a lower page or the game world — empty modal
space blocks a control several layers down exactly like a real control
would. A page that's `LayerModal` for stacking only, not a real dialog
(e.g. `scripts/popup.lua`'s notification cards), opts out via
`UI.setPageInputExclusive(page, false)`. `LayerDebug` (the shell) and
the F8 debug overlay (`scripts/debug.lua`, which hit-tests itself in
Lua via a parallel `tryClaimClick` rather than through `UI.Manager` at
all) are pass-through by design and always paint above any modal, so
their owned controls keep receiving input regardless of what modal is
open; a miss on them keeps searching downward until it either hits a
lower control or reaches the modal boundary.

`UI.isInputBlocked()` is true while any visible page holds the modal
boundary; `scripts/ui_manager.lua`'s `isGameplayInputActive()` folds
this in (alongside the pre-existing `currentMenu`/pause-menu checks) so
ordinary gameplay key handlers, click-selection/tool-claim handling,
camera-zoom scroll, and Shift-wheel z-slice paging all go inert behind
a modal uniformly. Middle-click (camera drag) shares the same
boundary-aware swallow. Escape's own dismiss cascade
(`scripts/init_keys.lua`, closing popups/context menu/logs)
deliberately runs before that gate — the thing that dismisses the
block can't itself be blocked by it.

The F8 overlay and `scripts/debug_anim_panel.lua` both render on a real
`LayerDebug` page (`UI.newPage(_, "debug")`, #742 review round 2 — they
used to sit on `LayerOverlay`, below `LayerModal`'s band, which let
their raw parallel rects claim input from a screen position a visible
modal was actually painted over) but own NO clickable `UI.Manager`
elements of their own — every real click they claim goes through their
own parallel `tryClaimClick`, entirely outside `routePointer`/
`topHitBy`. They're pass-through surfaces that must keep receiving
input above an arbitrary modal, so their validity gate
(`debugOverlay.inGameplayView`/`canShow`) deliberately checks
`uiManager.isGameplayView()` — the narrower, pre-#742 predicate
(current view + pause menu only) — rather than the modal-aware
`isGameplayInputActive()`. A handful of raw per-widget handlers that
iterate every live instance regardless of page, outside
`UI.Manager`/`routePointer` hit-testing entirely (dropdown/randbox
"click outside" close-or-submit), use `UI.isPageInScope(pageHandle)`
instead — it filters out instances on a page the boundary has
excluded, while leaving same-page instances (e.g. a dropdown on the
modal itself, dismissed by clicking elsewhere on that page) working
exactly as before.

Within the page-level boundary above, an individual `UIElement` (#743,
`UI.InputOwnership`/`UI.Manager.Query`) exposes three **independent**
input policies rather than one conflated `ueClickable`+callback gate:
whether it fires a click callback, whether it **blocks pointer** input
(left/right/middle) from reaching whatever is beneath it, and whether
it **captures wheel/scroll** input. Registering a click callback via
`UI.setClickable`+`UI.setOnClick`/`setOnRightClick` still makes a
control pointer-blocking by default (existing widgets are unaffected),
but an element can now opt into pointer-blocking
(`UI.setPointerBlocking`) or scroll-capture (`UI.setScrollCapture`)
with **no callback at all** — the effective predicates are
`elementBlocksPointer`/`elementCapturesScroll`, queryable from Lua via
`UI.isPointerBlocking`/`UI.isScrollCapturing`. A pointer-blocking
element with no callback relevant to a given gesture still consumes it
(`RouteBlocked` — no fake Lua callback fires, but the press can't fall
through to a lower element, page, or gameplay, and stale UI focus still
clears); blocking applies per-element across all three mouse buttons,
so a right-click-only control also blocks a left-click over it. Wheel
routing (`routeScroll`) no longer shares the click-callback machinery
at all — it selects the visually topmost in-scope
`elementCapturesScroll` surface via the same `topHitBy` paint-order
walk hit-testing already uses, so a scroll-capturing container still
wins over its own passive child visuals. This is what let the combat/
injury/unit/event log panels and the settings tab frame drop their old
no-op-click-callback-just-for-wheel-routing workarounds
(`scripts/combat_log.lua`, `scripts/injury_log_panel.lua`,
`scripts/unit_log.lua`, `scripts/event_log.lua`,
`scripts/settings_menu.lua`, `scripts/create_world/log_panel.lua`) for
the explicit contract.

A3 (#744) finishes Phase A: `Engine.Input.Thread.Scroll`'s
`dispatchScrollEvent` now routes an ordinary wheel event and a
Shift-held one through the IDENTICAL pipeline — framebuffer-coordinate
conversion, the degenerate-viewport guard, and `routeScroll`'s
modal-scoped `elementCapturesScroll` search — before either can become
a gameplay action. Pre-#744, Shift-wheel bypassed all of that and went
straight to z-slice paging, so a scroll-capturing UI element under the
cursor was invisible to it and a visible modal's empty space didn't
block it either. Now a capturing element always wins the event
regardless of Shift state (dispatched as `LuaUIScrollEvent`, which
carries the Shift flag so `uiManager.onUIScroll` can tell modified from
unmodified wheel input); short of that, a visible modal boundary
(`isGameplayBlocked`) consumes the event outright — neither zoom nor
z-slice reaches gameplay — and only past both checks does Shift select
z-slice (`onZSliceScroll`) or its absence select camera-zoom
(`onScroll`). Both Lua handlers dropped the `UI.isInputBlocked()`
self-gate they used to carry as a compensating stopgap for exactly the
empty-modal case, now that the engine decides it once, upstream,
instead of the two enforcing the same rule with room to drift apart.

B1 (#745) starts Phase B on top of Phase A: discrete-control pointer
activation and keyboard control focus. Before B1 every discrete
control (button, checkbox, toggle, tab, list row, dropdown, scroll
arrow, hand-built menu control) fired its callback the instant a press
routed to it (`Engine.Input.Thread.Mouse` queued `LuaUIClickEvent`
immediately) — press-drag-away-to-cancel was impossible. Now a press
against a non-`ueDragActivation` control only records a
`UI.ControlActivation.PendingActivation` and fires
`LuaUIPressBeginEvent` (pending-visual signal only, no callback); the
matching release re-runs the SAME `routePointer` decision the press
used, and only activates (fires `LuaUIClickEvent`/`LuaUIRightClickEvent`
with the freshly re-resolved callback) when it still resolves to the
same element — a re-check that covers hidden/deleted/disabled/
detached/a-modal-now-on-top/dragged-outside-and-released-outside as
`UI.ControlActivation.Cancel` whenever any of those are STILL true at
release. F4's `aoOutcome` reports this truthfully (`"accepted"`/
`"rejected"`, the existing vocabulary — see `Engine.ActionOutcome`),
orthogonal to the pre-existing movement-based `"input.click"`/
`"input.drag"` `aoKind` classification. Slider knobs, the slider
track, and scrollbar thumbs opt out via `UI.setDragActivation`
(`ueDragActivation`) — they still fire immediately on press and start
a drag, unchanged from before B1; scroll arrows and every other
discrete control get the new contract for free via `UI.setOnClick`, no
per-widget Lua change needed.

A live re-check alone misses an interruption that's REVERTED before
release (hidden then re-shown, disabled then re-enabled, detached then
re-attached, its page hidden then re-shown, a SEPARATE modal/menu page
appearing then disappearing over the point, or an ANCESTOR hidden then
re-shown) — by release time routing looks identical to press time
again, so the re-check alone would wrongly restore activation. Review
round 9 first closed this with a PER-ELEMENT/PER-PAGE epoch, but that
still missed the separate-modal and ancestor cases (neither mutation
touches the pressed element or its own page directly); round 10
replaced it with a single GLOBAL `UI.Types.upmRouteEpoch`, bumped by
every route-affecting mutation ANYWHERE in the manager. `PendingActivation`
captures the epoch at press time; `resolveActivation` cancels
unconditionally when it no longer matches at release, regardless of
what the live `routePointer` re-check would otherwise say — per the
#745 issue text, "returning inside before release may restore pending
activation" is scoped to drag POSITION only, never to a route-affecting
state change anywhere in the tree.

Round 10's global epoch was itself too broad: bumping on (re)attach
(`UI.addToPage`/`addChild`) as well as detach broke a real production
flow (round 11) — a click that moves keyboard control focus fires
`scripts/ui/focus_indicator.lua`'s `onUIControlFocusChanged`, which
creates and `UI.addChild`s four fresh ring sprites onto the newly
focused element as a purely visual side effect of the SAME click; that
attach bumped the epoch mid-press and wrongly canceled the click's own
activation. Round 11 fixed that by bumping only on the DETACH side
(`UI.removeElement`/`removeFromPage`) plus `UI.setVisible`/
`setClickable`/`hidePage`/`showPage` — never on `addToPage`/`addChild`
— since a detach→re-attach sequence is still caught by the detach's
own bump alone.

Round 11's epoch was STILL global (any `setVisible`/`setClickable`
anywhere bumped one manager-wide counter), and that broke a second real
flow (round 12): ordinary hover decorations. `scripts/ui/toggle.lua`'s
`onHoverEnter`/`onHoverLeave` and `scripts/ui/list.lua`'s
`setHoveredSlot` toggle a highlight sprite's visibility as the cursor
moves over/off a control — completely routine during a press-drag-out-
return-inside gesture — and that toggle, on an element that is a CHILD
(not an ancestor) of the pressed control, wrongly canceled the same
gesture's own activation. Round 12 split the mechanism in two:
`UI.Types.upmPageEpoch` stays a single global counter, bumped only by
`hidePage`/`showPage` for ANY page (page-level visibility genuinely
affects routing everywhere, so this one stays global by design — it's
what catches the pressed control's own page hiding/showing AND a
SEPARATE modal/menu page appearing then disappearing over the point).
`UI.Types.ueRouteEpoch` moved onto each `UIElement` itself, bumped only
by `setVisible`/`setClickable`/detach on THAT element;
`UI.ControlActivation.PendingActivation` captures a snapshot of the
pressed element's own epoch AND every ANCESTOR's epoch (walking
`ueParent` pointers, `UI.ControlActivation.ancestorChain`) at press
time, and `resolveActivation` cancels if that chain no longer matches
at release — so hiding/disabling/detaching the pressed element OR a
real ancestor still cancels, but an unrelated sibling/child's own
visibility churn (a hover highlight, a newly attached decoration) never
touches the chain and can't poison an activation it was never part of.

Round 12's element/page epochs still bumped unconditionally, even when
the requested value already matched the live state — round 13 guards
`setElementVisible`/`setElementClickable`/`showPage`/`hidePage` to only
bump when the value actually changes, so a defensive no-op re-assign
(e.g. re-showing an already-visible page, re-asserting `UI.setVisible`
to what it already is) can no longer poison a pending activation that
was never really interrupted.

Keyboard CONTROL focus (`UI.FocusNavigation`, `upmControlFocus`) is a
second, independent focus system alongside the pre-existing TEXT-input
focus (`upmGlobalFocus`) — a non-text clickable control (anything with
`ueOnClick` and no `ueTextBuffer`) can hold it, distinct from a focused
textbox. `Engine.Input.Thread.Keyboard`'s `(GameInputMode, Nothing)`
branch (reached only when neither shell-text nor UI-text focus is
active, so both retain their pre-existing priority automatically)
drives it directly: Tab/Shift+Tab traverse `focusableElements`
(`UI.InputOwnership.pagesInScope`-scoped, so a modal traps traversal
exactly like it traps pointer routing, while `LayerDebug` stays
reachable above it — same paint-traversal order `topHitBy` hit-tests
with, `UI.Manager.Query.paintTraversalOrder`) with wraparound and a
first-Tab/first-Shift+Tab entry default; Escape clears it; Enter/Space
fire the focused control's `ueOnClick` callback through the IDENTICAL
`LuaUIClickEvent` dispatch a real click uses ("their logical action" —
so every widget family gets keyboard activation for free, no Lua
change); arrow keys step a `ueSteppable` focused control (only a
slider's knob sets this) via a new `LuaUIStepEvent`. Every key the
control-focus layer actually consumes is withheld from `inpKeyStates`
(camera-pan/gameplay polling) the same way a text-focused key already
is, so e.g. arrow-stepping a focused slider doesn't also pan the
camera; an unconsumed key (an arrow with no steppable control focused)
reaches gameplay exactly as before. `upmControlFocus` is validated
(repaired-or-cleared) on every keyboard dispatch, mirroring
`UI.Manager.Focus.validateFocus`'s exact contract for text focus.
`UI.getElementInfo`'s pre-existing `focused` field stays TEXT-focus-only
(every existing consumer — `ui.dumpWidgets`, the F3 oracle — keeps its
meaning); control focus is reported through the new, separate
`controlFocused` field instead of overloading it.
C1 (#747, Phase C child of #741) adds two independent contracts on top
of Phase A's ownership/policy layer: opt-in rectangular clipping and
shared viewport-aware floating placement.

**Clipping** (`UI.Clipping`, pure — no Vulkan/window/Lua engine, see
`Test.Headless.UI.Clipping`): a container opts in via
`UI.setClipChildren(el, true)` (`UI.Types.ueClipChildren`, default
`False`) to clip its DESCENDANTS to its own current absolute bounds —
overflow:hidden semantics; the container's own bounds are never
restricted by its own flag. `UI.Clipping.effectiveClip` is the ONE
shared helper both `UI.Render` (rendering) and
`UI.Manager.Query.isPointInElement` (hit-testing — reached from every
click/hover/scroll/tooltip entry point via `hitsAtPointBy`) consult, so
paint and hit-test can never drift apart — the same discipline
`uiLayerBand` already enforces for z-order. The effective clip is the
intersection of every `ueClipChildren`-opted ancestor's own bounds
(nested clips intersect; no clipping ancestor ⇒ `Nothing`, unclipped);
it's recomputed fresh on every call from live `uePosition`/`ueSize`, so
a move/resize takes effect immediately with nothing cached to go
stale. Rendering clips for real: `UI.Clipping.clipQuadUV` intersects an
element's screen rect against the effective clip and proportionally
adjusts its UV rect, so a box tile, sprite, or glyph straddling the
clip boundary draws only its visible slice (its overflow-expanded
visual rect for boxes) rather than being culled all-or-nothing; a
quad with no overlap at all is dropped. `UI.getEffectiveClip(el)` (a
`{x,y,w,h}` table or `nil`) and `UI.isClipChildren(el)` expose the
state to Lua and to `ui.dumpWidgets`-style introspection
(`pointerBlocking`/`scrollCapturing`-style fields added to
`UI.getElementInfo`'s table: `clipsChildren`, `effectiveClip`).
Floating root-mounted content (dropdown option lists, context menus —
already added via `UI.addToPage`, never as a real child of their
trigger) is naturally unaffected by a trigger's clip, since clipping
only walks REAL ancestors. Adopted on `scripts/ui/list.lua` (the
reusable list widget, transitively covering `scripts/save_browser.lua`
and `scripts/plant_panel.lua`): every visible slot is now a child of a
per-list clipping viewport element instead of a page-root element at
absolute coordinates, so `list.setPosition` collapses to moving the
one viewport (descendants follow automatically, textAlign offset
included) and a resize/rounding edge case can't leave a row visible or
clickable outside the list's own bounds even if virtualization ever
has a gap. `scripts/ui/panel.lua` gained an opt-in `clipChildren`
constructor param (`UI.setClipChildren` on the panel's own box) for any
panel-based screen that nests content via `panel.place`/`placeRow`/
`placeColumn`. Adoption for the settings tabs / create-world content /
event-combat-injury-unit log panels remains a follow-up: those build
their scrollable regions as page-root elements positioned by
hand-computed layout rather than real parent/child nesting, so turning
on the clip contract there needs a real reparenting migration per
panel, not a one-line flag flip.

**Floating placement** (`UI.PopupPlacement`, also pure, see
`Test.Headless.UI.PopupPlacement`): one framebuffer-coordinate
placement algorithm — `UI.placePopup(anchorX, anchorY, anchorW,
anchorH, contentW, contentH, direction)` returns `x, y, flipped` —
replacing two divergent implementations. `direction` is
`"below"`/`"above"`/`"right"`/`"left"` (preferred side, tries the
opposite side if the preferred one doesn't fit, then a final two-axis
clamp regardless of which side won) or `"anchored"` (no directional
preference — place exactly at the anchor point, then clamp; the
context-menu-root shape). `contentW`/`contentH` must be the FULL
interactive size including any scrollbar strip, so the scrollbar stays
reachable too. `UI.fitVisibleRows(preferredCount, rowHeight,
availableHeight)` backs oversized-list row reduction (never below 1).
`scripts/ui/dropdown.lua`'s `openList` now calls both: it fits the
option-list row count against whichever of above/below has more room,
then places with `"below"` (flips to `"above"` automatically) — the
resolved `dd.listX`/`dd.listY`/`dd.listHeight` are stored and reused by
`onClickOutside`'s bounds check instead of assuming a fixed
below-the-display-box stack, so it stays correct even when flipped.
`scripts/ui/context_menu.lua`'s `buildPanel` (root panel) now places
via `"anchored"`; `openSubMenu` places via `"right"` with the root
panel's rect (widened by `SUBMENU_GAP` on both sides so the generic
left-fallback reproduces the same gap) as the anchor — Y stays
row-aligned and only clamped, never flipped, since the preferred axis
there is horizontal. Tooltip placement (`UI.Tooltip.Layout`/`Render`)
deliberately stays on its own separate cursor-relative clamp,
untouched by this change.

**Responsive menu lifecycle** (#748, Phase C child C2, see
`Test.Headless.UI.ResponsiveMenus`): `scripts/ui/responsive.lua` is the
one shared framebuffer/UI-scale notification contract every C2 menu
screen (main, pause, settings, create-world, save browser, loading)
adopts, replacing the per-screen copies that used to exist —
`ui_manager_boot`'s hand-listed `onFramebufferResize` fan-out and
`settings_menu`'s own scaleChanged-only self-rebuild. Gameplay surfaces
(HUD/overlays) are C4 and still use their own explicit forwarding in
`ui_manager_boot.lua`; the contract is ready for C4 to adopt without
duplicating it.

The supported envelope (`responsive.bands`, inclusive on both ends,
contiguous, non-overlapping): 600-900px framebuffer height at
0.5x-1x UI scale; 901-1200 at 0.75x-2x; 1201-1600 at 1x-3x; 1601-2160
at 1.5x-4x. Formal minimum framebuffer: 800x600 (`responsive.MIN_WIDTH`/
`MIN_HEIGHT`). `responsive.classify(fbW, fbH, uiscale)` is pure
introspection only — it never clamps or mutates anything, so an
out-of-envelope combination (e.g. 800x600@4x, or any resolution below
900px height driven by loadDefaults' `1x` at 4K, which itself falls
outside its own 1.5x-4x band) stays fully explorable; screens degrade
best-effort (no crash, no invalid geometry, fixed actions stay
reachable) rather than being blocked or having their stored scale
silently rewritten.

`responsive.register(name, screenModule)` (called once per screen, at
`ui_manager_boot.lua` load time — safe even before a screen's own
`init()` ever runs, since every registered screen's
`onFramebufferResize` only touches its own module fields and no-ops
its rebuild while `uiCreated` is still false) and
`responsive.notifyResize(fbW, fbH)` (the fan-out call, using the
engine's live `engine.getUIScale()`) are the whole contract. It
0x0-minimize-guards uniformly: a zero/negative dimension is never
forwarded to a registered screen's own resize handler (never rebuilds
degenerate geometry), only remembered via
`responsive.getGeometry(name).pendingRestore`; the next call with a
real size rebuilds normally, since every screen's own
`onFramebufferResize` already rebuilds unconditionally off whatever
real size it's given. Re-invoking `notifyResize` with the SAME
dimensions (a UI-scale-only change, e.g. Settings Apply/Save) makes
every already-initialized screen pick up the new scale immediately —
not just whichever screen the change originated from, which is what
`settingsMenu.onApply`/`onSave` now do instead of rebuilding only
themselves.

Settings already followed the fixed-nav/scrollable-content pattern the
envelope needs (tab bar + bottom action buttons positioned once from
the panel's own dimensions, tab content virtual-scrolled within a fixed
frame via per-row show/hide) and create-world's LOG panel already
scrolled the same way — but create-world's actual TAB content (General/
Geology/Timeline) had no scroll or clip of its own at all: `showTab`
only toggled each row's visibility, with every row root-mounted at an
absolute position, so a tab whose rows exceeded the frame's height
(General's 5 rows at the formal 800x600@1x minimum, well over its
~152px frame) rendered rows outside the frame with no way to reach
them — a real gap review round 5 caught and closed.

create-world's tab content now scrolls via real clipping (#747) instead
of settings_menu's older manual per-row show/hide: `createUI` builds
ONE pair per tab key (`"settings"` — General, combining `settings_tab`+
`general_tab` — `"advanced"` — Geology — and `"timeline"`) via
`UI.newElement` — a stationary clipping VIEWPORT
(`UI.setClipChildren(viewport, true)`, `UI.setScrollCapture(viewport,
true)`, fixed at the tab frame's own bounds) holding a movable
SCROLL-CONTENT anchor as its one child. Every row widget across
`settings_tab.lua`/`general_tab.lua`/`advanced_tab.lua`/
`timeline_tab.lua` is parented to that anchor via each widget's #747
`parent` support (`label.new`/`randbox.new`/`dropdown.new`/
`textbox.new` all accept `parent = params.container`, wired straight
into `UI.addChild` inside the widget itself — the row-authoring code in
each tab module is otherwise UNCHANGED, since `contentX`/`contentY`
just become anchor-relative (0,0) instead of page-absolute
(frameX+pad, frameY+pad)) — a row scrolled outside the viewport is both
visually clipped AND un-hittable (`UI.Clipping` backs hit-testing too,
see `UI.Manager.Query.isPointInElement`), so no manual per-row
show/hide is needed; `createWorldMenu.onTabScroll(key, offset)` just
moves the anchor's Y (`UI.setPosition(scrollContentId, 0, -offset *
rowSpacing)`). Each tab module's `.create()` now returns a second value
(its own fixed row count, e.g. `return elements, 5`) so
`createWorldMenu` can size each tab's scrollbar
(`createWorldMenu.createTabScrollbar`) without hand-counting rows at
the call site. `showTab` still runs its existing per-element
type-dispatch visibility toggle (closing an open dropdown, clearing
randbox state) alongside the new per-tab viewport/scrollbar visibility
toggle — ancestor-hiding alone would visually and hit-test-wise hide a
row but wouldn't run a widget's own on-hide side effects.

A mere geometry rebuild (window resize, or a scale change forwarded via
`notifyResize`) must preserve state a semantic re-entry (opening the
menu fresh, Load Defaults) is allowed to reset.
`settingsMenu.createUI(opts)` takes `opts.preserveState` — `init`/
`show`/`onDefaults` omit it (fresh pending + scroll, as before);
`onFramebufferResize` and the scale-change notify path pass
`preserveState = true`, which skips `data.resetPending()` (the
concrete bug this closed — a resize used to silently discard
unapplied Settings edits) and restores each tab's scroll offset
clamped to the rebuilt content's new row count. `createWorldMenu`'s
log scroll offset is likewise clamped-and-restored (and its lines
repainted — `logPanelMod.create` always starts every slot blank)
across every rebuild unconditionally, since nothing there ever wants a
hard reset except `logPanelMod.clear()`'s own explicit call; its NEW
per-tab scroll offsets (one per key) are preserved the same way, via
`scrollbar.setScrollOffset` against each tab's freshly rebuilt
scrollbar. `saveBrowser.onFramebufferResize` restores the
previously-selected save via `list.setSelectedIndex`
(`scripts/ui/list.lua`) rather than `list.selectItem` — the latter
re-fires `onSelect`, which for the save list is a real, consequential
action (loads the save and transitions the whole game), so restoring a
highlight must never replay it.

Keyboard CONTROL focus (#745) preservation across a resize rebuild
(`responsive.snapshotControlFocusName`/`restoreControlFocusName`,
matched by element NAME since handles don't survive a rebuild) was
initially wired for settings_menu/create_world_menu only; review round
5 extended the same wasVisible-guarded snapshot/restore to
main_menu/pause_menu (their menu-item boxes are eligible `ueOnClick`
controls too) and save_browser (whose `createUI()` always deletes and
recreates a fresh page, unlike the other four screens' teardown-only
rebuild — the restore still waits for the fresh page to be genuinely
re-shown before searching `UI.getVisibleElements()`).

A dropdown's width is driven by its OPTION TEXT metrics
(`dropdown.measureOptions`) plus a fixed `minWidth` floor — neither is
a plain `baseSizes` field, so it couldn't be shrunk via the
`contentBase` shared shrink factor (which only ever covered textbox/
slider/checkbox widths). `graphics_tab.lua`'s four dropdowns
(Resolution/Window Mode/MSAA/Texture Filter) now compute ONE effective,
LOCAL `dropdownUiscale` — mirroring `dropdown.new`'s own
`displayWidth + arrowSize` formula exactly, fit via
`responsive.fitScale` against the tab's real `contentW` — and use it in
place of the tab's `uiscale` for every dropdown.new call, so the fit is
correct whether the floor or real text metrics dominate.
`settings_menu.lua`'s own tab BAR has the identical problem one level
up: `tabbar.new`'s frame is correctly sized to `bounds.width`, but each
tab's clickable box width is driven by its own label text + padding,
laid out left-to-right with no fit of its own — `createTabBar` computes
an equivalent local `tabBarUiscale` (real tab-name text width via
`engine.getTextWidth`, fit against `bounds.width`) and passes it only to
`tabbar.new`'s own `uiscale`, mirroring `create_world_menu`'s pre-
existing identical tab-bar fix for its own three tabs.

Review round 6 closed the remaining fixed-size-widget gaps this same
local-effective-scale technique hadn't yet reached: the Input tab's
key/plus buttons (`KEY_BTN_W`/`PLUS_BTN_W`, scaled by uiscale directly,
with no fit of their own) now compute ONE `keyBtnUiscale` fit against
the WORST-CASE row (the action with the most currently-bound keys),
applied uniformly so no row jumps size relative to another; the
Notifications tab's 3-column checkbox grid computes an equivalent
`uiscale` shadow fit against the CHECKBOX-driven natural width alone
(2 column steps + one checkbox) — deliberately excluding header/
`"Pause"`-label text from that fit target, since folding a 9-character
header string's full width into the same shrink ratio crushed the
actually-clickable checkboxes toward 0px; header text may still
overhang a little in truly extreme cases, which is the lesser problem.
`notifications_tab.lua` also had its own latent bug independent of any
narrow-width case: `getTextWidth` was measuring headers at the
UNSCALED `base.fontSize` while `label.new` rendered them at
`base.fontSize * uiscale`, silently under-measuring every header at
any uiscale other than 1 — fixed by introducing a separate
`headerFontSizePx` (the real rendered size) for measurement only,
leaving the unscaled value feeding `label.new` (which applies uiscale
itself) untouched. Settings' own bottom-action buttons had a distinct
bug: shrinking the button BOX width via `factor` without shrinking
`fontSize` by the same `factor` left labels rendering at full size
inside a shrunk box — `settingsMenu.createButtons` now computes
`btnFontSize = base.fontSize * factor` alongside the existing `btnW`.

Round 6 also closed two state-preservation gaps: World Name/Seed are
`randbox` (not `textbox`) controls, so `create_world_menu`'s existing
`textbox.snapshotPage`/`restoreAll` never covered them —
`scripts/ui/randbox.lua` gained its own `getCursor`/`setCursor`/
`snapshotPage`/`restoreAll`, mirroring textbox's exactly, wired into
the same `preserveState` branch. And the shell debug console
(`scripts/shell.lua`) was never registered with the shared
`responsive.notifyResize` contract — a UI-scale Apply/Save (same
framebuffer size, new scale) never reached it at all; it only
rescaled lazily the next time `shell.show()` ran its own `rescale()`.
`shell.onFramebufferResize` now also calls `rescale()` (previously it
only rebuilt geometry when visible, never refreshing the cached scale
values that geometry is computed FROM), and `ui_manager_boot.lua`
registers it via `responsive.register("shell", shell)` — shell isn't a
C2 menu screen, but shares the same live-scale-update need.

Review round 7 found that reserving a control's own fit target (round 6)
doesn't prevent overlap on its own: a row's LABEL sits at the row's own
left edge (`cx`) while the shrunk control right-aligns at
`cx+cw-controlWidth` — even once the control is correctly bounded to
`cw*(1-LABEL_COLUMN_FRACTION)`, the LABEL itself still rendered at the
tab's full uiscale, and a long label ("Tooltip Delay (ms)",
"Periods / era (min–max)") at 4x can be far wider than its own reserved
column, extending into and overlapping the control regardless.
`graphics_tab.lua` and all four create-world tab modules
(`settings_tab`/`general_tab`/`advanced_tab`/`timeline_tab.lua`) now
each compute ONE additional effective, LOCAL `labelUiscale` — mirroring
`dropdownUiscale`/`keyBtnUiscale`'s technique — from whichever row
label text in that tab is widest, fit via `responsive.fitScale` against
the SAME reserved `LABEL_COLUMN_FRACTION` (0.35) column width, and use
it for every row label's `label.new` call (leaving the actual CONTROL's
own uiscale/fit untouched). `settings_menu.lua`'s `createAllTabs` and
`create_world_menu.lua`'s `computeContentScaleFactor` both changed their
own control-fit target from a near-full `contentW*0.9` to
`contentW*(1-LABEL_COLUMN_FRACTION)` for the same reason, on the
control side.

Round 7 also closed a THIRD editable-text-input gap: dropdowns
(Resolution/Window Mode/MSAA/Texture Filter in Settings, World Size in
create-world) are ALSO editable filter inputs — `dropdown.destroy`
(called via `destroyOwned`, ahead of every rebuild) unfocuses and
resets the raw display text back to the selected option, silently
discarding an in-progress (unsubmitted) filter edit exactly like an
unfixed textbox/randbox would. `scripts/ui/dropdown.lua` gained
`getRawText`/`setRawText`/`getCursor`/`setCursor`/`snapshotPage`/
`restoreAll`, mirroring textbox/randbox's exactly (raw filter text via
`UI.getTextInput`/`setTextInput` on the display box — distinct from
`getValue`/`getText`, which report the currently SELECTED option, not
whatever's mid-typing), wired into `settings_menu.lua`'s and
`create_world_menu.lua`'s existing `preserveState` branch alongside
textbox/randbox.

Round 7's last fix reverted round 6's shell registration: registering
`shell` through `responsive.register`/`notifyResize` turned out to
DOUBLE-ROUTE a real framebuffer resize — the engine already broadcasts
`LuaFramebufferResize` straight to every independently-loaded script
(`Engine.Scripting.Lua.Thread.Dispatch`), including `scripts/shell.lua`
directly, so `ui_manager_boot.lua`'s own `onFramebufferResize` calling
`responsive.notifyResize` a second time rebuilt an already-open shell
TWICE per real resize, and bypassed `notifyResize`'s 0x0-minimize guard
for that second path. `ui_manager_boot.lua` no longer registers shell
at all; `settingsMenu.onApply`/`onSave` instead call
`shell.onFramebufferResize` DIRECTLY (alongside the unrelated
`responsive.notifyResize` call for the other registered C2 screens) —
the one case shell's own direct engine broadcast never covers (a
scale-only change, same framebuffer size, no resize event at all).

Review round 8 caught the one dropdown the round-7 label/control-
overlap sweep missed: create-world's own World Size dropdown (`Row 3`
of `settings_tab.lua`) still used the tab's unshrunk `uiscale` — unlike
`graphics_tab.lua`'s four dropdowns, which got a `dropdownUiscale` fix
in round 5. It now computes an equivalent local `sizeDropdownUiscale`
(same `dropdown.lua` displayWidth+arrow formula, fit against the same
reserved control column `cw*(1-LABEL_COLUMN_FRACTION)`), same as every
other dropdown in this codebase.

Round 8 also broadened the layout-matrix coverage: the existing
"screen geometry stays in-frame" describe only sampled six hand-picked
`(w, h, uiscale)` combinations. A new case loops over the REAL
`scripts/settings/data.lua`'s `data.resolutions` list (16 entries)
directly, asserting main/settings/create-world panel geometry at 1x
for every one of them — the scale every configured resolution is fully
supported at except 3840x2160, which is checked too, best-effort, at
its own auto-detected 2.5x default.

Review round 9 found the label/control-overlap sweep (round 7) had
missed the Input and Notifications tabs. `input_tab.lua`'s action rows
reserve a `labelColW` (42% of the tab's content width) for their action
names, but the label itself still rendered at the tab's full uiscale —
fixed with the same per-tab `labelUiscale` technique (widest action
name, fit against `labelColW`). `notifications_tab.lua` had a deeper
bug: its round-6/7 fit only constrained the CHECKBOX geometry, then let
`colStep` expand afterward for header text with no re-fit of the WHOLE
grid — so header text alone could still push the 3-column grid's total
span past the tab's content width, sliding the Log column into the
Category label's own region. Fixed by reserving a
`CATEGORY_LABEL_FRACTION` (0.30) column on the left (mirroring every
other tab's label reservation) and fitting the grid's uiscale using
BOTH the checkbox-driven AND header-text-driven components together —
exactly what `colStep`'s own formula needs — against the remaining
width, so the grid's total span is guaranteed to fit; the category/row
labels get their own separate `catLabelUiscale` fit against their
reserved column. Fixing the grid fit this way re-exposed a checkbox-
specific rounding bug: `checkbox.new`'s internal `math.floor(size *
uiscale)` has no floor-to-1 protection of its own, so passing it
`(base.checkboxSize, uiscale)` separately from the already-floored
local `cbSize` could independently round to 0 even after `cbSize`
itself was floored — fixed by passing the pre-floored `cbSize` directly
as `size` with `uiscale = 1.0`.

Review round 10 found the LAST two gaps: `create_world/bottom_buttons.lua`
had the exact button-label-font bug settings_menu's own bottom buttons
had before round 6 — shrinking the button BOX width via
`computeButtonScaleFactor`'s `factor` without shrinking `fontSize` by
the same factor left every label (Back/Defaults/Generate/Regenerate/
Continue, plus the progress bar) rendering at the unshrunk base size
inside a shrunk box at the formal 800x600@1x minimum. Fixed identically
to settings_menu's fix: `computeLayout` now returns a `btnFontSize =
base.fontSize * factor` alongside the existing `btnW`, threaded into
every button/bar creation call in the file.

`scripts/shell.lua` still lacked a 0x0-minimize guard even after round
7 deliberately un-registered it from `responsive.notifyResize` (to stop
double-routing a real resize) — shell receives `LuaFramebufferResize`
straight from the engine's own direct broadcast regardless of that
registration, so a minimize still reached `onFramebufferResize`
directly and rebuilt an already-visible shell against a degenerate 0x0
framebuffer (`rebuildBox`/`rebuildHistoryDisplay` read
`engine.getFramebufferSize()` directly). Fixed with a simple early
return on non-positive width/height — `shellvisible` is untouched by a
minimize, so the very next real-size resize rebuilds normally on its
own with no separate "pending restore" bookkeeping needed.

Review round 11 found the scale-change fan-out (`responsive.notifyResize`
+ the direct `shell.onFramebufferResize` call) was only wired into
`onApply`/`onSave` — but Settings' Defaults and Back flows can ALSO
change the live UI scale: `data.loadDefaults()` unconditionally calls
`engine.setUIScale` (with its own auto 4K/1440p/1080p detection), and
`data.revert()` conditionally does too (reverting an applied-but-
unsaved scale change back to the on-disk config). Without the fan-out,
every other already-initialized screen and the shell console kept
stale geometry until another resize or reopen. `onDefaults`/`onBack`
now capture `data.current.uiScale` before calling into `data`, compare
after, and fan out identically to `onApply`/`onSave` when it actually
changed. The Back BUTTON's own `onClick` used to inline a bare
`data.revert()` call directly (a second, duplicate copy of the same
"revert then navigate away" logic `onBack()` already encapsulated) —
routed through `settingsMenu.onBack()` instead, so the fan-out fix
lives in exactly one place rather than needing a third copy.

Review round 12 extended the round-8 all-resolution layout matrix
(`Test.Headless.UI.ResponsiveMenus`): it originally only looped
main/settings/create-world across every configured resolution, leaving
pause menu/save browser/loading at a 3-sample check
(800x600/1920x1080/3840x2160) — not the full matrix the issue requires
for every C2 screen. The same loop over `data.resolutions` now also
drives pause/save-browser (their own `panelId`) and loading (its own
fixed progress bar, via a new `barInFrameExpr` mirroring
`panelInFrameExpr`) at 1x for every configured resolution (2.5x
best-effort for 3840x2160, same as the other three screens).

Review round 13 found an invalid-geometry gap at the issue's own
OUT-OF-ENVELOPE exemplar (800x600@4x, outside the supported 800x600's
0.5x-1x band): `settingsMenu.createTabBar`'s `tabFrameHeight` — the
panel height minus several uiscale-scaled chrome terms (title, tab
row, bottom button row, gaps) — went negative once the panel's own
FIXED height (480px, `0.8*600`) was smaller than that scaled chrome sum
alone, and `tabbar.new` passes it straight to `UI.newBox` as a real
(invalid) box height. Since this is explicitly BEST-EFFORT territory
(never crashing/invalid, not required to look good — see the envelope
contract above), the fix is a simple floor rather than a full vertical
reflow: `tabFrameHeight = math.max(20, tabFrameHeight)`. (The
downstream `contentH`/`maxVisibleRows`/scrollbar `trackH` consumers
were already safely floored via their own `math.max` calls — only the
tab frame's own height, passed directly to `UI.newBox`, was
unguarded.)

Genuine text reflow/wrapping is a follow-up, not covered by this pass.

Geometry for headless introspection needs no new surface: a screen's
own tracked panel id via `panel.getPosition`/`getSize`, `UI.
getElementInfo` on any element handle, and `responsive.dump()`/
`getGeometry(name)` for the notification contract's own state are
sufficient — see `Test.Headless.UI.ResponsiveMenus` for the pattern
(bare Lua backend + synthetic texture/font handles, since the full
`ui_manager` boot never reaches menu construction headless — it gates
on `fontsReady`, which needs real font rasterization, gated behind
`Engine.Scripting.Lua.Message`'s `whenGraphical` and so never true
without a GPU).

## Project Layout

- `src/` — Library source (360+ modules)
- `app/Main.hs` — Executable entry point (draw loop)
- `test/` — hspec unit tests (engine core and Vulkan primitives)
- `cbits/` — C code (stb_truetype font rasterization, Lua debug FFI)
- `config/` — YAML config. `*_default.yaml` / `pathing.yaml` /
  `world_gen_default.yaml` are versioned defaults/templates (tracked).
  `keybinds.local.yaml`, `video.local.yaml`, `notifications.local.yaml`
  are local runtime state (gitignored) that the settings UI's Save
  actions write — absent on a fresh clone, where boot falls back to the
  `_default.yaml` template or (notifications) materializes from
  `data/notification_categories.yaml` (#638). `keybinds.yaml`,
  `video.yaml`, `notifications.yaml` are ALSO tracked, but only as a
  pre-#786 upgrade source read once at first boot if a local file
  doesn't exist yet — see "Testing config state headless" below (#786)
- `data/` — Game data YAML (materials, vegetation, flora, units)
- `assets/` — Images and graphical resources
- `scripts/` — Lua scripts for game logic

## Resource Root

Every runtime resource family — `scripts/`, `assets/`, `data/`,
`config/` — is loaded by cwd-relative paths. The executable resolves
ONE resource root at startup (`App.ResourceRoot`, #636) and chdirs
into it, so all relative paths (Haskell and Lua alike) resolve there.
Precedence: `--resource-root <path>` flag > `SYNARCHY_ROOT` env var >
current working directory. Running from the repo root (the normal dev
workflow, incl. every `cabal run` example in this file) therefore
needs nothing; launching the built binary from anywhere else needs one
of the two explicit forms:

```bash
$(cabal list-bin exe:synarchy) --headless --port 9008 --resource-root ~/work/synarchy
SYNARCHY_ROOT=~/work/synarchy $(cabal list-bin exe:synarchy) --dump
```

The root is validated before any dispatch: a nonexistent root, or one
missing a resource family directory, exits 1 with an error naming the
root, where it came from, and each missing path. Note the chdir means
relative *output* paths (`saves/`, config saves) also land under the
resource root, matching a repo-root launch. Gate:
`python3 tools/resource_root_probe.py` (manual-only probe) proves
`--dump` and `--headless` both work from a temp directory outside the
repo.

## Headless Mode & Debug Console

The engine supports a headless mode for automated testing, scripted world generation, and agent workflows. No GPU, no window, no focus stealing.

### Offscreen render mode (#650)

`--offscreen` is the third boot mode: **GPU on, window off** — the full
Vulkan render pipeline drawing to offscreen images with no GLFW window,
no swapchain, no focus steal. Unlike GPU-less `--headless`, the REAL UI
flow runs (loading screen → menus → in-game HUD), `debug.captureScreenshot`
works (always — the image usage is ours to choose, none of #700's surface
negotiation), and `input.*` injection (#644) drives the UI. This is the
unattended/parallel substrate for the playtest harness (#641): multiple
instances run concurrently on distinct ports.

```bash
# Boot offscreen (render size defaults to the video-config resolution;
# --size WxH pins it for deterministic parallel runs)
cabal run exe:synarchy -- --offscreen --port 9018 --size 1280x720 > /tmp/off.log 2>&1 &
until grep -q "READY" /tmp/off.log 2>/dev/null; do sleep 0.2; done
echo "return debug.captureScreenshot('/tmp/shot.png')" | nc -w 10 localhost 9018
echo "return input.click(640, 260)" | nc -w 5 localhost 9018
echo 'engine.quit()' | nc -w 2 localhost 9018
```

Frames pace on a fixed ~60 fps sleep (no vsync exists offscreen). Video
settings that need a window — resolution / window-mode / vsync / MSAA
changes — log a warning and no-op, same as they always have with no
window. Requires a GPU, so the gate is manual-only (`needs-gpu` in
`ci_probes.py --status`): **`python3 tools/offscreen_probe.py`** —
windowless boot → main menu → create-world → real worldgen → in-game
HUD, all verified through screenshots + injected clicks located via the
`ui.dumpWidgets` oracle (never hardcoded coordinates), plus a second
concurrent instance on another port.

### Starting headless

```bash
# Default port 8008
cabal run exe:synarchy -- --headless > /tmp/engine.log 2>&1 &

# Custom port (use when the user may have a graphical instance on 8008)
cabal run exe:synarchy -- --headless --port 9008 > /tmp/engine.log 2>&1 &

# Wait for the debug server to be ready (prints "READY port=NNNN" to stdout)
until grep -q "READY" /tmp/engine.log 2>/dev/null; do sleep 0.2; done
```

### Dump mode (no TCP, JSON to stdout)

Generate a world and dump per-tile data for a chunk region as JSON to stdout. No TCP server, no interaction needed. All logs go to stderr.

```bash
# Dump all layers (default: terrain, material, fluid, ice, ore)
cabal run exe:synarchy -- --dump > world.json 2>/dev/null

# Dump specific layers (whitelist with comma-separated names)
cabal run exe:synarchy -- --dump=terrain,ice --seed 42 --worldSize 32 --region -2,-2,2,2 > ice.json 2>/dev/null

# Full options. --plates is the canonical flag for tectonic plate count;
# --ages is a legacy alias that maps to the same value (the original
# name was misleading — it never controlled number of ages, which the
# timeline rolls randomly).
cabal run exe:synarchy -- --dump --seed 1337 --worldSize 256 --plates 5 --region -5,-5,5,5 > world.json 2>gen.log

# Pipe directly to analysis
cabal run exe:synarchy -- --dump=ice,terrain --seed 42 --region -3,-3,3,3 2>/dev/null | python3 analyze.py
```

**Layer names:** `terrain` (or `elevation`), `material`, `fluid`, `ice`, `ore`. No argument = those five. `slope` is **opt-in only** (`--dump=...,slope`) so a bare `--dump` stays byte-identical to historical output (the worldgen baselines + determinism/audit tools all drive a bare `--dump`).

**Output format:** JSON array with one object per tile in the region. Region coordinates are **chunk coords** (not tile coords).

Per-tile fields (present when layer is enabled):
| Field | Layer | Description |
|-------|-------|-------------|
| `x`, `y`, `v` | always | Global tile coords and v-axis (gx+gy) |
| `terrainZ`, `surfaceZ` | terrain | Raw terrain and max(terrain, fluid) |
| `matId` | material | Top surface material ID |
| `fluidType`, `fluidSurf` | fluid | "ocean"/"lake"/"river"/"lava" or null |
| `iceSurf`, `iceMode` | ice | Ice surface Z and "basin"/"drape" or null |
| `oreId`, `oreTopZ`, `oreCount` | ore | Topmost ore band in the column: material id, its top z, cells of it (null/0 if none) |
| `slope`, `hardness` | slope | Surface tile slope bitmask (bit0=N,1=E,2=S,3=W; 0=flat) and its surface-material hardness. Drives `tools/slope_report.py` (#224 sloping metric). |
| `glacierZone`, `beyondGlacier` | always | World boundary flags |

For cross-seed ore statistics (coverage, depth, deposit sizes) use
`python3 tools/ore_report.py` — it drives the dump itself.

### Debug console (TCP)

Send single-line Lua commands via netcat (use your chosen port). Each line is executed independently — multi-line blocks (if/for/function) must be written as one-liners.

```bash
echo 'return world.getInitProgress()' | nc -w 2 localhost 9008
```

**Return values are auto-serialized:** numbers and strings print directly, Lua tables are serialized to JSON. No manual string-building needed.

### World generation workflow

```bash
# Create a world (pageId, seed, worldSize, plateCount). Optional 5th/6th
# args (#707) attach a player-facing identity — displayName + English
# gloss: display TEXT (trimmed; no save-name rules; omitted/whitespace-
# only name = unnamed page), immutable per page (no setter), persisted
# per page in saves, and independent of both the internal pageId (restored
# verbatim on load, #763 — no remap, no collision rename) and the save-slot
# name. Named saves also expose worldName/worldGloss in engine.listSaves().
echo 'world.init("test", 42, 256, 5)' | nc -w 2 localhost 8008
echo 'world.init("t2", 42, 256, 5, "Aldermoor Deep", "the deep home")' | nc -w 2 localhost 8008

# Read a page's identity — {name, gloss?}, or nil (missing/unnamed/arena)
echo 'return world.getIdentity("t2")' | nc -w 2 localhost 8008

# Option A: Block until done (preferred — timeout in seconds)
echo 'return world.waitForInit(300)' | nc -w 300 localhost 8008

# Option B: Poll progress
# Returns: phase(int), current(int), total(int), stage(string)
# Phases: 0=idle, 1=setup, 2=chunks, 3=done
echo 'return world.getInitProgress()' | nc -w 2 localhost 8008

# Activate the world for queries (required before chunk/tile queries)
echo 'world.show("test")' | nc -w 2 localhost 8008
```

### Query API (returns JSON)

```bash
# River data — array of {source, mouth, flowRate, segments[...]}
echo 'return world.getRivers()' | nc -w 5 localhost 8008

# Chunk info — {loaded, fluidCounts, minSurf, maxSurf, ...}
echo 'return world.getChunkInfo(cx, cy)' | nc -w 2 localhost 8008

# Single tile terrain — returns: surfaceZ, terrainSurfaceZ
echo 'return world.getTerrainAt(gx, gy)' | nc -w 2 localhost 8008

# Surface tile slope bitmask (bit0=N,1=E,2=S,3=W; 0=flat) — what the
# dig + construction (#96) corner-progress displays write
echo 'return world.getSlopeAt(gx, gy)' | nc -w 2 localhost 8008

# Surface tile vegetation id — what the till AI (#333) writes via
# world.setVegAt; vegTilledSoil = 77
echo 'return world.getVegAt(gx, gy)' | nc -w 2 localhost 8008

# Plantable contract (#333) — true iff the tile is tilled soil. Farming's
# planting tool (#335) should call this rather than compare getVegAt to 77.
echo 'return world.isPlantable(gx, gy)' | nc -w 2 localhost 8008

# Single tile fluid — returns: type(string), surface(int)
echo 'return world.getFluidAt(gx, gy)' | nc -w 2 localhost 8008

# Combined surface — returns: surfaceZ, terrainZ, fluidType, fluidSurface
echo 'return world.getSurfaceAt(gx, gy)' | nc -w 2 localhost 8008

# Area fluid scan — array of {x, y, type, surface, terrainZ} (max radius 64)
echo 'return world.getAreaFluid(gx, gy, radius)' | nc -w 5 localhost 8008

# Bulk chunk loading — queue chunks in region [cx1..cx2]×[cy1..cy2], returns count
echo 'return world.loadChunksInRegion(cx1, cy1, cx2, cy2)' | nc -w 5 localhost 8008

# Wait for all queued chunks to load (timeout in seconds), returns remaining count
echo 'return world.waitForChunks(120)' | nc -w 120 localhost 8008

# Camera position
echo 'return camera.getPosition()' | nc -w 2 localhost 8008

# Move camera (chunks load on demand near camera)
echo 'camera.goToTile(gx, gy)' | nc -w 2 localhost 8008
```

### Verifying unit / combat animations headless

You can't see pixels headless, but the engine tracks the animation
**state** — which animation each unit is playing — on the unit thread,
which runs headless. `unit.getInfo(uid)` returns `currentAnim` (the
resolved animation name, e.g. `attack_heavy_RH_dagger`, `combat_idle`,
`injured_death`) and `animStart` (game-time it began). Poll it over time
to verify an animation *timeline* without a GPU — enough to catch
anim-sequencing bugs (a swing that never plays, a corpse stuck in combat
idle, a missing transition).

Turnkey harness: **`python3 tools/combat_anim_probe.py`** — boots a
headless engine, loads defs + AI scripts (the loading screen doesn't run
headless), spawns an attacker next to a target on flat ground, issues
`unitAi.commandAttack`, and prints each unit's `currentAnim` timeline
plus pass/fail checks (a swing animation appeared; a killed unit settled
on a death animation). `--attacker`/`--target`/`--seed`/`--size`/`--port`/`--seconds`.

To drive it by hand: load the AI stack with
`engine.loadScript('scripts/unit_stats.lua',0.1)` (+ `unit_resources`,
`unit_ai`), then `require('scripts.unit_ai').commandAttack(atk,tgt)` and
poll `unit.getInfo(atk).currentAnim`. The AI ticks headless.

### Testing & refining unit movement headless

`scripts/movement_arena.lua` builds no-generator **obstacle courses** on a
flat `world.initArena` world, sculpted with the tile-edit Lua API
(`world.addTile` stacks a column into a cliff/wall, `world.deleteTile`
lowers it, `world.setFluidTile` places ocean/lava barriers or wadeable
river/lake, `world.setSlope` authors a *walkable ramp* — `addTile` always
makes flat tops / cliffs, so `setSlope` is the only way to make a step
walkable). Courses: `flat`, `corner_trap`, `cliff`, `fall_edge`, `ramp`,
`ramp_detour`. Each returns `{name, sx, sy, gx, gy, note}`.

Turnkey harness: **`python3 tools/movement_probe.py [--course NAME]`** —
boots headless, loads defs + the arena module, **neutralises the auto-loaded
`unit_ai` wander tick** (replaces its `update` with a no-op so `moveTo` is
the only thing steering — otherwise the AI wanders the unit off-course),
builds the course, spawns a unit, issues `unit.moveTo`, and prints a
position / activity / pose / `currentAnim` timeline plus per-course
pass/fail checks (e.g. `corner_trap`: reaches the goal and does NOT freeze
in walking — the diagonal-corner-cut stuck-unit bug). `--mode {move,stamina}`/
`--course`/`--unit`/`--speed`/`--seconds`/`--port`; `--list` lists courses.

Note: `startFall` clears the move target on landing (AI re-issues after
recovery), so a unit can't reach a goal across a fall in one `moveTo` —
fall checks assert the fall mechanic + landing z, not arrival.

### Testing construction build jobs headless

Turnkey harness: **`python3 tools/construction_probe.py`** — the #96
gate. Boots headless on a flat arena, designates structure pieces +
a building via `construction.*` (#95), and asserts the construct_job
AI end-to-end: claim (status observable), material sourcing (inventory
→ ground items → technomule), progress accrual, piece placement,
building staking, and dead-claimant claim release. `--phase` runs one
phase; the stake phase runs LAST (the staked portal spawns its roster,
which would contaminate later phases). Structure build costs live in
the pack YAML's `build:` block (`data/structure_packs/*.yaml`).

### Testing derived unit roles headless

Roles (#265) are DERIVED labels, never assigned: the highest work
skill ≥ 30 (with +5 switch hysteresis) names the role — mining→Miner,
woodcutting→Woodcutter, construction→Builder, smithing→Smith, else
Laborer; units with no work skills (wildlife, technomule) have none.
Definitions + weights live in `scripts/unit_roles.lua`; the role
multiplies matching work-action ENTRY utilities (on-role ×1.4,
off-role ×0.7 — never the 6.0 in-progress locks, never survival/
combat/orders). Skills grow with use (dig→mining, fell→woodcutting,
piece placed→construction), so units specialise emergently. Query with
`unitAi.getRole(uid)`; the unit-info header Role row displays it.

Turnkey harness: **`python3 tools/role_probe.py`** — the #265 gate.
Boots headless on a real world (needs a tree) and asserts derivation +
hysteresis + demotion, no-role species, steering (same geometry, a
woodcutter picks the chop job while a miner picks the dig job), and
work-XP growth incl. the legacy-save lazy seeding path
(`grantWorkXP`).

### Testing crafting recipes headless

Turnkey harness: **`python3 tools/craft_probe.py`** — the
#325/#326/#343 gate. Boots headless on a flat arena and asserts the
`craft.*` Lua API end-to-end: catalogue queries (`craft.get` /
`craft.getNames`), the knowledge gate, all-or-nothing input+fuel
consumption, factory-new outputs (condition/sharpness 100), work
stations, and crafter-derived quality. Recipes live in
`data/recipes/*.yaml` (station tag = an operation name below, `inputs`,
optional `fuel`/`knowledge`/`skill`, `work`, `outputs`; loaded via
`engine.loadRecipeYaml`); `craft.execute(uid, recipeId)` runs one craft
against a unit's inventory, station-blind (tests/debug console).

Work stations (#326) are ordinary buildings whose def carries an
`operations:` list (`data/buildings/furnace.yaml` = smelt+repair,
`workbench.yaml` = forge+assemble+repair — repair ops are the hook the
repair epic #299/#301 plugs into). They're built through the normal
construction machinery (materials + build progress). Lua surface:
`building.getOperations(bid)`, `building.findStation(op[,gx,gy])`
(nearest BUILT station on the active page offering op), and
`craft.executeAt(uid, recipeId, bid)` — craft.execute semantics gated
on a Built station offering the recipe's station kind with the unit on
or adjacent to the footprint (Chebyshev ≤ 1).

Craft bills (#329) are per-station standing orders driving production:
`craft.addBill(bid, recipeId[, count[, untilTarget]])` (count
omitted/<1 = repeat forever) validates the station offers the recipe's
operation and returns a bill id; `craft.getBill(s)`/`cancelBill` are
the queue surface (UI = #330), `claimBill(billId, uid, timeout)` /
`releaseBill` / `addBillProgress` / `completeBillCycle` the worker
lifecycle. The queue lives per world page (`Craft.Bills`, engine-side
atomic claims — no Lua claim registry) and persists in saves (v70).
The `craft_job` acolyte action works bills end to end: source inputs +
fuel (inventory → ground → technomule → cargo storage), stand beside
the station, pour skill-scaled work in, `craft.executeAt` (returns the
fresh outputs' instance ids on success), drop exactly those instances
at the station (`unit.dropItemById`; `unit.dropItemToGround` is the
first-match-by-def sibling), grant trade-skill XP (recipe `skill` tag,
default smithing — feeds the Smith role #265). Turnkey harness:
**`python3 tools/craft_bill_probe.py`** — the #329 gate (backend verbs
+ the AI loop + the knowledge gate).

A `skill`-tagged recipe sets output `iiQuality` deterministically from
the crafter (#343): the skill level, blended 70/30 with the knowledge
level when the recipe is knowledge-gated
(`Craft.Execute.craftQuality`); untagged recipes keep the item-def
quality roll. Applies to both `craft.execute` and `craft.executeAt`.

Until-stock bills (#795) are a third persisted `CraftBill` mode
(`Craft.Bills.BillMode`: `FixedCount` | `RepeatForever` | `UntilStock`)
alongside the count-based ones — a real standing order that crafts
`recipeId` while the LIVE ground stock of its first output stays below
a target, rather than #330's original client-side PR #507 snapshot
that converted a target into an ordinary fixed cycle count once at Add
time and never revisited it. `craft.addBill`'s optional 4th arg
(`untilTarget`, a positive integer) requests this mode:
`Craft.Bills.addUntilStockBill` stores `cbMode`/`cbTarget`/
`cbOutputItem` (the recipe's first output, resolved once at add time)
and runs `cbRemaining` at -1 forever, exactly like repeat-forever — an
until-stock bill is never deleted by `completeBillCycle`, it goes idle
(condition-satisfied) instead and becomes claimable again the moment a
later scan sees stock drop back below target. `craft.getBill`/
`getBills` expose `mode` ("fixed"|"repeat"|"until") plus, for `"until"`
only, `target` and `outputItem`. The one authoritative stock scope
(ground-only, unbounded — no fetch-ladder inventory/mule/cargo rungs)
is evaluated LIVE in Lua at two checkpoints, never persisted as its own
flag: `scripts/unit_ai_craft.lua`'s `findCraftBill` (before a fresh
claim) and the `craftExecute` working-phase completion branch (between
finished cycles) both call `unit_ai_fetch.lua`'s
`untilStockSatisfied`/`groundStockCountOf`, the SAME formula
`crafting_panel.lua`'s `groundStockTally()` already used for the
readiness dot and the queue's target/current display — so the AI and
the #330 panel can never disagree on whether a bill is satisfied. This
two-checkpoint re-evaluation (never continuously mid-cycle) is also
what bounds overproduction when two separate bills target the same
output without a dedicated in-flight reservation system: neither can
run forever, since each stops within one cycle of stock reaching its
own target, at the cost of a small, bounded (not unbounded) overshoot
when two bills race. Turnkey harness: **`python3 tools/craft_bill_probe.py`**
(extended for #795) — bill shape, crafting up to (never short of or
past) a multi-item-per-cycle target, replenishing after consumption, a
bill added while already satisfied never being claimed, and the
bounded-overshoot race between two bills on the same output. Save
version 88 (`cbMode`/`cbTarget`/`cbOutputItem` appended after
`cbWorking`); the pure roundtrip lives in
`Test.Headless.Craft.Bills`'s "until-stock (#795)" block, since this
probe's ARENA fixture can't itself exercise a save/load round-trip
(#365).

### Testing power nodes headless (#358)

The power epic's (#357) foundation: `solar_panel` / `high_voltage_battery`
are ordinary 1x1 buildings (`build_work` left at its 0 default — instant,
like the portal) placed through the SAME build-tool ghost-placement flow
as everything else, but item-consuming: `scripts/build_tool.lua`'s
`buildTool.commitPlacement(defName, gx, gy)` routes any def where
`power.isPlaceable(defName)` is true through `power.placeNode(uid,
defName, gx, gy)` — popping one matching item off whichever
`unit.getSelected()` unit carries it (rolling the item back if placement
is rejected) and registering a `Power.Types` node (role + peak watts /
capacity Wh) — instead of the free `building.spawn` every other def
still uses. `power.getNode(nodeId)` / `getNodeForBuilding(bid)` /
`listNodes()` report each node's role + parameters; the registry
persists per-world (`wpsPowerNodes`, v73) keyed by the `BuildingId` it
rides on, reconnecting on load like `Craft.Bills`. Wire adjacency /
network energy balance are #359/#360, not this.

Turnkey harness: **`python3 tools/power_probe.py`** — the #358/#360 gate.
Boots headless on a flat arena, confirms the technomule's starting kit
carries the new items, and drives `buildTool.commitPlacement` (not just
the raw Lua verb) end to end: refusal with no unit selected (an ordinary
building still places free), consumption + role/parameter reporting for
a source and a storage node once a carrying unit is selected, no node
leaking onto an ordinary building, refusal once the carrying unit's
stock is exhausted, and a full save → quit → fresh-restart → load
round-trip reconnecting every node to its building.

### Testing power network connectivity + balance headless (#360)

`Power.Network` is the connected-components + energy-balance sim on top
of #358's nodes and #359's wire (`Structure.Types.SWire`, placed via
`scripts/wire.lua`'s `M.place(gx,gy)`). A "network" is a 4-dir-adjacent
run of wire tiles plus whichever nodes sit on or beside it — connectivity
and a network's generation/drain numbers are recomputed fresh on every
tick/query (nothing about network membership is persisted); only a
battery's own accumulated charge (`pnStoredWh`, save v75) survives a
save. It ticks on the WORLD thread beside `tickWorldTime` (not the
fluid-specific `Sim.Thread`, which mirrors per-chunk cell data for a much
higher-throughput problem than power needs), in the same game-scaled
`dtGame` clock flora/item-temperature already follow — so `world.
setTimeScale` fast-forwards a network's charge exactly like it does a
crop's growth. Solar generation scales by `World.Time.Types.
worldTimeToSunAngle` through a cosine curve (`Power.Network.
solarIntensity`: 1 at noon, 0 at dawn/dusk/midnight). `power.
listNetworks()` / `power.getNetworkForNode(nodeId)` report each network's
`nodeIds`/`generationW`/`drainW`/`storedWh`/`capacityWh`/`powered`;
`power.getNode`/`getNodeForBuilding`/`listNodes` also gained a `storedWh`
field on each node.

Turnkey harnesses:
- **`python3 tools/power_probe.py`** (extended for #360) — wires a
  placed solar panel + battery together, confirms they land on one
  network (and an unwired panel doesn't), fast-forwards the clock and
  confirms the battery's `storedWh` actually rises, then confirms the
  charge survives a save → quit → fresh-restart → load round-trip.
- **`Test.Headless.Power.Network`** (hspec, no engine needed) — the
  #360 gate for the algorithm itself: flood-fill connectivity (incl. the
  4-dir-only / no-wire-no-network cases), instantaneous status
  (Powered/Brownout independent of `dtHours`), charging + capacity
  clamping, and discharge/brownout under a synthetic drain incl.
  proportional multi-battery split.

### Testing powered workshops headless (#361, superseded for crafting by #590)

The first real power consumer. A workshop/building def gains a
`power_drain` (watts) field — data-driven YAML, not a `Power.Types`
node/role. A building is a consumer iff `power_drain > 0` — there's no
separate `requires_power` flag that could fall out of sync with it,
and a power-consuming building never gets a registry entry of its own.
`Power.Network.consumersOn` derives every Built power-draining
building's tile + drain fresh from `BuildingManager` on each tick/query
(mirroring how `positionsOf` derives a node's tile from the building it
rides on), and its drain joins the SAME connected-components pass as
nodes — touching/adjacent to wire like a node, but never BRIDGING two
otherwise-disconnected wire runs the way a node can (a workshop is a
passive tap on the grid, not infrastructure). `power.
isBuildingPowered(bid)` is the gating query (true immediately for a
building with no power_drain); it's false whenever the building isn't
wired to a network at all, same as a Brownout one.

**#590 changed the CRAFTING half of this**: a station's actual load is
now job-dependent (see below) — `power_drain`/`isBuildingPowered` stay
exactly as described here, but purely for a hypothetical future
ALWAYS-ON non-crafting device (lights, etc.); no shipped or crafting
building sets `power_drain` any more, and `validateStation`/`craft_job`
no longer consult `isBuildingPowered` at all.

No shipped building sets a `power_drain` today — `workbench` and
`furnace` are both power-free as *buildings*. `furnace` gained a
powered `smelt` recipe alongside its coal-fired ones in #591 (see
below) — the electrical load lives on that recipe's `power_draw`
(#590), not on the building.

- **`Test.Headless.Power.Network`** (hspec) — the pure `consumersOn`/
  `groupByComponent` folding: drain sums into `drainW`, a consumer not
  adjacent to any wire is dropped (silently unpowered, not an error), a
  consumer with no node-backed network anywhere produces no snapshot
  (vacuously correct — it could never be Powered regardless), a
  bridging node still lets a consumer on either stub it joins land on
  the merged network, `tickPowerNodes` actually discharges a battery
  under real consumer drain, and (#590) `combineConsumers` unions two
  consumer maps (always-on + active-job) summing drain per building.

### Testing job-dependent recipe power draw headless (#590)

Craft/repair stations don't draw a flat building-level wattage —
electrical load belongs to the RECIPE/job being worked, not the
building. A recipe gains an optional `power_draw` (watts, default 0 —
every recipe predating #590) in `data/recipes/*.yaml`
(`Craft.Types.rdPowerDraw`), exposed on `craft.get`/`repair.get`
alongside the other fields (`powerDraw`, always present unlike the
`?`-suffixed optionals).

Claiming a bill is NOT the same as drawing power for it: `CraftBill`
gains `cbWorking` (`Craft.Bills`, save v80), a flag distinct from
`cbClaimant` that's True only while the claimant is actually standing
at the station pouring progress. The craft_job AI flips it via
`craft.setBillWorking(billId, true)` at the walking→working phase
transition (`scripts/unit_ai.lua`) — fetching materials and walking
over draw nothing — and back to `false` in `craftOnExit` (preempted
mid-work); `Craft.Bills.releaseBill`/`completeBillCycle` also clear it
on their own so a released or finished bill never lingers "working".
`claimBill` preserves `cbWorking` across a SAME-holder refresh (called
every AI tick, including throughout "working" — it must not flicker
the flag off) but resets it to False on any takeover by a different
claimant, so a new holder never inherits a stale "working" state from
whoever it replaced. Pausing (`cbPaused`, #330) is orthogonal: per
`claimAvailable`'s existing rule that a paused bill's existing holder
keeps working to the end of the cycle, pausing never touches
`cbWorking` either — a paused-but-still-held bill keeps drawing for
that one remaining cycle. It stops there: `completeBillCycle` (#796)
clears the claimant on a paused bill instead of rolling into another
cycle, and the craft_job AI aborts and releases on its own if it
notices the pause before it ever reaches "working" (fetching/walking
never drew power to begin with), so a paused bill can't run — or
draw — indefinitely.
`Power.Network.activeCraftConsumersOn` derives a station's tile + drain
fresh from every bill that is BOTH claimed AND `cbWorking`, whose
recipe demands power — an unclaimed bill, a claimed-but-not-yet-working
one, or a zero-power recipe, all contribute nothing; two simultaneously
worked power-drawing bills at the same station sum their loads. Every
network tick/query (`World.Thread.Power`, `power.listNetworks`,
`isRecipePoweredAt`) unions this with the old `consumersOn` via
`Power.Network.combineConsumers`, so a future always-on device and an
active craft job on the same network both count toward Brownout.

`power.isStationPoweredForRecipe(bid, recipeId[, billId])` is the
job-aware gating query: looks the recipe up itself, is trivially true
for a zero-power recipe at ANY station (wired or not — an unknown
recipe id also resolves to 0 draw here, so it's trivially true too;
callers that need "unknown recipe" to be a hard refusal go through
`validateStation` instead), and for a positive-power recipe checks the
station's network status against its FULL current demand: the optional
`billId` — the bill THIS check is for, if any — is EXCLUDED from
`activeCraftConsumersOn`'s fold (a new `Maybe BillId` parameter) before
this call's own `drawW` is added back in exactly once, via
`HM.insertWith` summing (never overwriting). This matters both ways: a
plain overwrite would silently DROP any other simultaneous consumer at
the same station (a second active bill, or an always-on device) and
report Powered when `power.listNetworks` would correctly show Brownout;
a plain unconditional add (no exclusion) would DOUBLE-COUNT the common
case where the check is for a bill that's already registered its own
draw (the craft_job AI checking or completing its own job). Passing the
right `billId` is what makes both directions correct at once — the
craft AI passes its own `job.billId` (both from its per-tick working-
phase gate and its cycle-completion `craft.executeAt`/`repair.repairAt`
call, which also gained an optional trailing `billId` argument for this
— `craft.executeAt(uid, recipeId, bid[, billId])`); a bare/ad-hoc call
with no bill in play (debug console, tests, `repair.repairAt` — repairs
aren't bill-driven at all) passes `Nothing`, in which case the query
just sums with whatever's genuinely already active anywhere else on
that station. `Engine.Scripting.Lua.API.Craft.validateStation` (shared
by `craft.executeAt` and `repair.repairAt`, see `Repair.hs`) threads the
same `Maybe BillId` through in place of the old `isBuildingPowered`; the
`craft_job` AI's `"working"` phase gates its progress-pour loop the same
way, resetting its elapsed-time accumulator on every stall so outage
time is never credited once power returns — pours no progress while
browned out (idle, not failed, not released), and resumes on its own
once the network can cover it.

Turnkey harness: **`python3 tools/power_workshop_probe.py`** — the
#590 gate (rewritten from its original #361 form). Registers its OWN
throwaway "forge" workshop (no `power_drain`) + TWO probe recipes
(`power_draw: 150` and `power_draw: 300`, mirroring how
`craft_bill_probe.py` injects a temp recipe YAML) rather than flipping
a shipped building/recipe, so it exercises the mechanism fully isolated
from every other probe's fixtures — the second recipe exists solely to
prove two DIFFERENT recipes' demand at the same station sums correctly.
Boots headless on a flat arena and asserts: `isBuildingPowered` stays
trivially true throughout (no `power_drain` on the station); unwired
refusal (`craft.executeAt` "no power"); wired-but-idle (no bill
claimed) reports `drainW == 0` even at midnight; flipping to noon makes
`isStationPoweredForRecipe` true and `craft.executeAt` succeed, but
`drainW` STAYS 0 with no bill claimed — full generation, idle station,
zero demand; a manually driven bill (bypassing the AI) shows `drainW`
stays 0 on claim alone, jumps to 150W only once marked working, and —
while that 150W bill is still working — a BARE `isStationPoweredForRecipe`
check (no billId) for the second, 300W recipe at the SAME station
correctly sees the combined 450W demand against the 400W panel (0Wh
stored) and refuses, proving the exclude-and-add-back logic sums with
an already-active consumer instead of displacing it; `drainW` then
STAYS 150W while the first bill is paused (an existing holder keeps
working through a pause), and drops to 0 once un-marked working or
released; the `craft_job` AI end-to-end shows `drainW` stays 0 through
fetch/walking and only reads 150W once the AI reaches "working" (its
`craft.executeAt` call passing its own `job.billId`), zero progress
while browned out at midnight, completion once flipped to noon, and
`drainW` back to 0 once the bill is done and gone; and a real
fast-forward with a bill held claimed AND marked working by hand
(deterministic — AI off) shows the battery's `storedWh` both rise
(daylight, generation > active drain) and fall (night, active drain
with no generation). `Test.Headless.
Craft.Bills`'s "working (#590)" block covers `cbWorking`'s pure
transitions directly: default-False, `setBillWorking`, preserved across
a same-holder refresh, reset on a different-claimant takeover, cleared
by `releaseBill`/`completeBillCycle`, and untouched by `setBillPaused`.

### Testing the electric furnace + machine shop headless (#591)

The first shipped content to actually use #590's recipe-level power
draw. There's only ever one furnace — `furnace`
(`data/buildings/furnace.yaml`) — not a separate powered/fuel pair:
`smelt_steel_electric` (`data/recipes/smelting.yaml`) is a plain
`power_draw`-carrying alternative sitting alongside the existing
coal-fired `smelt_steel_*`/`smelt_bronze_*` recipes at the SAME `smelt`
station, same ore input and bar yield, no `fuel:` line. The coal
recipes are untouched and keep working with no power at all.

`machine_shop` (`data/buildings/machine_shop.yaml`) is the genuinely
new building — a dedicated `"machine"` station operation, deliberately
NOT folded into `workbench`'s existing (currently recipe-less)
`"assemble"` operation, so the power requirement stays exclusive to
the new station and `workbench` keeps its current power-free role
untouched. `data/recipes/machining.yaml` has two `power_draw`-carrying
recipes: `machine_wiring` (bronze_bar → wiring) feeds
`machine_electric_motor` (steel_bar + wiring → electric_motor) — the
first player-fabricable path to a good that was previously only
`make: factory` spawn/loot stock.

Turnkey harness: **`python3 tools/machine_shop_probe.py`** — the #591
gate. Boots headless on a flat arena, builds a real `furnace` and a
real `machine_shop` through their normal materials + build-progress
machinery (not synthetic fixtures — `tools/power_workshop_probe.py`
already covers the #590 mechanism itself in isolation), and asserts:
both new recipes load with `power_draw > 0` and no `fuel` line;
existing coal-fired smelting still succeeds on a completely unwired
furnace (the regression check that #591 is additive); both new
recipes refuse with "no power" while unwired; wiring each to its own
solar panel + battery and flipping to noon lets `craft.executeAt`
succeed for `smelt_steel_electric`, `machine_wiring`, and
`machine_electric_motor` in turn, with fresh output appearing each
time; and a manually-driven bill (claim → mark working → add progress
→ complete, AI off throughout for determinism) on `machine_wiring`
shows progress frozen at midnight and completing once flipped to noon.
`building.listDefs()` is also checked for `machine_shop`'s real sprite
path (not a missing or reused placeholder) and its `"machine"`
operation.

### Testing tilling headless (#333)

The farming epic's (#331) first step, unblocked once #332 (flora growth
runtime) landed. `till_tool.lua` is a DF-style two-click designation
tool mirroring `chop_tool.lua` (#97) / `mine_tool.lua`: first click
anchors, second commits via `till.designate(pageId, x1, y1, x2, y2)`,
filtered to tillable tiles **at the anchor's surface z** (a farmed field
is flat ground, unlike chop's slope-spanning forest sweep) — no fluid
on top, no flora instance on the tile, not already tilled. `till.*`
otherwise mirrors `chop.*` exactly: `getDesignationAt` /
`getDesignationCount` / `nearestDesignation` / `cancelDesignation` /
`setDesignateTexture`. Completion goes through a new **generic**
primitive rather than a till-specific one:
`world.setVegAt(pageId, gx, gy, z, vegId)` (mirrors `world.setSlope`,
read back with `world.getVegAt(gx, gy)`) sets the tile's vegetation id
via the `WeSetVeg` edit-log entry, so it survives chunk eviction and
save/load like every other edit. The till AI (`unit_ai.lua`'s
`till_designation` action, mirroring `chop_designation`'s claim/walk/
equip/work structure) flips a tilled tile to
`World.Vegetation.vegTilledSoil` (id 77) on completion — one texture
regardless of soil type or climate (the epic left "tilled texture by
soil type" open; simplest default, variants are a follow-up). No tool
item is required to till (also left open by the epic; a tiller item is
a future speed-up, not a gate), and no new work skill/role — that's
farm AI's (#336) job, not this designation layer's.

Tilled soil's "reads as plantable" acceptance is a formal contract, not
just a raw id: `World.Vegetation.isTilledSoil` is the one classification
predicate (mirrors `isBarrenMaterial`/`isWetlandSoil`), exposed to Lua as
`world.isPlantable(gx, gy)`. Farming's planting tool (#335) and any
future consumer should call this rather than compare `world.getVegAt`'s
raw id to 77 — a soil-type-variant tilled texture (the still-open
follow-up above) only needs `isTilledSoil` to grow to match.

The dedicated push/tiller animation (#517, PR #639) has shipped: the
till AI's `till_equip_anim`/`till_work_anim` (`scripts/unit_ai_tunables.lua`)
are wired to `standing_to_pushing`/`pushing`, replacing the earlier
`standing_to_holding_shovel`/`shoveling` reuse-until-real-art stand-in.
The toolbar icon, designation marker, and tilled-soil ground texture
are still functional placeholders (correct size/alpha conventions,
matching sibling assets) pending real pixel art.

Till/plant/harvest's helpers ride as fields on the existing `unitAi`
module table (`unitAi.till.*`, plain assignments) rather than one
top-level `local` per helper — originally a workaround for
`unit_ai.lua`'s single-chunk 200-local ceiling, which #538's module
split (below) has since resolved, but kept as-is across that split so
the public shape (any caller poking `unitAi.till.claims`) doesn't move.

Turnkey harness: **`python3 tools/till_probe.py`** — the #333 gate.
Boots headless on a real generated world (natural ground cover needs
worldgen) and asserts: designate / query / cancel; a fluid-covered tile
is excluded from designation; the designation survives save → quit →
fresh-restart → load; an acolyte (real unit_ai stack, no tool required)
autonomously claims, walks to, and tills the designated tile —
`world.getVegAt` confirms the flip and the designation clears;
`world.isPlantable` is false before and true after; and a re-sweep of
the same rectangle skips the now-tilled tile (idempotent).

### Flora growth runtime (#332)

Flora growth is **derived** state: the calendar date advances (midnight
rollover on the world thread — it used to be frozen forever), and a
plant's age / life phase / annual stage / reseed generation derive from
the absolute world day plus its deterministic placement fields. No
per-instance mutable state, nothing new in saves (the date already
persists); chunk eviction/regen can't lose growth. `fiHealth` =
placement-time habitat fitness and scales growth speed. Species whose
annual cycle authors a `fruiting` stage (red_raspberry) harvest only in
that window — unharvested fruit is lost at senescing; species without
one (white_clover) stay open year-round. The #94 regrowth-timer map is
still the only mutable flora state.

```bash
# Calendar date — dayOfYear drives the annual cycle, absoluteDay is the growth clock
echo 'return world.getDate("main_world")' | nc -w 2 localhost 9008
# Per-instance derived growth on a tile: array of {id, age, health, phase,
# stage, generation, dead, harvestable, regrowthRemaining}
echo 'return world.getFloraGrowthAt(gx, gy)' | nc -w 2 localhost 9008
# Poke the clock (queued world command — poll getDate for it to land)
echo 'world.setDate("main_world", 2, 7, 21)' | nc -w 2 localhost 9008
```

Turnkey harness: **`python3 tools/flora_growth_probe.py`** — the #332
gate. Registers a max-tolerance `probe_berry` species (real raspberries
are climate-gated and absent from many spawn regions), generates a real
world, and asserts: the date ticks under the game clock, growth state
derives per instance, the fruiting window gates `harvestFlora`/
`findHarvestableFlora` (clover stays open off-season), ages and reseed
generations advance under `setDate` jumps, and the growth clock
survives save → load.

The growth window gates **bare (food) calls only**: `harvestFlora(gx,gy)`
and `findHarvestableFlora(gx,gy,r)` without a tag, plus the
`getFloraAt().harvestable` flag, which mirrors exactly "would a bare
harvestFlora yield here" (the query/action contract). Tagged calls
(#97 — the chop AI's `"wood"`) deliberately skip the window, and the
chop-claim check keys on `regrowthRemaining` + `tags`, NOT
`harvestable` — a designated tree must stay choppable as a sprout or
standing dead. Per-instance gated state lives in `getFloraGrowthAt`.

### Testing location discovery headless (#780)

Placed locations gain a persisted per-page "discovered" flag —
undiscovered until a player-faction unit enters the definition's
`discovery_margin` halo around its authoritative bounds (#777). A
location becomes discovered the instant a player-faction unit's tile
falls inside `Location.Bounds.expandBounds discovery_margin bounds`
(seam-aware, inclusive of the boundary); "player-faction" is the
current player-control contract — `uiFactionId == "player"` (portal-
spawned units use this tag), so hostile/wildlife/neutral/debug
factions never discover a location just by standing in it. The pure
transition detector is `Location.Discovery.findDiscoveries`; the
engine-owned tick that mutates state and emits the player event is
`World.Thread.Discovery.tickLocationDiscovery`, run every world-thread
tick for **every loaded page** (not just the visible one — a
player-controlled unit can be simulated on a hidden page) and
independent of the pause flag, so a freshly loaded (auto-paused) save
with a unit already standing in the margin discovers it immediately.
Discovery is a one-way, idempotent transition (`undiscovered →
discovered`); stamping a location's geometry (#424) or spawning its
contents (#90) never discovers it, and discovering it never touches
those flags — all three booleans persist independently in
`WorldGenParams` (`wgpLocationStamped` / `wgpLocationContentsSpawned` /
`wgpLocationDiscovered`).

A successful first transition emits one player event
(category `location_discovery`) via the normal
`engine.emitEventForUnit`-style surface — `"Discovered: <label>"`,
carrying the discovering unit's id — so it shows up through
`engine.getEventLog()` like any other event. Repeated entry (leaving
and returning, or simply still standing there next tick) never emits a
duplicate. Because the tick runs on every loaded page rather than only
the active one, every discovery event also names its source page
(`peSourcePage` / the query's `page` field) — but only carries the
location's anchor tile as clickable coords
(`Engine.PlayerEvent.Emit.emitEventFullOnPage`) when the discovery
happened on the page that's CURRENTLY active; a hidden-page discovery
omits coords entirely so its popup can never silently pan the visible
page's camera to a different page's tile.

```bash
# Per placed location: cx, cy, gx, gy, id, discovered, and (when the
# id has a registered def) bounds + discovery_margin.
echo 'return world.listPlacedLocations()' | nc -w 5 localhost 9008
```

Turnkey harness: **`python3 tools/location_content_probe.py`** (extended
for #780; manual-only/scenario-heavy, see `ci_probes.py --status` — it
needs real worldgen and boots the engine four times) — on top of its
existing ruin content-spawn coverage, asserts stamping + content-
spawning do not discover a ruin, a hostile unit standing on it doesn't
either, a player-faction unit within the margin does (flipping
`world.listPlacedLocations()`'s `discovered` field and emitting exactly
one attributed `location_discovery` event), leaving and returning emits
no duplicate, and the discovered flag (but not the one-time event, since
player events are per-session and never saved) survives a save → quit →
fresh restart → load. The Hspec gate is the `Location discovery` group
(`cabal test synarchy-test-headless --test-options='--match "Location
discovery"'`): `Location.Discovery.findDiscoveries` against bounds/
margin/faction/page/seam-wrap/idempotency scenarios (pure, no engine),
`WorldGenParams`'s discovery-state save round-trip and independence from
the stamped/contents-spawned flags, and (needing a bare
`initializeEngineHeadless` env, no world/unit thread) `tickLocationDiscovery`
itself mutating state and emitting the real player event.

### Logging: event / combat / injury

Three log panels share UI machinery and feed off three streams:
- **Event log** — engine ring via `engine.getEventLog()`. Emit with
  `engine.emitEvent(cat,text)` / `emitEventAt(cat,text,gx,gy)` /
  `engine.emitEventForUnit(cat,text,uid[,gx,gy])`. The last tags the
  event with a unit so the per-unit log can filter it; `getEventLog()`
  returns that `uid`. A category only lands in the log if its
  notifications YAML has `log: true` (e.g. `survival_critical/warning`
  do; `unit_event` is popup-only).
- **Combat log** — `combat.drainEvents()` (engine `combatEventsRef`),
  per-battle tabs, per-LAYER prose (`scripts/injury_log.lua` `hitLine`).
- **Injury log** — `injury.drainEvents()` (engine `injuryEventsRef`,
  reuses the `CombatEvent` shape: victim in `target`, kind
  `"fall"|"injure"|"death"`). **NON-combat only** by design (falls,
  hazards, wound-caused deaths). Producers: `Unit.Thread.Movement`
  (fall), `unit.injure`, and Lua `injury.emit(uid,kind[,cause,part,
  woundKind,severity])`. Per-VICTIM tabs, per-WOUND prose
  (`scripts/injury_log.lua` `eventLine`). NB: a streaming consumer
  (the panel) drains these — don't manually drain in a test while the
  panel script is loaded, or you'll race it.

The PANELS are GUI (not headless-verifiable), but the data path is.
Turnkey harness: **`python3 tools/injury_log_probe.py`** — boots
headless and checks the injury stream roundtrip, `unit.injure` →
event, and `emitEventForUnit` → `getEventLog().uid` (gating), plus a
best-effort real-fall test. `--no-fall` skips the movement phase.

### Testing config state headless (#638, upgrade path #786)

`config/video.local.yaml`, `config/keybinds.local.yaml`, and
`config/notifications.local.yaml` are gitignored local runtime state
written by the settings UI's save paths (`engine.saveVideoConfig`/
`saveKeybinds`/`setNotificationOverrides`), not versioned source. Boot
falls back to the tracked `config/video_default.yaml`/
`config/keybinds_default.yaml` templates (`Engine.Core.Init.resolveConfigPath`)
when the local file is absent; `config/notifications.local.yaml` has no
separate default file — it self-materializes from
`data/notification_categories.yaml`'s `default_settings`, same as
before #638.

**Correction (#786):** #638's original claim that the pre-#638 tracked
`config/video.yaml`/`config/keybinds.yaml`/`config/notifications.yaml`
were "left untouched (untracked via `git rm --cached`, not deleted)" on
an update was **wrong**. `git rm --cached` only spares the file in the
PR author's own working tree; a downstream `git pull` that finds no
local modification to one of those paths deletes it as an ordinary
tree change — no different from any other file removed upstream. That
already happened for real on the paths above: `config/video.yaml` and
`config/keybinds.yaml` are gone from disk on the checkout that carried
#638 forward, and `config/notifications.yaml` only survived because its
loader self-materializes fresh registry defaults on the next boot when
the file is absent — silently replacing the old drifted values
(`ui_scale: 1.5`/`vsync: false`; `building.popup`/`unit_warning.pause`
flipped from the registry's `false`) rather than preserving them. That
loss is irretrievable for any checkout that already pulled past it —
#786 does not attempt to recover it — but #786 does fix the pattern
going forward:

- `config/video.yaml`, `config/keybinds.yaml`, and
  `config/notifications.yaml` are tracked again, deliberately, with
  content equal to the versioned default/registry (never a real
  player's values — see the notes above on why the old tracked content
  was itself accidental drift, not a real setting worth re-enshrining).
  Their only purpose is to guarantee a *readable legacy file always
  exists* for a direct updater's first boot, so `migrateLegacyConfig`
  (below) always has something to react to instead of reaching a
  silently-already-deleted path. Because the tracked content is neutral,
  this is invisible for anyone who never touches these files: migrating
  it produces the same effective values as falling back to the default
  directly (the exact pattern `config/notifications.yaml` already used
  pre-#638 — it always self-materialized a file on first boot too).
- A player who actually saved through the *old* settings UI (pre-#786)
  has a **locally modified** copy of one of these paths. `git` itself —
  not this PR — is what protects that: a plain `git pull`/checkout
  refuses to silently discard an uncommitted local modification to a
  tracked file, so their real values stay on disk until the update is
  resolved by hand, at which point `migrateLegacyConfig` picks up
  *their* real content (not the tracked neutral placeholder). This is
  the only case where "preserve the player's real values" is actually
  meaningful, and it works precisely because the tracked content
  choice above is irrelevant to it.
- Save actions target the new `*.local.yaml` paths, which are never
  tracked, so gitignoring them can never repeat this exact loss for a
  *new* config path going forward.
- `Engine.Core.Init.migrateLegacyConfig` is the one-time upgrade step:
  at boot, if a legacy-named file is present on disk and the
  current-format local file is not, it decodes the legacy file against
  the SAME `FromJSON` type its subsystem's own loader uses (passed in
  via a `Proxy` — `VideoConfigFile` / `KeyBindingConfig` / `OverridesFile`)
  and, only on success, copies it to the local path and logs the fact.
  Validating against the real target schema (not just "is this
  syntactically valid YAML") matters: a legacy file that parses as YAML
  but is missing a field the real loader requires (e.g. video's
  `resolution`) must not be copied and logged as a successful
  migration — that would silently mask the load failure the loader
  would otherwise report AND permanently block any future migration
  attempt, since the existence gate below would see the copied-but-
  unusable local file and never look at legacy again.
- The migration only ever runs when the local path is absent — a single
  existence gate that makes a second boot a no-op (idempotent) and
  guarantees a real local file always wins over a stale legacy one,
  without needing to compare timestamps or content.
- A legacy file that fails that schema-aware decode (malformed, partial,
  schema-incomplete, or unreadable) is left alone and logged rather than
  copied, falling back to the versioned default/registry exactly like a
  missing legacy file — it can never destroy or mask an already-valid
  local file. Notifications' own `OverridesFile` parser treats a missing
  `categories` key (or any of a category's three fields) as valid —
  the exact same tolerance `loadOverrides` already applies when
  self-materializing a fresh file — so "schema-incomplete" only rejects
  a legacy notifications file for video/keybinds-style reasons
  (unparseable YAML); an empty or field-sparse-but-well-formed one
  still migrates successfully, unlike video's genuinely required
  `resolution` field.
- A fresh clone has no local file (only the tracked, neutral legacy
  file), so its first boot migrates that neutral content — same
  effective values as the versioned template/registry either way.

Turnkey harnesses:
- **`python3 tools/config_state_probe.py`** — the #638 gate. Backs up
  any local *and* legacy config files present (restored afterward),
  boots headless to confirm a simulated fresh clone (neither local nor
  legacy files present) falls back to the versioned templates,
  exercises the three public save paths (asserting they write the
  `*.local.yaml` paths and never resurrect a legacy path), and asserts
  `git status --short -- config .gitignore` is clean both before and
  after.
- **`python3 tools/config_migration_probe.py`** — the #786 gate. First
  proves the upgrade against the REAL committed tree (no fixtures): the
  tracked legacy files resolve to the versioned default/registry and
  migrate into `*.local.yaml` on a plain boot — the direct-updater path
  itself, not just the mechanism in isolation. Then drives the
  mechanism directly with hand-placed fixtures carrying distinct
  non-default values (a probe can't reproduce the interesting `git
  pull` case itself, since git blocks it rather than executing it — see
  above): a legacy-only boot migrates all three subsystems' values and
  writes the matching local files; a second boot is idempotent even
  after the legacy file is edited further; an existing local file wins
  outright over a legacy file present from the start, and migration
  never touches either file in that case; and a malformed (or
  schema-incomplete) legacy file falls back to the versioned default
  without ever creating a local file, and — separately — without
  touching an already-valid local file sitting next to it.

Also covered by pure hspec coverage (no engine): `cabal test
synarchy-test-headless --test-options='--match "config"'` includes
`Test.Headless.Core.ConfigState`'s direct tests of `resolveConfigPath`,
`migrateLegacyConfig` (present/absent/syntactically-malformed/
schema-incomplete legacy, idempotency, never overwriting an existing
local file), and the notification-overrides materialize/round-trip
contract.

### Shutdown

```bash
echo 'engine.quit()' | nc -w 2 localhost 8008
```

### Tips for agents

- **NEVER launch `cabal run synarchy` or `cabal run exe:synarchy` without `--dump`, `--headless`, or `--offscreen`** — otherwise it opens a graphical window that steals focus from the user (`--offscreen` renders with the GPU but creates no window, so it is safe)
- **Prefer `--dump` for testing** — it's self-contained, no TCP needed, outputs JSON to stdout. It already implies headless (no window, no GPU).
- If you must use `--headless`, use `--port 9008` (or another non-8008 port) so you don't conflict with the user's graphical instance
- **NEVER use `pkill -f synarchy`** — this kills the user's GUI window. Instead:
  - Shut down your instance with `echo 'engine.quit()' | nc -w 2 localhost 9008`
  - Track your PID: `HPID=$!` after the background `&`, then `kill $HPID` if needed
- **worldSize 256** generates in ~2 minutes; 512 takes much longer
- **Prefer `loadChunksInRegion` over camera movement** for bulk testing — it's faster and more reliable:
  ```bash
  echo 'return world.loadChunksInRegion(-10, -10, 10, 10)' | nc -w 5 localhost 9008
  echo 'return world.waitForChunks(120)' | nc -w 120 localhost 9008
  ```
- Chunks also load on-demand when the camera is nearby — call `camera.goToTile(gx, gy)` and wait before querying tiles at that location
- `world.show("name")` must be called before tile/chunk queries work
- The debug console is **single-line only** — use semicolons for compound statements: `local r=world.getRivers(); return #r`
- All table return values (getRivers, getChunkInfo, getAreaFluid) auto-serialize to JSON
- If port is busy and you're sure it's a stale headless instance, use `kill $HPID` or `lsof -ti:9008 | xargs kill`

## Save / Load

**Persistence contract:** [`docs/persistence_contract.md`](docs/persistence_contract.md) is the
authoritative contract for what a save represents, how every piece of engine/Lua state is
classified (persist exactly / persist as reference / rebuild / reset to default / exclude), and
how the classification stays enforced as the codebase grows — see
[`docs/persistence_state_inventory.md`](docs/persistence_state_inventory.md) for the field-by-field
classification and `tools/persistence_inventory_audit.py` (wired into `make ci`) for the automated
guard that fails when a new root state owner or registered Lua save module has no classification.
Read the contract before changing anything that adds state to `EngineEnv`, `WorldState`,
`World.Save.Types`, or `scripts/lib/save_modules.lua`'s registry.

**Session snapshot (#758, save-overhaul A3):** `World.Save.Snapshot` is the immutable,
validated in-memory capture of an entire session — `SessionSnapshot`/`PageSnapshot`,
built by the pure `captureSessionSnapshot` (globals + a `[PageSnapshot]` list in, `Either
[SnapshotError] SessionSnapshot` out — no IO, callable straight from hspec with synthetic
managers). This is deliberately NOT `SaveData`/`WorldPageSave` (`World.Save.Types`): those
stay the positional cereal WIRE SCHEMA, append-only and version-bumped on every layout
change; the snapshot has no `Serialize` instance and no such constraint, since it's built
once, validated once, and handed to a serializer rather than written to disk itself.
`World.Save.Snapshot.Adapter`'s `snapshotToSaveData` is the TEMPORARY bridge that encodes a
captured snapshot through the unchanged save format (`currentSaveVersion` is untouched by
this — B1 owns the real new envelope); it fabricates the handful of v88-only fields the
snapshot deliberately excludes (`wpsTimeScale` always `1`, `wpsToolMode` always
`DefaultTool`, `sdEnginePaused` always `True` — all three are load POLICY per the contract,
never captured gameplay state) and duplicates the one global live camera's zoom/facing into
every page the same lossy way v88 always has (no per-page zoom/facing exists in that
format). `World.Thread.Command.Save.WriteWorld.handleWorldSaveCommand` still owns every
`readIORef`, but now calls `captureSessionSnapshot` and — critically — releases the #757
barrier's CAPTURE LOCK (`Engine.Save.Barrier.releaseCaptureLock`, transitioning to a
non-terminal `SaveEncoding` phase) as soon as the snapshot is captured, validated, AND fully
FORCED, **before** the disk write runs; previously the barrier stayed held through the
entire encode+write, and before that (review round 2) the transaction was declared
terminally complete (`finishSave`) at the same too-early point, so a disk-write failure
after release could no longer flip the outcome and `engine.getSaveStatus()` would report a
save that was never written. Fixed two ways: (1) `releaseCaptureLock` only unblocks
`captureLocked` for other state owners — `ssOutcome` stays `Nothing` (`saveInProgress`
still `True`) until the real disk write resolves via `finishSave`/`failSave`, so a write
failure surfaces as a genuine `SaveFailed`; (2) `World.Save.Serialize.encodeSaveData` (pure
— splits what used to be one `saveWorld` into an encode step and a `writeSaveFiles` I/O
step) is forced via `evaluate` **before** `releaseCaptureLock` runs, not after — cereal
cannot produce a `ByteString` without visiting every field, so this either fully succeeds or
throws right there, catching a bug hiding in an unevaluated thunk (the snapshot/adapter's
record fields are only forced to WHNF, not deeply — Strict/`HashMap.Strict` forces the
outer shape and each value at ONE level, never recursively) as a capture failure while the
barrier still blocks everyone, instead of letting it surface later during the write after
owners have already resumed. `World.Save.Serialize.writeSaveFiles` (the actual disk I/O,
run after the release) delegates to `World.Save.Storage.publishGeneration` (issue #762,
persistence-overhaul C1 — see "Atomic save storage" below), whose every phase is wrapped
in `try` and converts any exception to a `Left PublishFailure` naming the phase (review
round 2 follow-up, generalized by #762 from a single directory-creation/`BS.writeFile`
wrap into the full write-validate-publish-rotate transaction): this runs on the world
thread AFTER the capture lock has already released, so an uncaught exception there would
otherwise escape all the way to `World.Thread`'s top-level crash handler instead of
reaching `failSave` — crashing the whole world thread AND leaving the barrier stuck open
(`saveInProgress` permanently `True`) rather than surfacing a `SaveFailed` outcome. A
validation, encode, OR write failure fails the transaction and writes nothing beyond
whatever the failed write itself left behind — no partial `SaveData` is ever serialized as
a reported success. Pure coverage:
`Test.Headless.Save.Snapshot` (construction, all ~10 referential-integrity validators,
camera representation, adapter field-mapping, and a "full-encode forcing" pair proving
`encodeSaveData` forces a deferred exploding thunk buried in a page's edit list that
capture/validation silently pass — no engine boot); `Test.Headless.Save.Barrier` covers
`SaveEncoding`/`releaseCaptureLock` directly (unblocks `captureLocked` without completing
the transaction, ignores a stray ack during that phase, `finishSave`/`failSave` finalize
correctly after it). Real multi-thread coverage:
**`python3 tools/save_barrier_probe.py`** — extended for #758 to also prove a mutation
issued the instant the barrier releases (i.e. once the save file has already appeared)
never reaches the save that already captured it, that a later save captures that mutation
as its own, distinct boundary, and (review round 2 follow-up) that a genuine disk-level
write failure (the save's own directory path pre-occupied by a plain file) surfaces as a
real `SaveFailed` via `engine.getSaveStatus()` — never crashing the world thread or wedging
the barrier open — with an immediate follow-up save proving it's accepted right after,
alongside the pre-existing #757 checks (owners fully acknowledged before capture, a
pre-boundary World→Sim→World fluid writeback surviving the save, and the loaded session
staying paused). `NoPersistablePages`/`ActivePageMissing`/
etc. (`SnapshotError`) deliberately do NOT include a craft-bill-station or power-node
dangling-reference check — a demolished station's bills "lingering, visible + cancellable"
is documented, tolerated gameplay behaviour (see the craft-bills section above), not
corruption; hard-failing on it would reject otherwise-valid saves. Likewise no "Lua capture
succeeded" check: an empty Lua-blob map is indistinguishable, from the snapshot's own data,
between a real capture failure and a legitimate Lua-less engine-only save (exercised by
`Test.Headless.World.Identity`'s save/load-mapping test, which drives `WorldSave` directly
with an empty blob map).

**Save envelope (#759, save-overhaul B1):** the on-disk `world.synworld`
file is a tagged, checksummed COMPONENT ENVELOPE, not a raw positional
`[header][SaveData]` blob. `World.Save.Envelope.Codec` is the generic,
pure codec (`encodeEnvelope`/`decodeEnvelope`) — magic + a framing
version + a length-prefixed, checksummed manifest of component
descriptors (id, schema version, required/optional, offset, length,
its own checksum), then each component's payload bytes laid out
back-to-back in canonical (component-id-ascending) order. Integrity
uses FNV-1a 64-bit (`World.Save.Envelope.Types.fnv1a64`) over the
manifest and over each component's payload — corruption detection
only, explicitly not authentication. Decoding validates the COMPLETE
structure (magic, framing version, every documented allocation limit,
duplicate ids, the required/optional contract, payload bounds,
per-component checksums, forbidden trailing bytes) before any
component's payload is interpreted; a component the writer marks
optional can still be a hard requirement for THIS reader (an unknown
component the writer marked required is rejected outright; a known
component missing from the file is rejected even if the writer never
marked anything required at all). `World.Save.Envelope` wires this
codec to two components: `"metadata"` (just `SaveMetadata` — so
`listSaves`/`engine.listSaves()` never decodes a save's gameplay
payload at all) and `"session"` (the complete, unchanged `SaveData`,
the same positional `Generic Serialize` shape v88 always used — a
TRANSITIONAL component B2 will later split into independently-owned
Haskell components). The envelope's own framing version
(`World.Save.Envelope.currentEnvelopeVersion`) is independent of the
session component's schema version (still `currentSaveVersion`,
`World.Save.Types`) — bump the former only if the FRAMING contract
itself changes incompatibly, the latter exactly as before whenever
`SaveData`/`WorldPageSave` gains, loses, or reorders a field. A
pre-#759 flat file is rejected outright (its first 16 bytes never
parse as a valid envelope header) with no heuristic positional
decoding attempted — v82-and-earlier saves are a clean break, not a
migration target. `world_gen.yaml` is GONE: `world.synworld` is the
sole authoritative file for a save generation now that generation
params live in the "session" component like every other gameplay
field. Pure coverage: `Test.Headless.World.Save.Envelope` (the "save
envelope" describe block) — round trips, canonical ordering +
determinism, the required/optional contract, every structural
corruption/limit rejection path (with the phase + offending component
identified), a real `SaveData`/`SaveMetadata` round trip, the
metadata-without-gameplay-decoding property, component-version
rejection, the pre-#759 clean break, and a frozen tracked-bytes
fixture decoded independently of any encoder call in that same test.

**Save components (#760, save-overhaul B2):** B1's single transitional
`"session"` component (which wrapped the whole positional `SaveData`) is
GONE. Gameplay state now rides as a set of independently versioned,
Haskell-owned components inside the SAME B1 envelope
(`World.Save.Component.*`): `core-session` (game time, active/visible
pages, live camera, the GLOBAL item/building/unit allocators),
`texture-palette`, `lua-state` (the transitional opaque Lua blob map —
superseded by #761/B3's dynamic per-module `"lua.<module>"` components
below; `lua-state` no longer exists in the codebase), `world-pages`
(the page-set AUTHORITY: identity, gen params,
dates/clocks, map mode, per-page camera), `world-edits`,
`world-activity` (designations/flora/crops/ground/spoil), `buildings`,
`units`, `unit-sim`, `craft-bills`, `power-nodes` — plus the unchanged
`metadata` component. The canonical in-memory form is
`World.Save.Snapshot.SessionSnapshot`; each component converts to/from a
slice of it, is version-dispatched on decode, self-validates, and
declares its dependencies + owner in the ONE authoritative registry
(`World.Save.Component.saveComponentRegistry`; ids in
`World.Save.Component.Types`). The envelope framing version
(`currentEnvelopeVersion`) is unchanged — component evolution uses
per-component schema versions, NOT a global save-version bump.
Assembly (`World.Save.Component.assembleSnapshot`) decodes + self-
validates every component without touching live state, checks page-set
consistency across components against `world-pages`, reassembles the
`SessionSnapshot`, then runs the whole-session invariants
(`validateSessionSnapshot` — active/visible page, orphan sim owner,
allocator bounds) plus a manifest-metadata agreement check — any single
failure yields NO partial snapshot. Save encoding is now
`World.Save.Envelope.encodeSessionSnapshot` (the WHNF-forcing point the
#758 barrier relies on, replacing the old `encodeSaveData`); load is
`decodeSessionEnvelope` → reconstruct the `SessionSnapshot` from
components → bridge it into the still-unchanged world-thread load path
via `snapshotToSaveData`. `SaveData`/`WorldPageSave` are therefore no
longer any wire contract — only that transitional in-memory load bridge.
The audit (`tools/persistence_inventory_audit.py`) now cross-checks that
every persistent component owner in `persistence_state_inventory.md` §10
is registered and vice-versa. Pure coverage:
`Test.Headless.World.Save.Components` (the "save components" describe) —
registry well-formedness + dependency ordering/cycle rejection,
per-component round trips + version dispatch + malformed-payload +
component-local invariants, page-scoping mismatch, the production
encode↔decode round trip reconstructing the exact snapshot, every
assembly cross-validation (metadata mismatch, orphan sim, allocator
collision, missing active page, one-bad-component-no-partial), and a
frozen multi-component tracked-bytes fixture.

Save format version: see `currentSaveVersion` in `src/World/Save/Types.hs` (bumped frequently — don't trust any number written down here; since #760 it versions only the transitional `SaveData`/`WorldPageSave` load bridge, not any wire contract — gameplay components carry their own schema versions). Saves live under `saves/<name>/` — `world.synworld` (binary) is the SOLE authoritative file a load/list reads, but is never written to directly (see "Atomic save storage" below); a slot may also carry a `world.synworld.prev` recovery copy, invisible to listing/loading except as an automatic fallback. There is no companion `world_gen.yaml` (removed by #759 — generation params now live in the `world-pages` component); a stale one left over from an older save is removed the next time that slot publishes successfully (#762).

**Atomic save storage (#762, persistence-overhaul C1):** `World.Save.Storage`
owns the ONLY code path that ever touches a save slot's files on disk — a
crash, power loss, disk-full condition, or write failure can no longer
truncate or corrupt the sole copy of a generation the way a direct
`BS.writeFile` (the pre-#762 `writeSaveFilesUnsafe`) could. It receives
only already-encoded envelope bytes, the `SaveMetadata` they should decode
back to, a slot directory, and a slot name for diagnostics — no live
gameplay state, no snapshot-capture participation (it runs after the #758
barrier has already released). `publishGeneration`, `selectLoadGeneration`,
AND `World.Save.Serialize.listSaves` (which walks `saves/` directly
rather than through either of the other two, so it applies the same
check itself before ever reading a slot's bytes) all first refuse to
operate at all through a slot directory that is itself a symlink, OR
whose IMMEDIATE PARENT (in production, `saves/` itself) is
(`rejectSymlinkedSlotDir`, `PhaseUnsafePath`) — otherwise a pre-existing
symlink at either level would be silently followed by directory
creation, the temp candidate, every rename, cleanup, and even a plain
listing alike, reading from and publishing into and deleting from
wherever it points, outside `saves/` entirely (requirement 12). This
deliberately checks only ONE level up, not every ancestor to the
filesystem root: `pathIsSymbolicLink` inspects just a path's own final
component (ordinary POSIX `lstat` semantics), so it can never misfire on
an unrelated OS-level symlink further up the resource root's own path
(e.g. macOS's `/tmp` → `/private/tmp`). `rejectSymlinkedSlotDir` is built
from a single exported primitive, `rejectSymlinkedPath` (reject one exact
path that is itself a symlink) — the SAME primitive `decodeGenerationFile`
uses to check `world.synworld`/`world.synworld.prev` THEMSELVES (a slot
directory being safe says nothing about a FILE inside it also being a
symlink — a scenario `publishGeneration` itself never produces, since an
atomic rename replaces a destination symlink's own entry rather than
writing through it, but which requirement 12 still asks to be rejected if
found), and `listSaves` uses AGAIN for the exact same reason, since its
directory-listing read path (`loadDirEntry`/`tryPreviousListing`) is
entirely separate from `selectLoadGeneration`/`decodeGenerationFile` and
does not automatically inherit that check.

`publishGeneration` is a write-validate-publish-rotate transaction: write
the candidate to a uniquely-named temp file in the SAME slot directory
(`System.IO.openBinaryTempFile`, so concurrent publishers can't collide on
a predictable name and the eventual publish rename stays on one
filesystem); flush + durably `fsync` it
(`System.Posix.Unistd.fileSynchronise`); re-read it FROM DISK (never trust
the in-memory encode alone) and fully decode + deep-force it, applying the
same validation bar the load path enforces (structure, checksums, every
component, `checkWorldCount`), PLUS confirming the re-read metadata is
EXACTLY (`≡`, every field) the metadata this publish intended — not just
the slot name/timestamp, so a caller-side mix-up between two saves'
metadata can't slip through on two matching fields alone; only once
validated: if a previous generation already exists, stage it out of the
way under a freshly-claimed unique name FIRST (`stageOldPrevious`,
`PhaseStalePrevious`) rather than letting the rotate rename destroy it
outright — requirement 5's "older generations are removed only after the
new publication is durable" means the displaced generation must stay
fully intact on disk (just under a different name) until AFTER the
durability boundary, not disappear the instant it's rotated out of the
`world.synworld.prev` slot; `fsync` the directory so that staging handoff
is itself a confirmed-durable recovery point; THEN atomically `renameFile`
the current authoritative generation into the now-free
`world.synworld.prev` slot, `fsync` again; then atomically `renameFile`
the candidate into `world.synworld`, `fsync` a FINAL time — every one of
the three renames gets its own directory sync immediately after it, not
one combined sync at the very end, so each intermediate handoff is a
confirmed-durable recovery point in its own right before the next step
ever runs; THEN report success — never before; ONLY NOW remove the staged-
away old generation and any other recognized stale artifact. Every phase
(`World.Save.Storage.StoragePhase`) is wrapped in `try` and reported as a
`PublishFailure` naming the phase, slot, and path, so a failure text like
`engine.getSaveStatus()`'s `SaveAborted` outcome always identifies exactly
where a save failed. A best-effort cleanup sweep (the just-staged old
generation, any other leftover temp/staged artifact from an earlier
interrupted publish, a stale `world_gen.yaml`) runs only AFTER the
durability boundary, so a cleanup failure is a non-fatal warning, never an
overall save failure. Every transient name this module generates uses a
dot-free template (`"world-synworld-tmp"`/`"world-synworld-stale"`) so
`openBinaryTempFile`'s numeric suffix always trails the template instead
of splicing before a `.`-preserved extension; cleanup recognises ONLY a
name that is the template immediately followed by a digit
(`isOwnedArtifactName`) — never a mere shared prefix — so a real but
oddly-named user file dropped in the slot directory (e.g.
`world-synworld-tmp-notes`) is never swept (requirement 12/13). Durability
means: candidate data + every rename's directory entry have been handed to
`fsync` successfully. On Linux this is a real crash/power-loss guarantee;
on macOS, plain POSIX `fsync` does NOT guarantee the drive's own write
cache reached physical media (only the much slower `F_FULLFSYNC` does) —
this module deliberately uses ordinary `fsync` on both platforms rather
than pay that cost on every save, the same trade-off most non-database
desktop applications make. Windows is out of scope (see Platform Notes).

Load-source selection (`selectLoadGeneration`, used by both
`World.Save.Serialize.loadWorld` and `listSaves`) classifies WHY a
candidate failed to decode
(`World.Save.Envelope.GenerationFailure`/`isRecoverableEnvelopeError`):
absent, truncated, bad framing, a checksum failure, or the file itself
being a symlink — the shape routine storage corruption (or something
outside the transaction entirely, since `publishGeneration` never leaves
a symlink at either path itself — `renameFile` replaces a destination
symlink's own entry rather than writing through it) actually takes — is
`GenerationCorrupt` and falls back to a fully valid `world.synworld.prev`
(itself independently re-checked, so a symlink at BOTH paths still
correctly reports a hard failure rather than reading through either);
a structurally coherent but unsupported/incompatible file (unknown
component version, failed content/assembly validation, INCLUDING
`checkWorldCount`'s empty-pages check — a content-validation failure, not
storage damage, even though in practice `assembleSnapshot`'s own
`validateSessionSnapshot` already rejects an empty page set earlier in the
same decode) is `GenerationIncompatible` and is reported directly, NEVER
triggering a fallback — silently rolling back to an older generation would
hide a real compatibility problem instead of reporting it. A load never
combines components across generations (exactly one generation's
`SaveData` is ever returned) and is read-only (a recovered load never
rewrites or promotes `world.synworld.prev`); a later explicit save is what
publishes a fresh authoritative generation. `loadWorld`/`listSaves` log a
recovered selection loudly (`logWarn`); `engine.listSaves()` additionally
exposes a `recovered` boolean on a listing recovered this way (no
save/load UI change required to consume it). A pre-#759 legacy flat file
(`saves/<name>.synworld`, no slot directory) has no previous-generation/
temp-file/rotation concept and either decodes cleanly or is rejected
outright, exactly as before #762 — but it DOES get the same symlink
containment check both `loadWorld` and `listSaves` apply everywhere else
(`rejectSymlinkedSlotDir` on the flat file's own path, which — via its
immediate-parent check — also covers a symlinked `saves/`), since that
one piece of #762's hardening is orthogonal to the generation/rotation
machinery the legacy format otherwise never touches.

`listSaves` itself only decodes the envelope structure/checksums and the
`"metadata"` component for each candidate (`decodeSaveEnvelopeMetadata`/
`Classified`) — the SAME depth #759 requirement 4 established before
#762 existed, deliberately never a full per-save component decode (that
would cost proportionally to every listed save's full gameplay payload,
exactly what #759 designed listing to avoid). #762 does not deepen this:
it only adds the previous-generation fallback ON TOP of that pre-existing
metadata-only depth, so a slot can still list as normal even when its
authoritative generation would actually fail to fully load — a problem
confined to a gameplay component OTHER than `"metadata"` — precisely as
it already could before this issue.

Pure/integration coverage: `Test.Headless.World.Save.Storage` (the "atomic
save storage" describe) — first/second/third publish retention (incl. the
staged-and-cleaned-up first generation leaving no stray file behind),
candidate re-read+validation (corrupt bytes, a full-metadata mismatch —
including a same-name/timestamp-but-different-seed candidate) never
touching an existing authoritative generation, every forceable failure
phase (directory pre-occupied by a plain file, a read-only slot directory,
a symlinked slot directory or its immediate parent, a rotate/publish-rename
target blocked by an existing directory) reporting the correct
`StoragePhase` without destroying anything, a publish from a "previous-only"
recovery state (authoritative missing) leaving the sole previous generation
completely untouched rather than staging it away, stale-temp/staged-
previous/stale-`world_gen.yaml` cleanup never touching an unrelated file
(incl. one merely sharing a transient-name prefix without its digit
suffix), `selectLoadGeneration` across every constructed on-disk state
(missing/truncated/bad-framing/checksum-corrupt authoritative, a
structurally-valid-but-empty-pages authoritative, a stray leftover temp
file, a symlinked slot directory or its immediate parent, a symlinked
authoritative or previous FILE itself (incl. both at once, proving a
hard failure rather than reading through either), the exact intermediate
state a crash immediately after staging would leave, an incompatible-
but-checksummed authoritative that must NOT fall back, neither generation
valid) proving a recovered load is read-only and never selects a partial
candidate, and `listSaves`/`loadWorld` refusing to list or read through a
slot (directory-format OR a legacy flat file) reached via a symlinked
slot directory, a symlinked `saves/` itself, a symlinked legacy file, OR
a symlinked authoritative/previous generation FILE (incl. listing
falling back to the previous generation exactly like load selection
does, and skipping the slot outright when both generation files are
symlinks). Real multi-thread/real-restart
coverage: **`python3 tools/save_storage_probe.py`** — against an ISOLATED
temporary resource root (never a real player's `saves/`) — two real saves
to one slot (publish, then retain-as-previous), restart-and-select across
the same constructed on-disk states via the resumed session's live camera
position (an unambiguous, real, in-session observation of which
generation actually loaded), `engine.listSaves()`'s `recovered` flag, and
a real disk-level write failure (the #758-established pre-occupied-path
trick) reporting its `StoragePhase` through `engine.getSaveStatus()` with
the barrier recovering for an immediate follow-up save.

**Lua persistence components (#761, save-overhaul B3):** Lua-owned
gameplay state (previously one opaque `snapLuaModules :: HashMap Text
Text` blob map, carried as the transitional `"lua-state"` envelope
component) is now a versioned, scoped, fail-fast COMPONENT per
registered Lua module, mirroring the Haskell component contract
("World.Save.Component.Types.ComponentCodec") but owned entirely in
Lua. `scripts/lib/save_modules.lua`'s `saveModules.register(id, spec)`
takes a declarative spec — `version`/`inputVersions`/`required`/
`scope`/`deps` plus `snapshot`/`decode`/`validate`/`apply` functions
(and `default` iff `required = false`) — validated at registration
time (duplicate id, invalid id/version, a missing required callback,
or an optional component missing `default()` all fail registration
outright via `error()`, never silently overwrite or default).
`saveModules.registerResetHook(id, resetFn)` is the separate, non-
component path for a module with no durable state (`unit_resources`) —
`resetFn` runs on every load, unconditionally, with no envelope entry
at all. `pause` no longer registers anything: it isn't persistent
(`pause.paused` was already dead — see `pause.onSaveLoaded`'s existing
"resume at default speed" reset). `unit_ai`/`building_spawn` are the
two real required components today.

Each registered id rides in the SAME envelope manifest namespace as
Haskell's components under a reserved, disjoint prefix —
`"lua.<module>"` (`World.Save.Component.Types.luaComponentPrefix`,
applied in exactly one place, `Engine.Scripting.Lua.API.Save`) — so
`unit_ai` becomes envelope component `"lua.unit_ai"`, independently
versioned/checksummed from every other component; a same-prefix
collision is only possible Lua-to-Lua, and the generic envelope codec
(#759/#760) already rejects that structurally as a duplicate id.
Because the set of Lua components is dynamic (unknown to Haskell at
compile time), `World.Save.Envelope`'s encode/decode functions take the
CURRENT Lua registry's known/required NAMES as explicit parameters
(gathered via `saveModules.describeAll()` by the one caller with Lua
access, `Engine.Scripting.Lua.API.Save`) rather than a static set —
`World.Save.Snapshot.SessionSnapshot` no longer has any Lua-shaped
field at all; Lua state travels beside it, never through it
(`World.Command.Types.WorldSave`'s 4th argument is now
`[(Text, Word32, Bool, ByteString)]` — bare name/version/required/
payload per component — not a blob map).

Payloads are canonical, data-only bytes from `scripts/lib/data_codec.lua`
(NOT the old `scripts/lib/serialize.lua`, removed entirely — it had no
remaining callers): booleans, finite numbers, valid-UTF-8 strings, and
tables built recursively from those as either a dense array or a
string/integer-keyed map, encoded in canonical (numeric-then-string,
ascending) key order for determinism. Decoding a payload NEVER compiles
or executes it (no `load()`/`loadstring()` anywhere in the codec) —
the old serializer's `load()`-based round trip is exactly the gap this
closes. Rejected at encode time: functions, userdata, threads,
metatable-carrying tables, cyclic tables, non-finite numbers (no
current schema needs them), unsupported key types, and anything past
the documented depth/entry/string/payload limits — every rejection
names a data path (e.g. `root.claims[3].power`).

Four entry points drive the registry from Haskell, all in
`Engine.Scripting.Lua.API.Save`: `describeLuaComponents` (→
`saveModules.describeAll()`, the known/required id sets), and — new
replacements for the old `collectLuaBlobs`/`restoreLuaBlobs` —
`collectLuaComponents` (→ `snapshotAll()`: a REQUIRED component's
snapshot/encode failure aborts the WHOLE save transaction via
`failSave`, before the `WorldSave` command is ever queued — no more
"the engine save still proceeds, just without Lua blobs"),
`prepareLuaLoad` (→ `prepareLoad(components)`: decode + migrate +
component-locally-validate EVERY registered component with NO live
mutation, requirement 11 — any failure, or a REQUIRED component
missing from the save, aborts the load before touching any Lua
state), and `applyLuaLoad` (→ `applyAll()`: apply the components
`prepareLoad` already validated, in dependency order, then run every
registered reset hook — only reachable after a successful
`prepareLoad`). `loadSaveFn` calls `prepareLuaLoad` immediately after
the existing #760 missing-content-definition check and before
`loadValidatedSave` pauses the engine and calls `applyLuaLoad` —
same ordering guarantee as before (Lua state lands before the
world-thread restore is queued), just through the new contract.

`unit_ai`/`building_spawn`'s `apply()` functions are a DELIBERATE,
clearly-marked TEMPORARY C2 compatibility adapter (requirement 15):
they clobber the module's live singleton wholesale from the decoded
payload and snapshot pre-load state into `_preLoadState`, exactly like
the pre-#761 deserializer bodies — the existing `onSaveLoaded`
broadcast (#195/#191's off-page-unit/building state preservation +
stale-nested-reference scrub) is untouched and still runs afterward,
once the engine-side restore settles. A future C2 will replace this
whole-singleton-clobber-then-reconcile dance with a real per-page/
per-entity component model; until then this is explicitly the
compatibility seam, not the final contract. `references()` (optional
on a registration) documents/traverses a component's entity-id fields
for diagnostics but does not itself hard-reject a dangling reference —
per the landed #758 precedent (a demolished station's lingering craft
bills are tolerated gameplay, not corruption), a target that legitimately
died before the save boundary stays representable and is cleared at
apply/reconcile time instead.

Save version bumped to v91 (`sdLuaModules` removed from the legacy
`SaveData` bridge — Lua state no longer rides through it at all).

Turnkey coverage: **`Test.Headless.Lua.SaveModules`** (the "Lua
persistence components" gate, `cabal test synarchy-test-headless
--test-options='--match "Lua persistence components"'`) — a standalone
Lua VM (no engine) driving `data_codec.lua` and `save_modules.lua`
directly via HsLua's `Lua.dostring`: canonical round trips, canonical
map key order, every rejected shape (functions/userdata/threads/
metatables/cycles/depth/entries/key-types/non-finite numbers/malformed
payloads, plus a proof that a code-shaped string decodes as inert data
rather than executing), registration validation (duplicate id, invalid
id/version, missing callback, optional-without-default), the
persistent/reset-hook id-namespace split, canonical `describeAll`
ordering, dependency ordering + cycle rejection, the full
`snapshotAll`→`prepareLoad`→`applyAll` round trip including reset
hooks, a required-component-missing-from-the-save load abort, a
required-snapshot-failure save abort, and the registration-blocked-
during-capture guard. **`tools/lua_orphan_prune_probe.py`** (updated
for #761) still gates the #195 reconcile end to end through a REAL
save/load, now driving `saveModules.snapshotAll()`/a component's own
`decode`/`apply` directly instead of the retired `serializeAll`/
`deserializeAll`/`sm.registry.X.deserialize`. **`tools/test_lua_save_api.sh`**
gained a case proving a REQUIRED component's injected snapshot failure
makes `engine.saveWorld` return `false` with no save directory created,
and that a normal save still succeeds once restored.

**Typed persistent references + shared integrity graph (#764,
save-overhaul C3):** every durable cross-component reference now
declares its expected target kind and scope rather than staying an
untyped raw id. `World.Save.Reference` is the leaf vocabulary
(`RefKind`/`ContentKind`/`RefScope`) plus `SamePageRef`/`CrossPageRef`
— thin wrapper newtypes a component DTO field uses to declare "this
reference's target must live on the same page as the record carrying
it" at the TYPE level; wire-identical to the wrapped id (a newtype has
no cereal discriminant), so no new bytes ride on disk, only a new
Haskell type. `CraftBillDTO.bilStation`/`bilClaimant` and
`PowerNodeDTO.nodBuilding` (`World.Save.Component.Entities`) are the
first fields to adopt it — both components bumped to v2, decoding a v1
payload via an explicit, unambiguous migration
(`migrateCraftBillDTOv1`/`migratePowerNodeDTOv1`; the frozen v1 DTOs
`CraftBillDTOv1`/`PowerNodeDTOv1` stay exactly as they shipped, per the
frozen-DTO boundary rule).

`World.Save.Integrity` is the shared diagnostic graph: `IntegrityError`/
`IntegrityReport` (component, version, data path, reference kind/value,
expected-vs-actual scope, a stable machine-readable code, a message —
deterministically sorted and capped with an omitted-count, never
silently truncated) and `sessionIntegrityErrors`, run over an assembled
`SessionSnapshot` at BOTH boundaries — `World.Save.Component.assembleSnapshot`
(pre-load) and, via a plain post-capture call (never folded into
`captureSessionSnapshot` itself — that would cycle back through
`World.Save.Snapshot`), `World.Thread.Command.Save.WriteWorld` (pre-save).
It validates a craft bill's station/claimant and a power node's host
building for wrong-PAGE (a target that resolves on a DIFFERENT page
than the record referencing it is a hard error) while staying tolerant
of the target being absent from the WHOLE session — the pre-existing
#758 "a demolished station's lingering bill is tolerated gameplay, not
corruption" contract, unchanged. `World.Load.Stage` already restored
bills/power-nodes verbatim rather than pruning them (issue #763 round
9) — #764 adds the cross-component check a component-local validator
structurally can't run, it doesn't change that restore path.

`luaReferenceErrors`/`KnownEntities` do the analogous cross-check for
Lua: every reference a registered Lua save component's `references()`
hook reports (issue #761 defined the hook; #764 is what actually
consumes its output — previously only crash-checked and discarded)
is validated against the load's real entity sets
(`Engine.Scripting.Lua.API.Save`'s `knownEntitiesFromSaveData`, built
from the decoded `SaveData`). Always a non-blocking diagnostic (logged
via `logWarn`, folded into `continueLoad`'s existing log/diagnostic
surface) — never load-rejecting, matching the same tolerated-dangling-
reference precedent scrubStaleRefs already relies on. `save_modules.lua`'s
`prepareLoad` now RETURNS the collected `{component=,kind=,id=}` edges
(`references` field) instead of only calling `references()` to catch a
crash.

Lua's persisted reference fields are typed on the wire too: every field
`scripts/unit_ai_save_refs.lua`'s `unitAiReferences` declares
(`attackTargetUid`/`retreatThreatUid`/`notifyTarget`/`lungeTarget`/
`buildTarget`/`storeTarget`, plus the nested `treatClaim.patient`/
`treatPending.uid`/`deliveryClaim.bid`/`deliveryPendingTarget.bid`/
`craftJob.billId`/`craftJob.bid`/`repairJob.instanceId`/`repairJob.bid`/
`pickupOrder.gid`/`forageTarget.gid`/`forageLoot[]`/`harvestLoot[]`) and
`scripts/building_spawn.lua`'s `lastUid` are wrapped to a structured
`{__ref=kind, id=N}` shape at `snapshot()`/`decode()` time and unwrapped
back to a bare number at `apply()` time — both components bumped to
schema v2 (`inputVersions={1,2}`), with an unambiguous v1→v2 migration:
v1's fields have always meant exactly what the declared list says, so
there is nothing to guess. This wrap/unwrap lives ONLY in
`unit_ai_save_refs.lua` (split out of `unit_ai_save.lua` to stay under
its line budget, #538) and `building_spawn.lua` — `aiState`'s LIVE
in-memory shape never changes, so `unit_ai_combat.lua`,
`unit_ai_deliver.lua`, `unit_ai.lua`'s own `scrubStaleRefs`, and every
other consumer needed no change at all; `unitAiReferences`'s `addRef`
reads either a wrapped table or a bare number transparently
(`refId()`), so it works unchanged whether called against decoded
(wrapped) data or the outer per-unit key (always bare, since a Lua
table key can't be a table).

Round-3 review closed three more gaps. `PageSimDTO.psSim`
(`unit-sim`, `World.Save.Component.Entities`) is keyed by `SamePageRef
UnitId` rather than a bare `UnitId` — the map KEY (a unit-sim state's
owning unit) is exactly as durable a cross-component reference as a
bill's station, just carried as a `HashMap` key instead of a field
value (`SamePageRef` now derives `Hashable` for this); bumped to v2 via
`migratePageSimDTOv1`/`migrateUnitSimDTOv1`. `checkRefTag`
(`unit_ai_save_refs.lua`) and its `building_spawn.lua` mirror now
reject a wrapped reference whose `__ref` tag doesn't match the field's
expected kind AND whose `id` isn't a positive integer — a tag-only
check would still accept `{__ref="unit", id="bad"}`, silently
unwrapping into live `aiState` and vanishing from every diagnostic that
`Lua.tointeger()`s the id instead of being reported. `World.Save.Component.assembleSnapshot`'s
decode/apply-phase error lists, and `World.Thread.Command.Save.WriteWorld`'s
`captureSessionSnapshot` failure path, now go through the SAME
sort+cap (`capComponentErrors`, exported from `World.Save.Component`)
every other boundary already used — previously only the cross-component
`crossErrs` list was capped, and the `sessionIntegrityErrors` portion
was capped TWICE (once alone, then again as part of the combined list),
which could badly under-report the true omitted count.

Round-4 review caught a real correctness bug in round-3's own id-floor
fix: `checkRefTag`'s blanket `id >= 1` incorrectly rejected a valid
`ground_item` reference of `0` — `Item.Ground`'s ground-item allocator
is ZERO-based (`emptyGroundItems` starts `gisNextId` at 0, so the very
first spawned ground item legitimately has `gid = 0`), unlike
unit/building/craft_bill/item_instance's allocators, which all start
at 1. `checkRefTag` now floors at 0 specifically for the `ground_item`
kind, 1 for every other kind.

Round-5 review hardened `tools/persistence_inventory_audit.py`'s
`find_lua_reference_kinds` gate: it only scanned a file for
`kind = "..."`/`addRef("...")` literals when that SAME file also
contained `references = ` text — which happened to work for
`unit_ai_save.lua`/`unit_ai_save_refs.lua` only by ACCIDENT
(`unit_ai_save_refs.lua` independently satisfies the gate via its own
unrelated `M.references = unitAiReferences` re-export line, not
because the audit traced the real delegation from
`unit_ai_save.lua`'s `references = refsMod.references` registration
field to the module `refsMod` is `require()`'d from). It now also
resolves `references = <var>.<field>` delegations against a same-file
`local <var> = require(...)` binding and includes that required
module's own file in the scan — tracing the real relationship instead
of relying on incidental text matching (can only WIDEN the scanned set,
never narrow it, so it can't introduce false positives from unrelated
Lua tables that happen to use a `kind = "..."` field for something else
entirely, e.g. UI element kinds).

Round-6 review closed three more gaps, the deepest round yet.
`checkRefTag` gained a `required` flag: `craftJob.billId`/`craftJob.bid`
(`unit_ai_craft.lua` sets both unconditionally the instant `craftJob`
is created) and `repairJob.instanceId` (`unit_ai_repair.lua`, same) are
now REJECTED when missing — previously `nil` was silently valid for
every reference field, so a v2/v3 payload with the job table present
but the field missing would apply with the job effectively discarded,
no diagnostic at all. `repairJob.bid` deliberately stays optional:
`unit_ai_repair.lua` never actually sets it at job creation, so
requiring it would reject every real repair job — each "required"
classification is verified against its actual construction call site,
never assumed, after round-4's blanket-minimum mistake made that
lesson concrete. `scripts/lib/save_modules.lua`'s `snapshotAllImpl` now
also calls `reg.validate(dataOrErr)` between `snapshot()` and
`dataCodec.encode()`, failing the whole save (required) or omitting
with a warning (optional) exactly like the existing snapshot/encode
failure branches — `validate()` used to run ONLY on the load side, so a
live state mutated into a malformed shape (e.g. `attackTargetUid` set
to a non-numeric value by some other bug) could snapshot, encode, and
WRITE to disk untouched, only surfacing as a silently-dropped reference
edge on a LATER load rather than as a save-time failure.

Finally, the outer per-unit (`aiState[uid]`) and per-building
(`state[bid]`) table KEYS are now typed too, closing the one asymmetry
left after `psSim`'s HashMap key went through the same treatment in
round 3: `unitAiReferences`/`buildingSpawnReferences` already reported
the outer key as a `"unit"`/`"building"` reference edge, but nothing
tagged it on the wire the way a wrapped VALUE field is. Lua has no
equivalent of `SamePageRef`'s wire-transparent newtype trick — a table
can never itself BE a `data_codec.lua` map key (canonical encoding only
supports integer/string keys) — so the fix is a self-describing
`__owner = {__ref=kind, id=N}` field carried INSIDE each row's own
value instead, redundant with the key by construction: `wrapUnitState`/
`wrapLastUid` synthesize it, `checkOwnerRef`/`validateBuildingSpawnData`
require it AND verify its id exactly matches the outer key (not merely
well-formed), and `unwrapUnitState`/`unwrapLastUid` strip it back off —
`aiState`/`state`'s LIVE in-memory shape never grows this field. Both
components bumped to v3 (`inputVersions = {1,2,3}`): a v1 payload
migrates straight to v3 in one step (`wrapUnitState`/`wrapLastUid`
already synthesize `__owner` for fresh writes); a v2 payload (every
OTHER field already wrapped, no `__owner` yet) migrates via
`addOwnerToAiState`/`addOwnerToAllLastUid`, which add ONLY `__owner`
without re-wrapping already-wrapped fields.

Deliberately NOT rewritten onto this vocabulary: the nine existing
`missingXReferences` content-definition checks (`World.Save.Types`/
`Engine.Scripting.Lua.API.Save`'s `continueLoad`) stay as they are —
already working, already tested, each against its own IO-loaded content
registry — reporting through the SAME `continueLoad` load-rejection
gate the new checks report through (one combined message), rather than
being rewritten onto `IntegrityError`'s Haskell type for a vocabulary-
only gain.

`tools/persistence_inventory_audit.py` extends the same "every root-
owner field / Lua save module must be classified" discipline (§7-style)
to this: a new DTO field typed `SamePageRef`/`CrossPageRef`, or a new
Lua `kind` string a `references()` hook reports, with no row under
`docs/persistence_state_inventory.md`'s "Typed persistent references" /
"Lua reference kinds" headings fails the audit.

Turnkey harness: **`python3 tools/persistence_integrity_probe.py`** —
the real-engine coverage the pure hspec gate (below) can't reach: a
unit's `attackTargetUid` pointing at a unit destroyed before the save
(engine paused first, so the AI's own self-heal can't race the save)
survives a real save → quit → fresh restart → load round trip as a
non-blocking diagnostic naming the component/kind/id, and a truncated
save is rejected with `LoadFailed` while the ALREADY-LOADED live
session (active page, unit existence, paused status) is left completely
unchanged. Registered manual-only (`slow/worldgen-heavy`) in
`tools/ci_probes.py`. Pure gate: `cabal test synarchy-test-headless
--test-options='--match "persistence reference integrity"'`
(`Test.Headless.World.Save.Integrity`) — reference-codec round trips
(same-page/global/permitted-and-forbidden-cross-page scope decisions,
optional-reference semantics, wrong-kind-cannot-resolve), the v1→v2
migrations, the wrong-page/tolerated-absence graph checks for bills and
power nodes, Lua reference dangling/allocator-exceeds/unknown-kind
diagnostics, and deterministic-ordering + truncation-with-omitted-count
for the capped report.

```bash
# From headless / debug console
echo 'engine.saveWorld("test", "my_save"); return "saved"' | nc -w 2 localhost 9008
echo 'engine.loadSave("my_save"); return "queued"' | nc -w 2 localhost 9008
# engine.loadSave only ACCEPTS the request synchronously (issue #763) — poll
# engine.getLoadStatus() for completion, below, before touching anything the
# save contains. A loaded page keeps ITS OWN saved id (never remapped to
# "main_world" — call world.getActiveWorldId() to find it).
echo 'return engine.getLoadStatus()' | nc -w 2 localhost 9008
# Auto-pause on save/load: the world loads paused; engine.setPaused(false)
# (or, in-game, scripts.pause's pause.set(false)/pause.toggle(), which also
# restores the time scale) to resume.
```

What's preserved: gen-params + camera + time + climate + river flow, edited tiles (chunks regen + edits replay), buildings (with spawn-roster countdown), units (with stats / modifiers / skills / inventory / sim state), Lua AI memory (water locations, in-flight commands, source-drink phase, search-spiral state) and pause state. Saves from older schema versions are rejected with a clear "expected vN, got vM" error.

Not preserved (transient UI / in-progress state, by design): current selection (cursor selection, `umSelected`, `bmSelected`), build-tool placement mode (`buildTool.state` + `buildingGhostRef`), and the active toolbar tool. A loaded world always comes up on the **default tool**: `sdToolMode` is still recorded at save time but intentionally NOT restored — staging sets each restored page's engine `ToolMode` to `DefaultTool` (`World/Load/Stage.hs`), and the `onSaveLoaded` broadcast (fired on *every* load — menu or debug-console `engine.loadSave`, once the transaction actually publishes) resets the HUD toolbar to match (`scripts/ui_manager.lua` `onSaveLoaded` → `hud.selectDefaultTool`), so `world.getToolMode()` and the visible toolbar always agree after a load (#103). A fresh-session load lands in default UI state — the player re-selects, re-enters the build tool, etc. Within-session loads keep the singleton Lua state otherwise.

Enum schema policy: `Direction`, `Pose`, and `UnitActivity` derive `Generic Serialize`, which is **positional by constructor tag**. They are **append-only** — inserting or reordering constructors silently corrupts saved units' facing/pose/activity. New variants go at the end. Anything else (renames that change tag order, removals, semantic redefinitions) requires bumping `currentSaveVersion`. Same convention applies to any future enum that lands in `SaveData` via `Generic Serialize`.

Wait ~15 seconds after `loadSave` for a 128-world before querying — the center chunk gens synchronously but the rest queue progressively. Headless tests need to budget the wait; poll `engine.getLoadStatus()` for `phase == "LoadPublished"` first (below) so the wait targets a page that actually exists.

**Whole-session load transaction (#763, save-overhaul C2):** `engine.loadSave` replaced its old incrementally-mutating merge path (decode → pause → restore Lua blobs → hardcode the active page to `main_world` → mutate a live world's phase → queue an engine-side merge, #214/#218/#191) with one coordinated, staged transaction. `World.Load.Stage.stageSession` reconstructs the ENTIRE replacement session (every saved page's `WorldState`, the merged `BuildingManager`/`UnitManager`, camera/zoom-atlas/preview for the active page) without touching a single live `EngineEnv` ref — chunk gen, zoom cache, and arena rebuild all run exactly as before, just against fresh values instead of the live refs, with every cross-thread side effect (sim-chunk seeding, location-stamp dispatch) collected as data instead of sent. `World.Load.Publish.publishStagedSession` then performs the actual swap in one authorized window, reusing `Engine.Save.Barrier`'s existing owner-quiescence protocol (`Engine.Scripting.Lua.Thread.Dispatch`'s `handleLoadStaged` drives `beginSave`/`waitForOwners`/`reachSnapshot` exactly like `engine.saveWorld` does, then applies the already-validated Lua state via `applyLuaLoad` *before* queuing the Haskell-side `WorldLoadPublish`, so Haskell state never becomes observable while Lua could still fail) — every other state-owner thread is quiesced for the swap's duration, so no gameplay consumer ever observes a mixed generation.

A load **REPLACES the complete session**: every saved page keeps its own id verbatim (no `main_world` remap, no `<id>#N` collision suffix — `World.Thread.Command.Save.RestoreIds` and `loadProvenanceRef` are gone), and any live page that was NOT part of the save does not survive publication — the old "preserve unrelated live pages" merge behavior (#191/#214) is gone along with the incremental load path it protected. Mutual exclusion (`Engine.Load.Status`, the load-side counterpart to `Engine.Save.Barrier`) rejects a save request for the entire duration of an in-flight load and vice versa — not just during the brief publish window — so `engine.saveWorld`/`engine.loadSave` never overlap. `engine.loadSave` pauses synchronously at request acceptance (before any decode work) and a **failed** load leaves the unchanged session paused too — pausing is a one-way ratchet per attempt, never auto-restored. A missing gameplay definition (unit/building/item/recipe/...) still rejects the whole load with nothing changed (#760 requirement 9, unchanged by this issue), now reported through `engine.getLoadStatus()`'s `LoadFailed` phase like every other failure. `engine.getLoadStatus()` exposes the 12-phase lifecycle (`Engine.Load.Status.LoadPhase`: `LoadRequested` → `LoadPaused` → `LoadSourceSelected` → `LoadEnvelopeValidated` → `LoadComponentsDecoded` → `LoadComponentsMigrated` → `LoadSnapshotAssembled` → `LoadContentValidated` → `LoadStaged` → `LoadWaitingPublish` → `LoadPublished` | `LoadFailed`) so a headless caller can wait for a SPECIFIC load's completion rather than polling stale state left by a previous one (mirrors `engine.getSaveStatus()` exactly).

Turnkey harness: **`python3 tools/transactional_load_probe.py`** — the #763 gate. Boots real engines and asserts: several deliberately invalid loads (missing save, corrupt save, missing gameplay definition) each leave the current session completely unchanged and paused, reporting `LoadFailed`; mutual exclusion rejects a save mid-load and a second concurrent load; a successful load replaces the complete session (a page live only pre-load, never part of the save, does not survive publication) rather than merging; Haskell and Lua state agree immediately post-publication; a paused dwell advances no gameplay state and unpausing (through `scripts.pause`, which pairs `engine.setPaused` with the time-scale restore) lands on the default speed; repeated loads accumulate no ghost pages.

Multi-world save regression: **`python3 tools/multiworld_save_probe.py`** — the #214/#219 gate, and since #707 also the world-identity gate. Generates two real world pages ("main_world" + "second_world", both kept under their own saved ids — issue #763 removed the old active-page-remap-to-main_world behavior), spawns a unit + building on each, saves, then does the gold-standard **save → quit → fresh restart → load** and asserts both pages' entities survive on the right page (cross-page negative checks included), each identity stays on its own page, and `engine.listSaves()` reports the save-slot name separately from `worldName`/`worldGloss`. `--port`/`--seed`/`--seed2`/`--size`/`--plates`. NB: it uses two `world.init` pages, not `world.initArena` — loading a save that contains an arena page currently hangs the world thread (#365), so arenas can't be a save-test secondary page.

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
