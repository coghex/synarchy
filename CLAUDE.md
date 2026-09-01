# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

This file carries the rules that prevent damage: what you must not undo,
and which gate proves it. The layer below — the as-built mechanics behind
those rules — lives in
[`docs/engine_contracts.md`](docs/engine_contracts.md), which the sections
here point at by name. Read the relevant section there before changing
code in the area it covers; every contract in it is enforced by the gate
its entry names, which is why it could move out of the always-loaded file.

Three trims have shrunk this file, each archived verbatim: deep
per-issue history on 2026-07-23
(`docs/history/claude_md_2026-07-23_pretrim.md`), the 2026-08-18 pass
that extracted `docs/engine_contracts.md`
(`docs/history/claude_md_2026-08-18_pretrim.md`), and the 2026-08-20
pass that moved more gate-enforced mechanics into that file
(`docs/history/claude_md_2026-08-20_pretrim.md`). Consult those
snapshots, git history, or the referenced issues/PRs when you need the
full story behind a contract stated tersely here.

## Build Commands

- **Build:** `cabal build all` (does NOT build test suites — use `cabal build synarchy-test-headless` explicitly)
- **Run:** `cabal run synarchy`
- **Run tests:** see **Testing Tiers** below — pick the cheapest tier that covers the change; don't run the gates as an iteration loop
- **Pre-push gate:** `make ci` runs the same **gate set** as `ci.yml`'s
  `test-and-audits` worker — the warning-clean (`-Werror`) builds, the headless
  hspec suite (full tier: it sets `SYNARCHY_FULL_TESTS=1`, which CI sets
  only on worldgen-selected runs — #1364), and every `python3 tools/*.py`
  audit and self-test.
  `tools/ci_parity_audit.py` (#1355, run on both sides) compares the two
  files' tool invocations in both directions and fails on any difference
  outside its hard-coded, reason-carrying exemption list, so the set
  cannot silently drift; the full enumeration and the CI-only exemptions
  live in `docs/engine_contracts.md` §The `make ci` gate set. One member
  of that set is path-selective on BOTH sides (#1360): the save-compat
  self-test's `cabal repl` reproducibility test runs only when the
  working tree's own changes select `ci_expensive_gates.py`'s
  `save-compat` gate — everything else, `save_compat_audit.py` and every
  other member of that module included, still runs unconditionally. It
  uses the prod profile and your warm `dist-newstyle`. It is NOT an
  iteration loop and must not be run automatically before opening a PR —
  only on an explicit user request for full local CI validation.
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
   | `test_save_compat_audit.py --only-reproducibility` (~26 s, spawns a `cabal repl`) | the save format, the tracked fixture corpus, `save_compat_audit.py`, or a Cabal path — `ci_expensive_gates.py --gate save-compat` is the authority; `--without-reproducibility` covers the rest of the module and is cheap |
   | `findings_report_audit.py` | a findings report |
   | unit-asset gate (`test_pack_atlas.py` + `pack_atlas.py --validate-only --strict`, ~2 s) | `assets/textures/units/` (source frames or generated `atlas/`), `data/units/`, `tools/unit_texture_budget.json`, `src/Unit/Atlas/`, or the unit-YAML / preview / registration decoders |

   Do NOT run the whole headless suite, the 21-seed world check, or
   `make ci` by default — CI is the full-suite authority.
3. **Worldgen-OUTPUT changes only (full tier).**
   `SYNARCHY_FULL_TESTS=1 cabal test synarchy-test-headless` (+~11 s on
   a warm macOS/aarch64 tree; +~64 s of hspec wall on CI's Linux
   runner — measure each platform, don't port one number), then
   re-capture baselines `python3 tools/world_baseline.py` (~7 min) and
   re-run world_check. Remember the save-version bump.

   Since #1364 this tier is no longer local-only: CI's `Headless test
   suite` step sets `SYNARCHY_FULL_TESTS=1` whenever the **same
   worldgen selector** that gates `world_check --quick` fires — so
   every worldgen-output PR and every push to master runs it and a
   failure blocks — and `tools/ci-local.sh` (`make ci`) sets it
   unconditionally. Running it by hand is still the fast way to see a
   failure before pushing; it is no longer the only thing standing
   between a full-tier regression and master.

   **The variable is wholesale, not per-test.** It has exactly one
   consumer today (`Test.Headless.WorldGen.Exposure`'s w128 seed-42
   volcano case), and any new example added behind it automatically
   joins BOTH of those gates. Add one only after deliberately accepting
   that recurring CI cost. Note also that the guard matches any present
   value: `SYNARCHY_FULL_TESTS=` (empty) reads as ENABLED, so anything
   turning it off must leave it unset.
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
profiling (`--enable-profiling -f profile --builddir=dist-prof`) has two
hard rules: **`+RTS -N1` is mandatory** (the profiled RTS segfaults
under the sparked worldgen parallelism), and drive it via `--headless` +
`world.waitForInit`, never `--dump` (its watchdog can force-kill
mid-profile and truncate the `.prof`). Full recipe:
`docs/history/worldgen_timeline_profile_2026-07.md`.

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
| `≠` | inequality — **prose only**, never an operator | use `≢` |

Five of these are **enforced**: `.&.`, `.\|.`, `>>=`, `==`, and `/=` must
not appear as Haskell operators in `src/`/`app/` outside
`tools/unicode_operator_audit.py`'s short, explicit exemption list
(`src/UPrelude.hs`'s own definitions; `ShaderCode.hs`'s quasiquoted
GLSL; the `Eq`/`Monad` instance method names, which must stay ASCII) —
see issue #1005 / `docs/code_health_findings.md` CH-49. `fmap`'s two
spellings, `<$>` and `⊚`, are a deliberate exception: **both are kept**,
picked per call site by readability, not enforced either way.

`≠` is **not** a second such exception. `Prelude.Unicode` exports it as
the same `/=` at the same fixity, but this project spells inequality
`≢` and nothing else: `≠` is allowed only inside comment prose —
pseudocode or a maths formula — and never as an operator in
`src/`/`app/`. The same audit enforces that (#1494), by its own
single-code-point path, since its ASCII lexer cannot see `≠` at all.

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

`Engine.Core.State`'s `EngineEnv` is one shared record reachable from
any thread. The capability-split epic (#537/#889–#899) that narrowed it
is **complete**.
[`docs/engineenv_capability_inventory.md`](docs/engineenv_capability_inventory.md)
(#876) is the authoritative capability/thread/lifecycle ownership
inventory for every field, and `tools/engine_env_capability_audit.py`
(CI + `make ci`) fails if a classification drifts from the live record.

**Before adding any state, read that doc's §6.4 post-flip procedure** —
it leads with the case that resolves most of them: the state doesn't
belong on `EngineEnv` at all (`WorldState`, a manager, `EngineState`, or
a local), and needs no new field. Before adding a capability record,
read §2.1's canonical convention block rather than inferring the shape
from an existing one. Each capability lives in its own
`Engine.Core.Capability.<Name>` module exporting one `<Name>Capability`
record plus a total projection; `EngineM` stays hard-wired to
`MonadReader EngineEnv` (no capability typeclass layer), so a narrowed
module keeps thin `MonadReader` wrappers over primitives that take the
capability explicitly — narrowing the *module's own field access* is the
goal, not rewriting every caller.

**Full access is a closed allowlist.** The same audit treats importing
`Engine.Core.State` with `EngineEnv(..)` or as a bare import (either
shape) as unrestricted access, production-only (`src/`+`app/`; `test/`
exempt). Since #899 that is allowed **only** for §6.1's hard-coded
permanent allowlist of genuine whole-session orchestration boundaries.
§6.2's temporary ceiling is **empty** and shrink-only, so "add the field
now, narrow it later" no longer exists; the audit also parses §6.1 and
requires it to equal the checked-in constants, so neither the doc nor
the code can admit a permanent importer alone. §6.4(c)/(d) govern the
two escape hatches (a new capability; a new §6.1 module) — both need
explicit maintainer approval and synchronized doc + constant + self-test
changes.

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
- Chunk-based with zoom-level LOD system: `World.ZoomMap.*` builds the
  zoom cache/atlas at world-init time (`World.ZoomMap.Cache.*`,
  `ChunkTexture`, `ColorPalette`, and the cache's own output types in
  `World.ZoomMap.Types`); `World.Render.Zoom.*` renders from it. The
  dependency runs one way — nothing under `World/ZoomMap/` imports
  `World.Render`.

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

**`math.random` is GAMEPLAY's stream (#1330).** Its per-state entropy is
established once by `Lua.openlibs` before `scripts/init.lua` loads, and
eleven gameplay modules draw from it — so **nothing under `scripts/` may
call `math.randomseed`** (reseeding makes two engines launched in the
same second share one simulation), and non-gameplay code keeps its OWN
stream: `scripts/ui/random.lua` (SplitMix64) is the UI widget kit's.
Gate: hspec `--match "random stream ownership"`. History and the
randbox incident: `docs/engine_contracts.md` §Lua random streams.

### UI system
`UI.*` handles focus management, text input, and UI rendering; layout
and behavior are driven from Lua. Regression suites:
`Test.Headless.UI.*` (InputOwnership, Clipping, PopupPlacement,
InteractiveBounds, ResponsiveMenus, ResponsiveGameplay). Contracts:

**Text coordinates:** `UI.TextBuffer`/`UI.getCursor`/`UI.setCursor` use
zero-based Unicode code-point offsets. Lua strings are UTF-8 byte
arrays — editable widgets must use `scripts/ui/utf8_safe.lua`, never
`#text` or byte-based `string.sub`. The debug console's own input line
(`scripts/shell.lua`) holds the same contract on
`cursorPos`/`inputScrollOffset` and every derived path — tab completion
(which snaps its byte-wise agreement point back to a character
boundary), the ghost hint, and the scroll/measure walk. The Delete key
arrives as `onTextDelete` (`LuaTextDelete`), not `onDelete`; a
`config/shell_history.txt` line that isn't valid UTF-8 is dropped at
load. Gate: hspec `--match "Lua.ShellInput"`.

**Text display (#1159):** the same rule binds read-only DISPLAY paths —
wrapping, truncation, and any other per-character walk advances one code
point at a time, or non-ASCII text renders as mojibake and gets measured
once per byte. Lua PATTERNS are byte-oriented too, so `gmatch(".")` is a
byte loop. Pixel-width wrapping goes through `scripts/ui/text_wrap.lua` —
`byCharacter` (the debug console) and `byWord` (all three log panels) —
rather than a fourth private copy. Unlike `utf8_safe`, it never raises on
malformed UTF-8 and never drops a byte. Gate: hspec
`--match "Lua.TextWrapping"`.

**Pointer, scroll, and focus routing (#742–#749):** the six input
contracts are in `docs/engine_contracts.md` §UI input routing — read it
before touching hit-testing, activation, clipping, or wheel handling.
The rules to know on sight:

- Pages live on six `UILayer`s; `uiLayerBand` is the single paint-order
  source of truth for BOTH hit-testing and rendering. Whether a page
  blocks input is the separate per-page `upInputExclusive` flag
  (`LayerModal` defaults exclusive); the topmost visible exclusive page
  owns the modal boundary, and empty modal space consumes. `LayerDebug`
  is pass-through above any modal.
- Click callback, pointer-blocking, and scroll-capture are three
  independent per-element policies; wheel routing picks the topmost
  in-scope scroll-capturing surface by the same paint-order walk, never
  the click machinery.
- Plain and Shift wheel go through the IDENTICAL engine pipeline —
  don't reintroduce `UI.isInputBlocked()` self-gates in Lua handlers;
  the engine decides once, upstream.
- Press→release activation is epoch-guarded; unrelated sibling/child
  churn must never cancel an activation — don't "simplify" it back to a
  global counter. Keyboard CONTROL focus is independent of text focus
  and reports as `controlFocused`.
- `UI.Clipping.effectiveClip` is the ONE helper rendering and
  hit-testing both consult, so paint and hit-test can't drift;
  `UI.placePopup` is the one placement algorithm for floating content.
- All hit-testing uses the INTERACTIVE rect
  (`UI.InteractiveBounds.interactiveRect`); visible overflow never
  enlarges a target unless opted in via `UI.setInteractiveOverflow`.

**Container window stack (#1238/#1250):** `scripts/cargo_inventory_panel.lua`
is THE container window and owns an ordered STACK of levels, not one
popup. Two windows never coexist at one level: opening container B where
A is open REPLACES A and discards every deeper level, and an EXTERNAL
request always targets the base. Only the DEEPEST level is interactive,
and nothing enforces that by hand — a level past the base gets its own
`LayerModal` page, so #742's boundary makes every shallower level
painted-but-unclickable. Escape closes ONE level per press. The stack is
transient session UI: `hud.createUI()` snapshots and restores the whole
thing across a resize, and `uiManager.onSaveLoaded` drops it. The four
level kinds (`endpoint`/`unitItem`/`buildingItem`/`escort`), the
exact-instance-identity descent, the render-only rule for item levels,
pane semantics, the load-bearing `paneWidgetName` rule, and the teardown
REASONS (`"layout"` being the one that does not fire `onClose`) are in
`docs/engine_contracts.md` §Container window stack. Gates: hspec
`--match "container window stack"` / `"Container knowledge"` /
`"Nested item contents"` / `"Item list widget"`, plus
`tools/item_list_widget_probe.py` (manual-only, `needs-gpu`).

**Responsive lifecycle (#748 menus / #750 gameplay):**
`scripts/ui/responsive.lua` owns the supported envelope (formal minimum
800x600); out-of-envelope combinations degrade best-effort — never
crash, never invalid geometry, fixed actions stay reachable. Menu
screens register via `responsive.register` + `responsive.notifyResize`;
gameplay surfaces stay OFF that registry (they're reached through
`ui_manager_boot.lua`'s manual forward or the engine's automatic
`broadcastToModules` resize — registering a broadcast-reached module
DOUBLE-FIRES it). For any new screen/panel: a geometry rebuild must
preserve state a semantic re-entry may reset, and restores must not
re-fire `onChange`/`onSelect`; keyboard control focus survives rebuilds
BY NAME; panel content derives from the panel's REAL bounds, never an
independently recomputed value; zIndex ACCUMULATES through the parent
chain, so leave wrapper/viewport elements at 0; hud rebuilds first and
dependent surfaces `reflow()` after it. The envelope bands and the full
resize-correctness rules are in `docs/engine_contracts.md` §Responsive
UI lifecycle.

Headless UI tests use a bare Lua backend + synthetic texture/font
handles (`engine.getTextWidth` returns 0 there — stub it when a test
needs real measurement); the shared fixture wipes `package.loaded`
between cases. The full `ui_manager` boot never runs headless (it gates
on `fontsReady`, which needs a GPU font atlas) — use `--offscreen` for
end-to-end UI verification.

## Project Layout

- `src/` — Library source (~730 modules)
- `app/Main.hs` — Executable entry point (draw loop)
- `test/` — the `synarchy-test-graphical` hspec suite: display-requiring
  GLFW-window and Vulkan window-target specs. **Automated gates only
  COMPILE it** — CI and `make ci` both `cabal build` it and neither ever
  `cabal test`s it — because `test/Spec.hs` calls `GLFW.init` and creates
  a real window before `hspec` runs, so with no display it yields no
  assertions at all rather than a partial run. Running it by hand on a
  graphics-capable desktop is the only way it executes. Every GPU-free
  spec belongs in `test-headless/` instead (#1153), which every CI run
  does run
- `test-headless/` — the `synarchy-test-headless` hspec suite: the
  running gate, executed on every CI run (see **Testing Tiers**)
- `cbits/` — C code (stb_truetype font rasterization, Lua debug FFI)
- `config/` — YAML config: tracked `*_default.yaml` templates +
  gitignored `*.local.yaml` runtime state (see "Config state" below)
- `data/` — Game data YAML (materials, vegetation, flora, units)
- `assets/` — Images and graphical resources
- `scripts/` — Lua scripts for game logic

## Working-tree discipline

**`~/work/synarchy` is the PRIMARY checkout and must be left CLEAN.** The
PR drainer fast-forwards it after every merge, autostashing whatever
uncommitted work it finds; a restore that CONFLICTS wedges every later
drainer pass until a human resolves it (it happened four times in
2026-08, every time on `docs/code_health_findings.md`). So: **any file
you write into the repo but do not commit belongs in the docs worktree,
never the primary checkout** — report annotation (`/process-report`),
findings documents, design-doc drafts, anything a workflow leaves
sitting for review. The same rule covers EDITS to tracked documents
(`CLAUDE.md`, anything under `docs/`): unless the work is a PR running
in its own separate worktree, make the edit in the docs worktree and
land it with `tools/docs_land.sh` — never write to a markdown doc in
the primary checkout. Resolve the worktree by BRANCH — never hard-code
the path, never assume the current directory is right:

```bash
DOCS_WT="$(git worktree list --porcelain \
  | awk '/^worktree /{p=substr($0,10)} /^branch refs\/heads\/docs-wip$/{print p; exit}')"
[ -n "$DOCS_WT" ] || { DOCS_WT=~/work/synarchy-docs
                       git worktree add "$DOCS_WT" -b docs-wip origin/master; }
```

**An agent never lands docs on its own.** Landing is the user's call: either
they ask for it explicitly, in which case use the tool below, or the work
accumulates uncommitted in the `docs-wip` worktree until they batch it. Docs
pile up there indefinitely by design — that is the worktree's whole job — and a
push per edit is what floods master CI and makes the drainer re-check every
open PR. There is no third option: never push `docs-wip`, never run
`docs_land.sh` unasked, and never hand-roll an equivalent.

Docs land on master by direct push, not a PR. **Use `tools/docs_land.sh`,
not a hand-rolled sequence:**

```bash
tools/docs_land.sh -m "Commit subject" docs/foo.md [docs/bar.md …]
tools/docs_land.sh -n -m "…" docs/foo.md      # dry run
tools/docs_land.sh -f -m "…" docs/foo.md      # proceed despite the risk warning
```

It commits ONLY the paths you name, refuses up front the one combination
that makes a rebase autostash conflict (a file dirty here but NOT being
landed that also changed on master), tolerates other half-written docs,
fast-forwards the primary only when it is clean, and judges success by
`rev-list` — the push prints `Cannot update this protected ref` and then
**succeeds anyway** under admin bypass, so never judge it by the
warning. **`docs-wip` is not a feature branch** — it tracks
`origin/master` and lands by direct push. Details and the manual
fallback: `docs/engine_contracts.md` §Docs landing.

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
self-test) fails when a CH item's heading and checklist markers
disagree, or when the two sides don't declare the same set of CH numbers
exactly once each. It audits AGREEMENT only — whether a marker is the
right one stays the processing lane's judgement.

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

- **NEVER launch `cabal run synarchy` / `cabal run exe:synarchy` without `--dump`, `--headless`, or `--offscreen`** — otherwise it opens a graphical window that steals the user's focus (`--offscreen` uses the GPU but creates no window, so it is safe). **`--preview` (below) is NOT in this safe list** — outside the explicit sprite-signoff workflow below, never launch it yourself even transiently; a bad target rejects before boot, but a valid one steals focus like the graphical path
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
have no window, so if the listener can't start — an occupied or
unbindable port, or `--port 0` (a `--dump`-only sentinel) — the boot
ABORTS: non-zero exit, no `READY` marker, cause on stderr, partial boot
torn down. So the wait loop above fails fast instead of hanging forever
on a live process with no reachable `engine.quit()`. `--dump`, graphical
and `--preview` keep their existing tolerance. Details:
`docs/engine_contracts.md` §Debug-console listener policy. Gates: hspec
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
`scripts/preview_manager.lua` — for eyeballing a texture without booting
a game session. **It always opens a real window** (see the warning
above) — there is no offscreen/headless variant, so treat it exactly
like the graphical path.

**Sprite signoff deliberately uses that real window.** When an active task
reaches owner approval of a sprite or sprite group, do not ask the owner to
judge a chat thumbnail or contact sheet. From the task's isolated worktree,
first run `cabal build all`; only after that build succeeds, launch that same
worktree's executable with `cabal run exe:synarchy -- --preview
<category>/<item>`. Leave the preview open so it appears in front of the owner
and intentionally takes focus for the approval decision. This signoff step is
the exception to the no-focus-stealing rule above. Record the owner's verdict;
close or replace the preview only as the interactive review requires.

Canonical category contract (`App.Cli.classifyPreviewCategory`) — the
unknown-category error lists exactly this set, no compatibility aliases:
**simple** (a flat, recursively-browsable asset folder): `icons`,
`items`, `ui`, `world`; **grouped** (one named entry per item — a bare
grouped category prints "select a specific ..." and exits without
booting): `units`, `flora`, `buildings`, `structures`. `equipment`,
`hud`, `facemap`, `utility`, `vegetation` are NOT exposed.

**Pre-boot rejection is the load-bearing rule**
(`Engine.Preview.Discovery` / `.Unit` / `.Building`; `resolveItemDir`
shared by all four grouped categories): an unknown name, a name with
path structure or `.`/`..`/absolute traversal, a symlinked directory
(BOTH levels for `units/<name>` — `doesDirectoryExist` follows links),
and a FILE where a directory was expected all exit 1 **before a window
exists**. **Trimmed loading:** only its font, the list widget's own
chrome textures, and textures within the requested category/item — never
`data/*.yaml` gameplay catalogs, with exactly TWO single-file
exceptions: the units viewer's `data/units/<name>.yaml` and the
buildings viewer's `data/buildings/<name>.yaml`.

`flora/<name>` and `structures/<name>` reuse the shared browser
(`scripts/ui/asset_browser.lua` + `scripts/ui/list.lua`, #888) rooted at
the ITEM's folder — anything beyond routing the resolved folder into
`discoverEntries` means the routing is wrong, not the reuse. The **units
viewer** (#887/#1261) samples the compiled atlas through the SAME loader
(`Unit.Atlas.Yaml.resolveUnitAtlases`) and frozen cell arithmetic
(`atlasCellUV`) the game uses — a preview-only decoder would miss the
regressions the viewer exists to catch — and a rejected, missing,
animation-less or uncompiled index is a PRE-BOOT failure, never a quiet
fallback to source frames. The **buildings viewer** (#888) is the
opposite authority split: **the filesystem is authoritative** and
`data/buildings/<name>.yaml` only AUGMENTS a matched animation
(association by CONTENT, never by equal names; playback defaults `fps=8`,
`loop=false` — NOT the units viewer's `loop=true`). Browser behavior,
ordering, mirroring, playback clocks, both default-selection ladders,
and the full `previewManager.dump()` contract (what lets a probe click a
located row instead of a hardcoded coordinate) are in
`docs/engine_contracts.md` §Preview mode.

**Centered bounded zoom (#1907):** every main preview display has ONE
per-session zoom multiplier, `1` (the initial value AND the maximum,
i.e. the aspect fit) down to `1/8`, centered on its region with NO
anchor, pan or crop — the complete texture is inside its region at every
level by construction. The region owns a scroll-CAPTURING invisible
element and nothing else (#743's three policies stay independent), which
is what makes plain and Shift wheel identical; `dy < 0` ENLARGES, the
gameplay camera's sign, not the list's. Reset follows preview-OBJECT
identity — a different BARE-category texture resets, while another
animation, direction, building entry, flora stage, structure piece,
playback frame or a resize preserves — discriminated by
`engine.getPreviewTarget()`'s `item`, never the mode string. Unit mode's
region is the enlarged sub-rect, never `panelBounds`. Full contract:
`docs/engine_contracts.md` §Centered bounded zoom.

Gates: `tools/preview_cli_probe.py` (CI-eligible, no boot at all — every
rejection above) and `tools/preview_probe.py` (manual-only, `needs-gpu` —
discovery/selection/scroll/resize via the dump, forced nearest
filtering, both viewers, trimmed loading verified against the engine's
own authoritative texture record, and zoom on all six display kinds via
real `input.moveMouse`/`input.scroll`). Pure logic: hspec
`--match "Preview.Discovery"` / `"Preview.UnitAnimation"` /
`"Preview.Building"` / `"Preview.Zoom"` — the last is the only BLOCKING
automated gate zoom has, the probe being manual-only.

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
# The optional identity (#707) is immutable display text, persisted,
# independent of pageId and slot name. A name with no languageSeed is a
# CUSTOM name with NO language provenance (#1092); languageSeed (#1101,
# a decimal STRING — a Word64 has no lossless Lua number) states the
# name was RENDERED from that language and is what names the page's
# placed locations in the same one. Provenance is never inferred; a
# malformed seed is refused with a warning, leaving a custom name.
# Full contract: docs/engine_contracts.md §World identity and language
# provenance.
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
- **Position hold (#1216, SURV-4)** — a unit that COMPLETES a
  player-issued move order holds the destination instead of resuming
  wander, and stops contributing autonomously until re-commanded — a
  deliberate trade-off, not a bug. `hold_position` scores EXACTLY
  `FOLLOW_COMMAND_UTILITY` so the #306 ladder is reused — don't add a
  second constant. Only an ARRIVAL of a PLAYER-intent move creates a
  hold (`commandMove`'s `internal` flag exists so portal walk-outs don't
  pin fresh acolytes), and only an accepted, explicit player command (or
  `unitAi.releaseHold`) clears one. Persisted via `lua.unit_ai` v6;
  v1-v5 decode as not-holding. Full mechanism:
  `docs/engine_contracts.md` §Position hold. Gates: hspec
  `--match "position hold"`, `tools/position_hold_probe.py`
  (manual-only).
- **Tile-coordinate frame at the U seam (#1175/#1230)** — chunks are
  STORED u-wrapped, so one physical tile has two names near the seam.
  ONE contract (stated in full on `World.Render.HitTest` and in
  `docs/engine_contracts.md` §Tile-coordinate seam frame): every point
  read / mutation / cancellation — picking, designation maps, and the
  verbs a worker FINISHES a job with — uses CANONICAL coords and accepts
  any alias. RECTANGLES are the exception: a drag's second endpoint is
  re-expressed in the anchor's local alias frame
  (`localizeTileToAnchor`; Lua `world.localizeTile`) BEFORE any
  clamp/`min`/`max` — canonicalising one end alone MEASURED worse than
  seam-blind behaviour; don't. Terrain LOOKUPS must `wrapChunkCoordU`
  before `lookupChunk` (a miss reads as "not loaded → flat", which for
  occlusion means "nothing blocks"). Where a tile is DRAWN is the
  separate `bestWrapOffset` axis (#1176). Away from the seam, and in
  arenas, every step is the identity. Gates: hspec
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
  `withdrawFromCargo`) are a SEPARATE path the fetch/repair/medic
  ladders depend on — never route AI work through the strict one, and
  never delete them. What is unchecked there is adjacency and receiver
  eligibility (and unit-to-unit capacity), **never the world PAGE**
  (#1673): all four refuse a cross-page endpoint pair, mutating nothing
  and revealing nothing, which is the floor `Unit.Transfer.reachable`
  holds even where it defers adjacency. The AI finders page-qualify
  every candidate against the ACTING unit
  (`scripts/unit_ai_page.lua`) instead of trusting the active page that
  `unit.getAllIds` / `building.getActiveIds` / `craft.getBills` each
  snapshot separately, and revalidate every PERSISTED building
  reference (`deliveryClaim.bid`, `craftJob.bid`, `repairJob.bid`)
  before it can steer a walk or reach a verb. Gates: hspec
  `--match "Unit cargo"` / `"AI page pairing"`.

  **TWO player modes, ONE commit policy.** Mode B queues a durable order
  and Mode A commits on the spot, but both build the IDENTICAL request
  and both reach `checkTransfer`/`commitTransfer`. The player-facing
  IMMEDIATE paths retired with #1249 must not come back — the Store /
  Retrieve gestures replaced them and NEITHER requires adjacency; only
  the PLAYER paths retired, the verbs stay registered for the AI (D-7).

  **Contents are REMEMBERED, never live (D-2).** A container window
  renders the player's last observation. Exactly four things reveal
  (`Building.Knowledge.Live`): a completed transfer commit into or out
  of the container, the lax AI cargo verbs, a Mode A session OPENING on
  it (`building.refreshContainerKnowledge`'s only in-game caller), and
  the first completion of a storage-capable building (seeds KNOWN-EMPTY
  because the player watched it go up). Walking past, selecting,
  right-clicking and opening the window reveal NOTHING; every
  unit-driven reveal is gated on `isPlayerCommandable`; knowledge is
  player-global, never per-unit.

  The three player-facing modes — durable ORDERS (#1246/#1247/#1253,
  where arrival is the commit and every ending surfaces once then
  prunes), Mode B queued GESTURES (#1249), and Mode A ESCORT
  (#1250/#1251, walk first then choose, with a two-sided hold) — are
  specified in `docs/engine_contracts.md` §Player transfers. Read it
  before touching an executor, a gesture's eligibility, or a teardown
  path: what is OMITTED rather than disabled, which timer is a stall
  rather than a trip budget, and why `escort_hold` is auto-prepended to
  every species are all load-bearing.

  Gates: hspec `--match "Unit transfer"` / `"Transfer context menu"` /
  `"durable transfer orders survive"` / `"Container knowledge"`;
  `tools/transfer_order_probe.py` and `tools/item_list_widget_probe.py`
  (manual-only; the latter owns the real-AI proof that a MOVING target
  is preempted and then stays put for the whole approach), and — the
  arc's INTEGRATED gate — `tools/unified_transfer_probe.py` (#1255,
  manual-only `needs-gpu`): one fixed-seed session proving an exact
  instance moves both ways between all three endpoint classes through
  BOTH modes, plus the partial batch, the reveal rule, one widget
  rendering every container view, and a Mode B order surviving a
  fresh-process reload while a Mode A session does not.
- **Power (#358-#361, #590/#591, #1206)** — solar/battery nodes are
  item-consuming placements (`power.placeNode` via
  `buildTool.commitPlacement`); networks (wire 4-adjacency +
  nodes/consumers) are recomputed fresh every tick — only battery
  `storedWh` persists. Solar follows the sun angle and
  `world.setTimeScale`. Electrical load lives on the RECIPE
  (`power_draw`), not the building: a bill draws only while claimed AND
  `cbWorking`; `power.isStationPoweredForRecipe(bid, recipeId[, billId])`
  is the gating query — pass the bill's own id so its already-registered
  draw isn't double-counted. A node's LIFETIME is its building's:
  `BuildingDestroy` retires it in the same live transaction that removes
  the instance (`Power.Live.retirePowerNodeEverywhere`), so a demolition
  never reaches the save — but that is NOT load-time pruning: a save
  already carrying a dangling node still restores it verbatim.
  Retirement is a delete, never a compaction (`pnsNextId` keeps
  advancing; a retired id is never reissued), and there is deliberately
  no public `power.removeNode`. Gates: `power_probe.py`,
  `power_workshop_probe.py`, `machine_shop_probe.py`, hspec
  `--match "power node demolition"`; pure algorithm in
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
  never from hashmap order — so ids survive save/load and chunk
  eviction. Consumers read the STORED values, never re-derive from the
  live registry. Lifecycle transitions are one-way (`promoteLifecycle`
  refuses backward AND same-state — what makes discovery fire exactly
  one event); `hinted` is deliberately unreachable but must NOT be
  deleted (positionally serialized, append-only enum). Stored fields,
  queries, and the frozen v1 DTO path: `docs/engine_contracts.md`
  §Location instances. Gates: hspec
  `--match "Location instance identity"`, `location_content_probe.py`.
- **Location + river naming, etymology (#1101/#1102/#1104)** — a placed
  instance's `name` is rendered in its PAGE's own generated language
  (from the identity's #1092 provenance); `gloss` is the same
  `NameExpr`'s English reading. Names are WRITE-ONCE (#708 principle 5):
  rendered by the single writer at creation (`newLocationInstance`;
  `buildRiverNames` at world init) and read thereafter — growing the
  concept catalogue never re-renders one. A page with NO provenance
  falls back to `ldLabel` with `gloss` ABSENT / an EMPTY river-name
  table — absence is never papered over by inventing a language. River
  identity is `(WorldPageId, GeoFeatureId)`; `World.River.Identity` is
  the ONE event/feature pairing and it is CHECKED before it is trusted —
  a violated invariant yields no id rather than a wrong one. Etymology
  (#1104) re-renders from the persisted `EtymologySource` and CHECKS
  against the stored text before showing any of it;
  `world.getEtymology`'s `pageId` names the TARGET only (#1265) — the
  recurrence set is always the ACTIVE page, so self-exclusion is
  PAGE-QUALIFIED. Concept pools, river identity detail, the panel
  hosting rule, the token trace and the frozen DTOs:
  `docs/engine_contracts.md` §Location and river naming + §Name
  etymology. Gates: hspec `--match "Location naming"` /
  `"River naming"` / `"River identity"` / `"Language etymology"` /
  `"Etymology panel"`; `river_naming_probe.py`,
  `location_content_probe.py`, `etymology_probe.py` (manual-only,
  `needs-gpu`).
- **Location discovery (#780, sight-based since #1230)** — a one-way
  promotion to `discovered`, fired when a player-faction unit SEES the
  location: its visible-tile set intersects the instance's stored
  `liBounds`, seam-aware, one tile being enough (the `discovery_margin`
  halo is GONE from YAML, def, instance, Lua and wire). Sight is
  `Unit.LineOfSight.visibleTilesOnPage` — the SAME calculation
  `unit.getVisibleTiles` runs, minus its `wmVisible` gate, so reveal
  works on a loaded-but-hidden page. Ticks for EVERY loaded page,
  independent of pause; emits exactly one `location_discovery` event. A
  night-scaled radius is intentionally shorter — any distance-sensitive
  expectation over `unit.getVisibleTiles` must pin the clock. Detail:
  `docs/engine_contracts.md` §Location discovery, map icons, and
  per-unit knowledge. Gates: `location_content_probe.py`,
  `location_embark_probe.py`; hspec `--match "Location discovery"` /
  `"Location map icons"` / `"Unit.LineOfSight"`.
- **Location map icons (#781/#1230)** — all six lifecycle constructors
  map explicitly (`World.Render.Zoom.Icons.locationIconAppearance`):
  `unknown`/`hinted` draw the ONE shared `location_unknown.png` so the
  zoom map never leaks WHAT is there before a unit has seen it;
  `discovered`/`active` draw the def's own `map_icon`;
  `cleared`/`depleted` draw that SAME bitmap darkened — an explicit,
  enumerated exception to the no-tinting rule, confined to the icon
  quad. A def with no `map_icon` places no annotation. Asset gate:
  `tools/location_map_icon_asset_check.py`.
- **Per-unit location knowledge (#915)** — the EXPERIENTIAL layer beside
  that CARTOGRAPHIC one, and neither derives from the other: global
  lifecycle = "the player has mapped it", `aiState[uid].knownLocations`
  = "this acolyte knows where it is". Keyed by the durable `(page,
  instance id)` pair — dedup is by IDENTITY, never by distance (don't
  copy `knownWaterSources`' 6-tile rule across). Both layers come from
  ONE containment enumeration in `Location.Discovery`, so they cannot
  drift; awareness ignores lifecycle, so a unit arriving at an
  already-mapped ruin still learns it. Persisted via `lua.unit_ai` v4
  typed refs; v1-v3 decode with the field ABSENT, never inferred.
  Gates: hspec `--match "unit location knowledge"`,
  `location_content_probe.py`.
- **Expedition retrieval (#920)** — recovering a remote item uses ONLY
  the direct-RTS verbs a player already has (`unitAi.commandPickup` →
  `commandMove` home → adjacent `unit.depositToCargo`, that last step
  the LAX verb, not a player gesture);
  `docs/expedition_gameplay_loop.md` forbids a caravan/logistics
  interface until direct retrieval proves inadequate. `commandPickup`
  gates capacity at COMMAND time AND again on ARRIVAL — keep both; the
  load changes en route. `pickup_timeout`/`TASK_TIMEOUT_SEC` are STALL
  timers spent in ELIGIBLE time only (#1291), never trip budgets — they
  reset on a new closest approach, and the budget still ACCUMULATES
  across interruptions so no order becomes immortal. The accounting and
  versioning: `docs/engine_contracts.md` §Commanded-order stall budget.
  Gates: `expedition_retrieval_probe.py` (manual-only), hspec
  `--match "commanded order stall budget"`.
- **The expedition loop (#923)** — the shipped slice is **prepare →
  travel → discover → extract → return → invest**, run as ONE session by
  `tools/expedition_loop_probe.py` (manual-only, fixed-seed, ~15 min,
  two engine boots); `docs/expedition_gameplay_loop.md` is the design
  authority (step 9's combat encounter and progression reward are
  deferred, #916/#917). The gate pins: the colony comes from a real
  `acolyte_portal` and its OWN roster, never hand-spawned units; the
  expected end lifecycle is `discovered` with contents spawned exactly
  once (a gate calling `setLocationLifecycle` would be asserting its own
  writes); and every durable identity is re-checked in a FRESH PROCESS.
  It also runs an **unprepared control** — a second traveller sharing
  ONE identical leg, differing only in FOOD — which must end measurably
  worse off, which is what makes the scenario prove preparation matters
  rather than prove a walk succeeds. The six conditions that keep that
  comparison honest, and the two live-observed physiology traps, are in
  `docs/engine_contracts.md` §The expedition loop. Read it before
  editing this gate.
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
- **Autosave (#913)** — OFF by default (`config/save_default.yaml` +
  key-level `save.local.yaml` overlay; Settings → General edits it).
  `scripts/autosave.lua` owns the WALL-CLOCK interval and fires only
  when `uiManager.isGameplayView()` — a deadline reached in a menu /
  with no world / mid save-or-load is SKIPPED silently, and menus never
  suspend or reset the cadence. Slots are the reserved `autosave-<n>`
  family, `autosave-1` newest; ownership is the durable `smAutosave`
  metadata flag (`"metadata"` v2; v1 migrates to manual), NEVER the
  name — a manual save squatting on one of those names fails the
  attempt with nothing rotated. PUBLISH FIRST, ROTATE SECOND, and an
  interruption leaves a partially shifted family, never a shorter one. A
  FAILED autosave stays paused and zero-scaled. Staging, rotation
  ordering and the `playerIntentGenRef` mutex:
  `docs/engine_contracts.md` §Autosave. Gate: `autosave_probe.py`
  (manual-only).
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

  **A neutral placeholder is NOT promoted (#1937).** Those tracked legacy
  files hold the versioned default's own content, and copying that was
  never a no-op — it froze the then-current defaults as durable local
  state that outranks the template for ever after, so a revised shipped
  value never reached anyone who booted once and never saved. Video and
  keybindings therefore pass a `LegacyNeutralityCheck`: a legacy file
  whose DECODED value (not its bytes) equals the tracked
  `_default.yaml`'s is recognized, not copied — the local file stays
  absent and the log line is deliberately not the migration line. The
  determination is recorded in a gitignored
  `config/*.legacy-neutral.local.yaml` so a LATER revision of that template
  cannot make the untouched placeholder look like player state; a legacy
  file the player really edited still migrates, with the unchanged
  `Migrated legacy config <legacy> -> <local>` message. Notifications get
  no check (`Nothing`) and keep the unconditional copy: they have no
  tracked template to be neutral against, and an absent overrides file
  already defers to `data/notification_categories.yaml`. Gates: hspec
  `--match "config"`, `tools/config_migration_probe.py`.

  **A headless spec that drives a production path which WRITES `config/`
  must wrap `Test.Headless.Harness.Isolation.withIsolatedResourceRoot`
  AROUND `withHeadlessEngine`** (#1357, enforcing #1266's "tests never
  modify, truncate or regenerate the developer's `config/*.local.yaml`").
  Outside, never inside — engine init is itself a writer. Why the
  fixture is built the way it is (a fresh exclusive scratch root, no
  marker files) and the two suites that need it:
  `docs/engine_contracts.md` §Config-writing tests. Gates:
  `config_state_probe.py`, `config_migration_probe.py`; hspec
  `--match "config"`, `--match "Settings Defaults keybind persistence"`.

## Save / Load

**Persistence contract:** [`docs/persistence_contract.md`](docs/persistence_contract.md)
is the authoritative contract for what a save represents and how every
piece of engine/Lua state is classified;
[`docs/persistence_state_inventory.md`](docs/persistence_state_inventory.md)
is the field-by-field classification, enforced by
`tools/persistence_inventory_audit.py` (in `make ci`/CI — fails when a
new root state owner, Lua save module, component, or typed reference
kind lacks a classification/coverage row). Read the contract before
adding state to `EngineEnv`, `WorldState`, `World.Save.Types`, the three
gameplay managers `EngineEnv` points at (`UnitManager`,
`BuildingManager`, `UnitThreadState` — scanned directly since #1703), or
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
tag — **append-only**. Inserting/reordering silently corrupts saves. A
constructor's own FIELDS are positional too, so reordering them or
changing one field's serialized type corrupts saves the same way while
moving no tag (#1270).

Anything beyond appending is a **per-component migration**, never a
`currentSaveVersion` change — that marker does not gate on-disk
compatibility (see the architecture note below). Find EVERY component
storing the enum — `Direction` is stored by both `units`
(`UnitInstanceDTO.uidFacing`) and `unit-sim`
(`UnitSimStateDTO.simFacing`), while `Pose` and `UnitActivity` are
`unit-sim`'s alone — and for each: raise its `csVersion`, freeze the
outgoing DTO, and register that frozen type in `csOlderVersions` via
`atVersion` with an explicit migration. `componentCodec` derives
`ccInputVers` from those declarations, so the reader gains the new
version while retaining every version it already accepted.

Retaining a version means still DECODING it, so freezing the OUTGOING
DTO is only half the job: **every** version left in `csOlderVersions`
needs a wire type that reaches a frozen COPY of the constructor order
that version was written with — transitively, the `Pose` nested in
`UnitActivity` included. Today's frozen DTOs do not satisfy that.
`UnitSimStateDTOv1` (which `unit-sim` v1 AND v2 both decode through)
still names the live `Pose`/`UnitActivity`/`Direction`, and
`UnitInstanceDTOv1.uid1Facing` still names the live `Direction`, so a
reorder that froze only the current shape would decode every retained
legacy payload against the new order anyway. `unitSimCodec`'s v1/v2
entries are the exemplar for version dispatch and explicit migration
only — no codec has needed a frozen enum yet, so they do not
demonstrate that half.

Enforced since #1145 by `tools/enum_append_only_audit.py` (CI + `make
ci`, with its own `--self-test`), which is the authority on which types
are guarded and why — read its module docstring before adding, moving,
or changing one. It guards **every** multi-constructor `data`
declaration under `src/`/`app/` deriving `Serialize` through `Generic` —
a deliberate superset of "reachable from a save component" (the audit
prints the guarded count on every run), so a type that becomes persisted
later was already guarded the day its instance was derived.
`docs/save_compat/enum_baseline.json` is the GENERATED golden
constructor list; don't hand-edit it — a pure append ratchets it with
`--update-baseline`, and anything else is a wire-format break the audit
refuses to record. Payload-slot normalization and the split against
`tools/save_compat_audit.py`: `docs/engine_contracts.md` §Enum
append-only audit.

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
`atlas/` directory is DERIVED and nobody hand-edits it (`atlas/` is a
SIBLING of `animations/`, which keeps generated artifacts outside the
filesystem-first inventory walk).

**One command covers all four concerns:**
`python3 tools/pack_atlas.py --validate-only --strict` runs the art
inventory, the compiler's freshness comparison, AND the two budgets. Add
`--compile [--unit <name>]` to regenerate, or `--compile --check` to
report staleness without writing. Deps are pinned in
`tools/requirements-assets.txt` (PyYAML + Pillow), spelled again in
`.github/ci/Dockerfile` (`test_pack_atlas.py` fails if the two drift).
Pillow is load-bearing for VALIDATION too: an absent decoder is one
loud error naming the install command, never a silent skip.

### Inventory (#1257, #1311)

Discovery is **filesystem-first**: it walks every PNG under
`assets/textures/units/<unit>/animations/<animation>/<direction>/` and
checks the declarations against it, never the other way round. **Every
committed animation PNG is owned by exactly one animation-frame
declaration; there is no directory or glob exemption mechanism.** Scope
is `animations/` — non-animation unit textures (`sprite`,
`directional_sprites`, `portrait`) are existence-checked only, and
**"duplicate" means duplicate ANIMATION-FRAME claims only** — reusing an
animation frame as a `sprite`, `directional_sprites` entry, or
`portrait` is deliberately legal.

Two declaration forms live under `data/units/`, and the top-level key is
the entire runtime distinction: `units:` (a gameplay unit — registers,
loads textures, lists, spawns; `name` + `sprite` mandatory) and
`asset_units:` (asset-only: exactly `name` + `animations`, as a
WHITELIST explicitly enforced by BOTH decoders; nothing registers,
loads, lists, or spawns them; shipped asset-only art remains validated,
previewable, and fixture-tested). A file holding
NEITHER key is refused rather than decoded as zero units (that is what a
mistyped top-level key looks like), and so is a key present with an
explicit `null` (aeson's `.:?` reads that as absent). Three decoders
share the shape: `UnitYamlFile`, `Engine.Preview.Unit`'s
`UnitAnimMetaFile`, and `pack_atlas.py`. Animation/direction keys are
strings, never coerced.

The structural invariants and the three independent CONTENT checks every
declared frame is put through (#1311) are enumerated in
`docs/engine_contracts.md` §Unit animation art — read it before adding,
relaxing or "simplifying" a rule there; the three content checks each
have a fixture the other two accept, so do not fold them into one.

**Deleting art needs the owner's explicit confirmation** (#1257 R4):
present an exact path-level classification first. #1257 deleted nothing —
all 695 previously-unowned paths were retained and declared.

### Compiler (#1258, TEX-2)

Output is **one atlas per ANIMATION** (D-2),
`assets/textures/units/<unit>/atlas/<animation>.png`, beside a generated
`atlas/index.json`. Rows are the AUTHORED directions in the engine's own
`Unit.Direction` order (five for `flip: true`, eight for `flip: false`);
columns are the max authored frame count, with the index recording each
direction's TRUE count — no padding slot is addressable. Cells are exact
integer copies of the source frames' decoded RGBA8 samples; a size
mismatch is a compile error, never an implicit rescale (D-6). Since
#2076 each cell sits one texel inside a `(cell+2) x (cell+2)` SLOT whose
gutter copies that cell's own edge texels outward, corners included — a
linear tap can no longer reach a neighbouring frame, and nearest is
untouched because the index still addresses the inner cell. The index
records that `cell_padding` as a required field at `schema_version` 2,
and carries two PER-ANIMATION `sha256` digests, so one animation's edit
never invalidates an unrelated atlas (D-12); rebuilds are deterministic
and incremental runs write only on real content differences.
`--validate-only` is index-aware: a unit with NO index is valid to the
TOOL (a legitimate working-copy state) but not to the ENGINE; an
existing index is REGENERATED and compared, so a tampered index cannot
certify a tampered atlas. The exact invariants:
`docs/engine_contracts.md` §Unit atlas compiler.

**Every shipped unit's atlases ARE committed**, so a fresh checkout runs
with no packer step; the corpus sits well under D-12's 2x on-disk
ceiling. Gates: `python3 tools/test_pack_atlas.py` (fixture-based,
isolated temp trees, never touching shipped assets) plus the strict run
— both unconditional in `make ci` and post-merge CI, path-selective on
PRs via `ci_expensive_gates.py --gate unit-assets`; hspec
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
  compared as `measured × roster_growth_factor > threshold` (strict `>`)
  against a 384 MiB threshold **confirmed by the project owner on
  2026-08-16** — raising it is the owner's call, not a maintenance edit.
  A breach IS D-10's precondition for resuming deferred TEX-5 (KTX2
  atlas loading). A single-unit `--unit` run deliberately does NOT
  evaluate this one.

Not to be confused with D-12's on-disk guardrail above: that is
repository size, this is resident memory, measured independently.

### Runtime (#1259/#1260/#1261, TEX-3/TEX-6)

**Every shipped unit uses the compiled path, and there is no other way
for a unit animation to load.** The per-frame representation and its
loader are GONE from the tree, not merely unused.

**Storage is a named SUM with one constructor, so no animation is
half-migrated.** `Unit.Types.Def.Animation` carries an `aStorage` of
`Unit.Atlas.Types.AnimStorage`, now exactly `StorageAtlas` — D-10's
"exactly one resident representation" is unrepresentable rather than
merely enforced, and the named type stays the seam a later
representation would be added at. Read frames through the
storage-neutral accessors — `storageFrameCount` / `storageFrameCounts` /
`storageMaxFrameCount` / `storageSampleAt` — never by matching the
constructor. **Buildings are not on this type at all**: they are never
compiled (D-8) and live on their own `Building.Types.BuildingAnimation`,
byte-for-byte the behaviour they had.

**The index is the whole answer, and failure is failure.**
`Unit.Atlas.Load.loadUnitAtlasIndex` reads, parses, decodes and verifies
EVERY declared atlas before `loadUnitYaml` allocates one handle or
queues one upload; `Unit.Atlas.Index.planUnitAtlasStorage` adds the
YAML-staleness half, including reverse coverage. A missing, incomplete,
stale, unsupported or malformed index refuses the whole unit definition,
naming unit, animation and artifact — no partial registration, nothing
to fall back to. The three validation passes, what each digest catches,
and the `pythonFloatRepr` pinning are in `docs/engine_contracts.md`
§Unit animation atlas runtime — read it before touching index parsing,
the digests, or the upload cache.

**`pickFrame` returns a `FrameSample`, and its arithmetic is FROZEN**
(D-3): the stable handle (#286 — never a slot), the frame's UV endpoints
within that handle's image, the frame's pixel dimensions when the
storage knows them, and the mirror flag. The only storage-dependent step
is the per-direction frame COUNT, which is the index's REAL count and
never the padded column count, so padding is unreachable by construction
(D-5). Non-rendering consumers of a clip's LENGTH read the real counts
too: `Unit.Thread.Command.Pose`'s pose-transition durations and
`unit.getAnimDuration` both go through `storageMaxFrameCount`.

**Cell dimensions size everything.** `frameDimensions` is the one funnel:
an atlas sample answers from its cell, a whole-image sample (the direct
default/directional sprite a T-pose falls back to) falls through to
`rvTextureSizeRef`. Nothing may measure an atlas handle's whole-image
entry where it means a frame — including hit testing, which sizes from
the SAME `pickFrame` sample the renderer draws (`Unit.HitTest.unitHitRect`,
shared by click and box selection).

**Mirroring reflects across the frame's own sub-rect**, never the whole
image — with atlases, `1-u` lands in a different cell.
`UI.Render.renderSpriteBatch` mirrors as `u' = su0 + su1 - u` over the
sprite's source sub-rect (`ussUV`); a whole-image sprite is the
unchanged `1-u`. Anything DISPLAYING a unit's live frame must use
`unit.getFrameSample`, not `unit.getFrameTexture` (which cannot describe
an atlas frame and would draw the whole sheet), and must publish it with
`UI.setSpriteFrame`, which lands texture, sub-rect and mirror in ONE
manager transition — the render thread reads the manager concurrently,
so separate setters leave a window pairing the new handle with the
previous frame's rect.

Atlas slots are registered PINNED to the nearest sampler with one mip
level (D-6), so a runtime `setTextureFilter` toggle cannot start
bilinearly resampling unit art; the upload path's path cache is
therefore policy-aware, and cell UVs sit on the LOGICAL cell's own exact
edges — one texel inside its padded slot — with no half-texel inset; the
#2076 extrusion gutter is what buys linear isolation, moving no sampled
texel. See `docs/engine_contracts.md` §Unit animation atlas runtime.

Gates: hspec `--match "pickFrame"` (the whole logical-choice matrix
checked against an independently written `expectedChoice` table, so an
edit to either side fails), `--match "Unit.Atlas"` (index
parsing/validation, digests against `pack_atlas.py`'s reference values,
real consumer geometry, a texel-level cell-vs-source comparison, and a
real on-disk fixture tree), and `--match "the real unit registration
boundary"` (drives `registerUnitDefs` against a live headless engine,
asserting on the messages actually queued: one atlas upload and one
published `Animation` per animation, no per-frame textures, a rejected
index queueing and publishing nothing, and every shipped unit through
the PRODUCTION resolver). Roster-wide headless evidence:
`tools/combat_anim_probe.py` (`--roster-only`), which reads the
texture-NAME registry (`engine.getTextureHandle`) rather than
`engine.getLoadedTexturePaths()` — the latter is EMPTY headless, where a
probe built on it would pass vacuously.

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
