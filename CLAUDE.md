# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

- **Build:** `cabal build all` (does NOT build test suites — use `cabal build synarchy-test-headless` explicitly)
- **Run:** `cabal run synarchy`
- **Run tests:** see **Testing Tiers** below — pick the cheapest tier that covers the change; don't run the gates as an iteration loop
- **Pre-push gate:** `make ci` runs the exact checks CI runs (`.github/workflows/ci.yml`) — warning-clean (`-Werror`) build of the library/exe + both test suites, the headless hspec suite, `test_audit.py`, and `world_check.py --quick` — so a green `make ci` predicts a green CI. It uses the default prod profile and your warm `dist-newstyle`, and restores any existing `cabal.project.local` on exit (see `tools/ci-local.sh`). Not an iteration loop — run it once before pushing.
- **Debug output:** Set `ENGINE_DEBUG=Vulkan,Graphics,etc...` environment variable

## Testing Tiers

Worldgen is the entire cost of the test stack (~10 s per w64 generation;
every non-worldgen test is milliseconds). The tiers exist so iteration
stays in seconds and the expensive gates run once, at the end.

1. **Iteration (seconds–1 min).** Targeted hspec:
   `cabal test synarchy-test-headless --test-options='--match "<describe name>"'`.
   For worldgen-output sanity: `python3 tools/world_check.py --quick`
   (6 tagged seeds × 1 dump, <1 min).
2. **Before reporting done (~3 min total).**
   `cabal test synarchy-test-headless` (~1 min — one engine, worlds
   memoized across specs) · `python3 tools/world_check.py` (21 seeds,
   ~2 min) · `python3 tools/test_audit.py` (instant).
3. **Worldgen-OUTPUT changes only (full tier).**
   `SYNARCHY_FULL_TESTS=1 cabal test synarchy-test-headless` (adds the
   w128 volcano exposure case, +~25 s), then re-capture baselines
   `python3 tools/world_baseline.py` (~7 min) and re-run world_check.
   Remember the save-version bump.
4. **Behavior probes — opt-in, not a default gate.** ~40 headless
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
   since parallel contention is what a retry needs to escape. Keep
   `--jobs 1` (the default) for the CI gate path.

Baselines (`tools/baselines/`) are **tracked in git** (#421): a fresh
clone/worktree can run world_check directly, and a tier-3 re-capture
lands in the PR diff where reviewers can see the intended drift. Don't
edit baseline JSON by hand — regenerate with `world_baseline.py`.

CI (`.github/workflows/ci.yml`, #436) runs on every PR and push to
master, on Linux: full build with `-Werror` (the #435 warning-clean
state is enforced, dependency warnings excluded), both test-suite
builds, the headless suite, `test_audit.py`, and `world_check --quick`
— all blocking. Worldgen output proved bit-identical between
macOS/aarch64 (where baselines are captured) and Linux/x86_64, so the
tracked baselines are platform-agnostic; a worldgen-output PR that
skips its tier-3 rebaseline fails CI.

On PRs, CI additionally runs a **path-selective, blocking behavior-probe
gate** (#530): `tools/ci_probes.py` diffs the PR against its base and
picks which behavior probes to run — only the ones relevant to the
changed files, the full curated set for a core/unclassified change
(fail-safe: anything the mapping can't classify runs everything), and
**zero** probes for docs/assets-only changes or paths whose probes are
manual-only. Selected probes run via
`run_probes.py --only ... --retries 1` (a failed probe re-runs SOLO once
before failing the PR, to absorb the sequential-engine contention flakes
a back-to-back run shows). Only a **curated CI-eligible smoke subset** is
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
`targeted`, or `unclassified` for a probe simply not yet reviewed for
promotion — never trust a comment enumerating probe names, they drift).
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

### UI system
`UI.*` handles focus management, text input, and UI rendering. UI layout and behavior is driven from Lua scripts.

## Project Layout

- `src/` — Library source (360+ modules)
- `app/Main.hs` — Executable entry point (draw loop)
- `test/` — hspec unit tests (engine core and Vulkan primitives)
- `cbits/` — C code (stb_truetype font rasterization, Lua debug FFI)
- `config/` — YAML config (keybinds, video settings)
- `data/` — Game data YAML (materials, vegetation, flora, units)
- `assets/` — Images and graphical resources
- `scripts/` — Lua scripts for game logic

## Headless Mode & Debug Console

The engine supports a headless mode for automated testing, scripted world generation, and agent workflows. No GPU, no window, no focus stealing.

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
# Create a world (name, seed, worldSize, plateCount)
echo 'world.init("test", 42, 256, 5)' | nc -w 2 localhost 8008

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
`craft.addBill(bid, recipeId[, count])` (count omitted/<1 = repeat
forever) validates the station offers the recipe's operation and
returns a bill id; `craft.getBill(s)`/`cancelBill` are the queue
surface (UI = #330), `claimBill(billId, uid, timeout)` /
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
`cbWorking` either — a paused-but-still-held bill keeps drawing.
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

No dedicated push/tiller animation exists yet — `standing_to_holding_
shovel` / `shoveling` are the closest hand-tool-on-ground visual (same
reuse-until-real-art convention as chop's pickaxe-swing stand-in for
felling). **The dedicated push animation + tiller prop art is tracked
separately as #517** (issue #333's "done when" text calls for it by
name, so this mechanism-only landing is a deliberate partial close —
see #517 for the exact frame paths/spec and the one-line
`till_equip_anim`/`till_work_anim` wiring once the art exists). The
toolbar icon, designation marker, and tilled-soil ground texture are
functional placeholders (correct size/alpha conventions, matching
sibling assets) pending real pixel art too.

`unit_ai.lua` sits at Lua's 200-local-per-chunk ceiling: a new work
action's helpers ride as fields on the existing `unitAi` module table
(`unitAi.till.*`, plain assignments) rather than one top-level `local`
per helper (the dig/chop convention) — adding even a couple of new
top-level locals broke script loading (`too many local variables`).
Any future action added the old way should check `grep -c "^local "
scripts/unit_ai.lua` stays at parity with what's on disk before adding
more top-level locals.

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

### Shutdown

```bash
echo 'engine.quit()' | nc -w 2 localhost 8008
```

### Tips for agents

- **NEVER launch `cabal run synarchy` or `cabal run exe:synarchy` without `--dump` or `--headless`** — this opens a graphical window that steals focus from the user
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

Save format version: see `currentSaveVersion` in `src/World/Save/Types.hs` (bumped frequently — don't trust any number written down here). Saves live under `saves/<name>/world.synworld` (binary) plus a human-readable `world_gen.yaml` alongside.

```bash
# From headless / debug console
echo 'engine.saveWorld("test", "my_save"); return "saved"' | nc -w 2 localhost 9008
echo 'engine.loadSave("my_save"); return "queued"' | nc -w 2 localhost 9008
# Loaded worlds land at pageId="main_world" regardless of original page name.
# You must call world.show("main_world") before tile/building/unit queries work.
# Auto-pause on save: the world loads paused; engine.setPaused(false) to resume.
```

What's preserved: gen-params + camera + time + climate + river flow, edited tiles (chunks regen + edits replay), buildings (with spawn-roster countdown), units (with stats / modifiers / skills / inventory / sim state), Lua AI memory (water locations, in-flight commands, source-drink phase, search-spiral state) and pause state. Saves from older schema versions are rejected with a clear "expected vN, got vM" error.

Not preserved (transient UI / in-progress state, by design): current selection (cursor selection, `umSelected`, `bmSelected`), build-tool placement mode (`buildTool.state` + `buildingGhostRef`), and the active toolbar tool. A loaded world always comes up on the **default tool**: `sdToolMode` is still recorded at save time but intentionally NOT restored — the load path resets engine `ToolMode` to `DefaultTool` (`World/Thread/Command/Save.hs`), and the `onSaveLoaded` broadcast (fired on *every* load — menu or debug-console `engine.loadSave`) resets the HUD toolbar to match (`scripts/ui_manager.lua` `onSaveLoaded` → `hud.selectDefaultTool`), so `world.getToolMode()` and the visible toolbar always agree after a load (#103). A fresh-session load lands in default UI state — the player re-selects, re-enters the build tool, etc. Within-session loads keep the singleton Lua state otherwise.

Enum schema policy: `Direction`, `Pose`, and `UnitActivity` derive `Generic Serialize`, which is **positional by constructor tag**. They are **append-only** — inserting or reordering constructors silently corrupts saved units' facing/pose/activity. New variants go at the end. Anything else (renames that change tag order, removals, semantic redefinitions) requires bumping `currentSaveVersion`. Same convention applies to any future enum that lands in `SaveData` via `Generic Serialize`.

Wait ~15 seconds after `loadSave` for a 128-world before querying — the center chunk gens synchronously but the rest queue progressively. Headless tests need to budget the wait.

Multi-world save regression: **`python3 tools/multiworld_save_probe.py`** — the #214/#219 gate. Generates two real world pages (active→`main_world` + a `second_world`), spawns a unit + building on each, saves, then does the gold-standard **save → quit → fresh restart → load** and asserts both pages' entities survive on the right page (cross-page negative checks included). `--port`/`--seed`/`--seed2`/`--size`/`--plates`. NB: it uses two `world.init` pages, not `world.initArena` — loading a save that contains an arena page currently hangs the world thread (#365), so arenas can't be a save-test secondary page.

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
