# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

- **Build:** `cabal build all` (does NOT build test suites — use `cabal build synarchy-test-headless` explicitly)
- **Run:** `cabal run synarchy`
- **Run tests:** see **Testing Tiers** below — pick the cheapest tier that covers the change; don't run the gates as an iteration loop
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

**Do NOT use `-f dev` for routine work.** The project is ~300 modules; switching between the `dev` and production flag profiles forces a full rebuild that takes about five minutes. The default (prod) profile is what the test suite, dump tool, and binaries are expected to run under, and what every code change should be validated against.

The `dev` flag enables Vulkan validation layers, address sanitizer on macOS, and `ENGINE_DEBUG` plumbing. Reach for it only when actively chasing a graphics or memory bug, and tell the user before switching — they may want to flip back afterward to avoid the rebuild cost on the next change. Production builds use `-O2 -optc-O3`.

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
`Engine.Scripting.Lua.*` provides a Lua API for game logic. Lua scripts live in `.claude/scripts/` (UI, menus, HUD, world management). The API modules in `Engine.Scripting.Lua.API.*` expose engine functionality to Lua.

### UI system
`UI.*` handles focus management, text input, and UI rendering. UI layout and behavior is driven from Lua scripts.

## Project Layout

- `src/` — Library source (260+ modules)
- `app/Main.hs` — Executable entry point (draw loop)
- `test/` — hspec unit tests (engine core and Vulkan primitives)
- `cbits/` — C code (stb_truetype font rasterization, Lua debug FFI)
- `config/` — YAML config (keybinds, video settings)
- `data/` — Game data YAML (materials, vegetation, flora, units)
- `assets/` — Images and graphical resources
- `.claude/scripts/` — Lua scripts for game logic

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
# Create a world (name, seed, worldSize, numAges)
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
on a death animation). `--attacker`/`--target`/`--seed`/`--port`/`--seconds`.

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
in walking — the diagonal-corner-cut stuck-unit bug). `--course`/`--unit`/
`--speed`/`--seconds`/`--port`; `--list` lists courses.

Note: `startFall` clears the move target on landing (AI re-issues after
recovery), so a unit can't reach a goal across a fall in one `moveTo` —
fall checks assert the fall mechanic + landing z, not arrival.

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

Save format version: see `currentSaveVersion` in `src/World/Save/Types.hs` (bumped frequently — don't trust any number written down here; v30 as of the ore-deposit work, 2026-06). Saves live under `saves/<name>/world.synworld` (binary) plus a human-readable `world_gen.yaml` alongside.

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
