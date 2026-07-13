# Synarchy

Synarchy is a Haskell/Vulkan engine. It runs Ecce Homo, a colony/survival simulation game with procedurally generated worlds. This repository holds both: the engine (`src/Engine`) and the game built on it (world generation, units, combat, construction, crafting, UI — Haskell + Lua).

## Prerequisites

Vulkan to run. Vulkan SDK, glslang, and validation layers to develop, plus GHC (GHC2024) and cabal (>=3.16, matching `synarchy.cabal`'s `cabal-version` field) — other dependencies are installed by cabal. Developed primarily on macOS; works on Linux with minor adjustments (see `CLAUDE.md`).

## Building

`cabal build synarchy`, or `cabal build all` to build the library and executable together (this does **not** build the test suites — see Testing below).

## Usage

`cabal run synarchy` to run the game. `ENGINE_DEBUG=Vulkan,Graphics,etc...` for debug output from those subsystems.

The engine also has a **headless mode** (no window, no GPU) for scripted world generation, automated testing, and agent workflows — see the Debug Console section below and `CLAUDE.md` for the full headless API (world generation, unit/combat queries, save/load, dump mode).

### Resource root (running from outside the repo)

All runtime resources — `scripts/`, `assets/`, `data/`, `config/` — are loaded relative to a single **resource root**. By default that's the current working directory, so running from the repo root (as `cabal run` does) needs no flags. To launch the built executable from any other directory, point it at a checkout with the `--resource-root` flag or the `SYNARCHY_ROOT` environment variable (the flag wins if both are set):

```bash
/path/to/built/synarchy --headless --resource-root /path/to/synarchy-checkout
SYNARCHY_ROOT=/path/to/synarchy-checkout /path/to/built/synarchy --dump
```

The root is validated at startup: a nonexistent root, or one missing any of the four resource directories, fails immediately with an error naming the root in use and the missing paths. `python3 tools/resource_root_probe.py` regression-tests this contract end to end.

## Testing

`cabal test` builds and runs the test suites. `synarchy-test-headless` (hspec unit/integration tests, no GPU) is the one to reach for during iteration:

```bash
cabal test synarchy-test-headless
```

`tools/` also has a large suite of Python-driven checks: world-generation regression tools (`world_check.py`, `world_audit.py`, ...), documented in `tools/README.md`, and ~20 headless **behavior probes** that boot a real headless engine to regression-test specific systems (combat animation, movement, physiology, crafting, construction, save/load, and more). `CLAUDE.md` documents each probe and which tier of tests to run for a given kind of change.

## Gameplay & simulation systems

- **World generation** (`World.Generate`, `World.Geology`, `World.Hydrology`, `World.Fluid`, `World.Flora`) — tectonic plates, erosion, rivers, glaciers, lakes, ore deposits, and vegetation placement over a chunked, cylindrical (wraparound) world with LOD-based zoom rendering.
- **Units & AI** (`Unit.*`, driven from `scripts/unit_ai.lua` and friends) — pathing, movement, wildlife and NPC behavior, derived work roles (miner/woodcutter/builder/smith/laborer), and a physiology/homeostasis model (thermoregulation, hunger, stamina).
- **Combat & injury** (`Combat.*`) — layered penetration damage, weapon/armor degradation, wounds, infection, and a medical treatment arc, surfaced through combat/injury log panels.
- **Construction & crafting** (`Building.*`, `Structure.*`, `Craft.*`, `Item.*`, `Equipment.*`) — buildings and structure pieces raised via designation + build AI, a recipe-driven crafting system with work stations, and item instances with condition/sharpness and repair.
- **Locations** (`Location.*`) — premade structure stamps placed into procedural worlds.
- **UI** (`UI.*`) — focus management, text input, and rendering, with layout and behavior driven from Lua.

## Project structure

- `README`, `CHANGELOG`, `LICENSE`, cabal file
- `src/` — library source (360+ modules)
  - `UPrelude` — unicode prelude and shared utilities (imported instead of the standard `Prelude`)
  - `Math` — math utilities and types
  - `Engine/`
    - `Asset` — loading and managing assets
    - `Core` — the `EngineM` continuation-passing monad, state, and resource handling
    - `Event` — event handling
    - `Graphics` — Vulkan rendering, windowing/input (GLFW), font rendering
    - `Input` — input handling
    - `Scene` — scene graph, batching, and rendering
    - `Scripting` — the Lua scripting bridge and its API modules
    - `Loop` — main loop and timing
  - `World` — procedural world generation, geology, hydrology, fluids, flora, chunk/tile state, save/load
  - `Unit` — units, movement, pathing, line of sight, animation
  - `Combat` — damage resolution and wounds
  - `Craft` — recipe execution
  - `Building` / `Structure` — buildings and structure pieces
  - `Item` / `Equipment` / `Substance` / `LootTable` — items, equipment, materials, loot tables
  - `Location` — premade structure stamps
  - `Infection` — wound infection simulation
  - `Sim` — simulation glue (e.g. active fluids)
  - `UI` — focus, text input, tooltips, UI rendering
- `app/Main.hs` — executable entry point (draw loop)
- `test/`, `test-headless/` — hspec test suites (`synarchy-test-graphical`, `synarchy-test-headless`)
- `tools/` — Python worldgen regression tools (see `tools/README.md`) and headless behavior probes (see `CLAUDE.md`)
- `cbits/` — C code (stb_truetype font rasterization, Lua debug FFI)
- `config/` — YAML config. Versioned defaults/templates (tracked):
  `keybinds_default.yaml`, `video_default.yaml`, `pathing.yaml`,
  `world_gen_default.yaml`. Local runtime state (gitignored,
  machine-specific, written by the settings UI's Save actions):
  `keybinds.local.yaml`, `video.local.yaml`, `notifications.local.yaml`
  — absent on a fresh clone; the engine falls back to the matching
  `_default.yaml` template (keybinds/video) or materializes the file
  from `data/notification_categories.yaml`'s defaults (notifications)
  until the first Save creates it (#638). `keybinds.yaml`, `video.yaml`,
  and `notifications.yaml` are ALSO tracked, but only as a one-time
  upgrade source for a pre-#786 direct update (#786) — never read or
  written after the local file exists
- `data/` — game data YAML (buildings, items, units, flora, recipes, materials, structure packs, ...)
- `assets/` — textures and fonts
- `scripts/` — Lua scripts driving game logic (UI, AI, world management, item/building loaders)
- `docs/` — design notes and audit history

## Debug Console

The engine runs a TCP debug server on port 8008 that accepts Lua commands. Use it to inspect and manipulate the running engine in real time.

### Interactive REPL

```bash
./debug-console.py
```

Or run `python3 debug-console.py` if the script is not executable.

### Single commands

```bash
# run one command and print the result
./debug-console.py -c 'return camera.getPosition()'

# or use nc/netcat directly
printf 'return 2 + 2\n' | nc -w 2 localhost 8008
```

### Scripting multiple commands

Each command must be a single line. For multi-statement scripts, separate with semicolons or chain with `printf`:

```bash
printf 'world.init("test", 42, 64, 10)\n' | nc -w 2 localhost 8008
sleep 8
printf 'world.show("test")\n' | nc -w 2 localhost 8008
printf 'camera.goToTile(100, 93)\n' | nc -w 2 localhost 8008
```

### Available APIs

The debug console has access to the full Lua environment. Key namespaces:

- `world` — world creation and queries (`init`, `show`, `getTerrainAt`, `getFluidAt`, `getSurfaceAt`, `getChunkInfo`, `getAreaFluid`, `getRivers`, `getInitProgress`)
- `camera` — camera control (`goToTile`, `getPosition`)
- `engine` — engine control (`quit`)

`CLAUDE.md` documents the much larger headless/dump API surface (unit and combat queries, construction, crafting, save/load, and the various turnkey test probes) in detail.

### Configuration

The default port is 8008. Pass `--port` to both the engine and the client to use a different one, e.g. `cabal run synarchy -- --headless --port 9008` and `./debug-console.py --port 9008`.

## Known Issues

- On macOS you will get junk in stdout — Apple says there is no way around this, which is wild, even redirecting stdout doesn't work.

## Contributing

Issues and PRs are welcome — bug reports, feature requests, and design discussion included. Read `CLAUDE.md` first: it covers build commands, testing tiers, save/load conventions, and headless-agent workflows.

## License

MIT — see `LICENSE`.
