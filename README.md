# Synarchy

Synarchy is a colony/survival simulation game with procedurally generated worlds, written in Haskell on a custom Vulkan renderer. Units live on a tile-based world with real geology, hydrology, and weather; they fight, build, craft, farm, and get hurt in a layered combat/injury/physiology simulation, all driven from a Lua scripting layer on top of the engine.

## Prerequisites

To run you just need Vulkan installed. For development you need the Vulkan SDK, glslang, and validation layers installed, as well as GHC (GHC2024) and cabal (>3.4) — any other libraries will be installed by cabal. This project is developed primarily on macOS but works on Linux with minor adjustments (see `CLAUDE.md`).

## Building

`cabal build synarchy`, or `cabal build all` to build the library and executable together (this does **not** build the test suites — see Testing below).

## Usage

To run the game, use `cabal run synarchy`. Use `ENGINE_DEBUG=Vulkan,Graphics,etc...` to get debug output from those subsystems.

The engine also supports a **headless mode** (no window, no GPU) for scripted world generation, automated testing, and agent workflows — see the Debug Console section below and `CLAUDE.md` for the full headless API (world generation, unit/combat queries, save/load, dump mode).

## Testing

`cabal test` builds and runs the test suites. `synarchy-test-headless` (hspec unit/integration tests, no GPU) is the one to reach for during iteration:

```bash
cabal test synarchy-test-headless
```

`tools/` also has a large suite of Python-driven checks: world-generation regression tools (`world_check.py`, `world_audit.py`, ...) and ~20 headless **behavior probes** that boot a real headless engine to regression-test specific systems (combat animation, movement, physiology, crafting, construction, save/load, and more — see `tools/README.md`). `CLAUDE.md` documents which tier of tests to run for a given kind of change.

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
- `tools/` — Python worldgen regression tools and headless behavior probes (see `tools/README.md`)
- `cbits/` — C code (stb_truetype font rasterization, Lua debug FFI)
- `config/` — YAML config (keybinds, video, pathing, notifications, world-gen defaults)
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
