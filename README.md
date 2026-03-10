# Synarchy

synarchy is a modularized graphics engine for 2D sprites and animations written in haskell using the vulkan bindings.

## Prerequisites

to run you just need vulkan installed, for development you need the vulkan sdk, glslang, and validation layers installed, as well as GHC (GHC2024) and cabal (>3.4), any other libraries will be installed by cabal.  This project has only been tested on macos, but will work on linux as well, and windows with some modifications.

## Building

for regular use, `cabal build synarchy`, for development use `cabal build -f dev all` and you will get validation layers and debug output

## Usage

to run the program, use `cabal run synarchy`, to run the tests use `cabal -f dev test`, or you can of course run the binary directly. use "ENGINE_DEBUG=Vulkan,Graphics,etc..." to get debug output from those modules.

## Development

- data is split into `Base.hs` and `Types.hs`, base files have no local dependencies, and types files have everything else.
- the project is organized as follows:
    - README, CHANGELOG, LICENSE, cabal file
    - src
        - UPrelude (unicode and other utilities)
        - Math (math utilities and types)
        - Engine
            - Asset (loading and managing assets)
            - Core (main engine functionality/continuation monad/resource handling)
            - Event (event handling)
            - Graphics (vulkan and graphics functionality)
                - Vulkan (vulkan specific functionality)
                - Window (windowing and input handling (glfw))
                - Font (font rendering)
            - Input (input handling)
            - Scene (scene graph and rendering)
            - Scripting (lua scripting support)
            - Loop
                - main loop and timing
        - UI
            - code for the ui system
        - World
            - code for the world
    - app (here Main.hs controls the draw loop)
    - assets (images and other data)
    - cbits (c code for text rasterization library and lua debug info)
    - config (yaml config files)
    - scripts (lua scripts)
    - test (unit tests are really just for the base engine and most of the functionality is tested in the app)

## Task List

- [x] initialize Vulkan and GLFW
- [x] load and render multiple textured sprites
- [x] multiple shader support through asset manager
- [x] sprite manager for batch processing
- [x] scene creation and switching
- [x] UI
- [x] basic world generation
- [ ] animation
- [ ] hotloading
- [ ] game (working title: "Ecce Homo")

## Debug Console

The engine runs a TCP debug server on port 8008 that accepts Lua commands. Use it to inspect and manipulate the running engine in real time.

### Interactive REPL

```bash
./debug-console.sh
```

Install `rlwrap` (`brew install rlwrap`) for readline support (arrow key history, line editing).

### Single commands

```bash
# run one command and print the result
./debug-console.sh -c 'return camera.getPosition()'

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

Use `return` to get values back, e.g. `return world.getInitProgress()`.

### Configuration

Set `SYNARCHY_DEBUG_PORT` to change the port (default: 8008).

## Known Issues

- on macos you will get junk in stdout, apple says there is no way around this, which is wild, even redirecting stdout doesnt work
