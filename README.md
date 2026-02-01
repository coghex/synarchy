# Synarchy

synarchy is a modularized graphics engine for 2D sprites and animations written in haskell using the vulkan bindings.

## Prerequisites

to run you just need vulkan installed, for development you need the vulkan sdk, glslang, and validation layers installed, as well as GHC (GHC2024) and cabal (>3.4), any other libraries will be installed by cabal.  This project has only been tested on macos, but will work on linux as well, and windows with some modifications.

## Building

for regular use, `cabal build synarchy`, for development use `cabal build -f dev all` and you will get validation layers and debug output

## Usage

to run the program, use `cabal run synarchy`, to run the tests use `cabal -f dev test`, or you can of course run the binary directly. user "ENGINE_DEBUG=Vulkan,Graphics,etc..." do get debug output from those modules.

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
        - UI
            - code for the ui system
        - Loop
            - main loop and timing
    - app (here Main.hs controls the draw loop and spawns the child threads)
    - assets (images and other data)
    - cbits (c code for text rasterization library)
    - config (yaml config files)
    - scripts (lua scripts)
    - test (unit tests are really just for the base engine and most of the functionality is tested in the app)

## Task List

- [x] initialize Vulkan and GLFW
- [x] load and render multiple textured sprites
- [x] multiple shader support through asset manager
- [x] sprite manager for batch processing
- [x] scene creation and switching
- [ ] animation
- [ ] hotloading

## Known Issues

- CallStack traces dont show the whole callstack, only where it is called in Error.Exception
- on macos you will get junk in stdout, apple says there is no way around this, which is wild, even redirecting stdout doesnt work
