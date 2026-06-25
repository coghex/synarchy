# Synarchy Copilot Instructions

## Build and test commands

- Build the production executable with `cabal build synarchy`.
- Build with development-only validation and debug settings with `cabal build -f dev all`.
- Run the app with `cabal run synarchy`.
- Run the full test suite with `cabal test -f dev` or `cabal run synarchy-test`.
- Run a single Hspec group or example with `cabal run synarchy-test -- --match 'Engine.Core.Monad'` or another `describe` / `it` name from `test/Spec.hs`.
- Use `ENGINE_DEBUG=Vulkan,Graphics,etc... cabal run synarchy` to enable category-based engine debug logging.

`test/Spec.hs` initializes GLFW and creates a real window before running `hspec`, so graphics-capable local environments are the expected way to run tests.

## High-level architecture

- `app/Main.hs` is the runtime entry point. It calls `Engine.Core.Init.initializeEngine`, starts the input, Lua, world, and unit worker threads, creates the GLFW window, initializes Vulkan, then hands control to `Engine.Loop.mainLoop`.
- Shared runtime state is split between `EngineEnv` and `EngineState` in `Engine.Core.State`. `EngineEnv` holds the long-lived IORefs and queues used across subsystems; `EngineState` holds mutable render-loop state such as timing, input, graphics, and scene management.
- Most engine code runs in `EngineM` from `Engine.Core.Monad`, a custom continuation-style monad that reads the environment and mutable state through STM-backed vars.
- The render thread stays in `Engine.Loop`: poll GLFW, process queued input, update camera state, process Lua messages, draw the frame, and manage shutdown.
- The worker threads are specialized:
  - `Engine.Input.Thread` consumes queued input, resolves focus/UI behavior, and forwards events to Lua or engine shutdown.
  - `Engine.Scripting.Lua.Thread` registers the Lua API, loads `scripts/init.lua`, runs scheduled Lua modules, and starts the TCP debug server used by `debug-console.sh`.
  - `World.Thread` drains world commands, advances world simulation/loading, and publishes world quads for rendering.
  - `Unit.Thread` processes unit commands and movement, then publishes render-visible unit state.
- UI and game flow are Lua-driven. The Lua bootstrap in `scripts/init.lua` loads the shell, debug, unit manager, and UI manager scripts, so many gameplay/UI changes require coordinated Haskell API and Lua script edits.
- Engine startup reads repository configuration from YAML files in `config/` (for example `video.yaml`, `keybinds.yaml`, and `world_gen_default.yaml`), while gameplay content is also driven by YAML-backed loaders in the engine and Lua scripts.

## Key conventions

- `NoImplicitPrelude` and `UnicodeSyntax` are enabled project-wide. Import `UPrelude` instead of `Prelude`, and preserve the existing Unicode-heavy style (`∷`, `→`, `⇒`, `⊚`, `⌦`, `≡`, `∧`, `∨`, `⊘`, `⊙`, etc.) when editing nearby code.
- Follow the repository's `Base.hs` / `Types.hs` split. `Base.hs` modules are intended to stay free of project-local imports; `Types.hs` and higher-level modules can depend on other local modules. This split is part of how the codebase avoids circular dependencies.
- When adding new shared queues, refs, or other global runtime state, wire them through both `Engine.Core.State.EngineEnv` and `Engine.Core.Init.initializeEngine`; one without the other is usually an incomplete change.
- The executable and the test suite both inherit language defaults from `synarchy.cabal` (`GHC2024`, `NoImplicitPrelude`, `UnicodeSyntax`, plus the repo warning/profile flags). Match those defaults instead of adding per-file workarounds unless the file already does so.
- The test harness is centralized in `test/Spec.hs`. Adding a new test module is not enough by itself: it also needs to be listed in `synarchy.cabal` and imported/attached to the `hspec` tree in `test/Spec.hs`, or it will never run.
- Use `scripts/` as the source of truth for runtime Lua modules. Changes to shell/UI/world behavior often require matching updates in `Engine.Scripting.Lua.API.*` on the Haskell side and the corresponding Lua modules under `scripts/`.
