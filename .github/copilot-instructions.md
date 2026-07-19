# Synarchy Copilot Instructions

## Build and test commands

- Build with `cabal build all` (this does NOT build test suites — build `synarchy-test-headless` explicitly when you need it).
- Run the app with `cabal run synarchy`. **Never run it without `--dump` or `--headless`** — the plain form opens a graphical window that steals focus.
- The day-to-day test suite is `synarchy-test-headless` (`cabal test synarchy-test-headless`), not `synarchy-test`/`synarchy-test-graphical` — that other suite boots a real GLFW window and Vulkan device, so it only runs in a graphics-capable local environment, not in most agent/CI contexts. Prefer the cheapest applicable tier:
  1. Iteration: `cabal test synarchy-test-headless --test-options='--match "<describe name>"'`, or `python3 tools/world_check.py --quick` for worldgen-output sanity.
  2. Before reporting done: run targeted tests plus only the relevant probes, audits, or worldgen checks; CI runs full Hspec on every PR.
  3. Worldgen-OUTPUT changes only: `SYNARCHY_FULL_TESTS=1 cabal test synarchy-test-headless`, then re-capture baselines with `python3 tools/world_baseline.py` and re-run `world_check.py`.
  4. `tools/*_probe.py` — opt-in headless behavior probes (combat anim, movement, construction, saves, physiology, ...); run only the ones relevant to what you touched.
  See `CLAUDE.md` for the full tier rationale and `tools/README.md` for the probe list.
- Use `ENGINE_DEBUG=Vulkan,Graphics,etc... cabal run synarchy -- --headless` to enable category-based engine debug logging without opening a window.
- Don't build with `-f dev` for routine work — it forces a full rebuild (~1.5 min) and is only for chasing a graphics/memory bug; use its own build dir (`--builddir=dist-dev`) if you do.

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
- Engine startup reads repository configuration from YAML files in `config/` (for example `video.local.yaml`, `keybinds.local.yaml`, and `world_gen_default.yaml`), while gameplay content is also driven by YAML-backed loaders in the engine and Lua scripts.

## Key conventions

- `NoImplicitPrelude` and `UnicodeSyntax` are enabled project-wide. Import `UPrelude` instead of `Prelude`, and preserve the existing Unicode-heavy style (`∷`, `→`, `⇒`, `⊚`, `⌦`, `≡`, `∧`, `∨`, `⊘`, `⊙`, etc.) when editing nearby code.
- Follow the repository's `Base.hs` / `Types.hs` split. `Base.hs` modules are intended to stay free of project-local imports; `Types.hs` and higher-level modules can depend on other local modules. This split is part of how the codebase avoids circular dependencies.
- When adding new shared queues, refs, or other global runtime state, wire them through both `Engine.Core.State.EngineEnv` and `Engine.Core.Init.initializeEngine`; one without the other is usually an incomplete change.
- The executable and the test suite both inherit language defaults from `synarchy.cabal` (`GHC2024`, `NoImplicitPrelude`, `UnicodeSyntax`, plus the repo warning/profile flags). Match those defaults instead of adding per-file workarounds unless the file already does so.
- The headless test harness is centralized in `test-headless/Spec.hs` (the graphics-only `synarchy-test-graphical` suite has its own `test/Spec.hs`). Adding a new test module is not enough by itself: it also needs to be listed in `synarchy.cabal` and imported/attached to the `hspec` tree in the relevant `Spec.hs`, or it will never run. Worldgen specs should reuse the canonical shared world (`Test.Headless.Harness.sharedWorld`) rather than booting their own.
- Use `scripts/` as the source of truth for runtime Lua modules. Changes to shell/UI/world behavior often require matching updates in `Engine.Scripting.Lua.API.*` on the Haskell side and the corresponding Lua modules under `scripts/`.
