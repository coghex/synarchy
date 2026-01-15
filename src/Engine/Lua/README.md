# Lua Integration Module

This directory contains the Lua integration subsystem for the synarchy engine.

## Overview

The Lua integration provides a complete scripting environment for game logic, allowing mods to be loaded dynamically and executed with deterministic tick-based scheduling.

## Modules

### Engine.Lua.Types
Defines the core data types for Lua integration:
- `LuaScript`: Represents a single Lua script with execution metadata
- `LuaScripts`: Type alias for managing multiple scripts (Map)
- `LuaEnv`: The Lua environment containing state and configuration
- `LuaState`: Execution state tracking
- `LuaError`: Error types for better error handling

### Engine.Lua.Base
Common utility functions for Lua operations:
- Logging utilities (`logLuaError`, `logLuaInfo`, `logLuaDebug`)
- Script management (`addScript`, `removeScript`, `getScript`, `updateScript`)
- LCM calculation for tick scheduling optimization
- Error formatting

### Engine.Lua.Init
Handles Lua environment initialization:
- `luaInit`: Initialize the Lua environment and load all mod scripts
- `loadModScripts`: Load all Lua scripts from the mod directory
- `executeInitScripts`: Execute all `initLua` functions from loaded scripts

### Engine.Lua.Thread
Manages the centralized Lua execution thread:
- `startLuaThread`: Start the Lua execution thread
- `runLuaLoop`: Main loop with deterministic tick-based scheduling
- `executeLuaScripts`: Execute all scripts that should run on the current tick
- `shutdownLuaThread`: Safely shutdown the Lua thread

## Usage

### Initialization

```haskell
import qualified Engine.Lua.Init as Lua
import qualified Engine.Core.Queue as Q

main = do
  logQueue ← Q.newQueue
  let modPath = "./mods"
  
  result ← Lua.luaInit logQueue modPath
  case result of
    Right luaEnv → do
      -- Lua environment initialized successfully
      -- Start the Lua thread
      threadState ← Lua.startLuaThread logQueue luaEnv
      -- ... rest of game logic ...
      -- Shutdown when done
      Lua.shutdownLuaThread logQueue luaEnv threadState
    Left err → 
      putStrLn $ "Failed to initialize Lua: " ⧺ show err
```

### Script Structure

Lua scripts in the `mods/` directory should follow this structure:

```lua
-- initLua is called once when the script is loaded
function initLua()
  print("Script initialized")
  -- Setup game state, register callbacks, etc.
end

-- runLua is called every tick (or at specified intervals)
function runLua(tick)
  -- Game logic that runs each tick
  print("Running at tick: " .. tick)
end
```

### Tick Intervals

Scripts can specify custom tick intervals by setting metadata. By default, scripts run every tick. The system uses LCM (Least Common Multiple) optimization to efficiently schedule scripts with different intervals.

## Design Principles

1. **Deterministic Execution**: Scripts are executed in a consistent order (sorted by name) to ensure reproducibility
2. **Tick-Based Scheduling**: All scripts run on a fixed tick schedule, not wall-clock time
3. **Thread Safety**: Uses STM (Software Transactional Memory) for safe concurrent access
4. **Graceful Error Handling**: Errors are logged to the event queue rather than crashing the engine
5. **Functional Design**: Emphasizes immutability and pure functions where applicable

## Configuration

- **Target Frame Rate**: 60 FPS (16.666ms per frame)
- **Mod Directory**: Configurable path (default: "./mods")
- **Default Tick Interval**: 1 (execute every tick)

## Error Handling

All errors are logged via the engine's event queue (`envEventQ`). The Lua subsystem will continue running even if individual scripts fail, ensuring robustness.

Error types:
- `LuaLoadError`: Script file could not be loaded
- `LuaExecutionError`: Error during script execution
- `LuaScriptNotFound`: Requested script not found
- `LuaStateError`: General Lua state error
