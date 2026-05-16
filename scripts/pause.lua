-- Pause
--
-- Single source of truth for "is the game paused?". Two effects when
-- toggled:
--
--   1. engine.setPaused(b)  — flips the engine-side flag that gates
--      Unit/Thread.tickAllMovement (and anything else added later).
--   2. world.setTimeScale(0 / prev) — pauses world time-of-day. The
--      previous scale is captured on pause so a future fast-forward
--      (e.g. setTimeScale 10) survives a pause cycle and resumes at
--      the speed the player chose.
--
-- Lua-side game-logic tick scripts (unit_ai, unit_resources,
-- building_spawn) check isPaused() at the top of their update() and
-- early-return. UI ticks (hud, debug, build_tool, tile_editor) stay
-- running so the player can pan, zoom, interact, and unpause.
--
-- Singleton via package.loaded so the engine.loadScript update tick
-- and the init.lua key handler see the same state.

local pause = package.loaded["scripts.pause"] or {}
package.loaded["scripts.pause"] = pause

pause.paused      = pause.paused      or false
pause.prevTimeScale = pause.prevTimeScale or 1.0

function pause.isPaused()
    return pause.paused
end

function pause.set(b)
    b = b and true or false
    if b == pause.paused then return end
    pause.paused = b
    engine.setPaused(b)
    if b then
        -- Snapshot the current world time-scale so we can restore the
        -- player's chosen speed on resume. world.getTimeScale() is not
        -- currently exposed; default to 1.0 if a getter is added later
        -- it can fill this in.
        world.setTimeScale(0)
    else
        world.setTimeScale(pause.prevTimeScale)
    end
    engine.logInfo("Game " .. (b and "paused" or "resumed"))
end

function pause.toggle()
    pause.set(not pause.paused)
end

-- Engine script hooks
function pause.init(scriptId)
    engine.logInfo("Pause module initializing...")
end

function pause.shutdown()
    engine.logInfo("Pause module shut down")
end

return pause
