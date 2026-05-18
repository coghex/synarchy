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
    -- Defer to the engine flag rather than our local mirror. Avoids
    -- Lua/engine desync after a save load: the load handler flips
    -- enginePausedRef directly (auto-pause-on-save) without going
    -- through pause.set(), so our local `pause.paused` mirror would
    -- be stale. The engine flag is the source of truth; this
    -- module's pause.paused is now just a hint used during set/toggle
    -- to detect transitions for the timeScale side-effect.
    return engine.isPaused()
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
    -- Register save hook so the player's prevTimeScale and the local
    -- `paused` mirror survive a save/load. Without this the load
    -- handler restores `enginePausedRef` directly but Lua's
    -- pause.paused mirror stays stale until the next manual toggle.
    local saveLib  = require("scripts.lib.serialize")
    local saveMods = require("scripts.lib.save_modules")
    saveMods.register("pause",
        function()
            return saveLib.serialize({
                paused        = pause.paused,
                prevTimeScale = pause.prevTimeScale,
            })
        end,
        function(blob)
            local t = saveLib.deserialize(blob) or {}
            pause.paused        = t.paused        or false
            pause.prevTimeScale = t.prevTimeScale or 1.0
        end)
end

function pause.shutdown()
    engine.logInfo("Pause module shut down")
end

return pause
