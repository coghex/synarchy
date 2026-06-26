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
    -- world.setTimeScale needs a pageId. Resolve "the current world"
    -- once and pass it through. If no world is active (main menu,
    -- mid-transition) the time-scale dance is a no-op — pausing the
    -- menu doesn't need to freeze a world's clock.
    local wid = world.getActiveWorldId()

    -- Check against the engine flag (source of truth), not the local
    -- mirror. The mirror can be stale after a path that flips
    -- enginePausedRef directly (auto-pause-on-save in saveWorldFn
    -- and Save.hs:60). The mirror below is kept as a cache so the
    -- save hook doesn't need an extra engine.isPaused() call.
    if b == engine.isPaused() then
        -- Engine flag already matches, so this is normally a no-op. But
        -- enginePausedRef can be set WITHOUT going through this module —
        -- a notification category with `pause: true`
        -- (Engine.PlayerEvent.Emit) or auto-pause-on-save flips it
        -- directly and never zeroes wsTimeScaleRef. That leaves a
        -- half-paused world: ticks frozen, but world time still
        -- advancing. Re-pausing must HEAL that split so the clock
        -- follows the flag (and a save doesn't persist sdEnginePaused
        -- = true alongside a live sdTimeScale). Only act when pausing
        -- and the clock is still live; if it's already zero the
        -- snapshot in prevTimeScale is the player's real scale and must
        -- be preserved.
        if b and wid and world.getTimeScale(wid) ~= 0 then
            pause.prevTimeScale = world.getTimeScale(wid)
            world.setTimeScale(wid, 0)
        end
        return
    end
    pause.paused = b
    engine.setPaused(b)

    if wid then
        if b then
            -- Snapshot the player's chosen time-scale BEFORE zeroing
            -- so a fast-forward (e.g. setTimeScale 10) survives the
            -- pause cycle. Pre-fix this was a documented-but-broken
            -- promise: `world.setTimeScale(0)` was being called with
            -- no pageId and silently no-op'd, and prevTimeScale was
            -- never read from the engine.
            pause.prevTimeScale = world.getTimeScale(wid)
            world.setTimeScale(wid, 0)
        else
            world.setTimeScale(wid, pause.prevTimeScale)
        end
    end
    engine.logInfo("Game " .. (b and "paused" or "resumed"))
end

function pause.toggle()
    -- Toggle off the engine flag, not the local mirror. See pause.set
    -- for why — the mirror can be stale after auto-pause-on-save.
    pause.set(not engine.isPaused())
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
            -- Ignore t.paused: the engine flag is authoritative at this
            -- point (engine restored enginePausedRef from sdEnginePaused
            -- before this Lua hook fires). The blob field is kept for
            -- forward-compat with v6 saves but is no longer load-bearing.
            pause.paused        = engine.isPaused()
            pause.prevTimeScale = t.prevTimeScale or 1.0
        end)
end

function pause.shutdown()
    engine.logInfo("Pause module shut down")
end

return pause
