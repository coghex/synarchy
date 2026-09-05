-- Pause
--
-- Single source of truth for "is the game paused?". One call does it:
--
--   engine.setPaused(b) — flips the engine-side flag that gates
--   Unit/Thread.tickAllMovement (and anything else added later) AND,
--   in the same step, retimes the paused page's clock (World.Pause).
--   The page's chosen speed is captured when a pause epoch opens and
--   handed back when it closes, so a fast-forward (e.g. setTimeScale
--   10) survives a pause cycle and resumes at the speed the player
--   chose.
--
-- That pairing used to live HERE, and it could only ever hold for a
-- pause this module imposed (issue #1599). Several engine paths write
-- the pause flag without running a line of Lua — a notification
-- category with `pause: true` (Engine.PlayerEvent.Emit), the
-- acceptance of engine.saveWorld, a load publish — so the resume
-- branch was handing back whatever speed the last Lua-imposed pause
-- had recorded, 1.0 in an ordinary session. The engine now owns the
-- pair; this module owns the POLICY (who may pause, what a load
-- resumes at) and the player-facing logging.
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
-- Diagnostic MIRROR of the speed this module last saw a page paused
-- at, never the authority: the engine holds the real resume scale in
-- the paused page's own pause epoch, which is the only place that can
-- see an engine-imposed pause. Kept because it is the observable
-- tools/transactional_load_probe.py pins for "a rejected unpause
-- applied no time-scale side effect".
pause.prevTimeScale = pause.prevTimeScale or 1.0

function pause.isPaused()
    -- Defer to the engine flag rather than our local mirror. Avoids
    -- Lua/engine desync after a save load: the load handler flips
    -- enginePausedRef directly (auto-pause-on-save) without going
    -- through pause.set(), so our local `pause.paused` mirror would
    -- be stale. The engine flag is the source of truth; this
    -- module's pause.paused is now just a hint used during set/toggle
    -- to detect transitions for logging.
    return engine.isPaused()
end

function pause.set(b)
    b = b and true or false
    -- Check against the engine flag (source of truth), not the local
    -- mirror. The mirror can be stale after a path that flips
    -- enginePausedRef directly (a `pause: true` notification, the
    -- acceptance of a save, a load publish).
    if b == engine.isPaused() then
        -- Already in the requested state, so there is nothing to do —
        -- and in particular nothing to REPAIR. Re-pausing an already
        -- paused session used to have to heal a half-paused world here
        -- (ticks frozen, time-of-day still advancing) because those
        -- engine-side writers left the clock running; World.Pause now
        -- zeroes it as part of imposing the pause, so the split cannot
        -- arise. Re-pausing must never re-capture the clock either: it
        -- reads 0 during a pause, and storing that would resume the
        -- world stopped (#1599 requirement 4).
        return
    end
    -- engine.setPaused(false) can be REJECTED outright (issue #763
    -- round 15) while a load transaction is in flight -- staging runs
    -- before the save barrier's capture lock, so this call can land
    -- mid-transaction and resuming here could let the OLD, still-live
    -- session's simulation advance before the load either publishes or
    -- fails. Round 16 rereview: the engine reports whether it actually
    -- applied the flag, and this side must honour that. A rejection
    -- must leave EVERYTHING here untouched, matching the #763 "nothing
    -- changed" contract for the pre-load session -- pause.paused stays
    -- whatever it already was (matching the engine flag, which the
    -- rejection also left untouched), and so does prevTimeScale.
    --
    -- Read the live speed BEFORE asking the engine to pause, purely to
    -- keep the mirror above honest: by the time setPaused returns, the
    -- clock has already been zeroed. world.getTimeScale needs a pageId;
    -- with no world active (main menu, mid-transition) there is no
    -- clock to mirror and none for the engine to retime either. A
    -- RESUME reads nothing: the engine reinstates the speed it captured
    -- on the page that actually holds the pause epoch, which is not
    -- necessarily the page active by now.
    local liveScale = nil
    if b then
        local wid = world.getActiveWorldId()
        liveScale = wid and world.getTimeScale(wid) or nil
    end
    local applied = engine.setPaused(b)
    if applied == false then
        return
    end
    pause.paused = b
    if liveScale then
        pause.prevTimeScale = liveScale
    end
    engine.logInfo("Game " .. (b and "paused" or "resumed"))
end

function pause.toggle()
    -- Toggle off the engine flag, not the local mirror. See pause.set
    -- for why — the mirror can be stale after auto-pause-on-save.
    pause.set(not engine.isPaused())
end

-- Broadcast after the engine finishes a save-load, and the authoritative
-- statement of this module's LOAD policy: a load never restores a
-- pre-save speed. The published session comes up paused with every page
-- at the default 1.0 (time scale is deliberately not persisted), so the
-- speed asked for here is the default and nothing else -- never the
-- speed the world happened to be running at when it was saved, and
-- never whatever a compatibility blob from an older save carried.
--
-- world.setTimeScale on a PAUSED session records the request as the
-- pause epoch's resume scale and leaves the live clock at 0
-- (World.Thread.Command.Time), so this both keeps the loaded world
-- coherently paused and pins what resuming it runs at.
function pause.onSaveLoaded(survUnitIds, survBuildingIds)
    local wid = world.getActiveWorldId()
    if not wid then return end
    pause.prevTimeScale = 1.0
    world.setTimeScale(wid, 1.0)
end

-- Engine script hooks
function pause.init(scriptId)
    engine.logDebug("Pause module initializing...")
    -- No save registration (issue #761, requirement 5): pause is NOT
    -- persistent. `pause.paused` is never read for real logic (see
    -- `pause.isPaused`/`pause.set` above -- it's just a transition-
    -- detection hint compared against the authoritative
    -- `engine.isPaused()`), and a loaded session always begins paused
    -- and resumes at default speed via `pause.onSaveLoaded` above.
end

function pause.shutdown()
    engine.logDebug("Pause module shut down")
end

return pause
