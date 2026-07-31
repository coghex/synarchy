-- Autosave scheduler (#913)
--
-- Owns the interval clock, the eligibility gate, and the rotate-then-save
-- sequence. Everything durable lives in the engine: the config
-- (config/save_default.yaml overlaid with config/save.local.yaml), the
-- autosave/manual classification stored in each save's metadata, and the
-- reserved autosave-<n> slot rotation. This module is the timing and
-- policy layer on top of those.
--
-- The clock is WALL time (os.time()), never accumulated tick dt: an
-- autosave interval is a promise about real minutes, and tick dt drifts
-- with frame rate, pausing, and load spikes.
--
-- Skip semantics, which the rest of this file exists to get right:
--
--   * A deadline reached while there is no active visible gameplay
--     world, while a menu is open, while a save is already running, or
--     while a load is running is SKIPPED. Skipping creates no request,
--     no failure notification, and nothing queued for later -- the next
--     attempt is simply the next scheduled interval.
--   * Opening a menu therefore neither suspends nor resets the cadence.
--     The clock keeps running; only the ATTEMPT is dropped.
--   * Enabling autosave, or changing the interval, starts a fresh full
--     interval. Applying/saving/backing out of unrelated settings does
--     not. Disabling clears the schedule outright.
--
-- Singleton via package.loaded so the engine.loadScript update tick and
-- require()s from settings/data.lua share one instance (the same pattern
-- pause.lua and unit_ai.lua use).
local autosave = package.loaded["scripts.autosave"] or {}
package.loaded["scripts.autosave"] = autosave

-- Effective config, refreshed from the engine at init and whenever the
-- settings screen persists a change.
autosave.config = autosave.config or nil

-- The world the current interval belongs to. A DIFFERENT active visible
-- gameplay world (a load, or entering another world) restarts the
-- interval; returning to the same one does not.
autosave.scheduledWorld = autosave.scheduledWorld or nil

-- Wall-clock second the next attempt is due, or nil for "no schedule"
-- (autosave disabled, or no gameplay world has been entered yet).
autosave.nextDueAt = autosave.nextDueAt or nil

-- Introspection for tools/autosave_probe.py. Counters only ever grow.
autosave.stats = autosave.stats or {
    attempts = 0,   -- eligible deadlines that reached a save request
    skips    = 0,   -- deadlines dropped by the eligibility gate
    failures = 0,   -- deadlines that reported an autosave failure
}
autosave.lastResult = autosave.lastResult or nil

-----------------------------------------------------------
-- Clock
-----------------------------------------------------------

-- Indirected so a test can pin it. os.time() has 1-second resolution,
-- which is exact enough for a schedule whose smallest legal interval is
-- a whole minute.
function autosave.now()
    return os.time()
end

-----------------------------------------------------------
-- Scheduling
-----------------------------------------------------------

local function intervalSeconds()
    local cfg = autosave.config
    if not cfg then return nil end
    local minutes = cfg.intervalMinutes
    if type(minutes) ~= "number" or minutes < 1 then return nil end
    return math.floor(minutes) * 60
end

-- Begin a fresh FULL interval from now. A no-op when there is nothing to
-- schedule for yet: the next gameplay world entry picks it up.
function autosave.startInterval()
    local seconds = intervalSeconds()
    if not autosave.config or not autosave.config.enabled
        or not autosave.scheduledWorld or not seconds then
        autosave.nextDueAt = nil
        return
    end
    autosave.nextDueAt = autosave.now() + seconds
end

-- Advance past a deadline that has just been consumed (whether it
-- produced a save or was skipped). Normally one interval forward; if the
-- process was stalled for longer than a whole interval, re-base on now
-- rather than firing a burst of catch-up attempts for deadlines nobody
-- was there for.
local function advancePastDeadline()
    local seconds = intervalSeconds()
    if not seconds or not autosave.nextDueAt then
        autosave.nextDueAt = nil
        return
    end
    autosave.nextDueAt = autosave.nextDueAt + seconds
    local now = autosave.now()
    if autosave.nextDueAt <= now then
        autosave.nextDueAt = now + seconds
    end
end

-----------------------------------------------------------
-- Eligibility
-----------------------------------------------------------

-- uiManager.isGameplayView() is the predicate the issue names: true only
-- in world_view/test_arena_view with the pause menu hidden. Anything
-- else is a menu for this feature's purposes. Resolved lazily and
-- defensively -- autosave.lua is loaded before ui_manager.lua, and a
-- headless boot may never build the UI at all, in which case there is by
-- definition no gameplay view to be in.
local function inGameplayView()
    local ok, uiManager = pcall(require, "scripts.ui_manager")
    if not ok or not uiManager or not uiManager.isGameplayView then
        return false
    end
    local viewOk, isView = pcall(uiManager.isGameplayView)
    return viewOk and isView == true
end

-- A transaction with no terminal outcome yet is still running. Save and
-- load are mutually exclusive engine-side for their whole durations, so
-- an autosave that started during either would simply be rejected --
-- skipping keeps that out of the failure channel entirely.
local function transactionRunning(status)
    return status ~= nil and status.outcome == nil
end

function autosave.eligible()
    if not world.getActiveWorldId() then return false, "no world" end
    if not inGameplayView() then return false, "menu" end
    if transactionRunning(engine.getSaveStatus()) then
        return false, "save in progress"
    end
    if transactionRunning(engine.getLoadStatus()) then
        return false, "load in progress"
    end
    return true
end

-----------------------------------------------------------
-- The attempt itself
-----------------------------------------------------------

-- Rotate first, then publish into the freed autosave-1. The rotation is
-- the step that can find a MANUAL save squatting on one of the reserved
-- names; that is a real autosave FAILURE (reported through save_load),
-- unlike the silent skips above, because the player asked for autosaves
-- and is not getting them until they move that save.
function autosave.performSave(pageId)
    local depth = autosave.config and autosave.config.rotationDepth or 3
    local rotated, reason = engine.rotateAutosaveSlots(depth)
    if not rotated then
        autosave.stats.failures = autosave.stats.failures + 1
        autosave.lastResult = "failed: " .. tostring(reason or "rotation refused")
        engine.emitEvent("save_load",
            "Autosave failed: " .. tostring(reason or "slot rotation refused"))
        return false
    end

    local requested = engine.saveWorld(pageId, "autosave-1", { autosave = true })
    if not requested then
        -- A synchronous rejection never started a save transaction, so
        -- it is not an autosave failure and gets no failure event (the
        -- engine has already logged whatever it rejected on). The next
        -- interval simply tries again.
        autosave.stats.skips = autosave.stats.skips + 1
        autosave.lastResult = "rejected"
        return false
    end
    autosave.stats.attempts = autosave.stats.attempts + 1
    autosave.lastResult = "requested"
    return true
end

-----------------------------------------------------------
-- Tick
-----------------------------------------------------------

function autosave.update(dt)
    if not autosave.config then return end

    -- Epoch: a fresh interval begins when a world first becomes the
    -- active visible gameplay world, and again for any DIFFERENT one.
    -- Evaluated before the enabled check so entering a world while
    -- autosave is off still establishes which world a later enable
    -- schedules against.
    if inGameplayView() then
        local wid = world.getActiveWorldId()
        if wid and wid ~= autosave.scheduledWorld then
            autosave.scheduledWorld = wid
            autosave.startInterval()
        end
    end

    if not autosave.config.enabled then return end
    if not autosave.nextDueAt then return end
    if autosave.now() < autosave.nextDueAt then return end

    -- The deadline is consumed no matter what happens next: a skipped
    -- attempt is never deferred or queued.
    advancePastDeadline()

    local ok = autosave.eligible()
    if not ok then
        autosave.stats.skips = autosave.stats.skips + 1
        autosave.lastResult = "skipped"
        return
    end
    autosave.performSave(world.getActiveWorldId())
end

-----------------------------------------------------------
-- Configuration changes
-----------------------------------------------------------

-- Called by settings/data.lua after it persists a change, and at init.
-- Only a transition to ENABLED or a changed INTERVAL restarts the clock;
-- a rotation-depth edit (or re-applying identical values) leaves the
-- current interval running, since neither changes when the next save is
-- due.
function autosave.onConfigChanged(newCfg)
    if type(newCfg) ~= "table" then return end
    local old = autosave.config
    autosave.config = newCfg

    if not newCfg.enabled then
        autosave.nextDueAt = nil
        return
    end

    local becameEnabled  = not (old and old.enabled)
    local intervalMoved  = not old or old.intervalMinutes ~= newCfg.intervalMinutes
    if becameEnabled or intervalMoved or not autosave.nextDueAt then
        autosave.startInterval()
    end
end

function autosave.reload()
    autosave.onConfigChanged(engine.getSaveConfig())
end

-----------------------------------------------------------
-- Engine broadcasts
-----------------------------------------------------------

-- A load REPLACES the whole session, so whatever interval was running
-- belonged to a world that no longer exists. Clearing the scheduled
-- world makes the next gameplay tick treat the loaded world as a fresh
-- entry, which is exactly the issue's "loading another active visible
-- world starts a fresh interval".
function autosave.onSaveLoaded(survUnitIds, survBuildingIds)
    autosave.scheduledWorld = nil
    autosave.nextDueAt = nil
end

-----------------------------------------------------------
-- Introspection (tools/autosave_probe.py)
-----------------------------------------------------------

function autosave.dump()
    local now = autosave.now()
    return {
        config         = autosave.config,
        scheduledWorld = autosave.scheduledWorld,
        nextDueAt      = autosave.nextDueAt,
        secondsUntilDue = autosave.nextDueAt and (autosave.nextDueAt - now) or nil,
        now            = now,
        eligible       = (autosave.eligible()) and true or false,
        lastResult     = autosave.lastResult,
        stats          = autosave.stats,
    }
end

-----------------------------------------------------------
-- Engine script hooks
-----------------------------------------------------------

function autosave.init(scriptId)
    engine.logInfo("Autosave module initializing...")
    autosave.reload()
    local cfg = autosave.config
    if cfg then
        engine.logInfo(string.format(
            "Autosave: %s, every %d min, keeping %d generation(s)",
            cfg.enabled and "enabled" or "disabled",
            cfg.intervalMinutes or 0, cfg.rotationDepth or 0))
    end
end

function autosave.shutdown()
    engine.logInfo("Autosave module shut down")
end

return autosave
