-- Building Spawn Sequencer
--
-- Watches each placed building and, once it transitions to the "built"
-- state, spawns a configured starting roster of units one at a time.
-- The next unit doesn't spawn until the previous one has cleared the
-- spawn tile (or been destroyed). Keeps units from piling up on top
-- of each other when a portal opens with a starting party.
--
-- Per-def config:
--   roster         = array of unit def names, spawned in order. The
--                    index is derived from the engine-side remaining
--                    countdown, so the order survives save/load.
--   unit_type      = string fallback when roster is absent (all
--                    spawns are this def).
--   count          = integer, total units to spawn (defaults to
--                    #roster when a roster is given).
--   spawn_offset   = {x, y}, where the unit appears relative to the
--                    building's anchor tile (in tile units).
--   walk_to_offset = {x, y}, where the unit is then commanded to walk
--                    via unit_ai.commandMove — beats the AI's wander
--                    so they clear the spawn tile predictably.
--   clear_radius   = how far the previous unit must move from its
--                    spawn point before the next one is allowed.
--
-- Per-building runtime state lives on the module table so reloads /
-- package.loaded singleton-ness preserves it through script changes:
--   buildingState[bid] = {
--     lastUid     = uid | nil,
--     lastSpawnX  = x,
--     lastSpawnY  = y,
--     lastSpawnedAt = posix,
--   }
--
-- The countdown ("how many units left to spawn from this building")
-- lives on the engine side as biSpawnRemaining so it survives chunk
-- eviction + save/load without a Lua serializer. Lua only owns the
-- per-spawn sequencing state, which is transient (re-derivable from
-- "is a fresh-spawned unit still on the spawn tile?").

local buildingSpawn = package.loaded["scripts.building_spawn"] or {}
package.loaded["scripts.building_spawn"] = buildingSpawn

local config = {
    acolyte_portal = {
        -- Five acolytes, then the technomule hauling the colony's
        -- construction stock walks out last.
        roster         = { "acolyte", "acolyte", "acolyte",
                           "acolyte", "acolyte", "technomule" },
        count          = 6,
        spawn_offset   = { x = 0.5, y = 0.5 },
        -- Items handed out to each spawned unit immediately after
        -- spawn, via unit.addItem. Empty now — the acolyte def's
        -- starting_inventory carries the canteen + rations baseline so
        -- the unit-info v2 inventory panel can see them on debug-
        -- spawned units too. Add portal-specific extras here if any
        -- ever appear (e.g. a sigil only the portal grants).
        starting_items = {},
        -- Walk two tiles south so the unit clears the spawn tile
        -- before its commandedTask resolves (the AI's "arrived"
        -- threshold is 0.6 tiles, so a 1-tile walk barely makes it
        -- off the portal). 2 tiles gives breathing room.
        walk_to_offset = { x = 0.5, y = 2.5 },
        -- Minimum seconds between spawns. Acts on top of the
        -- previous-unit-cleared check: even if the previous acolyte
        -- has walked off the spawn tile, we hold the next spawn for
        -- this long so the procession paces out evenly.
        spawn_interval = 2.0,
        -- Fallback timeout: if a spawned unit gets terrain-stuck and
        -- never clears its tile, don't gridlock the spawn forever.
        -- After this many seconds, spawn the next unit anyway.
        stuck_timeout  = 8.0,
    },
}

buildingSpawn.state = buildingSpawn.state or {}
local state = buildingSpawn.state

-- How many consecutive spawn failures to log per building before
-- suppressing further warnings (we keep retrying quietly so the spawn
-- self-heals once the tile clears, but we don't flood the log).
local SPAWN_FAIL_LOG_LIMIT = 5

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

-- True if the previous spawned unit has cleared the actual spawn
-- tile, OR has been stuck on it long enough that we give up waiting.
-- Cleared = destroyed OR the unit's tile-floor differs from the
-- spawn tile-floor in at least one axis. The timeout handles the
-- edge case of a terrain-stuck unit; in normal terrain the unit
-- walks away quickly via its commandedTask.
local function previousUnitCleared(s, params)
    if not s.lastUid then return true end
    local info = unit.getInfo(s.lastUid)
    if not info then return true end   -- unit destroyed
    if math.floor(info.gridX) ~= math.floor(s.lastSpawnX)
       or math.floor(info.gridY) ~= math.floor(s.lastSpawnY) then
        return true
    end
    -- Stuck-timeout fallback. Without this a single bad-terrain spawn
    -- locks the portal forever.
    if s.lastSpawnedAt and (engine.gameTime() - s.lastSpawnedAt) > params.stuck_timeout then
        return true
    end
    return false
end

-- Ensure a state entry exists for this building, and seed the
-- engine-side biSpawnRemaining countdown the first time we see this
-- bid. Engine sets biSpawnRemaining = -1 (sentinel) at spawn; Lua
-- seeds it to params.count on first sight. Loaded-from-save buildings
-- already have a real count (which may be 0 if the spawn finished
-- before save) — we leave those alone.
local function ensureState(bid, params)
    local s = state[bid]
    if not s then
        local engineRem = building.getSpawnRemaining(bid)
        if engineRem == nil or engineRem < 0 then
            -- Fresh placement, never seeded. Apply config.
            building.setSpawnRemaining(bid, params.count)
        end
        -- engineRem == 0 means "previously depleted by spawns" — load
        -- branch, don't re-seed. engineRem > 0 means "in progress" —
        -- also load branch.
        s = {
            lastUid    = nil,
            lastSpawnX = 0,
            lastSpawnY = 0,
        }
        state[bid] = s
    end
    return s
end

-----------------------------------------------------------
-- Per-building tick
-----------------------------------------------------------

local function tickOne(bid, info)
    local params = config[info.defName]
    if not params then return end

    -- Wait until the portal is done appearing — spawning during the
    -- "appearing" anim would have the unit pop out of an animating
    -- texture, which looks broken.
    if building.getActivity(bid) ~= "built" then return end

    local s = ensureState(bid, params)
    local remaining = building.getSpawnRemaining(bid) or 0
    if remaining <= 0 then return end
    if not previousUnitCleared(s, params) then return end
    -- Inter-spawn cooldown. Even if the previous acolyte cleared the
    -- spawn tile quickly, hold the next one until spawn_interval has
    -- elapsed so the line emerges at a steady cadence.
    if s.lastSpawnedAt
       and (engine.gameTime() - s.lastSpawnedAt) < (params.spawn_interval or 0) then
        return
    end

    -- Spawn at the portal tile's center plus offset. The anchor is
    -- the building's bottom-left tile (integer coords); offset is
    -- in tile-fractional units.
    local spawnX = info.gridX + params.spawn_offset.x
    local spawnY = info.gridY + params.spawn_offset.y
    local walkX  = info.gridX + params.walk_to_offset.x
    local walkY  = info.gridY + params.walk_to_offset.y

    -- Pick this spawn's unit def from the roster by how far the
    -- countdown has progressed (count - remaining spawns are already
    -- out). Derived, not stored — survives save/load with the
    -- engine-side biSpawnRemaining.
    local total = params.count or (params.roster and #params.roster) or 0
    local unitType = params.unit_type
    if params.roster then
        local idx = total - remaining + 1
        unitType = params.roster[idx] or params.roster[#params.roster]
    end

    -- Units produced by player-built portal buildings are player-
    -- controlled. Pass faction explicitly so the spawn-time-only
    -- faction system gets the right tag. Pass the building's OWN page
    -- (info.page) so the unit lands in the building's world even if the
    -- active page changes between the active-page scan and this call
    -- (#196) — unit.spawn otherwise stamps the active page.
    local newUid = unit.spawn(unitType, spawnX, spawnY,
                              nil, "player", info.page)
    if not newUid then
        -- Throttle retries: stamp lastSpawnedAt so the spawn_interval
        -- cooldown gate at the top of tickOne rate-limits the next
        -- attempt to ~spawn_interval (~2s) instead of every unpaused
        -- frame (~60/s). The original failure path left lastSpawnedAt
        -- untouched, so buildingSpawn.update re-entered every frame —
        -- flooding the log and gridlocking the roster. We deliberately
        -- do NOT consumeSpawn here: the spawn didn't happen, so the
        -- countdown must not advance.
        s.lastSpawnedAt = engine.gameTime()
        s.spawnFailures = (s.spawnFailures or 0) + 1
        -- Escalate-then-suppress so a persistently un-spawnable
        -- building (blocked tile, unloaded chunk, bad unitType) stays
        -- actionable without becoming a log flood. We keep retrying
        -- quietly past the limit so it self-heals once the tile clears.
        if s.spawnFailures <= SPAWN_FAIL_LOG_LIMIT then
            engine.logWarn(string.format(
                "BuildingSpawn: portal=%d unit.spawn(%s) failed at (%.2f, %.2f) (attempt %d)",
                bid, tostring(unitType), spawnX, spawnY, s.spawnFailures))
            if s.spawnFailures == SPAWN_FAIL_LOG_LIMIT then
                engine.logWarn(string.format(
                    "BuildingSpawn: portal=%d still failing to spawn; suppressing further warnings (will keep retrying every %.1fs)",
                    bid, params.spawn_interval or 0))
            end
        end
        return
    end

    -- Hand out per-building starting items. Unit YAML carries no
    -- inventory now, so this is what differentiates a portal-spawned
    -- unit from a debug-spawned one.
    if params.starting_items then
        for _, entry in ipairs(params.starting_items) do
            unit.addItem(newUid, entry.def, entry.fill or 0)
        end
    end

    -- Route through the AI so the walk-out is a commanded task (beats
    -- the autonomous wander utility; survives if the AI tick fires
    -- before the unit reaches its destination).
    -- No explicit speed → the sustainable "ordered" regime. A hard-coded
    -- fast speed exhausts the unit's stamina (collapses it mid-walk).
    local unitAi = require("scripts.unit_ai")
    unitAi.commandMove(newUid, walkX, walkY)

    local newRemaining = building.consumeSpawn(bid) or 0
    s.lastUid       = newUid
    s.lastSpawnX    = spawnX
    s.lastSpawnY    = spawnY
    s.lastSpawnedAt = engine.gameTime()
    s.spawnFailures = 0

    engine.logInfo(string.format(
        "BuildingSpawn: portal=%d spawned %s id=%d at (%.2f, %.2f) -> walk to (%.2f, %.2f), remaining=%d",
        bid, unitType, newUid, spawnX, spawnY, walkX, walkY, newRemaining))
end

-----------------------------------------------------------
-- Iteration: scan the buildings on the ACTIVE world page only.
-- building.getActiveIds() is the world-scoping boundary (#198) — it
-- never returns a building from another live world page. Ticking the
-- global, page-agnostic building.list() instead would advance
-- construction on off-world buildings and, worse, route any unit they
-- spawn into the active world because unit.spawn stamps the active
-- page rather than the building's own (#196). Scoping the scan to the
-- active page keeps the building and its spawned unit on the same
-- world. Returns empty when no world is active.
-----------------------------------------------------------

local function getActiveBuildingIds()
    return building.getActiveIds() or {}
end

-----------------------------------------------------------
-- Engine script hooks
-----------------------------------------------------------

-- Component-local validator (issue #761): `data` must be a table keyed
-- by positive-integer building ids, each mapping to a state table.
local function validateBuildingSpawnData(data)
    if type(data) ~= "table" then
        return { "building_spawn: payload must be a table" }
    end
    local errs = {}
    for bid, s in pairs(data) do
        if type(bid) ~= "number" or bid ~= math.floor(bid) or bid < 1 then
            errs[#errs + 1] = "building_spawn: invalid building id key "
                .. tostring(bid)
        elseif type(s) ~= "table" then
            errs[#errs + 1] = "building_spawn: state for building "
                .. tostring(bid) .. " is not a table"
        end
    end
    if #errs > 0 then return errs end
    return nil
end

-- Every reference this component carries (requirement 12) -- traversed
-- for documentation/diagnostics; a dangling `lastUid` is tolerated
-- (scrubbed at reconcile time by onSaveLoaded below), not rejected.
local function buildingSpawnReferences(data)
    local refs = {}
    for bid, s in pairs(data) do
        refs[#refs + 1] = { kind = "building", id = bid }
        if s.lastUid ~= nil then
            refs[#refs + 1] = { kind = "unit", id = s.lastUid }
        end
    end
    return refs
end

function buildingSpawn.init(scriptId)
    engine.logInfo("Building spawn sequencer initializing...")
    -- Persistent save component (issue #761, save-overhaul B3) for
    -- per-building transient sequencer state (lastUid/lastSpawnX/Y/
    -- lastSpawnedAt). The roster countdown itself lives on the
    -- BuildingInstance and persists via the buildings component — this
    -- one only covers the spawn-rate gating data. Required: a missing/
    -- invalid building_spawn component aborts the whole load.
    local saveMods = require("scripts.lib.save_modules")
    saveMods.register("building_spawn", {
        version = 1,
        inputVersions = { 1 },
        required = true,
        scope = "global",
        -- Requirement 2 (round-8 review): buildingSpawnReferences above
        -- declares "building" (the outer per-building key) and "unit"
        -- (lastUid) -- the Haskell components that own those entity
        -- kinds.
        deps = { "buildings", "units" },
        snapshot = function()
            -- Serialize only LIVE buildings' state. `state` is a global
            -- singleton that can retain entries for buildings destroyed
            -- before the save; persisting those is unsafe, since on a later
            -- cross-session load such a bid can collide with a live
            -- off-page building and onSaveLoaded can't distinguish the
            -- stale loaded-page leftover from legitimate off-page state.
            -- building.getInfo is GLOBAL, so live buildings on every page
            -- are still saved (#195).
            local live = {}
            for bid, s in pairs(state) do
                if building.getInfo(bid) then live[bid] = s end
            end
            return live
        end,
        decode = function(_version, data)
            return data or {}
        end,
        validate = validateBuildingSpawnData,
        references = buildingSpawnReferences,
        -- Temporary C2 compatibility adapter (issue #761, requirement 15)
        -- -- see unit_ai.lua's identical note: clobber `state` wholesale,
        -- onSaveLoaded below still does the #195/#191 reconciliation.
        apply = function(data)
            -- Snapshot the pre-load singleton BEFORE clobbering, so
            -- onSaveLoaded can restore still-live OFF-PAGE buildings'
            -- current state instead of the payload's stale copy (#195, #191).
            buildingSpawn._preLoadState = {}
            for k, v in pairs(state) do buildingSpawn._preLoadState[k] = v end
            for k in pairs(state) do state[k] = nil end
            for k, v in pairs(data) do state[k] = v end
        end,
    })
end

-- Broadcast from the engine once a save has finished loading (#195).
-- The Lua spawn-state blob is restored before the engine load path runs,
-- and that path can drop "orphan" buildings whose defs are no longer
-- registered. The restored state still holds entries for those dropped
-- ids, so a reused bid could inherit stale spawn-rate state.
--
-- `state` is a global singleton, clobbered wholesale by the restore with
-- save-time state for every building the save contained. Since issue #763
-- (save-overhaul C2), a load REPLACES THE COMPLETE SESSION — there is no
-- more "other live page" to preserve (#191's off-page preservation is gone
-- along with the merge-based load path it protected); survBuildingIds now
-- names every building in the whole new session. We still rebuild `state`
-- defensively as:
--   * survivor (now: every live building) → its restored (blob) state;
--   * any OTHER entry (dead code in normal operation post-#763 — kept only
--     as a defensive no-op) → its pre-load state, IF that bid still
--     somehow exists live;
--   * everything else (orphans, dead, gone-before-save) → dropped.
-- The nested s.lastUid (last unit spawned) is scrubbed on every survivor
-- entry against the surviving unit set, so a stale/colliding uid can't gate
-- spawning.
function buildingSpawn.onSaveLoaded(survUnitIds, survBuildingIds)
    local survUnitSet, survBuildingSet = {}, {}
    for _, uid in ipairs(survUnitIds or {})     do survUnitSet[uid] = true end
    for _, bid in ipairs(survBuildingIds or {}) do survBuildingSet[bid] = true end

    local pre = buildingSpawn._preLoadState or {}
    buildingSpawn._preLoadState = nil
    local blob = state   -- current contents = the just-restored blob

    local rebuilt = {}
    for bid in pairs(survBuildingSet) do
        if blob[bid] ~= nil then rebuilt[bid] = blob[bid] end
    end
    for bid, s in pairs(pre) do
        if not survBuildingSet[bid] and building.getInfo(bid) then
            rebuilt[bid] = s        -- live off-page building: keep current state
        end
    end

    local kept = 0
    for k in pairs(state) do state[k] = nil end
    for k, v in pairs(rebuilt) do state[k] = v; kept = kept + 1 end

    local scrubbed = 0
    for bid, s in pairs(state) do
        if survBuildingSet[bid] and s.lastUid ~= nil
           and not survUnitSet[s.lastUid] then
            s.lastUid = nil
            scrubbed = scrubbed + 1
        end
    end
    engine.logInfo("Building spawn: reconciled state after load ("
        .. kept .. " kept, " .. scrubbed .. " stale ref(s) scrubbed)")
end

-- Construction progress curve. Solo workers build at 1× the base
-- rate (R(1)=1 worker-second per real second); coordination bonus
-- ramps as n² up to 3 workers, then tapers logarithmically so the
-- 5th+ helper still adds something but with diminishing returns.
--   R(1) = 1,  R(2) = 4,  R(3) = 9
--   R(4) ≈ 12, R(5) ≈ 13.75, R(6) = 15, R(8) ≈ 16.75, R(10) = 18
local function workerRate(n)
    if n <= 0 then return 0 end
    if n <= 3 then return n * n end
    return 9 + 3 * (math.log(n - 2) / math.log(2))
end

-- Tick the construction progress for one Appearing building. Only
-- fires if the def has bdBuildWork > 0 (legacy time-based defs like
-- the portal are untouched). Counts acolytes physically adjacent
-- with currentAction == "build_nearby", scales by R(n) * dt, and
-- bumps biBuildProgress via the engine API. The engine's
-- currentActivity check flips the building to Built once
-- biBuildProgress ≥ bdBuildWork — no explicit "complete" needed.
local function constructionTickOne(bid, dt)
    if building.getActivity(bid) ~= "appearing" then return end
    local required = building.getBuildRequired(bid)
    if not required or required <= 0 then return end

    -- Materials gate: don't advance progress until every required
    -- material has been fully delivered. While unsatisfied the
    -- building renders as a ghost; once satisfied it flips to the
    -- first construction frame and progresses normally.
    if not building.areMaterialsSatisfied(bid) then return end

    local unitAi = require("scripts.unit_ai")
    local workers = unitAi.countAdjacentBuilders(bid)
    if workers <= 0 then return end

    local delta = workerRate(workers) * dt
    if delta > 0 then
        building.addBuildProgress(bid, delta)
    end
end

function buildingSpawn.update(dt)
    if require("scripts.pause").isPaused() then return end
    local ids = getActiveBuildingIds()
    if #ids == 0 then return end
    for _, bid in ipairs(ids) do
        local info = building.getInfo(bid)
        if info and info.defName then
            tickOne(bid, info)
            constructionTickOne(bid, dt)
        end
    end
end

function buildingSpawn.shutdown()
    for k in pairs(state) do state[k] = nil end
    engine.logInfo("Building spawn sequencer shut down")
end

return buildingSpawn
