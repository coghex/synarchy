-- Persistent ruin-encounter guard behavior (#916).
--
-- Membership, home positions, bounds and clearance live on the placed
-- LocationInstance. This module only owns the decision-time episode state:
-- which visible hostile was last seen, whether the guard is returning, and
-- the exactly-once aggression/disengagement notices for the current episode.

local unitAi = package.loaded["scripts.unit_ai"]
local core = require("scripts.unit_ai_core")
local page = require("scripts.unit_ai_page")
local mv = require("scripts.movement_speed")
local claims = require("scripts.unit_ai_claims")

local M = {}

local LEASH_TILES = 12
local SIGHT_MEMORY_SEC = 10
local RETALIATION_WINDOW_SEC = 10
local RETURN_UTILITY = 10
local MEMORY_UTILITY = 8.5
local ENGAGE_UTILITY = 8

-- World mutations are queued, while several guards can decide in one Lua
-- update. Keep the just-issued episode/participation edges here as a
-- same-process overlay until world.listPlacedLocations reflects them; the
-- persisted encounter remains the save/load authority.
local localEpisodeActive = claims.track({})
local localEpisodeAggression = claims.track({})
local localEpisodeDisengaged = claims.track({})
local localParticipation = claims.track({})

local function encounterKey(binding)
    return tostring(binding.info.page) .. ":"
        .. tostring(binding.location.instance_id)
end

local function participationFor(key)
    local states = localParticipation[key]
    if not states then
        states = {}
        localParticipation[key] = states
    end
    return states
end

local function overlayValue(persisted, overlay, key)
    if overlay[key] ~= nil then return overlay[key] end
    return persisted
end

local function label(info)
    if info and info.name and info.name ~= "" then return info.name end
    return info and info.displayName or "unit"
end

local function locationIsDiscovered(binding)
    return binding.location.discovered == true
end

-- Re-derive the persisted binding every thought tick. This deliberately has
-- no fallback for hand-stamped ruins: they own no encounter instance.
local function bindingFor(uid)
    local info = unit.getInfo(uid)
    if not info or not info.page then return nil end
    for _, location in ipairs(world.listPlacedLocations(info.page) or {}) do
        local encounter = location.encounter
        if encounter then
            for _, occupant in ipairs(encounter.occupants or {}) do
                if occupant.uid == uid then
                    return { location = location, occupant = occupant,
                             info = info }
                end
            end
        end
    end
    return nil
end

local function relationIsHostile(uid, target)
    return faction.relation(unit.getFaction(uid), unit.getFaction(target))
        == "hostile"
end

local function targetVisible(uid, targetInfo)
    if not targetInfo then return false end
    local tx, ty = math.floor(targetInfo.gridX), math.floor(targetInfo.gridY)
    for _, tile in ipairs(unit.getVisibleTiles(uid) or {}) do
        if tile.x == tx and tile.y == ty then return true end
    end
    return false
end

local function outsideLeash(info, bounds)
    return info.gridX < bounds.min_x - LEASH_TILES
        or info.gridX > bounds.max_x + LEASH_TILES
        or info.gridY < bounds.min_y - LEASH_TILES
        or info.gridY > bounds.max_y + LEASH_TILES
end

local function atHome(info, occupant)
    return math.max(math.abs(info.gridX - occupant.home_x),
                    math.abs(info.gridY - occupant.home_y)) <= 0.5
end

local function persistOccupantState(binding, uid, engaged, returning)
    world.setLocationEncounterOccupantState(
        binding.location.instance_id, uid, engaged, returning,
        binding.info.page)
end

local function persistEpisodeState(binding, active, aggressionAnnounced,
                                   disengageAnnounced)
    world.setLocationEncounterEpisodeState(
        binding.location.instance_id, active, aggressionAnnounced,
        disengageAnnounced, binding.info.page)
end

local function clearCombat(uid, s)
    s.attackTargetUid = nil
    s.committed = nil
    s.attackLastMoveTo = nil
    require("scripts.unit_ai_combat_lunge").clear(s)
    core.markGoalAccomplished(s, "attack")
    s.ruinEncounterCombat = nil
    unit.clearAnimOverride(uid)
    unit.stop(uid)
end

local function disengageReason(uid, s, binding)
    local target = s.attackTargetUid
    if not target then return nil end
    if not unit.exists(target) then return "target disappeared" end
    if unit.getPose(target) == "dead" then return "target died" end
    local targetInfo = unit.getInfo(target)
    if not targetInfo or not page.same(binding.info.page, targetInfo.page) then
        return "target left the page"
    end
    if not relationIsHostile(uid, target) then
        return "faction relation is no longer hostile"
    end
    if outsideLeash(targetInfo, binding.location.bounds) then
        return "target crossed the ruin leash"
    end
    if outsideLeash(binding.info, binding.location.bounds) then
        return "guard crossed the ruin leash"
    end
    if targetVisible(uid, targetInfo) then
        s.ruinLastSeenAt = engine.gameTime()
        s.ruinLastSeenX = targetInfo.gridX
        s.ruinLastSeenY = targetInfo.gridY
        return nil
    end
    if engine.gameTime() - (s.ruinLastSeenAt or -math.huge)
       > SIGHT_MEMORY_SEC then
        return "target lost from sight"
    end
    return nil
end

-- While canonical sight is lost, the encounter guard owns pursuit at the
-- last observed tile. Leaving attack_target in charge here would let it read
-- the target's current hidden position every tick, turning ten-second memory
-- into perfect tracking through terrain and facing loss.
local function memoryUtility(uid, s)
    if not s.attackTargetUid then return -math.huge end
    local binding = bindingFor(uid)
    if not binding or disengageReason(uid, s, binding)
       or targetVisible(uid, unit.getInfo(s.attackTargetUid))
       or s.ruinLastSeenX == nil or s.ruinLastSeenY == nil then
        return -math.huge
    end
    return MEMORY_UTILITY
end

local function memoryExecute(uid, s)
    if s.ruinLastSeenX == nil or s.ruinLastSeenY == nil then return end
    local info = unit.getInfo(uid)
    if info and math.max(math.abs(info.gridX - s.ruinLastSeenX),
                         math.abs(info.gridY - s.ruinLastSeenY)) <= 0.5 then
        unit.stop(uid)
        return
    end
    unit.moveTo(uid, s.ruinLastSeenX, s.ruinLastSeenY, mv.comfort(uid))
end

local function guardUtility(uid, s)
    local binding = bindingFor(uid)
    if not binding then return -math.huge end
    if s.ruinReturning or binding.occupant.returning then
        return RETURN_UTILITY
    end
    if s.attackTargetUid then
        local reason = disengageReason(uid, s, binding)
        if reason then
            s.ruinDisengageReason = reason
            return RETURN_UTILITY
        end
    elseif not atHome(binding.info, binding.occupant) then
        s.ruinDisengageReason = "returning to assigned post"
        return RETURN_UTILITY
    end
    return -math.huge
end

local function guardExecute(uid, s)
    local binding = bindingFor(uid)
    if not binding then return end
    local key = encounterKey(binding)
    local participation = participationFor(key)
    if not s.ruinReturning then
        clearCombat(uid, s)
        s.ruinReturning = true
        participation[uid] = "returning"
        persistOccupantState(binding, uid, false, true)
    end

    -- The episode ends only after every surviving participant has broken
    -- contact. Local participation overlays queued writes so two guards
    -- disengaging in the same update still produce exactly one notice.
    local anyEngaged = false
    for _, occupant in ipairs(binding.location.encounter.occupants or {}) do
        if unit.exists(occupant.uid) and unit.getPose(occupant.uid) ~= "dead" then
            local localState = participation[occupant.uid]
            local engaged = localState == "engaged"
                or (localState == nil and occupant.engaged)
            if engaged then anyEngaged = true; break end
        end
    end
    local episodeActive = overlayValue(
        binding.location.encounter.episode_active, localEpisodeActive, key)
    local disengageAnnounced = overlayValue(
        binding.location.encounter.disengage_announced,
        localEpisodeDisengaged, key)
    if episodeActive and not anyEngaged and not disengageAnnounced then
        local visible = locationIsDiscovered(binding)
        if visible then
            engine.emitEventForUnit("unit_event", string.format(
                "%s disengaged at %s (%s)", label(binding.info),
                binding.location.name, s.ruinDisengageReason or "returning"),
                uid, binding.info.gridX, binding.info.gridY)
        end
        local aggressionAnnounced = overlayValue(
            binding.location.encounter.aggression_announced,
            localEpisodeAggression, key)
        localEpisodeActive[key] = false
        localEpisodeDisengaged[key] = visible
        persistEpisodeState(binding, false, aggressionAnnounced, visible)
    end
    if atHome(binding.info, binding.occupant) then
        unit.stop(uid)
        participation[uid] = "home"
        persistOccupantState(binding, uid, false, false)
        s.ruinReturning = nil
        s.ruinLastSeenAt = nil
        s.ruinLastSeenX = nil
        s.ruinLastSeenY = nil
        s.ruinDisengageReason = nil
        return
    end
    unit.moveTo(uid, binding.occupant.home_x, binding.occupant.home_y,
                mv.comfort(uid))
end

local function visibleHostile(uid, binding)
    if binding.location.encounter.cleared then return nil end
    local visible = {}
    for _, tile in ipairs(unit.getVisibleTiles(uid) or {}) do
        visible[tostring(tile.x) .. ":" .. tostring(tile.y)] = true
    end
    local ids = unit.getAllIds() or {}
    table.sort(ids)
    for _, target in ipairs(ids) do
        if target ~= uid and unit.getPose(target) ~= "dead" then
            local info = unit.getInfo(target)
            local key = info and (tostring(math.floor(info.gridX)) .. ":"
                .. tostring(math.floor(info.gridY))) or nil
            if info and page.same(binding.info.page, info.page)
               and visible[key]
               and not outsideLeash(info, binding.location.bounds)
               and relationIsHostile(uid, target) then
                return target
            end
        end
    end
    return nil
end

local function recentHostileAttacker(uid, binding)
    if binding.location.encounter.cleared then return nil end
    local attacker = unit.getLastAttacker(uid)
    if not attacker or attacker.uid == uid
       or engine.gameTime() - (attacker.at or 0) > RETALIATION_WINDOW_SEC
       or not unit.exists(attacker.uid)
       or unit.getPose(attacker.uid) == "dead" then
        return nil
    end
    local info = unit.getInfo(attacker.uid)
    if not info or not page.same(binding.info.page, info.page)
       or outsideLeash(info, binding.location.bounds)
       or not relationIsHostile(uid, attacker.uid) then
        return nil
    end
    return attacker.uid
end

local function hostileTarget(uid, binding)
    local attacker = recentHostileAttacker(uid, binding)
    if attacker then return attacker, false end
    return visibleHostile(uid, binding), true
end

local function engageUtility(uid, s)
    if s.ruinReturning or core.isGoalActive(s, "retreat")
       or core.isGoalActive(s, "attack") then return -math.huge end
    local binding = bindingFor(uid)
    local localState = binding and participationFor(encounterKey(binding))[uid]
    if not binding or binding.occupant.returning or localState == "returning"
       or outsideLeash(binding.info, binding.location.bounds) then
        return -math.huge
    end
    local target = hostileTarget(uid, binding)
    return target and ENGAGE_UTILITY or -math.huge
end

local function engageExecute(uid, s)
    local binding = bindingFor(uid)
    local target, requiresSight = nil, true
    if binding then target, requiresSight = hostileTarget(uid, binding) end
    if not target or not unit.exists(target) then return end
    local targetInfo = unit.getInfo(target)
    if not targetInfo or not page.same(binding.info.page, targetInfo.page)
       or outsideLeash(targetInfo, binding.location.bounds)
       or not relationIsHostile(uid, target)
       or (requiresSight and not targetVisible(uid, targetInfo)) then
        return
    end
    s.ruinLastSeenAt = engine.gameTime()
    s.ruinLastSeenX = targetInfo.gridX
    s.ruinLastSeenY = targetInfo.gridY
    s.ruinEncounterCombat = true
    unitAi.commandAttack(uid, target)
    local key = encounterKey(binding)
    local participation = participationFor(key)
    participation[uid] = "engaged"
    local isNewEpisode = not overlayValue(
        binding.location.encounter.episode_active, localEpisodeActive, key)
    if isNewEpisode then
        local visible = locationIsDiscovered(binding)
        localEpisodeActive[key] = true
        localEpisodeDisengaged[key] = false
        localEpisodeAggression[key] = visible
        if visible then
            engine.emitEventForUnit("unit_event", string.format(
                "%s at %s attacks %s because their faction relation is hostile",
                label(binding.info), binding.location.name, label(targetInfo)),
                uid, binding.info.gridX, binding.info.gridY)
        end
        persistEpisodeState(binding, true, visible, false)
    end
    persistOccupantState(binding, uid, true, false)
end

M.guardUtility = guardUtility
M.guardExecute = guardExecute
M.memoryUtility = memoryUtility
M.memoryExecute = memoryExecute
M.engageUtility = engageUtility
M.engageExecute = engageExecute

function M.register(needs)
    unitAi.setConfig("nomad_primitive", {
        thought_interval = 0.25,
        thought_jitter = 0.05,
        combat_thought_interval = 0.1,
        stuck_walk_timeout = 6.0,
    })
    unitAi.registerActions("nomad_primitive", {
        { name = "ruin_guard", utility = guardUtility,
          execute = guardExecute },
        { name = "ruin_memory", utility = memoryUtility,
          execute = memoryExecute },
        { name = "ruin_engage", utility = engageUtility,
          execute = engageExecute },
        { name = "idle", utility = needs.idleUtility,
          execute = needs.idleExecute },
    }, {
        -- Generic engage reacts to a recent hit without consulting faction,
        -- page, or leash rules. Ruin occupants acquire ONLY through the
        -- encounter-owned visible-hostile and qualified recent-attacker
        -- sources above; retreat and attack execution remain universal once
        -- those rules have selected a target.
        excludeUniversal = { engage = true },
    })
end

return M
