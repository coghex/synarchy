-- Persistent save-component registration for unit_ai (issue #761,
-- save-overhaul B3). Split out of scripts/unit_ai.lua to stay under
-- its line budget (#538, tools/lua_module_budget.py) -- this is
-- otherwise exactly what unit_ai.lua's init would register inline.

local M = {}

-- aiState fields on a per-unit entry that hold a direct reference to
-- another entity by raw id. After a load these can point at an id that
-- did NOT survive on the loaded page — a missing-def orphan, an entity
-- already gone before the save (its stale ref was still serialized),
-- or an id that now collides with a LIVE off-page entity. The per-tick
-- validators (unit.exists / unit.getInfo / building.getInfo) are
-- GLOBAL raw lookups, so for a collision they'd pass for the wrong
-- off-page entity and the survivor would resume targeting / delivering
-- to it (#195). unit_ai.lua's scrubStaleRefs clears any ref whose
-- target isn't in the surviving loaded-page set.
-- NB: any NEW aiState field that stores a unit/building id MUST be
-- listed here, or it silently reintroduces the stale-ref bug.
M.AI_UNIT_REF_FIELDS     = { "attackTargetUid", "retreatThreatUid",
                             "notifyTarget", "lungeTarget" }
M.AI_BUILDING_REF_FIELDS = { "buildTarget", "storeTarget" }

-- Component-local validator (issue #761): `data` must be a table keyed
-- by positive-integer unit ids, each mapping to a state table. Deep
-- per-field validation of aiState's own shape is deliberately not
-- attempted here (it's a large, free-form utility-AI scratch table) --
-- this catches real corruption (wrong top-level shape) without gold-
-- plating a full schema for every possible field.
local function validateUnitAiData(data)
    if type(data) ~= "table" then
        return { "unit_ai: payload must be a table" }
    end
    local errs = {}
    for uid, s in pairs(data) do
        if type(uid) ~= "number" or uid ~= math.floor(uid) or uid < 1 then
            errs[#errs + 1] = "unit_ai: invalid unit id key " .. tostring(uid)
        elseif type(s) ~= "table" then
            errs[#errs + 1] = "unit_ai: state for unit " .. tostring(uid)
                .. " is not a table"
        end
    end
    if #errs > 0 then return errs end
    return nil
end

-- Every reference this component carries (requirement 12) -- unit/
-- building/craft-bill/item/ground-item ids reachable from a per-unit
-- aiState entry, including ones nested inside claim/job/candidate
-- tables and collection-held ones inside loot lists:
--   unit_ai_medic.lua's treatClaim/treatPending
--   unit_ai_deliver.lua's deliveryClaim/deliveryPendingTarget
--   unit_ai_craft.lua's craftJob/craftCandidate
--   unit_ai_repair.lua's repairJob/repairCandidate
--   unit_ai_pickup.lua's pickupOrder
--   unit_ai_needs.lua's forageTarget/forageLoot
--   unit_ai_farm.lua's harvestLoot
-- Traversed here for documentation/diagnostics (actually CALLED by
-- saveModules.prepareLoad, requirement 11/12 -- not merely declared
-- and left dead); a dangling entry is NOT rejected by this validator
-- (per the #761 issue-review clarification: a target that legitimately
-- died before the save boundary must stay representable) -- a job/
-- claim one is cleared at reconcile time instead, by unit_ai.lua's
-- scrubStaleRefs/onSaveLoaded (a candidate/loot one self-heals on the
-- next tick, since those are recomputed/re-validated fresh rather than
-- trusted across a load the way a committed claim is).
-- NB: any NEW nested claim/job/candidate field, or new loot-style list,
-- that stores a unit/building/bill/item/ground-item id MUST be added
-- here too (mirroring scrubStaleRefs for the claim/job fields it also
-- reconciles).
local function unitAiReferences(data)
    local refs = {}
    local function addRef(kind, id)
        if id ~= nil then refs[#refs + 1] = { kind = kind, id = id } end
    end
    local function addRefList(kind, ids)
        if ids ~= nil then
            for _, id in ipairs(ids) do addRef(kind, id) end
        end
    end
    for _, s in pairs(data) do
        for _, f in ipairs(M.AI_UNIT_REF_FIELDS) do addRef("unit", s[f]) end
        for _, f in ipairs(M.AI_BUILDING_REF_FIELDS) do addRef("building", s[f]) end
        if s.treatClaim then addRef("unit", s.treatClaim.patient) end
        if s.treatPending then addRef("unit", s.treatPending.uid) end
        if s.deliveryClaim then addRef("building", s.deliveryClaim.bid) end
        if s.deliveryPendingTarget then addRef("building", s.deliveryPendingTarget.bid) end
        if s.craftJob then
            addRef("craft_bill", s.craftJob.billId)
            addRef("building", s.craftJob.bid)
        end
        if s.craftCandidate and s.craftCandidate.bill then
            addRef("craft_bill", s.craftCandidate.bill.id)
            addRef("building", s.craftCandidate.bill.station)
        end
        if s.repairJob then
            addRef("item_instance", s.repairJob.instanceId)
            addRef("building", s.repairJob.bid)
        end
        if s.repairCandidate then
            addRef("item_instance", s.repairCandidate.instanceId)
        end
        if s.pickupOrder then addRef("ground_item", s.pickupOrder.gid) end
        if s.forageTarget and s.forageTarget.kind == "ground" then
            addRef("ground_item", s.forageTarget.gid)
        end
        addRefList("ground_item", s.forageLoot)
        addRefList("ground_item", s.harvestLoot)
    end
    return refs
end

-- Register the "unit_ai" persistent save component. `unitAi` is the
-- orchestrator singleton (so apply() can stash `_preLoadState` onto it
-- for the #195/#191 onSaveLoaded reconcile); `aiState` is
-- scripts.unit_ai_core's shared per-unit state table.
function M.register(unitAi, aiState)
    -- Persistent save component (issue #761, save-overhaul B3): persist
    -- aiState (knownWaterSources, commandedTask, currentAction,
    -- source-drink phase, search-spiral progress, etc.). Without this,
    -- units load with empty AI state and lose their water memory + any
    -- in-flight player commands. Required: a missing/invalid unit_ai
    -- component aborts the whole load (requirement 6) rather than
    -- silently starting every unit with blank AI state.
    local saveMods = require("scripts.lib.save_modules")
    saveMods.register("unit_ai", {
        version = 1,
        inputVersions = { 1 },
        required = true,
        scope = "global",
        deps = {},
        snapshot = function()
            -- Serialize only LIVE units' state. aiState is a global
            -- singleton that accumulates entries and never drops them when
            -- a unit is destroyed, so it leaks stale entries for
            -- gone-before-save units. Persisting those is actively unsafe:
            -- on a later cross-session load such an id can collide with a
            -- live off-page entity, and onSaveLoaded then can't tell the
            -- stale loaded-page leftover from legitimate off-page state
            -- (the payload isn't page-keyed) — it would keep + misattribute
            -- it. Dropping dead ids at the source means they never reach
            -- the payload. unit.exists is GLOBAL, so live units on every
            -- page are still saved (#195).
            local live = {}
            for uid, s in pairs(aiState) do
                if unit.exists(uid) then live[uid] = s end
            end
            return live
        end,
        decode = function(_version, data)
            return data or {}
        end,
        validate = validateUnitAiData,
        references = unitAiReferences,
        -- Temporary C2 compatibility adapter (issue #761, requirement 15):
        -- clobber aiState wholesale from the decoded payload, exactly like
        -- the pre-#761 deserializer body. unitAi.onSaveLoaded is the
        -- #195/#191 off-page-preservation reconciliation that runs AFTER
        -- the engine-side restore — kept as-is rather than replaced by
        -- C2's eventual real per-page component model; the canonical
        -- decode/validate/apply split above is already separate from it.
        apply = function(data)
            -- Snapshot the pre-load singleton BEFORE clobbering. The
            -- payload holds save-time state for ALL pages, but a load
            -- should only touch the loaded page; onSaveLoaded uses this
            -- snapshot to restore still-live OFF-PAGE units' CURRENT
            -- state instead of the payload's stale copy (#195, #191).
            unitAi._preLoadState = {}
            for k, v in pairs(aiState) do unitAi._preLoadState[k] = v end
            -- Replace in-place so the package.loaded singleton sees it
            for k in pairs(aiState) do aiState[k] = nil end
            for k, v in pairs(data) do aiState[k] = v end
        end,
    })
end

return M
