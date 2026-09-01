-- The lua.unit_ai payload VALIDATOR (split out of
-- scripts/unit_ai_save.lua to stay under the #538 line budget, exactly
-- as unit_ai_save_refs.lua was). Everything here answers one question
-- asked once per load, at prepare time: is this payload's SHAPE sound
-- and does every content-definition id it durably names still exist?
--
-- Requireable and testable on its own (Test.Headless.Lua.SaveModules),
-- which is why it never requires unit_ai_construct.lua: that module (via
-- unit_ai_core.lua) expects scripts.unit_ai to already be self-registered
-- in package.loaded, a bootstrap order only unit_ai.lua's own require
-- chain guarantees.

local refsMod = require("scripts.unit_ai_save_refs")

local M = {}

local function buildItemDefSet()
    local set = {}
    for _, d in ipairs(item.listDefs() or {}) do
        set[d.name] = true
    end
    return set
end

local function buildBuildingDefSet()
    local set = {}
    for _, d in ipairs(building.listDefs() or {}) do
        set[d.name] = true
    end
    return set
end

-- #1844: a structure constructJob whose pack/kind no longer resolves is
-- NO LONGER a load rejection.
--
-- It used to be: this validator mirrored unit_ai_construct.lua's
-- packBuildInfo lookup and refused the whole save when the entry was
-- gone. That answer predates the engine having anywhere to put such a
-- job. Load staging now reconciles saved structure designations against
-- the registered catalogue and atomically self-clears the ones whose art
-- or build metadata has disappeared, refunding their persisted receipt
-- exactly once (World.Construct.Reconcile) -- and it must be allowed to
-- reach that path. Rejecting here would abort the entire load for a
-- situation the engine can now resolve losslessly.
--
-- What is REJECTED is unchanged and deliberately narrow: a malformed
-- payload, a building-category job naming a missing building def, and
-- any material/item id that no longer exists -- including this job's own
-- sourcing maps, checked below. Those are the cases where nothing
-- downstream can make the reference whole again.

-- craftJob/repairJob (issue #761 round-4 review) durably persist
-- content-definition ids (a recipe id, item def names for the crafted
-- item's shortfall-sourcing maps, and the item being repaired + its
-- repair consumable) rather than a copy of the definition itself --
-- correct per requirement 14, but that means a load with a since-
-- removed recipe/item must be REJECTED here, at prepare time, rather
-- than reaching apply()/the AI's next tick with a dangling reference
-- (the same "reject before any mutation" contract the def-reference
-- check in Engine.Scripting.Lua.API.Save already enforces for
-- building/unit defs). `itemDefs` is built once per validate() call,
-- not per job, since scanning item.listDefs() is a linear walk.
local function validateJobContentRefs(uid, s, itemDefs, buildingDefs, errs)
    local function checkItem(name, what)
        if name ~= nil and not itemDefs[name] then
            errs[#errs + 1] = "unit_ai: unit " .. tostring(uid) .. " " .. what
                .. " references unknown item def '" .. tostring(name) .. "'"
        end
    end
    local function checkItemKeys(t, what)
        if type(t) == "table" then
            for name in pairs(t) do checkItem(name, what) end
        end
    end
    if s.craftJob then
        if craft.get(s.craftJob.recipeId) == nil then
            errs[#errs + 1] = "unit_ai: unit " .. tostring(uid)
                .. " craftJob references unknown recipe '"
                .. tostring(s.craftJob.recipeId) .. "'"
        end
        checkItemKeys(s.craftJob.need, "craftJob.need")
        checkItemKeys(s.craftJob.fromGround, "craftJob.fromGround")
        checkItemKeys(s.craftJob.fromMule, "craftJob.fromMule")
        checkItemKeys(s.craftJob.fromCargo, "craftJob.fromCargo")
    end
    if s.repairJob then
        if repair.get(s.repairJob.recipeId) == nil then
            errs[#errs + 1] = "unit_ai: unit " .. tostring(uid)
                .. " repairJob references unknown recipe '"
                .. tostring(s.repairJob.recipeId) .. "'"
        end
        checkItem(s.repairJob.defName, "repairJob.defName")
        checkItem(s.repairJob.consumable, "repairJob.consumable")
    end
    -- round-5 review: the same "reject a dangling content reference
    -- before any mutation" contract extends to every other job type
    -- that persists a content-definition id -- constructJob's
    -- pack/kind (a structure-pack build entry) and material-sourcing
    -- maps, deliveryClaim/deliveryPendingTarget's material-sourcing
    -- maps (materials/claim/fromGround/fromMule are all item def
    -- names), and plantJob's crop (a flora species name).
    if s.constructJob and s.constructJob.category == "building" then
        -- A "building" job persists a durable building-def NAME
        -- (unit_ai_construct.lua's building.spawn(job.building, ...)
        -- call once the piece is placed), not a pack/kind pair -- round-6
        -- review: this must be checked too, the same as every other
        -- content id here.
        local job = s.constructJob
        if not buildingDefs[job.building] then
            errs[#errs + 1] = "unit_ai: unit " .. tostring(uid)
                .. " constructJob references unknown building def '"
                .. tostring(job.building) .. "'"
        end
    elseif s.constructJob then
        -- #1844: the pack/kind is NOT checked here (see the note at the
        -- top of this file) -- a job whose structure pack has gone is
        -- reconciled and self-cleared during load staging, and refusing
        -- it here would abort a load the engine can complete losslessly.
        -- The item ids it durably names are still checked, because a
        -- dangling one there reaches live execution.
        local job = s.constructJob
        checkItemKeys(job.need, "constructJob.need")
        checkItemKeys(job.fromGround, "constructJob.fromGround")
        checkItemKeys(job.fromMule, "constructJob.fromMule")
    end
    if s.deliveryClaim then
        checkItemKeys(s.deliveryClaim.materials, "deliveryClaim.materials")
        checkItemKeys(s.deliveryClaim.fromGround, "deliveryClaim.fromGround")
        checkItemKeys(s.deliveryClaim.fromMule, "deliveryClaim.fromMule")
    end
    if s.deliveryPendingTarget then
        checkItemKeys(s.deliveryPendingTarget.claim,
            "deliveryPendingTarget.claim")
        checkItemKeys(s.deliveryPendingTarget.fromGround,
            "deliveryPendingTarget.fromGround")
        checkItemKeys(s.deliveryPendingTarget.fromMule,
            "deliveryPendingTarget.fromMule")
    end
    if s.plantJob and s.plantJob.crop ~= nil
            and not flora.exists(s.plantJob.crop) then
        errs[#errs + 1] = "unit_ai: unit " .. tostring(uid)
            .. " plantJob references unknown crop species '"
            .. tostring(s.plantJob.crop) .. "'"
    end
end

-- Component-local validator (issue #761): `data` must be a table keyed
-- by positive-integer unit ids, each mapping to a state table. Deep
-- per-field validation of aiState's own shape is deliberately not
-- attempted here (it's a large, free-form utility-AI scratch table) --
-- this catches real corruption (wrong top-level shape) without gold-
-- plating a full schema for every possible field, EXCEPT for the
-- craftJob/repairJob content-definition ids above, which get a real
-- existence check since a dangling one there reaches live execution.
local function validateUnitAiData(data)
    if type(data) ~= "table" then
        return { "unit_ai: payload must be a table" }
    end
    local errs = {}
    local itemDefs = nil
    local buildingDefs = nil
    for uid, s in pairs(data) do
        if type(uid) ~= "number" or uid ~= math.floor(uid) or uid < 1 then
            errs[#errs + 1] = "unit_ai: invalid unit id key " .. tostring(uid)
        elseif type(s) ~= "table" then
            errs[#errs + 1] = "unit_ai: state for unit " .. tostring(uid)
                .. " is not a table"
        else
            -- Requirement 13/round-2 review: reject a wrong-kind or
            -- untagged reference wrapper before it can ever reach
            -- apply()/unwrapUnitState, which trusts field position
            -- alone. Runs for every unit entry, not just job-bearing
            -- ones -- attackTargetUid et al. carry no job field.
            refsMod.validateRefTags(uid, s, errs)
            if s.craftJob or s.repairJob or s.constructJob
                    or s.deliveryClaim or s.deliveryPendingTarget
                    or s.plantJob then
                itemDefs = itemDefs or buildItemDefSet()
                -- buildingDefs is only ever consulted for a "building"-
                -- category constructJob -- built lazily so every other
                -- scenario (craft/repair/delivery/plant-only saves, and
                -- every existing test/probe fixture that stubs `item`/
                -- `craft`/`repair`/`flora` but not `building`) never
                -- touches the `building` global at all.
                if s.constructJob and s.constructJob.category == "building" then
                    buildingDefs = buildingDefs or buildBuildingDefSet()
                end
                validateJobContentRefs(uid, s, itemDefs, buildingDefs, errs)
            end
        end
    end
    if #errs > 0 then return errs end
    return nil
end

M.validate = validateUnitAiData

return M
