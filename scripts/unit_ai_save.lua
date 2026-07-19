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

-- Per-unit "*Candidate" fields (issue #761 requirement 13/14): scratch
-- space a utility function fills in on ITS OWN tick and that the
-- matching execute() immediately consumes or drops within the same
-- tick -- never a committed, durable decision the way a Job/Claim is.
-- Some of these (craftCandidate/repairCandidate in particular) embed a
-- full live content definition (craft.get()'s RecipeDef, by way of
-- unit_ai_craft.lua's `cand.recipe`) rather than a stable id, which
-- requirement 14 forbids persisting as a copy. Since every candidate is
-- cheaply re-derivable from scratch on the very next tick (the utility
-- functions always re-scan rather than trusting a stale candidate), the
-- simplest and most correct fix is to never persist any of them at all
-- -- stripped at snapshot time (below), so they load back as nil and
-- get recomputed fresh, exactly like Lua RNG/iteration state is never
-- persisted for the same reason.
local TRANSIENT_CANDIDATE_FIELDS = {
    "chopCandidate", "digCandidate", "tillCandidate", "plantCandidate",
    "constructCandidate", "repairCandidate", "craftCandidate",
}

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

-- Self-contained mirror of unit_ai_construct.lua's packBuildInfo lookup
-- (issue #761 round-5 review): does a pack/kind still resolve to a real
-- structure-pack build entry? Deliberately NOT a require of
-- unit_ai_construct.lua itself -- that module (via unit_ai_core.lua)
-- expects scripts.unit_ai to already be self-registered in
-- package.loaded, a bootstrap order only unit_ai.lua's own require
-- chain guarantees, which this standalone validator (requireable and
-- tested on its own, see Test.Headless.Lua.SaveModules) cannot assume.
-- Uncached (unlike the original): prepareLoad runs once per load, not
-- per tick, so there's no hot-path cost to justify the cache's
-- complexity here.
local function packHasBuildEntry(pack, kind)
    if type(pack) ~= "string" or type(kind) ~= "string" then return false end
    local y = engine.loadYaml("data/structure_packs/" .. pack .. ".yaml")
    local build = y and y.build
    return build ~= nil and build[kind] ~= nil
end

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
        local job = s.constructJob
        if not packHasBuildEntry(job.pack, job.kind) then
            errs[#errs + 1] = "unit_ai: unit " .. tostring(uid)
                .. " constructJob references unknown structure pack/kind '"
                .. tostring(job.pack) .. "/" .. tostring(job.kind) .. "'"
        end
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
        elseif s.craftJob or s.repairJob or s.constructJob
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
    if #errs > 0 then return errs end
    return nil
end

-- Every reference this component carries (requirement 12) -- unit/
-- building/craft-bill/item/ground-item ids reachable from a per-unit
-- aiState entry, including ones nested inside claim/job tables and
-- collection-held ones inside loot lists, plus (round-6 review) the
-- OUTER per-unit key itself -- the same "the id this entry is keyed by
-- is a reference too" pattern building_spawn.lua's own references()
-- already uses for its per-building key:
--   unit_ai_medic.lua's treatClaim/treatPending
--   unit_ai_deliver.lua's deliveryClaim/deliveryPendingTarget
--   unit_ai_craft.lua's craftJob
--   unit_ai_repair.lua's repairJob
--   unit_ai_pickup.lua's pickupOrder
--   unit_ai_needs.lua's forageTarget/forageLoot
--   unit_ai_farm.lua's harvestLoot
-- (the *Candidate fields carry no reference here at all -- see
-- TRANSIENT_CANDIDATE_FIELDS above: they are stripped before this
-- function ever sees them.) Traversed here for documentation/
-- diagnostics (actually CALLED by saveModules.prepareLoad, requirement
-- 11/12 -- not merely declared and left dead); a dangling entry is NOT
-- rejected by this validator (per the #761 issue-review clarification:
-- a target that legitimately died before the save boundary must stay
-- representable) -- it is cleared at reconcile time instead, by
-- unit_ai.lua's scrubStaleRefs/onSaveLoaded.
-- NB: any NEW nested claim/job field, or new loot-style list, that
-- stores a unit/building/bill/item/ground-item id MUST be added here
-- too (mirroring scrubStaleRefs for the claim/job fields it also
-- reconciles); any NEW *Candidate-style scratch field should instead be
-- added to TRANSIENT_CANDIDATE_FIELDS if it can embed a raw id or a
-- copy of live content, matching the existing ones.
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
    for uid, s in pairs(data) do
        addRef("unit", uid)
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
        if s.repairJob then
            addRef("item_instance", s.repairJob.instanceId)
            addRef("building", s.repairJob.bid)
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

-- A shallow copy of one unit's aiState entry with every transient
-- candidate field stripped (requirement 13/14) -- see
-- TRANSIENT_CANDIDATE_FIELDS. Nested tables that DO get persisted
-- (craftJob, treatClaim, ...) are shared by reference with the live
-- state, which is safe: the snapshot is encoded (deep-copied into a
-- byte string) before this tick's AI loop could mutate them again.
local function snapshotUnitState(s)
    local copy = {}
    for k, v in pairs(s) do copy[k] = v end
    for _, f in ipairs(TRANSIENT_CANDIDATE_FIELDS) do copy[f] = nil end
    -- constructJob (round-5 review) retains the full parsed structure-
    -- pack YAML build-cost table (unit_ai_construct.lua's
    -- packBuildInfo -- materials/build_work/etc.) rather than a stable
    -- id, which requirement 14 forbids persisting as a copy. Unlike
    -- the *Candidate fields above, constructJob is a multi-tick
    -- DURABLE job (can't just be dropped and re-derived next tick), so
    -- only its .build sub-field is stripped, on a shallow copy of the
    -- job table itself -- constructJob is a reference SHARED with the
    -- live aiState entry, so mutating it in place here would corrupt
    -- the live job the AI is still working. unit_ai_construct.lua's
    -- refundStructureMaterials already falls back to a fresh
    -- packBuildInfo(job.pack, job.kind) lookup whenever job.build is
    -- absent, so nothing needs to re-populate it after a load.
    if copy.constructJob and copy.constructJob.build ~= nil then
        local jobCopy = {}
        for jk, jv in pairs(copy.constructJob) do jobCopy[jk] = jv end
        jobCopy.build = nil
        copy.constructJob = jobCopy
    end
    return copy
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
        -- Requirement 2 (round-8 review): unitAiReferences above
        -- declares every reference KIND this component's data actually
        -- carries -- "unit"/"building" (AI_UNIT_REF_FIELDS/
        -- AI_BUILDING_REF_FIELDS, claim/job bid/patient/uid fields),
        -- "craft_bill" (craftJob.billId), and "ground_item"
        -- (pickupOrder/forageTarget/forageLoot/harvestLoot). Each maps
        -- to the Haskell component that owns that entity kind:
        -- units/buildings/craft-bills directly, and ground items via
        -- world-activity ("designations/flora/crops/ground/spoil" --
        -- see the persistence contract). "item_instance"
        -- (repairJob.instanceId) is carried inventory, owned by the
        -- "units" component's own snapshot, not a separate one.
        deps = { "units", "buildings", "craft-bills", "world-activity" },
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
            -- Also strips every transient candidate field
            -- (snapshotUnitState, requirement 13/14) -- scratch
            -- utility-scoring state that's cheaply re-derived next
            -- tick, one path of which would otherwise copy a live
            -- content definition (a full RecipeDef) into the payload.
            local live = {}
            for uid, s in pairs(aiState) do
                if unit.exists(uid) then live[uid] = snapshotUnitState(s) end
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
