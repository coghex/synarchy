-- Reference-field schema, references() traversal, and typed structured-
-- reference wrap/unwrap for unit_ai's persisted aiState (issue #764,
-- save-overhaul C3 requirement 13). Split out of scripts/unit_ai_save.lua
-- to stay under its line budget (#538, tools/lua_module_budget.py).

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
-- listed here, or it silently reintroduces the stale-ref bug -- AND
-- must be wrapped/unwrapped in wrapUnitState/unwrapUnitState below.
M.AI_UNIT_REF_FIELDS     = { "attackTargetUid", "retreatThreatUid",
                             "notifyTarget", "lungeTarget" }
M.AI_BUILDING_REF_FIELDS = { "buildTarget", "storeTarget" }

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
-- unit_ai_save.lua's TRANSIENT_CANDIDATE_FIELDS: they are stripped
-- before this function ever sees them.) CALLED by saveModules.prepareLoad
-- (requirement 11/12) and, since issue #764 (save-overhaul C3), its
-- returned {kind=,id=} list is actually CROSS-VALIDATED --
-- Engine.Scripting.Lua.API.Save's knownEntitiesFromSaveData /
-- World.Save.Integrity.luaReferenceErrors check every entry against
-- this load's real entity sets and log a diagnostic naming the
-- component/kind/id for one that doesn't resolve (#761 landed this
-- traversal as crash-checked-but-otherwise-unused; #764 is what
-- actually consumes the list). A dangling entry is NEVER rejected by
-- either that check or this component's own validator (per the #761
-- issue-review clarification: a target that legitimately died before
-- the save boundary must stay representable) -- it is cleared at
-- reconcile time instead, by unit_ai.lua's scrubStaleRefs/onSaveLoaded.
-- NB: any NEW nested claim/job field, or new loot-style list, that
-- stores a unit/building/bill/item/ground-item id MUST be added here
-- too (mirroring scrubStaleRefs for the claim/job fields it also
-- reconciles, AND wrapUnitState/unwrapUnitState below); any NEW
-- *Candidate-style scratch field should instead be added to
-- unit_ai_save.lua's TRANSIENT_CANDIDATE_FIELDS if it can embed a raw
-- id or a copy of live content, matching the existing ones.
-- Every edge is tagged with the OWNING unit id (issue #764, save-
-- overhaul C3): craft_bill/ground_item ids are PER-PAGE allocators (the
-- same number legitimately names two different real entities on two
-- different pages), so the Haskell-side cross-validator
-- (World.Save.Integrity.luaEdgeResolves) resolves them against the
-- OWNING unit's page specifically rather than session-wide -- session-
-- wide would let a reference meant for one page's (missing) bill
-- silently "resolve" against an unrelated same-numbered bill elsewhere.
-- unit/building/item_instance stay correctly session-wide (global
-- allocators), so `owner` is harmless-but-unused for those kinds.
-- `id` here may be the WRAPPED persisted shape ({__ref=kind, id=N}, see
-- wrapUnitState below) or a bare number -- refId() reads either, so
-- this function works unchanged against decoded (wrapped) data.
local function refId(v)
    if type(v) == "table" then return v.id end
    return v
end
-- `path` (round-2 review, issue #764) names the field this edge came
-- from, in the SAME dotted-path style Haskell-side integrity errors
-- already use (e.g. "craft-bills[page=...].station") -- "attackTargetUid",
-- "craftJob.billId", "forageLoot[3]" -- so a diagnostic naming a
-- dangling/wrong-kind Lua reference points at the actual field instead
-- of a synthetic "kind#id" string with no location in it.
local function unitAiReferences(data)
    local refs = {}
    local function addRef(kind, rawId, owner, path)
        local id = refId(rawId)
        if id ~= nil then
            refs[#refs + 1] = { kind = kind, id = id, owner = owner, path = path }
        end
    end
    local function addRefList(kind, ids, owner, path)
        if ids ~= nil then
            for i, id in ipairs(ids) do
                addRef(kind, id, owner, path .. "[" .. i .. "]")
            end
        end
    end
    for uid, s in pairs(data) do
        local prefix = "unit[" .. tostring(uid) .. "]"
        addRef("unit", uid, uid, prefix)
        for _, f in ipairs(M.AI_UNIT_REF_FIELDS) do
            addRef("unit", s[f], uid, prefix .. "." .. f)
        end
        for _, f in ipairs(M.AI_BUILDING_REF_FIELDS) do
            addRef("building", s[f], uid, prefix .. "." .. f)
        end
        if s.treatClaim then
            addRef("unit", s.treatClaim.patient, uid, prefix .. ".treatClaim.patient")
        end
        if s.treatPending then
            addRef("unit", s.treatPending.uid, uid, prefix .. ".treatPending.uid")
        end
        if s.deliveryClaim then
            addRef("building", s.deliveryClaim.bid, uid,
                prefix .. ".deliveryClaim.bid")
        end
        if s.deliveryPendingTarget then
            addRef("building", s.deliveryPendingTarget.bid, uid,
                prefix .. ".deliveryPendingTarget.bid")
        end
        if s.craftJob then
            addRef("craft_bill", s.craftJob.billId, uid, prefix .. ".craftJob.billId")
            addRef("building", s.craftJob.bid, uid, prefix .. ".craftJob.bid")
        end
        if s.repairJob then
            addRef("item_instance", s.repairJob.instanceId, uid,
                prefix .. ".repairJob.instanceId")
            addRef("building", s.repairJob.bid, uid, prefix .. ".repairJob.bid")
        end
        if s.pickupOrder then
            addRef("ground_item", s.pickupOrder.gid, uid, prefix .. ".pickupOrder.gid")
        end
        if s.forageTarget and s.forageTarget.kind == "ground" then
            addRef("ground_item", s.forageTarget.gid, uid,
                prefix .. ".forageTarget.gid")
        end
        addRefList("ground_item", s.forageLoot, uid, prefix .. ".forageLoot")
        addRefList("ground_item", s.harvestLoot, uid, prefix .. ".harvestLoot")
    end
    return refs
end

-- Typed structured references on the wire (issue #764, save-overhaul C3
-- requirement 13): every field M.AI_UNIT_REF_FIELDS/M.AI_BUILDING_REF_FIELDS/
-- unitAiReferences above declares is wrapped to {__ref=kind, id=N} at
-- snapshot/decode time and unwrapped back to a bare number at apply time
-- -- mirrors unitAiReferences' own field list exactly (any NEW reference
-- field needs BOTH updated together, same as the NB comment above
-- already requires). aiState's LIVE in-memory shape (read by every
-- OTHER module -- unit_ai_combat.lua, unit_ai_deliver.lua, ..., and
-- unit_ai.lua's own scrubStaleRefs) never changes: only the bytes on
-- disk do.
local function wrapRef(kind, id)
    if id == nil then return nil end
    return { __ref = kind, id = id }
end
local function unwrapRef(v)
    if type(v) == "table" then return v.id end
    return v
end
local function wrapRefList(kind, ids)
    if ids == nil then return nil end
    local out = {}
    for i, id in ipairs(ids) do out[i] = wrapRef(kind, id) end
    return out
end
local function unwrapRefList(vs)
    if vs == nil then return nil end
    local out = {}
    for i, v in ipairs(vs) do out[i] = unwrapRef(v) end
    return out
end
-- Shallow-copy `s[field]` and apply `fn` to its own `subfield`.
local function mapNested(s, field, subfield, fn)
    if s[field] == nil then return end
    local t = {}
    for k, v in pairs(s[field]) do t[k] = v end
    t[subfield] = fn(t[subfield])
    s[field] = t
end

local function wrapUnitState(s)
    local copy = {}
    for k, v in pairs(s) do copy[k] = v end
    for _, f in ipairs(M.AI_UNIT_REF_FIELDS) do
        copy[f] = wrapRef("unit", copy[f])
    end
    for _, f in ipairs(M.AI_BUILDING_REF_FIELDS) do
        copy[f] = wrapRef("building", copy[f])
    end
    mapNested(copy, "treatClaim", "patient", function(v) return wrapRef("unit", v) end)
    mapNested(copy, "treatPending", "uid", function(v) return wrapRef("unit", v) end)
    mapNested(copy, "deliveryClaim", "bid", function(v) return wrapRef("building", v) end)
    mapNested(copy, "deliveryPendingTarget", "bid", function(v) return wrapRef("building", v) end)
    if copy.craftJob then
        local t = {}
        for k, v in pairs(copy.craftJob) do t[k] = v end
        t.billId = wrapRef("craft_bill", t.billId)
        t.bid = wrapRef("building", t.bid)
        copy.craftJob = t
    end
    if copy.repairJob then
        local t = {}
        for k, v in pairs(copy.repairJob) do t[k] = v end
        t.instanceId = wrapRef("item_instance", t.instanceId)
        t.bid = wrapRef("building", t.bid)
        copy.repairJob = t
    end
    mapNested(copy, "pickupOrder", "gid", function(v) return wrapRef("ground_item", v) end)
    if copy.forageTarget and copy.forageTarget.kind == "ground" then
        mapNested(copy, "forageTarget", "gid", function(v) return wrapRef("ground_item", v) end)
    end
    copy.forageLoot = wrapRefList("ground_item", copy.forageLoot)
    copy.harvestLoot = wrapRefList("ground_item", copy.harvestLoot)
    return copy
end

local function unwrapUnitState(s)
    local copy = {}
    for k, v in pairs(s) do copy[k] = v end
    for _, f in ipairs(M.AI_UNIT_REF_FIELDS) do copy[f] = unwrapRef(copy[f]) end
    for _, f in ipairs(M.AI_BUILDING_REF_FIELDS) do copy[f] = unwrapRef(copy[f]) end
    mapNested(copy, "treatClaim", "patient", unwrapRef)
    mapNested(copy, "treatPending", "uid", unwrapRef)
    mapNested(copy, "deliveryClaim", "bid", unwrapRef)
    mapNested(copy, "deliveryPendingTarget", "bid", unwrapRef)
    if copy.craftJob then
        local t = {}
        for k, v in pairs(copy.craftJob) do t[k] = v end
        t.billId = unwrapRef(t.billId)
        t.bid = unwrapRef(t.bid)
        copy.craftJob = t
    end
    if copy.repairJob then
        local t = {}
        for k, v in pairs(copy.repairJob) do t[k] = v end
        t.instanceId = unwrapRef(t.instanceId)
        t.bid = unwrapRef(t.bid)
        copy.repairJob = t
    end
    mapNested(copy, "pickupOrder", "gid", unwrapRef)
    if copy.forageTarget and type(copy.forageTarget.gid) == "table" then
        mapNested(copy, "forageTarget", "gid", unwrapRef)
    end
    copy.forageLoot = unwrapRefList(copy.forageLoot)
    copy.harvestLoot = unwrapRefList(copy.harvestLoot)
    return copy
end

function M.wrapAiState(data)
    local out = {}
    for uid, s in pairs(data) do out[uid] = wrapUnitState(s) end
    return out
end
function M.unwrapAiState(data)
    local out = {}
    for uid, s in pairs(data) do out[uid] = unwrapUnitState(s) end
    return out
end
M.references = unitAiReferences

-- Wrapper-KIND validation (issue #764 round-2 review): unwrapRef/refId
-- above read `.id` off ANY table unconditionally, trusting the field's
-- POSITION in the schema alone to mean the wrapper's `__ref` tag is
-- correct. A malformed or hand-edited v2 payload can carry a
-- wrong-kind wrapper (e.g. attackTargetUid = {__ref="building", id=9})
-- that would silently apply as if it were the right kind, since
-- nothing before apply() ever compares the tag to the field's expected
-- kind. checkRefTag/M.validateRefTags close that gap at validate() time
-- (prepareLoadImpl runs validate() straight after decode(), before any
-- apply() ever touches live state, so a mismatch here aborts the whole
-- load per requirement 11 rather than reaching unwrapUnitState) --
-- mirrors wrapUnitState/unwrapUnitState/unitAiReferences' own field
-- walk; any NEW reference field needs this walk updated too, same as
-- the NB comment above already requires for the other three.
--
-- Round-3 review: a `.__ref` tag matching the field's expected kind is
-- not enough on its own -- {__ref="unit", id="bad"} would still pass a
-- tag-only check, unwrap into live aiState as a non-numeric id, and
-- (Engine.Scripting.Lua.API.Save's readReferenceEdgeField, which
-- Lua.tointeger()s the id) silently drop that edge from every
-- diagnostic entirely rather than reporting it as malformed. Also
-- reject a non-integer id here -- the same "well-formed integer"
-- contract validateUnitAiData already enforces on the OUTER per-unit
-- key. The minimum differs by kind: unit/building/craft_bill/
-- item_instance allocators all start at 1 (Unit.Types.umNextId,
-- Building.Types.bmNextId, Craft.Bills.emptyCraftBills,
-- Engine.Core.Init's nextItemInstanceIdRef), so 0 can never be a real
-- id for those -- but Item.Ground's ground-item allocator is
-- ZERO-based (emptyGroundItems starts gisNextId at 0, so the very
-- first spawned ground item legitimately has gid=0). Round-3 review
-- itself caught this: a blanket "id >= 1" incorrectly rejected a valid
-- ground_item reference of 0.
local GROUND_ITEM_KIND = "ground_item"
local function checkRefTag(v, expectedKind, uid, path, errs)
    if v == nil then return end
    if type(v) ~= "table" or v.__ref == nil then
        errs[#errs + 1] = "unit_ai: unit " .. tostring(uid) .. " " .. path
            .. " is not a typed reference (expected __ref='"
            .. expectedKind .. "')"
        return
    end
    if v.__ref ~= expectedKind then
        errs[#errs + 1] = "unit_ai: unit " .. tostring(uid) .. " " .. path
            .. " has wrong reference kind '" .. tostring(v.__ref)
            .. "' (expected '" .. expectedKind .. "')"
        return
    end
    local minId = (expectedKind == GROUND_ITEM_KIND) and 0 or 1
    if type(v.id) ~= "number" or v.id ~= math.floor(v.id) or v.id < minId then
        errs[#errs + 1] = "unit_ai: unit " .. tostring(uid) .. " " .. path
            .. " has a non-numeric or invalid id (" .. tostring(v.id) .. ")"
    end
end
function M.validateRefTags(uid, s, errs)
    for _, f in ipairs(M.AI_UNIT_REF_FIELDS) do
        checkRefTag(s[f], "unit", uid, f, errs)
    end
    for _, f in ipairs(M.AI_BUILDING_REF_FIELDS) do
        checkRefTag(s[f], "building", uid, f, errs)
    end
    if s.treatClaim then
        checkRefTag(s.treatClaim.patient, "unit", uid, "treatClaim.patient", errs)
    end
    if s.treatPending then
        checkRefTag(s.treatPending.uid, "unit", uid, "treatPending.uid", errs)
    end
    if s.deliveryClaim then
        checkRefTag(s.deliveryClaim.bid, "building", uid, "deliveryClaim.bid", errs)
    end
    if s.deliveryPendingTarget then
        checkRefTag(s.deliveryPendingTarget.bid, "building", uid,
            "deliveryPendingTarget.bid", errs)
    end
    if s.craftJob then
        checkRefTag(s.craftJob.billId, "craft_bill", uid, "craftJob.billId", errs)
        checkRefTag(s.craftJob.bid, "building", uid, "craftJob.bid", errs)
    end
    if s.repairJob then
        checkRefTag(s.repairJob.instanceId, "item_instance", uid,
            "repairJob.instanceId", errs)
        checkRefTag(s.repairJob.bid, "building", uid, "repairJob.bid", errs)
    end
    if s.pickupOrder then
        checkRefTag(s.pickupOrder.gid, "ground_item", uid, "pickupOrder.gid", errs)
    end
    if s.forageTarget and s.forageTarget.kind == "ground" then
        checkRefTag(s.forageTarget.gid, "ground_item", uid, "forageTarget.gid", errs)
    end
    if s.forageLoot then
        for i, v in ipairs(s.forageLoot) do
            checkRefTag(v, "ground_item", uid, "forageLoot[" .. i .. "]", errs)
        end
    end
    if s.harvestLoot then
        for i, v in ipairs(s.harvestLoot) do
            checkRefTag(v, "ground_item", uid, "harvestLoot[" .. i .. "]", errs)
        end
    end
end

return M
