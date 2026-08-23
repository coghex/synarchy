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
-- to it (#195). scripts/unit_ai_reconcile.lua's scrubStaleRefs clears
-- any ref whose target isn't in the surviving loaded-page set.
-- NB: any NEW aiState field that stores a unit/building id MUST be
-- listed here, or it silently reintroduces the stale-ref bug. Since
-- issue #1589 these two lists feed REF_SCHEMA below, which is the ONE
-- declaration the wrap, unwrap, reference-report, tag-validate and
-- post-load reconcile walks are all derived from -- so a field added
-- here is covered by every one of them, with no second edit to make.
M.AI_UNIT_REF_FIELDS     = { "attackTargetUid", "retreatThreatUid",
                             "notifyTarget", "lungeTarget" }
M.AI_BUILDING_REF_FIELDS = { "buildTarget", "storeTarget" }

-------------------------------------------------------------------
-- THE reference schema (issue #1589)
-------------------------------------------------------------------
-- One row per persisted reference edge an aiState entry can carry.
-- Before #1589 the same field list was spelled out four times here
-- (unitAiReferences, wrapUnitState, unwrapUnitState, validateRefTags)
-- and a FIFTH time in unit_ai.lua's post-load scrub, which had
-- silently fallen six families behind the other four (craftJob,
-- repairJob, pickupOrder, ground forageTarget, forageLoot,
-- harvestLoot). All five walks now derive from this table, so a family
-- declared here is reported, wrapped, unwrapped, validated AND
-- reconciled by construction.
--
-- Row fields:
--   holder   "field" (top-level field holding a bare id), "table" (a
--            nested claim/job table whose `sub` holds the id), or
--            "list" (a field holding an array of ids).
--   field    the aiState field name; `sub` the id-bearing subfield.
--   kind     the reference kind -- the same vocabulary
--            World.Save.Integrity.luaEdgeResolves speaks.
--   required (table only) validateRefTags rejects a payload whose
--            holder is present but this subfield is missing. Only for
--            a subfield set UNCONDITIONALLY at the holder's
--            construction site (see checkRefTag's own note below).
--   when     (table only) predicate on the holder -- the edge exists
--            only in some of its shapes.
--   also     (table only) sibling fields cleared together with the
--            holder when a post-load reconcile drops it, so no
--            half-dismantled job table is left behind.
--   drop     (table only) the module-owned release path the reconcile
--            must go through instead of a bare field assignment;
--            scripts/unit_ai_reconcile.lua refuses to run without a
--            hook for every `drop` named here.
--   onEmpty  (list only) fields to clear when a reconcile filters the
--            list down to nothing -- what the owning action's own
--            exhaustion path clears.
local REF_SCHEMA = {}
for _, f in ipairs(M.AI_UNIT_REF_FIELDS) do
    REF_SCHEMA[#REF_SCHEMA + 1] = { holder = "field", field = f, kind = "unit" }
end
for _, f in ipairs(M.AI_BUILDING_REF_FIELDS) do
    REF_SCHEMA[#REF_SCHEMA + 1] = { holder = "field", field = f,
                                    kind = "building" }
end
-- forageTarget is the one holder whose reference is conditional: a
-- "flora" target names a tile, not a ground item, and carries no gid.
local function isGroundForageTarget(t) return t.kind == "ground" end
local NESTED_REF_SCHEMA = {
    { holder = "table", field = "treatClaim",   sub = "patient", kind = "unit" },
    { holder = "table", field = "treatPending", sub = "uid",     kind = "unit" },
    { holder = "table", field = "deliveryClaim", sub = "bid", kind = "building" },
    { holder = "table", field = "deliveryPendingTarget", sub = "bid",
      kind = "building" },
    -- craftCandidate is unit_ai_craft.lua's own releaseCraftJob
    -- companion clear (it is transient and stripped from every save, so
    -- after a load it is already nil -- this keeps the reconcile drop
    -- identical to the module's release path either way).
    { holder = "table", field = "craftJob", sub = "billId", kind = "craft_bill",
      required = true, also = { "craftCandidate" } },
    { holder = "table", field = "craftJob", sub = "bid", kind = "building",
      required = true, also = { "craftCandidate" } },
    -- repairJob goes out through unit_ai_repair.lua's abort path, not a
    -- field assignment: an already-fetched item has to be handed back
    -- and the repairClaims entry released, exactly as on any other
    -- abort (issue #1589 requirement 3).
    { holder = "table", field = "repairJob", sub = "instanceId",
      kind = "item_instance", required = true, drop = "repairJob" },
    { holder = "table", field = "repairJob", sub = "bid", kind = "building",
      drop = "repairJob" },
    { holder = "table", field = "pickupOrder", sub = "gid",
      kind = "ground_item" },
    { holder = "table", field = "forageTarget", sub = "gid",
      kind = "ground_item", when = isGroundForageTarget },
    { holder = "list", field = "forageLoot", kind = "ground_item",
      onEmpty = { "foragePhase", "forageLoot", "forageTarget" } },
    { holder = "list", field = "harvestLoot", kind = "ground_item",
      onEmpty = { "harvestPhase", "harvestLoot" } },
}
for _, row in ipairs(NESTED_REF_SCHEMA) do
    REF_SCHEMA[#REF_SCHEMA + 1] = row
end
M.REF_SCHEMA = REF_SCHEMA

-- Every reference this component carries (requirement 12) -- every
-- REF_SCHEMA row above, plus knownLocations (#915 -- the only kind
-- whose edge also carries its own `page`, see addRef below) and
-- (round-6 review) the OUTER per-unit key itself, the same "the id
-- this entry is keyed by is a reference too" pattern
-- building_spawn.lua's own references() already uses for its
-- per-building key.
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
-- reconcile time instead, by scripts/unit_ai_reconcile.lua's
-- scrubStaleRefs, reached through unit_ai.lua's onSaveLoaded. Since
-- issue #1589 that promise holds for EVERY family listed above rather
-- than the flat fields and treat/delivery claims alone: the reconcile
-- walks this same REF_SCHEMA, so no family can be declared here and
-- silently skipped there. knownLocations is the one deliberate
-- exception -- its (page, id) identity is per-page in its OWN declared
-- page rather than the owning unit's, so it keeps the specialized
-- scrub (and the separate count) unit_ai_locations.lua owns.
-- NB: any NEW nested claim/job field, or new loot-style list, that
-- stores a unit/building/bill/item/ground-item id is added as a
-- REF_SCHEMA row above -- one edit, not one per walk; any NEW
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
-- THE walk over one per-unit row's declared reference edges -- the
-- single traversal every consumer of REF_SCHEMA shares (issue #1589).
-- `emit(row, value, path, index)` pairs each schema row with the value
-- actually stored there: `value` may be nil (an absent optional field,
-- or a `required` one a malformed payload dropped -- precisely what
-- validateRefTags has to be shown), `path` is the unprefixed field
-- path ("craftJob.billId", "forageLoot[3]") in the same spelling the
-- diagnostics already used, and `index` is the array position for a
-- list-held edge. A `table` holder is visited only when present AND
-- passing its own `when`, so an absent holder yields no phantom edge.
local function forEachSchemaEdge(s, emit)
    for _, row in ipairs(REF_SCHEMA) do
        if row.holder == "field" then
            emit(row, s[row.field], row.field)
        elseif row.holder == "table" then
            local t = s[row.field]
            if t ~= nil and (row.when == nil or row.when(t)) then
                emit(row, t[row.sub], row.field .. "." .. row.sub)
            end
        else
            local list = s[row.field]
            if list ~= nil then
                for i, v in ipairs(list) do
                    emit(row, v, row.field .. "[" .. i .. "]", i)
                end
            end
        end
    end
end
M.forEachRefEdge = forEachSchemaEdge

-- `path` (round-2 review, issue #764) names the field this edge came
-- from, in the SAME dotted-path style Haskell-side integrity errors
-- already use (e.g. "craft-bills[page=...].station") -- "attackTargetUid",
-- "craftJob.billId", "forageLoot[3]" -- so a diagnostic naming a
-- dangling/wrong-kind Lua reference points at the actual field instead
-- of a synthetic "kind#id" string with no location in it.
local function unitAiReferences(data)
    local refs = {}
    -- `page` (#915) is the edge's OWN declared world page, supplied only
    -- by the one kind whose id is meaningless without it
    -- (location_instance -- a PER-PAGE allocator whose durable identity
    -- is (page, id), #911). Unlike craft_bill/ground_item, which borrow
    -- the OWNING unit's page, a remembered location names its page
    -- itself: the page is part of what was remembered, not a fact about
    -- where the remembering unit currently stands. nil (and harmlessly
    -- absent on the wire) for every other kind.
    local function addRef(kind, rawId, owner, path, page)
        local id = refId(rawId)
        if id ~= nil then
            refs[#refs + 1] = { kind = kind, id = id, owner = owner,
                                path = path, page = page }
        end
    end
    for uid, s in pairs(data) do
        local prefix = "unit[" .. tostring(uid) .. "]"
        addRef("unit", uid, uid, prefix)
        forEachSchemaEdge(s, function(row, value, path)
            addRef(row.kind, value, uid, prefix .. "." .. path)
        end)
        -- Per-unit location memory (#915). Each entry IS its own typed
        -- reference on the wire (see wrapUnitState), so the id is read
        -- off the entry itself rather than a nested field.
        if s.knownLocations then
            for i, k in ipairs(s.knownLocations) do
                addRef("location_instance", k.id, uid,
                       prefix .. ".knownLocations[" .. i .. "]", k.page)
            end
        end
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
-- Per-unit location memory (#915): the wire codec lives with the module
-- that owns the field's live shape (scripts/unit_ai_locations.lua) --
-- both to keep this file inside its line budget and because a memory
-- entry, unlike every field above, is not a bare id that gets boxed.
-- It is already a record ({ page, id, x, y }), so the ENTRY ITSELF is
-- the typed reference and only gains a `__ref` tag. Requiring it here
-- is safe: unit_ai_locations.lua requires nothing at module scope.
local locationsMod  = require("scripts.unit_ai_locations")
local LOCATION_KIND = locationsMod.REF_KIND

-- Rewrite every declared reference in a per-unit COPY, driven by the
-- same REF_SCHEMA the reference walk above uses -- `mapRef(kind, v)`
-- for a single edge, `mapList(kind, ids)` for a collection-held one.
-- Nested holders are shallow-copied before being touched (the live
-- aiState table must never be mutated by a snapshot).
local function mapSchemaRefs(copy, mapRef, mapList)
    for _, row in ipairs(REF_SCHEMA) do
        if row.holder == "field" then
            copy[row.field] = mapRef(row.kind, copy[row.field])
        elseif row.holder == "table" then
            local t = copy[row.field]
            if t ~= nil and (row.when == nil or row.when(t)) then
                local n = {}
                for k, v in pairs(t) do n[k] = v end
                n[row.sub] = mapRef(row.kind, n[row.sub])
                copy[row.field] = n
            end
        else
            copy[row.field] = mapList(row.kind, copy[row.field])
        end
    end
end

-- mapSchemaRefs' mapRef/mapList take the edge's kind first; the
-- unwrap direction reads the wrapper's own tag and does not need
-- it (checkRefTag has already rejected a wrong-kind wrapper by
-- the time anything unwraps).
local function unwrapRefAt(_kind, v) return unwrapRef(v) end
local function unwrapRefListAt(_kind, vs) return unwrapRefList(vs) end

-- __owner (round-6 review, issue #764): the per-unit KEY this state
-- table is stored under (`aiState[uid]`) is itself a durable cross-
-- component reference -- unitAiReferences already reports it as one
-- (`addRef("unit", uid, uid, prefix)` above) -- but the Lua table KEY
-- alone has no wire kind/scope tag the way a wrapped VALUE field does,
-- unlike World.Save.Component.Entities' PageSimDTO.psSim, whose
-- analogous Haskell HashMap key was typed via SamePageRef in round 3.
-- There is no Lua equivalent of that wire-transparent newtype trick:
-- scripts/lib/data_codec.lua's canonical map encoding only supports
-- integer/string keys, so a table can never BE a map key on this wire
-- format. __owner is the alternative the round-6 review itself allowed
-- for ("another typed-key representation"): a self-describing
-- {__ref="unit", id=uid} field carried INSIDE the row's own value,
-- redundant with the key by construction but giving the row a real,
-- validated typed reference to its own identity. Stripped back off on
-- unwrap -- aiState's LIVE in-memory shape never grows this field.
local function wrapUnitState(uid, s)
    local copy = {}
    for k, v in pairs(s) do copy[k] = v end
    copy.__owner = wrapRef("unit", uid)
    mapSchemaRefs(copy, wrapRef, wrapRefList)
    copy.knownLocations = locationsMod.wrapForSave(copy.knownLocations)
    return copy
end

local function unwrapUnitState(s)
    local copy = {}
    for k, v in pairs(s) do copy[k] = v end
    copy.__owner = nil
    mapSchemaRefs(copy, unwrapRefAt, unwrapRefListAt)
    copy.knownLocations = locationsMod.unwrapFromSave(copy.knownLocations)
    return copy
end

function M.wrapAiState(data)
    local out = {}
    for uid, s in pairs(data) do out[uid] = wrapUnitState(uid, s) end
    return out
end
function M.unwrapAiState(data)
    local out = {}
    for uid, s in pairs(data) do out[uid] = unwrapUnitState(s) end
    return out
end
-- v2->v3 migration (round-6 review, issue #764): a v2 payload already
-- has every OTHER reference field wrapped -- only __owner is new in
-- v3 -- so this must NOT re-wrap already-wrapped fields the way
-- wrapUnitState (built for BARE v1 input) would.
function M.addOwnerToAiState(data)
    local out = {}
    for uid, s in pairs(data) do
        local copy = {}
        for k, v in pairs(s) do copy[k] = v end
        copy.__owner = wrapRef("unit", uid)
        out[uid] = copy
    end
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
-- load per requirement 11 rather than reaching unwrapUnitState) -- and
-- since issue #1589 it walks the SAME REF_SCHEMA wrapUnitState /
-- unwrapUnitState / unitAiReferences do, so a new reference field is
-- covered here the moment its row exists.
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
-- `required` (round-6 review, issue #764): most reference fields are
-- legitimately absent depending on gameplay state (no active job, no
-- pending claim, ...), so `nil` is valid by default -- but a field that
-- is ALWAYS set together with its own enclosing job/claim table at
-- CONSTRUCTION time (verified against the actual construction call
-- site, not assumed) is a structural invariant: a v2/v3 payload whose
-- container is present but the required field is missing is malformed,
-- not merely "job in an earlier phase" -- and must be rejected here
-- rather than silently applying with the field dropped (the AI would
-- otherwise discard the job with no diagnostic at all). Verified
-- required today: craftJob.billId/bid (unit_ai_craft.lua sets both
-- unconditionally the instant craftJob is created) and
-- repairJob.instanceId (unit_ai_repair.lua, same). Left optional:
-- every other nested field -- including repairJob.bid, which
-- unit_ai_repair.lua DOES set, but only once the job reaches its
-- walking phase and resolves a station (building.findStation, see that
-- module's repairExecute). A job saved before that point legitimately
-- carries no bid, so requiring it would reject a real repair job; it
-- is still a declared reference, and is still wrapped, validated and
-- reconciled like any other whenever it IS present.
local function checkRefTag(v, expectedKind, uid, path, errs, required)
    if v == nil then
        if required then
            errs[#errs + 1] = "unit_ai: unit " .. tostring(uid) .. " " .. path
                .. " is required but missing (expected a typed reference "
                .. "with __ref='" .. expectedKind .. "')"
        end
        return
    end
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
-- __owner must be present, correctly tagged/well-formed (checkRefTag),
-- AND its id must exactly equal the outer key it's redundant with --
-- a tag-only/shape-only check would still accept a wrapper that
-- validly names a DIFFERENT unit than the row it's attached to.
local function checkOwnerRef(v, uid, errs)
    local before = #errs
    checkRefTag(v, "unit", uid, "__owner", errs, true)
    if #errs > before then return end
    if v.id ~= uid then
        errs[#errs + 1] = "unit_ai: unit " .. tostring(uid)
            .. " __owner id (" .. tostring(v.id)
            .. ") does not match its own key (" .. tostring(uid) .. ")"
    end
end
-- One persisted location memory (#915). checkRefTag already covers the
-- typed-reference half (present, __ref == "location_instance", integer
-- id >= 1 -- Location.Instance.firstLocationInstanceId, the same
-- allocator base every non-ground-item kind uses). The rest is what
-- makes a per-page identity usable at all: without a well-formed page
-- string the id names nothing (two pages allocate the same numbers), and
-- without numeric anchor coordinates the nearest-lookup arithmetic in
-- unit_ai_locations.lua would error on a value that decoded "fine".
local function checkKnownLocation(v, uid, path, errs)
    local before = #errs
    checkRefTag(v, LOCATION_KIND, uid, path, errs, true)
    if #errs > before then return end
    if type(v.page) ~= "string" or v.page == "" then
        errs[#errs + 1] = "unit_ai: unit " .. tostring(uid) .. " " .. path
            .. " has a missing or non-string world page ("
            .. tostring(v.page) .. ")"
    end
    if type(v.x) ~= "number" or type(v.y) ~= "number" then
        errs[#errs + 1] = "unit_ai: unit " .. tostring(uid) .. " " .. path
            .. " has non-numeric remembered coordinates ("
            .. tostring(v.x) .. ", " .. tostring(v.y) .. ")"
    end
end

function M.validateRefTags(uid, s, errs)
    checkOwnerRef(s.__owner, uid, errs)
    forEachSchemaEdge(s, function(row, value, path)
        checkRefTag(value, row.kind, uid, path, errs, row.required)
    end)
    if s.knownLocations then
        for i, v in ipairs(s.knownLocations) do
            checkKnownLocation(v, uid, "knownLocations[" .. i .. "]", errs)
        end
    end
end

return M
