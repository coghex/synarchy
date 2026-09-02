-- The reference SCHEMA for unit_ai's persisted aiState, and the single
-- walk every consumer of it shares (issue #1589). Own module with NO
-- local dependencies, because three otherwise-unrelated layers name it:
-- unit_ai_save_refs.lua (the reference report, the wire wrap/unwrap and
-- the tag validator), scripts/unit_ai_reconcile.lua (the post-load
-- stale-reference scrub), and their tests.

local M = {}

-------------------------------------------------------------------
-- THE reference schema (issue #1589)
-------------------------------------------------------------------
-- One row per persisted reference edge an aiState entry can carry.
-- Before #1589 the same field list was spelled out four times in
-- unit_ai_save_refs.lua (unitAiReferences, wrapUnitState,
-- unwrapUnitState, validateRefTags)
-- and a FIFTH time in unit_ai.lua's post-load scrub, which had silently
-- fallen six families behind the other four (craftJob, repairJob,
-- pickupOrder, ground forageTarget, forageLoot, harvestLoot). All five
-- walks now derive from this table, so a family declared here is
-- reported, wrapped, unwrapped, validated AND reconciled by
-- construction.
--
-- Row fields:
--   holder   "field" (top-level field holding a bare id), "table" (a
--            nested claim/job table whose `sub` holds the id), or
--            "list" (a field holding an array of ids).
--   field    the aiState field name; `sub` the id-bearing subfield.
--   kind     the reference kind -- the same vocabulary
--            World.Save.Integrity.luaEdgeResolves speaks.
--   required (table only) validateRefTags rejects a PAYLOAD whose
--            holder is present but this subfield is missing. Only for
--            a subfield set UNCONDITIONALLY at the holder's
--            construction site (see checkRefTag's own note below).
--   absentOk (table only) the holder is legitimately present with this
--            subfield absent in a LIVE row, so a reconcile leaves it
--            alone. A separate axis from `required`, which governs
--            payload shape only: every other holder names nothing
--            without its id, so an absent one reads as unresolved and
--            the holder is dropped -- what the pre-#1589 scrub did for
--            the treat/delivery claims via `liveSet[nil]` being falsy.
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
-- forageTarget is the one holder whose reference is conditional: a
-- "flora" target names a tile, not a ground item, and carries no gid.
local function isGroundForageTarget(t) return t.kind == "ground" end
local REF_SCHEMA = {
    { holder = "field", field = "attackTargetUid",  kind = "unit" },
    { holder = "field", field = "retreatThreatUid", kind = "unit" },
    { holder = "field", field = "notifyTarget",     kind = "unit" },
    { holder = "field", field = "lungeTarget",      kind = "unit" },
    { holder = "field", field = "buildTarget", kind = "building" },
    { holder = "field", field = "storeTarget", kind = "building" },
    { holder = "table", field = "treatClaim",   sub = "patient", kind = "unit" },
    { holder = "table", field = "treatPending", sub = "uid",     kind = "unit" },
    { holder = "table", field = "deliveryClaim", sub = "bid", kind = "building" },
    { holder = "table", field = "deliveryPendingTarget", sub = "bid",
      kind = "building" },
    -- craftCandidate is unit_ai_craft.lua's own releaseCraftJob
    -- companion clear (transient and stripped from every save, so after
    -- a load it is already nil -- this keeps the reconcile drop
    -- identical to that release path either way).
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
    -- The two subfields legitimately absent in a live row: bid is set
    -- only once the job reaches its walking phase and resolves a
    -- station, and groundGid (#1737) only while a ground-sourced target
    -- is still lying on the ground -- it is cleared the moment the
    -- pickup lands, because from then on the target is inventory and
    -- the gid names nothing. A gid that IS present and no longer
    -- resolves on the owning unit's page is a target that vanished
    -- under the job, so the whole job goes out through the same abort
    -- path (the item, if any, is returned and the claim released).
    { holder = "table", field = "repairJob", sub = "bid", kind = "building",
      absentOk = true, drop = "repairJob" },
    { holder = "table", field = "repairJob", sub = "groundGid",
      kind = "ground_item", absentOk = true, drop = "repairJob" },
    -- #1845: the building a construct job STAKED, held only between the
    -- queued spawn and the completion that retires the job. `absentOk`
    -- because that window is a phase, not the whole job -- a structure
    -- job never has one, and a building job does not until it arrives.
    -- A PRESENT id that no longer resolves means the stake never landed
    -- (its page torn down, its definition gone, the load that discarded
    -- the building queue): the job is dropped whole, its designation
    -- stays pending, and it is re-claimed and re-staked. That is the
    -- honest outcome -- nothing was built.
    { holder = "table", field = "constructJob", sub = "stakedBid",
      kind = "building", absentOk = true, drop = "constructJob" },
    { holder = "table", field = "pickupOrder", sub = "gid",
      kind = "ground_item" },
    { holder = "table", field = "forageTarget", sub = "gid",
      kind = "ground_item", when = isGroundForageTarget },
    { holder = "list", field = "forageLoot", kind = "ground_item",
      onEmpty = { "foragePhase", "forageLoot", "forageTarget" } },
    { holder = "list", field = "harvestLoot", kind = "ground_item",
      onEmpty = { "harvestPhase", "harvestLoot" } },
}
M.REF_SCHEMA = REF_SCHEMA

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
M.forEach = forEachSchemaEdge

return M
