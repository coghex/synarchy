-- Post-load reconciliation for unit_ai's persisted aiState (issue
-- #1589). Split out of scripts/unit_ai.lua, which is at its line
-- budget (#538, tools/lua_module_budget.py); unit_ai.lua's
-- onSaveLoaded is now a thin forwarder into M.reconcile below.
--
-- What this module exists to guarantee: EVERY reference family
-- scripts/unit_ai_ref_schema.lua declares is resolved or cleared on a
-- surviving row at the reconciliation boundary. Both the Haskell-side
-- cross-validator (World.Save.Integrity.luaReferenceErrors) and this
-- component's own validator deliberately TOLERATE a dangling
-- reference -- a target that legitimately died before the save
-- boundary must stay representable -- on the stated promise that it is
-- cleared here instead. Before #1589 the scrub reached only the flat
-- unit/building fields and the treat/delivery claims, so six declared
-- families (craftJob, repairJob, pickupOrder, ground forageTarget,
-- forageLoot, harvestLoot) crossed the boundary untouched.
--
-- Per-page ids are the reason this needs an explicit context rather
-- than live queries. craft_bill and ground_item are PER-PAGE
-- allocators, and the live Lua queries that could answer "does this id
-- exist?" resolve through the ACTIVE page (craft.getBill,
-- item.listGround) while the mutating verb they would authorize
-- resolves through the OWNING unit's page (item.pickupGround). #1666
-- has since given the ground-item half an owning-page read of its own,
-- item.getGroundForUnit(uid, gid), which is what unit_ai_pickup.lua's
-- own order now uses; craft.getBill still has no counterpart, and
-- reconciliation takes the engine's authoritative restored sets either
-- way rather than interrogating live pages at all. Asking
-- the active page about another page's id is exactly the wrong-entity
-- match World.Save.Integrity.luaEdgeResolves refuses to make, so the
-- engine hands the restored session's real sets to onSaveLoaded
-- instead (World.Save.Payload.LoadReconcileContext ->
-- Engine.Scripting.Lua.Thread.Dispatch's reconcileToScriptValue).

local M = {}

-- The reference schema and its one walk (issue #1589). Deliberately the
-- SCHEMA module, not unit_ai_save_refs.lua: the reconcile shares the
-- declaration, not that module's wire codec.
local schema    = require("scripts.unit_ai_ref_schema")
-- Safe at module scope: unit_ai_locations.lua requires nothing at
-- module scope itself (the same reason unit_ai_save_refs.lua requires
-- it directly).
local locations = require("scripts.unit_ai_locations")
-- #2055: the transient runtime fields a thought tick reads before it
-- has decided anything, and their fresh-unit values. Requires nothing
-- at module scope itself.
local defaults  = require("scripts.unit_ai_defaults")

-- Session-global reference kinds: one allocator for the whole session,
-- so the id alone identifies the entity. Mirrors luaEdgeResolves.
local GLOBAL_KINDS = { unit = true, building = true, item_instance = true }
-- Per-page kinds: the id means nothing without a page, and the page is
-- the OWNING unit's (never the active one, never session-wide).
local PER_PAGE_KINDS = { craft_bill = true, ground_item = true }

-- The engine always sends every one of these tables, empty or not (see
-- reconcileToScriptValue). A MISSING one is an engine fault, not "an
-- empty session" -- and silently treating it as empty would clear every
-- per-page reference in the save, while silently falling back to
-- active-page queries would resolve them against the wrong page. Both
-- are worse than failing, so a malformed context raises and the load
-- reports LoadReconciliationFailed (issue #1204's honest disposition
-- for a reconcile that did not complete).
local function validateContext(raw)
    if type(raw) ~= "table" then
        return "absent (expected a table, got " .. type(raw) .. ")"
    end
    if type(raw.item_instance) ~= "table" then
        return "missing or malformed 'item_instance' set"
    end
    if type(raw.unitPage) ~= "table" then
        return "missing or malformed 'unitPage' map"
    end
    if type(raw.byPage) ~= "table" then
        return "missing or malformed 'byPage' table"
    end
    for kind in pairs(PER_PAGE_KINDS) do
        if type(raw.byPage[kind]) ~= "table" then
            return "missing or malformed 'byPage." .. kind .. "' table"
        end
    end
    return nil
end

-- Does one declared edge still name a real entity in the restored
-- session? A kind this build does not know about resolves trivially
-- rather than being invented into a stale reference -- the same
-- decision luaEdgeResolves makes for the same reason (an unknown kind
-- is a registration-time vocabulary mismatch, caught elsewhere).
local function resolves(ctx, kind, id, owner)
    if GLOBAL_KINDS[kind] then
        return ctx[kind][id] == true
    end
    if PER_PAGE_KINDS[kind] then
        local page = owner ~= nil and ctx.unitPage[owner] or nil
        if page == nil then return false end
        local set = ctx.byPage[kind][page]
        return set ~= nil and set[id] == true
    end
    return true
end

-- Clear every reference in one surviving row that no longer names a
-- real entity, and return how many UNRESOLVED DECLARED EDGES were
-- removed. That count is deliberately per-edge, not per-field: two
-- dangling subfields of one job count twice (each was its own declared
-- edge), a duplicate stale gid in a loot list counts once per entry,
-- and a still-valid sibling removed only because its enclosing job was
-- dropped counts zero -- it was not itself unresolved. A present
-- holder missing its own id subfield counts once, the same as the
-- dangling case: it is one declared edge this reconcile removed.
--
-- Mutation is deferred until the whole walk has finished so a holder
-- can be judged on ALL of its edges before being dropped once.
function M.scrubStaleRefs(uid, s, ctx, hooks)
    local cleared = 0
    local dropHolders, staleIndices = {}, {}
    schema.forEach(s, function(row, value, path, index)
        if value == nil then
            -- A PRESENT nested holder whose id-bearing subfield is
            -- absent names nothing at all: it cannot be resolved, and
            -- the next thought tick would run its lock-state action
            -- against a nil target. That is unresolved, not "no edge
            -- here" -- the pre-#1589 scrub dropped exactly these
            -- (`liveUnitSet[nil]` is falsy), and `absentOk` marks the
            -- one pair where absence is a legitimate job phase.
            -- A "field"/"list" holder has no such distinction: an
            -- absent field is simply an absent field, and ipairs never
            -- yields a nil element.
            if row.holder ~= "table" or row.absentOk then return end
        elseif resolves(ctx, row.kind, value, uid) then
            return
        end
        cleared = cleared + 1
        if row.holder == "field" then
            s[row.field] = nil
        elseif row.holder == "table" then
            dropHolders[row.field] = row
        else
            local drops = staleIndices[row.field]
            if drops == nil then drops = { row = row }; staleIndices[row.field] = drops end
            drops[index] = true
        end
    end)

    -- A job whose required target is gone is dropped WHOLE: clearing
    -- one subfield would leave a malformed live job the next thought
    -- tick would act on. `drop` routes through the owning module's own
    -- release path (repairJob has an item to hand back and a claim to
    -- release); `also` clears the siblings that release path clears.
    for field, row in pairs(dropHolders) do
        if row.drop ~= nil then
            hooks[row.drop](uid, s, ctx)
        else
            s[field] = nil
            for _, extra in ipairs(row.also or {}) do s[extra] = nil end
        end
    end

    -- A collection is FILTERED, not dropped: the still-resolvable gids
    -- are real loot this unit is standing over. Surviving entries keep
    -- their original order as a dense array; a list that empties leaves
    -- the owning phase exactly where its own exhaustion path leaves it.
    for field, drops in pairs(staleIndices) do
        local kept = {}
        for i, v in ipairs(s[field]) do
            if not drops[i] then kept[#kept + 1] = v end
        end
        if #kept == 0 then
            for _, f in ipairs(drops.row.onEmpty) do s[f] = nil end
        else
            s[field] = kept
        end
    end
    return cleared
end

-- The module-owned release paths a whole-holder drop must go through,
-- keyed by the `drop` name unit_ai_ref_schema.lua's REF_SCHEMA declares.
-- Lives here rather than in unit_ai.lua so the reconcile that depends on
-- them can be driven -- and gated -- without booting the whole AI
-- orchestration module.
M.DROP_HOOKS = {
    -- A repair job past fetch_item has the target item sitting in this
    -- unit's own inventory, plus a repairClaims entry -- both handled by
    -- unit_ai_repair.lua's abort path, exactly as on any other abort
    -- (issue #1589 requirement 3), never by a bare field assignment.
    --
    -- The `info` argument decides whether that path may look for a
    -- technomule to hand the item back to, and it is deliberately
    -- WITHHELD for a unit that is not on the ACTIVE page: the mule
    -- search runs through unit.getAllIds(), which only ever lists the
    -- active page, so an off-page unit would be handing its item to a
    -- stranger on someone else's map. Passing nil takes abortRepairJob's
    -- existing no-mule branch, leaving the item in this unit's own
    -- inventory -- the same outcome as aborting anywhere no mule is in
    -- reach.
    --
    -- Required lazily: unit_ai_repair.lua reaches unit_ai_core.lua,
    -- which expects scripts.unit_ai to be self-registered already -- a
    -- bootstrap order only unit_ai.lua's own require chain guarantees,
    -- and one this module (requireable on its own) must not assume at
    -- module scope.
    repairJob = function(uid, s, ctx)
        local info = nil
        if ctx.activePage ~= nil and ctx.unitPage[uid] == ctx.activePage then
            info = unit.getInfo(uid)
        end
        require("scripts.unit_ai_repair").abort(uid, s, info)
    end,

    -- A construct job dropped because its STAKE no longer resolves
    -- (#1845): the queued spawn was captured by the save and discarded
    -- by the load, so nothing was built. The engine-side designation is
    -- still `claimed` by this unit, and a bare field assignment would
    -- leave it that way -- adopted as an orphan by the next scan and
    -- unavailable to anybody for a whole claim timeout, for a job that
    -- is already gone. Released to `pending` here instead, on the
    -- JOB's own page and for the exact attempt it observed, so the
    -- designation is reclaimable on the very next tick and a successor
    -- at the same tile is untouched.
    --
    -- Required lazily for the same bootstrap reason as repairJob above.
    constructJob = function(uid, s, ctx)
        local job = s.constructJob
        -- The unit's page comes from the RESTORED session's own map,
        -- not from a live `unit.getInfo`: a reconcile runs before any
        -- thought tick, and the actor may not be on the active page.
        -- With no page there is nothing safe to release -- a
        -- page-blind status write would hand a stranger's designation
        -- at the matching coordinate back to `pending`.
        local wid = job and ctx.unitPage[uid]
        if wid then
            require("scripts.unit_ai_construct")
                .abandonClaim(wid, job.x, job.y, job.attempt)
            construction.setJobStatus(wid, job.x, job.y, "pending",
                                      job.attempt)
        end
        s.constructJob = nil
        s.constructCandidate = nil
    end,
}

-- Build the resolution context handed to scrubStaleRefs from the two
-- survivor arrays onSaveLoaded already received and the engine-supplied
-- reconciliation payload. Unit/building sets come from the survivor
-- arrays rather than being re-sent, so there is only ever one statement
-- of which units and buildings exist.
--
-- `activePage` is carried for the drop hooks alone: an action module's
-- own cleanup may reach for active-page-only engine queries, which are
-- unsafe for a unit that lives on some other page.
function M.buildContext(survUnitSet, survBuildingSet, raw)
    local activePage = nil
    if world ~= nil and world.getActiveWorldId ~= nil then
        activePage = world.getActiveWorldId()
    end
    return { unit = survUnitSet, building = survBuildingSet,
             item_instance = raw.item_instance,
             unitPage = raw.unitPage, byPage = raw.byPage,
             activePage = activePage }
end

-- The whole post-load reconcile for aiState: the ORPHAN PRUNE (a row
-- whose unit did not survive is dropped rather than left for a later
-- id reuse to inherit), then the NESTED-REF SCRUB and the per-page
-- location-memory scrub on every surviving row. `aiState` is emptied
-- and refilled IN PLACE -- every module holds the same table.
-- Settle one restored unit's constructJob against the PUBLISHED
-- session's designations (#1844). Returns true when the job was dropped.
--
-- This runs here, in the post-publication broadcast, and not in the
-- component's own apply(): Lua components are applied while
-- worldManagerRef still names the OUTGOING session, so a designation
-- query there would answer about the world being replaced — adopting an
-- attempt from it, or dropping a job whose real designation exists only
-- in the staged one.
--
-- Both directions matter, so both are checked here:
--
--   * A pre-v8 job carries no attempt at all. It ADOPTS the attempt of
--     the designation standing at its page and tile, but only when that
--     designation is the same JOB — same category, and same pack/kind or
--     building def. A designation the player made there while the save
--     sat on disk is a different job, and steering a restored worker
--     onto it is exactly the confusion attempt identity exists to
--     prevent.
--   * A v8 job carries one, and it is VERIFIED. Load staging
--     self-clears designations whose art or build metadata has gone, so
--     a job naming one of those would otherwise stay live over nothing.
--
-- Anything that does not match exactly clears the job and nothing else.
-- It costs one re-scan on that unit's next tick.
function M.settleConstructJob(uid, s)
    local job = s.constructJob
    if not job then return false end
    if not (construction and construction.getDesignationAt) then
        return false
    end
    local wid = require("scripts.unit_ai_page").ofUnit(uid)
    local live = wid and construction.getDesignationAt(wid, job.x, job.y)
    local matches = live and live.category == job.category
        and (job.category ~= "structure"
             or (live.pack == job.pack and live.kind == job.kind))
        and (job.category ~= "building" or live.building == job.building)
        and (job.attempt == nil or live.attempt == job.attempt)
    if matches then
        job.attempt = live.attempt
        return false
    end
    s.constructJob = nil
    s.constructCandidate = nil
    return true
end

function M.reconcile(aiState, survUnitIds, survBuildingIds, raw, hooks)
    hooks = hooks or M.DROP_HOOKS
    local survUnitSet, survBuildingSet = {}, {}
    for _, uid in ipairs(survUnitIds or {})     do survUnitSet[uid] = true end
    for _, bid in ipairs(survBuildingIds or {}) do survBuildingSet[bid] = true end

    local problem = validateContext(raw)
    if problem == nil then
        for _, row in ipairs(schema.REF_SCHEMA) do
            if row.drop ~= nil and type(hooks[row.drop]) ~= "function" then
                problem = "no drop hook registered for '" .. row.drop .. "'"
                break
            end
        end
    end
    if problem ~= nil then
        local msg = "Unit AI: post-load reconciliation context " .. problem
            .. " -- refusing to reconcile aiState against active-page "
            .. "queries, which cannot resolve a per-page reference"
        engine.logError(msg)
        error(msg, 0)
    end

    local rebuilt = {}
    for uid in pairs(survUnitSet) do
        if aiState[uid] ~= nil then rebuilt[uid] = aiState[uid] end
    end
    local kept = 0
    for k in pairs(aiState) do aiState[k] = nil end
    for k, v in pairs(rebuilt) do aiState[k] = v; kept = kept + 1 end

    -- #2055: a row restored from an accepted schema version need not
    -- carry the transient runtime fields a thought tick reads before it
    -- has decided anything -- this component's validator accepts a
    -- free-form state row on purpose, and applyEntityRows installs each
    -- decoded row VERBATIM. Such a row survived decode, canonical
    -- comparison, resave, restart and reload and then errored on its
    -- first live tick at `engine.gameTime() < s.nextActionAt`; the
    -- tracked b3-lua-versioned-session-v1 fixture's v1 payload,
    -- `{[1] = {buildTarget = 1}}`, is exactly one.
    --
    -- HERE, and not at decode() or apply(), for three reasons that all
    -- have to hold at once:
    --
    --   * THE CLOCK IS RIGHT. `actionStartedAt` defaults to
    --     `engine.gameTime()`, and decode/apply run during STAGING --
    --     World.Load.Publish does not swap `gameTimeRef` to the save's
    --     own game time until afterwards, so a value stamped there
    --     would be the OUTGOING session's time (0 in a fresh process).
    --     A partially sparse `currentAction = "wander"` row would then
    --     have unit_ai_needs.lua's wanderUtility subtract that stale
    --     stamp from the restored clock and abandon the wander on a
    --     time it never spent -- and the wrong stamp would persist on
    --     the next save.
    --   * ROLLBACK STAYS VERBATIM. apply() is also applyAll's rollback
    --     entry point: an abandoned load re-applies each component's
    --     OWN pre-load snapshot, and that unwind must restore the old
    --     session untouched. This broadcast is post-PUBLICATION, so a
    --     rolled-back load never reaches it.
    --   * NOTHING TICKS FIRST. onSaveLoaded is the first point the Lua
    --     thread reaches after publish (luaTick drains luaQueue ahead
    --     of debug commands and script updates), so no unit_ai update
    --     can observe a row between the swap and this fill.
    --
    -- Version-independent by construction, too: every accepted
    -- inputVersion has already converged on live state by now, so this
    -- is one stage rather than a back-fill per migration branch. Fills
    -- only what is missing and overwrites nothing -- a save's own
    -- scheduling is the save's to state. Runs BEFORE the scrub below so
    -- every drop hook sees a well-formed row.
    local filled = defaults.normalizeAll(aiState)

    local ctx = M.buildContext(survUnitSet, survBuildingSet, raw)
    local scrubbed, forgotten, jobsDropped = 0, 0, 0
    for uid, s in pairs(aiState) do
        scrubbed = scrubbed + M.scrubStaleRefs(uid, s, ctx, hooks)
        -- #1844: a restored constructJob has to name a designation that
        -- is REALLY THERE, and name it exactly.
        if M.settleConstructJob(uid, s) then
            jobsDropped = jobsDropped + 1
        end
        -- #915: a location memory is scrubbed against the RESTORED
        -- session's own instance tables, not against the unit/building
        -- survivor sets -- its target is a (page, instance id) pair
        -- owned by the world-pages component, which those sets say
        -- nothing about. Counted separately for the same reason.
        forgotten = forgotten + locations.scrubStaleKnownLocations(uid, s)
    end
    engine.logInfo("Unit AI: reconciled AI state after load ("
        .. kept .. " kept, " .. filled .. " row(s) given runtime "
        .. "defaults, " .. scrubbed .. " stale ref(s) scrubbed, "
        .. forgotten .. " stale location memory/memories dropped, "
        .. jobsDropped .. " construct job(s) dropped)")
end

return M
