-- What a unit_ai snapshot DROPS, and why (#2553 split from
-- scripts/unit_ai_save.lua).
--
-- The third split out of that file for the reason the first two
-- (unit_ai_save_refs.lua, unit_ai_save_validate.lua) name in their own
-- headers: its 500-line budget (#538, tools/lua_module_budget.py).
-- The boundary is cohesive rather than convenient -- everything here
-- answers one question about one function, namely which per-unit
-- aiState fields never reach the wire and on what grounds, and nothing
-- here participates in versioning, encoding or validation.
--
-- snapshotUnitState is the single consumer. It is deliberately NOT
-- re-exported from unit_ai_save.lua: the component's own snapshot()
-- calls it through this module.

local M = {}

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
    -- #1247: the transfer order table read out of the engine store on
    -- THIS tick and consumed by transferExecute on the same one. Same
    -- rule as every candidate above -- and the same specific hazard as
    -- craftCandidate/repairCandidate, since it embeds a live engine
    -- projection (the counterpart's current placement) that requirement
    -- 14 forbids persisting as a copy.
    "transferCandidate",
}

-- #1247: trip bookkeeping for an in-flight transfer order -- its stall
-- budget and the closest approach so far -- keyed by the order id it
-- belongs to. Stripped for a reason the *Candidate fields do not share,
-- so it gets its own list rather than a misleading name in theirs:
--
--   * #1246's order store is AUTHORITATIVE for order state and is
--     persisted by its own engine-side component, so nothing about the
--     order itself needs mirroring here. Persisting this would durably
--     carry a TransferOrderId as a bare number -- a reference kind
--     unit_ai_save_refs.lua does not declare and the integrity graph
--     could not check.
--   * What is left is re-derivable and SHOULD restart: the first tick
--     after a load re-reads the store, re-establishes the closest
--     approach from where the carrier actually stands, and gives the
--     order its full budget again. That is the same answer
--     MAX_CHARGED_INTERVAL already gives a load boundary
--     (unit_ai_stall.lua) -- an interval the AI could not tick through
--     charges a pending order nothing, however long it lasted.
local TRANSIENT_ORDER_FIELDS = { "transferOrder" }

-- #1582: auto-harvest's picking accumulator and its work clock. Its own
-- list for a third distinct reason -- restarting the pick is the HONEST
-- post-load state, not a loss. The three-part why is stated where the
-- fields are owned, in scripts/unit_ai_harvest.lua's TRANSIENCE note.
--
-- #2550 adds the two retained-yield collection approach records
-- (scripts/unit_ai_yield.lua) for the same reason TRANSIENT_ORDER_FIELDS
-- drops a restored transfer order's accounting: the closest approach is
-- re-established from where the worker actually stands on the first
-- tick after a load, and an interval the AI could not tick through
-- charges a pending approach nothing. The pending collection ITSELF
-- (harvestPhase/harvestLoot, foragePhase/forageLoot) is durable and
-- unaffected -- only its budget restarts.
--
-- #2545 adds the source-drink phase's deadline for the reason
-- constructJob.staking is stripped below: it is a CLOCK, and a wait
-- cannot outlive the session whose clock it was measured against. The
-- phase ITSELF (sourcePhase) is durable and stays -- a unit saved on
-- all fours at a bank resumes drinking there -- and
-- unit_ai_source_phase.expire re-arms a full budget on the first tick
-- after a load, which is the honest answer for an interval the AI
-- could not tick through.
local TRANSIENT_WORK_FIELDS =
    { "harvestProgress", "harvestProgressAt", "lastHarvestAt",
      "harvestCollect", "forageCollect", "sourcePhaseAt" }

-- A shallow copy of one unit's aiState entry with every transient
-- candidate field stripped (requirement 13/14) -- see
-- TRANSIENT_CANDIDATE_FIELDS, TRANSIENT_ORDER_FIELDS and
-- TRANSIENT_WORK_FIELDS, which strip for three different reasons.
-- Nested tables that DO get persisted (craftJob, treatClaim, ...) are
-- shared by reference with the live state, which is safe: the snapshot
-- is encoded (deep-copied into a byte string) before this tick's AI
-- loop could mutate them again.
local function snapshotUnitState(s)
    local copy = {}
    for k, v in pairs(s) do copy[k] = v end
    for _, f in ipairs(TRANSIENT_CANDIDATE_FIELDS) do copy[f] = nil end
    for _, f in ipairs(TRANSIENT_ORDER_FIELDS) do copy[f] = nil end
    for _, f in ipairs(TRANSIENT_WORK_FIELDS) do copy[f] = nil end
    -- constructJob (round-5 review) retains the full parsed structure-
    -- pack build-cost table (unit_ai_construct.lua's packBuildInfo:
    -- materials/build_work) rather than a stable id, which requirement
    -- 14 forbids persisting as a copy. Unlike the *Candidate fields
    -- above, constructJob is a multi-tick DURABLE job (can't just be
    -- dropped and re-derived next tick), so only its .build sub-field is
    -- stripped, on a shallow copy of the job table itself: constructJob
    -- is a reference SHARED with the live aiState entry, so mutating it
    -- in place here would corrupt the live job the AI is still working.
    --
    -- Nothing needs to re-populate it after a load. It is a FETCH PLAN
    -- and nothing else: a resumed job re-reads the registered cost
    -- through packBuildInfo when it next needs one, and since #1844 the
    -- REFUND does not consult it at all -- that comes from the
    -- designation's own durable receipt, which is engine-side state this
    -- payload never carried.
    --
    -- constructJob.staking (#1845) is stripped on the same shallow copy
    -- and for a different reason: it is the CLOCK a building stake's
    -- visibility wait is bounded by, and a wait cannot outlive the
    -- session whose building queue it was waiting on -- the load
    -- discards that queue, so on the other side either the building is
    -- standing there or it never will be.
    --
    -- The spawned building's id BESIDE it is deliberately NOT stripped.
    -- It is the only thing that tells a resumed job whether its OWN
    -- stake landed, rather than whether something that merely looks like
    -- it is standing at the tile -- and designation admission does not
    -- check occupancy, so a stranger really can be. It is safe to carry
    -- because unit_ai_ref_schema.lua DECLARES it
    -- (constructJob.stakedBid, kind "building", absentOk): wrapped on
    -- the wire, checked by the integrity graph, and reconciled on load,
    -- where a stake that never landed dangles and the whole job is
    -- dropped so its designation goes back to the pool. A bare id with
    -- no declared kind is the hazard the chopJob.iid note below records;
    -- a declared one is the answer to it, and any future change here
    -- must keep it declared rather than reach for the world again.
    -- See unit_ai_construct_site.stakedBuildingAt.
    if copy.constructJob
       and (copy.constructJob.build ~= nil
            or copy.constructJob.staking ~= nil) then
        local jobCopy = {}
        for jk, jv in pairs(copy.constructJob) do jobCopy[jk] = jv end
        jobCopy.build = nil
        jobCopy.staking = nil
        copy.constructJob = jobCopy
    end
    -- chopJob.iid (#1854) is stripped on exactly the constructJob.build
    -- pattern above, and for the same class of reason: it is a durable
    -- FloraInstanceId, and persisting it as a bare number would carry a
    -- reference kind unit_ai_save_refs.lua does not declare and the
    -- integrity graph could not check -- the same hazard the
    -- TransientOrderFields note records for TransferOrderId. The chop
    -- DESIGNATION is the durable authority and is persisted engine-side
    -- (world-activity), so unit_ai_chop.lua's jobInstance() re-resolves
    -- the target from the job's own saved tile on the first tick after
    -- a load. The job itself (tile, phase, progress) still persists
    -- exactly as before, so lua.unit_ai's schema is untouched by the
    -- #1854 re-key -- no version bump, no new reference kind.
    --
    -- Re-resolution is CLAIM-AWARE, and it has to be: a tile can carry
    -- several designated plants, so two acolytes can restore jobs on
    -- one. jobInstance() walks chop.getDesignationsAt's deterministic
    -- list and adopts (claiming in the same step) the first plant no
    -- other acolyte holds, and chopExecute refuses to refresh a claim
    -- that is not its own -- without both, the pair would fell one tree
    -- together, orphan the other's designation, and silently overwrite
    -- the loser's claim. Pinned by Test.Headless.Lua.UnitAiLoadReset's
    -- two restored-chop-job examples.
    if copy.chopJob and copy.chopJob.iid ~= nil then
        local jobCopy = {}
        for jk, jv in pairs(copy.chopJob) do jobCopy[jk] = jv end
        jobCopy.iid = nil
        copy.chopJob = jobCopy
    end
    -- The two FOOD targets' selected plant (#2553), stripped on exactly
    -- the chopJob.iid pattern above and for exactly its reason: a
    -- FloraInstanceId persisted as a bare number would carry a
    -- reference kind unit_ai_save_refs.lua does not declare and the
    -- integrity graph could not check. Both holders would otherwise
    -- reach the wire -- forageTarget is a declared reference row (its
    -- GROUND-kind gid), harvestTarget is durable and named in
    -- unit_ai_harvest.lua's TRANSIENCE note as one of the fields a load
    -- deliberately keeps -- so neither is covered by any list above.
    --
    -- Nothing has to re-populate them, and no new reference kind and no
    -- schema version are spent: both callers re-run their food search
    -- on every arbitration tick and OVERWRITE the whole target, so the
    -- first tick after a load re-selects a plant that really is there
    -- and re-attaches its live id. Until it does, the restored target
    -- names a tile and no plant, which unit_ai_forage_pick.pick REFUSES
    -- rather than resolving by coordinate -- an id-less wild target is
    -- exactly the state this strip creates, and picking by coordinate
    -- there is the defect #2553 exists to prevent.
    --
    -- Only the id is dropped. The target itself, the collecting phase
    -- and its loot list are untouched, so a save taken mid-collection
    -- still resumes the pickup it was on.
    for _, f in ipairs({ "forageTarget", "harvestTarget" }) do
        local tgt = copy[f]
        if tgt ~= nil and tgt.iid ~= nil then
            local tgtCopy = {}
            for tk, tv in pairs(tgt) do tgtCopy[tk] = tv end
            tgtCopy.iid = nil
            copy[f] = tgtCopy
        end
    end
    return copy
end

M.snapshotUnitState = snapshotUnitState

return M
