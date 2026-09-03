-- Persistent save-component registration for unit_ai (issue #761,
-- save-overhaul B3). Split out of scripts/unit_ai.lua to stay under
-- its line budget (#538, tools/lua_module_budget.py) -- this is
-- otherwise exactly what unit_ai.lua's init would register inline.

local M = {}

-- Reference-field schema, references() traversal, and typed structured-
-- reference wrap/unwrap (issue #764) live in unit_ai_save_refs.lua --
-- split out to stay under this file's line budget (#538). Since #1589
-- that module's REF_SCHEMA is the single declaration the reference
-- walk, the wire wrap/unwrap, the tag validator and the post-load
-- reconcile (scripts/unit_ai_reconcile.lua) are all derived from.
-- AI_UNIT_REF_FIELDS/AI_BUILDING_REF_FIELDS are re-exported unchanged
-- so any existing `unitAiSave.AI_UNIT_REF_FIELDS`/
-- `AI_BUILDING_REF_FIELDS` access keeps working exactly as before.
local refsMod = require("scripts.unit_ai_save_refs")
-- The payload validator is split out for the same reason and tested the
-- same way (#1737): this file's own line budget.
local validateUnitAiData = require("scripts.unit_ai_save_validate").validate
M.AI_UNIT_REF_FIELDS     = refsMod.AI_UNIT_REF_FIELDS
M.AI_BUILDING_REF_FIELDS = refsMod.AI_BUILDING_REF_FIELDS

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
local TRANSIENT_WORK_FIELDS =
    { "harvestProgress", "harvestProgressAt", "lastHarvestAt" }

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
    return copy
end

-- Register the "unit_ai" persistent save component. `aiState` is
-- scripts.unit_ai_core's shared per-unit state table, applied into IN
-- PLACE (issue #900) -- the orchestrator singleton itself is no longer
-- passed, since the only thing apply() ever wanted from it was somewhere
-- to stash the retired `_preLoadState` snapshot.
function M.register(aiState)
    -- Persistent save component (issue #761, save-overhaul B3): persist
    -- aiState (knownWaterSources, commandedTask, currentAction,
    -- source-drink phase, search-spiral progress, etc.). Without this,
    -- units load with empty AI state and lose their water memory + any
    -- in-flight player commands. Required: a missing/invalid unit_ai
    -- component aborts the whole load (requirement 6) rather than
    -- silently starting every unit with blank AI state.
    local saveMods = require("scripts.lib.save_modules")
    saveMods.register("unit_ai", {
        -- v2 (issue #764, save-overhaul C3): every reference field
        -- unitAiReferences declares is now a typed structured reference
        -- on the wire ({__ref=kind, id=N} via wrapUnitState above), not
        -- a bare number. v1 payloads migrate via decode() below.
        -- v3 (round-6 review, issue #764): each per-unit entry also
        -- carries a self-describing __owner = {__ref="unit", id=uid}
        -- field, typing the OUTER per-unit key the same way (there is
        -- no way to wrap a Lua table KEY itself -- see
        -- unit_ai_save_refs.lua's wrapUnitState haddock for why this is
        -- the closest Lua equivalent to psSim's SamePageRef-typed
        -- HashMap key).
        -- v4 (issue #915): per-unit location knowledge
        -- (aiState[uid].knownLocations, see
        -- scripts/unit_ai_locations.lua) -- a new durable,
        -- reference-bearing field, so a schema evolution rather than a
        -- silent addition. v1/v2/v3 payloads predate the field and
        -- decode with it ABSENT (a unit that never saw a location is
        -- exactly what "no memory" means); it is deliberately NOT
        -- back-filled from the player-wide discovery state, which is a
        -- different fact entirely (what the PLAYER has mapped, not what
        -- this acolyte has seen).
        -- v5 (issue #1291): a pending commandedTask/pickupOrder carries
        -- its stall budget as ACCUMULATED ELIGIBLE TIME
        -- (stalledFor/stallSeenAt, scripts/unit_ai_stall.lua) instead of
        -- the absolute progressAt origin v1-v4 wrote. A v1-v4 payload
        -- decodes with the new pair ABSENT and is seeded on its first
        -- tick from the absolute `progressAt` origin it does carry,
        -- which reproduces exactly the charge the old rule had accrued
        -- by then -- an order restored from an older save behaves as it
        -- did before this change, never as one that can no longer
        -- expire. Nothing is inferred here at decode time: the seeding
        -- needs the current game clock, which only the AI tick has.
        -- v6 (issue #1216): a unit that COMPLETED a player move order
        -- carries the position hold that order left behind
        -- (aiState[uid].holdAnchor, see scripts/unit_ai_hold.lua) --
        -- durable player intent, not scratch, so it survives a save
        -- the same way a pending commandedTask does. A v1-v5 payload
        -- predates the field and decodes with it ABSENT, which is
        -- exactly right: those sessions had no hold to record, and a
        -- hold is never INFERRED from an arrived-and-cleared order,
        -- because the payload cannot say whether the order that ended
        -- was the player's or scripts/building_spawn.lua's walk-out.
        -- The anchor carries no entity reference (two tile coordinates,
        -- this hold's own stall accounting, and its optional combat-
        -- withdrawal completion cutoff), so
        -- unit_ai_save_refs.lua's field walk, the typed-reference
        -- graph and the dangling/wrong-page rules are untouched.
        -- v7 (issue #1737): a repairJob sourced from the GROUND carries
        -- its provenance -- `fromGround`, plus the page-local
        -- `groundGid` until the pickup lands. Durable rather than
        -- derived, because a save can land anywhere between the pickup
        -- and the repair and only the job itself can then say that the
        -- instance in this worker's inventory owes a DROP on the ground
        -- rather than a hand-back to a technomule. A v1-v6 payload
        -- predates the whole rung and decodes with both fields ABSENT,
        -- which is exactly right and is the honest reading rather than
        -- a default: those sessions could only ever have sourced a
        -- repair target from held gear or a mule, so "not from the
        -- ground" is what their bytes actually mean. groundGid is a
        -- per-page ground-item id, so unit_ai_ref_schema.lua declares
        -- it as a typed `ground_item` edge and the post-load reconcile
        -- resolves it against the OWNING unit's page like every other
        -- one; an unresolvable gid drops the job through the same abort
        -- path any other stale reference does.
        -- v8 (issue #1844): a constructJob carries the exact
        -- construction-designation ATTEMPT it claimed
        -- (constructJob.attempt). Durable rather than derived: every
        -- lifecycle call the resumed job makes -- status, progress,
        -- payment, cancellation, completion -- names that attempt, and a
        -- job that came back naming only its TILE would happily pour
        -- work into, and complete, a successor designation a player made
        -- there while the save sat on disk. A v1-v7 payload predates the
        -- field, so its jobs are settled at the POST-PUBLICATION
        -- reconcile boundary (unit_ai_reconcile.lua's
        -- settleConstructJob): a legacy job adopts the attempt of the
        -- designation really standing at its page and tile, a v8 job has
        -- its own verified against it, and anything that does not match
        -- exactly is dropped -- never guessed, never left attempt-less.
        -- Not at apply() time: Lua components are applied while the
        -- OUTGOING session is still current, so a designation query
        -- there answers about the world being replaced.
        -- v9 (issue #1845): a constructJob carries the BUILDING it
        -- staked (constructJob.stakedBid), between the queued spawn and
        -- the completion that retires the job. Durable rather than
        -- derived: it is the only thing that tells a resumed job whether
        -- its OWN stake landed, rather than whether something that
        -- merely looks like it is standing at the tile -- and
        -- designation admission does not check occupancy, so a stranger
        -- really can be. A v1-v8 payload predates the field and decodes
        -- with it ABSENT, which is exactly right and is the honest
        -- reading rather than a default: at most one job per unit can be
        -- inside that window, the window is a tick or two wide, and a
        -- payload written without the field cannot say which job that
        -- was. Such a job resumes as not-yet-staked and re-stakes; the
        -- engine refuses a spawn onto an occupied tile, so it cannot
        -- double-build. unit_ai_ref_schema.lua declares the field as a
        -- typed `building` edge with its own drop path, so a v9 payload
        -- whose stake did not survive the load has that job released and
        -- its designation handed back to `pending`.
        version = 9,
        inputVersions = { 1, 2, 3, 4, 5, 6, 7, 8, 9 },
        required = true,
        scope = "global",
        -- Requirement 2 (round-8 review): unit_ai_save_refs.lua's
        -- unitAiReferences declares every reference KIND this component's
        -- data actually
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
        -- "location_instance" (knownLocations, #915) resolves against
        -- "world-pages", which owns each page's placed-location
        -- instance table (#911).
        deps = { "units", "buildings", "craft-bills", "world-activity",
                 "world-pages" },
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
            return refsMod.wrapAiState(live)
        end,
        decode = function(version, data)
            data = data or {}
            -- v1 payloads carry bare-number reference fields -- wrapping
            -- them here is the unambiguous v1->v2 migration (requirement
            -- 14): v1's fields have always meant exactly what
            -- unitAiReferences already declares, so there is nothing to
            -- guess. wrapUnitState (via wrapAiState) ALSO synthesizes
            -- __owner for v3, so a v1 payload migrates straight to v3 in
            -- one step. v2 payloads have every OTHER field already
            -- wrapped but no __owner yet -- addOwnerToAiState adds ONLY
            -- that, without re-wrapping fields that are already wrapped.
            -- v3 payloads are already complete (identity).
            --
            -- v3 -> v4 (#915) is identity too: the only v4 addition is
            -- knownLocations, and a v3 payload's ABSENCE of it is
            -- already the correct v4 value. Nothing infers it -- see
            -- the version field's own comment above for why
            -- back-filling from player-wide discovery would be wrong.
            --
            -- v4 -> v5 (#1291) is identity for the same reason: an
            -- older order's ABSENT stalledFor/stallSeenAt pair is the
            -- correct v5 value, and the honest seed for it is derived
            -- from the origin the payload already carries, on the first
            -- tick that has a clock to derive it against (see the
            -- version field's comment above).
            --
            -- v5 -> v6 (#1216) is identity again: an older payload's
            -- ABSENT holdAnchor is the correct v6 value -- that unit
            -- was not holding, and nothing in the payload could tell
            -- us otherwise without guessing (version field, above).
            --
            -- v6 -> v7 (#1737) is identity for the same reason: a
            -- payload written before the ground rung existed has no
            -- fromGround/groundGid, and their ABSENCE already means
            -- "this repair target did not come off the ground", which
            -- is the only thing those bytes could have meant.
            --
            -- v7 -> v8 (#1844) cannot be done here at all, and that is
            -- the point: settling a constructJob's attempt needs the
            -- PUBLISHED session's designations, which exist neither at
            -- decode time nor at apply() time. It happens in
            -- unit_ai_reconcile.lua, off the onSaveLoaded broadcast.
            --
            -- v8 -> v9 (#1845) is identity, for the same reason v3->v4
            -- through v6->v7 are: a payload written before
            -- constructJob.stakedBid existed has no such field, and its
            -- ABSENCE already means the only thing those bytes could
            -- mean -- "this job is not inside its staking window". A
            -- default would be a guess about which job was, and the
            -- payload cannot say. The resumed job re-stakes, and the
            -- engine's own occupancy refusal is what stops that
            -- double-building (version field, above).
            --
            -- #2055's runtime-default normalization is deliberately NOT
            -- here either. One of the three defaults reads the CLOCK,
            -- and decode runs during staging -- `gameTimeRef` is not
            -- swapped to the save's own game time until
            -- World.Load.Publish, so a value stamped here would be the
            -- OUTGOING session's time (0 in a fresh process), not the
            -- restored one. It runs at the post-publish reconcile
            -- instead: scripts/unit_ai_reconcile.lua.
            if version == 1 then return refsMod.wrapAiState(data) end
            if version == 2 then return refsMod.addOwnerToAiState(data) end
            return data
        end,
        validate = validateUnitAiData,
        references = refsMod.references,
        -- Per-entity application (issue #900). Each per-unit row is
        -- applied against the restored session's own unit set rather
        -- than the singleton being replaced wholesale: a row whose unit
        -- is absent is dropped with a diagnostic, its siblings apply
        -- normally, and aiState ends up holding EXACTLY the applicable
        -- rows (see applyEntityRows for why "exactly" matters against
        -- session-global id reuse). `entities` is nil for a contextless
        -- apply -- including applyAll's own rollback pass, which must
        -- restore the OLD session's rows verbatim.
        --
        -- unwrapAiState turns every reference field back into a bare
        -- number first, so aiState's LIVE in-memory shape (read by every
        -- OTHER module) never changes -- only the bytes on disk do. The
        -- table is mutated in place: consumers hold direct references to
        -- it and rebinding would orphan every one of them.
        --
        -- #2055's runtime-default normalization is deliberately NOT
        -- here: apply() is also the ROLLBACK entry point (applyAll
        -- hands it the OLD session's own snapshot, contextless, and
        -- that unwind is required to be verbatim), so normalizing at
        -- this boundary would edit pre-load state during a load that
        -- is being abandoned. It runs at the post-publish reconcile
        -- instead -- see scripts/unit_ai_reconcile.lua.
        apply = function(data, entities)
            saveMods.applyEntityRows(aiState, refsMod.unwrapAiState(data),
                entities, { kind = "unit", component = "unit_ai" })
        end,
    })

    -- The unit-AI family's TRANSIENT coordination tables (#1329): the
    -- five coordinate claim registries, repairClaims/repairPriority, and
    -- #916's three same-tick encounter overlays. None lives in aiState or
    -- is persisted. registerResetHook fires unconditionally on every
    -- load -- including a load whose envelope carries no data for this
    -- module family at all -- which is exactly the contract these need:
    -- a load REPLACES the session, both id allocators rewind, and the
    -- loaded clock can be EARLIER than the session that wrote a claim,
    -- so the `now - c.at > timeout` expiry would not fire until game
    -- time caught up. onSaveLoaded is the wrong hook for the same job:
    -- it only reconciles aiState, and it is a post-publication
    -- broadcast, so a load rejected before publication must leave the
    -- live session's claims intact -- which it does, because applyAll
    -- runs reset hooks only after every component has committed.
    --
    -- Its id is NOT "unit_ai": that belongs to the persistent component
    -- registered above, and saveModules refuses a reset-hook/component
    -- collision (`duplicate id`).
    local claimsLib = require("scripts.unit_ai_claims")
    saveMods.registerResetHook("unit_ai_claims", function()
        local dropped = claimsLib.resetAll()
        if dropped > 0 then
            engine.logInfo("Unit AI: cleared " .. dropped
                .. " transient claim/priority entries on load")
        end
    end)

    -- aiState across the OTHER session-replacement path (#1610). Exit to
    -- Menu destroys every world and resets the entity managers without
    -- ever reaching saveModules, so neither the reset hook above nor
    -- unitAi.onSaveLoaded fires -- and aiState kept one row per unit for
    -- the life of the process, growing across every
    -- Exit-to-Menu -> New Game cycle.
    --
    -- Registered HERE, beside the component, because a session boundary
    -- is registry wiring and that is this module's job -- the same
    -- reason the reset hook above lives here rather than in unit_ai.lua.
    -- The clear empties aiState IN PLACE for the same reason
    -- shutdown/onSaveLoaded do: the table is published on the
    -- package.loaded singleton and held directly by unit_ai_core and
    -- every submodule.
    local teardown = require("scripts.lib.session_teardown")
    teardown.register("unit_ai", function()
        local n = 0
        for k in pairs(aiState) do aiState[k] = nil; n = n + 1 end
        local transient = claimsLib.resetAll()
        engine.logInfo("Unit AI: cleared " .. n
            .. " AI state row(s) and " .. transient
            .. " transient coordination entries on session teardown")
    end)
end

return M
