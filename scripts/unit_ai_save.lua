-- Persistent save-component registration for unit_ai (issue #761,
-- save-overhaul B3). Split out of scripts/unit_ai.lua to stay under
-- its line budget (#538, tools/lua_module_budget.py) -- this is
-- otherwise exactly what unit_ai.lua's init would register inline.

local M = {}

-- Reference-field schema, references() traversal, and typed structured-
-- reference wrap/unwrap (issue #764) live in unit_ai_save_refs.lua --
-- split out to stay under this file's line budget (#538). AI_UNIT_REF_FIELDS/
-- AI_BUILDING_REF_FIELDS are re-exported unchanged so unit_ai.lua's
-- existing `unitAiSave.AI_UNIT_REF_FIELDS`/`AI_BUILDING_REF_FIELDS`
-- access (scrubStaleRefs) keeps working exactly as before.
local refsMod = require("scripts.unit_ai_save_refs")
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
    for _, f in ipairs(TRANSIENT_ORDER_FIELDS) do copy[f] = nil end
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
        -- The anchor carries no entity reference (two tile
        -- coordinates plus this hold's own stall accounting), so
        -- unit_ai_save_refs.lua's field walk, the typed-reference
        -- graph and the dangling/wrong-page rules are untouched.
        version = 6,
        inputVersions = { 1, 2, 3, 4, 5, 6 },
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
        apply = function(data, entities)
            saveMods.applyEntityRows(aiState, refsMod.unwrapAiState(data),
                entities, { kind = "unit", component = "unit_ai" })
        end,
    })

    -- The unit-AI family's TRANSIENT coordination tables (#1329): the
    -- five coordinate claim registries plus repairClaims and
    -- repairPriority, none of which lives in aiState and none of which
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
end

return M
