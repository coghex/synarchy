-- Transfer session (#1014, epic #1013 phase B1; generalized to named
-- endpoints by #1085 phase A2): the shared entry point right-click
-- "Transfer" opens.
--
-- NOT an A2 (#1085) transfer order: a real order needs concrete item
-- instances, which only C1's later item selection provides. This module
-- is that order's IDENTITY -- the source endpoint, the destination
-- endpoint, and the lifecycle vocabulary the eventual request will use
-- -- recorded the moment the player commits to a transfer, well before
-- an item is chosen. It is TRANSIENT by design (never registered as a
-- save_modules component, matching A1/A2): 'M.init' registers a reset
-- hook so a stale session pointing at a dead endpoint can never survive
-- a LOAD. A reset hook only fires from saveModules.applyAll(), never
-- from an ordinary "Exit to Menu" -> world.destroyAll() -> fresh
-- world.init() -- that path clears M.active explicitly in
-- scripts/pause_menu.lua's onExitToMenu, the same place
-- build_tool/mine_tool clear their own transient armed state for the
-- identical reason.
--
-- Reusable on purpose (requirement 8): 'M.create' is the ONE place a
-- session gets built. scripts/init_context_menu.lua's "Transfer"
-- callback calls it below; a future drag-and-drop surface calls the
-- exact same function rather than duplicating this validation.

local M = package.loaded["scripts.transfer_session"] or {}
package.loaded["scripts.transfer_session"] = M

local nextSessionId = 1

-- The contract's reason vocabulary (Unit.Transfer.transferReasonId).
-- These three literals name WHICH of B1's own creation-time failure
-- branches fired (there is no other way to ask the contract "give me
-- the id meaning source-missing" -- 'reasons' is one flat, unordered
-- array covering every per-item reason, so a caller has to know which
-- string it means); 'resolveReason' below is what verifies each one
-- against the LIVE contract before ever handing it back, so B1 never
-- reports an id unit.checkTransfer/commitTransfer wouldn't also
-- recognise (#1014 review round 2). 'became_stale' is the vocabulary's
-- OWN name for an item that passed at create time and broke before
-- commit, which is C2's concern, not B1's. 'contract_unavailable' is
-- NOT part of the engine's vocabulary -- it names a distinct B1-internal
-- failure class (the live contract itself came back malformed/missing
-- what B1 needs), never a transfer policy refusal.
local REASON_SOURCE_MISSING       = "source_missing"
local REASON_RECEIVER_MISSING     = "receiver_missing"
local REASON_RECEIVER_INELIGIBLE  = "receiver_ineligible"
local REASON_CONTRACT_UNAVAILABLE = "contract_unavailable"

-- This session's initial lifecycle state (Unit.Transfer.
-- transferStateId's 'TransferQueued'), validated by MEMBERSHIP against
-- the live contract, never by array position.
--
-- #1014 review round 1 replaced a hardcoded "queued" literal with a
-- positional operations[1]/states[1] read, because back then the
-- contract was three flat arrays with no per-kind tagging and position
-- was the only way to name a value. A2 (#1085) removed the operation
-- concept entirely (direction is derived from the endpoint pair) and
-- publishes 'endpointKinds' as a NAMED SET, so there is no longer an
-- array position that means anything here. What survives of that review
-- is the real rule -- never report an identifier the live contract
-- doesn't advertise -- and 'resolveState'/'resolveReason'/
-- 'resolveEndpointKind' below all enforce it by membership, which does
-- not care how the engine orders its enums.
local STATE_QUEUED = "queued"

-- The endpoint kinds this module names. B1's gesture always makes the
-- source a unit; the destination kind is whatever the caller targeted.
local KIND_UNIT = "unit"
local KIND_BUILDING = "building"

local function containsValue(list, v)
    for _, x in ipairs(list or {}) do
        if x == v then return true end
    end
    return false
end

-- Confirm `id` is actually one of the live contract's own reason ids
-- before ever returning it -- refuses to hand back a string the
-- contract doesn't advertise. Returns nil on drift/unavailability;
-- every 'M.create' call site below then reports
-- REASON_CONTRACT_UNAVAILABLE instead of the unverified id.
local function resolveReason(id)
    local c = unit.transferContract()
    local reasons = c and c.reasons
    if reasons and containsValue(reasons, id) then return id end
    return nil
end

-- The same discipline for a lifecycle state id.
local function resolveState(id)
    local c = unit.transferContract()
    local states = c and c.states
    if states and containsValue(states, id) then return id end
    return nil
end

-- ...and for an endpoint kind, read from the contract's NAMED set
-- (endpointKinds.unit / endpointKinds.building) rather than a
-- positional array. Returns nil for a kind the engine doesn't
-- advertise, which 'M.create' treats as a hard failure rather than
-- falling back to a guessed string.
local function resolveEndpointKind(kind)
    local c = unit.transferContract()
    local kinds = c and c.endpointKinds
    if kinds and kinds[kind] == true then return kind end
    return nil
end

-- One-time boot-time warning (developer-facing log noise, not a
-- gameplay gate -- the resolve* helpers are what actually enforce this
-- on every real call) that every id this module names is present in the
-- live contract, so drift is visible in the log the moment the engine
-- starts rather than only the first time a player happens to hit an
-- affected branch.
local function checkVocabulary()
    for _, id in ipairs({ REASON_SOURCE_MISSING, REASON_RECEIVER_MISSING,
                           REASON_RECEIVER_INELIGIBLE }) do
        if not resolveReason(id) then
            engine.logWarn("transfer_session: reason id '" .. id
                .. "' missing from unit.transferContract() -- drifted "
                .. "from Unit.Transfer's vocabulary")
        end
    end
    if not resolveState(STATE_QUEUED) then
        engine.logWarn("transfer_session: state id '" .. STATE_QUEUED
            .. "' missing from unit.transferContract() -- drifted "
            .. "from Unit.Transfer's vocabulary")
    end
    for _, kind in ipairs({ KIND_UNIT, KIND_BUILDING }) do
        if not resolveEndpointKind(kind) then
            engine.logWarn("transfer_session: endpoint kind '" .. kind
                .. "' missing from unit.transferContract() -- drifted "
                .. "from Unit.Transfer's vocabulary")
        end
    end
end

function M.init(scriptId)
    local saveMods = require("scripts.lib.save_modules")
    saveMods.registerResetHook("transfer_session", function()
        M.active = nil
    end)
    checkVocabulary()
end

-- The B1-owned "valid source" rule (#1014 review, requirements 4 + 7):
-- exactly one selected unit, player-commandable (the same
-- faction.isPlayerCommandable gate the "Attack" entry already uses --
-- and, since A2, the same gate the engine's own unit-endpoint
-- eligibility applies), and not the destination itself (the
-- self-transfer case the contract refuses at request time). Zero
-- selected units and MULTIPLE selected units both fail this the same
-- way, which is deliberate -- see the context-menu wiring for why
-- "Transfer" is OMITTED rather than disabled for a multi-unit
-- selection.
function M.resolveSource(selectedUids, excludeUid)
    if not selectedUids or #selectedUids ~= 1 then return nil end
    local uid = selectedUids[1]
    if excludeUid ~= nil and uid == excludeUid then return nil end
    if not faction.isPlayerCommandable(unit.getFaction(uid)) then
        return nil
    end
    return uid
end

-- Create (or replace) the active transfer session for `sourceUid` ->
-- `(kind, destinationId)`. Re-validates BOTH sides against fresh live
-- state (never the menu-build-time snapshot the caller may have used to
-- decide whether to show "Transfer" at all), so a destination destroyed
-- or made ineligible between right-click and the player's actual click
-- is caught here as a stale target (requirement 9) instead of producing
-- a session that points at nothing.
--
-- Returns the session table on success, or (nil, reasonId) on failure.
-- A failure emits a player-visible "unit_warning" event and mutates
-- NEITHER endpoint -- B1 never moves an item; that stays C2's job once
-- C1 has picked one.
function M.create(sourceUid, kind, destinationId)
    if not sourceUid or not unit.exists(sourceUid) then
        engine.emitEvent("unit_warning", "Cannot transfer: source unit not found")
        return nil, resolveReason(REASON_SOURCE_MISSING) or REASON_CONTRACT_UNAVAILABLE
    end

    local info = unit.transferEndpointInfo({ kind = kind, id = destinationId })
    if not info then
        engine.emitEventForUnit("unit_warning",
            "Cannot transfer: target no longer exists", sourceUid)
        return nil, resolveReason(REASON_RECEIVER_MISSING) or REASON_CONTRACT_UNAVAILABLE
    end
    if not info.eligible then
        local name = (info.displayName ~= "" and info.displayName) or "Target"
        engine.emitEventForUnit("unit_warning",
            "Cannot transfer: " .. name .. " cannot receive items", sourceUid)
        return nil, resolveReason(REASON_RECEIVER_INELIGIBLE) or REASON_CONTRACT_UNAVAILABLE
    end

    -- BOTH endpoint kinds are resolved by membership, not just the
    -- destination: B1's gesture always makes the source a unit, but
    -- "always a unit" is still an id this module names, and naming an
    -- id the live contract does not advertise is exactly what the
    -- membership rule exists to prevent. A contract that dropped
    -- 'unit' would otherwise still mint a session whose source kind no
    -- engine verb recognises.
    local sourceKind = resolveEndpointKind(KIND_UNIT)
    local destinationKind = resolveEndpointKind(kind)
    local state = resolveState(STATE_QUEUED)
    if not (sourceKind and destinationKind and state) then
        engine.emitEventForUnit("unit_warning",
            "Cannot transfer: internal contract error", sourceUid)
        return nil, REASON_CONTRACT_UNAVAILABLE
    end

    local srcInfo = unit.getInfo(sourceUid)

    local id = nextSessionId
    nextSessionId = nextSessionId + 1

    M.active = {
        id                     = id,
        -- Named endpoints on BOTH sides (#1085 requirement 11): the
        -- contract no longer carries an independent operation value,
        -- because the direction IS the pair. A source endpoint is
        -- always a unit for B1's gesture; C1/C3 widen which gestures
        -- can produce one, not what a session records.
        source                 = { kind = sourceKind, id = sourceUid },
        destination            = { kind = destinationKind,
                                    id   = destinationId },
        destinationDisplayName = info.displayName,
        -- Same page on both sides by construction: the contract refuses
        -- any cross-page request outright (Unit.Transfer's own page
        -- check), so the destination's resolved page IS the source's
        -- page for any real player interaction (hit-testing only ever
        -- matches entities on the currently visible world).
        sourceLocation         = { page  = info.page,
                                    gridX = srcInfo and srcInfo.gridX,
                                    gridY = srcInfo and srcInfo.gridY },
        destinationLocation    = { page = info.page, gridX = info.gridX,
                                    gridY = info.gridY },
        -- This session's OWN lifecycle state -- the contract's first
        -- vocabulary value (resolved by membership above, not a
        -- hardcoded literal and not an array position), meaning "the
        -- player has committed to this transfer", NOT a real queued
        -- transfer order (see module header). C2/C3 own advancing it
        -- past here.
        contract               = { state = state },
    }
    return M.active
end

-- The current session, or nil. C1's paired inventory view reads
-- through this rather than the context-menu module.
function M.get()
    return M.active
end

-- Explicit teardown (also run unconditionally on world load via the
-- reset hook above). C3 owns player-facing cancellation; this is the
-- mechanical clear underneath it.
function M.clear()
    M.active = nil
end

return M
