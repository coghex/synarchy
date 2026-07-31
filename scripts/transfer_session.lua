-- Transfer session (#1014, epic #1013 phase B1): the shared entry
-- point right-click "Transfer" opens.
--
-- NOT an A1 (#1000) QueuedTransfer: 'Unit.Transfer's own header notes
-- A1 ships no producer for one, because a real QueuedTransfer needs a
-- concrete item instance, which only C1's later item selection
-- provides. This module is that state machine's first producer's
-- IDENTITY — source unit, receiver, and the A1 operation vocabulary
-- the eventual request will use — recorded the moment the player
-- commits to a transfer, well before an item is chosen. It is
-- TRANSIENT by design (never registered as a save_modules component,
-- matching A1): 'M.init' registers a reset hook so a stale session
-- pointing at a dead receiver can never survive a LOAD. A reset hook
-- only fires from saveModules.applyAll(), never from an ordinary
-- "Exit to Menu" -> world.destroyAll() -> fresh world.init() -- that
-- path clears M.active explicitly in scripts/pause_menu.lua's
-- onExitToMenu, the same place build_tool/mine_tool clear their own
-- transient armed state for the identical reason.
--
-- Reusable on purpose (requirement 8): 'M.create' is the ONE place a
-- session gets built. scripts/init_context_menu.lua's "Transfer"
-- callback calls it below; a future drag-and-drop surface calls the
-- exact same function rather than duplicating this validation.

local M = package.loaded["scripts.transfer_session"] or {}
package.loaded["scripts.transfer_session"] = M

local nextSessionId = 1

-- A1's reason vocabulary (Unit.Transfer.transferReasonId). These three
-- literals name WHICH of B1's own creation-time failure branches fired
-- (there is no other way to ask the contract "give me the id meaning
-- source-missing" — 'reasons' is one flat, unordered array covering
-- all eleven A1 reasons, so a caller has to know which string it
-- means); 'resolveReason' below is what verifies each one against the
-- LIVE contract before ever handing it back, so B1 never reports an id
-- unit.checkTransfer/commitTransfer wouldn't also recognise (#1014
-- review round 2). 'became_stale' is the vocabulary's OWN name for a
-- request that passed at create time and broke before commit, which
-- is C2's concern, not B1's. 'contract_unavailable' is NOT part of
-- A1's vocabulary — it names a distinct B1-internal failure class (the
-- live contract itself came back malformed/missing what B1 needs),
-- never a transfer policy refusal.
local REASON_SOURCE_MISSING       = "source_missing"
local REASON_RECEIVER_MISSING     = "receiver_missing"
local REASON_RECEIVER_INELIGIBLE  = "receiver_ineligible"
local REASON_CONTRACT_UNAVAILABLE = "contract_unavailable"

local function containsValue(list, v)
    for _, x in ipairs(list or {}) do
        if x == v then return true end
    end
    return false
end

-- Confirm `id` is actually one of the live contract's own reason ids
-- before ever returning it — refuses to hand back a string the
-- contract doesn't advertise (same "never assume, always confirm
-- live" discipline as 'resolveContractIdentity' below). Returns nil on
-- drift/unavailability; every 'M.create' call site below then reports
-- REASON_CONTRACT_UNAVAILABLE instead of the unverified id.
local function resolveReason(id)
    local c = unit.transferContract()
    local reasons = c and c.reasons
    if reasons and containsValue(reasons, id) then return id end
    return nil
end

-- One-time boot-time warning (developer-facing log noise, not a
-- gameplay gate — 'resolveReason'/'resolveContractIdentity' are what
-- actually enforce this on every real call) that every id this module
-- names is present in the live A1 contract, so drift is visible in the
-- log the moment the engine starts rather than only the first time a
-- player happens to hit an affected branch.
local function checkVocabulary()
    for _, id in ipairs({ REASON_SOURCE_MISSING, REASON_RECEIVER_MISSING,
                           REASON_RECEIVER_INELIGIBLE }) do
        if not resolveReason(id) then
            engine.logWarn("transfer_session: reason id '" .. id
                .. "' missing from unit.transferContract() -- drifted "
                .. "from Unit.Transfer's vocabulary")
        end
    end
end

-- The A1 operation id for `kind`, and this session's initial
-- ('queued') state id — RESOLVED from the live unit.transferContract()
-- every call, never a hardcoded string table (#1014 review round 1: a
-- local OPERATION_BY_KIND/"queued" literal could silently diverge from
-- Unit.Transfer.transferOperationId/transferStateId with nothing
-- catching it). Positional, because the contract is three FLAT arrays
-- with no per-kind tagging of its own: Unit.Transfer.
-- allTransferOperations is literally [ToBuildingStorage,
-- ToUnitInventory] and allTransferStateIds starts with TransferQueued,
-- so operations[1]/states[1] are exactly the values
-- transferOperationId/transferStateId would produce for those
-- constructors — read live here rather than assumed. Returns (nil,
-- nil) if the live contract doesn't have what's needed, which
-- 'M.create' treats as a hard failure rather than falling back to a
-- guessed string.
local function resolveContractIdentity(kind)
    local c = unit.transferContract()
    local ops = c and c.operations
    local states = c and c.states
    if not (ops and states and states[1]) then return nil, nil end
    if kind == "building" then return ops[1], states[1] end
    if kind == "unit"     then return ops[2], states[1] end
    return nil, nil
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
-- faction.isPlayerCommandable gate the "Attack" entry already uses),
-- and not the receiver itself (the self-transfer case A1 refuses at
-- request time). Zero selected units and MULTIPLE selected units both
-- fail this the same way, which is deliberate — see the context-menu
-- wiring for why "Transfer" is OMITTED rather than disabled for a
-- multi-unit selection.
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
-- `(kind, receiverId)`. Re-validates BOTH sides against fresh live
-- state (never the menu-build-time snapshot the caller may have used
-- to decide whether to show "Transfer" at all), so a receiver
-- destroyed or made ineligible between right-click and the player's
-- actual click is caught here as a stale target (requirement 9)
-- instead of producing a session that points at nothing.
--
-- Returns the session table on success, or (nil, reasonId) on
-- failure. A failure emits a player-visible "unit_warning" event and
-- mutates NEITHER inventory -- B1 never moves an item; that stays
-- C2's job once C1 has picked one.
function M.create(sourceUid, kind, receiverId)
    if not sourceUid or not unit.exists(sourceUid) then
        engine.emitEvent("unit_warning", "Cannot transfer: source unit not found")
        return nil, resolveReason(REASON_SOURCE_MISSING) or REASON_CONTRACT_UNAVAILABLE
    end

    local info = unit.transferReceiverInfo(kind, receiverId)
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

    local operation, state = resolveContractIdentity(kind)
    if not (operation and state) then
        engine.emitEventForUnit("unit_warning",
            "Cannot transfer: internal contract error", sourceUid)
        return nil, REASON_CONTRACT_UNAVAILABLE
    end

    local srcInfo = unit.getInfo(sourceUid)

    local id = nextSessionId
    nextSessionId = nextSessionId + 1

    M.active = {
        id                  = id,
        sourceUid           = sourceUid,
        receiverKind        = kind,
        receiverId          = receiverId,
        receiverDisplayName = info.displayName,
        -- Same page on both sides by construction: A1 refuses any
        -- cross-page request outright (Unit.Transfer.samePage), so the
        -- receiver's resolved page IS the source's page for any real
        -- player interaction (hit-testing only ever matches entities
        -- on the currently visible world).
        sourceLocation      = { page  = info.page,
                                 gridX = srcInfo and srcInfo.gridX,
                                 gridY = srcInfo and srcInfo.gridY },
        receiverLocation    = { page = info.page, gridX = info.gridX,
                                 gridY = info.gridY },
        -- The A1 contract identity (#1014 review): the operation this
        -- session will eventually request, plus this session's OWN
        -- lifecycle state -- A1's OWN first vocabulary value (resolved
        -- above, not a hardcoded "queued" literal), meaning "the
        -- player has committed to this transfer", NOT a real A1
        -- QueuedTransfer (see module header). C2/C3 own advancing it
        -- past here.
        contract            = { operation = operation, state = state },
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
