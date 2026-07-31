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
-- pointing at a dead receiver can never survive a world load.
--
-- Reusable on purpose (requirement 8): 'M.create' is the ONE place a
-- session gets built. scripts/init_context_menu.lua's "Transfer"
-- callback calls it below; a future drag-and-drop surface calls the
-- exact same function rather than duplicating this validation.

local M = package.loaded["scripts.transfer_session"] or {}
package.loaded["scripts.transfer_session"] = M

local nextSessionId = 1

-- A1's reason/operation vocabulary (Unit.Transfer.transferReasonId /
-- transferOperationId), read from unit.transferContract() rather than
-- assumed — #1014 review round: "name the failure using A1's reason
-- ids ... obtained from unit.transferContract() rather than new
-- strings". These three are creation-time failures; 'became_stale' is
-- the vocabulary's OWN name for a request that passed at create time
-- and broke before commit, which is C2's concern, not B1's.
local REASON_SOURCE_MISSING      = "source_missing"
local REASON_RECEIVER_MISSING    = "receiver_missing"
local REASON_RECEIVER_INELIGIBLE = "receiver_ineligible"

local OPERATION_BY_KIND = {
    building = "unit_to_building_storage",
    unit     = "unit_to_unit_inventory",
}

local function containsValue(list, v)
    for _, x in ipairs(list or {}) do
        if x == v then return true end
    end
    return false
end

-- One-time drift check: every reason/operation id this module names
-- must actually appear in the live A1 contract. A silent mismatch here
-- would mean the failure/session strings this module hands to
-- C1/C2/C3 (and the player-visible warning text) no longer match what
-- 'unit.checkTransfer'/'unit.commitTransfer' actually report.
local function checkVocabulary()
    local c = unit.transferContract()
    local reasons = (c and c.reasons) or {}
    local operations = (c and c.operations) or {}
    for _, id in ipairs({ REASON_SOURCE_MISSING, REASON_RECEIVER_MISSING,
                           REASON_RECEIVER_INELIGIBLE }) do
        if not containsValue(reasons, id) then
            engine.logWarn("transfer_session: reason id '" .. id
                .. "' missing from unit.transferContract() -- drifted "
                .. "from Unit.Transfer's vocabulary")
        end
    end
    for _, id in pairs(OPERATION_BY_KIND) do
        if not containsValue(operations, id) then
            engine.logWarn("transfer_session: operation id '" .. id
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
        return nil, REASON_SOURCE_MISSING
    end

    local info = unit.transferReceiverInfo(kind, receiverId)
    if not info then
        engine.emitEventForUnit("unit_warning",
            "Cannot transfer: target no longer exists", sourceUid)
        return nil, REASON_RECEIVER_MISSING
    end
    if not info.eligible then
        local name = (info.displayName ~= "" and info.displayName) or "Target"
        engine.emitEventForUnit("unit_warning",
            "Cannot transfer: " .. name .. " cannot receive items", sourceUid)
        return nil, REASON_RECEIVER_INELIGIBLE
    end

    local srcInfo = unit.getInfo(sourceUid)
    local operation = OPERATION_BY_KIND[kind]

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
        -- lifecycle state -- 'queued' from A1's vocabulary, meaning
        -- "the player has committed to this transfer", NOT a real A1
        -- QueuedTransfer (see module header). C2/C3 own advancing it
        -- past here.
        contract            = { operation = operation, state = "queued" },
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
