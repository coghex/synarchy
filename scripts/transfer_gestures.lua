-- Mode B transfer gestures (#1249, epic #1013 phase 3): the player's
-- "Store" and "Retrieve" context-menu entries.
--
-- ONE builder for both, because they are the same gesture read in two
-- directions. What differs is only which endpoint is the source, which
-- is the destination, and which unit walks:
--
--   Store     a unit-info inventory row -> the open container window's
--             active endpoint. Source and executor are the unit whose
--             info panel the row belongs to.
--   Retrieve  a container-window row -> the unit the SHARED selection
--             rule resolves. Source is the window's own endpoint;
--             destination and executor are that unit.
--
-- These REPLACE the two immediate player paths they were promoted from
-- (requirement 5): the adjacent-cargo "Store in <cargo>" enumeration
-- that called `unit.depositToCargo`, and the container window's
-- "Withdraw with <unit>" that called `unit.withdrawFromCargo` (plus its
-- disabled "select an adjacent unit first" placeholder). Both of those
-- moved an item on the spot and required adjacency; these queue a
-- durable order (#1246) that #1247's unit job walks to and commits on
-- arrival, so NEITHER gesture requires adjacency and neither moves
-- anything at the moment it is clicked.
--
-- The lax verbs themselves are untouched and stay registered (D-7):
-- `unit.depositToCargo`, `unit.withdrawFromCargo`,
-- `unit.transferItemToUnit` and `unit.transferItemToBuilding` remain the
-- AI ladders' own deliberately unchecked path
-- (`scripts/unit_ai_fetch.lua` and friends). Only the PLAYER-facing
-- paths retired.
--
-- Nothing here moves the camera (D-4), and nothing here invalidates a
-- host's list: a queued order changes no contents, so there is nothing
-- to redraw. The items leave when the executor arrives and commits, and
-- each host's existing per-tick refresh is what shows that.
--
-- Since #1250 this builder serves MODE A as well, and that is the whole
-- reason `entries` takes a `submit`: an escort session's row menus are
-- the identical 1-and-all gesture with the identical omission rules and
-- the identical exact-instance membership, differing only in that the
-- source unit is already standing there, so the items move on the spot
-- instead of becoming an order. Everything a merged row means — which
-- instance the singular entry names, which set "all" names, when the
-- gesture is omitted entirely — is decided ONCE, here, for both modes.
--
-- Public API:
--   activeEndpoint()                     -- the open window's active
--                                           endpoint, or nil
--   storeEntries(uid, row)               -- unit-info row -> that window
--   retrieveEntries(endpoint, row)       -- window row -> resolved unit
--   entries(opts)                        -- the shared 1/all builder
--   TRANSFER_ORDER_ACTION                -- the executor capability both
--                                           gestures gate on
--
-- Both gestures require their EXECUTOR's species to be able to run the
-- queued order (#2030) -- Store the panel's own unit, Retrieve the
-- resolved one -- because an order queued for a species the action was
-- never registered for is never ticked and stays pending for ever. The
-- rule comes from the same registration the dispatch loop uses
-- (scripts/unit_ai_actions.lua), never a species list beside it, so the
-- two cannot drift as species are added.

local M = package.loaded["scripts.transfer_gestures"] or {}
package.loaded["scripts.transfer_gestures"] = M

local itemList = require("scripts.ui.item_list")
-- The per-species AI action inventory (#1250): a leaf module with no
-- dependencies, so requiring it at the top closes no cycle.
local aiActions = require("scripts.unit_ai_actions")

-- The AI action a Mode B order's EXECUTOR must be able to run
-- (scripts/unit_ai_transfer.lua's own name for it, exported there as
-- TRANSFER_ORDER_ACTION). Named here rather than spelled as a literal at
-- each of the two gates below, exactly as transfer_session.lua names
-- ESCORT_ACTION for Mode A -- and for the same reason it does not import
-- it: scripts/unit_ai_transfer.lua reads
-- package.loaded["scripts.unit_ai"] at module scope, so requiring it
-- from a UI module that never loaded the AI would fault rather than
-- answer. 'Test.Headless.UI.TransferGestures' pins the two strings
-- together so they cannot drift apart in silence.
local TRANSFER_ORDER_ACTION = "transfer_order"
M.TRANSFER_ORDER_ACTION = TRANSFER_ORDER_ACTION

-- Can `uid`'s species run a queued transfer order?
--
-- Both gestures ask this about their EXECUTOR -- the unit that would
-- walk the order and commit it on arrival -- because an order queued for
-- a species the action was never registered for is never ticked at all:
-- it sits in the store for ever, with nothing to show the player and
-- nothing to cancel it (#2030). Derived from the same registration that
-- decides who CAN run it, never a species list beside it.
--
-- An EMPTY registry answers true, which is unit_ai_actions' own
-- deliberate rule (requirement 5): a UI-only fixture or a menu process
-- that never loaded any AI has no inventory to consult and must not
-- invent a refusal from its absence.
local function canCarryOrder(uid)
    return aiActions.unitHas(uid, TRANSFER_ORDER_ACTION)
end

-- Batch granularity is 1 and all (signed off 2026-08-11), in BOTH
-- modes: #1250 shipped Mode A's session menus on this same builder, and
-- the fuller 1/N/all quantity picker stays deferred to a later issue if
-- play shows it is missed.
local LABEL_ONE = "%s 1"
local LABEL_ALL = "%s all"

local function sameEndpoint(a, b)
    return a ~= nil and b ~= nil and a.kind == b.kind and a.id == b.id
end

-- The container window's ACTIVE level, expressed as a transfer endpoint
-- -- or nil when it cannot supply one.
--
-- Only the deepest level may answer (#1238's interaction contract: it is
-- the only interactive one), and only when it is an ENDPOINT level.
-- An item-container level is render-only (D-5, requirement 4): it is not
-- a transfer endpoint, so a gesture targeting it has nowhere to put an
-- item. This deliberately does NOT walk up to a transfer-capable
-- ancestor -- a player looking into a toolbox inside a cargo hold is
-- pointing at the toolbox, and silently retargeting the cargo hold would
-- move the item somewhere they did not name.
--
-- An ESCORT level (#1250) answers nil for the same reason read the
-- other way: it shows TWO endpoints, so "the endpoint this window is
-- showing" has no single answer and picking one would be guessing. The
-- unit-info "Store" gesture is therefore omitted while a session is
-- open -- which costs the player nothing, because the session's own
-- panes already offer Store and Retrieve on both sides, immediately.
function M.activeEndpoint()
    -- Required lazily, like every other cross-reference to the window
    -- manager (it requires this module too, from its own transferMenu,
    -- and a top-level require in both directions is a load cycle).
    local window = require("scripts.cargo_inventory_panel")
    if not window.isOpen() then return nil end
    local level = window.getLevel()
    local src = level and level.src
    if not src or src.kind ~= "endpoint" then return nil end
    if src.endpointKind == nil or src.id == nil then return nil end
    return { kind = src.endpointKind, id = src.id }
end

-- Queue ONE order for the exact instance ids given.
--
-- Routed through unit_ai's public command rather than
-- `unit.createTransferOrder` directly, so a gesture-created order is
-- indistinguishable from any other: the same player-visible warning on
-- refusal, the same per-item outcome reporting on a partial batch, and
-- the same "decide on the next tick" responsiveness the other direct
-- commands (commandMove / commandPickup) buy.
local function queueOrder(executorUid, source, destination, defName, ids)
    local items = {}
    for i, iid in ipairs(ids) do
        items[i] = { instanceId = iid, defName = defName }
    end
    local unitAi = require("scripts.unit_ai")
    unitAi.commandTransferOrder(executorUid, {
        source      = source,
        destination = destination,
        items       = items,
    })
end

-- The shared 1/all builder. Returns a possibly-EMPTY list of
-- context-menu entries; a caller appends whatever it gets.
--
-- Omission, never a disabled row: every reason a gesture cannot run is
-- a reason the player has no decision to make here. A self-transfer is
-- one of them -- the contract refuses source == destination outright, so
-- offering it would queue a predictably invalid order.
--
-- opts:
--   verb         "Store" | "Retrieve" -- names the entries
--   row          the rendered row the player right-clicked
--   executor     uid that walks the order and commits it on arrival
--                (Mode A: the unit already standing there)
--   source       { kind, id }
--   destination  { kind, id }
--   submit       optional (executor, source, destination, defName, ids)
--                -- what a chosen entry DOES. Defaults to queueing a
--                durable Mode B order; the escort session (#1250)
--                passes its own immediate check-and-commit.
function M.entries(opts)
    if type(opts) ~= "table" then return {} end
    local row, source, destination = opts.row, opts.source, opts.destination
    if not row or not source or not destination then return {} end
    if type(opts.executor) ~= "number" then return {} end
    if sameEndpoint(source, destination) then return {} end

    local defName = row.defName
    if type(defName) ~= "string" or defName == "" then return {} end

    local ids = itemList.rowInstanceIds(row)
    if #ids == 0 then return {} end

    -- The representative drives the singular entry; the complete
    -- ordered membership drives "all". They agree on a single-instance
    -- row by construction (the representative is the row's first
    -- member), so this reads the row's own field and falls back rather
    -- than assuming which.
    local representative = type(row.instanceId) == "number"
                             and row.instanceId or ids[1]
    local verb = opts.verb or "Store"
    local executor = opts.executor
    local submit = opts.submit or queueOrder

    local out = {}
    out[1] = {
        label    = string.format(LABEL_ONE, verb),
        callback = function()
            submit(executor, source, destination, defName,
                   { representative })
        end,
    }
    -- A single-instance row shows the singular entry alone: an "all"
    -- beside it would name the same one item twice.
    if #ids > 1 then
        out[2] = {
            label    = string.format(LABEL_ALL, verb),
            callback = function()
                submit(executor, source, destination, defName, ids)
            end,
        }
    end
    return out
end

-- "Store": a unit-info inventory row into the open container window's
-- active endpoint. No window open, or an active level that is not an
-- endpoint, means no entries at all (requirement 1).
function M.storeEntries(uid, row)
    local destination = M.activeEndpoint()
    if not destination then return {} end
    -- The panel's own unit is BOTH source and executor here, so a
    -- species that cannot run the order omits the gesture entirely
    -- (#2030) -- omission, never a disabled row, like every other reason
    -- above.
    if not canCarryOrder(uid) then return {} end
    return M.entries({
        verb        = "Store",
        row         = row,
        executor    = uid,
        source      = { kind = "unit", id = uid },
        destination = destination,
    })
end

-- "Retrieve": a container-window row into the unit the SHARED selection
-- rule picks (`transfer_session.resolveSource` -- nearest of the
-- selection, lowest uid breaking a tie, non-commandable units and units
-- whose species cannot run the order both skipped).
-- Reusing it is the point: a third copy of "which selected unit acts?"
-- is exactly what this consumes rather than adds.
--
-- The window's own endpoint is excluded from that resolution when it is
-- a unit, so a unit endpoint's window never resolves itself as the
-- retriever. Zero eligible units means no entries (requirement 2 --
-- never a disabled row, which is what the retired withdraw placeholder
-- was).
function M.retrieveEntries(endpoint, row)
    if type(endpoint) ~= "table" then return {} end
    local session = require("scripts.transfer_session")
    local excludeUid = endpoint.kind == "unit" and endpoint.id or nil
    -- Ranked against the endpoint's OWN reported tile, which is the
    -- point the resolver measures to.
    local target = unit.transferEndpointInfo(endpoint)
    -- Capability filters BEFORE ranking (#2030), which is the whole
    -- reason the shared rule takes the action rather than being asked
    -- afterwards: a nearer unit that cannot carry an order must not win
    -- the resolution and then do nothing. The nearest CAPABLE candidate
    -- wins; none at all omits the gesture, the same as no candidate.
    local uid = session.resolveSource(unit.getSelected(), excludeUid, target,
                                      TRANSFER_ORDER_ACTION)
    if not uid then return {} end
    return M.entries({
        verb        = "Retrieve",
        row         = row,
        executor    = uid,
        source      = endpoint,
        destination = { kind = "unit", id = uid },
    })
end

return M
