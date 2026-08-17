-- Transfer session (#1014, epic #1013 phase B1; generalized to named
-- endpoints by #1085 phase A2; made a real ESCORT by #1250, slice
-- UIT-3B): the shared entry point right-click "Transfer" opens.
--
-- MODE A: walk FIRST, then choose items. Selecting "Transfer" resolves
-- the nearest eligible selected unit (D-8), records this session, and
-- that unit walks to the destination and STOPS there. On arrival the
-- session opens two flanking panels and the player moves exact
-- instances in either direction, immediately and repeatably, for as
-- long as the window is open. That is the whole difference from Mode B
-- (#1246/#1249), which queues a durable order and requires no
-- adjacency: here the unit is standing next to the endpoint, so each
-- request is created and committed in the same instant.
--
-- The three things this module owns, and nothing else does:
--
--   IDENTITY   the source endpoint, the destination endpoint and the
--              contract vocabulary the eventual requests use, recorded
--              the moment the player commits to the transfer.
--   PHASE      "approaching" while the unit walks, "open" once it has
--              arrived. The transition happens exactly once and is what
--              fires the one camera snap, the one container reveal and
--              the panels.
--   THE HOLD   which unit(s) are standing there.
--              scripts/unit_ai_escort.lua's `escort_transfer` action
--              reads this module every tick and scores an in-progress
--              LOCK for as long as a session names that unit, which is
--              what keeps the wander tick and ordinary utility churn
--              from stealing it. The hold is RELEASED by the session
--              ending -- there is no separate release call, and
--              therefore no way to end a session and leave a unit
--              pinned.
--
--              Since #1251 (UIT-4) a session with a UNIT destination
--              holds BOTH ends, because unit-to-unit is the one
--              endpoint pairing where both of them can walk away. The
--              two roles differ only in what the held unit DOES: the
--              SOURCE walks and then stands, the TARGET stands from
--              the moment the session is created, so the approach has
--              a fixed destination and the walk-away problem cannot
--              occur during it. Both are the same 7.5 in-progress
--              lock, so the target's hold preempts its autonomous work
--              exactly like any player order. A BUILDING destination
--              has nothing to hold and this is unchanged for it.
--
-- What this module deliberately does NOT own: the panels themselves
-- (scripts/transfer_session_panels.lua supplies the window manager's
-- escort LEVEL KIND), and every transfer policy question -- eligibility,
-- proximity, capacity, exact-instance identity, per-item atomicity and
-- the refusal vocabulary are all src/Unit/Transfer.hs's, reached
-- through the engine verbs.
--
-- TRANSIENT by design (D-3, never registered as a save_modules
-- component, matching A1/A2): 'M.init' registers a reset hook so a
-- stale session pointing at a dead endpoint can never survive a LOAD.
-- A reset hook only fires from saveModules.applyAll(), never from an
-- ordinary "Exit to Menu" -> world.destroyAll() -> fresh world.init()
-- -- that path clears the session explicitly in
-- scripts/pause_menu.lua's onExitToMenu, the same place
-- build_tool/mine_tool clear their own transient armed state for the
-- identical reason. Both paths run the SAME coupled teardown, so
-- neither can leave panels open or a unit held.
--
-- Reusable on purpose (#1014 requirement 8): 'M.create' is the ONE
-- place a session gets built. scripts/init_context_menu.lua's
-- "Transfer" callback calls it below; a future drag-and-drop surface
-- calls the exact same function rather than duplicating this
-- validation.

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

-- Likewise NOT engine vocabulary, and named the same way and for the
-- same reason as the constant above (#1250 review round 1): the chosen
-- source is a real, live, player-commandable unit -- the contract would
-- accept it as an endpoint -- but its SPECIES cannot run the escort
-- action, so no session on it could ever walk, arrive or open. That is
-- a Mode A capability failure, not a transfer policy refusal, and
-- reporting it as one of the contract's own reasons would be a lie.
local REASON_SOURCE_NOT_ESCORTABLE = "source_not_escortable"

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

-- This session's own two phases (#1250). NOT contract vocabulary: the
-- contract describes a REQUEST's lifecycle, and a Mode A session makes
-- many requests (or none at all). These name where the ESCORT is.
local PHASE_APPROACHING = "approaching"
local PHASE_OPEN        = "open"

-- The AI action a Mode A source must be able to run for a session on it
-- to mean anything (scripts/unit_ai_transfer.lua's own name for it).
-- Named here rather than duplicated at each gate below.
local ESCORT_ACTION = "escort_transfer"
M.ESCORT_ACTION = ESCORT_ACTION

M.PHASE_APPROACHING = PHASE_APPROACHING
M.PHASE_OPEN        = PHASE_OPEN

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

-----------------------------------------------------------
-- Live reads (#1250)
--
-- The escort decides everything against LIVE state. The unit is
-- walking, and the destination may be a unit that is walking too, so
-- nothing below ever reads the creation-time snapshot in M.active.
-----------------------------------------------------------

-- The contract's own endpoint projection: position, footprint,
-- eligibility, capacity, load and loose contents, all as of now.
local function liveEndpoint(ep)
    if not ep or ep.id == nil then return nil end
    return unit.transferEndpointInfo({ kind = ep.kind, id = ep.id })
end

-- Every unit this session holds, in a fixed order: the escort first,
-- then a UNIT destination (#1251). A building destination contributes
-- nothing, so this is a one-element list for the commonest session and
-- the callers below need no `if kind == unit` of their own.
--
-- Derived from the session rather than recorded on it, so there is no
-- second copy of "who is held" that could disagree with `roleOf`.
local function heldUnits(s)
    if not s then return {} end
    if s.destination.kind == KIND_UNIT
       and s.destination.id ~= s.source.id then
        return { s.source.id, s.destination.id }
    end
    return { s.source.id }
end

-- Ask `uid` to re-decide on its NEXT tick rather than at its natural
-- thought cadence -- the same responsiveness every direct command
-- (commandMove / commandPickup / commandTransferOrder) buys, and what
-- makes both starting and ending a session feel immediate.
--
-- Read through package.loaded rather than require'd: this is a
-- player-gesture module and every headless UI fixture loads it WITHOUT
-- the unit AI. A session created where no AI is running simply has
-- nobody to nudge, which is exactly right.
local function nudgeUnit(uid)
    if type(uid) ~= "number" then return false end
    local core = package.loaded["scripts.unit_ai_core"]
    if not core or type(core.ensureState) ~= "function" then return false end
    local ok, s = pcall(core.ensureState, uid)
    if not ok or type(s) ~= "table" then return false end
    s.nextActionAt = 0
    return true
end

-- Centre the camera on the pair, ONCE, on the transition to the open
-- state (D-4 -- Mode A only; no Mode B order and no plain container
-- inspect ever moves the camera).
--
-- Measured in the SOURCE's own u-alias frame (#1175): two tiles that
-- are physically adjacent across the wrap seam sit a whole world apart
-- in canonical coords, and the midpoint of those two canonical values
-- is nowhere near the pair. world.localizeTile re-expresses the
-- destination beside the source before averaging; identity away from
-- the seam and in arena / non-wrapping worlds.
--
-- The destination's centre is its FOOTPRINT's, not its anchor's, so a
-- multi-tile cargo hold frames on the building rather than on a corner.
local function snapToPair(s)
    local src = unit.getInfo(s.source.id)
    local dst = liveEndpoint(s.destination)
    if not (src and dst and src.gridX and src.gridY
            and dst.gridX and dst.gridY) then
        return false
    end
    local sx, sy = math.floor(src.gridX), math.floor(src.gridY)
    local lx, ly = world.localizeTile(sx, sy, dst.gridX, dst.gridY)
    if not (lx and ly) then lx, ly = dst.gridX, dst.gridY end
    local cx = lx + ((dst.tileW or 1) - 1) * 0.5
    local cy = ly + ((dst.tileH or 1) - 1) * 0.5
    camera.goToTile(math.floor((sx + cx) * 0.5 + 0.5),
                    math.floor((sy + cy) * 0.5 + 0.5))
    return true
end

function M.init(scriptId)
    local saveMods = require("scripts.lib.save_modules")
    -- The load reset runs the SAME coupled teardown every other path
    -- does (requirement 7), not a bare `M.active = nil`: a load
    -- replaces the whole session, so panels pointing at endpoints the
    -- replacement may not even have must go with it, and the unit the
    -- old session was holding must be released.
    saveMods.registerResetHook("transfer_session", function()
        M.close("save_loaded")
    end)
    checkVocabulary()
end

-- The B1-owned "valid source" rule, NEAREST-OF-N (#1239; design
-- authority docs/unified_item_transfers.md D-8, and D-11 which records
-- that #1014's original "exactly one selected unit" rule was unfinished
-- work rather than settled intent). A multi-unit selection is allowed
-- and the nearest eligible unit goes.
--
-- A candidate is a selected unit that is player-commandable (the same
-- faction.isPlayerCommandable gate the "Attack" entry already uses --
-- and, since A2, the same gate the engine's own unit-endpoint
-- eligibility applies) and is not the destination itself (the
-- self-transfer case the contract refuses at request time). A uid whose
-- live faction or position has disappeared between selection and this
-- call is SKIPPED as ineligible rather than aborting the whole
-- resolution -- one dead selection entry must not cost the player the
-- entry. ZERO eligible candidates returns nil, and the context-menu
-- wiring then OMITS "Transfer" entirely rather than showing a disabled
-- row.
--
-- Ranking is by squared-Euclidean grid distance to `target`
-- (`unit.transferEndpointInfo`'s own gridX/gridY, so the ranked point
-- is exactly the endpoint the session will name), measured in the
-- TARGET's local u-alias frame via world.localizeTile (#1175's
-- selection-gate rule): a candidate physically adjacent across the wrap
-- seam measures a whole world away in canonical coords and would
-- otherwise never be chosen. Identity away from the seam and in
-- arena / non-wrapping worlds.
--
-- Both sides are FLOORED to whole tiles first, and that is load-bearing
-- rather than tidiness. unit.getInfo's gridX/gridY are the CONTINUOUS
-- position (Unit.Types.Instance's uiGridX is a Float, pushed with
-- Lua.pushnumber), while transferEndpointInfo reports an already-whole
-- tile -- and it derives a unit endpoint's tile with FLOOR
-- (Unit.Transfer's uevTile). world.localizeTile rounds whatever it is
-- handed, so feeding it a raw position would rank a source standing at
-- x=10.6 -- inside tile 10, possibly the destination's own tile -- as
-- tile 11. That mixed floor/round frame invents distance-1 gaps, which
-- both manufactures artificial ties and lets a genuinely farther unit
-- win. Flooring here puts candidates in exactly the tile frame the
-- endpoint already reports.
--
-- An exact distance tie breaks on the LOWEST uid (D-8), never on
-- selection order -- unit.getSelected() converts a HashSet, so its
-- order is not contractual and two equidistant acolytes would otherwise
-- race. This is deliberately NOT the "Pick up" precedent
-- (init_context_menu.lua, #920), whose `d < best` lets the first unit in
-- selection order win a tie; that gap is out of scope here.
--
-- `target` may be nil or carry no coords (nothing to rank against),
-- in which case every candidate is equidistant and the lowest-uid
-- tiebreak alone decides -- still deterministic, never a nil-arithmetic
-- error.
--
-- `requiredAction` (optional, #1250 review round 1) additionally
-- requires a candidate's SPECIES to be able to run that AI action, and
-- it filters BEFORE ranking for the same reason commandability does: a
-- candidate that cannot do the job must not win by being nearest and
-- then do nothing. Mode A passes "escort_transfer", because a source
-- whose species never registered it would sit in `approaching`
-- forever. Mode B's own resolution (transfer_gestures.retrieveEntries)
-- deliberately passes nothing: this issue does not change a shipped
-- gesture, and the equivalent question for a QUEUED order's executor is
-- its own concern to raise.
function M.resolveSource(selectedUids, excludeUid, target, requiredAction)
    if not selectedUids then return nil end
    local aiActions = require("scripts.unit_ai_actions")
    local tx = target and target.gridX and math.floor(target.gridX)
    local ty = target and target.gridY and math.floor(target.gridY)
    local best, bestUid = nil, nil
    for _, uid in ipairs(selectedUids) do
        if excludeUid == nil or uid ~= excludeUid then
            local info = unit.getInfo(uid)
            local fac = info and unit.getFaction(uid)
            if info and info.gridX and info.gridY and fac
               and faction.isPlayerCommandable(fac)
               and (requiredAction == nil
                    or aiActions.unitHas(uid, requiredAction)) then
                local d = 0
                if tx and ty then
                    local cx = math.floor(info.gridX)
                    local cy = math.floor(info.gridY)
                    local lx, ly = world.localizeTile(tx, ty, cx, cy)
                    if not (lx and ly) then lx, ly = cx, cy end
                    d = (lx - tx) * (lx - tx) + (ly - ty) * (ly - ty)
                end
                if bestUid == nil or d < best
                   or (d == best and uid < bestUid) then
                    best, bestUid = d, uid
                end
            end
        end
    end
    return bestUid
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
    -- Defence in depth behind the menu's omission (#1250 review round
    -- 1): 'M.create' is the ONE place a session is built and is
    -- deliberately reusable by surfaces that never ran resolveSource,
    -- so the capability is re-checked here rather than trusted. Placed
    -- after the destination checks so a call that is wrong on both
    -- counts still reports the destination first, as it always did.
    if not require("scripts.unit_ai_actions").unitHas(sourceUid,
                                                      ESCORT_ACTION) then
        engine.emitEventForUnit("unit_warning",
            "Cannot transfer: this unit cannot be escorted", sourceUid)
        return nil, REASON_SOURCE_NOT_ESCORTABLE
    end

    local sourceKind = resolveEndpointKind(KIND_UNIT)
    local destinationKind = resolveEndpointKind(kind)
    local state = resolveState(STATE_QUEUED)
    if not (sourceKind and destinationKind and state) then
        engine.emitEventForUnit("unit_warning",
            "Cannot transfer: internal contract error", sourceUid)
        return nil, REASON_CONTRACT_UNAVAILABLE
    end

    local srcInfo = unit.getInfo(sourceUid)

    -- Every validation above has passed, so this call is committed and
    -- only NOW may it disturb what is already there. A REJECTED
    -- replacement leaves the existing session — its panels, its phase
    -- and its held unit — exactly as it found them, which is what makes
    -- a mis-click on a demolished cargo harmless.
    M.close("replaced")

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
        --
        -- A creation-time RECORD, and nothing more (#1250 review): the
        -- escort reads live positions for everything it decides -- the
        -- approach, the arrival test, the camera snap and the panels'
        -- placement -- because the unit is walking and the endpoint may
        -- be walking too, so a snapshot taken at right-click time is
        -- stale by the time any of them happen.
        sourceLocation         = { page  = info.page,
                                    gridX = srcInfo and srcInfo.gridX,
                                    gridY = srcInfo and srcInfo.gridY },
        destinationLocation    = { page = info.page, gridX = info.gridX,
                                    gridY = info.gridY },
        -- Where the ESCORT is (#1250). A fresh session always starts
        -- with the unit still to walk, even when it happens to be
        -- standing next to the endpoint already -- arrival is decided by
        -- the AI action against the contract's own reach rule, never
        -- assumed here.
        phase                  = PHASE_APPROACHING,
        -- This session's OWN lifecycle state -- the contract's first
        -- vocabulary value (resolved by membership above, not a
        -- hardcoded literal and not an array position), meaning "the
        -- player has committed to this transfer", NOT a real queued
        -- transfer order (see module header).
        contract               = { state = state },
    }
    -- Decide on the next tick rather than at the unit's natural
    -- cadence, the same responsiveness commandMove / commandPickup /
    -- commandTransferOrder buy.
    --
    -- BOTH held units (#1251), and for the target that is not a nicety:
    -- its hold begins HERE, so whatever autonomous work it was doing
    -- has to be preempted at the next tick rather than whenever its own
    -- thought cadence next came round. A unit walking somewhere would
    -- otherwise keep walking for up to a full thought interval after the
    -- session that pinned it existed.
    for _, uid in ipairs(heldUnits(M.active)) do
        nudgeUnit(uid)
    end
    return M.active
end

-- The current session, or nil.
function M.get()
    return M.active
end

-- Which side of this session `uid` is on: "source" (the escort that
-- walks and then stands) or "target" (a UNIT destination, held from
-- creation), or nil for a unit no session names. The AI action's whole
-- question, asked every tick -- it needs the ROLE and not just a
-- boolean, because the two sides do different things with the same
-- lock.
--
-- The source is tested FIRST, so a degenerate session whose two
-- endpoints are the same unit reads as the source and behaves exactly
-- as it did before this issue. The contract refuses such a transfer at
-- request time; nothing here has to invent a second answer for it.
function M.roleOf(uid)
    local s = M.active
    if not s or type(uid) ~= "number" then return nil end
    if s.source.id == uid then return "source" end
    if s.destination.kind == KIND_UNIT and s.destination.id == uid then
        return "target"
    end
    return nil
end

-- Is `uid` held by this session at all? Kept as its own predicate
-- because most callers only need the fact, not the side.
function M.holdsUnit(uid)
    return M.roleOf(uid) ~= nil
end

-- The destination as it is RIGHT NOW: position, footprint and
-- eligibility, straight from the contract's own endpoint projection.
-- nil when it no longer resolves at all.
function M.destinationNow()
    local s = M.active
    if not s then return nil end
    return liveEndpoint(s.destination)
end

-- The one-way transition to the open/held state, run by the AI action
-- the moment the unit is within the contract's own reach of the
-- destination. Returns true only on the transition itself, so
-- everything below happens EXACTLY ONCE per session:
--
--   * the container REVEAL (requirement 3) -- and only for a building,
--     because a unit endpoint has no remembered snapshot to refresh.
--     Before the panels, so the container pane's first render is
--     already the fresh one.
--   * the two panels.
--   * the one camera SNAP (D-4, requirement 1), centred on the pair's
--     LIVE positions. Mode A only: no Mode B order and no plain
--     container inspect ever moves the camera.
--
-- A resize, a reflow and every later commit re-run none of it.
function M.markArrived()
    local s = M.active
    if not s or s.phase ~= PHASE_APPROACHING then return false end
    s.phase = PHASE_OPEN

    if s.destination.kind == KIND_BUILDING then
        building.refreshContainerKnowledge(s.destination.id)
    end
    -- pcall'd, and the FAILURE PATH ends the session: the phase has
    -- already flipped (so nothing above can run twice), which means a
    -- window that did not open would otherwise leave this unit held
    -- against nothing, forever, with no way for the player to release
    -- it. Ending here is the only disposition that cannot strand it.
    local ok, opened = pcall(function()
        return require("scripts.transfer_session_panels").open(s)
    end)
    if not (ok and opened) then
        if not ok then
            engine.logError("transfer_session: opening the escort panels "
                .. "failed -- " .. tostring(opened))
        end
        engine.emitEventForUnit("unit_warning",
            "Cannot transfer: could not open the transfer window",
            s.source.id)
        M.close("panels_unavailable")
        return false
    end
    snapToPair(s)
    return true
end

-- Coupled teardown (requirement 7), and the ONLY way a session ends.
-- Idempotent, and the same call for every path that ends one: a closed
-- panel, a replacement, an endpoint that vanished, Exit to Menu, a save
-- load.
--
-- The active session is cleared FIRST and deliberately: closing the
-- level fires the escort kind's own `onClose`, which calls back into
-- `onLevelClosed` below, and finding no session with that id is what
-- stops the two halves of the coupling from re-entering each other. No
-- flag, no ordering rule to remember.
--
-- Releasing the hold is simply the session no longer existing -- the AI
-- action scores -inf on its next tick with nothing left to hold it --
-- so the only thing left to do is ask that unit to re-decide NOW rather
-- than at its natural cadence.
function M.close(reason)
    local s = M.active
    if not s then return false end
    M.active = nil
    engine.logDebug("transfer_session: session " .. tostring(s.id)
        .. " ended (" .. tostring(reason or "closed") .. ")")
    pcall(function()
        require("scripts.transfer_session_panels").closeFor(s.id)
    end)
    -- STOP each held unit, not just release it. A session that ends
    -- while its escort is still APPROACHING leaves a walk in flight
    -- toward an endpoint that no longer means anything, and the AI will
    -- not interrupt that walk on its own: unit_ai's execute gate re-runs
    -- an action only on a SWITCH or when the unit is idle, and this
    -- action is deliberately not forceExecute (re-issuing moveTo
    -- mid-walk wipes the engine-side path). So a replacement session on
    -- the SAME unit would keep walking to the OLD destination until that
    -- path ran out before it ever looked at the new one. Stopping here
    -- is what makes the unit idle, which is what makes the next tick
    -- re-decide immediately -- and it is a no-op for the far commoner
    -- case, a unit that is already standing still.
    --
    -- BOTH ends for a unit-to-unit session (#1251, requirement 2): no
    -- session path may leave either unit held, and there is exactly one
    -- teardown, so covering the pair here covers a panel close, a
    -- replacement, an endpoint that vanished, Exit to Menu and the
    -- successful-load reset alike. Stopping is all this does to the
    -- target: it cancels no order and clears no goal, so a durable
    -- Mode B order the target is carrying (its own, or one a load just
    -- restored onto a reused uid) survives the release and its executor
    -- simply re-issues the walk on its next tick.
    for _, uid in ipairs(heldUnits(s)) do
        unit.stop(uid)
        nudgeUnit(uid)
    end
    return true
end

-- The window manager's side of the coupling: the escort level was
-- destroyed for a real reason (Escape, another container replacing it,
-- an endpoint that stopped resolving), so the session it was showing
-- ends. A "layout" teardown never reaches here -- the manager filters
-- it -- which is what lets a resize rebuild both panes with the session
-- and its hold intact (requirement 8).
function M.onLevelClosed(sessionId, reason)
    local s = M.active
    if not s or s.id ~= sessionId then return false end
    return M.close(reason or "closed")
end

-- Explicit teardown, kept under its historical name because
-- scripts/pause_menu.lua's onExitToMenu speaks it (#1014 review round
-- 1: the save-load reset hook alone misses that path).
function M.clear()
    M.close("cleared")
end

return M
