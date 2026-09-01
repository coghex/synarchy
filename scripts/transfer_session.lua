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
--              scripts/unit_ai_escort.lua's two actions read this
--              module every tick and score an in-progress LOCK for as
--              long as a session names that unit, which is what keeps
--              the wander tick and ordinary utility churn from
--              stealing it. The hold is RELEASED by the session
--              ending -- there is no separate release call, and
--              therefore no way to end a session and leave a unit
--              pinned.
--
--              Since #1251 (UIT-4) a session with a UNIT destination
--              holds BOTH ends, because unit-to-unit is the one
--              endpoint pairing where both of them can walk away. The
--              two roles differ only in what the held unit DOES: the
--              SOURCE walks and then stands (`escort_transfer`), the
--              TARGET stands from the moment the session is created
--              (`escort_hold`), so the approach has a fixed
--              destination and the walk-away problem cannot occur
--              during it. Both score the same 7.5 in-progress lock, so
--              the target's hold preempts its autonomous work exactly
--              like any player order and neither end can outscore the
--              other. `roleOf` is what tells them apart, and it is the
--              ONE answer both consult. A BUILDING destination has
--              nothing to hold and this is unchanged for it.
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
-- -- so this module also registers with the session-teardown boundary
-- (scripts/lib/session_teardown.lua, #1610), which pause_menu runs on
-- that path and nowhere else. build_tool/mine_tool register there for
-- the identical reason. Both paths run the SAME coupled teardown, so
-- neither can leave panels open or a unit held.
--
-- FAILURE HANDLING (#1254, slice UIT-5B). Every way a session can be
-- interrupted ends it through that one teardown, and the module's job
-- is to NOTICE each of them:
--
--   an endpoint that vanished, stopped being eligible, or is dead or
--   unconscious      'M.update', every tick, in BOTH phases
--   a new player order to a held unit
--                    'M.notePlayerOrder', from the player's own
--                    ingress sites only
--   a replacement session
--                    'M.create', after every validation has passed
--   the window closing, an endpoint the manager saw vanish
--                    'M.onLevelClosed', the window stack's own side
--   a zoom-band change or the HUD leaving the screen
--                    scripts/ui/view_teardown.lua (#156), which is
--                    where a new teardown trigger belongs -- never a
--                    one-off call at the transition site
--   Exit to Menu     scripts/pause_menu.lua's 'M.clear', before
--                    world.destroyAll() so the release still reaches
--                    live entities
--   a successful load
--                    the reset hook above
--
-- What none of them changes is COMMIT atomicity (requirement 7): a
-- session owns no transaction of its own. Each row gesture is one
-- 'unit.checkTransfer' + 'unit.commitTransfer' pair on the identical
-- request, and src/Unit/Transfer.hs commits each item both-or-neither,
-- so an interruption can only ever land between two whole requests.
-- Ending a session never rolls back what already committed.
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
-- These five literals name WHICH of B1's own creation-time failure
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
--
-- 'source_ineligible' and 'out_of_range' joined the list with #1415,
-- which gave 'M.create' the two source rules 'M.resolveSource' had
-- always enforced and it never did. Both are the engine's OWN answer
-- to the condition they report, not a name invented here:
-- Unit.Transfer.planItemWith refuses an uncommandable source with
-- ReasonSourceIneligible, and a page mismatch is exactly how its
-- 'reachable' check fails, which it reports as ReasonOutOfRange. The
-- self-transfer rule needs no new id at all -- the engine maps
-- 'from == to' onto ReasonReceiverIneligible, which B1 already named.
local REASON_SOURCE_MISSING       = "source_missing"
local REASON_SOURCE_INELIGIBLE    = "source_ineligible"
local REASON_RECEIVER_MISSING     = "receiver_missing"
local REASON_RECEIVER_INELIGIBLE  = "receiver_ineligible"
local REASON_OUT_OF_RANGE         = "out_of_range"
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
    for _, id in ipairs({ REASON_SOURCE_MISSING, REASON_SOURCE_INELIGIBLE,
                           REASON_RECEIVER_MISSING,
                           REASON_RECEIVER_INELIGIBLE,
                           REASON_OUT_OF_RANGE }) do
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

-- The poses in which a unit has stopped being commandable ALTOGETHER,
-- named exactly as scripts/unit_ai.lua's own short-circuit names them
-- (its `pose == "collapsed" or pose == "dead"` gate, which returns
-- before any action is scored). A unit in one of them runs no AI at
-- all, so a hold on it holds nothing and an escort on it will never
-- take another step.
--
-- Deliberately NOT the recoverable poses beside them -- crawling,
-- sleeping, an engine animation -- which are interruptions a session
-- is expected to sit through, the same way a queued Mode B order does.
local INCAPACITATED_POSES = { collapsed = true, dead = true }

-- Why `ep` can no longer be an endpoint of a LIVE session, or nil
-- while it still can. `role` ("source" / "destination") only names
-- which side the answer is about.
--
-- The contract's own endpoint projection answers most of it: a
-- demolished building and a unit that left the player's factions both
-- stop resolving, or stop being `eligible`, right there. What it
-- deliberately does not answer is whether a live unit is in any state
-- to be HELD -- Unit.Transfer.endpointEligible is
-- `uevCommandable` and nothing else, so a dead acolyte is still a
-- perfectly eligible endpoint by the contract's lights. Requirement 2
-- is exactly that case, so the pose test is added HERE rather than
-- widened in the contract, which owns transfer POLICY (may these two
-- endpoints exchange this item?) and not the session's own liveness.
--
-- Every string returned is a DEBUG-LOG reason for `M.close`, in the
-- same free-form family as "replaced" / "cleared" / "save_loaded".
-- None of them is contract vocabulary and none reaches the player:
-- this issue adds no failure vocabulary (its own out-of-scope list),
-- and `Unit.Transfer`'s reason ids are untouched.
local function endpointFailure(ep, role)
    if not ep or ep.id == nil then return role .. "_gone" end
    local info = liveEndpoint(ep)
    if not info then return role .. "_gone" end
    if info.eligible ~= true then return role .. "_ineligible" end
    if ep.kind == KIND_UNIT then
        local pose = unit.getPose(ep.id)
        if pose == nil then return role .. "_gone" end
        if INCAPACITATED_POSES[pose] then
            return role .. "_incapacitated"
        end
    end
    return nil
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
    -- `unitsAreStale` is what makes this the ONE teardown every other
    -- path also runs, without it reaching into a session it no longer
    -- describes. saveModules.applyAll fires reset hooks only after
    -- every component has committed, so by the time this runs the uids
    -- this session recorded name whatever the LOAD restored onto them
    -- -- session-global entity ids are reused across sessions. Panels
    -- and identity are this module's own state and go; stopping and
    -- nudging a unit is not, and would be applied to a stranger.
    saveMods.registerResetHook("transfer_session", function()
        M.close("save_loaded", { unitsAreStale = true })
    end)
    checkVocabulary()
end

-- The engine's own per-tick entry point (scripts/init_loader.lua
-- loadScript's this module), and the ONE place requirements 1 and 2
-- are noticed.
--
-- It has to be here rather than on the window, because a session
-- spends its whole APPROACH with no window at all: the container
-- manager's per-tick `stillThere` hook covers an endpoint that
-- vanishes while the panels are open, and nothing covered the walk. A
-- demolished cargo, a dead escort or a target that dropped out of the
-- player's factions mid-approach would otherwise leave the pair held
-- against nothing until the player noticed and closed a window that
-- was never opened.
--
-- Cheap by construction: a tick with no session reads nothing at all.
function M.update(_dt)
    if not M.active then return end
    local reason = M.staleReason()
    if reason then M.close(reason) end
end

-- Why the active session can no longer continue, or nil while it can.
-- Public because it is the rule, not an implementation detail: a gate
-- asserting "this interruption ends the session" should be able to ask
-- the same question the tick asks.
function M.staleReason()
    local s = M.active
    if not s then return nil end
    return endpointFailure(s.source, "source")
        or endpointFailure(s.destination, "destination")
end

-- The shared PLAYER-ORDER boundary (requirement 3, signed off
-- 2026-08-11): a player giving a held unit a new order ends the
-- session, and the order then proceeds. Player intent wins, which is
-- both the RTS convention and this codebase's own rule for every other
-- window where a player verb races an automated one.
--
-- Called from the player's own ingress sites and from nowhere else --
-- scripts/init_mouse_entity.lua's right-click move order, and
-- scripts/init_context_menu.lua's Attack / Pick up / Move here. It is
-- deliberately NOT inside unitAi.commandMove / commandAttack /
-- commandPickup: scripts/building_spawn.lua and
-- scripts/unit_ai_combat.lua call those for scripted and autonomous
-- behaviour, and a session must not be cancelled by a spawn roster
-- walking a fresh acolyte out of a portal.
--
-- Ending the session STOPS every unit it held (see `M.close`), so this
-- must run BEFORE the order is issued or the teardown would wipe the
-- walk the player just asked for. Returns whether it ended anything,
-- so the caller can order unconditionally either way.
function M.notePlayerOrder(uid)
    if not M.holdsUnit(uid) then return false end
    M.close("player_order")
    return true
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
-- forever.
--
-- BOTH modes pass one now (#2030): Mode B's own resolution
-- (transfer_gestures.retrieveEntries) passes "transfer_order", because
-- an executor whose species never registered THAT would leave a queued
-- order pending for ever. The argument stays optional because a caller
-- with no capability question -- one resolving a unit for something
-- every species can do -- still has none to ask.
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
-- `(kind, destinationId)`. Re-validates BOTH sides, and the PAIR they
-- form, against fresh live state (never the menu-build-time snapshot
-- the caller may have used to decide whether to show "Transfer" at
-- all), so a destination destroyed or made ineligible between
-- right-click and the player's actual click is caught here as a stale
-- target (requirement 9) instead of producing a session that points at
-- nothing.
--
-- Exactly what each side is checked for, because this is the ONE place
-- a session is built and it is deliberately reusable by surfaces that
-- never ran 'M.resolveSource' (#1415):
--
--   * DESTINATION -- it still resolves as an endpoint, and it is
--     eligible.
--   * PAIR -- it is not the source itself, and it is on the source's
--     page.
--   * SOURCE -- the unit exists, it still projects as an endpoint, it
--     is eligible (which for a unit IS player-commandability --
--     Unit.Transfer.endpointEligible is `uevCommandable` and nothing
--     else), and its SPECIES can run the escort action.
--
-- Check ORDER is the engine's, not this module's convenience: a call
-- wrong on both sides reports the DESTINATION first (the placement rule
-- the escort-capability check already documented), and a self-pair
-- reports `receiver_ineligible` ahead of any source rule exactly as
-- Unit.Transfer.planItemWith orders its own refusals. What this does
-- NOT re-derive is the engine's REACH rule: a fresh session is always
-- `approaching` and adjacency is the escort's job, so the pair check
-- here is page identity alone.
--
-- Returns the session table on success, or (nil, reasonId) on failure.
-- A failure emits ONE player-visible "unit_warning" event for the
-- source unit and mutates NOTHING -- no item moves (B1 never moves an
-- item; that stays C2's job once C1 has picked one), and an
-- already-active session keeps its panels, its phase and its held units
-- exactly as they were.
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

    -- The PAIR rules and the remaining SOURCE rules, in the engine's own
    -- order (#1415). Each is one 'M.resolveSource' already enforced and
    -- 'M.create' trusted the caller for; the two menus screen all of
    -- them before they ever get here, so nothing player-facing changes
    -- -- what changes is that a surface which never ran resolveSource
    -- can no longer mint a session the contract will refuse every
    -- commit from.

    -- Identity, not state, and ahead of every source rule because
    -- Unit.Transfer.planItemWith puts it there: `from == to` is the
    -- engine's own ReasonReceiverIneligible, so a caller that hands the
    -- same unit twice gets the same id from B1 as from a commit. Left
    -- unchecked this was the one failure NOTHING caught afterwards --
    -- 'endpointFailure' validates each side independently, so both
    -- passed, and the escort walked to itself forever.
    if kind == KIND_UNIT and destinationId == sourceUid then
        engine.emitEventForUnit("unit_warning",
            "Cannot transfer: a unit cannot transfer to itself", sourceUid)
        return nil, resolveReason(REASON_RECEIVER_INELIGIBLE)
                    or REASON_CONTRACT_UNAVAILABLE
    end

    -- The source's own live projection, which is where the remaining
    -- two rules are decided. Asked ONCE: 'eligible' and 'page' are both
    -- read off it, and a second query could answer differently.
    local srcEp = unit.transferEndpointInfo({ kind = KIND_UNIT,
                                              id   = sourceUid })
    if not srcEp then
        -- The existence guard at the top already passed, so the unit is
        -- still addressable and the warning goes to it. Reported as
        -- source-missing rather than as a contract failure because that
        -- is what it is, and what planItemWith calls the same condition
        -- (it notes an absent source ahead of every pair-policy check).
        engine.emitEventForUnit("unit_warning",
            "Cannot transfer: source unit not found", sourceUid)
        return nil, resolveReason(REASON_SOURCE_MISSING)
                    or REASON_CONTRACT_UNAVAILABLE
    end
    if not srcEp.eligible then
        engine.emitEventForUnit("unit_warning",
            "Cannot transfer: this unit cannot transfer items", sourceUid)
        return nil, resolveReason(REASON_SOURCE_INELIGIBLE)
                    or REASON_CONTRACT_UNAVAILABLE
    end
    -- Page identity is the half of Unit.Transfer's 'reachable' rule a
    -- session can decide at creation; the other half (adjacency) is the
    -- escort's whole job and is deliberately NOT re-derived here. A
    -- cross-page pair is refused with 'out_of_range', the id that rule
    -- fails as, and refusing it HERE rather than leaving it to the
    -- commit is the point: the escort would simply never arrive, so the
    -- session would sit in `approaching` with nothing ever going wrong
    -- enough for the liveness tick to close it.
    if srcEp.page ~= info.page then
        engine.emitEventForUnit("unit_warning",
            "Cannot transfer: target is not in this world", sourceUid)
        return nil, resolveReason(REASON_OUT_OF_RANGE)
                    or REASON_CONTRACT_UNAVAILABLE
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
    -- It stays LAST of the source rules for the same reason: it is this
    -- module's own capability question, not one of the contract's, so
    -- every id the engine would produce for this pair is reported
    -- before the one only Mode A knows about.
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
        -- Same page on both sides, and since #1415 that is a CHECKED
        -- fact rather than an assumed one: the pair gate above refuses
        -- a cross-page creation outright, so the destination's resolved
        -- page IS the source's page for every session that exists. (It
        -- was always true of real player interaction -- hit-testing only
        -- ever matches entities on the currently visible world -- and
        -- the contract refuses a cross-page request at commit time
        -- anyway; what the check adds is that a caller reaching this
        -- boundary directly cannot make the record lie.)
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
        -- A Mode A session is an explicit player command, so it
        -- supersedes a standing position hold (#1216) on either unit
        -- exactly as commandMove / commandPickup / commandTransferOrder
        -- do -- otherwise the source would walk back to a stale anchor
        -- the moment the session ended. Read through package.loaded for
        -- the same reason nudgeUnit is: a headless UI fixture loads
        -- this module without the unit AI, and a session with no AI
        -- running has no hold to release.
        local unitAi = package.loaded["scripts.unit_ai"]
        if unitAi and type(unitAi.releaseHold) == "function" then
            pcall(unitAi.releaseHold, uid)
        end
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
-- Idempotent, non-throwing, and the same call for every path that ends
-- one: a closed panel, a replacement, an endpoint that vanished or was
-- incapacitated, a new player order, a view transition, Exit to Menu, a
-- save load. Each of those is a TRIGGER; none of them is a second
-- teardown, which is what keeps "no interruption leaves a unit held"
-- one fact to check rather than one per trigger.
--
-- Every step is isolated from the ones after it: the panels close
-- inside a pcall and each held unit is released inside its own, so an
-- endpoint that has already stopped resolving cannot cost the OTHER
-- endpoint its release or leave a panel on screen.
--
-- `opts.unitsAreStale` says the recorded uids no longer name the units
-- this session held, so the release below must not touch them. The
-- successful-load reset is the one caller: its hook fires after every
-- component has applied, and session-global entity ids are reused
-- across sessions, so `unit.stop(1)` there stops whatever the save
-- restored onto uid 1. Panels and identity are this module's own state
-- and go regardless.
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
function M.close(reason, opts)
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
    -- replacement, an endpoint that vanished or was incapacitated, a
    -- new player order, a HUD/zoom-band transition and Exit to Menu
    -- alike. Stopping is all this does to the target: it cancels no
    -- order and clears no goal, so a durable Mode B order the target is
    -- carrying survives the release and its executor simply re-issues
    -- the walk on its next tick.
    --
    -- The successful-load reset is the ONE path that skips this
    -- entirely, and skips it because its uids are stale rather than
    -- because a restored unit deserves less care -- see `opts` above.
    if not (opts and opts.unitsAreStale) then
        for _, uid in ipairs(heldUnits(s)) do
            pcall(function()
                unit.stop(uid)
                nudgeUnit(uid)
            end)
        end
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

-- Explicit teardown, kept under its historical name because the
-- session-teardown registration below speaks it (#1014 review round 1:
-- the save-load reset hook alone misses the Exit to Menu path).
function M.clear()
    M.close("cleared")
end

-- #1014, migrated onto the declared session-teardown boundary (#1610).
-- Exit to Menu is the OTHER session-replacement path, and M.init's reset
-- hook never sees it: that one fires only from saveModules.applyAll,
-- which only a load reaches. Unlike it, this runs while the old session
-- is still live (pauseMenu calls the boundary BEFORE world.destroyAll),
-- so it is the ORDINARY coupled teardown -- `unitsAreStale` is
-- deliberately not passed, because the uids this session recorded still
-- name the units it is holding and they must actually be stopped.
--
-- At module scope rather than in M.init because the pcall this replaces
-- `require`d the module unconditionally: a caller that has this module
-- loaded but never initialized still had its session cleared, and still
-- must.
require("scripts.lib.session_teardown").register("transfer_session",
    function() M.clear() end)

return M
