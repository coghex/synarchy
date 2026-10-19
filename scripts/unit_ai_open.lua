-- Unit AI ground-container Open order (#2528, epic #1231 PLC-18).
--
-- Action: open_ground. Player-ordered: right-click a ground container →
-- Open (unitAi.commandOpen). The unit walks to the crate where it lies,
-- the open resolves at the RESULT BOUNDARY below (design D-9), the
-- contents become player knowledge, and the crate's portable window
-- level opens (D-25). Nothing ever moves the crate.
--
-- The order is the pickup order's sibling and keeps every one of its
-- rules (scripts/unit_ai_pickup.lua): a persisted `s.openOrder =
-- { gid, issuedAt }`, a floating arrival range (`open_arrival_tiles`),
-- a closest-approach STALL budget (`open_timeout`, charged through
-- unit_ai_stall.lua on eligible time only and suspended by the same
-- boundary), and the #1666 page rules: every phase resolves the ground
-- entry on the CARRIER'S OWN page through item.getGroundForUnit, a page
-- that says "no such id" retires the order quietly, and a page that
-- cannot be resolved at all leaves it standing. It differs in two
-- places only: there is no weight check anywhere (nothing is lifted),
-- and only an item-CONTAINER -- a definition declaring `storage:`,
-- reported as the row's `hasStorage` -- can be opened.
--
-- Knowledge and window identity are the crate's stable INSTANCE id,
-- never the page-local gid: item.observeContainerContents records one
-- contents observation (contents and weight, each stamped with the
-- observation time), and item_contents_panel.openForGround pushes the
-- `portableItem` base level, which renders the player's REMEMBERED view
-- and refreshes itself once the queued observation lands.

local unitAi = package.loaded["scripts.unit_ai"]
local core = require("scripts.unit_ai_core")
local reportFailure = core.reportFailure
local distance       = core.distance
local ensureState    = core.ensureState

local mv = require("scripts.movement_speed")
local stall = require("scripts.unit_ai_stall")
local hold = require("scripts.unit_ai_hold")
-- The player-facing unit and item labels, shared rather than copied so
-- the two orders' warnings can never name things differently.
local pickup = require("scripts.unit_ai_pickup")
local unitLabel, itemLabel = pickup.unitLabel, pickup.itemLabel

-- How much closer the unit must get before its stall budget resets;
-- the pickup order's figure, for the pickup order's reason.
local OPEN_PROGRESS_TILES = 0.5

local M = {}

M.ACTION = "open_ground"

local function openGroundEntry(uid, gid)
    return item.getGroundForUnit(uid, gid)
end

local function isContainer(g)
    return g ~= nil and g.hasStorage == true
        and type(g.instanceId) == "number" and g.instanceId > 0
end

-- THE result boundary (D-9): the one place an arrived Open decides
-- whether it succeeds. Today it answers success for every ordinary
-- container; locks, traps and perception checks belong HERE and nowhere
-- else, so a future refusal reaches the player through the same branch
-- as every other outcome. Returns { ok = true } or
-- { ok = false, reason = "<player-facing text>" }.
function M.resolveOpen(uid, g)
    return { ok = true }
end

-- The arrived unit's whole success path, or the reason it stopped
-- short. Returns true when the order is finished (succeeded or
-- reported), false when it must stay standing (no page to ask).
local function completeOpen(uid, s, order, g, info)
    local result = M.resolveOpen(uid, g)
    if not result.ok then
        reportFailure(uid, string.format("%s can't open %s — %s",
            unitLabel(uid), itemLabel(g.defName), result.reason or "refused"))
        return true
    end
    if g.pending == true then
        if info.page == nil then return false end
        -- Realized on the carrier's own page, named explicitly -- never
        -- the active page, which may hold a different item under this
        -- gid. Exactly once in the engine: a repeat answers
        -- already-realized and rerolls nothing.
        if not item.realizeGround(order.gid, info.page) then
            local again, resolved = openGroundEntry(uid, order.gid)
            if again and again.pending == true then
                -- D-23: the cargo could not be drawn. Nothing observed.
                reportFailure(uid, string.format(
                    "%s can't open %s — its contents could not be determined",
                    unitLabel(uid), itemLabel(g.defName)))
                return true
            end
            return resolved and not again
        end
        local realized, resolved = openGroundEntry(uid, order.gid)
        if not realized then return resolved end
        g = realized
    end
    -- One contents observation, keyed by the stable instance id. A
    -- refusal (the crate is no longer locatable) is reported as such:
    -- an observation that did not happen is never presented as one.
    if not item.observeContainerContents(g.instanceId) then
        reportFailure(uid, string.format("%s can't open %s — it is out of reach",
            unitLabel(uid), itemLabel(g.defName)))
        return true
    end
    engine.emitEventForUnit("unit_event",
        unitLabel(uid) .. " opened " .. itemLabel(g.defName),
        uid, math.floor(g.x), math.floor(g.y))
    -- D-25: the crate's window, at the base of the stack, centred the
    -- way the escort's arrival opens its own (#1250).
    local fbW, fbH = engine.getFramebufferSize()
    require("scripts.item_contents_panel").openForGround(g.instanceId,
        math.floor((fbW or 0) * 0.5), math.floor((fbH or 0) * 0.5),
        g.displayName, g.defName)
    return true
end

local function openUtility(uid, s, params)
    local order = s.openOrder
    if not order then return -math.huge end
    local g, resolved = openGroundEntry(uid, order.gid)
    if not g then
        -- Gone from the carrier's OWN page: retire quietly. No page to
        -- ask: not an answer, so the order stands and nothing is charged.
        if resolved then s.openOrder = nil end
        return -math.huge
    end
    -- open_timeout is a STALL timer on eligible time, reset only by a
    -- new closest approach the order's own action makes (#920, #1291) --
    -- the pickup order's rule, read the same way.
    local now = engine.gameTime()
    local eligible = s.currentAction == M.ACTION
    local info = eligible and unit.getInfo(uid)
    if info then
        local d = distance(info.gridX, info.gridY, g.x, g.y)
        if not order.bestDist or d < order.bestDist - OPEN_PROGRESS_TILES then
            order.bestDist = d
            stall.reset(order, now)
        end
    end
    if stall.charge(order, eligible, now) > (params.open_timeout or 30) then
        reportFailure(uid, string.format(
            "Couldn't reach container to open — %s couldn't reach %s",
            unitLabel(uid), itemLabel(g.defName)))
        s.openOrder = nil
        return -math.huge
    end
    return params.pickup_utility
end

local function openExecute(uid, s, params)
    local order = s.openOrder
    if not order then return end
    local g, resolved = openGroundEntry(uid, order.gid)
    local info = unit.getInfo(uid)
    if not info then
        s.openOrder = nil
        return
    end
    if not g then
        if resolved then s.openOrder = nil end
        return
    end
    local d = distance(info.gridX, info.gridY, g.x, g.y)
    if d > (params.open_arrival_tiles or params.pickup_arrival_tiles) then
        unit.moveTo(uid, g.x, g.y, mv.comfort(uid))
        return
    end
    unit.stop(uid)
    if completeOpen(uid, s, order, g, info) then s.openOrder = nil end
end

-- Player order: send `uid` to open the ground container `gid` where it
-- lies. Returns true if accepted, false if refused up front.
--
-- Refused -- silently, as a CALLER error, with nothing written: no
-- order stored or replaced, any position hold left as it was (#1216),
-- nextActionAt untouched -- when `gid` names nothing on `uid`'s OWN page
-- (#1666), when that page cannot be resolved, when `pageId` (optional:
-- the page the caller resolved `gid` on) is not the unit's page, and
-- when the item is not an item-container. There is no weight check:
-- opening lifts nothing.
--
-- Deliberately NOT a player-only boundary: it does not end a Mode A
-- session. The player's own ingress (the ground menu) calls
-- transfer_session.notePlayerOrder before dispatching here, exactly as
-- every other player order does (#1254).
function unitAi.commandOpen(uid, gid, pageId)
    local g, resolved = openGroundEntry(uid, gid)
    if not g then
        engine.logWarn("commandOpen: ground item " .. tostring(gid)
            .. " is not on unit " .. tostring(uid) .. "'s own page"
            .. (resolved and "" or " (no live page for that unit)")
            .. " — order refused")
        return false
    end
    if pageId ~= nil and g.page ~= pageId then
        engine.logWarn("commandOpen: ground item " .. tostring(gid)
            .. " was named on page " .. tostring(pageId) .. " but unit "
            .. tostring(uid) .. " is on page " .. tostring(g.page)
            .. " — order refused")
        return false
    end
    if not isContainer(g) then
        engine.logWarn("commandOpen: ground item " .. tostring(gid)
            .. " is not a container — order refused")
        return false
    end
    local s = ensureState(uid)
    hold.clear(s)
    s.openOrder = { gid = gid, issuedAt = engine.gameTime() }
    s.nextActionAt = 0
    return true
end

M.isContainer = isContainer
M.action = { name = M.ACTION, utility = openUtility, execute = openExecute }

return M
