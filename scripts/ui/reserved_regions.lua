-- Gameplay HUD collision/priority contract (#750, UI hardening C4).
--
-- Screen-space reservation for the always-reachable gameplay chrome —
-- distinct from (and composes with) two mechanisms this module does
-- NOT re-model:
--   * uiLayerBand / UI.InputOwnership (src/UI/Types.hs,
--     src/UI/InputOwnership.hs) already make a modal outrank gameplay
--     and LayerDebug (shell/F8) pass through above everything, in both
--     paint order and pointer routing. "modals outrank gameplay" and
--     "debug/shell may overlay but pass through outside controls" are
--     ALREADY the layer-band contract; nothing here changes that.
--   * scripts/ui/context_menu.lua already places itself via #747's
--     UI.placePopup — "context menus use #747" needs no new code.
--
-- What genuinely IS new territory (per the #750 issue) is SPATIAL
-- collision between simultaneously-visible gameplay surfaces that
-- share no layer-band relationship at all (e.g. a centered
-- notification card vs. the bottom-left tool toggle). This module is
-- the single source of truth for that: a deterministic priority order,
-- a pure overlap/violation check any surface's real geometry can be
-- audited against, and a minimal-translation nudge helper a
-- lower-priority surface can use to avoid a reserved region instead of
-- covering it.
--
-- "unit info reserves right edge and suppresses conflicting info" is
-- ALREADY implemented (scripts/unit_info_v2.lua's update() calls
-- scripts/hud/info_panel.lua's suppress("unit_info_v2")/unsuppress
-- whenever the unit-info column wants to be visible) — this module
-- does not duplicate that named-suppressor mechanism, only exposes the
-- geometry so it can be audited alongside everything else.

local reservedRegions = {}

-- Deterministic priority order — higher number outranks lower. Only
-- surfaces that can plausibly occupy overlapping screen space belong
-- here; see the header comment for what's intentionally excluded.
reservedRegions.PRIORITY = {
    toolbar        = 100,  -- hud.getToolbarRects(): log/map/tool toggle clusters
    unit_info      = 90,   -- unit_info_v2's flush-right column
    transient_info = 50,   -- hud/info_panel.lua's generic tile/building/item pane
    notifications  = 10,   -- popup.lua's notification cards
}

function reservedRegions.rectsOverlap(a, b)
    if not a or not b then return false end
    return a.x < b.x + b.w and b.x < a.x + a.w
       and a.y < b.y + b.h and b.y < a.y + a.h
end

-- regions: array of {name, priority, rect}, rect = {x,y,w,h} or nil
-- (not currently visible — skipped). Returns one entry per overlapping
-- pair: {loser, winner, ambiguous}. `loser` is the strictly-lower-
-- priority region (the one that should yield); on a priority tie
-- `ambiguous` is true and `loser` is simply the later region in the
-- input array, since a same-priority overlap has no deterministic
-- winner by rank alone but is still worth flagging.
function reservedRegions.checkViolations(regions)
    local violations = {}
    for i = 1, #regions do
        for j = i + 1, #regions do
            local a, b = regions[i], regions[j]
            if a.rect and b.rect and reservedRegions.rectsOverlap(a.rect, b.rect) then
                local loser, winner, ambiguous = b, a, false
                if a.priority == b.priority then
                    ambiguous = true
                elseif a.priority > b.priority then
                    loser, winner = b, a
                else
                    loser, winner = a, b
                end
                table.insert(violations,
                    { loser = loser, winner = winner, ambiguous = ambiguous })
            end
        end
    end
    return violations
end

-- Minimal-translation nudge: moves `rect` off `reserved` along
-- whichever axis has the smaller overlap, pushing away from
-- `reserved`'s center. Returns a NEW rect; never mutates the input.
local function separate(rect, reserved)
    if not reservedRegions.rectsOverlap(rect, reserved) then return rect end
    local overlapX = math.min(rect.x + rect.w, reserved.x + reserved.w)
                    - math.max(rect.x, reserved.x)
    local overlapY = math.min(rect.y + rect.h, reserved.y + reserved.h)
                    - math.max(rect.y, reserved.y)
    local rectCx, rectCy = rect.x + rect.w / 2, rect.y + rect.h / 2
    local resCx,  resCy  = reserved.x + reserved.w / 2, reserved.y + reserved.h / 2
    local nx, ny = rect.x, rect.y
    if overlapX < overlapY then
        nx = rect.x + (rectCx >= resCx and overlapX or -overlapX)
    else
        ny = rect.y + (rectCy >= resCy and overlapY or -overlapY)
    end
    return { x = nx, y = ny, w = rect.w, h = rect.h }
end

-- Nudges `rect` clear of every rect in `reservedRects` (e.g.
-- hud.getToolbarRects()), then clamps to [0,screenW]x[0,screenH] when
-- given. Deterministic; passes are order-dependent, so callers with
-- more than one plausible conflict should list higher-priority regions
-- first.
function reservedRegions.avoidReserved(rect, reservedRects, screenW, screenH)
    local out = { x = rect.x, y = rect.y, w = rect.w, h = rect.h }
    for _, reserved in ipairs(reservedRects or {}) do
        out = separate(out, reserved)
    end
    if screenW then
        out.x = math.max(0, math.min(out.x, math.max(0, screenW - out.w)))
    end
    if screenH then
        out.y = math.max(0, math.min(out.y, math.max(0, screenH - out.h)))
    end
    return out
end

-- Bounds-escape / unreachable-action detection (#750 acceptance:
-- introspection "detects escapes ... unreachable actions"). `elements`
-- is an array of UI.getElementInfo-shaped tables. Only a genuinely
-- reachable-if-on-screen control is considered — one that's both
-- currently shown (own `visible` AND its page's `pageVisible`) and a
-- real pointer target (`pointerBlocking`); a bare decorative sprite
-- drifting off-screen isn't an "unreachable ACTION". Any part hanging
-- outside [0,fbW]x[0,fbH] is enough to flag — a partially clipped edge
-- can still be unclickable, and this is a conservative audit, not a
-- hit-test.
function reservedRegions.findEscapes(elements, fbW, fbH)
    local escapes = {}
    for _, e in ipairs(elements or {}) do
        if e.visible ~= false and e.pageVisible ~= false and e.pointerBlocking then
            local outside = e.x < 0 or e.y < 0
                or (e.x + e.width)  > fbW
                or (e.y + e.height) > fbH
            if outside then
                table.insert(escapes, e)
            end
        end
    end
    return escapes
end

return reservedRegions
