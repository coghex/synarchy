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

local function clampRect(rect, screenW, screenH)
    local out = { x = rect.x, y = rect.y, w = rect.w, h = rect.h }
    if screenW then
        out.x = math.max(0, math.min(out.x, math.max(0, screenW - out.w)))
    end
    if screenH then
        out.y = math.max(0, math.min(out.y, math.max(0, screenH - out.h)))
    end
    return out
end

-- Minimal-translation nudge: moves `rect` off `reserved`, trying the 4
-- candidate directions (flush against reserved's near edge on each
-- side) smallest-push-first, clamping each candidate to the screen and
-- keeping the FIRST one that's actually clear once clamped — not just
-- whichever axis had the smaller raw overlap (#750 round-4 review: that
-- shortcut misfires whenever `reserved`'s own span fully CONTAINS
-- `rect`'s span on one axis, e.g. a toolbar column spanning nearly the
-- whole screen height — pushing by the "overlap" amount in that case
-- doesn't reach clear ground at all, and a later screen clamp could
-- shove `rect` right back into `reserved`). Falls back to the smallest-
-- push candidate, clamped, if every candidate still overlaps once
-- clamped (a genuinely infeasible placement — `rect` doesn't fit
-- anywhere clear of `reserved` within the screen at all; best-effort,
-- not guaranteed clear, same contract as everywhere else in this
-- module). Returns a NEW rect; never mutates the input.
local function separate(rect, reserved, screenW, screenH)
    if not reservedRegions.rectsOverlap(rect, reserved) then return rect end
    local candidates = {
        { axis = "x", delta = reserved.x - (rect.x + rect.w) },        -- push left
        { axis = "x", delta = (reserved.x + reserved.w) - rect.x },    -- push right
        { axis = "y", delta = reserved.y - (rect.y + rect.h) },        -- push up
        { axis = "y", delta = (reserved.y + reserved.h) - rect.y },    -- push down
    }
    table.sort(candidates, function(a, b) return math.abs(a.delta) < math.abs(b.delta) end)

    local fallback = nil
    for _, c in ipairs(candidates) do
        local moved = { x = rect.x, y = rect.y, w = rect.w, h = rect.h }
        if c.axis == "x" then moved.x = rect.x + c.delta else moved.y = rect.y + c.delta end
        local clamped = clampRect(moved, screenW, screenH)
        if not fallback then fallback = clamped end
        if not reservedRegions.rectsOverlap(clamped, reserved) then
            return clamped
        end
    end
    return fallback
end

-- Nudges `rect` clear of every rect in `reservedRects` (e.g.
-- hud.getToolbarRects()), clamping to [0,screenW]x[0,screenH] (when
-- given) after each individual separation so a later screen clamp can
-- never blindly undo an earlier one. Deterministic; passes are order-
-- dependent, so callers with more than one plausible conflict should
-- list higher-priority regions first.
function reservedRegions.avoidReserved(rect, reservedRects, screenW, screenH)
    local out = clampRect(rect, screenW, screenH)
    for _, reserved in ipairs(reservedRects or {}) do
        out = separate(out, reserved, screenW, screenH)
    end
    return out
end

-- #750 round-4 review: capping a card's width to the framebuffer alone
-- (as popup.lua/unit_info_v2.lua both do) isn't enough when a reserved
-- region spans nearly the full height at the card's own vertical
-- position — no amount of x-nudging then fits it beside that region.
-- Given a vertical span [y, y+h) and a list of reserved rects, returns
-- the widest horizontal gap within [0, screenW) not covered by any
-- reserved rect whose OWN vertical span intersects [y, y+h) — i.e. the
-- widest a rect placed at exactly this y could be without overlapping
-- any of them, without needing to move vertically at all. A reserved
-- rect whose span doesn't reach this y range is not a blocker here.
function reservedRegions.maxAvailableWidth(y, h, reservedRects, screenW)
    local blockers = {}
    for _, r in ipairs(reservedRects or {}) do
        if r.y < y + h and y < r.y + r.h then
            table.insert(blockers,
                { x = math.max(0, r.x), x2 = math.min(screenW, r.x + r.w) })
        end
    end
    table.sort(blockers, function(a, b) return a.x < b.x end)
    local best = 0
    local cursor = 0
    for _, b in ipairs(blockers) do
        if b.x > cursor then best = math.max(best, b.x - cursor) end
        cursor = math.max(cursor, b.x2)
    end
    best = math.max(best, screenW - cursor)
    return best
end

-- #750 round-8 review: maxAvailableWidth above finds the widest gap
-- ANYWHERE — fine for a freely-repositionable rect like a popup, but
-- unit_info_v2's flush-right column is never repositioned, only
-- resized; capping it to the framebuffer alone (as it originally was)
-- lets it grow wide enough to cover reserved regions nowhere NEAR its
-- right edge. Widest a RIGHT-EDGE-ANCHORED rect spanning [y, y+h)
-- could be — scanning inward from screenW — without overlapping any
-- reserved rect whose own vertical span intersects [y, y+h). A blocker
-- entirely clear of the anchored span (its right edge already left of
-- where a candidate width would start) doesn't constrain the result.
function reservedRegions.maxRightAnchoredWidth(y, h, reservedRects, screenW)
    local maxW = screenW
    for _, r in ipairs(reservedRects or {}) do
        if r.y < y + h and y < r.y + r.h then
            maxW = math.min(maxW, screenW - (r.x + r.w))
        end
    end
    return math.max(0, maxW)
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
