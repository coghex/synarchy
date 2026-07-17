-- Generic keyboard CONTROL FOCUS visual indicator (#745 review round
-- 6/7). Four thin edge sprites recreated and re-parented onto whatever
-- element currently holds upmControlFocus, so EVERY focusable control
-- family (checkbox/toggle/tab/list/dropdown/slider knob/hand-built
-- controls, not just button.lua's own reference implementation) gets
-- a visible focus state with no per-family wiring. Reuses
-- assets/textures/ui/highlight.png — the same flat-fill placeholder
-- scripts/ui/slider.lua already uses for its knob's hover highlight —
-- as a functional stand-in for a dedicated focus-ring texture; a
-- distinct asset is a follow-up art pass, same convention as this
-- epic's other placeholder assets.
--
-- Round 6 drew this as ONE sprite stretched over the control's full
-- bounding box, which at alpha 0.85 opaquely painted over the
-- control's own content/label instead of ringing it (review round 7).
-- Four thin strips (top/bottom/left/right) traced just outside the
-- control's edges leave the interior completely unobscured.
--
-- The strips are destroyed and recreated on every focus change rather
-- than reparented in place: UI.addChild appends to the NEW parent's
-- child list without removing the element from any PREVIOUS parent's
-- list (there is no generic "remove one child" primitive), so
-- repeatedly reparenting the same handles would leave stale entries
-- behind. Fresh elements every time sidestep that entirely.
local M = package.loaded["scripts.ui.focus_indicator"] or {}
package.loaded["scripts.ui.focus_indicator"] = M

local FOCUS_COLOR = {1.0, 0.85, 0.2, 0.85}
local RING_MARGIN = 2
local RING_THICKNESS = 2

local texHighlight = nil
local ringIds = nil -- {top, bottom, left, right} or nil

function M.init()
    if texHighlight then return end
    texHighlight = engine.loadTexture("assets/textures/ui/highlight.png")
end

local function destroyRing()
    if not ringIds then return end
    for _, id in ipairs(ringIds) do
        UI.deleteElement(id)
    end
    ringIds = nil
end

-- Exposed for tests (#745 review round 7: "cover the indicator
-- lifecycle") — the current ring element ids, or nil when no control
-- is focused.
function M.getRingIds()
    return ringIds
end

function M.onUIControlFocusChanged(elemHandle)
    destroyRing()
    if not elemHandle or not texHighlight then return end

    local info = UI.getElementInfo(elemHandle)
    if not info then return end

    local r, g, b, a = FOCUS_COLOR[1], FOCUS_COLOR[2], FOCUS_COLOR[3], FOCUS_COLOR[4]
    -- Bounding box of the ring itself (control size plus the outward
    -- margin on every side); edges are placed relative to the
    -- CONTROL's own local origin (0,0), matching UI.addChild's
    -- parent-relative positioning below.
    local w = info.width + RING_MARGIN * 2
    local h = info.height + RING_MARGIN * 2

    local topId = UI.newSprite("control_focus_ring_top", w, RING_THICKNESS, texHighlight, r, g, b, a, 0)
    local bottomId = UI.newSprite("control_focus_ring_bottom", w, RING_THICKNESS, texHighlight, r, g, b, a, 0)
    local leftId = UI.newSprite("control_focus_ring_left", RING_THICKNESS, h, texHighlight, r, g, b, a, 0)
    local rightId = UI.newSprite("control_focus_ring_right", RING_THICKNESS, h, texHighlight, r, g, b, a, 0)
    if not (topId and bottomId and leftId and rightId) then
        for _, id in ipairs({ topId, bottomId, leftId, rightId }) do
            if id then UI.deleteElement(id) end
        end
        return
    end

    -- Reparenting onto the focused element itself (rather than its
    -- page) means every strip automatically inherits the SAME page and
    -- z-stacking context — no separate page/modal-boundary bookkeeping
    -- needed here at all.
    UI.addChild(elemHandle, topId, -RING_MARGIN, -RING_MARGIN)
    UI.addChild(elemHandle, bottomId, -RING_MARGIN, h - RING_MARGIN - RING_THICKNESS)
    UI.addChild(elemHandle, leftId, -RING_MARGIN, -RING_MARGIN)
    UI.addChild(elemHandle, rightId, w - RING_MARGIN - RING_THICKNESS, -RING_MARGIN)

    UI.setZIndex(topId, 999)
    UI.setZIndex(bottomId, 999)
    UI.setZIndex(leftId, 999)
    UI.setZIndex(rightId, 999)

    ringIds = { topId, bottomId, leftId, rightId }
end

return M
