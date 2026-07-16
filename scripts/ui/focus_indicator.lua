-- Generic keyboard CONTROL FOCUS visual indicator (#745 review round
-- 6). A single reusable overlay sprite, recreated and re-parented onto
-- whatever element currently holds upmControlFocus, so EVERY
-- focusable control family (checkbox/toggle/tab/list/dropdown/slider
-- knob/hand-built controls, not just button.lua's own reference
-- implementation) gets a visible focus state with no per-family
-- wiring. Reuses assets/textures/ui/highlight.png — the same flat-
-- fill placeholder scripts/ui/slider.lua already uses for its knob's
-- hover highlight — as a functional stand-in for a dedicated focus
-- ring; a distinct texture is a follow-up art pass, same convention
-- as this epic's other placeholder assets.
--
-- The overlay is destroyed and recreated on every focus change rather
-- than reparented in place: UI.addChild appends to the NEW parent's
-- child list without removing the element from any PREVIOUS parent's
-- list (there is no generic "remove one child" primitive), so
-- repeatedly reparenting the same handle would leave stale entries
-- behind. A fresh element every time sidesteps that entirely.
local M = package.loaded["scripts.ui.focus_indicator"] or {}
package.loaded["scripts.ui.focus_indicator"] = M

local FOCUS_COLOR = {1.0, 0.85, 0.2, 0.85}
local RING_MARGIN = 2

local texHighlight = nil
local ringId = nil

function M.init()
    if texHighlight then return end
    texHighlight = engine.loadTexture("assets/textures/ui/highlight.png")
end

function M.onUIControlFocusChanged(elemHandle)
    if ringId then
        UI.deleteElement(ringId)
        ringId = nil
    end
    if not elemHandle or not texHighlight then return end

    local info = UI.getElementInfo(elemHandle)
    if not info then return end

    ringId = UI.newSprite(
        "control_focus_ring",
        info.width + RING_MARGIN * 2,
        info.height + RING_MARGIN * 2,
        texHighlight,
        FOCUS_COLOR[1], FOCUS_COLOR[2], FOCUS_COLOR[3], FOCUS_COLOR[4],
        0
    )
    if not ringId then return end
    -- Reparenting onto the focused element itself (rather than its
    -- page) means the ring automatically inherits the SAME page and
    -- z-stacking context — no separate page/modal-boundary bookkeeping
    -- needed here at all.
    UI.addChild(elemHandle, ringId, -RING_MARGIN, -RING_MARGIN)
    UI.setZIndex(ringId, 999)
end

return M
