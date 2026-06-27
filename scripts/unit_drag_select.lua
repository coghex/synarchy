-- Drag-box unit selection
--
-- State machine over mouse events:
--   idle    : nothing pressed
--   pressed : LMB down, waiting to see if it's a click or a drag.
--             Click handlers (game.onMouseDown) have already fired
--             selection logic for the down-press; we just observe.
--   dragging: mouse moved past `DRAG_THRESHOLD` since press. A
--             translucent selection rect is drawn and follows the
--             mouse. On release we replace (or, with Shift, add to)
--             the selection with the units whose center is inside.
--
-- Coordinate notes: mouse events deliver window-pixel coords; the
-- selection rect is rendered as a UI element which lives in
-- framebuffer-pixel coords. We store window coords and scale them
-- to FB at render and at the Haskell hit-test boundary.
--
-- The module self-registers in package.loaded so engine.loadScript
-- and require see the same instance — same reason as scripts/debug.lua.

local dragSelect = package.loaded["scripts.unit_drag_select"] or {}
package.loaded["scripts.unit_drag_select"] = dragSelect

local hud = require("scripts.hud")

dragSelect.state    = "idle"
dragSelect.startX   = 0
dragSelect.startY   = 0
dragSelect.currX    = 0
dragSelect.currY    = 0
dragSelect.page     = nil
-- 4 thin sprites for the rect outline (top / bottom / left / right).
-- Filled center stays transparent so units underneath remain visible.
dragSelect.edgeIds  = nil
dragSelect.whiteTex = nil

-- Pixels of mouse motion before we commit to a drag (window-space).
local DRAG_THRESHOLD = 4

-- Outline color (white) and thickness in framebuffer pixels.
local EDGE_R, EDGE_G, EDGE_B, EDGE_A = 1.0, 1.0, 1.0, 1.0
local EDGE_THICKNESS = 2

local function isShiftHeld()
    return engine.isKeyDown("LeftShift") or engine.isKeyDown("RightShift")
end

-- Merge two ID arrays into a deduped array.
local function mergeIds(a, b)
    local seen, out = {}, {}
    for _, id in ipairs(a) do
        if not seen[id] then seen[id] = true; table.insert(out, id) end
    end
    for _, id in ipairs(b) do
        if not seen[id] then seen[id] = true; table.insert(out, id) end
    end
    return out
end

function dragSelect.init(scriptId)
    engine.logInfo("Drag-select initializing...")
    -- 1×1 solid-white texture. Each edge sprite tints it white at
    -- full alpha; the resulting strips are uniformly colored without
    -- any pattern from a stretched chrome texture.
    dragSelect.whiteTex = engine.loadTexture(
        "assets/textures/hud/utility/white.png")

    dragSelect.page = UI.newPage("drag_select_overlay", "overlay")
    dragSelect.edgeIds = {}
    -- top, bottom, left, right
    for _, name in ipairs({"top", "bottom", "left", "right"}) do
        local id = UI.newSprite(
            "drag_select_" .. name, 1, 1, dragSelect.whiteTex,
            EDGE_R, EDGE_G, EDGE_B, EDGE_A,
            dragSelect.page)
        UI.addToPage(dragSelect.page, id, 0, 0)
        UI.setZIndex(id, 999)
        UI.setVisible(id, false)
        dragSelect.edgeIds[name] = id
    end
    UI.showPage(dragSelect.page)
end

local function setEdgesVisible(visible)
    for _, id in pairs(dragSelect.edgeIds) do
        UI.setVisible(id, visible)
    end
end

-- Sync the 4 edge sprites to the current drag bounds. Called every
-- tick while dragging.
local function updateRectVisual()
    local ww, wh = engine.getWindowSize()
    local fbW, fbH = engine.getFramebufferSize()
    local scaleX = (ww and ww > 0) and (fbW / ww) or 1
    local scaleY = (wh and wh > 0) and (fbH / wh) or 1
    local x1 = math.min(dragSelect.startX, dragSelect.currX) * scaleX
    local y1 = math.min(dragSelect.startY, dragSelect.currY) * scaleY
    local x2 = math.max(dragSelect.startX, dragSelect.currX) * scaleX
    local y2 = math.max(dragSelect.startY, dragSelect.currY) * scaleY
    local w = math.max(1, x2 - x1)
    local h = math.max(1, y2 - y1)
    local t = EDGE_THICKNESS
    -- top
    UI.setPosition(dragSelect.edgeIds.top,    x1, y1)
    UI.setSize    (dragSelect.edgeIds.top,    w,  t)
    -- bottom
    UI.setPosition(dragSelect.edgeIds.bottom, x1, y2 - t)
    UI.setSize    (dragSelect.edgeIds.bottom, w,  t)
    -- left
    UI.setPosition(dragSelect.edgeIds.left,   x1, y1)
    UI.setSize    (dragSelect.edgeIds.left,   t,  h)
    -- right
    UI.setPosition(dragSelect.edgeIds.right,  x2 - t, y1)
    UI.setSize    (dragSelect.edgeIds.right,  t,  h)
end

function dragSelect.update(dt)
    if dragSelect.state == "idle" then return end

    local mx, my = engine.getMousePosition()
    if not mx then return end
    dragSelect.currX = mx
    dragSelect.currY = my

    if dragSelect.state == "pressed" then
        local dx = mx - dragSelect.startX
        local dy = my - dragSelect.startY
        if dx * dx + dy * dy >= DRAG_THRESHOLD * DRAG_THRESHOLD then
            dragSelect.state = "dragging"
            setEdgesVisible(true)
            -- The press might have triggered a stray tile-cursor
            -- select via hud.onMouseDown. Now that we know it was a
            -- drag, undo that so we don't leave a tile selected
            -- behind the box.
            if world.clearWorldCursorSelect and hud.worldId then
                world.clearWorldCursorSelect(hud.worldId)
            end
        end
    end

    if dragSelect.state == "dragging" then
        updateRectVisual()
    end
end

function dragSelect.onMouseDown(button, x, y)
    if button ~= 1 then return end
    -- Defer to debug overlay's parallel hit-test; if it claims the
    -- click, we don't start a drag from a debug UI element.
    local debugOverlay = require("scripts.debug")
    if debugOverlay.tryClaimClick(button, x, y) then return end

    dragSelect.state  = "pressed"
    dragSelect.startX = x
    dragSelect.startY = y
    dragSelect.currX  = x
    dragSelect.currY  = y
end

function dragSelect.onMouseUp(button, x, y, downRoute)
    if button ~= 1 then return end
    local wasDragging = (dragSelect.state == "dragging")
    if wasDragging then
        -- A focus-loss / minimize transition arrives as a synthetic
        -- release routed "swallowed" (Engine.Input.Thread). That cancels
        -- the drag: tear the box down without committing a selection at
        -- the stale last-cursor position. A real release commits.
        if downRoute ~= "swallowed" then
            local ids = unit.hitTestInRect(
                dragSelect.startX, dragSelect.startY, x, y) or {}
            if isShiftHeld() then
                local current = unit.getSelected() or {}
                unit.setSelection(mergeIds(current, ids))
            else
                unit.setSelection(ids)
            end
        end
        setEdgesVisible(false)
    end
    dragSelect.state = "idle"
end

function dragSelect.shutdown()
    if dragSelect.edgeIds then
        for _, id in pairs(dragSelect.edgeIds) do
            UI.deleteElement(id)
        end
        dragSelect.edgeIds = nil
    end
    if dragSelect.page then
        UI.hidePage(dragSelect.page)
        UI.deletePage(dragSelect.page)
        dragSelect.page = nil
    end
end

-- Lets game.onMouseDown skip its own single-unit click logic when a
-- drag is in progress (drag-select is going to set the selection on
-- mouse-up anyway, so we don't want a click-select to flash first).
function dragSelect.isDragging()
    return dragSelect.state == "dragging"
end

return dragSelect
