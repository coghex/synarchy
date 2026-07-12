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
        "assets/textures/utility/white.png")

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

-- Arm the drag. Forward-only: called from game.onMouseDown (init.lua)
-- AFTER the ordered tool/overlay claim guards (debug overlay, debug
-- anim panel, build tool, mine tool) have each had their crack and
-- returned early on a consumed click. That single ordered decision is
-- the only thing that arms drag-select now, so a click already eaten by
-- one of those handlers can no longer start a background box-selection
-- (#114).
--
-- Named handle* (not on*) deliberately: this module is engine-loaded
-- (loadScript), so an on*-named function would ALSO fire on every engine
-- broadcast — independent of the ordered guards above, which was the
-- bug. handle* keeps it forward-only (same convention as build_tool /
-- mine_tool / debug.lua's tryClaimClick).
function dragSelect.handleMouseDown(button, x, y)
    if button ~= 1 then return end

    dragSelect.state  = "pressed"
    dragSelect.startX = x
    dragSelect.startY = y
    dragSelect.currX  = x
    dragSelect.currY  = y
    dragSelect.pendingClick = nil
end

-- F4 (#730) Layer A: a drag-select box's real outcome can only be
-- known at release (hitTestInRect against the final rect). Kind-
-- distinct from "input.click" ("input.drag") so a completed drag's
-- ONE record describes the box selection itself rather than the
-- press-time click classification init_mouse.lua computed before any
-- drag threshold had even been crossed — that press-time record is
-- deferred (see deferClick/pendingClick below) rather than recorded
-- immediately, precisely so a real drag doesn't ALSO carry it as a
-- second, misleading "input.click" record (review: exactly one
-- primary Layer A record per H1 action). Only fires once the drag
-- actually reached "dragging" (crossed DRAG_THRESHOLD).
local function recordDragOutcome(outcome, x, y, requested, applied, reason)
    debug.recordOutcome{
        kind = "input.drag",
        outcome = outcome,
        where = { x = x, y = y },
        handler = "unit_drag_select",
        requested = requested,
        applied = applied,
        reason = reason,
    }
end

-- F4 (#730): mirrors init_mouse.lua's own recordClick shape exactly,
-- so a deferred click reads identically to one recorded immediately —
-- only ever called with a real dragSelect.pendingClick, once the
-- gesture is known to have stayed a plain click (never reached
-- "dragging").
local function recordDeferredClick(pc)
    debug.recordOutcome{
        kind = "input.click",
        outcome = pc.outcome or "accepted",
        where = { x = pc.x, y = pc.y },
        handler = pc.handler,
        reason = pc.reason,
    }
end

-- Store this press's click classification instead of recording it
-- immediately (#730). Called by init_mouse.lua's onMouseDown chain for
-- every drag-eligible press (unit/item/building selection or
-- deselect) — whichever of onMouseUp / cancel below resolves this
-- gesture is responsible for eventually recording it exactly once (a
-- real drag drops it in favor of its own "input.drag" record instead).
-- The SELECTION EFFECT (unit.select etc.) is unaffected — it still
-- runs immediately in init_mouse.lua, at press, exactly as before;
-- only the F4 record's timing/existence moves.
function dragSelect.deferClick(handler, outcome, x, y, reason)
    dragSelect.pendingClick = { handler = handler, outcome = outcome,
                                 x = x, y = y, reason = reason }
end

function dragSelect.onMouseUp(button, x, y, downRoute)
    if button ~= 1 then return end
    local wasDragging = (dragSelect.state == "dragging")
    local wasPressed   = (dragSelect.state == "pressed")
    if wasDragging then
        -- A focus-loss / minimize transition arrives as a synthetic
        -- release routed "swallowed" (Engine.Input.Thread). That cancels
        -- the drag: tear the box down without committing a selection at
        -- the stale last-cursor position. A real release commits.
        if downRoute ~= "swallowed" then
            local ids = unit.hitTestInRect(
                dragSelect.startX, dragSelect.startY, x, y) or {}
            local final
            if isShiftHeld() then
                local current = unit.getSelected() or {}
                final = mergeIds(current, ids)
            else
                final = ids
            end
            unit.setSelection(final)
            -- A drag that establishes a unit selection must clear the
            -- item/building domains: ground-item selection is mutually
            -- exclusive with unit/building selection (World.Cursor.Types),
            -- enforced by the click routing. The box can start over an
            -- item/building, so clear the other domains whenever we end
            -- up with units selected — matching scripts/init.lua.
            if #final > 0 then
                item.deselect()
                building.deselect()
            end
            recordDragOutcome(#final > 0 and "accepted" or "noop",
                x, y, #ids, #final)
        else
            recordDragOutcome("noop", x, y, 0, 0,
                "release swallowed (focus loss / minimize)")
        end
        setEdgesVisible(false)
    elseif wasPressed then
        -- Never crossed the drag threshold — this gesture is really
        -- just a click. Fire init_mouse.lua's deferred classification
        -- now that it's known to be the gesture's final (and only)
        -- outcome (#730 — keeps a below-threshold "drag" H1 action to
        -- exactly one record, same as a real click).
        if dragSelect.pendingClick then
            recordDeferredClick(dragSelect.pendingClick)
        end
    end
    dragSelect.pendingClick = nil
    dragSelect.state = "idle"
end

-- Idempotent teardown of an in-progress drag. Drops back to "idle" and
-- hides the rect overlay WITHOUT committing a selection — the box is
-- abandoned, not finished. Safe to call when already idle (no-op).
--
-- Called on view transitions (hud.reconcileView on zoom, hud.hide on
-- menu): the overlay lives on its own "drag_select_overlay" page, so
-- neither the world/zoom page swap nor the HUD-page hide touches it.
-- Without this, an armed/dragging box survives the transition and could
-- resume or commit later against the wrong view (#146).
--
-- F4 (#730): also the one path that resolves a gesture abandoned mid-
-- flight (no mouse-up ever arrives) — flushes whatever it was heading
-- toward (a deferred click if still "pressed", a cancelled-drag "noop"
-- if already "dragging") rather than silently dropping it, since
-- dragSelect.onMouseUp will never get a chance to.
function dragSelect.cancel()
    if dragSelect.state == "idle" then return end
    if dragSelect.state == "dragging" then
        recordDragOutcome("noop", dragSelect.startX, dragSelect.startY, 0, 0,
            "cancelled (view transition)")
    elseif dragSelect.pendingClick then
        recordDeferredClick(dragSelect.pendingClick)
    end
    if dragSelect.edgeIds then
        setEdgesVisible(false)
    end
    dragSelect.pendingClick = nil
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
