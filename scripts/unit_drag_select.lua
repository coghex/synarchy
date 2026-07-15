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
-- to FB at render and at the Haskell hit-test boundary. The F4
-- (#774) action-outcome records this module pushes are the one
-- other place that needs framebuffer coords — see toFbCoords below —
-- so they share F1/F2/F3's oracle coordinate space.
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
-- F4 (#730 review round 6): dragSelect.handleMouseDown now arms
-- click-vs-drag CLASSIFICATION for every left-button press reaching
-- init_mouse.lua's onMouseDown — debug overlay/anim panel, build/
-- mine/chop/till/plant tool claims, and the gameplay-inactive
-- deadclick, not just the "no tool claimed it" unit/item/building
-- select-or-deselect fallback. Only THAT fallback has real
-- box-selection behavior (the visual rect, hitTestInRect commit,
-- world.clearWorldCursorSelect) — boxSelectArmed opts a press into
-- it explicitly (see armBoxSelect); every other press still gets
-- correct classification (exactly one "input.click" or "input.drag"),
-- just without the box-select-specific effects, which would be
-- meaningless (or actively wrong — a selection rect flashing while
-- dragging a build-tool placement) for them.
dragSelect.boxSelectArmed = false
-- F4 (#730 review round 4): a right-button press through
-- init_mouse.lua's context-menu/move-order/deadclick chain has no
-- box-selection behavior of its own, but an H1 `drag` action can
-- specify button="right" just as easily as "left" — this parallel,
-- visual-less state tracks ONLY whether such a press crossed
-- DRAG_THRESHOLD before release, so its deferred click classification
-- (see deferClick/onMouseUp) can still resolve to exactly one
-- "input.click" or "input.drag" record. "idle" | "pressed" | "dragging".
dragSelect.rightState        = "idle"
dragSelect.rightStartX       = 0
dragSelect.rightStartY       = 0
dragSelect.rightPendingClick = nil
-- 4 thin sprites for the rect outline (top / bottom / left / right).
-- Filled center stays transparent so units underneath remain visible.
dragSelect.edgeIds  = nil
dragSelect.whiteTex = nil

-- Pixels of mouse motion before we commit to a drag (window-space).
local DRAG_THRESHOLD = 4

-- F4 (#730 review round 5): the click-vs-drag CLASSIFICATION must be
-- computed directly from press vs. release coordinates, not derived
-- from dragSelect.state — state's "pressed" -> "dragging" transition
-- only happens inside dragSelect.update(dt), which runs on the
-- script's periodic 0.03s tick (scripts/init_loader.lua), not on
-- every mouse event. A fast real drag — or an H1 mouseDown/moveMouse/
-- mouseUp sequence — can complete entirely within one tick interval,
-- so onMouseUp could see state still "pressed" despite the gesture
-- having genuinely crossed DRAG_THRESHOLD, misclassifying a real drag
-- as a click (and skipping the box-selection commit too). This helper
-- is the single source of truth both onMouseUp and update() use.
local function pastThreshold(startX, startY, x, y)
    local dx = x - startX
    local dy = y - startY
    return (dx * dx + dy * dy) >= DRAG_THRESHOLD * DRAG_THRESHOLD
end

-- Outline color (white) and thickness in framebuffer pixels.
local EDGE_R, EDGE_G, EDGE_B, EDGE_A = 1.0, 1.0, 1.0, 1.0
local EDGE_THICKNESS = 2

local function isShiftHeld()
    return engine.isKeyDown("LeftShift") or engine.isKeyDown("RightShift")
end

-- F4 (#774): the recorded Layer-A `where` must share F1/F2/F3's
-- framebuffer-pixel oracle space, not the window coords mouse events
-- deliver (see the coordinate note at the top of this file). Box-select
-- geometry (pastThreshold, startX/startY/currX/currY, updateRectVisual's
-- own scale) stays in window space untouched — only the recorded
-- location converts, mirroring updateRectVisual's own degenerate
-- fallback (scale 1 when the window size isn't usable).
local function toFbCoords(x, y)
    local ww, wh = engine.getWindowSize()
    local fbW, fbH = engine.getFramebufferSize()
    if not ww or ww <= 0 or not wh or wh <= 0 then
        return x, y
    end
    return x * (fbW / ww), y * (fbH / wh)
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
    -- Box-select visuals/effects only ever apply to a boxSelectArmed
    -- press (#730 review round 6) — a non-armed press (debug/build/
    -- mine/chop/till/plant tool claims, a menu-background deadclick)
    -- never transitions to "dragging" here at all; its click-vs-drag
    -- F4 classification is computed directly from coordinates in
    -- onMouseUp regardless (round 5), so it needs nothing from this
    -- periodic tick.
    if dragSelect.state == "pressed" and dragSelect.boxSelectArmed then
        local mx, my = engine.getMousePosition()
        if mx then
            dragSelect.currX = mx
            dragSelect.currY = my
            if pastThreshold(dragSelect.startX, dragSelect.startY, mx, my) then
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
    elseif dragSelect.state == "dragging" then
        local mx, my = engine.getMousePosition()
        if mx then
            dragSelect.currX = mx
            dragSelect.currY = my
            updateRectVisual()
        end
    end

    -- Right-button threshold tracking only (#730 review round 4) — no
    -- visual, no box-select effect, purely for the deferred F4
    -- click-vs-drag classification below.
    if dragSelect.rightState == "pressed" then
        local mx, my = engine.getMousePosition()
        if mx and pastThreshold(dragSelect.rightStartX, dragSelect.rightStartY, mx, my) then
            dragSelect.rightState = "dragging"
        end
    end
end

-- Arm click-vs-drag CLASSIFICATION tracking. Called from game.onMouseDown
-- (init_mouse.lua) at the very TOP, before ANY of the ordered tool/
-- overlay claim guards (#730 review round 6 — every one of those
-- claims, plus the gameplay-inactive deadclick, needs its H1 `drag`
-- outcome classified too, not just the "no tool claimed it" fallback).
-- Box-selection's own EFFECT (the visual rect, hitTestInRect commit) is
-- separately opt-in via armBoxSelect, called only by that one fallback
-- path (#114's original ordering restriction — a click already eaten by
-- an overlay/tool guard still can't start a background box-selection —
-- is preserved by armBoxSelect's OWN placement, not by this function).
--
-- Named handle* (not on*) deliberately: this module is engine-loaded
-- (loadScript), so an on*-named function would ALSO fire on every engine
-- broadcast — independent of the ordered guards above, which was the
-- bug. handle* keeps it forward-only (same convention as build_tool /
-- mine_tool / debug.lua's tryClaimClick).
function dragSelect.handleMouseDown(button, x, y)
    if button == 1 then
        dragSelect.state  = "pressed"
        dragSelect.startX = x
        dragSelect.startY = y
        dragSelect.currX  = x
        dragSelect.currY  = y
        dragSelect.pendingClick    = nil
        dragSelect.boxSelectArmed  = false
    elseif button == 2 then
        dragSelect.rightState        = "pressed"
        dragSelect.rightStartX       = x
        dragSelect.rightStartY       = y
        dragSelect.rightPendingClick = nil
    end
end

-- Opts THIS press into box-selection's visual/commit behavior — called
-- only by init_mouse.lua's "no tool claimed it" unit/item/building
-- select-or-deselect fallback (#730 review round 6), never by the
-- debug/build/mine/chop/till/plant tool-claim branches or the
-- gameplay-inactive deadclick, which have no box-select meaning.
function dragSelect.armBoxSelect()
    dragSelect.boxSelectArmed = true
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
    local fx, fy = toFbCoords(x, y)
    debug.recordOutcome{
        kind = "input.drag",
        outcome = outcome,
        where = { x = fx, y = fy },
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
    local fx, fy = toFbCoords(pc.x, pc.y)
    debug.recordOutcome{
        kind = "input.click",
        outcome = pc.outcome or "accepted",
        where = { x = fx, y = fy },
        handler = pc.handler,
        reason = pc.reason,
    }
end

-- Store this press's click classification instead of recording it
-- immediately (#730). Called by init_mouse.lua's onMouseDown chain for
-- every drag-eligible press — left-button (unit/item/building
-- selection or deselect) or right-button (context menus, move order,
-- deadclick — round 4) — whichever of onMouseUp / cancel below
-- resolves this gesture is responsible for eventually recording it
-- exactly once (a real drag drops it in favor of its own "input.drag"
-- record instead). The SELECTION/MENU/MOVE-ORDER EFFECT is unaffected
-- — it still runs immediately in init_mouse.lua, at press, exactly as
-- before; only the F4 record's timing/existence moves.
function dragSelect.deferClick(button, handler, outcome, x, y, reason)
    local pc = { handler = handler, outcome = outcome, x = x, y = y, reason = reason }
    if button == 1 then
        dragSelect.pendingClick = pc
    elseif button == 2 then
        dragSelect.rightPendingClick = pc
    else
        -- Buttons this module doesn't track (middle is classified
        -- entirely at the engine level, Engine.Input.Thread; side
        -- buttons 4-8 aren't in H1's button vocabulary at all, and
        -- init_mouse.lua's onMouseDown broadcast still fires for them)
        -- — record immediately rather than silently dropping it, since
        -- onMouseUp/cancel above never resolve anything for these
        -- buttons. Preserves the pre-#730 behavior for this fallback.
        recordDeferredClick(pc)
    end
end

function dragSelect.onMouseUp(button, x, y, downRoute)
    if button == 1 then
        if dragSelect.state ~= "idle" then
            -- #730 review round 5: classify against the ACTUAL
            -- press->release distance (pastThreshold), NOT
            -- dragSelect.state — see pastThreshold's own comment for
            -- why state alone can be stale here.
            local wasDragging = pastThreshold(
                dragSelect.startX, dragSelect.startY, x, y)
            if wasDragging and dragSelect.boxSelectArmed then
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
            elseif wasDragging then
                -- Crossed the threshold, but this press was never
                -- box-select-armed (#730 review round 6) — a debug/
                -- build/mine/chop/till/plant tool claim, or a
                -- gameplay-inactive deadclick, dragged past the
                -- threshold. None of those have a drag GESTURE bound to
                -- them, so record that honestly instead of inventing a
                -- fake box-selection outcome.
                if downRoute ~= "swallowed" then
                    recordDragOutcome("noop", x, y, 0, 0,
                        "no drag gesture is defined for this input")
                else
                    recordDragOutcome("noop", x, y, 0, 0,
                        "release swallowed (focus loss / minimize)")
                end
            else
                -- Never crossed the drag threshold — this gesture is really
                -- just a click. Fire init_mouse.lua's deferred classification
                -- now that it's known to be the gesture's final (and only)
                -- outcome (#730 — keeps a below-threshold "drag" H1 action to
                -- exactly one record, same as a real click).
                if dragSelect.pendingClick then
                    recordDeferredClick(dragSelect.pendingClick)
                end
            end
            dragSelect.pendingClick   = nil
            dragSelect.boxSelectArmed = false
            dragSelect.state = "idle"
        end
    elseif button == 2 then
        -- Right-button (#730 review round 4): no box-selection effect
        -- to commit, just the click-vs-drag classification. A real
        -- drag has no gameplay meaning here, so it's recorded as an
        -- honest noop rather than inventing a fake accepted outcome.
        if dragSelect.rightState ~= "idle" then
            local wasDragging = pastThreshold(
                dragSelect.rightStartX, dragSelect.rightStartY, x, y)
            if wasDragging then
                if downRoute ~= "swallowed" then
                    recordDragOutcome("noop", x, y, 0, 0,
                        "no drag gesture is defined for right-button game-world input")
                else
                    recordDragOutcome("noop", x, y, 0, 0,
                        "release swallowed (focus loss / minimize)")
                end
            else
                if dragSelect.rightPendingClick then
                    recordDeferredClick(dragSelect.rightPendingClick)
                end
            end
            dragSelect.rightPendingClick = nil
            dragSelect.rightState = "idle"
        end
    end
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
    if dragSelect.state ~= "idle" then
        if dragSelect.state == "dragging" then
            recordDragOutcome("noop", dragSelect.startX, dragSelect.startY, 0, 0,
                "cancelled (view transition)")
        elseif dragSelect.pendingClick then
            recordDeferredClick(dragSelect.pendingClick)
        end
        if dragSelect.edgeIds then
            setEdgesVisible(false)
        end
        dragSelect.pendingClick   = nil
        dragSelect.boxSelectArmed = false
        dragSelect.state = "idle"
    end
    -- Right-button (#730 review round 4): same resolve-don't-lose
    -- contract as the left-button case above.
    if dragSelect.rightState ~= "idle" then
        if dragSelect.rightState == "dragging" then
            recordDragOutcome("noop", dragSelect.rightStartX, dragSelect.rightStartY, 0, 0,
                "cancelled (view transition)")
        elseif dragSelect.rightPendingClick then
            recordDeferredClick(dragSelect.rightPendingClick)
        end
        dragSelect.rightPendingClick = nil
        dragSelect.rightState = "idle"
    end
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
