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
-- WHEN that conversion happens is load-bearing (#1676). A gesture
-- resolved at its own event (a drag's release) converts live. A
-- DEFERRED one — a pending click, or a drag cancelled by a view
-- transition, both of which resolve arbitrarily later than the press
-- they report — converts at PRESS and stores the result
-- (startFbX/startFbY, pendingClick.fbX/fbY), because the
-- window→framebuffer ratio can change during the hold and converting
-- afterwards would report the press under a ratio it never had.
--
-- The module self-registers in package.loaded so engine.loadScript
-- and require see the same instance — same reason as scripts/debug.lua.

local dragSelect = package.loaded["scripts.unit_drag_select"] or {}
package.loaded["scripts.unit_drag_select"] = dragSelect

local hud = require("scripts.hud")

dragSelect.state    = "idle"
dragSelect.startX   = 0
dragSelect.startY   = 0
-- F4 (#1676): the press's location in FRAMEBUFFER pixels, captured
-- from the window/framebuffer ratio live when the gesture began. The
-- window-space startX/startY above are what the drag threshold and the
-- rect visual use and stay untouched; these two exist because a DPI
-- change or a window/framebuffer resize DURING the hold moves that
-- ratio, so converting startX/startY at resolution time would report
-- the press under a ratio it never happened at. Degenerate-viewport
-- fallback is baked in at capture (see toFbCoords), so a press taken
-- while the viewport was unusable keeps its raw window coordinate
-- however the geometry later recovers.
dragSelect.startFbX = 0
dragSelect.startFbY = 0
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
-- #1676: the right-button gesture keeps its OWN press-time framebuffer
-- capture, for the same reason as startFbX/startFbY above — the two
-- buttons track independent gestures with independent start points.
dragSelect.rightStartFbX     = 0
dragSelect.rightStartFbY     = 0
dragSelect.rightPendingClick = nil
dragSelect.rightCurrX        = 0
dragSelect.rightCurrY        = 0
-- #1856: a designation TOOL's pluggable box effect, one slot per
-- tracked button. A tool arms its own press (chopTool.handleMouseDown
-- calls armToolBox) and the SAME machinery every other gesture uses —
-- the four-pixel threshold, the fast-release classification, the
-- visible rect, the view-transition teardown and the one-record-per-
-- action outcome path — resolves it to exactly one click or one box.
--
-- This is deliberately NOT boxSelectArmed: that flag means "commit a
-- UNIT selection at release" and is the #114/#730 fallback's alone. An
-- armed tool box replaces the effect, not the gesture, which is why
-- both buttons can carry one — Chop's left adds and its right erases
-- (D-12), and neither has any unit-selection meaning.
--
-- An effect is a table of
--   { handler = <F4 handler name>,
--     onClick = function(x, y)               -> applied count,
--     onBox   = function(x1, y1, x2, y2)     -> applied count }
dragSelect.toolBox = {}
-- Which button currently owns the shared rect overlay, or nil.
dragSelect.visualButton = nil
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
-- location converts. Falls back to the raw coordinate when EITHER side
-- of the ratio is unusable (review round 1: a minimized/hidden window
-- can report a zero framebuffer while the window size itself stays
-- positive, and checking only the window side silently collapsed every
-- recorded location to (0,0) instead of falling back) — mirrors the
-- engine-side windowToFb's all-four-dimensions guard.
local function toFbCoords(x, y)
    local ww, wh = engine.getWindowSize()
    local fbW, fbH = engine.getFramebufferSize()
    if not ww or ww <= 0 or not wh or wh <= 0
       or not fbW or fbW <= 0 or not fbH or fbH <= 0 then
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
        "assets/textures/utility/white.png", "ui")

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

-- Sync the 4 edge sprites to the given drag bounds (window pixels).
-- Called every tick while dragging. #1856 made the bounds explicit so
-- a right-button tool box draws the identical rect from its own start
-- point — one visual, one geometry, both buttons.
local function updateRectVisual(ax, ay, bx, by)
    local ww, wh = engine.getWindowSize()
    local fbW, fbH = engine.getFramebufferSize()
    local scaleX = (ww and ww > 0) and (fbW / ww) or 1
    local scaleY = (wh and wh > 0) and (fbH / wh) or 1
    local x1 = math.min(ax, bx) * scaleX
    local y1 = math.min(ay, by) * scaleY
    local x2 = math.max(ax, bx) * scaleX
    local y2 = math.max(ay, by) * scaleY
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

-- Does THIS button's press draw and commit a box? Either the #114/#730
-- unit-selection fallback (left only) or a #1856 tool box (either).
local function boxArmed(button)
    if button == 1 then
        return dragSelect.boxSelectArmed or dragSelect.toolBox[1] ~= nil
    end
    return dragSelect.toolBox[button] ~= nil
end

-- Claim the shared rect overlay for one button and show it. Only one
-- gesture can own the visual; the first to cross the threshold keeps
-- it until it resolves, so two simultaneous holds cannot fight over it.
local function claimVisual(button)
    if dragSelect.visualButton == nil then
        dragSelect.visualButton = button
        setEdgesVisible(true)
    end
end

local function releaseVisual(button)
    if dragSelect.visualButton == button then
        dragSelect.visualButton = nil
        if dragSelect.edgeIds then
            setEdgesVisible(false)
        end
    end
end

function dragSelect.update(dt)
    -- Box visuals/effects only ever apply to an ARMED press (#730
    -- review round 6) — a non-armed press (debug/build/mine/till/plant
    -- tool claims, a menu-background deadclick) never transitions to
    -- "dragging" here at all; its click-vs-drag F4 classification is
    -- computed directly from coordinates in onMouseUp regardless
    -- (round 5), so it needs nothing from this periodic tick.
    if dragSelect.state == "pressed" and boxArmed(1) then
        local mx, my = engine.getMousePosition()
        if mx then
            dragSelect.currX = mx
            dragSelect.currY = my
            if pastThreshold(dragSelect.startX, dragSelect.startY, mx, my) then
                dragSelect.state = "dragging"
                claimVisual(1)
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
            if dragSelect.visualButton == 1 then
                updateRectVisual(dragSelect.startX, dragSelect.startY, mx, my)
            end
        end
    end

    -- Right-button tracking. Without an armed tool box this is
    -- threshold tracking only (#730 review round 4) — no visual, no
    -- effect, purely for the deferred F4 click-vs-drag classification.
    -- WITH one (#1856: Chop's erase gesture) it draws and commits the
    -- identical box the left button does.
    if dragSelect.rightState == "pressed" then
        local mx, my = engine.getMousePosition()
        if mx then
            dragSelect.rightCurrX = mx
            dragSelect.rightCurrY = my
            if pastThreshold(dragSelect.rightStartX, dragSelect.rightStartY,
                             mx, my) then
                dragSelect.rightState = "dragging"
                if boxArmed(2) then claimVisual(2) end
            end
        end
    elseif dragSelect.rightState == "dragging" then
        local mx, my = engine.getMousePosition()
        if mx then
            dragSelect.rightCurrX = mx
            dragSelect.rightCurrY = my
            if dragSelect.visualButton == 2 then
                updateRectVisual(dragSelect.rightStartX,
                                 dragSelect.rightStartY, mx, my)
            end
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
        -- #1676: the drag's origin in framebuffer space, captured with
        -- the ratio live at press — what a "cancelled (view
        -- transition)" record reports later.
        dragSelect.startFbX, dragSelect.startFbY = toFbCoords(x, y)
        dragSelect.currX  = x
        dragSelect.currY  = y
        dragSelect.pendingClick    = nil
        dragSelect.boxSelectArmed  = false
    elseif button == 2 then
        dragSelect.rightState        = "pressed"
        dragSelect.rightStartX       = x
        dragSelect.rightStartY       = y
        dragSelect.rightStartFbX, dragSelect.rightStartFbY = toFbCoords(x, y)
        dragSelect.rightPendingClick = nil
    end
end

-- Opts THIS press into box-selection's visual/commit behavior — called
-- only by init_mouse.lua's "no tool claimed it" unit/item/building
-- select-or-deselect fallback (#730 review round 6, which #1875 moved
-- to init_mouse_entity.lua behind its zoom-band gate), never by the
-- debug/build/mine/chop/till/plant tool-claim branches or the
-- gameplay-inactive deadclick, which have no box-select meaning.
function dragSelect.armBoxSelect()
    dragSelect.boxSelectArmed = true
end

-- #1856: arm a designation TOOL's box effect on this press. Called from
-- the tool's own handleMouseDown, which init_mouse.lua has already run
-- dragSelect.handleMouseDown ahead of, so the press's start point and
-- framebuffer capture are in place.
--
-- The effect replaces what happens at RELEASE, never the gesture
-- machinery: the four-pixel threshold, the fast-drag classification,
-- the visible rect, the view-transition teardown and the
-- exactly-one-record outcome path are all the shared ones. Passing nil
-- disarms.
function dragSelect.armToolBox(button, effect)
    dragSelect.toolBox[button] = effect
end

-- Disarm a tool's box effect MID-GESTURE — Escape, a tool switch, a
-- view transition, all of which can arrive while the button is still
-- held. The gesture itself is still live, so this does not resolve it;
-- what it must do is leave nothing behind that outlives the effect:
--
--   * the visible rect, which the EFFECT owned. Without this the
--     release falls through to the generic unarmed-drag path, which has
--     no visual to release, and the box stays painted on screen; and
--   * the press's deferred click record, which still carries the
--     press-time "accepted" default. A cancelled below-threshold
--     gesture performs nothing, so recording an accepted chop click for
--     it is a lie.
--
-- Idempotent, like every other teardown here.
function dragSelect.disarmToolBox(button)
    if dragSelect.toolBox[button] == nil then return end
    dragSelect.toolBox[button] = nil
    releaseVisual(button)
    local pending = (button == 1) and dragSelect.pendingClick
                                   or dragSelect.rightPendingClick
    if pending then
        pending.outcome = "noop"
        pending.reason = "the tool gesture was cancelled before release"
    end
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
-- #1676: takes an ALREADY-framebuffer-space location, for the callers
-- that hold a press-time capture rather than a live coordinate.
local function recordDragOutcomeFb(outcome, fx, fy, requested, applied, reason)
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

-- A drag that RESOLVES at its release point: the location is the live
-- release coordinate, so converting it with the geometry live now is
-- correct (#1676 changes nothing here — it is the same event).
local function recordDragOutcome(outcome, x, y, requested, applied, reason)
    local fx, fy = toFbCoords(x, y)
    recordDragOutcomeFb(outcome, fx, fy, requested, applied, reason)
end

-- #1875: a gesture that crosses DRAG_THRESHOLD resolves as ONE
-- "input.drag" record and DISCARDS the deferred click (#730 — exactly
-- one primary Layer A record per H1 action). That is correct for the
-- record COUNT and wrong for its CONTENT when the press carried its own
-- reason for having been declined at mouse-down: the off-band
-- world-entity gate (scripts/init_mouse_entity.lua, whose reason names
-- the actual press view band) and the two gameplay-inactive gates
-- (scripts/init_mouse.lua) both do. Dropping that text left a dragged
-- off-band press describable only as "no drag gesture is defined",
-- which identifies nothing about WHY it was declined.
--
-- So the drag record carries the generic gesture text AND the press's
-- own reason when there is one. `pending` is the deferred click about
-- to be discarded; a press that carried no reason (an ordinary
-- box-select, a tool claim) keeps exactly the text it had before.
local function dragReason(generic, pending)
    local pressReason = pending and pending.reason
    if pressReason then
        return generic .. "; " .. pressReason
    end
    return generic
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
        -- #1676: the press's OWN press-time framebuffer position,
        -- captured by deferClick — never a conversion of the retained
        -- window coordinate under whatever ratio happens to be live at
        -- this (arbitrarily later) resolution.
        where = { x = pc.fbX, y = pc.fbY },
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
    -- #1676: capture the framebuffer position NOW, at press. Every
    -- resolution path below (onMouseUp, cancel) spends this stored
    -- value instead of reconverting x/y later.
    local fbX, fbY = toFbCoords(x, y)
    local pc = { handler = handler, outcome = outcome, x = x, y = y,
                 fbX = fbX, fbY = fbY, reason = reason }
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

-- #1856: resolve an armed TOOL box at release. Returns true when the
-- effect owned this release, so the caller skips its own commit path.
--
-- The three endings are the same three every other gesture has, and
-- each produces exactly one Layer A record:
--
--   * a swallowed release (focus loss / minimize) commits NOTHING —
--     the box is abandoned, not finished, so no designation lands at
--     whatever stale coordinate the cursor was left at;
--   * past the threshold, the box commits and records "input.drag";
--   * below it, the click commits and the press's own deferred
--     "input.click" record is what lands.
local function resolveToolBox(effect, startX, startY, x, y,
                              pending, downRoute)
    if downRoute == "swallowed" then
        recordDragOutcome("noop", x, y, 0, 0, dragReason(
            "release swallowed (focus loss / minimize)", pending))
        return
    end
    if pastThreshold(startX, startY, x, y) then
        local applied = effect.onBox and effect.onBox(startX, startY, x, y) or 0
        recordDragOutcome(applied > 0 and "accepted" or "noop",
            x, y, applied, applied)
    else
        local applied = effect.onClick and effect.onClick(x, y) or 0
        local pc = pending
        if not pc then
            local fx, fy = toFbCoords(x, y)
            pc = { handler = effect.handler, fbX = fx, fbY = fy }
        end
        -- The press-time record could not know whether the click would
        -- find a target; the release does.
        pc.outcome = applied > 0 and "accepted" or "noop"
        recordDeferredClick(pc)
    end
end

function dragSelect.onMouseUp(button, x, y, downRoute)
    -- An armed tool box owns its whole release, both buttons alike.
    local effect = dragSelect.toolBox[button]
    if effect then
        local live = (button == 1 and dragSelect.state ~= "idle")
                  or (button == 2 and dragSelect.rightState ~= "idle")
        if live then
            local sx = button == 1 and dragSelect.startX or dragSelect.rightStartX
            local sy = button == 1 and dragSelect.startY or dragSelect.rightStartY
            local pending = button == 1 and dragSelect.pendingClick
                                         or dragSelect.rightPendingClick
            resolveToolBox(effect, sx, sy, x, y, pending, downRoute)
        end
        releaseVisual(button)
        dragSelect.toolBox[button] = nil
        if button == 1 then
            dragSelect.pendingClick = nil
            dragSelect.boxSelectArmed = false
            dragSelect.state = "idle"
        else
            dragSelect.rightPendingClick = nil
            dragSelect.rightState = "idle"
        end
        return
    end

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
                releaseVisual(1)
            elseif wasDragging then
                -- Crossed the threshold, but this press was never
                -- box-select-armed (#730 review round 6) — a debug/
                -- build/mine/chop/till/plant tool claim, or a
                -- gameplay-inactive deadclick, dragged past the
                -- threshold. None of those have a drag GESTURE bound to
                -- them, so record that honestly instead of inventing a
                -- fake box-selection outcome.
                if downRoute ~= "swallowed" then
                    recordDragOutcome("noop", x, y, 0, 0, dragReason(
                        "no drag gesture is defined for this input",
                        dragSelect.pendingClick))
                else
                    recordDragOutcome("noop", x, y, 0, 0, dragReason(
                        "release swallowed (focus loss / minimize)",
                        dragSelect.pendingClick))
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
            releaseVisual(1)
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
                    recordDragOutcome("noop", x, y, 0, 0, dragReason(
                        "no drag gesture is defined for right-button game-world input",
                        dragSelect.rightPendingClick))
                else
                    recordDragOutcome("noop", x, y, 0, 0, dragReason(
                        "release swallowed (focus loss / minimize)",
                        dragSelect.rightPendingClick))
                end
            else
                if dragSelect.rightPendingClick then
                    recordDeferredClick(dragSelect.rightPendingClick)
                end
            end
            releaseVisual(2)
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
            recordDragOutcomeFb("noop", dragSelect.startFbX, dragSelect.startFbY,
                0, 0, dragReason("cancelled (view transition)",
                                 dragSelect.pendingClick))
        elseif dragSelect.pendingClick then
            recordDeferredClick(dragSelect.pendingClick)
        end
        releaseVisual(1)
        dragSelect.pendingClick   = nil
        dragSelect.boxSelectArmed = false
        dragSelect.state = "idle"
    end
    -- #1856: an armed tool box is abandoned by a view transition
    -- exactly as a box selection is — no designation lands, and the
    -- tool is left with nothing half-committed. Through disarmToolBox
    -- so the rect it owned comes down with it.
    dragSelect.disarmToolBox(1)
    -- Right-button (#730 review round 4): same resolve-don't-lose
    -- contract as the left-button case above.
    if dragSelect.rightState ~= "idle" then
        if dragSelect.rightState == "dragging" then
            recordDragOutcomeFb("noop", dragSelect.rightStartFbX,
                dragSelect.rightStartFbY, 0, 0,
                dragReason("cancelled (view transition)",
                           dragSelect.rightPendingClick))
        elseif dragSelect.rightPendingClick then
            recordDeferredClick(dragSelect.rightPendingClick)
        end
        releaseVisual(2)
        dragSelect.rightPendingClick = nil
        dragSelect.rightState = "idle"
    end
    dragSelect.disarmToolBox(2)
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
