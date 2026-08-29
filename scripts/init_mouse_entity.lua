-- Fallback world-entity mouse interaction for scripts/init_mouse.lua
-- (#543 family, split out in #1875).
--
-- "Fallback" is the routing sense: this is what a gameplay press
-- reaches once every earlier claimant in init_mouse.lua's ordered
-- chain has declined it — the debug overlay, the debug anim panel, the
-- five designation tools, and the six armed debug modes (their
-- placements on the left button, their cancels on the right).
-- Everything here hit-tests, selects, or issues orders against WORLD
-- ENTITIES: units, ground items and buildings.
--
-- That shared property is why the zoom-band gate below lives at this
-- module's two entry points rather than being sprinkled through the
-- branches: entity interaction is exactly the thing the HUD view band
-- has to gate, and nothing else in init_mouse.lua's chain is.
--
-- Called forward-only (handle*, not on*) from init_mouse.lua, which
-- passes its own SHADOWED recordClick — the one that defers through
-- unit_drag_select.deferClick — so every record made here resolves
-- through the matching dragSelect.onMouseUp or the zoomBand/hudHide
-- teardown's dragSelect.cancel, exactly as it did inline.
local M = {}

-- #1875: `zoomed_in` is the only HUD view band where world entities
-- are an interaction plane at all. The three entity hit tests
-- (src/Unit/HitTest.hs, src/Building/HitTest.hs,
-- src/World/Render/GroundItemQuads.hs) read camZoom for projection,
-- depth and visibility bounds but know nothing about the HUD band or
-- the render fade, so without this predicate a zoom-map or fade-band
-- press selects, box-selects, context-menus and move-orders entities
-- the player is not looking at: the zoom map draws none of them, and
-- src/World/Render.hs suppresses the entity passes once tileAlpha
-- decays to 0 across the `none` fade band. Matches the zoomed_in-only
-- convention every other world-acting surface already uses
-- (scripts/debug.lua's inGameplayView, the five designation tools'
-- active() predicates).
local BAND_ZOOMED_IN = "zoomed_in"

-- hud.currentView is a plain field (scripts/hud.lua) initialized to
-- "none" and re-derived from the live camera zoom by
-- hud.reconcileView. The `or "none"` only covers a partial hud table,
-- and lands on the same conservative side a real one would.
local function pressViewBand()
    return require("scripts.hud").currentView or "none"
end

-- The F4 (#646) outcome for a press this gate declines. The two
-- off-bands are genuinely different events, and this chain already
-- distinguishes "recognized gesture, nothing to act on" (noop) from
-- "phantom affordance" (deadclick):
--   * "zoomed_out" — the zoom map. Mouse-down reaches Lua through
--     broadcastToModules, so scripts/hud.lua's own subscriber still
--     acts on this SAME press (chunk select on left, chunk clear on
--     right). The player gets feedback; declining here is a
--     recognized no-op, not a dead click.
--   * "none" — the fade band, where neither handler owns the press on
--     either button. That really is a dead click.
local function offBandOutcome(band)
    if band == "zoomed_out" then return "noop" end
    return "deadclick"
end

-- The reason names the ACTUAL press band rather than a generic
-- "off-band", so the oracle can tell a zoom-map press from a
-- fade-band one.
local function offBandReason(band)
    return "world-entity input suppressed outside the zoomed-in view "
        .. "(press view band: " .. tostring(band) .. ")"
end

-- True when this press must not reach world-entity interaction, having
-- already recorded its own diagnostic. The band is read HERE, at
-- mouse-down, and nothing at release re-consults it — see
-- handleLeftPress for why declining to arm box-selection is what makes
-- a press begun off-band stay suppressed even when it is released back
-- in zoomed_in.
local function suppressedOffBand(x, y, recordClick)
    local band = pressViewBand()
    if band == BAND_ZOOMED_IN then
        return false
    end
    recordClick(nil, offBandOutcome(band), x, y, offBandReason(band))
    return true
end

-- Left button: box-select arming plus the unit > item > building
-- select-or-deselect chain.
function M.handleLeftPress(x, y, recordClick)
    -- Suppressing the arm is the whole cross-band contract (#1875 §3):
    -- unit_drag_select.onMouseUp gates its unit.hitTestInRect commit on
    -- boxSelectArmed, so a press classified off-band here can never
    -- commit a box selection at release, whatever band the release
    -- happens in.
    if suppressedOffBand(x, y, recordClick) then return end

    local dragSelect = require("scripts.unit_drag_select")
    -- Arm unit drag-select's BOX-SELECTION effect specifically
    -- (click-vs-drag classification itself was already armed at the
    -- top of init_mouse.lua's onMouseDown, #730 review round 6).
    -- Reached forward-only (handle*, not a broadcast) so it stays in
    -- THAT ordered claim chain: every guard ahead of this call — the
    -- debug overlay / anim panel / build tool / mine tool, AND the
    -- debug armed-placement modes (spawn / item / fluid / terrain /
    -- location / structure) that each `return` there — has already
    -- consumed and bailed on its own click. So a click eaten by any
    -- of them can no longer also start a background box-selection
    -- (#114). Reached below those returns rather than enumerating the
    -- armed* fields, so a future armed mode stays shielded for free.
    -- It doesn't consume the click — the single-unit selection /
    -- tile-cursor logic below still runs; the drag only takes over on
    -- mouse-up if it passes threshold. The gameplay-active gate
    -- (#154/#146 — a box-select must never arm behind a menu / pause
    -- overlay) is the early return at the top of init_mouse.lua's
    -- MOUSE_LEFT branch, so no per-call check is needed.
    dragSelect.armBoxSelect()

    local id = unit.hitTestAt(x, y)
    local shift = engine.isKeyDown("LeftShift")
                  or engine.isKeyDown("RightShift")
    if id then
        -- Hit a unit. Shift adds to the current selection;
        -- otherwise replace. The unit_info_panel watcher will
        -- see the change next tick and push unit info into the
        -- HUD panel + clear any tile cursor.
        if shift then
            local current = unit.getSelected() or {}
            local seen = {}
            local merged = {}
            for _, uid in ipairs(current) do
                if not seen[uid] then
                    seen[uid] = true
                    table.insert(merged, uid)
                end
            end
            if not seen[id] then table.insert(merged, id) end
            unit.setSelection(merged)
        else
            unit.select(id)
        end
        -- Selecting a unit takes over the info panel — deselect
        -- any building/item so the panel doesn't flicker between
        -- schemas.
        building.deselect()
        item.deselect()
        recordClick("unit_select", nil, x, y)
    else
        -- No unit hit. Try a ground item (click priority:
        -- units > items > buildings — moving things win).
        local gid = item.hitTestAt(x, y)
        if gid then
            item.select(gid)
            -- Ground-item selection is mutually exclusive with
            -- unit/building selection (see World.Cursor.Types). Items
            -- are single-select, so Shift carries no additive meaning
            -- here — always clear the other domains, even on Shift.
            unit.deselectAll()
            building.deselect()
            recordClick("item_select", nil, x, y)
        else
            -- No item. Try a building.
            local bid = building.hitTestAt(x, y)
            if bid then
                building.select(bid)
                -- Buildings are single-select and mutually exclusive
                -- with unit/item selection; clear the others
                -- unconditionally (Shift adds units, not buildings).
                item.deselect()
                unit.deselectAll()
                recordClick("building_select", nil, x, y)
            else
                -- Click missed everything. With Shift held, keep
                -- the current selection (so shift-dragging from
                -- empty terrain can extend it). Otherwise deselect.
                -- Not a "deadclick" — an empty-terrain click is a
                -- recognized deselect gesture the player understands,
                -- not a phantom affordance; "noop" reflects that
                -- nothing was there to act on, without flagging it as
                -- a UX defect.
                if not shift then
                    unit.deselectAll()
                    building.deselect()
                    item.deselect()
                end
                recordClick("deselect", "noop", x, y)
            end
        end
    end
end

-- Right button: per-target context menus, then move orders for the
-- current selection, then the tile context menu.
function M.handleRightPress(x, y, recordClick)
    if suppressedOffBand(x, y, recordClick) then return end

    -- Per-target menu construction lives in init_context_menu.lua;
    -- each try*Menu hit-tests its own target, shows the menu, and
    -- returns true if it claimed the click. Building menus win over
    -- unit menus win over item menus, matching the original inline
    -- ordering.
    local contextMenus = require("scripts.init_context_menu")
    if contextMenus.tryBuildingMenu(x, y) then
        recordClick("context_menu_building", nil, x, y)
        return
    end
    if contextMenus.tryUnitMenu(x, y) then
        recordClick("context_menu_unit", nil, x, y)
        return
    end
    if contextMenus.tryItemMenu(x, y) then
        recordClick("context_menu_item", nil, x, y)
        return
    end

    -- Right-click is a move order when units are selected.
    -- hud.onMouseDown also fires on right-click and clears the
    -- tile cursor — that's fine, it doesn't touch unit selection.
    local selected = unit.getSelected()
    if selected and #selected > 0 then
        -- Live pick at the click coords so the move order targets the
        -- tile under the click, not the 0.1s-cached hover (#123).
        local gx, gy = world.pickTile(x, y)
        if gx and gy then
            local tx = gx + 0.5
            local ty = gy + 0.5
            for _, uid in ipairs(selected) do
                -- Route through the AI so the command becomes a
                -- utility-scored candidate that high-priority needs
                -- (thirst, etc.) can interrupt and resume. No explicit
                -- speed → the "ordered" regime (a sustainable push above
                -- comfort); a hard-coded fast speed here exhausts the
                -- unit's stamina and collapses it mid-move. #1254: a
                -- PLAYER order, ending any Mode A session on this unit.
                require("scripts.transfer_session").notePlayerOrder(uid)
                require("scripts.unit_ai").commandMove(uid, tx, ty)
            end
            recordClick("move_order", nil, x, y)
        else
            -- Off-world right-click with a selection: no tile to
            -- order to, and no tile menu either (that branch is the
            -- `else` below, gated on no-selection).
            recordClick("move_order", "noop", x, y, "no tile under cursor")
        end
    else
        -- No selection → open the tile context menu. tryTileMenu
        -- returns false on an off-world click (world.pickTile
        -- misses) without opening anything — that's a genuine
        -- deadclick, not an accepted context-menu open (review
        -- round 5 found this recorded unconditionally as accepted).
        if contextMenus.tryTileMenu(x, y) then
            recordClick("context_menu_tile", nil, x, y)
        else
            recordClick(nil, "deadclick", x, y, "no tile under cursor")
        end
    end
end

return M
