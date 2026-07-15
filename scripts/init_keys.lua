-- Gameplay key routing for scripts/init.lua (#543): pause toggle,
-- the Escape dismiss cascade, log-panel toggle, and tool Esc-cancel
-- dispatch.
local M = {}

function M.onKeyDown(key)
    -- #742: Escape's dismiss cascade (context menu / transient popups /
    -- event-combat-injury-unit logs) must keep working even while a
    -- visible modal makes ordinary gameplay input inactive — closing the
    -- very thing that's blocking gameplay can't itself be blocked by
    -- that same block. So it runs BEFORE the isGameplayInputActive()
    -- gate below, unconditionally. Every handler here already
    -- self-guards on its own open/visible flag, so calling them when
    -- there's nothing to dismiss is a safe no-op — this is purely a
    -- widening of when the (unchanged) cascade can fire, not new logic.
    if key == "Escape" then
        -- Remote-settlement confirmation modal (#779) is a real
        -- input-exclusive modal (LayerModal), not a transient popup —
        -- dismiss it first, as "Choose Another Site".
        local remoteWarning = require("scripts.build_tool_remote_warning")
        if remoteWarning.handleKeyDown(key) then return end

        -- Context menu takes priority — close it before any popup /
        -- selection-cleanup runs. Matches "Escape cancels the topmost
        -- transient UI thing" intuition.
        local contextMenu = require("scripts.ui.context_menu")
        if contextMenu.handleEscape() then return end

        -- Cargo inventory popup is next-most-transient.
        local cargoPanel = require("scripts.cargo_inventory_panel")
        if cargoPanel.handleKeyDown(key) then return end

        -- Item contents popup (unit-carried container) — same tier.
        local itemContents = require("scripts.item_contents_panel")
        if itemContents.handleKeyDown(key) then return end

        -- Crafting station bills popup (#330) — same tier.
        local craftingPanel = require("scripts.crafting_panel")
        if craftingPanel.handleKeyDown(key) then return end

        local popup = require("scripts.popup")
        local shift = engine.isKeyDown("LeftShift")
                      or engine.isKeyDown("RightShift")
        if shift then
            if popup.dismissAll() > 0 then return end
        else
            if popup.dismissTopmost() then return end
        end
        local eventLog = require("scripts.event_log")
        if eventLog.isVisible() then
            eventLog.hide()
            return
        end
        local combatLog = require("scripts.combat_log")
        if combatLog.isVisible() then
            combatLog.hide()
            return
        end
        local injuryLog = require("scripts.injury_log_panel")
        if injuryLog.isVisible() then
            injuryLog.hide()
            return
        end
        local unitLog = require("scripts.unit_log")
        if unitLog.isVisible() then
            unitLog.hide()
            return
        end
        -- (fall through to the existing unit-deselect path below)
    end

    -- Don't run ordinary gameplay key handlers while a menu/overlay is
    -- up (#182), OR while a visible modal page makes gameplay input
    -- inactive (#742, isGameplayInputActive() now folds in
    -- UI.isInputBlocked()). Menus and overlays don't establish UI
    -- focus, so the input thread stays in the generic game-keydown path
    -- and broadcasts every key to all Lua modules — including this one.
    -- Gate here on the active view/modal state so menu-open presses
    -- (Space pause-toggle, L log-toggle, build/mine Escape,
    -- selection-clear, etc.) can't reach the world underneath. Escape's
    -- OWN dismiss cascade above deliberately runs before this gate;
    -- menu-level Escape (e.g. closing the settings menu itself) is
    -- unaffected either way — it goes through uiManager.onUIEscape, a
    -- separate broadcast this gate doesn't touch.
    if not require("scripts.ui_manager").isGameplayInputActive() then
        return
    end

    -- Space toggles pause first — works regardless of tool mode or
    -- selection state, and shouldn't be eaten by a tool's local
    -- handler.
    if key == "Space" then
        require("scripts.pause").toggle()
        return
    end

    -- User-rebindable: toggle the currently-selected log panel
    -- (event or combat, matching the HUD icon's mode). Default
    -- bind is L. engine.isActionDown is true while the action's
    -- key is held; the press-transition broadcast makes "down"
    -- coincide with "just pressed" for this handler.
    if engine.isActionDown("toggleEventLog") then
        local hud = require("scripts.hud")
        if hud.logMode == "combat" then
            require("scripts.combat_log").toggle()
        else
            require("scripts.event_log").toggle()
        end
        return
    end

    -- Build tool's Esc cancels placement before the default Esc
    -- handler clears unit selection.
    local buildTool = require("scripts.build_tool")
    if buildTool.handleKeyDown(key) then
        return
    end
    -- Mine tool's Esc cancels a pending designation anchor.
    local mineTool = require("scripts.mine_tool")
    if mineTool.handleKeyDown(key) then
        return
    end
    -- Chop tool's Esc cancels a pending designation anchor.
    local chopTool = require("scripts.chop_tool")
    if chopTool.handleKeyDown(key) then
        return
    end
    -- Till tool's Esc cancels a pending designation anchor.
    local tillTool = require("scripts.till_tool")
    if tillTool.handleKeyDown(key) then
        return
    end
    -- Plant tool's Esc closes the planting screen if it's open.
    local plantTool = require("scripts.plant_tool")
    if plantTool.handleKeyDown(key) then
        return
    end
    -- ESC clears any active gameplay selection / cursor.
    -- Doesn't conflict with shell/UI focus: those modes consume ESC
    -- earlier in the input thread (LuaFocusLost / LuaUIEscape) and
    -- never reach this broadcast.
    --
    -- Unit, building, and ground-item selections all share the same HUD
    -- info-panel ownership system, so Escape clears whichever one is
    -- active (#177). building.deselect()/item.deselect() are called
    -- unconditionally (matching every other deselection site in this
    -- file): they are no-ops when nothing is selected, and
    -- building.getSelected() in particular is filtered to the active
    -- world page while building.deselect() clears the underlying global
    -- building selection, so guarding on getSelected() would skip a
    -- stale off-page building selection and leave it live when that page
    -- is shown again.
    if key == "Escape" then
        local selected = unit.getSelected()
        if #selected > 0 then
            unit.deselectAll()
        end
        building.deselect()
        item.deselect()
        -- ESC also clears the active cursor (tile/chunk) selection so it
        -- obeys the same cancel semantics as unit selection (#180). Both
        -- the world-tile (zoomed-in) and zoom-map chunk (zoomed-out)
        -- selections are cleared unconditionally rather than gating on
        -- hud.currentView: a selection made in one view persists in the
        -- engine cursor state across a zoom (and through the mid-fade
        -- band where currentView is "none"), so a view-gated clear would
        -- miss it there (e.g. select a tile, stop in the fade zone, press
        -- Escape — the old selection would survive). clearWorld/
        -- ZoomCursorSelect is idempotent (no-op when nothing is
        -- selected), so clearing both is safe. The cursor poller tears
        -- down the dependent tile/chunk info panel — and, via the empty
        -- onSetInfoText broadcast, the tile-editor popup — on the next
        -- tick. Gated on hud.visible like onMouseDown so a menu-open Esc
        -- never acts on a hidden world.
        local hud = require("scripts.hud")
        if hud and hud.visible and hud.worldId then
            world.clearWorldCursorSelect(hud.worldId)
            world.clearZoomCursorSelect(hud.worldId)
        end
    end
end

function M.onKeyUp(key)
end

return M
