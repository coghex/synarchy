-- UI Manager: panel click forwarding (#544 split from ui_manager.lua).
--
-- Owns click routes that forward into a specific panel/overlay module
-- rather than a generic widget family: the event/combat/injury/unit
-- logs, popups, the unit-info-v2 pane, the build menu, the cargo
-- inventory popup, context menus, and the keybind-capture modal. Most
-- targets here are singleton modules reached via package.loaded rather
-- than a plain require, because they're also engine.loadScript'd
-- (dofile) elsewhere — see scripts/ui_manager.lua's header and the
-- dofile-module-state gotcha these all follow.
local uiManager = package.loaded["scripts.ui_manager"]

local tabbar      = require("scripts.ui.tabbar")
local contextMenu = require("scripts.ui.context_menu")
local eventLog    = require("scripts.event_log")
local combatLog   = require("scripts.combat_log")
local injuryLog   = require("scripts.injury_log_panel")
local unitLog     = require("scripts.unit_log")
local popup       = require("scripts.popup")
local settingsMenu = require("scripts.settings_menu")

-- Dispatched when a row in the event-log panel is clicked. Routes
-- to event_log.onRowClick which re-pops the popup for that entry.
function uiManager.onEventLogRowClick(elemHandle)
    if eventLog.onRowClick then
        return eventLog.onRowClick(elemHandle)
    end
    return false
end

-- Combat-log panel click routes. Tab clicks (including the pinned
-- "All" tab) re-render content for the selected battle. The scroll
-- buttons shift the battle-tab strip one cell at a time.
function uiManager.onCombatLogTabClick(elemHandle)
    if combatLog.onTabClick then
        return combatLog.onTabClick(elemHandle)
    end
    return false
end

function uiManager.onCombatLogScrollPrev(elemHandle)
    if combatLog.onScrollPrev then
        return combatLog.onScrollPrev()
    end
    return false
end

function uiManager.onCombatLogScrollNext(elemHandle)
    if combatLog.onScrollNext then
        return combatLog.onScrollNext()
    end
    return false
end

-- Injury-log panel click routes (mirror the combat-log ones; tabs are
-- per injured unit).
function uiManager.onInjuryLogTabClick(elemHandle)
    if injuryLog.onTabClick then
        return injuryLog.onTabClick(elemHandle)
    end
    return false
end

function uiManager.onInjuryLogScrollPrev(elemHandle)
    if injuryLog.onScrollPrev then
        return injuryLog.onScrollPrev()
    end
    return false
end

function uiManager.onInjuryLogScrollNext(elemHandle)
    if injuryLog.onScrollNext then
        return injuryLog.onScrollNext()
    end
    return false
end

-- Per-unit log panel: tab clicks switch the All/Event/Combat/Injury view.
function uiManager.onUnitLogTabClick(elemHandle)
    if unitLog.onTabClick then
        return unitLog.onTabClick(elemHandle)
    end
    return false
end

-- Dispatched when a popup line is clicked. Routes to popup.onLineClick
-- which cycles the camera through the line's stored coords.
function uiManager.onPopupLineClick(elemHandle)
    if popup.onLineClick then
        return popup.onLineClick(elemHandle)
    end
    return false
end

-- Dispatched when a popup's title-bar mute-toggle icon is clicked.
-- Routes to popup.onMuteToggleClick, which toggles the category's
-- `popup` notification setting via setNotificationOverrides.
function uiManager.onPopupMuteToggleClick(elemHandle)
    if popup.onMuteToggleClick then
        return popup.onMuteToggleClick(elemHandle)
    end
    return false
end

-- Unit-info v2 sprite-tab strip. Each tab's box has this callback
-- registered via UI.setOnClick; clicks make that unit the active one.
function uiManager.onUnitInfoTabClick(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.handleTabClick then
        return mod.handleTabClick(elemHandle)
    end
    return false
end

function uiManager.onUnitInfoScrollLeft(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.handleScrollLeft then
        return mod.handleScrollLeft(elemHandle)
    end
    return false
end

function uiManager.onUnitInfoScrollRight(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.handleScrollRight then
        return mod.handleScrollRight(elemHandle)
    end
    return false
end

function uiManager.onUnitInfoSubTabClick(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.handleSubTabClick then
        return mod.handleSubTabClick(elemHandle)
    end
    return false
end

function uiManager.onUnitInfoLogClick(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.handleLogClick then
        return mod.handleLogClick(elemHandle)
    end
    return false
end

function uiManager.onInventoryTabClick(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.handleInvTabClick then
        return mod.handleInvTabClick(elemHandle)
    end
    return false
end

-- Build-menu tab strip + icon clicks. See scripts/build_tool.lua's
-- handleTabClick / handleIconClick for dispatch.
function uiManager.onBuildMenuTabClick(elemHandle)
    local mod = package.loaded["scripts.build_tool"]
    if mod and mod.handleTabClick then
        return mod.handleTabClick(elemHandle)
    end
    return false
end

function uiManager.onBuildMenuIconClick(elemHandle)
    local mod = package.loaded["scripts.build_tool"]
    if mod and mod.handleIconClick then
        return mod.handleIconClick(elemHandle)
    end
    return false
end

-- Cargo inventory popup tab strip + per-row right-click. The popup
-- is triggered from init.lua's right-click handler (Contents menu).
function uiManager.onCargoInventoryTabClick(elemHandle)
    local mod = package.loaded["scripts.cargo_inventory_panel"]
    if mod and mod.handleTabClick then
        return mod.handleTabClick(elemHandle)
    end
    return false
end

function uiManager.onCargoInventoryItemRightClick(elemHandle)
    local mod = package.loaded["scripts.cargo_inventory_panel"]
    if mod and mod.handleItemRightClick then
        return mod.handleItemRightClick(elemHandle)
    end
    return false
end

function uiManager.onInventoryItemRightClick(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.handleInvItemRightClick then
        return mod.handleInvItemRightClick(elemHandle)
    end
    return false
end

function uiManager.onEquipSlotRightClick(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.handleEquipSlotRightClick then
        return mod.handleEquipSlotRightClick(elemHandle)
    end
    return false
end

function uiManager.onAccessoryRowRightClick(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.handleAccessoryRowRightClick then
        return mod.handleAccessoryRowRightClick(elemHandle)
    end
    return false
end

-- Click on the unit-info-v2 stats-panel transparent background. Pure
-- no-op; we register the click solely to keep the element in the
-- engine's clickable set so wheel events over the panel body route
-- through onUIScroll instead of being misread as world zoom.
function uiManager.onStatsPanelBgClick(elemHandle)
    local mod = package.loaded["scripts.unit_info_v2"]
    if mod and mod.onStatsPanelBgClick then
        return mod.onStatsPanelBgClick(elemHandle)
    end
    return false
end

-- Context menu click routes. Both fire as broadcast callbacks named in
-- scripts.ui.context_menu — items run the row's callback, backdrop
-- closes the menu without doing anything else.
function uiManager.onContextMenuItemClick(elemHandle)
    return contextMenu.handleItemClick(elemHandle)
end

function uiManager.onContextMenuBackdrop(elemHandle)
    return contextMenu.handleBackdropClick(elemHandle)
end

-- Keybind editor modal backdrop: clicking outside the capture/conflict
-- popup cancels the in-progress capture.
function uiManager.onKeybindPopupBackdrop(elemHandle)
    if uiManager.currentMenu == "settings"
       and settingsMenu.cancelKeyCapture then
        settingsMenu.cancelKeyCapture()
        return true
    end
    return false
end

-- Clicks on the popup body itself are swallowed (modal no-op) so they
-- don't fall through to the backdrop and cancel.
function uiManager.onKeybindPopupSwallow(elemHandle)
    return true
end
-- Note: "onEventLogRightClick" is broadcast to every loaded script —
-- hud.onEventLogRightClick handles it directly. No uiManager delegate
-- needed (and adding one would cause the menu to open twice).

function uiManager.onTabFrameScroll(elemHandle)
    return false
end

function uiManager.onLogPanelScroll(elemHandle)
    -- This callback exists so the right panel is clickable and receives
    -- scroll events. The actual scrolling is handled by onUIScroll.
    return false
end
