-- UI Manager: scroll/wheel routing (#544 split from ui_manager.lua).
--
-- Owns the scrollbar up/down BUTTON click routes (onScrollUp/
-- onScrollDown), mouse-WHEEL routing over UI elements (onUIScroll),
-- game-view scroll with no UI element under the cursor (onScroll —
-- world zoom), and shift+scroll z-slice paging (onZSliceScroll).
-- currentMenu lives on the uiManager singleton since it's the
-- authoritative view every branch below gates on.
local uiManager = package.loaded["scripts.ui_manager"]

local dropdown        = require("scripts.ui.dropdown")
local settingsMenu    = require("scripts.settings_menu")
local createWorldMenu = require("scripts.create_world_menu")
local saveBrowser     = require("scripts.save_browser")
local combatLog       = require("scripts.combat_log")
local injuryLog       = require("scripts.injury_log_panel")
local unitLog         = require("scripts.unit_log")
local eventLog        = require("scripts.event_log")
local worldView       = require("scripts.world_view")
local testArena       = require("scripts.test_arena")
local hud             = require("scripts.hud")

-----------------------------------------------------------
-- Scroll Button Clicks (scrollbar up/down arrows)
-----------------------------------------------------------

function uiManager.onScrollUp(elemHandle)
    local handled = dropdown.handleCallback("onScrollUp", elemHandle)
    if handled then return true end

    local currentMenu = uiManager.currentMenu
    if currentMenu == "settings" and settingsMenu.handleScrollCallback then
        if settingsMenu.handleScrollCallback("onScrollUp", elemHandle) then
            return true
        end
    end
    if currentMenu == "create_world" and createWorldMenu.handleScrollCallback then
        if createWorldMenu.handleScrollCallback("onScrollUp", elemHandle) then
            return true
        end
    end
    if currentMenu == "save_browser" and saveBrowser.handleScrollCallback then
        if saveBrowser.handleScrollCallback("onScrollUp", elemHandle) then
            return true
        end
    end
    if combatLog.isVisible and combatLog.isVisible()
       and combatLog.handleScrollCallback then
        if combatLog.handleScrollCallback("onScrollUp", elemHandle) then
            return true
        end
    end
    if injuryLog.isVisible and injuryLog.isVisible()
       and injuryLog.handleScrollCallback then
        if injuryLog.handleScrollCallback("onScrollUp", elemHandle) then
            return true
        end
    end
    if unitLog.isVisible and unitLog.isVisible()
       and unitLog.handleScrollCallback then
        if unitLog.handleScrollCallback("onScrollUp", elemHandle) then
            return true
        end
    end
    -- Unit info v2 has its own stats-panel scrollbar.
    local uimod = package.loaded["scripts.unit_info_v2"]
    if uimod and uimod.handleScrollCallback then
        if uimod.handleScrollCallback("onScrollUp", elemHandle) then
            return true
        end
    end
    return false
end

function uiManager.onScrollDown(elemHandle)
    local handled = dropdown.handleCallback("onScrollDown", elemHandle)
    if handled then return true end

    local currentMenu = uiManager.currentMenu
    if currentMenu == "settings" and settingsMenu.handleScrollCallback then
        if settingsMenu.handleScrollCallback("onScrollDown", elemHandle) then
            return true
        end
    end
    if currentMenu == "create_world" and createWorldMenu.handleScrollCallback then
        if createWorldMenu.handleScrollCallback("onScrollDown", elemHandle) then
            return true
        end
    end
    if currentMenu == "save_browser" and saveBrowser.handleScrollCallback then
        if saveBrowser.handleScrollCallback("onScrollDown", elemHandle) then
            return true
        end
    end
    if combatLog.isVisible and combatLog.isVisible()
       and combatLog.handleScrollCallback then
        if combatLog.handleScrollCallback("onScrollDown", elemHandle) then
            return true
        end
    end
    if injuryLog.isVisible and injuryLog.isVisible()
       and injuryLog.handleScrollCallback then
        if injuryLog.handleScrollCallback("onScrollDown", elemHandle) then
            return true
        end
    end
    if unitLog.isVisible and unitLog.isVisible()
       and unitLog.handleScrollCallback then
        if unitLog.handleScrollCallback("onScrollDown", elemHandle) then
            return true
        end
    end
    local uimod = package.loaded["scripts.unit_info_v2"]
    if uimod and uimod.handleScrollCallback then
        if uimod.handleScrollCallback("onScrollDown", elemHandle) then
            return true
        end
    end
    return false
end

-----------------------------------------------------------
-- Mouse Wheel Scroll (UI elements)
-----------------------------------------------------------

-- shiftHeld (#744) is the modifier state routing already resolved --
-- this element won the scroll-capture search regardless of Shift, but
-- the flag rides along so a handler that cares CAN tell modified from
-- unmodified wheel input. No current handler needs to.
function uiManager.onUIScroll(elemHandle, dx, dy, shiftHeld)
    local handled = dropdown.onScroll(elemHandle, dx, dy)
    if handled then return end

    local currentMenu = uiManager.currentMenu
    if currentMenu == "settings" and settingsMenu.onScroll(elemHandle, dx, dy) then
        return
    end
    if currentMenu == "create_world" and createWorldMenu.onScroll(elemHandle, dx, dy) then
        return
    end
    if currentMenu == "save_browser" and saveBrowser.onScroll(elemHandle, dx, dy) then
        return
    end
    if combatLog.isVisible and combatLog.isVisible()
       and combatLog.onScroll then
        if combatLog.onScroll(elemHandle, dx, dy) then
            return
        end
    end
    if injuryLog.isVisible and injuryLog.isVisible()
       and injuryLog.onScroll then
        if injuryLog.onScroll(elemHandle, dx, dy) then
            return
        end
    end
    if unitLog.isVisible and unitLog.isVisible()
       and unitLog.onScroll then
        if unitLog.onScroll(elemHandle, dx, dy) then
            return
        end
    end
    if eventLog.isVisible and eventLog.isVisible()
       and eventLog.onScroll then
        if eventLog.onScroll(elemHandle, dx, dy) then
            return
        end
    end
    -- Planting screen (#335) crop-list scrollbar.
    local plantPanel = package.loaded["scripts.plant_panel"]
    if plantPanel and plantPanel.isOpen and plantPanel.isOpen()
       and plantPanel.onScroll then
        if plantPanel.onScroll(elemHandle, dx, dy) then
            return
        end
    end
    -- Unit info v2 stats-panel scrollbar.
    local uimod = package.loaded["scripts.unit_info_v2"]
    if uimod and uimod.onScroll then
        if uimod.onScroll(elemHandle, dx, dy) then
            return
        end
    end
end

-----------------------------------------------------------
-- Game Scroll (no UI element under cursor, no shift)
-----------------------------------------------------------

-- #744: the engine (Engine.Input.Thread.Scroll.dispatchScrollEvent)
-- now decides modal-boundary blocking itself, upstream of this
-- broadcast — a wheel miss stopped at a modal boundary (no
-- scroll-capturing element in scope AND isGameplayBlocked) never
-- reaches here at all, on top of a game-scroll AND a z-slice event
-- alike. Pre-#744 this function carried its own UI.isInputBlocked()
-- re-check to cover exactly that case; keeping it here too, now that
-- the engine already guarantees it, would just be the same rule
-- enforced in two places that could drift apart.
function uiManager.onScroll(dx, dy)
    local currentMenu = uiManager.currentMenu
    if currentMenu == "world_view" then
        worldView.onScroll(dx, dy)
        hud.onScroll(dx, dy)
    elseif currentMenu == "test_arena" or currentMenu == "test_arena_view" then
        testArena.onScroll(dx, dy)
        hud.onScroll(dx, dy)
    end
end

-----------------------------------------------------------
-- Z-Slice Scroll (shift+scroll)
-----------------------------------------------------------

-- #744: Shift+wheel now shares the exact same engine-side routing as
-- plain wheel (Engine.Input.Thread.Scroll.dispatchScrollEvent) —
-- cursor conversion, the degenerate-viewport guard, scroll-capture,
-- AND the modal-boundary check, all before this broadcast ever fires.
-- Pre-#744, Shift+wheel bypassed routing entirely on the engine side,
-- so this function carried its own compensating UI.isInputBlocked()
-- check to keep a Shift-wheel over a visible modal's empty space from
-- changing the z-slice behind it. That compensating check is gone now
-- that the engine enforces it once, upstream, for both wheel kinds
-- alike — see uiManager.onScroll above for the same reasoning.
function uiManager.onZSliceScroll(dx, dy)
    local currentMenu = uiManager.currentMenu
    if currentMenu == "world_view" then
        worldView.onZSliceScroll(dx, dy)
    elseif currentMenu == "test_arena" or currentMenu == "test_arena_view" then
        testArena.onZSliceScroll(dx, dy)
    end
end
