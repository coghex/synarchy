-- Pause Menu Module (in-game escape menu)
local scale = require("scripts.ui.scale")
local responsive = require("scripts.ui.responsive")
local panel = require("scripts.ui.panel")
local label = require("scripts.ui.label")
local pauseMenu = {}

pauseMenu.page = nil
pauseMenu.panelId = nil
pauseMenu.boxTexSet = nil
pauseMenu.menuFont = nil
pauseMenu.titleFont = nil
pauseMenu.fbW = 0
pauseMenu.fbH = 0
pauseMenu.uiCreated = false
pauseMenu.visible = false

pauseMenu.titleLabelId = nil

-- Owned element IDs for scoped cleanup
pauseMenu.ownedLabels  = {}
pauseMenu.ownedBoxes   = {}
pauseMenu.ownedPanels  = {}

-- Click dispatch table: elemHandle -> function
pauseMenu.clickHandlers = {}

-- Callback to switch menus (set by ui_manager)
pauseMenu.showMenuCallback = nil

-- Base sizes (same as main menu)
pauseMenu.baseSizes = {
    titleFontSize = 96,
    fontSize = 32,
    buttonHeight = 80,
    buttonSpacing = 24,
    buttonTileSize = 64,
    buttonPaddingX = 160,
    buttonOverflow = 16,
    menuPaddingX = 100,
    menuPaddingY = 100,
    menuTileSize = 64,
    menuOverflow = 0,
    titleOffset = 80,
}

function pauseMenu.buildMenuItems()
    local items = {}

    if pauseMenu.showSave then
        table.insert(items, {
            name = "save",
            label = "Save",
            onClick = function() pauseMenu.onSave() end,
        })
    end

    table.insert(items, {
        name = "settings",
        label = "Settings",
        onClick = function() pauseMenu.onSettings() end,
    })
    table.insert(items, {
        name = "exit_to_menu",
        label = "Exit to Menu",
        onClick = function() pauseMenu.onExitToMenu() end,
    })
    table.insert(items, {
        name = "exit_to_desktop",
        label = "Exit to Desktop",
        onClick = function() pauseMenu.onExitToDesktop() end,
    })

    return items
end

-----------------------------------------------------------
-- Scoped cleanup
-----------------------------------------------------------

function pauseMenu.destroyOwned()
    for _, id in ipairs(pauseMenu.ownedLabels) do label.destroy(id) end
    for _, h  in ipairs(pauseMenu.ownedBoxes)  do UI.deleteElement(h) end
    for _, id in ipairs(pauseMenu.ownedPanels) do panel.destroy(id) end
    pauseMenu.ownedLabels  = {}
    pauseMenu.ownedBoxes   = {}
    pauseMenu.ownedPanels  = {}
    pauseMenu.clickHandlers = {}
end

function pauseMenu.setShowMenuCallback(callback)
    pauseMenu.showMenuCallback = callback
end

function pauseMenu.init(boxTex, btnTex, font, tFont, width, height)
    pauseMenu.boxTexSet = boxTex
    pauseMenu.menuFont = font
    pauseMenu.titleFont = tFont
    pauseMenu.fbW = width
    pauseMenu.fbH = height
end

function pauseMenu.createUI()
    if pauseMenu.uiCreated then
        if pauseMenu.page then
            UI.deletePage(pauseMenu.page)
            pauseMenu.destroyOwned()
            pauseMenu.titleLabelId = nil
        end
    end

    local menuItems = pauseMenu.buildMenuItems()

    local uiscale = scale.get()
    local s = scale.applyAll(pauseMenu.baseSizes)

    -- #748: compact fallback — see main_menu.lua's identical comment.
    -- Shrinks this menu's own effective scale only, never the stored
    -- UI scale, so the panel + title stay in-frame regardless of item
    -- count or configured scale.
    local naturalMenuHeight = #menuItems * (s.buttonHeight + s.buttonSpacing)
                             + s.buttonSpacing + s.menuPaddingY
    local maxMenuHeight = math.floor(pauseMenu.fbH * 0.9)
    uiscale = responsive.fitScale(
        naturalMenuHeight + s.titleOffset, maxMenuHeight, uiscale)
    s = scale.applyAllWith(pauseMenu.baseSizes, uiscale)

    pauseMenu.page = UI.newPage("pause_menu", "modal")

    -- Calculate max text width for sizing
    local maxLabelWidth = 0
    for _, item in ipairs(menuItems) do
        local w = engine.getTextWidth(pauseMenu.menuFont, item.label, s.fontSize)
        if w > maxLabelWidth then maxLabelWidth = w end
    end

    local itemWidth  = maxLabelWidth + s.buttonPaddingX
    local menuHeight = #menuItems * (s.buttonHeight + s.buttonSpacing)
                     + s.buttonSpacing + s.menuPaddingY
    local menuWidth  = itemWidth + s.menuPaddingX

    local menuX = (pauseMenu.fbW - menuWidth) / 2
    local menuY = (pauseMenu.fbH - menuHeight) / 2
    -- Clamp so the title never goes off the top edge, even if the
    -- compact fallback above still leaves things a little tight.
    menuY = math.max(menuY, s.titleOffset + 4)

    -- Background panel
    pauseMenu.panelId = panel.new({
        name       = "pause_panel",
        page       = pauseMenu.page,
        x = menuX, y = menuY,
        width      = menuWidth,
        height     = menuHeight,
        textureSet = pauseMenu.boxTexSet,
        color      = {1.0, 1.0, 1.0, 1.0},
        tileSize   = s.menuTileSize,
        overflow   = s.menuOverflow,
        zIndex     = 1,
        padding    = {
            top    = s.menuPaddingY / 2,
            bottom = s.menuPaddingY / 2,
            left   = s.menuPaddingX / 2,
            right  = s.menuPaddingX / 2,
        },
        uiscale = 1.0,
    })
    table.insert(pauseMenu.ownedPanels, pauseMenu.panelId)

    local baseZ = panel.getZIndex(pauseMenu.panelId)

    -- Title
    pauseMenu.titleLabelId = label.new({
        name     = "pause_title",
        text     = "Paused",
        font     = pauseMenu.titleFont,
        fontSize = pauseMenu.baseSizes.titleFontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = pauseMenu.page,
        uiscale  = uiscale,
    })
    table.insert(pauseMenu.ownedLabels, pauseMenu.titleLabelId)

    local titleW, _ = label.getSize(pauseMenu.titleLabelId)
    local titleX = (pauseMenu.fbW - titleW) / 2
    local titleY = menuY - s.titleOffset
    UI.addToPage(pauseMenu.page,
        label.getElementHandle(pauseMenu.titleLabelId), titleX, titleY)

    -- Menu items
    local boxElements = {}
    local boxSizes    = {}

    for _, item in ipairs(menuItems) do
        local lblW = engine.getTextWidth(pauseMenu.menuFont, item.label, s.fontSize)
        local thisW = lblW + s.buttonPaddingX
        local thisH = s.buttonHeight

        local boxH = UI.newBox(
            item.name .. "_pause_box",
            thisW, thisH,
            pauseMenu.boxTexSet,
            s.buttonTileSize,
            1.0, 1.0, 1.0, 1.0,
            s.buttonOverflow,
            pauseMenu.page
        )
        table.insert(pauseMenu.ownedBoxes, boxH)

        UI.setClickable(boxH, true)
        UI.setOnClick(boxH, "onPauseMenuItem")

        pauseMenu.clickHandlers[boxH] = item.onClick

        local textH = UI.newText(
            item.name .. "_pause_label",
            item.label,
            pauseMenu.menuFont,
            s.fontSize,
            1.0, 1.0, 1.0, 1.0,
            pauseMenu.page
        )

        local labelX = (thisW - lblW) / 2
        local labelY = (thisH / 2) + (s.fontSize / 2)
        UI.addChild(boxH, textH, labelX, labelY)
        UI.setZIndex(textH, 1)

        UI.setZIndex(boxH, baseZ + 1)

        table.insert(boxElements, boxH)
        table.insert(boxSizes, { width = thisW, height = thisH })
    end

    panel.placeColumn(
        pauseMenu.panelId,
        boxElements,
        boxSizes,
        {
            x = "50%",
            y = "50%",
            origin = "center",
            spacing = s.buttonSpacing,
        }
    )

    pauseMenu.uiCreated = true
end

-----------------------------------------------------------
-- Click dispatch (called from ui_manager.onPauseMenuItem)
-----------------------------------------------------------

function pauseMenu.handleClick(elemHandle)
    local handler = pauseMenu.clickHandlers[elemHandle]
    if handler then
        handler()
        return true
    end
    return false
end

-----------------------------------------------------------
-- Callbacks
-----------------------------------------------------------

function pauseMenu.onSave()
    engine.logInfo("Pause menu: Save")
    local worldManager = require("scripts.world_manager")
    if worldManager.currentWorld then
        -- engine.saveWorld auto-pauses the engine (sets enginePausedRef)
        -- for a clean snapshot, but it does NOT touch the world
        -- timescale. Pausing through the pause module first keeps
        -- enginePausedRef and the world clock (wsTimeScaleRef) in sync —
        -- otherwise the save leaves a half-paused world (unit/AI ticks
        -- frozen, but time-of-day still advancing), and a later
        -- Space-resume restores a stale timescale because prevTimeScale
        -- was never snapshotted. pause.set captures the player's current
        -- scale and zeroes the clock so resume returns to it. (Opening
        -- this menu does not itself pause; the game runs underneath.)
        require("scripts.pause").set(true)
        -- Route through the canonical save path (engine.saveWorld with
        -- validation + async write event). engine.save is not a
        -- registered API; calling it crashed the Save action.
        require("scripts.world_view").saveGame()
    end
    pauseMenu.hide()
end

function pauseMenu.onSettings()
    engine.logInfo("Pause menu: Settings")
    if pauseMenu.showMenuCallback then
        pauseMenu.showMenuCallback("settings")
    end
end

function pauseMenu.onExitToMenu()
    engine.logInfo("Pause menu: Exit to Menu")
    local worldManager = require("scripts.world_manager")
    -- Clear transient build-tool placement so an armed action can't carry
    -- into the next world (#82).
    pcall(function() require("scripts.build_tool").exitPlacement() end)
    -- Clear any pending mine-tool anchor so the next world's first click
    -- doesn't commit a rectangle from a stale origin (#102).
    pcall(function() require("scripts.mine_tool").cancel() end)
    if worldManager.currentWorld then
        world.hide(worldManager.currentWorld)
    end
    -- Destroy EVERY world, not just currentWorld — a hidden leftover (e.g.
    -- a test arena) would otherwise stay in wmWorlds and resolveActiveWorld's
    -- head-fallback would keep resolving it as the implicit active world
    -- behind the menu (#58). destroyAll also clears the unit/building
    -- managers so no entities leak into the next game.
    world.destroyAll()
    worldManager.active = false
    worldManager.currentWorld = nil
    -- Clear the paused-engine flag so it can't leak into the next game
    -- (Save auto-pauses; Exit must undo that). pause.set keeps the pause
    -- module's own state in sync; the explicit setPaused is a belt-and-
    -- braces in case the module wasn't the one that paused (#58).
    pcall(function() require("scripts.pause").set(false) end)
    engine.setPaused(false)
    if pauseMenu.showMenuCallback then
        pauseMenu.showMenuCallback("main")
    end
end

function pauseMenu.onExitToDesktop()
    engine.quit()
end

-----------------------------------------------------------
-- Show / Hide / Toggle
-----------------------------------------------------------

function pauseMenu.show(opts)
    opts = opts or {}
    if opts.showSave ~= nil then
        pauseMenu.showSave = opts.showSave
    end
    -- The pause menu is a non-gameplay overlay (isGameplayInputActive
    -- treats it as "menu on top"), but it opens via its own modal page
    -- without going through hud.hide() or uiManager.showMenu(). Hide the
    -- F8 debug overlay here too so it doesn't survive onto the pause
    -- screen, where it would stay visible and clickable (#147). Done
    -- before createUI so it happens regardless of the pause UI build.
    pcall(function() require("scripts.debug").hide() end)
    -- Same story for an in-flight drag-select box (#146): the pause menu
    -- bypasses hud.hide(), so an armed/dragging box would survive onto
    -- the pause screen and could commit afterward. cancel() abandons it
    -- (idempotent no-op when idle).
    pcall(function() require("scripts.unit_drag_select").cancel() end)
    pauseMenu.createUI()
    if pauseMenu.page then
        UI.showPage(pauseMenu.page)
        pauseMenu.visible = true
    end
end

function pauseMenu.hide()
    if pauseMenu.page then
        UI.hidePage(pauseMenu.page)
        pauseMenu.visible = false
    end
end

function pauseMenu.toggle(opts)
    if pauseMenu.visible then
        pauseMenu.hide()
    else
        pauseMenu.show(opts)
    end
end

function pauseMenu.onFramebufferResize(width, height)
    pauseMenu.fbW = width
    pauseMenu.fbH = height
    if pauseMenu.uiCreated and pauseMenu.visible then
        pauseMenu.createUI()
        UI.showPage(pauseMenu.page)
    end
end

function pauseMenu.shutdown()
    pauseMenu.destroyOwned()
    if pauseMenu.page then
        UI.deletePage(pauseMenu.page)
    end
end

return pauseMenu
