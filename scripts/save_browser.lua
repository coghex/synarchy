-- Save Browser - lists saved worlds using the list widget
local scale  = require("scripts.ui.scale")
local panel  = require("scripts.ui.panel")
local label  = require("scripts.ui.label")
local button = require("scripts.ui.button")
local list   = require("scripts.ui.list")

local saveBrowser = {}

saveBrowser.page = nil
saveBrowser.panelId = nil
saveBrowser.listId = nil
saveBrowser.saves = {}
saveBrowser.onSelectCallback = nil
saveBrowser.onBackCallback = nil
saveBrowser.menuFont = nil
saveBrowser.boxTexSet = nil
saveBrowser.btnTexSet = nil
saveBrowser.fbW = 0
saveBrowser.fbH = 0
saveBrowser.uiCreated = false
saveBrowser.showMenuCallback = nil

-- Owned IDs for cleanup
saveBrowser.ownedLabels  = {}
saveBrowser.ownedButtons = {}
saveBrowser.ownedPanels  = {}
saveBrowser.ownedLists   = {}

saveBrowser.baseSizes = {
    fontSize      = 24,
    titleFontSize = 32,
    itemHeight    = 40,
    maxVisible    = 12,
    panelPadX     = 60,
    panelPadY     = 60,
    tileSize      = 64,
    btnHeight     = 52,
    btnSpacing    = 16,
    btnPaddingX   = 80,
    btnTileSize   = 64,
    btnOverflow   = 16,
    textPadding   = 14,
    scrollBtnSize = 24,
}

-----------------------------------------------------------
-- Init (called once from ui_manager.checkReady)
-----------------------------------------------------------

function saveBrowser.init(boxTex, btnTex, font, width, height)
    saveBrowser.boxTexSet = boxTex
    saveBrowser.btnTexSet = btnTex
    saveBrowser.menuFont  = font
    saveBrowser.fbW       = width
    saveBrowser.fbH       = height
end

function saveBrowser.setShowMenuCallback(callback)
    saveBrowser.showMenuCallback = callback
end

-----------------------------------------------------------
-- Cleanup
-----------------------------------------------------------

function saveBrowser.destroyOwned()
    for _, id in ipairs(saveBrowser.ownedLists)   do list.destroy(id)   end
    for _, id in ipairs(saveBrowser.ownedLabels)   do label.destroy(id)  end
    for _, id in ipairs(saveBrowser.ownedButtons)  do button.destroy(id) end
    for _, id in ipairs(saveBrowser.ownedPanels)   do panel.destroy(id)  end
    saveBrowser.ownedLists   = {}
    saveBrowser.ownedLabels  = {}
    saveBrowser.ownedButtons = {}
    saveBrowser.ownedPanels  = {}
    saveBrowser.listId = nil
end

function saveBrowser.shutdown()
    saveBrowser.destroyOwned()
    if saveBrowser.page then
        UI.deletePage(saveBrowser.page)
        saveBrowser.page = nil
    end
end

-----------------------------------------------------------
-- Show / Hide
-----------------------------------------------------------

function saveBrowser.show(saves, onSelect, onBack)
    saveBrowser.saves = saves or engine.listSaves() or {}
    saveBrowser.onSelectCallback = onSelect
    saveBrowser.onBackCallback   = onBack
    saveBrowser.createUI()
    if saveBrowser.page then
        UI.showPage(saveBrowser.page)
    end
end

function saveBrowser.hide()
    if saveBrowser.page then
        UI.hidePage(saveBrowser.page)
    end
end

-----------------------------------------------------------
-- UI Build
-----------------------------------------------------------

function saveBrowser.createUI()
    saveBrowser.destroyOwned()
    if saveBrowser.page then
        UI.deletePage(saveBrowser.page)
    end

    if not saveBrowser.menuFont or not saveBrowser.boxTexSet then
        engine.logWarn("Save browser not initialized, skipping UI creation")
        return
    end

    local uiscale = scale.get()
    local s = scale.applyAllWith(saveBrowser.baseSizes, uiscale)

    saveBrowser.page = UI.newPage("save_browser", "modal")

    local saves = saveBrowser.saves

    -- Build list items
    local listItems = {}
    for i, save in ipairs(saves) do
        local displayText = save.name
        if save.timestamp then
            displayText = displayText .. "  -  " .. save.timestamp
        end
        table.insert(listItems, {
            text  = displayText,
            value = save.name,
        })
    end

    -- Panel sizing
    local visibleCount = math.min(#listItems, saveBrowser.baseSizes.maxVisible)
    if visibleCount < 1 then visibleCount = 1 end
    local listHeight = visibleCount * s.itemHeight

    local panelWidth  = math.floor(saveBrowser.fbW * 0.6)
    local contentHeight = s.titleFontSize + s.btnSpacing
                        + listHeight + s.btnSpacing
                        + s.btnHeight
    local panelHeight = s.panelPadY * 2 + contentHeight
    panelHeight = math.min(panelHeight, math.floor(saveBrowser.fbH * 0.85))

    local panelX = (saveBrowser.fbW - panelWidth) / 2
    local panelY = (saveBrowser.fbH - panelHeight) / 2

    saveBrowser.panelId = panel.new({
        name       = "save_browser_panel",
        page       = saveBrowser.page,
        x = panelX, y = panelY,
        width      = panelWidth,
        height     = panelHeight,
        textureSet = saveBrowser.boxTexSet,
        color      = {1.0, 1.0, 1.0, 1.0},
        tileSize   = s.tileSize,
        zIndex     = 1,
        padding    = {
            top = s.panelPadY, bottom = s.panelPadY,
            left = s.panelPadX, right = s.panelPadX,
        },
        uiscale = 1.0,
    })
    table.insert(saveBrowser.ownedPanels, saveBrowser.panelId)

    local baseZ  = panel.getZIndex(saveBrowser.panelId)
    local bounds = panel.getContentBounds(saveBrowser.panelId)

    -- Title
    local titleId = label.new({
        name     = "save_browser_title",
        text     = "Load Game",
        font     = saveBrowser.menuFont,
        fontSize = saveBrowser.baseSizes.titleFontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = saveBrowser.page,
        uiscale  = uiscale,
    })
    table.insert(saveBrowser.ownedLabels, titleId)

    local titleW, _ = label.getSize(titleId)
    local titleX = panelX + bounds.x + (bounds.width - titleW) / 2
    local titleY = panelY + bounds.y + s.titleFontSize
    UI.addToPage(saveBrowser.page,
        label.getElementHandle(titleId), titleX, titleY)
    UI.setZIndex(label.getElementHandle(titleId), baseZ + 1)

    -- List widget
    local listWidth = bounds.width - 20  -- leave room for potential scrollbar
    local listX = panelX + bounds.x + 10
    local listY = titleY + s.btnSpacing

    if #listItems > 0 then
        saveBrowser.listId = list.new({
            name           = "save_list",
            page           = saveBrowser.page,
            x              = listX,
            y              = listY,
            width          = listWidth,
            font           = saveBrowser.menuFont,
            fontSize       = saveBrowser.baseSizes.fontSize,
            itemHeight     = saveBrowser.baseSizes.itemHeight,
            textPadding    = saveBrowser.baseSizes.textPadding,
            scrollButtonSize = saveBrowser.baseSizes.scrollBtnSize,
            maxVisible     = saveBrowser.baseSizes.maxVisible,
            uiscale        = uiscale,
            zIndex         = baseZ + 2,
            items          = listItems,
            textColor           = {1.0, 1.0, 1.0, 1.0},
            highlightColor      = {0.3, 0.5, 0.8, 0.8},
            highlightTextColor  = {1.0, 1.0, 1.0, 1.0},
            selectedColor       = {0.2, 0.4, 0.7, 1.0},
            selectedTextColor   = {1.0, 1.0, 1.0, 1.0},
            onSelect = function(value, text, index, listId, listName)
                engine.logInfo("Save selected: " .. value)
                if saveBrowser.onSelectCallback then
                    saveBrowser.onSelectCallback(value)
                end
            end,
        })
        table.insert(saveBrowser.ownedLists, saveBrowser.listId)
    else
        -- No saves: show a message
        local noSavesId = label.new({
            name     = "no_saves_label",
            text     = "No saved games found.",
            font     = saveBrowser.menuFont,
            fontSize = saveBrowser.baseSizes.fontSize,
            color    = {0.7, 0.7, 0.7, 1.0},
            page     = saveBrowser.page,
            uiscale  = uiscale,
        })
        table.insert(saveBrowser.ownedLabels, noSavesId)
        local nsW, _ = label.getSize(noSavesId)
        local nsX = panelX + bounds.x + (bounds.width - nsW) / 2
        UI.addToPage(saveBrowser.page,
            label.getElementHandle(noSavesId), nsX, listY + s.itemHeight / 2)
        UI.setZIndex(label.getElementHandle(noSavesId), baseZ + 2)
    end

    -- Back button
    local backText = "Back"
    local backBtnId = button.new({
        name       = "save_browser_back",
        text       = backText,
        width      = 120,
        height     = saveBrowser.baseSizes.btnHeight,
        fontSize   = saveBrowser.baseSizes.fontSize,
        tileSize   = saveBrowser.baseSizes.btnTileSize,
        overflow   = saveBrowser.baseSizes.btnOverflow,
        uiscale    = uiscale,
        page       = saveBrowser.page,
        font       = saveBrowser.menuFont,
        textureSet = saveBrowser.btnTexSet,
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textColor  = {1.0, 1.0, 1.0, 1.0},
        callbackName = "onSaveBrowserBack",
    })
    table.insert(saveBrowser.ownedButtons, backBtnId)

    local btnW, btnH = button.getSize(backBtnId)
    local btnX = panelX + bounds.x + (bounds.width - btnW) / 2
    local btnY = listY + listHeight + s.btnSpacing
    UI.addToPage(saveBrowser.page,
        button.getElementHandle(backBtnId), btnX, btnY)
    UI.setZIndex(button.getElementHandle(backBtnId), baseZ + 2)

    saveBrowser.uiCreated = true
    engine.logInfo("Save browser created with " .. #saves .. " saves")
end

-----------------------------------------------------------
-- Scroll events (forwarded from ui_manager)
-----------------------------------------------------------

function saveBrowser.onScroll(elemHandle, dx, dy)
    if saveBrowser.listId then
        return list.onScroll(elemHandle, dx, dy)
    end
    return false
end

function saveBrowser.handleScrollCallback(callbackName, elemHandle)
    if saveBrowser.listId then
        return list.handleCallback(callbackName, elemHandle)
    end
    return false
end

-----------------------------------------------------------
-- Resize
-----------------------------------------------------------

function saveBrowser.onFramebufferResize(width, height)
    saveBrowser.fbW = width
    saveBrowser.fbH = height
    if saveBrowser.uiCreated and saveBrowser.page then
        saveBrowser.createUI()
    end
end

return saveBrowser
