-- Save Browser - lists all saved worlds for loading
local scale = require("scripts.ui.scale")
local panel = require("scripts.ui.panel")
local label = require("scripts.ui.label")

local saveBrowser = {}

saveBrowser.page = nil
saveBrowser.panelId = nil
saveBrowser.saves = {}
saveBrowser.ownedLabels = {}
saveBrowser.ownedBoxes = {}
saveBrowser.ownedPanels = {}
saveBrowser.clickHandlers = {}
saveBrowser.onSelectCallback = nil
saveBrowser.onBackCallback = nil
saveBrowser.menuFont = nil
saveBrowser.boxTexSet = nil
saveBrowser.fbW = 0
saveBrowser.fbH = 0
saveBrowser.uiCreated = false

saveBrowser.baseSizes = {
    fontSize      = 24,
    rowHeight     = 60,
    rowSpacing    = 12,
    rowPaddingX   = 120,
    titleFontSize = 32,
    panelPadX     = 60,
    panelPadY     = 60,
    tileSize      = 64,
    backBtnWidth  = 200,
    backBtnHeight = 52,
}

-----------------------------------------------------------
-- Init (called once from ui_manager.checkReady or on first show)
-----------------------------------------------------------

function saveBrowser.init(boxTex, font, width, height)
    saveBrowser.boxTexSet = boxTex
    saveBrowser.menuFont  = font
    saveBrowser.fbW       = width
    saveBrowser.fbH       = height
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
-- Cleanup
-----------------------------------------------------------

function saveBrowser.destroyOwned()
    for _, id in ipairs(saveBrowser.ownedLabels) do label.destroy(id) end
    for _, h  in ipairs(saveBrowser.ownedBoxes)  do UI.deleteElement(h) end
    for _, id in ipairs(saveBrowser.ownedPanels) do panel.destroy(id) end
    saveBrowser.ownedLabels  = {}
    saveBrowser.ownedBoxes   = {}
    saveBrowser.ownedPanels  = {}
    saveBrowser.clickHandlers = {}
end

function saveBrowser.shutdown()
    saveBrowser.destroyOwned()
    if saveBrowser.page then
        UI.deletePage(saveBrowser.page)
        saveBrowser.page = nil
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
    -- +1 row for the Back button, +1 for title
    local rowCount = #saves + 1
    local panelHeight = s.panelPadY * 2
                      + s.titleFontSize + s.rowSpacing
                      + rowCount * (s.rowHeight + s.rowSpacing)
    local panelWidth  = math.floor(saveBrowser.fbW * 0.6)
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
    local baseZ = panel.getZIndex(saveBrowser.panelId)
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

    -- Save rows
    local rowElements = {}
    local rowSizes    = {}
    local rowY = titleY + s.titleFontSize + s.rowSpacing

    for i, save in ipairs(saves) do
        local displayText = save.name
        if save.timestamp then
            displayText = displayText .. "  —  " .. save.timestamp
        end
        if save.seed then
            displayText = displayText .. "  (seed: " .. tostring(save.seed) .. ")"
        end

        local textW = engine.getTextWidth(
            saveBrowser.menuFont, displayText, s.fontSize)
        local rowW = math.max(textW + s.rowPaddingX, bounds.width - 20)

        local boxH = UI.newBox(
            "save_row_" .. i .. "_box",
            rowW, s.rowHeight,
            saveBrowser.boxTexSet,
            s.tileSize,
            0.9, 0.9, 0.9, 1.0,
            0,
            saveBrowser.page
        )
        table.insert(saveBrowser.ownedBoxes, boxH)

        UI.setClickable(boxH, true)
        UI.setOnClick(boxH, "onSaveBrowserItem")

        local saveName = save.name  -- capture for closure
        saveBrowser.clickHandlers[boxH] = function()
            if saveBrowser.onSelectCallback then
                saveBrowser.onSelectCallback(saveName)
            end
        end

        local textH = UI.newText(
            "save_row_" .. i .. "_text",
            displayText,
            saveBrowser.menuFont,
            s.fontSize,
            0.0, 0.0, 0.0, 1.0,
            saveBrowser.page
        )
        local lblX = s.rowPaddingX / 2
        local lblY = (s.rowHeight / 2) + (s.fontSize / 2)
        UI.addChild(boxH, textH, lblX, lblY)
        UI.setZIndex(textH, 1)
        UI.setZIndex(boxH, baseZ + 2)

        table.insert(rowElements, boxH)
        table.insert(rowSizes, { width = rowW, height = s.rowHeight })
    end

    -- Back button (same raw box+label style)
    local backText = "Back"
    local backTextW = engine.getTextWidth(
        saveBrowser.menuFont, backText, s.fontSize)
    local backW = backTextW + s.rowPaddingX

    local backBoxH = UI.newBox(
        "save_browser_back_box",
        backW, s.backBtnHeight,
        saveBrowser.boxTexSet,
        s.tileSize,
        1.0, 1.0, 1.0, 1.0,
        0,
        saveBrowser.page
    )
    table.insert(saveBrowser.ownedBoxes, backBoxH)

    UI.setClickable(backBoxH, true)
    UI.setOnClick(backBoxH, "onSaveBrowserItem")
    saveBrowser.clickHandlers[backBoxH] = function()
        if saveBrowser.onBackCallback then
            saveBrowser.onBackCallback()
        end
    end

    local backTextH = UI.newText(
        "save_browser_back_text",
        backText,
        saveBrowser.menuFont,
        s.fontSize,
        1.0, 1.0, 1.0, 1.0,
        saveBrowser.page
    )
    local backLblX = (backW - backTextW) / 2
    local backLblY = (s.backBtnHeight / 2) + (s.fontSize / 2)
    UI.addChild(backBoxH, backTextH, backLblX, backLblY)
    UI.setZIndex(backTextH, 1)
    UI.setZIndex(backBoxH, baseZ + 2)

    table.insert(rowElements, backBoxH)
    table.insert(rowSizes, { width = backW, height = s.backBtnHeight })

    -- Layout all rows in a column
    panel.placeColumn(
        saveBrowser.panelId,
        rowElements,
        rowSizes,
        {
            x = "50%",
            y = "50%",
            origin = "center",
            spacing = s.rowSpacing,
        }
    )

    saveBrowser.uiCreated = true
    engine.logInfo("Save browser created with " .. #saves .. " saves")
end

-----------------------------------------------------------
-- Click dispatch (called from ui_manager.onSaveBrowserItem)
-----------------------------------------------------------

function saveBrowser.handleClick(elemHandle)
    local handler = saveBrowser.clickHandlers[elemHandle]
    if handler then
        handler()
        return true
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
