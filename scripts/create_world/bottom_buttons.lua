-- Bottom Buttons for Create World Menu
-- Manages the dynamic button bar: idle set (Back/Defaults/Generate)
-- and done set (Back/Defaults/Regenerate/Continue).
local button = require("scripts.ui.button")

local bottomButtons = {}

-----------------------------------------------------------
-- Build the "idle" button set: [Back] [Defaults]  [Generate World]
-----------------------------------------------------------

-- params = {
--   menu       = createWorldMenu ref,
--   page, menuFont, buttonTexSet, baseSizes, btnLayout,
--   zButtons,
--   trackButton,
--   onBack, onDefaults, onGenerateWorld,
-- }
function bottomButtons.buildIdle(params)
    bottomButtons.destroyAll(params.menu)

    local L       = params.btnLayout
    local uiscale = L.uiscale
    local s       = L.s
    local menu    = params.menu
    local base    = params.baseSizes

    menu.backButtonId = params.trackButton(button.new({
        name       = "back_btn",
        text       = "Back",
        width      = base.btnWidth,
        height     = base.btnHeight,
        fontSize   = base.fontSize,
        uiscale    = uiscale,
        page       = params.page,
        font       = params.menuFont,
        textureSet = params.buttonTexSet,
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textColor  = {0.0, 0.0, 0.0, 1.0},
        zIndex     = params.zButtons,
        onClick    = function() params.onBack() end,
    }))

    menu.defaultsButtonId = params.trackButton(button.new({
        name       = "defaults_btn",
        text       = "Defaults",
        width      = base.btnWidth,
        height     = base.btnHeight,
        fontSize   = base.fontSize,
        uiscale    = uiscale,
        page       = params.page,
        font       = params.menuFont,
        textureSet = params.buttonTexSet,
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textColor  = {0.0, 0.0, 0.0, 1.0},
        zIndex     = params.zButtons,
        onClick    = function() params.onDefaults() end,
    }))

    menu.generateButtonId = params.trackButton(button.new({
        name       = "generate_btn",
        text       = "Generate World",
        width      = base.generateBtnWidth,
        height     = base.btnHeight,
        fontSize   = base.fontSize,
        uiscale    = uiscale,
        page       = params.page,
        font       = params.menuFont,
        textureSet = params.buttonTexSet,
        bgColor    = {0.2, 0.8, 0.2, 1.0},
        textColor  = {1.0, 1.0, 1.0, 1.0},
        zIndex     = params.zButtons,
        onClick    = function() params.onGenerateWorld() end,
    }))

    -- Layout
    local backW, backH = button.getSize(menu.backButtonId)
    local defaultsW, _ = button.getSize(menu.defaultsButtonId)
    local generateW, _ = button.getSize(menu.generateButtonId)

    local bottomPad = math.floor(100 * uiscale)
    local btnY = L.panelY + L.panelHeight - bottomPad
               + (bottomPad - backH) / 2

    local backX     = L.panelX + L.bounds.x
    local defaultsX = backX + backW + s.btnSpacing
    local generateX = L.panelX + L.bounds.x + L.bounds.width - generateW

    UI.setPosition(button.getElementHandle(menu.backButtonId),     backX,     btnY)
    UI.setPosition(button.getElementHandle(menu.defaultsButtonId), defaultsX, btnY)
    UI.setPosition(button.getElementHandle(menu.generateButtonId), generateX, btnY)

    UI.setZIndex(button.getElementHandle(menu.backButtonId),     params.zButtons)
    UI.setZIndex(button.getElementHandle(menu.defaultsButtonId), params.zButtons)
    UI.setZIndex(button.getElementHandle(menu.generateButtonId), params.zButtons)
end

-----------------------------------------------------------
-- Build the "done" button set:
--   [Back] [Defaults] [Regenerate]     [Continue]
-----------------------------------------------------------

function bottomButtons.buildDone(params)
    bottomButtons.destroyAll(params.menu)

    local L       = params.btnLayout
    local uiscale = L.uiscale
    local s       = L.s
    local menu    = params.menu
    local base    = params.baseSizes

    menu.backButtonId = params.trackButton(button.new({
        name       = "back_btn",
        text       = "Back",
        width      = base.btnWidth,
        height     = base.btnHeight,
        fontSize   = base.fontSize,
        uiscale    = uiscale,
        page       = params.page,
        font       = params.menuFont,
        textureSet = params.buttonTexSet,
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textColor  = {0.0, 0.0, 0.0, 1.0},
        zIndex     = params.zButtons,
        onClick    = function() params.onBack() end,
    }))

    menu.defaultsButtonId = params.trackButton(button.new({
        name       = "defaults_btn",
        text       = "Defaults",
        width      = base.btnWidth,
        height     = base.btnHeight,
        fontSize   = base.fontSize,
        uiscale    = uiscale,
        page       = params.page,
        font       = params.menuFont,
        textureSet = params.buttonTexSet,
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textColor  = {0.0, 0.0, 0.0, 1.0},
        zIndex     = params.zButtons,
        onClick    = function() params.onDefaults() end,
    }))

    menu.regenerateButtonId = params.trackButton(button.new({
        name       = "regenerate_btn",
        text       = "Regenerate",
        width      = base.btnWidth,
        height     = base.btnHeight,
        fontSize   = base.fontSize,
        uiscale    = uiscale,
        page       = params.page,
        font       = params.menuFont,
        textureSet = params.buttonTexSet,
        bgColor    = {0.8, 0.6, 0.1, 1.0},
        textColor  = {1.0, 1.0, 1.0, 1.0},
        zIndex     = params.zButtons,
        onClick    = function() params.onRegenerate() end,
    }))

    menu.continueButtonId = params.trackButton(button.new({
        name       = "continue_btn",
        text       = "Continue",
        width      = base.generateBtnWidth,
        height     = base.btnHeight,
        fontSize   = base.fontSize,
        uiscale    = uiscale,
        page       = params.page,
        font       = params.menuFont,
        textureSet = params.buttonTexSet,
        bgColor    = {0.2, 0.8, 0.2, 1.0},
        textColor  = {1.0, 1.0, 1.0, 1.0},
        zIndex     = params.zButtons,
        onClick    = function() params.onContinue() end,
    }))

    -- Layout
    local backW, backH = button.getSize(menu.backButtonId)
    local defaultsW, _ = button.getSize(menu.defaultsButtonId)
    local regenW, _    = button.getSize(menu.regenerateButtonId)
    local contW, _     = button.getSize(menu.continueButtonId)

    local bottomPad = math.floor(100 * uiscale)
    local btnY = L.panelY + L.panelHeight - bottomPad
               + (bottomPad - backH) / 2

    local backX     = L.panelX + L.bounds.x
    local defaultsX = backX + backW + s.btnSpacing
    local regenX    = defaultsX + defaultsW + s.btnSpacing
    local contX     = L.panelX + L.bounds.x + L.bounds.width - contW

    UI.setPosition(button.getElementHandle(menu.backButtonId),       backX,     btnY)
    UI.setPosition(button.getElementHandle(menu.defaultsButtonId),   defaultsX, btnY)
    UI.setPosition(button.getElementHandle(menu.regenerateButtonId), regenX,    btnY)
    UI.setPosition(button.getElementHandle(menu.continueButtonId),   contX,     btnY)

    UI.setZIndex(button.getElementHandle(menu.backButtonId),       params.zButtons)
    UI.setZIndex(button.getElementHandle(menu.defaultsButtonId),   params.zButtons)
    UI.setZIndex(button.getElementHandle(menu.regenerateButtonId), params.zButtons)
    UI.setZIndex(button.getElementHandle(menu.continueButtonId),   params.zButtons)
end

-----------------------------------------------------------
-- Destroy all dynamic buttons
-----------------------------------------------------------

function bottomButtons.destroyAll(menu)
    local function destroyAndUntrack(id)
        if not id then return end
        local handle = button.getElementHandle(id)
        if handle then
            UI.deleteElement(handle)
        end
        button.destroy(id)
        for i = #menu.ownedButtons, 1, -1 do
            if menu.ownedButtons[i] == id then
                table.remove(menu.ownedButtons, i)
                break
            end
        end
    end

    destroyAndUntrack(menu.backButtonId)
    destroyAndUntrack(menu.defaultsButtonId)
    destroyAndUntrack(menu.generateButtonId)
    destroyAndUntrack(menu.regenerateButtonId)
    destroyAndUntrack(menu.continueButtonId)

    menu.backButtonId       = nil
    menu.defaultsButtonId   = nil
    menu.generateButtonId   = nil
    menu.regenerateButtonId = nil
    menu.continueButtonId   = nil
end

return bottomButtons
