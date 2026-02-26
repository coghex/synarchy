-- Bottom Buttons for Create World Menu
-- Manages the dynamic button bar: idle set (Back/Defaults/Generate),
-- generating set (Back/Defaults/ProgressBar), and
-- done set (Back/Defaults/Regenerate/Continue).
local button = require("scripts.ui.button")
local bar    = require("scripts.ui.bar")

local bottomButtons = {}

-----------------------------------------------------------
-- Shared layout helper: computes btnY and left-side positions
-----------------------------------------------------------

local function computeLayout(params)
    local L       = params.btnLayout
    local uiscale = L.uiscale
    local s       = L.s
    local menu    = params.menu
    local base    = params.baseSizes

    -- Back button
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

    -- Defaults button
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

    local backW, backH = button.getSize(menu.backButtonId)
    local defaultsW, _ = button.getSize(menu.defaultsButtonId)

    local bottomPad = math.floor(100 * uiscale)
    local btnY = L.panelY + L.panelHeight - bottomPad
               + (bottomPad - backH) / 2

    local backX     = L.panelX + L.bounds.x
    local defaultsX = backX + backW + s.btnSpacing

    UI.setPosition(button.getElementHandle(menu.backButtonId),     backX,     btnY)
    UI.setPosition(button.getElementHandle(menu.defaultsButtonId), defaultsX, btnY)
    UI.setZIndex(button.getElementHandle(menu.backButtonId),     params.zButtons)
    UI.setZIndex(button.getElementHandle(menu.defaultsButtonId), params.zButtons)

    return {
        btnY       = btnY,
        backW      = backW,
        backH      = backH,
        defaultsW  = defaultsW,
        defaultsX  = defaultsX,
    }
end

-----------------------------------------------------------
-- Build the "idle" button set: [Back] [Defaults]  [Generate World]
-----------------------------------------------------------

function bottomButtons.buildIdle(params)
    bottomButtons.destroyAll(params.menu)

    local L       = params.btnLayout
    local uiscale = L.uiscale
    local menu    = params.menu
    local base    = params.baseSizes

    local layout = computeLayout(params)

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

    local generateW, _ = button.getSize(menu.generateButtonId)
    local generateX = L.panelX + L.bounds.x + L.bounds.width - generateW

    UI.setPosition(button.getElementHandle(menu.generateButtonId), generateX, layout.btnY)
    UI.setZIndex(button.getElementHandle(menu.generateButtonId), params.zButtons)
end

-----------------------------------------------------------
-- Build the "generating" set: [Back] [Defaults]  [===ProgressBar===]
-----------------------------------------------------------

function bottomButtons.buildGenerating(params)
    bottomButtons.destroyAll(params.menu)

    local L       = params.btnLayout
    local uiscale = L.uiscale
    local menu    = params.menu
    local base    = params.baseSizes

    local layout = computeLayout(params)

    -- Progress bar in the same slot as the Generate/Continue button
    local barWidth = base.generateBtnWidth
    local barHeight = base.btnHeight
    local scaledBarW = math.floor(barWidth * uiscale)
    local barX = L.panelX + L.bounds.x + L.bounds.width - scaledBarW
    local barY = layout.btnY

    menu.genBarId = bar.new({
        name           = "gen_progress_bar",
        page           = params.page,
        x              = barX,
        y              = barY,
        width          = barWidth,
        height         = barHeight,
        capWidth       = 8,
        trackLeftTex   = params.barTextures.trackLeft,
        trackCenterTex = params.barTextures.trackCenter,
        trackRightTex  = params.barTextures.trackRight,
        fillLeftTex    = params.barTextures.fillLeft,
        fillCenterTex  = params.barTextures.fillCenter,
        color          = {0.4, 0.4, 0.4, 1.0},
        fillColor      = {0.2, 0.7, 0.3, 1.0},
        font           = params.menuFont,
        fontSize       = base.fontSize,
        textColor      = {1.0, 0.2, 0.2, 1.0},
        uiscale        = uiscale,
        zIndex         = params.zButtons,
        progress       = 0,
    })
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

    local layout = computeLayout(params)

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

    local regenW, _    = button.getSize(menu.regenerateButtonId)
    local contW, _     = button.getSize(menu.continueButtonId)

    local regenX    = layout.defaultsX + layout.defaultsW + s.btnSpacing
    local contX     = L.panelX + L.bounds.x + L.bounds.width - contW

    UI.setPosition(button.getElementHandle(menu.regenerateButtonId), regenX,    layout.btnY)
    UI.setPosition(button.getElementHandle(menu.continueButtonId),   contX,     layout.btnY)
    UI.setZIndex(button.getElementHandle(menu.regenerateButtonId), params.zButtons)
    UI.setZIndex(button.getElementHandle(menu.continueButtonId),   params.zButtons)
end

-----------------------------------------------------------
-- Destroy all dynamic buttons + bar
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

    -- Destroy progress bar if present
    if menu.genBarId then
        bar.destroy(menu.genBarId)
        menu.genBarId = nil
    end

    menu.backButtonId       = nil
    menu.defaultsButtonId   = nil
    menu.generateButtonId   = nil
    menu.regenerateButtonId = nil
    menu.continueButtonId   = nil
end

return bottomButtons
