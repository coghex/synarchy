-- Bottom Buttons for Create World Menu
-- Manages the dynamic button bar: idle set (Back/Defaults/Generate),
-- generating set (Back/Defaults/ProgressBar), and
-- done set (Back/Defaults/Regenerate/Continue).
local button = require("scripts.ui.button")
local bar    = require("scripts.ui.bar")

local bottomButtons = {}

-----------------------------------------------------------
-- #748: responsive width shrink
--
-- The button bar's base widths (btnWidth=270, generateBtnWidth=360)
-- are sized for a typical desktop window; at the supported envelope's
-- 800px-wide minimum, the "done" set (Back+Defaults+Regenerate+
-- Continue, the widest of the three button sets) doesn't fit its
-- natural single-row width — Idle's Generate button used to overlap
-- Defaults there. Rather than reflowing to multiple rows (which would
-- also need the panel's own bottom padding / content-height split to
-- change), every button width shrinks by one uniform factor computed
-- from the WIDEST set (done) so the bar always fits in one row and
-- never jumps size when genState changes (idle/generating share the
-- same factor as done, even though they need less room).
-----------------------------------------------------------

local function computeButtonScaleFactor(base, boundsWidth, uiscale)
    local maxNeededBase = base.btnWidth * 3 + base.generateBtnWidth
                         + base.btnSpacing * 3
    local maxNeededPx = maxNeededBase * uiscale
    if maxNeededPx <= 0 or maxNeededPx <= boundsWidth then
        return 1.0
    end
    return boundsWidth / maxNeededPx
end

-----------------------------------------------------------
-- Shared layout helper: computes btnY and left-side positions
-----------------------------------------------------------

local function computeLayout(params)
    local L       = params.btnLayout
    local uiscale = L.uiscale
    local s       = L.s
    local menu    = params.menu
    local base    = params.baseSizes

    local factor  = computeButtonScaleFactor(base, L.bounds.width, uiscale)
    local btnW    = math.floor(base.btnWidth * factor)
    local spacing = math.floor(s.btnSpacing * factor)
    -- #748 round 10: shrinking only the button BOX width left labels
    -- rendering at the unshrunk base font size — at the supported
    -- 800x600@1x combination the "done" set's boxes shrink to ~128-171px
    -- but "Regenerate"/"Generate World" still rendered at the full
    -- 24px font, overflowing into neighboring controls. Apply the SAME
    -- factor to fontSize (mirrors settings_menu.createButtons' identical
    -- round-6 fix) so button.new's own `fontSize * uiscale` internal
    -- math shrinks both by the identical ratio.
    local btnFontSize = base.fontSize * factor

    -- Back button
    menu.backButtonId = params.trackButton(button.new({
        name       = "back_btn",
        text       = "Back",
        width      = btnW,
        height     = base.btnHeight,
        fontSize   = btnFontSize,
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
        width      = btnW,
        height     = base.btnHeight,
        fontSize   = btnFontSize,
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
    local defaultsX = backX + backW + spacing

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
        factor     = factor,
        spacing    = spacing,
        btnFontSize = btnFontSize,
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
    local primaryW = math.floor(base.generateBtnWidth * layout.factor)

    menu.generateButtonId = params.trackButton(button.new({
        name       = "generate_btn",
        text       = "Generate World",
        width      = primaryW,
        height     = base.btnHeight,
        fontSize   = layout.btnFontSize,
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
    -- bar.new (like button.new) multiplies width by uiscale itself, so
    -- barWidthBase is the UNSCALED value passed in; scaledBarW is only
    -- for the X-position math here, matching button.getSize's contract
    -- on the sibling Generate/Continue buttons.
    local barWidthBase = math.floor(base.generateBtnWidth * layout.factor)
    local scaledBarW = math.floor(barWidthBase * uiscale)
    local barHeight = base.btnHeight
    local barX = L.panelX + L.bounds.x + L.bounds.width - scaledBarW
    local barY = layout.btnY

    menu.genBarId = bar.new({
        name           = "gen_progress_bar",
        page           = params.page,
        x              = barX,
        y              = barY,
        width          = barWidthBase,
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
        fontSize       = layout.btnFontSize,
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
    local menu    = params.menu
    local base    = params.baseSizes

    local layout = computeLayout(params)
    local btnW     = math.floor(base.btnWidth * layout.factor)
    local primaryW = math.floor(base.generateBtnWidth * layout.factor)

    menu.regenerateButtonId = params.trackButton(button.new({
        name       = "regenerate_btn",
        text       = "Regenerate",
        width      = btnW,
        height     = base.btnHeight,
        fontSize   = layout.btnFontSize,
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
        width      = primaryW,
        height     = base.btnHeight,
        fontSize   = layout.btnFontSize,
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

    local regenX    = layout.defaultsX + layout.defaultsW + layout.spacing
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
