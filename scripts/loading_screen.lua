-- Loading Screen
-- Displays a progress bar during world generation or save loading.
-- Polls world.getInitProgress() each frame.
local scale = require("scripts.ui.scale")
local panel = require("scripts.ui.panel")
local label = require("scripts.ui.label")
local bar   = require("scripts.ui.bar")

local loadingScreen = {}

loadingScreen.page       = nil
loadingScreen.panelId    = nil
loadingScreen.barId      = nil
loadingScreen.statusLabelId = nil
loadingScreen.percentLabelId = nil
loadingScreen.boxTexSet  = nil
loadingScreen.menuFont   = nil
loadingScreen.fbW        = 0
loadingScreen.fbH        = 0
loadingScreen.uiCreated  = false
loadingScreen.showMenuCallback = nil

-- Tracking
loadingScreen.phase           = "idle"  -- "idle", "loading", "done"
loadingScreen.statusText      = "Loading..."

-- Texture handles for bar (loaded once)
loadingScreen.barTextures = {
    trackLeft   = nil,
    trackCenter = nil,
    trackRight  = nil,
    fillLeft    = nil,
    fillCenter  = nil,
}

-- Owned elements for cleanup
loadingScreen.ownedLabels  = {}
loadingScreen.ownedPanels  = {}
loadingScreen.ownedBars    = {}

loadingScreen.baseSizes = {
    fontSize    = 24,
    barWidth    = 500,
    barHeight   = 32,
    barCapWidth = 8,
    barFontSize  = 16,
    panelPadX   = 60,
    panelPadY   = 60,
    tileSize    = 64,
    spacing     = 20,
}

-----------------------------------------------------------
-- Init (called once from ui_manager.checkReady)
-----------------------------------------------------------

function loadingScreen.init(boxTex, font, width, height)
    loadingScreen.boxTexSet = boxTex
    loadingScreen.menuFont  = font
    loadingScreen.fbW       = width
    loadingScreen.fbH       = height

    loadingScreen.barTextures.trackLeft   = engine.loadTexture("assets/textures/ui/bar/bar_left.png")
    loadingScreen.barTextures.trackCenter = engine.loadTexture("assets/textures/ui/bar/bar_center.png")
    loadingScreen.barTextures.trackRight  = engine.loadTexture("assets/textures/ui/bar/bar_right.png")
    loadingScreen.barTextures.fillLeft    = engine.loadTexture("assets/textures/ui/bar/bar_fill_left.png")
    loadingScreen.barTextures.fillCenter  = engine.loadTexture("assets/textures/ui/bar/bar_fill_center.png")
end

function loadingScreen.setShowMenuCallback(callback)
    loadingScreen.showMenuCallback = callback
end

-----------------------------------------------------------
-- Cleanup
-----------------------------------------------------------

function loadingScreen.destroyOwned()
    for _, id in ipairs(loadingScreen.ownedBars)    do bar.destroy(id)   end
    for _, id in ipairs(loadingScreen.ownedLabels)  do label.destroy(id) end
    for _, id in ipairs(loadingScreen.ownedPanels)  do panel.destroy(id) end
    loadingScreen.ownedBars   = {}
    loadingScreen.ownedLabels = {}
    loadingScreen.ownedPanels = {}
    loadingScreen.barId          = nil
    loadingScreen.statusLabelId  = nil
    loadingScreen.percentLabelId = nil
end

-----------------------------------------------------------
-- UI Build
-----------------------------------------------------------

function loadingScreen.createUI()
    loadingScreen.destroyOwned()
    if loadingScreen.page then
        UI.deletePage(loadingScreen.page)
    end

    local uiscale = scale.get()
    local s = scale.applyAllWith(loadingScreen.baseSizes, uiscale)

    loadingScreen.page = UI.newPage("loading_screen", "modal")

    -- s.barWidth is already scaled, so use it directly for layout
    local panelWidth  = s.barWidth + s.panelPadX * 2
    local panelHeight = s.fontSize + s.spacing + s.barHeight
                      + s.spacing + s.fontSize + s.panelPadY * 2
    local panelX = math.floor((loadingScreen.fbW - panelWidth) / 2)
    local panelY = math.floor((loadingScreen.fbH - panelHeight) / 2)

    loadingScreen.panelId = panel.new({
        name       = "loading_panel",
        page       = loadingScreen.page,
        x = panelX, y = panelY,
        width      = panelWidth,
        height     = panelHeight,
        textureSet = loadingScreen.boxTexSet,
        color      = {1.0, 1.0, 1.0, 1.0},
        tileSize   = s.tileSize,
        zIndex     = 1,
        padding    = {
            top = s.panelPadY, bottom = s.panelPadY,
            left = s.panelPadX, right = s.panelPadX,
        },
        uiscale = 1.0,  -- panel dimensions already scaled
    })
    table.insert(loadingScreen.ownedPanels, loadingScreen.panelId)

    local baseZ  = panel.getZIndex(loadingScreen.panelId)
    local bounds = panel.getContentBounds(loadingScreen.panelId)

    -- Status text
    loadingScreen.statusLabelId = label.new({
        name     = "loading_status",
        text     = loadingScreen.statusText,
        font     = loadingScreen.menuFont,
        fontSize = loadingScreen.baseSizes.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = loadingScreen.page,
        uiscale  = uiscale,
    })
    table.insert(loadingScreen.ownedLabels, loadingScreen.statusLabelId)

    local statusW, _ = label.getSize(loadingScreen.statusLabelId)
    local statusX = panelX + bounds.x + math.floor((bounds.width - statusW) / 2)
    local statusY = panelY + bounds.y + s.fontSize
    UI.addToPage(loadingScreen.page,
        label.getElementHandle(loadingScreen.statusLabelId), statusX, statusY)
    UI.setZIndex(label.getElementHandle(loadingScreen.statusLabelId), baseZ + 1)

    -- Progress bar — bar.new scales internally, so pass BASE (unscaled) sizes
    -- but position using the already-scaled layout coordinates
    local barScaledW = s.barWidth
    local barX = panelX + bounds.x + math.floor((bounds.width - barScaledW) / 2)
    local barY = statusY + s.spacing

    loadingScreen.barId = bar.new({
        name           = "loading_bar",
        page           = loadingScreen.page,
        x              = barX,
        y              = barY,
        width          = loadingScreen.baseSizes.barWidth,
        height         = loadingScreen.baseSizes.barHeight,
        capWidth       = loadingScreen.baseSizes.barCapWidth,
        trackLeftTex   = loadingScreen.barTextures.trackLeft,
        trackCenterTex = loadingScreen.barTextures.trackCenter,
        trackRightTex  = loadingScreen.barTextures.trackRight,
        fillLeftTex    = loadingScreen.barTextures.fillLeft,
        fillCenterTex  = loadingScreen.barTextures.fillCenter,
        color          = {0.4, 0.4, 0.4, 1.0},
        fillColor      = {0.2, 0.7, 0.3, 1.0},
        font           = loadingScreen.menuFont,
        fontSize       = loadingScreen.baseSizes.barFontSize,
        textColor      = {1.0, 0.2, 0.2, 1.0},
        uiscale        = uiscale,
        zIndex         = baseZ + 2,
        progress       = 0,
    })
    table.insert(loadingScreen.ownedBars, loadingScreen.barId)

    -- Percentage text
    loadingScreen.percentLabelId = label.new({
        name     = "loading_percent",
        text     = "0%",
        font     = loadingScreen.menuFont,
        fontSize = loadingScreen.baseSizes.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = loadingScreen.page,
        uiscale  = uiscale,
    })
    table.insert(loadingScreen.ownedLabels, loadingScreen.percentLabelId)

    local pctW, _ = label.getSize(loadingScreen.percentLabelId)
    local pctX = panelX + bounds.x + math.floor((bounds.width - pctW) / 2)
    local pctY = barY + s.barHeight + s.spacing
    UI.addToPage(loadingScreen.page,
        label.getElementHandle(loadingScreen.percentLabelId), pctX, pctY)
    UI.setZIndex(label.getElementHandle(loadingScreen.percentLabelId), baseZ + 1)

    loadingScreen.uiCreated = true
end

-----------------------------------------------------------
-- Show / Hide
-----------------------------------------------------------

function loadingScreen.show(params)
    params = params or {}
    loadingScreen.statusText = params.statusText or "Loading..."

    -- Always grab the latest framebuffer size from the caller
    if params.fbW and params.fbW > 0 then
        loadingScreen.fbW = params.fbW
    end
    if params.fbH and params.fbH > 0 then
        loadingScreen.fbH = params.fbH
    end

    loadingScreen.phase = "loading"

    loadingScreen.createUI()
    if loadingScreen.page then
        UI.showPage(loadingScreen.page)
    end
    engine.logInfo("Loading screen shown: " .. loadingScreen.statusText
        .. " (" .. loadingScreen.fbW .. "x" .. loadingScreen.fbH .. ")")
end

function loadingScreen.hide()
    loadingScreen.phase = "idle"
    if loadingScreen.page then
        UI.hidePage(loadingScreen.page)
    end
end

-----------------------------------------------------------
-- Update (called every frame from ui_manager)
-----------------------------------------------------------

function loadingScreen.update(dt)
    if loadingScreen.phase ~= "loading" then return end

    local remaining, total = world.getInitProgress()

    if not total or total == 0 then return end

    local generated = total - (remaining or 0)
    local progress = generated / total

    if loadingScreen.barId then
        bar.setProgress(loadingScreen.barId, progress)

        -- Optional overlay text from the Haskell side
        local barText = world.getInitStatus and world.getInitStatus() or nil
        if barText and barText ~= "" then
            bar.setText(loadingScreen.barId, barText)
        elseif remaining > 0 then
            bar.setText(loadingScreen.barId,
                tostring(generated) .. " / " .. tostring(total))
        else
            bar.setText(loadingScreen.barId, "")
        end
    end

    local pctInt = math.floor(progress * 100)
    if loadingScreen.percentLabelId then
        label.setText(loadingScreen.percentLabelId,
            tostring(pctInt) .. "%")
    end

    if remaining == 0 then
        loadingScreen.phase = "done"
        bar.setProgress(loadingScreen.barId, 1.0)
        bar.setText(loadingScreen.barId, "")
        label.setText(loadingScreen.percentLabelId, "100%")
        engine.logInfo("Loading complete, transitioning to world view")

        if loadingScreen.showMenuCallback then
            loadingScreen.showMenuCallback("world_view")
        end
    end
end

-----------------------------------------------------------
-- Resize
-----------------------------------------------------------

function loadingScreen.onFramebufferResize(width, height)
    loadingScreen.fbW = width
    loadingScreen.fbH = height
    if loadingScreen.uiCreated and loadingScreen.page then
        loadingScreen.createUI()
    end
end

-----------------------------------------------------------
-- Shutdown
-----------------------------------------------------------

function loadingScreen.shutdown()
    loadingScreen.destroyOwned()
    if loadingScreen.page then
        UI.deletePage(loadingScreen.page)
        loadingScreen.page = nil
    end
end

return loadingScreen
