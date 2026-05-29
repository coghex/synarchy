-- Loading Screen
-- Displays a progress bar during world generation or save loading.
-- Polls world.getInitProgress() each frame.
--
-- The Haskell API returns three values:
--   phase:   0 = idle, 1 = setup, 2 = chunks, 3 = done
--   current: step number (phase 1) or chunks generated (phase 2)
--   total:   total steps (phase 1) or total chunks (phase 2)
--
-- Phase 1 maps to 0%–50% of the bar (synchronous setup work).
-- Phase 2 maps to 50%–100% of the bar (chunk generation).
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
loadingScreen.mode            = "worldgen"  -- "worldgen" | "startup"
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
    barCapWidth = 32,
    barFontSize  = 16,
    panelPadX   = 60,
    panelPadY   = 60,
    tileSize    = 64,
    spacing     = 40,
}

-----------------------------------------------------------
-- Phase 1 step labels (matches the order in World.Thread)
-----------------------------------------------------------
local phase1Labels = {
    [1] = "Building geological timeline...",
    [2] = "Computing ocean map...",
    [3] = "Initializing climate...",
    [4] = "Building zoom cache...",
    [5] = "Rendering world preview...",
    [6] = "Generating center chunk...",
    [7] = "Queuing chunks...",
    [8] = "Registering world...",
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

    -- No background panel — bar + labels are placed directly against
    -- the page, centered. Layout is the same vertical stack as before:
    --   [status text]
    --   [spacing]
    --   [progress bar]
    --   [spacing]
    --   [percent text]
    local stackHeight = s.fontSize + s.spacing + s.barHeight
                      + s.spacing + s.fontSize
    local stackY = math.floor((loadingScreen.fbH - stackHeight) / 2)
    local baseZ  = 1

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

    -- Status text is left-justified to the bar's left edge so the
    -- two read as a unit. (Bar is centered horizontally below.)
    local barLeft = math.floor((loadingScreen.fbW - s.barWidth) / 2)
    local statusX = barLeft
    local statusY = stackY
    UI.addToPage(loadingScreen.page,
        label.getElementHandle(loadingScreen.statusLabelId), statusX, statusY)
    UI.setZIndex(label.getElementHandle(loadingScreen.statusLabelId), baseZ + 1)

    -- Progress bar
    local barX = barLeft
    local barY = statusY + s.fontSize + s.spacing

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
    local pctX = math.floor((loadingScreen.fbW - pctW) / 2)
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
    loadingScreen.mode       = params.mode or "worldgen"
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

    -- Startup mode: drive startup_loader (which owns the asset queue)
    -- and reflect its progress into the bar / labels. We deliberately
    -- do NOT fire showMenuCallback here — ui_manager polls our phase
    -- and runs finishStartupBoot (which inits menus) before pushing
    -- the main menu.
    if loadingScreen.mode == "startup" then
        local startupLoader = require("scripts.startup_loader")
        startupLoader.tick(dt)

        local progress, statusText = startupLoader.getProgress()

        if loadingScreen.barId then
            bar.setProgress(loadingScreen.barId, progress)
            bar.setText(loadingScreen.barId, "")
        end
        local pctInt = math.floor(progress * 100)
        if loadingScreen.percentLabelId then
            label.setText(loadingScreen.percentLabelId,
                tostring(pctInt) .. "%")
        end
        if loadingScreen.statusLabelId then
            label.setText(loadingScreen.statusLabelId, statusText)
        end

        if startupLoader.isDone() then
            loadingScreen.phase = "done"
            engine.logInfo("Startup loader complete")
        end
        return
    end

    local wphase, current, total = world.getInitProgress()

    -- phase 0 = idle (world not registered yet), nothing to show
    if not wphase or wphase == 0 then return end

    local progress = 0
    local barText  = ""

    if wphase == 1 then
        -- Phase 1: synchronous setup → maps to 0%..50%
        if total and total > 0 then
            progress = (current / total) * 0.5
        end
        local stepLabel = phase1Labels[current] or
            ("Setup " .. tostring(current) .. "/" .. tostring(total))
        barText = stepLabel

    elseif wphase == 2 then
        -- Phase 2: chunk generation → maps to 50%..100%
        if total and total > 0 then
            progress = 0.5 + (current / total) * 0.5
        else
            progress = 0.5
        end
        barText = "Chunks " .. tostring(current) .. " / " .. tostring(total)

    elseif wphase == 3 then
        -- Done
        progress = 1.0
        barText  = ""
    end

    -- Update bar
    if loadingScreen.barId then
        bar.setProgress(loadingScreen.barId, progress)
        bar.setText(loadingScreen.barId, barText)
    end

    -- Update percentage label
    local pctInt = math.floor(progress * 100)
    if loadingScreen.percentLabelId then
        label.setText(loadingScreen.percentLabelId,
            tostring(pctInt) .. "%")
    end

    -- Update status label with the current step description
    if loadingScreen.statusLabelId and wphase == 1 then
        local stepLabel = phase1Labels[current]
        if stepLabel then
            label.setText(loadingScreen.statusLabelId, stepLabel)
        end
    elseif loadingScreen.statusLabelId and wphase == 2 then
        label.setText(loadingScreen.statusLabelId, "Generating terrain...")
    end

    -- Transition to world view when done
    if wphase == 3 then
        loadingScreen.phase = "done"

        if loadingScreen.barId then
            bar.setProgress(loadingScreen.barId, 1.0)
            bar.setText(loadingScreen.barId, "")
        end
        if loadingScreen.percentLabelId then
            label.setText(loadingScreen.percentLabelId, "100%")
        end
        if loadingScreen.statusLabelId then
            label.setText(loadingScreen.statusLabelId, "Complete!")
        end

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
