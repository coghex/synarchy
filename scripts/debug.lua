-- Debug overlay module
local debugOverlay = {}

local page = nil
local fpsText = nil
local debugFont = nil
local visible = false
local uiCreated = false

-- Configuration
local baseFontSize = 24
local baseMargin = 10

function debugOverlay.init(scriptId)
    engine.logInfo("Debug overlay initializing...")
    
    debugFont = engine.loadFont("assets/fonts/shell.ttf", baseFontSize)
    engine.logInfo("Debug font loaded: " .. tostring(debugFont))
    
    engine.logDebug("Debug overlay initialized")
end

function debugOverlay.createUI()
    if uiCreated and page then
        UI.deletePage(page)
    end
    
    local uiscale = engine.getUIScale()
    local fontSize = math.floor(baseFontSize * uiscale)
    local margin = math.floor(baseMargin * uiscale)
    
    -- Create overlay page
    page = UI.newPage("debug_overlay", "overlay")
    
    -- FPS text in top-left corner
    fpsText = UI.newText(
        "fps_text",
        "FPS: --",
        debugFont,
        fontSize,
        0.0, 1.0, 0.0, 1.0,  -- Green color
        page
    )
    UI.addToPage(page, fpsText, margin, margin + fontSize)
    UI.setZIndex(fpsText, 1000)
    
    uiCreated = true
    engine.logDebug("Debug UI created")
end

function debugOverlay.update(dt)
    if not visible then return end
    
    -- Get FPS from engine (updated every second in Haskell)
    local fps = engine.getFPS()
    
    if fpsText then
        UI.setText(fpsText, "FPS: " .. tostring(math.floor(fps)))
    end
end

function debugOverlay.show()
    if not uiCreated then
        debugOverlay.createUI()
    end
    
    visible = true
    
    if page then
        UI.showPage(page)
    end
    
    engine.logInfo("Debug overlay shown")
end

function debugOverlay.hide()
    visible = false
    
    if page then
        UI.hidePage(page)
    end
    
    engine.logInfo("Debug overlay hidden")
end

function debugOverlay.toggle()
    if visible then
        debugOverlay.hide()
    else
        debugOverlay.show()
    end
end

function debugOverlay.isVisible()
    return visible
end

function debugOverlay.onFramebufferResize(width, height)
    if uiCreated then
        debugOverlay.createUI()
        if visible then
            UI.showPage(page)
        end
    end
end

function debugOverlay.shutdown()
    if page then
        UI.hidePage(page)
        UI.deletePage(page)
        page = nil
    end
end

function debugOverlay.onKeyDown(key)
    if key == "F3" then
        debugOverlay.toggle()
    end
end


return debugOverlay
