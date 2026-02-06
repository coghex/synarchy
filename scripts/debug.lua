-- Debug overlay module
local scale = require("scripts.ui.scale")
local label = require("scripts.ui.label")
local debugOverlay = {}

debugOverlay.page = nil
debugOverlay.fpsLabelId = nil
debugOverlay.debugFont = nil
debugOverlay.visible = false
debugOverlay.uiCreated = false

-- Base sizes (unscaled)
debugOverlay.baseSizes = {
    fontSize = 32,
    margin = 10,
}

function debugOverlay.init(scriptId)
    engine.logInfo("Debug overlay initializing...")
    
    debugOverlay.debugFont = engine.loadFont("assets/fonts/shell.ttf", debugOverlay.baseSizes.fontSize)
    engine.logInfo("Debug font loaded: " .. tostring(debugOverlay.debugFont))
    
    engine.logDebug("Debug overlay initialized")
end

function debugOverlay.createUI()
    if debugOverlay.uiCreated and debugOverlay.page then
        UI.deletePage(debugOverlay.page)
        label.destroyAll()
        debugOverlay.fpsLabelId = nil
    end
    
    local s = scale.applyAll(debugOverlay.baseSizes)
    local uiscale = scale.get()
    
    debugOverlay.page = UI.newPage("debug_overlay", "overlay")
    
    -- FPS label in top-left corner
    debugOverlay.fpsLabelId = label.new({
        name = "fps_text",
        text = "FPS: --",
        font = debugOverlay.debugFont,
        fontSize = debugOverlay.baseSizes.fontSize,
        color = {0.0, 1.0, 0.0, 1.0},  -- Green
        page = debugOverlay.page,
        uiscale = uiscale,
        x = s.margin,
        y = s.margin + s.fontSize,
        zIndex = 1000,
    })
    
    debugOverlay.uiCreated = true
    engine.logDebug("Debug UI created")
end

function debugOverlay.update(dt)
    if not debugOverlay.visible then return end
    
    local fps = engine.getFPS()
    
    if debugOverlay.fpsLabelId then
        label.setText(debugOverlay.fpsLabelId, "FPS: " .. tostring(math.floor(fps)))
    end
end

function debugOverlay.show()
    if not debugOverlay.uiCreated then
        debugOverlay.createUI()
    end
    
    debugOverlay.visible = true
    
    if debugOverlay.page then
        UI.showPage(debugOverlay.page)
    end
    
    engine.logInfo("Debug overlay shown")
end

function debugOverlay.hide()
    debugOverlay.visible = false
    
    if debugOverlay.page then
        UI.hidePage(debugOverlay.page)
    end
    
    engine.logInfo("Debug overlay hidden")
end

function debugOverlay.toggle()
    if debugOverlay.visible then
        debugOverlay.hide()
    else
        debugOverlay.show()
    end
end

function debugOverlay.isVisible()
    return debugOverlay.visible
end

function debugOverlay.onFramebufferResize(width, height)
    if debugOverlay.uiCreated then
        debugOverlay.createUI()
        if debugOverlay.visible then
            UI.showPage(debugOverlay.page)
        end
    end
end

function debugOverlay.shutdown()
    label.destroyAll()
    if debugOverlay.page then
        UI.hidePage(debugOverlay.page)
        UI.deletePage(debugOverlay.page)
        debugOverlay.page = nil
    end
end

function debugOverlay.onKeyDown(key)
    if key == "F3" then
        debugOverlay.toggle()
    end
end

return debugOverlay
