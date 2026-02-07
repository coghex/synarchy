-- Settings Data Module
-- Owns all settings state, validation, and engine config calls.
-- Has zero UI knowledge.
local data = {}

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------

data.uiScaleMin    = 0.5
data.uiScaleMax    = 4.0
data.frameLimitMin = 30
data.frameLimitMax = 240
data.brightnessMin = 50
data.brightnessMax = 300

-- Standard resolutions
data.resolutions = {
    { text = "1280x720",   value = "1280x720",   width = 1280,  height = 720 },
    { text = "1366x768",   value = "1366x768",   width = 1366,  height = 768 },
    { text = "1600x900",   value = "1600x900",   width = 1600,  height = 900 },
    { text = "1920x1080",  value = "1920x1080",  width = 1920,  height = 1080 },
    { text = "2560x1440",  value = "2560x1440",  width = 2560,  height = 1440 },
    { text = "3840x2160",  value = "3840x2160",  width = 3840,  height = 2160 },
    { text = "1280x800",   value = "1280x800",   width = 1280,  height = 800 },
    { text = "1440x900",   value = "1440x900",   width = 1440,  height = 900 },
    { text = "1680x1050",  value = "1680x1050",  width = 1680,  height = 1050 },
    { text = "1920x1200",  value = "1920x1200",  width = 1920,  height = 1200 },
    { text = "2560x1600",  value = "2560x1600",  width = 2560,  height = 1600 },
    { text = "800x600",    value = "800x600",     width = 800,   height = 600 },
    { text = "1024x768",   value = "1024x768",    width = 1024,  height = 768 },
    { text = "1600x1200",  value = "1600x1200",   width = 1600,  height = 1200 },
    { text = "2560x1080",  value = "2560x1080",  width = 2560,  height = 1080 },
    { text = "3440x1440",  value = "3440x1440",  width = 3440,  height = 1440 },
}

-- Window modes
data.windowModes = {
    { text = "Fullscreen",          value = "fullscreen" },
    { text = "Borderless Windowed", value = "borderless" },
    { text = "Windowed",            value = "windowed" },
}

-- MSAA options
data.msaaOptions = {
    { text = "Off", value = "0" },
    { text = "2x",  value = "2" },
    { text = "4x",  value = "4" },
    { text = "8x",  value = "8" },
}

-----------------------------------------------------------
-- State
-----------------------------------------------------------

data.current = {
    width      = 800,
    height     = 600,
    windowMode = "fullscreen",  -- "fullscreen", "borderless", "windowed"
    uiScale    = 1.0,
    vsync      = true,
    frameLimit = 60,
    msaa       = 0,
    brightness = 100,
}

data.pending = {}

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

function data.resolutionString(w, h)
    return tostring(w) .. "x" .. tostring(h)
end

function data.findResolutionIndex(w, h)
    local target = data.resolutionString(w, h)
    for i, res in ipairs(data.resolutions) do
        if res.value == target then
            return i
        end
    end
    return nil
end

function data.msaaToString(msaa)
    return tostring(msaa or 0)
end

function data.msaaFromString(str)
    local n = tonumber(str)
    if n == 2 or n == 4 or n == 8 then return n end
    return 0
end

function data.resetPending()
    data.pending = {
        width      = data.current.width,
        height     = data.current.height,
        windowMode = data.current.windowMode,
        uiScale    = data.current.uiScale,
        vsync      = data.current.vsync,
        frameLimit = data.current.frameLimit,
        msaa       = data.current.msaa,
        brightness = data.current.brightness,
    }
end

-----------------------------------------------------------
-- Load from engine
-----------------------------------------------------------

function data.reload()
    local w, h, wm, uiScale, vs, frameLimit, msaa = engine.getVideoConfig()
    data.current.width      = w
    data.current.height     = h
    data.current.windowMode = wm
    data.current.uiScale    = uiScale
    data.current.vsync      = vs
    data.current.frameLimit = frameLimit or 60
    data.current.msaa       = msaa or 0
end

-----------------------------------------------------------
-- Apply pending → current, push to engine
-- Returns { scaleChanged = bool, resolutionChanged = bool }
-----------------------------------------------------------

function data.apply(widgetValues)
    local result = { scaleChanged = false, resolutionChanged = false }

    -- Resolution
    if data.pending.width ~= data.current.width
        or data.pending.height ~= data.current.height then
        data.current.width  = data.pending.width
        data.current.height = data.pending.height
        result.resolutionChanged = true
        engine.logInfo("Resolution applied: "
            .. data.current.width .. "x" .. data.current.height)
    end

    -- Window Mode
    if data.pending.windowMode ~= data.current.windowMode then
        data.current.windowMode = data.pending.windowMode
        engine.setWindowMode(data.current.windowMode)
        engine.logInfo("Window mode applied: " .. data.current.windowMode)
    end

    -- VSync
    if data.pending.vsync ~= data.current.vsync then
        data.current.vsync = data.pending.vsync
        -- TODO: call engine.setVSync(data.current.vsync)
        engine.logInfo("VSync applied: " .. tostring(data.current.vsync))
    end

    -- MSAA
    if data.pending.msaa ~= data.current.msaa then
        data.current.msaa = data.pending.msaa
        -- TODO: call engine.setMSAA(data.current.msaa)
        engine.logInfo("MSAA applied: " .. tostring(data.current.msaa))
    end

    -- Brightness
    if data.pending.brightness ~= data.current.brightness then
        data.current.brightness = data.pending.brightness
        -- TODO: call engine.setBrightness(data.current.brightness)
        engine.logInfo("Brightness applied: " .. tostring(data.current.brightness))
    end

    -- UI Scale (read from widget)
    if widgetValues.uiScale then
        local newScale = widgetValues.uiScale
        if newScale >= data.uiScaleMin and newScale <= data.uiScaleMax then
            if data.current.uiScale ~= newScale then
                result.scaleChanged = true
                data.current.uiScale  = newScale
                data.pending.uiScale  = newScale
                engine.setUIScale(newScale)
                engine.logInfo("UI scale applied: " .. tostring(newScale))
            end
        else
            engine.logWarn("UI scale out of range: " .. tostring(newScale))
        end
    end

    -- Frame Limit (read from widget)
    if widgetValues.frameLimit then
        local fl = widgetValues.frameLimit
        if fl >= data.frameLimitMin and fl <= data.frameLimitMax then
            data.current.frameLimit  = math.floor(fl)
            data.pending.frameLimit  = data.current.frameLimit
            engine.setFrameLimit(data.current.frameLimit)
            engine.logInfo("Frame limit applied: " .. tostring(data.current.frameLimit))
        else
            engine.logWarn("Frame limit out of range: " .. tostring(fl))
        end
    end

    if result.resolutionChanged then
        engine.setResolution(data.current.width, data.current.height)
    end

    return result
end

-----------------------------------------------------------
-- Save (apply then persist)
-----------------------------------------------------------

function data.save(widgetValues)
    engine.logInfo("Saving settings...")
    local result = data.apply(widgetValues)
    engine.saveVideoConfig()
    engine.logInfo("Settings saved.")
    return result
end

-----------------------------------------------------------
-- Revert to last saved config
-----------------------------------------------------------

function data.revert()
    engine.logInfo("Reverting settings to saved config...")

    local w, h, wm, uiScale, vs, frameLimit, msaa = engine.getVideoConfig()

    if data.current.windowMode ~= wm then
        engine.setWindowMode(wm)
    end
    if data.current.uiScale ~= uiScale then engine.setUIScale(uiScale) end
    if data.current.frameLimit ~= frameLimit then engine.setFrameLimit(frameLimit) end
    if data.current.width ~= w or data.current.height ~= h then
        engine.setResolution(w, h)
    end

    data.current.width      = w
    data.current.height     = h
    data.current.windowMode = wm
    data.current.uiScale    = uiScale
    data.current.vsync      = vs
    data.current.frameLimit = frameLimit
    data.current.msaa       = msaa or 0
end

-----------------------------------------------------------
-- TextBox submit validation (called from graphics_tab)
-----------------------------------------------------------

function data.validateTextBoxSubmit(name, value)
    if name == "uiscale_input" then
        local n = tonumber(value)
        if not n then return nil, data.current.uiScale end
        n = math.max(data.uiScaleMin, math.min(data.uiScaleMax, n))
        data.pending.uiScale = n
        return n, nil
    elseif name == "framelimit_input" then
        local n = tonumber(value)
        if not n then return nil, data.current.frameLimit end
        n = math.max(data.frameLimitMin,
            math.min(data.frameLimitMax, math.floor(n)))
        data.pending.frameLimit = n
        return n, nil
    elseif name == "brightness_input" then
        local n = tonumber(value)
        if not n then return nil, data.current.brightness end
        n = math.max(data.brightnessMin,
            math.min(data.brightnessMax, math.floor(n)))
        data.pending.brightness = n
        return n, nil
    end
    return nil, nil
end

return data
