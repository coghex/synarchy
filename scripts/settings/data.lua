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
data.savedBrightness = nil
data.tooltipDwellMin = 0
data.tooltipDwellMax = 1000
data.tooltipHintDelayMin = 0
data.tooltipHintDelayMax = 1000
-- Snapshots of the last saved tooltip values, used by revert().
-- These (like savedBrightness) capture the saved state because dwell/hint
-- are live-previewed to the engine, so getTooltipDwellMs()/getTooltipHintDelayMs()
-- return the just-edited values, not what we should revert to.
data.savedTooltipDwellMs = nil
data.savedTooltipHintDelayMs = nil

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
    { text = "Off", value = "1" },
    { text = "2x",  value = "2" },
    { text = "4x",  value = "4" },
    { text = "8x",  value = "8" },
}

-- Texture filter options
data.textureFilterOptions = {
    { text = "Nearest (Pixel)", value = "nearest" },
    { text = "Linear (Smooth)", value = "linear" },
}

-----------------------------------------------------------
-- State
-----------------------------------------------------------

data.current = {
    width         = 800,
    height        = 600,
    windowMode    = "fullscreen",  -- "fullscreen", "borderless", "windowed"
    uiScale       = 1.0,
    vsync         = true,
    frameLimit    = 60,
    msaa          = 1,
    brightness    = 100,
    pixelSnap     = false,
    textureFilter = "nearest",     -- "nearest" or "linear"
    tooltipDwellMs = 400,
    tooltipHintDelayMs = 400,
}

data.pending = {}

-----------------------------------------------------------
-- Load factory defaults from video_default.yaml
-- and apply to engine
-----------------------------------------------------------

-----------------------------------------------------------
-- Load factory defaults from video_default.yaml
-- and apply to engine
-----------------------------------------------------------

-----------------------------------------------------------
-- Load factory defaults from video_default.yaml
-- and apply to engine
-----------------------------------------------------------

function data.loadDefaults()
    engine.logInfo("Loading factory default settings...")

    local w, h, wm, uiScale, vs, frameLimit, msaa, brightness,
          pixelSnap, textureFilter = engine.loadDefaultConfig()

    engine.logDebug("loadDefaultConfig() returned: " .. w .. "x" .. h)

    -- Auto-adjust UI scale for large/HiDPI displays
    -- If the resolution is very high, scale up the UI so it's not tiny
    local screenArea = w * h
    local is1080p = screenArea >= (1920 * 1080 * 0.9)
    local is1440p = screenArea >= (2560 * 1440 * 0.9)
    local is4K = screenArea >= (3840 * 2160 * 0.9)
    
    if is4K then
        uiScale = uiScale * 2.5
        engine.logInfo("Detected 4K+ display, scaling UI to: " .. tostring(uiScale))
    elseif is1440p then
        uiScale = uiScale * 2.0
        engine.logInfo("Detected 1440p+ display, scaling UI to: " .. tostring(uiScale))
    elseif is1080p then
        uiScale = uiScale * 1.5
        engine.logInfo("Detected 1080p+ display, scaling UI to: " .. tostring(uiScale))
    end


    -- Update current state
    data.current.width         = w
    data.current.height        = h
    data.current.windowMode    = wm
    data.current.uiScale       = uiScale
    data.current.vsync         = vs
    data.current.frameLimit    = frameLimit or 60
    data.current.msaa          = msaa or 1
    data.current.brightness    = brightness or 100
    data.current.pixelSnap     = pixelSnap or false
    data.current.textureFilter = textureFilter or "nearest"
    data.current.tooltipDwellMs = engine.getTooltipDwellMs() or 400
    data.current.tooltipHintDelayMs = engine.getTooltipHintDelayMs() or 400

    -- Snapshot brightness/tooltip values for revert
    data.savedBrightness = data.current.brightness
    data.savedTooltipDwellMs = data.current.tooltipDwellMs
    data.savedTooltipHintDelayMs = data.current.tooltipHintDelayMs

    -- Push all values to engine via individual setters
    engine.setResolution(data.current.width, data.current.height)
    engine.setWindowMode(data.current.windowMode)
    engine.setUIScale(data.current.uiScale)
    engine.setVSync(data.current.vsync)
    engine.setFrameLimit(data.current.frameLimit)
    engine.setMSAA(data.current.msaa)
    engine.setBrightness(data.current.brightness)
    engine.setPixelSnap(data.current.pixelSnap)
    engine.setTextureFilter(data.current.textureFilter)
    engine.setTooltipDwellMs(data.current.tooltipDwellMs)
    engine.setTooltipHintDelayMs(data.current.tooltipHintDelayMs)

    -- Reset pending to match current
    data.resetPending()

    engine.logInfo("Default settings loaded and applied.")
end

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
    return tostring(msaa or 1)
end

function data.msaaFromString(str)
    local n = tonumber(str)
    if n == 2 or n == 4 or n == 8 then return n end
    return 1
end

function data.findTextureFilterIndex(filter)
    for i, opt in ipairs(data.textureFilterOptions) do
        if opt.value == filter then
            return i
        end
    end
    return 1  -- default to nearest
end

function data.resetPending()
    data.pending = {
        width         = data.current.width,
        height        = data.current.height,
        windowMode    = data.current.windowMode,
        uiScale       = data.current.uiScale,
        vsync         = data.current.vsync,
        frameLimit    = data.current.frameLimit,
        msaa          = data.current.msaa,
        brightness    = data.current.brightness,
        pixelSnap     = data.current.pixelSnap,
        textureFilter = data.current.textureFilter,
        tooltipDwellMs = data.current.tooltipDwellMs,
        tooltipHintDelayMs = data.current.tooltipHintDelayMs,
    }
end

-----------------------------------------------------------
-- Load from engine
-- getVideoConfig returns 10 values:
--   w, h, wm, uiScale, vs, frameLimit, msaa, brightness,
--   pixelSnap, textureFilter
-----------------------------------------------------------

function data.reload()
    local w, h, wm, uiScale, vs, frameLimit, msaa, brightness,
          pixelSnap, textureFilter = engine.getVideoConfig()
    engine.logDebug("reload() got resolution: " .. w .. "x" .. h
                  .. ", uiScale: " .. tostring(uiScale))
    data.current.width         = w
    data.current.height        = h
    data.current.windowMode    = wm
    data.current.uiScale       = uiScale
    data.current.vsync         = vs
    data.current.frameLimit    = frameLimit or 60
    data.current.msaa          = msaa or 1
    data.current.brightness    = brightness or 100
    data.current.pixelSnap     = pixelSnap or false
    data.current.textureFilter = textureFilter or "nearest"
    data.current.tooltipDwellMs = engine.getTooltipDwellMs() or 400
    data.current.tooltipHintDelayMs = engine.getTooltipHintDelayMs() or 400
    data.savedBrightness = data.current.brightness
    data.savedTooltipDwellMs = data.current.tooltipDwellMs
    data.savedTooltipHintDelayMs = data.current.tooltipHintDelayMs
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
        engine.logDebug("Resolution applied: "
            .. data.current.width .. "x" .. data.current.height)
    end

    -- Window Mode
    if data.pending.windowMode ~= data.current.windowMode then
        data.current.windowMode = data.pending.windowMode
        engine.setWindowMode(data.current.windowMode)
        engine.logDebug("Window mode applied: " .. data.current.windowMode)
    end

    -- VSync
    if data.pending.vsync ~= data.current.vsync then
        data.current.vsync = data.pending.vsync
        engine.setVSync(data.current.vsync)
        engine.logDebug("VSync applied: " .. tostring(data.current.vsync))
    end

    -- MSAA
    if data.pending.msaa ~= data.current.msaa then
        data.current.msaa = data.pending.msaa
        engine.setMSAA(data.current.msaa)
        engine.logDebug("MSAA applied: " .. tostring(data.current.msaa))
    end

    -- Brightness (from pending, set by dropdown onChange callbacks)
    if data.pending.brightness ~= data.current.brightness then
        data.current.brightness = data.pending.brightness
        engine.setBrightness(data.current.brightness)
        engine.logDebug("Brightness applied: " .. tostring(data.current.brightness))
    end

    -- Pixel Snap
    if data.pending.pixelSnap ~= data.current.pixelSnap then
        data.current.pixelSnap = data.pending.pixelSnap
        engine.setPixelSnap(data.current.pixelSnap)
        engine.logDebug("Pixel Snap applied: " .. tostring(data.current.pixelSnap))
    end

    -- Texture Filter
    if data.pending.textureFilter ~= data.current.textureFilter then
        data.current.textureFilter = data.pending.textureFilter
        engine.setTextureFilter(data.current.textureFilter)
        engine.logDebug("Texture filter applied: " .. data.current.textureFilter)
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
                engine.logDebug("UI scale applied: " .. tostring(newScale))
            end
        else
            engine.logWarn("UI scale out of range: " .. tostring(newScale))
        end
    end

    -- Brightness (live-previewed, just commit to current)
    if widgetValues.brightness then
        local br = math.floor(widgetValues.brightness)
        br = math.max(data.brightnessMin, math.min(data.brightnessMax, br))
        if data.current.brightness ~= br then
            data.current.brightness = br
            data.pending.brightness = br
            -- Engine already has this value from live preview,
            -- but ensure it's set in case apply is called without slider interaction
            engine.setBrightness(br)
            engine.logDebug("Brightness applied: " .. tostring(br))
        end
    end

    -- Frame Limit (read from widget)
    if widgetValues.frameLimit then
        local fl = widgetValues.frameLimit
        if fl >= data.frameLimitMin and fl <= data.frameLimitMax then
            data.current.frameLimit  = math.floor(fl)
            data.pending.frameLimit  = data.current.frameLimit
            engine.setFrameLimit(data.current.frameLimit)
            engine.logDebug("Frame limit applied: " .. tostring(data.current.frameLimit))
        else
            engine.logWarn("Frame limit out of range: " .. tostring(fl))
        end
    end

    -- Tooltip dwell delay (live-previewed; commit to current + persist)
    if widgetValues.tooltipDwellMs then
        local dw = math.floor(widgetValues.tooltipDwellMs)
        dw = math.max(data.tooltipDwellMin,
            math.min(data.tooltipDwellMax, dw))
        if data.current.tooltipDwellMs ~= dw then
            data.current.tooltipDwellMs = dw
            data.pending.tooltipDwellMs = dw
            engine.setTooltipDwellMs(dw)
            engine.logDebug("Tooltip dwell applied: " .. tostring(dw) .. "ms")
        end
    end

    -- Tooltip hint delay (live-previewed; commit to current + persist)
    if widgetValues.tooltipHintDelayMs then
        local hd = math.floor(widgetValues.tooltipHintDelayMs)
        hd = math.max(data.tooltipHintDelayMin,
            math.min(data.tooltipHintDelayMax, hd))
        if data.current.tooltipHintDelayMs ~= hd then
            data.current.tooltipHintDelayMs = hd
            data.pending.tooltipHintDelayMs = hd
            engine.setTooltipHintDelayMs(hd)
            engine.logDebug("Tooltip hint delay applied: "
                .. tostring(hd) .. "ms")
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
    -- Refresh revert snapshots so a later revert restores these saved values,
    -- not the pre-save ones.
    data.savedBrightness = data.current.brightness
    data.savedTooltipDwellMs = data.current.tooltipDwellMs
    data.savedTooltipHintDelayMs = data.current.tooltipHintDelayMs
    engine.logInfo("Settings saved.")
    return result
end

-----------------------------------------------------------
-- Revert to last saved config
-----------------------------------------------------------

function data.revert()
    engine.logInfo("Reverting settings to saved config...")

    local w, h, wm, uiScale, vs, frameLimit, msaa, brightness,
          pixelSnap, textureFilter = engine.getVideoConfig()

    if data.current.windowMode ~= wm then
        engine.setWindowMode(wm)
    end
    if data.current.uiScale ~= uiScale then engine.setUIScale(uiScale) end
    if data.current.frameLimit ~= frameLimit then engine.setFrameLimit(frameLimit) end
    if data.current.width ~= w or data.current.height ~= h then
        engine.setResolution(w, h)
    end

    if data.current.vsync ~= vs then engine.setVSync(vs) end
    if data.current.msaa ~= (msaa or 1) then engine.setMSAA(msaa or 1) end

    local revertBrightness = data.savedBrightness or 100
    engine.setBrightness(revertBrightness)

    local savedPixelSnap = pixelSnap or false
    if data.current.pixelSnap ~= savedPixelSnap then
        engine.setPixelSnap(savedPixelSnap)
    end
    local savedTextureFilter = textureFilter or "nearest"
    if data.current.textureFilter ~= savedTextureFilter then
        engine.setTextureFilter(savedTextureFilter)
    end

    local savedDwell = data.savedTooltipDwellMs or 400
    if data.current.tooltipDwellMs ~= savedDwell then
        engine.setTooltipDwellMs(savedDwell)
    end

    local savedHintDelay = data.savedTooltipHintDelayMs or 400
    if data.current.tooltipHintDelayMs ~= savedHintDelay then
        engine.setTooltipHintDelayMs(savedHintDelay)
    end

    data.current.width         = w
    data.current.height        = h
    data.current.windowMode    = wm
    data.current.uiScale       = uiScale
    data.current.vsync         = vs
    data.current.frameLimit    = frameLimit
    data.current.msaa          = msaa or 1
    data.current.brightness    = revertBrightness
    data.current.pixelSnap     = savedPixelSnap
    data.current.textureFilter = savedTextureFilter
    data.current.tooltipDwellMs = savedDwell
    data.current.tooltipHintDelayMs = savedHintDelay
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
