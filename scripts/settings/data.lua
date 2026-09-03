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
data.tooltipDwellMin = 0
data.tooltipDwellMax = 1000
data.tooltipHintDelayMin = 0
data.tooltipHintDelayMax = 1000
-- #913 autosave. These MIRROR Engine.Save.Config's validated ranges;
-- the engine clamps again on write, so a mismatch here can only ever
-- make the UI stricter than the file format, never looser.
data.saveIntervalMin = 1
data.saveIntervalMax = 60
data.saveDepthMin    = 1
data.saveDepthMax    = 10

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
-- The persisted baseline for Back (#2194)
--
-- An independent value snapshot of every video field the Settings
-- screen manages. Back restores from THIS, never from
-- engine.getVideoConfig().
--
-- Why it has to exist: Apply pushes each pending value straight into
-- the live rvVideoConfigRef through the per-field engine setters, and
-- engine.getVideoConfig reads that same ref. Only engine.saveVideoConfig
-- writes the ref out to config/video.local.yaml, and no registered verb
-- reads that file back. So after an Apply WITHOUT Save the live and the
-- persisted states are indistinguishable through the getter: the eight
-- fields Back used to read back from it "reverted" to their applied
-- values and stayed live until the process exited. Brightness and the
-- two tooltip delays were already exempt because they were snapshotted
-- in Lua; this table generalizes that snapshot to all eleven fields.
--
-- Refreshed at exactly the three points that establish a new persisted
-- baseline, and nowhere else:
--   * data.reload()       -- Settings state established from the
--                            boot/default-loaded config
--   * data.loadDefaults() -- factory-default state established
--   * data.save()         -- after persistence returns
-- Apply, live preview and Revert must NOT refresh it: Apply without
-- Save is precisely the state Back has to undo.
-----------------------------------------------------------

local function copyVideo(src)
    return {
        width              = src.width,
        height             = src.height,
        windowMode         = src.windowMode,
        uiScale            = src.uiScale,
        vsync              = src.vsync,
        frameLimit         = src.frameLimit,
        msaa               = src.msaa,
        brightness         = src.brightness,
        pixelSnap          = src.pixelSnap,
        textureFilter      = src.textureFilter,
        tooltipDwellMs     = src.tooltipDwellMs,
        tooltipHintDelayMs = src.tooltipHintDelayMs,
    }
end

data.savedVideo = copyVideo(data.current)

-- Make data.current the new persisted baseline. Callers are the three
-- refresh points documented above.
function data.captureSavedVideo()
    data.savedVideo = copyVideo(data.current)
end

-----------------------------------------------------------
-- Autosave state (#913)
--
-- Deliberately a SEPARATE current/pending pair from the video settings
-- above: the two families have different engine-side homes (video lives
-- in videoConfigRef and is live-previewed; autosave lives only in
-- config/save.local.yaml plus the Lua scheduler's cached copy), and
-- folding them into one table would make every video Apply rewrite the
-- save config from whatever stale values happened to be in `pending`.
-----------------------------------------------------------

data.currentSave = {
    enabled         = false,
    intervalMinutes = 10,
    rotationDepth   = 3,
}

data.pendingSave = {
    enabled         = false,
    intervalMinutes = 10,
    rotationDepth   = 3,
}

local function copySaveConfig(src)
    return {
        enabled         = src.enabled and true or false,
        intervalMinutes = src.intervalMinutes,
        rotationDepth   = src.rotationDepth,
    }
end

-- The scheduler is the one live consumer. Resolved lazily so this module
-- keeps working in a headless/UI-test context where autosave.lua was
-- never loaded.
local function notifyScheduler(cfg)
    local autosave = package.loaded["scripts.autosave"]
    if autosave and autosave.onConfigChanged then
        autosave.onConfigChanged(copySaveConfig(cfg))
    end
end

function data.resetPendingSave()
    data.pendingSave = copySaveConfig(data.currentSave)
end

-- Read the EFFECTIVE config (tracked template overlaid with the local
-- overrides) back from the engine.
function data.reloadSave()
    local cfg = engine.getSaveConfig()
    if type(cfg) == "table" then
        data.currentSave = {
            enabled         = cfg.enabled and true or false,
            intervalMinutes = cfg.intervalMinutes or 10,
            rotationDepth   = cfg.rotationDepth or 3,
        }
    end
    data.resetPendingSave()
end

local function clampInt(n, lo, hi)
    return math.max(lo, math.min(hi, math.floor(n)))
end

-- Commit whatever the General tab's widgets currently hold into
-- data.currentSave and tell the scheduler. Returns true when a value
-- actually moved, so callers can avoid pointless persistence.
function data.applySave(widgetValues)
    widgetValues = widgetValues or {}
    local before = copySaveConfig(data.currentSave)

    if widgetValues.autosaveEnabled ~= nil then
        data.pendingSave.enabled = widgetValues.autosaveEnabled and true or false
    end
    if type(widgetValues.autosaveIntervalMinutes) == "number" then
        data.pendingSave.intervalMinutes = clampInt(
            widgetValues.autosaveIntervalMinutes,
            data.saveIntervalMin, data.saveIntervalMax)
    end
    if type(widgetValues.autosaveRotationDepth) == "number" then
        data.pendingSave.rotationDepth = clampInt(
            widgetValues.autosaveRotationDepth,
            data.saveDepthMin, data.saveDepthMax)
    end

    data.currentSave = copySaveConfig(data.pendingSave)
    local changed = before.enabled ~= data.currentSave.enabled
        or before.intervalMinutes ~= data.currentSave.intervalMinutes
        or before.rotationDepth ~= data.currentSave.rotationDepth
    -- Always notify, even when nothing moved: onConfigChanged only
    -- restarts the interval on a real enable/interval transition, so an
    -- Apply with unchanged values is inert by design (the issue's
    -- "applying unrelated settings does not restart the interval").
    notifyScheduler(data.currentSave)
    return changed
end

-- Persist to config/save.local.yaml. Save = apply, then write.
function data.saveSaveConfig()
    local ok = engine.setSaveConfig({
        enabled         = data.currentSave.enabled,
        intervalMinutes = data.currentSave.intervalMinutes,
        rotationDepth   = data.currentSave.rotationDepth,
    })
    if not ok then
        engine.logWarn("Could not persist autosave settings")
    end
    return ok
end

-- Back: discard unapplied AND unsaved autosave edits by re-reading what
-- is actually on disk, then push that to the scheduler.
function data.revertSave()
    data.reloadSave()
    notifyScheduler(data.currentSave)
end

-- Defaults: the tracked template ALONE, never folded with the player's
-- own local overrides.
function data.loadDefaultSaveConfig()
    local cfg = engine.getDefaultSaveConfig()
    if type(cfg) == "table" then
        data.currentSave = {
            enabled         = cfg.enabled and true or false,
            intervalMinutes = cfg.intervalMinutes or 10,
            rotationDepth   = cfg.rotationDepth or 3,
        }
    end
    data.resetPendingSave()
    notifyScheduler(data.currentSave)
end

-- Clamp a General-tab textbox submission into its validated range.
-- Returns the clamped number, or nil when the field is not one of ours
-- (or the text is not a number at all, in which case the caller leaves
-- the box alone).
function data.validateSaveTextBoxSubmit(name, value)
    local n = tonumber(value)
    if not n then return nil end
    if name == "autosave_interval_input" then
        local clamped = clampInt(n, data.saveIntervalMin, data.saveIntervalMax)
        data.pendingSave.intervalMinutes = clamped
        return clamped
    elseif name == "autosave_depth_input" then
        local clamped = clampInt(n, data.saveDepthMin, data.saveDepthMax)
        data.pendingSave.rotationDepth = clamped
        return clamped
    end
    return nil
end

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
    -- The product must stay inside the engine's UI-scale domain (#2198):
    -- engine.setUIScale REJECTS an out-of-domain value and leaves the
    -- previous scale in place, so a default above uiScaleMax / 2.5 would
    -- otherwise silently apply nothing on a 4K display.
    uiScale = math.max(data.uiScaleMin, math.min(data.uiScaleMax, uiScale))


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

    -- #2194: factory-default state is a new persisted baseline, so a
    -- Back taken after Defaults returns to the defaults rather than to
    -- whatever was on disk before them. This is the refresh point the
    -- three per-field snapshots used to take here.
    data.captureSavedVideo()

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

    -- #913: the General tab's autosave settings reset to the tracked
    -- config/save_default.yaml alone (never folded with the player's own
    -- local overrides) and reach the live scheduler immediately, the
    -- same way every video setting above is pushed to the engine here.
    data.loadDefaultSaveConfig()

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
    data.resetPendingSave()
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
    -- #2194: opening Settings establishes the baseline Back returns to.
    data.captureSavedVideo()
    -- #913: the autosave family has its own engine accessor (it lives in
    -- config/save.local.yaml, not videoConfigRef), so it reloads
    -- alongside rather than through getVideoConfig above.
    data.reloadSave()
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

    -- #913: autosave settings commit here too, so Apply gives them the
    -- same live-but-unpersisted behaviour every other setting has. The
    -- scheduler is notified inside; nothing is written to disk until
    -- data.save below.
    result.autosaveChanged = data.applySave(widgetValues)

    return result
end

-----------------------------------------------------------
-- Save (apply then persist)
-----------------------------------------------------------

function data.save(widgetValues)
    engine.logInfo("Saving settings...")
    local result = data.apply(widgetValues)
    -- #2202: engine.saveVideoConfig() returns false (never raises) when
    -- config/video.local.yaml could not be written, so name the family
    -- the way saveSaveConfig already names autosave. Without this the
    -- only trace of a lost video save was "Saving settings..." followed
    -- by "Settings saved.".
    local videoSaved = engine.saveVideoConfig()
    if not videoSaved then
        engine.logWarn("Could not persist video settings")
    end
    -- #913: persist the just-applied autosave settings to
    -- config/save.local.yaml.
    data.saveSaveConfig()
    -- Refresh the baseline so a later revert restores these saved values,
    -- not the pre-save ones. #2194: this covers all eleven fields, and it
    -- runs AFTER persistence so the snapshot is of what actually reached
    -- disk. #2202: which is exactly why a FAILED write must not advance
    -- it — the values only ever reached the live ref, so adopting them as
    -- the baseline would leave Back with no way back to the configuration
    -- that is genuinely saved on disk.
    if videoSaved then
        data.captureSavedVideo()
    end
    engine.logInfo("Settings saved.")
    return result
end

-----------------------------------------------------------
-- Revert to last saved config
-----------------------------------------------------------

function data.revert()
    engine.logInfo("Reverting settings to saved config...")

    -- #2194: the baseline is data.savedVideo, NEVER engine.getVideoConfig().
    -- That getter reads the live rvVideoConfigRef, which Apply has already
    -- overwritten, so reverting to it reverts nothing.
    local saved = data.savedVideo

    -- Fields below reach the engine ONLY through data.apply, which moves
    -- data.current in the same step -- so data.current is an accurate
    -- record of what the engine holds for them and these guards are
    -- sound. They are guards rather than unconditional calls because
    -- several of these setters post to luaToEngineQueue (window
    -- recreation, swapchain rebuild, texture-filter re-bind): a Back
    -- that changed nothing must not trigger any of that.
    if data.current.windowMode ~= saved.windowMode then
        engine.setWindowMode(saved.windowMode)
    end
    if data.current.uiScale ~= saved.uiScale then
        engine.setUIScale(saved.uiScale)
    end
    if data.current.frameLimit ~= saved.frameLimit then
        engine.setFrameLimit(saved.frameLimit)
    end
    if data.current.width ~= saved.width
        or data.current.height ~= saved.height then
        engine.setResolution(saved.width, saved.height)
    end
    if data.current.vsync ~= saved.vsync then
        engine.setVSync(saved.vsync)
    end
    if data.current.msaa ~= saved.msaa then
        engine.setMSAA(saved.msaa)
    end
    if data.current.pixelSnap ~= saved.pixelSnap then
        engine.setPixelSnap(saved.pixelSnap)
    end
    if data.current.textureFilter ~= saved.textureFilter then
        engine.setTextureFilter(saved.textureFilter)
    end

    -- Brightness and the two tooltip delays are LIVE-PREVIEWED: the
    -- graphics-tab sliders call the engine setter directly and write
    -- only data.pending (graphics_tab.lua), so for these three
    -- data.current is NOT a record of what the engine holds and the
    -- guard above would leave a previewed value live. Call them
    -- unconditionally instead -- as brightness already did.
    engine.setBrightness(saved.brightness)
    engine.setTooltipDwellMs(saved.tooltipDwellMs)
    engine.setTooltipHintDelayMs(saved.tooltipHintDelayMs)

    data.current.width              = saved.width
    data.current.height             = saved.height
    data.current.windowMode         = saved.windowMode
    data.current.uiScale            = saved.uiScale
    data.current.vsync              = saved.vsync
    data.current.frameLimit         = saved.frameLimit
    data.current.msaa               = saved.msaa
    data.current.brightness         = saved.brightness
    data.current.pixelSnap          = saved.pixelSnap
    data.current.textureFilter      = saved.textureFilter
    data.current.tooltipDwellMs     = saved.tooltipDwellMs
    data.current.tooltipHintDelayMs = saved.tooltipHintDelayMs

    -- #913: Back must abandon unsaved autosave edits too. That family
    -- has its own on-disk home (config/save.local.yaml) and its own
    -- engine accessor, so it genuinely CAN re-read the last saved state
    -- from disk -- which is why it keeps its own revert path rather
    -- than joining the snapshot above.
    data.revertSave()
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
