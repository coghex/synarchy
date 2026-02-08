-- Graphics Tab
-- Creates all widget rows for the Graphics settings tab.
-- Returns rowHandles[] for the scroll system.
--
-- Row order:
--   1. Resolution      (dropdown)
--   2. Window Mode     (dropdown)
--   3. VSync           (checkbox)
--   4. Frame Limit     (textbox)
--   5. MSAA            (dropdown)
--   6. Brightness      (textbox — TODO: replace with slider)
--   7. UI Scaling      (textbox)
local label    = require("scripts.ui.label")
local textbox  = require("scripts.ui.textbox")
local checkbox = require("scripts.ui.checkbox")
local dropdown = require("scripts.ui.dropdown")
local slider   = require("scripts.ui.slider")
local data     = require("scripts.settings.data")

local graphicsTab = {}

-- Widget IDs stored here so settings_menu can read values at apply time
graphicsTab.resolutionDropdownId  = nil
graphicsTab.windowModeDropdownId  = nil
graphicsTab.vsyncCheckboxId       = nil
graphicsTab.frameLimitTextBoxId   = nil
graphicsTab.msaaDropdownId        = nil
graphicsTab.brightnessTextBoxId   = nil
graphicsTab.uiScaleTextBoxId      = nil

-----------------------------------------------------------
-- MSAA dropdown validators
-- Only allow digits that could form "0", "2", "4", "8"
-----------------------------------------------------------

function graphicsTab.msaaValidator(char)
    return char == "1" or char == "2" or char == "4" or char == "8"
end

function graphicsTab.msaaMatcher(inputText, options)
    if not inputText or inputText == "" then return nil end
    -- Match against the value field (e.g. "0", "2", "4", "8")
    for i, opt in ipairs(options) do
        if opt.value == inputText then
            return i
        end
    end
    -- Also match against display text (e.g. "Off", "2x", "4x", "8x")
    local lower = inputText:lower()
    for i, opt in ipairs(options) do
        if opt.text:lower():sub(1, #lower) == lower then
            return i
        end
    end
    return nil
end

-----------------------------------------------------------
-- Window mode dropdown validators
-- Allow letters that could start "fullscreen", "borderless", "windowed"
-----------------------------------------------------------

function graphicsTab.windowModeValidator(char)
    local c = char:lower()
    return c:match("[a-z]") ~= nil
end

function graphicsTab.windowModeMatcher(inputText, options)
    if not inputText or inputText == "" then return nil end
    local lower = inputText:lower()
    for i, opt in ipairs(options) do
        if opt.text:lower():sub(1, #lower) == lower then
            return i
        end
    end
    return nil
end

-----------------------------------------------------------
-- Create all rows
-----------------------------------------------------------

-- params = {
--   page, font, baseSizes, uiscale, s (scaled sizes),
--   contentX, contentY, contentW,
--   zContent, zWidgets,
--   currentSettings, pendingSettings,
-- }
-- Returns: rowHandles[]
function graphicsTab.create(params)
    local page     = params.page
    local font     = params.font
    local base     = params.baseSizes
    local uiscale  = params.uiscale
    local s        = params.s
    local cx       = params.contentX
    local cy       = params.contentY
    local cw       = params.contentW
    local zContent = params.zContent
    local zWidgets = params.zWidgets
    local pending  = params.pendingSettings

    local rows = {}
    local rowIndex = 0

    -- Helper: compute Y position for row N (0-based)
    local function rowY(n)
        return cy + s.rowSpacing * n
    end

    ---------------------------------------------------------
    -- Row 1: Resolution (dropdown)
    ---------------------------------------------------------
    local resLabelId = label.new({
        name     = "resolution_label",
        text     = "Resolution",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = uiscale,
    })
    local resLabelHandle = label.getElementHandle(resLabelId)
    UI.addToPage(page, resLabelHandle, cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(resLabelHandle, zContent)

    local currentRes = data.resolutionString(
        data.current.width, data.current.height)

    graphicsTab.resolutionDropdownId = dropdown.new({
        name              = "resolution",
        options           = data.resolutions,
        default           = currentRes,
        font              = font,
        fontSize          = 24,
        height            = base.dropdownHeight,
        page              = page,
        x = 0, y = 0,
        uiscale           = uiscale,
        zIndex            = zWidgets,
        validateChar      = dropdown.resolutionValidator,
        matchFn           = dropdown.resolutionMatcher,
        maxVisibleOptions = 8,
        onChange = function(value, text, id, name)
            local w, h = value:match("^(%d+)x(%d+)$")
            if w and h then
                pending.width  = tonumber(w)
                pending.height = tonumber(h)
                engine.logInfo("Resolution pending: " .. text)
            end
        end,
    })

    local ddResId = graphicsTab.resolutionDropdownId
    local ddResW, _ = dropdown.getSize(ddResId)
    dropdown.setPosition(ddResId, cx + cw - ddResW, rowY(rowIndex))

    table.insert(rows, {
        labelHandle = resLabelHandle,
        widgetHandles = {
            dropdown.getElementHandle(ddResId),
            dropdown.getArrowHandle(ddResId),
        },
        widgetSetPosition = function(ry)
            dropdown.setPosition(ddResId, cx + cw - ddResW, ry)
        end,
        widgetSetVisible = function(vis)
            dropdown.setVisible(ddResId, vis)
        end,
    })
    rowIndex = rowIndex + 1

    ---------------------------------------------------------
    -- Row 2: Window Mode (dropdown)
    ---------------------------------------------------------
    local wmLabelId = label.new({
        name     = "windowmode_label",
        text     = "Window Mode",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = uiscale,
    })
    local wmLabelHandle = label.getElementHandle(wmLabelId)
    UI.addToPage(page, wmLabelHandle, cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(wmLabelHandle, zContent)

    graphicsTab.windowModeDropdownId = dropdown.new({
        name              = "window_mode",
        options           = data.windowModes,
        default           = data.current.windowMode,
        font              = font,
        fontSize          = 24,
        height            = base.dropdownHeight,
        page              = page,
        x = 0, y = 0,
        uiscale           = uiscale,
        zIndex            = zWidgets,
        validateChar      = graphicsTab.windowModeValidator,
        matchFn           = graphicsTab.windowModeMatcher,
        maxVisibleOptions = 3,
        onChange = function(value, text, id, name)
            pending.windowMode = value
            engine.logInfo("Window mode pending: " .. text)
        end,
    })

    local ddWmId = graphicsTab.windowModeDropdownId
    local ddWmW, _ = dropdown.getSize(ddWmId)
    dropdown.setPosition(ddWmId, cx + cw - ddWmW, rowY(rowIndex))

    table.insert(rows, {
        labelHandle = wmLabelHandle,
        widgetHandles = {
            dropdown.getElementHandle(ddWmId),
            dropdown.getArrowHandle(ddWmId),
        },
        widgetSetPosition = function(ry)
            dropdown.setPosition(ddWmId, cx + cw - ddWmW, ry)
        end,
        widgetSetVisible = function(vis)
            dropdown.setVisible(ddWmId, vis)
        end,
    })
    rowIndex = rowIndex + 1

    ---------------------------------------------------------
    -- Row 3: VSync (checkbox)
    ---------------------------------------------------------
    local vsLabelId = label.new({
        name     = "vsync_label",
        text     = "VSync",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = uiscale,
    })
    local vsLabelHandle = label.getElementHandle(vsLabelId)
    UI.addToPage(page, vsLabelHandle, cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(vsLabelHandle, zContent)

    local cbSize = math.floor(base.checkboxSize * uiscale)
    graphicsTab.vsyncCheckboxId = checkbox.new({
        name    = "vsync",
        size    = base.checkboxSize,
        uiscale = uiscale,
        page    = page,
        x       = cx + cw - cbSize,
        y       = rowY(rowIndex),
        default = data.current.vsync,
        zIndex  = zWidgets,
        onChange = function(checked, id, name)
            pending.vsync = checked
            engine.logInfo("VSync pending: " .. tostring(checked))
        end,
    })
    local vsCbId = graphicsTab.vsyncCheckboxId

    table.insert(rows, {
        labelHandle = vsLabelHandle,
        widgetHandles = {
            checkbox.getElementHandle(vsCbId),
        },
        widgetSetPosition = function(ry)
            checkbox.setPosition(vsCbId, cx + cw - cbSize, ry)
        end,
        widgetSetVisible = function(vis)
            checkbox.setVisible(vsCbId, vis)
        end,
    })
    rowIndex = rowIndex + 1

    ---------------------------------------------------------
    -- Row 4: Frame Limit (textbox)
    ---------------------------------------------------------
    local flLabelId = label.new({
        name     = "framelimit_label",
        text     = "Frame Limit",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = uiscale,
    })
    local flLabelHandle = label.getElementHandle(flLabelId)
    UI.addToPage(page, flLabelHandle, cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(flLabelHandle, zContent)

    local flW = math.floor(base.textboxWidth * uiscale)
    graphicsTab.frameLimitTextBoxId = textbox.new({
        name     = "framelimit_input",
        width    = base.textboxWidth,
        height   = base.textboxHeight,
        page     = page,
        x        = cx + cw - flW,
        y        = rowY(rowIndex),
        uiscale  = uiscale,
        font     = font,
        fontSize = 24,
        default  = tostring(data.current.frameLimit or 60),
        textType = textbox.Type.NUMBER,
        zIndex   = zWidgets,
    })
    local flId = graphicsTab.frameLimitTextBoxId

    table.insert(rows, {
        labelHandle = flLabelHandle,
        widgetHandles = {
            textbox.getElementHandle(flId),
        },
        widgetSetPosition = function(ry)
            textbox.setPosition(flId, cx + cw - flW, ry)
        end,
        widgetSetVisible = function(vis)
            textbox.setVisible(flId, vis)
        end,
    })
    rowIndex = rowIndex + 1

    ---------------------------------------------------------
    -- Row 5: MSAA (dropdown)
    ---------------------------------------------------------
    local msaaLabelId = label.new({
        name     = "msaa_label",
        text     = "Anti-Aliasing",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = uiscale,
    })
    local msaaLabelHandle = label.getElementHandle(msaaLabelId)
    UI.addToPage(page, msaaLabelHandle, cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(msaaLabelHandle, zContent)

    graphicsTab.msaaDropdownId = dropdown.new({
        name              = "msaa",
        options           = data.msaaOptions,
        default           = data.msaaToString(data.current.msaa),
        font              = font,
        fontSize          = 24,
        height            = base.dropdownHeight,
        page              = page,
        x = 0, y = 0,
        uiscale           = uiscale,
        zIndex            = zWidgets,
        validateChar      = graphicsTab.msaaValidator,
        matchFn           = graphicsTab.msaaMatcher,
        maxVisibleOptions = 4,
        onChange = function(value, text, id, name)
            pending.msaa = data.msaaFromString(value)
            engine.logInfo("MSAA pending: " .. text)
        end,
    })

    local ddMsaaId = graphicsTab.msaaDropdownId
    local ddMsaaW, _ = dropdown.getSize(ddMsaaId)
    dropdown.setPosition(ddMsaaId, cx + cw - ddMsaaW, rowY(rowIndex))

    table.insert(rows, {
        labelHandle = msaaLabelHandle,
        widgetHandles = {
            dropdown.getElementHandle(ddMsaaId),
            dropdown.getArrowHandle(ddMsaaId),
        },
        widgetSetPosition = function(ry)
            dropdown.setPosition(ddMsaaId, cx + cw - ddMsaaW, ry)
        end,
        widgetSetVisible = function(vis)
            dropdown.setVisible(ddMsaaId, vis)
        end,
    })
    rowIndex = rowIndex + 1

    ---------------------------------------------------------
    -- Row 6: Brightness (textbox — TODO: replace with slider)
    ---------------------------------------------------------

    local brLabelId = label.new({
        name     = "brightness_label",
        text     = "Brightness",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = uiscale,
    })
    local brLabelHandle = label.getElementHandle(brLabelId)
    UI.addToPage(page, brLabelHandle, cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(brLabelHandle, zContent)

    local slW = math.floor((base.sliderWidth or 200) * uiscale)
    graphicsTab.brightnessSlider = slider.new({
        name     = "brightness",
        width    = base.sliderWidth or 200,
        height   = base.sliderHeight or 24,
        min      = data.brightnessMin,
        max      = data.brightnessMax,
        default  = data.current.brightness,
        page     = page,
        x        = cx + cw - slW,
        y        = rowY(rowIndex),
        uiscale  = uiscale,
        zIndex   = zWidgets,
        onChange  = function(value, id, name)
            pending.brightness = math.floor(value)
        end,
    })
    local brSliderId = graphicsTab.brightnessSlider

    table.insert(rows, {
        labelHandle = brLabelHandle,
        widgetHandles = {
            slider.getElementHandle(brSliderId),
            slider.getKnobHandle(brSliderId),
        },
        widgetSetPosition = function(ry)
            slider.setPosition(brSliderId, cx + cw - slW, ry)
        end,
        widgetSetVisible = function(vis)
            slider.setVisible(brSliderId, vis)
        end,
    })
    rowIndex = rowIndex + 1

    ---------------------------------------------------------
    -- Row 7: UI Scaling (textbox)
    ---------------------------------------------------------
    local scaleLabelId = label.new({
        name     = "scaling_label",
        text     = "UI Scaling",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = uiscale,
    })
    local scaleLabelHandle = label.getElementHandle(scaleLabelId)
    UI.addToPage(page, scaleLabelHandle, cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(scaleLabelHandle, zContent)

    local tbW = math.floor(base.textboxWidth * uiscale)
    graphicsTab.uiScaleTextBoxId = textbox.new({
        name     = "uiscale_input",
        width    = base.textboxWidth,
        height   = base.textboxHeight,
        page     = page,
        x        = cx + cw - tbW,
        y        = rowY(rowIndex),
        uiscale  = uiscale,
        font     = font,
        fontSize = 24,
        default  = tostring(data.current.uiScale),
        textType = textbox.Type.SCALE,
        zIndex   = zWidgets,
    })
    local scaleId = graphicsTab.uiScaleTextBoxId

    table.insert(rows, {
        labelHandle = scaleLabelHandle,
        widgetHandles = {
            textbox.getElementHandle(scaleId),
        },
        widgetSetPosition = function(ry)
            textbox.setPosition(scaleId, cx + cw - tbW, ry)
        end,
        widgetSetVisible = function(vis)
            textbox.setVisible(scaleId, vis)
        end,
    })
    rowIndex = rowIndex + 1

    return rows
end

-----------------------------------------------------------
-- Read current widget values (for apply/save)
-----------------------------------------------------------

function graphicsTab.getWidgetValues()
    local vals = {}
    if graphicsTab.uiScaleTextBoxId then
        vals.uiScale = textbox.getNumericValue(graphicsTab.uiScaleTextBoxId)
    end
    if graphicsTab.frameLimitTextBoxId then
        vals.frameLimit = textbox.getNumericValue(graphicsTab.frameLimitTextBoxId)
    end
    if graphicsTab.brightnessSlider then
        vals.brightness = slider.getValue(graphicsTab.brightnessSlider)
    end
    return vals
end

-----------------------------------------------------------
-- TextBox submit handler
-----------------------------------------------------------

function graphicsTab.onTextBoxSubmit(name, value)
    engine.logInfo("TextBox submit: " .. tostring(name)
        .. " = " .. tostring(value))

    local validated, fallback = data.validateTextBoxSubmit(name, value)

    if name == "uiscale_input" then
        local target = graphicsTab.uiScaleTextBoxId
        if target then
            textbox.setText(target, tostring(validated or fallback))
        end
    elseif name == "framelimit_input" then
        local target = graphicsTab.frameLimitTextBoxId
        if target then
            textbox.setText(target, tostring(validated or fallback))
        end
    end
    -- brightness_input case removed — slider handles it directly
end

return graphicsTab
