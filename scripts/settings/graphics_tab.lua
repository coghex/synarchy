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
--   6. Brightness      (slider)
--   7. UI Scaling      (textbox)
--   8. Pixel Snap      (checkbox)
--   9. Texture Filter  (dropdown)
local label    = require("scripts.ui.label")
local textbox  = require("scripts.ui.textbox")
local checkbox = require("scripts.ui.checkbox")
local dropdown = require("scripts.ui.dropdown")
local slider   = require("scripts.ui.slider")
local data     = require("scripts.settings.data")
local responsive = require("scripts.ui.responsive")

local graphicsTab = {}

-- Widget IDs stored here so settings_menu can read values at apply time
graphicsTab.resolutionDropdownId    = nil
graphicsTab.windowModeDropdownId    = nil
graphicsTab.vsyncCheckboxId         = nil
graphicsTab.frameLimitTextBoxId     = nil
graphicsTab.msaaDropdownId          = nil
graphicsTab.brightnessSlider        = nil
graphicsTab.uiScaleTextBoxId        = nil
graphicsTab.pixelSnapCheckboxId     = nil
graphicsTab.textureFilterDropdownId = nil

-----------------------------------------------------------
-- MSAA dropdown validators
-- Only allow digits that could form "1", "2", "4", "8"
-----------------------------------------------------------

function graphicsTab.msaaValidator(char)
    return char == "1" or char == "2" or char == "4" or char == "8"
end

function graphicsTab.msaaMatcher(inputText, options)
    if not inputText or inputText == "" then return nil end
    -- Match against the value field (e.g. "1", "2", "4", "8")
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
-- Texture filter dropdown validators
-----------------------------------------------------------

function graphicsTab.textureFilterValidator(char)
    local c = char:lower()
    return c == "n" or c == "l"
end

function graphicsTab.textureFilterMatcher(inputText, options)
    if not inputText or inputText == "" then return nil end
    local lower = inputText:lower()
    for i, opt in ipairs(options) do
        if opt.text:lower():sub(1, #lower) == lower then
            return i
        end
    end
    -- Also match against the value field
    for i, opt in ipairs(options) do
        if opt.value:lower():sub(1, #lower) == lower then
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
--   trackLabel, trackTextbox, trackCheckbox, trackDropdown,
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

    -- Checkbox size used by multiple rows
    local cbSize = math.floor(base.checkboxSize * uiscale)

    -- #748 round 5: a dropdown's width is driven by its OPTION TEXT
    -- metrics (dropdown.measureOptions) plus a fixed minWidth floor —
    -- neither is a plain baseSizes field, so it can't be shrunk via
    -- settings_menu's `contentBase` (which only covers textbox/slider/
    -- checkbox widths). At a narrow content area and high uiscale (the
    -- supported 800x2160@4x combination), the unshrunk floor alone
    -- (100 * uiscale display width + a same-height arrow) already
    -- exceeds `cw`, driving the right-aligned dropdown's x position
    -- negative. Compute ONE effective, LOCAL uiscale for every dropdown
    -- in this tab — never the stored/configured scale — from whichever
    -- of the four option sets needs the most room at the tab's real
    -- uiscale, mirroring dropdown.lua's own displayWidth+arrow formula
    -- exactly so the fit is correct whether text metrics or the floor
    -- dominate.
    local dropFontSize = math.floor(24 * uiscale)
    local dropHeight = math.floor(base.dropdownHeight * uiscale)
    local naturalDropdownWidth = 0
    for _, opts in ipairs({
        data.resolutions, data.windowModes, data.msaaOptions, data.textureFilterOptions,
    }) do
        local displayW = math.max(
            dropdown.measureOptions(opts, font, dropFontSize),
            math.floor(100 * uiscale))
        local totalW = displayW + dropHeight
        if totalW > naturalDropdownWidth then naturalDropdownWidth = totalW end
    end
    -- #748 round 7: fit against a REDUCED target (not the full row
    -- width cw) that reserves a label column — the row's label sits at
    -- cx, the right-aligned dropdown at cx+cw-width; letting the
    -- dropdown's own fit consume the whole row left it landing back at
    -- ~cx too, overlapping the label. Mirrors create_world_menu's
    -- identical LABEL_COLUMN_FRACTION reservation.
    local LABEL_COLUMN_FRACTION = 0.35
    local dropdownUiscale = responsive.fitScale(
        naturalDropdownWidth, cw * (1 - LABEL_COLUMN_FRACTION), uiscale)

    -- #748 round 7: reserving a label column doesn't help if the LABEL
    -- itself still renders at the tab's full uiscale — a long label
    -- ("Tooltip Delay (ms)") at 4x can still be far wider than even a
    -- 35%-of-cw column, extending into and overlapping the row's own
    -- control regardless of how little room the control itself needs.
    -- Compute ONE effective, LOCAL uiscale for every row LABEL in this
    -- tab from whichever label text is widest, fit against the SAME
    -- reserved label column width — applied uniformly so no row's
    -- label jumps size relative to another.
    local ROW_LABELS = {
        "Resolution", "Window Mode", "VSync", "Frame Limit", "Anti-Aliasing",
        "Brightness", "UI Scaling", "Pixel Snap", "Texture Filter",
        "Tooltip Delay (ms)", "Hint Delay (ms)",
    }
    local labelFontSizePx = math.floor(base.fontSize * uiscale)
    local naturalLabelWidth = 0
    for _, t in ipairs(ROW_LABELS) do
        local w = engine.getTextWidth(font, t, labelFontSizePx)
        if w > naturalLabelWidth then naturalLabelWidth = w end
    end
    local labelUiscale = responsive.fitScale(
        naturalLabelWidth, cw * LABEL_COLUMN_FRACTION, uiscale)

    ---------------------------------------------------------
    -- Row 1: Resolution (dropdown)
    ---------------------------------------------------------
    local resLabelId = params.trackLabel(label.new({
        name     = "resolution_label",
        text     = "Resolution",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = labelUiscale,
    }))
    local resLabelHandle = label.getElementHandle(resLabelId)
    UI.addToPage(page, resLabelHandle, cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(resLabelHandle, zContent)

    local currentRes = data.resolutionString(
        pending.width, pending.height)

    graphicsTab.resolutionDropdownId = params.trackDropdown(dropdown.new({
        name              = "resolution",
        options           = data.resolutions,
        default           = currentRes,
        font              = font,
        fontSize          = 24,
        height            = base.dropdownHeight,
        page              = page,
        x = 0, y = 0,
        uiscale           = dropdownUiscale,
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
    }))

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
    local wmLabelId = params.trackLabel(label.new({
        name     = "windowmode_label",
        text     = "Window Mode",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = labelUiscale,
    }))
    local wmLabelHandle = label.getElementHandle(wmLabelId)
    UI.addToPage(page, wmLabelHandle, cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(wmLabelHandle, zContent)

    graphicsTab.windowModeDropdownId = params.trackDropdown(dropdown.new({
        name              = "window_mode",
        options           = data.windowModes,
        default           = pending.windowMode,
        font              = font,
        fontSize          = 24,
        height            = base.dropdownHeight,
        page              = page,
        x = 0, y = 0,
        uiscale           = dropdownUiscale,
        zIndex            = zWidgets,
        validateChar      = graphicsTab.windowModeValidator,
        matchFn           = graphicsTab.windowModeMatcher,
        maxVisibleOptions = 3,
        onChange = function(value, text, id, name)
            pending.windowMode = value
            engine.logInfo("Window mode pending: " .. text)
        end,
    }))

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
    local vsLabelId = params.trackLabel(label.new({
        name     = "vsync_label",
        text     = "VSync",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = labelUiscale,
    }))
    local vsLabelHandle = label.getElementHandle(vsLabelId)
    UI.addToPage(page, vsLabelHandle, cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(vsLabelHandle, zContent)

    graphicsTab.vsyncCheckboxId = params.trackCheckbox(checkbox.new({
        name    = "vsync",
        size    = base.checkboxSize,
        uiscale = uiscale,
        page    = page,
        x       = cx + cw - cbSize,
        y       = rowY(rowIndex),
        default = pending.vsync,
        zIndex  = zWidgets,
        onChange = function(checked, id, name)
            pending.vsync = checked
            engine.logInfo("VSync pending: " .. tostring(checked))
        end,
    }))
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
    local flLabelId = params.trackLabel(label.new({
        name     = "framelimit_label",
        text     = "Frame Limit",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = labelUiscale,
    }))
    local flLabelHandle = label.getElementHandle(flLabelId)
    UI.addToPage(page, flLabelHandle, cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(flLabelHandle, zContent)

    local flW = math.floor(base.textboxWidth * uiscale)
    graphicsTab.frameLimitTextBoxId = params.trackTextbox(textbox.new({
        name     = "framelimit_input",
        width    = base.textboxWidth,
        height   = base.textboxHeight,
        page     = page,
        x        = cx + cw - flW,
        y        = rowY(rowIndex),
        uiscale  = uiscale,
        font     = font,
        fontSize = 24,
        default  = tostring(pending.frameLimit or 60),
        textType = textbox.Type.NUMBER,
        zIndex   = zWidgets,
    }))
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
    local msaaLabelId = params.trackLabel(label.new({
        name     = "msaa_label",
        text     = "Anti-Aliasing",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = labelUiscale,
    }))
    local msaaLabelHandle = label.getElementHandle(msaaLabelId)
    UI.addToPage(page, msaaLabelHandle, cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(msaaLabelHandle, zContent)

    graphicsTab.msaaDropdownId = params.trackDropdown(dropdown.new({
        name              = "msaa",
        options           = data.msaaOptions,
        default           = data.msaaToString(pending.msaa),
        font              = font,
        fontSize          = 24,
        height            = base.dropdownHeight,
        page              = page,
        x = 0, y = 0,
        uiscale           = dropdownUiscale,
        zIndex            = zWidgets,
        validateChar      = graphicsTab.msaaValidator,
        matchFn           = graphicsTab.msaaMatcher,
        maxVisibleOptions = 4,
        onChange = function(value, text, id, name)
            pending.msaa = data.msaaFromString(value)
            engine.logInfo("MSAA pending: " .. text)
        end,
    }))

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
    -- Row 6: Brightness (slider)
    ---------------------------------------------------------
    local brLabelId = params.trackLabel(label.new({
        name     = "brightness_label",
        text     = "Brightness",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = labelUiscale,
    }))
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
        default  = pending.brightness,
        page     = page,
        x        = cx + cw - slW,
        y        = rowY(rowIndex),
        uiscale  = uiscale,
        zIndex   = zWidgets,
        onChange  = function(value, id, name)
            local clamped = math.floor(value)
            pending.brightness = clamped
            -- brightness is a live preview
            engine.setBrightness(clamped)
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
    local scaleLabelId = params.trackLabel(label.new({
        name     = "scaling_label",
        text     = "UI Scaling",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = labelUiscale,
    }))
    local scaleLabelHandle = label.getElementHandle(scaleLabelId)
    UI.addToPage(page, scaleLabelHandle, cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(scaleLabelHandle, zContent)

    local tbW = math.floor(base.textboxWidth * uiscale)
    graphicsTab.uiScaleTextBoxId = params.trackTextbox(textbox.new({
        name     = "uiscale_input",
        width    = base.textboxWidth,
        height   = base.textboxHeight,
        page     = page,
        x        = cx + cw - tbW,
        y        = rowY(rowIndex),
        uiscale  = uiscale,
        font     = font,
        fontSize = 24,
        default  = tostring(pending.uiScale),
        textType = textbox.Type.SCALE,
        zIndex   = zWidgets,
    }))
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

    ---------------------------------------------------------
    -- Row 8: Pixel Snap (checkbox)
    ---------------------------------------------------------
    local psLabelId = params.trackLabel(label.new({
        name     = "pixelsnap_label",
        text     = "Pixel Snap",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = labelUiscale,
        tooltip  = "Snaps sprites to integer pixel positions for a sharper, retro look",
    }))
    local psLabelHandle = label.getElementHandle(psLabelId)
    UI.addToPage(page, psLabelHandle, cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(psLabelHandle, zContent)

    graphicsTab.pixelSnapCheckboxId = params.trackCheckbox(checkbox.new({
        name    = "pixel_snap",
        size    = base.checkboxSize,
        uiscale = uiscale,
        page    = page,
        x       = cx + cw - cbSize,
        y       = rowY(rowIndex),
        default = pending.pixelSnap,
        zIndex  = zWidgets,
        onChange = function(checked, id, name)
            pending.pixelSnap = checked
            engine.logInfo("Pixel snap pending: " .. tostring(checked))
        end,
    }))
    local psCbId = graphicsTab.pixelSnapCheckboxId

    table.insert(rows, {
        labelHandle = psLabelHandle,
        widgetHandles = {
            checkbox.getElementHandle(psCbId),
        },
        widgetSetPosition = function(ry)
            checkbox.setPosition(psCbId, cx + cw - cbSize, ry)
        end,
        widgetSetVisible = function(vis)
            checkbox.setVisible(psCbId, vis)
        end,
    })
    rowIndex = rowIndex + 1

    ---------------------------------------------------------
    -- Row 9: Texture Filter (dropdown)
    ---------------------------------------------------------
    local tfLabelId = params.trackLabel(label.new({
        name     = "texture_filter_label",
        text     = "Texture Filter",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = labelUiscale,
        tooltipRich = {
            text = "How textures are sampled when scaled",
            hint = "Nearest: pixel-perfect, crisp art    Linear: smooth bilinear blur",
        },
    }))
    local tfLabelHandle = label.getElementHandle(tfLabelId)
    UI.addToPage(page, tfLabelHandle, cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(tfLabelHandle, zContent)

    graphicsTab.textureFilterDropdownId = params.trackDropdown(dropdown.new({
        name              = "texture_filter",
        options           = data.textureFilterOptions,
        default           = pending.textureFilter,
        font              = font,
        fontSize          = 24,
        height            = base.dropdownHeight,
        page              = page,
        x = 0, y = 0,
        uiscale           = dropdownUiscale,
        zIndex            = zWidgets,
        validateChar      = graphicsTab.textureFilterValidator,
        matchFn           = graphicsTab.textureFilterMatcher,
        maxVisibleOptions = 2,
        onChange = function(value, text, id, name)
            pending.textureFilter = value
            engine.logInfo("Texture filter pending: " .. text)
        end,
    }))

    local ddTfId = graphicsTab.textureFilterDropdownId
    local ddTfW, _ = dropdown.getSize(ddTfId)
    dropdown.setPosition(ddTfId, cx + cw - ddTfW, rowY(rowIndex))

    table.insert(rows, {
        labelHandle = tfLabelHandle,
        widgetHandles = {
            dropdown.getElementHandle(ddTfId),
            dropdown.getArrowHandle(ddTfId),
        },
        widgetSetPosition = function(ry)
            dropdown.setPosition(ddTfId, cx + cw - ddTfW, ry)
        end,
        widgetSetVisible = function(vis)
            dropdown.setVisible(ddTfId, vis)
        end,
    })
    rowIndex = rowIndex + 1

    ---------------------------------------------------------
    -- Row 10: Tooltip Dwell (slider, 0-1000 ms)
    ---------------------------------------------------------
    local tdLabelId = params.trackLabel(label.new({
        name     = "tooltip_dwell_label",
        text     = "Tooltip Delay (ms)",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = labelUiscale,
        tooltip  = "How long the cursor must rest on an element before its tooltip appears",
    }))
    local tdLabelHandle = label.getElementHandle(tdLabelId)
    UI.addToPage(page, tdLabelHandle, cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(tdLabelHandle, zContent)

    graphicsTab.tooltipDwellSlider = slider.new({
        name     = "tooltip_dwell",
        width    = base.sliderWidth or 200,
        height   = base.sliderHeight or 24,
        min      = data.tooltipDwellMin,
        max      = data.tooltipDwellMax,
        default  = pending.tooltipDwellMs,
        page     = page,
        x        = cx + cw - slW,
        y        = rowY(rowIndex),
        uiscale  = uiscale,
        zIndex   = zWidgets,
        onChange = function(value, id, name)
            local clamped = math.floor(value)
            pending.tooltipDwellMs = clamped
            -- Live preview: persists config + updates tooltip style.
            engine.setTooltipDwellMs(clamped)
        end,
    })
    local tdSliderId = graphicsTab.tooltipDwellSlider

    table.insert(rows, {
        labelHandle = tdLabelHandle,
        widgetHandles = {
            slider.getElementHandle(tdSliderId),
            slider.getKnobHandle(tdSliderId),
        },
        widgetSetPosition = function(ry)
            slider.setPosition(tdSliderId, cx + cw - slW, ry)
        end,
        widgetSetVisible = function(vis)
            slider.setVisible(tdSliderId, vis)
        end,
    })
    rowIndex = rowIndex + 1

    ---------------------------------------------------------
    -- Row 11: Hint Delay (slider, 0-1000 ms, cumulative with Tooltip Delay)
    ---------------------------------------------------------
    local hdLabelId = params.trackLabel(label.new({
        name     = "tooltip_hint_delay_label",
        text     = "Hint Delay (ms)",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = labelUiscale,
        tooltip  = "Extra delay after the tooltip appears before the hint section transitions in",
    }))
    local hdLabelHandle = label.getElementHandle(hdLabelId)
    UI.addToPage(page, hdLabelHandle, cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(hdLabelHandle, zContent)

    graphicsTab.tooltipHintDelaySlider = slider.new({
        name     = "tooltip_hint_delay",
        width    = base.sliderWidth or 200,
        height   = base.sliderHeight or 24,
        min      = data.tooltipHintDelayMin,
        max      = data.tooltipHintDelayMax,
        default  = pending.tooltipHintDelayMs,
        page     = page,
        x        = cx + cw - slW,
        y        = rowY(rowIndex),
        uiscale  = uiscale,
        zIndex   = zWidgets,
        onChange = function(value, id, name)
            local clamped = math.floor(value)
            pending.tooltipHintDelayMs = clamped
            engine.setTooltipHintDelayMs(clamped)
        end,
    })
    local hdSliderId = graphicsTab.tooltipHintDelaySlider

    table.insert(rows, {
        labelHandle = hdLabelHandle,
        widgetHandles = {
            slider.getElementHandle(hdSliderId),
            slider.getKnobHandle(hdSliderId),
        },
        widgetSetPosition = function(ry)
            slider.setPosition(hdSliderId, cx + cw - slW, ry)
        end,
        widgetSetVisible = function(vis)
            slider.setVisible(hdSliderId, vis)
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
    if graphicsTab.tooltipDwellSlider then
        vals.tooltipDwellMs = slider.getValue(graphicsTab.tooltipDwellSlider)
    end
    if graphicsTab.tooltipHintDelaySlider then
        vals.tooltipHintDelayMs = slider.getValue(graphicsTab.tooltipHintDelaySlider)
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
end

return graphicsTab
