-- Graphics Tab
-- Creates all widget rows for the Graphics settings tab.
-- Returns rowHandles[] for the scroll system.
local label    = require("scripts.ui.label")
local textbox  = require("scripts.ui.textbox")
local checkbox = require("scripts.ui.checkbox")
local dropdown = require("scripts.ui.dropdown")
local data     = require("scripts.settings.data")

local graphicsTab = {}

-- Widget IDs stored here so settings_menu can read values at apply time
graphicsTab.resolutionDropdownId = nil
graphicsTab.fullscreenCheckboxId = nil
graphicsTab.uiScaleTextBoxId    = nil
graphicsTab.frameLimitTextBoxId  = nil

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

    ---------------------------------------------------------
    -- Row 1: Resolution
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
    UI.addToPage(page, resLabelHandle, cx, cy + s.fontSize)
    UI.setZIndex(resLabelHandle, zContent)

    local currentRes = data.resolutionString(
        data.current.width, data.current.height)

    graphicsTab.resolutionDropdownId = dropdown.new({
        name             = "resolution",
        options          = data.resolutions,
        default          = currentRes,
        font             = font,
        fontSize         = 24,
        height           = base.dropdownHeight,
        page             = page,
        x = 0, y = 0,
        uiscale          = uiscale,
        zIndex           = zWidgets,
        validateChar     = dropdown.resolutionValidator,
        matchFn          = dropdown.resolutionMatcher,
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

    local ddId = graphicsTab.resolutionDropdownId
    local ddW, _ = dropdown.getSize(ddId)
    dropdown.setPosition(ddId, cx + cw - ddW, cy)

    table.insert(rows, {
        labelHandle = resLabelHandle,
        widgetHandles = {
            dropdown.getElementHandle(ddId),
            dropdown.getArrowHandle(ddId),
        },
        widgetSetPosition = function(rowY)
            dropdown.setPosition(ddId, cx + cw - ddW, rowY)
        end,
        widgetSetVisible = function(vis)
            dropdown.setVisible(ddId, vis)
        end,
    })

    ---------------------------------------------------------
    -- Row 2: Fullscreen
    ---------------------------------------------------------
    local fsLabelId = label.new({
        name     = "fullscreen_label",
        text     = "Fullscreen",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = uiscale,
    })
    local fsLabelHandle = label.getElementHandle(fsLabelId)
    UI.addToPage(page, fsLabelHandle, cx, cy + s.rowSpacing + s.fontSize)
    UI.setZIndex(fsLabelHandle, zContent)

    local cbSize = math.floor(base.checkboxSize * uiscale)
    graphicsTab.fullscreenCheckboxId = checkbox.new({
        name    = "fullscreen",
        size    = base.checkboxSize,
        uiscale = uiscale,
        page    = page,
        x       = cx + cw - cbSize,
        y       = cy + s.rowSpacing,
        default = data.current.fullscreen,
        zIndex  = zWidgets,
        onChange = function(checked, id, name)
            pending.fullscreen = checked
            engine.logInfo("Fullscreen pending: " .. tostring(checked))
        end,
    })
    local cbId = graphicsTab.fullscreenCheckboxId

    table.insert(rows, {
        labelHandle = fsLabelHandle,
        widgetHandles = {
            checkbox.getElementHandle(cbId),
        },
        widgetSetPosition = function(rowY)
            checkbox.setPosition(cbId, cx + cw - cbSize, rowY)
        end,
        widgetSetVisible = function(vis)
            checkbox.setVisible(cbId, vis)
        end,
    })

    ---------------------------------------------------------
    -- Row 3: UI Scaling
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
    UI.addToPage(page, scaleLabelHandle,
        cx, cy + s.rowSpacing * 2 + s.fontSize)
    UI.setZIndex(scaleLabelHandle, zContent)

    local tbW = math.floor(base.textboxWidth * uiscale)
    graphicsTab.uiScaleTextBoxId = textbox.new({
        name     = "uiscale_input",
        width    = base.textboxWidth,
        height   = base.textboxHeight,
        page     = page,
        x        = cx + cw - tbW,
        y        = cy + s.rowSpacing * 2,
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
        widgetSetPosition = function(rowY)
            textbox.setPosition(scaleId, cx + cw - tbW, rowY)
        end,
        widgetSetVisible = function(vis)
            textbox.setVisible(scaleId, vis)
        end,
    })

    ---------------------------------------------------------
    -- Row 4: Frame Limit
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
    UI.addToPage(page, flLabelHandle,
        cx, cy + s.rowSpacing * 3 + s.fontSize)
    UI.setZIndex(flLabelHandle, zContent)

    local flW = math.floor(base.textboxWidth * uiscale)
    graphicsTab.frameLimitTextBoxId = textbox.new({
        name     = "framelimit_input",
        width    = base.textboxWidth,
        height   = base.textboxHeight,
        page     = page,
        x        = cx + cw - flW,
        y        = cy + s.rowSpacing * 3,
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
        widgetSetPosition = function(rowY)
            textbox.setPosition(flId, cx + cw - flW, rowY)
        end,
        widgetSetVisible = function(vis)
            textbox.setVisible(flId, vis)
        end,
    })

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
        if validated then
            if graphicsTab.uiScaleTextBoxId then
                textbox.setText(graphicsTab.uiScaleTextBoxId,
                    tostring(validated))
            end
        elseif fallback then
            if graphicsTab.uiScaleTextBoxId then
                textbox.setText(graphicsTab.uiScaleTextBoxId,
                    tostring(fallback))
            end
        end
    elseif name == "framelimit_input" then
        if validated then
            if graphicsTab.frameLimitTextBoxId then
                textbox.setText(graphicsTab.frameLimitTextBoxId,
                    tostring(validated))
            end
        elseif fallback then
            if graphicsTab.frameLimitTextBoxId then
                textbox.setText(graphicsTab.frameLimitTextBoxId,
                    tostring(fallback))
            end
        end
    end
end

return graphicsTab
