-- Advanced Tab for Create World Menu
-- Creates widget rows for Plate Count (and future advanced params).
-- Returns element handles for show/hide tab switching.
--
-- Row order:
--   1. Plate Count (textbox)
local label   = require("scripts.ui.label")
local textbox = require("scripts.ui.textbox")

local advancedTab = {}

-- Widget IDs stored here so create_world_menu can read values
advancedTab.plateCountTextBoxId = nil
advancedTab.erosionIntensityTextBoxId = nil
advancedTab.volcanicActivityTextBoxId = nil

-----------------------------------------------------------
-- Create all rows
-----------------------------------------------------------

-- params = {
--   page, font, baseSizes, uiscale, s (scaled sizes),
--   contentX, contentY, contentW,
--   zContent, zWidgets,
--   pending,
--   trackLabel, trackTextBox,
-- }
-- Returns: elements[] for show/hide
function advancedTab.create(params)
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
    local pending  = params.pending

    local elements = {}
    local rowIndex = 0

    local function rowY(n)
        return cy + s.rowSpacing * n
    end

    ---------------------------------------------------------
    -- Row 1: Plate Count (textbox)
    ---------------------------------------------------------
    local plateLabelId = params.trackLabel(label.new({
        name     = "plate_count_label",
        text     = "Plate Count",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = uiscale,
    }))
    local plateLabelHandle = label.getElementHandle(plateLabelId)
    UI.addToPage(page, plateLabelHandle,
                 cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(plateLabelHandle, zContent)
    table.insert(elements, { type = "label", handle = plateLabelHandle })

    local tbW = math.floor(base.textboxWidth * uiscale)
    advancedTab.plateCountTextBoxId = params.trackTextBox(textbox.new({
        name     = "plate_count_input",
        width    = base.textboxWidth,
        height   = base.textboxHeight,
        page     = page,
        x        = cx + cw - tbW,
        y        = rowY(rowIndex),
        uiscale  = uiscale,
        font     = font,
        fontSize = 24,
        default  = pending.plateCount,
        textType = textbox.Type.NUMBER,
        zIndex   = zWidgets,
    }))
    table.insert(elements, { type = "textbox", id = advancedTab.plateCountTextBoxId })

    rowIndex = rowIndex + 1

    ---------------------------------------------------------
    -- Row 2: Erosion Intensity (textbox DECIMAL)
    ---------------------------------------------------------
    local erosionLabelId = params.trackLabel(label.new({
        name     = "erosion_intensity_label",
        text     = "Erosion Intensity",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = uiscale,
    }))
    local erosionLabelHandle = label.getElementHandle(erosionLabelId)
    UI.addToPage(page, erosionLabelHandle,
                 cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(erosionLabelHandle, zContent)
    table.insert(elements, { type = "label", handle = erosionLabelHandle })

    advancedTab.erosionIntensityTextBoxId = params.trackTextBox(textbox.new({
        name     = "erosion_intensity_input",
        width    = base.textboxWidth,
        height   = base.textboxHeight,
        page     = page,
        x        = cx + cw - tbW,
        y        = rowY(rowIndex),
        uiscale  = uiscale,
        font     = font,
        fontSize = 24,
        default  = pending.erosionIntensity,
        textType = textbox.Type.DECIMAL,
        zIndex   = zWidgets,
    }))
    table.insert(elements, { type = "textbox", id = advancedTab.erosionIntensityTextBoxId })

    rowIndex = rowIndex + 1

    ---------------------------------------------------------
    -- Row 3: Volcanic Activity (textbox DECIMAL)
    ---------------------------------------------------------
    local volcanicLabelId = params.trackLabel(label.new({
        name     = "volcanic_activity_label",
        text     = "Volcanic Activity",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = uiscale,
    }))
    local volcanicLabelHandle = label.getElementHandle(volcanicLabelId)
    UI.addToPage(page, volcanicLabelHandle,
                 cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(volcanicLabelHandle, zContent)
    table.insert(elements, { type = "label", handle = volcanicLabelHandle })

    advancedTab.volcanicActivityTextBoxId = params.trackTextBox(textbox.new({
        name     = "volcanic_activity_input",
        width    = base.textboxWidth,
        height   = base.textboxHeight,
        page     = page,
        x        = cx + cw - tbW,
        y        = rowY(rowIndex),
        uiscale  = uiscale,
        font     = font,
        fontSize = 24,
        default  = pending.volcanicActivity,
        textType = textbox.Type.DECIMAL,
        zIndex   = zWidgets,
    }))
    table.insert(elements, { type = "textbox", id = advancedTab.volcanicActivityTextBoxId })

    return elements
end

-----------------------------------------------------------
-- Read current widget values (for generate)
-----------------------------------------------------------

function advancedTab.getWidgetValues()
    local vals = {}
    if advancedTab.plateCountTextBoxId then
        vals.plateCount = textbox.getValue(advancedTab.plateCountTextBoxId)
    end
    if advancedTab.erosionIntensityTextBoxId then
        vals.erosionIntensity = textbox.getValue(advancedTab.erosionIntensityTextBoxId)
    end
    if advancedTab.volcanicActivityTextBoxId then
        vals.volcanicActivity = textbox.getValue(advancedTab.volcanicActivityTextBoxId)
    end
    return vals
end

return advancedTab
