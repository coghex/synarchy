-- Climate Tab for Create World Menu
-- Creates widget rows for climate simulation parameters.
--
-- Row order:
--   1. Iterations        (textbox NUMBER)
--   2. Coriolis Scale    (textbox DECIMAL)
--   3. Wind Drag         (textbox DECIMAL)
--   4. Thermal Inertia   (textbox DECIMAL)
--   5. Orographic Scale  (textbox DECIMAL)
--   6. Evaporation Scale (textbox DECIMAL)
--   7. Albedo Feedback   (textbox DECIMAL)
--   8. THC Threshold     (textbox DECIMAL)
local label   = require("scripts.ui.label")
local textbox = require("scripts.ui.textbox")

local climateTab = {}

-- Widget IDs
climateTab.iterationsId      = nil
climateTab.coriolisScaleId   = nil
climateTab.windDragId        = nil
climateTab.thermalInertiaId  = nil
climateTab.orographicScaleId = nil
climateTab.evapScaleId       = nil
climateTab.albedoFeedbackId  = nil
climateTab.thcThresholdId    = nil

-----------------------------------------------------------
-- Create all rows
-----------------------------------------------------------

function climateTab.create(params)
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

    local function addRow(labelText, name, pendingKey, textType, widgetIdSetter)
        local lblId = params.trackLabel(label.new({
            name     = name .. "_label",
            text     = labelText,
            font     = font,
            fontSize = base.fontSize,
            color    = {1.0, 1.0, 1.0, 1.0},
            page     = page,
            uiscale  = uiscale,
        }))
        local lblHandle = label.getElementHandle(lblId)
        UI.addToPage(page, lblHandle, cx, rowY(rowIndex) + s.fontSize)
        UI.setZIndex(lblHandle, zContent)
        table.insert(elements, { type = "label", handle = lblHandle })

        local tbW = math.floor(base.textboxWidth * uiscale)
        local tbId = params.trackTextBox(textbox.new({
            name     = name .. "_input",
            width    = base.textboxWidth,
            height   = base.textboxHeight,
            page     = page,
            x        = cx + cw - tbW,
            y        = rowY(rowIndex),
            uiscale  = uiscale,
            font     = font,
            fontSize = 24,
            default  = pending[pendingKey],
            textType = textType,
            zIndex   = zWidgets,
        }))
        table.insert(elements, { type = "textbox", id = tbId })
        widgetIdSetter(tbId)

        rowIndex = rowIndex + 1
    end

    addRow("Iterations",    "iterations",       "climateIterations",  textbox.Type.NUMBER,
        function(id) climateTab.iterationsId = id end)
    addRow("Coriolis",      "coriolis_scale",   "coriolisScale",      textbox.Type.DECIMAL,
        function(id) climateTab.coriolisScaleId = id end)
    addRow("Wind Drag",     "wind_drag",        "windDrag",           textbox.Type.DECIMAL,
        function(id) climateTab.windDragId = id end)
    addRow("Thermal Inertia","thermal_inertia", "thermalInertia",     textbox.Type.DECIMAL,
        function(id) climateTab.thermalInertiaId = id end)
    addRow("Orographic",    "orographic_scale", "orographicScale",    textbox.Type.DECIMAL,
        function(id) climateTab.orographicScaleId = id end)
    addRow("Evaporation",   "evap_scale",       "evapScale",          textbox.Type.DECIMAL,
        function(id) climateTab.evapScaleId = id end)
    addRow("Albedo Feedback","albedo_feedback",  "albedoFeedback",    textbox.Type.DECIMAL,
        function(id) climateTab.albedoFeedbackId = id end)
    addRow("THC Threshold", "thc_threshold",    "thcThreshold",       textbox.Type.DECIMAL,
        function(id) climateTab.thcThresholdId = id end)

    return elements
end

-----------------------------------------------------------
-- Read current widget values
-----------------------------------------------------------

function climateTab.getWidgetValues()
    local vals = {}
    if climateTab.iterationsId then
        vals.climateIterations = textbox.getValue(climateTab.iterationsId)
    end
    if climateTab.coriolisScaleId then
        vals.coriolisScale = textbox.getValue(climateTab.coriolisScaleId)
    end
    if climateTab.windDragId then
        vals.windDrag = textbox.getValue(climateTab.windDragId)
    end
    if climateTab.thermalInertiaId then
        vals.thermalInertia = textbox.getValue(climateTab.thermalInertiaId)
    end
    if climateTab.orographicScaleId then
        vals.orographicScale = textbox.getValue(climateTab.orographicScaleId)
    end
    if climateTab.evapScaleId then
        vals.evapScale = textbox.getValue(climateTab.evapScaleId)
    end
    if climateTab.albedoFeedbackId then
        vals.albedoFeedback = textbox.getValue(climateTab.albedoFeedbackId)
    end
    if climateTab.thcThresholdId then
        vals.thcThreshold = textbox.getValue(climateTab.thcThresholdId)
    end
    return vals
end

return climateTab
