-- General Tab for Create World Menu
-- Adds the calendar settings that affect runtime behavior.
--
-- Row order:
--   1. Days / Month      (textbox NUMBER)
--   2. Months / Year     (textbox NUMBER)
local label   = require("scripts.ui.label")
local textbox = require("scripts.ui.textbox")

local generalTab = {}

generalTab.daysPerMonthId  = nil
generalTab.monthsPerYearId = nil

-----------------------------------------------------------
-- Create all rows
-----------------------------------------------------------

function generalTab.create(params)
    local page      = params.page
    local font      = params.font
    local base      = params.baseSizes
    local uiscale   = params.uiscale
    local s         = params.s
    local cx        = params.contentX
    local cy        = params.contentY
    local cw        = params.contentW
    local zContent  = params.zContent
    local zWidgets  = params.zWidgets
    local pending   = params.pending
    local container = params.container

    local elements = {}
    local rowIndex = 0

    local function rowY(n)
        return cy + s.rowSpacing * n
    end

    local function addRow(labelText, name, pendingKey, textType, widgetIdSetter, tooltip)
        local lblId = params.trackLabel(label.new({
            name     = name .. "_label",
            text     = labelText,
            font     = font,
            fontSize = base.fontSize,
            color    = {1.0, 1.0, 1.0, 1.0},
            page     = page,
            uiscale  = uiscale,
            tooltip  = tooltip,
        }))
        local lblHandle = label.getElementHandle(lblId)
        UI.addChild(container, lblHandle, cx, rowY(rowIndex) + s.fontSize)
        UI.setZIndex(lblHandle, zContent)
        table.insert(elements, { type = "label", handle = lblHandle })

        local tbW = math.floor(base.textboxWidth * uiscale)
        local tbId = params.trackTextBox(textbox.new({
            name     = name .. "_input",
            width    = base.textboxWidth,
            height   = base.textboxHeight,
            page     = page,
            parent   = container,
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

    addRow("Days / Month",   "days_per_month",   "daysPerMonth",   textbox.Type.NUMBER,
        function(id) generalTab.daysPerMonthId = id end)
    addRow("Months / Year",  "months_per_year",  "monthsPerYear",  textbox.Type.NUMBER,
        function(id) generalTab.monthsPerYearId = id end)

    return elements, 2
end

-----------------------------------------------------------
-- Read current widget values
-----------------------------------------------------------

function generalTab.getWidgetValues()
    local vals = {}
    if generalTab.daysPerMonthId then
        vals.daysPerMonth = textbox.getValue(generalTab.daysPerMonthId)
    end
    if generalTab.monthsPerYearId then
        vals.monthsPerYear = textbox.getValue(generalTab.monthsPerYearId)
    end
    return vals
end

return generalTab
