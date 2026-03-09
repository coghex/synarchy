-- General Tab for Create World Menu
-- Combines calendar and astronomy (sun/moon) settings.
--
-- Row order:
--   Calendar:
--     1. Days / Month      (textbox NUMBER)
--     2. Months / Year     (textbox NUMBER)
--     3. Hours / Day       (textbox NUMBER)
--     4. Minutes / Hour    (textbox NUMBER)
--   Astronomy:
--     5. Axial Tilt        (textbox DECIMAL)
--     6. Day/Night Ratio   (textbox DECIMAL)
--     7. Lunar Cycle       (textbox NUMBER)
--     8. Moon Phase        (textbox DECIMAL)
local label   = require("scripts.ui.label")
local textbox = require("scripts.ui.textbox")

local generalTab = {}

-- Widget IDs — Calendar
generalTab.daysPerMonthId   = nil
generalTab.monthsPerYearId  = nil
generalTab.hoursPerDayId    = nil
generalTab.minutesPerHourId = nil

-- Widget IDs — Astronomy
generalTab.tiltAngleId   = nil
generalTab.dayLengthId   = nil
generalTab.cycleDaysId   = nil
generalTab.phaseOffsetId = nil

-----------------------------------------------------------
-- Create all rows
-----------------------------------------------------------

function generalTab.create(params)
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

    ---------------------------------------------------------
    -- Calendar
    ---------------------------------------------------------
    addRow("Days / Month",   "days_per_month",   "daysPerMonth",   textbox.Type.NUMBER,
        function(id) generalTab.daysPerMonthId = id end)
    addRow("Months / Year",  "months_per_year",  "monthsPerYear",  textbox.Type.NUMBER,
        function(id) generalTab.monthsPerYearId = id end)
    addRow("Hours / Day",    "hours_per_day",    "hoursPerDay",    textbox.Type.NUMBER,
        function(id) generalTab.hoursPerDayId = id end)
    addRow("Minutes / Hour", "minutes_per_hour", "minutesPerHour", textbox.Type.NUMBER,
        function(id) generalTab.minutesPerHourId = id end)

    ---------------------------------------------------------
    -- Astronomy
    ---------------------------------------------------------
    addRow("Axial Tilt",      "tilt_angle",   "tiltAngle",   textbox.Type.DECIMAL,
        function(id) generalTab.tiltAngleId = id end)
    addRow("Day/Night Ratio", "day_length",   "dayLength",   textbox.Type.DECIMAL,
        function(id) generalTab.dayLengthId = id end)
    addRow("Lunar Cycle",     "cycle_days",   "cycleDays",   textbox.Type.NUMBER,
        function(id) generalTab.cycleDaysId = id end)
    addRow("Moon Phase",      "phase_offset", "phaseOffset", textbox.Type.DECIMAL,
        function(id) generalTab.phaseOffsetId = id end)

    return elements
end

-----------------------------------------------------------
-- Read current widget values
-----------------------------------------------------------

function generalTab.getWidgetValues()
    local vals = {}
    -- Calendar
    if generalTab.daysPerMonthId then
        vals.daysPerMonth = textbox.getValue(generalTab.daysPerMonthId)
    end
    if generalTab.monthsPerYearId then
        vals.monthsPerYear = textbox.getValue(generalTab.monthsPerYearId)
    end
    if generalTab.hoursPerDayId then
        vals.hoursPerDay = textbox.getValue(generalTab.hoursPerDayId)
    end
    if generalTab.minutesPerHourId then
        vals.minutesPerHour = textbox.getValue(generalTab.minutesPerHourId)
    end
    -- Astronomy
    if generalTab.tiltAngleId then
        vals.tiltAngle = textbox.getValue(generalTab.tiltAngleId)
    end
    if generalTab.dayLengthId then
        vals.dayLength = textbox.getValue(generalTab.dayLengthId)
    end
    if generalTab.cycleDaysId then
        vals.cycleDays = textbox.getValue(generalTab.cycleDaysId)
    end
    if generalTab.phaseOffsetId then
        vals.phaseOffset = textbox.getValue(generalTab.phaseOffsetId)
    end
    return vals
end

return generalTab
