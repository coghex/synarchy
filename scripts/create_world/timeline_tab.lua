-- Timeline Tab for Create World Menu
-- Controls geological timeline depth: how many of each segment the
-- worldgen timeline produces. Eon + Era are fixed counts; Period, Epoch
-- and Age are min–max ranges (a uniform random count is rolled per
-- parent). Fewer Ages ⇒ much faster worldgen (less per-tile erosion
-- replay). Returns element handles for show/hide tab switching.
local label      = require("scripts.ui.label")
local textbox    = require("scripts.ui.textbox")
local responsive = require("scripts.ui.responsive")

local timelineTab = {}

-- Widget IDs stored here so generation.lua can read values back.
timelineTab.eonId       = nil
timelineTab.eraId       = nil
timelineTab.periodMinId = nil
timelineTab.periodMaxId = nil
timelineTab.epochMinId  = nil
timelineTab.epochMaxId  = nil
timelineTab.ageMinId    = nil
timelineTab.ageMaxId    = nil

-----------------------------------------------------------
-- Row helpers
-----------------------------------------------------------

local function addLabel(params, elements, name, text, tooltip, cx, y, labelUiscale)
    local id = params.trackLabel(label.new({
        name     = name,
        text     = text,
        font     = params.font,
        fontSize = params.baseSizes.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = params.page,
        uiscale  = labelUiscale or params.uiscale,
        tooltip  = tooltip,
    }))
    local h = label.getElementHandle(id)
    UI.addChild(params.container, h, cx, y + params.s.fontSize)
    UI.setZIndex(h, params.zContent)
    table.insert(elements, { type = "label", handle = h })
end

local function addBox(params, elements, name, x, y, default)
    local id = params.trackTextBox(textbox.new({
        name     = name,
        width    = params.baseSizes.textboxWidth,
        height   = params.baseSizes.textboxHeight,
        page     = params.page,
        parent   = params.container,
        x        = x,
        y        = y,
        uiscale  = params.uiscale,
        font     = params.font,
        fontSize = 24,
        default  = default,
        textType = textbox.Type.NUMBER,
        zIndex   = params.zWidgets,
    }))
    table.insert(elements, { type = "textbox", id = id })
    return id
end

-----------------------------------------------------------
-- Create all rows
-----------------------------------------------------------

function timelineTab.create(params)
    local cx, cy, cw = params.contentX, params.contentY, params.contentW
    local s          = params.s
    local uiscale    = params.uiscale
    local pending    = params.pending
    local elements   = {}

    local function rowY(n) return cy + s.rowSpacing * n end
    local tbW = math.floor(params.baseSizes.textboxWidth * uiscale)
    local gap = math.floor(8 * uiscale)
    local xMax = cx + cw - tbW            -- right-aligned (single box / max box)
    local xMin = cx + cw - 2 * tbW - gap  -- min box (left of max)

    -- #748 round 7: see settings_tab.lua's identical comment — the
    -- control's shrink (via computeContentScaleFactor) already
    -- reserves a label column, but the label itself still needs its
    -- own effective uiscale to actually fit inside it. This tab's
    -- labels are the longest of the four create-world tab modules.
    local LABEL_COLUMN_FRACTION = 0.35
    local labelFontSizePx = math.floor(params.baseSizes.fontSize * uiscale)
    local naturalLabelWidth = 0
    for _, t in ipairs({
        "Eons", "Eras / eon", "Periods / era  (min–max)",
        "Epochs / period  (min–max)", "Ages / epoch  (min–max)",
    }) do
        local w = engine.getTextWidth(params.font, t, labelFontSizePx)
        if w > naturalLabelWidth then naturalLabelWidth = w end
    end
    local labelUiscale = responsive.fitScale(
        naturalLabelWidth, cw * LABEL_COLUMN_FRACTION, uiscale)

    -- Row 0: Eons (single box)
    addLabel(params, elements, "timeline_eon_label", "Eons",
        "How many eons (full planetary lifecycles). Each eon re-bombards the planet "
        .. "(atmosphere lost and regained between eons).", cx, rowY(0), labelUiscale)
    timelineTab.eonId = addBox(params, elements, "tl_eon_input", xMax, rowY(0), pending.timelineEon)

    -- Row 1: Eras per eon (single box)
    addLabel(params, elements, "timeline_era_label", "Eras / eon",
        "Number of eras per eon (fixed count).", cx, rowY(1), labelUiscale)
    timelineTab.eraId = addBox(params, elements, "tl_era_input", xMax, rowY(1), pending.timelineEra)

    -- Row 2: Periods per era (min–max)
    addLabel(params, elements, "timeline_period_label", "Periods / era  (min–max)",
        "Periods per era: a uniform random count in this range.", cx, rowY(2), labelUiscale)
    timelineTab.periodMinId = addBox(params, elements, "tl_pmin_input", xMin, rowY(2), pending.periodMin)
    timelineTab.periodMaxId = addBox(params, elements, "tl_pmax_input", xMax, rowY(2), pending.periodMax)

    -- Row 3: Epochs per period (min–max)
    addLabel(params, elements, "timeline_epoch_label", "Epochs / period  (min–max)",
        "Epochs per period: a uniform random count in this range.", cx, rowY(3), labelUiscale)
    timelineTab.epochMinId = addBox(params, elements, "tl_emin_input", xMin, rowY(3), pending.epochMin)
    timelineTab.epochMaxId = addBox(params, elements, "tl_emax_input", xMax, rowY(3), pending.epochMax)

    -- Row 4: Ages per epoch (min–max)
    addLabel(params, elements, "timeline_age_label", "Ages / epoch  (min–max)",
        "Ages per epoch: a uniform random count in this range. Ages dominate worldgen "
        .. "cost — fewer ages generate much faster (but with less erosion, so terrain "
        .. "may be more dramatic).", cx, rowY(4), labelUiscale)
    timelineTab.ageMinId = addBox(params, elements, "tl_amin_input", xMin, rowY(4), pending.ageMin)
    timelineTab.ageMaxId = addBox(params, elements, "tl_amax_input", xMax, rowY(4), pending.ageMax)

    return elements, 5
end

-----------------------------------------------------------
-- Read current widget values (for generate)
-----------------------------------------------------------

function timelineTab.getWidgetValues()
    local v = {}
    if timelineTab.eonId       then v.eonCount  = textbox.getValue(timelineTab.eonId)       end
    if timelineTab.eraId       then v.eraCount  = textbox.getValue(timelineTab.eraId)       end
    if timelineTab.periodMinId then v.periodMin = textbox.getValue(timelineTab.periodMinId) end
    if timelineTab.periodMaxId then v.periodMax = textbox.getValue(timelineTab.periodMaxId) end
    if timelineTab.epochMinId  then v.epochMin  = textbox.getValue(timelineTab.epochMinId)  end
    if timelineTab.epochMaxId  then v.epochMax  = textbox.getValue(timelineTab.epochMaxId)  end
    if timelineTab.ageMinId    then v.ageMin    = textbox.getValue(timelineTab.ageMinId)    end
    if timelineTab.ageMaxId    then v.ageMax    = textbox.getValue(timelineTab.ageMaxId)    end
    return v
end

return timelineTab
