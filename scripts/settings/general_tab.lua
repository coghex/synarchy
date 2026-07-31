-- General Tab (#913)
-- The first settings tab, ahead of Graphics. Owns the autosave
-- controls:
--
--   Autosave           [X]
--   Interval (minutes) [ 10 ]
--   Keep Generations   [ 3  ]
--
-- All three edit PENDING state only (data.pendingSave) and reach the
-- engine through data.apply/data.save, exactly like the Graphics tab's
-- rows -- Back must be able to abandon an unapplied edit, and a live
-- write-through would make the interval restart on every keystroke.
local label      = require("scripts.ui.label")
local textbox    = require("scripts.ui.textbox")
local checkbox   = require("scripts.ui.checkbox")
local data       = require("scripts.settings.data")
local responsive = require("scripts.ui.responsive")

local generalTab = {}

generalTab.autosaveCheckboxId = nil
generalTab.intervalTextBoxId  = nil
generalTab.depthTextBoxId     = nil

-- Row labels, shared by the width fit below and the rows themselves so
-- the reserved label column is measured against what is actually drawn.
local ROW_LABELS = {
    "Autosave",
    "Interval (minutes)",
    "Keep Generations",
}

-----------------------------------------------------------
-- Create all rows
-----------------------------------------------------------

-- params: same shape every other settings tab receives (see
-- settings_menu.tabCreateParams). Returns rowHandles[].
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

    local rows = {}
    local rowIndex = 0

    local function rowY(n)
        return cy + s.rowSpacing * n
    end

    local cbSize = math.max(1, math.floor(base.checkboxSize * uiscale))

    -- #748's recurring pattern: reserve a label column and fit ONE
    -- effective local uiscale for every row label against it, so a long
    -- label at a high scale can never run into its own row's control.
    local LABEL_COLUMN_FRACTION = 0.35
    local labelFontSizePx = math.floor(base.fontSize * uiscale)
    local naturalLabelWidth = 0
    for _, t in ipairs(ROW_LABELS) do
        local w = engine.getTextWidth(font, t, labelFontSizePx)
        if w > naturalLabelWidth then naturalLabelWidth = w end
    end
    local labelUiscale = responsive.fitScale(
        naturalLabelWidth, cw * LABEL_COLUMN_FRACTION, uiscale)

    local function addRowLabel(name, text)
        local id = params.trackLabel(label.new({
            name     = name,
            text     = text,
            font     = font,
            fontSize = base.fontSize,
            color    = {1.0, 1.0, 1.0, 1.0},
            page     = page,
            uiscale  = labelUiscale,
        }))
        local handle = label.getElementHandle(id)
        UI.addToPage(page, handle, cx, rowY(rowIndex) + s.fontSize)
        UI.setZIndex(handle, zContent)
        return handle
    end

    ---------------------------------------------------------
    -- Row 1: Autosave enabled (checkbox)
    ---------------------------------------------------------
    local enabledLabelHandle = addRowLabel("autosave_label", "Autosave")

    generalTab.autosaveCheckboxId = params.trackCheckbox(checkbox.new({
        name    = "autosave_enabled",
        size    = cbSize,
        uiscale = 1.0,
        page    = page,
        x       = cx + cw - cbSize,
        y       = rowY(rowIndex),
        default = data.pendingSave.enabled,
        zIndex  = zWidgets,
        onChange = function(checked)
            data.pendingSave.enabled = checked
        end,
    }))
    local enabledCbId = generalTab.autosaveCheckboxId

    table.insert(rows, {
        labelHandle = enabledLabelHandle,
        widgetHandles = { checkbox.getElementHandle(enabledCbId) },
        widgetSetPosition = function(ry)
            checkbox.setPosition(enabledCbId, cx + cw - cbSize, ry)
        end,
        widgetSetVisible = function(vis)
            checkbox.setVisible(enabledCbId, vis)
        end,
    })
    rowIndex = rowIndex + 1

    ---------------------------------------------------------
    -- Row 2: Interval (textbox, whole minutes)
    ---------------------------------------------------------
    local intervalLabelHandle =
        addRowLabel("autosave_interval_label", "Interval (minutes)")

    local tbW = math.floor(base.textboxWidth * uiscale)
    generalTab.intervalTextBoxId = params.trackTextbox(textbox.new({
        name     = "autosave_interval_input",
        width    = base.textboxWidth,
        height   = base.textboxHeight,
        page     = page,
        x        = cx + cw - tbW,
        y        = rowY(rowIndex),
        uiscale  = uiscale,
        font     = font,
        fontSize = 24,
        default  = tostring(data.pendingSave.intervalMinutes),
        textType = textbox.Type.NUMBER,
        zIndex   = zWidgets,
    }))
    local intervalId = generalTab.intervalTextBoxId

    table.insert(rows, {
        labelHandle = intervalLabelHandle,
        widgetHandles = { textbox.getElementHandle(intervalId) },
        widgetSetPosition = function(ry)
            textbox.setPosition(intervalId, cx + cw - tbW, ry)
        end,
        widgetSetVisible = function(vis)
            textbox.setVisible(intervalId, vis)
        end,
    })
    rowIndex = rowIndex + 1

    ---------------------------------------------------------
    -- Row 3: Rotation depth (textbox, whole slots)
    ---------------------------------------------------------
    local depthLabelHandle =
        addRowLabel("autosave_depth_label", "Keep Generations")

    generalTab.depthTextBoxId = params.trackTextbox(textbox.new({
        name     = "autosave_depth_input",
        width    = base.textboxWidth,
        height   = base.textboxHeight,
        page     = page,
        x        = cx + cw - tbW,
        y        = rowY(rowIndex),
        uiscale  = uiscale,
        font     = font,
        fontSize = 24,
        default  = tostring(data.pendingSave.rotationDepth),
        textType = textbox.Type.NUMBER,
        zIndex   = zWidgets,
    }))
    local depthId = generalTab.depthTextBoxId

    table.insert(rows, {
        labelHandle = depthLabelHandle,
        widgetHandles = { textbox.getElementHandle(depthId) },
        widgetSetPosition = function(ry)
            textbox.setPosition(depthId, cx + cw - tbW, ry)
        end,
        widgetSetVisible = function(vis)
            textbox.setVisible(depthId, vis)
        end,
    })
    rowIndex = rowIndex + 1

    return rows
end

-----------------------------------------------------------
-- Widget value collection (read at Apply/Save time)
-----------------------------------------------------------

-- Mirrors graphicsTab.getWidgetValues: the live textbox contents win
-- over pending state so a value typed but never submitted with Enter is
-- still applied, which is what a player pressing Apply expects.
function generalTab.getWidgetValues()
    local values = {}
    if generalTab.autosaveCheckboxId then
        values.autosaveEnabled = checkbox.isChecked(generalTab.autosaveCheckboxId)
    end
    if generalTab.intervalTextBoxId then
        values.autosaveIntervalMinutes =
            tonumber(textbox.getText(generalTab.intervalTextBoxId))
    end
    if generalTab.depthTextBoxId then
        values.autosaveRotationDepth =
            tonumber(textbox.getText(generalTab.depthTextBoxId))
    end
    return values
end

-- Forwarded from settings_menu.onTextBoxSubmit. Clamps into the
-- validated range and writes the clamped value straight back into the
-- box, so what the player sees is what will be persisted.
function generalTab.onTextBoxSubmit(name, value)
    local clamped = data.validateSaveTextBoxSubmit(name, value)
    if clamped == nil then return end
    if name == "autosave_interval_input" and generalTab.intervalTextBoxId then
        textbox.setText(generalTab.intervalTextBoxId, tostring(clamped))
    elseif name == "autosave_depth_input" and generalTab.depthTextBoxId then
        textbox.setText(generalTab.depthTextBoxId, tostring(clamped))
    end
end

return generalTab
