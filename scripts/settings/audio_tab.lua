local label = require("scripts.ui.label")
local slider = require("scripts.ui.slider")
local responsive = require("scripts.ui.responsive")
local data = require("scripts.settings.data")
local audioTab = {}

local fields = {{"master", "Master"}, {"world", "World"}, {"ui", "Interface"}}

function audioTab.create(p)
    local rows = {}
    local cw = math.max(1, p.contentW)
    local fontSize = math.max(1, math.floor(p.baseSizes.fontSize * p.uiscale))
    local widest = 0
    for _, field in ipairs(fields) do
        widest = math.max(widest, engine.getTextWidth(p.font, field[2] .. " 100%", fontSize))
    end
    local labelScale = responsive.fitScale(widest, cw * 0.4, p.uiscale)
    -- Fit the WHOLE slider, including caps and knob. Shrinking only its width
    -- can otherwise make the track negative at narrow/high-scale sizes.
    local sliderScale = responsive.fitScale(200 * p.uiscale, cw * 0.55, p.uiscale)
    local width = math.max(3, math.floor(200 * sliderScale))
    local x = p.contentX + cw - width
    for index, field in ipairs(fields) do
        local key, title = field[1], field[2]
        local y = p.contentY + (index - 1) * p.s.rowSpacing
        local labelId = p.trackLabel(label.new({
            name = "audio_" .. key .. "_label",
            text = title .. " " .. data.pendingAudio[key] .. "%",
            font = p.font, fontSize = p.baseSizes.fontSize, page = p.page,
            color = {1, 1, 1, 1}, uiscale = labelScale,
        }))
        local labelHandle = label.getElementHandle(labelId)
        UI.addToPage(p.page, labelHandle, p.contentX, y + p.s.fontSize)
        UI.setZIndex(labelHandle, p.zContent)
        local id = p.trackSlider(slider.new({
            name = "audio_" .. key, min = 0, max = 100, default = data.pendingAudio[key],
            width = width, height = math.max(1, math.floor(20 * sliderScale)),
            capWidth = math.max(1, math.ceil(8 * sliderScale)),
            knobWidth = math.max(1, math.floor(16 * sliderScale)), uiscale = 1,
            page = p.page, x = x, y = y, zIndex = p.zWidgets,
            onChange = function(value)
                data.previewAudio(key, value)
                label.setText(labelId, title .. " " .. data.pendingAudio[key] .. "%")
            end,
        }))
        rows[#rows + 1] = {
            labelHandle = labelHandle,
            widgetHandles = {slider.getElementHandle(id), slider.getKnobHandle(id)},
            widgetSetPosition = function(ry) slider.setPosition(id, x, ry) end,
            widgetSetVisible = function(visible) slider.setVisible(id, visible) end,
        }
    end
    return rows
end

return audioTab
