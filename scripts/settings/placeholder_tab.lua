-- Placeholder Tab
-- Reusable for any tab that just shows "coming soon..." text.
local label = require("scripts.ui.label")

local placeholderTab = {}

-- params = { name, text, page, font, baseSizes, uiscale, s,
--            contentX, contentY, zContent }
-- Returns: rowHandles[]
function placeholderTab.create(params)
    local labelId = label.new({
        name     = params.name,
        text     = params.text,
        font     = params.font,
        fontSize = params.baseSizes.fontSize,
        color    = {0.7, 0.7, 0.7, 1.0},
        page     = params.page,
        uiscale  = params.uiscale,
    })
    local handle = label.getElementHandle(labelId)
    UI.addToPage(params.page, handle,
        params.contentX, params.contentY + params.s.fontSize)
    UI.setZIndex(handle, params.zContent)

    return {
        {
            labelHandle       = handle,
            widgetHandles     = {},
            widgetSetPosition = nil,
            widgetSetVisible  = nil,
        },
    }
end

return placeholderTab
