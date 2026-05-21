-- Label UI component
local scale = require("scripts.ui.scale")
local label = {}

-----------------------------------------------------------
-- Module State
-----------------------------------------------------------

local labels = {}
local nextId = 1

-----------------------------------------------------------
-- Creation / Destruction
-----------------------------------------------------------

function label.new(params)
    local id = nextId
    nextId = nextId + 1

    local uiscale = params.uiscale or scale.get()
    local fontSize = math.floor((params.fontSize or 24) * uiscale)

    local color = params.color or {1.0, 1.0, 1.0, 1.0}
    local text = params.text or ""
    local font = params.font

    local lbl = {
        id = id,
        name = params.name or ("label_" .. id),
        text = text,
        font = font,
        fontSize = fontSize,
        baseFontSize = params.fontSize or 24,
        color = color,
        page = params.page,
        parent = params.parent,
        textId = nil,
        hitBoxId = nil,   -- only created when a tooltip is attached
        width = 0,
        height = fontSize,
        uiscale = uiscale,
    }

    if font and text ~= "" then
        lbl.width = engine.getTextWidth(font, text, fontSize)
    end

    lbl.textId = UI.newText(
        lbl.name .. "_text",
        text,
        font,
        fontSize,
        color[1], color[2], color[3], color[4],
        lbl.page
    )

    -- Optional tooltip. Text elements use uePosition as the *baseline*
    -- and have a zero-sized bounding box, so they're invisible to
    -- hover hit-testing. When a tooltip is requested, we create a
    -- transparent hit-box child of the text positioned at (0, -fontSize)
    -- so it covers the visible glyph mass (which sits ABOVE the
    -- baseline). The tooltip is attached to the hit-box, not the
    -- text element.
    if params.tooltip or params.tooltipRich then
        local hitH = math.floor(fontSize * 1.25)
        lbl.hitBoxId = UI.newElement(
            lbl.name .. "_hit",
            lbl.width, hitH,
            lbl.page
        )
        UI.addChild(lbl.textId, lbl.hitBoxId, 0, -fontSize)
        if params.tooltipRich then
            UI.setTooltipRich(lbl.hitBoxId, params.tooltipRich)
        else
            UI.setTooltip(lbl.hitBoxId, params.tooltip)
        end
    end

    if params.x and params.y and not params.parent then
        UI.addToPage(lbl.page, lbl.textId, params.x, params.y)
    elseif params.x and params.y and params.parent then
        UI.addChild(params.parent, lbl.textId, params.x, params.y)
    end

    if params.zIndex then
        UI.setZIndex(lbl.textId, params.zIndex)
    end

    labels[id] = lbl

    return id
end

function label.destroy(id)
    local lbl = labels[id]
    if not lbl then return end

    if lbl.hitBoxId then
        UI.deleteElement(lbl.hitBoxId)
    end
    if lbl.textId then
        UI.deleteElement(lbl.textId)
    end

    labels[id] = nil
end

function label.destroyAll()
    labels = {}
    nextId = 1
end

-----------------------------------------------------------
-- Text Management
-----------------------------------------------------------

function label.setText(id, text)
    local lbl = labels[id]
    if not lbl then return end

    lbl.text = text

    if lbl.font and text ~= "" then
        lbl.width = engine.getTextWidth(lbl.font, text, lbl.fontSize)
    else
        lbl.width = 0
    end

    if lbl.textId then
        UI.setText(lbl.textId, text)
    end

    -- Keep the tooltip hit-box width in sync with the new text width.
    if lbl.hitBoxId then
        local hitH = math.floor(lbl.fontSize * 1.25)
        UI.setSize(lbl.hitBoxId, lbl.width, hitH)
    end
end

function label.getText(id)
    local lbl = labels[id]
    if not lbl then return "" end
    return lbl.text
end

-- Attach (or update) a rich tooltip on the label's hit-box. The hit-
-- box is created on first call if it doesn't exist yet; subsequent
-- calls just update the tooltip content. Used by per-tick refresh
-- closures that want to keep the modifier-source list live without
-- destroying / recreating the label.
function label.setTooltipRich(id, content)
    local lbl = labels[id]
    if not lbl then return end
    if not lbl.hitBoxId then
        local hitH = math.floor(lbl.fontSize * 1.25)
        lbl.hitBoxId = UI.newElement(
            lbl.name .. "_hit",
            lbl.width, hitH,
            lbl.page
        )
        UI.addChild(lbl.textId, lbl.hitBoxId, 0, -lbl.fontSize)
    end
    UI.setTooltipRich(lbl.hitBoxId, content)
end

function label.setColor(id, color)
    local lbl = labels[id]
    if not lbl then return end
    
    lbl.color = color
    
    if lbl.textId then
        UI.setColor(lbl.textId, color[1], color[2], color[3], color[4])
    end
end

function label.setPosition(id, x, y)
    local lbl = labels[id]
    if not lbl then return end
    if lbl.textId then UI.setPosition(lbl.textId, x, y) end
end

function label.setVisible(id, visible)
    local lb = labels[id]
    if not lb then return end
    
    if lb.textId then UI.setVisible(lb.textId, visible) end
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function label.getSize(id)
    local lbl = labels[id]
    if not lbl then return 0, 0 end
    return lbl.width, lbl.height
end

function label.getElementHandle(id)
    local lbl = labels[id]
    if not lbl then return nil end
    return lbl.textId
end

function label.getName(id)
    local lbl = labels[id]
    if not lbl then return nil end
    return lbl.name
end

function label.getFont(id)
    local lbl = labels[id]
    if not lbl then return nil end
    return lbl.font
end

function label.getFontSize(id)
    local lbl = labels[id]
    if not lbl then return 0 end
    return lbl.fontSize
end

-----------------------------------------------------------
-- Targeted Destruction
-----------------------------------------------------------

-- Destroy a list of label IDs (for module-scoped cleanup)
function label.destroyList(ids)
    if not ids then return end
    for _, id in ipairs(ids) do
        label.destroy(id)
    end
end

return label
