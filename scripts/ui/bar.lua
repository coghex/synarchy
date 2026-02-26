-- Progress Bar UI widget
-- Renders a track (left cap + center + right cap) with a fill
-- (left cap + stretchable center) that grows based on progress.
-- Optionally displays a short text label centered inside the bar.
local scale = require("scripts.ui.scale")
local bar = {}

-----------------------------------------------------------
-- Module State
-----------------------------------------------------------

local bars = {}
local nextId = 1

-----------------------------------------------------------
-- Creation / Destruction
-----------------------------------------------------------

-- params:
--   name, page, x, y, width, height
--   trackLeftTex, trackCenterTex, trackRightTex
--   fillLeftTex, fillCenterTex
--   capWidth        -- pixel width of each cap (unscaled)
--   color           -- {r,g,b,a} tint for the track
--   fillColor       -- {r,g,b,a} tint for the fill
--   uiscale         -- (optional, default from engine)
--   zIndex          -- base z for track; fill is zIndex+1, text is zIndex+2
--   progress        -- initial 0..1 (default 0)
--   font            -- (optional) font handle for overlay text
--   fontSize        -- (optional) unscaled font size for overlay text
--   textColor       -- (optional) {r,g,b,a} for overlay text (default red)

function bar.new(params)
    local id = nextId
    nextId = nextId + 1

    local uiscale  = params.uiscale or scale.get()
    local width    = math.floor((params.width or 300) * uiscale)
    local height   = math.floor((params.height or 24) * uiscale)
    local capW     = math.floor((params.capWidth or 32) * uiscale)
    local x        = params.x or 0
    local y        = params.y or 0
    local page     = params.page
    local zBase    = params.zIndex or 1

    local color     = params.color     or {1.0, 1.0, 1.0, 1.0}
    local fillColor = params.fillColor or {0.2, 0.7, 0.3, 1.0}

    local innerW = width - capW * 2
    if innerW < 1 then innerW = 1 end

    local fontSize  = params.fontSize and math.floor(params.fontSize * uiscale) or nil
    local textColor = params.textColor or {1.0, 0.2, 0.2, 1.0}

    local b = {
        id = id,
        name = params.name or ("bar_" .. id),
        x = x, y = y,
        width = width,
        height = height,
        capW = capW,
        innerW = innerW,
        page = page,
        uiscale = uiscale,
        zBase = zBase,
        progress = params.progress or 0,
        -- Text overlay
        font = params.font,
        fontSize = fontSize,
        textColor = textColor,
        overlayText = "",
        -- Element handles
        trackLeftId   = nil,
        trackCenterId = nil,
        trackRightId  = nil,
        fillLeftId    = nil,
        fillCenterId  = nil,
        textId        = nil,
    }

    -- Track: left cap
    b.trackLeftId = UI.newSprite(
        b.name .. "_tl", capW, height,
        params.trackLeftTex,
        color[1], color[2], color[3], color[4],
        page
    )
    UI.addToPage(page, b.trackLeftId, x, y)
    UI.setZIndex(b.trackLeftId, zBase)

    -- Track: center (stretched)
    b.trackCenterId = UI.newSprite(
        b.name .. "_tc", innerW, height,
        params.trackCenterTex,
        color[1], color[2], color[3], color[4],
        page
    )
    UI.addToPage(page, b.trackCenterId, x + capW, y)
    UI.setZIndex(b.trackCenterId, zBase)

    -- Track: right cap
    b.trackRightId = UI.newSprite(
        b.name .. "_tr", capW, height,
        params.trackRightTex,
        color[1], color[2], color[3], color[4],
        page
    )
    UI.addToPage(page, b.trackRightId, x + capW + innerW, y)
    UI.setZIndex(b.trackRightId, zBase)

    -- Fill: left cap
    b.fillLeftId = UI.newSprite(
        b.name .. "_fl", capW, height,
        params.fillLeftTex,
        fillColor[1], fillColor[2], fillColor[3], fillColor[4],
        page
    )
    UI.addToPage(page, b.fillLeftId, x, y)
    UI.setZIndex(b.fillLeftId, zBase + 1)

    -- Fill: center (stretchable)
    b.fillCenterId = UI.newSprite(
        b.name .. "_fc", 1, height,
        params.fillCenterTex,
        fillColor[1], fillColor[2], fillColor[3], fillColor[4],
        page
    )
    UI.addToPage(page, b.fillCenterId, x + capW, y)
    UI.setZIndex(b.fillCenterId, zBase + 1)

    -- Overlay text (created only if font was provided)
    if b.font and b.fontSize then
        b.textId = UI.newText(
            b.name .. "_text",
            "",
            b.font,
            b.fontSize,
            b.textColor[1], b.textColor[2], b.textColor[3], b.textColor[4],
            page
        )
        UI.addToPage(page, b.textId, x, y)
        UI.setZIndex(b.textId, zBase + 2)
        UI.setVisible(b.textId, false)
    end

    -- Apply initial progress
    bar.applyProgress(b)

    bars[id] = b

    engine.logDebug("Bar created: " .. b.name
        .. " width=" .. width .. " capW=" .. capW
        .. " innerW=" .. innerW)

    return id
end

function bar.destroy(id)
    local b = bars[id]
    if not b then return end

    if b.trackLeftId   then UI.deleteElement(b.trackLeftId) end
    if b.trackCenterId then UI.deleteElement(b.trackCenterId) end
    if b.trackRightId  then UI.deleteElement(b.trackRightId) end
    if b.fillLeftId    then UI.deleteElement(b.fillLeftId) end
    if b.fillCenterId  then UI.deleteElement(b.fillCenterId) end
    if b.textId        then UI.deleteElement(b.textId) end

    bars[id] = nil
    engine.logDebug("Bar destroyed: " .. (b.name or "?"))
end

function bar.destroyAll()
    for id, _ in pairs(bars) do
        bar.destroy(id)
    end
    bars = {}
    nextId = 1
end

-----------------------------------------------------------
-- Progress
-----------------------------------------------------------

function bar.applyProgress(b)
    local p = math.max(0, math.min(1, b.progress))
    local fillInnerW = math.floor(b.innerW * p)

    if p < 0.001 then
        UI.setVisible(b.fillLeftId, false)
        UI.setVisible(b.fillCenterId, false)
    else
        UI.setVisible(b.fillLeftId, true)
        UI.setVisible(b.fillCenterId, true)
        UI.setSize(b.fillCenterId, math.max(1, fillInnerW), b.height)
    end

    -- Re-center overlay text
    bar.repositionText(b)
end

function bar.setProgress(id, progress)
    local b = bars[id]
    if not b then return end

    b.progress = progress
    bar.applyProgress(b)
end

function bar.getProgress(id)
    local b = bars[id]
    if not b then return 0 end
    return b.progress
end

-----------------------------------------------------------
-- Overlay Text
-----------------------------------------------------------

function bar.repositionText(b)
    if not b.textId then return end
    if b.overlayText == "" then
        UI.setVisible(b.textId, false)
        return
    end

    UI.setVisible(b.textId, true)

    local textW = engine.getTextWidth(b.font, b.overlayText, b.fontSize)
    local textX = b.x + math.floor((b.width - textW) / 2)
    local textY = b.y + math.floor(b.height / 2) + math.floor(b.fontSize / 3)
    UI.setPosition(b.textId, textX, textY)
end

function bar.setText(id, text)
    local b = bars[id]
    if not b then return end
    if not b.textId then return end

    b.overlayText = text or ""
    UI.setText(b.textId, b.overlayText)
    bar.repositionText(b)
end

function bar.setTextColor(id, r, g, b_val, a)
    local b = bars[id]
    if not b or not b.textId then return end
    UI.setColor(b.textId, r, g, b_val, a)
end

-----------------------------------------------------------
-- Position / Visibility
-----------------------------------------------------------

function bar.setPosition(id, x, y)
    local b = bars[id]
    if not b then return end

    b.x = x
    b.y = y

    UI.setPosition(b.trackLeftId, x, y)
    UI.setPosition(b.trackCenterId, x + b.capW, y)
    UI.setPosition(b.trackRightId, x + b.capW + b.innerW, y)
    UI.setPosition(b.fillLeftId, x, y)
    UI.setPosition(b.fillCenterId, x + b.capW, y)

    bar.repositionText(b)
end

function bar.setVisible(id, visible)
    local b = bars[id]
    if not b then return end

    UI.setVisible(b.trackLeftId, visible)
    UI.setVisible(b.trackCenterId, visible)
    UI.setVisible(b.trackRightId, visible)

    if visible then
        bar.applyProgress(b)
    else
        UI.setVisible(b.fillLeftId, false)
        UI.setVisible(b.fillCenterId, false)
        if b.textId then UI.setVisible(b.textId, false) end
    end
end

-----------------------------------------------------------
-- Fill Color
-----------------------------------------------------------

function bar.setFillColor(id, r, g, b_val, a)
    local b = bars[id]
    if not b then return end

    UI.setSpriteColor(b.fillLeftId, r, g, b_val, a)
    UI.setSpriteColor(b.fillCenterId, r, g, b_val, a)
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function bar.getSize(id)
    local b = bars[id]
    if not b then return 0, 0 end
    return b.width, b.height
end

function bar.getElementHandle(id)
    local b = bars[id]
    if not b then return nil end
    return b.trackCenterId
end

return bar
