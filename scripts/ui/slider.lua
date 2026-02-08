-- Slider UI component
-- Horizontal slider with left cap, stretchable track, right cap, and draggable knob.
-- Integer-only values; supports click-on-track to jump.
local scale = require("scripts.ui.scale")
local slider = {}

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------
local TRACK_CALLBACK = "onSliderTrackClick"
local KNOB_CALLBACK  = "onSliderKnobClick"

-----------------------------------------------------------
-- Module State
-----------------------------------------------------------

local sliders = {}
local nextId = 1

local texLeft  = nil
local texTrack = nil
local texRight = nil
local texKnob  = nil
local assetsLoaded = false

-- Drag state (only one slider can be dragged at a time)
local draggingId = nil
local dragOffsetX = 0  -- offset from knob center to mouse X at grab time

-----------------------------------------------------------
-- Initialization
-----------------------------------------------------------

function slider.init()
    if assetsLoaded then return end

    texLeft  = engine.loadTexture("assets/textures/ui/slider/sliderleft.png")
    texTrack = engine.loadTexture("assets/textures/ui/slider/slidertrack.png")
    texRight = engine.loadTexture("assets/textures/ui/slider/sliderright.png")
    texKnob  = engine.loadTexture("assets/textures/ui/slider/sliderknob.png")

    assetsLoaded = true
    engine.logDebug("Slider module initialized")
end

-----------------------------------------------------------
-- Internal Helpers
-----------------------------------------------------------

-- Compute the X position of the knob center given the current value
local function knobCenterX(sl)
    local ratio = (sl.value - sl.min) / (sl.max - sl.min)
    return sl.trackX + ratio * sl.trackWidth
end

-- Compute value from a pixel X position (clamped and snapped)
local function valueFromX(sl, px)
    local ratio = (px - sl.trackX) / sl.trackWidth
    ratio = math.max(0, math.min(1, ratio))
    local raw = sl.min + ratio * (sl.max - sl.min)
    -- Snap to step (integer)
    local stepped = math.floor(raw + 0.5)
    return math.max(sl.min, math.min(sl.max, stepped))
end

-- Reposition all sprites based on sl.x, sl.y and current value
local function layoutSprites(sl)
    local capW = sl.capWidth
    local h    = sl.height

    -- Left cap
    UI.setPosition(sl.leftCapId, sl.x, sl.y)
    UI.setSize(sl.leftCapId, capW, h)

    -- Track (stretchable middle)
    UI.setPosition(sl.trackSpriteId, sl.x + capW, sl.y)
    UI.setSize(sl.trackSpriteId, sl.trackWidth, h)

    -- Right cap
    UI.setPosition(sl.rightCapId, sl.x + capW + sl.trackWidth, sl.y)
    UI.setSize(sl.rightCapId, capW, h)

    -- Knob
    local cx = knobCenterX(sl)
    local knobX = cx - sl.knobWidth / 2
    UI.setPosition(sl.knobSpriteId, knobX, sl.y)
    UI.setSize(sl.knobSpriteId, sl.knobWidth, h)
end

-----------------------------------------------------------
-- Creation / Destruction
-----------------------------------------------------------

function slider.new(params)
    local id = nextId
    nextId = nextId + 1

    local uiscale  = params.uiscale or scale.get()
    local width    = math.floor((params.width or 200) * uiscale)
    local height   = math.floor((params.height or 24) * uiscale)
    local capWidth = math.floor((params.capWidth or 8) * uiscale)
    local knobWidth = math.floor((params.knobWidth or 16) * uiscale)

    local trackWidth = width - capWidth * 2  -- stretchable region

    local sl = {
        id         = id,
        name       = params.name or ("slider_" .. id),
        x          = params.x or 0,
        y          = params.y or 0,
        width      = width,
        height     = height,
        capWidth   = capWidth,
        knobWidth  = knobWidth,
        trackWidth = trackWidth,
        -- trackX = absolute X where the track starts (for value mapping)
        trackX     = (params.x or 0) + capWidth,
        page       = params.page,
        min        = params.min or 0,
        max        = params.max or 100,
        value      = params.default or params.min or 0,
        onChange    = params.onChange,
        zIndex     = params.zIndex,
        -- Element handles (filled below)
        leftCapId     = nil,
        trackSpriteId = nil,
        rightCapId    = nil,
        knobSpriteId  = nil,
    }

    -- Clamp default
    sl.value = math.max(sl.min, math.min(sl.max, math.floor(sl.value + 0.5)))

    -- Left cap
    sl.leftCapId = UI.newSprite(
        sl.name .. "_left",
        capWidth, height,
        texLeft,
        1.0, 1.0, 1.0, 1.0,
        sl.page
    )
    UI.addToPage(sl.page, sl.leftCapId, sl.x, sl.y)

    -- Track (clickable)
    sl.trackSpriteId = UI.newSprite(
        sl.name .. "_track",
        trackWidth, height,
        texTrack,
        1.0, 1.0, 1.0, 1.0,
        sl.page
    )
    UI.addToPage(sl.page, sl.trackSpriteId, sl.x + capWidth, sl.y)
    UI.setClickable(sl.trackSpriteId, true)
    UI.setOnClick(sl.trackSpriteId, TRACK_CALLBACK)

    -- Right cap
    sl.rightCapId = UI.newSprite(
        sl.name .. "_right",
        capWidth, height,
        texRight,
        1.0, 1.0, 1.0, 1.0,
        sl.page
    )
    UI.addToPage(sl.page, sl.rightCapId,
        sl.x + capWidth + trackWidth, sl.y)

    -- Knob (clickable, drawn on top)
    sl.knobSpriteId = UI.newSprite(
        sl.name .. "_knob",
        knobWidth, height,
        texKnob,
        1.0, 1.0, 1.0, 1.0,
        sl.page
    )
    local cx = knobCenterX(sl)
    UI.addToPage(sl.page, sl.knobSpriteId, cx - knobWidth / 2, sl.y)
    UI.setClickable(sl.knobSpriteId, true)
    UI.setOnClick(sl.knobSpriteId, KNOB_CALLBACK)

    -- Z-indices: track parts share one layer, knob sits above
    if sl.zIndex then
        UI.setZIndex(sl.leftCapId,     sl.zIndex)
        UI.setZIndex(sl.trackSpriteId, sl.zIndex)
        UI.setZIndex(sl.rightCapId,    sl.zIndex)
        UI.setZIndex(sl.knobSpriteId,  sl.zIndex + 1)
    end

    sliders[id] = sl

    engine.logDebug("Slider created: " .. sl.name
        .. " range=[" .. sl.min .. ".." .. sl.max .. "]"
        .. " value=" .. sl.value
        .. " trackWidth=" .. sl.trackWidth)

    return id
end

function slider.destroy(id)
    local sl = sliders[id]
    if not sl then return end

    if draggingId == id then
        draggingId = nil
    end

    if sl.leftCapId     then UI.deleteElement(sl.leftCapId) end
    if sl.trackSpriteId then UI.deleteElement(sl.trackSpriteId) end
    if sl.rightCapId    then UI.deleteElement(sl.rightCapId) end
    if sl.knobSpriteId  then UI.deleteElement(sl.knobSpriteId) end

    sliders[id] = nil
    engine.logDebug("Slider destroyed: " .. (sl.name or "?"))
end

function slider.destroyAll()
    for id, _ in pairs(sliders) do
        slider.destroy(id)
    end
    sliders = {}
    nextId = 1
    draggingId = nil
end

-----------------------------------------------------------
-- Value Access
-----------------------------------------------------------

function slider.getValue(id)
    local sl = sliders[id]
    if not sl then return 0 end
    return sl.value
end

function slider.setValue(id, val)
    local sl = sliders[id]
    if not sl then return end

    val = math.max(sl.min, math.min(sl.max, math.floor(val + 0.5)))
    if val == sl.value then return end

    sl.value = val
    layoutSprites(sl)

    if sl.onChange then
        sl.onChange(sl.value, id, sl.name)
    end
end

-----------------------------------------------------------
-- Position / Visibility
-----------------------------------------------------------

function slider.setPosition(id, x, y)
    local sl = sliders[id]
    if not sl then return end

    sl.x = x
    sl.y = y
    sl.trackX = x + sl.capWidth

    layoutSprites(sl)
end

function slider.setVisible(id, visible)
    local sl = sliders[id]
    if not sl then return end

    UI.setVisible(sl.leftCapId, visible)
    UI.setVisible(sl.trackSpriteId, visible)
    UI.setVisible(sl.rightCapId, visible)
    UI.setVisible(sl.knobSpriteId, visible)
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function slider.getSize(id)
    local sl = sliders[id]
    if not sl then return 0, 0 end
    return sl.width, sl.height
end

function slider.getElementHandle(id)
    -- Return the track as the "main" handle (for scroll row tracking)
    local sl = sliders[id]
    if not sl then return nil end
    return sl.trackSpriteId
end

function slider.getKnobHandle(id)
    local sl = sliders[id]
    if not sl then return nil end
    return sl.knobSpriteId
end

function slider.getDraggingId()
    return draggingId
end

-----------------------------------------------------------
-- Element Lookup
-----------------------------------------------------------

function slider.findByElementHandle(elemHandle)
    for id, sl in pairs(sliders) do
        if sl.trackSpriteId == elemHandle
            or sl.knobSpriteId == elemHandle
            or sl.leftCapId == elemHandle
            or sl.rightCapId == elemHandle then
            return id
        end
    end
    return nil
end

function slider.isSliderCallback(callbackName)
    return callbackName == TRACK_CALLBACK
        or callbackName == KNOB_CALLBACK
end

-----------------------------------------------------------
-- Click Handling (called from uiManager)
-----------------------------------------------------------

function slider.handleCallback(callbackName, elemHandle)
    if callbackName == KNOB_CALLBACK then
        return slider.onKnobClick(elemHandle)
    elseif callbackName == TRACK_CALLBACK then
        return slider.onTrackClick(elemHandle)
    end
    return false
end

function slider.onKnobClick(elemHandle)
    for id, sl in pairs(sliders) do
        if sl.knobSpriteId == elemHandle then
            draggingId = id
            -- Compute offset: mouse X - knob center X
            -- We don't have mouse coords here, so set offset to 0
            -- (the knob will snap to center-under-mouse on first move)
            dragOffsetX = 0
            return true
        end
    end
    return false
end

function slider.onTrackClick(elemHandle)
    -- Find which slider owns this track, jump value to click position,
    -- then start dragging
    for id, sl in pairs(sliders) do
        if sl.trackSpriteId == elemHandle then
            -- We need mouse position to compute the value.
            -- The actual position update will happen on the next
            -- onDragMove call. Start dragging immediately so
            -- the user can keep holding.
            draggingId = id
            dragOffsetX = 0
            return true
        end
    end
    return false
end

-----------------------------------------------------------
-- Drag Handling (called from uiManager.update)
-----------------------------------------------------------

-- Called every frame while draggingId is set.
-- mx, my are in framebuffer coordinates (already scaled).
function slider.onDragMove(mx, my)
    if not draggingId then return end
    local sl = sliders[draggingId]
    if not sl then
        draggingId = nil
        return
    end

    local newValue = valueFromX(sl, mx - dragOffsetX)
    if newValue ~= sl.value then
        sl.value = newValue
        layoutSprites(sl)
        if sl.onChange then
            sl.onChange(sl.value, sl.id, sl.name)
        end
    end
end

-- Called on any mouse-up; clears drag state
function slider.onMouseUp()
    draggingId = nil
end

-----------------------------------------------------------
-- Hover (visual feedback — optional, stub for now)
-----------------------------------------------------------

function slider.onHoverEnter(elemHandle)
    -- Could swap knob texture to a highlighted version
end

function slider.onHoverLeave(elemHandle)
    -- Revert knob texture
end

return slider
