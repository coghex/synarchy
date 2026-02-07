-- Scrollbar UI component
local boxTextures = require("scripts.ui.box_textures")
local scale = require("scripts.ui.scale")
local scrollbar = {}

-----------------------------------------------------------
-- Module State
-----------------------------------------------------------

local scrollbars = {}
local nextId = 1

local texScrollUp = nil
local texScrollDown = nil
local texScrollBar = nil
local texScrollBarTop = nil
local texScrollBarBottom = nil
local texScrollTabSet = nil
local assetsLoaded = false

-----------------------------------------------------------
-- Initialization
-----------------------------------------------------------

function scrollbar.init()
    if assetsLoaded then return end
    
    texScrollUp = engine.loadTexture("assets/textures/ui/scrollup.png")
    texScrollDown = engine.loadTexture("assets/textures/ui/scrolldown.png")
    texScrollBar = engine.loadTexture("assets/textures/ui/scrollbar.png")
    texScrollBarTop = engine.loadTexture("assets/textures/ui/scrollbartop.png")
    texScrollBarBottom = engine.loadTexture("assets/textures/ui/scrollbarbottom.png")
    texScrollTabSet = boxTextures.load("assets/textures/ui/scrolltab", "scrolltab")
    
    assetsLoaded = true
    engine.logDebug("Scrollbar module initialized")
end

-----------------------------------------------------------
-- Creation / Destruction
-----------------------------------------------------------

function scrollbar.new(params)
    local id = nextId
    nextId = nextId + 1
    
    -- All sizes are expected to be PRE-SCALED by the caller
    local buttonSize = params.buttonSize or 24
    local trackWidth = buttonSize
    local trackHeight = params.trackHeight or 200
    local capHeight = params.capHeight or 4
    local tileSize = params.tileSize or 8
    local uiscale = params.uiscale or scale.get()
    
    local totalItems = params.totalItems or 1
    local visibleItems = params.visibleItems or 1
    
    local sb = {
        id = id,
        name = params.name or ("scrollbar_" .. id),
        x = params.x or 0,
        y = params.y or 0,
        page = params.page,
        buttonSize = buttonSize,
        trackWidth = trackWidth,
        trackHeight = trackHeight,
        capHeight = capHeight,
        tileSize = tileSize,
        uiscale = uiscale,
        totalItems = totalItems,
        visibleItems = visibleItems,
        scrollOffset = 0,
        -- Element handles
        upButtonId = nil,
        downButtonId = nil,
        trackTopId = nil,
        trackMidId = nil,
        trackBottomId = nil,
        tabId = nil,
        -- Computed layout
        totalHeight = 0,
        trackInnerHeight = 0,
        tabHeight = 0,
        tabMinHeight = math.floor(12 * uiscale),
        -- Callbacks
        onScroll = params.onScroll or nil,
    }
    
    sb.totalHeight = buttonSize + capHeight + trackHeight + capHeight + buttonSize
    sb.trackInnerHeight = trackHeight
    
    scrollbar.recalcTab(sb)
    
    local currentY = sb.y
    
    -- Scroll up button
    sb.upButtonId = UI.newSprite(
        sb.name .. "_up",
        buttonSize,
        buttonSize,
        texScrollUp,
        1.0, 1.0, 1.0, 1.0,
        sb.page
    )
    UI.addToPage(sb.page, sb.upButtonId, sb.x, currentY)
    UI.setClickable(sb.upButtonId, true)
    UI.setOnClick(sb.upButtonId, "onScrollUp")
    currentY = currentY + buttonSize
    
    -- Track top cap
    sb.trackTopId = UI.newSprite(
        sb.name .. "_tracktop",
        trackWidth,
        capHeight,
        texScrollBarTop,
        1.0, 1.0, 1.0, 1.0,
        sb.page
    )
    UI.addToPage(sb.page, sb.trackTopId, sb.x, currentY)
    currentY = currentY + capHeight
    
    -- Track middle (stretched)
    sb.trackMidId = UI.newSprite(
        sb.name .. "_trackmid",
        trackWidth,
        trackHeight,
        texScrollBar,
        1.0, 1.0, 1.0, 1.0,
        sb.page
    )
    UI.addToPage(sb.page, sb.trackMidId, sb.x, currentY)
    currentY = currentY + trackHeight
    
    -- Track bottom cap
    sb.trackBottomId = UI.newSprite(
        sb.name .. "_trackbot",
        trackWidth,
        capHeight,
        texScrollBarBottom,
        1.0, 1.0, 1.0, 1.0,
        sb.page
    )
    UI.addToPage(sb.page, sb.trackBottomId, sb.x, currentY)
    currentY = currentY + capHeight
    
    -- Scroll down button
    sb.downButtonId = UI.newSprite(
        sb.name .. "_down",
        buttonSize,
        buttonSize,
        texScrollDown,
        1.0, 1.0, 1.0, 1.0,
        sb.page
    )
    UI.addToPage(sb.page, sb.downButtonId, sb.x, currentY)
    UI.setClickable(sb.downButtonId, true)
    UI.setOnClick(sb.downButtonId, "onScrollDown")
    
    -- Scroll tab (9-tile box drawn on top of the track)
    sb.tabId = UI.newBox(
        sb.name .. "_tab",
        trackWidth,
        sb.tabHeight,
        texScrollTabSet,
        sb.tileSize,
        1.0, 1.0, 1.0, 1.0,
        sb.page
    )
    
    local tabY = scrollbar.getTabY(sb)
    UI.addToPage(sb.page, sb.tabId, sb.x, tabY)
    
    -- Z-index: track behind, buttons and tab in front
    UI.setZIndex(sb.upButtonId, 501)
    UI.setZIndex(sb.trackTopId, 500)
    UI.setZIndex(sb.trackMidId, 500)
    UI.setZIndex(sb.trackBottomId, 500)
    UI.setZIndex(sb.downButtonId, 501)
    UI.setZIndex(sb.tabId, 502)
    
    scrollbars[id] = sb
    
    engine.logDebug("Scrollbar created: " .. sb.name
        .. " totalHeight=" .. sb.totalHeight
        .. " trackHeight=" .. sb.trackHeight
        .. " tabHeight=" .. sb.tabHeight
        .. " items=" .. sb.totalItems .. "/" .. sb.visibleItems)
    
    return id
end

function scrollbar.destroy(id)
    local sb = scrollbars[id]
    if not sb then return end
    
    if sb.upButtonId then UI.deleteElement(sb.upButtonId) end
    if sb.trackTopId then UI.deleteElement(sb.trackTopId) end
    if sb.trackMidId then UI.deleteElement(sb.trackMidId) end
    if sb.trackBottomId then UI.deleteElement(sb.trackBottomId) end
    if sb.downButtonId then UI.deleteElement(sb.downButtonId) end
    if sb.tabId then UI.deleteElement(sb.tabId) end
    
    scrollbars[id] = nil
    engine.logDebug("Scrollbar destroyed: " .. (sb.name or "?"))
end

function scrollbar.destroyAll()
    for id, _ in pairs(scrollbars) do
        scrollbar.destroy(id)
    end
    scrollbars = {}
    nextId = 1
end

-----------------------------------------------------------
-- Tab Calculation
-----------------------------------------------------------

function scrollbar.recalcTab(sb)
    if sb.totalItems <= sb.visibleItems then
        sb.tabHeight = sb.trackInnerHeight
    else
        local ratio = sb.visibleItems / sb.totalItems
        sb.tabHeight = math.max(sb.tabMinHeight, math.floor(sb.trackInnerHeight * ratio))
    end
end

function scrollbar.getTabY(sb)
    local trackStartY = sb.y + sb.buttonSize + sb.capHeight
    
    if sb.totalItems <= sb.visibleItems then
        return trackStartY
    end
    
    local maxOffset = sb.totalItems - sb.visibleItems
    local scrollRatio = sb.scrollOffset / maxOffset
    local availableTravel = sb.trackInnerHeight - sb.tabHeight
    
    return trackStartY + math.floor(scrollRatio * availableTravel)
end

-----------------------------------------------------------
-- Scroll Control
-----------------------------------------------------------

function scrollbar.setScrollOffset(id, offset)
    local sb = scrollbars[id]
    if not sb then return end
    
    local maxOffset = math.max(0, sb.totalItems - sb.visibleItems)
    sb.scrollOffset = math.max(0, math.min(maxOffset, math.floor(offset)))
    
    local tabY = scrollbar.getTabY(sb)
    UI.setPosition(sb.tabId, sb.x, tabY)
    
    if sb.onScroll then
        sb.onScroll(sb.scrollOffset, id, sb.name)
    end
end

function scrollbar.scrollUp(id)
    local sb = scrollbars[id]
    if not sb then return end
    scrollbar.setScrollOffset(id, sb.scrollOffset - 1)
end

function scrollbar.scrollDown(id)
    local sb = scrollbars[id]
    if not sb then return end
    scrollbar.setScrollOffset(id, sb.scrollOffset + 1)
end

function scrollbar.getScrollOffset(id)
    local sb = scrollbars[id]
    if not sb then return 0 end
    return sb.scrollOffset
end

-----------------------------------------------------------
-- Update Content Size
-----------------------------------------------------------

function scrollbar.setContentSize(id, totalItems, visibleItems)
    local sb = scrollbars[id]
    if not sb then return end
    
    sb.totalItems = totalItems
    sb.visibleItems = visibleItems
    scrollbar.recalcTab(sb)
    
    local maxOffset = math.max(0, sb.totalItems - sb.visibleItems)
    if sb.scrollOffset > maxOffset then
        sb.scrollOffset = maxOffset
    end
    
    UI.setSize(sb.tabId, sb.trackWidth, sb.tabHeight)
    local tabY = scrollbar.getTabY(sb)
    UI.setPosition(sb.tabId, sb.x, tabY)
end

-----------------------------------------------------------
-- Position / Visibility
-----------------------------------------------------------

function scrollbar.setPosition(id, x, y)
    local sb = scrollbars[id]
    if not sb then return end
    
    sb.x = x
    sb.y = y
    
    local currentY = y
    
    UI.setPosition(sb.upButtonId, x, currentY)
    currentY = currentY + sb.buttonSize
    
    UI.setPosition(sb.trackTopId, x, currentY)
    currentY = currentY + sb.capHeight
    
    UI.setPosition(sb.trackMidId, x, currentY)
    currentY = currentY + sb.trackInnerHeight
    
    UI.setPosition(sb.trackBottomId, x, currentY)
    currentY = currentY + sb.capHeight
    
    UI.setPosition(sb.downButtonId, x, currentY)
    
    local tabY = scrollbar.getTabY(sb)
    UI.setPosition(sb.tabId, x, tabY)
end

function scrollbar.setVisible(id, visible)
    local sb = scrollbars[id]
    if not sb then return end
    
    UI.setVisible(sb.upButtonId, visible)
    UI.setVisible(sb.trackTopId, visible)
    UI.setVisible(sb.trackMidId, visible)
    UI.setVisible(sb.trackBottomId, visible)
    UI.setVisible(sb.downButtonId, visible)
    UI.setVisible(sb.tabId, visible)
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function scrollbar.getSize(id)
    local sb = scrollbars[id]
    if not sb then return 0, 0 end
    return sb.trackWidth, sb.totalHeight
end

function scrollbar.getTotalHeight(id)
    local sb = scrollbars[id]
    if not sb then return 0 end
    return sb.totalHeight
end

function scrollbar.getTrackWidth(id)
    local sb = scrollbars[id]
    if not sb then return 0 end
    return sb.trackWidth
end

function scrollbar.findByElementHandle(elemHandle)
    for id, sb in pairs(scrollbars) do
        if sb.upButtonId == elemHandle then
            return id, "up"
        elseif sb.downButtonId == elemHandle then
            return id, "down"
        elseif sb.tabId == elemHandle then
            return id, "tab"
        elseif sb.trackMidId == elemHandle then
            return id, "track"
        end
    end
    return nil, nil
end

function scrollbar.isScrollbarCallback(callbackName)
    return callbackName == "onScrollUp" or callbackName == "onScrollDown"
end

function scrollbar.setZIndices(id, trackZ, buttonZ, tabZ)
    local sb = scrollbars[id]
    if not sb then return end
    
    UI.setZIndex(sb.trackTopId, trackZ)
    UI.setZIndex(sb.trackMidId, trackZ)
    UI.setZIndex(sb.trackBottomId, trackZ)
    UI.setZIndex(sb.upButtonId, buttonZ)
    UI.setZIndex(sb.downButtonId, buttonZ)
    UI.setZIndex(sb.tabId, tabZ)
end

return scrollbar
