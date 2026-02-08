-- Tab Bar UI component
local boxTextures = require("scripts.ui.box_textures")
local scale = require("scripts.ui.scale")
local tabbar = {}

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------
local TAB_CALLBACK = "onTabClick"

-----------------------------------------------------------
-- Module State
-----------------------------------------------------------

local tabbars = {}
local nextId = 1

local texSetSelected = nil
local texSetUnselected = nil
local texSetFrame = nil
local assetsLoaded = false

-----------------------------------------------------------
-- Initialization
-----------------------------------------------------------

function tabbar.init()
    if assetsLoaded then return end
    
    texSetSelected = boxTextures.load("assets/textures/ui/tabselected", "tabselected")
    texSetUnselected = boxTextures.load("assets/textures/ui/tabunselected", "tabunselected")
    texSetFrame = boxTextures.load("assets/textures/ui/tabframe", "tabframe")
    
    assetsLoaded = true
    engine.logDebug("TabBar module initialized")
end

-----------------------------------------------------------
-- Creation / Destruction
-----------------------------------------------------------

function tabbar.new(params)
    local id = nextId
    nextId = nextId + 1
    
    local uiscale = params.uiscale or scale.get()
    local tabHeight = math.floor((params.tabHeight or 40) * uiscale)
    local fontSize = math.floor((params.fontSize or 24) * uiscale)
    local tileSize = math.floor((params.tileSize or 16) * uiscale)
    local tabPadding = math.floor((params.tabPadding or 20) * uiscale)
    local textPadding = math.floor((params.textPadding or 10) * uiscale)
    
    local textColor = params.textColor or {0.0, 0.0, 0.0, 1.0}
    local selectedTextColor = params.selectedTextColor or {1.0, 1.0, 1.0, 1.0}
    
    local tabs = params.tabs or {}
    
    local tb = {
        id = id,
        name = params.name or ("tabbar_" .. id),
        x = params.x or 0,
        y = params.y or 0,
        width = params.width or 400,
        height = tabHeight,
        fontSize = fontSize,
        tileSize = tileSize,
        tabPadding = tabPadding,
        textPadding = textPadding,
        page = params.page,
        font = params.font,
        uiscale = uiscale,
        textColor = textColor,
        selectedTextColor = selectedTextColor,
        tabs = {},
        selectedIndex = 1,
        frameBoxId = nil,
        frameX = params.x or 0,
        frameY = (params.y or 0) + tabHeight,
        frameWidth = params.width or 400,
        frameHeight = params.frameHeight or 300,
        frameTileSize = tileSize,
        onChange = params.onChange or nil,
        zIndex = params.zIndex or 10,
    }
    
    -- Measure tab widths
    local totalTabWidth = 0
    for i, tabDef in ipairs(tabs) do
        local textWidth = engine.getTextWidth(params.font, tabDef.name, fontSize)
        local tabWidth = textWidth + (textPadding * 2)
        table.insert(tb.tabs, {
            name = tabDef.name,
            key = tabDef.key or tabDef.name:lower(),
            width = tabWidth,
            boxId = nil,
            textId = nil,
        })
        totalTabWidth = totalTabWidth + tabWidth
    end
    
    -- Create the content frame
    tb.frameBoxId = UI.newBox(
        tb.name .. "_frame",
        tb.frameWidth,
        tb.frameHeight,
        texSetFrame,
        tb.frameTileSize,
        1.0, 1.0, 1.0, 1.0,
        0,
        tb.page
    )
    UI.addToPage(tb.page, tb.frameBoxId, tb.frameX, tb.frameY)
    UI.setZIndex(tb.frameBoxId, tb.zIndex)
    
    -- Create tab boxes, laid out left to right
    local currentX = tb.x
    for i, tab in ipairs(tb.tabs) do
        local isSelected = (i == tb.selectedIndex)
        local texSet = isSelected and texSetSelected or texSetUnselected
        
        tab.boxId = UI.newBox(
            tb.name .. "_tab_" .. i,
            tab.width,
            tabHeight,
            texSet,
            tileSize,
            1.0, 1.0, 1.0, 1.0,
            0,
            tb.page
        )
        UI.addToPage(tb.page, tab.boxId, currentX, tb.y)
        UI.setClickable(tab.boxId, true)
        UI.setOnClick(tab.boxId, TAB_CALLBACK)
        UI.setZIndex(tab.boxId, tb.zIndex + 1)
        
        local txtColor = isSelected and selectedTextColor or textColor
        local textY = (tabHeight / 2) + (fontSize / 3)
        tab.textId = UI.newText(
            tb.name .. "_tab_text_" .. i,
            tab.name,
            tb.font,
            fontSize,
            txtColor[1], txtColor[2], txtColor[3], txtColor[4],
            tb.page
        )
        UI.addChild(tab.boxId, tab.textId, textPadding, textY)
        UI.setZIndex(tab.textId, 1)
        
        currentX = currentX + tab.width
    end
    
    tabbars[id] = tb
    
    engine.logDebug("TabBar created: " .. tb.name .. " with " .. #tb.tabs .. " tabs")
    
    return id
end

function tabbar.destroy(id)
    local tb = tabbars[id]
    if not tb then return end
    
    if tb.frameBoxId then
        UI.deleteElement(tb.frameBoxId)
    end
    for _, tab in ipairs(tb.tabs) do
        if tab.boxId then
            UI.deleteElement(tab.boxId)
        end
    end
    
    tabbars[id] = nil
    engine.logDebug("TabBar destroyed: " .. (tb.name or "?"))
end

function tabbar.destroyAll()
    for id, _ in pairs(tabbars) do
        tabbar.destroy(id)
    end
    tabbars = {}
    nextId = 1
end

-----------------------------------------------------------
-- Tab Selection
-----------------------------------------------------------

function tabbar.select(id, index)
    local tb = tabbars[id]
    if not tb then return end
    if index < 1 or index > #tb.tabs then return end
    if tb.selectedIndex == index then return end
    
    -- Deselect old tab
    local oldTab = tb.tabs[tb.selectedIndex]
    if oldTab and oldTab.boxId then
        UI.setBoxTextures(oldTab.boxId, texSetUnselected)
        UI.setColor(oldTab.textId,
            tb.textColor[1], tb.textColor[2],
            tb.textColor[3], tb.textColor[4])
    end
    
    -- Select new tab
    tb.selectedIndex = index
    local newTab = tb.tabs[index]
    if newTab and newTab.boxId then
        UI.setBoxTextures(newTab.boxId, texSetSelected)
        UI.setColor(newTab.textId,
            tb.selectedTextColor[1], tb.selectedTextColor[2],
            tb.selectedTextColor[3], tb.selectedTextColor[4])
    end
    
    if tb.onChange then
        tb.onChange(newTab.key, index, id)
    end
    
    engine.logDebug("TabBar selected: " .. tb.name .. " -> " .. newTab.name)
end

function tabbar.selectByKey(id, key)
    local tb = tabbars[id]
    if not tb then return end
    
    for i, tab in ipairs(tb.tabs) do
        if tab.key == key then
            tabbar.select(id, i)
            return
        end
    end
end

-----------------------------------------------------------
-- Click Handling
-----------------------------------------------------------

function tabbar.findByElementHandle(elemHandle)
    for id, tb in pairs(tabbars) do
        for i, tab in ipairs(tb.tabs) do
            if tab.boxId == elemHandle then
                return id, i
            end
        end
    end
    return nil, nil
end

function tabbar.handleCallback(callbackName, elemHandle)
    if callbackName ~= TAB_CALLBACK then return false end
    
    local id, tabIndex = tabbar.findByElementHandle(elemHandle)
    if id and tabIndex then
        tabbar.select(id, tabIndex)
        return true
    end
    return false
end

function tabbar.isTabBarCallback(callbackName)
    return callbackName == TAB_CALLBACK
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function tabbar.getSelectedIndex(id)
    local tb = tabbars[id]
    if not tb then return nil end
    return tb.selectedIndex
end

function tabbar.getSelectedKey(id)
    local tb = tabbars[id]
    if not tb then return nil end
    local tab = tb.tabs[tb.selectedIndex]
    if not tab then return nil end
    return tab.key
end

function tabbar.getFrameHandle(id)
    local tb = tabbars[id]
    if not tb then return nil end
    return tb.frameBoxId
end

function tabbar.getFrameBounds(id)
    local tb = tabbars[id]
    if not tb then return 0, 0, 0, 0 end
    return tb.frameX, tb.frameY, tb.frameWidth, tb.frameHeight
end

function tabbar.getSize(id)
    local tb = tabbars[id]
    if not tb then return 0, 0 end
    return tb.width, tb.height + tb.frameHeight
end

return tabbar
