-- List UI component (selectable list with hover highlight + optional scrollbar)
-- Modeled after dropdown.lua but simpler: no text input, no arrow button.
-- Items are { text = "...", value = "..." }
local scale = require("scripts.ui.scale")
local scrollbar = require("scripts.ui.scrollbar")
local list = {}

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------
local LIST_ITEM_CALLBACK = "onListItemClick"

-----------------------------------------------------------
-- Module State
-----------------------------------------------------------

local lists = {}
local nextId = 1

local highlightTex = nil
local assetsLoaded = false

-----------------------------------------------------------
-- Initialization
-----------------------------------------------------------

function list.init()
    if assetsLoaded then return end

    highlightTex = engine.loadTexture("assets/textures/ui/highlight.png")

    scrollbar.init()

    assetsLoaded = true
    engine.logDebug("List module initialized")
end

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

function list.measureItems(items, font, fontSize)
    local maxWidth = 0
    for _, item in ipairs(items) do
        local w = engine.getTextWidth(font, item.text, fontSize)
        if w > maxWidth then
            maxWidth = w
        end
    end
    return maxWidth
end

-----------------------------------------------------------
-- Creation / Destruction
-----------------------------------------------------------

function list.new(params)
    local id = nextId
    nextId = nextId + 1

    local uiscale = params.uiscale or scale.get()
    local fontSize = math.floor((params.fontSize or 24) * uiscale)
    local itemHeight = math.floor((params.itemHeight or 36) * uiscale)
    local textPadding = math.floor((params.textPadding or 10) * uiscale)
    local scrollButtonSize = math.floor((params.scrollButtonSize or 24) * uiscale)

    local textColor = params.textColor or {1.0, 1.0, 1.0, 1.0}
    local highlightColor = params.highlightColor or {0.3, 0.5, 0.8, 0.8}
    local highlightTextColor = params.highlightTextColor or {1.0, 1.0, 1.0, 1.0}
    local selectedColor = params.selectedColor or {0.2, 0.4, 0.7, 1.0}
    local selectedTextColor = params.selectedTextColor or {1.0, 1.0, 1.0, 1.0}

    local items = params.items or {}
    local maxVisible = params.maxVisible or 10

    local visibleCount = math.min(#items, maxVisible)
    local listWidth = params.width or 300
    local listHeight = visibleCount * itemHeight

    local ls = {
        id = id,
        name = params.name or ("list_" .. id),
        x = params.x or 0,
        y = params.y or 0,
        width = listWidth,
        height = listHeight,
        fontSize = fontSize,
        itemHeight = itemHeight,
        textPadding = textPadding,
        scrollButtonSize = scrollButtonSize,
        page = params.page,
        font = params.font,
        items = items,
        maxVisible = maxVisible,
        visibleCount = visibleCount,
        selectedIndex = nil,
        hoveredSlot = nil,
        uiscale = uiscale,
        zIndex = params.zIndex or 1,
        -- Colors
        textColor = textColor,
        highlightColor = highlightColor,
        highlightTextColor = highlightTextColor,
        selectedColor = selectedColor,
        selectedTextColor = selectedTextColor,
        -- Callbacks
        onSelect = params.onSelect or nil,
        -- Scrollbar
        scrollbarId = nil,
        scrollOffset = 0,
        needsScroll = #items > maxVisible,
        -- Element handles
        slotElements = {},  -- { hitId, textId, highlightId, slot }
    }

    -- Create visible item slots
    ls.slotElements = {}
    for i = 1, visibleCount do
        local slotY = (i - 1) * itemHeight
        local textY = slotY + (itemHeight / 2) + (fontSize / 3)

        -- Highlight sprite (hidden by default)
        local hlId = UI.newSprite(
            ls.name .. "_hl_" .. i,
            listWidth,
            itemHeight,
            highlightTex,
            highlightColor[1], highlightColor[2],
            highlightColor[3], highlightColor[4],
            ls.page
        )
        UI.addToPage(ls.page, hlId, ls.x, ls.y + slotY)
        UI.setZIndex(hlId, ls.zIndex)
        UI.setVisible(hlId, false)

        -- Text label
        local dataIndex = i
        local itemText = ""
        if dataIndex <= #items then
            itemText = items[dataIndex].text
        end

        local txtId = UI.newText(
            ls.name .. "_txt_" .. i,
            itemText,
            ls.font,
            fontSize,
            textColor[1], textColor[2], textColor[3], textColor[4],
            ls.page
        )
        UI.addToPage(ls.page, txtId, ls.x + textPadding, ls.y + textY)
        UI.setZIndex(txtId, ls.zIndex + 2)

        -- Invisible hit-box sprite for click detection
        local hitId = UI.newSprite(
            ls.name .. "_hit_" .. i,
            listWidth,
            itemHeight,
            highlightTex,
            0.0, 0.0, 0.0, 0.0,
            ls.page
        )
        UI.addToPage(ls.page, hitId, ls.x, ls.y + slotY)
        UI.setClickable(hitId, true)
        UI.setOnClick(hitId, LIST_ITEM_CALLBACK)
        UI.setZIndex(hitId, ls.zIndex + 3)

        table.insert(ls.slotElements, {
            hitId = hitId,
            textId = txtId,
            highlightId = hlId,
            slot = i,
        })
    end

    -- Create scrollbar if needed
    if ls.needsScroll then
        local trackHeight = listHeight
            - (scrollButtonSize * 2)
            - (math.floor(4 * uiscale) * 2)
        if trackHeight < math.floor(20 * uiscale) then
            trackHeight = math.floor(20 * uiscale)
        end

        ls.scrollbarId = scrollbar.new({
            name = ls.name .. "_scrollbar",
            page = ls.page,
            x = ls.x + listWidth,
            y = ls.y,
            buttonSize = scrollButtonSize,
            trackHeight = trackHeight,
            capHeight = math.floor(4 * uiscale),
            tileSize = math.floor(8 * uiscale),
            totalItems = #items,
            visibleItems = visibleCount,
            uiscale = uiscale,
            zIndex = ls.zIndex + 4,
            onScroll = function(offset, sbId, sbName)
                list.onScrollChanged(id, offset)
            end,
        })
    end

    lists[id] = ls

    engine.logDebug("List created: " .. ls.name
        .. " items=" .. #ls.items
        .. " visible=" .. ls.visibleCount
        .. " scroll=" .. tostring(ls.needsScroll))

    return id
end

function list.destroy(id)
    local ls = lists[id]
    if not ls then return end

    if ls.scrollbarId then
        scrollbar.destroy(ls.scrollbarId)
        ls.scrollbarId = nil
    end

    for _, slot in ipairs(ls.slotElements) do
        if slot.hitId then UI.deleteElement(slot.hitId) end
        if slot.textId then UI.deleteElement(slot.textId) end
        if slot.highlightId then UI.deleteElement(slot.highlightId) end
    end

    lists[id] = nil
    engine.logDebug("List destroyed: " .. (ls.name or "?"))
end

function list.destroyAll()
    for id, _ in pairs(lists) do
        list.destroy(id)
    end
    lists = {}
    nextId = 1
end

-----------------------------------------------------------
-- Virtual Scrolling
-----------------------------------------------------------

function list.onScrollChanged(id, newOffset)
    local ls = lists[id]
    if not ls then return end

    ls.scrollOffset = newOffset
    ls.hoveredSlot = nil
    list.refreshSlots(id)
end

function list.refreshSlots(id)
    local ls = lists[id]
    if not ls then return end

    for _, slot in ipairs(ls.slotElements) do
        local dataIndex = ls.scrollOffset + slot.slot

        if dataIndex <= #ls.items then
            local item = ls.items[dataIndex]
            UI.setText(slot.textId, item.text)

            -- Determine colors based on selection state
            if dataIndex == ls.selectedIndex then
                UI.setVisible(slot.highlightId, true)
                UI.setColor(slot.textId,
                    ls.selectedTextColor[1], ls.selectedTextColor[2],
                    ls.selectedTextColor[3], ls.selectedTextColor[4])
            else
                UI.setVisible(slot.highlightId, false)
                UI.setColor(slot.textId,
                    ls.textColor[1], ls.textColor[2],
                    ls.textColor[3], ls.textColor[4])
            end
        else
            UI.setText(slot.textId, "")
            UI.setVisible(slot.highlightId, false)
        end
    end
end

-----------------------------------------------------------
-- Hover Handling
-----------------------------------------------------------

function list.setHoveredSlot(id, slotIndex)
    local ls = lists[id]
    if not ls then return end
    if ls.hoveredSlot == slotIndex then return end

    -- Clear previous hover
    if ls.hoveredSlot then
        local prev = ls.slotElements[ls.hoveredSlot]
        if prev then
            local prevDataIndex = ls.scrollOffset + ls.hoveredSlot
            if prevDataIndex == ls.selectedIndex then
                -- Restore selected appearance
                UI.setVisible(prev.highlightId, true)
                UI.setColor(prev.textId,
                    ls.selectedTextColor[1], ls.selectedTextColor[2],
                    ls.selectedTextColor[3], ls.selectedTextColor[4])
            else
                UI.setVisible(prev.highlightId, false)
                UI.setColor(prev.textId,
                    ls.textColor[1], ls.textColor[2],
                    ls.textColor[3], ls.textColor[4])
            end
        end
    end

    ls.hoveredSlot = slotIndex

    -- Apply new hover
    if slotIndex then
        local slot = ls.slotElements[slotIndex]
        if slot then
            local dataIndex = ls.scrollOffset + slotIndex
            if dataIndex >= 1 and dataIndex <= #ls.items then
                UI.setVisible(slot.highlightId, true)
                UI.setColor(slot.textId,
                    ls.highlightTextColor[1], ls.highlightTextColor[2],
                    ls.highlightTextColor[3], ls.highlightTextColor[4])
            end
        end
    end
end

function list.clearHover(id)
    list.setHoveredSlot(id, nil)
end

function list.onHoverEnter(elemHandle)
    for id, ls in pairs(lists) do
        for _, slot in ipairs(ls.slotElements) do
            if slot.hitId == elemHandle then
                list.setHoveredSlot(id, slot.slot)
                return
            end
        end
    end
end

function list.onHoverLeave(elemHandle)
    for id, ls in pairs(lists) do
        if ls.hoveredSlot then
            local slot = ls.slotElements[ls.hoveredSlot]
            if slot and slot.hitId == elemHandle then
                list.clearHover(id)
                return
            end
        end
    end
end

-----------------------------------------------------------
-- Selection
-----------------------------------------------------------

function list.selectItem(id, dataIndex)
    local ls = lists[id]
    if not ls then return end
    if dataIndex < 1 or dataIndex > #ls.items then return end

    ls.selectedIndex = dataIndex
    list.refreshSlots(id)

    if ls.onSelect then
        local item = ls.items[dataIndex]
        ls.onSelect(item.value, item.text, dataIndex, id, ls.name)
    end

    engine.logDebug("List selected: " .. ls.name
        .. " [" .. dataIndex .. "] = " .. ls.items[dataIndex].text)
end

-----------------------------------------------------------
-- Click Handling
-----------------------------------------------------------

function list.findByElementHandle(elemHandle)
    for id, ls in pairs(lists) do
        for _, slot in ipairs(ls.slotElements) do
            if slot.hitId == elemHandle then
                return id, slot.slot
            end
        end
    end
    return nil, nil
end

function list.handleCallback(callbackName, elemHandle)
    if callbackName == LIST_ITEM_CALLBACK then
        local id, slotIdx = list.findByElementHandle(elemHandle)
        if id and slotIdx then
            local ls = lists[id]
            local dataIndex = ls.scrollOffset + slotIdx
            if dataIndex >= 1 and dataIndex <= #ls.items then
                list.selectItem(id, dataIndex)
            end
            return true
        end
    elseif callbackName == "onScrollUp" then
        local sbId, _ = scrollbar.findByElementHandle(elemHandle)
        if sbId then
            -- Check if this scrollbar belongs to one of our lists
            for id, ls in pairs(lists) do
                if ls.scrollbarId == sbId then
                    scrollbar.scrollUp(sbId)
                    return true
                end
            end
        end
    elseif callbackName == "onScrollDown" then
        local sbId, _ = scrollbar.findByElementHandle(elemHandle)
        if sbId then
            for id, ls in pairs(lists) do
                if ls.scrollbarId == sbId then
                    scrollbar.scrollDown(sbId)
                    return true
                end
            end
        end
    end
    return false
end

-----------------------------------------------------------
-- Scroll Input (mouse wheel)
-----------------------------------------------------------

function list.onScroll(elemHandle, dx, dy)
    for id, ls in pairs(lists) do
        if ls.needsScroll and ls.scrollbarId then
            local isInList = false

            for _, slot in ipairs(ls.slotElements) do
                if slot.hitId == elemHandle or slot.textId == elemHandle
                    or slot.highlightId == elemHandle then
                    isInList = true
                    break
                end
            end

            if not isInList then
                local sbId, _ = scrollbar.findByElementHandle(elemHandle)
                if sbId == ls.scrollbarId then
                    isInList = true
                end
            end

            if isInList then
                if dy > 0 then
                    scrollbar.scrollUp(ls.scrollbarId)
                elseif dy < 0 then
                    scrollbar.scrollDown(ls.scrollbarId)
                end
                return true
            end
        end
    end
    return false
end

-----------------------------------------------------------
-- Data Management
-----------------------------------------------------------

function list.setItems(id, items)
    local ls = lists[id]
    if not ls then return end

    ls.items = items or {}
    ls.selectedIndex = nil
    ls.scrollOffset = 0
    ls.hoveredSlot = nil

    local needsScroll = #ls.items > ls.maxVisible
    ls.needsScroll = needsScroll

    if ls.scrollbarId then
        if needsScroll then
            scrollbar.setContentSize(ls.scrollbarId, #ls.items, ls.visibleCount)
        else
            scrollbar.setVisible(ls.scrollbarId, false)
        end
    end

    list.refreshSlots(id)
end

-----------------------------------------------------------
-- Visibility / Position
-----------------------------------------------------------

function list.setVisible(id, visible)
    local ls = lists[id]
    if not ls then return end

    for _, slot in ipairs(ls.slotElements) do
        UI.setVisible(slot.hitId, visible)
        UI.setVisible(slot.textId, visible)
        if not visible then
            UI.setVisible(slot.highlightId, false)
        end
    end

    if ls.scrollbarId then
        scrollbar.setVisible(ls.scrollbarId, visible and ls.needsScroll)
    end
end

function list.setPosition(id, x, y)
    local ls = lists[id]
    if not ls then return end

    ls.x = x
    ls.y = y

    for _, slot in ipairs(ls.slotElements) do
        local slotY = (slot.slot - 1) * ls.itemHeight
        local textY = slotY + (ls.itemHeight / 2) + (ls.fontSize / 3)

        UI.setPosition(slot.highlightId, x, y + slotY)
        UI.setPosition(slot.textId, x + ls.textPadding, y + textY)
        UI.setPosition(slot.hitId, x, y + slotY)
    end

    if ls.scrollbarId then
        scrollbar.setPosition(ls.scrollbarId, x + ls.width, y)
    end
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function list.getSize(id)
    local ls = lists[id]
    if not ls then return 0, 0 end
    local totalW = ls.width
    if ls.needsScroll and ls.scrollbarId then
        totalW = totalW + scrollbar.getTrackWidth(ls.scrollbarId)
    end
    return totalW, ls.height
end

function list.getSelectedIndex(id)
    local ls = lists[id]
    if not ls then return nil end
    return ls.selectedIndex
end

function list.getSelectedValue(id)
    local ls = lists[id]
    if not ls then return nil end
    if not ls.selectedIndex then return nil end
    return ls.items[ls.selectedIndex].value
end

function list.getSelectedText(id)
    local ls = lists[id]
    if not ls then return nil end
    if not ls.selectedIndex then return nil end
    return ls.items[ls.selectedIndex].text
end

function list.isListCallback(callbackName)
    return callbackName == LIST_ITEM_CALLBACK
end

return list
