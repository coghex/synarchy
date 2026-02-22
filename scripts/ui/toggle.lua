-- Toggle Group UI component
-- A horizontal row of sprite buttons where exactly one is selected at a time.
-- Each item has a "default" (unselected) and "selected" texture.
local scale = require("scripts.ui.scale")
local toggle = {}

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------
local TOGGLE_CALLBACK = "onToggleClick"

-----------------------------------------------------------
-- Module State
-----------------------------------------------------------

local groups = {}   -- groupId -> group data
local nextGroupId = 1

-----------------------------------------------------------
-- Initialization (call once from ui_manager)
-----------------------------------------------------------

function toggle.init()
    engine.logDebug("Toggle module initialized")
end

-----------------------------------------------------------
-- Creation / Destruction
-----------------------------------------------------------

--- Create a new toggle group.
-- @param params table:
--   name        (string)  human-readable group name
--   page        (handle)  UI page the sprites belong to
--   items       (array)   ordered list of item descriptors, laid out left-to-right:
--       { name = "mapTemp",
--         texDefault  = <handle>,   -- texture when unselected
--         texSelected = <handle> }  -- texture when selected
--   selectedIndex (int)   1-based index of the initially selected item (default 1)
--   direction   (string) "left" or "right" (default "left") - which way the items grow
--   size        (number)  base (unscaled) width & height of each button sprite
--   padding     (number)  base (unscaled) gap between buttons
--   x           (number)  screen x of the *right* edge of the rightmost button
--   y           (number)  screen y position (top of the sprites)
--   zIndex      (number)  z-index for the sprites (optional, default 100)
--   uiscale     (number)  override scale (optional, uses engine scale)
--   onChange    (function) callback(selectedIndex, itemName)  -- TODO: wire up
-- @return groupId
function toggle.new(params)
    local id = nextGroupId
    nextGroupId = nextGroupId + 1

    local uiscale = params.uiscale or scale.get()
    local size    = math.floor((params.size or 64) * uiscale)
    local pad     = math.floor((params.padding or 8) * uiscale)
    local items   = params.items or {}
    local selIdx  = params.selectedIndex or 1
    local zIndex  = params.zIndex or 100
    local direction = params.direction or "left"

    local grp = {
        id            = id,
        name          = params.name or ("toggle_" .. id),
        page          = params.page,
        size          = size,
        padding       = pad,
        anchorX       = params.x or 0,   -- right-edge anchor
        anchorY       = params.y or 0,
        zIndex        = zIndex,
        selectedIndex = selIdx,
        direction     = direction,
        onChange       = params.onChange,
        buttons       = {},   -- array of { name, spriteId, texDefault, texSelected }
    }

    -- Layout: rightmost item is items[#items], placed so its right edge = anchorX.
    -- Items go left from there.
    local count = #items
    if direction == "left" then
        for i = count, 1, -1 do
            local item = items[i]
            -- Distance from right edge: (count - i) * (size + pad)
            local offsetFromRight = (count - i) * (size + pad)
            local spriteX = grp.anchorX - size - offsetFromRight
            local spriteY = grp.anchorY

            local tex = (i == selIdx) and item.texSelected or item.texDefault

            local spriteId = UI.newSprite(
                grp.name .. "_" .. (item.name or tostring(i)) .. "_sprite",
                size,
                size,
                tex,
                1.0, 1.0, 1.0, 1.0,
                grp.page
            )

            UI.addToPage(grp.page, spriteId, spriteX, spriteY)
            UI.setClickable(spriteId, true)
            UI.setOnClick(spriteId, TOGGLE_CALLBACK)
            UI.setZIndex(spriteId, zIndex)

            grp.buttons[i] = {
                name       = item.name or ("item_" .. i),
                spriteId   = spriteId,
                texDefault = item.texDefault,
                texSelected = item.texSelected,
            }
        end
    elseif direction == "up" then
        for i = 1, count do
            local item = items[i]
            local offsetFromBottom = (count - i) * (size + pad)
            local spriteX = grp.anchorX
            local spriteY = grp.anchorY - offsetFromBottom
            local tex = (i == selIdx) and item.texSelected or item.texDefault
            local spriteId = UI.newSprite(
                grp.name .. "_" .. (item.name or tostring(i)) .. "_sprite",
                size,
                size,
                tex,
                1.0, 1.0, 1.0, 1.0,
                grp.page
            )
            UI.addToPage(grp.page, spriteId, spriteX, spriteY)
            UI.setClickable(spriteId, true)
            UI.setOnClick(spriteId, TOGGLE_CALLBACK)
            UI.setZIndex(spriteId, zIndex)
            grp.buttons[i] = {
                name       = item.name or ("item_" .. i),
                spriteId   = spriteId,
                texDefault = item.texDefault,
                texSelected = item.texSelected,
            }
        end
    elseif direction == "down" then
        for i = 1, count do
            local item = items[i]
            local offsetFromTop = (i - 1) * (size + pad)
            local spriteX = grp.anchorX
            local spriteY = grp.anchorY + offsetFromTop
            local tex = (i == selIdx) and item.texSelected or item.texDefault

            local spriteId = UI.newSprite(
                grp.name .. "_" .. (item.name or tostring(i)) .. "_sprite",
                size,
                size,
                tex,
                1.0, 1.0, 1.0, 1.0,
                grp.page
            )

            UI.addToPage(grp.page, spriteId, spriteX, spriteY)
            UI.setClickable(spriteId, true)
            UI.setOnClick(spriteId, TOGGLE_CALLBACK)
            UI.setZIndex(spriteId, zIndex)

            grp.buttons[i] = {
                name       = item.name or ("item_" .. i),
                spriteId   = spriteId,
                texDefault = item.texDefault,
                texSelected = item.texSelected,
            }
        end
    end

    groups[id] = grp

    engine.logDebug("Toggle group created: " .. grp.name
        .. " with " .. count .. " items, selected=" .. selIdx)

    return id
end

function toggle.destroy(id)
    local grp = groups[id]
    if not grp then return end
    -- sprites are owned by the page; just forget our bookkeeping
    groups[id] = nil
    engine.logDebug("Toggle group destroyed: " .. grp.name)
end

function toggle.destroyAll()
    groups = {}
    nextGroupId = 1
end

-----------------------------------------------------------
-- Selection
-----------------------------------------------------------

--- Programmatically select an item (1-based index).
function toggle.select(groupId, index)
    local grp = groups[groupId]
    if not grp then return end
    if index < 1 or index > #grp.buttons then return end

    grp.selectedIndex = index

    for i, btn in ipairs(grp.buttons) do
        local tex = (i == index) and btn.texSelected or btn.texDefault
        UI.setSpriteTexture(btn.spriteId, tex)
    end
end

function toggle.getSelectedIndex(groupId)
    local grp = groups[groupId]
    if not grp then return nil end
    return grp.selectedIndex
end

function toggle.getSelectedName(groupId)
    local grp = groups[groupId]
    if not grp then return nil end
    local btn = grp.buttons[grp.selectedIndex]
    return btn and btn.name or nil
end

-----------------------------------------------------------
-- Click Handling
-----------------------------------------------------------

function toggle.findByElementHandle(elemHandle)
    for gid, grp in pairs(groups) do
        for i, btn in ipairs(grp.buttons) do
            if btn.spriteId == elemHandle then
                return gid, i
            end
        end
    end
    return nil, nil
end

function toggle.handleClickByElement(elemHandle)
    local gid, idx = toggle.findByElementHandle(elemHandle)
    if gid and idx then
        toggle.select(gid, idx)

        local grp = groups[gid]
        if grp.onChange then
            grp.onChange(idx, grp.buttons[idx].name)
        end
        return true
    end
    return false
end

function toggle.handleCallback(callbackName, elemHandle)
    if callbackName == TOGGLE_CALLBACK and elemHandle then
        return toggle.handleClickByElement(elemHandle)
    end
    return false
end

function toggle.isToggleCallback(callbackName)
    return callbackName == TOGGLE_CALLBACK
end

-----------------------------------------------------------
-- Hover (stubs — fill in if you want visual hover state)
-----------------------------------------------------------

function toggle.onHoverEnter(elemHandle)
    -- TODO: optional hover highlight
end

function toggle.onHoverLeave(elemHandle)
    -- TODO: optional hover unhighlight
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function toggle.getElementHandles(groupId)
    local grp = groups[groupId]
    if not grp then return {} end
    local handles = {}
    for _, btn in ipairs(grp.buttons) do
        table.insert(handles, btn.spriteId)
    end
    return handles
end

return toggle
