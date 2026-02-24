-- Toggle Group UI component
-- A horizontal row of sprite buttons where exactly one is selected at a time.
-- Each item has a "default" (unselected) and "selected" texture.
-- Right-clicking a button opens an options popup; selecting an option swaps it
-- into the toggle list and moves the old item into the options pool.
local scale = require("scripts.ui.scale")
local toggle = {}

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------
local TOGGLE_CALLBACK       = "onToggleClick"
local TOGGLE_RIGHT_CALLBACK = "onToggleRightClick"
local OPTION_CALLBACK       = "onToggleOptionClick"

-----------------------------------------------------------
-- Module State
-----------------------------------------------------------

local groups      = {}   -- groupId -> group data
local nextGroupId = 1

-----------------------------------------------------------
-- Initialization (call once from ui_manager)
-----------------------------------------------------------

function toggle.init()
    engine.logDebug("Toggle module initialized")
end

-----------------------------------------------------------
-- Internal helpers
-----------------------------------------------------------

--- Compute the position of a button at index `i` within a group of `count`
--- items, given layout direction and anchor point.
local function buttonPosition(grp, i, count)
    local size = grp.size
    local pad  = grp.padding
    local dir  = grp.direction

    if dir == "left" then
        local offsetFromRight = (count - i) * (size + pad)
        return grp.anchorX - size - offsetFromRight, grp.anchorY

    elseif dir == "right" then
        local offsetFromLeft = (i - 1) * (size + pad)
        return grp.anchorX + offsetFromLeft, grp.anchorY

    elseif dir == "up" then
        local offsetFromBottom = (count - i) * (size + pad)
        return grp.anchorX, grp.anchorY - offsetFromBottom

    elseif dir == "down" then
        local offsetFromTop = (i - 1) * (size + pad)
        return grp.anchorX, grp.anchorY + offsetFromTop
    end

    return grp.anchorX, grp.anchorY
end

--- Compute position for an option sprite at rank `rank` (1 = closest to the
--- button) expanding from `bx, by` in `optDir`.
local function optionPosition(grp, bx, by, rank, optDir)
    local size = grp.size
    local pad  = grp.padding
    local offset = rank * (size + pad)

    if optDir == "left" then
        return bx - offset, by
    elseif optDir == "right" then
        return bx + offset, by
    elseif optDir == "up" then
        return bx, by - offset
    elseif optDir == "down" then
        return bx, by + offset
    end

    return bx, by
end

--- Create a single sprite for a button/option, add it to the page, and
--- configure click callbacks.
local function createSprite(grp, label, tex, x, y, callback)
    local spriteId = UI.newSprite(
        grp.name .. "_" .. label,
        grp.size, grp.size,
        tex,
        1.0, 1.0, 1.0, 1.0,
        grp.page
    )
    UI.addToPage(grp.page, spriteId, x, y)
    UI.setClickable(spriteId, true)
    UI.setOnClick(spriteId, TOGGLE_CALLBACK)
    UI.setOnRightClick(spriteId, TOGGLE_RIGHT_CALLBACK)
    UI.setZIndex(spriteId, grp.zIndex)
    return spriteId
end

-----------------------------------------------------------
-- Options popup management
-----------------------------------------------------------

--- Close any currently open options popup in the group, destroying sprites.
local function closeOptions(grp)
    if not grp.openPopup then return end

    for _, opt in ipairs(grp.openPopup.sprites) do
        UI.removeFromPage(grp.page, opt.spriteId)
    end

    grp.openPopup = nil
end

--- Open an options popup for button at index `btnIdx`.
local function openOptions(grp, btnIdx)
    -- Close any existing popup first.
    closeOptions(grp)

    local btn = grp.buttons[btnIdx]
    if not btn.options or #btn.options == 0 then
        return  -- nothing to show
    end

    -- Find the screen position of the button that was right-clicked.
    local bx, by = buttonPosition(grp, btnIdx, #grp.buttons)
    local optDir = grp.optionsDirection

    local popup = {
        btnIdx  = btnIdx,
        sprites = {},   -- { spriteId, optionIndex }
    }

    for rank, opt in ipairs(btn.options) do
        local ox, oy = optionPosition(grp, bx, by, rank, optDir)

        -- Options are always shown in their "default" (unselected) look.
        local spriteId = UI.newSprite(
            grp.name .. "_opt_" .. btnIdx .. "_" .. rank .. "_sprite",
            grp.size, grp.size,
            opt.texDefault,
            1.0, 1.0, 1.0, 1.0,
            grp.page
        )
        UI.addToPage(grp.page, spriteId, ox, oy)
        UI.setClickable(spriteId, true)
        UI.setOnClick(spriteId, OPTION_CALLBACK)
        UI.setZIndex(spriteId, grp.zIndex + 1)  -- above toggle buttons

        popup.sprites[rank] = {
            spriteId    = spriteId,
            optionIndex = rank,
        }
    end

    grp.openPopup = popup
end

-----------------------------------------------------------
-- Creation / Destruction
-----------------------------------------------------------

--- Create a new toggle group.
-- @param params table:
--   name             (string)   human-readable group name
--   page             (handle)   UI page the sprites belong to
--   items            (array)    ordered list of item descriptors:
--       { name          = "mapTemp",
--         texDefault    = <handle>,   -- texture when unselected
--         texSelected   = <handle>,   -- texture when selected
--         options       = {           -- right-click alternatives (optional)
--             { name = "altA", texDefault = <h>, texSelected = <h> },
--             ...
--         } }
--   selectedIndex    (int)      1-based initially selected item (default 1)
--   direction        (string)   "left"|"right"|"up"|"down" (default "left")
--   optionsDirection (string)   direction options expand from a button
--                               "left"|"right"|"up"|"down" (default "down")
--   size             (number)   base (unscaled) width & height of each button
--   padding          (number)   base (unscaled) gap between buttons
--   x                (number)   screen x of the anchor edge
--   y                (number)   screen y of the anchor edge
--   zIndex           (number)   z-index for sprites (default 100)
--   uiscale          (number)   override scale (optional)
--   onChange         (function)  callback(selectedIndex, itemName)
--   onOptionSelect   (function)  callback(buttonIndex, newItemName, oldItemName)
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

    local grp = {
        id               = id,
        name             = params.name or ("toggle_" .. id),
        page             = params.page,
        size             = size,
        padding          = pad,
        anchorX          = params.x or 0,
        anchorY          = params.y or 0,
        zIndex           = zIndex,
        selectedIndex    = selIdx,
        direction        = params.direction or "left",
        optionsDirection = params.optionsDirection or "down",
        onChange          = params.onChange,
        onOptionSelect   = params.onOptionSelect,
        buttons          = {},
        openPopup        = nil,   -- { btnIdx, sprites[] }
    }

    local count = #items
    for i = 1, count do
        local item = items[i]
        local bx, by = buttonPosition(grp, i, count)
        local tex = (i == selIdx) and item.texSelected or item.texDefault

        local spriteId = createSprite(
            grp,
            (item.name or tostring(i)) .. "_sprite",
            tex, bx, by,
            TOGGLE_CALLBACK
        )

        -- Deep-copy the options list so mutations stay local to this group.
        local opts = {}
        if item.options then
            for j, o in ipairs(item.options) do
                opts[j] = {
                    name        = o.name,
                    texDefault  = o.texDefault,
                    texSelected = o.texSelected,
                }
            end
        end

        grp.buttons[i] = {
            name        = item.name or ("item_" .. i),
            spriteId    = spriteId,
            texDefault  = item.texDefault,
            texSelected = item.texSelected,
            options     = opts,
        }
    end

    groups[id] = grp

    engine.logDebug("Toggle group created: " .. grp.name
        .. " with " .. count .. " items, selected=" .. selIdx)

    return id
end

function toggle.destroy(id)
    local grp = groups[id]
    if not grp then return end
    closeOptions(grp)
    
    for _, btn in ipairs(grp.buttons) do
        if btn.spriteId then
            UI.deleteElement(btn.spriteId)
        end
    end
    
    groups[id] = nil
    engine.logDebug("Toggle group destroyed: " .. grp.name)
end

function toggle.destroyAll()
    for _, grp in pairs(groups) do
        closeOptions(grp)
    end
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
-- Option swap logic
-----------------------------------------------------------

--- Swap an option into a button's slot. The old button data goes to position 1
--- (closest to the button) in the options list so the user can swap back.
local function applyOption(grp, btnIdx, optionIndex)
    local btn = grp.buttons[btnIdx]
    local opt = btn.options[optionIndex]
    if not opt then return end

    -- Save the current button identity as a new option entry.
    local displaced = {
        name        = btn.name,
        texDefault  = btn.texDefault,
        texSelected = btn.texSelected,
    }

    -- Remove the chosen option from the list.
    table.remove(btn.options, optionIndex)

    -- Insert the displaced item at position 1 (closest to button on next open).
    table.insert(btn.options, 1, displaced)

    local oldName = btn.name

    -- Replace button identity with the chosen option.
    btn.name        = opt.name
    btn.texDefault  = opt.texDefault
    btn.texSelected = opt.texSelected

    -- Select this button in the group (making it the active toggle).
    toggle.select(grp.id, btnIdx)

    engine.logDebug("Toggle option applied: button " .. btnIdx
        .. " now '" .. btn.name .. "' (was '" .. oldName .. "')")

    -- Fire callbacks.
    if grp.onOptionSelect then
        grp.onOptionSelect(btnIdx, btn.name, oldName)
    end
    if grp.onChange then
        grp.onChange(btnIdx, btn.name)
    end
end

-----------------------------------------------------------
-- Click Handling
-----------------------------------------------------------

--- Find which group and button index owns `elemHandle`.
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

--- Find which group and option index owns a popup sprite handle.
local function findOptionByHandle(elemHandle)
    for gid, grp in pairs(groups) do
        if grp.openPopup then
            for _, entry in ipairs(grp.openPopup.sprites) do
                if entry.spriteId == elemHandle then
                    return gid, grp.openPopup.btnIdx, entry.optionIndex
                end
            end
        end
    end
    return nil, nil, nil
end

--- Left-click on a toggle button: select it and close any open popup.
function toggle.handleClickByElement(elemHandle)
    -- First check if this is an option sprite click.
    local ogid, oBtnIdx, oOptIdx = findOptionByHandle(elemHandle)
    if ogid then
        local grp = groups[ogid]
        closeOptions(grp)
        applyOption(grp, oBtnIdx, oOptIdx)
        return true
    end

    -- Otherwise it's a regular toggle button click.
    local gid, idx = toggle.findByElementHandle(elemHandle)
    if gid and idx then
        local grp = groups[gid]
        closeOptions(grp)  -- dismiss any open popup
        toggle.select(gid, idx)

        if grp.onChange then
            grp.onChange(idx, grp.buttons[idx].name)
        end
        return true
    end
    return false
end

--- Right-click on a toggle button: open its options popup.
function toggle.handleRightClickByElement(elemHandle)
    local gid, idx = toggle.findByElementHandle(elemHandle)
    if gid and idx then
        local grp = groups[gid]
        -- If the same button's popup is already open, close it (toggle).
        if grp.openPopup and grp.openPopup.btnIdx == idx then
            closeOptions(grp)
        else
            openOptions(grp, idx)
        end
        return true
    end

    -- Right-clicking elsewhere (but not on an option) dismisses popup.
    for _, grp in pairs(groups) do
        if grp.openPopup then
            closeOptions(grp)
        end
    end
    return false
end

function toggle.handleCallback(callbackName, elemHandle)
    if callbackName == TOGGLE_CALLBACK and elemHandle then
        return toggle.handleClickByElement(elemHandle)
    elseif callbackName == TOGGLE_RIGHT_CALLBACK and elemHandle then
        return toggle.handleRightClickByElement(elemHandle)
    elseif callbackName == OPTION_CALLBACK and elemHandle then
        return toggle.handleClickByElement(elemHandle)  -- reuse; it checks options first
    end
    return false
end

function toggle.isToggleCallback(callbackName)
    return callbackName == TOGGLE_CALLBACK
        or callbackName == TOGGLE_RIGHT_CALLBACK
        or callbackName == OPTION_CALLBACK
end

--- Dismiss any open options popup in any group (useful for global click-away).
function toggle.dismissOpenPopups()
    for _, grp in pairs(groups) do
        if grp.openPopup then
            closeOptions(grp)
        end
    end
end

-----------------------------------------------------------
-- Hover (stubs)
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

--- Returns true if a popup is currently open in any group.
function toggle.hasOpenPopup()
    for _, grp in pairs(groups) do
        if grp.openPopup then return true end
    end
    return false
end

--- Returns true if `elemHandle` belongs to an open options popup.
function toggle.isOptionElement(elemHandle)
    local gid, _, _ = findOptionByHandle(elemHandle)
    return gid ~= nil
end

return toggle
