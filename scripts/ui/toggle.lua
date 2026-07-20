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

local texHighlight = nil

-- Hover overlay colour, matched to the dropdown/list highlight so the
-- whole UI kit gives the same hover affordance.
local HIGHLIGHT_COLOR = {0.3, 0.5, 0.8, 0.8}

-----------------------------------------------------------
-- Initialization (call once from ui_manager)
-----------------------------------------------------------

function toggle.init()
    texHighlight = engine.loadTexture("assets/textures/ui/highlight.png")
    engine.logDebug("Toggle module initialized")
end

-- Create a hidden hover-highlight overlay parented to a button/option
-- sprite. As a child it tracks the sprite and resolves its hover target
-- up to the clickable sprite (no flicker). Returns the overlay handle.
local function makeHighlight(grp, parentSpriteId, label)
    local hlId = UI.newSprite(
        grp.name .. "_hl_" .. label,
        grp.size, grp.size,
        texHighlight,
        HIGHLIGHT_COLOR[1], HIGHLIGHT_COLOR[2],
        HIGHLIGHT_COLOR[3], HIGHLIGHT_COLOR[4],
        grp.page
    )
    UI.addChild(parentSpriteId, hlId, 0, 0)
    UI.setZIndex(hlId, 1)
    UI.setVisible(hlId, false)
    return hlId
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

--- Apply the tooltip configuration to the in-slot sprite of `btnIdx`.
--- Buttons whose slot has alternative options get a rich tooltip
--- ("title" + small grey hint from grp.expandHintText) so the right-
--- click affordance is discoverable. Buttons with no options get the
--- plain title. Buttons with no `tooltip` field get nothing.
local function applyButtonTooltip(grp, btnIdx)
    local btn = grp.buttons[btnIdx]
    if not btn or not btn.spriteId then return end
    if not btn.tooltip then
        UI.clearTooltip(btn.spriteId)
        return
    end
    local hasOptions = btn.options and #btn.options > 0
    if hasOptions and grp.expandHintText then
        UI.setTooltipRich(btn.spriteId, {
            text = btn.tooltip,
            hint = grp.expandHintText,
        })
    else
        UI.setTooltip(btn.spriteId, btn.tooltip)
    end
end

-----------------------------------------------------------
-- Options popup management
-----------------------------------------------------------

--- Close any currently open options popup in the group, destroying sprites.
local function closeOptions(grp)
    if not grp.openPopup then return end

    -- Full delete, not removeFromPage: openOptions creates fresh
    -- sprites every time, and removeFromPage only detaches (the
    -- element handle stays alive in the page manager) — using it
    -- here leaked one element per option per open/close cycle.
    for _, opt in ipairs(grp.openPopup.sprites) do
        UI.deleteElement(opt.spriteId)
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
        -- Plain tooltip on options: the user has already chosen to
        -- expand and is looking at alternatives, so the "right click to
        -- expand" hint isn't relevant here. Tooltip on the in-slot
        -- button conveys discoverability; tooltips on options just name
        -- the alternative.
        if opt.tooltip then
            UI.setTooltip(spriteId, opt.tooltip)
        end

        popup.sprites[rank] = {
            spriteId    = spriteId,
            optionIndex = rank,
            highlightId = makeHighlight(grp, spriteId,
                "opt_" .. btnIdx .. "_" .. rank),
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
        -- Introspection type (F3, #645): most toggle groups are plain
        -- settings toggles, but hud.lua's toolbar icon stacks pass
        -- "toolbarTool" so ui.dumpWidgets() can tell them apart.
        widgetType       = params.widgetType or "toggle",
        buttons          = {},
        openPopup        = nil,   -- { btnIdx, sprites[] }
        -- Tooltip hint shown beneath the title on slot buttons that
        -- have at least one alternative option. Set to `false` (not
        -- nil) to disable the hint per-group.
        expandHintText   = params.expandHintText == nil
                              and "Right click to expand"
                              or params.expandHintText,
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
                    tooltip     = o.tooltip,
                }
            end
        end

        grp.buttons[i] = {
            name        = item.name or ("item_" .. i),
            spriteId    = spriteId,
            texDefault  = item.texDefault,
            texSelected = item.texSelected,
            tooltip     = item.tooltip,
            options     = opts,
            highlightId = makeHighlight(grp, spriteId, (item.name or tostring(i))),
        }

        applyButtonTooltip(grp, i)
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
local function applyOption(grp, btnIdx, optionIndex, silent)
    local btn = grp.buttons[btnIdx]
    local opt = btn.options[optionIndex]
    if not opt then return end

    -- Save the current button identity as a new option entry.
    local displaced = {
        name        = btn.name,
        texDefault  = btn.texDefault,
        texSelected = btn.texSelected,
        tooltip     = btn.tooltip,
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
    btn.tooltip     = opt.tooltip

    -- Update the in-slot tooltip to reflect the new identity. The button
    -- still has options (the displaced item is now one), so the rich
    -- form with the expand-hint is appropriate.
    applyButtonTooltip(grp, btnIdx)

    if silent then
        -- #750: a layout-only rebuild restoring a previously-swapped
        -- slot identity must not select an unrelated slot or fire the
        -- real click-path callbacks (world.setToolMode/setMapMode,
        -- onOptionSelect) a second time for a mode that's already
        -- active engine-side — see toggle.restoreSlotIdentity.
        return
    end

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

--- Find an option (by name) on any of the group's buttons and apply
--- it, mirroring the right-click-then-pick-option UX. Fires the same
--- onOptionSelect + onChange callbacks as a user pick. Returns true if
--- found and applied, false otherwise.
---
--- Two hit-paths in priority order:
---  1. The name already matches a slot button — just re-select it.
---  2. The name appears in some button's `options` list — swap it in.
function toggle.applyOptionByName(groupId, name)
    local grp = groups[groupId]
    if not grp then return false end

    for i, btn in ipairs(grp.buttons) do
        if btn.name == name then
            toggle.select(groupId, i)
            if grp.onChange then grp.onChange(i, btn.name) end
            return true
        end
    end

    for btnIdx, btn in ipairs(grp.buttons) do
        if btn.options then
            for optIdx, opt in ipairs(btn.options) do
                if opt.name == name then
                    applyOption(grp, btnIdx, optIdx)
                    return true
                end
            end
        end
    end
    return false
end

-- #750: per-slot identity snapshot/restore for a layout-only rebuild
-- (hud.lua's createUI). A slot's NAME (and texture set) can be swapped
-- by picking an alternative from its options popup (applyOption above)
-- independently of which slot is selectedIndex — a fresh toggle.new
-- after a rebuild always recreates every slot at its hardcoded default
-- identity, so restoring only the selected INDEX (toggle.select) still
-- displays the wrong icon for a slot whose identity had been swapped.

--- Live per-slot names, in button order — unlike toggle.dump(), this
--- doesn't require the element to be currently visible.
function toggle.getSlotNames(groupId)
    local grp = groups[groupId]
    if not grp then return {} end
    local names = {}
    for i, btn in ipairs(grp.buttons) do
        names[i] = btn.name
    end
    return names
end

--- Restore slot `btnIdx`'s identity to `name` without selecting it or
--- firing onOptionSelect/onChange (the mode it names is already active
--- engine-side — a layout-only rebuild must not re-issue that
--- transition). No-op if the slot already shows `name`, or if `name`
--- isn't found among that slot's current options (e.g. the slot has no
--- swappable alternates at all). Callers that also need the correct
--- selectedIndex restored should follow up with one final
--- toggle.select — its refresh loop re-applies every slot's (now
--- correct) texDefault/texSelected regardless of the order restores
--- happened in.
function toggle.restoreSlotIdentity(groupId, btnIdx, name)
    local grp = groups[groupId]
    if not grp or not name then return false end
    local btn = grp.buttons[btnIdx]
    if not btn then return false end
    if btn.name == name then return true end
    if not btn.options then return false end
    for i, opt in ipairs(btn.options) do
        if opt.name == name then
            applyOption(grp, btnIdx, i, true)
            return true
        end
    end
    return false
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
-- Hover (visual feedback)
-----------------------------------------------------------

-- Resolve the hover-highlight overlay for a button or open-popup option
-- sprite handle. Returns nil if the handle isn't one of ours.
local function highlightForHandle(elemHandle)
    for _, grp in pairs(groups) do
        for _, btn in ipairs(grp.buttons) do
            if btn.spriteId == elemHandle then
                return btn.highlightId
            end
        end
        if grp.openPopup then
            for _, opt in ipairs(grp.openPopup.sprites) do
                if opt.spriteId == elemHandle then
                    return opt.highlightId
                end
            end
        end
    end
    return nil
end

function toggle.onHoverEnter(elemHandle)
    local hlId = highlightForHandle(elemHandle)
    if hlId then UI.setVisible(hlId, true) end
end

function toggle.onHoverLeave(elemHandle)
    local hlId = highlightForHandle(elemHandle)
    if hlId then UI.setVisible(hlId, false) end
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

-----------------------------------------------------------
-- Introspection (F3, #645)
-----------------------------------------------------------

-- One entry per slot button (each is independently clickable). Open
-- popup options are transient and not enumerated, same call as
-- dropdown's closed-list dump.
function toggle.dump()
    local out = {}
    for gid, grp in pairs(groups) do
        for i, btn in ipairs(grp.buttons) do
            local info = btn.spriteId and UI.getElementInfo(btn.spriteId) or nil
            if info and info.pageVisible and info.visible then
                table.insert(out, {
                    id = "toggle:" .. gid .. ":" .. i,
                    name = btn.name,
                    type = grp.widgetType,
                    bounds = {
                        x = info and info.x or 0,
                        y = info and info.y or 0,
                        w = info and info.width or grp.size,
                        h = info and info.height or grp.size,
                    },
                    label = btn.tooltip,
                    enabled = info ~= nil and info.clickable,
                    visible = info ~= nil and info.visible,
                    hovered = info ~= nil and info.hovered,
                    focused = info ~= nil and info.focused,
                    value = (i == grp.selectedIndex),
                    screen = info and info.page or nil,
                    handle = info and info.handle or nil,
                })
            end
        end
    end
    return out
end

return toggle
