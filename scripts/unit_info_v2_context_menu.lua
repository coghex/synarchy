-- Unit info v2 right-click context menus (#542 split from
-- unit_info_v2.lua).
--
-- Equip/Unequip/Contents/Store menus for inventory rows, silhouette
-- equipment slots, and accessory rows. All three routed via
-- ui_manager into the handleXxxRightClick functions attached below to
-- the shared unitInfoV2 singleton.

local unitInfoV2 = package.loaded["scripts.unit_info_v2"]
local repairStatus = require("scripts.ui.repair_status")

local M = {}

-- Built storage-capable buildings adjacent (Chebyshev ≤ 1) to the
-- unit. Used by the right-click "Store" path so the menu only offers
-- the player real, reachable targets.
local function findAdjacentStorageBuildings(uid)
    local info = unit.getInfo(uid)
    if not info then return {} end
    local utx = math.floor(info.gridX)
    local uty = math.floor(info.gridY)
    local listStr = building.list()
    if not listStr or listStr == "No buildings placed" then return {} end
    local result = {}
    for id in listStr:gmatch("id=(%d+)") do
        local bid = tonumber(id)
        if bid and building.getActivity(bid) == "built" then
            local cap = building.getStorageCapacity(bid)
            if cap and cap > 0 then
                local binfo = building.getInfo(bid)
                if binfo then
                    local tw = binfo.tileW or 1
                    local th = binfo.tileH or 1
                    local dx, dy = 0, 0
                    if utx < binfo.gridX then
                        dx = binfo.gridX - utx
                    elseif utx >= binfo.gridX + tw then
                        dx = utx - (binfo.gridX + tw - 1)
                    end
                    if uty < binfo.gridY then
                        dy = binfo.gridY - uty
                    elseif uty >= binfo.gridY + th then
                        dy = uty - (binfo.gridY + th - 1)
                    end
                    if math.max(dx, dy) <= 1 then
                        result[#result + 1] = {
                            bid = bid,
                            displayName = binfo.displayName or binfo.defName,
                        }
                    end
                end
            end
        end
    end
    return result
end

-- All slots on the unit's equipment class that accept items of the
-- given kind. Used by the right-click "Equip" path: if exactly one
-- slot matches we equip into it directly; if multiple match we surface
-- a submenu so the player picks. Returns a list of slot tables.
local function findEquipSlotsForKind(uid, itemKind)
    local info = uid and unit.getInfo(uid) or nil
    if not info or not info.equipmentClass then return {} end
    local cls = equipment.getClass(info.equipmentClass)
    if not cls or not cls.slots then return {} end
    local out = {}
    for _, s in ipairs(cls.slots) do
        if s.kind == itemKind then out[#out + 1] = s end
    end
    return out
end

-- For single-slot cases (or when the caller picks a specific slot),
-- choose between "empty matching" and "first matching" — preferring
-- empty when possible. Returns the chosen slot id.
local function preferredEmptySlot(uid, slots)
    local lo = equipment.getLoadout(uid) or {}
    for _, s in ipairs(slots) do
        if not lo[s.id] then return s.id end
    end
    return slots[1] and slots[1].id or nil
end

-- Wrap a repairStatus.menuItem so clicking it also forces an immediate
-- panel rebuild (#303 review): computeInvKey/computeEquipKey already
-- fold in repair priority/claim state so the AI claiming or finishing a
-- job is picked up on the next tick regardless, but a player's own
-- click should redraw the SAME frame — every other mutating menu action
-- below already does this inline.
local function withInvalidate(menuItem)
    if not menuItem then return nil end
    local baseCallback = menuItem.callback
    menuItem.callback = function()
        baseCallback()
        unitInfoV2.lastInvKey   = nil
        unitInfoV2.lastEquipKey = nil
    end
    return menuItem
end

-- Right-click on an inventory row → context menu with Equip / Unequip.
-- Routed via uiManager.onInventoryItemRightClick (set by the row's
-- hit-zone in rebuildInventorySection).
function unitInfoV2.handleInvItemRightClick(elemHandle)
    local row
    for _, r in ipairs(unitInfoV2.invRows) do
        if r.hitId == elemHandle then row = r; break end
    end
    if not row then return false end

    local uid = unitInfoV2.activeUid
    if not uid then return false end
    local item = row.item

    local menuItems = {}
    if item.equipped then
        -- Three flavours of "equipped":
        --  - slot equipment (item.accessoryIndex == nil, item.equippedSlot is a slot id)
        --  - accessory (item.accessoryIndex set; uses unequipAccessory)
        --  - either may be flagged `unequippable` — grey out instead.
        if item.unequippable then
            menuItems[1] = { label = "Unequip", enabled = false }
        elseif item.accessoryIndex then
            local idx = item.accessoryIndex
            menuItems[1] = {
                label    = "Unequip",
                callback = function()
                    equipment.unequipAccessory(uid, idx)
                    unitInfoV2.lastInvKey   = nil
                    unitInfoV2.lastEquipKey = nil
                end,
            }
        else
            menuItems[1] = {
                label    = "Unequip",
                callback = function()
                    equipment.unequip(uid, item.equippedSlot)
                    unitInfoV2.lastInvKey   = nil
                    unitInfoV2.lastEquipKey = nil
                end,
            }
        end
    elseif item.kind == "accessory" then
        -- Accessories don't go in a silhouette slot — they append to
        -- the unit's accessory list. No submenu, no slot matching.
        menuItems[1] = {
            label    = "Equip",
            callback = function()
                equipment.equipAccessory(uid, item.defName, item.instanceId)
                unitInfoV2.lastInvKey   = nil
                unitInfoV2.lastEquipKey = nil
            end,
        }
    else
        local matching = findEquipSlotsForKind(uid, item.kind)
        if #matching == 0 then
            menuItems[1] = {
                label   = "Equip",
                enabled = false,
            }
        elseif #matching == 1 then
            local slotId = matching[1].id
            menuItems[1] = {
                label    = "Equip",
                callback = function()
                    equipment.equip(uid, slotId, item.defName, item.instanceId)
                    unitInfoV2.lastInvKey   = nil
                    unitInfoV2.lastEquipKey = nil
                end,
            }
        else
            -- Multiple matching slots — surface each as a submenu item
            -- so the player picks. Slot display names (e.g. "Weapon
            -- (R)") come from the equipment class YAML.
            local sub = {}
            for _, s in ipairs(matching) do
                sub[#sub + 1] = {
                    label    = s.name,
                    callback = function()
                        equipment.equip(uid, s.id, item.defName, item.instanceId)
                        unitInfoV2.lastInvKey   = nil
                        unitInfoV2.lastEquipKey = nil
                    end,
                }
            end
            menuItems[1] = { label = "Equip", submenu = sub }
        end
    end

    -- Contents: item-containers (first-aid kit / toolbox) open a
    -- floating list of what they hold. Available whether carried or
    -- equipped — you can inspect a kit without unslinging it.
    if item.kind == "container" then
        menuItems[#menuItems + 1] = {
            label    = "Contents",
            callback = function()
                local icp = require("scripts.item_contents_panel")
                local mx, my = engine.getMousePosition()
                local fbW, fbH = engine.getFramebufferSize()
                local ww, wh = engine.getWindowSize()
                if ww and wh and ww > 0 and wh > 0 then
                    mx = mx * (fbW / ww)
                    my = my * (fbH / wh)
                end
                icp.openFor(uid, item.defName, mx, my, item.instanceId)
            end,
        }
    end

    -- Repair queue (#303): flag/unflag this item so the AI's
    -- autonomous repair job (#302) picks it ahead of the unit's other
    -- degraded gear. Absent entirely once a unit has already claimed
    -- the repair (repairStatus.menuItem returns nil).
    local repairMenuItem = withInvalidate(repairStatus.menuItem(item))
    if repairMenuItem then
        menuItems[#menuItems + 1] = repairMenuItem
    end

    -- Storage: append "Store in <cargo>" entries for each adjacent
    -- built cargo. Equipped/accessory items can't be deposited
    -- directly (player must unequip first). The deposit API enforces
    -- capacity; menu entries don't pre-check, so a deposit that
    -- overflows the cargo will return false and the item stays put.
    if not item.equipped then
        for _, c in ipairs(findAdjacentStorageBuildings(uid)) do
            menuItems[#menuItems + 1] = {
                label    = "Store in " .. c.displayName,
                callback = function()
                    unit.depositToCargo(uid, c.bid, item.defName,
                                        item.instanceId)
                    unitInfoV2.lastInvKey = nil
                end,
            }
        end
    end

    -- Open the context menu at the current mouse position, in the
    -- same framebuffer-pixel space the widget expects.
    local contextMenu = require("scripts.ui.context_menu")
    local mx, my = engine.getMousePosition()
    local fbW, fbH = engine.getFramebufferSize()
    local ww, wh = engine.getWindowSize()
    if ww and wh and ww > 0 and wh > 0 then
        mx = mx * (fbW / ww)
        my = my * (fbH / wh)
    end
    contextMenu.show(menuItems, mx, my)
    return true
end

-- Right-click on a silhouette slot (or its equipped icon) → context
-- menu with Equip / Unequip. The Equip path lists matching-kind items
-- from the unit's inventory as a submenu; clicking one calls
-- equipment.equip into THIS slot (the swap-on-equip semantics in the
-- engine handle any currently-equipped item by returning it to the
-- inventory).
function unitInfoV2.handleEquipSlotRightClick(elemHandle)
    local rec
    for _, s in ipairs(unitInfoV2.equipSlots) do
        if s.slotElemId == elemHandle or s.iconElemId == elemHandle then
            rec = s; break
        end
    end
    if not rec then return false end

    local uid = unitInfoV2.activeUid
    if not uid then return false end

    -- Build the inventory-side picklist for "Equip into this slot":
    -- every inventory item whose kind matches this slot's accepted
    -- kind. Equipped items aren't included (they're already in a slot
    -- — the user would unequip first).
    local equipSubmenu = {}
    local inv = unit.getInventory(uid) or {}
    for _, it in ipairs(inv) do
        if it.kind == rec.slot.kind then
            local defName    = it.defName
            local instId     = it.instanceId
            local displayNm  = it.displayName or it.defName
            equipSubmenu[#equipSubmenu + 1] = {
                label    = displayNm,
                icon     = it.iconTex,
                callback = function()
                    equipment.equip(uid, rec.slotId, defName, instId)
                    unitInfoV2.lastInvKey   = nil
                    unitInfoV2.lastEquipKey = nil
                end,
            }
        end
    end

    local menuItems = {}
    if rec.equippedItem then
        menuItems[#menuItems + 1] = {
            label    = "Unequip",
            callback = function()
                equipment.unequip(uid, rec.slotId)
                unitInfoV2.lastInvKey   = nil
                unitInfoV2.lastEquipKey = nil
            end,
        }
    end
    if #equipSubmenu > 0 then
        menuItems[#menuItems + 1] = { label = "Equip", submenu = equipSubmenu }
    else
        menuItems[#menuItems + 1] = {
            label   = "Equip",
            enabled = false,
        }
    end

    -- Repair queue (#303): the equipped item itself, if any.
    if rec.equippedItem then
        local repairMenuItem = withInvalidate(repairStatus.menuItem(rec.equippedItem))
        if repairMenuItem then
            menuItems[#menuItems + 1] = repairMenuItem
        end
    end

    local contextMenu = require("scripts.ui.context_menu")
    local mx, my = engine.getMousePosition()
    local fbW, fbH = engine.getFramebufferSize()
    local ww, wh = engine.getWindowSize()
    if ww and wh and ww > 0 and wh > 0 then
        mx = mx * (fbW / ww)
        my = my * (fbH / wh)
    end
    contextMenu.show(menuItems, mx, my)
    return true
end

-- Right-click on an accessory row icon (right side of equipment
-- section) → Unequip menu. Greyed out for unequippable items.
function unitInfoV2.handleAccessoryRowRightClick(elemHandle)
    local row
    for _, r in ipairs(unitInfoV2.accessoryRows) do
        if r.elemId == elemHandle then row = r; break end
    end
    if not row then return false end
    local uid = unitInfoV2.activeUid
    if not uid then return false end

    local menuItems
    if row.item.unequippable then
        menuItems = { { label = "Unequip", enabled = false } }
    else
        local idx = row.accessoryIndex
        menuItems = { {
            label    = "Unequip",
            callback = function()
                equipment.unequipAccessory(uid, idx)
                unitInfoV2.lastInvKey   = nil
                unitInfoV2.lastEquipKey = nil
            end,
        } }
    end

    -- Repair queue (#303): the accessory item itself.
    local repairMenuItem = withInvalidate(repairStatus.menuItem(row.item))
    if repairMenuItem then
        menuItems[#menuItems + 1] = repairMenuItem
    end

    local contextMenu = require("scripts.ui.context_menu")
    local mx, my = engine.getMousePosition()
    local fbW, fbH = engine.getFramebufferSize()
    local ww, wh = engine.getWindowSize()
    if ww and wh and ww > 0 and wh > 0 then
        mx = mx * (fbW / ww)
        my = my * (fbH / wh)
    end
    contextMenu.show(menuItems, mx, my)
    return true
end

return M
