-- Unit info v2 inventory data prep (#542 split from unit_info_v2.lua).
--
-- Merges inventory + equipped items + accessories into one flat list.
-- Stacking, category tabs and the rebuild comparison used to live here
-- too; since #1088 they belong to the shared item-list widget
-- (scripts/ui/item_list.lua), which the cargo and item-contents popups
-- share — this module supplies the raw item set and nothing else.

local M = {}

-- Pull inventory + equipment, tag each entry with equipped state, and
-- return a flat list in insertion order (inventory first, then
-- equipped slots in EquipmentClass slot order so the player sees a
-- stable layout). Each entry: { defName, displayName, weight,
--                               category, kind, iconTex, currentFill,
--                               equipped, equippedSlot }
function M.collectInventoryAndEquipment(uid)
    local inv = unit.getInventory(uid) or {}
    local lo  = equipment.getLoadout(uid)  or {}
    local out = {}
    for _, it in ipairs(inv) do
        out[#out + 1] = {
            defName      = it.defName,
            instanceId   = it.instanceId,
            contentsKey  = it.contentsKey,
            displayName  = it.displayName or it.defName,
            weight       = it.weight or 0,
            category     = it.category or "Misc",
            kind         = it.kind or "misc",
            make         = it.make or "",
            material     = it.material or "",
            iconTex      = it.iconTex,
            currentFill  = it.currentFill or 0,
            capacity     = it.capacity,
            quality      = it.quality,
            qualityTier  = it.qualityTier,
            condition    = it.condition,
            weapon       = it.weapon,
            sharpness    = it.sharpness,
            buffs        = it.buffs,
            unequippable = it.unequippable,
            equipped     = false,
        }
    end
    -- Walk equipment in the unit's class slot order so the equipped
    -- block reads predictably (helmet, armor, gauntlets, weapons, …).
    local info = unit.getInfo(uid)
    local cls  = info and info.equipmentClass
                 and equipment.getClass(info.equipmentClass) or nil
    local slotIds = {}
    if cls and cls.slots then
        for _, s in ipairs(cls.slots) do
            slotIds[#slotIds + 1] = s.id
        end
    end
    for _, slotId in ipairs(slotIds) do
        local it = lo[slotId]
        if it then
            out[#out + 1] = {
                defName       = it.defName,
                instanceId    = it.instanceId,
                contentsKey   = it.contentsKey,
                displayName   = it.displayName or it.defName,
                weight        = it.weight or 0,
                category      = it.category or "Misc",
                kind          = it.kind or "misc",
                make          = it.make or "",
                material      = it.material or "",
                iconTex       = it.iconTex,
                currentFill   = it.currentFill or 0,
                capacity      = it.capacity,
                quality       = it.quality,
                qualityTier   = it.qualityTier,
                condition     = it.condition,
                weapon        = it.weapon,
                sharpness     = it.sharpness,
                equipped      = true,
                equippedSlot  = slotId,
            }
        end
    end
    -- Accessories — worn items that don't sit on the silhouette.
    -- Each carries its 1-based index (for unequipAccessory).
    local accs = equipment.getAccessories(uid) or {}
    for i, it in ipairs(accs) do
        out[#out + 1] = {
            defName        = it.defName,
            instanceId     = it.instanceId,
            contentsKey    = it.contentsKey,
            displayName    = it.displayName or it.defName,
            weight         = it.weight or 0,
            category       = it.category or "Misc",
            kind           = it.kind or "misc",
            make           = it.make or "",
            material       = it.material or "",
            iconTex        = it.iconTex,
            currentFill    = it.currentFill or 0,
            capacity       = it.capacity,
            quality        = it.quality,
            qualityTier    = it.qualityTier,
            condition      = it.condition,
            weapon         = it.weapon,
            sharpness      = it.sharpness,
            buffs          = it.buffs,
            unequippable   = it.unequippable,
            equipped       = true,
            equippedSlot   = "(worn)",
            accessoryIndex = i,
        }
    end
    return out
end

return M
