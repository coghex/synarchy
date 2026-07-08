-- Unit info v2 inventory data prep (#542 split from unit_info_v2.lua).
--
-- Pure data-shaping helpers for the Inventory section: merging
-- inventory + equipped items + accessories into one flat list,
-- collapsing identical items into stacked rows, and the tab/row
-- content-hash keys that let rebuildInventorySection (see
-- unit_info_v2_inventory.lua) skip work when nothing changed.

local repairStatus = require("scripts.ui.repair_status")

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

-- Build the per-row stacking key. Returns nil for equipped items so
-- they never collapse into a stack (each occupies a distinct slot).
-- Non-equipped items only merge when their defName + quality +
-- condition match exactly — a 100% motor and a 99% motor stay on two
-- rows so the player sees the real spread of conditions. currentFill is
-- in the key so two canteens with different fill split into separate
-- rows (the fill is shown per row, and each row targets its own
-- instance — #67). weight is in the key too: raw gems roll a
-- per-instance weight, the row shows weight×stackCount, and the "Store"
-- action targets a representative — so a 0.05 kg and a 0.09 kg garnet
-- must stay on separate rows rather than merge and mis-report their
-- weight. Sharpness is added ONLY for weapons (the only items whose
-- tooltip shows it) so two weapons with differing edge wear likewise
-- split; armor and other gear also carry a combat-mutated iiSharpness
-- but never display it, so splitting their rows on it would be an
-- invisible, confusing reason. Items that DO merge are interchangeable,
-- so acting on the stack's representative instanceId is always correct.
local function stackKey(it)
    if it.equipped then return nil end
    return table.concat({
        it.defName,
        tostring(it.quality     or "_"),
        tostring(it.condition   or "_"),
        tostring(it.currentFill or "_"),
        tostring(it.weight      or "_"),
        it.weapon and tostring(it.sharpness or "_") or "_",
        -- Nested-contents signature: two first-aid kits whose internal
        -- supplies have diverged must NOT merge, so "Contents" / "Store"
        -- act on the kit the player sees (#67A). Empty for non-containers.
        tostring(it.contentsKey or ""),
    }, "|")
end
M.stackKey = stackKey

-- Collapse identical non-equipped entries into single rows tagged
-- with stackCount. The representative instance carries the visible
-- fields (icon, quality, condition); per-tooltip data uses it too.
-- Total-weight aggregation uses the RAW item list rather than this
-- one, so the footer stays accurate.
function M.groupForDisplay(items)
    local groups = {}
    local seen   = {}    -- stackKey → index in groups
    for _, it in ipairs(items) do
        local key = stackKey(it)
        if key and seen[key] then
            groups[seen[key]].stackCount =
                groups[seen[key]].stackCount + 1
        else
            local copy = {}
            for k, v in pairs(it) do copy[k] = v end
            copy.stackCount = 1
            groups[#groups + 1] = copy
            if key then seen[key] = #groups end
        end
    end
    return groups
end

-- Compute the tab strip: "All" first, then per-category in the order
-- categories first appear in the merged list. Each tab carries its
-- count. The All tab counts everything.
function M.computeInvTabs(items)
    local tabs = { { name = "All", count = #items } }
    local seen = { All = true }
    for _, it in ipairs(items) do
        if not seen[it.category] then
            seen[it.category] = true
            tabs[#tabs + 1] = { name = it.category, count = 0 }
        end
    end
    for _, it in ipairs(items) do
        for _, t in ipairs(tabs) do
            if t.name == it.category then
                t.count = t.count + 1
            end
        end
    end
    return tabs
end

-- Stable hash of (uid, activeTab, items signature) so we only rebuild
-- the list when something actually changed. Fill is included so a
-- depleting canteen redraws when its label needs to change, and
-- stackCount so consuming one item from a stack of identical rows
-- (grouped list keeps the same single entry) updates the "×N" label,
-- tab counts, and total-weight footer. condition is included so a
-- wear-degraded item refreshes its "condition: N%" tooltip and its
-- condition<=0 broken overlay: equipped items merged into the "All" view
-- are each their own row (stackKey returns nil for them), so a
-- condition-only drop on equipped armor (gambeson / gloves / boots)
-- wouldn't otherwise change any other field in this hash.
function M.computeInvKey(uid, activeTab, items)
    local parts = { tostring(uid or ""), activeTab or "" }
    for _, it in ipairs(items) do
        parts[#parts + 1] = it.defName .. "/" .. tostring(it.currentFill)
            .. "/" .. (it.equipped and "e" or "i")
            .. "/" .. tostring(it.stackCount or 1)
            .. "/" .. tostring(it.sharpness or 0)
            .. "/" .. tostring(it.weight or 0)
            .. "/" .. tostring(it.condition or 0)
            .. "/" .. tostring(it.contentsKey or "")
            -- Repair priority/claim state (#303 review), same reasoning
            -- as computeEquipKey above.
            .. "/" .. repairStatus.cacheKey(it)
    end
    return table.concat(parts, "|")
end

return M
