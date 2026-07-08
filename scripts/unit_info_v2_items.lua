-- Unit info v2 item tooltip/overlay helpers (#542 split from
-- unit_info_v2.lua).
--
-- Shared between the Equipment section (silhouette slots + accessory
-- list) and the Inventory section (item rows): the rich item hover
-- hint, the buff-stat capitalizer it uses, and the broken-equipment
-- overlay sprite.

local qualityTier   = require("scripts.ui.quality_tier")
local repairStatus  = require("scripts.ui.repair_status")
local brokenOverlay = require("scripts.ui.broken_overlay")

local M = {}

-- Capitalize the first letter of a stat name for display
-- ("perception" → "Perception"). Used by the buff line and the stats
-- panel.
local function capitalizeStat(name)
    if not name or #name == 0 then return name or "" end
    return name:sub(1, 1):upper() .. name:sub(2)
end

-- Effective buff amount given the item's condition. For buffs with
-- `scalesWithCondition`, the amount is multiplied by condition/100;
-- otherwise it's the flat amount.
local function effectiveBuffAmount(buff, condition)
    local amt = buff.amount or 0
    if buff.scalesWithCondition then
        local cond = condition or 100
        return amt * (cond / 100)
    end
    return amt
end
M.effectiveBuffAmount = effectiveBuffAmount

-- Build the rich tooltip hint shown for an item — same content for
-- inventory rows AND silhouette slot icons. `it` accepts both shapes
-- (from unit.getInventory / equipment.getLoadout / getAccessories);
-- equippedSlot is the slot id string when the item lives in a slot,
-- "(worn)" for accessories, nil for inventory items.
function M.buildItemHint(it, equippedSlot)
    local hintLines = { string.format("%.2f kg", it.weight or 0) }
    if it.make and it.make ~= "" then
        hintLines[#hintLines + 1] = "make: " .. it.make
    end
    if it.material and it.material ~= "" then
        hintLines[#hintLines + 1] = "material: " .. it.material
    end
    -- Container fill: shown for items that have a capacity (canteens
    -- etc.). Format is "(currentFill/capacity unit)" where unit
    -- defaults to L for fluids — fits most starting items.
    if it.capacity and it.capacity > 0 then
        hintLines[#hintLines + 1] = string.format("(%.1f/%.1f L)",
            it.currentFill or 0, it.capacity)
    end
    if it.quality then
        hintLines[#hintLines + 1] =
            string.format("quality: %d%%", math.floor(it.quality + 0.5))
            .. qualityTier.suffix(it)
    end
    if it.condition then
        hintLines[#hintLines + 1] =
            string.format("condition: %d%%", math.floor(it.condition + 0.5))
    end
    if it.weapon then
        -- Live effective sharpness on the def's engineering scale
        -- (lower = sharper), so distinct weapons stay distinct and a
        -- worn edge reads correctly. `it.sharpness` is the instance's
        -- 0..100 edge-wear % (100 = factory edge); mirror combat's
        -- derivation: effective = base * 100/wear (wear clamped 10..100,
        -- matching Combat.Resolution). Falls back to the raw base when
        -- the backend didn't supply the instance wear value.
        local base = it.weapon.baseSharpness or 0
        local wear = it.sharpness
        local sharp = base
        if wear ~= nil then
            -- Clamp 10..100 like Combat.Resolution, so a fully-dulled
            -- edge (wear 0) reads as the dullest case (base * 10), not a
            -- pristine fallback. Only a missing field falls back to base.
            local w = math.max(10, math.min(100, wear))
            sharp = base * (100.0 / w)
        end
        hintLines[#hintLines + 1] = string.format(
            "length %.0fcm  ·  sharpness %d",
            it.weapon.bladeLength or 0,
            math.floor(sharp + 0.5))
        hintLines[#hintLines + 1] = string.format(
            "stab %.2f  ·  slash %.2f  ·  blunt %.2f",
            it.weapon.stabEffectiveness or 0,
            it.weapon.slashEffectiveness or 0,
            it.weapon.bluntEffectiveness or 0)
    end
    -- Repair designation/queue state (#303): "in progress" once a unit
    -- has claimed the repair job, else "queued (priority)" once the
    -- player has flagged it via the row's context menu.
    local repairHint = repairStatus.hintLine(it)
    if repairHint then
        hintLines[#hintLines + 1] = repairHint
    end
    if it.buffs then
        for _, b in ipairs(it.buffs) do
            -- "Perception + 1", "Perception + 10%", or both:
            -- "Perception + 1 + 10%". Percent arrives fractional
            -- (0.1 = +10%), same convention as unit modifiers.
            local parts = {}
            if (b.amount or 0) ~= 0 then
                parts[#parts + 1] = string.format("%g", b.amount)
            end
            if (b.percent or 0) ~= 0 then
                parts[#parts + 1] = string.format("%g%%", b.percent * 100)
            end
            if #parts == 0 then parts[1] = "0" end
            local line = capitalizeStat(b.stat)
                .. " + " .. table.concat(parts, " + ")
            if b.scalesWithCondition and it.condition then
                line = line .. string.format(" (x%.2f)", it.condition / 100)
            end
            hintLines[#hintLines + 1] = line
        end
    end
    if equippedSlot then
        hintLines[#hintLines + 1] = "equipped: " .. equippedSlot
    end
    return table.concat(hintLines, "\n")
end

-- Overlay broken_equipment.png over an item icon at (x,y,w,h) when the
-- item is broken (condition 0). z should sit just above the icon. The
-- overlay sprite is tracked in `track` so it tears down with the panel.
-- Cache + draw live in scripts.ui.broken_overlay (shared with the cargo
-- and item-container contents panels).
function M.addBrokenOverlay(unitInfoV2, it, name, x, y, w, h, z, track)
    local oid = brokenOverlay.add(unitInfoV2.page, name,
                                  it and it.condition, x, y, w, h, z)
    if oid then table.insert(track, { kind = "sprite", id = oid }) end
end

return M
