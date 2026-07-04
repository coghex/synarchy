-- Shared quality-tier display helper (#345).
--
-- Any item whose def declares a quality spec resolves its numeric
-- iiQuality (0..100) to a named band on the engine side
-- (Item.Types.qualityTierLabel — "excellent" / "good" / "average" /
-- "bad" / "atrocious" by default, or a def's own `quality_tiers:`
-- override). It arrives on item tables as `qualityTier`, alongside
-- `quality`, only when the def has a spec (unit.getInventory,
-- equipment.getLoadout/getAccessories, building.getStorage,
-- item.listGround). This module is the one place that turns that
-- into display text, so every panel reads the same suffix.

local qualityTier = {}

-- " (excellent)" appended to an item's name, or "" when the item has
-- no tier (no quality spec on the def).
function qualityTier.suffix(it)
    if it and it.qualityTier and it.qualityTier ~= "" then
        return " (" .. it.qualityTier .. ")"
    end
    return ""
end

-- `baseName` with the tier suffix appended — "coffee" at 95% reads
-- "coffee (excellent)".
function qualityTier.withSuffix(baseName, it)
    return (baseName or "") .. qualityTier.suffix(it)
end

return qualityTier
