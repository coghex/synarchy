-- Ground-Item Info Watcher
--
-- Mirrors scripts/building_info_panel.lua for items lying in the
-- world:
--   * Watches `item.getSelected()` each tick. On change, pushes item
--     info to the HUD info panel under the "grounditem" schema (the
--     white selection outline is engine-side, driven by the same
--     selection).
--   * Listens to the tile-info broadcast (`onSetInfoText`): a
--     non-empty tile push means the player clicked a tile with the
--     info tool — deselect the item so the tile owns the panel.
--     Same one-thing-at-a-time rule as units/buildings.

local infoPanel = require("scripts.hud.info_panel")

local itemInfoWatch = {}

itemInfoWatch.lastSelectedId = nil
itemInfoWatch.lastWasItem = false
itemInfoWatch.tilePushed = false

local function fmt1(x)
    if x == nil then return "?" end
    return string.format("%.1f", x)
end

-- Find the ground entry + def info for a gid. Returns nil when the
-- item no longer exists (picked up / removed).
local function groundEntry(gid)
    local l = item.listGround() or {}
    for _, g in ipairs(l) do
        if g.id == gid then return g end
    end
    return nil
end

local function defEntry(defName)
    local defs = item.listDefs() or {}
    for _, d in ipairs(defs) do
        if d.name == defName then return d end
    end
    return nil
end

local function pushItemInfo(gid)
    local g = groundEntry(gid)
    if not g then return false end
    local d = defEntry(g.defName)
    local lines = {}
    local nm = (d and d.displayName) or g.defName
    -- Condition 0 = broken (see Combat.Resolution weapon wear).
    if g.condition and g.condition <= 0 then nm = nm .. " (broken)" end
    table.insert(lines, nm)
    if d then
        table.insert(lines, "Category: " .. (d.category or "?"))
    end
    -- Live total mass from listGround (itemTotalWeight: empty weight +
    -- fill + nested contents), so filled containers and stocked kits
    -- show their real mass — not the static definition weight (d.weight).
    table.insert(lines, "Weight: " .. fmt1(g.weight) .. " kg")
    table.insert(lines, "Quality: " .. fmt1(g.quality))
    table.insert(lines, "Condition: " .. fmt1(g.condition))
    if g.fill and g.fill > 0 then
        table.insert(lines, "Fill: " .. fmt1(g.fill) .. " L")
    end
    table.insert(lines, string.format("At (%.1f, %.1f)", g.x or 0, g.y or 0))
    infoPanel.setGroundItemInfo(table.concat(lines, "\n"))
    return true
end

function itemInfoWatch.update(dt)
    local cur = item.getSelected()

    if cur ~= itemInfoWatch.lastSelectedId then
        itemInfoWatch.lastSelectedId = cur
        if cur then
            if pushItemInfo(cur) then
                itemInfoWatch.lastWasItem = true
                itemInfoWatch.tilePushed = false
                local hud = require("scripts.hud")
                if hud and hud.worldId then
                    world.clearWorldCursorSelect(hud.worldId)
                end
            end
        else
            if itemInfoWatch.lastWasItem
               and not itemInfoWatch.tilePushed then
                infoPanel.setGroundItemInfo("")
            end
            itemInfoWatch.lastWasItem = false
        end
    elseif cur then
        -- Same item still selected — refresh (it may have been picked
        -- up or removed between ticks).
        if not pushItemInfo(cur) then
            item.deselect()
            itemInfoWatch.lastSelectedId = nil
            if itemInfoWatch.lastWasItem
               and not itemInfoWatch.tilePushed then
                infoPanel.setGroundItemInfo("")
            end
            itemInfoWatch.lastWasItem = false
        end
    end
end

-- Tile-info broadcast: defer to the tile when one is clicked.
function itemInfoWatch.onSetInfoText(basic, advanced)
    if basic and basic ~= "" then
        itemInfoWatch.tilePushed = true
        if item.getSelected() then
            item.deselect()
            itemInfoWatch.lastSelectedId = nil
            itemInfoWatch.lastWasItem = false
        end
    end
end

package.loaded["scripts.item_info_panel"] = itemInfoWatch
return itemInfoWatch
