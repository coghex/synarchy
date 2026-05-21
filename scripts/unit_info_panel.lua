-- Unit Info Watcher
--
-- The "info panel" is shared with the tile-info system (the HUD's
-- info_panel module). Whoever has the most recent selection — unit OR
-- tile — owns the panel's content. This module is the unit half of
-- that contract:
--
--   * Watches `unit.getSelected()` each tick. On selection change, it
--     writes the unit's attributes into the HUD info panel and clears
--     any tile cursor selection (so the tile cursor visual goes away).
--   * Subscribes to the tile-info broadcast (`onSetInfoText`). When a
--     non-empty tile-info push arrives — meaning the player selected a
--     tile — we deselect any active unit. This is the "click a tile
--     deselects the unit" rule.
--
-- Multi-unit selection: the first selected unit's info is shown. Future
-- work: a row of portraits + summary.

local infoPanel = require("scripts.hud.info_panel")
local hud       = require("scripts.hud")
local stats     = require("scripts.unit_stats")

local unitInfoWatch = {}

unitInfoWatch.lastSelectedId = nil
unitInfoWatch.lastWasUnit    = false  -- did we last push unit info?

-- When unit_info_v2 is enabled it owns the unit-info display. We skip
-- our push path so the shared HUD info panel doesn't compete. The new
-- module sets this flag during its init.
unitInfoWatch.suppressed = package.loaded.__unit_info_v2_suppress or false

-- True if a non-empty tile-info push has arrived since we last wrote
-- unit content. Used to suppress our deselect-clear when a tile click
-- is the cause of deselection (the tile-info push has already taken
-- ownership of the panel; we'd be wiping the tile info if we cleared).
-- Reset whenever we write unit content; set in onSetInfoText.
unitInfoWatch.tilePushed = false

-----------------------------------------------------------
-- Formatters
-----------------------------------------------------------
-- Round-to-2-decimals for display. Returns "?" for nil so the layout
-- doesn't shift when a value isn't available yet (e.g. a unit type
-- that doesn't define endurance).
local function fmt2(x)
    if x == nil then return "?" end
    return string.format("%.2f", x)
end

local function formatStatus(uid, info)
    local gx = fmt2(info.gridX)
    local gy = fmt2(info.gridY)
    local gz = info.gridZ or 0
    local activity = unit.getActivity(uid) or "?"

    -- Body composition: height is fixed at spawn; body_mass /
    -- lean_mass (skeletal muscle) / fat_mass are live and shrink under
    -- Phase 4 catabolism. The wasting visibly cascades to strength /
    -- max_hydration / carrying_capacity via the engine-side
    -- recomputeBodyDerivedStats, but those show up under Physical etc.
    local height   = unit.getStat(uid, "height")
    local bodyMass = unit.getStat(uid, "body_mass")
    local leanMass = unit.getStat(uid, "lean_mass")
    local fatMass  = unit.getStat(uid, "fat_mass")
    local heightLine   = "Height: ?"
    local bodyMassLine = "Body Mass: ?"
    local leanMassLine = "Lean Mass: ?"
    local fatMassLine  = "Fat Mass: ?"
    if height   then heightLine   = string.format("Height: %s m",      fmt2(height))   end
    if bodyMass then bodyMassLine = string.format("Body Mass: %s kg", fmt2(bodyMass)) end
    if leanMass then leanMassLine = string.format("Lean Mass: %s kg", fmt2(leanMass)) end
    if fatMass  then fatMassLine  = string.format("Fat Mass: %s kg",  fmt2(fatMass))  end

    -- Resources: cur / max display.
    local maxStam  = stats.get(uid, "max_stamina")
    local curStam  = unit.getStat(uid, "stamina")
    local staminaLine = "Stamina: ?"
    if curStam and maxStam then
        staminaLine = string.format("Stamina: %s / %s",
                                    fmt2(curStam), fmt2(maxStam))
    end

    local maxHydr = stats.get(uid, "max_hydration")
    local curHydr = unit.getStat(uid, "hydration")
    local hydrationLine = "Hydration: ?"
    if curHydr and maxHydr then
        hydrationLine = string.format("Hydration: %s / %s",
                                      fmt2(curHydr), fmt2(maxHydr))
    end

    local maxHun = stats.get(uid, "max_hunger")
    local curHun = unit.getStat(uid, "hunger")
    local hungerLine = "Hunger: ?"
    if curHun and maxHun then
        hungerLine = string.format("Hunger: %s / %s",
                                   fmt2(curHun), fmt2(maxHun))
    end

    return (info.defName or "(unnamed)")
        .. "\nActivity: " .. activity
        .. "\nFacing: " .. (info.facing or "?")
        .. "\nAt (" .. gx .. ", " .. gy .. ", " .. tostring(gz) .. ")"
        .. "\n" .. heightLine
        .. "\n" .. bodyMassLine
        .. "\n" .. leanMassLine
        .. "\n" .. fatMassLine
        .. "\n" .. staminaLine
        .. "\n" .. hydrationLine
        .. "\n" .. hungerLine
end

local function formatPhysical(uid)
    local lines = {}
    local order = { "strength", "endurance", "reflexes", "constitution" }
    for _, name in ipairs(order) do
        local v = unit.getStat(uid, name)
        if v ~= nil then
            table.insert(lines,
                name:sub(1,1):upper() .. name:sub(2) .. ": " .. fmt2(v))
        end
    end
    if #lines == 0 then return "(none)" end
    return table.concat(lines, "\n")
end

local function formatMental(uid)
    local lines = {}
    local order = { "perception" }
    for _, name in ipairs(order) do
        local v = unit.getStat(uid, name)
        if v ~= nil then
            table.insert(lines,
                name:sub(1,1):upper() .. name:sub(2) .. ": " .. fmt2(v))
        end
    end
    if #lines == 0 then return "(none)" end
    return table.concat(lines, "\n")
end

local function formatSkills(uid)
    local all = unit.getAllSkills(uid)
    if not all then return "(none)" end
    local lines = {}
    -- Iterate in name order for stable display.
    local names = {}
    for name, _ in pairs(all) do table.insert(names, name) end
    table.sort(names)
    for _, name in ipairs(names) do
        local s = all[name]
        local label = name:sub(1,1):upper() .. name:sub(2)
        table.insert(lines, string.format("%s: %s", label, fmt2(s.level)))
    end
    if #lines == 0 then return "(none)" end
    return table.concat(lines, "\n")
end

local function formatInventory(uid)
    local items = unit.getInventory(uid)
    if not items or #items == 0 then return "(empty)" end
    local lines = {}
    for _, it in ipairs(items) do
        if it.capacity then
            -- Container line: "Stainless Steel Canteen (2L): 2.00 / 2.00 L water"
            table.insert(lines, string.format("%s: %s / %s L %s",
                it.displayName, fmt2(it.currentFill),
                fmt2(it.capacity), it.holds or ""))
        else
            table.insert(lines, it.displayName)
        end
    end
    return table.concat(lines, "\n")
end

local function pushUnitInfo(uid)
    local info = unit.getInfo(uid)
    if not info then return false end
    infoPanel.setUnitInfo(
        formatStatus(uid, info),
        formatPhysical(uid),
        formatMental(uid),
        formatSkills(uid),
        formatInventory(uid))
    return true
end

-----------------------------------------------------------
-- Init
-----------------------------------------------------------
function unitInfoWatch.init(scriptId)
    engine.logInfo("Unit info watcher initializing...")
    -- The HUD's info panel is created by hud.createUI() in its own
    -- lifecycle; we never create it. We just call setText on it once
    -- it exists. setText is safe to call before the panel is built —
    -- it just stores the text and shows on next create.
end

-----------------------------------------------------------
-- Update (called at tick interval)
-----------------------------------------------------------
function unitInfoWatch.update(dt)
    if unitInfoWatch.suppressed then return end
    local sel = unit.getSelected()
    local cur = sel and sel[1] or nil

    if cur ~= unitInfoWatch.lastSelectedId then
        unitInfoWatch.lastSelectedId = cur
        if cur then
            -- New unit just got selected. Push its info — this swaps
            -- the panel to the "unit" schema, overriding any tile
            -- content. Clear the tile cursor visual so it doesn't
            -- compete.
            if pushUnitInfo(cur) then
                unitInfoWatch.lastWasUnit = true
                unitInfoWatch.tilePushed = false
                if hud and hud.worldId then
                    world.clearWorldCursorSelect(hud.worldId)
                end
            end
        else
            -- Selection just got cleared. Two cases:
            --   (a) Cleared because the user clicked a tile — the
            --       tile-info push will populate the panel; we must
            --       NOT wipe it. tilePushed will be true if that push
            --       already arrived, OR the watcher ran first and the
            --       push is in flight (see else-branch logic below).
            --   (b) Cleared via ESC or unit.destroy — no tile push is
            --       coming, so we should wipe our stale unit content.
            if unitInfoWatch.lastWasUnit and not unitInfoWatch.tilePushed then
                infoPanel.setUnitInfo("", "", "", "")
            end
            unitInfoWatch.lastWasUnit = false
        end
    elseif cur then
        -- Same unit selected as last tick. Refresh content so the
        -- panel reflects movement (gridX/gridY change as units walk)
        -- and live values like stamina that tick over time.
        if pushUnitInfo(cur) then
            unitInfoWatch.tilePushed = false  -- we own the panel again
        else
            -- Unit was destroyed between ticks; reset.
            unitInfoWatch.lastSelectedId = nil
            if unitInfoWatch.lastWasUnit and not unitInfoWatch.tilePushed then
                infoPanel.setUnitInfo("", "", "", "")
            end
            unitInfoWatch.lastWasUnit = false
        end
    end
end

-----------------------------------------------------------
-- Hook: tile info broadcast arrives
-----------------------------------------------------------
-- The HUD's tile-info poller pushes `onSetInfoText` every time the
-- world cursor's selected tile changes. We use this as the signal
-- that "a tile was just selected" — if a unit was also selected, the
-- user's most recent intent is the tile, so we deselect the unit.
--
-- An empty payload means "nothing selected" (e.g. clicking empty
-- space, or our own clearWorldCursorSelect call). We ignore those.
function unitInfoWatch.onSetInfoText(basic, advanced)
    if basic and basic ~= "" then
        -- A tile is now the active selection. Two effects:
        --   1. If a unit was selected, defer to the tile and deselect.
        --   2. Mark the panel as owned by the tile-info path. The next
        --      update() will see this flag and skip the clear-on-
        --      deselect (otherwise we'd race-wipe the tile content
        --      that just landed in the panel).
        unitInfoWatch.tilePushed = true
        if #unit.getSelected() > 0 then
            unit.deselectAll()
        end
    end
end

-----------------------------------------------------------
-- Shutdown
-----------------------------------------------------------
function unitInfoWatch.shutdown()
    -- Don't touch the panel — hud owns its lifecycle. Just reset our state.
    unitInfoWatch.lastSelectedId = nil
    unitInfoWatch.lastWasUnit    = false
    engine.logInfo("Unit info watcher shut down")
end

return unitInfoWatch
