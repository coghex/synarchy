-- Building Info Watcher
--
-- Mirrors scripts/unit_info_panel.lua for buildings:
--   * Watches `building.getSelected()` each tick. On change, pushes
--     building info to the HUD info panel under the "building" schema.
--   * Listens to the tile-info broadcast (`onSetInfoText`). When a
--     non-empty tile push arrives — meaning the player clicked a tile
--     with the info tool — we deselect the building so the tile
--     content can take over the panel. Same one-thing-at-a-time rule
--     the unit watcher uses.
--
-- Single-select for now: building.getSelected returns one id (or nil).

local infoPanel = require("scripts.hud.info_panel")

local buildingInfoWatch = {}

buildingInfoWatch.lastSelectedId = nil
buildingInfoWatch.lastWasBuilding = false

-- True if a tile-info push has arrived since we last wrote building
-- content. Suppresses our clear-on-deselect path so the tile content
-- isn't wiped by our follow-up. Same trick as unit_info_panel.
buildingInfoWatch.tilePushed = false

-----------------------------------------------------------
-- Format helpers
-----------------------------------------------------------
local function fmt2(x)
    if x == nil then return "?" end
    return string.format("%.2f", x)
end

local function formatInfo(bid, info)
    local activity = building.getActivity(bid) or "?"
    local footprint = string.format("%dx%d", info.tileW or 1, info.tileH or 1)
    local lines = {}
    table.insert(lines, info.displayName or info.defName or "(unnamed)")
    table.insert(lines, "Activity: " .. activity)
    -- Worker-driven construction progress (only while Appearing and
    -- the def has a non-zero work cost — instant-build defs like the
    -- portal show no progress line).
    if activity == "appearing" then
        local required = building.getBuildRequired(bid)
        local progress = building.getBuildProgress(bid)
        if required and required > 0 and progress then
            local pct = math.floor((progress / required) * 100 + 0.5)
            if pct > 100 then pct = 100 end
            table.insert(lines, string.format("Construction: %d%%", pct))
        end
    end
    table.insert(lines, "At (" .. tostring(info.gridX or "?")
                              .. ", " .. tostring(info.gridY or "?")
                              .. ", " .. tostring(info.gridZ or "?") .. ")")
    table.insert(lines, "Footprint: " .. footprint)
    -- Storage line for any building that can hold cargo. Shown
    -- regardless of construction state so the player sees the cap
    -- they're working toward.
    local cap = building.getStorageCapacity(bid)
    if cap and cap > 0 then
        local used = building.getStorageWeight(bid) or 0
        table.insert(lines, string.format("Storage: %.2f / %.2f kg",
                                          used, cap))
        -- Itemised view lives in the floating cargo inventory popup,
        -- opened via right-click → "Contents" on the cargo.
    end
    return table.concat(lines, "\n")
end

local function pushBuildingInfo(bid)
    local info = building.getInfo(bid)
    if not info then return false end
    infoPanel.setBuildingInfo(formatInfo(bid, info))
    return true
end

-----------------------------------------------------------
-- Init
-----------------------------------------------------------
function buildingInfoWatch.init(scriptId)
    engine.logInfo("Building info watcher initializing...")
end

-----------------------------------------------------------
-- Update (tick interval)
-----------------------------------------------------------
function buildingInfoWatch.update(dt)
    local cur = building.getSelected()

    if cur ~= buildingInfoWatch.lastSelectedId then
        buildingInfoWatch.lastSelectedId = cur
        if cur then
            if pushBuildingInfo(cur) then
                buildingInfoWatch.lastWasBuilding = true
                buildingInfoWatch.tilePushed = false
                -- A selected building takes over the panel — clear
                -- the tile cursor visual so it doesn't compete.
                local hud = require("scripts.hud")
                if hud and hud.worldId then
                    world.clearWorldCursorSelect(hud.worldId)
                end
            end
        else
            -- Selection cleared. Don't wipe the panel if a tile push
            -- already replaced our content (the tile owns the panel
            -- now and our clear would race-wipe it).
            if buildingInfoWatch.lastWasBuilding
               and not buildingInfoWatch.tilePushed then
                infoPanel.setBuildingInfo("")
            end
            buildingInfoWatch.lastWasBuilding = false
        end
    elseif cur then
        -- Same building still selected — refresh so activity/footprint
        -- updates land in the panel (activity transitions Appearing →
        -- Built as the appear anim finishes).
        if pushBuildingInfo(cur) then
            buildingInfoWatch.tilePushed = false
        else
            -- Building was destroyed between ticks.
            buildingInfoWatch.lastSelectedId = nil
            if buildingInfoWatch.lastWasBuilding
               and not buildingInfoWatch.tilePushed then
                infoPanel.setBuildingInfo("")
            end
            buildingInfoWatch.lastWasBuilding = false
        end
    end
end

-----------------------------------------------------------
-- Tile-info broadcast: defer to tile when one is clicked
-----------------------------------------------------------
function buildingInfoWatch.onSetInfoText(basic, advanced)
    if basic and basic ~= "" then
        buildingInfoWatch.tilePushed = true
        if building.getSelected() then
            building.deselect()
        end
    end
end

-----------------------------------------------------------
-- Shutdown
-----------------------------------------------------------
function buildingInfoWatch.shutdown()
    buildingInfoWatch.lastSelectedId = nil
    buildingInfoWatch.lastWasBuilding = false
    engine.logInfo("Building info watcher shut down")
end

return buildingInfoWatch
