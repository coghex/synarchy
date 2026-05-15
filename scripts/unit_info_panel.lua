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

local unitInfoWatch = {}

unitInfoWatch.lastSelectedId = nil
unitInfoWatch.lastWasUnit    = false  -- did we last push unit info?

-- True if a non-empty tile-info push has arrived since we last wrote
-- unit content. Used to suppress our deselect-clear when a tile click
-- is the cause of deselection (the tile-info push has already taken
-- ownership of the panel; we'd be wiping the tile info if we cleared).
-- Reset whenever we write unit content; set in onSetInfoText.
unitInfoWatch.tilePushed = false

-----------------------------------------------------------
-- Formatters
-----------------------------------------------------------
local function formatBasic(info)
    local gx = math.floor((info.gridX or 0) * 10 + 0.5) / 10
    local gy = math.floor((info.gridY or 0) * 10 + 0.5) / 10
    local gz = info.gridZ or 0
    return (info.defName or "(unnamed)")
        .. "\nat (" .. tostring(gx) .. ", " .. tostring(gy)
        .. ", " .. tostring(gz) .. ")"
        .. "\nfacing " .. (info.facing or "?")
end

local function formatAdvanced(info)
    return "baseWidth: " .. tostring(info.baseWidth or 0)
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
    local sel = unit.getSelected()
    local cur = sel and sel[1] or nil

    if cur ~= unitInfoWatch.lastSelectedId then
        unitInfoWatch.lastSelectedId = cur
        if cur then
            -- New unit just got selected. Push its info, override
            -- any tile content in the panel, and clear the tile
            -- cursor visual so it doesn't compete.
            local info = unit.getInfo(cur)
            if info then
                infoPanel.setInfo(formatBasic(info), formatAdvanced(info))
                unitInfoWatch.lastWasUnit = true
                -- Our write replaces any tile content in the panel,
                -- so reset the flag — a tile push from this point
                -- forward is a new fact.
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
                infoPanel.setInfo("", "")
            end
            unitInfoWatch.lastWasUnit = false
        end
    elseif cur then
        -- Same unit selected as last tick. Refresh content so the
        -- panel reflects movement (gridX/gridY change as units walk).
        local info = unit.getInfo(cur)
        if info then
            infoPanel.setInfo(formatBasic(info), formatAdvanced(info))
            unitInfoWatch.tilePushed = false  -- we own the panel again
        else
            -- Unit was destroyed between ticks; reset.
            unitInfoWatch.lastSelectedId = nil
            if unitInfoWatch.lastWasUnit and not unitInfoWatch.tilePushed then
                infoPanel.setInfo("", "")
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
