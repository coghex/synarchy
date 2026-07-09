-- Right-click context-menu construction for scripts/init.lua (#543).
-- Each try*Menu function hit-tests its own target kind, builds the
-- menu items for it, shows the menu, and returns true if it claimed
-- the click — mirrors the tool handleMouseDown claim convention.
local M = {}

-- Storage / work-station building right-click → "Contents" / "Bills"
-- menu, regardless of unit selection. Move commands still work on
-- non-cargo, non-station tiles. building.hitTestAt takes framebuffer
-- pixel coords via the same conversion the tile-menu function uses.
-- A single building can offer BOTH (a workshop with cargo storage),
-- so this hit-tests once and builds one combined menu instead of two
-- competing ones.
function M.tryBuildingMenu(x, y)
    local hitBid = building.hitTestAt(x, y)
    if not hitBid then return false end

    local activity = building.getActivity(hitBid)
    local cap = building.getStorageCapacity(hitBid)
    local ops = building.getOperations(hitBid)
    local hasStorage = cap and cap > 0 and activity == "built"
    -- Bills can be queued on an under-construction station
    -- (craft.addBill only refuses an unbuilt bid if the
    -- station doesn't exist at all — the craft AI simply
    -- won't work it until Built), so this doesn't gate on
    -- activity the way Contents does.
    local hasStation = ops and #ops > 0
    if not (hasStorage or hasStation) then return false end

    local fbW, fbH = engine.getFramebufferSize()
    local ww, wh   = engine.getWindowSize()
    local mx, my   = x, y
    if ww and wh and ww > 0 and wh > 0 then
        mx = x * (fbW / ww)
        my = y * (fbH / wh)
    end
    local contextMenu = require("scripts.ui.context_menu")
    local items = {}
    if hasStorage then
        local cargoPanel =
            require("scripts.cargo_inventory_panel")
        table.insert(items, { label = "Contents",
            callback = function()
                cargoPanel.openFor(hitBid, mx, my)
            end })
    end
    if hasStation then
        local craftingPanel =
            require("scripts.crafting_panel")
        table.insert(items, { label = "Bills",
            callback = function()
                craftingPanel.show(hitBid)
            end })
    end
    contextMenu.show(items, mx, my)
    return true
end

-- Right-click on a unit → Info / Attack context menu. Callers run
-- this before the move-when-selected branch so right-clicking a unit
-- doesn't get interpreted as a move-to-tile order. "Attack" only
-- appears when at least one player-faction unit is selected; it's
-- greyed when every player attacker shares the target's faction (no
-- friendly-fire by default — the force-attack override lands in a
-- later phase).
function M.tryUnitMenu(x, y)
    local targetUid = unit.hitTestAt(x, y)
    if not targetUid then return false end

    local fbW, fbH = engine.getFramebufferSize()
    local ww, wh   = engine.getWindowSize()
    local mx, my   = x, y
    if ww and wh and ww > 0 and wh > 0 then
        mx = x * (fbW / ww)
        my = y * (fbH / wh)
    end
    local contextMenu = require("scripts.ui.context_menu")
    local selectedUids = unit.getSelected() or {}
    local targetFac    = unit.getFaction(targetUid)
    local items = {
        { label = "Info",
          callback = function()
              unit.select(targetUid)
              -- Mirror the left-click unit-selection path:
              -- selecting a unit takes over the info panel,
              -- so clear any building/item selection to keep
              -- the shared HUD panel from flickering schemas.
              building.deselect()
              item.deselect()
          end },
    }
    -- Filter selection down to player-commandable
    -- attackers (faction "player" or "debug"), excluding
    -- the target itself.
    local attackers = {}
    for _, uid in ipairs(selectedUids) do
        local fac = unit.getFaction(uid)
        if uid ~= targetUid
           and (fac == "player" or fac == "debug") then
            table.insert(attackers, uid)
        end
    end
    if #attackers > 0 then
        -- Friendly check: every attacker shares the
        -- target's faction → Attack greyed. Debug-faction
        -- attackers bypass this check entirely (the whole
        -- point of "debug" is no friendly-fire restriction
        -- so the player can stage acolyte-vs-acolyte
        -- fights in the debug overlay).
        local allFriendly = true
        for _, uid in ipairs(attackers) do
            local fac = unit.getFaction(uid)
            if fac == "debug" or fac ~= targetFac then
                allFriendly = false
                break
            end
        end
        local unitAi = require("scripts.unit_ai")
        table.insert(items, {
            label    = "Attack",
            enabled  = not allFriendly,
            callback = function()
                -- Player order → committed (holds far longer
                -- before futility breaks it; soft, not absolute).
                for _, uid in ipairs(attackers) do
                    unitAi.commandAttack(uid, targetUid, true)
                end
            end,
        })
    end
    -- Treat bleeding: a selected unit that KNOWS bleed-control
    -- dresses the target's worst bleeding wound, drawing
    -- bandages from a first-aid kit carried by the medic OR
    -- any other selected unit (e.g. the technomule standing
    -- by). Greyed until both a kit and a bleeding wound exist.
    do
        local medic
        for _, uid in ipairs(selectedUids) do
            if uid ~= targetUid
               and unit.getKnowledge(uid, "bleed_control") then
                medic = uid; break
            end
        end
        if medic then
            local function hasBandages(uid)
                for _, it in ipairs(unit.getInventory(uid) or {}) do
                    if it.kind == "container" then
                        for _, r in ipairs(unit.getItemContents(
                                      uid, it.defName) or {}) do
                            if r.defName == "bandage"
                               and (r.count or 0) > 0 then
                                return true
                            end
                        end
                    end
                end
                return false
            end
            local kitOwner
            if hasBandages(medic) then
                kitOwner = medic
            else
                for _, uid in ipairs(selectedUids) do
                    if hasBandages(uid) then kitOwner = uid; break end
                end
            end
            local bleeding = false
            for _, w in ipairs(unit.getWounds(targetUid) or {}) do
                if (w.bandage or 1) > 0.02
                   and w.kind ~= "concussion" then
                    bleeding = true; break
                end
            end
            table.insert(items, {
                label   = "Treat bleeding",
                enabled = (kitOwner ~= nil) and bleeding,
                callback = function()
                    local res = unit.treatBleeding(
                        medic, targetUid, kitOwner)
                    if res then
                        local msg, cat
                        if res.ok then
                            local pct = math.floor(
                                (res.seep or 0) * 100 + 0.5)
                            msg = (pct <= 0)
                                and "Bleeding stopped"
                                or string.format(
                                    "Bleeding cut to %d%%", pct)
                            if (res.bandagesUsed or 1) > 1 then
                                msg = msg .. string.format(
                                    " (%d bandages used)",
                                    res.bandagesUsed)
                            end
                            cat = "unit_event"
                        else
                            msg = "Treatment failed: "
                                  .. (res.message or "")
                            cat = "unit_warning"   -- red; a failed job
                        end
                        -- Tag the patient so it shows in their Log.
                        engine.emitEventForUnit(cat, msg, targetUid)
                    end
                end,
            })
            -- Treat infection: administer antibiotics (the CURE)
            -- to an infected wound. Greyed until the target has an
            -- infected wound AND a kit with antibiotics is on the
            -- medic or another selected unit.
            local function hasAntibiotics(uid)
                for _, it in ipairs(unit.getInventory(uid) or {}) do
                    if it.kind == "container" then
                        for _, r in ipairs(unit.getItemContents(
                                      uid, it.defName) or {}) do
                            if r.defName == "antibiotics"
                               and (r.fill or 0) > 0 then
                                return true
                            end
                        end
                    end
                end
                return false
            end
            -- The cure needs INFECTION-CONTROL knowledge, a
            -- different skill from bleed-control; resolve a medic
            -- for it independently (may be the same acolyte, who
            -- typically knows both).
            local infMedic
            for _, uid in ipairs(selectedUids) do
                if uid ~= targetUid
                   and unit.getKnowledge(uid, "infection_control") then
                    infMedic = uid; break
                end
            end
            local abOwner
            if infMedic and hasAntibiotics(infMedic) then
                abOwner = infMedic
            else
                for _, uid in ipairs(selectedUids) do
                    if hasAntibiotics(uid) then abOwner = uid; break end
                end
            end
            local infected = false
            for _, w in ipairs(unit.getWounds(targetUid) or {}) do
                if (w.infection or 0) >= 0.1 then
                    infected = true; break
                end
            end
            table.insert(items, {
                label   = "Treat infection",
                enabled = (infMedic ~= nil) and (abOwner ~= nil)
                          and infected,
                callback = function()
                    local res = unit.treatInfection(
                        infMedic, targetUid, abOwner)
                    if res then
                        local msg, cat
                        if res.ok then
                            local pct = math.floor(
                                (res.infection or 0) * 100 + 0.5)
                            msg = (pct <= 0)
                                and "Infection cleared"
                                or string.format(
                                    "Infection cut to %d%%", pct)
                            cat = "unit_event"
                        else
                            msg = "Treatment failed: "
                                  .. (res.message or "")
                            cat = "unit_warning"
                        end
                        engine.emitEventForUnit(cat, msg, targetUid)
                    end
                end,
            })
        end
    end
    contextMenu.show(items, mx, my)
    return true
end

-- Right-click on a ground item → context menu. With units selected:
-- Info / Pick up / Move here. Without: just Info. Pick up dispatches
-- the NEAREST selected unit; capacity is checked at the moment of
-- pickup (it can change en route).
function M.tryItemMenu(x, y)
    local gid = item.hitTestAt(x, y)
    if not gid then return false end

    local fbW, fbH = engine.getFramebufferSize()
    local ww, wh   = engine.getWindowSize()
    local mx, my   = x, y
    if ww and wh and ww > 0 and wh > 0 then
        mx = x * (fbW / ww)
        my = y * (fbH / wh)
    end
    local contextMenu = require("scripts.ui.context_menu")
    local menuItems = {
        { label = "Info",
          callback = function()
              item.select(gid)
              unit.deselectAll()
              building.deselect()
          end },
    }
    local selUids = unit.getSelected() or {}
    if #selUids > 0 then
        local ipos = nil
        for _, g in ipairs(item.listGround() or {}) do
            if g.id == gid then ipos = g; break end
        end
        if ipos then
            local unitAi = require("scripts.unit_ai")
            table.insert(menuItems, {
                label = "Pick up",
                callback = function()
                    local best, bestUid = math.huge, nil
                    for _, uid in ipairs(selUids) do
                        local info = unit.getInfo(uid)
                        if info then
                            local d = (info.gridX - ipos.x) ^ 2
                                    + (info.gridY - ipos.y) ^ 2
                            if d < best then
                                best, bestUid = d, uid
                            end
                        end
                    end
                    if bestUid then
                        unitAi.commandPickup(bestUid, gid)
                    end
                end })
            table.insert(menuItems, {
                label = "Move here",
                callback = function()
                    for _, uid in ipairs(selUids) do
                        unitAi.commandMove(uid, ipos.x, ipos.y)
                    end
                end })
        end
    end
    contextMenu.show(menuItems, mx, my)
    return true
end

-- No-selection right-click on a tile → tile context menu. Capture the
-- right-clicked tile NOW (the cursor moves once the menu opens) and
-- stash it in the callbacks. Currently a one-item menu ("Info") as a
-- smoke test of the right-click + context-menu plumbing; per-target
-- providers replace this hardcoded list later.
-- Capture the right-clicked tile with a live pick at the click coords
-- (the cached hover lags a fast move, and once the menu opens the
-- cursor moves off the tile anyway) (#123). The pick resolves the
-- tile at the active z-slice, so stash its z too and select that
-- exact tile — a right-click on a cliff face / below the surface must
-- not snap to the column top (#367).
function M.tryTileMenu(x, y)
    local gx, gy, gz = world.pickTile(x, y)
    if not (gx and gy) then return false end

    local hud = require("scripts.hud")
    local contextMenu = require("scripts.ui.context_menu")
    local fbW, fbH = engine.getFramebufferSize()
    local ww, wh = engine.getWindowSize()
    local mx, my = x, y
    if ww and wh and ww > 0 and wh > 0 then
        mx = x * (fbW / ww)
        my = y * (fbH / wh)
    end
    local tileX, tileY, tileZ = gx, gy, gz
    contextMenu.show({
        { label = "Info",
          callback = function()
              -- Drive the HUD toolbar widget so the bottom-
              -- left icon flips to the info tool. The
              -- toggle's onChange runs world.setToolMode +
              -- the build/tile_editor side effects, so this
              -- single call replaces a manual setToolMode.
              local toggle = require("scripts.ui.toggle")
              toggle.applyOptionByName(
                  hud.toolToggleId, "tool_info")
              -- selectTile uses the direct tile-coord
              -- select API — the cursor's pixel-hover state
              -- has already moved to the menu, so the
              -- usual hover+select would pick the wrong
              -- tile. tileZ is the z captured at right-click
              -- time so the selection lands on the clicked
              -- tile, not the column surface (#367).
              world.selectTile(hud.worldId, tileX, tileY, tileZ)
              local tileEditor =
                  require("scripts.tile_editor")
              tileEditor.onTileSelected(tileX, tileY)
          end },
    }, mx, my)
    return true
end

return M
