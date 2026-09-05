-- Right-click context-menu construction for scripts/init.lua (#543).
-- Each try*Menu function hit-tests its own target kind, builds the
-- menu items for it, shows the menu, and returns true if it claimed
-- the click — mirrors the tool handleMouseDown claim convention.
local M = {}

-- Storage / work-station building right-click → "Contents" / "Bills" /
-- "Transfer" menu, regardless of unit selection. Move commands still
-- work on non-cargo, non-station tiles. building.hitTestAt takes
-- framebuffer pixel coords via the same conversion the tile-menu
-- function uses. A single building can offer more than one of these
-- (a workshop with cargo storage, transfer-eligible), so this
-- hit-tests once and builds one combined menu instead of competing
-- ones.
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

    -- Transfer (#1014, #1085): resolved through the contract's own
    -- eligibility query (unit.transferEndpointInfo) rather than the
    -- hasStorage check above, so a future change to
    -- Unit.Transfer.endpointEligible (e.g. an additional precondition)
    -- flows through without a UI change here — a Built station with
    -- zero storage capacity (hasStation but not hasStorage, "Bills"
    -- only) is exactly the case that query must ALSO refuse.
    -- transferSession.resolveSource picks the NEAREST player-commandable
    -- unit out of the selection (#1239, D-8: multi-unit selections are
    -- allowed, exact ties break on lowest uid), and returns nil only
    -- when the selection holds no eligible candidate at all -- which
    -- omits the entry rather than showing a disabled row. The endpoint
    -- query runs FIRST here because its gridX/gridY is the point
    -- candidates are ranked against. Since #1250 "eligible" also means
    -- the species can actually RUN the escort, so a debug-spawned bear
    -- in the selection is skipped here rather than becoming a session
    -- that never walks.
    local transferSession = require("scripts.transfer_session")
    local endpointInfo =
        unit.transferEndpointInfo({ kind = "building", id = hitBid })
    local source = nil
    if endpointInfo and endpointInfo.eligible then
        source = transferSession.resolveSource(unit.getSelected(), nil,
                                               endpointInfo,
                                               transferSession.ESCORT_ACTION)
    end
    local hasTransfer = source ~= nil

    if not (hasStorage or hasStation or hasTransfer) then return false end

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
                cargoPanel.openFor("building", hitBid, mx, my)
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
    if hasTransfer then
        table.insert(items, { label = "Transfer",
            callback = function()
                transferSession.create(source, "building", hitBid)
            end })
    end
    contextMenu.show(items, mx, my)
    return true
end

-- Right-click on a unit → Info / Attack context menu. Callers run
-- this before the move-when-selected branch so right-clicking a unit
-- doesn't get interpreted as a move-to-tile order. "Attack" only
-- appears when at least one player-COMMANDABLE unit is selected; it's
-- greyed when none of them is permitted to attack the target (no
-- friendly-fire by default — the force-attack override lands in a
-- later phase). Both questions come from the shared faction model
-- (#912), not from comparing tags here.
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
              -- #1929: targetUid was captured when the menu
              -- opened, and the target can die or leave the page
              -- before this fires. unit.select reports that
              -- refusal and changes nothing, so the unconditional
              -- deselects below must not run either (#1580's
              -- stale-captured-target contract).
              if not unit.select(targetUid) then return end
              -- Mirror the left-click unit-selection path:
              -- selecting a unit takes over the info panel,
              -- so clear any building/item selection to keep
              -- the shared HUD panel from flickering schemas.
              building.deselect()
              item.deselect()
          end },
    }
    -- Filter selection down to units that can take orders at all,
    -- excluding the target itself. That's the COMMANDABILITY
    -- property (#912) — a different question from "friendly to the
    -- player", which is why it isn't the relation below.
    local attackers = {}
    for _, uid in ipairs(selectedUids) do
        if uid ~= targetUid
           and faction.isPlayerCommandable(unit.getFaction(uid)) then
            table.insert(attackers, uid)
        end
    end
    if #attackers > 0 then
        -- Friendly check: Attack is greyed unless at least one
        -- attacker is actually PERMITTED to hit the target.
        -- faction.canAttack folds in the debug faction's
        -- unrestricted-combat property (the whole point of "debug"
        -- is no friendly-fire restriction so the player can stage
        -- acolyte-vs-acolyte fights in the debug overlay), so this
        -- no longer carries its own inline special case.
        local anyPermitted = false
        for _, uid in ipairs(attackers) do
            if faction.canAttack(unit.getFaction(uid), targetFac) then
                anyPermitted = true
                break
            end
        end
        local unitAi = require("scripts.unit_ai")
        table.insert(items, {
            label    = "Attack",
            enabled  = anyPermitted,
            callback = function()
                -- Player order → committed (holds far longer
                -- before futility breaks it; soft, not absolute).
                -- #1254: a player order to a unit a Mode A transfer
                -- session is holding ends that session first, then
                -- proceeds — player intent wins. Before the command,
                -- because the teardown stops every unit it held.
                for _, uid in ipairs(attackers) do
                    require("scripts.transfer_session").notePlayerOrder(uid)
                    unitAi.commandAttack(uid, targetUid, true)
                end
            end,
        })
    end
    -- Transfer (#1014, #1085): the target itself is the destination
    -- endpoint here, resolved through the contract's own eligibility
    -- query (unit.transferEndpointInfo) rather than a def-name check.
    -- A2 deleted the transfer_receiver data marker, so this row is now
    -- offered for EVERY distinct player-commandable unit (acolytes and
    -- debug units included), not only the technomule — an intentional,
    -- player-visible widening, because faction eligibility is what
    -- replaced the marker. transferSession.resolveSource excludes the
    -- target from a would-be source (self-transfer) and, since #1239,
    -- picks the NEAREST player-commandable unit out of the rest of the
    -- selection (D-8; exact ties break on lowest uid). A selection that
    -- holds only the target still yields no candidate, so no row. The
    -- endpoint query runs FIRST because its gridX/gridY is the point
    -- candidates are ranked against.
    do
        local transferSession = require("scripts.transfer_session")
        local endpointInfo =
            unit.transferEndpointInfo({ kind = "unit", id = targetUid })
        local source = nil
        if endpointInfo and endpointInfo.eligible then
            source = transferSession.resolveSource(selectedUids, targetUid,
                                                   endpointInfo,
                                                   transferSession.ESCORT_ACTION)
        end
        if source then
            table.insert(items, { label = "Transfer",
                callback = function()
                    transferSession.create(source, "unit", targetUid)
                end })
        end
    end
    -- Cancel transfer (#1253, requirement 1): the way out of a durable
    -- order (#1246) the player queued and has changed their mind about.
    --
    -- Gated on the TARGET's own orders, not the selection's -- this is
    -- the same right-clicked unit's menu that shows its Info, and the
    -- order belongs to whichever unit is carrying it. Omitted entirely
    -- when there is none: a unit with nothing queued has no decision to
    -- make here, so a disabled row would be noise (the omission rule
    -- #1249's gestures already follow).
    do
        local unitAi = require("scripts.unit_ai")
        if unitAi.hasActiveTransferOrder(targetUid) then
            table.insert(items, { label = "Cancel transfer",
                callback = function()
                    unitAi.cancelTransferOrder(targetUid)
                end })
        end
    end
    -- Treat bleeding: a selected unit that KNOWS bleed-control
    -- dresses the target's worst bleeding wound, drawing
    -- bandages from a first-aid kit carried by the medic OR
    -- any other selected unit (e.g. the technomule standing
    -- by). Greyed until a kit, a bleeding wound and the
    -- engine's own reach (unit.canTreat, #2297) all agree.
    do
        local medic
        for _, uid in ipairs(selectedUids) do
            if uid ~= targetUid
               and unit.getKnowledge(uid, "bleed_control") then
                medic = uid; break
            end
        end
        if medic then
            -- Supply discovery is the shared exact-instance scan
            -- (#2302): a row is enabled on the same container the
            -- treatment verb would draw from, so an empty kit ordered
            -- before a stocked same-definition one no longer greys out
            -- a treatment that would have committed.
            local supply = require("scripts.medical_supply")
            local function hasBandages(uid)
                return supply.bandageKit(uid) ~= nil
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
                enabled = (kitOwner ~= nil) and bleeding
                          and unit.canTreat(medic, targetUid, kitOwner),
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
            -- infected wound, a kit with antibiotics on the medic
            -- or another selected unit, and both within reach (#2297).
            local function hasAntibiotics(uid)
                return supply.antibioticsKit(uid) ~= nil
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
                          and infected
                          and unit.canTreat(infMedic, targetUid, abOwner),
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

-- Right-click on a ground item → context menu. The construction lives
-- in scripts/init_context_menu_item.lua (#2300): this file sits at its
-- 500-line budget (tools/lua_module_budget.py), and the Info row's
-- stale-gid gate needed room to state its contract the way the unit
-- row's does above.
function M.tryItemMenu(x, y)
    return require("scripts.init_context_menu_item").tryItemMenu(x, y)
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
