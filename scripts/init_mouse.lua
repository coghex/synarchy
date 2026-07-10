-- Gameplay mouse routing for scripts/init.lua (#543): the tool-claim
-- chain, left-click selection dispatch, and right-click context-menu
-- / move-order dispatch. Per-target right-click menu construction
-- lives in scripts/init_context_menu.lua.
local M = {}

-- Mouse-button constants (match Engine.Scripting.Lua.Thread::LuaMouseDownEvent)
local MOUSE_LEFT  = 1
local MOUSE_RIGHT = 2

-- F4 (#646) Layer A: one action-outcome record per click that reaches
-- this chain (a UI-consumed click never gets here at all — that half is
-- recorded earlier, at the engine dispatch level, see
-- Engine.Scripting.Lua.Thread.Dispatch's LuaUIClickEvent case). `handler`
-- names the tool/selection domain that consumed the click; `outcome`
-- defaults to "accepted" (routed to something real) unless overridden
-- (e.g. "deadclick"/"noop"). Debug-only armed-spawn/item/fluid/terrain/
-- location/structure branches below are deliberately NOT instrumented —
-- they're developer tooling the naive playtest persona never drives.
local function recordClick(handler, outcome, x, y, reason)
    debug.recordOutcome{
        kind = "input.click",
        outcome = outcome or "accepted",
        where = { x = x, y = y },
        handler = handler,
        reason = reason,
    }
end

function M.onMouseDown(button, x, y)
    -- Only handle clicks that reach us — UI hit-tests run earlier in
    -- the input thread; if a UI element ate the click, this never fires.
    local debugOverlay = require("scripts.debug")

    -- #154: this is a focus-less broadcast handler, so a blank click that
    -- misses every UI element still reaches us even when no gameplay world
    -- is interactable — in a menu (resolveActiveWorld then falls back to a
    -- HIDDEN world), or under a non-gameplay overlay that bypasses
    -- hud.hide() (pause menu / keep-world Settings). isGameplayInputActive()
    -- is the canonical "the player is driving a visible world" predicate
    -- (same one the box-select arm #146 and the gameplay key handlers #182
    -- use). When it's false we must not select, mutate, or move-order the
    -- world — but a stray RIGHT click is still allowed to *cancel* a leaked
    -- build / mine / armed-debug mode (their state teardown is #138/#140/
    -- #148; this gate just keeps blank clicks from ACTING on a hidden world).
    local gameplayActive = require("scripts.ui_manager").isGameplayInputActive()

    -- Debug overlay's parallel hit-test gets first crack. If a debug
    -- rect (spawn button / list entry) eats the click, we stop here
    -- so the click can't fall through into selection / tile-cursor.
    -- (UI hit-test on a self-hiding overlay — safe to run ungated; it
    -- returns false whenever the overlay isn't shown.)
    if debugOverlay.tryClaimClick(button, x, y) then
        recordClick("debug_overlay", nil, x, y)
        return
    end

    -- Debug anim panel (per-selection). Sits to the LEFT of the
    -- info-v2 pane and lists clickable animation names. Same
    -- parallel hit-test pattern as debug.lua so clicks on anim rows
    -- don't fall through into deselect-on-empty.
    local debugAnimPanel = require("scripts.debug_anim_panel")
    if debugAnimPanel.tryClaimClick(button, x, y) then
        recordClick("debug_anim_panel", nil, x, y)
        return
    end

    -- Build tool gets first crack at mouse clicks when in placement
    -- mode, so the placement click doesn't fall through into unit
    -- selection / tile-cursor. Left-click places (world mutation),
    -- right-click cancels. #154: when gameplay input is inactive only
    -- let the right-click cancel through, so a blank left-click can't
    -- commit a placement onto a hidden/paused world behind an overlay.
    if gameplayActive or button == MOUSE_RIGHT then
        local buildTool = require("scripts.build_tool")
        if buildTool.handleMouseDown(button, x, y) then
            recordClick("build_tool", nil, x, y)
            return
        end

        -- Mine tool claims clicks while the mine tool mode is active
        -- (anchor / commit / cancel), so they don't fall through into
        -- unit selection. Same left=mutate / right=cancel split, same
        -- #154 gate as the build tool above.
        local mineTool = require("scripts.mine_tool")
        if mineTool.handleMouseDown(button, x, y) then
            recordClick("mine_tool", nil, x, y)
            return
        end

        -- Chop designation tool claims clicks while active (anchor /
        -- commit / cancel), same left=designate / right=cancel split
        -- and #154 gate.
        local chopTool = require("scripts.chop_tool")
        if chopTool.handleMouseDown(button, x, y) then
            recordClick("chop_tool", nil, x, y)
            return
        end

        -- Till designation tool claims clicks while active (anchor /
        -- commit / cancel), same left=designate / right=cancel split
        -- and #154 gate.
        local tillTool = require("scripts.till_tool")
        if tillTool.handleMouseDown(button, x, y) then
            recordClick("till_tool", nil, x, y)
            return
        end

        -- Plant designation tool claims clicks while active (opens the
        -- planting screen on a tilled tile / closes it), same
        -- left=act / right=cancel split and #154 gate.
        local plantTool = require("scripts.plant_tool")
        if plantTool.handleMouseDown(button, x, y) then
            recordClick("plant_tool", nil, x, y)
            return
        end
    end

    if button == MOUSE_LEFT then
        -- #154: every left-click branch below either MUTATES the world
        -- (armed debug spawn / item / fluid / terrain / location /
        -- structure placement) or SELECTS in it (units / buildings /
        -- items / tile cursor). None of them is a cancel. So a single
        -- gate covers them all: a blank left-click on a hidden/paused
        -- world must do nothing. (Right-click cancels live in the
        -- MOUSE_RIGHT branch and stay reachable below.)
        if not gameplayActive then
            -- F4 (#646): the "clicked where a control used to be" case —
            -- e.g. the create-world screen's progress bar sits where the
            -- Generate/Continue button was, but isn't itself clickable,
            -- so the press falls all the way through to here and does
            -- nothing. Genuinely a deadclick, not a recognized no-op.
            recordClick(nil, "deadclick", x, y,
                "gameplay input inactive (menu/paused/hidden world)")
            return
        end

        -- #148: defense in depth for the armed debug spawn/edit modes.
        -- They are only meaningful in the zoomed-in gameplay view, and the
        -- leave-gameplay transitions already tear them down (hud.hide /
        -- hud.reconcileView / uiManager.showMenu all call
        -- debugOverlay.hide()). But gate the armed-click ROUTING on the same
        -- current-view predicate the overlay uses for F8 and its parallel
        -- claim (#147/#151) so an armed mode that ever survives a transition
        -- still can't fire a spawn/edit on the zoom map or under a menu.
        -- gameplayActive alone (#154) is not enough: it stays true on the
        -- zoom map / fade band, where these tile-level placements have no
        -- meaning. When false, fall through to the normal selection logic
        -- below — only the armed branches are gated, not selection.
        local debugArmable = debugOverlay.inGameplayView()

        -- Debug spawn mode: if armed, this click is a spawn, not a
        -- selection. Spawn at the hovered tile and stay armed.
        --
        -- Debug-spawned units always get faction "debug" — that
        -- tag means "player-controlled AND has no friendly-fire
        -- restrictions". Lets the user spawn two acolytes (or
        -- acolyte + bear, etc.) and make them fight for testing.
        -- Production unit sources still pass their canonical
        -- faction (portal spawns → "player"; world-gen wildlife
        -- spawns → "wildlife").
        if debugArmable and debugOverlay.armedDef then
            -- Live pick at the click coords, not the 0.1s-cached hover, so
            -- a fast move-then-click spawns under the click (#123).
            local gx, gy = world.pickTile(x, y)
            if gx and gy then
                unit.spawn(debugOverlay.armedDef, gx + 0.5, gy + 0.5,
                           nil, "debug")
            end
            return
        end

        -- Debug item-spawn mode: arms an item def; the click drops
        -- the item onto the ground exactly under the cursor (float
        -- coords from the fractional hover position; resting height
        -- derives from terrain at render). Tile-center fallback
        -- covers the no-hover edge case.
        if debugArmable and debugOverlay.armedItemDef then
            -- Live sub-tile pick at the click coords (#123).
            local hx, hy = world.pickPos(x, y)
            if hx and hy then
                item.spawnGround(debugOverlay.armedItemDef, hx, hy)
            else
                local gx, gy = world.pickTile(x, y)
                if gx and gy then
                    item.spawnGround(debugOverlay.armedItemDef,
                                     gx + 0.5, gy + 0.5)
                end
            end
            return
        end

        -- Debug fluid-spawn mode: arms a kind ("water" / "lava"); the
        -- click places one tile of that fluid on top of the column.
        if debugArmable and debugOverlay.armedFluidType then
            local gx, gy = world.pickTile(x, y)  -- live pick (#123)
            if gx and gy then
                local hud = require("scripts.hud")
                local worldId = (hud and hud.worldId) or "test_arena"
                world.setFluidTile(worldId, math.floor(gx), math.floor(gy),
                                   debugOverlay.armedFluidType)
            end
            return
        end

        -- Debug terrain-placement mode: arms a material id; the click
        -- raises the column at the hover tile one z of that material
        -- (WeAddTile through the edit log — persists like any edit).
        if debugArmable and debugOverlay.armedTerrainId then
            local gx, gy = world.pickTile(x, y)  -- live pick (#123)
            if gx and gy then
                local hud = require("scripts.hud")
                local worldId = (hud and hud.worldId) or "test_arena"
                world.addTile(worldId, math.floor(gx), math.floor(gy),
                              debugOverlay.armedTerrainId)
            end
            return
        end

        -- Debug location-stamp mode: arms a location def name; the click
        -- stamps that premade structure (room/outpost/...) anchored at the
        -- hover tile (world.setCell terrain edits + content spawns).
        if debugArmable and debugOverlay.armedLocation then
            local gx, gy = world.pickTile(x, y)  -- live pick (#123)
            if gx and gy then
                local hud = require("scripts.hud")
                local worldId = (hud and hud.worldId) or "test_arena"
                local locations = require("scripts.locations")
                locations.stamp(debugOverlay.armedLocation,
                                math.floor(gx), math.floor(gy), worldId)
            end
            return
        end

        -- Debug structure-placement mode: arms a kind (wall/floor/ceiling/
        -- post). Floor/ceiling/post place on the clicked tile; a wall goes
        -- in the clicked QUARTER of the tile (→ its diamond edge).
        if debugArmable and debugOverlay.armedStructure then
            -- Derive the tile from the FRACTIONAL pick (floor), NOT pickTile:
            -- the latter rounds in a ~0.17-tile-shifted space, so near a tile
            -- border it disagrees with the quarter-corner/edge frac (from the
            -- fractional pick) → posts landed on the wrong tile and the
            -- floor-gate flaked. floor(hx,hy) keeps tile + corner consistent.
            -- pickPos runs the hit-test live at the click coords, not the
            -- 0.1s-cached hover (#123).
            local hx, hy = world.pickPos(x, y)
            if hx and hy then
                local structures = require("scripts.structures")
                structures.placeKind(math.floor(hx), math.floor(hy),
                                     debugOverlay.armedStructure, hx, hy)
            end
            return
        end

        -- Arm unit drag-select. Forward-only (handle*, not a broadcast)
        -- so it sits in THIS ordered claim chain: every guard above —
        -- the debug overlay / anim panel / build tool / mine tool, AND
        -- the debug armed-placement modes (spawn / item / fluid /
        -- terrain / location / structure) that each `return` above —
        -- has already consumed and bailed on its own click. So a click
        -- eaten by any of them can no longer also start a background
        -- box-selection (#114). Placed below those returns rather than
        -- enumerating the armed* fields, so a future armed mode stays
        -- shielded for free. It doesn't consume the click — the
        -- single-unit selection / tile-cursor logic below still runs;
        -- the drag only takes over on mouse-up if it passes threshold.
        -- The gameplay-active gate (#154/#146 — a box-select must never
        -- arm behind a menu / pause overlay) is the early return at the
        -- top of this MOUSE_LEFT branch, so no per-call check is needed.
        require("scripts.unit_drag_select").handleMouseDown(button, x, y)

        local id = unit.hitTestAt(x, y)
        local shift = engine.isKeyDown("LeftShift")
                      or engine.isKeyDown("RightShift")
        if id then
            -- Hit a unit. Shift adds to the current selection;
            -- otherwise replace. The unit_info_panel watcher will
            -- see the change next tick and push unit info into the
            -- HUD panel + clear any tile cursor.
            if shift then
                local current = unit.getSelected() or {}
                local seen = {}
                local merged = {}
                for _, uid in ipairs(current) do
                    if not seen[uid] then
                        seen[uid] = true
                        table.insert(merged, uid)
                    end
                end
                if not seen[id] then table.insert(merged, id) end
                unit.setSelection(merged)
            else
                unit.select(id)
            end
            -- Selecting a unit takes over the info panel — deselect
            -- any building/item so the panel doesn't flicker between
            -- schemas.
            building.deselect()
            item.deselect()
            recordClick("unit_select", nil, x, y)
        else
            -- No unit hit. Try a ground item (click priority:
            -- units > items > buildings — moving things win).
            local gid = item.hitTestAt(x, y)
            if gid then
                item.select(gid)
                -- Ground-item selection is mutually exclusive with
                -- unit/building selection (see World.Cursor.Types). Items
                -- are single-select, so Shift carries no additive meaning
                -- here — always clear the other domains, even on Shift.
                unit.deselectAll()
                building.deselect()
                recordClick("item_select", nil, x, y)
            else
                -- No item. Try a building.
                local bid = building.hitTestAt(x, y)
                if bid then
                    building.select(bid)
                    -- Buildings are single-select and mutually exclusive
                    -- with unit/item selection; clear the others
                    -- unconditionally (Shift adds units, not buildings).
                    item.deselect()
                    unit.deselectAll()
                    recordClick("building_select", nil, x, y)
                else
                    -- Click missed everything. With Shift held, keep
                    -- the current selection (so shift-dragging from
                    -- empty terrain can extend it). Otherwise deselect.
                    -- Not a "deadclick" — an empty-terrain click is a
                    -- recognized deselect gesture the player understands,
                    -- not a phantom affordance; "noop" reflects that
                    -- nothing was there to act on, without flagging it as
                    -- a UX defect.
                    if not shift then
                        unit.deselectAll()
                        building.deselect()
                        item.deselect()
                    end
                    recordClick("deselect", "noop", x, y)
                end
            end
        end
    elseif button == MOUSE_RIGHT then
        -- Right-click is a cancel for debug spawn mode (highest priority).
        if debugOverlay.armedDef then
            debugOverlay.clearArmed()
            return
        end
        if debugOverlay.armedItemDef then
            debugOverlay.clearArmedItem()
            return
        end
        if debugOverlay.armedFluidType then
            debugOverlay.clearArmedFluid()
            return
        end
        if debugOverlay.armedTerrainId then
            debugOverlay.clearArmedTerrain()
            return
        end
        if debugOverlay.armedLocation then
            debugOverlay.clearArmedLocation()
            return
        end
        if debugOverlay.armedStructure then
            debugOverlay.clearArmedStructure()
            return
        end
        -- #154: every right-click branch below hit-tests buildings / units
        -- / ground items or issues move orders against the active world. A
        -- blank right-click on a non-gameplay overlay or in a menu resolves
        -- to a HIDDEN world (resolveActiveWorld's empty-wmVisible fallback),
        -- so without this gate a stray right-click could open a context menu
        -- on a hidden-world entity or move-order a unit the player can't see.
        -- The armed-mode cancels above (plus the build/mine right-click
        -- cancels near the top) run while inactive so a stray click still
        -- dismisses a leaked mode (#138/#140/#148); past here we need an
        -- active, visible world. Same gate as the MOUSE_LEFT branch / #182.
        if not gameplayActive then
            recordClick(nil, "deadclick", x, y,
                "gameplay input inactive (menu/paused/hidden world)")
            return
        end

        -- Per-target menu construction lives in init_context_menu.lua;
        -- each try*Menu hit-tests its own target, shows the menu, and
        -- returns true if it claimed the click. Building menus win over
        -- unit menus win over item menus, matching the original inline
        -- ordering.
        local contextMenus = require("scripts.init_context_menu")
        if contextMenus.tryBuildingMenu(x, y) then
            recordClick("context_menu_building", nil, x, y)
            return
        end
        if contextMenus.tryUnitMenu(x, y) then
            recordClick("context_menu_unit", nil, x, y)
            return
        end
        if contextMenus.tryItemMenu(x, y) then
            recordClick("context_menu_item", nil, x, y)
            return
        end

        -- Right-click is a move order when units are selected.
        -- hud.onMouseDown also fires on right-click and clears the
        -- tile cursor — that's fine, it doesn't touch unit selection.
        local selected = unit.getSelected()
        if selected and #selected > 0 then
            -- Live pick at the click coords so the move order targets the
            -- tile under the click, not the 0.1s-cached hover (#123).
            local gx, gy = world.pickTile(x, y)
            if gx and gy then
                local tx = gx + 0.5
                local ty = gy + 0.5
                local unitAi = require("scripts.unit_ai")
                for _, uid in ipairs(selected) do
                    -- Route through the AI so the command becomes a
                    -- utility-scored candidate that high-priority
                    -- needs (thirst, etc.) can interrupt and resume.
                    -- No explicit speed → the "ordered" regime (a
                    -- sustainable push above comfort). A hard-coded
                    -- fast speed here exhausts the unit's stamina and
                    -- collapses it mid-move.
                    unitAi.commandMove(uid, tx, ty)
                end
                recordClick("move_order", nil, x, y)
            else
                -- Off-world right-click with a selection: no tile to
                -- order to, and no tile menu either (that branch is the
                -- `else` below, gated on no-selection).
                recordClick("move_order", "noop", x, y, "no tile under cursor")
            end
        else
            -- No selection → open the tile context menu.
            contextMenus.tryTileMenu(x, y)
            recordClick("context_menu_tile", nil, x, y)
        end
    end
end

-- onMouseUp fires on EVERY physical release — unlike onMouseDown,
-- which only fires for presses that reached the game world (UI
-- elements and the tooltip lock eat theirs). UI widget drags (slider
-- knob, scrollbar tab) start from a UI click callback and depend on
-- the unconditional release to end. downRoute says where the matching
-- press went: "game" (onMouseDown fired), "ui" (a UI element ate it),
-- or "swallowed" (tooltip lock / minimized window). To pair strictly
-- with onMouseDown, guard on downRoute == "game".
function M.onMouseUp(button, x, y, downRoute)
end

return M
