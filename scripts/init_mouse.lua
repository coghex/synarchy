-- Gameplay mouse routing for scripts/init.lua (#543): the ordered
-- tool-claim chain, the armed debug placement modes, and the two
-- gameplay-activity gates. The world-entity FALLBACK each button falls
-- through to — left-click selection, right-click context menus and
-- move orders — lives in scripts/init_mouse_entity.lua (#1875), behind
-- the zoom-band gate that module owns; per-target right-click menu
-- construction lives in scripts/init_context_menu.lua.
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
--
-- Unreachable (#774 review): every call below is inside M.onMouseDown,
-- which shadows this name with a version forwarding through
-- unit_drag_select.lua's deferClick — THAT converts window→framebuffer
-- for the F4 oracle; this dead copy never runs.
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

    -- F4 (#730 review round 6): arm click-vs-drag classification for
    -- EVERY press reaching this function, before any of the ordered
    -- tool/overlay claim guards below get a chance to consume it — an
    -- H1 `drag` action can start on any of them (a build-tool
    -- placement drag, a menu-background drag, ...), not just the
    -- "no tool claimed it" unit/item/building select-or-deselect
    -- fallback that used to be the only place this armed. Shadowing
    -- recordClick here (not renaming every call site below) defers
    -- ALL of them through dragSelect.deferClick, so whichever of
    -- dragSelect.onMouseUp/.cancel resolves the gesture records
    -- exactly one outcome for it. The SELECTION/TOOL/MENU EFFECT each
    -- branch below performs is unaffected — it still happens
    -- immediately, at press, exactly as before; only the F4 record's
    -- timing/kind moves. Box-selection's own visual/commit behavior
    -- stays opt-in via dragSelect.armBoxSelect (called only at the
    -- fallback path, below), preserving #114's original ordering
    -- restriction on where a background box-selection can arm.
    local dragSelect = require("scripts.unit_drag_select")
    dragSelect.handleMouseDown(button, x, y)
    local function recordClick(handler, outcome, x, y, reason)
        dragSelect.deferClick(button, handler, outcome, x, y, reason)
    end

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
    -- #742 review round 3: that right-click bypass is NOT a pure
    -- cancel for any of these five tools — with no pending anchor,
    -- build_tool/mine_tool/till_tool/plant_tool's right-click branch
    -- instead ERASES whatever designation sits under the cursor (a real
    -- world mutation, see e.g. scripts/build_tool.lua's MOUSE_RIGHT
    -- case), and since #1856 chop's right button is a dedicated ERASE
    -- gesture with no cancel meaning at all. A modal-blocked right-click reaches here as an
    -- ordinary game-route miss (debug/shell still need first refusal
    -- on it), so exclude UI.isInputBlocked() specifically from the
    -- bypass — the pre-#742 view/pause right-click-cancel behavior
    -- (isGameplayView() false but no modal) is otherwise unchanged.
    if gameplayActive or (button == MOUSE_RIGHT and not UI.isInputBlocked()) then
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

        -- Chop designation tool claims BOTH buttons while active
        -- (#1856): left arms an add gesture, right an erase, and each
        -- must be claimed here so neither can fall through to unit
        -- selection, a context menu or a move order. The effect itself
        -- lands at RELEASE, through unit_drag_select's shared box
        -- machinery, not at this press. Same #154 gate.
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

        -- #1875: the world-entity fallback — box-select arming and the
        -- unit/item/building hit-test selection chain — now lives in
        -- scripts/init_mouse_entity.lua, behind the zoom-band gate that
        -- module owns. Deliberately the LAST thing in this branch, so
        -- every guard above keeps exactly the opportunity it has today:
        -- the debug overlay / anim panel / build / mine / chop / till /
        -- plant claim chain, and the six armed debug placement modes,
        -- each of which already `return`ed on its own click.
        require("scripts.init_mouse_entity")
            .handleLeftPress(x, y, recordClick)
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

        -- #1875: the right-button world-entity fallback — per-target
        -- context menus and move orders — now lives in
        -- scripts/init_mouse_entity.lua, behind the same zoom-band gate.
        -- Placed after the six armed-mode cancels above (whose dismissal
        -- behavior is unchanged) and after the gameplay-activity gate,
        -- immediately before the first thing that hit-tests or
        -- move-orders a world entity.
        require("scripts.init_mouse_entity")
            .handleRightPress(x, y, recordClick)
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
