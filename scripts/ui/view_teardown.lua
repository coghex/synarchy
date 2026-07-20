-- Shared view-transition teardown registry (#156).
--
-- Root cause of a whole bug family (#99/#100/#104/#137–#148): overlays,
-- popups, and armed tool modes live on their own pages or module state,
-- so the page swaps done by view transitions never touch their LOGICAL
-- state — they survive off-view, leak into the wrong view or the next
-- world, reappear stale, or keep consuming clicks. Each widget used to
-- be fixed with one-off cleanup calls sprinkled across the transition
-- sites; this registry is the shared mechanism: one ordered table of
-- per-widget hooks, swept by every transition site.
--
-- Transitions (the key passed to run()):
--   * "zoomBand" — hud.reconcileView(): the camera crossed a zoom band
--     (zoomed_in / none / zoomed_out) and the HUD swapped view pages.
--     ctx = { worldId = hud.worldId, newView = <the band just entered> }.
--   * "hudHide"  — hud.hide(): the gameplay HUD is leaving the screen
--     (world view exits to a menu, or a menu opens over it).
--   * "menu"     — uiManager.showMenu(): every menu transition,
--     INCLUDING the keepWorld Settings path that deliberately skips
--     hud.hide() — so only widgets that must die even while the world
--     stays visible behind Settings hook this one (#146/#147). The
--     non-keepWorld paths also run hud.hide(), i.e. the full "hudHide"
--     sweep.
--   * "resize"   — hud.createUI() (#750): about to UI.deletePage
--     hud.world_page and replace it with a fresh page handle, as part
--     of a real framebuffer resize or a UI-scale rebuild. Any popup
--     mounted ON hud.world_page (panel.new({page = h.page, ...})) has
--     its elements destroyed by that delete regardless of its own
--     module state, so left alone it would reappear "open"
--     (state.open/panelId still set) pointing at deleted elements.
--     Hooked widgets close BEFORE the delete, same as any other
--     view-transition teardown — this is not a gameplay action, the
--     underlying job/build/craft keeps running, only the popup view
--     itself closes (the player can reopen it). Deliberately NOT
--     hooked: build_tool's "placement" mode (the structure two-click
--     anchor + building ghost) — its ghost is engine-side world-space
--     rendering (building.setGhost, re-established every tick), not a
--     hud.world_page element, and per the #750 issue thread's
--     amendment a layout-only rebuild must never cancel a committed/
--     armed two-click designation anchor the way it may a merely
--     PENDING (unreleased) interaction. Same reasoning keeps
--     mine_tool/chop_tool/till_tool's own designation anchors off this
--     transition too (they aren't hud.world_page-mounted UI at all).
--
-- Rules for entries:
--   * Hooks MUST be idempotent — they run on every transition of their
--     kind, almost always with nothing to tear down.
--   * Hooks are pcall-wrapped: a failing hook logs and never blocks the
--     rest of the sweep (the page swap has already happened).
--   * require() the widget module inside the hook, not at file scope,
--     so widget modules keep loading lazily.
--   * A new overlay / popup / armed tool mode gets an entry HERE, not a
--     one-off call at a transition site. If your widget owns its own
--     page or module-level "open"/"armed" state, it belongs in this
--     table — decide per transition what should happen to it.

local viewTeardown = {}

local registry = {
    -- Shared HUD info panel. On a band change the content is stale for
    -- the new view — clear it. On hud.hide the panel is only SUPPRESSED:
    -- this hides the page and blocks background info watchers (which
    -- keep pushing text for a still-selected object) from re-opening it
    -- over the menu, while the stored tab text survives for re-show —
    -- hud.show() releases the suppression and restores from content
    -- (#134).
    { name = "info_panel",
      zoomBand = function() require("scripts.hud.info_panel").clear() end,
      hudHide  = function() require("scripts.hud.info_panel").suppress("hud") end },

    -- Item-contents popup (#142/#100): mounted on hud.world_page, so
    -- hiding the page only takes it off-view; its logical state stays
    -- open and it would reappear stale. closeIfOpen() is idempotent.
    { name = "item_contents_panel",
      zoomBand = function() require("scripts.item_contents_panel").closeIfOpen() end,
      hudHide  = function() require("scripts.item_contents_panel").closeIfOpen() end,
      resize   = function() require("scripts.item_contents_panel").closeIfOpen() end },

    -- Cargo-inventory popup (#141/#99): same story as the item-contents
    -- popup — mounted on hud.world_page, so a page hide leaves
    -- state.open true and the popup reappears stale. closeIfOpen() is
    -- idempotent.
    { name = "cargo_inventory_panel",
      zoomBand = function() require("scripts.cargo_inventory_panel").closeIfOpen() end,
      hudHide  = function() require("scripts.cargo_inventory_panel").closeIfOpen() end,
      resize   = function() require("scripts.cargo_inventory_panel").closeIfOpen() end },

    -- Crafting station bills popup (#330): same story — mounted on
    -- hud.world_page, own module-level "open" state, opened via
    -- right-click. closeIfOpen() is idempotent.
    { name = "crafting_panel",
      zoomBand = function() require("scripts.crafting_panel").closeIfOpen() end,
      hudHide  = function() require("scripts.crafting_panel").closeIfOpen() end,
      resize   = function() require("scripts.crafting_panel").closeIfOpen() end },

    -- Planting screen (#335): same story — mounted on hud.world_page,
    -- own module-level "open" state, opened by the plant tool.
    -- closeIfOpen() is idempotent.
    { name = "plant_panel",
      zoomBand = function() require("scripts.plant_panel").closeIfOpen() end,
      hudHide  = function() require("scripts.plant_panel").closeIfOpen() end,
      resize   = function() require("scripts.plant_panel").closeIfOpen() end },

    -- Right-click context menu (#139/#86): lives on its own modal page,
    -- anchored to the tile/unit/item under the click. Page swaps never
    -- touch it, so it could survive over the wrong view or leak into the
    -- next screen. hide() is idempotent (no-op when no menu is open).
    { name = "context_menu",
      zoomBand = function() require("scripts.ui.context_menu").hide() end,
      hudHide  = function() require("scripts.ui.context_menu").hide() end },

    -- Active drag-select box (#146): the rect lives on its own
    -- "drag_select_overlay" page, independent of every page the
    -- transitions swap. An armed/dragging box would survive and resume
    -- or commit against the wrong view — including on the keepWorld
    -- Settings path, hence the "menu" hook. cancel() abandons it
    -- without committing a selection (idempotent, no-op when idle).
    { name = "unit_drag_select",
      zoomBand = function() require("scripts.unit_drag_select").cancel() end,
      hudHide  = function() require("scripts.unit_drag_select").cancel() end,
      menu     = function() require("scripts.unit_drag_select").cancel() end },

    -- Mine-designation anchor (#144): a pending first-corner anchor
    -- lives in mine_tool's Lua state + the world cursor (mineAnchor)
    -- and renders a preview grid. The tool only acts in zoomed_in, and
    -- Escape/right-click cancel is gated on that view — so an anchor
    -- surviving a band change could not be canceled off-view. cancel()
    -- is idempotent (WorldClearMineAnchor no-ops when nothing pending).
    { name = "mine_tool",
      zoomBand = function() require("scripts.mine_tool").cancel() end },

    -- Chop designation anchor (#97): same idempotent teardown.
    { name = "chop_tool",
      zoomBand = function() require("scripts.chop_tool").cancel() end },

    -- Till designation anchor (#333): same idempotent teardown.
    { name = "till_tool",
      zoomBand = function() require("scripts.till_tool").cancel() end },

    -- Build picker (#143): the picker panel lives on hud.world_page and
    -- its "picker" mode persists across band changes, so a picker opened
    -- in zoomed_in stays logically alive and reappears stale when the
    -- world page is next shown. hidePicker() is idempotent (destroyPicker
    -- no-ops with no panel; mode only resets from "picker") and
    -- deliberately does NOT touch "placement" mode.
    { name = "build_tool_picker",
      zoomBand = function() require("scripts.build_tool").hidePicker() end,
      resize   = function() require("scripts.build_tool").hidePicker() end },

    -- Build placement (#140): once the build tool enters "placement"
    -- mode, its ghost preview and click handling keep running off the
    -- world cursor regardless of the HUD view, consuming clicks in
    -- zoomed-out / fade-zone. exitPlacement() is idempotent (clearGhost
    -- no-ops with no ghost; mode resets to "off") — and since #403 folded
    -- construction designation into this tool, it also drops any pending
    -- structure-rectangle anchor (construction.clearAnchor), covering what
    -- used to be the separate construct_tool teardown entry.
    { name = "build_tool_placement",
      zoomBand = function() require("scripts.build_tool").exitPlacement() end },

    -- Remote-settlement confirmation modal (#779): its own page +
    -- module-level pending state, opened mid-placement from
    -- build_tool.lua's starting-building click branch. A stray modal
    -- surviving a band/HUD/menu transition would keep blocking input
    -- (LayerModal is input-exclusive by default) over whatever's now on
    -- screen. closeIfOpen() is idempotent and records no outcome (a
    -- teardown sweep isn't a player cancel decision).
    { name = "build_tool_remote_warning",
      zoomBand = function() require("scripts.build_tool_remote_warning").closeIfOpen() end,
      hudHide  = function() require("scripts.build_tool_remote_warning").closeIfOpen() end,
      menu     = function() require("scripts.build_tool_remote_warning").closeIfOpen() end },

    -- Arena tile-editor popup (#138): lives on hud.world_page and was
    -- only torn down on empty tile-info broadcasts, tool changes, or
    -- arena exit — zoom-map chunk selection produces non-empty HUD info
    -- text, so a band change left it alive off-view to reappear stale.
    -- clear() is idempotent (destroyPopup no-ops with no popup, and is
    -- a no-op outside arena mode).
    { name = "tile_editor",
      zoomBand = function() require("scripts.tile_editor").clear() end,
      resize   = function() require("scripts.tile_editor").clear() end },

    -- Main debug overlay (#147, and the armed spawn/edit modes it owns,
    -- #148 — debug.hide() clears them all): only meaningful in the
    -- zoomed-in gameplay view and never torn down by any page it sits
    -- over. Without the "menu" hook, Settings opened from a game view
    -- (keepWorld skips hud.hide()) left the F8 overlay visible AND
    -- clickable (uiManager.onMouseDown gives it first crack via
    -- tryClaimClick) on the Settings screen. hide() is idempotent.
    { name = "debug_overlay",
      zoomBand = function(ctx)
          if ctx.newView ~= "zoomed_in" then
              require("scripts.debug").hide()
          end
      end,
      hudHide  = function() require("scripts.debug").hide() end,
      menu     = function() require("scripts.debug").hide() end },

    -- HUD-owned log panels (#84) and notification popups (#85): each
    -- owns its own modal page / visible flag, so hiding the HUD pages
    -- does not cover them — hide on hudHide so they don't leak into the
    -- next screen.
    { name = "event_log",
      hudHide = function() require("scripts.event_log").hide() end },
    { name = "combat_log",
      hudHide = function() require("scripts.combat_log").hide() end },
    { name = "injury_log_panel",
      hudHide = function() require("scripts.injury_log_panel").hide() end },
    { name = "notification_popups",
      hudHide = function() require("scripts.popup").dismissAll() end },

    -- Per-unit log overlay (#104): same own-page story as the log
    -- panels above.
    { name = "unit_log",
      hudHide = function() require("scripts.unit_log").hide() end },

    -- Ground-item selection (#175): per-world cursor state the item
    -- watcher (item_info_panel.update) keeps polling and would use to
    -- repopulate the panel. On a band change the world is still the
    -- visible/active one, so item.deselect() resolves activeWorld
    -- correctly. Deliberately NO hudHide hook: on the hide path the
    -- page is leaving wmVisible and activeWorld head-falls-back to a
    -- DIFFERENT registered world, so a Lua deselect could clear the
    -- wrong world — the world thread clears it deterministically in
    -- handleWorldHideCommand, keyed on the exact page being hidden
    -- (which worldView.hide()/testArena.hide() issue just before
    -- hud.hide() runs).
    { name = "item_selection",
      zoomBand = function() item.deselect() end },

    -- Building selection (#176): a single global id (bmSelected), not a
    -- per-world cursor — so no wrong-world hazard, and the
    -- building-info watcher keeps polling building.getSelected() and
    -- re-pushing the same building into the shared info panel,
    -- repopulating it stale after either transition. deselect() is a
    -- no-op when nothing is selected.
    { name = "building_selection",
      zoomBand = function() building.deselect() end,
      hudHide  = function() building.deselect() end },

    -- Chunk/tile cursor selection (#132): the zoom-map chunk selection
    -- (zoomSelectedPos) and zoomed-in tile selection (worldSelectedTile)
    -- both live in the per-world cursor (wsCursorRef), independent of
    -- the HUD pages. The info panel is cleared on the band change, but
    -- pollCursorInfo only republishes on a selection *change* — so a
    -- surviving selection leaves the highlight set while the panel
    -- stays blank. Clear both, keyed on ctx.worldId (the visible/active
    -- world that owns the view, the same id hud.onMouseDown uses);
    -- no-ops when nothing is selected.
    { name = "cursor_selection",
      zoomBand = function(ctx)
          world.clearZoomCursorSelect(ctx.worldId)
          world.clearWorldCursorSelect(ctx.worldId)
      end },
}

-- Sweep every registered hook for one transition, in registry order.
-- ctx is forwarded to each hook (zoomBand passes worldId/newView).
function viewTeardown.run(transition, ctx)
    ctx = ctx or {}
    for _, entry in ipairs(registry) do
        local hook = entry[transition]
        if hook then
            local ok, err = pcall(hook, ctx)
            if not ok then
                engine.logError("view_teardown: " .. entry.name
                    .. " failed on " .. transition .. ": " .. tostring(err))
            end
        end
    end
end

return viewTeardown
