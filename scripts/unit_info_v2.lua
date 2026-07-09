-- Unit Info v2 — full-height right-edge pane
--
-- Shows a per-unit (or multi-unit) info pane while units are
-- selected: sprite tabs across the top, a Name/Type/Role/Action
-- header, Status/Physical/Mental/Skill/Knowledge stat sub-tabs,
-- equipment silhouette + accessory list, and an inventory list.
--
-- The old unit_info_panel.lua is suppressed while this module is
-- active (sets unitInfoWatch.suppressed = true on init). The current
-- HUD info panel still handles tile info; only the unit-info push is
-- skipped.
--
-- Lifecycle: loaded via engine.loadScript from init_loader.lua. Polls
-- unit.getSelected() each tick; shows the page when >=1 unit is
-- selected, hides on deselect. All section content rebuilds when the
-- selection identity changes.
--
-- Self-registers in package.loaded so engine.loadScript (dofile, a
-- fresh chunk) and require return the same instance — ui_manager's
-- click handlers (which look us up via package.loaded) need to see
-- the same module table dofile loaded, same pattern as unit_ai.lua.
--
-- #542: this file is a pane shell/orchestrator — the singleton, the
-- core texture cache + top-level page/box state, clearOwned (which
-- now just dispatches into each section's own clear function),
-- bootstrap, rebuildLayout, and the init/update/onFramebufferResize/
-- shutdown lifecycle. Section content lives in
-- scripts/unit_info_v2_*.lua submodules, required below: tabs (sprite
-- tab strip), header (Name/Type/Role/Action rows + Log button), stats
-- (sub-tab strip + Status/Physical/Mental/Skill/Knowledge dispatch —
-- panel bodies split further into unit_info_v2_status.lua +
-- unit_info_v2_panels.lua, on top of the shared
-- unit_info_v2_panel_engine.lua row/scrollbar engine and
-- unit_info_v2_stat_defs.lua metadata), equipment (silhouette +
-- slots + accessories), inventory (tab strip + item list, data prep
-- in unit_info_v2_inventory_data.lua), and context_menu (the
-- right-click Equip/Unequip/Contents/Store menus, shared item-hint
-- formatting in unit_info_v2_items.lua). Layout constants shared by
-- two or more sections live in unit_info_v2_layout.lua.

local unitInfoV2 = package.loaded["scripts.unit_info_v2"] or {}
package.loaded["scripts.unit_info_v2"] = unitInfoV2

local hud         = require("scripts.hud")
local infoPanel   = require("scripts.hud.info_panel")
local label       = require("scripts.ui.label")
local scale       = require("scripts.ui.scale")
local boxTextures = require("scripts.ui.box_textures")

local L = require("scripts.unit_info_v2_layout")

local tabs        = require("scripts.unit_info_v2_tabs")
local header      = require("scripts.unit_info_v2_header")
local statsMod    = require("scripts.unit_info_v2_stats")
local equipmentMod = require("scripts.unit_info_v2_equipment")
local inventoryMod = require("scripts.unit_info_v2_inventory")
require("scripts.unit_info_v2_context_menu")

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------

unitInfoV2.scriptId      = nil
unitInfoV2.bootstrapped  = false
unitInfoV2.page          = nil
unitInfoV2.outerBoxId    = nil
unitInfoV2.dividerIds    = {}   -- thin sprite handles for inter-section rules
unitInfoV2.ownedLabels   = {}   -- label.* IDs to clean up
unitInfoV2.lastSelCount  = 0
unitInfoV2.lastWantVisible = false  -- last resolved pane-visibility gate (#137)
unitInfoV2.whitePixelTex = nil  -- 1×1 white texture for dividers
unitInfoV2.tabSelectedTex = nil -- shaped backdrop drawn behind the active tab's sprite
unitInfoV2.subTabSelectedTexSet   = nil  -- 9-patch box set for active sub-tab
unitInfoV2.subTabUnselectedTexSet = nil  -- 9-patch box set for inactive sub-tab

-----------------------------------------------------------
-- Cleanup
-----------------------------------------------------------

local function clearOwned()
    for _, lblId in ipairs(unitInfoV2.ownedLabels) do
        label.destroy(lblId)
    end
    unitInfoV2.ownedLabels = {}

    for _, divId in ipairs(unitInfoV2.dividerIds) do
        UI.deleteElement(divId)
    end
    unitInfoV2.dividerIds = {}

    tabs.clearTabs()
    statsMod.clearAll()
    equipmentMod.clearAll()
    inventoryMod.clearAll()

    unitInfoV2.headerNameLabelId   = nil
    unitInfoV2.headerTypeLabelId   = nil
    unitInfoV2.headerActionLabelId = nil

    if unitInfoV2.outerBoxId then
        UI.deleteElement(unitInfoV2.outerBoxId)
        unitInfoV2.outerBoxId = nil
    end

    if unitInfoV2.logBtnBoxId then
        UI.deleteElement(unitInfoV2.logBtnBoxId)
        unitInfoV2.logBtnBoxId = nil
    end
end

-----------------------------------------------------------
-- Bootstrap: create the page + layout once HUD assets are ready.
-----------------------------------------------------------

local function bootstrap()
    if unitInfoV2.bootstrapped then return end
    if not (hud and hud.boxTexSet and hud.menuFont
            and hud.fbW and hud.fbW > 0) then
        return  -- HUD not ready yet, retry next tick
    end

    unitInfoV2.bootstrapped = true
    unitInfoV2.page = UI.newPage("unit_info_v2", "overlay")

    -- engine.loadTexture caches by path so this returns the same
    -- handle that drag_select / tooltip separators already use.
    unitInfoV2.whitePixelTex = engine.loadTexture(
        "assets/textures/utility/white.png")
    unitInfoV2.tabSelectedTex = engine.loadTexture(
        "assets/textures/ui/unittabselected.png")

    -- Sub-tab look (Status / Stats / Mental / …) re-uses the menu
    -- tab textures so it visually matches the settings + create-world
    -- panels. boxTextures.load caches by path so we're sharing the
    -- same texture set those menus loaded earlier.
    unitInfoV2.subTabSelectedTexSet =
        boxTextures.load("assets/textures/ui/tabselected", "tabselected")
    unitInfoV2.subTabUnselectedTexSet =
        boxTextures.load("assets/textures/ui/tabunselected", "tabunselected")

    -- Take over the unit-info display. The legacy unit watcher
    -- (unit_info_panel.lua) already self-suppresses via the
    -- __unit_info_v2_suppress sentinel, so it never pushes unit content
    -- into the shared HUD info panel while v2 is loaded. We do NOT blanket-
    -- suppress that shared panel here: it is still the renderer for tile /
    -- building / ground-item info, which must keep showing (#136). Instead
    -- suppression is driven dynamically in update() — the shared panel is
    -- hidden only while v2's pane is actually on screen (a unit is
    -- selected), so the two can never visually overlap, and released the
    -- moment the pane hides so tile/building/item readouts reappear.
end

-----------------------------------------------------------
-- Rebuild the layout. Called on bootstrap and on every framebuffer
-- resize (section content itself rebuilds independently, driven by
-- selection/state changes inside update()).
-----------------------------------------------------------

local function rebuildLayout()
    clearOwned()

    local uiscale  = scale.get()
    local panelW   = math.floor(L.PANEL_W * uiscale)
    local outerPad = math.floor(L.PANEL_PAD * uiscale)
    local sectGap  = math.floor(L.SECTION_GAP * uiscale)

    local fbW = hud.fbW
    local fbH = hud.fbH
    local panelX = fbW - panelW
    local panelY = 0
    local panelH = fbH

    -- Outer pane
    unitInfoV2.outerBoxId = UI.newBox(
        "unit_info_v2_outer",
        panelW, panelH,
        hud.boxTexSet,
        L.OUTER_TILE,
        1.0, 1.0, 1.0, 1.0,
        0,
        unitInfoV2.page
    )
    UI.addToPage(unitInfoV2.page, unitInfoV2.outerBoxId, panelX, panelY)
    UI.setZIndex(unitInfoV2.outerBoxId, 10)

    -- Section sizes (scaled)
    local contentX = panelX + outerPad
    local contentW = panelW - 2 * outerPad
    local cursorY  = panelY + outerPad

    local tabsH   = math.floor(L.TABS_H   * uiscale)
    local headerH = math.floor(L.HEADER_H * uiscale)
    local statsH  = math.floor(L.STATS_H  * uiscale)
    local equipH  = math.floor(L.EQUIP_H  * uiscale)
    local dThick  = math.floor(L.DIVIDER_THICKNESS * uiscale)

    -- Helper: lay down a section's content, then a divider beneath it.
    -- The cursorY ends up below the divider, ready for the next
    -- section. The last section in the panel skips its divider.
    local function nextSection(content, h, drawDivider)
        content(contentX, cursorY, contentW, h)
        cursorY = cursorY + h + sectGap
        if drawDivider then
            L.placeDivider(unitInfoV2, contentX, cursorY, contentW, uiscale)
            cursorY = cursorY + dThick + sectGap
        end
    end

    -- Sprite tab strip. We just record the section's rect here;
    -- tabs.rebuildTabs() (driven by update() on selection-identity
    -- change) creates the actual tab + arrow elements inside this rect.
    nextSection(function(x, y, w, h)
        unitInfoV2.tabsRect = { x = x, y = y, w = w, h = h }
    end, tabsH, true)

    -- Header (Name / Type / Role rows)
    nextSection(function(x, y, w, h)
        header.place(x, y, w, h)
    end, headerH, true)

    -- Stats / Physical / Mental / Skills sub-tabs section. We just
    -- record the section's rect; statsMod.rebuildSubTabs (driven by
    -- update()) populates the sub-tab strip + content area inside it.
    nextSection(function(x, y, w, h)
        unitInfoV2.statsRect = { x = x, y = y, w = w, h = h }
    end, statsH, true)

    -- Equipment. Only record the rect here; rebuildEquipmentSection
    -- (driven by update() on selection-identity change) populates the
    -- silhouette + slot overlays + accessory list inside it.
    nextSection(function(x, y, w, h)
        unitInfoV2.equipRect = { x = x, y = y, w = w, h = h }
    end, equipH, true)

    -- Inventory: remaining height, no divider after. Just record the
    -- rect; rebuildInventorySection (driven by update()) populates the
    -- tab strip + item list + total-weight footer inside it.
    local invH = (panelY + panelH - outerPad) - cursorY
    if invH > 0 then
        unitInfoV2.invRect = { x = contentX, y = cursorY,
                               w = contentW, h = invH }
    end
end

-----------------------------------------------------------
-- Public lifecycle
-----------------------------------------------------------

function unitInfoV2.init(scriptId)
    unitInfoV2.scriptId = scriptId
    engine.logInfo("Unit info v2 initialising...")

    -- Suppress the old unit_info_panel push so the existing HUD info
    -- panel stays empty for unit selections (the new pane owns it).
    local oldWatch = package.loaded["scripts.unit_info_panel"]
    if oldWatch then
        oldWatch.suppressed = true
    else
        -- Old module hasn't been required yet; tag it through a
        -- pending flag the old module will check during its own init.
        -- We don't load it here because that'd start its update loop.
        package.loaded.__unit_info_v2_suppress = true
    end
end

function unitInfoV2.update(dt)
    bootstrap()
    if not unitInfoV2.page then return end

    -- Rebuild on first show. For now layout is static so we only need
    -- to build it once, but we re-call on selection-identity change
    -- in later passes so leave the hook here.
    if not unitInfoV2.outerBoxId then
        rebuildLayout()
        statsMod.rebuildSubTabs()
        UI.hidePage(unitInfoV2.page)
    end

    local sel = unit.getSelected()
    local count = (sel and #sel) or 0

    -- Selection-identity change → rebuild the tab strip. selectionKey
    -- is order-insensitive so picking the same units in a different
    -- click order doesn't churn.
    local key = tabs.selectionKey(sel)
    if key ~= unitInfoV2.lastSelKey then
        tabs.rebuildTabs(sel)
        unitInfoV2.lastSelKey = key
        -- Selecting a unit takes ownership of the readout, so clear any
        -- world-tile cursor selection — exactly as the legacy unit
        -- watcher did (unit_info_panel.lua), which is suppressed under v2.
        -- Without this the tile-info watcher keeps a stale tile readout
        -- alive in the (suppressed) shared panel, which then resurfaces
        -- the moment the v2 pane hides and we unsuppress it (#136).
        if count > 0 and hud and hud.worldId then
            world.clearWorldCursorSelect(hud.worldId)
        end
    end

    -- Stats sub-panel: rebuild content when the active unit OR sub-tab
    -- changes; otherwise just refresh in-place values.
    if unitInfoV2.activeUid then
        statsMod.rebuildStatsContent()
        if unitInfoV2.statsRefresh then
            unitInfoV2.statsRefresh(unitInfoV2.activeUid)
        end
    end

    -- Equipment section: rebuild when the active unit changes
    -- (silhouette + slot overlays per the unit's equipment class).
    equipmentMod.rebuildEquipmentSection()

    -- Inventory section: dynamic tab strip + per-tab item list. Cached
    -- by a content-hash so the per-tick redraw is cheap when nothing
    -- has changed.
    inventoryMod.rebuildInventorySection()

    -- Header rows (Name/Type/Role/Action) for the active unit.
    header.refresh()

    -- Refresh every visible tab's portrait.
    tabs.refreshPortraits()

    -- Visibility gate. The pane belongs to the zoomed-in gameplay view,
    -- so a selection alone is not enough to show it: it must also hide
    -- when gameplay UI isn't actually active or when the camera isn't in
    -- the zoomed-in band (zoomed-out map / fade band). Driving show/hide
    -- off a single predicate keeps it in sync with zoom + menu transitions
    -- instead of only selection count, so the pane can no longer persist
    -- over the zoomed-out map or non-gameplay menus (#137).
    --
    -- isGameplayInputActive() is the authoritative "gameplay UI is active"
    -- check (#182): it is false for any non-gameplay currentMenu AND while
    -- the pause overlay is up. That covers the modal paths that DON'T call
    -- hud.hide() — pauseMenu.show() and uiManager.showMenu("settings") in
    -- its keepWorld path — which a bare hud.visible check would miss.
    local uiManager = require("scripts.ui_manager")
    local want = count > 0
                 and uiManager.isGameplayInputActive()
                 and hud.currentView == "zoomed_in"
    if want ~= unitInfoV2.lastWantVisible then
        if want then
            UI.showPage(unitInfoV2.page)
            -- Pane is coming up over the right edge: hide the shared HUD
            -- info panel so a lingering tile/building/item readout can't
            -- overlap it (#136). Released again the moment the pane hides.
            infoPanel.suppress("unit_info_v2")
        else
            UI.hidePage(unitInfoV2.page)
            -- Pane gone: let the shared panel render tile/building/item
            -- info again (refresh re-derives from content + any other
            -- suppressors, so it only resurfaces if it has something).
            infoPanel.unsuppress("unit_info_v2")
        end
        unitInfoV2.lastWantVisible = want
    end
    unitInfoV2.lastSelCount = count
end

function unitInfoV2.onFramebufferResize(width, height)
    -- Layout depends on framebuffer dimensions, so rebuild on resize.
    if unitInfoV2.page then
        rebuildLayout()
        -- Tabs got cleared by clearOwned in rebuildLayout. Re-create
        -- them for the current selection (lastSelKey reset so the next
        -- update tick will see "new" selection and rebuild).
        unitInfoV2.lastSelKey = ""
        -- Re-apply the same visibility gate as update() so a resize while
        -- a menu / pause / settings overlay is open or the camera is zoomed
        -- out doesn't flash the pane back on (#137).
        local uiManager = require("scripts.ui_manager")
        local want = unitInfoV2.lastSelCount > 0
                     and uiManager.isGameplayInputActive()
                     and hud.currentView == "zoomed_in"
        if want then
            UI.showPage(unitInfoV2.page)
            infoPanel.suppress("unit_info_v2")
        else
            UI.hidePage(unitInfoV2.page)
            infoPanel.unsuppress("unit_info_v2")
        end
        unitInfoV2.lastWantVisible = want
    end
end

-- "Log" button → open the per-unit collated log for the active unit.
function unitInfoV2.handleLogClick(elemHandle)
    if elemHandle ~= unitInfoV2.logBtnBoxId then return false end
    if not unitInfoV2.activeUid then return true end
    require("scripts.unit_log").show(unitInfoV2.activeUid)
    return true
end

function unitInfoV2.shutdown()
    -- Re-enable the old watcher in case we're being unloaded.
    local oldWatch = package.loaded["scripts.unit_info_panel"]
    if oldWatch then
        oldWatch.suppressed = false
    end
    -- Also clear the global suppression sentinel. init() sets this when the
    -- legacy watcher isn't loaded yet, and a LATER require of the watcher
    -- reads it to start suppressed — so leaving it set keeps the legacy
    -- panel suppressed forever after v2 shuts down (#87).
    package.loaded.__unit_info_v2_suppress = nil

    -- Restore the shared HUD info panel — we suppressed it on bootstrap
    -- so v2 could own the unit-info display. Releasing re-derives from
    -- content, so it only resurfaces if it has something to show (an
    -- unconditional show would pop an empty/stale panel, #134). If the
    -- HUD is also hidden, its own suppressor keeps the panel down.
    infoPanel.unsuppress("unit_info_v2")

    clearOwned()
    if unitInfoV2.page then
        UI.deletePage(unitInfoV2.page)
        unitInfoV2.page = nil
    end
    unitInfoV2.bootstrapped = false
    unitInfoV2.lastSelCount = 0
    unitInfoV2.lastWantVisible = false
end

return unitInfoV2
