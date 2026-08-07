-- Cargo Inventory Panel
--
-- Floating popup showing a cargo building's stored items as a
-- tabbed icon list. Triggered by right-click on the cargo →
-- context menu → "Contents".
--
-- Since #1088 the tabbed list itself is the shared item-list widget
-- (scripts/ui/item_list.lua) — this module owns only the popup window,
-- its title/subtitle chrome, the data source, the presentation policy
-- it hands the widget, and what "Withdraw" means. Grouping, tabs, rows,
-- truncation and rebuild invalidation all live in the widget, which the
-- unit-info inventory section and the item-contents popup share.
--
-- Singleton: opening for a new cargo destroys the old popup.
-- Pinned at the mouse position when opened; doesn't follow the
-- building if the camera moves. Esc closes; clicking the cargo
-- again re-opens fresh.
--
-- Public API:
--   openFor(bid, mx, my)  — open on this cargo at framebuffer pixel (mx, my)
--   closeIfOpen()         — destroy the popup if shown
--   isOpen()              — bool
--
-- Engine script hooks: setup / init / update / shutdown.
--
-- Module is registered in package.loaded so init.lua's right-click
-- handler and ui_manager's click dispatchers all see the same
-- instance even though engine.loadScript uses dofile.

local cargoInventoryPanel =
    package.loaded["scripts.cargo_inventory_panel"] or {}
package.loaded["scripts.cargo_inventory_panel"] = cargoInventoryPanel

local panel       = require("scripts.ui.panel")
local label       = require("scripts.ui.label")
local scale       = require("scripts.ui.scale")
local qualityTier = require("scripts.ui.quality_tier")
local itemList    = require("scripts.ui.item_list")

-----------------------------------------------------------
-- Layout constants. Mirrors unit_info_v2's inventory section so
-- the two read the same visually. Base units; uiscale applied at
-- draw time. Padding clears the 9-patch border art (~16–20 px per
-- side at scale 1) AND leaves visible breathing room — same lesson
-- as the build menu's padding fix.
-----------------------------------------------------------
local PANEL_W_BASE   = 460
local PANEL_PAD_X    = 32
local PANEL_PAD_TOP  = 28
local PANEL_PAD_BOT  = 20
local TITLE_FONT     = 16
local TITLE_H        = 22
local SUBTITLE_FONT  = 13
local SUBTITLE_H     = 18
local TAB_H          = 28
local TAB_TILE       = 16
local TAB_FONT       = 13
local TAB_TEXT_PAD   = 22    -- horizontal pad inside each tab
local TAB_GAP        = 6     -- gap between tabs
local TAB_TEXT_COL   = { 0.0, 0.0, 0.0, 1.0 }
local TAB_SEL_TEXT_COL = { 1.0, 1.0, 1.0, 1.0 }
local ROW_H          = 32
local ROW_PAD        = 2
local ICON_SZ        = 28
local TEXT_PAD       = 12    -- horizontal pad inside each row
local NAME_RIGHT_GAP = 24    -- gap between name and weight columns
local MAX_ROWS       = 10
local TITLE_COL      = { 1.0, 1.0, 1.0, 1.0 }
local SUBTITLE_COL   = { 0.85, 0.85, 0.85, 1.0 }
local ROW_NAME_COL   = { 1.0, 1.0, 1.0, 1.0 }
local ROW_WEIGHT_COL = { 0.85, 0.85, 0.85, 1.0 }

-- Frame-free single-row tab strip, shrunk proportionally when its
-- natural width exceeds the panel's content column (#750 round-8/12).
local CARGO_TABS = {
    mode        = "row",
    shrinkToFit = true,
    tabHeight   = TAB_H,
    tileSize    = TAB_TILE,
    fontSize    = TAB_FONT,
    textPad     = TAB_TEXT_PAD,
    gap         = TAB_GAP,
    textColor         = TAB_TEXT_COL,
    selectedTextColor = TAB_SEL_TEXT_COL,
}

-----------------------------------------------------------
-- State
-----------------------------------------------------------
-- `activeTab` is the panel's own durable selection (hud.lua snapshots
-- it across a resize and hands it back through reopenWithTab); the
-- rendered tab strip, the rows and the rebuild comparison all belong
-- to the shared item-list widget (#1088), reachable through `listId`.
cargoInventoryPanel.state = cargoInventoryPanel.state or {
    open          = false,
    bid           = nil,
    panelId       = nil,
    activeTab     = "All",
    titleId       = nil,
    subtitleId    = nil,
    listId        = nil,
}

cargoInventoryPanel.hud = nil   -- assets set by setup()

-----------------------------------------------------------
-- HUD hookup
-----------------------------------------------------------
function cargoInventoryPanel.setup(opts)
    cargoInventoryPanel.hud = opts
end

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

-- The item-list parameters that describe THIS panel's data and
-- presentation policy. Everything the widget needs to group, tab,
-- render and invalidate; bounds are added by buildLayout once the
-- panel has been sized from the resulting row count.
local function listDataParams(bid, activeTab)
    return {
        items     = building.getStorage(bid) or {},
        activeTab = activeTab,
        uiscale   = scale.get(),
        tabs      = CARGO_TABS,
        maxRows   = MAX_ROWS,
        rowName   = function(g)
            local n = qualityTier.withSuffix(
                g.displayName or g.defName or "?", g)
            if (g.count or 1) > 1 then
                n = string.format("%s ×%d", n, g.count)
            end
            return n
        end,
        rowWeightText = function(g)
            return string.format("%.2f kg", (g.weight or 0) * (g.count or 1))
        end,
        rowColor  = function() return ROW_NAME_COL end,
        onTabChange     = cargoInventoryPanel.onTabChange,
        onRowRightClick = cargoInventoryPanel.showRowMenu,
    }
end

-- Chebyshev tile distance from (utx, uty) to the cargo footprint.
local function chebToFootprint(utx, uty, bx, by, tileW, tileH)
    local dx = 0
    if utx < bx then dx = bx - utx
    elseif utx >= bx + tileW then dx = utx - (bx + tileW - 1) end
    local dy = 0
    if uty < by then dy = by - uty
    elseif uty >= by + tileH then dy = uty - (by + tileH - 1) end
    return math.max(dx, dy)
end

-- One selected acolyte adjacent to the cargo, or nil.
local function adjacentSelectedUnit(bid)
    local sel = unit.getSelected() or {}
    if #sel == 0 then return nil end
    local binfo = building.getInfo(bid)
    if not binfo then return nil end
    local tw = binfo.tileW or 1
    local th = binfo.tileH or 1
    for _, uid in ipairs(sel) do
        local info = unit.getInfo(uid)
        if info then
            local cheb = chebToFootprint(math.floor(info.gridX),
                                         math.floor(info.gridY),
                                         binfo.gridX, binfo.gridY, tw, th)
            if cheb <= 1 then return uid end
        end
    end
    return nil
end

-----------------------------------------------------------
-- Element teardown
-----------------------------------------------------------
local function destroyList()
    local s = cargoInventoryPanel.state
    if s.listId then
        itemList.destroy(s.listId)
        s.listId = nil
    end
end

local function destroyTitle()
    local s = cargoInventoryPanel.state
    if s.titleId    then label.destroy(s.titleId);    s.titleId    = nil end
    if s.subtitleId then label.destroy(s.subtitleId); s.subtitleId = nil end
end

local function destroyAll()
    destroyList()
    destroyTitle()
    local s = cargoInventoryPanel.state
    if s.panelId then
        panel.destroy(s.panelId)
        s.panelId = nil
    end
end

-----------------------------------------------------------
-- Render: title row
-----------------------------------------------------------
local function buildTitle(originX, originY, bid)
    local s = cargoInventoryPanel.state
    local h = cargoInventoryPanel.hud
    if not h then return end
    local uiscale = scale.get()
    local binfo = building.getInfo(bid)
    local name = (binfo and (binfo.displayName or binfo.defName)) or "Cargo"

    s.titleId = label.new({
        name     = "cargo_inv_title",
        text     = name,
        font     = h.menuFont,
        fontSize = TITLE_FONT,
        color    = TITLE_COL,
        page     = h.page,
        uiscale  = uiscale,
    })
    local th = label.getElementHandle(s.titleId)
    UI.addToPage(h.page, th, originX,
                 originY + math.floor(TITLE_FONT * 0.85))
    UI.setZIndex(th, 132)

    local cap  = building.getStorageCapacity(bid) or 0
    local used = building.getStorageWeight(bid)   or 0
    s.subtitleId = label.new({
        name     = "cargo_inv_subtitle",
        text     = string.format("Storage: %.2f / %.2f kg", used, cap),
        font     = h.menuFont,
        fontSize = SUBTITLE_FONT,
        color    = SUBTITLE_COL,
        page     = h.page,
        uiscale  = uiscale,
    })
    local sh = label.getElementHandle(s.subtitleId)
    UI.addToPage(h.page, sh, originX,
                 originY + TITLE_H + math.floor(SUBTITLE_FONT * 0.85))
    UI.setZIndex(sh, 132)
end


-----------------------------------------------------------
-- Open / refresh
-----------------------------------------------------------
local function buildLayout(bid, mx, my)
    local s = cargoInventoryPanel.state
    local h = cargoInventoryPanel.hud
    if not h or not h.page then return end

    -- Normalize the data ONCE through the shared widget, then size the
    -- panel from the row count it produces. The widget snaps a
    -- no-longer-present selection back to "All"; mirror that into the
    -- panel's own durable activeTab so hud.lua's resize snapshot and
    -- reopenWithTab never carry a dead category forward.
    local dataParams = listDataParams(bid, s.activeTab)
    local model = itemList.prepare(dataParams)
    dataParams.model = model
    s.activeTab = model.activeTab

    -- Size the panel.
    local uiscale = scale.get()
    local panelW  = math.floor(PANEL_W_BASE * uiscale)
    local padTop  = math.floor(PANEL_PAD_TOP * uiscale)
    local padBot  = math.floor(PANEL_PAD_BOT * uiscale)
    local titleH  = math.floor(TITLE_H    * uiscale)
    local subH    = math.floor(SUBTITLE_H * uiscale)
    local tabH    = math.floor(TAB_H      * uiscale)
    local rowH    = math.floor(ROW_H      * uiscale)
    local rowPad  = math.floor(ROW_PAD    * uiscale)

    local visibleCount = math.min(#model.visible, MAX_ROWS)
    -- Always reserve one row's height so an empty cargo isn't a flat
    -- strip — easier to read "(empty)" / nothing than a single line.
    if visibleCount < 1 then visibleCount = 1 end

    local rowsH    = visibleCount * rowH + (visibleCount - 1) * rowPad
    local panelH   = padTop + titleH + subH + 6 + tabH + 8 + rowsH + padBot

    -- #750 round-7 review: cap against the actual framebuffer — the
    -- pre-existing px/py clamp below only ever repositions the panel,
    -- never shrinks it, so PANEL_W_BASE*uiscale (460 at 1x, 1840 at a
    -- still-C2-supported 4x) could exceed the framebuffer several times
    -- over regardless of position, leaving tabs/items/actions
    -- off-screen. Best-effort degrade, same pattern as popup.lua/
    -- unit_info_v2.lua/build_tool_remote_warning.lua's earlier fixes.
    if h.fbW then panelW = math.min(panelW, h.fbW) end
    if h.fbH then panelH = math.min(panelH, h.fbH) end

    -- Clamp the panel position to the framebuffer so it doesn't open
    -- partly off-screen if the player right-clicks near an edge.
    local px = mx
    local py = my
    if h.fbW and px + panelW > h.fbW then px = math.max(0, h.fbW - panelW) end
    if h.fbH and py + panelH > h.fbH then py = math.max(0, h.fbH - panelH) end

    -- Recreate or reuse the panel box.
    if s.panelId then panel.destroy(s.panelId); s.panelId = nil end
    s.panelId = panel.new({
        name       = "cargo_inv_panel",
        page       = h.page,
        x          = px,
        y          = py,
        width      = panelW,
        height     = panelH,
        textureSet = h.boxTexSet,
        color      = { 0.1, 0.1, 0.1, 0.95 },
        tileSize   = 64,
        zIndex     = 130,
        padding    = { top = PANEL_PAD_TOP, bottom = PANEL_PAD_BOT,
                       left = PANEL_PAD_X,  right  = PANEL_PAD_X },
        uiscale    = uiscale,
    })
    local pbounds = panel.getContentBounds(s.panelId)
    local cx = px + pbounds.x
    local cy = py + pbounds.y
    local cw = pbounds.width

    destroyTitle()
    destroyList()
    buildTitle(cx, cy, bid)

    dataParams.name         = "cargo_inv"
    dataParams.page         = h.page
    dataParams.font         = h.menuFont
    dataParams.x            = cx
    dataParams.y            = cy + titleH + subH + 6
    dataParams.width        = cw
    dataParams.height       = tabH + 8 + rowsH
    dataParams.tabBottomPadPx = 8   -- literal, matching panelH's own gap
    dataParams.rowHeight    = ROW_H
    dataParams.rowPad       = ROW_PAD
    dataParams.iconSize     = ICON_SZ
    dataParams.textPad      = TEXT_PAD
    dataParams.nameRightGap = NAME_RIGHT_GAP
    dataParams.rowFontSize  = 13
    dataParams.weightColor  = ROW_WEIGHT_COL
    dataParams.zBase        = 132
    s.listId = itemList.new(dataParams)
end

function cargoInventoryPanel.openFor(bid, mx, my)
    if not bid then return end
    local cap = building.getStorageCapacity(bid)
    if not cap or cap <= 0 then return end
    cargoInventoryPanel.closeIfOpen()
    cargoInventoryPanel.state.open = true
    cargoInventoryPanel.state.bid  = bid
    cargoInventoryPanel.state.mx   = mx
    cargoInventoryPanel.state.my   = my
    buildLayout(bid, mx, my)
end

-- #750 round-13 review: hud.lua's "resize" teardown (scripts/ui/
-- view_teardown.lua) closes this popup before hud.world_page — which it
-- is mounted on — gets deleted and replaced; a resize/rescale otherwise
-- silently discarded the player's open cargo panel (and which tab they
-- had selected) rather than treating it as the layout-only change #750
-- requires it to survive. hud.lua snapshots isOpen()/state.bid/mx/my/
-- activeTab BEFORE the teardown runs and calls this to rebuild the SAME
-- panel, on the SAME tab, once its own rebuild is done. Plain openFor()
-- always resets to the "All" tab (closeIfOpen's own reset), so the
-- saved tab is re-applied afterward via the same rebuild path a tab
-- click uses, IF it's still a valid tab for the (possibly changed)
-- current contents.
function cargoInventoryPanel.reopenWithTab(bid, mx, my, tab)
    cargoInventoryPanel.openFor(bid, mx, my)
    local s = cargoInventoryPanel.state
    if not s.open or not tab or tab == s.activeTab then return end
    local stillValid = false
    for _, t in ipairs(itemList.getTabs(s.listId)) do
        if t.key == tab then stillValid = true; break end
    end
    if stillValid then
        s.activeTab = tab
        buildLayout(s.bid, s.mx, s.my)
    end
end

function cargoInventoryPanel.closeIfOpen()
    if not cargoInventoryPanel.state.open then return end
    destroyAll()
    cargoInventoryPanel.state.open      = false
    cargoInventoryPanel.state.bid       = nil
    cargoInventoryPanel.state.activeTab = "All"
end

function cargoInventoryPanel.isOpen()
    return cargoInventoryPanel.state.open == true
end

-----------------------------------------------------------
-- Selection + row actions (routed by the shared widget)
-----------------------------------------------------------

-- The tab strip is scripts/ui/tabbar's, so a click arrives through
-- uiManager.onTabClick like every other tabbar; this only records the
-- panel's own durable selection and rebuilds around it.
function cargoInventoryPanel.onTabChange(category)
    local s = cargoInventoryPanel.state
    if not s.open or category == s.activeTab then return end
    s.activeTab = category
    buildLayout(s.bid, s.mx, s.my)
end

-- The widget hands back the exact rendered row's representative
-- instance; this decides what "Withdraw" means, which the widget
-- deliberately never learns.
function cargoInventoryPanel.showRowMenu(item)
    local s = cargoInventoryPanel.state
    if not s.open or not item then return false end

    local bid     = s.bid
    local defName = item.defName
    local instId  = item.instanceId
    local target  = adjacentSelectedUnit(bid)

    local items = {}
    if target then
        local info = unit.getInfo(target)
        -- Prefer the unit's personal name, else its species label (#264).
        local who  = "unit"
        if info then
            if info.name and info.name ~= "" then
                who = info.name
            elseif info.displayName and info.displayName ~= "" then
                who = info.displayName
            elseif info.defName then
                who = info.defName
            end
        end
        items[1] = {
            label    = "Withdraw with " .. who,
            callback = function()
                unit.withdrawFromCargo(target, bid, defName, instId)
                -- Redraw on the SAME frame rather than waiting for the
                -- next tick's comparison to notice.
                itemList.invalidate(cargoInventoryPanel.state.listId)
            end,
        }
    else
        items[1] = {
            label   = "Withdraw (select an adjacent unit first)",
            enabled = false,
        }
    end

    local contextMenu = require("scripts.ui.context_menu")
    local mx, my = engine.getMousePosition()
    local fbW, fbH = engine.getFramebufferSize()
    local ww, wh = engine.getWindowSize()
    if ww and wh and ww > 0 and wh > 0 then
        mx = mx * (fbW / ww)
        my = my * (fbH / wh)
    end
    contextMenu.show(items, mx, my)
    return true
end

-----------------------------------------------------------
-- Per-tick refresh. The rebuild comparison belongs to the shared
-- widget (this panel keeps no hash of its own); the "target went
-- away" check below is host-owned lifecycle, not invalidation — the
-- popup must not outlive the cargo it describes.
-----------------------------------------------------------
function cargoInventoryPanel.update(dt)
    local s = cargoInventoryPanel.state
    if not s.open or not s.bid then return end
    if not building.getInfo(s.bid) then
        -- Cargo destroyed while popup was open — close.
        cargoInventoryPanel.closeIfOpen()
        return
    end
    if itemList.isStale(s.listId, listDataParams(s.bid, s.activeTab)) then
        buildLayout(s.bid, s.mx, s.my)
    end
end

-----------------------------------------------------------
-- Engine script hooks
-----------------------------------------------------------
function cargoInventoryPanel.init(scriptId)
    engine.logInfo("Cargo inventory panel initializing...")
end

function cargoInventoryPanel.shutdown()
    cargoInventoryPanel.closeIfOpen()
    engine.logInfo("Cargo inventory panel shut down")
end

-- Esc closes the popup. Returns true if consumed.
-- Named handle* (not on*) deliberately: this module is engine-loaded,
-- so an on*-named function would also fire directly on every engine
-- broadcast — double-firing on top of init.lua's ordered forward.
function cargoInventoryPanel.handleKeyDown(key)
    if key == "Escape" and cargoInventoryPanel.state.open then
        cargoInventoryPanel.closeIfOpen()
        return true
    end
    return false
end

return cargoInventoryPanel
