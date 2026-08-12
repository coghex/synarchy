-- Container Window (historically the cargo inventory panel)
--
-- Floating popup showing ONE transfer endpoint's stored items as a
-- tabbed icon list. A BUILDING endpoint is reached by right-click on
-- the cargo → context menu → "Contents"; a UNIT endpoint is reached
-- through this module's public API.
--
-- Since #1088 the tabbed list itself is the shared item-list widget
-- (scripts/ui/item_list.lua) — this module owns only the popup window,
-- its title/subtitle chrome, the data source, the presentation policy
-- it hands the widget, and what "Withdraw" means. Grouping, tabs, rows,
-- truncation and rebuild invalidation all live in the widget, which the
-- unit-info inventory section and the item-contents popup share.
--
-- Since #1234 the window is ENDPOINT-KIND AGNOSTIC: everything that
-- differs between a cargo and an acolyte lives in the ENDPOINTS table
-- below (one live read, one weight label, one optional row action), and
-- nothing else in this file knows which kind is open. An unknown kind
-- is REJECTED rather than assumed, the same way
-- scripts/etymology_panel.lua's own openFor(kind, id) rejects one.
-- A building endpoint's rendered rows, tabs, header and row menu are
-- deliberately unchanged by that generalization.
--
-- Contents are read LIVE on every tick. Last-known contents
-- (building.getContainerKnowledge) belong to UIT-1B and are
-- deliberately not consulted here — mixing the two would make a
-- regression in either impossible to attribute.
--
-- Singleton: opening for a new endpoint destroys the old popup.
-- Pinned at the mouse position when opened; doesn't follow the
-- endpoint if the camera moves (or if a unit walks away). Esc closes;
-- opening again re-opens fresh.
--
-- Public API — an endpoint identity is a KIND plus an id, never a bare
-- building id:
--   openFor(kind, id, mx, my)            — open on this endpoint at
--                                          framebuffer pixel (mx, my);
--                                          false if it isn't one
--   reopenWithTab(kind, id, mx, my, tab) — the resize-restore entry point
--   closeIfOpen()                        — destroy the popup if shown
--   isOpen()                             — bool
--   showRowMenu(item)                    — the open endpoint's row action
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
-- `kind` + `id` are the open endpoint's identity (#1234) — the pair
-- hud.lua snapshots across a resize, not a bare building id.
-- `activeTab` is the panel's own durable selection (also snapshotted,
-- and handed back through reopenWithTab); the rendered tab strip, the
-- rows and the rebuild comparison all belong to the shared item-list
-- widget (#1088), reachable through `listId`.
cargoInventoryPanel.state = cargoInventoryPanel.state or {
    open          = false,
    kind          = nil,
    id            = nil,
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

-- The window's row action for a BUILDING endpoint, unchanged by #1234:
-- withdraw the right-clicked stack into an adjacent selected acolyte.
-- The widget deliberately never learns what "Withdraw" means, so this
-- is the host's answer and belongs to this endpoint kind alone.
local function buildingRowMenu(bid, item)
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
    return items
end

-- A unit's own display text, mirroring buildingRowMenu's #264
-- precedence so the window titles a unit the same way its withdraw
-- menu names one. transferEndpointInfo reports only the species-level
-- displayName, which is the fallback rather than the answer.
local function unitTitle(uid, info)
    local live = unit.getInfo(uid)
    if live then
        if live.name and live.name ~= "" then return live.name end
        if live.displayName and live.displayName ~= "" then
            return live.displayName
        end
        if live.defName then return live.defName end
    end
    if info.displayName and info.displayName ~= "" then
        return info.displayName
    end
    return "Inventory"
end

-----------------------------------------------------------
-- Endpoint kinds (#1234)
--
-- ONE window, two data sources. Each entry answers exactly the
-- questions that differ between kinds; everything below this table is
-- kind-blind. `view` is the SINGLE live read — nil means "not an
-- eligible endpoint", which covers an unknown id, a destroyed
-- instance, and a unit that is not player-commandable alike, so open
-- and the per-tick lifecycle check share one definition of eligible.
--
-- `stillThere` exists only where liveness is a DIFFERENT question from
-- eligibility: a cargo's capacity is def-declared, so a building's
-- popup must not outlive the instance rather than its storage. A kind
-- that omits it is governed by `view` alone.
--
-- `rowMenu` is optional and a kind that omits it renders rows with no
-- right-click action at all (the widget's own `rc=` signature covers
-- that, so the two kinds can never share a stale list). Unit rows have
-- none in this slice: transfer gestures and orders are UIT-2's, and
-- `unit.withdrawFromCargo` is a building-only verb.
-----------------------------------------------------------
local ENDPOINTS = {
    building = {
        weightLabel = "Storage",
        view = function(id)
            local cap = building.getStorageCapacity(id)
            if not cap or cap <= 0 then return nil end
            local binfo = building.getInfo(id)
            return {
                title    = (binfo and (binfo.displayName or binfo.defName))
                             or "Cargo",
                capacity = cap,
                stored   = building.getStorageWeight(id) or 0,
                contents = building.getStorage(id) or {},
            }
        end,
        stillThere = function(id) return building.getInfo(id) ~= nil end,
        rowMenu = buildingRowMenu,
    },
    unit = {
        weightLabel = "Carrying",
        -- `contents` is LOOSE INVENTORY; `storedWeight` deliberately is
        -- not its sum — it is the endpoint's whole recursive load
        -- (inventory + equipment + accessories), measured by the same
        -- rule the capacity gate uses. Both come from the one engine
        -- read so the header and the rows can never disagree about
        -- which instant they describe.
        view = function(id)
            local info = unit.transferEndpointInfo({ kind = "unit", id = id })
            if not info or info.eligible ~= true then return nil end
            return {
                title    = unitTitle(id, info),
                capacity = info.capacity or 0,
                stored   = info.storedWeight or 0,
                contents = info.contents or {},
            }
        end,
    },
}

-- The one live read, and the one place an unknown kind is refused.
local function endpointView(kind, id)
    local def = ENDPOINTS[kind]
    if not def or id == nil then return nil end
    return def.view(id)
end

-- The item-list parameters that describe THIS panel's data and
-- presentation policy. Everything the widget needs to group, tab,
-- render and invalidate; bounds are added by buildLayout once the
-- panel has been sized from the resulting row count.
--
-- The host-owned title/subtitle chrome rides in `presentationKey`
-- because the widget cannot see it: a unit's stored weight moves when
-- it equips something its loose inventory never listed, and without
-- this the header would keep reporting the load it had at open.
local function listDataParams(kind, view, activeTab)
    local def = ENDPOINTS[kind]
    return {
        items     = view.contents,
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
        presentationKey = string.format("%s|%s|%.3f/%.3f", tostring(kind),
                                        tostring(view.title),
                                        view.stored, view.capacity),
        onTabChange     = cargoInventoryPanel.onTabChange,
        onRowRightClick = (def and def.rowMenu)
                            and cargoInventoryPanel.showRowMenu or nil,
    }
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
local function buildTitle(originX, originY, kind, view)
    local s = cargoInventoryPanel.state
    local h = cargoInventoryPanel.hud
    if not h then return end
    local uiscale = scale.get()

    s.titleId = label.new({
        name     = "cargo_inv_title",
        text     = view.title,
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

    local def   = ENDPOINTS[kind]
    local wlabel = (def and def.weightLabel) or "Storage"
    s.subtitleId = label.new({
        name     = "cargo_inv_subtitle",
        text     = string.format("%s: %.2f / %.2f kg", wlabel,
                                 view.stored, view.capacity),
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
local function buildLayout(view)
    local s = cargoInventoryPanel.state
    local h = cargoInventoryPanel.hud
    if not h or not h.page then return end
    local mx, my = s.mx, s.my

    -- Normalize the data ONCE through the shared widget, then size the
    -- panel from the row count it produces. The widget snaps a
    -- no-longer-present selection back to "All"; mirror that into the
    -- panel's own durable activeTab so hud.lua's resize snapshot and
    -- reopenWithTab never carry a dead category forward.
    local dataParams = listDataParams(s.kind, view, s.activeTab)
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
    buildTitle(cx, cy, s.kind, view)

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

-- Open on one endpoint. `kind` is "building" or "unit"; `id` is that
-- kind's own id. Returns true when the window opened.
--
-- The eligibility read comes FIRST, before closeIfOpen — an unknown
-- kind, a vanished id, or a unit that is not player-commandable is
-- refused without creating any panel or list state and without
-- disturbing a window that is already open on a valid endpoint. Same
-- ordering scripts/etymology_panel.lua's openFor uses, and the same
-- ordering the building path already had.
function cargoInventoryPanel.openFor(kind, id, mx, my)
    local view = endpointView(kind, id)
    if not view then return false end
    cargoInventoryPanel.closeIfOpen()
    cargoInventoryPanel.state.open = true
    cargoInventoryPanel.state.kind = kind
    cargoInventoryPanel.state.id   = id
    cargoInventoryPanel.state.mx   = mx
    cargoInventoryPanel.state.my   = my
    buildLayout(view)
    return true
end

-- #750 round-13 review: hud.lua's "resize" teardown (scripts/ui/
-- view_teardown.lua) closes this popup before hud.world_page — which it
-- is mounted on — gets deleted and replaced; a resize/rescale otherwise
-- silently discarded the player's open cargo panel (and which tab they
-- had selected) rather than treating it as the layout-only change #750
-- requires it to survive. hud.lua snapshots isOpen()/state.kind/state.id/
-- mx/my/activeTab BEFORE the teardown runs and calls this to rebuild the
-- SAME panel, on the SAME endpoint and the SAME tab, once its own
-- rebuild is done. Plain openFor() always resets to the "All" tab
-- (closeIfOpen's own reset), so the saved tab is re-applied afterward
-- via the same rebuild path a tab click uses, IF it's still a valid tab
-- for the (possibly changed) current contents.
--
-- A REFUSED open must abandon the whole call, not fall through to the
-- tab step. openFor deliberately leaves an already-open valid window
-- alone when it refuses, so `state` afterwards describes THAT window —
-- continuing here would apply this call's requested tab to an unrelated
-- endpoint the caller never named.
function cargoInventoryPanel.reopenWithTab(kind, id, mx, my, tab)
    if not cargoInventoryPanel.openFor(kind, id, mx, my) then return false end
    local s = cargoInventoryPanel.state
    if not s.open or not tab or tab == s.activeTab then return true end
    local stillValid = false
    for _, t in ipairs(itemList.getTabs(s.listId)) do
        if t.key == tab then stillValid = true; break end
    end
    if stillValid then
        local view = endpointView(s.kind, s.id)
        if not view then return true end
        s.activeTab = tab
        buildLayout(view)
    end
    return true
end

function cargoInventoryPanel.closeIfOpen()
    if not cargoInventoryPanel.state.open then return end
    destroyAll()
    cargoInventoryPanel.state.open      = false
    cargoInventoryPanel.state.kind      = nil
    cargoInventoryPanel.state.id        = nil
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
    local view = endpointView(s.kind, s.id)
    if not view then return end
    s.activeTab = category
    buildLayout(view)
end

-- The widget hands back the exact rendered row's representative
-- instance; the OPEN ENDPOINT'S KIND decides what a row action means,
-- which the widget deliberately never learns. A kind with no row
-- action never reaches here (the widget is handed no callback at all),
-- so the guard below is the belt to that braces.
function cargoInventoryPanel.showRowMenu(item)
    local s = cargoInventoryPanel.state
    if not s.open or not item then return false end
    local def = ENDPOINTS[s.kind]
    if not def or not def.rowMenu then return false end
    local items = def.rowMenu(s.id, item)
    if not items or #items == 0 then return false end

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
-- Per-tick refresh, reading the endpoint's contents LIVE. The rebuild
-- comparison belongs to the shared widget (this panel keeps no hash of
-- its own); the "target went away" check below is host-owned
-- lifecycle, not invalidation — the popup must not outlive the
-- endpoint it describes. A cargo that is demolished, a unit that dies,
-- and a unit that stops being player-commandable all close it.
-----------------------------------------------------------
function cargoInventoryPanel.update(dt)
    local s = cargoInventoryPanel.state
    if not s.open or not s.kind or s.id == nil then return end
    local def  = ENDPOINTS[s.kind]
    local view = def and def.view(s.id)
    if not view or (def.stillThere and not def.stillThere(s.id)) then
        cargoInventoryPanel.closeIfOpen()
        return
    end
    if itemList.isStale(s.listId, listDataParams(s.kind, view, s.activeTab)) then
        buildLayout(view)
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
