-- Container Window: pane renderer (#2155)
--
-- Split out of scripts/cargo_inventory_panel.lua, which remains THE
-- container-window manager and the only engine-loaded script of the
-- three. This module is `require`-only: it is NOT registered in
-- scripts/init_loader.lua or scripts/ui/view_teardown.lua and defines
-- no `on*`-named function, because an engine-loaded module double-fires
-- those on top of init.lua's ordered forward.
--
-- What lives here is the GENERIC pane: the window and row layout
-- constants, header baselines and the title/subtitle/age labels, the
-- item-list parameter completion, pane measurement and placement, panel
-- and element teardown, the row context menu, scroll capture, and the
-- one- and multi-pane build.
--
-- This module is LEVEL-KIND AGNOSTIC. It never resolves a level kind,
-- never reads a building or a unit, never touches the level stack, and
-- never imports scripts/cargo_inventory_panel. Everything it cannot
-- derive from the descriptor, the view, the pane and the HUD assets it
-- is handed arrives through `ctl` — the narrow controller table the
-- manager composes (see `Controller contract` below).
--
-- It DOES consume the endpoint owner, and only for presentation:
-- `ageText` has exactly one owner there, and the height this module
-- reserves for that line must be keyed on the very same answer the
-- label is drawn from or the list overlaps the header. That is the one
-- permitted edge, and it runs render → endpoints, never back.
--
-- Controller contract (`ctl`), all optional-free — the manager supplies
-- every field:
--   ctl.hud()                       HUD assets (fonts, textures, fbW/fbH)
--   ctl.page(level)                 the page a level's elements live on
--   ctl.panesOf(level)              that level's panes, in render order
--   ctl.paneWidgetName(pane)        the pane-unique element-name prefix
--   ctl.mainPaneKey                 the key a single-pane level is
--                                   addressed by
--   ctl.onTabChange(level, pane, c) record a pane's tab selection
--   ctl.openLevel(src, mx, my, i)   open a child level below level `i`

local panel    = require("scripts.ui.panel")
local label    = require("scripts.ui.label")
local scale    = require("scripts.ui.scale")
local itemList = require("scripts.ui.item_list")
local endpoints = require("scripts.cargo_inventory_endpoints")

local cargoInventoryRender = {}

-----------------------------------------------------------
-- Layout constants. Mirrors unit_info_v2's inventory section so
-- the two read the same visually. Base units; uiscale applied at
-- draw time. Padding clears the 9-patch border art (~16–20 px per
-- side at scale 1) AND leaves visible breathing room — same lesson
-- as the build menu's padding fix.
--
-- Single-owned here since #2155, including the two that endpoint
-- policy also needs (`CARGO_TABS` and `ROW_NAME_COL`): the endpoint
-- owner may not import this module, so the manager reads them through
-- the accessors below and injects them as values.
-----------------------------------------------------------
local PANEL_PAD_X    = 32
local PANEL_PAD_TOP  = 28
local PANEL_PAD_BOT  = 20
local TITLE_FONT     = 16
local TITLE_H        = 22
local SUBTITLE_FONT  = 13
local SUBTITLE_H     = 18
local AGE_FONT       = 12
local AGE_H          = 16
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
local TITLE_COL      = { 1.0, 1.0, 1.0, 1.0 }
local SUBTITLE_COL   = { 0.85, 0.85, 0.85, 1.0 }
local AGE_COL        = { 0.70, 0.70, 0.70, 1.0 }
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

-- The two style values endpoint policy needs. The manager reads them
-- here and hands them to the endpoint owner, which must not import this
-- module (#2155 requirement 8).
function cargoInventoryRender.tabSpec()      return CARGO_TABS end
function cargoInventoryRender.rowNameColor() return ROW_NAME_COL end

-----------------------------------------------------------
-- Teardown
--
-- Widget handles only: the PAGE is the manager's (a level owns its
-- LayerModal page, and UI.deletePage must follow these calls, never
-- precede them, or the registries would point at handles the manager
-- has already forgotten).
-----------------------------------------------------------
function cargoInventoryRender.destroyPaneElements(pane)
    if pane.listId then itemList.destroy(pane.listId); pane.listId = nil end
    if pane.titleId    then label.destroy(pane.titleId);    pane.titleId = nil end
    if pane.subtitleId then label.destroy(pane.subtitleId); pane.subtitleId = nil end
    if pane.ageId      then label.destroy(pane.ageId);      pane.ageId = nil end
    if pane.panelId    then panel.destroy(pane.panelId);    pane.panelId = nil end
end

function cargoInventoryRender.destroyLevelElements(panes)
    for _, pane in ipairs(panes) do
        cargoInventoryRender.destroyPaneElements(pane)
    end
end

-----------------------------------------------------------
-- Scroll capture
--
-- Only the DEEPEST level captures the wheel. Scroll routing (#744)
-- picks the topmost in-scope scroll-capturing surface, and the modal
-- boundary already puts shallower levels out of scope — but the BASE
-- level is not behind any boundary when it is alone, so its capture
-- has to be released explicitly the moment a deeper level opens.
--
-- The stack is READ, never held: `levels` is the manager's array, and
-- the deepest index is the only fact this needs from it.
-----------------------------------------------------------
function cargoInventoryRender.applyScrollCapture(ctl, levels)
    for i, level in ipairs(levels) do
        for _, pane in ipairs(ctl.panesOf(level)) do
            if pane.panelId then
                local h = panel.getBoxHandle(pane.panelId)
                if h then UI.setScrollCapture(h, i == #levels) end
            end
        end
    end
end

-----------------------------------------------------------
-- Render: title row
-----------------------------------------------------------
-- Every header baseline is measured in SCALED units, matching the
-- scaled band heights measurePane reserves (titleH/subH/ageH) and the
-- scaled font label.new actually rasterises. Round-1 review of #1237:
-- the pre-#1237 code advanced by the RAW TITLE_H/SUBTITLE_H constants
-- and offset each baseline by a raw fontSize, so above 1x the lines
-- advanced more slowly than their own glyphs grew — at 2x the third
-- line's glyph mass reached back up into the second's, and the reserved
-- space below stayed empty. Identical arithmetic at uiscale 1.
--
-- A text element's position IS its baseline and its glyph mass sits
-- ABOVE it (scripts/ui/label.lua), so band N's baseline is its band top
-- plus a fraction of the scaled font — the ascent — not its band
-- height.
local function headerBaselines(uiscale)
    local titleH = math.floor(TITLE_H    * uiscale)
    local subH   = math.floor(SUBTITLE_H * uiscale)
    return math.floor(TITLE_FONT * uiscale * 0.85),
           titleH + math.floor(SUBTITLE_FONT * uiscale * 0.85),
           titleH + subH + math.floor(AGE_FONT * uiscale * 0.85)
end

-- `uiscale` is the PANE's effective scale, not the configured one
-- (#1250 review round 2). measurePane reserves the header's three bands
-- at that scale and buildPane places the list below them from the same
-- number, so rasterising the labels at any other scale would draw
-- glyphs into space that was never measured for them — at 800x600 the
-- fitted escort boxes and lists shrink while full-size headers would
-- reach down into the rows. One scale in, three bands and three labels
-- out.
local function buildTitle(ctl, level, pane, originX, originY, view, uiscale,
                          name)
    local h = ctl.hud()
    if not h then return end
    local page = ctl.page(level)
    local titleBase, subBase, ageBase = headerBaselines(uiscale)

    pane.titleId = label.new({
        name     = name .. "_title",
        text     = view.title,
        font     = h.menuFont,
        fontSize = TITLE_FONT,
        color    = TITLE_COL,
        page     = page,
        uiscale  = uiscale,
    })
    local th = label.getElementHandle(pane.titleId)
    UI.addToPage(page, th, originX, originY + titleBase)
    UI.setZIndex(th, 132)

    pane.subtitleId = label.new({
        name     = name .. "_subtitle",
        text     = view.subtitle or "",
        font     = h.menuFont,
        fontSize = SUBTITLE_FONT,
        color    = SUBTITLE_COL,
        page     = page,
        uiscale  = uiscale,
    })
    local sh = label.getElementHandle(pane.subtitleId)
    UI.addToPage(page, sh, originX, originY + subBase)
    UI.setZIndex(sh, 132)

    -- The "as of…" line exists only for a snapshot there is an
    -- observation time for, so a live endpoint and a never-inspected
    -- container both render no third line at all — and measurePane
    -- reserves its height from the SAME predicate (ageLineHeight below),
    -- so the panel can never size for a line it does not draw.
    local age = endpoints.ageText(view)
    if age then
        pane.ageId = label.new({
            name     = name .. "_age",
            text     = age,
            font     = h.menuFont,
            fontSize = AGE_FONT,
            color    = AGE_COL,
            page     = page,
            uiscale  = uiscale,
        })
        local ah = label.getElementHandle(pane.ageId)
        UI.addToPage(page, ah, originX, originY + ageBase)
        UI.setZIndex(ah, 132)
    end
end

-- The vertical space measurePane must reserve for that line. Keyed on
-- the same endpoints.ageText() answer buildTitle draws from, never on
-- the state alone: they must agree or the list overlaps the header,
-- which is why the measure and the draw are single-owned in THIS module
-- (#2155) even though the string itself is the endpoint owner's.
local function ageLineHeight(view, uiscale)
    if not endpoints.ageText(view) then return 0 end
    return math.floor(AGE_H * uiscale)
end

-----------------------------------------------------------
-- Row actions
--
-- ONE menu builder for every level kind. The kind's own transfer action
-- comes first (an endpoint row's "Retrieve" entries, #1249); "Contents"
-- is APPENDED for an item-container row, which is inspection rather
-- than transfer and is therefore offered on every kind — including the
-- item-container levels themselves, which is what makes the stack nest
-- arbitrarily deep. A kind with neither produces no menu at all, so the
-- widget's right-click resolves to nothing rather than an empty popup.
-----------------------------------------------------------
local function rowIsContainer(row)
    return row ~= nil and row.kind == "container"
       and type(row.instanceId) == "number" and row.instanceId > 0
end

-- Framebuffer-space mouse position: engine.getMousePosition reports
-- WINDOW pixels and every placement here is in framebuffer pixels, so
-- the two differ by the backing-scale ratio on a HiDPI display.
local function mouseFramebufferPos()
    local mx, my = engine.getMousePosition()
    local fbW, fbH = engine.getFramebufferSize()
    local ww, wh = engine.getWindowSize()
    if ww and wh and ww > 0 and wh > 0 then
        mx = mx * (fbW / ww)
        my = my * (fbH / wh)
    end
    return mx, my
end

-- A level kind's transfer action takes the level's SOURCE, the row and
-- the PANE the row was rendered in, and nothing else. It used to be
-- handed an `invalidate` closure too, because the retired withdraw
-- entry moved an item on the spot and wanted the list redrawn on the
-- same frame; since #1249 a Mode B transfer gesture only QUEUES an
-- order, so no contents change when it fires and there is nothing to
-- invalidate. The movement lands when the executor arrives, and the
-- manager's update() per-tick re-read is what shows it. A Mode A escort
-- commit (#1250) DOES move items immediately, and it reaches the same
-- per-tick re-read the same way rather than reviving that closure.
--
-- The pane key is what tells a two-paned level which of its endpoints
-- the player right-clicked, and it is passed to every single-pane kind
-- as well (as the controller's main pane key) so the hook signature
-- never varies.
local function rowMenuFor(ctl, kind, level, pane, row)
    if not kind then return nil end
    local paneKey = pane.paneKey or ctl.mainPaneKey
    local items = (kind.transferMenu
                   and kind.transferMenu(level.src, row, paneKey)) or {}
    if rowIsContainer(row) and kind.childOf then
        local childSrc = kind.childOf(level.src, row, paneKey)
        if childSrc then
            local index = level.index
            items[#items + 1] = {
                label    = "Contents",
                callback = function()
                    local mx, my = mouseFramebufferPos()
                    ctl.openLevel(childSrc, mx, my, index)
                end,
            }
        end
    end
    if #items == 0 then return nil end
    return items
end

-- The widget hands back the exact rendered row's representative
-- instance; the LEVEL decides what a row action means, which the widget
-- deliberately never learns.
function cargoInventoryRender.showRowMenu(ctl, kind, level, pane, row)
    if not row then return false end
    local items = rowMenuFor(ctl, kind, level, pane, row)
    if not items or #items == 0 then return false end

    local contextMenu = require("scripts.ui.context_menu")
    local mx, my = mouseFramebufferPos()
    contextMenu.show(items, mx, my)
    return true
end

-----------------------------------------------------------
-- Build one level
-----------------------------------------------------------

-- Full item-list params: the level kind's presentation policy plus the
-- pieces every pane shares (tabs, row cap, scroll offset, routing).
function cargoInventoryRender.listDataParams(ctl, kind, level, pane, view)
    local paneKey = pane.paneKey or ctl.mainPaneKey
    local p = kind.listParams(level.src, view, paneKey)
    p.activeTab    = pane.activeTab
    p.tabs         = kind.tabs or false
    p.maxRows      = kind.maxRows
    p.scrollOffset = pane.scroll or 0
    p.onTabChange  = kind.tabs and function(category)
        ctl.onTabChange(level, pane, category)
    end or nil
    -- Every pane routes right-clicks: even a kind with no transfer
    -- action can offer "Contents", and the widget's `rc=` signature
    -- means the callback's presence is part of the rebuild comparison.
    p.onRowRightClick = function(row)
        return cargoInventoryRender.showRowMenu(ctl, kind, level, pane, row)
    end
    return p
end

-- Everything about ONE pane that depends only on its own data: the
-- normalized item-list model and the panel size that model implies.
-- Separated from drawing because a multi-pane level has to know EVERY
-- pane's size before it can place ANY of them (they avoid each other).
local function measurePane(ctl, kind, level, pane, view)
    local h = ctl.hud()

    -- Normalize the data ONCE through the shared widget, then size the
    -- panel from the row count it produces. The widget snaps a
    -- no-longer-present selection back to "All"; mirror that into the
    -- pane's own durable activeTab so the resize snapshot never
    -- carries a dead category forward.
    local dataParams =
        cargoInventoryRender.listDataParams(ctl, kind, level, pane, view)
    local model = itemList.prepare(dataParams)
    dataParams.model = model
    pane.activeTab = model.activeTab

    -- A kind may render its panes at a LOCAL effective uiscale rather
    -- than the configured one (#1250): the escort pair has to fit TWO
    -- panels side by side, which is a constraint no single pane can see.
    -- Its own listParams sets the same value on the widget, so the box
    -- and the text inside it shrink together (#750).
    local uiscale = (kind.paneScale and kind.paneScale(level, pane.paneKey))
                      or scale.get()
    local panelW  = math.floor(kind.panelWidthBase * uiscale)
    local padTop  = math.floor(PANEL_PAD_TOP * uiscale)
    local padBot  = math.floor(PANEL_PAD_BOT * uiscale)
    local titleH  = math.floor(TITLE_H    * uiscale)
    local subH    = math.floor(SUBTITLE_H * uiscale)
    local ageH    = ageLineHeight(view, uiscale)
    local tabH    = kind.tabs and math.floor(TAB_H * uiscale) or 0
    local tabPad  = kind.tabs and 8 or 0
    local rowH    = math.floor(ROW_H      * uiscale)
    local rowPad  = math.floor(ROW_PAD    * uiscale)

    local visibleCount = math.min(#model.visible, kind.maxRows)
    -- Always reserve one row's height so an empty container isn't a
    -- flat strip — easier to read "(empty)" / nothing than a single
    -- line.
    if visibleCount < 1 then visibleCount = 1 end

    local rowsH    = visibleCount * rowH + (visibleCount - 1) * rowPad
    local panelH   = padTop + titleH + subH + ageH + 6 + tabH + tabPad
                       + rowsH + padBot

    -- #750 round-7 review: cap against the actual framebuffer — the
    -- position clamp below only ever repositions the panel,
    -- never shrinks it, so panelWidthBase*uiscale (460 at 1x, 1840 at a
    -- still-C2-supported 4x) could exceed the framebuffer several times
    -- over regardless of position, leaving tabs/items/actions
    -- off-screen. Best-effort degrade, same pattern as popup.lua/
    -- unit_info_v2.lua/build_tool_remote_warning.lua's earlier fixes.
    if h.fbW then panelW = math.min(panelW, h.fbW) end
    if h.fbH then panelH = math.min(panelH, h.fbH) end

    return { dataParams = dataParams, w = panelW, h = panelH,
             titleH = titleH, subH = subH, ageH = ageH,
             tabH = tabH, tabPad = tabPad, rowsH = rowsH,
             uiscale = uiscale }
end

-- Where each pane's panel goes. A kind that supplies `placePanes` owns
-- the answer completely (the escort pair flanks the screen centre and
-- keeps clear of its sibling); every other kind keeps the pre-#1250
-- rule verbatim — anchored at the level's own (mx, my), clamped to the
-- framebuffer so it doesn't open partly off-screen when the player
-- right-clicks near an edge.
local function placePanes(ctl, kind, level, measures)
    local h = ctl.hud()
    if kind.placePanes then
        return kind.placePanes(level, measures, h)
    end
    local out = {}
    for i, m in ipairs(measures) do
        local px, py = level.mx, level.my
        if h.fbW and px + m.w > h.fbW then px = math.max(0, h.fbW - m.w) end
        if h.fbH and py + m.h > h.fbH then py = math.max(0, h.fbH - m.h) end
        out[i] = { x = px, y = py }
    end
    return out
end

local function buildPane(ctl, kind, level, pane, view, m, pos)
    local h = ctl.hud()
    local page = ctl.page(level)
    local dataParams = m.dataParams
    -- Every UI element this pane creates is named from here, and the
    -- name has to be PANE-UNIQUE (#1250 review round 3). Control focus
    -- survives a geometry rebuild by NAME
    -- (responsive.snapshotControlFocusName /
    -- restoreControlFocusName, which restores the FIRST visible match),
    -- so two panes sharing one widget name means focus parked on the
    -- destination pane's tab comes back on the source pane's — a
    -- silently wrong control, not a missing one. The rule itself is the
    -- manager's single `paneWidgetName`, reached through the controller.
    local widgetName = ctl.paneWidgetName(pane)

    cargoInventoryRender.destroyPaneElements(pane)
    pane.panelId = panel.new({
        name       = widgetName .. "_panel",
        page       = page,
        x          = pos.x,
        y          = pos.y,
        width      = m.w,
        height     = m.h,
        textureSet = h.boxTexSet,
        color      = { 0.1, 0.1, 0.1, 0.95 },
        tileSize   = 64,
        zIndex     = 130,
        padding    = { top = PANEL_PAD_TOP, bottom = PANEL_PAD_BOT,
                       left = PANEL_PAD_X,  right  = PANEL_PAD_X },
        uiscale    = m.uiscale,
    })
    local pbounds = panel.getContentBounds(pane.panelId)
    local cx = pos.x + pbounds.x
    local cy = pos.y + pbounds.y
    local cw = pbounds.width

    buildTitle(ctl, level, pane, cx, cy, view, m.uiscale, widgetName)

    dataParams.name         = widgetName
    dataParams.page         = page
    dataParams.font         = h.menuFont
    dataParams.x            = cx
    dataParams.y            = cy + m.titleH + m.subH + m.ageH + 6
    dataParams.width        = cw
    dataParams.height       = m.tabH + m.tabPad + m.rowsH
    if kind.tabs then
        dataParams.tabBottomPadPx = 8  -- literal, matching panelH's gap
    end
    dataParams.rowHeight    = ROW_H
    dataParams.rowPad       = ROW_PAD
    dataParams.iconSize     = ICON_SZ
    dataParams.textPad      = TEXT_PAD
    dataParams.nameRightGap = NAME_RIGHT_GAP
    dataParams.rowFontSize  = 13
    dataParams.weightColor  = ROW_WEIGHT_COL
    dataParams.zBase        = 132
    pane.listId = itemList.new(dataParams)
    -- The widget owns the scroll clamp (only it knows the visible
    -- capacity), so the pane's own durable offset takes its answer
    -- back — otherwise a pane restored against shrunken contents would
    -- keep re-requesting an offset it can never have.
    pane.scroll = itemList.getScrollOffset(pane.listId)
end

-- Build (or rebuild) every pane of one level. `views` is one view per
-- pane, in pane order — read by the caller, because whether a level
-- survives at all is decided by ALL of its panes together.
--
-- `levels` is the manager's stack, passed only so the scroll capture
-- can be re-applied against the current depth once every pane exists.
function cargoInventoryRender.buildLevel(ctl, kind, level, views, levels)
    local h = ctl.hud()
    if not h or not ctl.page(level) then return end

    local panes = ctl.panesOf(level)
    local measures = {}
    for i, pane in ipairs(panes) do
        measures[i] = measurePane(ctl, kind, level, pane, views[i])
    end
    local positions = placePanes(ctl, kind, level, measures)
    for i, pane in ipairs(panes) do
        buildPane(ctl, kind, level, pane, views[i], measures[i],
                  positions[i] or { x = 0, y = 0 })
    end
    cargoInventoryRender.applyScrollCapture(ctl, levels)
end

return cargoInventoryRender
