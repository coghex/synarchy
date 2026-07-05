-- Planting screen (#335): the suitability-sorted crop catalogue the
-- plant tool (scripts/plant_tool.lua) opens on a tilled tile. Lists
-- every registered plantable-crop species (world.getPlantSuitability,
-- row_crop + groundcover_crop worldGen categories) best-first, with a
-- search box, a sort mode, and category filter checkboxes over the
-- list. Selecting a row commits plant.designate for THIS tile
-- immediately and closes the screen — the same click-to-act
-- convention scripts/save_browser.lua uses for its save list. The
-- actual planting (AI claim/walk/place) is #336; this screen only
-- records the designation.
--
-- Mounted on hud.world_page, same lifecycle as crafting_panel.lua /
-- cargo_inventory_panel.lua — registered in ui/view_teardown.lua so a
-- view transition can't leave it open-but-invisible.
--
-- Singleton via package.loaded (engine.loadScript uses dofile).
-- Public API: setup(opts), show(pageId, gx, gy), closeIfOpen(),
--             isOpen(), handleKeyDown(key), onScroll(elemHandle,dx,dy).
-- Engine hooks: init, update(dt), shutdown.

local plantPanel = package.loaded["scripts.plant_panel"] or {}
package.loaded["scripts.plant_panel"] = plantPanel

local panel    = require("scripts.ui.panel")
local label    = require("scripts.ui.label")
local button   = require("scripts.ui.button")
local textbox  = require("scripts.ui.textbox")
local checkbox = require("scripts.ui.checkbox")
local list     = require("scripts.ui.list")
local scale    = require("scripts.ui.scale")

-- Sort control is a plain toggle button, not scripts/ui/dropdown.lua:
-- dropdown.new is unexercised by any shipped screen today (grep finds
-- zero live callers — settings_menu/create_world_menu only reference
-- dropdown.destroy/setVisible against an always-empty owned list), and
-- this panel's GUI can't be eyeballed in this headless-only change, so
-- it leans on button.lua (proven throughout crafting_panel/
-- save_browser) instead of being the first real exercise of an
-- untested widget path.
local SORT_LABELS = { score = "Sort: Suitability", name = "Sort: Name" }

-----------------------------------------------------------
-- Layout constants (base units; uiscale applied at draw time)
-----------------------------------------------------------
local PANEL_W_FRAC   = 0.5
local PANEL_H_FRAC   = 0.65
local PAD_X          = 24
local PAD_TOP        = 20
local PAD_BOT        = 20
local TITLE_FONT     = 20
local TITLE_H        = 26
local CLOSE_BTN_SZ   = 30
local CONTROL_H      = 28
local CONTROL_GAP    = 10
local SEARCH_TB_W    = 180
local SORT_BTN_W     = 150
local CB_SIZE        = 20
local CB_LABEL_FONT  = 13
local STATUS_FONT    = 12
local ITEM_H         = 32
local LIST_FONT      = 14

local TITLE_COL   = { 1.0, 1.0, 1.0, 1.0 }
local STATUS_COL  = { 0.75, 0.75, 0.75, 1.0 }

-----------------------------------------------------------
-- State
-----------------------------------------------------------
plantPanel.hud = nil

plantPanel.state = plantPanel.state or {
    open      = false,
    pageId    = nil,
    gx        = nil,
    gy        = nil,
    panelId   = nil,
    chrome    = {},   -- {kind, id} elements sans the list/panel
    listId    = nil,
    allRows   = {},   -- cached world.getPlantSuitability(gx,gy) rows
    searchTbId = nil,
    sortBtnId  = nil,
    sortMode   = "score",
    rowCbId    = nil,
    groundCbId = nil,
    statusLblId = nil,
    lastSearch = nil,
}

function plantPanel.setup(opts)
    plantPanel.hud = opts
end

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

-- "tomato_plant" -> "Tomato Plant". Flora species carry no dedicated
-- display-name field (unlike items) anywhere in the engine/Lua layer;
-- every other consumer (world.getFloraGrowthAt, etc.) shows the raw
-- YAML name, so this is presentation-only humanizing, not a new data
-- concept.
local function humanize(name)
    local out = name:gsub("_", " ")
    out = out:gsub("(%a)([%w']*)", function(first, rest)
        return first:upper() .. rest
    end)
    return out
end

local function categoryLabel(cat)
    if cat == "row_crop" then return "Row crop"
    elseif cat == "groundcover_crop" then return "Groundcover"
    else return cat end
end

-- Filter + sort the cached suitability rows against the current
-- search/sort/category control state. Pure function of state, so it's
-- exercisable directly (no UI.* calls).
function plantPanel.visibleRows(rows, search, sort, showRow, showGround)
    local needle = (search or ""):lower()
    local out = {}
    for _, r in ipairs(rows) do
        local matchesCategory =
            (r.category == "row_crop" and showRow) or
            (r.category == "groundcover_crop" and showGround)
        local matchesSearch = needle == "" or humanize(r.name):lower():find(needle, 1, true)
        if matchesCategory and matchesSearch then
            out[#out + 1] = r
        end
    end
    if sort == "name" then
        table.sort(out, function(a, b) return humanize(a.name) < humanize(b.name) end)
    else
        table.sort(out, function(a, b) return a.score > b.score end)
    end
    return out
end

local function rowText(r)
    local pct = math.floor(r.score * 100 + 0.5)
    return string.format("%s  (%s)  %d%%", humanize(r.name), categoryLabel(r.category), pct)
end

local function itemsFor(rows)
    local items = {}
    for _, r in ipairs(rows) do
        items[#items + 1] = { text = rowText(r), value = r.name }
    end
    return items
end

local function setStatus(count)
    local s = plantPanel.state
    if not s.statusLblId then return end
    if #s.allRows == 0 then
        label.setText(s.statusLblId, "No plantable crops registered.")
    elseif count == 0 then
        label.setText(s.statusLblId, "No crops match the current filters.")
    else
        label.setText(s.statusLblId, "Select a crop to plant here.")
    end
end

-- Rebuild the list widget's items from current control state. NOTE:
-- list.new fixes its visible-slot count from the ITEMS array passed at
-- creation time (list.lua's setItems only refreshes those pre-built
-- slots, it never grows them) — safe here because filtering only ever
-- NARROWS s.allRows, never exceeds it, so the list is always created
-- with the full unfiltered catalogue (see renderUI).
local function refreshList()
    local s = plantPanel.state
    if not s.listId then return end
    local search = textbox.getValue(s.searchTbId) or ""
    local showRow = checkbox.isChecked(s.rowCbId)
    local showGround = checkbox.isChecked(s.groundCbId)
    local rows = plantPanel.visibleRows(s.allRows, search, s.sortMode, showRow, showGround)
    list.setItems(s.listId, itemsFor(rows))
    setStatus(#rows)
    s.lastSearch = search
end

-----------------------------------------------------------
-- Destroy / close
-----------------------------------------------------------
local function destroyOwned()
    local s = plantPanel.state
    if s.listId then
        list.destroy(s.listId)
        s.listId = nil
    end
    for _, e in ipairs(s.chrome) do
        if e.kind == "label" then label.destroy(e.id)
        elseif e.kind == "button" then button.destroy(e.id)
        elseif e.kind == "textbox" then textbox.destroy(e.id)
        elseif e.kind == "checkbox" then checkbox.destroy(e.id)
        end
    end
    s.chrome = {}
    s.searchTbId, s.sortBtnId, s.rowCbId, s.groundCbId, s.statusLblId = nil, nil, nil, nil, nil
    if s.panelId then
        panel.destroy(s.panelId)
        s.panelId = nil
    end
end

function plantPanel.closeIfOpen()
    local s = plantPanel.state
    if not s.open then return end
    destroyOwned()
    s.open = false
    s.pageId, s.gx, s.gy = nil, nil, nil
    s.allRows = {}
end

function plantPanel.isOpen()
    return plantPanel.state.open == true
end

-----------------------------------------------------------
-- Build
-----------------------------------------------------------
local function renderUI()
    local h = plantPanel.hud
    local s = plantPanel.state
    local uiscale = scale.get()
    local fbW, fbH = h.fbW, h.fbH
    local panelW = math.floor(fbW * PANEL_W_FRAC)
    local panelH = math.floor(fbH * PANEL_H_FRAC)
    local panelX = math.floor((fbW - panelW) / 2)
    local panelY = math.floor((fbH - panelH) / 2)

    s.panelId = panel.new({
        name = "plant_panel", page = h.page,
        x = panelX, y = panelY, width = panelW, height = panelH,
        textureSet = h.boxTexSet, color = { 0.1, 0.1, 0.1, 0.92 },
        tileSize = 64, zIndex = 200,
        padding = { top = PAD_TOP, bottom = PAD_BOT, left = PAD_X, right = PAD_X },
        uiscale = uiscale,
    })
    local b = panel.getContentBounds(s.panelId)
    local cx, cy, cw = panelX + b.x, panelY + b.y, b.width

    local titleId = label.new({
        name = "plant_panel_title",
        text = string.format("Plant Crop -- (%d, %d)", s.gx, s.gy),
        font = h.menuFont, fontSize = TITLE_FONT, color = TITLE_COL,
        page = h.page, uiscale = uiscale,
        x = cx, y = cy + math.floor(TITLE_FONT * uiscale),
    })
    table.insert(s.chrome, { kind = "label", id = titleId })

    local closeSz = math.floor(CLOSE_BTN_SZ * uiscale)
    local closeId = button.new({
        name = "plant_panel_close", text = "X",
        x = cx + cw - closeSz, y = cy,
        width = CLOSE_BTN_SZ, height = CLOSE_BTN_SZ,
        fontSize = 15, uiscale = uiscale,
        page = h.page, font = h.menuFont, textureSet = h.boxTexSet,
        bgColor = { 1.0, 1.0, 1.0, 1.0 }, textColor = { 1.0, 1.0, 1.0, 1.0 },
        zIndex = 202,
        onClick = function() plantPanel.closeIfOpen() end,
    })
    table.insert(s.chrome, { kind = "button", id = closeId })

    local controlsY = cy + math.floor(TITLE_H * uiscale) + math.floor(6 * uiscale)

    s.searchTbId = textbox.new({
        name = "plant_panel_search", x = cx, y = controlsY,
        width = SEARCH_TB_W, height = CONTROL_H, fontSize = 13,
        page = h.page, font = h.menuFont, uiscale = uiscale,
        default = "", zIndex = 201,
    })
    table.insert(s.chrome, { kind = "textbox", id = s.searchTbId })

    local sortX = cx + math.floor((SEARCH_TB_W + CONTROL_GAP) * uiscale)
    s.sortBtnId = button.new({
        name = "plant_panel_sort", text = SORT_LABELS[s.sortMode],
        x = sortX, y = controlsY, width = SORT_BTN_W, height = CONTROL_H,
        fontSize = 13, uiscale = uiscale, page = h.page, font = h.menuFont,
        textureSet = h.boxTexSet,
        bgColor = { 1.0, 1.0, 1.0, 1.0 }, textColor = { 1.0, 1.0, 1.0, 1.0 },
        zIndex = 201,
        onClick = function()
            local st = plantPanel.state
            st.sortMode = (st.sortMode == "score") and "name" or "score"
            button.setText(st.sortBtnId, SORT_LABELS[st.sortMode])
            refreshList()
        end,
    })
    table.insert(s.chrome, { kind = "button", id = s.sortBtnId })

    local cbY = controlsY + math.floor((CONTROL_H + CONTROL_GAP) * uiscale)
    local cbSz = math.floor(CB_SIZE * uiscale)

    s.rowCbId = checkbox.new({
        name = "plant_panel_row_cb", x = cx, y = cbY, size = CB_SIZE,
        page = h.page, uiscale = uiscale, default = true, zIndex = 201,
        onChange = function() refreshList() end,
    })
    table.insert(s.chrome, { kind = "checkbox", id = s.rowCbId })
    local rowLblId = label.new({
        name = "plant_panel_row_lbl", text = "Row crops",
        font = h.menuFont, fontSize = CB_LABEL_FONT, color = STATUS_COL,
        page = h.page, uiscale = uiscale,
        x = cx + cbSz + math.floor(6 * uiscale),
        y = cbY + math.floor(cbSz * 0.75),
    })
    table.insert(s.chrome, { kind = "label", id = rowLblId })
    local rowLblW = select(1, label.getSize(rowLblId))

    local groundX = cx + cbSz + math.floor(6 * uiscale) + rowLblW + math.floor(16 * uiscale)
    s.groundCbId = checkbox.new({
        name = "plant_panel_ground_cb", x = groundX, y = cbY, size = CB_SIZE,
        page = h.page, uiscale = uiscale, default = true, zIndex = 201,
        onChange = function() refreshList() end,
    })
    table.insert(s.chrome, { kind = "checkbox", id = s.groundCbId })
    local groundLblId = label.new({
        name = "plant_panel_ground_lbl", text = "Groundcover",
        font = h.menuFont, fontSize = CB_LABEL_FONT, color = STATUS_COL,
        page = h.page, uiscale = uiscale,
        x = groundX + cbSz + math.floor(6 * uiscale),
        y = cbY + math.floor(cbSz * 0.75),
    })
    table.insert(s.chrome, { kind = "label", id = groundLblId })

    local listY = cbY + math.floor((CB_SIZE + CONTROL_GAP + 4) * uiscale)
    local statusY = panelY + panelH - PAD_BOT - math.floor(4 * uiscale)
    local listHeightPx = statusY - listY - math.floor(10 * uiscale)
    local maxVisible = math.max(1, math.floor(listHeightPx / (ITEM_H * uiscale)))

    s.statusLblId = label.new({
        name = "plant_panel_status", text = "",
        font = h.menuFont, fontSize = STATUS_FONT, color = STATUS_COL,
        page = h.page, uiscale = uiscale, x = cx, y = statusY,
    })
    table.insert(s.chrome, { kind = "label", id = s.statusLblId })

    -- Seed with the FULL unfiltered/default-sorted catalogue (both
    -- category checkboxes default true, search empty) — this fixes the
    -- list's max slot count for its lifetime, see refreshList's note.
    local initialRows = plantPanel.visibleRows(s.allRows, "", "score", true, true)

    s.listId = list.new({
        name = "plant_panel_list", page = h.page,
        x = cx, y = listY, width = cw,
        font = h.menuFont, fontSize = LIST_FONT,
        itemHeight = ITEM_H, textPadding = 12,
        scrollButtonSize = 22, maxVisible = maxVisible,
        uiscale = uiscale, zIndex = 201,
        items = itemsFor(initialRows),
        textColor = { 1.0, 1.0, 1.0, 1.0 },
        highlightColor = { 0.3, 0.5, 0.8, 0.8 },
        highlightTextColor = { 1.0, 1.0, 1.0, 1.0 },
        selectedColor = { 0.2, 0.4, 0.7, 1.0 },
        selectedTextColor = { 1.0, 1.0, 1.0, 1.0 },
        onSelect = function(value, text, index, listId, listName)
            local st = plantPanel.state
            plant.designate(st.pageId, st.gx, st.gy, value)
            plantPanel.closeIfOpen()
        end,
    })
    setStatus(#initialRows)
    s.lastSearch = ""
end

-----------------------------------------------------------
-- Public lifecycle
-----------------------------------------------------------

-- Open (or refresh) the planting screen for a tilled tile. Refused if
-- assets aren't ready, or the tile isn't plantable (the tool already
-- checks world.isPlantable before calling this, but a re-check here
-- keeps this function safe to call directly, e.g. from tests).
function plantPanel.show(pageId, gx, gy)
    if not plantPanel.hud or not pageId or not gx or not gy then return end
    if not world.isPlantable(gx, gy) then return end
    destroyOwned()
    local s = plantPanel.state
    s.pageId, s.gx, s.gy = pageId, gx, gy
    s.open = true
    s.sortMode = "score"
    s.allRows = world.getPlantSuitability(gx, gy) or {}
    renderUI()
end

-- Esc closes the screen. Returns true if consumed (init.lua's Escape
-- cascade, same tier as crafting_panel / cargo_inventory_panel).
function plantPanel.handleKeyDown(key)
    if key == "Escape" and plantPanel.state.open then
        plantPanel.closeIfOpen()
        return true
    end
    return false
end

-- Mouse-wheel forwarding (the list's internal scrollbar) — ui_manager
-- forwards here unconditionally like combatLog/injuryLog/unitLog,
-- gated on isOpen() below.
function plantPanel.onScroll(elemHandle, dx, dy)
    local s = plantPanel.state
    if not s.open or not s.listId then return false end
    return list.onScroll(elemHandle, dx, dy)
end

-----------------------------------------------------------
-- Engine script hooks
-----------------------------------------------------------
function plantPanel.init(scriptId)
    engine.logInfo("Plant panel initializing...")
end

-- Poll the search box each tick and rebuild the list only when the
-- text actually changed (textbox has no onChange callback, so this is
-- the same polling idiom crafting_panel's refreshTimer uses for its
-- recipe ground-stock readiness). Sort/category controls are
-- button/checkbox clicks and rebuild the list directly on their own
-- onClick/onChange, no polling needed.
function plantPanel.update(dt)
    local s = plantPanel.state
    if not s.open then return end
    local search = textbox.getValue(s.searchTbId) or ""
    if search ~= s.lastSearch then
        refreshList()
    end
end

function plantPanel.shutdown()
    plantPanel.closeIfOpen()
    engine.logInfo("Plant panel shut down")
end

return plantPanel
