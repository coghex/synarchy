-- Planting screen (#335): the suitability-sorted crop catalogue the
-- plant tool (scripts/plant_tool.lua) opens on a tilled tile. Two
-- columns: left is a search box, a sort mode, category filter
-- checkboxes, and a scrollable crop list (world.getPlantSuitability,
-- row_crop + groundcover_crop worldGen categories); right is the
-- per-factor suitability read-out for whichever crop is highlighted
-- (temperature/precipitation/humidity/altitude/slope/soil — why it's
-- good/bad HERE, not just a bare percentage) plus a "Plant here"
-- button that commits. Clicking a list row only PREVIEWS it (updates
-- the right column) — designating needs the explicit button, since
-- the whole point of the read-out is to see it before committing.
-- The actual planting (AI claim/walk/place) is #336; this screen only
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

-- world.getPlantSuitability's per-crop `factors` array order (see
-- World.Flora.Placement.FitnessFactor / Engine.Scripting.Lua.API.Plant).
local FACTOR_ORDER = { "temperature", "precipitation", "humidity",
                       "altitude", "slope", "soil" }
local FACTOR_LABELS = {
    temperature = "Temperature", precipitation = "Precipitation",
    humidity = "Humidity", altitude = "Altitude",
}
local FACTOR_FMT = {
    temperature = "%.1f", precipitation = "%.2f",
    humidity = "%.2f", altitude = "%.0f",
}

-----------------------------------------------------------
-- Layout constants (base units; uiscale applied at draw time)
-----------------------------------------------------------
local PANEL_W_FRAC   = 0.72
local PANEL_H_FRAC   = 0.7
local PAD_X          = 24
local PAD_TOP        = 20
local PAD_BOT        = 20
local TITLE_FONT     = 20
local TITLE_H        = 26
local CLOSE_BTN_SZ   = 30
local CONTROL_H      = 28
local CONTROL_GAP    = 10
local SEARCH_TB_W    = 150
local SORT_BTN_W     = 140
local CB_SIZE        = 20
local CB_LABEL_FONT  = 13
local STATUS_FONT    = 12
local ITEM_H         = 32
local LIST_FONT      = 14
local COL_GAP        = 20
local DETAIL_HEADER_FONT = 14
local DETAIL_NAME_FONT   = 16
local DETAIL_FONT       = 13
local DETAIL_LINE_H     = 22
local PLANT_BTN_W       = 140
local PLANT_BTN_H       = 30

local TITLE_COL   = { 1.0, 1.0, 1.0, 1.0 }
local STATUS_COL  = { 0.75, 0.75, 0.75, 1.0 }
local HEADER_COL  = { 0.8, 0.8, 0.8, 1.0 }

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
    detailNameLblId = nil,
    detailFactorLblIds = {},
    plantBtnId = nil,
    selectedCrop = nil,
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
        label.setText(s.statusLblId, "Click a crop to see why it fits, then Plant here.")
    end
end

-- One line of the right-column suitability read-out for a single
-- factor — the "why is this good/bad here" detail #335 calls for.
-- Slope (max-only threshold) and soil (boolean match) aren't
-- min/ideal/max-range-shaped, so they get their own formatting.
local function formatFactorLine(f)
    local pct = math.floor(f.fit * 100 + 0.5)
    if f.factor == "soil" then
        return string.format("Soil: %s -- %d%%",
            f.fit >= 1.0 and "suitable" or "not suitable", pct)
    elseif f.factor == "slope" then
        return string.format("Slope: %d (max %d) -- %d%%",
            math.floor(f.value + 0.5), math.floor(f.max + 0.5), pct)
    else
        local fmt = FACTOR_FMT[f.factor] or "%.2f"
        local label_ = FACTOR_LABELS[f.factor] or f.factor
        return string.format(
            "%s: " .. fmt .. " (ideal " .. fmt .. ", range " .. fmt .. "-" .. fmt .. ") -- %d%%",
            label_, f.value, f.ideal, f.min, f.max, pct)
    end
end

-- Update the right-column detail read-out for a previewed row (nil =
-- nothing selected yet: placeholder text, blank factor lines, Plant
-- disabled).
local function renderDetail(row)
    local s = plantPanel.state
    if not s.detailNameLblId then return end
    if not row then
        s.selectedCrop = nil
        label.setText(s.detailNameLblId,
            "Click a crop on the left to see details.")
        for _, id in ipairs(s.detailFactorLblIds) do label.setText(id, "") end
        return
    end
    s.selectedCrop = row.name
    local pct = math.floor(row.score * 100 + 0.5)
    label.setText(s.detailNameLblId, string.format("%s (%s) -- %d%%",
        humanize(row.name), categoryLabel(row.category), pct))
    for i, id in ipairs(s.detailFactorLblIds) do
        local f = row.factors and row.factors[i]
        label.setText(id, f and formatFactorLine(f) or "")
    end
end

-- Rebuild the list widget's items from current control state. NOTE:
-- list.new fixes its visible-slot count from the ITEMS array passed at
-- creation time (list.lua's setItems only refreshes those pre-built
-- slots, it never grows them) — safe here because filtering only ever
-- NARROWS s.allRows, never exceeds it, so the list is always created
-- with the full unfiltered catalogue (see renderUI). A crop previewed
-- in the right column that gets filtered out of the visible list stays
-- previewed (its data doesn't change), so this doesn't clear the
-- selection.
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
    s.searchTbId, s.sortBtnId, s.rowCbId, s.groundCbId, s.statusLblId
        = nil, nil, nil, nil, nil
    s.detailNameLblId, s.plantBtnId = nil, nil
    s.detailFactorLblIds = {}
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
    s.selectedCrop = nil
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

    -- Two columns: left = search/sort/filters/list, right = the
    -- suitability read-out for whichever crop is previewed + Plant.
    local colGap = math.floor(COL_GAP * uiscale)
    local leftW = math.floor((cw - colGap) * 0.55)
    local rightW = cw - leftW - colGap
    local leftX = cx
    local rightX = cx + leftW + colGap

    local controlsY = cy + math.floor(TITLE_H * uiscale) + math.floor(6 * uiscale)
    local statusY = panelY + panelH - PAD_BOT - math.floor(4 * uiscale)

    -- Left column: search + sort ------------------------------------
    s.searchTbId = textbox.new({
        name = "plant_panel_search", x = leftX, y = controlsY,
        width = SEARCH_TB_W, height = CONTROL_H, fontSize = 13,
        page = h.page, font = h.menuFont, uiscale = uiscale,
        default = "", zIndex = 201,
    })
    table.insert(s.chrome, { kind = "textbox", id = s.searchTbId })

    local sortX = leftX + math.floor((SEARCH_TB_W + CONTROL_GAP) * uiscale)
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

    -- Left column: category filters ----------------------------------
    local cbY = controlsY + math.floor((CONTROL_H + CONTROL_GAP) * uiscale)
    local cbSz = math.floor(CB_SIZE * uiscale)

    s.rowCbId = checkbox.new({
        name = "plant_panel_row_cb", x = leftX, y = cbY, size = CB_SIZE,
        page = h.page, uiscale = uiscale, default = true, zIndex = 201,
        onChange = function() refreshList() end,
    })
    table.insert(s.chrome, { kind = "checkbox", id = s.rowCbId })
    local rowLblId = label.new({
        name = "plant_panel_row_lbl", text = "Row crops",
        font = h.menuFont, fontSize = CB_LABEL_FONT, color = STATUS_COL,
        page = h.page, uiscale = uiscale,
        x = leftX + cbSz + math.floor(6 * uiscale),
        y = cbY + math.floor(cbSz * 0.75),
    })
    table.insert(s.chrome, { kind = "label", id = rowLblId })
    local rowLblW = select(1, label.getSize(rowLblId))

    local groundX = leftX + cbSz + math.floor(6 * uiscale) + rowLblW + math.floor(16 * uiscale)
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

    -- Left column: the crop list, and the status line under it -------
    local listY = cbY + math.floor((CB_SIZE + CONTROL_GAP + 4) * uiscale)
    local listHeightPx = statusY - listY - math.floor(10 * uiscale)
    local maxVisible = math.max(1, math.floor(listHeightPx / (ITEM_H * uiscale)))

    s.statusLblId = label.new({
        name = "plant_panel_status", text = "",
        font = h.menuFont, fontSize = STATUS_FONT, color = STATUS_COL,
        page = h.page, uiscale = uiscale, x = leftX, y = statusY,
    })
    table.insert(s.chrome, { kind = "label", id = s.statusLblId })

    -- Seed with the FULL unfiltered/default-sorted catalogue (both
    -- category checkboxes default true, search empty) — this fixes the
    -- list's max slot count for its lifetime, see refreshList's note.
    local initialRows = plantPanel.visibleRows(s.allRows, "", "score", true, true)

    s.listId = list.new({
        name = "plant_panel_list", page = h.page,
        x = leftX, y = listY, width = leftW,
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
            local found = nil
            for _, r in ipairs(st.allRows) do
                if r.name == value then found = r break end
            end
            renderDetail(found)
        end,
    })
    setStatus(#initialRows)
    s.lastSearch = ""

    -- Right column: suitability read-out for the previewed crop -------
    local detailHeaderId = label.new({
        name = "plant_panel_detail_header", text = "Suitability",
        font = h.menuFont, fontSize = DETAIL_HEADER_FONT, color = HEADER_COL,
        page = h.page, uiscale = uiscale,
        x = rightX, y = controlsY + math.floor(DETAIL_HEADER_FONT * uiscale),
    })
    table.insert(s.chrome, { kind = "label", id = detailHeaderId })

    local detailNameY = controlsY + math.floor(DETAIL_LINE_H * uiscale)
    s.detailNameLblId = label.new({
        name = "plant_panel_detail_name", text = "",
        font = h.menuFont, fontSize = DETAIL_NAME_FONT, color = TITLE_COL,
        page = h.page, uiscale = uiscale,
        x = rightX, y = detailNameY + math.floor(DETAIL_NAME_FONT * uiscale),
    })
    table.insert(s.chrome, { kind = "label", id = s.detailNameLblId })

    s.detailFactorLblIds = {}
    local factorStartY = detailNameY + math.floor(DETAIL_LINE_H * 1.6 * uiscale)
    for i = 1, #FACTOR_ORDER do
        local fy = factorStartY + math.floor((i - 1) * DETAIL_LINE_H * uiscale)
        local id = label.new({
            name = "plant_panel_factor_" .. i, text = "",
            font = h.menuFont, fontSize = DETAIL_FONT, color = STATUS_COL,
            page = h.page, uiscale = uiscale,
            x = rightX, y = fy + math.floor(DETAIL_FONT * uiscale),
        })
        table.insert(s.chrome, { kind = "label", id = id })
        table.insert(s.detailFactorLblIds, id)
    end

    s.plantBtnId = button.new({
        name = "plant_panel_plant_btn", text = "Plant here",
        x = rightX, y = statusY - math.floor(PLANT_BTN_H * uiscale) + math.floor(4 * uiscale),
        width = PLANT_BTN_W, height = PLANT_BTN_H,
        fontSize = 14, uiscale = uiscale, page = h.page, font = h.menuFont,
        textureSet = h.boxTexSet,
        bgColor = { 1.0, 1.0, 1.0, 1.0 }, textColor = { 1.0, 1.0, 1.0, 1.0 },
        zIndex = 201,
        onClick = function()
            local st = plantPanel.state
            if st.selectedCrop then
                plant.designate(st.pageId, st.gx, st.gy, st.selectedCrop)
                plantPanel.closeIfOpen()
            end
        end,
    })
    table.insert(s.chrome, { kind = "button", id = s.plantBtnId })

    renderDetail(nil)
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
    s.selectedCrop = nil
    s.allRows = world.getPlantSuitability(gx, gy) or {}
    renderUI()
end

-- #750 round-14 review: hud.lua's resize snapshot/reopen (round-13)
-- only preserved WHICH tile was open — plain show() always resets
-- sortMode/selectedCrop, so a resize still silently discarded the
-- player's sort choice and crop selection. Reopens via show()
-- (unchanged, still the right way to re-derive s.allRows for the
-- tile) and restores the saved sortMode unconditionally (always a
-- valid choice, "score" or "name") plus selectedCrop only if it's
-- still among the tile's current suitability rows — the suitability
-- list is re-derived fresh by show(), so a crop that dropped out
-- between resize and reopen must not be restored as if still
-- selectable. renderDetail(row) — not a raw state write — must run
-- AFTER renderUI(), since renderUI() unconditionally ends by calling
-- renderDetail(nil) itself (the normal "nothing previewed yet" initial
-- state); restoring selectedCrop through the same renderDetail() path
-- renderUI() uses keeps the visible detail read-out in sync with the
-- restored state, not just the field.
function plantPanel.reopenWithState(pageId, gx, gy, sortMode, selectedCrop)
    plantPanel.show(pageId, gx, gy)
    local s = plantPanel.state
    if not s.open then return end
    if sortMode then s.sortMode = sortMode end
    renderUI()
    if selectedCrop then
        for _, row in ipairs(s.allRows) do
            if row.name == selectedCrop then renderDetail(row); break end
        end
    end
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
