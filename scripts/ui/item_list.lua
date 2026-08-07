-- Shared item-list widget (#1088, epic #1013 phase C0).
--
-- ONE renderer for every panel that shows a list of items: the cargo
-- contents popup, the item-container contents popup, the unit-info
-- inventory section, and (later) C1's paired transfer view. Before
-- this module those three hosts each carried their own stack key,
-- grouper, tab computation, row builder, UTF-8 truncation and dirty
-- check; a fourth copy for C1 is what this extraction exists to avoid.
--
-- The widget owns: optional grouping and the canonical stack key,
-- category normalization + tab computation, row creation/layout/
-- teardown, representative-instance right-click routing, UTF-8-safe
-- truncation, selected-tab routing, rebuild invalidation, the optional
-- capacity/weight header, and the optional footer.
--
-- The HOST owns: its window or HUD section, data acquisition, its
-- title and surrounding chrome, the presentation policy it supplies
-- through the callbacks below, and the contents of any right-click
-- menu. The widget never creates, positions or closes a window, and
-- never learns what "Withdraw" or "Equip" mean.
--
-- Rendering within host-supplied bounds:
--
--   +-- p.y ------------------------------------------+
--   |  [optional header]                              |
--   |  [optional tab strip]     (scripts/ui/tabbar)   |
--   |  row 0 ... row n-1                              |
--   |  [optional footer]                              |
--   +-- p.y + p.height --------------------------------+
--
-- Metrics are BASE values multiplied by an effective scale, so each
-- host keeps its own visual proportions verbatim. When `fitVertical`
-- is set the widget derives a LOCAL effective scale
-- (responsive.fitScale) against the height needed for the tab strip
-- plus AT LEAST ONE row plus the footer, which is what keeps the
-- unit-info inventory reachable at a narrow, high-scale, still-
-- supported combination.
--
-- Invalidation: the widget owns the final rebuild comparison. A host
-- keeps NO hash of its own -- it asks `itemList.isStale(id, params)`
-- and rebuilds its own chrome plus the widget when the answer is yes.
-- Values the widget cannot see (a formatter's output, a repair-status
-- badge) reach the comparison through the host's `presentationKey`.

local label         = require("scripts.ui.label")
local scale         = require("scripts.ui.scale")
local tabbar        = require("scripts.ui.tabbar")
local brokenOverlay = require("scripts.ui.broken_overlay")
local utf8Safe      = require("scripts.ui.utf8_safe")
local responsive    = require("scripts.ui.responsive")

local itemList = {}

-----------------------------------------------------------
-- Constants / module state
-----------------------------------------------------------

local RIGHT_CLICK_CALLBACK = "onItemListRightClick"

local lists  = {}
local nextId = 1
local whitePixelTex = nil

function itemList.init()
    if not whitePixelTex then
        whitePixelTex = engine.loadTexture("assets/textures/utility/white.png")
    end
    engine.logDebug("ItemList module initialized")
end

-----------------------------------------------------------
-- Pure data shaping
-----------------------------------------------------------

-- A missing or empty category normalizes to "Misc" -- consistently in
-- BOTH tab discovery and tab counting, and in the active-tab filter.
-- The pre-#1088 cargo panel discovered tabs from the normalized value
-- but counted against the raw one, so an uncategorized item produced a
-- "Misc (0)" tab whose rows the filter nonetheless listed.
function itemList.normalizeCategory(category)
    if category == nil or category == "" then return "Misc" end
    return category
end

-- The canonical stack key. Rows merge only when every VISIBLE identity
-- field matches: defName + quality + condition + current fill + weight
-- (raw gems roll a per-instance weight and the row shows weight x
-- count) + weapon-only sharpness (non-weapon gear carries a mutated
-- sharpness it never displays, so splitting on it would be an
-- invisible, confusing split) + the nested contents signature (#67A).
-- Anything that still merges is interchangeable, so acting on the
-- group's representative instanceId is always correct (#67).
--
-- Returns nil for an equipped entry when the host asked for equipped
-- items to stay separate -- each occupies a distinct slot.
function itemList.stackKey(it, separateEquipped)
    if separateEquipped and it.equipped then return nil end
    return table.concat({
        tostring(it.defName),
        tostring(it.quality     or "_"),
        tostring(it.condition   or "_"),
        tostring(it.currentFill or "_"),
        tostring(it.weight      or "_"),
        it.weapon and tostring(it.sharpness or "_") or "_",
        tostring(it.contentsKey or ""),
    }, "|")
end

-- Copy-and-count grouping in FIRST-APPEARANCE order. The first item of
-- each group stays its representative instance; the shared count field
-- is `count`.
function itemList.groupItems(items, opts)
    local separateEquipped = opts and opts.separateEquipped
    local groups = {}
    local seen   = {}
    for _, it in ipairs(items or {}) do
        local key = itemList.stackKey(it, separateEquipped)
        if key and seen[key] then
            groups[seen[key]].count = groups[seen[key]].count + 1
        else
            local copy = {}
            for k, v in pairs(it) do copy[k] = v end
            copy.count = 1
            groups[#groups + 1] = copy
            if key then seen[key] = #groups end
        end
    end
    return groups
end

-- "All" first, then one tab per category in first-appearance order.
-- Counts are grouped DISPLAY-ROW counts, not summed item quantities:
-- a row standing for twelve identical items contributes one.
function itemList.computeTabs(rows)
    local tabs = { { name = "All", count = #rows } }
    local seen = { All = true }
    for _, r in ipairs(rows) do
        local c = itemList.normalizeCategory(r.category)
        if not seen[c] then
            seen[c] = true
            tabs[#tabs + 1] = { name = c, count = 0 }
        end
    end
    for _, r in ipairs(rows) do
        local c = itemList.normalizeCategory(r.category)
        for _, t in ipairs(tabs) do
            if t.name == c then t.count = t.count + 1 end
        end
    end
    return tabs
end

function itemList.filterByTab(rows, activeTab)
    if not activeTab or activeTab == "All" then return rows end
    local out = {}
    for _, r in ipairs(rows) do
        if itemList.normalizeCategory(r.category) == activeTab then
            out[#out + 1] = r
        end
    end
    return out
end

-- Defensive UTF-8-safe truncation with a ".." suffix (#618's rule, as
-- implemented by the unit-info inventory before this extraction):
--   * nil or empty text returns unchanged;
--   * maxPx <= 0 returns the ORIGINAL text unchanged;
--   * text that already fits returns unchanged;
--   * a positive maxPx too narrow for ".." itself returns "";
--   * otherwise the longest complete UTF-8 prefix that fits with "..".
-- Every candidate cut is snapped to a character boundary, because
-- string.sub cuts by byte offset, not codepoint.
function itemList.truncateToWidth(text, font, fontPx, maxPx)
    if not text or text == "" then return text end
    if not maxPx or maxPx <= 0 then return text end
    local full = engine.getTextWidth(font, text, fontPx) or 0
    if full <= maxPx then return text end
    local ellipsis = ".."
    local ellW = engine.getTextWidth(font, ellipsis, fontPx) or 0
    if ellW > maxPx then return "" end   -- not even the dots fit
    local lo, hi = 0, #text
    while lo < hi do
        local mid = math.floor((lo + hi + 1) / 2)
        local cut = utf8Safe.snapToCharBoundary(text, mid)
        local w = (engine.getTextWidth(font, text:sub(1, cut), fontPx) or 0) + ellW
        if w <= maxPx then lo = mid else hi = mid - 1 end
    end
    return text:sub(1, utf8Safe.snapToCharBoundary(text, lo)) .. ellipsis
end

-- Normalize the host's input into the model the renderer and the
-- invalidation comparison both read. Pure -- a host calls this to size
-- its own window before any element exists, then hands the SAME model
-- back to new() so grouping is never computed twice.
--
-- Pre-grouped rows (unit.getItemContents' deliberately coarse
-- by-defName grouping) pass through with their order and grouping
-- exactly as supplied: the widget never re-splits or re-sorts them.
function itemList.prepare(p)
    local rows
    if p.preGrouped then
        rows = p.items or {}
    else
        rows = itemList.groupItems(p.items,
            { separateEquipped = p.separateEquipped })
    end

    local tabs, activeTab = nil, "All"
    if p.tabs then
        tabs = itemList.computeTabs(rows)
        activeTab = p.activeTab or "All"
        local stillValid = false
        for _, t in ipairs(tabs) do
            if t.name == activeTab then stillValid = true; break end
        end
        if not stillValid then activeTab = "All" end
    end

    local visible = tabs and itemList.filterByTab(rows, activeTab) or rows
    return { rows = rows, tabs = tabs, activeTab = activeTab, visible = visible }
end

-----------------------------------------------------------
-- Invalidation
-----------------------------------------------------------

local function rowSignature(r)
    return table.concat({
        tostring(r.defName or ""),
        tostring(r.displayName or ""),
        tostring(r.count or 1),
        itemList.normalizeCategory(r.category),
        tostring(r.quality or "_"),
        tostring(r.qualityTier or "_"),
        tostring(r.condition or "_"),
        tostring(r.currentFill or "_"),
        tostring(r.fill or "_"),
        tostring(r.weight or "_"),
        r.weapon and "w" or "_",
        tostring(r.sharpness or "_"),
        tostring(r.contentsKey or ""),
        r.equipped and "e" or "i",
        tostring(r.equippedSlot or ""),
        tostring(r.accessoryIndex or "_"),
        tostring(r.instanceId or "_"),
        tostring(r.iconTex or "_"),
        tostring(r.unequippable or false),
    }, "/")
end

-- Two signatures, because a host that must size its own window from
-- the data cannot know its bounds yet when it asks whether anything
-- changed. `data` covers everything the widget derives from the
-- supplied values (including the host's presentationKey for anything
-- a callback produces); `layout` adds the bounds and row cap.
local function signatures(p, model)
    model = model or itemList.prepare(p)
    local parts = {
        "rows=" .. #model.rows,
        "tab=" .. tostring(model.activeTab),
        "pre=" .. tostring(p.preGrouped and 1 or 0),
        "eq=" .. tostring(p.separateEquipped and 1 or 0),
        "rc=" .. tostring(p.onRowRightClick and 1 or 0),
        "tt=" .. tostring(p.rowTooltip and 1 or 0),
        "s=" .. tostring(p.uiscale or scale.get()),
        "empty=" .. tostring(p.emptyText or ""),
        "footer=" .. tostring(p.footer and p.footer.text or ""),
        "hdr=" .. tostring(p.header and
            (tostring(p.header.stored) .. "/" .. tostring(p.header.capacity))
            or ""),
        "pk=" .. tostring(p.presentationKey or ""),
    }
    if model.tabs then
        for _, t in ipairs(model.tabs) do
            parts[#parts + 1] = "t:" .. t.name .. ":" .. t.count
        end
    end
    for _, r in ipairs(model.rows) do
        parts[#parts + 1] = rowSignature(r)
    end
    local data = table.concat(parts, "|")
    local layout = table.concat({
        tostring(p.x), tostring(p.y),
        tostring(p.width), tostring(p.height),
        tostring(p.maxRows or "fit"),
    }, ",")
    return data, layout, model
end

itemList.signatures = signatures

-- Has anything changed that must recreate elements? A caller that
-- already knows its bounds (an embedded section) passes them and gets
-- the bounds compared too; one that derives its bounds FROM the data
-- (a popup sized to its row count) omits them and is answered on the
-- data alone.
function itemList.isStale(id, p)
    local inst = lists[id]
    if not inst then return true end
    local data, layout = signatures(p)
    if data ~= inst.dataSig then return true end
    if p.width ~= nil and layout ~= inst.layoutSig then return true end
    return false
end

-- Force the next isStale() to answer true (a player action whose
-- effect the host wants reflected in the SAME frame).
function itemList.invalidate(id)
    local inst = lists[id]
    if inst then inst.dataSig = nil end
end

-----------------------------------------------------------
-- Rendering
-----------------------------------------------------------

local function track(inst, kind, id)
    inst.elements[#inst.elements + 1] = { kind = kind, id = id }
end

local function destroyElements(inst)
    for _, e in ipairs(inst.elements) do
        if e.kind == "label" then label.destroy(e.id)
        else UI.deleteElement(e.id) end
    end
    inst.elements = {}
    inst.rows = {}
    if inst.tabbarId then
        tabbar.destroy(inst.tabbarId)
        inst.tabbarId = nil
    end
end

local function buildTabs(inst, s, originY)
    local p = inst.params
    local t = p.tabs
    if not t then return 0 end
    local defs = {}
    for i, td in ipairs(inst.model.tabs) do
        defs[i] = { name = td.name .. " (" .. td.count .. ")", key = td.name }
    end
    local labelScale = p.tabLabelUiscale or s
    inst.tabbarId = tabbar.new({
        name           = inst.name .. "_tabs",
        page           = p.page,
        font           = p.font,
        layout         = t.mode or "row",
        tabs           = defs,
        x              = p.x,
        y              = originY,
        width          = p.width,
        wrapWidth      = t.wrapWidth or p.width,
        shrinkToFit    = t.shrinkToFit,
        centerRows     = t.centerRows,
        tabHeightPx    = math.floor((t.tabHeight or 28) * s),
        tileSizePx     = t.tileSize or 16,
        textPaddingPx  = math.floor((t.textPad or 8) * labelScale),
        measureFontPx  = math.floor((t.fontSize or 13) * labelScale),
        labelFontSize  = t.fontSize or 13,
        labelUiscale   = labelScale,
        gapPx          = math.floor((t.gap or 0) * s),
        rowGapPx       = math.floor((t.rowGap or 0) * s),
        minTabWidth    = t.minTabWidth or 20,
        textColor      = t.textColor,
        selectedTextColor = t.selectedTextColor,
        zIndex         = p.zBase or 10,
        labelZIndex    = (p.zBase or 10) + 1,
        selectedKey    = inst.model.activeTab,
        onChange       = function(key)
            if p.onTabChange then p.onTabChange(key) end
        end,
    })
    return tabbar.getStripHeight(inst.tabbarId)
end

-- Number of tab ROWS the strip will occupy, without creating anything.
-- Needed by the local vertical fit, which must know the strip's height
-- before it can decide the scale everything else is built at.
local function planTabRowCount(inst)
    local p = inst.params
    local t = p.tabs
    if not t then return 0 end
    if (t.mode or "row") ~= "wrapped" then return 1 end
    local labelScale = p.tabLabelUiscale or (p.uiscale or scale.get())
    local names = {}
    for i, td in ipairs(inst.model.tabs) do
        names[i] = td.name .. " (" .. td.count .. ")"
    end
    local widths = tabbar.measureTabs(p.font, names,
        math.floor((t.fontSize or 13) * labelScale),
        math.floor((t.textPad or 8) * labelScale))
    return #tabbar.planWrap(widths, t.wrapWidth or p.width, 0)
end

local function buildRows(inst, s, listY, listBottom)
    local p = inst.params
    local rowH    = math.floor((p.rowHeight or 32) * s)
    local rowPad  = math.floor((p.rowPad or 2) * s)
    local iconSz  = math.floor((p.iconSize or 28) * s)
    local textPad = math.floor((p.textPad or 12) * s)
    local inset   = math.floor((p.contentInset or 0) * s)
    local gap     = math.floor((p.nameRightGap or 24) * s)
    local fontSz  = p.rowFontSize or 13
    local fontPx  = math.floor(fontSz * (p.rowLabelUiscale or s))
    local baseline = math.floor(rowH / 2) + math.floor(fontSz * 0.3)
    local listX   = p.x + inset
    local listW   = p.width - 2 * inset
    local zBase   = p.zBase or 10

    local visible = inst.model.visible
    local maxRows = p.maxRows
    if not maxRows then
        maxRows = math.max(0,
            math.floor((listBottom - listY) / (rowH + rowPad)))
    end

    if #visible == 0 then
        if p.emptyText then
            local lbl = label.new({
                name = inst.name .. "_empty", text = p.emptyText,
                font = p.font, fontSize = fontSz,
                color = p.emptyColor or { 0.7, 0.7, 0.7, 1.0 },
                page = p.page, uiscale = p.rowLabelUiscale or s,
            })
            local h = label.getElementHandle(lbl)
            UI.addToPage(p.page, h, listX + textPad, listY + baseline)
            UI.setZIndex(h, zBase + 1)
            track(inst, "label", lbl)
        end
        return
    end

    for i = 1, math.min(#visible, maxRows) do
        local row  = visible[i]
        local rowY = listY + (i - 1) * (rowH + rowPad)

        -- Optional full-row backdrop (the unit-info equipped tint).
        local bg = p.rowBackground and p.rowBackground(row) or nil
        if bg then
            local bgId = UI.newSprite(inst.name .. "_rowbg_" .. i,
                listW, rowH, whitePixelTex,
                bg[1], bg[2], bg[3], bg[4], p.page)
            UI.addToPage(p.page, bgId, listX, rowY)
            UI.setZIndex(bgId, zBase)
            track(inst, "sprite", bgId)
        end

        -- Icon plus the shared broken-equipment overlay. A supplied
        -- rowIcon is AUTHORITATIVE, including when it returns nil: the
        -- item-contents host answers nil for a negative (missing) icon
        -- handle, and an `or row.iconTex` fallback would put that very
        -- handle back and render an undefined-texture sprite.
        local iconTex
        if p.rowIcon then iconTex = p.rowIcon(row) else iconTex = row.iconTex end
        if iconTex then
            local iconY = rowY + math.floor((rowH - iconSz) / 2)
            local iconId = UI.newSprite(inst.name .. "_icon_" .. i,
                iconSz, iconSz, iconTex, 1.0, 1.0, 1.0, 1.0, p.page)
            UI.addToPage(p.page, iconId, listX + textPad, iconY)
            UI.setZIndex(iconId, zBase + 1)
            track(inst, "sprite", iconId)
            local oid = brokenOverlay.add(p.page,
                inst.name .. "_broken_" .. i, row.condition,
                listX + textPad, iconY, iconSz, iconSz, zBase + 2)
            if oid then track(inst, "sprite", oid) end
        end

        -- Right-aligned weight column, built FIRST so its measured
        -- width bounds the name column's available space.
        local wW = 0
        local wText = p.rowWeightText and p.rowWeightText(row) or nil
        if wText then
            local wLbl = label.new({
                name = inst.name .. "_w_" .. i, text = wText,
                font = p.font, fontSize = fontSz,
                color = p.weightColor or { 0.85, 0.85, 0.85, 1.0 },
                page = p.page, uiscale = p.rowLabelUiscale or s,
            })
            local wH = label.getElementHandle(wLbl)
            wW = select(1, label.getSize(wLbl))
            UI.addToPage(p.page, wH,
                listX + listW - textPad - wW, rowY + baseline)
            UI.setZIndex(wH, zBase + 1)
            track(inst, "label", wLbl)
        end

        -- Display name, truncated so it never runs into the weight
        -- column. Measured at the size the label actually renders at.
        local nameX     = listX + textPad + iconSz + textPad
        local nameRight = listX + listW - textPad - wW - gap
        local nameMaxPx = math.max(0, nameRight - nameX)
        local rawName   = p.rowName and p.rowName(row)
                          or (row.displayName or row.defName or "?")
        local nameLbl = label.new({
            name = inst.name .. "_name_" .. i,
            text = itemList.truncateToWidth(rawName, p.font, fontPx, nameMaxPx),
            font = p.font, fontSize = fontSz,
            color = p.rowColor and p.rowColor(row) or { 1.0, 1.0, 1.0, 1.0 },
            page = p.page, uiscale = p.rowLabelUiscale or s,
        })
        local nameH = label.getElementHandle(nameLbl)
        UI.addToPage(p.page, nameH, nameX, rowY + baseline)
        UI.setZIndex(nameH, zBase + 1)
        track(inst, "label", nameLbl)

        -- Transparent full-row surface. It hosts the row's tooltip
        -- (hover hit-testing is purely geometric, so this works with or
        -- without a click callback) and, ONLY when the host supplied
        -- one, the right-click action. With no callback no click
        -- callback and no pointer-blocking opt-in is registered, so the
        -- row consumes nothing.
        local tip = p.rowTooltip and p.rowTooltip(row) or nil
        local hitId = UI.newSprite(inst.name .. "_hit_" .. i,
            listW, rowH, whitePixelTex, 1.0, 1.0, 1.0, 0.0, p.page)
        UI.addToPage(p.page, hitId, listX, rowY)
        UI.setZIndex(hitId, zBase + 3)
        if tip or p.onRowRightClick then
            UI.setClickable(hitId, true)
        end
        if p.onRowRightClick then
            UI.setOnRightClick(hitId, RIGHT_CLICK_CALLBACK)
        end
        if tip then UI.setTooltipRich(hitId, tip) end
        track(inst, "sprite", hitId)
        inst.rows[#inst.rows + 1] =
            { hitId = hitId, item = row, index = i }
    end
end

local function build(inst)
    local p = inst.params
    local uiscale = p.uiscale or scale.get()

    -- Pads come either as a BASE value scaled with everything else (an
    -- embedded section that also opts into the local vertical fit) or
    -- as a final pixel value (a popup whose own height arithmetic
    -- already used a literal gap).
    local function pad(px, base, s)
        if px ~= nil then return px end
        return math.floor((base or 0) * s)
    end

    local headerH = p.header and math.floor((p.headerHeight or 20) * uiscale) or 0
    local topPad  = pad(p.tabTopPadPx, p.tabTopPad, uiscale)
    local botPad  = pad(p.tabBottomPadPx, p.tabBottomPad, uiscale)
    local footerH = p.footer and math.floor((p.footerHeight or 22) * uiscale) or 0

    -- Local vertical fit: shrink this widget's own chrome (never
    -- another section's scale) until the tab strip, at least one item
    -- row and the footer all fit the host-supplied height.
    local s = uiscale
    if p.fitVertical and p.height then
        local nTabRows = planTabRowCount(inst)
        local tabH     = math.floor((p.tabs and p.tabs.tabHeight or 28) * uiscale)
        local rowGap   = math.floor((p.tabs and p.tabs.rowGap or 0) * uiscale)
        local naturalMinH = headerH + topPad + nTabRows * tabH
            + math.max(0, nTabRows - 1) * rowGap + botPad
            + (math.floor((p.rowHeight or 32) * uiscale)
               + math.floor((p.rowPad or 2) * uiscale))
            + footerH
        s = responsive.fitScale(naturalMinH, p.height, uiscale)
        headerH = p.header and math.floor((p.headerHeight or 20) * s) or 0
        topPad  = pad(p.tabTopPadPx, p.tabTopPad, s)
        botPad  = pad(p.tabBottomPadPx, p.tabBottomPad, s)
        footerH = p.footer and math.floor((p.footerHeight or 22) * s) or 0
    end
    inst.scale = s

    local cursorY = p.y

    -- Optional capacity/weight header from host-supplied values. The
    -- widget never queries a capacity or inventory manager itself.
    if p.header then
        local text = string.format("%.2f / %.2f kg",
            p.header.stored or 0, p.header.capacity or 0)
        local lbl = label.new({
            name = inst.name .. "_header", text = text,
            font = p.font, fontSize = p.headerFontSize or 13,
            color = p.headerColor or { 0.85, 0.85, 0.85, 1.0 },
            page = p.page, uiscale = p.rowLabelUiscale or s,
        })
        local h = label.getElementHandle(lbl)
        UI.addToPage(p.page, h, p.x + math.floor((p.contentInset or 0) * s),
            cursorY + math.floor((p.headerFontSize or 13) * 0.85))
        UI.setZIndex(h, (p.zBase or 10) + 1)
        track(inst, "label", lbl)
        cursorY = cursorY + headerH
    end

    if p.tabs then
        cursorY = cursorY + topPad
        local stripH = buildTabs(inst, s, cursorY)
        cursorY = cursorY + stripH + botPad
    end

    local listBottom = (p.height and (p.y + p.height - footerH))
                       or (cursorY + footerH)
    buildRows(inst, s, cursorY, listBottom)

    -- Optional footer, right-aligned along the bottom edge.
    if p.footer then
        local inset = math.floor((p.contentInset or 0) * s)
        local textPad = math.floor((p.textPad or 12) * s)
        local fontSz = p.footerFontSize or p.rowFontSize or 13
        local lbl = label.new({
            name = inst.name .. "_footer", text = p.footer.text or "",
            font = p.font, fontSize = fontSz,
            color = p.footer.color or { 0.85, 0.85, 0.85, 1.0 },
            page = p.page, uiscale = p.rowLabelUiscale or s,
        })
        local h = label.getElementHandle(lbl)
        local w = select(1, label.getSize(lbl))
        UI.addToPage(p.page, h,
            p.x + inset + (p.width - 2 * inset) - textPad - w,
            p.y + (p.height or 0) - footerH
                + math.floor(footerH / 2) + math.floor(fontSz * 0.3))
        UI.setZIndex(h, (p.zBase or 10) + 1)
        track(inst, "label", lbl)
    end
end

-----------------------------------------------------------
-- Creation / destruction
-----------------------------------------------------------

function itemList.new(p)
    if not whitePixelTex then itemList.init() end
    local id = nextId
    nextId = nextId + 1
    local data, layout, model = signatures(p, p.model)
    local inst = {
        id = id,
        name = p.name or ("item_list_" .. id),
        params = p,
        model = model,
        elements = {},
        rows = {},
        dataSig = data,
        layoutSig = layout,
    }
    lists[id] = inst
    build(inst)
    return id
end

function itemList.destroy(id)
    local inst = lists[id]
    if not inst then return end
    destroyElements(inst)
    lists[id] = nil
end

function itemList.destroyAll()
    for id, _ in pairs(lists) do itemList.destroy(id) end
    lists = {}
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function itemList.getModel(id)
    local inst = lists[id]
    return inst and inst.model or nil
end

function itemList.getActiveTab(id)
    local inst = lists[id]
    return inst and inst.model.activeTab or nil
end

-- Tab records straight from the tab strip that actually exists
-- ({name, key, width, height, x, y, boxId, labelId}).
function itemList.getTabs(id)
    local inst = lists[id]
    if not inst or not inst.tabbarId then return {} end
    return tabbar.getTabs(inst.tabbarId)
end

function itemList.getTabBarId(id)
    local inst = lists[id]
    return inst and inst.tabbarId or nil
end

-- Rendered rows: { hitId, item, index }. `item` is the representative
-- instance of its group.
function itemList.getRows(id)
    local inst = lists[id]
    return inst and inst.rows or {}
end

function itemList.getScale(id)
    local inst = lists[id]
    return inst and inst.scale or nil
end

function itemList.count()
    local n = 0
    for _ in pairs(lists) do n = n + 1 end
    return n
end

-----------------------------------------------------------
-- Click routing
-----------------------------------------------------------

function itemList.isItemListCallback(callbackName)
    return callbackName == RIGHT_CLICK_CALLBACK
end

-- Route a row right-click back to the owning host with the EXACT
-- rendered row and its representative identity.
function itemList.handleCallback(callbackName, elemHandle)
    if callbackName ~= RIGHT_CLICK_CALLBACK then return false end
    for _, inst in pairs(lists) do
        local cb = inst.params.onRowRightClick
        if cb then
            for _, r in ipairs(inst.rows) do
                if r.hitId == elemHandle then
                    return cb(r.item, r) and true or false
                end
            end
        end
    end
    return false
end

-----------------------------------------------------------
-- Introspection (F3, #645)
-----------------------------------------------------------

-- One entry per rendered row. Reported even when the row carries no
-- right-click action (item contents) -- registry.lua's generic
-- fallback pass only sees elements that HAVE a click callback, so
-- without this a probe could not verify that panel's rows at all.
function itemList.dump()
    local out = {}
    for id, inst in pairs(lists) do
        for _, r in ipairs(inst.rows) do
            local info = r.hitId and UI.getElementInfo(r.hitId) or nil
            if info and info.pageVisible and info.visible then
                table.insert(out, {
                    id = "item_list:" .. id .. ":" .. r.index,
                    name = inst.name .. "_row_" .. r.index,
                    type = "item_list",
                    bounds = {
                        x = info.x, y = info.y,
                        w = info.width, h = info.height,
                    },
                    label = r.item.displayName or r.item.defName or "?",
                    count = r.item.count or 1,
                    category = itemList.normalizeCategory(r.item.category),
                    defName = r.item.defName,
                    instanceId = r.item.instanceId,
                    rightClick = inst.params.onRowRightClick ~= nil,
                    enabled = info.clickable,
                    visible = info.visible,
                    hovered = info.hovered,
                    focused = info.focused,
                    value = false,
                    screen = info.page,
                    handle = info.handle,
                })
            end
        end
    end
    return out
end

return itemList
