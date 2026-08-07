-- Tab Bar UI component
--
-- Three parameterized layouts (#1088), selected by `params.layout`:
--
--   "framed"  (default, unchanged) — a framed, left-to-right single row
--             with a mandatory content frame below it. Every pre-#1088
--             consumer gets exactly this by not passing `layout` at all.
--   "row"     — frame-free single row with optional shrink-to-fit, for
--             the cargo contents popup's tab strip.
--   "wrapped" — frame-free rows that wrap and centre within the given
--             width, for the unit-info inventory tab strip.
--
-- The two frame-free modes create NO content frame (not a hidden or
-- zero-sized replacement one) and lay their labels out through
-- scripts/ui/label.lua rather than as box children, because both
-- migrated hosts centre a page-rooted label inside its tab box and one
-- of them scales that label independently of the box metrics. That
-- independence is the reason for the *Px metric params below: cargo
-- shrinks its label with its box (labelUiscale = uiscale * shrink),
-- while unit-info renders a FIXED-size label inside boxes whose height
-- and padding come from a locally fitted scale. A single `uiscale`
-- multiplier cannot express either, so the frame-free modes accept
-- already-final pixel metrics and a separate label scale.
local boxTextures = require("scripts.ui.box_textures")
local scale = require("scripts.ui.scale")
local label = require("scripts.ui.label")
local tabbar = {}

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------
local TAB_CALLBACK = "onTabClick"

-----------------------------------------------------------
-- Module State
-----------------------------------------------------------

local tabbars = {}
local nextId = 1

local texSetSelected = nil
local texSetUnselected = nil
local texSetFrame = nil
local assetsLoaded = false

-----------------------------------------------------------
-- Initialization
-----------------------------------------------------------

function tabbar.init()
    if assetsLoaded then return end
    
    texSetSelected = boxTextures.load("assets/textures/ui/tabselected", "tabselected")
    texSetUnselected = boxTextures.load("assets/textures/ui/tabunselected", "tabunselected")
    texSetFrame = boxTextures.load("assets/textures/ui/tabframe", "tabframe")
    
    assetsLoaded = true
    engine.logDebug("TabBar module initialized")
end

-----------------------------------------------------------
-- Creation / Destruction
-----------------------------------------------------------

-----------------------------------------------------------
-- Frame-free layouts ("row" / "wrapped")
-----------------------------------------------------------

-- Forward declaration; defined below planWrap because it uses it.
local buildFrameFree

-- Greedy row-wrap plan shared by the "wrapped" layout and any caller
-- that must know the strip's row count before creating it (the
-- item-list widget's local vertical fit). Pure: returns a list of
-- { startIdx, endIdx, totalW }.
function tabbar.planWrap(widths, wrapWidth, gap)
    gap = gap or 0
    local rows = {}
    local cur = { startIdx = 1, totalW = 0, count = 0 }
    for i = 1, #widths do
        local w = widths[i]
        local extra = (cur.count > 0) and gap or 0
        if cur.count > 0 and wrapWidth and cur.totalW + extra + w > wrapWidth then
            cur.endIdx = i - 1
            rows[#rows + 1] = cur
            cur = { startIdx = i, totalW = w, count = 1 }
        else
            cur.totalW = cur.totalW + extra + w
            cur.count = cur.count + 1
        end
    end
    cur.endIdx = #widths
    rows[#rows + 1] = cur
    return rows
end

-- Natural (unshrunk) width of one tab in a frame-free layout, from the
-- SAME measurement rule the strip itself uses. Exposed so a host can
-- plan wraps / vertical fit before any element exists.
function tabbar.measureTabs(font, names, measureFontPx, textPaddingPx)
    local widths = {}
    for i, name in ipairs(names) do
        local w = engine.getTextWidth(font, name, measureFontPx) or 0
        widths[i] = math.floor(w) + 2 * (textPaddingPx or 0)
    end
    return widths
end

-- Build one frame-free strip. `params` metrics are FINAL pixel values
-- (see the module header): tabHeightPx, tileSizePx, textPaddingPx,
-- measureFontPx, gapPx, rowGapPx. `labelFontSize` + `labelUiscale` are
-- the label's own base size and scale, deliberately independent of the
-- box metrics above. Row mode additionally honours `shrinkToFit`
-- against `wrapWidth`, scaling the boxes, the gaps AND the label's
-- effective scale by one factor so a compressed tab's text stays
-- inside its own box.
buildFrameFree = function(tb, params, tabDefs)
    local tabH        = params.tabHeightPx
                        or math.floor((params.tabHeight or 40) * tb.uiscale)
    local tilePx      = params.tileSizePx
                        or math.floor((params.tileSize or 16) * tb.uiscale)
    local textPadPx   = params.textPaddingPx
                        or math.floor((params.textPadding or 10) * tb.uiscale)
    local measureFont = params.measureFontPx
                        or math.floor((params.fontSize or 24) * tb.uiscale)
    local labelFont   = params.labelFontSize or params.fontSize or 24
    local labelScale  = params.labelUiscale or tb.uiscale
    local gap         = params.gapPx or 0
    local rowGap      = params.rowGapPx or 0
    local wrapWidth   = params.wrapWidth or tb.width
    local minTabW     = params.minTabWidth or 20
    local labelZ      = params.labelZIndex or (tb.zIndex + 1)

    tb.height  = tabH
    tb.tabHeightPx = tabH

    local names = {}
    for i, td in ipairs(tabDefs) do names[i] = td.name end
    local widths = tabbar.measureTabs(tb.font, names, measureFont, textPadPx)

    -- Layout plan: one row (optionally shrunk) or wrapped rows.
    local shrink = 1.0
    local rows
    if tb.layout == "row" then
        local natural = 0
        for i = 1, #widths do
            natural = natural + widths[i] + (i > 1 and gap or 0)
        end
        if params.shrinkToFit and wrapWidth and natural > wrapWidth
           and natural > 0 then
            shrink = wrapWidth / natural
        end
        rows = { { startIdx = 1, endIdx = #widths } }
    else
        rows = tabbar.planWrap(widths, wrapWidth, gap)
    end

    local effGap = math.floor(gap * shrink)
    local boxWidths = {}
    for i = 1, #widths do
        boxWidths[i] = math.max(minTabW, math.floor(widths[i] * shrink))
    end
    tb.shrink = shrink

    local selected = params.selectedIndex or 1
    if params.selectedKey then
        for i, td in ipairs(tabDefs) do
            if (td.key or td.name) == params.selectedKey then
                selected = i; break
            end
        end
    end
    if selected < 1 or selected > #tabDefs then selected = 1 end
    tb.selectedIndex = selected

    local cursorY = tb.y
    for _, r in ipairs(rows) do
        local rowW = 0
        for i = r.startIdx, r.endIdx do
            rowW = rowW + boxWidths[i] + (i > r.startIdx and effGap or 0)
        end
        local cx = tb.x
        if params.centerRows then
            cx = tb.x + math.floor((tb.width - rowW) / 2)
        end
        for i = r.startIdx, r.endIdx do
            local td     = tabDefs[i]
            local tabW   = boxWidths[i]
            local active = (i == selected)
            local texSet = active and texSetSelected or texSetUnselected
            local txtCol = active and tb.selectedTextColor or tb.textColor

            local boxId = UI.newBox(tb.name .. "_tab_" .. i,
                tabW, tabH, texSet, tilePx,
                1.0, 1.0, 1.0, 1.0, 0, tb.page)
            UI.addToPage(tb.page, boxId, cx, cursorY)
            UI.setZIndex(boxId, tb.zIndex)
            UI.setClickable(boxId, true)
            UI.setOnClick(boxId, TAB_CALLBACK)
            UI.setInteractiveOverflow(boxId, true)

            local lblId = label.new({
                name     = tb.name .. "_tab_lbl_" .. i,
                text     = td.name,
                font     = tb.font,
                fontSize = labelFont,
                color    = txtCol,
                page     = tb.page,
                uiscale  = labelScale * shrink,
            })
            local lblH = label.getElementHandle(lblId)
            local lblW = select(1, label.getSize(lblId))
            UI.addToPage(tb.page, lblH,
                cx + math.floor((tabW - lblW) / 2),
                cursorY + math.floor(tabH / 2)
                        + math.floor(labelFont * 0.3) + 2)
            UI.setZIndex(lblH, labelZ)

            tb.tabs[#tb.tabs + 1] = {
                name = td.name,
                key = td.key or td.name,
                width = tabW,
                height = tabH,
                x = cx, y = cursorY,
                boxId = boxId,
                labelId = lblId,
            }
            cx = cx + tabW + effGap
        end
        cursorY = cursorY + tabH + rowGap
    end
    -- Strip height excludes the trailing inter-row gap, so a host can
    -- place content flush below the LAST row.
    tb.rowCount = #rows
    tb.stripHeight = (#rows > 0) and (cursorY - rowGap - tb.y) or 0
end

function tabbar.new(params)
    local id = nextId
    nextId = nextId + 1

    if not assetsLoaded then tabbar.init() end

    local uiscale = params.uiscale or scale.get()
    local tabHeight = math.floor((params.tabHeight or 40) * uiscale)
    local fontSize = math.floor((params.fontSize or 24) * uiscale)
    local tileSize = math.floor((params.tileSize or 16) * uiscale)
    local tabPadding = math.floor((params.tabPadding or 20) * uiscale)
    local textPadding = math.floor((params.textPadding or 10) * uiscale)

    local textColor = params.textColor or {0.0, 0.0, 0.0, 1.0}
    local selectedTextColor = params.selectedTextColor or {1.0, 1.0, 1.0, 1.0}

    local tabs = params.tabs or {}
    local layout = params.layout or "framed"

    local tb = {
        id = id,
        name = params.name or ("tabbar_" .. id),
        layout = layout,
        x = params.x or 0,
        y = params.y or 0,
        width = params.width or 400,
        height = tabHeight,
        fontSize = fontSize,
        tileSize = tileSize,
        tabPadding = tabPadding,
        textPadding = textPadding,
        page = params.page,
        font = params.font,
        uiscale = uiscale,
        textColor = textColor,
        selectedTextColor = selectedTextColor,
        tabs = {},
        selectedIndex = 1,
        frameBoxId = nil,
        frameX = params.x or 0,
        frameY = (params.y or 0) + tabHeight,
        frameWidth = params.width or 400,
        frameHeight = params.frameHeight or 300,
        frameTileSize = tileSize,
        onChange = params.onChange or nil,
        zIndex = params.zIndex or 10,
    }

    if layout ~= "framed" then
        buildFrameFree(tb, params, tabs)
        tabbars[id] = tb
        engine.logDebug("TabBar created: " .. tb.name .. " with "
            .. #tb.tabs .. " tabs (" .. layout .. ")")
        return id
    end

    -- Measure tab widths
    local totalTabWidth = 0
    for i, tabDef in ipairs(tabs) do
        local textWidth = engine.getTextWidth(params.font, tabDef.name, fontSize)
        local tabWidth = textWidth + (textPadding * 2)
        table.insert(tb.tabs, {
            name = tabDef.name,
            key = tabDef.key or tabDef.name:lower(),
            width = tabWidth,
            boxId = nil,
            textId = nil,
        })
        totalTabWidth = totalTabWidth + tabWidth
    end

    -- Create the content frame
    tb.frameBoxId = UI.newBox(
        tb.name .. "_frame",
        tb.frameWidth,
        tb.frameHeight,
        texSetFrame,
        tb.frameTileSize,
        1.0, 1.0, 1.0, 1.0,
        0,
        tb.page
    )
    UI.addToPage(tb.page, tb.frameBoxId, tb.frameX, tb.frameY)
    UI.setZIndex(tb.frameBoxId, tb.zIndex)

    -- Create tab boxes, laid out left to right
    local currentX = tb.x
    for i, tab in ipairs(tb.tabs) do
        local isSelected = (i == tb.selectedIndex)
        local texSet = isSelected and texSetSelected or texSetUnselected
        
        tab.boxId = UI.newBox(
            tb.name .. "_tab_" .. i,
            tab.width,
            tabHeight,
            texSet,
            tileSize,
            1.0, 1.0, 1.0, 1.0,
            0,
            tb.page
        )
        UI.addToPage(tb.page, tab.boxId, currentX, tb.y)
        UI.setClickable(tab.boxId, true)
        UI.setOnClick(tab.boxId, TAB_CALLBACK)
        -- #749: a tab is a genuine box-backed control — opt its visible
        -- border into interaction (no-op at the current 0 overflow;
        -- keeps the drawn edge clickable if a bordered tab texture is
        -- ever used).
        UI.setInteractiveOverflow(tab.boxId, true)
        UI.setZIndex(tab.boxId, tb.zIndex + 1)
        
        local txtColor = isSelected and selectedTextColor or textColor
        local textY = (tabHeight / 2) + (fontSize / 3)
        tab.textId = UI.newText(
            tb.name .. "_tab_text_" .. i,
            tab.name,
            tb.font,
            fontSize,
            txtColor[1], txtColor[2], txtColor[3], txtColor[4],
            tb.page
        )
        UI.addChild(tab.boxId, tab.textId, textPadding, textY)
        UI.setZIndex(tab.textId, 1)
        
        currentX = currentX + tab.width
    end
    
    tabbars[id] = tb
    
    engine.logDebug("TabBar created: " .. tb.name .. " with " .. #tb.tabs .. " tabs")
    
    return id
end

function tabbar.destroy(id)
    local tb = tabbars[id]
    if not tb then return end
    
    if tb.frameBoxId then
        UI.deleteElement(tb.frameBoxId)
    end
    for _, tab in ipairs(tb.tabs) do
        -- Frame-free layouts own a label-module label per tab; the
        -- framed layout's text is a CHILD of the box and goes with it.
        if tab.labelId then
            label.destroy(tab.labelId)
        end
        if tab.boxId then
            UI.deleteElement(tab.boxId)
        end
    end

    tabbars[id] = nil
    engine.logDebug("TabBar destroyed: " .. (tb.name or "?"))
end

function tabbar.destroyAll()
    for id, _ in pairs(tabbars) do
        tabbar.destroy(id)
    end
    tabbars = {}
    nextId = 1
end

-----------------------------------------------------------
-- Tab Selection
-----------------------------------------------------------

-- #750 round-4 review: `silent` (default false) skips the onChange
-- callback — used to sync the visual selection to an already-current
-- logical tab (e.g. after a layout-only rebuild recreates the tabbar at
-- its hardcoded default selectedIndex=1) without re-issuing whatever
-- side effect the real onChange callback performs (event_log.lua's own
-- handler resets scrollOffset to 0, which a rebuild-while-already-on-
-- this-tab must not do).
function tabbar.select(id, index, silent)
    local tb = tabbars[id]
    if not tb then return end
    if index < 1 or index > #tb.tabs then return end
    if tb.selectedIndex == index then return end

    -- The text handle is the box's child text element (framed layout)
    -- or the label module's own element (frame-free layouts).
    local function textHandle(tab)
        if tab.labelId then return label.getElementHandle(tab.labelId) end
        return tab.textId
    end

    -- Deselect old tab
    local oldTab = tb.tabs[tb.selectedIndex]
    if oldTab and oldTab.boxId then
        UI.setBoxTextures(oldTab.boxId, texSetUnselected)
        UI.setColor(textHandle(oldTab),
            tb.textColor[1], tb.textColor[2],
            tb.textColor[3], tb.textColor[4])
    end

    -- Select new tab
    tb.selectedIndex = index
    local newTab = tb.tabs[index]
    if newTab and newTab.boxId then
        UI.setBoxTextures(newTab.boxId, texSetSelected)
        UI.setColor(textHandle(newTab),
            tb.selectedTextColor[1], tb.selectedTextColor[2],
            tb.selectedTextColor[3], tb.selectedTextColor[4])
    end

    if not silent and tb.onChange then
        tb.onChange(newTab.key, index, id)
    end

    engine.logDebug("TabBar selected: " .. tb.name .. " -> " .. newTab.name)
end

function tabbar.selectByKey(id, key, silent)
    local tb = tabbars[id]
    if not tb then return end

    for i, tab in ipairs(tb.tabs) do
        if tab.key == key then
            tabbar.select(id, i, silent)
            return
        end
    end
end

-----------------------------------------------------------
-- Click Handling
-----------------------------------------------------------

function tabbar.findByElementHandle(elemHandle)
    for id, tb in pairs(tabbars) do
        for i, tab in ipairs(tb.tabs) do
            if tab.boxId == elemHandle then
                return id, i
            end
        end
    end
    return nil, nil
end

function tabbar.handleCallback(callbackName, elemHandle)
    if callbackName ~= TAB_CALLBACK then return false end
    
    local id, tabIndex = tabbar.findByElementHandle(elemHandle)
    if id and tabIndex then
        tabbar.select(id, tabIndex)
        return true
    end
    return false
end

function tabbar.isTabBarCallback(callbackName)
    return callbackName == TAB_CALLBACK
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function tabbar.getSelectedIndex(id)
    local tb = tabbars[id]
    if not tb then return nil end
    return tb.selectedIndex
end

function tabbar.getSelectedKey(id)
    local tb = tabbars[id]
    if not tb then return nil end
    local tab = tb.tabs[tb.selectedIndex]
    if not tab then return nil end
    return tab.key
end

function tabbar.getFrameHandle(id)
    local tb = tabbars[id]
    if not tb then return nil end
    return tb.frameBoxId
end

function tabbar.getFrameBounds(id)
    local tb = tabbars[id]
    if not tb then return 0, 0, 0, 0 end
    return tb.frameX, tb.frameY, tb.frameWidth, tb.frameHeight
end

function tabbar.getSize(id)
    local tb = tabbars[id]
    if not tb then return 0, 0 end
    return tb.width, tb.height + tb.frameHeight
end

-- Vertical extent of the tab strip alone, excluding any trailing
-- inter-row gap. Frame-free layouts only (the framed layout's strip is
-- always one `height`-tall row above its own frame).
function tabbar.getStripHeight(id)
    local tb = tabbars[id]
    if not tb then return 0 end
    return tb.stripHeight or tb.height
end

function tabbar.getRowCount(id)
    local tb = tabbars[id]
    if not tb then return 0 end
    return tb.rowCount or 1
end

-- Per-tab records ({name, key, width, height, x, y, boxId, labelId}).
-- Read-only view for hosts that must re-validate a remembered
-- selection, and for tests asserting real tab geometry.
function tabbar.getTabs(id)
    local tb = tabbars[id]
    if not tb then return {} end
    return tb.tabs
end

function tabbar.hasFrame(id)
    local tb = tabbars[id]
    if not tb then return false end
    return tb.frameBoxId ~= nil
end

-----------------------------------------------------------
-- Introspection (F3, #645)
-----------------------------------------------------------

-- One entry per tab (each has its own boxId and is independently
-- clickable); the content frame itself isn't interactive so it's not
-- dumped.
function tabbar.dump()
    local out = {}
    for id, tb in pairs(tabbars) do
        for i, tab in ipairs(tb.tabs) do
            local info = tab.boxId and UI.getElementInfo(tab.boxId) or nil
            if info and info.pageVisible and info.visible then
                table.insert(out, {
                    id = "tabbar:" .. id .. ":" .. i,
                    name = tab.name,
                    type = "tabbar",
                    bounds = {
                        x = info and info.x or 0,
                        y = info and info.y or 0,
                        w = info and info.width or tab.width,
                        h = info and info.height or tb.height,
                    },
                    label = tab.name,
                    enabled = info ~= nil and info.clickable,
                    visible = info ~= nil and info.visible,
                    hovered = info ~= nil and info.hovered,
                    focused = info ~= nil and info.focused,
                    value = (i == tb.selectedIndex),
                    screen = info and info.page or nil,
                    handle = info and info.handle or nil,
                })
            end
        end
    end
    return out
end

return tabbar
