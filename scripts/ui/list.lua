-- List UI component (selectable list with hover highlight + optional scrollbar)
-- Modeled after dropdown.lua but simpler: no text input, no arrow button.
-- Items are { text = "...", value = "...", icon = <textureHandle>,
--             iconUV = { u0, v0, u1, v1 } }
--
-- `icon` is OPT-IN and inert unless the list was created with
-- params.iconSize (#887): without it no icon element is created at all
-- and every existing caller behaves exactly as before. With it, each
-- visible slot gains a square sprite left of its label and the label
-- indents past it; an item with no `icon` simply leaves its slot's
-- sprite hidden.
--
-- `iconUV` is the sub-rect to sample within that texture (#1260) — an
-- icon that is one CELL of a compiled atlas rather than a whole image.
-- Omitted, the icon is the whole texture, exactly as before. Texture
-- and sub-rect are published together via UI.setSpriteFrame so the
-- render thread never observes a new image paired with the previous
-- one's rect.
--
-- params.columns (#1107) is OPT-IN the same way: an ordered list of
-- EXTRA text columns, one real text element per column per visible
-- slot, so a row can carry several independent facts that stay
-- separately readable rather than being concatenated into one string.
-- Each entry is
--     { key       = "<item field>",       -- the value comes from item[key]
--       fraction  = <0..1 of the row's usable width>,
--       color     = {r,g,b,a},            -- resting color, own by default
--       line      = <1 (default) or more>,-- which text line of the row
--       fontScale = <multiplier, default 1> }
-- nil (every pre-#1107 caller) means no column elements exist at all,
-- and a single-line list lays out byte-identically to before.
--
-- `line` exists because a row whose width is already spoken for cannot
-- gain a column without taking space from what it already showed:
-- putting the new fields on line 2 leaves line 1's allocation intact.
-- The lines are laid out as one tight block centered in the row, so the
-- CALLER must size itemHeight to hold them all.
local scale = require("scripts.ui.scale")
local scrollbar = require("scripts.ui.scrollbar")
local textWrap = require("scripts.ui.text_wrap")
local list = {}

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------
local LIST_ITEM_CALLBACK = "onListItemClick"

-----------------------------------------------------------
-- Module State
-----------------------------------------------------------

local lists = {}
local nextId = 1

local highlightTex = nil
local assetsLoaded = false

-- Publish an item's icon onto its slot sprite: texture and sub-rect in
-- ONE manager transition (see the header note on `iconUV`). An item
-- without a sub-rect gets the whole image, which is the pre-#1260
-- behaviour spelled out rather than a separate code path.
local function applyIcon(iconId, item)
    local uv = item.iconUV
    UI.setSpriteFrame(iconId, item.icon,
        uv and uv[1] or 0.0, uv and uv[2] or 0.0,
        uv and uv[3] or 1.0, uv and uv[4] or 1.0, false)
end

-----------------------------------------------------------
-- Initialization
-----------------------------------------------------------

function list.init()
    if assetsLoaded then return end

    highlightTex = engine.loadTexture("assets/textures/ui/highlight.png", "ui")

    scrollbar.init()

    assetsLoaded = true
    engine.logDebug("List module initialized")
end

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

-- Bound one row field to its allocated column (#1107). A list with no
-- columns keeps its historical behavior exactly: the raw text, bounded
-- only by the viewport clip.
local function fitField(ls, text, maxPx, fontPx)
    if not ls.columns then return text end
    return textWrap.truncateToWidth(text, ls.font, fontPx or ls.fontSize, maxPx)
end

-- The value one item contributes to column `i`, as a string. A missing
-- field is an EMPTY column, never a substitute drawn from another field
-- -- an unnamed world must not read as though it were named.
local function columnText(ls, item, i)
    local raw = item and ls.columns[i].key and item[ls.columns[i].key]
    if raw == nil then return "" end
    return tostring(raw)
end

-- Recolor a whole row. `emphasis` is the selected/hover color that
-- overrides every field at once, or nil to restore each field's own
-- resting color -- a column may define its own (a secondary fact reads
-- dimmer than the row's identity), so restoring cannot simply reuse the
-- primary label's.
local function setRowColor(ls, slot, emphasis)
    local c = emphasis or ls.textColor
    UI.setColor(slot.textId, c[1], c[2], c[3], c[4])
    if slot.columnIds then
        for i, cId in ipairs(slot.columnIds) do
            local cc = emphasis or ls.columns[i].color or ls.textColor
            UI.setColor(cId, cc[1], cc[2], cc[3], cc[4])
        end
    end
end

-- One rendered row field, for list.dump (#1107). `width` is what the
-- column was ALLOCATED; `textWidth` is what the rendered text actually
-- measures, so a caller can tell "fits" from "was truncated to fit"
-- without re-deriving the list's own font metrics.
local function describeField(ls, elemId, width, fontPx)
    local info = elemId and UI.getElementInfo(elemId) or nil
    local text = (info and info.text) or ""
    return {
        text = text,
        x = info and info.x or ls.x,
        y = info and info.y or ls.y,
        width = width or 0,
        textWidth = engine.getTextWidth(ls.font, text,
                        fontPx or ls.fontSize) or 0,
    }
end

function list.measureItems(items, font, fontSize)
    local maxWidth = 0
    for _, item in ipairs(items) do
        local w = engine.getTextWidth(font, item.text, fontSize)
        if w > maxWidth then
            maxWidth = w
        end
    end
    return maxWidth
end

-----------------------------------------------------------
-- Creation / Destruction
-----------------------------------------------------------

function list.new(params)
    local id = nextId
    nextId = nextId + 1

    local uiscale = params.uiscale or scale.get()
    local fontSize = math.floor((params.fontSize or 24) * uiscale)
    local itemHeight = math.floor((params.itemHeight or 36) * uiscale)
    local textPadding = math.floor((params.textPadding or 10) * uiscale)
    local scrollButtonSize = math.floor((params.scrollButtonSize or 24) * uiscale)

    local textColor = params.textColor or {1.0, 1.0, 1.0, 1.0}
    local highlightColor = params.highlightColor or {0.3, 0.5, 0.8, 0.8}
    local highlightTextColor = params.highlightTextColor or {1.0, 1.0, 1.0, 1.0}
    local selectedColor = params.selectedColor or {0.2, 0.4, 0.7, 1.0}
    local selectedTextColor = params.selectedTextColor or {1.0, 1.0, 1.0, 1.0}

    local items = params.items or {}
    local maxVisible = params.maxVisible or 10

    -- Optional per-row icon column (#887). nil = no icons at all (the
    -- historical behavior); a number is the unscaled square edge, the
    -- same `* uiscale` treatment every other dimension above gets.
    local iconSize = params.iconSize and math.max(1, math.floor(params.iconSize * uiscale)) or nil
    -- Where the label starts: past the icon column when there is one.
    local labelX = iconSize and (textPadding * 2 + iconSize) or textPadding

    -- Horizontal text alignment. Defaults to "left" (legacy behavior:
    -- text sits at x + textPadding). "center" / "right" compute an
    -- offset per-item using engine.getTextWidth so labels of varying
    -- widths still align cleanly.
    local textAlign = params.textAlign or "left"

    -- Optional extra text columns (#1107). Column geometry is a LIST
    -- level property, not a per-item one, so the columns line up down
    -- the whole list; each item supplies only its own text per column.
    -- Deliberately left-aligned (and forcing the primary label back to
    -- left too): a per-item textAlign offset is computed once at
    -- creation and never revisited, which a scrolling multi-column row
    -- would silently desync.
    local columns = params.columns
    if columns and #columns == 0 then columns = nil end
    if columns then textAlign = "left" end

    local visibleCount = math.min(#items, maxVisible)
    local listWidth = params.width or 300
    local listHeight = visibleCount * itemHeight

    -- Resolve the column layout once (#1107). The usable span is
    -- between the label's start and the row's right padding, and each
    -- column claims its `fraction` of it; on LINE 1 the primary label
    -- keeps whatever is left over, on any further line the columns
    -- start from the label's own x. Each width is a hard bound the row
    -- text is truncated to -- the viewport clip alone is not enough
    -- once a row carries more than one fact, because a clip cuts a
    -- glyph mid-stroke and lets one overlong field run visually into
    -- the next column. Every width floors at 0, so an out-of-envelope
    -- framebuffer/scale combination degrades instead of producing
    -- negative geometry.
    --
    -- A column's `line` is what lets a row carry facts that genuinely
    -- do not fit side by side: putting every added field on line 2
    -- leaves line 1's own allocation untouched, so a row that already
    -- filled its width keeps showing exactly what it did before. The
    -- caller is responsible for an `itemHeight` tall enough for the
    -- lines it asked for.
    local columnWidths, columnX, columnLine, columnFontSize = nil, nil, nil, nil
    local primaryWidth = nil
    -- Tallest font on each text line; line 1 always exists (the primary
    -- label), so a row is at minimum what it always was.
    local lineFont = { fontSize }
    if columns then
        local content = math.max(0, listWidth - labelX - textPadding)
        columnWidths, columnX, columnLine, columnFontSize = {}, {}, {}, {}
        local claimed = {}
        for i, col in ipairs(columns) do
            local line = col.line or 1
            local taken = claimed[line] or 0
            local w = math.max(0, math.floor(content * (col.fraction or 0)))
            if taken + w > content then w = math.max(0, content - taken) end
            columnWidths[i] = w
            columnLine[i] = line
            local fs = math.max(1, math.floor(fontSize * (col.fontScale or 1)))
            columnFontSize[i] = fs
            claimed[line] = taken + w
            if not lineFont[line] or fs > lineFont[line] then
                lineFont[line] = fs
            end
        end
        primaryWidth = math.max(0, content - (claimed[1] or 0))
        -- Line 1 resumes after the primary label; every other line
        -- starts at the label column, since nothing precedes it there.
        local cursor = { [1] = labelX + primaryWidth }
        for i = 1, #columns do
            local line = columnLine[i]
            cursor[line] = cursor[line] or labelX
            columnX[i] = cursor[line]
            cursor[line] = cursor[line] + columnWidths[i]
        end
    end

    -- Baseline of each text line within a slot. Empirically the text
    -- element's origin is the baseline of a glyph row whose ascent is
    -- close to the full font size (pixel font with little/no
    -- descender), so centering a one-line block means baseline at
    -- slotCenter + fontSize/2.
    --
    -- The lines are laid out as one TIGHT block centered in the row,
    -- rather than one line per equal share of it. Even shares make the
    -- gap inside a row identical to the gap between rows, and a reader
    -- then can't tell which row a second line belongs to; leaving the
    -- slack at the row's edges groups each row's lines together. With a
    -- single line the result is byte-identical to the pre-#1107
    -- formula.
    local lineLeading = math.max(1, math.floor(4 * uiscale))
    local lineBaseline = {}
    do
        local blockHeight = 0
        for line = 1, #lineFont do
            blockHeight = blockHeight + lineFont[line]
                        + (line > 1 and lineLeading or 0)
        end
        local offset = math.floor((itemHeight - blockHeight) / 2)
        for line = 1, #lineFont do
            offset = offset + (line > 1 and lineLeading or 0) + lineFont[line]
            lineBaseline[line] = offset
        end
    end

    local function baselineFor(slotY, line)
        return slotY + (lineBaseline[line] or lineBaseline[1])
    end

    local ls = {
        id = id,
        name = params.name or ("list_" .. id),
        x = params.x or 0,
        y = params.y or 0,
        width = listWidth,
        height = listHeight,
        fontSize = fontSize,
        itemHeight = itemHeight,
        textPadding = textPadding,
        scrollButtonSize = scrollButtonSize,
        page = params.page,
        font = params.font,
        items = items,
        maxVisible = maxVisible,
        visibleCount = visibleCount,
        selectedIndex = nil,
        hoveredSlot = nil,
        uiscale = uiscale,
        zIndex = params.zIndex or 1,
        textAlign = textAlign,
        iconSize = iconSize,
        labelX = labelX,
        columns = columns,
        columnWidths = columnWidths,
        columnX = columnX,
        columnFontSize = columnFontSize,
        primaryWidth = primaryWidth,
        -- Colors
        textColor = textColor,
        highlightColor = highlightColor,
        highlightTextColor = highlightTextColor,
        selectedColor = selectedColor,
        selectedTextColor = selectedTextColor,
        -- Callbacks
        onSelect = params.onSelect or nil,
        -- Scrollbar
        scrollbarId = nil,
        scrollOffset = 0,
        needsScroll = #items > maxVisible,
        -- Element handles
        slotElements = {},  -- { hitId, textId, highlightId, slot }
        viewportId = nil,
    }

    -- #747: a shared, opt-in clipping viewport that every visible slot
    -- is parented under (relative offsets) instead of being placed
    -- directly on the page at absolute coordinates. Virtualization
    -- (only `visibleCount` slots ever exist) still does the heavy
    -- lifting, but this closes the gap where a rounding/resize edge
    -- case could otherwise leave a row visible or clickable outside
    -- the list's own bounds. The scrollbar stays a page-level sibling
    -- (outside the clip) since it's chrome, not list content.
    ls.viewportId = UI.newElement(ls.name .. "_viewport", listWidth, listHeight, ls.page)
    UI.addToPage(ls.page, ls.viewportId, ls.x, ls.y)
    UI.setClipChildren(ls.viewportId, true)

    -- Create visible item slots
    ls.slotElements = {}
    for i = 1, visibleCount do
        local slotY = (i - 1) * itemHeight
        local textY = baselineFor(slotY, 1)

        -- Highlight sprite (hidden by default)
        local hlId = UI.newSprite(
            ls.name .. "_hl_" .. i,
            listWidth,
            itemHeight,
            highlightTex,
            highlightColor[1], highlightColor[2],
            highlightColor[3], highlightColor[4],
            ls.page
        )
        UI.addChild(ls.viewportId, hlId, 0, slotY)
        UI.setZIndex(hlId, ls.zIndex)
        UI.setVisible(hlId, false)

        -- Optional icon column (#887): one square sprite per visible
        -- slot, vertically centered, created only when the caller asked
        -- for icons. Starts on the item's own handle (or hidden when
        -- the item has none); refreshSlots keeps it in sync from then on.
        local iconId = nil
        if iconSize then
            local item0 = items[i]
            iconId = UI.newSprite(
                ls.name .. "_icon_" .. i,
                iconSize, iconSize,
                (item0 and item0.icon) or highlightTex,
                1.0, 1.0, 1.0, 1.0,
                ls.page
            )
            UI.addChild(ls.viewportId, iconId,
                textPadding, slotY + math.floor((itemHeight - iconSize) / 2))
            UI.setZIndex(iconId, ls.zIndex + 1)
            UI.setVisible(iconId, (item0 and item0.icon) ~= nil)
            if item0 and item0.icon then applyIcon(iconId, item0) end
        end

        -- Text label
        local dataIndex = i
        local itemText = ""
        if dataIndex <= #items then
            itemText = items[dataIndex].text
        end

        local txtId = UI.newText(
            ls.name .. "_txt_" .. i,
            fitField(ls, itemText, primaryWidth),
            ls.font,
            fontSize,
            textColor[1], textColor[2], textColor[3], textColor[4],
            ls.page
        )
        -- Resolve horizontal position based on textAlign. For "center"
        -- and "right" we need each item's pixel width to position
        -- correctly; "left" is just a constant offset. Relative to the
        -- viewport (0,0 = the list's own top-left), not the page.
        local textX = labelX
        if textAlign == "center" or textAlign == "right" then
            local tw = engine.getTextWidth(ls.font, itemText, fontSize)
            if textAlign == "center" then
                textX = math.floor((listWidth - tw) / 2)
            else
                textX = listWidth - tw - textPadding
            end
        end
        UI.addChild(ls.viewportId, txtId, textX, textY)
        UI.setZIndex(txtId, ls.zIndex + 2)

        -- Extra columns (#1107): one real text element per column, so
        -- each fact is independently readable (and independently
        -- findable by name from a test/introspection oracle) rather
        -- than being spliced into the primary label.
        local columnIds = nil
        if columns then
            columnIds = {}
            for c, col in ipairs(columns) do
                local cc = col.color or textColor
                local cfs = columnFontSize[c]
                local cId = UI.newText(
                    ls.name .. "_col" .. c .. "_" .. i,
                    fitField(ls, columnText(ls, items[dataIndex], c),
                             columnWidths[c], cfs),
                    ls.font,
                    cfs,
                    cc[1], cc[2], cc[3], cc[4],
                    ls.page
                )
                UI.addChild(ls.viewportId, cId,
                    columnX[c], baselineFor(slotY, columnLine[c]))
                UI.setZIndex(cId, ls.zIndex + 2)
                columnIds[c] = cId
            end
        end

        -- Invisible hit-box sprite for click detection
        local hitId = UI.newSprite(
            ls.name .. "_hit_" .. i,
            listWidth,
            itemHeight,
            highlightTex,
            0.0, 0.0, 0.0, 0.0,
            ls.page
        )
        UI.addChild(ls.viewportId, hitId, 0, slotY)
        UI.setClickable(hitId, true)
        UI.setOnClick(hitId, LIST_ITEM_CALLBACK)
        -- #743: explicit scroll-capture so hovering a row and scrolling
        -- still reaches list.onScroll — pre-#743 this rode along for
        -- free on any clickable+onClick element; now it's independent.
        UI.setScrollCapture(hitId, true)
        UI.setZIndex(hitId, ls.zIndex + 3)

        table.insert(ls.slotElements, {
            hitId = hitId,
            textId = txtId,
            highlightId = hlId,
            iconId = iconId,
            columnIds = columnIds,
            slot = i,
        })
    end

    -- Create scrollbar if needed
    if ls.needsScroll then
        local trackHeight = listHeight
            - (scrollButtonSize * 2)
            - (math.floor(4 * uiscale) * 2)
        if trackHeight < math.floor(20 * uiscale) then
            trackHeight = math.floor(20 * uiscale)
        end

        ls.scrollbarId = scrollbar.new({
            name = ls.name .. "_scrollbar",
            page = ls.page,
            x = ls.x + listWidth,
            y = ls.y,
            buttonSize = scrollButtonSize,
            trackHeight = trackHeight,
            capHeight = math.floor(4 * uiscale),
            tileSize = math.floor(8 * uiscale),
            totalItems = #items,
            visibleItems = visibleCount,
            uiscale = uiscale,
            zIndex = ls.zIndex + 4,
            onScroll = function(offset, sbId, sbName)
                list.onScrollChanged(id, offset)
            end,
        })
    end

    lists[id] = ls

    engine.logDebug("List created: " .. ls.name
        .. " items=" .. #ls.items
        .. " visible=" .. ls.visibleCount
        .. " scroll=" .. tostring(ls.needsScroll))

    return id
end

function list.destroy(id)
    local ls = lists[id]
    if not ls then return end

    if ls.scrollbarId then
        scrollbar.destroy(ls.scrollbarId)
        ls.scrollbarId = nil
    end

    for _, slot in ipairs(ls.slotElements) do
        if slot.hitId then UI.deleteElement(slot.hitId) end
        if slot.textId then UI.deleteElement(slot.textId) end
        if slot.highlightId then UI.deleteElement(slot.highlightId) end
        if slot.iconId then UI.deleteElement(slot.iconId) end
        if slot.columnIds then
            for _, cId in ipairs(slot.columnIds) do UI.deleteElement(cId) end
        end
    end
    if ls.viewportId then UI.deleteElement(ls.viewportId) end

    lists[id] = nil
    engine.logDebug("List destroyed: " .. (ls.name or "?"))
end

function list.destroyAll()
    for id, _ in pairs(lists) do
        list.destroy(id)
    end
    lists = {}
    nextId = 1
end

-----------------------------------------------------------
-- Virtual Scrolling
-----------------------------------------------------------

function list.onScrollChanged(id, newOffset)
    local ls = lists[id]
    if not ls then return end

    ls.scrollOffset = newOffset
    ls.hoveredSlot = nil
    list.refreshSlots(id)
end

function list.refreshSlots(id)
    local ls = lists[id]
    if not ls then return end

    for _, slot in ipairs(ls.slotElements) do
        local dataIndex = ls.scrollOffset + slot.slot

        if dataIndex <= #ls.items then
            local item = ls.items[dataIndex]
            UI.setText(slot.textId, fitField(ls, item.text, ls.primaryWidth))

            if slot.columnIds then
                for c, cId in ipairs(slot.columnIds) do
                    UI.setText(cId, fitField(ls, columnText(ls, item, c),
                        ls.columnWidths[c], ls.columnFontSize[c]))
                end
            end

            if slot.iconId then
                if item.icon then
                    applyIcon(slot.iconId, item)
                    UI.setVisible(slot.iconId, true)
                else
                    UI.setVisible(slot.iconId, false)
                end
            end

            -- Determine colors based on selection state
            if dataIndex == ls.selectedIndex then
                UI.setVisible(slot.highlightId, true)
                setRowColor(ls, slot, ls.selectedTextColor)
            else
                UI.setVisible(slot.highlightId, false)
                setRowColor(ls, slot, nil)
            end
        else
            UI.setText(slot.textId, "")
            if slot.columnIds then
                for _, cId in ipairs(slot.columnIds) do UI.setText(cId, "") end
            end
            UI.setVisible(slot.highlightId, false)
            if slot.iconId then UI.setVisible(slot.iconId, false) end
        end
    end
end

-----------------------------------------------------------
-- Hover Handling
-----------------------------------------------------------

function list.setHoveredSlot(id, slotIndex)
    local ls = lists[id]
    if not ls then return end
    if ls.hoveredSlot == slotIndex then return end

    -- Clear previous hover
    if ls.hoveredSlot then
        local prev = ls.slotElements[ls.hoveredSlot]
        if prev then
            local prevDataIndex = ls.scrollOffset + ls.hoveredSlot
            if prevDataIndex == ls.selectedIndex then
                -- Restore selected appearance
                UI.setVisible(prev.highlightId, true)
                setRowColor(ls, prev, ls.selectedTextColor)
            else
                UI.setVisible(prev.highlightId, false)
                setRowColor(ls, prev, nil)
            end
        end
    end

    ls.hoveredSlot = slotIndex

    -- Apply new hover
    if slotIndex then
        local slot = ls.slotElements[slotIndex]
        if slot then
            local dataIndex = ls.scrollOffset + slotIndex
            if dataIndex >= 1 and dataIndex <= #ls.items then
                UI.setVisible(slot.highlightId, true)
                setRowColor(ls, slot, ls.highlightTextColor)
            end
        end
    end
end

function list.clearHover(id)
    list.setHoveredSlot(id, nil)
end

function list.onHoverEnter(elemHandle)
    for id, ls in pairs(lists) do
        for _, slot in ipairs(ls.slotElements) do
            if slot.hitId == elemHandle then
                list.setHoveredSlot(id, slot.slot)
                return
            end
        end
    end
end

function list.onHoverLeave(elemHandle)
    for id, ls in pairs(lists) do
        if ls.hoveredSlot then
            local slot = ls.slotElements[ls.hoveredSlot]
            if slot and slot.hitId == elemHandle then
                list.clearHover(id)
                return
            end
        end
    end
end

-----------------------------------------------------------
-- Selection
-----------------------------------------------------------

function list.selectItem(id, dataIndex)
    local ls = lists[id]
    if not ls then return end
    if dataIndex < 1 or dataIndex > #ls.items then return end

    ls.selectedIndex = dataIndex
    list.refreshSlots(id)

    if ls.onSelect then
        local item = ls.items[dataIndex]
        ls.onSelect(item.value, item.text, dataIndex, id, ls.name)
    end

    engine.logDebug("List selected: " .. ls.name
        .. " [" .. dataIndex .. "] = " .. ls.items[dataIndex].text)
end

-- Like list.selectItem, but never fires onSelect (#748). For restoring
-- highlight state after a rebuild that re-created the list (e.g. a
-- framebuffer-resize rebuild) — re-running the real selection callback
-- there would re-trigger whatever consequential action it performs
-- (save_browser's onSelect loads and transitions the whole game).
function list.setSelectedIndex(id, dataIndex)
    local ls = lists[id]
    if not ls then return end
    if dataIndex < 1 or dataIndex > #ls.items then return end

    ls.selectedIndex = dataIndex
    list.refreshSlots(id)
end

-----------------------------------------------------------
-- Click Handling
-----------------------------------------------------------

function list.findByElementHandle(elemHandle)
    for id, ls in pairs(lists) do
        for _, slot in ipairs(ls.slotElements) do
            if slot.hitId == elemHandle then
                return id, slot.slot
            end
        end
    end
    return nil, nil
end

function list.handleCallback(callbackName, elemHandle)
    if callbackName == LIST_ITEM_CALLBACK then
        local id, slotIdx = list.findByElementHandle(elemHandle)
        if id and slotIdx then
            local ls = lists[id]
            local dataIndex = ls.scrollOffset + slotIdx
            if dataIndex >= 1 and dataIndex <= #ls.items then
                list.selectItem(id, dataIndex)
            end
            return true
        end
    elseif callbackName == "onScrollUp" then
        local sbId, _ = scrollbar.findByElementHandle(elemHandle)
        if sbId then
            -- Check if this scrollbar belongs to one of our lists
            for id, ls in pairs(lists) do
                if ls.scrollbarId == sbId then
                    scrollbar.scrollUp(sbId)
                    return true
                end
            end
        end
    elseif callbackName == "onScrollDown" then
        local sbId, _ = scrollbar.findByElementHandle(elemHandle)
        if sbId then
            for id, ls in pairs(lists) do
                if ls.scrollbarId == sbId then
                    scrollbar.scrollDown(sbId)
                    return true
                end
            end
        end
    end
    return false
end

-----------------------------------------------------------
-- Scroll Input (mouse wheel)
-----------------------------------------------------------

function list.onScroll(elemHandle, dx, dy)
    for id, ls in pairs(lists) do
        if ls.needsScroll and ls.scrollbarId then
            local isInList = false

            for _, slot in ipairs(ls.slotElements) do
                if slot.hitId == elemHandle or slot.textId == elemHandle
                    or slot.highlightId == elemHandle then
                    isInList = true
                    break
                end
                for _, cId in ipairs(slot.columnIds or {}) do
                    if cId == elemHandle then
                        isInList = true
                        break
                    end
                end
                if isInList then break end
            end

            if not isInList then
                local sbId, _ = scrollbar.findByElementHandle(elemHandle)
                if sbId == ls.scrollbarId then
                    isInList = true
                end
            end

            if isInList then
                if dy > 0 then
                    scrollbar.scrollUp(ls.scrollbarId)
                elseif dy < 0 then
                    scrollbar.scrollDown(ls.scrollbarId)
                end
                return true
            end
        end
    end
    return false
end

-----------------------------------------------------------
-- Data Management
-----------------------------------------------------------

function list.setItems(id, items)
    local ls = lists[id]
    if not ls then return end

    ls.items = items or {}
    ls.selectedIndex = nil
    ls.scrollOffset = 0
    ls.hoveredSlot = nil

    local needsScroll = #ls.items > ls.maxVisible
    ls.needsScroll = needsScroll

    if ls.scrollbarId then
        if needsScroll then
            scrollbar.setContentSize(ls.scrollbarId, #ls.items, ls.visibleCount)
        else
            scrollbar.setVisible(ls.scrollbarId, false)
        end
    end

    list.refreshSlots(id)
end

-----------------------------------------------------------
-- Visibility / Position
-----------------------------------------------------------

function list.setVisible(id, visible)
    local ls = lists[id]
    if not ls then return end

    -- The clipping viewport (#747) is itself a real, sized element, so
    -- it must follow the list's own visibility toggle: left visible
    -- while every slot underneath it is hidden, it would still be a
    -- valid hover/tooltip/hit target (an empty box with nothing behind
    -- it reachable), blocking whatever's actually behind a hidden list.
    UI.setVisible(ls.viewportId, visible)

    for _, slot in ipairs(ls.slotElements) do
        UI.setVisible(slot.hitId, visible)
        UI.setVisible(slot.textId, visible)
        if slot.columnIds then
            for _, cId in ipairs(slot.columnIds) do
                UI.setVisible(cId, visible)
            end
        end
        if not visible then
            UI.setVisible(slot.highlightId, false)
            if slot.iconId then UI.setVisible(slot.iconId, false) end
        end
    end
    -- Re-showing an ICON list only (no behavior change for every other
    -- caller): refreshSlots is the single owner of per-slot icon
    -- visibility — an item with no icon must stay hidden — so let it
    -- decide rather than blanket-showing every icon element here.
    if visible and ls.iconSize then list.refreshSlots(id) end

    if ls.scrollbarId then
        scrollbar.setVisible(ls.scrollbarId, visible and ls.needsScroll)
    end
end

function list.setPosition(id, x, y)
    local ls = lists[id]
    if not ls then return end

    ls.x = x
    ls.y = y

    -- #747: every slot is a CHILD of the clipping viewport at a fixed
    -- relative offset (set once at creation), so moving the list is
    -- just moving the one viewport element — its descendants follow
    -- automatically, textAlign offset included.
    UI.setPosition(ls.viewportId, x, y)

    if ls.scrollbarId then
        scrollbar.setPosition(ls.scrollbarId, x + ls.width, y)
    end
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function list.getSize(id)
    local ls = lists[id]
    if not ls then return 0, 0 end
    local totalW = ls.width
    if ls.needsScroll and ls.scrollbarId then
        totalW = totalW + scrollbar.getTrackWidth(ls.scrollbarId)
    end
    return totalW, ls.height
end

function list.getSelectedIndex(id)
    local ls = lists[id]
    if not ls then return nil end
    return ls.selectedIndex
end

function list.getScrollOffset(id)
    local ls = lists[id]
    if not ls then return 0 end
    return ls.scrollOffset
end

-- Set the scroll offset directly (e.g. restoring it across a geometry
-- rebuild, #886). Routed through the scrollbar widget when one exists so
-- its tab position and the list content never desync — the identical
-- path a real scrollbar drag/click already uses.
function list.setScrollOffset(id, offset)
    local ls = lists[id]
    if not ls then return end
    if ls.scrollbarId then
        scrollbar.setScrollOffset(ls.scrollbarId, offset)
    else
        ls.scrollOffset = math.max(0, math.floor(offset))
        list.refreshSlots(id)
    end
end

function list.getSelectedValue(id)
    local ls = lists[id]
    if not ls then return nil end
    if not ls.selectedIndex then return nil end
    return ls.items[ls.selectedIndex].value
end

function list.getSelectedText(id)
    local ls = lists[id]
    if not ls then return nil end
    if not ls.selectedIndex then return nil end
    return ls.items[ls.selectedIndex].text
end

function list.isListCallback(callbackName)
    return callbackName == LIST_ITEM_CALLBACK
end

-- The shared highlight chrome handle (#887): lets a caller reuse the
-- ONE texture list.init() already loaded for its own selection
-- markers/hit boxes instead of issuing a second engine.loadTexture for
-- the same path (there is no dedup at that layer — every call allocates
-- a fresh handle). nil until list.init() has run.
function list.getChromeTexture()
    return highlightTex
end

-----------------------------------------------------------
-- Introspection (F3, #645)
-----------------------------------------------------------

-- One entry per visible slot (each has its own hit-box and is
-- independently clickable); slots beyond the current item count are
-- skipped (empty rows in a short list).
function list.dump()
    local out = {}
    for id, ls in pairs(lists) do
        for _, slot in ipairs(ls.slotElements) do
            local dataIndex = ls.scrollOffset + slot.slot
            local item = ls.items[dataIndex]
            if item then
                local info = slot.hitId and UI.getElementInfo(slot.hitId) or nil
                if info and info.pageVisible and info.visible then
                    -- #1107: a multi-column row's fields AS RENDERED
                    -- (post-truncation), each with the absolute
                    -- position it drew at, the width it was allocated
                    -- and the width it actually measures. `label` stays
                    -- the primary item text alone, so an oracle reads
                    -- each fact separately instead of matching one
                    -- concatenated string, and can check a field
                    -- against its own column rather than the row.
                    -- Emitted only for a multi-column list, so a
                    -- plain list's dump keeps its historical shape and
                    -- cost.
                    local primary, cols = nil, nil
                    if ls.columns then
                        cols = {}
                        primary = describeField(ls, slot.textId, ls.primaryWidth)
                        for c, col in ipairs(ls.columns) do
                            cols[col.key] = describeField(ls,
                                slot.columnIds and slot.columnIds[c],
                                ls.columnWidths[c], ls.columnFontSize[c])
                        end
                    end
                    table.insert(out, {
                        id = "list:" .. id .. ":" .. slot.slot,
                        name = ls.name .. "_item_" .. dataIndex,
                        type = "list",
                        primary = primary,
                        columns = cols,
                        bounds = {
                            x = info and info.x or ls.x,
                            y = info and info.y or ls.y,
                            w = info and info.width or ls.width,
                            h = info and info.height or ls.itemHeight,
                        },
                        label = item.text,
                        enabled = info ~= nil and info.clickable,
                        visible = info ~= nil and info.visible,
                        hovered = info ~= nil and info.hovered,
                        focused = info ~= nil and info.focused,
                        value = (dataIndex == ls.selectedIndex),
                        screen = info and info.page or nil,
                        handle = info and info.handle or nil,
                    })
                end
            end
        end
    end
    return out
end

return list
