-- Shared browser chrome for the --preview texture browser epic (#427,
-- Phase 2 #886): a scrollable, clipped left-hand list
-- (scripts/ui/list.lua already provides selection, virtual scrolling,
-- and the #747 clip viewport) plus a main-panel geometry region.
-- Category-agnostic by design (Requirement 2's amendment) — this module
-- owns list construction/scroll/click routing and hands the caller back
-- a plain {x,y,width,height} panel rect; what gets drawn there (a
-- static texture here, unit animation playback in #887, ...) is always
-- the caller's own concern, never embedded here.
local list = require("scripts.ui.list")

local assetBrowser = {}

local browsers = {}
local nextId = 1

function assetBrowser.init()
    list.init()
end

-- params:
--   page, x, y, width, height -- overall browser bounds
--   font, fontSize, itemHeight, maxVisible, uiscale, zIndex
--   listWidthFraction (default 0.35), gap (default 20)
--   entries = { { label = "...", path = "..." }, ... }
--   onSelect = function(path, label, index) end -- fired by
--     assetBrowser.selectEntry (below) and by every later click/
--     keyboard selection. NOT fired by 'new' itself — a caller doing
--     texture-fit math off getPanelBounds must be able to read it
--     BEFORE the initial selection fires (selectEntry's own doc
--     explains why), which 'new' auto-selecting internally could not
--     guarantee.
function assetBrowser.new(params)
    local id = nextId
    nextId = nextId + 1

    local gap = params.gap or 20
    local listWidth = params.listWidth
        or math.floor(params.width * (params.listWidthFraction or 0.35))

    local items = {}
    for i, e in ipairs(params.entries or {}) do
        items[i] = { text = e.label, value = e.path }
    end

    local onSelect = params.onSelect
    local listId = list.new({
        name       = (params.name or ("asset_browser_" .. id)) .. "_list",
        page       = params.page,
        x          = params.x,
        y          = params.y,
        width      = listWidth,
        font       = params.font,
        fontSize   = params.fontSize or 20,
        itemHeight = params.itemHeight or 32,
        maxVisible = params.maxVisible or 16,
        uiscale    = params.uiscale,
        zIndex     = params.zIndex or 1,
        items      = items,
        onSelect = function(value, text, index, _lid, _lname)
            if onSelect then onSelect(value, text, index) end
        end,
    })

    browsers[id] = {
        id     = id,
        listId = listId,
        items  = items,
        panelBounds = {
            x = params.x + listWidth + gap,
            y = params.y,
            width = params.width - listWidth - gap,
            height = params.height,
        },
    }

    return id
end

-- Select an entry by its path, falling back to the first entry when
-- 'path' is nil or not found (Requirement 3's "first entry selected by
-- default"). Fires onSelect synchronously, same as a real click —
-- callers MUST read getPanelBounds(id) before calling this: a resize
-- rebuild (#886 round-1 review) recreates the browser at NEW bounds,
-- and onSelect's texture-fit math needs those new bounds already in
-- hand, not whatever a caller's own state held from before this browser
-- existed.
function assetBrowser.selectEntry(id, path)
    local b = browsers[id]
    if not b or #b.items == 0 then return end
    local index = 1
    if path then
        for i, item in ipairs(b.items) do
            if item.value == path then
                index = i
                break
            end
        end
    end
    list.selectItem(b.listId, index)
end

function assetBrowser.destroy(id)
    local b = browsers[id]
    if not b then return end
    list.destroy(b.listId)
    browsers[id] = nil
end

function assetBrowser.getPanelBounds(id)
    local b = browsers[id]
    return b and b.panelBounds or nil
end

function assetBrowser.getSelectedPath(id)
    local b = browsers[id]
    if not b then return nil end
    return list.getSelectedValue(b.listId)
end

function assetBrowser.getSelectedLabel(id)
    local b = browsers[id]
    if not b then return nil end
    return list.getSelectedText(b.listId)
end

function assetBrowser.getScrollOffset(id)
    local b = browsers[id]
    if not b then return 0 end
    return list.getScrollOffset(b.listId)
end

-- Restore a scroll position (e.g. across a resize rebuild, #886).
function assetBrowser.setScrollOffset(id, offset)
    local b = browsers[id]
    if not b then return end
    list.setScrollOffset(b.listId, offset)
end

-- Introspection (#886 Requirement 6): per-visible-row interactive
-- bounds + handles, exactly list.dump()'s existing F3 (#645) contract
-- — sufficient for a probe to locate rows via UI.getElementInfo and
-- drive real input.click/scroll without hardcoded coordinates.
function assetBrowser.dump(id)
    local b = browsers[id]
    if not b then return {} end
    return list.dump()
end

-----------------------------------------------------------
-- Input routing (delegates straight to scripts.ui.list — see
-- scripts/ui_manager_scroll.lua for the identical pattern every other
-- list-backed screen in the normal boot path uses).
-----------------------------------------------------------

function assetBrowser.handleCallback(callbackName, elemHandle)
    return list.handleCallback(callbackName, elemHandle)
end

function assetBrowser.onScroll(elemHandle, dx, dy)
    return list.onScroll(elemHandle, dx, dy)
end

return assetBrowser
