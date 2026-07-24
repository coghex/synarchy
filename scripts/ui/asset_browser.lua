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
--   onSelect = function(path, label, index) end -- fired once at
--     creation for the first entry (Requirement 3's "first entry
--     selected by default") and again on every later click/keyboard
--     selection.
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
        panelBounds = {
            x = params.x + listWidth + gap,
            y = params.y,
            width = params.width - listWidth - gap,
            height = params.height,
        },
    }

    -- Requirement 3: the first entry is selected by default.
    if #items > 0 then
        list.selectItem(listId, 1)
    end

    return id
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
