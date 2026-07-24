-- Preview mode entry point (#632 Phase 1, #886 Phase 2 of the
-- --preview texture browser epic #427).
--
-- Loaded by scripts/init.lua's game.init in place of the ~25 normal
-- gameplay/UI scripts whenever engine.getBootProfile() == "preview".
-- Phase 1 proved the boot skeleton: a font, one placeholder label, the
-- grey (#828382) clear color Engine.Graphics.Vulkan.Command.Record
-- already special-cases for the BootPreview profile. Phase 2 adds the
-- real simple-category browser: app/Main.hs already discovered and
-- validated everything (Engine.Preview.Discovery) BEFORE this script
-- ever ran, so engine.getPreviewBrowse() is the single source of truth
-- here — nil keeps the Phase 1 placeholder alive for a grouped category
-- (#887/#888 land their own browsing later), "list" backs a bare
-- --preview <simple category>, "item" backs a validated
-- --preview <simple category>/<item> (focused mode: no list).
--
-- Trimmed loading (Requirement 5) still holds: this module loads its
-- font, scripts.ui.list's chrome texture (highlight.png, only when a
-- list is actually built), and textures ON SELECTION from the ONE
-- requested category — never data/*.yaml gameplay catalogs, unrelated
-- world/HUD texture sets, or the normal script set. Eager-vs-on-
-- selection loading is implementer's discretion (#886) — this module
-- loads lazily (one texture at a time, as each entry is selected) and
-- never unloads (no engine.unloadTexture exists; acceptable for a
-- short-lived debug/preview session).
local label = require("scripts.ui.label")
local assetBrowser = require("scripts.ui.asset_browser")

-- #886: self-register into the require cache (the same convention
-- scripts/unit_ai.lua and scripts/debug.lua use) so the debug console
-- can reach this module's LIVE state via
-- `require("scripts.preview_manager").dump()` even though it's actually
-- loaded via engine.loadScript (dofile), not require.
local previewManager = package.loaded["scripts.preview_manager"] or {}
package.loaded["scripts.preview_manager"] = previewManager

local FONT_SIZE = 24
local labelFont = nil
local page = nil

-- mode: "placeholder" (Phase 1, grouped category) | "list" | "item"
local mode = nil
local readyState = "loading"  -- "loading" | "ready" | "empty"

-- Phase 1 placeholder (grouped category / no browse state).
local labelId = nil

-- Phase 2 simple-category browsing state.
local browserId = nil
local entries = nil
local focusedEntry = nil
local panelBounds = nil
local spriteId = nil

-- path -> texture handle, for entries already uploaded; never evicted
-- (see the module comment above).
local textureCache = {}
-- Every path this module has ever requested via engine.loadTexture, in
-- request order — the trimmed-loading probe observable (#886
-- Requirement 5/7): every entry here must resolve under the requested
-- category's own root (list mode's texture requests) or be a
-- documented chrome asset (scripts.ui.list's highlight.png).
local loadedPaths = {}
-- The texture handle/path the CURRENT selection is waiting on (nil once
-- it resolves) — guards against a stale onAssetLoaded from a
-- since-abandoned selection landing after the user already picked
-- something else.
local pendingHandle = nil
local pendingPath = nil

local function targetText()
    local target = engine.getPreviewTarget()
    if not target then
        return "Preview: (no target)"
    elseif target.item then
        return "Preview: " .. target.category .. "/" .. target.item
    else
        return "Preview: " .. target.category
    end
end

local function requestTexture(path)
    local cached = textureCache[path]
    if cached then
        pendingHandle = nil
        pendingPath = nil
        previewManager.applyTexture(cached, path)
        return
    end
    readyState = "loading"
    pendingPath = path
    pendingHandle = engine.loadTexture(path)
    table.insert(loadedPaths, path)
end

-- Fit 'handle' (already-uploaded texture at 'path') into panelBounds
-- with nearest-neighbour scaling (forced in previewManager.init below —
-- NOT assumed from the default video config, which is only nearest
-- until a user's own persisted config/video.local.yaml picks "linear")
-- and aspect ratio preserved (Requirement 3).
function previewManager.applyTexture(handle, path)
    textureCache[path] = handle
    if not panelBounds then return end
    local size = engine.getTextureSize(handle)
    -- Shouldn't happen: onAssetLoaded only fires once the upload (and
    -- its textureSizeRef entry) is already complete. Defensive only.
    if not size or size.width <= 0 or size.height <= 0 then return end

    local fitScale = math.min(panelBounds.width / size.width,
                               panelBounds.height / size.height)
    local dw = size.width * fitScale
    local dh = size.height * fitScale
    local dx = panelBounds.x + (panelBounds.width - dw) / 2
    local dy = panelBounds.y + (panelBounds.height - dh) / 2

    if not spriteId then
        spriteId = UI.newSprite("preview_target_sprite", dw, dh,
            handle, 1.0, 1.0, 1.0, 1.0, page)
        UI.addToPage(page, spriteId, dx, dy)
    else
        UI.setSpriteTexture(spriteId, handle)
        UI.setSize(spriteId, dw, dh)
        UI.setPosition(spriteId, dx, dy)
        UI.setVisible(spriteId, true)
    end
    readyState = "ready"
end

local function onEntrySelected(path, _label, _index)
    requestTexture(path)
end

-- restoreSelectedPath/restoreScroll: nil for the initial build (selects
-- entry 1 by default, per assetBrowser.selectEntry); real values passed
-- by onFramebufferResize's rebuild so a resize never resets what the
-- user already picked (#886 round-1 review).
local function buildListUI(browseEntries, fbW, fbH, restoreSelectedPath, restoreScroll)
    mode = "list"
    entries = browseEntries or {}

    local listItems = {}
    for i, e in ipairs(entries) do
        listItems[i] = { label = e.label, path = e.path }
    end

    assetBrowser.init()
    browserId = assetBrowser.new({
        page = page,
        font = labelFont,
        x = 40, y = 40,
        width = math.max(200, fbW - 80),
        height = math.max(100, fbH - 80),
        entries = listItems,
        onSelect = onEntrySelected,
    })
    -- panelBounds MUST be current before selectEntry fires onSelect
    -- synchronously below — see assetBrowser.selectEntry's own doc.
    panelBounds = assetBrowser.getPanelBounds(browserId)

    if #listItems == 0 then
        readyState = "empty"
        return
    end
    assetBrowser.selectEntry(browserId, restoreSelectedPath)
    if restoreScroll and restoreScroll > 0 then
        assetBrowser.setScrollOffset(browserId, restoreScroll)
    end
end

-- Recompute the focused-item panel geometry and, if the texture already
-- resolved, immediately re-fit the existing sprite to it — the initial
-- build (requestTexture hasn't fired yet) and a resize rebuild (the
-- texture is already cached) both route through here.
local function refitFocusedPanel(fbW, fbH)
    panelBounds = {
        x = 40, y = 40,
        width = math.max(1, fbW - 80),
        height = math.max(1, fbH - 80),
    }
    if focusedEntry then
        local cached = textureCache[focusedEntry.path]
        if cached then
            previewManager.applyTexture(cached, focusedEntry.path)
        end
    end
end

local function buildFocusedUI(entry, fbW, fbH)
    mode = "item"
    focusedEntry = entry
    refitFocusedPanel(fbW, fbH)
    requestTexture(entry.path)
end

function previewManager.init(scriptId)
    -- Requirement 3: nearest-neighbour is REQUIRED for the browser, not
    -- just the default — a user's persisted config/video.local.yaml can
    -- set "linear" (defaultVideoConfig is only nearest until then), and
    -- the bindless sampler is shared engine-wide, so pin it explicitly
    -- for this preview session. Live-only (no engine.saveVideoConfig
    -- call) — never touches the user's saved setting.
    engine.setTextureFilter("nearest")
    labelFont = engine.loadFont("assets/fonts/arcade.ttf", FONT_SIZE)
end

-- Fonts load asynchronously (engine.loadFont just requests the load);
-- the page can only be built once the engine broadcasts it ready.
function previewManager.onAssetLoaded(assetType, handle, path)
    if assetType == "font" and handle == labelFont and not page then
        page = UI.newPage("preview_manager", "menu")

        local browse = engine.getPreviewBrowse()
        local fbW, fbH = engine.getFramebufferSize()
        if browse and browse.mode == "list" then
            buildListUI(browse.entries, fbW, fbH, nil, nil)
        elseif browse and browse.mode == "item" then
            buildFocusedUI(browse.entry, fbW, fbH)
        else
            -- Phase 1 (#632) placeholder: grouped category, or no
            -- browse state at all.
            mode = "placeholder"
            readyState = "ready"
            labelId = label.new({
                name     = "preview_target_label",
                text     = targetText(),
                font     = labelFont,
                fontSize = FONT_SIZE,
                color    = {1.0, 1.0, 1.0, 1.0},
                page     = page,
                x        = 40,
                y        = 40,
            })
        end

        UI.showPage(page)
    elseif assetType == "texture" and handle == pendingHandle then
        local resolvedPath = pendingPath
        pendingHandle = nil
        pendingPath = nil
        previewManager.applyTexture(handle, resolvedPath)
    end
end

function previewManager.update(dt)
end

function previewManager.shutdown()
    if labelId then
        label.destroy(labelId)
        labelId = nil
    end
    if browserId then
        assetBrowser.destroy(browserId)
        browserId = nil
    end
    if spriteId then
        UI.deleteElement(spriteId)
        spriteId = nil
    end
    if page then
        UI.deletePage(page)
        page = nil
    end
    mode = nil
    readyState = "loading"
    entries = nil
    focusedEntry = nil
    panelBounds = nil
    textureCache = {}
    loadedPaths = {}
    pendingHandle = nil
    pendingPath = nil
end

-----------------------------------------------------------
-- Input routing (list mode only — see scripts/ui_manager_scroll.lua
-- for the identical broadcast-callback pattern every other list-backed
-- screen in the normal boot path uses).
-----------------------------------------------------------

function previewManager.onListItemClick(elemHandle)
    if not browserId then return false end
    return assetBrowser.handleCallback("onListItemClick", elemHandle)
end

function previewManager.onScrollUp(elemHandle)
    if not browserId then return false end
    return assetBrowser.handleCallback("onScrollUp", elemHandle)
end

function previewManager.onScrollDown(elemHandle)
    if not browserId then return false end
    return assetBrowser.handleCallback("onScrollDown", elemHandle)
end

function previewManager.onUIScroll(elemHandle, dx, dy, _shiftHeld)
    if not browserId then return end
    assetBrowser.onScroll(elemHandle, dx, dy)
end

-- Preview windows are resizable (App.Preview reuses the normal window
-- config), so a bare-category list or a focused item must reflow on
-- resize instead of leaving stale bounds/sprite dimensions behind
-- (#886 round-1 review) — the SAME broadcast every other loaded script
-- receives (Engine.Scripting.Lua.Thread.Dispatch's
-- LuaFramebufferResize -> "onFramebufferResize").
function previewManager.onFramebufferResize(width, height)
    if not page then return end
    if mode == "list" then
        local prevPath = browserId and assetBrowser.getSelectedPath(browserId)
        local prevScroll = browserId and assetBrowser.getScrollOffset(browserId) or 0
        if browserId then
            assetBrowser.destroy(browserId)
            browserId = nil
        end
        buildListUI(entries, width, height, prevPath, prevScroll)
    elseif mode == "item" then
        refitFocusedPanel(width, height)
    end
    -- "placeholder" mode (Phase 1, #632): the label's fixed (40,40)
    -- position never overflows, so nothing to reflow.
end

-----------------------------------------------------------
-- Introspection (#886 Requirement 6): debug-console queryable via
-- `require("scripts.preview_manager").dump()` (see the self-
-- registration at the top of this file) — no pixels required to verify
-- discovery, selection, or scrolling.
-----------------------------------------------------------

function previewManager.dump()
    local out = {
        mode = mode,
        state = readyState,
        loadedPaths = loadedPaths,
    }
    if mode == "list" then
        -- The FULL discovered entry list, not just entryCount + the
        -- currently visible rows (#886 round-4 review) — a probe
        -- comparing only a count and the first/visible labels can't
        -- catch an omission or substitution further down the list.
        out.entries = entries
        out.entryCount = entries and #entries or 0
        out.selected = {
            label = assetBrowser.getSelectedLabel(browserId),
            path  = assetBrowser.getSelectedPath(browserId),
        }
        out.scrollOffset = assetBrowser.getScrollOffset(browserId)
        out.rows = assetBrowser.dump(browserId)
        out.panelBounds = panelBounds
    elseif mode == "item" then
        out.selected = { label = focusedEntry.label, path = focusedEntry.path }
        out.panelBounds = panelBounds
    end
    return out
end

return previewManager
