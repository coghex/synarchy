-- Preview mode entry point (#632 Phase 1, #886 Phase 2, #887 Phase 3,
-- #888 Phase 4 of the --preview texture browser epic #427).
--
-- Loaded by scripts/init.lua's game.init in place of the ~25 normal
-- gameplay/UI scripts whenever engine.getBootProfile() == "preview".
-- Phase 1 proved the boot skeleton: a font, one placeholder label, the
-- grey (#828382) clear color Engine.Graphics.Vulkan.Command.Record
-- already special-cases for the BootPreview profile. Phase 2 replaced
-- that with the real simple-category browser, and #888 deleted the
-- placeholder path entirely: app/Main.hs discovers and validates every
-- canonical target (Engine.Preview.Discovery / .Unit / .Building)
-- BEFORE this script ever runs, so engine.getPreviewBrowse() is the
-- single source of truth here and always answers with a real mode —
-- "list" backs a bare --preview <simple category> AND a
-- --preview flora/<name> / structures/<name> target (#888 routes those
-- flat, static-PNG item folders into the SAME browser rather than
-- forking a viewer per category), "item" backs a validated
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
--
-- #887 (Phase 3) adds a third real mode, "unit": engine.getPreviewBrowse()
-- reports mode == "unit" for a validated --preview units/<name>, and the
-- whole animation model (membership, ordering, per-direction frames,
-- mirroring, fps/loop, default selection) arrives already resolved from
-- Engine.Preview.Unit. The list holds animation names with frame-zero/
-- south thumbnails; scripts/ui/unit_animation_view.lua owns the enlarged
-- sprite, the direction row, and the shared playback clock.
--
-- #1260 makes a unit frame a TABLE ({ path, u0, v0, u1, v1, and
-- width/height for a compiled cell}) rather than a path, because an
-- atlas-backed animation's frames are all one image. Every sprite that
-- shows one — the row thumbnail included, via scripts.ui.list's iconUV
-- — must publish texture and sub-rect together, or it draws the whole
-- sheet. acquireTexture's per-path cache then dedups an entire
-- animation to ONE engine.loadTexture rather than one per frame.
--
-- #888 (Phase 4) adds the fourth, "building": one list mixing the
-- building folder's animation subdirectories and its loose static PNGs,
-- resolved by Engine.Preview.Building (including the
-- state_animations.built default selection, matched to a directory
-- through the frame paths its YAML animation declares, never by equal
-- names). scripts/ui/building_asset_view.lua owns the panel sprite and
-- the animation clock.
local assetBrowser = require("scripts.ui.asset_browser")
local list = require("scripts.ui.list")
local unitAnimationView = require("scripts.ui.unit_animation_view")
local buildingAssetView = require("scripts.ui.building_asset_view")

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

-- mode: "list" | "item" | "unit" | "building" (nil only if the engine
-- somehow reports no browse state at all — see onAssetLoaded)
local mode = nil
local readyState = "loading"  -- "loading" | "ready" | "empty"

-- Thumbnail edge (unscaled) for the units list's per-row frame-zero/
-- south icons — scripts/ui/list.lua applies the uiscale itself.
local UNIT_THUMB_SIZE = 24

-- Phase 2 simple-category browsing state.
local browserId = nil
local entries = nil
local focusedEntry = nil
local panelBounds = nil
local spriteId = nil

-- Phase 3 (#887) unit-animation state.
local unitData = nil    -- the resolved PreviewUnit table from the engine
local animViewId = nil
local selectedAnim = nil

-- Phase 4 (#888) building state.
local buildingData = nil   -- the resolved PreviewBuilding table
local buildingViewId = nil
local selectedEntry = nil

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

-- Unit/building mode's texture accessor: hand back a handle for 'path'
-- IMMEDIATELY (uploads are async, and both UI.newSprite and
-- UI.setSpriteTexture tolerate a not-yet-resolved handle — the same
-- thing scripts/ui/list.lua's own init() does with highlight.png), one
-- engine.loadTexture per distinct path. There is no dedup at the
-- engine.loadTexture layer, so this cache IS the dedup: without it a
-- 60 Hz playback tick would allocate a fresh handle every frame.
--
-- Unlike list/item mode's requestTexture below, this never touches
-- pendingHandle/readyState: unit and building modes have many textures
-- in flight at once and drive their own readiness off
-- engine.getTextureSize instead of a single-slot onAssetLoaded
-- handshake.
local viewHandles = {}

local function acquireTexture(path)
    if not path then return nil end
    local cached = textureCache[path]
    if cached then return cached end
    local handle = engine.loadTexture(path)
    textureCache[path] = handle
    table.insert(loadedPaths, path)
    return handle
end

-- The handles the CURRENT panel view is actually showing, and nothing
-- else (#1690). Reset on every genuine selection and repopulated by the
-- view itself, which re-requests each frame it applies — so it always
-- describes the live selection rather than everything the session has
-- ever loaded. That distinction is what stops a failure queued for an
-- abandoned selection from blanking the preview the user is looking at
-- now; textureCache is session-wide and deliberately never says this.
--
-- List-row THUMBNAILS go through bare acquireTexture instead, so a
-- failed thumbnail leaves the panel alone: the list waits on no
-- handshake and simply draws that row without an icon.
local function requestViewTexture(path)
    local handle = acquireTexture(path)
    if handle then viewHandles[handle] = true end
    return handle
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
    -- List/item mode's panel shows exactly ONE texture, so this handle
    -- REPLACES whatever the view was showing rather than joining it
    -- (#1690). Selecting a second entry therefore stops the first one's
    -- in-flight request from being able to settle this view.
    viewHandles = { [handle] = true }
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

-- restoreSelectedPath/restoreScroll: nil for the initial build (a
-- genuinely fresh selection — entry 1 by default, firing onSelect);
-- real values passed by onFramebufferResize's rebuild, RESTORED
-- silently (#886 round-6 review) — re-selecting through onSelect there
-- would treat a mere geometry change as if the user had clicked
-- something new, and would issue a DUPLICATE engine.loadTexture request
-- if the original load was still pending (the repo's established
-- resize-restore contract: never re-fire onSelect/onChange, e.g.
-- list.setSelectedIndex elsewhere in this codebase).
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
    -- panelBounds MUST be current before selection below — a real
    -- selectEntry's onSelect fires synchronously and needs it; a silent
    -- restore's own explicit re-fit (below) needs it too.
    panelBounds = assetBrowser.getPanelBounds(browserId)

    if #listItems == 0 then
        readyState = "empty"
        return
    end

    if restoreSelectedPath then
        assetBrowser.selectEntrySilently(browserId, restoreSelectedPath)
        -- If the restored selection's texture already resolved, refit
        -- the cached sprite to the NEW panel bounds directly (onSelect
        -- never fired, so nothing else will). If it's still pending,
        -- the eventual onAssetLoaded completion already reads the
        -- panelBounds just set above — nothing else to do here.
        local cached = textureCache[restoreSelectedPath]
        if cached then
            previewManager.applyTexture(cached, restoreSelectedPath)
        end
    else
        assetBrowser.selectEntry(browserId, nil)
    end
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

-----------------------------------------------------------
-- Unit animation viewer (#887, Phase 3)
-----------------------------------------------------------

local function findAnim(name)
    for _, a in ipairs(unitData and unitData.animations or {}) do
        if a.name == name then return a end
    end
    return nil
end

-- A genuine animation selection: always starts the clip fresh from the
-- current wall clock. The resize path deliberately does NOT come
-- through here (it must preserve the playback phase) — see buildUnitUI.
local function onAnimSelected(value, _label, _index)
    local anim = findAnim(value)
    if not anim or not animViewId then return end
    selectedAnim = value
    -- The previous selection's handles stop being this view's concern
    -- the moment a new one is chosen (#1690).
    viewHandles = {}
    -- "loading" until the frames actually upload; previewManager.update
    -- promotes it, so `state` means the same thing here as in list/item
    -- mode rather than reporting ready before anything is on screen.
    readyState = "loading"
    unitAnimationView.setAnimation(animViewId, anim, engine.realTime(), nil)
end

-- restoreAnim/restoreScroll/restoreDirection: nil for the initial build
-- (a fresh default selection). Real values on the resize rebuild, where
-- the #887 amendment requires the selected animation, selected
-- direction, list scroll offset, AND playback phase to all survive — so
-- the restore path silently re-selects the list row and only re-panels
-- the view, never re-entering setAnimation (which would reset the clock).
local function buildUnitUI(unit, fbW, fbH, restoreAnim, restoreScroll, restoreDirection)
    mode = "unit"
    unitData = unit

    assetBrowser.init()

    local listItems = {}
    for i, a in ipairs(unit.animations or {}) do
        listItems[i] = {
            label = a.name,
            path = a.name,
            -- Requirement 1: a frame-zero/south thumbnail per row. No
            -- thumb (an animation with no south frames at all) simply
            -- leaves that row's icon hidden. For an atlas-backed
            -- animation the thumbnail is one CELL of the compiled
            -- sheet, so the row icon needs its sub-rect too — without
            -- iconUV the list would draw the whole atlas in a 24px box.
            icon = a.thumb and acquireTexture(a.thumb.path) or nil,
            iconUV = a.thumb and { a.thumb.u0, a.thumb.v0,
                                   a.thumb.u1, a.thumb.v1 } or nil,
        }
    end

    browserId = assetBrowser.new({
        page = page,
        font = labelFont,
        x = 40, y = 40,
        width = math.max(200, fbW - 80),
        height = math.max(100, fbH - 80),
        entries = listItems,
        iconSize = UNIT_THUMB_SIZE,
        onSelect = onAnimSelected,
    })
    panelBounds = assetBrowser.getPanelBounds(browserId)

    if #listItems == 0 then
        readyState = "empty"
        return
    end

    if not animViewId then
        animViewId = unitAnimationView.new({
            page = page,
            font = labelFont,
            panel = panelBounds,
            requestTexture = requestViewTexture,
            chromeTexture = list.getChromeTexture(),
        })
    else
        unitAnimationView.setPanel(animViewId, panelBounds)
    end

    if restoreAnim then
        assetBrowser.selectEntrySilently(browserId, restoreAnim)
        selectedAnim = restoreAnim
        -- Phase-preserving: only the panel geometry changed.
        unitAnimationView.setPanel(animViewId, panelBounds)
        if restoreDirection then
            unitAnimationView.setDirection(animViewId, restoreDirection)
        end
    else
        -- Requirement 2: idle (or the first animation), direction south
        -- — both already decided by Engine.Preview.Unit / the view's own
        -- resolveDirection, never re-derived here.
        assetBrowser.selectEntry(browserId, unit.defaultAnim)
    end

    if restoreScroll and restoreScroll > 0 then
        assetBrowser.setScrollOffset(browserId, restoreScroll)
    end
end

-----------------------------------------------------------
-- Building asset viewer (#888, Phase 4)
-----------------------------------------------------------

local function findBuildingEntry(entryLabel)
    for _, e in ipairs(buildingData and buildingData.entries or {}) do
        if e.label == entryLabel then return e end
    end
    return nil
end

-- A genuine entry selection: always starts the clip fresh from the
-- current wall clock (a static entry simply never advances from frame
-- zero). The resize path deliberately does NOT come through here — it
-- must preserve the playback phase, see buildBuildingUI.
local function onBuildingEntrySelected(value, _label, _index)
    local entry = findBuildingEntry(value)
    if not entry or not buildingViewId then return end
    selectedEntry = value
    -- As above: a new selection owns a new handle set (#1690).
    viewHandles = {}
    -- "loading" until the frames actually upload; previewManager.update
    -- promotes it, so `state` means the same thing here as in every
    -- other mode.
    readyState = "loading"
    buildingAssetView.setEntry(buildingViewId, entry, engine.realTime())
end

-- restoreEntry/restoreScroll: nil for the initial build (a fresh
-- default selection). Real values on the resize rebuild, where the
-- selected entry, list scroll offset, AND playback phase must all
-- survive — so the restore path silently re-selects the list row and
-- only re-panels the view, never re-entering setEntry (which would
-- reset the clock).
local function buildBuildingUI(building, fbW, fbH, restoreEntry, restoreScroll)
    mode = "building"
    buildingData = building

    assetBrowser.init()

    local listItems = {}
    for i, e in ipairs(building.entries or {}) do
        -- The label IS the value: entry labels are unique within a
        -- building folder (they're distinct relative paths), and
        -- Engine.Preview.Building already ordered them.
        listItems[i] = { label = e.label, path = e.label }
    end

    browserId = assetBrowser.new({
        page = page,
        font = labelFont,
        x = 40, y = 40,
        width = math.max(200, fbW - 80),
        height = math.max(100, fbH - 80),
        entries = listItems,
        onSelect = onBuildingEntrySelected,
    })
    panelBounds = assetBrowser.getPanelBounds(browserId)

    if #listItems == 0 then
        readyState = "empty"
        return
    end

    if not buildingViewId then
        buildingViewId = buildingAssetView.new({
            page = page,
            panel = panelBounds,
            requestTexture = requestViewTexture,
        })
    else
        buildingAssetView.setPanel(buildingViewId, panelBounds)
    end

    if restoreEntry then
        assetBrowser.selectEntrySilently(browserId, restoreEntry)
        selectedEntry = restoreEntry
        -- Phase-preserving: only the panel geometry changed.
        buildingAssetView.setPanel(buildingViewId, panelBounds)
    else
        -- Requirement 1: state_animations.built, else sprite, else
        -- default.png, else the first entry — already decided by
        -- Engine.Preview.Building, never re-derived here.
        assetBrowser.selectEntry(browserId, building.defaultEntry)
    end

    if restoreScroll and restoreScroll > 0 then
        assetBrowser.setScrollOffset(browserId, restoreScroll)
    end
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
        elseif browse and browse.mode == "unit" then
            buildUnitUI(browse.unit, fbW, fbH, nil, nil, nil)
        elseif browse and browse.mode == "building" then
            buildBuildingUI(browse.building, fbW, fbH, nil, nil)
        else
            -- #888 Requirement 4: the Phase 1 (#632) placeholder-label
            -- boot is GONE. Every canonical --preview target now
            -- resolves to a real browse mode before this script ever
            -- runs, so a nil browse state can only mean a boot outside
            -- BootPreview — which never loads this script at all.
            readyState = "empty"
        end

        UI.showPage(page)
    elseif assetType == "texture" and handle == pendingHandle then
        local resolvedPath = pendingPath
        pendingHandle = nil
        pendingPath = nil
        previewManager.applyTexture(handle, resolvedPath)
    end
end

-- #1690: a texture request that TERMINALLY FAILED, because the bindless
-- registration refused it. Before #1690 this arrived as onAssetLoaded
-- and the viewer showed the undefined texture believing it had the real
-- one; now it arrives here and the session has to SETTLE on it, because
-- both of this module's readiness mechanisms would otherwise wait
-- forever: list/item mode holds a single pendingHandle waiting for its
-- onAssetLoaded, and unit/building mode polls engine.getTextureSize,
-- which a failed request never populates.
--
-- Only a failure the CURRENT view is waiting on settles it. A texture
-- request outlives the selection that made it, so a failure can arrive
-- after the user has moved to a different, perfectly resident asset —
-- and because "empty" is terminal by design, blanking the preview on
-- that stale news would be unrecoverable. So the test is the live
-- pendingHandle or the live viewHandles set, never the session-wide
-- textureCache, which says only that some earlier selection asked for
-- this path.
--
-- "empty" rather than a new state value: it is the existing terminal
-- state for "there is nothing to show here", it is what previewManager.
-- dump() already reports to a probe, and update() deliberately never
-- overwrites it — so this settles once and stays settled.
--
-- `reported` (#1842): the engine already logged a better-contextualised
-- line for this asset, so skip the duplicate one -- never the eviction
-- and state work below, which is what makes a later selection reissue
-- the request instead of reusing a dead handle.
function previewManager.onAssetFailed(assetType, handle, path, reason, reported)
    if assetType ~= "texture" then return end
    local isPending = (pendingHandle ~= nil) and handle == pendingHandle
    local isInView = viewHandles[handle] == true
    local wasCached = textureCache[path] == handle
    if not (isPending or isInView or wasCached) then return end

    if not reported then
        engine.logWarn("Preview texture failed to load: " .. tostring(path)
            .. " (" .. tostring(reason) .. ")")
    end

    -- The handle is dead whoever was waiting on it. Drop it from both
    -- records so a later selection of this path issues a fresh request
    -- rather than reusing a handle that resolves to the undefined
    -- texture. Guarded on identity: a newer request for the same path
    -- must not be evicted by an older failure.
    if wasCached then textureCache[path] = nil end
    viewHandles[handle] = nil
    if isPending then
        pendingHandle = nil
        pendingPath = nil
    end

    -- ...but only a CURRENT waiter settles the view.
    if isPending or isInView then
        readyState = "empty"
    end
end

-- Playback is driven off a WALL clock (engine.realTime), not an
-- accumulated dt: the tick rate only controls smoothness, never which
-- frame is correct, so a slow or bursty tick can't desynchronize the
-- direction row from the enlarged sprite.
function previewManager.update(dt)
    -- Unit/building readiness signal: the panel sprite has a real,
    -- uploaded texture fitted to the panel. Same meaning "ready"
    -- carries in list/item mode, so poll_state works uniformly across
    -- every mode.
    local view = nil
    if animViewId then
        unitAnimationView.update(animViewId, engine.realTime())
        view = unitAnimationView.dump(animViewId)
    elseif buildingViewId then
        buildingAssetView.update(buildingViewId, engine.realTime())
        view = buildingAssetView.dump(buildingViewId)
    else
        return
    end
    if readyState ~= "empty" then
        readyState = (view and view.ready) and "ready" or "loading"
    end
end

function previewManager.shutdown()
    if browserId then
        assetBrowser.destroy(browserId)
        browserId = nil
    end
    if animViewId then
        unitAnimationView.destroy(animViewId)
        animViewId = nil
    end
    if buildingViewId then
        buildingAssetView.destroy(buildingViewId)
        buildingViewId = nil
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
    unitData = nil
    selectedAnim = nil
    buildingData = nil
    selectedEntry = nil
    textureCache = {}
    viewHandles = {}
    loadedPaths = {}
    pendingHandle = nil
    pendingPath = nil
end

-----------------------------------------------------------
-- Input routing (every list-backed mode — see
-- scripts/ui_manager_scroll.lua for the identical broadcast-callback
-- pattern every other list-backed screen in the normal boot path uses).
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

-- Requirement 3: clicking a direction cell enlarges that direction.
-- Routed the same broadcast way as onListItemClick above — the engine
-- calls the callback name UI.setOnClick registered on the cell's hit box.
function previewManager.onPreviewDirectionClick(elemHandle)
    if not animViewId then return false end
    return unitAnimationView.handleCellClick(animViewId, elemHandle) ~= nil
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
    elseif mode == "unit" then
        -- #887 amendment: selected animation, selected direction, list
        -- scroll offset, AND playback phase all survive a reflow. The
        -- list is rebuilt (its row geometry depends on the new size);
        -- the animation view is only re-panelled, never re-selected.
        local prevScroll = browserId and assetBrowser.getScrollOffset(browserId) or 0
        local prevDump = animViewId and unitAnimationView.dump(animViewId)
        if browserId then
            assetBrowser.destroy(browserId)
            browserId = nil
        end
        buildUnitUI(unitData, width, height, selectedAnim, prevScroll,
                    prevDump and prevDump.direction or nil)
    elseif mode == "building" then
        -- #888 amendment: the selected entry, list scroll offset, AND
        -- playback phase all survive a reflow — same shape as unit
        -- mode above (list rebuilt, view only re-panelled).
        local prevScroll = browserId and assetBrowser.getScrollOffset(browserId) or 0
        if browserId then
            assetBrowser.destroy(browserId)
            browserId = nil
        end
        buildBuildingUI(buildingData, width, height, selectedEntry, prevScroll)
    end
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
    elseif mode == "unit" then
        -- #887 Requirement 8 + its amendment: the animation-entry list,
        -- aggregate playback state, AND per-direction entries carrying
        -- their own mirrored flag, source direction, frame index, and
        -- interactive bounds/handle — enough for a probe to locate and
        -- click a real direction cell without a hardcoded coordinate.
        out.unit = unitData and unitData.name or nil
        out.entries = {}
        for i, a in ipairs(unitData and unitData.animations or {}) do
            out.entries[i] = {
                label = a.name,
                name = a.name,
                fps = a.fps,
                loop = a.loop,
                flip = a.flip,
                thumb = a.thumb and a.thumb.path or nil,
                -- Per-animation storage mode (#1260). Since #1261 every
                -- unit animation is atlas-backed, so this is DERIVED
                -- from the atlas the engine actually pushed rather than
                -- asserted: a missing one would report "legacy" and
                -- fail the probe rather than pass silently. The list is
                -- reported so a probe can prove EVERY animation of a
                -- migrated unit selected the atlas, not just the one
                -- currently playing.
                storage = a.atlas and "atlas" or "legacy",
                atlas = a.atlas,
                directionCount = #(a.directions or {}),
            }
        end
        out.entryCount = #out.entries
        out.defaultAnim = unitData and unitData.defaultAnim or nil
        out.selected = {
            label = assetBrowser.getSelectedLabel(browserId),
            path  = assetBrowser.getSelectedPath(browserId),
        }
        out.scrollOffset = assetBrowser.getScrollOffset(browserId)
        out.rows = assetBrowser.dump(browserId)
        out.panelBounds = panelBounds
        out.playback = animViewId and unitAnimationView.dump(animViewId) or nil
    elseif mode == "building" then
        -- #888 Requirement 4 + its amendment: the FULL ordered entry
        -- list with each entry's static/animation identity and
        -- effective fps/loop, the current selection, per-visible-row
        -- interactive bounds/handles, the scroll offset, and — for an
        -- ANIMATION selection only — the live playback state. A static
        -- selection exposes no playback at all, which is exactly what
        -- distinguishes it.
        out.building = buildingData and buildingData.name or nil
        out.entries = {}
        for i, e in ipairs(buildingData and buildingData.entries or {}) do
            out.entries[i] = {
                label = e.label,
                kind = (e.animated == true) and "animation" or "static",
                animated = e.animated == true,
                fps = e.fps,
                loop = e.loop == true,
                frameCount = #(e.frames or {}),
            }
        end
        out.entryCount = #out.entries
        out.defaultEntry = buildingData and buildingData.defaultEntry or nil
        out.selected = {
            label = assetBrowser.getSelectedLabel(browserId),
            path  = assetBrowser.getSelectedPath(browserId),
        }
        out.scrollOffset = assetBrowser.getScrollOffset(browserId)
        out.rows = assetBrowser.dump(browserId)
        out.panelBounds = panelBounds
        local view = buildingViewId and buildingAssetView.dump(buildingViewId)
        if view and view.animated then
            out.playback = view
        end
    end
    return out
end

return previewManager
