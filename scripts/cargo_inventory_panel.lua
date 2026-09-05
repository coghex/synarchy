-- Container Window (historically the cargo inventory panel)
--
-- Since #1238 this module is THE container-window manager, and what it
-- owns is an ordered STACK of LEVELS rather than one popup. Level 1 is
-- an endpoint (a storage building or a unit); opening a container that
-- lives INSIDE an open level pushes a deeper level, and the nesting
-- path is remembered so a resize can rebuild the whole thing. D-9's
-- one-window rule is per LEVEL, not per screen.
--
-- Since #2155 this file is the STACK-LIFECYCLE owner and the FAÇADE,
-- and two focused owners sit behind it:
--
--   scripts/cargo_inventory_endpoints.lua — everything that knows an
--       endpoint is a building or a unit (#1234) and everything that
--       knows contents can be REMEMBERED (#1237): the ENDPOINTS table,
--       the knowledge/age/weight/empty presentation, the five shared
--       endpoint helpers, and the `endpoint` level-kind descriptor.
--   scripts/cargo_inventory_render.lua — the generic pane: layout
--       constants, header baselines and labels, item-list parameter
--       completion, measurement, placement, element teardown, the row
--       context menu, and scroll capture. Level-kind agnostic.
--
-- Dependency direction is one-way and acyclic: this file imports both;
-- the renderer imports the endpoint owner for the single-owned
-- `ageText` it must measure against; NEITHER extracted module imports
-- this one, and endpoint policy never imports the renderer — the tab
-- spec and row-name colour reach it as VALUES injected below. This
-- file remains the ONLY engine-loaded script of the three (see
-- scripts/init_loader.lua and scripts/ui/view_teardown.lua); the other
-- two are `require`-only and define no `on*`-named function.
--
-- What this file still owns, and owns alone: the ordered `levels`
-- state, base-versus-nested targeting, replacement at one level and
-- removal of every deeper level, modal-page creation and deletion,
-- deepest-level input ownership, one-level-per-Escape dismissal,
-- teardown reasons and `onClose` dispatch, resize snapshot and
-- restoration, per-tick liveness/staleness/close decisions, the engine
-- lifecycle hooks, and the public introspection surface.
--
-- Only the DEEPEST level is interactive. That is not enforced by
-- bookkeeping here: a level past the base gets its own `LayerModal`
-- page, whose #742 default input-exclusivity makes it the modal
-- boundary, so every shallower level stays painted and unclickable
-- automatically, and closing it restores its parent. The base level
-- keeps its pre-#1238 non-modal behaviour on hud.world_page, which is
-- why opening a container never freezes the game until you actually
-- descend into one. `scripts/popup.lua:796-801` is the inverse
-- precedent — notification cards opt OUT because they are
-- stacking-only; container levels want the default.
--
-- Two windows never coexist at one level: opening container B where A
-- is open REPLACES A and everything below it (signed off 2026-08-11 —
-- replace, not a refused gesture). An external request (a right-click
-- on a cargo, the unit-info "Contents" entry) targets the BASE level; a
-- container ROW inside level N opens at level N+1.
--
-- Where a level's data comes from is the LEVEL KIND's business
-- (LEVELS below), and there are four:
--
--   endpoint      — a storage building or a unit, the pre-#1238 window.
--   unitItem      — an item-container a unit carries, wears or has
--                   equipped: LIVE contents via unit.getItemContents.
--   buildingItem  — an item-container inside a storage building:
--                   the REMEMBERED contents recorded by the last
--                   reveal, carrying the parent record's own
--                   "as of…" age. Never a live read, never a write.
--   escort        — a Mode A transfer session (#1250): D-9's stated
--                   exception, ONE level owning TWO flanking panes,
--                   one per endpoint of the session.
--
-- `endpoint` is scripts/cargo_inventory_endpoints.lua's since #2155,
-- unitItem/buildingItem are `scripts/item_contents_panel.lua`'s (D-13)
-- and escort is `scripts/transfer_session_panels.lua`'s: none of those
-- modules owns a window lifecycle at all — each supplies its level's
-- data and presentation, and this file drives the shared renderer so
-- every level wears the same chrome.
--
-- A level owns one or more PANES (#1250). A pane is the unit of
-- RENDERING: one panel box, its header labels, and one item list, with
-- its own selected tab and scroll offset. Every level kind but `escort`
-- has exactly one, and for those the level table IS its own first pane
-- — `level.panes[1] == level` — so `level.listId` / `level.activeTab` /
-- `level.scroll` keep meaning exactly what they meant before the escort
-- session existed, with no mirrored copy that could drift out of sync.
-- A level is still the unit of NESTING, modality, teardown and restore,
-- which is what makes two flanking panels count as one stack level.
--
-- An item-container level is RENDER-ONLY (D-5): it is not a transfer
-- endpoint and offers no transfer operation. Inspection — scrolling,
-- closing, opening a child container — stays interactive on the
-- deepest level, which is what makes the stack usable at all.
--
-- Since #1250 the ESCORT level's two panes commit IMMEDIATELY instead
-- (Mode A): the source unit has already walked to the endpoint and is
-- held there, so its rows offer "Store 1" / "Store all" and the
-- endpoint's offer "Retrieve 1" / "Retrieve all", each moving items on
-- the spot through the contract's own check/commit pair. Those entries
-- are scripts/transfer_gestures.lua's too — the SAME 1-and-all builder,
-- given a different way to submit — so the two modes cannot disagree
-- about which exact instances a merged row stands for.
--
-- Since #1249 an ENDPOINT level's rows offer "Retrieve 1" / "Retrieve
-- all", which queues a durable transfer order (#1246) for the unit the
-- shared selection rule resolves and lets #1247's job walk it there.
-- That REPLACED "Withdraw with <unit>" and its disabled "select an
-- adjacent unit first" placeholder, so no player path in this window
-- calls `unit.withdrawFromCargo` any more and none requires adjacency.
-- The entries themselves are scripts/transfer_gestures.lua's, shared
-- with the unit-info panel's "Store" so the two directions cannot
-- drift; the endpoint owner supplies only which endpoint the level is
-- showing. An item-container level supplies no transfer action at all,
-- so the render-only rule above holds by construction rather than by a
-- test against the level kind.
--
-- Earlier contracts this window still holds:
--
-- Since #1088 the tabbed list is the shared item-list widget
-- (scripts/ui/item_list.lua) — this window owns the popup, its
-- title/subtitle chrome, the data source, and the presentation policy
-- it hands the widget. Grouping, tabs, rows, truncation, scrolling and
-- rebuild invalidation live in the widget.
--
-- Since #1268 a row also presents its group's tracked temperature
-- (#344), in the row text and in a row tooltip — both derived from the
-- widget's own summary, so this window and the unit inventory can
-- never word it differently.
--
-- Since #1234 the endpoint level is ENDPOINT-KIND AGNOSTIC: everything
-- that differs between a cargo and an acolyte lives in the ENDPOINTS
-- table (scripts/cargo_inventory_endpoints.lua since #2155) — one live
-- read, one weight label, one descent rule. An unknown kind is
-- REJECTED rather than assumed.
--
-- Since #1237 an endpoint's contents are either LIVE TRUTH or the
-- player's REMEMBERED view, and the endpoint kind decides which. A
-- remembering source says so by returning a `knowledge` sub-table from
-- its view, and every presentation helper branches on THAT rather than
-- on the kind — which is exactly why a building-side item level
-- (#1238) gets its "as of…" line for free.
--
-- Pinned at the mouse position when opened; doesn't follow the
-- endpoint if the camera moves. Esc closes the DEEPEST level, one per
-- press. The mouse WHEEL scrolls the deepest level's list — the window
-- captures it (#743's `UI.setScrollCapture`), so a wheel notch over an
-- open container window no longer reaches the camera zoom behind it,
-- the same way every other scrollable panel here already behaves.
--
-- Public API — an endpoint identity is a KIND plus an id, never a bare
-- building id:
--   openFor(kind, id, mx, my)            — open the BASE level on this
--                                          endpoint at framebuffer
--                                          pixel (mx, my); false if it
--                                          isn't one
--   reopenWithTab(kind, id, mx, my, tab) — base-level open + tab
--   openLevel(src, mx, my, parentIndex)  — open `src` at parentIndex+1
--   closeIfOpen(reason)                  — destroy the WHOLE stack
--   popLevel()                           — close the deepest level
--   refreshLevel(level)                  — rebuild one level now
--   isOpen() / depth() / getLevel(i)     — introspection
--   getPane(level, paneKey)              — one pane of a level
--   paneWidgetName(paneKey)              — the element-name rule
--   snapshotStack() / restoreStack(s)    — the resize-restore pair
--   onPaneTabChange(level, pane, cat)    — a pane's tab selection
--   onScroll(handle, dx, dy)             — wheel over the window
--   dump()                               — probe oracle
--
-- Shared with the escort level kind, so the pair renders through
-- exactly the endpoint machinery a lone container window uses. Since
-- #2155 these are DELEGATORS onto
-- scripts/cargo_inventory_endpoints.lua, unchanged in signature and
-- still the entry point every caller outside this file uses:
--   endpointView(kind, id) / endpointListParams(kind, view)
--   endpointStillThere(kind, id) / endpointChildOf(kind, id, row)
--   endpointTabSpec() / formatAge(elapsed)
--
-- Engine script hooks: setup / init / update / shutdown.
--
-- Module is registered in package.loaded so init.lua's right-click
-- handler and ui_manager's click dispatchers all see the same
-- instance even though engine.loadScript uses dofile.

local cargoInventoryPanel =
    package.loaded["scripts.cargo_inventory_panel"] or {}
package.loaded["scripts.cargo_inventory_panel"] = cargoInventoryPanel

local panel     = require("scripts.ui.panel")
local label     = require("scripts.ui.label")
local itemList  = require("scripts.ui.item_list")
local endpoints = require("scripts.cargo_inventory_endpoints")
local render    = require("scripts.cargo_inventory_render")

-- The two style values endpoint policy needs but may not import
-- (#2155): the renderer single-owns the constants, this file composes
-- them and hands them down. Injected BEFORE the endpoint level kind is
-- built below, so its `tabs` descriptor picks them up.
endpoints.setStyle({
    tabSpec      = render.tabSpec(),
    rowNameColor = render.rowNameColor(),
})

-----------------------------------------------------------
-- State
--
-- `levels` is the whole truth (#1238): an ordered array, [1] the base.
-- Each entry is
--   { src, index, mx, my, pageId, panes,
--     paneKey, activeTab, scroll, panelId, titleId, subtitleId,
--     ageId, listId }
-- where `src` is the level kind's own identity table — the thing
-- hud.lua snapshots across a resize, and the thing a child level
-- extends with one more instance id — and the second group is the
-- LEVEL'S OWN FIRST PANE (#1250; `panes[1] == level`, see the header).
-- A second pane is a plain table carrying only that same second group.
-----------------------------------------------------------
cargoInventoryPanel.state = cargoInventoryPanel.state or { levels = {} }
cargoInventoryPanel.state.levels = cargoInventoryPanel.state.levels or {}

cargoInventoryPanel.hud = nil   -- assets set by setup()

-----------------------------------------------------------
-- HUD hookup
-----------------------------------------------------------
function cargoInventoryPanel.setup(opts)
    cargoInventoryPanel.hud = opts
end

-----------------------------------------------------------
-- Endpoint delegators (#2155)
--
-- The endpoint owner's five shared helpers plus `formatAge`, re-exported
-- with unchanged signatures. scripts/transfer_session_panels.lua reaches
-- all five through `manager()`, and every probe and gate addresses them
-- here, so these stay the public entry points whatever module holds the
-- implementation.
-----------------------------------------------------------
function cargoInventoryPanel.endpointView(endpointKind, id)
    return endpoints.endpointView(endpointKind, id)
end

function cargoInventoryPanel.endpointStillThere(endpointKind, id)
    return endpoints.endpointStillThere(endpointKind, id)
end

function cargoInventoryPanel.endpointChildOf(endpointKind, id, row)
    return endpoints.endpointChildOf(endpointKind, id, row)
end

function cargoInventoryPanel.endpointTabSpec()
    return endpoints.endpointTabSpec()
end

function cargoInventoryPanel.endpointListParams(endpointKind, view)
    return endpoints.endpointListParams(endpointKind, view)
end

cargoInventoryPanel.formatAge = endpoints.formatAge

-----------------------------------------------------------
-- Level kinds (#1238)
--
-- What a level kind declares:
--
--   panelWidthBase / maxRows / tabs   window width, row cap, category
--                                     tabs
--   view(src, paneKey)                that pane's data; nil closes the
--                                     level
--   listParams(src, view, paneKey)    the item-list presentation policy
--   stillThere(src)                   the per-tick LIFECYCLE question,
--                                     asked only by update()
--   transferMenu(src, row, paneKey)   the row's transfer action
--   childOf(src, row, paneKey)        the level a container row opens
--   paneKeys                          #1250: more than one pane. Absent
--                                     means exactly one, addressed as
--                                     MAIN_PANE
--   paneScale(level, paneKey)         #1250: a LOCAL effective uiscale
--                                     for this kind's panes. Absent
--                                     means the configured one
--   placePanes(level, measures, hud)  #1250: where each pane's panel
--                                     goes. Absent means the shared
--                                     anchor-and-clamp rule
--   onClose(src, reason)              #1250: state that outlives the
--                                     level's own elements. Never fires
--                                     for reason == "layout"
--
-- Everything else — panel sizing, header baselines, the "as of…" line,
-- scrolling — is scripts/cargo_inventory_render.lua's and level-kind
-- blind; modality, teardown and restore are this file's.
--
-- The endpoint kind lives in scripts/cargo_inventory_endpoints.lua
-- (#2155), the two item kinds in scripts/item_contents_panel.lua (D-13)
-- and the escort kind in scripts/transfer_session_panels.lua; the
-- latter two are required lazily so the modules can reference each
-- other without a load-order cycle.
-----------------------------------------------------------
local function itemLevels()
    return require("scripts.item_contents_panel").levelKinds()
end

local function escortLevels()
    return require("scripts.transfer_session_panels").levelKinds()
end

local LEVELS = { endpoint = endpoints.levelKind() }

local function levelKind(level)
    if level.src.kind == "endpoint" then return LEVELS.endpoint end
    local k = itemLevels()[level.src.kind]
    if k then return k end
    return escortLevels()[level.src.kind]
end

-- The pane keys of a level kind, in render order. A kind that declares
-- none has exactly one pane, and `MAIN_PANE` is the key it is addressed
-- by — a key rather than nil so every per-pane hook has the same arity
-- whichever kind supplies it.
local MAIN_PANE = "main"
local DEFAULT_PANE_KEYS = { MAIN_PANE }

local function paneKeysOf(kind)
    return (kind and kind.paneKeys) or DEFAULT_PANE_KEYS
end

-----------------------------------------------------------
-- Level lifecycle
-----------------------------------------------------------

local function levels()
    return cargoInventoryPanel.state.levels
end

-- Every pane of `level`, in render order. `panes[1] == level` (see the
-- header): a level IS its own first pane, so this is never empty and a
-- single-pane level needs no allocation at all.
local function panesOf(level)
    return level.panes or { level }
end

-- The page a level's elements live on. The base level shares
-- hud.world_page (non-modal, pre-#1238 behaviour); a deeper level owns
-- a `LayerModal` page whose #742 default exclusivity is the whole
-- mechanism behind "only the deepest level is interactive".
local function levelPage(level)
    return level.pageId or (cargoInventoryPanel.hud
                            and cargoInventoryPanel.hud.page)
end

-----------------------------------------------------------
-- The renderer's controller (#2155)
--
-- The complete set of things scripts/cargo_inventory_render.lua may ask
-- this file for. Deliberately narrow and deliberately one-way: the
-- renderer never resolves a level kind, never reads the stack array
-- except as an argument, and never imports this module. Every entry is
-- late-bound through the façade table, so the two `cargoInventoryPanel`
-- methods below may be defined after this table is built.
-----------------------------------------------------------
local CONTROL = {
    mainPaneKey = MAIN_PANE,
    hud     = function() return cargoInventoryPanel.hud end,
    page    = function(level) return levelPage(level) end,
    panesOf = function(level) return panesOf(level) end,
    paneWidgetName = function(pane)
        return cargoInventoryPanel.paneWidgetName(pane.paneKey or MAIN_PANE)
    end,
    onTabChange = function(level, pane, category)
        return cargoInventoryPanel.onPaneTabChange(level, pane, category)
    end,
    openLevel = function(src, mx, my, parentIndex)
        return cargoInventoryPanel.openLevel(src, mx, my, parentIndex)
    end,
}

local function applyScrollCapture()
    render.applyScrollCapture(CONTROL, levels())
end

-- Build (or rebuild) every pane of one level against `views`, one per
-- pane in pane order.
local function buildLevel(level, views)
    render.buildLevel(CONTROL, levelKind(level), level, views, levels())
end

-- Widgets first, page second: UI.deletePage destroys the element tree
-- outright, so a page deleted before its widgets are torn down would
-- leave panel/label/item_list registry entries pointing at handles the
-- manager has already forgotten.
--
-- `reason` distinguishes a GEOMETRY teardown from a real close (#1250).
-- A level kind may own state that outlives its own elements — the
-- escort session does — and a resize destroys and rebuilds every level
-- precisely so that state survives. So the kind's `onClose` hook fires
-- for every reason EXCEPT "layout", which is what hud.createUI's
-- snapshot/restore pass passes.
local function destroyLevel(level, reason)
    render.destroyLevelElements(panesOf(level))
    if level.pageId then
        UI.deletePage(level.pageId)
        level.pageId = nil
    end
    local kind = levelKind(level)
    if reason ~= "layout" and kind and kind.onClose then
        kind.onClose(level.src, reason)
    end
end

-- Close every level from `index` down (deepest first, so the modal
-- boundary lifts in the same order it was established).
local function closeLevelsFrom(index, reason)
    local ls = levels()
    for i = #ls, math.max(1, index), -1 do
        destroyLevel(ls[i], reason)
        ls[i] = nil
    end
end

-- Every pane's view, or nil when ANY of them no longer resolves — a
-- two-paned escort level whose destination was demolished has nothing
-- coherent left to show, so the level closes as a whole rather than
-- rendering half of itself.
local function readViews(level)
    local kind = levelKind(level)
    if not kind then return nil end
    local out = {}
    for i, pane in ipairs(panesOf(level)) do
        local v = kind.view(level.src, pane.paneKey or MAIN_PANE)
        if not v then return nil end
        out[i] = v
    end
    return out
end

-----------------------------------------------------------
-- Opening
-----------------------------------------------------------

-- Re-apply a remembered tab to a freshly-opened pane. A plain open
-- always resets to "All" (a fresh pane's own default), so both restore
-- paths — the single-level reopenWithTab and the whole-stack
-- restoreStack — have to put the player's selection back afterwards,
-- and both must do it only IF it is still a valid category for the
-- (possibly changed) current contents.
local function applySavedTab(level, pane, tab)
    if not level or not pane or not tab or tab == pane.activeTab then return end
    for _, t in ipairs(itemList.getTabs(pane.listId)) do
        if t.key == tab then
            local views = readViews(level)
            if views then
                pane.activeTab = tab
                buildLevel(level, views)
            end
            return
        end
    end
end


-- Open `src` at depth `parentIndex + 1`, replacing whatever was there
-- and discarding every deeper level (requirement 3). The eligibility
-- read comes FIRST: an unknown kind, a vanished id, or a unit that is
-- not player-commandable is refused without creating any state and
-- WITHOUT disturbing the levels already open. Same ordering
-- scripts/etymology_panel.lua's openFor uses.
--
-- `reason` names why whatever was open at this depth is going away, and
-- reaches its level kind's `onClose`. It defaults to "replaced", which
-- is what an ordinary player gesture opening another container is —
-- only hud.createUI's own geometry pass passes "layout".
function cargoInventoryPanel.openLevel(src, mx, my, parentIndex, reason)
    if type(src) ~= "table" then return false end
    local index = (parentIndex or 0) + 1
    if index < 1 then return false end
    local ls = levels()
    if index > #ls + 1 then return false end

    local probe = { src = src, index = index }
    local kind = levelKind(probe)
    if not kind then return false end
    -- Probe EVERY pane before disturbing anything: a two-paned level
    -- whose second endpoint is already gone must be refused whole, and
    -- refusing after the replacement teardown would leave the player
    -- with nothing where their window used to be.
    local paneKeys = paneKeysOf(kind)
    local views = {}
    for i, key in ipairs(paneKeys) do
        local v = kind.view(src, key)
        if not v then return false end
        views[i] = v
    end

    closeLevelsFrom(index, reason or "replaced")
    local level = {
        src = src, index = index, mx = mx, my = my,
        paneKey = paneKeys[1], activeTab = "All", scroll = 0,
    }
    -- The level IS its own first pane (see the header); any further
    -- pane is a plain table carrying only the per-pane fields.
    level.panes = { level }
    for i = 2, #paneKeys do
        level.panes[i] = { paneKey = paneKeys[i], activeTab = "All",
                           scroll = 0 }
    end
    -- Depth 2 and beyond own a LayerModal page. Created fresh each time
    -- so its handle is higher than every shallower level's: pages sort
    -- by (layer, zIndex) with ties broken by handle order, so the
    -- newest modal page paints on top and owns the input boundary.
    if index > 1 then
        level.pageId = UI.newPage("container_window_lvl" .. index, "modal")
        UI.showPage(level.pageId)
    end
    ls[index] = level
    buildLevel(level, views)
    return true
end

-- Open the BASE level on one endpoint. `kind` is "building" or "unit";
-- `id` is that kind's own id. Returns true when the window opened.
function cargoInventoryPanel.openFor(kind, id, mx, my)
    return cargoInventoryPanel.openLevel(
        { kind = "endpoint", endpointKind = kind, id = id }, mx, my, 0)
end

-- #750 round-13 review: hud.lua's "resize" teardown (scripts/ui/
-- view_teardown.lua) closes this window before hud.world_page — which
-- the base level is mounted on — gets deleted and replaced; a
-- resize/rescale otherwise silently discarded the player's open window
-- (and which tab they had selected) rather than treating it as the
-- layout-only change #750 requires it to survive. Since #1238 hud.lua
-- snapshots the WHOLE STACK (snapshotStack) and restores it
-- (restoreStack); this entry point remains for the single-level case
-- and for callers outside that path.
--
-- A REFUSED open must abandon the whole call, not fall through to the
-- tab step. openFor deliberately leaves an already-open valid window
-- alone when it refuses, so the stack afterwards describes THAT window
-- — continuing here would apply this call's requested tab to an
-- unrelated endpoint the caller never named.
function cargoInventoryPanel.reopenWithTab(kind, id, mx, my, tab)
    if not cargoInventoryPanel.openFor(kind, id, mx, my) then return false end
    local base = levels()[1]
    applySavedTab(base, base, tab)
    return true
end

-----------------------------------------------------------
-- Closing / introspection
-----------------------------------------------------------

-- Destroy the whole stack. `reason` reaches every closed level's kind
-- hook: "layout" means a geometry rebuild that a snapshot/restore pass
-- is about to undo, anything else (the default) is a real close.
function cargoInventoryPanel.closeIfOpen(reason)
    closeLevelsFrom(1, reason or "closed")
end

-- Close the deepest level only. Returns true when one was closed —
-- which is what makes Escape a one-level-per-press cascade.
function cargoInventoryPanel.popLevel()
    local ls = levels()
    if #ls == 0 then return false end
    closeLevelsFrom(#ls, "dismissed")
    -- The newly deepest level takes the wheel back.
    applyScrollCapture()
    return true
end

-- Rebuild one level's panes against fresh views, right now. update()
-- does this on its own cadence when the shared widget reports
-- staleness; a Mode A commit (#1250) moves items on the spot and
-- refreshes both panes within the same gesture rather than leaving a
-- stale weight on screen until the next tick. Returns false when the
-- level is gone or no longer resolves — never closes anything, because
-- that is update()'s own decision to make.
function cargoInventoryPanel.refreshLevel(level)
    if not level then return false end
    local views = readViews(level)
    if not views then return false end
    buildLevel(level, views)
    return true
end

-- The element-name prefix every widget of `paneKey` is built under —
-- the ONE rule that keeps two panes' controls distinguishable by name
-- (#1250 review round 3: control focus survives a geometry rebuild by
-- NAME, so two panes sharing one would restore a silently wrong
-- control). The single-pane case keeps the historic bare "cargo_inv",
-- so every element name a lone container window has ever had is
-- unchanged. Exposed so a gate can address a pane's controls without
-- restating it, and reached by the renderer through the controller.
function cargoInventoryPanel.paneWidgetName(paneKey)
    if paneKey == nil or paneKey == MAIN_PANE then return "cargo_inv" end
    return "cargo_inv_" .. paneKey
end

-- The pane of `level` addressed by `paneKey` (default: its first), or
-- nil. The escort session's own reads go through this rather than
-- indexing `panes` positionally.
function cargoInventoryPanel.getPane(level, paneKey)
    if not level then return nil end
    if paneKey == nil then return level end
    for _, pane in ipairs(panesOf(level)) do
        if (pane.paneKey or MAIN_PANE) == paneKey then return pane end
    end
    return nil
end

function cargoInventoryPanel.isOpen()
    return #levels() > 0
end

function cargoInventoryPanel.depth()
    return #levels()
end

-- The level at `index` (default: the deepest), or nil.
function cargoInventoryPanel.getLevel(index)
    local ls = levels()
    return ls[index or #ls]
end

-----------------------------------------------------------
-- Resize snapshot / restore (#750, whole-stack since #1238)
--
-- What must survive a geometry rebuild: the nesting PATH (every
-- level's own identity), each level's anchor, and EVERY pane's selected
-- tab and scroll offset. Rebuilt through the SAME open path a fresh
-- open uses, so every level renders exactly as if it had just been
-- opened against the new layout — then the remembered tabs and offsets
-- are re-applied.
--
-- Both halves pass "layout" (#1250): a resize destroys and rebuilds
-- every level precisely so that its kind's own state survives, so no
-- level kind may treat this pass as a close. That is what keeps an
-- escort session — and the unit it is holding — alive across a resize.
-----------------------------------------------------------
function cargoInventoryPanel.snapshotStack()
    local out = {}
    for i, level in ipairs(levels()) do
        local panes = {}
        for j, pane in ipairs(panesOf(level)) do
            panes[j] = { activeTab = pane.activeTab, scroll = pane.scroll }
        end
        out[i] = { src = level.src, mx = level.mx, my = level.my,
                   panes = panes }
    end
    return out
end

function cargoInventoryPanel.restoreStack(snapshot)
    cargoInventoryPanel.closeIfOpen("layout")
    if type(snapshot) ~= "table" then return end
    for i, saved in ipairs(snapshot) do
        if not cargoInventoryPanel.openLevel(saved.src, saved.mx, saved.my,
                                             i - 1, "layout") then
            -- A level whose source is gone stops the restore: every
            -- deeper level is addressed THROUGH it, so re-opening one
            -- without its parent would strand it.
            return
        end
        local level = levels()[i]
        for j, pane in ipairs(panesOf(level)) do
            local savedPane = (saved.panes or {})[j]
            if savedPane then
                applySavedTab(level, pane, savedPane.activeTab)
                if (savedPane.scroll or 0) > 0 then
                    pane.scroll = itemList.setScrollOffset(pane.listId,
                                                           savedPane.scroll)
                end
            end
        end
    end
end

-----------------------------------------------------------
-- Selection + scrolling (routed by the shared widget)
-----------------------------------------------------------

-- The tab strip is scripts/ui/tabbar's, so a click arrives through
-- uiManager.onTabClick like every other tabbar; this only records the
-- pane's own durable selection and rebuilds around it. Changing tab
-- resets the scroll: the new category is a different list, and an
-- offset carried into it would land the player somewhere they never
-- scrolled to. Only the CLICKED pane changes; a two-paned level's other
-- endpoint keeps whatever category the player left it on.
function cargoInventoryPanel.onPaneTabChange(level, pane, category)
    if category == pane.activeTab then return end
    local views = readViews(level)
    if not views then return end
    pane.activeTab = category
    pane.scroll    = 0
    buildLevel(level, views)
end

-- Mouse WHEEL over the window (uiManager.onUIScroll). Only the deepest
-- level captures the wheel, so this needs no hit test of its own beyond
-- confirming the event belongs to one of that level's own panels — and
-- WHICH panel is what decides which pane scrolls.
function cargoInventoryPanel.onScroll(elemHandle, _dx, dy)
    local level = cargoInventoryPanel.getLevel()
    if not level then return false end
    for _, pane in ipairs(panesOf(level)) do
        if pane.panelId
           and panel.getBoxHandle(pane.panelId) == elemHandle then
            local step = (dy or 0) > 0 and -1 or 1
            pane.scroll = itemList.scrollBy(pane.listId, step)
            return true
        end
    end
    return false
end

-----------------------------------------------------------
-- Per-tick refresh, re-reading each level's own source: live truth for
-- a unit, the knowledge record for a container (#1237/#1238) — which is
-- how a completed deposit or withdrawal reaches an OPEN window with no
-- plumbing of its own, since the engine already replaced the record at
-- the moment that movement committed. The rebuild comparison belongs to
-- the shared widget (this module keeps no hash of its own).
--
-- A level whose source no longer resolves closes, AND SO DOES EVERY
-- LEVEL BELOW IT: a nested level is addressed through its parent, so a
-- cargo that is demolished, a unit that dies, a container consumed out
-- of an inventory, or a refreshed knowledge snapshot that no longer
-- holds the remembered instance must never leave a deeper window
-- interactive over nothing — and must never silently retarget another
-- same-def instance.
-----------------------------------------------------------
function cargoInventoryPanel.update(dt)
    local ls = levels()
    local i = 1
    while i <= #ls do
        local level = ls[i]
        local kind  = levelKind(level)
        local views = readViews(level)
        if not views or (kind.stillThere and not kind.stillThere(level.src)) then
            closeLevelsFrom(i, "endpoint_gone")
            applyScrollCapture()
            return
        end
        -- ONE staleness question for the whole level: a rebuild
        -- reconstructs every pane together (they are placed against each
        -- other), so rebuilding on the first stale pane is both correct
        -- and enough.
        local stale = false
        for j, pane in ipairs(panesOf(level)) do
            if itemList.isStale(pane.listId,
                                render.listDataParams(CONTROL, kind, level,
                                                      pane, views[j])) then
                stale = true
                break
            end
        end
        if stale then buildLevel(level, views) end
        -- The age advances every game second, which nothing else in the
        -- window does. Retexting the one label in place is deliberately
        -- NOT part of the staleness comparison: routing it through
        -- presentationKey would tear the whole popup down and rebuild it
        -- once a second for as long as a container window is open.
        for j, pane in ipairs(panesOf(level)) do
            if pane.ageId then
                local age = endpoints.ageText(views[j])
                if age then label.setText(pane.ageId, age) end
            end
        end
        i = i + 1
    end
end

-----------------------------------------------------------
-- Introspection (probe oracle, #1238)
--
-- The whole stack, in order, with each level's identity, chrome state
-- and list handle — enough to prove a nesting path, a modal boundary,
-- a per-pane scroll offset and a remembered age WITHOUT hardcoding a
-- screen coordinate. Row bounds/handles remain itemList.dump()'s.
--
-- A level's own `title`/`activeTab`/`scroll`/`listId` are its FIRST
-- pane's, which is every pane there is for all but an escort level
-- (#1250); `panes` reports each one separately, with the panel geometry
-- a two-paned level's flanking and clamping is proved from.
-----------------------------------------------------------
function cargoInventoryPanel.dump()
    local out = { depth = #levels(), levels = {},
                  inputBlocked = UI.isInputBlocked() }
    for i, level in ipairs(levels()) do
        local views = readViews(level) or {}
        local panes = {}
        for j, pane in ipairs(panesOf(level)) do
            local view = views[j]
            local rows = pane.listId and itemList.getRows(pane.listId) or {}
            local bx, by, bw, bh
            if pane.panelId then
                bx, by = panel.getPosition(pane.panelId)
                bw, bh = panel.getSize(pane.panelId)
            end
            panes[j] = {
                index      = j,
                paneKey    = pane.paneKey or MAIN_PANE,
                title      = view and view.title or nil,
                subtitle   = view and view.subtitle or nil,
                ageText    = view and endpoints.ageText(view) or nil,
                activeTab  = pane.activeTab,
                scroll     = pane.scroll,
                maxScroll  = pane.listId
                               and itemList.maxScrollOffset(pane.listId) or 0,
                rowCount   = #rows,
                listId     = pane.listId,
                x          = bx,
                y          = by,
                width      = bw,
                height     = bh,
            }
        end
        local view = views[1]
        out.levels[i] = {
            index        = i,
            kind         = level.src.kind,
            endpointKind = level.src.endpointKind,
            id           = level.src.id,
            uid          = level.src.uid,
            bid          = level.src.bid,
            defName      = level.src.defName,
            instanceId   = level.src.instanceId,
            path         = level.src.path,
            sessionId    = level.src.sessionId,
            title        = view and view.title or nil,
            subtitle     = view and view.subtitle or nil,
            ageText      = view and endpoints.ageText(view) or nil,
            revealedAt   = view and view.knowledge
                             and view.knowledge.revealedAt or nil,
            activeTab    = level.activeTab,
            scroll       = level.scroll,
            maxScroll    = level.listId
                             and itemList.maxScrollOffset(level.listId) or 0,
            rowCount     = panes[1] and panes[1].rowCount or 0,
            listId       = level.listId,
            paneCount    = #panes,
            panes        = panes,
            pageId       = level.pageId,
            modal        = level.pageId ~= nil,
            -- The direct read of "is this level still interactive?":
            -- a page below the modal boundary is out of scope, so
            -- every shallower level reports false the moment a deeper
            -- one opens, and the deepest always reports true.
            pageInScope  = (levelPage(level) ~= nil)
                             and UI.isPageInScope(levelPage(level)) or false,
        }
    end
    return out
end

-----------------------------------------------------------
-- Engine script hooks
-----------------------------------------------------------
function cargoInventoryPanel.init(scriptId)
    engine.logDebug("Container window initializing...")
end

function cargoInventoryPanel.shutdown()
    cargoInventoryPanel.closeIfOpen()
    engine.logDebug("Container window shut down")
end

-- Esc closes the DEEPEST level, one per press (requirement 6). Returns
-- true if consumed. Named handle* (not on*) deliberately: this module
-- is engine-loaded, so an on*-named function would also fire directly
-- on every engine broadcast — double-firing on top of init.lua's
-- ordered forward. The two modules split out in #2155 are require-only
-- and define no on*-named function at all, for the same reason.
function cargoInventoryPanel.handleKeyDown(key)
    if key == "Escape" then
        return cargoInventoryPanel.popLevel()
    end
    return false
end

return cargoInventoryPanel
