-- Container Window (historically the cargo inventory panel)
--
-- Since #1238 this module is THE container-window manager, and what it
-- owns is an ordered STACK of LEVELS rather than one popup. Level 1 is
-- an endpoint (a storage building or a unit); opening a container that
-- lives INSIDE an open level pushes a deeper level, and the nesting
-- path is remembered so a resize can rebuild the whole thing. D-9's
-- one-window rule is per LEVEL, not per screen.
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
-- unitItem/buildingItem are `scripts/item_contents_panel.lua`'s (D-13)
-- and escort is `scripts/transfer_session_panels.lua`'s: neither module
-- owns a window lifecycle at all — each supplies its level's data and
-- presentation, and this file draws it with the same chrome every other
-- level uses.
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
-- drift; this file supplies only which endpoint the level is showing.
-- An item-container level supplies no transfer action at all, so the
-- render-only rule above holds by construction rather than by a test
-- against the level kind.
--
-- Earlier contracts this window still holds:
--
-- Since #1088 the tabbed list is the shared item-list widget
-- (scripts/ui/item_list.lua) — this module owns the popup window, its
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
-- table below (one live read, one weight label, one descent rule). An
-- unknown kind is REJECTED rather than assumed.
--
-- Since #1237 an endpoint's contents are either LIVE TRUTH or the
-- player's REMEMBERED view, and the endpoint kind decides which. A
-- remembering source says so by returning a `knowledge` sub-table from
-- its view, and everything below branches on THAT rather than on the
-- kind — which is exactly why a building-side item level (#1238) gets
-- its "as of…" line for free.
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
--   isOpen() / depth() / getLevel(i)     — introspection
--   getPane(level, paneKey)              — one pane of a level
--   snapshotStack() / restoreStack(s)    — the resize-restore pair
--   onScroll(handle, dx, dy)             — wheel over the window
--   dump()                               — probe oracle
--
-- Shared with the escort level kind, so the pair renders through
-- exactly the endpoint machinery a lone container window uses:
--   endpointView(kind, id) / endpointListParams(kind, view)
--   endpointStillThere(kind, id) / endpointChildOf(kind, id, row)
--
-- Engine script hooks: setup / init / update / shutdown.
--
-- Module is registered in package.loaded so init.lua's right-click
-- handler and ui_manager's click dispatchers all see the same
-- instance even though engine.loadScript uses dofile.

local cargoInventoryPanel =
    package.loaded["scripts.cargo_inventory_panel"] or {}
package.loaded["scripts.cargo_inventory_panel"] = cargoInventoryPanel

local panel       = require("scripts.ui.panel")
local label       = require("scripts.ui.label")
local scale       = require("scripts.ui.scale")
local qualityTier = require("scripts.ui.quality_tier")
local itemList    = require("scripts.ui.item_list")

-----------------------------------------------------------
-- Layout constants. Mirrors unit_info_v2's inventory section so
-- the two read the same visually. Base units; uiscale applied at
-- draw time. Padding clears the 9-patch border art (~16–20 px per
-- side at scale 1) AND leaves visible breathing room — same lesson
-- as the build menu's padding fix.
-----------------------------------------------------------
local PANEL_PAD_X    = 32
local PANEL_PAD_TOP  = 28
local PANEL_PAD_BOT  = 20
local TITLE_FONT     = 16
local TITLE_H        = 22
local SUBTITLE_FONT  = 13
local SUBTITLE_H     = 18
local AGE_FONT       = 12
local AGE_H          = 16
local TAB_H          = 28
local TAB_TILE       = 16
local TAB_FONT       = 13
local TAB_TEXT_PAD   = 22    -- horizontal pad inside each tab
local TAB_GAP        = 6     -- gap between tabs
local TAB_TEXT_COL   = { 0.0, 0.0, 0.0, 1.0 }
local TAB_SEL_TEXT_COL = { 1.0, 1.0, 1.0, 1.0 }
local ROW_H          = 32
local ROW_PAD        = 2
local ICON_SZ        = 28
local TEXT_PAD       = 12    -- horizontal pad inside each row
local NAME_RIGHT_GAP = 24    -- gap between name and weight columns
local TITLE_COL      = { 1.0, 1.0, 1.0, 1.0 }
local SUBTITLE_COL   = { 0.85, 0.85, 0.85, 1.0 }
local AGE_COL        = { 0.70, 0.70, 0.70, 1.0 }
local ROW_NAME_COL   = { 1.0, 1.0, 1.0, 1.0 }
local ROW_WEIGHT_COL = { 0.85, 0.85, 0.85, 1.0 }

-- Frame-free single-row tab strip, shrunk proportionally when its
-- natural width exceeds the panel's content column (#750 round-8/12).
local CARGO_TABS = {
    mode        = "row",
    shrinkToFit = true,
    tabHeight   = TAB_H,
    tileSize    = TAB_TILE,
    fontSize    = TAB_FONT,
    textPad     = TAB_TEXT_PAD,
    gap         = TAB_GAP,
    textColor         = TAB_TEXT_COL,
    selectedTextColor = TAB_SEL_TEXT_COL,
}

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
-- Helpers
-----------------------------------------------------------

-- A unit's own display text, following the #264 precedence every other
-- surface that names a unit uses. transferEndpointInfo reports only the
-- species-level displayName, which is the fallback rather than the
-- answer.
local function unitTitle(uid, info)
    local live = unit.getInfo(uid)
    if live then
        if live.name and live.name ~= "" then return live.name end
        if live.displayName and live.displayName ~= "" then
            return live.displayName
        end
        if live.defName then return live.defName end
    end
    if info.displayName and info.displayName ~= "" then
        return info.displayName
    end
    return "Inventory"
end

-----------------------------------------------------------
-- Endpoint kinds (#1234)
--
-- ONE window, two data sources. Each entry answers exactly the
-- questions that differ between kinds; everything below this table is
-- kind-blind. `view` is the SINGLE read — nil means "not an
-- eligible endpoint", which covers an unknown id, a destroyed
-- instance, and a unit that is not player-commandable alike, so open
-- and the per-tick lifecycle check share one definition of eligible.
--
-- A kind whose contents can be STALE (#1237) returns a `knowledge`
-- sub-table beside them (`state` plus the observation's `revealedAt`);
-- omitting it declares the view live truth. `stored` is likewise
-- nil-able: nil means "not known", which is a different fact from the
-- 0 kg a never-inspected container's engine record reports.
--
-- `stillThere` exists only where liveness is a DIFFERENT question from
-- eligibility: a cargo's capacity is def-declared, so a building's
-- popup must not outlive the instance rather than its storage. A kind
-- that omits it is governed by `view` alone.
--
-- A kind declares NO transfer action of its own since #1249. Both
-- endpoint kinds now offer the SAME "Retrieve" gesture, built once by
-- scripts/transfer_gestures.lua from the endpoint identity alone (see
-- LEVELS.endpoint.transferMenu) — where the pre-#1249 building-only
-- `rowMenu` hook hung an immediate `unit.withdrawFromCargo` that
-- required an adjacent selected acolyte and had no unit-endpoint
-- counterpart at all. #1238's "Contents" entry is still appended
-- separately and for every kind, because inspecting a nested container
-- is not a transfer gesture.
--
-- `childOf` maps a row of THIS endpoint kind to the item-container
-- level it opens: a building endpoint descends into its own REMEMBERED
-- record, a unit endpoint into its LIVE inventory.
-----------------------------------------------------------
local ENDPOINTS = {
    building = {
        weightLabel = "Storage",
        -- A container reports the player's REMEMBERED contents (#1237),
        -- so this reads building.getContainerKnowledge rather than the
        -- live building.getStorage / getStorageWeight pair. That call
        -- is a pure READ: merely opening this window must not reveal
        -- anything, which is why building.refreshContainerKnowledge is
        -- deliberately absent from this file entirely.
        --
        -- `capacity` is the knowledge record's own field and is ALWAYS
        -- LIVE — the player knows how big a thing they built — so it
        -- keeps doubling as the "is this an endpoint at all?" gate the
        -- pre-#1237 getStorageCapacity read was.
        view = function(id)
            local k = building.getContainerKnowledge(id)
            if not k then return nil end
            local cap = k.capacity or 0
            if cap <= 0 then return nil end
            local binfo = building.getInfo(id)
            local state = k.state or "unknown"
            local view = {
                title    = (binfo and (binfo.displayName or binfo.defName))
                             or "Cargo",
                capacity = cap,
                stored   = nil,
                contents = {},
                knowledge = { state = state, revealedAt = k.revealedAt },
            }
            -- Never-inspected keeps `stored` nil and no rows: its
            -- remembered weight and item list are absences, not zeroes.
            if state ~= "unknown" then
                view.stored   = k.storedWeight or 0
                view.contents = k.items or {}
            end
            return view
        end,
        stillThere = function(id) return building.getInfo(id) ~= nil end,
        -- A building-stored container's nested contents are REMEMBERED
        -- (#1238 requirement 5): the level descends the knowledge
        -- record by exact instance identity, so it can never read the
        -- live building and can never reveal anything.
        childOf = function(id, row)
            return { kind = "buildingItem", bid = id,
                     path = { row.instanceId },
                     defName = row.defName, displayName = row.displayName }
        end,
    },
    unit = {
        weightLabel = "Carrying",
        -- `contents` is LOOSE INVENTORY; `storedWeight` deliberately is
        -- not its sum — it is the endpoint's whole recursive load
        -- (inventory + equipment + accessories), measured by the same
        -- rule the capacity gate uses. Both come from the one engine
        -- read so the header and the rows can never disagree about
        -- which instant they describe.
        view = function(id)
            local info = unit.transferEndpointInfo({ kind = "unit", id = id })
            if not info or info.eligible ~= true then return nil end
            return {
                title    = unitTitle(id, info),
                capacity = info.capacity or 0,
                stored   = info.storedWeight or 0,
                contents = info.contents or {},
            }
        end,
        childOf = function(id, row)
            return { kind = "unitItem", uid = id, defName = row.defName,
                     instanceId = row.instanceId, path = {},
                     displayName = row.displayName }
        end,
    },
}

-----------------------------------------------------------
-- Remembered-contents presentation (#1237)
--
-- These are the ONLY places the window knows that contents can be
-- stale, and each answers one question about a `view`. A view with no
-- `knowledge` sub-table is live truth and every one of them degrades to
-- exactly the pre-#1237 rendering, which is what keeps a unit endpoint
-- unchanged without any kind test down here.
-----------------------------------------------------------

-- "unknown" / "empty" / "known" for a remembering endpoint, "live" for
-- one that reports the truth. Distinct strings for all four so a
-- presentation key can never conflate never-inspected with known-empty.
local function knowledgeState(view)
    local k = view.knowledge
    if not k then return "live" end
    return k.state or "unknown"
end

-- Format an elapsed span of GAME-CLOCK seconds (engine.gameTime()'s own
-- currency — it freezes while the player is paused and skips the
-- real-world gap across a save/load) as player-legible text.
--
-- Deliberately NOT converted into calendar days/hours: the calendar
-- advances on its own per-page clock whose length is a worldgen
-- parameter, so no fixed factor relates the two, and spelling a game
-- second as an in-world hour would be wrong by whatever that world
-- chose. What this reads as is elapsed PLAY time, which is exactly what
-- the clock measures.
local function formatAge(elapsed)
    local s = math.floor(elapsed or 0)
    if s < 0 then s = 0 end
    if s < 5 then return "just now" end
    if s < 60 then return string.format("%ds ago", s) end
    if s < 3600 then
        local m, rs = math.floor(s / 60), s % 60
        if m < 10 and rs > 0 then return string.format("%dm %ds ago", m, rs) end
        return string.format("%dm ago", m)
    end
    if s < 86400 then
        local hr, m = math.floor(s / 3600), math.floor((s % 3600) / 60)
        if m > 0 then return string.format("%dh %dm ago", hr, m) end
        return string.format("%dh ago", hr)
    end
    local d, hr = math.floor(s / 86400), math.floor((s % 86400) / 3600)
    if hr > 0 then return string.format("%dd %dh ago", d, hr) end
    return string.format("%dd ago", d)
end

cargoInventoryPanel.formatAge = formatAge

-- The "as of…" line, or nil when there is no observation to date: a
-- live endpoint, a never-inspected container (whose revealedAt is
-- deliberately absent), or an engine with no game clock to compare
-- against. Recomputed on every read rather than cached, which is what
-- makes it ADVANCE as game time passes.
local function ageText(view)
    local k = view.knowledge
    if not k or type(k.revealedAt) ~= "number" then return nil end
    if knowledgeState(view) == "unknown" then return nil end
    if type(engine.gameTime) ~= "function" then return nil end
    local now = engine.gameTime()
    if type(now) ~= "number" then return nil end
    return "as of " .. formatAge(now - k.revealedAt)
end

-- The stored-weight half of the header. A never-inspected container's
-- remembered weight is not 0 kg — it is not known at all, and the
-- engine's own numeric 0 there must never be rendered as a measurement.
-- Capacity is always shown: the player knows how big a thing they built.
local function weightText(kind, view)
    local def = ENDPOINTS[kind]
    local wlabel = (def and def.weightLabel) or "Storage"
    if view.stored == nil then
        return string.format("%s: unknown / %.2f kg", wlabel, view.capacity)
    end
    return string.format("%s: %.2f / %.2f kg", wlabel, view.stored,
                         view.capacity)
end

-- What an empty row list MEANS, which is a different fact in each
-- state. A live endpoint keeps its pre-#1237 blank (nothing here claims
-- to know anything about a unit's inventory it did not render).
local function emptyText(view)
    local st = knowledgeState(view)
    if st == "unknown" then return "Contents unknown (never inspected)" end
    if st == "empty"   then return "(empty)" end
    return nil
end

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
-- modality, scrolling, teardown, restore — is shared below and
-- level-kind blind.
--
-- The two item kinds live in scripts/item_contents_panel.lua (D-13) and
-- the escort kind in scripts/transfer_session_panels.lua, both required
-- lazily so the modules can reference each other without a load-order
-- cycle.
-----------------------------------------------------------
local function itemLevels()
    return require("scripts.item_contents_panel").levelKinds()
end

local function escortLevels()
    return require("scripts.transfer_session_panels").levelKinds()
end

-- The endpoint level's own four questions, answered from an endpoint
-- IDENTITY rather than a level `src`. Exposed because the escort
-- session's level (#1250) is two of these side by side: reading them
-- through this module is what makes an escort pane render, refresh,
-- close and descend exactly the way a lone container window does, with
-- no second copy of the ENDPOINTS table to drift.
function cargoInventoryPanel.endpointView(endpointKind, id)
    local def = ENDPOINTS[endpointKind]
    if not def or id == nil then return nil end
    local view = def.view(id)
    if not view then return nil end
    view.subtitle  = weightText(endpointKind, view)
    view.emptyText = emptyText(view)
    return view
end

-- Deliberately NOT folded into `endpointView`: liveness is the per-tick
-- LIFECYCLE question, and it is asked only by update(). Opening
-- has always been governed by the view alone, and merging the two
-- would refuse an open the endpoint's own data supports.
function cargoInventoryPanel.endpointStillThere(endpointKind, id)
    local def = ENDPOINTS[endpointKind]
    if not def or not def.stillThere then return true end
    return def.stillThere(id) and true or false
end

function cargoInventoryPanel.endpointChildOf(endpointKind, id, row)
    local def = ENDPOINTS[endpointKind]
    if not def or not def.childOf then return nil end
    return def.childOf(id, row)
end

-- The container window's own tab-strip style. Exposed so an escort pane
-- (#1250) renders a tab strip identical to a lone window's rather than
-- a second copy of these constants that could drift.
function cargoInventoryPanel.endpointTabSpec()
    return CARGO_TABS
end

-- The item-list parameters that describe an endpoint's data and
-- presentation policy. Everything the widget needs to group, tab,
-- render and invalidate; bounds are added by buildLevel once the
-- panel has been sized from the resulting row count.
--
-- The host-owned title/subtitle chrome rides in `presentationKey`
-- because the widget cannot see it: a unit's stored weight moves
-- when it equips something its loose inventory never listed, and
-- without this the header would keep reporting the load it had at
-- open. The knowledge STATE rides there too (#1237) — an unknown
-- container and a known-empty one both render zero rows, so without
-- it the transition between them would leave the header still
-- reading "unknown". The "as of…" line deliberately does NOT: it
-- changes every game second and is refreshed in place by update()
-- instead of rebuilding the window.
--
-- The row TOOLTIP is #1268's, deliberately bounded to the row's own
-- display text plus the labeled temperature line: quality,
-- condition, weapon and fill detail stay out of this window. It is
-- supplied for EVERY endpoint kind, so a unit endpoint and a
-- building endpoint present temperature identically.
function cargoInventoryPanel.endpointListParams(endpointKind, view)
    -- The row's display text WITHOUT the temperature summary: the
    -- row appends it, and the tooltip labels it on its own line, so
    -- building both from this shared base keeps it from appearing
    -- twice in one tooltip.
    local function rowBaseName(g)
        local n = qualityTier.withSuffix(
            g.displayName or g.defName or "?", g)
        if (g.count or 1) > 1 then
            n = string.format("%s ×%d", n, g.count)
        end
        return n
    end
    return {
        emptyText = view.emptyText,
        items     = view.contents,
        uiscale   = scale.get(),
        rowName   = function(g)
            return itemList.withTempSuffix(rowBaseName(g), g)
        end,
        rowTooltip = function(g)
            local tempLine = itemList.tempHintLine(g)
            if not tempLine then return nil end
            return { text = rowBaseName(g), hint = tempLine }
        end,
        rowWeightText = function(g)
            return string.format("%.2f kg",
                                 (g.weight or 0) * (g.count or 1))
        end,
        rowColor  = function() return ROW_NAME_COL end,
        presentationKey = string.format("%s|%s|%s|%.3f/%.3f",
                                        tostring(endpointKind),
                                        tostring(view.title),
                                        knowledgeState(view),
                                        view.stored or -1, view.capacity),
    }
end

local LEVELS = {}

LEVELS.endpoint = {
    panelWidthBase = 460,
    maxRows        = 10,
    tabs           = CARGO_TABS,
    view = function(src)
        return cargoInventoryPanel.endpointView(src.endpointKind, src.id)
    end,
    stillThere = function(src)
        return cargoInventoryPanel.endpointStillThere(src.endpointKind, src.id)
    end,
    listParams = function(src, view)
        return cargoInventoryPanel.endpointListParams(src.endpointKind, view)
    end,
    -- The endpoint level's transfer action (#1249): "Retrieve 1" /
    -- "Retrieve all" into the unit the shared selection rule resolves.
    --
    -- Built from the endpoint IDENTITY alone, so a unit endpoint and a
    -- building endpoint offer the identical gesture — where the retired
    -- `unit.withdrawFromCargo` path was building-only by construction
    -- (that verb takes a BuildingId) and a unit endpoint's rows
    -- therefore had no transfer action at all.
    transferMenu = function(src, row)
        local gestures = require("scripts.transfer_gestures")
        return gestures.retrieveEntries(
            { kind = src.endpointKind, id = src.id }, row)
    end,
    childOf = function(src, row)
        return cargoInventoryPanel.endpointChildOf(src.endpointKind, src.id, row)
    end,
}

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

local function destroyPaneElements(pane)
    if pane.listId then itemList.destroy(pane.listId); pane.listId = nil end
    if pane.titleId    then label.destroy(pane.titleId);    pane.titleId = nil end
    if pane.subtitleId then label.destroy(pane.subtitleId); pane.subtitleId = nil end
    if pane.ageId      then label.destroy(pane.ageId);      pane.ageId = nil end
    if pane.panelId    then panel.destroy(pane.panelId);    pane.panelId = nil end
end

local function destroyLevelElements(level)
    for _, pane in ipairs(panesOf(level)) do destroyPaneElements(pane) end
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
    destroyLevelElements(level)
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

-- Only the DEEPEST level captures the wheel. Scroll routing (#744)
-- picks the topmost in-scope scroll-capturing surface, and the modal
-- boundary already puts shallower levels out of scope — but the BASE
-- level is not behind any boundary when it is alone, so its capture
-- has to be released explicitly the moment a deeper level opens.
local function applyScrollCapture()
    local ls = levels()
    for i, level in ipairs(ls) do
        for _, pane in ipairs(panesOf(level)) do
            if pane.panelId then
                local h = panel.getBoxHandle(pane.panelId)
                if h then UI.setScrollCapture(h, i == #ls) end
            end
        end
    end
end

-----------------------------------------------------------
-- Render: title row
-----------------------------------------------------------
-- Every header baseline is measured in SCALED units, matching the
-- scaled band heights buildLevel reserves (titleH/subH/ageH) and the
-- scaled font label.new actually rasterises. Round-1 review of #1237:
-- the pre-#1237 code advanced by the RAW TITLE_H/SUBTITLE_H constants
-- and offset each baseline by a raw fontSize, so above 1x the lines
-- advanced more slowly than their own glyphs grew — at 2x the third
-- line's glyph mass reached back up into the second's, and the reserved
-- space below stayed empty. Identical arithmetic at uiscale 1.
--
-- A text element's position IS its baseline and its glyph mass sits
-- ABOVE it (scripts/ui/label.lua), so band N's baseline is its band top
-- plus a fraction of the scaled font — the ascent — not its band
-- height.
local function headerBaselines(uiscale)
    local titleH = math.floor(TITLE_H    * uiscale)
    local subH   = math.floor(SUBTITLE_H * uiscale)
    return math.floor(TITLE_FONT * uiscale * 0.85),
           titleH + math.floor(SUBTITLE_FONT * uiscale * 0.85),
           titleH + subH + math.floor(AGE_FONT * uiscale * 0.85)
end

-- `uiscale` is the PANE's effective scale, not the configured one
-- (#1250 review round 2). measurePane reserves the header's three bands
-- at that scale and buildPane places the list below them from the same
-- number, so rasterising the labels at any other scale would draw
-- glyphs into space that was never measured for them — at 800x600 the
-- fitted escort boxes and lists shrink while full-size headers would
-- reach down into the rows. One scale in, three bands and three labels
-- out.
local function buildTitle(level, pane, originX, originY, view, uiscale)
    local h = cargoInventoryPanel.hud
    if not h then return end
    local page = levelPage(level)
    local titleBase, subBase, ageBase = headerBaselines(uiscale)

    pane.titleId = label.new({
        name     = "cargo_inv_title",
        text     = view.title,
        font     = h.menuFont,
        fontSize = TITLE_FONT,
        color    = TITLE_COL,
        page     = page,
        uiscale  = uiscale,
    })
    local th = label.getElementHandle(pane.titleId)
    UI.addToPage(page, th, originX, originY + titleBase)
    UI.setZIndex(th, 132)

    pane.subtitleId = label.new({
        name     = "cargo_inv_subtitle",
        text     = view.subtitle or "",
        font     = h.menuFont,
        fontSize = SUBTITLE_FONT,
        color    = SUBTITLE_COL,
        page     = page,
        uiscale  = uiscale,
    })
    local sh = label.getElementHandle(pane.subtitleId)
    UI.addToPage(page, sh, originX, originY + subBase)
    UI.setZIndex(sh, 132)

    -- The "as of…" line exists only for a snapshot there is an
    -- observation time for, so a live endpoint and a never-inspected
    -- container both render no third line at all — and buildLevel
    -- reserves its height from the SAME predicate (ageLineHeight below),
    -- so the panel can never size for a line it does not draw.
    local age = ageText(view)
    if age then
        pane.ageId = label.new({
            name     = "cargo_inv_age",
            text     = age,
            font     = h.menuFont,
            fontSize = AGE_FONT,
            color    = AGE_COL,
            page     = page,
            uiscale  = uiscale,
        })
        local ah = label.getElementHandle(pane.ageId)
        UI.addToPage(page, ah, originX, originY + ageBase)
        UI.setZIndex(ah, 132)
    end
end

-- The vertical space buildLevel must reserve for that line. Keyed on
-- the same ageText() answer buildTitle draws from, never on the state
-- alone: they must agree or the list overlaps the header.
local function ageLineHeight(view, uiscale)
    if not ageText(view) then return 0 end
    return math.floor(AGE_H * uiscale)
end

-----------------------------------------------------------
-- Row actions
--
-- ONE menu builder for every level kind. The kind's own transfer action
-- comes first (an endpoint row's "Retrieve" entries, #1249); "Contents"
-- is APPENDED for an item-container row, which is inspection rather
-- than transfer and is therefore offered on every kind — including the
-- item-container levels themselves, which is what makes the stack nest
-- arbitrarily deep. A kind with neither produces no menu at all, so the
-- widget's right-click resolves to nothing rather than an empty popup.
-----------------------------------------------------------
local function rowIsContainer(row)
    return row ~= nil and row.kind == "container"
       and type(row.instanceId) == "number" and row.instanceId > 0
end

-- A level kind's transfer action takes the level's SOURCE, the row and
-- the PANE the row was rendered in, and nothing else. It used to be
-- handed an `invalidate` closure too, because the retired withdraw
-- entry moved an item on the spot and wanted the list redrawn on the
-- same frame; since #1249 a Mode B transfer gesture only QUEUES an
-- order, so no contents change when it fires and there is nothing to
-- invalidate. The movement lands when the executor arrives, and
-- update()'s existing per-tick re-read is what shows it. A Mode A
-- escort commit (#1250) DOES move items immediately, and it reaches the
-- same per-tick re-read the same way rather than reviving that closure.
--
-- The pane key is what tells a two-paned level which of its endpoints
-- the player right-clicked, and it is passed to every single-pane kind
-- as well (as MAIN_PANE) so the hook signature never varies.
local function rowMenuFor(level, pane, row)
    local kind = levelKind(level)
    if not kind then return nil end
    local paneKey = pane.paneKey or MAIN_PANE
    local items = (kind.transferMenu
                   and kind.transferMenu(level.src, row, paneKey)) or {}
    if rowIsContainer(row) and kind.childOf then
        local childSrc = kind.childOf(level.src, row, paneKey)
        if childSrc then
            local index = level.index
            items[#items + 1] = {
                label    = "Contents",
                callback = function()
                    local mx, my = engine.getMousePosition()
                    local fbW, fbH = engine.getFramebufferSize()
                    local ww, wh = engine.getWindowSize()
                    if ww and wh and ww > 0 and wh > 0 then
                        mx = mx * (fbW / ww)
                        my = my * (fbH / wh)
                    end
                    cargoInventoryPanel.openLevel(childSrc, mx, my, index)
                end,
            }
        end
    end
    if #items == 0 then return nil end
    return items
end

-- The widget hands back the exact rendered row's representative
-- instance; the LEVEL decides what a row action means, which the widget
-- deliberately never learns.
local function showRowMenu(level, pane, row)
    if not row then return false end
    local items = rowMenuFor(level, pane, row)
    if not items or #items == 0 then return false end

    local contextMenu = require("scripts.ui.context_menu")
    local mx, my = engine.getMousePosition()
    local fbW, fbH = engine.getFramebufferSize()
    local ww, wh = engine.getWindowSize()
    if ww and wh and ww > 0 and wh > 0 then
        mx = mx * (fbW / ww)
        my = my * (fbH / wh)
    end
    contextMenu.show(items, mx, my)
    return true
end

-----------------------------------------------------------
-- Build one level
-----------------------------------------------------------

-- Full item-list params: the level kind's presentation policy plus the
-- pieces every pane shares (tabs, row cap, scroll offset, routing).
local function listDataParams(level, pane, view)
    local kind = levelKind(level)
    local paneKey = pane.paneKey or MAIN_PANE
    local p = kind.listParams(level.src, view, paneKey)
    p.activeTab    = pane.activeTab
    p.tabs         = kind.tabs or false
    p.maxRows      = kind.maxRows
    p.scrollOffset = pane.scroll or 0
    p.onTabChange  = kind.tabs and function(category)
        cargoInventoryPanel.onPaneTabChange(level, pane, category)
    end or nil
    -- Every pane routes right-clicks: even a kind with no transfer
    -- action can offer "Contents", and the widget's `rc=` signature
    -- means the callback's presence is part of the rebuild comparison.
    p.onRowRightClick = function(row) return showRowMenu(level, pane, row) end
    return p
end

-- Everything about ONE pane that depends only on its own data: the
-- normalized item-list model and the panel size that model implies.
-- Separated from drawing because a multi-pane level has to know EVERY
-- pane's size before it can place ANY of them (they avoid each other).
local function measurePane(level, pane, view)
    local h = cargoInventoryPanel.hud
    local kind = levelKind(level)

    -- Normalize the data ONCE through the shared widget, then size the
    -- panel from the row count it produces. The widget snaps a
    -- no-longer-present selection back to "All"; mirror that into the
    -- pane's own durable activeTab so the resize snapshot never
    -- carries a dead category forward.
    local dataParams = listDataParams(level, pane, view)
    local model = itemList.prepare(dataParams)
    dataParams.model = model
    pane.activeTab = model.activeTab

    -- A kind may render its panes at a LOCAL effective uiscale rather
    -- than the configured one (#1250): the escort pair has to fit TWO
    -- panels side by side, which is a constraint no single pane can see.
    -- Its own listParams sets the same value on the widget, so the box
    -- and the text inside it shrink together (#750).
    local uiscale = (kind.paneScale and kind.paneScale(level, pane.paneKey))
                      or scale.get()
    local panelW  = math.floor(kind.panelWidthBase * uiscale)
    local padTop  = math.floor(PANEL_PAD_TOP * uiscale)
    local padBot  = math.floor(PANEL_PAD_BOT * uiscale)
    local titleH  = math.floor(TITLE_H    * uiscale)
    local subH    = math.floor(SUBTITLE_H * uiscale)
    local ageH    = ageLineHeight(view, uiscale)
    local tabH    = kind.tabs and math.floor(TAB_H * uiscale) or 0
    local tabPad  = kind.tabs and 8 or 0
    local rowH    = math.floor(ROW_H      * uiscale)
    local rowPad  = math.floor(ROW_PAD    * uiscale)

    local visibleCount = math.min(#model.visible, kind.maxRows)
    -- Always reserve one row's height so an empty container isn't a
    -- flat strip — easier to read "(empty)" / nothing than a single
    -- line.
    if visibleCount < 1 then visibleCount = 1 end

    local rowsH    = visibleCount * rowH + (visibleCount - 1) * rowPad
    local panelH   = padTop + titleH + subH + ageH + 6 + tabH + tabPad
                       + rowsH + padBot

    -- #750 round-7 review: cap against the actual framebuffer — the
    -- position clamp below only ever repositions the panel,
    -- never shrinks it, so panelWidthBase*uiscale (460 at 1x, 1840 at a
    -- still-C2-supported 4x) could exceed the framebuffer several times
    -- over regardless of position, leaving tabs/items/actions
    -- off-screen. Best-effort degrade, same pattern as popup.lua/
    -- unit_info_v2.lua/build_tool_remote_warning.lua's earlier fixes.
    if h.fbW then panelW = math.min(panelW, h.fbW) end
    if h.fbH then panelH = math.min(panelH, h.fbH) end

    return { dataParams = dataParams, w = panelW, h = panelH,
             titleH = titleH, subH = subH, ageH = ageH,
             tabH = tabH, tabPad = tabPad, rowsH = rowsH,
             uiscale = uiscale }
end

-- Where each pane's panel goes. A kind that supplies `placePanes` owns
-- the answer completely (the escort pair flanks the screen centre and
-- keeps clear of its sibling); every other kind keeps the pre-#1250
-- rule verbatim — anchored at the level's own (mx, my), clamped to the
-- framebuffer so it doesn't open partly off-screen when the player
-- right-clicks near an edge.
local function placePanes(level, measures)
    local h = cargoInventoryPanel.hud
    local kind = levelKind(level)
    if kind.placePanes then
        return kind.placePanes(level, measures, h)
    end
    local out = {}
    for i, m in ipairs(measures) do
        local px, py = level.mx, level.my
        if h.fbW and px + m.w > h.fbW then px = math.max(0, h.fbW - m.w) end
        if h.fbH and py + m.h > h.fbH then py = math.max(0, h.fbH - m.h) end
        out[i] = { x = px, y = py }
    end
    return out
end

local function buildPane(level, pane, view, m, pos)
    local h = cargoInventoryPanel.hud
    local kind = levelKind(level)
    local page = levelPage(level)
    local dataParams = m.dataParams

    destroyPaneElements(pane)
    pane.panelId = panel.new({
        name       = "cargo_inv_panel",
        page       = page,
        x          = pos.x,
        y          = pos.y,
        width      = m.w,
        height     = m.h,
        textureSet = h.boxTexSet,
        color      = { 0.1, 0.1, 0.1, 0.95 },
        tileSize   = 64,
        zIndex     = 130,
        padding    = { top = PANEL_PAD_TOP, bottom = PANEL_PAD_BOT,
                       left = PANEL_PAD_X,  right  = PANEL_PAD_X },
        uiscale    = m.uiscale,
    })
    local pbounds = panel.getContentBounds(pane.panelId)
    local cx = pos.x + pbounds.x
    local cy = pos.y + pbounds.y
    local cw = pbounds.width

    buildTitle(level, pane, cx, cy, view, m.uiscale)

    dataParams.name         = "cargo_inv"
    dataParams.page         = page
    dataParams.font         = h.menuFont
    dataParams.x            = cx
    dataParams.y            = cy + m.titleH + m.subH + m.ageH + 6
    dataParams.width        = cw
    dataParams.height       = m.tabH + m.tabPad + m.rowsH
    if kind.tabs then
        dataParams.tabBottomPadPx = 8  -- literal, matching panelH's gap
    end
    dataParams.rowHeight    = ROW_H
    dataParams.rowPad       = ROW_PAD
    dataParams.iconSize     = ICON_SZ
    dataParams.textPad      = TEXT_PAD
    dataParams.nameRightGap = NAME_RIGHT_GAP
    dataParams.rowFontSize  = 13
    dataParams.weightColor  = ROW_WEIGHT_COL
    dataParams.zBase        = 132
    pane.listId = itemList.new(dataParams)
    -- The widget owns the scroll clamp (only it knows the visible
    -- capacity), so the pane's own durable offset takes its answer
    -- back — otherwise a pane restored against shrunken contents would
    -- keep re-requesting an offset it can never have.
    pane.scroll = itemList.getScrollOffset(pane.listId)
end

-- Build (or rebuild) every pane of one level. `views` is one view per
-- pane, in pane order — read by the caller, because whether a level
-- survives at all is decided by ALL of its panes together.
local function buildLevel(level, views)
    local h = cargoInventoryPanel.hud
    if not h or not levelPage(level) then return end

    local panes = panesOf(level)
    local measures = {}
    for i, pane in ipairs(panes) do
        measures[i] = measurePane(level, pane, views[i])
    end
    local positions = placePanes(level, measures)
    for i, pane in ipairs(panes) do
        buildPane(level, pane, views[i], measures[i],
                  positions[i] or { x = 0, y = 0 })
    end
    applyScrollCapture()
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
                                listDataParams(level, pane, views[j])) then
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
                local age = ageText(views[j])
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
                ageText    = view and ageText(view) or nil,
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
            ageText      = view and ageText(view) or nil,
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
    engine.logInfo("Container window initializing...")
end

function cargoInventoryPanel.shutdown()
    cargoInventoryPanel.closeIfOpen()
    engine.logInfo("Container window shut down")
end

-- Esc closes the DEEPEST level, one per press (requirement 6). Returns
-- true if consumed. Named handle* (not on*) deliberately: this module
-- is engine-loaded, so an on*-named function would also fire directly
-- on every engine broadcast — double-firing on top of init.lua's
-- ordered forward.
function cargoInventoryPanel.handleKeyDown(key)
    if key == "Escape" then
        return cargoInventoryPanel.popLevel()
    end
    return false
end

return cargoInventoryPanel
