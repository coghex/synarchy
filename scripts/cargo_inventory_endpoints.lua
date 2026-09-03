-- Container Window: endpoint owner (#2155)
--
-- Split out of scripts/cargo_inventory_panel.lua, which remains THE
-- container-window manager and the only engine-loaded script of the
-- three. This module is `require`-only: it is NOT registered in
-- scripts/init_loader.lua or scripts/ui/view_teardown.lua and defines
-- no `on*`-named function, because an engine-loaded module double-fires
-- those on top of init.lua's ordered forward.
--
-- What lives here is everything that knows an endpoint is a BUILDING or
-- a UNIT (#1234) and everything that knows an endpoint's contents can
-- be REMEMBERED rather than live (#1237): the ENDPOINTS table, the
-- unit-title precedence, the knowledge/age/weight/empty presentation,
-- the five shared endpoint helpers, and the `endpoint` level-kind
-- descriptor the manager renders.
--
-- What does NOT live here is any panel, label, page or element: this
-- module creates no UI. It also never imports the renderer
-- (scripts/cargo_inventory_render.lua) — the tab strip's style and the
-- row-name colour are single-owned THERE and reach this module as
-- VALUES the manager injects through `setStyle`, which is what keeps
-- the dependency direction acyclic (manager → {endpoints, render},
-- render → endpoints, and nothing back).
--
-- The presentation helpers below are deliberately KIND-BLIND: they
-- branch on the view's own `knowledge` sub-table and never on an
-- endpoint kind or a level kind, which is exactly why
-- scripts/item_contents_panel.lua's `buildingItem` level gets its
-- "as of…" line by handing back the same sub-table for a level kind
-- this module does not own.

local qualityTier = require("scripts.ui.quality_tier")
local scale       = require("scripts.ui.scale")
local itemList    = require("scripts.ui.item_list")

local cargoInventoryEndpoints = {}

-----------------------------------------------------------
-- Injected style (#2155)
--
-- The tab-strip spec and the row-name colour are the RENDERER's
-- constants — it measures and draws against the same numbers — but
-- endpoint TAB POLICY and endpoint LIST PARAMS are this module's. Since
-- this module may not import the renderer, the manager composes the two
-- and hands the values down here once at load. Defaults keep the module
-- usable (and unit-testable) before any injection.
-----------------------------------------------------------
local style = {
    tabSpec      = false,
    rowNameColor = { 1.0, 1.0, 1.0, 1.0 },
}

function cargoInventoryEndpoints.setStyle(opts)
    if type(opts) ~= "table" then return end
    if opts.tabSpec ~= nil then style.tabSpec = opts.tabSpec end
    if opts.rowNameColor ~= nil then style.rowNameColor = opts.rowNameColor end
end

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
-- the level-kind descriptor's transferMenu) — where the pre-#1249
-- building-only `rowMenu` hook hung an immediate
-- `unit.withdrawFromCargo` that required an adjacent selected acolyte
-- and had no unit-endpoint counterpart at all. #1238's "Contents" entry
-- is still appended separately and for every kind, because inspecting a
-- nested container is not a transfer gesture.
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
        -- deliberately absent from this file entirely — and from
        -- scripts/cargo_inventory_panel.lua and
        -- scripts/cargo_inventory_render.lua too, so the split moved
        -- that absence rather than losing it.
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
--
-- Every one of them branches on `view.knowledge` ALONE and never on an
-- endpoint kind or a level kind, which is what lets a level kind this
-- module does not own (item_contents_panel's `buildingItem`) present an
-- "as of…" line simply by supplying the same sub-table.
-----------------------------------------------------------

-- "unknown" / "empty" / "known" for a remembering endpoint, "live" for
-- one that reports the truth. Distinct strings for all four so a
-- presentation key can never conflate never-inspected with known-empty.
local function knowledgeState(view)
    local k = view.knowledge
    if not k then return "live" end
    return k.state or "unknown"
end

cargoInventoryEndpoints.knowledgeState = knowledgeState

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

cargoInventoryEndpoints.formatAge = formatAge

-- The "as of…" line, or nil when there is no observation to date: a
-- live endpoint, a never-inspected container (whose revealedAt is
-- deliberately absent), or an engine with no game clock to compare
-- against. Recomputed on every read rather than cached, which is what
-- makes it ADVANCE as game time passes.
--
-- THE single owner of that string (#2155). The renderer measures the
-- line's reserved height and draws its label from this same answer, and
-- the manager retexts it in place per tick and reports it in dump();
-- a second copy anywhere would let the reserved band and the drawn
-- glyphs disagree, which is the header/list overlap the renderer's
-- `ageLineHeight` comment names.
local function ageText(view)
    local k = view.knowledge
    if not k or type(k.revealedAt) ~= "number" then return nil end
    if knowledgeState(view) == "unknown" then return nil end
    if type(engine.gameTime) ~= "function" then return nil end
    local now = engine.gameTime()
    if type(now) ~= "number" then return nil end
    return "as of " .. formatAge(now - k.revealedAt)
end

cargoInventoryEndpoints.ageText = ageText

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

cargoInventoryEndpoints.emptyText = emptyText

-----------------------------------------------------------
-- The shared endpoint helpers
--
-- The endpoint level's own four questions, answered from an endpoint
-- IDENTITY rather than a level `src`. Re-exported unchanged by
-- scripts/cargo_inventory_panel.lua, which is where every caller
-- outside this module still reaches them: the escort session's level
-- (#1250) is two of these side by side, and reading them through the
-- manager is what makes an escort pane render, refresh, close and
-- descend exactly the way a lone container window does, with no second
-- copy of the ENDPOINTS table to drift.
-----------------------------------------------------------
function cargoInventoryEndpoints.endpointView(endpointKind, id)
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
function cargoInventoryEndpoints.endpointStillThere(endpointKind, id)
    local def = ENDPOINTS[endpointKind]
    if not def or not def.stillThere then return true end
    return def.stillThere(id) and true or false
end

function cargoInventoryEndpoints.endpointChildOf(endpointKind, id, row)
    local def = ENDPOINTS[endpointKind]
    if not def or not def.childOf then return nil end
    return def.childOf(id, row)
end

-- The container window's own tab-strip style. Exposed so an escort pane
-- (#1250) renders a tab strip identical to a lone window's rather than
-- a second copy of these constants that could drift. The constants
-- themselves are the renderer's (#2155); this returns the value the
-- manager injected.
function cargoInventoryEndpoints.endpointTabSpec()
    return style.tabSpec
end

-- The item-list parameters that describe an endpoint's data and
-- presentation policy. Everything the widget needs to group, tab,
-- render and invalidate; bounds are added by the renderer once the
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
-- changes every game second and is refreshed in place by the
-- manager's update() instead of rebuilding the window.
--
-- The row TOOLTIP is #1268's, deliberately bounded to the row's own
-- display text plus the labeled temperature line: quality,
-- condition, weapon and fill detail stay out of this window. It is
-- supplied for EVERY endpoint kind, so a unit endpoint and a
-- building endpoint present temperature identically.
function cargoInventoryEndpoints.endpointListParams(endpointKind, view)
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
        rowColor  = function() return style.rowNameColor end,
        presentationKey = string.format("%s|%s|%s|%.3f/%.3f",
                                        tostring(endpointKind),
                                        tostring(view.title),
                                        knowledgeState(view),
                                        view.stored or -1, view.capacity),
    }
end

-----------------------------------------------------------
-- The `endpoint` level kind (#1238)
--
-- The descriptor the manager renders, in the same shape
-- scripts/item_contents_panel.lua and
-- scripts/transfer_session_panels.lua hand back for theirs. Built fresh
-- so `tabs` picks up whatever style the manager injected.
-----------------------------------------------------------
function cargoInventoryEndpoints.levelKind()
    return {
        panelWidthBase = 460,
        maxRows        = 10,
        tabs           = style.tabSpec,
        view = function(src)
            return cargoInventoryEndpoints.endpointView(src.endpointKind,
                                                        src.id)
        end,
        stillThere = function(src)
            return cargoInventoryEndpoints.endpointStillThere(src.endpointKind,
                                                              src.id)
        end,
        listParams = function(src, view)
            return cargoInventoryEndpoints.endpointListParams(src.endpointKind,
                                                              view)
        end,
        -- The endpoint level's transfer action (#1249): "Retrieve 1" /
        -- "Retrieve all" into the unit the shared selection rule
        -- resolves.
        --
        -- Built from the endpoint IDENTITY alone, so a unit endpoint and
        -- a building endpoint offer the identical gesture — where the
        -- retired `unit.withdrawFromCargo` path was building-only by
        -- construction (that verb takes a BuildingId) and a unit
        -- endpoint's rows therefore had no transfer action at all.
        transferMenu = function(src, row)
            local gestures = require("scripts.transfer_gestures")
            return gestures.retrieveEntries(
                { kind = src.endpointKind, id = src.id }, row)
        end,
        childOf = function(src, row)
            return cargoInventoryEndpoints.endpointChildOf(src.endpointKind,
                                                           src.id, row)
        end,
    }
end

return cargoInventoryEndpoints
