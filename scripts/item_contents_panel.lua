-- Item Contents — the item-container LEVEL of the container-window
-- stack.
--
-- Since #1238 (D-13) this module no longer owns a window lifecycle. It
-- owns the two item-container LEVEL KINDS the window manager
-- (scripts/cargo_inventory_panel.lua) draws -- through the shared
-- renderer scripts/cargo_inventory_render.lua since #2155 -- and
-- nothing else: no
-- page, no panel, no singleton, no Escape handling, no per-tick
-- refresh. Opening, closing, modality, resize restore and the nesting
-- path all belong to the manager, which is what makes the one-window
-- rule per nesting LEVEL true rather than aspirational — before this,
-- this popup and the cargo popup were independent singletons that
-- could both be open at once.
--
-- Three kinds, one presentation:
--
--   unitItem      — a container a unit CARRIES, WEARS or has EQUIPPED.
--                   LIVE contents through unit.getItemContents, which
--                   searches all three of those locations (#1238) —
--                   the unit-info inventory list merges them and its
--                   context menu offers "Contents" for a carried OR
--                   equipped container, so the read has to cover the
--                   same set.
--   buildingItem  — a container stored inside a storage building. The
--                   player's REMEMBERED contents through
--                   building.getRememberedItemContents: never a live
--                   read of building storage, never a knowledge write,
--                   and carrying the parent record's own `revealedAt`
--                   so the level shows the same "as of…" age as the
--                   window it was opened from.
--   portableItem  — a PORTABLE container the player knows about: a
--                   crate on the floor, opened with no unit involved
--                   (#2527, epic #1231 PLC-17). Entirely the player's
--                   REMEMBERED view, through PLC-7's item-keyed
--                   knowledge layer: `item.getContainerKnowledge` for
--                   the crate's state, its remembered whole mass and
--                   its LIVE internal capacity, and
--                   `item.getRememberedItemContents` for the grouped
--                   rows at any depth. Never a live contents read and
--                   never a knowledge write at all — the crate's four
--                   states (never-inspected, weight-only, known-empty,
--                   known-contents) are presentation, not lifecycle.
--
-- The ONE knowledge WRITE in this module is D-26's, and it is the
-- `unitItem` kind's `onOpen`: opening the live level of a container a
-- PLAYER-COMMANDABLE unit is holding records one contents observation,
-- because the unit is holding it open and the player has seen inside.
-- It fires once per real open — never on a layout rebuild, a per-tick
-- refresh, a scroll, a tab change or a descent into a nested container
-- — and never for a unit the player does not command. Every remembered
-- level, at any depth, writes nothing.
--
-- All three descend by EXACT INSTANCE IDENTITY along a `path` of
-- instance ids, so two same-def kits inside one toolbox never show each
-- other's contents, and a path that stops resolving closes its level
-- instead of retargeting a sibling (the manager's update() does that).
--
-- The rows are ALREADY GROUPED by defName on the Haskell side, so this
-- host hands them to the widget pre-grouped: the finer stack key the
-- endpoint level uses must not re-split them, and their order (a
-- hashmap enumeration) must not be re-sorted. All three engine reads
-- answer that same grouped shape
-- (Engine.Scripting.Lua.API.Items.Contents' pushGroupedContents), so a
-- live level and a remembered one render identically. That is why the
-- portable base level reads `item.getRememberedItemContents` for its
-- rows rather than `item.getContainerKnowledge`'s own `items`: the
-- latter are INDIVIDUAL instance projections, not grouped rows, and
-- handing them straight to the widget would break `preGrouped`.
--
-- A level here is RENDER-ONLY (D-5): not a transfer endpoint, and it
-- offers no transfer operation. It supplies no `transferMenu` at all —
-- the manager's own "Contents" entry (inspection, not transfer) is
-- what lets a container inside a container open the next level.
--
-- It owns no elements, so it has no setup(): the window manager holds
-- the page and assets every level is drawn with, and hud.createUI
-- configures that one.
--
-- Public API:
--   levelKinds()                              — the manager's lookup
--   openFor(uid, defName, mx, my, instanceId[, displayName])
--                                             — the unit-info
--                                               "Contents" gesture:
--                                               opens at the BASE level
--   openForGround(instanceId, mx, my[, displayName[, defName]])
--                                             — the ground-item
--                                               "Contents" gesture
--                                               (#2527): the BASE
--                                               portable level, no unit
--   closeIfOpen() / isOpen()                  — stack delegates, kept
--                                               so existing callers and
--                                               teardown paths still
--                                               read naturally
--
-- Engine script hooks: setup / init / shutdown. There is deliberately
-- no update() and no handleKeyDown(): per-tick refresh and the Escape
-- cascade are the manager's, and a second handler here would close two
-- levels per keypress.
--
-- Registered in package.loaded so init.lua's key handler, hud setup,
-- and unit_info_v2's right-click all see the same instance even though
-- engine.loadScript uses dofile.

local itemContentsPanel =
    package.loaded["scripts.item_contents_panel"] or {}
package.loaded["scripts.item_contents_panel"] = itemContentsPanel

local scale    = require("scripts.ui.scale")

-- The manager is required lazily: it requires this module too (for
-- levelKinds), and a top-level require in both directions is a load
-- cycle.
local function manager()
    return require("scripts.cargo_inventory_panel")
end

-- The single owner of the knowledge presentation (#2155): the
-- stored-weight header and the per-state empty text. Required lazily
-- for the same reason the manager is — the manager injects that
-- module's style at load, so reaching it on demand keeps this file free
-- of any assumption about which of the three loaded first. It never
-- requires this one, so the edge is acyclic.
local function endpoints()
    return require("scripts.cargo_inventory_endpoints")
end

-----------------------------------------------------------
-- Layout. The window CHROME (panel padding, header baselines, row
-- metrics, the "as of…" line) is the manager's and shared with the
-- endpoint level, so all that differs here is how wide the window is
-- and how many rows it shows before scrolling.
-----------------------------------------------------------
local PANEL_W_BASE   = 420
local MAX_ROWS       = 12
local ROW_NAME_COL   = { 1.0, 1.0, 1.0, 1.0 }
local EMPTY_COL      = { 0.7, 0.7, 0.7, 1.0 }

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

-- The container's own display name. The opener knows it (it is the row
-- the player right-clicked), which is the only source that works for an
-- EQUIPPED container, an accessory, or one nested inside another — the
-- pre-#1238 loose-inventory scan could not name any of those. That scan
-- survives as the fallback for a caller that supplies nothing.
local function containerTitle(src)
    if src.displayName and src.displayName ~= "" then return src.displayName end
    if src.uid and src.defName then
        for _, it in ipairs(unit.getInventory(src.uid) or {}) do
            if it.defName == src.defName then
                return it.displayName or src.defName
            end
        end
    end
    return src.defName or "Contents"
end

-- Total piece count across all groups — the subtitle, unchanged.
local function pieceCountText(rows)
    local pieces = 0
    for _, r in ipairs(rows) do pieces = pieces + (r.count or 1) end
    return (pieces == 1) and "1 item" or (pieces .. " items")
end

-- The item-list parameters describing an item-container level's data
-- and presentation policy, shared by both kinds. No right-click
-- TRANSFER action: this level is read-only, and the manager attaches
-- the inspection entry itself.
local function listParams(_src, view)
    return {
        items      = view.contents,
        preGrouped = true,
        uiscale    = scale.get(),
        emptyText  = view.emptyText,
        emptyColor = EMPTY_COL,
        rowIcon    = function(g)
            -- Unlike the endpoint level, this API can report a missing
            -- icon as a negative handle.
            if g.iconTex and g.iconTex >= 0 then return g.iconTex end
            return nil
        end,
        rowName = function(g)
            local n = g.displayName or g.defName or "?"
            if (g.count or 1) > 1 then
                n = string.format("%s ×%d", n, g.count)
            end
            return n
        end,
        rowWeightText = function(g)
            -- Per-item TRUE mass (empty + fill + nested contents, from
            -- itemTotalWeight) × count.
            return string.format("%.2f kg", (g.weight or 0) * (g.count or 1))
        end,
        rowColor = function() return ROW_NAME_COL end,
        rowTooltip = function(g)
            local hintLines = {}
            if g.fill and g.fill > 0 then
                hintLines[#hintLines + 1] = string.format("Holds: %.2f", g.fill)
            end
            if g.condition and g.condition > 0 and g.condition < 100 then
                hintLines[#hintLines + 1] =
                    string.format("Condition: %.0f%%", g.condition)
            elseif g.condition and g.condition <= 0 then
                hintLines[#hintLines + 1] = "Broken"
            end
            if #hintLines == 0 then return nil end
            return {
                text = g.displayName or g.defName or "?",
                hint = table.concat(hintLines, "\n"),
            }
        end,
        -- The header/subtitle the widget cannot see, so a piece count
        -- or a title change rebuilds the level.
        -- The knowledge STATE rides in the key alongside the
        -- observation stamps (#2527): a crate that goes from
        -- never-inspected to weight-only gains a header and an empty
        -- text without gaining a single row, and a key that watched
        -- only `revealedAt` would leave the old wording on screen.
        presentationKey = string.format("%s|%s|%s|%s|%s",
                                        tostring(view.title),
                                        tostring(view.subtitle),
                                        tostring(view.emptyText),
                                        tostring(view.knowledge
                                                 and view.knowledge.state),
                                        tostring(view.knowledge
                                                 and view.knowledge.revealedAt)),
    }
end

-- Extend a level's descent path by one instance id, without mutating
-- the parent's own path (the parent level is still open and still
-- addressing itself with it).
local function extendPath(path, instanceId)
    local out = {}
    for i, v in ipairs(path or {}) do out[i] = v end
    out[#out + 1] = instanceId
    return out
end

-----------------------------------------------------------
-- The two level kinds
-----------------------------------------------------------
local KINDS = {
    unitItem = {
        panelWidthBase = PANEL_W_BASE,
        maxRows        = MAX_ROWS,
        tabs           = false,
        -- nil when the unit is gone, no longer holds that container, or
        -- the nested path no longer resolves. The manager closes this
        -- level and every deeper one on nil, which is the whole
        -- reconciliation rule.
        view = function(src)
            local rows = unit.getItemContents(src.uid, src.defName,
                                              src.instanceId, src.path)
            if not rows then return nil end
            return {
                title     = containerTitle(src),
                subtitle  = pieceCountText(rows),
                contents  = rows,
                emptyText = "(empty)",
            }
        end,
        listParams = listParams,
        childOf = function(src, row)
            return { kind = "unitItem", uid = src.uid, defName = src.defName,
                     instanceId = src.instanceId,
                     path = extendPath(src.path, row.instanceId),
                     displayName = row.displayName }
        end,
        -- D-26 (#2527): opening this level IS a contents observation.
        -- The unit is holding the container open and the player is
        -- looking inside, so PLC-7's record gets one write and the
        -- "as of…" age it feeds stays honest.
        --
        -- Four conditions, each one load-bearing:
        --
        --   * The manager never calls this for reason == "layout", so a
        --     resize's destroy-and-rebuild pass writes nothing. Nor is
        --     it reached by refreshLevel, the per-tick staleness
        --     rebuild, a scroll or a tab change: all of those go
        --     straight to buildLevel and never re-open a level.
        --   * A DESCENT writes nothing (`path` non-empty). A container
        --     nested inside an observed one gets no record of its own
        --     until it is itself observed — the same rule
        --     item.observeContainerContents documents — and the level
        --     the player descended FROM has already recorded the whole
        --     tree it is looking at.
        --   * Only an EXACT instance is observed. `openFor` still
        --     supports the by-defName fallback with no instance id
        --     (#67's pre-instance callers), and that path names no
        --     particular crate, so it renders without writing rather
        --     than guessing which of two same-def kits to stamp.
        --   * The unit must be PLAYER-COMMANDABLE. Reusing the transfer
        --     endpoint's own eligibility read is deliberate: that is
        --     already this window's one definition of "a unit the
        --     player commands" (the `unit` endpoint kind gates its whole
        --     view on it), so a hostile's container can never be
        --     observed by looking at it.
        --
        -- `view` has already resolved for this src by the time the
        -- manager calls this — a refused open never reaches here — so a
        -- write means the unit demonstrably holds that exact instance.
        onOpen = function(src)
            if #(src.path or {}) > 0 then return end
            if type(src.instanceId) ~= "number" or src.instanceId <= 0 then
                return
            end
            local info = unit.transferEndpointInfo({ kind = "unit",
                                                     id = src.uid })
            if not info or info.eligible ~= true then return end
            item.observeContainerContents(src.instanceId)
        end,
    },

    buildingItem = {
        panelWidthBase = PANEL_W_BASE,
        maxRows        = MAX_ROWS,
        tabs           = false,
        -- The `knowledge` sub-table is what gives this level the
        -- manager's "as of…" line (#1237's presentation, reused
        -- verbatim). Its `revealedAt` is the PARENT RECORD's, because
        -- that is genuinely when this snapshot was taken — a nested
        -- container was never observed separately.
        view = function(src)
            local res = building.getRememberedItemContents(src.bid, src.path)
            if not res then return nil end
            local rows = res.items or {}
            return {
                title     = containerTitle(src),
                subtitle  = pieceCountText(rows),
                contents  = rows,
                emptyText = "(empty)",
                knowledge = { state = "known", revealedAt = res.revealedAt },
            }
        end,
        listParams = listParams,
        childOf = function(src, row)
            return { kind = "buildingItem", bid = src.bid,
                     path = extendPath(src.path, row.instanceId),
                     displayName = row.displayName }
        end,
    },

    -- A PORTABLE container the player knows about (#2527, PLC-17),
    -- addressed by the crate's own `instanceId` — the key PLC-7's
    -- session-scoped record is filed under, which is why this level
    -- needs no owner at all: no unit, no building, no page. That is
    -- exactly what lets a ground crate have a window.
    --
    -- Two reads, and a deliberate asymmetry between the BASE level and
    -- a nested one:
    --
    --   base (`path` empty) — `item.getContainerKnowledge` answers the
    --       crate's STATE, its remembered whole mass and its LIVE
    --       internal capacity, and it answers for ANY id, so a crate
    --       nobody has ever touched still opens and says so. Absent
    --       rows are therefore NOT a reason to close: never-inspected
    --       and weight-only are legitimate things to be looking at, and
    --       the empty text says which.
    --   nested (`path` non-empty) — `item.getRememberedItemContents`
    --       descends the ROOT observation's own stored copies by exact
    --       instance identity, and nil CLOSES the level and every
    --       deeper one. A nested crate has no record of its own, so
    --       asking the map about its id would answer "unknown" about a
    --       container the player is demonstrably looking into; and a
    --       forgotten or re-observed root legitimately invalidates
    --       every path beneath it.
    --
    -- Both carry the ROOT observation's `revealedAt`, for the same
    -- reason `buildingItem` carries the parent record's: a nested
    -- container was never observed separately, so its age IS the
    -- snapshot's.
    --
    -- Never a live contents read, and never a knowledge write at any
    -- depth (D-7): opening a remembered level must not change what is
    -- remembered. There is no `onOpen` here at all, which is what makes
    -- that true by construction rather than by a test.
    portableItem = {
        panelWidthBase = PANEL_W_BASE,
        maxRows        = MAX_ROWS,
        tabs           = false,
        view = function(src)
            local path = src.path or {}
            if #path > 0 then
                local res = item.getRememberedItemContents(src.instanceId,
                                                           path)
                if not res then return nil end
                local rows = res.items or {}
                return {
                    title     = containerTitle(src),
                    subtitle  = pieceCountText(rows),
                    contents  = rows,
                    emptyText = "(empty)",
                    knowledge = { state = "known",
                                  revealedAt = res.revealedAt },
                }
            end
            local k = item.getContainerKnowledge(src.instanceId)
            if not k then return nil end
            local res = item.getRememberedItemContents(src.instanceId)
            local view = {
                title    = containerTitle(src),
                contents = (res and res.items) or {},
                -- The crate's WHOLE remembered mass against its LIVE
                -- internal capacity: two separate facts, never a ratio
                -- (see cargo_inventory_endpoints.weightText). Either
                -- may be absent — a never-weighed crate has no mass to
                -- report and an unlocatable one has no capacity — and
                -- an absence is rendered as such rather than as 0.
                weightLabel   = "Weight",
                weightMeasure = "whole",
                stored   = k.storedWeight,
                capacity = k.capacity,
                knowledge = { state      = k.state or "unknown",
                              revealedAt = k.revealedAt,
                              weighedAt  = k.weighedAt },
            }
            view.subtitle  = endpoints().weightText(nil, view)
            view.emptyText = endpoints().emptyText(view)
            return view
        end,
        listParams = listParams,
        -- A descent stays rooted at the crate that owns the RECORD and
        -- extends the path, because that is the only observation there
        -- is: the nested container's own id is a step inside it, never
        -- a second record to address.
        childOf = function(src, row)
            return { kind = "portableItem", instanceId = src.instanceId,
                     defName = src.defName,
                     path = extendPath(src.path, row.instanceId),
                     displayName = row.displayName }
        end,
    },
}

function itemContentsPanel.levelKinds()
    return KINDS
end

-----------------------------------------------------------
-- Entry point
-----------------------------------------------------------

-- The unit-info inventory row's "Contents" gesture. An EXTERNAL
-- request targets the BASE level (requirement: a container-row request
-- targets its owning level plus one; everything else starts over), so
-- this replaces whatever stack was open.
--
-- instanceId (optional) targets the EXACT container the player clicked,
-- so two same-def kits don't show each other's contents (#67). Falls
-- back to first-by-defName when nil.
function itemContentsPanel.openFor(uid, defName, mx, my, instanceId, displayName)
    if not uid or not defName then return false end
    return manager().openLevel(
        { kind = "unitItem", uid = uid, defName = defName,
          instanceId = instanceId, path = {}, displayName = displayName },
        mx, my, 0)
end

-- The ground-item "Contents" gesture (#2527,
-- scripts/init_context_menu_item.lua). Like openFor above, an EXTERNAL
-- request targets the BASE level and replaces whatever stack was open.
--
-- Takes the crate's own INSTANCE id, never the ground id the menu was
-- hit-tested with: a ground id is page-local and the active page can
-- change between a menu opening and its entry firing, while an instance
-- id names one crate for the life of the session. The caller resolves
-- one to the other while it still has the row in hand.
--
-- NO unit is involved and NOTHING is written: a remembered level is a
-- pure read at every depth, so this opens in all four knowledge states
-- — including a crate the player has never touched, which opens and
-- says exactly that.
function itemContentsPanel.openForGround(instanceId, mx, my, displayName,
                                         defName)
    if type(instanceId) ~= "number" or instanceId <= 0 then return false end
    return manager().openLevel(
        { kind = "portableItem", instanceId = instanceId, path = {},
          defName = defName, displayName = displayName },
        mx, my, 0)
end

-- Delegates. Kept because the teardown registry, the loader and
-- existing callers all speak this vocabulary; the stack is the one
-- thing that actually holds state.
function itemContentsPanel.closeIfOpen()
    manager().closeIfOpen()
end

function itemContentsPanel.isOpen()
    return manager().isOpen()
end

-----------------------------------------------------------
-- Engine script hooks
-----------------------------------------------------------
function itemContentsPanel.init(scriptId)
    engine.logDebug("Item contents level initializing...")
end

function itemContentsPanel.shutdown()
    engine.logDebug("Item contents level shut down")
end

return itemContentsPanel
