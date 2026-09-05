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
-- Two kinds, one presentation:
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
--
-- Both descend by EXACT INSTANCE IDENTITY along a `path` of instance
-- ids, so two same-def kits inside one toolbox never show each other's
-- contents, and a path that stops resolving closes its level instead of
-- retargeting a sibling (the manager's update() does that).
--
-- The rows are ALREADY GROUPED by defName on the Haskell side, so this
-- host hands them to the widget pre-grouped: the finer stack key the
-- endpoint level uses must not re-split them, and their order (a
-- hashmap enumeration) must not be re-sorted. Both engine reads answer
-- that same grouped shape (Engine.Scripting.Lua.API.Items.Contents), so
-- a live level and a remembered one render identically.
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
        presentationKey = string.format("%s|%s|%s", tostring(view.title),
                                        tostring(view.subtitle),
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
