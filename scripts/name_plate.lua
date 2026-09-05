-- Name Plate (#1104, epic #708)
--
-- The minimal read-only name display that HOSTS the etymology action.
--
-- #1104 requirement 10 puts an etymology action beside the displayed
-- current-world name, on a discovered location's name surface, and on a
-- selected river. None of those display surfaces existed when this
-- landed — world.getIdentity was referenced by no script at all, hud.lua
-- carried no location or world name, and a river had no selection path —
-- so this adds exactly the display needed to host the three actions and
-- nothing more. It is deliberately not a map legend, a gazetteer, or a
-- river browser.
--
-- Three rows, at most:
--   * the current world's name, always (when the page has one);
--   * the selected tile's DISCOVERED location, if it is inside one;
--   * the selected tile's river, if one runs through it.
--
-- The location and river rows follow the player's existing tile
-- selection (world.getSelectedTile), so "selecting a visible river
-- segment" is the selection the player already makes rather than a new
-- interaction. An UNDISCOVERED location never produces a row — the same
-- lifecycle gate the engine's own recurrence eligibility uses — so this
-- surface can never reveal a name the player has not found (requirement
-- 9's no-leak constraint).
--
-- Clicking a row opens scripts/etymology_panel.lua on that entity;
-- clicking the row of the entity already open closes it again.
--
-- Gameplay surface: reached through hud's manual forward, never
-- responsive.register.

local namePlate = package.loaded["scripts.name_plate"] or {}
package.loaded["scripts.name_plate"] = namePlate

local button = require("scripts.ui.button")
local scale  = require("scripts.ui.scale")

local ROW_W_BASE = 240
local ROW_H_BASE = 22
local ROW_FONT   = 12
local ROW_GAP    = 4
local Z_PLATE    = 120

namePlate.state = namePlate.state or {
    buttonIds = {},
    rows      = {},   -- { kind, id, name } in display order
    selection = nil,  -- last {gx, gy} the rows were built for
}

namePlate.hud = nil

function namePlate.setup(opts)
    namePlate.hud = opts
end

-----------------------------------------------------------
-- What the plate currently has to show
-----------------------------------------------------------

local function worldRow(worldId)
    if type(world) ~= "table" or type(world.getIdentity) ~= "function" then
        return nil
    end
    local ok, ident = pcall(world.getIdentity, worldId)
    if not ok or type(ident) ~= "table" or not ident.name then return nil end
    return { kind = "world", id = nil, name = ident.name }
end

-- Half of the page's cylindrical u-wrap period, in tiles — the one
-- alias step Location.Bounds.seamAliases shifts a box by. Zero for a
-- page that does not wrap, is not live, or an engine too old to answer,
-- which collapses the containment below to the plain Cartesian test.
--
-- PAGE-SCOPED on purpose: world.listPlacedLocations and
-- world.getSelectedTile are both read for this same worldId, and two
-- live pages can have different wrap sizes, so an active-page fallback
-- or a nominal default would measure the selection against the wrong
-- world.
local function seamStep(worldId)
    if type(world) ~= "table" or type(world.getWrapWidth) ~= "function" then
        return 0
    end
    local ok, w = pcall(world.getWrapWidth, worldId)
    if not ok or type(w) ~= "number" or w <= 0 then return 0 end
    return math.floor(w / 2)
end

-- Inclusive containment against a location's stored bounds, seam-aware:
-- Location.Bounds.boundsContainsPoint's exact topology — the box itself
-- plus one cylindrical image each way along (+u, -v).
--
-- Location bounds are cylindrical while the selected tile arrives in the
-- canonical storage frame (#1175), so a location straddling the U seam
-- is named by a tile that the raw comparison alone rejects even though
-- it is physically inside (#1264). Identity away from the seam and for a
-- non-wrapping page, where step is 0 and the loop runs once.
local function boundsContain(b, step, gx, gy)
    local lo, hi = 0, 0
    if step > 0 then lo, hi = -1, 1 end
    for k = lo, hi do
        local dx, dy = k * step, -k * step
        if gx >= b.min_x + dx and gx <= b.max_x + dx
           and gy >= b.min_y + dy and gy <= b.max_y + dy then
            return true
        end
    end
    return false
end

-- The DISCOVERED location containing a tile, if any. Lifecycle is the
-- gate, exactly as the engine's own eligibility rule has it: an
-- undiscovered ruin is invisible to this surface even though the record
-- exists in engine state.
local function locationRowAt(worldId, gx, gy)
    if type(world) ~= "table"
       or type(world.listPlacedLocations) ~= "function" then
        return nil
    end
    local ok, list = pcall(world.listPlacedLocations, worldId)
    if not ok or type(list) ~= "table" then return nil end
    local step = seamStep(worldId)
    for _, e in ipairs(list) do
        local b = e.bounds
        if e.instance_id and b and e.lifecycle ~= "unknown"
           and e.lifecycle ~= "hinted"
           and boundsContain(b, step, gx, gy) then
            return { kind = "location", id = e.instance_id,
                     name = e.name or "?" }
        end
    end
    return nil
end

-- The river running through a tile, resolved through #1102's stable
-- identity by the engine (world.getRiverAt). Exposes ONLY the selected
-- river: there is deliberately no list of the others.
local function riverRowAt(gx, gy)
    if type(world) ~= "table" or type(world.getRiverAt) ~= "function" then
        return nil
    end
    local ok, r = pcall(world.getRiverAt, gx, gy)
    if not ok or type(r) ~= "table" or not r.id then return nil end
    return { kind = "river", id = r.id, name = r.name or "(unnamed river)" }
end

local function selectedTile(worldId)
    if type(world) ~= "table" or type(world.getSelectedTile) ~= "function" then
        return nil
    end
    local ok, t = pcall(world.getSelectedTile, worldId)
    if not ok or type(t) ~= "table" or not t.gx or not t.gy then return nil end
    return t
end

-- The rows the plate should currently show, in display order.
function namePlate.computeRows()
    local h = namePlate.hud
    local worldId = h and h.worldId
    local rows = {}
    local w = worldRow(worldId)
    if w then rows[#rows + 1] = w end
    local t = selectedTile(worldId)
    if t then
        local loc = locationRowAt(worldId, t.gx, t.gy)
        if loc then rows[#rows + 1] = loc end
        local riv = riverRowAt(t.gx, t.gy)
        if riv then rows[#rows + 1] = riv end
    end
    return rows, t
end

-----------------------------------------------------------
-- Rendering
-----------------------------------------------------------

local function destroyButtons()
    local s = namePlate.state
    for _, id in ipairs(s.buttonIds) do button.destroy(id) end
    s.buttonIds = {}
end

local function openRow(row)
    local ep = require("scripts.etymology_panel")
    local curKind, curId = ep.currentTarget()
    if curKind == row.kind and curId == row.id then
        ep.closeIfOpen()
    else
        ep.openFor(row.kind, row.id)
    end
end

local function build()
    local s = namePlate.state
    local h = namePlate.hud
    destroyButtons()
    if not h or not h.page then return end
    -- 0x0 minimize: nothing to lay out against (requirement 11).
    if not h.fbW or not h.fbH or h.fbW <= 0 or h.fbH <= 0 then return end

    local uiscale = scale.get()
    local rowH = math.floor(ROW_H_BASE * uiscale)
    local gap  = math.floor(ROW_GAP * uiscale)
    local x    = math.floor(8 * uiscale)
    local y    = math.floor(8 * uiscale)

    for i, row in ipairs(s.rows) do
        local ry = y + (i - 1) * (rowH + gap)
        if ry + rowH <= h.fbH then
            local id = button.new({
                name       = "name_plate_" .. row.kind,
                text       = row.name,
                page       = h.page,
                x          = x,
                y          = ry,
                -- button.new scales these itself, so the framebuffer
                -- cap is applied to the BASE: a narrow, high-scale but
                -- still-supported combination must not produce a row
                -- wider than the screen.
                width      = math.min(ROW_W_BASE,
                                      math.floor(h.fbW / math.max(uiscale, 0.01))),
                height     = ROW_H_BASE,
                fontSize   = ROW_FONT,
                textureSet = h.boxTexSet,
                font       = h.menuFont,
                uiscale    = uiscale,
                zIndex     = Z_PLATE,
                onClick    = function() openRow(row) end,
            })
            s.buttonIds[#s.buttonIds + 1] = id
        end
    end
end

-- Rebuild the plate's rows from live state. Cheap enough to run on the
-- HUD tick because every source is an ordinary read, and the rebuild
-- only touches UI when the row SET actually changed.
function namePlate.refresh()
    local s = namePlate.state
    local rows, tile = namePlate.computeRows()
    local changed = #rows ~= #s.rows
    if not changed then
        for i, r in ipairs(rows) do
            local old = s.rows[i]
            if not old or old.kind ~= r.kind or old.id ~= r.id
               or old.name ~= r.name then
                changed = true
                break
            end
        end
    end
    s.rows = rows
    s.selection = tile
    if changed then
        build()
        -- A row that disappeared takes its open panel with it: the
        -- player deselected the thing being explained, so leaving the
        -- panel up would describe something no longer pointed at.
        local ep = require("scripts.etymology_panel")
        local kind, id = ep.currentTarget()
        if kind then
            local stillThere = false
            for _, r in ipairs(rows) do
                if r.kind == kind and r.id == id then stillThere = true end
            end
            if not stillThere then ep.closeIfOpen() end
        end
    end
end

function namePlate.update(dt)
    if not namePlate.hud or not namePlate.hud.page then return end
    namePlate.refresh()
end

-----------------------------------------------------------
-- Lifecycle
-----------------------------------------------------------

-- Called by hud AFTER its own rebuild, so this never reads stale hud
-- geometry (the ordering popup/unit_info_v2 already follow).
function namePlate.reflow()
    build()
end

function namePlate.onFramebufferResize(width, height)
    if not width or not height or width <= 0 or height <= 0 then return end
    build()
end

function namePlate.clear()
    destroyButtons()
    namePlate.state.rows = {}
    namePlate.state.selection = nil
end

-- Read-only introspection, same convention as the panel's own dump.
function namePlate.dump()
    local s = namePlate.state
    local out = { rowCount = #s.rows, rows = {} }
    for i, r in ipairs(s.rows) do
        local bid = s.buttonIds[i]
        local eh = bid and button.getElementHandle(bid)
        local info = eh and UI.getElementInfo(eh)
        out.rows[i] = {
            kind = r.kind, id = r.id, name = r.name, handle = eh,
            x = info and info.x, y = info and info.y,
            width = info and info.width, height = info and info.height,
        }
    end
    return out
end

function namePlate.init(scriptId)
    engine.logDebug("Name plate initializing...")
end

function namePlate.shutdown()
    namePlate.clear()
    engine.logDebug("Name plate shut down")
end

return namePlate
