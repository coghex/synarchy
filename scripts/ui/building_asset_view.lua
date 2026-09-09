-- Building asset viewer for --preview buildings/<name> (#888, Phase 4 of
-- the browser epic #427; #2492 adds the declared lifecycle/facing
-- matrix, BDA-4).
--
-- Owns everything to the RIGHT of scripts/ui/asset_browser.lua's list.
-- What that is depends on which CLASS of row is selected, and the two
-- are deliberately different because they answer different questions:
--
--   * a DECLARED row (a lifecycle role, or the static sprite) — the
--     enlarged view of one camera facing, plus a four-cell facing strip
--     beneath it in camera order south, west, north, east. This is the
--     inspection surface BDA-4 exists for: an art reviewer has to see
--     all four declared views of a role at once, and see at a glance
--     which of them do not exist yet.
--   * a RAW filesystem row — exactly what #888 always drew: ONE sprite
--     fitted to the whole panel, no facing model at all. A raw row is a
--     directory or a loose PNG; it has no declared facings to show, and
--     inventing some would be the viewer lying about the asset tree.
--
-- The row MODEL (membership, ordering, identities, which directories are
-- animations, frame order, fps/loop, the declared matrix, every
-- missing-cell verdict, and the default selection) is resolved pre-boot
-- by Engine.Preview.Building / .BuildingMatrix and arrives verbatim
-- through engine.getPreviewBrowse() — nothing here re-derives it. In
-- particular this module never decides whether a path exists: it is told.
--
-- Playback contract (#888 Requirement 1 + its review amendment, #2492
-- requirements 14-15):
--   * ONE wall clock per selected ROW, so the tick rate only affects
--     smoothness, never which frame is correct. Every facing cell of a
--     declared row computes its own index from that SAME elapsed value,
--     so the strip stays phase-aligned with the enlarged view.
--   * Selecting a different ROW resets the clock; selecting a different
--     FACING does not — the strip keeps playing, and the zoom
--     multiplier, list selection and scroll are untouched.
--   * A resize (setPanel) deliberately preserves the phase too.
--   * End-of-clip policy (#1833): the preview ALWAYS replays. Frame
--     N-1 is followed, after its own normal duration, by frame 0
--     again, indefinitely — for every clip, whatever its authored
--     `loop` says, which matters most here because buildings default
--     to loop=false (Engine.Asset.YamlBuildings' own default), unlike
--     the units viewer's loop=true. The wrap lives in the index
--     computation, never in the clock: rowStart is not restarted at
--     a cycle boundary, which is what keeps the phase across a resize.
--     The source `loop` value is still reported verbatim by dump()
--     (below); only gameplay (Building.Visual.pickBuildingFrame,
--     Building.Destruction.destructionFrame) still clamps and expires.
--   * A STATIC row — the declared sprite, or a raw static entry — has
--     no active playback at all: one frame, no clock advance.
--
-- Missing cells (#2492 requirement 7). A cell the engine reported
-- `missing` is DIAGNOSTIC, and the two rules that makes are absolute:
-- its declared path is never requested (requestTexture is not called for
-- it at all, which is what keeps trimmed loading exact even when a
-- definition points outside its own folder), and it never borrows a path
-- from another facing, role, raw row or the static sprite. It draws a
-- textureless marker instead — text on the existing font, so the marker
-- itself adds no texture load either — and the view still reaches
-- ready: a diagnostic is a TERMINAL state, not a load that never
-- finishes.
--
-- Zoom (#1907): the enlarged sprite renders at the owner's session
-- multiplier times its fit to getZoomRegion(). For a declared row that
-- region is the enlarged sub-rect ABOVE the facing strip, the same rule
-- the units viewer uses for its direction row; for a raw row, with no
-- strip to exclude, it stays the whole panel. This view never resets the
-- multiplier -- a building is ONE preview object, so a row change and a
-- facing change both preserve it.
local scale = require("scripts.ui.scale")
local previewZoom = require("scripts.ui.preview_zoom")

local buildingAssetView = {}

local views = {}
local nextId = 1

-- Camera order, and the captions the strip shows. Kept here rather than
-- read off the payload so a cell whose engine-side facing key were ever
-- misspelled draws with its raw key instead of silently vanishing.
local FACING_ORDER = { "south", "west", "north", "east" }
local FACING_SHORT = { south = "S", west = "W", north = "N", east = "E" }

local CELL_CALLBACK = "onPreviewFacingClick"

-- The marker a missing cell and a missing enlarged view draw. Text, not
-- a sprite: a textureless element cannot add a texture request, which is
-- what lets a diagnostic selection stay inside the trimmed-loading
-- allowance.
local MISSING_MARK = "X"

-- Must stay identical to Engine.Preview.BuildingMatrix.previewFrameIndexAt
-- (and Engine.Preview.Unit.frameIndexAt, and
-- scripts/ui/unit_animation_view.lua's copy) — the probe cross-checks the
-- dump's reported index against wall time, so a divergence here shows up
-- as a failing gate rather than silently. srcLoop is the entry's AUTHORED
-- loop value; the preview replays either way (#1833), so it is taken and
-- deliberately never read, exactly as the Haskell original takes and
-- ignores it.
local function frameIndexAt(srcLoop, fps, frameCount, elapsed)
    if frameCount <= 1 then return 0 end
    local rate = math.max(0, fps or 0)
    local raw = math.floor(math.max(0, elapsed or 0) * rate)
    return raw % frameCount
end

-- Fit (w,h) inside (boxW,boxH) preserving aspect ratio, CENTERED, at
-- 'multiplier' times the fitted scale — the same rule (and since #1907
-- the same implementation) previewManager and the units viewer use.
-- Nearest-neighbour is forced session-wide by previewManager.init.
--
-- The facing STRIP passes multiplier 1 deliberately: #1907 zooms the
-- enlarged view only, and cells keep their existing fixed sizing.
local function fitRect(box, w, h, multiplier)
    return previewZoom.fitRect(box, w, h, multiplier or previewZoom.MAX)
end

-----------------------------------------------------------
-- Row model
--
-- A row is one of the two classes described in the header. Both are
-- normalized here into the same shape so playback, geometry and the dump
-- have one code path; `facings` is nil for a raw row, which is what every
-- facing-aware branch tests.
-----------------------------------------------------------

local function isDeclared(row)
    return row ~= nil and row.kind ~= "filesystem"
end

-- The four cells of a declared row, in camera order, whatever order the
-- payload happened to arrive in. A cell the payload omits entirely is
-- synthesized as missing rather than skipped: the strip always shows four
-- facings, because "this facing has no art" is exactly the fact the strip
-- exists to show.
local function facingCells(row)
    local byKey = {}
    for _, c in ipairs((row.declared or {}).cells or {}) do
        byKey[c.facing] = c
    end
    local out = {}
    for i, key in ipairs(FACING_ORDER) do
        local c = byKey[key]
        out[i] = {
            facing = key,
            paths = c and c.paths or {},
            missing = (c == nil) or (c.missing == true),
            missingReason = c and c.missingReason
                or (c == nil and "absent" or nil),
            legacy = c ~= nil and c.legacy == true,
        }
    end
    return out
end

local function findCell(v, facing)
    for _, c in ipairs(v.cells) do
        if c.facing == facing then return c end
    end
    return nil
end

-- South when it exists, else the first cell. A declared row always has
-- four cells, so this only ever falls through for a malformed payload.
local function resolveFacing(v, wanted)
    if wanted and findCell(v, wanted) then return wanted end
    if findCell(v, "south") then return "south" end
    local first = v.cells[1]
    return first and first.facing or nil
end

-----------------------------------------------------------
-- Geometry
-----------------------------------------------------------

-- Split the panel into the enlarged region (top) and the facing strip
-- (bottom), mirroring the units viewer's own layout so the two panes
-- cannot drift onto different arithmetic. Every dimension is floored to
-- at least 1 so a degenerate panel (a heavily shrunk preview window)
-- still produces valid, never inverted, geometry — the #748 responsive
-- convention.
--
-- A RAW row has no strip, so its enlarged region IS the panel — which
-- is also what keeps #1907's existing whole-panel building zoom true for
-- exactly the rows it was written about.
local function layout(v)
    local p = v.panel
    if not isDeclared(v.row) or #v.cells == 0 then
        return {
            gap = 0, labelH = 0, cellSize = 0, rowX = p.x, rowY = p.y,
            enlarged = { x = p.x, y = p.y,
                         width = math.max(1, p.width),
                         height = math.max(1, p.height) },
        }
    end

    local uiscale = v.uiscale
    local gap = math.max(2, math.floor(8 * uiscale))
    local labelH = math.max(8, math.floor(14 * uiscale))
    local cells = math.max(1, #v.cells)

    local byWidth = math.floor((p.width - gap * (cells + 1)) / cells)
    local byHeight = math.floor(p.height * 0.25) - labelH - gap
    local cellSize = math.max(8, math.min(byWidth, byHeight))
    local rowH = cellSize + labelH + gap

    local rowY = p.y + math.max(0, p.height - rowH)
    local rowW = cells * cellSize + gap * (cells - 1)
    local rowX = p.x + math.max(0, math.floor((p.width - rowW) / 2))

    return {
        gap = gap,
        labelH = labelH,
        cellSize = cellSize,
        rowX = rowX,
        rowY = rowY,
        enlarged = {
            x = p.x,
            y = p.y,
            width = math.max(1, p.width),
            height = math.max(1, rowY - p.y - gap),
        },
    }
end

-----------------------------------------------------------
-- Creation / teardown
-----------------------------------------------------------

-- params:
--   page, font, panel = {x,y,width,height}
--   requestTexture = function(path) -> textureHandle  (the owner's
--     cache + trimmed-loading bookkeeping; called once per frame path,
--     and NEVER for a missing cell)
--   chromeTexture  = highlight.png handle, for the selected-cell marker
--   zoom = initial zoom multiplier (#1907); the OWNER holds the live
--     value (it survives a row change, a facing change, playback and a
--     resize), this view only renders at whatever it was last told.
--   uiscale, zIndex
function buildingAssetView.new(params)
    local id = nextId
    nextId = nextId + 1

    views[id] = {
        id = id,
        page = params.page,
        font = params.font,
        panel = params.panel,
        requestTexture = params.requestTexture,
        chromeTexture = params.chromeTexture,
        zoom = previewZoom.clamp(params.zoom),
        uiscale = params.uiscale or scale.get(),
        zIndex = params.zIndex or 1,
        row = nil,         -- the combined row table (see the header)
        rowStart = nil,    -- wall-clock second the current cycle began
        facing = nil,      -- enlarged facing, declared rows only
        cells = {},        -- per-facing element + playback state
        frameIndex = 0,
        spriteId = nil,
        missingId = nil,   -- the enlarged missing marker
        legacyId = nil,    -- the enlarged legacy flag
        fitKey = nil,      -- guards redundant geometry writes
        ready = false,
    }
    return id
end

local function destroyCells(v)
    for _, c in ipairs(v.cells) do
        if c.hitId then UI.deleteElement(c.hitId) end
        if c.spriteId then UI.deleteElement(c.spriteId) end
        if c.markerId then UI.deleteElement(c.markerId) end
        if c.labelId then UI.deleteElement(c.labelId) end
        if c.missingId then UI.deleteElement(c.missingId) end
    end
    v.cells = {}
end

function buildingAssetView.destroy(id)
    local v = views[id]
    if not v then return end
    destroyCells(v)
    if v.spriteId then UI.deleteElement(v.spriteId) end
    if v.missingId then UI.deleteElement(v.missingId) end
    if v.legacyId then UI.deleteElement(v.legacyId) end
    views[id] = nil
end

-----------------------------------------------------------
-- Selection
-----------------------------------------------------------

local function buildCells(v)
    destroyCells(v)
    if not isDeclared(v.row) then return end

    for i, c in ipairs(facingCells(v.row)) do
        local name = "preview_facing_" .. v.id .. "_" .. i

        local markerId = UI.newSprite(name .. "_marker", 1, 1,
            v.chromeTexture, 0.3, 0.5, 0.8, 0.8, v.page)
        UI.addToPage(v.page, markerId, 0, 0)
        UI.setZIndex(markerId, v.zIndex)
        UI.setVisible(markerId, false)

        -- The cell sprite exists for every cell, missing or not, so a
        -- later reflow never has to create elements mid-layout; a
        -- missing cell simply keeps it hidden and shows the marker
        -- text instead. Its initial texture is the CHROME handle the
        -- owner already requested, never one of this cell's own paths:
        -- an element must be born with some handle, and taking the
        -- cell's would request a texture before reflow has decided
        -- whether it may be requested at all.
        local spriteId = UI.newSprite(name .. "_frame", 1, 1,
            v.chromeTexture, 1.0, 1.0, 1.0, 1.0, v.page)
        UI.addToPage(v.page, spriteId, 0, 0)
        UI.setZIndex(spriteId, v.zIndex + 1)
        UI.setVisible(spriteId, false)

        local missingId = UI.newText(name .. "_missing", MISSING_MARK,
            v.font, math.max(8, math.floor(14 * v.uiscale)),
            1.0, 0.4, 0.4, 1.0, v.page)
        UI.addToPage(v.page, missingId, 0, 0)
        UI.setZIndex(missingId, v.zIndex + 2)
        UI.setVisible(missingId, false)

        -- Requirement 6: a repeated legacy view must be VISIBLY marked,
        -- not merely flagged in the dump — four identical pictures are
        -- exactly what a reviewer would otherwise read as four authored
        -- ones. The asterisk is that mark.
        local caption = FACING_SHORT[c.facing] or c.facing
        if c.legacy then caption = caption .. "*" end
        local labelId = UI.newText(name .. "_label", caption, v.font,
            math.max(8, math.floor(14 * v.uiscale)),
            1.0, 1.0, 1.0, 1.0, v.page)
        UI.addToPage(v.page, labelId, 0, 0)
        UI.setZIndex(labelId, v.zIndex + 2)

        local hitId = UI.newSprite(name .. "_hit", 1, 1,
            v.chromeTexture, 0.0, 0.0, 0.0, 0.0, v.page)
        UI.addToPage(v.page, hitId, 0, 0)
        UI.setZIndex(hitId, v.zIndex + 3)
        UI.setClickable(hitId, true)
        UI.setOnClick(hitId, CELL_CALLBACK)

        table.insert(v.cells, {
            facing = c.facing,
            paths = c.paths,
            missing = c.missing,
            missingReason = c.missingReason,
            legacy = c.legacy,
            frameIndex = -1,
            markerId = markerId,
            spriteId = spriteId,
            missingId = missingId,
            labelId = labelId,
            hitId = hitId,
        })
    end
end

-- Select a row. ALWAYS resets the playback clock: requirement 14 makes
-- one forced-replay cycle per row selection, which is the window
-- requirement 15's gameplay-equality fixtures are stated within.
--
-- The enlarged FACING carries across a row change (v.facing outlives the
-- row), so comparing the same view of two lifecycle roles does not snap
-- the reviewer back to south between them; south is only the fallback,
-- for the first selection and for a row that lacks the current facing.
-- A raw row has no facing model at all and clears it, so returning to a
-- declared row through one starts from south again.
function buildingAssetView.setRow(id, row, now)
    local v = views[id]
    if not v then return end
    v.row = row
    v.rowStart = now
    v.frameIndex = 0
    v.ready = false
    v.fitKey = nil
    buildCells(v)
    v.facing = isDeclared(row) and resolveFacing(v, v.facing) or nil

    if not v.spriteId then
        v.spriteId = UI.newSprite("preview_building_sprite", 1, 1,
            v.chromeTexture, 1.0, 1.0, 1.0, 1.0, v.page)
        UI.addToPage(v.page, v.spriteId, 0, 0)
        UI.setZIndex(v.spriteId, v.zIndex)
    end
    if not v.missingId then
        v.missingId = UI.newText("preview_building_missing", MISSING_MARK,
            v.font, math.max(12, math.floor(28 * v.uiscale)),
            1.0, 0.4, 0.4, 1.0, v.page)
        UI.addToPage(v.page, v.missingId, 0, 0)
        UI.setZIndex(v.missingId, v.zIndex + 1)
        UI.setVisible(v.missingId, false)
    end
    if not v.legacyId then
        v.legacyId = UI.newText("preview_building_legacy", "legacy",
            v.font, math.max(8, math.floor(14 * v.uiscale)),
            1.0, 0.8, 0.4, 1.0, v.page)
        UI.addToPage(v.page, v.legacyId, 0, 0)
        UI.setZIndex(v.legacyId, v.zIndex + 1)
        UI.setVisible(v.legacyId, false)
    end
    buildingAssetView.reflow(id)
end

-- Enlarge a different facing. Deliberately does NOT touch rowStart, the
-- zoom multiplier or anything the owner holds (list selection, scroll):
-- requirement 14 makes a facing change a pure change of which cell is
-- enlarged, and the strip must keep playing through it.
function buildingAssetView.setFacing(id, facing)
    local v = views[id]
    if not v or not isDeclared(v.row) or not findCell(v, facing) then
        return false
    end
    v.facing = facing
    v.fitKey = nil
    buildingAssetView.reflow(id)
    return true
end

-- Move through the facing strip in its DISPLAYED order, wrapping at both
-- ends (#2492 requirement 17). Routed through setFacing exactly like a
-- cell click, so the clock and the multiplier stay put either way.
function buildingAssetView.selectAdjacentFacing(id, step)
    local v = views[id]
    if not v or (step ~= -1 and step ~= 1) or #v.cells == 0
        or not v.facing then
        return false
    end
    local current = nil
    for i, c in ipairs(v.cells) do
        if c.facing == v.facing then
            current = i
            break
        end
    end
    if not current then return false end
    local target = ((current - 1 + step) % #v.cells) + 1
    return buildingAssetView.setFacing(id, v.cells[target].facing)
end

-- Deliberately does NOT touch rowStart: a resize must preserve the
-- playback phase (#888 amendment, #2492 requirement 18), same as the
-- units viewer's own setPanel.
function buildingAssetView.setPanel(id, panel)
    local v = views[id]
    if not v then return end
    v.panel = panel
    v.fitKey = nil
    buildingAssetView.reflow(id)
end

-- #1907. Deliberately does NOT touch rowStart or the selection: zoom
-- follows the preview OBJECT (this building), so it survives a row
-- change, a facing change, playback and a resize, and only a new preview
-- session resets it.
function buildingAssetView.setZoom(id, multiplier)
    local v = views[id]
    if not v then return end
    v.zoom = previewZoom.clamp(multiplier)
    v.fitKey = nil
    buildingAssetView.reflow(id)
end

-- The rect the wheel zooms over and the fit denominator. For a DECLARED
-- row that is layout()'s enlarged sub-rect, never the whole panel: the
-- panel also holds the facing strip, which #1907 Requirement 3 forbids
-- the enlarged sprite from overlapping. A RAW row has no strip, so it
-- keeps the whole panel it always had.
function buildingAssetView.getZoomRegion(id)
    local v = views[id]
    if not v or not v.panel then return nil end
    return layout(v).enlarged
end

-----------------------------------------------------------
-- Geometry application
-----------------------------------------------------------

-- The path a cell shows at its current index, or nil when the cell is
-- missing. The nil is the enforcement point for "never requests its
-- invalid texture": every requestTexture call below is downstream of it.
local function cellPath(c)
    if c.missing then return nil end
    local idx = c.frameIndex >= 0 and c.frameIndex or 0
    return c.paths[math.min(#c.paths, idx + 1)]
end

-- Recompute every rect from the panel. Texture uploads are async, so a
-- sprite whose natural size isn't known yet leaves fitKey nil and
-- update() retries next tick instead of freezing a placeholder. A
-- MISSING cell is never unresolved — there is nothing to wait for — so
-- it contributes readiness immediately, which is what makes a missing
-- selection terminal rather than perpetually "loading".
function buildingAssetView.reflow(id)
    local v = views[id]
    if not v or not v.panel or not v.row then return end
    local g = layout(v)
    local allResolved = true

    for i, c in ipairs(v.cells) do
        local cx = g.rowX + (i - 1) * (g.cellSize + g.gap)
        local cy = g.rowY + g.labelH
        c.bounds = { x = cx, y = cy, w = g.cellSize, h = g.cellSize }

        UI.setSize(c.markerId, g.cellSize, g.cellSize)
        UI.setPosition(c.markerId, cx, cy)
        UI.setVisible(c.markerId, c.facing == v.facing)

        UI.setSize(c.hitId, g.cellSize, g.cellSize)
        UI.setPosition(c.hitId, cx, cy)
        UI.setPosition(c.labelId, cx, g.rowY + g.labelH - math.floor(g.labelH / 4))

        local path = cellPath(c)
        if not path then
            c.handle = nil
            UI.setVisible(c.spriteId, false)
            UI.setVisible(c.missingId, true)
            UI.setPosition(c.missingId, cx + math.floor(g.cellSize / 2),
                           cy + math.floor(g.cellSize / 2))
        else
            UI.setVisible(c.missingId, false)
            UI.setVisible(c.spriteId, true)
            local handle = v.requestTexture(path)
            c.handle = handle
            UI.setSpriteTexture(c.spriteId, handle)
            local size = engine.getTextureSize(handle)
            local rect = size and fitRect(
                { x = cx, y = cy, width = g.cellSize, height = g.cellSize },
                size.width, size.height)
            if rect then
                UI.setSize(c.spriteId, rect.width, rect.height)
                UI.setPosition(c.spriteId, rect.x, rect.y)
            else
                allResolved = false
                UI.setSize(c.spriteId, g.cellSize, g.cellSize)
                UI.setPosition(c.spriteId, cx, cy)
            end
        end
    end

    -- The enlarged view: the selected facing's cell for a declared row,
    -- the row's own single frame list for a raw one.
    local shown = v.facing and findCell(v, v.facing) or nil
    local path, legacy
    if isDeclared(v.row) then
        path = shown and cellPath(shown) or nil
        legacy = shown ~= nil and shown.legacy
    else
        local frames = (v.row.entry or {}).frames or {}
        path = frames[math.min(#frames, v.frameIndex + 1)]
        legacy = false
    end

    UI.setVisible(v.legacyId, legacy == true)
    if legacy then
        UI.setPosition(v.legacyId, g.enlarged.x + 4, g.enlarged.y + 4)
    end

    if not path then
        -- Terminal, not pending: nothing is in flight, so this is as
        -- resolved as the selection will ever get.
        UI.setVisible(v.spriteId, false)
        UI.setVisible(v.missingId, true)
        UI.setPosition(v.missingId,
            g.enlarged.x + math.floor(g.enlarged.width / 2),
            g.enlarged.y + math.floor(g.enlarged.height / 2))
        v.ready = true
    else
        UI.setVisible(v.missingId, false)
        UI.setVisible(v.spriteId, true)
        -- Pushed unconditionally: a selection or facing change must
        -- appear at once rather than waiting for the next frame-index
        -- change.
        local handle = v.requestTexture(path)
        UI.setSpriteTexture(v.spriteId, handle)
        local size = engine.getTextureSize(handle)
        local rect = size and fitRect(g.enlarged, size.width, size.height,
                                      v.zoom)
        if rect then
            UI.setSize(v.spriteId, rect.width, rect.height)
            UI.setPosition(v.spriteId, rect.x, rect.y)
            v.ready = true
        else
            allResolved = false
        end
    end

    v.fitKey = allResolved
        and (tostring(v.facing) .. "|" .. tostring(v.frameIndex)
             .. "|" .. tostring(v.panel.width)
             .. "x" .. tostring(v.panel.height)
             .. "@" .. tostring(v.zoom))
        or nil
end

-----------------------------------------------------------
-- Playback
-----------------------------------------------------------

-- The frame count and playback metadata of the SELECTED row: a declared
-- row's come from its own declaration, a raw row's from the entry.
local function playbackOf(v)
    if not v.row then return 0, 8.0, false, false end
    if isDeclared(v.row) then
        local d = v.row.declared or {}
        local cell = v.facing and findCell(v, v.facing)
        local n = cell and #cell.paths or 0
        -- The sprite row declares one path per facing, so it is a
        -- static by construction — never a one-frame "animation" with a
        -- clock nobody can see running.
        local animated = v.row.kind == "lifecycle" and d.resolved == true
        return n, d.fps or 8.0, d.loop == true, animated
    end
    local e = v.row.entry or {}
    return #(e.frames or {}), e.fps or 8.0, e.loop == true,
           e.animated == true
end

-- Advance to the frame 'now' implies. Cheap on a steady tick: a sprite
-- is only rewritten when its index actually changed (or the geometry
-- hasn't resolved yet).
function buildingAssetView.update(id, now)
    local v = views[id]
    if not v or not v.row then return end
    local frameCount, fps, srcLoop, animated = playbackOf(v)

    if animated and v.rowStart then
        local elapsed = now - v.rowStart
        -- Every cell computes its own index from the SAME elapsed value
        -- against its OWN path count, exactly as the units viewer does,
        -- so the strip stays phase-aligned with the enlarged view.
        for _, c in ipairs(v.cells) do
            local idx = frameIndexAt(srcLoop, fps, #c.paths, elapsed)
            if idx ~= c.frameIndex then
                c.frameIndex = idx
                v.fitKey = nil
            end
        end
        -- The headline index the dump reports and the enlarged sprite
        -- draws. For a DECLARED row it is the enlarged facing's own, so
        -- a reviewer reads the frame they are looking at rather than a
        -- row-level average of four possibly-unequal counts. A RAW row
        -- has no cells at all, so it is computed straight from the
        -- entry's own frame list — the pre-#2492 behavior, which the
        -- cell loop above would otherwise silently stop performing.
        local cell = v.facing and findCell(v, v.facing) or nil
        local idx = cell and cell.frameIndex
            or frameIndexAt(srcLoop, fps, frameCount, elapsed)
        if idx and idx >= 0 and idx ~= v.frameIndex then
            v.frameIndex = idx
            v.fitKey = nil
        end
    end

    if not v.fitKey then buildingAssetView.reflow(id) end
end

-----------------------------------------------------------
-- Input
-----------------------------------------------------------

function buildingAssetView.isCellCallback(callbackName)
    return callbackName == CELL_CALLBACK
end

-- Resolve a clicked element handle to its facing and enlarge it. Returns
-- the facing name on a hit, nil when the handle isn't ours.
function buildingAssetView.handleCellClick(id, elemHandle)
    local v = views[id]
    if not v then return nil end
    for _, c in ipairs(v.cells) do
        if c.hitId == elemHandle then
            buildingAssetView.setFacing(id, c.facing)
            return c.facing
        end
    end
    return nil
end

-----------------------------------------------------------
-- Introspection (#888 Requirement 4, #2492 requirement 12)
-----------------------------------------------------------

function buildingAssetView.dump(id)
    local v = views[id]
    if not v or not v.row then return nil end
    local frameCount, fps, srcLoop, animated = playbackOf(v)
    local declared = v.row.declared or {}
    local shown = v.facing and findCell(v, v.facing) or nil

    local out = {
        -- The DISPLAY label, unchanged in meaning from #888: the probe
        -- and the existing fixtures read it as the row on screen.
        entry = v.row.label,
        identity = v.row.identity,
        kind = v.row.kind,
        frameIndex = v.frameIndex,
        frameCount = frameCount,
        fps = fps,
        ready = v.ready,
        facing = v.facing,
    }

    -- Bounds come from UI.getElementInfo, not this module's own layout
    -- arithmetic (scripts/ui/list.lua's F3 dump does the same): the
    -- engine is the authority on where the hit box actually is, so a
    -- probe clicking these coordinates exercises the real element rather
    -- than a self-reported guess a geometry bug could leave stale.
    local row = {}
    for _, c in ipairs(v.cells) do
        local info = UI.getElementInfo(c.hitId)
        local path = cellPath(c)
        table.insert(row, {
            facing = c.facing,
            frameIndex = c.frameIndex,
            frameCount = #c.paths,
            paths = c.paths,
            path = path,
            -- The handle reflow ACTUALLY requested, recorded there
            -- rather than re-derived here: a dump must observe, never
            -- request. Its absence is the machine-checkable half of
            -- "never requests its invalid texture" — a handle on a
            -- missing cell would mean a request was made.
            handle = c.handle,
            hitHandle = c.hitId,
            missing = c.missing,
            missingReason = c.missingReason,
            legacy = c.legacy,
            bounds = info and {
                x = info.x, y = info.y, w = info.width, h = info.height,
            } or c.bounds,
        })
    end
    out.facingRow = row

    if isDeclared(v.row) then
        out.role = declared.role
        out.animation = declared.animation
        out.resolved = declared.resolved == true
        out.declaration = declared.source
        out.legacy = declared.legacy == true
        out.missing = shown ~= nil and shown.missing == true
        out.missingReason = shown and shown.missingReason or nil
        out.path = shown and cellPath(shown) or nil
    else
        local e = v.row.entry or {}
        local frames = e.frames or {}
        out.missing = false
        out.path = frames[math.min(#frames, v.frameIndex + 1)]
    end

    -- #1907 Requirement 11: the zoom region and the sprite's ACTUAL
    -- rendered bounds, read back from UI.getElementInfo rather than
    -- restated from this module's own arithmetic, so a probe verifies
    -- where the sprite really is.
    local info = v.spriteId and UI.getElementInfo(v.spriteId)
    out.zoom = {
        multiplier = v.zoom,
        min = previewZoom.MIN,
        max = previewZoom.MAX,
        region = buildingAssetView.getZoomRegion(id),
        -- A missing enlarged view has no sprite on screen, and reporting
        -- the hidden element's stale rect would let a containment
        -- assertion pass over something nobody can see.
        sprite = (out.path and info) and {
            x = info.x, y = info.y, w = info.width, h = info.height,
        } or nil,
    }
    -- Assigned, never written as `x and y or z`: both fields can
    -- legitimately BE false, and Lua's and/or collapses that to the
    -- fallback (the bug #887's own dump documents).
    out.animated = animated
    out.loop = srcLoop
    return out
end

return buildingAssetView
