-- Build Tool
--
-- Owns the popup picker that lists available buildings + structure
-- pieces (#403 — absorbs the former scripts/construct_tool.lua) and the
-- "placement mode" state machine that drives what happens at the
-- hovered tile until the player commits or cancels.
--
-- Picker layout:
--   * Tab strip at top: "All" + one tab per visible category (building
--     categories, plus a "Structures" tab for the dungeon_1 pieces).
--     Clicking a tab swaps the icon grid below it.
--   * Icon grid below: 4-wide row of sprite icons. Hover → tooltip with
--     display name and a hint containing description + cost line.
--     Click → enterPlacement(target).
--
-- Visibility filter (buildings):
--   * is_starting defs are visible until any building of that defName
--     has been placed. (Portal: visible at game start, gone once
--     placed.)
--   * Non-starting defs, and the Structures category, are visible only
--     after at least one starting building has been placed anywhere.
--     (Cargo / structure pieces hidden until the portal exists.)
--   Re-evaluated on every showPicker() and after a successful place.
--
-- Placement is a hybrid, keyed on the picked target's kind:
--   * Power item building (#358)       → single click → power.placeNode
--     against whichever selected unit carries a matching item, consuming
--     it and registering a power node. A click with no carrying unit
--     selected fails without leaving placement mode.
--   * Starting building (the portal)   → single click → building.spawn
--     (instant; ghost preview via building.setGhost), then exits back to
--     the default tool — UNLESS the position is remote from every placed
--     location (#779, building.remoteCheck): a valid remote click opens
--     scripts.build_tool_remote_warning's confirmation modal instead of
--     spawning, and placement stays armed until the player confirms
--     ("Establish Here", which re-validates and then spawns) or cancels
--     ("Choose Another Site" / Escape).
--   * Non-starting building            → single click →
--     construction.designate(..., "building", def) (one footprint;
--     still previewed with building.setGhost). Stays armed so the
--     player can place more without reopening the picker.
--   * Structure piece                  → DF-style two-click rectangle
--     (construction.setAnchor / construction.designate "structure"),
--     mirroring the old construct_tool. No building ghost — the engine
--     renders the anchor→hover rectangle preview keyed off the same
--     BuildTool ToolMode (World/Render/Quads.hs). Stays armed.
-- Right-click cancels a pending structure rectangle, or (nothing
-- pending) erases the designation under the cursor — except the
-- starting-building path, where right-click just exits placement (no
-- designation to erase there).
--
-- Singleton via package.loaded so init.lua's input hooks see the same
-- state that the engine-ticked update() uses. Same pattern as
-- scripts/unit_ai.lua and scripts/debug.lua.

local buildTool = package.loaded["scripts.build_tool"] or {}
package.loaded["scripts.build_tool"] = buildTool

local panel = require("scripts.ui.panel")
local label = require("scripts.ui.label")
local scale = require("scripts.ui.scale")

-- Module state. Hung off the module table so it survives reloads.
--
-- Save/load contract: this state is intentionally NOT registered with
-- saveModules. It represents an in-progress UI action ("I am picking /
-- positioning a building right now"), not a durable player decision.
-- Within-session loads keep the state via the package.loaded singleton;
-- update() re-establishes the engine-side ghost on the next tick.
-- Fresh-session loads start at the default {mode="off"} — the player
-- re-enters the build tool via the toolbar. The Haskell-side
-- `buildingGhostRef` mirrors this: not in SaveData, defaults to Nothing
-- in a fresh engine.
buildTool.state = buildTool.state or {
    mode             = "off",   -- "off" / "picker" / "placement"
    target           = nil,     -- when in placement: the picked entry (see below)
    anchor           = nil,     -- {gx, gy} first-click anchor for a structure rectangle
    panelId          = nil,
    activeCategory   = "All",
    tabIds           = {},      -- list of {boxId, labelId, category}
    tabsByHandle     = {},      -- box element handle → category name
    iconIds          = {},      -- list of icon element ids
    iconsByHandle    = {},      -- icon element handle → picker entry
    iconAreaRect     = nil,     -- {x, y, w, h} for rebuilding icons on tab change
    lastHoverTile    = nil,
}

-- hud.lua module reference, read live (worldId/pages mutate outside
-- setup() — e.g. ui_manager.lua swaps hud.worldId on view changes
-- without re-calling setup). Same pattern as mine_tool/construct_tool.
buildTool.hud = nil

local MOUSE_LEFT  = 1
local MOUSE_RIGHT = 2

-- Layout (base units; uiscale applied at draw time).
-- Padding has to clear the 9-patch border art (~16–20 px on each
-- side at scale 1) AND leave visible breathing room around the
-- content; otherwise icons/labels appear to overlap the panel edge.
local PICKER_W_BASE    = 360
local PANEL_PAD_TOP    = 28
local PANEL_PAD_BOTTOM = 20
local PANEL_PAD_X      = 28
local TAB_H_BASE       = 28
local TAB_GAP_BASE     = 4
local TAB_PAD_X_BASE   = 12
local TAB_FONT_SIZE    = 14
local TAB_BG_ACTIVE    = {0.40, 0.40, 0.40, 0.95}
local TAB_BG_INACTIVE  = {0.15, 0.15, 0.15, 0.80}
local TAB_TEXT_ACTIVE  = {1.00, 1.00, 1.00, 1.00}
local TAB_TEXT_INACTIVE= {0.78, 0.78, 0.78, 1.00}
local ICON_SIZE_BASE   = 64
local ICON_GAP_BASE    = 8
local ICONS_PER_ROW    = 4

-----------------------------------------------------------
-- HUD hookup. Called once from hud.lua after the toolbar is built.
-----------------------------------------------------------
function buildTool.setup(ctx)
    buildTool.hud = ctx.hud
end

-----------------------------------------------------------
-- Visibility helpers
-----------------------------------------------------------

-- Active-world placed building ids. building.list() is global across
-- every live world page, so buildings on hidden/off-world pages used to
-- pollute this world's build-menu visibility (#198). building.getActiveIds()
-- is scoped to the active page, matching unit.getAllIds.
local function getAllBuildingIds()
    return building.getActiveIds() or {}
end

-- Picker entries for buildings. Implements the two-rule visibility
-- filter described in the file header, and returns whether any starting
-- building has been placed (gates the Structures category below).
local function visibleBuildingEntries()
    local defs = building.listDefs() or {}

    -- Lookup: defName → isStarting (for translating placed ids back
    -- to "was this a starting building?").
    local isStarting = {}
    for _, d in ipairs(defs) do isStarting[d.name] = d.isStarting end

    -- Scan placed buildings: set of defNames that already exist, and
    -- a flag for "any starting building has been placed at all".
    local placedDefNames    = {}
    local anyStartingPlaced = false
    for _, bid in ipairs(getAllBuildingIds()) do
        local info = building.getInfo(bid)
        if info and info.defName then
            placedDefNames[info.defName] = true
            if isStarting[info.defName] then
                anyStartingPlaced = true
            end
        end
    end

    local visible = {}
    for _, d in ipairs(defs) do
        local show = d.isStarting
            and not placedDefNames[d.name]
            or (not d.isStarting and anyStartingPlaced)
        if show then
            table.insert(visible, {
                kind        = "building",
                def         = d.name,
                isStarting  = d.isStarting,
                displayName = d.displayName,
                description = d.description,
                category    = d.category or "Misc",
                iconTex     = d.iconTex,
            })
        end
    end
    return visible, anyStartingPlaced
end

-- The structure pack the picker offers (matches scripts/structures.lua);
-- a multi-pack picker is a follow-up. Icons reuse the pack's own tile
-- textures (#403 — no dedicated icon art).
local STRUCTURE_PACK = "dungeon_1"
local STRUCTURE_PIECES = {
    { piece = "floor",   edge = nil,  label = "Floor"   },
    { piece = "wall",    edge = "ne", label = "Wall NE"  },
    { piece = "wall",    edge = "nw", label = "Wall NW"  },
    { piece = "wall",    edge = "se", label = "Wall SE"  },
    { piece = "wall",    edge = "sw", label = "Wall SW"  },
    { piece = "ceiling", edge = nil,  label = "Ceiling" },
    { piece = "post",    edge = nil,  label = "Post"    },
}

-- Lazily-loaded + cached structure-piece icon textures.
local function structureTex(texName)
    buildTool.structureTex = buildTool.structureTex or {}
    if not buildTool.structureTex[texName] then
        buildTool.structureTex[texName] = engine.loadTexture(
            "assets/textures/buildings/" .. STRUCTURE_PACK .. "/"
            .. texName .. ".png")
    end
    return buildTool.structureTex[texName]
end

local function structureEntries()
    local entries = {}
    for _, p in ipairs(STRUCTURE_PIECES) do
        local texName = (p.piece == "wall") and ("wall_" .. p.edge) or p.piece
        table.insert(entries, {
            kind        = "structure",
            pack        = STRUCTURE_PACK,
            piece       = p.piece,
            edge        = p.edge,
            displayName = p.label,
            description = "",
            category    = "Structures",
            iconTex     = structureTex(texName),
        })
    end
    return entries
end

-- Wire (#359): a second structure pack, offered alongside dungeon_1's
-- pieces. Unlike the DF-style rectangle every other structure piece
-- designates, wire is a PATH tool: the two-click commit snaps to a
-- straight 1-wide line along whichever axis has the larger extent from
-- the anchor (snapWirePath), and construction.setLineMode makes the
-- live anchor→hover ghost preview the SAME line (World/Render/Quads.hs)
-- instead of a filled rectangle — see isWirePath/enterPlacement/
-- handleMouseDown below. The connection-aware render (scripts/wire.lua)
-- then picks each tile's autotile variant from its neighbours, so the
-- committed line reads as one connected run. The icon reuses the pack's
-- "cross" connection art — the most recognizable single-glance "this is
-- wire" shape.
local function wireTex()
    if buildTool.wireTex then return buildTool.wireTex end
    buildTool.wireTex = engine.loadTexture(
        "assets/textures/structures/wire/cross.png")
    return buildTool.wireTex
end

local function wireEntry()
    return {
        kind        = "structure",
        pack        = "wire",
        piece       = "wire",
        edge        = nil,
        displayName = "Wire",
        description = "Power-grid wire. Connects to adjacent wire tiles.",
        category    = "Structures",
        iconTex     = wireTex(),
    }
end

-- Full picker entry list: buildings + (once a starting building has been
-- placed) the structure pieces.
local function visibleEntries()
    local entries, anyStartingPlaced = visibleBuildingEntries()
    if anyStartingPlaced then
        for _, e in ipairs(structureEntries()) do
            table.insert(entries, e)
        end
        table.insert(entries, wireEntry())
    end
    return entries
end

-- Stable per-entry identity for element naming / lookup (buildings key
-- on defName; structure pieces have no defName of their own).
local function entryId(d)
    if d.kind == "structure" then
        return "structure_" .. d.piece .. (d.edge and ("_" .. d.edge) or "")
    end
    return "building_" .. d.def
end

-- "All" + every distinct category present in visible defs, in the
-- order they first appear.
local function visibleCategories(visible)
    local seen = {}
    local cats = { "All" }
    for _, d in ipairs(visible) do
        local c = d.category or "Misc"
        if not seen[c] then
            seen[c] = true
            table.insert(cats, c)
        end
    end
    return cats
end

local function defsInCategory(visible, category)
    if category == "All" then return visible end
    local result = {}
    for _, d in ipairs(visible) do
        if (d.category or "Misc") == category then
            table.insert(result, d)
        end
    end
    return result
end

-----------------------------------------------------------
-- Element teardown
-----------------------------------------------------------
local function destroyTabs()
    for _, t in ipairs(buildTool.state.tabIds) do
        if t.labelId then label.destroy(t.labelId) end
        if t.boxId   then UI.deleteElement(t.boxId) end
    end
    buildTool.state.tabIds       = {}
    buildTool.state.tabsByHandle = {}
end

local function destroyIcons()
    for _, id in ipairs(buildTool.state.iconIds) do
        UI.deleteElement(id)
    end
    buildTool.state.iconIds       = {}
    buildTool.state.iconsByHandle = {}
end

local function destroyPicker()
    destroyIcons()
    destroyTabs()
    if buildTool.state.panelId then
        panel.destroy(buildTool.state.panelId)
        buildTool.state.panelId = nil
    end
    buildTool.state.iconAreaRect = nil
end

-----------------------------------------------------------
-- Icon-grid builder. Rebuilt on showPicker() and on tab clicks.
-- All other panel chrome stays put so tab switches are cheap.
-----------------------------------------------------------
local function rebuildIconGrid(visible)
    destroyIcons()

    local rect = buildTool.state.iconAreaRect
    if not rect then return end

    local h = buildTool.hud
    if not h then return end

    local uiscale     = scale.get()
    -- #750 round-8/9 review: the SAME columnsPerRow/iconSize/iconGap
    -- showPicker() derived (and, at round 9, possibly vertically
    -- compacted) when it computed rowCount/iconAreaH — never the fixed
    -- ICONS_PER_ROW/ICON_SIZE_BASE/ICON_GAP_BASE defaults, or a narrow
    -- or short panel's icon grid would drift back to running off the
    -- panel/framebuffer edge on the very next rebuild (every tab
    -- switch calls this function again).
    local iconSize    = buildTool.state.iconSize or math.floor(ICON_SIZE_BASE * uiscale)
    local iconGap     = buildTool.state.iconGap  or math.floor(ICON_GAP_BASE  * uiscale)
    local catDefs     = defsInCategory(visible, buildTool.state.activeCategory)
    local whiteTex    = buildTool.whitePixelTex
    local columnsPerRow = buildTool.state.columnsPerRow or ICONS_PER_ROW

    for i, d in ipairs(catDefs) do
        local col = (i - 1) % columnsPerRow
        local row = math.floor((i - 1) / columnsPerRow)
        local ix  = rect.x + col * (iconSize + iconGap)
        local iy  = rect.y + row * (iconSize + iconGap)

        local iconId = UI.newSprite(
            "build_tool_icon_" .. entryId(d),
            iconSize, iconSize,
            d.iconTex or whiteTex,
            1.0, 1.0, 1.0, 1.0,
            h.world_page
        )
        UI.addToPage(h.world_page, iconId, ix, iy)
        UI.setZIndex(iconId, 122)
        UI.setClickable(iconId, true)
        UI.setOnClick(iconId, "onBuildMenuIconClick")
        UI.setTooltipRich(iconId, {
            text = d.displayName,
            hint = (d.description ~= "" and d.description or "")
                   .. "\nCosts: —",
        })

        table.insert(buildTool.state.iconIds, iconId)
        buildTool.state.iconsByHandle[iconId] = d
    end
end

-----------------------------------------------------------
-- Tab strip builder. Tabs flow horizontally inside the panel's
-- content-rect at the top. Each tab is a coloured backdrop sprite
-- with a centred text label on top; the backdrop is the click
-- target (text element has a zero-sized hit box).
-----------------------------------------------------------
local function buildTabStrip(cats, stripX, stripY, contentW)
    destroyTabs()

    local h = buildTool.hud
    if not h then return end

    local uiscale  = scale.get()
    local tabH     = math.floor(TAB_H_BASE     * uiscale)
    local tabGap   = math.floor(TAB_GAP_BASE   * uiscale)
    local tabPadX  = math.floor(TAB_PAD_X_BASE * uiscale)
    local fontPx   = math.floor(TAB_FONT_SIZE  * uiscale)
    local whiteTex = buildTool.whitePixelTex

    -- #750 round-8 review: the tab strip used to flow horizontally with
    -- no bound at all against the panel's actual content width — with
    -- enough categories (or long category names) at a narrow, high-
    -- scale, still-C2-supported combination, later tabs could render
    -- past the panel/framebuffer edge, unclipped and unscrollable.
    -- Shrink-to-fit (never wrap to a second row, which would need its
    -- own height accounted for above): compute the natural row width
    -- first; if it exceeds what's actually available, scale every tab's
    -- width and every gap down by the same factor so the WHOLE row
    -- lands inside the panel — a tab stays reachable (clickable, even if
    -- visually cramped), never literally off-screen. Floored so a tab
    -- can never shrink to zero width.
    local availableTabW = contentW or math.huge
    local labelWidths, naturalTabWidths, naturalTotalW = {}, {}, 0
    for i, cat in ipairs(cats) do
        local labelW = engine.getTextWidth(h.menuFont, cat, fontPx) or 0
        local tabW   = labelW + 2 * tabPadX
        labelWidths[i] = labelW
        naturalTabWidths[i] = tabW
        naturalTotalW = naturalTotalW + tabW + (i > 1 and tabGap or 0)
    end
    local shrink = 1.0
    if naturalTotalW > availableTabW and naturalTotalW > 0 then
        shrink = availableTabW / naturalTotalW
    end
    local shrunkGap = math.floor(tabGap * shrink)

    local cursorX = stripX
    for i, cat in ipairs(cats) do
        local labelW = labelWidths[i]
        local tabW   = math.max(20, math.floor(naturalTabWidths[i] * shrink))
        local active = (cat == buildTool.state.activeCategory)
        local bg     = active and TAB_BG_ACTIVE   or TAB_BG_INACTIVE
        local fg     = active and TAB_TEXT_ACTIVE or TAB_TEXT_INACTIVE

        local boxId = UI.newSprite(
            "build_tool_tab_box_" .. cat,
            tabW, tabH,
            whiteTex,
            bg[1], bg[2], bg[3], bg[4],
            h.world_page
        )
        UI.addToPage(h.world_page, boxId, cursorX, stripY)
        UI.setZIndex(boxId, 122)
        UI.setClickable(boxId, true)
        UI.setOnClick(boxId, "onBuildMenuTabClick")

        local labelId = label.new({
            name     = "build_tool_tab_label_" .. cat,
            text     = cat,
            font     = h.menuFont,
            fontSize = TAB_FONT_SIZE,
            color    = fg,
            page     = h.world_page,
            uiscale  = uiscale,
        })
        local labelHandle = label.getElementHandle(labelId)
        -- Centre the label inside the box. UI text positioned at its
        -- baseline → push down by ~0.7 of fontSize.
        local lblX = cursorX + math.floor((tabW - labelW) / 2)
        local lblY = stripY  + math.floor((tabH + fontPx) / 2) - math.floor(fontPx * 0.15)
        UI.addToPage(h.world_page, labelHandle, lblX, lblY)
        UI.setZIndex(labelHandle, 123)

        table.insert(buildTool.state.tabIds,
            { boxId = boxId, labelId = labelId, category = cat })
        buildTool.state.tabsByHandle[boxId] = cat

        cursorX = cursorX + tabW + shrunkGap
    end
end

-----------------------------------------------------------
-- Public: open the picker. Computes visibility, builds tabs, builds
-- the active-category icon grid.
-----------------------------------------------------------
function buildTool.showPicker()
    if buildTool.state.mode == "picker" then return end
    destroyPicker()

    local h = buildTool.hud
    if not h or not h.world_page then
        engine.logWarn("BuildTool: showPicker called before setup()")
        return
    end

    -- Cache the white-pixel texture once; it's used by every tab and
    -- by any icon that lacks a sprite. engine.loadTexture is itself
    -- cached, but going through it on every rebuild is wasted work.
    if not buildTool.whitePixelTex then
        buildTool.whitePixelTex = engine.loadTexture(
            "assets/textures/utility/white.png")
    end

    local visible = visibleEntries()
    if #visible == 0 then
        engine.logWarn("BuildTool: no buildings currently available")
        return
    end

    local cats = visibleCategories(visible)

    -- Snap the active tab to a still-valid category. After a portal
    -- is placed, "Starting" disappears; if the user had it active,
    -- fall back to All.
    local stillValid = false
    for _, c in ipairs(cats) do
        if c == buildTool.state.activeCategory then stillValid = true; break end
    end
    if not stillValid then buildTool.state.activeCategory = "All" end

    -- Panel sizing. Width is fixed unless the framebuffer forces it
    -- smaller (below); height depends on how many rows of icons we need
    -- for the active category, which itself now depends on how many
    -- columns actually fit (also below).
    local uiscale  = scale.get()
    local iconSize = math.floor(ICON_SIZE_BASE * uiscale)
    local iconGap  = math.floor(ICON_GAP_BASE  * uiscale)
    local tabH     = math.floor(TAB_H_BASE     * uiscale)
    local activeCatDefs = defsInCategory(visible, buildTool.state.activeCategory)

    local pickerW = math.floor(PICKER_W_BASE * uiscale)

    -- Anchor to the build button (same geometry as before so the
    -- visual position doesn't shift from old to new picker).
    local margin       = math.floor(16 * uiscale)
    local buttonSize   = h.baseSizes and h.baseSizes.buttonSize
    local btnSize      = buttonSize and math.floor(buttonSize * uiscale) or 64
    local stackGap     = math.floor(8 * uiscale)
    local pickerX      = margin + btnSize + stackGap

    -- #750 round-7 review: cap against the actual framebuffer — unlike
    -- cargo_inventory_panel.lua/item_contents_panel.lua (which at least
    -- clamp POSITION), this picker had no framebuffer awareness at all:
    -- PICKER_W_BASE*uiscale (360 at 1x, 1440 at a still-C2-supported 4x)
    -- could extend far past the right/bottom edge from its fixed
    -- toolbar-anchored position. Best-effort degrade, same pattern as
    -- everywhere else this class of gap was found.
    if h.fbW then pickerW = math.min(pickerW, math.max(20, h.fbW - pickerX)) end

    -- #750 round-8 review: capping the PANEL alone isn't enough — the
    -- icon grid below used to always lay out ICONS_PER_ROW (a fixed 4)
    -- columns regardless of how wide the (possibly now-shrunk) panel
    -- actually is, so at a narrow, high-scale, still-C2-supported
    -- combination later icons rendered past the panel/framebuffer edge
    -- entirely uncapped, unclipped, unscrollable. columnsPerRow is now
    -- derived from the panel's actual usable content width (floored at
    -- 1, so a single column is always at least possible) and stored on
    -- buildTool.state so rebuildIconGrid uses the SAME value this row-
    -- count/height calculation assumed — never a drifted, independently
    -- recomputed one.
    local padX = math.floor(PANEL_PAD_X * uiscale)
    local availableIconW = math.max(iconSize, pickerW - 2 * padX)
    local columnsPerRow = math.max(1,
        math.floor((availableIconW + iconGap) / (iconSize + iconGap)))

    -- #750 round-9 review: size the icon area against the WORST-CASE
    -- category rather than whichever tab happens to be active when the
    -- picker opens — "All" is always a superset of every other tab, so
    -- using its count guarantees a later same-session tab switch
    -- (handleTabClick only rebuilds the icon grid, it never re-runs this
    -- sizing pass) can never need more rows than what's already
    -- budgeted. Mirrors the same "fit against the worst-case row"
    -- technique input_tab.lua/notifications_tab.lua already use
    -- elsewhere in this codebase (see CLAUDE.md's responsive-menu-
    -- lifecycle notes).
    local worstCaseCount = math.max(#activeCatDefs, #visible)

    -- Always show at least one slot of vertical space so the panel
    -- isn't a flat strip when a category is empty.
    local rowCount = math.max(1,
        math.ceil(worstCaseCount / columnsPerRow))
    local iconAreaH = rowCount * iconSize + math.max(0, rowCount - 1) * iconGap

    -- Tab-strip + small gap + icon area + panel padding.
    local tabGapBelow = math.floor(8 * uiscale)
    local chromeH = math.floor((PANEL_PAD_TOP + PANEL_PAD_BOTTOM) * uiscale)
                  + tabH + tabGapBelow

    -- #750 round-9 review: capping pickerH below constrains the PANEL's
    -- own box, but never touched iconAreaH itself — a narrow width
    -- forcing few columns could still stack enough rows into an icon
    -- area taller than the framebuffer, with nothing to clip or scroll
    -- to the overflow. Compact instead of scroll (same best-effort
    -- philosophy as every other fix in this class): if the natural icon
    -- area exceeds the tallest the picker could ever be (the picker's Y
    -- position floats below, so fbH minus its fixed chrome is the best
    -- case regardless of where it eventually clamps to), shrink icon
    -- size and gap by one factor (floored so an icon stays a real,
    -- visible target) and re-derive columnsPerRow from the shrunk size
    -- against the SAME width budget — a smaller icon fits more per row,
    -- which itself reduces how many rows are needed. Both are stored on
    -- buildTool.state so rebuildIconGrid (re-run on every tab switch)
    -- draws at the SAME size this budget assumed.
    if h.fbH and iconAreaH > 0 then
        local maxIconAreaH = math.max(iconSize, h.fbH - chromeH)
        if iconAreaH > maxIconAreaH then
            local vshrink = maxIconAreaH / iconAreaH
            iconSize = math.max(16, math.floor(iconSize * vshrink))
            iconGap  = math.max(2,  math.floor(iconGap  * vshrink))
            columnsPerRow = math.max(1,
                math.floor((math.max(iconSize, pickerW - 2 * padX) + iconGap)
                           / (iconSize + iconGap)))
            rowCount = math.max(1, math.ceil(worstCaseCount / columnsPerRow))
            iconAreaH = rowCount * iconSize + math.max(0, rowCount - 1) * iconGap
        end
    end
    buildTool.state.columnsPerRow = columnsPerRow
    buildTool.state.iconSize = iconSize
    buildTool.state.iconGap  = iconGap

    local pickerH = chromeH + iconAreaH

    local buildBtnTopY = h.fbH - margin - 2 * btnSize - stackGap
    local pickerY      = buildBtnTopY

    if h.fbH then
        pickerY = math.max(0, math.min(pickerY, h.fbH - pickerH))
        pickerH = math.min(pickerH, h.fbH - pickerY)
    end

    buildTool.state.panelId = panel.new({
        name       = "build_tool_picker",
        page       = h.world_page,
        x          = pickerX,
        y          = pickerY,
        width      = pickerW,
        height     = pickerH,
        textureSet = h.boxTexSet,
        color      = {0.1, 0.1, 0.1, 0.9},
        tileSize   = 64,
        zIndex     = 120,
        padding    = { top = PANEL_PAD_TOP, bottom = PANEL_PAD_BOTTOM,
                       left = PANEL_PAD_X, right = PANEL_PAD_X },
        uiscale    = uiscale,
    })

    local pbounds  = panel.getContentBounds(buildTool.state.panelId)
    local contentX = pickerX + pbounds.x
    local contentY = pickerY + pbounds.y

    buildTabStrip(cats, contentX, contentY, pbounds.width)

    buildTool.state.iconAreaRect = {
        x = contentX,
        y = contentY + tabH + tabGapBelow,
        w = pbounds.width,
        h = iconAreaH,
    }
    rebuildIconGrid(visible)

    buildTool.state.mode = "picker"
end

function buildTool.hidePicker()
    destroyPicker()
    if buildTool.state.mode == "picker" then
        buildTool.state.mode = "off"
    end
end

-----------------------------------------------------------
-- Click handlers wired up by ui_manager.lua.
-----------------------------------------------------------
function buildTool.handleTabClick(elemHandle)
    local cat = buildTool.state.tabsByHandle[elemHandle]
    if not cat then return false end
    if cat == buildTool.state.activeCategory then return true end
    buildTool.state.activeCategory = cat
    -- Re-tint the tabs in place (no need to destroy/rebuild the strip).
    for _, t in ipairs(buildTool.state.tabIds) do
        local active = (t.category == cat)
        local bg = active and TAB_BG_ACTIVE   or TAB_BG_INACTIVE
        local fg = active and TAB_TEXT_ACTIVE or TAB_TEXT_INACTIVE
        UI.setColor(t.boxId, bg[1], bg[2], bg[3], bg[4])
        label.setColor(t.labelId, fg)
    end
    rebuildIconGrid(visibleEntries())
    return true
end

function buildTool.handleIconClick(elemHandle)
    local target = buildTool.state.iconsByHandle[elemHandle]
    if not target then return false end
    buildTool.enterPlacement(target)
    return true
end

-- Wire (#359) is a structure target drawn as a straight PATH rather than
-- a filled rectangle: see construction.setLineMode / snapWirePath below.
local function isWirePath(target)
    return target and target.kind == "structure" and target.pack == "wire"
end

-- Snap a path endpoint to whichever axis has the larger extent from the
-- anchor, so a diagonal drag commits as a straight 1-wide line rather
-- than the filled rectangle every other structure piece designates.
-- MUST match the engine's line-mode preview exactly (the dx/dy compare
-- in World/Render/Quads.hs constructPreviewQuads) so what previews is
-- what commits. Exported (not local) so tools/wire_probe.py can verify
-- the snap directly instead of only the lower-level construction.*
-- calls it feeds.
function buildTool.snapWirePath(ax, ay, x, y)
    local dx, dy = x - ax, y - ay
    if math.abs(dx) >= math.abs(dy) then return x, ay
    else return ax, y end
end

-- The structure.hasAt slot a piece designation targets — mirrors
-- Construct.hs's structurePieceSlot / unit_ai_construct.lua's jobSlot
-- (#805): walls/posts fall back to the same "ne"/"n" default used when
-- no edge is recorded.
local function structureCommitSlot(target)
    if target.piece == "floor" then return "floor"
    elseif target.piece == "ceiling" then return "ceiling"
    elseif target.piece == "wall" then return "wall_" .. (target.edge or "ne")
    elseif target.piece == "post" then return "post_" .. (target.edge or "n")
    elseif target.piece == "wire" then return "wire"
    end
    return nil
end

-- Best-effort prediction of whether the commit about to be queued has AT
-- LEAST one free tile, so the buildTool.commitPlacement outcome below
-- doesn't claim "accepted" when the backend's occupancy filter (#805)
-- will actually create zero jobs. Doesn't replicate the backend's
-- z-level/unloaded-chunk filtering (an existing, unrelated gap) —
-- occupancy is the one thing this record needs to get right.
local function structureCommitHasFreeSlot(target, x1, y1, x2, y2)
    local slot = structureCommitSlot(target)
    if not slot then return true end
    for gx = math.min(x1, x2), math.max(x1, x2) do
        for gy = math.min(y1, y2), math.max(y1, y2) do
            if not structure.hasAt(gx, gy, slot) then return true end
        end
    end
    return false
end

-----------------------------------------------------------
-- Placement mode
-----------------------------------------------------------
function buildTool.enterPlacement(target)
    destroyPicker()
    -- Drop any ghost left over from a previous building target — a
    -- structure target drives no ghost of its own (update() below).
    building.clearGhost()
    buildTool.state.mode          = "placement"
    buildTool.state.target        = target
    buildTool.state.anchor        = nil
    buildTool.state.lastHoverTile = nil
    local wid = buildTool.hud and buildTool.hud.worldId
    if wid then construction.setLineMode(wid, isWirePath(target)) end
end

function buildTool.exitPlacement()
    building.clearGhost()
    local wid = buildTool.hud and buildTool.hud.worldId
    if buildTool.state.anchor and wid then construction.clearAnchor(wid) end
    if wid then construction.setLineMode(wid, false) end
    buildTool.state.mode          = "off"
    buildTool.state.target        = nil
    buildTool.state.anchor        = nil
    buildTool.state.lastHoverTile = nil
end

-----------------------------------------------------------
-- Per-tick: drive the ghost preview while in placement mode. Only the
-- building targets have a ghost — a structure rectangle previews via
-- the engine's anchor→hover render (constructAnchor, keyed off this
-- tool's ToolMode, see World/Render/Quads.hs), driven entirely by the
-- setAnchor/designate calls in handleMouseDown below.
-----------------------------------------------------------
function buildTool.update(dt)
    if buildTool.state.mode ~= "placement" then return end

    local target = buildTool.state.target
    if not target or target.kind ~= "building" then return end

    -- Drive the preview from the SAME synchronous hit-test the placement
    -- click uses (world.pickTile of the live cursor), not the async
    -- world.getHoverTile() cache. Otherwise, after a fast cursor move the
    -- ghost (cache, lagging the ~0.1s UI tick) and the click (live pick)
    -- could resolve different tiles, so the preview would lie (issue #66).
    local mx, my = engine.getMousePosition()
    local gx, gy
    if mx and my then
        gx, gy = world.pickTile(mx, my)
    end
    if not gx or not gy then
        building.clearGhost()
        -- No current hover tile: drop the cached placement target too, so a
        -- later off-world click can't place on stale coordinates (issue #66).
        buildTool.state.lastHoverTile = nil
        return
    end

    local igx = math.floor(gx)
    local igy = math.floor(gy)

    local valid = building.canPlaceAt(target.def, igx, igy)
    building.setGhost(target.def, igx, igy, valid)
    buildTool.state.lastHoverTile = { igx, igy }
end

-- One selected unit carrying at least one instance of defName, or nil.
-- Mirrors cargo_inventory_panel.lua's adjacentSelectedUnit pattern for
-- sourcing a player-issued inventory action from the current selection
-- — no adjacency requirement here (ghost placement is a top-down
-- action like every other building.spawn placement, not a "walk over
-- there" one).
local function carryingSelectedUnit(defName)
    local sel = unit.getSelected() or {}
    for _, uid in ipairs(sel) do
        for _, it in ipairs(unit.getInventory(uid) or {}) do
            if it.defName == defName then return uid end
        end
    end
    return nil
end

-- Commit a power-node placement at an already hit-tested tile. Power
-- items (#358: solar_panel / high_voltage_battery) consume a matching
-- item off a selected unit via power.placeNode. Non-power building
-- targets are handled by the caller according to the normal build-tool
-- rules (starting building spawn vs. construction designation).
--
-- Returns the placed building id, or nil + a reason on failure.
-- F4 (#646): every exit records its outcome so a silent placement
-- rejection (no carrying unit, item exhausted) is visible to the
-- playtest oracle even though the player only ever sees the
-- engine.logInfo line at the handleMouseDown call site below.
function buildTool.commitPlacement(defName, gx, gy)
    if not power.isPlaceable(defName) then
        debug.recordOutcome{
            kind = "buildTool.commitPlacement", outcome = "rejected",
            where = { x = gx, y = gy },
            reason = "not a placeable power item: " .. tostring(defName),
        }
        return nil, "not a placeable power item"
    end
    local uid = carryingSelectedUnit(defName)
    if not uid then
        debug.recordOutcome{
            kind = "buildTool.commitPlacement", outcome = "rejected",
            where = { x = gx, y = gy },
            reason = "no selected unit carries " .. tostring(defName),
        }
        return nil, "no selected unit carries " .. defName
    end
    local nodeId, buildingIdOrErr = power.placeNode(uid, defName, gx, gy)
    if not nodeId then
        debug.recordOutcome{
            kind = "buildTool.commitPlacement", outcome = "rejected",
            where = { x = gx, y = gy }, target = uid,
            reason = tostring(buildingIdOrErr),
        }
        return nil, buildingIdOrErr
    end
    debug.recordOutcome{
        kind = "buildTool.commitPlacement", outcome = "accepted",
        where = { x = gx, y = gy }, target = uid,
    }
    return buildingIdOrErr
end

-- Commit the starting building (portal) at an already hit-tested,
-- already-canPlaceAt-validated tile: spawn it, record the outcome,
-- and exit placement back to the default tool. Shared by the direct
-- single-click path below AND the remote-settlement confirmation
-- modal's "Establish Here" handler (#779) — the SAME code runs
-- exactly once regardless of which path reached it, so a successful
-- confirmation follows the ordinary successful-placement path exactly
-- once, per the issue's own requirement.
function buildTool.commitStartingPlacement(defName, gx, gy)
    local id = building.spawn(defName, gx, gy)
    if id then
        engine.logInfo("BuildTool: placed " .. defName ..
            " (id=" .. tostring(id) ..
            ") at " .. gx .. "," .. gy)
        debug.recordOutcome{
            kind = "buildTool.commitPlacement",
            outcome = "accepted",
            where = { x = gx, y = gy },
        }
    else
        debug.recordOutcome{
            kind = "buildTool.commitPlacement",
            outcome = "rejected",
            where = { x = gx, y = gy },
            reason = "building.spawn failed",
        }
    end
    buildTool.exitPlacement()
    if buildTool.hud and buildTool.hud.selectDefaultTool then
        buildTool.hud.selectDefaultTool()
    end
end

-----------------------------------------------------------
-- Mouse hooks. Called from init.lua's onMouseDown.
-- Return true if we consumed the click.
--
-- Named handle* (not on*) deliberately: this module is engine-loaded
-- (loadScript), so an on*-named function would ALSO be fired directly
-- by every engine broadcast — double-firing on top of init.lua's
-- ordered forward. handle* keeps it forward-only (same convention as
-- debug.lua's tryClaimClick).
-----------------------------------------------------------
function buildTool.handleMouseDown(button, x, y)
    if buildTool.state.mode ~= "placement" then return false end

    local target = buildTool.state.target
    if not target then return false end

    if button == MOUSE_LEFT then
        -- Hit-test the ACTUAL click coordinates synchronously rather than
        -- trusting the cached hover tile. getHoverTile()/lastHoverTile are
        -- only refreshed on the periodic update() tick (hud.update pushes
        -- the cursor pos every ~0.1s, then the render thread resolves it),
        -- so a click that arrives after a fast cursor move off-world would
        -- otherwise place on a stale on-world tile (issue #66). pickTile
        -- runs the hit-test now against live camera/window/tile state.
        local gx, gy = world.pickTile(x, y)
        if not gx or not gy then
            buildTool.state.lastHoverTile = nil
            -- F4 (#646): the routing chain (init_mouse.lua) already
            -- records "build_tool" consumed this click; without this,
            -- an off-world click during placement looks accepted with
            -- no corresponding Layer B rejection (review round 1).
            debug.recordOutcome{
                kind = "buildTool.commitPlacement", outcome = "rejected",
                reason = "off-world click during placement",
            }
            return true
        end
        local igx = math.floor(gx)
        local igy = math.floor(gy)
        buildTool.state.lastHoverTile = { igx, igy }

        if target.kind == "building" then
            local valid, invalidReason = building.canPlaceAt(target.def, igx, igy)
            if valid then
                if power.isPlaceable(target.def) then
                    local id, err = buildTool.commitPlacement(target.def, igx, igy)
                    if id then
                        engine.logInfo("BuildTool: placed " .. target.def ..
                            " (id=" .. tostring(id) ..
                            ") at " .. igx .. "," .. igy)
                        buildTool.exitPlacement()
                        if buildTool.hud and buildTool.hud.selectDefaultTool then
                            buildTool.hud.selectDefaultTool()
                        end
                    else
                        -- Stay in placement (same as an invalid-tile click)
                        -- so the player can select a carrying unit and retry.
                        engine.logInfo("BuildTool: could not place " ..
                            target.def .. (err and (": " .. tostring(err)) or ""))
                    end
                elseif target.isStarting then
                    -- The portal. #779: a valid position more than the
                    -- remote threshold from every placed location on
                    -- this page (or with no placed locations at all)
                    -- pauses for an explicit confirmation instead of
                    -- spawning instantly — placement stays armed, no
                    -- exitPlacement/selectDefaultTool here, matching
                    -- "opening the warning does not exit placement
                    -- mode". A non-remote position keeps the old
                    -- single-click instant-spawn behavior via the
                    -- shared commitStartingPlacement helper.
                    local remote, distance, thresholdTiles =
                        building.remoteCheck(target.def, igx, igy)
                    if remote then
                        require("scripts.build_tool_remote_warning").open(
                            target.def, igx, igy, distance, thresholdTiles)
                    else
                        buildTool.commitStartingPlacement(
                            target.def, igx, igy)
                    end
                else
                    -- Everything else designates a job for the build AI
                    -- (#96) rather than placing instantly. Stays armed so
                    -- the player can place more without reopening the
                    -- picker (matches the old construct_tool).
                    local wid = buildTool.hud and buildTool.hud.worldId
                    if wid then
                        construction.designate(wid, igx, igy, igx, igy,
                            "building", target.def)
                        debug.recordOutcome{
                            kind = "buildTool.commitPlacement",
                            outcome = "accepted", where = { x = igx, y = igy },
                            reason = "routed to construction.designate",
                        }
                    else
                        debug.recordOutcome{
                            kind = "buildTool.commitPlacement",
                            outcome = "rejected", where = { x = igx, y = igy },
                            reason = "no active world id",
                        }
                    end
                end
            else
                -- F4 (#646): building.canPlaceAt refused the tile —
                -- previously silent beyond the placement mode simply
                -- staying open (review round 1). #778: forward the
                -- specific reason (e.g. "inside a location's bounds")
                -- instead of a generic placeholder, so it distinguishes
                -- location overlap from uneven ground / unloaded terrain
                -- / existing occupation.
                debug.recordOutcome{
                    kind = "buildTool.commitPlacement", outcome = "rejected",
                    where = { x = igx, y = igy },
                    reason = invalidReason or "invalid placement tile",
                }
            end
        else -- "structure": DF-style two-click rectangle (or, for wire, a
             -- two-click straight PATH — see isWirePath/setLineMode above)
            local wid = buildTool.hud and buildTool.hud.worldId
            if wid then
                if not buildTool.state.anchor then
                    buildTool.state.anchor = { igx, igy }
                    construction.setAnchor(wid, igx, igy)
                else
                    local a = buildTool.state.anchor
                    local x2, y2 = igx, igy
                    if isWirePath(target) then
                        x2, y2 = buildTool.snapWirePath(a[1], a[2], igx, igy)
                    end
                    local hasFreeSlot = structureCommitHasFreeSlot(
                        target, a[1], a[2], x2, y2)
                    construction.designate(wid, a[1], a[2], x2, y2,
                        "structure", target.pack, target.piece, target.edge)
                    buildTool.state.anchor = nil
                    if hasFreeSlot then
                        debug.recordOutcome{
                            kind = "buildTool.commitPlacement", outcome = "accepted",
                            where = { x = x2, y = y2 },
                            reason = "routed to construction.designate",
                        }
                    else
                        debug.recordOutcome{
                            kind = "buildTool.commitPlacement", outcome = "rejected",
                            where = { x = x2, y = y2 },
                            reason = "requested structure slot(s) already occupied",
                        }
                    end
                end
            else
                debug.recordOutcome{
                    kind = "buildTool.commitPlacement", outcome = "rejected",
                    where = { x = igx, y = igy },
                    reason = "no active world id",
                }
            end
        end
        return true
    elseif button == MOUSE_RIGHT then
        if target.kind == "building" and target.isStarting then
            -- No designation to erase on the instant path — right-click
            -- just backs out of placement, same as before #403.
            buildTool.exitPlacement()
            if buildTool.hud and buildTool.hud.selectDefaultTool then
                buildTool.hud.selectDefaultTool()
            end
        elseif buildTool.state.anchor then
            -- First right-click cancels a pending structure rectangle.
            buildTool.state.anchor = nil
            local wid = buildTool.hud and buildTool.hud.worldId
            if wid then construction.clearAnchor(wid) end
        else
            -- Otherwise erase the designation under the cursor (works for
            -- both a non-starting building and a structure target).
            local gx, gy = world.pickTile(x, y)
            if gx and gy then
                gx, gy = math.floor(gx), math.floor(gy)
                local wid = buildTool.hud and buildTool.hud.worldId
                if wid then
                    local constructAi = require("scripts.unit_ai_construct")
                    -- #799 review round 5: an atomic engine-side pop-and-
                    -- return replaces the old getDesignationAt + queued
                    -- cancelDesignation pair (and the Lua-side debounce
                    -- table it needed) — the ATOMIC delete is what
                    -- actually serializes competing cancellations (a
                    -- rapid double right-click, or a new designation
                    -- quickly replacing the old one at the same tile),
                    -- which no Lua-side timing heuristic could replicate.
                    local removed = construction.cancelDesignationForRefund(wid, gx, gy)
                    -- No-silent-loss policy: a structure designation
                    -- whose materials were already paid must not just
                    -- vanish — return them to the ground. Buildings never
                    -- consume through this path (a separate delivered-
                    -- material system), so job.category filters them out.
                    if removed and removed.paid then
                        constructAi.refundStructureMaterials(removed)
                    end
                    -- Interrupt any live claimant so it can't keep
                    -- ticking progress on its own cached copy of the
                    -- now-cancelled job and still place the piece.
                    constructAi.abandonClaim(gx, gy)
                end
            end
        end
        return true
    end

    return false
end

-- Same handle*-not-on* rationale as handleMouseDown above.
function buildTool.handleKeyDown(key)
    if key == "Escape" and buildTool.state.mode ~= "off" then
        buildTool.hidePicker()
        buildTool.exitPlacement()
        if buildTool.hud and buildTool.hud.selectDefaultTool then
            buildTool.hud.selectDefaultTool()
        end
        return true
    end
    return false
end

-----------------------------------------------------------
-- Tool-mode change callback. Wired up by hud.lua via setup().
-----------------------------------------------------------
function buildTool.onToolMode(toolName)
    if toolName == "tool_build" then
        buildTool.showPicker()
    else
        buildTool.hidePicker()
        buildTool.exitPlacement()
    end
end

-----------------------------------------------------------
-- Engine script hooks (loaded via engine.loadScript)
-----------------------------------------------------------
function buildTool.init(scriptId)
    engine.logInfo("Build tool initializing...")
end

function buildTool.shutdown()
    destroyPicker()
    buildTool.exitPlacement()
    engine.logInfo("Build tool shut down")
end

return buildTool
