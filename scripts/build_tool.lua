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
--     the default tool.
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

-- Full picker entry list: buildings + (once a starting building has been
-- placed) the structure pieces.
local function visibleEntries()
    local entries, anyStartingPlaced = visibleBuildingEntries()
    if anyStartingPlaced then
        for _, e in ipairs(structureEntries()) do
            table.insert(entries, e)
        end
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
    local iconSize    = math.floor(ICON_SIZE_BASE * uiscale)
    local iconGap     = math.floor(ICON_GAP_BASE  * uiscale)
    local catDefs     = defsInCategory(visible, buildTool.state.activeCategory)
    local whiteTex    = buildTool.whitePixelTex

    for i, d in ipairs(catDefs) do
        local col = (i - 1) % ICONS_PER_ROW
        local row = math.floor((i - 1) / ICONS_PER_ROW)
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
local function buildTabStrip(cats, stripX, stripY)
    destroyTabs()

    local h = buildTool.hud
    if not h then return end

    local uiscale  = scale.get()
    local tabH     = math.floor(TAB_H_BASE     * uiscale)
    local tabGap   = math.floor(TAB_GAP_BASE   * uiscale)
    local tabPadX  = math.floor(TAB_PAD_X_BASE * uiscale)
    local fontPx   = math.floor(TAB_FONT_SIZE  * uiscale)
    local whiteTex = buildTool.whitePixelTex

    local cursorX = stripX
    for _, cat in ipairs(cats) do
        local labelW = engine.getTextWidth(h.menuFont, cat, fontPx) or 0
        local tabW   = labelW + 2 * tabPadX
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

        cursorX = cursorX + tabW + tabGap
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

    -- Panel sizing. Width is fixed; height depends on how many rows
    -- of icons we need for the active category.
    local uiscale  = scale.get()
    local iconSize = math.floor(ICON_SIZE_BASE * uiscale)
    local iconGap  = math.floor(ICON_GAP_BASE  * uiscale)
    local tabH     = math.floor(TAB_H_BASE     * uiscale)
    local activeCatDefs = defsInCategory(visible, buildTool.state.activeCategory)
    -- Always show at least one slot of vertical space so the panel
    -- isn't a flat strip when a category is empty.
    local rowCount = math.max(1,
        math.ceil(#activeCatDefs / ICONS_PER_ROW))
    local iconAreaH = rowCount * iconSize + math.max(0, rowCount - 1) * iconGap

    local pickerW = math.floor(PICKER_W_BASE * uiscale)
    -- Tab-strip + small gap + icon area + panel padding.
    local tabGapBelow = math.floor(8 * uiscale)
    local pickerH = math.floor((PANEL_PAD_TOP + PANEL_PAD_BOTTOM) * uiscale)
                  + tabH + tabGapBelow + iconAreaH

    -- Anchor to the build button (same geometry as before so the
    -- visual position doesn't shift from old to new picker).
    local margin       = math.floor(16 * uiscale)
    local buttonSize   = h.baseSizes and h.baseSizes.buttonSize
    local btnSize      = buttonSize and math.floor(buttonSize * uiscale) or 64
    local stackGap     = math.floor(8 * uiscale)
    local pickerX      = margin + btnSize + stackGap
    local buildBtnTopY = h.fbH - margin - 2 * btnSize - stackGap
    local pickerY      = buildBtnTopY

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

    buildTabStrip(cats, contentX, contentY)

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
end

function buildTool.exitPlacement()
    building.clearGhost()
    if buildTool.state.anchor then
        local wid = buildTool.hud and buildTool.hud.worldId
        if wid then construction.clearAnchor(wid) end
    end
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
function buildTool.commitPlacement(defName, gx, gy)
    if not power.isPlaceable(defName) then
        return nil, "not a placeable power item"
    end
    local uid = carryingSelectedUnit(defName)
    if not uid then
        return nil, "no selected unit carries " .. defName
    end
    local nodeId, buildingIdOrErr = power.placeNode(uid, defName, gx, gy)
    if not nodeId then return nil, buildingIdOrErr end
    return buildingIdOrErr
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
            return true
        end
        local igx = math.floor(gx)
        local igy = math.floor(gy)
        buildTool.state.lastHoverTile = { igx, igy }

        if target.kind == "building" then
            local valid = building.canPlaceAt(target.def, igx, igy)
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
                    -- The portal: bootstrap building, still instant.
                    local id = building.spawn(target.def, igx, igy)
                    if id then
                        engine.logInfo("BuildTool: placed " .. target.def ..
                            " (id=" .. tostring(id) ..
                            ") at " .. igx .. "," .. igy)
                    end
                    buildTool.exitPlacement()
                    if buildTool.hud and buildTool.hud.selectDefaultTool then
                        buildTool.hud.selectDefaultTool()
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
                    end
                end
            end
        else -- "structure": DF-style two-click rectangle
            local wid = buildTool.hud and buildTool.hud.worldId
            if wid then
                if not buildTool.state.anchor then
                    buildTool.state.anchor = { igx, igy }
                    construction.setAnchor(wid, igx, igy)
                else
                    local a = buildTool.state.anchor
                    construction.designate(wid, a[1], a[2], igx, igy,
                        "structure", target.pack, target.piece, target.edge)
                    buildTool.state.anchor = nil
                end
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
                construction.cancelDesignation(math.floor(gx), math.floor(gy))
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
