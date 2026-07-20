-- Tile Editor
--
-- Arena-only popup that appears below the info panel when the player
-- info-clicks a tile. Provides a "Delete Tile" button that digs the
-- top of the column down by 1 Z.
--
-- Lifecycle:
--   * test_arena enters → setArenaActive(true)        (popup armed)
--   * Player switches to info tool                    (popup stays armed)
--   * Player info-clicks a tile → onTileSelected      (popup shown / refreshed)
--   * Player info-clicks another tile → onTileSelected (popup refreshes coords)
--   * Player clears tile selection (info panel empties) → onSetInfoText("") → hide
--   * Player selects a CHUNK (zoom-map) → onSetInfoText(_,_,"chunk") → hide
--     (the popup is a zoomed-in tile tool; a chunk selection means the
--      tile is no longer the focus, and the selection commit clears the
--      underlying tile — issue #135 — so the popup must not linger and
--      reappear stale when the zoomed-in page is shown again)
--   * Player leaves info tool → onToolMode("tool_default") → hide
--   * test_arena exits → setArenaActive(false)        (popup torn down, disarmed)
--
-- Singleton via package.loaded so the engine-ticked update() sees the
-- same state that the hud's click hook mutates. Same pattern as
-- scripts/build_tool.lua and scripts/unit_ai.lua.

local tileEditor = package.loaded["scripts.tile_editor"] or {}
package.loaded["scripts.tile_editor"] = tileEditor

local panel      = require("scripts.ui.panel")
local button     = require("scripts.ui.button")
local scale      = require("scripts.ui.scale")
local responsive = require("scripts.ui.responsive")

-- Module state. Hung off the module table so it survives reloads.
tileEditor.state = tileEditor.state or {
    arenaMode = false,    -- only show in the test arena
    active    = false,    -- popup currently visible
    gx        = nil,
    gy        = nil,
    panelId   = nil,
    deleteBtn = nil,
}

-- HUD context, set by hud.lua at boot.
tileEditor.hud = nil

-----------------------------------------------------------
-- HUD hookup (called once from hud.createUI after the info panel)
-----------------------------------------------------------
function tileEditor.setup(opts)
    tileEditor.hud = opts
end

-----------------------------------------------------------
-- Arena mode toggle (called by test_arena on enter/exit)
-----------------------------------------------------------
function tileEditor.setArenaActive(on)
    tileEditor.state.arenaMode = on and true or false
    if not on then
        tileEditor.clear()
    end
end

-----------------------------------------------------------
-- Internal: destroy popup UI elements
-----------------------------------------------------------
local function destroyPopup()
    local s = tileEditor.state
    if s.deleteBtn then button.destroy(s.deleteBtn); s.deleteBtn = nil end
    if s.panelId   then panel.destroy(s.panelId);    s.panelId   = nil end
    s.active = false
    s.gx, s.gy = nil, nil
end

function tileEditor.clear()
    destroyPopup()
end

-----------------------------------------------------------
-- Compute popup geometry — below the info panel in the top-right.
-- The info panel uses widthFrac=0.20, heightFrac=0.33 of the
-- framebuffer with `margin` outside; mirror that math here so we
-- align with whatever the info panel chose at its last create call.
-----------------------------------------------------------
local function popupBounds()
    local h = tileEditor.hud
    if not h or not h.fbW or not h.fbH then return nil end
    local uiscale = scale.get()
    local marginBase = 16
    local margin = math.floor(marginBase * uiscale)
    local widthFrac  = 0.20
    local heightFrac = 0.33
    local pW = math.floor(h.fbW * widthFrac)
    local pH = math.floor(h.fbH * heightFrac)
    local pX = h.fbW - pW - margin
    local infoBottomY = margin + pH
    local gap = math.floor(8 * uiscale)
    -- Reserve enough vertical room for ~3 buttons in a future revision.
    local popupH = math.floor(120 * uiscale)
    return {
        x        = pX,
        y        = infoBottomY + gap,
        width    = pW,
        height   = popupH,
        uiscale  = uiscale,
    }
end

-----------------------------------------------------------
-- Show or refresh the popup at (gx, gy)
-----------------------------------------------------------
local function rebuild(gx, gy)
    local h = tileEditor.hud
    if not h or not h.page then return end
    destroyPopup()

    local b = popupBounds()
    if not b then return end

    local s = tileEditor.state
    s.gx, s.gy = gx, gy

    s.panelId = panel.new({
        name       = "tile_editor_panel",
        page       = h.page,
        x          = b.x,
        y          = b.y,
        width      = b.width,
        height     = b.height,
        textureSet = h.boxTexSet,
        color      = {0.1, 0.1, 0.1, 0.9},
        tileSize   = 64,
        zIndex     = 120,
        padding    = { top = 12, bottom = 12, left = 12, right = 12 },
        uiscale    = b.uiscale,
    })

    local pbounds = panel.getContentBounds(s.panelId)
    -- One row of buttons. Future-proof horizontally — for now, one
    -- centered button. Width is sized so the "Delete Tile" label sits
    -- comfortably inside the box without clipping the texture edges.
    local btnWidthBase  = 320
    local btnHeightBase = 40
    -- #750 round-15 review: the panel is width-fractional (mirrors
    -- info_panel.lua's own 20%-of-framebuffer sizing), but the button
    -- stayed a fixed 320-base-unit width regardless — at the issue's
    -- own 800x2160@4x, pbounds.width is ~64px while the button rendered
    -- at 1280px, placing the popup's only action off-screen. Shrink one
    -- effective, LOCAL uiscale for the button so both its box AND its
    -- text (button.new uses the SAME uiscale for width/height/fontSize
    -- internally, so this can't repeat round-13's "box shrinks, text
    -- doesn't" bug) fit within the panel's actual content width.
    local btnUiscale = responsive.fitScale(
        btnWidthBase * b.uiscale, math.max(20, pbounds.width), b.uiscale)
    local btnW = math.floor(btnWidthBase  * btnUiscale)
    local btnH = math.floor(btnHeightBase * btnUiscale)
    local btnX = b.x + pbounds.x + math.floor((pbounds.width  - btnW) / 2)
    local btnY = b.y + pbounds.y + math.floor((pbounds.height - btnH) / 2)

    s.deleteBtn = button.new({
        name       = "tile_editor_delete_btn",
        text       = "Delete Tile",
        x          = btnX,
        y          = btnY,
        width      = btnWidthBase,
        height     = btnHeightBase,
        fontSize   = 18,
        uiscale    = btnUiscale,
        page       = h.page,
        font       = h.menuFont,
        textureSet = h.boxTexSet,
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textColor  = {1.0, 1.0, 1.0, 1.0},
        zIndex     = 121,
        onClick    = function()
            local cur = tileEditor.state
            if cur.gx and cur.gy then
                -- Read worldId dynamically: hud.worldId is mutated per
                -- context by ui_manager (main_world vs test_arena), so
                -- the value captured at setup() time can be stale.
                local hudMod  = require("scripts.hud")
                local worldId = (hudMod and hudMod.worldId) or "test_arena"
                world.deleteTile(worldId, cur.gx, cur.gy)
            end
        end,
    })

    s.active = true
end

-----------------------------------------------------------
-- Public hooks
-----------------------------------------------------------

-- Called from hud.lua when an info-tool click selects a tile.
function tileEditor.onTileSelected(gx, gy)
    if not tileEditor.state.arenaMode then
        engine.logDebug("tile_editor: onTileSelected ignored (arenaMode=false)")
        return
    end
    if not gx or not gy then return end
    engine.logInfo("tile_editor: showing popup at " .. gx .. "," .. gy)
    rebuild(gx, gy)
end

-- Called from hud.lua's tool toggle onChange. itemName is "tool_info",
-- "tool_default", "tool_build", etc. We stay open for tool_info /
-- tool_default (the info tool sits under default in the toggle); any
-- other tool tears the popup down.
function tileEditor.onToolMode(itemName)
    if itemName == "tool_info" or itemName == "tool_default" then
        return
    end
    destroyPopup()
end

-- Broadcast from the engine when the HUD info text changes. `kind` is
-- "tile" | "chunk" (the selection that produced it). Empty `basic` means
-- "tile selection cleared" — couple our popup visibility to that signal
-- as user-spec 5 requires. A "chunk" payload means a zoom-map chunk was
-- selected, which clears the underlying tile (issue #135); the popup is a
-- zoomed-in tile tool, so tear it down too rather than let it reappear
-- stale when the zoomed-in page is next shown.
function tileEditor.onSetInfoText(basic, advanced, kind)
    if not basic or basic == "" or kind == "chunk" then
        destroyPopup()
    end
end

-----------------------------------------------------------
-- Engine script hooks
-----------------------------------------------------------
function tileEditor.init(scriptId)
    engine.logInfo("Tile editor initializing...")
end

function tileEditor.update(dt)
    -- No per-tick work yet. The popup is event-driven (onTileSelected,
    -- onSetInfoText, onToolMode); leaving this empty avoids spending
    -- ticks rechecking state that hasn't changed.
end

function tileEditor.shutdown()
    destroyPopup()
    engine.logInfo("Tile editor shut down")
end

return tileEditor
