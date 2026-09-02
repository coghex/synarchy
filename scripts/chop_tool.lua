-- Chop designation tool (#97, re-shaped by #1856 into a press-drag box).
--
--   * Toolbar "tool_chop" puts the world in chop tool mode; the world
--     render pass shows the hover cursor.
--   * LEFT press-drag ADDS designations, RIGHT press-drag ERASES them.
--     The two are symmetric: same threshold, same visible box, same
--     screen-space oracle, opposite effect (D-12).
--   * Below the shared four-pixel threshold the gesture is a CLICK and
--     acts on the topmost eligible tree sprite under the pointer; past
--     it, it is a BOX and acts on every eligible tree whose rendered
--     ground-contact anchor lies inside it.
--   * Selection is SCREEN-SPACE, not a tile rectangle (D-9): a tile
--     rectangle and the box the player drew disagree around cliffs,
--     because elevation, sub-tile offsets and sprite geometry move a
--     tree's rendered position away from its tile's. The engine-side
--     oracle (World.Flora.HitTest) derives its geometry from the same
--     values the renderer draws with.
--   * Eligibility is unchanged: a wood-tagged harvestable species with
--     no live regrowth timer. Each committed tree carries its stable
--     FloraInstanceId (#1854), so two trees on one tile are two
--     independent designations, and the chop AI
--     (scripts/unit_ai.lua) fells exactly the one it claimed.
--   * There is no ANCHOR and no pending state to cancel: a gesture is
--     resolved entirely at its own release. Escape, a tool switch, a
--     view transition and a focus-loss release each leave nothing
--     half-committed, because there is nothing to leave.
--
-- The gesture machinery — the threshold, the fast-release
-- classification, the visible rect, the teardown and the one-record
-- action-outcome path — is unit_drag_select.lua's, reused through
-- armToolBox rather than duplicated (#730's DRAG_THRESHOLD is the one
-- threshold in the game).
--
-- Handlers are named handle* (not on*) deliberately: this module is
-- forwarded to from init.lua's ordered chains; on*-named functions
-- would ALSO fire on every engine broadcast and double-handle clicks
-- (same convention as mine_tool.lua / build_tool.lua).
local chopTool = {}

-- Mouse button codes are module-locals by convention (each script
-- defines its own — they are NOT globals; see build_tool.lua).
local MOUSE_LEFT  = 1
local MOUSE_RIGHT = 2

-- The harvest tag a chop designation targets. Unchanged from the
-- two-click rectangle this replaced.
local CHOP_TAG = "wood"

chopTool.hud = nil   -- hud module reference (read worldId at call time)

function chopTool.setup(ctx)
    chopTool.hud = ctx.hud
end

local function worldId()
    return chopTool.hud and chopTool.hud.worldId
end

-- The tool only acts in the zoomed-in world view with the chop tool
-- selected; everywhere else the chains fall through to other handlers.
local function active()
    return worldId() ~= nil
       and chopTool.hud.currentView == "zoomed_in"
       and world.getToolMode and world.getToolMode() == "chop"
end

-- Both gestures are the same shape, so both effects are built the same
-- way — only the pair of engine verbs differs.
local function effect(handler, atFn, inRectFn)
    return {
        handler = handler,
        onClick = function(x, y)
            local wid = worldId()
            if not wid then return 0 end
            return atFn(wid, x, y, CHOP_TAG) or 0
        end,
        onBox = function(x1, y1, x2, y2)
            local wid = worldId()
            if not wid then return 0 end
            return inRectFn(wid, x1, y1, x2, y2, CHOP_TAG) or 0
        end,
    }
end

function chopTool.handleMouseDown(button, x, y)
    if not active() then return false end

    if button == MOUSE_LEFT then
        require("scripts.unit_drag_select").armToolBox(MOUSE_LEFT,
            effect("chop_tool", chop.designateAt, chop.designateInRect))
        return true
    elseif button == MOUSE_RIGHT then
        require("scripts.unit_drag_select").armToolBox(MOUSE_RIGHT,
            effect("chop_tool_erase", chop.eraseAt, chop.eraseInRect))
        return true
    end

    return false
end

-- Abandon any in-flight gesture without committing it. Idempotent —
-- there is no pending anchor to clear, only an armed box effect, and
-- unit_drag_select's own teardown resolves the gesture that carried it.
function chopTool.cancel()
    local ok, dragSelect = pcall(require, "scripts.unit_drag_select")
    if ok and dragSelect and dragSelect.disarmToolBox then
        -- Through disarmToolBox, not a bare toolBox clear: the effect
        -- owns the visible rect and the press's deferred record, so
        -- dropping only the effect leaves a box painted over the world
        -- and a cancelled click recorded as accepted.
        dragSelect.disarmToolBox(MOUSE_LEFT)
        dragSelect.disarmToolBox(MOUSE_RIGHT)
    end
end

function chopTool.handleKeyDown(key)
    -- Escape has nothing to cancel now that the gesture carries no
    -- pending anchor: it resolves at its own release. Disarming the
    -- effect still keeps a held button from committing after the
    -- player has said no.
    if key == "Escape" and active() then
        chopTool.cancel()
        return false
    end
    return false
end

-- Tool-mode change callback, wired from hud.lua's toolbar onChange
-- (direct call, not a broadcast — safe under the on* convention).
function chopTool.onToolMode(toolName)
    if toolName ~= "tool_chop" then
        chopTool.cancel()
    end
end

package.loaded["scripts.chop_tool"] = chopTool
return chopTool
