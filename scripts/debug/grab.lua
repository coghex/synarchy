-- Ground-item Grab gesture (#2489). Require-only: debug.lua owns hooks,
-- the button and armed mode. Both raw press subscribers share claim().
local M = {}
local capture
local claims = {}

-- Offset math uses the same three seam images as localizeTileToAnchor,
-- but never rounds the sub-tile positions. Arenas report width zero.
local function localize(ax, ay, x, y, width)
    if width <= 0 then return x, y end
    local step = width / 2
    local bx, by = x, y
    local best = math.max(math.abs(x - ax), math.abs(y - ay))
    for _, k in ipairs({-1, 1}) do
        local cx, cy = x + k * step, y - k * step
        local distance = math.max(math.abs(cx - ax), math.abs(cy - ay))
        if distance < best then bx, by, best = cx, cy, distance end
    end
    return bx, by
end

local function rowOnPage(page, gid)
    if world.getActiveWorldId() ~= page then return nil end
    local found
    for _, row in ipairs(item.listGround() or {}) do
        if row.id == gid then found = row; break end
    end
    if world.getActiveWorldId() ~= page then return nil end
    return found
end

local function pointerAllowed(overlay, x, y)
    if not overlay.visible or not overlay.inGameplayView()
       or not require("scripts.ui_manager").isGameplayInputActive() then
        return false
    end
    local ww, wh = engine.getWindowSize()
    local fw, fh = engine.getFramebufferSize()
    if not ww or not wh or not fw or not fh
       or ww <= 0 or wh <= 0 or fw <= 0 or fh <= 0
       or x < 0 or y < 0 or x >= ww or y >= wh then return false end
    return not UI.isPointerBlockedAt(x * fw / ww, y * fh / wh)
end

function M.cancel()
    capture = nil
    -- A cancellation can precede release, or no release may ever arrive.
    -- Resolve only the outcome this tool owns; later releases are inert.
    local ds = package.loaded["scripts.unit_drag_select"]
    if ds and ds.pendingClick and ds.pendingClick.handler == "debug_grab" then
        ds.cancel(1)
    end
    -- Keep press claims until physical release so the second subscriber
    -- cannot fall through after a right-click has already disarmed Grab.
end

function M.claim(overlay, button, x, y)
    local prior = claims[button]
    if prior and prior.x == x and prior.y == y then return true end
    if not overlay.armedGrab then return false end
    if button == 2 then
        overlay.clearArmedGrab()
        claims[button] = {x = x, y = y}
        return true
    end
    if button ~= 1 or not pointerAllowed(overlay, x, y) then return false end
    claims[button] = {x = x, y = y}
    capture = nil
    -- Even an empty press is owned. A hit without a live ground pick
    -- cannot establish an offset and remains an owned no-op.
    local page = world.getActiveWorldId()
    local gid = item.hitTestAt(x, y)
    if not page or not gid then return true end
    local row = rowOnPage(page, gid)
    local px, py, pickedPage, generation = world.pickPos(x, y)
    if not row or not row.instanceId or not px or not py
       or pickedPage ~= page or world.getActiveWorldId() ~= page then return true end
    local width = world.getWrapWidth(page)
    local ix, iy = localize(px, py, row.x, row.y, width)
    capture = {page = page, gid = gid, instanceId = row.instanceId,
        generation = generation, width = width, x = ix, y = iy,
        dx = px - ix, dy = py - iy}
    return true
end

local function sample(overlay, x, y)
    local c = capture
    if not c then return end
    if not overlay.armedGrab or not overlay.visible or not overlay.inGameplayView()
       or world.getActiveWorldId() ~= c.page then M.cancel(); return end
    -- The move verb's boolean does not distinguish invalid terrain from
    -- a vanished/replaced item. Observe identity separately, on its page.
    local row = rowOnPage(c.page, c.gid)
    if not row or row.instanceId ~= c.instanceId then M.cancel(); return end
    if not pointerAllowed(overlay, x, y) then return end
    local px, py, page, generation = world.pickPos(x, y)
    if not px or not py then return end
    if page ~= c.page or generation ~= c.generation
       or world.getActiveWorldId() ~= c.page then M.cancel(); return end
    px, py = localize(c.x + c.dx, c.y + c.dy, px, py, c.width)
    local nx, ny = px - c.dx, py - c.dy
    if item.debugMoveGround(c.gid, c.instanceId, nx, ny, c.page) then
        c.x, c.y = nx, ny
    else
        row = rowOnPage(c.page, c.gid)
        if not row or row.instanceId ~= c.instanceId then M.cancel() end
    end
end

function M.update(overlay)
    if capture then sample(overlay, engine.getMousePosition()) end
end

function M.release(overlay, button, x, y, route)
    if button == 1 and claims[1] then
        if route == "game" then sample(overlay, x, y) end
        capture = nil
    end
    claims[button] = nil
end

return M
