-- Ground-item right-click menu construction, split out of
-- scripts/init_context_menu.lua (#543's init_* family, #2300).
--
-- scripts/init_context_menu.lua keeps M.tryItemMenu as its entry point
-- and forwards here, so every caller and every fixture that stubs
-- `scripts.init_context_menu` keeps working unchanged.
local M = {}

-- Right-click on a ground item → context menu. With units selected:
-- Info / Pick up / Move here. Without: just Info. Pick up dispatches
-- the NEAREST selected unit; capacity is checked at the moment of
-- pickup (it can change en route).
function M.tryItemMenu(x, y)
    local gid = item.hitTestAt(x, y)
    if not gid then return false end

    local fbW, fbH = engine.getFramebufferSize()
    local ww, wh   = engine.getWindowSize()
    local mx, my   = x, y
    if ww and wh and ww > 0 and wh > 0 then
        mx = x * (fbW / ww)
        my = y * (fbH / wh)
    end
    local contextMenu = require("scripts.ui.context_menu")
    local menuItems = {
        { label = "Info",
          callback = function()
              -- #2300: gid was captured when the menu opened, and the
              -- item can be picked up or removed before this fires.
              -- item.select reports that refusal and changes nothing,
              -- so the unconditional clears below must not run either
              -- (#1580's stale-captured-target contract, as #1929
              -- already applied it to the unit menu's Info row).
              if not item.select(gid) then return end
              unit.deselectAll()
              building.deselect()
          end },
    }
    local selUids = unit.getSelected() or {}
    if #selUids > 0 then
        local ipos = nil
        for _, g in ipairs(item.listGround() or {}) do
            if g.id == gid then ipos = g; break end
        end
        if ipos then
            local unitAi = require("scripts.unit_ai")
            local transferSession = require("scripts.transfer_session")
            table.insert(menuItems, {
                label = "Pick up",
                callback = function()
                    local best, bestUid = math.huge, nil
                    for _, uid in ipairs(selUids) do
                        local info = unit.getInfo(uid)
                        if info then
                            local d = (info.gridX - ipos.x) ^ 2
                                    + (info.gridY - ipos.y) ^ 2
                            if d < best then
                                best, bestUid = d, uid
                            end
                        end
                    end
                    if bestUid then
                        -- #1254, as on Attack above: a player order to
                        -- a held unit ends the Mode A session first.
                        transferSession.notePlayerOrder(bestUid)
                        unitAi.commandPickup(bestUid, gid)
                    end
                end })
            table.insert(menuItems, {
                label = "Move here",
                callback = function()
                    for _, uid in ipairs(selUids) do
                        transferSession.notePlayerOrder(uid)
                        unitAi.commandMove(uid, ipos.x, ipos.y)
                    end
                end })
        end
    end
    contextMenu.show(menuItems, mx, my)
    return true
end

return M
