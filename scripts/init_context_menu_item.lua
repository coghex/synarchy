-- Ground-item right-click menu construction, split out of
-- scripts/init_context_menu.lua (#543's init_* family, #2300).
--
-- scripts/init_context_menu.lua keeps M.tryItemMenu as its entry point
-- and forwards here, so every caller and every fixture that stubs
-- `scripts.init_context_menu` keeps working unchanged.
local M = {}

-- Right-click on a ground item → context menu. Info always; "Contents"
-- for an item-container (#2527); "Pick up" whenever the shared
-- assignment rule finds a carrier (#2522); and, with units selected,
-- "Move here". Capacity is checked by the pickup order itself, at
-- command time and again at the moment of pickup (it can change en
-- route).
--
-- The ground row is resolved ONCE up front because two entries need it
-- and one of them — "Contents" — is offered with no unit selected at
-- all: a crate's window is the player looking at what they remember,
-- not a unit interaction.
function M.tryItemMenu(x, y)
    local gid, hitPage = item.hitTestAt(x, y)
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
    local ipos = nil
    for _, g in ipairs(item.listGround() or {}) do
        if g.id == gid then ipos = g; break end
    end

    -- "Contents" (#2527, PLC-17): the portable container window, opened
    -- from the player's own remembered knowledge with no unit involved
    -- and nothing written. Offered in every knowledge state — a crate
    -- nobody has opened yet says exactly that — so the only question is
    -- whether this item is an item-container at all.
    --
    -- That question is answered by the DEFINITION's own `storage:`
    -- declaration, reported as `hasStorage`
    -- (Engine.Scripting.Lua.API.Items.Ground). Deliberately not
    -- `kind == "container"`, which is a free-form authoring label, and
    -- deliberately not a capacity test on `fill` — `container:` is the
    -- homogeneous FLUID component (D-12), so either substitute would
    -- offer a portable level on a canteen.
    --
    -- The gid is resolved to the crate's INSTANCE id here, while the
    -- row is in hand, and the level is opened with that: a gid is
    -- page-local and the active page can change before this entry
    -- fires, whereas an instance id names one crate for the session.
    if ipos and ipos.hasStorage == true
       and type(ipos.instanceId) == "number" and ipos.instanceId > 0 then
        local iid     = ipos.instanceId
        local dname   = ipos.displayName
        local defName = ipos.defName
        table.insert(menuItems, {
            label = "Contents",
            callback = function()
                require("scripts.item_contents_panel")
                    .openForGround(iid, mx, my, dname, defName)
            end })
    end

    local selUids = unit.getSelected() or {}

    -- "Pick up" (#2522): the carrier is whoever the arc-wide assignment
    -- rule picks (transfer_session.resolveSource, D-8 / D-28) -- nearest
    -- to the item in three dimensions, lowest uid on an exact tie,
    -- player-commandable and able to run pickup_ground, filtered BEFORE
    -- ranking -- with the no-selection fallback switched on:
    --
    --   * a nonempty selection is the candidate set, and when none of it
    --     is eligible the entry is omitted (no reaching past the
    --     player's choice for an unselected unit);
    --   * an empty selection assigns the nearest eligible player unit on
    --     the item's own page;
    --   * no eligible unit at all omits the entry.
    --
    -- The target names its page explicitly. The row's own `page` must be
    -- the page the hit was tested on (a gid is page-local, and the two
    -- queries each read whatever page was active when they ran); the
    -- resolver then ranks only units on that page, and the order is
    -- dispatched with it, so a carrier on any other page is refused
    -- rather than handed a same-numbered id from a different world.
    -- The height ranked against is the terrain the item rests on.
    if ipos and ipos.page ~= nil and ipos.page == hitPage then
        local transferSession = require("scripts.transfer_session")
        local target = { gridX = ipos.x, gridY = ipos.y,
                         gridZ = ipos.gridZ, page = ipos.page }
        local carrier = transferSession.resolveSource(selUids, nil, target,
            "pickup_ground", { fallback = true })
        if carrier then
            local unitAi = require("scripts.unit_ai")
            local page = ipos.page
            table.insert(menuItems, {
                label = "Pick up",
                callback = function()
                    -- #1254, as on Attack: a player order to a held
                    -- unit ends the Mode A session first.
                    transferSession.notePlayerOrder(carrier)
                    unitAi.commandPickup(carrier, gid, page)
                end })
        end
    end

    if #selUids > 0 and ipos then
        local unitAi = require("scripts.unit_ai")
        local transferSession = require("scripts.transfer_session")
        table.insert(menuItems, {
            label = "Move here",
            callback = function()
                for _, uid in ipairs(selUids) do
                    transferSession.notePlayerOrder(uid)
                    unitAi.commandMove(uid, ipos.x, ipos.y)
                end
            end })
    end
    contextMenu.show(menuItems, mx, my)
    return true
end

return M
