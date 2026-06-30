-- Shared "broken equipment" icon overlay.
--
-- Item-list panels (the unit-info inventory section, the cargo
-- contents popup, the item-container contents popup) draw a
-- broken_equipment.png badge over an item icon when the item is worn
-- out (condition <= 0). The overlay texture is registered by name
-- during item load (see Engine.Scripting.Lua.API.Items); we resolve
-- its handle once and cache it module-wide so every panel shares one
-- lookup instead of caching it three times.

local brokenOverlay = {}

local _tex = nil

-- Resolve the broken_equipment overlay texture handle, cached once.
-- Returns the handle, or nil if it isn't resident yet.
function brokenOverlay.tex()
    if _tex then return _tex end
    local h = engine.getTextureHandle("broken_equipment")
    if h and h >= 0 then _tex = h end
    return _tex
end

-- Overlay broken_equipment.png over an item icon at (x, y, w, h) when
-- `condition` marks the item broken (<= 0). `z` should sit just above
-- the icon. Creates the sprite on `page` named `name` and returns its
-- id so the caller can track it for teardown; returns nil (draws
-- nothing) when the item isn't broken or the overlay texture is absent.
function brokenOverlay.add(page, name, condition, x, y, w, h, z)
    if not (condition and condition <= 0) then return nil end
    local bt = brokenOverlay.tex()
    if not bt then return nil end
    local oid = UI.newSprite(name, w, h, bt, 1.0, 1.0, 1.0, 1.0, page)
    UI.addToPage(page, oid, x, y)
    UI.setZIndex(oid, z)
    return oid
end

return brokenOverlay
