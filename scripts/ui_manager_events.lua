-- UI Manager: HUD/world-event forwarding (#544 split from
-- ui_manager.lua).
--
-- Owns the small broadcast forwards from the world/create-world-gen
-- systems into the create-world-menu log + preview, and the tile/chunk
-- info panel forwards into the HUD.
local uiManager = package.loaded["scripts.ui_manager"]

local createWorldMenu = require("scripts.create_world_menu")
local hud              = require("scripts.hud")
local worldView        = require("scripts.world_view")
local testArena        = require("scripts.test_arena")

-----------------------------------------------------------
-- World Generation Log (forwarded from world thread)
-----------------------------------------------------------

function uiManager.onWorldGenLog(text)
    if uiManager.moduleReady.createWorldMenu then
        createWorldMenu.onWorldGenLog(text)
    end
end

function uiManager.onWorldPreviewReady(textureHandle)
    engine.logDebug("World preview texture ready: " .. tostring(textureHandle))
    if uiManager.moduleReady.createWorldMenu
       and createWorldMenu.onWorldPreviewReady then
        createWorldMenu.onWorldPreviewReady(textureHandle)
    end
end

-----------------------------------------------------------
-- Tile/Chunk Info Panel (forwarded to HUD)
-----------------------------------------------------------

function uiManager.onSetInfoBasic(text)
    if uiManager.moduleReady.hud then hud.setInfoBasic(text) end
end

function uiManager.onSetInfoAdvanced(text)
    if uiManager.moduleReady.hud then hud.setInfoAdvanced(text) end
end

function uiManager.onSetInfoText(basicText, advancedText)
    if uiManager.moduleReady.hud then hud.setInfoText(basicText, advancedText) end
end

function uiManager.onSetWeatherInfo(text)
    if uiManager.moduleReady.hud then hud.setWeatherInfo(text) end
end

function uiManager.onSetResourcesInfo(text)
    if uiManager.moduleReady.hud then hud.setResourcesInfo(text) end
end

function uiManager.onClearInfo()
    if uiManager.moduleReady.hud then hud.clearInfo() end
end

-----------------------------------------------------------
-- Terminal asset failures (#1690)
--
-- onAssetLoaded's twin: a texture request that FAILED, because the
-- bindless slot allocator was full and nothing can ever sample that
-- handle. worldView and testArena are reached through ui_manager's
-- manual forward rather than the engine's own broadcast, so this
-- callback has to be handed on the same way its success twin
-- (ui_manager_boot.lua) is -- otherwise a failed request never settles
-- the readiness gate they wait on and boot stalls.
-----------------------------------------------------------

-- `reported` (#1842) rides through unchanged: the engine has already
-- logged a better-contextualised line for this asset, so a handler that
-- would log its own must skip it. Forwarding it is not optional -- a
-- handler that never receives it logs the duplicate.
function uiManager.onAssetFailed(assetType, handle, path, reason, reported)
    if worldView.onAssetFailed then
        worldView.onAssetFailed(assetType, handle, path, reason, reported)
    end
    if testArena.onAssetFailed then
        testArena.onAssetFailed(assetType, handle, path, reason, reported)
    end
end
