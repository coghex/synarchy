-- UI Manager: HUD/world-event forwarding (#544 split from
-- ui_manager.lua).
--
-- Owns the small broadcast forwards from the world/create-world-gen
-- systems into the create-world-menu log + preview, and the tile/chunk
-- info panel forwards into the HUD.
local uiManager = package.loaded["scripts.ui_manager"]

local createWorldMenu = require("scripts.create_world_menu")
local hud              = require("scripts.hud")

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
