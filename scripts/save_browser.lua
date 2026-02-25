-- Save Browser - lists all saved worlds for loading
local scale = require("scripts.ui.scale")
local panel = require("scripts.ui.panel")
local button = require("scripts.ui.button")
local label = require("scripts.ui.label")

local saveBrowser = {}

saveBrowser.page = nil
saveBrowser.saves = {}
saveBrowser.ownedLabels = {}
saveBrowser.ownedButtons = {}
saveBrowser.ownedPanels = {}
saveBrowser.onBackCallback = nil

function saveBrowser.show(saves, onBack)
    saveBrowser.saves = saves or engine.listSaves() or {}
    saveBrowser.onBackCallback = onBack
    saveBrowser.createUI()
    if saveBrowser.page then
        UI.showPage(saveBrowser.page)
    end
end

function saveBrowser.hide()
    if saveBrowser.page then
        UI.hidePage(saveBrowser.page)
    end
end

function saveBrowser.createUI()
    -- Clean up previous
    saveBrowser.destroyOwned()
    if saveBrowser.page then
        UI.deletePage(saveBrowser.page)
    end
    
    saveBrowser.page = UI.newPage("save_browser", "menu")
    
    -- Create a panel listing each save as a clickable row
    -- Each row shows: name, seed, world size, timestamp
    -- Clicking a row loads that save
    
    for i, save in ipairs(saveBrowser.saves) do
        -- Create a button for each save
        local text = save.name .. "  (seed: " .. tostring(save.seed)
            .. ", " .. save.timestamp .. ")"
        
        -- You'd create button rows here using your existing
        -- button/panel system, similar to main_menu.createUI
        -- Each button's callback loads the save:
        --   engine.loadSave(save.name)
    end
    
    -- Add a "Back" button
end

function saveBrowser.destroyOwned()
    for _, id in ipairs(saveBrowser.ownedLabels)  do label.destroy(id) end
    for _, id in ipairs(saveBrowser.ownedButtons) do button.destroy(id) end
    for _, id in ipairs(saveBrowser.ownedPanels)  do panel.destroy(id) end
    saveBrowser.ownedLabels  = {}
    saveBrowser.ownedButtons = {}
    saveBrowser.ownedPanels  = {}
end

return saveBrowser
