-- Substance Loader — enumerates data/substances/*.yaml and registers
-- each. Mirrors equipment_loader / item_loader.
local substanceLoader = {}

function substanceLoader.loadAll(folder)
    folder = folder or "data/substances"
    local files = engine.listFiles(folder, ".yaml")
    if not files then
        engine.logWarn("No substance YAML files found in " .. folder)
        return 0
    end

    local total = 0
    for _, filename in ipairs(files) do
        local fullPath = folder .. "/" .. filename
        local count = engine.loadSubstanceYaml(fullPath)
        if count and count > 0 then
            engine.logInfo("Loaded " .. count .. " substances from " .. filename)
            total = total + count
        else
            engine.logWarn("Failed to load substances from " .. filename)
        end
    end

    engine.logInfo("Total substances loaded: " .. total)
    return total
end

return substanceLoader
