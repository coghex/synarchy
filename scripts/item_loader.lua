-- Item Loader — enumerates data/items/*.yaml and loads each one.
-- Mirrors scripts/vegetation_loader and scripts/building_loader.
local itemLoader = {}

function itemLoader.loadAll(folder)
    folder = folder or "data/items"
    local files = engine.listFiles(folder, ".yaml")
    if not files then
        engine.logWarn("No item YAML files found in " .. folder)
        return 0
    end

    local totalCount = 0
    for _, filename in ipairs(files) do
        local fullPath = folder .. "/" .. filename
        local count = engine.loadItemYaml(fullPath)
        if count and count > 0 then
            engine.logInfo("Loaded " .. count .. " item defs from " .. filename)
            totalCount = totalCount + count
        else
            engine.logWarn("Failed to load items from " .. filename)
        end
    end

    engine.logInfo("Total item definitions loaded: " .. totalCount)
    return totalCount
end

return itemLoader
