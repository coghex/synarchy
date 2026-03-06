-- Vegetation Loader - enumerates data/vegetation/*.yaml and loads each one
local vegetationLoader = {}

function vegetationLoader.loadAll(folder)
    folder = folder or "data/vegetation"
    local files = engine.listFiles(folder, ".yaml")
    if not files then
        engine.logWarn("No vegetation YAML files found in " .. folder)
        return 0
    end

    local totalCount = 0
    for _, filename in ipairs(files) do
        local fullPath = folder .. "/" .. filename
        local count = engine.loadVegetationYaml(fullPath)
        if count and count > 0 then
            engine.logInfo("Loaded " .. count .. " veg textures from " .. filename)
            totalCount = totalCount + count
        else
            engine.logWarn("Failed to load vegetation from " .. filename)
        end
    end

    engine.logInfo("Total vegetation textures loaded: " .. totalCount)
    return totalCount
end

return vegetationLoader
