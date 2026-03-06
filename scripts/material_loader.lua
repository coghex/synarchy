-- Material Loader - enumerates data/materials/*.yaml and loads each one
local materialLoader = {}

function materialLoader.loadAll(folder)
    folder = folder or "data/materials"
    local files = engine.listFiles(folder, ".yaml")
    if not files then
        engine.logWarn("No material YAML files found in " .. folder)
        return 0
    end

    local totalCount = 0
    for _, filename in ipairs(files) do
        local fullPath = folder .. "/" .. filename
        local count = engine.loadMaterialYaml(fullPath)
        if count and count > 0 then
            engine.logInfo("Loaded " .. count .. " materials from " .. filename)
            totalCount = totalCount + count
        else
            engine.logWarn("Failed to load materials from " .. filename)
        end
    end

    engine.logInfo("Total materials loaded: " .. totalCount)
    return totalCount
end

return materialLoader
