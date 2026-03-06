-- Flora Loader - enumerates data/flora/*.yaml and loads each one
local floraLoader = {}

function floraLoader.loadAll(folder)
    folder = folder or "data/flora"
    local files = engine.listFiles(folder, ".yaml")
    if not files then
        engine.logWarn("No flora YAML files found in " .. folder)
        return 0
    end

    local totalCount = 0
    for _, filename in ipairs(files) do
        local fullPath = folder .. "/" .. filename
        local count = engine.loadFloraYaml(fullPath)
        if count and count > 0 then
            engine.logInfo("Loaded " .. count .. " flora textures from " .. filename)
            totalCount = totalCount + count
        else
            engine.logWarn("Failed to load flora from " .. filename)
        end
    end

    engine.logInfo("Total flora textures loaded: " .. totalCount)
    return totalCount
end

return floraLoader
