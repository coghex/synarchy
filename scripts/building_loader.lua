-- Building Loader - enumerates data/buildings/*.yaml and loads each one.
-- Mirrors scripts/unit_loader.lua exactly.
local buildingLoader = {}

function buildingLoader.loadAll(folder)
    folder = folder or "data/buildings"
    local files = engine.listFiles(folder, ".yaml")
    if not files then
        engine.logWarn("No building YAML files found in " .. folder)
        return 0
    end

    local totalCount = 0
    for _, filename in ipairs(files) do
        local fullPath = folder .. "/" .. filename
        local count = engine.loadBuildingYaml(fullPath)
        if count and count > 0 then
            engine.logInfo("Loaded " .. count .. " building definitions from " .. filename)
            totalCount = totalCount + count
        else
            engine.logWarn("Failed to load buildings from " .. filename)
        end
    end

    engine.logInfo("Total building definitions loaded: " .. totalCount)
    return totalCount
end

return buildingLoader
