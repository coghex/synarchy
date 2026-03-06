-- Unit Loader - enumerates data/units/*.yaml and loads each one
local unitLoader = {}

function unitLoader.loadAll(folder)
    folder = folder or "data/units"
    local files = engine.listFiles(folder, ".yaml")
    if not files then
        engine.logWarn("No unit YAML files found in " .. folder)
        return 0
    end

    local totalCount = 0
    for _, filename in ipairs(files) do
        local fullPath = folder .. "/" .. filename
        local count = engine.loadUnitYaml(fullPath)
        if count and count > 0 then
            engine.logInfo("Loaded " .. count .. " unit definitions from " .. filename)
            totalCount = totalCount + count
        else
            engine.logWarn("Failed to load units from " .. filename)
        end
    end

    engine.logInfo("Total unit definitions loaded: " .. totalCount)
    return totalCount
end

return unitLoader
