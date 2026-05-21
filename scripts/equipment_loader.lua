-- Equipment Loader — enumerates data/equipment/*.yaml and loads each.
-- Mirrors scripts/item_loader / scripts/unit_loader. One class per file
-- is the expected shape, but a single YAML can contain many classes.
local equipmentLoader = {}

function equipmentLoader.loadAll(folder)
    folder = folder or "data/equipment"
    local files = engine.listFiles(folder, ".yaml")
    if not files then
        engine.logWarn("No equipment YAML files found in " .. folder)
        return 0
    end

    local totalCount = 0
    for _, filename in ipairs(files) do
        local fullPath = folder .. "/" .. filename
        local count = engine.loadEquipmentYaml(fullPath)
        if count and count > 0 then
            engine.logInfo("Loaded " .. count .. " equipment classes from " .. filename)
            totalCount = totalCount + count
        else
            engine.logWarn("Failed to load equipment from " .. filename)
        end
    end

    engine.logInfo("Total equipment classes loaded: " .. totalCount)
    return totalCount
end

return equipmentLoader
