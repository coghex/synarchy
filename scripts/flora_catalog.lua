-- Flora Catalog - delegates to flora_loader which calls engine.loadFloraYaml()
-- The Haskell side now handles all species registration, texture loading,
-- lifecycle setup, phases, annual cycles, overrides, and worldgen registration.
local floraLoader = require("scripts.flora_loader")

local floraCatalog = {}

function floraCatalog.init()
    local count = floraLoader.loadAll("data/flora")
    engine.logInfo("Flora catalog initialized: " .. count .. " textures loaded")
end

return floraCatalog
