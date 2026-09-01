-- World Manager - coordinates world state and rendering
local worldManager = {}

worldManager.currentWorld = nil
worldManager.active = false

-----------------------------------------------------------
-- Internal: send all textures to a world
-----------------------------------------------------------

local function sendStructuralTextures(worldId, st)
    if not st then return end
    if st.ocean        then world.setTexture(worldId, "ocean",     st.ocean)        end
    if st.glacier      then world.setTexture(worldId, "glacier",   st.glacier)      end
    if st.lava         then world.setTexture(worldId, "lava",      st.lava)         end
    if st.blankTexture then world.setTexture(worldId, "blank",     st.blankTexture) end
    if st.noTexture    then world.setTexture(worldId, "notexture", st.noTexture)    end
    if st.isoFaceMap   then world.setTexture(worldId, "iso_facemap", st.isoFaceMap) end
    if st.noFaceMap    then world.setTexture(worldId, "nofacemap",   st.noFaceMap)  end
    -- Side face maps
    if st.sideFaceMapLeft  then world.setTexture(worldId, "side_facemap_left",  st.sideFaceMapLeft)  end
    if st.sideFaceMapRight then world.setTexture(worldId, "side_facemap_right", st.sideFaceMapRight) end
    -- Terrain slope facemaps
    local slopeNames = {
        "n", "e", "ne", "s", "ns", "es", "nes",
        "w", "nw", "ew", "new", "sw", "nsw", "esw", "nesw"
    }
    local slopeFields = {
        "isoSlopeFaceMapN", "isoSlopeFaceMapE", "isoSlopeFaceMapNE",
        "isoSlopeFaceMapS", "isoSlopeFaceMapNS", "isoSlopeFaceMapES",
        "isoSlopeFaceMapNES", "isoSlopeFaceMapW", "isoSlopeFaceMapNW",
        "isoSlopeFaceMapEW", "isoSlopeFaceMapNEW", "isoSlopeFaceMapSW",
        "isoSlopeFaceMapNSW", "isoSlopeFaceMapESW", "isoSlopeFaceMapNESW"
    }
    for i, field in ipairs(slopeFields) do
        if st[field] then
            world.setTexture(worldId, "iso_slope_facemap_" .. slopeNames[i], st[field])
        end
    end
    -- Vegetation facemaps
    if st.vegFaceMap then world.setTexture(worldId, "veg_facemap", st.vegFaceMap) end
    local vegSlopeFields = {
        "vegSlopeFaceMapN", "vegSlopeFaceMapE", "vegSlopeFaceMapNE",
        "vegSlopeFaceMapS", "vegSlopeFaceMapNS", "vegSlopeFaceMapES",
        "vegSlopeFaceMapNES", "vegSlopeFaceMapW", "vegSlopeFaceMapNW",
        "vegSlopeFaceMapEW", "vegSlopeFaceMapNEW", "vegSlopeFaceMapSW",
        "vegSlopeFaceMapNSW", "vegSlopeFaceMapESW", "vegSlopeFaceMapNESW"
    }
    for i, field in ipairs(vegSlopeFields) do
        if st[field] then
            world.setTexture(worldId, "veg_slope_facemap_" .. slopeNames[i], st[field])
        end
    end
end

local function sendMaterialTextures(worldId)
    -- Material IDs known to the engine (must match data/materials/*.yaml)
    local matIds = {
        1, 2, 3, 6, 7, 8,           -- igneous intrusive
        10, 11, 12, 13, 14, 15, 16, -- igneous extrusive
        20, 21, 22, 23, 24, 25,     -- sedimentary clastic
        30, 31, 32, 33, 34, 35,     -- sedimentary chemical
        40, 41, 42, 43, 44, 45,     -- metamorphic
        50, 51, 52, 53, 54, 55, 56, 57, 58, 59, -- soils mineral
        60, 61, 62, 63, 64, 65, 66, 67,         -- soils silt/special
        70, 71, 72,                 -- carbonaceous
        80, 81, 82, 83, 84, 85, 86, -- ores
        90, 91,                     -- impact
        100, 101, 102, 103,         -- volcanic
        110, 111, 112, 113,         -- glacial
        250, 251, 252, 255,         -- special
    }

    for _, matId in ipairs(matIds) do
        local tileH = engine.getTextureHandle("mat_tile_" .. matId)
        local zoomH = engine.getTextureHandle("mat_zoom_" .. matId)
        local bgH   = engine.getTextureHandle("mat_bg_"   .. matId)
        if tileH and tileH >= 0 then
            world.setTexture(worldId, "mat_tile_" .. matId, tileH)
        end
        if zoomH and zoomH >= 0 then
            world.setTexture(worldId, "mat_zoom_" .. matId, zoomH)
        end
        if bgH and bgH >= 0 then
            world.setTexture(worldId, "mat_bg_"   .. matId, bgH)
        end
    end
end

local function sendVegTextures(worldId)
    -- Vegetation IDs: 18 four-variant types + tilled_soil's single
    -- variant (#333) = IDs 1..77
    for vegId = 1, 77 do
        local h = engine.getTextureHandle("veg_tile_" .. vegId)
        if h and h >= 0 then
            world.setTexture(worldId, "veg_tile_" .. vegId, h)
        end
    end
end

-----------------------------------------------------------
-- World Management
-----------------------------------------------------------

function worldManager.createWorld(params)
    local worldId    = params.worldId or "main_world"
    local seed       = params.seed or 42
    local worldSize  = params.worldSize or 128
    local plateCount = params.plateCount or 10
    -- Deliberately NO `or` default (#1105): the player's world name is
    -- forwarded verbatim, and its absence must stay absent. Normalization
    -- belongs to the engine (mkWorldIdentity strips, and treats a blank or
    -- whitespace-only name as no identity at all), so nothing here trims,
    -- defaults, or synthesizes a name.
    local worldName  = params.worldName
    -- #1106: the gloss and language provenance of a name that came from
    -- the generated-language system. Same rule as worldName — forwarded
    -- verbatim, never defaulted, never synthesized. All three are nil
    -- for a player-typed name, which is exactly how the engine tells a
    -- generated identity from a custom one.
    local worldGloss      = params.worldGloss
    local languageSeed    = params.languageSeed
    local languageVersion = params.languageVersion
    -- #1104: the semantic expression the name was rendered from. Same
    -- rule again — verbatim, never defaulted, nil for a player-typed
    -- name, which is how the engine tells a decomposable name from one
    -- with no meaning behind it.
    local nameExpr        = params.nameExpr

    engine.logInfo("Creating world: " .. worldId
        .. " (seed=" .. seed .. ", size=" .. worldSize .. " chunks)")

    -- Generating a world is the start of a NEW SESSION, and this is the
    -- one funnel every generated world goes through (world_view.
    -- createWorld, itself guarded so it only ever creates the session's
    -- first world). Tutorial progress is session state living on a Lua
    -- singleton that outlives any single world, so without this it
    -- carries over from whatever was played or LOADED earlier in the
    -- same process — start a new game after finishing an objective and
    -- it begins already complete (#958).
    --
    -- A load never reaches here: it replaces the whole session
    -- Haskell-side and restores progress through the save component's
    -- own apply(), so resetting on this path cannot race it. Required
    -- lazily to keep this module free of module-scope dependencies.
    require("scripts.tutorial_progress").reset()

    -- The checklist's own PRESENTATION state (open/closed + scroll
    -- offset) lives on a second Lua singleton with the same lifetime
    -- problem (#960): without this, starting a new game after playing
    -- one comes up with the previous session's panel already open and
    -- scrolled. Reached through package.loaded rather than require()
    -- so generating a world never forces the HUD module to load in a
    -- profile that has none.
    local tutorialHud = package.loaded["scripts.tutorial_hud"]
    if tutorialHud then tutorialHud.resetPresentation() end

    -- Send init command with seed and world size
    -- This queues the WorldInit command; the world state will exist
    -- once the world thread processes it.
    -- A nil worldName leaves argument 5 nil, which world.init reads
    -- identically to the four-argument form — arena, debug, and console
    -- callers are unaffected (#708 acceptance criterion 7). Arguments
    -- 6-8 (gloss, language seed, generator version) follow the same
    -- rule: nil for every caller that has no generated name, which is
    -- indistinguishable from not passing them at all. Argument 9
    -- (#1104's name expression) follows it exactly.
    -- #2020: world.init admits the world's map image synchronously and
    -- enqueues nothing when it refuses. Reaching a refusal HERE means
    -- something already destroyed the previous world, so the caller
    -- responsible for asking first (scripts/create_world/generation.lua,
    -- via world.checkMapImagePlan, before its destroyWorld()) did not.
    -- Nothing downstream can recover the lost world, so this reports the
    -- diagnostic as loudly as it can and returns nil rather than handing
    -- back a world id no page will ever exist under.
    local queued, refusal = world.init(worldId, seed, worldSize, plateCount,
                                       worldName, worldGloss, languageSeed,
                                       languageVersion, nameExpr)
    if queued == false then
        engine.logError("World creation refused: " .. tostring(refusal))
        return nil
    end

    -- Send all textures
    sendStructuralTextures(worldId, params.structural)
    sendMaterialTextures(worldId)
    sendVegTextures(worldId)

    worldManager.currentWorld = worldId

    engine.logInfo("World created: " .. worldId)
    return worldId
end

function worldManager.showWorld(worldId)
    worldId = worldId or worldManager.currentWorld

    if not worldId then
        engine.logWarn("No world to show")
        return
    end

    engine.logInfo("Showing world: " .. worldId)
    world.show(worldId)
    worldManager.active = true
end

function worldManager.hideWorld(worldId)
    worldId = worldId or worldManager.currentWorld

    if not worldId then
        return
    end

    engine.logInfo("Hiding world: " .. worldId)
    world.hide(worldId)
end

function worldManager.destroyWorld(worldId)
    worldId = worldId or worldManager.currentWorld

    if not worldId then
        return
    end

    engine.logInfo("Destroying world: " .. worldId)

    worldManager.hideWorld(worldId)

    -- Tell Haskell to remove this world from wmWorlds
    world.destroy(worldId)

    if worldManager.currentWorld == worldId then
        worldManager.currentWorld = nil
    end

    -- CRITICAL: reset active flag so a new world can be created
    worldManager.active = false
end

-----------------------------------------------------------
-- Update
-----------------------------------------------------------

function worldManager.update(dt)
    if not worldManager.active then return end
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function worldManager.isActive()
    return worldManager.active
end

function worldManager.getCurrentWorld()
    return worldManager.currentWorld
end

return worldManager
