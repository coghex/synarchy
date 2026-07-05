{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Lua API for the plant-designation tool (issue #335) — the
--   @plant.*@ namespace, plus @world.getPlantSuitability@. Mirrors the
--   till-designation API (#333) minus the anchor functions: the plant
--   tool is single-tile (the planting screen already scopes the player
--   to one tile before a crop is chosen), so there is no pending
--   rectangle to track. The tool drives designate, the farm AI (#336)
--   will drive nearestDesignation / getDesignationAt / cancelDesignation
--   (claims are Lua-side like dig/chop/till jobs, so there is no
--   engine-side job status), and the HUD sets the marker texture.
module Engine.Scripting.Lua.API.Plant
    ( plantDesignateFn
    , plantCancelDesignationFn
    , plantGetDesignationAtFn
    , plantGetDesignationCountFn
    , plantNearestDesignationFn
    , plantSetDesignateTextureFn
    , worldGetPlantSuitabilityFn
    ) where

import UPrelude
import Data.List (sortOn)
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified HsLua as Lua
import Data.IORef (readIORef)
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..), activeWorldPage, activeWorldState)
import Engine.Asset.Handle (TextureHandle(..))
import World.Types hiding (activeWorldPage)
import World.Plant.Types
import World.Flora.Placement (speciesFitness)
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))
import World.Generate.Coordinates (globalToChunk)

-- | plant.designate(pageId, gx, gy, cropName) — single-tile plant
--   designation, no anchor. Refused world-thread-side unless the tile
--   is tilled soil and cropName names a registered plantable-crop
--   species (row_crop or groundcover_crop worldGen category); query
--   getDesignationAt afterward to confirm it landed (a queued world
--   command, same as world.setVegAt / till.designate).
plantDesignateFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
plantDesignateFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    cropArg ← Lua.tostring 4
    case (pageIdArg, gxArg, gyArg, cropArg) of
        (Just pageIdBS, Just gx, Just gy, Just cropBS) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) $
                WorldDesignatePlant pageId (round gx) (round gy)
                    (TE.decodeUtf8 cropBS)
        _ → pure ()
    return 0

-- | plant.cancelDesignation(gx, gy) — remove the designation at a tile
--   on the active world. Both the player-cancel path and the farm AI's
--   completion call this (best-effort, returns nothing).
plantCancelDesignationFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
plantCancelDesignationFn env = do
    gxArg ← Lua.tonumber 1
    gyArg ← Lua.tonumber 2
    case (gxArg, gyArg) of
        (Just gx, Just gy) → do
            mPage ← Lua.liftIO $ activeWorldPage env
            case mPage of
                Just (pageId, _) → Lua.liftIO $
                    Q.writeQueue (worldQueue env) $
                        WorldCancelPlant pageId (round gx) (round gy)
                Nothing → pure ()
        _ → pure ()
    return 0

-- | plant.getDesignationAt(pageId, gx, gy) → {x, y, z, crop} | nil.
plantGetDesignationAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
plantGetDesignationAtFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gxN, Just gyN) → do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                gx = round gxN ∷ Int
                gy = round gyN ∷ Int
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsPlantDesignationsRef ws)
                    case HM.lookup (gx, gy) m of
                        Just pd → do
                            cat ← Lua.liftIO $ readIORef (floraCatalogRef env)
                            let cropName =
                                    maybe "" fsName (lookupSpecies (ptCrop pd) cat)
                            Lua.newtable
                            Lua.pushinteger (fromIntegral gx)
                            Lua.setfield (Lua.nth 2) "x"
                            Lua.pushinteger (fromIntegral gy)
                            Lua.setfield (Lua.nth 2) "y"
                            Lua.pushinteger (fromIntegral (ptZ pd))
                            Lua.setfield (Lua.nth 2) "z"
                            Lua.pushstring (TE.encodeUtf8 cropName)
                            Lua.setfield (Lua.nth 2) "crop"
                            return 1
                        Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | plant.getDesignationCount(pageId) → n.
plantGetDesignationCountFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
plantGetDesignationCountFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsPlantDesignationsRef ws)
                    Lua.pushinteger (fromIntegral (HM.size m))
                    return 1
                Nothing → Lua.pushinteger 0 >> return 1
        _ → Lua.pushinteger 0 >> return 1

-- | plant.nearestDesignation(pageId, x, y) → gx, gy, dist | nil.
--   Nearest designated tile by Euclidean distance — the farm AI's
--   "distance to nearest plant job" term. Mirrors till.nearestDesignation.
plantNearestDesignationFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
plantNearestDesignationFn env = do
    pageIdArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3
    case (pageIdArg, xArg, yArg) of
        (Just pageIdBS, Just x, Just y) → do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                ux = realToFrac x ∷ Float
                uy = realToFrac y ∷ Float
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsPlantDesignationsRef ws)
                    let dist2 (gx, gy) =
                            let dx = fromIntegral gx - ux
                                dy = fromIntegral gy - uy
                            in dx * dx + dy * dy
                        best = foldl' (\acc k → case acc of
                                  Nothing → Just (k, dist2 k)
                                  Just (_, d) | dist2 k < d → Just (k, dist2 k)
                                  _ → acc)
                                Nothing (HM.keys m)
                    case best of
                        Just ((gx, gy), d2) → do
                            Lua.pushinteger (fromIntegral gx)
                            Lua.pushinteger (fromIntegral gy)
                            Lua.pushnumber (Lua.Number (realToFrac (sqrt d2)))
                            return 3
                        Nothing → Lua.pushnil >> return 1
                Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | plant.setDesignateTexture(pageId, texHandle) — marker texture for
--   committed plant designations.
plantSetDesignateTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
plantSetDesignateTextureFn env = do
    pageIdArg ← Lua.tostring 1
    handleArg ← Lua.tointeger 2
    case (pageIdArg, handleArg) of
        (Just pageIdBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
                WorldSetPlantDesignateTexture pageId texHandle
        _ → pure ()
    return 0

-- | world.getPlantSuitability(gx, gy) → array of {id, name, category,
--   score} sorted best-first, | nil (chunk unloaded / no active world).
--   Scores every registered plantable-crop species (row_crop /
--   groundcover_crop worldGen category) against the tile's climate +
--   slope + altitude via World.Flora.Placement.speciesFitness — the
--   SAME fitness function that gates natural worldgen placement (#332's
--   fiHealth). This is the suitability read-out #335's planting screen
--   sorts/filters/searches over; no separate crop-listing primitive is
--   needed since this query already enumerates the full catalogue.
--
--   Soil (ctMats) is read and passed through, but every shipped
--   species' fwSoils is currently [] (see data/flora/crops.yaml), so it
--   has no effect on the score yet — a future soils: YAML key would
--   activate it with no change needed here.
worldGetPlantSuitabilityFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetPlantSuitabilityFn env = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            let gx = fromIntegral gx' ∷ Int
                gy = fromIntegral gy'
                (coord, (lx, ly)) = globalToChunk gx gy
                idx = ly * chunkSize + lx
            mResult ← Lua.liftIO $ do
                mWs ← activeWorldState env
                case mWs of
                    Nothing → pure Nothing
                    Just ws → do
                        tileData ← readIORef (wsTilesRef ws)
                        mParams ← readIORef (wsGenParamsRef ws)
                        case (lookupChunk coord tileData, mParams) of
                            (Just lc, Just params) → do
                                cat ← readIORef (floraCatalogRef env)
                                let col = lcTiles lc V.! idx
                                    z   = lcSurfaceMap lc VU.! idx
                                    i   = z - ctStartZ col
                                    matId = if i ≥ 0 ∧ i < VU.length (ctMats col)
                                            then ctMats col VU.! i else 0
                                    slopeId = if i ≥ 0 ∧ i < VU.length (ctSlopes col)
                                              then ctSlopes col VU.! i else 0
                                    climate = lookupLocalClimate
                                                  (wgpClimateState params)
                                                  (wgpWorldSize params) gx gy
                                    scored =
                                        [ (fid, sp, fwCategory wg, score)
                                        | (fid, wg) ← worldGenSpecies cat
                                        , isPlantableCropCategory (fwCategory wg)
                                        , Just sp ← [lookupSpecies fid cat]
                                        , let score = speciesFitness wg matId
                                                  slopeId (lcTemp climate)
                                                  (lcPrecip climate)
                                                  (lcHumidity climate) z
                                        ]
                                pure (Just (sortOn (\(_,_,_,s) → negate s) scored))
                            _ → pure Nothing
            case mResult of
                Nothing → Lua.pushnil >> return 1
                Just rows → do
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] rows) $
                        \(ix, (fid, sp, cat, score)) → do
                            Lua.newtable
                            Lua.pushinteger (fromIntegral (unFloraId fid))
                            Lua.setfield (-2) "id"
                            Lua.pushstring (TE.encodeUtf8 (fsName sp))
                            Lua.setfield (-2) "name"
                            Lua.pushstring (TE.encodeUtf8 cat)
                            Lua.setfield (-2) "category"
                            Lua.pushnumber (Lua.Number (realToFrac score))
                            Lua.setfield (-2) "score"
                            Lua.rawseti (-2) (fromIntegral ix)
                    return 1
        _ → Lua.pushnil >> return 1
