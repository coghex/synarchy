{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread.Cursor
    ( pollCursorInfo
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef)
import Engine.Core.State (EngineEnv(..))
import World.Types
import World.Generate.Coordinates (globalToChunk)
import World.Fluids (isOceanChunk)
import World.Material (MaterialId(..), getMaterialProps, MaterialProps(..))
import World.Thread.Helpers (sendHudInfo, sendHudWeatherInfo)
import World.Weather.Types (ClimateCoord(..), ClimateState(..), ClimateGrid(..)
                           , RegionClimate(..), SeasonalClimate(..)
                           , OceanGrid(..), OceanCell(..)
                           , climateRegionSize)

-----------------------------------------------------------
-- Cursor Info Polling
-----------------------------------------------------------

pollCursorInfo ∷ EngineEnv → IO ()
pollCursorInfo env = do
    manager ← readIORef (worldManagerRef env)
    forM_ (wmVisible manager) $ \pageId →
        case lookup pageId (wmWorlds manager) of
            Nothing → return ()
            Just worldState → do
                cs   ← readIORef (wsCursorRef worldState)
                snap ← readIORef (wsCursorSnapshotRef worldState)
                mParams ← readIORef (wsGenParamsRef worldState)

                let curZoom  = zoomSelectedPos cs
                    curWorld = worldSelectedTile cs
                    oldZoom  = csZoomSel snap
                    oldWorld = csWorldSel snap

                when (curZoom ≢ oldZoom) $ do
                    case curZoom of
                        Nothing → sendHudInfo env "" ""
                        Just (baseGX, baseGY) →
                            sendChunkInfo env worldState mParams baseGX baseGY

                when (curWorld ≢ oldWorld) $ do
                    case curWorld of
                        Nothing → sendHudInfo env "" ""
                        Just (gx, gy, z) →
                            sendTileInfo env worldState mParams gx gy z

                let newSnap = CursorSnapshot curZoom curWorld
                when (newSnap ≢ snap) $
                    writeIORef (wsCursorSnapshotRef worldState) newSnap

-----------------------------------------------------------
-- sendChunkInfo: zoom-level (chunk) selection
-----------------------------------------------------------

-- | Format and send HUD info for a selected chunk (zoomed-out view).
--   baseGX/baseGY are the chunk's global grid origin (i.e. chunkX * chunkSize).
sendChunkInfo ∷ EngineEnv → WorldState → Maybe WorldGenParams
              → Int → Int → IO ()
sendChunkInfo env worldState mParams baseGX baseGY = do
    let cx = if baseGX >= 0 then baseGX `div` chunkSize
             else -(((-baseGX) + chunkSize - 1) `div` chunkSize)
        cy = if baseGY >= 0 then baseGY `div` chunkSize
             else -(((-baseGY) + chunkSize - 1) `div` chunkSize)
        coord = ChunkCoord cx cy

    -- Try to find this chunk's zoom cache entry for material/elevation
    zoomCache ← readIORef (wsZoomCacheRef worldState)
    let mEntry = V.find (\e → zceChunkX e ≡ cx ∧ zceChunkY e ≡ cy) zoomCache

    let basicLines = T.unlines $ filter (not . T.null)
            [ "Chunk (" <> T.pack (show cx) <> ", " <> T.pack (show cy) <> ")"
            , case mEntry of
                Just entry →
                    let props = getMaterialProps (MaterialId (zceTexIndex entry))
                    in "Material: " <> matName props
                     <> "\nElevation: " <> T.pack (show (zceElev entry))
                     <> (if zceIsOcean entry then "\nOcean" else "")
                     <> (if zceHasLava entry then "\nLava" else "")
                Nothing → ""
            ]

    let advLines = T.unlines $ filter (not . T.null)
            [ "Grid origin: (" <> T.pack (show baseGX)
              <> ", " <> T.pack (show baseGY) <> ")"
            , case mEntry of
                Just entry →
                    "MatID: " <> T.pack (show (zceTexIndex entry))
                Nothing → ""
            , case mParams of
                Just params →
                    let ocean = isOceanChunk (wgpOceanMap params) coord
                    in "Ocean map: " <> T.pack (show ocean)
                Nothing → ""
            ]
        weatherInfo = case mParams of
            Just params → chunkWeatherInfo params cx cy
            Nothing → ""

    sendHudInfo env basicLines advLines
    sendHudWeatherInfo env weatherInfo

-----------------------------------------------------------
-- chunkWeatherInfo: format weather for a chunk's climate region
-----------------------------------------------------------

-- | Given a chunk's (cx, cy) and the world gen params, look up the
--   climate region that contains this chunk and format it as a
--   multi-line Text for the HUD weather tab.
chunkWeatherInfo ∷ WorldGenParams → Int → Int → Text
chunkWeatherInfo params cx cy =
    let worldSize  = wgpWorldSize params
        halfChunks = worldSize `div` 2

        -- Convert (cx, cy) → (u, v) chunk space
        chunkU = cx - cy
        chunkV = cx + cy

        -- Climate region indices (same formula as ZoomMap / initEarlyClimate)
        ru = (chunkU + halfChunks) `div` climateRegionSize
        rv = (chunkV + halfChunks) `div` climateRegionSize
        coord = ClimateCoord ru rv

        climateGrid = cgRegions (csClimate (wgpClimateState params))
        oceanGrid   = ogCells   (csOcean   (wgpClimateState params))

    in case HM.lookup coord climateGrid of
        Nothing → "No climate data"
        Just rc →
            let showF1 f =
                    let whole = floor f ∷ Int
                        frac  = abs (round ((f - fromIntegral whole) * 10.0) ∷ Int)
                    in T.pack (show whole) <> "." <> T.pack (show frac)
                showSeas (SeasonalClimate s w) =
                    showF1 s <> " / " <> showF1 w

                -- Ocean cell info (if this region is ocean)
                oceanLine = case HM.lookup coord oceanGrid of
                    Nothing → ""
                    Just oc →
                        "SST: " <> showSeas (ocTemperature oc)
                        <> "\nSalinity: " <> showF1 (ocSalinity oc)
                        <> "\nIce cover: " <> showF1 (ocIceCover oc)
                        <> "\nUpwelling: " <> showF1 (ocUpwelling oc)

            in T.unlines $ filter (not . T.null)
                [ "Region (" <> T.pack (show ru) <> ", " <> T.pack (show rv) <> ")"
                , "Air temp (S/W): " <> showSeas (rcAirTemp rc) <> " C"
                , "Humidity: " <> showF1 (rcHumidity rc)
                , "Precip (S/W): " <> showSeas (rcPrecipitation rc)
                , "Cloud cover: " <> showF1 (rcCloudCover rc)
                , "Pressure: " <> showF1 (rcPressure rc)
                , "Wind: " <> showF1 (rcWindSpeed rc)
                  <> " @ " <> showF1 (rcWindDir rc) <> " rad"
                , "Continentality: " <> showF1 (rcContinentality rc)
                , "Albedo: " <> showF1 (rcAlbedo rc)
                , "Elev avg: " <> T.pack (show (rcElevAvg rc))
                , oceanLine
                ]

-----------------------------------------------------------
-- sendTileInfo: world-level (tile) selection
-----------------------------------------------------------

-- | Format and send HUD info for a selected tile (zoomed-in view).
--   gx/gy are global grid coords, z is the z-level the cursor hit.
sendTileInfo ∷ EngineEnv → WorldState → Maybe WorldGenParams
             → Int → Int → Int → IO ()
sendTileInfo env worldState _mParams gx gy z = do
    tileData ← readIORef (wsTilesRef worldState)

    let (coord, (lx, ly)) = globalToChunk gx gy
        mChunk = lookupChunk coord tileData
        colIdx = columnIndex lx ly

    let (matText, surfText, fluidText) = case mChunk of
            Nothing → ("(unloaded)", "", "")
            Just lc →
                let col  = (lcTiles lc) V.! colIdx
                    surfZ = (lcSurfaceMap lc) VU.! colIdx
                    -- Material at the SELECTED z-level, not the surface
                    relZ = z - ctStartZ col
                    selectedMat =
                        if relZ >= 0 && relZ < VU.length (ctMats col)
                        then ctMats col VU.! relZ
                        else 0
                    props = getMaterialProps (MaterialId selectedMat)
                    -- Fluid info
                    mFluid = (lcFluidMap lc) V.! colIdx
                    fluidStr = case mFluid of
                        Nothing → ""
                        Just fc → "Fluid: " <> T.pack (show (fcType fc))
                                <> " (surface z=" <> T.pack (show (fcSurface fc)) <> ")"
                in ( matName props
                   , T.pack (show surfZ)
                   , fluidStr
                   )

    let basicLines = T.unlines $ filter (not . T.null)
            [ "Tile (" <> T.pack (show gx) <> ", " <> T.pack (show gy) <> ")"
            , "Material: " <> matText
            , "Surface: " <> surfText
            , "Z: " <> T.pack (show z)
            , fluidText
            ]

    let ChunkCoord ccx ccy = coord
        advLines = T.unlines $ filter (not . T.null)
            [ "Chunk: (" <> T.pack (show ccx) <> ", " <> T.pack (show ccy) <> ")"
            , "Local: (" <> T.pack (show lx) <> ", " <> T.pack (show ly) <> ")"
            , "Column start Z: " <> case mChunk of
                  Nothing → "?"
                  Just lc → let col = (lcTiles lc) V.! colIdx
                             in T.pack (show (ctStartZ col))
            ]

    sendHudInfo env basicLines advLines
