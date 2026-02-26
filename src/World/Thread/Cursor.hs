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
-- sendChunkInfo
-----------------------------------------------------------

sendChunkInfo ∷ EngineEnv → WorldState → Maybe WorldGenParams
              → Int → Int → IO ()
sendChunkInfo env worldState mParams baseGX baseGY = do
    -- ... (move the existing sendChunkInfo body here unchanged)
    -- This includes the zoom cache lookup, basicLines, advLines,
    -- weatherInfo formatting, and the sendHudInfo / sendHudWeatherInfo calls.
    let cx = if baseGX >= 0 then baseGX `div` chunkSize
             else -(((-baseGX) + chunkSize - 1) `div` chunkSize)
        cy = if baseGY >= 0 then baseGY `div` chunkSize
             else -(((-baseGY) + chunkSize - 1) `div` chunkSize)
        coord = ChunkCoord cx cy
    zoomCache ← readIORef (wsZoomCacheRef worldState)
    let mEntry = V.find (\e → zceChunkX e ≡ cx ∧ zceChunkY e ≡ cy) zoomCache
    -- ... (rest of the body as-is from Thread.hs)
    -- (abbreviated for space — copy verbatim from the original)
    sendHudInfo env "" ""  -- placeholder

-----------------------------------------------------------
-- chunkWeatherInfo
-----------------------------------------------------------

chunkWeatherInfo ∷ WorldGenParams → Int → Int → Text
chunkWeatherInfo params cx cy =
    -- ... (move the existing pure function here unchanged)
    ""  -- placeholder

-----------------------------------------------------------
-- sendTileInfo
-----------------------------------------------------------

sendTileInfo ∷ EngineEnv → WorldState → Maybe WorldGenParams
             → Int → Int → Int → IO ()
sendTileInfo env worldState _mParams gx gy z = do
    -- ... (move the existing sendTileInfo body here unchanged)
    sendHudInfo env "" ""  -- placeholder
