{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | River-network query: world.getRivers.
module Engine.Scripting.Lua.API.WorldQuery.River
    ( worldGetRiversFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Vector as V
import Engine.Core.State (EngineEnv(..))
import World.Types
import Engine.Scripting.Lua.API.WorldQuery.Lookup (getWorldGenParams)

-- | world.getRivers() → array of river tables
--   Each river: { source={x,y}, mouth={x,y}, flowRate=N, segments={...} }
--   Each segment: { sx,sy, ex,ey, width, valleyWidth, depth, flowRate,
--                   startElev, endElev }
--
--   Water surface elevation is no longer carried on the segment — it
--   is derived per-tile from the water-table compute at chunk gen.
--   Scripts that need surface heights should call world.getSurfaceAt.
worldGetRiversFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetRiversFn env = do
    mParams ← Lua.liftIO $ getWorldGenParams env
    case mParams of
        Nothing → do
            Lua.pushnil
            return 1
        Just params → do
            let timeline = wgpGeoTimeline params
                -- Extract rivers from all periods' events
                rivers = concatMap extractRivers (gtPeriods timeline)
            Lua.newtable
            mapM_ (\(rIdx, river) → do
                Lua.newtable
                -- Source
                let GeoCoord srcX srcY = rpSourceRegion river
                Lua.newtable
                Lua.pushinteger (fromIntegral srcX)
                Lua.setfield (Lua.nth 2) "x"
                Lua.pushinteger (fromIntegral srcY)
                Lua.setfield (Lua.nth 2) "y"
                Lua.setfield (Lua.nth 2) "source"
                -- Mouth
                let GeoCoord mthX mthY = rpMouthRegion river
                Lua.newtable
                Lua.pushinteger (fromIntegral mthX)
                Lua.setfield (Lua.nth 2) "x"
                Lua.pushinteger (fromIntegral mthY)
                Lua.setfield (Lua.nth 2) "y"
                Lua.setfield (Lua.nth 2) "mouth"
                -- Flow rate
                Lua.pushnumber (Lua.Number (realToFrac (rpFlowRate river)))
                Lua.setfield (Lua.nth 2) "flowRate"
                -- Segment count
                Lua.pushinteger (fromIntegral (V.length (rpSegments river)))
                Lua.setfield (Lua.nth 2) "segmentCount"
                -- Segments
                Lua.newtable
                V.iforM_ (rpSegments river) $ \sIdx seg → do
                    let GeoCoord sx sy = rsStart seg
                        GeoCoord ex ey = rsEnd seg
                    Lua.newtable
                    Lua.pushinteger (fromIntegral sx)
                    Lua.setfield (Lua.nth 2) "sx"
                    Lua.pushinteger (fromIntegral sy)
                    Lua.setfield (Lua.nth 2) "sy"
                    Lua.pushinteger (fromIntegral ex)
                    Lua.setfield (Lua.nth 2) "ex"
                    Lua.pushinteger (fromIntegral ey)
                    Lua.setfield (Lua.nth 2) "ey"
                    Lua.pushinteger (fromIntegral (rsWidth seg))
                    Lua.setfield (Lua.nth 2) "width"
                    Lua.pushinteger (fromIntegral (rsValleyWidth seg))
                    Lua.setfield (Lua.nth 2) "valleyWidth"
                    Lua.pushinteger (fromIntegral (rsDepth seg))
                    Lua.setfield (Lua.nth 2) "depth"
                    Lua.pushnumber (Lua.Number (realToFrac (rsFlowRate seg)))
                    Lua.setfield (Lua.nth 2) "flowRate"
                    Lua.pushinteger (fromIntegral (rsStartElev seg))
                    Lua.setfield (Lua.nth 2) "startElev"
                    Lua.pushinteger (fromIntegral (rsEndElev seg))
                    Lua.setfield (Lua.nth 2) "endElev"
                    Lua.rawseti (Lua.nth 2) (fromIntegral sIdx + 1)
                Lua.setfield (Lua.nth 2) "segments"
                Lua.rawseti (Lua.nth 2) rIdx
                ) (zip [1..] rivers)
            return 1

-- | Extract RiverParams from all HydroEvents in a period
extractRivers ∷ GeoPeriod → [RiverParams]
extractRivers period = concatMap go (gpEvents period)
  where
    go (HydroEvent (RiverFeature rp)) = [rp]
    go _ = []
