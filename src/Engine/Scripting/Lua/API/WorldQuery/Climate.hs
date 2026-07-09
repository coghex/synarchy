{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Climate/ambient-temperature/sun-angle queries: world.getClimateAt,
--   world.getAmbientAt, world.getSunAngleAt.
module Engine.Scripting.Lua.API.WorldQuery.Climate
    ( worldGetClimateAtFn
    , worldGetAmbientAtFn
    , worldGetSunAngleAtFn
    ) where

import UPrelude
import qualified HsLua as Lua
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..))
import World.Types
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))
import World.Weather.Ambient (ambientTempAt)
import World.Time.Local (localSunAngle)
import Engine.Scripting.Lua.API.WorldQuery.Lookup (getWorldGenParams)

-- | world.getClimateAt(gx, gy) → { temp, summerTemp, winterTemp, precip,
--   humidity, snow } | nil. Region climate sampled (bilinear) at a global
--   tile. temp in °C; precip/humidity/snow 0..1. nil if no world is active.
worldGetClimateAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetClimateAtFn env = do
    gxArg ← Lua.tointeger 1
    gyArg ← Lua.tointeger 2
    case (gxArg, gyArg) of
        (Just gx, Just gy) → do
            mParams ← Lua.liftIO (getWorldGenParams env)
            case mParams of
                Nothing → Lua.pushnil >> return 1
                Just p → do
                    let c = lookupLocalClimate (wgpClimateState p)
                                (wgpWorldSize p) (fromIntegral gx)
                                (fromIntegral gy)
                        putN k v = Lua.pushnumber (Lua.Number (realToFrac v))
                                   >> Lua.setfield (-2) k
                    Lua.newtable
                    putN "temp"       (lcTemp c)
                    putN "summerTemp" (lcSummerTemp c)
                    putN "winterTemp" (lcWinterTemp c)
                    putN "precip"     (lcPrecip c)
                    putN "humidity"   (lcHumidity c)
                    putN "snow"       (lcSnow c)
                    return 1
        _ → Lua.pushnil >> return 1

-- | world.getAmbientAt(gx, gy) → ambient air temperature (°C) | nil. The
--   elevation-corrected temperature a unit actually feels at the tile: the
--   regional climate mean minus the altitude lapse rate — the SAME correction
--   worldgen's ice system applies (see World.Weather.Ambient / issue #308), so
--   a unit on an ice-capped peak reads below freezing instead of the valley's
--   temperate mean. nil if no world is active. Used by scripts/thermo.lua.
worldGetAmbientAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetAmbientAtFn env = do
    gxArg ← Lua.tointeger 1
    gyArg ← Lua.tointeger 2
    case (gxArg, gyArg) of
        (Just gx, Just gy) → do
            mParams ← Lua.liftIO (getWorldGenParams env)
            case mParams of
                Nothing → Lua.pushnil >> return 1
                Just p → do
                    let t = ambientTempAt (wgpSeed p) (wgpPlates p)
                                (wgpClimateState p) (wgpWorldSize p)
                                (fromIntegral gx) (fromIntegral gy)
                    Lua.pushnumber (Lua.Number (realToFrac t))
                    return 1
        _ → Lua.pushnil >> return 1

-- | world.getSunAngleAt(gx, gy) → local sun angle (0..1) | nil. The
--   longitude-local day/night phase (#483) at a tile: the global clock's
--   angle offset by how far around the world cylinder (u = gx - gy) the
--   tile sits, so colonists on opposite sides of a small planet read
--   different times of day. nil if no world is active. Lets Lua (e.g. a
--   future sleep/circadian need, #479) be longitude-aware without
--   duplicating the wrap math.
worldGetSunAngleAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetSunAngleAtFn env = do
    gxArg ← Lua.tointeger 1
    gyArg ← Lua.tointeger 2
    case (gxArg, gyArg) of
        (Just gx, Just gy) → do
            mParams ← Lua.liftIO (getWorldGenParams env)
            case mParams of
                Nothing → Lua.pushnil >> return 1
                Just p → do
                    sunAngle ← Lua.liftIO (readIORef (sunAngleRef env))
                    let localAngle = localSunAngle (wgpWorldSize p)
                                        (fromIntegral gx) (fromIntegral gy)
                                        sunAngle
                    Lua.pushnumber (Lua.Number (realToFrac localAngle))
                    return 1
        _ → Lua.pushnil >> return 1
