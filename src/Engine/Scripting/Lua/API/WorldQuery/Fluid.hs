{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Fluid-surface queries: world.getFluidAt, world.getSurfaceAt (the
--   combined terrain+fluid query), world.getAreaFluid.
module Engine.Scripting.Lua.API.WorldQuery.Fluid
    ( worldGetFluidAtFn
    , worldGetSurfaceAtFn
    , worldGetAreaFluidFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Engine.Core.State (EngineEnv(..))
import World.Types
import World.Generate.Coordinates (globalToChunk)
import Engine.Scripting.Lua.API.WorldQuery.Lookup (getWorldTileData)

-- | world.getFluidAt(gx, gy) → type, surface | nil
--   Arity is the contract: on dry tiles (and unloaded/out-of-range
--   chunks) it pushes a single nil; on a fluid tile it pushes TWO
--   values — the fluid type string ("ocean","lake","river","lava") and
--   the fluid surface Z. So test `getFluidAt(x,y) ~= nil` (or capture
--   the first return) to distinguish "no fluid" from a fluid sitting at
--   surface 0 — never rely on the surface alone.
worldGetFluidAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetFluidAtFn env = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            let gx = fromIntegral gx'
                gy = fromIntegral gy'
                (coord, (lx, ly)) = globalToChunk gx gy
                idx = ly * chunkSize + lx
            mTd ← Lua.liftIO $ getWorldTileData env
            case mTd >>= lookupChunk coord of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just lc →
                    case (lcFluidMap lc) V.! idx of
                        Nothing → do
                            Lua.pushnil
                            return 1
                        Just fc → do
                            let typeStr = case fcType fc of
                                    Ocean → "ocean"
                                    Lake  → "lake"
                                    River → "river"
                                    Lava  → "lava"
                            Lua.pushstring typeStr
                            Lua.pushinteger (fromIntegral (fcSurface fc))
                            return 2
        _ → do
            Lua.pushnil
            return 1

-- | world.getSurfaceAt(gx, gy) → surfaceZ, terrainSurfaceZ, fluidType, fluidSurface
--   Combined query: returns all surface info at a global coordinate.
worldGetSurfaceAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetSurfaceAtFn env = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    case (mGx, mGy) of
        (Just gx', Just gy') → do
            let gx = fromIntegral gx'
                gy = fromIntegral gy'
                (coord, (lx, ly)) = globalToChunk gx gy
                idx = ly * chunkSize + lx
            mTd ← Lua.liftIO $ getWorldTileData env
            case mTd >>= lookupChunk coord of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just lc → do
                    let surfZ = (lcSurfaceMap lc) VU.! idx
                        terrZ = (lcTerrainSurfaceMap lc) VU.! idx
                    Lua.pushinteger (fromIntegral surfZ)
                    Lua.pushinteger (fromIntegral terrZ)
                    case (lcFluidMap lc) V.! idx of
                        Nothing → do
                            Lua.pushnil
                            Lua.pushnil
                        Just fc → do
                            let typeStr = case fcType fc of
                                    Ocean → "ocean"
                                    Lake  → "lake"
                                    River → "river"
                                    Lava  → "lava"
                            Lua.pushstring typeStr
                            Lua.pushinteger (fromIntegral (fcSurface fc))
                    return 4
        _ → do
            Lua.pushnil
            return 1

-- | world.getAreaFluid(gx, gy, radius) → table of {x,y,type,surface,terrainZ}
--   Scans a square area and returns all fluid cells found.
--   Useful for debugging river coverage around a point.
worldGetAreaFluidFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetAreaFluidFn env = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    mR  ← Lua.tointeger 3
    case (mGx, mGy, mR) of
        (Just gx', Just gy', Just r') → do
            let gx = fromIntegral gx' ∷ Int
                gy = fromIntegral gy' ∷ Int
                radius = min 64 (fromIntegral r' ∷ Int)  -- cap at 64
            mTd ← Lua.liftIO $ getWorldTileData env
            case mTd of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just td → do
                    Lua.newtable
                    let go !n !x !y
                            | y > gy + radius = return n
                            | x > gx + radius = go n (gx - radius) (y + 1)
                            | otherwise = do
                                let (coord, (lx, ly)) = globalToChunk x y
                                    idx = ly * chunkSize + lx
                                case lookupChunk coord td of
                                    Nothing → go n (x + 1) y
                                    Just lc →
                                        case (lcFluidMap lc) V.! idx of
                                            Nothing → go n (x + 1) y
                                            Just fc → do
                                                let terrZ = (lcTerrainSurfaceMap lc) VU.! idx
                                                    typeStr = case fcType fc of
                                                        Ocean → "ocean"
                                                        Lake  → "lake"
                                                        River → "river"
                                                        Lava  → "lava"
                                                Lua.newtable
                                                Lua.pushinteger (fromIntegral x)
                                                Lua.setfield (Lua.nth 2) "x"
                                                Lua.pushinteger (fromIntegral y)
                                                Lua.setfield (Lua.nth 2) "y"
                                                Lua.pushstring typeStr
                                                Lua.setfield (Lua.nth 2) "type"
                                                Lua.pushinteger (fromIntegral (fcSurface fc))
                                                Lua.setfield (Lua.nth 2) "surface"
                                                Lua.pushinteger (fromIntegral terrZ)
                                                Lua.setfield (Lua.nth 2) "terrainZ"
                                                Lua.rawseti (Lua.nth 2) n
                                                go (n + 1) (x + 1) y
                    _ ← go 1 (gx - radius) (gy - radius)
                    return 1
        _ → do
            Lua.pushnil
            return 1
