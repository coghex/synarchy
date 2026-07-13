{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.World.Query
    ( worldGetDigInfoAtFn
    , worldGetGemInfoAtFn
    , worldGetSpoilInfoFn
    , worldListMaterialsFn
    , worldDebugTileQuadsFn
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..))
import World.Types
import World.Generate.Coordinates (globalToChunk)
import World.Material (MaterialId(..), getMaterialProps, MaterialProps(..)
                      , materialIdByName)
import World.Gem (gemChanceAt)
import World.Spoil.Logic (spoilBlockedAt)
import World.Spoil.Types (SpoilPile(..))
import World.Mine.Types (MineDesignation(..))
import World.Render.Quads (renderWorldQuads)
import World.Render.Textures (getTileTexture)
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..))
import Engine.Scene.Types (SortableQuad(..))

-- | world.getDigInfoAt(pageId, gx, gy)
--     → matId, pickSpeed, shovelSpeed, spoilBlocked | nil
--   Material being dug at a designated tile (the column's material at
--   the designation's z) and its per-tool dig-rate multipliers. nil
--   when the tile isn't designated or its chunk isn't loaded.
--   spoilBlocked is true when the material produces spoil and the
--   surrounding piles have no room — the dig command will refuse, so
--   the AI should skip the tile instead of digging in place forever.
worldGetDigInfoAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetDigInfoAtFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gxN, Just gyN) → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                gx = round gxN ∷ Int
                gy = round gyN ∷ Int
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    desigs ← Lua.liftIO $ readIORef (wsMineDesignationsRef ws)
                    tileData ← Lua.liftIO $ readIORef (wsTilesRef ws)
                    registry ← Lua.liftIO $ readIORef (materialRegistryRef env)
                    piles ← Lua.liftIO $ readIORef (wsSpoilRef ws)
                    let mInfo = do
                            md ← HM.lookup (gx, gy) desigs
                            let (coord, (lx, ly)) = globalToChunk gx gy
                            lc ← lookupChunk coord tileData
                            let col = lcTiles lc V.! columnIndex lx ly
                                relZ = mdZ md - ctStartZ col
                            if relZ ≥ 0 ∧ relZ < VU.length (ctMats col)
                                then pure (ctMats col VU.! relZ, mdZ md)
                                else Nothing
                    case mInfo of
                        Nothing → Lua.pushnil >> return 1
                        Just (matId, digZ) → do
                            let props = getMaterialProps registry
                                            (MaterialId matId)
                                -- Blocked check from the tile center;
                                -- the per-tick gate re-checks with the
                                -- digger's real position.
                                blocked = case mpDigSpoil props of
                                    Nothing → False
                                    Just spoilName →
                                        case materialIdByName registry
                                                 spoilName of
                                            Nothing → False
                                            Just spoilId →
                                                spoilBlockedAt tileData
                                                    desigs piles spoilId
                                                    digZ
                                                    ( fromIntegral gx + 0.5
                                                    , fromIntegral gy + 0.5 )
                                                    (gx, gy)
                            Lua.pushinteger (fromIntegral matId)
                            Lua.pushnumber (Lua.Number
                                (realToFrac (mpPickSpeed props)))
                            Lua.pushnumber (Lua.Number
                                (realToFrac (mpShovelSpeed props)))
                            Lua.pushboolean blocked
                            return 4
        _ → Lua.pushnil >> return 1

-- | world.listMaterials() → array of {id, name}
--   Every registered (non-default) material, sorted by id. Drives the
--   debug overlay's Terrain placement list.
worldListMaterialsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldListMaterialsFn env = do
    registry ← Lua.liftIO $ readIORef (materialRegistryRef env)
    let mats = [ (i, mpName props)
               | i ← [1 ∷ Int .. 255]
               , let props = getMaterialProps registry
                                 (MaterialId (fromIntegral i))
               , mpName props ≠ "unknown" ]
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] mats) $ \(n, (i, name)) → do
        Lua.newtable
        Lua.pushinteger (fromIntegral i)
        Lua.setfield (Lua.nth 2) "id"
        Lua.pushstring (TE.encodeUtf8 name)
        Lua.setfield (Lua.nth 2) "name"
        Lua.rawseti (Lua.nth 2) (fromIntegral n)
    return 1

-- | world.debugTileQuads(pageId, matId) → {total, matQuads, sample}
--   Introspection: run the REAL chunk-quad pass with the current
--   camera and report how many quads sample the given material's
--   tile texture (plus the first match's rect). Splits "the cell
--   isn't producing a quad" from "the quad reaches the GPU and the
--   shader/sort hides it" without a GUI debugger — same idea as
--   item.debugQuads.
worldDebugTileQuadsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldDebugTileQuadsFn env = do
    pageIdArg ← Lua.tostring 1
    matArg ← Lua.tonumber 2
    case (pageIdArg, matArg) of
        (Just pageIdBS, Just (Lua.Number matN)) → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    (quads, texH) ← Lua.liftIO $ do
                        camera ← readIORef (cameraRef env)
                        (fbW, fbH) ← readIORef (framebufferSizeRef env)
                        textures ← readIORef (wsTexturesRef ws)
                        let snap = WorldCameraSnapshot
                                { wcsPosition = camPosition camera
                                , wcsZoom     = camZoom camera
                                , wcsZSlice   = camZSlice camera
                                , wcsFbSize   = (fbW, fbH)
                                , wcsFacing   = camFacing camera
                                }
                        qs ← renderWorldQuads env ws 1.0 snap
                        pure (qs, getTileTexture textures
                                      (round matN ∷ Word8))
                    let matches = V.filter (\q → sqTexture q ≡ texH) quads
                    Lua.newtable
                    Lua.pushinteger (fromIntegral (V.length quads))
                    Lua.setfield (Lua.nth 2) "total"
                    Lua.pushinteger (fromIntegral (V.length matches))
                    Lua.setfield (Lua.nth 2) "matQuads"
                    case matches V.!? 0 of
                        Nothing → pure ()
                        Just q → do
                            let Vec2 qx qy = pos (sqV0 q)
                            Lua.newtable
                            Lua.pushnumber (Lua.Number (realToFrac qx))
                            Lua.setfield (Lua.nth 2) "x"
                            Lua.pushnumber (Lua.Number (realToFrac qy))
                            Lua.setfield (Lua.nth 2) "y"
                            Lua.pushnumber (Lua.Number
                                (realToFrac (sqSortKey q)))
                            Lua.setfield (Lua.nth 2) "sort"
                            Lua.setfield (Lua.nth 2) "sample"
                    return 1
        _ → Lua.pushnil >> return 1

-- | world.getGemInfoAt(pageId, gx, gy) → {gem, chance} | nil
--   What the seeded gem region field says about a tile: the gem the
--   region hosts and the per-completed-tile find chance at
--   perception 1.0. nil for barren regions. Debug/validation (and a
--   future prospecting skill could surface this in-game).
worldGetGemInfoAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetGemInfoAtFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gxN, Just gyN) → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    paramsM ← Lua.liftIO $ readIORef (wsGenParamsRef ws)
                    let seed = maybe 0 (fromIntegral ∘ wgpSeed) paramsM
                    case gemChanceAt seed (round gxN, round gyN) 1.0 of
                        Nothing → Lua.pushnil >> return 1
                        Just (gem, chance) → do
                            Lua.newtable
                            Lua.pushstring (TE.encodeUtf8 gem)
                            Lua.setfield (Lua.nth 2) "gem"
                            Lua.pushnumber (Lua.Number
                                (realToFrac chance))
                            Lua.setfield (Lua.nth 2) "chance"
                            return 1
        _ → Lua.pushnil >> return 1

-- | world.getSpoilInfo(pageId) → {piles, totalFill} | nil
--   Debug/validation view of the spoil overlay: pile (vertex) count
--   and the summed fractional fill in corner-units. Promoted cells
--   are NOT in here (they're real terrain via WeAddTile) — the G4
--   conservation harness combines this with terrain deltas.
worldGetSpoilInfoFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetSpoilInfoFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Nothing → Lua.pushnil >> return 1
        Just pageIdBS → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    piles ← Lua.liftIO $ readIORef (wsSpoilRef ws)
                    let sumC (a, b, c, d) = a + b + c + d
                        total = sum [ sumC (spFill p)
                                    | p ← HM.elems piles ]
                    Lua.newtable
                    Lua.pushinteger (fromIntegral (HM.size piles))
                    Lua.setfield (Lua.nth 2) "piles"
                    Lua.pushnumber (Lua.Number (realToFrac total))
                    Lua.setfield (Lua.nth 2) "totalFill"
                    return 1
