{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Lua API for the structures debug builder (walls / floors / ceilings).
--   Textures + facemaps are loaded Lua-side via engine.loadTexture (which
--   returns the raw TextureHandle int); their handles are passed straight
--   into structure.place. The store is a plain in-memory IORef (no save
--   yet) — this is a debug/iteration tool.
module Engine.Scripting.Lua.API.Structure
    ( structurePlaceFn
    , structureClearFn
    , structureClearAllFn
    , structureCountFn
    , structureFloorZAtFn
    ) where

import UPrelude
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..))
import Structure.Types

-- | structure.place(gx, gy, slot, texHandle, faceHandle [, z]) → bool
--   slot ∈ "floor"/"ceiling"/"wall_ne"/"wall_nw"/"wall_se"/"wall_sw".
--   texHandle/faceHandle are engine.loadTexture handles. z defaults to 0.
structurePlaceFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structurePlaceFn env = do
    gxA   ← Lua.tointeger 1
    gyA   ← Lua.tointeger 2
    slotA ← Lua.tostring 3
    texA  ← Lua.tointeger 4
    faceA ← Lua.tointeger 5
    zA    ← Lua.tointeger 6
    case (gxA, gyA, slotA, texA, faceA) of
        (Just gx, Just gy, Just slotBS, Just tex, Just face) →
            case slotFromText (TE.decodeUtf8 slotBS) of
                Nothing → Lua.pushboolean False >> return 1
                Just slot → do
                    let z     = maybe 0 fromIntegral zA
                        piece = StructurePiece
                                  (TextureHandle (fromIntegral tex))
                                  (TextureHandle (fromIntegral face))
                                  z
                        key   = (fromIntegral gx, fromIntegral gy, fromEnum slot)
                    Lua.liftIO $ atomicModifyIORef' (structureStoreRef env) $ \s →
                        (HM.insert key piece s, ())
                    Lua.pushboolean True
                    return 1
        _ → Lua.pushboolean False >> return 1

-- | structure.clear(gx, gy, slot) → bool — remove one piece.
structureClearFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureClearFn env = do
    gxA   ← Lua.tointeger 1
    gyA   ← Lua.tointeger 2
    slotA ← Lua.tostring 3
    case (gxA, gyA, slotA) of
        (Just gx, Just gy, Just slotBS) →
            case slotFromText (TE.decodeUtf8 slotBS) of
                Nothing → Lua.pushboolean False >> return 1
                Just slot → do
                    let key = (fromIntegral gx, fromIntegral gy, fromEnum slot)
                    Lua.liftIO $ atomicModifyIORef' (structureStoreRef env) $ \s →
                        (HM.delete key s, ())
                    Lua.pushboolean True
                    return 1
        _ → Lua.pushboolean False >> return 1

-- | structure.clearAll() — wipe the whole store.
structureClearAllFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureClearAllFn env = do
    Lua.liftIO $ writeIORef (structureStoreRef env) emptyStructureStore
    return 0

-- | structure.count() → int — number of placed pieces (headless verify).
structureCountFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureCountFn env = do
    s ← Lua.liftIO $ readIORef (structureStoreRef env)
    Lua.pushinteger (fromIntegral (HM.size s))
    return 1

-- | structure.floorZAt(gx, gy) → int|nil — the z of the FLOOR at this tile,
--   or nil if there is none. Posts are only placeable where a floor exists,
--   and take the floor's z (so they render on the floor, not the terrain).
structureFloorZAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureFloorZAtFn env = do
    gxA ← Lua.tointeger 1
    gyA ← Lua.tointeger 2
    case (gxA, gyA) of
        (Just gx, Just gy) → do
            s ← Lua.liftIO $ readIORef (structureStoreRef env)
            let key = (fromIntegral gx, fromIntegral gy, fromEnum SFloor)
            case HM.lookup key s of
                Just p  → Lua.pushinteger (fromIntegral (spGridZ p)) >> return 1
                Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1
