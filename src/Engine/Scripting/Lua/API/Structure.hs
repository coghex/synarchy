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
    , structureHasAtFn
    , structureLoadedCountFn
    , structureUnresolvedPaletteIdsFn
    , structureSetPaletteHandleFn
    , structurePaletteCountFn
    ) where

import UPrelude
import Data.Word (Word8)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..))
import Structure.Types
import Structure.Palette (internPath, TexPalette(..))
import Control.Monad (forM_)
import World.Types (wmWorlds, wsTilesRef)
import World.Chunk.Types (LoadedChunk(..))
import World.Tile.Types (WorldTileData(..))
import World.Command.Types (WorldCommand(..))

-- | structure.place(gx, gy, slot, texHandle, faceHandle, z, texPath, facePath)
--   → bool. slot ∈ "floor"/"ceiling"/"post_n…w"/"wall_ne…sw".
--   texHandle/faceHandle = engine.loadTexture handles (for the in-memory store
--   the debug builder + current render use). texPath/facePath = the texture
--   PATHS — interned into the save-level palette + queued as a WeSetStructure
--   edit (the persistent, per-chunk, evict-survivable path). Omit the paths to
--   place WITHOUT persisting (pure debug). z defaults to 0.
structurePlaceFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structurePlaceFn env = do
    gxA   ← Lua.tointeger 1
    gyA   ← Lua.tointeger 2
    slotA ← Lua.tostring 3
    texA  ← Lua.tointeger 4
    faceA ← Lua.tointeger 5
    zA    ← Lua.tointeger 6
    texPathA  ← Lua.tostring 7
    facePathA ← Lua.tostring 8
    case (gxA, gyA, slotA, texA, faceA) of
        (Just gx, Just gy, Just slotBS, Just tex, Just face) →
            case slotFromText (TE.decodeUtf8 slotBS) of
                Nothing → Lua.pushboolean False >> return 1
                Just slot → do
                    let z       = maybe 0 fromIntegral zA
                        gxi     = fromIntegral gx
                        gyi     = fromIntegral gy
                        slotTag = fromIntegral (fromEnum slot) ∷ Word8
                        piece   = StructurePiece
                                    (TextureHandle (fromIntegral tex))
                                    (TextureHandle (fromIntegral face))
                                    z
                        key     = (gxi, gyi, fromEnum slot)
                    Lua.liftIO $ do
                        -- in-memory store (debug builder queries + current render)
                        atomicModifyIORef' (structureStoreRef env) $ \s →
                            (HM.insert key piece s, ())
                        -- persistent path: intern PATHS → palette ids, queue the edit
                        case (texPathA, facePathA) of
                            (Just tp, Just fp) → do
                                texId  ← atomicModifyIORef' (texPaletteRef env) $ \pal →
                                    let (i, pal') = internPath (TE.decodeUtf8 tp) pal
                                    in (pal', i)
                                faceId ← atomicModifyIORef' (texPaletteRef env) $ \pal →
                                    let (i, pal') = internPath (TE.decodeUtf8 fp) pal
                                    in (pal', i)
                                -- record paletteId → handle so the renderer can
                                -- resolve this piece immediately (the handle is
                                -- already loaded — passed in by the builder)
                                atomicModifyIORef' (texPaletteHandlesRef env) $ \m →
                                    ( HM.insert texId  (TextureHandle (fromIntegral tex))
                                    $ HM.insert faceId (TextureHandle (fromIntegral face)) m
                                    , () )
                                mgr ← readIORef (worldManagerRef env)
                                case wmWorlds mgr of
                                    ((pageId, _):_) →
                                        Q.writeQueue (worldQueue env)
                                            (WorldSetStructure pageId gxi gyi
                                                               slotTag texId faceId z)
                                    [] → pure ()
                            _ → pure ()   -- no paths → not persisted (debug-only)
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

-- | structure.count() → int — pieces in the OLD in-memory store (debug verify).
structureCountFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureCountFn env = do
    s ← Lua.liftIO $ readIORef (structureStoreRef env)
    Lua.pushinteger (fromIntegral (HM.size s))
    return 1

-- | structure.loadedCount() → int — pieces in the PERSISTENT per-chunk overlay
--   (lcStructures) across all loaded chunks of the active world. Verifies the
--   WeSetStructure edit path: nonzero after placement, and again after a
--   save/load (replayed from sdEdits).
structureLoadedCountFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureLoadedCountFn env = do
    mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
    case wmWorlds mgr of
        ((_, ws):_) → do
            td ← Lua.liftIO $ readIORef (wsTilesRef ws)
            let n = sum [ HM.size (lcStructures lc)
                        | lc ← HM.elems (wtdChunks td) ]
            Lua.pushinteger (fromIntegral n)
            return 1
        [] → Lua.pushinteger 0 >> return 1

-- | structure.paletteCount() → int — number of texture paths in the palette
--   (debug probe for save/restore of the palette).
structurePaletteCountFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structurePaletteCountFn env = do
    pal ← Lua.liftIO $ readIORef (texPaletteRef env)
    Lua.pushinteger (fromIntegral (HM.size (tpPathToId pal)))
    return 1

-- | structure.unresolvedPaletteIds() → { {id=, path=}, ... } — palette ids
--   that have no runtime texture handle yet (e.g. right after a load, which
--   clears the session-local handle map). The Lua resolve tick loadTexture's
--   each path and feeds it back via setPaletteHandle. Empty once all resolved.
structureUnresolvedPaletteIdsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureUnresolvedPaletteIdsFn env = do
    pal     ← Lua.liftIO $ readIORef (texPaletteRef env)
    handles ← Lua.liftIO $ readIORef (texPaletteHandlesRef env)
    let unresolved = [ (i, p) | (i, p) ← HM.toList (tpIdToPath pal)
                              , not (HM.member i handles) ]
    Lua.newtable
    forM_ (zip [1 ..] unresolved) $ \(n, (i, p)) → do
        Lua.newtable
        Lua.pushinteger (fromIntegral i)
        Lua.setfield (-2) "id"
        Lua.pushstring (TE.encodeUtf8 p)
        Lua.setfield (-2) "path"
        Lua.rawseti (-2) n
    return 1

-- | structure.setPaletteHandle(id, texHandle) — record a resolved palette id →
--   runtime handle (the renderer then resolves pieces using that id).
structureSetPaletteHandleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureSetPaletteHandleFn env = do
    idA ← Lua.tointeger 1
    hA  ← Lua.tointeger 2
    case (idA, hA) of
        (Just i, Just h) → Lua.liftIO $
            atomicModifyIORef' (texPaletteHandlesRef env) $ \m →
                (HM.insert (fromIntegral i) (TextureHandle (fromIntegral h)) m, ())
        _ → pure ()
    return 0

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

-- | structure.hasAt(gx, gy, slot) → bool — is there a piece at this (tile,
--   slot)? Used by the wall builder to test for a corner post at a node.
structureHasAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureHasAtFn env = do
    gxA   ← Lua.tointeger 1
    gyA   ← Lua.tointeger 2
    slotA ← Lua.tostring 3
    case (gxA, gyA, slotA) of
        (Just gx, Just gy, Just slotBS) →
            case slotFromText (TE.decodeUtf8 slotBS) of
                Nothing → Lua.pushboolean False >> return 1
                Just slot → do
                    s ← Lua.liftIO $ readIORef (structureStoreRef env)
                    let key = (fromIntegral gx, fromIntegral gy, fromEnum slot)
                    Lua.pushboolean (HM.member key s)
                    return 1
        _ → Lua.pushboolean False >> return 1
