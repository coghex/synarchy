{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Lua API for the structures debug builder (walls / floors / ceilings).
--   Textures + facemaps are loaded Lua-side via engine.loadTexture (which
--   returns the raw TextureHandle int); their handles are passed straight
--   into structure.place. Writes route through the WeSetStructure /
--   WeClearStructure edit path so the AUTHORITATIVE per-chunk 'lcStructures'
--   overlay (the same data rendering + persistence use) stays the single
--   source of truth and survives chunk eviction + save/load. (The old
--   'structureStoreRef', a SECOND authority rendering never read, is gone.)
--
--   Those edits apply asynchronously on the world thread, but the builder
--   places a piece and queries it within the same Lua call (floors→posts→
--   walls), so the read helpers consult the active world's per-world
--   'wsStructureStageRef' write-ahead cache first and fall back to
--   'lcStructures' — read-your-writes without a second authority. Per-world
--   so a placement can't leak across worlds (see 'wsStructureStageRef').
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
import Engine.Core.State (EngineEnv(..), activeWorldPage, activeWorldState)
import Engine.Asset.Handle (TextureHandle(..))
import Structure.Types
import Structure.Palette (internPath, TexPalette(..))
import Control.Monad (forM_)
import World.Types (wsTilesRef, wsStructureStageRef, wmWorlds, WorldPageId(..), WorldState)
import World.Chunk.Types (LoadedChunk(..))
import World.Tile.Types (WorldTileData(..), lookupChunk)
import World.Generate.Coordinates (globalToChunk)
import World.Command.Types (WorldCommand(..))

-- | Resolve which world page a structure op targets: a named page (any in
--   wmWorlds, even hidden / non-active) when a page-id string is given,
--   else the active world. Location stamping (#89) passes the page id so a
--   room authored on a hidden secondary page writes onto THAT page — not
--   whichever page happens to be active when its chunk loaded.
resolveStructurePage ∷ EngineEnv → Maybe Text → IO (Maybe (WorldPageId, WorldState))
resolveStructurePage env (Just pid) = do
    mgr ← readIORef (worldManagerRef env)
    pure $ (\ws → (WorldPageId pid, ws)) <$> lookup (WorldPageId pid) (wmWorlds mgr)
resolveStructurePage env Nothing = activeWorldPage env

-- | structure.place(gx, gy, slot, texHandle, faceHandle, z, texPath, facePath)
--   → bool. slot ∈ "floor"/"ceiling"/"post_n…w"/"wall_ne…sw".
--   texHandle/faceHandle = engine.loadTexture handles, recorded against the
--   piece's palette ids so the renderer can resolve it immediately.
--   texPath/facePath = the texture PATHS — interned into the save-level
--   palette + queued as a WeSetStructure edit (the persistent, per-chunk,
--   evict-survivable path, and the only path that actually places a piece).
--   Omit the paths and nothing is placed. z defaults to 0. Returns false (and
--   does nothing) when the paths are omitted, there is no active world, or the
--   target chunk isn't loaded — the last because the world thread drops a
--   structure edit for an unloaded chunk, so staging one would be a phantom.
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
    pageA     ← Lua.tostring 9
    case (gxA, gyA, slotA, texA, faceA) of
        (Just gx, Just gy, Just slotBS, Just tex, Just face) →
            case slotFromText (TE.decodeUtf8 slotBS) of
                Nothing → Lua.pushboolean False >> return 1
                Just slot → do
                    let z       = maybe 0 fromIntegral zA
                        gxi     = fromIntegral gx
                        gyi     = fromIntegral gy
                        slotTag = fromIntegral (fromEnum slot) ∷ Word8
                    placed ← Lua.liftIO $
                        -- The per-chunk overlay (lcStructures), reached via the
                        -- WeSetStructure edit, is the SINGLE source of truth that
                        -- rendering + persistence read. Intern the PATHS → palette
                        -- ids and queue the edit. Omitting the paths places
                        -- nothing — there is no separate in-memory debug store.
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
                                mActive ← resolveStructurePage env (TE.decodeUtf8 <$> pageA)
                                case mActive of
                                    Just (pageId, ws) → do
                                        -- Only stage + queue when the target chunk
                                        -- is loaded: the world thread DROPS a
                                        -- WeSetStructure for an unloaded chunk
                                        -- (Edit.hs), so staging one regardless would
                                        -- leave a phantom that floorZAt/hasAt report
                                        -- as real though the world never changed
                                        -- (reachable stamping a room across a chunk
                                        -- boundary). Return false instead.
                                        td ← readIORef (wsTilesRef ws)
                                        let (coord, _) = globalToChunk gxi gyi
                                        case lookupChunk coord td of
                                            Nothing → pure False
                                            Just _  → do
                                                -- staged for read-your-writes: the
                                                -- builder queries this tile within
                                                -- the same Lua call (floors→posts→
                                                -- walls) before the world thread has
                                                -- applied it. (See wsStructureStageRef.)
                                                atomicModifyIORef' (wsStructureStageRef ws) $ \st →
                                                    ( HM.insert (gxi, gyi, slotTag)
                                                                (StructurePieceData texId faceId z) st
                                                    , () )
                                                Q.writeQueue (worldQueue env)
                                                    (WorldSetStructure pageId gxi gyi
                                                                       slotTag texId faceId z)
                                                pure True
                                    Nothing → pure False
                            _ → pure False   -- no paths → nothing placed
                    Lua.pushboolean placed
                    return 1
        _ → Lua.pushboolean False >> return 1

-- | structure.clear(gx, gy, slot) → bool — remove one piece from the
--   authoritative overlay via the WeClearStructure edit path (so it stays
--   cleared after eviction + save/load). Returns false if there is no active
--   world to queue against.
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
                    let gxi     = fromIntegral gx
                        gyi     = fromIntegral gy
                        slotTag = fromIntegral (fromEnum slot) ∷ Word8
                    ok ← Lua.liftIO $ do
                        mActive ← activeWorldPage env
                        case mActive of
                            Just (pageId, ws) → do
                                -- drop any staged add (no-op if not staged) so a
                                -- re-query doesn't surface the just-cleared piece
                                atomicModifyIORef' (wsStructureStageRef ws) $ \st →
                                    (HM.delete (gxi, gyi, slotTag) st, ())
                                Q.writeQueue (worldQueue env)
                                    (WorldClearStructure pageId gxi gyi slotTag)
                                pure True
                            Nothing → pure False
                    Lua.pushboolean ok
                    return 1
        _ → Lua.pushboolean False >> return 1

-- | structure.clearAll() — wipe every structure piece in the active world
--   (live overlays + persisted edits) via the WorldClearAllStructures command,
--   and drop the read-your-writes staging cache.
structureClearAllFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureClearAllFn env = do
    Lua.liftIO $ do
        mActive ← activeWorldPage env
        case mActive of
            Just (pageId, ws) → do
                writeIORef (wsStructureStageRef ws) emptyChunkStructures
                Q.writeQueue (worldQueue env) (WorldClearAllStructures pageId)
            Nothing → pure ()
    return 0

-- | structure.count() → int — distinct structure pieces in the active world:
--   the authoritative per-chunk overlay (lcStructures across loaded chunks)
--   unioned with the read-your-writes staging cache (just-placed pieces the
--   world thread hasn't applied yet). Agrees with floorZAt/hasAt.
structureCountFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureCountFn env = do
    mWs ← Lua.liftIO $ activeWorldState env
    n ← case mWs of
        Just ws → Lua.liftIO $ do
            st ← readIORef (wsStructureStageRef ws)
            td ← readIORef (wsTilesRef ws)
            let lcMap = HM.unions [ lcStructures lc | lc ← HM.elems (wtdChunks td) ]
            pure $ HM.size (HM.union st lcMap)
        Nothing → pure 0
    Lua.pushinteger (fromIntegral n)
    return 1

-- | structure.loadedCount() → int — pieces in the PERSISTENT per-chunk overlay
--   (lcStructures) across all loaded chunks of the active world. Verifies the
--   WeSetStructure edit path: nonzero after placement, and again after a
--   save/load (replayed from sdEdits).
structureLoadedCountFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureLoadedCountFn env = do
    mWs ← Lua.liftIO $ activeWorldState env
    case mWs of
        Just ws → do
            td ← Lua.liftIO $ readIORef (wsTilesRef ws)
            let n = sum [ HM.size (lcStructures lc)
                        | lc ← HM.elems (wtdChunks td) ]
            Lua.pushinteger (fromIntegral n)
            return 1
        Nothing → Lua.pushinteger 0 >> return 1

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
--   Reads the staging cache then the authoritative per-chunk overlay (see
--   'lookupStructure'), so it sees a floor placed earlier in the same Lua
--   call AND agrees with what is rendered/persisted after a save/load replay.
structureFloorZAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureFloorZAtFn env = do
    gxA ← Lua.tointeger 1
    gyA ← Lua.tointeger 2
    pageA ← Lua.tostring 3
    case (gxA, gyA) of
        (Just gx, Just gy) → do
            mSpd ← Lua.liftIO $
                lookupStructure env (TE.decodeUtf8 <$> pageA)
                                (fromIntegral gx) (fromIntegral gy) SFloor
            case mSpd of
                Just spd → Lua.pushinteger (fromIntegral (spdGridZ spd)) >> return 1
                Nothing  → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | structure.hasAt(gx, gy, slot) → bool — is there a piece at this (tile,
--   slot)? Used by the wall builder to test for a corner post at a node.
--   Staging cache then authoritative overlay (see 'structureFloorZAtFn').
structureHasAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureHasAtFn env = do
    gxA   ← Lua.tointeger 1
    gyA   ← Lua.tointeger 2
    slotA ← Lua.tostring 3
    pageA ← Lua.tostring 4
    case (gxA, gyA, slotA) of
        (Just gx, Just gy, Just slotBS) →
            case slotFromText (TE.decodeUtf8 slotBS) of
                Nothing → Lua.pushboolean False >> return 1
                Just slot → do
                    mSpd ← Lua.liftIO $
                        lookupStructure env (TE.decodeUtf8 <$> pageA)
                                        (fromIntegral gx) (fromIntegral gy) slot
                    Lua.pushboolean (maybe False (const True) mSpd)
                    return 1
        _ → Lua.pushboolean False >> return 1

-- | Look up a structure piece for the builder. Consults the read-your-writes
--   staging cache first (a piece placed earlier in the SAME Lua call that the
--   world thread hasn't applied yet), then falls back to the AUTHORITATIVE
--   per-chunk overlay ('lcStructures') — the same data rendering and
--   persistence read, and what's left after a save/load replay (when the cache
--   is empty). Nothing if it's in neither: no staged add, and either no active
--   world, the chunk holding (gx,gy) isn't loaded, or no piece there.
lookupStructure ∷ EngineEnv → Maybe Text → Int → Int → StructureSlot
                → IO (Maybe StructurePieceData)
lookupStructure env mPage gx gy slot = do
    mWs ← fmap snd <$> resolveStructurePage env mPage
    case mWs of
        Nothing → pure Nothing
        Just ws → do
            let key = (gx, gy, fromIntegral (fromEnum slot) ∷ Word8)
            st ← readIORef (wsStructureStageRef ws)
            case HM.lookup key st of
                Just spd → pure (Just spd)
                Nothing  → do
                    td ← readIORef (wsTilesRef ws)
                    let (coord, _) = globalToChunk gx gy
                    pure $ lookupChunk coord td ⌦ HM.lookup key . lcStructures
