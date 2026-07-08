{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Structure-piece edit handlers (set/clear one piece, clear every
--   piece in the world). Split out of "World.Thread.Command.Edit"
--   (issue #563).
module World.Thread.Command.Edit.Structure
    ( handleWorldSetStructureCommand
    , handleWorldClearStructureCommand
    , handleWorldClearAllStructuresCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logWarn, LogCategory(..), LoggerState)
import World.Types
import World.Generate.Coordinates (globalToChunk)
import World.Edit.Types (WorldEdit(..), appendEdit)
import World.Edit.Apply (applyEdit)
import Structure.Types (emptyChunkStructures)
import World.Thread.Helpers (unWorldPageId)

-- | Place a structure piece (floor/wall/post/ceiling) at (gx,gy,slot-tag) via
--   the WeSetStructure edit path: live-apply to the loaded chunk's structure
--   overlay AND append to the per-chunk edit log, so it persists + replays on
--   eviction. Palette ids (texture/facemap) are resolved Lua-side; the cap
--   variant is already baked into facePaletteId (the BUILDER chose it). No
--   terrain is touched — but it shares the ordered log with terrain edits, so
--   a dig recorded before this lands before it on replay.
handleWorldSetStructureCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Word8 → Int → Int → Int → IO ()
handleWorldSetStructureCommand env logger pageId gx gy slotTag texId faceId z = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for set structure: " <> unWorldPageId pageId
        Just ws → do
            let (coord, _) = globalToChunk gx gy
                edit = WeSetStructure gx gy slotTag texId faceId z
            td ← readIORef (wsTilesRef ws)
            case lookupChunk coord td of
                Nothing →
                    logWarn logger CatWorld $
                        "Chunk not loaded for set structure at "
                          <> T.pack (show gx) <> "," <> T.pack (show gy)
                Just lc → do
                    let lc' = applyEdit edit lc
                    atomicModifyIORef' (wsTilesRef ws) $ \w →
                        (insertChunk lc' w, ())
                    atomicModifyIORef' (wsEditsRef ws) $ \es →
                        (appendEdit coord edit es, ())

-- | Remove the structure piece at (gx,gy,slot-tag) via WeClearStructure.
--   Unlike the SET path, the clear is recorded in the per-chunk edit log
--   ALWAYS — even when the chunk isn't loaded. The piece being cleared may
--   live only in the persisted edits of an UNLOADED/evicted chunk (its
--   WeSetStructure), so without recording the clear it would replay back on
--   reload / after save/load. The live lcStructures overlay is additionally
--   updated when the chunk happens to be loaded. (Replaying a clear with no
--   matching set is a harmless no-op — a HM.delete on an absent key.)
handleWorldClearStructureCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Word8 → IO ()
handleWorldClearStructureCommand env logger pageId gx gy slotTag = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for clear structure: " <> unWorldPageId pageId
        Just ws → do
            let (coord, _) = globalToChunk gx gy
                edit = WeClearStructure gx gy slotTag
            atomicModifyIORef' (wsEditsRef ws) $ \es →
                (appendEdit coord edit es, ())
            td ← readIORef (wsTilesRef ws)
            case lookupChunk coord td of
                Nothing → pure ()
                Just lc → do
                    let lc' = applyEdit edit lc
                    atomicModifyIORef' (wsTilesRef ws) $ \w →
                        (insertChunk lc' w, ())

-- | Remove EVERY structure piece in the world. Clears the live per-chunk
--   'lcStructures' overlay on all loaded chunks AND strips the structure
--   edits (WeSetStructure / WeClearStructure) from the per-chunk log so they
--   do not replay on eviction/reload. This is the authoritative "wipe all":
--   it touches the same overlay + edit-log that rendering and persistence
--   read, so a cleared world stays cleared after a chunk evicts or a
--   save/load round-trip. (No quad-cache bust: the structure pass renders
--   from 'lcStructures' live every frame, never from the cached terrain quads.)
handleWorldClearAllStructuresCommand ∷ EngineEnv → LoggerState → WorldPageId
    → IO ()
handleWorldClearAllStructuresCommand env logger pageId = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for clear all structures: " <> unWorldPageId pageId
        Just ws → do
            atomicModifyIORef' (wsTilesRef ws) $ \w →
                ( w { wtdChunks = HM.map clearChunkStructures (wtdChunks w) }
                , () )
            atomicModifyIORef' (wsEditsRef ws) $ \es →
                (HM.map (filter (not . isStructureEdit)) es, ())
  where
    clearChunkStructures lc = lc { lcStructures = emptyChunkStructures }
    isStructureEdit (WeSetStructure {})   = True
    isStructureEdit (WeClearStructure {}) = True
    isStructureEdit _                     = False
