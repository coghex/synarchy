{-# LANGUAGE Strict #-}

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
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv)
import World.Construct.Revalidate
    (ConstructScope(..), revalidateConstructDesignations)
import Engine.Core.Log (logWarn, LogCategory(..), LoggerState)
import World.Types
import World.Generate.Coordinates (globalToChunk)
import World.Edit.Types (WorldEdit(..), appendEdit)
import World.Edit.Apply (applyEdit)
import Structure.Types
    ( StructureStageToken, dropStagedAttempt, recordDeclinedAttempt
    , emptyChunkStructures )
import World.Flora.Designation (replaceChunkForgettingFlora)

-- | Place a structure piece (floor/wall/post/ceiling) at (gx,gy,slot-tag) via
--   the WeSetStructure edit path: live-apply to the loaded chunk's structure
--   overlay AND append to the per-chunk edit log, so it persists + replays on
--   eviction. Palette ids (texture/facemap) are resolved Lua-side; the cap
--   variant is already baked into facePaletteId (the BUILDER chose it). No
--   terrain is touched — but it shares the ordered log with terrain edits, so
--   a dig recorded before this lands before it on replay.
--
--   __Declining is a retraction, not just a refusal (#1674).__ The chunk can
--   evict between @structure.place@'s own residency check and this one (a
--   load pass inserts and evicts in one atomic 'wsTilesRef' update), and the
--   caller has already written its read-your-writes entry into
--   'wsStructureStageRef'. Dropping the edit alone would leave that entry as
--   a phantom every structure query reports as real, absent from the edit log
--   and gone after a save/load. So the unloaded branch retracts the staged
--   entry for the attempt this command names — matched by TOKEN, so a newer
--   placement staged at the same tile and slot (even a byte-identical one)
--   survives its predecessor's decline. A commit that SUCCEEDS leaves the
--   stage alone: it agrees with the overlay there.
--
--   __The decline is also RECORDED (#2051).__ Retracting the stage undoes
--   this attempt, but it leaves no trace that the attempt ever failed —
--   and the caller has by then been told @structure.place@ returned true,
--   which is what a location stamp was reading as "materialized". So the
--   attempt's token joins 'ssDeclined' in the same atomic update, where
--   'World.Thread.Command.Location.handleWorldMarkLocationStampedCommand'
--   consults it before writing a durable completion marker.
handleWorldSetStructureCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Word8 → Int → Int → Int → StructureStageToken → IO ()
handleWorldSetStructureCommand env logger pageId gx gy slotTag texId faceId z tok = do
    let wsc = toWorldSimCapability env
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for set structure: " <> unWorldPageId pageId
        Just ws → do
            let (coord, _) = globalToChunk gx gy
                edit = WeSetStructure gx gy slotTag texId faceId z
            td ← readIORef (wsTilesRef ws)
            case lookupChunk coord td of
                Nothing → do
                    atomicModifyIORef' (wsStructureStageRef ws) $ \st →
                        ( recordDeclinedAttempt tok
                            (dropStagedAttempt (gx, gy, slotTag) tok st)
                        , () )
                    logWarn logger CatWorld $
                        "Chunk not loaded for set structure at "
                          <> tshow gx <> "," <> tshow gy
                Just lc → do
                    let lc' = applyEdit edit lc
                    -- #1854 requirement 16: an edit that takes the tile's
                    -- rooted flora with it must take that plant's
                    -- designation and regrowth timer too, or an orphan
                    -- entry outlives the plant it addressed.
                    replaceChunkForgettingFlora ws lc lc'
                    atomicModifyIORef' (wsEditsRef ws) $ \es →
                        (appendEdit coord edit es, ())
                    -- #1844: the tile's slot occupancy just changed, so
                    -- any structure designation on it is re-resolved —
                    -- scoped to that one tile. A designation inside its
                    -- own claimant's placement hand-off is skipped
                    -- there, which is what stops a worker's successful
                    -- placement from cancelling its own job.
                    _ ← revalidateConstructDesignations env logger ws
                            (ConstructKeys [(gx, gy)])
                    pure ()

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
    let wsc = toWorldSimCapability env
    mgr ← readIORef (wsWorldManagerRef wsc)
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
                    -- #1854 requirement 16: an edit that takes the tile's
                    -- rooted flora with it must take that plant's
                    -- designation and regrowth timer too, or an orphan
                    -- entry outlives the plant it addressed.
                    replaceChunkForgettingFlora ws lc lc'
            -- #1844: a CLEAR can free a slot a designation wanted, and
            -- can remove the floor a post designation stands on. Fired
            -- whatever the chunk's residency, since the clear is
            -- recorded either way.
            _ ← revalidateConstructDesignations env logger ws
                    (ConstructKeys [(gx, gy)])
            pure ()

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
    let wsc = toWorldSimCapability env
    mgr ← readIORef (wsWorldManagerRef wsc)
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
            -- #1844: a wholesale wipe changes every tile's occupancy at
            -- once, which is the one structure write whose scope really
            -- is the page.
            _ ← revalidateConstructDesignations env logger ws
                    ConstructWholePage
            pure ()
  where
    clearChunkStructures lc = lc { lcStructures = emptyChunkStructures }
    isStructureEdit (WeSetStructure {})   = True
    isStructureEdit (WeClearStructure {}) = True
    isStructureEdit _                     = False
