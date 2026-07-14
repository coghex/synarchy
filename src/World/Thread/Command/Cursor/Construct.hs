{-# LANGUAGE UnicodeSyntax #-}

-- | Construction designation tool (#95). Mirrors the mine designation
--   tool: an anchor→rectangle commit that stores per-tile designations
--   (build target + status + progress) in wsConstructDesignationsRef.
--   The build AI (#96) is the consumer. Split out of
--   "World.Thread.Command.Cursor" (issue #564).
module World.Thread.Command.Cursor.Construct
    ( handleWorldSetConstructAnchorCommand
    , handleWorldClearConstructAnchorCommand
    , handleWorldDesignateConstructCommand
    , handleWorldCancelConstructCommand
    , handleWorldSetConstructStatusCommand
    , handleWorldAddConstructProgressCommand
    , handleWorldSetConstructDesignateTextureCommand
    , handleWorldSetConstructLineModeCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import qualified Data.Vector.Unboxed as VU
import World.Types
import World.Generate (globalToChunk)
import World.Construct.Types ( ConstructTarget(..), ConstructStatus(..)
                             , ConstructDesignation(..)
                             , StructurePiece(..)
                             , newConstructDesignation
                             , constructTargetCategory )
import World.Construct.Apply ( applyConstructSlopeToChunk
                             , clearConstructSlope )
import World.Thread.Command.Cursor.Common
    (maxDesignateSide, recordDesignationOutcome, recordMissingWorldOutcome)
import Structure.Types (StructureSlot, slotFromText)

handleWorldSetConstructAnchorCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldSetConstructAnchorCommand env _logger pageId gx gy = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { constructAnchor = Just (gx, gy) }, ())
        Nothing → pure ()

handleWorldClearConstructAnchorCommand ∷ EngineEnv → LoggerState → WorldPageId
    → IO ()
handleWorldClearConstructAnchorCommand env _logger pageId = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { constructAnchor = Nothing }, ())
        Nothing → pure ()

-- | Which structure slot a designation targets, mirroring
--   scripts/unit_ai_construct.lua's placeStructurePiece slot derivation
--   (a wall with no recorded edge defaults to "ne", a post to "n" — the
--   designation tool has no corner picker yet) so occupancy is checked
--   against the exact slot the worker will eventually place into (#805).
structurePieceSlot ∷ StructurePiece → Maybe StructureSlot
structurePieceSlot (StructurePiece _ kind edge) = case kind of
    "floor"   → slotFromText "floor"
    "ceiling" → slotFromText "ceiling"
    "wire"    → slotFromText "wire"
    "wall"    → slotFromText ("wall_" <> fromMaybe "ne" edge)
    "post"    → slotFromText ("post_" <> fromMaybe "n" edge)
    _         → Nothing

-- | Is a structure piece already placed at this (tile, slot)? Reads only
--   the authoritative per-chunk overlay ('lcStructures') — this handler
--   runs on the world thread's single command queue, so any
--   WorldSetStructure queued earlier (e.g. a worker's prior piece
--   placement) has already applied by the time this command runs; there
--   is no need to also consult the Lua read-your-writes staging cache
--   ('wsStructureStageRef'), which exists only for same-tick reads from
--   the debug builder (#805).
structureOccupiedAt ∷ WorldTileData → Int → Int → StructureSlot → Bool
structureOccupiedAt tileData gx gy slot =
    let (coord, _) = globalToChunk gx gy
        key = (gx, gy, fromIntegral (fromEnum slot) ∷ Word8)
    in maybe False (HM.member key . lcStructures) (lookupChunk coord tileData)

-- | Commit a construction designation. Per-z-level like mining: only
--   tiles at the anchor's surface z are taken. STRUCTURE targets fill the
--   whole rectangle (paint a floor / wall run), skipping any tile whose
--   requested slot is already occupied by a placed piece (#805 — a
--   structure designation must never spawn a job that would overwrite an
--   existing floor/ceiling/wall edge/post corner/wire, though compatible
--   slots on the same tile, e.g. a floor and a wall, still coexist);
--   BUILDING targets mark only the anchor tile (one footprint, not a
--   grid of buildings) and are unaffected by the occupancy check.
--   Unloaded-chunk tiles are skipped. Clears the anchor afterwards.
handleWorldDesignateConstructCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Int → Int → ConstructTarget → IO ()
handleWorldDesignateConstructCommand env logger pageId gx1 gy1 gx2 gy2 tgt = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing → recordMissingWorldOutcome env "construction.designate"
            pageId gx1 gy1
        Just worldState → do
            tileData ← readIORef (wsTilesRef worldState)
            let surfaceZAt gx gy = do
                    let (coord, (lx, ly)) = globalToChunk gx gy
                    lc ← lookupChunk coord tileData
                    pure (lcSurfaceMap lc VU.! columnIndex lx ly)
                xLo = min gx1 gx2
                yLo = min gy1 gy2
                xHi = min (max gx1 gx2) (xLo + maxDesignateSide - 1)
                yHi = min (max gy1 gy2) (yLo + maxDesignateSide - 1)
                -- A building only ever targets its single anchor tile
                -- (never the swept rectangle), so it always "requests"
                -- exactly 1 regardless of the two-click rectangle size.
                requested = case tgt of
                    CtBuilding _  → 1
                    CtStructure _ → (xHi - xLo + 1) * (yHi - yLo + 1)
                entries = case surfaceZAt gx1 gy1 of
                    Nothing → []   -- anchor chunk unloaded: nothing
                    Just anchorZ → case tgt of
                        -- A building is a single footprint: only the
                        -- anchor tile, at its own surface z.
                        CtBuilding _ →
                            [ ((gx1, gy1), newConstructDesignation anchorZ tgt) ]
                        -- Structure pieces tile the rectangle, per-z-level,
                        -- skipping any tile whose target slot is occupied.
                        CtStructure piece →
                            [ ((gx, gy), newConstructDesignation z tgt)
                            | gx ← [xLo .. xHi]
                            , gy ← [yLo .. yHi]
                            , Just z ← [surfaceZAt gx gy]
                            , z ≡ anchorZ
                            , maybe True (not . structureOccupiedAt tileData gx gy)
                                        (structurePieceSlot piece)
                            ]
            atomicModifyIORef' (wsConstructDesignationsRef worldState) $ \m →
                (foldl' (\acc (k, v) → HM.insert k v acc) m entries, ())
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { constructAnchor = Nothing }, ())
            logDebug logger CatWorld $
                "Construct designation: +" <> T.pack (show (length entries))
                <> " tiles (" <> constructTargetCategory tgt <> ")"
            recordDesignationOutcome env "construction.designate"
                "anchor tile ineligible, unloaded, or requested slot already occupied"
                xLo yLo requested (length entries)

handleWorldCancelConstructCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldCancelConstructCommand env _logger pageId gx gy = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            mCd ← atomicModifyIORef' (wsConstructDesignationsRef worldState) $
                \m → (HM.delete (gx, gy) m, HM.lookup (gx, gy) m)
            forM_ mCd $ resetConstructSlope worldState (gx, gy)
        Nothing → pure ()

-- | Build AI hook (#96): set a designation's status. Complete removes it
--   (and resets the corner-progress display back to flat ground — the
--   placed piece takes over from there).
handleWorldSetConstructStatusCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → ConstructStatus → IO ()
handleWorldSetConstructStatusCommand env _logger pageId gx gy st = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            mCd ← atomicModifyIORef' (wsConstructDesignationsRef worldState) $
                \m → case st of
                    CsComplete → (HM.delete (gx, gy) m, HM.lookup (gx, gy) m)
                    _          → (HM.adjust (\cd → cd { cdStatus = st })
                                           (gx, gy) m, Nothing)
            forM_ mCd $ resetConstructSlope worldState (gx, gy)
        Nothing → pure ()

-- | Build AI hook (#96): pour progress into a designation. Deltas are
--   normalised to the job's total work (1.0 = done); the accumulated
--   value is clamped to [0, 1]. Completion is NOT triggered here — the
--   build AI watches the value and places the piece itself, then sends
--   CsComplete. Each application re-stamps the tile's corner-progress
--   display (the mining slope-mask pipeline, 'World.Construct.Apply')
--   so the site visibly works corner-by-corner.
handleWorldAddConstructProgressCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Float → IO ()
handleWorldAddConstructProgressCommand env _logger pageId gx gy delta = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            mUpd ← atomicModifyIORef' (wsConstructDesignationsRef worldState) $
                \m → case HM.lookup (gx, gy) m of
                    Nothing → (m, Nothing)
                    Just cd →
                        let cd' = cd { cdProgress = max 0.0 (min 1.0
                                          (cdProgress cd + delta)) }
                        in ( HM.insert (gx, gy) cd' m
                           , Just (cdProgress cd, cd') )
            forM_ mUpd $ \(prevProgress, cd') →
                withConstructChunk worldState (gx, gy) $
                    applyConstructSlopeToChunk (gx, gy) prevProgress cd'
        Nothing → pure ()

-- | Run a chunk transform for the designation tile's loaded chunk and
--   invalidate the render caches — the same writeback the live dig
--   path uses ('handleWorldDigTileCommand'). No-op when the chunk
--   isn't loaded (the load path re-derives the display instead).
withConstructChunk ∷ WorldState → (Int, Int)
                   → (LoadedChunk → LoadedChunk) → IO ()
withConstructChunk worldState (gx, gy) f = do
    let (coord, _) = globalToChunk gx gy
    td ← readIORef (wsTilesRef worldState)
    case lookupChunk coord td of
        Nothing → pure ()
        Just lc → do
            let lc' = f lc
            atomicModifyIORef' (wsTilesRef worldState) $ \w →
                (insertChunk lc' w, ())
            bumpQuadCacheGen worldState
            writeIORef (wsZoomQuadCacheRef worldState) Nothing
            writeIORef (wsBgQuadCacheRef worldState)   Nothing

-- | Reset a removed designation's corner-progress display to flat
--   (guarded inside 'clearConstructSlope' to the designation's own
--   mask, so natural/authored slopes are untouched).
resetConstructSlope ∷ WorldState → (Int, Int) → ConstructDesignation → IO ()
resetConstructSlope worldState (gx, gy) cd =
    withConstructChunk worldState (gx, gy) $ clearConstructSlope (gx, gy) cd

handleWorldSetConstructDesignateTextureCommand ∷ EngineEnv → LoggerState
    → WorldPageId → Text → TextureHandle → IO ()
handleWorldSetConstructDesignateTextureCommand env _logger pageId cat tid = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                case cat of
                    "building" → (cs { constructBuildingTexture = Just tid }, ())
                    _          → (cs { constructStructTexture = Just tid }, ())
        Nothing → pure ()

-- | Wire path tool (#359): toggle the anchor→hover preview between the
--   default filled rectangle and a straight 1-wide line.
handleWorldSetConstructLineModeCommand ∷ EngineEnv → LoggerState
    → WorldPageId → Bool → IO ()
handleWorldSetConstructLineModeCommand env _logger pageId enabled = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { constructLineMode = enabled }, ())
        Nothing → pure ()
