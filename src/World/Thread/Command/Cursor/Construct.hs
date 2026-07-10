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
                             , newConstructDesignation
                             , constructTargetCategory )
import World.Construct.Apply ( applyConstructSlopeToChunk
                             , clearConstructSlope )
import World.Thread.Command.Cursor.Common (maxDesignateSide)

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

-- | Commit a construction designation. Per-z-level like mining: only
--   tiles at the anchor's surface z are taken. STRUCTURE targets fill the
--   whole rectangle (paint a floor / wall run); BUILDING targets mark
--   only the anchor tile (one footprint, not a grid of buildings).
--   Unloaded-chunk tiles are skipped. Clears the anchor afterwards.
handleWorldDesignateConstructCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Int → Int → ConstructTarget → IO ()
handleWorldDesignateConstructCommand env logger pageId gx1 gy1 gx2 gy2 tgt = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing → pure ()
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
                entries = case surfaceZAt gx1 gy1 of
                    Nothing → []   -- anchor chunk unloaded: nothing
                    Just anchorZ → case tgt of
                        -- A building is a single footprint: only the
                        -- anchor tile, at its own surface z.
                        CtBuilding _ →
                            [ ((gx1, gy1), newConstructDesignation anchorZ tgt) ]
                        -- Structure pieces tile the rectangle, per-z-level.
                        CtStructure _ →
                            [ ((gx, gy), newConstructDesignation z tgt)
                            | gx ← [xLo .. xHi]
                            , gy ← [yLo .. yHi]
                            , Just z ← [surfaceZAt gx gy]
                            , z ≡ anchorZ
                            ]
            atomicModifyIORef' (wsConstructDesignationsRef worldState) $ \m →
                (foldl' (\acc (k, v) → HM.insert k v acc) m entries, ())
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { constructAnchor = Nothing }, ())
            logDebug logger CatWorld $
                "Construct designation: +" <> T.pack (show (length entries))
                <> " tiles (" <> constructTargetCategory tgt <> ")"

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
