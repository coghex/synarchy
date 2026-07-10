{-# LANGUAGE UnicodeSyntax #-}

-- | Chop designation tool (#97). Mirrors the mine/construct designation
--   tools' anchor→rectangle commit, but the commit filters to FLORA:
--   only tiles holding a currently-harvestable species carrying the
--   requested harvest tag ("wood") are designated. No per-z-level
--   filter — forests span slopes, and the designation z is only the
--   marker's render height. The chop AI (scripts/unit_ai.lua) is the
--   consumer. Split out of "World.Thread.Command.Cursor" (issue #564).
module World.Thread.Command.Cursor.Chop
    ( handleWorldSetChopAnchorCommand
    , handleWorldClearChopAnchorCommand
    , handleWorldDesignateChopCommand
    , handleWorldCancelChopCommand
    , handleWorldSetChopDesignateTextureCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import qualified Data.Vector.Unboxed as VU
import World.Types
import World.Generate (chunkToGlobal, globalToChunk)
import World.Chop.Types (newChopDesignation)
import World.Thread.Command.Cursor.Common
    (maxDesignateSide, recordDesignationOutcome)

handleWorldSetChopAnchorCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldSetChopAnchorCommand env _logger pageId gx gy = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { chopAnchor = Just (gx, gy) }, ())
        Nothing → pure ()

handleWorldClearChopAnchorCommand ∷ EngineEnv → LoggerState → WorldPageId
    → IO ()
handleWorldClearChopAnchorCommand env _logger pageId = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { chopAnchor = Nothing }, ())
        Nothing → pure ()

-- | Commit a chop designation: every loaded-chunk tile in the rectangle
--   whose flora includes a harvestable species tagged @tag@ with no live
--   regrowth timer (a regrowing stump has nothing to chop), each at its
--   own surface z. Clears the anchor afterwards.
handleWorldDesignateChopCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Int → Int → Text → IO ()
handleWorldDesignateChopCommand env logger pageId gx1 gy1 gx2 gy2 tag = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing → pure ()
        Just worldState → do
            tileData ← readIORef (wsTilesRef worldState)
            cat ← readIORef (floraCatalogRef env)
            harvests ← readIORef (wsFloraHarvestsRef worldState)
            let xLo = min gx1 gx2
                yLo = min gy1 gy2
                xHi = min (max gx1 gx2) (xLo + maxDesignateSide - 1)
                yHi = min (max gy1 gy2) (yLo + maxDesignateSide - 1)
                (cLo, _) = globalToChunk xLo yLo
                (cHi, _) = globalToChunk xHi yHi
                ChunkCoord cx0 cy0 = cLo
                ChunkCoord cx1 cy1 = cHi
                -- Walk the overlapped chunks' flora instances rather
                -- than probing every rect tile — a designation sweep is
                -- mostly empty ground.
                entries =
                    [ ((tgx, tgy), newChopDesignation z)
                    | cx ← [cx0 .. cx1], cy ← [cy0 .. cy1]
                    , Just lc ← [lookupChunk (ChunkCoord cx cy) tileData]
                    , i ← fcdInstances (lcFlora lc)
                    , Just sp ← [lookupSpecies (fiSpecies i) cat]
                    , Just fh ← [fsHarvest sp]
                    , tag `elem` fhTags fh
                    , let lx = fromIntegral (fiTileX i)
                          ly = fromIntegral (fiTileY i)
                          (tgx, tgy) = chunkToGlobal (ChunkCoord cx cy) lx ly
                    , tgx ≥ xLo, tgx ≤ xHi, tgy ≥ yLo, tgy ≤ yHi
                    , HM.lookupDefault 0 (tgx, tgy) harvests ≤ 0
                    , let z = lcSurfaceMap lc VU.! columnIndex lx ly
                    ]
            atomicModifyIORef' (wsChopDesignationsRef worldState) $ \m →
                (foldl' (\acc (k, v) → HM.insert k v acc) m entries, ())
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { chopAnchor = Nothing }, ())
            logDebug logger CatWorld $
                "Chop designation: +" <> T.pack (show (length entries))
                <> " trees (" <> T.pack (show xLo) <> ","
                <> T.pack (show yLo) <> ")–(" <> T.pack (show xHi)
                <> "," <> T.pack (show yHi) <> ")"
            -- F4 (#646): requested is the FULL swept-rectangle tile
            -- count, matching till/mine and the naive player's own
            -- mental model ("I dragged a 5x5 box") — NOT the count of
            -- flora instances found, which undercounts every empty
            -- non-flora tile as if it had never been requested at all
            -- (review round 1: a 5x5 sweep with one tree must report
            -- 25/1/24 partial, not 1/1/0 accepted).
            recordDesignationOutcome env "chop.designate"
                "no harvestable target for the requested tag in the swept rectangle"
                xLo yLo ((xHi - xLo + 1) * (yHi - yLo + 1)) (length entries)

handleWorldCancelChopCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldCancelChopCommand env _logger pageId gx gy = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsChopDesignationsRef worldState) $ \m →
                (HM.delete (gx, gy) m, ())
        Nothing → pure ()

handleWorldSetChopDesignateTextureCommand ∷ EngineEnv → LoggerState
    → WorldPageId → TextureHandle → IO ()
handleWorldSetChopDesignateTextureCommand env _logger pageId tid = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { chopDesignTexture = Just tid }, ())
        Nothing → pure ()
