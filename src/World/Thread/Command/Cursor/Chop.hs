-- | Chop designation tool (#97). Mirrors the mine/construct designation
--   tools' anchor→rectangle commit, but the commit filters to FLORA:
--   only currently-harvestable PLANTS carrying the requested harvest tag
--   ("wood") are designated. No per-z-level filter — forests span
--   slopes, and the designation z is only the marker's render height.
--
--   #1854: one designation per matching PLANT, not per tile. A sweep
--   over a tile holding two wood-tagged trees marks both, each
--   addressable on its own, and each one's regrowth skip is read against
--   its own timer. The chop AI (scripts/unit_ai.lua) is the
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
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv)
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import qualified Data.Vector.Unboxed as VU
import World.Types
import World.Generate (chunkToGlobal, globalToChunk)
import World.Generate.Coordinates (canonicalTile)
import World.Flora.Designation
    (designateChopInstances, cancelChopAtTile, cancelChopForInstance)
import World.Thread.Command.Cursor.Common
    (designateRect, recordDesignationOutcome, recordMissingWorldOutcome)

handleWorldSetChopAnchorCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldSetChopAnchorCommand env _logger pageId gx gy = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            -- #1175: canonical anchor, rectangle formed in its frame.
            worldSize ← pageWrapWorldSize worldState
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { chopAnchor = Just (canonicalTile worldSize gx gy) }, ())
        Nothing → pure ()

handleWorldClearChopAnchorCommand ∷ EngineEnv → LoggerState → WorldPageId
    → IO ()
handleWorldClearChopAnchorCommand env _logger pageId = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
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
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Nothing → recordMissingWorldOutcome env "chop.designate" pageId gx1 gy1
        Just worldState → do
            tileData ← readIORef (wsTilesRef worldState)
            cat ← readIORef (wsFloraCatalogRef (toWorldSimCapability env))
            harvests ← readIORef (wsFloraHarvestsRef worldState)
            worldSize ← pageWrapWorldSize worldState
            let ((xLo, yLo), (xHi, yHi)) =
                    designateRect worldSize (gx1, gy1) (gx2, gy2)
                (cLo, _) = globalToChunk xLo yLo
                (cHi, _) = globalToChunk xHi yHi
                ChunkCoord cx0 cy0 = cLo
                ChunkCoord cx1 cy1 = cHi
                -- Walk the overlapped chunks' flora instances rather
                -- than probing every rect tile — a designation sweep is
                -- mostly empty ground.
                --
                -- #1175: the rectangle is in the ANCHOR's alias frame, so
                -- the chunks it spans are too. Each is canonicalised to
                -- find the chunk that stores it, while the tile coord
                -- stays in the rectangle's own frame (so the bounds test
                -- means what it says) and is canonicalised again for the
                -- harvest read and the stored key. Identity inland.
                --
                -- #1854: one entry per matching PLANT, not per tile. Two
                -- wood-tagged trees sharing a tile are two designations
                -- now, each addressable on its own — and the regrowth
                -- skip is read against that plant's own timer, so a
                -- regrowing stump beside a standing tree no longer
                -- suppresses the tree.
                entries =
                    [ (fiInstanceId i, tgx, tgy, z)
                    | cx ← [cx0 .. cx1], cy ← [cy0 .. cy1]
                    , let rawCoord = ChunkCoord cx cy
                    , Just lc ← [lookupChunk (wrapChunkCoordU worldSize rawCoord)
                                             tileData]
                    , i ← fcdInstances (lcFlora lc)
                    , Just sp ← [lookupSpecies (fiSpecies i) cat]
                    , Just fh ← [fsHarvest sp]
                    , tag `elem` fhTags fh
                    , let lx = fromIntegral (fiTileX i)
                          ly = fromIntegral (fiTileY i)
                          (tgx, tgy) = chunkToGlobal rawCoord lx ly
                    , tgx ≥ xLo, tgx ≤ xHi, tgy ≥ yLo, tgy ≤ yHi
                    , HM.lookupDefault 0 (fiInstanceId i) harvests ≤ 0
                    , let z = lcSurfaceMap lc VU.! columnIndex lx ly
                    ]
            -- The ONE owning write (#1854 requirement 8): the durable
            -- map and every loaded instance's fiChopDesignated mirror
            -- move together, so they cannot drift.
            designateChopInstances worldState entries
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { chopAnchor = Nothing }, ())
            logDebug logger CatWorld $
                "Chop designation: +" <> tshow (length entries)
                <> " trees (" <> tshow xLo <> ","
                <> tshow yLo <> ")–(" <> tshow xHi
                <> "," <> tshow yHi <> ")"
            -- F4 (#646): requested is the FULL swept-rectangle tile
            -- count, matching till/mine and the naive player's own
            -- mental model ("I dragged a 5x5 box") — NOT the count of
            -- flora instances found, which undercounts every empty
            -- non-flora tile as if it had never been requested at all
            -- (a 5x5 sweep with one tree must report
            -- 25/1/24 partial, not 1/1/0 accepted).
            recordDesignationOutcome env "chop.designate"
                "no harvestable target for the requested tag in the swept rectangle"
                xLo yLo ((xHi - xLo + 1) * (yHi - yLo + 1)) (length entries)

handleWorldCancelChopCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Maybe FloraInstanceId → IO ()
handleWorldCancelChopCommand env _logger pageId gx gy mIid = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState → case mIid of
            -- #1854: the felling acolyte cancels EXACTLY the plant it
            -- claimed, so a second designated tree on the same tile
            -- stays designated for whoever claims it next.
            Just iid → cancelChopForInstance worldState iid
            -- The player's cancel gesture still points at a TILE, so it
            -- clears every designation standing there — including a
            -- pending legacy entry that has not resolved yet, which
            -- would otherwise come back the moment its chunk loaded.
            Nothing → cancelChopAtTile worldState gx gy
        Nothing → pure ()

handleWorldSetChopDesignateTextureCommand ∷ EngineEnv → LoggerState
    → WorldPageId → TextureHandle → IO ()
handleWorldSetChopDesignateTextureCommand env _logger pageId tid = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { chopDesignTexture = Just tid }, ())
        Nothing → pure ()
