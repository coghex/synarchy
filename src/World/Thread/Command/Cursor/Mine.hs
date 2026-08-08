-- | Mine designation tool. Split out of
--   "World.Thread.Command.Cursor" (issue #564).
module World.Thread.Command.Cursor.Mine
    ( handleWorldSetMineAnchorCommand
    , handleWorldClearMineAnchorCommand
    , handleWorldDesignateMineCommand
    , handleWorldSetMineDesignateTextureCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv)
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import qualified Data.Vector.Unboxed as VU
import World.Types
import World.Generate.Coordinates (canonicalTile, canonicalTileFrame)
import World.Mine.Types (designationFromSlope)
import World.Thread.Command.Cursor.Common
    (designateRect, recordDesignationOutcome, recordMissingWorldOutcome)

handleWorldSetMineAnchorCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldSetMineAnchorCommand env _logger pageId gx gy = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            -- #1175: an anchor is a stored coord like any other, so it is
            -- canonicalised on the way in. The rectangle is then formed in
            -- ITS frame ('designateRect'), which is what keeps a
            -- seam-crossing drag the small rectangle the player drew.
            worldSize ← pageWrapWorldSize worldState
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { mineAnchor = Just (canonicalTile worldSize gx gy) }, ())
        Nothing → pure ()

handleWorldClearMineAnchorCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldClearMineAnchorCommand env _logger pageId = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { mineAnchor = Nothing }, ())
        Nothing → pure ()

-- | Commit a designation rectangle. DF-style: designations are
--   PER-Z-LEVEL — (gx1, gy1) is the anchor corner, and only tiles
--   whose surface z equals the anchor tile's surface z are taken, so
--   a sweep across a slope marks just the anchor's level. Entries
--   store that z (markers render from it, no per-frame column reads).
--   Unloaded-chunk tiles are skipped — designate what you can see.
--   Also clears the anchor so the Lua side can't desync from a
--   dropped clear command.
handleWorldDesignateMineCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Int → Int → IO ()
handleWorldDesignateMineCommand env logger pageId gx1 gy1 gx2 gy2 = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Nothing → recordMissingWorldOutcome env "world.designateMine" pageId gx1 gy1
        Just worldState → do
            tileData ← readIORef (wsTilesRef worldState)
            worldSize ← pageWrapWorldSize worldState
            -- #1175: every column read canonicalises its coord, so an
            -- anchor-local alias resolves the chunk that actually stores
            -- the tile instead of missing at the seam. Identity inland.
            let surfaceZAt gx gy = do
                    let (coord, (lx, ly), _) = canonicalTileFrame worldSize gx gy
                    lc ← lookupChunk coord tileData
                    pure (lcSurfaceMap lc VU.! columnIndex lx ly)
                -- Gen-time slope at the tile's surface: tiles that are
                -- already sloped start with the lowered corners pre-dug
                -- ('designationFromSlope'), so the designation's volume
                -- matches the material that's actually there.
                slopeAt gx gy z =
                    let (coord, (lx, ly), _) = canonicalTileFrame worldSize gx gy
                    in case lookupChunk coord tileData of
                        Nothing → 0
                        Just lc →
                            let col = lcTiles lc V.! columnIndex lx ly
                                i = z - ctStartZ col
                            in if i ≥ 0 ∧ i < VU.length (ctSlopes col)
                               then ctSlopes col VU.! i
                               else 0
                ((xLo, yLo), (xHi, yHi)) =
                    designateRect worldSize (gx1, gy1) (gx2, gy2)
                entries = case surfaceZAt gx1 gy1 of
                    Nothing → []   -- anchor chunk unloaded: nothing
                    Just anchorZ →
                        [ ( canonicalTile worldSize gx gy
                          , designationFromSlope z (slopeAt gx gy z) )
                        | gx ← [xLo .. xHi]
                        , gy ← [yLo .. yHi]
                        , Just z ← [surfaceZAt gx gy]
                        , z ≡ anchorZ
                        ]
            atomicModifyIORef' (wsMineDesignationsRef worldState) $ \m →
                (foldl' (\acc (k, v) → HM.insert k v acc) m entries, ())
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { mineAnchor = Nothing }, ())
            logDebug logger CatWorld $
                "Mine designation: +" <> T.pack (show (length entries))
                <> " tiles (" <> T.pack (show xLo) <> ","
                <> T.pack (show yLo) <> ")–(" <> T.pack (show xHi)
                <> "," <> T.pack (show yHi) <> ")"
            recordDesignationOutcome env "world.designateMine"
                "anchor tile ineligible or unloaded" xLo yLo
                ((xHi - xLo + 1) * (yHi - yLo + 1)) (length entries)

handleWorldSetMineDesignateTextureCommand ∷ EngineEnv → LoggerState
    → WorldPageId → TextureHandle → IO ()
handleWorldSetMineDesignateTextureCommand env _logger pageId tid = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { mineDesignTexture = Just tid }, ())
        Nothing → pure ()
