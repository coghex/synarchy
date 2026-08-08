-- | Till designation tool (#333). Mirrors the mine/construct designation
--   tools' anchor→rectangle commit: per-z-level like mining (a farmed
--   field is flat ground), but the commit ALSO filters out tiles that
--   can't be tilled — fluid on top (no floor to till), an existing
--   flora instance (can't till under a standing plant/tree), or
--   already-tilled ground (idempotent re-sweep). The till AI
--   (scripts/unit_ai.lua) is the consumer; completion goes through the
--   generic world.setVegAt primitive, not a till-specific one (mirrors
--   how chop completion calls world.harvestFlora then
--   chop.cancelDesignation). Split out of
--   "World.Thread.Command.Cursor" (issue #564).
module World.Thread.Command.Cursor.Till
    ( handleWorldSetTillAnchorCommand
    , handleWorldClearTillAnchorCommand
    , handleWorldDesignateTillCommand
    , handleWorldCancelTillCommand
    , handleWorldSetTillDesignateTextureCommand
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
import World.Till.Types (newTillDesignation)
import World.Vegetation (isTilledSoil)
import World.Thread.Command.Cursor.Common
    (designateRect, recordDesignationOutcome, recordMissingWorldOutcome)

handleWorldSetTillAnchorCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldSetTillAnchorCommand env _logger pageId gx gy = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            -- #1175: canonical anchor, rectangle formed in its frame.
            worldSize ← pageWrapWorldSize worldState
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { tillAnchor = Just (canonicalTile worldSize gx gy) }, ())
        Nothing → pure ()

handleWorldClearTillAnchorCommand ∷ EngineEnv → LoggerState → WorldPageId
    → IO ()
handleWorldClearTillAnchorCommand env _logger pageId = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { tillAnchor = Nothing }, ())
        Nothing → pure ()

-- | Commit a till designation. Per-z-level like mining: only tiles at
--   the anchor's surface z are taken, further filtered to tillable
--   ground (no fluid, no flora, not already tilled). Unloaded-chunk
--   tiles are skipped. Clears the anchor afterwards.
handleWorldDesignateTillCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Int → Int → IO ()
handleWorldDesignateTillCommand env logger pageId gx1 gy1 gx2 gy2 = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Nothing → recordMissingWorldOutcome env "till.designate" pageId gx1 gy1
        Just worldState → do
            tileData ← readIORef (wsTilesRef worldState)
            worldSize ← pageWrapWorldSize worldState
            -- #1175: canonicalised column read, so an anchor-local alias
            -- resolves the chunk that stores the tile. Identity inland.
            let tillableAt gx gy = do
                    let (coord, (lx, ly), _) = canonicalTileFrame worldSize gx gy
                    lc ← lookupChunk coord tileData
                    let idx = columnIndex lx ly
                        z     = lcSurfaceMap lc VU.! idx
                        fluid = lcFluidMap lc V.! idx
                        hasFlora = any (\fi → fromIntegral (fiTileX fi) ≡ lx
                                            ∧ fromIntegral (fiTileY fi) ≡ ly)
                                       (fcdInstances (lcFlora lc))
                        col = lcTiles lc V.! idx
                        i   = z - ctStartZ col
                        alreadyTilled = i ≥ 0 ∧ i < VU.length (ctVeg col)
                                      ∧ isTilledSoil (ctVeg col VU.! i)
                    if isNothing fluid ∧ not hasFlora ∧ not alreadyTilled
                        then Just z
                        else Nothing
                ((xLo, yLo), (xHi, yHi)) =
                    designateRect worldSize (gx1, gy1) (gx2, gy2)
                entries = case tillableAt gx1 gy1 of
                    Nothing → []   -- anchor unloaded or untillable: nothing
                    Just anchorZ →
                        [ (canonicalTile worldSize gx gy, newTillDesignation z)
                        | gx ← [xLo .. xHi]
                        , gy ← [yLo .. yHi]
                        , Just z ← [tillableAt gx gy]
                        , z ≡ anchorZ
                        ]
            atomicModifyIORef' (wsTillDesignationsRef worldState) $ \m →
                (foldl' (\acc (k, v) → HM.insert k v acc) m entries, ())
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { tillAnchor = Nothing }, ())
            logDebug logger CatWorld $
                "Till designation: +" <> T.pack (show (length entries))
                <> " tiles (" <> T.pack (show xLo) <> ","
                <> T.pack (show yLo) <> ")–(" <> T.pack (show xHi)
                <> "," <> T.pack (show yHi) <> ")"
            recordDesignationOutcome env "till.designate"
                "anchor tile ineligible or unloaded" xLo yLo
                ((xHi - xLo + 1) * (yHi - yLo + 1)) (length entries)

handleWorldCancelTillCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldCancelTillCommand env _logger pageId gx gy = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            -- #1175: cancellation accepts any alias of the stored key.
            worldSize ← pageWrapWorldSize worldState
            atomicModifyIORef' (wsTillDesignationsRef worldState) $ \m →
                (HM.delete (canonicalTile worldSize gx gy) m, ())
        Nothing → pure ()

handleWorldSetTillDesignateTextureCommand ∷ EngineEnv → LoggerState
    → WorldPageId → TextureHandle → IO ()
handleWorldSetTillDesignateTextureCommand env _logger pageId tid = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { tillDesignTexture = Just tid }, ())
        Nothing → pure ()
