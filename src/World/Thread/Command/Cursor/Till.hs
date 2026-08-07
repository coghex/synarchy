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
import World.Generate (globalToChunk)
import World.Till.Types (newTillDesignation)
import World.Vegetation (isTilledSoil)
import World.Thread.Command.Cursor.Common
    ( maxDesignateSide, canonicalDesignationTile
    , recordDesignationOutcome, recordMissingWorldOutcome )

handleWorldSetTillAnchorCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldSetTillAnchorCommand env _logger pageId gx gy = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            -- Normalise into the stored frame so the anchor, the live
            -- preview and the commit all name the same tile (#1135).
            (cgx, cgy) ← canonicalDesignationTile worldState gx gy
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { tillAnchor = Just (cgx, cgy) }, ())
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
handleWorldDesignateTillCommand env logger pageId rgx1 rgy1 rgx2 rgy2 = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Nothing → recordMissingWorldOutcome env "till.designate" pageId rgx1 rgy1
        Just worldState → do
            -- Canonicalise the commit's own corners too (#1135):
            -- these arrive straight from Lua, not from the stored
            -- anchor, so a caller that bypassed pickTile could otherwise
            -- designate in a u-seam alias frame — missing its loaded
            -- chunk while the preview (already canonical) showed the
            -- real tiles. Identity for coords already canonical.
            (gx1, gy1) ← canonicalDesignationTile worldState rgx1 rgy1
            (gx2, gy2) ← canonicalDesignationTile worldState rgx2 rgy2
            tileData ← readIORef (wsTilesRef worldState)
            let tillableAt gx gy = do
                    let (coord, (lx, ly)) = globalToChunk gx gy
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
                xLo = min gx1 gx2
                yLo = min gy1 gy2
                xHi = min (max gx1 gx2) (xLo + maxDesignateSide - 1)
                yHi = min (max gy1 gy2) (yLo + maxDesignateSide - 1)
                entries = case tillableAt gx1 gy1 of
                    Nothing → []   -- anchor unloaded or untillable: nothing
                    Just anchorZ →
                        [ ((gx, gy), newTillDesignation z)
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
        Just worldState →
            atomicModifyIORef' (wsTillDesignationsRef worldState) $ \m →
                (HM.delete (gx, gy) m, ())
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
