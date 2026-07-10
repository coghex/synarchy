{-# LANGUAGE UnicodeSyntax #-}

-- | Plant designation tool (#335). Single-tile, no anchor: the planting
--   screen already scopes the player to one tile before a crop is
--   chosen, so there is no pending rectangle to preview (unlike
--   mine/construct/chop/till). The commit validates both halves of
--   "can this be planted here" — the tile is tilled soil (the same
--   'isTilledSoil' check world.isPlantable uses) and the given crop
--   name resolves to a REGISTERED plantable-crop species (row_crop or
--   groundcover_crop worldGen category) — before recording the
--   designation. The farm AI (scripts/unit_ai.lua, #336) is the
--   eventual consumer. Split out of "World.Thread.Command.Cursor"
--   (issue #564).
module World.Thread.Command.Cursor.Plant
    ( handleWorldDesignatePlantCommand
    , handleWorldCancelPlantCommand
    , handleWorldSetPlantDesignateTextureCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Text as T
import World.Types
import World.Generate (globalToChunk)
import World.Plant.Types (newPlantDesignation)
import World.Vegetation (isTilledSoil)
import Engine.ActionOutcome (ActionOutcome(..), pushActionOutcome)

-- | Commit a plant designation at (gx, gy) for the named crop. Refused
--   (silently — the caller polls plant.getDesignationAt to confirm) if
--   the chunk isn't loaded, the tile isn't tilled soil, the tile is
--   already occupied (an existing flora instance or crop plot — #336's
--   plantCropAt/plantRowCropAt both refuse to plant over one, and
--   world.isPlantable is tilled-soil-only so it can't tell the
--   difference on its own; excluding an occupied tile HERE, same as
--   till's own designation excludes flora-carrying tiles, keeps a farm
--   AI from spending a full walk-and-work cycle on a designation that
--   was always going to fail), or cropName doesn't name a registered
--   plantable-crop species.
handleWorldDesignatePlantCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Text → IO ()
handleWorldDesignatePlantCommand env logger pageId gx gy cropName = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing → pure ()
        Just worldState → do
            tileData ← readIORef (wsTilesRef worldState)
            cat ← readIORef (floraCatalogRef env)
            plots ← readIORef (wsCropPlotsRef worldState)
            let (coord, (lx, ly)) = globalToChunk gx gy
                idx = columnIndex lx ly
                hasExistingPlot = HM.member (gx, gy) plots
                resolvedCrop = case findSpeciesByName cropName cat of
                    Just (fid, _sp)
                        | Just wg ← HM.lookup (unFloraId fid) (fcWorldGen cat)
                        , isPlantableCropCategory (fwCategory wg) → Just fid
                    _ → Nothing
                tileZ = do
                    lc ← lookupChunk coord tileData
                    let z   = lcSurfaceMap lc VU.! idx
                        col = lcTiles lc V.! idx
                        i   = z - ctStartZ col
                        vg  = if i ≥ 0 ∧ i < VU.length (ctVeg col)
                              then ctVeg col VU.! i else 0
                        hasExistingFlora = any
                            (\fi → fromIntegral (fiTileX fi) ≡ lx
                                 ∧ fromIntegral (fiTileY fi) ≡ ly)
                            (fcdInstances (lcFlora lc))
                    if isTilledSoil vg ∧ not hasExistingFlora
                       ∧ not hasExistingPlot
                    then Just z else Nothing
            gt ← readIORef (gameTimeRef env)
            case (tileZ, resolvedCrop) of
                (Just z, Just fid) → do
                    atomicModifyIORef' (wsPlantDesignationsRef worldState) $
                        \m → (HM.insert (gx, gy) (newPlantDesignation z fid) m, ())
                    logDebug logger CatWorld $
                        "Plant designation: (" <> T.pack (show gx) <> ","
                        <> T.pack (show gy) <> ") crop=" <> cropName
                    pushActionOutcome (actionOutcomeRef env) ActionOutcome
                        { aoTs = gt, aoKind = "plant.designate"
                        , aoOutcome = "accepted"
                        , aoWhereX = Just gx, aoWhereY = Just gy, aoTarget = Nothing
                        , aoRequested = Nothing, aoApplied = Nothing, aoDropped = Nothing
                        , aoReason = Nothing, aoHandler = Nothing
                        }
                _ → do
                    logDebug logger CatWorld $
                        "Plant designation refused at (" <> T.pack (show gx)
                        <> "," <> T.pack (show gy) <> ") crop=" <> cropName
                    let reason
                            | isNothing resolvedCrop =
                                "unknown or non-plantable crop: " <> cropName
                            | otherwise =
                                "tile not tilled soil, or already occupied"
                    pushActionOutcome (actionOutcomeRef env) ActionOutcome
                        { aoTs = gt, aoKind = "plant.designate"
                        , aoOutcome = "rejected"
                        , aoWhereX = Just gx, aoWhereY = Just gy, aoTarget = Nothing
                        , aoRequested = Nothing, aoApplied = Nothing, aoDropped = Nothing
                        , aoReason = Just reason, aoHandler = Nothing
                        }

handleWorldCancelPlantCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldCancelPlantCommand env _logger pageId gx gy = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsPlantDesignationsRef worldState) $ \m →
                (HM.delete (gx, gy) m, ())
        Nothing → pure ()

handleWorldSetPlantDesignateTextureCommand ∷ EngineEnv → LoggerState
    → WorldPageId → TextureHandle → IO ()
handleWorldSetPlantDesignateTextureCommand env _logger pageId tid = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { plantDesignTexture = Just tid }, ())
        Nothing → pure ()
