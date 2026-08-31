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
--
--   #1858: the soil half of that commit is no longer the last word on
--   it. Tilled soil is a CONTINUOUS requirement — "World.Plant.Validate"
--   re-resolves it after every write that can move a designated tile's
--   resolved surface and whenever terrain becomes resident, and REMOVES
--   a record whose soil is resident and lost. Both sides share the one
--   'isTilledSoil' predicate deliberately, so admission and
--   invalidation cannot drift; the CROP half stays admission-only, as
--   growing the catalogue must not retroactively cancel a player's
--   designation.
module World.Thread.Command.Cursor.Plant
    ( handleWorldDesignatePlantCommand
    , handleWorldCancelPlantCommand
    , handleWorldSetPlantDesignateTextureCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv, actionOutcomeRef)
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Types
import World.Generate.Coordinates (canonicalTile, canonicalTileFrame)
import World.Plant.Types (newPlantDesignation)
import World.Vegetation (isTilledSoil)
import Engine.ActionOutcome (ActionOutcome(..), pushActionOutcome)
import World.Thread.Command.Cursor.Common (recordMissingWorldOutcome)

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
--
--   The tilled-soil read below is the ADMISSION half of #1858's
--   continuous check; 'World.Plant.Validate.plantSoilState' is the
--   other half and resolves the tile through the same steps — the same
--   canonical frame, the same 'lcSurfaceMap', the same bounded 'ctVeg'
--   read and the same 'isTilledSoil'. Change one and change the other.
handleWorldDesignatePlantCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Text → IO ()
handleWorldDesignatePlantCommand env logger pageId gx gy cropName = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Nothing → recordMissingWorldOutcome env "plant.designate" pageId gx gy
        Just worldState → do
            tileData ← readIORef (wsTilesRef worldState)
            cat ← readIORef (wsFloraCatalogRef (toWorldSimCapability env))
            plots ← readIORef (wsCropPlotsRef worldState)
            worldSize ← pageWrapWorldSize worldState
            -- #1175: a plant designation is a single-tile point op, so
            -- the whole handler works in the canonical frame — the
            -- eligibility reads, the occupancy checks and the stored key
            -- all resolve the same physical tile whichever alias Lua
            -- passed. Identity inland.
            let (coord, (lx, ly), _) = canonicalTileFrame worldSize gx gy
                (cgx, cgy) = canonicalTile worldSize gx gy
                idx = columnIndex lx ly
                hasExistingPlot = HM.member (cgx, cgy) plots
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
            gt ← readIORef (wsGameTimeRef (toWorldSimCapability env))
            case (tileZ, resolvedCrop) of
                (Just z, Just fid) → do
                    atomicModifyIORef' (wsPlantDesignationsRef worldState) $
                        \m → (HM.insert (cgx, cgy) (newPlantDesignation z fid) m, ())
                    logDebug logger CatWorld $
                        "Plant designation: (" <> tshow gx <> ","
                        <> tshow gy <> ") crop=" <> cropName
                    pushActionOutcome (actionOutcomeRef env) ActionOutcome
                        { aoTs = gt, aoKind = "plant.designate"
                        , aoOutcome = "accepted"
                        , aoWhereX = Just (fromIntegral gx), aoWhereY = Just (fromIntegral gy)
                        , aoTarget = Nothing
                        , aoRequested = Nothing, aoApplied = Nothing, aoDropped = Nothing
                        , aoReason = Nothing, aoHandler = Nothing
                        }
                _ → do
                    logDebug logger CatWorld $
                        "Plant designation refused at (" <> tshow gx
                        <> "," <> tshow gy <> ") crop=" <> cropName
                    let reason
                            | isNothing resolvedCrop =
                                "unknown or non-plantable crop: " <> cropName
                            | otherwise =
                                "tile not tilled soil, or already occupied"
                    pushActionOutcome (actionOutcomeRef env) ActionOutcome
                        { aoTs = gt, aoKind = "plant.designate"
                        , aoOutcome = "rejected"
                        , aoWhereX = Just (fromIntegral gx), aoWhereY = Just (fromIntegral gy)
                        , aoTarget = Nothing
                        , aoRequested = Nothing, aoApplied = Nothing, aoDropped = Nothing
                        , aoReason = Just reason, aoHandler = Nothing
                        }

handleWorldCancelPlantCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldCancelPlantCommand env _logger pageId gx gy = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            -- #1175: cancellation accepts any alias of the stored key.
            worldSize ← pageWrapWorldSize worldState
            atomicModifyIORef' (wsPlantDesignationsRef worldState) $ \m →
                (HM.delete (canonicalTile worldSize gx gy) m, ())
        Nothing → pure ()

handleWorldSetPlantDesignateTextureCommand ∷ EngineEnv → LoggerState
    → WorldPageId → TextureHandle → IO ()
handleWorldSetPlantDesignateTextureCommand env _logger pageId tid = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { plantDesignTexture = Just tid }, ())
        Nothing → pure ()
