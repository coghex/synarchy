{-# LANGUAGE Strict #-}

-- | Vegetation edit handlers: tile ground-cover id (tilling) and
--   row-crop planting. Split out of "World.Thread.Command.Edit"
--   (issue #563).
module World.Thread.Command.Edit.Vegetation
    ( handleWorldSetVegCommand
    , handleWorldPlantRowCropAtCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import Engine.Core.Log (logDebug, logWarn, LogCategory(..), LoggerState)
import World.Types
import World.Generate.Coordinates (canonicalTileFrame)
import World.Edit.Types (WorldEdit(..), appendEdit)
import World.Edit.Apply (applyEdit)
import World.Vegetation (isTilledSoil)
import World.Plant.Validate (revalidatePlantDesignations)

-- | Set the vegetation id of an existing tile at (gx,gy,z) via the
--   WeSetVeg edit path. Mirrors handleWorldSetSlopeCommand exactly —
--   the till AI's (#333) completion primitive: flips a tilled tile's
--   ground cover to 'World.Vegetation.vegTilledSoil' so it survives
--   chunk eviction + save/load like every other edit.
handleWorldSetVegCommand ∷ WorldSimCapability → LoggerState → WorldPageId
    → Int → Int → Int → Word8 → IO ()
handleWorldSetVegCommand wsc logger pageId rawGX rawGY z vegId = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for set veg: " <> unWorldPageId pageId
        Just ws → do
            -- #1175: the till AI reaches here with a designation job coord,
            -- which a pre-#1175 save can hold as a u-alias. Canonicalise so
            -- the edit lands in the chunk that stores the tile (and is
            -- logged under the key a replay will match). Identity inland.
            worldSize ← pageWrapWorldSize ws
            let (coord, (lx, ly), (dgx, dgy)) =
                    canonicalTileFrame worldSize rawGX rawGY
                gx = rawGX + dgx
                gy = rawGY + dgy
                idx  = columnIndex lx ly
                edit = WeSetVeg gx gy z vegId
            td ← readIORef (wsTilesRef ws)
            case lookupChunk coord td of
                Nothing →
                    logWarn logger CatWorld $
                        "Chunk not loaded for set veg at "
                          <> tshow gx <> "," <> tshow gy
                Just lc → do
                    let col = lcTiles lc V.! idx
                        i   = z - ctStartZ col
                    if i < 0 ∨ i ≥ VU.length (ctVeg col)
                      then logWarn logger CatWorld $
                             "Set veg z out of column range at "
                               <> tshow gx <> "," <> tshow gy
                               <> " z=" <> tshow z
                      else do
                        let lc' = applyEdit edit lc
                        atomicModifyIORef' (wsTilesRef ws) $ \w →
                            (insertChunk lc' w, ())
                        atomicModifyIORef' (wsEditsRef ws) $ \es →
                            (appendEdit coord edit es, ())
                        bumpQuadCacheGen ws
                        writeIORef (wsZoomQuadCacheRef ws) Nothing
                        writeIORef (wsBgQuadCacheRef ws)   Nothing
                        -- #1858: the direct vegetation trigger. Tilling
                        -- writes tilled soil through here, and anything
                        -- writing a different cover over a designated
                        -- tile is exactly the case D-14 makes continuous.
                        _ ← revalidatePlantDesignations logger ws
                        logDebug logger CatWorld $
                            "Set veg at " <> tshow gx <> ","
                              <> tshow gy <> " z=" <> tshow z
                              <> " vegId=" <> tshow vegId

-- | Plant a single row-crop FloraInstance at (gx, gy) via the
--   WePlaceFlora edit path — the farm AI's (#336) row-crop planting
--   completion, the FloraInstance counterpart to world.plantCropAt's
--   CropPlot for groundcover crops. Refused (a warn, no mutation) unless
--   the chunk is loaded, the tile is tilled soil, and cropName names a
--   registered row_crop species — mirrors handleWorldDesignatePlantCommand's
--   validation.
handleWorldPlantRowCropAtCommand ∷ WorldSimCapability → LoggerState → WorldPageId
    → Int → Int → Text → IO ()
handleWorldPlantRowCropAtCommand wsc logger pageId rawGX rawGY cropName = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for plant row crop: " <> unWorldPageId pageId
        Just ws → do
            -- #1175: same as setVeg above — the farm AI's completion coord
            -- comes from a plant designation and may be a legacy alias.
            worldSize ← pageWrapWorldSize ws
            let (coord, (lx, ly), (dgx, dgy)) =
                    canonicalTileFrame worldSize rawGX rawGY
                gx = rawGX + dgx
                gy = rawGY + dgy
                idx  = columnIndex lx ly
            td ← readIORef (wsTilesRef ws)
            cat ← readIORef (wsFloraCatalogRef wsc)
            let resolved = do
                    (fid, _sp) ← findSpeciesByName cropName cat
                    wg ← HM.lookup (unFloraId fid) (fcWorldGen cat)
                    if fwCategory wg ≡ "row_crop"
                        then Just (fid, fwFootprint wg) else Nothing
            case lookupChunk coord td of
                Nothing →
                    logWarn logger CatWorld $
                        "Chunk not loaded for plant row crop at "
                          <> tshow gx <> "," <> tshow gy
                Just lc → do
                    let col = lcTiles lc V.! idx
                        z   = lcSurfaceMap lc VU.! idx
                        i   = z - ctStartZ col
                        vg  = if i ≥ 0 ∧ i < VU.length (ctVeg col)
                              then ctVeg col VU.! i else 0
                        -- A tile already carrying a flora instance
                        -- (a prior plantRowCropAt, or in principle any
                        -- other rooted flora) must refuse a second
                        -- planting — otherwise repeated calls (a
                        -- re-designate + re-plant, or the primitive
                        -- called twice) stack overlapping FloraInstances
                        -- that all persist via the edit log.
                        hasExistingFlora = any
                            (\fi → fromIntegral (fiTileX fi) ≡ lx
                                 ∧ fromIntegral (fiTileY fi) ≡ ly)
                            (fcdInstances (lcFlora lc))
                    plots ← readIORef (wsCropPlotsRef ws)
                    let hasExistingPlot = HM.member (gx, gy) plots
                    case resolved of
                        Nothing →
                            logWarn logger CatWorld $
                                "Plant row crop refused (unknown/non-row-crop"
                                  <> " species) at " <> tshow gx <> ","
                                  <> tshow gy <> " crop=" <> cropName
                        Just _ | not (isTilledSoil vg) →
                            logWarn logger CatWorld $
                                "Plant row crop refused (not tilled soil) at "
                                  <> tshow gx <> "," <> tshow gy
                        Just _ | hasExistingFlora ∨ hasExistingPlot →
                            logWarn logger CatWorld $
                                "Plant row crop refused (tile already "
                                  <> "planted) at " <> tshow gx <> ","
                                  <> tshow gy
                        Just (fid, baseWidth) → do
                            paramsM ← readIORef (wsGenParamsRef ws)
                            date ← readIORef (wsDateRef ws)
                            let calendar = maybe defaultCalendarConfig
                                               wgpCalender paramsM
                                absDay = worldAbsoluteDay calendar date
                                edit = WePlaceFlora gx gy fid absDay baseWidth
                                lc'  = applyEdit edit lc
                            atomicModifyIORef' (wsTilesRef ws) $ \w →
                                (insertChunk lc' w, ())
                            atomicModifyIORef' (wsEditsRef ws) $ \es →
                                (appendEdit coord edit es, ())
                            bumpQuadCacheGen ws
                            writeIORef (wsZoomQuadCacheRef ws) Nothing
                            writeIORef (wsBgQuadCacheRef ws)   Nothing
                            logDebug logger CatWorld $
                                "Planted row crop " <> cropName <> " at "
                                  <> tshow gx <> ","
                                  <> tshow gy
