{-# LANGUAGE Strict #-}
-- | Groundcover crop-plot planting verb (#334). See
--   Engine.Scripting.Lua.API.Forage.Query for the read-only
--   world.getCropPlotAt counterpart and .Harvest for reaping one.
module Engine.Scripting.Lua.API.Forage.Crop
    ( worldPlantCropAtFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import Engine.Core.State (activeWorldStateFrom)
import World.Types
import World.Vegetation (isTilledSoil)
import World.Flora.CropPlot (newCropPlot)
import World.Generate.Coordinates (canonicalTileFrame)
import World.Flora.Clock (growthClock)

-- | The one YAML worldGen category tag (World.Flora.Placement) that
--   marks a species as the ctVeg-tile-fill groundcover form (#334) —
--   the only form world.plantCropAt (a CropPlot) is valid for. A
--   row_crop species is an ordinary FloraInstance placed by the
--   worldgen pipeline; planting one as a CropPlot would render it as a
--   tile-fill (World.Render.Quads), the WRONG form for its species.
groundcoverCropCategory ∷ Text
groundcoverCropCategory = "groundcover_crop"

-- | world.plantCropAt(gx, gy, speciesName) → true | nil
--
--   Foundation-level planting primitive (#334): plants a groundcover
--   crop plot at (gx, gy) at full health, dated to the CURRENT world
--   day (the #332 runtime's age-0 baseline for this plot — see
--   World.Flora.CropPlot). Refuses (nil) unless the tile is plantable
--   (tilled soil, #333 — same gate as world.isPlantable), speciesName
--   names a REGISTERED GROUNDCOVER species (worldGen category
--   "groundcover_crop" — a row_crop species, e.g. tomato_plant, is
--   refused: it's an ordinary FloraInstance, not a CropPlot, see
--   groundcoverCropCategory), and the tile's chunk is loaded.
--
--   This is a low-level verb with no player-facing tool/AI/UI yet —
--   #335 (planting tool + suitability) and #336 (farm AI) build the
--   real planting flow on top of it, the same foundation-first shape
--   as #300's unit.repairItem preceding the repair AI/UI. A planted
--   plot renders as the tile's veg-fill (World.Render.Quads), NOT a
--   floating sprite.
worldPlantCropAtFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldPlantCropAtFn wsc = do
    mGx ← Lua.tointeger 1
    mGy ← Lua.tointeger 2
    mName ← Lua.tostring 3
    case (mGx, mGy, mName) of
        (Just gx', Just gy', Just nameBS) → do
            let rawGX = fromIntegral gx'
                rawGY = fromIntegral gy'
                name = TE.decodeUtf8Lenient nameBS
            ok ← Lua.liftIO $ do
                mWs ← activeWorldStateFrom (wsWorldManagerRef wsc)
                case mWs of
                    Nothing → pure False
                    Just ws → do
                        tileData ← readIORef (wsTilesRef ws)
                        -- #1175: the farm AI plants at a plant-designation
                        -- coord, which a pre-#1175 save can hold as a
                        -- u-alias. Resolve the stored frame so the column
                        -- read and the crop-plot key agree with everything
                        -- else keyed by tile. Identity inland.
                        worldSize ← pageWrapWorldSize ws
                        let (coord, (lx, ly), (dgx, dgy)) =
                                canonicalTileFrame worldSize rawGX rawGY
                            gx = rawGX + dgx
                            gy = rawGY + dgy
                            idx = ly * chunkSize + lx
                        case lookupChunk coord tileData of
                            Nothing → pure False
                            Just lc → do
                                let col = lcTiles lc V.! idx
                                    z    = lcSurfaceMap lc VU.! idx
                                    i    = z - ctStartZ col
                                    vg   = if i ≥ 0 ∧ i < VU.length (ctVeg col)
                                           then ctVeg col VU.! i else 0
                                    -- A tile already carrying a flora
                                    -- instance (e.g. a row crop planted
                                    -- via world.plantRowCropAt) must
                                    -- refuse — otherwise a groundcover
                                    -- plot lands underneath/on top of it
                                    -- (world.isPlantable is tilled-soil
                                    -- only, so it stays "plantable" after
                                    -- either form is already planted).
                                    hasExistingFlora = any
                                        (\fi → fromIntegral (fiTileX fi) ≡ lx
                                             ∧ fromIntegral (fiTileY fi) ≡ ly)
                                        (fcdInstances (lcFlora lc))
                                plots ← readIORef (wsCropPlotsRef ws)
                                let hasExistingPlot = HM.member (gx, gy) plots
                                if not (isTilledSoil vg)
                                   ∨ hasExistingFlora ∨ hasExistingPlot
                                then pure False
                                else do
                                    cat ← readIORef (wsFloraCatalogRef wsc)
                                    case findSpeciesByName name cat of
                                        Nothing → pure False
                                        Just (fid, _) →
                                            case HM.lookup (unFloraId fid)
                                                     (fcWorldGen cat) of
                                                Just wg
                                                    | fwCategory wg
                                                        ≡ groundcoverCropCategory → do
                                                    (_, absDay) ← growthClock ws
                                                    let plot = newCropPlot fid absDay 1.0
                                                    atomicModifyIORef' (wsCropPlotsRef ws) $
                                                        \ps → (HM.insert (gx, gy) plot ps, ())
                                                    bumpQuadCacheGen ws
                                                    pure True
                                                _ → pure False
            Lua.pushboolean ok
            return 1
        _ → Lua.pushnil >> return 1
