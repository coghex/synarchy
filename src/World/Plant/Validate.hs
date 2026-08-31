{-# LANGUAGE Strict #-}
-- | Continuous tilled-soil validity for crop plant designations (#1858).
--
--   'World.Thread.Command.Cursor.Plant.handleWorldDesignatePlantCommand'
--   admits a designation only over tilled soil, but before this module
--   nothing re-read the soil afterwards: the record persisted, the
--   marker kept drawing, and the farm AI (#336) kept walking to ground
--   it could no longer plant. D-14 makes tilled soil a CONTINUOUS
--   validity requirement, and is explicit that the renderer must not
--   merely hide invalid state while leaving an invisible job active.
--
--   So validity is world-owned and resolves to THREE states, never two
--   ('PlantSoilState'): an unloaded chunk is UNKNOWN, not proof the
--   soil was lost. Removal is the whole cancellation protocol — the farm
--   AI's @unitAi.plant.utility@ already releases its claim, job, phase
--   and progress the moment @plant.getDesignationAt@ comes back empty
--   (@scripts\/unit_ai_farm.lua@), so the world dropping the record is
--   sufficient and no new AI teardown path exists.
--
--   'plantSoilState' resolves a tile through the SAME steps admission
--   uses — 'canonicalTileFrame', 'lookupChunk', 'lcSurfaceMap', the
--   bounded 'ctVeg' read and 'World.Vegetation.isTilledSoil' — so the
--   two cannot disagree about what "tilled" means. Requirement 3: one
--   predicate, never a raw comparison to a vegetation id.
--
--   'revalidatePlantDesignations' sweeps the WHOLE map rather than a
--   scoped tile set. That is deliberate and is what makes the two
--   trigger families one call: a live edit needs the tiles it touched
--   re-checked, and a chunk publication needs every UNKNOWN record
--   whose terrain just resolved re-checked — and the second set is not
--   derivable from the publication alone once eviction and the seam are
--   in play. The map holds tens of entries and the sweep short-circuits
--   on an empty one, so the superset costs nothing.
module World.Plant.Validate
    ( PlantSoilState(..)
    , plantSoilState
    , prunePlantDesignations
    , revalidatePlantDesignations
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import World.Types
import World.Generate.Coordinates (canonicalTileFrame)
import World.Plant.Types (PlantDesignations)
import World.Vegetation (isTilledSoil)

-- | The tri-state current-surface resolution of one designated tile.
--
--   'PlantSoilUnknown' is the load-bearing case: a chunk that is not
--   resident says nothing about its soil, so its designation is
--   RETAINED and re-checked when the terrain publishes. Chunk eviction
--   must therefore only suppress DRAWING; it never removes a record.
data PlantSoilState
    = PlantSoilTilled
      -- ^ Resident and tilled: the designation stands.
    | PlantSoilLost
      -- ^ Resident and missing / out of column range / not tilled: the
      --   designation is removed.
    | PlantSoilUnknown
      -- ^ The storing chunk is not resident: retain and re-check later.
    deriving (Show, Eq)

-- | Resolve one designated tile's current soil state, step for step the
--   way 'handleWorldDesignatePlantCommand' resolves it at admission.
--
--   #1175: the key is resolved through 'canonicalTileFrame', so a
--   designation stored under a u-alias by a pre-#1175 save reaches the
--   chunk that actually STORES its tile instead of missing and reading
--   as unknown forever.
plantSoilState ∷ Int              -- ^ page wrap world size, in chunks
               → WorldTileData
               → (Int, Int)       -- ^ designated tile (gx, gy)
               → PlantSoilState
plantSoilState worldSize td (gx, gy) =
    let (coord, (lx, ly), _) = canonicalTileFrame worldSize gx gy
        idx = columnIndex lx ly
    in case lookupChunk coord td of
        Nothing → PlantSoilUnknown
        Just lc →
            let z   = lcSurfaceMap lc VU.! idx
                col = lcTiles lc V.! idx
                i   = z - ctStartZ col
                vg  = if i ≥ 0 ∧ i < VU.length (ctVeg col)
                      then ctVeg col VU.! i else 0
            in if isTilledSoil vg then PlantSoilTilled else PlantSoilLost

-- | Split a designation map against the current terrain: the records
--   that survive, and the keys whose soil is resident and no longer
--   tilled. Pure, so the invalidation rule is testable without an
--   engine — and so the sweep below has exactly one decision point.
prunePlantDesignations ∷ Int → WorldTileData → PlantDesignations
                       → (PlantDesignations, [(Int, Int)])
prunePlantDesignations worldSize td designations =
    let lost = HM.filterWithKey
            (\k _ → plantSoilState worldSize td k ≡ PlantSoilLost)
            designations
    in (HM.difference designations lost, HM.keys lost)

-- | Re-run the tilled-soil check over this page's whole plant
--   designation map and drop every record whose soil is resident and
--   lost. Returns the removed keys (also logged), which is the AI's
--   cancellation signal by way of the record simply not being there on
--   its next tick.
--
--   Call after any successful live write that can change a designated
--   tile's resolved surface or its vegetation, and whenever terrain
--   becomes resident.
revalidatePlantDesignations ∷ LoggerState → WorldState → IO [(Int, Int)]
revalidatePlantDesignations logger ws = do
    designations ← readIORef (wsPlantDesignationsRef ws)
    if HM.null designations then pure [] else do
        worldSize ← pageWrapWorldSize ws
        td ← readIORef (wsTilesRef ws)
        removed ← atomicModifyIORef' (wsPlantDesignationsRef ws) $ \current →
            let (kept, gone) = prunePlantDesignations worldSize td current
            in (kept, gone)
        forM_ removed $ \(gx, gy) →
            logDebug logger CatWorld $
                "Plant designation invalidated (soil no longer tilled) at ("
                <> tshow gx <> "," <> tshow gy <> ")"
        pure removed
