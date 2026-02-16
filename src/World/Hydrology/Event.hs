{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Event
    ( applyHydroFeature
    , applyHydroEvolution
    ) where

import UPrelude
import World.Types
import World.Hydrology.Types
import World.Hydrology.River (applyRiverCarve, applyRiverEvolution)
import World.Hydrology.Glacier (applyGlacierCarve, applyGlacierEvolution)
import World.Geology.Types (GeoModification(..), noModification)

-----------------------------------------------------------
-- HydroEvent dispatch (for new feature placement)
-----------------------------------------------------------

-- | Apply a hydrological feature's terrain modification to
--   a single column. Called from applyGeoEvent when it hits
--   a HydroEvent.
--
--   RiverFeature  → V-shaped valley carve
--   GlacierFeature → U-shaped valley carve + moraine
--   LakeFeature   → no terrain mod (lakes are fluid, not carving)
applyHydroFeature ∷ HydroFeature → Int → Int → Int → Int → GeoModification
applyHydroFeature (RiverFeature river) worldSize gx gy baseElev =
    applyRiverCarve river worldSize gx gy baseElev
applyHydroFeature (GlacierFeature glacier) worldSize gx gy baseElev =
    applyGlacierCarve glacier worldSize gx gy baseElev
applyHydroFeature (LakeFeature _) _worldSize _gx _gy _baseElev =
    -- Lakes don't carve terrain. They fill with water at the
    -- fluid stage (computeChunkFluid / computeChunkLakes).
    -- The basin they sit in was carved by the glacier or river
    -- that created them.
    noModification

-----------------------------------------------------------
-- HydroModify dispatch (for feature evolution)
-----------------------------------------------------------

-- | Apply a hydrological evolution event to a single column.
--   Called from applyGeoEvent when it hits a HydroModify.
--
--   Most evolution events modify the PersistentFeature's shape
--   (updated in TimelineBuildState during timeline build) and
--   the terrain effect comes from the *next* age's HydroEvent
--   using the updated params. Only a few produce immediate
--   per-tile terrain changes.
--
--   River events with terrain effect:
--     RiverDam → small debris ridge at the dam point
--
--   Glacier events with terrain effect:
--     (none — all glacier terrain comes from the feature's
--      GlacierParams being replayed via HydroEvent. The
--      evolution updates the params, and the next age's
--      GlacierFeature applies the updated carve.)
--
--   This mirrors how applyEvolution works for volcanoes:
--     GoDormant → noModification
--     GoExtinct → noModification
--     Reactivate → terrain change (growth)
--     CollapseToCaldera → terrain change (depression)
--
applyHydroEvolution ∷ HydroEvolution → Int → Int → Int → Int → GeoModification

-- ===== River evolution =====
applyHydroEvolution (RiverBranch branchPt angle len childId) ws gx gy e =
    applyRiverEvolution (RiverBranch branchPt angle len childId) ws gx gy e

applyHydroEvolution (RiverMeander seed' amt) ws gx gy e =
    applyRiverEvolution (RiverMeander seed' amt) ws gx gy e

applyHydroEvolution (RiverCapture capturedId capturePoint) ws gx gy e =
    applyRiverEvolution (RiverCapture capturedId capturePoint) ws gx gy e

applyHydroEvolution (RiverDam damPt lakeId damH) ws gx gy e =
    applyRiverEvolution (RiverDam damPt lakeId damH) ws gx gy e

applyHydroEvolution RiverDryUp ws gx gy e =
    applyRiverEvolution RiverDryUp ws gx gy e

-- ===== Glacier evolution =====
applyHydroEvolution (GlacierAdvance advLen advWid) ws gx gy e =
    applyGlacierEvolution (GlacierAdvance advLen advWid) ws gx gy e

applyHydroEvolution (GlacierRetreat retreatLen moraineDep) ws gx gy e =
    applyGlacierEvolution (GlacierRetreat retreatLen moraineDep) ws gx gy e

applyHydroEvolution (GlacierMelt moraineDep) ws gx gy e =
    applyGlacierEvolution (GlacierMelt moraineDep) ws gx gy e

applyHydroEvolution (GlacierBranch branchPt angle len childId) ws gx gy e =
    applyGlacierEvolution (GlacierBranch branchPt angle len childId) ws gx gy e

-- ===== Lake evolution =====
applyHydroEvolution (LakeDrain _) _ws _gx _gy _e =
    noModification  -- lake draining doesn't carve terrain

applyHydroEvolution (LakeExpand _ _) _ws _gx _gy _e =
    noModification  -- lake expansion is a fluid change, not terrain
