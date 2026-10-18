{-# LANGUAGE Strict #-}
-- | Adding a location to a page that may be a FINITE arena (#2708).
--
--   A location is indivisible (D-21): if any part of its complete
--   footprint lies outside the arena's rectangle it is rejected whole.
--   'allocateLocationInstanceIn' is 'Location.Instance.allocateLocationInstance'
--   with that rule decided BEFORE the table changes, so a rejected
--   location leaves @lis@ entirely untouched — no id spent, no instance
--   stored, and therefore nothing for a later stage to stamp, reserve or
--   fill with owned contents (a rejected container contributes none,
--   recursively, because the object that would own them never exists).
--   Nothing is relocated or clipped to make it fit.
--
--   The footprint is the instance's stored absolute box
--   ('Location.Instance.liBounds'), the single footprint authority since
--   #1230.
module Location.ArenaPlacement
    ( LocationPlacementError(..)
    , locationPlacementErrorText
    , allocateLocationInstanceIn
    ) where

import UPrelude
import Location.Bounds (AbsBounds(..))
import Location.Instance
    ( LocationGeometryError, LocationInstance(..), LocationInstanceId(..)
    , LocationInstances(..), newLocationInstance )
import Location.Naming (LocationNamer)
import Location.Types (LocationDef)
import Scenario.Types (Footprint(..))
import World.Arena.Bounds (ArenaBounds, FootprintVerdict(..), footprintVerdict)
import World.Chunk.Types (ChunkCoord)
import qualified Data.HashMap.Strict as HM

-- | Why a location could not be added.
data LocationPlacementError
    = PlacementGeometry !LocationGeometryError
      -- ^ The location's own geometry is unrepresentable (unchanged).
    | PlacementCrossesArena ![(Integer, Integer)]
      -- ^ Part of its footprint lies outside the finite arena; the
      --   offending corners, for the warning.
    deriving (Show, Eq)

locationPlacementErrorText ∷ LocationPlacementError → Text
locationPlacementErrorText (PlacementGeometry e) = tshow e
locationPlacementErrorText (PlacementCrossesArena cs) =
    "its footprint crosses the finite arena boundary at "
    <> tshow cs

-- | 'Location.Instance.allocateLocationInstance' under a page's bounds.
allocateLocationInstanceIn
    ∷ Maybe ArenaBounds → Maybe LocationNamer → ChunkCoord → LocationDef
    → LocationInstances
    → Either LocationPlacementError (LocationInstanceId, LocationInstances)
allocateLocationInstanceIn mb namer coord def lis = do
    let iid = LocationInstanceId (lisNextId lis)
    inst ← either (Left . PlacementGeometry) Right
               (newLocationInstance namer iid coord def)
    let AbsBounds x0 y0 x1 y1 = liBounds inst
    case footprintVerdict mb (0, 0) (Footprint x0 y0 x1 y1) of
        FootprintCrosses cs → Left (PlacementCrossesArena cs)
        FootprintFits →
            pure ( iid
                 , lis { lisNextId = lisNextId lis + 1
                       , lisById   = HM.insert iid inst (lisById lis) } )
