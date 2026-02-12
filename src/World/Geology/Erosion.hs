{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Erosion
    ( applyErosion
    ) where

import UPrelude
import World.Types
import World.Geology.Types

-----------------------------------------------------------
-- Erosion Application (stub for now)
-----------------------------------------------------------

-- | Apply erosion to a tile position.
--   Takes the post-event elevation and returns a modification.
applyErosion ∷ ErosionParams → Int → Int → Int → Int → GeoModification
applyErosion _params _worldSize _gx _gy _elev = noModification
