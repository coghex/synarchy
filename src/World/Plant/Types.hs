{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Plant-designation state (issue #335).
--
--   A plant designation marks one tile slated for planting a chosen
--   crop — the fifth per-tile designation layer, after mining, chop,
--   construction, and till. Unlike mine/chop/till/construct it is
--   SINGLE-TILE only (no anchor→rectangle sweep): the planting screen
--   already scopes the player to one tile before a crop is chosen, so
--   there is no pending-anchor state to track here.
--
--   Claim state (which acolyte plants which tile) will live Lua-side
--   like the other designation layers once the farm AI (#336) claims
--   it; the record here is just the tile annotation the marker renders
--   from, plus the resolved crop species. Persisted in saves
--   (wpsPlantDesignations).
module World.Plant.Types
    ( PlantDesignation(..)
    , PlantDesignations
    , newPlantDesignation
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import World.Flora.Types (FloraId)

-- | One designated tile. Field order is load-bearing (positional
--   Generic Serialize — append, don't reorder).
data PlantDesignation = PlantDesignation
    { ptZ    ∷ !Int
      -- ^ Surface z captured at designation time (markers render from
      --   it, same convention as 'World.Till.Types.TillDesignation').
    , ptCrop ∷ !FloraId
      -- ^ The chosen crop species, resolved from the player-facing
      --   crop name at designation time (mirrors
      --   'World.Flora.CropPlot.CropPlot''s cpSpecies).
    } deriving (Show, Eq, Generic, Serialize, NFData)

type PlantDesignations = HM.HashMap (Int, Int) PlantDesignation

newPlantDesignation ∷ Int → FloraId → PlantDesignation
newPlantDesignation = PlantDesignation
