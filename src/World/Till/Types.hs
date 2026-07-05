{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Till-designation state (issue #333).
--
--   A till designation marks one tile slated for tilling — the fourth
--   per-tile designation layer, after mining ('World.Mine.Types',
--   removes terrain), construction ('World.Construct.Types', adds it),
--   and chop ('World.Chop.Types', removes flora). Tilling changes
--   neither terrain nor flora: on completion the till AI flips the
--   tile's vegetation id to 'World.Vegetation.vegTilledSoil' via the
--   generic @world.setVegAt@ edit-log primitive and the designation is
--   removed, mirroring the chop tile's z-annotation-only shape.
--
--   Claim state (which acolyte is tilling which tile) lives Lua-side in
--   scripts/unit_ai.lua, the same in-flight-claim shape as dig/chop
--   jobs, so the record here is just the tile annotation: the surface z
--   captured at designation time, which the marker renders from with no
--   per-frame column reads. Persisted in saves (wpsTillDesignations,
--   save v76).
module World.Till.Types
    ( TillDesignation(..)
    , TillDesignations
    , newTillDesignation
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM

-- | One designated tile. Field order is load-bearing (positional
--   Generic Serialize — append, don't reorder).
data TillDesignation = TillDesignation
    { tlZ ∷ !Int
      -- ^ Surface z captured at designation time (markers render from
      --   it; till progress itself is Lua AI state, not persisted —
      --   an interrupted till restarts, there is no mid-till visual).
    } deriving (Show, Eq, Generic, Serialize, NFData)

type TillDesignations = HM.HashMap (Int, Int) TillDesignation

newTillDesignation ∷ Int → TillDesignation
newTillDesignation = TillDesignation
