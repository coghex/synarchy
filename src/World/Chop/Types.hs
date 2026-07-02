{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Chop-designation state (issue #97).
--
--   A chop designation marks one tile whose flora is slated for felling.
--   The third per-tile designation layer, after mining ('World.Mine.Types',
--   removes terrain) and construction ('World.Construct.Types', adds it):
--   chopping removes FLORA — the tile must hold a @wood@-tagged
--   harvestable species (#94's interactive-flora backend), and the commit
--   handler filters the designated rectangle down to such tiles, at ANY
--   surface z (forests span slopes, so there is no per-z-level semantics
--   like the dig tool's).
--
--   Claim state (which acolyte is felling which tree) lives Lua-side in
--   scripts/unit_ai.lua, the same in-flight-claim shape as dig jobs, so
--   the record here is just the tile annotation: the surface z captured
--   at designation time, which the marker renders from with no per-frame
--   column reads (same trick as 'MineDesignation'). Persisted in saves
--   ('wpsChopDesignations', save v67).
module World.Chop.Types
    ( ChopDesignation(..)
    , ChopDesignations
    , newChopDesignation
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM

-- | One designated tile. Field order is load-bearing (positional
--   Generic Serialize — append, don't reorder).
data ChopDesignation = ChopDesignation
    { chZ ∷ !Int
      -- ^ Surface z captured at designation time (markers render from
      --   it; chop progress itself is Lua AI state, not persisted —
      --   an interrupted fell restarts, there is no mid-chop visual).
    } deriving (Show, Eq, Generic, Serialize, NFData)

type ChopDesignations = HM.HashMap (Int, Int) ChopDesignation

newChopDesignation ∷ Int → ChopDesignation
newChopDesignation = ChopDesignation
