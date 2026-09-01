{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | Chop-designation state (issue #97, re-keyed by issue #1854).
--
--   A chop designation marks one PLANT slated for felling. The third
--   per-tile designation layer, after mining ('World.Mine.Types',
--   removes terrain) and construction ('World.Construct.Types', adds
--   it): chopping removes FLORA — the target must be a @wood@-tagged
--   harvestable species (#94's interactive-flora backend), and the
--   commit handler filters the designated rectangle down to such
--   plants, at ANY surface z (forests span slopes, so there is no
--   per-z-level semantics like the dig tool's).
--
--   #1854: the map is keyed by 'World.Flora.Identity.FloraInstanceId',
--   NOT by tile. Two wood-tagged trees can legitimately share one tile
--   ('World.Flora.Types.FloraInstance'\'s own co-tenancy note), and a
--   tile key cannot tell them apart — designating one used to mark
--   both, and felling one used to cancel the other's designation. The
--   record therefore carries the plant's own tile coords itself, so a
--   marker still has somewhere to draw and a nearest-designation scan
--   still has somewhere to measure to.
--
--   Claim state (which acolyte is felling which tree) lives Lua-side in
--   scripts/unit_ai_chop.lua, keyed by page + instance id since #1854
--   (the same in-flight-claim shape as dig jobs). Persisted in saves
--   ('wpsChopDesignations', @world-activity@ v4).
--
--   'PendingChopDesignations' is the ONE remaining tile-keyed shape,
--   and it is explicitly NOT a second authority (#1854 requirement
--   14): it holds pre-#1854 saved entries whose chunk was not loaded
--   when the save was read, so no instance could be resolved yet. It
--   is persisted so repeated save/load cannot silently discard it, and
--   "World.Flora.Designation" drains it into the real map as each
--   chunk arrives. Nothing may answer a designation, marker, claim or
--   harvest query from it.
module World.Chop.Types
    ( ChopDesignation(..)
    , ChopDesignations
    , PendingChopDesignations
    , emptyChopDesignations
    , emptyPendingChopDesignations
    , newChopDesignation
    , chopDesignationTile
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import World.Flora.Identity (FloraInstanceId)

-- | One designated plant. Field order is load-bearing (positional
--   Generic Serialize — append, don't reorder).
data ChopDesignation = ChopDesignation
    { chZ ∷ !Int
      -- ^ Surface z captured at designation time (markers render from
      --   it; chop progress itself is Lua AI state, not persisted —
      --   an interrupted fell restarts, there is no mid-chop visual).
    , chGX ∷ !Int
      -- ^ CANONICAL global tile x the designated plant stands on
      --   (#1854). The map is keyed by instance identity now, so the
      --   place to draw a marker and the place to walk to have to be
      --   carried here. Canonical because every other tile-keyed
      --   world map is (#1175's frame contract) and a seam plant must
      --   report one coordinate, not an alias that varies with who
      --   designated it.
    , chGY ∷ !Int
      -- ^ CANONICAL global tile y — see 'chGX'.
    } deriving (Show, Eq, Generic, Serialize, NFData)

-- | Designated plants on one page, keyed by stable flora-instance
--   identity (#1854).
type ChopDesignations = HM.HashMap FloraInstanceId ChopDesignation

-- | Pre-#1854 tile-keyed designations awaiting an instance to attach
--   to (see the module header). Deferred, never authoritative.
type PendingChopDesignations = HM.HashMap (Int, Int) ChopDesignation

emptyChopDesignations ∷ ChopDesignations
emptyChopDesignations = HM.empty

emptyPendingChopDesignations ∷ PendingChopDesignations
emptyPendingChopDesignations = HM.empty

newChopDesignation ∷ Int → Int → Int → ChopDesignation
newChopDesignation = ChopDesignation

chopDesignationTile ∷ ChopDesignation → (Int, Int)
chopDesignationTile cd = (chGX cd, chGY cd)
