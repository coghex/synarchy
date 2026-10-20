{-# LANGUAGE Strict #-}
-- | The vocabulary the placement request and its answer share (#2712,
--   SCN-12 of epic #2698), kept free of any engine-thread import so
--   "World.Command.Types" can carry it.
module Scenario.Placement.Types
    ( ScenarioPlacementRequest(..)
    , PlacementInstallResult(..)
    , PlacementInstallOutcome(..)
    , PlacementInstallReply(..)
    ) where

import UPrelude
import Control.Concurrent.MVar (MVar)
import Location.Instance (LocationInstanceId)
import Scenario.Placement.Location (LocationInstall)
import Scenario.Types

-- | The world-thread-owned half of placement: the structure pieces (part
--   of the page's durable edit log) and the placed-location table (part of
--   the page's generation parameters). Each part is OPTIONAL: 'Nothing'
--   leaves that family exactly as it is, @Just []@ REPLACES it with
--   nothing — a materialization establishes the baseline, so an empty
--   scenario clears it.
data ScenarioPlacementRequest = ScenarioPlacementRequest
    { sprStructures ∷ !(Maybe [(Text, StructurePiece)])
      -- ^ each entry with its document path
    , sprLocations  ∷ !(Maybe [LocationInstall])
      -- ^ locations ready to commit, their references already resolved
      --   against the units and items installed before them
    } deriving (Show, Eq)

-- | What a committed placement reports. A piece or a location appears only
--   if it is actually in the page's state: a queued or accepted request is
--   not a result.
data PlacementInstallResult = PlacementInstallResult
    { pirStructures  ∷ ![(ScenarioId, (Int, Int, Word8))]
      -- ^ a piece → its tile and slot tag
    , pirLocations   ∷ ![(ScenarioId, LocationInstanceId)]
    , pirDiagnostics ∷ ![ScenarioDiagnostic]
    } deriving (Show, Eq)

data PlacementInstallOutcome
    = PlacementInstalled !PlacementInstallResult
    | PlacementNotAdmitted
      -- ^ the page is gone, or is no longer the incarnation the request
      --   named; nothing was changed
    | PlacementNotArena
      -- ^ the page is not an arena page: its chunks are generated from a
      --   world, so rebuilding structures or a location table over them
      --   would destroy generated content; nothing was changed
    deriving (Show, Eq)

-- | Where a request answers. A newtype only so the command can stay
--   'Show'.
newtype PlacementInstallReply = PlacementInstallReply (MVar PlacementInstallOutcome)

instance Show PlacementInstallReply where
    show _ = "<placement install reply>"
