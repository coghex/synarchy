{-# LANGUAGE Strict #-}
-- | The vocabulary the world-content request and its answer share
--   (#2711, SCN-11 of epic #2698), kept free of any engine import so
--   "World.Command.Types" can carry it.
module Scenario.World.Types
    ( ScenarioWorldRequest(..)
    , WorldRefs(..)
    , WorldInstallResult(..)
    , WorldInstallOutcome(..)
    , WorldInstallReply(..)
    ) where

import UPrelude
import Control.Concurrent.MVar (MVar)
import Scenario.Types
import World.Chunk.Types (ChunkCoord)
import World.Flora.Identity (FloraInstanceId)

-- | The terrain, fluid and flora entries to materialize, each with its
--   document path (the path diagnostics are reported at).
data ScenarioWorldRequest = ScenarioWorldRequest
    { swrTerrain ∷ ![(Text, TerrainPatch)]
    , swrFluids  ∷ ![(Text, FluidPatch)]
    , swrFlora   ∷ ![(Text, FloraEntry)]
    } deriving (Show, Eq)

-- | Scenario identity → runtime target, for every ACCEPTED entry. A
--   rejected entry appears nowhere.
data WorldRefs = WorldRefs
    { wrTerrain     ∷ ![(ScenarioId, [(Int, Int, Int)])]
      -- ^ a patch → the cells its tags label (where they survive)
    , wrFluids      ∷ ![(ScenarioId, [(Int, Int)])]
    , wrOccurrences ∷ ![(ScenarioId, FloraInstanceId)]
    , wrPlots       ∷ ![(ScenarioId, (Int, Int))]
    } deriving (Show, Eq)

-- | What a committed materialization reports.
data WorldInstallResult = WorldInstallResult
    { wirRefs        ∷ !WorldRefs
    , wirDiagnostics ∷ ![ScenarioDiagnostic]
    , wirChunks      ∷ ![ChunkCoord]
      -- ^ the resident chunks that were rebuilt and re-seeded into the
      --   simulation
    } deriving (Show, Eq)

data WorldInstallOutcome
    = WorldInstalled !WorldInstallResult
    | WorldInstallNotAdmitted
      -- ^ the page is gone, or is no longer the incarnation the request
      --   named; nothing was changed
    | WorldInstallNotArena
      -- ^ the page is not an arena page: its chunks are generated from a
      --   world, not rebuilt from the arena base, so a materialization
      --   would destroy them; nothing was changed
    deriving (Show, Eq)

-- | Where a request answers. A newtype only so the command can stay
--   'Show'.
newtype WorldInstallReply = WorldInstallReply (MVar WorldInstallOutcome)

instance Show WorldInstallReply where
    show _ = "<world install reply>"
