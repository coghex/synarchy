-- | On-demand scalar snapshots made by the simulation owner. No fluid
-- vectors or chunk payloads escape in a reply, so diagnostics do not keep
-- an old simulation heap alive while the client is idle.
module Sim.Memory
    ( SimMemory(..), SimPageMemory(..), captureSimMemory, simChunkBytes ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import Engine.Core.Clock (monotonicSeconds)
import Sim.State.Types (SimState(..), SimWorldState(..), SimChunkState(..))
import World.Chunk.Memory (wordBytes, vectorBytes, boxedMaybeBytes, mapEntryBytes)
import World.Chunk.Types (ChunkCoord)
import World.Chunk.Residency (ChunkGeneration)
import World.Page.Types (WorldPageId)

data SimPageMemory = SimPageMemory
    { spmPage ∷ !WorldPageId
    , spmEpoch ∷ !(Maybe ChunkGeneration)
    , spmChunks ∷ ![(ChunkCoord, Integer)]
    } deriving (Eq, Show)
data SimMemory = SimMemory
    { smTime ∷ !Double
    , smPages ∷ ![SimPageMemory]
    } deriving (Eq, Show)

simChunkBytes ∷ SimChunkState → Integer
simChunkBytes sc = 10*wordBytes + mapEntryBytes
    + boxedMaybeBytes 3 (scsFluid sc)
    + vectorBytes wordBytes (VU.length (scsTerrain sc))
    + boxedMaybeBytes 4 (scsActiveFluid sc)
    + vectorBytes 1 (VU.length (scsSideDeco sc))

captureSimMemory ∷ SimState → IO SimMemory
captureSimMemory ss = do
    now ← monotonicSeconds
    -- Strict folds force all estimates here, rather than returning thunks
    -- which would retain the owner's old vectors until the Lua reader runs.
    let pages = HM.foldlWithKey' (\acc pid sws →
            let chunks = HM.foldlWithKey' (\rows cc sc →
                    let !bytes = simChunkBytes sc in (cc, bytes):rows)
                    [] (swsChunks sws)
                !row = SimPageMemory pid (swsIncarnation sws) chunks
            in row:acc) [] (ssWorlds ss)
    length pages `seq` pure (SimMemory now pages)
