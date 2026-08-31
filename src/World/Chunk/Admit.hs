{-# LANGUAGE Strict #-}
-- | The page-scoped IO face of "World.Chunk.Residency": every caller
--   that creates, claims, admits or drops chunk residency goes through
--   exactly the verbs here (#2001).
--
--   Each one applies a single pure transition with one
--   'atomicModifyIORef'' on the page's own owner, so \"is this chunk
--   already being worked on?\" is answered by ONE value read once,
--   rather than reconstructed from 'World.State.Types.wsInitQueueRef'
--   and 'World.State.Types.wsTilesRef' snapshotted in a documented order
--   (#43's residue). That matters because the Lua thread asks the
--   question ('World.Chunk.Queue.enqueueChunkRequest') while the world
--   thread is answering it.
--
--   The owner mirrors the tile map, so the two must move together:
--   'admitResidentChunks' is called for exactly the chunks an insert
--   puts into 'World.State.Types.wsTilesRef', and
--   'releaseEvictedChunks' for exactly the coords an eviction reports.
--   An edit that rewrites an ALREADY resident chunk changes neither.
module World.Chunk.Admit
    ( pageChunkKey
    , claimedChunkCoord
    , registerChunkDemand
    , claimChunkGeneration
    , admitResidentChunks
    , seedResidentChunks
    , releaseEvictedChunks
    , withTransientChunkClaim
    , readChunkOwner
    ) where

import UPrelude
import Control.Exception (finally)
import Data.IORef (readIORef, atomicModifyIORef')
import World.Chunk.Residency
    ( ChunkKey, ChunkOwner, ChunkRequest, ClaimOutcome(..), RequestOutcome(..)
    , admitChunk, chunkKeyFor, claimChunk, crKey, ckCoord, evictChunk
    , mintChunkRequest, releaseChunk, requestChunk )
import World.Chunk.Types (ChunkCoord(..))
import World.Generate.Types (WorldGenParams(..))
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldState(..))

-- | This page's canonical key for a coord — the identity every verb
--   below and every owner entry is stated in.
pageChunkKey ∷ WorldPageId → WorldGenParams → ChunkCoord → ChunkKey
pageChunkKey = chunkKeyFor

-- | The coord a granted claim owns the generation of: what the caller
--   actually feeds to the generator, and the key it later admits under.
claimedChunkCoord ∷ ChunkRequest → ChunkCoord
claimedChunkCoord = ckCoord . crKey

-- | The page's residency owner, for callers that only observe it.
readChunkOwner ∷ WorldState → IO ChunkOwner
readChunkOwner ws = readIORef (wsChunkResidencyRef ws)

-- | Register DURABLE demand for these coords, returning the ones whose
--   demand is NEW — in input order, one entry per physical chunk.
--
--   That return is what @world.loadChunksInRegion@ and the dump path's
--   @--region@ fill report and then append to the init queue: a coord
--   naming another spelling of a resident, requested or in-flight chunk
--   is already-known work, so it is neither counted nor queued.
registerChunkDemand ∷ WorldState → WorldPageId → WorldGenParams
                    → [ChunkCoord] → IO [ChunkCoord]
registerChunkDemand ws pid params coords =
    atomicModifyIORef' (wsChunkResidencyRef ws) $ \owner0 →
        let step (owner, acc) coord =
                let key = pageChunkKey pid params coord
                    (owner', outcome) = requestChunk key owner
                in case outcome of
                    RequestRegistered → (owner', coord : acc)
                    _                 → (owner', acc)
            (owner1, revNew) = foldl' step (owner0, []) coords
        in (owner1, reverse revNew)

-- | Claim the right to GENERATE these coords, returning one tagged
--   request per coord actually granted, in input order.
--
--   A coord already in flight or already resident is refused and simply
--   absent from the result, so a caller generates exactly what it owns.
claimChunkGeneration ∷ WorldState → WorldPageId → WorldGenParams
                     → [ChunkCoord] → IO [ChunkRequest]
claimChunkGeneration ws pid params coords =
    atomicModifyIORef' (wsChunkResidencyRef ws) $ \owner0 →
        let step (owner, acc) coord =
                let req = mintChunkRequest owner (pageChunkKey pid params coord)
                    (owner', outcome) = claimChunk req owner
                in case outcome of
                    ClaimGranted → (owner', req : acc)
                    ClaimRefused → (owner', acc)
            (owner1, revGranted) = foldl' step (owner0, []) coords
        in (owner1, reverse revGranted)

-- | THE admission boundary. Call it with exactly the claims whose
--   payloads an insert just put into 'World.State.Types.wsTilesRef'.
--
--   The 'World.Chunk.Residency.AdmitOutcome' each transition reports is
--   deliberately discarded here: generation is synchronous on the world
--   thread in this slice, so no admission can name a superseded page
--   generation. CRS-11 moves generation off-thread and branches on it.
admitResidentChunks ∷ WorldState → [ChunkRequest] → IO ()
admitResidentChunks _  []   = pure ()
admitResidentChunks ws reqs =
    atomicModifyIORef' (wsChunkResidencyRef ws) $ \owner0 →
        ( foldl' (\owner req → fst (admitChunk req owner)) owner0 reqs, () )

-- | Claim and admit in one step, for a page SEED: the synchronously
--   generated centre chunk of a fresh or restored world, and an arena's
--   whole chunk set.
--
--   Those payloads are built before the page can take a request, so
--   there is never prior demand to claim — but they are new residency
--   all the same, and they reach the resident set through the same
--   'admitResidentChunks' every other path uses.
seedResidentChunks ∷ WorldState → WorldPageId → WorldGenParams
                   → [ChunkCoord] → IO ()
seedResidentChunks ws pid params coords =
    admitResidentChunks ws =≪ claimChunkGeneration ws pid params coords

-- | These coords left the tile map: their keys are requestable again.
--
--   Eviction POLICY is unchanged and still lives in
--   'World.Tile.Types.evictDistantChunksWithReport'; this only keeps the
--   owner from claiming a chunk is resident after its payload is gone,
--   which would make the evicted chunk unreloadable.
releaseEvictedChunks ∷ WorldState → WorldPageId → WorldGenParams
                     → [ChunkCoord] → IO ()
releaseEvictedChunks _  _   _      []     = pure ()
releaseEvictedChunks ws pid params coords =
    atomicModifyIORef' (wsChunkResidencyRef ws) $ \owner0 →
        ( foldl' (\owner c → evictChunk (pageChunkKey pid params c) owner)
                 owner0 coords
        , () )

-- | Run a TRANSIENT generation of one chunk under the page's canonical
--   key: claimed for the duration, never admitted.
--
--   The cursor's ore survey ('World.Thread.Cursor') builds an unloaded
--   chunk into a LOCAL tile map to count its ore, memoises the rendered
--   text and drops the chunk. Registering that here is what stops a
--   concurrent @world.loadChunksInRegion@ from starting a second
--   generation of the same chunk — and releasing it on the way out
--   (including on an exception) is what stops a throwaway generation
--   from leaving a permanent in-flight entry.
--
--   Durable demand survives the round trip: a key nothing else wanted
--   returns to absent, a key the init queue had already requested
--   returns to requested, and a request that arrives WHILE the survey is
--   generating is retained rather than dropped. The body runs either
--   way — a refused claim (the chunk is resident, or the world thread is
--   already generating it) changes nothing about what the survey is
--   allowed to count.
withTransientChunkClaim ∷ WorldState → WorldPageId → WorldGenParams
                        → ChunkCoord → IO α → IO α
withTransientChunkClaim ws pid params coord body = do
    granted ← claimChunkGeneration ws pid params [coord]
    body `finally` release granted
  where
    release reqs = atomicModifyIORef' (wsChunkResidencyRef ws) $ \owner0 →
        ( foldl' (flip releaseChunk) owner0 reqs, () )
