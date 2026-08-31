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
    , reconcileResidentChunks
    , publishSeedChunks
    , releaseEvictedChunks
    , withTransientChunkClaim
    , readChunkOwner
    ) where

import UPrelude
import Control.Exception (finally)
import Data.IORef (readIORef, atomicModifyIORef')
import World.Chunk.Residency
    ( ChunkKey, ChunkOwner, ChunkRequest, ClaimKind(..), ClaimOutcome(..)
    , RequestOutcome(..), admitChunk, chunkKeyFor, claimChunk, crKey, ckCoord
    , evictChunk, mintChunkRequest, releaseChunk, requestChunk )
import World.Chunk.Types (ChunkCoord(..))
import World.Tile.Types (WorldTileData(..))
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

-- | Register DURABLE demand for these coords, returning the ones the
--   caller must SCHEDULE — in input order, one entry per physical chunk.
--
--   That return is what @world.loadChunksInRegion@ and the dump path's
--   @--region@ fill report and then append to the init queue, and it is
--   the caller's half of the owner's invariant: a key the owner holds as
--   'World.Chunk.Residency.ChunkRequested' is on the init queue, because
--   nothing scans the owner looking for unscheduled demand. So the
--   append must follow this call with no other queue write in between,
--   and must APPEND rather than replace.
--
--   A coord naming another spelling of a resident chunk, of an
--   already-scheduled request, or of a generation that is going to admit
--   its result is already-known work: neither counted nor queued.
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

-- | Claim the right to GENERATE these coords for KEEPS, returning one
--   tagged request per coord actually granted, in input order.
--
--   A coord already in flight or already resident is refused and simply
--   absent from the result, so a caller generates exactly what it owns —
--   and, because a refusal leaves the demand untouched, a caller that
--   pulled the coord off a work list has to leave it there.
claimChunkGeneration ∷ WorldState → WorldPageId → WorldGenParams
                     → [ChunkCoord] → IO [ChunkRequest]
claimChunkGeneration ws pid params coords =
    atomicModifyIORef' (wsChunkResidencyRef ws) $ \owner0 →
        let step (owner, acc) coord =
                let req = mintChunkRequest owner (pageChunkKey pid params coord)
                    (owner', outcome) = claimChunk DurableClaim req owner
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

-- | Record coords the page ALREADY holds as resident.
--
--   Not a new admission — the payloads are in
--   'World.State.Types.wsTilesRef' already — but the reconciliation that
--   keeps the owner from carrying a stale 'ChunkRequested' for a chunk
--   that is in fact loaded. 'World.Thread.ChunkLoading.drainInitQueues'
--   calls it for the half of its batch it finds already resident, which
--   is what makes the owner self-healing: a request that raced an
--   eviction (registered against the owner while the tile map still had
--   the payload) is settled here rather than sitting requested for ever
--   after its queue entry is dropped.
reconcileResidentChunks ∷ WorldState → WorldPageId → WorldGenParams
                        → [ChunkCoord] → IO ()
reconcileResidentChunks _  _   _      []     = pure ()
reconcileResidentChunks ws pid params coords =
    atomicModifyIORef' (wsChunkResidencyRef ws) $ \owner0 →
        ( foldl' (\owner c →
              fst (admitChunk (mintChunkRequest owner (pageChunkKey pid params c))
                              owner))
                 owner0 coords
        , () )

-- | Publish a page's SEED chunk set — the synchronously generated centre
--   chunk of a fresh or restored world, or an arena's whole chunk
--   set — from the claims taken before it was generated.
--
--   A seed runs the SAME lifecycle every other path runs: claim with
--   'claimChunkGeneration', generate, then admit and publish here. It is
--   split in two rather than done in one call because the generation
--   sits between the halves, and that is the whole point — a page is
--   registered in @wmWorlds@, and given its generation params, before
--   its seed is built ('World.Thread.Command.Init' does it early so Lua
--   can watch the loading phase), so the Lua thread can call
--   @world.loadChunksInRegion@ for the very chunk being generated.
--   Holding the claim across that window reports it as pending, which is
--   what it is; leaving the key absent would have that call queue and
--   COUNT a chunk the page is already producing.
--
--   Then owner before payloads, welded together here so no seed site can
--   get that order wrong either. Writing the tile map first would leave
--   a window in which the chunk is resident but the owner still says
--   absent, and a request landing there would queue and count a chunk
--   the page already holds. Admitting first reports it as satisfied,
--   which is true a moment later and never wrong.
--
--   The tile write REPLACES the page's whole map, as every seed site
--   does: a seed is the first thing a page holds.
publishSeedChunks ∷ WorldState → [ChunkRequest] → WorldTileData → IO ()
publishSeedChunks ws claims td = do
    admitResidentChunks ws claims
    atomicModifyIORef' (wsTilesRef ws) $ \_ → (td, ())

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
--   generating is retained rather than dropped — AND scheduled by the
--   requester, because this claim is going to throw its chunk away
--   rather than admit it. That is what the 'TransientClaim' below tells
--   'World.Chunk.Residency.requestChunk'. The body runs either way — a
--   refused claim (the chunk is resident, or the world thread is already
--   generating it) changes nothing about what the survey is allowed to
--   count.
withTransientChunkClaim ∷ WorldState → WorldPageId → WorldGenParams
                        → ChunkCoord → IO α → IO α
withTransientChunkClaim ws pid params coord body = do
    granted ← atomicModifyIORef' (wsChunkResidencyRef ws) $ \owner0 →
        let req = mintChunkRequest owner0 (pageChunkKey pid params coord)
            (owner1, outcome) = claimChunk TransientClaim req owner0
        in (owner1, [req | outcome ≡ ClaimGranted])
    body `finally` release granted
  where
    release reqs = atomicModifyIORef' (wsChunkResidencyRef ws) $ \owner0 →
        ( foldl' (flip releaseChunk) owner0 reqs, () )
