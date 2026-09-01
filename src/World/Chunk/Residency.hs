{-# LANGUAGE Strict #-}
-- | ONE canonical chunk key, and ONE owner that says what is happening
--   to it.
--
--   Chunk demand used to reach the resident tile map through several
--   independent paths that shared neither an identity for \"this
--   physical chunk\" nor an admission point (#2001). 'World.Tile.Types'
--   is keyed by a bare 'ChunkCoord' with no page and no
--   canonicalisation, chunks are STORED u-wrapped, and the camera
--   loader canonicalised with a bare 'wrapChunkCoordU' while the init
--   queue used the guarded 'canonicalChunkCoord' — two functions that
--   agree on ordinary worlds and nothing held together.
--
--   This module is the fix, in three layers:
--
--     * 'ChunkKey' — the identity. It pairs the 'WorldPageId' with the
--       CANONICAL coordinate, and 'chunkKeyFor' is the only way to build
--       one, so the guards below cannot be skipped at a call site.
--     * 'ChunkGeneration' — the epoch. Every 'ChunkOwner' is stamped
--       with a process-unique one, so a result can be recognised as
--       belonging to a page generation that has since been replaced.
--     * 'ChunkOwner' — the per-page residency state machine. Absent,
--       requested, in flight, resident; one atomic value, so
--       \"is this chunk already being worked on?\" no longer has to be
--       reconstructed from two 'IORef's read in a documented order
--       (#43's residue, in 'World.Chunk.Queue.enqueueChunkRequest').
--
--   The owner is PURE. All of it is @(ChunkOwner → (ChunkOwner, α))@
--   transitions, so the callers in "World.Chunk.Admit" can apply each
--   one with a single 'Data.IORef.atomicModifyIORef''.
module World.Chunk.Residency
    ( -- * The one canonical coordinate
      canonicalChunkCoord
      -- * The one canonical key
    , ChunkKey
    , chunkKeyFor
    , ckPage
    , ckCoord
      -- * The generation epoch
    , ChunkGeneration
    , newChunkGeneration
    , chunkGenerationValue
      -- * Tagged requests
    , ChunkRequest
    , crKey
    , crGeneration
      -- * The per-page residency owner
    , ChunkOwner
    , emptyChunkOwner
    , chunkOwnerGeneration
    , chunkOwnerSize
    , ChunkState(..)
    , chunkStateOf
    , mintChunkRequest
    , isCurrentGeneration
      -- * Transitions
    , RequestOutcome(..)
    , requestChunk
    , ClaimKind(..)
    , ClaimOutcome(..)
    , claimChunk
    , AdmitOutcome(..)
    , admitChunk
    , evictChunk
    , releaseChunk
    ) where

import UPrelude
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import System.IO.Unsafe (unsafePerformIO)
import World.Chunk.Types (ChunkCoord(..), wrapChunkCoordU)
import World.Generate.Types (WorldGenParams(..), isArenaParams)
import World.Page.Types (WorldPageId(..))

-- | The physical identity of a chunk coord on a page with these
--   generation params: the key the chunk is (or will be) stored under.
--
--   Identity on a NON-wrapping page, and the two of those are selected
--   separately. @worldSize ≤ 0@ has no seam at all. An arena's
--   'wgpWorldSize' is a sentinel 100000 rather than a real extent
--   ('World.Thread.Command.Init.handleWorldInitArenaCommand'), so it is
--   recognised by 'isArenaParams' and never handed to
--   'wrapChunkCoordU' — passing that sentinel through would leave an
--   arena coord past u = ±50000 silently wrapped.
--
--   This is the ONE canonicalisation (#2001). It was
--   @World.Chunk.Queue.chunkQueueCanon@ when only the init queue's
--   producers shared it (#1723); the camera loader ran its own bare
--   'wrapChunkCoordU' beside it, missing both guards above. Everything
--   now measures through this, and "World.Chunk.Queue" re-exports it for
--   the producers that already did.
canonicalChunkCoord ∷ WorldGenParams → ChunkCoord → ChunkCoord
canonicalChunkCoord params
    | isArenaParams params = id
    | worldSize ≤ 0        = id
    | otherwise            = wrapChunkCoordU worldSize
  where worldSize = wgpWorldSize params

-- | One physical chunk on one page.
--
--   Two coordinate SPELLINGS of one physical chunk produce equal keys;
--   the same coordinate on two PAGES does not. The constructor is
--   deliberately not exported — 'chunkKeyFor' is the only way to build
--   one, so a caller cannot assemble a key from a coordinate that was
--   never canonicalised.
data ChunkKey = ChunkKey
    { ckPage  ∷ !WorldPageId  -- ^ the page this chunk belongs to
    , ckCoord ∷ !ChunkCoord   -- ^ the CANONICAL coordinate on that page
    } deriving (Show, Eq, Ord)

instance Hashable ChunkKey where
    hashWithSalt s (ChunkKey p c) = s `hashWithSalt` p `hashWithSalt` c

-- | The only 'ChunkKey' constructor: canonicalise, then qualify by page.
chunkKeyFor ∷ WorldPageId → WorldGenParams → ChunkCoord → ChunkKey
chunkKeyFor pid params = ChunkKey pid . canonicalChunkCoord params

-- | A page GENERATION: which incarnation of a 'WorldPageId' a request
--   was made against.
--
--   Process-unique and monotonic, never persisted. A page id is reused
--   constantly — @main_world@ is re-initialised on every Exit to Menu,
--   an arena replaces it wholesale, and a transactional load republishes
--   the whole session — and each of those builds a FRESH
--   'World.State.Types.WorldState'. So the epoch is allocated where that
--   state is, which is what makes \"the same page, a later generation\"
--   distinguishable at all: a page-id comparison cannot see it.
newtype ChunkGeneration = ChunkGeneration Word64
    deriving (Show, Eq, Ord)

-- | The epoch as a plain number, for logging and for a test that needs
--   to show two generations differ.
chunkGenerationValue ∷ ChunkGeneration → Word64
chunkGenerationValue (ChunkGeneration n) = n

-- | The process-wide allocator behind 'newChunkGeneration'.
--
--   A module-global counter rather than a field threaded from somewhere:
--   the epoch has to be unique across every 'WorldState' this process
--   ever builds, and those are built from three unrelated places
--   ("World.Thread.Command.Init" twice, "World.Load.Stage" once) plus
--   every test fixture. Seeding each one from a shared constant would
--   make a replacement page indistinguishable from the page it replaced,
--   which is the single thing this value exists to detect.
chunkGenerationCounter ∷ IORef Word64
chunkGenerationCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE chunkGenerationCounter #-}

-- | Allocate the next process-unique 'ChunkGeneration'. Thread-safe
--   (one atomic bump), so it is correct to call from any thread.
newChunkGeneration ∷ IO ChunkGeneration
newChunkGeneration =
    atomicModifyIORef' chunkGenerationCounter (\n → (n + 1, ChunkGeneration (n + 1)))

-- | A demand for one chunk, tagged with the page generation it was made
--   against.
--
--   Minted by 'mintChunkRequest' from the owner's own epoch and carried
--   through claim and admission, so a result arriving after its page was
--   replaced can be recognised as superseded ('AdmittedSuperseded').
--   Nothing acts on that yet — generation is synchronous on the world
--   thread — but the tag has to exist before it can be acted on.
data ChunkRequest = ChunkRequest
    { crKey        ∷ !ChunkKey
    , crGeneration ∷ !ChunkGeneration
    } deriving (Show, Eq)

-- | What the owner is holding for one key. The four-valued view
--   'chunkStateOf' answers with; the owner's own entries carry a little
--   more (see 'ChunkEntry').
data ChunkState
    = ChunkAbsent     -- ^ nobody has asked for it and it is not loaded
    | ChunkRequested  -- ^ durable demand registered, generation not started
    | ChunkInFlight   -- ^ generation claimed and running
    | ChunkResident   -- ^ admitted into the page's tile map
    deriving (Show, Eq, Ord)

-- | Whether a durable request is outstanding behind an in-flight claim.
--
--   This is what lets a TRANSIENT generation — the cursor's ore survey,
--   which builds a chunk into a local map and drops it — release its
--   claim without discarding real demand: an otherwise-absent key goes
--   back to absent, but a key that was already requested (or that a
--   request arrived for WHILE the survey was generating) goes back to
--   requested, so the durable work still happens.
data DurableDemand = NoDurableDemand | HasDurableDemand
    deriving (Show, Eq)

-- | Whether an in-flight claim is going to ADMIT its result.
--
--   The distinction decides what a request meeting that claim has to do.
--   A 'DurableClaim' — the camera loader, the init-queue drain — ends in
--   'admitChunk', so demand behind it is already going to be met and the
--   caller has nothing to schedule. A 'TransientClaim' — the cursor's
--   ore survey — ends in 'releaseChunk' and THROWS ITS CHUNK AWAY, so
--   demand behind it still has to be scheduled or it would sit
--   'ChunkRequested' for ever with nothing generating it.
data ClaimKind = DurableClaim | TransientClaim
    deriving (Show, Eq)

-- | The owner's internal per-key entry. Absent is the absence of an
--   entry, so an idle page's owner is genuinely empty.
data ChunkEntry
    = EntryRequested
    | EntryInFlight !ClaimKind !DurableDemand
    | EntryResident
    deriving (Show, Eq)

-- | One page's residency bookkeeping: its generation epoch, and what it
--   is holding for each key.
data ChunkOwner = ChunkOwner
    { coGeneration ∷ !ChunkGeneration
    , coEntries    ∷ !(HM.HashMap ChunkKey ChunkEntry)
    } deriving (Show, Eq)

-- | A page's owner at birth: its own epoch, and nothing resident,
--   requested or in flight.
emptyChunkOwner ∷ ChunkGeneration → ChunkOwner
emptyChunkOwner gen = ChunkOwner { coGeneration = gen, coEntries = HM.empty }

-- | The epoch every request this owner mints is stamped with.
chunkOwnerGeneration ∷ ChunkOwner → ChunkGeneration
chunkOwnerGeneration = coGeneration

-- | How many keys the owner is holding anything at all for. Absent keys
--   are not entries, so this is requested + in flight + resident.
chunkOwnerSize ∷ ChunkOwner → Int
chunkOwnerSize = HM.size . coEntries

-- | The four-valued residency of one key.
chunkStateOf ∷ ChunkKey → ChunkOwner → ChunkState
chunkStateOf key owner = case HM.lookup key (coEntries owner) of
    Nothing                  → ChunkAbsent
    Just EntryRequested      → ChunkRequested
    Just (EntryInFlight _ _) → ChunkInFlight
    Just EntryResident       → ChunkResident

-- | Tag a key with this owner's generation.
mintChunkRequest ∷ ChunkOwner → ChunkKey → ChunkRequest
mintChunkRequest owner key = ChunkRequest
    { crKey = key, crGeneration = coGeneration owner }

-- | Was this request made against the generation the owner is still on?
--
--   The query CRS-11 will reject an out-of-date asynchronous result
--   with. Today it is always 'True' for a request minted from the owner
--   it is used against, and 'False' across a page replacement — which is
--   exactly the distinction that has to exist before generation can move
--   off the world thread.
isCurrentGeneration ∷ ChunkRequest → ChunkOwner → Bool
isCurrentGeneration req owner = crGeneration req ≡ coGeneration owner

-- | What a 'requestChunk' found.
--
--   The distinction is a SCHEDULING one, not a bookkeeping one:
--   'RequestRegistered' means the caller must now put this key on the
--   page's work list, and the other two mean it must not. That is the
--   owner's central invariant —
--
--     a key the owner holds as 'ChunkRequested' is on
--     'World.State.Types.wsInitQueueRef'
--
--   — and every producer depends on it, because nothing scans the owner
--   looking for unscheduled demand.
data RequestOutcome
    = RequestRegistered       -- ^ new demand the caller must schedule
    | RequestAlreadyPending   -- ^ demand already scheduled, or already being generated for keeps
    | RequestAlreadySatisfied -- ^ already resident
    deriving (Show, Eq)

-- | Register DURABLE demand for a key.
--
--   Idempotent under the canonical identity, which is the whole point:
--   a second request naming the other seam spelling of a resident,
--   requested or in-flight chunk adds no second entry and reports the
--   work as already satisfied or already pending — whether the two
--   requests came from the camera path, the init queue, or one of each.
--
--   The one case that is NOT simply \"already pending\" is a request
--   meeting a 'TransientClaim'. That generation is thrown away
--   ('releaseChunk'), so nothing is going to admit this chunk: the
--   demand is recorded on the claim AND reported as
--   'RequestRegistered', so the caller schedules it. Without that, the
--   release would leave the key 'ChunkRequested' with nothing queued to
--   generate it — @world.loadChunksInRegion@ reporting no work and
--   @world.waitForChunks@ reporting completion for a chunk that never
--   becomes resident. A SECOND request during the same transient claim
--   is genuinely already pending, because the first one scheduled it.
requestChunk ∷ ChunkKey → ChunkOwner → (ChunkOwner, RequestOutcome)
requestChunk key owner = case HM.lookup key (coEntries owner) of
    Nothing → ( insertEntry key EntryRequested owner, RequestRegistered )
    Just EntryRequested → ( owner, RequestAlreadyPending )
    Just (EntryInFlight DurableClaim _) → ( owner, RequestAlreadyPending )
    Just (EntryInFlight TransientClaim HasDurableDemand) →
        ( owner, RequestAlreadyPending )
    Just (EntryInFlight TransientClaim NoDurableDemand) →
        ( insertEntry key (EntryInFlight TransientClaim HasDurableDemand) owner
        , RequestRegistered )
    Just EntryResident → ( owner, RequestAlreadySatisfied )

-- | What a 'claimChunk' found.
data ClaimOutcome
    = ClaimGranted -- ^ this caller now owns the generation of that key
    | ClaimRefused -- ^ someone else is generating it, or it is resident
    deriving (Show, Eq)

-- | Claim the right to GENERATE a key.
--
--   Granted from absent (an undemanded chunk the camera wants now, or a
--   transient survey) and from requested — a camera request meeting a
--   key the init queue already asked for CLAIMS that same demand rather
--   than creating a second entry or skipping. Refusing there would move
--   generation out of the drain-then-camera order
--   'World.Thread.worldTick' runs today, which requirement 8 forbids.
--
--   Refused for a key already in flight (nobody generates a chunk twice)
--   and for a resident one (nothing to generate). A REFUSED claim leaves
--   the demand exactly as it was, so a caller that took the key off a
--   work list on the strength of a claim must put it back — see
--   'World.Thread.ChunkLoading.drainInitQueues'.
--
--   The 'ClaimKind' records whether this claim is going to admit its
--   result, which is what a later 'requestChunk' meeting it needs to
--   know.
claimChunk ∷ ClaimKind → ChunkRequest → ChunkOwner → (ChunkOwner, ClaimOutcome)
claimChunk kind req owner = case HM.lookup key (coEntries owner) of
    Nothing →
        ( insertEntry key (EntryInFlight kind NoDurableDemand) owner
        , ClaimGranted )
    Just EntryRequested →
        ( insertEntry key (EntryInFlight kind HasDurableDemand) owner
        , ClaimGranted )
    Just (EntryInFlight _ _) → ( owner, ClaimRefused )
    Just EntryResident       → ( owner, ClaimRefused )
  where key = crKey req

-- | Whether an admission belonged to the page generation that is still
--   live.
data AdmitOutcome
    = AdmittedCurrent    -- ^ the request names this owner's generation
    | AdmittedSuperseded -- ^ the page has been replaced since the request
    deriving (Show, Eq)

-- | THE admission boundary: this key's payload is now in the page's tile
--   map.
--
--   Every newly generated resident payload goes through here — fresh
--   world centres, arena seeds, restored-page centres, camera batches
--   and init-queue batches alike. An edit that REPLACES an already
--   resident chunk is not an admission (the key was resident before and
--   after), and neither is the ore survey's local temporary map, which
--   never becomes resident at all.
--
--   The outcome reports whether the request's epoch is still the owner's.
--   Nothing acts on 'AdmittedSuperseded' in this slice: generation is
--   synchronous on the world thread, so a superseded result cannot
--   arrive. CRS-11 moves generation off-thread, and this is the seam it
--   rejects a late candidate at.
admitChunk ∷ ChunkRequest → ChunkOwner → (ChunkOwner, AdmitOutcome)
admitChunk req owner =
    ( insertEntry (crKey req) EntryResident owner
    , if isCurrentGeneration req owner then AdmittedCurrent
                                       else AdmittedSuperseded )

-- | The payload left the tile map: the key is requestable again.
--
--   Only a RESIDENT key is evicted. An in-flight or requested key names
--   work that has not landed yet, and dropping its entry would let a
--   second generation of the same chunk start beside the first.
evictChunk ∷ ChunkKey → ChunkOwner → ChunkOwner
evictChunk key owner = case HM.lookup key (coEntries owner) of
    Just EntryResident → owner { coEntries = HM.delete key (coEntries owner) }
    _                  → owner

-- | Give up an in-flight claim without admitting anything.
--
--   The transient counterpart of 'admitChunk', and the reason a failed
--   or throwaway generation cannot leave a permanent in-flight entry
--   behind. Durable demand survives it: a claim that displaced a
--   requested key — or that a request arrived for while it was in
--   flight — falls back to requested, and only a claim nothing else
--   wanted returns the key to absent.
releaseChunk ∷ ChunkRequest → ChunkOwner → ChunkOwner
releaseChunk req owner = case HM.lookup key (coEntries owner) of
    Just (EntryInFlight _ HasDurableDemand) → insertEntry key EntryRequested owner
    Just (EntryInFlight _ NoDurableDemand)  →
        owner { coEntries = HM.delete key (coEntries owner) }
    _ → owner
  where key = crKey req

insertEntry ∷ ChunkKey → ChunkEntry → ChunkOwner → ChunkOwner
insertEntry key entry owner =
    owner { coEntries = HM.insert key entry (coEntries owner) }
