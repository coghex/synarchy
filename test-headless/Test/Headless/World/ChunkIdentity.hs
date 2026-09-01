{-# LANGUAGE Strict #-}
-- | ONE physical chunk is ONE key at ONE admission point (#2001).
--
--   Chunk demand used to reach the resident tile map through several
--   paths that shared no identity for \"this physical chunk\" and no
--   admission point: 'World.Tile.Types.WorldTileData' is keyed by a bare
--   'ChunkCoord' with no page and no canonicalisation; the camera loader
--   canonicalised with an unguarded 'wrapChunkCoordU' applied to a field
--   that is a SENTINEL on arena pages, while the init queue used the
--   guarded identity #1723 built; and \"is this chunk already being
--   worked on?\" had to be reconstructed from two 'IORef's snapshotted
--   in a documented order (#43's residue).
--
--   "World.Chunk.Residency" is the replacement, and this spec pins it:
--   the key, the generation epoch, and the request → in-flight →
--   resident → evicted state machine every producer now goes through.
--   Nearly all of it is PURE — the owner is a plain value — so the only
--   example that boots a page is the one asserting that re-initialising
--   a page id really does move its epoch.
module Test.Headless.World.ChunkIdentity (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (threadDelay)
import Data.IORef (readIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Engine.Core.State (EngineEnv)
import World.Chunk.Admit
    ( admitResidentChunks, claimChunkGeneration, claimedChunkCoord
    , pageChunkKey, publishSeedChunks, readChunkOwner
    , reconcileResidentChunks, registerChunkDemand, releaseEvictedChunks
    , withTransientChunkClaim )
import World.Generate.Constants (chunkLoadRadius)
import World.Chunk.Residency
    ( AdmitOutcome(..), ChunkGeneration, ChunkOwner, ChunkState(..)
    , ClaimKind(..), ClaimOutcome(..)
    , RequestOutcome(..), admitChunk, canonicalChunkCoord, chunkKeyFor
    , chunkOwnerGeneration, chunkOwnerSize, chunkStateOf, ckCoord, ckPage
    , claimChunk, crGeneration, crKey, emptyChunkOwner, evictChunk
    , isCurrentGeneration, mintChunkRequest, newChunkGeneration
    , releaseChunk, requestChunk )
import World.Chunk.Queue
    ( drainedLoadPhase, enqueueChunkRequest, initialChunkQueue
    , reconcileQueuedPhase, seedInitialQueue, settleDrainedPhase )
import World.Chunk.Types
    (ChunkCoord(..), LoadedChunk(..), chunkSize, wrapChunkCoordU)
import World.Generate.Coordinates (globalToChunk)
import World.Command.Types (WorldCommand(..))
import World.Generate.Types
    (WorldGenParams(..), defaultWorldGenParams, isArenaParams)
import World.Page.Types (WorldPageId(..))
import Data.Maybe (mapMaybe)
import World.State.Types (WorldState(..), LoadPhase(..), emptyWorldState)
import World.Tile.Types (WorldTileData(..), emptyWorldTileData, lookupChunk)
import World.Generate.Arena (generateFlatChunk)
import World.Edit.Apply (replayEdits)
import World.Fluid.Types (FluidType(..))
import World.Flora.Types (FloraId(..))
import World.Material.Id (MaterialId(..))
import World.Edit.Types
    ( WorldEdit(..), appendEdit, canonicalizeWorldEdits, emptyWorldEdits
    , shiftWorldEdit )
import Test.Headless.Harness
    (getWorldState, sendWorldCommand, waitForWorldInit)

-- | A deliberately small world: 8 chunks around u means the canonical
--   range is u ∈ [-4, 4), and chunk (4, 0) — u = 4 — is the first coord
--   past it. Same fixture "Test.Headless.World.ChunkQueueFrame" uses.
seamWorldSize ∷ Int
seamWorldSize = 8

-- | Wide enough that nothing in a load-radius box aliases.
wideWorldSize ∷ Int
wideWorldSize = 64

sizedParams ∷ Int → WorldGenParams
sizedParams n = defaultWorldGenParams { wgpWorldSize = n, wgpSeed = 1 }

-- | An arena's params: the empty timeline and seed 0 'isArenaParams'
--   recognises, plus the 100000 SENTINEL that stands in for a world
--   size ('World.Thread.Command.Init.handleWorldInitArenaCommand').
arenaParams ∷ WorldGenParams
arenaParams = defaultWorldGenParams
    { wgpSeed = 0, wgpWorldSize = 100000 }

pageA, pageB ∷ WorldPageId
pageA = WorldPageId "chunk_identity_a"
pageB = WorldPageId "chunk_identity_b"

-- | The alias a caller might hand the engine, and the key the chunk
--   actually lives under. (4,0): u = 4, v = 4 → wraps to u = -4 → (0,4).
aliasCoord, canonCoord ∷ ChunkCoord
aliasCoord = ChunkCoord 4 0
canonCoord = ChunkCoord 0 4

-- | A detached page with the given params: a real 'WorldState' with a
--   real residency owner, but no engine and no world thread, so nothing
--   loads chunks behind the example's back.
detachedPage ∷ WorldGenParams → IO WorldState
detachedPage params = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws) (Just params)
    pure ws

-- | Block until the page under this id is a DIFFERENT generation from
--   the one given, then let its own init finish.
--
--   'waitForWorldInit' alone cannot do this: a re-init keeps the page
--   id, and the OUTGOING page is still registered and already
--   'LoadDone', so a poll on the load phase resolves against the page
--   being replaced. The epoch is what distinguishes them, which is the
--   whole reason it exists.
waitForNewGeneration ∷ EngineEnv → WorldPageId → ChunkGeneration → Int
                     → IO ChunkGeneration
waitForNewGeneration env pid old timeoutSecs = go (timeoutSecs * 10)
  where
    go ∷ Int → IO ChunkGeneration
    go n
      | n ≤ 0 = error "waitForNewGeneration: page was never replaced"
      | otherwise = do
          mWs ← getWorldState env pid
          mGen ← traverse (fmap chunkOwnerGeneration . readChunkOwner) mWs
          case mGen of
              Just gen | gen ≢ old → pure gen
              _ → threadDelay 100000 ⌦ \_ → go (n - 1)

-- | A seed page's tile map: one chunk under its CANONICAL key, the
--   shape every seed site writes. Only the KEY matters to these
--   examples, so the payload is the cheapest real chunk available.
seedTileData ∷ ChunkCoord → WorldTileData
seedTileData coord = emptyWorldTileData
    { wtdChunks = HM.singleton canonical (generateFlatChunk canonical) }
  where canonical = canonicalChunkCoord (sizedParams seamWorldSize) coord

-- | The global tile coords an edit carries, for the shift table below.
editCoords ∷ WorldEdit → (Int, Int)
editCoords edit = case edit of
    WeDeleteTile gx gy           → (gx, gy)
    WeSetFluidTile gx gy _       → (gx, gy)
    WeAddTile gx gy _            → (gx, gy)
    WeSetSlope gx gy _ _         → (gx, gy)
    WeSetCell gx gy _ _          → (gx, gy)
    WeSetStructure gx gy _ _ _ _ → (gx, gy)
    WeClearStructure gx gy _     → (gx, gy)
    WeSetVeg gx gy _ _           → (gx, gy)
    WePlaceFlora gx gy _ _ _     → (gx, gy)
    WeSetFluidSnapshot gx gy _ _ → (gx, gy)
    WeClearFluidSnapshot gx gy   → (gx, gy)

-- | Is this the delete edit the ordering check looks for?
isDeleteEdit ∷ WorldEdit → Bool
isDeleteEdit (WeDeleteTile _ _) = True
isDeleteEdit _                  = False

-- | The four-valued residency of a coord on a page.
stateOf ∷ WorldGenParams → WorldPageId → ChunkCoord → ChunkOwner → ChunkState
stateOf params pid coord = chunkStateOf (pageChunkKey pid params coord)

spec ∷ SpecWith EngineEnv
spec = describe "canonical chunk identity" $ do

    it "gives one physical chunk one key, per page" $ \_ → do
        let seam = sizedParams seamWorldSize
            wide = sizedParams wideWorldSize
        -- Non-vacuity: the fixture really does name an out-of-range coord.
        wrapChunkCoordU seamWorldSize aliasCoord `shouldBe` canonCoord
        aliasCoord `shouldNotBe` canonCoord

        -- Both seam SPELLINGS of one physical chunk are one key.
        chunkKeyFor pageA seam aliasCoord
            `shouldBe` chunkKeyFor pageA seam canonCoord
        ckCoord (chunkKeyFor pageA seam aliasCoord) `shouldBe` canonCoord

        -- The same COORD on two pages is two keys, and each key knows
        -- which page it belongs to.
        chunkKeyFor pageA seam canonCoord
            `shouldNotBe` chunkKeyFor pageB seam canonCoord
        ckPage (chunkKeyFor pageB seam canonCoord) `shouldBe` pageB

        -- Identity is the PAGE's own: on a world with no seam between
        -- them these two coords stay two chunks.
        chunkKeyFor pageA wide aliasCoord
            `shouldNotBe` chunkKeyFor pageA wide canonCoord

    it "never wraps an arena coord through its sentinel world size" $ \_ → do
        -- The guard that a bare 'wrapChunkCoordU (wgpWorldSize params)'
        -- misses: an arena's wgpWorldSize is a 100000 SENTINEL, not an
        -- extent, so wrapping through it would fold every arena coord
        -- past u = ±50000 onto a different chunk.
        isArenaParams arenaParams `shouldBe` True
        let far = ChunkCoord 60000 0
        canonicalChunkCoord arenaParams far `shouldBe` far
        wrapChunkCoordU (wgpWorldSize arenaParams) far `shouldNotBe` far
        ckCoord (chunkKeyFor pageA arenaParams far) `shouldBe` far

        -- And the other guard: a non-positive world size has no seam.
        let unsized = defaultWorldGenParams { wgpWorldSize = 0, wgpSeed = 1 }
        isArenaParams unsized `shouldBe` False
        canonicalChunkCoord unsized aliasCoord `shouldBe` aliasCoord

    it "walks one key through request, in flight, resident and eviction" $ \_ → do
        gen ← newChunkGeneration
        let params = sizedParams seamWorldSize
            key    = chunkKeyFor pageA params aliasCoord
            owner0 = emptyChunkOwner gen
            req    = mintChunkRequest owner0 key

        chunkStateOf key owner0 `shouldBe` ChunkAbsent

        let (owner1, o1) = requestChunk key owner0
        o1 `shouldBe` RequestRegistered
        chunkStateOf key owner1 `shouldBe` ChunkRequested

        let (owner2, c1) = claimChunk DurableClaim req owner1
        c1 `shouldBe` ClaimGranted
        chunkStateOf key owner2 `shouldBe` ChunkInFlight

        -- Nobody generates a chunk twice.
        snd (claimChunk DurableClaim req owner2) `shouldBe` ClaimRefused

        let (owner3, a1) = admitChunk req owner2
        a1 `shouldBe` AdmittedCurrent
        chunkStateOf key owner3 `shouldBe` ChunkResident

        -- Eviction makes the key REQUESTABLE again, and a re-request
        -- registers as new demand rather than reporting satisfied work.
        let owner4 = evictChunk key owner3
        chunkStateOf key owner4 `shouldBe` ChunkAbsent
        chunkOwnerSize owner4 `shouldBe` 0
        let (owner5, o2) = requestChunk key owner4
        o2 `shouldBe` RequestRegistered
        chunkStateOf key owner5 `shouldBe` ChunkRequested

        -- Eviction only takes RESIDENT keys: dropping a requested or
        -- in-flight entry would let a second generation start beside
        -- the work already under way.
        chunkStateOf key (evictChunk key owner5) `shouldBe` ChunkRequested
        chunkStateOf key (evictChunk key owner2) `shouldBe` ChunkInFlight

    it "dedups an alias in every pending state, from either producer" $ \_ → do
        let params = sizedParams seamWorldSize
        ws ← detachedPage params
        let ownerNow = readChunkOwner ws
            aliasState o = stateOf params pageA aliasCoord o
            canonState o = stateOf params pageA canonCoord o

        -- REQUESTED: the init queue asks for one spelling, the camera
        -- path asks for the other. One entry, and the second request
        -- reports pending work rather than queueing a duplicate.
        registerChunkDemand ws pageA params [aliasCoord]
            `shouldReturn` [aliasCoord]
        registerChunkDemand ws pageA params [canonCoord] `shouldReturn` []
        o1 ← ownerNow
        chunkOwnerSize o1 `shouldBe` 1
        aliasState o1 `shouldBe` ChunkRequested
        canonState o1 `shouldBe` ChunkRequested

        -- Two aliases inside ONE request are one chunk too.
        registerChunkDemand ws pageA params [aliasCoord, canonCoord]
            `shouldReturn` []

        -- IN FLIGHT: the camera claims the key the init queue requested.
        -- That is the SAME demand — one entry, no second in-flight
        -- generation — and the request naming the other spelling still
        -- reports pending.
        claims ← claimChunkGeneration ws pageA params [canonCoord]
        map claimedChunkCoord claims `shouldBe` [canonCoord]
        o2 ← ownerNow
        chunkOwnerSize o2 `shouldBe` 1
        aliasState o2 `shouldBe` ChunkInFlight
        registerChunkDemand ws pageA params [aliasCoord] `shouldReturn` []
        -- A second claim naming the OTHER spelling is refused outright.
        claimChunkGeneration ws pageA params [aliasCoord] `shouldReturn` []

        -- RESIDENT: admitting through the one boundary leaves one entry,
        -- and an alias of a resident chunk is satisfied work.
        admitResidentChunks ws claims
        o3 ← ownerNow
        chunkOwnerSize o3 `shouldBe` 1
        aliasState o3 `shouldBe` ChunkResident
        registerChunkDemand ws pageA params [aliasCoord] `shouldReturn` []

        -- And an eviction naming the OTHER spelling still frees it.
        releaseEvictedChunks ws pageA params [canonCoord]
        o4 ← ownerNow
        chunkOwnerSize o4 `shouldBe` 0
        registerChunkDemand ws pageA params [aliasCoord]
            `shouldReturn` [aliasCoord]

    it "lets the camera claim an init-queue request without disturbing it" $ \_ → do
        -- 'World.Thread.worldTick' drains the init queue BEFORE the
        -- camera loader runs, and requirement 8 forbids changing that
        -- order. So a camera request meeting an already-requested key
        -- must CLAIM it, not skip it — and must leave the queue itself
        -- exactly as it found it.
        let params = sizedParams seamWorldSize
            queued = [ChunkCoord 1 1, aliasCoord, ChunkCoord (-1) (-1)]
        ws ← detachedPage params
        registerChunkDemand ws pageA params queued `shouldReturn` queued
        writeIORef (wsInitQueueRef ws) queued

        claims ← claimChunkGeneration ws pageA params [canonCoord]
        map claimedChunkCoord claims `shouldBe` [canonCoord]
        owner ← readChunkOwner ws
        chunkOwnerSize owner `shouldBe` 3
        stateOf params pageA aliasCoord owner `shouldBe` ChunkInFlight
        readIORef (wsInitQueueRef ws) `shouldReturn` queued

    it "keeps durable demand across a transient survey's claim" $ \_ → do
        -- The cursor's ore survey generates an unloaded chunk into a
        -- LOCAL map and drops it. Its claim must never become residency,
        -- and releasing it must not discard demand somebody else has.
        let params = sizedParams seamWorldSize
        ws ← detachedPage params

        -- A key nothing else wants returns to ABSENT.
        inside ← withTransientChunkClaim ws pageA params aliasCoord $
            stateOf params pageA aliasCoord ⊚ readChunkOwner ws
        inside `shouldBe` ChunkInFlight
        after1 ← readChunkOwner ws
        stateOf params pageA aliasCoord after1 `shouldBe` ChunkAbsent
        chunkOwnerSize after1 `shouldBe` 0

        -- A key the init queue had already REQUESTED comes back
        -- requested, not absent.
        registerChunkDemand ws pageA params [canonCoord]
            `shouldReturn` [canonCoord]
        _ ← withTransientChunkClaim ws pageA params aliasCoord (pure ())
        after2 ← readChunkOwner ws
        stateOf params pageA canonCoord after2 `shouldBe` ChunkRequested

        -- And a request that arrives WHILE the survey is generating is
        -- retained: the survey drops its chunk, the durable work stands.
        ws2 ← detachedPage params
        _ ← withTransientChunkClaim ws2 pageA params aliasCoord $
            registerChunkDemand ws2 pageA params [canonCoord]
        after3 ← readChunkOwner ws2
        stateOf params pageA aliasCoord after3 `shouldBe` ChunkRequested
        chunkOwnerSize after3 `shouldBe` 1

        -- A release only ever undoes an IN-FLIGHT claim; it cannot
        -- demote a resident chunk.
        gen ← newChunkGeneration
        let key      = chunkKeyFor pageA params aliasCoord
            owner0   = emptyChunkOwner gen
            req      = mintChunkRequest owner0 key
            resident = fst (admitChunk req owner0)
        chunkStateOf key (releaseChunk req resident) `shouldBe` ChunkResident

    it "tags every request with the page generation it was made against" $ \_ → do
        let params = sizedParams seamWorldSize
            coord  = ChunkCoord 1 1
        wsA ← detachedPage params
        wsB ← detachedPage params

        ownerA ← readChunkOwner wsA
        ownerB ← readChunkOwner wsB
        -- Each WorldState carries its OWN epoch: a shared default would
        -- make a replacement page indistinguishable from the page it
        -- replaced, which is the one thing this value exists to detect.
        chunkOwnerGeneration ownerA
            `shouldNotBe` chunkOwnerGeneration ownerB

        [reqA] ← claimChunkGeneration wsA pageA params [coord]
        -- The tag names the page AND the epoch, and it stays attached
        -- through the admission boundary.
        ckPage (crKey reqA) `shouldBe` pageA
        crGeneration reqA `shouldBe` chunkOwnerGeneration ownerA
        isCurrentGeneration reqA ownerA `shouldBe` True

        -- A result carrying a superseded page generation is recognisable
        -- as such at admission. Nothing acts on it in this slice —
        -- generation is synchronous on the world thread — but CRS-11
        -- rejects an out-of-date asynchronous candidate here.
        isCurrentGeneration reqA ownerB `shouldBe` False
        snd (admitChunk reqA ownerB) `shouldBe` AdmittedSuperseded
        snd (admitChunk reqA ownerA) `shouldBe` AdmittedCurrent

    it "moves a page's epoch when that page id is re-initialized" $ \env → do
        -- The live half: a page id is REUSED constantly (@main_world@ on
        -- every Exit to Menu, an arena replacing it, a load republishing
        -- the session), and each of those builds a fresh WorldState. The
        -- epoch has to move with it, or a page-id comparison is all a
        -- late result would have to go on.
        let pid = WorldPageId "chunk_identity_epoch"
        sendWorldCommand env (WorldInit pid 42 seamWorldSize 3 Nothing)
        ws1 ← waitForWorldInit env pid 300
        gen1 ← chunkOwnerGeneration ⊚ readChunkOwner ws1

        -- The centre chunk reached the resident set through the one
        -- admission boundary, under its canonical key.
        Just params ← readIORef (wsGenParamsRef ws1)
        owner1 ← readChunkOwner ws1
        stateOf params pid (ChunkCoord 0 0) owner1 `shouldBe` ChunkResident

        sendWorldCommand env (WorldInit pid 43 seamWorldSize 3 Nothing)
        gen2 ← waitForNewGeneration env pid gen1 300
        gen2 `shouldNotBe` gen1
        -- Let the replacement finish generating rather than leaving a
        -- half-built page racing the rest of the suite.
        ws2 ← waitForWorldInit env pid 300
        (chunkOwnerGeneration ⊚ readChunkOwner ws2) `shouldReturn` gen2

    it "holds a seed claim across generation, then admits before publishing" $ \_ → do
        -- The seed paths (a fresh world's centre, a restored centre, an
        -- arena's whole chunk set) build their payloads before the page
        -- can take a request, and still reach the resident set through
        -- the same admission boundary — owner first, payloads second.
        --
        -- The order is what matters. A fresh world's page is registered
        -- and given its generation params BEFORE its centre is built, so
        -- the Lua thread can call world.loadChunksInRegion mid-seed. The
        -- example below reconstructs both interleavings deterministically
        -- rather than racing them.
        let params = sizedParams seamWorldSize
        ws ← detachedPage params
        claims ← claimChunkGeneration ws pageA params [aliasCoord]

        -- MID-GENERATION. A fresh page is registered and carries its
        -- generation params before its centre exists, so a
        -- world.loadChunksInRegion naming that centre can land here,
        -- while generateChunk is still running. The claim reports it as
        -- pending — which it is — so the call neither queues nor counts a
        -- chunk this page is already producing.
        enqueueChunkRequest pageA ws [canonCoord] `shouldReturn` 0
        readIORef (wsInitQueueRef ws) `shouldReturn` []
        midOwner ← readChunkOwner ws
        stateOf params pageA canonCoord midOwner `shouldBe` ChunkInFlight

        publishSeedChunks ws claims (seedTileData aliasCoord)
        owner ← readChunkOwner ws
        chunkOwnerSize owner `shouldBe` 1
        stateOf params pageA canonCoord owner `shouldBe` ChunkResident
        registerChunkDemand ws pageA params [canonCoord] `shouldReturn` []
        -- The payload landed under the canonical key, not the alias the
        -- caller named.
        td ← readIORef (wsTilesRef ws)
        (lcCoord ⊚ lookupChunk canonCoord td) `shouldBe` Just canonCoord

        -- Both windows the lifecycle closes, reconstructed rather than
        -- raced. UNCLAIMED during generation: the owner says absent, so
        -- the same request queues and COUNTS work already under way.
        wsUnclaimed ← detachedPage params
        enqueueChunkRequest pageA wsUnclaimed [canonCoord] `shouldReturn` 1

        -- PAYLOAD BEFORE OWNER: the chunk is resident and the owner still
        -- says absent, so the request queues and counts a chunk the page
        -- already holds.
        wsBad ← detachedPage params
        writeIORef (wsTilesRef wsBad) (seedTileData aliasCoord)
        enqueueChunkRequest pageA wsBad [canonCoord] `shouldReturn` 1

        -- Claim, then publish owner-first: satisfied work, nothing queued.
        wsGood ← detachedPage params
        goodClaims ← claimChunkGeneration wsGood pageA params [aliasCoord]
        publishSeedChunks wsGood goodClaims (seedTileData aliasCoord)
        enqueueChunkRequest pageA wsGood [canonCoord] `shouldReturn` 0
        readIORef (wsInitQueueRef wsGood) `shouldReturn` []

    it "schedules a request that lands behind a transient claim" $ \_ → do
        -- The loss this closes: the cursor's ore survey holds a
        -- TRANSIENT claim, a world.loadChunksInRegion for the same chunk
        -- arrives, and the owner absorbs it as "already pending". The
        -- survey then throws its chunk away and releases the key to
        -- requested — with nothing queued to generate it. Nothing scans
        -- the owner for unscheduled demand, so the call reported 0,
        -- world.waitForChunks reported completion, and the chunk never
        -- became resident.
        let params = sizedParams seamWorldSize
        ws ← detachedPage params

        queued ← withTransientChunkClaim ws pageA params aliasCoord $
            enqueueChunkRequest pageA ws [canonCoord]
        -- Counted as real work, and actually on the queue.
        queued `shouldBe` 1
        readIORef (wsInitQueueRef ws) `shouldReturn` [canonCoord]

        -- A SECOND request during the same claim is genuinely pending:
        -- the first one already scheduled it.
        secondDuring ← withTransientChunkClaim ws pageA params aliasCoord $
            enqueueChunkRequest pageA ws [aliasCoord]
        secondDuring `shouldBe` 0

        -- Released to requested, still queued exactly once — which is
        -- the owner's invariant: a requested key is on the init queue.
        owner ← readChunkOwner ws
        stateOf params pageA aliasCoord owner `shouldBe` ChunkRequested
        readIORef (wsInitQueueRef ws) `shouldReturn` [canonCoord]

        -- A DURABLE claim absorbs a request instead, because it is going
        -- to admit its result: queueing behind it would be duplicate
        -- work, not lost work.
        ws2 ← detachedPage params
        _ ← claimChunkGeneration ws2 pageA params [aliasCoord]
        enqueueChunkRequest pageA ws2 [canonCoord] `shouldReturn` 0
        readIORef (wsInitQueueRef ws2) `shouldReturn` []

    it "keeps a request that arrives against an already-queued page" $ \_ → do
        -- The loss this closes: world init and saved-page restore used
        -- to WRITE their initial box over the queue. A page is
        -- registered before its initial box is queued, so a
        -- loadChunksInRegion accepted in that window was registered on
        -- the owner and then had its queue entries thrown away — leaving
        -- coords requested for ever and every later request for them
        -- deduplicated away. 'seedInitialQueue' appends, so the two sets
        -- coexist.
        --
        -- The PAGE here is a bare wide world, so its box does not alias
        -- against itself and the arithmetic below is the plain one.
        let params = sizedParams wideWorldSize
            centre  = ChunkCoord 0 0
            (box, boxTotal) = initialChunkQueue
                                  (canonicalChunkCoord params) centre
            outside = [ ChunkCoord (2 * chunkLoadRadius + 4) 0
                      , ChunkCoord (2 * chunkLoadRadius + 5) 0 ]

        -- With nothing outstanding, the phase pair is exactly the box's
        -- own: every chunk but the synchronously loaded centre remains,
        -- and the total is the box's physical total. The call INSTALLS
        -- that phase rather than returning it for the caller to write —
        -- an interval between the two is one a concurrent region request
        -- can be lost in.
        wsPlain ← detachedPage params
        seedInitialQueue pageA wsPlain params box
            `shouldReturn` (length box, boxTotal)
        readIORef (wsLoadPhaseRef wsPlain)
            `shouldReturn` LoadPhase2 (length box) boxTotal

        -- Now the racing case: two off-box requests accepted before the
        -- box is queued.
        ws ← detachedPage params
        enqueueChunkRequest pageA ws outside `shouldReturn` 2
        (remaining, total) ← seedInitialQueue pageA ws params box

        -- Both sets are on the queue, the outstanding ones first.
        readIORef (wsInitQueueRef ws) `shouldReturn` (outside ⧺ box)
        remaining `shouldBe` length box + 2

        -- ...and the total ACCOUNTS for them. world.getInitProgress
        -- reports (total - remaining) completed, so a total left at the
        -- box's own 'boxTotal' would surface as NEGATIVE progress
        -- through the public API.
        total `shouldSatisfy` (> boxTotal)
        total - remaining `shouldBe` 1
        total `shouldSatisfy` (≥ remaining)
        readIORef (wsLoadPhaseRef ws) `shouldReturn` LoadPhase2 remaining total

        -- A prior request INSIDE the box is not counted twice.
        wsOverlap ← detachedPage params
        case filter (≢ centre) box of
            [] → expectationFailure "fixture box holds only its centre"
            (inBox : _) → do
                enqueueChunkRequest pageA wsOverlap [inBox] `shouldReturn` 1
                seedInitialQueue pageA wsOverlap params box
                    `shouldReturn` (length box, boxTotal)

    it "never leaves a requested key off the page's work list" $ \_ → do
        -- The invariant all three losses violated, stated directly:
        -- every key the owner holds as requested is on the init queue.
        let params = sizedParams seamWorldSize
        ws ← detachedPage params
        _ ← enqueueChunkRequest pageA ws [ChunkCoord 1 1, aliasCoord]
        _ ← withTransientChunkClaim ws pageA params (ChunkCoord 2 2) $
                enqueueChunkRequest pageA ws [ChunkCoord 2 2]
        _ ← enqueueChunkRequest pageA ws [ChunkCoord 3 3, canonCoord]
        owner ← readChunkOwner ws
        queue ← readIORef (wsInitQueueRef ws)
        let queuedKeys = HS.fromList (map (pageChunkKey pageA params) queue)
            requested = [ k | k ← map (pageChunkKey pageA params) allTouched
                            , chunkStateOf k owner ≡ ChunkRequested ]
            allTouched = [ ChunkCoord 1 1, aliasCoord, canonCoord
                         , ChunkCoord 2 2, ChunkCoord 3 3 ]
        -- Non-vacuity: there really are requested keys to check.
        requested `shouldNotBe` []
        [ k | k ← requested, not (HS.member k queuedKeys) ] `shouldBe` []

    it "leaves demand alone when a claim is refused" $ \_ → do
        -- The init-queue drain pulls a batch off the queue and claims
        -- it. A refusal means somebody else is generating that chunk, so
        -- the demand is untouched — and the drain has to put the coord
        -- back rather than drop it, or the owner keeps a request nothing
        -- is scheduled to meet.
        let params = sizedParams seamWorldSize
        ws ← detachedPage params
        _ ← enqueueChunkRequest pageA ws [aliasCoord]
        first ← claimChunkGeneration ws pageA params [aliasCoord]
        length first `shouldBe` 1

        before ← readChunkOwner ws
        claimChunkGeneration ws pageA params [canonCoord] `shouldReturn` []
        after ← readChunkOwner ws
        after `shouldBe` before
        stateOf params pageA aliasCoord after `shouldBe` ChunkInFlight
        readIORef (wsInitQueueRef ws) `shouldReturn` [aliasCoord]

    it "settles a request the page turns out to already hold" $ \_ → do
        -- The drain's reconciliation, and the reason the owner is
        -- self-healing: a request registered against a key the page in
        -- fact holds (it raced an eviction that then did not happen, or
        -- landed while the tile map still had the payload) would sit
        -- requested for ever once its queue entry is dropped.
        let params = sizedParams seamWorldSize
        ws ← detachedPage params
        _ ← enqueueChunkRequest pageA ws [aliasCoord]
        stale ← readChunkOwner ws
        stateOf params pageA aliasCoord stale `shouldBe` ChunkRequested

        reconcileResidentChunks ws pageA params [canonCoord]
        settled ← readChunkOwner ws
        stateOf params pageA aliasCoord settled `shouldBe` ChunkResident
        chunkOwnerSize settled `shouldBe` 1
        enqueueChunkRequest pageA ws [aliasCoord] `shouldReturn` 0

    it "never lets init progress run negative when a region is appended" $ \_ → do
        -- world.getInitProgress reports (total - remaining) completed, so
        -- a phase whose remaining outruns its total surfaces as NEGATIVE
        -- progress through the public API. Appending work completes none
        -- of it, so a request made during LoadPhase2 raises the total by
        -- exactly what it raises the remaining count by.
        let params = sizedParams wideWorldSize
            region = [ ChunkCoord cx 40 | cx ← [0 .. 11] ]
        ws ← detachedPage params
        writeIORef (wsLoadPhaseRef ws) (LoadPhase2 24 25)

        queued ← enqueueChunkRequest pageA ws region
        queued `shouldBe` length region
        phase ← readIORef (wsLoadPhaseRef ws)
        phase `shouldBe` LoadPhase2 (24 + length region) (25 + length region)
        -- Non-vacuity: leaving the total alone really would go negative.
        length region `shouldSatisfy` (> 25 - 24)
        case phase of
            LoadPhase2 remaining total → do
                total - remaining `shouldBe` 1
                total `shouldSatisfy` (≥ remaining)
            other → expectationFailure ("expected LoadPhase2, got " ⧺ show other)

        -- A request that adds no NEW physical chunk moves neither.
        again ← enqueueChunkRequest pageA ws region
        again `shouldBe` 0
        readIORef (wsLoadPhaseRef ws) `shouldReturn` phase

        -- A page still in SETUP is left alone: its box has not been
        -- queued yet, and 'seedInitialQueue' installs the pair from the
        -- whole queue — this request included — when it gets there.
        -- (A finished load is the case that DOES reopen; see
        -- "reopens a finished load when a region is appended".)
        wsSetup ← detachedPage params
        writeIORef (wsLoadPhaseRef wsSetup) (LoadPhase1 3 8)
        _ ← enqueueChunkRequest pageA wsSetup region
        readIORef (wsLoadPhaseRef wsSetup) `shouldReturn` LoadPhase1 3 8
        -- ...and the seed then accounts for it.
        let (box, _) = initialChunkQueue (canonicalChunkCoord params)
                                         (ChunkCoord 0 0)
        (seeded, seededTotal) ← seedInitialQueue pageA wsSetup params box
        seeded `shouldBe` length region + length box
        seededTotal `shouldBe` seeded + 1

    it "merges a concurrent total bump instead of clobbering it" $ \_ → do
        -- Two threads write wsLoadPhaseRef: the world thread on every
        -- drain, and the Lua thread whenever a region is appended during
        -- LoadPhase2. The drain used to read, compute, and write, so an
        -- increment landing in between was lost — permanently, because
        -- the total is exactly what later ticks carry forward.
        -- 'drainedLoadPhase' is applied inside one atomicModifyIORef',
        -- and takes the total from the value it is replacing.
        let boxTotal = 25

        -- Ordinary tick: eight chunks drained, total carried forward.
        drainedLoadPhase boxTotal 16 (LoadPhase2 24 boxTotal)
            `shouldBe` LoadPhase2 16 boxTotal

        -- THE RACE. The drain observed rest = 16 against a recorded 25,
        -- then Lua appended ten chunks and atomically installed
        -- LoadPhase2 34 35. The merge keeps that 35; the old
        -- read-compute-write wrote 25 back and lost it for good.
        drainedLoadPhase boxTotal 16 (LoadPhase2 34 35)
            `shouldBe` LoadPhase2 16 35

        -- The remaining count is the DRAIN's, never the recorded one:
        -- that figure is the previous tick's and is always the larger,
        -- so taking it would freeze progress. A concurrent append
        -- understates it for one tick and the next drain corrects it.
        drainedLoadPhase boxTotal 16 (LoadPhase2 34 35)
            `shouldNotBe` LoadPhase2 34 35

        -- Floored throughout: getInitProgress reports total - remaining,
        -- so the pair may never run the other way round.
        drainedLoadPhase boxTotal 26 (LoadPhase2 24 boxTotal)
            `shouldBe` LoadPhase2 26 26
        drainedLoadPhase boxTotal 30 LoadDone `shouldBe` LoadPhase2 30 30
        drainedLoadPhase boxTotal 12 LoadIdle `shouldBe` LoadPhase2 12 boxTotal

        -- An empty queue ends the phase whatever was recorded.
        drainedLoadPhase boxTotal 0 (LoadPhase2 8 boxTotal) `shouldBe` LoadDone

        -- Non-vacuity: every pair above reports non-negative progress.
        let completed ph = case ph of
                LoadPhase2 remaining total → Just (total - remaining)
                _                          → Nothing
            pairs = [ drainedLoadPhase boxTotal r ph
                    | r ← [0 .. 40]
                    , ph ← [ LoadIdle, LoadDone, LoadPhase2 24 boxTotal
                           , LoadPhase2 34 35, LoadPhase2 0 0 ] ]
        filter (< 0) (mapMaybe completed pairs) `shouldBe` []

    it "keeps an early centre request out of the initial queue" $ \_ → do
        -- A fresh page's generation params are published in step 3 but
        -- its centre is not generated until step 6, and publishing the
        -- params is what makes world.loadChunksInRegion able to act on
        -- the page at all. Init claims the centre BEFORE that write, so
        -- any request able to see the page already sees the centre in
        -- flight.
        let params = sizedParams wideWorldSize
            centre = ChunkCoord 0 0
            (box, boxTotal) = initialChunkQueue
                                  (canonicalChunkCoord params) centre

        wsClaimed ← detachedPage params
        _ ← claimChunkGeneration wsClaimed pageA params [centre]
        -- Neither queued nor counted: this page is already producing it.
        enqueueChunkRequest pageA wsClaimed [centre] `shouldReturn` 0
        seedInitialQueue pageA wsClaimed params box
            `shouldReturn` (length box, boxTotal)
        readIORef (wsInitQueueRef wsClaimed) `shouldReturn` box

        -- Unclaimed, the same request queues the centre and the initial
        -- phase then overstates the work by exactly that entry — for a
        -- chunk the seed is about to make resident, so the queue would
        -- carry an entry no generation will ever satisfy.
        wsUnclaimed ← detachedPage params
        enqueueChunkRequest pageA wsUnclaimed [centre] `shouldReturn` 1
        (badRemaining, badTotal) ←
            seedInitialQueue pageA wsUnclaimed params box
        badRemaining `shouldBe` length box + 1
        badTotal `shouldSatisfy` (> boxTotal)
        readIORef (wsInitQueueRef wsUnclaimed) `shouldReturn` (centre : box)

    it "never leaves LoadDone standing over a non-empty queue" $ \_ → do
        -- The phase WRITE is atomic, but the queue length it is computed
        -- from is a separate read. On the final batch a snapshot of an
        -- empty queue becomes LoadDone, and a region appended in that
        -- window would leave world.getInitProgress — and every waiter
        -- polling for the terminal phase — reporting a load that has not
        -- happened. settleDrainedPhase rechecks that conclusion.
        let params = sizedParams wideWorldSize
            fallback = 25
            pending = [ChunkCoord 3 3, ChunkCoord 4 4]

        wsBusy ← detachedPage params
        writeIORef (wsInitQueueRef wsBusy) pending
        writeIORef (wsLoadPhaseRef wsBusy) (LoadPhase2 10 fallback)
        settleDrainedPhase wsBusy fallback
        readIORef (wsLoadPhaseRef wsBusy)
            `shouldReturn` LoadPhase2 (length pending) fallback

        wsDone ← detachedPage params
        writeIORef (wsInitQueueRef wsDone) []
        writeIORef (wsLoadPhaseRef wsDone) (LoadPhase2 1 fallback)
        settleDrainedPhase wsDone fallback
        readIORef (wsLoadPhaseRef wsDone) `shouldReturn` LoadDone

        -- The invariant, checked against the queue rather than assumed:
        -- a settled page reports done only when there is nothing left.
        forM_ [wsBusy, wsDone] $ \ws → do
            settleDrainedPhase ws fallback
            phase ← readIORef (wsLoadPhaseRef ws)
            queue ← readIORef (wsInitQueueRef ws)
            when (phase ≡ LoadDone) $ queue `shouldBe` []

    it "reopens a finished load when a region is appended" $ \_ → do
        -- The drain settles the phase from a queue length it read a
        -- moment earlier, so an append can always land after that read
        -- and leave LoadDone standing over real work — world.waitForInit
        -- and world.getInitProgress would report a load that has not
        -- happened. The appender reopens the phase, which is the other
        -- half of settleDrainedPhase's recheck; between them the two
        -- orderings converge.
        let params = sizedParams wideWorldSize
            region = [ ChunkCoord cx 60 | cx ← [0 .. 5] ]
        ws ← detachedPage params
        writeIORef (wsLoadPhaseRef ws) LoadDone

        queued ← enqueueChunkRequest pageA ws region
        queued `shouldBe` length region
        -- The drain reached LoadDone with an empty queue, so this work is
        -- the whole of the new load and none of it is done.
        readIORef (wsLoadPhaseRef ws)
            `shouldReturn` LoadPhase2 (length region) (length region)

        -- The OTHER ordering: the drain finishes last, and its recheck
        -- sees the work this append left behind and reopens the phase.
        wsRace ← detachedPage params
        _ ← enqueueChunkRequest pageA wsRace region
        writeIORef (wsLoadPhaseRef wsRace) LoadDone   -- a stale settle
        settleDrainedPhase wsRace 25
        phase ← readIORef (wsLoadPhaseRef wsRace)
        phase `shouldNotBe` LoadDone
        case phase of
            LoadPhase2 remaining total → do
                remaining `shouldBe` length region
                total `shouldSatisfy` (≥ remaining)
            other → expectationFailure ("expected LoadPhase2, got " ⧺ show other)

    it "seeds a phase pair the queue has stopped moving under" $ \_ → do
        -- seedInitialQueue reads the queue and writes the phase in two
        -- steps, so a region appended in between would be lost from the
        -- installed total: it sees LoadPhase1 and correctly leaves the
        -- phase alone. The pair therefore comes back STABLE — read,
        -- write, read again — and once LoadPhase2 is published an
        -- appending enqueueChunkRequest raises the total itself.
        let params = sizedParams wideWorldSize
            centre = ChunkCoord 0 0
            (box, boxTotal) = initialChunkQueue
                                  (canonicalChunkCoord params) centre
            region = [ ChunkCoord cx 60 | cx ← [0 .. 9] ]
        ws ← detachedPage params
        (remaining, total) ← seedInitialQueue pageA ws params box
        (remaining, total) `shouldBe` (length box, boxTotal)

        -- The pair it returned is the pair it installed, and it agrees
        -- with the queue it was derived from.
        readIORef (wsLoadPhaseRef ws) `shouldReturn` LoadPhase2 remaining total
        (length ⊚ readIORef (wsInitQueueRef ws)) `shouldReturn` remaining

        -- From here the appender owns the total, so a later region is
        -- accounted for without another seed pass.
        added ← enqueueChunkRequest pageA ws region
        added `shouldBe` length region
        readIORef (wsLoadPhaseRef ws)
            `shouldReturn` LoadPhase2 (remaining + added) (total + added)
        (length ⊚ readIORef (wsInitQueueRef ws))
            `shouldReturn` (remaining + added)

    it "does not reopen a load the world thread already finished" $ \_ → do
        -- The append and the phase update that accounts for it are two
        -- steps, and the world thread can drain the appended work to
        -- completion in between — so the update would reopen LoadPhase2
        -- over an empty queue. drainInitQueues returns immediately for an
        -- empty queue, so nothing would ever settle it back:
        -- world.waitForInit would block for ever while
        -- world.waitForChunks reported nothing remaining.
        let params = sizedParams wideWorldSize

        -- The preempted state, constructed directly: the phase was
        -- reopened for six chunks the drain has already finished.
        wsDrained ← detachedPage params
        writeIORef (wsInitQueueRef wsDrained) []
        writeIORef (wsLoadPhaseRef wsDrained) (LoadPhase2 6 6)
        reconcileQueuedPhase wsDrained
        readIORef (wsLoadPhaseRef wsDrained) `shouldReturn` LoadDone

        -- Work genuinely outstanding is left alone.
        wsBusy ← detachedPage params
        writeIORef (wsInitQueueRef wsBusy) [ChunkCoord 5 5]
        writeIORef (wsLoadPhaseRef wsBusy) (LoadPhase2 6 6)
        reconcileQueuedPhase wsBusy
        readIORef (wsLoadPhaseRef wsBusy) `shouldReturn` LoadPhase2 6 6

        -- A page still in SETUP is never forced done, however empty its
        -- queue is: seedInitialQueue has not put its box on yet, so an
        -- empty queue there is not a finished load, and retiring it
        -- would report a world ready before it has any chunks at all.
        wsSetup ← detachedPage params
        writeIORef (wsInitQueueRef wsSetup) []
        writeIORef (wsLoadPhaseRef wsSetup) (LoadPhase1 3 8)
        reconcileQueuedPhase wsSetup
        readIORef (wsLoadPhaseRef wsSetup) `shouldReturn` LoadPhase1 3 8

        -- And the invariant the appender now maintains end to end: after
        -- a request, the phase reports a load in progress only when the
        -- queue actually holds work.
        wsLive ← detachedPage params
        writeIORef (wsLoadPhaseRef wsLive) LoadDone
        _ ← enqueueChunkRequest pageA wsLive [ChunkCoord 7 7]
        livePhase ← readIORef (wsLoadPhaseRef wsLive)
        liveQueue ← readIORef (wsInitQueueRef wsLive)
        livePhase `shouldBe` LoadPhase2 1 1
        liveQueue `shouldBe` [ChunkCoord 7 7]
        -- Draining it by hand and reconciling retires the phase again.
        writeIORef (wsInitQueueRef wsLive) []
        reconcileQueuedPhase wsLive
        readIORef (wsLoadPhaseRef wsLive) `shouldReturn` LoadDone

    it "carries a concurrent total across the settle's own retry" $ \_ → do
        -- The settle concludes "done" from a queue it read empty, and
        -- that write ERASES the recorded total. If an append landed in
        -- between, the recheck reruns — and a rerun that re-read the
        -- phase would find LoadDone and fall back to the box-shaped
        -- figure, permanently underreporting a load the append had
        -- already enlarged. The retry carries the total it replaced.
        let params = sizedParams wideWorldSize
            boxFallback = 25

        -- The state that ordering leaves behind: one chunk queued, and a
        -- phase the appender had already raised to 26.
        ws ← detachedPage params
        writeIORef (wsInitQueueRef ws) [ChunkCoord 9 9]
        writeIORef (wsLoadPhaseRef ws) (LoadPhase2 2 26)
        settleDrainedPhase ws boxFallback
        readIORef (wsLoadPhaseRef ws) `shouldReturn` LoadPhase2 1 26

        -- Non-vacuity: the box-shaped fallback really is the smaller
        -- figure, so falling back to it would have lost a chunk of work.
        boxFallback `shouldSatisfy` (< 26)

        -- And the pure step it is built from never invents a total when
        -- one is recorded.
        drainedLoadPhase boxFallback 1 (LoadPhase2 2 26)
            `shouldBe` LoadPhase2 1 26

    it "publishes a relabelled seed under the canonical key" $ \_ → do
        -- Whatever frame it was generated in, the chunk reaches the owner
        -- and the tile map under ONE key, so a later request for either
        -- spelling is satisfied rather than generating a second payload
        -- for the same physical place.
        let params = sizedParams seamWorldSize
        ws ← detachedPage params
        claims ← claimChunkGeneration ws pageA params [aliasCoord]
        map claimedChunkCoord claims `shouldBe` [canonCoord]
        publishSeedChunks ws claims (seedTileData aliasCoord)

        owner ← readChunkOwner ws
        chunkOwnerSize owner `shouldBe` 1
        stateOf params pageA aliasCoord owner `shouldBe` ChunkResident
        td ← readIORef (wsTilesRef ws)
        (lcCoord ⊚ lookupChunk canonCoord td) `shouldBe` Just canonCoord
        lookupChunk aliasCoord td `shouldBe` Nothing
        -- Neither spelling is work any more.
        enqueueChunkRequest pageA ws [aliasCoord] `shouldReturn` 0
        enqueueChunkRequest pageA ws [canonCoord] `shouldReturn` 0

    it "confirms an empty queue before publishing a finished load" $ \_ → do
        -- LoadDone is the one conclusion that is wrong to publish even
        -- briefly: world.getInitProgress and every waiter polling for the
        -- terminal phase would report a load that has not happened. Both
        -- reconciliations therefore re-read the queue before concluding
        -- it, so the value is never published from a snapshot an append
        -- has already invalidated — and both recheck afterwards too,
        -- because two IORefs cannot be read as one.
        let params = sizedParams wideWorldSize
            fallback = 25
            pending = [ChunkCoord 2 2, ChunkCoord 3 3]

        -- Work present: neither reconciliation retires the phase.
        wsBusy ← detachedPage params
        writeIORef (wsInitQueueRef wsBusy) pending
        writeIORef (wsLoadPhaseRef wsBusy) (LoadPhase2 5 fallback)
        reconcileQueuedPhase wsBusy
        readIORef (wsLoadPhaseRef wsBusy) `shouldReturn` LoadPhase2 5 fallback
        settleDrainedPhase wsBusy fallback
        readIORef (wsLoadPhaseRef wsBusy)
            `shouldReturn` LoadPhase2 (length pending) fallback

        -- Genuinely empty: both conclude done.
        wsIdle ← detachedPage params
        writeIORef (wsInitQueueRef wsIdle) []
        writeIORef (wsLoadPhaseRef wsIdle) (LoadPhase2 1 fallback)
        reconcileQueuedPhase wsIdle
        readIORef (wsLoadPhaseRef wsIdle) `shouldReturn` LoadDone

        wsIdle2 ← detachedPage params
        writeIORef (wsInitQueueRef wsIdle2) []
        writeIORef (wsLoadPhaseRef wsIdle2) (LoadPhase2 1 fallback)
        settleDrainedPhase wsIdle2 fallback
        readIORef (wsLoadPhaseRef wsIdle2) `shouldReturn` LoadDone

        -- The invariant both maintain, checked against the queue rather
        -- than assumed: a page reports done only with nothing left.
        forM_ [wsBusy, wsIdle, wsIdle2] $ \ws → do
            phase ← readIORef (wsLoadPhaseRef ws)
            queue ← readIORef (wsInitQueueRef ws)
            when (phase ≡ LoadDone) $ queue `shouldBe` []

    it "normalizes a seam save's edit log into the canonical frame" $ \_ → do
        -- A save written by the old restore path holds entries keyed to
        -- a seam ALIAS — every settled-fluid snapshot included, since
        -- appendFluidSnapshot keys by the chunk's own lcCoord — and
        -- applyEdit refuses an edit whose coords do not belong to the
        -- chunk it is handed. Normalizing the log ONCE at load is what
        -- keeps them replaying for the life of the session: the restored
        -- centre is only the first of many loads, and the streaming
        -- loader regenerates that chunk canonically every time it is
        -- evicted and comes back.
        let params = sizedParams seamWorldSize
            canon = canonicalChunkCoord params
            ChunkCoord ax ay = aliasCoord
            aliasGX = ax * chunkSize + 6
            aliasGY = ay * chunkSize + 2
            saved = appendEdit aliasCoord (WeDeleteTile aliasGX aliasGY)
                        (appendEdit aliasCoord
                            (WeSetFluidSnapshot aliasGX aliasGY River 3)
                            emptyWorldEdits)
        canon aliasCoord `shouldBe` canonCoord
        fst (globalToChunk aliasGX aliasGY) `shouldBe` aliasCoord

        let normalized = canonicalizeWorldEdits canon saved
        HM.member aliasCoord normalized `shouldBe` False
        -- The COORDINATES moved with the key, so the entries now belong
        -- to the canonical chunk...
        [ fst (globalToChunk gx gy)
            | WeDeleteTile gx gy ← HM.lookupDefault [] canonCoord normalized ]
            `shouldBe` [canonCoord]
        -- ...addressing the same column, because a u-wrap displaces a
        -- chunk by a whole number of chunks.
        [ snd (globalToChunk gx gy)
            | WeDeleteTile gx gy ← HM.lookupDefault [] canonCoord normalized ]
            `shouldBe` [snd (globalToChunk aliasGX aliasGY)]
        -- Order is preserved: snapshot first, delete second, as saved.
        map isDeleteEdit (HM.lookupDefault [] canonCoord normalized)
            `shouldBe` [False, True]

        -- The regression itself: a freshly generated canonical chunk —
        -- exactly what the streaming loader produces on every reload —
        -- now replays them, and would not have before.
        let fresh = generateFlatChunk canonCoord
        lcTiles (replayEdits normalized fresh) `shouldNotBe` lcTiles fresh
        lcTiles (replayEdits saved fresh) `shouldBe` lcTiles fresh

        -- An already-canonical log is returned UNCHANGED, so an ordinary
        -- save is not rebuilt, reordered, or otherwise touched.
        let plain = appendEdit canonCoord (WeDeleteTile 3 67) emptyWorldEdits
        canonicalizeWorldEdits canon plain `shouldBe` plain
        canonicalizeWorldEdits (canonicalChunkCoord (sizedParams wideWorldSize))
                               saved `shouldBe` saved

    it "shifts every edit constructor by the same whole-chunk offset" $ \_ → do
        -- The shift is uniform: each constructor carries its global tile
        -- coords as its first two fields and nothing else positional
        -- depends on them. Written out per constructor so a new one is a
        -- compile error rather than an edit that silently stops moving.
        let d = chunkSize
            every =
                [ WeDeleteTile 1 2
                , WeSetFluidTile 1 2 River
                , WeAddTile 1 2 (MaterialId 4)
                , WeSetSlope 1 2 3 7
                , WeSetCell 1 2 3 (MaterialId 4)
                , WeSetStructure 1 2 5 6 7 8
                , WeClearStructure 1 2 5
                , WeSetVeg 1 2 3 9
                , WePlaceFlora 1 2 (FloraId 3) 4 1.5
                , WeSetFluidSnapshot 1 2 River 3
                , WeClearFluidSnapshot 1 2
                ]
        map (editCoords . shiftWorldEdit d (2 * d)) every
            `shouldBe` replicate (length every) (1 + d, 2 + 2 * d)
        -- A zero shift is the identity, which is what an already
        -- canonical key gets.
        map (shiftWorldEdit 0 0) every `shouldBe` every
