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
import Engine.Core.State (EngineEnv)
import World.Chunk.Admit
    ( admitResidentChunks, claimChunkGeneration, claimedChunkCoord
    , pageChunkKey, readChunkOwner, registerChunkDemand
    , releaseEvictedChunks, seedResidentChunks, withTransientChunkClaim )
import World.Chunk.Residency
    ( AdmitOutcome(..), ChunkGeneration, ChunkOwner, ChunkState(..)
    , ClaimOutcome(..)
    , RequestOutcome(..), admitChunk, canonicalChunkCoord, chunkKeyFor
    , chunkOwnerGeneration, chunkOwnerSize, chunkStateOf, ckCoord, ckPage
    , claimChunk, crGeneration, crKey, emptyChunkOwner, evictChunk
    , isCurrentGeneration, mintChunkRequest, newChunkGeneration
    , releaseChunk, requestChunk )
import World.Chunk.Types (ChunkCoord(..), wrapChunkCoordU)
import World.Command.Types (WorldCommand(..))
import World.Generate.Types
    (WorldGenParams(..), defaultWorldGenParams, isArenaParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldState(..), emptyWorldState)
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

        let (owner2, c1) = claimChunk req owner1
        c1 `shouldBe` ClaimGranted
        chunkStateOf key owner2 `shouldBe` ChunkInFlight

        -- Nobody generates a chunk twice.
        snd (claimChunk req owner2) `shouldBe` ClaimRefused

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

    it "admits a seeded page's chunks under their canonical keys" $ \_ → do
        -- The seed paths (a fresh world's centre, a restored centre, an
        -- arena's whole chunk set) build their payloads before the page
        -- can take a request, and still reach the resident set through
        -- the same admission boundary.
        let params = sizedParams seamWorldSize
        ws ← detachedPage params
        seedResidentChunks ws pageA params [aliasCoord]
        owner ← readChunkOwner ws
        chunkOwnerSize owner `shouldBe` 1
        stateOf params pageA canonCoord owner `shouldBe` ChunkResident
        registerChunkDemand ws pageA params [canonCoord] `shouldReturn` []
