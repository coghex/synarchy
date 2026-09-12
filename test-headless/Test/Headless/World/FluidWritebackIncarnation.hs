-- | The causal fence between a page's DESTRUCTION and the sim's fluid
--   writebacks (#2477).
--
--   'World.Command.Types.FluidWritebackBatch' used to name its page by
--   'World.Page.Types.WorldPageId' alone. A page id is reused
--   constantly — a reinit, an arena replacement, a load republish — and
--   each of those builds a fresh 'World.State.Types.WorldState' whose
--   chunks have had no live edit, so the page's
--   'World.State.Types.wsChunkEditGenRef' is empty and every chunk of
--   it reads as live-edit generation ZERO. That is precisely where a
--   batch the simulation computed against the PREVIOUS incarnation was
--   stamped, so the per-chunk freshness fence (#1596) read such a batch
--   as fresh and applied a dead page's fluid on top of its replacement.
--
--   The fix reuses the epoch the page already has: the
--   'World.Chunk.Residency.ChunkGeneration' every fresh 'WorldState'
--   mints (#2001), carried on every sim message that carries topology
--   (#2044) and stamped onto every batch the sim emits. The world
--   thread — the sole writer of 'World.State.Types.wsTilesRef' — refuses
--   any batch that is not the live page's own, ahead of the per-chunk
--   fence.
--
--   What each group here is for:
--
--     * @the refusal decision@ exercises
--       'World.Thread.Command.batchIsCurrentIncarnation' directly, so
--       the rule is pinned independently of everything that has to line
--       up for a batch to reach it.
--     * @the fence, through the world thread@ drives the REAL
--       'World.Command.Types.WorldApplyFluids' handler with payloads
--       that provably WOULD be applied but for the epoch: a resident
--       chunk whose live-edit generation matches, and a writeback that
--       changes every field 'World.Thread.Command.applyOneWriteback'
--       writes.
--     * @the epoch on the wire@ drives the real sim command handler and
--       the real emit step, so a missing assignment in "Sim.Thread"
--       fails here rather than passing behind a hand-built batch.
--     * @same-id replacement@ covers the one reuse that reaches no
--       teardown of its own — a direct re-init — where recording the
--       incoming epoch on state the OUTGOING page left behind would
--       re-label exactly the chunks the fence exists to refuse.
--
--   Like "Test.Headless.World.FluidWritebackStaleness", nothing here
--   runs a sim thread: the interleaving under test is one a live sim
--   only reaches by chance, so the sim state is driven by hand through
--   the production transition and the batch is delivered to the live
--   world thread directly.
--
--   Run:
--   @cabal test synarchy-test-headless --test-options='--match "fluid writeback incarnation"'@
module Test.Headless.World.FluidWritebackIncarnation (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar)
import Data.IORef (IORef, newIORef, readIORef)
import Data.List (findIndex, sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Timeout (timeout)

import Engine.Core.Clock (monotonicSeconds)
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Sim.Command.Types
    (FastSettleOutcome(..), FastSettleRequest(..), SimCommand(..))
import Sim.State.Types
    (SimState(..), SimWorldState(..), emptySimState)
import Sim.Thread
    (completeFastSettleWith, emitWorldDirtyFluids, handleSimCommand)
import Sim.Topology (SimTopology)
import Test.Headless.Harness
    (getWorldState, sendWorldCommand, waitForWorldInit)
import World.Chunk.Admit (pageIncarnation)
import World.Chunk.Residency (ChunkGeneration, newChunkGeneration)
import World.Thread.Command (batchIsCurrentIncarnation)
import World.Types

-- | The page every example that does not destroy a page shares. Each
--   takes a DIFFERENT resident chunk of it (see 'nthChunk'), so one
--   example's applied writeback is invisible to the next and a single
--   w8 worldgen covers the module's non-destructive half.
fencePageId ∷ WorldPageId
fencePageId = WorldPageId "fwi_fence_w8"

-- | Its own page, because it destroys and re-creates one.
replayPageId ∷ WorldPageId
replayPageId = WorldPageId "fwi_replay_w8"

-- | …and its own again, because it re-initialises one in place.
reinitPageId ∷ WorldPageId
reinitPageId = WorldPageId "fwi_reinit_w8"

chunkCells ∷ Int
chunkCells = chunkSize * chunkSize

-- | Generous, and bounded: a regression that never acks must fail the
--   example rather than hang the suite.
ackTimeoutMicros ∷ Int
ackTimeoutMicros = 30 * 1000 * 1000

spec ∷ SpecWith EngineEnv
spec = describe "fluid writeback incarnation fence (#2477)" $ do
    decisionSpec
    fenceSpec
    wireSpec
    replacementSpec

-- * The decision itself

-- | The rule, with nothing else in the way. Everything below has to get
--   a page generated, a chunk resident and a batch delivered before the
--   comparison is even reached; this pins the comparison on its own, so
--   a failure here says the RULE moved rather than that some step on
--   the way to it did.
decisionSpec ∷ SpecWith EngineEnv
decisionSpec = describe "the refusal decision" $ do

    it "accepts exactly the live page's own epoch" $ \_ → do
        live ← newChunkGeneration
        batchIsCurrentIncarnation live (Just live) `shouldBe` True

    it "refuses any other incarnation's epoch, newer or older" $ \_ → do
        older ← newChunkGeneration
        live  ← newChunkGeneration
        newer ← newChunkGeneration
        batchIsCurrentIncarnation live (Just older) `shouldBe` False
        batchIsCurrentIncarnation live (Just newer) `shouldBe` False

    it "refuses an ABSENT epoch rather than waving it through, since it \
       \makes no claim about which incarnation it was computed against" $
      \_ → do
        live ← newChunkGeneration
        batchIsCurrentIncarnation live Nothing `shouldBe` False

-- * The fence, through the real world thread

fenceSpec ∷ SpecWith EngineEnv
fenceSpec = describe "the fence, through the world thread" $ do

    it "refuses a batch stamped with an incarnation the page never was, \
       \and one stamped with none at all, while applying the SAME batch \
       \stamped with the live epoch — every per-chunk generation \
       \matching throughout" $ \env → do
        ws ← page env fencePageId
        (coord, before) ← nthChunk ws 0
        live ← pageIncarnation ws

        -- The per-chunk fence (#1596) would pass every one of the three
        -- deliveries below: this chunk has had no live edit, so the page
        -- has issued no generation for it and reads 0, which is exactly
        -- what the writeback claims. Without this the refusals would be
        -- indistinguishable from #1596 doing the work.
        gens ← readIORef (wsChunkEditGenRef ws)
        HM.lookupDefault 0 coord gens `shouldBe` (0 ∷ Word64)

        let payload = perturbed coord before

        -- An epoch belonging to no page this engine holds — the shape a
        -- batch computed against a previous incarnation of this id
        -- arrives in.
        foreign' ← newChunkGeneration
        deliver env fencePageId (Just foreign') [payload]
        unchanged ws coord before

        deliver env fencePageId Nothing [payload]
        unchanged ws coord before

        -- …and the control: nothing about the payload, the chunk or the
        -- page changed, so the epoch is the only thing that decided.
        deliver env fencePageId (Just live) [payload]
        applied ws coord payload

    it "refuses a batch taken from a page that was then destroyed and \
       \re-created under the SAME id, leaving every tile of the \
       \replacement untouched and still filling the batch's ack" $
      \env → do
        first' ← page env replayPageId
        (coord, before) ← nthChunk first' 0
        -- Stamped with the page's own epoch, as the sim would stamp it:
        -- at the moment it is built this batch is entirely legitimate.
        doomed ← pageIncarnation first'
        let payload = perturbed coord before

        -- Destroy the page and re-create it under the same id, with the
        -- batch still in hand — the race this issue is about, forced.
        -- The empty acked batch is the FIFO barrier that makes the
        -- destroy provably complete before the re-init is sent.
        sendWorldCommand env (WorldDestroy replayPageId)
        barrier env replayPageId
        (isNothing ⊚ getWorldState env replayPageId) `shouldReturn` True

        sendWorldCommand env (WorldInit replayPageId 45 8 3 Nothing)
        second' ← waitForWorldInit env replayPageId 120
        replacement ← pageIncarnation second'
        replacement `shouldNotBe` doomed

        -- The replacement holds the same coord, at generation zero and
        -- never edited — so the pre-#2477 fence would have applied this.
        (_, fresh) ← chunkNamed second' coord
        gens ← readIORef (wsChunkEditGenRef second')
        HM.lookupDefault 0 coord gens `shouldBe` (0 ∷ Word64)

        deliver env replayPageId (Just doomed) [payload]
        unchanged second' coord fresh

-- * The epoch on the wire

wireSpec ∷ SpecWith EngineEnv
wireSpec = describe "the epoch on the wire" $ do

    it "records the page's epoch on its sim state from EVERY \
       \topology-bearing command, exactly as it records the topology" $
      \env → do
        ws ← page env fencePageId
        (coord, lc) ← nthChunk ws 1
        topo ← pageSimTopology ws
        epoch ← pageIncarnation ws

        -- One fresh sim state per command, so each proves its OWN
        -- assignment rather than inheriting the previous command's.
        forM_ [ SimActivateWorld fencePageId epoch topo
              , SimChunkLoaded fencePageId epoch topo coord
                               (lcFluidMap lc) (lcTerrainSurfaceMap lc)
              , SimChunkEdited fencePageId epoch topo coord 7
                               (lcFluidMap lc) (lcTerrainSurfaceMap lc)
              ] $ \cmd → do
            simRef ← freshSimState
            drive env simRef cmd
            sws ← simWorld simRef fencePageId
            swsIncarnation sws `shouldBe` Just epoch
            -- The topology it is modelled on lands too, so a fixture
            -- that stopped seeding one would be visible here.
            swsTopology sws `shouldBe` topo

        -- …and it starts absent, so the assertions above are assignments
        -- rather than a value that was always there.
        empty' ← freshSimState
        drive env empty' (SimChunkUnloaded fencePageId coord)
        swsIncarnation ⊚ simWorld empty' fencePageId `shouldReturn` Nothing

    it "stamps the epoch its sim state holds onto the batch a normal \
       \tick emits, for a page that was seeded and never activated" $
      \env → do
        ws ← page env fencePageId
        (coord, before) ← nthChunk ws 2
        live ← pageIncarnation ws

        -- Seeded through the production command handler, never
        -- activated: 'SimFastSettleAll' emits for stored worlds too, so
        -- activation must not be what carries the epoch.
        dirty ← seededPage env fencePageId live coord before
        swsActive dirty `shouldBe` False

        -- The tick shape: no ack at all. The barrier behind it is what
        -- makes "the world thread has finished with it" observable.
        emitWorldDirtyFluids env fencePageId dirty [] Nothing
        barrier env fencePageId
        appliedSeed ws coord before

    it "stamps the same epoch on the batch a FAST SETTLE emits, and \
       \emits a foreign page's as foreign so the world refuses it" $
      \env → do
        ws ← page env fencePageId
        live ← pageIncarnation ws

        -- Refused: the settle emits whatever epoch the sim state holds,
        -- so a state stamped with another incarnation produces a batch
        -- the world thread refuses.
        (staleCoord, staleBefore) ← nthChunk ws 3
        foreign' ← newChunkGeneration
        staleSws ← seededPage env fencePageId foreign' staleCoord staleBefore
        settle env fencePageId staleSws `shouldReturn` Just FastSettleApplied
        unchanged ws staleCoord staleBefore

        -- Applied: same page, same shape, the page's own epoch.
        (freshCoord, freshBefore) ← nthChunk ws 4
        freshSws ← seededPage env fencePageId live freshCoord freshBefore
        settle env fencePageId freshSws `shouldReturn` Just FastSettleApplied
        appliedSeed ws freshCoord freshBefore

-- * Same-id replacement

replacementSpec ∷ SpecWith EngineEnv
replacementSpec = describe "same-id replacement" $ do

    it "drops the previous incarnation's sim state before any seed \
       \carrying the replacement's epoch, when a page id is \
       \re-initialised in place with no WorldDestroy" $ \env → do
        first' ← page env reinitPageId
        firstEpoch ← pageIncarnation first'

        -- A clean window on the sim queue. Nothing drains it in a
        -- headless fixture, so what the re-init writes is all that is
        -- left in it afterwards, in the order the sim would read it.
        _ ← Q.flushQueue (simQueue env)

        -- The DIRECT re-init: no WorldDestroy, no load publish, no exit
        -- to menu — the one same-id replacement that reaches none of
        -- their teardowns.
        sendWorldCommand env (WorldInit reinitPageId 45 8 3 Nothing)
        second' ← waitForReplacement env reinitPageId firstEpoch
        secondEpoch ← pageIncarnation second'

        cmds ← Q.flushQueue (simQueue env)
        let dropAt = findIndex (isDropFor reinitPageId) cmds
            seedAt = findIndex (isSeedFor reinitPageId) cmds
            epochs = seedEpochs reinitPageId cmds

        -- The re-init seeded the replacement at all, so the ordering
        -- claim below is about something.
        epochs `shouldNotBe` []
        -- …every seed names the INCOMING incarnation…
        epochs `shouldSatisfy` all (≡ secondEpoch)
        -- …and the drop precedes the first of them. The sim reads this
        -- queue in order, so that is the whole guarantee.
        case (dropAt, seedAt) of
            (Nothing, _) → expectationFailure
                "re-initialising a live page id enqueued no SimDropWorld"
            (_, Nothing) → expectationFailure
                "re-initialising a live page id enqueued no seed"
            (Just d, Just t) → d `shouldSatisfy` (< t)

    it "discards the retained chunks on that drop, rather than leaving \
       \them for the next seed to re-label with the new epoch" $ \env → do
        ws ← page env fencePageId
        (outgoing, lc) ← nthChunk ws 5
        (incoming, _) ← nthChunk ws 6
        topo ← pageSimTopology ws
        old ← newChunkGeneration
        new ← newChunkGeneration
        let seed epoch coord = SimChunkLoaded fencePageId epoch topo coord
                                   (lcFluidMap lc) (lcTerrainSurfaceMap lc)

        -- What the world-side drop is protecting against, stated: the
        -- epoch is recorded on the PAGE's state, so a seed for the
        -- replacement re-labels whatever the outgoing incarnation left
        -- under that key — and writebacks derived from those chunks then
        -- carry the live epoch and pass the fence.
        relabelled ← runSim env [seed old outgoing, seed new incoming]
        swsIncarnation relabelled `shouldBe` Just new
        sort (HM.keys (swsChunks relabelled))
            `shouldBe` sort [outgoing, incoming]

        -- With the drop in between — which the example above proves the
        -- re-init enqueues first — there is nothing left to re-label.
        dropped ← runSim env
            [seed old outgoing, SimDropWorld fencePageId, seed new incoming]
        swsIncarnation dropped `shouldBe` Just new
        HM.keys (swsChunks dropped) `shouldBe` [incoming]

-- Fixture ---------------------------------------------------------

-- | The shared page, generated on first use. Every example takes a
--   different chunk of it, so one worldgen serves them all.
page ∷ EngineEnv → WorldPageId → IO WorldState
page env pageId = do
    mWs ← getWorldState env pageId
    case mWs of
        Just ws → pure ws
        Nothing → do
            sendWorldCommand env (WorldInit pageId 45 8 3 Nothing)
            waitForWorldInit env pageId 120

-- | Wait for a page id to come back under a DIFFERENT incarnation, which
--   is what a re-init in place produces: the outgoing page is still
--   registered and still @LoadDone@ when the command is enqueued, so
--   waiting on the phase alone can answer with the page being replaced.
waitForReplacement ∷ EngineEnv → WorldPageId → ChunkGeneration
                   → IO WorldState
waitForReplacement env pageId outgoing = go (1200 ∷ Int)
  where
    go 0 = expectationFailure
        ("page never came back under a new incarnation: " ⧺ show pageId)
        ≫ error "unreachable"
    go n = do
        mWs ← getWorldState env pageId
        case mWs of
            Nothing → retry n
            Just ws → do
                epoch ← pageIncarnation ws
                phase ← readIORef (wsLoadPhaseRef ws)
                if epoch ≢ outgoing ∧ phase ≡ LoadDone
                    then pure ws
                    else retry n
    retry n = threadDelay 100000 ≫ go (n - 1)

-- | 'SimDropWorld' for this page.
isDropFor ∷ WorldPageId → SimCommand → Bool
isDropFor pageId cmd = case cmd of
    SimDropWorld p → p ≡ pageId
    _              → False

-- | A chunk seed for this page.
isSeedFor ∷ WorldPageId → SimCommand → Bool
isSeedFor pageId cmd = case cmd of
    SimChunkLoaded p _ _ _ _ _ → p ≡ pageId
    _                          → False

-- | Every epoch this page's seeds carry, in queue order.
seedEpochs ∷ WorldPageId → [SimCommand] → [ChunkGeneration]
seedEpochs pageId cmds =
    [ epoch | SimChunkLoaded p epoch _ _ _ _ ← cmds, p ≡ pageId ]

-- | Fold a command sequence through the production transition and return
--   the page's resulting sim state.
runSim ∷ EngineEnv → [SimCommand] → IO SimWorldState
runSim env cmds = do
    simRef ← freshSimState
    forM_ cmds (drive env simRef)
    simWorld simRef fencePageId

-- | The @n@th resident chunk in coordinate order — deterministic, so two
--   examples asking for different indices provably get different chunks.
nthChunk ∷ WorldState → Int → IO (ChunkCoord, LoadedChunk)
nthChunk ws n = do
    td ← readIORef (wsTilesRef ws)
    let coords = sort (HM.keys (wtdChunks td))
    case drop n coords of
        (coord : _) → chunkNamed ws coord
        []          → expectationFailure
            ("fixture: page has no chunk at index " ⧺ show n)
            ≫ error "unreachable"

-- | One named resident chunk, as the page currently holds it.
chunkNamed ∷ WorldState → ChunkCoord → IO (ChunkCoord, LoadedChunk)
chunkNamed ws coord = do
    td ← readIORef (wsTilesRef ws)
    case lookupChunk coord td of
        Just lc → pure (coord, lc)
        Nothing → expectationFailure ("chunk not loaded: " ⧺ show coord)
                  ≫ error "unreachable"

-- | A writeback that differs from @lc@ in ALL FOUR fields
--   'World.Thread.Command.applyOneWriteback' writes, at the live-edit
--   generation a never-edited chunk sits at — so "was it applied?" has
--   an unambiguous answer and the #1596 fence is not what answers it.
perturbed ∷ ChunkCoord → LoadedChunk → FluidWriteback
perturbed coord lc = FluidWriteback
    { fwCoord    = coord
    , fwEditGen  = 0
    , fwFluid    = V.replicate chunkCells (Just (FluidCell Lava 7))
    , fwTerrain  = VU.map (+ 11) (lcTerrainSurfaceMap lc)
    , fwSurf     = VU.map (+ 13) (lcSurfaceMap lc)
    , fwSideDeco = VU.replicate chunkCells 5
    }

-- | The chunk still holds exactly what it held before the batch.
unchanged ∷ WorldState → ChunkCoord → LoadedChunk → IO ()
unchanged ws coord before = do
    (_, now) ← chunkNamed ws coord
    lcFluidMap now          `shouldBe` lcFluidMap before
    lcTerrainSurfaceMap now `shouldBe` lcTerrainSurfaceMap before
    lcSurfaceMap now        `shouldBe` lcSurfaceMap before
    lcSideDeco now          `shouldBe` lcSideDeco before

-- | …and the negative control: every field the writeback carries landed.
applied ∷ WorldState → ChunkCoord → FluidWriteback → IO ()
applied ws coord fw = do
    (_, now) ← chunkNamed ws coord
    lcFluidMap now          `shouldBe` fwFluid fw
    lcTerrainSurfaceMap now `shouldBe` fwTerrain fw
    lcSurfaceMap now        `shouldBe` fwSurf fw
    lcSideDeco now          `shouldBe` fwSideDeco fw

-- | Send one batch stamped with @mEpoch@ and block until the world
--   thread has finished with it.
deliver ∷ EngineEnv → WorldPageId → Maybe ChunkGeneration
        → [FluidWriteback] → IO ()
deliver env pageId mEpoch writebacks = do
    ack ← newEmptyMVar
    sendWorldCommand env
        (WorldApplyFluids
            (FluidWritebackBatch pageId mEpoch writebacks [] (Just ack)))
    awaitAck ack

-- | A FIFO barrier on the world queue: an empty batch applies nothing
--   and still acks, so its acknowledgement proves every command sent
--   before it has been handled.
barrier ∷ EngineEnv → WorldPageId → IO ()
barrier env pageId = deliver env pageId Nothing []

awaitAck ∷ MVar FluidAckOutcome → IO ()
awaitAck ack = do
    got ← timeout ackTimeoutMicros (takeMVar ack)
    case got of
        Just FluidAckApplied → pure ()
        Just (FluidAckFailed why) → expectationFailure
            ("world thread failed a WorldApplyFluids batch: " ⧺ T.unpack why)
        Nothing → expectationFailure
            "world thread never acked a WorldApplyFluids batch"

-- * Driving the sim by hand

freshSimState ∷ IO (IORef SimState)
freshSimState = newIORef emptySimState

-- | One production sim-command transition, with this engine's logger.
drive ∷ EngineEnv → IORef SimState → SimCommand → IO ()
drive env simRef cmd = do
    logger ← readIORef (loggerRef env)
    handleSimCommand env logger simRef cmd

-- | One page's sim state, or an example failure naming the page.
simWorld ∷ IORef SimState → WorldPageId → IO SimWorldState
simWorld simRef pageId = do
    ss ← readIORef simRef
    case HM.lookup pageId (ssWorlds ss) of
        Just sws → pure sws
        Nothing  → expectationFailure
            ("sim holds no state for " ⧺ show pageId)
            ≫ error "unreachable"

-- | A page seeded through the production 'SimChunkLoaded' handler with
--   @epoch@ and a payload that differs from what the page holds, with
--   that one chunk marked dirty so the emit produces a writeback for it.
--
--   Never activated: the emit paths must carry the epoch for a stored
--   world too, which is the case 'SimFastSettleAll' reaches.
seededPage ∷ EngineEnv → WorldPageId → ChunkGeneration → ChunkCoord
           → LoadedChunk → IO SimWorldState
seededPage env pageId epoch coord lc = do
    topo ← seedTopologyFor env pageId
    simRef ← freshSimState
    drive env simRef
        (SimChunkLoaded pageId epoch topo coord
            (V.replicate chunkCells (Just (FluidCell Lava 7)))
            (VU.map (+ 11) (lcTerrainSurfaceMap lc)))
    sws ← simWorld simRef pageId
    pure sws { swsDirtyChunks = HS.singleton coord }

-- | The seam topology the page itself would send.
seedTopologyFor ∷ EngineEnv → WorldPageId → IO SimTopology
seedTopologyFor env pageId = do
    mWs ← getWorldState env pageId
    case mWs of
        Just ws → pageSimTopology ws
        Nothing → expectationFailure ("no such page: " ⧺ show pageId)
                  ≫ error "unreachable"

-- | What 'seededPage' leaves in the tiles once its batch is applied: the
--   fluid and terrain it seeded, and the rendered surface derived from
--   them by the one shared rule.
appliedSeed ∷ WorldState → ChunkCoord → LoadedChunk → IO ()
appliedSeed ws coord before = do
    let seedFluid   = V.replicate chunkCells (Just (FluidCell Lava 7))
        seedTerrain = VU.map (+ 11) (lcTerrainSurfaceMap before)
    (_, now) ← chunkNamed ws coord
    lcFluidMap now          `shouldBe` seedFluid
    lcTerrainSurfaceMap now `shouldBe` seedTerrain
    lcSurfaceMap now        `shouldBe`
        VU.imap (\idx terrZ → renderedSurfaceZ terrZ (seedFluid V.! idx))
                seedTerrain

-- | Drive the production fast-settle tail with the production emit step,
--   and return what it PUBLISHED — which is the only thing the @--dump@
--   path ever reads.
--
--   The acknowledgement comes from the REAL world thread handling the
--   REAL batch, so a settle that stalls on a refused batch fails here.
--   Bounded twice over: the settle's own deadline, and this call's.
settle ∷ EngineEnv → WorldPageId → SimWorldState → IO (Maybe FastSettleOutcome)
settle env pageId sws = do
    done ← newEmptyMVar
    now ← monotonicSeconds
    let req = FastSettleRequest
                { fsrDone = done
                , fsrDeadline = now + fromIntegral (ackTimeoutMicros ∷ Int)
                                      / 1000000
                }
    ran ← timeout ackTimeoutMicros $ completeFastSettleWith
        (\pid s ack → emitWorldDirtyFluids env pid s [] (Just ack))
        monotonicSeconds (const (pure ())) req [(pageId, sws)]
    case ran of
        Nothing → pure Nothing
        Just () → timeout ackTimeoutMicros (takeMVar done)
