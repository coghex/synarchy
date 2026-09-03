-- | Admission of init-generated chunks to the fluid simulation (#2232).
--
--   'Sim.Command.Types.SimChunkLoaded' and 'SimChunkEdited' are the ONLY
--   two ways a chunk enters sim state, and both streaming loaders skip a
--   coord already in @wsTilesRef@ by design. Fresh-world init publishes
--   the centre chunk synchronously and then deliberately EXCLUDES it
--   from the init queue; arena init publishes all 25 chunks and has no
--   streaming loader at all. Neither emitted a seed, so those chunks
--   never entered simulation: their fluid never ticked, the @--dump@
--   fast-settle skipped them, and nothing could cross their boundaries
--   until a live edit happened to land in them — which made the
--   behaviour depend on cache history rather than on the world.
--
--   Every example below reads the REAL sim queue. Nothing drains it in a
--   headless fixture (the harness starts the world worker and no sim
--   worker, and 'Test.Headless.World.FluidWritebackStaleness' relies on
--   the same property), so one flush after init returns exactly the
--   messages the world thread wrote. The flow examples then replay those
--   captured messages through the sim thread's own transitions
--   ('Sim.Chunk.loadedChunkState', 'Sim.Chunk.applyChunkEdit',
--   'Sim.Fluid.Active.simulateActiveTick'), so what settles is what init
--   actually sent rather than a hand-built fixture that could agree with
--   nothing.
--
--   __What is deliberately not here.__ The save\/load axis of "no chunk
--   is seeded twice" is not a duplicate to hunt: a load REPLACES the
--   whole session, drops every old page's simulation state and then
--   seeds the reconstructed centre once
--   ("World.Load.Publish"), which is the existing whole-session
--   replacement contract. The re-seed examples therefore drive the three
--   paths that keep a live page: a region request, a hide\/show cycle,
--   and a camera pass.
module Test.Headless.Sim.Admission (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (threadDelay)
import Data.IORef (readIORef)
import Data.List (nub, sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.Core.State (EngineEnv(..))
import qualified Engine.Core.Queue as Q
import Sim.Chunk (applyChunkEdit, loadedChunkState)
import Sim.Command.Types (SimCommand(..))
import Sim.Fluid.Active (simulateActiveTick)
import Sim.Fluid.Types (ActiveFluidCell(..))
import Sim.State.Types (SimChunkState(..), SimWorldState(..))
import Sim.Topology
    (SimTopology, simCardinalNeighbors, simTopologyForParams)
import Test.Headless.Harness
    (moveCamera, queueChunks, sendWorldCommand, waitForWorldInit)
import World.Fluid.Internal (FluidMap)
import World.Generate.Arena (arenaGenForSeed, generateArenaChunks)
import World.Types

-- * Fixture

-- | One seed as the examples talk about it: everything
--   'Sim.Command.Types.SimChunkLoaded' carries except the page id, which
--   every collection below is already filtered by.
data Seed = Seed
    { seedTopo    ∷ SimTopology
    , seedCoord   ∷ ChunkCoord
    , seedFluid   ∷ FluidMap
    , seedTerrain ∷ VU.Vector Int
    } deriving (Eq, Show)

-- | Everything the examples read, captured once. Two pages are
--   generated (one small fresh world, one arena) and the sim queue is
--   flushed four times: after each page's init, and again after each
--   page has been driven through every live path that could re-seed it.
data Fixture = Fixture
    { fxWorldSeeds    ∷ [Seed]
      -- ^ Seeds the fresh page's init wrote, in queue order.
    , fxWorldReseeds  ∷ [Seed]
      -- ^ Seeds that page produced afterwards. Must add no coord the
      --   first flush already carried.
    , fxWorldUnloads  ∷ [ChunkCoord]
      -- ^ 'SimChunkUnloaded' for that page across every flush. A chunk
      --   that was evicted and reloaded WOULD legitimately be seeded
      --   twice, so the duplicate check is only honest while this is
      --   empty.
    , fxWorldTopo     ∷ SimTopology
    , fxWorldCentre   ∷ LoadedChunk
      -- ^ The resident centre chunk, read from @wsTilesRef@.
    , fxArenaSeeds    ∷ [Seed]
    , fxArenaReseeds  ∷ [Seed]
    , fxArenaUnloads  ∷ [ChunkCoord]
    , fxArenaTopo     ∷ SimTopology
    }

-- | The fresh page. Small (8 chunks a side) because worldgen is the only
--   expensive thing in this module, and the centre is @ChunkCoord 0 0@
--   at every size ("World.Thread.Command.Init").
worldPageId ∷ WorldPageId
worldPageId = WorldPageId "sim_admission_world_2232"

arenaPageId ∷ WorldPageId
arenaPageId = WorldPageId "sim_admission_arena_2232"

worldSeed ∷ Word64
worldSeed = 7

worldSizeChunks, worldPlates ∷ Int
worldSizeChunks = 8
worldPlates = 3

-- | The synchronously generated centre: the coord fresh init publishes
--   straight to @wsTilesRef@ and excludes from the init queue.
centreCoord ∷ ChunkCoord
centreCoord = ChunkCoord 0 0

cells ∷ Int
cells = chunkSize * chunkSize

setup ∷ EngineEnv → IO Fixture
setup env = do
    -- The fresh page.
    sendWorldCommand env (WorldInit worldPageId worldSeed worldSizeChunks
                                    worldPlates Nothing)
    ws ← waitForWorldInit env worldPageId 300
    settle ws
    flushA ← Q.flushQueue (simQueue env)

    params ← readIORef (wsGenParamsRef ws) ⌦ \case
        Just p  → pure p
        Nothing → expectationFailure
            "the fresh page recorded no WorldGenParams"
            ≫ error "unreachable"
    tiles ← readIORef (wsTilesRef ws)
    centre ← case HM.lookup centreCoord (wtdChunks tiles) of
        Just lc → pure lc
        Nothing → expectationFailure
            "the centre chunk is not resident after init"
            ≫ error "unreachable"

    -- Every live path that could seed it a second time: a region
    -- request for the coord that is already resident (literally what
    -- @world.loadChunksInRegion@ does), a hide/show cycle, and a camera
    -- pass over the centre.
    queueChunks worldPageId ws [centreCoord]
    hideShow env worldPageId
    moveCamera env 0 0
    settle ws
    flushB ← Q.flushQueue (simQueue env)

    -- The arena page.
    sendWorldCommand env (WorldInitArena arenaPageId)
    aws ← waitForWorldInit env arenaPageId 120
    settle aws
    flushC ← Q.flushQueue (simQueue env)

    queueChunks arenaPageId aws [centreCoord]
    hideShow env arenaPageId
    settle aws
    flushD ← Q.flushQueue (simQueue env)

    aparams ← readIORef (wsGenParamsRef aws) ⌦ \case
        Just p  → pure p
        Nothing → expectationFailure
            "the arena page recorded no WorldGenParams"
            ≫ error "unreachable"

    let allFlushes = flushA ⧺ flushB ⧺ flushC ⧺ flushD
    pure Fixture
        { fxWorldSeeds   = seedsFor worldPageId flushA
        , fxWorldReseeds = seedsFor worldPageId (flushB ⧺ flushC ⧺ flushD)
        , fxWorldUnloads = unloadsFor worldPageId allFlushes
        , fxWorldTopo    = simTopologyForParams params
        , fxWorldCentre  = centre
        , fxArenaSeeds   = seedsFor arenaPageId flushC
        , fxArenaReseeds = seedsFor arenaPageId flushD
        , fxArenaUnloads = unloadsFor arenaPageId allFlushes
        , fxArenaTopo    = simTopologyForParams aparams
        }

-- | Let the world worker reach a quiescent tick: nothing left on the
--   page's init queue, and a couple of further ticks so anything that
--   queue's last batch dispatched has been written.
settle ∷ WorldState → IO ()
settle ws = go (0 ∷ Int)
  where
    go n
      | n ≥ 100  = pure ()
      | otherwise = do
          threadDelay 50000
          remaining ← readIORef (wsInitQueueRef ws)
          if null remaining ∧ n ≥ 4 then pure () else go (n + 1)

-- | Hide the page and show it again, blocking until the world thread
--   has applied each command — 'wmVisible' is what those handlers write,
--   so polling it is a real fence rather than a sleep.
hideShow ∷ EngineEnv → WorldPageId → IO ()
hideShow env pid = do
    sendWorldCommand env (WorldHide pid)
    awaitVisible False
    sendWorldCommand env (WorldShow pid)
    awaitVisible True
  where
    awaitVisible want = go (0 ∷ Int)
      where
        go n
          | n ≥ 100   = pure ()
          | otherwise = do
              mgr ← readIORef (worldManagerRef env)
              if (pid `elem` wmVisible mgr) ≡ want
                  then pure ()
                  else threadDelay 50000 ≫ go (n + 1)

seedsFor ∷ WorldPageId → [SimCommand] → [Seed]
seedsFor pid cmds =
    [ Seed topo coord fluidMap terrainMap
    | SimChunkLoaded p topo coord fluidMap terrainMap ← cmds, p ≡ pid ]

unloadsFor ∷ WorldPageId → [SimCommand] → [ChunkCoord]
unloadsFor pid cmds = [ coord | SimChunkUnloaded p coord ← cmds, p ≡ pid ]

-- * Replaying captured seeds

-- | The sim thread's own 'SimChunkLoaded' transition, folded over the
--   captured seeds ("Sim.Thread"): each becomes a PASSIVE chunk holding
--   the seeded fluid and terrain.
replaySeeds ∷ SimTopology → [Seed] → SimWorldState
replaySeeds topo = foldl' step empty
  where
    empty = SimWorldState
        { swsChunks      = HM.empty
        , swsDirtyChunks = HS.empty
        , swsActive      = True
        , swsTopology    = topo
        }
    step sws s = sws
        { swsTopology = seedTopo s
        , swsChunks   = HM.insert (seedCoord s)
                            (loadedChunkState (seedFluid s) (seedTerrain s))
                            (swsChunks sws)
        }

-- | The state a @world.setFluidTile@ edit produces: the chunk's whole
--   post-edit fluid map plus its own terrain surface, through the
--   production 'applyChunkEdit'. @surfaceZ@ is absolute, so a value
--   above every terrain height in the fixture floods the chunk whatever
--   the generated relief underneath is.
floodEdit ∷ Int → ChunkCoord → SimWorldState → SimWorldState
floodEdit surfaceZ coord sws = applyChunkEdit coord 1 flooded terrain sws
  where
    terrain = case HM.lookup coord (swsChunks sws) of
        Just scs → scsTerrain scs
        Nothing  → VU.replicate cells 0
    flooded = V.replicate cells (Just (FluidCell Lake surfaceZ))

-- | An absolute water surface clear of every terrain height AND every
--   generated water surface in the fixture, so the flooded chunk always
--   stands above its neighbour and the transfer direction is the one the
--   example names. Reading only the terrain would be wrong on a page
--   whose sea level sits above these two chunks' relief.
floodSurface ∷ SimWorldState → Int
floodSurface sws = 8 + maximum (0 : concatMap heights (HM.elems (swsChunks sws)))
  where
    heights scs =
        [ VU.maximum (scsTerrain scs) | not (VU.null (scsTerrain scs)) ]
        ⧺ [ fcSurface fc | Just fc ← V.toList (scsFluid scs) ]

tick ∷ Int → SimWorldState → SimWorldState
tick k sws = iterate simulateActiveTick sws !! k

-- | Live (active-grid) volume in one chunk. A chunk that is not in sim
--   state at all counts as zero, which is exactly what a refused
--   transfer leaves behind.
chunkVolume ∷ ChunkCoord → SimWorldState → Int
chunkVolume cc sws = case HM.lookup cc (swsChunks sws) of
    Nothing  → 0
    Just scs → sum [ fromIntegral (afcVolume afc)
                   | Just afc ← V.toList (scsActiveFluid scs) ]

-- | The first cardinal neighbour of @coord@ that the page actually
--   seeded, resolved through the page's own topology so a seam-adjacent
--   pair is named by the key it is STORED under (#2044).
neighbourOf ∷ SimTopology → [Seed] → ChunkCoord → IO ChunkCoord
neighbourOf topo seeds coord =
    case [ nc | nc ← simCardinalNeighbors topo coord
              , nc ≢ coord
              , nc `elem` map seedCoord seeds ] of
        (nc : _) → pure nc
        []       → expectationFailure
            ("no seeded cardinal neighbour of " ⧺ show coord)
            ≫ error "unreachable"

-- | The two-chunk state the flow examples settle: only @a@ and @b@, so
--   the seam between them is the only one either chunk has and a volume
--   that leaves one has nowhere to go but the other.
pairState ∷ SimTopology → [Seed] → ChunkCoord → ChunkCoord → SimWorldState
pairState topo seeds a b =
    replaySeeds topo [ s | s ← seeds, seedCoord s ≡ a ∨ seedCoord s ≡ b ]

-- * Examples

spec ∷ SpecWith EngineEnv
spec = beforeAllWith setup $ do

    describe "fresh-world init" $ do
        it "seeds the synchronously generated centre chunk exactly once" $
          \fx → do
            let centres = [ s | s ← fxWorldSeeds fx
                              , seedCoord s ≡ centreCoord ]
            length centres `shouldBe` 1

        it "carries the page topology and the POST-admission chunk" $ \fx →
            case [ s | s ← fxWorldSeeds fx, seedCoord s ≡ centreCoord ] of
                [s] → do
                    seedTopo s `shouldBe` fxWorldTopo fx
                    seedFluid s `shouldBe` lcFluidMap (fxWorldCentre fx)
                    seedTerrain s
                        `shouldBe` lcTerrainSurfaceMap (fxWorldCentre fx)
                _ → expectationFailure "expected exactly one centre seed"

        it "seeds it before the init queue, ahead of every queued chunk" $
          \fx →
            -- 'SimFastSettleAll' is enqueued behind the init queue's last
            -- batch and only settles chunks already in sim state, so a
            -- centre seed written after those would miss the settle.
            map seedCoord (take 1 (fxWorldSeeds fx))
                `shouldBe` [centreCoord]

        it "seeds no chunk twice across a region request, a hide/show \
           \cycle and a camera pass" $ \fx → do
            -- Honest only while nothing was evicted: an evicted chunk is
            -- reloaded, and a reload SHOULD seed again.
            fxWorldUnloads fx `shouldBe` []
            let coords = map seedCoord (fxWorldSeeds fx ⧺ fxWorldReseeds fx)
            sort coords `shouldBe` sort (nub coords)

    describe "arena init" $ do
        it "seeds every generated arena chunk exactly once" $ \fx → do
            let generated = map lcCoord
                    (generateArenaChunks (arenaGenForSeed 0))
                seeded = map seedCoord (fxArenaSeeds fx)
            length generated `shouldBe` 25
            sort seeded `shouldBe` sort generated

        it "carries the arena page's topology and each chunk's own \
           \fluid and terrain surface" $ \fx → do
            let byCoord = HM.fromList
                    [ (lcCoord lc, lc)
                    | lc ← generateArenaChunks (arenaGenForSeed 0) ]
            forM_ (fxArenaSeeds fx) $ \s → do
                seedTopo s `shouldBe` fxArenaTopo fx
                case HM.lookup (seedCoord s) byCoord of
                    Nothing → expectationFailure
                        ("seed for an ungenerated coord: "
                         ⧺ show (seedCoord s))
                    Just lc → do
                        seedFluid s `shouldBe` lcFluidMap lc
                        seedTerrain s `shouldBe` lcTerrainSurfaceMap lc

        it "seeds no arena chunk twice across a region request and a \
           \hide/show cycle" $ \fx → do
            fxArenaUnloads fx `shouldBe` []
            let coords = map seedCoord (fxArenaSeeds fx ⧺ fxArenaReseeds fx)
            sort coords `shouldBe` sort (nub coords)

    describe "fluid crosses the admitted chunk's boundary" $ do
        it "into the centre from an edit in the adjacent chunk" $ \fx → do
            nc ← neighbourOf (fxWorldTopo fx) (fxWorldSeeds fx) centreCoord
            let st0    = pairState (fxWorldTopo fx) (fxWorldSeeds fx)
                                   centreCoord nc
                edited = floodEdit (floodSurface st0) nc st0
                after  = tick 40 edited
            chunkVolume centreCoord after
                `shouldSatisfy` (> chunkVolume centreCoord edited)
            chunkVolume nc after
                `shouldSatisfy` (< chunkVolume nc edited)

        it "and nothing crosses when the centre was never admitted" $
          \fx → do
            -- The pre-fix state, and the control that keeps the example
            -- above from passing on any two chunks: drop the centre's
            -- seed and the identical edit moves nothing. An edit in a
            -- NEIGHBOUR cannot create the missing chunk —
            -- 'applyChunkEdit' only adjusts keys already present — so
            -- the transfer is refused and the whole volume stays put.
            nc ← neighbourOf (fxWorldTopo fx) (fxWorldSeeds fx) centreCoord
            let seeds  = [ s | s ← fxWorldSeeds fx, seedCoord s ≢ centreCoord ]
                st0    = pairState (fxWorldTopo fx) seeds centreCoord nc
                full   = pairState (fxWorldTopo fx) (fxWorldSeeds fx)
                                   centreCoord nc
                edited = floodEdit (floodSurface full) nc st0
                after  = tick 40 edited
            HM.member centreCoord (swsChunks after) `shouldBe` False
            chunkVolume nc after `shouldBe` chunkVolume nc edited

        it "out of the centre into an adjacent chunk" $ \fx → do
            nc ← neighbourOf (fxWorldTopo fx) (fxWorldSeeds fx) centreCoord
            let st0    = pairState (fxWorldTopo fx) (fxWorldSeeds fx)
                                   centreCoord nc
                edited = floodEdit (floodSurface st0) centreCoord st0
                after  = tick 40 edited
            chunkVolume nc after `shouldSatisfy` (> chunkVolume nc edited)

        it "and on an arena, into an unedited neighbour" $ \fx → do
            let a = ChunkCoord 0 0
            nc ← neighbourOf (fxArenaTopo fx) (fxArenaSeeds fx) a
            let st0    = pairState (fxArenaTopo fx) (fxArenaSeeds fx) a nc
                edited = floodEdit (floodSurface st0) a st0
                after  = tick 40 edited
            chunkVolume nc after `shouldSatisfy` (> chunkVolume nc edited)

        it "and on an arena, nothing crosses to a chunk init never \
           \seeded" $ \fx → do
            let a = ChunkCoord 0 0
            nc ← neighbourOf (fxArenaTopo fx) (fxArenaSeeds fx) a
            let seeds  = [ s | s ← fxArenaSeeds fx, seedCoord s ≢ nc ]
                st0    = pairState (fxArenaTopo fx) seeds a nc
                full   = pairState (fxArenaTopo fx) (fxArenaSeeds fx) a nc
                edited = floodEdit (floodSurface full) a st0
                after  = tick 40 edited
            HM.member nc (swsChunks after) `shouldBe` False
            chunkVolume a after `shouldBe` chunkVolume a edited
