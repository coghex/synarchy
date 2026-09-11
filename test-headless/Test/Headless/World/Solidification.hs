-- | Committing the lava-water reaction's product as durable stone
--   (#2485, FR-2 of epic #2480).
--
--   Two halves, both registered under the @solidification@ describe:
--
--   * 'pureSpec' pins the decisions — which stone, how a delivery is
--     grouped into coherent results, whether a result is fresh, what the
--     sim is left holding, and how one chunk's tile is patched into the
--     zoom atlas — without an engine.
--   * 'spec' drives the REAL world thread. Every batch is hand-delivered
--     through 'World.Command.Types.WorldApplyFluids' exactly as the sim
--     would deliver it, so the production
--     'World.Thread.Command.applyFluidWritebacks' admission, the
--     production 'World.Edit.Apply.applyEdit', the production edit-log
--     append and the production replay are all the code under test. What
--     it does NOT use is a live sim thread: the point is to force ONE
--     interleaving (a stale participant, a sibling event, a writeback
--     arriving after a commit) that a running sim only reaches by
--     chance, and to read the messages the world thread published off an
--     undrained sim queue.
--
--   Run:
--   @cabal test synarchy-test-headless --test-options='--match "solidification"'@
module Test.Headless.World.Solidification (spec, pureSpec) where

import UPrelude
import Test.Hspec
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar)
import Control.Exception (finally)
import Control.Monad.ST (runST)
import Data.IORef (readIORef, writeIORef)
import Data.List (find, sort)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import System.Timeout (timeout)

import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv(..), zoomAtlasDataRef)
import qualified Engine.Core.Queue as Q
import Sim.Chunk (applyChunkEdit, applyReactionCommit)
import Sim.Command.Types (SimCommand(..), ReactionChunkSync(..))
import Sim.Fluid.Reaction
    ( CellSite(..), ReactionResult(..), SolidProduct(..)
    , SolidificationEvent(..), TransferOutcome(..)
    , applyTransfer, groupReactionResults, solidProductFor )
import Sim.Fluid.Types (ActiveFluidCell(..), activeToFluidCell)
import Sim.State.Types (SimChunkState(..), SimWorldState(..), emptySimWorldState)
import Sim.Thread (drainReactionResults)
import Sim.Topology (SimTopology(..))
import Test.Headless.Harness (sendWorldCommand, waitForWorldInit)
import World.Chunk.Admit (pageIncarnation)
import World.Edit.Apply (replayEdits)
import World.Edit.Types (WorldEdit(..))
import World.Generate.Coordinates (chunkToGlobal)
import World.Material
    (MaterialId(..), emptyMaterialRegistry, materialIdByName, matLoam)
import World.Reaction.Stone (stoneMaterialFor, stoneMaterialName)
import World.Thread.Command.Reaction
    (reactionChunks, reactionEventTile, reactionIsFresh)
import World.Thread.Command.Reaction.Zoom
    (atlasTileIndexFor, zoomTileOverrideFor)
import World.ZoomMap.Live (ZoomTileOverride(..), patchAtlasTile)
import World.ZoomMap.Live.Types (ZoomLiveAtlas(..))
import World.Types

-- * Pure fixtures ---------------------------------------------------

chunkA, chunkB, chunkC ∷ ChunkCoord
chunkA = ChunkCoord 0 0
chunkB = ChunkCoord 1 0
chunkC = ChunkCoord 5 5

cellsPerChunk ∷ Int
cellsPerChunk = chunkSize * chunkSize

-- | The event one CONTACT produces, resolved by the production applier
--   against two one-cell grids — the seam shape, which is the only one
--   that can put the two sides in different chunks.
--
--   @lavaFirst@ picks the ordering: 'True' runs the lava cell as the
--   transfer SOURCE, 'False' as the destination. D-5 is a property of
--   the contact, not of which side happened to move, so every predicate
--   fixture below is asserted through both.
contactEvent ∷ Bool
             → (Word16, Int)        -- ^ lava volume, lava terrain top
             → (FluidType, Word16, Int) -- ^ water type, volume, terrain top
             → Maybe SolidificationEvent
contactEvent lavaFirst (lavaVol, lavaZ) (waterT, waterVol, waterZ) = runST $ do
    lavaGrid  ← MV.replicate 1 (Just (ActiveFluidCell Lava lavaVol 0))
    waterGrid ← MV.replicate 1 (Just (ActiveFluidCell waterT waterVol 0))
    let lavaSite  = CellSite chunkA 0 lavaZ
        waterSite = CellSite chunkB 0 waterZ
    out ← if lavaFirst
          then applyTransfer lavaGrid lavaSite waterGrid waterSite 1
          else applyTransfer waterGrid waterSite lavaGrid lavaSite 1
    pure (toEvent out)

-- | An event naming one cell of one chunk, with a product chosen for it.
eventAt ∷ ChunkCoord → Int → ChunkCoord → SolidProduct → SolidificationEvent
eventAt lavaChunk idx waterChunk product' = SolidificationEvent
    { sevChunk        = lavaChunk
    , sevIndex        = idx
    , sevWaterChunk   = waterChunk
    , sevWaterType    = Lake
    , sevConsumed     = 3
    , sevStoneTop     = 1
    , sevWaterSurface = 0
    , sevProduct      = product'
    }

-- | An ACTIVE sim chunk whose grid is given explicitly.
activeChunk ∷ Word64 → [(Int, Maybe ActiveFluidCell)] → SimChunkState
activeChunk gen placed = SimChunkState
    { scsFluid       = V.replicate cellsPerChunk Nothing
    , scsTerrain     = VU.replicate cellsPerChunk 0
    , scsSettleTicks = 0
    , scsActive      = True
    , scsActiveFluid = V.replicate cellsPerChunk Nothing V.// placed
    , scsEquilTicks  = 0
    , scsSideDeco    = VU.replicate cellsPerChunk 0
    , scsEditGen     = gen
    }

simWorldWith ∷ [(ChunkCoord, SimChunkState)] → SimWorldState
simWorldWith chunks = emptySimWorldState
    { swsChunks   = HM.fromList chunks
    , swsActive   = True
    , swsTopology = SimFlatTopology
    }

pureSpec ∷ Spec
pureSpec = describe "solidification (#2485)" $ do

    -- * Requirement 2: the product predicate, one fixture per clause.
    --   Each fixture satisfies EXACTLY ONE basalt clause, so deleting
    --   any one of the three flips exactly one example to obsidian.
    describe "the product predicate" $ do
        it "is basalt when the water side was Ocean, and nothing else \
           \about the fixture is" $ do
            -- stoneTop 5 is above seaLevel and above the water surface
            -- left behind, so only the Ocean clause can answer basalt.
            solidProductFor Ocean 5 2 `shouldBe` SolidBasalt
            solidProductFor Lake  5 2 `shouldBe` SolidObsidian

        it "is basalt when the surviving water surface stands above the \
           \new stone top" $ do
            solidProductFor Lake 5 6 `shouldBe` SolidBasalt
            -- one z lower and the same fixture is subaerial
            solidProductFor Lake 5 5 `shouldBe` SolidObsidian

        it "is basalt when the new stone top is at or below sea level" $ do
            seaLevel `shouldBe` 0
            solidProductFor Lake 0    (-3) `shouldBe` SolidBasalt
            solidProductFor Lake (-1) (-3) `shouldBe` SolidBasalt
            solidProductFor Lake 1    (-3) `shouldBe` SolidObsidian

        it "is obsidian for a subaerial fresh-water contact" $
            solidProductFor River 5 3 `shouldBe` SolidObsidian

        -- The predicate is a property of the CONTACT, so the same four
        -- clauses have to come out of the real applier whichever side of
        -- the transfer the lava was on.
        it "answers the same in both contact orderings" $
            -- One clause each, as above: the Ocean pair differs from
            -- the Lake pair only in the water TYPE, and the third sits
            -- its stone top on sea level with the surviving water below
            -- it.
            forM_ [ (Ocean, (3, 4),    0,  SolidBasalt)
                  , (Lake,  (3, 4),    0,  SolidObsidian)
                  , (Lake,  (3, (-1)), -5, SolidBasalt)
                  ] $ \(waterT, (lavaVol, lavaZ), waterZ, want) → do
                let lavaSrc = contactEvent True  (lavaVol, lavaZ) (waterT, 9, waterZ)
                    lavaDst = contactEvent False (lavaVol, lavaZ) (waterT, 9, waterZ)
                fmap sevProduct lavaSrc `shouldBe` Just want
                fmap sevProduct lavaDst `shouldBe` Just want
                -- …and both name the lava cell's chunk as the event's
                -- own and the water cell's as the other participant.
                fmap sevChunk      lavaSrc `shouldBe` Just chunkA
                fmap sevChunk      lavaDst `shouldBe` Just chunkA
                fmap sevWaterChunk lavaSrc `shouldBe` Just chunkB
                fmap sevWaterChunk lavaDst `shouldBe` Just chunkB

    describe "resolving the product material" $ do
        it "names the two authored igneous materials" $ do
            stoneMaterialName SolidBasalt   `shouldBe` "basalt"
            stoneMaterialName SolidObsidian `shouldBe` "obsidian"

        it "fails loudly when the registry does not know the name, \
           \rather than answering with an absent stone" $ do
            -- The consumed lava is already gone by the time this is
            -- asked, so a silently omitted tile would destroy volume
            -- with nothing to show for it.
            stoneMaterialFor emptyMaterialRegistry SolidBasalt
                `shouldSatisfy` isLeft
            stoneMaterialFor emptyMaterialRegistry SolidObsidian
                `shouldSatisfy` isLeft

    -- * Requirements 3, 4 and 5: what a delivery is grouped into.
    describe "grouping a delivery into coherent results" $ do
        let gens cc | cc ≡ chunkA = 7
                    | cc ≡ chunkB = 9
                    | otherwise   = 0

        it "puts two events for ONE chunk in ONE result, so committing \
           \the first cannot stale the second" $ do
            let evs = [ eventAt chunkA 10 chunkA SolidBasalt
                      , eventAt chunkA 11 chunkA SolidObsidian ]
                rrs = groupReactionResults gens evs
            map rrParticipants rrs `shouldBe` [[(chunkA, 7)]]
            map (map sevIndex . rrEvents) rrs `shouldBe` [[10, 11]]

        it "names BOTH chunks of a cross-chunk contact, each with its \
           \own generation" $ do
            map rrParticipants (groupReactionResults gens
                    [ eventAt chunkA 10 chunkB SolidBasalt ])
                `shouldBe` [[(chunkA, 7), (chunkB, 9)]]

        it "keeps genuinely disjoint contacts independently eligible" $ do
            let evs = [ eventAt chunkA 10 chunkA SolidBasalt
                      , eventAt chunkC 20 chunkC SolidObsidian ]
                rrs = groupReactionResults gens evs
            map rrParticipants rrs
                `shouldBe` [[(chunkA, 7)], [(chunkC, 0)]]

        it "merges transitively: two contacts that share only a middle \
           \chunk still commit together" $ do
            let evs = [ eventAt chunkA 10 chunkB SolidBasalt
                      , eventAt chunkC 20 chunkB SolidObsidian ]
                rrs = groupReactionResults gens evs
            map (map fst . rrParticipants) rrs
                `shouldBe` [sort [chunkA, chunkB, chunkC]]
            map (map sevIndex . rrEvents) rrs `shouldBe` [[10, 20]]

        it "drains a world's events exactly once, stamping each chunk's \
           \own live-edit generation" $ do
            let sws = (simWorldWith [ (chunkA, activeChunk 7 [])
                                    , (chunkB, activeChunk 9 []) ])
                        { swsSolidEvents = Seq.fromList
                            [ eventAt chunkA 10 chunkB SolidBasalt ] }
                (drained, results) = drainReactionResults sws
            map rrParticipants results `shouldBe` [[(chunkA, 7), (chunkB, 9)]]
            -- …and a second drain of what is left yields nothing, which
            -- is what makes "committed exactly once" a property of the
            -- drain rather than of the world thread's bookkeeping.
            swsSolidEvents drained `shouldBe` Seq.empty
            snd (drainReactionResults drained) `shouldBe` []

    describe "admitting a result" $ do
        let rr = ReactionResult
                    { rrParticipants = [(chunkA, 3), (chunkB, 0)]
                    , rrEvents = [eventAt chunkA 10 chunkB SolidBasalt] }

        it "admits only when EVERY participant is still at its own \
           \recorded generation" $
            reactionIsFresh (HM.fromList [(chunkA, 3)]) rr `shouldBe` True

        it "reads an absent entry as generation 0, the baseline a \
           \reloaded chunk sits at on both sides" $
            reactionIsFresh (HM.fromList [(chunkA, 3), (chunkB, 0)]) rr
                `shouldBe` True

        it "rejects the WHOLE result when any one participant has moved \
           \on, however fresh the other is" $ do
            reactionIsFresh (HM.fromList [(chunkA, 3), (chunkB, 1)]) rr
                `shouldBe` False
            reactionIsFresh (HM.fromList [(chunkA, 4), (chunkB, 0)]) rr
                `shouldBe` False

        it "rejects a generation this page never issued, not just an \
           \older one" $
            reactionIsFresh (HM.fromList [(chunkA, 99)]) rr `shouldBe` False

        it "names every participant for convergence, whether or not it \
           \receives stone" $
            reactionChunks rr `shouldBe` [chunkA, chunkB]

        it "resolves an event's global tile from its chunk and local \
           \index" $
            reactionEventTile (eventAt (ChunkCoord 2 3) 17 chunkB SolidBasalt)
                `shouldBe` chunkToGlobal (ChunkCoord 2 3) 1 1

    -- * Requirement 6: the exact-volume handoff.
    describe "handing a committed result back to the sim" $ do
        -- 1 unit of water — deliberately not a multiple of
        -- 'Sim.Fluid.Types.volumePerLevel' — beside a cell that just
        -- became stone.
        let waterIdx = 5
            stoneIdx = 6
            remainder = ActiveFluidCell Lake 1 0
            before = simWorldWith
                [ (chunkA, activeChunk 3
                    [ (waterIdx, Just remainder)
                    , (stoneIdx, Nothing) ]) ]
            -- What the world's tiles say: the passive map the edit left,
            -- in which the remainder can only be expressed as a whole
            -- surface level.
            passive = V.replicate cellsPerChunk Nothing
                        V.// [(waterIdx, activeToFluidCell 0 remainder)]
            terrain = VU.replicate cellsPerChunk 0 VU.// [(stoneIdx, 1)]
            after = applyReactionCommit chunkA 4 passive terrain [stoneIdx] before
            gridOf sws = maybe V.empty scsActiveFluid
                            (HM.lookup chunkA (swsChunks sws))

        it "keeps the contacting cell's EXACT active volume, including a \
           \remainder that is not a multiple of seven" $
            gridOf after V.! waterIdx `shouldBe` Just remainder

        it "is what the passive round trip would have destroyed" $ do
            -- The guard for this example: routing the same state through
            -- the ordinary edit re-seed hands the reaction's 1 unit back
            -- as 7, which is the volume the contact consumed.
            let reseeded = applyChunkEdit chunkA 4 passive terrain before
            fmap afcVolume (scsActiveFluid
                (swsChunks reseeded HM.! chunkA) V.! waterIdx)
                `shouldBe` Just 7

        it "leaves the solidified cell empty and adopts the post-commit \
           \generation and terrain" $ do
            gridOf after V.! stoneIdx `shouldBe` Nothing
            scsEditGen (swsChunks after HM.! chunkA) `shouldBe` 4
            scsTerrain (swsChunks after HM.! chunkA) `shouldBe` terrain

        it "re-seeds an INACTIVE chunk from the passive map instead, \
           \which is the only truth it has" $ do
            let inactive = simWorldWith
                    [ (chunkA, (activeChunk 3 []) { scsActive = False }) ]
                committed = applyReactionCommit chunkA 4 passive terrain
                                                [stoneIdx] inactive
                grid = scsActiveFluid (swsChunks committed HM.! chunkA)
            fmap afcVolume (grid V.! waterIdx) `shouldBe` Just 7
            grid V.! stoneIdx `shouldBe` Nothing

    -- * Requirement 9: the atlas patch itself.
    describe "patching one chunk's tile into the zoom atlas" $ do
        let tilesPerRow = 2
            atlasW = tilesPerRow * zoomTileSize
            atlasH = tilesPerRow * zoomTileSize
            blank = BS.replicate (atlasW * atlasH * 4) 0
            block byte = BS.replicate (zoomTileSize * zoomTileSize * 4) byte
            byteAt bs x y = BS.index bs ((y * atlasW + x) * 4)

        it "writes exactly the target tile's rows and columns" $
            case patchAtlasTile atlasW tilesPerRow 3 (block 0xAB) blank of
                Left why → expectationFailure (T.unpack why)
                Right out → do
                    BS.length out `shouldBe` BS.length blank
                    -- tile 3 is column 1, row 1: the bottom-right block
                    byteAt out zoomTileSize zoomTileSize `shouldBe` 0xAB
                    byteAt out (atlasW - 1) (atlasH - 1) `shouldBe` 0xAB
                    -- …and nothing outside it moved
                    byteAt out 0 0 `shouldBe` 0
                    byteAt out (zoomTileSize - 1) (zoomTileSize - 1)
                        `shouldBe` 0
                    byteAt out (atlasW - 1) 0 `shouldBe` 0
                    byteAt out 0 (atlasH - 1) `shouldBe` 0

        it "refuses a block of the wrong size rather than copying past \
           \the end of it" $
            patchAtlasTile atlasW tilesPerRow 0 (BS.replicate 16 0) blank
                `shouldSatisfy` isLeft

        it "refuses a tile index outside the atlas" $ do
            patchAtlasTile atlasW tilesPerRow 4 (block 1) blank
                `shouldSatisfy` isLeft
            patchAtlasTile atlasW tilesPerRow (-1) (block 1) blank
                `shouldSatisfy` isLeft

-- * Live fixtures ---------------------------------------------------

-- | Every example generates its OWN private w8 page. They EDIT their
--   page and hand-deliver batches to it, so sharing one would let an
--   earlier example's generation decide a later one's admission.
commitPageId, siblingPageId, stalePageId, mixedPageId ∷ WorldPageId
crossPageId, ackPageId, missingMatPageId, zoomPageId ∷ WorldPageId
commitPageId     = WorldPageId "solid_commit_w8"
siblingPageId    = WorldPageId "solid_sibling_w8"
stalePageId      = WorldPageId "solid_stale_w8"
crossPageId      = WorldPageId "solid_cross_w8"
mixedPageId      = WorldPageId "solid_mixed_w8"
ackPageId        = WorldPageId "solid_ack_w8"
missingMatPageId = WorldPageId "solid_nomat_w8"
zoomPageId       = WorldPageId "solid_zoom_w8"

ackTimeoutMicros ∷ Int
ackTimeoutMicros = 30 * 1000 * 1000

-- | A generated page plus two of its loaded chunks.
data LivePage = LivePage
    { lpState  ∷ WorldState
    , lpLava   ∷ ChunkCoord
    , lpWater  ∷ ChunkCoord
    , lpBefore ∷ LoadedChunk   -- ^ 'lpLava' as generation left it.
    }

livePage ∷ EngineEnv → WorldPageId → IO LivePage
livePage env pageId = do
    sendWorldCommand env (WorldInit pageId 45 8 3 Nothing)
    ws ← waitForWorldInit env pageId 120
    td ← readIORef (wsTilesRef ws)
    let coords = sort (HM.keys (wtdChunks td))
    case coords of
        (lava : water : _) → do
            before ← chunkAt ws lava
            pure (LivePage ws lava water before)
        _ → expectationFailure "fixture: page has fewer than two chunks"
            ≫ error "unreachable"

chunkAt ∷ WorldState → ChunkCoord → IO LoadedChunk
chunkAt ws coord = do
    td ← readIORef (wsTilesRef ws)
    case lookupChunk coord td of
        Just lc → pure lc
        Nothing → expectationFailure ("chunk not loaded: " ⧺ show coord)
                  ≫ error "unreachable"

-- | Deliver one batch stamped with the page's OWN incarnation and wait
--   for the world thread to finish with it. Every example here is about
--   a decision BEHIND the #2477 fence, so each batch has to clear it.
deliver ∷ EngineEnv → WorldState → WorldPageId → [FluidWriteback]
        → [ReactionResult] → IO ()
deliver env ws pageId writebacks reactions = do
    epoch ← pageIncarnation ws
    ack ← newEmptyMVar
    sendWorldCommand env
        (WorldApplyFluids
            (FluidWritebackBatch pageId (Just epoch) writebacks reactions
                                 (Just ack)))
    awaitAck ack

awaitAck ∷ MVar FluidAckOutcome → IO ()
awaitAck ack = do
    got ← timeout ackTimeoutMicros (takeMVar ack)
    case got of
        Just FluidAckApplied → pure ()
        Just (FluidAckFailed why) → expectationFailure
            ("world thread failed a WorldApplyFluids batch: " ⧺ T.unpack why)
        Nothing → expectationFailure
            "world thread never acked a WorldApplyFluids batch"

-- | A FIFO barrier: an empty acked batch proves every command sent
--   before it has been handled, without asserting anything about what
--   they did.
barrier ∷ EngineEnv → WorldPageId → IO ()
barrier env pageId = do
    ack ← newEmptyMVar
    sendWorldCommand env
        (WorldApplyFluids (FluidWritebackBatch pageId Nothing [] [] (Just ack)))
    awaitAck ack

-- | An event naming one LOCAL cell of a loaded chunk.
liveEvent ∷ ChunkCoord → (Int, Int) → ChunkCoord → SolidProduct
          → SolidificationEvent
liveEvent lavaChunk (lx, ly) waterChunk product' =
    (eventAt lavaChunk (columnIndex lx ly) waterChunk product')
        { sevWaterType = if product' ≡ SolidBasalt then Ocean else Lake }

-- | Every 'WeAddTile' recorded for one chunk.
addTilesFor ∷ WorldState → ChunkCoord → IO [WorldEdit]
addTilesFor ws coord = do
    edits ← readIORef (wsEditsRef ws)
    pure [ e | e@(WeAddTile _ _ _) ← HM.lookupDefault [] coord edits ]

-- | The reaction-commit messages the world thread published, read off
--   the REAL sim queue (nothing drains it here).
simCommands ∷ EngineEnv → IO [SimCommand]
simCommands env = Q.flushQueue (simQueue env)

materialFor ∷ EngineEnv → SolidProduct → IO MaterialId
materialFor env product' = do
    registry ← readIORef (wsMaterialRegistryRef (toWorldSimCapability env))
    case stoneMaterialFor registry product' of
        Right mid → pure mid
        Left why  → expectationFailure (T.unpack why) ≫ error "unreachable"

topMaterialAt ∷ LoadedChunk → Int → Word8
topMaterialAt lc idx =
    let topZ = lcTerrainSurfaceMap lc VU.! idx
        col  = lcTiles lc V.! idx
    in ctMats col VU.! (topZ - ctStartZ col)

spec ∷ SpecWith EngineEnv
spec = describe "solidification (#2485)" $ do

    it "commits an admitted result as a durable WeAddTile of the chosen \
       \stone, visible in the tiles and in the edit log" $ \env → do
        lp ← livePage env commitPageId
        basalt ← materialFor env SolidBasalt
        let idx = columnIndex 4 4
            ev  = liveEvent (lpLava lp) (4, 4) (lpLava lp) SolidBasalt
            rr  = ReactionResult [(lpLava lp, 0)] [ev]
        _ ← simCommands env   -- clear the queue of init chatter
        deliver env (lpState lp) commitPageId [] [rr]

        after ← chunkAt (lpState lp) (lpLava lp)
        -- Requirement 1: the terrain rose by exactly one z of the
        -- selected material, through the same edit a player's add-tile
        -- makes.
        lcTerrainSurfaceMap after VU.! idx
            `shouldBe` lcTerrainSurfaceMap (lpBefore lp) VU.! idx + 1
        topMaterialAt after idx `shouldBe` unMaterialId basalt
        -- …and it is in the DURABLE log, not only in memory.
        adds ← addTilesFor (lpState lp) (lpLava lp)
        adds `shouldBe` [uncurry WeAddTile (reactionEventTile ev) basalt]

        -- Requirement 10: replaying that log over the freshly generated
        -- chunk — exactly what chunk loading does after an eviction —
        -- reproduces both the height and the product material.
        edits ← readIORef (wsEditsRef (lpState lp))
        let replayed = replayEdits edits (lpBefore lp)
        lcTerrainSurfaceMap replayed VU.! idx
            `shouldBe` lcTerrainSurfaceMap after VU.! idx
        topMaterialAt replayed idx `shouldBe` unMaterialId basalt

    it "advances the chunk's generation ONCE for two sibling events \
       \admitted from the same pre-commit generation, and appends both" $
      \env → do
        lp ← livePage env siblingPageId
        obsidian ← materialFor env SolidObsidian
        let idxA = columnIndex 4 4
            idxB = columnIndex 5 4
            evA = liveEvent (lpLava lp) (4, 4) (lpLava lp) SolidObsidian
            evB = liveEvent (lpLava lp) (5, 4) (lpLava lp) SolidObsidian
            -- ONE result, as 'groupReactionResults' produces for two
            -- events in one chunk: admitted together, against the
            -- generation that stood BEFORE either landed.
            rr = ReactionResult [(lpLava lp, 0)] [evA, evB]
        _ ← simCommands env
        deliver env (lpState lp) siblingPageId [] [rr]

        after ← chunkAt (lpState lp) (lpLava lp)
        -- Requirement 5: the first stone did not stale the second.
        forM_ [idxA, idxB] $ \i → do
            lcTerrainSurfaceMap after VU.! i
                `shouldBe` lcTerrainSurfaceMap (lpBefore lp) VU.! i + 1
            topMaterialAt after i `shouldBe` unMaterialId obsidian
        adds ← addTilesFor (lpState lp) (lpLava lp)
        length adds `shouldBe` 2

        -- ONE bump, not one per event, and one sim handoff naming both
        -- solidified cells at that single post-commit generation.
        gens ← readIORef (wsChunkEditGenRef (lpState lp))
        HM.lookupDefault 0 (lpLava lp) gens `shouldBe` 1
        cmds ← simCommands env
        case [ syncs | SimReactionCommitted p _ _ syncs ← cmds
                     , p ≡ siblingPageId ] of
            [syncs] → case find ((≡ lpLava lp) . rcsCoord) syncs of
                Nothing → expectationFailure
                    "the commit named no sync for the edited chunk"
                Just sync → do
                    rcsEditGen sync `shouldBe` 1
                    sort (rcsSolidified sync) `shouldBe` sort [idxA, idxB]
            other → expectationFailure
                ("expected exactly one SimReactionCommitted, got "
                 ⧺ show (length other))

    it "commits a cross-chunk contact whole: the stone lands in the lava \
       \chunk and the sim handoff accounts for the water chunk too" $
      \env → do
        lp ← livePage env crossPageId
        basalt ← materialFor env SolidBasalt
        let idx = columnIndex 4 4
            -- The lava cell is in one chunk and the water cell it reacted
            -- with is in another, which is what a seam or boundary
            -- contact looks like by the time it reaches the world thread.
            ev = (liveEvent (lpLava lp) (4, 4) (lpWater lp) SolidBasalt)
                     { sevWaterType = Ocean }
            rr = ReactionResult [(lpLava lp, 0), (lpWater lp, 0)] [ev]
        _ ← simCommands env
        deliver env (lpState lp) crossPageId [] [rr]

        after ← chunkAt (lpState lp) (lpLava lp)
        lcTerrainSurfaceMap after VU.! idx
            `shouldBe` lcTerrainSurfaceMap (lpBefore lp) VU.! idx + 1
        topMaterialAt after idx `shouldBe` unMaterialId basalt

        -- Requirement 6: a result spanning two chunks cannot partially
        -- commit one side, so the handoff names BOTH — the lava chunk
        -- with its new generation and its solidified cell, and the water
        -- chunk with the generation it still has and no stone.
        cmds ← simCommands env
        gens ← readIORef (wsChunkEditGenRef (lpState lp))
        case [ syncs | SimReactionCommitted p _ _ syncs ← cmds
                     , p ≡ crossPageId ] of
            [syncs] → do
                sort (map rcsCoord syncs)
                    `shouldBe` sort [lpLava lp, lpWater lp]
                case find ((≡ lpLava lp) . rcsCoord) syncs of
                    Nothing → expectationFailure "no sync for the lava chunk"
                    Just sync → do
                        rcsEditGen sync `shouldBe` 1
                        rcsSolidified sync `shouldBe` [idx]
                case find ((≡ lpWater lp) . rcsCoord) syncs of
                    Nothing → expectationFailure "no sync for the water chunk"
                    Just sync → do
                        -- Nothing about it changed, so its generation is
                        -- untouched: bumping it would fence out its own
                        -- in-flight writebacks for no reason.
                        rcsEditGen sync
                            `shouldBe` HM.lookupDefault 0 (lpWater lp) gens
                        rcsSolidified sync `shouldBe` []
            other → expectationFailure
                ("expected exactly one SimReactionCommitted, got "
                 ⧺ show (length other))

    it "rejects a cross-chunk result whose water side was edited in the \
       \meantime, keeps that newer edit, drops the reaction's own fluid \
       \with it, and re-seeds both participants from the live tiles" $
      \env → do
        lp ← livePage env stalePageId
        let lavaIdx  = columnIndex 4 4
            waterIdx = columnIndex 6 6
            (wgx, wgy) = chunkToGlobal (lpWater lp) 6 6
        loam ← pure matLoam

        -- The intervening edit: a real live add-tile on the WATER side,
        -- which mints a new generation for that chunk.
        sendWorldCommand env (WorldAddTile stalePageId wgx wgy loam)
        barrier env stalePageId
        waterAfterEdit ← chunkAt (lpState lp) (lpWater lp)
        lavaBefore ← chunkAt (lpState lp) (lpLava lp)
        _ ← simCommands env

        -- The losing race: a result computed before that edit, claiming
        -- generation 0 for both sides, delivered with the writeback that
        -- carries its own annihilated fluid.
        let ev = liveEvent (lpLava lp) (4, 4) (lpWater lp) SolidBasalt
            rr = ReactionResult [(lpLava lp, 0), (lpWater lp, 0)] [ev]
            annihilated = FluidWriteback
                { fwCoord    = lpLava lp
                , fwEditGen  = 0
                , fwFluid    = V.replicate (chunkSize * chunkSize) Nothing
                , fwTerrain  = lcTerrainSurfaceMap lavaBefore
                , fwSurf     = lcSurfaceMap lavaBefore
                , fwSideDeco = VU.replicate (chunkSize * chunkSize) 0x5A
                }
        deliver env (lpState lp) stalePageId [annihilated] [rr]

        lavaAfter ← chunkAt (lpState lp) (lpLava lp)
        waterAfter ← chunkAt (lpState lp) (lpWater lp)
        -- No stone, anywhere: rejection is in FULL, so the lava side
        -- cannot land its half of a result the water side refused.
        lcTerrainSurfaceMap lavaAfter VU.! lavaIdx
            `shouldBe` lcTerrainSurfaceMap lavaBefore VU.! lavaIdx
        adds ← addTilesFor (lpState lp) (lpLava lp)
        adds `shouldBe` []
        -- The newer edit is untouched.
        lcTerrainSurfaceMap waterAfter VU.! waterIdx
            `shouldBe` lcTerrainSurfaceMap waterAfterEdit VU.! waterIdx
        -- And the rejected result's OWN fluid went with it: applying
        -- that writeback would have recorded the annihilation with no
        -- stone to account for it, and then been the state the
        -- convergence re-seed read back as authoritative.
        lcFluidMap lavaAfter `shouldBe` lcFluidMap lavaBefore
        lcSideDeco lavaAfter `shouldBe` lcSideDeco lavaBefore

        -- Convergence: BOTH participants are re-seeded from the live
        -- tiles, including the lava chunk no edit touched — otherwise
        -- its sim grid would keep the lava the discarded contact
        -- consumed, with no product.
        cmds ← simCommands env
        let reseeded = [ c | SimChunkEdited p _ _ c _ _ _ ← cmds
                           , p ≡ stalePageId ]
        sort reseeded `shouldSatisfy` \cs →
            lpLava lp `elem` cs ∧ lpWater lp `elem` cs
        [ () | SimReactionCommitted p _ _ _ ← cmds, p ≡ stalePageId ]
            `shouldBe` []

    it "commits an unrelated fresh result in the same delivery, and \
       \refuses the writeback a later pre-commit output carries" $
      \env → do
        lp ← livePage env mixedPageId
        basalt ← materialFor env SolidBasalt
        let freshIdx = columnIndex 8 8
            staleIdx = columnIndex 2 2
            (wgx, wgy) = chunkToGlobal (lpWater lp) 9 9
            staleRR = ReactionResult
                        [(lpWater lp, 0)]
                        [liveEvent (lpWater lp) (2, 2) (lpWater lp) SolidBasalt]
            freshRR = ReactionResult
                        [(lpLava lp, 0)]
                        [liveEvent (lpLava lp) (8, 8) (lpLava lp) SolidBasalt]
        -- Move the WATER chunk's generation on with a real live edit, so
        -- 'staleRR' is stale on arrival and 'freshRR' is not. The two
        -- results share no chunk, which is what makes them independently
        -- eligible.
        sendWorldCommand env (WorldAddTile mixedPageId wgx wgy matLoam)
        barrier env mixedPageId
        beforeLava ← chunkAt (lpState lp) (lpLava lp)
        beforeWater ← chunkAt (lpState lp) (lpWater lp)
        _ ← simCommands env
        deliver env (lpState lp) mixedPageId [] [staleRR, freshRR]

        afterWater ← chunkAt (lpState lp) (lpWater lp)
        lcTerrainSurfaceMap afterWater VU.! staleIdx
            `shouldBe` lcTerrainSurfaceMap beforeWater VU.! staleIdx

        afterLava ← chunkAt (lpState lp) (lpLava lp)
        lcTerrainSurfaceMap afterLava VU.! freshIdx
            `shouldBe` lcTerrainSurfaceMap beforeLava VU.! freshIdx + 1
        topMaterialAt afterLava freshIdx `shouldBe` unMaterialId basalt

        -- Requirement 7: output computed from the PRE-commit generation
        -- is refused now, so the commit's own terrain cannot be undone
        -- by a writeback still in flight from before it.
        gens ← readIORef (wsChunkEditGenRef (lpState lp))
        HM.lookupDefault 0 (lpLava lp) gens `shouldSatisfy` (> 0)
        let precommit = FluidWriteback
                { fwCoord    = lpLava lp
                , fwEditGen  = 0
                , fwFluid    = lcFluidMap beforeLava
                , fwTerrain  = lcTerrainSurfaceMap beforeLava
                , fwSurf     = lcSurfaceMap beforeLava
                , fwSideDeco = VU.replicate (chunkSize * chunkSize) 0x7C
                }
        deliver env (lpState lp) mixedPageId [precommit] []
        settled ← chunkAt (lpState lp) (lpLava lp)
        lcTerrainSurfaceMap settled VU.! freshIdx
            `shouldBe` lcTerrainSurfaceMap afterLava VU.! freshIdx
        lcSideDeco settled `shouldBe` lcSideDeco afterLava

    it "acknowledges the delivery only after the stone is already in the \
       \tiles, so a fast settle never reads a half-applied commit" $
      \env → do
        lp ← livePage env ackPageId
        let idx = columnIndex 9 9
            rr = ReactionResult [(lpLava lp, 0)]
                    [liveEvent (lpLava lp) (9, 9) (lpLava lp) SolidObsidian]
        before ← chunkAt (lpState lp) (lpLava lp)
        -- The ack is filled only after the application step returns, so
        -- reading the tiles the instant it arrives proves the ordering.
        deliver env (lpState lp) ackPageId [] [rr]
        after ← chunkAt (lpState lp) (lpLava lp)
        lcTerrainSurfaceMap after VU.! idx
            `shouldBe` lcTerrainSurfaceMap before VU.! idx + 1

    it "drops the stone loudly when the registry cannot name the product \
       \material, rather than committing the reaction's fluid alone" $
      \env → do
        lp ← livePage env missingMatPageId
        let registryRef = wsMaterialRegistryRef (toWorldSimCapability env)
            idx = columnIndex 4 4
            rr = ReactionResult [(lpLava lp, 0)]
                    [liveEvent (lpLava lp) (4, 4) (lpLava lp) SolidBasalt]
        real ← readIORef registryRef
        before ← chunkAt (lpState lp) (lpLava lp)
        (`finally` writeIORef registryRef real) $ do
            -- The commit must CONSULT the registry; a hardcoded id would
            -- survive the pure resolution test above and fail here.
            writeIORef registryRef emptyMaterialRegistry
            deliver env (lpState lp) missingMatPageId [] [rr]
        after ← chunkAt (lpState lp) (lpLava lp)
        lcTerrainSurfaceMap after VU.! idx
            `shouldBe` lcTerrainSurfaceMap before VU.! idx
        adds ← addTilesFor (lpState lp) (lpLava lp)
        adds `shouldBe` []
        -- …and the real registry does know both names, so the example
        -- above is about the missing case and not a broken fixture.
        materialIdByName real (stoneMaterialName SolidBasalt)
            `shouldSatisfy` isJust
        materialIdByName real (stoneMaterialName SolidObsidian)
            `shouldSatisfy` isJust

    it "regenerates the affected chunk's zoom terrain pixels and \
       \republishes the atlas the renderer samples" $ \env → do
        lp ← livePage env zoomPageId
        mLive ← readIORef (wsZoomLiveRef (lpState lp))
        live ← case mLive of
            Just l  → pure l
            Nothing → expectationFailure
                "fixture: this page published no zoom atlas to refresh"
                ≫ error "unreachable"
        cache ← readIORef (wsZoomCacheRef (lpState lp))
        tileIdx ← case atlasTileIndexFor cache (lpLava lp) of
            Just i  → pure i
            Nothing → expectationFailure
                "fixture: the edited chunk is not in this page's zoom cache"
                ≫ error "unreachable"

        -- Clear the handoff so what is read back afterwards can only be
        -- this commit's own republication.
        writeIORef (zoomAtlasDataRef env) Nothing
        let idx = columnIndex 10 10
            rr = ReactionResult [(lpLava lp, 0)]
                    [liveEvent (lpLava lp) (10, 10) (lpLava lp) SolidBasalt]
        deliver env (lpState lp) zoomPageId [] [rr]

        -- The retained pixels changed, and only inside the edited
        -- chunk's own tile.
        patched ← readIORef (wsZoomLiveRef (lpState lp)) ⌦ \m → case m of
            Just l  → pure l
            Nothing → expectationFailure "the refresh dropped the atlas"
                      ≫ error "unreachable"
        BS.length (zlaPixels patched) `shouldBe` BS.length (zlaPixels live)
        zlaPixels patched `shouldNotBe` zlaPixels live
        changedTiles (zlaWidth live) (zlaChunksPerRow live)
                     (zlaPixels live) (zlaPixels patched)
            `shouldBe` [tileIdx]

        -- …and the whole image was handed to the upload path, targeted
        -- at exactly the page that accepted the edit (#763 / #1670).
        published ← readIORef (zoomAtlasDataRef env)
        case published of
            Nothing → expectationFailure
                "the commit published no zoom atlas for upload"
            Just (w, h, bytes, targets) → do
                w `shouldBe` zlaWidth patched
                h `shouldBe` zlaHeight patched
                bytes `shouldBe` zlaPixels patched
                length targets `shouldBe` 1

        -- The override the refresh fed the generator is the LIVE column,
        -- which is what makes the new pixels show the stone rather than
        -- generation-time terrain.
        after ← chunkAt (lpState lp) (lpLava lp)
        basalt ← materialFor env SolidBasalt
        case zoomTileOverrideFor after idx of
            Nothing → expectationFailure "no override for the solidified cell"
            Just o  → do
                ztoElev o `shouldBe` lcTerrainSurfaceMap after VU.! idx
                ztoMaterial o `shouldBe` unMaterialId basalt
                ztoFluid o `shouldBe` Nothing

-- | Which atlas tiles differ between two images of the same size.
changedTiles ∷ Int → Int → BS.ByteString → BS.ByteString → [Int]
changedTiles atlasW chunksPerRow old new =
    [ idx
    | idx ← [0 .. tileCount - 1]
    , any (rowDiffers idx) [0 .. zoomTileSize - 1] ]
  where
    tileCount = (BS.length old `div` 4) `div` (zoomTileSize * zoomTileSize)
    rowDiffers idx ty =
        let col = idx `mod` chunksPerRow
            row = idx `div` chunksPerRow
            off = ((row * zoomTileSize + ty) * atlasW + col * zoomTileSize) * 4
            len = zoomTileSize * 4
        in BS.take len (BS.drop off old) ≢ BS.take len (BS.drop off new)

isLeft ∷ Either α β → Bool
isLeft (Left _)  = True
isLeft (Right _) = False
