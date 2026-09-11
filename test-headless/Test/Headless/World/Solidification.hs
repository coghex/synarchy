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
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar)
import Control.Exception (finally)
import Control.Monad.ST (runST)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, sort, sortOn)
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
import Engine.Core.State
    ( EngineEnv(..), ZoomAtlasUpload(..), queueZoomAtlasUpload
    , replaceZoomAtlasTextures, retireZoomAtlasTextures, zoomAtlasDataRef )
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import World.Grid (gridToWorld)
import World.Chunk.Admit (pageIncarnation)
import qualified Engine.Core.Queue as Q
import Sim.Chunk (applyChunkEdit, applyReactionCommit)
import Sim.Command.Types (SimCommand(..), ReactionChunkSync(..))
import Sim.Fluid.Reaction
    ( CellSite(..), ReactionResult(..), SolidProduct(..)
    , SolidificationEvent(..), TransferOutcome(..)
    , applyTransfer, groupReactionResults, solidProductFor )
import Sim.Fluid.Types
    (ActiveFluidCell(..), activeToFluidCell, fluidCellToActive)
import Sim.State.Types
    (SimChunkState(..), SimState(..), SimWorldState(..)
    , emptySimState, emptySimWorldState)
import Sim.Thread (drainReactionResults, handleSimCommand)
import Sim.Topology (SimTopology(..))
import Test.Headless.Harness (sendWorldCommand, waitForWorldInit)
import World.Edit.Apply (replayEdits)
import World.Edit.Types (WorldEdit(..))
import World.Generate.Coordinates (chunkToGlobal, globalToChunk)
import World.Material
    (MaterialId(..), emptyMaterialRegistry, materialIdByName, matLoam)
import World.Reaction.Stone (stoneMaterialFor, stoneMaterialName)
import World.Thread.Command.Reaction
    ( ReactionAdmission(..), ReactionRefusal(..), admitReaction
    , reactionChunks, reactionEventTile, reactionIsFresh )
import World.Thread.Command.Reaction.Zoom (atlasTileIndexFor)
import World.Render.Zoom.Project (zoomTexelExtent, zoomTileScreenRect)
import World.ZoomMap.Live
    (ZoomTileOverride(..), liveChunkZoom, liveTileOverrides, patchAtlasTile)
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
    , sevWaterChunks  = [waterChunk]
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
                fmap sevWaterChunks lavaSrc `shouldBe` Just [chunkB]
                fmap sevWaterChunks lavaDst `shouldBe` Just [chunkB]

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

        it "names every chunk a twice-reacting coordinate took fluid \
           \from, not just the first" $ do
            -- One stone, two contacts: exhausted against an in-chunk
            -- neighbour, refilled with lava, exhausted again across the
            -- seam. Both chunks' consumed-fluid writebacks ride the same
            -- delivery, so both have to be inside this result's own
            -- admission.
            let ev = (eventAt chunkA 10 chunkA SolidBasalt)
                         { sevWaterChunks = [chunkA, chunkB] }
            map rrParticipants (groupReactionResults gens [ev])
                `shouldBe` [[(chunkA, 7), (chunkB, 9)]]

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

        it "DISPLACES one level from a solidified cell that was refilled \
           \in the same tick, rather than emptying it" $ do
            -- A cell emptied by annihilation is an ordinary empty
            -- destination for the rest of that tick (#2481), so the cell
            -- an event names can be holding water again by the time the
            -- commit lands — and 'World.Edit.Apply' keeps fluid whose
            -- surface stays above the raised terrain. Clearing it would
            -- delete water the page's own tiles still record.
            let deep = ActiveFluidCell Lake 20 0
                refilled = simWorldWith
                    [ (chunkA, activeChunk 3 [ (stoneIdx, Just deep) ]) ]
                committed = applyReactionCommit chunkA 4 passive terrain
                                                [stoneIdx] refilled
                grid = scsActiveFluid (swsChunks committed HM.! chunkA)
            -- One z of terrain arrived under it, so exactly one level's
            -- worth of volume no longer fits.
            grid V.! stoneIdx `shouldBe` Just deep { afcVolume = 20 - 7 }

        it "empties a solidified cell whose fluid the new stone reaches" $ do
            -- …and the other side of the same rule: a cell no deeper
            -- than the level just filled is displaced entirely, which is
            -- what 'World.Edit.Apply' does to the passive cell.
            let shallow = ActiveFluidCell Lake 7 0
                refilled = simWorldWith
                    [ (chunkA, activeChunk 3 [ (stoneIdx, Just shallow) ]) ]
                committed = applyReactionCommit chunkA 4 passive terrain
                                                [stoneIdx] refilled
            scsActiveFluid (swsChunks committed HM.! chunkA) V.! stoneIdx
                `shouldBe` Nothing

        it "does NOT displace an INACTIVE chunk a second time: its \
           \passive map is already post-edit" $ do
            -- Reached in ordinary play, not only in a fixture: a
            -- synchronous fast settle drains reaction results only after
            -- settling its chunks inactive. 'rcsFluid' is read AFTER the
            -- WeAddTile, so 'World.Edit.Apply' has already taken the
            -- level the new stone fills; taking another here would
            -- charge a deep cell twice.
            let deep = ActiveFluidCell Lake 20 0
                -- What the post-edit tiles say at the solidified cell:
                -- the stone's terrain top with the surviving fluid above
                -- it. Its surface is what 'applyEdit' left.
                postEdit = V.replicate cellsPerChunk Nothing
                    V.// [(stoneIdx, activeToFluidCell 1 deep)]
                inactive = simWorldWith
                    [ (chunkA, (activeChunk 3 []) { scsActive = False }) ]
                committed = applyReactionCommit chunkA 4 postEdit terrain
                                                [stoneIdx] inactive
                grid = scsActiveFluid (swsChunks committed HM.! chunkA)
            grid V.! stoneIdx
                `shouldBe` (fluidCellToActive 1
                                =≪ (postEdit V.! stoneIdx))

        it "re-seeds an INACTIVE chunk from the passive map instead, \
           \which is the only truth it has" $ do
            let inactive = simWorldWith
                    [ (chunkA, (activeChunk 3 []) { scsActive = False }) ]
                committed = applyReactionCommit chunkA 4 passive terrain
                                                [stoneIdx] inactive
                grid = scsActiveFluid (swsChunks committed HM.! chunkA)
            fmap afcVolume (grid V.! waterIdx) `shouldBe` Just 7
            grid V.! stoneIdx `shouldBe` Nothing

    describe "queueing a page's zoom atlas upload" $ do
        let pageA = WorldPageId "a"
            pageB = WorldPageId "b"
            upload pid n = ZoomAtlasUpload n n (BS.singleton 1) pid []

        it "supersedes that PAGE's own pending image and leaves others" $
            map zauPage (queueZoomAtlasUpload (upload pageA 2)
                            [upload pageA 1, upload pageB 1])
                `shouldBe` [pageB, pageA]

        it "keys supersession by the page id, not by the target states, \
           \so a same-id reinitialization replaces the previous \
           \incarnation's image rather than queueing beside it" $ do
            -- A reinit builds a fresh WorldState with fresh refs, so a
            -- rule keyed on those would leave the old incarnation's
            -- image queued for a page that no longer exists.
            let reinit = ZoomAtlasUpload 3 3 (BS.singleton 2) pageA []
                queued = queueZoomAtlasUpload reinit [upload pageA 1]
            map zauPage queued `shouldBe` [pageA]
            map zauWidth queued `shouldBe` [3]

    describe "installing a page's zoom atlas upload" $ do
        let a = 0 ∷ Int
            b = 1 ∷ Int

        it "retires only the entries the upload's OWN targets held" $ do
            -- A single slot disposed "the" previous atlas, so uploading
            -- page B's image destroyed the texture page A's
            -- 'wsZoomAtlasRef' still named and left A sampling a dead
            -- handle.
            let (installed, retired) =
                    replaceZoomAtlasTextures [b] ("texB" ∷ String) [(a, "texA")]
            installed `shouldBe` [(a, "texA"), (b, "texB")]
            retired `shouldBe` []

        it "retires the previous texture when the SAME page uploads again" $ do
            let (installed, retired) =
                    replaceZoomAtlasTextures [a] "texA2"
                        [(a, "texA1"), (b, "texB")]
            installed `shouldBe` [(b, "texB"), (a, "texA2")]
            retired `shouldBe` ["texA1"]

        it "installs one entry per target page" $ do
            let (installed, retired) =
                    replaceZoomAtlasTextures [a, b] "shared" []
            installed `shouldBe` [(a, "shared"), (b, "shared")]
            retired `shouldBe` []

        it "retires the entry of a page that no longer exists" $ do
            -- Nothing uploads for a destroyed, reinitialized or replaced
            -- page, so without this its entry keeps a GPU image, view,
            -- sampler and bindless slot alive until shutdown and every
            -- load leaks one atlas.
            let (kept, retired) =
                    retireZoomAtlasTextures [a] [(a, "texA"), (b, "texB")]
            kept `shouldBe` [(a, "texA")]
            retired `shouldBe` ["texB"]

        it "retires nothing while every page is still live" $
            retireZoomAtlasTextures [a, b] [(a, "texA"), (b, "texB")]
                `shouldBe` ([(a, "texA"), (b, "texB")], [] ∷ [String])

    describe "projecting a tile onto the zoom map" $ do
        let facing = FaceSouth
            rect gx gy = zoomTileScreenRect facing 2.0 0 0 1024 768
                                            1024 768 8 gx gy

        it "gives every tile of a chunk a texel box inside the block" $
            -- The block is a square holding a diamond, so a LOCAL
            -- coordinate the diamond does not reach has no texels — and
            -- says so rather than returning a plausible empty rectangle.
            forM_ [ (lx, ly) | ly ← [0 .. chunkSize - 1]
                             , lx ← [0 .. chunkSize - 1] ] $ \(lx, ly) →
                case zoomTexelExtent lx ly of
                    Nothing → expectationFailure
                        ("no atlas texels for local tile " ⧺ show (lx, ly))
                    Just (x0, y0, x1, y1) → do
                        (x0, y0) `shouldSatisfy` \(a, b) → a ≥ 0 ∧ b ≥ 0
                        (x1, y1) `shouldSatisfy` \(a, b) →
                            a < zoomTileSize ∧ b < zoomTileSize
                        (x0 ≤ x1 ∧ y0 ≤ y1) `shouldBe` True

        it "answers nothing for a local coordinate outside the chunk" $ do
            zoomTexelExtent (-1) 0 `shouldBe` Nothing
            zoomTexelExtent 0 chunkSize `shouldBe` Nothing

        it "places a tile's rectangle INSIDE its own chunk's block" $ do
            -- The projection is the map's, not the detail view's: it
            -- never consults terrain height, so a tile's rectangle is
            -- where its texels are drawn rather than where its column
            -- would be picked.
            let block = (,) <$> rect 0 0 <*> rect (chunkSize - 1) (chunkSize - 1)
            block `shouldSatisfy` isJust

        it "separates two tiles of the same chunk" $ do
            -- Adjacent tiles must not project onto the same pixels, or
            -- a per-tile assertion could be satisfied by its neighbour.
            let a = rect 0 0
                b = rect 4 4
            (a ≢ b) `shouldBe` True
            a `shouldSatisfy` isJust
            b `shouldSatisfy` isJust

        it "refuses a degenerate viewport rather than unprojecting to \
           \a garbage rectangle" $
            zoomTileScreenRect facing 2.0 0 0 0 0 0 0 8 0 0
                `shouldBe` Nothing

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
coherentPageId, evictedPageId, cumulativePageId, bareZoomPageId ∷ WorldPageId
regenPageId, queueAPageId, queueBPageId, initPageId ∷ WorldPageId
multiChunkPageId, initOtherPageId, noMapPageId ∷ WorldPageId
twiceReactedPageId, icePageId ∷ WorldPageId
commitPageId     = WorldPageId "solid_commit_w8"
siblingPageId    = WorldPageId "solid_sibling_w8"
stalePageId      = WorldPageId "solid_stale_w8"
crossPageId      = WorldPageId "solid_cross_w8"
mixedPageId      = WorldPageId "solid_mixed_w8"
ackPageId        = WorldPageId "solid_ack_w8"
missingMatPageId = WorldPageId "solid_nomat_w8"
zoomPageId       = WorldPageId "solid_zoom_w8"
coherentPageId   = WorldPageId "solid_coherent_w8"
evictedPageId    = WorldPageId "solid_evicted_w8"
cumulativePageId = WorldPageId "solid_cumulative_w8"
bareZoomPageId   = WorldPageId "solid_barezoom_w8"
regenPageId      = WorldPageId "solid_regen_w8"
queueAPageId     = WorldPageId "solid_queue_a_w8"
queueBPageId     = WorldPageId "solid_queue_b_w8"
initPageId       = WorldPageId "solid_init_w8"
initOtherPageId  = WorldPageId "solid_init_other_w8"
noMapPageId      = WorldPageId "solid_nomap_w8"
twiceReactedPageId = WorldPageId "solid_twice_w8"
icePageId        = WorldPageId "solid_ice_w8"
multiChunkPageId = WorldPageId "solid_multichunk_w8"

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
livePage env pageId = livePageSized env pageId 8

-- | 'livePage' on a page of the given world size in chunks.
--
--   Size is a parameter for exactly one example: a w8 page is only eight
--   chunks around in u, so the camera's own five-chunk keep window wraps
--   back onto itself and NOTHING can ever fall outside it. An eviction
--   example has to be given a world big enough for the camera to leave a
--   chunk behind.
livePageSized ∷ EngineEnv → WorldPageId → Int → IO LivePage
livePageSized env pageId worldSize = do
    sendWorldCommand env (WorldInit pageId 45 worldSize 3 Nothing)
    ws ← waitForWorldInit env pageId 300
    td ← readIORef (wsTilesRef ws)
    -- Nearest the origin first, not lexicographically first: a corner
    -- chunk of a small page can sit beyond the glacier, where most
    -- columns carry the sentinel elevation and a summary-entry
    -- assertion is about the boundary rather than about the stone.
    let coords = sortOn (\(ChunkCoord cx cy) → (cx * cx + cy * cy, cx, cy))
                        (HM.keys (wtdChunks td))
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

    it "commits a cross-chunk contact whole: both sides' fluid and the \
       \stone land in ONE delivery, and the handoff keeps the water \
       \chunk's exact remainder" $ \env → do
        lp ← livePage env crossPageId
        basalt ← materialFor env SolidBasalt
        waterBefore ← chunkAt (lpState lp) (lpWater lp)
        let idx = columnIndex 4 4
            waterIdx = columnIndex 7 7
            -- The lava cell is in one chunk and the water cell it
            -- reacted with is in another, which is what a seam or
            -- boundary contact looks like by the time it reaches the
            -- world thread.
            ev = (liveEvent (lpLava lp) (4, 4) (lpWater lp) SolidBasalt)
                     { sevWaterType = Ocean }
            rr = ReactionResult [(lpLava lp, 0), (lpWater lp, 0)] [ev]
            -- One unit of water is left on the far side — deliberately
            -- not a multiple of 'volumePerLevel'.
            remainder = ActiveFluidCell Ocean 1 0
            lavaWb = FluidWriteback
                { fwCoord    = lpLava lp
                , fwEditGen  = 0
                , fwFluid    = V.replicate cellsPerChunk Nothing
                , fwTerrain  = lcTerrainSurfaceMap (lpBefore lp)
                , fwSurf     = lcSurfaceMap (lpBefore lp)
                , fwSideDeco = VU.replicate cellsPerChunk 0x2A
                }
            waterWb = FluidWriteback
                { fwCoord    = lpWater lp
                , fwEditGen  = 0
                , fwFluid    = V.replicate cellsPerChunk Nothing
                    V.// [(waterIdx, activeToFluidCell 0 remainder)]
                , fwTerrain  = lcTerrainSurfaceMap waterBefore
                , fwSurf     = lcSurfaceMap waterBefore
                , fwSideDeco = VU.replicate cellsPerChunk 0x2B
                }
        _ ← simCommands env
        deliver env (lpState lp) crossPageId [lavaWb, waterWb] [rr]

        after ← chunkAt (lpState lp) (lpLava lp)
        waterAfter ← chunkAt (lpState lp) (lpWater lp)
        -- The stone on one side…
        lcTerrainSurfaceMap after VU.! idx
            `shouldBe` lcTerrainSurfaceMap (lpBefore lp) VU.! idx + 1
        topMaterialAt after idx `shouldBe` unMaterialId basalt
        -- …and BOTH sides' fluid outcomes, in the same delivery.
        lcSideDeco after `shouldBe` fwSideDeco lavaWb
        lcSideDeco waterAfter `shouldBe` fwSideDeco waterWb
        fmap fcType (lcFluidMap waterAfter V.! waterIdx) `shouldBe` Just Ocean

        -- Requirement 6: a result spanning two chunks cannot partially
        -- commit one side, so the handoff names BOTH — the lava chunk
        -- with its new generation and its solidified cell, and the water
        -- chunk with the generation it still has and no stone.
        cmds ← simCommands env
        logger ← readIORef (loggerRef env)
        gens ← readIORef (wsChunkEditGenRef (lpState lp))
        case [ c | c@(SimReactionCommitted p _ _ _) ← cmds
                 , p ≡ crossPageId ] of
            [cmd@(SimReactionCommitted _ _ _ syncs)] → do
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
                -- …and applying that handoff through the REAL command
                -- handler leaves the far side's one unit at one.
                ref ← newIORef emptySimState
                    { ssWorlds = HM.singleton crossPageId
                        (simWorldWith
                            [ (lpLava lp, activeChunk 0 [(idx, Nothing)])
                            , (lpWater lp, activeChunk 0
                                [(waterIdx, Just remainder)]) ]) }
                handleSimCommand env logger ref cmd
                ss ← readIORef ref
                let gridOf cc = scsActiveFluid
                        (swsChunks (ssWorlds ss HM.! crossPageId) HM.! cc)
                gridOf (lpWater lp) V.! waterIdx `shouldBe` Just remainder
                gridOf (lpLava lp) V.! idx `shouldBe` Nothing
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

    it "refuses a twice-reacting coordinate's result when the SECOND \
       \chunk it took fluid from was edited in the meantime" $ \env → do
        lp ← livePage env twiceReactedPageId
        let idx = columnIndex 4 4
            (wgx, wgy) = chunkToGlobal (lpWater lp) 9 9
        -- The first contact was in-chunk and the second across the seam,
        -- so the lava chunk alone reads fresh. Only carrying the second
        -- chunk as a participant makes this result refusable at all.
        sendWorldCommand env (WorldAddTile twiceReactedPageId wgx wgy matLoam)
        barrier env twiceReactedPageId
        before ← chunkAt (lpState lp) (lpLava lp)
        _ ← simCommands env

        let ev = (liveEvent (lpLava lp) (4, 4) (lpLava lp) SolidBasalt)
                     { sevWaterChunks = [lpLava lp, lpWater lp] }
            rr = ReactionResult [(lpLava lp, 0), (lpWater lp, 0)] [ev]
            annihilated = FluidWriteback
                { fwCoord    = lpLava lp
                , fwEditGen  = 0
                , fwFluid    = V.replicate cellsPerChunk Nothing
                , fwTerrain  = lcTerrainSurfaceMap before
                , fwSurf     = lcSurfaceMap before
                , fwSideDeco = VU.replicate cellsPerChunk 0x6D
                }
        deliver env (lpState lp) twiceReactedPageId [annihilated] [rr]

        after ← chunkAt (lpState lp) (lpLava lp)
        lcTerrainSurfaceMap after VU.! idx
            `shouldBe` lcTerrainSurfaceMap before VU.! idx
        addTilesFor (lpState lp) (lpLava lp) ⌦ (`shouldBe` [])
        -- …and the second chunk's own consumed-fluid writeback went with
        -- it, rather than recording an annihilation with no stone.
        lcSideDeco after `shouldBe` lcSideDeco before
        lcFluidMap after `shouldBe` lcFluidMap before

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

    it "commits the reaction's own fluid writeback and its stone in ONE \
       \delivery, and hands the sim back the exact water remainder" $
      \env → do
        lp ← livePage env coherentPageId
        basalt ← materialFor env SolidBasalt
        let lavaIdx  = columnIndex 4 4
            waterIdx = columnIndex 5 4
            ev = liveEvent (lpLava lp) (4, 4) (lpLava lp) SolidBasalt
            rr = ReactionResult [(lpLava lp, 0)] [ev]
            -- What the sim really sends alongside a reaction: the
            -- post-annihilation fluid for the reacting chunk. One unit
            -- of water is left beside the exhausted lava — deliberately
            -- not a multiple of 'volumePerLevel', which is the whole
            -- point of the handoff below.
            remainder = ActiveFluidCell Lake 1 0
            postFluid = V.replicate cellsPerChunk Nothing
                V.// [ (waterIdx, activeToFluidCell 0 remainder) ]
            wb = FluidWriteback
                { fwCoord    = lpLava lp
                , fwEditGen  = 0
                , fwFluid    = postFluid
                , fwTerrain  = lcTerrainSurfaceMap (lpBefore lp)
                , fwSurf     = lcSurfaceMap (lpBefore lp)
                , fwSideDeco = VU.replicate cellsPerChunk 0x3C
                }
        _ ← simCommands env
        deliver env (lpState lp) coherentPageId [wb] [rr]

        after ← chunkAt (lpState lp) (lpLava lp)
        -- Both halves landed: the writeback's own fluid AND the stone on
        -- top of it, in one handler, before any generation moved.
        lcSideDeco after `shouldBe` fwSideDeco wb
        fmap fcType (lcFluidMap after V.! waterIdx) `shouldBe` Just Lake
        lcTerrainSurfaceMap after VU.! lavaIdx
            `shouldBe` lcTerrainSurfaceMap (lpBefore lp) VU.! lavaIdx + 1
        topMaterialAt after lavaIdx `shouldBe` unMaterialId basalt

        -- …and the sim handoff the commit published, applied through the
        -- REAL command handler to a sim chunk holding that exact
        -- remainder, leaves it at 1 rather than rounding it back to 7.
        cmds ← simCommands env
        logger ← readIORef (loggerRef env)
        case [ c | c@(SimReactionCommitted p _ _ _) ← cmds
                 , p ≡ coherentPageId ] of
            [cmd] → do
                ref ← newIORef emptySimState
                    { ssWorlds = HM.singleton coherentPageId
                        (simWorldWith
                            [ (lpLava lp, activeChunk 0
                                [ (waterIdx, Just remainder)
                                , (lavaIdx, Nothing) ]) ]) }
                handleSimCommand env logger ref cmd
                ss ← readIORef ref
                let grid = scsActiveFluid
                        (swsChunks (ssWorlds ss HM.! coherentPageId)
                            HM.! lpLava lp)
                grid V.! waterIdx `shouldBe` Just remainder
                grid V.! lavaIdx  `shouldBe` Nothing
            other → expectationFailure
                ("expected exactly one SimReactionCommitted, got "
                 ⧺ show (length other))

    it "refuses a result naming a participant the page no longer holds, \
       \even though eviction leaves its generation reading as zero" $
      \env → do
        lp ← livePage env evictedPageId
        -- A chunk this page has not loaded. Its generation entry is
        -- absent, which reads as 0 — exactly what an initial-generation
        -- result carries — so the generation check alone would admit it.
        let absent = ChunkCoord 900 900
            rr = ReactionResult [(lpLava lp, 0), (absent, 0)]
                    [liveEvent (lpLava lp) (4, 4) absent SolidBasalt]
            idx = columnIndex 4 4
        gens ← readIORef (wsChunkEditGenRef (lpState lp))
        reactionIsFresh gens rr `shouldBe` True
        td ← readIORef (wsTilesRef (lpState lp))
        registry ← readIORef (wsMaterialRegistryRef (toWorldSimCapability env))
        case admitReaction registry gens td rr of
            ReactionAdmitted _ → expectationFailure
                "an absent participant was admitted"
            ReactionRefused (RefusedFaulty why) → expectationFailure
                ("an eviction is a lost race, not a fault: " ⧺ T.unpack why)
            ReactionRefused (RefusedStale why) →
                T.unpack why `shouldContain` "no longer loaded"

        -- …and the delivery behaves accordingly: no stone, and the
        -- result's own writeback is quarantined with it.
        before ← chunkAt (lpState lp) (lpLava lp)
        let wb = FluidWriteback
                { fwCoord    = lpLava lp
                , fwEditGen  = 0
                , fwFluid    = V.replicate cellsPerChunk Nothing
                , fwTerrain  = lcTerrainSurfaceMap before
                , fwSurf     = lcSurfaceMap before
                , fwSideDeco = VU.replicate cellsPerChunk 0x11
                }
        deliver env (lpState lp) evictedPageId [wb] [rr]
        after ← chunkAt (lpState lp) (lpLava lp)
        lcTerrainSurfaceMap after VU.! idx
            `shouldBe` lcTerrainSurfaceMap before VU.! idx
        lcSideDeco after `shouldBe` lcSideDeco before
        lcFluidMap after `shouldBe` lcFluidMap before
        addTilesFor (lpState lp) (lpLava lp) ⌦ (`shouldBe` [])

    it "keeps an earlier solidification in the SAME chunk when a later \
       \one refreshes it, instead of repainting it to generated terrain" $
      \env → do
        lp ← livePage env cumulativePageId
        mLive ← readIORef (wsZoomLiveRef (lpState lp))
        live ← maybe (expectationFailure "fixture: page has no zoom atlas"
                      ≫ error "unreachable") pure mLive
        cache ← readIORef (wsZoomCacheRef (lpState lp))
        tileIdx ← maybe (expectationFailure "fixture: chunk not in the cache"
                         ≫ error "unreachable") pure
                        (atlasTileIndexFor cache (lpLava lp))
        let idxA = columnIndex 4 4
            idxB = columnIndex 6 6
            commit local p = deliver env (lpState lp) cumulativePageId []
                [ ReactionResult [(lpLava lp, p)]
                    [liveEvent (lpLava lp) local (lpLava lp) SolidBasalt] ]
        commit (4, 4) 0
        commit (6, 6) 1

        after ← chunkAt (lpState lp) (lpLava lp)
        params ← readIORef (wsGenParamsRef (lpState lp)) ⌦ \m → case m of
            Just ps → pure ps
            Nothing → expectationFailure "page has no gen params"
                      ≫ error "unreachable"
        registry ← readIORef (wsMaterialRegistryRef (toWorldSimCapability env))
        -- Both stones are in the live chunk, so both must be among the
        -- overrides the SECOND refresh regenerated from.
        edits ← readIORef (wsEditsRef (lpState lp))
        let cells = [ columnIndex lx ly
                    | WeAddTile agx agy _ ←
                        HM.lookupDefault [] (lpLava lp) edits
                    , let (cc, (lx, ly)) = globalToChunk agx agy
                    , cc ≡ lpLava lp ]
        sort (map ztoIndex (liveTileOverrides after cells))
            `shouldSatisfy` \is → idxA `elem` is ∧ idxB `elem` is

        -- …and the atlas the page now holds agrees with the live chunk
        -- tile for tile, which is the invariant a delivery-scoped
        -- override list breaks: it would have restored the first
        -- stone's pixels to their generated appearance.
        patched ← readIORef (wsZoomLiveRef (lpState lp)) ⌦ maybe
            (expectationFailure "the refresh dropped the atlas"
             ≫ error "unreachable") pure
        case snd (liveChunkZoom params registry (Just (zlaPalette live))
                                (lpLava lp) after
                                (HM.lookupDefault [] (lpLava lp) edits)) of
            Nothing → expectationFailure
                "a palette was supplied but no block came back"
            Just expected →
                case patchAtlasTile (zlaWidth live) (zlaChunksPerRow live)
                                    tileIdx expected (zlaPixels patched) of
                    Left why → expectationFailure (T.unpack why)
                    Right reapplied →
                        reapplied `shouldBe` zlaPixels patched

    it "gives EVERY page that has a zoom map an atlas to refresh, so a \
       \single-tile commit is never invisible on one" $ \env → do
        lp ← livePage env bareZoomPageId
        cache ← readIORef (wsZoomCacheRef (lpState lp))
        live ← readIORef (wsZoomLiveRef (lpState lp))
        -- The invariant the whole refresh rests on. A page holding a
        -- zoom cache but no atlas would render one texture per chunk
        -- ('World.Render.Zoom.Bake.bakeEntries' colours a whole chunk by
        -- its majority material), in which ONE solidified tile cannot
        -- appear at all — so its map would silently stop tracking the
        -- world the first time anything was edited.
        (V.null cache, isJust live) `shouldNotBe` (False, False)
        V.null cache `shouldBe` False
        isJust live `shouldBe` True

        -- …and an ORDINARY single-event commit really does move the
        -- pixels, without the whole chunk having to change material.
        basalt ← materialFor env SolidBasalt
        tileIdx ← maybe (expectationFailure "fixture: chunk not in the cache"
                         ≫ error "unreachable") pure
                        (atlasTileIndexFor cache (lpLava lp))
        before ← maybe (expectationFailure "fixture: page has no atlas"
                        ≫ error "unreachable") pure live
        let idx = columnIndex 4 4
        deliver env (lpState lp) bareZoomPageId []
            [ ReactionResult [(lpLava lp, 0)]
                [liveEvent (lpLava lp) (4, 4) (lpLava lp) SolidBasalt] ]

        after ← chunkAt (lpState lp) (lpLava lp)
        topMaterialAt after idx `shouldBe` unMaterialId basalt
        patched ← maybe (expectationFailure "the refresh dropped the atlas"
                         ≫ error "unreachable") pure
                  =≪ readIORef (wsZoomLiveRef (lpState lp))
        zlaPixels patched `shouldNotBe` zlaPixels before
        changedTiles (zlaWidth before) (zlaChunksPerRow before)
                     (zlaPixels before) (zlaPixels patched)
            `shouldBe` [tileIdx]

        -- …and the baked entries were dropped so the next frame rebakes
        -- from the refreshed summary rather than waiting for the upload.
        (baked, _, _) ← readIORef (wsBakedZoomRef (lpState lp))
        V.null baked `shouldBe` True

    it "keeps an edited cell's LIVE ice and vegetation when it \
       \regenerates that chunk's zoom tile" $ \env → do
        lp ← livePage env icePageId
        -- No world edit clears 'lcIceMap', so the detailed render goes
        -- on showing whatever ice a cell has. The override set is the
        -- whole edit LOG, so a refresh that cleared ice would strip it
        -- from every cell the chunk has ever edited — and the two
        -- presentations would then disagree about the same tile.
        let idx = columnIndex 4 4
            icy = IceCell 7 DrapeIce
        before ← chunkAt (lpState lp) (lpLava lp)
        atomicModifyIORef' (wsTilesRef (lpState lp)) $ \td →
            (insertChunk before { lcIceMap = lcIceMap before V.// [(idx, Just icy)] }
                         td, ())
        deliver env (lpState lp) icePageId []
            [ ReactionResult [(lpLava lp, 0)]
                [liveEvent (lpLava lp) (4, 4) (lpLava lp) SolidBasalt] ]

        after ← chunkAt (lpState lp) (lpLava lp)
        basalt ← materialFor env SolidBasalt
        edits ← readIORef (wsEditsRef (lpState lp))
        let cells = [ columnIndex lx ly
                    | WeAddTile agx agy _ ←
                        HM.lookupDefault [] (lpLava lp) edits
                    , let (cc, (lx, ly)) = globalToChunk agx agy
                    , cc ≡ lpLava lp ]
        case find ((≡ idx) . ztoIndex) (liveTileOverrides after cells) of
            Nothing → expectationFailure
                "the solidified cell is not among the live overrides"
            Just o  → do
                ztoIce o `shouldBe` Just icy
                ztoMaterial o `shouldBe` unMaterialId basalt
                -- …and the vegetation comes from the live column too,
                -- rather than from the pass's own ice-driven snow.
                ztoVeg o `shouldBe` 0

    it "skips a page that has no zoom map at all, and commits the stone \
       \anyway" $ \env → do
        lp ← livePage env noMapPageId
        -- An arena, or a page whose atlas the device refused — which
        -- drops its zoom cache with it, precisely so no map is left that
        -- cannot track the world.
        writeIORef (wsZoomLiveRef (lpState lp)) Nothing
        writeIORef (wsZoomCacheRef (lpState lp)) V.empty
        writeIORef (zoomAtlasDataRef env) []
        basalt ← materialFor env SolidBasalt
        let idx = columnIndex 4 4
        deliver env (lpState lp) noMapPageId []
            [ ReactionResult [(lpLava lp, 0)]
                [liveEvent (lpLava lp) (4, 4) (lpLava lp) SolidBasalt] ]

        -- The stone is durable regardless: a page that cannot draw it is
        -- not a reason to fail a transaction that has already succeeded.
        after ← chunkAt (lpState lp) (lpLava lp)
        topMaterialAt after idx `shouldBe` unMaterialId basalt
        queued ← readIORef (zoomAtlasDataRef env)
        map zauPage queued `shouldBe` []

    it "keeps the stone through a REAL eviction and regeneration of its \
       \chunk, driven by the world thread's own chunk loader" $ \env → do
        lp ← livePage env regenPageId
        basalt ← materialFor env SolidBasalt
        -- The chunk loader only considers VISIBLE pages, so this example
        -- shows its page: the eviction and the reload below are that
        -- loop's own work, including its edit replay.
        sendWorldCommand env (WorldShow regenPageId)
        barrier env regenPageId
        let idx = columnIndex 4 4
            ev  = liveEvent (lpLava lp) (4, 4) (lpLava lp) SolidBasalt
            rr  = ReactionResult [(lpLava lp, 0)] [ev]
            (gx, gy) = reactionEventTile ev
        deliver env (lpState lp) regenPageId [] [rr]
        committed ← chunkAt (lpState lp) (lpLava lp)
        let stoneTop = lcTerrainSurfaceMap committed VU.! idx
        stoneTop `shouldBe` lcTerrainSurfaceMap (lpBefore lp) VU.! idx + 1

        -- Eviction is BUDGET-driven as well as distance-driven, and a
        -- small page never exceeds 'wtdMaxChunks' — so the loader's
        -- eviction branch is unreachable on one until the budget is
        -- lowered. Lowering it is the only thing this example arranges;
        -- which chunks go, when, and what comes back are all the
        -- loader's own decisions.
        atomicModifyIORef' (wsTilesRef (lpState lp)) $ \td →
            (td { wtdMaxChunks = 1 }, ())

        cam0 ← readIORef (rvCameraRef (toRenderViewCapability env))
        let facing = camFacing cam0
            lookAt tx ty = writeIORef (rvCameraRef (toRenderViewCapability env))
                cam0 { camPosition = gridToWorld facing tx ty
                     , camZoom = 0.5 }
            ChunkCoord lcx lcy = lpLava lp
            -- Along the LONGITUDE axis, which wraps, so the camera stays
            -- inside the world diamond and the loader has chunks to
            -- generate there — its eviction rides the same pass.
            (ax, ay) = chunkToGlobal (ChunkCoord (lcx + 3) (lcy - 3))
                                     (chunkSize `div` 2) (chunkSize `div` 2)
        lookAt ax ay
        evicted ← waitForChunk (lpState lp) (lpLava lp) False
        evicted `shouldBe` True

        lookAt gx gy
        reloaded ← waitForChunk (lpState lp) (lpLava lp) True
        reloaded `shouldBe` True
        writeIORef (rvCameraRef (toRenderViewCapability env)) cam0

        -- Requirement 10: the chunk the loader brought back has the
        -- raised height AND the product material, from the recorded
        -- WeAddTile alone — nothing of the in-memory chunk that held
        -- them survived.
        regenerated ← chunkAt (lpState lp) (lpLava lp)
        lcTerrainSurfaceMap regenerated VU.! idx `shouldBe` stoneTop
        topMaterialAt regenerated idx `shouldBe` unMaterialId basalt

    it "queues one pending atlas per page, so two pages committing \
       \between render frames cannot overwrite each other" $ \env → do
        lpA ← livePage env queueAPageId
        lpB ← livePage env queueBPageId
        writeIORef (zoomAtlasDataRef env) []
        let commit lp pid local gen = deliver env (lpState lp) pid []
                [ ReactionResult [(lpLava lp, gen)]
                    [liveEvent (lpLava lp) local (lpLava lp) SolidBasalt] ]
        commit lpA queueAPageId (4, 4) 0
        commit lpB queueBPageId (4, 4) 0
        queued ← readIORef (zoomAtlasDataRef env)
        -- Both images are still pending: nothing the render thread has
        -- not drained yet may be silently replaced by another page's.
        length queued `shouldBe` 2
        map (targetsOf [lpState lpA, lpState lpB]) queued
            `shouldBe` [[0 ∷ Int], [1]]

        -- …and a SECOND commit on one of them replaces that page's own
        -- pending entry rather than appending, so a busy page cannot
        -- queue without bound.
        commit lpA queueAPageId (6, 6) 1
        requeued ← readIORef (zoomAtlasDataRef env)
        length requeued `shouldBe` 2
        map (targetsOf [lpState lpA, lpState lpB]) requeued
            `shouldBe` [[1 ∷ Int], [0]]

    it "supersedes only its OWN page's pending atlas when a world is \
       \initialized, leaving another page's live refresh queued" $
      \env → do
        lpA ← livePage env initOtherPageId
        writeIORef (zoomAtlasDataRef env) []
        deliver env (lpState lpA) initOtherPageId []
            [ ReactionResult [(lpLava lpA, 0)]
                [liveEvent (lpLava lpA) (8, 8) (lpLava lpA) SolidBasalt] ]
        queuedBefore ← readIORef (zoomAtlasDataRef env)
        length queuedBefore `shouldBe` 1

        -- A DIFFERENT page comes up while that refresh is still pending.
        -- Its own init supersedes nothing of A's: A's page was not
        -- rebuilt, and its image is still the one A is showing.
        lpB ← livePage env initPageId
        queuedAfter ← readIORef (zoomAtlasDataRef env)
        map (targetsOf [lpState lpA, lpState lpB]) queuedAfter
            `shouldBe` [[0 ∷ Int], [1]]

    it "refreshes the summary entry of EVERY chunk a delivery edits, not \
       \just the last one" $ \env → do
        lp ← livePage env multiChunkPageId
        basalt ← materialFor env SolidBasalt
        cache0 ← readIORef (wsZoomCacheRef (lpState lp))
        idxA ← maybe (expectationFailure "lava chunk not in the cache"
                      ≫ error "unreachable") pure
                     (atlasTileIndexFor cache0 (lpLava lp))
        idxB ← maybe (expectationFailure "water chunk not in the cache"
                      ≫ error "unreachable") pure
                     (atlasTileIndexFor cache0 (lpWater lp))
        -- Two chunks in one delivery, each made entirely of basalt so
        -- each one's majority material moves. Writing every refreshed
        -- entry from the SAME starting vector would have the second
        -- chunk's write restore the first chunk's original entry.
        let whole cc = [ ReactionResult [(cc, 0)]
                            [liveEvent cc local cc SolidBasalt]
                       | local ← [ (lx, ly) | ly ← [0 .. chunkSize - 1]
                                            , lx ← [0 .. chunkSize - 1] ] ]
        deliver env (lpState lp) multiChunkPageId []
            (whole (lpLava lp) ⧺ whole (lpWater lp))

        cache1 ← readIORef (wsZoomCacheRef (lpState lp))
        zceTexIndex (cache1 V.! idxA) `shouldBe` unMaterialId basalt
        zceTexIndex (cache1 V.! idxB) `shouldBe` unMaterialId basalt
        zceTexIndex (cache0 V.! idxA) `shouldNotBe` unMaterialId basalt
        zceTexIndex (cache0 V.! idxB) `shouldNotBe` unMaterialId basalt

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
        gens ← readIORef (wsChunkEditGenRef (lpState lp))
        td ← readIORef (wsTilesRef (lpState lp))
        -- The refusal is a FAULT, not a lost race, and that distinction
        -- is what decides whether anyone hears about it: world debug
        -- logging is off by default, so a product material the registry
        -- cannot name would otherwise be refused in silence while the
        -- delivery reported success.
        case admitReaction emptyMaterialRegistry gens td rr of
            ReactionRefused (RefusedFaulty why) →
                T.unpack why `shouldContain` "material registry"
            other → expectationFailure
                ("an unresolvable product material is a fault: " ⧺ show other)
        -- …while a participant that has merely moved on is the ordinary
        -- outcome of the race the fence exists for.
        case admitReaction real (HM.insert (lpLava lp) 99 gens) td rr of
            ReactionRefused (RefusedStale _) → pure ()
            other → expectationFailure
                ("a moved-on participant is a lost race: " ⧺ show other)
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

        -- Clear the handoff queue so what is read back afterwards can
        -- only be this commit's own republication.
        writeIORef (zoomAtlasDataRef env) []
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
            [] → expectationFailure
                "the commit published no zoom atlas for upload"
            (_ : _ : _) → expectationFailure
                "one commit queued more than one atlas for this page"
            [upload] → do
                zauWidth upload `shouldBe` zlaWidth patched
                zauHeight upload `shouldBe` zlaHeight patched
                zauPixels upload `shouldBe` zlaPixels patched
                zauPage upload `shouldBe` zoomPageId
                length (zauTargets upload) `shouldBe` 1

        -- The overrides the refresh feeds the generator are derived by
        -- DIFFING the live chunk against what generation would produce,
        -- not from the delivery's own index list — which is what keeps a
        -- later commit in this chunk from repainting this stone back to
        -- its generated appearance.
        after ← chunkAt (lpState lp) (lpLava lp)
        basalt ← materialFor env SolidBasalt
        edits ← readIORef (wsEditsRef (lpState lp))
        let cells = [ columnIndex lx ly
                    | WeAddTile agx agy _ ←
                        HM.lookupDefault [] (lpLava lp) edits
                    , let (cc, (lx, ly)) = globalToChunk agx agy
                    , cc ≡ lpLava lp ]
        case find ((≡ idx) . ztoIndex) (liveTileOverrides after cells) of
            Nothing → expectationFailure
                "the solidified cell is not among the live overrides"
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

-- | Poll until a chunk is (or is no longer) loaded, giving the world
--   thread's own chunk-loading pass time to run.
waitForChunk ∷ WorldState → ChunkCoord → Bool → IO Bool
waitForChunk ws coord want = go (300 ∷ Int)
  where
    go 0 = pure False
    go n = do
        td ← readIORef (wsTilesRef ws)
        if isJust (lookupChunk coord td) ≡ want
            then pure True
            else threadDelay 100000 ≫ go (n - 1)

-- | Which of the known pages a queued atlas entry targets, by index.
--
--   'WorldState' has neither 'Eq' nor 'Show'; a page's own private
--   'IORef' IS its identity, and 'IORef''s 'Eq' is pointer equality, so
--   the comparison goes through that and the RESULT is printable.
targetsOf ∷ [WorldState] → ZoomAtlasUpload → [Int]
targetsOf known upload =
    [ i | t ← zauTargets upload
        , (i, k) ← zip [0 ..] known
        , wsTilesRef k ≡ wsTilesRef t ]

isLeft ∷ Either α β → Bool
isLeft (Left _)  = True
isLeft (Right _) = False
