-- | The exact eighth-z fluid plane (#2520, DFL-2).
--
--   Two halves, registered separately by @test-headless/Spec.hs@:
--
--   * 'spec' — the scale vocabulary's pure laws, the 'Word16'
--     conversion domain, and the active ↔ passive identity through the
--     PRODUCTION conversion paths (activation, the per-tick derivation,
--     the seam re-derivation, and equilibrium deactivation). Registered
--     under @"Sim.Fluid.Exact"@.
--   * 'saveSpec' — what @world-edits@ writes and what every accepted
--     older version migrates to. Registered under @"save migrations"@,
--     beside the rest of the component-version evidence.
module Test.Headless.Sim.ExactFluid
    ( spec
    , saveSpec
    ) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Fluid.Types
    ( FluidCell(..), FluidType(..), fluidCellAtZ, fluidSurfaceCeilZ
    , fluidSurfaceFloorZ, fluidTopLevel, fluidVolumeOverTerrain )
import World.Fluid.Exact
    ( fluidUnitsPerZ, exactSurfaceOfZ, exactSurfaceCeilZ
    , exactSurfaceFloorZ, exactTopLevel, exactVolumeOverTerrain )
import World.Save.Component.PageEdits
    ( WorldEditDTO(..), WorldEditDTOv1(..), WorldEditDTOv2(..)
    , WorldEditDTOv3(..)
    , PageEditsDTO(..), PageEditsDTOv1(..), PageEditsDTOv2(..)
    , PageEditsDTOv3(..), WorldEditsDTO(..), WorldEditsDTOv1(..)
    , WorldEditsDTOv2(..), WorldEditsDTOv3(..), worldEditsCodec )
import World.Save.Component.Types
    (ccDecode, ccEncode, ccVersion, ccInputVers, renderComponentError)
import World.Edit.Types (WorldEdit(..))
import World.Page.Types (WorldPageId(..))
import World.Flora.Identity (firstPlantedFloraCursor)
import World.Save.Snapshot (PageSnapshot(..), captureSessionSnapshot)
import Test.Headless.World.Save.Components.Fixture
    (minimalPage, minimalGlobals, page1)
import Sim.Fluid.Types
    ( ActiveFluidCell(..), clampFluidVolume, exactSurfaceOf
    , surfaceCeilZOf, fluidCellToActive, activeToFluidCell
    , derivePassiveFluid )
import Sim.Fluid.Active (simulateActiveTick)
import Sim.Chunk (activateChunk, loadedChunkState)
import Sim.State.Types (SimWorldState(..), SimChunkState(..), emptySimWorldState)
import Sim.Thread (fastSettleWorld)
import Sim.Topology (SimTopology(..), simTopologyForParams)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)

-- * Fixtures

n ∷ Int
n = chunkSize * chunkSize

homeChunk, eastChunk ∷ ChunkCoord
homeChunk = ChunkCoord 0 0
eastChunk = ChunkCoord 1 0

-- | A passive chunk at a uniform terrain height, carrying one fluid map.
passiveChunk ∷ Int → V.Vector (Maybe FluidCell) → SimChunkState
passiveChunk terrainZ fluid =
    (loadedChunkState fluid (VU.replicate n terrainZ))
        { scsSettleTicks = 0 }

oneCell ∷ Int → Maybe FluidCell → V.Vector (Maybe FluidCell)
oneCell idx mfc = V.replicate n Nothing V.// [(idx, mfc)]

worldOf ∷ [(ChunkCoord, SimChunkState)] → SimWorldState
worldOf = worldOnTopology SimFlatTopology

worldOnTopology ∷ SimTopology → [(ChunkCoord, SimChunkState)] → SimWorldState
worldOnTopology topo chunks = emptySimWorldState
    { swsChunks   = HM.fromList chunks
    , swsActive   = True
    , swsTopology = topo
    }

-- * Pressure fixtures
--
-- Terrain is WALLED everywhere except the cells an example names, so
-- only the named pair can exchange anything and the assertion is about
-- that pair alone.

wallZ ∷ Int
wallZ = 64

walledTerrain ∷ [(Int, Int)] → VU.Vector Int
walledTerrain placed = VU.replicate n wallZ VU.// placed

-- | An ACTIVE chunk from an explicit terrain vector and volume grid.
activeChunk ∷ VU.Vector Int → V.Vector (Maybe ActiveFluidCell) → SimChunkState
activeChunk terrain active = SimChunkState
    { scsFluid       = V.replicate n Nothing
    , scsTerrain     = terrain
    , scsSettleTicks = 0
    , scsActive      = True
    , scsActiveFluid = active
    , scsEquilTicks  = 0
    , scsSideDeco    = VU.replicate n 0
    , scsEditGen     = 0
    }

volumeGrid ∷ [(Int, Maybe ActiveFluidCell)] → V.Vector (Maybe ActiveFluidCell)
volumeGrid placed = V.replicate n Nothing V.// placed

water ∷ Word16 → Maybe ActiveFluidCell
water v = Just (ActiveFluidCell Lake v 0)

volumeAt ∷ ChunkCoord → Int → SimWorldState → Maybe Word16
volumeAt cc idx sws = case HM.lookup cc (swsChunks sws) of
    Nothing  → Nothing
    Just scs → fmap afcVolume (scsActiveFluid scs V.! idx)

-- | A cylindrical page, so the wrapped-seam pair below really wraps.
cylTopo ∷ Int → SimTopology
cylTopo worldSize =
    simTopologyForParams defaultWorldGenParams { wgpWorldSize = worldSize }

-- | The #2044 wrap fixture's own stored keys: @seamXA@ sits at the
--   maximum u on a worldSize-64 page, so its raw +X neighbour is past
--   the seam and is STORED as @seamXB@.
seamWorldSize ∷ Int
seamWorldSize = 64

seamXA, seamXB ∷ ChunkCoord
seamXA = ChunkCoord 16 (-15)
seamXB = ChunkCoord (-15) 17

-- | The east-edge pair of a chunk and its +X neighbour, at row 8.
eastEdgeIdx, westEdgeIdx ∷ Int
eastEdgeIdx = 8 * chunkSize + (chunkSize - 1)
westEdgeIdx = 8 * chunkSize

-- | THE discriminating pair (#2520). Source terrain 1 holding 8 units,
--   destination terrain 0 holding 9: exact surfaces 16 and 9, integer
--   ceilings 2 and 2.
--
--   Three wrong implementations each give a different answer, so this
--   one number separates all of them from the right one:
--
--     * comparing the integer CEILINGS sees no difference and moves 0;
--     * reading the SOURCE's volume for the neighbour's surface sees
--       @16 - 8 = 8@ and moves 2;
--     * re-multiplying the exact difference by the scale sees @7 * 8@
--       and moves the source's whole 8 units;
--     * comparing exact surfaces sees @16 - 9 = 7@ and moves
--       @7 `div` 4@ = 1.
pressureSrcTerrain, pressureDstTerrain :: Int
pressureSrcTerrain = 1
pressureDstTerrain = 0

pressureSrcVolume, pressureDstVolume, pressureMoved ∷ Word16
pressureSrcVolume = 8
pressureDstVolume = 9
pressureMoved     = 1

-- | EVERY non-zero volume a 'Word16' can hold, 1 through 65535 —
--   not a sample. The identity is promised over the whole
--   representable domain, and a sampled list cannot rule out an
--   intermediate-volume regression between two sampled points.
roundTripVolumes ∷ [Word16]
roundTripVolumes = [1 .. maxBound]

-- | A cheap sample of the same domain, for the cases that also cross a
--   list of terrain tops and would otherwise multiply out.
sampleVolumes ∷ [Word16]
sampleVolumes =
    [1 .. fromIntegral fluidUnitsPerZ]
    ⧺ [ fromIntegral (k * fluidUnitsPerZ) + r
      | k ← [1, 2, 7, 1000 ∷ Int], r ← [0, 1, 7] ]
    ⧺ [maxBound - 1, maxBound]

-- | Terrain tops the identity has to hold over, negative ones included.
roundTripTerrains ∷ [Int]
roundTripTerrains = [-2048, -17, -1, 0, 1, 63, 8191]

-- * The pure scale vocabulary and the conversion paths

spec ∷ Spec
spec = do
    describe "the scale constant" $
        it "is eight units per whole z" $
            fluidUnitsPerZ `shouldBe` 8

    describe "whole z to the exact plane" $ do
        it "scales every z, negative ones included" $
            map exactSurfaceOfZ [-3, -1, 0, 1, 12]
                `shouldBe` [-24, -8, 0, 8, 96]

        it "is a full top level, never an empty one" $
            map (exactTopLevel . exactSurfaceOfZ) [-3, 0, 1, 12]
                `shouldBe` replicate 4 fluidUnitsPerZ

    describe "the integer ceiling and floor views" $ do
        it "agree on an exact multiple" $
            ( [ (exactSurfaceCeilZ (exactSurfaceOfZ z)
                , exactSurfaceFloorZ (exactSurfaceOfZ z))
              | z ← [-3, -1, 0, 1, 12] ]
                `shouldBe` [ (z, z) | z ← [-3, -1, 0, 1, 12] ] )

        it "bracket every remainder inside one level" $
            ( [ (r, exactSurfaceFloorZ (40 + r), exactSurfaceCeilZ (40 + r))
              | r ← [0 .. fluidUnitsPerZ - 1] ]
                `shouldBe`
              ( (0, 5, 5)
              : [ (r, 5, 6) | r ← [1 .. fluidUnitsPerZ - 1] ] ) )

        it "keep bracketing below zero, where truncation would not" $
            ( [ (e, exactSurfaceFloorZ e, exactSurfaceCeilZ e)
              | e ← [-9, -8, -7, -1] ]
                `shouldBe` [ (-9, -2, -1), (-8, -1, -1)
                           , (-7, -1, 0), (-1, -1, 0) ] )

    describe "the top fill level" $ do
        it "runs 1 .. 8 across one level and never reads 0" $
            map exactTopLevel [33 .. 40] `shouldBe` [1 .. 8]

        it "reads 8 at every exact multiple, including zero and below" $
            map exactTopLevel [-16, -8, 0, 8, 16]
                `shouldBe` replicate 5 fluidUnitsPerZ

    describe "absolute surface to same-footprint volume" $ do
        it "counts the units standing over the terrain" $
            ( [ exactVolumeOverTerrain t (exactSurfaceOfZ t + v)
              | t ← roundTripTerrains, v ← [0, 1, 7, 8, 9] ]
                `shouldBe`
              concat [ [0, 1, 7, 8, 9] | _ ← roundTripTerrains ] )

        it "is zero for a surface at or below the terrain, never negative" $
            ( [ exactVolumeOverTerrain t (exactSurfaceOfZ t - d)
              | t ← roundTripTerrains, d ← [0, 1, 8, 4096] ]
                `shouldBe` concat [ [0, 0, 0, 0] | _ ← roundTripTerrains ] )

    describe "the Word16 conversion domain" $ do
        it "narrows every representable count exactly" $
            ( [ n' | v ← [0, 1, 7, 8, 65534, 65535]
                   , let n' = clampFluidVolume v
                   , fromIntegral n' ≢ v ]
                `shouldBe` [] )

        it "saturates rather than wrapping above the bound" $
            map clampFluidVolume [65536, 65543, 10 ^ (9 ∷ Int)]
                `shouldBe` replicate 3 (maxBound ∷ Word16)

        it "floors a negative count at zero" $
            map clampFluidVolume [-1, -65536] `shouldBe` [0, 0]

    describe "active to passive and back is the identity" $ do
        it "preserves EVERY representable volume, 1 through 65535" $
            -- The whole non-zero 'Word16' domain at one terrain top, so
            -- no intermediate volume can regress between two samples.
            ( [ v
              | v ← roundTripVolumes
              , let afc = ActiveFluidCell Lake v 0
              , (fluidCellToActive 0 =≪ activeToFluidCell 0 afc)
                  ≢ Just afc ]
                `shouldBe` [] )

        it "preserves it over every terrain top, negatives included" $
            ( [ (t, v)
              | t ← roundTripTerrains, v ← sampleVolumes
              , let afc = ActiveFluidCell Lake v 0
              , (fluidCellToActive t =≪ activeToFluidCell t afc)
                  ≢ Just afc { afcFlowDir = 0 } ]
                `shouldBe` [] )

        it "carries the type across, Ocean included" $
            ( [ ft
              | ft ← [Ocean, Lake, River, Lava]
              , fmap afcType (fluidCellToActive 0
                    =≪ activeToFluidCell 0 (ActiveFluidCell ft 1 0))
                  ≢ Just ft ]
                `shouldBe` [] )

        it "writes a partial cell's own remainder, not a whole level" $ do
            activeToFluidCell 5 (ActiveFluidCell Lake 1 0)
                `shouldBe` Just (FluidCell Lake (exactSurfaceOfZ 5 + 1))
            activeToFluidCell 5 (ActiveFluidCell Ocean 7 0)
                `shouldBe` Just (FluidCell Ocean (exactSurfaceOfZ 5 + 7))

        it "still reads as one occupied z through the ceiling view" $
            ( [ fluidSurfaceCeilZ fc
              | v ← [1 .. fromIntegral fluidUnitsPerZ ∷ Word16]
              , Just fc ← [activeToFluidCell 5 (ActiveFluidCell Lake v 0)] ]
                `shouldBe` replicate fluidUnitsPerZ 6 )

        it "reads its floor and its fill level too" $
            ( fmap (\fc → ( fluidSurfaceFloorZ fc, fluidTopLevel fc
                          , fluidVolumeOverTerrain 5 fc ))
                   (activeToFluidCell 5 (ActiveFluidCell Lake 1 0))
                `shouldBe` Just (5, 1, 1) )

        it "a whole-z cell is a full level" $ do
            fluidTopLevel (fluidCellAtZ Lake 6) `shouldBe` fluidUnitsPerZ
            fluidVolumeOverTerrain 5 (fluidCellAtZ Lake 6)
                `shouldBe` fluidUnitsPerZ

    describe "activation reconstructs exactly what the passive cell holds" $ do
        it "takes a one-unit and a seven-unit cell at their own size" $
            ( [ fmap afcVolume (fluidCellToActive 5
                    (FluidCell Lake (exactSurfaceOfZ 5 + v)))
              | v ← [1, 7] ]
                `shouldBe` [Just 1, Just 7] )

        it "runs through the real activateChunk" $ do
            let idx = 5 * chunkSize + 5
                passive = oneCell idx
                    (Just (FluidCell Ocean (exactSurfaceOfZ 2 + 3)))
                scs = activateChunk (passiveChunk 2 passive)
            fmap afcVolume (scsActiveFluid scs V.! idx) `shouldBe` Just 3
            fmap afcType (scsActiveFluid scs V.! idx) `shouldBe` Just Ocean

    describe "the tick's own derivation keeps every remaining unit" $ do
        -- One isolated cell on flat terrain: nothing can flow out of it,
        -- so what the tick writes back is purely the conversion.
        let settledAt v =
                let idx = 5 * chunkSize + 5
                    passive = oneCell idx
                        (Just (FluidCell Lake (exactSurfaceOfZ 3 + v)))
                    st = worldOf [ (homeChunk
                                   , activateChunk (passiveChunk 3 passive)) ]
                in ( idx
                   , scsFluid (swsChunks (simulateActiveTick st) HM.! homeChunk) )

        it "derives the exact plane for a partial cell" $
            ( [ fluid V.! idx | v ← [1, 7], let (idx, fluid) = settledAt v ]
                `shouldBe`
              [ Just (FluidCell Lake (exactSurfaceOfZ 3 + v)) | v ← [1, 7] ] )

        it "survives the real equilibrium deactivation unrounded" $ do
            let idx = 5 * chunkSize + 5
                passive = oneCell idx
                    (Just (FluidCell Ocean (exactSurfaceOfZ 3 + 1)))
                st = worldOf [ (homeChunk
                               , activateChunk (passiveChunk 3 passive)) ]
                -- The dump path's own synchronous settle: it runs until
                -- every chunk has quiesced AND deactivated, so this is
                -- 'deactivateInPlace' through the production loop.
                settled = fastSettleWorld 500 st
                scs = swsChunks settled HM.! homeChunk
            scsActive scs `shouldBe` False
            scsFluid scs V.! idx
                `shouldBe` Just (FluidCell Ocean (exactSurfaceOfZ 3 + 1))

        it "survives the seam pass's re-derivation unrounded" $ do
            -- Two adjacent ACTIVE chunks, so 'reconcileSeams' runs and
            -- re-derives both sides. The cell under test is in the
            -- middle of its chunk, far from the shared edge.
            let idx = 5 * chunkSize + 5
                passive = oneCell idx
                    (Just (FluidCell Lake (exactSurfaceOfZ 3 + 5)))
                st = worldOf
                    [ (homeChunk, activateChunk (passiveChunk 3 passive))
                    , (eastChunk, activateChunk (passiveChunk 3 passive)) ]
                after = simulateActiveTick st
            ( [ scsFluid (swsChunks after HM.! cc) V.! idx
              | cc ← [homeChunk, eastChunk] ]
                `shouldBe`
              replicate 2 (Just (FluidCell Lake (exactSurfaceOfZ 3 + 5))) )

    describe "a passive cell at or below its terrain (requirement 5)" $ do
        let idx = 5 * chunkSize + 5
            sunk d = FluidCell River (exactSurfaceOfZ 3 - d)
            world d = worldOf [ (homeChunk
                               , activateChunk
                                     (passiveChunk 3 (oneCell idx (Just (sunk d))))) ]

        it "is not inflated into a shallow level by activation" $
            ( [ scsActiveFluid (swsChunks (world d) HM.! homeChunk) V.! idx
              | d ← [0, 1, 8, 4096] ]
                `shouldBe` replicate 4 Nothing )

        it "is not erased by the tick's writeback either" $
            ( [ scsFluid (swsChunks (simulateActiveTick (world d))
                              HM.! homeChunk) V.! idx
              | d ← [0, 1, 8, 4096] ]
                `shouldBe` [ Just (sunk d) | d ← [0, 1, 8, 4096] ] )

        it "keeps its identity when ANOTHER cell makes the chunk dirty" $ do
            -- A live cell beside it spills, so the chunk is dirty and
            -- re-derived for real, rather than being left alone.
            -- Far enough away that its own spill cannot reach the cell
            -- under test: a sub-terrain cell holds no volume, so it IS
            -- an ordinary empty destination for a neighbour, and this
            -- example is about the DERIVATION, not about that.
            let live = 9 * chunkSize + 9
                deep = FluidCell Lake (exactSurfaceOfZ 4 + 4)
                fluid = V.replicate n Nothing
                    V.// [ (idx, Just (sunk 1)), (live, Just deep) ]
                st = worldOf [ (homeChunk
                               , activateChunk (passiveChunk 3 fluid)) ]
                after = simulateActiveTick st
                derived = scsFluid (swsChunks after HM.! homeChunk)
            derived V.! idx `shouldBe` Just (sunk 1)
            -- and the live cell really did move, so this was a real
            -- derivation and not an untouched map.
            (derived V.! live ≢ Just deep) `shouldBe` True

        it "survives the real equilibrium deactivation" $ do
            let scs = swsChunks (fastSettleWorld 500 (world 1)) HM.! homeChunk
            scsActive scs `shouldBe` False
            scsFluid scs V.! idx `shouldBe` Just (sunk 1)

        it "cannot resurrect an ordinary cell that drained away" $ do
            -- The prior passive map says Lake at a POSITIVE depth; the
            -- active grid is empty because the cell drained. Preservation
            -- must not hand that cell back.
            let prior = oneCell idx (Just (FluidCell Lake (exactSurfaceOfZ 9)))
            derivePassiveFluid (VU.replicate n 3) prior
                              (V.replicate n Nothing) V.! idx
                `shouldBe` Nothing

        it "cannot resurrect a cell annihilation emptied either" $ do
            let prior = oneCell idx (Just (FluidCell Lava (exactSurfaceOfZ 3 + 2)))
                emptied = V.replicate n Nothing
            derivePassiveFluid (VU.replicate n 3) prior emptied V.! idx
                `shouldBe` Nothing

    describe "the whole-z compatibility view of an active cell" $
        it "reads a dry cell at its terrain and any partial level as one z" $
            ( [ surfaceCeilZOf 4 v | v ← [0, 1, 7, 8, 9] ]
                `shouldBe` [4, 5, 5, 5, 6] )

    describe "exact surfaces order finer than their integer ceilings" $
        it "separates two cells whose ceiling views coincide" $ do
            -- Both read as z 5 through the ceiling, and the pressure
            -- expressions in 'Sim.Fluid.Active' still see the difference.
            surfaceCeilZOf 4 1 `shouldBe` surfaceCeilZOf 4 7
            (exactSurfaceOf 4 1 < exactSurfaceOf 4 7) `shouldBe` True

    -- The pressure examples below drive the REAL 'simulateActiveTick',
    -- not a helper: the whole point is that the production gravity and
    -- seam paths read the exact plane, which no pure comparison can
    -- establish.
    describe "unequal-terrain gravity reads the exact surface" $ do
        let srcIdx = 8 * chunkSize + 8
            dstIdx = 8 * chunkSize + 9
            terrain = walledTerrain [ (srcIdx, pressureSrcTerrain)
                                    , (dstIdx, pressureDstTerrain) ]
            worldWith srcV dstV = worldOf
                [ (homeChunk, activeChunk terrain
                      (volumeGrid [ (srcIdx, water srcV)
                                  , (dstIdx, water dstV) ])) ]

        it "pins the fixture: the two ceilings really do coincide" $ do
            surfaceCeilZOf pressureSrcTerrain pressureSrcVolume
                `shouldBe` surfaceCeilZOf pressureDstTerrain pressureDstVolume
            (exactSurfaceOf pressureSrcTerrain pressureSrcVolume
                > exactSurfaceOf pressureDstTerrain pressureDstVolume)
                `shouldBe` True

        it "moves what the EXACT difference asks for, and only that" $ do
            let after = simulateActiveTick
                            (worldWith pressureSrcVolume pressureDstVolume)
            volumeAt homeChunk srcIdx after
                `shouldBe` Just (pressureSrcVolume - pressureMoved)
            volumeAt homeChunk dstIdx after
                `shouldBe` Just (pressureDstVolume + pressureMoved)

        it "stops once the lower cell's exact surface has caught up" $ do
            -- Destination one unit ABOVE the source on the exact plane,
            -- and on the SAME ceiling (both read as z 2) -- pinned here
            -- so this cannot quietly stop being the interesting case.
            -- Reading the source's own volume for the neighbour's
            -- surface would see a whole z of head and drain it.
            surfaceCeilZOf pressureSrcTerrain 1
                `shouldBe` surfaceCeilZOf pressureDstTerrain 10
            (exactSurfaceOf pressureSrcTerrain 1
                < exactSurfaceOf pressureDstTerrain 10) `shouldBe` True
            let after = simulateActiveTick (worldWith 1 10)
            volumeAt homeChunk srcIdx after `shouldBe` Just 1
            volumeAt homeChunk dstIdx after `shouldBe` Just 10

    describe "unequal-terrain seam flow reads the exact surface" $ do
        let terrainAt z = walledTerrain [ (eastEdgeIdx, z), (westEdgeIdx, z) ]
            pairWorld topo a b srcV dstV = worldOnTopology topo
                [ (a, activeChunk (terrainAt pressureSrcTerrain)
                          (volumeGrid [(eastEdgeIdx, water srcV)]))
                , (b, activeChunk (terrainAt pressureDstTerrain)
                          (volumeGrid [(westEdgeIdx, water dstV)])) ]
            ordinary = pairWorld SimFlatTopology
                           (ChunkCoord 0 0) (ChunkCoord 1 0)
            wrapped  = pairWorld (cylTopo seamWorldSize) seamXA seamXB

        it "pins the wrap fixture: the +X neighbour really is stored \
           \across the seam" $
            (seamXA ≢ seamXB) `shouldBe` True

        it "moves the exact difference across an ordinary seam" $ do
            let after = simulateActiveTick
                    (ordinary pressureSrcVolume pressureDstVolume)
            volumeAt (ChunkCoord 0 0) eastEdgeIdx after
                `shouldBe` Just (pressureSrcVolume - pressureMoved)
            volumeAt (ChunkCoord 1 0) westEdgeIdx after
                `shouldBe` Just (pressureDstVolume + pressureMoved)

        it "moves the same amount across the WRAPPED seam" $ do
            let after = simulateActiveTick
                    (wrapped pressureSrcVolume pressureDstVolume)
            volumeAt seamXA eastEdgeIdx after
                `shouldBe` Just (pressureSrcVolume - pressureMoved)
            volumeAt seamXB westEdgeIdx after
                `shouldBe` Just (pressureDstVolume + pressureMoved)

        it "stops at the seam once the lower side has caught up" $ do
            -- Same same-ceiling, destination-higher pair as in-chunk.
            surfaceCeilZOf pressureSrcTerrain 1
                `shouldBe` surfaceCeilZOf pressureDstTerrain 10
            let after = simulateActiveTick (ordinary 1 10)
            volumeAt (ChunkCoord 0 0) eastEdgeIdx after `shouldBe` Just 1
            volumeAt (ChunkCoord 1 0) westEdgeIdx after `shouldBe` Just 10

-- * The world-edits wire contract

saveSpec ∷ Spec
saveSpec = describe "world-edits v4: exact fluid surfaces (#2520)" $ do
    let pageId = WorldPageId "exact"
        coord  = ChunkCoord 0 0

        decodeAt ver bytes = case ccDecode worldEditsCodec ver bytes of
            Left e  → Left (T.unpack (renderComponentError e))
            Right v → Right v

        snapshotsOf (WorldEditsDTO slices) =
            [ (ft, z)
            | s ← slices, edits ← HM.elems (pedEdits s)
            , WeSetFluidSnapshotD _ _ ft z ← edits ]

        v1Payload = S.encode (WorldEditsDTOv1
            [ PageEditsDTOv1 pageId (HM.singleton coord
                [ WeSetFluidSnapshotDv1 3 4 Lake 6 ]) ])
        v2Payload = S.encode (WorldEditsDTOv2
            [ PageEditsDTOv2 pageId (HM.singleton coord
                [ WeSetFluidSnapshotDv2 3 4 River 6 ])
                firstPlantedFloraCursor ])
        v3Payload = S.encode (WorldEditsDTOv3
            [ PageEditsDTOv3 pageId (HM.singleton coord
                [ WeSetFluidSnapshotDv3 3 4 Ocean 6 ])
                firstPlantedFloraCursor ])

    it "is at version 4 and still accepts every older one" $ do
        ccVersion worldEditsCodec `shouldBe` 4
        ccInputVers worldEditsCodec `shouldBe` [1, 2, 3, 4]

    it "migrates a v1 snapshot's whole z to exactly z * 8" $
        fmap snapshotsOf (decodeAt 1 v1Payload)
            `shouldBe` Right [(Lake, exactSurfaceOfZ 6)]

    it "migrates a v2 snapshot the same way, exactly once" $
        fmap snapshotsOf (decodeAt 2 v2Payload)
            `shouldBe` Right [(River, exactSurfaceOfZ 6)]

    it "migrates a v3 snapshot the same way, exactly once" $
        fmap snapshotsOf (decodeAt 3 v3Payload)
            `shouldBe` Right [(Ocean, exactSurfaceOfZ 6)]

    it "invents no fractional remainder: every migrated cell is full" $
        ( [ fluidTopLevel (FluidCell ft z)
          | payload ← [(1 ∷ Word32, v1Payload), (2, v2Payload), (3, v3Payload)]
          , Right dto ← [uncurry decodeAt payload]
          , (ft, z) ← snapshotsOf dto ]
            `shouldBe` replicate 3 fluidUnitsPerZ )

    it "keeps an exact v4 payload's remainder, scaling nothing again" $ do
        -- One unit, seven units, and a PARTIAL Ocean cell: the three
        -- shapes a rounding writeback or a second rescale would destroy.
        let exact =
                [ WeSetFluidSnapshotD 1 1 Lake  (exactSurfaceOfZ 6 + 1)
                , WeSetFluidSnapshotD 2 2 River (exactSurfaceOfZ 6 + 7)
                , WeSetFluidSnapshotD 3 3 Ocean (exactSurfaceOfZ 0 + 3)
                , WeSetFluidSnapshotD 4 4 Lake  (exactSurfaceOfZ (-2) + 5) ]
            payload = S.encode (WorldEditsDTO
                [ PageEditsDTO pageId (HM.singleton coord exact)
                               firstPlantedFloraCursor ])
        fmap snapshotsOf (decodeAt 4 payload) `shouldBe` Right
            [ (Lake, exactSurfaceOfZ 6 + 1)
            , (River, exactSurfaceOfZ 6 + 7)
            , (Ocean, 3)
            , (Lake, exactSurfaceOfZ (-2) + 5) ]

    it "round-trips those remainders through a real encode and decode" $ do
        -- The LIVE edits the sim's save writeback emits, through the
        -- component's own encode and back out of its own decode.
        let live = [ WeSetFluidSnapshot 1 1 Lake  (exactSurfaceOfZ 6 + 1)
                   , WeSetFluidSnapshot 2 2 Ocean (exactSurfaceOfZ 6 + 7) ]
            -- 'minimalGlobals' names page1 as the active/visible page,
            -- so the capture only validates for that one.
            snap = case captureSessionSnapshot minimalGlobals
                            [ (minimalPage page1)
                                { pgsEdits = HM.singleton coord live } ] of
                Right ok  → ok
                Left errs → error ("fixture snapshot invalid: " <> show errs)
            reread = decodeAt (ccVersion worldEditsCodec)
                              (ccEncode worldEditsCodec snap)
        fmap snapshotsOf reread `shouldBe` Right
            [ (Lake, exactSurfaceOfZ 6 + 1)
            , (Ocean, exactSurfaceOfZ 6 + 7) ]
