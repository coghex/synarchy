-- | The shared structure-plan resolver and its bounded-drag helper
--   (#1844).
--
--   Two rules used to be written twice and drifted accordingly: the
--   anchor→hover PREVIEW clamped outward from the anchor to 64 cells
--   while the COMMIT clamped from the low-coordinate end at 128, and the
--   wire path tool picked its dominant axis from the CLAMPED delta in
--   one place and the RAW one in the other. This suite pins the single
--   helper both now call ('World.Construct.Extent') and the single
--   resolver every candidate is now judged by ('World.Construct.Plan').
--
--   The resolver examples run against a synthetic in-memory page — the
--   'Test.Headless.World.DesignationSeam' fixture pattern — so the
--   geography, the structure overlay and the catalogue are all exactly
--   what the example says they are.
module Test.Headless.Construct.Plan (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (readIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.Core.Init (EngineInitResult(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Engine.Core.State (EngineEnv(..))
import Structure.Types
    ( StructureSlot(..), StructurePieceData(..), StructureStage
    , emptyChunkStructures, emptyStructureStage, stageStructurePlacement )
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Construct.Attempt (ConstructAttemptId(..))
import World.Construct.Extent
    (maxStructureDragSide, structureDragBounds, structureDragExtent)
import World.Construct.Plan
import World.Construct.Types
    (ConstructTarget(..), StructurePiece(..), newConstructDesignation)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Coordinates (canonicalTile)
import World.Tile.Types (WorldTileData(..))

import Test.Headless.Construct.Fixture
    ( artOnlyPackName, fixturePackName, fixtureWirePack
    , registerArtOnlyPack, registerFixturePacks )

-- * Fixture

-- | A wrapping page 64 chunks across — the same size the seam suite
--   uses, so "across the seam" means the same thing here.
worldSize, zSlice ∷ Int
worldSize = 64
zSlice    = 10

floorPiece, wallPiece, postPiece, wirePiece, ghostPiece ∷ StructurePiece
floorPiece = StructurePiece fixturePackName "floor" Nothing
wallPiece  = StructurePiece fixturePackName "wall" (Just "ne")
postPiece  = StructurePiece fixturePackName "post" Nothing
wirePiece  = StructurePiece fixtureWirePack "wire" Nothing
-- A pack nothing ever registers: the missing-ART case.
ghostPiece = StructurePiece "no_such_pack" "floor" Nothing

spec ∷ Spec
spec = describe "structure plan" $ do
    extentSpec
    beforeAll initializeEngineHeadlessQuiet resolverSpec

-- * The bounded-drag helper

extentSpec ∷ Spec
extentSpec = describe "the shared bounded-drag extent" $ do

    it "always contains the anchor, in every direction and at every \
       \reach" $
        forM_ [ (dx, dy) | dx ← reaches, dy ← reaches ] $ \(dx, dy) →
            forM_ [False, True] $ \lineMode →
                structureDragExtent worldSize lineMode anchor
                    (fst anchor + dx, snd anchor + dy)
                    `shouldSatisfy` elem anchor

    it "spans at most 64 cells along each dragged axis, INCLUDING the \
       \anchor" $
        forM_ [ (dx, dy) | dx ← reaches, dy ← reaches ] $ \(dx, dy) → do
            let ((xLo, yLo), (xHi, yHi)) =
                    structureDragBounds worldSize False anchor
                        (fst anchor + dx, snd anchor + dy)
            (xHi - xLo + 1) `shouldSatisfy` (≤ maxStructureDragSide)
            (yHi - yLo + 1) `shouldSatisfy` (≤ maxStructureDragSide)

    it "reaches exactly anchor ± 63, and no further" $ do
        -- The boundary either side, immediately inside and immediately
        -- outside. D-26's prose could be read as anchor ± 64 (129 cells
        -- total); this is the tighter, shipped arithmetic.
        let sideOf d = let ((xLo, _), (xHi, _)) =
                             structureDragBounds worldSize False anchor
                                 (fst anchor + d, snd anchor)
                       in xHi - xLo + 1
        sideOf 62   `shouldBe` 63
        sideOf 63   `shouldBe` 64
        sideOf 64   `shouldBe` 64
        sideOf 5000 `shouldBe` 64
        sideOf (-62)   `shouldBe` 63
        sideOf (-63)   `shouldBe` 64
        sideOf (-64)   `shouldBe` 64
        sideOf (-5000) `shouldBe` 64

    it "does not lose the anchor on a long NEGATIVE drag" $ do
        -- The exact regression: the commit used to clamp from the LOW
        -- end, so a long drag toward lower coordinates clamped the
        -- anchor itself out of the committed rectangle even though the
        -- preview always kept it.
        let ((xLo, yLo), (xHi, yHi)) =
                structureDragBounds worldSize False anchor
                    (fst anchor - 5000, snd anchor - 5000)
        xLo `shouldBe` fst anchor - maxStructureDragSide + 1
        yLo `shouldBe` snd anchor - maxStructureDragSide + 1
        xHi `shouldBe` fst anchor
        yHi `shouldBe` snd anchor

    it "picks a wire's axis from the RAW localized delta, before \
       \clamping" $ do
        -- x dominates before the clamp and would NOT after it: clamping
        -- shortens the longer axis toward the shorter one, so choosing
        -- afterwards flips a long diagonal onto the wrong axis.
        let (ax, ay) = anchor
            end = (ax + 5000, ay + 70)
            tiles = structureDragExtent worldSize True anchor end
        map snd tiles `shouldSatisfy` all (≡ ay)
        length tiles `shouldBe` maxStructureDragSide
        -- The mirror case, where y dominates.
        let tilesY = structureDragExtent worldSize True anchor
                         (ax + 70, ay + 5000)
        map fst tilesY `shouldSatisfy` all (≡ ax)

    it "breaks an exact wire tie toward x, as both old paths did" $
        map snd (structureDragExtent worldSize True anchor
                     (fst anchor + 9, snd anchor + 9))
            `shouldSatisfy` all (≡ snd anchor)

    it "keeps a seam-crossing drag the size it was drawn" $ do
        -- The endpoint arrives CANONICAL and a whole world away in that
        -- frame; localizing it against the anchor first is what stops
        -- the drag spanning the map and the cap then sweeping 64 tiles
        -- of unrelated terrain.
        let seamAnchor = (worldSize * chunkSize `div` 2 - 2, 0)
            farAlias   = canonicalTile worldSize
                             (fst seamAnchor + 3) (snd seamAnchor)
            tiles = structureDragExtent worldSize False seamAnchor farAlias
        length tiles `shouldBe` 4

    it "is the identity on a non-wrapping page" $
        structureDragExtent 0 False (3, 3) (5, 4)
            `shouldBe` [ (gx, gy) | gx ← [3 .. 5], gy ← [3 .. 4] ]
  where
    anchor = (100, 100)
    reaches = [0, 1, 5, 63, 64, 65, 500, -1, -5, -63, -64, -65, -500]

-- * The resolver

resolverSpec ∷ SpecWith EngineInitResult
resolverSpec = describe "the resolver" $ do

    it "says VALID for a registered, costed piece on loaded level ground" $
        \(EngineInitResult env) → do
            pw ← planWorld env flatTiles HM.empty
            outcomeOf pw floorPiece (5, 5) `shouldBe` PlanValid

    it "captures the tile's current surface as the plan's z" $
        \(EngineInitResult env) → do
            pw ← planWorld env flatTiles HM.empty
            prSurfaceZ (resolveStructurePlan pw PlanForPlacement zSlice
                            floorPiece (5, 5))
                `shouldBe` Just zSlice

    it "says MISSING-ART for an unregistered pack" $
        \(EngineInitResult env) → do
            pw ← planWorld env flatTiles HM.empty
            outcomeOf pw ghostPiece (5, 5) `shouldBe` PlanMissingArt

    it "says MISSING-ART for an unparseable kind" $
        \(EngineInitResult env) → do
            pw ← planWorld env flatTiles HM.empty
            outcomeOf pw (StructurePiece fixturePackName "doorway" Nothing)
                (5, 5) `shouldBe` PlanMissingArt

    it "keeps a KNOWN catalogue failure invalid even with terrain \
       \unloaded" $ \(EngineInitResult env) → do
        -- Requirement 6: 'unresolved-terrain' means a TERRAIN-dependent
        -- fact is unavailable and nothing else. Answering it for a pack
        -- that does not exist would let an unloaded chunk hide a
        -- permanent failure until the player walked over there.
        pw ← planWorld env emptyTiles HM.empty
        outcomeOf pw ghostPiece (5, 5) `shouldBe` PlanMissingArt
        outcomeOf pw floorPiece (5, 5) `shouldBe` PlanUnresolvedTerrain

    it "says VISIBLE-INVALID for art with no complete build metadata" $
        \(EngineInitResult env) → do
            -- Art registered, no cost declared: the ghost can be DRAWN
            -- (in red) because we know what it would look like, but the
            -- job cannot be costed.
            registerArtOnlyPack env
            pw ← planWorld env flatTiles HM.empty
            outcomeOf pw (StructurePiece artOnlyPackName "floor" Nothing)
                (5, 5) `shouldBe` PlanVisibleInvalid

    it "says UNRESOLVED-TERRAIN when the storing chunk is not resident" $
        \(EngineInitResult env) → do
            pw ← planWorld env emptyTiles HM.empty
            outcomeOf pw floorPiece (5, 5) `shouldBe` PlanUnresolvedTerrain

    it "says VISIBLE-INVALID when the surface differs from the plan's z" $
        \(EngineInitResult env) → do
            pw ← planWorld env flatTiles HM.empty
            prOutcome (resolveStructurePlan pw PlanForPlacement (zSlice + 1)
                           floorPiece (5, 5))
                `shouldBe` PlanVisibleInvalid

    it "says VISIBLE-INVALID for a post with no floor under it" $
        \(EngineInitResult env) → do
            pw ← planWorld env flatTiles HM.empty
            outcomeOf pw postPiece (5, 5) `shouldBe` PlanVisibleInvalid

    it "admits that post once a floor is placed there" $
        \(EngineInitResult env) → do
            pw ← planWorld env (withPiece SFloor (5, 5) flatTiles) HM.empty
            outcomeOf pw postPiece (5, 5) `shouldBe` PlanValid

    it "says VISIBLE-INVALID when the target SLOT is already occupied" $
        \(EngineInitResult env) → do
            pw ← planWorld env (withPiece SFloor (5, 5) flatTiles) HM.empty
            outcomeOf pw floorPiece (5, 5) `shouldBe` PlanVisibleInvalid

    it "leaves a COMPATIBLE slot on the same tile alone" $
        \(EngineInitResult env) → do
            -- A floor and a wall coexist once placed, so a floor filling
            -- its own slot must not refuse the wall.
            pw ← planWorld env (withPiece SFloor (5, 5) flatTiles) HM.empty
            outcomeOf pw wallPiece (5, 5) `shouldBe` PlanValid

    it "counts an ACCEPTED read-your-writes staged placement as \
       \occupancy" $ \(EngineInitResult env) → do
        -- A placement is visible through the staging cache before its
        -- queued overlay mutation commits; a resolver reading only the
        -- committed overlay would admit a second designation onto a slot
        -- a worker has already filled this tick.
        pw ← planWorld env flatTiles HM.empty
        outcomeOf pw floorPiece (5, 5) `shouldBe` PlanValid
        let staged = pw { pwStage = stagedFloorAt (5, 5) }
        outcomeOf staged floorPiece (5, 5) `shouldBe` PlanVisibleInvalid

    it "treats EVERY outstanding designation as a conflict for a \
       \placement" $ \(EngineInitResult env) → do
        pw ← planWorld env flatTiles (designated (5, 5) attempt7 wallPiece)
        outcomeOf pw floorPiece (5, 5) `shouldBe` PlanVisibleInvalid

    it "excludes ONLY the exact attempt being revalidated" $
        \(EngineInitResult env) → do
            pw ← planWorld env flatTiles
                     (designated (5, 5) attempt7 floorPiece)
            -- Its own attempt is itself, not a conflict…
            prOutcome (resolveStructurePlan pw (PlanForAttempt attempt7)
                           zSlice floorPiece (5, 5))
                `shouldBe` PlanValid
            -- …and a DIFFERENT attempt at the same canonical tile is
            -- still the other job that is really there.
            prOutcome (resolveStructurePlan pw (PlanForAttempt attempt8)
                           zSlice floorPiece (5, 5))
                `shouldBe` PlanVisibleInvalid

    it "resolves a wire the same way, through its own pack" $
        \(EngineInitResult env) → do
            pw ← planWorld env flatTiles HM.empty
            outcomeOf pw wirePiece (5, 5) `shouldBe` PlanValid
  where
    attempt7 = ConstructAttemptId 7
    attempt8 = ConstructAttemptId 8

    outcomeOf pw piece tile =
        prOutcome (resolveStructurePlan pw PlanForPlacement zSlice piece tile)

    planWorld env tiles designs = do
        registerFixturePacks env
        cat ← readIORef (structureArtCatalogRef env)
        planWorldWith cat tiles designs

    planWorldWith cat tiles designs = pure PlanWorld
        { pwWorldSize    = worldSize
        , pwTiles        = tiles
        , pwStage        = emptyStructureStage
        , pwDesignations = designs
        , pwCatalog      = cat
        , pwProposedWire = HS.empty
        }

    designated tile aid piece = HM.singleton
        (canonicalTile worldSize (fst tile) (snd tile))
        (newConstructDesignation zSlice (CtStructure piece) aid)

-- | A staging cache holding ONE accepted placement — what
--   @structure.place@ leaves behind before its queued overlay mutation
--   commits.
stagedFloorAt ∷ (Int, Int) → StructureStage
stagedFloorAt (gx, gy) = fst $ stageStructurePlacement
    (gx, gy, fromIntegral (fromEnum SFloor))
    (StructurePieceData 1 2 (zSlice + 1))
    emptyStructureStage

-- * Synthetic geography

emptyTiles ∷ WorldTileData
emptyTiles = WorldTileData { wtdChunks = HM.empty, wtdMaxChunks = 200 }

flatTiles ∷ WorldTileData
flatTiles =
    let coord = ChunkCoord 0 0
        area  = chunkSize * chunkSize
        col   = ColumnTiles
                  { ctStartZ = 0
                  , ctMats   = VU.replicate 20 1
                  , ctSlopes = VU.replicate 20 0
                  , ctVeg    = VU.replicate 20 0
                  }
        lc = LoadedChunk
               { lcCoord = coord
               , lcTiles = V.replicate area col
               , lcSurfaceMap = VU.replicate area zSlice
               , lcTerrainSurfaceMap = VU.replicate area zSlice
               , lcFluidMap = V.replicate area Nothing
               , lcIceMap = emptyIceMap, lcFlora = emptyFloraChunkData
               , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
               , lcMagma = Nothing, lcStructures = emptyChunkStructures
               }
    in WorldTileData { wtdChunks = HM.singleton coord lc, wtdMaxChunks = 200 }

-- | The same geography with one piece PLACED in the committed overlay.
withPiece ∷ StructureSlot → (Int, Int) → WorldTileData → WorldTileData
withPiece slot (gx, gy) td = td
    { wtdChunks = HM.adjust addIt (ChunkCoord 0 0) (wtdChunks td) }
  where
    addIt lc = lc { lcStructures = HM.insert key piece (lcStructures lc) }
    key = (gx, gy, fromIntegral (fromEnum slot) ∷ Word8)
    piece = StructurePieceData { spdTexId = 1, spdFaceId = 2
                               , spdGridZ = zSlice + 1 }
