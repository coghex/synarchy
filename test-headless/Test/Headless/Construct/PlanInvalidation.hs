-- | Continuous validity for committed structure designations (#1844
--   requirements 9, 10 and 18).
--
--   Admission used to be the only check a construction designation ever
--   got: afterwards nothing re-read anything, so a job survived its
--   captured surface changing, the floor under a post being removed, its
--   slot being filled by someone else, and its pack's art disappearing.
--   This suite drives the REAL world-thread hooks against a synthetic
--   page and pins what each one removes — and, just as importantly,
--   what it must NOT.
module Test.Headless.Construct.PlanInvalidation (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.Log (LoggerState)
import Engine.Core.State (EngineEnv(..))
import Item.Ground (GroundItem(..), GroundItems(..), emptyGroundItems)
import Item.Types (ItemInstance(..))
import Structure.Types
    ( StructureSlot(..), StructurePieceData(..), StructureStage
    , emptyChunkStructures, emptyStructureStage, stageStructurePlacement )
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Construct.Attempt (ConstructAttemptId(..))
import World.Construct.Receipt (ConstructPayment(..), mkMaterialReceipt)
import World.Construct.Revalidate
    (ConstructScope(..), revalidateConstructDesignations)
import World.Construct.Types
    ( ConstructDesignation(..), ConstructStatus(..), ConstructTarget(..)
    , StructurePiece(..), newConstructDesignation )
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager)
import World.Tile.Types (WorldTileData(..))

import Test.Headless.Construct.Fixture
    ( artOnlyPackName, fixturePackName, fixtureItems
    , registerArtOnlyPack, registerFixturePacks )

worldSize, zSlice ∷ Int
worldSize = 64
zSlice    = 10

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "construct_plan_invalidation"

tile ∷ (Int, Int)
tile = (5, 5)

floorPiece, postPiece, ghostPiece, artOnlyPiece ∷ ConstructTarget
floorPiece   = CtStructure (StructurePiece fixturePackName "floor" Nothing)
postPiece    = CtStructure (StructurePiece fixturePackName "post" Nothing)
ghostPiece   = CtStructure (StructurePiece "no_such_pack" "floor" Nothing)
artOnlyPiece = CtStructure (StructurePiece artOnlyPackName "floor" Nothing)

attempt ∷ ConstructAttemptId
attempt = ConstructAttemptId 7

spec ∷ Spec
spec = beforeAll initializeEngineHeadless $
  describe "structure plan invalidation" $ do

    it "keeps a designation whose plan still resolves" $
        \(EngineInitResult env) → do
      (ws, logger) ← scene env flatTiles
      seedDesignation ws floorPiece CpUnpaid
      sweep env logger ws `shouldReturn` []
      HM.size <$> readIORef (wsConstructDesignationsRef ws) `shouldReturn` 1

    it "removes one whose target slot has since been filled" $
        \(EngineInitResult env) → do
      (ws, logger) ← scene env flatTiles
      seedDesignation ws floorPiece CpUnpaid
      writeIORef (wsTilesRef ws) (withPiece SFloor tile flatTiles)
      sweep env logger ws `shouldReturn` [tile]
      HM.size <$> readIORef (wsConstructDesignationsRef ws) `shouldReturn` 0

    it "removes a post designation whose supporting floor is gone" $
        \(EngineInitResult env) → do
      (ws, logger) ← scene env (withPiece SFloor tile flatTiles)
      seedDesignation ws postPiece CpUnpaid
      sweep env logger ws `shouldReturn` []
      writeIORef (wsTilesRef ws) flatTiles
      sweep env logger ws `shouldReturn` [tile]

    it "removes one whose pack's art has gone" $
        \(EngineInitResult env) → do
      (ws, logger) ← scene env flatTiles
      seedDesignation ws ghostPiece CpUnpaid
      sweep env logger ws `shouldReturn` [tile]

    it "removes one whose build metadata is incomplete" $
        \(EngineInitResult env) → do
      registerArtOnlyPack env
      (ws, logger) ← scene env flatTiles
      seedDesignation ws artOnlyPiece CpUnpaid
      sweep env logger ws `shouldReturn` [tile]

    it "CANCELS on surface drift rather than retargeting vertically" $
        \(EngineInitResult env) → do
      -- Requirement 4: 'cdZ' is the surface the site was captured at, and
      -- the progress slope and the ghost are both stamped at that level.
      -- Silently adopting the new surface would mis-stamp every slope and
      -- make the drift invisible.
      (ws, logger) ← scene env flatTiles
      seedDesignation ws floorPiece CpUnpaid
      writeIORef (wsTilesRef ws) (surfaceAt (zSlice + 1))
      sweep env logger ws `shouldReturn` [tile]
      HM.lookup tile <$> readIORef (wsConstructDesignationsRef ws)
          `shouldReturn` Nothing

    it "RETAINS one whose terrain is merely unloaded, and resolves it \
       \when the chunk comes back" $ \(EngineInitResult env) → do
      -- Unloaded terrain is not a verdict. Removing here would make a
      -- chunk eviction silently delete the player's jobs.
      (ws, logger) ← scene env flatTiles
      seedDesignation ws floorPiece CpUnpaid
      writeIORef (wsTilesRef ws) emptyTiles
      sweep env logger ws `shouldReturn` []
      HM.size <$> readIORef (wsConstructDesignationsRef ws) `shouldReturn` 1
      -- …and the deferred check really does run once terrain publishes.
      writeIORef (wsTilesRef ws) (withPiece SFloor tile flatTiles)
      sweep env logger ws `shouldReturn` [tile]

    it "leaves a BUILDING designation alone — that is DTV-10's scope" $
        \(EngineInitResult env) → do
      (ws, logger) ← scene env flatTiles
      seedDesignation ws (CtBuilding "no_such_building") CpUnpaid
      sweep env logger ws `shouldReturn` []

    it "skips a designation inside its claimant's placement hand-off" $
        \(EngineInitResult env) → do
      -- Requirement 18: the worker's own accepted read-your-writes
      -- placement is visible the instant it is staged. Without the
      -- hand-off state this sweep would read that success as an external
      -- conflict, cancel the job and refund correctly spent materials.
      (ws, logger) ← scene env flatTiles
      seedDesignationWith ws floorPiece CpUnpaid CsPlacing
      writeIORef (wsStructureStageRef ws) (stagedFloorAt tile)
      sweep env logger ws `shouldReturn` []
      HM.size <$> readIORef (wsConstructDesignationsRef ws) `shouldReturn` 1
      -- The SAME staged placement cancels it once the hand-off is over.
      atomicModifyIORef' (wsConstructDesignationsRef ws) $ \m →
          (HM.adjust (\cd → cd { cdStatus = CsClaimed }) tile m, ())
      sweep env logger ws `shouldReturn` [tile]

    it "scopes a keyed sweep to the tiles whose inputs changed" $
        \(EngineInitResult env) → do
      -- Requirement 9: a live mutation hook must not rescan every job on
      -- the page. Both tiles here are invalid; only the named one goes.
      (ws, logger) ← scene env flatTiles
      writeIORef (wsConstructDesignationsRef ws) $ HM.fromList
          [ (tile, designation ghostPiece CpUnpaid CsPending)
          , ((6, 5), designation ghostPiece CpUnpaid CsPending) ]
      revalidateConstructDesignations env logger ws (ConstructKeys [tile])
          `shouldReturn` [tile]
      HM.keys <$> readIORef (wsConstructDesignationsRef ws)
          `shouldReturn` [(6, 5)]

    it "refunds an invalidated job's receipt to the ground, exactly once" $
        \(EngineInitResult env) → do
      (ws, logger) ← scene env flatTiles
      seedDesignation ws floorPiece
          (CpPaid (mkMaterialReceipt [("steel_plate", 2)]))
      writeIORef (wsTilesRef ws) (withPiece SFloor tile flatTiles)
      sweep env logger ws `shouldReturn` [tile]
      groundNames ws `shouldReturn` ["steel_plate", "steel_plate"]
      -- A second sweep has nothing left to remove, so nothing to refund.
      sweep env logger ws `shouldReturn` []
      groundNames ws `shouldReturn` ["steel_plate", "steel_plate"]

    it "refunds NOTHING for an unpaid job" $ \(EngineInitResult env) → do
      (ws, logger) ← scene env flatTiles
      seedDesignation ws floorPiece CpUnpaid
      writeIORef (wsTilesRef ws) (withPiece SFloor tile flatTiles)
      sweep env logger ws `shouldReturn` [tile]
      groundNames ws `shouldReturn` []

-- * Fixture

sweep ∷ EngineEnv → LoggerState → WorldState → IO [(Int, Int)]
sweep env logger ws =
    revalidateConstructDesignations env logger ws ConstructWholePage

groundNames ∷ WorldState → IO [Text]
groundNames ws =
    sortNames ∘ map (iiDefName ∘ giInst) ∘ HM.elems ∘ gisItems
        <$> readIORef (wsGroundItemsRef ws)
  where sortNames = foldr insertSorted []
        insertSorted x [] = [x]
        insertSorted x (y : ys) | x ≤ y     = x : y : ys
                                | otherwise = y : insertSorted x ys

designation ∷ ConstructTarget → ConstructPayment → ConstructStatus
            → ConstructDesignation
designation tgt payment st =
    (newConstructDesignation zSlice tgt attempt)
        { cdPayment = payment, cdStatus = st }

seedDesignation ∷ WorldState → ConstructTarget → ConstructPayment → IO ()
seedDesignation ws tgt payment =
    seedDesignationWith ws tgt payment CsPending

seedDesignationWith ∷ WorldState → ConstructTarget → ConstructPayment
                    → ConstructStatus → IO ()
seedDesignationWith ws tgt payment st =
    writeIORef (wsConstructDesignationsRef ws)
        (HM.singleton tile (designation tgt payment st))

scene ∷ EngineEnv → WorldTileData → IO (WorldState, LoggerState)
scene env tiles = do
    registerFixturePacks env
    writeIORef (itemManagerRef env) fixtureItems
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws)
        (Just defaultWorldGenParams { wgpWorldSize = worldSize })
    writeIORef (wsTilesRef ws) tiles
    writeIORef (wsGroundItemsRef ws) emptyGroundItems
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    logger ← readIORef (loggerRef env)
    pure (ws, logger)

stagedFloorAt ∷ (Int, Int) → StructureStage
stagedFloorAt (gx, gy) = fst $ stageStructurePlacement
    (gx, gy, fromIntegral (fromEnum SFloor))
    (StructurePieceData 1 2 (zSlice + 1))
    emptyStructureStage

emptyTiles ∷ WorldTileData
emptyTiles = WorldTileData { wtdChunks = HM.empty, wtdMaxChunks = 200 }

flatTiles ∷ WorldTileData
flatTiles = surfaceAt zSlice

surfaceAt ∷ Int → WorldTileData
surfaceAt z =
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
               , lcSurfaceMap = VU.replicate area z
               , lcTerrainSurfaceMap = VU.replicate area z
               , lcFluidMap = V.replicate area Nothing
               , lcIceMap = emptyIceMap, lcFlora = emptyFloraChunkData
               , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
               , lcMagma = Nothing, lcStructures = emptyChunkStructures
               }
    in WorldTileData { wtdChunks = HM.singleton coord lc, wtdMaxChunks = 200 }

withPiece ∷ StructureSlot → (Int, Int) → WorldTileData → WorldTileData
withPiece slot (gx, gy) td = td
    { wtdChunks = HM.adjust addIt (ChunkCoord 0 0) (wtdChunks td) }
  where
    addIt lc = lc { lcStructures = HM.insert key piece (lcStructures lc) }
    key = (gx, gy, fromIntegral (fromEnum slot) ∷ Word8)
    piece = StructurePieceData { spdTexId = 1, spdFaceId = 2
                               , spdGridZ = zSlice + 1 }
