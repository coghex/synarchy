-- | Admission refusal for a tile that already carries a construction
--   job (issue #1595).
--
--   'World.Construct.Types.ConstructDesignations' is keyed by tile
--   COORDINATE alone — the structure slot lives inside @cdTarget@, not
--   in the key — so a second designation on an already-designated tile
--   used to fold in with a plain @HM.insert@ and REPLACE the first
--   while the outcome ring still called the request accepted. That
--   discarded a claimed, part-built, already-PAID job without running
--   the refund + @constructAi.abandonClaim@ pair the cancel path
--   (scripts/build_tool.lua) exists to run, so the materials were gone
--   and the claimant kept ticking progress on its cached copy.
--
--   The contract under test: admission treats ANY existing entry as
--   occupying the tile — whatever its status, progress, payment marker
--   or target category, and in BOTH target branches — and the refusal
--   is reported through the existing requested/applied calculus rather
--   than counted as accepted. #805's PLACED-slot filter is a separate
--   rule and is deliberately not exercised here: every tile in this
--   fixture is empty, so a failure can only be the pending-key rule.
--
--   Drives the REAL world-thread command handler against a synthetic
--   in-memory page (the 'Test.Headless.World.DesignationSeam' fixture
--   pattern, minus the seam) — no worldgen, geography the fixture
--   fully controls.
module Test.Headless.Construct.PendingRefusal (spec) where

import UPrelude
import Test.Hspec
import Data.Foldable (toList)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Data.List (sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.ActionOutcome (ActionOutcome(..))
import Engine.Core.Init (EngineInitResult(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Engine.Core.State (EngineEnv(..))
import Structure.Types (emptyChunkStructures)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Construct.Attempt (firstConstructAttemptId)
import World.Construct.Receipt (ConstructPayment(..), mkMaterialReceipt)
import World.Construct.Types
    ( ConstructDesignation(..), ConstructStatus(..), ConstructTarget(..)
    , StructurePiece(..), newConstructDesignation )
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager)
import World.Thread.Command.Cursor
    ( handleWorldCancelConstructCommand, handleWorldDesignateConstructCommand )
import World.Tile.Types (WorldTileData(..))
import Test.Headless.Construct.Fixture (registerFixturePacks)

-- * Fixture geometry — one loaded chunk at the origin, flat at 'zSlice'.

worldSize, zSlice ∷ Int
worldSize = 64
zSlice    = 10

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "construct_pending_refusal"

-- | Two free tiles well inside the single loaded chunk. 'freeTile' is
--   only ever used by the rectangle example, which needs a SECOND
--   admissible tile beside a blocked one.
tile, freeTile ∷ (Int, Int)
tile     = (5, 5)
freeTile = (6, 5)

floorPiece, wallPiece ∷ ConstructTarget
floorPiece = CtStructure (StructurePiece "dungeon_1" "floor" Nothing)
wallPiece  = CtStructure (StructurePiece "dungeon_1" "wall" (Just "ne"))

portalBuilding, holdBuilding ∷ ConstructTarget
portalBuilding = CtBuilding "acolyte_portal"
holdBuilding   = CtBuilding "cargo_hold_S"

-- | The state the issue is actually about: a job a worker has claimed,
--   poured progress into, and — irreversibly — PAID for. Replacing this
--   is what loses materials with no refund.
paidClaimedJob ∷ ConstructTarget → ConstructDesignation
paidClaimedJob tgt = ConstructDesignation
    { cdZ = zSlice, cdTarget = tgt, cdStatus = CsClaimed
    , cdProgress = 0.4, cdAttempt = firstConstructAttemptId
    , cdPayment = CpPaid (mkMaterialReceipt [("steel_plate", 1)]) }

spec ∷ Spec
spec = beforeAll initializeEngineHeadlessQuiet $ do

  describe "a tile that already carries a job refuses a second one" $ do

    it "keeps the first structure job and admits nothing" $ \(EngineInitResult env) → do
      ws ← resetPage env
      designate env ws tile tile floorPiece
      designate env ws tile tile wallPiece
      m ← readIORef (wsConstructDesignationsRef ws)
      HM.size m `shouldBe` 1
      cdTarget <$> HM.lookup tile m `shouldBe` Just floorPiece

    it "leaves a claimed, part-built, already-paid job byte-identical" $
        \(EngineInitResult env) → do
      -- Every field compared, via the derived Eq: the replacement this
      -- refuses would have reset status, progress AND the durable
      -- payment marker to a fresh designation's values, and the paid
      -- materials would have had no refund path to run through.
      ws ← resetPage env
      let job = paidClaimedJob floorPiece
      writeIORef (wsConstructDesignationsRef ws) (HM.singleton tile job)
      designate env ws tile tile wallPiece
      m ← readIORef (wsConstructDesignationsRef ws)
      HM.lookup tile m `shouldBe` Just job

    it "refuses a re-designation of the very same target too" $
        \(EngineInitResult env) → do
      -- The status/progress reset is the loss even when the target is
      -- identical, so "same piece" is not an exemption.
      ws ← resetPage env
      let job = paidClaimedJob floorPiece
      writeIORef (wsConstructDesignationsRef ws) (HM.singleton tile job)
      designate env ws tile tile floorPiece
      HM.lookup tile <$> readIORef (wsConstructDesignationsRef ws)
          `shouldReturn` Just job

    it "refuses regardless of the existing job's status" $
        \(EngineInitResult env) → do
      forM_ [CsPending, CsClaimed, CsComplete] $ \st → do
        ws ← resetPage env
        let job = (newConstructDesignation zSlice floorPiece
                       firstConstructAttemptId) { cdStatus = st }
        writeIORef (wsConstructDesignationsRef ws) (HM.singleton tile job)
        designate env ws tile tile wallPiece
        HM.lookup tile <$> readIORef (wsConstructDesignationsRef ws)
            `shouldReturn` Just job

    it "still admits a job once the first one is cancelled" $
        \(EngineInitResult env) → do
      -- The refusal must be about the LIVE entry, not the tile: after
      -- the cancel path has run (refund included) the tile is free.
      ws ← resetPage env
      logger ← readIORef (loggerRef env)
      designate env ws tile tile floorPiece
      handleWorldCancelConstructCommand env logger fixturePage
          (fst tile) (snd tile) Nothing
      designate env ws tile tile wallPiece
      targetAt ws tile `shouldReturn` Just wallPiece

  describe "the building branch is covered by the same rule" $ do

    it "refuses a second building at the same anchor" $
        \(EngineInitResult env) → do
      -- CtBuilding takes its own unconditional insert path and is
      -- exempt from #805's placed-slot filter, so it needs its own
      -- example or the branch stays untested.
      ws ← resetPage env
      let job = paidClaimedJob portalBuilding
      writeIORef (wsConstructDesignationsRef ws) (HM.singleton tile job)
      designate env ws tile tile holdBuilding
      HM.lookup tile <$> readIORef (wsConstructDesignationsRef ws)
          `shouldReturn` Just job

    it "refuses a building over a structure job, and the reverse" $
        \(EngineInitResult env) → do
      ws ← resetPage env
      designate env ws tile tile floorPiece
      designate env ws tile tile portalBuilding
      targetAt ws tile `shouldReturn` Just floorPiece

      ws2 ← resetPage env
      designate env ws2 tile tile portalBuilding
      designate env ws2 tile tile floorPiece
      targetAt ws2 tile `shouldReturn` Just portalBuilding

  describe "the refusal is recorded, not counted as accepted" $ do

    it "records rejected 1/0/1 naming the existing designation" $
        \(EngineInitResult env) → do
      ws ← resetPage env
      designate env ws tile tile floorPiece
      _ ← drainOutcomes env
      designate env ws tile tile wallPiece
      os ← designateOutcomes <$> drainOutcomes env
      case os of
        [o] → do
          aoOutcome o `shouldBe` "rejected"
          aoRequested o `shouldBe` Just 1
          aoApplied o `shouldBe` Just 0
          aoDropped o `shouldBe` Just 1
          -- Not the placed-slot reason: nothing is built on this tile,
          -- so blaming an occupied slot would be a false report.
          fromMaybe "" (aoReason o)
              `shouldSatisfy` T.isInfixOf "construction designation"
        _ → expectationFailure ("expected one outcome, got " ⧺ show (length os))

    it "records partial when only part of a rectangle is blocked" $
        \(EngineInitResult env) → do
      ws ← resetPage env
      designate env ws tile tile floorPiece
      _ ← drainOutcomes env
      -- A two-tile drag over the blocked tile and its free neighbour.
      designate env ws tile freeTile wallPiece
      os ← designateOutcomes <$> drainOutcomes env
      case os of
        [o] → do
          aoOutcome o `shouldBe` "partial"
          aoRequested o `shouldBe` Just 2
          aoApplied o `shouldBe` Just 1
          aoDropped o `shouldBe` Just 1
        _ → expectationFailure ("expected one outcome, got " ⧺ show (length os))
      m ← readIORef (wsConstructDesignationsRef ws)
      sort (HM.keys m) `shouldBe` sort [tile, freeTile]
      cdTarget <$> HM.lookup tile m `shouldBe` Just floorPiece
      cdTarget <$> HM.lookup freeTile m `shouldBe` Just wallPiece

    it "still records accepted when nothing is blocked" $
        \(EngineInitResult env) → do
      ws ← resetPage env
      _ ← drainOutcomes env
      designate env ws tile freeTile floorPiece
      os ← designateOutcomes <$> drainOutcomes env
      map aoOutcome os `shouldBe` ["accepted"]
      HM.size <$> readIORef (wsConstructDesignationsRef ws) `shouldReturn` 2

-- * Fixture plumbing

-- | Run the REAL world-thread commit handler for a drag from one tile
--   to another.
designate ∷ EngineEnv → WorldState → (Int, Int) → (Int, Int)
          → ConstructTarget → IO ()
designate env _ws (ax, ay) (bx, by) tgt = do
    logger ← readIORef (loggerRef env)
    -- #1602: unbound — this fixture drives the handler directly, with no
    -- click binding behind it, so it takes the no-binding path every AI
    -- caller does.
    handleWorldDesignateConstructCommand env logger fixturePage ax ay bx by
                                         tgt Nothing

-- | Atomically read and clear the F4 outcome ring, exactly as
--   @debug.drainActionOutcomes@ does.
drainOutcomes ∷ EngineEnv → IO [ActionOutcome]
drainOutcomes env = atomicModifyIORef' (actionOutcomeRef env) $
    \q → (Seq.empty, toList q)

-- | The live target on one tile, if any.
targetAt ∷ WorldState → (Int, Int) → IO (Maybe ConstructTarget)
targetAt ws key =
    fmap cdTarget . HM.lookup key <$> readIORef (wsConstructDesignationsRef ws)

designateOutcomes ∷ [ActionOutcome] → [ActionOutcome]
designateOutcomes = filter ((≡ "construction.designate") . aoKind)

-- | A fresh synthetic page: one flat, fully loaded chunk at the origin
--   with no placed structures and an empty designation map.
resetPage ∷ EngineEnv → IO WorldState
resetPage env = do
    -- #1844: commit resolves every structure candidate against the
    -- registered art/build catalogue, so the pack this fixture
    -- designates from has to exist the way it does at boot.
    registerFixturePacks env
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws)
        (Just defaultWorldGenParams { wgpWorldSize = worldSize })
    writeIORef (wsTilesRef ws) flatTiles
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    pure ws

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
