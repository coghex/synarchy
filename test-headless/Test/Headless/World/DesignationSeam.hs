{-# LANGUAGE Strict #-}
-- | The designation half of the U-seam coordinate-frame contract
--   (issue #1175). 'Test.Headless.World.Render.PickSeam' covers the
--   other half — what a pick reports; this covers what a designation
--   does with it.
--
--   Two things have to hold together, and canonicalising only one end
--   was measured to be WORSE than the old uniform-but-seam-blind
--   behaviour (see the issue):
--
--     * A two-click drag that straddles the seam stays the small
--       rectangle the player drew. Both picks come back canonical, and
--       two physically adjacent tiles across the seam sit a whole world
--       apart in that frame — a raw @min@/@max@ makes a world-sized
--       rectangle whose 128-tile cap then sweeps unrelated terrain.
--     * Every paired point operation — create, read, cancel, nearest,
--       count — resolves the SAME stored key, from any u-alias of it.
--       Otherwise a designation created at one alias is uncancellable
--       from the other.
--
--   The engine-backed examples drive the REAL world-thread command
--   handlers and the REAL Lua verbs (through the debug console's own
--   loadstring+pcall, like 'Test.Headless.World.SelectTileZ') against a
--   synthetic in-memory page — no worldgen, and geography the fixture
--   fully controls.
module Test.Headless.World.DesignationSeam (spec, engineSpec) where

import UPrelude
import Test.Hspec
import Data.IORef (IORef, atomicModifyIORef', readIORef, writeIORef, newIORef)
import Data.List (sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..), defaultCamera)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), noFaceMapVertexId)
import Engine.Scene.Types (SortableQuad(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Structure.Types
    (StructurePieceData(..), StructureSlot(..), emptyChunkStructures)
import World.Chop.Types (newChopDesignation)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize, columnIndex)
import Test.Headless.Construct.Fixture (registerFixturePacks)
import World.Construct.Attempt
    (ConstructAttemptId(..), firstConstructAttemptId)
import World.Construct.Receipt (ConstructPayment(..), mkMaterialReceipt)
import World.Construct.Types
    ( ConstructDesignation(..), ConstructStatus(..), ConstructTarget(..)
    , StructurePiece(..), constructDesignationPaid )
import World.Flora.CropPlot (newCropPlot)
import World.Flora.Types
    ( FloraCatalog(..), FloraChunkData(..), FloraHarvest(..), FloraId(..)
    , FloraInstance(..), FloraSpecies(..), FloraWorldGen(..)
    , emptyFloraCatalog, emptyFloraChunkData, newFloraSpecies )
import Item.Types (ItemDef(..), ItemFood(..), ItemManager(..))
import World.Cursor.Types (CursorState(..), emptyCursorState)
import World.Fluid.Types (emptyIceMap)
import World.Grid (gridToWorld, tileHeight)
import World.Generate.Coordinates
    ( canonicalTile, canonicalTileFrame, chunkInSeamRegion, globalToChunk
    , localizeTileToAnchor, seamTileDist2, tileAliasStep )
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Save.Component.Page
    ( GroundItemsDTO(..), MineDesignationDTO(..), PageActivityDTO(..)
    , TillDesignationDTO(..), WorldActivityDTO(..), applyWorldActivity
    , blankPageSnapshot )
import World.Render.CursorQuads (renderWorldCursorQuads)
import World.Render.Textures.Types
    (WorldTextures(..), defaultWorldTextures)
import World.Save.Snapshot (PageSnapshot(..))
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager)
import World.Thread.Command.Cursor
    ( handleWorldCancelChopCommand, handleWorldCancelConstructCommand
    , handleWorldCancelPlantCommand, handleWorldCancelTillCommand
    , handleWorldDesignateChopCommand, handleWorldDesignateConstructCommand
    , handleWorldDesignateMineCommand, handleWorldDesignatePlantCommand
    , handleWorldDesignateTillCommand, handleWorldSetConstructStatusCommand
    , handleWorldSetMineAnchorCommand )
import World.Thread.Command.Cursor.Common (designateRect, maxDesignateSide)
import World.Tile.Types (WorldTileData(..))
import World.Tool.Types (ToolMode(..))
import World.Till.Types (newTillDesignation)
import World.Vegetation (vegTilledSoil)

-- * Geometry
--
--   worldSize 64 → wrap period 64 chunks, canonical chunk u ∈ [-32, 32),
--   one u-alias step = 32 chunks = 512 tiles.

worldSize ∷ Int
worldSize = 64

zSlice ∷ Int
zSlice = 10

-- | Two PHYSICALLY ADJACENT chunks that straddle the seam: @inner@ has
--   u = 31, the last canonical value, and @outer@ has u = 32 and is
--   therefore stored a whole world away, under 'outerStored'.
innerChunk, outerRaw, outerStored ∷ ChunkCoord
innerChunk  = ChunkCoord 16 (-15)
outerRaw    = ChunkCoord 17 (-15)
outerStored = ChunkCoord (-15) 17

-- | A row of five tiles crossing that boundary: two in @inner@, three in
--   @outer@. This is the rectangle a player drags.
rowY ∷ Int
rowY = (-15) * chunkSize + 8

anchorTile, farTilePicked, farTileLocal ∷ (Int, Int)
anchorTile    = (16 * chunkSize + 14, rowY)          -- inner, canonical
farTilePicked = canonicalTile worldSize (17 * chunkSize + 2) rowY
                                                      -- outer, as PICKED
-- | The far tile as a worker standing in 'innerChunk' sees it: the
--   u-alias of its canonical key that sits four tiles away rather than a
--   world away. What @getPendingJobs@ reports as @lx@/@ly@.
farTileLocal  = (17 * chunkSize + 2, rowY)

-- | What the drag must designate: the five physical tiles, each under
--   its own canonical key.
drawnKeys ∷ [(Int, Int)]
drawnKeys = sort [ canonicalTile worldSize gx rowY
                 | gx ← [16 * chunkSize + 14 .. 17 * chunkSize + 2] ]

-- * Forage-query geometry (#1707)
--
--   One search origin in the inner chunk, and three candidates around
--   it. The seam-crossing one is PHYSICALLY the nearest; the planar one
--   is genuinely farther but is the only candidate a raw chunk-box scan
--   can even reach, so it is what the pre-#1707 query returned.

-- | The forager's tile: 'anchorTile', two tiles short of the seam.
forageOrigin ∷ (Int, Int)
forageOrigin = anchorTile

-- | 4 tiles from the origin, across the seam: 'farTilePicked' is its
--   canonical key, 'farTileLocal' the same tile in the origin's frame.
seamCandidate ∷ (Int, Int)
seamCandidate = farTilePicked

-- | The inner chunk's corner tile — sqrt 260 ≈ 16.12 tiles away, so
--   strictly farther than 'seamCandidate', and the only one of the two a
--   raw chunk-box scan can reach.
planarCandidate ∷ (Int, Int)
planarCandidate = (16 * chunkSize, (-15) * chunkSize)

-- | Radius covering both candidates, well inside the 64 clamp.
forageRadius ∷ Int
forageRadius = 20

-- | Two candidates at EXACTLY equal distance from the origin, one along
--   each axis, both inside the inner chunk. The historical ordering
--   settles it on gx, so 'tieByX' (the smaller gx) must win.
tieByX, tieByY ∷ (Int, Int)
tieByX = (fst forageOrigin - 4, snd forageOrigin)
tieByY = (fst forageOrigin, snd forageOrigin - 4)

-- * Chunk fixtures

baseChunk ∷ ChunkCoord → Word8 → FloraChunkData → LoadedChunk
baseChunk coord veg flora =
    let area = chunkSize * chunkSize
        col  = ColumnTiles
                 { ctStartZ = 0
                 , ctMats   = VU.replicate 20 1
                 , ctSlopes = VU.replicate 20 0
                 , ctVeg    = VU.replicate 20 veg
                 }
    in LoadedChunk
        { lcCoord = coord
        , lcTiles = V.replicate area col
        , lcSurfaceMap = VU.replicate area zSlice
        , lcTerrainSurfaceMap = VU.replicate area zSlice
        , lcFluidMap = V.replicate area Nothing
        , lcIceMap = emptyIceMap, lcFlora = flora
        , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
        , lcMagma = Nothing, lcStructures = emptyChunkStructures
        }

-- | The pair of chunks, keyed the way the chunk loader stores them: the
--   inner one under itself, the outer one under its wrapped alias.
seamTiles ∷ Word8 → (ChunkCoord → FloraChunkData) → WorldTileData
seamTiles veg flora = WorldTileData
    { wtdChunks = HM.fromList
        [ (innerChunk,  baseChunk innerChunk  veg (flora innerChunk))
        , (outerStored, baseChunk outerStored veg (flora outerStored)) ]
    , wtdMaxChunks = 200
    }

noFlora ∷ ChunkCoord → FloraChunkData
noFlora _ = emptyFloraChunkData

probeTreeId ∷ FloraId
probeTreeId = FloraId 1

-- | A wood-tagged tree on every tile of both seam chunks, so chop's
--   commit has an eligible target wherever the rectangle lands.
treesEverywhere ∷ ChunkCoord → FloraChunkData
treesEverywhere _ = emptyFloraChunkData
    { fcdInstances =
        [ FloraInstance
            { fiSpecies = probeTreeId
            , fiTileX = fromIntegral lx, fiTileY = fromIntegral ly
            , fiOffU = 0, fiOffV = 0, fiZ = zSlice
            , fiAge = 1, fiHealth = 1, fiVariant = 0, fiBaseWidth = 8
            }
        | lx ← [0 .. chunkSize - 1 ∷ Int], ly ← [0 .. chunkSize - 1 ∷ Int] ]
    }

woodCatalog ∷ FloraCatalog
woodCatalog =
    let sp = (newFloraSpecies "probe_tree" (TextureHandle 0))
                { fsHarvest = Just FloraHarvest
                    { fhTags = ["wood"], fhYield = []
                    , fhRegrowth = 0, fhHarvestedTexture = TextureHandle 0 } }
    in emptyFloraCatalog { fcSpecies = HM.fromList [(1, sp)], fcNextId = 2 }

-- | Flora instances at named CANONICAL tiles, dropped into whichever
--   seam chunk stores each — the per-chunk shape 'seamTiles' wants.
floraAtTiles ∷ [((Int, Int), FloraId)] → ChunkCoord → FloraChunkData
floraAtTiles spots coord = emptyFloraChunkData
    { fcdInstances =
        [ FloraInstance
            { fiSpecies = fid
            , fiTileX = fromIntegral lx, fiTileY = fromIntegral ly
            , fiOffU = 0, fiOffV = 0, fiZ = zSlice
            , fiAge = 1, fiHealth = 1, fiVariant = 0, fiBaseWidth = 8
            }
        | (tile, fid) ← spots
        , let (home, (lx, ly)) = globalToChunk (fst tile) (snd tile)
        , home ≡ coord ]
    }

berrySpecies, cloverSpecies, logSpecies ∷ FloraId
berrySpecies  = FloraId 1
cloverSpecies = FloraId 2
logSpecies    = FloraId 3

-- | Three species that separate the bare and tagged scans (#97/#1707):
--   two whose yield is EDIBLE and carry no tags (a bare call takes
--   them), and one wood-tagged whose yield is not (only a "wood" call
--   takes it). Evergreen with no phases and no annual cycle, so the #332
--   growth window is open and these examples test the coordinate frame
--   and nothing else.
forageCatalog ∷ FloraCatalog
forageCatalog =
    let harvestOf tags yield = Just FloraHarvest
            { fhTags = tags, fhYield = yield
            , fhRegrowth = 0, fhHarvestedTexture = TextureHandle 0 }
        sp name tags yield = (newFloraSpecies name (TextureHandle 0))
            { fsHarvest = harvestOf tags yield }
    in emptyFloraCatalog
        { fcSpecies = HM.fromList
            [ (1, sp "probe_berry"  [] [("probe_fruit", 1, 1)])
            , (2, sp "probe_clover" [] [("probe_fruit", 1, 1)])
            , (3, sp "probe_log" ["wood"] [("probe_timber", 1, 1)]) ]
        , fcNextId = 4 }

-- | One edible item and one inedible one, so 'edibleYield' — the gate a
--   BARE world.findHarvestableFlora applies — has something to separate.
forageItems ∷ ItemManager
forageItems = ItemManager $ HM.fromList
    [ ("probe_fruit",  baseItem { idName = "probe_fruit"
                                , idFood = Just (ItemFood 50 0) })
    , ("probe_timber", baseItem { idName = "probe_timber" }) ]
  where
    baseItem = ItemDef
        { idName = "", idDisplayName = "", idTexture = TextureHandle 0
        , idWeight = 1, idWeightSpec = Nothing, idBulk = 1
        , idStorage = Nothing, idKind = "misc", idCategory = "Misc"
        , idMake = "", idMaterial = "", idQualitySpec = Nothing
        , idQualityTiers = [], idContainer = Nothing
        , idDefaultContents = [], idFood = Nothing, idWeapon = Nothing
        , idArmor = Nothing, idUnequippable = False, idBuffs = []
        , idInsulation = 0, idSourcePath = "test-fixture"
        }

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "seam_frame_probe"

wirePiece ∷ ConstructTarget
wirePiece = CtStructure (StructurePiece "wire" "wire" Nothing)

-- * Pure helpers

spec ∷ Spec
spec = do

  describe "anchor-local alias frame" $ do
    let step = tileAliasStep worldSize
        anchor = (100, 100)

    it "is the identity for a non-wrapping world" $
      localizeTileToAnchor 0 anchor (5, 5) `shouldBe` (5, 5)

    it "is the identity away from the seam" $
      localizeTileToAnchor worldSize anchor (104, 97) `shouldBe` (104, 97)

    it "brings a whole-world-away alias back next to the anchor" $
      -- Exactly the pair a seam-crossing drag produces: adjacent tiles,
      -- reported one wrap apart.
      localizeTileToAnchor worldSize anchor (100 - step, 100 + step)
        `shouldBe` anchor

    it "keeps the supplied alias on an exact tie" $ do
      -- Equidistant candidates must not silently prefer a wrapped image;
      -- half a step each way from the anchor along the alias axis.
      let half = step `div` 2
          supplied = (100 + half, 100 - half)
      localizeTileToAnchor worldSize anchor supplied `shouldBe` supplied

  describe "rectangle formation" $ do
    it "keeps a seam-crossing drag the size it was drawn" $ do
      let ((xLo, yLo), (xHi, yHi)) =
              designateRect worldSize anchorTile farTilePicked
      (xHi - xLo + 1) `shouldBe` 5
      (yHi - yLo + 1) `shouldBe` 1

    it "would otherwise have swept the whole cap" $ do
      -- The measured regression this contract exists to prevent: raw
      -- min/max over the SAME two picks.
      let (ax, ay) = anchorTile
          (bx, by) = farTilePicked
          rawLo = min ax bx
          rawHi = min (max ax bx) (rawLo + maxDesignateSide - 1)
      (rawHi - rawLo + 1) `shouldBe` maxDesignateSide
      by `shouldNotBe` ay

    it "still applies the cap to a genuinely huge drag" $ do
      let ((xLo, _), (xHi, _)) = designateRect worldSize (0, 0) (5000, 0)
      (xHi - xLo + 1) `shouldBe` maxDesignateSide

    it "is unchanged away from the seam" $
      designateRect worldSize (10, 10) (14, 12)
        `shouldBe` ((10, 10), (14, 12))

  describe "seam-aware point maths" $ do
    it "measures a designation through its nearest alias" $ do
      let step = tileAliasStep worldSize
          key  = (100 - step, 100 + step)
      -- Same physical tile as the query point: distance 0, where the
      -- raw compare it replaces saw two half-worlds.
      seamTileDist2 worldSize (100, 100) key `shouldBe` 0
      seamTileDist2 0 (100, 100) key `shouldSatisfy` (> 0)

    it "counts a region's chunk aliases as inside it" $ do
      chunkInSeamRegion worldSize (16, -16) (17, -14) innerChunk
        `shouldBe` True
      -- outerStored is the alias of a chunk the raw box names, so a
      -- worker scanning around itself must still see the job.
      chunkInSeamRegion worldSize (16, -16) (17, -14) outerStored
        `shouldBe` True
      chunkInSeamRegion worldSize (0, 0) (1, 1) innerChunk
        `shouldBe` False

    it "is the identity for a non-wrapping world" $
      chunkInSeamRegion 0 (0, 0) (1, 1) outerStored `shouldBe` False

  -- The wire LAYOUT of @world-activity@ did not change at v2; its KEY
  -- INVARIANT did. A v1 payload's designations were keyed in whatever
  -- frame the pick of the day reported, so a legacy alias key has to be
  -- re-keyed on the way in — or the designation is loaded into a map
  -- everything else now addresses canonically, and is unreachable from
  -- every read and cancel.
  describe "world-activity v1 → v2 load migration" $ do
    let aliasKey = aliasOf anchorTile
        v1Slice = (emptyActivity fixturePage)
            { padMine = HM.fromList [(aliasKey, MineDesignationDTO zSlice
                                                    (0, 0, 0, 0) 0)]
            , padTill = HM.fromList [(aliasKey, TillDesignationDTO zSlice)] }
        applyAt ver slice = applyWorldActivity ver (WorldActivityDTO [slice])
            (HM.fromList [(fixturePage, basePage)])

    it "re-keys a legacy alias-keyed designation into the stored frame" $
      case applyAt 1 v1Slice of
        Left errs → expectationFailure ("v1 apply failed: " ⧺ show errs)
        Right pages → case HM.lookup fixturePage pages of
          Nothing → expectationFailure "page vanished"
          Just p → do
            HM.keys (pgsMineDesignations p) `shouldBe` [anchorTile]
            HM.keys (pgsTillDesignations p) `shouldBe` [anchorTile]

    it "leaves an already-canonical v2 payload untouched" $
      case applyAt 2 (v1Slice { padMine = HM.fromList
                                  [(anchorTile, MineDesignationDTO zSlice
                                                    (0, 0, 0, 0) 0)] }) of
        Left errs → expectationFailure ("v2 apply failed: " ⧺ show errs)
        Right pages → case HM.lookup fixturePage pages of
          Nothing → expectationFailure "page vanished"
          Just p → HM.keys (pgsMineDesignations p) `shouldBe` [anchorTile]

    it "collapses two aliases of one tile to a single key" $
      -- A v1 save could name the same physical tile twice; the count
      -- must not stay inflated after the repair.
      case applyAt 1 (v1Slice { padTill = HM.fromList
                                  [ (anchorTile, TillDesignationDTO zSlice)
                                  , (aliasKey,   TillDesignationDTO zSlice) ] }) of
        Left errs → expectationFailure ("v1 apply failed: " ⧺ show errs)
        Right pages → case HM.lookup fixturePage pages of
          Nothing → expectationFailure "page vanished"
          Just p → HM.size (pgsTillDesignations p) `shouldBe` 1

-- | A page core carrying THIS fixture's world size — the only thing
--   'applyWorldActivity' reads off it, and the reason the component
--   depends on @world-pages@.
basePage ∷ PageSnapshot
basePage = blankPageSnapshot fixturePage
    defaultWorldGenParams { wgpWorldSize = worldSize }

emptyActivity ∷ WorldPageId → PageActivityDTO
emptyActivity pid = PageActivityDTO
    { padPageId = pid
    , padMine = HM.empty, padConstruct = HM.empty, padChop = HM.empty
    , padTill = HM.empty, padPlant = HM.empty
    , padFloraHarvests = HM.empty, padCropPlots = HM.empty
    , padGroundItems = GroundItemsDTO 0 HM.empty
    , padSpoilPiles = HM.empty
    , padConstructNextAttempt = firstConstructAttemptId
    }

-- * Engine-backed

engineSpec ∷ Spec
engineSpec = beforeAll setup $ do

  describe "the fixture really straddles the seam (#1175)" $ do
    it "stores the outer chunk only under its wrapped key" $ \_ → do
      let tiles = seamTiles 0 noFlora
      HM.member outerRaw (wtdChunks tiles) `shouldBe` False
      HM.member outerStored (wtdChunks tiles) `shouldBe` True

    it "reports the two adjacent picks a whole world apart" $ \_ → do
      -- If this collapsed the rectangle examples would pass trivially.
      let (ax, _) = anchorTile
          (bx, _) = farTilePicked
      abs (ax - bx) `shouldSatisfy` (> maxDesignateSide)

  describe "a seam-crossing drag designates exactly the drawn tiles" $ do

    it "mine" $ \(env, _) → do
      ws ← resetPage env 0 noFlora
      logger ← readIORef (loggerRef env)
      -- Through setMineAnchor, so the anchor's own canonicalisation is
      -- part of the path under test.
      handleWorldSetMineAnchorCommand env logger fixturePage
          (fst anchorTile) (snd anchorTile)
      handleWorldDesignateMineCommand env logger fixturePage
          (fst anchorTile) (snd anchorTile)
          (fst farTilePicked) (snd farTilePicked)
      keysOf (wsMineDesignationsRef ws) `shouldReturn` drawnKeys

    it "till" $ \(env, _) → do
      ws ← resetPage env 0 noFlora
      logger ← readIORef (loggerRef env)
      handleWorldDesignateTillCommand env logger fixturePage
          (fst anchorTile) (snd anchorTile)
          (fst farTilePicked) (snd farTilePicked)
      keysOf (wsTillDesignationsRef ws) `shouldReturn` drawnKeys

    it "construction (structure pieces)" $ \(env, _) → do
      ws ← resetPage env 0 noFlora
      logger ← readIORef (loggerRef env)
      handleWorldDesignateConstructCommand env logger fixturePage
          (fst anchorTile) (snd anchorTile)
          (fst farTilePicked) (snd farTilePicked) wirePiece Nothing
      keysOf (wsConstructDesignationsRef ws) `shouldReturn` drawnKeys

    it "chop" $ \(env, _) → do
      ws ← resetPage env 0 treesEverywhere
      writeIORef (floraCatalogRef env) woodCatalog
      logger ← readIORef (loggerRef env)
      handleWorldDesignateChopCommand env logger fixturePage
          (fst anchorTile) (snd anchorTile)
          (fst farTilePicked) (snd farTilePicked) "wood"
      keysOf (wsChopDesignationsRef ws) `shouldReturn` drawnKeys

  describe "Till is restricted to level ground" $ do

    it "refuses a sloped anchor" $ \(env, _) → do
      ws ← resetPageWithSlopes env [anchorTile]
      logger ← readIORef (loggerRef env)
      handleWorldDesignateTillCommand env logger fixturePage
          (fst anchorTile) (snd anchorTile)
          (fst anchorTile) (snd anchorTile)
      keysOf (wsTillDesignationsRef ws) `shouldReturn` []

    it "filters a sloped tile out of an otherwise level drag" $ \(env, _) → do
      ws ← resetPageWithSlopes env [farTilePicked]
      logger ← readIORef (loggerRef env)
      handleWorldDesignateTillCommand env logger fixturePage
          (fst anchorTile) (snd anchorTile)
          (fst farTilePicked) (snd farTilePicked)
      keysOf (wsTillDesignationsRef ws)
          `shouldReturn` filter (≢ farTilePicked) drawnKeys

    it "refuses a tile whose surface slope entry is missing" $ \(env, _) → do
      ws ← resetPageWithoutSurfaceSlope env anchorTile
      logger ← readIORef (loggerRef env)
      handleWorldDesignateTillCommand env logger fixturePage
          (fst anchorTile) (snd anchorTile)
          (fst anchorTile) (snd anchorTile)
      keysOf (wsTillDesignationsRef ws) `shouldReturn` []

  describe "the preview draws exactly what the commit designates" $ do

    it "mine" $ \(env, _) → do
      -- The pairing the issue turns on: a preview built in one frame and
      -- a commit in another is how "preview and commit agree — on the
      -- WRONG tiles" happens. Both run the anchor-local localisation,
      -- so the ghost is the same five tiles the commit stores.
      ws ← resetPage env 0 noFlora
      n ← previewQuadCount env ws MineTool
              (\cs → cs { mineAnchor = Just anchorTile })
      n `shouldBe` length drawnKeys

    it "construction, including the wire line snap" $ \(env, _) → do
      ws ← resetPage env 0 noFlora
      rect ← previewQuadCount env ws BuildTool
              (\cs → cs { constructAnchor = Just anchorTile })
      rect `shouldBe` length drawnKeys
      -- Line mode picks its axis from the anchor-relative delta, which
      -- is meaningless in the canonical frame: a seam-crossing drag
      -- would snap to the wrong axis and preview a single tile.
      line ← previewQuadCount env ws BuildTool
              (\cs → cs { constructAnchor = Just anchorTile
                        , constructLineMode = True })
      line `shouldBe` length drawnKeys

    it "chop" $ \(env, _) → do
      ws ← resetPage env 0 noFlora
      n ← previewQuadCount env ws ChopTool
              (\cs → cs { chopAnchor = Just anchorTile })
      n `shouldBe` length drawnKeys

    it "till" $ \(env, _) → do
      ws ← resetPage env 0 noFlora
      n ← previewQuadCount env ws TillTool
              (\cs → cs { tillAnchor = Just anchorTile
                        , tillDesignTexture = Just tillMarkerTexture })
      n `shouldBe` length drawnKeys

  describe "Till production facemap routing" $ do

    it "uses the neutral map and approved texture for committed markers" $
        \(env, _) → do
      ws ← resetPage env 0 noFlora
      writeIORef (wsTexturesRef ws) defaultWorldTextures
          { wtIsoFaceMap = isoFaceMapTexture }
      writeIORef (wsTillDesignationsRef ws) $ HM.singleton anchorTile
          (newTillDesignation zSlice)
      quads ← markerQuads env ws emptyCursorState
          { tillDesignTexture = Just tillMarkerTexture }
      case V.toList quads of
        [quad] → do
          sqTexture quad `shouldBe` tillMarkerTexture
          quadFaceMaps quad `shouldBe` replicate 4 noFaceMapVertexId
        _ → expectationFailure
            ("expected one committed Till marker, got " ⧺ show (V.length quads))

    it "uses the neutral map and approved texture for the rectangle preview" $
        \(env, _) → do
      ws ← resetPage env 0 noFlora
      writeIORef (wsTexturesRef ws) defaultWorldTextures
          { wtIsoFaceMap = isoFaceMapTexture }
      quads ← previewQuads env ws TillTool
          (\cs → cs { tillAnchor = Just anchorTile
                    , tillDesignTexture = Just tillMarkerTexture })
      V.length quads `shouldBe` length drawnKeys
      map sqTexture (V.toList quads)
          `shouldBe` replicate (length drawnKeys) tillMarkerTexture
      map quadFaceMaps (V.toList quads)
          `shouldBe` replicate (length drawnKeys)
              (replicate 4 noFaceMapVertexId)

  describe "one physical tile is one stored key" $
    it "re-designating through the other alias does not add an entry" $
        \(env, _) → do
      ws ← resetPage env 0 noFlora
      logger ← readIORef (loggerRef env)
      let alias = aliasOf anchorTile
      handleWorldDesignateTillCommand env logger fixturePage
          (fst anchorTile) (snd anchorTile) (fst anchorTile) (snd anchorTile)
      handleWorldDesignateTillCommand env logger fixturePage
          (fst alias) (snd alias) (fst alias) (snd alias)
      m ← readIORef (wsTillDesignationsRef ws)
      HM.size m `shouldBe` 1
      HM.keys m `shouldBe` [anchorTile]

  describe "create through one alias, read and cancel through another" $ do

    it "till" $ \(env, ls) → do
      ws ← resetPage env 0 noFlora
      logger ← readIORef (loggerRef env)
      let alias = aliasOf anchorTile
      handleWorldDesignateTillCommand env logger fixturePage
          (fst alias) (snd alias) (fst alias) (snd alias)
      HM.keys <$> readIORef (wsTillDesignationsRef ws)
          `shouldReturn` [anchorTile]
      -- Read back through BOTH names, and confirm the reported coords
      -- are canonical either way.
      forM_ [anchorTile, alias] $ \(gx, gy) →
        evalDebug ls (T.concat
            [ "local d = till.getDesignationAt('", pageText, "', "
            , tshow gx, ", ", tshow gy, "); "
            , "return d and (d.x .. ',' .. d.y) or 'nil'" ])
          `shouldReturn` tshow (fst anchorTile) <> "," <> tshow (snd anchorTile)
      handleWorldCancelTillCommand env logger fixturePage
          (fst alias) (snd alias)
      HM.size <$> readIORef (wsTillDesignationsRef ws) `shouldReturn` 0

    it "chop" $ \(env, ls) → do
      ws ← resetPage env 0 noFlora
      logger ← readIORef (loggerRef env)
      let alias = aliasOf anchorTile
      writeIORef (wsChopDesignationsRef ws)
          (HM.fromList [(anchorTile, newChopDesignation zSlice)])
      evalDebug ls (T.concat
          [ "local d = chop.getDesignationAt('", pageText, "', "
          , tshow (fst alias), ", ", tshow (snd alias), "); "
          , "return d and tostring(d.z) or 'nil'" ])
        `shouldReturn` tshow zSlice
      handleWorldCancelChopCommand env logger fixturePage
          (fst alias) (snd alias)
      HM.size <$> readIORef (wsChopDesignationsRef ws) `shouldReturn` 0

    it "plant" $ \(env, ls) → do
      _ ← resetPage env vegTilledSoil noFlora
      writeIORef (floraCatalogRef env) cropCatalog
      ws ← pageState env
      logger ← readIORef (loggerRef env)
      let alias = aliasOf anchorTile
      handleWorldDesignatePlantCommand env logger fixturePage
          (fst alias) (snd alias) "probe_crop"
      HM.keys <$> readIORef (wsPlantDesignationsRef ws)
          `shouldReturn` [anchorTile]
      evalDebug ls (T.concat
          [ "local d = plant.getDesignationAt('", pageText, "', "
          , tshow (fst alias), ", ", tshow (snd alias), "); "
          , "return d and (d.x .. ',' .. d.y) or 'nil'" ])
        `shouldReturn` tshow (fst anchorTile) <> "," <> tshow (snd anchorTile)
      handleWorldCancelPlantCommand env logger fixturePage
          (fst alias) (snd alias)
      HM.size <$> readIORef (wsPlantDesignationsRef ws) `shouldReturn` 0

    it "mine reads through either alias (its consumption path is dig, \
       \not a public cancel)" $ \(env, ls) → do
      ws ← resetPage env 0 noFlora
      logger ← readIORef (loggerRef env)
      let alias = aliasOf anchorTile
      handleWorldDesignateMineCommand env logger fixturePage
          (fst alias) (snd alias) (fst alias) (snd alias)
      HM.keys <$> readIORef (wsMineDesignationsRef ws)
          `shouldReturn` [anchorTile]
      forM_ [anchorTile, alias] $ \(gx, gy) →
        evalDebug ls (T.concat
            [ "local z = world.getMineDesignationAt('", pageText, "', "
            , tshow gx, ", ", tshow gy, "); "
            , "return z and tostring(z) or 'nil'" ])
          `shouldReturn` tshow zSlice

  describe "every construction job verb shares that key" $
    it "status, progress, payment, refund, region scan and nearest" $
        \(env, ls) → do
      ws ← resetPage env 0 noFlora
      logger ← readIORef (loggerRef env)
      let alias@(agx, agy) = aliasOf anchorTile
          (cgx, cgy) = anchorTile
      handleWorldDesignateConstructCommand env logger fixturePage
          agx agy agx agy wirePiece Nothing
      HM.keys <$> readIORef (wsConstructDesignationsRef ws)
          `shouldReturn` [anchorTile]

      -- Read + mutate through the alias, observe on the canonical key.
      evalDebug ls (T.concat
          [ "local j = construction.getDesignationAt('", pageText, "', "
          , tshow agx, ", ", tshow agy, "); "
          , "return j and (j.x .. ',' .. j.y) or 'nil'" ])
        `shouldReturn` tshow cgx <> "," <> tshow cgy
      claimAttempt ← cdAttempt <$> designationAt ws anchorTile
      handleWorldSetConstructStatusCommand env logger fixturePage
          agx agy CsClaimed claimAttempt Nothing
      cdStatus <$> designationAt ws anchorTile `shouldReturn` CsClaimed
      _ ← evalDebug ls (T.concat
          [ "construction.addJobProgress('", pageText, "', "
          , tshow agx, ", ", tshow agy, ", 0.5, "
          , tshow (rawAttempt claimAttempt), "); return 'ok'" ])
      -- #1844: the two NEW coordinate verbs are alias-tolerant like
      -- every other one here. `beginPlacement` is a compare-and-set on
      -- the exact attempt, so it also proves the alias resolved to the
      -- SAME designation the canonical read above found — a different
      -- one would carry a different attempt and refuse.
      attempt ← cdAttempt <$> designationAt ws anchorTile
      let ConstructAttemptId attemptN = attempt
      evalDebug ls (T.concat
          [ "return tostring(construction.beginPlacement('", pageText, "', "
          , tshow agx, ", ", tshow agy, ", ", tshow attemptN, "))" ])
        `shouldReturn` "true"
      cdStatus <$> designationAt ws anchorTile `shouldReturn` CsPlacing
      -- …and refuses an attempt that is not the one standing there.
      evalDebug ls (T.concat
          [ "return tostring(construction.beginPlacement('", pageText, "', "
          , tshow agx, ", ", tshow agy, ", ", tshow (attemptN + 1), "))" ])
        `shouldReturn` "false"
      -- The durable PAYMENT record the refund pop below reads. Written
      -- here directly rather than through construction.payMaterials: the
      -- verb's own losslessness is
      -- 'Test.Headless.Construct.AttemptIdentity's subject, and what
      -- this example is pinning is that the popped job reports the
      -- receipt through the tile's alias.
      -- …and back out of the hand-off, because a PLACING designation is
      -- deliberately not poppable (#1844: cancelling one would refund a
      -- receipt while the queued placement still lands). What the refund
      -- pop below is pinning is its ALIAS tolerance, not that race.
      atomicModifyIORef' (wsConstructDesignationsRef ws) $ \m →
          ( HM.adjust (\cd → cd { cdStatus = CsClaimed
                               , cdPayment =
                CpPaid (mkMaterialReceipt [("wiring", 1)]) }) anchorTile m
          , () )
      cd ← designationAt ws anchorTile
      constructDesignationPaid cd `shouldBe` True

      -- A worker's scan box is a RAW chunk region around itself, so at
      -- the seam it names the alias of the stored key.
      evalDebug ls (T.concat
          [ "local js = construction.getPendingJobs(16, -16, 17, -14); "
          , "return tostring(#js)" ])
        `shouldReturn` "1"
      -- Nearest reports the canonical key, measured through its nearest
      -- alias to the (aliased) query point.
      evalDebug ls (T.concat
          [ "local x, y = construction.nearestDesignation('", pageText, "', "
          , tshow agx, ", ", tshow agy, "); "
          , "return (x and (x .. ',' .. y)) or 'nil'" ])
        `shouldReturn` tshow cgx <> "," <> tshow cgy

      -- The synchronous refund pop and the queued cancel must agree.
      evalDebug ls (T.concat
          [ "local j = construction.cancelDesignationForRefund('"
          , pageText, "', ", tshow agx, ", ", tshow agy, "); "
          , "local r = j and j.receipt and j.receipt[1]; "
          , "return j and (j.x .. ',' .. j.y .. ',' .. tostring(j.paid) "
          , "     .. ',' .. tostring(r and r.name) "
          , "     .. ',' .. tostring(r and r.count)) or 'nil'" ])
        `shouldReturn` tshow cgx <> "," <> tshow cgy <> ",true,wiring,1"
      HM.size <$> readIORef (wsConstructDesignationsRef ws) `shouldReturn` 0

      handleWorldDesignateConstructCommand env logger fixturePage
          cgx cgy cgx cgy wirePiece Nothing
      handleWorldCancelConstructCommand env logger fixturePage
          (fst alias) (snd alias) Nothing
      HM.size <$> readIORef (wsConstructDesignationsRef ws) `shouldReturn` 0

  describe "a job across the seam is reachable AND measurable" $
    it "getPendingJobs reports it, canonically keyed and worker-local" $
        \(env, ls) → do
      -- The worker stands in the inner chunk; the job's canonical key
      -- lives a whole world away because its chunk is stored wrapped.
      ws ← resetPage env 0 noFlora
      logger ← readIORef (loggerRef env)
      handleWorldDesignateConstructCommand env logger fixturePage
          (fst farTilePicked) (snd farTilePicked)
          (fst farTilePicked) (snd farTilePicked) wirePiece Nothing
      HM.keys <$> readIORef (wsConstructDesignationsRef ws)
          `shouldReturn` [farTilePicked]
      -- Listing it at all needs the region test to count chunk aliases;
      -- x/y stay the canonical key every point verb accepts, and lx/ly
      -- put the SAME tile back beside the worker so a range gate on
      -- construct_scan_range can actually pass. Measured raw, the job is
      -- a world away and no builder would ever claim it.
      evalDebug ls (T.concat
          [ "local js = construction.getPendingJobs(15, -16, 17, -14); "
          , "if #js ~= 1 then return 'count=' .. #js end; "
          , "local j = js[1]; "
          , "return j.x .. ',' .. j.y .. '|' .. j.lx .. ',' .. j.ly" ])
        `shouldReturn` tshow (fst farTilePicked) <> ","
                       <> tshow (snd farTilePicked) <> "|"
                       <> tshow (fst farTileLocal) <> ","
                       <> tshow (snd farTileLocal)

  describe "the worker verbs that finish a job take the same alias" $ do

    it "structure.hasAt sees an occupied seam tile through its alias" $
        \(env, ls) → do
      -- Concern behind #1175 round 1: build_tool's occupancy pre-check
      -- scans ANCHOR-LOCAL alias tiles, so a raw structure lookup reports
      -- an occupied seam tile as free and the tool records an accepted
      -- outcome for a commit that will create no jobs.
      _ ← resetPageWithFloor env
      evalDebug ls (T.concat
          [ "return tostring(structure.hasAt(", tshow (fst anchorTile), ", "
          , tshow (snd anchorTile), ", 'floor')) .. ',' .. "
          , "tostring(structure.hasAt(", tshow (fst (aliasOf anchorTile))
          , ", ", tshow (snd (aliasOf anchorTile)), ", 'floor'))" ])
        `shouldReturn` "true,true"

    it "world.getDigInfoAt resolves an aliased dig job" $ \(env, ls) → do
      -- A digJob restored from a pre-#1175 save can still hold an alias;
      -- read and consume must find the canonical designation it names.
      ws ← resetPage env 0 noFlora
      logger ← readIORef (loggerRef env)
      handleWorldDesignateMineCommand env logger fixturePage
          (fst anchorTile) (snd anchorTile) (fst anchorTile) (snd anchorTile)
      HM.keys <$> readIORef (wsMineDesignationsRef ws)
          `shouldReturn` [anchorTile]
      let (agx, agy) = aliasOf anchorTile
      evalDebug ls (T.concat
          [ "local m = world.getDigInfoAt('", pageText, "', "
          , tshow agx, ", ", tshow agy, "); "
          , "return m and tostring(m) or 'nil'" ])
        `shouldReturn` "1"

  -- The forage half of the same contract (#1707). world.harvestFlora
  -- already canonicalises, so the ACTION verb was correct while the two
  -- DISCOVERY surfaces that steer a unit to it were not: the search
  -- scanned raw chunk keys and measured planar distance, and the point
  -- queries read the caller's raw coord straight through.
  --
  -- (world.getCropPlotAt's own raw lookup is deliberately out of scope
  -- here — it is a separate surface with its own follow-up.)
  describe "the forage search is seam-aware (#1707)" $ do

    it "reaches a seam-crossing candidate and ranks it ahead of a \
       \nearer-LOOKING planar one" $ \(env, ls) → do
      -- 4 tiles away physically against 16.1 — but the near one's chunk
      -- is stored a whole world along u, so the raw box scan never named
      -- its key, and the raw distance would have failed the radius even
      -- if it had.
      _ ← forageWorld env worldSize 0 (floraAtTiles
              [ (seamCandidate,   berrySpecies)
              , (planarCandidate, berrySpecies) ])
      findFlora ls forageOrigin forageRadius Nothing
        `shouldReturn` expectedFind seamCandidate "probe_berry" "4.00"

    it "answers the same when the search ORIGIN is the other alias" $
        \(env, ls) → do
      _ ← forageWorld env worldSize 0 (floraAtTiles
              [ (seamCandidate,   berrySpecies)
              , (planarCandidate, berrySpecies) ])
      findFlora ls (aliasOf forageOrigin) forageRadius Nothing
        `shouldReturn` expectedFind seamCandidate "probe_berry" "4.00"

    it "returns the planar candidate once the seam one is gone" $
        \(env, ls) → do
      -- The control that keeps the two examples above honest: the planar
      -- candidate really is an eligible, in-range answer, so winning
      -- against it is an ORDERING result and not a filtering accident.
      _ ← forageWorld env worldSize 0
              (floraAtTiles [(planarCandidate, berrySpecies)])
      findFlora ls forageOrigin forageRadius Nothing
        `shouldReturn` expectedFind planarCandidate "probe_berry" "16.12"

    it "ranks a seam-crossing CROP PLOT on that same geometry" $
        \(env, ls) → do
      -- The plot scan (#334) is a separate branch over a flat world-level
      -- map, so it needs its own proof: plots were always REACHED (no
      -- chunk box gates them) but were measured planar, which put this
      -- one outside the radius entirely.
      ws ← forageWorld env worldSize vegTilledSoil noFlora
      writeIORef (wsCropPlotsRef ws) $ HM.fromList
          [ (seamCandidate,   newCropPlot cloverSpecies 0 1)
          , (planarCandidate, newCropPlot cloverSpecies 0 1) ]
      findFlora ls forageOrigin forageRadius Nothing
        `shouldReturn` expectedFind seamCandidate "probe_clover" "4.00"

    it "keeps the tagged scan seam-aware AND the bare/tagged split \
       \intact" $ \(env, ls) → do
      _ ← forageWorld env worldSize 0 (floraAtTiles
              [ (seamCandidate,   logSpecies)
              , (planarCandidate, berrySpecies) ])
      -- The chop flow's "wood" call (#97) crosses the seam too.
      findFlora ls forageOrigin forageRadius (Just "wood")
        `shouldReturn` expectedFind seamCandidate "probe_log" "4.00"
      -- A BARE call must still refuse the inedible yield, near as it is,
      -- and take the farther berry instead.
      findFlora ls forageOrigin forageRadius Nothing
        `shouldReturn` expectedFind planarCandidate "probe_berry" "16.12"

    it "still skips a tile whose CANONICAL key carries a live regrowth \
       \timer" $ \(env, ls) → do
      -- The timer map is canonical-keyed, and the coords the scan derives
      -- from a stored chunk are canonical, so the #94 skip keeps working
      -- across the seam rather than silently passing every wrapped tile.
      ws ← forageWorld env worldSize 0 (floraAtTiles
              [ (seamCandidate,   berrySpecies)
              , (planarCandidate, berrySpecies) ])
      writeIORef (wsFloraHarvestsRef ws) (HM.singleton seamCandidate 123)
      findFlora ls forageOrigin forageRadius Nothing
        `shouldReturn` expectedFind planarCandidate "probe_berry" "16.12"

    it "breaks an equidistant tie by canonical gx, as it always has" $
        \(env, ls) → do
      -- Distance is a Float now; the historical (d2, gx, gy, name)
      -- ordering underneath it must be untouched.
      _ ← forageWorld env worldSize 0 (floraAtTiles
              [ (tieByY, berrySpecies), (tieByX, berrySpecies) ])
      findFlora ls forageOrigin forageRadius Nothing
        `shouldReturn` expectedFind tieByX "probe_berry" "4.00"

    it "is the identity on a non-wrapping page" $ \(env, ls) → do
      -- Same fixture, world size 0: nothing aliases, so the far chunk
      -- really IS a world away and the planar candidate wins on the raw
      -- geometry — exactly the pre-#1707 answer.
      _ ← forageWorld env 0 0 (floraAtTiles
              [ (seamCandidate,   berrySpecies)
              , (planarCandidate, berrySpecies) ])
      findFlora ls forageOrigin forageRadius Nothing
        `shouldReturn` expectedFind planarCandidate "probe_berry" "16.12"

  describe "the flora point queries take either alias (#1707)" $ do

    -- Both examples pin a LIVE regrowth timer on the canonical key.
    -- 'floraAt' and the timer map are two separate lookups, so
    -- canonicalising only the first reports a real species with the
    -- wrong (default, zero) timer — harvestable, when it is not.

    it "world.getFloraAt" $ \(env, ls) → do
      ws ← forageWorld env worldSize 0
              (floraAtTiles [(seamCandidate, berrySpecies)])
      writeIORef (wsFloraHarvestsRef ws) (HM.singleton seamCandidate 123)
      forM_ [seamCandidate, farTileLocal] $ \(gx, gy) →
        evalDebug ls (T.concat
            [ "local f = world.getFloraAt(", tshow gx, ", ", tshow gy, "); "
            , "return f and (f.id .. ',' .. tostring(f.harvestable) .. ',' "
            , ".. string.format('%.0f', f.regrowthRemaining)) or 'nil'" ])
          `shouldReturn` "probe_berry,false,123"

    it "world.getFloraGrowthAt" $ \(env, ls) → do
      ws ← forageWorld env worldSize 0
              (floraAtTiles [(seamCandidate, berrySpecies)])
      writeIORef (wsFloraHarvestsRef ws) (HM.singleton seamCandidate 123)
      forM_ [seamCandidate, farTileLocal] $ \(gx, gy) →
        evalDebug ls (T.concat
            [ "local g = world.getFloraGrowthAt(", tshow gx, ", ", tshow gy
            , "); if not g then return 'nil' end; "
            , "return #g .. ',' .. g[1].id .. ',' .. "
            , "tostring(g[1].harvestable) .. ',' .. "
            , "string.format('%.0f', g[1].regrowthRemaining)" ])
          `shouldReturn` "1,probe_berry,false,123"

  where
    setup = do
        EngineInitResult env ← initializeEngineHeadless
        ls ← newBareLuaBackend env
        pure (env, ls)

-- * Fixture plumbing

pageText ∷ Text
pageText = "seam_frame_probe"

-- | The OTHER name for a tile: one u-alias step away, which is what a
--   pre-#1175 save or a caller working in a raw frame can hold.
aliasOf ∷ (Int, Int) → (Int, Int)
aliasOf (gx, gy) =
    let step = tileAliasStep worldSize
    in (gx + step, gy - step)

-- | Install a fresh synthetic page: the two seam chunks, this world
--   size, and empty designation maps.
resetPage ∷ EngineEnv → Word8 → (ChunkCoord → FloraChunkData) → IO WorldState
resetPage env = resetPageSized env worldSize

-- | 'resetPage' at an arbitrary declared world size. Size 0 is the
--   non-wrapping (arena / zero-size) page every seam helper is the
--   identity on — the control that pins "unchanged away from the seam".
resetPageSized ∷ EngineEnv → Int → Word8 → (ChunkCoord → FloraChunkData)
               → IO WorldState
resetPageSized env size veg flora = do
    -- #1844: structure commits resolve against the registered art/build
    -- catalogue, so this fixture registers the packs its descriptors
    -- name exactly as boot does.
    registerFixturePacks env
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws)
        (Just defaultWorldGenParams { wgpWorldSize = size })
    writeIORef (wsTilesRef ws) (seamTiles veg flora)
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    pure ws

-- | The standard flat fixture with selected surface tiles made non-level.
-- Till must reject these even though every other eligibility input remains
-- valid, pinning the farming-domain rule independently of generated terrain.
resetPageWithSlopes ∷ EngineEnv → [(Int, Int)] → IO WorldState
resetPageWithSlopes env slopedTiles = do
    ws ← resetPage env 0 noFlora
    writeIORef (wsTilesRef ws) (foldl' slopeTile (seamTiles 0 noFlora) slopedTiles)
    pure ws
  where
    slopeTile tileData (gx, gy) =
        let (coord, (lx, ly), _) = canonicalTileFrame worldSize gx gy
            idx = columnIndex lx ly
            setSlope lc =
                let col = lcTiles lc V.! idx
                    i = (lcSurfaceMap lc VU.! idx) - ctStartZ col
                    col' = col { ctSlopes = ctSlopes col VU.// [(i, 1)] }
                in lc { lcTiles = lcTiles lc V.// [(idx, col')] }
        in tileData
            { wtdChunks = HM.adjust setSlope coord (wtdChunks tileData) }

-- | Remove the selected surface's slope slot. A malformed column must be
-- rejected, never inferred to be level merely because no nonzero slope exists.
resetPageWithoutSurfaceSlope ∷ EngineEnv → (Int, Int) → IO WorldState
resetPageWithoutSurfaceSlope env (gx, gy) = do
    ws ← resetPage env 0 noFlora
    let tiles = seamTiles 0 noFlora
        (coord, (lx, ly), _) = canonicalTileFrame worldSize gx gy
        idx = columnIndex lx ly
        removeSlope lc =
            let col = lcTiles lc V.! idx
                i = (lcSurfaceMap lc VU.! idx) - ctStartZ col
                col' = col { ctSlopes = VU.take i (ctSlopes col) }
            in lc { lcTiles = lcTiles lc V.// [(idx, col')] }
    writeIORef (wsTilesRef ws) tiles
        { wtdChunks = HM.adjust removeSlope coord (wtdChunks tiles) }
    pure ws

-- | The seam page with a floor piece already placed on 'anchorTile',
--   stored under its CANONICAL key inside the chunk that holds it —
--   exactly how the structure overlay records a real placement.
resetPageWithFloor ∷ EngineEnv → IO WorldState
resetPageWithFloor env = do
    ws ← resetPage env 0 noFlora
    let floorTag = fromIntegral (fromEnum SFloor) ∷ Word8
        key = (fst anchorTile, snd anchorTile, floorTag)
        occupied lc = lc { lcStructures =
            HM.insert key (StructurePieceData 0 0 zSlice) (lcStructures lc) }
    writeIORef (wsTilesRef ws) $ (seamTiles 0 noFlora)
        { wtdChunks = HM.adjust occupied innerChunk
                          (wtdChunks (seamTiles 0 noFlora)) }
    pure ws

pageState ∷ EngineEnv → IO WorldState
pageState env = do
    mgr ← readIORef (worldManagerRef env)
    case lookup fixturePage (wmWorlds mgr) of
        Just ws → pure ws
        Nothing → fail "fixture page missing"

keysOf ∷ IORef (HM.HashMap (Int, Int) v) → IO [(Int, Int)]
keysOf ref = sort . HM.keys <$> readIORef ref

-- | How many quads the live cursor pass draws for a pending drag whose
--   anchor is 'anchorTile' and whose CURSOR sits over the far side of
--   the seam. Only the tool's own preview contributes: no designations
--   exist, and neither marker nor hover textures are set, so every other
--   quad source in 'renderWorldCursorQuads' is empty by construction.
--
--   The camera is parked on the far tile's ALIAS — the configuration
--   that makes the pick report a canonical coord a whole world from the
--   anchor, which is the whole point.
previewQuadCount ∷ EngineEnv → WorldState → ToolMode
                 → (CursorState → CursorState) → IO Int
previewQuadCount env ws tool arm =
    V.length <$> previewQuads env ws tool arm

previewQuads ∷ EngineEnv → WorldState → ToolMode
             → (CursorState → CursorState) → IO (V.Vector SortableQuad)
previewQuads env ws tool arm = do
    (pixX, pixY) ← configureSeamView env
    writeIORef (wsToolModeRef ws) tool
    writeIORef (wsCursorRef ws) $ arm emptyCursorState
        { worldCursorPos = Just (pixX, pixY)
        , worldCursorTexture = Just (TextureHandle 1)
        }
    renderWorldCursorQuads env ws 1.0

markerQuads ∷ EngineEnv → WorldState → CursorState
            → IO (V.Vector SortableQuad)
markerQuads env ws cursorState = do
    _ ← configureSeamView env
    writeIORef (wsToolModeRef ws) TillTool
    writeIORef (wsCursorRef ws) cursorState
    renderWorldCursorQuads env ws 1.0

configureSeamView ∷ EngineEnv → IO (Int, Int)
configureSeamView env = do
    let farAlias = aliasOf farTilePicked
        (camX, camY) = gridToWorld FaceSouth (fst farAlias) (snd farAlias)
        (wx, wy0) = gridToWorld FaceSouth (fst farAlias) (snd farAlias)
        wy = wy0 + tileHeight * 0.5
        aspect = fromIntegral previewFbW / fromIntegral previewFbH ∷ Float
        pixX = round (fromIntegral previewWinW
                      * (((wx - camX) / (previewZoom * aspect)) + 1.0) / 2.0)
        pixY = round (fromIntegral previewWinH
                      * (((wy - camY) / previewZoom) + 1.0) / 2.0)
    writeIORef (cameraRef env) defaultCamera
        { camPosition = (camX, camY), camZoom = previewZoom
        , camFacing = FaceSouth, camZSlice = zSlice }
    writeIORef (windowSizeRef env) (previewWinW, previewWinH)
    writeIORef (framebufferSizeRef env) (previewFbW, previewFbH)
    pure (pixX, pixY)

tillMarkerTexture, isoFaceMapTexture ∷ TextureHandle
tillMarkerTexture = TextureHandle 41
isoFaceMapTexture = TextureHandle 29

quadFaceMaps ∷ SortableQuad → [Float]
quadFaceMaps quad = map faceMapId [sqV0 quad, sqV1 quad, sqV2 quad, sqV3 quad]

-- | Same generous viewport 'Test.Headless.World.Render.PickSeam' uses,
--   and for the same reason: the pick must resolve rather than be culled.
previewZoom ∷ Float
previewZoom = 40.0

previewFbW, previewFbH, previewWinW, previewWinH ∷ Int
previewFbW = 800
previewFbH = 600
previewWinW = 8000
previewWinH = 6000

rawAttempt ∷ ConstructAttemptId → Word64
rawAttempt (ConstructAttemptId n) = n

designationAt ∷ WorldState → (Int, Int) → IO ConstructDesignation
designationAt ws k = do
    m ← readIORef (wsConstructDesignationsRef ws)
    case HM.lookup k m of
        Just cd → pure cd
        Nothing → fail ("no construction designation at " ⧺ show k)

-- | A registered plantable crop, so plant.designate's own species check
--   passes and the example is really testing the coordinate frame.
cropCatalog ∷ FloraCatalog
cropCatalog =
    let sp = newFloraSpecies "probe_crop" (TextureHandle 0)
    in emptyFloraCatalog
        { fcSpecies  = HM.fromList [(1, sp)]
        , fcWorldGen = HM.fromList [(1, cropWorldGen)]
        , fcNextId   = 2 }

-- | A bare "row_crop" registration — only 'fwCategory' is consulted by
--   the designation's plantable check.
cropWorldGen ∷ FloraWorldGen
cropWorldGen = FloraWorldGen
    { fwCategory = "row_crop"
    , fwMinTemp = -100, fwMaxTemp = 100, fwIdealTemp = 15
    , fwMinPrecip = 0, fwMaxPrecip = 10000, fwIdealPrecip = 500
    , fwMinAlt = -10000, fwMaxAlt = 10000, fwIdealAlt = 0
    , fwMinHumidity = 0, fwMaxHumidity = 1, fwIdealHumidity = 0.5
    , fwMaxSlope = 255, fwDensity = 0, fwSoils = [], fwFootprint = 1
    }

-- | Bare Lua backend with the real API registered — no scripts loaded,
--   so only the engine verbs under test are in play.
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | One console command, with the JSON quoting the console applies to a
--   returned string stripped — every example here returns a scalar it
--   built as a Lua string, so the quotes are transport, not content.
evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls src = T.dropAround (≡ '"') <$> executeDebugLua (lbsLuaState ls) src

-- | Drive the PRODUCTION world.findHarvestableFlora through the real
--   registered verb, flattening its table to "gx,gy,id,dist" (or "nil").
--   Every field the issue's consumers read is in that string: the coords
--   they forward to world.harvestFlora and the distance the auto-harvest
--   action divides into its utility.
findFlora ∷ LuaBackendState → (Int, Int) → Int → Maybe Text → IO Text
findFlora ls (ox, oy) radius mTag =
    evalDebug ls $ T.concat
        [ "local f = world.findHarvestableFlora(", tshow ox, ", ", tshow oy
        , ", ", tshow radius, tagArg, "); "
        , "return f and (f.gx .. ',' .. f.gy .. ',' .. f.id .. ',' .. "
        , "string.format('%.2f', f.dist)) or 'nil'" ]
  where tagArg = maybe "" (\t → T.concat [", '", t, "'"]) mTag

-- | The 'findFlora' string a given canonical tile, species and distance
--   should produce.
expectedFind ∷ (Int, Int) → Text → Text → Text
expectedFind (gx, gy) name dist =
    T.intercalate "," [tshow gx, tshow gy, name, dist]

-- | Prepare the seam page for a forage example: the given flora, this
--   fixture's three species, and the item registry the BARE call's
--   edible-yield gate consults.
forageWorld ∷ EngineEnv → Int → Word8 → (ChunkCoord → FloraChunkData)
            → IO WorldState
forageWorld env size veg flora = do
    ws ← resetPageSized env size veg flora
    writeIORef (floraCatalogRef env) forageCatalog
    writeIORef (itemManagerRef env) forageItems
    pure ws
