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
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
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
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Structure.Types (emptyChunkStructures)
import World.Chop.Types (newChopDesignation)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Construct.Types
    ( ConstructDesignation(..), ConstructStatus(..), ConstructTarget(..)
    , StructurePiece(..) )
import World.Flora.Types
    ( FloraCatalog(..), FloraChunkData(..), FloraHarvest(..), FloraId(..)
    , FloraInstance(..), FloraSpecies(..), FloraWorldGen(..)
    , emptyFloraCatalog, emptyFloraChunkData, newFloraSpecies )
import World.Cursor.Types (CursorState(..), emptyCursorState)
import World.Fluid.Types (emptyIceMap)
import World.Grid (gridToWorld, tileHeight)
import World.Generate.Coordinates
    ( canonicalTile, chunkInSeamRegion, localizeTileToAnchor, seamTileDist2
    , tileAliasStep )
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Save.Component.Page
    ( GroundItemsDTO(..), MineDesignationDTO(..), PageActivityDTO(..)
    , TillDesignationDTO(..), WorldActivityDTO(..), applyWorldActivity
    , blankPageSnapshot )
import World.Render.CursorQuads (renderWorldCursorQuads)
import World.Save.Snapshot (PageSnapshot(..))
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldState)
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

anchorTile, farTilePicked ∷ (Int, Int)
anchorTile    = (16 * chunkSize + 14, rowY)          -- inner, canonical
farTilePicked = canonicalTile worldSize (17 * chunkSize + 2) rowY
                                                      -- outer, as PICKED

-- | What the drag must designate: the five physical tiles, each under
--   its own canonical key.
drawnKeys ∷ [(Int, Int)]
drawnKeys = sort [ canonicalTile worldSize gx rowY
                 | gx ← [16 * chunkSize + 14 .. 17 * chunkSize + 2] ]

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
          (fst farTilePicked) (snd farTilePicked) wirePiece
      keysOf (wsConstructDesignationsRef ws) `shouldReturn` drawnKeys

    it "chop" $ \(env, _) → do
      ws ← resetPage env 0 treesEverywhere
      writeIORef (floraCatalogRef env) woodCatalog
      logger ← readIORef (loggerRef env)
      handleWorldDesignateChopCommand env logger fixturePage
          (fst anchorTile) (snd anchorTile)
          (fst farTilePicked) (snd farTilePicked) "wood"
      keysOf (wsChopDesignationsRef ws) `shouldReturn` drawnKeys

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
              (\cs → cs { tillAnchor = Just anchorTile })
      n `shouldBe` length drawnKeys

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
          agx agy agx agy wirePiece
      HM.keys <$> readIORef (wsConstructDesignationsRef ws)
          `shouldReturn` [anchorTile]

      -- Read + mutate through the alias, observe on the canonical key.
      evalDebug ls (T.concat
          [ "local j = construction.getDesignationAt('", pageText, "', "
          , tshow agx, ", ", tshow agy, "); "
          , "return j and (j.x .. ',' .. j.y) or 'nil'" ])
        `shouldReturn` tshow cgx <> "," <> tshow cgy
      handleWorldSetConstructStatusCommand env logger fixturePage
          agx agy CsClaimed
      cdStatus <$> designationAt ws anchorTile `shouldReturn` CsClaimed
      _ ← evalDebug ls (T.concat
          [ "construction.addJobProgress('", pageText, "', "
          , tshow agx, ", ", tshow agy, ", 0.5); return 'ok'" ])
      _ ← evalDebug ls (T.concat
          [ "construction.setMaterialsPaid('", pageText, "', "
          , tshow agx, ", ", tshow agy, ", true); return 'ok'" ])
      cd ← designationAt ws anchorTile
      cdMaterialsPaid cd `shouldBe` True

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
          , "return j and (j.x .. ',' .. j.y .. ',' .. tostring(j.paid)) "
          , "or 'nil'" ])
        `shouldReturn` tshow cgx <> "," <> tshow cgy <> ",true"
      HM.size <$> readIORef (wsConstructDesignationsRef ws) `shouldReturn` 0

      handleWorldDesignateConstructCommand env logger fixturePage
          cgx cgy cgx cgy wirePiece
      handleWorldCancelConstructCommand env logger fixturePage
          (fst alias) (snd alias)
      HM.size <$> readIORef (wsConstructDesignationsRef ws) `shouldReturn` 0

  where
    setup = do
        EngineInitResult env ← initializeEngineHeadless
        ls ← newBareLuaBackend env
        pure (env, ls)

-- * Fixture plumbing

pageText ∷ Text
pageText = "seam_frame_probe"

tshow ∷ Show α ⇒ α → Text
tshow = T.pack . show

-- | The OTHER name for a tile: one u-alias step away, which is what a
--   pre-#1175 save or a caller working in a raw frame can hold.
aliasOf ∷ (Int, Int) → (Int, Int)
aliasOf (gx, gy) =
    let step = tileAliasStep worldSize
    in (gx + step, gy - step)

-- | Install a fresh synthetic page: the two seam chunks, this world
--   size, and empty designation maps.
resetPage ∷ EngineEnv → Word8 → (ChunkCoord → FloraChunkData) → IO WorldState
resetPage env veg flora = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws)
        (Just defaultWorldGenParams { wgpWorldSize = worldSize })
    writeIORef (wsTilesRef ws) (seamTiles veg flora)
    writeIORef (worldManagerRef env) WorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
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
previewQuadCount env ws tool arm = do
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
    writeIORef (wsToolModeRef ws) tool
    writeIORef (wsCursorRef ws) $ arm emptyCursorState
        { worldCursorPos = Just (pixX, pixY)
        , worldCursorTexture = Just (TextureHandle 1)
        }
    V.length <$> renderWorldCursorQuads env ws 1.0

-- | Same generous viewport 'Test.Headless.World.Render.PickSeam' uses,
--   and for the same reason: the pick must resolve rather than be culled.
previewZoom ∷ Float
previewZoom = 40.0

previewFbW, previewFbH, previewWinW, previewWinH ∷ Int
previewFbW = 800
previewFbH = 600
previewWinW = 8000
previewWinH = 6000

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
