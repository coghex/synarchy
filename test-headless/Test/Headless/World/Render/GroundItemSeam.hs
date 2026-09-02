{-# LANGUAGE Strict #-}
-- | Pure tests for 'World.Render.GroundItemQuads.itemGeometry' and the
--   shared wrap OFFSET at the U seam (issues #1135 and #1176).
--
--   Ground items store bare float @(x, y)@ and NOTHING normalises them
--   on the way in — @item.spawnGround@ takes arbitrary numbers and
--   stores them directly, with no requirement that the tile be loaded or
--   the coord canonical. So an item genuinely can come to rest at a
--   u-seam alias of a loaded chunk. Left raw, the chunk lookup missed
--   that loaded chunk and the item was BOTH invisible and unhittable —
--   this helper backs the render pass and 'hitTestGroundItemAt' alike.
--
--   The fix canonicalises the whole tile frame, not just the map key:
--   the coords also drive the screen position here and the sort key /
--   wrap offset in the render pass, so an aliased item must resolve to
--   exactly the geometry it would have had at its canonical coords.
--
--   #1176 then made the offset APPLIED after that lookup two-dimensional
--   and facing-aware, which is what the four-facing groups below pin: a
--   u-wrap displaces screen X at south/north but screen Y at west/east,
--   so the old X-only offset culled or half-world-displaced everything
--   across the seam at two of the four facings.
module Test.Headless.World.Render.GroundItemSeam (spec, engineSpec) where

import UPrelude
import Test.Hspec
import Data.IORef (writeIORef)
import qualified Data.HashMap.Strict as HM
import Engine.Core.Init (EngineInitResult(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Engine.Core.State
    (EngineEnv(..), itemManagerRef)
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..), defaultCamera)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..))
import Engine.Scene.Types (SortableQuad(..))
import Item.Ground (GroundItem(..), GroundItems(..))
import World.Generate (viewDepth)
import World.Grid (gridToWorld)
import World.Render.ChunkCulling (isChunkVisibleWrapped)
import World.Render.GroundItemQuads
    (hitTestGroundItemAt, itemGeometry, renderGroundItemQuads)
import World.Render.ViewBounds (computeViewBounds)
import World.State.Types (WorldState(..), emptyWorldState)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Engine.Asset.Handle (TextureHandle(..))
import Item.Types (ItemDef(..), ItemInstance(..), ItemManager(..))
import Structure.Types (emptyChunkStructures)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Tile.Types (WorldTileData(..))

-- | worldSize 64 chunks → canonical chunk u ∈ [-32, 32).
worldSize ∷ Int
worldSize = 64

zSlice ∷ Int
zSlice = 10

-- | Chunk (17,-15) has u = 32 — one past the canonical range — and is
--   STORED under ChunkCoord (-15) 17. The tile shift between the two
--   frames is a whole world: (-512, +512) in tiles.
stored ∷ ChunkCoord
stored = ChunkCoord (-15) 17

rawTile, canonTile ∷ (Int, Int)
rawTile   = (17 * chunkSize, (-15) * chunkSize)
canonTile = ((-15) * chunkSize, 17 * chunkSize)

solidChunk ∷ ChunkCoord → LoadedChunk
solidChunk coord =
    let area = chunkSize * chunkSize
        col  = ColumnTiles
                 { ctStartZ = 0
                 , ctMats   = VU.replicate 20 1
                 , ctSlopes = VU.replicate 20 0
                 , ctVeg    = VU.replicate 20 0
                 }
    in LoadedChunk
        { lcCoord = coord
        , lcTiles = V.replicate area col
        , lcSurfaceMap = VU.replicate area zSlice
        , lcTerrainSurfaceMap = VU.replicate area zSlice
        , lcFluidMap = V.replicate area Nothing
        , lcIceMap = emptyIceMap, lcFlora = emptyFloraChunkData
        , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
        , lcMagma = Nothing, lcStructures = emptyChunkStructures
        }

tilesAt ∷ ChunkCoord → WorldTileData
tilesAt coord = WorldTileData
    { wtdChunks = HM.fromList [(coord, solidChunk coord)]
    , wtdMaxChunks = 200
    }

bareItemDef ∷ ItemDef
bareItemDef = ItemDef
    { idName = "probe_rock", idDisplayName = "probe_rock"
    , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
    , idWeight = 1.0, idWeightSpec = Nothing, idBulk = 1.0
    , idStorage = Nothing, idKind = "misc"
    , idCategory = "Misc", idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = []
    , idContainer = Nothing, idDefaultContents = [], idFood = Nothing
    , idWeapon = Nothing, idArmor = Nothing, idUnequippable = False
    , idBuffs = [], idInsulation = 0
    , idSourcePath = "test-fixture"
    }

items ∷ ItemManager
items = ItemManager (HM.fromList [("probe_rock", bareItemDef)])

-- | An item resting at a deliberately off-centre point in its tile, so
--   the in-tile fraction is non-trivial and a frame shift that mangled
--   it would move the sprite.
itemAt ∷ Float → Float → GroundItem
itemAt x y = GroundItem
    { giInst = ItemInstance
        { iiDefName = "probe_rock", iiCurrentFill = 0
        , iiQuality = 100, iiCondition = 100, iiWeight = 1.0
        , iiSharpness = 100, iiContents = [], iiInstanceId = 1
        , iiTemp = Nothing, iiBulk = Just 1, iiStorage = Nothing
        }
    , giX = x, giY = y
    }

geometryAt ∷ CameraFacing → WorldTileData → GroundItem
           → Maybe (Int, TextureHandle, Float, Float, Float, Float, Int)
geometryAt facing tiles =
    itemGeometry tiles items HM.empty facing zSlice worldSize

geometry ∷ WorldTileData → GroundItem
         → Maybe (Int, TextureHandle, Float, Float, Float, Float, Int)
geometry = geometryAt FaceSouth

-- | Every camera facing, in the game's own rotation order.
allFacings ∷ [CameraFacing]
allFacings = [FaceSouth, FaceWest, FaceNorth, FaceEast]

zoom ∷ Float
zoom = 4.0

fbW, fbH ∷ Int
(fbW, fbH) = (800, 600)

effDepth ∷ Int
effDepth = min viewDepth (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))

-- | A camera parked exactly on the ALIAS of the fixture's tile at this
--   facing: the configuration that forces a non-zero wrap offset,
--   because the chunk is only reachable through its wrapped image.
cameraOnAlias ∷ CameraFacing → Camera2D
cameraOnAlias facing =
    let (rawX, rawY) = rawTile
        (aliasWX, aliasWY) = gridToWorld facing rawX rawY
    in defaultCamera { camPosition = (aliasWX, aliasWY), camZoom = zoom
                     , camFacing = facing, camZSlice = zSlice }

-- | The TRUE screen displacement of one u-wrap at this facing, taken
--   from the projection itself rather than from any constant this
--   fixture could restate wrongly. @gridToScreen@'s half-tile shift is
--   common to both terms and cancels, so 'gridToWorld' is the same
--   difference.
wrapDisplacement ∷ CameraFacing → (Float, Float)
wrapDisplacement facing =
    let (rawX, rawY) = rawTile
        (canonX, canonY) = canonTile
        (aliasWX, aliasWY) = gridToWorld facing rawX rawY
        (canonWX, canonWY) = gridToWorld facing canonX canonY
    in (aliasWX - canonWX, aliasWY - canonWY)

spec ∷ Spec
spec = do

  describe "an item at a U-seam alias (#1135)" $ do
    let tiles = tilesAt stored
        (rawX, rawY) = rawTile
        (canonX, canonY) = canonTile
        -- Same physical spot, named two ways: the alias coords a Lua
        -- item.spawnGround could store, and their canonical equivalent.
        aliased   = itemAt (fromIntegral rawX + 0.25)
                           (fromIntegral rawY + 0.75)
        canonical = itemAt (fromIntegral canonX + 0.25)
                           (fromIntegral canonY + 0.75)

    it "precondition: only the canonical chunk is a key in the map" $ do
      -- Pins that this fixture exercises the alias path — the chunk IS
      -- loaded, and only the wrapped key finds it.
      HM.member (ChunkCoord 17 (-15)) (wtdChunks tiles) `shouldBe` False
      HM.member stored (wtdChunks tiles) `shouldBe` True

    it "resolves its loaded chunk instead of vanishing" $
      -- Before the fix the raw lookup missed: no quad, and no hit —
      -- itemGeometry backs rendering AND hitTestGroundItemAt.
      geometry tiles aliased `shouldSatisfy` isJust

    it "produces exactly the geometry of the same spot named canonically" $
      -- The whole frame moves together, so the aliased item must land on
      -- the identical resting z and screen quad — not merely resolve.
      geometry tiles aliased `shouldBe` geometry tiles canonical

    it "is still Nothing when the chunk genuinely is not loaded" $
      -- The negative the raw lookup could not tell apart from an alias.
      geometry (WorldTileData HM.empty 200) aliased `shouldBe` Nothing

  describe "away from the seam the wrap is the identity" $ do
    -- Chunk (16,-15) has u = 31 — inside the canonical range, so it is
    -- its own key and nothing shifts (requirement 4).
    let interior = ChunkCoord 16 (-15)
        tiles = tilesAt interior
        gi = itemAt (fromIntegral (16 * chunkSize) + 0.25)
                    (fromIntegral ((-15) * chunkSize) + 0.75)

    it "resolves an interior item unchanged" $
      geometry tiles gi `shouldSatisfy` isJust

  -- #1176: the offset itself, at every facing. The pre-#1176 helper
  -- returned a screen-X shift only — exact at south/north, and unable
  -- to touch east/west, where the SAME u-wrap displaces screen Y
  -- instead. Content across the seam was culled by the bounds test or
  -- placed a half-world off in Y at those two facings.
  describe "the wrap offset is facing-aware (#1176)" $
    forM_ allFacings $ \facing → describe (show facing) $ do
      let (expX, expY) = wrapDisplacement facing
          cam = cameraOnAlias facing
          (camWX, camWY) = camPosition cam
          vb = computeViewBounds cam fbW fbH effDepth
          got = isChunkVisibleWrapped facing worldSize vb camWX camWY stored

      it "precondition: a u-wrap moves exactly one screen axis" $ do
        -- A u-wrap shifts u by a whole world and PRESERVES v, so one
        -- component is identically zero and the other is world-sized —
        -- not a rounding artefact. WHICH axis moves is the whole bug,
        -- so this is derived, never hardcoded to the 2:1 tile ratio.
        min (abs expX) (abs expY) `shouldBe` 0
        max (abs expX) (abs expY) `shouldSatisfy` (> 1.0)

      it "resolves the chunk through its nearest alias" $
        -- Requirement 2: visibility is judged against bounds translated
        -- by the returned offset, so an offset that cannot reach the
        -- camera culls the chunk outright. This is Nothing at
        -- west/east before the fix.
        got `shouldSatisfy` isJust

      it "returns that displacement on both axes" $
        case got of
          Nothing → expectationFailure "chunk culled at its own alias"
          Just (offX, offY) → do
            abs (offX - expX) `shouldSatisfy` (< 0.001)
            abs (offY - expY) `shouldSatisfy` (< 0.001)

  -- Requirement 4, at every facing: nothing moves away from the seam.
  describe "away from the seam the offset is (0, 0) at every facing" $
    forM_ allFacings $ \facing →
      it (show facing) $ do
        -- Chunk (16,-15) has u = 31, inside the canonical range, and
        -- the camera sits on its own tile — no alias is nearer.
        let interior = ChunkCoord 16 (-15)
            (iw, ih) = gridToWorld facing (16 * chunkSize) ((-15) * chunkSize)
            cam = defaultCamera { camPosition = (iw, ih), camZoom = zoom
                                , camFacing = facing, camZSlice = zSlice }
            vb = computeViewBounds cam fbW fbH effDepth
        isChunkVisibleWrapped facing worldSize vb iw ih interior
            `shouldBe` Just (0, 0)


-- | The other half of the round-2 review concern: an item drawn through
--   its WRAPPED image across the U seam has to be clickable where it is
--   DRAWN. The render pass paints at @drawX0 + xOff@, so a hit test that
--   compared clicks against the unwrapped geometry alone was a whole
--   world screen period away from every such click — visible, unhittable.
--
--   This drives the real 'hitTestGroundItemAt' (engine-backed: it reads
--   camera / viewport / item-manager state off EngineEnv) with the
--   camera parked on the item's ALIASED position, which is exactly the
--   case that forces a non-zero wrap offset. The offset is asserted
--   non-zero first, so the test cannot pass trivially by the scenario
--   collapsing to the interior one.
--
--   ALL FOUR FACINGS since #1176. The offset used to be screen-X only,
--   which is exact at south/north but cannot correct east/west, where a
--   u-wrap displaces screen Y instead. So the click centre here is taken
--   from the quad 'renderGroundItemQuads' actually EMITS, not from a
--   presumed draw position recomputed alongside it — a render pass that
--   dropped the Y component would otherwise still agree with the test's
--   own arithmetic and the failure would hide.
engineSpec ∷ Spec
engineSpec = beforeAll initEnv $
  describe "ground-item click across the U seam (#1135, #1176)" $
    forM_ allFacings $ \facing →
      it ("hits the item where its wrapped image is drawn at "
          <> show facing) $ \env → do
        ws ← emptyWorldState
        writeIORef (wsGenParamsRef ws)
            (Just defaultWorldGenParams { wgpWorldSize = worldSize })
        writeIORef (wsTilesRef ws) (tilesAt stored)
        writeIORef (itemManagerRef env) items

        -- The item sits at its CANONICAL coords, mid-tile.
        let (canonX, canonY) = canonTile
            gi = itemAt (fromIntegral canonX + 0.5) (fromIntegral canonY + 0.5)
            gid = 7
        writeIORef (wsGroundItemsRef ws)
            (GroundItems 8 (HM.fromList [(gid, gi)]))

        -- Park the camera on the ALIAS of that tile — a whole world away
        -- along u — so the item can only be on screen via its wrapped
        -- image, which is the configuration the bug hid in.
        let cam = cameraOnAlias facing
            (camWX, camWY) = camPosition cam
            (winW, winH) = (fbW, fbH)
        writeIORef (cameraRef env) cam
        writeIORef (windowSizeRef env) (winW, winH)
        writeIORef (framebufferSizeRef env) (fbW, fbH)

        -- The real render pass, through EngineEnv exactly as the frame
        -- loop drives it.
        quads ← renderGroundItemQuads env ws 1.0
        case V.toList quads of
          [] → expectationFailure
                 "the item emitted no quad — culled at its own alias"
          (q:_) → do
            -- Quad centre: midpoint of the two opposite corners the
            -- pass wrote (v0 = top-left, v2 = bottom-right).
            let Vec2 qx0 qy0 = pos (sqV0 q)
                Vec2 qx2 qy2 = pos (sqV2 q)
                cx = (qx0 + qx2) * 0.5
                cy = (qy0 + qy2) * 0.5

            -- Precondition, measured against the render output itself:
            -- the emitted quad really is a whole wrap away from the
            -- item's own unshifted geometry, on BOTH axes. This is what
            -- catches a render pass that applied only the X component.
            case geometryAt facing (tilesAt stored) gi of
              Nothing → expectationFailure "item geometry did not resolve"
              Just (_, _, drawX0, drawY0, quadW, quadH, _) → do
                let (expX, expY) = wrapDisplacement facing
                abs ((cx - (drawX0 + quadW * 0.5)) - expX)
                    `shouldSatisfy` (< 0.001)
                abs ((cy - (drawY0 + quadH * 0.5)) - expY)
                    `shouldSatisfy` (< 0.001)

            -- Click dead centre of the quad AS DRAWN, converted back to a
            -- window pixel with the hit test's own pixel→world mapping.
            let aspect = fromIntegral winW / fromIntegral winH ∷ Float
                vw = zoom * aspect
                vh = zoom
                pixX = fromIntegral winW
                     * (((cx - camWX) / vw) + 1.0) / 2.0
                pixY = fromIntegral winH
                     * (((cy - camWY) / vh) + 1.0) / 2.0
            hit ← hitTestGroundItemAt env ws (realToFrac pixX) (realToFrac pixY)
            hit `shouldBe` Just gid
  where
    initEnv = do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        pure env
