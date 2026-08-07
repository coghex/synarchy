{-# LANGUAGE Strict #-}
-- | Pure tests for 'World.Render.HitTest.pickWorldTile' at the U seam
--   (issue #1135).
--
--   The camera position is wrapped into the canonical range, but the
--   VIEWPORT around it is not: near the seam the far half of the screen
--   unprojects to tile coords outside that range, whose chunk is stored
--   under the wrapped alias. The hit-test used to hand that raw coord
--   straight to @HM.lookup@, miss a LOADED chunk, and walk every z down
--   to "no tile" — clicks on the far side of the seam picked nothing.
--
--   The fix resolves the STORED chunk and carries the reported tile
--   coords AND the fractional hover position into that same frame, so a
--   seam hit can be handed straight to a designation / build / pick
--   command. Reporting the raw alias instead would be its own bug: those
--   commands only speak the canonical frame.
--
--   No engine needed: 'pickWorldTile' is pure. One hand-built chunk is
--   keyed ONLY at its canonical coord, and the camera is parked on the
--   ALIASED world position of that chunk's own tile — so the unprojected
--   coord is genuinely non-canonical, which the precondition below pins.
module Test.Headless.World.Render.HitTest (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Engine.Graphics.Camera (CameraFacing(..))
import Structure.Types (emptyChunkStructures)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Tile.Types (WorldTileData(..))
import World.Grid (gridToWorld, tileHeight)
import World.Render.HitTest (pickWorldTile)
import World.Render.ViewBounds (ViewBounds(..))

-- | worldSize 64 chunks → canonical chunk u ∈ [-32, 32), and the world
--   is 64 * 16 = 1024 tiles around the cylinder.
worldSize ∷ Int
worldSize = 64

-- | The z the camera slices at, and the solid column's extent.
zSlice ∷ Int
zSlice = 10

-- | A chunk whose every column is solid over z 0..19, so the pick lands
--   on the first z it tries.
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

-- | Bounds that accept every chunk, so visibility never trims the hit.
allVisible ∷ ViewBounds
allVisible = ViewBounds (-1.0e9) 1.0e9 (-1.0e9) 1.0e9

-- | Park the camera dead-centre on the tile @(gx, gy)@ and pick the
--   centre pixel. At the screen centre the view offset is zero, so
--   @world{X,Y} = camPosition@; the extra half tile-height cancels the
--   hit-test's own @- tileHeight * 0.5@ elevation adjustment, so the
--   centre pixel unprojects to exactly this tile.
pickAt ∷ WorldTileData → Int → Int → Maybe (Int, Int, Int, Float, (Float, Float))
pickAt tiles gx gy =
    let (wx, wy) = gridToWorld FaceSouth gx gy
        camX = wx
        camY = wy + tileHeight * 0.5
    in pickWorldTile FaceSouth 2.0 zSlice camX camY 800 600 800 600
           worldSize 64 allVisible tiles 400 300

spec ∷ Spec
spec = do

  describe "picks in the canonical frame away from the seam" $ do
    -- Chunk (16,-15) has u = 31 — inside [-32, 32), so it is its own
    -- canonical key and the wrap is the identity (requirement 4).
    let interior = ChunkCoord 16 (-15)
        (igx, igy) = (16 * chunkSize, (-15) * chunkSize)

    it "reports the unprojected tile unchanged" $
      case pickAt (tilesAt interior) igx igy of
        Nothing → expectationFailure "interior centre pixel missed its own tile"
        Just (gx, gy, z, _, _) → (gx, gy, z) `shouldBe` (igx, igy, zSlice)

  describe "picks across the U seam (#1135)" $ do
    -- Chunk (17,-15) has u = 32 — one past the canonical range. It is
    -- STORED as ChunkCoord (-15) 17; the tile shift between the two
    -- frames is a whole world, (-512, +512) in tiles.
    let stored = ChunkCoord (-15) 17
        (rawGX, rawGY) = (17 * chunkSize, (-15) * chunkSize)
        (canonGX, canonGY) = ((-15) * chunkSize, 17 * chunkSize)
        tiles = tilesAt stored

    it "precondition: the raw coord's chunk is NOT a key in the map" $ do
      -- Pins that this fixture really exercises the alias path — the
      -- chunk is loaded, and only the wrapped key finds it.
      HM.member (ChunkCoord 17 (-15)) (wtdChunks tiles) `shouldBe` False
      HM.member stored (wtdChunks tiles) `shouldBe` True

    it "resolves the loaded chunk instead of reporting no tile" $
      -- Before the fix the raw lookup missed and tryZ walked to Nothing.
      pickAt tiles rawGX rawGY `shouldSatisfy` isJust

    it "reports the tile in the CANONICAL stored frame" $
      case pickAt tiles rawGX rawGY of
        Nothing → expectationFailure "seam pick found no tile"
        Just (gx, gy, z, _, _) →
          (gx, gy, z) `shouldBe` (canonGX, canonGY, zSlice)

    it "shifts the fractional hover position into that same frame" $
      -- hoverPos must travel with the integer coords: left unshifted it
      -- would floor to the raw alias (272, -240) and downstream
      -- item/unit placement would read a tile the session does not hold.
      case pickAt tiles rawGX rawGY of
        Nothing → expectationFailure "seam pick found no tile"
        Just (_, _, _, _, (hx, hy)) →
          (floor hx ∷ Int, floor hy ∷ Int) `shouldBe` (canonGX, canonGY)

    it "keeps the wrap offset pointing at the on-screen alias" $
      -- The offset is taken against the canonical chunk, so it must map
      -- that chunk back onto the aliased position the camera is parked
      -- at — a non-zero shift, unlike the interior case.
      case (pickAt tiles rawGX rawGY, pickAt (tilesAt (ChunkCoord 16 (-15)))
                                             (16 * chunkSize) ((-15) * chunkSize)) of
        (Just (_, _, _, seamOff, _), Just (_, _, _, interiorOff, _)) → do
          interiorOff `shouldBe` 0.0
          seamOff `shouldNotBe` 0.0
        _ → expectationFailure "expected both picks to resolve"
