{-# LANGUAGE Strict #-}
-- | Pure tests for 'World.Render.HitTest.pickWorldTile' at the U seam
--   (issue #1175).
--
--   The camera is wrapped into the canonical chunk range but the
--   viewport around it is not, so near the seam the far half of the
--   screen unprojects to a coord whose chunk is STORED under the wrapped
--   alias. Left raw, the lookup missed and @tryZ@ walked all the way
--   down to "no tile": clicks on the far side of the seam picked
--   nothing.
--
--   What the fix has to get right is not just "resolves the chunk" but
--   the frame the RESULT is reported in — this function is the head of
--   the designation coordinate contract (see the module haddock on
--   'World.Render.HitTest'). So each case below checks the integer tile
--   AND the fractional hover position, which must take the identical
--   whole-tile shift or a caller receives a tile and a sub-tile position
--   naming different places.
--
--   All four camera facings are covered. The wrap OFFSET the pick also
--   returns is screen-X only and cannot be right at east/west (#1176,
--   recorded on 'World.Render.ChunkCulling.bestWrapOffset'); these
--   examples therefore assert the coordinate frame, which #1175 owns,
--   and not that offset. The fixture keeps the resolved chunk inside the
--   view bounds at every facing so the visibility gate is never the
--   thing under test.
module Test.Headless.World.Render.PickSeam (spec) where

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
import World.Generate.Coordinates (canonicalTile)
import World.Grid (gridToWorld, tileHeight, worldToGridF)
import World.Render.HitTest (pickWorldTile)
import World.Render.ViewBounds (ViewBounds(..), computeViewBounds)
import World.Tile.Types (WorldTileData(..))
import Engine.Graphics.Camera (Camera2D(..), defaultCamera)

-- | worldSize 64 chunks → canonical chunk u ∈ [-32, 32).
worldSize ∷ Int
worldSize = 64

zSlice ∷ Int
zSlice = 10

-- | Chunk (17,-15) has u = 32 — one past the canonical range — and is
--   STORED under ChunkCoord (-15) 17: the same physical chunk, a whole
--   world away along u. The tile shift between the two frames is
--   (-512, +512).
storedChunk ∷ ChunkCoord
storedChunk = ChunkCoord (-15) 17

-- | A tile in the middle of that chunk, named both ways.
aliasTile, canonTileCoord ∷ (Int, Int)
aliasTile      = (17 * chunkSize + 8, (-15) * chunkSize + 8)
canonTileCoord = canonicalTile worldSize (fst aliasTile) (snd aliasTile)

-- | A tile in an INTERIOR chunk (u = 31, inside the canonical range), so
--   the wrap is the identity and the pick must behave exactly as before.
interiorChunk ∷ ChunkCoord
interiorChunk = ChunkCoord 16 (-15)

interiorTile ∷ (Int, Int)
interiorTile = (16 * chunkSize + 8, (-15) * chunkSize + 8)

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

-- | A deliberately generous viewport. @zoom@ has to exceed the half-world
--   screen-Y displacement a u-wrap produces at east/west (38.4 for this
--   world) or the seam case would be culled there before the coordinate
--   frame — the thing under test — was ever exercised. The WINDOW is
--   sized well above the framebuffer purely for pick precision: the
--   pixel→world step must stay far below a half tile so rounding the
--   target pixel to an integer cannot land on a neighbour.
zoomOut ∷ Float
zoomOut = 40.0

fbW, fbH, winW, winH, effDepth ∷ Int
fbW = 800
fbH = 600
winW = 8000
winH = 6000
effDepth = 250

-- | Aim a pixel at the exact point that unprojects to @(gx, gy)@ at the
--   active z-slice: 'gridToWorld' is the inverse of 'worldToGrid', and
--   'pickWorldTile' offsets screen Y by half a tile at @relZ == 0@.
pixelFor ∷ CameraFacing → (Float, Float) → (Int, Int) → (Int, Int)
pixelFor facing (camX, camY) (gx, gy) =
    let (wx, wy0) = gridToWorld facing gx gy
        wy     = wy0 + tileHeight * 0.5
        aspect = fromIntegral fbW / fromIntegral fbH ∷ Float
        vw     = zoomOut * aspect
        vh     = zoomOut
        px = fromIntegral winW * (((wx - camX) / vw) + 1.0) / 2.0
        py = fromIntegral winH * (((wy - camY) / vh) + 1.0) / 2.0
    in (round px, round py)

-- | Run the real pick with the camera parked ON the given tile — the
--   configuration that puts the seam alias under the cursor.
pickAt ∷ CameraFacing → WorldTileData → (Int, Int)
       → Maybe (Int, Int, Int, Float, (Float, Float))
pickAt facing tiles cameraTile =
    let (camX, camY) = gridToWorld facing (fst cameraTile) (snd cameraTile)
        cam = defaultCamera { camPosition = (camX, camY)
                            , camZoom = zoomOut
                            , camFacing = facing
                            , camZSlice = zSlice }
        vb = computeViewBounds cam fbW fbH effDepth
        (px, py) = pixelFor facing (camX, camY) cameraTile
    in pickWorldTile facing zoomOut zSlice camX camY fbW fbH winW winH
                     worldSize effDepth vb tiles px py

-- | The fractional position the pick would report with NO frame shift
--   applied: the same unprojection, run against the same pixel and
--   camera. Expressing the expectation this way (rather than flooring
--   the reported position) is what makes it a statement about the SHIFT
--   — the sub-tile fraction itself is whatever the aimed pixel lands on,
--   and differs per facing.
rawHoverAt ∷ CameraFacing → (Int, Int) → (Float, Float)
rawHoverAt facing cameraTile =
    let (camX, camY) = gridToWorld facing (fst cameraTile) (snd cameraTile)
        (px, py) = pixelFor facing (camX, camY) cameraTile
        aspect = fromIntegral fbW / fromIntegral fbH ∷ Float
        normX  = fromIntegral px / fromIntegral winW
        normY  = fromIntegral py / fromIntegral winH
        worldX = (normX * 2.0 - 1.0) * (zoomOut * aspect) + camX
        worldY = (normY * 2.0 - 1.0) * zoomOut + camY
    in worldToGridF facing worldX worldY

-- | Comparing fractional grid coords: well below a tile, well above the
--   float noise either side of the whole-tile addition.
nearTile ∷ (Float, Float) → (Float, Float) → Bool
nearTile (ax, ay) (bx, by) = abs (ax - bx) < 1.0e-3 ∧ abs (ay - by) < 1.0e-3

facings ∷ [(String, CameraFacing)]
facings = [ ("FaceSouth", FaceSouth), ("FaceNorth", FaceNorth)
          , ("FaceWest",  FaceWest),  ("FaceEast",  FaceEast) ]

spec ∷ Spec
spec = do

  describe "the fixture really exercises the alias path" $ do
    it "stores the seam chunk only under its canonical key" $ do
      let tiles = tilesAt storedChunk
      HM.member (ChunkCoord 17 (-15)) (wtdChunks tiles) `shouldBe` False
      HM.member storedChunk (wtdChunks tiles) `shouldBe` True

    it "puts the two names for that tile a whole world apart" $
      -- If this collapsed, every seam example below would pass trivially.
      canonTileCoord `shouldBe` ( fst aliasTile - 512, snd aliasTile + 512 )

  forM_ facings $ \(label, facing) → describe ("at " ⧺ label) $ do

    it "resolves a seam-alias pick to the CANONICAL tile" $
      -- Before the fix this whole case was Nothing: the raw lookup
      -- missed the loaded chunk and tryZ walked down to "no tile".
      case pickAt facing (tilesAt storedChunk) aliasTile of
        Nothing → expectationFailure "seam pick resolved no tile"
        Just (gx, gy, z, _, _) → do
          (gx, gy) `shouldBe` canonTileCoord
          z `shouldBe` zSlice

    it "shifts the fractional hover position by the SAME whole tile" $
      -- A shift applied to the integer tile but not the sub-tile
      -- position would hand callers two different places.
      case pickAt facing (tilesAt storedChunk) aliasTile of
        Nothing → expectationFailure "seam pick resolved no tile"
        Just (_, _, _, _, hover) → do
          let (rawHX, rawHY) = rawHoverAt facing aliasTile
              (dgx, dgy) = ( fst canonTileCoord - fst aliasTile
                           , snd canonTileCoord - snd aliasTile )
              expected = ( rawHX + fromIntegral dgx
                         , rawHY + fromIntegral dgy )
          hover `shouldSatisfy` nearTile expected

    it "is the identity away from the seam" $
      -- An interior chunk is its own storage key, so nothing may move —
      -- integer tile and fractional position alike.
      case pickAt facing (tilesAt interiorChunk) interiorTile of
        Nothing → expectationFailure "interior pick resolved no tile"
        Just (gx, gy, _, _, hover) → do
          (gx, gy) `shouldBe` interiorTile
          hover `shouldSatisfy` nearTile (rawHoverAt facing interiorTile)

    it "still reports no tile when the chunk genuinely is not loaded" $
      -- The negative the raw lookup could not tell an alias apart from.
      pickAt facing (WorldTileData HM.empty 200) aliasTile
        `shouldBe` Nothing

  describe "a degenerate viewport is still rejected" $
    it "reports no tile when the window has collapsed" $
      -- The canonicalisation runs inside tryZ, well past this guard; a
      -- refactor that hoisted it above the guard would unproject through
      -- a division by zero before ever reaching the wrap.
      pickWorldTile FaceSouth zoomOut zSlice 0 0 fbW fbH 0 0
                    worldSize effDepth (ViewBounds 0 0 0 0)
                    (tilesAt storedChunk) 0 0
        `shouldBe` Nothing
