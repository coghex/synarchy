-- | The camera z-tracking lookup in 'World.Render.updateWorldTiles'
--   resolves its chunk across the U seam (issue #1135).
--
--   That lookup converts the CAMERA position to a tile and reads the
--   surface there to retarget @camZSlice@. The camera is wrapped into
--   the canonical range on the pan path, but only up to rounding: right
--   on the seam boundary the rounded tile can land one past the range,
--   whose chunk is stored under the wrapped alias. Left raw the lookup
--   missed a LOADED chunk and z-tracking silently stalled for that
--   frame.
--
--   It is the one audited site where canonicalising the key is the whole
--   fix: the result is read at a local index and no tile coord travels
--   on from it, so there is nothing to shift into the stored frame.
--
--   Driven through the real 'updateWorldTiles' rather than a
--   reimplementation. Zoom is parked above 'zoomFadeEnd' so @tileAlpha@
--   is zero and every quad pass short-circuits to empty — but
--   @camZTracking@ is set, which is what keeps the tracking block itself
--   running.
module Test.Headless.World.Render.ZTrackSeam (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef, writeIORef)
import Engine.Core.Init (EngineInitResult(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Engine.Core.State (EngineEnv(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..), defaultCamera)
import Structure.Types (emptyChunkStructures)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Grid (gridToWorld)
import World.Page.Types (WorldPageId(..))
import World.Render (updateWorldTiles, surfaceHeadroom)
import World.State.Types
    ( WorldState(..), WorldManager(..), emptyWorldState, emptyWorldManager )
import World.Tile.Types (WorldTileData(..))

pid ∷ WorldPageId
pid = WorldPageId "ztrack_seam_test"

-- | worldSize 64 chunks → canonical chunk u ∈ [-32, 32). Chunk (17,-15)
--   has u = 32 — one past that range — and is STORED as (-15, 17).
worldSize ∷ Int
worldSize = 64

stored ∷ ChunkCoord
stored = ChunkCoord (-15) 17

-- | A tile in that chunk, named in both frames.
rawTile, canonTile ∷ (Int, Int)
rawTile   = (17 * chunkSize + 5, (-15) * chunkSize + 6)
canonTile = ((-15) * chunkSize + 5, 17 * chunkSize + 6)

-- | Distinctive surface height, so the resulting z-slice can only have
--   come from THIS chunk's surface map.
surfZ ∷ Int
surfZ = 42

solidChunk ∷ LoadedChunk
solidChunk =
    let area = chunkSize * chunkSize
        col  = ColumnTiles
                 { ctStartZ = 0
                 , ctMats   = VU.replicate 60 1
                 , ctSlopes = VU.replicate 60 0
                 , ctVeg    = VU.replicate 60 0
                 }
    in LoadedChunk
        { lcCoord = stored
        , lcTiles = V.replicate area col
        , lcSurfaceMap = VU.replicate area surfZ
        , lcTerrainSurfaceMap = VU.replicate area surfZ
        , lcFluidMap = V.replicate area Nothing
        , lcIceMap = emptyIceMap, lcFlora = emptyFloraChunkData
        , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
        , lcMagma = Nothing, lcStructures = emptyChunkStructures
        }

-- | A page registered as the sole visible world, holding the given
--   chunks. Zoom sits above zoomFadeEnd (1.6) so tileAlpha is 0 and no
--   quad pass does real work; camZTracking keeps the tracked lookup live.
setUp ∷ EngineEnv → WorldTileData → (Int, Int) → IO ()
setUp env tiles (camGX, camGY) = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws)
        (Just defaultWorldGenParams { wgpWorldSize = worldSize })
    writeIORef (wsTilesRef ws) tiles
    writeIORef (worldManagerRef env) (emptyWorldManager
        { wmWorlds = [(pid, ws)]
        , wmVisible = [pid] })
    let (camX, camY) = gridToWorld FaceSouth camGX camGY
    writeIORef (cameraRef env) defaultCamera
        { camPosition = (camX, camY)
        , camFacing = FaceSouth
        , camZoom = 4.0
        , camZTracking = True
        , camZSlice = 0
        }
    writeIORef (windowSizeRef env) (800, 600)
    writeIORef (framebufferSizeRef env) (800, 600)

loadedTiles ∷ WorldTileData
loadedTiles = WorldTileData (HM.fromList [(stored, solidChunk)]) 200

spec ∷ Spec
spec = beforeAll initEnv $
  describe "camera z-tracking across the U seam (#1135)" $ do

    it "precondition: only the canonical chunk is a key in the map" $ \_env → do
      HM.member (ChunkCoord 17 (-15)) (wtdChunks loadedTiles) `shouldBe` False
      HM.member stored (wtdChunks loadedTiles) `shouldBe` True

    it "retargets the z-slice from a camera parked on the ALIAS tile" $ \env → do
      -- The camera position rounds to the raw alias coord, whose chunk is
      -- stored under the wrapped key. Before the fix this missed and
      -- camZSlice was left untouched.
      setUp env loadedTiles rawTile
      _ ← updateWorldTiles env
      cam ← readIORef (cameraRef env)
      camZSlice cam `shouldBe` surfZ + surfaceHeadroom

    it "matches the canonical naming of the same tile exactly" $ \env → do
      setUp env loadedTiles canonTile
      _ ← updateWorldTiles env
      cam ← readIORef (cameraRef env)
      camZSlice cam `shouldBe` surfZ + surfaceHeadroom

    it "leaves the z-slice alone when the chunk genuinely is not loaded" $ \env → do
      -- The negative the raw lookup could not tell apart from an alias
      -- miss: nothing loaded, so tracking has nothing to retarget to.
      setUp env (WorldTileData HM.empty 200) rawTile
      _ ← updateWorldTiles env
      cam ← readIORef (cameraRef env)
      camZSlice cam `shouldBe` 0
  where
    initEnv = do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        pure env
