-- | Designation coords are normalised into the canonical (u-wrapped)
--   frame at the engine's entry points (issue #1135).
--
--   Designation coords come in from Lua — @world.setMineAnchor@,
--   @world.designateMine@ and their chop/till/construct/plant siblings
--   all round arbitrary numbers — so nothing guarantees they are already
--   in the frame chunks are stored under. Left raw, an anchor could sit
--   in a u-seam ALIAS while the hover tile beside it (from
--   'pickWorldTile', canonical since #1135) sat in the stored frame: the
--   two are a whole world apart, so a one-tile designation became a
--   capped 128-wide sweep, and the commit missed its own loaded chunk
--   while the preview showed the real tiles.
--
--   Normalising at the entry point is what keeps the anchor, the live
--   preview ('World.Render.CursorQuads') and the commit all naming the
--   same tile. These drive the real command handlers.
module Test.Headless.World.DesignationSeam (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef, writeIORef)
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Structure.Types (emptyChunkStructures)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Cursor.Types (CursorState(..))
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldState(..), WorldManager(..), emptyWorldState)
import World.Thread.Command.Cursor.Mine
    (handleWorldSetMineAnchorCommand, handleWorldDesignateMineCommand)
import World.Tile.Types (WorldTileData(..))

pid ∷ WorldPageId
pid = WorldPageId "designation_seam_test"

-- | worldSize 64 chunks → canonical chunk u ∈ [-32, 32). Chunk (17,-15)
--   has u = 32 — one past that range — and is STORED as (-15, 17); the
--   tile shift between the two frames is a whole world.
worldSize, zSlice ∷ Int
worldSize = 64
zSlice = 10

stored ∷ ChunkCoord
stored = ChunkCoord (-15) 17

rawTile, canonTile ∷ (Int, Int)
rawTile   = (17 * chunkSize + 3, (-15) * chunkSize + 4)
canonTile = ((-15) * chunkSize + 3, 17 * chunkSize + 4)

solidChunk ∷ LoadedChunk
solidChunk =
    let area = chunkSize * chunkSize
        col  = ColumnTiles
                 { ctStartZ = 0
                 , ctMats   = VU.replicate 20 1
                 , ctSlopes = VU.replicate 20 0
                 , ctVeg    = VU.replicate 20 0
                 }
    in LoadedChunk
        { lcCoord = stored
        , lcTiles = V.replicate area col
        , lcSurfaceMap = VU.replicate area zSlice
        , lcTerrainSurfaceMap = VU.replicate area zSlice
        , lcFluidMap = V.replicate area Nothing
        , lcIceMap = emptyIceMap, lcFlora = emptyFloraChunkData
        , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
        , lcMagma = Nothing, lcStructures = emptyChunkStructures
        }

-- | A page holding exactly one chunk, keyed canonically — so only a
--   canonicalised coord can reach it.
freshPage ∷ EngineEnv → IO WorldState
freshPage env = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws)
        (Just defaultWorldGenParams { wgpWorldSize = worldSize })
    writeIORef (wsTilesRef ws)
        (WorldTileData (HM.fromList [(stored, solidChunk)]) 200)
    writeIORef (worldManagerRef env) (WorldManager [(pid, ws)] [pid])
    pure ws

spec ∷ Spec
spec = beforeAll initEnv $
  describe "designation coords across the U seam (#1135)" $ do

    it "stores an anchor given in alias coords in the CANONICAL frame" $ \env → do
        ws ← freshPage env
        logger ← readIORef (loggerRef env)
        let (rawX, rawY) = rawTile
        handleWorldSetMineAnchorCommand env logger pid rawX rawY
        cs ← readIORef (wsCursorRef ws)
        -- The preview reads this anchor alongside a canonical hover
        -- tile; left in the alias frame the pair would be a whole world
        -- apart and the rectangle between them would blow up to the cap.
        mineAnchor cs `shouldBe` Just canonTile

    it "leaves an already-canonical anchor untouched" $ \env → do
        ws ← freshPage env
        logger ← readIORef (loggerRef env)
        let (canonX, canonY) = canonTile
        handleWorldSetMineAnchorCommand env logger pid canonX canonY
        cs ← readIORef (wsCursorRef ws)
        mineAnchor cs `shouldBe` Just canonTile

    it "commits a designation given in alias coords onto the real tile" $ \env → do
        ws ← freshPage env
        logger ← readIORef (loggerRef env)
        let (rawX, rawY) = rawTile
        -- A one-tile rectangle named entirely in the alias frame. The
        -- commit takes its corners straight from Lua, not from the
        -- stored anchor, so it has to normalise them itself — otherwise
        -- surfaceZAt misses the loaded chunk and nothing is designated.
        handleWorldDesignateMineCommand env logger pid rawX rawY rawX rawY
        designs ← readIORef (wsMineDesignationsRef ws)
        HM.keys designs `shouldBe` [canonTile]

    it "commits identically when the same tile is named canonically" $ \env → do
        ws ← freshPage env
        logger ← readIORef (loggerRef env)
        let (canonX, canonY) = canonTile
        handleWorldDesignateMineCommand env logger pid canonX canonY canonX canonY
        designs ← readIORef (wsMineDesignationsRef ws)
        -- Same key AND same stored designation as the alias commit
        -- above: preview and designation cannot disagree about which
        -- tile was marked, whichever frame the caller used.
        HM.keys designs `shouldBe` [canonTile]

    it "designates nothing for a tile whose chunk really is unloaded" $ \env → do
        ws ← freshPage env
        logger ← readIORef (loggerRef env)
        -- Far from the one loaded chunk in BOTH frames — the negative a
        -- raw lookup could not tell apart from an alias miss.
        handleWorldDesignateMineCommand env logger pid 5000 5000 5000 5000
        designs ← readIORef (wsMineDesignationsRef ws)
        HM.null designs `shouldBe` True
  where
    initEnv = do
        EngineInitResult env ← initializeEngineHeadless
        pure env
