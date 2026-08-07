{-# LANGUAGE Strict #-}
-- | Pure tests for 'World.Render.GroundItemQuads.itemGeometry' at the U
--   seam (issue #1135).
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
module Test.Headless.World.Render.GroundItemSeam (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Item.Ground (GroundItem(..))
import Item.Types (ItemDef(..), ItemInstance(..), ItemManager(..))
import Structure.Types (emptyChunkStructures)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Render.GroundItemQuads (itemGeometry)
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
    , idTexture = TextureHandle 0
    , idWeight = 1.0, idWeightSpec = Nothing, idKind = "misc"
    , idCategory = "Misc", idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = [], idConditionSpec = Nothing
    , idContainer = Nothing, idDefaultContents = [], idFood = Nothing
    , idWeapon = Nothing, idArmor = Nothing, idUnequippable = False
    , idBuffs = [], idInsulation = 0
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
        , iiTemp = Nothing
        }
    , giX = x, giY = y
    }

geometry ∷ WorldTileData → GroundItem
         → Maybe (Int, TextureHandle, Float, Float, Float, Float, Int)
geometry tiles = itemGeometry tiles items HM.empty FaceSouth zSlice worldSize

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
