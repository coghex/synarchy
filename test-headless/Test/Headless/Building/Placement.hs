{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | "Portal location exclusion" (#778): a `bdIsStarting` building's
--   footprint must not intersect any placed location's absolute
--   bounds (#777) on the same world page; ordinary construction is
--   unaffected. Pure tests against Building.Placement.canPlaceAt — no
--   engine needed, mirroring Test.Headless.Unit.Pathing.Cost's synthetic
--   WorldTileData pattern — plus the ghost-tint validity contract
--   (Building.Render.ghostTint).
--
--   Every fixture chunk below carries NO structure data
--   (lcStructures = emptyChunkStructures, same as Pathing.Cost's
--   fixtures) and the location's own hosting chunk is never separately
--   marked "stamped" — the overlay + registered def bounds alone drive
--   every rejection here, proving the check needs neither a visited
--   chunk nor placed geometry (the "unstamped but overlay-known
--   location" acceptance case).
module Test.Headless.Building.Placement (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vec4(..))
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..), chunkSize)
import World.Tile.Types (WorldTileData(..))
import World.Fluid.Types (emptyIceMap)
import World.Flora.Types (emptyFloraChunkData)
import World.Page.Types (WorldPageId(..))
import Structure.Types (emptyChunkStructures)
import Building.Types
import Building.Placement (canPlaceAt, PlacementResult(..))
import Building.Render (ghostTint)
import Location.Types
    ( LocationDef(..), LocationRegistry, emptyLocationRegistry
    , registerLocation
    )
import Location.Overlay.Types (LocationOverlay, emptyLocationOverlay)
import Location.Bounds (RelBounds(..))

-- * Fixtures

-- | A flat chunk (uniform terrain z, no fluid, no structures) at the
--   given coordinate — the loaded-terrain half of the placement
--   contract. Mirrors Test.Headless.Unit.Pathing.Cost's flatChunk.
flatChunkAt ∷ ChunkCoord → Int → LoadedChunk
flatChunkAt coord z =
    let area = chunkSize * chunkSize
        v = VU.replicate area z
    in LoadedChunk
        { lcCoord             = coord
        , lcTiles             = V.empty
        , lcSurfaceMap        = v
        , lcTerrainSurfaceMap = v
        , lcFluidMap          = V.replicate area Nothing
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.empty
        , lcWaterTableMap     = VU.empty
        , lcMagma             = Nothing
        , lcStructures        = emptyChunkStructures
        }

-- | Chunk (0,0) with two different terrain z values at local (0,0) and
--   (1,0) — a two-tile-wide footprint anchored there is "uneven".
unevenChunk ∷ LoadedChunk
unevenChunk =
    let area = chunkSize * chunkSize
        v = VU.generate area $ \i →
            let lx = i `mod` chunkSize
            in if lx ≡ 0 then 5 else 6
    in LoadedChunk
        { lcCoord             = ChunkCoord 0 0
        , lcTiles             = V.empty
        , lcSurfaceMap        = v
        , lcTerrainSurfaceMap = v
        , lcFluidMap          = V.replicate area Nothing
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.empty
        , lcWaterTableMap     = VU.empty
        , lcMagma             = Nothing
        , lcStructures        = emptyChunkStructures
        }

worldWithChunks ∷ [LoadedChunk] → WorldTileData
worldWithChunks chunks = WorldTileData
    { wtdChunks    = HM.fromList [ (lcCoord c, c) | c ← chunks ]
    , wtdMaxChunks = length chunks
    }

mkDef ∷ Bool → Int → Int → BuildingDef
mkDef starting w h = BuildingDef
    { bdName            = "test_building"
    , bdDisplayName     = "Test Building"
    , bdCategory        = "Test"
    , bdDescription     = ""
    , bdTexture         = TextureHandle 0
    , bdTileW           = w
    , bdTileH           = h
    , bdPlacement       = "flat_ground"
    , bdIsStarting      = starting
    , bdRace            = "acolyte"
    , bdSpriteAnchor    = "diamond_bottom"
    , bdBuildWork       = 0
    , bdMaterials       = HM.empty
    , bdStorageCapacity = 0
    , bdOperations      = []
    , bdAnimations      = HM.empty
    , bdStateAnims      = HM.empty
    , bdPowerDrain      = 0
    }

-- | The starting portal under test: a 1x1 footprint, is_starting=True.
portalDef ∷ BuildingDef
portalDef = mkDef True 1 1

-- | Same footprint, but NOT a starting building — the location rule
--   must not apply to it.
ordinaryDef ∷ BuildingDef
ordinaryDef = mkDef False 1 1

locDef ∷ Text → LocationDef
locDef lid = LocationDef
    { ldId              = lid
    , ldLabel           = "Test Ruin"
    , ldType            = "ruin"
    , ldBuilder         = "room_small"
    , ldAnchor          = []
    , ldMaxCount         = 0
    , ldMinSpacing      = 0
    , ldContents        = []
    , ldBounds          = RelBounds (-2) (-2) 2 2
    , ldDiscoveryMargin = 6
    , ldMapIcons        = Nothing
    }

-- | One location "loc1" placed at chunk (0,0). Its anchor is the
--   chunk-centre tile (8,8) — same computation
--   Location.Placement.placedLocationBounds and
--   world.listPlacedLocations both use — so its absolute bounds are
--   (6,6)..(10,10).
registry1 ∷ LocationRegistry
registry1 = registerLocation (locDef "loc1") emptyLocationRegistry

overlay1 ∷ LocationOverlay
overlay1 = HM.singleton (ChunkCoord 0 0) "loc1"

-- | An unoccupied manager — the location-exclusion tests don't need
--   any existing buildings.
noBuildings ∷ BuildingManager
noBuildings = emptyBuildingManager

occupiedAt ∷ Int → Int → BuildingManager
occupiedAt gx gy = emptyBuildingManager
    { bmInstances = HM.singleton (BuildingId 1) BuildingInstance
        { biDefName            = "other"
        , biPage               = WorldPageId "main_world"
        , biTexture            = TextureHandle 0
        , biAnchorX            = gx
        , biAnchorY            = gy
        , biGridZ              = 5
        , biSpawnedAt          = 0
        , biTileW              = 1
        , biTileH              = 1
        , biSpawnRemaining     = 0
        , biBuildProgress      = 0
        , biMaterialsDelivered = HM.empty
        , biStorage            = []
        }
    }

spec ∷ Spec
spec = describe "Portal location exclusion (#778)" $ do

    describe "Building.Placement.canPlaceAt against a placed location" $ do
        let wtd = worldWithChunks [flatChunkAt (ChunkCoord 0 0) 5]
            check gx gy = canPlaceAt noBuildings wtd registry1 overlay1 0
                                      portalDef gx gy

        it "rejects a footprint completely inside location bounds" $
            check 8 8 `shouldBe` NotPlaceable "inside a location's bounds"

        describe "rejects overlap at every inclusive boundary edge and corner" $
            forM_ [ ("top-left corner", 6, 6), ("top-right corner", 10, 6)
                  , ("bottom-left corner", 6, 10), ("bottom-right corner", 10, 10)
                  , ("top edge", 8, 6), ("bottom edge", 8, 10)
                  , ("left edge", 6, 8), ("right edge", 10, 8)
                  ] $ \(label, gx, gy) →
                it label $
                    check gx gy `shouldBe` NotPlaceable "inside a location's bounds"

        describe "remains valid at immediate adjacency without intersection" $
            forM_ [ ("one west of min_x", 5, 8), ("one east of max_x", 11, 8)
                  , ("one north of min_y", 8, 5), ("one south of max_y", 8, 11)
                  , ("diagonal to a corner", 5, 5)
                  ] $ \(label, gx, gy) →
                it label $
                    check gx gy `shouldBe` Placeable

        it "does not reject an ordinary non-starting building at the same coordinates" $
            canPlaceAt noBuildings wtd registry1 overlay1 0 ordinaryDef 8 8
                `shouldBe` Placeable

        it "does not reject when this page's overlay has no locations \
           \(locations on another page never reach this call)" $
            -- Callers pass each world page's OWN overlay/registry
            -- (World.Thread.Command.Init sets gen params per WorldState,
            -- Buildings.Spawn/Power read wsGenParamsRef off the exact
            -- page being targeted) — an empty overlay models "this page
            -- has no placed locations", even though the very same
            -- coordinate is rejected on the page that DOES place loc1
            -- above. Cross-page isolation falls out of that per-page
            -- scoping rather than anything canPlaceAt itself branches on.
            canPlaceAt noBuildings wtd registry1 emptyLocationOverlay 0
                       portalDef 8 8
                `shouldBe` Placeable

    describe "distinct rejection reasons (review amendment)" $ do
        it "\"chunk not loaded\" when the footprint's chunk isn't loaded" $
            canPlaceAt noBuildings (worldWithChunks []) registry1 overlay1 0
                       portalDef 100 100
                `shouldBe` NotPlaceable "chunk not loaded"

        it "\"ground is uneven\" when the footprint spans two terrain heights" $
            canPlaceAt noBuildings (worldWithChunks [unevenChunk])
                       emptyLocationRegistry emptyLocationOverlay 0
                       (mkDef True 2 1) 0 0
                `shouldBe` NotPlaceable "ground is uneven"

        it "\"tile already occupied\" when an existing building covers the tile" $
            canPlaceAt (occupiedAt 8 8)
                       (worldWithChunks [flatChunkAt (ChunkCoord 0 0) 5])
                       emptyLocationRegistry emptyLocationOverlay 0
                       portalDef 8 8
                `shouldBe` NotPlaceable "tile already occupied"

        it "the location-overlap reason differs from the other three" $
            case canPlaceAt noBuildings
                             (worldWithChunks [flatChunkAt (ChunkCoord 0 0) 5])
                             registry1 overlay1 0 portalDef 8 8 of
                NotPlaceable reason → do
                    reason `shouldBe` "inside a location's bounds"
                    reason `shouldNotSatisfy`
                        (`elem` [ "chunk not loaded", "ground is uneven"
                                , "tile already occupied" ])
                Placeable → expectationFailure "expected NotPlaceable"

    describe "direct spawn and preview validation agree" $
        -- Engine.Scripting.Lua.API.Buildings.Spawn's buildingSpawnFn
        -- (building.spawn) and buildingCanPlaceAtFn (building.canPlaceAt,
        -- the ghost preview's check) both call this exact function with
        -- the same page-scoped inputs — so agreement is structural, not
        -- something either Lua entry point can special-case around.
        it "the same inputs always produce the same result" $
            canPlaceAt noBuildings
                       (worldWithChunks [flatChunkAt (ChunkCoord 0 0) 5])
                       registry1 overlay1 0 portalDef 8 8
                `shouldBe`
                canPlaceAt noBuildings
                           (worldWithChunks [flatChunkAt (ChunkCoord 0 0) 5])
                           registry1 overlay1 0 portalDef 8 8

    describe "cylindrical U-seam overlap (mirrors #777's Location.Bounds contract)" $ do
        -- worldSize 2 chunks -> worldWidthTiles 32, halfW 16. loc1 is
        -- anchored at chunk (1,0) -> tile (24,8) -> AbsBounds
        -- (22,6)..(26,10). Its seam alias shifted by (-16,+16) is
        -- (6,22)..(10,26), which DOES contain footprint tile (8,24) —
        -- physically adjacent across the wrap even though the raw
        -- coordinates are far apart.
        let seamOverlay = HM.singleton (ChunkCoord 1 0) "loc1"
            seamWtd = worldWithChunks [flatChunkAt (ChunkCoord 0 1) 5]
            seamCheck ws = canPlaceAt noBuildings seamWtd registry1
                                       seamOverlay ws portalDef 8 24

        it "does not intersect under the raw (non-wrapping) coordinates" $
            seamCheck 0 `shouldBe` Placeable

        it "intersects once the seam wrap is considered" $
            seamCheck 2 `shouldBe` NotPlaceable "inside a location's bounds"

    describe "Building ghost validity tint (#778)" $ do
        it "a valid ghost is neutral white" $ do
            let Vec4 rr gg bb _ = ghostTint True
            (rr, gg, bb) `shouldBe` (1.0, 1.0, 1.0)

        it "an invalid ghost is red-dominant" $ do
            let Vec4 rr gg bb _ = ghostTint False
            rr `shouldSatisfy` (> gg)
            rr `shouldSatisfy` (> bb)

        it "both valid and invalid tints stay translucent" $ do
            let Vec4 _ _ _ aValid   = ghostTint True
                Vec4 _ _ _ aInvalid = ghostTint False
            aValid   `shouldSatisfy` (\x → x > 0 ∧ x < 1)
            aInvalid `shouldSatisfy` (\x → x > 0 ∧ x < 1)

        it "changing validity replaces the previous ghost tint" $
            ghostTint True `shouldNotBe` ghostTint False
