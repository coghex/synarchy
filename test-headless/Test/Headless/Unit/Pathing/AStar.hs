{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Tests for Unit.Pathing.AStar.
--   Uses synthetic WorldTileData fixtures: a single chunk at the
--   origin with handcrafted per-tile terrain and fluid.
module Test.Headless.Unit.Pathing.AStar (spec) where

import UPrelude
import Test.Hspec
import Data.Word (Word8)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..), ColumnTiles(..), chunkSize)
import World.Tile.Types (WorldTileData(..))
import World.Fluid.Types (FluidCell(..), FluidType(..), emptyIceMap)
import World.Flora.Types (emptyFloraChunkData)
import Structure.Types (emptyChunkStructures)
import World.Material ( MaterialRegistry, MaterialProps(..), emptyMaterialRegistry
                      , defaultMaterialProps, registerMaterial )
import Unit.Pathing.AStar
import Unit.Pathing.Cost ( PathingConfig(..), defaultPathingConfig, stepCost
                         , materialFactor, lookupSurfaceMaterial, materialDetour )

-- A* cost comes from the pathing config; the tests use the default
-- profile (the historical hard-coded weights).
pc ∷ PathingConfig
pc = defaultPathingConfig

-- These fixtures carry no per-column material data (lcTiles = V.empty),
-- so the #312 material factor resolves to 1.0 everywhere — an empty
-- registry keeps these route/cost assertions byte-identical.
reg ∷ MaterialRegistry
reg = emptyMaterialRegistry

flatChunk ∷ Int → LoadedChunk
flatChunk terrZ =
    let area = chunkSize * chunkSize
        terrV = VU.replicate area terrZ
        surfV = VU.replicate area terrZ
        fluidV = V.replicate area Nothing
    in LoadedChunk
        { lcCoord             = ChunkCoord 0 0
        , lcTiles             = V.empty
        , lcSurfaceMap        = surfV
        , lcTerrainSurfaceMap = terrV
        , lcFluidMap          = fluidV
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.empty
        , lcWaterTableMap     = VU.empty
        , lcMagma             = Nothing
        , lcStructures        = emptyChunkStructures
        }

customChunk ∷ ((Int, Int) → (Int, Maybe FluidType)) → LoadedChunk
customChunk f =
    let area = chunkSize * chunkSize
        terrV = VU.generate area $ \i →
            let (lx, ly) = (i `mod` chunkSize, i `div` chunkSize)
            in fst (f (lx, ly))
        surfV = terrV
        fluidV = V.generate area $ \i →
            let (lx, ly) = (i `mod` chunkSize, i `div` chunkSize)
            in case snd (f (lx, ly)) of
                Just ft → Just (FluidCell ft (fst (f (lx, ly))))
                Nothing → Nothing
    in LoadedChunk
        { lcCoord             = ChunkCoord 0 0
        , lcTiles             = V.empty
        , lcSurfaceMap        = surfV
        , lcTerrainSurfaceMap = terrV
        , lcFluidMap          = fluidV
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.empty
        , lcWaterTableMap     = VU.empty
        , lcMagma             = Nothing
        , lcStructures        = emptyChunkStructures
        }

worldWith ∷ LoadedChunk → WorldTileData
worldWith lc = WorldTileData
    { wtdChunks    = HM.singleton (ChunkCoord 0 0) lc
    , wtdMaxChunks = 1
    }

-- A flat chunk whose every column is a single surface tile at z, tagged
-- with a per-tile material id from `matAt`. The #312 movement factor
-- reads the TOP of each column's ctMats, so a one-tile column suffices.
materialChunk ∷ Int → ((Int, Int) → Word8) → LoadedChunk
materialChunk z matAt =
    let area  = chunkSize * chunkSize
        terrV = VU.replicate area z
        tiles = V.generate area $ \i →
            let (lx, ly) = (i `mod` chunkSize, i `div` chunkSize)
            in ColumnTiles { ctStartZ = z
                           , ctMats   = VU.singleton (matAt (lx, ly))
                           , ctSlopes = VU.singleton 0
                           , ctVeg    = VU.singleton 0 }
    in LoadedChunk
        { lcCoord             = ChunkCoord 0 0
        , lcTiles             = tiles
        , lcSurfaceMap        = terrV
        , lcTerrainSurfaceMap = terrV
        , lcFluidMap          = V.replicate area Nothing
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.empty
        , lcWaterTableMap     = VU.empty
        , lcMagma             = Nothing
        , lcStructures        = emptyChunkStructures
        }

-- Registry where material id 1 is "soft" ground (high move_cost) and id
-- 0 keeps the firm default (move_cost 1.0).
softReg ∷ Float → MaterialRegistry
softReg cost =
    registerMaterial 1 (defaultMaterialProps { mpMoveCost = cost })
        emptyMaterialRegistry

spec ∷ Spec
spec = do
    describe "Unit.Pathing.AStar.localAStar" $ do

        describe "open flat terrain" $ do
            let wtd = worldWith (flatChunk 5)

            it "src == dst → empty path" $
                localAStar pc reg wtd (3, 3) (3, 3) 16 `shouldBe` []

            it "one cardinal step" $
                localAStar pc reg wtd (3, 3) (4, 3) 16 `shouldBe` [(4, 3)]

            it "corner-to-corner of chunk uses 15 diagonal steps" $ do
                let path = localAStar pc reg wtd (0, 0) (15, 15) 32
                length path `shouldBe` 15
                last path `shouldBe` (15, 15)

        describe "obstacle avoidance" $ do
            -- Lava wall at lx=8 ly ∈ [1..14]; openings at ly=0 and ly=15.
            let wtd = worldWith $ customChunk $ \(lx, ly) →
                    if lx ≡ 8 ∧ ly > 0 ∧ ly < 15
                       then (10, Just Lava)
                       else (5, Nothing)
                onLava (x, y) = x ≡ 8 ∧ y > 0 ∧ y < 15

            it "routes around the lava wall" $ do
                let path = localAStar pc reg wtd (5, 5) (12, 5) 32
                any onLava path `shouldBe` False
                last path `shouldBe` (12, 5)

        describe "unreachable destination" $ do
            -- Full vertical wall at lx=8 cuts the chunk in half.
            let wtd = worldWith $ customChunk $ \(lx, _) →
                    if lx ≡ 8 then (10, Just Lava) else (5, Nothing)

            it "returns a partial path that ends at the wall" $ do
                let path = localAStar pc reg wtd (5, 5) (12, 5) 16
                case path of
                    [] → expectationFailure "expected partial progress toward wall"
                    _  → let (lx, _) = last path
                         in lx `shouldBe` 7

        describe "radius bound" $ do
            let wtd = worldWith (flatChunk 5)

            it "tiny radius yields a path that stops at the bound" $ do
                let path = localAStar pc reg wtd (0, 0) (15, 15) 3
                length path `shouldBe` 3
                last path `shouldBe` (3, 3)

        describe "high-cost climb" $ do
            -- Cheap detour around a high-cost ridge. The ridge is at
            -- lx=8, all ly. Stepping onto the ridge costs ~1+climbFactor;
            -- stepping past it via flat tiles around the corner is
            -- much cheaper if any flat passage exists.
            --
            -- Here we leave a flat passage at ly=0 only; everywhere
            -- else lx=8 is z=15 (10 z above z=5 surroundings).
            let wtd = worldWith $ customChunk $ \(lx, ly) →
                    if lx ≡ 8 ∧ ly /= 0
                       then (15, Nothing)
                       else (5, Nothing)

            it "prefers the flat passage at ly=0 over climbing the ridge" $ do
                let path = localAStar pc reg wtd (5, 5) (12, 5) 32
                -- Path must include at least one tile at ly=0.
                any (\(_, y) → y ≡ 0) path `shouldBe` True
                last path `shouldBe` (12, 5)

        describe "surface material movement factor (#312)" $ do
            -- All tiles firm (mat 0) except a vertical "soft" band at
            -- lx=8 for ly ∈ [1..14] (mat 1). Firm openings at ly=0/15.
            let band (x, y) = if x ≡ 8 ∧ y > 0 ∧ y < 15 then 1 else 0
                wtd = worldWith (materialChunk 5 band)
                onSoft (x, y) = x ≡ 8 ∧ y > 0 ∧ y < 15

            it "materialFactor reads the tile's surface move_cost" $ do
                materialFactor (softReg 1.7) wtd 8 5 `shouldBe` 1.7   -- soft band
                materialFactor (softReg 1.7) wtd 2 5 `shouldBe` 1.0   -- firm

            it "lookupSurfaceMaterial reads the top-of-column material" $ do
                lookupSurfaceMaterial wtd 8 5 `shouldBe` Just 1
                lookupSurfaceMaterial wtd 2 5 `shouldBe` Just 0

            it "a step onto soft ground costs more than onto firm ground" $ do
                let firm = stepCost pc (softReg 1.6) wtd (2, 5) (3, 5)  -- firm→firm
                    soft = stepCost pc (softReg 1.6) wtd (7, 5) (8, 5)  -- firm→soft
                firm `shouldBe` Just 1.0
                soft `shouldBe` Just 1.6

            it "with no material data the factor is a 1.0 no-op (firm baseline)" $ do
                let wtdFlat = worldWith (flatChunk 5)   -- lcTiles = V.empty
                stepCost pc (softReg 1.6) wtdFlat (2, 5) (3, 5) `shouldBe` Just 1.0

            it "A* detours around a costly soft band when a firm route is cheaper" $ do
                -- move_cost 12 makes crossing the band dearer than the
                -- firm detour through the ly=0/15 openings.
                let path = localAStar pc (softReg 12.0) wtd (5, 5) (12, 5) 32
                any onSoft path `shouldBe` False
                last path `shouldBe` (12, 5)

            it "A* still crosses a mildly soft band when detouring costs more" $ do
                -- move_cost 1.05 is cheaper to walk through than the long
                -- way around, so the optimal route goes straight across.
                let path = localAStar pc (softReg 1.05) wtd (5, 5) (12, 5) 32
                any onSoft path `shouldBe` True
                last path `shouldBe` (12, 5)

            -- The greedy mover never crosses pcReplanCostThreshold for mild
            -- material, so it needs `materialDetour` to even ASK A* to look
            -- for a firmer route when stepping onto soft ground (#312).
            it "materialDetour fires stepping onto soft ground" $
                -- step onto the soft band (8,5): move_cost 1.5 ≥ 1.0+0.25.
                materialDetour pc (softReg 1.5) wtd (8, 5) `shouldBe` True

            it "materialDetour also fires WITHIN soft ground (deep-field re-eval)" $
                -- A tile deep in the soft band still triggers, so a wide
                -- field keeps being re-evaluated as the unit advances and a
                -- firmer route beyond the first A* horizon is eventually
                -- found. (The mover bounds this to ~one A* per local-path
                -- length via its follow-the-path suppression, not here.)
                materialDetour pc (softReg 1.5) wtd (8, 7) `shouldBe` True

            it "materialDetour ignores mild (sub-margin) ground" $
                -- move_cost 1.2 < 1.0+0.25: speed-only, no detour-check
                -- (units don't reroute around mild ground like loam/gravel).
                materialDetour pc (softReg 1.2) wtd (8, 5) `shouldBe` False

            it "materialDetour is quiet on firm ground" $
                materialDetour pc (softReg 1.5) wtd (2, 5) `shouldBe` False
