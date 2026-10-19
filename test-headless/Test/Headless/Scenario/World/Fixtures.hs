{-# LANGUAGE Strict #-}
-- | Fixtures the world-content tests share (#2711): the species and
--   material catalogs, the build environment and the entry constructors.
module Test.Headless.Scenario.World.Fixtures
    ( registry, materialNamed, bounds, oakId, wheatId, catalog, seed, tag, ex
    , envAt, terrainPatch, fluidPatch, floraEntry, withPaths, content
    , chunkOf, columnAt, at, speciesOf, sole, occurrencesIn
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Engine.Asset.Handle (TextureHandle(..))
import Gameplay.Tags.Types (GameplayTag, mkGameplayTag)
import Scenario.Types
import Scenario.World.Build
import World.Arena.Bounds (ArenaBounds, mkArenaBounds)
import World.Chunk.Types
    ( ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), columnIndex )
import World.Edit.Apply (replayEdits)
import World.Edit.Types (WorldEdits)
import World.Flora.Identity (firstPlantedFloraCursor)
import World.Flora.Types
import World.Generate.Arena (clipArenaChunk, generateArenaChunkIn)
import World.Generate.Coordinates (globalToChunk)
import World.Material
    ( MaterialProps(..), MaterialRegistry, emptyMaterialRegistry, defaultMaterialProps
    , matGranite, matLoam, matSand, registerMaterial )
import World.Material.Id (MaterialId(..))

-- * Fixtures

-- | A tiny material registry: loam, granite and sand by name.
registry ∷ MaterialRegistry
registry = foldl' (\r (i, n) → registerMaterial i defaultMaterialProps { mpName = n } r)
    emptyMaterialRegistry
    [ (unMaterialId matGranite, "granite")
    , (unMaterialId matLoam, "loam")
    , (unMaterialId matSand, "sand") ]

materialNamed ∷ Text → Maybe MaterialId
materialNamed n = case n of
    "granite" → Just matGranite
    "loam"    → Just matLoam
    "sand"    → Just matSand
    "air"     → Just (MaterialId 0)
    _         → Nothing

bounds ∷ Int → Int → ArenaBounds
bounds w h = either (error ∘ show) id (mkArenaBounds w h)

-- | The flora species the fixtures use, by id: a wild harvestable tree, a
--   wild decorative fern, a harvestable row crop, and a groundcover crop.
oakId, wheatId ∷ FloraId
oakId = FloraId 1; wheatId = FloraId 4

catalog ∷ FloraCatalog
catalog = emptyFloraCatalog
    { fcSpecies = HM.fromList
        [ (1, harvestable "oak" 100 Evergreen)
        , (2, (newFloraSpecies "fern" (TextureHandle 0)))
        , (3, harvestable "beet" 200 Annual)
        , (4, (newFloraSpecies "wheat" (TextureHandle 0)) { fsLifecycle = Annual }) ]
    , fcWorldGen = HM.fromList
        [ (1, worldGen "tree" 16), (2, worldGen "shrub" 8)
        , (3, worldGen "row_crop" 12), (4, worldGen "groundcover_crop" 0) ]
    , fcNextId = 5 }
  where
    harvestable name secs life = (newFloraSpecies name (TextureHandle 0))
        { fsLifecycle = life
        , fsHarvest = Just FloraHarvest
            { fhTags = [], fhUngatedTags = [], fhYield = [("log", 1, 1)]
            , fhPhaseYields = HM.empty, fhRegrowth = secs
            , fhHarvestedTexture = TextureHandle 0 } }
    worldGen cat width = FloraWorldGen
        { fwCategory = cat, fwMinTemp = -50, fwMaxTemp = 50, fwIdealTemp = 10
        , fwMinPrecip = 0, fwMaxPrecip = 1, fwIdealPrecip = 0.5
        , fwMinAlt = -100, fwMaxAlt = 100, fwIdealAlt = 0
        , fwMinHumidity = 0, fwMaxHumidity = 1, fwIdealHumidity = 0.5
        , fwMaxSlope = 15, fwDensity = 0, fwSoils = [], fwFootprint = width }

seed ∷ Word64
seed = 2711

tag ∷ Text → GameplayTag
tag t = fromMaybe (error "tag") (mkGameplayTag t)

ex ∷ Text → ScenarioId
ex = ExplicitId

-- | The build environment at a given absolute day and cursor.
envAt ∷ Maybe ArenaBounds → Int → Word64 → WorldBuildEnv
envAt b day cursor = WorldBuildEnv
    { wbeBounds = b, wbeMaterial = materialNamed, wbeFlora = catalog
    , wbeAbsDay = day, wbeCursor = cursor }

terrainPatch ∷ Text → TileRegion → Text → Int → Authored Word8 → [GameplayTag] → TerrainPatch
terrainPatch i r m z s tags = TerrainPatch (ex i) tags r m z s

fluidPatch ∷ Text → TileRegion → FluidKind → Int → FluidPatch
fluidPatch i r k e = FluidPatch (ex i) [] r k e

floraEntry ∷ Text → Text → Int → Int → FloraEntry
floraEntry i sp x y = FloraEntry (ex i) [] sp x y Omitted Omitted Omitted
                                 Omitted Omitted Omitted Omitted

withPaths ∷ Text → [α] → [(Text, α)]
withPaths fam xs = [ (fam <> "[" <> tshow i <> "]", x) | (i, x) ← zip [0 ∷ Int ..] xs ]

content ∷ Maybe ArenaBounds → Int → [TerrainPatch] → [FluidPatch] → [FloraEntry] → WorldContent
content b day ts fs ls =
    buildWorldContent (envAt b day firstPlantedFloraCursor)
        (withPaths "terrain" ts) (withPaths "fluids" fs) (withPaths "flora" ls)

-- | A chunk as the page reconstructs it: the arena base from the seed and
--   bounds, the edit log replayed over it, the rectangle re-asserted.
chunkOf ∷ Maybe ArenaBounds → WorldEdits → ChunkCoord → LoadedChunk
chunkOf b edits c = clipArenaChunk b (replayEdits edits (generateArenaChunkIn b seed c))

-- | What a chunk holds at a global tile.
columnAt ∷ LoadedChunk → (Int, Int) → (Int, Maybe Word8, Word8, Word8)
columnAt lc (gx, gy) =
    let (_, (lx, ly)) = globalToChunk gx gy
        idx = columnIndex lx ly
        col = lcTiles lc V.! idx
        top = lcTerrainSurfaceMap lc VU.! idx
        i = top - ctStartZ col
        inCol = i ≥ 0 ∧ i < VU.length (ctMats col)
    in ( top
       , if inCol then Just (ctMats col VU.! i) else Nothing
       , if inCol then ctSlopes col VU.! i else 0
       , if inCol then ctVeg col VU.! i else 0 )

at ∷ Maybe ArenaBounds → WorldEdits → (Int, Int) → (Int, Maybe Word8, Word8, Word8)
at b edits t = columnAt (chunkOf b edits (fst (globalToChunk (fst t) (snd t)))) t

-- | The species record for a fixture id.
speciesOf ∷ FloraId → FloraSpecies
speciesOf fid = fromMaybe (error "species") (lookupSpecies fid catalog)

-- | The one element of a list that must have exactly one.
sole ∷ [α] → α
sole [x] = x
sole xs  = error ("expected exactly one, got " ⧺ show (length xs))

occurrencesIn ∷ Maybe ArenaBounds → WorldEdits → [FloraInstance]
occurrencesIn b es =
    [ fi | c ← HM.keys es, fi ← fcdInstances (lcFlora (chunkOf b es c)) ]

