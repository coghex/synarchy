-- Reaches the WML-5 generator unchanged. Source classifications are
-- observed from the actual sampled cells, never from corpus labels.
module Corpus (generateCorpus, inventories) where

import UPrelude
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.IORef (readIORef)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Test.Headless.Harness (withHeadlessEngine, sharedWorld, getWorldGenParams)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Engine.Core.State (materialRegistryRef, loggerRef)
import World.ZoomMap.Pyramid
import World.ZoomMap.ColorPalette (buildColorPalette)
import World.ZoomMap.Cache.ChunkPass (ZoomChunkPass(..))
import World.Fluid.Types (FluidCell(..), FluidType(..))

data Page = Page String Int Int Int
instance A.FromJSON Page where
    parseJSON = A.withObject "page" $ \o → Page
        ⊚ o A..: "id" ⊛ o A..: "level" ⊛ o A..: "u" ⊛ o A..: "v"

data World = World Word64 Int Int [Page]
instance A.FromJSON World where
    parseJSON = A.withObject "world" $ \o → World
        ⊚ o A..: "seed" ⊛ o A..: "size" ⊛ o A..: "plates" ⊛ o A..: "pages"

accept ∷ Show e ⇒ Either e α → IO α
accept = either (fail ∘ show) pure

inventories ∷ IO A.Value
inventories = A.toJSON ⊚ forM [64, 136, 1024, 8192] (\size → do
    inv ← accept (mapPyramidInventory size)
    let geom = mpiGeometry inv
    pure $ A.object
        ["size" A..= size, "coarse_cutoff" A..= mpiCoarseCutoffLevel inv,
         "levels" A..= [A.object ["level" A..= level,
             "u" A..= mapLevelPagesU geom level,
             "v" A..= mapLevelPagesV geom level]
             | level ← [0 .. mpiRootLevel inv]]])

generateCorpus ∷ FilePath → FilePath → IO ()
generateCorpus manifest output = do
    worlds ← A.eitherDecodeFileStrict manifest ≫= either fail pure
    createDirectoryIfMissing True output
    withIsolatedResourceRoot $ withHeadlessEngine $ \env → do
      registry ← readIORef (materialRegistryRef env)
      logger ← readIORef (loggerRef env)
      palette ← buildColorPalette logger "data/materials" "data/vegetation"
      rows ← forM (worlds ∷ [World]) $ \(World seed size plates pages) → do
        hPutStrLn stderr ("Generating world " ⧺ show (seed, size, plates))
        ws ← sharedWorld env seed size plates
        params ← getWorldGenParams ws ≫= maybe (fail "no generation params") pure
        inv ← accept (mapPyramidInventory size)
        let geom = mpiGeometry inv
        source ← accept (worldGenCellSource geom params registry palette Nothing)
        forM pages $ \(Page ident level pu pv) → do
          hPutStrLn stderr ("Generating page " ⧺ ident)
          let key = MapPageKey level pu pv
          rgba ← accept (mapPageImage inv source key)
          BS.writeFile (output </> ident ⧺ ".rgba") rgba
          -- Include gutters, wrapping U and clipping V exactly as WML-5
          -- samples them. Classification is descriptive; it changes no pixels.
          let scale = 2 ^ level
              x0 = (pu * 512 - 1) * scale
              y0 = (pv * 512 - 1) * scale
              side = 514 * scale
              cells = Set.toList $ Set.fromList
                [ MapCell (x `mod` mgCellsU geom) y
                | y ← [y0 `div` 32 .. (y0 + side - 1) `div` 32]
                , y ≥ 0, y < mgCellsV geom
                , x ← [x0 `div` 32 .. (x0 + side - 1) `div` 32] ]
          coords ← mapM (accept ∘ chunkOfFinestCell geom) cells
          roundTrips ← mapM (accept ∘ finestCellOfChunk geom) coords
          let table = mapCellHaloTable params registry Nothing coords
              sampled = [table Map.! coord | coord ← coords]
              fluids = concatMap (V.toList ∘ zcpRawFluid) sampled
              ocean = length [() | Just (FluidCell Ocean _) ← fluids]
              lava = length [() | Just (FluidCell Lava _) ← fluids]
              ice = sum [V.length (V.filter isJust (zcpIceMap pass)) | pass ← sampled]
              materials = Set.fromList
                [mat | pass ← sampled, (e, mat, _, _, _) ← V.toList (zcpTiles pass), e > minBound]
              dryMaterials = Set.fromList
                [mat | pass ← sampled,
                 ((e, mat, _, _, _), Nothing) ← V.toList (V.zip (zcpTiles pass) (zcpRawFluid pass)),
                 e > minBound]
              transparent = length [() | i ← [3, 7 .. BS.length rgba - 1], BS.index rgba i ≡ 0]
          pure $ A.object
            [ "id" A..= ident, "seed" A..= seed, "size" A..= size
            , "level" A..= level, "u" A..= pu, "v" A..= pv
            , "ocean_source_tiles" A..= ocean, "lava_source_tiles" A..= lava
            , "ice_source_tiles" A..= ice
            , "material_ids" A..= Set.toList materials
            , "dry_material_ids" A..= Set.toList dryMaterials
            , "transparent_pixels" A..= transparent
            , "parity_round_trip" A..= (roundTrips ≡ cells)
            , "longitude_seam" A..= (x0 < 0 ∨ x0 + side > mapLevelWidth geom 0)
            , "latitude_edge" A..= (y0 < 0 ∨ y0 + side > mapLevelHeight geom 0)
            , "pages_u" A..= mapLevelPagesU geom level
            , "pages_v" A..= mapLevelPagesV geom level
            , "root_level" A..= mpiRootLevel inv
            , "coarse_cutoff" A..= mpiCoarseCutoffLevel inv ]
      BL.writeFile (output </> "corpus.json") (A.encode (concat rows))
