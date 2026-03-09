{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Sample zoom and vegetation textures to build a color palette
--   for per-chunk zoom map texture generation.
module World.ZoomMap.ColorPalette
    ( ZoomColorPalette(..)
    , emptyColorPalette
    , buildColorPalette
    , lookupMatColor
    , lookupVegColor
    , lookupVegColorById
    , defaultOceanColor
    , defaultLavaColor
    ) where

import UPrelude
import qualified Codec.Picture as JP
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Engine.Asset.YamlTextures (MaterialDef(..), VegetationDef(..)
                                 , loadMaterialDirectory, loadVegetationYaml)
import Engine.Core.Log (LoggerState, logInfo, logWarn, LogCategory(..))
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Control.DeepSeq (NFData(..))

-- | Average RGBA color for a material or vegetation type.
type RGBA = (Word8, Word8, Word8, Word8)

-- | Color palette built from sampling the actual texture PNGs.
--   Material colors are keyed by material ID (Word8).
--   Vegetation colors are keyed by vegetation ID (Word8).
data ZoomColorPalette = ZoomColorPalette
    { zcpMaterials  ∷ !(Map.Map Word8 RGBA)
    , zcpVegetation ∷ !(Map.Map Word8 RGBA)
    } deriving (Show)

instance NFData ZoomColorPalette where
    rnf (ZoomColorPalette m v) = rnf m `seq` rnf v

emptyColorPalette ∷ ZoomColorPalette
emptyColorPalette = ZoomColorPalette Map.empty Map.empty

defaultOceanColor ∷ RGBA
defaultOceanColor = (30, 60, 120, 255)

defaultLavaColor ∷ RGBA
defaultLavaColor = (200, 60, 20, 255)

-----------------------------------------------------------
-- Texture Sampling
-----------------------------------------------------------

-- | Sample a texture file at a grid of points and return
--   the average RGBA color.  Returns Nothing if the file
--   cannot be loaded.
sampleTextureAvgColor ∷ FilePath → IO (Maybe RGBA)
sampleTextureAvgColor path = do
    result ← JP.readImage path
    case result of
        Left _err → pure Nothing
        Right dynImg → do
            let img = JP.convertRGBA8 dynImg
                w   = JP.imageWidth img
                h   = JP.imageHeight img
                -- Sample up to 8×8 = 64 points
                gridN = min 8 (min w h)
                stepX = max 1 (w `div` (gridN + 1))
                stepY = max 1 (h `div` (gridN + 1))
                samples = [ JP.pixelAt img px py
                          | sx ← [1 .. gridN]
                          , sy ← [1 .. gridN]
                          , let px = min (w - 1) (sx * stepX)
                          , let py = min (h - 1) (sy * stepY)
                          ]
                n = length samples
            if n ≡ 0
                then pure Nothing
                else do
                    let (sR, sG, sB, sA) = foldl' acc (0 ∷ Int, 0, 0, 0) samples
                        acc (r,g,b,a) (JP.PixelRGBA8 pr pg pb pa) =
                            ( r + fromIntegral pr, g + fromIntegral pg
                            , b + fromIntegral pb, a + fromIntegral pa )
                    pure $ Just ( fromIntegral (sR `div` n)
                                , fromIntegral (sG `div` n)
                                , fromIntegral (sB `div` n)
                                , fromIntegral (sA `div` n) )

-----------------------------------------------------------
-- Palette Construction
-----------------------------------------------------------

-- | Build the complete color palette by loading material and
--   vegetation YAML files and sampling their textures.
buildColorPalette ∷ LoggerState → FilePath → FilePath
                  → IO ZoomColorPalette
buildColorPalette logger matDir vegDir = do
    logInfo logger CatWorld "Building zoom color palette from textures..."

    -- Load all material YAMLs
    matDefs ← loadMaterialDirectory logger matDir

    -- Load all vegetation YAMLs
    vegFiles ← listVegetationYamls vegDir
    vegDefs ← concat ⊚ mapM (loadVegetationYaml logger) vegFiles

    -- Sample material zoom textures
    matPalette ← buildMatPalette logger matDefs

    -- Sample vegetation textures
    vegPalette ← buildVegPalette logger vegDefs

    logInfo logger CatWorld $ "Palette built: "
        <> T.pack (show (Map.size matPalette)) <> " materials, "
        <> T.pack (show (Map.size vegPalette)) <> " vegetation"

    pure $ ZoomColorPalette matPalette vegPalette

buildMatPalette ∷ LoggerState → [MaterialDef]
               → IO (Map.Map Word8 RGBA)
buildMatPalette logger defs = do
    pairs ← forM defs $ \def → do
        mColor ← sampleTextureAvgColor (T.unpack (mdZoom def))
        case mColor of
            Nothing → do
                logWarn logger CatWorld $ "Cannot sample zoom texture for "
                    <> mdName def <> ": " <> mdZoom def
                pure Nothing
            Just c → pure $ Just (mdId def, c)
    pure $ Map.fromList [ (matId, c) | Just (matId, c) ← pairs ]

buildVegPalette ∷ LoggerState → [VegetationDef]
               → IO (Map.Map Word8 RGBA)
buildVegPalette logger defs = do
    allPairs ← forM defs $ \def → do
        let baseId = vdIdStart def
        pairs ← forM (zip [0 ..] (vdVariants def)) $ \(i, path) → do
            mColor ← sampleTextureAvgColor (T.unpack path)
            case mColor of
                Nothing → pure Nothing
                Just c  → pure $ Just (baseId + fromIntegral (i ∷ Int), c)
        pure [ (vid, c) | Just (vid, c) ← pairs ]
    pure $ Map.fromList (concat allPairs)

listVegetationYamls ∷ FilePath → IO [FilePath]
listVegetationYamls dir = do
    entries ← listDirectory dir
    pure [ dir </> f | f ← entries
         , takeExtension f ∈ [".yaml", ".yml"] ]

-----------------------------------------------------------
-- Palette Lookup
-----------------------------------------------------------

-- | Look up material color, falling back to grey.
lookupMatColor ∷ ZoomColorPalette → Word8 → RGBA
lookupMatColor palette matId =
    case Map.lookup matId (zcpMaterials palette) of
        Just c  → c
        Nothing → (128, 128, 128, 255)

-- | Look up vegetation color for a veg category (1-4).
--   Uses a representative vegetation ID per category by
--   finding the closest match in the loaded palette.
--   Returns Nothing for category 0 or if no veg textures loaded.
lookupVegColor ∷ ZoomColorPalette → Word8 → Int → Int → Maybe RGBA
lookupVegColor _palette 0 _ _ = Nothing
lookupVegColor palette vegCat gx gy
    | Map.null (zcpVegetation palette) = Nothing
    | otherwise =
        let vegMap = zcpVegetation palette
            keys   = Map.keys vegMap
            -- Deterministic selection based on position hash
            hash   = abs (gx * 7919 + gy * 6271) `mod` length keys
            vegId  = keys !! hash
        in Map.lookup vegId vegMap

-- | Look up vegetation color by exact veg ID.
--   Returns Nothing for vegNone (0) or if the ID isn't in the palette.
lookupVegColorById ∷ ZoomColorPalette → Word8 → Maybe RGBA
lookupVegColorById _palette 0 = Nothing
lookupVegColorById palette vegId =
    Map.lookup vegId (zcpVegetation palette)
