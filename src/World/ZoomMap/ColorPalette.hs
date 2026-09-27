{-# LANGUAGE Strict #-}
-- | Sample zoom and vegetation textures to build a color palette
--   for per-chunk zoom map texture generation.
module World.ZoomMap.ColorPalette
    ( ZoomColorPalette(..)
    , PaletteSource(..)
    , buildColorPalette
    , lookupMatColor
    , lookupVegColorById
    , defaultOceanColor
    , defaultLavaColor
    ) where

import UPrelude
import qualified Codec.Picture as JP
import Control.Exception (IOException, try)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Engine.Asset.YamlMaterials (MaterialDef(..), loadMaterialDirectory)
import Engine.Asset.YamlVegetation (VegetationDef(..), loadVegetationYaml)
import Engine.Core.Log (LoggerState, logInfo, logWarn, LogCategory(..))
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Control.DeepSeq (NFData(..))

-- | Average RGBA color for a material or vegetation type.
type RGBA = (Word8, Word8, Word8, Word8)

-- | Color palette built from sampling the actual texture PNGs.
--   Material colors are keyed by material ID (Word8).
--   Vegetation colors are keyed by vegetation ID (Word8).
--   'zcpSources' records every texture the palette tried to sample
--   (#2692), so a cache keyed on the palette can depend on exactly the
--   bytes that produced its colours.
data ZoomColorPalette = ZoomColorPalette
    { zcpMaterials  ∷ !(Map.Map Word8 RGBA)
    , zcpVegetation ∷ !(Map.Map Word8 RGBA)
    , zcpSources    ∷ ![PaletteSource]
    } deriving (Show)

-- | One texture a palette build sampled: the definition that named it,
--   the path exactly as authored, and the SHA-256 of the very bytes the
--   sample decoded — or why it could not be sampled (missing, unreadable,
--   undecodable or empty).
data PaletteSource = PaletteSource
    { psOwner  ∷ !Text
    , psPath   ∷ !FilePath
    , psDigest ∷ !(Either Text BS.ByteString)
    } deriving (Show, Eq)

instance NFData PaletteSource where
    rnf (PaletteSource o p d) = rnf o `seq` rnf p `seq` rnf d

instance NFData ZoomColorPalette where
    rnf (ZoomColorPalette m v s) = rnf m `seq` rnf v `seq` rnf s

defaultOceanColor ∷ RGBA
defaultOceanColor = (30, 60, 120, 255)

defaultLavaColor ∷ RGBA
defaultLavaColor = (200, 60, 20, 255)

-- * Texture Sampling

-- | Sample a texture file at a grid of points and return the SHA-256
--   of the bytes read together with their average RGBA color.  Reading
--   and decoding ONE byte string is what lets the digest name exactly
--   the input the colour came from.  Returns the reason on failure.
sampleTexture ∷ FilePath → IO (Either Text (BS.ByteString, RGBA))
sampleTexture path = do
    readResult ← try (BS.readFile path)
    pure $ case readResult of
        Left (e ∷ IOException) → Left ("cannot read: " <> tshow e)
        Right bytes → case JP.decodeImage bytes of
            Left err → Left ("cannot decode: " <> T.pack err)
            Right dynImg → case averageColor (JP.convertRGBA8 dynImg) of
                Nothing → Left "texture has no pixels"
                Just c  → Right (SHA256.hash bytes, c)

-- | Average up to 8×8 grid samples; Nothing for an empty image.
averageColor ∷ JP.Image JP.PixelRGBA8 → Maybe RGBA
averageColor img =
    let w   = JP.imageWidth img
        h   = JP.imageHeight img
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
        (sR, sG, sB, sA) = foldl' acc (0 ∷ Int, 0, 0, 0) samples
        acc (r,g,b,a) (JP.PixelRGBA8 pr pg pb pa) =
            ( r + fromIntegral pr, g + fromIntegral pg
            , b + fromIntegral pb, a + fromIntegral pa )
    in if n ≡ 0
        then Nothing
        else Just ( fromIntegral (sR `div` n)
                  , fromIntegral (sG `div` n)
                  , fromIntegral (sB `div` n)
                  , fromIntegral (sA `div` n) )

-- | Sample one texture and record it as a 'PaletteSource'.
sampleSource ∷ Text → FilePath → IO (PaletteSource, Maybe RGBA)
sampleSource owner path = do
    result ← sampleTexture path
    pure ( PaletteSource owner path (fst ⊚ result)
         , either (const Nothing) (Just . snd) result )

-- * Palette Construction

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
    (matPalette, matSources) ← buildMatPalette logger matDefs

    -- Sample vegetation textures
    (vegPalette, vegSources) ← buildVegPalette logger vegDefs

    logInfo logger CatWorld $ "Palette built: "
        <> tshow (Map.size matPalette) <> " materials, "
        <> tshow (Map.size vegPalette) <> " vegetation"

    pure $ ZoomColorPalette matPalette vegPalette (matSources <> vegSources)

buildMatPalette ∷ LoggerState → [MaterialDef]
               → IO (Map.Map Word8 RGBA, [PaletteSource])
buildMatPalette logger defs = do
    pairs ← forM defs $ \def → do
        (source, mColor) ← sampleSource
            ("material " <> mdName def <> " (id " <> tshow (mdId def) <> ")")
            (T.unpack (mdZoom def))
        case mColor of
            Nothing → do
                logWarn logger CatWorld $ "Cannot sample zoom texture for "
                    <> mdName def <> ": " <> mdZoom def
                pure (source, Nothing)
            Just c → pure (source, Just (mdId def, c))
    pure ( Map.fromList [ (matId, c) | (_, Just (matId, c)) ← pairs ]
         , map fst pairs )

buildVegPalette ∷ LoggerState → [VegetationDef]
               → IO (Map.Map Word8 RGBA, [PaletteSource])
buildVegPalette _logger defs = do
    allPairs ← forM defs $ \def → do
        let baseId = vdIdStart def
        forM (zip [0 ..] (vdVariants def)) $ \(i, path) → do
            (source, mColor) ← sampleSource
                ("vegetation " <> vdName def <> " variant " <> tshow (i ∷ Int)
                    <> " (id " <> tshow (baseId + fromIntegral i) <> ")")
                (T.unpack path)
            pure (source, (\c → (baseId + fromIntegral i, c)) ⊚ mColor)
    let pairs = concat allPairs
    pure ( Map.fromList [ (vid, c) | (_, Just (vid, c)) ← pairs ]
         , map fst pairs )

listVegetationYamls ∷ FilePath → IO [FilePath]
listVegetationYamls dir = do
    entries ← listDirectory dir
    pure [ dir </> f | f ← entries
         , takeExtension f ∈ [".yaml", ".yml"] ]

-- * Palette Lookup

-- | Look up material color, falling back to grey.
lookupMatColor ∷ ZoomColorPalette → Word8 → RGBA
lookupMatColor palette matId =
    case Map.lookup matId (zcpMaterials palette) of
        Just c  → c
        Nothing → (128, 128, 128, 255)

-- | Look up vegetation color by exact veg ID.
--   Returns Nothing for vegNone (0) or if the ID isn't in the palette.
lookupVegColorById ∷ ZoomColorPalette → Word8 → Maybe RGBA
lookupVegColorById _palette 0 = Nothing
lookupVegColorById palette vegId =
    Map.lookup vegId (zcpVegetation palette)
