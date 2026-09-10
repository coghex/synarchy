{-# LANGUAGE Strict #-}
module Test.Headless.World.Render.FluidLevelMasks (spec) where

import UPrelude
import Test.Hspec
import qualified Codec.Picture as JP
import qualified Data.Vector.Storable as VS
import World.Slope.FaceMaps
    (generateFluidLevelFaceMap, generateSlopeFaceMaps, SlopeFaceMaps(..))

pixels ∷ Int → VS.Vector Word8
pixels level = fromMaybe VS.empty (generateFluidLevelFaceMap level)

pixel ∷ VS.Vector Word8 → Int → Int → [Word8]
pixel image x y = VS.toList (VS.slice ((y * 96 + x) * 4) 4 image)

readPixels ∷ FilePath → IO (VS.Vector Word8)
readPixels path = do
    result ← JP.readPng path
    case result of
        Left problem → expectationFailure problem >> pure VS.empty
        Right (JP.ImageRGBA8 image) → do
            (JP.imageWidth image, JP.imageHeight image) `shouldBe` (96, 64)
            pure (JP.imageData image)
        Right _ → expectationFailure (path ++ ": expected RGBA8 PNG") >> pure VS.empty

spec ∷ Spec
spec = do
    it "rejects dry, negative, and overflowing levels instead of clamping" $ do
        forM_ [-1, 0, 9, maxBound] $ \level →
            generateFluidLevelFaceMap level `shouldBe` Nothing

    it "preserves every top and fixed footprint pixel across all eight levels" $ do
        reference ← readPixels "assets/textures/facemap/isoface.png"
        forM_ [1..8] $ \level →
            forM_ [(x,y) | x ← [0..95], y ← [0..24 + min x (95-x) `div` 2]] $ \(x,y) →
                pixel (pixels level) x y `shouldBe` pixel reference x y

    it "preserves the four side-coloured corners even at level one" $ do
        forM_ [1..8] $ \level → do
            forM_ [23,24] $ \y → do
                pixel (pixels level) 0 y `shouldBe` [0,0,255,255]
                pixel (pixels level) 95 y `shouldBe` [255,0,0,255]

    it "adds precisely two correctly coloured side pixels per column per level" $ do
        forM_ [1..7] $ \level →
            forM_ [0..95] $ \x → do
                let end = 24 + min x (95-x) `div` 2 + 2 * level
                    low = pixels level
                    high = pixels (level + 1)
                    changed = [y | y ← [0..63], pixel low x y ≢ pixel high x y]
                    color = if x < 48 then [0,0,255,255] else [255,0,0,255]
                changed `shouldBe` [end + 1, end + 2]
                forM_ changed $ \y → do
                    pixel low x y `shouldBe` [0,0,0,0]
                    pixel high x y `shouldBe` color

    it "has the exact side depth with neither missing nor surplus pixels" $ do
        forM_ [1..8] $ \level →
            forM_ [0..95] $ \x → do
                let start = 25 + min x (95-x) `div` 2
                    color = if x < 48 then [0,0,255,255] else [255,0,0,255]
                forM_ [start..63] $ \y →
                    pixel (pixels level) x y `shouldBe`
                        if y < start + 2 * level then color else [0,0,0,0]

    it "uses only transparent black or one fully opaque face channel" $ do
        forM_ [1..8] $ \level → do
            VS.length (pixels level) `shouldBe` (96 * 64 * 4)
            forM_ [(x,y) | x ← [0..95], y ← [0..63]] $ \(x,y) →
                pixel (pixels level) x y `shouldSatisfy`
                    (`elem` [[0,0,0,0], [255,0,0,255], [0,255,0,255], [0,0,255,255]])

    it "matches both the flat generator and every decoded byte of shipped isoface.png" $ do
        pixels 8 `shouldBe` sfmFlat generateSlopeFaceMaps
        reference ← readPixels "assets/textures/facemap/isoface.png"
        pixels 8 `shouldBe` reference

    forM_ [1..8] $ \level →
        it ("matches tracked level " ++ show level ++ " to the production Haskell output") $ do
            tracked ← readPixels ("assets/textures/facemap/isoface_level_" ++ show level ++ ".png")
            pixels level `shouldBe` tracked
