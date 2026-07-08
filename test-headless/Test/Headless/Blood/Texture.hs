{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Procedural blood texture generation tests (#606): determinism,
--   distinctness across descriptor fields, transparent background, and
--   bounded size — the pure contract 'Blood.Texture.generateBloodTexture'
--   makes for the debug surface and the real renderer. No engine boot
--   needed.
module Test.Headless.Blood.Texture (spec) where

import UPrelude
import qualified Data.ByteString as BS
import Test.Hspec
import Blood.Types
import Blood.Texture

baseDescriptor ∷ BloodTextureDescriptor
baseDescriptor = BloodTextureDescriptor
    { btdId         = BloodTextureId 1
    , btdStyle      = StylePool
    , btdWoundKind  = "stab"
    , btdSeverity   = SeverityModerate
    , btdFootprint  = FootprintMedium
    , btdAnisotropy = AnisotropyNone
    , btdEdge       = EdgeModerate
    , btdSeed       = 7
    }

-- | Every RGBA pixel as a (r,g,b,a) tuple, for corner/shape checks.
pixels ∷ BloodTextureImage → [(Word8, Word8, Word8, Word8)]
pixels img = go (BS.unpack (btiPixels img))
  where
    go (r:g:b:a:rest) = (r, g, b, a) : go rest
    go _               = []

spec ∷ Spec
spec = do
    describe "generateBloodTexture / determinism" $ do
        it "the same descriptor produces byte-identical pixel data" $
            btiPixels (generateBloodTexture baseDescriptor)
                `shouldBe` btiPixels (generateBloodTexture baseDescriptor)

        it "the same descriptor produces the same hash" $
            bloodTextureHash (generateBloodTexture baseDescriptor)
                `shouldBe` bloodTextureHash (generateBloodTexture baseDescriptor)

    describe "generateBloodTexture / distinctness" $ do
        it "a different style produces a different hash" $
            bloodTextureHash (generateBloodTexture baseDescriptor)
                `shouldNotBe` bloodTextureHash
                    (generateBloodTexture baseDescriptor { btdStyle = StyleStreak })

        it "a different severity bucket produces a different hash" $
            bloodTextureHash (generateBloodTexture baseDescriptor)
                `shouldNotBe` bloodTextureHash
                    (generateBloodTexture baseDescriptor
                        { btdSeverity = SeverityCatastrophic })

        it "a different seed produces a different hash" $
            bloodTextureHash (generateBloodTexture baseDescriptor)
                `shouldNotBe` bloodTextureHash
                    (generateBloodTexture baseDescriptor { btdSeed = 99 })

        it "a different footprint bucket produces a different hash" $
            bloodTextureHash (generateBloodTexture baseDescriptor)
                `shouldNotBe` bloodTextureHash
                    (generateBloodTexture baseDescriptor
                        { btdFootprint = FootprintLarge })

    describe "generateBloodTexture / transparent background" $ do
        it "the four corner pixels are fully transparent" $ do
            let img = generateBloodTexture baseDescriptor
                w = btiWidth img
                h = btiHeight img
                corners = [(0, 0), (w - 1, 0), (0, h - 1), (w - 1, h - 1)]
                alphaAt (x, y) =
                    let (_, _, _, a) = pixels img !! (y * w + x)
                    in a
            map alphaAt corners `shouldBe` replicate 4 0

        it "has at least one non-transparent (blood-shaped) pixel" $ do
            let img = generateBloodTexture baseDescriptor
                anyOpaque = any (\(_, _, _, a) → a > 0) (pixels img)
            anyOpaque `shouldBe` True

        it "every fully-transparent pixel is also black (no color fringe)" $ do
            let img = generateBloodTexture baseDescriptor
                transparentIsBlack (r, g, b, a) = a ≢ 0 ∨ (r, g, b) ≡ (0, 0, 0)
            all transparentIsBlack (pixels img) `shouldBe` True

    describe "generateBloodTexture / bounded size" $ do
        it "dimensions match the footprint bucket's canvas size" $ do
            let sizes =
                    [ (fp, btiWidth (generateBloodTexture
                          baseDescriptor { btdFootprint = fp }))
                    | fp ← [FootprintSmall, FootprintMedium, FootprintLarge]
                    ]
            sizes `shouldBe`
                [ (fp, bloodTextureDim fp)
                | fp ← [FootprintSmall, FootprintMedium, FootprintLarge]
                ]

        it "never exceeds maxBloodTextureDim in either dimension" $ do
            let dims =
                    [ (btiWidth img, btiHeight img)
                    | fp ← [FootprintSmall, FootprintMedium, FootprintLarge]
                    , let img = generateBloodTexture
                                    baseDescriptor { btdFootprint = fp }
                    ]
            all (\(w, h) → w ≤ maxBloodTextureDim ∧ h ≤ maxBloodTextureDim) dims
                `shouldBe` True

        it "pixel buffer length matches width * height * 4" $ do
            let img = generateBloodTexture baseDescriptor
            BS.length (btiPixels img) `shouldBe` btiWidth img * btiHeight img * 4
