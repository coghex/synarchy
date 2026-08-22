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

-- | Every RGBA pixel as a (r,g,b,a) tuple, for the pinned sample
--   coordinates and the corner/shape checks.
pixels ∷ BloodTextureImage → [(Word8, Word8, Word8, Word8)]
pixels img = go (BS.unpack (btiPixels img))
  where
    go (r:g:b:a:rest) = (r, g, b, a) : go rest
    go _               = []

spec ∷ Spec
spec = do
    describe "generateBloodTexture / pinned output" $
        -- The golden below is the only absolute statement in this module
        -- about WHAT 'baseDescriptor' draws; everything else here is
        -- relative (distinctness) or structural (dimensions, bounds).
        -- Deliberately NOT pinned via 'bloodTextureHash': that is
        -- 'Data.Hashable.hash', and 'hashable' carries no version bound,
        -- so an absolute hash would move on a dependency bump with no
        -- texture change. These are the repository's own numbers.
        --
        -- Six samples across the 24x24 medium canvas, chosen so the set
        -- spans the splat rather than repeating the corners already
        -- pinned below: the fully-opaque centre, two interior body
        -- pixels either side of it, the left and top soft edges (where
        -- 'splatAlpha' is between 0 and 255, so an edge-roughness or
        -- softness change lands here first), and one fully transparent
        -- pixel OUTSIDE the splat but away from any corner, which pins
        -- the shape's extent.
        --
        -- If this fails, the generator's output changed. That is a
        -- deliberate VISUAL decision about how blood looks — look at the
        -- new texture and decide it is what you want, then update these
        -- numbers. It is not a value to regenerate until the suite goes
        -- green.
        it "draws the pinned RGBA values at named sample coordinates" $ do
            let img = generateBloodTexture baseDescriptor
                w   = btiWidth img
                ps  = pixels img
                at (x, y) = ps !! (y * w + x)
                samples =
                    [ (12, 12)  -- centre of the pool: fully opaque
                    , ( 8,  8)  -- interior body, upper-left of centre
                    , (16, 16)  -- interior body, lower-right of centre
                    , ( 6, 12)  -- left soft edge
                    , (12,  6)  -- top soft edge
                    , ( 2, 12)  -- outside the splat (not a corner)
                    ]
            map at samples `shouldBe`
                [ (150, 9, 8, 255)
                , (150, 9, 8, 196)
                , (150, 9, 8, 171)
                , (150, 9, 8,  51)
                , (150, 9, 8,  19)
                , (  0, 0, 0,   0)
                ]

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
