module Test.Headless.World.Render.SlopeFacing (spec) where

import UPrelude
import Test.Hspec
import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Graphics.Camera (CameraFacing(..), rotateCW)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..))
import Engine.Scene.Types (SortableQuad(..))
import World.Render.Textures
import World.Render.Textures.Types (WorldTextures(..), defaultWorldTextures)
import World.Render.TileQuads (tileToQuad, vegQuadWithTexture)
import World.Render.QuadContext
import World.Types (Tile(..))

textures ∷ WorldTextures
textures = defaultWorldTextures
    { wtIsoFaceMap = TextureHandle 100
    , wtSlopeFaceMapN = TextureHandle 101
    , wtSlopeFaceMapE = TextureHandle 102
    , wtSlopeFaceMapNE = TextureHandle 103
    , wtSlopeFaceMapS = TextureHandle 104
    , wtSlopeFaceMapNS = TextureHandle 105
    , wtSlopeFaceMapES = TextureHandle 106
    , wtSlopeFaceMapNES = TextureHandle 107
    , wtSlopeFaceMapW = TextureHandle 108
    , wtSlopeFaceMapNW = TextureHandle 109
    , wtSlopeFaceMapEW = TextureHandle 110
    , wtSlopeFaceMapNEW = TextureHandle 111
    , wtSlopeFaceMapSW = TextureHandle 112
    , wtSlopeFaceMapNSW = TextureHandle 113
    , wtSlopeFaceMapESW = TextureHandle 114
    , wtSlopeFaceMapNESW = TextureHandle 115
    , wtVegFaceMap = TextureHandle 200
    , wtVegSlopeFaceMapN = TextureHandle 201
    , wtVegSlopeFaceMapE = TextureHandle 202
    , wtVegSlopeFaceMapNE = TextureHandle 203
    , wtVegSlopeFaceMapS = TextureHandle 204
    , wtVegSlopeFaceMapNS = TextureHandle 205
    , wtVegSlopeFaceMapES = TextureHandle 206
    , wtVegSlopeFaceMapNES = TextureHandle 207
    , wtVegSlopeFaceMapW = TextureHandle 208
    , wtVegSlopeFaceMapNW = TextureHandle 209
    , wtVegSlopeFaceMapEW = TextureHandle 210
    , wtVegSlopeFaceMapNEW = TextureHandle 211
    , wtVegSlopeFaceMapSW = TextureHandle 212
    , wtVegSlopeFaceMapNSW = TextureHandle 213
    , wtVegSlopeFaceMapESW = TextureHandle 214
    , wtVegSlopeFaceMapNESW = TextureHandle 215
    }

facings ∷ [CameraFacing]
facings = [FaceSouth, FaceWest, FaceNorth, FaceEast]

-- Independent orientation witnesses for each world N/E/S/W bit.
-- In particular E's right corner becomes back, left, front as the view turns.
expectedMask ∷ CameraFacing → Word8 → Int
expectedMask facing bits = sum
    [target | (worldBit,target) ← zip [1,2,4,8] targets, bits ⌃ worldBit ≢ 0]
  where
    targets = case facing of
        FaceSouth → [1,2,4,8]
        FaceWest → [8,1,2,4]
        FaceNorth → [4,8,1,2]
        FaceEast → [2,4,8,1]

quadContext ∷ CameraFacing → QuadContext
quadContext f = QuadContext (fromIntegral ∘ toInt) (fromIntegral ∘ toInt)
    textures f (ZSlice 0) (EffectiveDepth 8) 1 (0,0)

maskIds ∷ SortableQuad → [Float]
maskIds q = map faceMapId [sqV0 q,sqV1 q,sqV2 q,sqV3 q]

spec ∷ Spec
spec = do
    it "rotates all 16 terrain and vegetation masks through all four facings" $
        forM_ facings $ \f → forM_ [0..15] $ \bits → do
            let k = expectedMask f bits
            getTileFaceMapTexture textures f 1 bits
                `shouldBe` TextureHandle (fromIntegral (100+k))
            getVegFaceMapTexture textures f bits
                `shouldBe` TextureHandle (fromIntegral (200+k))
    it "applies the rotation at the real terrain quad boundary" $
        forM_ facings $ \f → forM_ [0..15] $ \bits → do
            let q = tileToQuad (quadContext f) (WorldX 16) (WorldY (-3))
                      (WorldZ (-2)) (Tile 1 bits) Nothing False
            maskIds q `shouldBe` replicate 4 (fromIntegral (100+expectedMask f bits))
    it "keeps plant and crop overlays aligned through their shared quad boundary" $
        forM_ facings $ \f → forM_ [0..15] $ \bits → do
            let result = vegQuadWithTexture (fromIntegral ∘ toInt) (fromIntegral ∘ toInt)
                           textures f 16 (-3) (-2) (TextureHandle 17) bits 0 8 1 (0,0)
            fmap maskIds result `shouldBe`
                Just (replicate 4 (fromIntegral (200+expectedMask f bits)))
    it "preserves the flat fallback for every invalid mask ID" $
        forM_ facings $ \f → forM_ [16..255] $ \bits → do
            getTileFaceMapTexture textures f 1 bits `shouldBe` TextureHandle 100
            getVegFaceMapTexture textures f bits `shouldBe` TextureHandle 200
    it "returns to the original masks after four turns without changing world flags" $
        forM_ facings $ \f → forM_ [0..15] $ \bits → do
            let turned = iterate rotateCW f !! 4
            getTileFaceMapTexture textures turned 1 bits
                `shouldBe` getTileFaceMapTexture textures f 1 bits
    it "keeps missing vegetation absent at every facing" $
        forM_ facings $ \f →
            isNothing (vegQuadWithTexture (fromIntegral ∘ toInt) (fromIntegral ∘ toInt)
                       textures f 0 0 0 (TextureHandle 0) 2 0 8 1 (0,0))
                `shouldBe` True
