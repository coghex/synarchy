{-# LANGUAGE Strict #-}
-- | Pure coverage for flat-surface Till rendering and its authored alpha.
module Test.Headless.World.Render.DesignationFaceMap (spec) where

import UPrelude
import Test.Hspec
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Codec.Picture as JP
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import Numeric (showHex)

import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex
    ( Vec2(..), Vec4(..), Vertex, faceMapId, mkVertexWorld, noFaceMapVertexId
    , tileWorldUV )
import Engine.Scene.Base (LayerId)
import Engine.Scene.Types (SortableQuad(..))
import World.Grid
    (applyFacing, gridToScreen, tileHeight, tileSideHeight, tileWidth, worldLayer)
import World.Render.Textures.Types (WorldTextures(..), defaultWorldTextures)
import World.Render.TileQuads (worldCursorToQuad, worldFlatCursorToQuad)

spec ∷ Spec
spec = do
    describe "designation facemap" $ do
        it "keeps ordinary cursor output field-for-field on the isometric map" $ do
            quadSnapshot ordinaryQuad `shouldBe` quadSnapshot expectedOrdinaryQuad
            sqSortKey ordinaryQuad
                `shouldSatisfy` withinSortNudge (sqSortKey expectedOrdinaryQuad)

        it "uses the neutral face map for flat Till surfaces in every facing" $
            forM_ [FaceSouth, FaceWest, FaceNorth, FaceEast] $ \facing →
                faceMaps (worldFlatCursorToQuad lookupSlot lookupFaceMap textures
                            facing 2 3 12 8 16 1.0 (0, 0) cursorTexture)
                    `shouldBe` replicate 4 noFaceMapVertexId

    describe "Till source alpha" $ do
        it "is intrinsically the same 96x64 flat diamond for marker and soil" $ do
            mask ← readRGBA "assets/textures/facemap/vegface.png"
            marker ← readRGBA "assets/textures/ui/hud/utility/till_designate.png"
            soil ← readRGBA "assets/textures/vegetation/tilled_soil/frame_000.png"
            map imageSize [mask, marker, soil]
                `shouldBe` replicate 3 (96, 64)
            alphaMismatches mask marker soil `shouldBe` []

        it "keeps the designation as one flat translucent colour" $ do
            marker ← readRGBA "assets/textures/ui/hud/utility/till_designate.png"
            visiblePixels marker
                `shouldBe` Set.singleton (JP.PixelRGBA8 232 126 38 88)

        it "preserves the actual tilled-soil RGB plane byte-for-byte" $ do
            soil ← readRGBA "assets/textures/vegetation/tilled_soil/frame_000.png"
            rgbDigest soil `shouldBe`
                "ffd42d55278fc819a3c45eb63a7f8519351209c10438ec30701a9885ef01e931"

cursorTexture, isoFaceMapTexture ∷ TextureHandle
cursorTexture = TextureHandle 11
isoFaceMapTexture = TextureHandle 29

textures ∷ WorldTextures
textures = defaultWorldTextures { wtIsoFaceMap = isoFaceMapTexture }

lookupSlot ∷ TextureHandle → Int
lookupSlot (TextureHandle handle) = handle + 100

lookupFaceMap ∷ TextureHandle → Float
lookupFaceMap (TextureHandle handle) = fromIntegral handle

isoFaceMapId ∷ Float
isoFaceMapId = lookupFaceMap isoFaceMapTexture

ordinaryQuad ∷ SortableQuad
ordinaryQuad = worldCursorToQuad lookupSlot lookupFaceMap textures
    FaceSouth 2 3 12 8 16 1.0 (0, 0) cursorTexture

-- The pre-flat-seam cursor formula, restated as an exact fixture so Mine's
-- geometry, UVs, tint, texture, sort order, flags and world coordinates cannot
-- drift while Till takes the new helper.
expectedOrdinaryQuad ∷ SortableQuad
expectedOrdinaryQuad =
    let gx = 2
        gy = 3
        surfZ = 12
        zSlice = 8
        (drawX, rawY) = gridToScreen FaceSouth gx gy
        relativeZ = surfZ - zSlice
        drawY = rawY - fromIntegral relativeZ * tileSideHeight
        (fa, fb) = applyFacing FaceSouth gx gy
        sortKey = fromIntegral (fa + fb)
                + fromIntegral relativeZ * 0.001 + 0.0004
        tint = Vec4 1.0 1.0 1.0 0.7
        atlas = fromIntegral (lookupSlot cursorTexture)
        wuv = tileWorldUV gx gy
        vertex position uv = mkVertexWorld wuv position uv tint atlas isoFaceMapId
    in SortableQuad
        { sqSortKey = sortKey
        , sqV0 = vertex (Vec2 drawX drawY) (Vec2 0 0)
        , sqV1 = vertex (Vec2 (drawX + tileWidth) drawY) (Vec2 1 0)
        , sqV2 = vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight))
                        (Vec2 1 1)
        , sqV3 = vertex (Vec2 drawX (drawY + tileHeight)) (Vec2 0 1)
        , sqTexture = cursorTexture
        , sqLayer = worldLayer
        }

-- | Everything about the quad that is compared EXACTLY. The sort key is
--   deliberately absent: it is the one field both sides compute by
--   float arithmetic rather than carry, and GHC folds the fixture's
--   copy of that expression at a different precision than it evaluates
--   the production one at, so exact equality there was pinning an
--   optimisation decision rather than the cursor's behaviour. It is
--   checked separately, by 'withinSortNudge'.
quadSnapshot ∷ SortableQuad → ([Vertex], TextureHandle, LayerId)
quadSnapshot quad =
    ( [sqV0 quad, sqV1 quad, sqV2 quad, sqV3 quad]
    , sqTexture quad
    , sqLayer quad
    )

-- | The sort key agrees to far better than the nudge it encodes.
--
--   That nudge is what the key is FOR: a cursor sorts at its tile's key
--   plus 0.0004, after terrain (+0.0) and before fluid (+0.0005). A
--   tolerance of 1e-5 is two orders of magnitude inside the 1e-4 gap to
--   either neighbour, so a drift that could reorder anything fails
--   while a last-bit float difference does not.
withinSortNudge ∷ Float → Float → Bool
withinSortNudge expected actual = abs (actual - expected) < 1.0e-5

faceMaps ∷ SortableQuad → [Float]
faceMaps quad = map faceMapId [sqV0 quad, sqV1 quad, sqV2 quad, sqV3 quad]

readRGBA ∷ FilePath → IO (JP.Image JP.PixelRGBA8)
readRGBA path = do
    result ← JP.readImage path
    case result of
        Left err → fail ("could not decode " ⧺ path ⧺ ": " ⧺ err)
        Right image → pure (JP.convertRGBA8 image)

imageSize ∷ JP.Image pixel → (Int, Int)
imageSize image = (JP.imageWidth image, JP.imageHeight image)

alphaMismatches ∷ JP.Image JP.PixelRGBA8 → JP.Image JP.PixelRGBA8
                → JP.Image JP.PixelRGBA8 → [(Int, Int)]
alphaMismatches mask marker soil =
    [ (x, y)
    | y ← [0 .. JP.imageHeight mask - 1]
    , x ← [0 .. JP.imageWidth mask - 1]
    , let JP.PixelRGBA8 _ _ _ maskAlpha = JP.pixelAt mask x y
          JP.PixelRGBA8 _ _ _ markerAlpha = JP.pixelAt marker x y
          JP.PixelRGBA8 _ _ _ soilAlpha = JP.pixelAt soil x y
    , (markerAlpha > 0) ≢ (maskAlpha > 0) ∨ soilAlpha ≢ maskAlpha
    ]

visiblePixels ∷ JP.Image JP.PixelRGBA8 → Set.Set JP.PixelRGBA8
visiblePixels image = Set.fromList
    [ pixel
    | y ← [0 .. JP.imageHeight image - 1]
    , x ← [0 .. JP.imageWidth image - 1]
    , let pixel@(JP.PixelRGBA8 _ _ _ alpha) = JP.pixelAt image x y
    , alpha > 0
    ]

rgbDigest ∷ JP.Image JP.PixelRGBA8 → String
rgbDigest image = concatMap byteHex . BS.unpack . SHA256.hash . BS.pack $
    [ channel
    | y ← [0 .. JP.imageHeight image - 1]
    , x ← [0 .. JP.imageWidth image - 1]
    , let JP.PixelRGBA8 red green blue _ = JP.pixelAt image x y
    , channel ← [red, green, blue]
    ]
  where
    byteHex byte = case showHex byte "" of
        [digit] → ['0', digit]
        digits  → digits
