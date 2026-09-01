-- | Signed 32-bit world cylinder coordinates in the vertex (#2019).
--
--   Before this, a vertex carried @(u,v)@ packed into ONE 'Word32' as
--   two 'Word16' halves. That round-tripped negatives correctly but
--   WRAPPED — silently, with no clamp, warning or refusal — the moment
--   @|u|@ or @|v|@ passed 32767, i.e. around worldSize 2048; the only
--   symptom would have been geometry lit as though it sat at a
--   different longitude. #2017 commits to a 1024 guarantee with map
--   addressing designed through 8192, so the carrier is now two whole
--   signed 32-bit components ('WorldUV', @FORMAT_R32G32_SINT@, GLSL
--   @ivec2@).
--
--   The shader half cannot be checked without a GPU. Everything on the
--   HOST side of that boundary can be, and is, here: the carrier's
--   exactness across its whole domain, its refusal to wrap a value it
--   cannot hold, @v@'s survival through every path that copies or
--   rebuilds a vertex, and the zoom map's facing composition at
--   worldSize 8192.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "world vertex coordinates"'@.
module Test.Headless.Graphics.WorldVertexCoords (spec) where

import UPrelude
import Control.Exception (evaluate)
import Foreign.Marshal.Alloc (allocaBytes)
import Data.Int (Int32)
import Test.Hspec

import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex
    ( Vertex(..), Vec2(..), Vec4(..), WorldUV(..), QuadPayload(..)
    , mkVertex, mkVertexWorld, mkWorldUV, tileWorldUV, worldUVNone
    , quadVertices, rectCorners, fullQuadUV
    , noFaceMapVertexId, vertexTotalSize )
import Engine.Scene.Base (LayerId(..))
import Engine.Scene.Types (SortableQuad(..))
import World.Chunk.Types (chunkSize)
import World.Render.Zoom.Bake (zoomQuadWorldUVs)
import World.Render.Zoom.Quads (emitQuad)
import World.Render.Zoom.Types (BakedZoomEntry(..))

-- * The old boundary, and the world sizes that retire it

-- | The largest magnitude the retired 'Word16' halves could represent.
--   Every "large" fixture below is checked against this, so a fixture
--   that quietly stopped being large would fail rather than pass
--   vacuously.
packedHalfLimit ∷ Int
packedHalfLimit = 32767

-- | What the pre-#2019 encoding did to one component: truncate to 16
--   bits, then sign-restore. Reproduced here (it no longer exists in
--   the tree) so the large fixtures can assert they carry a value the
--   old carrier would have turned into a DIFFERENT tile, rather than
--   merely a big one.
packedHalfWrap ∷ Int → Int
packedHalfWrap n =
    let low = n `mod` 65536
    in if low ≥ 32768 then low - 65536 else low

-- | The map addressing #2017 designs through.
worldSizeMax ∷ Int
worldSizeMax = 8192

-- | Tiles across a worldSize-8192 world: @8192 * chunkSize@.
worldTilesMax ∷ Int
worldTilesMax = worldSizeMax * chunkSize

-- | A chunk origin at the far positive edge of a worldSize-8192 world.
--   @u = v = 131056@ — four times the retired limit.
farChunk ∷ (Int, Int)
farChunk = (worldTilesMax - chunkSize, 0)

-- | The same world's far NEGATIVE-u corner: @u = -131056@, @v = 131056@.
farNegChunk ∷ (Int, Int)
farNegChunk = (0, worldTilesMax - chunkSize)

-- | A chunk at the origin — the small-world case every facing
--   expectation is also stated at, so the large-coordinate examples are
--   comparing against a known-good shape rather than only against
--   themselves.
originChunk ∷ (Int, Int)
originChunk = (0, 0)

-- * Facing expectations, derived by hand

-- | The four rectangle-corner @(u,v)@ pairs 'zoomQuadWorldUVs' must
--   produce for the chunk based at @(gx,gy)@ under one facing.
--
--   Derived here from the DEFINITIONS of @applyFacing@ \/
--   @unapplyFacing@ and written out per facing, never by calling the
--   implementation: the chunk's four grid corners have @u@ in
--   @[u0-chunkSize, u0+chunkSize]@ and @v@ in @[v0, v0+2*chunkSize]@,
--   the bake rotates that box into screen axes, takes its min\/max, and
--   rotates each rectangle corner back. Each facing permutes the same
--   four @(uMin|uMax, vMin|vMax)@ pairs differently, and getting the
--   permutation wrong is exactly the bug this pins.
expectedCorners ∷ CameraFacing → (Int, Int) → [(Int, Int)]
expectedCorners facing (gx, gy) =
    let u0   = gx - gy
        v0   = gx + gy
        uMin = u0 - chunkSize
        uMax = u0 + chunkSize
        vMin = v0
        vMax = v0 + 2 * chunkSize
    in case facing of
        FaceSouth → [ (uMin, vMin), (uMax, vMin), (uMax, vMax), (uMin, vMax) ]
        FaceWest  → [ (uMax, vMin), (uMax, vMax), (uMin, vMax), (uMin, vMin) ]
        FaceNorth → [ (uMax, vMax), (uMin, vMax), (uMin, vMin), (uMax, vMin) ]
        FaceEast  → [ (uMin, vMax), (uMin, vMin), (uMax, vMin), (uMax, vMax) ]

quadList ∷ (α, α, α, α) → [α]
quadList (a, b, c, d) = [a, b, c, d]

bakedCorners ∷ CameraFacing → (Int, Int) → [WorldUV]
bakedCorners facing (gx, gy) = quadList (zoomQuadWorldUVs facing gx gy)

expectedUVs ∷ CameraFacing → (Int, Int) → [WorldUV]
expectedUVs facing base = map (uncurry mkWorldUV) (expectedCorners facing base)

-- | A corner's coordinates relative to its own chunk's @(u0,v0)@ — the
--   facing-shape invariant with the base subtracted out, so a
--   worldSize-8192 chunk and an origin chunk are directly comparable.
relativeCorners ∷ CameraFacing → (Int, Int) → [(Int, Int)]
relativeCorners facing base@(gx, gy) =
    [ (fromIntegral (wuvU w) - (gx - gy), fromIntegral (wuvV w) - (gx + gy))
    | w ← bakedCorners facing base ]

allFacings ∷ [CameraFacing]
allFacings = [FaceSouth, FaceWest, FaceNorth, FaceEast]

-- * A vertex-copying fixture

-- | A @(u,v)@ neither component of which fits the retired halves, and
--   whose two components differ — so a transposed pair, a dropped @v@,
--   or a wrapped component are all separately visible.
bigUV ∷ WorldUV
bigUV = mkWorldUV 1234567 (-7654321)

payloadWith ∷ WorldUV → QuadPayload
payloadWith wuv = QuadPayload
    { qpTint      = Vec4 0.25 0.5 0.75 1
    , qpAtlasSlot = 3
    , qpFaceMap   = noFaceMapVertexId
    , qpFlags     = 0
    , qpWorldUV   = wuv
    }

-- | A baked zoom entry whose four corners carry four DISTINCT
--   coordinates, the way a real bake's per-corner longitude gradient
--   does — so 'emitQuad' cannot pass by broadcasting one of them.
bakedEntry ∷ [WorldUV] → BakedZoomEntry
bakedEntry [w0, w1, w2, w3] = BakedZoomEntry
    { bzeChunkX = 0, bzeChunkY = 0
    , bzeDrawX = 10, bzeDrawY = 20
    , bzeWidth = 4, bzeHeight = 6
    , bzeSortKey = 0
    , bzeV0 = corner w0 (Vec2 10 20)
    , bzeV1 = corner w1 (Vec2 14 20)
    , bzeV2 = corner w2 (Vec2 14 26)
    , bzeV3 = corner w3 (Vec2 10 26)
    , bzeTexture = TextureHandle 0
    , bzeIsOcean = False, bzeHasLava = False, bzeElev = 0
    }
  where
    corner w p = mkVertexWorld w p (Vec2 0 0) (Vec4 1 1 1 1) 0 noFaceMapVertexId
bakedEntry ws = error ("bakedEntry: expected 4 corners, got " ⧺ show (length ws))

-- | Poke a vertex into a scratch buffer at the real stride and read it
--   back — the same trip the vertex buffer makes.
throughStorable ∷ Vertex → IO Vertex
throughStorable v = allocaBytes vertexTotalSize $ \p → poke p v ≫ peek p

-- * Spec

spec ∷ Spec
spec = do
    carrierSpec
    vertexSpec
    copyingSpec
    facingSpec

-- | Requirements 1 and 3, plus the review's signed-32 domain
--   clarification: exactness over the WHOLE carrier, and a refusal
--   rather than a wrap outside it.
carrierSpec ∷ Spec
carrierSpec = describe "the signed 32-bit carrier" $ do

    it "keeps coordinates far beyond the retired ±32767 boundary, both signs" $ do
        let cases = [ (131056, 131056), (-131056, 131056)
                    , (262144, -262144), (32768, -32768) ]
        [ (fromIntegral (wuvU w), fromIntegral (wuvV w))
          | (u, v) ← cases, let w = mkWorldUV u v ] `shouldBe` cases

    it "keeps values the retired packed halves would have turned into a\
       \ DIFFERENT tile" $ do
        let u = fst farChunk - chunkSize    -- 131040
            w = mkWorldUV u 0
        abs u `shouldSatisfy` (> packedHalfLimit)
        packedHalfWrap u `shouldNotBe` u
        fromIntegral (wuvU w) `shouldBe` u

    it "keeps both signed 32-bit boundaries exactly" $ do
        let hi = fromIntegral (maxBound ∷ Int32)
            lo = fromIntegral (minBound ∷ Int32)
        mkWorldUV hi lo `shouldBe` WorldUV maxBound minBound
        mkWorldUV lo hi `shouldBe` WorldUV minBound maxBound

    it "REFUSES a coordinate one past either boundary instead of wrapping it" $ do
        let hi = fromIntegral (maxBound ∷ Int32) + 1
            lo = fromIntegral (minBound ∷ Int32) - 1
        evaluate (mkWorldUV hi 0) `shouldThrow` anyErrorCall
        evaluate (mkWorldUV 0 hi) `shouldThrow` anyErrorCall
        evaluate (mkWorldUV lo 0) `shouldThrow` anyErrorCall
        evaluate (mkWorldUV 0 lo) `shouldThrow` anyErrorCall

    it "derives the cylinder pair from a tile the same way, and refuses the\
       \ same way" $ do
        tileWorldUV 200000 (-50000) `shouldBe` WorldUV 250000 150000
        evaluate (tileWorldUV (fromIntegral (maxBound ∷ Int32)) (-1))
            `shouldThrow` anyErrorCall

-- | Requirements 3 and 5 at the vertex itself: the pair survives the
--   binary trip through the vertex buffer, and @v@ survives it even
--   though the shader reads only @u@.
vertexSpec ∷ Spec
vertexSpec = describe "a vertex's world coordinates" $ do

    it "round-trip through the vertex buffer's own encoding" $ do
        let v = mkVertexWorld bigUV (Vec2 1 2) (Vec2 0 0) (Vec4 1 1 1 1) 5 6
        back ← throughStorable v
        worldUV back `shouldBe` bigUV
        back `shouldBe` v

    it "keep v exactly, though no consumer reads it" $ do
        let uvs = [ WorldUV 0 131056, WorldUV 0 (-131056)
                  , WorldUV 0 maxBound, WorldUV 0 minBound ]
        backs ← mapM (\w → throughStorable
                             (mkVertexWorld w (Vec2 0 0) (Vec2 0 0)
                                            (Vec4 1 1 1 1) 0 0)) uvs
        map (wuvV . worldUV) backs `shouldBe` map wuvV uvs

    it "default to the origin for a vertex with no world position" $
        worldUV (mkVertex (Vec2 0 0) (Vec2 0 0) (Vec4 1 1 1 1) 0 0)
            `shouldBe` worldUVNone

-- | The review's third correction and third spec addition: the paths
--   that COPY or REBUILD a vertex must carry both components through
--   untouched, not just the paths that create one.
copyingSpec ∷ Spec
copyingSpec = describe "every path that copies a vertex" $ do

    it "gives all four corners of a quad the payload's exact (u,v)" $ do
        let (a, b, c, d) = quadVertices (rectCorners (Vec2 0 0) (Vec2 8 8))
                                        fullQuadUV (payloadWith bigUV)
        map worldUV [a, b, c, d] `shouldBe` replicate 4 bigUV

    it "leaves both components alone through the zoom map's emitQuad" $ do
        -- Four DISTINCT per-corner coordinates, as a real bake emits.
        let corners = expectedUVs FaceSouth farChunk
            q = emitQuad (bakedEntry corners) (Vec4 0.1 0.2 0.3 0.4)
                         37 41 (LayerId 3)
        map worldUV [sqV0 q, sqV1 q, sqV2 q, sqV3 q] `shouldBe` corners
        -- and the translation it exists to perform still happened.
        map (x . pos) [sqV0 q, sqV1 q] `shouldBe` [37, 41]

-- | Requirement 7 and the review's second spec addition: the facing
--   composition at worldSize 8192, checked against independently
--   derived exact corner identities and against the small-world shape.
facingSpec ∷ Spec
facingSpec = describe "the four map facings" $ do

    it "place every corner exactly, at the origin" $
        mapM_ (\f → bakedCorners f originChunk `shouldBe` expectedUVs f originChunk)
              allFacings

    it "place every corner exactly, at worldSize 8192's far edge" $
        mapM_ (\f → bakedCorners f farChunk `shouldBe` expectedUVs f farChunk)
              allFacings

    it "place every corner exactly, at worldSize 8192's far negative-u edge" $
        mapM_ (\f → bakedCorners f farNegChunk
                        `shouldBe` expectedUVs f farNegChunk)
              allFacings

    it "hold the SAME facing invariant at worldSize 8192 as at the origin" $
        mapM_ (\f → mapM_ (\base → relativeCorners f base
                                       `shouldBe` relativeCorners f originChunk)
                          [farChunk, farNegChunk])
              allFacings

    it "exercise coordinates the retired carrier could not have held" $ do
        let comps = [ c | f ← allFacings
                        , base ← [farChunk, farNegChunk]
                        , w ← bakedCorners f base
                        , c ← [fromIntegral (wuvU w), fromIntegral (wuvV w)] ]
        maximum (map abs comps) `shouldSatisfy` (> packedHalfLimit)
        filter (\c → packedHalfWrap c ≢ c) comps `shouldNotBe` []
