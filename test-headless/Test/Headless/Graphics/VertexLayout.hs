{-# LANGUAGE OverloadedRecordDot #-}
-- | The world vertex's binary layout (#983, pinned by #1869, widened
--   by #2019).
--
--   @Engine.Graphics.Vulkan.Vertex@ states the pipeline's stride and
--   attribute offsets as literals, and its own Haddock records that
--   nothing checks them against
--   "Engine.Graphics.Vulkan.Types.Vertex"'s constants — the ones the
--   'Storable' instance actually peeks and pokes at — at compile time.
--   A disagreement there is silent: the shader reads whatever bytes the
--   offsets name, and the only symptom is wrong pixels on a machine
--   with a GPU.
--
--   This closes that with no device. Every expectation below is an
--   INDEPENDENT literal rather than a value recomputed from the
--   implementation's own constants, because a test that derived them
--   the same way would agree with any mistake.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Graphics.VertexLayout"'@.
module Test.Headless.Graphics.VertexLayout (spec) where

import UPrelude
import Data.Int (Int32)
import Test.Hspec
import qualified Data.Vector as V
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
-- Record-dot access throughout: @vulkan@ gives several structs in
-- 'Vulkan.Core10.Pipeline' fields of the same name, so a plain selector
-- is ambiguous.
import Vulkan.Core10
    ( VertexInputBindingDescription(..), VertexInputAttributeDescription(..)
    , VertexInputRate(..), Format(..) )
import Engine.Graphics.Vulkan.Vertex
    (getVertexBindingDescription, getVertexAttributeDescriptions)
import Engine.Graphics.Vulkan.Types.Vertex
    ( Vertex(..), Vec2(..), Vec4(..), WorldUV(..)
    , vertexPositionOffset, vertexTexCoordOffset, vertexColorOffset
    , vertexAtlasIdOffset, vertexFaceMapIdOffset, vertexRenderFlagsOffset
    , vertexWorldUVOffset, vertexSolarPageOffset, vertexTotalSize )

-- | Every attribute's offset, in @location@ order. Literals.
expectedOffsets ∷ [Int]
expectedOffsets = [0, 8, 16, 32, 36, 40, 44, 52]

-- | The stride the pipeline declares, and what @sizeOf Vertex@ must be.
expectedStride ∷ Int
expectedStride = 56

-- | Every attribute's format, in @location@ order. Literals.
expectedFormats ∷ [Format]
expectedFormats =
    [ FORMAT_R32G32_SFLOAT        -- position
    , FORMAT_R32G32_SFLOAT        -- texCoord
    , FORMAT_R32G32B32A32_SFLOAT  -- color
    , FORMAT_R32_SFLOAT           -- atlas id
    , FORMAT_R32_SFLOAT           -- face-map id
    , FORMAT_R32_UINT             -- render flags
    , FORMAT_R32G32_SINT          -- signed world (u,v), #2019
    , FORMAT_R32_UINT             -- solar page slot
    ]

-- | A vertex whose every field is distinguishable from every other, so
--   a swapped or shifted member cannot compare equal by luck.
sampleVertex ∷ Vertex
sampleVertex = Vertex
    { pos         = Vec2 1.5 2.5
    , tex         = Vec2 3.5 4.5
    , color       = Vec4 5.5 6.5 7.5 8.5
    , atlasId     = 9.5
    , faceMapId   = 10.5
    , renderFlags = 0xDEADBEEF
    , worldUV     = WorldUV 0x1234CDEF (-0x5678ABCD)
    , solarPage   = 7
    }

-- | Byte the scratch buffer is pre-filled with, so an unwritten byte is
--   distinguishable from a written one. Chosen not to occur anywhere in
--   'sampleVertex''s own encoding.
sentinel ∷ Word8
sentinel = 0xAB

withScratch ∷ (Ptr Vertex → IO α) → IO α
withScratch act = allocaBytes expectedStride $ \p → do
    fillBytes p sentinel expectedStride
    act p

attributes ∷ [VertexInputAttributeDescription]
attributes = V.toList getVertexAttributeDescriptions

spec ∷ Spec
spec = do
    describe "the Storable instance" $ do
        it "reports the stride as its size, without forcing its argument" $ do
            -- Data.Vector.Storable calls `sizeOf undefined`, and these
            -- modules are compiled with Strict.
            sizeOf (undefined ∷ Vertex) `shouldBe` expectedStride
            alignment (undefined ∷ Vertex) `shouldBe` 4

        it "round-trips every field through poke then peek" $ do
            back ← withScratch $ \p → poke p sampleVertex ≫ peek p
            back `shouldBe` sampleVertex

        it "writes each field at its own literal offset" $ do
            (v0, v1, c, a, f, rf, wu, wv, sp) ← withScratch $ \p → do
                poke p sampleVertex
                -- The two world components are read as INDEPENDENT
                -- signed 32-bit values at 44 and 48 — the shape
                -- FORMAT_R32G32_SINT names — rather than as one word.
                (,,,,,,,,) ⊚ (peekByteOff p 0  ∷ IO Vec2)
                           <*> (peekByteOff p 8  ∷ IO Vec2)
                           <*> (peekByteOff p 16 ∷ IO Vec4)
                           <*> (peekByteOff p 32 ∷ IO Float)
                           <*> (peekByteOff p 36 ∷ IO Float)
                           <*> (peekByteOff p 40 ∷ IO Word32)
                           <*> (peekByteOff p 44 ∷ IO Int32)
                           <*> (peekByteOff p 48 ∷ IO Int32)
                           <*> (peekByteOff p 52 ∷ IO Word32)
            (v0, v1) `shouldBe` (Vec2 1.5 2.5, Vec2 3.5 4.5)
            c `shouldBe` Vec4 5.5 6.5 7.5 8.5
            (a, f) `shouldBe` (9.5, 10.5)
            (rf, wu, wv, sp) `shouldBe` (0xDEADBEEF, 0x1234CDEF, -0x5678ABCD, 7)

        it "leaves no unwritten byte inside the stride" $ do
            bytes ← withScratch $ \p → do
                poke p sampleVertex
                peekArray expectedStride (castPtr p ∷ Ptr Word8)
            bytes `shouldNotSatisfy` elem sentinel

    describe "the layout constants" $
        it "name exactly the offsets the Storable instance uses" $ do
            [ vertexPositionOffset, vertexTexCoordOffset, vertexColorOffset
              , vertexAtlasIdOffset, vertexFaceMapIdOffset
              , vertexRenderFlagsOffset, vertexWorldUVOffset
              , vertexSolarPageOffset ] `shouldBe` expectedOffsets
            vertexTotalSize `shouldBe` expectedStride

    describe "the pipeline's binding description" $ do
        it "declares the stride the Storable instance writes" $ do
            let bd = getVertexBindingDescription ∷ VertexInputBindingDescription
            bd.binding `shouldBe` 0
            fromIntegral bd.stride `shouldBe` expectedStride
            bd.inputRate `shouldBe` VERTEX_INPUT_RATE_VERTEX

    describe "the pipeline's attribute descriptions" $ do
        it "cover every field, on binding 0, in location order" $ do
            map (\a → a.location) attributes `shouldBe` [0 .. 7]
            map (\a → a.binding) attributes `shouldBe` replicate 8 0

        it "place each attribute at its Storable offset" $
            map (\a → fromIntegral a.offset) attributes `shouldBe` expectedOffsets

        it "declare the format each field is actually stored as" $
            map (\a → a.format) attributes `shouldBe` expectedFormats
