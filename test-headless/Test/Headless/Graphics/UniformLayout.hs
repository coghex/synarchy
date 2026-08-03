-- | The uniform buffer object's binary layout (#1072).
--
--   #1072 replaced a hand-written 'Storable' instance carrying literal
--   byte offsets, and four hand-written GLSL blocks, with one enumeration
--   ("Engine.Graphics.Vulkan.Uniform.Layout") that both are derived from.
--   That was meant to be REPRESENTATION-PRESERVING: the same 14 members at
--   the same offsets, so a running frame produces identical output.
--
--   Rendering itself needs a GPU, and the builds only prove the shaders
--   compile — neither checks an offset. This does, without a device: it
--   reproduces the pre-#1072 instance's writes verbatim in 'legacyPoke'
--   and diffs the payload byte for byte, so a generator that quietly
--   moved a member fails here rather than in a frame nobody automated
--   can look at.
module Test.Headless.Graphics.UniformLayout (spec) where

import UPrelude
import Test.Hspec
import Data.List (isInfixOf)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import Linear (V4(..), M44)
import Engine.Graphics.Vulkan.Types (UniformBufferObject(..))
import Engine.Graphics.Vulkan.Uniform.Layout
  ( UBOMember(..), uboMembers, uboMemberOffsets, uboPayloadSize
  , uboBaseAlignment, uboStd140Size, uboGlslBlock, glslTypeName )

-- | The offsets every shader shipped before #1072 was compiled against.
--   Written out as literals ON PURPOSE: a test that recomputed them from
--   the same rules the implementation uses would agree with any mistake.
legacyOffsets ∷ [Int]
legacyOffsets = [0, 64, 128, 192, 256, 320, 324, 328, 332, 336, 340, 344, 348, 352]

-- | Bytes the members occupy, end of the last one. The std140 size adds
--   trailing padding on top of this; the PAYLOAD is what must not move.
legacyPayloadSize ∷ Int
legacyPayloadSize = 356

-- | The pre-#1072 @poke@, reproduced at its literal offsets.
legacyPoke ∷ Ptr UniformBufferObject → UniformBufferObject → IO ()
legacyPoke p (UBO model view proj uiView uiProj brightness screenW screenH
                  pixelSnap sunAngle ambientLight facing defFmSlot worldCirc) = do
    pokeByteOff p 0   model
    pokeByteOff p 64  view
    pokeByteOff p 128 proj
    pokeByteOff p 192 uiView
    pokeByteOff p 256 uiProj
    pokeByteOff p 320 brightness
    pokeByteOff p 324 screenW
    pokeByteOff p 328 screenH
    pokeByteOff p 332 pixelSnap
    pokeByteOff p 336 sunAngle
    pokeByteOff p 340 ambientLight
    pokeByteOff p 344 facing
    pokeByteOff p 348 defFmSlot
    pokeByteOff p 352 worldCirc

-- | A matrix whose 16 entries are all distinct and unique to @k@, so a
--   swapped or shifted member cannot coincidentally compare equal.
mat ∷ Float → M44 Float
mat k = V4 (V4 (k + 1)  (k + 2)  (k + 3)  (k + 4))
           (V4 (k + 5)  (k + 6)  (k + 7)  (k + 8))
           (V4 (k + 9)  (k + 10) (k + 11) (k + 12))
           (V4 (k + 13) (k + 14) (k + 15) (k + 16))

-- | Every member distinguishable from every other.
sampleUBO ∷ UniformBufferObject
sampleUBO = UBO (mat 100) (mat 200) (mat 300) (mat 400) (mat 500)
                1.5 2.5 3.5 4.5 5.5 6.5 7.5 8.5 9.5

-- | Byte the scratch buffers are pre-filled with, so an unwritten byte is
--   distinguishable from a written zero.
sentinel ∷ Word8
sentinel = 0xAB

-- | Run an action over a sentinel-filled buffer of the full std140 size.
withScratch ∷ (Ptr UniformBufferObject → IO α) → IO α
withScratch act = allocaBytes uboStd140Size $ \p → do
    fillBytes p sentinel uboStd140Size
    act p

-- | Split on a character, dropping the separators.
splitOnChar ∷ Char → String → [String]
splitOnChar c s = case break (≡ c) s of
    (chunk, [])       → [chunk]
    (chunk, _ : rest) → chunk : splitOnChar c rest

-- | The @type name@ pairs a GLSL uniform block declares, in order.
parseBlockMembers ∷ String → [(String, String)]
parseBlockMembers src =
    let body = takeWhile (≢ '}') (drop 1 (dropWhile (≢ '{') src))
    in [ (ty, nm) | decl ← splitOnChar ';' body
                  , (ty : nm : _) ← [words decl] ]

spec ∷ Spec
spec = do
    describe "std140 metadata" $ do
        it "reports size 368 and alignment 16, without forcing its argument" $ do
            -- The pair was inconsistent before #1072: alignment 16 against a
            -- sizeOf of 356, which std140 rounds up to 368. Passing `undefined`
            -- is not incidental — Data.Vector.Storable does exactly this, and
            -- these modules are compiled with Strict.
            sizeOf (undefined ∷ UniformBufferObject) `shouldBe` 368
            alignment (undefined ∷ UniformBufferObject) `shouldBe` 16
        it "derives that size and alignment, not just reports them" $ do
            uboStd140Size `shouldBe` 368
            uboBaseAlignment `shouldBe` 16
            uboPayloadSize `shouldBe` legacyPayloadSize
            uboStd140Size - uboPayloadSize `shouldBe` 12  -- trailing padding

    describe "member offsets" $ do
        it "puts all 14 members exactly where the shipped shaders expect" $ do
            length uboMembers `shouldBe` 14
            uboMemberOffsets `shouldBe` legacyOffsets

    describe "Storable" $ do
        it "writes a payload byte-identical to the pre-#1072 instance" $ do
            new ← withScratch $ \p → poke p sampleUBO
                    ≫ peekArray legacyPayloadSize (castPtr p ∷ Ptr Word8)
            old ← withScratch $ \p → legacyPoke p sampleUBO
                    ≫ peekArray legacyPayloadSize (castPtr p ∷ Ptr Word8)
            new `shouldBe` old

        it "touches nothing past the payload — the 12 extra bytes are padding" $ do
            tailBytes ← withScratch $ \p → do
                poke p sampleUBO
                peekArray (uboStd140Size - legacyPayloadSize)
                          (castPtr p `plusPtr` legacyPayloadSize ∷ Ptr Word8)
            tailBytes `shouldBe` replicate (uboStd140Size - legacyPayloadSize) sentinel

        it "round-trips every member through poke then peek" $ do
            back ← withScratch $ \p → poke p sampleUBO ≫ peek p
            show back `shouldBe` show sampleUBO

        it "reads each member back from its own legacy offset" $ do
            (mats, floats) ← withScratch $ \p → do
                poke p sampleUBO
                ms ← mapM (\o → peekByteOff p o ∷ IO (M44 Float)) (take 5 legacyOffsets)
                fs ← mapM (\o → peekByteOff p o ∷ IO Float) (drop 5 legacyOffsets)
                pure (ms, fs)
            mats `shouldBe` [mat 100, mat 200, mat 300, mat 400, mat 500]
            floats `shouldBe` [1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5]

    describe "GLSL block" $ do
        it "declares exactly the canonical members, in canonical order" $
            parseBlockMembers (show uboGlslBlock)
                `shouldBe` [ (glslTypeName (umType m), umGlslName m) | m ← uboMembers ]
        it "states std140 rather than relying on the block default" $
            show uboGlslBlock `shouldSatisfy` isInfixOf "std140"
        it "is a single line, so it cannot shift a shader's #line numbering" $
            lines (show uboGlslBlock) `shouldSatisfy` ((≡ 1) . length)
