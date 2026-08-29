-- | The uniform buffer object's binary layout (#1072).
--
--   #1072 replaced a hand-written 'Storable' instance carrying literal
--   byte offsets, and four hand-written GLSL blocks, with one enumeration
--   ("Engine.Graphics.Vulkan.Uniform.Layout") that both are derived from.
--   That was meant to be REPRESENTATION-PRESERVING: the same 14 members at
--   the same offsets, so a running frame produces identical output.
--
--   #1869 APPENDED a fifteenth, the per-page solar table, which is the
--   other thing this gate is for: appending must leave all fourteen
--   original offsets alone, and the array member must be laid out
--   densely (std140 gives a @vec4@ array a 16-byte stride, which is the
--   element's own size — a @float@ array would have padded each element
--   to 16 and silently disagreed with the shader).
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
import qualified Data.Vector as V
import Linear (V4(..), M44)
import Engine.Graphics.Solar (maxSolarPages)
import Engine.Graphics.Vulkan.Types (UniformBufferObject(..))
import Engine.Graphics.Vulkan.Uniform.Layout
  ( UBOMember(..), uboMembers, uboMemberOffsets, uboPayloadSize
  , uboBaseAlignment, uboStd140Size, uboGlslBlock, glslTypeName
  , glslArraySuffix )

-- | The offsets every shader shipped before #1072 was compiled against.
--   Written out as literals ON PURPOSE: a test that recomputed them from
--   the same rules the implementation uses would agree with any mistake.
legacyOffsets ∷ [Int]
legacyOffsets = [0, 64, 128, 192, 256, 320, 324, 328, 332, 336, 340, 344, 348, 352]

-- | Bytes the members occupy, end of the last one. The std140 size adds
--   trailing padding on top of this; the PAYLOAD is what must not move.
legacyPayloadSize ∷ Int
legacyPayloadSize = 356

-- | The offset #1869's appended @solarPages@ member takes: 352 + 4,
--   rounded up to a @vec4@ array's 16-byte alignment. A literal for the
--   same reason 'legacyOffsets' are.
solarPagesOffset ∷ Int
solarPagesOffset = 368

-- | Alignment padding between @worldCircumferenceTiles@ and
--   @solarPages@: bytes 356..368, which nothing writes.
legacyTailPadding ∷ Int
legacyTailPadding = solarPagesOffset - legacyPayloadSize

-- | The pre-#1072 @poke@, reproduced at its literal offsets. #1869's
--   member is bound and ignored: this reproduces what the OLD instance
--   wrote, and the prefix comparison below is exactly the claim that the
--   fourteen members it covers have not moved.
legacyPoke ∷ Ptr UniformBufferObject → UniformBufferObject → IO ()
legacyPoke p (UBO model view proj uiView uiProj brightness screenW screenH
                  pixelSnap sunAngle ambientLight facing defFmSlot worldCirc
                  _solarPages) = do
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

-- | Solar-table entries all distinct from each other and from every
--   scalar above, so a slot written to the wrong index shows up.
sampleSolarPages ∷ V.Vector (V4 Float)
sampleSolarPages = V.generate maxSolarPages $ \i →
    let k = 1000 + 10 * fromIntegral i
    in V4 k (k + 1) (k + 2) (k + 3)

-- | Every member distinguishable from every other.
sampleUBO ∷ UniformBufferObject
sampleUBO = UBO (mat 100) (mat 200) (mat 300) (mat 400) (mat 500)
                1.5 2.5 3.5 4.5 5.5 6.5 7.5 8.5 9.5 sampleSolarPages

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
--   An array member's @[n]@ rides on the NAME, which is how the block
--   declares it (@vec4 solarPages[8];@).
parseBlockMembers ∷ String → [(String, String)]
parseBlockMembers src =
    let body = takeWhile (≢ '}') (drop 1 (dropWhile (≢ '{') src))
    in [ (ty, nm) | decl ← splitOnChar ';' body
                  , (ty : nm : _) ← [words decl] ]

spec ∷ Spec
spec = do
    describe "std140 metadata" $ do
        it "reports size 496 and alignment 16, without forcing its argument" $ do
            -- The pair was inconsistent before #1072: alignment 16 against a
            -- sizeOf of 356, which std140 rounds up to 368. Passing `undefined`
            -- is not incidental — Data.Vector.Storable does exactly this, and
            -- these modules are compiled with Strict.
            sizeOf (undefined ∷ UniformBufferObject) `shouldBe` 496
            alignment (undefined ∷ UniformBufferObject) `shouldBe` 16
        it "derives that size and alignment, not just reports them" $ do
            uboStd140Size `shouldBe` 496
            uboBaseAlignment `shouldBe` 16
            -- #1869's vec4[8] ends the block on a 16-byte boundary, so
            -- there is no trailing padding left to pay for.
            uboPayloadSize `shouldBe` solarPagesOffset + 16 * maxSolarPages
            uboStd140Size - uboPayloadSize `shouldBe` 0

    describe "member offsets" $ do
        it "leaves the 14 pre-#1869 members where the shipped shaders expect" $ do
            length uboMembers `shouldBe` 15
            take 14 uboMemberOffsets `shouldBe` legacyOffsets
        it "appends solarPages at its own aligned offset" $
            drop 14 uboMemberOffsets `shouldBe` [solarPagesOffset]

    describe "Storable" $ do
        it "writes a payload byte-identical to the pre-#1072 instance" $ do
            new ← withScratch $ \p → poke p sampleUBO
                    ≫ peekArray legacyPayloadSize (castPtr p ∷ Ptr Word8)
            old ← withScratch $ \p → legacyPoke p sampleUBO
                    ≫ peekArray legacyPayloadSize (castPtr p ∷ Ptr Word8)
            new `shouldBe` old

        it "touches nothing between worldCircumferenceTiles and solarPages" $ do
            tailBytes ← withScratch $ \p → do
                poke p sampleUBO
                peekArray legacyTailPadding
                          (castPtr p `plusPtr` legacyPayloadSize ∷ Ptr Word8)
            tailBytes `shouldBe` replicate legacyTailPadding sentinel

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

        it "lays solarPages out densely, 16 bytes per element" $ do
            entries ← withScratch $ \p → do
                poke p sampleUBO
                mapM (\i → peekByteOff p (solarPagesOffset + 16 * i)
                             ∷ IO (V4 Float))
                     [0 .. maxSolarPages - 1]
            entries `shouldBe` V.toList sampleSolarPages

    describe "GLSL block" $ do
        it "declares exactly the canonical members, in canonical order" $
            parseBlockMembers (show uboGlslBlock)
                `shouldBe` [ ( glslTypeName (umType m)
                             , umGlslName m <> glslArraySuffix (umType m) )
                           | m ← uboMembers ]
        it "declares solarPages as a maxSolarPages-long vec4 array" $
            parseBlockMembers (show uboGlslBlock)
                `shouldSatisfy` elem ("vec4", "solarPages[" <> show maxSolarPages <> "]")
        it "states std140 rather than relying on the block default" $
            show uboGlslBlock `shouldSatisfy` isInfixOf "std140"
        it "is a single line, so it cannot shift a shader's #line numbering" $
            lines (show uboGlslBlock) `shouldSatisfy` ((≡ 1) . length)
