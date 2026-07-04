{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Graphics.Vulkan.Types.Vertex where
import UPrelude
import Control.DeepSeq (NFData(..), rwhnf)
import qualified Foreign.Storable as Storable

-- Constants for vertex layout
vertexPositionOffset ∷ Int
vertexPositionOffset = 0

vertexTexCoordOffset ∷ Int
vertexTexCoordOffset = 8

vertexColorOffset ∷ Int
vertexColorOffset = 16

vertexAtlasIdOffset ∷ Int
vertexAtlasIdOffset = 32

vertexFaceMapIdOffset ∷ Int
vertexFaceMapIdOffset = 36

-- | Per-vertex render flags (Word32 bitset). Bit 0 = SELECTED_OUTLINE.
-- See bindless fragment shader for outline logic.
vertexRenderFlagsOffset ∷ Int
vertexRenderFlagsOffset = 40

-- | Packed world tile coordinates (#483 longitude-local day/night):
-- two signed 16-bit halves, low = u = gx-gy, high = v = gx+gy. See
-- 'packWorldUV'. Ignored by pipelines that don't declare the matching
-- vertex input (UI/font) — same "extra trailing field, inert unless
-- read" pattern as 'vertexRenderFlagsOffset'.
vertexWorldUVOffset ∷ Int
vertexWorldUVOffset = 44

vertexTotalSize ∷ Int
vertexTotalSize = 48

-- | Bit 0 of renderFlags: when set, the fragment shader emits a 1-pixel
-- white outline around alpha-cutout sprite edges. Used by selected units.
renderFlagSelected ∷ Word32
renderFlagSelected = 1

-- | Pack a tile's cylinder coordinates (u = gx-gy, v = gx+gy — see
-- 'World.Plate.worldWidthTiles' / 'World.Time.Local.localSunAngle') into
-- the vertex's worldUV attribute: two Word16 halves, v in the high bits.
-- 'fromIntegral' to 'Word16' truncates by wrapping (two's-complement),
-- matching the GLSL decode's @(x & 0xFFFF)@ sign-restore exactly — so
-- this round-trips correctly for negative u/v, only wrapping (not
-- clamping) once |u| or |v| exceeds 32767 tiles (worldSize ≳ 2048,
-- beyond any world this engine generates today).
packWorldUV ∷ Int → Int → Word32
packWorldUV gx gy =
    let u16 = fromIntegral (fromIntegral (gx - gy) ∷ Word16) ∷ Word32
        v16 = fromIntegral (fromIntegral (gx + gy) ∷ Word16) ∷ Word32
    in (v16 `shiftL` 16) ⌄ u16

-- | Backward-compatible Vertex constructor: takes the original 5 fields
-- and defaults renderFlags AND worldUV to 0. Use the full `Vertex`
-- constructor when you need to set flags (e.g. Unit.Render for selected
-- units) or 'mkVertexWorld' when you need real world coordinates (e.g.
-- tile/flora/structure quads, #483).
mkVertex ∷ Vec2 → Vec2 → Vec4 → Float → Float → Vertex
mkVertex p t c a f = Vertex p t c a f 0 0

-- | Like 'mkVertex', but stamps the tile's packed world coordinates
-- (pass the result of 'packWorldUV') instead of defaulting worldUV to
-- 0. renderFlags still defaults to 0 — combine with a direct 'Vertex'
-- construction if a caller ever needs both a non-zero worldUV AND flags.
mkVertexWorld ∷ Word32 → Vec2 → Vec2 → Vec4 → Float → Float → Vertex
mkVertexWorld wuv p t c a f = Vertex p t c a f 0 wuv

-- | 2D vector for positions and texture coordinates
data Vec2 = Vec2
    { x ∷ !Float
    , y ∷ !Float
    } deriving (Show, Eq)

-- | All fields strict and primitive, so WHNF = NF for these three
--   (needed by the parallel quad build's rdeepseq, #447).
instance NFData Vec2 where
    rnf = rwhnf

-- NB: sizeOf/alignment take LAZY (~) patterns. This module is compiled
-- with Strict, and Data.Vector.Storable calls @sizeOf undefined@ — a
-- strict wildcard would force it and crash (same trap family as the
-- Strict+derivingUnbox gotcha).
instance Storable Vec2 where
    sizeOf ~_ = 8
    alignment ~_ = 4
    peek ptr = do
        x' ← Storable.peekElemOff (castPtr ptr ∷ Ptr Float) 0
        y' ← Storable.peekElemOff (castPtr ptr ∷ Ptr Float) 1
        return $! Vec2 x' y'
    poke ptr (Vec2 x' y') = do
        Storable.pokeElemOff (castPtr ptr ∷ Ptr Float) 0 x'
        Storable.pokeElemOff (castPtr ptr ∷ Ptr Float) 1 y'

-- | 4D vector for colors
data Vec4 = Vec4
    { r ∷ !Float
    , g ∷ !Float
    , b ∷ !Float
    , a ∷ !Float
    } deriving (Show, Eq)

instance NFData Vec4 where
    rnf = rwhnf

instance Storable Vec4 where
    sizeOf ~_ = 16
    alignment ~_ = 4
    peek ptr = do
        r' ← Storable.peekElemOff (castPtr ptr ∷ Ptr Float) 0
        g' ← Storable.peekElemOff (castPtr ptr ∷ Ptr Float) 1
        b' ← Storable.peekElemOff (castPtr ptr ∷ Ptr Float) 2
        a' ← Storable.peekElemOff (castPtr ptr ∷ Ptr Float) 3
        return $! Vec4 r' g' b' a'
    poke ptr (Vec4 r' g' b' a') = do
        Storable.pokeElemOff (castPtr ptr ∷ Ptr Float) 0 r'
        Storable.pokeElemOff (castPtr ptr ∷ Ptr Float) 1 g'
        Storable.pokeElemOff (castPtr ptr ∷ Ptr Float) 2 b'
        Storable.pokeElemOff (castPtr ptr ∷ Ptr Float) 3 a'

data Vertex = Vertex
    { pos         ∷ !Vec2   -- ^ Position (layout = 0)
    , tex         ∷ !Vec2   -- ^ Texture coordinates (layout = 1)
    , color       ∷ !Vec4   -- ^ Color (layout = 2)
    , atlasId     ∷ !Float  -- ^ Atlas ID (layout = 3)
    , faceMapId   ∷ !Float  -- ^ Face map texture slot (layout = 4)
    , renderFlags ∷ !Word32 -- ^ Render-flag bitset, see renderFlag* (layout = 5)
    , worldUV     ∷ !Word32 -- ^ Packed world (u,v), see packWorldUV (layout = 6)
    } deriving (Show, Eq)

instance NFData Vertex where
    rnf = rwhnf

instance Storable Vertex where
    sizeOf ~_ = vertexTotalSize
    alignment ~_ = 4
    peek ptr = do
        p ← peek (ptr `plusPtr` vertexPositionOffset)
        t ← peek (ptr `plusPtr` vertexTexCoordOffset)
        c ← peek (ptr `plusPtr` vertexColorOffset)
        a ← Storable.peekElemOff (castPtr (ptr `plusPtr` vertexAtlasIdOffset) ∷ Ptr Float) 0
        f ← Storable.peekElemOff (castPtr (ptr `plusPtr` vertexFaceMapIdOffset) ∷ Ptr Float) 0
        rf ← Storable.peekElemOff (castPtr (ptr `plusPtr` vertexRenderFlagsOffset) ∷ Ptr Word32) 0
        wuv ← Storable.peekElemOff (castPtr (ptr `plusPtr` vertexWorldUVOffset) ∷ Ptr Word32) 0
        return $! Vertex p t c a f rf wuv
    poke ptr (Vertex p t c a f rf wuv) = do
        poke (ptr `plusPtr` vertexPositionOffset) p
        poke (ptr `plusPtr` vertexTexCoordOffset) t
        poke (ptr `plusPtr` vertexColorOffset) c
        Storable.pokeElemOff (castPtr (ptr `plusPtr` vertexAtlasIdOffset) ∷ Ptr Float) 0 a
        Storable.pokeElemOff (castPtr (ptr `plusPtr` vertexFaceMapIdOffset) ∷ Ptr Float) 0 f
        Storable.pokeElemOff (castPtr (ptr `plusPtr` vertexRenderFlagsOffset) ∷ Ptr Word32) 0 rf
        Storable.pokeElemOff (castPtr (ptr `plusPtr` vertexWorldUVOffset) ∷ Ptr Word32) 0 wuv
