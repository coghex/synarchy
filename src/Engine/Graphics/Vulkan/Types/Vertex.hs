{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Graphics.Vulkan.Types.Vertex where
import UPrelude
import qualified Data.Text as T
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

vertexTotalSize ∷ Int
vertexTotalSize = 44

-- | Bit 0 of renderFlags: when set, the fragment shader emits a 1-pixel
-- white outline around alpha-cutout sprite edges. Used by selected units.
renderFlagSelected ∷ Word32
renderFlagSelected = 1

-- | Backward-compatible Vertex constructor: takes the original 5 fields
-- and defaults renderFlags to 0. Use the full `Vertex` constructor when
-- you need to set flags (e.g. Unit.Render for selected units).
mkVertex ∷ Vec2 → Vec2 → Vec4 → Float → Float → Vertex
mkVertex p t c a f = Vertex p t c a f 0

-- | 2D vector for positions and texture coordinates
data Vec2 = Vec2 
    { x ∷ !Float
    , y ∷ !Float 
    } deriving (Show, Eq)

instance Storable Vec2 where
    sizeOf _ = 8
    alignment _ = 4
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

instance Storable Vec4 where
    sizeOf _ = 16
    alignment _ = 4
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
    } deriving (Show, Eq)

instance Storable Vertex where
    sizeOf _ = vertexTotalSize
    alignment _ = 4
    peek ptr = do
        p ← peek (ptr `plusPtr` vertexPositionOffset)
        t ← peek (ptr `plusPtr` vertexTexCoordOffset)
        c ← peek (ptr `plusPtr` vertexColorOffset)
        a ← Storable.peekElemOff (castPtr (ptr `plusPtr` vertexAtlasIdOffset) ∷ Ptr Float) 0
        f ← Storable.peekElemOff (castPtr (ptr `plusPtr` vertexFaceMapIdOffset) ∷ Ptr Float) 0
        rf ← Storable.peekElemOff (castPtr (ptr `plusPtr` vertexRenderFlagsOffset) ∷ Ptr Word32) 0
        return $! Vertex p t c a f rf
    poke ptr (Vertex p t c a f rf) = do
        poke (ptr `plusPtr` vertexPositionOffset) p
        poke (ptr `plusPtr` vertexTexCoordOffset) t
        poke (ptr `plusPtr` vertexColorOffset) c
        Storable.pokeElemOff (castPtr (ptr `plusPtr` vertexAtlasIdOffset) ∷ Ptr Float) 0 a
        Storable.pokeElemOff (castPtr (ptr `plusPtr` vertexFaceMapIdOffset) ∷ Ptr Float) 0 f
        Storable.pokeElemOff (castPtr (ptr `plusPtr` vertexRenderFlagsOffset) ∷ Ptr Word32) 0 rf
