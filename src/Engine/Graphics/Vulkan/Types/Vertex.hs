{-# LANGUAGE Strict #-}
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

-- | Pack already-computed cylinder coordinates (u = gx-gy, v = gx+gy)
-- into the vertex's worldUV attribute: two Word16 halves, v in the high
-- bits. 'fromIntegral' to 'Word16' truncates by wrapping
-- (two's-complement), matching the GLSL decode's @(x & 0xFFFF)@
-- sign-restore exactly — so this round-trips correctly for negative
-- u/v, only wrapping (not clamping) once |u| or |v| exceeds 32767
-- tiles (worldSize ≳ 2048, beyond any world this engine generates
-- today). Split from 'packWorldUV' for callers that already have u,v
-- in hand (e.g. the zoom map's per-corner bake, #483 review) rather
-- than a (gx,gy) pair.
packUV ∷ Int → Int → Word32
packUV u v =
    let u16 = fromIntegral (fromIntegral u ∷ Word16) ∷ Word32
        v16 = fromIntegral (fromIntegral v ∷ Word16) ∷ Word32
    in (v16 `shiftL` 16) ⌄ u16

-- | Pack a tile's cylinder coordinates (u = gx-gy, v = gx+gy — see
-- 'World.Plate.worldWidthTiles' / 'World.Time.Local.localSunAngle') into
-- the vertex's worldUV attribute. See 'packUV' for the encoding.
packWorldUV ∷ Int → Int → Word32
packWorldUV gx gy = packUV (gx - gy) (gx + gy)

-- | Backward-compatible Vertex constructor: takes the original 5 fields
-- and defaults renderFlags AND worldUV to 0. Use the full `Vertex`
-- constructor when you need to set flags (e.g. Unit.Render for selected
-- units) or 'mkVertexWorld' when you need real world coordinates (e.g.
-- tile/flora/structure quads, #483).
mkVertex ∷ Vec2 → Vec2 → Vec4 → Float → Float → Vertex
mkVertex p t c a f = Vertex p t c a f 0 0

-- | Like 'mkVertex', but stamps the tile's packed world coordinates
-- (pass the result of 'packWorldUV') instead of defaulting worldUV to
-- 0. renderFlags still defaults to 0 — for a whole sprite quad that
-- needs both a non-zero worldUV AND flags, build its four corners with
-- 'quadVertices' rather than restating the 'Vertex' constructor.
mkVertexWorld ∷ Word32 → Vec2 → Vec2 → Vec4 → Float → Float → Vertex
mkVertexWorld wuv p t c a f = Vertex p t c a f 0 wuv

-- | The four corner POSITIONS of a sprite quad, in the order
-- 'Engine.Scene.Types.Batch.SortableQuad' documents for
-- @sqV0@..@sqV3@: top-left, top-right, bottom-right, bottom-left.
--
-- Positions are given per corner rather than as an origin plus a size
-- because three of the quad producers cannot express themselves that
-- way: the blood decals rotate each corner about the quad centre
-- independently, and the sliced front wall moves its left and right
-- edges together with its U range. 'rectCorners' is the convenience
-- for the axis-aligned majority.
data QuadCorners = QuadCorners
    { qcTopLeft     ∷ !Vec2
    , qcTopRight    ∷ !Vec2
    , qcBottomRight ∷ !Vec2
    , qcBottomLeft  ∷ !Vec2
    } deriving (Show, Eq)

-- | An axis-aligned quad: its top-left corner and its size. Expands to
-- the same @(x,y)@, @(x+w,y)@, @(x+w,y+h)@, @(x,y+h)@ winding every
-- upright sprite has always emitted.
rectCorners ∷ Vec2   -- ^ top-left corner
            → Vec2   -- ^ (width, height)
            → QuadCorners
rectCorners (Vec2 x0 y0) (Vec2 w h) = QuadCorners
    { qcTopLeft     = Vec2 x0 y0
    , qcTopRight    = Vec2 (x0 + w) y0
    , qcBottomRight = Vec2 (x0 + w) (y0 + h)
    , qcBottomLeft  = Vec2 x0 (y0 + h)
    }

-- | The texture sub-rect a quad samples, named by which corners each
-- value reaches rather than by min\/max: 'quLeftU' is the U of the two
-- LEFT-hand corners (v0, v3) and 'quRightU' the U of the two RIGHT-hand
-- ones (v1, v2). Passing them the other way round is exactly a
-- horizontal mirror, which is how 'Unit.Render' draws SW\/W\/NW from the
-- SE\/E\/NE sprites, and a narrowed U range is how a front wall's
-- vertical strip samples its own slice.
data QuadUV = QuadUV
    { quLeftU   ∷ !Float
    , quTopV    ∷ !Float
    , quRightU  ∷ !Float
    , quBottomV ∷ !Float
    } deriving (Show, Eq)

-- | The whole texture, corner to corner: the UV rect of every quad that
-- draws a sprite unflipped and unsliced.
fullQuadUV ∷ QuadUV
fullQuadUV = QuadUV 0 0 1 1

-- | The five per-vertex values a quad repeats identically across all
-- four of its corners — everything a 'Vertex' carries except its
-- position and its texture coordinate.
data QuadPayload = QuadPayload
    { qpTint      ∷ !Vec4   -- ^ 'color'
    , qpAtlasSlot ∷ !Float  -- ^ 'atlasId'
    , qpFaceMap   ∷ !Float  -- ^ 'faceMapId'
    , qpFlags     ∷ !Word32 -- ^ 'renderFlags'
    , qpWorldUV   ∷ !Word32 -- ^ 'worldUV', from 'packWorldUV'
    } deriving (Show, Eq)

-- | Build a textured quad's four vertices (#1152).
--
-- THIS IS THE ONE PLACE the world-sprite quads state their winding and
-- their UV-to-corner pairing. The corners come out in the
-- @sqV0@..@sqV3@ order 'Engine.Scene.Types.Batch.SortableQuad'
-- documents, each taking its texture coordinate from the matching
-- edges of the UV rect:
--
-- * v0 = top-left     samples (leftU,  topV)
-- * v1 = top-right    samples (rightU, topV)
-- * v2 = bottom-right samples (rightU, bottomV)
-- * v3 = bottom-left  samples (leftU,  bottomV)
--
-- Swapping a pair produces a mirrored or sheared sprite rather than a
-- type error, which is why the eight quad producers (buildings and
-- their ghosts, structure pieces\/walls\/posts, units, ground items and
-- blood decals) share this definition instead of restating it.
--
-- Not to be confused with
-- 'Engine.Scene.Batch.Vertex.generateQuadVertices' (scene nodes, no
-- flags\/world UV) or 'UI.Render.makeQuadVertices' (the UI pipeline's
-- six-vertex triangle list).
quadVertices ∷ QuadCorners → QuadUV → QuadPayload
             → (Vertex, Vertex, Vertex, Vertex)
quadVertices corners uv payload =
    ( corner (qcTopLeft     corners) (quLeftU  uv) (quTopV    uv)
    , corner (qcTopRight    corners) (quRightU uv) (quTopV    uv)
    , corner (qcBottomRight corners) (quRightU uv) (quBottomV uv)
    , corner (qcBottomLeft  corners) (quLeftU  uv) (quBottomV uv)
    )
  where
    corner p u v = Vertex p (Vec2 u v) (qpTint payload)
                          (qpAtlasSlot payload) (qpFaceMap payload)
                          (qpFlags payload) (qpWorldUV payload)

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
