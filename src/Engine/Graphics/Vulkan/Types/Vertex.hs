{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Types.Vertex where
import UPrelude
import Control.DeepSeq (NFData(..), rwhnf)
import Data.Int (Int32)
import qualified Foreign.Storable as Storable
import Engine.Graphics.Solar (solarPageNone)

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

-- | World cylinder coordinates (#483 longitude-local day/night,
-- widened by #2019): TWO signed 32-bit components, u = gx-gy first then
-- v = gx+gy, declared as @FORMAT_R32G32_SINT@ \/ GLSL @ivec2@ and
-- occupying eight bytes. See 'WorldUV' and 'tileWorldUV'. Ignored by
-- pipelines that don't declare the matching vertex input (UI/font) —
-- same "extra trailing field, inert unless read" pattern as
-- 'vertexRenderFlagsOffset'.
vertexWorldUVOffset ∷ Int
vertexWorldUVOffset = 44

-- | Which world page's clock and circumference light this vertex
--   (#1869): @0@ ('Engine.Graphics.Solar.solarPageNone') means none —
--   the vertex takes the UBO's global @sunAngle@ \/
--   @worldCircumferenceTiles@, which is what UI and generic scene
--   sprites have always done — and @n > 0@ selects
--   @ubo.solarPages[n - 1]@.
--
--   Ignored by pipelines that don't declare the matching vertex input,
--   exactly like 'vertexRenderFlagsOffset' and 'vertexWorldUVOffset'.
--
--   Kept out of 'renderFlags' deliberately: that word is a FLAG bitset
--   whose bits are allocated one at a time, and a page index sharing it
--   would be corrupted the day someone allocates a bit above it.
vertexSolarPageOffset ∷ Int
vertexSolarPageOffset = 52

vertexTotalSize ∷ Int
vertexTotalSize = 56

-- | The ONE value a quad's 'faceMapId' \/ 'qpFaceMap' carries when its
-- producer has NO directional face map of its own (#1696).
--
-- The bindless fragment shader resolves a vertex's face-map id through
-- the handle→slot table and falls back to @fragDefaultFaceMapSlot@ —
-- the 1x1 neutral map published through the UBO — whenever that
-- resolves to slot 0. Any id outside the table resolves to slot 0 by
-- the shader's own bounds check, which is what makes @-1@ mean "use the
-- default" unconditionally, independent of what any texture handle is
-- registered as.
--
-- Use this rather than spelling the meaning locally: generic scene
-- sprites, buildings, units, blood decals, ground items, the zoom
-- map\/its icons\/its cursor, and flora all mean the same thing by it.
-- It is NOT the missing-ASSET sentinel — that is
-- 'Engine.Asset.Handle.missingTextureHandle', a texture handle, and a
-- different axis.
noFaceMapVertexId ∷ Float
noFaceMapVertexId = -1

-- | Bit 0 of renderFlags: when set, the fragment shader emits a 1-pixel
-- white outline around alpha-cutout sprite edges. Used by selected units.
renderFlagSelected ∷ Word32
renderFlagSelected = 1

-- | A world vertex's cylinder coordinates (#483 longitude-local
-- day\/night; widened by #2019): @u = gx - gy@ and @v = gx + gy@, each
-- carried WHOLE as a signed 32-bit integer rather than packed into
-- halves of one word.
--
-- Until #2019 the pair lived in a single 'Word32' as two 'Word16'
-- halves, which round-tripped negatives correctly but WRAPPED — silently,
-- with no clamp, warning or refusal — once |u| or |v| passed 32767
-- (worldSize ≳ 2048). #2017 commits to a 1024 guarantee with map
-- addressing designed through 8192, so that boundary is retired here:
-- the attribute is @FORMAT_R32G32_SINT@ \/ GLSL @ivec2@, and nothing on
-- the path from a tile coordinate to the shader packs, truncates or
-- wraps.
--
-- 'wuvV' is transported but not yet consumed — the shader reads only
-- 'wuvU'. That is deliberate (design D-9): @v@ is intended for the
-- future seasonal\/directional shading model, and paying the stride
-- increase once now avoids a second whole-renderer vertex-format
-- migration later. It is not dead weight to be optimized away; every
-- producer supplies it and every copying path preserves it exactly.
data WorldUV = WorldUV
    { wuvU ∷ !Int32   -- ^ @gx - gy@
    , wuvV ∷ !Int32   -- ^ @gx + gy@
    } deriving (Show, Eq)

instance NFData WorldUV where
    rnf = rwhnf

-- NB: lazy (~) patterns for the same reason 'Vec2' uses them — this
-- module is compiled with Strict and Storable calls @sizeOf undefined@.
instance Storable WorldUV where
    sizeOf ~_ = 8
    alignment ~_ = 4
    peek ptr = do
        u ← Storable.peekElemOff (castPtr ptr ∷ Ptr Int32) 0
        v ← Storable.peekElemOff (castPtr ptr ∷ Ptr Int32) 1
        return $! WorldUV u v
    poke ptr (WorldUV u v) = do
        Storable.pokeElemOff (castPtr ptr ∷ Ptr Int32) 0 u
        Storable.pokeElemOff (castPtr ptr ∷ Ptr Int32) 1 v

-- | The coordinates a vertex with no world position carries: the origin.
-- What 'mkVertex' defaults to, exactly as it defaulted the packed word
-- to @0@ before #2019.
worldUVNone ∷ WorldUV
worldUVNone = WorldUV 0 0

-- | One component of a 'WorldUV', from the 'Int' the render code
-- computes in. REFUSES a value the signed 32-bit carrier cannot hold
-- instead of letting 'fromIntegral' wrap it (#2019): a wrapped
-- coordinate is a DIFFERENT tile, and the old packed encoding's only
-- symptom for that was mislit geometry. Unreachable for any world this
-- engine addresses — worldSize 8192 reaches |u|,|v| ≤ 262144, four
-- orders of magnitude inside the bound — so this is a tripwire for a
-- future coordinate space, not a hot-path branch anyone pays for.
worldUVComponent ∷ String → Int → Int32
worldUVComponent name n
    | n < fromIntegral (minBound ∷ Int32) ∨ n > fromIntegral (maxBound ∷ Int32)
    = error $ "worldUVComponent: world cylinder coordinate " ⧺ name
           ⧺ " = " ⧺ show n ⧺ " does not fit the vertex's signed 32-bit "
           ⧺ "worldUV attribute"
    | otherwise = fromIntegral n

-- | Build the vertex attribute from already-computed cylinder
-- coordinates (u = gx-gy, v = gx+gy). Split from 'tileWorldUV' for
-- callers that already have u,v in hand (e.g. the zoom map's per-corner
-- bake, #483 review) rather than a (gx,gy) pair.
mkWorldUV ∷ Int → Int → WorldUV
mkWorldUV u v = WorldUV (worldUVComponent "u" u) (worldUVComponent "v" v)

-- | Build the vertex attribute from a tile's grid coordinates, deriving
-- the cylinder pair (u = gx-gy, v = gx+gy — see
-- 'World.Plate.worldWidthTiles' / 'World.Time.Local.localSunAngle').
tileWorldUV ∷ Int → Int → WorldUV
tileWorldUV gx gy = mkWorldUV (gx - gy) (gx + gy)

-- | Backward-compatible Vertex constructor: takes the original 5 fields
-- and defaults renderFlags to 0 AND worldUV to 'worldUVNone'. Use the
-- full `Vertex` constructor when you need to set flags (e.g. Unit.Render
-- for selected units) or 'mkVertexWorld' when you need real world
-- coordinates (e.g. tile/flora/structure quads, #483).
mkVertex ∷ Vec2 → Vec2 → Vec4 → Float → Float → Vertex
mkVertex p t c a f = Vertex p t c a f 0 worldUVNone solarPageNone

-- | Like 'mkVertex', but stamps the tile's world cylinder coordinates
-- (pass the result of 'tileWorldUV') instead of defaulting worldUV to
-- the origin. renderFlags still defaults to 0 — for a whole sprite quad
-- that needs both a real worldUV AND flags, build its four corners with
-- 'quadVertices' rather than restating the 'Vertex' constructor.
mkVertexWorld ∷ WorldUV → Vec2 → Vec2 → Vec4 → Float → Float → Vertex
mkVertexWorld wuv p t c a f = Vertex p t c a f 0 wuv solarPageNone

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
    , qpWorldUV   ∷ !WorldUV -- ^ 'worldUV', from 'tileWorldUV'
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
                          (qpFlags payload) (qpWorldUV payload) solarPageNone

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
    , worldUV     ∷ !WorldUV -- ^ Signed world (u,v), see 'WorldUV' (layout = 6)
    , solarPage   ∷ !Word32 -- ^ Solar page slot, see vertexSolarPageOffset (layout = 7)
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
        wuv ← peek (ptr `plusPtr` vertexWorldUVOffset)
        sp ← Storable.peekElemOff (castPtr (ptr `plusPtr` vertexSolarPageOffset) ∷ Ptr Word32) 0
        return $! Vertex p t c a f rf wuv sp
    poke ptr (Vertex p t c a f rf wuv sp) = do
        poke (ptr `plusPtr` vertexPositionOffset) p
        poke (ptr `plusPtr` vertexTexCoordOffset) t
        poke (ptr `plusPtr` vertexColorOffset) c
        Storable.pokeElemOff (castPtr (ptr `plusPtr` vertexAtlasIdOffset) ∷ Ptr Float) 0 a
        Storable.pokeElemOff (castPtr (ptr `plusPtr` vertexFaceMapIdOffset) ∷ Ptr Float) 0 f
        Storable.pokeElemOff (castPtr (ptr `plusPtr` vertexRenderFlagsOffset) ∷ Ptr Word32) 0 rf
        poke (ptr `plusPtr` vertexWorldUVOffset) wuv
        Storable.pokeElemOff (castPtr (ptr `plusPtr` vertexSolarPageOffset) ∷ Ptr Word32) 0 sp
