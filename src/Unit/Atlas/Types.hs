{-# LANGUAGE Strict #-}
-- | Storage-neutral animation-frame addressing (#1259 TEX-3, narrowed
--   to atlases alone by #1261 TEX-6).
--
--   A unit 'Animation' stores its frames in exactly ONE representation
--   ('AnimStorage'), and every consumer reads a frame through the same
--   storage-neutral 'FrameSample'. Keeping that a named SUM rather than
--   inlining the atlas is the point: D-10 allows exactly one resident
--   representation per logical animation and forbids mixing
--   representations within one animation, so a shape that cannot
--   represent a mixture cannot drift into one — and it stays the seam a
--   later representation would be added at.
--
--   * 'StorageAtlas' — one compiled image per animation (D-2), one
--     texture handle, one bindless slot; a frame is a UV sub-rect of
--     it, addressed through the generated index ('Unit.Atlas.Index').
--     Since #1261 this is the ONLY unit-animation representation: the
--     historical per-frame @StorageLegacy@ (one whole-image texture per
--     frame) is gone, along with the loader that built it. WHICH
--     encoding an atlas's pixels use stays open behind
--     'AtlasStorageFormat', which is where deferred TEX-5's KTX2 slots
--     in.
--
--   Buildings are NOT on this type. They were the other user of the
--   shared per-frame record and are never compiled to atlases (D-8
--   leaves their storage untouched), so #1261 split them onto their own
--   'Building.Types.BuildingAnimation' rather than deleting the
--   representation they still need.
--
--   A 'FrameSample' therefore carries the stable bindless handle
--   (#286 — never a slot), the frame's own UV endpoints WITHIN that
--   handle's image, the frame's pixel dimensions when the storage
--   knows them, and the mirror flag. An atlas sample's dimensions are
--   the CELL's, so no consumer ever measures the whole sheet where it
--   means one frame. 'wholeImageSample' remains for the direct
--   single-texture families D-8 preserves — the T-pose default sprite
--   and its directional overrides — which span their own whole image
--   and report no dimensions of their own, so their consumers keep
--   measuring 'Engine.Core.Capability.RenderView.rvTextureSizeRef'
--   exactly as before.
module Unit.Atlas.Types
    ( AtlasStorageFormat(..)
    , atlasStorageFormatName
    , AtlasDirectionRow(..)
    , AtlasAnimation(..)
    , ResidentAtlas(..)
    , AnimStorage(..)
    , UVRect
    , FrameSample(..)
    , wholeImageUV
    , wholeImageSample
    , storageFrameCount
    , storageFrameCounts
    , storageMaxFrameCount
    , storageSampleAt
    , frameDimensions
    , atlasCellUV
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle)
import Unit.Direction (Direction)

-- | How one compiled atlas's pixels are stored on disk.
--
--   D-10 keeps this an explicit, closed set rather than an inferred
--   file extension so a later transcoded format (TEX-5's KTX2) slots
--   in ADDITIVELY: a new constructor, a new decode path, and every
--   geometry consumer below unchanged. An index naming a format this
--   build does not know is rejected at load, never guessed at.
data AtlasStorageFormat
    = AtlasFormatPng
      -- ^ Lossless RGBA8 PNG — what @tools/pack_atlas.py@ emits today
      --   (its @STORAGE_FORMAT = "png"@).
    deriving (Show, Eq, Ord)

-- | The token an index uses for a storage format.
atlasStorageFormatName ∷ AtlasStorageFormat → Text
atlasStorageFormatName AtlasFormatPng = "png"

-- | One AUTHORED direction's row in a compiled atlas.
--
--   'adrFrameCount' is the direction's REAL frame count and the sole
--   frame authority (D-5): a row shorter than the animation's column
--   count is rectangularized with transparent padding cells, and no
--   padding cell is ever addressable as a frame.
data AtlasDirectionRow = AtlasDirectionRow
    { adrDirection  ∷ !Direction
    , adrRow        ∷ !Int
      -- ^ Row index within the sheet, read from the index rather than
      --   re-derived from a direction order.
    , adrFrameCount ∷ !Int
    } deriving (Show, Eq)

-- | One animation's compiled-atlas metadata, as parsed and validated
--   from the generated index. Nobody hand-edits the index (D-11), and
--   nothing here is inferred: every field is read.
data AtlasAnimation = AtlasAnimation
    { aaName         ∷ !Text
    , aaFormat       ∷ !AtlasStorageFormat
    , aaPath         ∷ !FilePath
      -- ^ Resource-root-relative path of the atlas image.
    , aaAtlasWidth   ∷ !Int
    , aaAtlasHeight  ∷ !Int
    , aaCellWidth    ∷ !Int
    , aaCellHeight   ∷ !Int
    , aaCellPadding  ∷ !Int
      -- ^ The extrusion gutter, in texels per side (#2076). Every cell
      --   occupies a @(cellWidth + 2*p) x (cellHeight + 2*p)@ SLOT
      --   whose border is a copy of that cell's own edge texels, so a
      --   bilinear tap inside the logical cell cannot reach a
      --   neighbouring frame. Read from the index rather than assumed:
      --   'Unit.Atlas.Index' validates it as exactly the one layout
      --   this build supports, and 'atlasCellUV' strides by it.
    , aaColumns      ∷ !Int
    , aaRows         ∷ !Int
    , aaFlip         ∷ !Bool
    , aaFps          ∷ !Float
    , aaLoop         ∷ !Bool
    , aaDirections   ∷ !(Map.Map Direction AtlasDirectionRow)
    , aaSourceDigest ∷ !Text
      -- ^ The compiler's per-animation digest over its SOURCE frames —
      --   its identity, mirroring and timing, cell geometry, and every
      --   direction's declared frame paths and decoded pixels.
      --   RECOMPUTED and verified at load
      --   (@Unit.Atlas.Digest.sourceDigest@, checked by
      --   @Unit.Atlas.Index.validateSourceDigest@), which is what
      --   catches a forged digest and a frame renamed to a
      --   byte-identical file — the one input nothing else in the index
      --   records.
    , aaAtlasDigest  ∷ !Text
    } deriving (Show, Eq)

-- | A compiled animation whose atlas image is resident: the metadata
--   plus the ONE stable texture handle its cells share (#286).
data ResidentAtlas = ResidentAtlas
    { raAnimation ∷ !AtlasAnimation
    , raTexture   ∷ !TextureHandle
    } deriving (Show, Eq)

-- | Where one unit animation's frames live. Exactly one representation
--   per animation — see the module header.
newtype AnimStorage
    = StorageAtlas ResidentAtlas
    deriving (Show, Eq)

-- | @(u0, v0, u1, v1)@ — a texture sub-rect in normalized coordinates.
type UVRect = (Float, Float, Float, Float)

-- | The whole image.
wholeImageUV ∷ UVRect
wholeImageUV = (0, 0, 1, 1)

-- | ONE logical frame, resolved from either storage mode.
--
--   'fsUV' is frame-local: the endpoints to sample WITHIN 'fsTexture'.
--   'fsFlipX' mirrors the frame, and mirroring reflects across THIS
--   sub-rect — never across the whole image, which for an atlas would
--   land in a different cell (the #887 flip-after-clip ordering,
--   generalized: with atlases every sample is a sub-rect).
data FrameSample = FrameSample
    { fsTexture ∷ !TextureHandle
    , fsUV      ∷ !UVRect
    , fsCell    ∷ !(Maybe (Int, Int))
      -- ^ The frame's own pixel dimensions when the sample knows them
      --   (an atlas cell, from the index). 'Nothing' for a whole-image
      --   sample — the direct default/directional sprite a T-pose falls
      --   back to, whose image IS the frame, so its consumer measures
      --   the texture-size map as it always has.
    , fsFlipX   ∷ !Bool
    } deriving (Show, Eq)

-- | A whole-image sample: what the T-pose fallbacks — the unit's
--   direct default sprite and its directional overrides, both of which
--   D-8 keeps on ordinary single-texture loading — resolve to.
wholeImageSample ∷ TextureHandle → Bool → FrameSample
wholeImageSample tex flipX = FrameSample
    { fsTexture = tex
    , fsUV      = wholeImageUV
    , fsCell    = Nothing
    , fsFlipX   = flipX
    }

-- | This direction's REAL frame count, or 'Nothing' when the storage
--   authors no such direction at all.
--
--   The distinction matters to the mirror fallback: an ABSENT
--   direction may fall back to its mirror, while a direction that is
--   present but empty resolves to the T-pose exactly as it always
--   has.
storageFrameCount ∷ AnimStorage → Direction → Maybe Int
storageFrameCount (StorageAtlas res) dir =
    adrFrameCount <$> Map.lookup dir (aaDirections (raAnimation res))

-- | Every authored direction's REAL frame count — the index's own
--   counts, never the padded column count (D-5). This is what the
--   non-rendering consumers of a clip's length read:
--   'Unit.Thread.Command.Pose'\'s pose-transition durations and
--   @unit.getAnimDuration@ both go through here (via
--   'storageMaxFrameCount').
storageFrameCounts ∷ AnimStorage → Map.Map Direction Int
storageFrameCounts (StorageAtlas res) =
    adrFrameCount <$> aaDirections (raAnimation res)

-- | The longest authored direction's frame count (0 when the animation
--   authors none). This is the clip-LENGTH question — how long the
--   animation runs — which several callers ask independently of which
--   direction is on screen.
storageMaxFrameCount ∷ AnimStorage → Int
storageMaxFrameCount st =
    let counts = Map.elems (storageFrameCounts st)
    in if null counts then 0 else maximum counts

-- | Resolve one already-chosen logical frame to its sample.
--
--   'Nothing' when the direction is unauthored or the index is outside
--   that direction's REAL frame count — the caller has already clamped
--   or wrapped against 'storageFrameCount', so a 'Nothing' here means
--   a bug, not a padding cell being reached. Padding is unreachable by
--   construction: the bound is the real count, never the column count.
storageSampleAt ∷ AnimStorage → Direction → Int → Bool → Maybe FrameSample
storageSampleAt (StorageAtlas res) dir idx flipX =
    case Map.lookup dir (aaDirections anim) of
        Nothing → Nothing
        Just row
            | idx < 0 ∨ idx ≥ adrFrameCount row → Nothing
            | otherwise → Just FrameSample
                { fsTexture = raTexture res
                , fsUV      = atlasCellUV anim (adrRow row) idx
                , fsCell    = Just (aaCellWidth anim, aaCellHeight anim)
                , fsFlipX   = flipX
                }
  where
    anim = raAnimation res

-- | The normalized UV sub-rect of one atlas cell: @atlasCellUV anim row
--   column@.
--
--   Exact integer cell geometry, divided into normalized UV at the
--   LOGICAL CELL's own edges. Since #2076 each cell sits at the middle
--   of a @(cellW + 2p) x (cellH + 2p)@ slot, its 'aaCellPadding'
--   gutter holding a copy of its own edge texels, so the origin is
--   @column * slotW + p@ rather than @column * cellW@ and the
--   neighbouring frame is two texels away instead of adjacent.
--
--   Still deliberately no half-texel inset. The endpoints are the true
--   cell edges, which is what keeps NEAREST sampling pixel-identical:
--   unit art is nearest-neighbour (D-6) and drawn pixel-snapped, so a
--   fragment centre maps to texel @column*slotW + p + i@ and lands
--   inside the cell exactly as it did at the old stride. An inset would
--   SHIFT the sampled texels and break the pixel-identity #1259
--   requirement 7 asks for against the source frame, which spans 0..1
--   of its own image; the gutter buys linear-filter isolation
--   (epic #2072 D-3) WITHOUT moving a single sampled texel.
--
--   Exported rather than inlined into 'storageSampleAt' because the
--   @--preview units\/\<name\>@ viewer resolves its cells before any
--   texture handle exists (#1260, D-9): it has an 'AtlasAnimation' from
--   the same index and no 'ResidentAtlas' to sample through, and the
--   arithmetic it uses must BE this one rather than a second copy of
--   it. Nothing else about D-3's frozen arithmetic changed.
atlasCellUV ∷ AtlasAnimation → Int → Int → UVRect
atlasCellUV anim row column = (u0, v0, u1, v1)
  where
    atlasW = fromIntegral (aaAtlasWidth anim)  ∷ Float
    atlasH = fromIntegral (aaAtlasHeight anim) ∷ Float
    cellW  = aaCellWidth anim
    cellH  = aaCellHeight anim
    pad    = aaCellPadding anim
    x0     = column * (cellW + 2 * pad) + pad
    y0     = row * (cellH + 2 * pad) + pad
    u0 = fromIntegral x0 / atlasW
    u1 = fromIntegral (x0 + cellW) / atlasW
    v0 = fromIntegral y0 / atlasH
    v1 = fromIntegral (y0 + cellH) / atlasH

-- | The pixel dimensions a consumer must use to SIZE this frame.
--
--   An atlas sample answers from its own cell; a whole-image sample —
--   the direct default/directional sprite a T-pose falls back to —
--   falls through to the texture-size map, and to the supplied default
--   when the texture has not finished uploading. Routing both through
--   one helper is what stops a consumer from measuring a whole atlas
--   sheet where it means one frame.
frameDimensions
    ∷ HM.HashMap TextureHandle (Int, Int)
    → (Float, Float)                       -- ^ default (texture not yet sized)
    → FrameSample
    → (Float, Float)
frameDimensions texSizes dflt smp = case fsCell smp of
    Just (w, h) → (fromIntegral w, fromIntegral h)
    Nothing     → case HM.lookup (fsTexture smp) texSizes of
        Just (w, h) → (fromIntegral w, fromIntegral h)
        Nothing     → dflt
