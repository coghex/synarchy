{-# LANGUAGE Strict #-}
-- | Storage-neutral animation-frame addressing (#1259, TEX-3).
--
--   An 'Animation' stores its frames in exactly ONE of two
--   representations ('AnimStorage'), and every consumer reads a frame
--   through the same storage-neutral 'FrameSample'. Making the two
--   modes a SUM rather than two optional fields is the point: D-10
--   allows exactly one resident representation per logical animation,
--   and requirement 6 of #1259 forbids mixing them within one
--   animation — a shape that cannot represent a mixture cannot drift
--   into one.
--
--   * 'StorageLegacy' — the historical per-frame representation: one
--     texture handle per frame, each its own whole image. Buildings
--     ('Building.Render') and every unit shipped today are on this
--     mode, and TEX-6 owns its eventual removal.
--   * 'StorageAtlas' — one compiled image per animation (D-2), one
--     texture handle, one bindless slot; a frame is a UV sub-rect of
--     it, addressed through the generated index ('Unit.Atlas.Index').
--
--   A 'FrameSample' therefore carries the stable bindless handle
--   (#286 — never a slot), the frame's own UV endpoints WITHIN that
--   handle's image, the frame's pixel dimensions when the storage
--   knows them, and the mirror flag. Legacy samples span the whole
--   image and report no dimensions of their own, so their consumers
--   keep measuring 'Engine.Core.Capability.RenderView.rvTextureSizeRef'
--   exactly as before; an ATLAS sample's dimensions are the CELL's, so
--   no consumer ever measures the whole sheet where it means one frame
--   (requirement 4).
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
    , storageLegacyFrames
    , storageIsAtlas
    , frameDimensions
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
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
    , aaColumns      ∷ !Int
    , aaRows         ∷ !Int
    , aaFlip         ∷ !Bool
    , aaFps          ∷ !Float
    , aaLoop         ∷ !Bool
    , aaDirections   ∷ !(Map.Map Direction AtlasDirectionRow)
    , aaSourceDigest ∷ !Text
      -- ^ The compiler's per-animation digest over its SOURCE frames.
      --   Recorded and reported, never recomputed. Source freshness IS
      --   checked at load — @Unit.Atlas.Index@'s @planUnitAtlasStorage@
      --   covers every declaration this digest is taken over, and its
      --   @validateSourceFrame@ compares each declared frame's decoded
      --   pixels against the atlas cell that must hold them — just not
      --   by reproducing the compiler's field encoding, which would
      --   have to match @repr()@ of a Python float exactly.
    , aaAtlasDigest  ∷ !Text
    } deriving (Show, Eq)

-- | A compiled animation whose atlas image is resident: the metadata
--   plus the ONE stable texture handle its cells share (#286).
data ResidentAtlas = ResidentAtlas
    { raAnimation ∷ !AtlasAnimation
    , raTexture   ∷ !TextureHandle
    } deriving (Show, Eq)

-- | Where one animation's frames live. Exactly one representation per
--   animation — see the module header.
data AnimStorage
    = StorageLegacy !(Map.Map Direction (V.Vector TextureHandle))
    | StorageAtlas  !ResidentAtlas
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
      -- ^ The frame's own pixel dimensions when the storage knows them
      --   (an atlas cell, from the index). 'Nothing' for a legacy
      --   frame, whose image IS the frame — its consumer measures the
      --   texture-size map as it always has.
    , fsFlipX   ∷ !Bool
    } deriving (Show, Eq)

-- | A whole-image sample: the legacy frame shape, and what the T-pose
--   fallbacks resolve to.
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
storageFrameCount (StorageLegacy frames) dir = V.length <$> Map.lookup dir frames
storageFrameCount (StorageAtlas res) dir =
    adrFrameCount <$> Map.lookup dir (aaDirections (raAnimation res))

-- | Every authored direction's real frame count.
storageFrameCounts ∷ AnimStorage → Map.Map Direction Int
storageFrameCounts (StorageLegacy frames) = V.length <$> frames
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
storageSampleAt (StorageLegacy frames) dir idx flipX = do
    fs ← Map.lookup dir frames
    tex ← fs V.!? idx
    pure (wholeImageSample tex flipX)
storageSampleAt (StorageAtlas res) dir idx flipX =
    case Map.lookup dir (aaDirections anim) of
        Nothing → Nothing
        Just row
            | idx < 0 ∨ idx ≥ adrFrameCount row → Nothing
            | otherwise →
                let atlasW = fromIntegral (aaAtlasWidth anim)  ∷ Float
                    atlasH = fromIntegral (aaAtlasHeight anim) ∷ Float
                    cellW  = aaCellWidth anim
                    cellH  = aaCellHeight anim
                    -- Exact integer cell geometry, divided into
                    -- normalized UV at the CELL EDGES. Deliberately no
                    -- half-texel inset: unit art is nearest-neighbour
                    -- (D-6) and drawn pixel-snapped, so a fragment
                    -- centre maps to texel `idx*cellW + i` and lands
                    -- inside the cell; an inset would SHIFT the sampled
                    -- texels and break the pixel-identity requirement 7
                    -- asks for against the legacy path, which spans
                    -- 0..1 of its own image.
                    u0 = fromIntegral (idx * cellW) / atlasW
                    u1 = fromIntegral ((idx + 1) * cellW) / atlasW
                    v0 = fromIntegral (adrRow row * cellH) / atlasH
                    v1 = fromIntegral ((adrRow row + 1) * cellH) / atlasH
                in Just FrameSample
                    { fsTexture = raTexture res
                    , fsUV      = (u0, v0, u1, v1)
                    , fsCell    = Just (cellW, cellH)
                    , fsFlipX   = flipX
                    }
  where
    anim = raAnimation res

-- | The per-frame handle map when — and only when — this animation is
--   on the legacy representation.
--
--   Buildings reuse the shared 'Unit.Types.Def.Animation' and are
--   never compiled to atlases, so 'Building.Render' reads its frames
--   through here and keeps its exact current behaviour: an atlas-backed
--   animation would answer 'Nothing' and take the same branch a missing
--   animation already takes.
storageLegacyFrames ∷ AnimStorage → Maybe (Map.Map Direction (V.Vector TextureHandle))
storageLegacyFrames (StorageLegacy frames) = Just frames
storageLegacyFrames (StorageAtlas _)       = Nothing

-- | Is this animation atlas-backed?
storageIsAtlas ∷ AnimStorage → Bool
storageIsAtlas (StorageAtlas _)  = True
storageIsAtlas (StorageLegacy _) = False

-- | The pixel dimensions a consumer must use to SIZE this frame.
--
--   An atlas sample answers from its own cell; a legacy sample — whose
--   image is the frame — falls through to the texture-size map, and to
--   the supplied default when the texture has not finished uploading.
--   Routing both through one helper is what stops a consumer from
--   measuring a whole atlas sheet where it means one frame.
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
