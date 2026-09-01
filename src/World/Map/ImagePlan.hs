{-# LANGUAGE Strict #-}
-- | Checked planning for every image the world map produces (issue
--   #2020, WML-2).
--
--   Nothing used to stand between a world size and a Vulkan image: the
--   zoom atlas derived its dimensions with unchecked 'Int' arithmetic
--   and a @Double@ ceiling square root, allocated a contiguous RGBA8
--   buffer, and handed it to the driver — after every per-chunk pixel
--   block had already been generated and forced. A world whose atlas
--   cannot exist on the current device therefore cost the full pixel
--   corpus and then failed inside Vulkan.
--
--   This module is the admission gate that runs first. It is __pure__
--   and __total__: given source geometry and a pixel format it either
--   reports the intended dimensions, decoded byte count, layout and
--   source context, or a typed 'MapImageRefusal' naming exactly what
--   failed. Every intermediate is computed in 'Integer', so no step can
--   silently wrap, and every narrowing back to a downstream
--   representation ('Word32' extents, @VkDeviceSize@, the host's own
--   allocation bound) is checked rather than assumed.
--
--   == Reuse
--
--   The three sources cover what the map arc needs (design doc
--   @docs/world_map_level_of_detail_design.md@): the current whole-world
--   zoom atlas ('ZoomAtlasSource'), a tiled page image
--   ('TiledImageSource', D-11's 514×514 pages), and a plain bounded
--   image ('WholeImageSource', D-7's pixel-capped resident root). This
--   slice creates only the first; the other two exist so a later slice
--   reuses this planner without modifying it.
--
--   == The device ceiling is NOT decided here
--
--   'admitMapImage' takes the ceiling as a value ('MapImageCeiling')
--   rather than querying anything, because whether a ceiling applies at
--   all is a boot-mode question and this module has no business
--   answering it. "Engine.Map.ImageAdmission" resolves that and calls in
--   here.
module World.Map.ImagePlan
    ( -- * Sources and formats
      MapImageSource(..)
    , MapImageFormat(..)
    , mapImageBytesPerPixel
    , mapImageSourceText
      -- * Plans
    , MapImageLayout(..)
    , MapImagePlan(..)
    , planMapImage
      -- * Device ceiling
    , MapImageCeiling(..)
    , admitMapImage
      -- * Post-plan validation
    , checkPlannedCount
    , checkPlannedBlocks
    , checkUploadPayload
      -- * Refusals
    , MapImageRefusal(..)
    , mapImageRefusalText
      -- * Bounds and arithmetic
    , vulkanExtentBound
    , deviceSizeBound
    , hostAllocationBound
    , integerCeilSqrt
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import World.Generate.Config.Normalize (minimumWorldSize)
import World.ZoomMap.Types (zoomTileSize)

-- * Sources and formats

-- | What the image is being built from. The constructor is carried
--   through into the plan and into every refusal, so a diagnostic can
--   name the caller's own geometry rather than the derived pixels alone.
data MapImageSource
    = ZoomAtlasSource !Int
      -- ^ The whole-world zoom atlas for a world of this many chunks per
      --   side. Chunk count and tile size are DERIVED here, exactly as
      --   "World.ZoomMap.Cache.BuildPixels" derives them.
    | TiledImageSource !Int !Int
      -- ^ @tileCount@ square tiles of @tileEdge@ pixels, packed the same
      --   way the atlas packs chunks.
    | WholeImageSource !Int !Int
      -- ^ A plain @width × height@ image with no tile structure.
    deriving (Eq, Show)

-- | The decoded pixel format. Only the one the map actually produces
--   exists; the type is the seam a second one would be added at.
data MapImageFormat
    = MapImageRGBA8
    deriving (Eq, Show)

-- | Decoded bytes per pixel.
mapImageBytesPerPixel ∷ MapImageFormat → Int
mapImageBytesPerPixel MapImageRGBA8 = 4

-- | The format's name in a diagnostic.
mapImageFormatText ∷ MapImageFormat → Text
mapImageFormatText MapImageRGBA8 = "RGBA8"

-- | The source's own geometry, as the caller supplied it.
mapImageSourceText ∷ MapImageSource → Text
mapImageSourceText (ZoomAtlasSource n) =
    "world zoom atlas for worldSize " <> tshow n
mapImageSourceText (TiledImageSource count edge) =
    "tiled map image of " <> tshow count <> " tile(s) at "
    <> tshow edge <> "px"
mapImageSourceText (WholeImageSource w h) =
    "map image of " <> tshow w <> "×" <> tshow h <> " pixels"

-- * Plans

-- | How the planned pixels are laid out. A tiled plan is what
--   'checkPlannedBlocks' validates supplied blocks against.
data MapImageLayout
    = LayoutTiled
        { milTileCount  ∷ !Int   -- ^ Tiles the producer must supply
        , milTileEdge   ∷ !Int   -- ^ Square tile edge, in pixels
        , milTilesPerRow ∷ !Int  -- ^ Tiles per atlas row
        , milTileRows   ∷ !Int   -- ^ Atlas rows of tiles
        }
    | LayoutWhole
    deriving (Eq, Show)

-- | An ACCEPTED map image: its dimensions, its decoded byte count, how
--   it is laid out, and where it came from. Only 'planMapImage' and
--   'admitMapImage' construct one, so holding a 'MapImagePlan' is itself
--   the evidence that every check below passed.
data MapImagePlan = MapImagePlan
    { mipWidth     ∷ !Int
    , mipHeight    ∷ !Int
    , mipFormat    ∷ !MapImageFormat
    , mipByteCount ∷ !Int           -- ^ Decoded bytes: @w * h * bpp@
    , mipLayout    ∷ !MapImageLayout
    , mipSource    ∷ !MapImageSource
    } deriving (Eq, Show)

-- | The plan's dimensions, for a diagnostic.
planDimensionsText ∷ MapImagePlan → Text
planDimensionsText p = tshow (mipWidth p) <> "×" <> tshow (mipHeight p)

-- * Refusals

-- | Why a map image was refused. Each constructor carries exactly the
--   facts its own message needs and no others: a refusal never names a
--   dimension, byte count or device limit that was not successfully
--   calculated or queried (issue #2020, requirement 7).
data MapImageRefusal
    = MapImageInvalidGeometry !MapImageSource !Text
      -- ^ The supplied geometry, and the constraint it violates.
    | MapImageNotRepresentable !MapImageSource !Text !Integer !Text !Integer
      -- ^ Source, the quantity's name, its exact wide value, the target
      --   representation's name, and that target's bound.
    | MapImageExceedsDeviceLimit !MapImagePlan !Int
      -- ^ An otherwise valid plan against the device's real
      --   @maxImageDimension2D@.
    | MapImageDeviceUnavailable !MapImageSource !Text
      -- ^ A GPU-capable mode that has no device limit to check against.
    | MapImageCountMismatch !MapImagePlan !Text !Int !Int
      -- ^ Plan, what was counted, planned count, supplied count.
    | MapImageBlockSizeMismatch !MapImagePlan !Int !Int !Int
      -- ^ Plan, block index, planned block bytes, supplied block bytes.
    | MapImagePayloadLengthMismatch !MapImagePlan !Int
      -- ^ Plan and the supplied payload's actual length.
    deriving (Eq, Show)

-- | The player- and log-facing text for a refusal.
mapImageRefusalText ∷ MapImageRefusal → Text
mapImageRefusalText (MapImageInvalidGeometry src constraint) =
    "Refusing " <> mapImageSourceText src
    <> ": unsupported geometry — " <> constraint <> "."
mapImageRefusalText
  (MapImageNotRepresentable src quantity value target bound) =
    "Refusing " <> mapImageSourceText src <> ": " <> quantity <> " "
    <> tshow value <> " does not fit " <> target
    <> " (bound " <> tshow bound <> ")."
mapImageRefusalText (MapImageExceedsDeviceLimit plan limit) =
    "Refusing " <> mapImageSourceText (mipSource plan)
    <> ": planned image " <> planDimensionsText plan <> " ("
    <> tshow (mipByteCount plan) <> " decoded "
    <> mapImageFormatText (mipFormat plan)
    <> " bytes) exceeds the device maxImageDimension2D limit of "
    <> tshow limit <> "."
mapImageRefusalText (MapImageDeviceUnavailable src reason) =
    "Refusing " <> mapImageSourceText src
    <> ": the device maxImageDimension2D limit is required in this boot "
    <> "mode but is unavailable — " <> reason <> "."
mapImageRefusalText (MapImageCountMismatch plan what planned supplied) =
    "Refusing " <> mapImageSourceText (mipSource plan)
    <> ": planned image " <> planDimensionsText plan <> " expects "
    <> tshow planned <> " " <> what <> " but was given "
    <> tshow supplied <> "."
mapImageRefusalText
  (MapImageBlockSizeMismatch plan ix planned actual) =
    "Refusing " <> mapImageSourceText (mipSource plan)
    <> ": tile block " <> tshow ix <> " is " <> tshow actual
    <> " bytes, not the planned " <> tshow planned <> "."
mapImageRefusalText (MapImagePayloadLengthMismatch plan actual) =
    "Refusing " <> mapImageSourceText (mipSource plan)
    <> ": planned image " <> planDimensionsText plan <> " decodes to "
    <> tshow (mipByteCount plan) <> " bytes but the payload is "
    <> tshow actual <> " bytes."

-- * Bounds

-- | Vulkan's image extents are @Word32@ (@VkExtent2D@/@VkExtent3D@).
vulkanExtentBound ∷ Integer
vulkanExtentBound = toInteger (maxBound ∷ Word32)

-- | @VkDeviceSize@ is @Word64@ — the widest representation a byte count
--   has to survive on its way to the driver.
deviceSizeBound ∷ Integer
deviceSizeBound = toInteger (maxBound ∷ Word64)

-- | The host's own allocation bound: every buffer this engine builds is
--   sized by an 'Int'.
hostAllocationBound ∷ Integer
hostAllocationBound = toInteger (maxBound ∷ Int)

-- * Planning

-- | Plan an image from its source geometry, with no device ceiling
--   applied. Every representability check runs here, so a plan returned
--   from this function is already known to be describable to Vulkan and
--   allocatable by the host.
planMapImage ∷ MapImageFormat → MapImageSource
             → Either MapImageRefusal MapImagePlan
planMapImage fmt src = do
    (widthI, heightI, layoutOf) ← deriveGeometry src
    width  ← narrow src "image width" "a Word32 Vulkan extent"
                 vulkanExtentBound widthI
    height ← narrow src "image height" "a Word32 Vulkan extent"
                 vulkanExtentBound heightI
    -- Order is deliberate and documented: the representations the
    -- driver must be able to NAME come first (extents, then
    -- DeviceSize), and only then the host's own allocation bound. Both
    -- byte bounds are reachable — a Word32-legal image can exceed
    -- either — so neither check is dead.
    let bytesI = widthI * heightI * toInteger (mapImageBytesPerPixel fmt)
    _ ← narrow src "decoded byte count" "a Word64 Vulkan DeviceSize"
            deviceSizeBound bytesI
    bytes ← narrow src "decoded byte count" "a host Int allocation size"
                hostAllocationBound bytesI
    pure MapImagePlan
        { mipWidth     = width
        , mipHeight    = height
        , mipFormat    = fmt
        , mipByteCount = bytes
        , mipLayout    = layoutOf
        , mipSource    = src
        }

-- | Derive the intended pixel geometry, in 'Integer', from a source.
--   Unsupported geometry is refused here rather than silently producing
--   a plan the real producer would disagree with.
deriveGeometry ∷ MapImageSource
               → Either MapImageRefusal (Integer, Integer, MapImageLayout)
deriveGeometry src@(ZoomAtlasSource worldSize)
    | worldSize < minimumWorldSize
      ∨ worldSize `mod` minimumWorldSize ≢ 0 = Left $
        MapImageInvalidGeometry src $
            "worldSize must be a positive multiple of "
            <> tshow minimumWorldSize
            <> " (World.Generate.Config.Normalize.normalizeWorldSize), "
            <> "which is what makes it even and the atlas's chunk count "
            <> "agree with World.ZoomMap.Cache.BuildPixels"
    | otherwise =
        -- EXACTLY BuildPixels' own derivation: it walks u,v over
        -- @[-halfSize .. halfSize - 1]@ on a @w = halfSize * 2@ grid and
        -- retains the coordinates where @even (u + v)@, which is half of
        -- them. Restating it as @worldSize^2 / 2@ would silently
        -- disagree for any worldSize the guard above did not already
        -- reject.
        let w = 2 * (toInteger worldSize `div` 2)
            chunkCount = (w * w) `div` 2
        in tiledGeometry src chunkCount (toInteger zoomTileSize)
deriveGeometry src@(TiledImageSource tileCount tileEdge)
    | tileCount < 1 = Left $ MapImageInvalidGeometry src
        "a tiled map image needs at least one tile"
    | tileEdge < 1 = Left $ MapImageInvalidGeometry src
        "a tile edge must be at least one pixel"
    | otherwise =
        tiledGeometry src (toInteger tileCount) (toInteger tileEdge)
deriveGeometry src@(WholeImageSource w h)
    | w < 1 ∨ h < 1 = Left $ MapImageInvalidGeometry src
        "both image dimensions must be at least one pixel"
    | otherwise = Right (toInteger w, toInteger h, LayoutWhole)

-- | The square-ish packing "World.ZoomMap.ChunkTexture" performs, with
--   the layout metadata a producer needs to place its own blocks.
tiledGeometry ∷ MapImageSource → Integer → Integer
              → Either MapImageRefusal (Integer, Integer, MapImageLayout)
tiledGeometry src tileCount tileEdge =
    let perRow = integerCeilSqrt tileCount
        rows   = (tileCount + perRow - 1) `div` perRow
        width  = perRow * tileEdge
        height = rows * tileEdge
    in do
        -- The layout metadata is itself narrowed: a caller that indexes
        -- tiles with an Int must not be handed a count it cannot hold.
        count' ← narrow src "tile count" "a host Int"
                     hostAllocationBound tileCount
        edge'  ← narrow src "tile edge" "a host Int"
                     hostAllocationBound tileEdge
        perRow' ← narrow src "tiles per row" "a host Int"
                      hostAllocationBound perRow
        rows'  ← narrow src "tile rows" "a host Int"
                     hostAllocationBound rows
        pure ( width, height
             , LayoutTiled { milTileCount   = count'
                           , milTileEdge    = edge'
                           , milTilesPerRow = perRow'
                           , milTileRows    = rows' } )

-- | Narrow a wide intermediate to an 'Int', refusing with the exact
--   value and the bound it overran.
narrow ∷ MapImageSource → Text → Text → Integer → Integer
       → Either MapImageRefusal Int
narrow src quantity target bound value
    | value > bound = Left $
        MapImageNotRepresentable src quantity value target bound
    | otherwise = Right (fromInteger value)

-- | Exact integer ceiling square root — @ceiling (sqrt n)@ without ever
--   touching a 'Double'. Newton's method on 'Integer', so the answer is
--   right at every perfect-square boundary and at magnitudes where a
--   @Double@ has no bit left to round with.
integerCeilSqrt ∷ Integer → Integer
integerCeilSqrt n
    | n ≤ 0 = 0
    | otherwise =
        let r = integerFloorSqrt n
        in if r * r ≡ n then r else r + 1

-- | Exact integer floor square root (Newton's method).
integerFloorSqrt ∷ Integer → Integer
integerFloorSqrt n
    | n < 0 = 0
    | n < 2 = n
    | otherwise = go (initialGuess n)
  where
    go x =
        let y = (x + n `div` x) `div` 2
        in if y ≥ x then x else go y
    -- A starting point at or above the true root, so the iteration
    -- descends monotonically: 2^(ceil(bitlength/2)).
    initialGuess m = 1 `shiftL` ((integerBitLength m + 1) `div` 2)

integerBitLength ∷ Integer → Int
integerBitLength = go 0
  where
    go acc 0 = acc
    go acc m = go (acc + 1) (m `shiftR` 1)

-- * Device ceiling

-- | Whether a device ceiling applies to this admission, and if so what
--   it is. Resolved by the caller from the boot mode — see
--   "Engine.Map.ImageAdmission".
data MapImageCeiling
    = CeilingNotApplicable
      -- ^ A GPU-free boot mode: geometry and representability are still
      --   checked, but there is no device to bound the image, and the
      --   absence of one is expected rather than an error.
    | CeilingKnown !Int
      -- ^ The physical device's actual @maxImageDimension2D@.
    | CeilingUnavailable !Text
      -- ^ A GPU-capable boot mode whose limit could not be obtained.
      --   Carries the failed query's own description.
    deriving (Eq, Show)

-- | Plan an image and then apply the device ceiling. The plan is
--   computed FIRST so a geometry or representability failure reports
--   itself rather than being masked by a missing device.
admitMapImage ∷ MapImageCeiling → MapImageFormat → MapImageSource
              → Either MapImageRefusal MapImagePlan
admitMapImage mapCeiling fmt src = do
    plan ← planMapImage fmt src
    case mapCeiling of
        CeilingNotApplicable → Right plan
        CeilingUnavailable reason → Left $ MapImageDeviceUnavailable src reason
        CeilingKnown limit
            | mipWidth plan ≤ limit ∧ mipHeight plan ≤ limit → Right plan
            | otherwise → Left $ MapImageExceedsDeviceLimit plan limit

-- * Post-plan validation

-- | Check a producer's own count against the plan's tile count.
checkPlannedCount ∷ MapImagePlan → Text → Int → Either MapImageRefusal ()
checkPlannedCount plan what supplied = case mipLayout plan of
    LayoutWhole → Left $ MapImageInvalidGeometry (mipSource plan)
        "this plan has no tiled layout to count against"
    LayoutTiled { milTileCount = planned }
        | planned ≡ supplied → Right ()
        | otherwise → Left $
            MapImageCountMismatch plan what planned supplied

-- | Check supplied tile blocks: the number of them, and that each is
--   exactly the planned decoded size. Runs before any allocation or
--   copy, so a short block cannot be read past.
checkPlannedBlocks ∷ MapImagePlan → V.Vector BS.ByteString
                   → Either MapImageRefusal ()
checkPlannedBlocks plan blocks = case mipLayout plan of
    LayoutWhole → Left $ MapImageInvalidGeometry (mipSource plan)
        "this plan has no tiled layout to validate blocks against"
    LayoutTiled { milTileEdge = edge } → do
        checkPlannedCount plan "tile block(s)" (V.length blocks)
        let expected = edge * edge * mapImageBytesPerPixel (mipFormat plan)
            bad = [ (i, BS.length b)
                  | (i, b) ← zip [0 ∷ Int ..] (V.toList blocks)
                  , BS.length b ≢ expected ]
        case bad of
            [] → Right ()
            ((i, actual) : _) → Left $
                MapImageBlockSizeMismatch plan i expected actual

-- | Check an upload payload's length against the plan's decoded byte
--   count. The plan the caller passes is re-derived from the dimensions
--   it is about to hand Vulkan, so this is the planner's own checked
--   arithmetic rather than an ad-hoc @w * h * 4@ at the call site.
checkUploadPayload ∷ MapImagePlan → Int → Either MapImageRefusal ()
checkUploadPayload plan actual
    | mipByteCount plan ≡ actual = Right ()
    | otherwise = Left $ MapImagePayloadLengthMismatch plan actual
