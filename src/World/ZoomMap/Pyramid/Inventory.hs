{-# LANGUAGE Strict #-}
-- | The streamable level inventory of one world's map pyramid
--   (issue #2298, WML-5; design decisions D-3, D-7, D-11).
--
--   The inventory answers "what does this world's map consist of, and
--   what does it cost" WITHOUT materialising a single page: every
--   figure is derived from the world size through
--   "World.ZoomMap.Pyramid.Address"'s geometry and priced through
--   "World.Map.ImagePlan", which is the one place in the tree allowed
--   to turn geometry into decoded bytes.
--
--   == Where each number comes from
--
--     * A PAGE is planned as @'TiledImageSource' 1 'mapPageEdge'@ — ONE
--       514-square tile. Passing a level's page count instead would
--       plan the atlas those pages would pack into, padding included,
--       which is not what a page is.
--     * The ROOT is planned as @'WholeImageSource' width height@: it is
--       a plain bounded image with no tile structure (D-7).
--     * Aggregates are accumulated in 'Integer' from the accepted
--       plans' own byte counts and narrowed back to 'Int' only after a
--       bound check, so no world-scale product can wrap silently
--       (requirement 8).
--
--   Nothing here is activated: no caller in the shipping engine
--   consults this module, and the existing atlas path is untouched.
module World.ZoomMap.Pyramid.Inventory
    ( -- * Inventory
      MapPyramidLevel(..)
    , MapPyramidInventory(..)
    , mapPyramidInventory
    , mapPyramidLevel
    , mapPyramidMandatoryLevels
      -- * Refusals
    , MapPyramidRefusal(..)
    , mapPyramidRefusalText
    ) where

import UPrelude
import World.Map.ImagePlan
    ( MapImageFormat(..), MapImagePlan(..), MapImageRefusal
    , MapImageSource(..), hostAllocationBound, mapImageRefusalText
    , planMapImage )
import World.ZoomMap.Pyramid.Address
import World.ZoomMap.Pyramid.Reduce
    (MapReduceRefusal, mapReduceRefusalText)

-- * Inventory

-- | One level of the pyramid: its logical raster, its page grid, the
--   accepted plan for ONE of its pages, and the exact decoded byte
--   total of all of them.
data MapPyramidLevel = MapPyramidLevel
    { mplLevel        ∷ !Int
      -- ^ 0 is finest.
    , mplWidth        ∷ !Int
      -- ^ Logical raster width in texels (payload space, no gutter).
    , mplHeight       ∷ !Int
    , mplPagesU       ∷ !Int
    , mplPagesV       ∷ !Int
    , mplPageCount    ∷ !Int
      -- ^ @mplPagesU * mplPagesV@, narrowed after a checked product.
    , mplPagePlan     ∷ !MapImagePlan
      -- ^ The accepted plan for one 514-square page of this level.
    , mplDecodedBytes ∷ !Int
      -- ^ @mplPageCount * mipByteCount mplPagePlan@, exact.
    } deriving (Eq, Show)

-- | Every level of one world's pyramid, plus the two derived level
--   selections D-7 requires.
data MapPyramidInventory = MapPyramidInventory
    { mpiGeometry          ∷ !MapGeometry
    , mpiLevels            ∷ ![MapPyramidLevel]
      -- ^ Levels @0 .. mpiRootLevel@, finest first.
    , mpiRootLevel         ∷ !Int
      -- ^ The smallest level whose longest logical axis is at most
      --   'mapRootAxisCap'.
    , mpiCoarseCutoffLevel ∷ !Int
      -- ^ The smallest level whose longest logical axis is at most
      --   'mapCoarseAxisCap'. Never above 'mpiRootLevel'.
    , mpiRootPlan          ∷ !MapImagePlan
      -- ^ The root level planned as one whole resident image.
    , mpiMandatoryBytes    ∷ !Int
      -- ^ Decoded bytes of the mandatory range, cutoff through root
      --   inclusive, as pages.
    } deriving (Eq, Show)

-- | Enumerate one world's pyramid. Pure, total, and free of any page.
mapPyramidInventory ∷ Int → Either MapPyramidRefusal MapPyramidInventory
mapPyramidInventory worldSize = do
    geom ← first MapPyramidAddress (mapGeometry worldSize)
    let root   = mapRootLevel geom
        cutoff = mapCoarseCutoffLevel geom
    levels ← mapM (levelOf geom) [0 .. root]
    rootPlan ← first MapPyramidImage $ planMapImage MapImageRGBA8 $
        WholeImageSource (mapLevelWidth geom root)
                         (mapLevelHeight geom root)
    mandatory ← narrow "mandatory decoded byte total" $
        sum [ toInteger (mplDecodedBytes l)
            | l ← levels, mplLevel l ≥ cutoff, mplLevel l ≤ root ]
    pure MapPyramidInventory
        { mpiGeometry          = geom
        , mpiLevels            = levels
        , mpiRootLevel         = root
        , mpiCoarseCutoffLevel = cutoff
        , mpiRootPlan          = rootPlan
        , mpiMandatoryBytes    = mandatory
        }

-- | One level's row of the inventory.
levelOf ∷ MapGeometry → Int → Either MapPyramidRefusal MapPyramidLevel
levelOf geom l = do
    -- One PAGE, not the level's atlas: a page is a single 514-square
    -- upload, and its byte count is the planner's, not this module's.
    pagePlan ← first MapPyramidImage $
        planMapImage MapImageRGBA8 (TiledImageSource 1 mapPageEdge)
    let pagesU = mapLevelPagesU geom l
        pagesV = mapLevelPagesV geom l
    count ← narrow "page count" (toInteger pagesU * toInteger pagesV)
    total ← narrow "decoded byte total"
                (toInteger count * toInteger (mipByteCount pagePlan))
    pure MapPyramidLevel
        { mplLevel        = l
        , mplWidth        = mapLevelWidth geom l
        , mplHeight       = mapLevelHeight geom l
        , mplPagesU       = pagesU
        , mplPagesV       = pagesV
        , mplPageCount    = count
        , mplPagePlan     = pagePlan
        , mplDecodedBytes = total
        }

-- | Look one level up in an inventory.
mapPyramidLevel ∷ MapPyramidInventory → Int
                → Either MapPyramidRefusal MapPyramidLevel
mapPyramidLevel inv l =
    case [ row | row ← mpiLevels inv, mplLevel row ≡ l ] of
        (row : _) → Right row
        []        → Left $ MapPyramidAddress $
            MapAddressLevelOutOfRange l (mpiRootLevel inv)

-- | The mandatory generated coverage: cutoff through root inclusive.
mapPyramidMandatoryLevels ∷ MapPyramidInventory → [MapPyramidLevel]
mapPyramidMandatoryLevels inv =
    [ row | row ← mpiLevels inv
          , mplLevel row ≥ mpiCoarseCutoffLevel inv
          , mplLevel row ≤ mpiRootLevel inv ]

-- | Narrow a wide inventory intermediate, refusing with the exact
--   value and the bound it overran.
narrow ∷ Text → Integer → Either MapPyramidRefusal Int
narrow quantity value
    | value > hostAllocationBound =
        Left $ MapPyramidNotRepresentable quantity value hostAllocationBound
    | otherwise = Right (fromInteger value)

first ∷ (α → β) → Either α γ → Either β γ
first f (Left a)  = Left (f a)
first _ (Right c) = Right c

-- * Refusals

-- | Why a pyramid operation was refused. The three wrapped cases keep
--   each producer's own diagnostic intact rather than flattening it
--   into prose.
data MapPyramidRefusal
    = MapPyramidAddress !MapAddressRefusal
    | MapPyramidImage !MapImageRefusal
    | MapPyramidReduce !MapReduceRefusal
    | MapPyramidNotRepresentable !Text !Integer !Integer
      -- ^ The quantity's name, its exact wide value, and its bound.
    | MapPyramidInconsistentParams !Text
      -- ^ Generation parameters that disagree with the map geometry
      --   they were paired with.
    | MapPyramidCellSource !Text
      -- ^ What the cell source got wrong, in its own words.
    deriving (Eq, Show)

-- | The log-facing text for a pyramid refusal.
mapPyramidRefusalText ∷ MapPyramidRefusal → Text
mapPyramidRefusalText (MapPyramidAddress r) = mapAddressRefusalText r
mapPyramidRefusalText (MapPyramidImage r)   = mapImageRefusalText r
mapPyramidRefusalText (MapPyramidReduce r)  = mapReduceRefusalText r
mapPyramidRefusalText (MapPyramidNotRepresentable quantity value bound) =
    "Refusing the map pyramid: " <> quantity <> " " <> tshow value
    <> " does not fit a host Int (bound " <> tshow bound <> ")."
mapPyramidRefusalText (MapPyramidInconsistentParams reason) =
    "Refusing the map pyramid: " <> reason <> "."
mapPyramidRefusalText (MapPyramidCellSource reason) =
    "Refusing the map pyramid: the cell source " <> reason <> "."
