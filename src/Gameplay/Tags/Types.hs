-- | Gameplay tag vocabulary: the tag type, the typed targets a tag can
--   be attached to, and the nonempty set the registry stores (#2700,
--   epic #2698 SCN-02).
--
--   General gameplay tags are optional script-facing labels. They carry
--   no gameplay meaning of their own and are deliberately a DIFFERENT
--   type from 'Unit.Faction.Profile.FactionTag': a faction tag decides
--   relationships, a gameplay tag decides nothing, and the type system
--   keeps one from being passed where the other is expected.
--
--   The tag and the nonempty set are defined in "Gameplay.Tags.Tag"
--   (#2701, so the live owners can index them without importing the
--   targets) and re-exported here unchanged.
--
--   The contract (identity scoping, index invariants, query semantics,
--   ordering and memory) is written up in @docs\/gameplay_tags.md@.
module Gameplay.Tags.Types
    ( -- * Tags
      GameplayTag
    , mkGameplayTag
    , gameplayTagText
      -- * Nonempty sets
    , NonEmptySet
    , nonEmptySet
    , nonEmptySetToSet
    , singletonNonEmptySet
    , insertNonEmptySet
    , deleteNonEmptySet
      -- * Targets
    , TagCategory(..)
    , allTagCategories
    , LocationTarget(..)
    , PlantTarget
    , mkPlantTarget
    , mkPlotTarget
    , plotTargetFromStored
    , plantTargetPage
    , plantTargetId
    , plantTargetPlotTile
    , isPlotTarget
    , ItemTarget
    , mkItemTarget
    , itemTargetId
    , TileTarget
    , mkTileTarget
    , tileTargetFromStored
    , tileTargetPage
    , tileTargetX
    , tileTargetY
    , tileTargetZ
    , TagTarget(..)
    , targetCategory
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import Building.Types (BuildingId(..))
import Location.Instance (LocationInstanceId(..))
import Unit.Types.Manager (UnitId(..))
import World.Chunk.Types (ChunkCoord)
import World.Flora.Identity (FloraInstanceId, isFloraInstanceIdNone)
import World.Generate.Coordinates (canonicalTile, canonicalTileFrameWith)
import World.Page.Types (WorldPageId(..))
import Gameplay.Tags.Tag

-- * Targets

-- | The six taggable kinds. The declaration order IS the documented
--   result order: every list result sorts by category first, in this
--   order, then by the category's own key.
data TagCategory
    = UnitCategory
    | BuildingCategory
    | LocationCategory
    | PlantCategory
    | ItemCategory
    | TileCategory
    deriving (Show, Eq, Ord, Enum, Bounded)

allTagCategories ∷ [TagCategory]
allTagCategories = [minBound .. maxBound]

-- | A placed location. 'LocationInstanceId's are allocated per world
--   page (see "Location.Instance"), so the page is part of the key.
data LocationTarget = LocationTarget
    { ltPage ∷ !WorldPageId
    , ltId   ∷ !LocationInstanceId
    } deriving (Show, Eq, Ord)

instance NFData LocationTarget where
    rnf (LocationTarget p i) = rnf (unWorldPageId p) `seq` rnf i

-- | One plant, in one of the two owners plants have (#2705).
--
--   * A real flora OCCURRENCE — a wild plant or a planted row crop —
--     by its stable 'FloraInstanceId'. The planted namespace is a
--     per-page allocator cursor, so two pages can issue the same planted
--     id and the page is part of the key; co-tenants of one tile are
--     separate occurrences.
--   * A groundcover CROP PLOT, which has no occurrence identity at all
--     (its growth instance carries the reserved
--     'World.Flora.Identity.floraInstanceIdNone'): it is owned by the
--     page's tile-keyed plot map, so it is addressed by the same
--     canonical tile pair that map is keyed by.
--
--   The two constructors are a TYPED distinction: a plot is never
--   confused with an occurrence, and neither with a terrain cell
--   ('TileTarget'). Occurrences order before plots.
data PlantTarget
    = PlantOccurrence !WorldPageId !FloraInstanceId
    | PlantPlot !WorldPageId !Int !Int
    deriving (Show, Eq, Ord)

instance NFData PlantTarget where
    rnf (PlantOccurrence p i) = rnf (unWorldPageId p) `seq` rnf i
    rnf (PlantPlot p x y)     = rnf (unWorldPageId p) `seq` rnf x `seq` rnf y

-- | An occurrence target. 'Nothing' for the reserved non-identity
--   'World.Flora.Identity.floraInstanceIdNone', which the crop-plot
--   adapter synthesizes and which names no plant.
mkPlantTarget ∷ WorldPageId → FloraInstanceId → Maybe PlantTarget
mkPlantTarget page fid
    | isFloraInstanceIdNone fid = Nothing
    | otherwise                 = Just (PlantOccurrence page fid)

-- | A crop-plot target, through the SAME canonicalisation the plot map
--   keys by ('World.Generate.Coordinates.canonicalTile' at the page's
--   wrap size), so every alias of the plot's tile names one target.
mkPlotTarget ∷ Int → WorldPageId → Int → Int → PlantTarget
mkPlotTarget worldSize page gx gy =
    let (cgx, cgy) = canonicalTile worldSize gx gy
    in PlantPlot page cgx cgy

-- | A plot target for a plot-map KEY (#2705): the key is already the
--   canonical tile, so no canonicalisation is needed or wanted.
plotTargetFromStored ∷ WorldPageId → Int → Int → PlantTarget
plotTargetFromStored = PlantPlot

plantTargetPage ∷ PlantTarget → WorldPageId
plantTargetPage (PlantOccurrence p _) = p
plantTargetPage (PlantPlot p _ _)     = p

-- | The occurrence identity; 'Nothing' for a plot.
plantTargetId ∷ PlantTarget → Maybe FloraInstanceId
plantTargetId (PlantOccurrence _ i) = Just i
plantTargetId PlantPlot{}           = Nothing

-- | The plot's canonical tile; 'Nothing' for an occurrence.
plantTargetPlotTile ∷ PlantTarget → Maybe (Int, Int)
plantTargetPlotTile (PlantPlot _ x y) = Just (x, y)
plantTargetPlotTile PlantOccurrence{} = Nothing

isPlotTarget ∷ PlantTarget → Bool
isPlotTarget = isJust . plantTargetPlotTile

-- | One map tile on one page, at its CANONICAL (stored-frame) global
--   coordinate. Ordered by page, then @gx@, @gy@, @z@.
data TileTarget = TileTarget !WorldPageId !Int !Int !Int
    deriving (Show, Eq, Ord)

instance NFData TileTarget where
    rnf (TileTarget p x y z) =
        rnf (unWorldPageId p) `seq` rnf x `seq` rnf y `seq` rnf z

-- | The only way to build a 'TileTarget'. The raw coordinate goes
--   through 'World.Generate.Coordinates.canonicalTileFrameWith' with
--   the caller's chunk canonicalisation — normally
--   @'World.Chunk.Residency.canonicalChunkCoord' params@ for the page,
--   which is arena-aware — so every seam alias of one physical tile
--   yields the same target and can never create a second entry.
mkTileTarget ∷ (ChunkCoord → ChunkCoord)
             → WorldPageId → Int → Int → Int → TileTarget
mkTileTarget canon page gx gy z =
    let (_, _, (dgx, dgy)) = canonicalTileFrameWith canon gx gy
    in TileTarget page (gx + dgx) (gy + dgy) z

-- | A target for a cell read OFF a stored chunk (#2705): its chunk key
--   and local position are already the stored frame, so no
--   canonicalisation is needed or wanted. Never for a caller-supplied
--   coordinate, which goes through 'mkTileTarget'.
tileTargetFromStored ∷ WorldPageId → Int → Int → Int → TileTarget
tileTargetFromStored = TileTarget

tileTargetPage ∷ TileTarget → WorldPageId
tileTargetPage (TileTarget p _ _ _) = p

tileTargetX, tileTargetY, tileTargetZ ∷ TileTarget → Int
tileTargetX (TileTarget _ x _ _) = x
tileTargetY (TileTarget _ _ y _) = y
tileTargetZ (TileTarget _ _ _ z) = z

-- | Any taggable object. The constructor names the category, so unit 12
--   and building 12 are distinct targets. The derived 'Ord' is the
--   documented result order: category ('TagCategory' order), then key.
data TagTarget
    = TargetUnit     !UnitId
    | TargetBuilding !BuildingId
    | TargetLocation !LocationTarget
    | TargetPlant    !PlantTarget
    | TargetItem     !ItemTarget
    | TargetTile     !TileTarget
    deriving (Show, Eq, Ord)

instance NFData TagTarget where
    rnf (TargetUnit u)     = rnf u
    rnf (TargetBuilding b) = rnf (unBuildingId b)
    rnf (TargetLocation l) = rnf l
    rnf (TargetPlant p)    = rnf p
    rnf (TargetItem i)     = rnf i
    rnf (TargetTile t)     = rnf t

targetCategory ∷ TagTarget → TagCategory
targetCategory = \case
    TargetUnit _     → UnitCategory
    TargetBuilding _ → BuildingCategory
    TargetLocation _ → LocationCategory
    TargetPlant _    → PlantCategory
    TargetItem _     → ItemCategory
    TargetTile _     → TileCategory
