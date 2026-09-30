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
    , plantTargetPage
    , plantTargetId
    , ItemTarget
    , mkItemTarget
    , itemTargetId
    , TileTarget
    , mkTileTarget
    , tileTargetPage
    , tileTargetX
    , tileTargetY
    , tileTargetZ
    , TagTarget(..)
    , targetCategory
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import qualified Data.Set as Set
import qualified Data.Text as T
import Building.Types (BuildingId(..))
import Location.Instance (LocationInstanceId(..))
import Unit.Types.Manager (UnitId(..))
import World.Chunk.Types (ChunkCoord)
import World.Flora.Identity (FloraInstanceId, isFloraInstanceIdNone)
import World.Generate.Coordinates (canonicalTileFrameWith)
import World.Page.Types (WorldPageId(..))

-- * Tags

-- | An opaque gameplay label. The only validation is that the text is
--   nonempty: an empty label names nothing a script could ask for.
--   Anything else is a legitimate label; the tag system never reads
--   meaning into a tag's spelling.
newtype GameplayTag = GameplayTag Text
    deriving (Show, Eq, Ord)

instance NFData GameplayTag where
    rnf (GameplayTag t) = rnf t

-- | 'Nothing' for the empty string.
mkGameplayTag ∷ Text → Maybe GameplayTag
mkGameplayTag t
    | T.null t  = Nothing
    | otherwise = Just (GameplayTag t)

gameplayTagText ∷ GameplayTag → Text
gameplayTagText (GameplayTag t) = t

-- * Nonempty sets

-- | A 'Set.Set' that is never empty. The constructor is not exported,
--   so 'nonEmptySet' is the only way in and an empty record can never
--   be stored in either registry index.
newtype NonEmptySet a = NonEmptySet (Set.Set a)
    deriving (Show, Eq, Ord)

instance NFData a ⇒ NFData (NonEmptySet a) where
    rnf (NonEmptySet s) = rnf s

-- | 'Nothing' for the empty set.
nonEmptySet ∷ Set.Set a → Maybe (NonEmptySet a)
nonEmptySet s
    | Set.null s = Nothing
    | otherwise  = Just (NonEmptySet s)

nonEmptySetToSet ∷ NonEmptySet a → Set.Set a
nonEmptySetToSet (NonEmptySet s) = s

singletonNonEmptySet ∷ a → NonEmptySet a
singletonNonEmptySet = NonEmptySet . Set.singleton

-- | Inserting never empties a set, so this needs no check.
insertNonEmptySet ∷ Ord a ⇒ a → NonEmptySet a → NonEmptySet a
insertNonEmptySet x (NonEmptySet s) = NonEmptySet (Set.insert x s)

-- | 'Nothing' once the last element is gone.
deleteNonEmptySet ∷ Ord a ⇒ a → NonEmptySet a → Maybe (NonEmptySet a)
deleteNonEmptySet x (NonEmptySet s) = nonEmptySet (Set.delete x s)

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

-- | One real flora occurrence. The planted namespace of
--   'FloraInstanceId' is a per-page allocator cursor, so two pages can
--   issue the same planted id and the page is part of the key.
data PlantTarget = PlantTarget !WorldPageId !FloraInstanceId
    deriving (Show, Eq, Ord)

instance NFData PlantTarget where
    rnf (PlantTarget p i) = rnf (unWorldPageId p) `seq` rnf i

-- | 'Nothing' for the reserved non-identity
--   'World.Flora.Identity.floraInstanceIdNone', which the crop-plot
--   adapter synthesizes and which names no plant.
mkPlantTarget ∷ WorldPageId → FloraInstanceId → Maybe PlantTarget
mkPlantTarget page fid
    | isFloraInstanceIdNone fid = Nothing
    | otherwise                 = Just (PlantTarget page fid)

plantTargetPage ∷ PlantTarget → WorldPageId
plantTargetPage (PlantTarget p _) = p

plantTargetId ∷ PlantTarget → FloraInstanceId
plantTargetId (PlantTarget _ i) = i

-- | One physical item, by its process-unique
--   'Item.Types.iiInstanceId'. That counter starts at 1 and id 0 means
--   "no identity" ('Item.Types.itemMatches' treats only @iid > 0@ as an
--   identity), so 0 is refused.
newtype ItemTarget = ItemTarget Word64
    deriving (Show, Eq, Ord)

instance NFData ItemTarget where
    rnf (ItemTarget i) = rnf i

-- | 'Nothing' for the non-identity 0.
mkItemTarget ∷ Word64 → Maybe ItemTarget
mkItemTarget 0 = Nothing
mkItemTarget i = Just (ItemTarget i)

itemTargetId ∷ ItemTarget → Word64
itemTargetId (ItemTarget i) = i

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
