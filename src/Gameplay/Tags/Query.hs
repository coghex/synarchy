-- | Native evaluation of gameplay tag queries (#2700, epic #2698
--   SCN-02).
--
--   A query is a typed expression — tag-filter and universe leaves under
--   union, intersection, difference and complement — evaluated entirely
--   here against the registry's reverse indexes with 'Data.Set'
--   operations. There is no string syntax to parse and no persistent
--   cache.
--
--   The universe (the objects that currently EXIST, per category) is
--   supplied by the caller on every call and never stored: the registry
--   only knows tagged targets, and "units minus defenders" must include
--   the untagged units it has never heard of. Every result is a subset
--   of the supplied universe, so a tag held by a target outside it —
--   another category, or a tile whose chunk is not loaded — never leaks
--   into a result, and the assignment itself is untouched.
module Gameplay.Tags.Query
    ( -- * Queries
      TagQuery(..)
    , TagFilter(..)
    , tagFilter
    , tagged
    , unionQueries
    , intersectQueries
      -- * Universes and results
    , TargetSets(..)
    , QueryUniverse
    , QueryResult
    , emptyTargetSets
    , unitUniverse
    , buildingUniverse
    , locationUniverse
    , plantUniverse
    , itemUniverse
    , tileUniverse
    , targetSetsFromList
    , targetSetsToList
    , targetSetsSize
    , selectCategories
      -- * Evaluation
    , evaluateQuery
    , queryList
    , queryCount
    , queryExists
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import Data.List (sortOn)
import qualified Data.Set as Set
import Building.Types (BuildingId(..))
import Gameplay.Tags.Registry
import Gameplay.Tags.Types
import Unit.Types.Manager (UnitId)

-- * Queries

-- | The all/any/none shorthand. A target matches when it carries EVERY
--   'tfAll' tag, AT LEAST ONE 'tfAny' tag and NO 'tfNone' tag. Nonempty
--   groups combine conjunctively; an empty group imposes no
--   restriction, so the empty filter matches the whole universe. Sets
--   make a repeated tag the same as one occurrence.
data TagFilter = TagFilter
    { tfAll  ∷ !(Set.Set GameplayTag)
    , tfAny  ∷ !(Set.Set GameplayTag)
    , tfNone ∷ !(Set.Set GameplayTag)
    } deriving (Show, Eq)

-- | Build a filter from @all@, @any@ and @none@ lists.
tagFilter ∷ [GameplayTag] → [GameplayTag] → [GameplayTag] → TagFilter
tagFilter a o n = TagFilter (Set.fromList a) (Set.fromList o) (Set.fromList n)

-- | A composable query. Leaves are a tag filter ('QMatch') or the whole
--   selected universe ('QUniverse'); 'QComplement' is relative to the
--   selected universe.
data TagQuery
    = QMatch !TagFilter
    | QUniverse
    | QUnion !TagQuery !TagQuery
    | QIntersection !TagQuery !TagQuery
    | QDifference !TagQuery !TagQuery
    | QComplement !TagQuery
    deriving (Show, Eq)

-- | Targets carrying one tag.
tagged ∷ GameplayTag → TagQuery
tagged t = QMatch (TagFilter (Set.singleton t) Set.empty Set.empty)

-- | The union of a list of queries; the empty list matches nothing.
unionQueries ∷ [TagQuery] → TagQuery
unionQueries []       = QComplement QUniverse
unionQueries (q : qs) = foldl' QUnion q qs

-- | The intersection of a list of queries; the empty list matches the
--   whole universe.
intersectQueries ∷ [TagQuery] → TagQuery
intersectQueries []       = QUniverse
intersectQueries (q : qs) = foldl' QIntersection q qs

-- * Universes and results

-- | One ordered set of targets per category. The same shape is both a
--   query universe (what exists) and a query result (what matched).
data TargetSets = TargetSets
    { tsUnits     ∷ !(Set.Set UnitId)
    , tsBuildings ∷ !(Set.Set BuildingId)
    , tsLocations ∷ !(Set.Set LocationTarget)
    , tsPlants    ∷ !(Set.Set PlantTarget)
    , tsItems     ∷ !(Set.Set ItemTarget)
    , tsTiles     ∷ !(Set.Set TileTarget)
    } deriving (Show, Eq)

-- | The objects a query ranges over, supplied per call by the objects'
--   owners. A category left empty is not selected. For tiles it is the
--   set of currently LOADED tiles.
type QueryUniverse = TargetSets

-- | Always a subset of the universe the query ran against.
type QueryResult = TargetSets

instance Semigroup TargetSets where
    TargetSets a b c d e f <> TargetSets a' b' c' d' e' f' =
        TargetSets (Set.union a a') (Set.union b b') (Set.union c c')
                   (Set.union d d') (Set.union e e') (Set.union f f')

instance Monoid TargetSets where
    mempty = emptyTargetSets

-- | 'BuildingId' has no 'NFData'; set elements are already in WHNF and
--   a 'BuildingId' in WHNF is fully evaluated.
instance NFData TargetSets where
    rnf (TargetSets a b c d e f) =
        rnf a `seq` rnf (Set.map unBuildingId b) `seq` rnf c
        `seq` rnf d `seq` rnf e `seq` rnf f

emptyTargetSets ∷ TargetSets
emptyTargetSets = TargetSets Set.empty Set.empty Set.empty
                             Set.empty Set.empty Set.empty

unitUniverse ∷ Set.Set UnitId → QueryUniverse
unitUniverse s = emptyTargetSets { tsUnits = s }

buildingUniverse ∷ Set.Set BuildingId → QueryUniverse
buildingUniverse s = emptyTargetSets { tsBuildings = s }

locationUniverse ∷ Set.Set LocationTarget → QueryUniverse
locationUniverse s = emptyTargetSets { tsLocations = s }

plantUniverse ∷ Set.Set PlantTarget → QueryUniverse
plantUniverse s = emptyTargetSets { tsPlants = s }

itemUniverse ∷ Set.Set ItemTarget → QueryUniverse
itemUniverse s = emptyTargetSets { tsItems = s }

-- | The loaded tiles.
tileUniverse ∷ Set.Set TileTarget → QueryUniverse
tileUniverse s = emptyTargetSets { tsTiles = s }

targetSetsFromList ∷ [TagTarget] → TargetSets
targetSetsFromList = foldl' add emptyTargetSets
  where
    add ts = \case
        TargetUnit k     → ts { tsUnits     = Set.insert k (tsUnits ts) }
        TargetBuilding k → ts { tsBuildings = Set.insert k (tsBuildings ts) }
        TargetLocation k → ts { tsLocations = Set.insert k (tsLocations ts) }
        TargetPlant k    → ts { tsPlants    = Set.insert k (tsPlants ts) }
        TargetItem k     → ts { tsItems     = Set.insert k (tsItems ts) }
        TargetTile k     → ts { tsTiles     = Set.insert k (tsTiles ts) }

-- | Every target, in the documented order: category ('TagCategory'
--   order), then the key's 'Ord'. Duplicate-free by construction.
targetSetsToList ∷ TargetSets → [TagTarget]
targetSetsToList ts = concat
    [ TargetUnit     <$> Set.toAscList (tsUnits ts)
    , TargetBuilding <$> Set.toAscList (tsBuildings ts)
    , TargetLocation <$> Set.toAscList (tsLocations ts)
    , TargetPlant    <$> Set.toAscList (tsPlants ts)
    , TargetItem     <$> Set.toAscList (tsItems ts)
    , TargetTile     <$> Set.toAscList (tsTiles ts) ]

-- | Total targets; O(1) per category.
targetSetsSize ∷ TargetSets → Int
targetSetsSize ts = Set.size (tsUnits ts) + Set.size (tsBuildings ts)
    + Set.size (tsLocations ts) + Set.size (tsPlants ts)
    + Set.size (tsItems ts) + Set.size (tsTiles ts)

-- | Keep only the listed categories, emptying the rest.
selectCategories ∷ [TagCategory] → TargetSets → TargetSets
selectCategories cats ts = TargetSets
    { tsUnits     = keep UnitCategory     (tsUnits ts)
    , tsBuildings = keep BuildingCategory (tsBuildings ts)
    , tsLocations = keep LocationCategory (tsLocations ts)
    , tsPlants    = keep PlantCategory    (tsPlants ts)
    , tsItems     = keep ItemCategory     (tsItems ts)
    , tsTiles     = keep TileCategory     (tsTiles ts) }
  where
    keep ∷ TagCategory → Set.Set k → Set.Set k
    keep c s | c `elem` cats = s
             | otherwise     = Set.empty

-- * Evaluation

-- | Evaluate within one category. Every branch returns a subset of the
--   universe @u@: the leaves are clipped to it and complement is taken
--   relative to it.
evalIn ∷ Ord k ⇒ CategoryIndex k → Set.Set k → TagQuery → Set.Set k
evalIn ix u = go
  where
    go = \case
        QUniverse         → u
        QMatch f          → matchFilter ix u f
        QUnion a b        → Set.union (go a) (go b)
        QIntersection a b → let x = go a
                            in if Set.null x then x else Set.intersection x (go b)
        QDifference a b   → let x = go a
                            in if Set.null x then x else Set.difference x (go b)
        QComplement a     → Set.difference u (go a)

-- | The all/any/none filter. Positive @all@ groups start from the
--   smallest member set and stop at the first empty intersection;
--   exclusions subtract the union of the excluded tags' members.
matchFilter ∷ Ord k ⇒ CategoryIndex k → Set.Set k → TagFilter → Set.Set k
matchFilter ix u (TagFilter allTs anyTs noneTs) = excluded
  where
    members t = indexMembers t ix
    required  = foldl' meet u (sortOn Set.size (members <$> Set.toList allTs))
    meet acc s | Set.null acc = acc
               | otherwise    = Set.intersection acc s
    alternative
        | Set.null anyTs ∨ Set.null required = required
        | otherwise = Set.intersection required
                        (Set.unions (members <$> Set.toList anyTs))
    excluded
        | Set.null noneTs ∨ Set.null alternative = alternative
        | otherwise = Set.difference alternative
                        (Set.unions (members <$> Set.toList noneTs))

-- | The complete result, per category. Nothing is added to the registry
--   and the universe is not retained.
evaluateQuery ∷ TagRegistry → QueryUniverse → TagQuery → QueryResult
evaluateQuery r u q = TargetSets
    { tsUnits     = evalIn (registryUnits r)     (tsUnits u)     q
    , tsBuildings = evalIn (registryBuildings r) (tsBuildings u) q
    , tsLocations = evalIn (registryLocations r) (tsLocations u) q
    , tsPlants    = evalIn (registryPlants r)    (tsPlants u)    q
    , tsItems     = evalIn (registryItems r)     (tsItems u)     q
    , tsTiles     = evalIn (registryTiles r)     (tsTiles u)     q
    }

-- | Every match, complete and duplicate-free, in the documented order
--   (see 'targetSetsToList'). Never truncated.
queryList ∷ TagRegistry → QueryUniverse → TagQuery → [TagTarget]
queryList r u q = targetSetsToList (evaluateQuery r u q)

-- | The number of matches: the sizes of the native result sets, with
--   no target list built.
queryCount ∷ TagRegistry → QueryUniverse → TagQuery → Int
queryCount r u q = targetSetsSize (evaluateQuery r u q)

-- | Whether anything matches. Categories are evaluated in order and
--   evaluation stops at the first nonempty one.
queryExists ∷ TagRegistry → QueryUniverse → TagQuery → Bool
queryExists r u q = or
    [ not (Set.null (evalIn (registryUnits r)     (tsUnits u)     q))
    , not (Set.null (evalIn (registryBuildings r) (tsBuildings u) q))
    , not (Set.null (evalIn (registryLocations r) (tsLocations u) q))
    , not (Set.null (evalIn (registryPlants r)    (tsPlants u)    q))
    , not (Set.null (evalIn (registryItems r)     (tsItems u)     q))
    , not (Set.null (evalIn (registryTiles r)     (tsTiles u)     q)) ]
