-- | The gameplay tag itself and the nonempty set the registry stores
--   (#2700), split out of "Gameplay.Tags.Types" by #2701.
--
--   This module imports no object identity at all, which is the reason
--   it exists: the live owners of the unit, building and location
--   assignments ('Unit.Types.Manager.UnitManager',
--   'Building.Types.BuildingManager', 'Location.Instance.LocationInstances')
--   hold a "Gameplay.Tags.Index" built from these types, while
--   "Gameplay.Tags.Types" names the targets BY those owners' identities.
--   Both are re-exported from "Gameplay.Tags.Types", so the #2700
--   interface is unchanged.
module Gameplay.Tags.Tag
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
    , mapNonEmptySetMonotonic
    ) where

import UPrelude hiding (get)
import Control.DeepSeq (NFData(..))
import Data.Serialize (Serialize(..))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- * Tags

-- | An opaque gameplay label. The only validation is that the text is
--   nonempty: an empty label names nothing a script could ask for.
--   Anything else is a legitimate label; the tag system never reads
--   meaning into a tag's spelling.
newtype GameplayTag = GameplayTag Text
    deriving (Show, Eq, Ord)

instance NFData GameplayTag where
    rnf (GameplayTag t) = rnf t

-- | The in-memory snapshot encoding only (#2701): the snapshot records
--   that carry assignments derive 'Serialize' as a structural value,
--   and this keeps them total. The save FORMAT never uses it — each
--   save component writes its own frozen text DTO and validates it on
--   decode. Decoding the empty string fails, so this cannot construct
--   a tag 'mkGameplayTag' would refuse.
instance Serialize GameplayTag where
    put (GameplayTag t) = put (TE.encodeUtf8 t)
    get = do
        bytes ← get
        case TE.decodeUtf8' bytes of
            Left err → fail ("gameplay tag is not UTF-8: " <> show err)
            Right t  → maybe (fail "empty gameplay tag") pure (mkGameplayTag t)

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

-- | Map a strictly monotonic function over the elements. Monotonicity
--   is the caller's obligation; it keeps the set's order, and a
--   nonempty set stays nonempty.
mapNonEmptySetMonotonic ∷ (a → b) → NonEmptySet a → NonEmptySet b
mapNonEmptySetMonotonic f (NonEmptySet s) = NonEmptySet (Set.mapMonotonic f s)
