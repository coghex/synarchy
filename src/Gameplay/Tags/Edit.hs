-- | One edit to one target's tag set, and its outcome (#2701; split out
--   by #2705 so the page-owned stores of "Gameplay.Tags.Page", which
--   "World.State.Types" holds, can apply one without importing the
--   live-owner layer). Re-exported unchanged by "Gameplay.Tags.Live".
module Gameplay.Tags.Edit
    ( TagEdit(..)
    , applyTagEdit
    , TagEditResult(..)
    ) where

import UPrelude
import qualified Data.Set as Set
import Gameplay.Tags.Tag (GameplayTag)

-- | One edit to one target's tag set, with "Gameplay.Tags.Registry"'s
--   meanings: add and remove are set union and difference (repeats
--   idempotent, absent tags ignored), replace makes the set the
--   target's complete set, and clear (or replacing with the empty set)
--   removes the target's entry. Shared tag names stay on every other
--   target.
data TagEdit
    = AddTagsEdit     !(Set.Set GameplayTag)
    | RemoveTagsEdit  !(Set.Set GameplayTag)
    | ReplaceTagsEdit !(Set.Set GameplayTag)
    | ClearTagsEdit
    deriving (Show, Eq)

applyTagEdit ∷ TagEdit → Set.Set GameplayTag → Set.Set GameplayTag
applyTagEdit = \case
    AddTagsEdit ts     → Set.union ts
    RemoveTagsEdit ts  → (`Set.difference` ts)
    ReplaceTagsEdit ts → const ts
    ClearTagsEdit      → const Set.empty

-- | Whether an edit found its target. A missing target is not an error
--   to raise but a fact to report: it changes nothing, and in
--   particular never creates an assignment.
data TagEditResult = TagEditApplied | TagEditNoTarget
    deriving (Show, Eq)

