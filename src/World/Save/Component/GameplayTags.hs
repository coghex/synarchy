-- | The one wire shape for general gameplay tag assignments (#2701),
--   shared by the three components that own them: @units@ and
--   @buildings@ (per page slice) and @world-pages@ (per page, beside the
--   location table).
--
--   On the wire an assignment list is @[(key, [Text])]@: ascending by
--   key, each tag list ascending, nonempty and duplicate-free, each tag
--   nonempty. Only the authoritative FORWARD assignments are written;
--   the reverse index is derived state and is rebuilt on restore. Tag
--   text is written verbatim — the tag system never reads meaning into
--   a tag's spelling — and the shape is plain cereal leaves ('Text' and
--   the owner's existing id leaf), so nothing here can drift with a
--   live record (the frozen-DTO boundary rule,
--   "World.Save.Component.Types").
--
--   Decoding is strict: 'tagAssignmentErrors' names every malformed entry
--   (an empty tag, an empty or duplicated tag list, a repeated key)
--   and the owning component reports them as validation failures.
--   Whether a well-formed assignment's TARGET exists is a different
--   question, answered by the integrity graph and the restore.
module World.Save.Component.GameplayTags
    ( TagAssignmentsDTO
    , encodeTagAssignments
    , decodeTagAssignments
    , tagAssignmentErrors
    ) where

import UPrelude
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Gameplay.Tags.Tag (GameplayTag, mkGameplayTag, gameplayTagText)

-- | One owner's forward assignments as written.
type TagAssignmentsDTO k = [(k, [Text])]

-- | Ascending keys, ascending unique tags.
encodeTagAssignments ∷ Map.Map k (Set.Set GameplayTag) → TagAssignmentsDTO k
encodeTagAssignments m =
    [ (k, gameplayTagText <$> Set.toAscList ts) | (k, ts) ← Map.toAscList m ]

-- | Total: a malformed entry decodes to whatever survives (an empty tag
--   is skipped, an empty set contributes nothing, a repeated key takes
--   the union), and 'tagAssignmentErrors' is what refuses the payload. The
--   two are separate so a validator over the decoded value can still
--   see what was wrong.
decodeTagAssignments ∷ Ord k ⇒ TagAssignmentsDTO k → Map.Map k (Set.Set GameplayTag)
decodeTagAssignments wire = Map.filter (not . Set.null) $ Map.fromListWith Set.union
    [ (k, Set.fromList (catMaybes (mkGameplayTag <$> ts))) | (k, ts) ← wire ]

-- | Every malformed entry, rendered with the given key renderer, in
--   wire order.
tagAssignmentErrors ∷ Ord k ⇒ (k → Text) → TagAssignmentsDTO k → [Text]
tagAssignmentErrors render wire =
    concatMap entryErrors wire
    ⧺ [ "gameplay tags for " <> render k <> " are listed more than once"
      | k ← repeated (map fst wire) ]
  where
    entryErrors (k, ts)
        | null ts = [ "gameplay tags for " <> render k <> " are an empty list" ]
        | otherwise =
            [ "gameplay tags for " <> render k <> " include an empty tag"
            | any isNothing (mkGameplayTag <$> ts) ]
            ⧺ [ "gameplay tags for " <> render k <> " repeat the tag '" <> t <> "'"
              | t ← repeated ts ]
    repeated xs = [ y | (y : _ : _) ← L.group (L.sort xs) ]
