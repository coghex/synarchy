-- | The durable physical-item → scenario-reference mapping (#2709,
--   SCN-09 of epic #2698).
--
--   A scenario names its items by a stable 'Scenario.Types.ScenarioId';
--   the engine names the same physical items by an allocated
--   'Item.Types.iiInstanceId'. The two are different identities and
--   neither stands in for the other (D-29): the reference is what a
--   scenario FILE keeps stable, the instance id is what a SESSION
--   allocates, and a recapture must write the reference an item was
--   constructed from rather than a number the allocator happened to
--   issue (the design's "export should preserve existing scenario
--   IDs").
--
--   The mapping is therefore session state with the same lifetime as the
--   item's gameplay tags: it follows the instance across every move,
--   dies with the instance, and rides ordinary saves (D-52) so a
--   save → load → capture still names the item by its reference. It is
--   owned by 'World.State.Types.wmItemScenarioRefs', written only by
--   "Gameplay.Tags.Live"'s attachment boundary and forgotten by
--   "Gameplay.Tags.ItemLifecycle" beside the item's tags, and persisted
--   by @core-session@ v3.
--
--   The reference is stored as TEXT, the 'Scenario.Types.scenarioIdText'
--   of the id. That loses nothing: an explicit id can never contain
--   @[@ and an automatic (document-path) id always does, so the two
--   spaces stay disjoint ('refIsAutomatic').
--
--   Pure and engine-free, so the wire layer can use it without pulling
--   in the scenario decoder.
module Scenario.Items.Refs
    ( ItemRefs
    , emptyItemRefs
    , nullItemRefs
    , itemRefsFromList
    , itemRefsToAscList
    , lookupItemRef
    , insertItemRefs
    , restrictItemRefs
    , itemRefIds
    , refIsAutomatic
    , itemRefTextError
    , itemRefsError
    , maxItemRefLength
    , stableHash64
    ) where

import UPrelude hiding (get)
import Data.Serialize (Serialize(..))
import qualified Data.Map.Strict as Map
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS

-- | Instance id → scenario reference text. Sparse: an item nothing
--   constructed from a scenario has no entry. Abstract so every writer
--   goes through the functions below and the map stays injective.
newtype ItemRefs = ItemRefs (Map.Map Word64 Text)
    deriving (Show, Eq)

-- | Cereal (for the in-memory 'World.Save.Types.SaveData' record that
--   derives it): the ascending assignment list, re-validated through
--   'itemRefsFromList' on the way in. The ordinary-save WIRE shape is
--   @core-session@'s own frozen-boundary DTO, not this instance.
instance Serialize ItemRefs where
    put = put ∘ itemRefsToAscList
    get = itemRefsFromList ⊚ get

-- | Union, the left operand winning any clash ('insertItemRefs').
instance Semigroup ItemRefs where
    a <> b = insertItemRefs (itemRefsToAscList b) a

instance Monoid ItemRefs where
    mempty = emptyItemRefs

emptyItemRefs ∷ ItemRefs
emptyItemRefs = ItemRefs Map.empty

nullItemRefs ∷ ItemRefs → Bool
nullItemRefs (ItemRefs m) = Map.null m

-- | Build from assignments. A reference two ids claim is kept by the
--   LOWEST id only, so the result is injective whatever the input; the
--   wire validator reports such an input before it ever gets here.
itemRefsFromList ∷ [(Word64, Text)] → ItemRefs
itemRefsFromList = foldl' step (ItemRefs Map.empty) ∘ Map.toAscList ∘ Map.fromList
  where
    step acc@(ItemRefs m) (i, r)
        | i ≡ 0 = acc
        | r `elem` Map.elems m = acc
        | otherwise = ItemRefs (Map.insert i r m)

itemRefsToAscList ∷ ItemRefs → [(Word64, Text)]
itemRefsToAscList (ItemRefs m) = Map.toAscList m

lookupItemRef ∷ Word64 → ItemRefs → Maybe Text
lookupItemRef i (ItemRefs m) = Map.lookup i m

-- | Add assignments. An id that already has a reference keeps it, and a
--   reference already held by another id is not given away twice.
insertItemRefs ∷ [(Word64, Text)] → ItemRefs → ItemRefs
insertItemRefs new (ItemRefs m0) = ItemRefs (foldl' step m0 new)
  where
    step m (i, r)
        | i ≡ 0 ∨ Map.member i m ∨ r `elem` Map.elems m = m
        | otherwise = Map.insert i r m

-- | Keep only the assignments whose id satisfies the predicate (the
--   forget path and the load scrub).
restrictItemRefs ∷ (Word64 → Bool) → ItemRefs → ItemRefs
restrictItemRefs keep (ItemRefs m) = ItemRefs (Map.filterWithKey (\i _ → keep i) m)

-- | Every instance id that carries a reference.
itemRefIds ∷ ItemRefs → HS.HashSet Word64
itemRefIds (ItemRefs m) = HS.fromList (Map.keys m)

-- | Whether a reference is an AUTOMATIC id (a document path such as
--   @units[2].inventory[0]@) rather than an authored explicit one.
refIsAutomatic ∷ Text → Bool
refIsAutomatic = T.any (≡ '[')

-- | The longest reference the wire accepts. Far above any authored id;
--   it bounds what a corrupt save can make the loader hold.
maxItemRefLength ∷ Int
maxItemRefLength = 512

-- | FNV-1a (64-bit) over a text's UTF-8 bytes: the stable mixer behind
--   every derived name and roll in scenario construction. Fixed: changing
--   it renames every derived reference.
stableHash64 ∷ Text → Word64
stableHash64 = BS.foldl' step 14695981039346656037 ∘ TE.encodeUtf8
  where step h b = (h `xor` fromIntegral b) * 1099511628211

-- | Why a reference text cannot be one, if it cannot: non-empty, bounded,
--   and drawn from the scenario id vocabulary (letters, digits,
--   @_ - : .@) plus the @[ ]@ of an automatic path id.
itemRefTextError ∷ Text → Maybe Text
itemRefTextError t
    | T.null t = Just "empty scenario reference"
    | T.length t > maxItemRefLength = Just "scenario reference longer than the limit"
    | not (T.all ok t) = Just "scenario reference outside the scenario id vocabulary"
    | otherwise = Nothing
  where ok c = (c ≥ 'a' ∧ c ≤ 'z') ∨ (c ≥ 'A' ∧ c ≤ 'Z')
             ∨ (c ≥ '0' ∧ c ≤ '9') ∨ c `elem` ("_-:.[]" ∷ String)

-- | The structural errors of a wire assignment list: id 0 (never
--   minted), a malformed reference, an id listed twice, a reference
--   claimed twice. Each is one message.
itemRefsError ∷ [(Word64, Text)] → [Text]
itemRefsError xs =
    [ "scenario references name item instance id 0, which names no item"
    | any ((≡ 0) ∘ fst) xs ]
    ⧺ [ "item " <> tshow i <> ": " <> e
      | (i, r) ← xs, Just e ← [itemRefTextError r] ]
    ⧺ [ "item " <> tshow i <> " is listed twice"
      | i ← dupes (map fst xs) ]
    ⧺ [ "scenario reference '" <> r <> "' is claimed by more than one item"
      | r ← dupes (map snd xs) ]
  where
    dupes ∷ Ord β ⇒ [β] → [β]
    dupes ys = Map.keys (Map.filter (> (1 ∷ Int)) (Map.fromListWith (+) [ (y, 1) | y ← ys ]))
