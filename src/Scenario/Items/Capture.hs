{-# LANGUAGE Strict #-}
-- | Reading live item trees back into scenario entries (#2709, SCN-09
--   of epic #2698): the capture half of the reusable item adapters.
--
--   Capture is a pure read of values the caller already holds — a
--   coherent owner snapshot (a unit's collections, a building's, a
--   ground store) plus the live identity mapping and tag lookups taken
--   from the same instant. It consumes no gameplay RNG, mints nothing,
--   moves nothing and realizes no pending loot, because it has no access
--   to any of those: there is no engine handle in this module.
--
--   __Every supported current value is written explicitly.__ An entry
--   states its fill, quality, condition, sharpness, own empty weight,
--   bulk (or its honest absence), internal storage capacities (or their
--   absence), temperature and — always — its current contents, so a
--   kit that has been emptied is captured as @contents: []@ and a
--   partially used one as exactly what is left, never as an omission
--   that a definition's default kit would later fill in. Nothing from
--   the scenario an item was built from is reused: the entry is the item
--   as it IS. Player observations, orders and progress are not item
--   state and do not appear.
--
--   __Identity.__ A node keeps the scenario reference it was built from
--   ("Scenario.Items.Refs") — an item constructed from @R@ is captured
--   as @R@. A node with no reference (crafted, found or spawned after
--   the scenario loaded) is given one that depends only on WHERE it
--   sits (its owner's reference, the collection, its position) and on
--   the references already taken; never on an allocated instance id, a
--   ground-row id, a display group or a tag name. A reference that is
--   an automatic document path (@units[2].inventory[0]@) cannot be
--   written as an explicit id, so it is captured in its explicit form
--   (@units.2.inventory.0@): the same words without brackets, which is
--   the form every later capture of that item repeats.
--
--   Each physical instance is captured once. The state that threads
--   through a whole capture ('CaptureState') is what keeps two nodes
--   from ever claiming one reference.
module Scenario.Items.Capture
    ( CaptureCtx(..)
    , CaptureState
    , emptyCaptureState
    , reserveMapped
    , captureNodes
    , Captured(..)
      -- * Owners
    , UnitCapture(..)
    , captureUnitItems
    , BuildingCapture(..)
    , captureBuildingItems
    , captureGroundItems
      -- * Names
    , explicitRefOf
    ) where

import UPrelude
import Data.List (sortOn)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import Building.Types (BuildingInstance(..))
import Gameplay.Tags.Types (GameplayTag)
import qualified Item.Ground as G
import Item.Ground (GroundItems(..))
import Item.Types (ItemInstance(..), ItemStorage(..))
import Scenario.Types
import Unit.Types.Instance (UnitInstance(..))

-- | What capture reads besides the trees: the live identity mapping and
--   tag lookups, taken at the same instant as the owner snapshot.
data CaptureCtx = CaptureCtx
    { ccRef  ∷ Word64 → Maybe Text
      -- ^ The scenario reference an instance was built from, if any
      --   ('World.State.Types.wmItemScenarioRefs').
    , ccTags ∷ Word64 → Set.Set GameplayTag
      -- ^ The instance's tags ('World.State.Types.wmItemTags'); empty
      --   for an untagged one.
    }

-- | The references taken so far in one capture, and by which instance.
newtype CaptureState = CaptureState (HM.HashMap Text Word64)

emptyCaptureState ∷ CaptureState
emptyCaptureState = CaptureState HM.empty

-- | One capture's output for a list of roots.
data Captured = Captured
    { capEntries ∷ ![ItemEntry]
    , capRefs    ∷ ![(Word64, Text)]
      -- ^ Instance id → the explicit reference its entry carries, for
      --   every captured node (parents before children). A later
      --   service that persists the capture's identities (SCN-13/14)
      --   installs exactly this.
    }

-- | The explicit-id form of a reference: an automatic path
--   (@units[2].inventory[0]@) loses its brackets (@units.2.inventory.0@);
--   an explicit id is unchanged.
explicitRefOf ∷ Text → Text
explicitRefOf = T.concatMap f
  where f '[' = "."
        f ']' = ""
        f c = T.singleton c

-- | Reserve the references the given trees ALREADY hold, before anything
--   is captured, so a reference minted for an unmapped node can never
--   take one a later mapped node is entitled to. When two instances
--   claim one reference the first in traversal order keeps it; the
--   other is treated as unmapped.
reserveMapped ∷ CaptureCtx → [ItemInstance] → CaptureState → CaptureState
reserveMapped ctx roots st0 = foldl' step st0 (concatMap flat roots)
  where
    flat i = i : concatMap flat (iiContents i)
    step st@(CaptureState m) i = case explicitRefOf ⊚ ccRef ctx (iiInstanceId i) of
        Just r | not (HM.member r m) → CaptureState (HM.insert r (iiInstanceId i) m)
        _ → st

-- | Capture a list of roots, each with the base reference an UNMAPPED
--   node would take. Contents are captured recursively and in order; a
--   child's base is its parent's final reference plus @.contents.i@.
captureNodes ∷ CaptureCtx → CaptureState → [(Text, ItemInstance)]
             → (CaptureState, Captured)
captureNodes ctx st0 roots = (stN, Captured (concatMap fst parts) (concatMap snd parts))
  where
    (stN, parts) = mapAccumL' one st0 roots
    one st (base, inst) =
        let (st', e, rs) = node st base inst in (st', ([e], rs))
    node st base inst =
        let iid = iiInstanceId inst
            ref = resolve st base iid
            CaptureState m0 = st
            st1 = CaptureState (HM.insert ref iid m0)
            (st2, kids) = mapAccumL' (\s (i, c) → let (s', e, rs) = node s (ref <> ".contents." <> tshow (i ∷ Int)) c
                                                  in (s', (e, rs)))
                                     st1 (zip [0 ..] (iiContents inst))
            entry = ItemEntry
                { ieId = ExplicitId ref
                , ieTags = Set.toAscList (ccTags ctx iid)
                , ieDefinition = iiDefName inst
                , ieFill = Authored (iiCurrentFill inst)
                , ieQuality = Authored (iiQuality inst)
                , ieCondition = Authored (iiCondition inst)
                , ieSharpness = Authored (iiSharpness inst)
                , ieWeight = Authored (iiWeight inst)
                , ieBulk = Authored (iiBulk inst)
                , ieStorage = Authored (fmap toCap (iiStorage inst))
                , ieTemperature = Authored (maybe AtAmbient TrackedTemp (iiTemp inst))
                , ieContents = Authored (map fst kids)
                }
        in (st2, entry, (iid, ref) : concatMap snd kids)
    toCap s = StorageCapacity (isWeightCapacity s) (isBulkCapacity s)
    resolve (CaptureState m) base iid =
        case explicitRefOf ⊚ ccRef ctx iid of
            -- a mapped node keeps its reference unless a DIFFERENT
            -- instance already holds it
            Just r | maybe True (≡ iid) (HM.lookup r m) → r
            _ → freshRef m (explicitRefOf base)

-- | The first free spelling of a base: the base itself, then @-2@,
--   @-3@, … — deterministic, and independent of anything but the
--   references already taken.
freshRef ∷ HM.HashMap Text Word64 → Text → Text
freshRef m base = head' [ c | c ← base : [ base <> "-" <> tshow n | n ← [2 ∷ Int ..] ]
                           , not (HM.member c m) ]
  where head' (x : _) = x
        head' [] = base

mapAccumL' ∷ (s → a → (s, b)) → s → [a] → (s, [b])
mapAccumL' _ s [] = (s, [])
mapAccumL' f s (x : xs) =
    let (s', y) = f s x
        (s'', ys) = mapAccumL' f s' xs
    in (s'', y : ys)

-- * Owners

-- | A unit's captured item collections, ready for the unit's own entry.
data UnitCapture = UnitCapture
    { ucInventory   ∷ ![ItemEntry]
    , ucEquipment   ∷ ![(Text, ItemEntry)]   -- ^ sorted by slot
    , ucAccessories ∷ ![ItemEntry]
    , ucCaptured    ∷ !Captured
    }

-- | Capture a unit's inventory, equipment (by sorted slot) and
--   accessories. @unitRef@ is the unit's own reference.
captureUnitItems ∷ CaptureCtx → CaptureState → Text → UnitInstance
                 → (CaptureState, UnitCapture)
captureUnitItems ctx st0 unitRef u =
    ( st3
    , UnitCapture
        { ucInventory = capEntries invC
        , ucEquipment = zip (map fst slots) (capEntries eqC)
        , ucAccessories = capEntries accC
        , ucCaptured = Captured (concatMap capEntries' [invC, eqC, accC])
                                (concatMap capRefs [invC, eqC, accC])
        } )
  where
    capEntries' = capEntries
    slots = sortOn fst (HM.toList (uiEquipment u))
    st0' = reserveMapped ctx (uiInventory u ⧺ map snd slots ⧺ uiAccessories u) st0
    (st1, invC) = captureNodes ctx st0'
        [ (unitRef <> ".inventory." <> tshow (i ∷ Int), x) | (i, x) ← zip [0 ..] (uiInventory u) ]
    (st2, eqC) = captureNodes ctx st1
        [ (unitRef <> ".equipment." <> slot, x) | (slot, x) ← slots ]
    (st3, accC) = captureNodes ctx st2
        [ (unitRef <> ".accessories." <> tshow (i ∷ Int), x) | (i, x) ← zip [0 ..] (uiAccessories u) ]

-- | A building's captured item collections.
data BuildingCapture = BuildingCapture
    { bcStorage   ∷ ![ItemEntry]
    , bcDelivered ∷ ![ItemEntry]
      -- ^ Flat, ordered by definition name then stated order; the
      --   scenario format lists delivered materials and groups them at
      --   construction.
    , bcCaptured  ∷ !Captured
    }

captureBuildingItems ∷ CaptureCtx → CaptureState → Text → BuildingInstance
                     → (CaptureState, BuildingCapture)
captureBuildingItems ctx st0 buildingRef b =
    ( st2
    , BuildingCapture
        { bcStorage = capEntries stC
        , bcDelivered = capEntries dlC
        , bcCaptured = Captured (capEntries stC ⧺ capEntries dlC) (capRefs stC ⧺ capRefs dlC)
        } )
  where
    delivered = [ (d, i, x) | (d, xs) ← sortOn fst (HM.toList (biMaterialsDelivered b))
                            , (i, x) ← zip [0 ∷ Int ..] xs ]
    st0' = reserveMapped ctx (biStorage b ⧺ [ x | (_, _, x) ← delivered ]) st0
    (st1, stC) = captureNodes ctx st0'
        [ (buildingRef <> ".storage." <> tshow (i ∷ Int), x) | (i, x) ← zip [0 ..] (biStorage b) ]
    (st2, dlC) = captureNodes ctx st1
        [ (buildingRef <> ".delivered." <> d <> "." <> tshow i, x) | (d, i, x) ← delivered ]

-- | Capture a ground store. Rows are ordered by position, definition and
--   structure (never by row id or instance id), and an unmapped item's
--   reference is @ground.n@ for its place in that order, so two
--   captures of the same arena agree however the store happened to be
--   filled.
captureGroundItems ∷ CaptureCtx → CaptureState → GroundItems
                   → (CaptureState, [(Float, Float, ItemEntry)], Captured)
captureGroundItems ctx st0 g = (st1, zip3 xs ys (capEntries c), c)
  where
    rows = sortOn key (map snd (sortOn fst (HM.toList (gisItems g))))
    key r = (G.giX r, G.giY r, iiDefName (G.giInst r), structure (G.giInst r))
    -- Everything about a tree EXCEPT its instance ids, so the order
    -- never leans on the allocator.
    structure i = T.intercalate "|"
        [ iiDefName i, tshow (iiCurrentFill i), tshow (iiQuality i)
        , tshow (iiCondition i), tshow (iiWeight i), tshow (iiSharpness i)
        , tshow (iiTemp i), tshow (iiBulk i)
        , tshow (fmap (\st → (isWeightCapacity st, isBulkCapacity st)) (iiStorage i))
        , "[" <> T.intercalate "," (map structure (iiContents i)) <> "]" ]
    xs = map G.giX rows
    ys = map G.giY rows
    st0' = reserveMapped ctx (map G.giInst rows) st0
    (st1, c) = captureNodes ctx st0'
        [ ("ground." <> tshow (n ∷ Int), G.giInst r) | (n, r) ← zip [0 ..] rows ]
