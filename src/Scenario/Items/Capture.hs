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
    , newCaptureState
    , reserveNames
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
import Numeric (showHex)
import Scenario.Items.Refs (maxItemRefLength, refIsAutomatic, stableHash64)
import Building.Types (BuildingInstance(..))
import Gameplay.Tags.Types (GameplayTag, gameplayTagText)
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

-- | The state to start a whole capture with: every reference ANY tree it
--   will capture already holds is reserved up front. Start here, over
--   every owner's trees, so that no owner captured first can mint a name
--   an item captured later is entitled to keep (the per-call
--   reservations inside 'captureNodes' and the owner functions only see
--   their own trees).
newCaptureState ∷ CaptureCtx → [ItemInstance] → CaptureState
newCaptureState ctx roots = reserveMapped ctx roots emptyCaptureState

-- | Reserve names that belong to OTHER entries of the document being
--   written (a unit's or building's id): scenario ids share one
--   namespace, so a minted item name must never equal one.
reserveNames ∷ [Text] → CaptureState → CaptureState
reserveNames names (CaptureState m) =
    CaptureState (foldl' (\acc n → HM.insertWith (\_ old → old) n 0 acc) m names)

-- | Reserve the references the given trees ALREADY hold, before anything
--   is captured, so a reference minted for an unmapped node can never
--   take one a later mapped node is entitled to. When two instances
--   claim one reference the first in traversal order keeps it; the
--   other is treated as unmapped.
reserveMapped ∷ CaptureCtx → [ItemInstance] → CaptureState → CaptureState
reserveMapped ctx roots st0 = foldl' claim (foldl' claim st0 authored) converted
  where
    flat i = i : concatMap flat (iiContents i)
    mapped = [ (iiInstanceId i, r) | i ← concatMap flat roots
             , Just r ← [ccRef ctx (iiInstanceId i)] ]
    -- An authored explicit reference is the document's own spelling and
    -- is claimed FIRST; a converted automatic path (@units[0].inventory[0]@
    -- → @units.0.inventory.0@) only gets that spelling if no authored
    -- reference already has it, and is disambiguated otherwise. Two
    -- distinct references must never collapse onto one name.
    authored = [ (i, spelling r) | (i, r) ← mapped, not (refIsAutomatic r) ]
    converted = [ (i, spelling r) | (i, r) ← mapped, refIsAutomatic r ]
    claim st@(CaptureState m) (i, r)
        | HM.member r m = st
        | otherwise = CaptureState (HM.insert r i m)

-- | Capture a list of roots, each with the base reference an UNMAPPED
--   node would take. Contents are captured recursively and in order; a
--   child's base is its parent's final reference plus @.contents.i@.
captureNodes ∷ CaptureCtx → CaptureState → [(Text, ItemInstance)]
             → (CaptureState, Captured)
captureNodes ctx st00 roots = (stN, Captured (concatMap fst parts) (concatMap snd parts))
  where
    -- every reference these roots already hold is taken before any node
    -- is named, whatever order they sit in (idempotent for a caller that
    -- already reserved them)
    st0 = reserveMapped ctx (map snd roots) st00
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
        case spelling ⊚ ccRef ctx iid of
            -- a mapped node keeps its reference unless a DIFFERENT
            -- instance already holds it; then it is disambiguated from
            -- its OWN spelling, not renamed to the position it sits at
            Just r | maybe True (≡ iid) (HM.lookup r m) → r
                   | otherwise → freshRef m r
            Nothing → freshRef m (spelling base)

-- | The explicit-id spelling of a reference an item ALREADY holds,
--   verbatim: an admitted reference is at most 'maxItemRefLength'
--   characters and the bracketless conversion never lengthens it, so an
--   existing id is preserved exactly, however close to the limit.
spelling ∷ Text → Text
spelling = explicitRefOf

-- | A GENERATED name that fits the save: kept when it fits, otherwise
--   cut short with a stable hash of the whole so two long names stay
--   distinct. Only generated names (position-derived, or a numeric
--   suffix added to resolve a clash) are ever shortened.
bounded ∷ Text → Text
bounded c
    | T.length c ≤ maxItemRefLength = c
    | otherwise = T.take (maxItemRefLength - 17) c <> "-"
        <> T.justifyRight 16 '0' (T.pack (showHex (stableHash64 c) ""))

-- | The first free spelling of a base: the base itself, then @-2@,
--   @-3@, … — deterministic, and independent of anything but the
--   references already taken. Every candidate fits the save.
freshRef ∷ HM.HashMap Text Word64 → Text → Text
freshRef m base = head' [ c | c0 ← base : [ base <> "-" <> tshow n | n ← [2 ∷ Int ..] ]
                           , let c = bounded c0, not (HM.member c m) ]
  where head' (x : _) = x
        head' [] = bounded base

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
    -- Everything the capture will EXPORT about a tree except its
    -- instance ids: physical state, the scenario reference it carries
    -- and its tags, at every node. The order therefore never leans on
    -- the allocator or on the ground-row ids; two rows that still tie
    -- are indistinguishable in the output, so their order cannot show.
    -- A STRUCTURED key: a tag may contain any character, so nothing is
    -- joined into one string that two different tag sets could share.
    structure i = Sig
        [ iiDefName i, tshow (iiCurrentFill i), tshow (iiQuality i)
        , tshow (iiCondition i), tshow (iiWeight i), tshow (iiSharpness i)
        , tshow (iiTemp i), tshow (iiBulk i)
        , tshow (fmap (\st → (isWeightCapacity st, isBulkCapacity st)) (iiStorage i)) ]
        (ccRef ctx (iiInstanceId i))
        (map gameplayTagText (Set.toAscList (ccTags ctx (iiInstanceId i))))
        (map structure (iiContents i))
    xs = map G.giX rows
    ys = map G.giY rows
    st0' = reserveMapped ctx (map G.giInst rows) st0
    (st1, c) = captureNodes ctx st0'
        [ ("ground." <> tshow (n ∷ Int), G.giInst r) | (n, r) ← zip [0 ..] rows ]

-- | One node of the ground-order key: physical fields, the reference AS
--   HELD (not its explicit-id spelling, which would tie an automatic path
--   with an explicit id that spells like it), tags and children, each kept
--   as its own element.
data Sig = Sig [Text] (Maybe Text) [Text] [Sig]
    deriving (Eq, Ord)
