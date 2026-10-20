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
    , reserveDocumentId
    , ItemSig(..)
    , RefKey(..)
    , itemSig
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
    , nameReferenced
    , DocKey(..)
    , nameDocumentRefs
    ) where

import UPrelude
import Data.List (sortOn)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.HashSet as HS
import GHC.Float (castFloatToWord32)
import qualified Data.Text as T
import Numeric (showHex)
import Scenario.Items.Refs (maxItemRefLength, refIsAutomatic, stableHash64)
import qualified Data.Map.Strict as Map
import Building.Types (BuildingId)
import Location.Instance (LocationInstanceId)
import Unit.Types.Manager (UnitId)
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

-- | The names taken so far in one capture.
--
--   __The naming rule.__ Scenario ids share ONE namespace, so every name
--   an item can be written under falls in one of four tiers, and a lower
--   tier never displaces a higher one, whatever order things are
--   reserved or captured in:
--
--   1. an id of ANOTHER document entry (a unit, building, location, …),
--      given to 'newCaptureState' — never taken by an item;
--   2. an item's authored EXPLICIT reference, verbatim;
--   3. an item's AUTOMATIC reference in its bracketless form
--      (@units[0].inventory[0]@ → @units.0.inventory.0@), if no higher
--      tier has that spelling;
--   4. a GENERATED name (position-derived), if nothing above has it.
--
--   An item that cannot have its tier's spelling is written as that
--   spelling plus @-2@, @-3@, … — the first one nothing has taken
--   (shortened with a stable hash when it would pass the save's length
--   limit).
--
--   Every item that HOLDS a reference is named up front by
--   'reserveMapped', in the order (explicit before automatic, then the
--   raw reference text), so the names of tiers 2 and 3 depend only on
--   the set of references and document ids — not on which owner, list
--   position or ground row an item sits in, nor on the order owners are
--   captured. Only tier 4, a name derived from an unreferenced item's
--   position, depends on where the item sits.
data CaptureState = CaptureState
    { csClaims  ∷ !(HM.HashMap Text Word64)   -- ^ item names, by claiming instance
    , csForeign ∷ !(HS.HashSet Text)          -- ^ tier 1
    , csFinal   ∷ !(HM.HashMap Word64 Text)   -- ^ the final name of each instance that holds a reference
    }

emptyCaptureState ∷ CaptureState
emptyCaptureState = CaptureState HM.empty HS.empty HM.empty

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

-- | The state to start a whole capture with. Pass the ids of EVERY
--   other entry of the document being written (units, buildings,
--   locations, … — scenario ids share one namespace; these are tier 1)
--   and EVERY tree that will be captured, in any owner: every reference
--   they hold is then named up front, by the rule on 'CaptureState', so
--   no name an owner minted first can take one that a later item is
--   entitled to keep. The ids and the trees go in together because a
--   name is final once chosen: an id given later could not displace it.
newCaptureState ∷ CaptureCtx → [Text] → [ItemInstance] → CaptureState
newCaptureState ctx docIds roots =
    reserveMapped ctx roots emptyCaptureState { csForeign = HS.fromList docIds }

-- | Claim a GENERATED id for another document entry (#2710: a unit that
--   holds no scenario reference) after the items' references are
--   reserved: the first free spelling of @base@ ('freshRef'), recorded as
--   a tier-1 id so no later item name can take it. It never displaces a
--   reserved item reference, which is what keeps tiers 2 and 3 a function
--   of the references alone.
reserveDocumentId ∷ Text → CaptureState → (Text, CaptureState)
reserveDocumentId base st =
    let name = freshRef st (sanitize base)
    in (name, st { csForeign = HS.insert name (csForeign st) })

-- | Name every referenced item in these trees (see 'newCaptureState').
reserveMapped ∷ CaptureCtx → [ItemInstance] → CaptureState → CaptureState
reserveMapped ctx roots st0 = foldl' displace (foldl' claimClean st0 ordered) ordered
  where
    flat i = i : concatMap flat (iiContents i)
    mapped = [ (iid, r) | i ← concatMap flat roots
             , let iid = iiInstanceId i
             , Just r ← [ccRef ctx iid]
             , not (HM.member iid (csFinal st0)) ]
    -- authored explicit references first (the document's own spelling),
    -- then converted automatic ones, each group by raw reference: the
    -- outcome is a function of the SET, never of traversal order
    ordered = sortOn (\(_, r) → (refIsAutomatic r, r)) mapped
    taken st n = HM.member n (csClaims st) ∨ HS.member n (csForeign st)
    -- pass 1: every item whose own spelling is free takes it, so a
    -- suffixed name can never be chosen ahead of an item entitled to
    -- that spelling
    claimClean st (iid, r)
        | taken st (spelling r) = st
        | otherwise = st { csClaims = HM.insert (spelling r) iid (csClaims st)
                         , csFinal = HM.insert iid (spelling r) (csFinal st) }
    -- pass 2: the displaced ones (a document entry, or another item, has
    -- their spelling) get the first free suffixed spelling, same order
    displace st (iid, r)
        | HM.member iid (csFinal st) = st
        | otherwise =
            let name = freshRef st (spelling r)
            in st { csClaims = HM.insert name iid (csClaims st)
                  , csFinal = HM.insert iid name (csFinal st) }

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
            st1 = st { csClaims = HM.insert ref iid (csClaims st) }
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
    resolve st base iid = case HM.lookup iid (csFinal st) of
        -- named up front by 'reserveMapped' (every referenced item is)
        Just name → name
        Nothing → freshRef st (sanitize (spelling base))

-- | Name the entries that hold a scenario reference, document-wide: each
--   takes its own explicit spelling ('explicitRefOf') when nothing holds
--   it, then the displaced ones take the first free @-2@, @-3@, … — the
--   item rule, so the outcome is a function of the SET of references and
--   the foreign (tier-1) names, never of traversal order. Explicit
--   references come before automatic ones, then by raw reference.
nameReferenced ∷ Ord k ⇒ HS.HashSet Text → [(k, Text)] → Map.Map k Text
nameReferenced foreign0 refs0 = snd (foldl' displace (foldl' clean (foreign0, Map.empty) refs) refs)
  where
    refs = sortOn (\(_, r) → (refIsAutomatic r, r)) refs0
    clean (taken, m) (k, r)
        | HS.member (explicitRefOf r) taken = (taken, m)
        | otherwise = (HS.insert (explicitRefOf r) taken, Map.insert k (explicitRefOf r) m)
    displace (taken, m) (k, r)
        | Map.member k m = (taken, m)
        | otherwise =
            let n = head' [ c | j ← [2 ∷ Int ..], let c = bounded (explicitRefOf r <> "-" <> tshow j)
                              , not (HS.member c taken) ]
            in (HS.insert n taken, Map.insert k n m)
    head' (x : _) = x
    head' [] = ""

-- | Which kind of document entry holds a reference: scenario ids share ONE
--   namespace across units, buildings and locations (and items), so their
--   references are named together ('nameDocumentRefs').
data DocKey = DocUnit !UnitId | DocBuilding !BuildingId | DocLocation !LocationInstanceId
    deriving (Eq, Ord, Show)

-- | 'nameReferenced' over every referenced unit, building and location of
--   one document at once, so no family's name can take another's.
nameDocumentRefs ∷ HS.HashSet Text → [(DocKey, Text)] → Map.Map DocKey Text
nameDocumentRefs = nameReferenced

-- | The explicit-id spelling of a reference an item ALREADY holds,
--   verbatim: an admitted reference is at most 'maxItemRefLength'
--   characters and the bracketless conversion never lengthens it, so an
--   existing id is preserved exactly, however close to the limit.
spelling ∷ Text → Text
spelling = explicitRefOf

-- | A generated name is built from owner references, slot names and
--   definition names, none of which is guaranteed to be a legal id: any
--   character outside the id vocabulary becomes @_@ (the clash rule then
--   keeps the result distinct).
sanitize ∷ Text → Text
sanitize = T.map (\c → if ok c then c else '_')
  where ok c = (c ≥ 'a' ∧ c ≤ 'z') ∨ (c ≥ 'A' ∧ c ≤ 'Z') ∨ (c ≥ '0' ∧ c ≤ '9')
               ∨ c `elem` ("_-:." ∷ String)

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
freshRef ∷ CaptureState → Text → Text
freshRef st base = head' [ c | c0 ← base : [ base <> "-" <> tshow n | n ← [2 ∷ Int ..] ]
                           , let c = bounded c0
                           , not (HM.member c (csClaims st)), not (HS.member c (csForeign st)) ]
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
    key r = (floatKey (G.giX r), floatKey (G.giY r), itemSig ctx (G.giInst r))
    xs = map G.giX rows
    ys = map G.giY rows
    st0' = reserveMapped ctx (map G.giInst rows) st0
    (st1, c) = captureNodes ctx st0'
        [ ("ground." <> tshow (n ∷ Int), G.giInst r) | (n, r) ← zip [0 ..] rows ]

-- | A float as its bit pattern: a total, exact order (NaN and the two
--   zeros included) with no rendering.
floatKey ∷ Float → Word32
floatKey = castFloatToWord32

-- | How a node's scenario reference is held — as the RAW data, with its
--   kind, so an automatic path and an explicit id that spells like it
--   are different values.
data RefKey = NoRef | AutoRef !Text | ExplicitRef !Text
    deriving (Eq, Ord, Show)

refKey ∷ Maybe Text → RefKey
refKey Nothing = NoRef
refKey (Just r)
    | refIsAutomatic r = AutoRef r
    | otherwise = ExplicitRef r

-- | Everything the capture EXPORTS about a tree except instance ids, as
--   a structural value compared with the derived 'Ord': physical fields
--   (floats as bit patterns), the held reference with its kind, the tag
--   SET, and the children in order. Nothing is rendered or spelled, so
--   two different trees can never share a key, and two trees with one key
--   export identically — which is what makes the ground order
--   independent of the allocator and of ground-row ids.
data ItemSig = ItemSig
    { sgDef     ∷ !Text
    , sgFloats  ∷ ![Word32]    -- ^ fill, quality, condition, weight, sharpness
    , sgTemp    ∷ !(Maybe Word32)
    , sgBulk    ∷ !(Maybe Word32)
    , sgStorage ∷ !(Maybe (Word32, Word32))
    , sgRef     ∷ !RefKey
    , sgTags    ∷ !(Set.Set GameplayTag)
    , sgKids    ∷ ![ItemSig]
    } deriving (Eq, Ord, Show)

itemSig ∷ CaptureCtx → ItemInstance → ItemSig
itemSig ctx i = ItemSig
    { sgDef = iiDefName i
    , sgFloats = map floatKey [ iiCurrentFill i, iiQuality i, iiCondition i
                              , iiWeight i, iiSharpness i ]
    , sgTemp = floatKey ⊚ iiTemp i
    , sgBulk = floatKey ⊚ iiBulk i
    , sgStorage = (\st → (floatKey (isWeightCapacity st), floatKey (isBulkCapacity st)))
                    ⊚ iiStorage i
    , sgRef = refKey (ccRef ctx (iiInstanceId i))
    , sgTags = ccTags ctx (iiInstanceId i)
    , sgKids = map (itemSig ctx) (iiContents i)
    }
