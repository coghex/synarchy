{-# LANGUAGE Strict #-}
-- | Turning validated scenario item entries into live item trees
--   (#2709, SCN-09 of epic #2698).
--
--   This is the construction half of the reusable item adapters: it
--   takes the 'Scenario.Types.ItemEntry' trees #2699 validates and
--   produces complete 'Item.Types.ItemInstance' trees through the ONE
--   mint boundary ("Item.Materialize"), together with the identity
--   mapping and the tag assignments a later owner adapter commits. It
--   installs nothing: the owner adapters ("Scenario.Items.Owners") shape
--   the trees for their owner, and the orchestration (SCN-13) installs
--   the owner and then attaches the mapping and tags through
--   "Gameplay.Tags.Live"'s all-or-nothing boundary.
--
--   __Explicit values are authoritative; only omissions default.__ An
--   explicit @0@, an explicit empty @contents@, an explicit @null@ bulk
--   or storage is applied verbatim: no clamp, no salvage wear, no
--   re-roll, no spill. An OMITTED value comes from the definition, or
--   (quality, empty weight) from a private generator seeded by the
--   node's stable scenario identity and the roll's domain — never by
--   the gameplay RNG, an allocation number, a sibling or the tags — so
--   the same scenario and definitions give the same values however much
--   gameplay has happened since (D-5).
--
--   __Exact contents (D-57).__ Contents are installed exactly as stated,
--   whatever the container's capacities say. Nothing here consults
--   "Item.Ownership": that policy governs ordinary moves, and a tree
--   built here is still refused by it afterwards.
--
--   __Recoverable failures are data.__ Every problem is a
--   'Scenario.Types.ScenarioDiagnostic' with the nested item path (the
--   same document paths the decoder reports) and the effect it had.
--   An invalid optional field falls back to its omission value and the
--   entry survives; an entry whose definition is unknown, or whose
--   identity is claimed twice, is rejected together with everything it
--   owns, and none of its descendants is promoted into a sibling or onto
--   the ground. A rejected tree is never minted, so it consumes no
--   instance id and leaves nothing to attach.
--
--   The definition checks run against the real 'Item.Types.ItemDef' —
--   not the catalog snapshot the decoder used — because the fill rule
--   the materializer would otherwise clamp silently ('resolveDefaultFill')
--   must be a rejection here.
module Scenario.Items.Build
    ( ItemBuildEnv(..)
    , ItemBuild(..)
    , buildItemForest
    , itemRefTargets
    , itemTagTargets
      -- * Definition catalog
    , itemCatalogEntryOf
    , itemCatalogOf
    , defHoldsItems
      -- * Deterministic defaults
    , scenarioItemSeed
    , scenarioItemRolls
    ) where

import UPrelude
import Data.List (sortOn)
import Data.IORef (newIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import System.Random (StdGen, mkStdGen)
import Gameplay.Tags.Tag (ItemTarget, mkItemTarget)
import Gameplay.Tags.Types (GameplayTag)
import Item.Materialize
import Item.Types
import Scenario.Decode.Monad (indexPath, keyPath)
import Scenario.Types

-- | What construction needs from the engine: the item registry and the
--   one instance-id allocator production already uses
--   (@freshItemInstanceId env@). Deliberately no random generator and no
--   logger: every roll is private and every report is data.
data ItemBuildEnv = ItemBuildEnv
    { ibeItems   ∷ !ItemManager
    , ibeAllocId ∷ IO Word64
    }

-- | The result of building a list of sibling trees (one owner's
--   collection).
data ItemBuild = ItemBuild
    { ibTrees       ∷ ![Maybe ItemInstance]
      -- ^ One slot per INPUT root, in input order; 'Nothing' for a
      --   rejected root. A rejected root's whole subtree is absent.
    , ibRefs        ∷ ![(Word64, Text)]
      -- ^ Instance id → scenario reference for every built node that
      --   came from an entry, parents before children, in tree order.
      --   Default-expanded children carry no reference: nothing named
      --   them.
    , ibTags        ∷ ![(Word64, Set.Set GameplayTag)]
      -- ^ The nonempty tag set of every built node that stated tags.
    , ibDiagnostics ∷ ![ScenarioDiagnostic]
      -- ^ Every recoverable problem, in document order.
    }

-- | The reference mapping as the live attachment boundary takes it.
itemRefTargets ∷ ItemBuild → [(ItemTarget, Text)]
itemRefTargets b = [ (t, r) | (i, r) ← ibRefs b, Just t ← [mkItemTarget i] ]

-- | The tag assignments as the live attachment boundary takes them.
itemTagTargets ∷ ItemBuild → [(ItemTarget, Set.Set GameplayTag)]
itemTagTargets b = [ (t, ts) | (i, ts) ← ibTags b, Just t ← [mkItemTarget i] ]

-- * Definition catalog

-- | Whether an instance of this definition may OWN item contents: an
--   item with internal storage, or one that authors default contents (a
--   shipped kit or toolbox authors @contents:@ and no @storage:@). The
--   decoder's catalog flag and this builder read the same rule.
defHoldsItems ∷ ItemDef → Bool
defHoldsItems d = isJust (idStorage d) ∨ not (null (idDefaultContents d))

-- | The catalog row the decoder validates an item against, derived from
--   the live definition.
itemCatalogEntryOf ∷ ItemDef → ItemCatalogEntry
itemCatalogEntryOf d = ItemCatalogEntry
    { icFluidCapacity = icCapacity ⊚ idContainer d
    , icHoldsItems    = defHoldsItems d }

-- | The whole item catalog for 'Scenario.Types.catItems'.
itemCatalogOf ∷ ItemManager → HM.HashMap Text ItemCatalogEntry
itemCatalogOf = HM.map itemCatalogEntryOf ∘ imDefs

-- * Deterministic defaults

-- | The seed of one roll: FNV-1a (64-bit) over the UTF-8 bytes of
--   @"synarchy.scenario.item/v1" 0x00 reference 0x00 domain@. The
--   reference is 'Scenario.Types.scenarioIdText' of the node's stable
--   identity, the domain names the value (@"quality"@, @"weight"@, and
--   the derived labels of default-expanded children). Nothing else
--   enters: not the tags, the owner, the position, the allocated ids or
--   the order siblings are built in. The encoding is fixed; changing it
--   changes every scenario's omitted values and is a format version.
scenarioItemSeed ∷ Text → Text → Word64
scenarioItemSeed ref domain = BS.foldl' step offset bytes
  where
    bytes = TE.encodeUtf8 ("synarchy.scenario.item/v1\0" <> ref <> "\0" <> domain)
    offset = 14695981039346656037
    prime = 1099511628211
    step h b = (h `xor` fromIntegral b) * prime

-- | The per-domain generator of one node, for 'Item.Materialize.spRolls'.
scenarioItemRolls ∷ Text → Text → StdGen
scenarioItemRolls ref domain = mkStdGen (fromIntegral (scenarioItemSeed ref domain))

-- * Checking

-- | An entry that survived checking, with its invalid optional fields
--   already set back to 'Omitted' and its rejected descendants removed.
data Checked = Checked
    { ckEntry ∷ !ItemEntry
    , ckPath  ∷ !Text
    , ckKids  ∷ !(Maybe [Checked])
      -- ^ 'Nothing' when the entry's contents are omitted; the SURVIVING
      --   children (original order) when stated. A child's original
      --   index is in its 'ckPath'.
    }

-- | Check one entry (and, recursively, its contents).
checkEntry ∷ ItemManager → Text → ItemEntry → ([ScenarioDiagnostic], Maybe Checked)
checkEntry items path e = case lookupItemDef (ieDefinition e) items of
    Nothing →
        ( ScenarioDiagnostic path (UnknownDefinition (ieDefinition e)) EntryRejected
          : cascade path e
        , Nothing )
    Just def →
        let (fieldDiags, e1) = checkFields def path e
            (kidDiags, kids) = checkContents items def path e1
        in ( fieldDiags ⧺ kidDiags
           , Just Checked { ckEntry = e1, ckPath = path, ckKids = kids } )

-- | Every descendant of a rejected entry, each owned by the rejected
--   path (the decoder's @excludeItemTree@ shape).
cascade ∷ Text → ItemEntry → [ScenarioDiagnostic]
cascade owner e = go (keyPath owner "contents") (authoredOr [] (ieContents e))
  where
    go base kids = concat
        [ ScenarioDiagnostic cp (OwnerRejected owner) CascadeRejected
          : go (keyPath cp "contents") (authoredOr [] (ieContents k))
        | (i, k) ← zip [0 ..] kids, let cp = indexPath base i ]

checkContents ∷ ItemManager → ItemDef → Text → ItemEntry
              → ([ScenarioDiagnostic], Maybe [Checked])
checkContents items def path e = case ieContents e of
    Omitted → ([], Nothing)
    Authored [] → ([], Just [])
    Authored kids
        | defHoldsItems def →
            let base = keyPath path "contents"
                rs = [ checkEntry items (indexPath base i) k | (i, k) ← zip [0 ..] kids ]
            in (concatMap fst rs, Just (catMaybes (map snd rs)))
        | otherwise →
            ( ScenarioDiagnostic (keyPath path "contents")
                  (InvalidValue "no contents: this item holds no items") FieldRejected
              : concat [ ScenarioDiagnostic cp (OwnerRejected (keyPath path "contents")) CascadeRejected
                         : cascadeBelow cp k
                       | (i, k) ← zip [0 ..] kids
                       , let cp = indexPath (keyPath path "contents") i ]
            , Nothing )
  where
    cascadeBelow cp k = go (keyPath cp "contents") (authoredOr [] (ieContents k))
      where go base ks = concat
                [ ScenarioDiagnostic p (OwnerRejected (keyPath path "contents")) CascadeRejected
                  : go (keyPath p "contents") (authoredOr [] (ieContents c))
                | (i, c) ← zip [0 ..] ks, let p = indexPath base i ]

-- | Validate every stated physical value against the definition. An
--   invalid one is dropped to 'Omitted' with a 'FieldRejected'
--   diagnostic; nothing is clamped into an apparently valid value.
checkFields ∷ ItemDef → Text → ItemEntry → ([ScenarioDiagnostic], ItemEntry)
checkFields def path e =
    ( concat [fillD, qualD, condD, sharpD, weightD, bulkD, storD, tempD]
    , e { ieFill = fill', ieQuality = qual', ieCondition = cond'
        , ieSharpness = sharp', ieWeight = weight', ieBulk = bulk'
        , ieStorage = stor', ieTemperature = temp' } )
  where
    (fillD, fill') = field "fill" (ieFill e) fillOk
    (qualD, qual') = field "quality" (ieQuality e) percentOk
    (condD, cond') = field "condition" (ieCondition e) percentOk
    (sharpD, sharp') = field "sharpness" (ieSharpness e) percentOk
    (weightD, weight') = field "weight" (ieWeight e) nonNegative
    (bulkD, bulk') = field "bulk" (ieBulk e) (maybe (Right ()) nonNegative)
    (storD, stor') = field "storage_capacity" (ieStorage e) storageOk
    (tempD, temp') = field "temperature" (ieTemperature e) tempOk
    field ∷ Text → Authored α → (α → Either Text ()) → ([ScenarioDiagnostic], Authored α)
    field key a ok = case a of
        Omitted → ([], Omitted)
        Authored v → case ok v of
            Right () → ([], a)
            Left why → ( [ScenarioDiagnostic (keyPath path key) (InvalidValue why) FieldRejected]
                       , Omitted )
    fillOk f = case idContainer def of
        Just c | finite f ∧ f ≥ 0 ∧ f ≤ icCapacity c → Right ()
               | otherwise → Left ("a fill in [0, " <> tshow (icCapacity c) <> "]")
        Nothing | f ≡ 0 → Right ()
                | otherwise → Left "0: this item holds no fluid"
    percentOk v | finite v ∧ v ≥ 0 ∧ v ≤ 100 = Right ()
                | otherwise = Left "a number in [0, 100]"
    nonNegative v | finite v ∧ v ≥ 0 = Right ()
                  | otherwise = Left "a number ≥ 0"
    storageOk Nothing = Right ()
    storageOk (Just (StorageCapacity w b))
        | finite w ∧ w ≥ 0 ∧ finite b ∧ b ≥ 0 = Right ()
        | otherwise = Left "weight and bulk, each a number ≥ 0"
    tempOk AtAmbient = Right ()
    tempOk (TrackedTemp t)
        | finite t ∧ t ≥ -273.15 = Right ()
        | otherwise = Left "\"ambient\" or a temperature in °C ≥ -273.15"
    finite x = not (isNaN x ∨ isInfinite x)

-- | Reject every holder of an identity more than one surviving node
--   claims, with everything those holders own. A function of the SET of
--   nodes: the result does not depend on the order the roots came in.
rejectDuplicates ∷ [Checked] → ([ScenarioDiagnostic], [Maybe Checked])
rejectDuplicates roots = (concatMap fst pruned, map snd pruned)
  where
    pruned = map prune roots
    allNodes ck = ck : concatMap allNodes (fromMaybe [] (ckKids ck))
    counts = HM.fromListWith (+)
        [ (scenarioIdText (ieId (ckEntry n)), 1 ∷ Int) | r ← roots, n ← allNodes r ]
    dup n = HM.lookupDefault 0 (scenarioIdText (ieId (ckEntry n))) counts > 1
    prune ck
        | dup ck =
            ( ScenarioDiagnostic (ckPath ck) (DuplicateId (scenarioIdText (ieId (ckEntry ck))))
                  EntryRejected
              : cascadeChecked (ckPath ck) ck
            , Nothing )
        | otherwise =
            let rs = map prune (fromMaybe [] (ckKids ck))
                kept = catMaybes (map snd rs)
            in ( concatMap fst rs
               , Just ck { ckKids = fmap (const kept) (ckKids ck) } )
    cascadeChecked owner ck = concat
        [ ScenarioDiagnostic (ckPath k) (OwnerRejected owner) CascadeRejected
          : cascadeChecked owner k
        | k ← fromMaybe [] (ckKids ck) ]

-- * Building

-- | Build one owner's list of sibling roots. @roots@ pairs each entry
--   with the document path of its slot in the owner (for example
--   @units[0].inventory[2]@, or @ground_items[3]@).
buildItemForest ∷ ItemBuildEnv → [(Text, ItemEntry)] → IO ItemBuild
buildItemForest env roots = do
    let items = ibeItems env
        checked = [ checkEntry items p e | (p, e) ← roots ]
        checkDiags = concatMap fst checked
        survivors = catMaybes (map snd checked)
        (dupDiags, afterDup) = rejectDuplicates survivors
        -- back to one slot per input root, rejected ones empty
        slots = fill (map snd checked) afterDup
        fill [] _ = []
        fill (Nothing : rest) ks = Nothing : fill rest ks
        fill (Just _ : rest) (k : ks) = k : fill rest ks
        fill (Just _ : rest) [] = Nothing : fill rest []
    scratch ← newIORef (mkStdGen 0)
    built ← forM slots $ \slot → case slot of
        Nothing → return (Nothing, [], [])
        Just ck → do
            (mInst, notes) ← materializeSpec items scratch (ibeAllocId env) (specOf ck)
            return (mInst, map noteDiag notes, maybe [] (identityOf ck) mInst)
    let ids = concat [ i | (_, _, i) ← built ]
    return ItemBuild
        { ibTrees = [ t | (t, _, _) ← built ]
        , ibRefs = [ (i, r) | (i, r, _) ← ids ]
        , ibTags = [ (i, ts) | (i, _, ts) ← ids, not (Set.null ts) ]
        , ibDiagnostics = sortOnPath (checkDiags ⧺ dupDiags ⧺ concat [ d | (_, d, _) ← built ])
        }

-- | Stable by document path, so the list is the same whatever order the
--   passes produced it in; equal paths keep their relative order.
sortOnPath ∷ [ScenarioDiagnostic] → [ScenarioDiagnostic]
sortOnPath = sortOn sdPath

noteDiag ∷ DefaultContentNote → ScenarioDiagnostic
noteDiag (DefaultContentNote path issue) = case issue of
    DefaultUnknownDefinition n →
        ScenarioDiagnostic path (UnknownDefinition n) EntryRejected
    DefaultCyclic closed →
        ScenarioDiagnostic path (CyclicContents (T.intercalate " → " closed)) EntryRejected

-- | The construction tree of a checked entry.
specOf ∷ Checked → ItemSpec
specOf ck = (itemSpec (ieDefinition e))
    { spOverrides = ItemOverrides
        { ovFill = authoredMaybe (ieFill e)
        , ovQuality = authoredMaybe (ieQuality e)
        , ovCondition = authoredMaybe (ieCondition e)
        , ovTemp = case ieTemperature e of
            Authored (TrackedTemp t) → Just t
            _ → Nothing   -- explicit ambient and omitted are both ambient
        }
    , spSharpness = authoredMaybe (ieSharpness e)
    , spWeight = authoredMaybe (ieWeight e)
    , spBulk = authoredMaybe (ieBulk e)
    , spStorage = authoredMaybe (fmap (fmap toStorage) (ieStorage e))
    , spContents = fmap (map specOf) (ckKids ck)
    , spPath = ckPath ck
    , spRolls = Just (scenarioItemRolls (scenarioIdText (ieId e)))
    }
  where
    e = ckEntry ck
    authoredMaybe a = case a of { Authored v → Just v; Omitted → Nothing }
    toStorage (StorageCapacity w b) = ItemStorage { isWeightCapacity = w, isBulkCapacity = b }

-- | Pair each checked node with the instance built from it. Authored
--   children are materialized one-for-one in order, so a stated
--   contents list lines up with the instance's; a node whose contents
--   were omitted holds default-expanded children, which carry no
--   scenario identity.
identityOf ∷ Checked → ItemInstance → [(Word64, Text, Set.Set GameplayTag)]
identityOf ck inst =
    (iiInstanceId inst, scenarioIdText (ieId e), Set.fromList (ieTags e))
    : case ckKids ck of
        Just kids | length kids ≡ length (iiContents inst) →
            concat (zipWith identityOf kids (iiContents inst))
        _ → []
  where e = ckEntry ck
