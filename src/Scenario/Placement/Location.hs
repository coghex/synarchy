{-# LANGUAGE Strict #-}
-- | Constructing and capturing scenario locations (#2712, SCN-12 of epic
--   #2698): REAL placed-location instances at authored tiles, with their
--   encounter roster, significant-item bindings and container slots bound
--   to the actual runtime units and physical items.
--
--   __A location is an instance, not a stamp.__ The instance table
--   ('Location.Instance.LocationInstances') is the page's authority for
--   which locations exist, where, and what they owe; this module adds one
--   instance per accepted entry at the authored ANCHOR tile
--   ('Location.Instance.newLocationInstanceAt'; two instances may share a
--   chunk and a footprint may span several — a chunk is never identity) and
--   never touches the world-generation overlay. The geometry a ruin is made
--   of is the scenario's own @structures[]@ and @terrain@, so nothing
--   re-stamps it; the contents are the scenario's own units, items and
--   buildings, so nothing re-spawns them.
--
--   __Three phases, because the dependencies run both ways.__ A unit that
--   joins an encounter needs its location to SURVIVE, and a location's
--   roster needs the unit's runtime id:
--
--   1. 'validateLocations' judges every entry that can be rejected — the
--      definition, representable geometry and the exact finite bounds
--      (the whole footprint, partial chunks included) — and reports which
--      locations were rejected, so the caller can exclude the units that
--      join them ('rejectUnitsOfRejectedLocations') BEFORE building those
--      units. A rejected location leaves no instance, no id and nothing
--      for a later stage to bind.
--   2. The caller installs the units and items.
--   3. 'resolveLocationInstalls' binds each location's references to the
--      runtime identities that now exist, dropping (with a diagnostic) an
--      optional binding whose target did not survive, and
--      'applyLocationInstalls' commits them on the page's table in one
--      pure transition.
--
--   __The roster is exactly the units that join.__ An encounter's rolled
--   count is the number of units installed with @encounter@ naming the
--   location, its roster is complete, and nothing is rolled or spawned:
--   zero joined units is a zero-roll empty ruin, cleared exactly as the
--   ordinary roll's zero is ('Location.Instance.scenarioEncounter').
--
--   __Completion state is rebuilt, never copied.__ A fresh instance starts
--   'Location.Instance.LifecycleUnknown' with its clearance notice seated;
--   discovery, activation and clearance are player progress and are not
--   scenario state, and the notice is seated LAST, against the final
--   instance. Guaranteed content is OMITTED (constructed once from the
--   definition, 'expandLocationDefaults') or STATED (exactly what the entry
--   binds, even when empty): for a stated map an unbound significant slot is
--   dropped (nothing would ever fill it, and it would keep the location
--   unclearable for ever), and the content-spawn flag is then set because
--   every remaining slot is bound (the only state a saved table may hold
--   with the flag set). Neither the stamper's every-load
--   dispatch nor @spawnContents@ can add a second copy of anything — and
--   the anchor chunk's geometry marker is set by the commit.
--
--   __Containers.__ A bound slot names the physical shell. A PENDING slot
--   keeps its loot profile (the stated one, else the definition's) and
--   nothing is drawn; a REALIZED slot is latched and its profile discarded
--   (D-3), its contents being exactly the stated tree. Capture reads the
--   table and writes the slot as it IS: it never opens a shell and never
--   rolls loot.
module Scenario.Placement.Location
    ( -- * Validation
      LocationBuildEnv(..)
    , LocationValidation(..)
    , validateLocations
    , rejectUnitsOfRejectedLocations
    , expandLocationDefaults
      -- * Binding
    , UnitJoin(..)
    , ItemBinding(..)
    , LocationInstall(..)
    , resolveLocationInstalls
      -- * Commit
    , applyLocationInstalls
      -- * Capture
    , LocationCaptureCtx(..)
    , LocationsCapture(..)
    , locationNames
    , captureLocations
    ) where

import UPrelude
import Data.List (sortOn)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Gameplay.Tags.Index (categoryForward, setCategoryTags)
import Location.ArenaPlacement
    ( LocationPlacementError(..), allocateLocationInstanceAtIn
    , locationPlacementErrorText )
import Location.Instance
import Location.Bounds (AbsBounds(..), RelBounds(..))
import Scenario.Items.Refs (stableHash64)
import Location.Naming (LocationNamer)
import Location.Types (LocationContent(..), LocationDef(..), LocationRegistry, lookupLocation)
import LootTable.Roll (LootRollContext(..), rollLootTableFor)
import LootTable.Types (LootTableRegistry, lookupLootTable)
import Scenario.Decode.Monad (indexPath, keyPath)
import Scenario.Items.Capture (CaptureState, reserveDocumentId)
import Scenario.Types
import Unit.Types.Manager (UnitId)
import World.Arena.Bounds (ArenaBounds)

-- * Validation

-- | What the checks that can reject a location read.
data LocationBuildEnv = LocationBuildEnv
    { lbeDefs   ∷ !LocationRegistry
    , lbeBounds ∷ !(Maybe ArenaBounds)
    , lbeSeed   ∷ !Word64
      -- ^ the page's persisted world seed (an ordinary instance salts its
      --   default encounter roll with it; a scenario replaces the roll, but
      --   the constructor is the owner's)
    , lbeNamer  ∷ !(Maybe LocationNamer)
      -- ^ the page's language, or 'Nothing' (an arena has none): the
      --   instance is then named from the definition's own label
    }

data LocationValidation = LocationValidation
    { lvAccepted    ∷ ![(Text, LocationEntry)]
    , lvRejected    ∷ ![ScenarioId]
    , lvDiagnostics ∷ ![ScenarioDiagnostic]
    } deriving (Show, Eq)

-- | Judge every entry by the owner's own allocation, on a scratch table:
--   the same checked geometry and the same whole-footprint rule an
--   instance gets when it is really added, so a location accepted here
--   cannot be refused when it is committed.
validateLocations ∷ LocationBuildEnv → [(Text, LocationEntry)] → LocationValidation
validateLocations env entries =
    let judged = map judge entries
    in LocationValidation
        { lvAccepted = [ e | Right e ← judged ]
        , lvRejected = [ leId (snd e) | (e, Left _) ← zip entries judged ]
        , lvDiagnostics = concat [ ds | Left ds ← judged ] }
  where
    judge (path, e) = case lookupLocation (leDefinition e) (lbeDefs env) of
        Nothing → Left [ ScenarioDiagnostic (keyPath path "definition")
                           (UnknownDefinition (leDefinition e)) EntryRejected ]
        Just def → case allocateLocationInstanceAtIn (lbeBounds env) (lbeSeed env)
                            (lbeNamer env) (leX e, leY e) def emptyLocationInstances of
            Left (PlacementCrossesArena _) →
                Left [ ScenarioDiagnostic path FootprintOutsideBounds EntryRejected ]
            Left err →
                Left [ ScenarioDiagnostic path
                         (InvalidValue ("a representable footprint: " <> locationPlacementErrorText err))
                         EntryRejected ]
            Right _ → Right (path, e)

-- | Exclude every unit that joins a rejected location's encounter, with
--   everything it owns (D-17, D-47): a required reference to a rejected
--   target is itself rejected ('CascadeRejected', @RejectedReference@),
--   and the unit's items follow their owner. Returns the surviving units
--   and the diagnostics of the excluded ones.
rejectUnitsOfRejectedLocations ∷ [ScenarioId] → [(Text, UnitEntry)]
                               → ([(Text, UnitEntry)], [ScenarioDiagnostic])
rejectUnitsOfRejectedLocations rejected units =
    ( [ u | u@(_, e) ← units, not (joinsRejected e) ]
    , concat [ unitCascade p e | (p, e) ← units, joinsRejected e ] )
  where
    joinsRejected e = maybe False (`elem` rejected) (ueEncounter e)
    unitCascade p e =
        [ ScenarioDiagnostic (keyPath p "encounter")
            (RejectedReference (maybe "" scenarioIdText (ueEncounter e))) CascadeRejected ]
        ⧺ concat [ ownedBy p ip x | (ip, x) ← unitItems p e ]
    ownedBy owner ip x =
        ScenarioDiagnostic ip (OwnerRejected owner) CascadeRejected
        : concat [ ownedBy owner (indexPath (keyPath ip "contents") j) c
                 | (j, c) ← zip [0 ..] (authoredOr [] (ieContents x)) ]
    unitItems p e =
        [ (indexPath (keyPath p "inventory") i, x) | (i, x) ← zip [0 ..] (authoredOr [] (ueInventory e)) ]
        ⧺ [ (keyPath (keyPath p "equipment") k, x)
          | (k, x) ← sortOn fst (HM.toList (authoredOr HM.empty (ueEquipment e))) ]
        ⧺ [ (indexPath (keyPath p "accessories") i, x) | (i, x) ← zip [0 ..] (authoredOr [] (ueAccessories e)) ]

-- * Binding

-- | One installed unit that joins an encounter: the location it joined,
--   its runtime id and its home tile (where it stands).
data UnitJoin = UnitJoin
    { ujLocation ∷ !ScenarioId
    , ujUnit     ∷ !UnitId
    , ujHome     ∷ !(Float, Float)
    } deriving (Show, Eq)

-- | An installed item: its physical identity and its definition.
data ItemBinding = ItemBinding
    { ibId  ∷ !Word64
    , ibDef ∷ !Text
    , ibOuterGround ∷ !Bool
      -- ^ the item lies on the page's ground as its own root (not in a
      --   unit's pack, a building's storage or inside another item) — the
      --   only place an UNTAKEN significant item or a PENDING shell may be
      --   (the ordinary-save integrity rules), so the only ownership a
      --   pending binding accepts
    } deriving (Show, Eq)

-- | One location ready to commit, its references resolved.
data LocationInstall = LocationInstall
    { liiPath        ∷ !Text
    , liiEntry       ∷ !LocationEntry
    , liiOccupants   ∷ ![(UnitId, (Float, Float))]
    , liiSignificant ∷ ![(Int, ItemBinding, Text)]
      -- ^ slot, the bound item, and the binding's document path
    , liiContainers  ∷ ![(Int, ItemBinding, Bool, Maybe Text, Text)]
      -- ^ slot, the bound shell, realized, the stated profile (pending
      --   only), and the binding's document path
    } deriving (Show, Eq)

-- | Resolve each accepted location's references against what is actually
--   installed. A binding whose item is not installed (its owner or the
--   item itself was rejected) is dropped with a @RejectedReference@
--   diagnostic and the slot is left unbound (commit then drops an
--   obligation nothing discharges, 'dropUnboundSignificantSlots'); an
--   installed item of the wrong definition is dropped with
--   @DefinitionMismatch@; a PENDING shell that does not lie on the ground
--   as its own root is dropped (@ibOuterGround@) with the item left where
--   it is. A significant item held anywhere but the ground binds as taken.
resolveLocationInstalls
    ∷ LocationRegistry → [UnitJoin] → Map.Map Text ItemBinding → [(Text, LocationEntry)]
    → ([LocationInstall], [ScenarioDiagnostic])
resolveLocationInstalls registry joins items accepted =
    ( map fst resolved, concatMap snd resolved )
  where
    resolved = map one accepted
    one (path, e) =
        let def = lookupLocation (leDefinition e) registry
            sigDefs = maybe [] (map lsiItemDefName ∘ significantItemsFromDef) def
            conDefs = maybe [] (map lcsItemDefName ∘ containerSlotsFromDef) def
            occupants = [ (ujUnit j, ujHome j) | j ← joins, ujLocation j ≡ leId e ]
            sigs = [ bind (keyPath (keyPath path "significant_items") (tshow slot))
                          (drop (slot - 1) sigDefs) ref
                   | (slot, ref) ← sortOn fst (HM.toList (authoredOr HM.empty (leSignificant e))) ]
            cons = [ bindCon slot b
                   | (slot, b) ← sortOn fst (HM.toList (authoredOr HM.empty (leContainers e))) ]
            bindCon slot b =
                let p = keyPath (keyPath (keyPath path "containers") (tshow slot)) "item"
                    realized = authoredOr False (cbRealized b)
                in case bind p (drop (slot - 1) conDefs) (cbItem b) of
                    Left d → Left d
                    -- a PENDING shell must be an outer ground item: one in
                    -- storage or inside another item is unreachable by
                    -- play (the pickup boundary realizes a shell before it
                    -- moves it), and the save refuses it. The physical
                    -- item stays; only the binding is dropped. A realized
                    -- shell may be anywhere.
                    Right (ib, _) | not realized ∧ not (ibOuterGround ib) →
                        Left (ScenarioDiagnostic p
                            (InvalidValue "an item lying on the ground (a pending container shell cannot be in storage, a pack or another item)")
                            FieldRejected)
                    Right (ib, _) → Right (slot, ib
                        , authoredOr False (cbRealized b)
                        , case cbProfile b of Authored pr → Just pr; Omitted → Nothing
                        , p)
            okSigs = [ (slot, ib, p) | ((slot, _), Right (ib, p)) ← zip (sortOn fst (HM.toList (authoredOr HM.empty (leSignificant e)))) sigs ]
            okCons = [ c | Right c ← cons ]
        in ( LocationInstall path e occupants okSigs okCons
           , [ d | Left d ← sigs ] ⧺ [ d | Left d ← cons ] )
    -- a binding resolves to an installed item of the slot's definition
    bind p wantDefs ref = case Map.lookup (scenarioIdText ref) items of
        Nothing → Left (ScenarioDiagnostic p (RejectedReference (scenarioIdText ref)) FieldRejected)
        Just ib → case wantDefs of
            (want : _) | ibDef ib ≢ want →
                Left (ScenarioDiagnostic p (DefinitionMismatch want) FieldRejected)
            _ → Right (ib, p)

-- * Default expansion

-- | Construct the definition-provided NON-UNIT content a scenario OMITS
--   (requirement 5; units are the encounter rule, buildings are never
--   constructed from a definition). Per accepted location:
--
--   * @significant_items@ omitted: every significant slot gets its own
--     default item of the slot's definition;
--   * @containers@ omitted: every container slot gets its pending shell
--     (the definition's profile stays the slot's; nothing is drawn);
--   * @incidental_content@ omitted (or @true@): every fixed non-significant
--     item entry is placed, and every @loot_table@ entry is rolled, with
--     PRIVATE scenario randomness — 'rollLootTableFor' (the very draw an
--     ordinary placement makes) over a context folded from the location's
--     scenario reference, the entry's positional index and the roll number,
--     never gameplay RNG.
--
--   A key that is PRESENT — even @{}@ or @false@ — is exactly what is
--   stated and expands nothing.
--
--   Each constructed item is a ground item at a deterministic private tile
--   of the footprint (the entry's fixed offset from the anchor when it has
--   one, else a stable hash of the location's reference, the content and
--   the slot or roll) under an automatic reference (@ground_items[n]@,
--   the first one no surviving ground item holds), so it is as unique as a
--   path and is captured thereafter as an ordinary explicit ground item;
--   capture states every key, so a recapture never expands again. Returns
--   the locations with their keys filled in and the ground items to add;
--   rejected locations are not passed here, so they expand nothing.
expandLocationDefaults
    ∷ LocationRegistry → LootTableRegistry → [GroundItemEntry] → [(Text, LocationEntry)]
    → ([(Text, LocationEntry)], [GroundItemEntry])
expandLocationDefaults registry loot existing entries =
    let (_, outRev, groundRev) = foldl' step (taken0, [], []) entries
    in (reverse outRev, reverse groundRev)
  where
    -- every reference the surviving ground items already hold (an
    -- automatic one keeps the position it was DECODED at, which an earlier
    -- rejected sibling leaves a gap in), nested trees included
    taken0 = Set.fromList (concatMap (nodeIds ∘ giItem) existing)
    nodeIds i = scenarioIdText (ieId i) : concatMap nodeIds (authoredOr [] (ieContents i))
    -- the first @ground_items[k]@ nothing holds, counting from the number of
    -- items that exist
    fresh taken = head' [ r | k ← [length existing ..]
                            , let r = indexPath "ground_items" k, not (Set.member r taken) ]
    head' (x : _) = x
    head' [] = ""
    step (taken, outAcc, gAcc) (path, e) = case lookupLocation (leDefinition e) registry of
        Nothing → (taken, (path, e) : outAcc, gAcc)
        Just def →
            let sigWanted = case leSignificant e of
                    Omitted → [ ("significant", slot, lconId c, c)
                              | (slot, c) ← zip [1 ∷ Int ..] (significantContentsFromDef def) ]
                    Authored _ → []
                conWanted = case leContainers e of
                    Omitted → [ ("container", slot, lconId c, c)
                              | (slot, (c, _)) ← zip [1 ∷ Int ..] (containerContentsFromDef def) ]
                    Authored _ → []
                incWanted = case leIncidental e of
                    Authored False → []
                    _ → incidentalWanted e def
                wanted = sigWanted ⧺ conWanted ⧺ incWanted
                (taken', madeRev) = foldl' (\(tk, acc) (fam, slot, d, c) →
                        let r = fresh tk
                            iid = AutoId r
                        in (Set.insert r tk, (fam, slot, iid, defaultGround e def fam slot d c iid) : acc))
                    (taken, []) wanted
                made = reverse madeRev
                sigB = HM.fromList [ (slot, i) | ("significant", slot, i, _) ← made ]
                conB = HM.fromList [ (slot, ContainerBinding i Omitted Omitted) | ("container", slot, i, _) ← made ]
                e' = e { leSignificant = case leSignificant e of
                                Omitted → Authored sigB
                                a → a
                       , leContainers = case leContainers e of
                                Omitted → Authored conB
                                a → a
                       , leIncidental = Authored False }
            in (taken', (path, e') : outAcc, reverse [ g | (_, _, _, g) ← made ] ⧺ gAcc)
    -- the definition's incidental entries, in content order: a fixed item
    -- entry places its @count@ items, a loot-table entry makes its @rolls@
    -- draws (an unknown table or an empty one yields nothing, as it does
    -- for an ordinary placement)
    incidentalWanted e def = concat
        [ case lconKind c of
            "item" | not (lconSignificant c) →
                [ ("item" <> tshow ix, k, lconId c, c) | k ← [1 .. lconCount c] ]
            "loot_table" →
                [ ("loot" <> tshow ix, r, d, c)
                | r ← [1 .. lconRolls c]
                , Just t ← [lookupLootTable (lconId c) loot]
                , Just d ← [rollLootTableFor t (LootRollContext
                      { lrcWorldSeed = 0
                      , lrcInstanceId = fromIntegral (stableHash64 (scenarioIdText (leId e)))
                      , lrcEntryIndex = ix
                      , lrcRollIndex = r })] ]
            _ → []
        | (ix, c) ← zip [1 ∷ Int ..] (ldContents def) ]
    defaultGround e def fam slot defName content iid = GroundItemEntry
        { giX = fromIntegral tx + 0.5, giY = fromIntegral ty + 0.5
        , giItem = ItemEntry
            { ieId = iid, ieTags = [], ieDefinition = defName
            , ieFill = Omitted, ieQuality = Omitted, ieCondition = Omitted
            , ieSharpness = Omitted, ieWeight = Omitted, ieBulk = Omitted
            , ieStorage = Omitted, ieTemperature = Omitted, ieContents = Omitted } }
      where
        RelBounds x0 y0 x1 y1 = ldBounds def
        (tx, ty) = case lconPosition content of
            Just (dx, dy) → (leX e + dx, leY e + dy)
            Nothing →
                let w = x1 - x0 + 1
                    h = y1 - y0 + 1
                    k = fromIntegral (stableHash64 (scenarioIdText (leId e) <> "#" <> fam <> "#" <> tshow slot))
                          `mod` (max 1 (w * h)) ∷ Int
                in (leX e + x0 + k `mod` max 1 w, leY e + y0 + k `div` max 1 w)

-- * Commit

-- | Replace the page's location table with the accepted locations, in
--   order. A materialization establishes the page's physical baseline, so
--   the instances (and the tags and references that rode them) of any
--   earlier one are gone; the allocator is kept, so an id is never reused.
--   Returns the new table, each location's runtime id, and the diagnostics
--   of anything a binding could not hold.
--
--   The table's own owner functions do every write
--   ('allocateLocationInstanceAtIn', 'registerLocationSignificantSpawn',
--   'registerLocationContainerSpawn', 'realizeLocationContainerSlot',
--   'setLocationScenarioRef'), so every rule a decoded table must satisfy
--   binds this path too.
applyLocationInstalls ∷ LocationBuildEnv → [LocationInstall] → LocationInstances
                      → (LocationInstances, [(ScenarioId, LocationInstanceId)], [ScenarioDiagnostic])
applyLocationInstalls env installs lis0 = (lisEnd, reverse refsRev, diagsEnd)
  where
    start = emptyLocationInstances { lisNextId = lisNextId lis0 }
    (lisEnd, refsRev, diagsEnd) = foldl' step (start, [], []) installs
    step (lis, refs, diags) i = case place i lis of
        Left ds → (lis, refs, diags ⧺ ds)
        Right (iid, lis', ds) → (lis', (leId (liiEntry i), iid) : refs, diags ⧺ ds)

    place i lis =
        let e = liiEntry i
        in case lookupLocation (leDefinition e) (lbeDefs env) of
            Nothing → Left [ ScenarioDiagnostic (liiPath i) (UnknownDefinition (leDefinition e)) EntryRejected ]
            Just def → case allocateLocationInstanceAtIn (lbeBounds env) (lbeSeed env)
                                (lbeNamer env) (leX e, leY e) def lis of
                Left err → Left [ ScenarioDiagnostic (liiPath i)
                                    (InvalidValue (locationPlacementErrorText err)) EntryRejected ]
                Right (iid, lis1) →
                    let lis2 = adjustLocationInstance iid (withRoster def i) lis1
                        (lis3, sigDiags) = foldl' (bindSig iid) (lis2, []) (liiSignificant i)
                        (lis4, conDiags) = foldl' (bindCon iid) (lis3, []) (liiContainers i)
                        lis5 = adjustLocationInstance iid (withInitialClearFeedback ∘ sealContents)
                                           (dropUnboundSignificantSlots iid lis4)
                        lis6 = lis5 { lisTags = if null (leTags e) then lisTags lis5
                                                else setCategoryTags iid (Set.fromList (leTags e)) (lisTags lis5) }
                        lis7 = fromMaybe lis6 (setLocationScenarioRef iid (scenarioIdText (leId e)) lis6)
                    in Right (iid, lis7, sigDiags ⧺ conDiags)

    -- the stated roster replaces the constructor's roll; the clearance
    -- notice is seated LAST, against the instance's real, final state (a
    -- dropped obligation or a taken item can satisfy clearance at birth)
    withRoster def i inst = inst { liEncounter = scenarioEncounter def (liiOccupants i) }

    -- complete exactly when nothing is owed: an obligation nothing
    -- discharges is dropped first ('dropUnboundSignificantSlots'), so every
    -- slot left is bound
    sealContents inst = inst
        { liContentsSpawned = all (isJust ∘ lsiInstanceId) (liSignificant inst) }

    -- a significant item lying on the ground is owed and untaken; one held
    -- anywhere else (a pack, storage, inside another item) has been taken
    -- from the ruin, which is the one valid state for it (an untaken item
    -- off the ground fails the save's integrity rules)
    bindSig iid (lis, ds) (slot, ib, p) =
        case registerLocationSignificantSpawn iid slot (ibDef ib) (ibId ib) lis of
            Just lis' → (if ibOuterGround ib then lis'
                         else fromMaybe lis' (latchLocationSignificantTaken (ibId ib) lis'), ds)
            Nothing → (lis, ds ⧺ [ ScenarioDiagnostic p
                (InvalidValue "an item this slot can hold (unowned, of its definition)") FieldRejected ])

    bindCon iid (lis, ds) (slot, ib, realized, profile, p) =
        let bound = registerLocationContainerSpawn iid slot (ibDef ib) (ibId ib) lis
            profiled = case (profile, bound) of
                (Just pr, Just b) | not realized → setLocationContainerProfile iid slot pr b
                _ → bound
            final = if realized then profiled ⌦ realizeLocationContainerSlot iid slot (ibId ib)
                    else profiled
        in case final of
            Just lis' → (lis', ds)
            Nothing → (lis, ds ⧺ [ ScenarioDiagnostic p
                (InvalidValue "a container this slot can hold (unowned, of its definition)") FieldRejected ])

-- * Capture

-- | What capture reads: the page's table as one coherent value and the
--   live reference lookups taken at the same instant.
data LocationCaptureCtx = LocationCaptureCtx
    { lccDefs    ∷ !LocationRegistry
    , lccNameOf  ∷ LocationInstanceId → Maybe Text
      -- ^ the FINAL explicit id of a referenced location, decided for the
      --   whole document by 'Scenario.Items.Capture.nameDocumentRefs' over
      --   'Location.Instance.lisScenarioRefs'; a location with none is
      --   named here
    , lccItemRef ∷ Word64 → Maybe Text
      -- ^ the explicit id an installed item is captured under
    }

data LocationsCapture = LocationsCapture
    { lcapEntries    ∷ ![LocationEntry]
      -- ^ by anchor, then definition
    , lcapRefs       ∷ ![(LocationInstanceId, Text)]
      -- ^ location id → the explicit id its entry carries
    , lcapOccupantOf ∷ ![(UnitId, ScenarioId)]
      -- ^ roster member → its location's explicit id, for the unit
      --   capture's @encounter@
    , lcapSkipped    ∷ ![LocationInstanceId]
      -- ^ instances of an unknown definition, not captured
    , lcapState      ∷ !CaptureState
    }

-- | The instances capture writes, in its structural order: an anchor tile
--   is not unique (two instances may share one), so ties fall to the
--   stored geometry and then the allocation order — raw values, never a
--   rendered name.
capturedInstances ∷ LocationCaptureCtx → LocationInstances → [LocationInstance]
capturedInstances ctx lis = sortOn key known
  where
    known = [ i | i ← instancesToList lis, isJust (lookupLocation (liDefId i) (lccDefs ctx)) ]
    key i = (liAnchor i, liDefId i, bounds' i, unLocationInstanceId (liId i))
    bounds' i = let b = liBounds i in (abMinX b, abMinY b, abMaxX b, abMaxY b)

-- | The explicit id every captured location is written under, decided
--   BEFORE anything is captured so the units that join a roster can name
--   it: a referenced location takes the name the caller decided for it
--   ('lccNameOf', document-wide); any other is named from its definition
--   and anchor, the first free spelling ('reserveDocumentId'). The state
--   is returned for the owners captured after.
locationNames ∷ LocationCaptureCtx → CaptureState → LocationInstances
              → (CaptureState, Map.Map LocationInstanceId Text)
locationNames ctx st0 lis = (stEnd, Map.fromList namesRev)
  where
    (stEnd, namesRev) = foldl' one (st0, []) (capturedInstances ctx lis)
    one (st, acc) inst = case lccNameOf ctx (liId inst) of
        Just r → (st, (liId inst, r) : acc)
        Nothing →
            let (n, st') = reserveDocumentId
                    ("locations." <> liDefId inst <> "." <> tshow (fst (liAnchor inst))
                       <> "." <> tshow (snd (liAnchor inst))) st
            in (st', (liId inst, n) : acc)

-- | Capture the page's locations. Names come from 'locationNames' (given
--   the same context and the same state it is idempotent for every
--   location the context names), so a caller that needed them earlier
--   gets the identical ones here.
captureLocations ∷ LocationCaptureCtx → CaptureState → LocationInstances → LocationsCapture
captureLocations ctx st0 lis = LocationsCapture
    { lcapEntries = map (\(_, e) → e) done
    , lcapRefs = [ (liId inst, scenarioIdText (leId e)) | (inst, e) ← done ]
    , lcapOccupantOf = [ (leoUnitId o, leId e)
                       | (inst, e) ← done, Just enc ← [liEncounter inst], o ← leOccupants enc ]
    , lcapSkipped = [ liId i | i ← instancesToList lis, isNothing (lookupLocation (liDefId i) (lccDefs ctx)) ]
    , lcapState = stEnd }
  where
    (stEnd, names) = locationNames ctx st0 lis
    done = [ (inst, entry inst name)
           | inst ← capturedInstances ctx lis, Just name ← [Map.lookup (liId inst) names] ]
    entry inst name = LocationEntry
        { leId = ExplicitId name
        , leTags = Set.toAscList (Map.findWithDefault Set.empty (liId inst) (categoryForward (lisTags lis)))
        , leDefinition = liDefId inst
        , leX = fst (liAnchor inst), leY = snd (liAnchor inst)
        , leSignificant = Authored $ HM.fromList
            [ (lsiSlot s, ExplicitId r)
            | s ← liSignificant inst, Just iid ← [lsiInstanceId s]
            , Just r ← [lccItemRef ctx iid] ]
        , leIncidental = Authored False
        , leContainers = Authored $ HM.fromList
            [ (lcsSlot c, ContainerBinding (ExplicitId r)
                  (Authored (lcsRealized c))
                  (maybe Omitted Authored (if lcsRealized c then Nothing else lcsProfile c)))
            | c ← liContainers inst, Just iid ← [lcsInstanceId c]
            , Just r ← [lccItemRef ctx iid] ] }
