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
--   scenario state. Its content-spawn flag is set exactly when every
--   significant slot is bound (the only state a saved table may hold with
--   the flag set), so neither the stamper's every-load dispatch nor
--   @spawnContents@ can add a second copy of anything — and the anchor
--   chunk's geometry marker is set by the commit.
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
import Location.Bounds (AbsBounds(..))
import Location.Naming (LocationNamer)
import Location.Types (LocationRegistry, lookupLocation)
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
--   diagnostic and the slot stays unbound — the ordinary not-yet-spawned
--   state; an installed item of the wrong definition is dropped with
--   @DefinitionMismatch@.
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
                   | (slot, ref) ← sortOn fst (HM.toList (leSignificant e)) ]
            cons = [ bindCon slot b
                   | (slot, b) ← sortOn fst (HM.toList (leContainers e)) ]
            bindCon slot b =
                let p = keyPath (keyPath (keyPath path "containers") (tshow slot)) "item"
                in case bind p (drop (slot - 1) conDefs) (cbItem b) of
                    Left d → Left d
                    Right (ib, _) → Right (slot, ib
                        , authoredOr False (cbRealized b)
                        , case cbProfile b of Authored pr → Just pr; Omitted → Nothing
                        , p)
            okSigs = [ (slot, ib, p) | ((slot, _), Right (ib, p)) ← zip (sortOn fst (HM.toList (leSignificant e))) sigs ]
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
                        lis5 = adjustLocationInstance iid sealContents lis4
                        lis6 = lis5 { lisTags = if null (leTags e) then lisTags lis5
                                                else setCategoryTags iid (Set.fromList (leTags e)) (lisTags lis5) }
                        lis7 = fromMaybe lis6 (setLocationScenarioRef iid (scenarioIdText (leId e)) lis6)
                    in Right (iid, lis7, sigDiags ⧺ conDiags)

    -- the stated roster replaces the constructor's roll, and the clearance
    -- notice is seated against the instance's real, final state
    withRoster def i inst = withInitialClearFeedback
        inst { liEncounter = scenarioEncounter def (liiOccupants i) }

    -- complete exactly when nothing is owed: every significant slot bound
    sealContents inst = inst
        { liContentsSpawned = all (isJust ∘ lsiInstanceId) (liSignificant inst) }

    bindSig iid (lis, ds) (slot, ib, p) =
        case registerLocationSignificantSpawn iid slot (ibDef ib) (ibId ib) lis of
            Just lis' → (lis', ds)
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

-- | Capture the page's locations.
captureLocations ∷ LocationCaptureCtx → CaptureState → LocationInstances → LocationsCapture
captureLocations ctx st0 lis = LocationsCapture
    { lcapEntries = map (\(_, e) → e) done
    , lcapRefs = [ (liId inst, scenarioIdText (leId e)) | (inst, e) ← done ]
    , lcapOccupantOf = [ (leoUnitId o, leId e)
                       | (inst, e) ← done, Just enc ← [liEncounter inst], o ← leOccupants enc ]
    , lcapSkipped = [ liId i | i ← instances, isNothing (lookupLocation (liDefId i) (lccDefs ctx)) ]
    , lcapState = stEnd }
  where
    instances = instancesToList lis
    known = [ i | i ← instances, isJust (lookupLocation (liDefId i) (lccDefs ctx)) ]
    -- an anchor tile is not unique (two instances may share one), so ties
    -- fall to the stored geometry and then the allocation order — raw
    -- values, never a rendered name
    ordered = sortOn (\i → (liAnchor i, liDefId i, liBounds' i, unLocationInstanceId (liId i))) known
    liBounds' i = let b = liBounds i in (abMinX b, abMinY b, abMaxX b, abMaxY b)
    (stEnd, doneRev) = foldl' one (st0, []) ordered
    done = reverse doneRev
    one (st, acc) inst =
        let (name, st1) = case Map.lookup (liId inst) (lisScenarioRefs lis) of
                Just r → (r, st)
                Nothing → reserveDocumentId
                    ("locations." <> liDefId inst <> "." <> tshow (fst (liAnchor inst))
                       <> "." <> tshow (snd (liAnchor inst))) st
            tags = categoryTagsAt (liId inst)
            entry = LocationEntry
                { leId = ExplicitId name
                , leTags = tags
                , leDefinition = liDefId inst
                , leX = fst (liAnchor inst), leY = snd (liAnchor inst)
                , leSignificant = HM.fromList
                    [ (lsiSlot s, ExplicitId r)
                    | s ← liSignificant inst, Just iid ← [lsiInstanceId s]
                    , Just r ← [lccItemRef ctx iid] ]
                , leContainers = HM.fromList
                    [ (lcsSlot c, ContainerBinding (ExplicitId r)
                          (Authored (lcsRealized c))
                          (maybe Omitted Authored (if lcsRealized c then Nothing else lcsProfile c)))
                    | c ← liContainers inst, Just iid ← [lcsInstanceId c]
                    , Just r ← [lccItemRef ctx iid] ] }
        in (st1, (inst, entry) : acc)
    categoryTagsAt iid = Set.toAscList (Map.findWithDefault Set.empty iid (categoryForward (lisTags lis)))
