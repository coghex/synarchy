{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass
           , DerivingStrategies #-}
-- | First-class placed-location instance identity and lifecycle (#911,
--   epic #159 phase 3 / @docs\/expedition_gameplay_loop.md@ step 2).
--
--   A placed location's persisted state used to be three chunk-keyed
--   'Data.HashSet.HashSet's on 'World.Generate.Types.WorldGenParams'.
--   A chunk is not a location: two locations in one chunk are
--   indistinguishable, a footprint straddling a chunk boundary has no
--   single key, and there is nowhere to record encounter, loot, or
--   cleared state. This module replaces two of those three sets
--   ('wgpLocationDiscovered', 'wgpLocationContentsSpawned') with a
--   per-page table of instance records keyed by a stable
--   'LocationInstanceId'.
--
--   'World.Generate.Types.wgpLocationStamped' deliberately STAYS
--   chunk-keyed and is untouched: "has this chunk had geometry written
--   into it" is genuinely a fact about the chunk, and it is what makes
--   stamping idempotent under player edits (#424).
--
--   /Identity./ The durable identity is @(WorldPageId,
--   LocationInstanceId)@ — this table is per world page (it rides that
--   page's gen params), and ids are allocated per page from
--   'lisNextId', starting at 'firstLocationInstanceId' (1; the
--   engine-wide convention every allocator except ground items follows).
--   Ids are assigned at PLACEMENT time from the deterministic overlay,
--   in 'Location.Overlay.Types.overlayToList' order — a canonical total
--   order, never 'Data.HashMap.Strict.HashMap' iteration order, which
--   is unstable across runs. Recomputing the same overlay therefore
--   reproduces the same id → (definition, anchor) mapping, and an id
--   survives save/load and chunk eviction/reload because nothing about
--   it is derived from chunk residency.
--
--   /Stored geometry./ An instance stores its own anchor tile, absolute
--   bounds (#777) and display name, resolved from
--   the definition ONCE when the instance is created. Consumers
--   (queries, discovery, placement exclusion) read those stored values
--   rather than re-deriving them from the live registry, so a
--   definition edited later never silently reshapes a placed instance.
--
--   /Display name./ Rendered in the world's own generated language
--   (#1101) when the page has one, from the definition's authored
--   concept pools ('Location.Types.ldNaming') and the instance's own
--   id — see "Location.Naming". A page with NO language provenance
--   (#1092: a custom-named world, or one saved before provenance was
--   recorded) falls back to the definition's 'ldLabel' with no gloss.
--   Both are written ONCE, when the instance is created, and read
--   thereafter: a location named under one generator version keeps that
--   name forever (#708 principle 5), so nothing on the load or
--   migration path re-derives either field.
module Location.Instance
    ( -- * Identity
      LocationInstanceId(..)
    , firstLocationInstanceId
      -- * Lifecycle
    , LocationLifecycle(..)
    , lifecycleName
    , lifecycleFromName
    , isDiscoveredLifecycle
    , promoteLifecycle
      -- * Encounters
    , LocationEncounterOccupant(..)
    , LocationEncounter(..)
    , encounterOccupant
      -- * Guaranteed significant contents (#917)
    , LocationSignificantItem(..)
    , significantItemsFromDef
    , locationEncounterCondition
    , locationSignificantCondition
    , significantRecovered
    , locationAuthorsClearance
    , locationClearanceSatisfied
    , locationDiscoveryLifecycle
      -- * Records
    , LocationInstance(..)
    , LocationInstances(..)
    , LegacyLocationChunkFlags(..)
    , emptyLocationInstances
    , newLocationInstance
    , newLocationInstanceWithSeed
      -- * Checked geometry construction
    , LocationGeometryError(..)
    , locationGeometryErrorText
    , locationAnchorTileInteger
    , locationAnchorTileChecked
    , locationInstanceGeometry
      -- * Queries
    , instancesToList
    , instancesCount
    , lookupLocationInstance
    , instancesInChunk
    , locationInstanceBounds
      -- * Construction / mutation
    , buildLocationInstances
    , buildLocationInstancesWithSeed
    , allocateLocationInstance
    , adjustLocationInstance
    , setLocationLifecycle
    , markLocationContentsSpawned
    , registerLocationEncounterOccupants
    , adjustLocationEncounterOccupant
    , setLocationEncounterEpisodeState
    , markLocationEncounterCleared
    , registerLocationSignificantSpawn
    , latchLocationSignificantTaken
    , resolveLocationClearance
      -- * v1 chunk-set migration
    , pendingLegacyFlags
    , resolveLegacyLocationInstances
      -- * Validation
    , locationInstanceAllocatorErrors
    , locationInstanceBoundsErrors
    , locationSignificantItemErrors
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.List (find, sortOn)
import qualified Data.Text as T
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Location.Bounds
    ( AbsBounds(..), LocationGeometryFailure(..), narrowTileCoordinate
    , translateBoundsChecked )
import Language.Etymology.Source (EtymologySource)
import Location.Naming (LocationNamer, nameLocationInstance)
import Location.Overlay.Types (LocationOverlay, overlayToList)
import Location.Types
    (LocationContent(..), LocationDef(..), LocationRegistry, lookupLocation)
import Language.Generated.Hash (fmix64)
import Unit.Types.Manager (UnitId)
import World.Chunk.Types (ChunkCoord(..), chunkSize)

-- * Identity ---------------------------------------------------------

-- | A placed location's stable, page-local id. Allocated at placement
--   time and never reused within a page.
newtype LocationInstanceId = LocationInstanceId { unLocationInstanceId ∷ Int }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Ord, Hashable, NFData, Serialize)

-- | The first id every page's allocator hands out. 1, matching every
--   allocator in the engine except ground items (which are zero-based —
--   see @docs\/persistence_contract.md@).
firstLocationInstanceId ∷ Int
firstLocationInstanceId = 1

-- * Lifecycle --------------------------------------------------------

-- | The expedition-loop lifecycle (@docs\/expedition_gameplay_loop.md@
--   step 2). Ordered: 'Ord' IS the progression, so \"discovered or
--   beyond\" is a plain comparison and a backward transition is a plain
--   rejection ('promoteLifecycle').
--
--   Serialized positionally by constructor tag through @Generic
--   Serialize@ — APPEND-ONLY, exactly like 'World.Types.Direction' /
--   @Pose@ / @UnitActivity@ (see the enum schema policy in
--   @CLAUDE.md@). Inserting or reordering a constructor silently
--   corrupts saves.
--
--   #916's ruin encounters promote visible occupied locations to
--   'LifecycleActive' and death-cleared (including zero-roll) locations to
--   'LifecycleCleared'. Reward and retrieval remain later lifecycle work.
--
--   'LifecycleHinted' is deliberately unreachable today: nothing
--   produces it, and #1230 removed the future information-reveal class
--   it used to be reserved for. It is NOT dead weight, and it is not
--   removable either way — it is a positionally serialized append-only
--   enum constructor (see the schema note above), so deleting it would
--   silently corrupt every save written after it. Behaviourally it is
--   an ordinary unknown state: it draws the shared unknown map icon
--   ('World.Render.Zoom.Icons.locationIconAppearance') and promotes to
--   'LifecycleDiscovered' on sight exactly as 'LifecycleUnknown' does.
--   This note is here so the state is not later mistaken for a
--   forgotten placeholder and deleted by someone tidying an enum.
data LocationLifecycle
    = LifecycleUnknown
    | LifecycleHinted
    | LifecycleDiscovered
    | LifecycleActive
    | LifecycleCleared
    | LifecycleDepleted
    deriving (Show, Eq, Ord, Enum, Bounded, Generic, NFData, Serialize)

-- | The stable lowercase wire name a lifecycle state carries across the
--   Lua boundary (@world.listPlacedLocations@ / @world.getLocationInstance@
--   / @world.setLocationLifecycle@). Paired with 'lifecycleFromName'.
lifecycleName ∷ LocationLifecycle → Text
lifecycleName l = case l of
    LifecycleUnknown    → "unknown"
    LifecycleHinted     → "hinted"
    LifecycleDiscovered → "discovered"
    LifecycleActive     → "active"
    LifecycleCleared    → "cleared"
    LifecycleDepleted   → "depleted"

-- | Inverse of 'lifecycleName'; 'Nothing' for an unknown name.
lifecycleFromName ∷ Text → Maybe LocationLifecycle
lifecycleFromName t =
    listToMaybe [ l | l ← [minBound .. maxBound], lifecycleName l ≡ t ]

-- | Has this location been discovered — i.e. is it at or beyond
--   'LifecycleDiscovered'? The one predicate every consumer that used
--   to test @HashSet.member coord wgpLocationDiscovered@ now uses, so
--   the zoom-map icons (#781) and the @discovered@ query field can
--   never disagree about what a later lifecycle state means.
isDiscoveredLifecycle ∷ LocationLifecycle → Bool
isDiscoveredLifecycle = (≥ LifecycleDiscovered)

-- | A lifecycle transition, applied only when it moves STRICTLY
--   forward. 'Nothing' means the transition was rejected and nothing
--   changes: a backward transition (@cleared → discovered@) and a
--   same-state transition (@discovered → discovered@) are both refused.
--   Refusing the same-state case is what makes discovery fire exactly
--   one player event per location — the caller emits only on a 'Just'.
promoteLifecycle
    ∷ LocationLifecycle  -- ^ current
    → LocationLifecycle  -- ^ requested
    → Maybe LocationLifecycle
promoteLifecycle cur next
    | next > cur = Just next
    | otherwise  = Nothing

-- * Encounters ------------------------------------------------------

-- | One unit originally assigned to a placed-location encounter (#916).
--   Membership survives a missing runtime unit: a dangling UID is still
--   evidence that the death-only encounter has NOT been cleared. The home
--   coordinate is recorded at the successful spawn and never inferred from
--   the unit's later position.
data LocationEncounterOccupant = LocationEncounterOccupant
    { leoUnitId              ∷ !UnitId
    , leoHome                ∷ !(Float, Float)
    , leoEngaged             ∷ !Bool
    , leoReturning           ∷ !Bool
    } deriving (Show, Eq, Generic, NFData, Serialize)

-- | Durable outcome and progress for one placed encounter. The clearance
--   policy is deliberately ruin-local: @leDeathOnlyClearance@ is stored on
--   the instance instead of changing the meaning of the shared location
--   lifecycle. @leCleared@ records the encounter outcome separately from
--   discovery, so a zero roll can remain visually unknown until first sight.
data LocationEncounter = LocationEncounter
    { leRolledCount        ∷ !Int
    , leOccupants          ∷ ![LocationEncounterOccupant]
    , leRosterComplete     ∷ !Bool
    , leDeathOnlyClearance ∷ !Bool
    , leActivated          ∷ !Bool
      -- ^ Whether this encounter has ever entered combat. Kept separate
      --   from discovery so pre-discovery aggression remains private while
      --   first sight can still expose the location as active.
    , leEpisodeActive      ∷ !Bool
      -- ^ Whether at least one assigned survivor is in the current combat
      --   episode. This may return to false without moving lifecycle back.
    , leAggressionAnnounced ∷ !Bool
    , leDisengageAnnounced  ∷ !Bool
    , leCleared            ∷ !Bool
      -- ^ ENCOUNTER completion only (#917): every assigned occupant is
      --   dead (or the roll was zero). Deliberately NOT the location's
      --   cleared state — a ruin whose nomads are down but whose
      --   guaranteed significant item is still lying there has
      --   @leCleared@ true and is not cleared. The compound predicate
      --   ('locationClearanceSatisfied') owns that, and the one
      --   player-facing feedback latch lives on the INSTANCE
      --   ('liClearEventEmitted') so a location authoring significant
      --   items and no encounter has one too.
    } deriving (Show, Eq, Generic, NFData, Serialize)

-- * Guaranteed significant contents (#917) ---------------------------

-- | One GUARANTEED SIGNIFICANT item a placed location owes (#917) — the
--   second half of the expedition completion predicate
--   (@docs\/expedition_gameplay_loop.md@ D-17): a location clears only
--   when its authored encounter condition is satisfied AND every
--   significant item it spawned has been taken.
--
--   /Cardinality is fixed at PLACEMENT, not at spawn./ The whole list
--   is built by 'significantItemsFromDef' when the instance is created,
--   with 'lsiInstanceId' still 'Nothing' — so the obligation exists
--   before @scripts\/locations.lua@ has spawned anything, and a
--   never-spawned (or failed-to-spawn) item keeps the loot condition
--   INCOMPLETE instead of an empty collection reading as satisfied.
--
--   /Provenance is the PHYSICAL item identity./ 'lsiInstanceId' holds
--   'Item.Types.iiInstanceId', not a page-local ground-item id: the
--   physical id is preserved verbatim through pickup, transfer, storage
--   and drop, while 'Item.Ground.spawnGroundItem' hands out a NEW
--   ground id every time an item is dropped or a failed pickup is
--   rolled back. Keying on the ground id would lose the item the first
--   time anyone put it down.
--
--   /'lsiTaken' is a LATCH./ It is set once, by the authoritative
--   ground→inventory boundary
--   ('Engine.Scripting.Lua.API.Items.Ground.pickupGroundOnPage'), the
--   first time ANY unit of ANY faction successfully picks the item up —
--   a pickup that returns false never latches. Dropping, transferring,
--   losing, consuming or destroying the item afterwards never clears
--   it: the location was looted, and that does not become untrue.
data LocationSignificantItem = LocationSignificantItem
    { lsiSlot        ∷ !Int
      -- ^ Stable per-instance obligation slot, 1-based in authored
      --   order — the entry's positional index in
      --   'Location.Types.ldContents' first, then its ordinal within
      --   that entry's 'Location.Types.lconCount'. The address
      --   @scripts\/locations.lua@ registers a spawn against, so a
      --   resumed spawn fills exactly the slots still empty.
    , lsiItemDefName ∷ !Text
      -- ^ the authored 'Location.Types.lconId' this slot must spawn
    , lsiInstanceId  ∷ !(Maybe Word64)
      -- ^ the spawned item's 'Item.Types.iiInstanceId'; 'Nothing' until
      --   the content spawn registers one
    , lsiTaken       ∷ !Bool
    } deriving (Show, Eq, Generic, NFData, Serialize)

-- | The obligations a definition's authored contents impose on ONE
--   placed instance, in authored order (see 'lsiSlot').
--
--   Only fixed @kind: item@ entries flagged 'Location.Types.lconSignificant'
--   contribute — the YAML boundary
--   ('Engine.Asset.YamlLocations') already refuses the flag on any
--   other kind, so a @loot_table@ draw can never become an obligation
--   whatever it rolls (requirement 4).
significantItemsFromDef ∷ LocationDef → [LocationSignificantItem]
significantItemsFromDef def =
    [ LocationSignificantItem
        { lsiSlot        = slot
        , lsiItemDefName = lconId c
        , lsiInstanceId  = Nothing
        , lsiTaken       = False
        }
    | (slot, c) ← zip [1 ..]
        [ c
        | c ← ldContents def
        , lconSignificant c
        , lconKind c ≡ "item"
        , _ ← [1 .. lconCount c] ]
    ]

-- | The ENCOUNTER half of the clearance predicate: 'Nothing' when the
--   instance authors no encounter at all, @Just satisfied@ otherwise.
locationEncounterCondition ∷ LocationInstance → Maybe Bool
locationEncounterCondition = fmap leCleared ∘ liEncounter

-- | The SIGNIFICANT-ITEM half: 'Nothing' when the instance owes none,
--   @Just satisfied@ otherwise.
locationSignificantCondition ∷ LocationInstance → Maybe Bool
locationSignificantCondition inst
    | null (liSignificant inst) = Nothing
    | otherwise = Just (all significantRecovered (liSignificant inst))

-- | Has ONE obligation actually been discharged? A real item must have
--   been spawned for it AND that item taken — both halves, never the
--   latch alone.
--
--   Requiring the bound id is not redundant with the latch. No engine
--   path can set 'lsiTaken' without one ('latchLocationSignificantTaken'
--   matches on the id, so an unbound obligation is unreachable), but a
--   'lsiTaken' payload carrying no id would otherwise satisfy clearance
--   with no item ever spawned and nothing ever picked up — and the
--   session provenance rules cannot catch it either, because there is
--   no id for them to resolve. The predicate refuses the shape here,
--   and 'locationSignificantItemErrors' rejects it at decode; this side
--   exists so a payload that somehow reached a live table still cannot
--   clear a location for free.
--
--   An obligation that was never spawned is therefore incomplete for
--   BOTH reasons, which is what requirement 4 wants: an unspawned,
--   failed-to-spawn or missing item keeps the loot condition open.
significantRecovered ∷ LocationSignificantItem → Bool
significantRecovered e = isJust (lsiInstanceId e) ∧ lsiTaken e

-- | Does this instance author ANY clearance condition? A location with
--   neither an encounter nor a significant item authors none, and must
--   never clear through an empty conjunction — it keeps whatever
--   lifecycle discovery gives it, exactly as it did before #917.
locationAuthorsClearance ∷ LocationInstance → Bool
locationAuthorsClearance inst = not (null (conditionsOf inst))

-- | The compound clearance predicate (#917 requirement 5): the
--   CONJUNCTION of the conditions actually authored for this location.
--   A location authoring only one of the two clears on that one; a
--   location authoring neither never clears (an empty conjunction is
--   'False' here, deliberately not the vacuous 'True').
locationClearanceSatisfied ∷ LocationInstance → Bool
locationClearanceSatisfied inst = case conditionsOf inst of
    []         → False
    conditions → and conditions

conditionsOf ∷ LocationInstance → [Bool]
conditionsOf inst = catMaybes
    [ locationEncounterCondition inst
    , locationSignificantCondition inst
    ]

-- | The first visible lifecycle for an instance. Discovery is still the
--   single edge that emits @location_discovery@; the location's own
--   state only chooses where that edge lands — so a ruin whose nomads
--   were already dead AND whose significant items were already taken
--   is discovered straight into 'LifecycleCleared', while one with
--   either half outstanding is not.
locationDiscoveryLifecycle ∷ LocationInstance → LocationLifecycle
locationDiscoveryLifecycle inst
    | locationClearanceSatisfied inst = LifecycleCleared
    | maybe False leActivated (liEncounter inst) = LifecycleActive
    | otherwise = LifecycleDiscovered

-- | Find one assigned occupant without dropping dangling membership.
encounterOccupant ∷ UnitId → LocationEncounter → Maybe LocationEncounterOccupant
encounterOccupant uid = find ((≡ uid) . leoUnitId) . leOccupants

-- * Records ----------------------------------------------------------

-- | One placed location. Everything gameplay needs to address it by id
--   without consulting the overlay, the chunk it happens to sit in, or
--   the live definition registry.
data LocationInstance = LocationInstance
    { liId              ∷ !LocationInstanceId
      -- ^ stable, page-local (see the module haddock)
    , liDefId           ∷ !Text
      -- ^ the 'Location.Types.ldId' this instance was placed from
    , liChunk           ∷ !ChunkCoord
      -- ^ the chunk hosting its anchor. NOT an identity: two instances
      --   may share one chunk, and the footprint may straddle chunks.
    , liAnchor          ∷ !(Int, Int)
      -- ^ absolute anchor tile ('locationAnchorTileChecked' of
      --   'liChunk' at placement time), stored rather than re-derived
    , liBounds          ∷ !AbsBounds
      -- ^ absolute, inclusive tile footprint (#777), resolved from the
      --   definition when the instance was created
    , liDisplayName     ∷ !Text
      -- ^ the instance's name, rendered ONCE at creation: native text
      --   in the page's own language (#1101) when it has one, else the
      --   definition's 'ldLabel'. Never re-derived (see the module
      --   haddock).
    , liGloss           ∷ !(Maybe Text)
      -- ^ the English gloss of 'liDisplayName', from the SAME name
      --   expression that rendered it (#1101 requirement 5), mirroring
      --   'World.Page.Types.wiGloss'. 'Nothing' exactly when
      --   'liDisplayName' is an 'ldLabel' fallback — a label is not a
      --   generated name and has no meaning to explain.
    , liEtymology       ∷ !(Maybe EtymologySource)
      -- ^ what 'liDisplayName' was rendered FROM (#1104), mirroring
      --   'World.Page.Types.wiEtymology': the originating expression
      --   plus the provenance that rendered it. Written ONCE beside the
      --   name and read thereafter; 'Nothing' for an 'ldLabel' fallback
      --   and for every instance placed before #1104, and never
      --   inferred afterwards.
    , liLifecycle       ∷ !LocationLifecycle
    , liContentsSpawned ∷ !Bool
      -- ^ one-time content-spawn flag (#90), now per INSTANCE. Stays
      --   deliberately independent of 'liLifecycle' and of
      --   'World.Generate.Types.wgpLocationStamped' (#424).
    , liEncounter       ∷ !(Maybe LocationEncounter)
      -- ^ Optional placed-location encounter (#916), rolled once when the
      --   overlay instance is built. Independent from discovery and the
      --   generic content-spawn flag; historical saves decode with absence.
    , liSignificant     ∷ ![LocationSignificantItem]
      -- ^ The GUARANTEED SIGNIFICANT items this instance owes (#917),
      --   fixed at PLACEMENT from the definition's authored contents
      --   ('significantItemsFromDef') so the expected cardinality is
      --   durable before anything spawns. Empty for a definition that
      --   authors none, and empty for every historical instance —
      --   no migration infers obligations from today's YAML.
    , liClearEventEmitted ∷ !Bool
      -- ^ Has this location's ONE player-facing clearance notice been
      --   spent? Generalized from #916's per-encounter
      --   @leClearEventEmitted@ (#917) so a location that authors
      --   significant items and NO encounter has a latch too. Set only
      --   by 'resolveLocationClearance', which is the single writer of
      --   the cleared transition, so the notice fires exactly once
      --   however the last condition was satisfied — including the
      --   deferred case, where the location completed while still
      --   unknown and the notice waits for sight.
    } deriving (Show, Eq, Generic, NFData, Serialize)

-- | A page's placed-location table plus its own id allocator.
data LocationInstances = LocationInstances
    { lisNextId        ∷ !Int
      -- ^ next id to hand out; every live id is strictly below it
    , lisById          ∷ !(HM.HashMap LocationInstanceId LocationInstance)
    , lisPendingLegacy ∷ !(Maybe LegacyLocationChunkFlags)
      -- ^ TRANSIENT, never serialized (the save DTO has no field for
      --   it, and 'World.Generate.Types.WorldGenParams' skips it the
      --   same way it skips @wgpVolcanoCtx@). 'Just' ONLY between a
      --   pre-#911 (@world-pages@ v1) payload decode — which carries the
      --   old per-chunk discovered\/contents-spawned sets but no
      --   instances — and 'resolveLegacyLocationInstances', which the
      --   load path runs at its content-validation stage, the one place
      --   the location registry needed to resolve bounds\/labels is
      --   actually available. 'Nothing' everywhere else: at placement
      --   time, after a v2 decode, and after resolution.
    } deriving (Show, Eq, Generic, NFData)

-- | The pre-#911 per-chunk flags a @world-pages@ v1 payload carries,
--   held until 'resolveLegacyLocationInstances' can turn them into
--   instance state. @wgpLocationStamped@ is NOT here — it stays a chunk
--   property and is migrated by nothing.
data LegacyLocationChunkFlags = LegacyLocationChunkFlags
    { llcDiscovered      ∷ !(HS.HashSet ChunkCoord)
    , llcContentsSpawned ∷ !(HS.HashSet ChunkCoord)
    } deriving (Show, Eq, Generic, NFData)

emptyLocationInstances ∷ LocationInstances
emptyLocationInstances = LocationInstances
    { lisNextId        = firstLocationInstanceId
    , lisById          = HM.empty
    , lisPendingLegacy = Nothing
    }

-- * Checked geometry construction ------------------------------------

-- | A placement whose geometry cannot be represented (#1796), with
--   enough attribution to act on: which definition, which chunk, which
--   component, and the exact value that component would have held.
--
--   Produced only by 'locationInstanceGeometry', and propagated
--   unchanged by every construction path below, so an unrepresentable
--   placement is refused BEFORE any 'LocationInstance' — or any partial
--   'LocationInstances' table — exists. That is the difference from
--   'locationInstanceBoundsErrors', which is a save-boundary check on
--   geometry that is already live.
data LocationGeometryError = LocationGeometryError
    { lgeDefId     ∷ !Text
    , lgeChunk     ∷ !ChunkCoord
    , lgeComponent ∷ !Text
    , lgeValue     ∷ !Integer
    } deriving (Show, Eq, Generic, NFData)

-- | Render a geometry failure for a log line or a load rejection.
locationGeometryErrorText ∷ LocationGeometryError → Text
locationGeometryErrorText err =
    "location '" <> lgeDefId err <> "' anchored at chunk ("
        <> tshow cx <> ", " <> tshow cy <> "): " <> lgeComponent err
        <> " would be " <> tshow (lgeValue err)
        <> ", which is not a representable tile coordinate"
  where
    ChunkCoord cx cy = lgeChunk err

-- | A chunk's anchor tile — its centre, the tile every location stamp,
--   query, and bounds translation has always been anchored at — as
--   exact 'Integer's.
--
--   'ChunkCoord' holds unrestricted 'Int's and nothing caps them
--   (neither 'World.Generate.Config.Normalize.normalizeWorldSize' nor
--   @world.init@), so @cx * chunkSize + half@ can overflow on its own:
--   at 'World.Chunk.Types.chunkSize' 16, @ChunkCoord (2^59 - 1) 0@
--   already lands on @maxBound - 7@ and one more chunk wraps. Computing
--   in 'Integer' is what makes that observable instead of silent; the
--   result is narrowed only by 'locationAnchorTileChecked' (#1796).
locationAnchorTileInteger ∷ ChunkCoord → (Integer, Integer)
locationAnchorTileInteger (ChunkCoord cx cy) =
    let side = toInteger chunkSize
        half = toInteger (chunkSize `div` 2)
    in (toInteger cx * side + half, toInteger cy * side + half)

-- | 'locationAnchorTileInteger' narrowed back to tile coordinates, or
--   the first anchor component that does not fit (#1796). For any chunk
--   a real world places — 'Location.Overlay.allCoords' spans about
--   ±8,184 tiles even on the largest advertised 1,024-chunk world —
--   this is the identity on the old unchecked arithmetic.
locationAnchorTileChecked
    ∷ ChunkCoord → Either LocationGeometryFailure (Int, Int)
locationAnchorTileChecked coord =
    let (ax, ay) = locationAnchorTileInteger coord
    in (,) ⊚ narrowTileCoordinate "anchor.x" ax
           ⊛ narrowTileCoordinate "anchor.y" ay

-- | The ONE checked route from a chunk coordinate and a definition to
--   placed geometry (#1796) — the anchor tile and the definition's box
--   translated onto it.
--
--   Every component is computed in 'Integer' and checked; the @(Int,
--   Int)@ anchor and the 'AbsBounds' are constructed only once all six
--   are known representable, so this never wraps, never clamps, and
--   never returns an inverted box. A failure carries the definition id
--   and chunk coordinate ('LocationGeometryError').
--
--   Ordering is preserved for free on every accepted result: the two
--   ends of an axis are offset by the same anchor, exactly as
--   'Location.Bounds.translateBounds' does, and that function stays the
--   suites' oracle for the values this must produce.
locationInstanceGeometry
    ∷ ChunkCoord → LocationDef
    → Either LocationGeometryError ((Int, Int), AbsBounds)
locationInstanceGeometry coord def =
    case ( locationAnchorTileChecked coord
         , translateBoundsChecked (locationAnchorTileInteger coord)
                                  (ldBounds def) ) of
        (Left f,  _)              → Left (attribute f)
        (_,       Left f)         → Left (attribute f)
        (Right anchor, Right box) → Right (anchor, box)
  where
    attribute f =
        LocationGeometryError (ldId def) coord (lgfComponent f) (lgfValue f)

-- | A fresh, undiscovered instance with its geometry resolved from
--   @def@ and its name rendered from @namer@ — the page's language
--   (#1101), or 'Nothing' for a page that has none, which names it
--   @def@'s 'ldLabel' with no gloss.
--
--   This is the ONLY place either name field is ever written, and since
--   #1796 the only place an instance is built at all: geometry comes
--   from 'locationInstanceGeometry', so an unrepresentable placement
--   yields a 'LocationGeometryError' and no instance rather than a
--   wrapped anchor and an inverted box.
newLocationInstance
    ∷ Maybe LocationNamer → LocationInstanceId → ChunkCoord → LocationDef
    → Either LocationGeometryError LocationInstance
newLocationInstance = newLocationInstanceWithSeed 0

-- | Seed-aware instance constructor used by real world placement. The
--   compatibility wrapper above keeps tests and debug allocation honest with
--   a fixed seed, while production includes the page's persisted world seed.
newLocationInstanceWithSeed
    ∷ Word64 → Maybe LocationNamer → LocationInstanceId → ChunkCoord → LocationDef
    → Either LocationGeometryError LocationInstance
newLocationInstanceWithSeed seed namer iid coord def = do
    (anchor, box) ← locationInstanceGeometry coord def
    let (name, gloss, ety) = nameLocationInstance namer def
                                 (unLocationInstanceId iid)
    -- The clearance notice starts SPENT exactly when the instance is
    -- born already satisfied — a zero-roll encounter with no
    -- significant items, which is #916's own @leClearEventEmitted =
    -- rolled == 0@ rule generalized. Nobody cleared such a place, so
    -- discovering it must not announce a clearance; a location with any
    -- outstanding condition starts unspent and earns its one notice
    -- when the last condition falls.
    pure $ withInitialClearFeedback LocationInstance
        { liId              = iid
        , liDefId           = ldId def
        , liChunk           = coord
        , liAnchor          = anchor
        , liBounds          = box
        , liDisplayName     = name
        , liGloss           = gloss
        , liEtymology       = ety
        , liLifecycle       = LifecycleUnknown
        , liContentsSpawned = False
        , liEncounter       = encounterFromDef seed iid def
        , liSignificant     = significantItemsFromDef def
        , liClearEventEmitted = False
        }

-- | Seat a freshly built instance's one-shot clearance notice: spent
--   when the instance is already clearance-satisfied at birth, unspent
--   otherwise. See 'newLocationInstanceWithSeed'.
withInitialClearFeedback ∷ LocationInstance → LocationInstance
withInitialClearFeedback inst =
    inst { liClearEventEmitted = locationClearanceSatisfied inst }

-- | At most one ranged unit-content entry is accepted by the YAML boundary.
--   Its inclusive range is rolled by a stateless avalanche hash over the
--   persisted page seed and stable instance id: uniform over the authored
--   span, independent of chunk/load order, and never rolled again.
encounterFromDef ∷ Word64 → LocationInstanceId → LocationDef
                 → Maybe LocationEncounter
encounterFromDef seed iid def = case
    [ (range, lconClearance c)
    | c ← ldContents def, Just range ← [lconCountRange c] ] of
        []          → Nothing
        (((lo, hi), policy):_) →
            let spanSize = fromIntegral (hi - lo + 1) ∷ Word64
                rawId = fromIntegral (unLocationInstanceId iid) ∷ Word64
                rolled = lo + fromIntegral
                    (fmix64 (seed `xor` (rawId * 0xD6E8FEB86659FD93))
                        `mod` spanSize)
            in Just LocationEncounter
                { leRolledCount        = rolled
                , leOccupants          = []
                , leRosterComplete     = rolled ≡ 0
                , leDeathOnlyClearance = policy ≡ Just "death_only"
                , leActivated          = False
                , leEpisodeActive      = False
                , leAggressionAnnounced = False
                , leDisengageAnnounced  = False
                , leCleared            = rolled ≡ 0
                }

-- * Queries ----------------------------------------------------------

-- | Every instance, ordered by id — a canonical total order, never
--   'HM.HashMap' iteration order, so anything that surfaces or renders
--   instances is deterministic call over call (the same reason
--   'Location.Overlay.Types.overlayToList' sorts).
instancesToList ∷ LocationInstances → [LocationInstance]
instancesToList = sortOn liId . HM.elems . lisById

-- | How many instances there are, without materialising them. Exists
--   for the scene-assembly telemetry (#1921), which must report how
--   many the zoom-map icon pass enumerates without allocating a second
--   ordered copy of the list it is counting.
instancesCount ∷ LocationInstances → Int
instancesCount = HM.size . lisById

lookupLocationInstance
    ∷ LocationInstanceId → LocationInstances → Maybe LocationInstance
lookupLocationInstance iid = HM.lookup iid . lisById

-- | Every instance anchored in a chunk, in id order. More than one is
--   legitimate — that two locations in one chunk stay independently
--   addressable is the whole point of instance keying. Used by the
--   coordinate-addressed compatibility wrappers the Lua location
--   scripts still call.
instancesInChunk ∷ ChunkCoord → LocationInstances → [LocationInstance]
instancesInChunk coord =
    filter ((≡ coord) . liChunk) . instancesToList

-- | Every instance's stored absolute bounds, in id order (#778 portal
--   placement exclusion, #779 remote-start distance).
locationInstanceBounds ∷ LocationInstances → [AbsBounds]
locationInstanceBounds = map liBounds . instancesToList

-- * Construction / mutation ------------------------------------------

-- | Allocate one instance per overlay entry, in the overlay's canonical
--   @(cx, cy)@ order. Ids are assigned across the FULL placement list,
--   so an entry whose definition is not currently registered consumes
--   its id without producing an instance rather than shifting every
--   later id — the id → (definition, anchor) mapping then depends only
--   on the deterministic overlay, never on which YAML happens to be
--   loaded. (That tolerance is defensive only: the overlay is computed
--   FROM the registered defs at world init, and the load path rejects a
--   save naming an unregistered location def before this ever runs.)
--
--   #1796: ALL-OR-NOTHING. One unrepresentable placement fails the whole
--   build with that placement's 'LocationGeometryError' and produces no
--   table at all — not a table missing one instance, and not a table
--   whose allocator counted an instance that was never built. The
--   registry tolerance above is unchanged and is a different case
--   entirely: an entry naming an UNREGISTERED def still consumes its id
--   and produces no instance, on the success path.
buildLocationInstances
    ∷ Maybe LocationNamer → LocationRegistry → LocationOverlay
    → Either LocationGeometryError LocationInstances
buildLocationInstances = buildLocationInstancesWithSeed 0

-- | Production placement counterpart of 'buildLocationInstances', salted by
--   the page's persisted world seed for one-time encounter rolls (#916).
buildLocationInstancesWithSeed
    ∷ Word64 → Maybe LocationNamer → LocationRegistry → LocationOverlay
    → Either LocationGeometryError LocationInstances
buildLocationInstancesWithSeed seed namer registry overlay = do
    built ← traverse mk entries
    pure LocationInstances
        { lisNextId        = firstLocationInstanceId + length entries
        , lisById          = HM.fromList [ (liId i, i) | Just i ← built ]
        , lisPendingLegacy = Nothing
        }
  where
    entries = zip [firstLocationInstanceId ..] (overlayToList overlay)
    mk (n, (coord, lid)) = case lookupLocation lid registry of
        Nothing  → Right Nothing
        Just def → Just ⊚ newLocationInstanceWithSeed seed namer
                        (LocationInstanceId n) coord def

-- | Add one instance under a freshly allocated id. The engine's own
--   placement pass uses 'buildLocationInstances'; this is the seam a
--   later feature (or a test building two instances in one chunk) adds
--   through without reaching into 'lisNextId' by hand.
--
--   #1796: an unrepresentable placement returns its
--   'LocationGeometryError' and leaves @lis@ ENTIRELY untouched —
--   'lisNextId' does not advance and 'lisById' gains nothing — so a
--   refused allocation costs no id and the table a caller already holds
--   stays exactly as valid as it was.
allocateLocationInstance
    ∷ Maybe LocationNamer → ChunkCoord → LocationDef → LocationInstances
    → Either LocationGeometryError (LocationInstanceId, LocationInstances)
allocateLocationInstance namer coord def lis = do
    let iid = LocationInstanceId (lisNextId lis)
    inst ← newLocationInstance namer iid coord def
    pure ( iid
         , lis { lisNextId = lisNextId lis + 1
               , lisById   = HM.insert iid inst (lisById lis)
               }
         )

-- | Update one instance in place; a no-op for an unknown id.
adjustLocationInstance
    ∷ LocationInstanceId → (LocationInstance → LocationInstance)
    → LocationInstances → LocationInstances
adjustLocationInstance iid f lis =
    lis { lisById = HM.adjust f iid (lisById lis) }

-- | Apply a lifecycle transition to one instance. 'Nothing' when the id
--   is unknown or the transition is refused by 'promoteLifecycle' — the
--   caller uses that to decide whether anything actually happened (e.g.
--   whether to emit a discovery event).
setLocationLifecycle
    ∷ LocationInstanceId → LocationLifecycle → LocationInstances
    → Maybe LocationInstances
setLocationLifecycle iid next lis = do
    inst ← lookupLocationInstance iid lis
    l'   ← promoteLifecycle (liLifecycle inst) next
    pure (adjustLocationInstance iid (\i → i { liLifecycle = l' }) lis)

-- | Mark one instance's contents spawned (#90). Independent of
--   'liLifecycle' and of the chunk-keyed stamp flag (#424); a no-op for
--   an unknown id.
markLocationContentsSpawned
    ∷ LocationInstanceId → LocationInstances → LocationInstances
markLocationContentsSpawned iid =
    adjustLocationInstance iid (\i → i { liContentsSpawned = True })

-- | Install the exact roster allocated by the content spawn. Repeating the
--   call is an idempotent no-op once complete, so a chunk-load retry cannot
--   replace the originally assigned membership or homes.
registerLocationEncounterOccupants
    ∷ LocationInstanceId → [(UnitId, (Float, Float))] → LocationInstances
    → LocationInstances
registerLocationEncounterOccupants iid assigned =
    adjustLocationInstance iid $ \inst → inst
        { liEncounter = case liEncounter inst of
            Just e | not (leRosterComplete e) → Just e
                { leOccupants =
                    [ LocationEncounterOccupant uid home False False
                    | (uid, home) ← take (leRolledCount e) assigned ]
                , leRosterComplete =
                    length assigned ≡ leRolledCount e
                    ∧ HS.size (HS.fromList (map fst assigned))
                        ≡ leRolledCount e
                }
            other → other
        }

-- | Mutate one persisted occupant row, preserving roster membership even
--   when the live unit itself is unresolved.
adjustLocationEncounterOccupant
    ∷ LocationInstanceId → UnitId
    → (LocationEncounterOccupant → LocationEncounterOccupant)
    → LocationInstances → LocationInstances
adjustLocationEncounterOccupant iid uid f =
    adjustLocationInstance iid adjust
  where
    adjust inst = inst { liEncounter = alter ⊚ liEncounter inst }
    alter e = e { leOccupants = map (\o → if leoUnitId o ≡ uid then f o else o)
                                    (leOccupants e) }

-- | Replace the encounter-wide episode state. Feedback flags live here,
--   rather than on each occupant, because one ruin episode emits one initial
--   aggression and one all-disengaged notice regardless of how many guards
--   join it. Entering combat permanently records activation and promotes an
--   already-visible location to @active@; an unknown location remains hidden
--   until ordinary sight discovers it.
setLocationEncounterEpisodeState
    ∷ LocationInstanceId → Bool → Bool → Bool
    → LocationInstances → LocationInstances
setLocationEncounterEpisodeState iid active aggressionAnnounced
        disengageAnnounced =
    adjustLocationInstance iid $ \inst → case liEncounter inst of
        Nothing → inst
        Just e →
            let lifecycle =
                    if active ∧ isDiscoveredLifecycle (liLifecycle inst)
                    then fromMaybe (liLifecycle inst)
                           (promoteLifecycle (liLifecycle inst) LifecycleActive)
                    else liLifecycle inst
            in inst
                { liLifecycle = lifecycle
                , liEncounter = Just e
                    { leActivated = leActivated e ∨ active
                    , leEpisodeActive = active
                    , leAggressionAnnounced = aggressionAnnounced
                    , leDisengageAnnounced = disengageAnnounced
                    }
                }

-- | Record death-only ENCOUNTER completion, and nothing else (#917).
--   'Nothing' when the instance is absent, authors no encounter, or has
--   already completed it.
--
--   Since #917 this deliberately does NOT touch the lifecycle or the
--   clearance notice. Completing the roster is one CONJUNCT of the
--   location's clearance predicate, not the clearance itself: a ruin
--   whose guaranteed significant item is still on the floor stays
--   uncleared with @leCleared@ true. 'resolveLocationClearance' is the
--   single writer of the cleared transition and of
--   'liClearEventEmitted', so whichever conjunct lands last promotes
--   the location exactly once.
markLocationEncounterCleared
    ∷ LocationInstanceId → LocationInstances → Maybe LocationInstances
markLocationEncounterCleared iid lis = do
    inst ← lookupLocationInstance iid lis
    e ← liEncounter inst
    guard (not (leCleared e))
    pure $ adjustLocationInstance iid (\i → i
        { liEncounter = Just e
            { leCleared = True
            , leEpisodeActive = False
            }
        }) lis

-- | Bind one spawned significant item to its obligation slot (#917):
--   the item's physical 'Item.Types.iiInstanceId', recorded by the
--   content spawn as soon as the spawn succeeds.
--
--   WRITE-ONCE per slot, and refused rather than overwritten: a slot
--   that already names an item keeps it, so a retried or duplicated
--   content spawn cannot repoint an obligation at a second physical
--   item and orphan the first.
--
--   @defName@ is the SPAWNED item's own definition name, and it must
--   equal the slot's authored 'lsiItemDefName'. Without that check any
--   ground item would satisfy any obligation: binding a ration to a
--   @processing_unit@ slot would let picking the ration up latch the
--   slot and clear the location, with the guaranteed item still lying
--   where it spawned. The obligation names WHAT is owed, so the
--   binding has to prove the item it is offered is that thing.
--
--   'Nothing' when the instance or the slot is unknown, the slot is
--   already bound, or the item is the wrong definition. The
--   already-bound case is exactly the edge a resuming spawn uses to
--   tell "still owed" from "already done"; the others are refusals.
registerLocationSignificantSpawn
    ∷ LocationInstanceId → Int → Text → Word64 → LocationInstances
    → Maybe LocationInstances
registerLocationSignificantSpawn iid slot defName itemId lis = do
    inst ← lookupLocationInstance iid lis
    entry ← find ((≡ slot) . lsiSlot) (liSignificant inst)
    guard (isNothing (lsiInstanceId entry))
    guard (lsiItemDefName entry ≡ defName)
    pure $ adjustLocationInstance iid (\i → i
        { liSignificant =
            [ if lsiSlot e ≡ slot then e { lsiInstanceId = Just itemId } else e
            | e ← liSignificant i ]
        }) lis

-- | Latch @taken@ on whichever obligation of ANY instance on this page
--   owns @itemId@ (#917 requirement 3), the first time that physical
--   item is successfully picked up — by any unit, of any faction.
--
--   'Nothing' when no obligation on the page names the item, or the one
--   that does is already latched, so the caller pays nothing for an
--   ordinary pickup of ordinary salvage. Idempotent by construction:
--   once true it is never written again, and nothing anywhere clears
--   it.
latchLocationSignificantTaken
    ∷ Word64 → LocationInstances → Maybe LocationInstances
latchLocationSignificantTaken itemId lis =
    case [ liId inst
         | inst ← instancesToList lis
         , entry ← liSignificant inst
         , lsiInstanceId entry ≡ Just itemId
         , not (lsiTaken entry) ] of
        []      → Nothing
        (iid:_) → Just $ adjustLocationInstance iid (\i → i
            { liSignificant =
                [ if lsiInstanceId e ≡ Just itemId then e { lsiTaken = True }
                                                   else e
                | e ← liSignificant i ]
            }) lis

-- | The ONE compound-clearance transition (#917 requirement 7). Applies
--   the predicate to one instance and, when it holds and the location
--   is already VISIBLE, promotes it to 'LifecycleCleared' and spends
--   its one clearance notice.
--
--   'Just' means — and means only — that the caller now owes exactly
--   one player-facing clearance event. 'Nothing' means nothing changed:
--   the instance is unknown, authors no clearance condition at all
--   (which must never clear through an empty conjunction), has a
--   condition still outstanding, has already spent its notice, or is
--   still undiscovered — in which case the completion stays PRIVATE and
--   this is retried on the discovery edge, which is what makes a
--   location completed before it was ever seen announce itself once,
--   when it is seen, instead of leaking.
--
--   The promotion goes through 'promoteLifecycle', so an instance
--   already at or beyond 'LifecycleCleared' keeps its state while still
--   spending the notice — the single call site for both the clearance
--   tick and the discovery edge, where discovery has already landed the
--   instance on 'LifecycleCleared' itself.
resolveLocationClearance
    ∷ LocationInstanceId → LocationInstances → Maybe LocationInstances
resolveLocationClearance iid lis = do
    inst ← lookupLocationInstance iid lis
    guard (locationAuthorsClearance inst)
    guard (not (liClearEventEmitted inst))
    guard (locationClearanceSatisfied inst)
    guard (isDiscoveredLifecycle (liLifecycle inst))
    let lifecycle = fromMaybe (liLifecycle inst)
            (promoteLifecycle (liLifecycle inst) LifecycleCleared)
    pure $ adjustLocationInstance iid (\i → i
        { liLifecycle         = lifecycle
        , liClearEventEmitted = True
        }) lis

-- * v1 chunk-set migration -------------------------------------------

-- | Wrap the pre-#911 per-chunk flags for later resolution. Produces an
--   otherwise-empty table: the instances themselves cannot be built
--   until the location registry is available.
pendingLegacyFlags
    ∷ HS.HashSet ChunkCoord  -- ^ legacy @wgpLocationDiscovered@
    → HS.HashSet ChunkCoord  -- ^ legacy @wgpLocationContentsSpawned@
    → LocationInstances
pendingLegacyFlags discovered spawned = emptyLocationInstances
    { lisPendingLegacy = Just (LegacyLocationChunkFlags discovered spawned) }

-- | Turn a pending v1 chunk-set carry into real instances: one per
--   overlay entry (via 'buildLocationInstances', so migrated ids match
--   what a fresh placement of the same overlay would allocate), with
--   each chunk's legacy discovered / contents-spawned marker mapped
--   onto the instance occupying it. A marker naming a chunk with no
--   overlay entry identifies no placed instance and is discarded.
--
--   Idempotent and total: a table with nothing pending (a payload from
--   any version that carries instances, a freshly placed world, or an
--   already-resolved one) is returned untouched, so this can never
--   overwrite stored instance state with values re-derived from a
--   definition edited since placement.
--
--   Reconstructed instances are named from 'ldLabel' with NO gloss
--   (#1101 requirement 7): this path runs only for a pre-#911 payload,
--   which predates language provenance entirely, so there is no
--   language to name them in and one must never be invented. Every
--   later payload carries its instances — and their already-rendered
--   names — and never reaches this function at all.
--
--   The same historical boundary applies to encounters (#916): this
--   migration necessarily consults today's definitions to recover the old
--   instance table, but it must not let a newly-authored @count_range@
--   retroactively populate a pre-encounter world. Reconstructed instances
--   therefore explicitly discard the constructor's encounter roll.
--
--   #1796: the rebuild goes through the checked construction, so a
--   legacy payload whose SAVED overlay names a chunk coordinate outside
--   the representable envelope propagates that failure instead of
--   resolving into wrapped geometry. The load path turns it into a
--   rejection before anything is staged or published
--   ('Engine.Scripting.Lua.API.Save.loadSaveFn'). No valid legacy save
--   is affected: generated overlays span
--   'Location.Overlay.allCoords' 's @[-half .. half-1]@ chunk range,
--   and no registered definition can carry an out-of-domain box.
resolveLegacyLocationInstances
    ∷ LocationRegistry → LocationOverlay → LocationInstances
    → Either LocationGeometryError LocationInstances
resolveLegacyLocationInstances registry overlay lis =
    case lisPendingLegacy lis of
        Nothing → Right lis
        Just flags → do
            base ← buildLocationInstances Nothing registry overlay
            pure base { lisById = HM.map (applyFlags flags) (lisById base) }
  where
    applyFlags flags inst = inst
        { liLifecycle =
            if HS.member (liChunk inst) (llcDiscovered flags)
                then LifecycleDiscovered
                else LifecycleUnknown
        , liContentsSpawned =
            HS.member (liChunk inst) (llcContentsSpawned flags)
        , liEncounter = Nothing
        -- Same historical boundary, same reason (#917): the rebuild
        -- necessarily consults TODAY's definitions to recover the table,
        -- so a significant item authored since would otherwise appear
        -- as an obligation this world never spawned — an eternally
        -- unclearable location invented at load. A pre-#911 payload owes
        -- none, and its notice is unspent because it never cleared.
        , liSignificant = []
        , liClearEventEmitted = False
        }

-- * Validation -------------------------------------------------------

-- | Component-local invariants for a decoded table (mirrors the
--   ground-item allocator check @World.Save.Component.Page@ already
--   runs): the allocator itself sits at or above
--   'firstLocationInstanceId', every id sits at or above that same
--   floor and strictly below the allocator, and every map key matches
--   the 'liId' of the instance stored under it. Literal duplicate ids
--   are structurally impossible once decoded into a 'HM.HashMap'.
--
--   #1667: the allocator's OWN floor is checked separately from the
--   per-id comparison, and therefore independently of whether the table
--   is empty — an empty table used to certify any cursor at all,
--   including 0 and (the field being an unrestricted wire 'Int') a
--   negative one, which 'allocateLocationInstance' would then hand out
--   verbatim as the next 'LocationInstanceId'. No engine path produces
--   such a cursor ('emptyLocationInstances' and 'buildLocationInstances'
--   both start at or above the floor), so this is hardening against a
--   corrupt or hand-crafted payload. An empty table whose cursor IS
--   valid stays well-formed, exactly as before.
--   'World.Save.Component.Transfer.validateTransferOrders' is the
--   precedent this generalizes.
locationInstanceAllocatorErrors ∷ LocationInstances → [Text]
locationInstanceAllocatorErrors lis =
    [ "location-instance allocator is " <> tshow (lisNextId lis)
        <> ", below the first valid location-instance id ("
        <> tshow firstLocationInstanceId <> ")"
    | lisNextId lis < firstLocationInstanceId
    ]
    ⧺
    [ "location instance #" <> tshow (unLocationInstanceId iid)
        <> " is not below the page's location-instance allocator ("
        <> tshow (lisNextId lis) <> ")"
    | iid ← HM.keys (lisById lis)
    , unLocationInstanceId iid ≥ lisNextId lis
        ∨ unLocationInstanceId iid < firstLocationInstanceId
    ]
    ⧺
    [ "location instance keyed #" <> tshow (unLocationInstanceId iid)
        <> " carries id #" <> tshow (unLocationInstanceId (liId inst))
    | (iid, inst) ← HM.toList (lisById lis)
    , liId inst ≢ iid
    ]

-- | Component-local GEOMETRY invariant for a decoded table (#1668): the
--   stored 'liBounds' box of every instance must be ORDERED on both
--   axes, @abMinX ≤ abMaxX@ and @abMinY ≤ abMaxY@. Empty ⇒ every box is
--   well-formed.
--
--   The SAVE decode path is the construction site the authored-bounds
--   gate does not cover at all:
--   'World.Save.Component.WorldGen.fromAbsBoundsDTO' copies four
--   unrestricted 'Int's straight off the wire, so a corrupt or
--   hand-edited payload reaches an 'AbsBounds' without passing
--   "Engine.Asset.YamlLocations" 's inverted-bounds rejection (#777) —
--   which is why this rule needs a second implementation here rather
--   than being left to #1151's single YAML-side one.
--
--   Since #1796 decode is the ONLY remaining source. The engine side
--   is now proved rather than merely likely: 'newLocationInstance'
--   builds every box through 'locationInstanceGeometry', which computes
--   the anchor and all four translated bounds in 'Integer' and refuses
--   the placement outright unless every component is representable —
--   and an accepted translation offsets both ends of an axis by the
--   same anchor, so ordering is preserved. The old qualifier that made
--   this only DOMINANT — unchecked 'Int' addition under a loader that
--   constrained ordering but not RANGE, which let an extreme authored
--   box at a nonzero anchor wrap and invert — no longer holds:
--   'Engine.Asset.YamlLocations.authoredLocationCoordinateLimit' bounds
--   authored coordinates to @±(2^31 - 1)@, and the checked construction
--   above covers arbitrary chunk coordinates, which that limit alone
--   never could.
--
--   This check is NOT thereby redundant, and stays exactly as it is:
--   'World.Save.Component.WorldGen.fromAbsBoundsDTO' still copies four
--   unrestricted wire 'Int's, which no construction-time proof can
--   reach. A wrapped box is not a usable footprint, so refusing to
--   publish it is correct whichever site produced it.
--
--   An inverted box fails SILENTLY rather than loudly, differently in
--   each consumer: 'Location.Bounds.boundsContainsPoint' is false at
--   every wrap image, so 'Location.Discovery' can never reveal the
--   location, while 'Location.Bounds.boundsIntersect' compares each
--   box's min against the other's max and still reports overlap, so
--   #778's placement exclusion blocks valid ground far away.
--
--   A DEGENERATE box — @min ≡ max@ on either or both axes — is a
--   legitimate 1x1 footprint under inclusive bounds and stays valid,
--   exactly as the YAML loader accepts it.
--
--   Each inverted axis is reported SEPARATELY, so a box inverted on
--   BOTH names both rather than reporting one unspecified inversion.
--   Entries are addressed by their MAP KEY, like the allocator check
--   above; a key that disagrees with the instance's own 'liId' is that
--   check's finding, not this one's.
locationInstanceBoundsErrors ∷ LocationInstances → [Text]
locationInstanceBoundsErrors lis =
    [ "location instance #" <> tshow (unLocationInstanceId iid)
        <> " has inverted bounds on the " <> axis <> " axis ("
        <> lo <> " > " <> hi <> ")"
    | (iid, inst) ← sortOn (unLocationInstanceId . fst) (HM.toList (lisById lis))
    , (axis, lo, hi) ← invertedAxes (liBounds inst)
    ]
  where
    invertedAxes b =
        [ ("x", "minX " <> tshow (abMinX b), "maxX " <> tshow (abMaxX b))
        | abMinX b > abMaxX b ]
        ⧺
        [ ("y", "minY " <> tshow (abMinY b), "maxY " <> tshow (abMaxY b))
        | abMinY b > abMaxY b ]

-- | Component-local invariants for a decoded table's SIGNIFICANT
--   obligations (#917), mirroring the two checks above: an
--   obligation-shaped payload that no engine path can produce is
--   rejected at decode rather than published as gameplay authority.
--
--   Rules about the obligation SET rather than about whether any
--   particular item still exists (which is "World.Save.Integrity" 's
--   reference question, tolerant of absence by design):
--
--   * a slot is at or above 1. 'significantItemsFromDef' numbers from
--     there and nothing else allocates one, but the field is an
--     unrestricted wire 'Int', and a slot below the floor is
--     UNBINDABLE: 'Engine.Scripting.Lua.API.World.Edit' refuses a
--     non-positive slot outright, so the content spawn would fail,
--     leave @contents_spawned@ unmarked, and re-spawn an orphaned item
--     on every chunk load for ever. Same shape as the allocator floor
--     'locationInstanceAllocatorErrors' checks (#1667), and hardening
--     against the same corrupt-or-hand-crafted payload;
--   * slots are unique within one instance — they are the address a
--     resuming content spawn registers against, so two slots sharing a
--     number would let one spawn satisfy both, or repoint the wrong
--     one;
--   * an obligation marked TAKEN must name the item that was taken.
--     No engine path can produce the other shape
--     ('latchLocationSignificantTaken' matches on a bound id), and it
--     is exactly the shape the session-wide provenance rules cannot
--     see — there is no id for them to resolve — so a corrupt or
--     hand-crafted payload could otherwise clear a location with
--     nothing ever spawned. 'significantRecovered' refuses to count it
--     either way; this rejects it before it is published at all;
--   * one physical item identity is owned by at most ONE obligation
--     across the whole page. A single 'Item.Types.iiInstanceId' bound
--     to two obligations would let one pickup latch both, clearing a
--     location whose own item was never taken.
--
--   Entries are addressed by their instance's id and slot, and each
--   violation is reported once, in a deterministic order.
locationSignificantItemErrors ∷ LocationInstances → [Text]
locationSignificantItemErrors lis =
    [ "location instance #" <> tshow (unLocationInstanceId (liId inst))
        <> " declares significant slot " <> tshow (lsiSlot e)
        <> ", below the first valid slot (1)"
    | inst ← instancesToList lis
    , e ← sortOn lsiSlot (liSignificant inst)
    , lsiSlot e < 1
    ]
    ⧺
    [ "location instance #" <> tshow (unLocationInstanceId (liId inst))
        <> " declares significant slot " <> tshow slot <> " more than once"
    | inst ← instancesToList lis
    , (slot, n) ← sortOn fst (HM.toList (HM.fromListWith (+)
        [ (lsiSlot e, 1 ∷ Int) | e ← liSignificant inst ]))
    , n > 1
    ]
    ⧺
    [ "location instance #" <> tshow (unLocationInstanceId (liId inst))
        <> " significant slot " <> tshow (lsiSlot e)
        <> " is marked taken but names no item instance"
    | inst ← instancesToList lis
    , e ← sortOn lsiSlot (liSignificant inst)
    , lsiTaken e
    , isNothing (lsiInstanceId e)
    ]
    ⧺
    [ "significant item instance " <> tshow itemId
        <> " is owned by more than one location obligation: "
        <> T.intercalate ", " owners
    | (itemId, owners) ← sortOn fst (HM.toList (HM.fromListWith (flip (⧺))
        [ ( iid
          , [ "#" <> tshow (unLocationInstanceId (liId inst))
                  <> " slot " <> tshow (lsiSlot e) ] )
        | inst ← instancesToList lis
        , e ← liSignificant inst
        , Just iid ← [lsiInstanceId e] ]))
    , length owners > 1
    ]
