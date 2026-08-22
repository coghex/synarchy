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
      -- * Records
    , LocationInstance(..)
    , LocationInstances(..)
    , LegacyLocationChunkFlags(..)
    , emptyLocationInstances
    , newLocationInstance
    , locationAnchorTile
      -- * Queries
    , instancesToList
    , lookupLocationInstance
    , instancesInChunk
    , locationInstanceBounds
      -- * Construction / mutation
    , buildLocationInstances
    , allocateLocationInstance
    , adjustLocationInstance
    , setLocationLifecycle
    , markLocationContentsSpawned
      -- * v1 chunk-set migration
    , pendingLegacyFlags
    , resolveLegacyLocationInstances
      -- * Validation
    , locationInstanceAllocatorErrors
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.List (sortOn)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Location.Bounds (AbsBounds, translateBounds)
import Language.Etymology.Source (EtymologySource)
import Location.Naming (LocationNamer, nameLocationInstance)
import Location.Overlay.Types (LocationOverlay, overlayToList)
import Location.Types (LocationDef(..), LocationRegistry, lookupLocation)
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
--   #911 lands the representation only. Nothing in the game yet moves
--   an instance past 'LifecycleDiscovered' — the encounter (step 4),
--   reward (step 5), and retrieval (step 6) issues are what these
--   states exist to serve.
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
      -- ^ absolute anchor tile ('locationAnchorTile' of 'liChunk' at
      --   placement time), stored rather than re-derived
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

-- | A chunk's anchor tile — its centre, the tile every location stamp,
--   query, and bounds translation has always been anchored at.
locationAnchorTile ∷ ChunkCoord → (Int, Int)
locationAnchorTile (ChunkCoord cx cy) =
    let half = chunkSize `div` 2
    in (cx * chunkSize + half, cy * chunkSize + half)

-- | A fresh, undiscovered instance with its geometry resolved from
--   @def@ and its name rendered from @namer@ — the page's language
--   (#1101), or 'Nothing' for a page that has none, which names it
--   @def@'s 'ldLabel' with no gloss.
--
--   This is the ONLY place either name field is ever written.
newLocationInstance
    ∷ Maybe LocationNamer → LocationInstanceId → ChunkCoord → LocationDef
    → LocationInstance
newLocationInstance namer iid coord def =
    let anchor              = locationAnchorTile coord
        (name, gloss, ety)  = nameLocationInstance namer def
                                 (unLocationInstanceId iid)
    in LocationInstance
        { liId              = iid
        , liDefId           = ldId def
        , liChunk           = coord
        , liAnchor          = anchor
        , liBounds          = translateBounds anchor (ldBounds def)
        , liDisplayName     = name
        , liGloss           = gloss
        , liEtymology       = ety
        , liLifecycle       = LifecycleUnknown
        , liContentsSpawned = False
        }

-- * Queries ----------------------------------------------------------

-- | Every instance, ordered by id — a canonical total order, never
--   'HM.HashMap' iteration order, so anything that surfaces or renders
--   instances is deterministic call over call (the same reason
--   'Location.Overlay.Types.overlayToList' sorts).
instancesToList ∷ LocationInstances → [LocationInstance]
instancesToList = sortOn liId . HM.elems . lisById

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
buildLocationInstances
    ∷ Maybe LocationNamer → LocationRegistry → LocationOverlay
    → LocationInstances
buildLocationInstances namer registry overlay = LocationInstances
    { lisNextId        = firstLocationInstanceId + length entries
    , lisById          = HM.fromList [ (liId i, i) | Just i ← map mk entries ]
    , lisPendingLegacy = Nothing
    }
  where
    entries = zip [firstLocationInstanceId ..] (overlayToList overlay)
    mk (n, (coord, lid)) =
        newLocationInstance namer (LocationInstanceId n) coord
            <$> lookupLocation lid registry

-- | Add one instance under a freshly allocated id. The engine's own
--   placement pass uses 'buildLocationInstances'; this is the seam a
--   later feature (or a test building two instances in one chunk) adds
--   through without reaching into 'lisNextId' by hand.
allocateLocationInstance
    ∷ Maybe LocationNamer → ChunkCoord → LocationDef → LocationInstances
    → (LocationInstanceId, LocationInstances)
allocateLocationInstance namer coord def lis =
    let iid  = LocationInstanceId (lisNextId lis)
        inst = newLocationInstance namer iid coord def
    in ( iid
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
resolveLegacyLocationInstances
    ∷ LocationRegistry → LocationOverlay → LocationInstances
    → LocationInstances
resolveLegacyLocationInstances registry overlay lis =
    case lisPendingLegacy lis of
        Nothing → lis
        Just flags →
            let base = buildLocationInstances Nothing registry overlay
            in base { lisById = HM.map (applyFlags flags) (lisById base) }
  where
    applyFlags flags inst = inst
        { liLifecycle =
            if HS.member (liChunk inst) (llcDiscovered flags)
                then LifecycleDiscovered
                else LifecycleUnknown
        , liContentsSpawned =
            HS.member (liChunk inst) (llcContentsSpawned flags)
        }

-- * Validation -------------------------------------------------------

-- | Component-local invariants for a decoded table (mirrors the
--   ground-item allocator check @World.Save.Component.Page@ already
--   runs): every id sits at or above 'firstLocationInstanceId' and
--   strictly below the page's allocator, and every map key matches the
--   'liId' of the instance stored under it. Empty ⇒ the table is
--   well-formed. Literal duplicate ids are structurally impossible once
--   decoded into a 'HM.HashMap'.
locationInstanceAllocatorErrors ∷ LocationInstances → [Text]
locationInstanceAllocatorErrors lis =
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
