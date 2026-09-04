{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
module World.Save.Types
    ( SaveData(..)
    , WorldPageSave(..)
    , activeWorldPage
    , SaveMetadata(..)
    , AutosaveRequest(..)
    , SaveHeader(..)
    , saveMagic
    , currentSaveVersion
    , checkWorldCount
    , BuildingSnapshot(..)
    , BuildingInstanceSnapshot(..)
    , toBuildingSnapshot
    , fromBuildingSnapshot
    , toBuildingInstanceSnapshot
    , fromBuildingInstanceSnapshot
    , UnitSnapshot(..)
    , UnitInstanceSnapshot(..)
    , toUnitSnapshot
    , fromUnitSnapshot
    , flattenItemInstances
    , ItemWalkOrder(..)
    , pageItemContainers
    , MissingDefRef(..)
    , renderMissingDefRef
    , missingDefReferences
    , MissingItemDefRef(..)
    , renderMissingItemDefRef
    , missingItemDefReferences
    , MissingSignificantItemRef(..)
    , renderMissingSignificantItemRef
    , missingSignificantItemReferences
    , MissingRecipeRef(..)
    , renderMissingRecipeRef
    , missingRecipeReferences
    , MissingBillOutputItemRef(..)
    , renderMissingBillOutputItemRef
    , missingBillOutputItemReferences
    , MissingConstructDefRef(..)
    , renderMissingConstructDefRef
    , missingConstructDefReferences
    , MissingMaterialRef(..)
    , renderMissingMaterialRef
    , missingMaterialReferences
    , MissingFloraRef(..)
    , renderMissingFloraRef
    , renderUnnamedFloraRef
    , missingFloraReferences
    , nameFloraReferences
    , resolveFloraReferences
    , MissingLocationRef(..)
    , renderMissingLocationRef
    , missingLocationDefReferences
    , resolveLegacyLocations
    , resolveLegacyLocationParams
    , MissingInfectionRef(..)
    , renderMissingInfectionRef
    , missingInfectionReferences
    , ImmunityScrub(..)
    , emptyImmunityScrub
    , renderImmunityScrub
    ) where

import UPrelude
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Text as T
import Structure.Palette (TexPalette)
import Location.Instance
    ( LocationGeometryError, LocationInstance(..), LocationInstanceId(..)
    , LocationSignificantItem(..), instancesToList
    , resolveLegacyLocationInstances )
import Location.Types (LocationRegistry)
import World.Generate.Types (WorldGenParams(..))
import World.Page.Types (WorldPageId(..), WorldIdentity(..))
import World.Page.GeneratedId (GeneratedWorldId)
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Tool.Types (ToolMode(..))
import World.Edit.Types (WorldEdits, WorldEdit(..))
import World.Mine.Types (MineDesignations)
import World.Construct.Attempt (ConstructAttemptId)
import World.Construct.Types
    (ConstructDesignations, ConstructDesignation(..), ConstructTarget(..))
import Craft.Bills (CraftBills(..), CraftBill(..), BillId(..))
import Unit.Transfer.Orders (TransferOrders)
import Building.Knowledge (ContainerKnowledge(..), ContainerRecord(..))
import Power.Types (PowerNodes)
import World.Chop.Types (ChopDesignations, PendingChopDesignations)
import World.Till.Types (TillDesignations)
import World.Plant.Types
    ( PlantDesignations, SavedPlantDesignations, PlantDesignationOf(..) )
import World.Spoil.Types (SpoilPiles, SpoilPile(..))
import World.Material (MaterialId(..), MaterialRegistry, isKnownMaterial)
import World.Plate.Types (TectonicPlate(..))
import World.Flora.Harvest (FloraHarvests, PendingFloraHarvests)
import World.Flora.CropPlot
    ( CropPlots, SavedCropPlots, CropPlotOf(..) )
import World.Flora.Types (FloraCatalog)
import World.Flora.Identity (floraInstanceIdNone)
import World.Flora.Reference
    ( FloraRef(..), renderFloraRef, resolveFloraRef, floraRefForId )
import World.Chunk.Types (ChunkCoord(..))
import Infection.Types (InfectionManager, lookupInfection)
import Item.Ground (GroundItems(..), GroundItem(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Building.Types (BuildingId(..), BuildingInstance(..), BuildingDef(..)
                      , BuildingManager(..), buildingsOnPage, bdSouthTexture)
import Unit.Types (UnitId(..), UnitInstance(..), UnitDef(..), UnitManager(..)
                  , StatModifier(..), Wound(..), Scar(..), unitsOnPage)
import Unit.Direction (Direction(..))
import Unit.Faction (factionFromTag, factionTag, parseFaction)
import Unit.Sim.Types (UnitSimState(..))
import Item.Types (ItemInstance(..))

-- | 4-byte magic prefix of the pre-#759 flat save file. Spells
--   "SYRA" (Synarchy) in little-endian. Detects "this isn't a save
--   file" before we even try to decode the version field.
--
--   Kept exported with no code consumer (#1119): it is the referent
--   "World.Save.Envelope.Types" cites to explain why
--   'World.Save.Envelope.Types.envelopeMagic' repeats the same value
--   rather than importing it, and it documents the header triple
--   alongside 'SaveHeader' — itself retained for
--   @tools\/persistence_inventory_audit.py@'s root-owner record.
saveMagic ∷ Word32
saveMagic = 0x53595241

-- | Schema version of the transitional in-memory 'SaveData' /
--   'WorldPageSave' load bridge — a developer-maintained bookkeeping
--   marker, bumped by hand whenever either record's layout changes. It
--   does not govern on-disk save compatibility: each envelope component
--   carries its own version and migrations for that
--   ("World.Save.Envelope", "docs/persistence_contract.md"). Per-bump
--   history up to v91: "docs/history/savedata_version_changelog.md".
--
--   Kept exported with no code consumer (#1119): @CLAUDE.md@ and
--   @docs\/persistence_contract.md@ both instruct maintainers to bump
--   it, so it is a documented maintainer-facing marker, not dead code.
currentSaveVersion ∷ Int
currentSaveVersion = 99

-- | The shape of the tagged save envelope's fixed 16-byte header
--   (issue #759, save-overhaul B1): magic, the envelope FRAMING
--   version (separate from any carried component's own schema
--   version — see 'World.Save.Envelope.currentEnvelopeVersion'), and
--   the manifest's length in bytes. Retained here — rather than in
--   "World.Save.Envelope.Types" — purely so the persistence-inventory
--   audit (`tools/persistence_inventory_audit.py`) keeps a stable
--   root-owner record to classify; the real codec
--   ("World.Save.Envelope.Codec") manipulates these three values as
--   raw scalars under its own explicit byte-layout control, not this
--   record, and never constructs one. A pre-#759 flat file (v89 and
--   earlier) has no manifest at all — its first 16 bytes decode as a
--   header whose 'shEnvelopeVersion' can never coincide with a real
--   envelope version, so it is rejected the same way any other
--   version mismatch is, with no heuristic positional decoding.
data SaveHeader = SaveHeader
    { shMagic           ∷ !Word32
    , shEnvelopeVersion ∷ !Word32
    , shManifestLength  ∷ !Word64
    } deriving (Show, Eq, Generic)

-- | Human-readable metadata for save listing
data SaveMetadata = SaveMetadata
    { smName       ∷ !Text
        -- ^ The save-slot/file identity (validated by sanitizeSaveName)
        --   — NOT the world's player-facing name, see smWorldName.
    , smSeed       ∷ !Word64
    , smWorldSize  ∷ !Int
    , smPlateCount ∷ !Int
    , smTimestamp  ∷ !Text        -- ^ ISO 8601 string
    , smWorldName  ∷ !(Maybe Text)
        -- ^ v82 (#707): the ACTIVE page's player-facing display name at
        --   save time (its 'wpsIdentity'), so save listings can show it
        --   without decoding sdWorlds. Nothing for an unnamed world.
    , smWorldGloss ∷ !(Maybe Text)
        -- ^ v82 (#707): that identity's optional English gloss. Always
        --   Nothing when smWorldName is Nothing (a gloss cannot exist
        --   without a display name).
    , smAutosave   ∷ !Bool
        -- ^ #913: durable autosave\/manual CLASSIFICATION — 'True' only
        --   for a generation the interval autosave scheduler itself
        --   requested. Slot OWNERSHIP is decided by this flag, never by
        --   the @autosave-\<n\>@ name convention, which a player is free
        --   to type into the manual save box; that is what lets the
        --   rotation refuse to overwrite a manual save sitting on one of
        --   its own names. Carried in @"metadata"@ so
        --   'World.Save.Serialize.listSaves' can report it without
        --   decoding any gameplay component. Added by @"metadata"@
        --   component v2; every v1 payload migrates with this 'False'
        --   ("legacy saves are manual saves") — see
        --   "World.Save.Compat.MetadataV1".
    , smGeneratedWorldIds ∷ ![GeneratedWorldId]
        -- ^ #2021: every page's opaque 'GeneratedWorldId', as a
        --   duplicate-free collection written in ascending canonical
        --   order — EVERY page in the save, not just the active one, so
        --   the list is the complete inventory of generated foundations
        --   this save references.
        --
        --   Carried here for the same reason 'smWorldName' is (#707):
        --   so a read can obtain it WITHOUT decoding a gameplay
        --   component. 'World.Save.Serialize.listSaves' decodes only
        --   @"metadata"@ by documented design, and later
        --   reference-aware library cleanup has to decide which
        --   generated worlds are still referenced by some save — paying
        --   a full per-slot component decode for that would also mean a
        --   save with a broken @world-pages@ could never be PROVEN to
        --   reference anything, and so could never be safely cleaned
        --   up.
        --
        --   @world-pages@ stays AUTHORITATIVE; this is a copy. Both are
        --   written from one value
        --   ('World.Save.Snapshot.pgsGeneratedId', through
        --   'World.Save.Snapshot.Adapter.snapshotSaveMetadata'), and a
        --   save whose two copies disagree is rejected for full load by
        --   'World.Save.Component.metadataErrors' rather than silently
        --   preferring either.
        --
        --   Added by @"metadata"@ component v3. Every v1 and v2 payload
        --   migrates with this EMPTY — those formats predate generated
        --   world identity, and their pages get fresh ids at load
        --   staging — see "World.Save.Compat.MetadataV1" and
        --   "World.Save.Compat.MetadataV2".
    } deriving (Show, Eq, Serialize, Generic)

-- | #913: the extra request state an AUTOSAVE carries that a manual
--   save does not. Its presence on a @WorldSave@ command is what makes
--   that save an autosave at all: it both sets the durable
--   'smAutosave' classification and authorizes the one behaviour a
--   manual save never gets — restoring the player's pre-request pause
--   state and visible-world time scale once the transaction SUCCEEDS.
--
--   Every field is captured by 'Engine.Scripting.Lua.API.Save.saveWorldFn'
--   at request ACCEPTANCE, before the save path's own auto-pause has run,
--   so they describe the world the player was actually looking at rather
--   than the frozen one the save just produced.
data AutosaveRequest = AutosaveRequest
    { arPrePaused    ∷ !Bool
        -- ^ Whether the player was ALREADY paused when the request was
        --   accepted. If so the save leaves the session paused and
        --   zero-scaled: there is nothing to restore, and resuming a
        --   deliberately-paused game would be the autosave changing
        --   gameplay.
    , arPreTimeScale ∷ !Float
        -- ^ The VISIBLE page's exact time scale at acceptance, restored
        --   verbatim on success — fast-forward values included. 0 when
        --   no page is visible (nothing to restore).
    , arPausedPage   ∷ !(Maybe WorldPageId)
        -- ^ #1599: the page 'arPreTimeScale' was read from, i.e. the one
        --   whose clock this save's pause epoch ("World.Pause") took hold
        --   of. Restoring by page IDENTITY rather than by \"whichever page
        --   is visible when the transaction finishes\" is what keeps the
        --   restore off a bystander: the visible page can change while an
        --   autosave runs, and writing this speed onto whatever is
        --   on-screen by then would retime a page the save never paused.
        --   'Nothing' when no page was visible at acceptance — the same
        --   case that makes 'arPreTimeScale' 0.
    , arIntentGen    ∷ !Word64
        -- ^ 'Engine.Core.State.playerIntentGenRef' at acceptance. On
        --   success the restore happens only if the live generation
        --   still matches: any player pause/resume or time-scale
        --   request during the window wins outright, even when the
        --   final boolean happens to equal the pre-save one.
    , arEnginePauseGen ∷ !Word64
        -- ^ #1730: 'Engine.Core.State.enginePauseGenRef' at acceptance,
        --   read under the same lock as 'arIntentGen' and serving the
        --   same purpose for the pause sources the player does not
        --   drive. A @pause: true@ notification landing during the
        --   window is a complete no-op on an already-paused session, so
        --   this counter is the ONLY evidence that someone other than
        --   this save still wants the game paused; a restore that finds
        --   it moved leaves the pause alone.
        --
        --   The save's own re-assertion on the world thread
        --   ('World.Pause.reassertSavePause') deliberately does not move
        --   it — otherwise every real autosave would decline.
    } deriving (Show, Eq)

-- | Per-world-page save payload. Everything scoped to a single world
--   page — terrain gen params, camera, clock, edits, and that page's
--   buildings/units/sim-states — lives here. A 'SaveData' carries a list
--   of these ('sdWorlds') plus the genuinely global fields.
--
--   Splitting per-world state into its own record is what lets every
--   live world page be persisted in one save (epic #214): the save
--   command snapshots every page in 'wmWorlds' into one 'WorldPageSave'
--   each. Like 'SaveData', this is encoded positionally by cereal's
--   Generic instance, so any layout change bumps 'currentSaveVersion'.
data WorldPageSave = WorldPageSave
    { wpsPageId       ∷ !WorldPageId
        -- ^ The id this page had at save time. On load the active page
        --   restores under @main_world@ (see 'sdActivePage'); additional
        --   pages restore under this id.
    , wpsGenParams    ∷ !WorldGenParams
    , wpsCameraX      ∷ !Float
    , wpsCameraY      ∷ !Float
    , wpsCameraZoom   ∷ !Float
    , wpsCameraFacing ∷ !CameraFacing
    , wpsTimeHour     ∷ !Int
    , wpsTimeMinute   ∷ !Int
    , wpsDateYear     ∷ !Int
    , wpsDateMonth    ∷ !Int
    , wpsDateDay      ∷ !Int
    , wpsTimeScale    ∷ !Float
    , wpsMapMode      ∷ !ZoomMapMode
    , wpsToolMode     ∷ !ToolMode
    , wpsEdits        ∷ !WorldEdits
        -- ^ Per-chunk edit log. Restored before chunk regeneration on
        --   load; chunks then replay their edits to recover the player's
        --   modifications. Edits survive eviction during a play session.
    , wpsMineDesignations ∷ !MineDesignations
        -- ^ Mine designations incl. mid-dig corner progress. Restored
        --   straight into wsMineDesignationsRef; markers re-render from
        --   the stored z, so no chunk loading is required first.
    , wpsConstructDesignations ∷ !ConstructDesignations
    , wpsConstructNextAttempt ∷ !ConstructAttemptId
      -- ^ #1844: this page's construction ATTEMPT allocator.
        -- ^ Construction designations (#95): build target + status +
        --   progress per tile.
        --
        --   NEITHER ghost re-renders from the stored z the way a mine
        --   designation does. A BUILDING ghost reads the anchor's live
        --   terrain for the z its stake will land on (#1845), falling
        --   back to the stored value only where that terrain is not
        --   resident — so a restored designation over an unpublished
        --   chunk draws at its saved level and snaps to the real one
        --   when the chunk arrives, rather than being absent.
        --
        --   A STRUCTURE ghost is stricter (#1846): it
        --   draws the piece's own art at the final grid z
        --   'World.Construct.Plan' resolves, and that resolution reads
        --   live terrain. This is NOT a restoration requirement — the
        --   record restores exactly as it always did, and no load waits
        --   on a chunk — but such a ghost is simply ABSENT until its
        --   chunk publishes, which is the resolver's
        --   @unresolved-terrain@ outcome and is deliberately
        --   indistinguishable from any other frame in which the terrain
        --   is not yet there.
    , wpsGroundItems  ∷ !GroundItems
        -- ^ Items lying in the world. Full ItemInstances + float
        --   positions; resting height derives from terrain at render,
        --   so restoration needs no chunk loading either.
    , wpsSpoilPiles   ∷ !SpoilPiles
        -- ^ Spoil mounds (vertex-keyed partial fills — see
        --   World.Spoil.Types). Fills are relative to each tile's terrain
        --   surface; promoted cells live in wpsEdits as WeAddTile, so
        --   restoration is order-independent.
    , wpsBuildings    ∷ !BuildingSnapshot
        -- ^ This page's placed buildings. Restored AFTER edits +
        --   center-chunk regen so buildings landing on player-edited
        --   terrain end up at the right z (their saved biGridZ already
        --   reflects the post-edit terrain at place time, but the chunk
        --   needs to have replayed its edits first for downstream queries
        --   to agree).
    , wpsUnits        ∷ !UnitSnapshot
        -- ^ This page's live unit instances + their stats/skills/
        --   modifiers/inventory. Restored alongside the sim states below.
    , wpsUnitSimStates ∷ !(HM.HashMap UnitId UnitSimState)
        -- ^ Per-unit sim state (position, pose, activity, target, path,
        --   *Until timers) for this page's units. Restored into utsRef on
        --   EngineEnv.
    , wpsFloraHarvests ∷ !FloraHarvests
        -- ^ Harvested flora (#94): flora INSTANCE id → regrowth
        --   game-seconds remaining. #1854 re-keyed this off the tile so
        --   one plant's timer stops depleting its co-tenants. Like
        --   designations, restored straight into wsFloraHarvestsRef —
        --   render and queries need no chunk loading first.
    , wpsChopDesignations ∷ !ChopDesignations
        -- ^ Chop designations (#97): flora INSTANCE id → surface z plus
        --   the plant's canonical tile (#1854). Like the other
        --   designation layers, restored straight into
        --   wsChopDesignationsRef. #1856: the MARKER no longer reads
        --   that z — it anchors to the live flora instance's own
        --   projected ground contact, because a stored z drifts the
        --   annotation off the sprite as soon as the column changes.
        --   The z survives as the value the @chop.*@ query verbs report
        --   to the AI.
    , wpsPendingChopMigration ∷ !PendingChopDesignations
        -- ^ #1854: pre-identity tile-keyed chop designations that could
        --   not be resolved to an instance at load time because their
        --   chunk was not resident. Persisted (rather than dropped) so
        --   a second save/load cannot silently discard a designation the
        --   player made; drained by "World.Flora.Designation" as each
        --   chunk arrives, and never consulted by a runtime query.
    , wpsPendingFloraHarvests ∷ !PendingFloraHarvests
        -- ^ #1854: pre-identity tile-keyed regrowth timers, on the same
        --   deferred-never-authoritative terms.
    , wpsPlantedFloraCursor ∷ !Word64
        -- ^ #1854: this page's planted-flora id allocator cursor. Kept
        --   strictly above every planted FloraInstanceId the page's
        --   edit log carries, so planting after a load can never reissue
        --   a live id. Persisted in @world-edits@ v2 beside those edits.
    , wpsCraftBills ∷ !CraftBills
        -- ^ Craft-bill queue (#329): standing per-station craft orders
        --   incl. claim + cycle progress. Station references are
        --   BuildingIds, which restore verbatim (see wpsBuildings), so
        --   bills reconnect to their stations; restore keeps a bill
        --   whose station is orphaned intact rather than pruning it
        --   (issue #763 — "World.Load.Stage" restores bills
        --   VERBATIM, never against the loaded page's own building
        --   snapshot; a demolished station's lingering bill is tolerated
        --   gameplay, not corruption). A station that resolves on a
        --   DIFFERENT page than its bill is a genuine wrong-page
        --   violation instead, hard-rejected by
        --   "World.Save.Integrity" (issue #764). Appended for save v72.
    , wpsPowerNodes ∷ !PowerNodes
        -- ^ Power-node registry (#358): placed solar-panel/battery
        --   source + storage nodes (role + parameters), plus each
        --   storage node's current charge (pnStoredWh, #360). Like
        --   craft bills, references a BuildingId that restores
        --   verbatim (see wpsBuildings); an orphaned host building is
        --   tolerated the same way (issue #763 — no pruning),
        --   a wrong-page one hard-rejected (issue #764). Appended for
        --   save v73, pnStoredWh for v75.
    , wpsTillDesignations ∷ !TillDesignations
        -- ^ Till designations (#333): tile → surface z. Like the other
        --   designation layers, restored straight into
        --   wsTillDesignationsRef; markers re-render from the stored z.
        --   Appended for save v76.
    , wpsCropPlots ∷ !SavedCropPlots
        -- ^ Planted groundcover-crop tiles (#334): tile → (species,
        --   planted day, health). Restored into wsCropPlotsRef by
        --   'World.Load.Stage.stagePage', which resolves each species
        --   reference against the loading build's flora catalog;
        --   render/harvest then derive growth state from it with no
        --   chunk loading needed. Appended for save v77.
        --
        --   #2243: the species is a durable
        --   'World.Flora.Reference.FloraRef' — an authored name, or the
        --   ordinal a pre-name payload carried — not the runtime handle
        --   this field held through save v98. Same for
        --   'wpsPlantDesignations' below and for the planting entries in
        --   'wpsEdits'.
    , wpsPlantDesignations ∷ !SavedPlantDesignations
        -- ^ Plant designations (#335): tile → (surface z, chosen crop).
        --   Like the other designation layers, restored straight into
        --   wsPlantDesignationsRef; markers re-render from the stored
        --   z. Appended for save v78.
        --
        --   #1858 adds the one qualification: the restore is still
        --   verbatim, but tilled soil is a CONTINUOUS requirement, so a
        --   restored record is RECONCILED against the terrain this load
        --   reconstructs ('World.Plant.Validate', called from
        --   "World.Load.Stage" and again as each chunk publishes). One
        --   whose ground came back untilled is removed rather than
        --   re-rendered over soil it can no longer be planted in; one
        --   whose chunk is not resident yet is UNKNOWN, and is retained
        --   until it can be resolved.
    , wpsContainerKnowledge ∷ !ContainerKnowledge
        -- ^ The player's remembered view of each container's contents
        --   on this page (#1087, epic #1013): last-known items + their
        --   derived weight + when observed, keyed by BuildingId.
        --   Restored straight into wsContainerKnowledgeRef, after
        --   scrubbing any record whose building is absent from
        --   wpsBuildings — a demolished cargo's lingering memory is a
        --   tolerated, non-blocking diagnostic (the same contract
        --   wpsCraftBills' dangling station gets), never a load
        --   failure. Deliberately NOT restored verbatim like bills: a
        --   bill is a live job the player can still see and cancel,
        --   whereas a memory of a container that no longer exists has
        --   no surface at all and nothing would ever clear it.
        --   Capacity is never stored — it is always read live from the
        --   def. Appended for save v92.
    , wpsTransferOrders ∷ !TransferOrders
        -- ^ Durable transfer orders (#1246, epic #1013): this page's
        --   queue of standing "move these exact item instances from
        --   this endpoint to that one" orders, each carrying its acting
        --   unit and every requested item's own lifecycle state, plus
        --   the page-local id allocator. Restored VERBATIM, exactly as
        --   wpsCraftBills/wpsPowerNodes are (issue #763): an order whose
        --   carrier died or whose destination was demolished is a
        --   tolerated, non-blocking diagnostic — logged at staging by
        --   "World.Load.Stage" and never pruned — while a reference
        --   resolving on a DIFFERENT page is the hard error
        --   "World.Save.Integrity" raises. Appended for save v93.
    , wpsIdentity ∷ !(Maybe WorldIdentity)
        -- ^ Player-facing identity (#707): display name + optional
        --   gloss. Lives HERE — on the page's saved state — rather than
        --   deriving from any id, because load remaps ids (the active
        --   page → main_world, collisions → "<id>#N") while the
        --   identity must follow the page itself. Restored straight
        --   into wsIdentityRef. Appended for save v82.
    , wpsGeneratedId ∷ !(Maybe GeneratedWorldId)
        -- ^ #2021: this page's opaque generated-world identity, carried
        --   across the in-memory load bridge so "World.Load.Stage" can
        --   restore it onto the staged page's 'wsGeneratedIdRef'.
        --
        --   'Nothing' means the save predates the id entirely (a
        --   @world-pages@ payload older than v9). Staging then keeps the
        --   fresh id the staged 'World.State.Types.WorldState' minted
        --   for itself, and — because loading never rewrites its source
        --   — the file it came from still says nothing afterwards.
    } deriving (Show, Serialize, Generic)

-- | Everything needed to reconstruct the saved game. Per-world state is
--   carried in 'sdWorlds' (one 'WorldPageSave' per saved page — every
--   live page, not just the active one); the remaining fields are
--   genuinely global, shared by every page or describing the save as
--   a whole.
--
--   Schema is versioned via the file header — see saveMagic /
--   currentSaveVersion / SaveHeader above. Bump currentSaveVersion
--   whenever this record's layout (or 'WorldPageSave''s) changes
--   (cereal's Generic encoding is positional, so reordering or inserting
--   a field is breaking).
data SaveData = SaveData
    { sdMetadata   ∷ !SaveMetadata
        -- ^ Save-listing metadata (name/seed/size/plates/timestamp),
        --   describing the primary (active) world.
    -- Global fields (one per save, shared across all pages):
    , sdGameTime     ∷ !Double   -- ^ gameTimeRef value (game-clock seconds).
    , sdEnginePaused ∷ !Bool     -- ^ enginePausedRef value. Auto-pause-on-save
                                  --   means this is always True for v2+ saves.
        -- Lua-owned state (issue #761, save-overhaul B3): no longer a
        -- field here at all. Each registered Lua module is its own
        -- dynamically-added envelope component (@"lua.<module>"@),
        -- prepared and applied directly by
        -- "Engine.Scripting.Lua.API.Save" — decode/validate BEFORE the
        -- engine-side restore is queued, apply still ahead of it, same
        -- ordering as before, just no longer routed through this record.
    , sdTexPalette ∷ !TexPalette
        -- ^ Texture path↔id palette. Structure edits in each page's edits
        --   store palette ids; this resolves them to paths → runtime
        --   handles on load. Stable ids → no per-object remap.
    , sdNextItemInstanceId ∷ !Word64
        -- ^ Snapshot of 'nextItemInstanceIdRef' at save time. ASSIGNED
        --   straight onto the allocator on load (#763: a load replaces the
        --   session, so the discarded value is not max'd in). Post-load
        --   item creation still continues above every saved
        --   'iiInstanceId', because 'World.Save.Snapshot' refuses to
        --   capture a session whose ids reach this value.
    -- Multi-page fields (#215 / epic #214):
    , sdActivePage   ∷ !WorldPageId
        -- ^ The primary/active page at save time. Restores under id
        --   @main_world@ (the documented convention that existing Lua /
        --   headless code assumes) regardless of its original id.
    , sdVisiblePages ∷ ![WorldPageId]
        -- ^ Pages that were visible (wmVisible) at save time, so the
        --   loaded game comes up showing what the player last saw.
    , sdWorlds       ∷ ![WorldPageSave]
        -- ^ Every saved world page, one entry per live page in
        --   wmWorlds at save time.
    } deriving (Show, Serialize, Generic)

-- | The primary/active world page in a save — the one that restores as
--   @main_world@. Falls back to the first page if 'sdActivePage' names no
--   page in 'sdWorlds' (defensive; every real save records a valid active
--   id). 'Nothing' only for a malformed empty-world save.
activeWorldPage ∷ SaveData → Maybe WorldPageSave
activeWorldPage sd =
    case filter ((≡ sdActivePage sd) . wpsPageId) (sdWorlds sd) of
        (w:_) → Just w
        []    → case sdWorlds sd of
                  (w:_) → Just w
                  []    → Nothing

-- | Reject a decoded save with NO world pages (corrupt / truncated). Any
--   non-empty 'sdWorlds' is accepted: the save command snapshots every live
--   page (#216) and the load handler restores all of them (#217/#218), so a
--   multi-page save is fully supported. Only the empty case is rejected, so
--   the rest of the loader can assume at least one page.
--
--   Applied by 'World.Save.Serialize.loadWorld' AND, for issue #762's
--   storage-transaction candidate re-read (requirement 3), by
--   'World.Save.Storage' before either a fresh publish or a
--   previous-generation load fallback trusts a decoded generation. NOT
--   applied by 'World.Save.Serialize.listSaves': listing decodes just the
--   \"metadata\" component (issue #759 requirement 4), which never carries
--   'sdWorlds' at all.
checkWorldCount ∷ SaveData → Either Text SaveData
checkWorldCount sd = case sdWorlds sd of
    [] → Left "Save contains no world pages (corrupt or truncated file)"
    _  → Right sd

-- | Persistable snapshot of `BuildingManager`. Drops `bmDefs`
--   (regenerated from YAML at boot) and `bmSelected` (transient UI
--   state, reset on load).
data BuildingSnapshot = BuildingSnapshot
    { bsnInstances ∷ !(HM.HashMap BuildingId BuildingInstanceSnapshot)
    , bsnNextId    ∷ !Word32
    } deriving (Show, Eq, Serialize, Generic)

-- | Persistable snapshot of `BuildingInstance`. Drops `biTexture`
--   (runtime asset handle, meaningless across sessions — re-resolved
--   from the def at load time via `biDefName`).
data BuildingInstanceSnapshot = BuildingInstanceSnapshot
    { bisDefName    ∷ !Text
    , bisAnchorX    ∷ !Int
    , bisAnchorY    ∷ !Int
    , bisGridZ      ∷ !Int
    , bisSpawnedAt  ∷ !Double
    , bisTileW      ∷ !Int
    , bisTileH      ∷ !Int
    , bisSpawnRemaining ∷ !Int
    , bisBuildProgress ∷ !Float
    , bisMaterialsDelivered ∷ !(HM.HashMap Text [ItemInstance])
    , bisStorage           ∷ ![ItemInstance]
      -- ^ An item container added here must also be listed in
      --   'buildingItemContainers' — the record definition and the DTO
      --   codec fail loudly if missed, but the id/def-name validators
      --   walk containers by enumeration and would silently stop
      --   seeing it (#1090).
    } deriving (Show, Eq, Serialize, Generic)

-- | Build a snapshot from a live BuildingManager, restricted to the world
--   being saved (@page@). The manager is global across worlds, so an
--   unfiltered snapshot would serialize other worlds' buildings and stamp
--   them onto the load target (#76). Strips `bmDefs` (regenerated from
--   YAML on next boot) and `bmSelected` (resets to Nothing).
toBuildingSnapshot ∷ WorldPageId → BuildingManager → BuildingSnapshot
toBuildingSnapshot page bm = BuildingSnapshot
    { bsnInstances = HM.map toBuildingInstanceSnapshot
                            (buildingsOnPage page (bmInstances bm))
    , bsnNextId    = bmNextId bm
    }

toBuildingInstanceSnapshot ∷ BuildingInstance → BuildingInstanceSnapshot
toBuildingInstanceSnapshot bi = BuildingInstanceSnapshot
    { bisDefName       = biDefName bi
    , bisAnchorX       = biAnchorX bi
    , bisAnchorY       = biAnchorY bi
    , bisGridZ         = biGridZ bi
    , bisSpawnedAt     = biSpawnedAt bi
    , bisTileW         = biTileW bi
    , bisTileH         = biTileH bi
    , bisSpawnRemaining = biSpawnRemaining bi
    , bisBuildProgress = biBuildProgress bi
    , bisMaterialsDelivered = biMaterialsDelivered bi
    , bisStorage           = biStorage bi
    }

-- | Restore a BuildingManager from a snapshot. `defs` come from the
--   already-loaded BuildingManager (registered from YAML at boot);
--   we use them to re-resolve each instance's `biTexture` from its
--   `bisDefName`. Instances whose def is no longer registered (e.g.
--   the player removed the YAML between sessions) are dropped with a
--   warning written to the IO log by the caller.
--
--   Returns (manager, [orphan BuildingId]) so the caller can log
--   the dropped entries.
--   @page@ is the world the buildings load into (the load target); every
--   restored building is stamped with it so the runtime world scoping
--   holds after a load (#76).
fromBuildingSnapshot ∷ WorldPageId → HM.HashMap Text BuildingDef
                     → BuildingSnapshot
                     → (BuildingManager, [BuildingId])
fromBuildingSnapshot page defs snap =
    let pairs = HM.toList (bsnInstances snap)
        resolved = [ (bid, fromBuildingInstanceSnapshot page d snap')
                   | (bid, snap') ← pairs
                   , Just d ← [HM.lookup (bisDefName snap') defs]
                   ]
        orphans  = [ bid
                   | (bid, snap') ← pairs
                   , not (HM.member (bisDefName snap') defs)
                   ]
        bm = BuildingManager
                { bmDefs      = defs
                , bmInstances = HM.fromList resolved
                , bmNextId    = bsnNextId snap
                , bmSelected  = Nothing
                  -- Destruction presentations are session-transient
                  -- (#2091): never in the snapshot, never restored.
                , bmDestructions = HM.empty
                }
    in (bm, orphans)

fromBuildingInstanceSnapshot ∷ WorldPageId → BuildingDef
                             → BuildingInstanceSnapshot → BuildingInstance
fromBuildingInstanceSnapshot page def s = BuildingInstance
    { biDefName        = bisDefName s
    , biPage           = page             -- runtime world scoping (#76)
    , biTexture        = bdSouthTexture def  -- re-resolved
    , biAnchorX        = bisAnchorX s
    , biAnchorY        = bisAnchorY s
    , biGridZ          = bisGridZ s
    , biSpawnedAt      = bisSpawnedAt s
    , biTileW          = bisTileW s
    , biTileH          = bisTileH s
    , biSpawnRemaining = bisSpawnRemaining s
    , biBuildProgress  = bisBuildProgress s
    , biMaterialsDelivered = bisMaterialsDelivered s
    , biStorage            = bisStorage s
    }

-- Unit snapshots ---------------------------------------------------

-- | Persistable snapshot of `UnitManager`. Drops `umDefs` (regenerated
--   from YAML at boot) and `umSelected` (transient UI state).
data UnitSnapshot = UnitSnapshot
    { usnInstances ∷ !(HM.HashMap UnitId UnitInstanceSnapshot)
    , usnNextId    ∷ !Word32
    } deriving (Show, Eq, Serialize, Generic)

-- | Persistable snapshot of `UnitInstance`. Drops `uiTexture` +
--   `uiDirSprites` (runtime asset handles, re-resolved from the def at
--   load via `uisDefName`). Everything else round-trips faithfully —
--   stats, modifiers, skills, inventory, the current animation frame
--   the unit was on, and pose/activity strings.
data UnitInstanceSnapshot = UnitInstanceSnapshot
    { uisDefName     ∷ !Text
    , uisBaseWidth   ∷ !Float
    , uisGridX       ∷ !Float
    , uisGridY       ∷ !Float
    , uisGridZ       ∷ !Int
    , uisFacing      ∷ !Direction
    , uisCurrentAnim ∷ !Text
    , uisAnimStart   ∷ !Double
    , uisAnimReverse ∷ !Bool
    , uisActivity    ∷ !Text
    , uisPose        ∷ !Text
    , uisAnimStride  ∷ !Int
    , uisStats       ∷ !(HM.HashMap Text Float)
    , uisModifiers   ∷ !(HM.HashMap Text [StatModifier])
    , uisSkills      ∷ !(HM.HashMap Text Float)
    , uisKnowledge   ∷ !(HM.HashMap Text Float)
    , uisInventory   ∷ ![ItemInstance]
    , uisEquipped    ∷ !(HM.HashMap Text ItemInstance)
      -- ^ v8: slot id → equipped item. Empty map is legal (no gear).
      --   Serialize roundtrip is positional, not name-keyed, so any
      --   future addition must go after this field; bump again if so.
    , uisAccessories ∷ ![ItemInstance]
      -- ^ v10: items worn off the silhouette (robes, goggles, rings…).
      --   Order preserved. An item container added to this record must
      --   also be listed in 'unitItemContainers' — see
      --   'bisStorage''s note (#1090).
    , uisFactionId   ∷ !Text
      -- ^ v16: spawn-time-only faction tag (no def-level default).
      --   Deliberately stays 'Text' on the wire even though the runtime
      --   field is a typed 'Unit.Faction.Faction' (#912): a
      --   'Generic'-derived @Serialize@ enum is positional by
      --   constructor tag, so serializing the type would make its
      --   constructor ORDER load-bearing forever for no gain here.
      --   Rendered by 'Unit.Faction.factionTag' and parsed by
      --   'Unit.Faction.factionFromTag' at the two adapters below, so
      --   this field's format — and the units component's schema
      --   version — are unchanged.
    , uisWounds      ∷ ![Wound]
      -- ^ v16: per-unit wound list. Roundtrips faithfully. Generic
      --   Serialize over the Wound record below; fields are
      --   positional, so appending a field to Wound also bumps v.
    , uisScars       ∷ ![Scar]
      -- ^ v45: permanent scar records left by healed severe wounds.
    , uisImmuneResponse ∷ !Float
      -- ^ v50: systemic immune-response level (the active infection fight).
    , uisImmunities   ∷ !(HM.HashMap Text Float)
      -- ^ v50: acquired per-type immunity (decays very slowly).
    , uisBlood       ∷ !Float
      -- ^ v16: current blood volume in litres. Spawn-time seeded
      --   from body_mass; ticked down by Combat.Wounds bleeding.
    , uisName        ∷ !Text
      -- ^ v57: persistent per-unit display name (#264). "" for unnamed
      --   units. Appended last — Serialize roundtrip is positional.
    } deriving (Show, Eq, Serialize, Generic)

-- | Build a snapshot from a live UnitManager, restricted to the world
--   being saved (@page@). The manager is global across worlds, so an
--   unfiltered snapshot would serialize other worlds' units and stamp
--   them onto the load target (#78).
toUnitSnapshot ∷ WorldPageId → UnitManager → UnitSnapshot
toUnitSnapshot page um = UnitSnapshot
    { usnInstances = HM.map toUnitInstanceSnapshot
                            (unitsOnPage page (umInstances um))
    , usnNextId    = umNextId um
    }

toUnitInstanceSnapshot ∷ UnitInstance → UnitInstanceSnapshot
toUnitInstanceSnapshot ui = UnitInstanceSnapshot
    { uisDefName     = uiDefName ui
    , uisBaseWidth   = uiBaseWidth ui
    , uisGridX       = uiGridX ui
    , uisGridY       = uiGridY ui
    , uisGridZ       = uiGridZ ui
    , uisFacing      = uiFacing ui
    , uisCurrentAnim = uiCurrentAnim ui
    , uisAnimStart   = uiAnimStart ui
    , uisAnimReverse = uiAnimReverse ui
    , uisActivity    = uiActivity ui
    , uisPose        = uiPose ui
    , uisAnimStride  = uiAnimStride ui
    , uisStats       = uiStats ui
    , uisModifiers   = uiModifiers ui
    , uisSkills      = uiSkills ui
    , uisKnowledge   = uiKnowledge ui
    , uisInventory   = uiInventory ui
    , uisEquipped    = uiEquipment ui
    , uisAccessories = uiAccessories ui
    , uisFactionId   = factionTag (uiFactionId ui)   -- typed → wire (#912)
    , uisWounds      = uiWounds ui
    , uisScars       = uiScars ui
    , uisImmuneResponse = uiImmuneResponse ui
    , uisImmunities  = uiImmunities ui
    , uisBlood       = uiBlood ui
    , uisName        = uiName ui
    }

-- | Restore a UnitManager from a snapshot. Like buildings: instances
--   whose def is no longer registered get dropped with the orphan
--   list returned for caller logging. `umSelected` resets to empty.
--   @page@ is the world the units are loaded into (always the load
--   target, "main_world"); every restored unit is stamped with it so the
--   runtime-only world scoping holds after a load (#78).
--
--   The third component is every DISTINCT unrecognized faction tag seen
--   in this snapshot, sorted (#912). Those units load as
--   'Unit.Faction.fallbackFaction' — a bad tag never fails a load — and
--   the list exists so the caller can warn once per distinct tag instead
--   of once per unit, however many units share it.
--
--   The fourth is the 'ImmunityScrub' diagnostic for the acquired-immunity
--   entries this restore dropped against @infMgr@ (#2305). The scrub lives
--   HERE, inside the one pure restore every load goes through, so no
--   caller can publish a unit carrying immunity to an infection that no
--   longer exists; @infMgr@ is one immutable catalogue view for the whole
--   staging attempt, never re-read per page, so a single load cannot
--   reconcile two of its pages against different definition sets.
fromUnitSnapshot ∷ WorldPageId → HM.HashMap Text UnitDef → InfectionManager
                 → UnitSnapshot
                 → (UnitManager, [UnitId], [Text], ImmunityScrub)
fromUnitSnapshot page defs infMgr snap =
    let pairs = HM.toList (usnInstances snap)
        -- Everything below is derived from the units that actually made
        -- it in: an orphan's whole instance is discarded, so neither its
        -- faction tag nor its immunity map is a live unit's problem.
        kept     = [ (uid, d, s)
                   | (uid, s) ← pairs
                   , Just d ← [HM.lookup (uisDefName s) defs]
                   ]
        resolved = [ (uid, fromUnitInstanceSnapshot page d (scrubbed s))
                   | (uid, d, s) ← kept
                   ]
        orphans  = [ uid
                   | (uid, s) ← pairs
                   , not (HM.member (uisDefName s) defs)
                   ]
        unknownFactions = L.sort $ HS.toList $ HS.fromList
                   [ uisFactionId s
                   | (_, _, s) ← kept
                   , isNothing (parseFaction (uisFactionId s))
                   ]
        -- #2305: acquired immunity is keyed by infection definition id
        -- and restored verbatim before this. An entry whose def has been
        -- removed from the catalogue has no surface but the raw key
        -- 'Engine.Scripting.Lua.API.Units.Combat.unitGetImmunitiesFn'
        -- would print in the Status tab, and 'Combat.Wounds.Tick' would
        -- resume honouring it for a DIFFERENT infection if the id were
        -- ever reintroduced. It is dropped here on #1087's terms —
        -- diagnosed, never a load failure — rather than added to
        -- 'missingInfectionReferences': see that function's haddock for
        -- why the two halves of an infection reference part company.
        resolves k = isJust (lookupInfection k infMgr)
        scrubbed s = s { uisImmunities =
                           HM.filterWithKey (\k _ → resolves k)
                                            (uisImmunities s) }
        dropped  = [ k
                   | (_, _, s) ← kept
                   , k ← HM.keys (uisImmunities s)
                   , not (resolves k)
                   ]
        immScrub = ImmunityScrub
                     { iscRemoved = length dropped
                     , iscIds     = L.sort (HS.toList (HS.fromList dropped))
                     }
        um = UnitManager
                { umDefs      = defs
                , umInstances = HM.fromList resolved
                , umSelected  = mempty
                , umNextId    = usnNextId snap
                }
    in (um, orphans, unknownFactions, immScrub)

fromUnitInstanceSnapshot ∷ WorldPageId → UnitDef → UnitInstanceSnapshot
                         → UnitInstance
fromUnitInstanceSnapshot page def s = UnitInstance
    { uiDefName     = uisDefName s
    , uiName        = uisName s
    , uiPage        = page                -- runtime world scoping (#78)
    , uiTexture     = udTexture def       -- re-resolved
    , uiDirSprites  = udDirSprites def    -- re-resolved
    , uiBaseWidth   = uisBaseWidth s
    , uiGridX       = uisGridX s
    , uiGridY       = uisGridY s
    , uiGridZ       = uisGridZ s
    -- uiRealZ is render-only; restore it from the integer Z so loaded
    -- units stand at the right visual height. Active climbs at save
    -- time would lose their interpolation progress, but combat/climb
    -- state is transient by design.
    , uiRealZ       = fromIntegral (uisGridZ s)
    , uiFacing      = uisFacing s
    , uiCurrentAnim = uisCurrentAnim s
    , uiAnimStart   = uisAnimStart s
    , uiAnimReverse = uisAnimReverse s
    , uiActivity    = uisActivity s
    , uiPose        = uisPose s
    , uiAnimStride  = uisAnimStride s
    , uiStats       = uisStats s
    , uiModifiers   = uisModifiers s
    , uiSkills      = uisSkills s
    , uiKnowledge   = uisKnowledge s
    , uiInventory   = uisInventory s
    , uiEquipment   = uisEquipped s
    , uiAccessories = uisAccessories s
    -- wire → typed (#912). An unrecognized tag degrades to the inert
    -- fallback rather than failing the load; 'fromUnitSnapshot' hands
    -- the raw tags back so the caller can say so once each.
    , uiFactionId   = factionFromTag (uisFactionId s)
    , uiWounds      = uisWounds s
    , uiScars       = uisScars s
    , uiImmuneResponse = uisImmuneResponse s
    , uiImmunities  = uisImmunities s
    , uiBlood       = uisBlood s
    -- Runtime-only combat memory — reset on load. A bear that was
    -- in the middle of a fight gets a clean slate on reload; the
    -- next incoming hit will re-trigger retaliation.
    , uiLastAttackerUid = Nothing
    , uiLastAttackerAt  = 0
    -- Runtime-only animation override — Lua re-sets if needed.
    , uiAnimOverride = ""
    -- Runtime-only debug flags — always False on load.
    , uiFrozen      = False
    , uiForceLoop   = False
    , uiClimbDest   = Nothing   -- runtime-only; not persisted
    -- Runtime-only bleeding-trail emitter state (#882) — always a clean
    -- slate on load, same as the combat-memory fields above.
    , uiTrailState  = Nothing
    }

-- Missing-definition validation (#760 requirement 9) -----------------

-- | A saved building/unit instance whose content-definition reference
--   ('bisDefName'/'uisDefName') does NOT resolve against the currently-
--   registered definitions. Per #760 requirement 9 a missing gameplay
--   definition is a LOAD-VALIDATION FAILURE (the complete load is
--   rejected), not the silent per-entity pruning
--   'fromBuildingSnapshot'/'fromUnitSnapshot' fall back to — those still
--   return orphans as defense-in-depth, but the load boundary rejects a
--   save carrying any such reference before publishing any live state, so
--   that pruning path is unreachable in normal play. (Missing VISUAL
--   assets remain a soft #756 fallback — this is only about definitions.)
data MissingDefRef = MissingDefRef
    { mdrKind    ∷ !Text          -- ^ @"building"@ or @"unit"@
    , mdrPage    ∷ !WorldPageId
    , mdrEntity  ∷ !Word32        -- ^ the 'BuildingId'/'UnitId' raw value
    , mdrDefName ∷ !Text          -- ^ the unresolved definition name
    } deriving (Show, Eq)

renderMissingDefRef ∷ MissingDefRef → Text
renderMissingDefRef r =
    mdrKind r <> " #" <> tshow (mdrEntity r) <> " on page '"
        <> unWorldPageId (mdrPage r) <> "' references unknown definition '"
        <> mdrDefName r <> "'"

-- | Every saved building/unit whose definition name is absent from the
--   registered-definition key sets, across all saved pages. Empty ⇒ every
--   content reference resolves and the load may proceed. Pure: the def
--   key-sets come from the live managers at the load boundary
--   (@'Building.Types.bmDefs'@/@'Unit.Types.umDefs'@), the per-page
--   snapshots from the decoded save.
missingDefReferences
    ∷ HS.HashSet Text                               -- ^ registered building def names
    → HS.HashSet Text                               -- ^ registered unit def names
    → [(WorldPageId, BuildingSnapshot, UnitSnapshot)]
    → [MissingDefRef]
missingDefReferences buildingDefs unitDefs pages = concatMap pageRefs pages
  where
    pageRefs (pid, bs, us) =
        [ MissingDefRef "building" pid (unBuildingId bid) (bisDefName b)
        | (bid, b) ← HM.toList (bsnInstances bs)
        , not (HS.member (bisDefName b) buildingDefs) ]
        ⧺
        [ MissingDefRef "unit" pid (unUnitId uid) (uisDefName u)
        | (uid, u) ← HM.toList (usnInstances us)
        , not (HS.member (uisDefName u) unitDefs) ]

-- Shared item-instance enumeration (#1090) ---------------------------

-- | One 'ItemInstance' plus every item nested (recursively) in its
--   'iiContents' — a first-aid kit's own kit-in-kit contents.
--
--   THE recursive item walk of the save system (#1090). It used to be
--   written out three times, once per consumer; all three now go
--   through this one definition, together with 'pageItemContainers'
--   below: 'World.Save.Snapshot.allItemInstanceIds' (the id-allocator
--   and duplicate-id checks),
--   'Engine.Scripting.Lua.API.Save.Integrity.knownEntitiesFromSaveData'
--   (the load-time known-entity set), and 'missingItemDefReferences'.
flattenItemInstances ∷ ItemInstance → [ItemInstance]
flattenItemInstances i = i : concatMap flattenItemInstances (iiContents i)

-- | The order 'pageItemContainers' visits a page's containers in.
--
--   The three pre-#1090 enumerations were written with two opposite
--   conventions, and the order is observable in the lists two of them
--   produce ('World.Save.Snapshot.itemAllocatorErrors', the
--   'MissingItemDefRef' list), so unification keeps both orders rather
--   than imposing a new one on either. What is shared — and what makes
--   a newly added container reach every consumer from a single edit —
--   is the CONTAINER SET, not the order.
data ItemWalkOrder
    = ItemsGroundFirst
      -- ^ ground items → units → buildings, a building's
      --   materials-delivered before its storage.
    | ItemsBuildingsFirst
      -- ^ buildings → units → ground items, a building's storage
      --   before its materials-delivered.
    deriving (Show, Eq)

-- | Every item container on one saved unit, each tagged with the source
--   label 'MissingItemDefRef' reports for it.
--
--   THE place 'UnitInstanceSnapshot''s item-bearing fields are
--   enumerated: a container added to that record and listed here is
--   seen by every 'pageItemContainers' consumer, with no second or
--   third site to remember (#1090).
unitItemContainers ∷ UnitInstanceSnapshot → [(Text, [ItemInstance])]
unitItemContainers u =
    [ ("unit inventory",   uisInventory u)
    , ("unit equipped",    HM.elems (uisEquipped u))
    , ("unit accessories", uisAccessories u) ]

-- | Every item container on one saved building, each tagged with its
--   source label — 'unitItemContainers''s contract, for
--   'BuildingInstanceSnapshot'.
buildingItemContainers ∷ BuildingInstanceSnapshot → [(Text, [ItemInstance])]
buildingItemContainers b =
    [ ("building materials delivered"
      , concat (HM.elems (bisMaterialsDelivered b)))
    , ("building storage", bisStorage b) ]

-- | Every item container on one saved page: ground items, each unit's
--   inventory/equipped/accessories, each building's
--   materials-delivered/storage.
--
--   Parameterised over the three page projections rather than over a
--   page type, because the item-bearing fields of the two page shapes
--   have identical types — 'WorldPageSave''s @wps*@ and
--   'World.Save.Snapshot.PageSnapshot''s @pgs*@ — so one enumeration
--   serves both without any adaptation between them.
--
--   'wpsContainerKnowledge' (#1087) is deliberately NOT a container
--   here and must not become one: its remembered instances are
--   historical OBSERVATIONS of items living (or once living)
--   elsewhere in this very enumeration, not additional live entities —
--   see 'World.Save.Snapshot.allItemInstanceIds' for the full
--   reasoning. Only 'missingItemDefReferences' looks at them, and it
--   does so separately, because a remembered item's DEF NAME is still
--   an ordinary content reference.
pageItemContainers
    ∷ ItemWalkOrder
    → (page → GroundItems)
    → (page → UnitSnapshot)
    → (page → BuildingSnapshot)
    → page
    → [(Text, [ItemInstance])]
pageItemContainers order groundOf unitsOf buildingsOf page = case order of
    ItemsGroundFirst    → groundCs ⧺ unitCs ⧺ buildingCs
    ItemsBuildingsFirst → buildingCs ⧺ unitCs ⧺ groundCs
  where
    groundCs = [ ( "ground item"
                 , map giInst (HM.elems (gisItems (groundOf page))) ) ]
    unitCs   = concatMap unitItemContainers
                   (HM.elems (usnInstances (unitsOf page)))
    -- One definition of a building's containers, read in whichever
    -- direction the caller's historical order needs.
    buildingCs = concatMap (slotOrder ∘ buildingItemContainers)
                     (HM.elems (bsnInstances (buildingsOf page)))
    slotOrder = case order of
        ItemsGroundFirst    → id
        ItemsBuildingsFirst → reverse

-- Item / recipe / construct-target def-name validation (#760) --------

-- | A saved 'ItemInstance' (anywhere in the save — building storage/
--   materials-delivered, unit inventory/equipped/accessories, ground
--   items) whose 'iiDefName' does not resolve against the currently-
--   registered item definitions. Checked recursively through
--   'iiContents' (a first-aid kit's own nested items) — same
--   load-validation contract as 'MissingDefRef' (requirement 9): the
--   complete load is rejected before any live state publishes, not a
--   silent per-item drop.
data MissingItemDefRef = MissingItemDefRef
    { midrSource  ∷ !Text          -- ^ e.g. "building storage", "unit inventory"
    , midrPage    ∷ !WorldPageId
    , midrItemId  ∷ !Word64        -- ^ the item's own 'iiInstanceId'
    , midrDefName ∷ !Text          -- ^ the unresolved item definition name
    } deriving (Show, Eq)

renderMissingItemDefRef ∷ MissingItemDefRef → Text
renderMissingItemDefRef r =
    midrSource r <> " item #" <> tshow (midrItemId r) <> " on page '"
        <> unWorldPageId (midrPage r) <> "' references unknown item \
           \definition '" <> midrDefName r <> "'"

-- | Every saved item instance — across ground items, unit inventory/
--   equipped/accessories, and building storage/materials-delivered,
--   recursively through nested contents — whose def name is absent
--   from the registered item-definition key set. Empty ⇒ every item
--   reference resolves and the load may proceed.
missingItemDefReferences
    ∷ HS.HashSet Text                     -- ^ registered item def names
    → [(WorldPageId, WorldPageSave)]
    → [MissingItemDefRef]
missingItemDefReferences itemDefs pages = concatMap pageRefs pages
  where
    pageRefs (pid, w) = concat
        [ concat [ itemRefs pid src inst
                 | (src, insts) ← pageItemContainers ItemsBuildingsFirst
                       wpsGroundItems wpsUnits wpsBuildings w
                 , inst ← insts ]
        , concatMap (knowledgeRefs pid)
              (HM.elems (ckRecords (wpsContainerKnowledge w)))
        ]
    -- #1087: a REMEMBERED item's def name is still an ordinary persisted
    -- content reference and follows this same contract — only its
    -- INSTANCE ID is exempt from live-entity treatment (see
    -- 'World.Save.Snapshot.allItemInstanceIds'). A save whose remembered
    -- contents name a deregistered item def is rejected exactly like one
    -- whose live storage does.
    knowledgeRefs pid r =
        concatMap (itemRefs pid "container knowledge") (crItems r)
    itemRefs pid src inst =
        [ MissingItemDefRef src pid (iiInstanceId i) (iiDefName i)
        | i ← flattenItemInstances inst
        , not (HS.member (iiDefName i) itemDefs) ]

-- | A saved GUARANTEED SIGNIFICANT obligation (#917) that has not been
--   spawned yet and whose stored item def name no longer resolves.
--
--   Same load-validation contract as 'MissingItemDefRef', and for a
--   sharper reason than most: the obligation is what
--   @scripts\/locations.lua@ will try to spawn the next time the
--   location's chunk loads. If its def is gone, that spawn fails on
--   every attempt, @contents_spawned@ is never marked, and the location
--   can never satisfy its clearance predicate — the same unrecoverable
--   outcome 'Engine.Asset.YamlLocations.significantItemErrors' refuses
--   at the AUTHORING boundary, arriving here by the other route: a save
--   written before the item spawned, loaded against a build whose
--   definition set has moved on.
--
--   Deliberately restricted to UNSPAWNED obligations. Once one names a
--   physical item, nothing re-spawns it — the def name is then a
--   historical record of what was made, and the item may legitimately
--   have been consumed or destroyed — so a deregistered def there is
--   inert rather than fatal, and rejecting the load over it would
--   refuse a save nothing is actually wrong with.
data MissingSignificantItemRef = MissingSignificantItemRef
    { msirPage     ∷ !WorldPageId
    , msirInstance ∷ !Int          -- ^ the owning 'LocationInstanceId'
    , msirSlot     ∷ !Int          -- ^ the obligation's own slot
    , msirDefName  ∷ !Text         -- ^ the unresolved item definition name
    } deriving (Show, Eq)

renderMissingSignificantItemRef ∷ MissingSignificantItemRef → Text
renderMissingSignificantItemRef r =
    "location #" <> tshow (msirInstance r) <> " on page '"
        <> unWorldPageId (msirPage r) <> "' owes an unspawned significant \
           \item at slot " <> tshow (msirSlot r)
        <> " referencing unknown item definition '" <> msirDefName r <> "'"

-- | Every unspawned significant obligation across every page whose
--   stored def name is absent from the registered item-definition key
--   set. Empty ⇒ every obligation the load would still have to spawn
--   can actually be spawned.
missingSignificantItemReferences
    ∷ HS.HashSet Text                     -- ^ registered item def names
    → [(WorldPageId, WorldPageSave)]
    → [MissingSignificantItemRef]
missingSignificantItemReferences itemDefs pages =
    [ MissingSignificantItemRef pid (unLocationInstanceId (liId inst))
                                (lsiSlot e) (lsiItemDefName e)
    | (pid, w) ← pages
    , inst ← instancesToList (wgpLocationInstances (wpsGenParams w))
    , e ← liSignificant inst
    , isNothing (lsiInstanceId e)
    , not (HS.member (lsiItemDefName e) itemDefs)
    ]

-- | A saved craft bill whose 'cbRecipe' does not resolve against the
--   currently-registered recipe catalogue. Same load-validation
--   contract as 'MissingDefRef'.
data MissingRecipeRef = MissingRecipeRef
    { mrrPage   ∷ !WorldPageId
    , mrrBillId ∷ !Word32
    , mrrRecipe ∷ !Text
    } deriving (Show, Eq)

renderMissingRecipeRef ∷ MissingRecipeRef → Text
renderMissingRecipeRef r =
    "craft bill #" <> tshow (mrrBillId r) <> " on page '"
        <> unWorldPageId (mrrPage r) <> "' references unknown recipe '"
        <> mrrRecipe r <> "'"

-- | Every saved craft bill, across all pages, whose recipe id is absent
--   from the registered recipe key set. Empty ⇒ every recipe reference
--   resolves and the load may proceed.
missingRecipeReferences
    ∷ HS.HashSet Text                     -- ^ registered recipe ids
    → [(WorldPageId, WorldPageSave)]
    → [MissingRecipeRef]
missingRecipeReferences recipeDefs pages =
    [ MissingRecipeRef pid (unBillId (cbId b)) (cbRecipe b)
    | (pid, w) ← pages
    , b ← HM.elems (cbsBills (wpsCraftBills w))
    , not (HS.member (cbRecipe b) recipeDefs) ]

-- | A saved craft bill's 'cbOutputItem' (#795 — the item-definition name an
--   UntilStock bill's stock target counts against, captured at add time)
--   that does not resolve against the currently-registered item
--   definitions. Same load-validation contract as 'MissingRecipeRef'/
--   'MissingItemDefRef': the complete load is rejected before any live
--   state publishes. 'cbOutputItem' is empty for FixedCount/RepeatForever
--   bills (never set), so only a non-empty value is checked here.
data MissingBillOutputItemRef = MissingBillOutputItemRef
    { mbirPage    ∷ !WorldPageId
    , mbirBillId  ∷ !Word32
    , mbirDefName ∷ !Text
    } deriving (Show, Eq)

renderMissingBillOutputItemRef ∷ MissingBillOutputItemRef → Text
renderMissingBillOutputItemRef r =
    "craft bill #" <> tshow (mbirBillId r) <> " on page '"
        <> unWorldPageId (mbirPage r) <> "' references unknown output item \
           \definition '" <> mbirDefName r <> "'"

-- | Every saved craft bill, across all pages, whose non-empty
--   'cbOutputItem' is absent from the registered item-definition key set.
--   Empty ⇒ every bill output-item reference resolves (or has none) and
--   the load may proceed.
missingBillOutputItemReferences
    ∷ HS.HashSet Text                     -- ^ registered item def names
    → [(WorldPageId, WorldPageSave)]
    → [MissingBillOutputItemRef]
missingBillOutputItemReferences itemDefs pages =
    [ MissingBillOutputItemRef pid (unBillId (cbId b)) (cbOutputItem b)
    | (pid, w) ← pages
    , b ← HM.elems (cbsBills (wpsCraftBills w))
    , not (T.null (cbOutputItem b))
    , not (HS.member (cbOutputItem b) itemDefs) ]

-- | A saved construct designation whose target names a building
--   definition ('World.Construct.Types.CtBuilding') that does not
--   resolve against the currently-registered building definitions.
--   Same load-validation contract as 'MissingDefRef' — a 'CtStructure'
--   target references a structure-pack piece, not a building def, and
--   is out of scope here.
data MissingConstructDefRef = MissingConstructDefRef
    { mcdPage    ∷ !WorldPageId
    , mcdTile    ∷ !(Int, Int)
    , mcdDefName ∷ !Text
    } deriving (Show, Eq)

renderMissingConstructDefRef ∷ MissingConstructDefRef → Text
renderMissingConstructDefRef r =
    "construct designation at " <> tshow (mcdTile r) <> " on page '"
        <> unWorldPageId (mcdPage r) <> "' references unknown building \
           \definition '" <> mcdDefName r <> "'"

-- | Every saved construct designation, across all pages, whose
--   'CtBuilding' target names a building definition absent from the
--   registered building key set. Empty ⇒ every construct-target
--   reference resolves and the load may proceed.
missingConstructDefReferences
    ∷ HS.HashSet Text                     -- ^ registered building def names
    → [(WorldPageId, WorldPageSave)]
    → [MissingConstructDefRef]
missingConstructDefReferences buildingDefs pages =
    [ MissingConstructDefRef pid tile defName
    | (pid, w) ← pages
    , (tile, cd) ← HM.toList (wpsConstructDesignations w)
    , CtBuilding defName ← [cdTarget cd]
    , not (HS.member defName buildingDefs) ]

-- Material-id validation (issue #763) --------------------------------

-- | A saved 'MaterialId' — from the edit log ('WeAddTile'/'WeSetCell'),
--   a spoil pile, or a worldgen tectonic plate's base material — that
--   this build's 'World.Material.MaterialRegistry' never registered.
--   Unlike every other 'Missing*Ref' above, 'MaterialId' is a numeric
--   'Word8' index that is ALWAYS a structurally valid slot (see
--   'World.Material.isKnownMaterial''s haddock), so this can only mean
--   the material genuinely existed in the save's origin build's YAML
--   data and was later removed from this one — same load-validation
--   contract as every other missing reference (the issue's own
--   acceptance criteria names "material" explicitly alongside
--   unit/item/building/recipe).
data MissingMaterialRef = MissingMaterialRef
    { mmrSource ∷ !Text          -- ^ e.g. "edit log", "spoil pile", "tectonic plate"
    , mmrPage   ∷ !WorldPageId
    , mmrCoord  ∷ !(Int, Int)
    , mmrMatId  ∷ !Word8
    } deriving (Show, Eq)

renderMissingMaterialRef ∷ MissingMaterialRef → Text
renderMissingMaterialRef r =
    mmrSource r <> " at " <> tshow (mmrCoord r) <> " on page '"
        <> unWorldPageId (mmrPage r) <> "' references unknown material id "
        <> tshow (mmrMatId r)

-- | Every saved material reference, across all pages, that does not
--   resolve against the currently-registered material set. Covers the
--   edit log, spoil piles, AND each page's worldgen plate data
--   ('wgpPlates' — a plate's base material is
--   persisted just like any other 'MaterialId' and staging would
--   otherwise silently render it with 'defaultMaterialProps' instead
--   of rejecting the load). Empty ⇒ every reference resolves and the
--   load may proceed.
missingMaterialReferences
    ∷ MaterialRegistry
    → [(WorldPageId, WorldPageSave)]
    → [MissingMaterialRef]
missingMaterialReferences registry pages = concatMap pageRefs pages
  where
    pageRefs (pid, w) =
        [ MissingMaterialRef "edit log" pid (gx, gy) (unMaterialId mat)
        | edits ← HM.elems (wpsEdits w)
        , edit ← edits
        , (gx, gy, mat) ← editMaterialRef edit
        , not (isKnownMaterial registry mat) ]
        ⧺
        [ MissingMaterialRef "spoil pile" pid coord (unMaterialId (spMat sp))
        | (coord, sp) ← HM.toList (wpsSpoilPiles w)
        , not (isKnownMaterial registry (spMat sp)) ]
        ⧺
        [ MissingMaterialRef "tectonic plate" pid
              (plateCenterX plate, plateCenterY plate)
              (unMaterialId (plateMaterial plate))
        | plate ← wgpPlates (wpsGenParams w)
        , not (isKnownMaterial registry (plateMaterial plate)) ]
    editMaterialRef (WeAddTile gx gy mat)   = [(gx, gy, mat)]
    editMaterialRef (WeSetCell gx gy _z mat) = [(gx, gy, mat)]
    editMaterialRef _                        = []

-- Flora-species-reference validation (issues #763, #2243) ------------

-- | A saved flora-species reference — from the edit log
--   ('World.Edit.Types.WePlaceFloraRef'), a crop plot's species, or a
--   plant designation's chosen crop — that this build's
--   'World.Flora.Types.FloraCatalog' does not resolve. Flora species
--   drive crop/foraging gameplay (growth stage, harvest yield, ...), so
--   an unresolved one is exactly the same class of load-blocking
--   problem as a missing unit/item/building/recipe/material definition.
--
--   #2243 changed WHAT is unresolved rather than what happens when it
--   is. A save written by this build names its species, so the usual
--   failure is a name this build no longer ships — the same shape as
--   'MissingItemDefRef', and reported the same way, by name. A payload
--   written before names carries an ordinal instead, and that stays
--   reportable too: 'World.Flora.Reference.FloraRef' holds whichever
--   the save actually recorded, so the diagnostic never has to invent
--   a name for a reference that never had one.
data MissingFloraRef = MissingFloraRef
    { mfrSource  ∷ !Text          -- ^ e.g. "edit log", "crop plot"
    , mfrPage    ∷ !WorldPageId
    , mfrCoord   ∷ !(Int, Int)
    , mfrSpecies ∷ !FloraRef
    } deriving (Show, Eq)

renderMissingFloraRef ∷ MissingFloraRef → Text
renderMissingFloraRef r =
    mfrSource r <> " at " <> tshow (mfrCoord r) <> " on page '"
        <> unWorldPageId (mfrPage r) <> "' references unknown "
        <> renderFloraRef (mfrSpecies r)

-- | Every saved flora-species reference, across all pages, that does
--   not resolve against the currently-registered flora catalog. Empty
--   ⇒ every reference resolves and the load may proceed.
--
--   Walks all THREE durable reference sites: the edit log, crop plots,
--   and — since #2243 — plant designations, which were silently omitted
--   before and could therefore restore a designation naming a species
--   nothing could plant.
--
--   Every site is validated through the one
--   'World.Flora.Reference.resolveFloraRef', so a name and a legacy
--   ordinal are resolved by exactly the rule
--   'World.Load.Stage.stagePage' will apply a moment later. That
--   agreement is what makes staging total: this check refuses the whole
--   load before anything is staged, so by the time staging resolves the
--   same references, every one of them is known to resolve.
missingFloraReferences
    ∷ FloraCatalog
    → [(WorldPageId, WorldPageSave)]
    → [MissingFloraRef]
missingFloraReferences catalog pages = concatMap pageRefs pages
  where
    pageRefs (pid, w) =
        [ MissingFloraRef "edit log" pid (gx, gy) ref
        | edits ← HM.elems (wpsEdits w)
        , edit ← edits
        , (gx, gy, ref) ← editFloraRef edit
        , unresolved ref ]
        ⧺
        [ MissingFloraRef "crop plot" pid coord (cpSpecies cp)
        | (coord, cp) ← HM.toList (wpsCropPlots w)
        , unresolved (cpSpecies cp) ]
        ⧺
        [ MissingFloraRef "plant designation" pid coord (ptCrop pd)
        | (coord, pd) ← HM.toList (wpsPlantDesignations w)
        , unresolved (ptCrop pd) ]
    -- Only the #2243 constructor can appear here: a decoded page's
    -- planting edits are named, whatever version the payload was
    -- written at
    -- ('World.Save.Component.PageEdits.applyWorldEdits' rewrites
    -- the two legacy numeric forms into it), and a captured page's are
    -- named by 'World.Thread.Command.Save.WriteWorld'. The two legacy
    -- constructors are matched anyway rather than falling into the
    -- catch-all, so a payload that somehow still carried one would be
    -- VALIDATED as the legacy ordinal it is instead of skipped
    -- unchecked.
    editFloraRef (WePlaceFloraRef gx gy ref _day _grow _iid) =
        [(gx, gy, ref)]
    editFloraRef (WePlaceFlora gx gy fid _day _grow) =
        [(gx, gy, FloraByLegacyId fid)]
    editFloraRef (WePlaceFloraWithId gx gy fid _day _grow _iid) =
        [(gx, gy, FloraByLegacyId fid)]
    editFloraRef _ = []
    unresolved ref = isNothing (resolveFloraRef catalog ref)

-- | The save-side rendering of the same finding: a LIVE species handle
--   the capturing build's own catalog could not name, so no durable
--   reference could be written for it. Distinct wording from
--   'renderMissingFloraRef' because the failure is the mirror image —
--   the save is refused, nothing is published, and the reference that
--   could not be named is always an ordinal (there is no name to quote).
renderUnnamedFloraRef ∷ MissingFloraRef → Text
renderUnnamedFloraRef r =
    mfrSource r <> " at " <> tshow (mfrCoord r) <> " on page '"
        <> unWorldPageId (mfrPage r) <> "' names " <> renderFloraRef
               (mfrSpecies r) <> ", which this build's flora catalog \
           \does not resolve"

-- | Save side (#2243 requirement 4): turn one page's LIVE flora
--   references into the durable ones a capture persists, or report every
--   handle the catalog could not name.
--
--   This is where a save acquires its species names, and the only
--   place: the page-scoped components' encoders (behind
--   "World.Save.Component.Page") see a
--   'World.Save.Snapshot.SessionSnapshot' that already holds
--   'World.Flora.Reference.FloraRef's and never consult live catalog
--   state, so an encode cannot fail and cannot invent a name. A handle
--   with no species behind it is refused HERE, before
--   'World.Save.Storage.publishGeneration' is reached, so the slot on
--   disk is never touched — the save-side counterpart of
--   'missingFloraReferences' refusing a load.
--
--   An edit that is already named crosses unchanged. That is not a
--   live-session case (a live log holds 'WePlaceFloraWithId'); it keeps
--   the function total and idempotent, so re-capturing an already-named
--   page is the identity rather than a failure.
nameFloraReferences
    ∷ FloraCatalog
    → WorldPageId
    → WorldEdits
    → CropPlots
    → PlantDesignations
    → Either [MissingFloraRef]
             (WorldEdits, SavedCropPlots, SavedPlantDesignations)
nameFloraReferences catalog pid edits crops plants =
    case (traverse (traverse nameEdit) edits
         , traverse nameCrop crops
         , traverse namePlant plants) of
        (Just es, Just cs, Just ps) → Right (es, cs, ps)
        _                           → Left problems
  where
    -- One lookup per reference, and the SAME lookup decides both
    -- outcomes: a 'Nothing' anywhere is what makes the triple above
    -- fail, and 'problems' re-walks the identical sites to say which.
    -- Nothing here can fall back to a default reference, because there
    -- is no total conversion to fall back to.
    named = floraRefForId catalog
    unnamed fid = isNothing (named fid)
    nameEdit e = case e of
        WePlaceFlora gx gy fid day w →
            (\ref → WePlaceFloraRef gx gy ref day w floraInstanceIdNone)
                <$> named fid
        WePlaceFloraWithId gx gy fid day w iid →
            (\ref → WePlaceFloraRef gx gy ref day w iid) <$> named fid
        -- Already named. Not a live-session case (a live log holds
        -- 'WePlaceFloraWithId'); it keeps this idempotent, so
        -- re-capturing an already-named page is the identity rather
        -- than a failure.
        _ → Just e
    nameCrop cp  = (\ref → cp { cpSpecies = ref }) <$> named (cpSpecies cp)
    namePlant pd = (\ref → pd { ptCrop = ref }) <$> named (ptCrop pd)
    problems =
        [ MissingFloraRef "edit log" pid (gx, gy) (FloraByLegacyId fid)
        | es ← HM.elems edits, e ← es, (gx, gy, fid) ← liveEditSpecies e
        , unnamed fid ]
        ⧺
        [ MissingFloraRef "crop plot" pid coord
              (FloraByLegacyId (cpSpecies cp))
        | (coord, cp) ← HM.toList crops, unnamed (cpSpecies cp) ]
        ⧺
        [ MissingFloraRef "plant designation" pid coord
              (FloraByLegacyId (ptCrop pd))
        | (coord, pd) ← HM.toList plants, unnamed (ptCrop pd) ]
    liveEditSpecies (WePlaceFlora gx gy fid _ _)         = [(gx, gy, fid)]
    liveEditSpecies (WePlaceFloraWithId gx gy fid _ _ _) = [(gx, gy, fid)]
    liveEditSpecies _                                    = []

-- | Load side (#2243 requirement 2): turn one decoded page's durable
--   flora references back into the runtime handles the live session
--   uses, or report every reference the loading build's catalog does not
--   resolve.
--
--   Called from 'World.Load.Stage.stagePage', the first point in the
--   load transaction holding a catalog. It is total in practice —
--   'missingFloraReferences' has already refused the whole load, against
--   this same catalog and through this same
--   'World.Flora.Reference.resolveFloraRef', if any reference did not
--   resolve — but it still returns every unresolved reference rather
--   than defaulting one, so a caller that skipped that gate fails
--   loudly instead of planting an arbitrary species.
--
--   A legacy ordinal resolves to ITSELF where the loading catalog holds
--   that number (D-2): the species it now names may not be the species
--   that was planted, which is the documented, deliberately unguarded
--   limitation of every pre-#2243 save.
resolveFloraReferences
    ∷ FloraCatalog
    → WorldPageId
    → WorldEdits
    → SavedCropPlots
    → SavedPlantDesignations
    → Either [MissingFloraRef] (WorldEdits, CropPlots, PlantDesignations)
resolveFloraReferences catalog pid edits crops plants =
    case (traverse (traverse resolveEdit) edits
         , traverse resolveCrop crops
         , traverse resolvePlant plants) of
        (Just es, Just cs, Just ps) → Right (es, cs, ps)
        _                           → Left problems
  where
    resolve = resolveFloraRef catalog
    unresolved ref = isNothing (resolve ref)
    resolveEdit e = case e of
        WePlaceFloraRef gx gy ref day w iid →
            (\fid → WePlaceFloraWithId gx gy fid day w iid) <$> resolve ref
        -- A decoded page carries no numeric planting edit —
        -- @World.Save.Component.PageEdits.migrateWorldEditDTOv2@ names
        -- both
        -- legacy forms — so this crosses whatever a caller handed in
        -- unchanged rather than re-deriving an id it already has.
        _ → Just e
    resolveCrop cp  = (\fid → cp { cpSpecies = fid }) <$> resolve (cpSpecies cp)
    resolvePlant pd = (\fid → pd { ptCrop = fid }) <$> resolve (ptCrop pd)
    problems =
        [ MissingFloraRef "edit log" pid (gx, gy) ref
        | es ← HM.elems edits, e ← es, (gx, gy, ref) ← savedEditSpecies e
        , unresolved ref ]
        ⧺
        [ MissingFloraRef "crop plot" pid coord (cpSpecies cp)
        | (coord, cp) ← HM.toList crops, unresolved (cpSpecies cp) ]
        ⧺
        [ MissingFloraRef "plant designation" pid coord (ptCrop pd)
        | (coord, pd) ← HM.toList plants, unresolved (ptCrop pd) ]
    savedEditSpecies (WePlaceFloraRef gx gy ref _ _ _) = [(gx, gy, ref)]
    savedEditSpecies _                                 = []

-- Location-overlay-id validation (issue #763) ------------------------

-- | A saved location reference — an overlay entry
--   ('WorldGenParams.wgpLocationOverlay', one per placed chunk) or a
--   placed-location INSTANCE (#911, 'WorldGenParams.wgpLocationInstances')
--   — whose location id does not resolve against the
--   currently-registered 'Location.Types.LocationRegistry'. Unlike
--   'MaterialId'/'FloraId', this is a plain Text key — same shape as
--   'MissingDefRef' — but it lives on 'WorldGenParams' rather than
--   inside 'WorldPageSave' proper, which is why it needed its own
--   validation function even though the check itself is a direct
--   'HS.HashSet' membership test. A missing location definition would
--   otherwise silently skip location discovery/placement-bounds
--   checks for that chunk after publication instead of rejecting the
--   load — and, since #911, would leave a pre-#911 save's chunk flags
--   with no instance to migrate onto.
data MissingLocationRef = MissingLocationRef
    { mlrPage  ∷ !WorldPageId
    , mlrCoord ∷ !(Int, Int)
    , mlrLocId ∷ !Text
    } deriving (Show, Eq)

renderMissingLocationRef ∷ MissingLocationRef → Text
renderMissingLocationRef r =
    "location overlay chunk " <> tshow (mlrCoord r) <> " on page '"
        <> unWorldPageId (mlrPage r) <> "' references unknown location id '"
        <> mlrLocId r <> "'"

-- | Every saved location reference, across all pages, that does not
--   resolve against the currently-registered location definitions.
--   Empty ⇒ every reference resolves and the load may proceed.
--
--   #911 made this the successor to the overlay-only check: BOTH the
--   overlay entries and the placed instances are covered, so a v2 save
--   whose instance names a def this build no longer registers is
--   rejected too. Deduplicated by (page, chunk, id) — an instance and
--   the overlay entry it was placed from name the same def in the same
--   chunk, so the pair reports once.
missingLocationDefReferences
    ∷ HS.HashSet Text                     -- ^ registered location def ids
    → [(WorldPageId, WorldPageSave)]
    → [MissingLocationRef]
missingLocationDefReferences locationDefs pages = concatMap pageRefs pages
  where
    pageRefs (pid, w) =
        [ MissingLocationRef pid coord locId
        | (coord, locId) ← L.nub (overlayRefs w ⧺ instanceRefs w)
        , not (HS.member locId locationDefs) ]
    overlayRefs w =
        [ ((cx, cy), locId)
        | (ChunkCoord cx cy, locId)
            ← HM.toList (wgpLocationOverlay (wpsGenParams w)) ]
    instanceRefs w =
        [ ((cx, cy), liDefId inst)
        | inst ← instancesToList (wgpLocationInstances (wpsGenParams w))
        , let ChunkCoord cx cy = liChunk inst ]

-- | Resolve one loaded page's PENDING pre-#911 per-chunk location flags
--   into real instances, against the registry the caller has already
--   checked every location id against
--   ('missingLocationDefReferences'). Total and idempotent: a page whose
--   instance table carries nothing pending — a v2 (post-#911) save, a
--   freshly generated world, or a page already resolved — is returned
--   unchanged, so a definition edited since placement can never
--   overwrite an existing instance's stored bounds or display name
--   (#1230 removed the margin that used to sit between them). Called by the load path AFTER content validation and BEFORE
--   staging/publication ('Engine.Scripting.Lua.API.Save.loadSaveFn'):
--   the pure component decoders have no registry to resolve against.
--
--   #1796: reconstruction goes through the checked geometry
--   construction, so a legacy payload whose saved overlay names a chunk
--   coordinate outside the representable envelope yields a
--   'LocationGeometryError' here and the caller rejects the load —
--   again, before anything is staged or published.
resolveLegacyLocations
    ∷ LocationRegistry → WorldPageSave
    → Either LocationGeometryError WorldPageSave
resolveLegacyLocations registry w =
    (\params → w { wpsGenParams = params })
        ⊚ resolveLegacyLocationParams registry (wpsGenParams w)

-- | 'resolveLegacyLocations' at the gen-params level — the actual work,
--   exposed separately so any other stage holding a page's params (and
--   the migration gates) applies the SAME resolution rather than
--   re-deriving it.
resolveLegacyLocationParams
    ∷ LocationRegistry → WorldGenParams
    → Either LocationGeometryError WorldGenParams
resolveLegacyLocationParams registry params =
    (\instances → params { wgpLocationInstances = instances })
        ⊚ resolveLegacyLocationInstances registry
              (wgpLocationOverlay params) (wgpLocationInstances params)

-- Infection-definition validation (issue #763) -----------------------

-- | A saved 'Wound' whose 'woundInfectionType' does not resolve against
--   the currently-registered 'Infection.Types.InfectionManager'. Empty
--   string is the documented "no infection" sentinel (every wound
--   starts this way; see 'Combat.Resolution'/'Combat.Wounds.Sever') and
--   is deliberately excluded, mirroring 'World.Material.isKnownMaterial'\'s
--   own air (id 0) exclusion — same shape of problem, a real sentinel
--   value that must never be treated as "missing". A genuinely
--   unresolved infection type is a required-content gap of the same
--   kind as a missing unit/item/building/recipe/material/location
--   definition: 'Engine.Scripting.Lua.API.Units.Combat.lookupInfection'
--   drives the actual gameplay treatment/progression path off it, so
--   loading with a fallback here would silently change the wound's
--   behavior after publication rather than rejecting the load.
data MissingInfectionRef = MissingInfectionRef
    { mirPage    ∷ !WorldPageId
    , mirUnitId  ∷ !Word32
    , mirWoundPart ∷ !Text
    , mirInfType ∷ !Text
    } deriving (Show, Eq)

renderMissingInfectionRef ∷ MissingInfectionRef → Text
renderMissingInfectionRef r =
    "unit #" <> tshow (mirUnitId r) <> " wound (" <> mirWoundPart r
        <> ") on page '" <> unWorldPageId (mirPage r)
        <> "' references unknown infection id '" <> mirInfType r <> "'"

-- | Every saved WOUND-infection reference, across all pages, that does
--   not resolve against the currently-registered infection catalogue.
--   Empty ⇒ every reference resolves and the load may proceed.
--
--   Wound references are the whole of this detector's scope, and
--   deliberately so (#2305). A saved unit carries a SECOND kind of
--   infection-id reference — the 'Unit.Types.uiImmunities' keys of the
--   acquired immunity it has earned — and those are NOT inventoried
--   here: an unresolved one is scrubbed during staging by
--   'fromUnitSnapshot' and logged, never rejected. The asymmetry is the
--   blast radius. A wound reference names an infection a unit is
--   CURRENTLY carrying, so rejecting the load is the only way not to
--   silently change that wound's treatment and progression after
--   publication; an immunity key is a memory of content the unit has
--   already survived, and inventorying it here would make removing one
--   infection definition reject every save in which any surviving unit
--   still holds un-decayed immunity to it. #1087's container-knowledge
--   scrub set the precedent for that second case: a memory of removed
--   content has no surface of its own, so it is dropped and diagnosed
--   rather than treated as corruption.
missingInfectionReferences
    ∷ InfectionManager
    → [(WorldPageId, WorldPageSave)]
    → [MissingInfectionRef]
missingInfectionReferences infMgr pages = concatMap pageRefs pages
  where
    pageRefs (pid, w) =
        [ MissingInfectionRef pid (unUnitId uid) (woundPart wd) infType
        | (uid, u) ← HM.toList (usnInstances (wpsUnits w))
        , wd ← uisWounds u
        , let infType = woundInfectionType wd
        , not (T.null infType)
        , not (isJust (lookupInfection infType infMgr)) ]

-- Acquired-immunity reconciliation (issue #2305) ---------------------

-- | What one page's 'fromUnitSnapshot' dropped from the units it
--   restored, because the acquired-immunity entry's key names an
--   infection definition that is no longer registered.
--
--   'iscRemoved' counts immunity-map ENTRIES actually removed, so a
--   page on which four units each carried the same dead id reports 4;
--   'iscIds' is the DISTINCT unresolved ids those removals represent,
--   sorted, so the same page names it once. A page that dropped nothing
--   is 'emptyImmunityScrub' and produces no diagnostic at all.
data ImmunityScrub = ImmunityScrub
    { iscRemoved ∷ !Int
    , iscIds     ∷ ![Text]
    } deriving (Show, Eq)

emptyImmunityScrub ∷ ImmunityScrub
emptyImmunityScrub = ImmunityScrub { iscRemoved = 0, iscIds = [] }

-- | The staging diagnostic for one page's scrub, or 'Nothing' when that
--   page dropped nothing. Non-blocking by construction: there is no
--   error value to return, only a line to log.
renderImmunityScrub ∷ WorldPageId → ImmunityScrub → Maybe Text
renderImmunityScrub pid s
    | iscRemoved s ≡ 0 = Nothing
    | otherwise = Just $
        "dropping " <> tshow (iscRemoved s) <> " acquired-immunity entr"
        <> (if iscRemoved s ≡ 1 then "y" else "ies") <> " on page '"
        <> unWorldPageId pid <> "' whose infection definition no longer \
           \exists (" <> T.intercalate ", " (iscIds s) <> ")"
