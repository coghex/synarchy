{-# LANGUAGE Strict #-}
module World.State.Types
    ( WorldState(..)
    , BloodTextureHandles
    , emptyWorldState
    , pageWrapWorldSize
    , pageSimTopology
    , bumpQuadCacheGen
    , WorldManager(..)
    , emptyWorldManager
    , bumpSelectionGen
    , requestSelectionChange
    , completeSelectionChange
    , settleSelectionProjection
    , projectSelectionVisible
    , selectionHead
    , selectionMovedSince
    , selectionChangeInFlight
    , projectedVisible
    , visiblePage
    , visiblePageState
    , pageLanguageProvenance
    , CursorSnapshot(..)
    , LoadPhase(..)
    ) where

import UPrelude
import Control.Concurrent.MVar (MVar, newMVar)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef)
import Language.Generated.Types (LanguageProvenance)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import Engine.Graphics.Camera (CameraFacing(..))
import World.Cursor.Types (CursorState(..), emptyCursorState)
import World.Page.Types (WorldPageId(..), WorldIdentity(..))
import World.Page.GeneratedId (GeneratedWorldId, newGeneratedWorldId)
import World.Chunk.Types (ChunkCoord(..))
import World.Chunk.Residency (ChunkOwner, emptyChunkOwner, newChunkGeneration)
import World.Tile.Types (WorldTileData(..), emptyWorldTileData)
import World.Render.Camera.Types (WorldCamera(..), WorldQuadCache(..))
import World.Render.Textures.Types (WorldTextures(..), defaultWorldTextures)
import World.ZoomMap.Types (ZoomChunkEntry(..))
import World.Render.Zoom.Types (ZoomQuadCache(..), BakedZoomEntry(..), ZoomMapMode(..), ZoomAtlasInfo(..))
import World.Tool.Types (ToolMode(..))
import World.Generate.Types (WorldGenParams(..))
import Sim.Topology (SimTopology(..), simTopologyForParams)
import World.Time.Types (WorldTime(..), WorldDate(..), defaultWorldTime, defaultWorldDate)
import World.Edit.Types (WorldEdit, WorldEdits, emptyWorldEdits)
import Structure.Types (StructureStage, emptyStructureStage)
import World.Mine.Types (MineDesignations)
import World.Construct.Types (ConstructDesignations)
import World.Construct.Attempt (ConstructAttemptId, firstConstructAttemptId)
import World.Chop.Types (ChopDesignations, PendingChopDesignations)
import World.Till.Types (TillDesignations)
import World.Plant.Types (PlantDesignations)
import Craft.Bills (CraftBills, emptyCraftBills)
import Unit.Transfer.Orders (TransferOrders, emptyTransferOrders)
import Building.Types (BuildingId)
import Building.Knowledge (ContainerKnowledge, emptyContainerKnowledge)
import Power.Types (PowerNodes, emptyPowerNodes)
import Blood.Types (BloodStore, BloodTextureId, emptyBloodStore, defaultBloodTextureCap)
import Engine.Asset.Handle (TextureHandle)
import World.Spoil.Types (SpoilPiles, emptySpoilPiles)
import World.Flora.Harvest (FloraHarvests, PendingFloraHarvests,
                            emptyFloraHarvests, emptyPendingFloraHarvests)
import World.Flora.CropPlot (CropPlots, emptyCropPlots)
import World.Flora.Identity (firstPlantedFloraCursor)
import Item.Ground (GroundItems, emptyGroundItems)

-- | Per-world GPU-upload bookkeeping for #606's procedurally generated
--   blood textures: each live 'BloodTextureId' maps to its bindless
--   'TextureHandle' plus that texture's own GPU cleanup action. Named so
--   the cross-thread dispose transport ('Engine.Core.State.bloodDisposeQueue',
--   #788) can carry a page's live handle 'IORef' to the render thread.
type BloodTextureHandles = HM.HashMap BloodTextureId (TextureHandle, IO ())

data WorldState = WorldState
    { wsTilesRef     ∷ IORef WorldTileData
    , wsCameraRef    ∷ IORef WorldCamera
    , wsTexturesRef  ∷ IORef WorldTextures
    , wsGenParamsRef ∷ IORef (Maybe WorldGenParams)
    , wsTimeRef      ∷ IORef WorldTime
    , wsDateRef      ∷ IORef WorldDate
    , wsTimeScaleRef ∷ IORef Float    -- ^ Game-minutes per real-second
    , wsResumeScaleRef ∷ IORef (Maybe Float)
      -- ^ #1599: the 'wsTimeScaleRef' value to reinstate on this page
      --   when the CURRENT pause epoch ends, or 'Nothing' when this page
      --   is not carrying one.
      --
      --   "World.Pause" is the only writer. It captures this page's
      --   chosen speed here immediately before zeroing the clock on the
      --   unpaused→paused transition — whatever imposed that pause,
      --   including the engine-side writers that never run a line of Lua
      --   ('Engine.PlayerEvent.Emit''s @pause: true@ category,
      --   @engine.saveWorld@'s acceptance) — and writes it back, clearing
      --   this slot, on the paused→unpaused transition. Holding it PER
      --   PAGE is what makes a resume restore only the page whose clock
      --   the pause actually zeroed: a page that disappears mid-pause
      --   takes its slot with it, so no other page can inherit its speed.
      --
      --   Runtime-only, never persisted: a load comes up paused at the
      --   default speed, so a saved epoch could never be meaningful (see
      --   @docs\/persistence_state_inventory.md@ §3).
    , wsZoomCacheRef ∷ IORef (V.Vector ZoomChunkEntry)  -- ^ Pre-computed zoom map cache for current world state
    , wsQuadCacheRef  ∷ IORef (Maybe WorldQuadCache)  -- ^ Cached quads for current camera state
    , wsQuadCacheGenRef ∷ IORef Int
      -- ^ Invalidation generation for the quad cache. Bumped atomically
      --   (from any thread) instead of nulling 'wsQuadCacheRef', so a
      --   cross-thread invalidation can't be clobbered by the render
      --   thread's read-rebuild-write of the cache. A cache is only valid
      --   when its 'wqcGen' matches this counter; the render thread is the
      --   sole writer of 'wsQuadCacheRef'. See 'bumpQuadCacheGen'.
    , wsZoomQuadCacheRef ∷ IORef (Maybe ZoomQuadCache)  -- ^ Cached quads for zoomed-out view
    , wsBgQuadCacheRef ∷ IORef (Maybe ZoomQuadCache)    -- ^ Cached quads for background layer
    , wsBakedZoomRef ∷ IORef (V.Vector BakedZoomEntry, WorldTextures, CameraFacing)  -- ^ Pre-baked
    , wsBakedBgRef ∷ IORef (V.Vector BakedZoomEntry, WorldTextures, CameraFacing)    -- ^ Pre-baked background entries with resolved textures and vertices
    , wsInitQueueRef ∷ IORef [ChunkCoord]  -- ^ Queue of chunks to generate at world init (for progress tracking)
    , wsInitQueueLock ∷ MVar ()
      -- ^ Held while this page's init QUEUE and its 'wsLoadPhaseRef' are
      --   changed together (#2001).
      --
      --   The two are separate 'IORef's with two writing threads — the
      --   world thread settles the phase as it drains, and the Lua
      --   thread appends and accounts for its own request — so no
      --   ordering of reads and writes makes them consistent on its own.
      --   The failure that matters is a false TERMINAL phase: an append
      --   landing between the drain's queue read and its phase write
      --   leaves 'LoadDone' standing over accepted work, and every
      --   waiter polling for that phase reports a load that has not
      --   happened.
      --
      --   Each writer takes this for the whole read-decide-write, which
      --   makes the pair atomic with respect to the other writer.
      --   READERS do not take it: they read one value and may see it a
      --   moment stale, which is inherent to any single read and is not
      --   the inconsistency this closes.
      --
      --   Runtime-only, never persisted: it protects a transient queue
      --   and a transient phase, and a fresh page gets a fresh lock.
    , wsChunkResidencyRef ∷ IORef ChunkOwner
      -- ^ This page's chunk-residency owner (#2001): for every canonical
      --   'World.Chunk.Residency.ChunkKey', whether it is absent,
      --   requested, in flight, or resident — plus the page's own
      --   generation epoch, which every request it mints is tagged with.
      --
      --   It sits beside 'wsTilesRef' and 'wsInitQueueRef' because it is
      --   the identity those two never had. 'wsTilesRef' is keyed by a
      --   bare 'ChunkCoord' with no page and no canonicalisation, and
      --   'wsInitQueueRef' is an ordered work list annotated "for
      --   progress tracking"; neither can answer "is this physical
      --   chunk already being worked on?" without the caller
      --   reconstructing it, which is why that question used to need two
      --   'IORef's read in a documented order (#43). This is ONE value,
      --   so "World.Chunk.Admit"'s verbs each settle it with a single
      --   'atomicModifyIORef''.
      --
      --   Mirrors the tile map by construction: every admission is an
      --   insert into 'wsTilesRef' and every eviction is a removal from
      --   it. Runtime residency bookkeeping, never persisted — a fresh
      --   or loaded page starts with an empty owner and a brand-new
      --   epoch (see docs\/persistence_state_inventory.md).
    , wsMapModeRef ∷ IORef ZoomMapMode
    , wsCursorRef ∷ IORef CursorState
    , wsToolModeRef ∷ IORef ToolMode
    , wsCursorSnapshotRef ∷ IORef CursorSnapshot
    , wsLoadPhaseRef ∷ IORef LoadPhase
    , wsZoomAtlasRef ∷ IORef (Maybe ZoomAtlasInfo)  -- ^ Atlas info once uploaded to GPU
    , wsEditsRef    ∷ IORef WorldEdits
      -- ^ Player edits accumulated this session. Per-chunk so eviction
      --   doesn't lose them — chunks regenerate, edits replay onto the
      --   fresh chunk. Saved verbatim; restored before any chunk
      --   regeneration on load.
    , wsChunkEditGenRef ∷ IORef (HM.HashMap ChunkCoord Word64)
      -- ^ Per-chunk LIVE-EDIT generation for this page: the number of
      --   live terrain/fluid edits the world thread has committed to
      --   that chunk (absent = 0). It is the causal fence that lets the
      --   world thread reject a sim fluid writeback computed BEFORE an
      --   edit it would otherwise overwrite (#1596).
      --
      --   Why a fence is needed at all: 'World.Command.Types.WorldApplyFluids'
      --   and every edit command share the WORLD queue, but the re-seed
      --   an edit sends the sim ('World.Thread.Command.Edit.Sync.syncEditToSim')
      --   goes down the INDEPENDENT sim queue with no acknowledgement.
      --   Nothing orders the two, so a batch the sim computed from the
      --   pre-edit chunk can land behind the edit and
      --   'World.Thread.Command.applyOneWriteback' would replace the
      --   chunk's whole fluid/terrain-surface/rendered-surface/side-deco
      --   set with the state the player just changed.
      --
      --   The protocol is one number travelling a full round trip:
      --   'syncEditToSim' bumps this entry and stamps the new value into
      --   'Sim.Command.Types.SimChunkEdited'; the sim stores it as
      --   'Sim.State.Types.scsEditGen' when it re-seeds and stamps every
      --   writeback it later produces for that chunk with
      --   'World.Command.Types.fwEditGen'; the world applies a writeback
      --   only when the stamp EQUALS this entry. Equality (not @>=@) is
      --   deliberate — it also rejects a writeback claiming a generation
      --   this page never issued, which is what a batch in flight across
      --   a chunk eviction looks like.
      --
      --   Scoped per chunk, so an edit to one chunk never drops a
      --   writeback for another. Entries are DELETED when the chunk is
      --   evicted ("World.Thread.ChunkLoading"), matching the fresh
      --   'SimChunkLoaded' the sim gets for the reloaded chunk, which
      --   re-seeds 'scsEditGen' to 0. Transient session bookkeeping:
      --   never saved, never loaded, and a fresh page starts empty (see
      --   docs/persistence_state_inventory.md).
    , wsOreSurveyRef ∷ IORef (HM.HashMap ChunkCoord ([WorldEdit], Text))
      -- ^ Memo for the zoom-map Resources survey of UNLOADED chunks
      --   (transient generation is ~10–300 ms; reselecting shouldn't
      --   repeat it). Each entry stores the chunk's edit list at
      --   compute time — a lookup only hits when the current edit
      --   list is identical, so edits self-invalidate. Wholesale
      --   flush at 256 entries. Loaded chunks never consult this.
    , wsMineDesignationsRef ∷ IORef MineDesignations
      -- ^ Mine-designation set: tile (gx, gy) → designation (surface
      --   z + corner dig progress; see World.Mine.Types). Written by
      --   the world thread (WorldDesignateMine / dig commands), read
      --   by the render pass and the dig AI. Persisted in saves
      --   (sdMineDesignations, v31) including mid-dig corners.
    , wsGroundItemsRef ∷ IORef GroundItems
      -- ^ Items lying in the world (see Item.Ground — float x/y,
      --   height derived from current terrain at render). Persisted
      --   in saves (sdGroundItems, v32).
    , wsSpoilRef ∷ IORef SpoilPiles
      -- ^ Spoil mounds from digging, keyed by tile vertex (see
      --   World.Spoil.Types). Written by the world thread's dig
      --   handler; read by the spoil render pass. Persisted in
      --   saves (sdSpoilPiles, v34).
    , wsStructureStageRef ∷ IORef StructureStage
      -- ^ Lua-thread write-ahead staging for THIS world's structure
      --   placements. The authoritative structure state is the per-chunk
      --   'lcStructures' overlay (rendered + persisted), but those writes
      --   apply asynchronously on the world thread via WeSetStructure. The
      --   builder (scripts/structures.lua, scripts/locations.lua) places a
      --   piece then queries it within the SAME Lua call (floors→posts→
      --   walls), so structure.place records it here and structure.floorZAt/
      --   hasAt consult it before falling back to lcStructures —
      --   read-your-writes without a second authority. Per-world so it can't
      --   leak across worlds (it dies with the WorldState, and a reloaded
      --   world gets a fresh empty one); never saved.
      --
      --   #1674: each staged entry carries the token of the
      --   'structure.place' attempt that wrote it, and the queued
      --   WorldSetStructure carries the same token, so the world thread
      --   can undo exactly the attempt it declines (an unloaded target
      --   chunk) without touching a newer placement at the same key.
    , wsConstructDesignationsRef ∷ IORef ConstructDesignations
      -- ^ Construction-designation set: tile (gx, gy) → designation
      --   (surface z, build target, status, progress; see
      --   World.Construct.Types). Written by the world thread
      --   (WorldDesignateConstruct / cancel / set-status commands), read
      --   by the render pass and the build AI (#96). The render pass
      --   draws each target as the thing itself — a STRUCTURE as the
      --   piece's own art (#1846), a BUILDING as one sprite of its own
      --   definition at the z its stake will land on (#1845) — and
      --   reads this map for a second reason besides drawing it: a wire
      --   ghost's connection variant counts designated neighbours
      --   (D-22).
      --   Persisted in saves (wpsConstructDesignations).
    , wsConstructAttemptRef ∷ IORef ConstructAttemptId
      -- ^ This page's construction ATTEMPT allocator (#1844): the id the
      --   next designation admitted here will take.
      --
      --   It only ever advances — a cancellation, a completion, a
      --   self-clearing load and a whole-page sweep all leave it where
      --   it is — so an id that named a removed attempt can never come
      --   to name a live one while delayed work for the first may still
      --   be in flight. That is the entire point of the identity; see
      --   "World.Construct.Attempt".
      --
      --   Durable, and persisted beside the designations it hands ids to
      --   (the @world-activity@ page slice), because a designation
      --   created after a load must not be able to collide with one the
      --   save already holds.
    , wsFloraHarvestsRef ∷ IORef FloraHarvests
      -- ^ Harvested flora (#94): flora INSTANCE id → regrowth
      --   game-seconds remaining (#1854 re-keyed this off the tile, so
      --   one berry bush's timer no longer depletes every harvestable
      --   co-tenant beside it). World-level (NOT in lcFlora) so chunk
      --   eviction can't wipe it; written by world.harvestFlora + the
      --   regrowth tick, read by the flora render pass and the foraging
      --   AI's queries. Persisted in saves (wpsFloraHarvests).
    , wsChopDesignationsRef ∷ IORef ChopDesignations
      -- ^ Chop-designation set (#97): flora INSTANCE id → designation
      --   (surface z + the plant's canonical tile; see
      --   World.Chop.Types). #1854 re-keyed this off the tile too, so
      --   designating one of two trees on a tile marks exactly one.
      --   Written by the world thread (#1856's exact-instance
      --   WorldDesignateChopInstances / WorldEraseChopInstances, and
      --   the AI's WorldCancelChop) THROUGH "World.Flora.Designation",
      --   which is the one operation that keeps this map and the loaded
      --   'fiChopDesignated' mirror in step; read by the render pass
      --   (marker) and the chop AI. Persisted in saves
      --   (wpsChopDesignations).
    , wsPendingChopMigrationRef ∷ IORef PendingChopDesignations
      -- ^ Pre-#1854 tile-keyed chop designations read out of a save
      --   whose chunk was not loaded, so no instance could be resolved
      --   yet. Deferred, NEVER authoritative: no designation, marker,
      --   claim or harvest query may be answered from it, and
      --   "World.Flora.Designation" drains an entry the moment its
      --   chunk is admitted. Persisted (wpsPendingChopMigration) so
      --   repeated save/load cannot quietly discard it.
    , wsPendingFloraHarvestsRef ∷ IORef PendingFloraHarvests
      -- ^ Pre-#1854 tile-keyed regrowth timers awaiting their chunk,
      --   on exactly the terms above. Drained by expanding one legacy
      --   tile timer onto every harvestable instance on that tile with
      --   the same remaining time — the observable behaviour the
      --   tile-keyed map used to produce. Persisted
      --   (wpsPendingFloraHarvests).
    , wsPlantedFloraCursorRef ∷ IORef Word64
      -- ^ This page's planted-flora id allocator (#1854 requirement 5).
      --   Strictly above every planted 'FloraInstanceId' the page has
      --   ever issued, so a plant after a load can never reuse a live
      --   id. Persisted (wpsPlantedFloraCursor) in @world-edits@ v2
      --   beside the WePlaceFlora edits whose ids it accounts for.
    , wsCraftBillsRef ∷ IORef CraftBills
      -- ^ Craft-bill queue (#329): per-station standing craft orders
      --   (see Craft.Bills). Unlike the designation layers this has no
      --   world-thread side effects (no ghost render, no chunk edits),
      --   so the craft.* bill verbs mutate it directly from the Lua
      --   thread with atomicModifyIORef' — claims resolve atomically
      --   without a queue round-trip. Persisted in saves
      --   (wpsCraftBills, v70).
    , wsTransferOrdersRef ∷ IORef TransferOrders
      -- ^ Durable transfer orders (#1246, epic #1013): this page's queue
      --   of standing "move these exact item instances from this
      --   endpoint to that one" orders — the acting unit, the endpoint
      --   pair, and every requested item's own lifecycle state (see
      --   Unit.Transfer.Orders), plus the page-local id allocator, which
      --   lives inside the record so a load cannot mint a colliding id.
      --   Lives here, not on EngineEnv, for exactly the reason craft
      --   bills and container knowledge do: it is per-page gameplay
      --   state, so a destroyed page takes its orders with it. Like
      --   those two it has no world-thread side effects, so a mutation
      --   is one atomicModifyIORef' from whichever thread owns the verb.
      --   Persisted in saves (wpsTransferOrders, v93) as its own
      --   OPTIONAL "transfer-orders" component.
    , wsPowerNodesRef ∷ IORef PowerNodes
      -- ^ Power-node registry (#358): placed solar-panel/battery source
      --   and storage nodes (role + peak watts / capacity Wh), keyed by
      --   their own id and referencing the BuildingId they ride on. Like
      --   craft bills, no world-thread side effects, so the power.*
      --   verbs mutate it directly with atomicModifyIORef'. Persisted in
      --   saves (wpsPowerNodes, v73).
    , wsContainerKnowledgeRef ∷ IORef ContainerKnowledge
      -- ^ The player's remembered view of what each container on this
      --   page holds (#1087, epic #1013): last-known contents + their
      --   derived weight + when it was observed, keyed by BuildingId.
      --   PLAYER-GLOBAL, never per-unit (epic decision 2) and never
      --   live — the live truth is biStorage on the building itself,
      --   and this deliberately goes stale until an interaction reveals
      --   it again (see Building.Knowledge). Lives here, not on
      --   EngineEnv: it is per-page gameplay state, exactly like craft
      --   bills and power nodes, so a destroyed or replaced page takes
      --   its memories with it. Written by the reveal triggers in
      --   Building.Knowledge.Live from the Lua thread with
      --   atomicModifyIORef' (like craft bills, it has no world-thread
      --   side effects). Persisted in saves (wpsContainerKnowledge) as
      --   its own optional "container-knowledge" component.
    , wsPendingContainerSeedsRef ∷ IORef (HS.HashSet BuildingId)
      -- ^ Containers on this page that were PLACED this session and
      --   have not yet reached Built (#1087). Only the instant-built
      --   storage class ever lands here: a worker-built one seeds from
      --   its own build-progress crossing, while a zero-build-work def
      --   reaches Built on currentActivity's time-based arm, which no
      --   tick otherwise observes. The building drain re-evaluates each
      --   entry every tick and seeds the known-empty record the moment
      --   the transition actually happens (see Building.Knowledge.Live).
      --   Deliberately NOT persisted, and NOT derivable: it is precisely
      --   "the player watched THIS one go up in THIS session", which is
      --   what keeps a loaded already-built container from masquerading
      --   as a new construction event. A restored page starts with an
      --   empty set, exactly right — nothing in it was just placed.
    , wsTillDesignationsRef ∷ IORef TillDesignations
      -- ^ Till-designation set (#333): tile (gx, gy) → designation
      --   (surface z; see World.Till.Types). Written by the world
      --   thread (WorldDesignateTill / cancel commands), read by the
      --   render pass (marker) and the till AI. Persisted in saves
      --   (wpsTillDesignations, v76).
    , wsCropPlotsRef ∷ IORef CropPlots
      -- ^ Planted groundcover-crop set (#334): tile (gx, gy) → plot
      --   (species + planted day + health; see World.Flora.CropPlot).
      --   World-level (NOT in chunk data, which is transient) so
      --   eviction can't wipe it; written by world.plantCropAt, read
      --   by the render pass (tile-fill texture) and world.harvestFlora
      --   / world.getCropPlotAt. Persisted in saves (wpsCropPlots, v77).
    , wsPlantDesignationsRef ∷ IORef PlantDesignations
      -- ^ Plant-designation set (#335): tile (gx, gy) → designation
      --   (surface z + chosen crop; see World.Plant.Types). Written by
      --   the world thread — the WorldDesignatePlant / cancel commands
      --   AND, since #1858, 'World.Plant.Validate''s invalidation sweep,
      --   which is a third writer and removes any record whose tile is
      --   resident and no longer tilled soil. Read by the render pass
      --   (marker, which draws only records whose chunk is resident) and
      --   the farm AI (#336, which releases its claim and job the tick a
      --   record disappears). Persisted in saves (wpsPlantDesignations),
      --   and reconciled against the terrain each load publishes.
    , wsBloodStoreRef ∷ IORef BloodStore
      -- ^ Blood decal model (#604): generated-texture FIFO pool +
      --   world decal placements (see Blood.Types). Like
      --   wsStructureStageRef, per-world so it can't leak across
      --   worlds and dies with the WorldState; a reloaded world gets a
      --   fresh empty store. Never saved — blood is transient BY
      --   DESIGN, an epic-wide (#603) deliberate contract covering
      --   every landed piece (impact marks, trails, pooling), not a
      --   #604-only scope limit; see docs/blood_decals.md's
      --   "Transience" section and closed issue #884.
    , wsBloodTextureHandlesRef ∷ IORef BloodTextureHandles
      -- ^ GPU-upload state for #606's procedurally generated blood
      --   textures: which 'BloodTextureId's currently have a live
      --   bindless 'TextureHandle', plus that texture's own GPU
      --   cleanup action. Populated/pruned once per frame by
      --   'World.Render.BloodQuads.uploadBloodTextures' — reading
      --   'wsBloodStoreRef's FIFO to upload anything new and unregister
      --   anything evicted — since the FIFO itself is mutated from the
      --   Lua/world thread with no GPU access of its own. When a world
      --   page is removed or replaced (destroy / destroy-all / init /
      --   arena / save-load), the world thread hands THIS 'IORef' to the
      --   render thread via 'Engine.Core.State.bloodDisposeQueue' (#788),
      --   which disposes every remaining handle — bindless unregister,
      --   'textureSizeRef' removal, image/view cleanup — and empties the
      --   map, so a destroyed page's GPU textures are reclaimed rather
      --   than leaked. Not persisted: a reloaded world uploads fresh.
    , wsIdentityRef ∷ IORef (Maybe WorldIdentity)
      -- ^ The page's optional player-facing identity (#707): display
      --   name + optional gloss, distinct from the routing page id and
      --   from any save-slot name. Written exactly once — at creation
      --   ('WorldInit') or save-load restore ('wpsIdentity') — and
      --   never mutated afterward (no rename API in this phase).
      --   Persisted per page in saves (wpsIdentity, v82).
    , wsGeneratedIdRef ∷ IORef GeneratedWorldId
      -- ^ #2021: this page's opaque 'GeneratedWorldId' — which
      --   GENERATED FOUNDATION the page descends from, distinct from
      --   every identifier beside it: the routing 'WorldPageId' (which a
      --   later session may reuse for a different world), the
      --   player-facing 'wsIdentityRef' (which is display text a player
      --   chooses), the save-slot name, and the file path.
      --
      --   Non-optional, because 'emptyWorldState' mints one: every live
      --   page carries exactly one id from the instant it exists, so no
      --   creation path — 'WorldInit', arena init, or load staging — can
      --   forget to assign one, and no consumer has an absent case to
      --   handle. Staging OVERWRITES it with the saved id when the save
      --   carries one (@world-pages@ v9 and later); a page restored
      --   from a pre-v9 save simply keeps the fresh id minted here, which is
      --   requirement 7's "assigned a fresh id during transactional load
      --   staging, not derived from anything in the legacy save".
      --
      --   Persisted per page in @world-pages@ (authoritative, since
      --   v9) and copied into the @"metadata"@ component at v3 so a
      --   @listSaves@-depth read can obtain it without decoding any
      --   gameplay component.
    }

emptyWorldState ∷ IO WorldState
emptyWorldState = do
    tilesRef     ← newIORef emptyWorldTileData
    cameraRef    ← newIORef (WorldCamera 0 0)
    texturesRef  ← newIORef defaultWorldTextures
    genParamsRef ← newIORef Nothing
    timeRef      ← newIORef defaultWorldTime
    dateRef      ← newIORef defaultWorldDate
    timeScaleRef ← newIORef 1.0   -- 1 game-minute per real-second
    resumeScaleRef ← newIORef Nothing
    zoomCacheRef ← newIORef V.empty
    quadCacheRef  ← newIORef Nothing
    quadCacheGenRef ← newIORef 0
    zoomQCRef   ← newIORef Nothing
    bgQCRef     ← newIORef Nothing
    bakedZoomRef ← newIORef (V.empty, defaultWorldTextures, FaceSouth)
    bakedBgRef   ← newIORef (V.empty, defaultWorldTextures, FaceSouth)
    wsInitQueueRef ← newIORef []
    wsInitQueueLock ← newMVar ()
    -- A brand-new epoch per WorldState, so a page id reused by a
    -- reinit, an arena replacement or a load republish is a DIFFERENT
    -- generation (#2001). Every one of those builds a fresh WorldState
    -- through this function, which is why allocating here is enough.
    wsChunkResidencyRef ← newIORef ∘ emptyChunkOwner =≪ newChunkGeneration
    wsMapModeRef ← newIORef ZMDefault
    wsCursorRef ← newIORef emptyCursorState
    wsToolModeRef ← newIORef DefaultTool
    wsCursorSnapshotRef ← newIORef emptyCursorSnapshot
    wsLoadPhaseRef ← newIORef LoadIdle
    wsZoomAtlasRef ← newIORef Nothing
    wsEditsRef     ← newIORef emptyWorldEdits
    wsChunkEditGenRef ← newIORef HM.empty
    wsOreSurveyRef ← newIORef HM.empty
    wsMineDesignationsRef ← newIORef HM.empty
    wsGroundItemsRef ← newIORef emptyGroundItems
    wsSpoilRef ← newIORef emptySpoilPiles
    wsStructureStageRef ← newIORef emptyStructureStage
    wsConstructDesignationsRef ← newIORef HM.empty
    wsConstructAttemptRef ← newIORef firstConstructAttemptId
    wsFloraHarvestsRef ← newIORef emptyFloraHarvests
    wsChopDesignationsRef ← newIORef HM.empty
    wsPendingChopMigrationRef ← newIORef HM.empty
    wsPendingFloraHarvestsRef ← newIORef emptyPendingFloraHarvests
    wsPlantedFloraCursorRef ← newIORef firstPlantedFloraCursor
    wsCraftBillsRef ← newIORef emptyCraftBills
    wsTransferOrdersRef ← newIORef emptyTransferOrders
    wsPowerNodesRef ← newIORef emptyPowerNodes
    wsContainerKnowledgeRef ← newIORef emptyContainerKnowledge
    wsPendingContainerSeedsRef ← newIORef HS.empty
    wsTillDesignationsRef ← newIORef HM.empty
    wsCropPlotsRef ← newIORef emptyCropPlots
    wsPlantDesignationsRef ← newIORef HM.empty
    wsBloodStoreRef ← newIORef (emptyBloodStore defaultBloodTextureCap)
    wsBloodTextureHandlesRef ← newIORef HM.empty
    wsIdentityRef ← newIORef Nothing
    -- #2021: minted here rather than at any individual creation site, so
    -- "every persistable generated page carries exactly one id" holds by
    -- construction across WorldInit, arena init and load staging alike.
    -- Staging replaces it when the save carries one.
    freshGeneratedId ← newGeneratedWorldId
    wsGeneratedIdRef ← newIORef freshGeneratedId
    return $ WorldState tilesRef cameraRef texturesRef genParamsRef
                        timeRef dateRef timeScaleRef resumeScaleRef
                        zoomCacheRef
                        quadCacheRef quadCacheGenRef zoomQCRef bgQCRef
                        bakedZoomRef bakedBgRef wsInitQueueRef
                        wsInitQueueLock
                        wsChunkResidencyRef
                        wsMapModeRef
                        wsCursorRef wsToolModeRef wsCursorSnapshotRef
                        wsLoadPhaseRef wsZoomAtlasRef wsEditsRef
                        wsChunkEditGenRef
                        wsOreSurveyRef wsMineDesignationsRef
                        wsGroundItemsRef wsSpoilRef wsStructureStageRef
                        wsConstructDesignationsRef wsConstructAttemptRef
                        wsFloraHarvestsRef
                        wsChopDesignationsRef
                        wsPendingChopMigrationRef
                        wsPendingFloraHarvestsRef
                        wsPlantedFloraCursorRef
                        wsCraftBillsRef
                        wsTransferOrdersRef
                        wsPowerNodesRef wsContainerKnowledgeRef
                        wsPendingContainerSeedsRef
                        wsTillDesignationsRef
                        wsCropPlotsRef wsPlantDesignationsRef
                        wsBloodStoreRef wsBloodTextureHandlesRef
                        wsIdentityRef wsGeneratedIdRef

-- | The world size (in chunks) that decides this page's u-wrap — the
--   single input every canonical-tile-frame helper needs (#1175).
--
--   Zero (no wrapping) when the page has no gen params yet, which is the
--   only honest answer: "World.Thread.ChunkLoading" bails out on the
--   same @Nothing@ and therefore has not inserted — nor wrapped the key
--   of — a single chunk. Defaulting to a nominal 128 the way the render
--   passes do would wrap STORAGE keys the loader never wrapped.
pageWrapWorldSize ∷ WorldState → IO Int
pageWrapWorldSize ws = maybe 0 wgpWorldSize <$> readIORef (wsGenParamsRef ws)

-- | This page's seam topology as the fluid simulation needs it (#2044),
--   which every 'Sim.Command.Types.SimCommand' that seeds or activates a
--   world carries.
--
--   'Sim.Topology.SimFlatTopology' when the page has no gen params yet,
--   for exactly 'pageWrapWorldSize'\'s reason: nothing has been
--   generated, so no chunk key has been wrapped either. Unlike that
--   helper this one also recognises an ARENA, whose 'wgpWorldSize' is a
--   sentinel rather than an extent.
pageSimTopology ∷ WorldState → IO SimTopology
pageSimTopology ws =
    maybe SimFlatTopology simTopologyForParams <$> readIORef (wsGenParamsRef ws)

-- | Invalidate a world's cached render quads in a thread-safe way.
--   Bumps the generation counter atomically rather than nulling
--   'wsQuadCacheRef', so an invalidation from the world/Lua thread can
--   never be lost to the render thread's read-rebuild-write of the cache:
--   the render thread stamps each rebuilt cache with the generation it
--   observed, and a cache whose 'wqcGen' no longer matches is treated as
--   stale and rebuilt next frame.
bumpQuadCacheGen ∷ WorldState → IO ()
bumpQuadCacheGen ws =
    atomicModifyIORef' (wsQuadCacheGenRef ws) (\g → (g + 1, ()))

data WorldManager = WorldManager
    { wmWorlds  ∷ [(WorldPageId, WorldState)]
    , wmVisible ∷ [WorldPageId]
    , wmSelectionGen ∷ !Word64
      -- ^ Monotonic page-SELECTION generation (#1602). Bumped by every
      --   mutation that can change which page 'resolveActiveWorld'
      --   answers with — a show that actually prepends, a hide that
      --   actually removes, a page destroy, a destroy-all, and a load's
      --   whole-session republish. A caller that resolves the manager
      --   ONCE can carry this number away as a freshness token and later
      --   prove the selection has not moved since: equality is exact
      --   page binding, not a "the id still matches" guess, so an
      --   A→B→A sequence back to the same page id reads as STALE
      --   (which a page-id comparison cannot see).
      --
      --   Monotonic across a transactional load as well
      --   ('World.Load.Publish.publishStagedSession' seeds the
      --   replacement manager from the outgoing one rather than from 0)
      --   — a load REPLACES the page set, so a binding captured before
      --   it must never read fresh after it.
      --
      --   NOT persisted: it is a within-session freshness counter, and a
      --   restored session's bindings are all invalidated by the bump
      --   the publish itself performs.
    , wmProjectedGen ∷ !Word64
      -- ^ The selection generation as of every request that has been
      --   MADE, whether or not the world thread has applied it yet
      --   (#1602). 'wmSelectionGen' above only moves when a change
      --   lands; this moves when one is asked for, so a synchronous
      --   caller can tell "a change is already on its way" from "nothing
      --   is happening" — which is the difference between honestly
      --   refusing a placement and accepting one the world thread is
      --   about to drop.
      --
      --   It counts only EFFECTIVE requests: showing an already-visible
      --   page, or hiding an already-hidden one, changes no selection
      --   and must not invalidate a live binding (a redundant
      --   @world.show@ is ordinary traffic). Judged against the applied
      --   state, which is exact when nothing is in flight — and when
      --   something IS in flight the binding is already stale by this
      --   field, so a coarser answer there costs nothing.
      --
      --   Re-synchronised to 'wmSelectionGen' by
      --   'settleSelectionProjection' whenever the queue drains, so a
      --   request the handler ends up refusing (a @world.show@ for a
      --   page that does not exist) cannot leave the two permanently
      --   apart.
    , wmProjectedWorlds ∷ ![WorldPageId]
      -- ^ Which page ids will be REGISTERED once every queued selection
      --   command has applied (#1602) — the companion to
      --   'wmProjectedVisible', and needed for the same reason.
      --
      --   @world.show@ refuses a page that is not in 'wmWorlds', so
      --   predicting its effect from the visible list alone calls a show
      --   of a missing page a real change and rejects a click for a
      --   command that will do nothing. A queued @world.init@ ahead of
      --   it flips that back, which is exactly why this has to be
      --   projected rather than read off the applied set.
    , wmProjectedVisible ∷ ![WorldPageId]
      -- ^ What 'wmVisible' will be once every queued selection command
      --   has been applied (#1602). Requests are judged against THIS,
      --   not against the applied list: a queued @world.hide B@ is
      --   ineffective against an applied @[A]@ yet very much effective
      --   behind a queued @world.show B@, and reading the applied list
      --   there would call the pair a no-op and then drop the placement
      --   at the commit.
      --
      --   Re-synchronised to 'wmVisible' by 'settleSelectionProjection'
      --   whenever the queue drains, so a request the handler ends up
      --   refusing (a @world.show@ for a page that does not exist)
      --   cannot leave the projection permanently wrong. While it IS
      --   wrong, it is wrong in the safe direction: an over-predicted
      --   change reads as stale.
    , wmSelectionPending ∷ !Int
      -- ^ How many selection-changing commands have been REQUESTED but
      --   not yet applied (#1602). The generation above moves when the
      --   world thread applies a change; this moves when another thread
      --   asks for one, and closes the window between the two.
      --
      --   Without it, a @world.hide@ sitting unapplied in the world
      --   queue is invisible to a caller reading this record: the
      --   synchronous check a click needs would answer "still fresh"
      --   while the world thread was about to (correctly) reject the
      --   very same placement, leaving the build tool having recorded an
      --   acceptance for something that never landed. A non-zero count
      --   means "selection is in flight", and every binding reads as
      --   stale until it settles — conservative in the safe direction,
      --   and only for the one world-thread tick the change takes.
      --
      --   Robust rather than exactly balanced: a decrement clamps at
      --   zero (the load's restore drives 'handleWorldShowCommand'
      --   directly, with no matching request), and
      --   'World.Load.Publish.publishStagedSession' resets it outright,
      --   which is also what covers the one path that DISCARDS queued
      --   commands ('World.Thread.processAuthorizedSave').
    , wmSessionTeardown ∷ !Bool
      -- ^ True from the moment 'World.Thread.Command.Basic'\'s
      --   @handleWorldDestroyAllCommand@ empties the page set until the
      --   Exit-to-Menu teardown it queued has actually COMPLETED
      --   (#2291) — that is, until the unit thread has drained
      --   @UnitClearAll@ and @BuildingClearAll@ and run
      --   'Unit.Thread.endSessionEpoch', which clears this flag as its
      --   last act.
      --
      --   It exists because the teardown spans two threads while the
      --   NEXT session is created on only one of them. The world thread
      --   registers pages and, since #1602, commits bound building
      --   placements itself rather than through the building queue, so
      --   the queue markers that order the boundary against those two
      --   queues say nothing about it. Without this flag a @world.init@
      --   drained in the same window could register a page, a bound
      --   placement could stamp @biSpawnedAt@ from the OUTGOING clock,
      --   and the reset would then move that clock backwards underneath
      --   the new session's own record.
      --
      --   'World.Thread.processAllCommands' is the only reader: while
      --   this is set it stops the drain at a page-registering command
      --   rather than running it, leaving it and everything behind it
      --   queued in order for a later tick. Nothing else is deferred,
      --   and nothing is discarded — the boundary costs at most the one
      --   unit tick the teardown needs.
      --
      --   A load is not fenced by it and does not need to be: a load
      --   publish discards both queue markers with the rest of the unit
      --   and building queues ('World.Load.Publish.discardStaleQueues'),
      --   so no teardown remains to complete, and the replacement
      --   manager it installs starts with this False.
      --
      --   NOT persisted: it names an in-flight cross-thread transition,
      --   and a save taken mid-teardown restores into a process where
      --   that transition does not exist.
    }

emptyWorldManager ∷ WorldManager
emptyWorldManager = WorldManager
    { wmWorlds  = []
    , wmVisible = []
    , wmSelectionGen = 0
    , wmProjectedGen = 0
    , wmProjectedWorlds = []
    , wmProjectedVisible = []
    , wmSelectionPending = 0
    , wmSessionTeardown = False
    }

-- | Advance the page-selection generation (#1602). Call inside the SAME
--   'atomicModifyIORef'' that changes 'wmVisible' / 'wmWorlds', so a
--   reader can never observe a moved selection under an unmoved
--   generation.
bumpSelectionGen ∷ WorldManager → WorldManager
bumpSelectionGen mgr = mgr { wmSelectionGen = wmSelectionGen mgr + 1 }

-- | Record that a selection-changing command has been ENQUEUED (#1602).
--   Call it in the same atomic step as the enqueue, from whichever
--   thread is asking, so no reader can see the request land without
--   seeing the state move.
--
--   @effective@ says whether the request will actually move the
--   selection; only an effective one advances 'wmProjectedGen' and so
--   invalidates live bindings. The outstanding COUNT moves either way —
--   it is what pairs a request with its handler, and must stay exactly
--   balanced regardless of what the request turns out to do.
requestSelectionChange
    ∷ Bool → ([WorldPageId], [WorldPageId]) → WorldManager → WorldManager
requestSelectionChange effective (worlds, visible) mgr = mgr
    { wmSelectionPending = wmSelectionPending mgr + 1
    , wmProjectedGen = wmProjectedGen mgr + (if effective then 1 else 0)
    , wmProjectedWorlds = worlds
    , wmProjectedVisible = visible
    }

-- | Record that one such command has now been APPLIED. Clamped at zero:
--   the same handlers are also driven directly by the load's restore,
--   which never went through a request.
completeSelectionChange ∷ WorldManager → WorldManager
completeSelectionChange mgr =
    mgr { wmSelectionPending = max 0 (wmSelectionPending mgr - 1) }

-- | Once nothing is outstanding, the projection IS the applied state
--   (#1602). Run after each world-thread command so a request whose
--   predicted effect never materialised — a @world.show@ the handler
--   refused because the page does not exist — cannot leave
--   'wmProjectedGen' permanently ahead and every binding permanently
--   stale.
settleSelectionProjection ∷ WorldManager → WorldManager
settleSelectionProjection mgr
    | wmSelectionPending mgr ≡ 0 = mgr
        { wmProjectedGen     = wmSelectionGen mgr
        , wmProjectedWorlds  = map fst (wmWorlds mgr)
        , wmProjectedVisible = wmVisible mgr }
    | otherwise                  = mgr

-- | 'wmProjectedVisible' after one more queued command (#1602). The
--   projection mirrors what each handler does to the visible list, so a
--   dependent sequence — show a page, then hide it again — is judged in
--   the order it will actually be applied rather than against a snapshot
--   that predates both.
--
--   Kinds that move the selection GENERATION without touching the
--   visible list (a page re-init replaces its state; a load publish
--   replaces the whole set) leave the list alone here; their
--   effectiveness is decided separately by the caller.
projectSelectionVisible
    ∷ WorldPageId → Bool → [WorldPageId] → [WorldPageId]
projectSelectionVisible pid shown projected
    | shown     = if pid `elem` projected then projected else pid : projected
    | otherwise = filter (≢ pid) projected

-- | The one page a placement binding can ever name (#1602): the head of
--   the visible list, which is what 'resolveActiveWorld' answers with
--   and what @world.pickTile@ hit-tests.
--
--   'wmVisible' is a LIST — several pages can be visible at once, and
--   @world.show@ prepends — so "the visible set changed" and "the page a
--   binding depends on changed" are different questions. Only the second
--   may invalidate a placement: hiding, destroying or rebuilding a
--   visible page that is not the head leaves every binding untouched.
selectionHead ∷ [WorldPageId] → Maybe WorldPageId
selectionHead = listToMaybe

-- | Is a live placement binding invalidated by the current selection
--   state (#1602)? Either the selection has already moved since the
--   binding was captured, or an effective change has been requested and
--   is still on its way.
selectionMovedSince ∷ Word64 → WorldManager → Bool
selectionMovedSince captured mgr =
    captured ≢ wmSelectionGen mgr ∨ selectionChangeInFlight mgr

-- | Is an EFFECTIVE selection change requested but not yet applied
--   (#1602)? Derived rather than read straight off 'wmProjectedGen', so
--   a manager written directly — a test fixture, or any code that
--   installs a page set wholesale — is consistent without having to know
--   about the projection at all: with nothing outstanding there is by
--   definition nothing in flight.
selectionChangeInFlight ∷ WorldManager → Bool
selectionChangeInFlight mgr =
    wmSelectionPending mgr > 0 ∧ wmProjectedGen mgr ≢ wmSelectionGen mgr

-- | The projected visible list — the applied one whenever nothing is
--   outstanding, for the same reason 'selectionChangeInFlight' derives.
projectedVisible ∷ WorldManager → ([WorldPageId], [WorldPageId])
projectedVisible mgr
    | wmSelectionPending mgr ≡ 0 = (map fst (wmWorlds mgr), wmVisible mgr)
    | otherwise = (wmProjectedWorlds mgr, wmProjectedVisible mgr)
-- | The page whose clock is "the world clock" for pause and save
--   purposes: the head of @wmVisible@ that is still a live page.
--
--   'Nothing' when there is no visible page at all — a main menu, a
--   mid-transition session, or (defensively) an autosave scheduler that
--   somehow fired outside a gameplay view.
--
--   ONE resolver, because its callers have to agree about which page
--   they are talking about or the pause\/clock pair they maintain drifts
--   apart: "World.Pause" captures and zeroes this page's clock at the
--   start of a pause epoch, and @engine.saveWorld@'s acceptance
--   ('Engine.Scripting.Lua.API.Save.acceptSaveRequest') reads the same
--   page's scale into its 'World.Save.Types.AutosaveRequest'.
--   "World.Thread.Command.Save.WriteWorld" spells the same rule out on
--   @wmVisible@ directly for its snapshot's own camera\/clock
--   attribution; that is the one copy.
--
--   It lives here, on the record it projects, so nothing has to reach
--   through an @EngineEnv@ to ask (issue #985's reason for the module
--   this moved out of).
visiblePage ∷ WorldManager → Maybe (WorldPageId, WorldState)
visiblePage mgr = case wmVisible mgr of
    (vid:_) → (\ws → (vid, ws)) <$> lookup vid (wmWorlds mgr)
    _       → Nothing

-- | 'visiblePage' for a caller that needs only the state.
visiblePageState ∷ WorldManager → Maybe WorldState
visiblePageState = fmap snd ∘ visiblePage

-- | The page-scoped language-provenance query (#1092 requirement 5):
--   which generated language named this page, and under which
--   generator version — enough for later work to rebuild the profile
--   ('Language.Generated.Profile.generateProfile') without reaching
--   into save internals. Seed and version come back as ONE value, so a
--   caller can never see half a provenance.
--
--   'Nothing' for every case that genuinely has no language: a missing
--   page, an unnamed page (an arena, a 4-argument @world.init@), a
--   custom-named page, and a page restored from a save written before
--   provenance was recorded. Nothing is inferred for any of them.
pageLanguageProvenance
    ∷ WorldManager → WorldPageId → IO (Maybe LanguageProvenance)
pageLanguageProvenance wm pid = case lookup pid (wmWorlds wm) of
    Nothing → pure Nothing
    Just ws → (⌦ wiLanguage) ⊚ readIORef (wsIdentityRef ws)

-- | Snapshot of the cursor selection state, used to detect changes
--   and avoid re-sending HUD info every frame.
data CursorSnapshot = CursorSnapshot
    { csZoomSel  ∷ !(Maybe (Int, Int))      -- ^ zoomSelectedPos last sent
    , csWorldSel ∷ !(Maybe (Int, Int, Int))  -- ^ worldSelectedTile last sent
    } deriving (Eq, Show)

emptyCursorSnapshot ∷ CursorSnapshot
emptyCursorSnapshot = CursorSnapshot Nothing Nothing

-- | Tracks overall world loading progress across both phases.
-- Phase 1: synchronous setup (timeline, ocean, climate, zoom cache, preview)
-- Phase 2: chunk generation (init queue draining)
data LoadPhase
    = LoadIdle
    | LoadPhase1 !Int !Int    -- ^ (currentStep, totalSteps) for setup work
    | LoadPhase2 !Int !Int    -- ^ (remaining, total) for chunk generation
    | LoadDone
    deriving (Eq, Show)
