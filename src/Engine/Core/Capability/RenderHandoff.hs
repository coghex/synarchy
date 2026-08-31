{-# LANGUAGE UnicodeSyntax #-}
-- | The coupled __render-handoff__ half of the
--   @world-sim-render-handoff@ capability (epic #537, issue #894 — E5b):
--   the fields 'docs/engineenv_capability_inventory.md' §7.4 held
--   back from E5a (#893) because their consumers straddle this group and
--   @render-gpu-asset@ — the world thread's staging surface for
--   @MainRender@ GPU uploads, plus the structure-palette translation
--   table and (since #1712) the structure packs' directional wall art.
--
--   Follows the capability-record convention
--   ('docs/engineenv_capability_inventory.md' SS2.1 is its one
--   authoritative statement, not restated here); this record's own
--   field prefix is @rh@.
--
--   == Producer \/ consumer \/ clearing contracts (§5)
--
--   This record grants no new read or write authority, changes no
--   handoff semantics, no upload cadence, no staleness policy and no
--   disposal ordering — it only removes the ability to reach fields a
--   render-handoff consumer has no business touching. The three
--   distinct __lifecycles__ §5 records for these fields survive
--   the projection unchanged, and the clearing contract differs per
--   lifecycle, so each field restates its own below rather than
--   inheriting a single blanket rule:
--
--   * @transient-handoff@ — 'rhWorldPreviewRef' and 'rhZoomAtlasDataRef'
--     are single slots CONSUMED TO 'Nothing' by their @MainRender@
--     upload handler; 'rhBloodDisposeQueue' is DRAINED by its
--     @MainRender@ consumer. Nothing else empties any of the three.
--   * @boot-process@ — 'rhWorldPreviewGenerationRef' is MONOTONIC and is
--     never cleared or lowered; 'rhWorldQuadsRef' stays PUBLISHED until
--     replaced by the next world-thread publish or explicitly cleared
--     by a world teardown; 'rhStructureWallCatalogRef' accumulates one
--     entry per pack variant at content load and is never cleared at
--     all — it is keyed by texture PATH precisely so that the palette
--     replacement a load performs cannot invalidate it.
--   * @session-replaced@ — 'rhTexPaletteRef' and
--     'rhTexPaletteHandlesRef' follow SESSION REPLACEMENT: a load
--     publish swaps both wholesale, and neither is cleared piecemeal.
--
--   == Thread-access notes that ride along (§5)
--
--   'rhTexPaletteHandlesRef' is the one field here with @LuaThread@
--   readers as well as its @WorldThread@ use — @structure.*@ resolves
--   palette ids to live texture handles synchronously on the Lua thread
--   — so it is deliberately NOT treated as world-thread-private.
--   'rhBloodDisposeQueue' preserves #788's GPU-disposal role exactly:
--   the world-thread teardown sites enqueue an orphaned page's LIVE
--   handle 'IORef' (never a snapshot), and the render thread disposes
--   whatever remains at drain time.
--
--   Unlike @render-gpu-asset@'s §3.1 split into a full record and a
--   worker-safe view, this capability needs only ONE interface: none of
--   its fields is private to a single thread the way
--   @engineStateRef@ is to @MainRender@ — every one is a deliberate
--   cross-thread handoff, which is the whole point of the group.
--
--   Like the other capability modules, this one imports only the narrow
--   slice of @Engine.Core.State@ it needs (the bare 'EngineEnv' type
--   plus its own field accessors) rather than @EngineEnv(..)@ or a
--   bare import, so it is not itself a full-@EngineEnv@-access consumer
--   under @tools/engine_env_capability_audit.py@'s ratchet.
module Engine.Core.Capability.RenderHandoff
  ( RenderHandoffCapability(..)
  , toRenderHandoffCapability
  ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef)
import Engine.Core.Queue as Q
import Engine.Asset.Handle (TextureHandle)
import Engine.Scene.Types (LayeredQuads)
import Engine.Scene.Stats (SceneStats)
import Structure.Palette (TexPalette)
import Structure.WallCatalog (StructureWallCatalog)
import Structure.ArtCatalog (StructureArtCatalog)
import World.Types (WorldState, BloodTextureHandles)
import Engine.Core.State
  ( EngineEnv
  , worldPreviewRef, worldPreviewGenerationRef, zoomAtlasDataRef
  , worldQuadsRef, sceneStatsRef, bloodDisposeQueue, texPaletteRef
  , texPaletteHandlesRef
  , structureWallCatalogRef, structureArtCatalogRef
  )

-- | The coupled render-handoff slice of @world-sim-render-handoff@: the
--   pending world-preview and zoom-atlas upload slots with the preview's
--   generation counter, the published world quads and the
--   scene-assembly telemetry measured while building them, the blood-texture
--   GPU-disposal transport, and the save-level texture palette with its
--   runtime handle translation table. See
--   'docs/engineenv_capability_inventory.md' §5
--   @world-sim-render-handoff@ and §7.4.
data RenderHandoffCapability = RenderHandoffCapability
  { rhWorldPreviewRef           ∷ IORef (Maybe (Int, Int, BS.ByteString, Word64))
    -- ^ Transient-handoff. Pending world-preview pixels for GPU upload,
    --   tagged with the generation they were enqueued under. Produced by
    --   @WorldThread@ (world init, and load publish); CONSUMED by
    --   @MainRender@'s @Message.WorldTexture.handleWorldPreview@, which
    --   clears the single slot back to 'Nothing' as it dequeues.
  , rhWorldPreviewGenerationRef ∷ IORef Word64
    -- ^ Boot-process. Bumped once per preview enqueue by @WorldThread@
    --   and __never read back down__: the @LuaThread@ delivery handler
    --   compares the generation it dequeued against this counter's
    --   CURRENT value to suppress a stale in-flight upload. Monotonic —
    --   it has no clearing contract at all, and giving it one would
    --   break that comparison.
  , rhZoomAtlasDataRef          ∷ IORef (Maybe (Int, Int, BS.ByteString, [WorldState]))
    -- ^ Transient-handoff. Pending zoom-atlas pixels for GPU upload,
    --   paired with the EXACT 'WorldState's they belong to, captured at
    --   enqueue time so a load publish swapping @worldManagerRef@
    --   mid-upload cannot mis-attribute them. Produced by
    --   @WorldThread@; CONSUMED to 'Nothing' by @MainRender@'s
    --   @Message.WorldTexture.handleZoomAtlasUpload@.
  , rhWorldQuadsRef             ∷ IORef LayeredQuads
    -- ^ Boot-process. The world thread's per-tick static\/dynamic quad
    --   split (#446), merged and drawn by @MainRender@'s frame loop.
    --   PUBLISHED, not handed off: it stays readable until the next
    --   world-thread publish replaces it, or a world teardown
    --   explicitly clears it to @emptyLayeredQuads@ so the renderer
    --   stops drawing a destroyed world.
  , rhSceneStatsRef             ∷ IORef (Maybe SceneStats)
    -- ^ Boot-process. Scene-assembly telemetry (#1921) for the pass that
    --   produced 'rhWorldQuadsRef''s current value: one immutable
    --   snapshot per completed @updateWorldTiles@ pass, written by
    --   @WorldThread@ and read by @LuaThread@ through
    --   @debug.getSceneStats()@. PUBLISHED on exactly the same terms as
    --   'rhWorldQuadsRef' — it stays readable until the next
    --   world-thread publish replaces it, or a world teardown clears it
    --   back to 'Nothing' at the same two sites that clear the quads,
    --   which is what keeps the two from disagreeing about whether a
    --   world lifecycle ended. 'Nothing' is the query's
    --   @available = false@ state; the next completed pass republishes
    --   at sequence 1.
  , rhBloodDisposeQueue         ∷ Q.Queue (IORef BloodTextureHandles)
    -- ^ Transient-handoff. Cross-thread GPU-dispose transport for #606
    --   blood textures owned by a page the world thread is removing or
    --   replacing (#788). @WorldThread@ teardown sites enqueue the
    --   orphaned page's LIVE handle 'IORef' — never a snapshot, so the
    --   drain stays disjoint from any still-in-flight FIFO eviction of
    --   the same map and the two never double-free. DRAINED by
    --   @MainRender@'s
    --   @World.Render.BloodQuads.disposeQueuedBloodTextures@. Empty and
    --   inert headless.
  , rhTexPaletteRef             ∷ IORef TexPalette
    -- ^ Session-replaced. Save-level texture palette (path↔id):
    --   @LuaThread@ interns paths→ids at structure placement,
    --   @WorldThread@ reads it when writing a save and REPLACES it
    --   wholesale on load publish. Persisted exactly as @sdTexPalette@;
    --   nothing clears it piecemeal.
  , rhTexPaletteHandlesRef      ∷ IORef (HM.HashMap Int TextureHandle)
    -- ^ Session-replaced. Runtime paletteId → texture handle
    --   translation table — NOT persisted, rebuilt per session.
    --   Written by @LuaThread@ (at placement, and lazily after a load
    --   as Lua re-resolves each palette path) and replaced by
    --   @WorldThread@ on load publish; read by @WorldThread@
    --   (@Structure.Render@) AND directly by @LuaThread@
    --   (@structure.unresolvedPaletteIds@). A palette id with no entry
    --   yet is skipped by the renderer, never treated as an error.
  , rhStructureWallCatalogRef   ∷ IORef StructureWallCatalog
    -- ^ Boot-process. The structure packs' directional wall art keyed by
    --   texture PATH (#1712) — four edge sprites and sixteen cap facemaps
    --   per pack variant, each with the runtime handle Lua loaded for it.
    --   Written by @LuaThread@ (@structure.registerWallFamily@, once per
    --   variant as @scripts/structures.lua@ reads the pack YAML); read by
    --   @WorldThread@ (@Structure.Render@) to pick the sprite a wall's edge
    --   occupies at the current facing. Not persisted, and deliberately NOT
    --   cleared on load publish: it is keyed by path, so it stays valid
    --   across the palette replacement a load performs.
  , rhStructureArtCatalogRef    ∷ IORef StructureArtCatalog
    -- ^ Boot-process. Per-kind art for every UNPLACED structure piece
    --   (#1842), keyed by pack NAME — the texture\/facemap pair the
    --   build AI would place for each kind a pack offers, plus which
    --   kinds carry complete @build:@ metadata. Written by @LuaThread@
    --   (@structure.registerPackArt@, once per pack as
    --   @scripts/structures.lua@ and @scripts/wire.lua@ read their pack
    --   YAML; and by @structure.failPackArt@'s engine-side counterpart
    --   when a registered texture terminally fails to load); read by
    --   @WorldThread@'s construction render pass, which cannot call into
    --   Lua. Not persisted and, like 'rhStructureWallCatalogRef', never
    --   cleared: it is keyed by pack name and holds texture PATHS, so a
    --   load's palette replacement cannot invalidate it.
  }

-- | Total projection — every field aliases the identical live
--   container 'EngineEnv' already carries; nothing is copied.
toRenderHandoffCapability ∷ EngineEnv → RenderHandoffCapability
toRenderHandoffCapability env = RenderHandoffCapability
  { rhWorldPreviewRef           = worldPreviewRef env
  , rhWorldPreviewGenerationRef = worldPreviewGenerationRef env
  , rhZoomAtlasDataRef          = zoomAtlasDataRef env
  , rhWorldQuadsRef             = worldQuadsRef env
  , rhSceneStatsRef             = sceneStatsRef env
  , rhBloodDisposeQueue         = bloodDisposeQueue env
  , rhTexPaletteRef             = texPaletteRef env
  , rhTexPaletteHandlesRef      = texPaletteHandlesRef env
  , rhStructureWallCatalogRef   = structureWallCatalogRef env
  , rhStructureArtCatalogRef    = structureArtCatalogRef env
  }
