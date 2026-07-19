{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Command.Types
    ( WorldTextureType(..)
    , WorldCommand(..)
    , FluidWriteback(..)
    , FluidWritebackBatch(..)
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Control.Concurrent.MVar (MVar)
import Engine.Asset.Handle (TextureHandle(..))
import World.Chunk.Types (ChunkCoord(..))
import World.Material.Id (MaterialId(..))
import World.Page.Types (WorldPageId(..), WorldIdentity(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Tool.Types (ToolMode(..))
import World.Construct.Types (ConstructTarget(..), ConstructStatus(..))
import World.Save.Types (SaveData(..))
import World.Texture.Types (WorldTextureType(..))
import World.Fluid.Types (FluidType(..), FluidCell(..))

-- | One chunk's simulated fluid result, produced by the sim thread and
--   applied to 'wsTilesRef' by the WORLD thread (the sole writer). The
--   sim derives all four fields so the world handler is a dumb inserter.
data FluidWriteback = FluidWriteback
    { fwCoord    ∷ !ChunkCoord
    , fwFluid    ∷ !(V.Vector (Maybe FluidCell))
    , fwTerrain  ∷ !(VU.Vector Int)
    , fwSurf     ∷ !(VU.Vector Int)
    , fwSideDeco ∷ !(VU.Vector Word8)
    }

-- | A batch of fluid writebacks for ONE world plus an optional ack
--   'MVar', signalled once the world thread has applied the batch. The
--   'WorldPageId' scopes the batch so the world thread applies it only to
--   the world that produced it (not every visible world, #59). Runtime
--   ticks pass 'Nothing' (fire-and-forget); the dump's synchronous
--   fast-settle passes 'Just' and waits on it so the write lands before
--   it reads.
data FluidWritebackBatch =
    FluidWritebackBatch !WorldPageId ![FluidWriteback] !(Maybe (MVar ()))
instance Show FluidWritebackBatch where
    show (FluidWritebackBatch pid ws _) =
        "FluidWritebackBatch(" <> show pid <> ", " <> show (length ws) <> ")"

data WorldCommand
    = WorldInit WorldPageId Word64 Int Int (Maybe WorldIdentity)
        -- ^ pageId, seed, worldSize (chunks), plateCount, and the
        --   page's optional player-facing identity (#707) — display
        --   name + gloss, already normalized via 'mkWorldIdentity'.
        --   'Nothing' creates an unnamed page (every pre-#707 caller).
    | WorldInitArena WorldPageId
    | WorldInitArenaDone WorldPageId
    | WorldShow WorldPageId
    | WorldHide WorldPageId
    | WorldTick Double
    | WorldSetTexture WorldPageId WorldTextureType TextureHandle
    | WorldSetCamera WorldPageId Float Float
    | WorldSetTime WorldPageId Int Int         -- ^ hour minute
    | WorldSetDate WorldPageId Int Int Int     -- ^ year month day
    | WorldSetTimeScale WorldPageId Float      -- ^ game-minutes per real-second
    | WorldSetMapMode WorldPageId ZoomMapMode      -- ^ map mode
    | WorldSetZoomCursorHover WorldPageId Int Int
    | WorldSetZoomCursorSelect WorldPageId
    | WorldSetZoomCursorDeselect WorldPageId
    | WorldSetZoomCursorSelectTexture WorldPageId TextureHandle
    | WorldSetZoomCursorHoverTexture WorldPageId TextureHandle
    | WorldSetWorldCursorSelectTexture WorldPageId TextureHandle
    | WorldSetWorldCursorHoverTexture WorldPageId TextureHandle
    | WorldSetWorldCursorSelectBgTexture WorldPageId TextureHandle
    | WorldSetWorldCursorHoverBgTexture WorldPageId TextureHandle
    | WorldSetWorldCursorHover WorldPageId Int Int
    | WorldSetWorldCursorSelect WorldPageId
    | WorldSetWorldCursorDeselect WorldPageId
    | WorldSelectTileByCoord WorldPageId Int Int (Maybe Int)
        -- ^ Atomically set worldSelectedTile to the column at (gx, gy).
        --   The @Maybe Int@ is the z to select: @Just z@ pins the exact
        --   tile (the live-picked z from a left-click or a right-click
        --   → Info context-menu selection, so a click below the
        --   surface selects the clicked tile, not the column top —
        --   issue #367); @Nothing@ falls back to the loaded chunk's
        --   surface z, a latent API affordance no current UI path
        --   exercises (both live callers always resolve a pick and
        --   pass its z). Bypasses the hover-then-select dance so
        --   the caller doesn't have to fight the continuous mouse-hover
        --   overwrites for one-shot selections.
    | WorldSelectChunkByCoord WorldPageId Int Int
        -- ^ Atomically set zoomSelectedPos to the chunk whose
        --   chunk-aligned grid origin is (gx, gy) — as produced by
        --   'World.Render.Zoom.Cursor.pixelToChunkOrigin' /
        --   @world.pickChunk@ — clearing any zoomed-in tile selection
        --   in the same write (issue #135's newest-selection-owns
        --   rule). This is the chunk-selection analog of
        --   'WorldSelectTileByCoord': the set and the opposing clear
        --   land in ONE atomic write, so a zoom-map left click binds
        --   to the chunk under the click at click time and can't be
        --   retargeted by a later hover update, camera pan/zoom, or
        --   render timing before some later render pass gets around
        --   to resolving it (issue #813).
    | WorldSetToolMode WorldPageId ToolMode
    | WorldSetMineAnchor WorldPageId Int Int
        -- ^ Mine tool: first click anchors the designation rectangle
        --   at (gx, gy). The render pass previews anchor→hover.
    | WorldClearMineAnchor WorldPageId
        -- ^ Mine tool: cancel the pending rectangle (right-click /
        --   Escape / tool switch).
    | WorldDesignateMine WorldPageId Int Int Int Int
        -- ^ Mine tool: second click commits the rectangle
        --   (gx1,gy1)–(gx2,gy2) (corners in either order). Tiles in
        --   loaded chunks land in wsMineDesignationsRef with their
        --   surface z; tiles in unloaded chunks are skipped.
    | WorldSetMineDesignateTexture WorldPageId TextureHandle
        -- ^ Texture for committed designation markers.
    | WorldSetConstructAnchor WorldPageId Int Int
        -- ^ Construction tool (#95): first click anchors the designation
        --   rectangle at (gx, gy). The render pass previews anchor→hover.
    | WorldClearConstructAnchor WorldPageId
        -- ^ Construction tool: cancel the pending rectangle (right-click /
        --   Escape / tool switch).
    | WorldDesignateConstruct WorldPageId Int Int Int Int ConstructTarget
        -- ^ Construction tool: second click commits the rectangle
        --   (gx1,gy1)–(gx2,gy2) for the given build target. Tiles in
        --   loaded chunks land in wsConstructDesignationsRef with their
        --   surface z; tiles in unloaded chunks are skipped. Per-z-level
        --   like mine designation. Building targets only mark the anchor
        --   tile (a building is one footprint, not a rectangle of them).
    | WorldCancelConstruct WorldPageId Int Int
        -- ^ Remove the construction designation at (gx, gy), if any
        --   (cancel mode / right-click on an existing blueprint).
    | WorldSetConstructStatus WorldPageId Int Int ConstructStatus
        -- ^ Build AI (#96): mark a designation Claimed / Complete. A
        --   Complete designation is removed (the structure/building it
        --   represents now exists).
    | WorldAddConstructProgress WorldPageId Int Int Float
        -- ^ Build AI (#96): add build progress to the designation at
        --   (gx, gy). Deltas are pre-normalised to the job's total
        --   work (1.0 = done) and the sum is clamped to [0, 1]; the
        --   ghost marker's alpha ramps with it. Completion (placing
        --   the piece + removing the designation) stays Lua-side —
        --   the AI resolves art/materials, so it owns final placement.
    | WorldSetConstructDesignateTexture WorldPageId Text TextureHandle
        -- ^ Ghost texture for committed construction designations, keyed
        --   by target category ("structure" | "building").
    | WorldSetConstructLineMode WorldPageId Bool
        -- ^ Wire path tool (#359): while true, the anchor→hover preview
        --   snaps to a straight 1-wide line (the build tool's commit
        --   snaps identically), instead of the default filled rectangle.
    | WorldSetChopAnchor WorldPageId Int Int
        -- ^ Chop tool (#97): first click anchors the designation
        --   rectangle at (gx, gy). The render pass previews anchor→hover.
    | WorldClearChopAnchor WorldPageId
        -- ^ Chop tool: cancel the pending rectangle (right-click /
        --   Escape / tool switch).
    | WorldDesignateChop WorldPageId Int Int Int Int Text
        -- ^ Chop tool: second click commits the rectangle
        --   (gx1,gy1)–(gx2,gy2). Only tiles in loaded chunks holding a
        --   currently-harvestable flora species carrying the given
        --   harvest tag (the tool passes "wood") are designated, each at
        --   its own surface z — trees grow across slopes, so unlike
        --   mine/construct there is no per-z-level filter.
    | WorldCancelChop WorldPageId Int Int
        -- ^ Remove the chop designation at (gx, gy), if any (the chop
        --   AI's completion, or a player cancel).
    | WorldSetChopDesignateTexture WorldPageId TextureHandle
        -- ^ Texture for committed chop-designation markers.
    | WorldSetTillAnchor WorldPageId Int Int
        -- ^ Till tool (#333): first click anchors the designation
        --   rectangle at (gx, gy). The render pass previews anchor→hover.
    | WorldClearTillAnchor WorldPageId
        -- ^ Till tool: cancel the pending rectangle (right-click /
        --   Escape / tool switch).
    | WorldDesignateTill WorldPageId Int Int Int Int
        -- ^ Till tool: second click commits the rectangle
        --   (gx1,gy1)–(gx2,gy2). Only tiles in loaded chunks at the
        --   ANCHOR's surface z, with no fluid on top, no flora instance,
        --   and not already tilled, are designated — a farmed field is
        --   flat ground, like a construction footprint.
    | WorldCancelTill WorldPageId Int Int
        -- ^ Remove the till designation at (gx, gy), if any (the till
        --   AI's completion, or a player cancel).
    | WorldSetTillDesignateTexture WorldPageId TextureHandle
        -- ^ Texture for committed till-designation markers.
    | WorldDesignatePlant WorldPageId Int Int Text
        -- ^ Plant tool (#335): single-tile designation (no anchor — the
        --   planting screen already scopes the player to one tile
        --   before a crop is chosen). gx gy cropName. Refused unless
        --   the tile is tilled soil (world.isPlantable) and cropName
        --   names a registered plantable-crop species (row_crop or
        --   groundcover_crop worldGen category).
    | WorldCancelPlant WorldPageId Int Int
        -- ^ Remove the plant designation at (gx, gy), if any (the farm
        --   AI's completion, or a player cancel).
    | WorldSetPlantDesignateTexture WorldPageId TextureHandle
        -- ^ Texture for committed plant-designation markers.
    | WorldSetVeg WorldPageId Int Int Int Word8
        -- ^ Set the vegetation id of the tile at (gx,gy,z) via the
        --   WeSetVeg edit path (world.setVegAt) — the till AI's
        --   completion primitive (#333), same shape as world.setSlope.
    | WorldPlantRowCropAt WorldPageId Int Int Text
        -- ^ worldId, gx, gy, cropName. Plant a single row-crop
        --   FloraInstance at (gx,gy) via the WePlaceFlora edit path
        --   (world.plantRowCropAt) — the farm AI's (#336) row-crop
        --   planting completion, the FloraInstance counterpart to
        --   world.plantCropAt's CropPlot for groundcover crops. Refused
        --   world-thread-side unless the tile is tilled soil and
        --   cropName names a registered row_crop species.
    | WorldDigTile WorldPageId Int Int Float Float Float Float Float
        -- ^ Apply dig progress to the designated tile at (gx, gy):
        --   pageId gx gy uxPos uyPos amount minerSkill perception.
        --   perception scales the gem-find roll when the tile
        --   completes (World.Gem). The digger's
        --   tile-space position picks which corners drain first
        --   (digger-side); amount is pre-scaled by tool × material
        --   speed. minerSkill is the CURRENT digger's mining skill —
        --   it scales the per-tick chunk-yield fill, so a mid-dig
        --   handoff uses the new digger's rate. Corners at zero →
        --   the tile drops one z via the WeDeleteTile path and the
        --   designation is removed.
    | WorldAddTile WorldPageId Int Int MaterialId
        -- ^ Raise the column at (gx, gy) one z of the given material
        --   via the WeAddTile edit path (debug terrain placement —
        --   same machinery spoil promotion uses, so it persists).
    | WorldSave WorldPageId Text Text [(Text, Word32, Bool, BS.ByteString)]
        -- ^ pageId, save-name, request-timestamp (ISO 8601 microsecond
        --   precision, monotonically clamped), and every currently-
        --   registered Lua save component (bare registry name, schema
        --   version, required flag, already-encoded payload — issue
        --   #761, save-overhaul B3, mirrors
        --   "World.Save.Envelope.LuaComponentSpec" without importing
        --   the whole Save/Envelope module graph into this one). The
        --   Lua side captures the timestamp at request time (so two
        --   saves queued close together get distinct timestamps
        --   reflecting when the player asked, not whenever the world
        --   thread happened to process them) and calls
        --   @saveModules.snapshotAll()@ before queueing this command,
        --   aborting the save entirely rather than enqueueing it if any
        --   REQUIRED Lua component failed to snapshot.
    | WorldLoadTransaction Int SaveData
        -- ^ requestId, decoded + content-validated 'SaveData' (issue
        --   #763, save-overhaul C2). Stages the complete replacement
        --   session ("World.Load.Stage") without touching any live ref
        --   (requirement 6) — the staged result lands in
        --   'Engine.Core.State.pendingLoadRef', keyed by requestId, and
        --   a 'LuaStagingComplete' message is posted so the Lua thread
        --   can drive the publish barrier
        --   ("Engine.Scripting.Lua.Thread.Dispatch").
    | WorldLoadPublish Int
        -- ^ requestId. Atomically publishes the 'World.Load.Types.StagedSession'
        --   the matching 'WorldLoadTransaction' produced
        --   ("World.Load.Publish") — issued ONLY while the save
        --   barrier's capture lock is held (mirrors 'WorldSave''s
        --   authorized-command handling in "World.Thread").
    | WorldDeleteTile WorldPageId Int Int      -- ^ worldId, gx, gy
    | WorldSetFluidTile WorldPageId Int Int FluidType
        -- ^ worldId, gx, gy, fluid kind. Sets one tile of fluid at
        --   surfaceZ + 1 on the given column. Idempotent; replaces any
        --   existing fluid cell. Currently a debug-tool affordance.
    | WorldSetSlope WorldPageId Int Int Int Word8
        -- ^ worldId, gx, gy, z, slope bitmask (0=N 1=E 2=S 3=W). Sets the
        --   walkable-ramp slope bits of an existing tile via the WeSetSlope
        --   edit path. No generator emits this — it exists for movement
        --   test harnesses, since addTile only ever produces flat tops.
    | WorldSetCell WorldPageId Int Int Int MaterialId
        -- ^ worldId, gx, gy, z, material (id 0 = air). Sets a single 3D
        --   cell via the WeSetCell edit path — the locations primitive for
        --   carving interior air, walls, ceilings, staircases. Grows the
        --   column up to reach z; z below the column floor is a no-op.
    | WorldSetStructure WorldPageId Int Int Word8 Int Int Int
        -- ^ worldId, gx, gy, slot-tag, texture palette id, facemap palette
        --   id, z. Places a structure piece via the WeSetStructure edit path
        --   (per-chunk overlay; persists). The palette ids are interned Lua-
        --   side before queueing; the resolved cap variant is already baked
        --   into facePaletteId (the BUILDER picks it, not this handler).
    | WorldClearStructure WorldPageId Int Int Word8
        -- ^ worldId, gx, gy, slot-tag. Removes a structure piece.
    | WorldClearAllStructures WorldPageId
        -- ^ worldId. Removes EVERY structure piece in the world: clears the
        --   live per-chunk overlays AND strips all WeSetStructure/
        --   WeClearStructure edits from the log so they don't replay on
        --   eviction/reload. The authoritative "wipe all structures".
    | WorldDestroy !WorldPageId
    | WorldDestroyAll
        -- ^ Tear down EVERY world (Exit to Menu): clears wmWorlds/wmVisible,
        --   sim-deactivates each page, and resets the global unit/building
        --   managers. Without this a hidden world (e.g. a leftover arena)
        --   stays in wmWorlds and resolveActiveWorld's head-fallback keeps
        --   resolving it as the implicit active world behind the menu (#58).
    | WorldApplyFluids !FluidWritebackBatch
        -- ^ Sim → World: apply the sim's settled/active fluid results to
        --   the visible world's 'wsTilesRef'. The world thread is the
        --   SOLE writer of 'wsTilesRef'; the sim never touches it.
    | WorldMarkLocationContentsSpawned WorldPageId Int Int
        -- ^ worldId, gx, gy. One-time content-spawn flag (#90): marks the
        --   chunk containing (gx, gy) as having had its placed location's
        --   `contents` spawned, in 'WorldGenParams.wgpLocationContentsSpawned'
        --   — so a later chunk (re)load never respawns them. The world
        --   thread is the sole owner of WorldGenParams; Lua queues this
        --   rather than mutating wsGenParamsRef directly.
    | WorldMarkLocationStamped WorldPageId Int Int
        -- ^ worldId, gx, gy. One-time geometry-stamp flag (#424): marks the
        --   chunk containing (gx, gy) as having had its placed location's
        --   builder run, in 'WorldGenParams.wgpLocationStamped' — so a
        --   later chunk (re)load never re-stamps it, even if the player has
        --   since cleared the anchor floor tile the old structure.hasAt
        --   guard relied on. The world thread is the sole owner of
        --   WorldGenParams; Lua queues this rather than mutating
        --   wsGenParamsRef directly.
    deriving (Show)
