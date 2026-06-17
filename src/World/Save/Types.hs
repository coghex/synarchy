{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.Save.Types
    ( SaveData(..)
    , SaveMetadata(..)
    , SaveHeader(..)
    , saveMagic
    , currentSaveVersion
    , BuildingSnapshot(..)
    , BuildingInstanceSnapshot(..)
    , toBuildingSnapshot
    , fromBuildingSnapshot
    , UnitSnapshot(..)
    , UnitInstanceSnapshot(..)
    , toUnitSnapshot
    , fromUnitSnapshot
    ) where

import UPrelude
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
import World.Generate.Types (WorldGenParams(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Tool.Types (ToolMode(..))
import World.Edit.Types (WorldEdits)
import World.Mine.Types (MineDesignations)
import World.Spoil.Types (SpoilPiles)
import Item.Ground (GroundItems)
import Engine.Graphics.Camera (CameraFacing(..))
import Building.Types (BuildingId(..), BuildingInstance(..), BuildingDef(..)
                      , BuildingManager(..))
import Unit.Types (UnitId(..), UnitInstance(..), UnitDef(..), UnitManager(..)
                  , StatModifier(..), Wound(..))
import Unit.Direction (Direction(..))
import Unit.Sim.Types (UnitSimState(..), UnitThreadState(..))
import Item.Types (ItemInstance(..))

-- | 4-byte magic prefix on every save file. Spells "SYRA"
--   (Synarchy) in little-endian. Detects "this isn't a save file"
--   before we even try to decode the version field.
saveMagic ∷ Word32
saveMagic = 0x53595241

-- | Current save schema version. Bumped on every schema change.
--   v2 introduced the version header + sdGameTime + sdEnginePaused.
--   v3 (Phase 2) added the per-chunk edit log so player tile changes
--   survive eviction + save/load.
--   v4 (Phase 3) added the building snapshot.
--   v5 (Phase 4) added units + sim state.
--   v6 (Phase 5) adds Lua module blobs (aiState, building_spawn state,
--       pause state).
--   v7 drops sdClimate + sdRiverFlow. Both fields round-tripped empty
--       HashMaps every save (no producer code ever wrote to the backing
--       IORefs); live climate state lives inside sdGenParams.
--   v8 (Phase 2 equipment) adds uisEquipped to UnitInstanceSnapshot:
--       a slot id → ItemInstance map persisting equipped gear.
--   v9 adds iiQuality + iiCondition to ItemInstance (per-item rolled
--       state). Positional Serialize means any field added to
--       ItemInstance bumps the format; can't be reverse-compatible
--       without a migration shim.
--   v10 adds uisAccessories to UnitInstanceSnapshot — items worn off
--       the silhouette (robes, goggles, rings…). Empty list is legal
--       for units without any accessory slots.
--   v11 adds bisBuildProgress to BuildingInstanceSnapshot — accumulated
--       worker-seconds toward bdBuildWork. Required so a save mid-
--       construction restores at the same progress fraction (otherwise
--       the building would either reset to 0 or skip to Built depending
--       on the new currentActivity branch).
--   v12 adds bisMaterialsDelivered to BuildingInstanceSnapshot — the
--       full ItemInstance list for each material type consumed into
--       the build. Required so an electric motor that went in at
--       100% condition comes back out at its then-current condition
--       on a future deconstruction recovery.
--   v13 adds bisStorage to BuildingInstanceSnapshot — cargo currently
--       deposited in the building (separate from delivered build
--       materials). Same ItemInstance preservation rule so deposited
--       items retain their quality/condition through save/load and
--       through deposit→withdraw round-trips.
--   v15 adds lcWaterTableMap to LoadedChunk for the water-table river
--       rework (Phase A). Chunks are transient so the field itself
--       doesn't round-trip via saves, but other shape/field deletions
--       on RiverSegment in Phase B will. Bumping now so any pre-rework
--       saves are clearly rejected before Phase B lands.
--   v22 removes the EruptionEvent constructor + LavaFlow record from
--       GeoEvent / Timeline.Types as part of the lava-v1 phase-3
--       cleanup. Lava placement is now driven by the pure-function
--       Magma system; per-period eruption rolls are gone. Constructor
--       tag shift makes the schema incompatible with v21.
--   v23 adds 'gtWorldLavaPools' (WorldLakes-shaped lava pool table)
--       to GeoTimeline — surface lava placed with pooling semantics
--       instead of the per-tile film. Positional Generic Serialize
--       gains a trailing field, incompatible with v22.
--   v24 adds 'wgpLavaPoolDepth' / 'wgpLavaPoolRadius' (volcanism
--       config levers) to WorldGenParams' manual Serialize.
--   v25 adds 'gtCoastal' (global coastal-erosion table) to
--       GeoTimeline — coastal erosion moved from the per-chunk
--       windowed pass to a world-init global pass on the stitched
--       terrain (cross-window coastline divergence / seam-cliff fix).
--   v26 adds 'gtSeabed' (global seabed table) to GeoTimeline —
--       ocean-floor relief (depth-from-shore ramp + noise replacing
--       the flat seaLevel−1 basin carve) + seabed materials + bedrock
--       outcrops.
--   v27 adds 'gtWorldOcean' (tile-resolution edge-connected ocean
--       bitmask) to GeoTimeline — composeFluidMap ORs it into the
--       chunk-level ocean test so sub-sea tiles the coarse chunk-flood
--       missed render ocean (sea-stops-at-chunk-boundary fix).
currentSaveVersion ∷ Int
currentSaveVersion = 38  -- v38: ItemInstance gains trailing 'iiSharpness'
                         -- (per-instance edge keenness, split from
                         -- iiCondition for weapon degradation).
                         -- (v37: WorldGenParams gains trailing
                         -- 'wgpTimelineParams' (player-configurable
                         -- timeline depth: eon/era/period/epoch/age counts).
                         -- (v36: ItemInstance gains trailing
                         -- 'iiWeight' (per-instance rolled weights —
                         -- raw gems vary per find).
                         -- (v35: mdChunkProgress; v34: sdSpoilPiles
                         -- + WeAddTile; v33: smPercent.)

-- | File prefix: magic + version. Decoded before the SaveData body.
--   Old (v1) saves have no header — magic check fails, loader rejects
--   with "Save file invalid or incompatible".
data SaveHeader = SaveHeader
    { shMagic   ∷ !Word32
    , shVersion ∷ !Int
    } deriving (Show, Eq, Serialize, Generic)

-- | Human-readable metadata for save listing
data SaveMetadata = SaveMetadata
    { smName       ∷ !Text
    , smSeed       ∷ !Word64
    , smWorldSize  ∷ !Int
    , smPlateCount ∷ !Int
    , smTimestamp  ∷ !Text        -- ^ ISO 8601 string
    } deriving (Show, Eq, Serialize, Generic)

-- | Everything needed to reconstruct a WorldState.
--   Schema is versioned via the file header — see saveMagic /
--   currentSaveVersion / SaveHeader above. Bump currentSaveVersion
--   whenever this record's layout changes (cereal's Generic encoding
--   is positional, so reordering or inserting a field is breaking).
data SaveData = SaveData
    { sdMetadata   ∷ !SaveMetadata
    , sdGenParams  ∷ !WorldGenParams
    , sdCameraX    ∷ !Float
    , sdCameraY    ∷ !Float
    , sdCameraZoom ∷ !Float
    , sdCameraFacing ∷ !CameraFacing
    , sdTimeHour   ∷ !Int
    , sdTimeMinute ∷ !Int
    , sdDateYear   ∷ !Int
    , sdDateMonth  ∷ !Int
    , sdDateDay    ∷ !Int
    , sdTimeScale  ∷ !Float
    , sdMapMode    ∷ !ZoomMapMode
    , sdToolMode   ∷ !ToolMode
    -- v2 fields (Phase 1):
    , sdGameTime     ∷ !Double   -- ^ gameTimeRef value (game-clock seconds).
    , sdEnginePaused ∷ !Bool     -- ^ enginePausedRef value. Auto-pause-on-save
                                  --   means this is always True for v2+ saves.
    -- v3 fields (Phase 2):
    , sdEdits        ∷ !WorldEdits
        -- ^ Per-chunk edit log. Restored before chunk regeneration on
        --   load; chunks then replay their edits to recover the player's
        --   modifications. Edits survive eviction during a play session.
    -- v4 fields (Phase 3):
    , sdBuildings    ∷ !BuildingSnapshot
        -- ^ All placed buildings. Restored AFTER edits + center-chunk
        --   regen so buildings landing on player-edited terrain end up
        --   at the right z (their saved biGridZ already reflects the
        --   post-edit terrain at place time, but the chunk needs to
        --   have replayed its edits first for downstream queries to
        --   agree).
    -- v5 fields (Phase 4):
    , sdUnits        ∷ !UnitSnapshot
        -- ^ All live unit instances + their stats/skills/modifiers
        --   /inventory. Restored alongside sim states below.
    , sdUnitSimStates ∷ !(HM.HashMap UnitId UnitSimState)
        -- ^ Per-unit sim state (position, pose, activity, target, path,
        --   *Until timers). Saved straight; restored into utsRef on
        --   EngineEnv (Phase 4 promoted utsRef from the unit-thread
        --   local to engine-level).
    -- v6 fields (Phase 5):
    , sdLuaModules   ∷ !(HM.HashMap Text Text)
        -- ^ Per-Lua-module opaque blobs. Each registered module
        --   serializes its state to a string via the Lua
        --   `saveModules.serializeAll()` registry; engine treats them
        --   as opaque text. On load, Lua restores via
        --   `saveModules.deserializeAll(blobs)` BEFORE the engine-side
        --   restore happens, so AI memory + spawn-sequencer state
        --   line up with the units/buildings the engine then writes.
    -- v31 fields (mining):
    , sdMineDesignations ∷ !MineDesignations
        -- ^ Mine designations incl. mid-dig corner progress. Restored
        --   straight into wsMineDesignationsRef; markers re-render
        --   from the stored z, so no chunk loading is required first.
    -- v32 fields (ground items):
    , sdGroundItems ∷ !GroundItems
        -- ^ Items lying in the world. Full ItemInstances + float
        --   positions; resting height derives from terrain at render,
        --   so restoration needs no chunk loading either.
    -- v34 fields (dig yields):
    , sdSpoilPiles ∷ !SpoilPiles
        -- ^ Spoil mounds (vertex-keyed partial fills — see
        --   World.Spoil.Types). Fills are relative to each tile's
        --   terrain surface; promoted cells live in sdEdits as
        --   WeAddTile, so restoration is order-independent.
    } deriving (Show, Serialize, Generic)

-- | Persistable snapshot of `BuildingManager`. Drops `bmDefs`
--   (regenerated from YAML at boot) and `bmSelected` (transient UI
--   state, reset on load).
data BuildingSnapshot = BuildingSnapshot
    { bsnInstances ∷ !(HM.HashMap BuildingId BuildingInstanceSnapshot)
    , bsnNextId    ∷ !Word32
    } deriving (Show, Serialize, Generic)

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
    } deriving (Show, Serialize, Generic)

-- | Build a snapshot from a live BuildingManager. Strips `bmDefs`
--   (regenerated from YAML on next boot, always in memory at load
--   time) and `bmSelected` (resets to Nothing).
toBuildingSnapshot ∷ BuildingManager → BuildingSnapshot
toBuildingSnapshot bm = BuildingSnapshot
    { bsnInstances = HM.map toBuildingInstanceSnapshot (bmInstances bm)
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
fromBuildingSnapshot ∷ HM.HashMap Text BuildingDef → BuildingSnapshot
                     → (BuildingManager, [BuildingId])
fromBuildingSnapshot defs snap =
    let pairs = HM.toList (bsnInstances snap)
        resolved = [ (bid, fromBuildingInstanceSnapshot d snap')
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
                }
    in (bm, orphans)

fromBuildingInstanceSnapshot ∷ BuildingDef → BuildingInstanceSnapshot
                             → BuildingInstance
fromBuildingInstanceSnapshot def s = BuildingInstance
    { biDefName        = bisDefName s
    , biTexture        = bdTexture def    -- re-resolved
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
    } deriving (Show, Serialize, Generic)

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
    , uisInventory   ∷ ![ItemInstance]
    , uisEquipped    ∷ !(HM.HashMap Text ItemInstance)
      -- ^ v8: slot id → equipped item. Empty map is legal (no gear).
      --   Serialize roundtrip is positional, not name-keyed, so any
      --   future addition must go after this field; bump again if so.
    , uisAccessories ∷ ![ItemInstance]
      -- ^ v10: items worn off the silhouette (robes, goggles, rings…).
      --   Order preserved.
    , uisFactionId   ∷ !Text
      -- ^ v16: spawn-time-only faction tag (no def-level default).
      --   Used by the combat layer for hostile/friendly checks.
      --   "player" / "wildlife" / future custom tags.
    , uisWounds      ∷ ![Wound]
      -- ^ v16: per-unit wound list. Roundtrips faithfully. Generic
      --   Serialize over the Wound record below; fields are
      --   positional, so appending a field to Wound also bumps v.
    , uisBlood       ∷ !Float
      -- ^ v16: current blood volume in litres. Spawn-time seeded
      --   from body_mass; ticked down by Combat.Wounds bleeding.
    } deriving (Show, Serialize, Generic)

toUnitSnapshot ∷ UnitManager → UnitSnapshot
toUnitSnapshot um = UnitSnapshot
    { usnInstances = HM.map toUnitInstanceSnapshot (umInstances um)
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
    , uisInventory   = uiInventory ui
    , uisEquipped    = uiEquipment ui
    , uisAccessories = uiAccessories ui
    , uisFactionId   = uiFactionId ui
    , uisWounds      = uiWounds ui
    , uisBlood       = uiBlood ui
    }

-- | Restore a UnitManager from a snapshot. Like buildings: instances
--   whose def is no longer registered get dropped with the orphan
--   list returned for caller logging. `umSelected` resets to empty.
fromUnitSnapshot ∷ HM.HashMap Text UnitDef → UnitSnapshot
                 → (UnitManager, [UnitId])
fromUnitSnapshot defs snap =
    let pairs = HM.toList (usnInstances snap)
        resolved = [ (uid, fromUnitInstanceSnapshot d s)
                   | (uid, s) ← pairs
                   , Just d ← [HM.lookup (uisDefName s) defs]
                   ]
        orphans  = [ uid
                   | (uid, s) ← pairs
                   , not (HM.member (uisDefName s) defs)
                   ]
        um = UnitManager
                { umDefs      = defs
                , umInstances = HM.fromList resolved
                , umSelected  = mempty
                , umNextId    = usnNextId snap
                }
    in (um, orphans)

fromUnitInstanceSnapshot ∷ UnitDef → UnitInstanceSnapshot → UnitInstance
fromUnitInstanceSnapshot def s = UnitInstance
    { uiDefName     = uisDefName s
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
    , uiInventory   = uisInventory s
    , uiEquipment   = uisEquipped s
    , uiAccessories = uisAccessories s
    , uiFactionId   = uisFactionId s
    , uiWounds      = uisWounds s
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
    }
