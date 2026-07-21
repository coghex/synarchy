{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | The frozen B1 baseline (issue #759, save-overhaul B1; migrated here by
--   issue #766, save-overhaul C4): the single required @"session"@
--   component's wire shape, exactly as it shipped before #760 split
--   persistence into independently versioned components.
--
--   Per the frozen-DTO boundary rule ("World.Save.Component.Types"), this
--   module never decodes through the LIVE 'World.Save.Types.SaveData'/
--   'World.Save.Types.WorldPageSave' bridge type — that shape keeps
--   evolving as an ordinary in-memory load bridge (its own haddock:
--   "bump 'currentSaveVersion' whenever this record's layout... changes").
--   'SaveDataV90'/'WorldPageSaveV90' below are an independent, hand-frozen
--   copy of exactly what those types looked like at the moment #759
--   shipped (save-version 90) — verified byte-for-byte against the
--   tracked fixture recovered from git history (commit 988c2727,
--   @trackedEnvelopeFixtureHex@ in the pre-#760 @Test.Headless.World.Save.Envelope@
--   gate) in "Test.Headless.World.Save.Compat".
--
--   Field-for-field, every non-global-allocator field composes EXISTING
--   frozen leaf/component DTOs from "World.Save.Component.Page"/
--   "World.Save.Component.Entities" rather than re-freezing them: those
--   DTOs were themselves built to be byte-identical to the live types'
--   shape at the point #760 split persistence — and per the persistence
--   changelog ('World.Save.Types.currentSaveVersion'), none of
--   'WorldPageSave'\'s embedded record shapes changed between v90 (#759)
--   and today except 'World.Save.Types.SaveData'\'s own since-removed
--   @sdLuaModules@ field (#761, v91) — so v90's wire bytes for gen
--   params/edits/designations/ground items/buildings/units/craft
--   bills/power nodes/unit-sim are EXACTLY what @"world-pages"@/
--   @"world-edits"@/@"world-activity"@/@"buildings"@/@"units"@/
--   @"craft-bills"@ (v1)/@"power-nodes"@ (v1)/@"unit-sim"@ (v1) already
--   decode today. 'migrateSessionV90' reshapes the decoded v90 tree into
--   those very same per-component DTOs and drives them through the
--   SAME 'World.Save.Component.Page'/'World.Save.Component.Entities'
--   assembly helpers ('basePageSnapshots'/'applyWorldEdits'/etc.) the
--   modern registry-driven path uses, so a migrated B1 session is
--   reconstructed by the identical code that reconstructs a modern one.
--
--   Two v90 fields have no modern home and are decoded-then-discarded,
--   matching 'World.Save.Snapshot.Adapter'\'s existing "these are load
--   policy, not gameplay state" contract for the reverse direction:
--   'wp90TimeScale' (always resets to the default speed on load) and
--   'wp90ToolMode' (always resets on load, #103). The v90-era global
--   building/unit id allocators were PER-PAGE (embedded in each page's
--   'BuildingSnapshot'/'UnitSnapshot', an artifact 'World.Save.Snapshot.Adapter'
--   already documents on the reverse path); 'migrateSessionV90' derives
--   ONE safe global allocator as the maximum of every page's own v90
--   allocator (requirement 7: an honest, non-arbitrary derivation from
--   authoritative older fields — never lower than any allocator a page
--   actually recorded, so no migrated id can ever collide).
--
--   The v90-era @sdLuaModules@ blob map (a module-name → opaque-string
--   map produced by the long-removed @scripts/lib/serialize.lua@
--   Lua-expression codec, #761) is decoded (so positional bytes past it
--   parse correctly) and then intentionally NOT interpreted — the old
--   deserializer no longer exists, so attempting to reconstruct its
--   contents would be exactly the "lossy migration / component salvage"
--   issue #766 rules out of scope. A migrated B1 session therefore always
--   carries zero Lua-owned persistent components; "Engine.Scripting.Lua.API.Save"
--   supplies each currently-required Lua module's own empty-state default
--   for a recognized legacy-baseline migration (see
--   @saveModules.prepareLoad@\'s @isMigratingLegacyBaseline@ parameter),
--   which is an honest default (issue #766 requirement 7): every
--   currently-registered persistent Lua component post-dates the B1
--   baseline, so "no persisted state yet" is exactly true for it, not a
--   guess.
module World.Save.Compat.SessionV90
    ( sessionComponentId
    , sessionComponentVersion
    , SaveDataV90(..)
    , SaveMetadataV90(..)
    , WorldPageSaveV90(..)
    , BuildingSnapshotV90(..)
    , UnitSnapshotV90(..)
    , decodeSessionV90
    , migrateSessionV90
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import qualified Data.Text as T
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

import Building.Types (BuildingId)
import Unit.Types (UnitId)
import Engine.Graphics.Camera (CameraFacing(..))
import World.Chunk.Types (ChunkCoord)
import World.Flora.Harvest (FloraHarvests)
import World.Page.Types (WorldPageId)
import World.Render.Zoom.Types (ZoomMapMode)
import World.Tool.Types (ToolMode)

import World.Save.Envelope.Types (ComponentId(..))
import World.Save.Component.Types
    ( ComponentError(..), ComponentPhase(..), coreSessionComponentId )
import World.Save.Component
    ( metadataErrors, capComponentErrors )
import World.Save.Integrity (IntegrityError(..), sessionIntegrityErrors)
import World.Save.Types (SaveMetadata)
import World.Save.Snapshot
    ( SessionSnapshot(..), LiveCameraSnapshot(..)
    , validateSessionSnapshot, structureEditPaletteErrors )
import World.Save.Component.Session
    ( TexPaletteDTO(..), fromTexPaletteDTO, validateTexPalette )
import World.Save.Component.Page
    ( WorldGenParamsDTO
    , WorldIdentityDTO(..), WorldEditDTO(..), MineDesignationDTO(..)
    , ConstructDesignationDTO(..), ChopDesignationDTO(..)
    , TillDesignationDTO(..), PlantDesignationDTO(..), CropPlotDTO(..)
    , GroundItemsDTO(..), SpoilPileDTO(..)
    , PageCoreDTO(..), WorldPagesDTO(..)
    , PageEditsDTO(..), WorldEditsDTO(..)
    , PageActivityDTO(..), WorldActivityDTO(..)
    , basePageSnapshots, applyWorldEdits, applyWorldActivity
    , validatePages, validateWorldActivity )
import World.Save.Component.Entities
    ( BuildingInstanceDTO, PageBuildingsDTO(..), BuildingsDTO(..)
    , applyBuildings
    , UnitInstanceDTO, PageUnitsDTO(..), UnitsDTO(..), applyUnits
    , UnitSimStateDTO, PageSimDTOv1(..), UnitSimDTOv1(..)
    , migrateUnitSimDTOv1, applyUnitSim
    , BillQueueDTOv1(..), PageCraftBillsDTOv1(..)
    , CraftBillsDTOv1(..), migrateCraftBillsDTOv1, applyCraftBills
    , validateCraftBills
    , NodeRegistryDTOv1(..), PagePowerNodesDTOv1(..)
    , PowerNodesDTOv1(..), migratePowerNodesDTOv1, applyPowerNodes
    , validatePowerNodes )

-- | The legacy B1 envelope's single gameplay component id (#759) — never
--   written by this build, only recognized on decode as a migration
--   source.
sessionComponentId ∷ ComponentId
sessionComponentId = ComponentId "session"

-- | The frozen encoded version #759 shipped ('World.Save.Types.currentSaveVersion'
--   at the time, i.e. the transitional-bridge shape immediately before
--   #760 split persistence).
sessionComponentVersion ∷ Word32
sessionComponentVersion = 90

-- | Frozen mirror of the v90 'BuildingSnapshot' (instance map + the
--   PER-PAGE allocator that existed before it became a single global
--   counter). Field order matches the original exactly.
data BuildingSnapshotV90 = BuildingSnapshotV90
    { bsn90Instances ∷ !(HM.HashMap BuildingId BuildingInstanceDTO)
    , bsn90NextId    ∷ !Word32
    } deriving (Show, Generic, Serialize)

-- | Frozen mirror of the v90 'UnitSnapshot', same reasoning.
data UnitSnapshotV90 = UnitSnapshotV90
    { usn90Instances ∷ !(HM.HashMap UnitId UnitInstanceDTO)
    , usn90NextId    ∷ !Word32
    } deriving (Show, Generic, Serialize)

-- | Frozen mirror of the v90 'World.Save.Types.WorldPageSave'. Field
--   order is the wire contract — it matches the live type's declaration
--   order exactly (verified against the tracked fixture in
--   "Test.Headless.World.Save.Compat").
data WorldPageSaveV90 = WorldPageSaveV90
    { wp90PageId       ∷ !WorldPageId
    , wp90GenParams    ∷ !WorldGenParamsDTO
    , wp90CameraX      ∷ !Float
    , wp90CameraY      ∷ !Float
    , wp90CameraZoom   ∷ !Float
    , wp90CameraFacing ∷ !CameraFacing
    , wp90TimeHour     ∷ !Int
    , wp90TimeMinute   ∷ !Int
    , wp90DateYear     ∷ !Int
    , wp90DateMonth    ∷ !Int
    , wp90DateDay      ∷ !Int
    , wp90TimeScale    ∷ !Float
        -- ^ Decoded, then discarded — load policy, not gameplay state
        --   (matches 'World.Save.Snapshot.Adapter's forward direction).
    , wp90MapMode      ∷ !ZoomMapMode
    , wp90ToolMode     ∷ !ToolMode
        -- ^ Decoded, then discarded — resets on load (#103).
    , wp90Edits        ∷ !(HM.HashMap ChunkCoord [WorldEditDTO])
    , wp90MineDesignations      ∷ !(HM.HashMap (Int, Int) MineDesignationDTO)
    , wp90ConstructDesignations ∷ !(HM.HashMap (Int, Int) ConstructDesignationDTO)
    , wp90GroundItems  ∷ !GroundItemsDTO
    , wp90SpoilPiles   ∷ !(HM.HashMap (Int, Int) SpoilPileDTO)
    , wp90Buildings    ∷ !BuildingSnapshotV90
    , wp90Units        ∷ !UnitSnapshotV90
    , wp90UnitSimStates ∷ !(HM.HashMap UnitId UnitSimStateDTO)
    , wp90FloraHarvests ∷ !FloraHarvests
    , wp90ChopDesignations ∷ !(HM.HashMap (Int, Int) ChopDesignationDTO)
    , wp90CraftBills   ∷ !BillQueueDTOv1
    , wp90PowerNodes   ∷ !NodeRegistryDTOv1
    , wp90TillDesignations ∷ !(HM.HashMap (Int, Int) TillDesignationDTO)
    , wp90CropPlots    ∷ !(HM.HashMap (Int, Int) CropPlotDTO)
    , wp90PlantDesignations ∷ !(HM.HashMap (Int, Int) PlantDesignationDTO)
    , wp90Identity     ∷ !(Maybe WorldIdentityDTO)
    } deriving (Show, Generic, Serialize)

-- | Frozen mirror of the v90 'World.Save.Types.SaveMetadata' (round-17
--   review): unlike every OTHER field 'SaveDataV90' below composes from
--   an already-frozen leaf DTO, this field previously embedded the LIVE,
--   ever-evolving 'SaveMetadata' directly -- a field added/removed/
--   reordered there in the future would silently corrupt this frozen
--   type's positional wire layout (everything AFTER 'sd90Metadata' in
--   'SaveDataV90' decodes relative to where its bytes end), with
--   nothing catching it: this field's own DECODED VALUE is never even
--   used by 'migrateSessionV90' (the real metadata comes from the
--   envelope's separately-decoded @"metadata"@ component instead), so
--   no assertion on it would incidentally notice either. Field order
--   matches 'SaveMetadata''s CURRENT declaration, which has been stable
--   since v82 (#707, predating this v90 baseline) — see
--   'World.Save.Types.SaveMetadata''s own haddock.
data SaveMetadataV90 = SaveMetadataV90
    { sm90Name       ∷ !Text
    , sm90Seed       ∷ !Word64
    , sm90WorldSize  ∷ !Int
    , sm90PlateCount ∷ !Int
    , sm90Timestamp  ∷ !Text
    , sm90WorldName  ∷ !(Maybe Text)
    , sm90WorldGloss ∷ !(Maybe Text)
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen mirror of the v90 'World.Save.Types.SaveData' — the exact
--   shape #759's single @"session"@ component payload was
--   ('S.encode'd directly from the then-current 'SaveData'). Field order
--   matches the live type's v90 declaration exactly: 'sd90LuaModules'
--   sat between 'sd90EnginePaused' and 'sd90TexPalette' until #761 (v91)
--   removed it (see 'World.Save.Types.currentSaveVersion's changelog).
data SaveDataV90 = SaveDataV90
    { sd90Metadata     ∷ !SaveMetadataV90
    , sd90GameTime     ∷ !Double
    , sd90EnginePaused ∷ !Bool
    , sd90LuaModules   ∷ !(HM.HashMap Text Text)
        -- ^ Decoded, then intentionally never interpreted — see the
        --   module haddock's "lossy migration/component salvage" note.
    , sd90TexPalette   ∷ !TexPaletteDTO
    , sd90NextItemInstanceId ∷ !Word64
    , sd90ActivePage   ∷ !WorldPageId
    , sd90VisiblePages ∷ ![WorldPageId]
    , sd90Worlds       ∷ ![WorldPageSaveV90]
    } deriving (Show, Generic, Serialize)

-- | Decode the raw @"session"@ component payload as the frozen v90 DTO,
--   naming the failing phase/version the same way every other
--   component's decode does (requirement 6).
decodeSessionV90 ∷ BS.ByteString → Either ComponentError SaveDataV90
decodeSessionV90 bytes = case S.decode bytes of
    Left err → Left (ComponentError sessionComponentId sessionComponentVersion
                        DecodePhase ("malformed payload: " <> T.pack err))
    Right d  → Right d

-- | Migrate a decoded v90 session into the current, fully-validated
--   'SessionSnapshot' — steps 2-4 of the compatibility contract (issue
--   #766): translate through the SAME per-component assembly helpers the
--   modern registry-driven path uses, supply the deliberate defaults
--   documented on the module haddock, then run the identical
--   cross-component + manifest-agreement checks
--   'World.Save.Component.assembleSnapshot' runs on a modern envelope.
--
--   Round-14 review: unlike 'World.Save.Component.assembleSnapshot',
--   this path previously only ran CROSS-component checks
--   ('validateSessionSnapshot'/'structureEditPaletteErrors'/
--   'sessionIntegrityErrors') and skipped every COMPONENT-LOCAL
--   validator entirely ('validatePages'/'validateTexPalette'/
--   'validateWorldActivity'/'validateCraftBills'/'validatePowerNodes' —
--   duplicate/empty page ids, a non-bijective texture palette, a
--   ground-item/craft-bill/power-node id at or above its own page
--   allocator, or a map key that disagrees with its own value's id). A
--   semantically malformed B1 save (hand-edited, corrupted, or from a
--   buggy historical writer) carrying one of those defects would
--   previously migrate/publish anyway, since nothing here ever ran the
--   SAME checks a modern envelope's decode always does. Now runs every
--   one of those five validators, all-or-nothing exactly like
--   'assembleSnapshot''s own decode-phase pass, BEFORE folding anything
--   onto a snapshot.
migrateSessionV90
    ∷ SaveMetadata → SaveDataV90 → Either [ComponentError] SessionSnapshot
migrateSessionV90 meta sd = do
    -- Requirement 7: a NON-EMPTY legacy Lua blob map means this B1 save
    -- carried real persisted AI/spawn-sequencer state — the old
    -- deserializer that could interpret it (scripts/lib/serialize.lua)
    -- was removed by #761, so there is no honest translation left.
    -- Silently proceeding would discard that state on the very next
    -- save with no record it ever existed (exactly the "lossy
    -- migration/component salvage" this issue rules out of scope) — an
    -- EMPTY map (the common real case: most B1 saves predate any
    -- registered Lua module actually persisting non-trivial state, and
    -- this migration's own generated fixtures always start empty) has
    -- nothing to lose, so it proceeds.
    when (not (HM.null (sd90LuaModules sd))) $
        Left [ComponentError sessionComponentId sessionComponentVersion
                MigratePhase
                ("legacy save carries " <> T.pack (show (HM.size (sd90LuaModules sd)))
                 <> " non-empty Lua module blob(s) (" <> luaModuleNames sd
                 <> ") that this build can no longer interpret (the pre-#761 \
                    \Lua deserializer was removed) -- refusing to migrate \
                    \rather than silently discard persisted Lua state")]
    let ps = sd90Worlds sd
        pagesDTO = WorldPagesDTO (map toPageCoreV90 ps)
        activityDTO = WorldActivityDTO (map toPageActivityV90 ps)
        craftBillsDTO =
            migrateCraftBillsDTOv1 (CraftBillsDTOv1 (map toPageCraftBillsV90 ps))
        powerNodesDTO =
            migratePowerNodesDTOv1 (PowerNodesDTOv1 (map toPagePowerNodesV90 ps))
        componentLocalErrs = capComponentErrors $
            validatePages pagesDTO
            ++ validateTexPalette (sd90TexPalette sd)
            ++ validateWorldActivity activityDTO
            ++ validateCraftBills craftBillsDTO
            ++ validatePowerNodes powerNodesDTO
    when (not (null componentLocalErrs)) $ Left componentLocalErrs
    let base = basePageSnapshots pagesDTO
    afterEdits ← applyWorldEdits 1
        (WorldEditsDTO (map toPageEditsV90 ps)) base
    afterActivity ← applyWorldActivity 1 activityDTO afterEdits
    afterBuildings ← applyBuildings 1 nextBuildingId
        (BuildingsDTO (map toPageBuildingsV90 ps)) afterActivity
    afterUnits ← applyUnits 1 nextUnitId
        (UnitsDTO (map toPageUnitsV90 ps)) afterBuildings
    afterSim ← applyUnitSim 1
        (migrateUnitSimDTOv1 (UnitSimDTOv1 (map toPageSimV90 ps))) afterUnits
    afterCraft ← applyCraftBills 1 craftBillsDTO afterSim
    afterPower ← applyPowerNodes 1 powerNodesDTO afterCraft
    let snap = SessionSnapshot
            { snapGameTime       = sd90GameTime sd
            , snapTexPalette     = fromTexPaletteDTO (sd90TexPalette sd)
            , snapNextItemId     = sd90NextItemInstanceId sd
            , snapNextBuildingId = nextBuildingId
            , snapNextUnitId     = nextUnitId
            , snapActivePage     = sd90ActivePage sd
            , snapVisiblePages   = sd90VisiblePages sd
            , snapLiveCamera     = deriveLiveCamera sd
            , snapPages          = afterPower
            }
        crossErrs = capComponentErrors $
            map snapErr (validateSessionSnapshot snap)
            ++ map snapErr (structureEditPaletteErrors snap)
            ++ metadataErrors meta snap
            ++ map integrityErr (sessionIntegrityErrors snap)
    if null crossErrs then Right snap else Left crossErrs
  where
    nextBuildingId = maximum (0 : [ bsn90NextId (wp90Buildings p) | p ← sd90Worlds sd ])
    nextUnitId     = maximum (0 : [ usn90NextId (wp90Units p) | p ← sd90Worlds sd ])

-- | The v90 live camera (requirement 7): each page carried an identical
--   copy of the one global camera's zoom/facing (see
--   'World.Save.Snapshot.Adapter's forward-direction comment), so the
--   active page's copy (falling back to the first page, same as
--   'World.Save.Types.activeWorldPage') is the honest, non-arbitrary
--   source for the migrated global camera.
deriveLiveCamera ∷ SaveDataV90 → LiveCameraSnapshot
deriveLiveCamera sd = case active of
    Just p → LiveCameraSnapshot
        { lcsOwnerPage = Just (wp90PageId p)
        , lcsX = wp90CameraX p, lcsY = wp90CameraY p
        , lcsZoom = wp90CameraZoom p, lcsFacing = wp90CameraFacing p
        }
    Nothing → LiveCameraSnapshot
        { lcsOwnerPage = Nothing, lcsX = 0, lcsY = 0
        , lcsZoom = 1, lcsFacing = FaceSouth
        }
  where
    active = case filter ((≡ sd90ActivePage sd) . wp90PageId) (sd90Worlds sd) of
        (p:_) → Just p
        []    → case sd90Worlds sd of
                    (p:_) → Just p
                    []    → Nothing

-- | A short, bounded preview of which legacy Lua modules carried
--   unmigratable state, for the rejection message above -- never the
--   blob CONTENTS (opaque, and irrelevant to naming what's affected).
luaModuleNames ∷ SaveDataV90 → Text
luaModuleNames sd = T.intercalate ", " (take 5 (HM.keys (sd90LuaModules sd)))

snapErr ∷ Show e ⇒ e → ComponentError
snapErr e = ComponentError coreSessionComponentId 1 AssemblePhase (T.pack (show e))

integrityErr ∷ IntegrityError → ComponentError
integrityErr e = ComponentError (ieComponent e) (ieVersion e) AssemblePhase
    (iePath e <> ": " <> ieCode e <> ": " <> ieMessage e)

toPageCoreV90 ∷ WorldPageSaveV90 → PageCoreDTO
toPageCoreV90 p = PageCoreDTO
    { pcPageId     = wp90PageId p
    , pcGenParams  = wp90GenParams p
    , pcCameraX    = wp90CameraX p
    , pcCameraY    = wp90CameraY p
    , pcTimeHour   = wp90TimeHour p
    , pcTimeMinute = wp90TimeMinute p
    , pcDateYear   = wp90DateYear p
    , pcDateMonth  = wp90DateMonth p
    , pcDateDay    = wp90DateDay p
    , pcMapMode    = wp90MapMode p
    , pcIdentity   = wp90Identity p
    }

toPageEditsV90 ∷ WorldPageSaveV90 → PageEditsDTO
toPageEditsV90 p = PageEditsDTO (wp90PageId p) (wp90Edits p)

toPageActivityV90 ∷ WorldPageSaveV90 → PageActivityDTO
toPageActivityV90 p = PageActivityDTO
    { padPageId        = wp90PageId p
    , padMine          = wp90MineDesignations p
    , padConstruct     = wp90ConstructDesignations p
    , padChop          = wp90ChopDesignations p
    , padTill          = wp90TillDesignations p
    , padPlant         = wp90PlantDesignations p
    , padFloraHarvests = wp90FloraHarvests p
    , padCropPlots     = wp90CropPlots p
    , padGroundItems   = wp90GroundItems p
    , padSpoilPiles    = wp90SpoilPiles p
    }

toPageBuildingsV90 ∷ WorldPageSaveV90 → PageBuildingsDTO
toPageBuildingsV90 p =
    PageBuildingsDTO (wp90PageId p) (bsn90Instances (wp90Buildings p))

toPageUnitsV90 ∷ WorldPageSaveV90 → PageUnitsDTO
toPageUnitsV90 p = PageUnitsDTO (wp90PageId p) (usn90Instances (wp90Units p))

toPageSimV90 ∷ WorldPageSaveV90 → PageSimDTOv1
toPageSimV90 p = PageSimDTOv1 (wp90PageId p) (wp90UnitSimStates p)

toPageCraftBillsV90 ∷ WorldPageSaveV90 → PageCraftBillsDTOv1
toPageCraftBillsV90 p = PageCraftBillsDTOv1 (wp90PageId p) (wp90CraftBills p)

toPagePowerNodesV90 ∷ WorldPageSaveV90 → PagePowerNodesDTOv1
toPagePowerNodesV90 p = PagePowerNodesDTOv1 (wp90PageId p) (wp90PowerNodes p)
