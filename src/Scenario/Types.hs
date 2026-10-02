{-# LANGUAGE DeriveGeneric, DeriveFunctor #-}
-- | The typed scenario-file representation (#2699, SCN-01 of epic #2698).
--
--   A scenario is a reusable testing-arena setup authored as YAML or
--   (later, SCN-14) captured from a running arena. This module holds the
--   VALIDATED form only: what "Scenario.Schema" hands back after a file
--   has been read, parsed, version-dispatched, migrated and checked
--   against a definition catalog. Nothing here constructs, touches or
--   replaces a live world; the runtime adapters of the later slices
--   consume these values.
--
--   The authoritative field table, the omission/explicit-empty rules and
--   the compatibility policy are in @docs/scenario_format.md@. The two
--   rules every type below encodes:
--
--     * 'Authored' keeps an OMITTED field apart from an EXPLICIT one,
--       including an explicit zero or an explicit empty collection. An
--       omission is eligible for the deterministic, definition-driven
--       fallback; an explicit value is authoritative.
--     * 'ScenarioId' is the stable scenario identity. It is neither a
--       runtime allocation id (item instance ids, unit ids, building
--       ids, location instance ids, flora instance ids) nor a gameplay
--       tag, and tags never feed it.
module Scenario.Types
    ( -- * Identity and authoring
      ScenarioId(..)
    , scenarioIdText
    , Authored(..)
    , authoredOr
    , isAuthored
      -- * Validated content
    , Scenario(..)
    , MapDimensions(..)
    , TileRegion(..)
    , TerrainPatch(..)
    , FluidKind(..)
    , fluidKindName
    , FluidPatch(..)
    , FloraEntry(..)
    , StructurePiece(..)
    , BuildingEntry(..)
    , LocationEntry(..)
    , UnitEntry(..)
    , ModifierSpec(..)
    , WoundSpec(..)
    , ScarSpec(..)
    , ItemEntry(..)
    , ItemTemperature(..)
    , GroundItemEntry(..)
      -- * Definition catalog
    , ScenarioCatalog(..)
    , emptyScenarioCatalog
    , StatRule(..)
    , UnitCatalogEntry(..)
    , BuildingCatalogEntry(..)
    , ItemCatalogEntry(..)
    , Footprint(..)
    , LocationCatalogEntry(..)
      -- * Diagnostics and outcome
    , DiagnosticEffect(..)
    , DiagnosticReason(..)
    , ScenarioDiagnostic(..)
    , ScenarioFailure(..)
    , ScenarioOutcome(..)
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Gameplay.Tags.Types (GameplayTag)
import Unit.Direction (Direction)

-- * Identity and authoring

-- | A stable scenario identity (requirement 5).
--
--   'ExplicitId' is an authored @id:@ — what a capture always writes and
--   the only kind a reference may name. 'AutoId' is assigned when the
--   author omitted @id:@: the entry's document path (@units[2]@,
--   @units[2].inventory[0]@). The two spaces cannot collide, because an
--   explicit id may not contain @[@ (see "Scenario.Decode").
--
--   This value is also the identity input of deterministic default
--   generation (D-5): the same identity in the same scenario rolls the
--   same omitted values. Tags are deliberately absent from it.
data ScenarioId
    = ExplicitId !Text
    | AutoId !Text
    deriving (Show, Eq, Ord, Generic)

instance Hashable ScenarioId

scenarioIdText ∷ ScenarioId → Text
scenarioIdText (ExplicitId t) = t
scenarioIdText (AutoId t)     = t

-- | An optional field's presence (requirement 4). 'Omitted' permits the
--   definition-driven fallback; 'Authored' is authoritative even when it
--   holds @0@, @[]@ or an empty map.
data Authored α
    = Omitted
    | Authored !α
    deriving (Show, Eq, Functor)

authoredOr ∷ α → Authored α → α
authoredOr d Omitted      = d
authoredOr _ (Authored x) = x

isAuthored ∷ Authored α → Bool
isAuthored Omitted = False
isAuthored _       = True

-- * Validated content

-- | Optional finite map size in tiles. Absent means the existing
--   expandable arena (D-9). The tile rectangle is
--   'Scenario.Bounds.mapTileBounds'.
data MapDimensions = MapDimensions
    { mdWidth  ∷ !Int
    , mdHeight ∷ !Int
    } deriving (Show, Eq)

-- | A set of horizontal tiles, in scenario Cartesian coordinates
--   (positive Y up). A rectangle is inclusive on all four sides and
--   stored normalized (@x0 ≤ x1@, @y0 ≤ y1@); a tile list keeps the
--   authored order with duplicates removed.
data TileRegion
    = RegionRect !Int !Int !Int !Int   -- ^ x0 y0 x1 y1, inclusive
    | RegionTiles ![(Int, Int)]
    deriving (Show, Eq)

-- | One terrain patch: every tile of the region gets a column whose top
--   solid cell is @surface_z@ in the named material.
data TerrainPatch = TerrainPatch
    { tpId       ∷ !ScenarioId
    , tpTags     ∷ ![GameplayTag]
    , tpRegion   ∷ !TileRegion          -- ^ already clipped to the map
    , tpMaterial ∷ !Text
    , tpSurfaceZ ∷ !Int
    , tpSlope    ∷ !(Authored Word8)    -- ^ ramp bitmask, bits N/E/S/W
    } deriving (Show, Eq)

data FluidKind = FluidOcean | FluidLake | FluidRiver | FluidLava
    deriving (Show, Eq, Enum, Bounded)

fluidKindName ∷ FluidKind → Text
fluidKindName k = case k of
    FluidOcean → "ocean"
    FluidLake  → "lake"
    FluidRiver → "river"
    FluidLava  → "lava"

-- | One fluid patch. The surface is stored in exact eighth-z units, the
--   plane "World.Fluid.Exact" uses (@surface_z: 4.5@ is 36).
data FluidPatch = FluidPatch
    { fpId             ∷ !ScenarioId
    , fpTags           ∷ ![GameplayTag]
    , fpRegion         ∷ !TileRegion    -- ^ already clipped to the map
    , fpKind           ∷ !FluidKind
    , fpSurfaceEighths ∷ !Int
    } deriving (Show, Eq)

data FloraEntry = FloraEntry
    { feId      ∷ !ScenarioId
    , feTags    ∷ ![GameplayTag]
    , feSpecies ∷ !Text
    , feX       ∷ !Int
    , feY       ∷ !Int
    , feZ       ∷ !(Authored Int)
    , feAge     ∷ !(Authored Float)     -- ^ game days
    , feHealth  ∷ !(Authored Float)     -- ^ 0..1
    } deriving (Show, Eq)

-- | One structure piece (floor, wall edge, post, ceiling, wire) from a
--   structure pack.
data StructurePiece = StructurePiece
    { spId    ∷ !ScenarioId
    , spTags  ∷ ![GameplayTag]
    , spPack  ∷ !Text
    , spPiece ∷ !Text                   -- ^ slot name, e.g. @wall_ne@
    , spX     ∷ !Int
    , spY     ∷ !Int
    , spZ     ∷ !(Authored Int)
    } deriving (Show, Eq)

data BuildingEntry = BuildingEntry
    { beId         ∷ !ScenarioId
    , beTags       ∷ ![GameplayTag]
    , beDefinition ∷ !Text
    , beX          ∷ !Int               -- ^ footprint anchor (min corner)
    , beY          ∷ !Int
    , beZ          ∷ !(Authored Int)
    , beStorage    ∷ !(Authored [ItemEntry])
    , beBuildProgress ∷ !(Authored Float)
      -- ^ worker-seconds toward the definition's build work; omitted =
      --   complete
    , beMaterialsDelivered ∷ !(Authored [ItemEntry])
      -- ^ items consumed into the build (the runtime groups them by
      --   definition); omitted = nothing delivered
    , bePowerCharge ∷ !(Authored Float)
      -- ^ stored watt-hours of a power-storage building; omitted = the
      --   placement default (empty)
    } deriving (Show, Eq)

-- | A real placed location. @significant_items@ binds guaranteed
--   significant slots to item entries — an OPTIONAL reference: a slot
--   whose binding is dropped stays unbound, which is the ordinary
--   not-yet-spawned state.
data LocationEntry = LocationEntry
    { leId          ∷ !ScenarioId
    , leTags        ∷ ![GameplayTag]
    , leDefinition  ∷ !Text
    , leX           ∷ !Int
    , leY           ∷ !Int
    , leSignificant ∷ !(HM.HashMap Int ScenarioId)
    } deriving (Show, Eq)

data UnitEntry = UnitEntry
    { ueId          ∷ !ScenarioId
    , ueTags        ∷ ![GameplayTag]
    , ueDefinition  ∷ !Text
    , ueX           ∷ !Float
    , ueY           ∷ !Float
    , ueZ           ∷ !(Authored Int)
    , ueName        ∷ !(Authored Text)       -- ^ @""@ = unnamed
    , ueFacing      ∷ !(Authored Direction)  -- ^ omitted = south
    , ueEncounter   ∷ !(Maybe ScenarioId)
      -- ^ REQUIRED reference when present: this unit is an occupant of
      --   that location's encounter, so it cannot outlive the location.
    , ueStats       ∷ !(HM.HashMap Text Float)
      -- ^ explicit stat overrides only; a stat name absent here is
      --   fallback-eligible ('Scenario.Validate.fallbackStatNames').
    , ueSkills      ∷ !(HM.HashMap Text Float)
      -- ^ explicit skill-level overrides; same omission rule as stats
    , ueKnowledge   ∷ !(Authored (HM.HashMap Text Float))
      -- ^ the COMPLETE known-knowledge map when authored (a key's
      --   presence means known); omitted = the definition's roll
    , ueModifiers   ∷ !(Authored (HM.HashMap Text [ModifierSpec]))
      -- ^ active stat/skill modifiers by stat or skill name
    , ueWounds      ∷ !(Authored [WoundSpec])
    , ueScars       ∷ !(Authored [ScarSpec])
    , ueBlood       ∷ !(Authored Float)          -- ^ litres
    , ueImmuneResponse ∷ !(Authored Float)       -- ^ 0..1
    , ueImmunities  ∷ !(Authored (HM.HashMap Text Float))
    , ueInventory   ∷ !(Authored [ItemEntry])
    , ueEquipment   ∷ !(Authored (HM.HashMap Text ItemEntry))
    , ueAccessories ∷ !(Authored [ItemEntry])
    } deriving (Show, Eq)

-- | One 'Unit.Types.Def.StatModifier'. Scenario files carry no game
--   clock (owner decision, 2026-10-02): an expiring modifier states its
--   REMAINING duration in game seconds after the scenario starts, and
--   the runtime turns that into @smExpiry = start + remaining@. An
--   omitted @remaining@ is a permanent modifier (@smExpiry = Nothing@).
data ModifierSpec = ModifierSpec
    { msSource    ∷ !Text
    , msDelta     ∷ !Float
    , msPercent   ∷ !Float
    , msRemaining ∷ !(Maybe Float)
    } deriving (Show, Eq)

-- | 'Unit.Types.Wound.Wound' with its game-time stamp made relative: @age@
--   is how many game seconds BEFORE the scenario starts the wound was
--   inflicted (@woundAt = start − age@); omitted = inflicted at start.
data WoundSpec = WoundSpec
    { wsPart          ∷ !Text
    , wsKind          ∷ !Text
    , wsSeverity      ∷ !Float
    , wsAge           ∷ !(Authored Float)
    , wsBandage       ∷ !(Authored Float)
    , wsClot          ∷ !(Authored Float)
    , wsHeal          ∷ !(Authored Float)
    , wsDressing      ∷ !(Authored Text)
    , wsInfection     ∷ !(Authored Float)
    , wsClean         ∷ !(Authored Bool)
    , wsInfectionType ∷ !(Authored Text)
    , wsNecrosis      ∷ !(Authored Float)
    } deriving (Show, Eq)

-- | 'Unit.Types.Wound.Scar'; @age@ is relative exactly as for a wound
--   (@scarAt = start − age@).
data ScarSpec = ScarSpec
    { ssPart     ∷ !Text
    , ssKind     ∷ !Text
    , ssSeverity ∷ !Float
    , ssAge      ∷ !(Authored Float)
    } deriving (Show, Eq)

-- | An item's temperature: at the tile's ambient (the runtime's
--   @Nothing@) or tracked at a value in °C.
data ItemTemperature = AtAmbient | TrackedTemp !Float
    deriving (Show, Eq)

-- | One physical item, recursively owning its contents.
data ItemEntry = ItemEntry
    { ieId          ∷ !ScenarioId
    , ieTags        ∷ ![GameplayTag]
    , ieDefinition  ∷ !Text
    , ieFill        ∷ !(Authored Float)     -- ^ litres
    , ieQuality     ∷ !(Authored Float)     -- ^ 0..100
    , ieCondition   ∷ !(Authored Float)     -- ^ 0..100
    , ieSharpness   ∷ !(Authored Float)     -- ^ 0..100
    , ieWeight      ∷ !(Authored Float)     -- ^ kg, empty weight
    , ieBulk        ∷ !(Authored Float)     -- ^ litres, external
    , ieTemperature ∷ !(Authored ItemTemperature)
    , ieContents    ∷ !(Authored [ItemEntry])
    } deriving (Show, Eq)

data GroundItemEntry = GroundItemEntry
    { giX    ∷ !Float
    , giY    ∷ !Float
    , giItem ∷ !ItemEntry
    } deriving (Show, Eq)

-- | A validated scenario: the usable content after every recoverable
--   rejection. Every family is a plain list in authored order.
data Scenario = Scenario
    { scSourceVersion ∷ !Int
      -- ^ the version the FILE declared, before in-memory migration
    , scMap         ∷ !(Maybe MapDimensions)
    , scCamera      ∷ !(Float, Float)
    , scTerrain     ∷ ![TerrainPatch]
    , scFluids      ∷ ![FluidPatch]
    , scFlora       ∷ ![FloraEntry]
    , scStructures  ∷ ![StructurePiece]
    , scBuildings   ∷ ![BuildingEntry]
    , scLocations   ∷ ![LocationEntry]
    , scUnits       ∷ ![UnitEntry]
    , scGroundItems ∷ ![GroundItemEntry]
    } deriving (Show, Eq)

-- * Definition catalog

-- | Whether a unit-definition stat may be authored. A 'DerivedStat' is
--   recomputed from other values when the unit is initialized (for a
--   body-block unit: @strength@ from @strength_base@ and lean mass,
--   @max_hydration@ from body mass, …), so authoring it is rejected.
data StatRule = AuthorableStat | DerivedStat
    deriving (Show, Eq)

data UnitCatalogEntry = UnitCatalogEntry
    { ucStats          ∷ !(HM.HashMap Text StatRule)
    , ucSkills         ∷ !(HS.HashSet Text)
    , ucBodyParts      ∷ !(HS.HashSet Text)
    , ucEquipmentSlots ∷ !(HS.HashSet Text)
    } deriving (Show, Eq)

data ItemCatalogEntry = ItemCatalogEntry
    { icFluidCapacity ∷ !(Maybe Float)
      -- ^ litres a fluid container holds; 'Nothing' = not a fluid
      --   container, so only a zero fill is valid
    , icHoldsItems    ∷ !Bool
      -- ^ an item container (first-aid kit, toolbox) that may own
      --   @contents@
    } deriving (Show, Eq)

data BuildingCatalogEntry = BuildingCatalogEntry
    { bcFootprint     ∷ !Footprint
    , bcBuildWork     ∷ !Float
      -- ^ worker-seconds to complete; build progress lies in
      --   @[0, bcBuildWork]@
    , bcMaterials     ∷ !(HS.HashSet Text)
      -- ^ material item definitions the build consumes
    , bcPowerCapacity ∷ !(Maybe Float)
      -- ^ watt-hours of a power-storage building; 'Nothing' = it
      --   stores no charge
    } deriving (Show, Eq)

-- | A footprint as inclusive tile offsets from the anchor:
--   @dx0 dy0 dx1 dy1@.
data Footprint = Footprint !Int !Int !Int !Int
    deriving (Show, Eq)

data LocationCatalogEntry = LocationCatalogEntry
    { lcFootprint        ∷ !Footprint
    , lcSignificantSlots ∷ !Int
    } deriving (Show, Eq)

-- | Everything validation needs to know about the current definitions.
--   Plain data, so validation never needs a live engine: the runtime
--   adapters build it from the loaded registries, tests build fixtures.
data ScenarioCatalog = ScenarioCatalog
    { catUnits          ∷ !(HM.HashMap Text UnitCatalogEntry)
    , catItems          ∷ !(HM.HashMap Text ItemCatalogEntry)
    , catBuildings      ∷ !(HM.HashMap Text BuildingCatalogEntry)
    , catLocations      ∷ !(HM.HashMap Text LocationCatalogEntry)
    , catFlora          ∷ !(HS.HashSet Text)
    , catMaterials      ∷ !(HS.HashSet Text)
    , catStructurePacks ∷ !(HM.HashMap Text (HS.HashSet Text))
      -- ^ pack name → piece kinds it supplies (floor, ceiling, wall,
      --   post, wire)
    , catInfections     ∷ !(HS.HashSet Text)
    , catKnowledge      ∷ !(HS.HashSet Text)
    } deriving (Show, Eq)

emptyScenarioCatalog ∷ ScenarioCatalog
emptyScenarioCatalog = ScenarioCatalog
    HM.empty HM.empty HM.empty HM.empty HS.empty HS.empty HM.empty HS.empty
    HS.empty

-- * Diagnostics and outcome

-- | What a recoverable diagnostic did to the scenario (requirement 6).
data DiagnosticEffect
    = EntryRejected     -- ^ the whole entry (and what it owns) is gone
    | FieldRejected     -- ^ one field/override dropped; entry retained
    | TilesClipped      -- ^ out-of-bounds tiles of a patch dropped
    | CascadeRejected   -- ^ dropped because an owner or required
                        --   reference target was rejected
    deriving (Show, Eq)

data DiagnosticReason
    = UnknownField
    | InvalidValue !Text            -- ^ what was expected
    | MissingRequired
    | UnknownDefinition !Text
    | DuplicateId !Text
    | DerivedValue
    | EmptyTag
    | MissingReference !Text        -- ^ target id never declared
    | WrongReferenceKind !Text      -- ^ target exists but is the wrong family
    | RejectedReference !Text       -- ^ target declared but rejected
    | OwnerRejected !Text           -- ^ path of the rejected owner
    | AmbiguousBinding !Text        -- ^ the same target bound twice
    | OutsideBounds
    | FootprintOutsideBounds
    | ClippedTiles !Integer         -- ^ how many tiles were dropped
    deriving (Show, Eq)

-- | One recoverable problem. @sdPath@ is the document path of the
--   affected entry or field (@units[1].stats.agility@), which is stable
--   for a given file and is what automated callers match on (D-15).
data ScenarioDiagnostic = ScenarioDiagnostic
    { sdPath   ∷ !Text
    , sdReason ∷ !DiagnosticReason
    , sdEffect ∷ !DiagnosticEffect
    } deriving (Show, Eq)

-- | An unsuccessful load: nothing usable was produced and nothing may be
--   constructed (D-16, D-23, D-44).
data ScenarioFailure
    = ScenarioUnreadable !Text
    | ScenarioSyntaxError !Text
    | ScenarioNotAMapping
    | ScenarioVersionMissing
    | ScenarioVersionMalformed !Text
    | ScenarioVersionUnsupported !Int
    | ScenarioMigrationFailed !Int !Text   -- ^ failing step's source version
    deriving (Show, Eq)

-- | The one result of reading a scenario. A failure is never an empty
--   'Scenario'; a usable scenario always carries its diagnostics.
data ScenarioOutcome
    = ScenarioFailed !ScenarioFailure
    | ScenarioLoaded !Scenario ![ScenarioDiagnostic]
    deriving (Show, Eq)
