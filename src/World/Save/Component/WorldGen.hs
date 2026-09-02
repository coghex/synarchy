-- | The worldgen wire-contract FAÇADE (issue #760, save-overhaul B2;
--   split into owners by #2098).
--
--   The @"world-pages"@ component embeds a page's
--   'World.Generate.Types.WorldGenParams', a mutable runtime record whose
--   nested worldgen config/state sub-records evolve as features land. Per
--   the component frozen-DTO boundary rule (see
--   'World.Save.Component.Types' — the ONE place that rule is stated),
--   each of those nested records that is itself LIVE runtime state with
--   no independent save-governance discipline is mirrored by a
--   component-owned DTO with an explicit, field-by-field @to…@/@from…@
--   conversion, so a field added / dropped / reordered on any live record
--   surfaces as a compile error in its conversion rather than as silent
--   byte drift in a shipped v1 save.
--
--   This module declares nothing. It re-exports the worldgen DTO graph
--   from four owners, so a downstream module keeps importing one name and
--   does not have to learn the internal topology. The owners, in
--   dependency order (each depends only on the ones above it):
--
--   ["World.Save.Component.WorldGenClimate"]
--     Generation and climate LEAVES: 'TectonicPlateDTO',
--     'CalendarConfigDTO', 'SunConfigDTO', 'MoonConfigDTO',
--     'ClimateParamsDTO', 'TimelineParamsDTO', 'OreLeversDTO', and the
--     complete recursively frozen 'ClimateStateDTO' tree.
--
--   ["World.Save.Component.WorldGenNaming"]
--     Names and places: 'NameExprDTO', 'EtymologySourceDTO',
--     'AbsBoundsDTO', the encounter shapes, the current and historical
--     location instances and tables, and ALL river-name shapes, current
--     and historical.
--
--   ["World.Save.Component.WorldGenCurrent"]
--     The currently written 'WorldGenParamsDTO' and its exhaustive live
--     conversions.
--
--   ["World.Save.Component.WorldGenHistory"]
--     Every decode-only 'WorldGenParamsDTOv1' … 'WorldGenParamsDTOv6'
--     shape and the migration conversions that carry them forward.
--
--   Which live records are frozen, and which are deliberately reused as
--   leaves, is the worldgen-specific reading of that boundary rule and is
--   recorded here because it spans the owners:
--
--   Frozen (live records lacking their own documented save-schema
--   discipline):
--
--   - 'World.Generate.Types.WorldGenParams' → 'WorldGenParamsDTO'
--   - 'World.River.Naming.RiverName' → 'RiverNameDTO' (and its table →
--     'RiverNamesDTO')
--   - 'World.Plate.Types.TectonicPlate'  → 'TectonicPlateDTO'
--   - 'World.Time.Types.CalendarConfig'  → 'CalendarConfigDTO'
--   - 'World.Time.Types.SunConfig'       → 'SunConfigDTO'
--   - 'World.Time.Types.MoonConfig'      → 'MoonConfigDTO'
--   - 'World.Weather.Types.ClimateParams' → 'ClimateParamsDTO'
--   - 'World.Geology.Timeline.Types.TimelineParams' → 'TimelineParamsDTO'
--   - 'World.Geology.Ore.Types.OreLevers' → 'OreLeversDTO'
--   - 'World.Weather.Types.ClimateState' → 'ClimateStateDTO', recursively:
--     'ClimateGridDTO', 'RegionClimateDTO', 'SeasonalClimateDTO',
--     'OceanGridDTO', 'OceanCellDTO', 'WaterMassDTO', 'OceanCurrentDTO',
--     'ThermohalineCellDTO', 'AtmoGridDTO', 'WindCellDTO',
--     'MoistureCellDTO', 'PressureSystemDTO', 'SurfaceBudgetDTO'.
--     The recursion is transitive precisely because a shallow
--     'ClimateStateDTO' that re-embedded live @OceanGrid@/@AtmoGrid@
--     would still drift when THOSE gained a field — the boundary rule's
--     "recurse the freezing as needed" clause.
--
--   Reused as LEAVES, NOT mirrored, exactly per the boundary rule:
--
--   - 'World.Geology.Timeline.Types.GeoTimeline' — a bulk immutable
--     worldgen-OUTPUT table that carries its OWN in-source, documented
--     positional-save-schema freeze discipline (its fields are annotated
--     with the save version each landed in — @gtCoastal@ "save v25",
--     @gtSeabed@ "save v26", @gtWorldOcean@ "save v27", @gtOreDeposits@
--     "save v30", and an explicit "keep field order stable (save
--     schema)" contract), so it is already an independently-governed
--     persistence shape whose change requires a deliberate schema bump —
--     the SAME leaf exception the boundary rule grants
--     @BuildingInstanceSnapshot@/@UnitInstanceSnapshot@. Freezing its
--     ~30-type tree would be no safer and pure gold-plating. This is the
--     exact line separating it from @ClimateState@, which carries no such
--     in-source discipline and is therefore frozen above.
--   - The content-collection aliases 'World.Ocean.Types.OceanMap'
--     (@HashSet ChunkCoord@), 'World.Ocean.Types.OceanDistMap'
--     (@HashMap ChunkCoord Int@) and
--     'Location.Overlay.Types.LocationOverlay' (@HashMap ChunkCoord Text@),
--     and the location-flag sets (@HashSet ChunkCoord@) —
--     content-reference collections with no record identity that could
--     gain / reorder a field.
--   - Leaf id / coordinate / enum references inside the frozen records:
--     'World.Material.Id.MaterialId' (TectonicPlate),
--     'World.Weather.Types.ClimateCoord' (a coordinate like
--     'World.Chunk.Types.ChunkCoord'), and the append-only enums
--     @PressureType@ / @SurfaceType@.
--
--   Every DTO's field order EXACTLY mirrors its live record's declaration
--   order (and 'World.Generate.Types.WorldGenParams''s manual 'Serialize'
--   field order), so the derived cereal layout is byte-identical to
--   embedding the live records directly — the frozen tracked fixture in
--   "Test.Headless.World.Save.Components" stays valid across this
--   change.
--
--   Splitting the graph across owners moved declarations only. No wire
--   fact — constructor order, field order, field type, strictness,
--   arity, 'Serialize' derivation, or current-versus-historical status —
--   changed, and each one still has exactly one definition.
module World.Save.Component.WorldGen
    ( WorldGenParamsDTO(..)
    , WorldGenParamsDTOv1(..)
    , WorldGenParamsDTOv2(..)
    , WorldGenParamsDTOv3(..)
    , WorldGenParamsDTOv4(..)
    , WorldGenParamsDTOv5(..)
    , WorldGenParamsDTOv6(..)
    , WorldGenParamsDTOv7(..)
    , NameExprDTO(..)
    , EtymologySourceDTO(..)
    , RiverNameDTO(..)
    , RiverNamesDTO(..)
    , RiverNameDTOv1(..)
    , RiverNamesDTOv1(..)
    , AbsBoundsDTO(..)
    , LocationInstanceDTO(..)
    , LocationInstancesDTO(..)
    , LocationInstanceDTOv1(..)
    , LocationInstancesDTOv1(..)
    , LocationInstanceDTOv2(..)
    , LocationInstancesDTOv2(..)
    , LocationInstanceDTOv3(..)
    , LocationInstancesDTOv3(..)
    , LocationEncounterDTOv1(..)
    , LocationSignificantItemDTO(..)
    , LocationInstanceDTOv5(..)
    , LocationInstancesDTOv5(..)
    , LocationInstanceDTOv4(..)
    , LocationInstancesDTOv4(..)
    , LocationEncounterOccupantDTO(..)
    , LocationEncounterDTO(..)
    , TectonicPlateDTO(..)
    , CalendarConfigDTO(..)
    , SunConfigDTO(..)
    , MoonConfigDTO(..)
    , ClimateParamsDTO(..)
    , TimelineParamsDTO(..)
    , OreLeversDTO(..)
    , SeasonalClimateDTO(..)
    , RegionClimateDTO(..)
    , ClimateGridDTO(..)
    , OceanCellDTO(..)
    , WaterMassDTO(..)
    , OceanCurrentDTO(..)
    , ThermohalineCellDTO(..)
    , OceanGridDTO(..)
    , WindCellDTO(..)
    , MoistureCellDTO(..)
    , PressureSystemDTO(..)
    , AtmoGridDTO(..)
    , SurfaceBudgetDTO(..)
    , ClimateStateDTO(..)
    , toWorldGenParamsDTO
    , fromWorldGenParamsDTO
    , fromWorldGenParamsDTOv1
    , toWorldGenParamsDTOv1
    , fromWorldGenParamsDTOv2
    , toWorldGenParamsDTOv2
    , fromWorldGenParamsDTOv3
    , toWorldGenParamsDTOv3
    , fromWorldGenParamsDTOv4
    , toWorldGenParamsDTOv4
    , fromWorldGenParamsDTOv5
    , toWorldGenParamsDTOv5
    , fromWorldGenParamsDTOv6
    , toWorldGenParamsDTOv6
    , toWorldGenParamsDTOv7
    , fromWorldGenParamsDTOv7
    , toEtymologySourceDTO
    , fromEtymologySourceDTO
    , toRiverNamesDTO
    , toLocationInstancesDTOv3
    , toLocationInstancesDTOv4
    , toLocationInstancesDTOv5
    , fromLocationInstancesDTOv5
    ) where

import World.Save.Component.WorldGenClimate
import World.Save.Component.WorldGenCurrent
import World.Save.Component.WorldGenHistory
import World.Save.Component.WorldGenNaming
