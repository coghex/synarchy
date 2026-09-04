-- | Page-scoped world components (issue #760, save-overhaul B2). Each
--   carries a slice PER world page, keyed by 'WorldPageId', and every
--   one is validated against the authoritative page set the
--   @"world-pages"@ component establishes (requirement 8).
--
--   This module declares nothing. Since #2135 it is the stable
--   compatibility FAÇADE over three owner modules, so a downstream
--   module keeps importing one name and does not have to learn the
--   internal topology. The owners, in dependency order:
--
--   ["World.Save.Component.PageCore"]
--     @"world-pages"@ (required, page-set authority) — per page:
--     identity, generation params, dates/clocks, map mode, and the
--     page's OWN remembered camera position. Owner: the world page.
--     Boundary reason: this is the spine every other page-scoped
--     component's page set is checked against; the world-generation seed
--     lives in its gen params (requirement 10). No dependencies — it is
--     the root of the page dependency graph, and therefore also the home
--     of @orderedPages@, the canonical (page-id ascending) encode order
--     its two siblings share.
--
--   ["World.Save.Component.PageEdits"]
--     @"world-edits"@ (required) — per page: the terrain + structure
--     edit log. Owner: the world edit layer. Boundary reason: player
--     terrain/structure modifications are a distinct, replay-on-load
--     concern.
--
--   ["World.Save.Component.PageActivity"]
--     @"world-activity"@ (required) — per page: designations (mine/
--     construct/chop/till/plant), flora harvests, crop plots, ground
--     items, and spoil piles. Owner: the mutable-world-activity layer.
--     Boundary reason: requirement 2 bullet 4's "designations, jobs,
--     progress, flora, crops, ground items, spoil" — the transient-ish
--     but persisted world activity, grouped away from the terrain spine
--     and the entity managers.
--
--   The dependency edges are one-way and shallow: each sibling imports
--   'World.Save.Component.PageCore.orderedPages' and nothing else of
--   each other, neither imports this façade, and the apply scaffolding
--   they share ('World.Save.Component.Types.applyPageSlices') already
--   lives in the module all three depend on — so no logic is duplicated
--   to avoid an edge.
--
--   Every slice list is encoded in canonical (page-id ascending) order
--   so identical input produces identical bytes (requirement 10).
--
--   Requirement 4 — the on-disk contract is FROZEN, distinct from every
--   mutable runtime record. NONE of the evolving live gameplay records
--   is embedded directly; each is mirrored by a component-owned DTO with
--   an explicit, reviewable field-by-field conversion ('to…'/'from…'),
--   exactly the discipline "World.Save.Component.EntitySimulation" and
--   "World.Save.Component.EntitySystems" apply to the unit-sim /
--   craft-bill / power-node records. Which live record each owner
--   freezes is listed in that owner's own haddock:
--
--   - 'WorldGenParams' and 'WorldIdentity' → "World.Save.Component.PageCore"
--   - 'WorldEdit' → "World.Save.Component.PageEdits"
--   - the designation, ground-item and spoil records →
--     "World.Save.Component.PageActivity"
--
--   'WorldGenParamsDTO' and its full nested worldgen config/state tree
--   are reached through the "World.Save.Component.WorldGen" façade
--   (imported + re-exported here), which since #2098 re-exports them from
--   four owner modules rather than declaring them itself;
--   'ItemInstanceDTO' is declared beside 'GroundItemDTO' in
--   "World.Save.Component.PageActivity".
--
--   A field/constructor added, dropped, or reordered on any of those
--   live records surfaces in the owning module (or in "…WorldGen") as a
--   compile error in its @from…@ conversion, never as silent byte drift
--   in a shipped v1 save. Per the component frozen-DTO boundary rule
--   (stated in "World.Save.Component.Types"), genuine LEAF references
--   are reused as-is rather than mirrored — each owner's haddock names
--   the ones it reuses. The DTO field order is chosen so the derived
--   cereal layout is byte-identical to the previous direct embedding —
--   the frozen tracked fixture stays valid.
--
--   Splitting the module across owners moved declarations only. No wire
--   fact — component id, version, dependency, accepted input version,
--   decoder dispatch, validation phase, assembly order, constructor
--   order, field order, field type, strictness, arity, 'Serialize'
--   derivation, or current-versus-historical status — changed, and each
--   one still has exactly one definition.
module World.Save.Component.Page
    ( worldPagesCodec
    , worldEditsCodec
    , worldActivityCodec
    , PageCoreDTO(..)
    , WorldPagesDTO(..)
    , PageCoreDTOv1(..)
    , WorldPagesDTOv1(..)
    , PageCoreDTOv2(..)
    , WorldPagesDTOv2(..)
    , PageCoreDTOv3(..)
    , WorldPagesDTOv3(..)
    , PageCoreDTOv4(..)
    , WorldPagesDTOv4(..)
    , PageCoreDTOv5(..)
    , WorldPagesDTOv5(..)
    , PageCoreDTOv6(..)
    , WorldPagesDTOv6(..)
    , PageCoreDTOv7(..)
    , PageCoreDTOv8(..)
    , WorldPagesDTOv7(..)
    , WorldPagesDTOv8(..)
    , PageCoreDTOv9(..)
    , WorldPagesDTOv9(..)
    , PageEditsDTOv1(..)
    , WorldEditsDTOv1(..)
    , PageEditsDTOv2(..)
    , WorldEditsDTOv2(..)
    , WorldPages(..)
    , migrateWorldEditsV1
    , migrateWorldEditsV2
    , migrateWorldPagesV1
    , migrateWorldPagesV2
    , migrateWorldPagesV3
    , migrateWorldPagesV4
    , migrateWorldPagesV5
    , migrateWorldPagesV6
    , migrateWorldPagesV7
    , migrateWorldPagesV8
    , migrateWorldPagesV9
    , PageEditsDTO(..)
    , WorldEditsDTO(..)
    , PageActivityDTO(..)
    , WorldActivityDTO(..)
    , PageActivityDTOv2(..)
    , PageActivityDTOv3(..)
    , WorldActivityDTOv3(..)
    , PageActivityDTOv4(..)
    , WorldActivityDTOv4(..)
    , PageActivityDTOv5(..)
    , WorldActivityDTOv5(..)
    , WorldActivityDTOv2(..)
    , migrateWorldActivityV2
      -- * Frozen leaf DTOs (requirement 4)
    , WorldGenParamsDTO(..)
    , WorldGenParamsDTOv1(..)
    , WorldGenParamsDTOv2(..)
    , WorldGenParamsDTOv3(..)
    , WorldGenParamsDTOv4(..)
    , WorldGenParamsDTOv5(..)
    , WorldGenParamsDTOv6(..)
    , WorldGenParamsDTOv7(..)
    , RiverNameDTO(..)
    , RiverNamesDTO(..)
    , EtymologySourceDTO(..)
    , WorldIdentityDTO(..)
    , WorldIdentityDTOv1(..)
    , WorldIdentityDTOv2(..)
    , LanguageProvenanceDTO(..)
    , toWorldIdentityDTO
    , toWorldIdentityDTOv2
    , WorldEditDTO(..)
    , WorldEditDTOv1(..)
    , WorldEditDTOv2(..)
    , MineDesignationDTO(..)
    , StructurePieceDTO(..)
    , ConstructTargetDTO(..)
    , ConstructDesignationDTO(..)
    , ConstructDesignationDTOv1(..)
    , toConstructDTO
    , fromConstructDTO
    , migrateConstructDesignations
    , ChopDesignationDTO(..)
    , ChopDesignationDTOv1(..)
    , FloraHarvestsDTOv1
    , TillDesignationDTO(..)
    , PlantDesignationDTO(..)
    , PlantDesignationDTOv1(..)
    , CropPlotDTO(..)
    , CropPlotDTOv1(..)
    , ItemStorageDTO(..)
    , ItemInstanceDTO(..)
    , ItemInstanceDTOv1(..)
    , GroundItemDTO(..)
    , GroundItemDTOv1(..)
    , GroundItemsDTO(..)
    , GroundItemsDTOv1(..)
    , SpoilPileDTO(..)
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
    , fromWorldGenParamsDTOv7
    , toWorldGenParamsDTOv7
    , toEtymologySourceDTO
    , fromEtymologySourceDTO
    , toItemInstanceDTO
    , fromItemInstanceDTO
    , toItemInstanceDTOv1
    , migrateItemInstanceDTOv1
    , toGroundItemDTO
    , fromGroundItemDTO
    , toGroundItemsDTO
    , toGroundItemsDTOv1
    , basePageSnapshots
    , blankPageSnapshot
    , applyWorldEdits
    , applyWorldActivity
    , validatePages
    , validateWorldActivity
    ) where


import World.Save.Component.PageCore
import World.Save.Component.PageEdits
import World.Save.Component.PageActivity
import World.Save.Component.WorldGen
