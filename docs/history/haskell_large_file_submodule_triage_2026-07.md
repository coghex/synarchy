# Haskell Large-File Submodule Triage

Date: 2026-07-07

Context: `wc -l` over `src`, `app`, and `test` found 43 Haskell files above 500 lines. `synarchy.cabal` uses an explicit `exposed-modules` list, so every module split must update the cabal module list.

| Lines | File | Feasibility | Best split shape |
|---:|---|---|---|
| 4963 | `src/Engine/Scripting/Lua/API/Units.hs` | High | Split Lua unit API by spawn/move/inventory/equipment/stats/selection |
| 1589 | `src/World/Fluid/River/Identify.hs` | Medium | Split river components, mouth selection, coastal breakthrough |
| 1550 | `src/Engine/Scripting/Lua/API/World.hs` | High | Split Lua world API by lifecycle/query/tools/edit/designations |
| 1525 | `src/World/Generate/Chunk.hs` | Medium | Split fluid/lava overlays, soil gates, zoom terrain helpers |
| 1422 | `src/Combat/Resolution.hs` | Medium | Split strike resolution, damage model, wear, events |
| 1229 | `src/Unit/Thread/Movement.hs` | Medium | Split climb/fall/leap/path advance/timers |
| 1159 | `src/Unit/Thread/Command.hs` | Medium | Split command dispatch from spawn inventory/body derivation |
| 1003 | `src/Engine/Scripting/Lua/API/Buildings.hs` | High | Split YAML/spawn/placement/ghost/query/selection |
| 951 | `src/World/Geology/Timeline.hs` | Medium | Split top-level orchestration from epoch loops and helpers |
| 951 | `src/World/Geology/Coastal.hs` | High | Already sectioned; split erosion, distance field, breach, smoothing |
| 948 | `src/World/Hydrology/Simulation.hs` | Medium | Split grid build, priority flood, flow sim, lake dedup |
| 943 | `src/Engine/Scripting/Lua/API/WorldQuery.hs` | High | Split terrain/fluid/river/chunk/debug query APIs |
| 911 | `src/Engine/Scripting/Lua/Message.hs` | Medium | Split message handling from GPU upload/render-cache side effects |
| 878 | `src/World/Render/Quads.hs` | Medium | Split water slope helpers, cached world quads, cursor quads |
| 847 | `src/World/Plate.hs` | High | Already sectioned; split generation, wrapping, elevation, profiles |
| 840 | `src/World/Thread/Command/Save.hs` | Medium | Split save path, load path, restore-id mapping |
| 808 | `src/World/Geology/Timeline/Types.hs` | Medium | Split event bbox/river events/feature-volcano types carefully |
| 801 | `src/World/Thread/Command/Edit.hs` | High | Split terrain, structure, vegetation, dig/spoil, fluid edits |
| 788 | `src/World/Thread/Command/Cursor.hs` | High | Split mine/construct/chop/till/plant/select handlers |
| 787 | `src/Engine/Scripting/Lua/API.hs` | High | Split registration blocks by Lua table/domain |
| 787 | `src/Combat/Wounds.hs` | Medium | Split constants/infection/healing/bleeding/tick outcome |
| 767 | `src/World/Slope.hs` | High | Already sectioned; split compute, roughness, face maps, edge patching |
| 729 | `src/World/Fluid/Lake/Identify.hs` | Medium | Split basin identify, graben logic, helpers |
| 726 | `src/World/Save/Types.hs` | Low-Med | Serialization-sensitive; split snapshots only with care |
| 701 | `src/Engine/Scripting/Lua/API/Equipment.hs` | High | Split equip/unequip/modifiers/render fields |
| 689 | `src/Engine/Scripting/Lua/API/Forage.hs` | Medium | Split query, harvest rolls, yield spawning |
| 686 | `src/UI/Tooltip.hs` | Medium | Split state/update, layout, rendering helpers, lock API |
| 662 | `src/World/ZoomMap/Cache.hs` | Medium | Split cache build, pixel generation, color/veg/ice helpers |
| 656 | `app/Main.hs` | High | Split dump mode, headless mode, graphical boot, CLI parsing |
| 654 | `src/Unit/Types.hs` | Low-Med | Central types; split only if cycles stay clean |
| 640 | `src/Engine/Scripting/Lua/API/Craft.hs` | High | Split recipe API, execute API, bill API |
| 638 | `src/Engine/Scripting/Lua/API/Items.hs` | High | Split item defs, ground items, rendering helpers |
| 635 | `src/UI/Manager.hs` | High | Already sectioned; split page/element/hierarchy/focus/query/text |
| 633 | `src/World/Geology/Erosion.hs` | Medium | Split erosion math, sediment selection, climate lookup |
| 630 | `src/Engine/Scripting/Lua/Thread.hs` | Medium | Split debug console builtins, command exec, Lua loop |
| 622 | `src/World/Geology/Timeline/RiverTrace.hs` | High | Already sectioned; split tracing, unwrap, subdivision, coast, noise |
| 570 | `src/World/Weather/Generate.hs` | Medium | Split climate builder, ocean regions, freshwater sources |
| 563 | `src/World/Geology/Timeline/River.hs` | High | Split reconciliation, evolution, merging, source diversity |
| 537 | `src/Engine/Core/Log.hs` | High | Split types/config/env parsing/formatting helpers |
| 532 | `src/Engine/Input/Thread.hs` | Medium | Split callbacks/loop from state update helpers |
| 525 | `src/Engine/Graphics/Font/Load.hs` | High | Already sectioned; split atlas generation, SDF, GPU upload |
| 514 | `src/World/Generate/Config.hs` | High | Split YAML types/defaults/load/save/normalization |
| 505 | `src/World/Generate/Timeline.hs` | Low-Med | Only barely over threshold; split after larger wins |

Best first Haskell targets: the Lua API binding modules, `World.Thread.Command.Cursor/Edit`, `UI.Manager`, `World.Slope`, `World.Plate`, and `app/Main.hs`. They already have clear section boundaries and should split with low behavioral risk.

Avoid starting with `World.Save.Types`, `Unit.Types`, or the central worldgen pipeline files unless the issue is tightly scoped, because those are more likely to expose serialization, import-cycle, or determinism problems.
