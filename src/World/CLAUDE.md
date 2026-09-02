# src/World/ — generation, hydrology, fluids, rendering

Loaded when you work under `src/World/`. Worldgen is free-hand
territory inside this tree; notify before reaching into engine or Lua.

## Namespaces

- `World.Generate` — terrain generation, chunk creation
- `World.Geology` — tectonic plates, erosion, volcanism, timeline evolution
- `World.Hydrology` — per-Age geological hydrology: flow accumulation,
  river/glacier carving, and the subsurface water table
- `World.Fluid` — global identification of the FINAL rivers, lakes,
  ocean, seabed and ice. The main identifiers read the stitched, settled
  terrain, but `Ocean` and `IceLevel` are prepared earlier from
  pre-stitch grids (`docs/hydrology_pipeline.md` §5). Not runtime
  simulation: fluid only moves in `Sim.Thread` / `Sim.Fluid.Active`
- `World.Flora` — vegetation placement
- `World.ZoomMap.*` builds the zoom cache/atlas at world-init time
  (`World.ZoomMap.Cache.*`, `ChunkTexture`, `ColorPalette`, output types
  in `World.ZoomMap.Types`); `World.Render.Zoom.*` renders from it. The
  dependency runs one way — nothing under `World/ZoomMap/` imports
  `World.Render`.

[`docs/hydrology_pipeline.md`](../../docs/hydrology_pipeline.md) is the
namespace-ownership map for water: the five pipeline stages in order,
which namespace owns each, the two distinct river-carving mechanisms,
where ocean and lake logic live, and a "where does X live?" index. Read
it before adding river, lake, ocean, ice, or water-table logic — the
namespaces do not divide the way their names suggest.

## Tile-coordinate frame at the U seam (#1175/#1230)

Chunks are STORED u-wrapped, so one physical tile has two names near
the seam. ONE contract (stated in full on `World.Render.HitTest` and in
`docs/engine_contracts.md` §Tile-coordinate seam frame): every point
read / mutation / cancellation — picking, designation maps, and the
verbs a worker FINISHES a job with — uses CANONICAL coords and accepts
any alias. RECTANGLES are the exception: a drag's second endpoint is
re-expressed in the anchor's local alias frame (`localizeTileToAnchor`;
Lua `world.localizeTile`) BEFORE any clamp/`min`/`max` — canonicalising
one end alone MEASURED worse than seam-blind behaviour; don't. Terrain
LOOKUPS must `wrapChunkCoordU` before `lookupChunk` (a miss reads as
"not loaded → flat", which for occlusion means "nothing blocks"). Where
a tile is DRAWN is the separate `bestWrapOffset` axis (#1176). Away from
the seam, and in arenas, every step is the identity. Gates: hspec
`--match "World.Render.PickSeam"` / `"World.DesignationSeam"` /
`"a seam-frame unit"`.

## Testing worldgen

Worldgen is the entire cost of the test stack (~10 s per w64
generation; every non-worldgen test is milliseconds).

- **Iteration:** targeted `--match`;
  `python3 tools/world_check.py --quick` (6 seeds, <1 min) for output
  sanity. `--dump` is the
  self-contained way to inspect output (`docs/headless_console.md`).
  worldSize 128 ≈ 30 s, 256 ≈ 2 min, 512 much longer.
- **Worldgen-OUTPUT changes (full tier):**
  `SYNARCHY_FULL_TESTS=1 cabal test synarchy-test-headless`, then
  re-capture baselines `python3 tools/world_baseline.py` (~7 min) and
  re-run world_check. Remember the save-version bump. Baselines
  (`tools/baselines/`) are tracked in git and land in the PR diff; don't
  edit baseline JSON by hand. Worldgen output is bit-identical across
  macOS/aarch64 and Linux/x86_64, so baselines are platform-agnostic; a
  worldgen-output PR that skips its rebaseline fails CI. The variable's
  semantics (wholesale, empty = enabled, CI's selector):
  `docs/engine_contracts.md` §The full test tier.
- **Conventions that keep this fast — don't undo them:** hspec worldgen
  specs share generated worlds via
  `Test.Headless.Harness.sharedWorld env seed size plates` (one engine,
  booted in `Spec.hs`); a spec that mutates its world must `WorldInit` a
  private page; new read-only specs reuse the canonical `42 64 3` world
  unless they need specific geography. `world_check.py` dumps each seed
  once; pass `--runs 3` only when chasing a suspected race. Don't add
  per-spec `WorldInit`s of worlds that already exist in the suite, and
  don't grow the baseline seed list without tagging the quick tier.
- Profiling (`--enable-profiling -f profile --builddir=dist-prof`):
  `+RTS -N1` is mandatory (the profiled RTS segfaults under sparked
  worldgen parallelism); drive via `--headless` + `world.waitForInit`,
  never `--dump` (its watchdog can force-kill mid-profile and truncate
  the `.prof`). Recipe:
  `docs/history/worldgen_timeline_profile_2026-07.md`.

## Locations, naming, rivers

Location instances, clearance, discovery, map icons, per-unit knowledge,
and river/location naming each have a section in
`docs/engine_contracts.md`; the root `CLAUDE.md` §Domain contracts lists
the rule on sight and the gate for each. River identity is
`(WorldPageId, GeoFeatureId)`; `World.River.Identity` is the ONE
event/feature pairing and it is CHECKED before it is trusted.

## Persistence

Save-format work under `src/World/Save/` has its own `CLAUDE.md`.
