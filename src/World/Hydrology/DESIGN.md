# Water Table & River Rework — Design Document

**Status:** Design, pre-implementation. Updated 2026-05-22 (v3).

## 1. Mental model

The world has a **water table**: a 2D field (one value per tile) describing the elevation of the saturation horizon. Below the water table, ground is saturated (groundwater). Above it, dry.

**The key insight:** in most non-arid terrain the water table sits 1–5 tiles below the surface, *following terrain shape*. Rivers are not generated as separate water objects — they are simply tiles where the terrain has been carved by geological erosion *below* the water table, exposing groundwater as surface water. Lakes are the same: depressions deep enough to intersect the water table.

This means:
- **Sloped banks emerge naturally** — terrain slopes down to a channel; water table follows terrain; water surface sits where the channel meets the water table. No "wallCarve" math; banks just are.
- **Junction discontinuities go away** — two rivers meeting at the same point share one water-table value. Their surfaces match by construction.
- **Digging near a river hits water** — because the water table is high there. Subsurface query is `z ≤ wt[x,y]`.
- **Climate drives river behavior** — wet climates have shallow water tables (rivers everywhere); arid climates have deep water tables (rivers only in deeply carved valleys).

## 2. Goals

1. Decouple **channel carving** (terrain shaping) from **surface water elevation** (water-table fill).
2. Compute a per-tile water-table field from terrain + climate + boundary conditions (ocean / lake / carved channel).
3. Surface water (river / lake / ocean) is **derived** from `wt[t] ≥ terrain[t]`, classified by geometry.
4. Subsurface query: `(x, y, z) is wet ⇔ z ≤ wt[x, y]`. Digging into wet rock reveals water.
5. Multi-period carving still works — periods continue to lower terrain; water table is computed once after the last period.
6. Per-chunk computation with neighbor border, no global pre-compute.

## 3. Non-goals (for the initial rework)

- Dynamic water table that responds to player digging / damming.
- Real Darcy-flow PDE / aquifer modeling / perched water pockets.
- Saltwater intrusion / brackish zones.
- Visible "saturated rock" tile type (wet underground tiles look identical to dry until dug).

## 4. Stretch goals (deferred)

Tagged for future work — *don't implement during the main rework*:

- **(S1) Seasonal water table.** Summer/winter ±delta on the annual mean. Affects river widths per season. The `waterTableSummer/Winter` fields stay around as inputs but the rework consumes the annual mean.
- **(S2) Material-driven drainage.** High-drainage materials (limestone, sandstone, karst) pull the water table down → encourage caves and dry uplands. Low-drainage materials (clay, granite, shale) keep it high. The depth-from-climate function gains a material-modulated term.
- **(S3) Cave formation in karst.** Once (S2) lands, areas with limestone + low water table + active erosion get explicit cave systems. Out of scope until (S2) is in.
- **(S4) Multi-period carving evolution — wide layered valleys.** Each geological period re-runs the simulation, finds rivers on the *current* terrain (which has been eroded since the last period), carves again. Combined with glacier carving running on the same terrain in parallel periods, this produces the layered-plateau valley shape real-world river valleys have (e.g., the terraces lining the Rhine or Columbia). Today rivers carve once per period using stale source data; the architecture allows for re-tracing each period after erosion has finished, but the timeline driver doesn't trigger that. Elevate to "S4-priority" because the user explicitly called it out as part of the river vision. **Implementation note:** the new architecture *enables* this naturally because carving no longer commits a water surface — repeated carving just lowers terrain more, and the water table fills the result. The blocker is the timeline scheduler, not the water/carving code.
- **(S5) Lava as fluid.** Today lava is treated alongside water as a fluid type, with override priority. After the rework lava remains a separate system (zombie code in `Fluid/`); the only invariant preserved is **"lava always overrides other fluids at the same tile"**. Re-architecting lava onto the same water-table-ish abstraction is a future call.

## 5. Architecture

### 5.1 Core data — per chunk

```haskell
data LoadedChunk = LoadedChunk
  { ...
  , lcWaterTableMap ∷ !(VU.Vector Int)
    -- ^ Per-tile water-table elevation (z-coord).
    --   A 3D position (lx, ly, z) is wet iff z ≤ lcWaterTableMap[lx, ly].
    --   When wt[t] ≥ terrainSurface[t], the tile shows surface water.
  , lcWaterClassMap ∷ !(VU.Vector Word8)
    -- ^ Per-tile water classification: river / lake / ocean / dry.
    --   Used to pick the correct fluid type when wt ≥ terrain.
  ...
  }
```

Both vectors are **transient** (regenerated from `WorldGenParams` + edits at chunk load). No save schema impact from these fields alone.

### 5.2 Computation — per chunk, with neighbor border

Compute each chunk's water table over a `chunkSize + 2*chunkBorder` region (same border as coastal erosion: 14 tiles each side). The border samples directly from neighbor terrain + climate + carved-channel data, all of which are derivable from `WorldGenParams` without recursive chunk loading.

This is the user's "any generation could have a look at the surrounding 4 chunks water table estimates for crossboundaries" — except instead of computing rough wt estimates per neighbor, we **share the border region**: each chunk's wt is computed using its neighbors' terrain + climate as boundary conditions, but the wt result is stored only for the chunk-interior `chunkSize × chunkSize` tiles. Adjacent chunks doing their own computes will see the same border inputs and arrive at compatible-but-not-bit-identical results. Tiny ε-discrepancies at chunk lines read as natural texture, not artifacts.

**Crucial:** the **river-segment skeleton is global** (already computed once at world init by `Hydrology/Simulation.hs`). Channel surface elevations are known globally — they're not part of each chunk's compute. This solves the "river starts in chunk C, drains 1000 tiles to ocean, chunk C needs to know about sea level" problem: the river segment already encodes the downstream condition.

### 5.2.1 The hydrology simulation is *history*, not state

A strong architectural rule: outputs from `Hydrology/Simulation.hs` (lake basin lists, flow accumulation, drainage networks) describe *the moment when rivers were last traced*. By the time the world reaches end-of-timeline:

- Erosion has reshaped the terrain since the simulation ran.
- Multiple periods may have rewritten the network.
- The simulation's "lake basin at tile X" might not even be a depression any more.

The new architecture **forgets** those outputs after they have served their one purpose: emitting `CarveChannel` events. The current state of the world — including which tiles are lakes — is derived **only** from terrain + climate + carved-channel skeleton + water-table compute. No legacy basin tables, no flow-accumulation re-reads.

Practically this means **lakes emerge naturally from the water-table algorithm**: a depression where `wt > terrain` and the tile is not a channel and not connected to the ocean is a lake. We never look at `frLakes` from the simulation output again.

### 5.3 Pipeline

```
[Geological Timeline — per period, runs at world init, same as today]
  ├─ Flow simulation on current terrain (existing — consumed once and forgotten)
  ├─ Source identification, tracing (existing, simplified)
  ├─ Emit CarveChannel events — TERRAIN ONLY, no water surface
  └─ Erosion + other events
              ↓
[Channel skeleton is stored in WorldGenParams as today, but
 RiverSegment.rsWaterStart/rsWaterEnd are DELETED — segments are
 pure geometry plus a per-segment carved channel-floor elevation]
              ↓
[Per-chunk generation — runs on demand at chunk load]
  ├─ Compute terrain over bordered region (existing)
  ├─ Lookup climate over bordered region (existing)
  ├─ Find which river segments overlap this bordered region (existing)
  ├─ Re-derive flow direction over the bordered region (local, from terrain)
  ├─ Compute lcWaterTableMap over bordered region:
  │    (a) initialize: wt[t] = terrain[t] - depthFromClimate(climate[t])
  │    (b) pin hard boundary conditions:
  │          ocean tiles (terrain ≤ seaLevel)     → wt = seaLevel
  │          carved channel tiles                  → wt = channel-floor elev
  │    (c) propagate via flow-direction sweep:
  │          wt[t] = max(wt[t], wt[downstream-of-t])
  │    (d) NO basin pin — depressions where wt > terrain
  │        will be classified as lakes in the next step.
  ├─ Compute lcWaterClassMap (river / lake / ocean / dry per tile)
  └─ Store both vectors in LoadedChunk (interior chunkSize × chunkSize only)
              ↓
[Fluid placement — runs at chunk gen, simplified]
  └─ For each tile t in chunk:
       if wt[t] ≥ terrain[t]:
         place surface fluid at z = wt[t], type = lcWaterClassMap[t]
       else: dry surface
       (Lava still placed by its own system, overriding the above)
              ↓
[Subsurface query — new]
  └─ isWet(x, y, z) = z ≤ wt[localOf(x,y)]
       Used by digging, cave generation (later), etc.
```

### 5.4 The propagation algorithm (per chunk)

```
Inputs:
  terrain     :: bordered tile field        -- already computed
  climate     :: bordered tile field        -- looked up from climate grid
  channelMap  :: bordered tile field        -- "is this tile inside a carved channel?"
                                            -- derived from segments overlapping this chunk
  channelFloor:: per-channel-tile elevation -- from segment data (carved floor)
  flowDir     :: bordered tile field        -- re-derived locally from terrain

Output:
  wt :: bordered tile field

Algorithm:
  1. for each tile t:
       wt[t] := terrain[t] - depthFromClimate(climate[t])
  2. for each tile t:
       if terrain[t] ≤ seaLevel:   wt[t] := seaLevel
       elif channelMap[t]:          wt[t] := channelFloor[t]
       -- interior tiles keep their step-1 initial value;
       -- no basin pin — depressions will rise naturally in step 3.
  3. Sort tiles by terrain elevation, ASCENDING (lowest to highest).
     For each tile t in that order:
       d := flowDir[t]  -- downstream neighbor (or NULL if t is a sink)
       if d is non-null and d is in bounds:
         wt[t] := max(wt[t], wt[d])
       -- order rationale: when we process t, t's downstream neighbor d
       -- has lower terrain so has already been fully processed in this
       -- sweep; wt[d] is its final value. t inherits that as a floor.
       -- result: wt is monotone-nondecreasing going upstream.
  4. (No clip step.) wt is permitted to exceed terrain — those tiles
     are surface water. Classification in §5.6 decides river / lake / ocean.
```

Step 3's sort+sweep is O(N log N) per chunk; with chunk area ≈ 44² = 1936 tiles in the bordered region, this is trivial.

The dropped clip step (step 4 in v2) is the change that makes lakes emerge from the algorithm. Where terrain dips into a closed depression, every tile inside the depression has the same downstream neighbor (the outflow lip). Propagation lifts every tile in the depression to that lip elevation — exactly what a real lake does. No basin detection needed.

### 5.5 depthFromClimate — the climate model

```haskell
depthFromClimate ∷ LocalClimate → Int
depthFromClimate c =
    let p = lcPrecip c            -- 0..1
        t = lcTemp c              -- °C
        -- Default: shallow. Most of the world is wet enough that
        -- the water table sits just below the surface — what
        -- people call "groundwater near grade."
        baseDepth
          | p > 0.40 = 2          -- humid
          | p > 0.20 = 5          -- moderate
          | p > 0.10 = 15         -- semi-arid
          | otherwise = 40        -- arid
        -- Hot evaporation pulls the table further down.
        aridPenalty
          | t > 25 ∧ p < 0.15 = 40
          | t > 20 ∧ p < 0.25 = 15
          | otherwise         = 0
    in baseDepth + aridPenalty
```

Strawman numbers — tune visually. The shape is: **default is shallow (2–5 tiles), arid is deep (40–80 tiles)**. Material modulation (stretch goal S2) goes here when ready.

### 5.6 lcWaterClassMap — choosing the fluid type

For each tile where `wt[t] ≥ terrain[t]`, classify as:

```
classify t =
  if terrain[t] ≤ seaLevel && connected-to-ocean(t):  Ocean
  elif channelMap[t]:                                  River
  else:                                                Lake
                  -- depression that wasn't explicitly carved;
                  -- the propagation lifted wt above terrain here
```

Note the absence of any "basinMap" — lakes are *whatever depression the water table fills that isn't a channel or ocean*. This is the v3 simplification.

**Channel mask is dilated proportionally to river width.** Each carved channel has an associated width (or flow rate, from segment data); the mask region extends `riverWidth` tiles either side of the segment centerline, so the total mask region is approximately **2× the carved channel width**. Strawman: for a 4-tile-wide carved channel, the mask region is 12 tiles wide (4 channel + 4 each side). For a 10-tile-wide river, 30 tiles total.

This solves two problems simultaneously:
- **No more "short lake blob" rivers** — even short coastal rivers extend their channel classification across the full width of any wide carved area, so they read as river/delta, not lake.
- **Riverbanks classified correctly** — tiles immediately adjacent to a river, where the water table tapers down to meet the bank, classify as river-bank rather than lake.

If river width data isn't already exposed on `RiverSegment`, surface it before Phase B. If only flow rate is available, derive width with the existing flow-to-width function used by the carving pipeline.

## 6. Migration plan

### Phase A — water table foundation (~1 day)
1. New module `src/World/Hydrology/WaterTable.hs` exporting `computeWaterTable :: BorderedInputs → VU.Vector Int`.
2. Add `lcWaterTableMap` field to `LoadedChunk`. Bump `currentSaveVersion`.
3. Wire compute into `Generate/Chunk.hs` after terrain + climate are ready.
4. **Don't change fluid placement yet.** Verify by dumping wt values from `--dump`.

### Phase B — fluid placement from water table (~1 day) **LANDED 2026-05-22**
1. ✓ `composeFluidMap` rewritten in `Generate/Chunk.hs` — water-table-driven, three-way classification (Ocean / River / Lake) via channel mask + ocean BFS reach.
2. ✓ `computeBorderedChannelMask` produces a terrain-aware ribbon (rsWidth either side) over the bordered region; channel mask sliced for interior. Terrain-aware so adjacent cliff/valley tiles don't both classify as river.
3. ✓ `lcWaterClassMap` not actually needed — classification computed inline at fluid placement time.
4. ✓ Lava order in composeFluidMap is `lavaFluid `preferFluidMap` waterFluid` — lava always wins.
5. ✓ `computeChunkRiversStatic` / `computeChunkLakes` / old `oceanFluid` BLI gradient code paths are now dead. Left in repo for Phase E cleanup.
6. ✗ `rsWaterStart` / `rsWaterEnd` deletion deferred to Phase E (untouched in Phase B — ripples into Carving, RiverTrace, save schema).
7. **Sink-fill added in v3**: `spillwayElev` in `WaterTable.hs` pins tiles with no strictly-lower neighbor to their min neighbor elevation. Without this, bowls couldn't fill. The user's mental model "lakes emerge from propagation" requires this; bare propagation only propagates downstream-inheritance.

#### Verified at end of Phase B
- w32 max river-jump 6 tiles (was 10 in old system).
- w32 max lake-jump 1 tile (was 20 in old).
- w64 max river-jump 5 (was 9).
- Spurious "short lake blob rivers" eliminated — w32 inland water count dropped from ~40k to ~8k.
- Lava override works (volcanic features still place lava on top of any water).

### Phase C — carving rewrite (~1 day)

### Phase C — carving rewrite (~1 day)
1. Rewrite `Hydrology/River/Carving.hs` as pure terrain lowering with lateral slope-rate (no water surface output).
2. Remove or repurpose `Generate/Timeline.hs`'s `smoothCliffs` pass — sloped-bank carving should make it redundant; verify before deleting.
3. Eyeball test.

### Phase D — subsurface query API (~½ day) **LANDED 2026-05-22**
1. ✓ `isSubsurfaceWet :: LoadedChunk → Int → Int → Int → Bool` + `waterTableAtTile` exported from `Hydrology/WaterTable.hs`.
2. ✓ Plumbed into `Edit/Apply.hs::applyEdit (WeDeleteTile ...)`. After the dig: if the newly exposed terrain top is at or below the water table and the column has no fluid, initialize a Lake-class FluidCell at `wt`. Existing fluid (river/lake/ocean) is preserved.
3. ✓ Wet tiles look identical to dry tiles pre-dig (rendering pipeline unchanged). Water appears on the dig that exposes saturation.
4. ✓ Side fix: dig now operates on `lcTerrainSurfaceMap` instead of `lcSurfaceMap`, so successive digs through revealed water continue to advance terrain instead of no-oping against the water column. Pre-existing edge case made more visible by Phase D's water-reveal logic — fixed as part of the same change.

### Phase E — cleanup (~½ day) **LANDED 2026-05-22**
1. ✓ `Fluid/River.hs` reduced from ~680 lines to ~100 — kept only `riverNearChunk`, `hasAnyRiverQuick`, `fixupSegmentContinuity` (with water-surface logic stripped). Deleted `computeChunkRivers`, `computeChunkRiversStatic`, `riverFillFromSegmentWithDist`, `pickLowestWater`, and all their ST-based helpers.
2. ✓ `Fluid/Lake.hs` reduced to just `hasAnyLakeQuick` + proximity helpers; deleted `computeChunkLakes`, `fillLakeFromFeature`, `fillLakePool`.
3. ✓ `ZoomMap/Cache.hs` no longer imports `computeChunkRivers` / `computeChunkLakes`.
4. ✓ `RiverSegment` no longer carries `rsWaterStart` / `rsWaterEnd`. NFData instance and all 5 constructor sites updated.
5. ✓ `Geology/Timeline/RiverTrace.hs::poolWaterSurface` deleted; its caller cleaned up.
6. ✓ Lua API `world.getRivers()` no longer exposes `waterStart`/`waterEnd` (with user authorization; no Lua scripts referenced them).
7. ✗ `Generate/Timeline.hs::smoothCliffs` pass NOT deleted — verified still useful at river/mountain transitions where natural terrain rises sharply beside a carved channel. Slope-based carving from Phase C reduces its work but doesn't eliminate it.

#### Smoke test at end of Phase E
- w64/w32 seed 42 fluid counts IDENTICAL to end-of-Phase-D state. River cross-section flatness unchanged (max-jump 7/18).
- Build clean in both `cabal build lib:synarchy` and `cabal build -f dev synarchy-test-headless`.

## 7. Risks & mitigations

| Concern | Risk | Mitigation |
|---|---|---|
| Cross-chunk water-table seams | Medium | 14-tile border absorbs boundary effects. Global river-segment data fixes the downstream-condition problem. Any visible seam is ε-pixel jitter, indistinguishable from texture noise. |
| Performance: O(N log N) sort per chunk | Low | N ≈ 1936 tiles; trivial. |
| Lake spawning gets weird without its old pipeline | Medium | Lake basins from depression detection still exist as **inputs** to the water-table compute. The classification cleanly preserves "this depression is a lake". |
| Ocean integration | Medium | Ocean tiles pinned to seaLevel as a boundary condition. Existing ocean fluid code reads wt = seaLevel and behaves identically to today. |
| Save schema bump | None (user has no saves) | Bump freely. |
| Lava integration | Low | Lava system is left untouched; the rule "lava placed last, overrides water" is preserved by ordering fluid passes. |

## 8. Decisions log

Resolved 2026-05-22 with user (v2):

| Q | Decision |
|---|---|
| Global or per-chunk water-table compute? | **Per-chunk with neighbor border.** Use existing `chunkBorder = 14`. Global river-segment data feeds downstream boundary conditions. |
| Seasonal water table? | **Annual mean for now.** Seasonal flex is stretch goal S1. |
| Save schema bumps? | **Freely.** User has no saves to preserve. |
| Wet rock visualization? | **Looks identical to dry until dug.** Water becomes visible when the tile is removed. |
| Multi-period flow simulation? | **Keep current cadence in initial rework.** Temporal drainage evolution is stretch goal S4 (elevated priority — user vision). |
| Lava treatment? | **Zombie code, untouched.** Preserve "lava overrides other fluids" invariant. Re-architecting lava onto water-table semantics is stretch goal S5. |

Resolved 2026-05-22 with user (v3):

| Q | Decision |
|---|---|
| Use Hydrology/Simulation's basin output as a boundary condition? | **No.** Simulation outputs are *history*, not authoritative state. Lakes emerge naturally from the water-table propagation step — wherever wt > terrain in a non-channel, non-ocean tile is a lake. Step 2(b) "basin pin" is **deleted from the algorithm.** |
| Channel mask dilation radius? | **2× the carved channel width.** Mask extends `riverWidth` tiles either side of segment centerline. Scales with flow rate via the existing river-width function. |
| Flow-direction field — reuse global or re-derive locally? | **Re-derive locally.** A bordered-chunk-local steepest-descent computation from terrain. Keeps the water-table compute self-contained and decoupled from the simulation's intermediate state. Also opens the door for dynamic water table updates later. |

## 9. Open implementation questions (resolve as we code)

- **What if a channel tile's `channelFloor` is above the bordered region's natural terrain?** Shouldn't happen for properly carved channels but the algorithm should not crash. Probably: `wt[t] := min(channelFloor, terrain[t])` at pin step.
- **River width source.** `RiverSegment` already carries width or flow rate — confirm during Phase B which field is canonical and whether the existing flow-to-width function lives in a reusable module.
- **What happens at the bordered region's outer edge?** Tiles on the outermost border row have no flow neighbor outside the border. Treat them as boundary conditions with wt = their initial step-1 value; downstream-of-self propagates only inward. Acceptable for the ε-noise model.

## 10. Acceptance criteria (eyeball)

When the rework is done, a player generating w64 worlds should see:

- ✓ Rivers with **sloped banks** that follow terrain shape.
- ✓ River junctions with **continuous water surfaces** (no steps at confluences).
- ✓ No "short lake blob" rivers — coastal rivers look like deltas, not stuck cells.
- ✓ Lakes form **only in actual basins**, not where the renderer happens to think there's water.
- ✓ Digging adjacent to a river or lake reveals water (subsurface query works).
- ✓ Arid biomes have **dry-rock interiors** — wells would need to dig deep.
- ✓ Wet biomes have **water near-surface everywhere** — digging anywhere hits water within a few tiles.
- ✓ Lava systems still work as they do today; lava overrides water at shared tiles.

### 10.1 Items to verify visually after implementation

Two architectural decisions whose effects can only be assessed post-implementation:

- **A1 — `RiverSegment` width / flow data availability.** Phase B assumes `RiverSegment` carries enough info to compute channel-mask width (either a width field or a flow rate that maps to width via the existing carving function). Confirm during Phase B prep; if absent, either add a field (small) or derive width from upstream area at chunk time (slightly more code). Either is acceptable.

- **A2 — Long upstream sea-level propagation in arid terrain.** With the "no clip" simplification (§5.4), a single carved channel reaching the ocean pulls every upstream tile along the flow path up to sea level via propagation. In wet terrain this is correct (rivers extend upstream all the way to their sources). In *arid* terrain it may inappropriately drown a desert plateau with a single river running through it — every tile in the flow chain inherits sea-level wt despite being far from real groundwater. Watch for: arid worlds where a single river makes the entire flow path classify as river. Mitigation if observed: clamp wt at each tile by `terrain[t] + maxStandingWater` (a small allowance, e.g., 3 tiles), reintroducing a soft clip that prevents wt from floating wildly above terrain in non-basin non-channel tiles.
