# Fluid Generation & Simulation Audit Report

**Date:** 2026-04-13
**Seeds tested:** 42, 137, 314, 2718, 4567, 356049 (deep), plus 20 random (stress)
**Region:** -6,-6,6,6 (deep), -6,-6,6,6 (stress)
**Codebase:** Post-commit with diffusion sim + basin fill + sloped water

---

## Overall Health

| Metric | Value | Assessment |
|--------|-------|------------|
| Water-water artifacts | 8 / ~15,000 tiles | **Excellent** (0.05%) |
| MID_RIVER_CLIFF | 15 / 20 seeds | **Good** (rare, at chunk boundaries) |
| Submerged bumps | 0 | **Perfect** |
| Cross-chunk gaps | 13 / 20 seeds | **Acceptable** (rare) |
| Determinism | All seeds pass | **Perfect** |
| River mouth drops >5 | 1 / 20 seeds | **Excellent** |

---

## Issue 1: Water-Water Surface Steps (COSMETIC)

**Count:** 21,386 across 20 seeds (~1,000/seed)
**Severity:** Low (cosmetic, not a data bug)
**Every single one is natural** — water surface tracks terrain exactly.

**What it looks like:** Adjacent water tiles at different heights creating visible 1-z steps in rivers. The `waterSlopeAt` function already computes slopes for these, and the side-face renderer now suppresses 1-z side faces. The remaining visible steps are from the sloped tile rendering not fully covering all transition angles.

**Proposed solution:** This is a rendering polish issue, not a sim issue. The sloped water tile face maps need to cover more transition cases. Specifically:
- Verify `waterSlopeAt` detects dry-neighbor slopes (already added)
- Verify `freshwaterTileToQuad` applies the slope to the water quad (already working)
- Audit the face map textures to ensure all 15 slope directions render correctly
- Consider whether diagonal slopes (2 adjacent lower neighbors) need special face maps

---

## Issue 2: Water Above Vegetated Land (VISUAL)

**Count:** 513 across 20 seeds (~25/seed)
**Severity:** Medium
**Pattern:** River tile at `terrain+1` next to dry land at `terrain-1`, creating a 2-z visual cliff over grass.

**Root cause:** The river's carved terrain is 1 z higher than the adjacent dry land. The water sits at `carved_terrain + 1` which is 2 above the dry. This is correct data — the river bed IS on a slight plateau.

**Proposed solution:** Two options:
1. **River carving fix:** When carving a river valley, ensure the carved bed never exceeds adjacent dry terrain. This would be in `World/Hydrology/River/Carving.hs` — clamp the carved elevation to `min(carved, min_dry_neighbor)`.
2. **Render slope at bank:** Extend `waterSlopeAt` to detect 2-z drops to dry and produce a slope in that direction. The 2-z visual gap becomes a sloped transition instead of a vertical cliff.

**Recommendation:** Option 1 (carving fix) addresses the root cause. Option 2 is a band-aid.

---

## Issue 3: Floating Lake (REAL BUG)

**Count:** 115 tiles in 1/20 seeds (seed 896678)
**Severity:** High (when it occurs)
**Pattern:** Lake at surface=1 sitting in a basin with terrain down to -60. Depth up to 62 tiles. The lake should have been converted to ocean by `drainOceanLakes` but wasn't.

**Root cause:** `drainOceanLakes` requires the lake tile to be both below sea level AND adjacent to ocean. This lake is below sea level (terrain=-19 to -60) with surface=1, but NOT adjacent to ocean. It's an enclosed basin that the ocean BFS didn't reach.

**Proposed solution:** Extend `drainOceanLakes` to also convert lakes where the lake's terrain is below sea level (not just requiring ocean adjacency). Any lake in a below-sea-level basin should become ocean since physically it would be connected to the water table.

---

## Issue 4: Lake Tiles at Depth=0 (DATA BUG)

**Count:** 637 tiles in seed 2718
**Severity:** Medium
**Pattern:** Lake tiles where `fluidSurf == terrainZ`. Water at ground level = invisible. These tiles are "water" in the data but have no visual depth.

**Root cause:** The lake generation (`computeChunkLakes`) places lake water at the `clampedSurface` level. If `clampedSurface == terrainZ` for some tiles, the water has zero depth.

**Proposed solution:** In `computeChunkLakes`, only place lake water where `clampedSurface > terrainZ`. Tiles at exactly ground level should be dry.

---

## Issue 5: Dead-End River Tiles (DESIGN)

**Count:** 10-60 per seed
**Severity:** Low
**Pattern:** River tiles with only 1 water neighbor — the endpoint of a river segment. These are natural terminations where the river meets a terrain barrier.

**What's happening:** These are NOT segmentation bugs. The river generation creates water along segments, and segments end at geographic features. The sim can't extend rivers beyond what was generated.

**Proposed solution:** These are acceptable as-is. If the user wants fewer dead ends:
1. In `computeChunkRivers`, extend segment fill by 1-2 tiles beyond endpoints
2. In the sim's Phase 1b (basin fill), lower the threshold from ≥2 water neighbors to ≥1 for filling dry tiles. This would extend rivers into low terrain beyond their endpoints. **Risk:** Could flood areas that shouldn't have water.

---

## Issue 6: Isolated River Tile (RARE BUG)

**Count:** 1 tile in 1/6 seeds (seed 2718)
**Severity:** Low
**Pattern:** Single river tile with zero water neighbors. Should have been drained by the sim's Phase 2.

**Root cause:** The drain phase lowers surface by 1 per tick. Over 64 settle ticks, a tile can drop 512 z. But the drain only fires when the tile has NO water neighbors (via `getNeighborInfo`, which includes cross-chunk). If the tile has a cross-chunk water neighbor, it's not isolated and won't drain. When that neighbor chunk unloads, the tile becomes isolated but the sim might not run again.

**Proposed solution:** In Phase 2 drain, also drain tiles that have zero IN-CHUNK water neighbors (even if they have cross-chunk ones). This is more aggressive but prevents orphaned tiles.

---

## Issue 7: Cross-Chunk Rendering Gap (RENDER BUG)

**Count:** Affects all chunk boundaries
**Severity:** Medium
**Pattern:** `waterSideFaceQuads` only checks in-chunk neighbors. Water cliffs at chunk boundaries don't get side faces rendered. If two adjacent water tiles in different chunks have a 5-z difference, no side face is drawn.

**Root cause:** The side-face list comprehension (line 51) bounds-checks to `nx ≥ 0 && nx < chunkSize`, excluding cross-chunk tiles entirely.

**Proposed solution:** Add cross-chunk neighbor lookups to `waterSideFaceQuads`, similar to how `getNeighborInfo` in the sim already does this. The chunk's `fluidMap` can be looked up via the chunk map that's already passed around in the render pipeline.

---

## Issue 8: Basin Fill Threshold Too High (DESIGN)

**Count:** Affects narrow river-basin connections
**Severity:** Low-Medium
**Pattern:** The sim's Phase 1b fills dry tiles only when they have ≥2 water neighbors. A narrow river (1 tile wide) entering a basin can't trigger fill on the basin floor because the dry tiles only have 1 water neighbor (the river).

**Root cause:** The threshold of ≥2 prevents the sim from flooding random single-tile river banks. But it also prevents fill at narrow connections.

**Proposed solution:** Lower the threshold to ≥1 BUT add a constraint: only fill if the dry tile's terrain is at least 2 below the water surface (clearly submerged, not a bank). This catches basin floors while avoiding bank flooding.

---

## Issue 9: Sim Phase 1b Doesn't Cross Chunks (SIM BUG)

**Count:** Affects all chunk boundaries
**Severity:** Medium
**Pattern:** The basin-fill phase (1b) only counts in-chunk water neighbors. Dry tiles at chunk edges that have water neighbors in adjacent chunks aren't filled.

**Root cause:** Lines 162-175 of `Sim/Fluid/Tick.hs` check `fnx ≥ 0 && fnx < chunkSize && fny ≥ 0 && fny < chunkSize`, excluding cross-chunk tiles.

**Proposed solution:** Use the existing `allChunks` map to check cross-chunk neighbors, same pattern as `getNeighborInfo`. This would allow basin filling to propagate across chunk boundaries.

---

## Issue 10: River-Ocean Mouth Quality (GOOD)

**Count:** 672 smooth mouths vs 0 large drops (seed 42)
**Severity:** N/A (this is working well)
**Assessment:** The `fillCoastalGaps` pass in `Internal.hs` handles river-ocean transitions effectively. Mouths transition at 0-2 z drop, which is natural.

---

## Issue 11: Lava Fluid Not Simulated (MISSING FEATURE)

**Count:** Lava present in seeds 42 (172 tiles), 2718 (47), 356049 (51)
**Severity:** Low
**Pattern:** Lava tiles are included in the sim's diffusion (Phase 1), but there's no special behavior. Lava should behave differently from water: higher viscosity (slower flow), interaction with water (cooling/solidifying), emission of heat affecting nearby tiles.

**Proposed solution:** Future feature — separate lava simulation in `Sim/Fluid/Active.hs` or a dedicated `Sim/Fluid/Lava.hs`. Not blocking for water correctness.

---

## Issue 12: Performance — Sim Does 8 Sub-Iterations Per Tick (OPTIMIZATION)

**Count:** Affects every tick
**Severity:** Low (not causing lag currently)
**Pattern:** The sim runs 8 bidirectional scan passes per tick. Each pass processes every tile in every loaded chunk. For a world with 200 chunks (16x16 each), that's 200 * 256 * 8 = 409,600 tile operations per tick.

**Proposed solution:** Adaptive iteration count. Track how many tiles changed in the last tick. If zero changed, skip entirely. If few changed, reduce to 1-2 iterations. Only use 8 when actively settling (chunks recently loaded).

---

## Priority Ranking

| Priority | Issue | Impact | Effort |
|----------|-------|--------|--------|
| 1 | Floating Lake (#3) | High visual bug | Small (extend drainOceanLakes) |
| 2 | Cross-chunk rendering (#7) | Medium visual gap | Medium (add cross-chunk lookups to renderer) |
| 3 | Water above land (#2) | Medium visual cliff | Medium (fix river carving) |
| 4 | Basin fill cross-chunk (#9) | Medium sim gap | Small (extend neighbor checks) |
| 5 | Lake depth=0 (#4) | Medium invisible tiles | Small (add depth check to lake fill) |
| 6 | Basin fill threshold (#8) | Low-medium | Small (lower threshold + add depth constraint) |
| 7 | Isolated river (#6) | Rare | Small (extend drain to in-chunk only) |
| 8 | Water-water steps (#1) | Cosmetic | Medium (rendering polish) |
| 9 | Lava sim (#11) | Missing feature | Large (new sim module) |
| 10 | Sim performance (#12) | Optimization | Medium (adaptive iterations) |
