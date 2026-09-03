"""Severity classification policy for the world audit (#2224).

The single owner of which categories are bugs, which are quality
metrics, what each quality metric's threshold is, and the two functions
that read that classification. Calibration comments live with the values
they calibrate.

Imports no check owner and no command façade: the classification is a
statement about categories, not about which check emitted one, so
nothing here needs to see a check. `tools/world_audit.py` re-exports
this surface and consumers import it from there.
"""
from __future__ import annotations


# ----- Severity classification --------------------------------------------
#
# Every category the audit emits — the `category` argument of every
# `Issue(...)` construction, NOT the ALL_CHECKS keys, which are check-function
# labels and disagree with the categories in both directions — belongs to one
# of two buckets:
#
#   BUG     — any occurrence is a real bug. Must be 0 in a healthy world.
#             world_check.py enforces this with a hard envelope of 0.
#
#   QUALITY — exists on a spectrum. Some occurrence is expected and
#             realistic (small islands, underground aquifers, rivers
#             drying up before the coast). Tracked as a quality score
#             against a threshold; failure means the metric drifted far
#             enough to indicate broken worldgen, not zero tolerance.
#
# The classification is CLOSED for the world_check.py gate: a category in
# neither set, or a QUALITY category with no QUALITY_THRESHOLDS entry, fails
# that seed's check by name rather than being tolerated under an implicit
# default (see classify_category below and world_check.py::check_issue_summary).
#
# See `feedback_testing_philosophy` in memory for the rationale.

# Bug categories — any occurrence is unambiguous corruption.
BUG_CATEGORIES = {
    "OCEAN_ON_LAND",        # Ocean fluid type leaked onto high terrain
    "TERRAIN_SPIKE",        # Despike pass should have removed
    "TERRAIN_PIT",          # Same
    "MINBOUND_LEAK",        # Int64 sentinel outside beyondGlacier zone
    "SURFACE_INCONSISTENT", # surfaceZ doesn't match the documented rule
    "WETLAND_ON_SLOPE",     # wetland post-pass gate violated (slope half)
}

# Quality categories — tracked as scores against thresholds, not bugs.
# A non-zero count can be legitimate; failure happens when the count
# drifts above the threshold for that metric.
QUALITY_CATEGORIES = {
    "DRY_BELOW_SEA",         # ocean-connected dry tile (after BFS filter)
    "RIVER_UNDER_TERRAIN",   # underground river/aquifer
    "LAKE_UNDER_TERRAIN",    # underground/cave lake
    "DEEP_LAVA_COLUMN",      # deep lava column — depth only, not geometry
    "LAVA_RIM_BREACH",       # lava unsupported by a lower dry/lava rim tile
    "LAVA_RIM_INCOMPLETE",   # rim judgement incomplete at the region edge
    "FLOATING_RIVER",
    "FLOATING_LAKE",
    "FLOATING_FLUID",
    "RIVER_CHUNK_GAP",       # cross-chunk seam mismatch or natural dry-up
    "RIVER_MOUTH_DROP",      # waterfall at coast — physical for steep rivers
    "ISLAND_1TILE",          # tiny isolated island — can be real
    "LAKE_HOLE",             # dry tile mid-lake — can be a tiny lake island
    "SUBMERGED_BUMP",        # terrain protrusion through water plane
    "ISOLATED_FLUID",        # singleton fluid tile — small puddle / artifact
    "WATER_ABOVE_LAND",      # river in valley with high banks
    "WATER_CLIFF",           # water against terrain cliff
    "WATER_WATER_CLIFF",     # downstream gradient stair-step
    "MID_RIVER_CLIFF",       # river surface step larger than terrain step
    "FLOATING_WATER",        # water-vs-dry side gap, often legitimate cliff
    "MULTI_ISLAND",          # small dry cluster in a water body
    "FLAT_ISOLATED_WATER",   # small puddle on flat terrain
    "DESERT_SOIL_ON_SLOPE",  # sand/salt-flat off the plateau — ungated, rare
}

# Quality thresholds — per-seed max occurrence count, calibrated against
# observed values across the 21-seed baseline set. A category whose count
# exceeds its threshold is flagged in world_check.py as a quality
# regression; under-threshold counts are tracked but don't fail.
#
# Calibration policy: set to ~1.5× the worst current value across the
# baseline set, so legitimate variance from new seeds doesn't trigger
# false fails, but a doubling of any metric does. Tighten downward as
# generation improves and the band of expected values narrows.
QUALITY_THRESHOLDS = {
    # Low-variance categories — should stay near zero.
    # Recalibrated 2026-06-07 for the volcanism default 1.0 → 1.25
    # (user-approved): rougher volcanic flanks pin more 1-tile
    # water-table puddles, so the puddle-flavored metrics shifted.
    # Differential vs the old constants confirmed counts move both
    # directions per seed (no new artifact class) and two seeds
    # already exceeded the old thresholds before the change.
    "DRY_BELOW_SEA":        200,  # observed max 155 coastal z=0 tiles
                                  # (seed 137, the known-bad seed —
                                  # 5803 before the wt rework)
    "DESERT_SOIL_ON_SLOPE": 250,  # observed max 150 (seed 123 w128); 1.5× policy
    "FLAT_ISOLATED_WATER":   90,  # observed max 59 (seed 5050)
    "FLOATING_WATER":       500,  # observed max 319 (seed 12321) after
                                  # coastline variety (#220, save v69):
                                  # cliff coasts + coastal mountains kept
                                  # by the steepness field make legitimate
                                  # water-adjacent cliffs common by
                                  # design. Gap histogram is ≥95% 1-8z
                                  # bank steps; the deep outliers are
                                  # high-altitude tarns/rivers against
                                  # valley headwalls (surf 97-183), not
                                  # coastal artifacts. 1.5× obs-max per
                                  # the calibration policy. Previously
                                  # 300 (obs max 176, recalibrated
                                  # 2026-06-11; before that 150).
    "ISOLATED_FLUID":        90,  # observed max 74 (seed 2718; was 77
                                  # even with old constants)
    "LAKE_HOLE":             25,  # observed max 0 after the terrainZ<surf
                                  # refinement (#21): flush waterline
                                  # islets no longer counted, only genuine
                                  # depressions the lake failed to fill.
                                  # Threshold kept for headroom against a
                                  # real unfilled-hole regression.
    "MULTI_ISLAND":          25,  # observed max 4
    "RIVER_CHUNK_GAP":       50,  # observed max 14
    "RIVER_MOUTH_DROP":      50,  # observed max 15
    "SUBMERGED_BUMP":        25,  # observed max 4

    # The three lava metrics were measured across all 21 baselines on
    # 2026-08-30, after #1876 split rim containment out of the old
    # depth-only FLOATING_LAVA. The superseded 450/"observed max 301
    # (seed 1337)" rationale predated the current generator: seed 1337
    # carries no deep lava column at all today.
    "DEEP_LAVA_COLUMN":      20,  # observed max 11 (seed 12321; seed 250
                                  # is the only other non-zero, at 2).
                                  # Column DEPTH only — a contained pool
                                  # is supposed to be deep, exactly like
                                  # FLOATING_LAKE. 1.5× obs-max is 16.5,
                                  # rounded up per the policy above.
    "LAVA_RIM_BREACH":        0,  # observed max 0 — NO baseline seed
                                  # produces one, because Chunk.hs's rim
                                  # caps + lava shell seal every pool's
                                  # rim flush with its surface before the
                                  # dump exists (see the check). The zero
                                  # therefore measures that SEALING pass,
                                  # not World.Magma.Pool.grow's own
                                  # clamp. 1.5× obs-max is 0, so the cap
                                  # is 0 and any breach fails. It stays
                                  # QUALITY rather than BUG because the
                                  # seal is a generation TUNING pass, not
                                  # corruption — raising this is the
                                  # evidence-backed response to a
                                  # deliberate generator change, which
                                  # BUG would forbid.
    "LAVA_RIM_INCOMPLETE":   20,  # observed max 13 (seed 5050); 1.5× is
                                  # 19.5, rounded up. Counts lava tiles
                                  # whose containment could not be judged
                                  # because a cardinal neighbour lay
                                  # outside the dumped region, so it
                                  # scales with how much lava meets the
                                  # REQUESTED WINDOW's edge, not with
                                  # world health. Calibrated for the
                                  # baseline windows; a sweep over a
                                  # different region legitimately
                                  # measures a different value.
    # High-variance / by-design categories.
    "FLOATING_LAKE":       7000,  # observed max 5697 (seed 99) after the
                                  # continental-margin seabed (save v26)
                                  # deepens sea-surrounded clamped basins
                                  # into the slope — deep sea-connected
                                  # basins are SUPPOSED to be deep, so the
                                  # "floating lake" (deep water column)
                                  # count rises. High-variance metric,
                                  # recalibrated 2026-06-08.
    "FLOATING_RIVER":       300,  # not observed in baselines
    "FLOATING_FLUID":       300,  # generic fallback
    "ISLAND_1TILE":         100,  # not observed; small islands possible
    "LAKE_UNDER_TERRAIN":   500,  # not observed; underground lakes possible
    "MID_RIVER_CLIFF":     1500,  # observed max 1046 (downstream gradient)
    "RIVER_UNDER_TERRAIN":  500,  # observed max 251 (underground rivers OK)
    "WATER_ABOVE_LAND":     500,  # observed max 323 after #220 coastline
                                  # variety (was 152, steep valleys) —
                                  # same rationale + histogram as
                                  # FLOATING_WATER above.
    "WATER_CLIFF":          500,  # observed max 323 after #220 — see
                                  # FLOATING_WATER rationale.
    "WATER_WATER_CLIFF":   3500,  # observed max 2704 (seed 13579) after the
                                  # #21 refinement: river-involved 1-z steps
                                  # (natural downhill flow) excluded, lake-to-
                                  # lake 1-z steps still flagged. Threshold
                                  # kept; headroom retained against new seeds.
}


def severity_of(category: str) -> str:
    """Return 'BUG' or 'QUALITY' for an issue category.

    This is the COARSE bucketing used for reporting: an unknown category
    falls back to 'QUALITY' so a caller that only partitions counts into
    two piles keeps working. world_stress.py relies on that total function.

    It is deliberately NOT the gate's classifier. Anything that must fail
    closed on an unclassified category — world_check.py — uses
    classify_category() instead, which reports the absence rather than
    hiding it behind this fallback.
    """
    if category in BUG_CATEGORIES:
        return "BUG"
    return "QUALITY"


def classify_category(category: str) -> str | None:
    """Return 'BUG', 'QUALITY', or None when the category is unclassified.

    Unlike severity_of(), this never guesses: a category declared in
    neither BUG_CATEGORIES nor QUALITY_CATEGORIES yields None, so callers
    that gate on the audit can reject it by name instead of silently
    treating a brand-new corruption class as a tolerated quality metric.
    """
    if category in BUG_CATEGORIES:
        return "BUG"
    if category in QUALITY_CATEGORIES:
        return "QUALITY"
    return None
