"""Shared pass/fail thresholds for the river placement tooling.

Single source of truth so the single-seed regression test
(``test_river_pour.py``) and the multi-seed stress test
(``test_river_stress.py``) agree on what counts as a failure.

Each value is the MAXIMUM allowed count for that metric (inclusive):
a run passes when its observed count is ``<= threshold``.

Both tools expose ``--max-*`` CLI flags backed by these defaults, so an
individual run can still be tightened or loosened without editing code.
"""

# River surface above an adjacent rendered body — should never happen.
MAX_VISIBLE_DROPS = 0

# No-fluid tiles wedged between a river and a body (ocean/lake).
# The single-seed and multi-seed tools previously disagreed here
# (15 vs 20); 20 is the unified value — the looser of the two, so no
# previously-passing run newly fails.
MAX_DRY_GAPS = 20

# riverMask=true tiles that ended up with no fluid placed.
MAX_MASK_DRY = 30

# Consecutive river tiles running alongside ocean at high elevation.
MAX_COASTAL_PARALLEL = 5
