#!/usr/bin/env python3
"""blood_decal's `probe-result/v1` migration contract (#2087).

The registry, texture-eviction and render-quad checks it declares are
its own; the contract they are driven through is `support.batch_contract`,
shared with every other batch-migrated probe.
"""
from __future__ import annotations

from . import support

PROBE = "blood_decal"


def test_blood_decal_migration() -> None:
    support.batch_contract(
        PROBE, "blood_decal_probe.py", 9011,
        ("near_requests_reuse", "distinct_requests_mint", "fifo_order_reported",
         "oldest_texture_evicted", "eviction_removes_decals",
         "pixel_data_bounded", "render_quads_live_only", "dry_tint_ages",
         "clear_empties_registry"))


TESTS = (test_blood_decal_migration,)
