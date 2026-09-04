#!/usr/bin/env python3
"""lua_orphan_prune's `probe-result/v1` migration contract (#2087)."""
from __future__ import annotations

from . import support

PROBE = "lua_orphan_prune"


def test_lua_orphan_prune_migration() -> None:
    support.batch_contract(
        PROBE, "lua_orphan_prune_probe.py", 9008,
        ("snapshot_filters_orphan", "load_pauses_immediately",
         "load_reconcile_prunes_orphan", "nested_references_scrubbed",
         "per_entity_apply"),
        invoke=support.namespace_invoke(seed=42, size=64))


TESTS = (test_lua_orphan_prune_migration,)
