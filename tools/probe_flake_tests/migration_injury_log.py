#!/usr/bin/env python3
"""injury_log's `probe-result/v1` migration contract (#2087)."""
from __future__ import annotations

from . import support

PROBE = "injury_log"


def test_injury_log_migration() -> None:
    support.batch_contract(
        PROBE, "injury_log_probe.py", 9140,
        ("emit_roundtrip", "drain_destructive", "injure_event",
         "event_log_uid", "fall_lane_damaging", "fall_event"),
        invoke=support.namespace_invoke(no_fall=False))


TESTS = (test_injury_log_migration,)
