#!/usr/bin/env python3
"""mental_efficiency's `probe-result/v1` migration contract (#2087)."""
from __future__ import annotations

from . import support

PROBE = "mental_efficiency"


def test_mental_efficiency_migration() -> None:
    support.batch_contract(
        PROBE, "mental_efficiency_probe.py", 9353,
        support.probe_checks("mental_efficiency_probe"))


TESTS = (test_mental_efficiency_migration,)
