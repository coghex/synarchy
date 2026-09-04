#!/usr/bin/env python3
"""machine_shop's `probe-result/v1` migration contract (#2087)."""
from __future__ import annotations

from . import support

PROBE = "machine_shop"


def test_machine_shop_migration() -> None:
    support.batch_contract(
        PROBE, "machine_shop_probe.py", 9391,
        support.probe_checks("machine_shop_probe"))


TESTS = (test_machine_shop_migration,)
