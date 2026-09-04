#!/usr/bin/env python3
"""wire's `probe-result/v1` migration contract (#2087)."""
from __future__ import annotations

from . import support

PROBE = "wire"


def test_wire_migration() -> None:
    support.batch_contract(
        PROBE, "wire_probe.py", 9359,
        support.probe_checks("wire_probe"),
        invoke=support.namespace_invoke(phase="all"))


TESTS = (test_wire_migration,)
