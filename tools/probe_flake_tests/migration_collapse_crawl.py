#!/usr/bin/env python3
"""collapse_crawl's `probe-result/v1` migration contract (#2087)."""
from __future__ import annotations

from . import support

PROBE = "collapse_crawl"


def test_collapse_crawl_migration() -> None:
    support.batch_contract(
        PROBE, "collapse_crawl_probe.py", 9304,
        ("hold_exercised", "no_premature_crawl", "rise_gate_releases"))


TESTS = (test_collapse_crawl_migration,)
