#!/usr/bin/env python3
"""thought's `probe-result/v1` migration contract (#2087)."""
from __future__ import annotations

from . import support

PROBE = "thought"


def test_thought_migration() -> None:
    support.batch_contract(
        PROBE, "thought_probe.py", 9351,
        ("emit_roundtrip", "drain_destructive", "catalogue_loaded",
         "state_thought_fired", "state_thought_moves_mood",
         "cold_thought_fired", "world_patches_restored",
         "mood_biases_valence", "thought_log_surfaces_text"))


TESTS = (test_thought_migration,)
