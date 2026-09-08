"""item_temp's probe-result/v1 entry point and assertion contract."""
from __future__ import annotations

import argparse
from contextlib import ExitStack
from unittest.mock import patch

from . import support
import item_temp_probe as probe

PROBE = 'item_temp'


def test_item_temp_launch():
    support.isolated_launch_contract(
        PROBE, fields=dict(seed=42, size=64, plates=3, self_test=False),
        logs=['item_temp_engine.log'],
        default_log='/tmp/item_temp_probe_engine.log')


def test_item_temp_failure():
    def exercise(rep):
        # Run the real control flow through an unusable ambient. Its
        # teardown must still emit cleanup and remove the private root.
        with patch.object(probe, "boot", return_value="owned-engine"), \
             patch.object(probe, "bootstrap"), patch.object(probe, "send"), \
             patch.object(probe, "num", side_effect=[None, 1.0, 2.0]), \
             patch.object(probe, "quit_engine") as quit_owned:
            rc = probe._run(argparse.Namespace(port=8911, seed=42, size=64, plates=3), rep)
            support.expect(rc == 1 and quit_owned.call_count == 1,
                           "item temperature setup failure exits nonzero and tears down")
    outcomes = support.captured_outcomes(probe, exercise)
    support.expect(outcomes == {"ambient_default": "FAIL"},
                   "temperature abort leaves later checks missing despite unconditional cleanup")


TESTS = (test_item_temp_launch, test_item_temp_failure)
