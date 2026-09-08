"""crop's probe-result/v1 entry point and assertion contract."""
from __future__ import annotations

import argparse
from contextlib import ExitStack
from unittest.mock import patch

from . import support
import crop_probe as probe

PROBE = 'crop'


def test_crop_launch():
    support.isolated_launch_contract(
        PROBE, fields=dict(seed=42, size=64, plates=3, self_test=False),
        logs=['crop_engine.log'],
        default_log='/tmp/crop_probe_engine.log')


def test_crop_failure():
    def exercise(rep):
        with patch.object(probe, "bootstrap", return_value={}), \
             patch.object(probe, "send", side_effect=support.StopBeforeEngine), \
             patch.object(probe, "quit_engine") as quit_owned:
            try:
                probe._exercise(8911, "owned-engine", argparse.Namespace(seed=42, size=64, plates=3), True, rep)
            except support.StopBeforeEngine:
                pass
            support.expect(quit_owned.call_count == 1, "crop failure still tears down the owned engine")
    outcomes = support.captured_outcomes(probe, exercise)
    support.expect(outcomes == {"content_loads": "FAIL"},
                   "crop content failure leaves growth checks unreached")


TESTS = (test_crop_launch, test_crop_failure)
