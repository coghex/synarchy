"""save_pause's probe-result/v1 entry point and assertion contract."""
from __future__ import annotations

import argparse
from contextlib import ExitStack
from unittest.mock import patch

from . import support
import save_pause_probe as probe

PROBE = 'save_pause'


def test_save_pause_launch():
    support.isolated_launch_contract(
        PROBE, fields=dict(seed=42, size=64, plates=3, self_test=False),
        logs=['save_pause_engine.log'],
        default_log='/tmp/save_pause_probe_engine.log')


def test_save_pause_failure():
    from unittest.mock import Mock
    def exercise(rep):
        def send(_port, lua, **_kwargs):
            if "engine.saveWorld" in lua:
                raise support.StopBeforeEngine()
            return "1"
        with patch.object(probe, "boot", return_value=Mock()), \
             patch.object(probe, "send", side_effect=send), \
             patch.object(probe, "wait_for_init", return_value=True), \
             patch.object(probe, "wait_time_scale", return_value=(False, 1.0)), \
             patch.object(probe, "quit_engine"):
            try:
                probe._run(argparse.Namespace(port=8911, seed=42), rep)
            except support.StopBeforeEngine:
                pass
    outcomes = support.captured_outcomes(probe, exercise)
    support.expect(outcomes == {"fast_forward": "FAIL"},
                   "save-pause reports a failed nondefault-speed precondition without later passes")


TESTS = (test_save_pause_launch, test_save_pause_failure)
