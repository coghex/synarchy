"""save_barrier's probe-result/v1 entry point and assertion contract."""
from __future__ import annotations

import argparse
from contextlib import ExitStack
from unittest.mock import patch

from . import support
import save_barrier_probe as probe

PROBE = 'save_barrier'


def test_save_barrier_launch():
    support.isolated_launch_contract(
        PROBE, fields=dict(seed=42, size=64, plates=3, self_test=False),
        logs=['save_barrier_probe.log', 'save_barrier_probe_reload.log', 'save_barrier_probe_reload2.log'],
        default_log='/tmp/save_barrier_probe.log')


def test_save_barrier_failure():
    import tempfile
    from unittest.mock import Mock
    def exercise(rep):
        def send(_port, lua, **_kwargs):
            if "getAreaFluid" in lua:
                return "[]"
            return "false"
        with tempfile.TemporaryDirectory() as root, \
             patch.object(probe, "boot", return_value=Mock()), \
             patch.object(probe, "send", side_effect=send), \
             patch.object(probe, "wait", return_value=((1, 1), {"type": "lake"})), \
             patch.object(probe, "quit_engine"):
            try:
                probe._exercise(argparse.Namespace(port=8911, seed=42), root, rep)
            except RuntimeError as exc:
                support.expect(str(exc) == "save rejected", "save-barrier preserves the rejection cause")
    outcomes = support.captured_outcomes(probe, exercise)
    support.expect(outcomes == {"save_accepted": "FAIL"},
                   "save-barrier attributes rejection and leaves capture and reload checks unreached")


TESTS = (test_save_barrier_launch, test_save_barrier_failure)
