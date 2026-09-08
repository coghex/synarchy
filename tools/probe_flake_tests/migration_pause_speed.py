"""pause_speed's probe-result/v1 entry point and assertion contract."""
from __future__ import annotations

import argparse
from contextlib import ExitStack
from unittest.mock import patch

from . import support
import pause_speed_probe as probe

PROBE = 'pause_speed'


def test_pause_speed_launch():
    support.isolated_launch_contract(
        PROBE, fields=dict(seed=42, size=64, plates=3, self_test=False),
        logs=['pause_speed_engine.log'],
        default_log='/tmp/pause_speed_probe_engine.log')


def test_pause_speed_failure():
    def exercise(rep):
        checks = probe.Checks(rep)
        checks.ok(False, probe.DESCRIPTOR.label("notification_speed"), "wrong speed")
        checks.ok(True, probe.DESCRIPTOR.label("notification_frozen"))
        support.expect(checks.failed == 1, "pause-speed failed count remains independent of later passes")
    outcomes = support.captured_outcomes(probe, exercise)
    support.expect(outcomes == {"notification_speed": "FAIL", "notification_frozen": "PASS"},
                   "pause-speed assertions report their own outcomes")


TESTS = (test_pause_speed_launch, test_pause_speed_failure)


def test_pause_speed_missing_request():
    def exercise(rep):
        for cid, label in probe.PROBE_CHECKS[:4]:
            rep.check(cid, True, label)
        checks = probe.Checks(rep)
        with patch.object(probe, "send", return_value="true"), \
             patch.object(probe, "capture_request_id", return_value=None):
            try:
                probe.save_and_settle(8911, "fixture", checks, "manual save")
            except RuntimeError:
                support.expect(rep.protocol_mode, "only protocol mode stops at the incomplete event prefix")
    outcomes = support.captured_outcomes(probe, exercise)
    support.expect(outcomes["manual_save_request"] == "FAIL" and
                   "manual_save_completed" not in outcomes,
                   "missing save id leaves completion and later phases missing")


TESTS += (test_pause_speed_missing_request,)
