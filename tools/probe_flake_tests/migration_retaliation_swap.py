"""retaliation_swap's probe-result/v1 entry point and assertion contract."""
from __future__ import annotations

import argparse
from contextlib import ExitStack
from unittest.mock import patch

from . import support
import retaliation_swap_probe as probe

PROBE = 'retaliation_swap'


def test_retaliation_swap_launch():
    support.isolated_launch_contract(
        PROBE, fields=dict(seed=42, size=64, plates=3, self_test=False),
        logs=['retaliation_swap_engine.log'],
        default_log='/tmp/retaliation_swap_probe_engine.log')


def test_retaliation_swap_failure():
    def exercise(rep):
        ledger = probe.Ledger(rep=rep)
        ledger.record("export", True)
        ledger.record("fresh:staged-hit", False, "wrong hit")
        support.expect(probe.finish(ledger) == 2, "retaliation still distinguishes incomplete fixtures")
    outcomes = support.captured_outcomes(probe, exercise)
    support.expect(outcomes == {"export": "PASS", "fresh_staged_hit": "FAIL"},
                   "retaliation translates ledger keys without losing failed or unreached checks")


TESTS = (test_retaliation_swap_launch, test_retaliation_swap_failure)
