"""config_migration's probe-result/v1 entry point and assertion contract."""
from __future__ import annotations

import argparse
from contextlib import ExitStack
from unittest.mock import patch

from . import support
import config_migration_probe as probe

PROBE = 'config_migration'


def test_config_migration_launch():
    support.isolated_launch_contract(
        PROBE, fields=dict(seed=42, size=64, plates=3, self_test=False),
        logs=['config_committed.log', 'config_revised.log', 'config_equivalent.log', 'config_migration.log', 'config_idempotent.log', 'config_local_precedence.log', 'config_malformed.log', 'config_malformed_local.log'],
        default_log='/tmp/config_migration_probe_engine.log', patch=neutralize_config)


def neutralize_config(module):
    stack = ExitStack()
    for name in ("backup_local_only", "restore_local_only"):
        stack.enter_context(patch.object(module, name, lambda *a: {}))
    stack.enter_context(patch.object(module, "git_status", lambda *a: ""))
    return stack.close


def test_config_migration_failure():
    def exercise(rep):
        support.expect(not probe.check(probe.DESCRIPTOR.label("legacy_video_present"),
                                       False, "missing fixture", rep=rep),
                       "config migration retains the failing boolean")
    outcomes = support.captured_outcomes(probe, exercise)
    support.expect(outcomes["legacy_video_present"] == "FAIL" and
                   "default_video_scale" not in outcomes,
                   "config failure has a stable id and later checks stay absent")


TESTS = (test_config_migration_launch, test_config_migration_failure)
