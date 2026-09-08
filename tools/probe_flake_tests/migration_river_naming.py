"""river_naming's probe-result/v1 entry point and assertion contract."""
from __future__ import annotations

import argparse
from contextlib import ExitStack
from unittest.mock import patch

from . import support
import river_naming_probe as probe

PROBE = 'river_naming'


def test_river_naming_launch():
    support.isolated_launch_contract(
        PROBE, fields=dict(seed=42, size=64, plates=3, self_test=False),
        logs=['river_generate.log', 'river_load.log', 'river_regenerate.log'],
        default_log='/tmp/river_naming_engine.log')


def test_river_naming_failure():
    named = [dict(id="duplicate", name="First", gloss="Large Water"),
             dict(id="duplicate", name="Second", gloss="Small Water")]
    plain = [dict(id="duplicate"), dict(id="duplicate")]
    def exercise(rep):
        with ExitStack() as stack:
            for name, value in {"boot": "owned-engine", "gen_world": None,
                                "send": None, "capture_request_id": 1,
                                "wait_save_complete": (True, {}), "quit_engine": None}.items():
                stack.enter_context(patch.object(probe, name, return_value=value))
            stack.enter_context(patch.object(probe, "rivers", side_effect=[named, named, plain]))
            support.expect(probe._run(argparse.Namespace(port=8911, seed=42, size=16), rep) == 1,
                           "duplicate river ids still stop before loading")
    outcomes = support.captured_outcomes(probe, exercise)
    support.expect(outcomes["named_identity"] == outcomes["unnamed_identity"] == "FAIL" and
                   outcomes["river_names"] == "PASS" and "restored_rivers" not in outcomes,
                   "river identity failures do not contaminate naming or invent a restore result")


TESTS = (test_river_naming_launch, test_river_naming_failure)
