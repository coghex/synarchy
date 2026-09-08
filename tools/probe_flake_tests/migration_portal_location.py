"""portal_location's probe-result/v1 entry point and assertion contract."""
from __future__ import annotations

import argparse
from contextlib import ExitStack
from unittest.mock import patch

from . import support
import portal_location_probe as probe

PROBE = 'portal_location'


def test_portal_location_launch():
    support.isolated_launch_contract(
        PROBE, fields=dict(seed=42, size=64, plates=3, self_test=False),
        logs=['portal_location_engine.log'],
        default_log='/tmp/portal_location_engine.log')


def test_portal_location_failure():
    ruin = dict(id="ruin_small", gx=0, gy=0, cx=0, cy=0,
                bounds=dict(max_x=2, max_y=2))
    def exercise(rep):
        with ExitStack() as stack:
            for name, value in {"boot": "owned-engine", "load_defs": None,
                                "gen_world": None, "placed_ready": [ruin],
                                "send": None, "wait_floor": True,
                                "ruin_geometry": {}, "spawn_counts": {},
                                "try_spawn": "unexpected-building", "quit_engine": None}.items():
                stack.enter_context(patch.object(probe, name, return_value=value))
            stack.enter_context(patch.object(probe, "can_place_at", side_effect=[
                (False, "inside a location's bounds"), (True, None), (True, None)]))
            rc = probe._run(argparse.Namespace(port=8911, seed=42, size=64), rep)
            support.expect(rc == 1, "portal spawn bypass still fails the probe")
    outcomes = support.captured_outcomes(probe, exercise)
    support.expect(outcomes["spawn_rejected"] == "FAIL" and
                   sum(value == "PASS" for value in outcomes.values()) == 7,
                   "portal reports the bypass failure and seven independent passes")


TESTS = (test_portal_location_launch, test_portal_location_failure)
