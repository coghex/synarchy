"""The registered corner-trap invocation and legacy metadata modes."""
from __future__ import annotations

import argparse
from unittest.mock import patch

from . import support
import movement_probe as probe

PROBE = "movement"
FIELDS = dict(mode="move", course="corner_trap", unit="acolyte", speed=None, seconds=0.5)


def test_movement_launch():
    support.isolated_launch_contract(PROBE, fields=FIELDS,
        logs=["movement_engine.log"], default_log=probe.LOG)


def test_movement_failed_route():
    def exercise(rep):
        point = dict(x=0, y=0, z=0, act="walking", pose="standing", anim="walk")
        with patch.object(probe, "boot", return_value=object()), \
             patch.object(probe, "bootstrap"), \
             patch.object(probe, "check_course_inventory", return_value=0), \
             patch.object(probe, "send_json", return_value=dict(sx=0, sy=0, gx=5, gy=5, name="corner_trap")), \
             patch.object(probe, "wait_world_ready", return_value=True), \
             patch.object(probe, "send", return_value="1"), \
             patch.object(probe, "speed_of", return_value=1), \
             patch.object(probe, "sample", return_value=point), \
             patch.object(probe.time, "sleep"), \
             patch.object(probe, "quit_engine") as quit_owned:
            rc = probe._run(argparse.Namespace(port=8911, **FIELDS), rep)
            support.expect(rc == 1 and quit_owned.call_count == 1,
                           "movement preserves failed-route exit and owned-engine cleanup")
    outcomes = support.captured_outcomes(probe, exercise)
    support.expect(outcomes == dict(course_inventory="PASS", reached_goal="FAIL", not_frozen="FAIL"),
                   "movement reports the actual failed corner-trap assertions in order")


def test_movement_unsupported_protocol_mode():
    def exercise(rep):
        with patch.object(probe.sys, "argv", ["movement_probe.py", "--mode", "pacing"]), \
             patch.object(probe.probe_protocol, "reporter_from_env", return_value=rep), \
             patch.object(probe, "boot", side_effect=AssertionError("must not boot")):
            if rep.protocol_mode:
                support.expect(probe.main() == 2, "unsupported protocol variant refuses before boot")
            else:
                rep.abort("standalone variants remain covered by their existing tests")
    outcomes = support.captured_outcomes(probe, exercise)
    support.expect(outcomes == {}, "unsupported mode emits no misleading corner-trap checks")


TESTS = (test_movement_launch, test_movement_failed_route, test_movement_unsupported_protocol_mode)
