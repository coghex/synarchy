"""Blood-trail scenario failures keep their ordered prefix and teardown."""
from __future__ import annotations

import argparse
import contextlib
import io
import tempfile
from pathlib import Path
from unittest.mock import patch

from . import support
import bleeding_trail_probe as probe

PROBE = "bleeding_trail"


def test_bleeding_trail_launch():
    support.isolated_launch_contract(PROBE, fields={},
        logs=["bleeding_trail_engine.log"], default_log=probe.LOG)


def test_bleeding_trail_scaled_route_failure():
    with tempfile.TemporaryDirectory() as tmp:
        for harnessed in (False, True):
            events = Path(tmp) / "events"
            output = io.StringIO()
            rep = support.probe_protocol.Reporter(probe.DESCRIPTOR,
                events_path=str(events) if harnessed else None, stream=output)
            with contextlib.ExitStack() as stack:
                for name in ("bootstrap_defs", "init_arena", "reset_blood", "injure", "move_to", "destroy", "set_time_scale"):
                    stack.enter_context(patch.object(probe, name))
                stack.enter_context(patch.object(probe, "boot", return_value=object()))
                stack.enter_context(patch.object(probe, "spawn_fresh", return_value=1))
                stack.enter_context(patch.object(probe, "impact_decal_ids", return_value=set()))
                stack.enter_context(patch.object(probe, "wait_arrival", return_value=True))
                stack.enter_context(patch.object(probe, "route_marks", side_effect=[[dict(x=10), dict(x=12)], []]))
                quit_owned = stack.enter_context(patch.object(probe, "quit_engine"))
                with contextlib.redirect_stdout(output):
                    rc = probe._run(argparse.Namespace(port=8911), rep)
                rep.close()
                support.expect(rc == 1 and quit_owned.call_count == 1,
                               "blood-trail later assertion failure tears down its engine")
            if harnessed:
                _, outcomes = support.probe_protocol.parse_event_stream(events.read_text(), probe.DESCRIPTOR)
                support.expect(outcomes["moving_trail"] == "PASS" and outcomes["scaled_trail"] == "FAIL"
                               and all(outcomes[key] == "MISSING" for key in probe.DESCRIPTOR.ids[2:]),
                               "scaled route fails independently and later scenarios stay missing")
                support.expect("PASS:" not in output.getvalue() and "FAIL:" not in output.getvalue(),
                               "blood-trail protocol has no legacy assertion output")
            else:
                support.expect("PASS: 2 marks" in output.getvalue() and "FAIL: mark count 0" in output.getvalue(),
                               "blood-trail standalone retains its original bare PASS/FAIL text")


TESTS = (test_bleeding_trail_launch, test_bleeding_trail_scaled_route_failure)
