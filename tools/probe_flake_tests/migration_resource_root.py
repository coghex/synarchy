"""Resource-root's direct-binary launches and assertion reporting."""
from __future__ import annotations

import argparse
import io
import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch

from . import support
import resource_root_probe as probe

PROBE = "resource_root"


def test_resource_root_descriptor():
    support.migration_descriptor("resource_root_probe.py", PROBE, (
        "missing_root", "nonexistent_root", "missing_operand", "empty_operand",
        "empty_overrides_env", "dump_from_root", "env_ready", "console_answers",
        "clean_shutdown"))

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events"
        env = dict(os.environ, SYNARCHY_PROBE_ENGINE_EXE=str(Path(tmp) / "absent"),
                   SYNARCHY_PROBE_EVENTS=str(events))
        result = subprocess.run([sys.executable, probe.__file__, "--describe"],
                                cwd=tmp, env=env, capture_output=True, text=True, timeout=30)
        support.expect(result.returncode == 0 and not events.exists(),
                       "resource-root describe needs no valid executable, cwd resources or event stream")


def test_resource_root_failure_and_launches():
    def exercise(rep):
        runs = []
        def run(argv, **kwargs):
            runs.append(argv)
            index = len(runs)
            # The first rejection deliberately has the wrong status. All
            # later fixtures meet their own, unchanged launch assertions.
            stderr = {
                1: f"invalid resource root: current directory {kwargs['cwd']}/scripts; --resource-root",
                2: f"invalid resource root: --resource-root {argv[-1]}",
                3: "--resource-root requires a path",
                4: "--resource-root empty",
                5: "--resource-root empty",
            }.get(index, "")
            stdout = json.dumps([dict(x=0, y=0, terrainZ=1)]) if index == 6 else ""
            return SimpleNamespace(returncode=0 if index in (1, 6) else 1,
                                   stdout=stdout, stderr=stderr)

        class Process:
            returncode = None
            def __init__(self, argv, **kwargs):
                runs.append(argv)
                kwargs["stdout"].write("READY\n")
                kwargs["stdout"].flush()
            def poll(self):
                return self.returncode
            def kill(self):
                self.returncode = -9
            def wait(self, **_kwargs):
                return self.returncode

        def quit_owned(_port, process):
            process.returncode = 0

        with tempfile.TemporaryDirectory() as tmp, \
             patch.object(probe, "locate_binary", return_value="/fixture/engine"), \
             patch.object(probe.tempfile, "mkdtemp", return_value=tmp), \
             patch.object(probe.subprocess, "run", side_effect=run), \
             patch.object(probe.subprocess, "Popen", Process), \
             patch.object(probe, "send", return_value="2"), \
             patch.object(probe, "quit_engine", quit_owned):
            # Retain every launch in this invocation's directory so even
            # standalone test runs do not clobber the operator's logs.
            rep.engine_log_dir = tmp
            rep.rts_caps = 3
            rc = probe._run(argparse.Namespace(port=8911), rep)
            support.expect(rc == 1, "resource-root bad rejection status still fails")
            support.expect(runs[2][-1] == "--resource-root",
                           "RTS arguments do not supply the deliberately missing flag operand")
            support.expect(all(argv[-3:] == ["+RTS", "-N3", "-RTS"] for argv in runs[-2:]),
                           "both successful binary launches receive harness RTS caps")
            support.expect(len(list(Path(tmp).glob("resource_root_*.log"))) == 7,
                           "all six direct runs and the headless boot retain distinct logs")
    outcomes = support.captured_outcomes(probe, exercise)
    support.expect(outcomes["missing_root"] == "FAIL" and
                   sum(v == "PASS" for v in outcomes.values()) == 8,
                   "resource-root reports one wrong rejection independently of later successful launches")


TESTS = (test_resource_root_descriptor, test_resource_root_failure_and_launches)


def test_resource_root_timeout_artifacts():
    with tempfile.TemporaryDirectory() as tmp:
        rep = support.probe_protocol.Reporter(probe.DESCRIPTOR, engine_log_dir=tmp)
        try:
            error = subprocess.TimeoutExpired(["/fixture/engine"], 1,
                                              output=b"partial stdout", stderr=b"partial stderr")
            with patch.object(probe.subprocess, "run", side_effect=error):
                try:
                    probe.run_captured(rep, "timeout.log", ["/fixture/engine"])
                except subprocess.TimeoutExpired:
                    pass
            support.expect((Path(tmp) / "timeout.log").read_text() == "partial stdout\npartial stderr",
                           "direct-binary timeout retains both partial output pipes")
        finally:
            rep.close()


TESTS += (test_resource_root_timeout_artifacts,)
