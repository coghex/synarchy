#!/usr/bin/env python3
"""Shared support for `test_deflake.py`'s three case owners (#2093).

This is the ONE source of the synthetic infrastructure the owner
modules -- `deflake_selftest_orchestration`, `deflake_selftest_handoff`
and `deflake_selftest_preparation` -- share: the assertion helper and
the single failure accumulator behind it, the temporary census, claim
and artifact tree, the real `probe_flake.Measurement` builder, the fake
claim with its ownership and renewal surface, the recording, resource
and engine-preparation adapters, and `run`, which is
`deflake.measure_next_probe` with every seam defaulted to a safe fake.

Two of those are single-sourced for correctness rather than tidiness:

* `FAILURES` is the ONE list `expect` appends to. Three owners each
  holding a private accumulator would let the aggregate exit 0 while a
  sibling owner had recorded a failure. Since #1922 that one list is
  `tools/selftestlib.py`'s, re-exported here so the owners import it --
  and the quiet-by-default `expect` behind it -- from the single place
  they already import everything else shared from.
* `run` is where every case's engine-free guarantee lives: the real
  preparation seam shells out to Cabal and the real measurement seam
  boots an engine, so a suite that is toolchain-free must never reach
  either by forgetting to substitute. Every case goes through this one
  function, and only the cases that deliberately exercise the real
  adapters name them.
* `saved_runner_executable` is the ONE save/restore of
  `probe_runner_resources.ENGINE_EXECUTABLE`, the only writable
  production module global this gate touches. `deflake` installs the
  prepared executable there under every `run` and, being the tool that
  is about to hand it to child probes, never uninstalls it -- so `run`
  restores it on every exit, and the preparation cases, which assign it
  themselves before calling `run`, use the same helper around their own
  assignment.

`seams_restored` is the check that all of that restoration actually
happened: every entry point runs it around whatever cases it selected,
so a focused owner cannot leak a module global, `PATH`, or a patched
function into whatever runs next in the same interpreter.

A helper with exactly ONE owner lives with that owner rather than here:
`Raiser` and `written_handoff` are the handoff's, and the `PATH`
save/restore is the real-preparation case's alone.

Nothing here runs a case and this module is not a gate of its own:
`python3 tools/test_deflake.py` remains the only invocation, in CI and
in `make ci` alike.
"""
from __future__ import annotations

import argparse
import datetime
import json
import os
import shutil
import tempfile
import uuid
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parent))
import deflake  # type: ignore  # noqa: E402
import probe_census  # type: ignore  # noqa: E402
# The recorder seams these cases inject at -- `tempfile.mkstemp` and
# `os.fsync` -- are reached through the STORAGE owner, which is the
# module that stages and fsyncs a census write since #2131.
import probe_census_storage as census_storage  # type: ignore  # noqa: E402
import probe_claim  # type: ignore  # noqa: E402
import probe_flake  # type: ignore  # noqa: E402
import probe_protocol  # type: ignore  # noqa: E402
import probe_resource_lock  # type: ignore  # noqa: E402
import probe_runner_resources  # type: ignore  # noqa: E402

from selftestlib import FAILURES, expect  # noqa: E402

__all__ = [
    "ARGV", "COMMIT", "CWD", "FAILURES", "FakeClaim", "NOW", "OTHER",
    "OTHER_COMMIT", "PREPARED_ENGINE", "PROBE", "Preparer", "Recorder",
    "Scratch", "TOOLS_DIR", "expect", "held_resources", "installed_census",
    "measurement", "run", "saved_runner_executable", "seams_restored",
    "selector_inputs",
]

TOOLS_DIR = str(Path(__file__).resolve().parent)

# Two REGISTERED, manual-only, protocol-compatible probe keys. Real keys
# rather than invented ones because the recorder validates against the
# live registry and refuses a CI-eligible probe -- but only their KEYS
# are used, and nothing here runs either of them.
PROBE = "position_hold"
OTHER = "role"

COMMIT = "a" * 40
OTHER_COMMIT = "b" * 40
NOW = datetime.datetime(2026, 8, 21, 12, 0, tzinfo=datetime.timezone.utc)


# --------------------------------------------------------------------------
# Synthetic state
# --------------------------------------------------------------------------
def selector_inputs(*, keys=(PROBE, OTHER), protocol=None) -> dict:
    """A synthetic registry and both classifications.

    Supplied rather than read from `ci_probes`, for the reason
    `test_probe_select` gives: CLAUDE.md's promotion procedure moves keys
    between the two classifications, and a gate pinned to live
    membership would redden on unrelated registry work.
    """
    statuses = protocol or {key: probe_protocol.PROTOCOL_VERSION for key in keys}
    return {
        "registry": {key: f"{key}_probe.py" for key in keys},
        "ci_eligible": frozenset(),
        "manual_only": frozenset(keys),
        "protocol": statuses,
    }


class Scratch:
    """A temporary census, claim root and artifact tree."""

    def __init__(self) -> None:
        self.root = Path(tempfile.mkdtemp(prefix="test_deflake_"))
        self.census = self.root / "probe_census.json"
        self.claims = self.root / "probe-claims"
        self.claims.mkdir()
        self.artifacts = self.root / "artifacts"
        self.artifacts.mkdir()
        probe_census.ensure_document(self.census)

    def bytes_now(self) -> bytes:
        return self.census.read_bytes()

    def entry(self, probe: str = PROBE) -> dict:
        document = json.loads(self.census.read_text(encoding="utf-8"))
        return probe_census.find_entry(document, probe) or {}

    def census_of(self, probe: str = PROBE) -> dict:
        return (self.entry(probe) or {}).get("census") or {}

    def cleanup(self) -> None:
        shutil.rmtree(self.root, ignore_errors=True)


def measurement(scratch: Scratch, *, probe: str = PROBE, outcomes=None,
                runs: int = deflake.CENSUS_RUN_COUNT,
                rts_caps: int = deflake.RTS_CAPABILITIES,
                commit: str = COMMIT, harness_error: bool = False):
    """A REAL `probe_flake.Measurement`, so its document is a real one."""
    descriptor = probe_protocol.build_descriptor(
        probe, [("alpha", "the only check")])
    invocation = scratch.artifacts / f"invocation-{uuid.uuid4().hex[:8]}"
    invocation.mkdir(parents=True, exist_ok=True)
    result = probe_flake.Measurement(probe, descriptor, runs, rts_caps,
                                     scratch.artifacts, invocation)
    result.commit_sha = commit
    result.timestamp = "2026-08-21T12:00:00Z"
    completed = list(outcomes if outcomes is not None
                     else [probe_flake.RUN_PASS] * runs)
    if harness_error:
        completed = completed[:runs - 1]
    for index, outcome in enumerate(completed, 1):
        run_dir = invocation / f"run-{index:03d}"
        run_dir.mkdir(parents=True, exist_ok=True)
        checks = {"alpha": {probe_flake.RUN_PASS: probe_protocol.PASS,
                            probe_flake.RUN_FAIL: probe_protocol.FAIL,
                            probe_flake.RUN_TIMEOUT: probe_protocol.MISSING,
                            }[outcome]}
        result.runs.append(probe_flake.RunRecord(
            index, 9100 + index, outcome, 1.5, checks, run_dir))
    if harness_error:
        error_dir = invocation / f"run-{runs:03d}"
        error_dir.mkdir(parents=True, exist_ok=True)
        result.status = "harness-error"
        result.error = f"run {runs}: the event stream could not be trusted"
        result.error_run = probe_flake.RunRecord(
            runs, 9100 + runs, "HARNESS_ERROR", 0.5, {}, error_dir)
    return result


class FakeClaim:
    """A `probe_claim.Claim` stand-in with the same ownership surface."""

    def __init__(self, probe: str = PROBE, *, token: str = "token-1",
                 lose_on: str = "", release_error: str = "") -> None:
        self.probe = probe
        self.token = token
        # Read by the REAL `probe_claim.Renewer`, which the orchestrator
        # runs around the measurement: the renewer is not faked, so a
        # fake claim has to answer the same lease question a real one
        # does. Long enough that its first tick never fires inside a test.
        self.lease_seconds = probe_claim.MIN_ORCHESTRATION_LEASE_SECONDS
        self.lose_on = lose_on
        self.release_error = release_error
        self.released = False
        self.commits: list = []
        self.reasserted = 0
        self.audits: list = []

    def census_record(self, *, commit_sha: str, requested_runs: int) -> dict:
        self.audits.append((commit_sha, requested_runs))
        return {"token": self.token, "timestamp_utc": "2026-08-21T12:00:00Z",
                "commit_sha": commit_sha, "owner": "selftest",
                "host": "selftest", "pid": os.getpid(),
                "lease_seconds": 1800.0, "requested_runs": requested_runs}

    def commit_while_held(self, commit, now=None):
        stage = "audit" if not self.commits else "record"
        self.commits.append(stage)
        if self.lose_on == stage:
            raise probe_claim.ClaimLost(
                f"the claim on {self.probe!r} is no longer ours")
        return commit()

    def reassert(self, now=None) -> None:
        self.reasserted += 1
        if self.lose_on == "reassert":
            raise probe_claim.ClaimLost(
                f"the claim on {self.probe!r} is no longer ours")

    def release(self) -> bool:
        if self.release_error:
            raise probe_claim.ClaimError(self.release_error)
        self.released = True
        return True


class Recorder:
    """Records what the measure seam was handed, and returns a measurement."""

    def __init__(self, result=None, raises=None) -> None:
        self.result = result
        self.raises = raises
        self.calls: list = []

    def __call__(self, probe, runs, *, artifact_root=None, rts_caps=None,
                 timeout=None, start_port=None, announce=None):
        self.calls.append({"probe": probe, "runs": runs,
                           "artifact_root": artifact_root,
                           "rts_caps": rts_caps, "timeout": timeout,
                           "start_port": start_port})
        if self.raises is not None:
            raise self.raises
        return self.result


ARGV = ["python3", "tools/deflake.py", "--json"]
CWD = "/private/tmp/synarchy-selftest-checkout"


def installed_census(*, probe: str = PROBE, acceptable: int = 0) -> dict:
    """A census document shaped like the one `update` installs.

    Only the fields the handoff reads: the row for the probe and its
    acceptable-failure count. `record_result_installed` hands the whole
    installed candidate back, and this is the part of it that matters
    here.
    """
    return {"schema": probe_census.CENSUS_SCHEMA,
            "probes": [{"key": probe, "classification": "manual-only",
                        "script": f"{probe}_probe.py", "protocol": "legacy",
                        "census": {"acceptable_failures": acceptable}}]}


def held_resources(probe, *, namespace=None, repo_root=None):
    """An injected resource hold that owns nothing and records nothing."""
    return probe_resource_lock.ResourceHold("selftest", frozenset(),
                                            frozenset(), {}, None)


#: What the preparation seam answers with when a case does not care.
#: Every case needs one: the real seam shells out to Cabal, and a suite
#: that is engine-free and toolchain-free must never reach it by
#: forgetting to substitute.
PREPARED_ENGINE = "/private/tmp/synarchy-selftest-checkout/synarchy"


class Preparer:
    """Records how the engine preparation seam was called."""

    def __init__(self, executable: str = PREPARED_ENGINE, raises=None,
                 observe=None) -> None:
        self.executable = executable
        self.raises = raises
        self.observe = observe
        self.calls: list = []

    def __call__(self, *, namespace, repo_root=None, announce=None):
        self.calls.append({"namespace": namespace, "repo_root": repo_root})
        if self.observe is not None:
            self.observe()
        if self.raises is not None:
            raise self.raises
        return self.executable


class saved_runner_executable:
    """Restore `probe_runner_resources.ENGINE_EXECUTABLE`, which `deflake` installs.

    It is a module global the runner reads when it hands a child probe
    its executable, so a case that leaves it set would decide what a
    later case observes.
    """

    def __enter__(self):
        self._saved = probe_runner_resources.ENGINE_EXECUTABLE
        return self

    def __exit__(self, *exc):
        probe_runner_resources.ENGINE_EXECUTABLE = self._saved
        return False


class seams_restored:
    """Assert, on exit, that every seam a case patches is back where it started.

    The seams a case in this suite reaches and restores in its own
    `finally`: `probe_runner_resources.ENGINE_EXECUTABLE` (installed by
    `deflake` under every `run`, and assigned by the preparation cases),
    `PATH` (prepended by the real-preparation case),
    `census_storage.tempfile.mkstemp` and `census_storage.os.fsync` (the two
    recorder-failure cases) and `argparse.ArgumentParser.parse_args` (the
    CLI case). A case that restores its own seam is the rule; this is
    the check that the rule held, run by every entry point around the
    cases it selected -- a focused owner included -- and on every
    outcome, an exception propagating through the cases included.
    """

    SEAMS = ("probe_runner_resources.ENGINE_EXECUTABLE", "PATH",
             "census_storage.tempfile.mkstemp", "census_storage.os.fsync",
             "argparse.ArgumentParser.parse_args")

    @staticmethod
    def snapshot() -> dict:
        return {
            "probe_runner_resources.ENGINE_EXECUTABLE":
                probe_runner_resources.ENGINE_EXECUTABLE,
            "PATH": os.environ.get("PATH"),
            "census_storage.tempfile.mkstemp": census_storage.tempfile.mkstemp,
            "census_storage.os.fsync": census_storage.os.fsync,
            "argparse.ArgumentParser.parse_args":
                argparse.ArgumentParser.parse_args,
        }

    def __enter__(self):
        self.before = self.snapshot()
        return self

    def __exit__(self, *exc):
        after = self.snapshot()
        for seam in self.SEAMS:
            expect(after[seam] == self.before[seam],
                   f"{seam} is restored once the selected cases have run "
                   f"(was {self.before[seam]!r}, now {after[seam]!r})")
        return False


def run(scratch: Scratch, **overrides):
    """`measure_next_probe` with every seam defaulted to a safe fake."""
    settings = {
        "census_path": scratch.census,
        "claim_root": scratch.claims,
        "artifact_root": scratch.artifacts,
        "now": NOW,
        "inputs": selector_inputs(),
        "acquire_claim": lambda probe, **kw: FakeClaim(probe),
        "acquire_resources": held_resources,
        "prepare_engine": Preparer(),
        "measure": Recorder(),
        "record_claim": lambda *a, **kw: PROBE,
        # The recording seam now answers with the census row the
        # transaction INSTALLED as well as the probe (#1659), because
        # the handoff's acceptable-failure count has to come from that
        # document rather than from a reread after the lock is gone.
        "record_result": lambda *a, **kw: (PROBE, installed_census()),
        "head_commit": lambda: COMMIT,
        "argv": ARGV,
        "cwd": CWD,
        "read_configuration": lambda root: [],
    }
    settings.update(overrides)
    # `measure_next_probe` installs the prepared executable as the
    # runner's module global (#1913) and leaves it there: for the tool
    # that is right, since the process is about to hand it to child
    # probes. For a case it is a leak -- a focused owner would leave the
    # synthetic path behind for whatever runs next in this interpreter
    # -- so it goes back on every exit, exceptions included. The
    # preparation cases observe the value INSIDE the call, which this
    # does not touch.
    with saved_runner_executable():
        return deflake.measure_next_probe(**settings)
