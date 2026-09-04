#!/usr/bin/env python3
"""Shared support for `test_probe_claim.py`'s three case owners (#2100).

This is the ONE source of the synthetic infrastructure the owner
modules -- `probe_claim_selftest_claim`, `probe_claim_selftest_census`
and `probe_claim_selftest_orchestration` -- share: the assertion helpers
and the single failure accumulator behind them, the synthetic
registries, the scratch trees and the scratch repository with its real
linked worktrees, the real `probe_flake.Measurement` builder, and the
subprocess programs the concurrency cases race.

Two of those are single-sourced for correctness rather than tidiness:

* `FAILURES` is the ONE list `expect` and `expect_raises` append to.
  Three owners each holding a private copy would let the aggregate exit
  0 while a sibling owner had recorded a failure. Since #1922 that one
  list is `tools/selftestlib.py`'s, re-exported here so the owners import
  it -- and the quiet-by-default `expect` behind it -- from the single
  place they already import everything else shared from.
* `ARTIFACTS` is created -- and its `atexit` removal registered -- once
  at import time, so importing all three owners together still produces
  one retained-artifact root and one cleanup, not three.

Nothing here runs a case and this module is not a gate of its own:
`python3 tools/test_probe_claim.py` remains the only invocation, in CI
and in `make ci` alike.
"""
from __future__ import annotations

import atexit
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time
from contextlib import contextmanager
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import ci_probes  # type: ignore  # noqa: E402
import probe_census  # type: ignore  # noqa: E402
import probe_flake  # type: ignore  # noqa: E402
import probe_protocol  # type: ignore  # noqa: E402
import probe_engine  # type: ignore  # noqa: E402
import probe_runner_registry  # type: ignore  # noqa: E402

from selftestlib import FAILURES, expect  # noqa: E402

TOOLS = str(Path(__file__).resolve().parent)
COMMIT_A = "a" * 40
COMMIT_B = "b" * 40

SYNTHETIC = [
    ("alpha", "alpha_probe.py", "the first synthetic probe"),
    ("beta", "beta_probe.py", "the second synthetic probe"),
    ("gamma", "gamma_probe.py", "the third synthetic probe"),
]


def skip(msg: str) -> None:
    """A case this environment cannot pose. Reported, never counted."""
    print(f"  skip {msg}")


def expect_raises(kind, call, msg: str, *fragments: str) -> None:
    try:
        call()
    except kind as error:
        missing = [f for f in fragments if f not in str(error)]
        expect(not missing,
               msg if not missing else f"{msg} (missing {missing} in {error})")
        return
    except Exception as error:  # noqa: BLE001
        expect(False, f"{msg} (raised {type(error).__name__}: {error})")
        return
    expect(False, f"{msg} (nothing was raised)")


# ==========================================================================
# Fixtures
# ==========================================================================
@contextmanager
def registry(probes=None, ci_eligible=(), protocol=None):
    """The live registries, pointed at a synthetic set for one case."""
    saved = (probe_runner_registry.PROBES, ci_probes.CI_ELIGIBLE,
             probe_flake.PROTOCOL_PROBES)
    probe_runner_registry.PROBES = list(SYNTHETIC if probes is None else probes)
    ci_probes.CI_ELIGIBLE = set(ci_eligible)
    probe_flake.PROTOCOL_PROBES = dict(
        {key: probe_protocol.PROTOCOL_VERSION for key, _s, _p in probe_runner_registry.PROBES}
        if protocol is None else protocol)
    try:
        yield
    finally:
        (probe_runner_registry.PROBES, ci_probes.CI_ELIGIBLE,
         probe_flake.PROTOCOL_PROBES) = saved


@contextmanager
def scratch(prefix="probe-claim-test-"):
    root = Path(tempfile.mkdtemp(prefix=prefix))
    try:
        yield root
    finally:
        shutil.rmtree(root, ignore_errors=True)


@contextmanager
def claim_root():
    with scratch() as root:
        yield root / "probe-claims"


@contextmanager
def scratch_repo():
    """A scratch repository with a linked worktree and a `docs-wip` one.

    Three checkouts of ONE repository, which is what the namespace rule
    is actually about: `main`, a second ordinary worktree, and the
    docs-wip worktree the census resolves through.
    """
    with scratch("probe-claim-repo-") as root:
        main_wt, other_wt, docs_wt = root / "main", root / "other", root / "docs"
        env = {**os.environ, "GIT_CONFIG_GLOBAL": str(root / "gitconfig"),
               "GIT_CONFIG_SYSTEM": "/dev/null"}

        def run(*args, cwd=None):
            return subprocess.run(args, cwd=str(cwd or main_wt), env=env,
                                  check=True, capture_output=True, text=True)

        subprocess.run(["git", "init", "-q", "-b", "master", str(main_wt)],
                       env=env, check=True, capture_output=True)
        run("git", "config", "user.email", "test@example.invalid")
        run("git", "config", "user.name", "Claim Test")
        run("git", "commit", "-q", "--allow-empty", "-m", "root")
        run("git", "worktree", "add", "-q", str(other_wt), "-b", "feature")
        run("git", "worktree", "add", "-q", str(docs_wt), "-b", "docs-wip")
        saved = probe_engine.REPO_ROOT
        probe_engine.REPO_ROOT = str(main_wt)
        try:
            yield main_wt, other_wt, docs_wt / probe_census.MANIFEST_RELPATH
        finally:
            probe_engine.REPO_ROOT = saved


def seeded_census(path: Path) -> Path:
    """A freshly seeded v3 census for the synthetic registry."""
    probe_census.ensure_document(path)
    return path


def descriptor(probe="alpha") -> probe_protocol.Descriptor:
    return probe_protocol.Descriptor(
        probe=probe, checks=(("first", "the first check"),
                             ("second", "the second check")))


ARTIFACTS = tempfile.mkdtemp(prefix="probe-claim-artifacts-")
atexit.register(shutil.rmtree, ARTIFACTS, True)


def fake_measurement(probe="alpha", runs=2, *, harness_error=False,
                     artifact_root=None):
    """A real `probe_flake.Measurement`, filled in without running anything.

    Its artifact root is a throwaway tree of this run's own, because a
    completed measurement is now RETAINED under its invocation
    directory — so a hard-coded root would have every case here writing
    into a shared path outside the scratch.
    """
    artifact_root = ARTIFACTS if artifact_root is None else artifact_root
    measurement = probe_flake.Measurement(
        probe, descriptor(probe), runs, 4, Path(artifact_root),
        Path(artifact_root) / "invocation")
    measurement.commit_sha = COMMIT_A
    measurement.timestamp = "2026-08-21T05:00:00Z"
    if harness_error:
        measurement.status = "harness-error"
        measurement.error = "run 2: duplicate event"
        measurement.runs = [probe_flake.RunRecord(
            1, 9100, probe_flake.RUN_PASS, 1.0,
            {"first": "PASS", "second": "PASS"}, None)]
        measurement.error_run = probe_flake.RunRecord(
            2, 9101, probe_flake.RUN_HARNESS_ERROR, 0.5, {}, None)
    else:
        measurement.runs = [
            probe_flake.RunRecord(index, 9100 + index, probe_flake.RUN_PASS,
                                  1.0, {"first": "PASS", "second": "PASS"},
                                  None)
            for index in range(1, runs + 1)]
    return measurement


CONTENDER = f"""
import json, os, sys, time
sys.path.insert(0, {TOOLS!r})
import probe_runner_registry
probe_runner_registry.PROBES = [(k, k + '_probe.py', k)
                                for k in ('alpha', 'beta', 'gamma')]
import probe_claim_lease as claim_lease
import probe_claim_storage as claim_storage

root, probe, barrier, lease = sys.argv[1], sys.argv[2], sys.argv[3], float(sys.argv[4])
hold = float(sys.argv[5]) if len(sys.argv) > 5 else 0.0
# An optional future offset, so a contender can look at a claim the way
# its own clock would once the lease had elapsed.
skew = float(sys.argv[6]) if len(sys.argv) > 6 else 0.0
# Every contender waits for the same starting gun, so the race is real.
while not os.path.exists(barrier):
    time.sleep(0.01)
# "I am about to block on the claim lock": a waiter that has not reached
# the lock yet would make a still-running assertion vacuous.
open(barrier + '.ready', 'w').close()
try:
    when = claim_storage.utc_now() + __import__('datetime').timedelta(seconds=skew)
    claim = claim_lease.acquire(probe, root=__import__('pathlib').Path(root),
                                lease_seconds=lease, now=when)
except claim_lease.ClaimDenied as denied:
    print(json.dumps({{"outcome": "denied", "detail": denied.describe()}}))
    sys.exit(0)
except claim_storage.ClaimError as error:
    print(json.dumps({{"outcome": "error", "detail": str(error)}}))
    sys.exit(0)
print(json.dumps({{"outcome": "won", "token": claim.token}}), flush=True)
if hold > 0:
    time.sleep(hold)
    claim.release()
"""


LOCK_HOLDER = f"""
import os, sys, time
sys.path.insert(0, {TOOLS!r})
import probe_runner_registry
probe_runner_registry.PROBES = [(k, k + '_probe.py', k)
                                for k in ('alpha', 'beta', 'gamma')]
import probe_claim_storage as claim_storage
from pathlib import Path

root, probe, held, ready = sys.argv[1], sys.argv[2], float(sys.argv[3]), sys.argv[4]
with claim_storage.serialized(probe, Path(root)):
    open(ready, 'w').close()
    time.sleep(held)
"""


def race(root: Path, probe: str, count: int, *, lease=60.0):
    """`count` separate processes contend for one probe. Returns their rows."""
    root.mkdir(parents=True, exist_ok=True)
    barrier = root.parent / f"barrier-{probe}-{count}"
    children = [subprocess.Popen(
        [sys.executable, "-c", CONTENDER, str(root), probe, str(barrier),
         str(lease)], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        for _ in range(count)]
    time.sleep(0.4)
    barrier.write_text("go", encoding="utf-8")
    rows = []
    for process in children:
        out, err = process.communicate(timeout=120)
        try:
            rows.append(json.loads(out.strip().splitlines()[-1]))
        except (IndexError, ValueError):
            rows.append({"outcome": "crashed", "detail": (err or out)[-400:]})
    return rows
