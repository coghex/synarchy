#!/usr/bin/env python3
"""Shared fixtures for `test_probe_census.py`'s two case owners (#2034).

`tools/test_probe_census.py` is the complete census gate and
`tools/test_probe_census_promotion.py` is the focused owner of #1441's
CI-promotion report. Both drive the same synthetic world, so this module
is the ONE source of it: the synthetic registry and the context manager
that installs it, the throwaway scratch tree, the scratch repository
with a real `docs-wip` worktree, the in-process CLI driver, the realistic
`probe-flake-result/v1` document, the fixed evaluation moment the
staleness cases are written against, and the two assertion helpers that
are census-specific rather than generic.

Single-sourced for correctness, not tidiness. `FAILURES` is
`tools/selftestlib.py`'s ONE list (#1922), re-exported here so both
owners append to the same accumulator: two private copies would let the
census gate exit 0 while the promotion owner had recorded a failure --
and #2034's whole condition is that a promotion regression still fails
`python3 tools/test_probe_census.py`. `expect_refusal` is here for the
same reason `expect` is: a second copy is a second definition of what a
controlled refusal looks like.

The scope is fenced deliberately. Only what the promotion owner actually
consumes moved here; every fixture the census gate alone uses -- the
stored-schema documents, the sample and attempt records, the malformed
mutators, the schema-file and dependency harnesses -- stays in
`tools/test_probe_census.py`. `tools/test_probe_census_page.py`'s
pre-existing private copies of `expect_refusal`, `registry` and
`scratch` are deliberately NOT converged onto this module; that is a
separate change to a separate gate.

`tools/probe_claim_selftest_support.py` (#2100) is the in-repo precedent
for this shape.

Nothing here runs a case and this module is not a gate of its own.
"""
from __future__ import annotations

import datetime
import os
import shutil
import subprocess
import sys
import tempfile
from contextlib import contextmanager
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import ci_probes  # type: ignore  # noqa: E402
import probe_census  # type: ignore  # noqa: E402
import probe_flake  # type: ignore  # noqa: E402
import probe_engine  # type: ignore  # noqa: E402
import probe_runner_registry  # type: ignore  # noqa: E402

from selftestlib import FAILURES, expect  # noqa: E402

__all__ = [
    "COMMIT_A", "COMMIT_B", "DAY", "FAILURES", "NOW", "SYNTHETIC", "at",
    "cli", "cli_repo", "expect", "expect_refusal", "registry",
    "result_document", "scratch", "seeded", "unchanged",
]

COMMIT_A = "a" * 40
COMMIT_B = "b" * 40

# One fixed evaluation moment. Nothing that uses it reads a clock:
# staleness is a function of an injected `now`, so a boundary case is a
# boundary case on every machine and at every hour.
NOW = datetime.datetime(2026, 8, 21, 12, 0, 0, tzinfo=datetime.timezone.utc)
DAY = probe_census.SECONDS_PER_DAY


def at(offset_days: float) -> str:
    """A census timestamp `offset_days` BEFORE the evaluation moment."""
    moment = NOW - datetime.timedelta(days=offset_days)
    return moment.strftime(probe_census.TIMESTAMP_FORMAT)


def expect_refusal(call, msg: str, *fragments: str) -> None:
    """`call` refuses with a `CensusError` naming each fragment."""
    try:
        call()
    except probe_census.CensusError as error:
        text = str(error)
        missing = [f for f in fragments if f not in text]
        if missing:
            expect(False, f"{msg} (message {text!r} is missing {missing})")
        else:
            expect(True, msg)
        return
    except Exception as error:  # noqa: BLE001 - an uncontrolled failure IS the bug
        expect(False, f"{msg} (raised {type(error).__name__}: {error})")
        return
    expect(False, f"{msg} (nothing was raised)")


# ==========================================================================
# Fixtures
# ==========================================================================
SYNTHETIC = [
    ("alpha", "alpha_probe.py", "the first synthetic probe"),
    ("beta", "beta_probe.py", "the second synthetic probe"),
    ("gamma", "gamma_probe.py", "the third synthetic probe"),
]


@contextmanager
def registry(probes=None, ci_eligible=(), protocol=None, reasons=None):
    """The live registries, pointed at a synthetic set for one case.

    `reasons` is `ci_probes.MANUAL_ONLY_REASONS` for the duration.
    #1441's report reads it LIVE, the same way it reads `CI_ELIGIBLE`,
    so a case that needs a synthetic probe to be `needs-gpu` states it
    here rather than reaching into the real registry.
    """
    saved = (probe_runner_registry.PROBES, ci_probes.CI_ELIGIBLE,
             probe_flake.PROTOCOL_PROBES, ci_probes.MANUAL_ONLY_REASONS)
    probe_runner_registry.PROBES = list(SYNTHETIC if probes is None else probes)
    ci_probes.CI_ELIGIBLE = set(ci_eligible)
    probe_flake.PROTOCOL_PROBES = dict(protocol or {})
    ci_probes.MANUAL_ONLY_REASONS = dict(reasons or {})
    try:
        yield
    finally:
        (probe_runner_registry.PROBES, ci_probes.CI_ELIGIBLE,
         probe_flake.PROTOCOL_PROBES,
         ci_probes.MANUAL_ONLY_REASONS) = saved


@contextmanager
def scratch(prefix="probe-census-test-"):
    root = Path(tempfile.mkdtemp(prefix=prefix))
    try:
        yield root
    finally:
        shutil.rmtree(root, ignore_errors=True)


def result_document(probe="alpha", status="ok", commit=COMMIT_A, **overrides):
    """A realistic `probe-flake-result/v1` document.

    It carries every producer-only field `Measurement.to_document` adds,
    so the exclusion case has something real to prove.

    A non-`ok` status returns what a real harness error looks like, not
    an accepted measurement with its status field flipped: the run that
    broke the stream is reported as `error_run` and is NOT one of the
    completed runs, so one requested run is left uncompleted and the
    tally covers only the run that finished. Since #1493 those are
    cross-field invariants, so a flipped-status fixture would be state
    the producer cannot write.
    """
    document = {
        "schema": probe_census.RESULT_SCHEMA,
        "probe": probe,
        "status": status,
        "error": None if status == "ok" else "run 2 emitted a duplicate event",
        "requested_runs": 2,
        "completed_runs": 2,
        "runs": [
            {"index": 1, "port": 9100, "outcome": "PASS",
             "elapsed_seconds": 12.5,
             "checks": {"first": "PASS", "second": "PASS"},
             "artifact_dir": None},
            {"index": 2, "port": 9101, "outcome": "FAIL",
             "elapsed_seconds": 13.25,
             "checks": {"first": "PASS", "second": "FAIL"},
             "artifact_dir": "/tmp/artifacts/run-002"},
        ],
        "error_run": None,
        "checks": [{"id": "first", "label": "the first check"},
                   {"id": "second", "label": "the second check"}],
        "check_counts": {"first": {"PASS": 2, "FAIL": 0, "MISSING": 0},
                         "second": {"PASS": 1, "FAIL": 1, "MISSING": 0}},
        "failure_count": 1,
        "failure_rate": 0.5,
        "timeout_count": 0,
        "worst_elapsed_seconds": 13.25,
        "total_elapsed_seconds": 25.75,
        "timestamp_utc": "2026-08-21T05:00:00Z",
        "commit_sha": commit,
        "rts_capabilities": 4,
        "peak_concurrency": 1,
        "artifact_root": "/tmp/artifacts",
        "invocation_dir": "/home/dev/synarchy",
        "retained_artifacts": ["/tmp/artifacts/run-002"],
    }
    if status != "ok":
        document.update({
            "completed_runs": 1,
            "runs": [{"index": 1, "port": 9100, "outcome": "PASS",
                      "elapsed_seconds": 12.5,
                      "checks": {"first": "PASS", "second": "MISSING"},
                      "artifact_dir": None}],
            "error_run": {"index": 2, "port": 9101, "outcome": "HARNESS_ERROR",
                          "elapsed_seconds": 0.5, "checks": {},
                          "artifact_dir": "/tmp/artifacts/run-002"},
            "check_counts": {"first": {"PASS": 1, "FAIL": 0, "MISSING": 0},
                             "second": {"PASS": 0, "FAIL": 0, "MISSING": 1}},
            "failure_count": 0,
            "failure_rate": None,
            "timeout_count": 0,
            "worst_elapsed_seconds": 12.5,
            "total_elapsed_seconds": 12.5,
        })
    document.update(overrides)
    return document

def seeded(path: Path) -> dict:
    """A fresh v2 census on disk, from the synthetic registry."""
    return probe_census.ensure_document(path)


def unchanged(path: Path, before: bytes, msg: str) -> None:
    expect(path.read_bytes() == before, msg)


# ==========================================================================
# The CLI harness
# ==========================================================================
@contextmanager
def cli_repo():
    """A scratch git repository with a real `docs-wip` worktree."""
    with scratch("probe-census-cli-") as root:
        main_wt = root / "main"
        docs_wt = root / "docs-worktree"
        env = {**os.environ, "GIT_CONFIG_GLOBAL": str(root / "gitconfig"),
               "GIT_CONFIG_SYSTEM": "/dev/null"}
        run = lambda *a, **k: subprocess.run(  # noqa: E731
            a, cwd=str(k.pop("cwd", main_wt)), env=env, check=True,
            capture_output=True, text=True)
        subprocess.run(["git", "init", "-q", "-b", "master", str(main_wt)],
                       env=env, check=True, capture_output=True)
        run("git", "config", "user.email", "test@example.invalid")
        run("git", "config", "user.name", "Census Test")
        run("git", "commit", "-q", "--allow-empty", "-m", "root")
        run("git", "worktree", "add", "-q", str(docs_wt), "-b", "docs-wip")
        saved = probe_engine.REPO_ROOT
        probe_engine.REPO_ROOT = str(main_wt)
        try:
            yield main_wt, docs_wt / probe_census.MANIFEST_RELPATH
        finally:
            probe_engine.REPO_ROOT = saved


def cli(*argv):
    """`main(argv)` with its streams captured. Returns (code, out, err).

    argparse's own usage errors raise `SystemExit`; they are a non-zero
    exit like any other, so they are reported as one here.
    """
    import io
    from contextlib import redirect_stdout, redirect_stderr
    out, err = io.StringIO(), io.StringIO()
    try:
        with redirect_stdout(out), redirect_stderr(err):
            code = probe_census.main(list(argv))
    except SystemExit as exit_code:
        code = exit_code.code if isinstance(exit_code.code, int) else 1
    return code, out.getvalue(), err.getvalue()
