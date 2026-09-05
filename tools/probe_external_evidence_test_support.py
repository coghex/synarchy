#!/usr/bin/env python3
"""Shared support for `test_probe_external_evidence.py`'s four case owners (#2187).

This is the ONE source of everything the owner modules --
`probe_external_evidence_test_identity`, `_reports`, `_confinement` and
`_resilience` -- share: the assertion helpers and the single failure
accumulator behind them, the `Tripwire`, the read-path recorder, the
forbidden-subprocess and forbidden-lock instrumentation with its
before/after tree digests, the synthetic run records, the report
Markdown builder, the synthetic registry and state-tree builder, and
the evidence-reader adapters.

`PASSED` and `FAILURES` are single-sourced for correctness rather than
tidiness: four owners each holding a private copy would let the
aggregate exit 0 while a sibling owner had recorded a failure. `check`
is quiet on a pass and records a failure only through that one list;
the aggregate reads both back after the selected cases have run.

The central contract under test is NON-INTERACTION, and it is proved
mechanically rather than inferred from the reader's output:

* every file under the synthetic tree is digested before and after each
  read, and the digests (and the path set) must be identical -- registry,
  reports and lock files alike;
* the confinement cases record every file the reader actually opens, so
  an out-of-scope read fails even though the reader would never echo a
  byte of it back;
* `subprocess.run` / `subprocess.Popen` / `call` / `check_output` are
  replaced with tripwires, so a coordinator invocation of ANY subcommand
  -- permitted or mutating -- fails the test rather than passing quietly;
* `fcntl.flock` / `fcntl.lockf` are replaced with tripwires, so taking
  any `$test` lock fails the test.

Both instrumentation guards are context managers, so every patched
function is restored on the way out even when a case raises inside.

Nothing here runs a case and this module is not a gate of its own:
`python3 tools/test_probe_external_evidence.py` remains the only
invocation.
"""
from __future__ import annotations

import fcntl
import hashlib
import json
import os
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_external_evidence as evidence  # noqa: E402

FAILURES: list[str] = []
PASSED = 0

SENTINEL = "SENTINEL-OUT-OF-SCOPE-CONTENT-MUST-NEVER-BE-READ"


# --------------------------------------------------------------------------
# Harness
# --------------------------------------------------------------------------

def check(condition: bool, label: str, detail: str = "") -> None:
    global PASSED
    if condition:
        PASSED += 1
        return
    FAILURES.append(f"{label}{': ' + detail if detail else ''}")


def check_equal(actual: object, expected: object, label: str) -> None:
    check(actual == expected, label, f"expected {expected!r}, got {actual!r}")


class Tripwire(AssertionError):
    """Raised when the reader reaches a forbidden interaction."""


class RecordReads:
    """Record every file path the reader actually opens for reading.

    The confinement contract cannot be proved from the reader's output
    alone: it extracts only an interpretation status and a section
    count, so an out-of-scope file could be read without a byte of it
    surfacing. This records the reads themselves.
    """

    def __init__(self) -> None:
        self.paths: list[Path] = []
        self._saved: dict[str, object] = {}

    def __enter__(self) -> "RecordReads":
        recorder = self

        def record(name: str, original):
            def wrapper(self_path, *args, **kwargs):
                recorder.paths.append(Path(self_path))
                return original(self_path, *args, **kwargs)
            return wrapper

        self._saved = {
            "read_text": Path.read_text,
            "read_bytes": Path.read_bytes,
            "open": Path.open,
        }
        Path.read_text = record("read_text", self._saved["read_text"])   # type: ignore[assignment]
        Path.read_bytes = record("read_bytes", self._saved["read_bytes"])  # type: ignore[assignment]
        Path.open = record("open", self._saved["open"])                  # type: ignore[assignment]
        return self

    def __exit__(self, *exc_info: object) -> bool:
        Path.read_text = self._saved["read_text"]      # type: ignore[assignment]
        Path.read_bytes = self._saved["read_bytes"]    # type: ignore[assignment]
        Path.open = self._saved["open"]                # type: ignore[assignment]
        return False


class NonInteraction:
    """Forbid subprocesses and locks, and pin every byte under `root`."""

    def __init__(self, root: Path) -> None:
        self.root = root
        self.before: dict[str, str] = {}
        self._saved: dict[str, object] = {}

    def _digest_tree(self) -> dict[str, str]:
        digests: dict[str, str] = {}
        for path in sorted(self.root.rglob("*")):
            relative = str(path.relative_to(self.root))
            if path.is_symlink():
                digests[relative] = "symlink:" + os.readlink(path)
            elif path.is_dir():
                digests[relative] = "dir"
            else:
                try:
                    digests[relative] = hashlib.sha256(path.read_bytes()).hexdigest()
                except OSError as exc:
                    digests[relative] = f"unreadable:{exc.errno}"
        return digests

    def __enter__(self) -> "NonInteraction":
        self.before = self._digest_tree()

        def forbidden_subprocess(*args: object, **kwargs: object) -> None:
            raise Tripwire(f"the reader invoked a subprocess: {args!r}")

        def forbidden_flock(*args: object, **kwargs: object) -> None:
            raise Tripwire("the reader took a lock")

        self._saved = {
            "run": subprocess.run,
            "Popen": subprocess.Popen,
            "call": subprocess.call,
            "check_output": subprocess.check_output,
            "flock": fcntl.flock,
            "lockf": fcntl.lockf,
        }
        subprocess.run = forbidden_subprocess            # type: ignore[assignment]
        subprocess.Popen = forbidden_subprocess          # type: ignore[assignment]
        subprocess.call = forbidden_subprocess           # type: ignore[assignment]
        subprocess.check_output = forbidden_subprocess   # type: ignore[assignment]
        fcntl.flock = forbidden_flock                    # type: ignore[assignment]
        fcntl.lockf = forbidden_flock                    # type: ignore[assignment]
        return self

    def __exit__(self, *exc_info: object) -> bool:
        subprocess.run = self._saved["run"]               # type: ignore[assignment]
        subprocess.Popen = self._saved["Popen"]           # type: ignore[assignment]
        subprocess.call = self._saved["call"]             # type: ignore[assignment]
        subprocess.check_output = self._saved["check_output"]  # type: ignore[assignment]
        fcntl.flock = self._saved["flock"]                # type: ignore[assignment]
        fcntl.lockf = self._saved["lockf"]                # type: ignore[assignment]
        return False

    def assert_untouched(self, label: str) -> None:
        after = self._digest_tree()
        check_equal(sorted(after), sorted(self.before), f"{label}: path set unchanged")
        changed = [p for p in after if p in self.before and after[p] != self.before[p]]
        check(not changed, f"{label}: bytes unchanged", f"changed: {changed}")


# --------------------------------------------------------------------------
# Synthetic state
# --------------------------------------------------------------------------

def make_run(test_id: str, run_id: str, **overrides: object) -> dict:
    """A synthetic registry record shaped like a real completed run."""
    record = {
        "area": "synthetic",
        "claimed_at": "2026-08-12T17:41:35Z",
        "completed_at": "2026-08-12T17:47:26Z",
        "elapsed_seconds": 288.783,
        "execution_status": "passed",
        "interpretation_outcome": "clean",
        "revision": "8f995f395dd1748f67ffcaeedc5cf8d7c2e9e430",
        "revision_committed_at": "2026-08-12T10:33:25-07:00",
        "revision_subject": "Document audio system design",
        "run_id": run_id,
        "status": "completed",
        "test_exit_code": 0,
        "test_id": test_id,
    }
    record.update(overrides)
    return {k: v for k, v in record.items() if v is not None or k in overrides}


MISSING = object()


def run_with_identity(run_id: str, identity: object, **overrides: object) -> dict:
    """A record whose `test_id` is set to an arbitrary value, or removed.

    `make_run` takes `test_id` positionally and drops None values, so a
    deliberately damaged identity has to be written onto the record
    afterwards — including the case where the field is absent entirely.
    """
    record = make_run("probe:placeholder", run_id, **overrides)
    if identity is MISSING:
        record.pop("test_id", None)
    else:
        record["test_id"] = identity
    return record


def report_text(run_id: str, test_id: str, interpretation: str,
                observations: int) -> str:
    lines = [
        "---",
        'schema: "codex-test-result/v1"',
        f'run_id: "{run_id}"',
        f'test_id: "{test_id}"',
        f'execution_status: "passed"',
        f'interpretation_status: "{interpretation}"',
        "---",
        "",
        f"# Test result: {test_id}",
        "",
        "## Observations",
        "",
    ]
    if observations == 0:
        lines.append("No reportable observations.")
    for index in range(1, observations + 1):
        lines.append(f"### OBS-{index:03d} — synthetic observation {index}")
        lines.append("")
        lines.append("- **Category:** gameplay")
        lines.append("")
    return "\n".join(lines) + "\n"


def build_state(root: Path, runs: list[dict], reports: dict[str, tuple[str, int]],
                *, schema: str = evidence.COORDINATOR_SCHEMA) -> Path:
    """Write a synthetic `codex-test` tree and return its root."""
    state = root / evidence.STATE_DIRNAME
    (state / evidence.REPORTS_DIRNAME).mkdir(parents=True, exist_ok=True)
    (state / "logs").mkdir(parents=True, exist_ok=True)
    (state / "registry.lock").write_text("", encoding="utf-8")
    (state / "base.lock").write_text("", encoding="utf-8")
    for run_id, (interpretation, count) in reports.items():
        test_id = next((r["test_id"] for r in runs if r.get("run_id") == run_id), "probe:x")
        path = state / evidence.REPORTS_DIRNAME / (run_id + evidence.REPORT_SUFFIX)
        path.write_text(report_text(run_id, test_id, interpretation, count),
                        encoding="utf-8")
    document = {
        "schema": schema,
        "updated_at": "2026-08-12T17:47:26Z",
        "snapshots": [],
        "proposals": [],
        "runs": runs,
    }
    (state / evidence.REGISTRY_FILENAME).write_text(
        json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return state


def report_path(state: Path, run_id: str) -> str:
    return str(state / evidence.REPORTS_DIRNAME / (run_id + evidence.REPORT_SUFFIX))


def read(state: Path | str, probe: str) -> dict:
    return evidence.read_probe_evidence(probe, state_root=state)
