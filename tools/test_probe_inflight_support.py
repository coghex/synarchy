#!/usr/bin/env python3
"""Shared support for the in-flight self-test (#1433, split by #2141).

Case-free. This module owns the pieces every case owner needs and that
must have exactly ONE implementation across the split:

* assertion state (`FAILURES`, `PASSED`) and the `check` / `check_equal`
  collectors that mutate it;
* `Tripwire`, the single exception class every tripwire raises and every
  `except Tripwire` clause in a case owner catches;
* the global `Offline` context and the per-evaluation `NonInteraction`
  context with its byte-for-byte tree comparison;
* the fixed evaluation time and the repository constants;
* the synthetic registry, report, issue, pull-request and GitHub
  transport fixtures;
* the fully injected `evaluate` helper and the `sources_of` selector.

`Tripwire`, `Offline` and `NonInteraction` are load-bearing as
identities, not merely as behavior: `Offline` patches
`probe_inflight.default_github_transport` as an attribute of that one
module object, and the integration owner discriminates on
`except Tripwire`. A second definition of any of them in a child module
would silently reopen the boundary it exists to hold, so every owner
imports these from here.

`PASSED` is an `int` rebound by `check`, so a reader must reach it
through this module (`support.PASSED`) rather than binding it at import;
`from ... import PASSED` would freeze a stale zero.

Not independently runnable and not a gate: it defines no cases and has
no command-line interface. The only entry point is
`tools/test_probe_inflight.py`.
"""
from __future__ import annotations

import fcntl
import hashlib
import json
import os
import socket
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_external_evidence as evidence  # noqa: E402
import probe_inflight as inflight  # noqa: E402
# The canonical probe registry (#2074). Imported here, and nowhere via a
# `run_probes` facade, so the whole split keeps one registry owner.
import probe_runner_registry  # noqa: E402,F401

FAILURES: list[str] = []
PASSED = 0

NOW = datetime(2026, 8, 21, 12, 0, 0, tzinfo=timezone.utc)
REPOSITORY = "coghex/synarchy"
ISSUES_PATH = f"repos/{REPOSITORY}/issues"
PULLS_PATH = f"repos/{REPOSITORY}/pulls"


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
    """Raised when the component reaches a forbidden interaction."""


class Offline:
    """Forbid the network, the `gh` binary, and the default transport.

    Installed for the WHOLE run. A case that forgets to inject a
    transport must fail loudly: a silent skip would let every
    pagination, open-versus-closed and draft-versus-merged check pass
    without ever being exercised.
    """

    def __enter__(self) -> "Offline":
        def forbidden_socket(*args: object, **kwargs: object) -> None:
            raise Tripwire("the component opened a socket")

        def forbidden_transport(*args: object, **kwargs: object) -> None:
            raise Tripwire(
                "the component reached for the default GitHub transport; "
                "every case must inject one")

        self._saved = {
            "socket": socket.socket,
            "create_connection": socket.create_connection,
            "transport": inflight.default_github_transport,
            "run": subprocess.run,
        }

        def guarded_run(args, *rest, **kwargs):
            argv = list(args) if isinstance(args, (list, tuple)) else [args]
            if argv and str(argv[0]).rsplit("/", 1)[-1] == "gh":
                raise Tripwire(f"the component invoked the gh binary: {argv!r}")
            return self._saved["run"](args, *rest, **kwargs)

        socket.socket = forbidden_socket                  # type: ignore[assignment]
        socket.create_connection = forbidden_socket       # type: ignore[assignment]
        inflight.default_github_transport = forbidden_transport  # type: ignore[assignment]
        subprocess.run = guarded_run                      # type: ignore[assignment]
        return self

    def __exit__(self, *exc_info: object) -> bool:
        socket.socket = self._saved["socket"]                         # type: ignore[assignment]
        socket.create_connection = self._saved["create_connection"]   # type: ignore[assignment]
        inflight.default_github_transport = self._saved["transport"]  # type: ignore[assignment]
        subprocess.run = self._saved["run"]                           # type: ignore[assignment]
        return False


class NonInteraction:
    """Forbid subprocesses and locks, and pin every byte under `roots`."""

    def __init__(self, *roots: Path) -> None:
        self.roots = [Path(root) for root in roots]
        self.before: dict[str, str] = {}
        self._saved: dict[str, object] = {}

    def _digest_tree(self) -> dict[str, str]:
        digests: dict[str, str] = {}
        for root in self.roots:
            if not root.exists():
                digests[f"{root}:MISSING"] = "missing"
                continue
            for path in sorted(root.rglob("*")):
                relative = f"{root}:{path.relative_to(root)}"
                if path.is_symlink():
                    digests[relative] = "symlink:" + os.readlink(path)
                elif path.is_dir():
                    digests[relative] = "dir"
                else:
                    try:
                        digests[relative] = hashlib.sha256(
                            path.read_bytes()).hexdigest()
                    except OSError as exc:
                        digests[relative] = f"unreadable:{exc.errno}"
        return digests

    def __enter__(self) -> "NonInteraction":
        self.before = self._digest_tree()

        def forbidden_subprocess(*args: object, **kwargs: object) -> None:
            raise Tripwire(f"the component invoked a subprocess: {args!r}")

        def forbidden_flock(*args: object, **kwargs: object) -> None:
            raise Tripwire("the component took a lock")

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
        subprocess.run = self._saved["run"]                    # type: ignore[assignment]
        subprocess.Popen = self._saved["Popen"]                # type: ignore[assignment]
        subprocess.call = self._saved["call"]                  # type: ignore[assignment]
        subprocess.check_output = self._saved["check_output"]  # type: ignore[assignment]
        fcntl.flock = self._saved["flock"]                     # type: ignore[assignment]
        fcntl.lockf = self._saved["lockf"]                     # type: ignore[assignment]
        return False

    def assert_untouched(self, label: str) -> None:
        after = self._digest_tree()
        check_equal(sorted(after), sorted(self.before), f"{label}: path set unchanged")
        changed = [p for p in after if p in self.before and after[p] != self.before[p]]
        check(not changed, f"{label}: fixtures byte-for-byte unchanged",
              f"changed: {changed}")


# --------------------------------------------------------------------------
# Synthetic fixtures
# --------------------------------------------------------------------------

def make_run(test_id: str, run_id: str, **overrides: object) -> dict:
    """A synthetic registry record shaped like a real completed run."""
    record = {
        "area": "synthetic",
        "claimed_at": "2026-08-21T11:00:00Z",
        "completed_at": "2026-08-21T11:30:00Z",
        "elapsed_seconds": 288.783,
        "execution_status": "passed",
        "interpretation_outcome": "clean",
        "revision": "8f995f395dd1748f67ffcaeedc5cf8d7c2e9e430",
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


def build_test_state(root: Path, runs: list, *,
                     schema: str = evidence.COORDINATOR_SCHEMA) -> Path:
    """Write a synthetic `codex-test` tree and return its root."""
    state = Path(root) / evidence.STATE_DIRNAME
    (state / evidence.REPORTS_DIRNAME).mkdir(parents=True, exist_ok=True)
    (state / "registry.lock").write_text("", encoding="utf-8")
    document = {
        "schema": schema,
        "updated_at": "2026-08-21T11:30:00Z",
        "snapshots": [],
        "proposals": [],
        "runs": runs,
    }
    (state / evidence.REGISTRY_FILENAME).write_text(
        json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return state


def report_source(family: str, entries) -> str:
    """A synthetic findings report: a `## Status` checklist plus headings.

    `entries` are `(number, title, marker, checked)` tuples, with an
    optional fifth element overriding the checklist entry's trailing
    prose. Shaped exactly like the four tracked reports, including the
    ` — ` marker separator and the `### [marker] KEY-N. Title` heading.
    """
    checklist, findings = [], []
    for entry in entries:
        number, title, marker, checked = entry[:4]
        trailer = entry[4] if len(entry) > 4 else ""
        box = "x" if checked else " "
        suffix = f"{inflight.MARKER_SEPARATOR}{marker}{trailer}" if marker else trailer
        checklist.append(f"- [{box}] {family}-{number}. {title}{suffix}")
        prefix = f"{marker} " if marker else ""
        findings.append(
            f"### {prefix}{family}-{number}. {title}\n\n"
            f"Narrative body for {family}-{number}. It may mention the "
            f"transfer_order probe in passing without that counting.\n")
    return ("# Synthetic findings\n\n## Status\n\n" + "\n".join(checklist)
            + "\n\n## Findings\n\n" + "\n".join(findings))


DEFAULT_REPORTS = {
    "NCT": [(1, "Something unrelated", "", False)],
    "CIT": [(1, "Something unrelated", "", False)],
    "PYT": [(1, "Something unrelated", "", False)],
    "CH": [(1, "Something unrelated", "", False)],
}


def build_reports(root: Path, spec: dict | None = None, *,
                  omit: tuple = ()) -> Path:
    """Write the four required reports under `<root>/docs/`."""
    entries = dict(DEFAULT_REPORTS if spec is None else spec)
    docs = Path(root) / "docs"
    docs.mkdir(parents=True, exist_ok=True)
    for relpath, family in inflight.REPORTS:
        if relpath in omit:
            continue
        name = Path(relpath).name
        body = entries.get(family)
        if isinstance(body, str):
            (docs / name).write_text(body, encoding="utf-8")
        else:
            (docs / name).write_text(
                report_source(family, body or DEFAULT_REPORTS[family]),
                encoding="utf-8")
    return Path(root)


def issue(number: int, title: str, *, state: str = "open", **extra) -> dict:
    record = {
        "number": number,
        "title": title,
        "state": state,
        "html_url": f"https://github.com/{REPOSITORY}/issues/{number}",
        "body": "The body mentions the injury_log probe, which must not match.",
    }
    record.update(extra)
    return record


def pull(number: int, title: str, *, state: str = "open", draft: bool = False,
         **extra) -> dict:
    record = {
        "number": number,
        "title": title,
        "state": state,
        "draft": draft,
        "html_url": f"https://github.com/{REPOSITORY}/pull/{number}",
        "body": "The body mentions the injury_log probe, which must not match.",
        "head": {"ref": "issue-1-injury-log-probe"},
    }
    record.update(extra)
    return record


class FakeGitHub:
    """An offline stand-in for `gh api`, paginating like the real endpoints.

    It filters by the requested `state` exactly as the server would, so a
    component that failed to ask for `state=open` — or that scanned the
    unfiltered list — is caught rather than accidentally passing.
    """

    def __init__(self, issues=(), pulls=()) -> None:
        self.data = {ISSUES_PATH: list(issues), PULLS_PATH: list(pulls)}
        self.requests: list[tuple[str, dict]] = []

    def __call__(self, path: str, params: dict) -> list:
        self.requests.append((path, dict(params)))
        entries = self.data.get(path)
        if entries is None:
            raise inflight.SourceError(f"unknown endpoint {path}")
        wanted = params.get("state")
        if wanted and wanted != "all":
            entries = [e for e in entries if e.get("state") == wanted]
        per_page = int(params["per_page"])
        start = (int(params["page"]) - 1) * per_page
        return entries[start:start + per_page]

    def pages_for(self, path: str) -> list[int]:
        return [int(p["page"]) for endpoint, p in self.requests if endpoint == path]


def evaluate(probe: str, *, state_root=None, repo_root=None, docs_root=None,
             github=None, now=NOW, repository=REPOSITORY) -> dict:
    """Drive the shipped entry point with every source fully specified.

    Fully specifying every source is what makes these cases run under
    `NonInteraction`: nothing here needs git, `gh`, or the network.
    """
    return inflight.evaluate_probe_inflight(
        probe, now=now, repo_root=repo_root, state_root=state_root,
        docs_root=docs_root, target_repository=repository,
        github=github if github is not None else FakeGitHub())


def sources_of(document: dict, source: str) -> list[dict]:
    return [m for m in document["matches"] if m["source"] == source]
