#!/usr/bin/env python3
"""Focused self-test for the per-probe in-flight eligibility check (#1433).

Deterministic, engine-free, GPU-free and OFFLINE. Every case runs against
synthetic fixtures in a throwaway temporary directory: a synthetic
`codex-test` registry shaped like the real `codex-test-coordinator/v1`
document, synthetic findings reports in both a checked-out and a
`docs-wip` role, and a fake GitHub transport. Nothing here boots an
engine, runs a registered probe, invokes `gh`, opens a socket, or touches
the developer's real machine-local `$test` state. The real
`tools/probe_inflight.py` is imported and driven, so this exercises the
shipped code paths — and the shipped CLI — rather than a copy.

Two contracts are proved MECHANICALLY rather than inferred from output:

* NON-INTERACTION. Every file under a fixture tree is digested before and
  after each evaluation and must be byte-for-byte identical, path set
  included — registry, reports and lock files alike. `subprocess.run` /
  `Popen`, `fcntl.flock` and `fcntl.lockf` are replaced with tripwires,
  so a coordinator invocation of any subcommand, or any lock, fails the
  test rather than passing quietly. A state root that does not exist is
  checked afterwards to still not exist, so a resolution that CREATED one
  would fail too.

* OFFLINE-NESS. `socket.socket` and `socket.create_connection` are
  tripwires for the whole run, and so is
  `probe_inflight.default_github_transport`. A case that forgot to inject
  a transport therefore FAILS rather than silently skipping or reaching
  the network — which is the difference between the pagination,
  open-versus-closed and draft-versus-merged coverage below meaning
  something and passing vacuously.

The handful of cases that legitimately shell out to `git` (state-root
resolution, `origin` parsing, docs-worktree resolution) build their own
scratch repositories and run outside the subprocess tripwire, but stay
inside the `gh`/socket tripwires.

Usage:
  python3 tools/test_probe_inflight.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import fcntl
import hashlib
import io
import json
import os
import socket
import subprocess
import sys
import tempfile
from contextlib import redirect_stdout, redirect_stderr
from datetime import datetime, timedelta, timezone
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_census  # noqa: E402
import probe_external_evidence as evidence  # noqa: E402
import probe_inflight as inflight  # noqa: E402
import probe_engine  # noqa: E402
import probe_runner_registry  # noqa: E402

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


# ==========================================================================
# Canonical identity
# ==========================================================================

def test_humanized_aliases_all_name_one_probe() -> None:
    """Case- and separator-normalized human forms resolve to one key."""
    index = inflight.build_identity_index()
    for spelling in ("injury_log", "injury-log", "Injury-Log", "INJURY LOG",
                     "injury log probe", "injury_log_probe", "injury_log_probe.py",
                     "Injury-log probe never gates a real fall's event emission",
                     "fix `injury_log_probe.py` at last"):
        check(bool(inflight.subject_matches(spelling, "injury_log", index)),
              f"{spelling!r} names injury_log")
    for spelling in ("injurylog", "injury", "log probe", "the injury of a log",
                     "injury_logging", ""):
        check_equal(inflight.subject_matches(spelling, "injury_log", index), [],
                    f"{spelling!r} does not name injury_log")


def test_prefix_families_stay_distinct() -> None:
    """Longest-registered-identity wins, PER OCCURRENCE, not per subject."""
    index = inflight.build_identity_index()
    families = (("repair", "repair_ai"), ("repair", "repair_item"),
                ("power", "power_workshop"),
                ("persistence_contract", "persistence_contract_sweep"))
    for shorter, longer in families:
        subject = f"the {longer} probe is flaky"
        check(bool(inflight.subject_matches(subject, longer, index)),
              f"{subject!r} names {longer}")
        check_equal(inflight.subject_matches(subject, shorter, index), [],
                    f"a single {longer} mention never also credits {shorter}")

    # The two-mention shape is the ONLY place a per-subject suppression
    # rule and a per-occurrence rule differ, so it is pinned explicitly.
    both = "repair_ai probe regressed after the repair probe changed"
    check_equal(len(inflight.subject_matches(both, "repair_ai", index)), 1,
                "the longer identity matches its own occurrence")
    check_equal(len(inflight.subject_matches(both, "repair", index)), 1,
                "and a separate standalone repair occurrence matches too")
    check_equal(inflight.subject_matches(both, "repair_item", index), [],
                "an unmentioned family member still does not match")

    # Longest-match is positional, so order does not rescue the prefix.
    reversed_order = "the repair probe and then repair_ai probe"
    check_equal(len(inflight.subject_matches(reversed_order, "repair", index)), 1,
                "prefix first still yields exactly one repair occurrence")
    check_equal(len(inflight.subject_matches(reversed_order, "repair_ai", index)), 1,
                "and one repair_ai occurrence")


def test_substring_matching_is_not_used() -> None:
    """A registered key embedded in a longer word is not a match."""
    index = inflight.build_identity_index()
    check_equal(inflight.subject_matches("powerful workshop tooling", "power", index),
                [], "`powerful` does not contain the power probe")
    check_equal(inflight.subject_matches("chopping block", "chop", index), [],
                "`chopping` does not contain the chop probe")
    check(bool(inflight.subject_matches("power probe", "power", index)),
          "the real identity still matches")


def test_a_common_word_key_over_excludes_by_design() -> None:
    """A single-word registered key matches an incidental title mention.

    This is the required direction of error, not a defect: a false
    exclusion costs the selector one skipped candidate, a false clear
    costs an hour of duplicated measurement colliding with live work. It
    is pinned here so it stays a stated contract rather than drifting
    into an accident — and so that any later narrowing has to change a
    test that says out loud what it is giving up.
    """
    index = inflight.build_identity_index()
    title = ("Move the power node role and rating from Haskell into the "
             "building YAML schema")
    for key in ("power", "role"):
        occurrences = inflight.subject_matches(title, key, index)
        check_equal([o["text"] for o in occurrences], [key],
                    f"an incidental {key!r} in a title still matches")
    check_equal(inflight.subject_matches(title, "power_workshop", index), [],
                "and the longer family member still does not")

    with tempfile.TemporaryDirectory() as tmp:
        root = build_reports(Path(tmp) / "repo")
        api = FakeGitHub(issues=[issue(1148, title)])
        document = evaluate("power", repo_root=root, github=api,
                            state_root=Path(tmp) / "none")
        check_equal(document["result"], inflight.RESULT_IN_FLIGHT,
                    "the incidental mention excludes the candidate")
        match = sources_of(document, inflight.SOURCE_ISSUE)[0]
        check_equal(match["matched_text"], ["power"],
                    "and matched_text shows exactly what it matched on, so a "
                    "reader can judge the exclusion")


def test_subject_ambiguity_excludes_and_keeps_its_evidence() -> None:
    """Subject-match ambiguity yields a MATCH, never a source-error.

    Two registered probes sharing one identity form is unlikely but
    representable, and the honest answer is to credit both and retain
    what caused it. That is a different outcome from the
    source-STRUCTURE ambiguity covered further down, and the two must
    never be folded into one path.
    """
    colliding = [("alpha_thing", "alpha_thing_probe.py", "one"),
                 ("alpha", "alpha_thing.py", "two")]
    index = inflight.build_identity_index(colliding)
    occurrences = inflight.find_occurrences("the alpha_thing is unreliable", index)
    check_equal(len(occurrences), 1, "one occurrence at the colliding position")
    check_equal(occurrences[0]["probes"], ["alpha", "alpha_thing"],
                "both owners are credited")
    check(occurrences[0]["ambiguous"], "and the occurrence is marked ambiguous")

    # Where the longest match DOES separate them, it still does.
    unambiguous = inflight.find_occurrences("the alpha_thing probe", index)
    check_equal(unambiguous[0]["probes"], ["alpha_thing"],
                "a longer distinguishing form still resolves cleanly")
    check(not unambiguous[0]["ambiguous"], "and is not marked ambiguous")

    # Through the shipped report source, an ambiguous subject EXCLUDES and
    # keeps its evidence — an `in-flight` match, never a source error.
    with tempfile.TemporaryDirectory() as tmp:
        root = build_reports(Path(tmp), {
            "NCT": [(1, "The alpha_thing is unreliable", "", False)]})
        matches = inflight.evaluate_reports("alpha", index, repo_root=root,
                                            docs_root=None)
        check_equal(len(matches), 1, "an ambiguous subject still matches")
        check(matches[0]["ambiguous"], "the match records the ambiguity")
        check_equal(matches[0]["competing_probes"], ["alpha_thing"],
                    "and names what it was ambiguous with")
        check_equal(matches[0]["source"], inflight.SOURCE_REPORT,
                    "reported as a report match, not as a broken source")


# ==========================================================================
# Source 1: active `$test` runs
# ==========================================================================

def test_both_test_identities_are_the_same_probes_work() -> None:
    """`probe:<key>` and `probe-flake:<key>` each exclude the other."""
    for test_id, kind in (("probe:injury-log", evidence.TEST_KIND_RUN),
                          ("probe-flake:injury-log", evidence.TEST_KIND_FLAKE)):
        with tempfile.TemporaryDirectory() as tmp:
            state = build_test_state(Path(tmp), [
                make_run(test_id, "active-run", status="running",
                         heartbeat_at="2026-08-21T11:58:00Z")])
            root = build_reports(Path(tmp) / "repo")
            with NonInteraction(state, root) as guard:
                document = evaluate("injury_log", state_root=state, repo_root=root)
                guard.assert_untouched(f"{test_id} match")
            check_equal(document["result"], inflight.RESULT_IN_FLIGHT,
                        f"an active {test_id} run excludes the probe")
            match = sources_of(document, inflight.SOURCE_TEST_RUN)[0]
            check_equal(match["evidence"]["run_id"], "active-run",
                        "the evidence carries the run id")
            check_equal(match["evidence"]["test_id"], test_id,
                        "and the exact test id")
            check_equal(match["evidence"]["test_kind"], kind,
                        "and which identity it was recorded under")

    # A near-miss identity is not this probe's work.
    with tempfile.TemporaryDirectory() as tmp:
        state = build_test_state(Path(tmp), [
            make_run("probe-flake:injury_log", "underscored", status="running",
                     heartbeat_at="2026-08-21T11:58:00Z"),
            make_run("probe-flake:injury-log-extra", "prefixed", status="running",
                     heartbeat_at="2026-08-21T11:58:00Z"),
            make_run("probe:role", "another-probe", status="running",
                     heartbeat_at="2026-08-21T11:58:00Z")])
        root = build_reports(Path(tmp) / "repo")
        document = evaluate("injury_log", state_root=state, repo_root=root)
        check_equal(document["result"], inflight.RESULT_CLEAR,
                    "no near-miss identity matches")


def test_every_active_and_terminal_state() -> None:
    """The five active states exclude; the three terminal ones do not."""
    for state_name in inflight.ACTIVE_STATUSES:
        with tempfile.TemporaryDirectory() as tmp:
            state = build_test_state(Path(tmp), [
                make_run("probe:injury-log", "run", status=state_name,
                         heartbeat_at="2026-08-21T11:59:00Z")])
            root = build_reports(Path(tmp) / "repo")
            document = evaluate("injury_log", state_root=state, repo_root=root)
            check_equal(document["result"], inflight.RESULT_IN_FLIGHT,
                        f"{state_name} is an active state")
            check_equal(sources_of(document, inflight.SOURCE_TEST_RUN)[0]
                        ["evidence"]["run_state"], state_name,
                        f"{state_name} is reported as the evidence")

    for state_name in inflight.TERMINAL_STATUSES:
        with tempfile.TemporaryDirectory() as tmp:
            state = build_test_state(Path(tmp), [
                make_run("probe:injury-log", "run", status=state_name,
                         heartbeat_at="2026-08-21T11:59:00Z")])
            root = build_reports(Path(tmp) / "repo")
            document = evaluate("injury_log", state_root=state, repo_root=root)
            check_equal(document["result"], inflight.RESULT_CLEAR,
                        f"{state_name} is terminal and does not exclude")


def test_fresh_stale_and_missing_timestamps() -> None:
    """The six-hour horizon, its fallback, and every unusable stamp."""
    horizon = inflight.STALE_HORIZON

    def stamp(delta: timedelta) -> str:
        return (NOW - delta).strftime("%Y-%m-%dT%H:%M:%SZ")

    cases = [
        ("fresh heartbeat", {"heartbeat_at": stamp(timedelta(minutes=1))},
         inflight.RESULT_IN_FLIGHT),
        ("heartbeat just inside the horizon",
         {"heartbeat_at": stamp(horizon - timedelta(seconds=1))},
         inflight.RESULT_IN_FLIGHT),
        ("heartbeat exactly on the horizon", {"heartbeat_at": stamp(horizon)},
         inflight.RESULT_IN_FLIGHT),
        ("heartbeat past the horizon",
         {"heartbeat_at": stamp(horizon + timedelta(seconds=1))},
         inflight.RESULT_CLEAR),
        ("fresh claim, no heartbeat",
         {"claimed_at": stamp(timedelta(minutes=5)), "heartbeat_at": None},
         inflight.RESULT_IN_FLIGHT),
        ("stale claim, no heartbeat",
         {"claimed_at": stamp(horizon + timedelta(hours=1)), "heartbeat_at": None},
         inflight.RESULT_CLEAR),
        ("a stale heartbeat is NOT rescued by a fresh claim",
         {"heartbeat_at": stamp(horizon + timedelta(hours=1)),
          "claimed_at": stamp(timedelta(minutes=1))},
         inflight.RESULT_CLEAR),
    ]
    for label, overrides, expected in cases:
        with tempfile.TemporaryDirectory() as tmp:
            record = make_run("probe:injury-log", "run", status="running",
                              **overrides)
            record = {k: v for k, v in record.items() if v is not None}
            state = build_test_state(Path(tmp), [record])
            root = build_reports(Path(tmp) / "repo")
            with NonInteraction(state) as guard:
                document = evaluate("injury_log", state_root=state, repo_root=root)
                guard.assert_untouched(f"{label}: stale records are never rewritten")
            check_equal(document["result"], expected, label)

    failures = [
        ("no timestamps at all",
         {"heartbeat_at": None, "claimed_at": None},
         "records neither heartbeat_at nor claimed_at"),
        ("a malformed heartbeat", {"heartbeat_at": "not-a-date"},
         "not a timezone-qualified"),
        ("a non-string heartbeat", {"heartbeat_at": 1755777600},
         "value is unusable"),
        ("a naive heartbeat", {"heartbeat_at": "2026-08-21T11:59:00"},
         "carries no timezone"),
        ("a malformed claim with no heartbeat",
         {"heartbeat_at": None, "claimed_at": "yesterday"},
         "not a timezone-qualified"),
        ("a non-string claim with no heartbeat",
         {"heartbeat_at": None, "claimed_at": []},
         "value is unusable"),
    ]
    for label, overrides, fragment in failures:
        with tempfile.TemporaryDirectory() as tmp:
            record = make_run("probe:injury-log", "run", status="running",
                              **overrides)
            record = {k: v for k, v in record.items() if v is not None}
            state = build_test_state(Path(tmp), [record])
            root = build_reports(Path(tmp) / "repo")
            document = evaluate("injury_log", state_root=state, repo_root=root)
            check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                        f"{label} fails closed")
            detail = " ".join(e["detail"] for e in document["source_errors"])
            check(fragment in detail, f"{label} is diagnosed actionably", detail)
            check_equal([e["source"] for e in document["source_errors"]],
                        [inflight.SOURCE_TEST_RUN],
                        f"{label} fails only the $test source")

    # An ABANDONED record is terminal: it is inactive whatever its age.
    with tempfile.TemporaryDirectory() as tmp:
        state = build_test_state(Path(tmp), [
            make_run("probe:injury-log", "run", status="abandoned",
                     heartbeat_at=stamp(timedelta(minutes=1)))])
        root = build_reports(Path(tmp) / "repo")
        document = evaluate("injury_log", state_root=state, repo_root=root)
        check_equal(document["result"], inflight.RESULT_CLEAR,
                    "a freshly abandoned run does not exclude")


def test_unknown_state_fails_closed() -> None:
    """A state that is neither active nor terminal is never guessed."""
    for state_name in ("paused", "", "RUNNING", "unknown-future-state"):
        with tempfile.TemporaryDirectory() as tmp:
            state = build_test_state(Path(tmp), [
                make_run("probe:injury-log", "odd", status=state_name,
                         heartbeat_at="2026-08-21T11:59:00Z")])
            root = build_reports(Path(tmp) / "repo")
            document = evaluate("injury_log", state_root=state, repo_root=root)
            check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                        f"state {state_name!r} fails closed")
            detail = document["source_errors"][0]["detail"]
            check("neither an active state" in detail,
                  f"state {state_name!r} is diagnosed", detail)

    # A record with NO status at all is the same refusal.
    with tempfile.TemporaryDirectory() as tmp:
        state = build_test_state(Path(tmp), [
            make_run("probe:injury-log", "stateless", status=None)])
        root = build_reports(Path(tmp) / "repo")
        document = evaluate("injury_log", state_root=state, repo_root=root)
        check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                    "a stateless record fails closed")


def test_absent_versus_damaged_test_state() -> None:
    """An absent state tree is normal; a damaged registry is not."""
    with tempfile.TemporaryDirectory() as tmp:
        missing = Path(tmp) / "codex-test"
        root = build_reports(Path(tmp) / "repo")
        with NonInteraction(missing, root) as guard:
            document = evaluate("injury_log", state_root=missing, repo_root=root)
            guard.assert_untouched("absent state")
        check_equal(document["result"], inflight.RESULT_CLEAR,
                    "an absent $test tree is a normal no-evidence state")
        check(not missing.exists(),
              "and resolving it creates nothing")
        check_equal(document["sources"][inflight.SOURCE_TEST_RUN], "read",
                    "the source still counts as read")

    for label, damage in (
            ("unparseable registry", lambda s: (s / "registry.json").write_text("{")),
            ("registry that is not an object",
             lambda s: (s / "registry.json").write_text('"a string"')),
            ("registry with no runs list",
             lambda s: (s / "registry.json").write_text('{"schema": "x"}')),
            ("deleted registry", lambda s: (s / "registry.json").unlink())):
        with tempfile.TemporaryDirectory() as tmp:
            state = build_test_state(Path(tmp), [])
            damage(state)
            root = build_reports(Path(tmp) / "repo")
            document = evaluate("injury_log", state_root=state, repo_root=root)
            check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                        f"a {label} fails closed")
            check_equal(document["sources"][inflight.SOURCE_TEST_RUN], "error",
                        f"a {label} marks its source in error")

    # A malformed RECORD is active-run state too.
    with tempfile.TemporaryDirectory() as tmp:
        state = build_test_state(Path(tmp), ["not-an-object"])
        root = build_reports(Path(tmp) / "repo")
        document = evaluate("injury_log", state_root=state, repo_root=root)
        check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                    "a non-object run record fails closed")

    # A state root that EXISTS but is not a state tree is damage, not
    # the normal "Codex is not installed here" absence, and it must not
    # be able to answer `clear` — the kind predicates would have called
    # every one of these absent.
    for label, build in (
            ("a regular file", lambda p: p.write_text("not a state tree")),
            ("a dangling symlink", lambda p: p.symlink_to(p.parent / "gone"))):
        with tempfile.TemporaryDirectory() as tmp:
            root = build_reports(Path(tmp) / "repo")
            state = Path(tmp) / evidence.STATE_DIRNAME
            build(state)
            document = evaluate("injury_log", state_root=state, repo_root=root)
            check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                        f"{label} at the $test state root fails closed")
            check_equal(document["sources"][inflight.SOURCE_TEST_RUN], "error",
                        f"{label} marks the $test source in error")
            check("is not a directory" in document["source_errors"][0]["detail"],
                  f"{label} is diagnosed actionably",
                  document["source_errors"][0]["detail"])

    with tempfile.TemporaryDirectory() as tmp:
        root = build_reports(Path(tmp) / "repo")
        blocker = Path(tmp) / "blocker"
        blocker.write_text("a file where a directory belongs", encoding="utf-8")
        document = evaluate("injury_log", state_root=blocker / "nested" / "codex-test",
                            repo_root=root)
        check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                    "an unstattable $test state root fails closed")
        check("could not be read" in document["source_errors"][0]["detail"],
              "and is diagnosed", document["source_errors"][0]["detail"])

    # A record whose IDENTITY cannot be read has indeterminate probe
    # ownership. It used to either crash the read outright (an unhashable
    # value) or vanish silently (a missing one) and let the scan answer
    # `clear` beside an active run nobody could attribute.
    for label, value in (("an unhashable list", []), ("an absent", MISSING),
                         ("a null", None), ("an empty", ""), ("a numeric", 17)):
        with tempfile.TemporaryDirectory() as tmp:
            state = build_test_state(Path(tmp), [
                run_with_identity("unattributable", value, status="running",
                                  heartbeat_at="2026-08-21T11:59:00Z")])
            root = build_reports(Path(tmp) / "repo")
            document = evaluate("injury_log", state_root=state, repo_root=root)
            check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                        f"{label} test_id fails closed rather than clearing")
            check_equal(document["sources"][inflight.SOURCE_TEST_RUN], "error",
                        f"{label} test_id marks the $test source in error")
            check("no usable test_id" in document["source_errors"][0]["detail"],
                  f"{label} test_id is diagnosed actionably",
                  document["source_errors"][0]["detail"])

    # A damaged REPORT belongs to a finished run's interpretation, not to
    # active-run state, and must not fail the scan.
    with tempfile.TemporaryDirectory() as tmp:
        state = build_test_state(Path(tmp), [
            make_run("probe:injury-log", "done",
                     report_path=str(Path(tmp) / "elsewhere.test-result.md"))])
        root = build_reports(Path(tmp) / "repo")
        document = evaluate("injury_log", state_root=state, repo_root=root)
        check_equal(document["result"], inflight.RESULT_CLEAR,
                    "an out-of-scope $test REPORT does not fail the scan")


# ==========================================================================
# Sources 2 and 3: issues and pull requests
# ==========================================================================

def test_open_issues_match_titles_only() -> None:
    """An open issue's TITLE excludes; its body never does."""
    with tempfile.TemporaryDirectory() as tmp:
        root = build_reports(Path(tmp) / "repo")
        api = FakeGitHub(issues=[
            issue(10, "Unrelated worldgen work"),
            issue(11, "Injury-log probe accepts the wrong unit"),
            issue(12, "Closed injury_log probe work", state="closed"),
        ])
        with NonInteraction(root) as guard:
            document = evaluate("injury_log", repo_root=root, github=api,
                                state_root=Path(tmp) / "none")
            guard.assert_untouched("issue scan")
        check_equal(document["result"], inflight.RESULT_IN_FLIGHT, "an open issue excludes")
        matches = sources_of(document, inflight.SOURCE_ISSUE)
        check_equal(len(matches), 1, "the closed issue does not match")
        check_equal(matches[0]["evidence"], {
            "number": 11,
            "title": "Injury-log probe accepts the wrong unit",
            "url": f"https://github.com/{REPOSITORY}/issues/11",
            "repository": REPOSITORY,
        }, "the evidence is number, title, url and repository")
        check_equal(matches[0]["ambiguous"], False, "an exact title is unambiguous")
        check_equal([p[1]["state"] for p in api.requests], ["open"] * len(api.requests),
                    "every request asks for open items only")

    # A body-only or branch-only mention is NOT the subject.
    with tempfile.TemporaryDirectory() as tmp:
        root = build_reports(Path(tmp) / "repo")
        api = FakeGitHub(issues=[issue(20, "Rework the wander hazard course")])
        document = evaluate("injury_log", repo_root=root, github=api,
                            state_root=Path(tmp) / "none")
        check_equal(document["result"], inflight.RESULT_CLEAR,
                    "a body-only mention does not exclude")

    # Pull requests arriving on the issues endpoint are counted once, by
    # the pull-request scan.
    with tempfile.TemporaryDirectory() as tmp:
        root = build_reports(Path(tmp) / "repo")
        pr_shaped = issue(30, "Fix the injury_log probe")
        pr_shaped["pull_request"] = {"url": "..."}
        api = FakeGitHub(issues=[pr_shaped],
                         pulls=[pull(30, "Fix the injury_log probe")])
        document = evaluate("injury_log", repo_root=root, github=api,
                            state_root=Path(tmp) / "none")
        check_equal(len(sources_of(document, inflight.SOURCE_ISSUE)), 0,
                    "a pull request on the issues endpoint is not an issue match")
        check_equal(len(sources_of(document, inflight.SOURCE_PULL_REQUEST)), 1,
                    "it is counted exactly once, as a pull request")


def test_open_draft_and_merged_pull_requests() -> None:
    """Drafts count as open; closed and merged pull requests do not."""
    with tempfile.TemporaryDirectory() as tmp:
        root = build_reports(Path(tmp) / "repo")
        api = FakeGitHub(pulls=[
            pull(40, "Draft: injury_log probe target attribution", draft=True),
            pull(41, "Merged injury-log probe fix", state="closed",
                 merged_at="2026-08-01T00:00:00Z"),
            pull(42, "Closed injury_log_probe.py attempt", state="closed"),
            pull(43, "Unrelated hydrology change"),
        ])
        document = evaluate("injury_log", repo_root=root, github=api,
                            state_root=Path(tmp) / "none")
        matches = sources_of(document, inflight.SOURCE_PULL_REQUEST)
        check_equal([m["evidence"]["number"] for m in matches], [40],
                    "only the open draft matches")
        check_equal(matches[0]["evidence"], {
            "number": 40,
            "title": "Draft: injury_log probe target attribution",
            "url": f"https://github.com/{REPOSITORY}/pull/40",
            "repository": REPOSITORY,
            "draft": True,
        }, "the pull-request evidence records its draft status")
        check_equal(document["result"], inflight.RESULT_IN_FLIGHT,
                    "an open draft excludes the probe")


def test_a_malformed_tracker_record_fails_closed() -> None:
    """A record whose subject cannot be read is not a non-match.

    `normalize_tokens` answers "no tokens" for a missing or non-string
    title, which is indistinguishable from a genuine non-match — so a
    page containing one used to sail through and let the scan report
    `clear`, despite the required subject being uninterpretable.
    """
    damaged_titles = [("an absent", MISSING), ("a null", None),
                      ("an empty", ""), ("a whitespace", "   "),
                      ("a numeric", 17), ("a list", ["x"])]
    for label, value in damaged_titles:
        for kind, source in (("issue", inflight.SOURCE_ISSUE),
                             ("pull", inflight.SOURCE_PULL_REQUEST)):
            with tempfile.TemporaryDirectory() as tmp:
                root = build_reports(Path(tmp) / "repo")
                record = (issue(50, "placeholder") if kind == "issue"
                          else pull(50, "placeholder"))
                if value is MISSING:
                    record.pop("title", None)
                else:
                    record["title"] = value
                api = FakeGitHub(**{kind + "s": [record]})
                document = evaluate("injury_log", repo_root=root, github=api,
                                    state_root=Path(tmp) / "none")
                check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                            f"{label} {kind} title fails closed")
                check_equal([e["source"] for e in document["source_errors"]],
                            [source], f"{label} {kind} title fails its own source")
                check("no usable title" in document["source_errors"][0]["detail"],
                      f"{label} {kind} title is diagnosed actionably",
                      document["source_errors"][0]["detail"])

    # The number and the state are validated for the same reason: one
    # makes the evidence inspectable, the other decides eligibility.
    for field, value, fragment in (("number", MISSING, "no usable number"),
                                   ("number", "50", "no usable number"),
                                   ("number", True, "no usable number"),
                                   ("state", MISSING, "no usable state"),
                                   ("state", 1, "no usable state"),
                                   ("state", "", "no usable state")):
        with tempfile.TemporaryDirectory() as tmp:
            root = build_reports(Path(tmp) / "repo")
            record = issue(50, "Injury-log probe accepts the wrong unit")
            if value is MISSING:
                record.pop(field, None)
            else:
                record[field] = value
            # A raw transport, not `FakeGitHub`: that fake filters by
            # `state` the way the server does, so it would drop a record
            # with a damaged `state` before the component ever saw it —
            # and the component's own guard is exactly what is under
            # test here.
            def raw(path, params, _record=record):
                return [_record] if int(params["page"]) == 1 else []

            document = evaluate("injury_log", repo_root=root, github=raw,
                                state_root=Path(tmp) / "none")
            check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                        f"a {value!r} {field} fails closed")
            check(fragment in document["source_errors"][0]["detail"],
                  f"a {value!r} {field} is diagnosed actionably",
                  document["source_errors"][0]["detail"])

    # A transport that ignores `state=open` must not let a closed item
    # exclude the probe: the returned state is checked, not assumed.
    with tempfile.TemporaryDirectory() as tmp:
        root = build_reports(Path(tmp) / "repo")

        def unfiltered(path, params):
            if int(params["page"]) > 1:
                return []
            return [issue(60, "Injury-log probe accepts the wrong unit",
                          state="closed")]

        document = evaluate("injury_log", repo_root=root, github=unfiltered,
                            state_root=Path(tmp) / "none")
        check_equal(document["result"], inflight.RESULT_CLEAR,
                    "a closed item returned anyway is still not open")


def test_every_page_is_retrieved() -> None:
    """Pagination walks to the end; a match on the last page is found."""
    with tempfile.TemporaryDirectory() as tmp:
        root = build_reports(Path(tmp) / "repo")
        issues = [issue(n, f"Filler issue {n}") for n in range(1, 251)]
        issues.append(issue(999, "Injury-log probe never gates a real fall"))
        pulls = [pull(n, f"Filler pull {n}") for n in range(1, 101)]
        pulls.append(pull(998, "injury_log_probe.py deserves a rewrite"))
        api = FakeGitHub(issues=issues, pulls=pulls)
        document = evaluate("injury_log", repo_root=root, github=api,
                            state_root=Path(tmp) / "none")
        check_equal(api.pages_for(ISSUES_PATH), [1, 2, 3],
                    "251 issues take three pages")
        check_equal(api.pages_for(PULLS_PATH), [1, 2],
                    "101 pulls take two pages, the second short")
        check_equal([m["evidence"]["number"]
                     for m in sources_of(document, inflight.SOURCE_ISSUE)], [999],
                    "a match on the LAST page is still found")
        check_equal([m["evidence"]["number"]
                     for m in sources_of(document, inflight.SOURCE_PULL_REQUEST)], [998],
                    "and so is one on the last pull-request page")
        check(all(int(p["per_page"]) == inflight.PER_PAGE for _e, p in api.requests),
              "every request asks for a full page")

    # An exactly-full final page still ends the walk with one more probe.
    with tempfile.TemporaryDirectory() as tmp:
        root = build_reports(Path(tmp) / "repo")
        api = FakeGitHub(issues=[issue(n, f"Filler {n}") for n in range(1, 101)])
        evaluate("injury_log", repo_root=root, github=api,
                 state_root=Path(tmp) / "none")
        check_equal(api.pages_for(ISSUES_PATH), [1, 2],
                    "an exactly-full page is followed by an empty confirming page")


def test_a_failing_or_endless_list_fails_closed() -> None:
    """Any page that cannot be retrieved or interpreted is a source error."""
    with tempfile.TemporaryDirectory() as tmp:
        root = build_reports(Path(tmp) / "repo")

        def broken(path, params):
            if path == PULLS_PATH:
                raise inflight.SourceError("gh api failed: HTTP 502")
            return []

        document = evaluate("injury_log", repo_root=root, github=broken,
                            state_root=Path(tmp) / "none")
        check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                    "an unretrievable pull-request page fails closed")
        check_equal([e["source"] for e in document["source_errors"]],
                    [inflight.SOURCE_PULL_REQUEST],
                    "and only that source is in error")
        check_equal(document["sources"][inflight.SOURCE_ISSUE], "read",
                    "the issue source was still read completely")

    with tempfile.TemporaryDirectory() as tmp:
        root = build_reports(Path(tmp) / "repo")
        document = evaluate("injury_log", repo_root=root,
                            github=lambda path, params: {"message": "Not Found"},
                            state_root=Path(tmp) / "none")
        check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                    "a non-list page fails closed")

    with tempfile.TemporaryDirectory() as tmp:
        root = build_reports(Path(tmp) / "repo")
        document = evaluate(
            "injury_log", repo_root=root,
            github=lambda path, params: [issue(1, "x")] * inflight.PER_PAGE,
            state_root=Path(tmp) / "none")
        check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                    "a list that never shortens fails closed rather than truncating")
        check("truncated" in document["source_errors"][0]["detail"],
              "and says so", document["source_errors"][0]["detail"])

    with tempfile.TemporaryDirectory() as tmp:
        root = build_reports(Path(tmp) / "repo")
        document = evaluate("injury_log", repo_root=root,
                            github=lambda path, params: ["not-an-object"],
                            state_root=Path(tmp) / "none")
        check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                    "a non-object entry fails closed")


def test_target_repository_resolution() -> None:
    """`origin` is the one definition, and a bad one is a source error."""
    for url, expected in (
            ("git@github.com:coghex/synarchy.git", "coghex/synarchy"),
            ("git@github.com:coghex/synarchy", "coghex/synarchy"),
            ("https://github.com/coghex/synarchy.git", "coghex/synarchy"),
            ("https://github.com/coghex/synarchy", "coghex/synarchy"),
            ("ssh://git@github.com/coghex/synarchy.git", "coghex/synarchy"),
            ("ssh://git@github.com:22/coghex/synarchy.git", "coghex/synarchy"),
            ("https://user:token@github.com/coghex/synarchy.git", "coghex/synarchy"),
            ("git@GitHub.com:coghex/synarchy.git", "coghex/synarchy")):
        check_equal(inflight.parse_github_remote(url), expected,
                    f"{url} resolves to {expected}")
    for url in ("git@gitlab.com:coghex/synarchy.git",
                "https://example.com/coghex/synarchy.git",
                "/srv/git/synarchy.git", "https://github.com/coghex",
                "https://github.com/coghex/synarchy/extra", "", None, 17):
        check_equal(inflight.parse_github_remote(url), None,
                    f"{url!r} names no GitHub repository")

    # Against a REAL scratch repository, through the shipped resolver.
    with tempfile.TemporaryDirectory() as tmp:
        repo = Path(tmp) / "scratch"
        repo.mkdir()
        subprocess.run(["git", "init", "-q", str(repo)], check=True,
                       capture_output=True)
        try:
            inflight.resolve_target_repository(repo)
            check(False, "a repository with no origin is a source error")
        except inflight.SourceError as exc:
            check("origin" in str(exc), "the diagnostic names the remote", str(exc))
            check("undefined" in str(exc),
                  "and says the target repository is undefined", str(exc))

        subprocess.run(["git", "-C", str(repo), "remote", "add", "origin",
                        "git@gitlab.com:someone/thing.git"], check=True,
                       capture_output=True)
        try:
            inflight.resolve_target_repository(repo)
            check(False, "a non-GitHub origin is a source error")
        except inflight.SourceError as exc:
            check("gitlab.com" in str(exc),
                  "the diagnostic quotes the offending remote", str(exc))

        subprocess.run(["git", "-C", str(repo), "remote", "set-url", "origin",
                        "git@github.com:coghex/synarchy.git"], check=True,
                       capture_output=True)
        check_equal(inflight.resolve_target_repository(repo), REPOSITORY,
                    "a GitHub origin resolves to owner/name")

    # An unresolvable target fails BOTH tracker sources, and neither reads
    # as skipped.
    with tempfile.TemporaryDirectory() as tmp:
        repo = Path(tmp) / "scratch"
        build_reports(repo)
        subprocess.run(["git", "init", "-q", str(repo)], check=True,
                       capture_output=True)
        document = inflight.evaluate_probe_inflight(
            "injury_log", now=NOW, repo_root=repo, state_root=Path(tmp) / "none",
            docs_root=None, github=FakeGitHub())
        check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                    "an unresolvable origin fails closed")
        check_equal(sorted(e["source"] for e in document["source_errors"]),
                    sorted([inflight.SOURCE_ISSUE, inflight.SOURCE_PULL_REQUEST]),
                    "both tracker sources report the failure")
        check_equal(document["target_repository"], None,
                    "and no target repository is claimed")


# ==========================================================================
# Source 4: the findings reports
# ==========================================================================

def test_all_four_report_families() -> None:
    """Each report is parsed with its own native finding-key family."""
    for relpath, family in inflight.REPORTS:
        with tempfile.TemporaryDirectory() as tmp:
            spec = dict(DEFAULT_REPORTS)
            spec[family] = [(6, "Injury-log probe accepts the wrong unit",
                             "", False)]
            root = build_reports(Path(tmp), spec)
            with NonInteraction(root) as guard:
                document = evaluate("injury_log", repo_root=root,
                                    state_root=Path(tmp) / "none")
                guard.assert_untouched(f"{family} scan")
            matches = sources_of(document, inflight.SOURCE_REPORT)
            check_equal(len(matches), 1, f"the open {family} finding matches")
            check_equal(matches[0]["evidence"], {
                "worktree": inflight.WORKTREE_CHECKOUT,
                "worktree_path": str(root),
                "report_path": relpath,
                "finding_key": f"{family}-6",
                "heading": f"### {family}-6. Injury-log probe accepts the wrong unit",
                "line": matches[0]["evidence"]["line"],
            }, f"the {family} evidence names its worktree, path, key and heading")


def test_every_heading_state() -> None:
    """The heading marker is authoritative for whether a finding is open."""
    open_states = [
        ("bare", "", False),
    ]
    closed_states = [
        ("filed", "[#1234]", True),
        ("annotated", "[#936, closed obsolete]", True),
        ("no-issue", "[no-issue]", True),
        ("deferred", "[deferred]", True),
        ("deferred but still unchecked", "[deferred]", False),
    ]
    for label, marker, checked in open_states + closed_states:
        with tempfile.TemporaryDirectory() as tmp:
            spec = dict(DEFAULT_REPORTS)
            spec["NCT"] = [(6, "Injury-log probe accepts the wrong unit",
                            marker, checked)]
            root = build_reports(Path(tmp), spec)
            document = evaluate("injury_log", repo_root=root,
                                state_root=Path(tmp) / "none")
            expected = (inflight.RESULT_IN_FLIGHT if not marker
                        else inflight.RESULT_CLEAR)
            check_equal(document["result"], expected,
                        f"a {label} heading is "
                        f"{'open' if not marker else 'dispositioned'}")

    # Trailing prose after a marker is ignored, exactly as the audit does.
    with tempfile.TemporaryDirectory() as tmp:
        spec = dict(DEFAULT_REPORTS)
        spec["NCT"] = [(6, "Injury-log probe accepts the wrong unit",
                        "[deferred]", False, ": awaits #1153's build-only record")]
        root = build_reports(Path(tmp), spec)
        document = evaluate("injury_log", repo_root=root,
                            state_root=Path(tmp) / "none")
        check_equal(document["result"], inflight.RESULT_CLEAR,
                    "trailing prose after a marker is ignored")


def test_broken_report_states_fail_closed() -> None:
    """Partial, duplicate, inconsistent and unlexable states never clear.

    This is source-STRUCTURE ambiguity, and its outcome is
    `source-error` — distinct from the subject-match ambiguity that
    yields `in-flight`.
    """
    heading = "Injury-log probe accepts the wrong unit"
    broken = {
        "checklist entry with no heading":
            "# R\n\n## Status\n\n- [ ] NCT-6. " + heading + "\n",
        "heading with no checklist entry":
            "# R\n\n## Status\n\n## Findings\n\n### NCT-6. " + heading + "\n",
        "duplicate checklist entry":
            "# R\n\n## Status\n\n- [ ] NCT-6. " + heading
            + "\n- [ ] NCT-6. " + heading
            + "\n\n### NCT-6. " + heading + "\n",
        "duplicate heading":
            "# R\n\n## Status\n\n- [ ] NCT-6. " + heading
            + "\n\n### NCT-6. " + heading
            + "\n\n### NCT-6. " + heading + "\n",
        "markers that disagree":
            "# R\n\n## Status\n\n- [x] NCT-6. " + heading
            + inflight.MARKER_SEPARATOR + "[#12]"
            + "\n\n### NCT-6. " + heading + "\n",
        "a checked but unmarked entry":
            "# R\n\n## Status\n\n- [x] NCT-6. " + heading
            + "\n\n### NCT-6. " + heading + "\n",
        "an unlexable checklist line":
            "# R\n\n## Status\n\n- [ ] not a finding entry\n"
            + "- [ ] NCT-6. " + heading + "\n\n### NCT-6. " + heading + "\n",
        "an unlexable heading":
            "# R\n\n## Status\n\n- [ ] NCT-6. " + heading
            + "\n\n### Some other section\n\n### NCT-6. " + heading + "\n",
        "a marker outside marker position":
            "# R\n\n## Status\n\n- [ ] NCT-6. [#12] " + heading
            + "\n\n### NCT-6. " + heading + "\n",
        "a foreign finding-key family in the checklist":
            "# R\n\n## Status\n\n- [ ] CH-6. " + heading
            + "\n\n### NCT-6. " + heading + "\n",
        "a foreign finding-key family in a heading":
            "# R\n\n## Status\n\n- [ ] NCT-6. " + heading
            + "\n\n### CH-6. " + heading + "\n",
    }
    for label, text in broken.items():
        with tempfile.TemporaryDirectory() as tmp:
            spec = dict(DEFAULT_REPORTS)
            spec["NCT"] = text
            root = build_reports(Path(tmp), spec)
            with NonInteraction(root) as guard:
                document = evaluate("injury_log", repo_root=root,
                                    state_root=Path(tmp) / "none")
                guard.assert_untouched(f"{label}: nothing is repaired")
            check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                        f"{label} fails closed")
            check_equal([e["source"] for e in document["source_errors"]],
                        [inflight.SOURCE_REPORT],
                        f"{label} fails only the report source")
            detail = document["source_errors"][0]["detail"]
            check("non_ci_test_audit_findings.md" in detail,
                  f"{label} names the offending report", detail)

    # A missing REQUIRED report is a source error too.
    with tempfile.TemporaryDirectory() as tmp:
        root = build_reports(Path(tmp), omit=("docs/python_testing_findings.md",))
        document = evaluate("injury_log", repo_root=root,
                            state_root=Path(tmp) / "none")
        check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                    "an absent required report fails closed")
        check("python_testing_findings.md"
              in document["source_errors"][0]["detail"],
              "and names it")


def test_narrative_mentions_are_not_the_subject() -> None:
    """Only the finding HEADING is the subject; the body never is."""
    with tempfile.TemporaryDirectory() as tmp:
        spec = dict(DEFAULT_REPORTS)
        # `report_source` always writes a body naming transfer_order.
        spec["NCT"] = [(6, "A worldgen determinism gap", "", False)]
        root = build_reports(Path(tmp), spec)
        document = evaluate("transfer_order", repo_root=root,
                            state_root=Path(tmp) / "none")
        check_equal(document["result"], inflight.RESULT_CLEAR,
                    "a narrative body mention does not exclude")
        body = (root / "docs" / "non_ci_test_audit_findings.md").read_text()
        check("transfer_order" in body,
              "the fixture really does mention it in the body")


def test_both_report_worktrees() -> None:
    """Checked-out-only, docs-wip-only and both-sides evidence."""
    matching = [(6, "Injury-log probe accepts the wrong unit", "", False)]
    resolved = [(6, "Injury-log probe accepts the wrong unit", "[#1234]", True)]

    with tempfile.TemporaryDirectory() as tmp:
        checkout = build_reports(Path(tmp) / "checkout",
                                 {**DEFAULT_REPORTS, "NCT": matching})
        docs = build_reports(Path(tmp) / "docs-wip",
                             {**DEFAULT_REPORTS, "NCT": resolved})
        document = evaluate("injury_log", repo_root=checkout, docs_root=docs,
                            state_root=Path(tmp) / "none")
        matches = sources_of(document, inflight.SOURCE_REPORT)
        check_equal([m["evidence"]["worktree"] for m in matches],
                    [inflight.WORKTREE_CHECKOUT],
                    "a checked-out-only open finding excludes")

    with tempfile.TemporaryDirectory() as tmp:
        checkout = build_reports(Path(tmp) / "checkout",
                                 {**DEFAULT_REPORTS, "NCT": resolved})
        docs = build_reports(Path(tmp) / "docs-wip",
                             {**DEFAULT_REPORTS, "NCT": matching})
        document = evaluate("injury_log", repo_root=checkout, docs_root=docs,
                            state_root=Path(tmp) / "none")
        matches = sources_of(document, inflight.SOURCE_REPORT)
        check_equal([m["evidence"]["worktree"] for m in matches],
                    [inflight.WORKTREE_DOCS],
                    "a docs-wip-only open finding excludes just as conservatively")
        check_equal(matches[0]["evidence"]["worktree_path"], str(docs),
                    "and the evidence names the docs worktree it came from")

    with tempfile.TemporaryDirectory() as tmp:
        checkout = build_reports(Path(tmp) / "checkout",
                                 {**DEFAULT_REPORTS, "NCT": matching})
        docs = build_reports(Path(tmp) / "docs-wip",
                             {**DEFAULT_REPORTS, "NCT": matching})
        with NonInteraction(checkout, docs) as guard:
            document = evaluate("injury_log", repo_root=checkout, docs_root=docs,
                                state_root=Path(tmp) / "none")
            guard.assert_untouched("both worktrees scanned")
        matches = sources_of(document, inflight.SOURCE_REPORT)
        check_equal([m["evidence"]["worktree"] for m in matches],
                    [inflight.WORKTREE_CHECKOUT, inflight.WORKTREE_DOCS],
                    "a finding open in both is reported once per worktree")


def test_docs_worktree_absence_is_normal_but_damage_is_not() -> None:
    """An absent docs worktree is no-evidence; a broken one is not."""
    with tempfile.TemporaryDirectory() as tmp:
        checkout = build_reports(Path(tmp) / "checkout")
        document = evaluate("injury_log", repo_root=checkout, docs_root=None,
                            state_root=Path(tmp) / "none")
        check_equal(document["result"], inflight.RESULT_CLEAR,
                    "an absent docs-wip worktree is a normal no-evidence state")
        check_equal(document["sources"][inflight.SOURCE_REPORT], "read",
                    "and the report source still counts as read")

    # A resolved worktree that simply lacks one of the four reports is
    # no-evidence for that path, NOT an error.
    with tempfile.TemporaryDirectory() as tmp:
        checkout = build_reports(Path(tmp) / "checkout")
        docs = build_reports(Path(tmp) / "docs-wip",
                             omit=("docs/code_health_findings.md",
                                   "docs/python_testing_findings.md"))
        document = evaluate("injury_log", repo_root=checkout, docs_root=docs,
                            state_root=Path(tmp) / "none")
        check_equal(document["result"], inflight.RESULT_CLEAR,
                    "a docs-wip worktree missing a report is no-evidence")

    # But a docs-wip report that IS present and unparseable is an error.
    with tempfile.TemporaryDirectory() as tmp:
        checkout = build_reports(Path(tmp) / "checkout")
        docs = build_reports(Path(tmp) / "docs-wip", {
            **DEFAULT_REPORTS,
            "CH": "# R\n\n## Status\n\n- [ ] CH-1. Title\n"})
        document = evaluate("injury_log", repo_root=checkout, docs_root=docs,
                            state_root=Path(tmp) / "none")
        check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                    "a present-but-broken docs-wip report fails closed")
        check("docs-wip" in document["source_errors"][0]["detail"],
              "and says which worktree it came from",
              document["source_errors"][0]["detail"])

    # A report path that is THERE but not a readable regular file is
    # damage, not absence, and damage fails closed in BOTH scopes —
    # `is_file()` alone would read every one of these as "absent".
    for role, damage in (("docs-wip", "mkdir"), ("docs-wip", "broken-symlink"),
                         ("checkout", "mkdir"), ("checkout", "broken-symlink")):
        with tempfile.TemporaryDirectory() as tmp:
            checkout = build_reports(Path(tmp) / "checkout")
            docs = build_reports(Path(tmp) / "docs-wip")
            target = (docs if role == "docs-wip" else checkout) / \
                "docs" / "code_health_findings.md"
            target.unlink()
            if damage == "mkdir":
                target.mkdir()
            else:
                target.symlink_to(Path(tmp) / "nowhere.md")
            document = evaluate("injury_log", repo_root=checkout, docs_root=docs,
                                state_root=Path(tmp) / "none")
            check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                        f"a {damage} at a {role} report path fails closed")
            detail = document["source_errors"][0]["detail"]
            check("not a readable regular file" in detail,
                  f"a {damage} at a {role} report path is diagnosed as present "
                  f"but unusable, never as absent", detail)
            check(role in detail, f"and names the {role} scope", detail)

    # An UNSTATTABLE path is not an absent one. The convenience
    # predicates (`lexists`, `exists`, `is_file`) all swallow OSError and
    # answer False, so without direct stat calls each of these would read
    # exactly like a missing file and the optional docs-wip scope would
    # skip it and answer `clear`.
    #
    # The first shape is real and needs no permissions, no root check and
    # no patching: a regular FILE standing where `docs/` belongs makes
    # every report path raise ENOTDIR rather than ENOENT.
    for role in ("docs-wip", "checkout"):
        with tempfile.TemporaryDirectory() as tmp:
            checkout = build_reports(Path(tmp) / "checkout")
            docs = build_reports(Path(tmp) / "docs-wip")
            broken = (docs if role == "docs-wip" else checkout) / "docs"
            for child in sorted(broken.iterdir()):
                child.unlink()
            broken.rmdir()
            broken.write_text("a file where a directory belongs", encoding="utf-8")
            document = evaluate("injury_log", repo_root=checkout, docs_root=docs,
                                state_root=Path(tmp) / "none")
            check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                        f"an unstattable {role} report path fails closed")
            detail = document["source_errors"][0]["detail"]
            check("could not be examined" in detail,
                  f"an unstattable {role} report path is diagnosed, never "
                  f"treated as absent", detail)

    # The second shape is the permission denial itself. It is injected
    # rather than chmod-ed so the case is deterministic everywhere,
    # including as root and on filesystems that ignore mode bits — a
    # chmod-based case would either flake or silently stop covering this.
    for patched in ("lstat", "stat"):
        with tempfile.TemporaryDirectory() as tmp:
            checkout = build_reports(Path(tmp) / "checkout")
            docs = build_reports(Path(tmp) / "docs-wip")
            denied = docs / "docs" / "code_health_findings.md"
            original = getattr(os, patched)

            def guarded(path, *args, _original=original, **kwargs):
                if str(path) == str(denied):
                    raise PermissionError(13, "Permission denied")
                return _original(path, *args, **kwargs)

            setattr(os, patched, guarded)
            try:
                document = evaluate("injury_log", repo_root=checkout,
                                    docs_root=docs,
                                    state_root=Path(tmp) / "none")
            finally:
                setattr(os, patched, original)
            check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                        f"a denied os.{patched} fails closed")
            detail = document["source_errors"][0]["detail"]
            check("Permission denied" in detail,
                  f"a denied os.{patched} is diagnosed actionably", detail)
            check("docs-wip" in detail, "and names the scope", detail)

    # A symlink whose TARGET is gone is present and unusable, not absent —
    # which is why the presence question is asked with lstat.
    with tempfile.TemporaryDirectory() as tmp:
        checkout = build_reports(Path(tmp) / "checkout")
        docs = build_reports(Path(tmp) / "docs-wip")
        dangling = docs / "docs" / "code_health_findings.md"
        dangling.unlink()
        dangling.symlink_to(Path(tmp) / "gone.md")
        check(not os.path.exists(dangling) and os.path.lexists(dangling),
              "the fixture really is a dangling symlink")
        document = evaluate("injury_log", repo_root=checkout, docs_root=docs,
                            state_root=Path(tmp) / "none")
        check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                    "a dangling symlink is present-and-unusable, not absent")

    # The shipped by-branch resolution is the census's own idiom, and its
    # actionable-stop is downgraded to None here.
    with tempfile.TemporaryDirectory() as tmp:
        repo = Path(tmp) / "scratch"
        repo.mkdir()
        subprocess.run(["git", "init", "-q", str(repo)], check=True,
                       capture_output=True)
        check_equal(inflight.resolve_docs_worktree(repo), None,
                    "a checkout with no docs-wip branch resolves to None")
        try:
            probe_census.resolve_docs_worktree(str(repo))
            check(False, "the census helper still stops on it")
        except probe_census.DocsWorktreeMissing:
            check(True, "the census helper still treats it as an actionable stop")


# ==========================================================================
# The whole evaluation
# ==========================================================================

def test_all_four_sources_together() -> None:
    """One evaluation reporting a match from every source category."""
    with tempfile.TemporaryDirectory() as tmp:
        state = build_test_state(Path(tmp), [
            make_run("probe-flake:injury-log", "measuring", status="running",
                     heartbeat_at="2026-08-21T11:55:00Z")])
        checkout = build_reports(Path(tmp) / "checkout", {
            **DEFAULT_REPORTS,
            "NCT": [(6, "Injury-log probe accepts the wrong unit", "", False)]})
        docs = build_reports(Path(tmp) / "docs-wip", {
            **DEFAULT_REPORTS,
            "CH": [(9, "The injury_log probe leaks a world thread", "", False)]})
        api = FakeGitHub(
            issues=[issue(11, "Injury-log probe accepts the wrong unit")],
            pulls=[pull(12, "Gate the injury_log probe's fall phase", draft=True)])

        with NonInteraction(state, checkout, docs) as guard:
            document = evaluate("injury_log", state_root=state, repo_root=checkout,
                                docs_root=docs, github=api)
            guard.assert_untouched("four-source evaluation")

        check_equal(document["result"], inflight.RESULT_IN_FLIGHT, "in flight")
        check_equal(document["source_errors"], [], "no source failed")
        check_equal(sorted({m["source"] for m in document["matches"]}),
                    sorted(inflight.SOURCES),
                    "every source category contributed a match")
        check_equal(document["sources"],
                    {source: "read" for source in inflight.SOURCES},
                    "and every source is recorded as read")
        check_equal(document["probe"], "injury_log", "the probe key is echoed")
        check_equal(document["script"], "injury_log_probe.py",
                    "with its registered script")
        check_equal(document["test_ids"], {"run": "probe:injury-log",
                                           "flake": "probe-flake:injury-log"},
                    "and both $test identities")
        check_equal(document["target_repository"], REPOSITORY,
                    "and the target repository")
        check_equal(document["evaluated_at"], NOW.isoformat(),
                    "and the evaluation instant")
        check_equal(document["schema"], inflight.INFLIGHT_SCHEMA, "schema declared")
        for match in document["matches"]:
            check_equal(match["probe"], "injury_log", "each match names the probe")
            check(bool(match["reason"]), "each match carries a reason")
            check(bool(match["evidence"]), "each match carries evidence")
        json.dumps(document)
        check(True, "the document is JSON-serializable")
        text = inflight.render(document)
        check("in-flight" in text and "NCT-6" in text,
              "the rendering shows the verdict and its evidence", text)


def test_a_source_error_beats_a_match() -> None:
    """A partial scan is never presented as determinate — nor as clear."""
    with tempfile.TemporaryDirectory() as tmp:
        checkout = build_reports(Path(tmp) / "checkout", {
            **DEFAULT_REPORTS,
            "NCT": [(6, "Injury-log probe accepts the wrong unit", "", False)]})

        def broken(path, params):
            raise inflight.SourceError("gh api failed: HTTP 500")

        document = evaluate("injury_log", repo_root=checkout, github=broken,
                            state_root=Path(tmp) / "none")
        check_equal(document["result"], inflight.RESULT_SOURCE_ERROR,
                    "an incomplete scan reports source-error, not in-flight")
        check(bool(sources_of(document, inflight.SOURCE_REPORT)),
              "and the evidence it DID find is still reported")
        check_equal(len(document["source_errors"]), 2,
                    "both failing sources are reported")


def test_a_fully_clear_evaluation() -> None:
    """Every source read completely, nothing matched."""
    with tempfile.TemporaryDirectory() as tmp:
        state = build_test_state(Path(tmp), [
            make_run("probe:injury-log", "finished", status="completed"),
            make_run("probe:role", "other", status="running",
                     heartbeat_at="2026-08-21T11:59:00Z")])
        checkout = build_reports(Path(tmp) / "checkout")
        docs = build_reports(Path(tmp) / "docs-wip")
        api = FakeGitHub(issues=[issue(1, "Unrelated"), issue(2, "Also unrelated")],
                         pulls=[pull(3, "Nothing to do with probes")])
        with NonInteraction(state, checkout, docs) as guard:
            document = evaluate("injury_log", state_root=state, repo_root=checkout,
                                docs_root=docs, github=api)
            guard.assert_untouched("clear evaluation")
        check_equal(document["result"], inflight.RESULT_CLEAR, "clear")
        check_equal(document["matches"], [], "with no matches")
        check_equal(document["source_errors"], [], "and no source errors")
        check("clear:" in inflight.render(document), "the rendering says so")


def test_an_unknown_probe_key_is_rejected() -> None:
    """An unregistered key is a caller error, never a `clear`."""
    try:
        inflight.evaluate_probe_inflight("definitely_not_a_probe", now=NOW,
                                         target_repository=REPOSITORY,
                                         github=FakeGitHub())
        check(False, "an unknown key raises InflightRejected")
    except inflight.InflightRejected as exc:
        check("definitely_not_a_probe" in str(exc),
              "the rejection names the offending key", str(exc))
        check("probe_runner_registry.PROBES" in str(exc),
              "and the authoritative registry", str(exc))

    try:
        inflight.evaluate_probe_inflight(
            "injury_log", now=datetime(2026, 8, 21, 12, 0, 0),
            target_repository=REPOSITORY, github=FakeGitHub())
        check(False, "a naive evaluation time is rejected")
    except inflight.InflightRejected as exc:
        check("timezone-aware" in str(exc), "and says why", str(exc))

    # An identity index that does not own the key would answer "no
    # occurrences" for every subject in every source — the one way a
    # caller can make this component answer `clear` without looking at
    # anything. It is refused rather than silently believed.
    try:
        inflight.evaluate_probe_inflight(
            "injury_log", now=NOW, target_repository=REPOSITORY,
            github=FakeGitHub(),
            identity_index=inflight.build_identity_index(
                [("something_else", "something_else_probe.py", "x")]))
        check(False, "an index that does not own the probe is rejected")
    except inflight.InflightRejected as exc:
        check("registers no forms" in str(exc), "and says why", str(exc))

    # An index that DOES own it is accepted.
    document = inflight.evaluate_probe_inflight(
        "injury_log", now=NOW, target_repository=REPOSITORY,
        github=FakeGitHub(), state_root=Path("/nonexistent-state-root"),
        docs_root=None, repo_root=probe_engine.REPO_ROOT,
        identity_index=inflight.build_identity_index())
    check(document["result"] in (inflight.RESULT_CLEAR,
                                 inflight.RESULT_IN_FLIGHT,
                                 inflight.RESULT_SOURCE_ERROR),
          "an index that owns the probe is accepted")


def test_the_shipped_cli() -> None:
    """The CLI is exercised end to end, offline, on a scratch repository.

    `main` takes no injection points, so this drives it exactly as a
    caller would: against a real scratch git repository whose `origin`
    names a GitHub repository, with `probe_engine.REPO_ROOT` pointed at it
    and only the default transport substituted.
    """
    with tempfile.TemporaryDirectory() as tmp:
        repo = Path(tmp) / "scratch"
        build_reports(repo, {**DEFAULT_REPORTS,
                             "NCT": [(6, "Injury-log probe accepts the wrong unit",
                                      "", False)]})
        subprocess.run(["git", "init", "-q", str(repo)], check=True,
                       capture_output=True)
        subprocess.run(["git", "-C", str(repo), "remote", "add", "origin",
                        f"git@github.com:{REPOSITORY}.git"], check=True,
                       capture_output=True)
        api = FakeGitHub()

        saved_root = probe_engine.REPO_ROOT
        saved_transport = inflight.default_github_transport
        probe_engine.REPO_ROOT = str(repo)
        inflight.default_github_transport = lambda: api
        try:
            buffer = io.StringIO()
            with redirect_stdout(buffer):
                code = inflight.main(["--probe", "injury_log", "--json"])
            document = json.loads(buffer.getvalue())
            check_equal(code, inflight.EXIT_OK, "a determinate verdict exits 0")
            check_equal(document["result"], inflight.RESULT_IN_FLIGHT,
                        "the CLI reports the open finding")
            check_equal(document["target_repository"], REPOSITORY,
                        "resolved from the scratch repository's own origin")
            check_equal(document["sources"],
                        {s: "read" for s in inflight.SOURCES},
                        "every source was read")
            check(api.requests, "the injected transport really was used")

            buffer = io.StringIO()
            with redirect_stdout(buffer):
                code = inflight.main(["--probe", "role"])
            check_equal(code, inflight.EXIT_OK, "a clear verdict exits 0 too")
            check("-> clear" in buffer.getvalue(),
                  "and renders as a table by default", buffer.getvalue())

            # A source error exits 1, distinctly from a rejection.
            (repo / "docs" / "code_health_findings.md").write_text(
                "# R\n\n## Status\n\n- [ ] CH-1. Title\n", encoding="utf-8")
            buffer, errors = io.StringIO(), io.StringIO()
            with redirect_stdout(buffer), redirect_stderr(errors):
                code = inflight.main(["--probe", "injury_log", "--json"])
            check_equal(code, inflight.EXIT_SOURCE_ERROR,
                        "a source error exits 1")
            check_equal(json.loads(buffer.getvalue())["result"],
                        inflight.RESULT_SOURCE_ERROR,
                        "and says so in the document")

            errors = io.StringIO()
            with redirect_stderr(errors):
                code = inflight.main(["--probe", "definitely_not_a_probe"])
            check_equal(code, inflight.EXIT_REJECTED,
                        "an unknown key exits 2, distinctly")
            check("definitely_not_a_probe" in errors.getvalue(),
                  "naming the key on stderr", errors.getvalue())
        finally:
            probe_engine.REPO_ROOT = saved_root
            inflight.default_github_transport = saved_transport


def test_the_default_transport_is_not_reached_by_accident() -> None:
    """The offline tripwire itself works, so no case can pass vacuously."""
    try:
        inflight.default_github_transport()
        check(False, "the default transport is a tripwire under this harness")
    except Tripwire:
        check(True, "the default transport is a tripwire under this harness")
    try:
        socket.socket()
        check(False, "sockets are a tripwire under this harness")
    except Tripwire:
        check(True, "sockets are a tripwire under this harness")
    try:
        subprocess.run(["gh", "api", "repos/x/y/issues"], capture_output=True)
        check(False, "the gh binary is a tripwire under this harness")
    except Tripwire:
        check(True, "the gh binary is a tripwire under this harness")


def main() -> int:
    cases = [
        test_humanized_aliases_all_name_one_probe,
        test_prefix_families_stay_distinct,
        test_substring_matching_is_not_used,
        test_a_common_word_key_over_excludes_by_design,
        test_subject_ambiguity_excludes_and_keeps_its_evidence,
        test_both_test_identities_are_the_same_probes_work,
        test_every_active_and_terminal_state,
        test_fresh_stale_and_missing_timestamps,
        test_unknown_state_fails_closed,
        test_absent_versus_damaged_test_state,
        test_open_issues_match_titles_only,
        test_open_draft_and_merged_pull_requests,
        test_a_malformed_tracker_record_fails_closed,
        test_every_page_is_retrieved,
        test_a_failing_or_endless_list_fails_closed,
        test_target_repository_resolution,
        test_all_four_report_families,
        test_every_heading_state,
        test_broken_report_states_fail_closed,
        test_narrative_mentions_are_not_the_subject,
        test_both_report_worktrees,
        test_docs_worktree_absence_is_normal_but_damage_is_not,
        test_all_four_sources_together,
        test_a_source_error_beats_a_match,
        test_a_fully_clear_evaluation,
        test_an_unknown_probe_key_is_rejected,
        test_the_shipped_cli,
        test_the_default_transport_is_not_reached_by_accident,
    ]
    with Offline():
        for case in cases:
            try:
                case()
            except Exception as error:                       # noqa: BLE001
                FAILURES.append(
                    f"{case.__name__} raised {type(error).__name__}: {error}")

    print(f"probe_inflight self-test: {PASSED} checks passed, "
          f"{len(FAILURES)} failed")
    for failure in FAILURES:
        print(f"  FAIL {failure}")
    return 1 if FAILURES else 0


if __name__ == "__main__":
    sys.exit(main())
