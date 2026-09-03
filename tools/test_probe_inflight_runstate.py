#!/usr/bin/env python3
"""`$test` run-state cases for the in-flight self-test (#2141).

Owns the five cases that pin source 1, the machine-local `codex-test`
registry: the two test identities excluding each other, every active and
terminal status, fresh/stale/missing/malformed timestamps, unknown-state
fail-closed behavior, and an absent registry versus a damaged one.

Every evaluation here runs under `NonInteraction`, so the registry
fixtures are proved byte-for-byte untouched and no coordinator
subprocess or lock is reachable.

Not independently runnable: it parses no arguments, executes nothing at
import time and exposes no command-line interface. `CASES` is its whole
public surface, and the only entry point is
`tools/test_probe_inflight.py`, which runs these inside the global
`Offline` boundary.
"""
from __future__ import annotations

import os
import sys
import tempfile
from datetime import timedelta
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_external_evidence as evidence  # noqa: E402
import probe_inflight as inflight  # noqa: E402
from test_probe_inflight_support import (  # noqa: E402
    MISSING,
    NOW,
    NonInteraction,
    build_reports,
    build_test_state,
    check,
    check_equal,
    evaluate,
    make_run,
    run_with_identity,
    sources_of,
)

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


CASES = (
    test_both_test_identities_are_the_same_probes_work,
    test_every_active_and_terminal_state,
    test_fresh_stale_and_missing_timestamps,
    test_unknown_state_fails_closed,
    test_absent_versus_damaged_test_state,
)
