#!/usr/bin/env python3
"""Cross-source and CLI cases for the in-flight self-test (#2141).

Owns the six cases that pin the whole evaluation rather than one
evidence source: all four sources reporting together, a source error
taking precedence over a match, a fully clear evaluation, unknown and
ambiguous probe-key rejection, the shipped CLI's output and exit
contract, and the proof that the default GitHub transport and the
network tripwires are actually armed.

That last case discriminates on `except Tripwire`, so it depends on
`Tripwire` having exactly one definition — the shared support module's —
and on `Offline` having patched the same `probe_inflight` module object
these cases import. `test_the_shipped_cli` is one of the three cases
that legitimately shells out to `git` against its own scratch
repositories, so it runs outside `NonInteraction`.

Not independently runnable: it parses no arguments, executes nothing at
import time and exposes no command-line interface. `CASES` is its whole
public surface, and the only entry point is
`tools/test_probe_inflight.py`, which runs these inside the global
`Offline` boundary.
"""
from __future__ import annotations

import io
import json
import os
import socket
import subprocess
import sys
import tempfile
from contextlib import redirect_stdout, redirect_stderr
from datetime import datetime
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_engine  # noqa: E402
import probe_inflight as inflight  # noqa: E402
from test_probe_inflight_support import (  # noqa: E402
    DEFAULT_REPORTS,
    NOW,
    REPOSITORY,
    FakeGitHub,
    NonInteraction,
    Tripwire,
    build_reports,
    build_test_state,
    check,
    check_equal,
    evaluate,
    issue,
    make_run,
    pull,
    sources_of,
)

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


CASES = (
    test_all_four_sources_together,
    test_a_source_error_beats_a_match,
    test_a_fully_clear_evaluation,
    test_an_unknown_probe_key_is_rejected,
    test_the_shipped_cli,
    test_the_default_transport_is_not_reached_by_accident,
)
