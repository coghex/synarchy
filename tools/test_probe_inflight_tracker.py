#!/usr/bin/env python3
"""GitHub tracker cases for the in-flight self-test (#2141).

Owns the six cases that pin sources 2 and 3, open issues and open pull
requests: title-only issue matching, open/draft/closed/merged pull
requests, malformed tracker records, complete pagination, failing and
endless pagination, and target-repository resolution.

Every tracker evaluation injects a `FakeGitHub` transport, so a case
that forgot to would hit the `Offline` tripwire on
`probe_inflight.default_github_transport` rather than pass vacuously.
`test_target_repository_resolution` is one of the three cases that
legitimately shells out to `git` against its own scratch repositories,
so it runs outside `NonInteraction`.

Not independently runnable: it parses no arguments, executes nothing at
import time and exposes no command-line interface. `CASES` is its whole
public surface, and the only entry point is
`tools/test_probe_inflight.py`, which runs these inside the global
`Offline` boundary.
"""
from __future__ import annotations

import os
import subprocess
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_inflight as inflight  # noqa: E402
from test_probe_inflight_support import (  # noqa: E402
    ISSUES_PATH,
    MISSING,
    NOW,
    PULLS_PATH,
    REPOSITORY,
    FakeGitHub,
    NonInteraction,
    build_reports,
    check,
    check_equal,
    evaluate,
    issue,
    pull,
    sources_of,
)

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


CASES = (
    test_open_issues_match_titles_only,
    test_open_draft_and_merged_pull_requests,
    test_a_malformed_tracker_record_fails_closed,
    test_every_page_is_retrieved,
    test_a_failing_or_endless_list_fails_closed,
    test_target_repository_resolution,
)
