#!/usr/bin/env python3
"""Findings-report cases for the in-flight self-test (#2141).

Owns the six cases that pin source 4, the tracked findings reports: all
four report families with their native key prefixes, every heading
disposition, partial/duplicate/inconsistent report states, narrative-only
mentions that must not match, checked-out versus docs-worktree evidence,
and an absent docs worktree versus a damaged one.

`test_docs_worktree_absence_is_normal_but_damage_is_not` is one of the
three cases that legitimately shells out to `git` against its own
scratch repositories, so it runs outside `NonInteraction`.

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
import probe_census  # noqa: E402
import probe_inflight as inflight  # noqa: E402
from test_probe_inflight_support import (  # noqa: E402
    DEFAULT_REPORTS,
    NonInteraction,
    build_reports,
    check,
    check_equal,
    evaluate,
    sources_of,
)

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


CASES = (
    test_all_four_report_families,
    test_every_heading_state,
    test_broken_report_states_fail_closed,
    test_narrative_mentions_are_not_the_subject,
    test_both_report_worktrees,
    test_docs_worktree_absence_is_normal_but_damage_is_not,
)
