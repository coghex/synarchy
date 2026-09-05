#!/usr/bin/env python3
"""Confinement cases for `test_probe_external_evidence.py` (#2187).

The five cases here own filesystem confinement: report reads stay
inside the canonical `reports/` directory, a symlinked or misplaced
`reports/` refuses every read, the registry must be a regular file
inside the state root, and a malformed report-path STRING (embedded
NUL, overlong) becomes one run's diagnostic rather than a traceback.
Each one records the paths the reader actually opens, so an
out-of-scope read fails even when no byte of it reaches the output,
and the `SENTINEL` planted in every out-of-scope file must stay unread.

One constraint on the last case, stated here so it is not
rediscovered: `test_a_malformed_report_path_never_aborts_the_read`
ends with a `read()` of an unusable state root and a
`resolve_state_root()` of an unusable repository path, and the latter
reaches `subprocess.run`. Both statements sit OUTSIDE the
`NonInteraction` block by construction and must stay there: under the
tripwire `subprocess.run` raises `Tripwire`, an `AssertionError` that
neither `except EvidenceRejected` nor `except ValueError` catches.

`CASES` is this owner's inventory in the order the aggregate runs it.
This module holds case bodies and that inventory only; `python3
tools/test_probe_external_evidence.py --only confinement` is the way
to run it.
"""
from __future__ import annotations

import json
import os
import shutil
import tempfile
from pathlib import Path

from probe_external_evidence_test_support import (  # noqa: E402
    SENTINEL, NonInteraction, RecordReads, build_state, check, check_equal,
    evidence, make_run, read, report_path, report_text,
)


def test_report_reads_are_confined_to_the_reports_directory() -> None:
    """A recorded path never widens read scope."""
    with tempfile.TemporaryDirectory() as tmp:
        outside = Path(tmp) / "outside.test-result.md"
        outside.write_text(SENTINEL + "\n### OBS-001 — leaked\n", encoding="utf-8")

        runs = [
            make_run("probe:role", "absolute-escape", claimed_at="2026-08-15T00:00:00Z"),
            make_run("probe:role", "traversal-escape", claimed_at="2026-08-14T00:00:00Z"),
            make_run("probe:role", "symlink-escape", claimed_at="2026-08-13T00:00:00Z"),
            make_run("probe:role", "wrong-suffix", claimed_at="2026-08-12T00:00:00Z"),
            make_run("probe:role", "nested", claimed_at="2026-08-11T00:00:00Z"),
        ]
        state = build_state(Path(tmp), runs, {})
        reports = state / evidence.REPORTS_DIRNAME
        os.symlink(outside, reports / ("symlink-escape" + evidence.REPORT_SUFFIX))
        (reports / "wrong-suffix.md").write_text(SENTINEL, encoding="utf-8")
        (reports / "nested").mkdir()
        (reports / "nested" / ("nested" + evidence.REPORT_SUFFIX)).write_text(
            SENTINEL, encoding="utf-8")

        paths = {
            "absolute-escape": str(outside),
            "traversal-escape": str(reports / ".." / ".." / "outside.test-result.md"),
            "symlink-escape": report_path(state, "symlink-escape"),
            "wrong-suffix": str(reports / "wrong-suffix.md"),
            "nested": str(reports / "nested" / ("nested" + evidence.REPORT_SUFFIX)),
        }
        for record in runs:
            record["report_path"] = paths[record["run_id"]]
        build_state(Path(tmp), runs, {})

        with NonInteraction(state) as guard:
            with RecordReads() as reads:
                result = read(state, "role")
            guard.assert_untouched("confined report reads")

        scope = reports.resolve()
        escaped = [p for p in reads.paths
                   if p.resolve() != (state / evidence.REGISTRY_FILENAME).resolve()
                   and p.resolve().parent != scope]
        check(not escaped, "no file outside reports/ is opened at all",
              str([str(p) for p in escaped]))
        check(outside.resolve() not in [p.resolve() for p in reads.paths],
              "the out-of-scope target is never opened")
        rendered = evidence.render(result) + json.dumps(result)
        check(SENTINEL not in rendered,
              "no out-of-scope file content reaches the output")
        for run in result["runs"]:
            check_equal(run["report"]["status"], evidence.REPORT_OUT_OF_SCOPE,
                        f"{run['run_id']} is refused as out of scope")
            check_equal(run["report"]["observation_count"], None,
                        f"{run['run_id']} contributes no observation count")
        check_equal(len(result["diagnostics"]), len(runs),
                    "each refusal is diagnosed exactly once")


def test_a_symlinked_reports_directory_refuses_every_read() -> None:
    """The scope check is on the DIRECTORY too, not only each path.

    A symlinked `reports/` relocates the whole read scope out of the
    state tree while every individual recorded path still resolves to a
    `*.test-result.md` file directly under its own parent — so the
    per-path check alone would happily read them.
    """
    with tempfile.TemporaryDirectory() as tmp:
        runs = [make_run("probe:role", "relocated-run")]
        state = build_state(Path(tmp), runs, {})

        # Move reports/ out of the state tree and symlink it back in,
        # with a perfectly well-formed report waiting inside it.
        external = Path(tmp) / "elsewhere"
        external.mkdir()
        leaked = external / ("relocated-run" + evidence.REPORT_SUFFIX)
        leaked.write_text(
            report_text("relocated-run", "probe:role", "observations", 3)
            + SENTINEL + "\n", encoding="utf-8")
        shutil.rmtree(state / evidence.REPORTS_DIRNAME)
        os.symlink(external, state / evidence.REPORTS_DIRNAME)

        runs[0]["report_path"] = str(state / evidence.REPORTS_DIRNAME
                                     / ("relocated-run" + evidence.REPORT_SUFFIX))
        document = json.loads((state / evidence.REGISTRY_FILENAME).read_text())
        document["runs"] = runs
        (state / evidence.REGISTRY_FILENAME).write_text(json.dumps(document))

        with NonInteraction(state) as guard:
            with RecordReads() as reads:
                result = read(state, "role")
            guard.assert_untouched("symlinked reports directory")

        opened = [path.resolve() for path in reads.paths]
        check(leaked.resolve() not in opened,
              "the relocated report is never opened", str([str(p) for p in opened]))
        check(external.resolve() not in [p.resolve().parent for p in reads.paths],
              "nothing in the relocated directory is opened")
        run = result["runs"][0]
        check_equal(run["report"]["status"], evidence.REPORT_OUT_OF_SCOPE,
                    "the relocated report is refused as out of scope")
        check_equal(run["report"]["observation_count"], None,
                    "it contributes no observation count")
        check_equal(run["execution_status"], "passed",
                    "the run's mechanical fields are still reported")
        check(any("immediate child of the state root" in d
                  for d in result["diagnostics"]),
              "the relocated directory is diagnosed once at directory level",
              str(result["diagnostics"]))
        check_equal(len(result["diagnostics"]), 1,
                    "one directory-level diagnostic, not one per run")
        check(SENTINEL not in evidence.render(result) + json.dumps(result),
              "no relocated content reaches the output")

        # The scope helper says so directly, and creates nothing.
        diagnostics = evidence.DiagnosticLog()
        check_equal(evidence.resolve_reports_scope(state, diagnostics), None,
                    "resolve_reports_scope refuses the relocated directory")
        check_equal(len(diagnostics.entries), 1, "and diagnoses it exactly once")
        check_equal(diagnostics.scopes(), {evidence.SCOPE_REPORT},
                    "and scopes it to the report, not to active-run state")

        # A real directory in the same place is trusted again.
        (state / evidence.REPORTS_DIRNAME).unlink()
        (state / evidence.REPORTS_DIRNAME).mkdir()
        trusted = evidence.DiagnosticLog()
        check_equal(evidence.resolve_reports_scope(state, trusted),
                    (state / evidence.REPORTS_DIRNAME).resolve(),
                    "a real reports directory is trusted")
        check_equal(trusted.entries, [], "and produces no diagnostic")


def test_a_misplaced_reports_directory_refuses_every_read() -> None:
    """`reports/` must be a DIRECTORY, not merely correctly named.

    A regular file sitting at `reports` passes the resolve-and-confine
    check, and every recorded report then resolves to a path under it
    that does not exist — so without a kind check each one would read as
    a silent `absent` rather than as the damaged state it is.
    """
    with tempfile.TemporaryDirectory() as tmp:
        runs = [make_run("probe:role", "run")]
        state = build_state(Path(tmp), runs, {})
        runs[0]["report_path"] = report_path(state, "run")
        document = json.loads((state / evidence.REGISTRY_FILENAME).read_text())
        document["runs"] = runs
        (state / evidence.REGISTRY_FILENAME).write_text(json.dumps(document))
        shutil.rmtree(state / evidence.REPORTS_DIRNAME)
        (state / evidence.REPORTS_DIRNAME).write_text(SENTINEL, encoding="utf-8")

        with NonInteraction(state) as guard:
            with RecordReads() as reads:
                result = read(state, "role")
            guard.assert_untouched("misplaced reports directory")

        run = result["runs"][0]
        check_equal(run["report"]["status"], evidence.REPORT_OUT_OF_SCOPE,
                    "the report is refused, not reported absent")
        check_equal(run["execution_status"], "passed",
                    "the run's mechanical fields are still reported")
        check(any("not a directory" in d for d in result["diagnostics"]),
              "the misplaced reports path is diagnosed", str(result["diagnostics"]))
        check_equal(len(result["diagnostics"]), 1,
                    "one directory-level diagnostic, not one per run")
        check((state / evidence.REPORTS_DIRNAME).resolve()
              not in [p.resolve() for p in reads.paths],
              "the file standing in for the directory is never opened")
        check(SENTINEL not in evidence.render(result) + json.dumps(result),
              "none of its content reaches the output")

        # An ABSENT reports directory is not damage: the reports are
        # simply not there, which each run already says for itself.
        (state / evidence.REPORTS_DIRNAME).unlink()
        clean = read(state, "role")
        check_equal(clean["runs"][0]["report"]["status"], evidence.REPORT_ABSENT,
                    "an absent reports directory makes each report absent")
        check_equal(clean["diagnostics"], [],
                    "and is not diagnosed as damage")


def test_the_registry_is_confined_to_the_state_root() -> None:
    """A symlinked or non-regular `registry.json` is refused, not followed."""
    with tempfile.TemporaryDirectory() as tmp:
        external = Path(tmp) / "planted.json"
        external.write_text(json.dumps({
            "schema": evidence.COORDINATOR_SCHEMA,
            "runs": [make_run("probe:role", "planted-run",
                              revision_subject=SENTINEL)],
        }), encoding="utf-8")

        state = build_state(Path(tmp), [make_run("probe:role", "real-run")], {})
        (state / evidence.REGISTRY_FILENAME).unlink()
        os.symlink(external, state / evidence.REGISTRY_FILENAME)

        with NonInteraction(state) as guard:
            with RecordReads() as reads:
                result = read(state, "role")
            guard.assert_untouched("symlinked registry")

        check_equal(result["runs"], [], "a relocated registry contributes no runs")
        check(external.resolve() not in [p.resolve() for p in reads.paths],
              "the planted registry is never opened",
              str([str(p) for p in reads.paths]))
        check(SENTINEL not in evidence.render(result) + json.dumps(result),
              "none of its content reaches the output")
        check(any("refused to read the registry" in d for d in result["diagnostics"]),
              "the relocated registry is diagnosed", str(result["diagnostics"]))
        check_equal(result["state"], evidence.STATE_PRESENT,
                    "the state is still present, just unusable")

        diagnostics = evidence.DiagnosticLog()
        check_equal(evidence.resolve_registry_path(state, diagnostics), None,
                    "resolve_registry_path refuses it directly")
        check_equal(len(diagnostics.entries), 1, "and diagnoses it exactly once")
        check_equal(diagnostics.scopes(), {evidence.SCOPE_REGISTRY},
                    "and scopes it to the registry")

    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [make_run("probe:role", "run")], {})
        (state / evidence.REGISTRY_FILENAME).unlink()
        (state / evidence.REGISTRY_FILENAME).mkdir()
        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("non-regular registry")
        check_equal(result["runs"], [], "a directory registry contributes no runs")
        check(any("not a regular file" in d for d in result["diagnostics"]),
              "a non-regular registry is diagnosed", str(result["diagnostics"]))

    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [make_run("probe:role", "run")], {})
        resolved = evidence.resolve_registry_path(state, evidence.DiagnosticLog())
        check_equal(resolved, (state / evidence.REGISTRY_FILENAME).resolve(),
                    "a real registry resolves to itself")


def test_a_malformed_report_path_never_aborts_the_read() -> None:
    """An unusable path STRING is one run's diagnostic, not a traceback.

    A registry field is arbitrary external text. A path built from a
    string containing an embedded NUL raises `ValueError` — not
    `OSError` — from `resolve` and `stat`, so catching only `OSError`
    would let one malformed record abort the whole read and take every
    later valid run with it.
    """
    with tempfile.TemporaryDirectory() as tmp:
        runs = [
            make_run("probe:role", "nul-run", claimed_at="2026-08-15T00:00:00Z"),
            make_run("probe:role", "long-run", claimed_at="2026-08-14T00:00:00Z"),
            make_run("probe:role", "good-run", claimed_at="2026-08-13T00:00:00Z"),
        ]
        state = build_state(Path(tmp), runs, {"good-run": ("observations", 2)})
        paths = {
            "nul-run": report_path(state, "nul-run").replace("nul-run", "nul\x00run"),
            "long-run": report_path(state, "n" * 4096),
            "good-run": report_path(state, "good-run"),
        }
        for record in runs:
            record["report_path"] = paths[record["run_id"]]
        document = json.loads((state / evidence.REGISTRY_FILENAME).read_text())
        document["runs"] = runs
        (state / evidence.REGISTRY_FILENAME).write_text(json.dumps(document))

        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("malformed report path")

        by_id = {r["run_id"]: r for r in result["runs"]}
        check_equal(len(result["runs"]), 3, "no run is lost to the malformed one")
        check_equal(by_id["nul-run"]["report"]["status"], evidence.REPORT_UNREADABLE,
                    "an unusable path string is unreadable evidence")
        check_equal(by_id["nul-run"]["execution_status"], "passed",
                    "the malformed run's mechanical fields survive")
        check(any("nul-run" in d for d in result["diagnostics"]),
              "the malformed path is diagnosed", str(result["diagnostics"]))

        # The LATER, valid run is still read in full — this is the half
        # a bare traceback would have destroyed.
        check_equal(by_id["good-run"]["report"]["status"], evidence.REPORT_AVAILABLE,
                    "a later valid report is still read")
        check_equal(by_id["good-run"]["report"]["observation_count"], 2,
                    "and its observations are still counted")
        check(by_id["long-run"]["report"]["status"] in (
                  evidence.REPORT_UNREADABLE, evidence.REPORT_ABSENT),
              "an over-long path is handled without raising",
              str(by_id["long-run"]["report"]["status"]))

        check_equal(evidence.main(["--probe", "role", "--json",
                                   "--state-root", str(state)]),
                    evidence.EXIT_OK, "the CLI still exits 0")

    # An unusable STATE ROOT is controlled too — a rejection naming it,
    # or the ordinary absent-state answer, but never a traceback.
    # (`Path.is_dir` swallows the NUL itself on CPython and answers
    # False; the reader's own guard covers platforms where it does not.)
    try:
        result = read("/tmp/nul\x00root", "role")
        check_equal(result["state"], evidence.STATE_ABSENT,
                    "an unstattable state root reads as absent")
        check_equal(result["runs"], [], "and contributes no runs")
    except evidence.EvidenceRejected as exc:
        check("state root" in str(exc),
              "the rejection names the state root", str(exc))
        check(True, "and is a controlled rejection")
    except ValueError as exc:                              # pragma: no cover
        check(False, "an unusable state root never raises ValueError", repr(exc))
        check(False, "an unusable state root never raises ValueError", repr(exc))

    # And so is one that git cannot resolve for the same reason.
    try:
        evidence.resolve_state_root("/tmp/nul\x00repo")
        check(False, "an unusable repo path raises EvidenceRejected")
    except evidence.EvidenceRejected as exc:
        check(True, "resolve_state_root rejects it")
    except ValueError as exc:                              # pragma: no cover
        check(False, "resolve_state_root raises EvidenceRejected", repr(exc))


#: This owner's cases, in the order the aggregate has always run them.
CASES = (
    test_report_reads_are_confined_to_the_reports_directory,
    test_a_symlinked_reports_directory_refuses_every_read,
    test_a_misplaced_reports_directory_refuses_every_read,
    test_the_registry_is_confined_to_the_state_root,
    test_a_malformed_report_path_never_aborts_the_read,
)
