#!/usr/bin/env python3
"""Report cases for `test_probe_external_evidence.py` (#2187).

The five cases here own report parsing and the mechanical-versus-
interpretive split: clean and observed reports keep distinct summaries,
an incomplete run reports unavailable fields rather than false facts,
execution status comes from the registry and never from the report's
prose, a missing or malformed report never aborts the read, and an
existing non-regular report is damage rather than absence.

`CASES` is this owner's inventory in the order the aggregate runs it.
This module holds case bodies and that inventory only; `python3
tools/test_probe_external_evidence.py --only reports` is the way to
run it.
"""
from __future__ import annotations

import json
import os
import tempfile
from pathlib import Path

from probe_external_evidence_test_support import (  # noqa: E402
    NonInteraction, RecordReads, build_state, check, check_equal, evidence,
    make_run, read, report_path,
)


def test_clean_and_observed_reports() -> None:
    """Observation status distinguishes clean, observed and not-yet-known."""
    with tempfile.TemporaryDirectory() as tmp:
        runs = [
            make_run("probe:role", "clean-run", claimed_at="2026-08-12T10:00:00Z",
                     interpretation_outcome="clean",
                     report_path=None),
            make_run("probe:role", "observed-run", claimed_at="2026-08-13T10:00:00Z",
                     interpretation_outcome="observations"),
        ]
        state = build_state(Path(tmp), runs,
                            {"clean-run": ("clean", 0), "observed-run": ("observations", 2)})
        for record in runs:
            record["report_path"] = report_path(state, record["run_id"])
        build_state(Path(tmp), runs,
                    {"clean-run": ("clean", 0), "observed-run": ("observations", 2)})

        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("clean and observed reports")

        by_id = {r["run_id"]: r for r in result["runs"]}
        check_equal([r["run_id"] for r in result["runs"]], ["observed-run", "clean-run"],
                    "runs are ordered newest-claimed first")
        check_equal(by_id["clean-run"]["observations"], evidence.OBSERVATIONS_NONE,
                    "a clean run records no observations")
        check_equal(by_id["clean-run"]["report"]["observation_count"], 0,
                    "a clean report has zero OBS sections")
        check_equal(by_id["clean-run"]["report"]["status"], evidence.REPORT_AVAILABLE,
                    "a clean report is readable")
        check_equal(by_id["observed-run"]["observations"], evidence.OBSERVATIONS_RECORDED,
                    "an observed run records observations")
        check_equal(by_id["observed-run"]["report"]["observation_count"], 2,
                    "both OBS sections are counted")
        check_equal(by_id["observed-run"]["report"]["interpretation_status"], "observations",
                    "the report's own interpretation status is surfaced")
        check_equal(result["diagnostics"], [], "readable reports produce no diagnostic")


def test_incomplete_run_reports_unavailable_not_false() -> None:
    """An active or partially recorded run is surfaced, with nulls."""
    with tempfile.TemporaryDirectory() as tmp:
        active = make_run(
            "probe:role", "active-run", status="running",
            execution_status="not-run", interpretation_outcome="pending",
            completed_at=None, elapsed_seconds=None, test_exit_code=None,
        )
        del active["completed_at"], active["elapsed_seconds"], active["test_exit_code"]
        legacy = make_run("probe:role", "legacy-run", claimed_at="2026-08-01T00:00:00Z")
        for field in ("execution_status", "interpretation_outcome", "elapsed_seconds",
                      "revision_subject", "test_exit_code"):
            del legacy[field]
        state = build_state(Path(tmp), [active, legacy], {})
        active["report_path"] = report_path(state, "active-run")   # not written yet
        build_state(Path(tmp), [active, legacy], {})

        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("incomplete runs")

        by_id = {r["run_id"]: r for r in result["runs"]}
        run = by_id["active-run"]
        check_equal(run["run_state"], "running", "an active run keeps its state")
        check_equal(run["execution_status"], "not-run", "its mechanical status is surfaced")
        check_equal(run["duration_seconds"], None, "an unrecorded duration is None")
        check_equal(run["exit_code"], None, "an unrecorded exit code is None")
        check_equal(run["observations"], evidence.OBSERVATIONS_UNAVAILABLE,
                    "a pending interpretation is unavailable, not 'none'")
        check_equal(run["report"]["status"], evidence.REPORT_ABSENT,
                    "a report that does not exist yet is absent")

        old = by_id["legacy-run"]
        check_equal(old["execution_status"], None, "a missing mechanical status is None")
        check_equal(old["exit_code"], None, "a missing exit code is None")
        check_equal(old["duration_seconds"], None, "a missing duration is None")
        check_equal(old["tested_commit"], active["revision"],
                    "a legacy record still reports its provenance")
        check_equal(old["observations"], evidence.OBSERVATIONS_UNAVAILABLE,
                    "a missing interpretation is unavailable")
        check_equal(old["report"]["status"], evidence.REPORT_NOT_RECORDED,
                    "a record with no report_path records no report")
        check_equal(result["diagnostics"], [],
                    "an incomplete record is data, not damage")


def test_mechanical_outcome_is_not_inferred_from_interpretation() -> None:
    """Execution status comes from the registry, never from the report."""
    with tempfile.TemporaryDirectory() as tmp:
        failed = make_run("probe:role", "failed-but-clean-report",
                          execution_status="failed", test_exit_code=1,
                          interpretation_outcome="observations")
        state = build_state(Path(tmp), [failed],
                            {"failed-but-clean-report": ("clean", 0)})
        failed["report_path"] = report_path(state, "failed-but-clean-report")
        build_state(Path(tmp), [failed], {"failed-but-clean-report": ("clean", 0)})

        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("mechanical vs interpreted")

        run = result["runs"][0]
        check_equal(run["execution_status"], "failed",
                    "the mechanical outcome is the registry's, not the report's")
        check_equal(run["exit_code"], 1, "the recorded exit code is surfaced")
        check_equal(run["report"]["interpretation_status"], "clean",
                    "the report's disagreeing interpretation is reported beside it")
        check_equal(run["observations"], evidence.OBSERVATIONS_RECORDED,
                    "the registry's interpretation drives observation status")


def test_missing_and_malformed_reports_are_non_fatal() -> None:
    """Damaged report state diagnoses; it never fails or drops the run."""
    with tempfile.TemporaryDirectory() as tmp:
        runs = [
            make_run("probe:role", "unreadable-run", claimed_at="2026-08-14T00:00:00Z"),
            make_run("probe:role", "headless-run", claimed_at="2026-08-13T00:00:00Z"),
        ]
        state = build_state(Path(tmp), runs, {"headless-run": ("clean", 0)})
        for record in runs:
            record["report_path"] = report_path(state, record["run_id"])
        build_state(Path(tmp), runs, {"headless-run": ("clean", 0)})

        # A report with no frontmatter at all, and one that is not UTF-8.
        Path(report_path(state, "headless-run")).write_text(
            "# Test result\n\n### OBS-001 — one\n", encoding="utf-8")
        Path(report_path(state, "unreadable-run")).write_bytes(b"\xff\xfe\x00 not utf-8")

        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("damaged reports")

        by_id = {r["run_id"]: r for r in result["runs"]}
        check_equal(len(result["runs"]), 2, "both runs are still reported")
        check_equal(by_id["unreadable-run"]["report"]["status"], evidence.REPORT_UNREADABLE,
                    "a non-decodable report is unreadable")
        check_equal(by_id["unreadable-run"]["execution_status"], "passed",
                    "a damaged report does not disturb the mechanical fields")
        check_equal(by_id["headless-run"]["report"]["status"], evidence.REPORT_AVAILABLE,
                    "a frontmatter-less report is still read")
        check_equal(by_id["headless-run"]["report"]["observation_count"], 1,
                    "its OBS section is still counted")
        check_equal(by_id["headless-run"]["report"]["interpretation_status"], None,
                    "it carries no interpretation status")
        check(any("unreadable-run" in d for d in result["diagnostics"]),
              "the unreadable report is diagnosed", str(result["diagnostics"]))
        check(any("frontmatter" in d for d in result["diagnostics"]),
              "the frontmatter-less report is diagnosed", str(result["diagnostics"]))


def test_an_existing_non_regular_report_is_damage_not_absence() -> None:
    """A path that EXISTS but is not a regular file is diagnosed.

    A genuinely missing report is data — the run has not written it yet,
    or it was cleaned up. A directory (or socket, or device) sitting
    where a `*.test-result.md` file belongs is damaged external state,
    and damage is non-fatal but never silent.
    """
    with tempfile.TemporaryDirectory() as tmp:
        runs = [
            make_run("probe:role", "directory-run", claimed_at="2026-08-15T00:00:00Z"),
            make_run("probe:role", "dangling-run", claimed_at="2026-08-14T00:00:00Z"),
        ]
        state = build_state(Path(tmp), runs, {})
        reports = state / evidence.REPORTS_DIRNAME

        # A DIRECTORY named exactly like a report.
        (reports / ("directory-run" + evidence.REPORT_SUFFIX)).mkdir()
        # A symlink inside reports/ whose target, also inside reports/,
        # does not exist: in scope, but genuinely not there.
        os.symlink(reports / ("missing" + evidence.REPORT_SUFFIX),
                   reports / ("dangling-run" + evidence.REPORT_SUFFIX))

        for record in runs:
            record["report_path"] = report_path(state, record["run_id"])
        document = json.loads((state / evidence.REGISTRY_FILENAME).read_text())
        document["runs"] = runs
        (state / evidence.REGISTRY_FILENAME).write_text(json.dumps(document))

        with NonInteraction(state) as guard:
            with RecordReads() as reads:
                result = read(state, "role")
            guard.assert_untouched("non-regular report")

        by_id = {r["run_id"]: r for r in result["runs"]}
        check_equal(by_id["directory-run"]["report"]["status"], evidence.REPORT_UNREADABLE,
                    "a directory where a report belongs is unreadable, not absent")
        check_equal(by_id["directory-run"]["report"]["observation_count"], None,
                    "it contributes no observation count")
        check_equal(by_id["directory-run"]["execution_status"], "passed",
                    "the run's mechanical fields survive the damage")
        check(any("directory-run" in d and "not a regular file" in d
                  for d in result["diagnostics"]),
              "the non-regular report is diagnosed", str(result["diagnostics"]))

        check_equal(by_id["dangling-run"]["report"]["status"], evidence.REPORT_ABSENT,
                    "an in-scope path that is simply not there is absent")
        check(not any("dangling-run" in d for d in result["diagnostics"]),
              "absence is not diagnosed", str(result["diagnostics"]))
        check_equal(len(result["diagnostics"]), 1,
                    "exactly one diagnostic, for the damage alone")
        check(not any(p.name.endswith(evidence.REPORT_SUFFIX) for p in reads.paths),
              "neither non-file path is opened for reading",
              str([str(p) for p in reads.paths]))


#: This owner's cases, in the order the aggregate has always run them.
CASES = (
    test_clean_and_observed_reports,
    test_incomplete_run_reports_unavailable_not_false,
    test_mechanical_outcome_is_not_inferred_from_interpretation,
    test_missing_and_malformed_reports_are_non_fatal,
    test_an_existing_non_regular_report_is_damage_not_absence,
)
