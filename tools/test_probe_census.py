#!/usr/bin/env python3
"""Focused self-test for the probe census record and its writer (#1428).

Deterministic, engine-free, GPU-free and offline: every case runs
against synthetic documents in a throwaway temporary tree. Nothing here
boots an engine, runs a registered probe, touches the developer's real
`docs-wip` worktree, or shells out to anything but `git` (to build a
two-worktree scratch repository the CLI cases can resolve) and this same
interpreter (for the independent-process contention case).

The real `tools/probe_census.py` is imported and driven — with
`probe_runner_registry.PROBES`, `probe_engine.REPO_ROOT`, `ci_probes.CI_ELIGIBLE` and
`probe_flake.PROTOCOL_PROBES` pointed at a synthetic registry — so this
exercises the shipped code paths rather than a copy.

#1492 added the declared schema, so shape, required-field, closure,
enum, length, range and finite-number validation ARE covered, by
`test_declared_schema` and `test_missing_dependency` driving the real
checked-in `tools/probe_census_schema.json`. #1493 added the CROSS-FIELD
invariants, covered by `test_cross_field_invariants`, which gives every
rule a rejecting fixture that is still schema-valid, the legitimate
retention flows it must not over-reject, and a mutation check that lifts
that one rule out of the production rule set. The safety promise every
case shares is a CONTROLLED refusal — no traceback, authoritative bytes
unchanged — rather than the discovery of every possible corruption in a
hand-edited document.

Usage:
  python3 tools/test_probe_census.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import copy
import datetime
import itertools
import json
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
import probe_protocol  # type: ignore  # noqa: E402
import probe_engine  # type: ignore  # noqa: E402
import probe_runner_registry  # type: ignore  # noqa: E402

import selftest  # noqa: E402
from selftest import FAILURES, expect  # noqa: E402


COMMIT_A = "a" * 40
COMMIT_B = "b" * 40


def expect_refusal_kind(kind, call, msg: str) -> None:
    """`call` raises exactly `kind`, not something uncontrolled."""
    try:
        call()
    except kind:
        expect(True, msg)
        return
    except Exception as error:  # noqa: BLE001
        expect(False, f"{msg} (raised {type(error).__name__}: {error})")
        return
    expect(False, f"{msg} (nothing was raised)")


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


def v1_document() -> dict:
    """A `probe-census/v1` seed exactly as #1425 writes one."""
    return {
        "schema": "probe-census/v1",
        "probes": [
            {"key": "alpha", "script": "alpha_probe.py",
             "classification": "manual-only", "protocol": "legacy"},
            {"key": "beta", "script": "beta_probe.py",
             "classification": "ci-eligible", "protocol": "probe-result/v1"},
        ],
    }


def stored_v2_document() -> dict:
    """A `probe-census/v2` census exactly as #1428 wrote one.

    Six-field records, no `claims`. This is what `--seed` migrates FROM,
    so it is spelled out here rather than derived from the current
    `empty_census()`: deriving it would silently start testing the
    current shape the moment the record grows another field.
    """
    return {
        "schema": "probe-census/v2",
        "probes": [{
            "key": "alpha", "script": "alpha_probe.py",
            "classification": "manual-only", "protocol": "legacy",
            "census": {
                "acceptable_failures": 2,
                "acceptable_failures_justification": "two known races",
                "estimated_worst_case_seconds": 480,
                "current": None,
                "history": [],
                "attempts": [],
            },
        }],
    }


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


def sample_record(mark: str, commit: str = COMMIT_A) -> dict:
    """A schema-valid durable sample, tagged by its retained artifact.

    Fixtures used to stand a sample in as `{"tag": ...}`. Since #1492
    declared the record's shape, stored state has to BE a sample — so
    these come from the real summarizer and carry one distinguishing
    value each.
    """
    record = probe_census.summarize_sample(result_document(commit=commit))
    record["retained_artifacts"] = [f"/tmp/artifacts/{mark}"]
    return record


def attempt_record(mark: str, commit: str = COMMIT_A) -> dict:
    """A schema-valid durable attempt, tagged by its error text."""
    record = probe_census.summarize_attempt(
        result_document(commit=commit), True)
    record["error"] = mark
    return record


def seeded(path: Path) -> dict:
    """A fresh v2 census on disk, from the synthetic registry."""
    return probe_census.ensure_document(path)


def unchanged(path: Path, before: bytes, msg: str) -> None:
    expect(path.read_bytes() == before, msg)


def staging_residue(directory: Path) -> list[str]:
    """Staging files by the writer's own rule — the lock is not one."""
    return sorted(p.name for p in directory.iterdir()
                  if p.name.startswith(probe_census.STAGING_PREFIX)
                  and p.name.endswith(probe_census.STAGING_SUFFIX))


# ==========================================================================
def test_record_shape() -> None:
    print("\n-- the record, and byte-stable serialization --")
    empty = probe_census.empty_census()
    expect(empty == {
        "acceptable_failures": 0,
        "acceptable_failures_justification": None,
        "estimated_worst_case_seconds": None,
        "current": None,
        "history": [],
        "attempts": [],
        "claims": [],
        "outcomes": [],
        "deferred": None,
    }, "an empty census record is exactly the nine specified fields")
    expect(empty["acceptable_failures"] == 0
           and empty["acceptable_failures_justification"] is None,
           "a fresh record starts at X=0 with no justification: it must pass "
           "every run until someone writes down why not (#1430)")
    expect(probe_census.empty_census() is not empty
           and probe_census.empty_census()["history"] is not empty["history"],
           "each empty record is a fresh object, never a shared default")

    with registry(ci_eligible={"beta"}, protocol={"beta": "probe-result/v1"}):
        document = probe_census.build_manifest()
        expect(document["schema"] == "probe-census/v5",
               "a freshly built census is probe-census/v5")
        expect([row["key"] for row in document["probes"]]
               == ["alpha", "beta", "gamma"],
               "rows are built in live registry order")
        expect(all(row["census"] == probe_census.empty_census()
                   for row in document["probes"]),
               "every row carries the exact empty census record")
        expect(all(set(row) == {"key", "script", "classification",
                                "protocol", "census"}
                   for row in document["probes"]),
               "a row is exactly the four inventory fields plus its census")

        text = probe_census.render_manifest(document)
        expect(text.endswith("}\n") and text.count("\n") > 1,
               "the serialization ends with exactly one trailing newline")
        expect(text == probe_census.render_manifest(json.loads(text)),
               "the document round-trips byte-for-byte")
        expect(probe_census.render_manifest(json.loads(text))
               == probe_census.render_manifest(
                   json.loads(json.dumps(json.loads(text)))),
               "serialization is a pure function of content, not key order")
        shuffled = {"probes": document["probes"], "schema": document["schema"]}
        expect(probe_census.render_manifest(shuffled) == text,
               "a document built with its top-level keys in another order "
               "serializes identically")

    expect_refusal(lambda: probe_census.render_manifest({"probes": {1, 2}}),
                   "an unserializable candidate is a controlled refusal",
                   "not serializable")


# ==========================================================================
def test_migration() -> None:
    print("\n-- lossless v1 -> v5 migration --")
    with registry(ci_eligible={"beta"}, protocol={"beta": "probe-result/v1"}):
        source = v1_document()
        # An inventory field this tool does not know about must survive.
        source["probes"][0]["note"] = "kept by migration"
        original = copy.deepcopy(source)
        migrated = probe_census.migrate_document(source)

        expect(source == original,
               "migration does not mutate the document it was given")
        expect(migrated["schema"] == "probe-census/v5",
               "the migrated document is probe-census/v5")
        expect([row["key"] for row in migrated["probes"]] == ["alpha", "beta"],
               "row order is preserved exactly")
        expect(len(migrated["probes"]) == 2,
               "migration does not append the newly registered `gamma`; it is "
               "migration, not reconciliation")
        for before, after in zip(original["probes"], migrated["probes"]):
            expect(all(after[k] == v for k, v in before.items()),
                   f"every existing value of {before['key']!r} survives, "
                   f"including unknown fields")
        expect(migrated["probes"][0]["note"] == "kept by migration",
               "an unknown inventory field is carried through")
        expect(all(row["census"] == probe_census.empty_census()
                   for row in migrated["probes"]),
               "every migrated row gains the exact empty census record")

        # v2 in, v2 out, untouched.
        with_data = copy.deepcopy(migrated)
        with_data["probes"][0]["census"]["acceptable_failures"] = 3
        expect(probe_census.migrate_document(with_data) == with_data,
               "migrating an already-v2 document changes nothing")

        # A v2 row with no census is NOT silently repaired here: it is
        # a declared-schema violation for the validator to report, and
        # inserting an empty record would erase the evidence.
        damaged = copy.deepcopy(migrated)
        del damaged["probes"][1]["census"]
        expect("census" not in probe_census.migrate_document(
            damaged)["probes"][1],
            "a v2 row missing its census record is left alone, not repaired")

        expect_refusal(
            lambda: probe_census.migrate_document({"schema": "probe-census/v9",
                                                   "probes": []}),
            "an unknown schema is a controlled refusal",
            "probe-census/v9")
        expect_refusal(lambda: probe_census.migrate_document([]),
                       "a non-object census is a controlled refusal",
                       "must be a JSON object")
        expect_refusal(
            lambda: probe_census.migrate_document({"schema": "probe-census/v1",
                                                   "probes": "no"}),
            "a non-list `probes` is a controlled refusal", "must be a list")
        expect_refusal(
            lambda: probe_census.migrate_document({"schema": "probe-census/v1",
                                                   "probes": [7]}),
            "a non-object row is a controlled refusal", "entry 0")


# ==========================================================================
def test_seed_and_noop() -> None:
    print("\n-- fresh seed, and a drift-free --seed changing nothing --")
    with registry(), scratch() as root:
        path = root / "docs" / "probe_census.json"
        document = seeded(path)
        expect(path.exists(), "an absent census is created")
        expect(document["schema"] == "probe-census/v5",
               "the fresh census is probe-census/v5")
        expect([row["key"] for row in document["probes"]]
               == ["alpha", "beta", "gamma"],
               "the fresh census lists the live registry in order")

        before = path.read_bytes()
        stamp = path.stat()
        again = probe_census.ensure_document(path)
        expect(again == document, "a second seed produces the same document")
        unchanged(path, before, "a drift-free --seed leaves the bytes alone")
        after = path.stat()
        expect((after.st_ino, after.st_mtime_ns)
               == (stamp.st_ino, stamp.st_mtime_ns),
               "a drift-free --seed does not even rewrite the file")

        # Seeding a v1 document migrates in place rather than regenerating.
        legacy = root / "legacy.json"
        legacy.write_text(json.dumps(v1_document()), encoding="utf-8")
        migrated = probe_census.ensure_document(legacy)
        expect(migrated["schema"] == "probe-census/v5",
               "seeding a v1 census migrates it")
        expect([row["key"] for row in migrated["probes"]]
               == ["alpha", "beta", "gamma"],
               "the migrated v1 rows keep their order and gain the new probe")


# ==========================================================================
def test_reconciliation() -> None:
    print("\n-- inventory reconciliation --")
    with scratch() as root:
        path = root / "probe_census.json"
        # A census written when `retired` was registered and `gamma` was
        # not, with real accumulated data on two rows.
        document = {
            "schema": probe_census.CENSUS_SCHEMA,
            "probes": [
                {"key": "retired", "script": "retired_probe.py",
                 "classification": "manual-only", "protocol": "legacy",
                 "census": {**probe_census.empty_census(),
                            "acceptable_failures": 1,
                            "acceptable_failures_justification": "one race",
                            "current": {"commit_sha": COMMIT_A,
                                        "samples": [sample_record("kept")]},
                            "attempts": [attempt_record("retired-attempt")]}},
                {"key": "alpha", "script": "stale_name.py",
                 "classification": "manual-only", "protocol": "legacy",
                 "census": {**probe_census.empty_census(),
                            "acceptable_failures": 2,
                            "acceptable_failures_justification": "two races",
                            "estimated_worst_case_seconds": 480,
                            "current": {"commit_sha": COMMIT_A,
                                        "samples": [sample_record("alpha-1")]},
                            "history": [{"commit_sha": COMMIT_B,
                                         "samples": [sample_record("old", COMMIT_B)]}],
                            "attempts": [attempt_record("attempt-1"),
                                         attempt_record("attempt-2",
                                                        COMMIT_B)]}},
                {"key": "beta", "script": "beta_probe.py",
                 "classification": "manual-only", "protocol": "legacy",
                 "census": {**probe_census.empty_census(),
                            "acceptable_failures": 0,
                            "current": {"commit_sha": COMMIT_A,
                                        "samples": [sample_record("beta-1")]},
                            "attempts": [attempt_record("beta-attempt")]}},
            ],
        }
        path.write_text(json.dumps(document), encoding="utf-8")

        # `beta` has since been promoted to CI eligibility; `alpha` has a
        # corrected script name and a migrated protocol; `gamma` is new.
        with registry(ci_eligible={"beta"},
                      protocol={"alpha": "probe-result/v1"}):
            result = probe_census.ensure_document(path)

        rows = {row["key"]: row for row in result["probes"]}
        expect([row["key"] for row in result["probes"]]
               == ["retired", "alpha", "beta", "gamma"],
               "existing rows keep their order and new probes are appended")
        expect(rows["retired"] == document["probes"][0],
               "a row for a probe that left the registry is retained "
               "untouched, for a person to dispose of")
        expect(rows["alpha"]["script"] == "alpha_probe.py"
               and rows["alpha"]["protocol"] == "probe-result/v1",
               "a still-registered row's inventory metadata is refreshed")
        expect(rows["alpha"]["census"] == document["probes"][1]["census"],
               "refreshing inventory metadata preserves the census exactly")
        expect(rows["gamma"]["census"] == probe_census.empty_census()
               and rows["gamma"]["classification"] == "manual-only",
               "a newly registered probe is appended with an empty record")

        promoted = rows["beta"]["census"]
        expect(rows["beta"]["classification"] == "ci-eligible",
               "promotion is read from tools/ci_probes.py")
        expect(promoted["current"] is None,
               "promotion clears the current manual-only cohort")
        expect(promoted["history"] == [{"commit_sha": COMMIT_A,
                                        "samples": [sample_record("beta-1")]}],
               "the promoted cohort is ARCHIVED into history, never dropped")
        expect(promoted["attempts"] == [attempt_record("beta-attempt")]
               and promoted["acceptable_failures"] == 0,
               "promotion keeps attempts and policy fields")

        # The reverse transition refreshes the classification only.
        with registry(ci_eligible=set(), protocol={"alpha": "probe-result/v1"}):
            back = probe_census.ensure_document(path)
        beta = [row for row in back["probes"] if row["key"] == "beta"][0]
        expect(beta["classification"] == "manual-only",
               "a probe falling back to manual-only is reclassified")
        expect(beta["census"] == promoted,
               "the reverse transition performs NO cohort surgery")


# ==========================================================================
def test_ingest_accepted() -> None:
    print("\n-- accepted measurement ingestion --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)

        probe_census.record_result(path, result_document())
        document = json.loads(path.read_text(encoding="utf-8"))
        rows = {row["key"]: row for row in document["probes"]}
        census = rows["alpha"]["census"]
        expect(census["current"]["commit_sha"] == COMMIT_A
               and len(census["current"]["samples"]) == 1,
               "an accepted measurement opens the current cohort with one sample")
        expect(len(census["attempts"]) == 1
               and census["attempts"][0]["accepted"] is True
               and census["attempts"][0]["status"] == "ok",
               "it also appends exactly one accepted attempt")
        expect(rows["beta"]["census"] == probe_census.empty_census()
               and rows["gamma"]["census"] == probe_census.empty_census(),
               "no unrelated row is touched")

        sample = census["current"]["samples"][0]
        expect(set(sample) == {
            "timestamp_utc", "commit_sha", "requested_runs", "completed_runs",
            "runs", "check_counts", "failure_count", "failure_rate",
            "timeout_count", "worst_elapsed_seconds", "total_elapsed_seconds",
            "rts_capabilities", "peak_concurrency", "retained_artifacts",
        }, "the durable sample is exactly the specified fields")
        expect(all(set(run) == {"index", "outcome", "elapsed_seconds",
                                "artifact_dir"} for run in sample["runs"]),
               "each durable run is exactly the four specified fields")
        expect(set(census["attempts"][0]) == {
            "timestamp_utc", "commit_sha", "status", "accepted",
            "requested_runs", "completed_runs", "error", "retained_artifacts",
        }, "the durable attempt is exactly the specified fields")
        expect(sample["retained_artifacts"] == ["/tmp/artifacts/run-002"],
               "artifact REFERENCES are retained")

        # Producer-only fields and every raw stream stay out.
        serialized = json.dumps(document)
        for leaked in ("port", "error_run", "artifact_root", "invocation_dir",
                       "the first check"):
            expect(leaked not in serialized,
                   f"the producer-only {leaked!r} never enters the census")
        expect("checks" not in sample and
               all("checks" not in run for run in sample["runs"]),
               "the per-run check map and descriptor labels are dropped")
        expect(sample["check_counts"]
               == {"first": {"PASS": 2, "FAIL": 0, "MISSING": 0},
                   "second": {"PASS": 1, "FAIL": 1, "MISSING": 0}},
               "the summarized per-check tallies are kept")

        # Same commit appends; a different commit archives first.
        probe_census.record_result(path, result_document())
        census = json.loads(path.read_text(encoding="utf-8"))["probes"][0]["census"]
        expect(len(census["current"]["samples"]) == 2 and census["history"] == [],
               "a second measurement of the SAME commit appends to the cohort")
        expect(len(census["attempts"]) == 2,
               "--record is deliberately not idempotent: the same document "
               "twice appends twice")

        probe_census.record_result(path, result_document(commit=COMMIT_B))
        census = json.loads(path.read_text(encoding="utf-8"))["probes"][0]["census"]
        expect(census["current"]["commit_sha"] == COMMIT_B
               and len(census["current"]["samples"]) == 1,
               "a different commit opens a new current cohort")
        expect(len(census["history"]) == 1
               and census["history"][0]["commit_sha"] == COMMIT_A
               and len(census["history"][0]["samples"]) == 2,
               "the COMPLETE prior cohort is archived, not truncated")


# ==========================================================================
def test_ci_eligible_takes_no_measurement() -> None:
    """#1431: "no further samples" is a STORAGE invariant, not a display one.

    `probe_flake.resolve_probe` refuses to RUN a CI-eligible probe, but
    a result document outlives its run: one measured before a promotion
    is still a well-formed, schema-valid document afterwards.
    """
    print("\n-- a CI-eligible probe takes no census measurement --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        probe_census.record_result(path, result_document())
        before = path.read_bytes()

        # Eligibility is read LIVE, so the same document that was just
        # accepted is refused once the registry promotes its probe --
        # and the stored row's own (not yet reconciled) classification
        # is not what decides it.
        with registry(ci_eligible={"alpha"}):
            stored = json.loads(before.decode("utf-8"))["probes"][0]
            expect(stored["classification"] == "manual-only",
                   "the stored row still says manual-only before --seed "
                   "reconciles it")
            expect_refusal(
                lambda: probe_census.record_result(path, result_document()),
                "an accepted measurement for a live CI-eligible probe is "
                "refused",
                "alpha", "CI-eligible")
            unchanged(path, before, "and nothing at all is written")
            expect_refusal(
                lambda: probe_census.record_result(
                    path, result_document(status="harness-error")),
                "so is a harness error for one: nothing about a promoted "
                "probe enters the append-only record",
                "alpha", "CI-eligible")
            unchanged(path, before, "again writing nothing")

        probe_census.record_result(path, result_document())
        expect(path.read_bytes() != before,
               "and the same document is accepted again once the probe is "
               "manual-only, so the refusal is live eligibility rather than "
               "a property of the document")


# ==========================================================================
def test_ingest_harness_error() -> None:
    print("\n-- harness-error ingestion --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        probe_census.record_result(path, result_document())
        with_sample = json.loads(path.read_text(encoding="utf-8"))

        probe_census.record_result(
            path, result_document(status="harness-error", commit=COMMIT_B))
        census = json.loads(path.read_text(encoding="utf-8"))["probes"][0]["census"]
        before = with_sample["probes"][0]["census"]
        expect(len(census["attempts"]) == 2
               and census["attempts"][1]["accepted"] is False
               and census["attempts"][1]["status"] == "harness-error",
               "a well-formed harness error appends one non-accepted attempt")
        expect(census["attempts"][1]["error"]
               == "run 2 emitted a duplicate event",
               "the attempt carries the harness's own diagnostic")
        expect(census["current"] == before["current"],
               "it creates no sample and does not touch the current cohort")
        expect(census["history"] == before["history"],
               "and it archives nothing")


# ==========================================================================
def test_policy() -> None:
    print("\n-- policy set, keep and explicit clear --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        probe_census.record_result(path, result_document())
        measured = json.loads(path.read_text(encoding="utf-8"))["probes"][0]["census"]

        def census_of(key="alpha"):
            rows = json.loads(path.read_text(encoding="utf-8"))["probes"]
            return {row["key"]: row["census"] for row in rows}[key]

        probe_census.record_policy(path, "alpha", acceptable_failures=2,
                                   justification="two engine-side races")
        record = census_of()
        expect(record["acceptable_failures"] == 2
               and record["acceptable_failures_justification"]
               == "two engine-side races",
               "--set-acceptable-failures stores X and its justification")
        expect(record["estimated_worst_case_seconds"] is None,
               "an unrelated policy field is untouched")
        expect(record["current"] == measured["current"]
               and record["attempts"] == measured["attempts"],
               "a policy update leaves every measurement alone")

        probe_census.record_policy(path, "alpha", acceptable_failures=5)
        record = census_of()
        expect(record["acceptable_failures"] == 5
               and record["acceptable_failures_justification"]
               == "two engine-side races",
               "omitting --justification LEAVES the existing one unchanged")

        probe_census.record_policy(path, "alpha", justification=None,
                                   acceptable_failures=0)
        expect(census_of()["acceptable_failures_justification"] is None
               and census_of()["acceptable_failures"] == 0,
               "an explicit clear removes the justification only")

        probe_census.record_policy(path, "alpha", estimate=480)
        record = census_of()
        expect(record["estimated_worst_case_seconds"] == 480
               and record["acceptable_failures"] == 0,
               "--set-estimate stores the estimate and keeps X")

        probe_census.record_policy(path, "alpha",
                                   justification="restored", acceptable_failures=5)
        probe_census.record_policy(path, "alpha", acceptable_failures=1)
        record = census_of()
        expect(record["acceptable_failures"] == 1
               and record["acceptable_failures_justification"] == "restored",
               "lowering X leaves its justification alone (#1479)")
        expect(record["estimated_worst_case_seconds"] == 480,
               "lowering X leaves the estimate alone")
        probe_census.record_policy(path, "alpha", acceptable_failures=0)
        expect(census_of()["acceptable_failures_justification"] == "restored",
               "and X=0 may keep a justification: only an explicit clear "
               "removes one")

        probe_census.record_policy(path, "alpha", estimate=None)
        expect(census_of()["estimated_worst_case_seconds"] is None,
               "--set-estimate none clears the estimate")
        expect(census_of()["acceptable_failures"] == 0,
               "clearing the estimate leaves X alone")
        expect(census_of()["current"] == measured["current"],
               "no policy command ever disturbed the measurements")
        expect(census_of("beta") == probe_census.empty_census(),
               "no policy command ever disturbed an unrelated row")

        before = path.read_bytes()
        expect_refusal(lambda: probe_census.record_policy(
            path, "nosuch", acceptable_failures=1),
            "a --probe naming no census row is refused",
            "nosuch", "no census row")
        unchanged(path, before, "a refused policy update changes no bytes")

        # #1430 closed the nullable X #1428 staged. The library refuses
        # it through the same `update` funnel the CLI does, so the
        # authoritative bytes survive either route.
        expect_refusal(lambda: probe_census.record_policy(
            path, "alpha", acceptable_failures=None),
            "storing a null X is refused, naming --seed as the repair",
            "no acceptable-failure policy", "--seed")
        unchanged(path, before, "...and that refusal changed no bytes")


# ==========================================================================
def _legacy_policy_census() -> dict:
    """A `probe-census/v2` census written BEFORE #1430 chose the policy.

    Every row's X is still the null #1428 staged, and two rows carry
    real accumulated data, so the initialization has something it could
    plausibly damage. It is a genuine v2 document — six-field records,
    no `claims` and no `outcomes` — so `--seed` here performs the real
    migration as well as the policy initialization, which is exactly the
    pairing an operator with an old census meets.
    """
    def row(key, census):
        return {"key": key, "script": f"{key}_probe.py",
                "classification": "manual-only", "protocol": "legacy",
                "census": {field: value for field, value in census.items()
                           if field not in ("claims", "outcomes", "deferred")}}

    return {
        "schema": probe_census.RECORD_SCHEMA,
        "probes": [
            row("alpha", {"acceptable_failures": None,
                          "acceptable_failures_justification": "kept text",
                          "estimated_worst_case_seconds": 480,
                          "current": {"commit_sha": COMMIT_A,
                                      "samples": [sample_record("alpha-1")]},
                          "history": [{"commit_sha": COMMIT_B,
                                       "samples": [sample_record("old",
                                                                 COMMIT_B)]}],
                          "attempts": [attempt_record("a-1", COMMIT_B),
                                       attempt_record("a-2")]}),
            row("beta", {**probe_census.empty_census(),
                         "acceptable_failures": 3,
                         "acceptable_failures_justification": "three races"}),
            row("retired", {**probe_census.empty_census(),
                            "acceptable_failures": None}),
        ],
    }


def test_acceptable_failure_policy_defaults() -> None:
    """#1430: every probe has an X, X=0 is the default, and only null moves."""
    print("\n-- X defaults to 0, and only an UNSET X is initialized --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        document = seeded(path)
        expect(all(row["census"]["acceptable_failures"] == 0
                   for row in document["probes"]),
               "a fresh seed gives every registered probe X=0")
        expect(all(row["census"]["acceptable_failures_justification"] is None
                   for row in document["probes"]),
               "and none of them a justification, which only X>0 needs")

        # Coverage is derived from the LIVE registry, never a frozen
        # count: a probe registered after the census was written is
        # appended with the same default.
        legacy = root / "legacy.json"
        legacy.write_text(json.dumps(v1_document()), encoding="utf-8")
        migrated = probe_census.ensure_document(legacy)
        expect([row["key"] for row in migrated["probes"]]
               == [key for key, _s, _p in SYNTHETIC],
               "a v1 migration covers exactly the live registry")
        expect(all(row["census"]["acceptable_failures"] == 0
                   for row in migrated["probes"]),
               "every v1-migrated and newly appended row is seeded at X=0")

        extra = list(SYNTHETIC) + [("delta", "delta_probe.py", "a new probe")]
        with registry(probes=extra):
            grown = probe_census.ensure_document(path)
        expect({row["key"] for row in grown["probes"]}
               == {key for key, _s, _p in extra},
               "a probe registered later is appended by --seed")
        expect([row["census"]["acceptable_failures"]
                for row in grown["probes"]] == [0, 0, 0, 0],
               "and it arrives at X=0 like every other row")

    # An existing v2 census whose X is still null: ONLY that field moves.
    with registry(probes=[("alpha", "alpha_probe.py", "one"),
                          ("beta", "beta_probe.py", "two")]), scratch() as root:
        path = root / "probe_census.json"
        stored = _legacy_policy_census()
        path.write_text(json.dumps(stored), encoding="utf-8")
        result = probe_census.ensure_document(path)
        rows = {row["key"]: row["census"] for row in result["probes"]}

        expect(rows["alpha"]["acceptable_failures"] == 0,
               "--seed initializes an unset X to 0")
        expect(rows["alpha"]["acceptable_failures_justification"]
               == "kept text"
               and rows["alpha"]["estimated_worst_case_seconds"] == 480,
               "...and touches neither the justification nor the estimate")
        expect(rows["alpha"]["current"] == stored["probes"][0]["census"]["current"]
               and rows["alpha"]["history"]
               == stored["probes"][0]["census"]["history"]
               and rows["alpha"]["attempts"]
               == stored["probes"][0]["census"]["attempts"],
               "...and no cohort, sample or attempt at all")
        expect(rows["beta"] == {**stored["probes"][1]["census"],
                                "claims": [], "outcomes": [],
                                "deferred": None},
               "a row whose X is already set is left exactly as it was, "
               "apart from the empty claim/outcome logs and null deferral "
               "its migration adds")
        expect(rows["retired"]["acceptable_failures"] == 0,
               "a row whose probe left the registry is initialized too, so a "
               "legacy census can always be made policy-valid")

        # Idempotent, and a second seed is a genuine no-op.
        before = path.read_bytes()
        probe_census.ensure_document(path)
        unchanged(path, before, "a second --seed initializes nothing further")


def test_acceptable_failure_policy_rules() -> None:
    """The three rules, each with its own rejecting case."""
    print("\n-- X is bounded, justified above 0, and manual-only above 0 --")
    with registry(ci_eligible={"beta"}), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)

        probe_census.record_policy(path, "alpha", acceptable_failures=2,
                                   justification="two engine-side races")
        stored = json.loads(path.read_text(encoding="utf-8"))
        rows = {row["key"]: row["census"] for row in stored["probes"]}
        expect(rows["alpha"]["acceptable_failures"] == 2,
               "a justified tolerance on a manual-only probe is stored")
        before = path.read_bytes()

        # (1) the range, at both ends and just past the top.
        for value in (0, 9):
            probe_census.record_policy(path, "alpha",
                                       acceptable_failures=value,
                                       justification="stated")
            expect(json.loads(path.read_text(encoding="utf-8"))["probes"][0][
                       "census"]["acceptable_failures"] == value,
                   f"X={value} is inside the admissible range")
        before = path.read_bytes()
        for value, why, fragment in (
                (10, "X=10 would accept a probe that never passes", "10"),
                (-1, "a negative X is not a count", "-1"),
                (True, "a boolean X (`bool` is an `int` subclass)", "True"),
                (2.5, "a fractional X", "2.5"),
                ("3", "a string X", "'3'"),
                (None, "a null X", "no acceptable-failure policy")):
            expect_refusal(
                lambda v=value: probe_census.record_policy(
                    path, "alpha", acceptable_failures=v),
                f"storing {value!r} as X is refused ({why})", fragment)
            unchanged(path, before, f"...and wrote nothing ({value!r})")

        # (2) X>0 needs a reason, and whitespace is not one.
        probe_census.record_policy(path, "alpha", acceptable_failures=0,
                                   justification=None)
        before = path.read_bytes()
        for text, why in ((None, "a cleared justification"),
                          ("", "an empty justification"),
                          ("   \t\n ", "a whitespace-only justification")):
            expect_refusal(
                lambda t=text: probe_census.record_policy(
                    path, "alpha", acceptable_failures=1, justification=t),
                f"X=1 with {why} is refused",
                "no stated reason", "--justification")
            unchanged(path, before, f"...and wrote nothing ({why})")
        expect(probe_census.record_policy(
            path, "alpha", acceptable_failures=1,
            justification="one known race") == "alpha",
            "the same X with a real reason is accepted")

        # A stored reason satisfies it: a later X change need not resupply.
        probe_census.record_policy(path, "alpha", acceptable_failures=4)
        expect(json.loads(path.read_text(encoding="utf-8"))["probes"][0][
                   "census"]["acceptable_failures_justification"]
               == "one known race",
               "an X change reuses the STORED reason rather than demanding "
               "one again (#1479's independence survives)")

        # (3) tolerance is a manual-only concept.
        before = path.read_bytes()
        expect_refusal(
            lambda: probe_census.record_policy(
                path, "beta", acceptable_failures=1, justification="a race"),
            "a tolerance on a CI-eligible probe is refused",
            "ci-eligible", "manual-only concept")
        unchanged(path, before, "...and wrote nothing")
        probe_census.record_policy(path, "beta", acceptable_failures=0)
        expect(json.loads(path.read_text(encoding="utf-8"))["probes"][1][
                   "census"]["acceptable_failures"] == 0,
               "X=0 on a CI-eligible probe is exactly what it must be")


def test_acceptable_failure_policy_promotion() -> None:
    """A promotion may not silently erase a maintainer's tolerance."""
    print("\n-- promotion requires X to be reset first --")
    with scratch() as root:
        path = root / "probe_census.json"
        with registry():
            seeded(path)
            probe_census.record_policy(path, "beta", acceptable_failures=3,
                                       justification="three races")
            probe_census.record_result(path, result_document(probe="beta"))
        before = path.read_bytes()

        with registry(ci_eligible={"beta"}):
            expect_refusal(
                lambda: probe_census.ensure_document(path),
                "--seed refuses to promote a row that still holds a "
                "tolerance", "beta", "ci-eligible", "manual-only concept")
            unchanged(path, before,
                      "...and the maintainer's X survives the refusal")
            expect(json.loads(path.read_text(encoding="utf-8"))["probes"][1][
                       "census"]["acceptable_failures"] == 3,
                   "the stored tolerance really is still 3, not erased")

            # A row that is policy-invalid blocks every mutation, which
            # is what keeps it VISIBLE instead of quietly repaired.
            expect_refusal(
                lambda: probe_census.record_policy(
                    path, "alpha", acceptable_failures=0),
                "an unrelated policy update is blocked while it stands",
                "beta")

            # Resetting X is the one move that unblocks it, and the
            # promotion then archives the cohort as it always did.
            probe_census.record_policy(path, "beta", acceptable_failures=0)
            result = probe_census.ensure_document(path)
        beta = {row["key"]: row for row in result["probes"]}["beta"]
        expect(beta["classification"] == "ci-eligible"
               and beta["census"]["current"] is None
               and len(beta["census"]["history"]) == 1,
               "with X reset, promotion archives the cohort as usual")
        expect(beta["census"]["acceptable_failures_justification"]
               == "three races",
               "and the maintainer's written reason is still there to read")


def test_acceptable_failure_threshold() -> None:
    """Failures <= X is acceptable; failures > X is over tolerance."""
    print("\n-- the threshold, at X and at X+1 --")
    N = probe_census.POLICY_RUN_COUNT
    for x in range(probe_census.MIN_ACCEPTABLE_FAILURES,
                   probe_census.MAX_ACCEPTABLE_FAILURES + 1):
        expect(probe_census.tolerance_state(x, N, N, x)
               == probe_census.TOLERANCE_ACCEPTABLE,
               f"X={x}: exactly {x} failure(s) in {N} runs is acceptable")
        expect(probe_census.tolerance_state(x, N, N, x + 1)
               == probe_census.TOLERANCE_OVER,
               f"X={x}: {x + 1} failure(s) in {N} runs is over tolerance")
    expect(probe_census.tolerance_state(0, N, N, 0)
           == probe_census.TOLERANCE_ACCEPTABLE
           and probe_census.tolerance_state(0, N, N, 1)
           == probe_census.TOLERANCE_OVER,
           "X=0 means a clean sweep, and one failure breaches it")
    expect(probe_census.tolerance_state(1, N, N, 0)
           == probe_census.TOLERANCE_ACCEPTABLE
           and probe_census.tolerance_state(1, N, N, 1)
           == probe_census.TOLERANCE_ACCEPTABLE,
           "X=1 accepts both 10/10 and 9/10")

    # The basis is a COMPLETE fixed-N measurement, and nothing else.
    for runs, why in ((N - 1, "a shorter run"), (N + 1, "a longer run"),
                      (0, "no runs at all")):
        expect(probe_census.tolerance_state(0, runs, runs, 0)
               == probe_census.TOLERANCE_NOT_COMPARABLE,
               f"{why} is not classified against a policy stated out of {N}")
    expect(probe_census.tolerance_state(0, N, N - 1, 0)
           == probe_census.TOLERANCE_NOT_COMPARABLE,
           "and an INCOMPLETE ten-run measurement is not one either")
    for value, why in ((None, "a null X"), (True, "a boolean X"),
                       (2.5, "a fractional X"), (10, "an out-of-range X")):
        expect(probe_census.tolerance_state(value, N, N, 0)
               == probe_census.TOLERANCE_NOT_COMPARABLE,
               f"{why} classifies nothing")
    expect(probe_census.tolerance_state(1, N, N, None)
           == probe_census.TOLERANCE_NOT_COMPARABLE
           and probe_census.tolerance_state(1, N, N, -1)
           == probe_census.TOLERANCE_NOT_COMPARABLE,
           "an unusable failure count classifies nothing either")

    # The measurement it is asked about is ONE sample, never a cohort's
    # pooled totals: two five-run measurements are not a ten-run one,
    # and two ten-run measurements are not a twenty-run one.
    def sized(runs, failures, mark):
        return {"requested_runs": runs, "completed_runs": runs,
                "failure_count": failures, "retained_artifacts": [mark]}

    expect(probe_census.policy_sample({"samples": []}) is None
           and probe_census.policy_sample({}) is None
           and probe_census.policy_sample(None) is None,
           "a cohort with no samples has no policy measurement")
    expect(probe_census.policy_sample(
        {"samples": [sized(5, 0, "a"), sized(5, 3, "b")]}) is None,
        "two five-run samples do NOT add up to a ten-run measurement")
    picked = probe_census.policy_sample(
        {"samples": [sized(N, 0, "first"), sized(N, 4, "second")]})
    expect(picked is not None and picked["retained_artifacts"] == ["second"],
           "two ten-run samples stay comparable, and the LAST appended one "
           "is the current measurement")
    picked = probe_census.policy_sample(
        {"samples": [sized(N, 0, "ten"), sized(3, 0, "three")]})
    expect(picked is not None and picked["retained_artifacts"] == ["ten"],
           "a later odd-sized run does not hide the newest ten-run one")
    expect(probe_census.policy_sample(
        {"samples": [{"requested_runs": N, "completed_runs": N - 1,
                      "failure_count": 0}]}) is None,
        "an incomplete ten-run sample is not a policy measurement")
    expect(probe_census.policy_sample({"samples": [7, None]}) is None,
           "and a malformed sample is skipped rather than raised on")


def test_acceptable_failure_policy_cli() -> None:
    """`--validate` reports policy violations, and `--summary` shows X."""
    print("\n-- the policy through the CLI --")
    with registry(ci_eligible={"beta"}), cli_repo() as (_, path):
        cli("--seed")
        code, out, err = cli("--validate")
        expect(code == 0, "a freshly seeded census validates")

        stored = json.loads(path.read_text(encoding="utf-8"))
        stored["probes"][0]["census"]["acceptable_failures"] = 5
        stored["probes"][1]["census"]["acceptable_failures"] = 2
        stored["probes"][1]["census"][
            "acceptable_failures_justification"] = "a race"
        path.write_text(json.dumps(stored, indent=2) + "\n", encoding="utf-8")
        before = path.read_bytes()

        code, _, err = cli("--validate")
        expect(code == 1 and "alpha" in err and "no stated reason" in err,
               "--validate reports an unjustified tolerance")
        expect("beta" in err and "ci-eligible" in err,
               "--validate reports a tolerance on a CI-eligible probe in the "
               "SAME pass, rather than stopping at the first")
        unchanged(path, before, "--validate reads and never repairs")

        # A null X is reported with the repair that fixes it.
        stored["probes"][0]["census"]["acceptable_failures"] = None
        stored["probes"][1]["census"]["acceptable_failures"] = 0
        path.write_text(json.dumps(stored, indent=2) + "\n", encoding="utf-8")
        code, _, err = cli("--validate")
        expect(code == 1 and "no acceptable-failure policy" in err
               and "--seed" in err,
               "--validate names --seed as the repair for an unset X")
        code, _, _ = cli("--seed")
        expect(code == 0, "and --seed performs it")
        code, _, _ = cli("--validate")
        expect(code == 0, "after which the census validates")

        # --summary reports X and where the newest cohort sits against it.
        cli("--probe", "alpha", "--set-acceptable-failures", "1",
            "--justification", "one known race")
        code, out, _ = cli("--summary", "--json")
        rows = {row["key"]: row for row in json.loads(out)}
        expect(rows["alpha"]["acceptable_failures"] == 1
               and rows["alpha"]["tolerance"] == "not-comparable",
               "an unmeasured probe reports its X and no comparison")
        code, out, _ = cli("--summary")
        expect(code == 0 and "tolerance" in out and "not-comparable" in out,
               "the human table carries the policy columns")

    # End to end, through real ingested measurements: the classification
    # is ONE sample's, never the cohort's pooled totals.
    N = probe_census.POLICY_RUN_COUNT
    with registry(), scratch() as root:
        def tolerance(path):
            return summary_of(path)["tolerance"]

        # A single complete ten-run measurement, at X and at X+1.
        for failures, expected in ((1, "acceptable"), (2, "over-tolerance")):
            path = root / f"one-{failures}.json"
            seeded(path)
            probe_census.record_policy(path, "alpha", acceptable_failures=1,
                                       justification="one known race")
            probe_census.record_result(
                path, measurement(runs=N, failures=failures, age_days=1))
            expect(tolerance(path) == expected,
                   f"a single {N}-run measurement with {failures} failure(s) "
                   f"against X=1 is {expected}")

        # Split: two five-run measurements on one commit pool to ten
        # runs, and must NOT be read as a ten-run result.
        path = root / "split.json"
        seeded(path)
        probe_census.record_policy(path, "alpha", acceptable_failures=1,
                                   justification="one known race")
        probe_census.record_result(path, measurement(runs=5, failures=0,
                                                     age_days=2))
        probe_census.record_result(path, measurement(runs=5, failures=3,
                                                     age_days=1))
        expect(summary_of(path)["requested_runs"] == 2 * 5,
               "the cohort really does pool to ten runs")
        expect(tolerance(path) == "not-comparable",
               "...and two five-run measurements are still not a ten-run "
               "result, so the policy does not classify them")

        # Repeated: two complete ten-run measurements on one commit pool
        # to twenty, which must not stop them being comparable.
        path = root / "repeated.json"
        seeded(path)
        probe_census.record_policy(path, "alpha", acceptable_failures=1,
                                   justification="one known race")
        probe_census.record_result(path, measurement(runs=N, failures=0,
                                                     age_days=2))
        probe_census.record_result(path, measurement(runs=N, failures=5,
                                                     age_days=1))
        expect(summary_of(path)["requested_runs"] == 2 * N,
               "the cohort pools to twenty runs")
        expect(tolerance(path) == "over-tolerance",
               "...and the newest ten-run measurement is what classifies, "
               "rather than the pooled total falling out of the policy")

        # An odd-sized run afterwards does not erase the last real one.
        probe_census.record_result(path, measurement(runs=3, failures=0,
                                                     age_days=0))
        expect(tolerance(path) == "over-tolerance",
               "a later three-run measurement leaves the newest ten-run "
               "verdict standing")


# ==========================================================================
def test_refusals() -> None:
    print("\n-- controlled refusals leave the bytes alone --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        before = path.read_bytes()

        # Malformed authoritative state.
        broken = root / "broken.json"
        broken.write_text("{not json", encoding="utf-8")
        broken_bytes = broken.read_bytes()
        expect_refusal(lambda: probe_census.record_result(broken,
                                                          result_document()),
                       "a census that is not valid JSON is refused",
                       "not valid JSON")
        unchanged(broken, broken_bytes,
                  "the malformed census is left byte-for-byte alone")

        # An absent or unmigrated census names --seed and never migrates.
        absent = root / "absent.json"
        expect_refusal(lambda: probe_census.record_result(absent,
                                                          result_document()),
                       "recording into an absent census names --seed", "--seed")
        expect(not absent.exists(),
               "the refusal did NOT seed the census as a side effect")
        legacy = root / "legacy.json"
        legacy.write_text(json.dumps(v1_document()) + "\n", encoding="utf-8")
        legacy_bytes = legacy.read_bytes()
        expect_refusal(lambda: probe_census.record_result(legacy,
                                                          result_document()),
                       "recording into a v1 census names --seed",
                       "probe-census/v1", "--seed")
        expect_refusal(lambda: probe_census.record_policy(
            legacy, "alpha", acceptable_failures=1),
            "a policy update on a v1 census names --seed", "--seed")
        unchanged(legacy, legacy_bytes,
                  "a v1 census is not migrated as a side effect of a refusal")

        # The three discriminating fields of a result document. Since
        # #1492 the declared schema reports these, so the refusal names
        # the offending JSON path rather than a hand-written phrase.
        for mutation, fragment, why in (
            ({"schema": "probe-flake-result/v9"}, "probe-flake-result/v9",
             "an unrecognized result schema"),
            ({"probe": None}, "$.probe", "a result naming no probe"),
            ({"status": "weird"}, "weird", "an unrecognized status"),
        ):
            expect_refusal(
                lambda m=mutation: probe_census.record_result(
                    path, result_document(**m)),
                f"{why} is refused", fragment)
        expect_refusal(lambda: probe_census.record_result(path, [1, 2]),
                       "a result that is not an object is refused",
                       "is not of type 'object'")
        after = json.loads(path.read_text(encoding="utf-8"))
        expect(all(row["census"]["attempts"] == []
                   for row in after["probes"]),
               "an unrecognized status is NOT logged as a failed attempt")

        # A result naming a probe with no row.
        expect_refusal(lambda: probe_census.record_result(
            path, result_document(probe="ghost")),
            "a result naming no census row is refused", "ghost", "--seed")

        # Structural/type errors that block building the durable record.
        # Each is now a declared-schema violation, reported at its path
        # BEFORE the census is opened rather than from inside the
        # summarizer that would have tripped over it.
        for mutation, fragment, why in (
            ({"runs": "no"}, "$.runs", "a non-list `runs`"),
            ({"runs": [{"index": 1}]}, "$.runs[0]", "a run missing a field"),
            ({"check_counts": []}, "$.check_counts",
             "a non-object `check_counts`"),
            ({"retained_artifacts": "one"}, "$.retained_artifacts",
             "a non-list `retained_artifacts`"),
        ):
            expect_refusal(
                lambda m=mutation: probe_census.record_result(
                    path, result_document(**m)),
                f"{why} is refused", fragment)
        missing = result_document()
        del missing["worst_elapsed_seconds"]
        expect_refusal(lambda: probe_census.record_result(path, missing),
                       "a result missing a summarized field is refused",
                       "worst_elapsed_seconds")
        unchanged(path, before, "not one refusal changed the census bytes")

        # CLI value parsing.
        expect_refusal(
            lambda: probe_census._acceptable_failures_argument("2.5"),
            "a non-integer X is refused", "integer", "2.5")
        expect_refusal(lambda: probe_census._optional_number("soon", "--e"),
                       "a non-numeric estimate is refused", "number", "none")
        expect_refusal(lambda: probe_census._optional_number("nan", "--e"),
                       "a non-finite estimate is refused (JSON has no NaN)",
                       "finite")
        expect(probe_census._optional_number("none", "--e") is None,
               "the literal `none` clears the nullable estimate")
        expect(probe_census._optional_number("480", "--e") == 480
               and isinstance(probe_census._optional_number("480", "--e"), int),
               "an integral estimate stays an integer")
        expect(probe_census._optional_number("12.5", "--e") == 12.5,
               "a fractional estimate is stored as a float")


# ==========================================================================
def test_malformed_rows_refuse_cleanly() -> None:
    """Valid JSON, structurally unusable: refuse, never traceback (#1503).

    Since #1492 the refusal comes from the declared schema, so each case
    asserts the JSON PATH the message names rather than a hand-written
    phrase. The two promises the cases exist for are unchanged: a
    controlled refusal, and not one byte of the stored census disturbed.
    """
    print("\n-- structurally malformed census state --")

    def census_with(rows):
        return {"schema": probe_census.CENSUS_SCHEMA, "probes": rows}

    def row(key="alpha", census=None):
        return {"key": key, "script": "alpha_probe.py",
                "classification": "manual-only", "protocol": "legacy",
                "census": probe_census.empty_census() if census is None
                else census}

    with registry(), scratch() as root:
        cases = [
            # An UNHASHABLE key: `key in live` and `{e["key"] for e ...}`
            # both raise TypeError on this, so it must be refused first.
            (census_with([row(key=[])]), "$.probes[0].key",
             "a row whose key is an unhashable list"),
            (census_with([row(key=7)]), "$.probes[0].key",
             "a row whose key is a number"),
            (census_with([{"script": "x.py"}]), "'key' is a required property",
             "a row with no key at all"),
            ({"schema": probe_census.CENSUS_SCHEMA,
              "probes": {"alpha": {}}},
             "$.probes", "a `probes` mapping instead of a list"),
            (census_with(["alpha"]), "$.probes[0]",
             "a row that is a bare string"),
            (census_with([row(census={**probe_census.empty_census(),
                                      "attempts": 5})]),
             "$.probes[0].census.attempts", "a non-list attempt log"),
            (census_with([row(census={**probe_census.empty_census(),
                                      "history": "old",
                                      "current": {"commit_sha": COMMIT_B,
                                                  "samples": []}})]),
             "$.probes[0].census.history", "a non-list history"),
            (census_with([row(census={**probe_census.empty_census(),
                                      "current": {"commit_sha": COMMIT_A,
                                                  "samples": 3}})]),
             "$.probes[0].census.current.samples", "a non-list sample list"),
            (census_with([row(census="not a record")]),
             "$.probes[0].census", "a row whose census is a string"),
        ]
        for index, (document, fragment, why) in enumerate(cases):
            path = root / f"case-{index}.json"
            path.write_text(json.dumps(document), encoding="utf-8")
            before = path.read_bytes()
            expect_refusal(
                lambda p=path: probe_census.record_result(p, result_document()),
                f"--record refuses {why}", fragment)
            unchanged(path, before, f"...and changes no bytes ({why})")

        # `--seed` walks the same rows, through reconcile_inventory.
        for index, (document, fragment, why) in enumerate(cases[:5]):
            path = root / f"seed-{index}.json"
            path.write_text(json.dumps(document), encoding="utf-8")
            before = path.read_bytes()
            expect_refusal(lambda p=path: probe_census.ensure_document(p),
                           f"--seed refuses {why}", fragment)
            unchanged(path, before, f"...and changes no bytes ({why})")

        # And a policy update, which addresses a row by key too.
        path = root / "policy.json"
        path.write_text(json.dumps(census_with([row(key=[])])), encoding="utf-8")
        before = path.read_bytes()
        expect_refusal(lambda: probe_census.record_policy(
            path, "alpha", acceptable_failures=1),
            "a policy update refuses an unusable row key", "$.probes[0].key")
        unchanged(path, before, "...and changes no bytes")

        # A truthy non-list append-only field used to reach the
        # preservation comparison, which reported it rather than slicing
        # it. The declared schema now refuses it a step earlier, for
        # every operation alike and at its exact path.
        for field in ("history", "attempts"):
            path = root / f"seedcmp-{field}.json"
            path.write_text(json.dumps(census_with([
                row(census={**probe_census.empty_census(), field: 5})])),
                encoding="utf-8")
            before = path.read_bytes()
            expect_refusal(lambda p=path: probe_census.ensure_document(p),
                           f"--seed refuses a stored non-list `{field}`",
                           f"$.probes[0].census.{field}")
            unchanged(path, before, f"...and changes no bytes (`{field}`)")
            expect_refusal(
                lambda p=path: probe_census.record_result(p, result_document()),
                f"--record refuses the same stored non-list `{field}`",
                f"$.probes[0].census.{field}")
            unchanged(path, before, f"...and changes no bytes (`{field}`)")

        # A non-list `samples` inside an ARCHIVED cohort is the case
        # #1428 could only tolerate: nothing but `_sample_total` and the
        # preservation comparison ever read it, and both were written to
        # cope rather than crash. It is exactly what "its shape is
        # #1492's to report" meant, and this is #1492 reporting it.
        path = root / "seedcmp-samples.json"
        path.write_text(json.dumps(census_with([
            row(census={**probe_census.empty_census(),
                        "history": [{"commit_sha": COMMIT_A, "samples": 4}]})])),
            encoding="utf-8")
        before = path.read_bytes()
        expect_refusal(lambda: probe_census.ensure_document(path),
                       "--seed refuses an archived cohort whose `samples` is "
                       "not a list, which #1428 could only tolerate",
                       "$.probes[0].census.history[0].samples")
        unchanged(path, before, "...and changes no bytes")

        # The safety boundary at `update`'s funnel: any structural or
        # type error a mutation meets becomes a controlled refusal.
        good = root / "good.json"
        seeded(good)
        payload = good.read_bytes()

        def exploding(_before):
            raise TypeError("unhashable type: 'list'")

        expect_refusal(lambda: probe_census.update(good, exploding),
                       "a structural/type error inside the transaction is a "
                       "controlled refusal, not a traceback",
                       "structurally malformed", "TypeError")
        unchanged(good, payload, "...and changes no bytes")

        # A CensusError from the mutation is NOT rewrapped: its own
        # actionable message survives.
        def refusing(_before):
            raise probe_census.CensusError("seed it first with --seed")

        expect_refusal(lambda: probe_census.update(good, refusing),
                       "a deliberate refusal keeps its own message",
                       "seed it first with --seed")


# ==========================================================================
def test_duplicate_target_rows() -> None:
    """A target key must name exactly one row (#1503)."""
    print("\n-- duplicate target rows --")

    def row(key, census=None):
        return {"key": key, "script": f"{key}_probe.py",
                "classification": "manual-only", "protocol": "legacy",
                "census": probe_census.empty_census() if census is None
                else census}

    with registry(), scratch() as root:
        duped = root / "duplicate-target.json"
        duped.write_text(json.dumps({
            "schema": probe_census.CENSUS_SCHEMA,
            "probes": [row("alpha"), row("beta"), row("alpha")]}),
            encoding="utf-8")
        before = duped.read_bytes()
        expect_refusal(lambda: probe_census.record_result(duped,
                                                          result_document()),
                       "--record refuses a probe with two census rows",
                       "2 census rows", "--record")
        unchanged(duped, before, "...and changes no bytes")
        expect_refusal(lambda: probe_census.record_policy(
            duped, "alpha", acceptable_failures=1),
            "a policy update refuses a probe with two census rows",
            "2 census rows", "a policy update")
        unchanged(duped, before, "...and changes no bytes")
        # Writing the first and leaving the second is exactly the silent
        # half-update the refusal exists to prevent.
        stored = json.loads(duped.read_text(encoding="utf-8"))
        expect(all(r["census"]["attempts"] == [] for r in stored["probes"]),
               "neither duplicate row was half-written")

        # The rule is about the TARGET row only. An UNRELATED duplicate
        # is inventory drift, and the parity rule says drift must never
        # discard a finished measurement.
        elsewhere = root / "unrelated-duplicate.json"
        elsewhere.write_text(json.dumps({
            "schema": probe_census.CENSUS_SCHEMA,
            "probes": [row("alpha"), row("beta"), row("beta")]}),
            encoding="utf-8")
        probe_census.record_result(elsewhere, result_document())
        stored = json.loads(elsewhere.read_text(encoding="utf-8"))
        expect(len(stored["probes"][0]["census"]["current"]["samples"]) == 1,
               "an unrelated duplicate row does not refuse the measurement")
        expect([r["key"] for r in stored["probes"]]
               == ["alpha", "beta", "beta"],
               "and the duplicate rows are preserved verbatim for a person")
        expect(all(r["census"] == probe_census.empty_census()
                   for r in stored["probes"][1:]),
               "neither unrelated row was touched")


# ==========================================================================
def test_path_substitution() -> None:
    print("\n-- symlink, hard-link and non-regular path refusal --")
    with registry(), scratch() as root:
        good = root / "real.json"
        seeded(good)
        payload = good.read_bytes()

        # A symlinked census target must never be followed: `os.replace`
        # replaces the LINK, so following one writes the census wherever
        # it points.
        linked = root / "linked.json"
        linked.symlink_to(good)
        expect_refusal(lambda: probe_census.record_result(linked,
                                                          result_document()),
                       "a symlinked census target is refused", "symlink")
        unchanged(good, payload, "the symlink's target is left alone")

        # A hard-linked target: replacing it would silently strand the
        # other name on the old bytes.
        hard = root / "hard.json"
        os.link(good, hard)
        expect_refusal(lambda: probe_census.record_result(hard,
                                                          result_document()),
                       "a hard-linked census target is refused", "one link")
        hard.unlink()

        # A non-regular target.
        directory = root / "adirectory.json"
        directory.mkdir()
        expect_refusal(lambda: probe_census.record_result(directory,
                                                          result_document()),
                       "a census target that is not a regular file is refused",
                       "regular file")

        # A symlinked census DIRECTORY.
        elsewhere = root / "elsewhere"
        elsewhere.mkdir()
        via_link = root / "vialink"
        via_link.symlink_to(elsewhere)
        expect_refusal(lambda: probe_census.record_result(
            via_link / "probe_census.json", result_document()),
            "a symlinked census directory is refused", "symlink")

        # The lock path is the THIRD path the rule covers.
        locked_root = root / "locked"
        locked_root.mkdir()
        target = locked_root / "probe_census.json"
        seeded(target)
        stamp = target.read_bytes()
        guard = probe_census.lock_path(target)
        # `seeded` above already created the lock; replace it with each
        # substitution in turn.
        guard.unlink()
        guard.symlink_to(good)
        expect_refusal(lambda: probe_census.record_result(target,
                                                          result_document()),
                       "a symlinked lock path is refused", "symlink")
        guard.unlink()
        unchanged(good, payload,
                  "the symlinked lock's target was never written through")

        os.link(good, guard)
        expect_refusal(lambda: probe_census.record_result(target,
                                                          result_document()),
                       "a hard-linked lock path is refused", "one link")
        guard.unlink()

        guard.mkdir()
        expect_refusal(lambda: probe_census.record_result(target,
                                                          result_document()),
                       "a lock path that is not a regular file is refused",
                       "regular file")
        guard.rmdir()
        unchanged(target, stamp, "no lock-path refusal changed the census")

        # And with a clean lock path the same call succeeds, so the
        # refusals above are the substitution and nothing else.
        probe_census.record_result(target, result_document())
        expect(target.read_bytes() != stamp,
               "with an unsubstituted lock path the same write succeeds")
        expect(probe_census.lock_path(target).exists(),
               "the lock file is left in place rather than unlinked")


# ==========================================================================
def test_atomicity() -> None:
    print("\n-- atomic replacement and injected failure --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        probe_census.record_result(path, result_document())
        before = path.read_bytes()

        expect(staging_residue(path.parent) == [],
               "a completed replacement leaves no staging residue")

        # Fail AFTER serialization and the preservation checks, but
        # before the replacement.
        original = probe_census._atomic_replace
        calls: list[bytes] = []

        def exploding(target, payload):
            calls.append(payload)
            raise OSError("injected: the machine died before the rename")

        probe_census._atomic_replace = exploding
        try:
            raised = None
            try:
                probe_census.record_result(path, result_document(commit=COMMIT_B))
            except OSError as error:
                raised = error
            expect(raised is not None and "injected" in str(raised),
                   "the injected failure propagates rather than being swallowed")
            expect(len(calls) == 1 and b"probe-census/v5" in calls[0],
                   "the candidate had been fully serialized before the failure")
        finally:
            probe_census._atomic_replace = original
        unchanged(path, before,
                  "a failure before replacement leaves the OLD census intact")
        expect(json.loads(path.read_text(encoding="utf-8"))["schema"]
               == "probe-census/v5",
               "and the old census is still a complete, readable document")

        # Stale staging residue from a killed writer is never
        # authoritative, and the next writer clears it.
        stale = path.parent / (probe_census.STAGING_PREFIX + "killed"
                               + probe_census.STAGING_SUFFIX)
        stale.write_text("{ truncated", encoding="utf-8")
        probe_census.record_result(path, result_document(commit=COMMIT_B))
        expect(not stale.exists(), "the next writer clears stale staging files")
        document = json.loads(path.read_text(encoding="utf-8"))
        expect(document["probes"][0]["census"]["current"]["commit_sha"]
               == COMMIT_B,
               "and the real write went through, ignoring the residue")
        expect(staging_residue(path.parent) == [],
               "no staging file survives a successful replacement")


# ==========================================================================
def test_preservation_guard() -> None:
    print("\n-- the preservation contract refuses to lose data --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        probe_census.record_result(path, result_document())
        probe_census.record_policy(path, "alpha", acceptable_failures=2,
                                   justification="kept")
        probe_census.record_result(path, result_document(commit=COMMIT_B))
        before = path.read_bytes()

        def refuses(mutation, msg, *fragments):
            def mutate(document):
                candidate = copy.deepcopy(document)
                mutation(candidate)
                return candidate, {"alpha": {"measurements"}}
            expect_refusal(lambda: probe_census.update(path, mutate),
                           msg, *fragments)
            unchanged(path, before, f"...and changes no bytes ({msg})")

        def row(document, key="alpha"):
            return [r for r in document["probes"] if r["key"] == key][0]

        refuses(lambda d: row(d)["census"].__setitem__("history", []),
                "dropping an archived cohort is refused", "append-only")
        refuses(lambda d: row(d)["census"]["current"]["samples"].clear(),
                "dropping the current cohort's samples is refused",
                "lost retained measurements")
        refuses(lambda d: row(d)["census"].__setitem__("attempts", []),
                "dropping the attempt log is refused", "append-only")
        refuses(lambda d: row(d)["census"]["attempts"].insert(0, {"x": 1}),
                "prepending to the attempt log is refused", "append-only")
        refuses(lambda d: row(d)["census"].__setitem__(
            "acceptable_failures", None),
            "a measurement clearing a policy field is refused",
            "changed policy field")
        refuses(lambda d: row(d, "beta")["census"].__setitem__(
            "acceptable_failures", 7),
            "touching an unrelated row is refused", "unrelated probe 'beta'")
        refuses(lambda d: d["probes"].pop(1),
                "dropping a row is refused", "order or membership")
        refuses(lambda d: d["probes"].reverse(),
                "reordering the inventory is refused", "order or membership")
        refuses(lambda d: row(d).__setitem__("script", "renamed.py"),
                "a measurement renaming its own row's script is refused",
                "inventory field")

        # A policy update may not touch measurements either.
        def policy_mutate(document):
            candidate = copy.deepcopy(document)
            row(candidate)["census"]["attempts"].append({"forged": True})
            return candidate, {"alpha": {"policy"}}
        expect_refusal(lambda: probe_census.update(path, policy_mutate),
                       "a policy update appending an attempt is refused",
                       "which a policy update may not touch")
        unchanged(path, before, "...and changes no bytes")

        # Reconciliation may not silently lose a measurement either.
        def seed_mutate(document):
            candidate = copy.deepcopy(document)
            row(candidate)["census"]["history"] = []
            row(candidate)["census"]["current"] = None
            return candidate, probe_census.TOUCH_ANY
        expect_refusal(lambda: probe_census.update(path, seed_mutate),
                       "reconciliation losing a cohort is refused",
                       "append-only")
        unchanged(path, before, "...and changes no bytes")


# ==========================================================================
# The adversarial sweep
# ==========================================================================
DELETE = object()

# Retyped to every JSON shape the field could plausibly be confused
# with, plus removal. `inf`/`nan` are here because `json.loads` accepts
# Python's non-standard spellings, so a census really can hold one.
FUZZ_VALUES = (None, 0, 5, -1, 1.5, "", "x", True, [], {}, [5], {"a": 1},
               [[]], float("inf"), float("nan"), DELETE)

# Producer-only fields `Measurement.to_document` adds. None may ride
# into an ACCEPTED census, whatever the rest of the document looks like.
PRODUCER_ONLY = ("port", "error_run", "artifact_root", "invocation_dir",
                 "label")


def _locations(node, prefix=()):
    """Every addressable location in a JSON document, depth first."""
    if isinstance(node, dict):
        for key, value in node.items():
            yield prefix + (key,)
            yield from _locations(value, prefix + (key,))
    elif isinstance(node, list):
        for index, value in enumerate(node):
            yield prefix + (index,)
            yield from _locations(value, prefix + (index,))


def _replace(document, path, value):
    """Set (or DELETE) one location. False when the path is unreachable."""
    node = document
    try:
        for step in path[:-1]:
            node = node[step]
        if value is DELETE:
            if isinstance(node, dict):
                node.pop(path[-1], None)
            else:
                node.pop(path[-1])
        else:
            node[path[-1]] = value
    except (KeyError, IndexError, TypeError, AttributeError):
        return False
    return True


def stored_v3_document() -> dict:
    """A `probe-census/v3` census exactly as #1434 left one.

    Seven-field records, `claims` but no `outcomes`. Spelled out here
    for the same reason `stored_v2_document` is: it describes migration
    INPUT, and deriving it from the current `empty_census()` would
    silently start testing the current shape the moment the record grows
    another field.
    """
    return {
        "schema": "probe-census/v3",
        "probes": [{
            "key": "alpha", "script": "alpha_probe.py",
            "classification": "manual-only", "protocol": "legacy",
            "census": {
                "acceptable_failures": 2,
                "acceptable_failures_justification": "two known races",
                "estimated_worst_case_seconds": 480,
                "current": None,
                "history": [],
                "attempts": [],
                "claims": [],
            },
        }],
    }


def stored_v4_document() -> dict:
    """A `probe-census/v4` census exactly as #1439 left one.

    Eight-field records, through `outcomes` but no `deferred`. Like the
    older fixtures, this is migration INPUT and must not derive from the
    current record shape.
    """
    document = stored_v3_document()
    document["schema"] = probe_census.OUTCOME_SCHEMA
    document["probes"][0]["census"]["outcomes"] = []
    return document


def rich_census() -> dict:
    """A current-schema census with real accumulated data on its first row.

    Consistent under #1493's cross-field invariants, which is what keeps
    every case built on it honest: a base document that were already
    inconsistent would be refused for its OWN defect, and each case
    would then pass without ever exercising the mutation it applies.
    So the two retained samples are matched by two accepted attempts —
    one per commit, in the order they were ingested — and the archived
    cohort's sample really is from that cohort's commit.
    """
    sample = probe_census.summarize_sample(result_document())
    attempt = probe_census.summarize_attempt(result_document(), True)
    archived = probe_census.summarize_sample(result_document(commit=COMMIT_B))
    archived_attempt = probe_census.summarize_attempt(
        result_document(commit=COMMIT_B), True)

    def row(key, census):
        return {"key": key, "script": f"{key}_probe.py",
                "classification": "manual-only", "protocol": "legacy",
                "census": census}

    return {
        "schema": probe_census.CENSUS_SCHEMA,
        "probes": [
            row("alpha", {"acceptable_failures": 2,
                          "acceptable_failures_justification": "two races",
                          "estimated_worst_case_seconds": 480,
                          "current": {"commit_sha": COMMIT_A,
                                      "samples": [copy.deepcopy(sample)]},
                          "history": [{"commit_sha": COMMIT_B,
                                       "samples": [copy.deepcopy(archived)]}],
                          "attempts": [copy.deepcopy(archived_attempt),
                                       copy.deepcopy(attempt)],
                          "claims": [],
                          "outcomes": [],
                          "deferred": None}),
            row("beta", probe_census.empty_census()),
            row("gamma", probe_census.empty_census()),
        ],
    }


def test_adversarial_malformed_input() -> None:
    """Retype and delete EVERY field of both input surfaces (#1503).

    Five review rounds each surfaced one more parseable-but-malformed
    input that crashed an operation or half-wrote through it, one field
    at a time. This sweep is what closes the class rather than the
    instance: it drives every operation over every location of a stored
    census and of a result document, and holds all three promises at
    once — no uncontrolled exception, no byte changed by a refusal, and
    no producer-only field in an accepted record.

    It is exhaustive rather than random, so it is fully deterministic:
    the locations come from insertion-ordered dicts and the values from
    a fixed tuple. It deliberately does not enumerate WHICH mutations
    must be rejected — that is the declared schema's specification, and
    restating it case by case here would rebuild by hand exactly the
    surface #1492 replaced. What it asserts is the three promises that
    hold whatever the schema says, and that both outcomes really occur.
    """
    print("\n-- adversarial sweep over both input surfaces --")
    with registry(ci_eligible={"beta"}), scratch() as root:
        target = root / "probe_census.json"

        operations = (
            ("--seed", lambda p: probe_census.ensure_document(p)),
            ("--record", lambda p: probe_census.record_result(
                p, result_document(commit=COMMIT_B))),
            ("--set-acceptable-failures", lambda p: probe_census.record_policy(
                p, "alpha", acceptable_failures=1, justification="j")),
            ("--set-estimate", lambda p: probe_census.record_policy(
                p, "alpha", estimate=9)),
            ("--validate", lambda p: probe_census.validate_manifest(
                probe_census.load(p))),
        )

        uncontrolled: list[str] = []
        disturbed: list[str] = []
        runs = refused = accepted = 0
        locations = list(_locations(rich_census()))
        for path, value in itertools.product(locations, FUZZ_VALUES):
            document = rich_census()
            if not _replace(document, path, value):
                continue
            try:
                text = json.dumps(document)
            except (TypeError, ValueError):
                continue          # not representable, so not reachable
            where = ".".join(str(step) for step in path)
            for name, operation in operations:
                target.write_text(text, encoding="utf-8")
                stored = target.read_bytes()
                runs += 1
                try:
                    operation(target)
                except (probe_census.CensusError,
                        probe_census.DocsWorktreeMissing):
                    refused += 1
                    if target.read_bytes() != stored:
                        disturbed.append(f"{name} on {where}={value!r}")
                except BaseException as error:  # noqa: BLE001 - the bug IS this
                    uncontrolled.append(
                        f"{name} on {where}={value!r} raised "
                        f"{type(error).__name__}: {error}")
                else:
                    accepted += 1

        expect(runs > 5000,
               f"the census sweep really ran ({runs} operations over "
               f"{len(locations)} locations)")
        expect(refused > 0 and accepted > 0,
               f"it exercised both outcomes ({refused} refused, "
               f"{accepted} accepted), so it cannot pass vacuously")
        expect(uncontrolled == [],
               f"no malformed census produces an uncontrolled exception "
               f"({len(uncontrolled)}: {uncontrolled[:3]})")
        expect(disturbed == [],
               f"no refusal changes a byte of the census "
               f"({len(disturbed)}: {disturbed[:3]})")

        # -- the other surface: the result document `--record` consumes --
        # The census sweep leaves its last mutated document on disk, and
        # the declared schema will not reconcile that into a fresh one;
        # the second sweep starts from a newly seeded census.
        target.unlink()
        probe_census.ensure_document(target)
        clean = target.read_bytes()
        uncontrolled, disturbed, leaked = [], [], []
        runs = refused = accepted = 0
        for path, value in itertools.product(
                list(_locations(result_document())), FUZZ_VALUES):
            document = result_document()
            if not _replace(document, path, value):
                continue
            try:
                json.dumps(document, allow_nan=False)
            except (TypeError, ValueError):
                continue
            where = ".".join(str(step) for step in path)
            target.write_bytes(clean)
            runs += 1
            try:
                probe_census.record_result(target, document)
            except (probe_census.CensusError,
                    probe_census.DocsWorktreeMissing):
                refused += 1
                if target.read_bytes() != clean:
                    disturbed.append(f"{where}={value!r}")
            except BaseException as error:  # noqa: BLE001
                uncontrolled.append(f"{where}={value!r} raised "
                                    f"{type(error).__name__}: {error}")
            else:
                accepted += 1
                text = target.read_text(encoding="utf-8")
                leaked += [f"{name!r} via {where}={value!r}"
                           for name in PRODUCER_ONLY if f'"{name}"' in text]

        expect(runs > 300,
               f"the result sweep really ran ({runs} --record operations)")
        expect(refused > 0 and accepted > 0,
               f"it exercised both outcomes ({refused} refused, "
               f"{accepted} accepted)")
        expect(uncontrolled == [],
               f"no malformed result produces an uncontrolled exception "
               f"({len(uncontrolled)}: {uncontrolled[:3]})")
        expect(disturbed == [],
               f"no refused result changes a byte of the census "
               f"({len(disturbed)}: {disturbed[:3]})")
        expect(leaked == [],
               f"no accepted result carries a producer-only field into the "
               f"census ({len(leaked)}: {leaked[:3]})")


# ==========================================================================
# The declared schema (#1492)
# ==========================================================================
def expect_valid(call, msg: str) -> None:
    """`call` accepts. A refusal is the failure, reported not raised."""
    try:
        call()
    except Exception as error:  # noqa: BLE001 - a refusal here IS the bug
        expect(False, f"{msg} (refused: {type(error).__name__}: {error})")
        return
    expect(True, msg)


def harness_error_result() -> dict:
    """A well-formed harness error, carrying the run that broke."""
    return result_document(status="harness-error")


def _no_runs_result() -> dict:
    """A harness error on the very FIRST run, so nothing completed.

    `measure` creates the run directory, launches, and returns through
    `stop_with_harness_error` before anything joins `runs` — so
    `check_counts` is the descriptor's ids seeded to all zeros, exactly
    what `Measurement.check_counts()` starts from.
    """
    return result_document(
        status="harness-error", error="run 1: unreadable event stream",
        requested_runs=3, completed_runs=0, runs=[],
        error_run={"index": 1, "port": 9100, "outcome": "HARNESS_ERROR",
                   "elapsed_seconds": 0.5, "checks": {},
                   "artifact_dir": "/tmp/artifacts/run-001"},
        check_counts={"first": {"PASS": 0, "FAIL": 0, "MISSING": 0},
                      "second": {"PASS": 0, "FAIL": 0, "MISSING": 0}},
        worst_elapsed_seconds=0.0, total_elapsed_seconds=0.0,
        retained_artifacts=["/tmp/artifacts/run-001"])


def test_declared_schema() -> None:
    """The schema file itself, and what it now refuses (#1492).

    Every rejection is driven through a REAL operation against a real
    census on disk, so each case proves the two things a declared
    validator has to prove together: the document is refused, and the
    authoritative bytes are not touched by the refusal.
    """
    print("\n-- the declared JSON Schema --")
    schema = probe_census.load_schema()
    expect(schema.get("$schema") == "https://json-schema.org/draft/2020-12/schema",
           "the checked-in schema identifies a supported draft")
    # `load_schema` runs that draft's own meta-schema check, so reaching
    # here at all is the self-check passing.
    expect(set(probe_census.SCHEMA_DEFINITIONS)
           == {probe_census.SEED_SCHEMA, probe_census.RECORD_SCHEMA,
               probe_census.CLAIM_SCHEMA, probe_census.OUTCOME_SCHEMA,
               probe_census.CENSUS_SCHEMA,
               probe_census.RESULT_SCHEMA},
           "every document kind the tool reads has a declared schema")
    expect(all(name in (schema.get("$defs") or {})
               for name in probe_census.SCHEMA_DEFINITIONS.values()),
           "each declared schema names a definition the file really has")
    expect(all("/" not in name
               for name in probe_census.SCHEMA_DEFINITIONS.values()),
           "no definition name contains a JSON Pointer separator")
    expect_refusal(
        lambda: probe_census.validate_document({}, "probe-census/v9", "x"),
        "asking for an undeclared schema is a controlled refusal",
        "probe-census/v9")

    with registry(ci_eligible={"beta"}), scratch() as root:
        # Applying every declared schema to a document that satisfies it
        # is what proves the file's internal `$ref`s all resolve — a
        # schema that self-checks can still be unusable.
        expect_valid(lambda: probe_census.validate_document(
            v1_document(), probe_census.SEED_SCHEMA, "a v1 seed"),
            "the v1 seed schema accepts a real v1 seed")
        expect_valid(lambda: probe_census.validate_document(
            rich_census(), probe_census.CENSUS_SCHEMA, "a v5 census"),
            "the v5 census schema accepts a real measured census")
        expect_valid(lambda: probe_census.validate_document(
            stored_v2_document(), probe_census.RECORD_SCHEMA, "a v2 census"),
            "the FROZEN v2 schema still accepts a real stored v2 census, "
            "which is what --seed migrates from")
        expect_valid(lambda: probe_census.validate_document(
            stored_v3_document(), probe_census.CLAIM_SCHEMA, "a v3 census"),
            "the FROZEN v3 schema still accepts a real stored v3 census, "
            "which --seed also migrates from")
        expect_valid(lambda: probe_census.validate_document(
            stored_v4_document(), probe_census.OUTCOME_SCHEMA, "a v4 census"),
            "the FROZEN v4 schema still accepts a real stored v4 census, "
            "which --seed also migrates from")
        expect_valid(lambda: probe_census.validate_document(
            probe_census.build_manifest(), probe_census.CENSUS_SCHEMA,
            "a fresh manifest"),
            "...and the manifest this tool generates for itself")
        expect_valid(lambda: probe_census.validate_result(result_document()),
                     "the result schema accepts a real ok measurement")
        expect_valid(
            lambda: probe_census.validate_result(harness_error_result()),
            "...and a harness error carrying its HARNESS_ERROR run")

        target = root / "probe_census.json"
        clean = root / "clean.json"
        probe_census.ensure_document(clean)
        clean_bytes = clean.read_bytes()

        def refuses_census(mutate, fragment, why) -> None:
            """A stored census `mutate` breaks: refused, and unchanged."""
            document = rich_census()
            mutate(document)
            target.write_text(json.dumps(document), encoding="utf-8")
            stored = target.read_bytes()
            expect_refusal(
                lambda: probe_census.record_result(target, result_document()),
                f"a census with {why} is refused", fragment)
            expect(target.read_bytes() == stored,
                   f"...and the refusal changed no bytes ({why})")

        def refuses_result(mutate, fragment, why) -> None:
            """A result document `mutate` breaks: refused, nothing written."""
            document = result_document()
            mutate(document)
            expect_refusal(
                lambda: probe_census.record_result(clean, document),
                f"a result with {why} is refused", fragment)
            expect(clean.read_bytes() == clean_bytes,
                   f"...and the refusal wrote nothing ({why})")

        # -- a nullable field DELETED, not set to null (`6a23027f`) -----
        # Nullable is spelled as a REQUIRED null-inclusive type, so
        # removing one is a violation rather than an absence. That is
        # the whole difference between `additionalProperties`/`required`
        # and reading fields with `.get()`.
        for field in probe_census.empty_census():
            refuses_census(
                lambda d, f=field: d["probes"][0]["census"].pop(f),
                f"'{field}' is a required property",
                f"the census field `{field}` deleted")
        for field in ("key", "script", "classification", "protocol", "census"):
            refuses_census(
                lambda d, f=field: d["probes"][0].pop(f),
                f"'{field}' is a required property",
                f"the inventory field `{field}` deleted")
        for field in ("timestamp_utc", "commit_sha", "failure_rate",
                      "retained_artifacts", "check_counts"):
            refuses_census(
                lambda d, f=field: d["probes"][0]["census"]["current"][
                    "samples"][0].pop(f),
                f"'{field}' is a required property",
                f"a stored sample missing `{field}`")

        # -- a truthy non-object `runs[i].checks` (`6ddc01d9`) ----------
        # This one used to raise AttributeError from inside the
        # transaction, because a truthy value passed the guard and then
        # was asked for `.items()`.
        for value, why in ((5, "the number 5"), ("PASS", "a string"),
                           (["first"], "a non-empty list"),
                           (True, "the boolean True"), (0, "the number 0"),
                           (None, "null"), ([], "an empty list")):
            refuses_result(
                lambda d, v=value: d["runs"][0].__setitem__("checks", v),
                "$.runs[0].checks", f"a per-run `checks` that is {why}")
        refuses_result(
            lambda d: d["runs"][0]["checks"].__setitem__("first", "MAYBE"),
            "$.runs[0].checks.first", "an unrecognized per-run check result")

        # -- unexpected properties, in every representative object -----
        for mutate, fragment, why in (
            (lambda d: d.__setitem__("extra", 1), "$", "the result root"),
            (lambda d: d["runs"][0].__setitem__("extra", 1), "$.runs[0]",
             "a result run"),
            (lambda d: d["checks"][0].__setitem__("extra", 1), "$.checks[0]",
             "a check descriptor"),
            (lambda d: d["check_counts"]["first"].__setitem__("SKIPPED", 1),
             "$.check_counts.first", "a check tally"),
        ):
            refuses_result(mutate, fragment,
                           f"an unexpected property in {why}")
        for mutate, fragment, why in (
            (lambda d: d.__setitem__("extra", 1), "$", "the census root"),
            (lambda d: d["probes"][0].__setitem__("extra", 1), "$.probes[0]",
             "an inventory row"),
            (lambda d: d["probes"][0]["census"].__setitem__("extra", 1),
             "$.probes[0].census", "a census record"),
            (lambda d: d["probes"][0]["census"]["current"].__setitem__(
                "extra", 1), "$.probes[0].census.current", "a cohort"),
            (lambda d: d["probes"][0]["census"]["current"]["samples"][
                0].__setitem__("port", 9100),
             "$.probes[0].census.current.samples[0]",
             "a stored sample (a producer-only field)"),
            (lambda d: d["probes"][0]["census"]["attempts"][0].__setitem__(
                "extra", 1), "$.probes[0].census.attempts[0]", "an attempt"),
        ):
            refuses_census(mutate, fragment,
                           f"an unexpected property in {why}")

        # -- enums, ranges and lengths ---------------------------------
        for mutate, fragment, why in (
            (lambda d: d["probes"][0].__setitem__("classification", "maybe"),
             "$.probes[0].classification", "an unrecognized classification"),
            (lambda d: d["probes"][0].__setitem__("protocol", "probe/v9"),
             "$.probes[0].protocol", "an unrecognized protocol status"),
            (lambda d: d["probes"][0]["census"].__setitem__(
                "acceptable_failures", -1),
             "$.probes[0].census.acceptable_failures", "a negative X"),
            (lambda d: d["probes"][0]["census"].__setitem__(
                "acceptable_failures", 10),
             "$.probes[0].census.acceptable_failures",
             "an X of 10, which would accept a probe that never passes"),
            (lambda d: d["probes"][0]["census"].__setitem__(
                "acceptable_failures", True),
             "$.probes[0].census.acceptable_failures",
             "a boolean X (`bool` is an `int` subclass, so this needs its "
             "own rejection)"),
            (lambda d: d["probes"][0]["census"].__setitem__(
                "estimated_worst_case_seconds", -1),
             "$.probes[0].census.estimated_worst_case_seconds",
             "a negative estimate"),
            (lambda d: d["probes"][0]["census"].__setitem__(
                "acceptable_failures_justification", "x" * 4001),
             "$.probes[0].census.acceptable_failures_justification",
             "a justification past its length bound"),
            (lambda d: d["probes"][0]["census"]["current"].__setitem__(
                "commit_sha", ""),
             "$.probes[0].census.current.commit_sha", "an empty commit sha"),
            (lambda d: d["probes"][0]["census"]["current"]["samples"][
                0].__setitem__("failure_rate", 1.5),
             "$.probes[0].census.current.samples[0].failure_rate",
             "a failure rate above 1"),
            (lambda d: d["probes"][0]["census"]["current"]["samples"][
                0].__setitem__("timestamp_utc", "yesterday"),
             "$.probes[0].census.current.samples[0].timestamp_utc",
             "a timestamp in no recognized form"),
            (lambda d: d["probes"][0]["census"]["current"]["samples"][0][
                "runs"][0].__setitem__("outcome", "HARNESS_ERROR"),
             "$.probes[0].census.current.samples[0].runs[0].outcome",
             "HARNESS_ERROR as a stored run outcome, which it never is"),
            (lambda d: d["probes"][0]["census"]["attempts"][0].__setitem__(
                "status", "nope"),
             "$.probes[0].census.attempts[0].status",
             "an unrecognized attempt status"),
            (lambda d: d["probes"][0]["census"]["attempts"][0].__setitem__(
                "accepted", "yes"),
             "$.probes[0].census.attempts[0].accepted",
             "a non-boolean `accepted`"),
        ):
            refuses_census(mutate, fragment, why)
        for mutate, fragment, why in (
            (lambda d: d["runs"][0].__setitem__("port", 0), "$.runs[0].port",
             "a port below the representable range"),
            (lambda d: d["runs"][0].__setitem__("port", 70000),
             "$.runs[0].port", "a port above the representable range"),
            (lambda d: d.__setitem__("rts_capabilities", 0),
             "$.rts_capabilities", "zero RTS capabilities"),
            (lambda d: d.__setitem__("failure_count", -1), "$.failure_count",
             "a negative failure count"),
            (lambda d: d.__setitem__("worst_elapsed_seconds", -1),
             "$.worst_elapsed_seconds", "a negative elapsed time"),
            (lambda d: d["retained_artifacts"].append(""),
             "$.retained_artifacts[1]", "an empty artifact path"),
            (lambda d: d.__setitem__("commit_sha", "c" * 65), "$.commit_sha",
             "a commit sha past its length bound"),
        ):
            refuses_result(mutate, fragment, why)

        # -- non-finite numbers, which no schema bound can express -----
        # `json.loads` really does accept these spellings, and `maximum`
        # does not reject a NaN: no comparison with one is ever true.
        for value, why in ((float("nan"), "NaN"), (float("inf"), "Infinity"),
                           (float("-inf"), "-Infinity")):
            refuses_census(
                lambda d, v=value: d["probes"][0]["census"].__setitem__(
                    "estimated_worst_case_seconds", v),
                "$.probes[0].census.estimated_worst_case_seconds",
                f"a stored {why}")
            refuses_result(
                lambda d, v=value: d.__setitem__("total_elapsed_seconds", v),
                "$.total_elapsed_seconds", f"an incoming {why}")
        expect_refusal(
            lambda: probe_census.record_policy(
                clean, "alpha", estimate=float("nan")),
            "a policy update may not store a NaN either",
            "non-finite")
        expect(clean.read_bytes() == clean_bytes,
               "...and that refusal wrote nothing")

        # -- the schema DISCRIMINATOR, which is not a schema keyword ----
        target.write_text(json.dumps({**rich_census(),
                                      "schema": "probe-census/v9"}),
                          encoding="utf-8")
        stored = target.read_bytes()
        expect_refusal(
            lambda: probe_census.record_result(target, result_document()),
            "a census declaring an unreadable schema is refused",
            "probe-census/v9")
        expect(target.read_bytes() == stored, "...and changes no bytes")

        # -- the intake contract against the REAL producer -------------
        # `result_document()` is hand-written, so a schema that only
        # ever met it could agree with the fixture while both drifted
        # from what tools/probe_flake.py actually writes. This builds a
        # real `Measurement` — no engine, no subprocess beyond the `git`
        # call it makes for its own commit sha — and validates its own
        # serialization.
        descriptor = probe_flake.probe_protocol.build_descriptor(
            "alpha", [("first", "the first check"),
                      ("second", "the second check")])
        measurement = probe_flake.Measurement(
            "alpha", descriptor, requested_runs=2,
            rts_caps=probe_flake.DEFAULT_RTS_CAPS,
            artifact_root=Path("/tmp/artifacts"),
            invocation_dir=Path("/tmp/artifacts/alpha-1"))
        measurement.runs.append(probe_flake.RunRecord(
            1, 9100, probe_flake.RUN_PASS, 12.5,
            {"first": "PASS", "second": "PASS"}, None))
        measurement.runs.append(probe_flake.RunRecord(
            2, 9101, probe_flake.RUN_FAIL, 13.25,
            {"first": "PASS", "second": "FAIL"},
            Path("/tmp/artifacts/alpha-1/run-002")))
        expect_valid(
            lambda: probe_census.validate_result(measurement.to_document()),
            "the producer's own serialization satisfies the declared "
            "intake schema")
        expect(set(measurement.to_document()) == set(result_document()),
               "...and this file's fixture carries exactly its fields")
        # A real harness error on run 3 means three runs were REQUESTED
        # and the broken one never joined `runs`, so two completed —
        # which is #1493's cross-field rule as well as the producer's
        # own behaviour.
        measurement.requested_runs = 3
        measurement.status = "harness-error"
        measurement.error = "run 3 emitted a duplicate event"
        measurement.error_run = probe_flake.RunRecord(
            3, 9102, probe_flake.RUN_HARNESS_ERROR, 0.5, {},
            Path("/tmp/artifacts/alpha-1/run-003"))
        expect_valid(
            lambda: probe_census.validate_result(measurement.to_document()),
            "...and so does one carrying a real HARNESS_ERROR run")

        # -- and a valid document still goes all the way through -------
        expect_valid(lambda: probe_census.record_result(clean,
                                                        result_document()),
                     "a valid measurement is still accepted end to end")
        expect_valid(lambda: probe_census.record_result(clean,
                                                        harness_error_result()),
                     "...and so is a valid harness error")
        expect_valid(lambda: probe_census.validate_document(
            json.loads(clean.read_text(encoding="utf-8")),
            probe_census.CENSUS_SCHEMA, "the written census"),
            "and what the writer produced validates against its own schema")


# ==========================================================================
DRAFT = "https://json-schema.org/draft/2020-12/schema"


@contextmanager
def schema_file(text: str | None, root: Path):
    """`probe_census.SCHEMA_PATH` pointed at `text` (None = absent)."""
    target = root / f"schema-{abs(hash(text)) % 10 ** 8}.json"
    if text is not None:
        target.write_text(text, encoding="utf-8")
    saved = probe_census.SCHEMA_PATH
    cache = dict(probe_census._SCHEMA_CACHE)
    probe_census.SCHEMA_PATH = target
    probe_census._SCHEMA_CACHE.clear()
    try:
        yield target
    finally:
        probe_census.SCHEMA_PATH = saved
        probe_census._SCHEMA_CACHE.clear()
        probe_census._SCHEMA_CACHE.update(cache)


def _refuses_every_operation(census, before, good, fragment, why) -> None:
    """Every writing operation refuses `fragment`, and writes nothing."""
    for name, operation in (
        ("--record", lambda: probe_census.record_result(census, good)),
        ("--seed", lambda: probe_census.ensure_document(census)),
        ("a policy update", lambda: probe_census.record_policy(
            census, "alpha", acceptable_failures=1)),
    ):
        expect_refusal(operation, f"...and {name} refuses ({why})", fragment)
        expect(census.read_bytes() == before,
               f"...having written nothing ({name}, {why})")


def test_malformed_schema_file() -> None:
    """A broken SCHEMA is a refusal too, not a traceback.

    The validator's own input is a checked-in file, so it is exactly as
    capable of being wrong as the documents it validates — and a gate
    that dies with a stack trace on its own configuration is the failure
    mode this module exists to avoid. Every step of `load_schema` is
    ordered so the next one is safe to take; each case here is one of
    those steps, and every one also proves a real operation refused
    without writing.
    """
    print("\n-- a broken schema file refuses cleanly --")
    with registry(), scratch() as root:
        census = root / "probe_census.json"
        seeded(census)
        before = census.read_bytes()
        good = result_document()

        # Files `load_schema` itself must refuse, before it hands the
        # document to a library helper that would subscript it.
        unloadable = [
            # A valid-JSON schema that is not an object at all. The
            # library's `validator_for` SUBSCRIPTS what it is given, so
            # reaching it with a list raised out of the library.
            ('["$schema"]', "must be a JSON object", "a list"),
            ("5", "must be a JSON object", "a bare number"),
            ('"x"', "must be a JSON object", "a bare string"),
            ("true", "must be a JSON object", "a bare boolean"),
            ("null", "must be a JSON object", "a bare null"),
            ("{oops", "is not valid JSON", "text that is not JSON"),
            ("{}", "does not identify a JSON Schema draft", "no `$schema`"),
            ('{"$schema": 5}', "does not identify a JSON Schema draft",
             "a numeric `$schema`"),
            ('{"$schema": [1]}', "does not identify a JSON Schema draft",
             "an unhashable `$schema`"),
            ('{"$schema": "https://example.invalid/draft/9"}',
             "not a draft this jsonschema implements", "an unknown draft"),
            (json.dumps({"$schema": DRAFT,
                         "$defs": {"census_v2": {"type": 5}}}),
             "is not a valid", "a draft-invalid schema body"),
            (None, "is unreadable", "no schema file at all"),
        ]
        # And files that LOAD but cannot be applied: a schema can be a
        # perfectly valid schema and still not describe this tool's
        # documents, or not resolve its own references.
        # Every root definition dangles, so the failure is reached
        # whichever document an operation validates first.
        dangling = {"$schema": DRAFT,
                    "$defs": {name: {"$ref": "#/$defs/gone"}
                              for name in probe_census.SCHEMA_DEFINITIONS
                              .values()}}
        unusable = [
            (json.dumps({"$schema": DRAFT, "$defs": {"nothing": True}}),
             "declares no", "no definition for the documents it validates"),
            (json.dumps(dangling), "could not be applied",
             "a `$ref` naming nothing"),
        ]

        for text, fragment, why in unloadable:
            with schema_file(text, root):
                expect_refusal(probe_census.load_schema,
                               f"loading the schema refuses {why}", fragment)
                _refuses_every_operation(census, before, good, fragment, why)
        for text, fragment, why in unusable:
            with schema_file(text, root):
                expect_valid(probe_census.load_schema,
                             f"the schema itself loads with {why}")
                _refuses_every_operation(census, before, good, fragment, why)

        # And the shipped schema still loads, so no case above leaked
        # global state into the rest of the suite.
        expect_valid(probe_census.load_schema,
                     "the shipped schema still loads afterwards")
        expect_valid(lambda: probe_census.record_result(census, good),
                     "...and a real measurement still records")


# ==========================================================================
class _BlockedImport:
    """A meta-path finder that makes one package deterministically absent."""

    def __init__(self, name: str):
        self.name = name

    def find_spec(self, fullname, path=None, target=None):
        if fullname == self.name or fullname.startswith(f"{self.name}."):
            raise ImportError(
                f"blocked by the missing-dependency case: {fullname}")
        return None


@contextmanager
def without_jsonschema():
    """`import jsonschema` fails for the duration, and nothing else does.

    Deliberately NOT a monkeypatched flag inside `probe_census`: the
    promise under test is about the ENVIRONMENT, so the import itself
    has to fail. Any already-imported submodule is purged too, or the
    blocked import would be served from `sys.modules`.
    """
    blocked = _BlockedImport("jsonschema")
    purged = {name: module for name, module in sys.modules.items()
              if name == "jsonschema" or name.startswith("jsonschema.")}
    for name in purged:
        del sys.modules[name]
    sys.meta_path.insert(0, blocked)
    try:
        yield
    finally:
        sys.meta_path.remove(blocked)
        sys.modules.update(purged)


def test_missing_dependency() -> None:
    """An absent `jsonschema` is one loud error, never a silent skip.

    A validator that quietly enforces nothing is worse than no validator
    at all: the run looks clean and the gate is gone. So every case here
    asserts BOTH halves — the refusal happened, and the operation it
    refused wrote nothing.
    """
    print("\n-- an absent jsonschema refuses loudly --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        before = path.read_bytes()
        good = result_document()
        # Warm every cache first. `_require_jsonschema` runs BEFORE the
        # schema and validator caches are consulted precisely so a
        # previously working environment cannot satisfy a later
        # validation, and priming here is what proves it.
        probe_census.validate_result(good)
        probe_census.load_schema()

        with without_jsonschema():
            expect_refusal(lambda: probe_census.validate_result(good),
                           "validation refuses when the library is absent",
                           "jsonschema is required", probe_census.INSTALL_HINT)
            expect_refusal(lambda: probe_census.load_schema(),
                           "a primed schema cache does not satisfy it either",
                           probe_census.INSTALL_HINT)
            for name, operation in (
                ("--record", lambda: probe_census.record_result(path, good)),
                ("--seed", lambda: probe_census.ensure_document(path)),
                ("a policy update", lambda: probe_census.record_policy(
                    path, "alpha", acceptable_failures=1)),
            ):
                expect_refusal(operation,
                               f"{name} refuses without the library",
                               probe_census.INSTALL_HINT)
                expect(path.read_bytes() == before,
                       f"...and {name} wrote nothing")

        expect_valid(lambda: probe_census.validate_result(good),
                     "and validation works again once the library is back")

    # Through the CLI, where the exit code and the streams are the
    # contract: one non-zero refusal, the install command named once,
    # no traceback, and no success line.
    with registry(ci_eligible={"beta"}), cli_repo() as (_root, path):
        cli("--seed")
        before = path.read_bytes()
        holding = Path(tempfile.mkdtemp(prefix="probe-census-nodep-"))
        try:
            result_file = holding / "result.json"
            result_file.write_text(json.dumps(result_document()),
                                   encoding="utf-8")
            with without_jsonschema():
                code, out, err = cli("--record", str(result_file))
                expect(code == 1,
                       "--record exits non-zero with no jsonschema")
                expect(err.count(probe_census.INSTALL_HINT) == 1,
                       "...naming the install command exactly once")
                expect("Traceback" not in err, "...with no traceback")
                expect(out == "", "...and printing no success line")
                expect(path.read_bytes() == before,
                       "...and leaving the census byte-for-byte alone")
                code, _, err = cli("--validate")
                expect(code == 1 and probe_census.INSTALL_HINT in err,
                       "--validate refuses rather than validating nothing")
                expect(path.read_bytes() == before, "...and changes no bytes")
                # `--print` reads and writes nothing and validates
                # nothing — it renders the live registry — so it is not
                # a skipped check, and keeping it dependency-free is
                # what lets a fresh checkout run it.
                code, out, _ = cli("--print")
                expect(code == 0 and '"probe-census/v5"' in out,
                       "--print still works: it validates nothing to skip")
            code, _, _ = cli("--record", str(result_file))
            expect(code == 0,
                   "and the same command succeeds once the library is back")
        finally:
            shutil.rmtree(holding, ignore_errors=True)


# ==========================================================================
CONTENDER = """
import json, sys
sys.path.insert(0, {tools!r})
import probe_census
probe_census.record_result({path!r}, json.loads(sys.argv[1]))
"""


def test_independent_process_contention() -> None:
    print("\n-- independent-process contention --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        program = CONTENDER.format(tools=str(Path(__file__).resolve().parent),
                                   path=str(path))

        # Two SEPARATE processes, each appending to the SAME probe. A
        # thread-only test would not exercise the cross-process flock at
        # all; a lost update here means one of the two disappears.
        documents = [
            json.dumps(result_document(commit=COMMIT_A,
                                       timestamp_utc="2026-08-21T05:00:00Z")),
            json.dumps(result_document(commit=COMMIT_A,
                                       timestamp_utc="2026-08-21T06:00:00Z")),
        ]
        processes = [subprocess.Popen([sys.executable, "-c", program, doc],
                                      stdout=subprocess.PIPE,
                                      stderr=subprocess.PIPE, text=True)
                     for doc in documents]
        outcomes = [(p.wait(timeout=120), p.communicate()) for p in processes]
        expect(all(code == 0 for code, _ in outcomes),
               f"both writers succeeded ({[o[1][1][-200:] for o in outcomes]})")

        census = json.loads(path.read_text(encoding="utf-8"))["probes"][0]["census"]
        stamps = sorted(s["timestamp_utc"] for s in census["current"]["samples"])
        expect(stamps == ["2026-08-21T05:00:00Z", "2026-08-21T06:00:00Z"],
               f"both concurrent samples survive, neither lost ({stamps})")
        expect(len(census["attempts"]) == 2,
               f"both attempts survive ({len(census['attempts'])})")
        expect(len(census["current"]["samples"]) == 2
               and census["history"] == [],
               "both landed in the one shared commit cohort")


# ==========================================================================
def test_unusable_docs_worktree() -> None:
    """A registered-but-gone docs worktree is exit 2, never a write (#1503)."""
    print("\n-- an unusable docs worktree --")
    with registry(), cli_repo() as (main_wt, path):
        code, _, _ = cli("--seed")
        expect(code == 0 and path.exists(),
               "the seed lands while the docs worktree is real")
        docs_wt = path.parent.parent

        # Git keeps LISTING a worktree whose directory is gone, marking
        # the record prunable. Returning that path anyway would let the
        # writer recreate the tree and publish the census outside any
        # worktree at all.
        shutil.rmtree(docs_wt)
        listing = subprocess.run(["git", "worktree", "list", "--porcelain"],
                                 cwd=str(main_wt), text=True,
                                 capture_output=True).stdout
        expect("refs/heads/docs-wip" in listing,
               "git still lists the deleted worktree, so resolution must "
               "reject it rather than trust the listing")

        for argv, what in ((("--seed",), "--seed"),
                           (("--validate",), "--validate"),
                           (("--probe", "alpha", "--set-estimate", "5"),
                            "a policy update")):
            code, _, err = cli(*argv)
            expect(code == 2, f"{what} on a prunable docs worktree exits 2")
            expect("git worktree add" in err and "git worktree prune" in err,
                   f"{what} names both halves of the repair")
        expect(not docs_wt.exists(),
               "and nothing recreated the deleted worktree directory")

        # Recreating the directory does not make the registration usable
        # again — git still calls it prunable, because the admin gitdir
        # pointer is what is broken.
        docs_wt.mkdir(parents=True)
        code, _, err = cli("--seed")
        expect(code == 2 and "git worktree prune" in err,
               "recreating the directory does not revive the registration")
        expect(not (docs_wt / "docs").exists(),
               "and no census directory was created inside it")

        expect_refusal_kind(
            probe_census.DocsWorktreeMissing,
            lambda: probe_census.resolve_docs_worktree(str(main_wt)),
            "resolve_docs_worktree itself raises DocsWorktreeMissing")

    # The belt-and-braces half of the same rule, for a git that reports
    # no `prunable` attribute: a listed path that is not a checkout is
    # refused on its own, without being trusted into the writer.
    class Listed:
        returncode = 0
        stderr = ""

        def __init__(self, stdout):
            self.stdout = stdout

    with scratch() as root:
        saved = probe_census.subprocess.run
        gone = root / "never-existed"
        empty = root / "not-a-checkout"
        empty.mkdir()
        for target, why in ((gone, "a listed path that does not exist"),
                            (empty, "a listed path with no .git")):
            listing = (f"worktree {target}\nHEAD 1\n"
                       f"branch refs/heads/docs-wip\n")
            probe_census.subprocess.run = (
                lambda *a, _out=listing, **k: Listed(_out))
            try:
                expect_refusal_kind(
                    probe_census.DocsWorktreeMissing,
                    lambda: probe_census.resolve_docs_worktree(str(root)),
                    f"{why} is refused even with no prunable attribute")
                try:
                    probe_census.resolve_docs_worktree(str(root))
                except probe_census.DocsWorktreeMissing as error:
                    expect("not a usable checkout" in str(error),
                           f"...naming why ({why})")
            finally:
                probe_census.subprocess.run = saved
        expect(not gone.exists(),
               "and resolution never created the missing directory")

    # The record parser reads `prunable` as an attribute of its own
    # record, not as a line anywhere in the listing.
    records = probe_census._worktree_records(
        "worktree /a\nHEAD 1\nbranch refs/heads/master\n\n"
        "worktree /b\nHEAD 1\nbranch refs/heads/docs-wip\n"
        "prunable gitdir file points to non-existent location\n")
    expect(len(records) == 2 and "prunable" not in records[0]
           and records[1]["prunable"].startswith("gitdir file"),
           "prunable attaches to its own record, never a neighbour's")
    bare = probe_census._worktree_records(
        "worktree /a\nHEAD 1\ndetached\n")
    expect(len(bare) == 1 and bare[0]["detached"] == "",
           "a valueless porcelain attribute parses as an empty string")


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


def test_cli_justification() -> None:
    """#1479: the CLI never clears the stored justification by omission.

    These cases drive `main` — the argparse layer and its forwarding
    into `record_policy` — because the defect was in that forwarding,
    not in `set_policy`, which already distinguished "leave alone" from
    "clear" with a sentinel the CLI never sent.
    """
    print("\n-- the justification is durable typed policy (#1479) --")
    with registry(ci_eligible={"beta"}), cli_repo() as (_, path):
        cli("--seed")

        def stored(key="alpha"):
            rows = json.loads(path.read_text(encoding="utf-8"))["probes"]
            return {row["key"]: row["census"] for row in rows}[key]

        # (a) an X-only update preserves the stored justification. This
        # is the case that fails against the unconditional forwarding.
        code, _, _ = cli("--probe", "alpha", "--set-acceptable-failures", "5",
                         "--justification", "two known engine-side races")
        expect(code == 0 and stored()["acceptable_failures"] == 5
               and stored()["acceptable_failures_justification"]
               == "two known engine-side races",
               "the CLI stores X and its justification together")
        code, _, err = cli("--probe", "alpha", "--set-acceptable-failures", "7")
        expect(code == 0 and stored()["acceptable_failures"] == 7
               and stored()["acceptable_failures_justification"]
               == "two known engine-side races",
               "raising X without --justification KEEPS the stored "
               "justification (#1479 requirement 1)")
        # Requirement 1 promises the same for the lowering path, which
        # is the one that used to clear the text as a side effect.
        # (#1430 replaced its `none` spelling: there is no null X.)
        code, _, _ = cli("--probe", "alpha", "--set-acceptable-failures", "1")
        expect(code == 0 and stored()["acceptable_failures"] == 1
               and stored()["acceptable_failures_justification"]
               == "two known engine-side races",
               "lowering X without --justification keeps it too")

        # (b) the explicit clear, and only the explicit clear, clears —
        # and since #1430 it is valid only while setting X back to 0,
        # because an X above 0 must state why it is there.
        code, _, _ = cli("--probe", "alpha", "--set-acceptable-failures", "0",
                         "--clear-justification")
        expect(code == 0 and stored()["acceptable_failures"] == 0
               and stored()["acceptable_failures_justification"] is None,
               "--clear-justification clears the stored justification")
        expect(stored()["acceptable_failures_justification"] is None
               and "acceptable_failures_justification" in stored(),
               "...to null, the same absent-vs-present meaning as the seed")
        code, _, err = cli("--probe", "alpha", "--clear-justification")
        expect(code != 0 and "--clear-justification" in err,
               "--clear-justification alone is an argument error")
        code, _, err = cli("--probe", "alpha", "--set-acceptable-failures", "3",
                           "--justification", "x", "--clear-justification")
        expect(code != 0 and "--clear-justification" in err,
               "--justification and --clear-justification together are "
               "refused, never silently resolved")
        before = path.read_bytes()
        code, _, err = cli("--probe", "alpha", "--set-acceptable-failures", "3",
                           "--clear-justification")
        expect(code != 0 and "--clear-justification" in err
               and "0" in err,
               "--clear-justification while raising X is refused: a "
               "tolerance may not be left with no stated reason")
        expect(path.read_bytes() == before,
               "...and that argument refusal never reached the census")

        # (c) every literal round-trips: no in-band magic string.
        for text in ("keep", "none", "  padded  ", "KEEP"):
            code, _, _ = cli("--probe", "alpha", "--set-acceptable-failures",
                             "4", "--justification", text)
            expect(code == 0
                   and stored()["acceptable_failures_justification"] == text,
                   f"--justification {text!r} round-trips as stored text")
        expect(stored()["acceptable_failures"] == 4
               and stored("beta") == probe_census.empty_census(),
               "and no justification command disturbed X or another row")

        # #1430: the staging spelling #1428 accepted is now refused,
        # naming the value that replaced it.
        before = path.read_bytes()
        for value, why, fragment in (
                ("none", "there is no null X", "none"),
                ("10", "an X of 10 accepts a probe that never passes", "10"),
                ("-1", "a negative X", "-1"),
                ("true", "a boolean X", "true"),
                ("2.5", "a fractional X", "2.5")):
            code, _, err = cli("--probe", "alpha",
                               "--set-acceptable-failures", value)
            expect(code == 1 and "--set-acceptable-failures" in err
                   and fragment in err,
                   f"--set-acceptable-failures {value} is refused ({why})")
        expect(path.read_bytes() == before,
               "and not one of those refusals touched the census")


def test_cli() -> None:
    print("\n-- the CLI contract --")
    with registry(ci_eligible={"beta"}):
        # `--print` must not require, read or create the docs worktree.
        saved = probe_census.manifest_path
        probe_census.manifest_path = lambda *a, **k: (_ for _ in ()).throw(
            AssertionError("--print resolved the docs worktree"))
        try:
            code, out, _ = cli("--print")
            document = json.loads(out)
            expect(code == 0 and document["schema"] == "probe-census/v5",
                   "--print emits the v5 census the live registry implies")
            expect(all(row["census"] == probe_census.empty_census()
                       for row in document["probes"]),
                   "--print gives every row an empty census record")
            # Returning early must not be a hole in the companion-flag
            # rules: a misused flag is an error for EVERY operation.
            code, out, err = cli("--print", "--probe", "alpha")
            expect(code == 1 and "--probe" in err and out == "",
                   "--print --probe is an argument error, not a silent print")
            code, out, err = cli("--print", "--probe", "")
            expect(code == 1 and "--probe" in err and out == "",
                   "an empty --probe is still a supplied --probe")
            code, out, err = cli("--print", "--justification", "x")
            expect(code == 1 and "--justification" in err and out == "",
                   "--print --justification is an argument error")
            code, _, err = cli("--validate", "--justification", "x")
            expect(code == 1 and "--justification" in err,
                   "--validate --justification is an argument error, checked "
                   "before the docs worktree is resolved")
            code, _, err = cli("--record", "/nonexistent.json", "--probe", "a")
            expect(code == 1 and "--probe" in err,
                   "--record takes no --probe: it names its row itself")
        except AssertionError as error:
            expect(False, str(error))
        finally:
            probe_census.manifest_path = saved

        # A repository with no docs-wip worktree: exit 2, actionable.
        with scratch() as bare:
            subprocess.run(["git", "init", "-q", str(bare / "solo")],
                           check=True, capture_output=True)
            was = probe_engine.REPO_ROOT
            probe_engine.REPO_ROOT = str(bare / "solo")
            try:
                code, _, err = cli("--validate")
                expect(code == 2 and "git worktree add" in err,
                       "a missing docs worktree exits 2 with its repair")
            finally:
                probe_engine.REPO_ROOT = was

    with registry(ci_eligible={"beta"}), cli_repo() as (_root, path):
        code, out, _ = cli("--seed")
        expect(code == 0 and path.exists() and "probe-census/v5" in out,
               "--seed creates the census in the docs worktree")
        code, _, _ = cli("--validate")
        expect(code == 0, "--validate accepts the freshly seeded v5 census")

        holding = Path(tempfile.mkdtemp(prefix="probe-census-results-"))
        good = holding / "result.json"
        good.write_text(json.dumps(result_document()), encoding="utf-8")

        # A v1 document is reported as schema drift, and NOT migrated.
        path.write_text(json.dumps(v1_document()) + "\n", encoding="utf-8")
        legacy = path.read_bytes()
        code, _, err = cli("--validate")
        expect(code == 1 and "probe-census/v1" in err and "schema" in err,
               "--validate reports a v1 census as schema drift, exit 1")
        expect(path.read_bytes() == legacy,
               "--validate did not migrate it as a side effect")
        code, _, err = cli("--record", str(good))
        expect(code == 1 and "--seed" in err,
               "--record on an unmigrated census names --seed, exit 1")
        expect(path.read_bytes() == legacy, "...and changes no bytes")
        code, _, err = cli("--probe", "alpha", "--set-estimate", "5")
        expect(code == 1 and "--seed" in err,
               "a policy update on an unmigrated census names --seed, exit 1")
        expect(path.read_bytes() == legacy, "...and changes no bytes")
        cli("--seed")

        # Drift is reported, not repaired, by --validate.
        with registry(probes=SYNTHETIC + [("delta", "delta_probe.py", "new")],
                      ci_eligible={"beta"}):
            code, _, err = cli("--validate")
            expect(code == 1 and "delta" in err,
                   "--validate reports a newly registered probe as drift")
            code, _, _ = cli("--seed")
            expect(code == 0, "--seed reconciles that drift")
            code, _, _ = cli("--validate")
            expect(code == 0, "...and --validate then agrees")

        try:
            code, out, _ = cli("--record", str(good))
            expect(code == 0 and "alpha" in out,
                   "--record ingests a measurement, naming the probe itself")
            census = json.loads(path.read_text(encoding="utf-8"))["probes"][0]["census"]
            expect(len(census["current"]["samples"]) == 1,
                   "...and the sample landed")

            code, _, err = cli("--record", str(holding / "nothing.json"))
            expect(code == 1 and "cannot read" in err,
                   "an unreadable result file is a controlled refusal")
            bad = holding / "bad.json"
            bad.write_text("{oops", encoding="utf-8")
            code, _, err = cli("--record", str(bad))
            expect(code == 1 and "not valid JSON" in err,
                   "a malformed result file is a controlled refusal")
            expect("Traceback" not in err,
                   "no refusal prints a traceback")
        finally:
            shutil.rmtree(holding, ignore_errors=True)

        before = path.read_bytes()
        code, _, err = cli("--probe", "alpha", "--set-acceptable-failures", "2",
                           "--justification", "two races")
        expect(code == 0, "a policy update through the CLI succeeds")
        stored = path.read_bytes()
        code, _, err = cli("--set-acceptable-failures", "2")
        expect(code == 1 and "--probe" in err,
               "a policy flag with no --probe exits 1")
        code, _, err = cli("--set-acceptable-failures", "2", "--justification",
                           "none", "--probe", "")
        expect(code == 1 and "--probe" in err,
               "an empty --probe is no --probe at all for a policy update")
        code, _, err = cli("--record", str(good), "--justification", "x")
        expect(code == 1 and "--justification" in err,
               "--justification without --set-acceptable-failures exits 1")
        code, _, err = cli("--seed", "--probe", "alpha")
        expect(code == 1 and "--probe" in err,
               "--probe is exclusive to the policy operations")
        code, _, err = cli("--probe", "alpha", "--set-estimate", "soon")
        expect(code == 1 and "number" in err,
               "a non-numeric estimate exits 1")
        code, _, _ = cli("--probe", "alpha", "--set-acceptable-failures", "2",
                         "--set-estimate", "5")
        expect(code != 0,
               "the two --set-* flags together are rejected, non-zero")
        code, _, _ = cli()
        expect(code != 0, "no operation at all is rejected, non-zero")
        expect(path.read_bytes() == stored,
               "no argument-combination error read or wrote the census")
        after = json.loads(path.read_text(encoding="utf-8"))["probes"][0]["census"]
        expect(after["acceptable_failures"] == 2
               and after["acceptable_failures_justification"] == "two races",
               "and the stored policy is exactly what was set")


# ==========================================================================
# The cross-field invariants (#1493)
# ==========================================================================
def _alpha(document: dict) -> dict:
    """The census record of `rich_census()`'s measured row."""
    return document["probes"][0]["census"]


def _harness_attempt(accepted: bool, **overrides) -> dict:
    """One well-formed harness-error attempt, from the real summarizer."""
    record = probe_census.summarize_attempt(
        result_document(status="harness-error"), accepted)
    record.update(overrides)
    return record


def _drop_attempt(document: dict) -> None:
    _alpha(document)["attempts"].pop()


def _lose_the_samples(document: dict) -> None:
    census = _alpha(document)
    census["current"] = None
    census["history"] = []


def _forge_accepted_flag(document: dict) -> None:
    _alpha(document)["attempts"][0]["accepted"] = False


def _forge_harness_accepted(document: dict) -> None:
    _alpha(document)["attempts"].append(_harness_attempt(True))


def _finished_harness_error(document: dict) -> None:
    _alpha(document)["attempts"].append(
        _harness_attempt(False, requested_runs=2, completed_runs=2))


def _misfiled_sample(document: dict) -> None:
    _alpha(document)["current"]["samples"][0]["commit_sha"] = COMMIT_B


def _blank_deferral_resume_condition(document: dict) -> None:
    _alpha(document)["deferred"] = {
        "reason": "tree assets are incomplete",
        "resume_when": "   ",
    }


# Each stored case breaks exactly ONE relationship while staying
# schema-valid, and names the rule that must be the one rejecting it.
CENSUS_CASES = (
    (probe_census._rule_attempts_reconcile_with_samples,
     "accepted attempts left behind by cleared cohorts",
     "logs 2 accepted attempt(s) but retains 0 sample(s)",
     _lose_the_samples),
    (probe_census._rule_attempts_reconcile_with_samples,
     "a sample with no accepted attempt to log it",
     "logs 1 accepted attempt(s) but retains 2 sample(s)",
     _drop_attempt),
    (probe_census._rule_accepted_derives_from_status,
     "`accepted` false beside an accepted status",
     "`accepted` is derived from `status`",
     _forge_accepted_flag),
    (probe_census._rule_accepted_derives_from_status,
     "`accepted` true beside a harness error",
     "`accepted` is derived from `status`",
     _forge_harness_accepted),
    (probe_census._rule_attempt_leaves_a_run_uncompleted,
     "a logged harness error that completed every run",
     "reports completing 2 of 2 requested run(s)",
     _finished_harness_error),
    (probe_census._rule_cohort_holds_one_commit,
     "a sample filed under another commit's cohort",
     "a cohort holds one commit's samples",
     _misfiled_sample),
    (probe_census._rule_deferral_is_actionable,
     "a deferral with a blank resume condition",
     "has no non-blank deferral resume when",
     _blank_deferral_resume_condition),
)


def _pass_run_fails_a_check(result: dict) -> None:
    result["runs"][0]["checks"]["second"] = "FAIL"
    # Kept in step so the tally rule has nothing to say about it.
    result["check_counts"]["second"] = {"PASS": 0, "FAIL": 2, "MISSING": 0}


def _wrong_tally(result: dict) -> None:
    result["check_counts"]["first"]["PASS"] = 1


def _untallied_check(result: dict) -> None:
    """A check a run reports that `check_counts` has no entry for.

    Undeclared on BOTH sides, so the descriptor-coverage rule sees a map
    still keyed by exactly the declared checks and stays silent: the
    tally rule is the only one that can notice the run.
    """
    result["runs"][0]["checks"]["ghost"] = "PASS"


def _undeclared_tally(result: dict) -> None:
    """An entry for a check the descriptor never declared.

    Its tally is all zero, which is precisely what the per-entry
    comparison accepts — no run shows anything for it, so the numbers
    agree. Only the keying rule can reject it.
    """
    result["check_counts"]["ghost"] = {"PASS": 0, "FAIL": 0, "MISSING": 0}


def _untallied_declared_check(result: dict) -> None:
    """A declared check with no entry, in a measurement with NO runs.

    A harness error on the very first run completes nothing, so every
    entry is all zero and the runs show nothing at all — which is what
    leaves the tally rule with nothing to say and isolates the loss of
    the key itself.
    """
    result.update(_no_runs_result())
    del result["check_counts"]["second"]


def _finished_harness_result(result: dict) -> None:
    """A real harness error, then its broken run counted as completed.

    Built from the realistic fixture rather than by flipping an accepted
    measurement's status, so `completed_runs` is the only field left
    disagreeing with the rest.
    """
    result.update(result_document(status="harness-error"))
    result["completed_runs"] = result["requested_runs"]


RESULT_CASES = (
    (probe_census._rule_pass_run_has_no_failed_check,
     "a PASS run carrying a FAIL check",
     "a failed check makes its run fail",
     _pass_run_fails_a_check),
    (probe_census._rule_check_counts_tally_runs,
     "a tally that is not what the runs show",
     "is not the PASS=2 FAIL=0 MISSING=0 `runs` shows",
     _wrong_tally),
    (probe_census._rule_check_counts_tally_runs,
     "a check tallied in the runs with no entry",
     "but has no entry",
     _untallied_check),
    (probe_census._rule_check_counts_cover_the_descriptor,
     "an all-zero tally for a check the descriptor never declared",
     "the probe's own descriptor does not declare it",
     _undeclared_tally),
    (probe_census._rule_check_counts_cover_the_descriptor,
     "a declared check with no tally at all",
     "declared check 'second' has no tally",
     _untallied_declared_check),
    (probe_census._rule_result_leaves_a_run_uncompleted,
     "a harness error that completed every requested run",
     "reports completing 2 of 2 requested run(s)",
     _finished_harness_result),
)


@contextmanager
def without_rule(rule):
    """The production rule set with exactly `rule` lifted out of it.

    This is the mutation check the issue requires, run rather than
    asserted: with one rule gone its own case must be ACCEPTED, which is
    what proves that rule is the one rejecting it and that the fixture
    isolates a single relationship. A neighbouring rule catching the
    same fixture would keep refusing it here and fail the case.
    """
    saved = (probe_census.CENSUS_RULES, probe_census.RESULT_RULES)
    probe_census.CENSUS_RULES = tuple(
        r for r in probe_census.CENSUS_RULES if r is not rule)
    probe_census.RESULT_RULES = tuple(
        r for r in probe_census.RESULT_RULES if r is not rule)
    try:
        yield
    finally:
        probe_census.CENSUS_RULES, probe_census.RESULT_RULES = saved


def test_cross_field_invariants() -> None:
    """The rules that span fields, which no schema keyword can state.

    Three things are proved for every rule, because "reject malformed
    input" fails by over-rejecting just as readily as by
    under-rejecting: a rejecting case driven through a REAL operation on
    a real census (refused, and not one byte rewritten), a positive case
    built the way the producer really builds one, and a mutation check
    that lifts that one rule out of the production rule set and requires
    its own case to be accepted again.
    """
    print("\n-- the cross-field invariants --")
    expect(len(probe_census.CENSUS_RULES) == len(
        {rule for rule, _why, _fragment, _mutate in CENSUS_CASES}),
        "every stored rule has a case of its own")
    expect(len(probe_census.RESULT_RULES) == len(
        {rule for rule, _why, _fragment, _mutate in RESULT_CASES}),
        "every intake rule has a case of its own")

    with registry(), scratch() as root:
        target = root / "probe_census.json"
        clean = root / "clean.json"
        probe_census.ensure_document(clean)
        clean_bytes = clean.read_bytes()

        # -- stored state: refused by every operation, and unchanged ---
        # A census this tool cannot trust must stop `--record`, the
        # policy updates, `--seed` and `--validate` alike: the defect
        # #1493 was filed for is that they each rewrote the file and so
        # made the inconsistency durable.
        operations = (
            ("--record", lambda p: probe_census.record_result(
                p, result_document(commit=COMMIT_B))),
            ("--set-acceptable-failures", lambda p: probe_census.record_policy(
                p, "alpha", acceptable_failures=1)),
            ("--seed", probe_census.ensure_document),
            ("--validate", lambda p: probe_census.validate_census(
                probe_census.load(p), f"census {p}")),
        )
        for rule, why, fragment, mutate in CENSUS_CASES:
            document = rich_census()
            mutate(document)
            target.write_text(json.dumps(document), encoding="utf-8")
            stored = target.read_bytes()
            expect_valid(
                lambda d=document: probe_census.validate_document(
                    d, probe_census.CENSUS_SCHEMA, "the case"),
                f"a census with {why} is still SCHEMA-valid, so only the "
                f"cross-field rule can reject it")
            for name, operation in operations:
                expect_refusal(lambda o=operation: o(target),
                               f"{name} refuses a census with {why}", fragment)
                expect(target.read_bytes() == stored,
                       f"...and rewrote nothing ({name}, {why})")
            with without_rule(rule):
                expect_valid(
                    lambda d=document: probe_census.validate_census(
                        d, "the case"),
                    f"mutation check: without {rule.__name__}, {why} is "
                    f"accepted — that rule is what rejects it")

        # -- the intake surface: refused, and nothing written ----------
        for rule, why, fragment, mutate in RESULT_CASES:
            document = result_document()
            mutate(document)
            expect_valid(
                lambda d=document: probe_census.validate_document(
                    d, probe_census.RESULT_SCHEMA, "the case"),
                f"a result with {why} is still SCHEMA-valid")
            expect_refusal(
                lambda d=document: probe_census.record_result(clean, d),
                f"--record refuses a result with {why}", fragment)
            expect(clean.read_bytes() == clean_bytes,
                   f"...and wrote nothing ({why})")
            with without_rule(rule):
                expect_valid(
                    lambda d=document: probe_census.validate_result(d),
                    f"mutation check: without {rule.__name__}, {why} is "
                    f"accepted — that rule is what rejects it")

        # The harness-error relationship is guarded on BOTH surfaces,
        # deliberately: the intake rule refuses the document, and the
        # stored rule refuses the attempt it would have become. Lifting
        # the intake one out therefore does NOT let the write through —
        # which is the defence in depth, and worth pinning rather than
        # hiding inside a mutation check that dodges it.
        finished = result_document()
        _finished_harness_result(finished)
        with without_rule(probe_census._rule_result_leaves_a_run_uncompleted):
            expect_refusal(
                lambda: probe_census.record_result(clean, finished),
                "the stored rule still catches a finished harness error "
                "the intake rule was not there to refuse",
                "reports completing 2 of 2 requested run(s)")
        expect(clean.read_bytes() == clean_bytes,
               "...and that refusal wrote nothing either")

    # -- the positive half: every legitimate flow still goes through ---
    # Over-rejection is this kind of rule's real failure mode, so the
    # accepting cases are the load-bearing ones. They come from the
    # producer's own serialization where they can, not from a
    # hand-written document that could agree with the fixtures while
    # both drifted from what probe_flake.py writes.
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        expect(probe_census.census_invariants(probe_census.build_manifest())
               == [], "an empty census reconciles 0 accepted against 0 "
                      "retained")
        probe_census.ensure_document(path)

        descriptor = probe_flake.probe_protocol.build_descriptor(
            "alpha", [("first", "the first check"),
                      ("second", "the second check")])

        def measurement(requested: int) -> probe_flake.Measurement:
            return probe_flake.Measurement(
                "alpha", descriptor, requested_runs=requested,
                rts_caps=probe_flake.DEFAULT_RTS_CAPS,
                artifact_root=Path("/tmp/artifacts"),
                invocation_dir=Path("/tmp/artifacts/alpha-1"))

        # A TIMEOUT wins outright, so a timed-out run really can carry a
        # FAIL check. The PASS rule must not reach it.
        timed_out = measurement(1)
        timed_out.runs.append(probe_flake.RunRecord(
            1, 9100, probe_flake.RUN_TIMEOUT, 900.0,
            {"first": "PASS", "second": "FAIL"},
            Path("/tmp/artifacts/alpha-1/run-001")))
        expect_valid(
            lambda: probe_census.record_result(path, timed_out.to_document()),
            "a TIMEOUT run carrying a FAIL check is accepted")

        # A harness error on the very FIRST run completes nothing, so
        # `check_counts` is the descriptor seeded to all zeros — the one
        # legitimate shape in which an entry counts nothing, and the
        # reason the keying rule cannot be inferred from the tallies.
        nothing_ran = measurement(3)
        nothing_ran.status = "harness-error"
        nothing_ran.error = "run 1 emitted a duplicate event"
        nothing_ran.error_run = probe_flake.RunRecord(
            1, 9101, probe_flake.RUN_HARNESS_ERROR, 0.5, {},
            Path("/tmp/artifacts/alpha-1/run-001"))
        produced = nothing_ran.to_document()
        expect_valid(
            lambda: probe_census.record_result(path, produced),
            "a harness error that completed no run at all is accepted")
        expect(set(produced["check_counts"])
               == {check["id"] for check in produced["checks"]}
               and all(sum(tally.values()) == 0
                       for tally in produced["check_counts"].values()),
               "the producer keys check_counts by exactly its descriptor, "
               "all zero when no run completed")
        expect(set(_no_runs_result()["check_counts"])
               == set(produced["check_counts"]),
               "...and this file's no-runs fixture is keyed the same way")
        expect_valid(lambda: probe_census.validate_result(_no_runs_result()),
                     "...so the fixture the keying case builds on is itself "
                     "consistent")

        # And the same keying holds once runs DO complete.
        expect(set(result_document()["check_counts"])
               == {check["id"] for check in result_document()["checks"]},
               "a completed measurement is keyed by its descriptor too")

        # Two measurements of ONE commit accumulate in a single cohort;
        # a third naming another commit rolls that whole cohort into
        # history. Both are legitimate retention, and the equality has
        # to survive each. (The producer-built measurements above stamp
        # the REAL checkout commit, so they sit in a cohort of their
        # own — which is exactly the multi-cohort state the equality is
        # summed across.)
        for index in (1, 2):
            expect_valid(lambda: probe_census.record_result(path,
                                                            result_document()),
                         f"accepted measurement {index} of one commit is "
                         f"accepted")
        rollover = result_document(commit=COMMIT_B)
        expect_valid(
            lambda: probe_census.record_result(path, rollover),
            "and one naming a new commit, which rolls the cohort over")
        stored = json.loads(path.read_text(encoding="utf-8"))
        census = stored["probes"][0]["census"]
        archived = {cohort["commit_sha"]: len(cohort["samples"])
                    for cohort in census["history"]}
        expect(archived.get(COMMIT_A) == 2
               and census["current"]["commit_sha"] == COMMIT_B
               and len(census["current"]["samples"]) == 1,
               f"the rollover archived the two-sample cohort rather than "
               f"dropping it ({archived})")
        expect(len(census["attempts"]) == 5
               and sum(1 for a in census["attempts"]
                       if a["status"] == "ok") == 4,
               "and the harness error is logged without a sample")
        expect(probe_census.census_invariants(stored) == [],
               "accepted attempts still reconcile against retained samples "
               "across current AND history")

        # Promotion archives the current cohort; reconciliation appends
        # rows. Neither may disturb the equality.
        cohorts = len(census["history"])
        with registry(ci_eligible={"alpha"}):
            promoted = probe_census.ensure_document(path)
        expect(promoted["probes"][0]["census"]["current"] is None
               and len(promoted["probes"][0]["census"]["history"])
               == cohorts + 1,
               "promotion archived the current cohort rather than dropping it")
        expect_valid(
            lambda: probe_census.validate_census(promoted, "the promoted "
                                                           "census"),
            "a promoted, reconciled census still satisfies every invariant")


# ==========================================================================
# Cohort semantics: the current statistic, its age, and staleness (#1429)
# ==========================================================================
COMMIT_C = "c" * 40

# One fixed evaluation moment. Nothing in this section reads a clock:
# staleness is a function of an injected `now`, so a boundary case is a
# boundary case on every machine and at every hour.
NOW = datetime.datetime(2026, 8, 21, 12, 0, 0, tzinfo=datetime.timezone.utc)
DAY = probe_census.SECONDS_PER_DAY


def at(offset_days: float) -> str:
    """A census timestamp `offset_days` BEFORE the evaluation moment."""
    moment = NOW - datetime.timedelta(days=offset_days)
    return moment.strftime(probe_census.TIMESTAMP_FORMAT)


def measurement(commit=COMMIT_A, *, runs=2, failures=1, age_days=0.0,
                probe="alpha", **overrides):
    """One accepted result: a batch of `runs` with `failures` of them bad."""
    return result_document(
        probe=probe, commit=commit, requested_runs=runs, completed_runs=runs,
        failure_count=failures,
        failure_rate=None if runs == 0 else failures / runs,
        timestamp_utc=at(age_days), **overrides)


def summary_of(path: Path, probe="alpha", *, now=NOW,
               stale_after_seconds=14 * DAY) -> dict:
    document = json.loads(path.read_text(encoding="utf-8"))
    return probe_census.census_summary(
        document, now=now, stale_after_seconds=stale_after_seconds,
        probe=probe)[0]


def test_cohort_accumulation() -> None:
    print("\n-- cohort accumulation and the combined statistic --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)

        # Two UNEQUAL batches on one commit. Averaging the stored batch
        # rates would give (0.5 + 0.1) / 2 = 0.30; the combined
        # numerator and denominator give 2 / 12 = 0.1667.
        probe_census.record_result(path, measurement(runs=2, failures=1,
                                                     age_days=2))
        probe_census.record_result(path, measurement(runs=10, failures=1,
                                                     age_days=1))
        summary = summary_of(path)
        expect(summary["requested_runs"] == 12
               and summary["failure_count"] == 2,
               "same-commit runs accumulate as summed counts")
        expect(abs(summary["failure_rate"] - 2 / 12) < 1e-12,
               "the rate is recomputed from the combined numerator and "
               "denominator, not averaged across batches")
        expect(abs(summary["failure_rate"] - 0.30) > 1e-9,
               "an unweighted mean of the two batch rates (0.30) is NOT "
               "what a cohort of unequal batches reports")
        expect(summary["sample_count"] == 2 and summary["cohort"] == "current",
               "both samples belong to one current cohort")

        # The freshness anchor is the LATEST contributing timestamp, so
        # an out-of-order same-commit result adds counts without
        # dragging it backwards.
        probe_census.record_result(path, measurement(runs=4, failures=0,
                                                     age_days=9))
        summary = summary_of(path)
        expect(summary["requested_runs"] == 16
               and summary["sample_count"] == 3,
               "an older same-commit result still contributes its counts")
        expect(summary["measured_at"] == at(1),
               "the freshness anchor is the latest contributing timestamp, "
               "never the most recently appended one")


def test_cohort_append_order() -> None:
    print("\n-- append order, not hash comparison --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        # A -> B -> A. Commit hashes have no intrinsic ordering, so the
        # third measurement opens a THIRD cohort rather than reopening
        # or merging with the first.
        probe_census.record_result(path, measurement(COMMIT_A, age_days=6))
        probe_census.record_result(path, measurement(COMMIT_B, age_days=4))
        probe_census.record_result(path, measurement(COMMIT_A, runs=5,
                                                     failures=5, age_days=2))
        census = json.loads(path.read_text(encoding="utf-8"))["probes"][0]["census"]
        expect([cohort["commit_sha"] for cohort in census["history"]]
               == [COMMIT_A, COMMIT_B],
               "A -> B -> A archives A then B, in the order they stopped "
               "being current")
        expect(census["current"]["commit_sha"] == COMMIT_A
               and len(census["current"]["samples"]) == 1,
               "the third measurement opens a NEW cohort for A, and does "
               "not reopen the first")
        summary = summary_of(path)
        expect(summary["requested_runs"] == 5 and summary["failure_count"] == 5
               and summary["failure_rate"] == 1.0,
               "the current statistic is the newest cohort alone, never "
               "pooled with the earlier cohort of the same commit")
        expect(sum(len(cohort["samples"])
                   for cohort in census["history"]) == 2,
               "every displaced cohort keeps its samples")


def test_head_movement_is_not_a_census_event() -> None:
    print("\n-- repository HEAD moving is not a measurement --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        probe_census.record_result(path, measurement(COMMIT_A, age_days=3))
        before_bytes = path.read_bytes()
        before = summary_of(path)

        # Nothing here records anything; the repository simply moved on.
        # Staleness is purely age-based, so the stored statistic and its
        # commit are exactly what they were.
        after = summary_of(path)
        unchanged(path, before_bytes,
                  "a HEAD change writes no census bytes")
        expect(after == before,
               "no commit movement can change a census summary; only a "
               "measurement does")
        expect(after["commit_sha"] == COMMIT_A,
               "the current statistic still names the commit it was "
               "measured on, not repository HEAD")

        # And the census never consults git for a summary: the same
        # document summarizes identically with the live registry's repo
        # root pointed somewhere else entirely.
        saved = probe_engine.REPO_ROOT
        probe_engine.REPO_ROOT = str(root)
        try:
            expect(summary_of(path) == before,
                   "a summary reads the stored cohort, never the working "
                   "tree it happens to run in")
        finally:
            probe_engine.REPO_ROOT = saved


def test_staleness_boundary() -> None:
    print("\n-- age and the staleness boundary --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        probe_census.record_result(path, measurement(age_days=7))
        summary = summary_of(path, stale_after_seconds=14 * DAY)
        expect(summary["age_seconds"] == 7 * DAY,
               "age is the distance from the anchor to the evaluation time")
        expect(summary["stale"] is False,
               "a cohort younger than the horizon is fresh")
        expect(summary_of(path, stale_after_seconds=7 * DAY)["stale"] is True,
               "the boundary is inclusive: age EQUAL to the horizon is stale")
        just_under = summary_of(path, stale_after_seconds=7 * DAY + 1)
        expect(just_under["stale"] is False,
               "one second more horizon than age is fresh")
        expect(summary_of(path, stale_after_seconds=0)["stale"] is True,
               "a zero horizon calls every measured cohort stale")

        # A cohort anchored in the FUTURE is the freshest thing there
        # is, never a negative age that would sort ahead of every real
        # measurement.
        future = summary_of(path, now=NOW - datetime.timedelta(days=10))
        expect(future["age_seconds"] == 0.0 and future["stale"] is False,
               "age is clamped at zero, so a future-anchored cohort is "
               "fresh rather than negatively old")


def test_unmeasured_and_zero_rate() -> None:
    print("\n-- unmeasured is not a zero failure rate --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        never = summary_of(path, "gamma")
        expect(never["measured"] is False,
               "a probe with no cohort reports measured: false")
        expect(all(never[field] is None for field in
                   ("cohort", "commit_sha", "measured_at", "age_seconds",
                    "stale", "sample_count", "requested_runs",
                    "failure_count", "failure_rate")),
               "every measurement field of an unmeasured probe is null")
        expect(never["failure_rate"] is None
               and not isinstance(never["failure_rate"], (int, float)),
               "an unmeasured probe never reports a zero failure rate")
        expect(never["key"] == "gamma"
               and never["classification"] == "manual-only",
               "it still carries its inventory identity")

        probe_census.record_result(
            path, measurement(probe="gamma", runs=8, failures=0, age_days=1))
        clean = summary_of(path, "gamma")
        expect(clean["measured"] is True and clean["failure_rate"] == 0.0
               and clean["requested_runs"] == 8,
               "a probe measured eight times with no failure reports a real "
               "rate of 0.0")
        expect(clean["stale"] is False,
               "and a real zero rate is still classified for freshness")

        # A cohort with no denominator has no rate at all. Reporting
        # 0.0 there would spell "never failed" for runs nobody made.
        no_runs = measurement(probe="beta", commit=COMMIT_B, runs=0,
                              failures=0, age_days=1)
        no_runs["runs"] = []
        # No run completed, so every DECLARED check tallies zero — the
        # shape `Measurement.check_counts()` starts from and never
        # leaves when the loop appends nothing. An empty map would be a
        # different claim (a probe declaring no checks at all), which
        # #1493's keying rule refuses; the denominator this case is
        # about is `requested_runs`, which is untouched either way.
        no_runs["check_counts"] = {check["id"]: {"PASS": 0, "FAIL": 0,
                                                 "MISSING": 0}
                                   for check in no_runs["checks"]}
        probe_census.record_result(path, no_runs)
        empty = summary_of(path, "beta")
        expect(empty["measured"] is True and empty["requested_runs"] == 0
               and empty["failure_rate"] is None,
               "a cohort that requested no runs reports a null rate, not 0.0")


def test_history_only_statistic() -> None:
    print("\n-- a promoted probe's statistic lives in history --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        probe_census.record_result(path, measurement(COMMIT_A, runs=3,
                                                     failures=3, age_days=30))
        probe_census.record_result(path, measurement(COMMIT_B, runs=4,
                                                     failures=1, age_days=9))
        # Promotion archives the current cohort and does not restore it
        # on a later downgrade, so `current` is null while the newest
        # measured statistic is real and must still be reported.
        with registry(ci_eligible=("alpha",)):
            probe_census.ensure_document(path)
        census = json.loads(path.read_text(encoding="utf-8"))["probes"][0]["census"]
        expect(census["current"] is None and len(census["history"]) == 2,
               "promotion archives the current cohort, keeping both")

        summary = summary_of(path, stale_after_seconds=9 * DAY)
        expect(summary["measured"] is True and summary["cohort"] == "history",
               "a history-only record is MEASURED, summarized from the last "
               "archived cohort")
        expect(summary["commit_sha"] == COMMIT_B
               and summary["requested_runs"] == 4
               and summary["failure_count"] == 1,
               "the statistic comes from history[-1], not from the older "
               "cohort and not from both pooled")
        expect(summary["age_seconds"] == 9 * DAY and summary["stale"] is True,
               "its freshness is its own latest sample's age at the "
               "boundary, unaffected by having been archived")
        lenient = summary_of(path, stale_after_seconds=10 * DAY)
        expect(lenient["stale"] is False,
               "and archiving alone never makes a record stale")

        # A downgrade refreshes the classification and nothing else, so
        # the archived statistic remains the authoritative one.
        probe_census.ensure_document(path)
        after = summary_of(path, stale_after_seconds=9 * DAY)
        expect(after["cohort"] == "history"
               and after["commit_sha"] == COMMIT_B,
               "a downgrade restores no cohort, so history[-1] stays "
               "authoritative")


def test_cohort_semantic_refusals() -> None:
    print("\n-- unusable semantic values refuse, writing nothing --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        probe_census.record_result(path, measurement(age_days=1))
        before = path.read_bytes()

        # The placeholder `probe_flake` writes when git could not be
        # consulted is well-formed and schema-valid; it names no commit,
        # so it may not open or extend a cohort.
        expect_refusal(
            lambda: probe_census.record_result(
                path, measurement(commit="unknown", age_days=0)),
            "the `unknown` commit placeholder cannot open a cohort",
            "unknown", "no commit")
        unchanged(path, before, "and that refusal wrote nothing")

        for commit, why in (("a" * 39, "an abbreviated hash"),
                            ("A" * 40, "an uppercase hash"),
                            ("z" * 40, "a non-hex hash")):
            expect_refusal(
                lambda commit=commit: probe_census.record_result(
                    path, measurement(commit=commit)),
                f"{why} is refused as a cohort identity",
                "lowercase hex")
        unchanged(path, before, "and none of those wrote anything")

        # A harness error is deliberately NOT gated: it contributes to
        # no cohort, and unmeasurable provenance is exactly what the
        # attempt log retains.
        probe_census.record_result(path, result_document(
            status="harness-error", commit="unknown"))
        census = json.loads(path.read_text(encoding="utf-8"))["probes"][0]["census"]
        expect(census["attempts"][-1]["commit_sha"] == "unknown"
               and census["attempts"][-1]["accepted"] is False,
               "a harness error with unknown provenance is still logged")
        expect(len(census["current"]["samples"]) == 1
               and census["history"] == [],
               "and it opened no cohort")

        # The same checks fail closed over ALREADY-STORED state, which
        # is how a census written before them (or by hand) is caught.
        document = json.loads(path.read_text(encoding="utf-8"))
        document["probes"][0]["census"]["current"]["commit_sha"] = "unknown"
        expect_refusal(
            lambda: probe_census.census_summary(
                document, now=NOW, stale_after_seconds=DAY, probe="alpha"),
            "a stored cohort keyed by the placeholder refuses on READ",
            "unknown")

        stored = json.loads(path.read_text(encoding="utf-8"))
        stored["probes"][0]["census"]["current"]["samples"][0][
            "commit_sha"] = "unknown"
        expect_refusal(
            lambda: probe_census.census_summary(
                stored, now=NOW, stale_after_seconds=DAY, probe="alpha"),
            "a stored SAMPLE carrying the placeholder refuses on READ too",
            "sample 0", "unknown")

        stored = json.loads(path.read_text(encoding="utf-8"))
        stored["probes"][0]["census"]["current"]["samples"][0][
            "timestamp_utc"] = "2026-08-21 05:00:00"
        expect_refusal(
            lambda: probe_census.census_summary(
                stored, now=NOW, stale_after_seconds=DAY, probe="alpha"),
            "a stored sample whose timestamp cannot be read refuses on READ",
            "timestamp")

        stored = json.loads(path.read_text(encoding="utf-8"))
        stored["probes"][0]["census"]["current"]["samples"][0][
            "requested_runs"] = -3
        expect_refusal(
            lambda: probe_census.census_summary(
                stored, now=NOW, stale_after_seconds=DAY, probe="alpha"),
            "a stored negative run count refuses on READ",
            "negative")

        stored = json.loads(path.read_text(encoding="utf-8"))
        stored["probes"][0]["census"]["current"]["samples"] = []
        expect_refusal(
            lambda: probe_census.census_summary(
                stored, now=NOW, stale_after_seconds=DAY, probe="alpha"),
            "a stored cohort with no samples has no statistic and refuses",
            "no samples")

        # The append-or-archive decision READS the stored cohort, so an
        # unusable one refuses the whole ingestion rather than being
        # quietly extended (same commit) or archived into history
        # (different commit). Before this, a valid measurement landed
        # and only the later READ failed.
        # `unknown` is spelled on the cohort AND its samples, which is
        # what a census written when `git` could not be consulted really
        # looks like: `ingest_result` copies one commit into both. It
        # also keeps the cohort internally consistent, so #1493's
        # membership rule stays silent and this case really exercises
        # #1429's identity check rather than being pre-empted by it.
        # The second case is the reverse on purpose: a sample that
        # disagrees with its cohort is #1493's, and it is reached first,
        # which the fragments below pin so neither rule can quietly
        # stop covering its own shape.
        for damage, why, fragment in (
                (lambda c: c.update(
                    {"commit_sha": "unknown",
                     "samples": [{**sample, "commit_sha": "unknown"}
                                 for sample in c["samples"]]}),
                 "keyed by the placeholder", "unknown"),
                (lambda c: c["samples"][0].update({"commit_sha": "unknown"}),
                 "holding a placeholder sample",
                 "a cohort holds one commit's samples"),
                (lambda c: c["samples"][0].update(
                    {"timestamp_utc": "2026-08-21 05:00:00"}),
                 "holding an unreadable sample timestamp", "timestamp"),
                (lambda c: c["samples"][0].update({"requested_runs": -1}),
                 "holding a negative sample run count", "requested_runs")):
            with scratch() as damaged_root:
                damaged_path = damaged_root / "probe_census.json"
                seeded(damaged_path)
                probe_census.record_result(damaged_path,
                                           measurement(COMMIT_A, age_days=3))
                stored = json.loads(damaged_path.read_text(encoding="utf-8"))
                damage(stored["probes"][0]["census"]["current"])
                damaged_path.write_text(json.dumps(stored), encoding="utf-8")
                damaged_before = damaged_path.read_bytes()
                for follow_up, kind in ((COMMIT_A, "same-commit"),
                                        (COMMIT_B, "different-commit")):
                    expect_refusal(
                        lambda follow_up=follow_up:
                            probe_census.record_result(
                                damaged_path,
                                measurement(follow_up, age_days=0)),
                        f"a {kind} measurement onto a stored cohort {why} "
                        f"refuses", fragment)
                    unchanged(damaged_path, damaged_before,
                              f"...and that {kind} refusal wrote nothing")

        # A harness error never reads the current cohort, so a damaged
        # one does not stop the attempt log from recording the failure.
        with scratch() as damaged_root:
            damaged_path = damaged_root / "probe_census.json"
            seeded(damaged_path)
            probe_census.record_result(damaged_path,
                                       measurement(COMMIT_A, age_days=3))
            stored = json.loads(damaged_path.read_text(encoding="utf-8"))
            # Damaged the way a real placeholder-provenance census is —
            # cohort and samples alike — so this pins #1429's "a harness
            # error never reads the cohort" and not an incidental
            # #1493 membership violation the whole census would stop on.
            cohort = stored["probes"][0]["census"]["current"]
            cohort["commit_sha"] = "unknown"
            for sample in cohort["samples"]:
                sample["commit_sha"] = "unknown"
            damaged_path.write_text(json.dumps(stored), encoding="utf-8")
            probe_census.record_result(damaged_path, result_document(
                status="harness-error", commit=COMMIT_B))
            after = json.loads(
                damaged_path.read_text(encoding="utf-8"))["probes"][0]["census"]
            expect(len(after["attempts"]) == 2
                   and after["attempts"][-1]["accepted"] is False,
                   "a harness error still logs against a damaged stored "
                   "cohort, which it never reads")

        # The evaluation time and the horizon are inputs, and an
        # unusable one is a refusal rather than a substituted default.
        good = json.loads(path.read_text(encoding="utf-8"))
        expect_refusal(
            lambda: probe_census.census_summary(
                good, now=datetime.datetime(2026, 8, 21),
                stale_after_seconds=DAY),
            "a naive evaluation time is refused, never assumed to be UTC",
            "timezone-aware")
        expect_refusal(
            lambda: probe_census.census_summary(
                good, now=NOW, stale_after_seconds=-1),
            "a negative staleness horizon is refused",
            "nonnegative")
        expect_refusal(
            lambda: probe_census.census_summary(
                good, now=NOW, stale_after_seconds=float("inf")),
            "a non-finite staleness horizon is refused",
            "finite")
        expect_refusal(
            lambda: probe_census.census_summary(
                good, now=NOW, stale_after_seconds=DAY, probe="nonesuch"),
            "summarizing a probe with no census row refuses",
            "no census row")


def test_summary_preserves_everything() -> None:
    print("\n-- the summary is a pure reader --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        probe_census.record_result(path, measurement(COMMIT_A, age_days=20))
        probe_census.record_result(path, measurement(COMMIT_B, age_days=10))
        probe_census.record_result(path, measurement(COMMIT_C, age_days=1))
        before_bytes = path.read_bytes()
        document = json.loads(path.read_text(encoding="utf-8"))
        original = copy.deepcopy(document)

        summaries = probe_census.census_summary(
            document, now=NOW, stale_after_seconds=14 * DAY)
        expect(document == original,
               "summarizing mutates no part of the document it reads")
        unchanged(path, before_bytes, "and writes no bytes")
        expect(len(summaries) == len(SYNTHETIC)
               and [s["key"] for s in summaries]
               == [key for key, _script, _purpose in SYNTHETIC],
               "the whole-census view reports every row, in inventory order")

        census = document["probes"][0]["census"]
        expect([cohort["commit_sha"] for cohort in census["history"]]
               == [COMMIT_A, COMMIT_B]
               and census["current"]["commit_sha"] == COMMIT_C,
               "every displaced cohort is still retained after two "
               "displacements")
        expect(summaries[0]["commit_sha"] == COMMIT_C,
               "and the newest one is the authoritative statistic")


def test_summary_cli() -> None:
    print("\n-- the --summary CLI --")
    with registry(), cli_repo() as (_main_wt, census_path):
        cli("--seed")
        probe_census.record_result(census_path,
                                   measurement(runs=4, failures=1, age_days=8))

        code, out, err = cli("--summary", "--as-of", at(0),
                             "--stale-after-days", "7", "--json")
        expect(code == 0 and err == "", f"--summary --json exits 0 ({err!r})")
        payload = json.loads(out)
        alpha = next(row for row in payload if row["key"] == "alpha")
        expect(alpha["stale"] is True and alpha["age_seconds"] == 8 * DAY
               and alpha["requested_runs"] == 4,
               "--json reports the combined statistic and the injected age")
        expect(any(row["measured"] is False for row in payload),
               "and reports unmeasured probes as such")

        code, out, _ = cli("--summary", "--as-of", at(0),
                           "--stale-after-days", "30", "--probe", "alpha",
                           "--json")
        single = json.loads(out)
        expect(code == 0 and len(single) == 1 and single[0]["key"] == "alpha"
               and single[0]["stale"] is False,
               "--probe narrows to one row, and the horizon is honoured")

        code, out, _ = cli("--summary", "--as-of", at(0))
        expect(code == 0 and "alpha" in out and "unmeasured" in out
               and "%" in out,
               "the default rendering is a human table")
        expect(COMMIT_A in out,
               "the table reports the EXACT commit, not an abbreviation")
        expect(all(COMMIT_A[:8] not in line or COMMIT_A in line
                   for line in out.splitlines()),
               "and no row abbreviates it")

        # The evaluation time and horizon are validated like every other
        # input, and the three new flags belong to --summary alone.
        code, _, err = cli("--summary", "--as-of", "yesterday")
        expect(code == 1 and "--as-of" in err,
               "an unreadable --as-of refuses instead of falling back to now")
        code, _, err = cli("--summary", "--stale-after-days", "soon")
        expect(code == 1 and "--stale-after-days" in err,
               "a non-numeric --stale-after-days refuses")
        code, _, err = cli("--summary", "--stale-after-days", "none")
        expect(code == 1 and "none" in err,
               "there is no `none` horizon")
        for argv, flag in ((("--print", "--json"), "--json"),
                           (("--print", "--as-of", at(0)), "--as-of"),
                           (("--validate", "--stale-after-days", "3"),
                            "--stale-after-days")):
            code, _, err = cli(*argv)
            expect(code == 1 and flag in err,
                   f"{flag} outside --summary is reported, not ignored")
        code, _, err = cli("--summary", "--probe", "nonesuch")
        expect(code == 1 and "nonesuch" in err,
               "--summary --probe on an unknown key refuses")


# ==========================================================================
# CI-promotion candidates (#1441)
# ==========================================================================
# The report is REPORTING, so every case here asserts two things at once:
# what the report says, and that it changed nothing. A tool whose ready
# list is read as "nothing measurable stands in the way of promoting
# this" has to be wrong in the safe direction, so most of these cases
# are a qualified probe made unqualified one field at a time.
GPU = ci_probes.Reason(ci_probes.NEEDS_GPU, "no GPU on the runner")
FLAKY = ci_probes.Reason(ci_probes.FLAKY, "AI timing flakes run-to-run")
SLOW = ci_probes.Reason(ci_probes.SLOW_WORLDGEN, "generates a w128 world")
VAGUE = ci_probes.Reason(ci_probes.UNCLASSIFIED, "nobody has said why")

# Every synthetic probe speaks the structured protocol unless a case
# says otherwise; a legacy probe emits no result to have measured.
MIGRATED = {key: probe_protocol.PROTOCOL_VERSION
            for key, _script, _purpose in SYNTHETIC}

# One clean pair of runs, so a fixture that reports zero failures does
# not carry a failing run in the same document.
CLEAN_RUNS = [
    {"index": 1, "port": 9100, "outcome": "PASS", "elapsed_seconds": 11.0,
     "checks": {"first": "PASS", "second": "PASS"}, "artifact_dir": None},
    {"index": 2, "port": 9101, "outcome": "PASS", "elapsed_seconds": 12.5,
     "checks": {"first": "PASS", "second": "PASS"}, "artifact_dir": None},
]
CLEAN_COUNTS = {"first": {"PASS": 2, "FAIL": 0, "MISSING": 0},
                "second": {"PASS": 2, "FAIL": 0, "MISSING": 0}}


def clean_result(commit=COMMIT_A, *, probe="alpha",
                 runs=probe_census.POLICY_RUN_COUNT, completed=None,
                 failures=0, timeouts=0, worst=12.5, age_days=1.0):
    """One accepted measurement with nothing wrong with it by default."""
    return result_document(
        probe=probe, commit=commit, timestamp_utc=at(age_days),
        requested_runs=runs,
        completed_runs=runs if completed is None else completed,
        runs=copy.deepcopy(CLEAN_RUNS),
        check_counts=copy.deepcopy(CLEAN_COUNTS),
        failure_count=failures,
        failure_rate=None if runs == 0 else failures / runs,
        timeout_count=timeouts,
        worst_elapsed_seconds=worst,
        total_elapsed_seconds=worst + 11.0,
        retained_artifacts=[])


def promotion_of(path: Path, *, now=NOW, stale_after_seconds=14 * DAY,
                 mutate=None) -> dict:
    """The report for the census on disk, optionally hand-edited first."""
    document = json.loads(path.read_text(encoding="utf-8"))
    if mutate is not None:
        mutate(document)
    return probe_census.promotion_report(
        document, now=now, stale_after_seconds=stale_after_seconds)


def buckets(report: dict) -> tuple[list, list]:
    return ([row["key"] for row in report["candidates"]],
            [row["key"] for row in report["blocked"]])


def qualified_census(path: Path, *, probe="alpha", **kwargs) -> None:
    """Record one complete, clean ten-run cohort for `probe`."""
    probe_census.record_result(path, clean_result(probe=probe, **kwargs))


def test_promotion_candidate() -> None:
    print("\n-- a reliability-qualified promotion candidate --")
    with registry(protocol=MIGRATED, reasons={"alpha": (FLAKY,)}), \
            scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        # Two samples on ONE commit, of unequal size: the cohort's
        # statistic is pooled, not the newest sample and not an average
        # of the two rates.
        qualified_census(path, runs=6, worst=9.5, age_days=3)
        qualified_census(path, runs=4, worst=21.25, age_days=2)
        before = path.read_bytes()

        report = promotion_of(path)
        ready, blocked = buckets(report)
        expect(ready == ["alpha"] and blocked == [],
               "a clean, fresh, complete, X=0 flaky probe is a candidate")
        unchanged(path, before, "and the report writes no census bytes")

        row = report["candidates"][0]
        expect(row["requested_runs"] == 10 and row["completed_runs"] == 10
               and row["sample_count"] == 2,
               "the cohort's runs are POOLED across every sample in it")
        expect(row["failure_count"] == 0 and row["timeout_count"] == 0
               and row["failure_rate"] == 0.0,
               "with pooled failures and timeouts, and a pooled rate")
        expect(row["commit_sha"] == COMMIT_A and row["measured_at"] == at(2)
               and row["opened_at"] == at(3)
               and row["age_seconds"] == 2 * DAY,
               "and reports the exact commit, both ends of the cohort's "
               "measurement window, and the age it was judged fresh on")
        expect(report["stale_after_seconds"] == 14 * DAY,
               "the horizon it was judged against is reported, since a "
               "candidacy is only meaningful against one")
        expect(row["acceptable_failures"] == 0
               and row["protocol"] == probe_protocol.PROTOCOL_VERSION
               and row["classification"] == probe_census.MANUAL_ONLY,
               "X, the protocol status and the live classification are "
               "reported beside the numbers")

        # Duration is TWO fields answering two questions, and the
        # missing one is never filled in from the other.
        expect(row["observed_worst_elapsed_seconds"] == 21.25,
               "the observed duration is the MAXIMUM worst_elapsed_seconds "
               "across the cohort, not the newest sample's")
        expect(row["estimated_worst_case_seconds"] is None,
               "an unset estimate stays unset rather than borrowing the "
               "observed duration")
        probe_census.record_policy(path, "alpha", estimate=480)
        stored = promotion_of(path)["candidates"][0]
        expect(stored["estimated_worst_case_seconds"] == 480
               and stored["observed_worst_elapsed_seconds"] == 21.25,
               "a stored estimate is reported SEPARATELY from the observed "
               "duration, never merged with it")

        # Every reason is reported, in declared order.
        expect(row["reasons"] == [{"category": "flaky",
                                   "explanation": FLAKY.explanation}],
               "the candidate carries its declared reason record verbatim")
        expect(row["blocking_categories"] == [],
               "and nothing about it blocks a promotion")

        # Cardinality is DERIVED. Nothing here, and nothing in the
        # report, knows how many probes the registry holds.
        expect(report["registered_probes"] == len(SYNTHETIC)
               and report["manual_only"] == len(SYNTHETIC)
               and report["ci_eligible"] == 0,
               "every count is derived from the live registry, never a "
               "frozen probe total")
        expect(report["reliability_qualified"] == 1,
               "the other synthetic probes are unmeasured, so they qualify "
               "for neither list")


def test_promotion_disqualifications() -> None:
    print("\n-- what disqualifies a probe from being reported at all --")

    def qualifies(mutate=None, *, stale_after_seconds=14 * DAY,
                  reasons=None, protocol=None, extra=None):
        with registry(protocol=MIGRATED if protocol is None else protocol,
                      reasons={"alpha": (FLAKY,)} if reasons is None
                      else reasons), scratch() as root:
            path = root / "probe_census.json"
            seeded(path)
            qualified_census(path, age_days=1)
            if extra is not None:
                extra(path)
            report = promotion_of(path, mutate=mutate,
                                  stale_after_seconds=stale_after_seconds)
            ready, blocked = buckets(report)
            return "alpha" in ready or "alpha" in blocked

    expect(qualifies() is True,
           "the shared fixture qualifies before anything is changed")

    def set_x(value):
        def mutate(document):
            document["probes"][0]["census"]["acceptable_failures"] = value
        return mutate

    expect(qualifies(set_x(1)) is False,
           "X above zero is never a promotion candidate")
    expect(qualifies(set_x(9)) is False,
           "however large")
    expect(qualifies(set_x(None)) is False,
           "and an UNSET X is ineligible exactly as X>0 is: the rule needs "
           "X to equal zero, not merely to be non-positive")

    def shorten(document):
        cohort = document["probes"][0]["census"]["current"]
        cohort["samples"][0]["requested_runs"] = 9
        cohort["samples"][0]["completed_runs"] = 9

    expect(qualifies(shorten) is False,
           "a cohort short of the policy's ten runs is incomplete")

    def leave_one_unrun(document):
        document["probes"][0]["census"]["current"]["samples"][0][
            "completed_runs"] = 9

    expect(qualifies(leave_one_unrun) is False,
           "a scheduled run that never completed makes the cohort "
           "incomplete, however clean the runs that did")

    # The COMPENSATED case: completion is checked per SAMPLE, so one
    # sample's shortfall is not cancelled by another's overrun. Pooled
    # totals read as a flawless 20 of 20 here.
    def compensate(document):
        cohort = document["probes"][0]["census"]["current"]
        cohort["samples"][0]["completed_runs"] = 9
        cohort["samples"][1]["completed_runs"] = 11

    def two_samples(path):
        qualified_census(path, age_days=0.5)

    expect(qualifies(compensate, extra=two_samples) is False,
           "a 9-of-10 beside an 11-of-10 does NOT qualify: pooling them "
           "to 20 of 20 would hide a measurement that lost a run")
    expect(qualifies(extra=two_samples) is True,
           "while the same two-sample cohort with both complete does")

    def overrun(document):
        document["probes"][0]["census"]["current"]["samples"][0][
            "completed_runs"] = 11

    expect(qualifies(overrun) is False,
           "and an overrun alone disqualifies too: more completions than "
           "were requested is a count nothing could have produced")

    expect(qualifies(extra=lambda path: probe_census.record_result(
        path, result_document(status="harness-error", commit=COMMIT_A,
                              timestamp_utc=at(0.5)))) is False,
           "a harness error at the cohort's own commit makes it incomplete: "
           "a scheduled measurement reported nothing, and the cohort's "
           "counts cannot show that")
    expect(qualifies(extra=lambda path: probe_census.record_result(
        path, result_document(status="harness-error", commit=COMMIT_A,
                              timestamp_utc=at(0.5),
                              commit_sha=probe_census.PLACEHOLDER_COMMIT))
    ) is False,
           "and so does one whose provenance git could not report: "
           "attribution fails CLOSED, because `we cannot tell which cohort "
           "lost a run` is not evidence that this one did not")
    expect(qualifies(extra=lambda path: probe_census.record_result(
        path, result_document(status="harness-error", commit=COMMIT_B,
                              timestamp_utc=at(0.5)))) is True,
           "a harness error at ANOTHER commit is not charged to this "
           "cohort")

    def fail_one(document):
        document["probes"][0]["census"]["current"]["samples"][0][
            "failure_count"] = 1

    def time_one_out(document):
        document["probes"][0]["census"]["current"]["samples"][0][
            "timeout_count"] = 1

    expect(qualifies(fail_one) is False, "a single failure disqualifies")
    expect(qualifies(time_one_out) is False,
           "and so does a single TIMEOUT, which the producer counts "
           "separately from failures")

    expect(qualifies(stale_after_seconds=1 * DAY) is False,
           "a stale current cohort is not evidence about the code as it "
           "stands")
    expect(qualifies(protocol={}) is False,
           "a legacy probe emits no structured result, so there is nothing "
           "to have measured")

    def archive(document):
        census = document["probes"][0]["census"]
        census["history"] = census["history"] + [census["current"]]
        census["current"] = None

    expect(qualifies(archive) is False,
           "an archived cohort is a promoted probe's retained statistic, "
           "not a current measurement")

    with registry(protocol=MIGRATED, reasons={"alpha": (FLAKY,)}), \
            scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        report = promotion_of(path)
        expect(buckets(report) == ([], [])
               and report["reliability_qualified"] == 0,
               "an unmeasured probe appears in NEITHER list")


def test_promotion_reason_buckets() -> None:
    print("\n-- which bucket a reliability-qualified probe lands in --")

    def report_for(reasons):
        with registry(protocol=MIGRATED, reasons=reasons), scratch() as root:
            path = root / "probe_census.json"
            seeded(path)
            qualified_census(path, age_days=1)
            return promotion_of(path)

    ready, blocked = buckets(report_for({"alpha": (FLAKY,)}))
    expect(ready == ["alpha"] and blocked == [],
           "`flaky` is a ground a clean cohort directly answers")
    ready, blocked = buckets(report_for({"alpha": (VAGUE,)}))
    expect(ready == ["alpha"] and blocked == [],
           "and so is `unclassified`: no stated ground survives the "
           "measurement either")

    for reason in (GPU, SLOW,
                   ci_probes.Reason(ci_probes.SCENARIO_HEAVY, "long scenario"),
                   ci_probes.Reason(ci_probes.TARGETED, "one narrow question"),
                   ci_probes.Reason(ci_probes.BASE_FAILING, "red on master")):
        ready, blocked = buckets(report_for({"alpha": (reason,)}))
        expect(ready == [] and blocked == ["alpha"],
               f"a clean probe held out on `{reason.category}` is reported "
               f"as mechanically blocked, not as ready")

    report = report_for({"alpha": (FLAKY, GPU, SLOW)})
    ready, blocked = buckets(report)
    row = report["blocked"][0]
    expect(ready == [] and blocked == ["alpha"],
           "ONE blocking category controls the bucket even when another "
           "declared category is `flaky`")
    expect([entry["category"] for entry in row["reasons"]]
           == ["flaky", "needs-gpu", "slow/worldgen-heavy"],
           "and EVERY declared category is retained, in declared order — "
           "first-reason-only handling would lose the rest")
    expect(row["blocking_categories"] == ["needs-gpu", "slow/worldgen-heavy"],
           "the blocking categories are named as a sorted set beside them")
    expect(all(entry["explanation"] for entry in row["reasons"]),
           "each carries its own explanation, so the report says WHY")

    # Fail closed. A category this file has never heard of is not in the
    # answerable allowlist, so it blocks rather than reading as ready.
    future = ci_probes.Reason.__new__(ci_probes.Reason)
    object.__setattr__(future, "category", "needs-network")
    object.__setattr__(future, "explanation", "a category from the future")
    ready, blocked = buckets(report_for({"alpha": (future,)}))
    expect(ready == [] and blocked == ["alpha"],
           "an unknown reason category fails CLOSED into the blocked list "
           "rather than appearing ready")
    ready, blocked = buckets(report_for({"alpha": (FLAKY, future)}))
    expect(ready == [] and blocked == ["alpha"],
           "and it still blocks beside an answerable one")

    ready, blocked = buckets(report_for({}))
    expect(ready == [] and blocked == ["alpha"],
           "a probe with no declared reason at all is blocked, not ready: "
           "`every category is answerable` is vacuously true of none")


def test_promotion_preserves_the_manifest() -> None:
    print("\n-- promoting a candidate keeps its row and its history --")
    reasons = {"alpha": (FLAKY,)}
    with registry(protocol=MIGRATED, reasons=reasons), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        qualified_census(path, age_days=1)
        expect(buckets(promotion_of(path))[0] == ["alpha"],
               "alpha is a candidate while it is manual-only")

        # The promotion itself: a person edits tools/ci_probes.py. The
        # report never does, so the case performs the edit by hand.
        ci_probes.CI_ELIGIBLE = {"alpha"}
        ci_probes.MANUAL_ONLY_REASONS = {}
        probe_census.ensure_document(path)
        document = json.loads(path.read_text(encoding="utf-8"))
        row = next(entry for entry in document["probes"]
                   if entry["key"] == "alpha")
        census = row["census"]

        expect(len(document["probes"]) == len(SYNTHETIC)
               and row["classification"] == probe_census.CI_ELIGIBLE,
               "the promoted probe keeps its row in the global manifest, "
               "reclassified")
        expect(census["current"] is None
               and [cohort["commit_sha"] for cohort in census["history"]]
               == [COMMIT_A]
               and len(census["history"][0]["samples"]) == 1,
               "its current cohort is ARCHIVED, not deleted: the retained "
               "history keeps every sample")
        expect(len(census["attempts"]) == 1,
               "and the attempt log is retained whole")

        ready, blocked = buckets(promotion_of(path))
        expect(ready == [] and blocked == [],
               "and it leaves the manual-only report, in both directions")
        report = promotion_of(path)
        expect(report["ci_eligible"] == 1
               and report["manual_only"] == len(SYNTHETIC) - 1,
               "the derived counts follow the promotion")

        # A promoted probe receives no further samples, and refusing is
        # not a partial write.
        before = path.read_bytes()
        expect_refusal(
            lambda: probe_census.record_result(path, clean_result()),
            "a promoted probe's census refuses a later measurement",
            "alpha", "CI-eligible", "no further samples")
        unchanged(path, before,
                  "and the refusal mutates not one byte of the record")


def test_promotion_cli() -> None:
    print("\n-- the --promotion-candidates CLI --")
    with registry(protocol=MIGRATED,
                  reasons={"alpha": (FLAKY,), "beta": (GPU,)}), \
            cli_repo() as (_main_wt, census_path):
        cli("--seed")
        probe_census.record_result(census_path, clean_result(age_days=1))
        probe_census.record_result(
            census_path, clean_result(probe="beta", age_days=1, worst=300.0))
        before = census_path.read_bytes()

        code, out, err = cli("--promotion-candidates", "--as-of", at(0),
                             "--json")
        expect(code == 0 and err == "",
               f"--promotion-candidates --json exits 0 ({err!r})")
        report = json.loads(out)
        expect(report["schema"] == probe_census.PROMOTION_SCHEMA,
               "the JSON form declares its own schema")
        expect(buckets(report) == (["alpha"], ["beta"]),
               "and separates the two lists")
        unchanged(census_path, before, "reporting writes no census bytes")

        code, out, _ = cli("--promotion-candidates", "--as-of", at(0))
        expect(code == 0 and "alpha" in out and "beta" in out,
               "the default rendering is a human report")
        expect(COMMIT_A in out,
               "which reports the EXACT commit, not an abbreviation")
        expect("needs-gpu" in out and "flaky" in out,
               "and names every reason category it read")
        expect("tools/ci_probes.py" in out,
               "and says where an actual promotion is made")

        # The horizon is an input here exactly as it is for --summary.
        code, out, _ = cli("--promotion-candidates", "--as-of", at(0),
                           "--stale-after-days", "0.5", "--json")
        expect(code == 0 and buckets(json.loads(out)) == ([], []),
               "a horizon that makes the cohort stale empties both lists")

        code, _, err = cli("--promotion-candidates", "--probe", "alpha")
        expect(code == 1 and "--probe" in err,
               "--probe is refused: which probes qualify is the question "
               "this mode answers")
        code, _, err = cli("--promotion-candidates", "--summary")
        expect(code != 0,
               "and the reading modes are mutually exclusive")

# ==========================================================================
# ==========================================================================
def outcome_record(mark: str, *, probe: str = "alpha",
                   outcome: str = "cannot-reproduce") -> dict:
    """A schema-valid durable de-flake outcome, tagged by its attempt.

    Spelled here rather than imported from `tools/deflake_outcome.py`:
    this file tests the census's own storage and preservation rules, and
    a fixture built by the one producer would stop being able to fail
    when the two disagree.
    """
    return {
        "attempt": mark,
        "outcome": outcome,
        "reason": "baseline-observed-nothing",
        "probe": probe,
        "timestamp_utc": "2026-08-28T12:00:00Z",
        "baseline_sha": COMMIT_A,
        "acceptable_failures": 0,
        "targets": ["first"],
        "configuration": [{"path": "config/save.local.yaml",
                           "sha256": "a" * 64}],
        "invocation": {"command": ["python3", "tools/deflake.py"],
                       "directory": "/tmp/checkout"},
        "measurements": [{
            "role": "baseline",
            "exit_code": 0,
            "status": "ok",
            "commit_sha": COMMIT_A,
            "timestamp_utc": "2026-08-28T11:00:00Z",
            "requested_runs": 10,
            "completed_runs": 10,
            "runs": [{"index": index, "outcome": "PASS"}
                     for index in range(1, 11)],
            "check_counts": {"first": {"PASS": 10, "FAIL": 0, "MISSING": 0}},
            "failure_count": 0,
            "failure_rate": 0.0,
            "timeout_count": 0,
            "rts_capabilities": 4,
            "error": None,
            "error_run_index": None,
            "retained_artifacts": [],
            "census_reference": {"cohort_commit_sha": COMMIT_A,
                                 "sample_timestamp_utc":
                                     "2026-08-28T11:00:00Z"},
        }],
        "retained_artifacts": [f"/tmp/artifacts/{mark}"],
        "summary": f"the {mark} attempt reproduced nothing",
        "recommendation": {"action": "de-list", "advisory": True,
                           "detail": "consider de-listing; advisory only"},
        "comparison": None,
    }


def test_outcome_log() -> None:
    """#1439's append-only outcome log: its own aspect, and idempotent.

    The census gained another mutating aspect, so the two questions the
    preservation guard exists to answer are asked of it in both
    directions: an outcome append may touch nothing else, and no other
    operation may touch `outcomes`.
    """
    print("\n-- the de-flake outcome log --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        probe_census.record_result(path, result_document())
        probe_census.record_claim(path, "alpha", {
            "token": "claim-1", "timestamp_utc": "2026-08-27T09:00:00Z",
            "commit_sha": COMMIT_A, "owner": "deflake", "host": "here",
            "pid": 4711, "lease_seconds": 3600.0, "requested_runs": 10})
        before = json.loads(path.read_text(encoding="utf-8"))

        probe_census.record_outcome(path, "alpha", outcome_record("a-1"))
        after = json.loads(path.read_text(encoding="utf-8"))
        row = probe_census.find_entry(after, "alpha")["census"]
        expect([entry["attempt"] for entry in row["outcomes"]] == ["a-1"],
               "an outcome append lands in the row's own outcome log")
        was = probe_census.find_entry(before, "alpha")["census"]
        expect(all(was[field] == row[field] for field in
                   ("current", "history", "attempts", "claims",
                    "acceptable_failures",
                    "acceptable_failures_justification",
                    "estimated_worst_case_seconds")),
               "...and touches no cohort, sample, attempt, claim or policy "
               "field")
        expect(all(a == b for a, b in zip(before["probes"][1:],
                                          after["probes"][1:])),
               "...nor any unrelated row")

        stored = path.read_bytes()
        probe_census.record_outcome(path, "alpha", outcome_record("a-1"))
        unchanged(path, stored,
                  "resuming the same attempt installs the identical bytes")
        expect_refusal(
            lambda: probe_census.record_outcome(
                path, "alpha", {**outcome_record("a-1"),
                                "summary": "a different account"}),
            "one attempt identity carrying different evidence is refused",
            "already recorded with different evidence")
        unchanged(path, stored, "...and changes no bytes")

        probe_census.record_outcome(path, "alpha", outcome_record("a-2"))
        expect([entry["attempt"] for entry in probe_census.find_entry(
            json.loads(path.read_text(encoding="utf-8")),
            "alpha")["census"]["outcomes"]] == ["a-1", "a-2"],
            "a second attempt appends after the first, in order")

        # The record names its own probe, because it is handed BETWEEN
        # workflows; the two must agree rather than one being trusted.
        expect_refusal(
            lambda: probe_census.record_outcome(
                path, "beta", outcome_record("b-1")),
            "an outcome naming another probe is refused",
            "so it is not this row's outcome")
        expect_refusal(
            lambda: probe_census.record_outcome(path, "alpha", ["not", "it"]),
            "a non-object outcome record is refused",
            "must be a JSON object")
        expect_refusal(
            lambda: probe_census.record_outcome(
                path, "alpha", {**outcome_record("a-3"), "attempt": ""}),
            "an outcome with no attempt identity is refused",
            "`attempt` identity")

        # The aspect boundary, from both sides.
        current = path.read_bytes()

        def outcome_touching_measurements(document):
            candidate = copy.deepcopy(document)
            row = [r for r in candidate["probes"] if r["key"] == "alpha"][0]
            row["census"]["outcomes"].append(outcome_record("a-9"))
            row["census"]["attempts"].append(attempt_record("forged"))
            return candidate, {"alpha": {"outcomes"}}
        expect_refusal(
            lambda: probe_census.update(path, outcome_touching_measurements),
            "an outcome append that also logs an attempt is refused",
            "which a diagnosis outcome may not touch")
        unchanged(path, current, "...and changes no bytes")

        def measurement_touching_outcomes(document):
            candidate = copy.deepcopy(document)
            row = [r for r in candidate["probes"] if r["key"] == "alpha"][0]
            row["census"]["outcomes"].append(outcome_record("a-9"))
            return candidate, {"alpha": {"measurements"}}
        expect_refusal(
            lambda: probe_census.update(path, measurement_touching_outcomes),
            "a measurement ingestion appending an outcome is refused",
            "which a measurement ingestion may not touch")
        unchanged(path, current, "...and changes no bytes")

        def policy_touching_outcomes(document):
            candidate = copy.deepcopy(document)
            row = [r for r in candidate["probes"] if r["key"] == "alpha"][0]
            row["census"]["outcomes"] = []
            return candidate, {"alpha": {"policy"}}
        expect_refusal(
            lambda: probe_census.update(path, policy_touching_outcomes),
            "a policy update clearing the outcome log is refused",
            "which a policy update may not touch")
        unchanged(path, current, "...and changes no bytes")

        # Append-only, like every other durable log on the record.
        for mutate, why in (
            (lambda outcomes: outcomes.clear(), "dropping the log"),
            (lambda outcomes: outcomes.insert(0, outcome_record("a-0")),
             "prepending to the log"),
            (lambda outcomes: outcomes.__setitem__(0, outcome_record("a-x")),
             "rewriting an existing entry"),
        ):
            def rewrite(document, apply=mutate):
                candidate = copy.deepcopy(document)
                row = [r for r in candidate["probes"]
                       if r["key"] == "alpha"][0]
                apply(row["census"]["outcomes"])
                return candidate, {"alpha": {"outcomes"}}
            expect_refusal(lambda: probe_census.update(path, rewrite),
                           f"{why} is refused", "append-only")
            unchanged(path, current, f"...and changes no bytes ({why})")

    # The v3 -> v5 migration adds the empty outcome log and null deferral.
    with registry(), scratch() as root:
        path = root / "legacy.json"
        stored = stored_v3_document()
        path.write_text(json.dumps(stored), encoding="utf-8")
        migrated = probe_census.ensure_document(path)
        expect(migrated["schema"] == probe_census.CENSUS_SCHEMA,
               "seeding a v3 census migrates it to the current schema")
        record = probe_census.find_entry(migrated, "alpha")["census"]
        expect(record == {**stored["probes"][0]["census"], "outcomes": [],
                          "deferred": None},
               "...adding only the empty outcome log and null deferral")

    # The immediately previous schema needs only the new field.
    with registry(), scratch() as root:
        path = root / "legacy-v4.json"
        stored = stored_v4_document()
        path.write_text(json.dumps(stored), encoding="utf-8")
        migrated = probe_census.ensure_document(path)
        record = probe_census.find_entry(migrated, "alpha")["census"]
        expect(record == {**stored["probes"][0]["census"],
                          "deferred": None},
               "a v4 census gains only the null deferral on migration")


def test_deferral_gate() -> None:
    """A deferral is durable availability state, never discarded evidence."""
    print("\n-- deferred probes retain evidence and leave selection --")
    reason = "planned biome tree assets are not implemented yet"
    resume_when = "the remaining biome tree assets and definitions merge"

    with registry(), scratch() as root:
        path = root / "probe_census.json"
        path.write_text(json.dumps(rich_census()), encoding="utf-8")
        before = json.loads(path.read_text(encoding="utf-8"))
        before_alpha = copy.deepcopy(_alpha(before))

        probe_census.record_deferral(
            path, "alpha", reason=reason, resume_when=resume_when)
        deferred_document = json.loads(path.read_text(encoding="utf-8"))
        deferred = _alpha(deferred_document)
        expect(deferred["deferred"] == {
            "reason": reason, "resume_when": resume_when},
            "a deferral stores both its reason and actionable resume condition")
        expect({key: value for key, value in deferred.items()
                if key != "deferred"}
               == {key: value for key, value in before_alpha.items()
                   if key != "deferred"},
               "deferring retains every measurement, attempt, claim, outcome "
               "and policy field")
        expect(deferred_document["probes"][1:] == before["probes"][1:],
               "deferring one probe changes no unrelated row")

        summaries = probe_census.census_summary(
            deferred_document,
            now=datetime.datetime(2026, 8, 30, tzinfo=datetime.timezone.utc),
            stale_after_seconds=probe_census.DEFAULT_STALE_AFTER_SECONDS)
        alpha = summaries[0]
        expect(alpha["deferred"] == deferred["deferred"],
               "the selection-facing summary exposes the complete deferral")
        expect("deferred" in probe_census.render_summary([alpha]),
               "the human summary names the probe's deferred state")

        stable = path.read_bytes()
        probe_census.record_deferral(
            path, "alpha", reason=reason, resume_when=resume_when)
        unchanged(path, stable, "repeating the same deferral is a byte no-op")

        def deferral_touching_attempts(document):
            candidate = copy.deepcopy(document)
            _alpha(candidate)["deferred"] = None
            _alpha(candidate)["attempts"].append(attempt_record("forged"))
            return candidate, {"alpha": {"deferral"}}

        expect_refusal(
            lambda: probe_census.update(path, deferral_touching_attempts),
            "a deferral update cannot forge a measurement attempt",
            "which a deferral update may not touch")
        unchanged(path, stable, "the mixed-aspect deferral wrote nothing")

        def measurement_touching_deferral(document):
            candidate = copy.deepcopy(document)
            _alpha(candidate)["deferred"] = None
            return candidate, {"alpha": {"measurements"}}

        expect_refusal(
            lambda: probe_census.update(path, measurement_touching_deferral),
            "measurement ingestion cannot silently resume a deferred probe",
            "which a measurement ingestion may not touch")
        unchanged(path, stable, "the mixed-aspect measurement wrote nothing")

        def reconciliation_touching_deferral(document):
            candidate = copy.deepcopy(document)
            _alpha(candidate)["deferred"] = None
            return candidate, probe_census.TOUCH_ANY

        expect_refusal(
            lambda: probe_census.update(path, reconciliation_touching_deferral),
            "inventory reconciliation cannot silently resume a deferred probe",
            "reconciliation changed deferral field")
        unchanged(path, stable, "the mixed reconciliation wrote nothing")

        for bad, label in (("", "empty"), ("   ", "whitespace-only")):
            expect_refusal(
                lambda value=bad: probe_census.record_deferral(
                    path, "alpha", reason=value, resume_when=resume_when),
                f"an {label} deferral reason is refused", "non-blank")
            expect_refusal(
                lambda value=bad: probe_census.record_deferral(
                    path, "alpha", reason=reason, resume_when=value),
                f"an {label} resume condition is refused", "non-blank")
        expect_refusal(
            lambda: probe_census.record_deferral(
                path, "nosuch", reason=reason, resume_when=resume_when),
            "deferring an unknown probe is refused", "nosuch", "no census row")
        unchanged(path, stable, "no refused deferral changed the census")

        probe_census.record_deferral(path, "alpha", resume=True)
        resumed_document = json.loads(path.read_text(encoding="utf-8"))
        resumed = _alpha(resumed_document)
        expect(resumed["deferred"] is None,
               "resuming clears only the availability gate")
        expect({key: value for key, value in resumed.items()
                if key != "deferred"}
               == {key: value for key, value in deferred.items()
                   if key != "deferred"},
               "resuming retains all accumulated evidence and policy")
        stable = path.read_bytes()
        probe_census.record_deferral(path, "alpha", resume=True)
        unchanged(path, stable, "resuming an active probe is a byte no-op")

    with registry(), cli_repo() as (_, path):
        cli("--seed")
        code, out, err = cli(
            "--defer", "--probe", "alpha", "--reason", reason,
            "--resume-when", resume_when)
        expect(code == 0 and "deferred alpha" in out and not err,
               "the CLI stores a deferral and reports the affected probe")
        code, out, err = cli(
            "--summary", "--probe", "alpha", "--json",
            "--as-of", "2026-08-30T00:00:00Z")
        reported = json.loads(out)[0] if code == 0 else {}
        expect(code == 0 and not err and reported.get("deferred") == {
            "reason": reason, "resume_when": resume_when},
            "the CLI JSON summary exposes the selector's deferral input")
        code, out, err = cli("--resume", "--probe", "alpha")
        expect(code == 0 and "resumed alpha" in out and not err,
               "the CLI resumes the probe explicitly")
        code, _, err = cli("--defer", "--probe", "alpha",
                           "--reason", reason)
        expect(code != 0 and "--resume-when" in err,
               "--defer refuses to guess a missing resume condition")
        code, _, err = cli("--resume", "--probe", "alpha",
                           "--reason", reason)
        expect(code != 0 and "--reason is only valid with --defer" in err,
               "--resume cannot smuggle in new deferral text")


def main() -> int:
    selftest.parse_verbose()
    for test in (test_record_shape, test_migration, test_seed_and_noop,
                 test_reconciliation, test_ingest_accepted,
                 test_ci_eligible_takes_no_measurement,
                 test_ingest_harness_error, test_outcome_log,
                 test_deferral_gate,
                 test_policy,
                 test_acceptable_failure_policy_defaults,
                 test_acceptable_failure_policy_rules,
                 test_acceptable_failure_policy_promotion,
                 test_acceptable_failure_threshold,
                 test_acceptable_failure_policy_cli, test_refusals,
                 test_malformed_rows_refuse_cleanly,
                 test_duplicate_target_rows,
                 test_adversarial_malformed_input,
                 test_cross_field_invariants,
                 test_declared_schema, test_malformed_schema_file,
                 test_missing_dependency,
                 test_path_substitution, test_atomicity,
                 test_preservation_guard,
                 test_independent_process_contention,
                 test_unusable_docs_worktree, test_cli,
                 test_cli_justification,
                 test_cohort_accumulation, test_cohort_append_order,
                 test_head_movement_is_not_a_census_event,
                 test_staleness_boundary, test_unmeasured_and_zero_rate,
                 test_history_only_statistic,
                 test_cohort_semantic_refusals,
                 test_summary_preserves_everything, test_summary_cli,
                 test_promotion_candidate, test_promotion_disqualifications,
                 test_promotion_reason_buckets,
                 test_promotion_preserves_the_manifest,
                 test_promotion_cli):
        test()
    print()
    if FAILURES:
        print(f"{len(FAILURES)} FAILED:")
        for message in FAILURES:
            print(f"  - {message}")
        return selftest.concluded(1)
    return selftest.concluded(0, "probe_census self-test: all cases pass")


if __name__ == "__main__":
    sys.exit(main())
