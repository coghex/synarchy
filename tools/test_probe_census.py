#!/usr/bin/env python3
"""Focused self-test for the probe census record and its writer (#1428).

Deterministic, engine-free, GPU-free and offline: every case runs
against synthetic documents in a throwaway temporary tree. Nothing here
boots an engine, runs a registered probe, touches the developer's real
`docs-wip` worktree, or shells out to anything but `git` (to build a
two-worktree scratch repository the CLI cases can resolve) and this same
interpreter (for the independent-process contention case).

The real `tools/probe_census.py` is imported and driven — with
`run_probes.PROBES`, `run_probes.REPO_ROOT`, `ci_probes.CI_ELIGIBLE` and
`probe_flake.PROTOCOL_PROBES` pointed at a synthetic registry — so this
exercises the shipped code paths rather than a copy.

What is deliberately NOT covered: comprehensive schema, required-field,
enum, range and finite-number validation (#1492) and the cross-field
census/measurement invariants (#1493). This slice owns the record, the
operations, and the atomic write path, and its safety promise is a
CONTROLLED refusal — no traceback, authoritative bytes unchanged — when
an operation cannot be performed, not the discovery of every possible
corruption in a hand-edited document.

Usage:
  python3 tools/test_probe_census.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import copy
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
import run_probes  # type: ignore  # noqa: E402

FAILURES: list[str] = []

COMMIT_A = "a" * 40
COMMIT_B = "b" * 40


def expect(cond: bool, msg: str) -> None:
    if not cond:
        FAILURES.append(msg)
    print(f"  {'ok  ' if cond else 'FAIL'} {msg}")


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
def registry(probes=None, ci_eligible=(), protocol=None):
    """The live registries, pointed at a synthetic set for one case."""
    saved = (run_probes.PROBES, ci_probes.CI_ELIGIBLE,
             probe_flake.PROTOCOL_PROBES)
    run_probes.PROBES = list(SYNTHETIC if probes is None else probes)
    ci_probes.CI_ELIGIBLE = set(ci_eligible)
    probe_flake.PROTOCOL_PROBES = dict(protocol or {})
    try:
        yield
    finally:
        (run_probes.PROBES, ci_probes.CI_ELIGIBLE,
         probe_flake.PROTOCOL_PROBES) = saved


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


def result_document(probe="alpha", status="ok", commit=COMMIT_A, **overrides):
    """A realistic `probe-flake-result/v1` document.

    It carries every producer-only field `Measurement.to_document` adds,
    so the exclusion case has something real to prove.
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
    document.update(overrides)
    return document


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
        "acceptable_failures": None,
        "acceptable_failures_justification": None,
        "estimated_worst_case_seconds": None,
        "current": None,
        "history": [],
        "attempts": [],
    }, "an empty census record is exactly the six specified fields")
    expect(probe_census.empty_census() is not empty
           and probe_census.empty_census()["history"] is not empty["history"],
           "each empty record is a fresh object, never a shared default")

    with registry(ci_eligible={"beta"}, protocol={"beta": "probe-result/v1"}):
        document = probe_census.build_manifest()
        expect(document["schema"] == "probe-census/v2",
               "a freshly built census is probe-census/v2")
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
    print("\n-- lossless v1 -> v2 migration --")
    with registry(ci_eligible={"beta"}, protocol={"beta": "probe-result/v1"}):
        source = v1_document()
        # An inventory field this tool does not know about must survive.
        source["probes"][0]["note"] = "kept by migration"
        original = copy.deepcopy(source)
        migrated = probe_census.migrate_document(source)

        expect(source == original,
               "migration does not mutate the document it was given")
        expect(migrated["schema"] == "probe-census/v2",
               "the migrated document is probe-census/v2")
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

        # A v2 row with no census is NOT silently repaired: that is
        # corruption for #1492 to report, and inserting an empty record
        # would erase the evidence.
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
        expect(document["schema"] == "probe-census/v2",
               "the fresh census is probe-census/v2")
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
        expect(migrated["schema"] == "probe-census/v2",
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
            "schema": "probe-census/v2",
            "probes": [
                {"key": "retired", "script": "retired_probe.py",
                 "classification": "manual-only", "protocol": "legacy",
                 "census": {**probe_census.empty_census(),
                            "acceptable_failures": 1,
                            "current": {"commit_sha": COMMIT_A,
                                        "samples": [{"tag": "kept"}]}}},
                {"key": "alpha", "script": "stale_name.py",
                 "classification": "manual-only", "protocol": "legacy",
                 "census": {**probe_census.empty_census(),
                            "acceptable_failures": 2,
                            "acceptable_failures_justification": "two races",
                            "estimated_worst_case_seconds": 480,
                            "current": {"commit_sha": COMMIT_A,
                                        "samples": [{"tag": "alpha-1"}]},
                            "history": [{"commit_sha": COMMIT_B,
                                         "samples": [{"tag": "old"}]}],
                            "attempts": [{"tag": "attempt-1"}]}},
                {"key": "beta", "script": "beta_probe.py",
                 "classification": "manual-only", "protocol": "legacy",
                 "census": {**probe_census.empty_census(),
                            "acceptable_failures": 9,
                            "current": {"commit_sha": COMMIT_A,
                                        "samples": [{"tag": "beta-1"}]},
                            "attempts": [{"tag": "beta-attempt"}]}},
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
                                        "samples": [{"tag": "beta-1"}]}],
               "the promoted cohort is ARCHIVED into history, never dropped")
        expect(promoted["attempts"] == [{"tag": "beta-attempt"}]
               and promoted["acceptable_failures"] == 9,
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
                                   acceptable_failures=5)
        expect(census_of()["acceptable_failures_justification"] is None
               and census_of()["acceptable_failures"] == 5,
               "--justification none clears the justification only")

        probe_census.record_policy(path, "alpha", estimate=480)
        record = census_of()
        expect(record["estimated_worst_case_seconds"] == 480
               and record["acceptable_failures"] == 5,
               "--set-estimate stores the estimate and keeps X")

        probe_census.record_policy(path, "alpha",
                                   justification="restored", acceptable_failures=5)
        probe_census.record_policy(path, "alpha", acceptable_failures=None)
        record = census_of()
        expect(record["acceptable_failures"] is None
               and record["acceptable_failures_justification"] is None,
               "clearing X clears its justification too")
        expect(record["estimated_worst_case_seconds"] == 480,
               "clearing X leaves the estimate alone")

        probe_census.record_policy(path, "alpha", estimate=None)
        expect(census_of()["estimated_worst_case_seconds"] is None,
               "--set-estimate none clears the estimate")
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

        # The three discriminating fields of a result document.
        for mutation, fragment, why in (
            ({"schema": "probe-flake-result/v9"}, "probe-flake-result/v9",
             "an unrecognized result schema"),
            ({"probe": None}, "names no probe", "a result naming no probe"),
            ({"status": "weird"}, "weird", "an unrecognized status"),
        ):
            expect_refusal(
                lambda m=mutation: probe_census.record_result(
                    path, result_document(**m)),
                f"{why} is refused", fragment)
        expect_refusal(lambda: probe_census.record_result(path, [1, 2]),
                       "a result that is not an object is refused",
                       "must be a JSON object")
        after = json.loads(path.read_text(encoding="utf-8"))
        expect(all(row["census"]["attempts"] == []
                   for row in after["probes"]),
               "an unrecognized status is NOT logged as a failed attempt")

        # A result naming a probe with no row.
        expect_refusal(lambda: probe_census.record_result(
            path, result_document(probe="ghost")),
            "a result naming no census row is refused", "ghost", "--seed")

        # Structural/type errors that block building the durable record.
        for mutation, fragment, why in (
            ({"runs": "no"}, "`runs` must be a list", "a non-list `runs`"),
            ({"runs": [{"index": 1}]}, "'outcome'", "a run missing a field"),
            ({"check_counts": []}, "`check_counts` must be an object",
             "a non-object `check_counts`"),
            ({"retained_artifacts": "one"}, "must be a list",
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
        expect_refusal(lambda: probe_census._optional_int("2.5", "--x"),
                       "a non-integer X is refused", "integer", "none")
        expect_refusal(lambda: probe_census._optional_number("soon", "--e"),
                       "a non-numeric estimate is refused", "number", "none")
        expect_refusal(lambda: probe_census._optional_number("nan", "--e"),
                       "a non-finite estimate is refused (JSON has no NaN)",
                       "finite")
        expect(probe_census._optional_int("none", "--x") is None
               and probe_census._optional_number("none", "--e") is None,
               "the literal `none` clears a nullable field")
        expect(probe_census._optional_number("480", "--e") == 480
               and isinstance(probe_census._optional_number("480", "--e"), int),
               "an integral estimate stays an integer")
        expect(probe_census._optional_number("12.5", "--e") == 12.5,
               "a fractional estimate is stored as a float")


# ==========================================================================
def test_malformed_rows_refuse_cleanly() -> None:
    """Valid JSON, structurally unusable: refuse, never traceback (#1503)."""
    print("\n-- structurally malformed census state --")

    def census_with(rows):
        return {"schema": "probe-census/v2", "probes": rows}

    def row(key="alpha", census=None):
        return {"key": key, "script": "alpha_probe.py",
                "classification": "manual-only", "protocol": "legacy",
                "census": probe_census.empty_census() if census is None
                else census}

    with registry(), scratch() as root:
        cases = [
            # An UNHASHABLE key: `key in live` and `{e["key"] for e ...}`
            # both raise TypeError on this, so it must be refused first.
            (census_with([row(key=[])]), "has no string `key`",
             "a row whose key is an unhashable list"),
            (census_with([row(key=7)]), "has no string `key`",
             "a row whose key is a number"),
            (census_with([{"script": "x.py"}]), "has no string `key`",
             "a row with no key at all"),
            ({"schema": "probe-census/v2", "probes": {"alpha": {}}},
             "must be a list", "a `probes` mapping instead of a list"),
            (census_with(["alpha"]), "is not an object",
             "a row that is a bare string"),
            (census_with([row(census={**probe_census.empty_census(),
                                      "attempts": 5})]),
             "must be a list to append to", "a non-list attempt log"),
            (census_with([row(census={**probe_census.empty_census(),
                                      "history": "old",
                                      "current": {"commit_sha": COMMIT_B,
                                                  "samples": []}})]),
             "must be a list to append to", "a non-list history"),
            (census_with([row(census={**probe_census.empty_census(),
                                      "current": {"commit_sha": COMMIT_A,
                                                  "samples": 3}})]),
             "must be a list to append to", "a non-list sample list"),
            (census_with([row(census="not a record")]),
             "no census record", "a row whose census is a string"),
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
            "a policy update refuses an unusable row key", "has no string `key`")
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
            expect(len(calls) == 1 and b"probe-census/v2" in calls[0],
                   "the candidate had been fully serialized before the failure")
        finally:
            probe_census._atomic_replace = original
        unchanged(path, before,
                  "a failure before replacement leaves the OLD census intact")
        expect(json.loads(path.read_text(encoding="utf-8"))["schema"]
               == "probe-census/v2",
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
        saved = run_probes.REPO_ROOT
        run_probes.REPO_ROOT = str(main_wt)
        try:
            yield main_wt, docs_wt / probe_census.MANIFEST_RELPATH
        finally:
            run_probes.REPO_ROOT = saved


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
            expect(code == 0 and document["schema"] == "probe-census/v2",
                   "--print emits the v2 census the live registry implies")
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
            was = run_probes.REPO_ROOT
            run_probes.REPO_ROOT = str(bare / "solo")
            try:
                code, _, err = cli("--validate")
                expect(code == 2 and "git worktree add" in err,
                       "a missing docs worktree exits 2 with its repair")
            finally:
                run_probes.REPO_ROOT = was

    with registry(ci_eligible={"beta"}), cli_repo() as (_root, path):
        code, out, _ = cli("--seed")
        expect(code == 0 and path.exists() and "probe-census/v2" in out,
               "--seed creates the census in the docs worktree")
        code, _, _ = cli("--validate")
        expect(code == 0, "--validate accepts the freshly seeded v2 census")

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
def main() -> int:
    for test in (test_record_shape, test_migration, test_seed_and_noop,
                 test_reconciliation, test_ingest_accepted,
                 test_ingest_harness_error, test_policy, test_refusals,
                 test_malformed_rows_refuse_cleanly,
                 test_path_substitution, test_atomicity,
                 test_preservation_guard,
                 test_independent_process_contention, test_cli):
        test()
    print()
    if FAILURES:
        print(f"{len(FAILURES)} FAILED:")
        for message in FAILURES:
            print(f"  - {message}")
        return 1
    print("probe_census self-test: all cases pass")
    return 0


if __name__ == "__main__":
    sys.exit(main())
