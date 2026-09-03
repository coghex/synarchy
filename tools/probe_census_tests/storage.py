#!/usr/bin/env python3
"""The census record, its migrations, and how it reaches disk (#2129).

Twelve groups in two fragments, matching the aggregate's order:

  `TESTS_RECORDS_AND_INGESTION`  7 groups -- what an empty record IS and
                                 how it serializes, the lossless v1 to v5
                                 migration, `--seed` on a fresh and on an
                                 unchanged tree, inventory reconciliation,
                                 accepted and harness-error ingestion, and
                                 the CI-eligible row that takes no
                                 measurement;
  `TESTS_PATHS_AND_PERSISTENCE`  5 groups -- symlink, hard-link and
                                 non-regular path refusal, atomic
                                 replacement under an injected failure,
                                 the preservation guard, contention
                                 between two independent processes, and
                                 the unusable docs worktree.

The two are separated because the aggregate interleaves them: #1428's
five persistence groups run after the whole validation family, which is
the order this gate has always had. Selecting `--family storage` runs
all twelve, in that same relative order.

These drive the real `tools/probe_census.py` writer against synthetic
documents in a throwaway tree. The only subprocesses are `git` (building
the scratch repository the docs-worktree case resolves) and this same
interpreter, which the contention case launches deliberately: nothing
here boots an engine, runs a registered probe, or touches the
developer's real `docs-wip` worktree.
"""

from __future__ import annotations

import copy
import json
import os
import shutil
import subprocess
import sys
from pathlib import Path

from .support import (
    attempt_record, census_contract, census_records, census_storage, cli,
    cli_repo, COMMIT_A, COMMIT_B, expect, expect_refusal, probe_census,
    registry, result_document, sample_record, scratch, seeded, TOOLS_DIR,
    unchanged, v1_document,
)


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


def staging_residue(directory: Path) -> list[str]:
    """Staging files by the writer's own rule — the lock is not one."""
    return sorted(p.name for p in directory.iterdir()
                  if p.name.startswith(census_storage.STAGING_PREFIX)
                  and p.name.endswith(census_storage.STAGING_SUFFIX))


# ==========================================================================
def test_record_shape() -> None:
    print("\n-- the record, and byte-stable serialization --")
    empty = census_records.empty_census()
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
    expect(census_records.empty_census() is not empty
           and census_records.empty_census()["history"] is not empty["history"],
           "each empty record is a fresh object, never a shared default")

    with registry(ci_eligible={"beta"}, protocol={"beta": "probe-result/v1"}):
        document = census_records.build_manifest()
        expect(document["schema"] == "probe-census/v5",
               "a freshly built census is probe-census/v5")
        expect([row["key"] for row in document["probes"]]
               == ["alpha", "beta", "gamma"],
               "rows are built in live registry order")
        expect(all(row["census"] == census_records.empty_census()
                   for row in document["probes"]),
               "every row carries the exact empty census record")
        expect(all(set(row) == {"key", "script", "classification",
                                "protocol", "census"}
                   for row in document["probes"]),
               "a row is exactly the four inventory fields plus its census")

        text = census_records.render_manifest(document)
        expect(text.endswith("}\n") and text.count("\n") > 1,
               "the serialization ends with exactly one trailing newline")
        expect(text == census_records.render_manifest(json.loads(text)),
               "the document round-trips byte-for-byte")
        expect(census_records.render_manifest(json.loads(text))
               == census_records.render_manifest(
                   json.loads(json.dumps(json.loads(text)))),
               "serialization is a pure function of content, not key order")
        shuffled = {"probes": document["probes"], "schema": document["schema"]}
        expect(census_records.render_manifest(shuffled) == text,
               "a document built with its top-level keys in another order "
               "serializes identically")

    expect_refusal(lambda: census_records.render_manifest({"probes": {1, 2}}),
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
        migrated = census_records.migrate_document(source)

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
        expect(all(row["census"] == census_records.empty_census()
                   for row in migrated["probes"]),
               "every migrated row gains the exact empty census record")

        # v2 in, v2 out, untouched.
        with_data = copy.deepcopy(migrated)
        with_data["probes"][0]["census"]["acceptable_failures"] = 3
        expect(census_records.migrate_document(with_data) == with_data,
               "migrating an already-v2 document changes nothing")

        # A v2 row with no census is NOT silently repaired here: it is
        # a declared-schema violation for the validator to report, and
        # inserting an empty record would erase the evidence.
        damaged = copy.deepcopy(migrated)
        del damaged["probes"][1]["census"]
        expect("census" not in census_records.migrate_document(
            damaged)["probes"][1],
            "a v2 row missing its census record is left alone, not repaired")

        expect_refusal(
            lambda: census_records.migrate_document({"schema": "probe-census/v9",
                                                   "probes": []}),
            "an unknown schema is a controlled refusal",
            "probe-census/v9")
        expect_refusal(lambda: census_records.migrate_document([]),
                       "a non-object census is a controlled refusal",
                       "must be a JSON object")
        expect_refusal(
            lambda: census_records.migrate_document({"schema": "probe-census/v1",
                                                   "probes": "no"}),
            "a non-list `probes` is a controlled refusal", "must be a list")
        expect_refusal(
            lambda: census_records.migrate_document({"schema": "probe-census/v1",
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
        again = census_storage.ensure_document(path)
        expect(again == document, "a second seed produces the same document")
        unchanged(path, before, "a drift-free --seed leaves the bytes alone")
        after = path.stat()
        expect((after.st_ino, after.st_mtime_ns)
               == (stamp.st_ino, stamp.st_mtime_ns),
               "a drift-free --seed does not even rewrite the file")

        # Seeding a v1 document migrates in place rather than regenerating.
        legacy = root / "legacy.json"
        legacy.write_text(json.dumps(v1_document()), encoding="utf-8")
        migrated = census_storage.ensure_document(legacy)
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
            "schema": census_contract.CENSUS_SCHEMA,
            "probes": [
                {"key": "retired", "script": "retired_probe.py",
                 "classification": "manual-only", "protocol": "legacy",
                 "census": {**census_records.empty_census(),
                            "acceptable_failures": 1,
                            "acceptable_failures_justification": "one race",
                            "current": {"commit_sha": COMMIT_A,
                                        "samples": [sample_record("kept")]},
                            "attempts": [attempt_record("retired-attempt")]}},
                {"key": "alpha", "script": "stale_name.py",
                 "classification": "manual-only", "protocol": "legacy",
                 "census": {**census_records.empty_census(),
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
                 "census": {**census_records.empty_census(),
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
            result = census_storage.ensure_document(path)

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
        expect(rows["gamma"]["census"] == census_records.empty_census()
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
            back = census_storage.ensure_document(path)
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

        census_storage.record_result(path, result_document())
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
        expect(rows["beta"]["census"] == census_records.empty_census()
               and rows["gamma"]["census"] == census_records.empty_census(),
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
        census_storage.record_result(path, result_document())
        census = json.loads(path.read_text(encoding="utf-8"))["probes"][0]["census"]
        expect(len(census["current"]["samples"]) == 2 and census["history"] == [],
               "a second measurement of the SAME commit appends to the cohort")
        expect(len(census["attempts"]) == 2,
               "--record is deliberately not idempotent: the same document "
               "twice appends twice")

        census_storage.record_result(path, result_document(commit=COMMIT_B))
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
        census_storage.record_result(path, result_document())
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
                lambda: census_storage.record_result(path, result_document()),
                "an accepted measurement for a live CI-eligible probe is "
                "refused",
                "alpha", "CI-eligible")
            unchanged(path, before, "and nothing at all is written")
            expect_refusal(
                lambda: census_storage.record_result(
                    path, result_document(status="harness-error")),
                "so is a harness error for one: nothing about a promoted "
                "probe enters the append-only record",
                "alpha", "CI-eligible")
            unchanged(path, before, "again writing nothing")

        census_storage.record_result(path, result_document())
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
        census_storage.record_result(path, result_document())
        with_sample = json.loads(path.read_text(encoding="utf-8"))

        census_storage.record_result(
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
        expect_refusal(lambda: census_storage.record_result(linked,
                                                          result_document()),
                       "a symlinked census target is refused", "symlink")
        unchanged(good, payload, "the symlink's target is left alone")

        # A hard-linked target: replacing it would silently strand the
        # other name on the old bytes.
        hard = root / "hard.json"
        os.link(good, hard)
        expect_refusal(lambda: census_storage.record_result(hard,
                                                          result_document()),
                       "a hard-linked census target is refused", "one link")
        hard.unlink()

        # A non-regular target.
        directory = root / "adirectory.json"
        directory.mkdir()
        expect_refusal(lambda: census_storage.record_result(directory,
                                                          result_document()),
                       "a census target that is not a regular file is refused",
                       "regular file")

        # A symlinked census DIRECTORY.
        elsewhere = root / "elsewhere"
        elsewhere.mkdir()
        via_link = root / "vialink"
        via_link.symlink_to(elsewhere)
        expect_refusal(lambda: census_storage.record_result(
            via_link / "probe_census.json", result_document()),
            "a symlinked census directory is refused", "symlink")

        # The lock path is the THIRD path the rule covers.
        locked_root = root / "locked"
        locked_root.mkdir()
        target = locked_root / "probe_census.json"
        seeded(target)
        stamp = target.read_bytes()
        guard = census_storage.lock_path(target)
        # `seeded` above already created the lock; replace it with each
        # substitution in turn.
        guard.unlink()
        guard.symlink_to(good)
        expect_refusal(lambda: census_storage.record_result(target,
                                                          result_document()),
                       "a symlinked lock path is refused", "symlink")
        guard.unlink()
        unchanged(good, payload,
                  "the symlinked lock's target was never written through")

        os.link(good, guard)
        expect_refusal(lambda: census_storage.record_result(target,
                                                          result_document()),
                       "a hard-linked lock path is refused", "one link")
        guard.unlink()

        guard.mkdir()
        expect_refusal(lambda: census_storage.record_result(target,
                                                          result_document()),
                       "a lock path that is not a regular file is refused",
                       "regular file")
        guard.rmdir()
        unchanged(target, stamp, "no lock-path refusal changed the census")

        # And with a clean lock path the same call succeeds, so the
        # refusals above are the substitution and nothing else.
        census_storage.record_result(target, result_document())
        expect(target.read_bytes() != stamp,
               "with an unsubstituted lock path the same write succeeds")
        expect(census_storage.lock_path(target).exists(),
               "the lock file is left in place rather than unlinked")


# ==========================================================================
def test_atomicity() -> None:
    print("\n-- atomic replacement and injected failure --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        census_storage.record_result(path, result_document())
        before = path.read_bytes()

        expect(staging_residue(path.parent) == [],
               "a completed replacement leaves no staging residue")

        # Fail AFTER serialization and the preservation checks, but
        # before the replacement.
        original = census_storage._atomic_replace
        calls: list[bytes] = []

        def exploding(target, payload):
            calls.append(payload)
            raise OSError("injected: the machine died before the rename")

        census_storage._atomic_replace = exploding
        try:
            raised = None
            try:
                census_storage.record_result(path, result_document(commit=COMMIT_B))
            except OSError as error:
                raised = error
            expect(raised is not None and "injected" in str(raised),
                   "the injected failure propagates rather than being swallowed")
            expect(len(calls) == 1 and b"probe-census/v5" in calls[0],
                   "the candidate had been fully serialized before the failure")
        finally:
            census_storage._atomic_replace = original
        unchanged(path, before,
                  "a failure before replacement leaves the OLD census intact")
        expect(json.loads(path.read_text(encoding="utf-8"))["schema"]
               == "probe-census/v5",
               "and the old census is still a complete, readable document")

        # Stale staging residue from a killed writer is never
        # authoritative, and the next writer clears it.
        stale = path.parent / (census_storage.STAGING_PREFIX + "killed"
                               + census_storage.STAGING_SUFFIX)
        stale.write_text("{ truncated", encoding="utf-8")
        census_storage.record_result(path, result_document(commit=COMMIT_B))
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
        census_storage.record_result(path, result_document())
        census_storage.record_policy(path, "alpha", acceptable_failures=2,
                                   justification="kept")
        census_storage.record_result(path, result_document(commit=COMMIT_B))
        before = path.read_bytes()

        def refuses(mutation, msg, *fragments):
            def mutate(document):
                candidate = copy.deepcopy(document)
                mutation(candidate)
                return candidate, {"alpha": {"measurements"}}
            expect_refusal(lambda: census_storage.update(path, mutate),
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
        expect_refusal(lambda: census_storage.update(path, policy_mutate),
                       "a policy update appending an attempt is refused",
                       "which a policy update may not touch")
        unchanged(path, before, "...and changes no bytes")

        # Reconciliation may not silently lose a measurement either.
        def seed_mutate(document):
            candidate = copy.deepcopy(document)
            row(candidate)["census"]["history"] = []
            row(candidate)["census"]["current"] = None
            return candidate, census_storage.TOUCH_ANY
        expect_refusal(lambda: census_storage.update(path, seed_mutate),
                       "reconciliation losing a cohort is refused",
                       "append-only")
        unchanged(path, before, "...and changes no bytes")


# ==========================================================================
CONTENDER = """
import json, sys
sys.path.insert(0, {tools!r})
import probe_census_storage as census_storage
census_storage.record_result({path!r}, json.loads(sys.argv[1]))
"""


def test_independent_process_contention() -> None:
    print("\n-- independent-process contention --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        program = CONTENDER.format(tools=str(TOOLS_DIR), path=str(path))

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
            census_storage.DocsWorktreeMissing,
            lambda: census_storage.resolve_docs_worktree(str(main_wt)),
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
        saved = census_storage.subprocess.run
        gone = root / "never-existed"
        empty = root / "not-a-checkout"
        empty.mkdir()
        for target, why in ((gone, "a listed path that does not exist"),
                            (empty, "a listed path with no .git")):
            listing = (f"worktree {target}\nHEAD 1\n"
                       f"branch refs/heads/docs-wip\n")
            census_storage.subprocess.run = (
                lambda *a, _out=listing, **k: Listed(_out))
            try:
                expect_refusal_kind(
                    census_storage.DocsWorktreeMissing,
                    lambda: census_storage.resolve_docs_worktree(str(root)),
                    f"{why} is refused even with no prunable attribute")
                try:
                    census_storage.resolve_docs_worktree(str(root))
                except census_storage.DocsWorktreeMissing as error:
                    expect("not a usable checkout" in str(error),
                           f"...naming why ({why})")
            finally:
                census_storage.subprocess.run = saved
        expect(not gone.exists(),
               "and resolution never created the missing directory")

    # The record parser reads `prunable` as an attribute of its own
    # record, not as a line anywhere in the listing.
    records = census_storage._worktree_records(
        "worktree /a\nHEAD 1\nbranch refs/heads/master\n\n"
        "worktree /b\nHEAD 1\nbranch refs/heads/docs-wip\n"
        "prunable gitdir file points to non-existent location\n")
    expect(len(records) == 2 and "prunable" not in records[0]
           and records[1]["prunable"].startswith("gitdir file"),
           "prunable attaches to its own record, never a neighbour's")
    bare = census_storage._worktree_records(
        "worktree /a\nHEAD 1\ndetached\n")
    expect(len(bare) == 1 and bare[0]["detached"] == "",
           "a valueless porcelain attribute parses as an empty string")


#: The seven groups the aggregate runs first: what a record is, how an
#: older one migrates into it, and how a measurement or a harness error
#: enters one.
TESTS_RECORDS_AND_INGESTION = (
    test_record_shape,
    test_migration,
    test_seed_and_noop,
    test_reconciliation,
    test_ingest_accepted,
    test_ci_eligible_takes_no_measurement,
    test_ingest_harness_error,
)

#: The five #1428 groups the aggregate runs after the validation family:
#: where the writer may write, and what it guarantees while writing.
TESTS_PATHS_AND_PERSISTENCE = (
    test_path_substitution,
    test_atomicity,
    test_preservation_guard,
    test_independent_process_contention,
    test_unusable_docs_worktree,
)

#: This family's complete ordered inventory. The facade reconstructs it
#: from the two fragments above, so a fragment dropped out of the
#: aggregate's order fails rather than quietly shortening the run.
TESTS = TESTS_RECORDS_AND_INGESTION + TESTS_PATHS_AND_PERSISTENCE
