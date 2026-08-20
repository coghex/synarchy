#!/usr/bin/env python3
"""Focused self-test for the probe census record and its writer (#1428).

Deterministic, engine-free, GPU-free and fast: no probe is ever run and
no world is generated. Every case drives the shipped
`tools/probe_census.py` against a census file in a throwaway temp
directory, with `run_probes.PROBES`, `ci_probes.CI_ELIGIBLE` and
`probe_flake.PROTOCOL_PROBES` pointed at a synthetic registry where the
case needs one — never against the real `docs/probe_census.json`, which
lives only in a `docs-wip` worktree and is deliberately never published.

The measurement fixture is built from the REAL
`probe_flake.Measurement`/`RunRecord` classes rather than a hand-written
dictionary, so `validate_result` is checked against the document the
harness actually emits. Mutation coverage is then the point: every
validation rule is exercised from both sides, because a validator that
only ever sees valid input proves nothing.

The concurrency case uses independent PROCESSES, not threads: the
contract is a cross-process lock, and a thread-only test would pass
against a lock that does not exist.

Usage:
  python3 tools/test_probe_census.py
Exit codes: 0 = all cases passed, 1 = one or more failed.
"""
from __future__ import annotations

import json
import os
import shutil
import subprocess
import sys
import tempfile
import textwrap
from pathlib import Path

TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))
import ci_probes  # type: ignore  # noqa: E402
import probe_census  # type: ignore  # noqa: E402
import probe_flake  # type: ignore  # noqa: E402
import probe_protocol  # type: ignore  # noqa: E402
import run_probes  # type: ignore  # noqa: E402

FAILURES: list[str] = []

CHECKS = [("alpha", "the first check"), ("beta", "the second check")]
COMMIT_A = "a" * 40
COMMIT_B = "b" * 40
PROBE = "censusprobe"
OTHER = "otherprobe"
CIPROBE = "ciprobe"


def expect(cond: bool, msg: str) -> None:
    if not cond:
        FAILURES.append(msg)
        print(f"  FAIL: {msg}")
    else:
        print(f"  OK:   {msg}")


def expect_raises(exc, fn, msg: str, substring: str | None = None) -> None:
    try:
        fn()
    except exc as error:
        if substring is not None and substring not in str(error):
            FAILURES.append(f"{msg} (raised {exc.__name__} but not about "
                            f"{substring!r}: {error})")
            print(f"  FAIL: {msg} — wrong message: {error}")
            return
        print(f"  OK:   {msg}")
        return
    except Exception as error:  # noqa: BLE001 - a wrong exception is a failure
        FAILURES.append(f"{msg} (raised {type(error).__name__}: {error})")
        print(f"  FAIL: {msg} — raised {type(error).__name__}: {error}")
        return
    FAILURES.append(f"{msg} (nothing raised)")
    print(f"  FAIL: {msg} — nothing raised")


# ==========================================================================
# Fixtures
# ==========================================================================
class SyntheticRegistry:
    """`run_probes.PROBES`, `ci_probes.CI_ELIGIBLE` and
    `probe_flake.PROTOCOL_PROBES` pointed at a small invented registry.

    Restored on exit, because the real ones are module-level globals the
    rest of this file's cases read."""

    def __init__(self, probes=None, ci=None, protocol=None):
        self.probes = probes if probes is not None else [
            (PROBE, f"{PROBE}_probe.py", "the measured probe"),
            (OTHER, f"{OTHER}_probe.py", "an unrelated probe"),
            (CIPROBE, f"{CIPROBE}_probe.py", "a CI-eligible probe"),
        ]
        self.ci = set(ci if ci is not None else {CIPROBE})
        self.protocol = dict(protocol if protocol is not None else {
            PROBE: probe_protocol.PROTOCOL_VERSION,
            OTHER: probe_protocol.PROTOCOL_VERSION,
            CIPROBE: probe_protocol.PROTOCOL_VERSION,
        })

    def __enter__(self):
        self.saved = (run_probes.PROBES, ci_probes.CI_ELIGIBLE,
                      probe_flake.PROTOCOL_PROBES)
        run_probes.PROBES = self.probes
        ci_probes.CI_ELIGIBLE = self.ci
        probe_flake.PROTOCOL_PROBES = self.protocol
        self.root = Path(tempfile.mkdtemp(prefix="probe-census-test-"))
        return self

    def __exit__(self, *exc):
        (run_probes.PROBES, ci_probes.CI_ELIGIBLE,
         probe_flake.PROTOCOL_PROBES) = self.saved
        shutil.rmtree(self.root, ignore_errors=True)
        return False

    @property
    def census(self) -> Path:
        return self.root / "docs" / "probe_census.json"


def build_result(probe: str = PROBE, *, commit: str = COMMIT_A,
                 outcomes=("PASS", "FAIL"), harness_error: bool = False,
                 requested: int | None = None, marker: str | None = None) -> dict:
    """One `probe-flake-result/v1` document, built by the real harness types.

    `probe_flake.Measurement.to_document` is the producer the census has
    to accept, so the fixture is that object rather than a hand-rolled
    dictionary that could drift away from it silently.
    """
    descriptor = probe_protocol.build_descriptor(probe, CHECKS)
    runs = len(outcomes) if requested is None else requested
    measurement = probe_flake.Measurement(
        probe, descriptor, runs, 4, Path("/tmp/artifacts"),
        Path("/tmp/artifacts/inv"))
    measurement.commit_sha = commit
    measurement.timestamp = "2026-08-20T12:00:00Z"
    for index, outcome in enumerate(outcomes, start=1):
        checks = {"alpha": probe_protocol.PASS,
                  "beta": probe_protocol.PASS if outcome == "PASS"
                  else probe_protocol.FAIL}
        keep = outcome in ("FAIL", "TIMEOUT")
        measurement.runs.append(probe_flake.RunRecord(
            index, 8100 + index, outcome, 1.5 * index, checks,
            Path(f"/tmp/artifacts/inv/run-{index:03d}") if keep else None))
    if harness_error:
        measurement.status = "harness-error"
        measurement.error = "run 3: the protocol event stream was malformed"
        measurement.error_run = probe_flake.RunRecord(
            len(outcomes) + 1, 8199, probe_flake.RUN_HARNESS_ERROR, 0.5,
            {"alpha": probe_protocol.PASS, "beta": probe_protocol.MISSING},
            Path("/tmp/artifacts/inv/run-099"))
    document = measurement.to_document()
    if marker is not None:
        document["retained_artifacts"] = [marker]
    return document


def seeded(reg: SyntheticRegistry) -> dict:
    return probe_census.ensure_document(reg.census)


# ==========================================================================
# Cases
# ==========================================================================
def test_migration() -> None:
    print("\n-- migration of the #1425 seeded manifest --")
    with SyntheticRegistry() as reg:
        # Exactly #1425's shape: v1, no census key anywhere.
        v1 = {
            "schema": probe_census.SEED_SCHEMA,
            "probes": [
                {"key": OTHER, "script": f"{OTHER}_probe.py",
                 "classification": "manual-only", "protocol": "legacy"},
                {"key": PROBE, "script": f"{PROBE}_probe.py",
                 "classification": "manual-only", "protocol": "legacy"},
            ],
        }
        reg.census.parent.mkdir(parents=True, exist_ok=True)
        reg.census.write_text(json.dumps(v1, indent=2) + "\n", encoding="utf-8")

        migrated = probe_census.migrate_document(v1)
        expect(migrated["schema"] == probe_census.CENSUS_SCHEMA,
               "a v1 document migrates to probe-census/v2")
        expect([e["key"] for e in migrated["probes"]] == [OTHER, PROBE],
               "migration preserves the seeded entries in their own order")
        expect(all(e["census"] == probe_census.empty_census()
                   for e in migrated["probes"]),
               "migration gives every entry an empty census record")
        expect(migrated["probes"][0]["protocol"] == "legacy",
               "migration alone does not touch the inventory columns")
        expect(v1["probes"][0].get("census") is None,
               "migration does not mutate its input document")

        installed = seeded(reg)
        keys = [e["key"] for e in installed["probes"]]
        expect(keys[:2] == [OTHER, PROBE],
               "seeding an existing census keeps its entries in place")
        expect(CIPROBE in keys and len(keys) == 3,
               "a probe registered since the seed is appended, not regenerated")
        expect(installed["probes"][1]["protocol"] == probe_protocol.PROTOCOL_VERSION,
               "reconciliation refreshes the protocol column from the registry")
        expect(probe_census.validate_manifest(installed) == [],
               "the reconciled census agrees with the live registry")

        expect_raises(probe_census.CensusError,
                      lambda: probe_census.migrate_document(
                          {"schema": "probe-census/v9", "probes": []}),
                      "an unreadable future schema is refused", "v9")
        expect_raises(probe_census.CensusError,
                      lambda: probe_census.migrate_document({"schema": "probe-census/v1"}),
                      "a document with no probes list is refused", "must be a list")


def test_inventory_preservation() -> None:
    print("\n-- every real inventory entry survives migration --")
    total = len(run_probes.PROBES)
    with SyntheticRegistry(probes=run_probes.PROBES, ci=ci_probes.CI_ELIGIBLE,
                           protocol=probe_flake.PROTOCOL_PROBES) as reg:
        current = probe_census.build_manifest()
        v1 = {"schema": probe_census.SEED_SCHEMA,
              "probes": [{k: v for k, v in e.items() if k != "census"}
                         for e in current["probes"]]}
        reg.census.parent.mkdir(parents=True, exist_ok=True)
        reg.census.write_text(json.dumps(v1, indent=2) + "\n", encoding="utf-8")
        installed = seeded(reg)
        expect(len(installed["probes"]) == total,
               f"all {total} inventory entries survive the migration")
        expect([e["key"] for e in installed["probes"]] ==
               [e["key"] for e in v1["probes"]],
               "the manifest array order is preserved exactly")
        expect(all(isinstance(e.get("census"), dict) for e in installed["probes"]),
               "every entry gains a census record")
        expect(probe_census.validate_structure(installed) == [],
               "the installed census validates structurally")
        expect(probe_census.validate_manifest(installed) == [],
               "the installed census still agrees with the live registry")


def test_measurement_update() -> None:
    print("\n-- one manual-only measurement update --")
    with SyntheticRegistry() as reg:
        before = seeded(reg)
        probe_census.record_result(reg.census, build_result())
        after = probe_census.read_for_update(reg.census)

        entry = probe_census.find_entry(after, PROBE)
        census = entry["census"]
        expect(census["current"]["commit_sha"] == COMMIT_A,
               "the accepted measurement opens a cohort for its own commit")
        expect(len(census["current"]["samples"]) == 1,
               "the cohort holds exactly the one accepted sample")
        sample = census["current"]["samples"][0]
        expect([r["outcome"] for r in sample["runs"]] == ["PASS", "FAIL"],
               "the sample keeps the per-run outcomes")
        expect(sample["check_counts"]["beta"] == {"PASS": 1, "FAIL": 1,
                                                  "MISSING": 0},
               "the sample keeps the per-check counts")
        expect(sample["timestamp_utc"] == "2026-08-20T12:00:00Z"
               and sample["commit_sha"] == COMMIT_A,
               "the sample keeps its timestamp and commit hash")
        expect(sample["rts_capabilities"] == 4 and sample["peak_concurrency"] >= 1,
               "the sample keeps the RTS capability and concurrency values")
        expect(len(census["attempts"]) == 1 and census["attempts"][0]["accepted"],
               "the attempt log records the accepted ingestion")
        expect(all(k not in sample for k in ("stdout", "events", "engine_log")),
               "no raw stream is copied into the census")

        # Only the affected probe changed, and only by appending.
        old = {e["key"]: e for e in before["probes"]}
        new = {e["key"]: e for e in after["probes"]}
        expect([e["key"] for e in before["probes"]] ==
               [e["key"] for e in after["probes"]],
               "the update leaves the inventory order untouched")
        expect(all(old[k] == new[k] for k in old if k != PROBE),
               "every unrelated entry is deeply equal after the update")
        expect(old[PROBE]["script"] == new[PROBE]["script"]
               and old[PROBE]["classification"] == new[PROBE]["classification"],
               "the touched entry keeps its inventory columns")

        # Policy fields are stored, not chosen.
        probe_census.record_policy(reg.census, PROBE, acceptable_failures=2,
                                   justification="two known engine races")
        probe_census.record_policy(reg.census, PROBE, estimate=480.0)
        census = probe_census.find_entry(
            probe_census.read_for_update(reg.census), PROBE)["census"]
        expect(census["acceptable_failures"] == 2
               and census["acceptable_failures_justification"] ==
               "two known engine races",
               "X and its justification are stored verbatim")
        expect(census["estimated_worst_case_seconds"] == 480.0,
               "the estimated worst-case duration is stored beside the "
               "observed one")
        expect(census["current"]["samples"][0]["worst_elapsed_seconds"] == 3.0,
               "the observed worst elapsed time stays a separate, measured "
               "field")


def test_cohorts() -> None:
    print("\n-- commit cohorts and the attempt log --")
    with SyntheticRegistry() as reg:
        seeded(reg)
        probe_census.record_result(reg.census, build_result(commit=COMMIT_A,
                                                            marker="a1"))
        probe_census.record_result(reg.census, build_result(commit=COMMIT_A,
                                                            marker="a2"))
        census = probe_census.find_entry(
            probe_census.read_for_update(reg.census), PROBE)["census"]
        expect(len(census["current"]["samples"]) == 2 and not census["history"],
               "a second measurement for the same commit appends to the "
               "current cohort")

        probe_census.record_result(reg.census, build_result(commit=COMMIT_B,
                                                            marker="b1"))
        census = probe_census.find_entry(
            probe_census.read_for_update(reg.census), PROBE)["census"]
        expect(census["current"]["commit_sha"] == COMMIT_B
               and len(census["current"]["samples"]) == 1,
               "a different commit opens a new current cohort")
        expect(len(census["history"]) == 1
               and census["history"][0]["commit_sha"] == COMMIT_A
               and len(census["history"][0]["samples"]) == 2,
               "the complete prior cohort is archived, not discarded")

        # A well-formed harness error is logged, but contributes nothing.
        probe_census.record_result(
            reg.census, build_result(commit=COMMIT_B, harness_error=True,
                                     requested=5, marker="err"))
        census = probe_census.find_entry(
            probe_census.read_for_update(reg.census), PROBE)["census"]
        expect(len(census["current"]["samples"]) == 1,
               "a harness-error result contributes no sample")
        expect(len(census["attempts"]) == 4
               and census["attempts"][-1]["accepted"] is False
               and census["attempts"][-1]["status"] == "harness-error",
               "a harness-error result is logged as a refused attempt")
        expect(census["attempts"][-1]["retained_artifacts"] == ["err"],
               "the attempt log keeps the artifact reference, not the artifact")


def test_ci_eligible_entries() -> None:
    print("\n-- CI-eligible entries take no current samples --")
    with SyntheticRegistry() as reg:
        seeded(reg)
        before = reg.census.read_bytes()
        expect_raises(probe_census.CensusError,
                      lambda: probe_census.record_result(
                          reg.census, build_result(CIPROBE)),
                      "a CI-eligible probe's measurement is refused",
                      "CI-eligible")
        expect(reg.census.read_bytes() == before,
               "the refused CI-eligible sample left the census byte-identical")

        # Promotion: the live classification flips under an entry that
        # already has a cohort, history and attempts.
        probe_census.record_result(reg.census, build_result(commit=COMMIT_A))
        probe_census.record_result(reg.census, build_result(commit=COMMIT_B))
        ci_probes.CI_ELIGIBLE = {CIPROBE, PROBE}
        promoted = probe_census.ensure_document(reg.census)
        census = probe_census.find_entry(promoted, PROBE)["census"]
        expect(probe_census.find_entry(promoted, PROBE)["classification"]
               == "ci-eligible",
               "promotion updates the classification from tools/ci_probes.py")
        expect(census["current"] is None,
               "promotion clears the current-sample field")
        expect(len(census["history"]) == 2
               and [c["commit_sha"] for c in census["history"]] ==
               [COMMIT_A, COMMIT_B],
               "promotion archives the manual-only cohort and retains history")
        expect(len(census["attempts"]) == 2,
               "promotion retains the attempt log")
        expect_raises(probe_census.CensusError,
                      lambda: probe_census.record_result(
                          reg.census, build_result(PROBE)),
                      "a promoted probe takes no further current samples",
                      "CI-eligible")


def test_result_validation() -> None:
    print("\n-- result validation, from both sides --")
    with SyntheticRegistry() as reg:
        document = seeded(reg)
        valid = build_result()
        expect(probe_census.validate_result(valid, document) == [],
               "the document probe_flake actually emits is accepted")

        def mutated(fn):
            doc = json.loads(json.dumps(valid))
            fn(doc)
            return probe_census.validate_result(doc, document)

        cases = [
            ("schema", lambda d: d.update({"schema": "probe-flake-result/v9"})),
            ("no census entry", lambda d: d.update({"probe": "ghost"})),
            ("commit hash", lambda d: d.update({"commit_sha": "abc123"})),
            ("commit hash", lambda d: d.update({"commit_sha": "z" * 40})),
            ("timestamp_utc", lambda d: d.update({"timestamp_utc": "yesterday"})),
            ("requested_runs", lambda d: d.update({"requested_runs": 0})),
            ("completed", lambda d: d.update({"completed_runs": 99})),
            ("rts_capabilities", lambda d: d.update({"rts_capabilities": 0})),
            ("peak_concurrency", lambda d: d.update({"peak_concurrency": 0})),
            ("elapsed_seconds", lambda d: d["runs"][0].update(
                {"elapsed_seconds": -1})),
            ("elapsed_seconds", lambda d: d["runs"][0].update(
                {"elapsed_seconds": float("inf")})),
            ("outcome", lambda d: d["runs"][0].update({"outcome": "MAYBE"})),
            ("contiguous", lambda d: d["runs"][0].update({"index": 7})),
            ("descriptor declares", lambda d: d["runs"][0]["checks"].pop("alpha")),
            ("is not one of", lambda d: d["runs"][0]["checks"].update(
                {"alpha": "MAYBE"})),
            ("the runs show", lambda d: d["check_counts"]["alpha"].update(
                {"PASS": 9})),
            ("failure_count", lambda d: d.update({"failure_count": 9})),
            ("timeout_count", lambda d: d.update({"timeout_count": 9})),
            ("worst_elapsed_seconds", lambda d: d.update(
                {"worst_elapsed_seconds": 99.0})),
            ("total_elapsed_seconds", lambda d: d.update(
                {"total_elapsed_seconds": 99.0})),
            ("failure_rate", lambda d: d.update({"failure_rate": 0.99})),
            ("no `error`", lambda d: d.update({"error": "surprise"})),
            ("retained_artifacts", lambda d: d.update(
                {"retained_artifacts": [7]})),
            ("non-empty list", lambda d: d.update({"checks": []})),
            ("{id, label} object", lambda d: d["checks"].append({"id": 7})),
            ("appears twice", lambda d: d["checks"].append(d["checks"][0])),
        ]
        for substring, mutate in cases:
            problems = mutated(mutate)
            expect(any(substring in p for p in problems),
                   f"a mutated result is rejected for {substring!r} "
                   f"(got {problems[:1]})")

        broken = build_result(harness_error=True, requested=5)
        expect(probe_census.validate_result(broken, document) == [],
               "a well-formed harness-error result is accepted for logging")
        expect(any("failure_rate" in p for p in probe_census.validate_result(
            {**broken, "failure_rate": 0.5}, document)),
               "a harness-error result claiming a failure rate is rejected")
        expect(probe_census.validate_result([], document) != [],
               "a non-object result is rejected")


def test_malformed_state_and_recovery() -> None:
    print("\n-- malformed state, interrupted write, stale staging --")
    with SyntheticRegistry() as reg:
        seeded(reg)
        probe_census.record_result(reg.census, build_result(marker="keep"))
        good = reg.census.read_bytes()

        # Malformed input never touches the census.
        expect_raises(probe_census.CensusError,
                      lambda: probe_census.record_result(
                          reg.census, {"schema": "nope"}),
                      "a malformed result document is refused")
        expect(reg.census.read_bytes() == good,
               "the refused result left the census byte-for-byte unchanged")
        census = probe_census.find_entry(
            probe_census.read_for_update(reg.census), PROBE)["census"]
        expect(len(census["attempts"]) == 1,
               "a malformed result is not written to the attempt log")

        # An interrupted replacement leaves the previous census whole.
        real_replace = probe_census.os.replace

        def exploding(*args, **kwargs):
            raise OSError("simulated crash between fsync and rename")
        probe_census.os.replace = exploding
        try:
            expect_raises(OSError,
                          lambda: probe_census.record_result(
                              reg.census, build_result(marker="lost")),
                          "a failure before the rename propagates")
        finally:
            probe_census.os.replace = real_replace
        expect(reg.census.read_bytes() == good,
               "the interrupted write left the previous census unchanged")
        expect(probe_census.validate_structure(
            probe_census.read_for_update(reg.census)) == [],
               "the previous census is still valid after the interruption")

        # A stale staging file — what a killed writer leaves behind — is
        # never authoritative and is cleared by the next writer.
        stale = reg.census.parent / f"{probe_census.STAGING_PREFIX}zz" \
            f"{probe_census.STAGING_SUFFIX}"
        stale.write_text("{ truncated", encoding="utf-8")
        probe_census.record_result(reg.census, build_result(marker="after"))
        expect(not stale.exists(),
               "the next writer removes the stale staging file")
        census = probe_census.find_entry(
            probe_census.read_for_update(reg.census), PROBE)["census"]
        markers = [s["retained_artifacts"] for s in census["current"]["samples"]]
        expect(markers == [["keep"], ["after"]],
               "the stale staging file never became the census")

        # Malformed census STATE is a clean stop, not a silent repair.
        for bad in ("{ not json",
                    json.dumps({"schema": "probe-census/v9", "probes": []}),
                    json.dumps({"schema": "probe-census/v2", "probes": [
                        {"key": PROBE, "script": "p.py",
                         "classification": "manual-only", "protocol": "legacy",
                         "census": {"history": "not a list"}}]})):
            reg.census.write_text(bad, encoding="utf-8")
            snapshot = reg.census.read_bytes()
            expect_raises(probe_census.CensusError,
                          lambda: probe_census.record_result(
                              reg.census, build_result()),
                          f"malformed census state is refused ({bad[:24]!r})")
            expect(reg.census.read_bytes() == snapshot,
                   "the refused write left the malformed file untouched")


def test_missing_docs_worktree() -> None:
    print("\n-- a missing docs-wip worktree stops, touching nothing --")
    scratch = Path(tempfile.mkdtemp(prefix="probe-census-git-"))
    try:
        subprocess.run(["git", "init", "-q", str(scratch)], check=True,
                       capture_output=True)
        expect_raises(probe_census.DocsWorktreeMissing,
                      lambda: probe_census.manifest_path(str(scratch)),
                      "with no docs-wip worktree the census path is an "
                      "actionable stop", "git worktree add")
        expect(not (scratch / "docs").exists(),
               "nothing was created in the checkout that lacks the worktree")
        primary = Path(run_probes.REPO_ROOT) / probe_census.MANIFEST_RELPATH
        expect(not primary.exists(),
               "the primary checkout never receives the census file")
    finally:
        shutil.rmtree(scratch, ignore_errors=True)


CONCURRENT_WORKER = textwrap.dedent('''\
    import json, sys
    sys.path.insert(0, {tools!r})
    import probe_census

    census, template, worker, count = sys.argv[1:5]
    base = json.loads(open(template).read())
    for i in range(int(count)):
        result = json.loads(json.dumps(base))
        result["retained_artifacts"] = ["w{{}}-{{}}".format(worker, i)]
        probe_census.record_result(census, result)
''')


def test_concurrent_writers() -> None:
    print("\n-- concurrent independent processes, no lost update --")
    workers, per_worker = 6, 4
    with SyntheticRegistry() as reg:
        seeded(reg)
        template = reg.root / "result.json"
        template.write_text(json.dumps(build_result()), encoding="utf-8")
        script = reg.root / "worker.py"
        script.write_text(CONCURRENT_WORKER.format(tools=str(TOOLS)),
                          encoding="utf-8")

        children = [
            subprocess.Popen(
                [sys.executable, str(script), str(reg.census), str(template),
                 str(w), str(per_worker)],
                stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
            for w in range(workers)
        ]
        failed = []
        for child in children:
            _out, err = child.communicate(timeout=300)
            if child.returncode != 0:
                failed.append(err.strip().splitlines()[-1:] or ["(no output)"])
        expect(not failed, f"every writer process succeeded ({failed[:2]})")

        document = probe_census.read_for_update(reg.census)
        expect(probe_census.validate_structure(document) == [],
               "the census is still structurally valid after the contention")
        census = probe_census.find_entry(document, PROBE)["census"]
        markers = {s["retained_artifacts"][0]
                   for s in census["current"]["samples"]}
        want = {f"w{w}-{i}" for w in range(workers) for i in range(per_worker)}
        expect(markers == want,
               f"all {workers * per_worker} concurrent appends survived "
               f"(missing {sorted(want - markers)[:3]}, "
               f"extra {sorted(markers - want)[:3]})")
        expect(len(census["attempts"]) == workers * per_worker,
               "every concurrent attempt is in the append-only log exactly once")
        expect(len(probe_census.find_entry(document, OTHER)["census"]["attempts"])
               == 0,
               "the unrelated probe was never touched by any writer")


def test_write_path_refuses_data_loss() -> None:
    print("\n-- the writer refuses a candidate that loses data --")
    with SyntheticRegistry() as reg:
        seeded(reg)
        probe_census.record_result(reg.census, build_result(commit=COMMIT_A))
        probe_census.record_result(reg.census, build_result(commit=COMMIT_B))
        good = reg.census.read_bytes()

        def drop_history(before):
            candidate = json.loads(json.dumps(before))
            probe_census.find_entry(candidate, PROBE)["census"]["history"] = []
            return candidate, {PROBE}

        def touch_a_stranger(before):
            candidate = json.loads(json.dumps(before))
            probe_census.find_entry(candidate, OTHER)["census"][
                "acceptable_failures"] = 9
            return candidate, {PROBE}

        def reorder(before):
            candidate = json.loads(json.dumps(before))
            candidate["probes"].reverse()
            return candidate, {PROBE}

        for label, mutate, substring in (
                ("discarded history", drop_history, "lost retained"),
                ("an unrelated entry", touch_a_stranger, "unrelated probe"),
                ("a reordered manifest", reorder, "order or membership")):
            expect_raises(probe_census.CensusError,
                          lambda m=mutate: probe_census.update(reg.census, m),
                          f"the writer refuses {label}", substring)
            expect(reg.census.read_bytes() == good,
                   f"the refused candidate ({label}) changed nothing on disk")


def test_lock_identity() -> None:
    print("\n-- one stable lock identity per resolved target --")
    with SyntheticRegistry() as reg:
        reg.census.parent.mkdir(parents=True, exist_ok=True)
        direct = probe_census.lock_path(reg.census)
        indirect = probe_census.lock_path(
            reg.census.parent / ".." / "docs" / "probe_census.json")
        expect(direct == indirect,
               "two spellings of one target resolve to the same lock")
        other = probe_census.lock_path(reg.root / "docs" / "other.json")
        expect(direct != other,
               "a different target gets a different lock")
        expect(direct.parent == reg.census.parent.resolve()
               and direct.name.endswith(probe_census.LOCK_SUFFIX),
               "the lock is a sibling of its target, out of reach of a "
               "temp reaper")


def test_seed_never_overwrites() -> None:
    print("\n-- seeding never overwrites accumulated census data --")
    with SyntheticRegistry() as reg:
        seeded(reg)
        probe_census.record_result(reg.census, build_result(marker="kept"))
        probe_census.record_policy(reg.census, PROBE, acceptable_failures=1,
                                   justification="one known race")
        before = probe_census.read_for_update(reg.census)
        seeded(reg)
        after = probe_census.read_for_update(reg.census)
        expect(before == after,
               "re-seeding an existing census is a no-op for its content")
        census = probe_census.find_entry(after, PROBE)["census"]
        expect(census["current"]["samples"][0]["retained_artifacts"] == ["kept"]
               and census["acceptable_failures"] == 1,
               "the accumulated sample and policy survive re-seeding")

        # A fresh seed is only ever written to an ABSENT target.
        reg.census.unlink()
        fresh = seeded(reg)
        expect(all(e["census"] == probe_census.empty_census()
                   for e in fresh["probes"]),
               "an absent target receives a fresh, empty seed")


def main() -> int:
    for test in (test_migration, test_inventory_preservation,
                 test_measurement_update, test_cohorts,
                 test_ci_eligible_entries, test_result_validation,
                 test_malformed_state_and_recovery,
                 test_missing_docs_worktree, test_concurrent_writers,
                 test_write_path_refuses_data_loss,
                 test_lock_identity, test_seed_never_overwrites):
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
