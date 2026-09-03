#!/usr/bin/env python3
"""Durable attempt outcomes and the deferral gate (#2129).

Two groups, in the aggregate's order, which runs them third -- straight
after the storage family's ingestion block, because both are about what
a run that did NOT produce a measurement leaves behind:

  `test_outcome_log`   #1439's append-only outcome log: every
                       non-success attempt recorded once, in order, with
                       its evidence, and never rewritten;
  `test_deferral_gate` explicit defer/resume -- a deferred probe retains
                       its evidence and leaves selection, the resume
                       condition is durable typed state, and the CLI
                       refuses to smuggle new deferral text through
                       `--resume`.
"""

from __future__ import annotations

import copy
import datetime
import json

from .support import (
    _alpha, attempt_record, census_contract, census_records, census_storage,
    census_summary, cli, cli_repo, COMMIT_A, expect, expect_refusal,
    probe_census, registry, result_document, rich_census, scratch, seeded,
    stored_v3_document, stored_v4_document, unchanged,
)


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
        census_storage.record_result(path, result_document())
        census_storage.record_claim(path, "alpha", {
            "token": "claim-1", "timestamp_utc": "2026-08-27T09:00:00Z",
            "commit_sha": COMMIT_A, "owner": "deflake", "host": "here",
            "pid": 4711, "lease_seconds": 3600.0, "requested_runs": 10})
        before = json.loads(path.read_text(encoding="utf-8"))

        census_storage.record_outcome(path, "alpha", outcome_record("a-1"))
        after = json.loads(path.read_text(encoding="utf-8"))
        row = census_records.find_entry(after, "alpha")["census"]
        expect([entry["attempt"] for entry in row["outcomes"]] == ["a-1"],
               "an outcome append lands in the row's own outcome log")
        was = census_records.find_entry(before, "alpha")["census"]
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
        census_storage.record_outcome(path, "alpha", outcome_record("a-1"))
        unchanged(path, stored,
                  "resuming the same attempt installs the identical bytes")
        expect_refusal(
            lambda: census_storage.record_outcome(
                path, "alpha", {**outcome_record("a-1"),
                                "summary": "a different account"}),
            "one attempt identity carrying different evidence is refused",
            "already recorded with different evidence")
        unchanged(path, stored, "...and changes no bytes")

        census_storage.record_outcome(path, "alpha", outcome_record("a-2"))
        expect([entry["attempt"] for entry in census_records.find_entry(
            json.loads(path.read_text(encoding="utf-8")),
            "alpha")["census"]["outcomes"]] == ["a-1", "a-2"],
            "a second attempt appends after the first, in order")

        # The record names its own probe, because it is handed BETWEEN
        # workflows; the two must agree rather than one being trusted.
        expect_refusal(
            lambda: census_storage.record_outcome(
                path, "beta", outcome_record("b-1")),
            "an outcome naming another probe is refused",
            "so it is not this row's outcome")
        expect_refusal(
            lambda: census_storage.record_outcome(path, "alpha", ["not", "it"]),
            "a non-object outcome record is refused",
            "must be a JSON object")
        expect_refusal(
            lambda: census_storage.record_outcome(
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
            lambda: census_storage.update(path, outcome_touching_measurements),
            "an outcome append that also logs an attempt is refused",
            "which a diagnosis outcome may not touch")
        unchanged(path, current, "...and changes no bytes")

        def measurement_touching_outcomes(document):
            candidate = copy.deepcopy(document)
            row = [r for r in candidate["probes"] if r["key"] == "alpha"][0]
            row["census"]["outcomes"].append(outcome_record("a-9"))
            return candidate, {"alpha": {"measurements"}}
        expect_refusal(
            lambda: census_storage.update(path, measurement_touching_outcomes),
            "a measurement ingestion appending an outcome is refused",
            "which a measurement ingestion may not touch")
        unchanged(path, current, "...and changes no bytes")

        def policy_touching_outcomes(document):
            candidate = copy.deepcopy(document)
            row = [r for r in candidate["probes"] if r["key"] == "alpha"][0]
            row["census"]["outcomes"] = []
            return candidate, {"alpha": {"policy"}}
        expect_refusal(
            lambda: census_storage.update(path, policy_touching_outcomes),
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
            expect_refusal(lambda: census_storage.update(path, rewrite),
                           f"{why} is refused", "append-only")
            unchanged(path, current, f"...and changes no bytes ({why})")

    # The v3 -> v5 migration adds the empty outcome log and null deferral.
    with registry(), scratch() as root:
        path = root / "legacy.json"
        stored = stored_v3_document()
        path.write_text(json.dumps(stored), encoding="utf-8")
        migrated = census_storage.ensure_document(path)
        expect(migrated["schema"] == census_contract.CENSUS_SCHEMA,
               "seeding a v3 census migrates it to the current schema")
        record = census_records.find_entry(migrated, "alpha")["census"]
        expect(record == {**stored["probes"][0]["census"], "outcomes": [],
                          "deferred": None},
               "...adding only the empty outcome log and null deferral")

    # The immediately previous schema needs only the new field.
    with registry(), scratch() as root:
        path = root / "legacy-v4.json"
        stored = stored_v4_document()
        path.write_text(json.dumps(stored), encoding="utf-8")
        migrated = census_storage.ensure_document(path)
        record = census_records.find_entry(migrated, "alpha")["census"]
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

        census_storage.record_deferral(
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

        summaries = census_summary.census_summary(
            deferred_document,
            now=datetime.datetime(2026, 8, 30, tzinfo=datetime.timezone.utc),
            stale_after_seconds=census_contract.DEFAULT_STALE_AFTER_SECONDS)
        alpha = summaries[0]
        expect(alpha["deferred"] == deferred["deferred"],
               "the selection-facing summary exposes the complete deferral")
        expect("deferred" in probe_census.render_summary([alpha]),
               "the human summary names the probe's deferred state")

        stable = path.read_bytes()
        census_storage.record_deferral(
            path, "alpha", reason=reason, resume_when=resume_when)
        unchanged(path, stable, "repeating the same deferral is a byte no-op")

        def deferral_touching_attempts(document):
            candidate = copy.deepcopy(document)
            _alpha(candidate)["deferred"] = None
            _alpha(candidate)["attempts"].append(attempt_record("forged"))
            return candidate, {"alpha": {"deferral"}}

        expect_refusal(
            lambda: census_storage.update(path, deferral_touching_attempts),
            "a deferral update cannot forge a measurement attempt",
            "which a deferral update may not touch")
        unchanged(path, stable, "the mixed-aspect deferral wrote nothing")

        def measurement_touching_deferral(document):
            candidate = copy.deepcopy(document)
            _alpha(candidate)["deferred"] = None
            return candidate, {"alpha": {"measurements"}}

        expect_refusal(
            lambda: census_storage.update(path, measurement_touching_deferral),
            "measurement ingestion cannot silently resume a deferred probe",
            "which a measurement ingestion may not touch")
        unchanged(path, stable, "the mixed-aspect measurement wrote nothing")

        def reconciliation_touching_deferral(document):
            candidate = copy.deepcopy(document)
            _alpha(candidate)["deferred"] = None
            return candidate, census_storage.TOUCH_ANY

        expect_refusal(
            lambda: census_storage.update(path, reconciliation_touching_deferral),
            "inventory reconciliation cannot silently resume a deferred probe",
            "reconciliation changed deferral field")
        unchanged(path, stable, "the mixed reconciliation wrote nothing")

        for bad, label in (("", "empty"), ("   ", "whitespace-only")):
            expect_refusal(
                lambda value=bad: census_storage.record_deferral(
                    path, "alpha", reason=value, resume_when=resume_when),
                f"an {label} deferral reason is refused", "non-blank")
            expect_refusal(
                lambda value=bad: census_storage.record_deferral(
                    path, "alpha", reason=reason, resume_when=value),
                f"an {label} resume condition is refused", "non-blank")
        expect_refusal(
            lambda: census_storage.record_deferral(
                path, "nosuch", reason=reason, resume_when=resume_when),
            "deferring an unknown probe is refused", "nosuch", "no census row")
        unchanged(path, stable, "no refused deferral changed the census")

        census_storage.record_deferral(path, "alpha", resume=True)
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
        census_storage.record_deferral(path, "alpha", resume=True)
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


#: This family's complete ordered inventory, and the whole of its
#: contribution to the aggregate.
TESTS = (
    test_outcome_log,
    test_deferral_gate,
)
