#!/usr/bin/env python3
"""The acceptable-failure policy, refusals, and the mutation CLI (#2129).

Eleven groups in two fragments, matching the aggregate's order:

  `TESTS_POLICY`  9 groups -- #1430's acceptable-failure policy end to
                  end: set, keep and explicit clear; the default and the
                  one-time initialization of an unset X; the bound, the
                  justification and the manual-only requirement; the
                  reset promotion demands; the threshold at X and at
                  X+1; the policy through the CLI; and then the generic
                  refusal contract, structurally malformed target rows,
                  and duplicate rows;
  `TESTS_CLI`     2 groups -- the mutation CLI's own contract and
                  #1479's durable typed justification, which the
                  aggregate runs after the storage family's persistence
                  block.

Every refusal case asserts the same safety promise the rest of this
gate does: a controlled `CensusError` naming the offending value, with
the authoritative bytes unchanged.
"""

from __future__ import annotations

import json
import shutil
import subprocess
import tempfile
from pathlib import Path

from .support import (
    attempt_record, census_contract, census_records, census_storage, cli,
    cli_repo, COMMIT_A, COMMIT_B, expect, expect_refusal, measurement,
    probe_census, registry, result_document, sample_record, scratch, seeded,
    summary_of, SYNTHETIC, unchanged, v1_document,
)

import probe_engine  # type: ignore  # noqa: E402 -- `.support` installs tools/


def test_policy() -> None:
    print("\n-- policy set, keep and explicit clear --")
    with registry(), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)
        census_storage.record_result(path, result_document())
        measured = json.loads(path.read_text(encoding="utf-8"))["probes"][0]["census"]

        def census_of(key="alpha"):
            rows = json.loads(path.read_text(encoding="utf-8"))["probes"]
            return {row["key"]: row["census"] for row in rows}[key]

        census_storage.record_policy(path, "alpha", acceptable_failures=2,
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

        census_storage.record_policy(path, "alpha", acceptable_failures=5)
        record = census_of()
        expect(record["acceptable_failures"] == 5
               and record["acceptable_failures_justification"]
               == "two engine-side races",
               "omitting --justification LEAVES the existing one unchanged")

        census_storage.record_policy(path, "alpha", justification=None,
                                   acceptable_failures=0)
        expect(census_of()["acceptable_failures_justification"] is None
               and census_of()["acceptable_failures"] == 0,
               "an explicit clear removes the justification only")

        census_storage.record_policy(path, "alpha", estimate=480)
        record = census_of()
        expect(record["estimated_worst_case_seconds"] == 480
               and record["acceptable_failures"] == 0,
               "--set-estimate stores the estimate and keeps X")

        census_storage.record_policy(path, "alpha",
                                   justification="restored", acceptable_failures=5)
        census_storage.record_policy(path, "alpha", acceptable_failures=1)
        record = census_of()
        expect(record["acceptable_failures"] == 1
               and record["acceptable_failures_justification"] == "restored",
               "lowering X leaves its justification alone (#1479)")
        expect(record["estimated_worst_case_seconds"] == 480,
               "lowering X leaves the estimate alone")
        census_storage.record_policy(path, "alpha", acceptable_failures=0)
        expect(census_of()["acceptable_failures_justification"] == "restored",
               "and X=0 may keep a justification: only an explicit clear "
               "removes one")

        census_storage.record_policy(path, "alpha", estimate=None)
        expect(census_of()["estimated_worst_case_seconds"] is None,
               "--set-estimate none clears the estimate")
        expect(census_of()["acceptable_failures"] == 0,
               "clearing the estimate leaves X alone")
        expect(census_of()["current"] == measured["current"],
               "no policy command ever disturbed the measurements")
        expect(census_of("beta") == census_records.empty_census(),
               "no policy command ever disturbed an unrelated row")

        before = path.read_bytes()
        expect_refusal(lambda: census_storage.record_policy(
            path, "nosuch", acceptable_failures=1),
            "a --probe naming no census row is refused",
            "nosuch", "no census row")
        unchanged(path, before, "a refused policy update changes no bytes")

        # #1430 closed the nullable X #1428 staged. The library refuses
        # it through the same `update` funnel the CLI does, so the
        # authoritative bytes survive either route.
        expect_refusal(lambda: census_storage.record_policy(
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
        "schema": census_contract.RECORD_SCHEMA,
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
            row("beta", {**census_records.empty_census(),
                         "acceptable_failures": 3,
                         "acceptable_failures_justification": "three races"}),
            row("retired", {**census_records.empty_census(),
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
        migrated = census_storage.ensure_document(legacy)
        expect([row["key"] for row in migrated["probes"]]
               == [key for key, _s, _p in SYNTHETIC],
               "a v1 migration covers exactly the live registry")
        expect(all(row["census"]["acceptable_failures"] == 0
                   for row in migrated["probes"]),
               "every v1-migrated and newly appended row is seeded at X=0")

        extra = list(SYNTHETIC) + [("delta", "delta_probe.py", "a new probe")]
        with registry(probes=extra):
            grown = census_storage.ensure_document(path)
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
        result = census_storage.ensure_document(path)
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
        census_storage.ensure_document(path)
        unchanged(path, before, "a second --seed initializes nothing further")


def test_acceptable_failure_policy_rules() -> None:
    """The three rules, each with its own rejecting case."""
    print("\n-- X is bounded, justified above 0, and manual-only above 0 --")
    with registry(ci_eligible={"beta"}), scratch() as root:
        path = root / "probe_census.json"
        seeded(path)

        census_storage.record_policy(path, "alpha", acceptable_failures=2,
                                   justification="two engine-side races")
        stored = json.loads(path.read_text(encoding="utf-8"))
        rows = {row["key"]: row["census"] for row in stored["probes"]}
        expect(rows["alpha"]["acceptable_failures"] == 2,
               "a justified tolerance on a manual-only probe is stored")
        before = path.read_bytes()

        # (1) the range, at both ends and just past the top.
        for value in (0, 9):
            census_storage.record_policy(path, "alpha",
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
                lambda v=value: census_storage.record_policy(
                    path, "alpha", acceptable_failures=v),
                f"storing {value!r} as X is refused ({why})", fragment)
            unchanged(path, before, f"...and wrote nothing ({value!r})")

        # (2) X>0 needs a reason, and whitespace is not one.
        census_storage.record_policy(path, "alpha", acceptable_failures=0,
                                   justification=None)
        before = path.read_bytes()
        for text, why in ((None, "a cleared justification"),
                          ("", "an empty justification"),
                          ("   \t\n ", "a whitespace-only justification")):
            expect_refusal(
                lambda t=text: census_storage.record_policy(
                    path, "alpha", acceptable_failures=1, justification=t),
                f"X=1 with {why} is refused",
                "no stated reason", "--justification")
            unchanged(path, before, f"...and wrote nothing ({why})")
        expect(census_storage.record_policy(
            path, "alpha", acceptable_failures=1,
            justification="one known race") == "alpha",
            "the same X with a real reason is accepted")

        # A stored reason satisfies it: a later X change need not resupply.
        census_storage.record_policy(path, "alpha", acceptable_failures=4)
        expect(json.loads(path.read_text(encoding="utf-8"))["probes"][0][
                   "census"]["acceptable_failures_justification"]
               == "one known race",
               "an X change reuses the STORED reason rather than demanding "
               "one again (#1479's independence survives)")

        # (3) tolerance is a manual-only concept.
        before = path.read_bytes()
        expect_refusal(
            lambda: census_storage.record_policy(
                path, "beta", acceptable_failures=1, justification="a race"),
            "a tolerance on a CI-eligible probe is refused",
            "ci-eligible", "manual-only concept")
        unchanged(path, before, "...and wrote nothing")
        census_storage.record_policy(path, "beta", acceptable_failures=0)
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
            census_storage.record_policy(path, "beta", acceptable_failures=3,
                                       justification="three races")
            census_storage.record_result(path, result_document(probe="beta"))
        before = path.read_bytes()

        with registry(ci_eligible={"beta"}):
            expect_refusal(
                lambda: census_storage.ensure_document(path),
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
                lambda: census_storage.record_policy(
                    path, "alpha", acceptable_failures=0),
                "an unrelated policy update is blocked while it stands",
                "beta")

            # Resetting X is the one move that unblocks it, and the
            # promotion then archives the cohort as it always did.
            census_storage.record_policy(path, "beta", acceptable_failures=0)
            result = census_storage.ensure_document(path)
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
    N = census_contract.POLICY_RUN_COUNT
    for x in range(census_contract.MIN_ACCEPTABLE_FAILURES,
                   census_contract.MAX_ACCEPTABLE_FAILURES + 1):
        expect(census_records.tolerance_state(x, N, N, x)
               == census_contract.TOLERANCE_ACCEPTABLE,
               f"X={x}: exactly {x} failure(s) in {N} runs is acceptable")
        expect(census_records.tolerance_state(x, N, N, x + 1)
               == census_contract.TOLERANCE_OVER,
               f"X={x}: {x + 1} failure(s) in {N} runs is over tolerance")
    expect(census_records.tolerance_state(0, N, N, 0)
           == census_contract.TOLERANCE_ACCEPTABLE
           and census_records.tolerance_state(0, N, N, 1)
           == census_contract.TOLERANCE_OVER,
           "X=0 means a clean sweep, and one failure breaches it")
    expect(census_records.tolerance_state(1, N, N, 0)
           == census_contract.TOLERANCE_ACCEPTABLE
           and census_records.tolerance_state(1, N, N, 1)
           == census_contract.TOLERANCE_ACCEPTABLE,
           "X=1 accepts both 10/10 and 9/10")

    # The basis is a COMPLETE fixed-N measurement, and nothing else.
    for runs, why in ((N - 1, "a shorter run"), (N + 1, "a longer run"),
                      (0, "no runs at all")):
        expect(census_records.tolerance_state(0, runs, runs, 0)
               == census_contract.TOLERANCE_NOT_COMPARABLE,
               f"{why} is not classified against a policy stated out of {N}")
    expect(census_records.tolerance_state(0, N, N - 1, 0)
           == census_contract.TOLERANCE_NOT_COMPARABLE,
           "and an INCOMPLETE ten-run measurement is not one either")
    for value, why in ((None, "a null X"), (True, "a boolean X"),
                       (2.5, "a fractional X"), (10, "an out-of-range X")):
        expect(census_records.tolerance_state(value, N, N, 0)
               == census_contract.TOLERANCE_NOT_COMPARABLE,
               f"{why} classifies nothing")
    expect(census_records.tolerance_state(1, N, N, None)
           == census_contract.TOLERANCE_NOT_COMPARABLE
           and census_records.tolerance_state(1, N, N, -1)
           == census_contract.TOLERANCE_NOT_COMPARABLE,
           "an unusable failure count classifies nothing either")

    # The measurement it is asked about is ONE sample, never a cohort's
    # pooled totals: two five-run measurements are not a ten-run one,
    # and two ten-run measurements are not a twenty-run one.
    def sized(runs, failures, mark):
        return {"requested_runs": runs, "completed_runs": runs,
                "failure_count": failures, "retained_artifacts": [mark]}

    expect(census_records.policy_sample({"samples": []}) is None
           and census_records.policy_sample({}) is None
           and census_records.policy_sample(None) is None,
           "a cohort with no samples has no policy measurement")
    expect(census_records.policy_sample(
        {"samples": [sized(5, 0, "a"), sized(5, 3, "b")]}) is None,
        "two five-run samples do NOT add up to a ten-run measurement")
    picked = census_records.policy_sample(
        {"samples": [sized(N, 0, "first"), sized(N, 4, "second")]})
    expect(picked is not None and picked["retained_artifacts"] == ["second"],
           "two ten-run samples stay comparable, and the LAST appended one "
           "is the current measurement")
    picked = census_records.policy_sample(
        {"samples": [sized(N, 0, "ten"), sized(3, 0, "three")]})
    expect(picked is not None and picked["retained_artifacts"] == ["ten"],
           "a later odd-sized run does not hide the newest ten-run one")
    expect(census_records.policy_sample(
        {"samples": [{"requested_runs": N, "completed_runs": N - 1,
                      "failure_count": 0}]}) is None,
        "an incomplete ten-run sample is not a policy measurement")
    expect(census_records.policy_sample({"samples": [7, None]}) is None,
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
    N = census_contract.POLICY_RUN_COUNT
    with registry(), scratch() as root:
        def tolerance(path):
            return summary_of(path)["tolerance"]

        # A single complete ten-run measurement, at X and at X+1.
        for failures, expected in ((1, "acceptable"), (2, "over-tolerance")):
            path = root / f"one-{failures}.json"
            seeded(path)
            census_storage.record_policy(path, "alpha", acceptable_failures=1,
                                       justification="one known race")
            census_storage.record_result(
                path, measurement(runs=N, failures=failures, age_days=1))
            expect(tolerance(path) == expected,
                   f"a single {N}-run measurement with {failures} failure(s) "
                   f"against X=1 is {expected}")

        # Split: two five-run measurements on one commit pool to ten
        # runs, and must NOT be read as a ten-run result.
        path = root / "split.json"
        seeded(path)
        census_storage.record_policy(path, "alpha", acceptable_failures=1,
                                   justification="one known race")
        census_storage.record_result(path, measurement(runs=5, failures=0,
                                                     age_days=2))
        census_storage.record_result(path, measurement(runs=5, failures=3,
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
        census_storage.record_policy(path, "alpha", acceptable_failures=1,
                                   justification="one known race")
        census_storage.record_result(path, measurement(runs=N, failures=0,
                                                     age_days=2))
        census_storage.record_result(path, measurement(runs=N, failures=5,
                                                     age_days=1))
        expect(summary_of(path)["requested_runs"] == 2 * N,
               "the cohort pools to twenty runs")
        expect(tolerance(path) == "over-tolerance",
               "...and the newest ten-run measurement is what classifies, "
               "rather than the pooled total falling out of the policy")

        # An odd-sized run afterwards does not erase the last real one.
        census_storage.record_result(path, measurement(runs=3, failures=0,
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
        expect_refusal(lambda: census_storage.record_result(broken,
                                                          result_document()),
                       "a census that is not valid JSON is refused",
                       "not valid JSON")
        unchanged(broken, broken_bytes,
                  "the malformed census is left byte-for-byte alone")

        # An absent or unmigrated census names --seed and never migrates.
        absent = root / "absent.json"
        expect_refusal(lambda: census_storage.record_result(absent,
                                                          result_document()),
                       "recording into an absent census names --seed", "--seed")
        expect(not absent.exists(),
               "the refusal did NOT seed the census as a side effect")
        legacy = root / "legacy.json"
        legacy.write_text(json.dumps(v1_document()) + "\n", encoding="utf-8")
        legacy_bytes = legacy.read_bytes()
        expect_refusal(lambda: census_storage.record_result(legacy,
                                                          result_document()),
                       "recording into a v1 census names --seed",
                       "probe-census/v1", "--seed")
        expect_refusal(lambda: census_storage.record_policy(
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
                lambda m=mutation: census_storage.record_result(
                    path, result_document(**m)),
                f"{why} is refused", fragment)
        expect_refusal(lambda: census_storage.record_result(path, [1, 2]),
                       "a result that is not an object is refused",
                       "is not of type 'object'")
        after = json.loads(path.read_text(encoding="utf-8"))
        expect(all(row["census"]["attempts"] == []
                   for row in after["probes"]),
               "an unrecognized status is NOT logged as a failed attempt")

        # A result naming a probe with no row.
        expect_refusal(lambda: census_storage.record_result(
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
                lambda m=mutation: census_storage.record_result(
                    path, result_document(**m)),
                f"{why} is refused", fragment)
        missing = result_document()
        del missing["worst_elapsed_seconds"]
        expect_refusal(lambda: census_storage.record_result(path, missing),
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
        return {"schema": census_contract.CENSUS_SCHEMA, "probes": rows}

    def row(key="alpha", census=None):
        return {"key": key, "script": "alpha_probe.py",
                "classification": "manual-only", "protocol": "legacy",
                "census": census_records.empty_census() if census is None
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
            ({"schema": census_contract.CENSUS_SCHEMA,
              "probes": {"alpha": {}}},
             "$.probes", "a `probes` mapping instead of a list"),
            (census_with(["alpha"]), "$.probes[0]",
             "a row that is a bare string"),
            (census_with([row(census={**census_records.empty_census(),
                                      "attempts": 5})]),
             "$.probes[0].census.attempts", "a non-list attempt log"),
            (census_with([row(census={**census_records.empty_census(),
                                      "history": "old",
                                      "current": {"commit_sha": COMMIT_B,
                                                  "samples": []}})]),
             "$.probes[0].census.history", "a non-list history"),
            (census_with([row(census={**census_records.empty_census(),
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
                lambda p=path: census_storage.record_result(p, result_document()),
                f"--record refuses {why}", fragment)
            unchanged(path, before, f"...and changes no bytes ({why})")

        # `--seed` walks the same rows, through reconcile_inventory.
        for index, (document, fragment, why) in enumerate(cases[:5]):
            path = root / f"seed-{index}.json"
            path.write_text(json.dumps(document), encoding="utf-8")
            before = path.read_bytes()
            expect_refusal(lambda p=path: census_storage.ensure_document(p),
                           f"--seed refuses {why}", fragment)
            unchanged(path, before, f"...and changes no bytes ({why})")

        # And a policy update, which addresses a row by key too.
        path = root / "policy.json"
        path.write_text(json.dumps(census_with([row(key=[])])), encoding="utf-8")
        before = path.read_bytes()
        expect_refusal(lambda: census_storage.record_policy(
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
                row(census={**census_records.empty_census(), field: 5})])),
                encoding="utf-8")
            before = path.read_bytes()
            expect_refusal(lambda p=path: census_storage.ensure_document(p),
                           f"--seed refuses a stored non-list `{field}`",
                           f"$.probes[0].census.{field}")
            unchanged(path, before, f"...and changes no bytes (`{field}`)")
            expect_refusal(
                lambda p=path: census_storage.record_result(p, result_document()),
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
            row(census={**census_records.empty_census(),
                        "history": [{"commit_sha": COMMIT_A, "samples": 4}]})])),
            encoding="utf-8")
        before = path.read_bytes()
        expect_refusal(lambda: census_storage.ensure_document(path),
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

        expect_refusal(lambda: census_storage.update(good, exploding),
                       "a structural/type error inside the transaction is a "
                       "controlled refusal, not a traceback",
                       "structurally malformed", "TypeError")
        unchanged(good, payload, "...and changes no bytes")

        # A CensusError from the mutation is NOT rewrapped: its own
        # actionable message survives.
        def refusing(_before):
            raise census_contract.CensusError("seed it first with --seed")

        expect_refusal(lambda: census_storage.update(good, refusing),
                       "a deliberate refusal keeps its own message",
                       "seed it first with --seed")


# ==========================================================================
def test_duplicate_target_rows() -> None:
    """A target key must name exactly one row (#1503)."""
    print("\n-- duplicate target rows --")

    def row(key, census=None):
        return {"key": key, "script": f"{key}_probe.py",
                "classification": "manual-only", "protocol": "legacy",
                "census": census_records.empty_census() if census is None
                else census}

    with registry(), scratch() as root:
        duped = root / "duplicate-target.json"
        duped.write_text(json.dumps({
            "schema": census_contract.CENSUS_SCHEMA,
            "probes": [row("alpha"), row("beta"), row("alpha")]}),
            encoding="utf-8")
        before = duped.read_bytes()
        expect_refusal(lambda: census_storage.record_result(duped,
                                                          result_document()),
                       "--record refuses a probe with two census rows",
                       "2 census rows", "--record")
        unchanged(duped, before, "...and changes no bytes")
        expect_refusal(lambda: census_storage.record_policy(
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
            "schema": census_contract.CENSUS_SCHEMA,
            "probes": [row("alpha"), row("beta"), row("beta")]}),
            encoding="utf-8")
        census_storage.record_result(elsewhere, result_document())
        stored = json.loads(elsewhere.read_text(encoding="utf-8"))
        expect(len(stored["probes"][0]["census"]["current"]["samples"]) == 1,
               "an unrelated duplicate row does not refuse the measurement")
        expect([r["key"] for r in stored["probes"]]
               == ["alpha", "beta", "beta"],
               "and the duplicate rows are preserved verbatim for a person")
        expect(all(r["census"] == census_records.empty_census()
                   for r in stored["probes"][1:]),
               "neither unrelated row was touched")


# ==========================================================================
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
               and stored("beta") == census_records.empty_census(),
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
            expect(all(row["census"] == census_records.empty_census()
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


#: The nine groups the aggregate runs after the outcomes family: the
#: policy itself, then the refusal contracts that share its surface.
TESTS_POLICY = (
    test_policy,
    test_acceptable_failure_policy_defaults,
    test_acceptable_failure_policy_rules,
    test_acceptable_failure_policy_promotion,
    test_acceptable_failure_threshold,
    test_acceptable_failure_policy_cli,
    test_refusals,
    test_malformed_rows_refuse_cleanly,
    test_duplicate_target_rows,
)

#: The two the aggregate runs after the storage family's persistence
#: block, in this order.
TESTS_CLI = (
    test_cli,
    test_cli_justification,
)

#: This family's complete ordered inventory, reconstructed by the facade
#: from the two fragments above.
TESTS = TESTS_POLICY + TESTS_CLI
