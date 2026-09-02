#!/usr/bin/env python3
"""Focused self-test for the CI-promotion report (#1441, extracted #2034).

Deterministic, engine-free, GPU-free and offline: every case runs
against synthetic documents in a throwaway temporary tree. Nothing here
boots an engine, runs a registered probe, or touches the developer's
real `docs-wip` worktree; the only subprocess is `git`, building the
scratch repository the CLI case resolves.

The real `tools/probe_census_promotion.py` is imported and driven --
through the real `tools/probe_census.py` writer, with
`probe_runner_registry.PROBES`, `ci_probes.CI_ELIGIBLE`,
`ci_probes.MANUAL_ONLY_REASONS` and `probe_flake.PROTOCOL_PROBES`
pointed at a synthetic registry -- so this exercises the shipped code
paths rather than a copy.

The report is REPORTING, so every case asserts two things at once: what
the report says, and that it changed nothing.

Where this runs
---------------
Both places, and that is deliberate. `CASES` below is this module's
inventory, and `tools/test_probe_census.py` runs every entry of it
inside its own sequence, appending to the same `selftestlib.FAILURES`
list -- so `python3 tools/test_probe_census.py` still exits non-zero on
a promotion regression, exactly as it did before the extraction. That
gate is the one CI and `tools/ci-local.sh` invoke, and this file is
added to NEITHER: like `tools/test_probe_census_page.py`, running it
directly is an iteration convenience, not a second CI step.

Usage:
  python3 tools/test_probe_census_promotion.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import copy
import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import ci_probes  # type: ignore  # noqa: E402
import probe_census  # type: ignore  # noqa: E402
import probe_census_promotion  # type: ignore  # noqa: E402
import probe_flake  # type: ignore  # noqa: E402
import probe_protocol  # type: ignore  # noqa: E402
import probe_runner_registry  # type: ignore  # noqa: E402

import selftestlib  # noqa: E402
from probe_census_selftest_support import (  # noqa: E402
    COMMIT_A, COMMIT_B, DAY, FAILURES, NOW, SYNTHETIC, at, cli, cli_repo,
    expect, expect_refusal, registry, result_document, scratch, seeded,
    unchanged,
)


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
    return probe_census_promotion.promotion_report(
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
        expect(report["schema"] == probe_census_promotion.PROMOTION_SCHEMA,
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


#: This module's complete case inventory, in the order it runs them.
#: `tools/test_probe_census.py` reads this rather than naming the cases
#: again, so a case added here joins the census gate by construction and
#: cannot be covered locally while silently missing from CI.
CASES = (
    test_promotion_candidate,
    test_promotion_disqualifications,
    test_promotion_reason_buckets,
    test_promotion_preserves_the_manifest,
    test_promotion_cli,
)

#: The module globals the fixtures install and must hand back. Every one
#: of them is a live registry the production code reads at call time, so
#: a case leaking one would silently steer whatever ran next -- in this
#: process or in `test_probe_census.py`'s longer sequence.
PATCHED_SEAMS = (
    (probe_runner_registry, "PROBES"),
    (ci_probes, "CI_ELIGIBLE"),
    (ci_probes, "MANUAL_ONLY_REASONS"),
    (probe_flake, "PROTOCOL_PROBES"),
)


def seam_snapshot() -> tuple:
    """What `PATCHED_SEAMS` holds right now, by identity."""
    return tuple(getattr(module, name) for module, name in PATCHED_SEAMS)


def expect_seams_restored(before: tuple) -> None:
    """Every patched registry is the object it was before the cases ran.

    Identity, not equality: `registry()` installs fresh containers, so a
    restore that rebuilt an equal one would still have replaced the real
    registry the rest of the process reads.
    """
    after = seam_snapshot()
    leaked = [f"{module.__name__}.{name}"
              for (module, name), was, now in zip(PATCHED_SEAMS, before, after)
              if was is not now]
    expect(not leaked,
           f"every patched registry is restored after the promotion cases "
           f"({leaked} still installed)")


def run_cases() -> None:
    """Every case, then the guard that no fixture leaked a registry.

    `test_probe_census.py` calls this too, so the guard runs whichever
    entry point drove the cases.
    """
    before = seam_snapshot()
    for case in CASES:
        case()
    expect_seams_restored(before)


def main(argv: list[str] | None = None) -> int:
    selftestlib.parse_verbose(argv)
    if not CASES:
        # Unreachable while the tuple above is populated. Kept as the
        # last word on the invariant: an emptied inventory must not be
        # able to report a pass, here or in the census gate.
        print("no promotion cases collected -- refusing to report a "
              "vacuous pass")
        return 1
    run_cases()
    print()
    if FAILURES:
        print(f"{len(FAILURES)} FAILED:")
        for message in FAILURES:
            print(f"  - {message}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(
        0, "probe_census promotion self-test: all cases pass")


if __name__ == "__main__":
    sys.exit(main())
