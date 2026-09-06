#!/usr/bin/env python3
"""#1439's cases: the non-success outcomes of a de-flake attempt.

The 36 cases covering `tools/deflake_outcome.py` — what is recorded
when an attempt ends in something other than a landed repair, which
documents that recording refuses, and how a recorded outcome survives
a crash, a replay and a census that is being written concurrently.

The fixtures below are the ones only these cases read: the forged and
truncated result documents, the redeclared descriptors, and the
`expect_non_success` / `expect_handoff_rejected` /
`expect_nothing_recorded` assertions built on them. The outcome
identity constants, `outcome_handoff`, `record_outcome`, `census_file`
and `stored_outcomes` are in `deflake_diagnosis_selftest_support` instead,
because #1438's publication cases build a defect handoff out of an
outcome record and read the same census.

Not a gate of its own. Run through the facade:

  python3 tools/test_deflake_diagnosis.py --only outcome
"""
from __future__ import annotations

import copy
import json
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import ci_probes  # type: ignore  # noqa: E402
import deflake_diagnosis as dd  # type: ignore  # noqa: E402
import deflake_outcome as do  # type: ignore  # noqa: E402
import probe_census  # type: ignore  # noqa: E402
# The census write these cases intercept resolves `_atomic_replace` in
# the STORAGE owner's globals, not the facade's (#2131), so that is
# the object they patch.
import probe_census_storage as census_storage  # type: ignore  # noqa: E402
import probe_flake  # type: ignore  # noqa: E402
from deflake_diagnosis_selftest_support import (  # noqa: E402
    ATTEMPT, BASE_COMMIT, CHECKS, CLEAN_WT, FAIL, FAILURES, MISSING,
    OTHER, OUTCOME_NOW, OUTCOME_SUMMARY, OUTSIDE, PASS, PRIMARY_WT, PROBE,
    Publisher, REPAIR_COMMIT, REPAIR_WT, VERIFY_ARTIFACTS, WORKTREES,
    census_file, command, config_entries, diagnosis_document,
    elsewhere_failure_result, evaluate, expect, failing_runs,
    forged_aggregate_result, handoff_document, manifest, outcome_handoff,
    produced, record_outcome, result_document, route_diagnosis, short_result,
    spotless_result, stored_outcomes, verification_result)


UNMET = ("the verification stayed above the acceptable-failure ceiling the "
         "handoff carries")


def missing_check_result(cid: str = "gamma") -> dict:
    """Ten PASSING runs, one declared check never emitted in any of them.

    The shape `error_run`'s clause and the per-check clause exist for:
    every run's OUTCOME is PASS and the aggregate failure count is zero,
    so a classifier reading only `runs` or only `failure_count` would
    call this spotless.
    """
    ids = [c for c, _label in CHECKS]
    run = {name: (MISSING if name == cid else PASS) for name in ids}
    return result_document(runs=[dict(run) for _ in range(dd.RUN_COUNT)])


def clean_timeout_result() -> dict:
    """A run that timed out AFTER emitting every declared check.

    Honest aggregates, spotless per-check tallies, and one run whose
    OUTCOME is TIMEOUT. Only the run list says anything went wrong, so
    it is what isolates that clause from the tally beside it.
    """
    ids = [c for c, _label in CHECKS]
    runs = [{name: PASS for name in ids} for _ in range(dd.RUN_COUNT - 1)]
    return result_document(
        runs=runs + [{**{name: PASS for name in ids}, "__timeout__": True}])


def timed_out_result() -> dict:
    ids = [c for c, _label in CHECKS]
    runs = [{name: PASS for name in ids} for _ in range(dd.RUN_COUNT - 1)]
    timed_out = {name: (PASS if name == ids[0] else MISSING) for name in ids}
    timed_out["__timeout__"] = True
    return result_document(runs=runs + [timed_out])


def forged_ok_result(*, error_run: bool = False, error=None) -> dict:
    """A complete all-PASS document that still carries a harness fault.

    `probe_flake` never writes one — `stop_with_harness_error` sets the
    status, the error and the error run together — but the DECLARED
    schema accepts it, and the run that broke the stream is deliberately
    kept OUT of `runs`. So a classifier that read only `runs`, or only
    the aggregates, would see a spotless batch here. These are the
    fields that say otherwise.
    """
    ids = [c for c, _label in CHECKS]
    keep = dd.RUN_COUNT - 1
    document = result_document(
        requested=keep,
        runs=[{name: PASS for name in ids} for _ in range(keep)])
    if error_run:
        broken = result_document(harness_error=True)["error_run"]
        document["error_run"] = broken
        # The retention pairing is #1437's and it is checked before any
        # of this: `probe_flake` keeps the broken run's directory, and
        # `retained_artifacts` is exactly the non-null ones in run order
        # with the error run last. Omitting it would make the fixture
        # malformed rather than a spotless-looking batch that kept one.
        document["retained_artifacts"] = list(
            document["retained_artifacts"]) + [broken["artifact_dir"]]
    if error is not None:
        document["error"] = error
    return document


def forged_verification_counts() -> dict:
    """An all-PASS verification batch reporting two failures.

    The other half of the reviewer's 4 -> 2 shape: paired with a
    `phantom-failures` baseline it reads as a measured improvement while
    neither batch observed anything at all.
    """
    document = verification_result(artifact_root=VERIFY_ARTIFACTS,
                                   runs=failing_runs(2, abort=False))
    document["failure_count"] = 2
    document["failure_rate"] = 0.2
    for run in document["runs"]:
        run["outcome"] = probe_flake.RUN_PASS
    for tally in document["check_counts"].values():
        tally[FAIL] = 0
        tally[PASS] = dd.RUN_COUNT
    for run in document["runs"]:
        for cid in run["checks"]:
            run["checks"][cid] = PASS
    document["retained_artifacts"] = []
    for run in document["runs"]:
        run["artifact_dir"] = None
    return document


def discarded_failure_result(count: int = 3) -> dict:
    """A failing batch that kept none of its failing runs' logs.

    `probe_flake.measure` deletes a run's directory the moment it passes
    and retains every unsuccessful one, so this is producer-impossible —
    and `probe_census.validate_result` does not look at the pairing at
    all. Recorded, it would be a failure nobody can diagnose stored as
    the evidence FOR a diagnosis.
    """
    document = result_document(runs=failing_runs(count, abort=False))
    for run in document["runs"]:
        run["artifact_dir"] = None
    document["retained_artifacts"] = []
    return document


def forged_completion_result() -> dict:
    """Nine all-PASS runs under a `completed_runs` of ten.

    The producer writes `completed_runs` as `len(runs)`, and nothing on
    the way back in re-establishes that: the declared schema does not,
    and `probe_census.validate_result` binds `check_counts` to the runs
    that ARE there, so nine matching tallies keep it consistent. A
    completeness test of `completed_runs == requested_runs` therefore
    passes, and the batch would be stored as ten of ten.
    """
    ids = [c for c, _label in CHECKS]
    document = result_document(
        runs=[{name: PASS for name in ids} for _ in range(dd.RUN_COUNT - 1)])
    document["completed_runs"] = dd.RUN_COUNT
    return document


def non_target_missing_verification() -> dict:
    """A clean verification that still fails #1437's MISSING rule.

    Zero failures, so the COUNT half of the acceptance gate is
    satisfied, and the targets (`beta`, `gamma`) are emitted in every
    run — but `alpha` is never emitted at all, which `missing_problems`
    refuses twice over: a PASSING run must omit nothing, and no declared
    identifier may vanish from the batch. `deflake_diagnosis.evaluate`
    routes this to `partial-improvement`, so a consumer that only looked
    at the targets would call it a passing verification and record
    nothing.
    """
    ids = [c for c, _label in CHECKS]
    run = {name: (MISSING if name == ids[0] else PASS) for name in ids}
    return verification_result(artifact_root=VERIFY_ARTIFACTS,
                               runs=[dict(run) for _ in range(dd.RUN_COUNT)])


def referenced(route: str, section: str, **fields) -> dict:
    """A handoff whose producer reference for `section` was moved.

    The binding checks the AGREEMENT between the record's reference and
    the document it names, so moving either side asks the same
    question — and moving the REFERENCE is what keeps the result
    document a shape `probe_flake.measure` could have written, which
    #1437's own result gate now requires of every declared measurement.
    """
    document = outcome_handoff(route)
    document["diagnosis_outcome"][section].update(fields)
    return document


def redeclared_result(*, rename=None, relabel=None, drop=False,
                      reorder=False, **kwargs) -> dict:
    """A spotless batch that reports against a DIFFERENT declared contract.

    Everything `_bind_to_producer` compares stays identical — probe,
    commit, instant, artifact root, invocation directory, retained
    artifacts — because a spotless batch retains nothing and the fixture
    keeps its stamps. Only the descriptor moves.
    """
    checks = [list(pair) for pair in CHECKS]
    if rename is not None:
        checks[-1][0] = rename
    if relabel is not None:
        checks[-1][1] = relabel
    if drop:
        checks = checks[:-1]
    if reorder:
        checks = [checks[1], checks[0]] + checks[2:]
    declared = [(cid, label) for cid, label in checks]
    return result_document(
        checks=declared,
        runs=[{cid: PASS for cid, _label in declared}
              for _ in range(dd.RUN_COUNT)],
        **kwargs)


def restamped(document: dict, stamp: str = "2026-08-22T09:30:00Z") -> dict:
    """The same measurement, claiming another instant."""
    document = copy.deepcopy(document)
    document["timestamp_utc"] = stamp
    return document


def expect_non_success(thunk, fragment: str, msg: str) -> None:
    """`thunk` is a well-formed handoff whose ending is not stable."""
    try:
        thunk()
    except do.NonSuccess as error:
        expect(fragment in str(error),
               f"{msg}: refused, but for {str(error)!r} rather than "
               f"{fragment!r}")
        return
    except do.HandoffError as error:
        FAILURES.append(f"{msg}: rejected the INPUT ({error}) where the "
                        f"EVIDENCE should have been refused")
        return
    FAILURES.append(f"{msg}: recorded a stable outcome")


def expect_handoff_rejected(thunk, fragment: str, msg: str) -> None:
    try:
        thunk()
    except do.HandoffError as error:
        expect(fragment in str(error),
               f"{msg}: rejected, but for {str(error)!r} rather than "
               f"{fragment!r}")
        return
    except do.NonSuccess as error:
        FAILURES.append(f"{msg}: refused the EVIDENCE ({error}) where the "
                        f"input should have been rejected")
        return
    FAILURES.append(f"{msg}: accepted")


def expect_nothing_recorded(path, before: bytes, publisher, msg: str) -> None:
    """The one assertion every non-success owes: no trace, anywhere."""
    expect(Path(path).read_bytes() == before,
           f"{msg}: the census bytes changed")
    expect(not stored_outcomes(path),
           f"{msg}: an outcome was recorded anyway")
    expect(publisher.calls == [],
           f"{msg}: the pull-request publisher was called")


def test_this_workflow_answers_every_route_1437_hands_it() -> None:
    """Parity with the producer, so a new route is a failure not a crash.

    #1437 decides which issue owns each ending. If it grows another one
    owned by #1439 and this module's table is not extended, the
    workflow would raise a `KeyError` on a real handoff; this is what
    turns that into a red test on the day the route is added.
    """
    owned = {route for route, owner in dd.ROUTE_OWNER.items()
             if owner == 1439}
    expect(set(do.ROUTE_TO_OUTCOME) == owned,
           f"every route #1437 hands to #1439 has an answer here; got "
           f"{sorted(do.ROUTE_TO_OUTCOME)} against {sorted(owned)}")
    expect(set(do.ROUTE_ROLES) == set(do.ROUTE_TO_OUTCOME),
           f"and each of them declares the roles its evidence needs; got "
           f"{sorted(do.ROUTE_ROLES)}")
    expect(set(do.ROUTE_TO_OUTCOME.values()) == set(do.STABLE_OUTCOMES),
           f"and between them they reach every stable outcome; got "
           f"{sorted(set(do.ROUTE_TO_OUTCOME.values()))}")
    for route, roles in do.ROUTE_ROLES.items():
        expect(roles["designated"] in roles["required"],
               f"the {route!r} route is judged on a measurement it also "
               f"requires; got {roles}")
        expect(not (set(roles["required"]) & set(roles["forbidden"])),
               f"the {route!r} route does not both require and forbid a "
               f"role; got {roles}")
        expect(set(roles["required"]) <= set(do.ROLES)
               and set(roles["forbidden"]) <= set(do.ROLES),
               f"the {route!r} route names only declared roles; got {roles}")
    # The routes #1437 keeps or hands elsewhere are not silently owned.
    for route in dd.ROUTES:
        if route in owned:
            continue
        expect(route not in do.ROUTE_TO_OUTCOME,
               f"the {route!r} route is not #1439's, and this workflow does "
               f"not claim it")


def test_a_spotless_controlled_baseline_records_cannot_reproduce() -> None:
    """The one predicate that may conclude "nothing is wrong here"."""
    with census_file() as path:
        recorded, publisher = record_outcome(outcome_handoff(), path)
        record = recorded.record
        expect(record["outcome"] == do.OUTCOME_CANNOT_REPRODUCE,
               f"a spotless controlled baseline is cannot-reproduce; got "
               f"{record['outcome']}")
        expect(publisher.calls == [],
               "and it opens no pull request")
        expect(not recorded.resumed and recorded.durable,
               "the first append is a real, durable write")

        stored = stored_outcomes(path)
        expect(len(stored) == 1 and stored[0] == record,
               f"the census row holds exactly the record that was built; got "
               f"{len(stored)} outcome(s)")
        recommendation = record["recommendation"]
        expect(isinstance(recommendation, dict)
               and recommendation["action"] == "de-list"
               and recommendation["advisory"] is True,
               f"with an advisory de-list recommendation; got "
               f"{recommendation}")
        expect(record["comparison"] is None,
               "and no before-and-after comparison, which it has no second "
               "batch for")
        expect(record["attempt"] == ATTEMPT and record["probe"] == PROBE
               and record["timestamp_utc"] == OUTCOME_NOW
               and record["baseline_sha"] == BASE_COMMIT
               and record["summary"] == OUTCOME_SUMMARY,
               "the record identifies the attempt, probe, instant, baseline "
               "commit and diagnostic summary")
        expect(record["acceptable_failures"] == 0
               and record["targets"] == ["beta", "gamma"],
               f"and the ceiling and targets it was judged against; got "
               f"X={record['acceptable_failures']}, {record['targets']}")

        # The stored document is a census this tool will still read.
        document = json.loads(path.read_text(encoding="utf-8"))
        try:
            probe_census.validate_document(
                document, probe_census.CENSUS_SCHEMA, "the written census")
        except probe_census.CensusError as error:
            FAILURES.append(f"an outcome append leaves an invalid census: "
                            f"{error}")


def test_a_no_target_ending_is_cannot_reproduce_on_its_own_measurement()\
        -> None:
    """`/deflake`'s all-PASS ending runs no controlled batch at all.

    It is #1437's `no-target` route, owned by #1439, and it means what
    `cannot-reproduce` means. The predicate is the SAME one, so an
    all-PASS ending cannot be recorded on weaker evidence than a
    controlled one.
    """
    with census_file() as path:
        recorded, publisher = record_outcome(
            outcome_handoff(dd.ROUTE_NO_TARGET), path)
        expect(recorded.record["outcome"] == do.OUTCOME_CANNOT_REPRODUCE,
               f"an all-PASS #1436 measurement is cannot-reproduce; got "
               f"{recorded.record['outcome']}")
        expect(recorded.record["measurements"][0]["role"] == do.ROLE_HANDOFF,
               "recorded against the measurement it actually has")
        expect(publisher.calls == [], "and it opens no pull request")

    # The same route over a measurement that is NOT spotless.
    with census_file() as path:
        before = path.read_bytes()
        handoff = outcome_handoff(dd.ROUTE_NO_TARGET, measurements=[
            {"role": do.ROLE_HANDOFF, "exit_code": probe_flake.EXIT_OK,
             "result": missing_check_result()}])
        publisher = Publisher()
        expect_non_success(
            lambda: record_outcome(handoff, path, publisher=publisher),
            "check 'gamma' is MISSING",
            "a no-target ending whose measurement lost a check")
        expect_nothing_recorded(path, before, publisher,
                                "a no-target ending over a MISSING check")


def test_untrustworthy_evidence_is_never_cannot_reproduce() -> None:
    """The load-bearing distinction of the whole issue.

    "We could not make it fail" and "we could not measure it" are
    opposite conclusions. Each input below must return an actionable
    non-success and record NO `cannot-reproduce` outcome — not be
    accepted as one, and not crash.
    """
    cases = (
        ("a failing run", probe_flake.EXIT_OK,
         result_document(runs=failing_runs(1, abort=False)),
         f"run 1 is {probe_flake.RUN_FAIL}"),
        ("a timed-out run", probe_flake.EXIT_OK, clean_timeout_result(),
         f"run {dd.RUN_COUNT} is {probe_flake.RUN_TIMEOUT}"),
        ("an incomplete run set", probe_flake.EXIT_OK, short_result(),
         f"completed {dd.RUN_COUNT - 1} of {dd.RUN_COUNT} requested runs"),
        ("a completion count its own run list contradicts",
         probe_flake.EXIT_OK, forged_completion_result(),
         f"reports {dd.RUN_COUNT} completed run(s) while its run list holds "
         f"{dd.RUN_COUNT - 1}"),
        ("a check that never arrived", probe_flake.EXIT_OK,
         missing_check_result(), "check 'gamma' is MISSING"),
        ("an untrustworthy harness result", probe_flake.EXIT_HARNESS_ERROR,
         result_document(harness_error=True), "exited 4"),
        ("a retained error run beside an all-PASS list", probe_flake.EXIT_OK,
         forged_ok_result(error_run=True), "kept an error run"),
        ("a reported harness error beside an all-PASS list",
         probe_flake.EXIT_OK,
         forged_ok_result(error="the event stream could not be trusted"),
         "reports the harness error"),
        ("a rejection before execution", probe_flake.EXIT_REJECTED, None,
         "exited 2"),
        ("port exhaustion", probe_flake.EXIT_NO_PORT, None, "exited 3"),
    )
    for label, code, result, fragment in cases:
        with census_file() as path:
            before = path.read_bytes()
            handoff = outcome_handoff(measurements=[
                {"role": do.ROLE_BASELINE, "exit_code": code,
                 "result": result}])
            publisher = Publisher()
            expect_non_success(
                lambda h=handoff, p=publisher: record_outcome(
                    h, path, publisher=p),
                fragment, f"{label} classified as cannot-reproduce")
            expect_nothing_recorded(path, before, publisher, label)


def test_a_condition_that_was_never_established_never_de_lists() -> None:
    """#1437 reaches `cannot-reproduce` three ways, and only one de-lists.

    A controlled batch whose CONFIGURATION could not be recreated from
    the handoff's manifest passed somewhere else, and "somewhere else"
    is no evidence about this probe. The evidence is still recorded —
    that is what this workflow exists for — but recommending a de-list
    from it would promote a measurement the invocation itself says was
    not the condition.
    """
    other = manifest(entries=[("config/probe.local.yaml", "0" * 64)])
    diagnosis = diagnosis_document(route=dd.ROUTE_CANNOT_REPRODUCE)
    diagnosis["baseline"]["result"] = spotless_result()
    diagnosis["baseline"]["configuration"] = other
    record = evaluate(diagnosis).to_document()
    expect(record["reason"] == dd.REASON_CONFIGURATION_NOT_RECREATED,
           f"#1437 routes a manifest mismatch here; got {record['reason']}")

    with census_file() as path:
        recorded, publisher = record_outcome(outcome_handoff(
            dd.ROUTE_CANNOT_REPRODUCE, diagnosis_outcome=record,
            measurements=[
                {"role": do.ROLE_BASELINE, "exit_code": probe_flake.EXIT_OK,
                 "result": diagnosis["baseline"]["result"]}]), path)
        expect(recorded.record["outcome"] == do.OUTCOME_CANNOT_REPRODUCE,
               f"the evidence is still recorded; got "
               f"{recorded.record['outcome']}")
        expect(recorded.record["recommendation"] is None,
               f"but nothing is recommended from it; got "
               f"{recorded.record['recommendation']}")
        expect(recorded.record["reason"]
               == dd.REASON_CONFIGURATION_NOT_RECREATED,
               "and the record says which condition was never established")
        expect(publisher.calls == [], "and it opens no pull request")

    # The batch that DID run under the handoff's condition still does.
    with census_file() as path:
        recorded, _publisher = record_outcome(outcome_handoff(), path)
        expect(recorded.record["reason"]
               == dd.REASON_BASELINE_OBSERVED_NOTHING
               and recorded.record["recommendation"] is not None,
               "a baseline under the recorded condition still recommends "
               "de-listing, or this case proves nothing")


def test_a_condition_failure_the_documents_cannot_see_is_still_recorded()\
        -> None:
    """#1437's gate is wider than the two halves a consumer can re-derive.

    Two comparison worktrees holding different configuration is a fact
    about the INVOCATION, not about either result document, so a clean
    4 -> 0 verification looks like a passing one here. #1437 routes it
    to `partial-improvement`, and that stable outcome has to be recorded
    rather than argued with.
    """
    diagnosis = route_diagnosis(dd.ROUTE_PARTIAL_IMPROVEMENT)
    diagnosis["verification"]["result"] = verification_result(
        artifact_root=VERIFY_ARTIFACTS)
    diagnosis["verification"]["configuration"] = manifest(
        entries=[("config/probe.local.yaml", "1" * 64)])
    record = evaluate(diagnosis).to_document()
    expect(record["reason"] == dd.REASON_VERIFICATION_NOT_COMPARABLE,
           f"#1437 routes an incomparable pair here; got "
           f"{record['reason']}")

    with census_file() as path:
        recorded, publisher = record_outcome(outcome_handoff(
            dd.ROUTE_PARTIAL_IMPROVEMENT, diagnosis_outcome=record,
            measurements=[
                {"role": do.ROLE_BASELINE, "exit_code": probe_flake.EXIT_OK,
                 "result": diagnosis["baseline"]["result"]},
                {"role": do.ROLE_VERIFICATION,
                 "exit_code": probe_flake.EXIT_OK,
                 "result": diagnosis["verification"]["result"]}]), path)
        expect(recorded.record["outcome"] == do.OUTCOME_PARTIAL_IMPROVEMENT,
               f"a 4 -> 0 verification the producer called incomparable is "
               f"still recorded; got {recorded.record['outcome']}")
        expect(recorded.record["comparison"]["unmet_condition"]
               == dd.REASON_VERIFICATION_NOT_COMPARABLE,
               "with the condition #1437 says stayed unmet")
        expect(publisher.calls == [], "and it opens no pull request")


def test_an_inconsistent_aggregate_reaches_no_stable_outcome() -> None:
    """A document that contradicts itself cannot support ANY conclusion.

    Nothing upstream establishes it: `probe_census.validate_result` binds
    `check_counts` to `runs` and refuses a PASS run carrying a FAIL
    check, but says nothing about `failure_count`, `timeout_count` or
    `failure_rate`. So an all-PASS run list under a forged failure count
    is schema-valid — and it would otherwise read as a REPRODUCED
    failure, which is the evidence `no-confident-fix` and
    `partial-improvement` rest on, not only the evidence
    `cannot-reproduce` denies.
    """
    diagnosis, _record = produced(dd.ROUTE_PARTIAL_IMPROVEMENT)
    clean = diagnosis["baseline"]["result"]

    def entry(role, result):
        return {"role": role, "exit_code": probe_flake.EXIT_OK,
                "result": result}

    cases = []
    for shape, fragment in (
            ("silent-timeout", "reports 0 failing run(s) while its run list "
                               "shows 1"),
            ("phantom-failures", "reports 3 failing run(s) while its run "
                                 "list shows 0"),
            ("phantom-timeouts", "reports 2 timed-out run(s) while its run "
                                 "list shows 0"),
            ("phantom-rate", "not the 0.0 its own counts imply")):
        forged = forged_aggregate_result(shape)
        cases.append((f"a {shape} baseline recorded as cannot-reproduce",
                      outcome_handoff(measurements=[
                          entry(do.ROLE_BASELINE, forged)]), fragment))
        cases.append((f"a {shape} baseline recorded as no-confident-fix",
                      outcome_handoff(dd.ROUTE_NO_CONFIDENT_FIX,
                                      measurements=[
                                          entry(do.ROLE_BASELINE, forged)]),
                      fragment))
    # The reviewer's exact partial-improvement shape: an all-PASS
    # baseline and verification whose forged counts read as 4 -> 2.
    improving = outcome_handoff(dd.ROUTE_PARTIAL_IMPROVEMENT, measurements=[
        entry(do.ROLE_BASELINE, forged_aggregate_result("phantom-failures")),
        entry(do.ROLE_VERIFICATION, forged_verification_counts())])
    cases.append(("a forged 4 -> 2 improvement over two all-PASS batches",
                  improving, "while its run list shows 0"))

    for label, document, fragment in cases:
        with census_file() as path:
            before = path.read_bytes()
            publisher = Publisher()
            expect_non_success(
                lambda d=document, p=publisher: record_outcome(
                    d, path, publisher=p),
                fragment, label)
            expect_nothing_recorded(path, before, publisher, label)


def test_a_measurement_of_another_state_is_not_this_attempts_evidence()\
        -> None:
    """Bound to the diagnosis outcome's OWN references, not just the probe.

    A well-formed batch of the same probe taken at another commit or
    another instant would otherwise be accepted under a diagnosis that
    judged a different one, and the census would store two conflicting
    accounts of one attempt.
    """
    other_commit = "c" * 40
    cases = (
        ("a baseline measured at another commit",
         outcome_handoff(rebind=False, measurements=[
             {"role": do.ROLE_BASELINE, "exit_code": probe_flake.EXIT_OK,
              "result": spotless_result(commit=other_commit)}]),
         "is not the measurement that diagnosis judged"),
        ("a baseline measured at another instant",
         outcome_handoff(rebind=False, measurements=[
             {"role": do.ROLE_BASELINE, "exit_code": probe_flake.EXIT_OK,
              "result": restamped(spotless_result())}]),
         "is not the measurement that diagnosis judged"),
        # These two move the REFERENCE rather than the result. #1437's
        # own layout rule binds an invocation directory to the root it
        # sits directly under, so a result document cannot differ in one
        # of them alone without becoming malformed — and it is the
        # AGREEMENT between the record and the document that this
        # checks, which side is moved being arbitrary.
        ("a baseline reference naming another artifact root",
         referenced(dd.ROUTE_CANNOT_REPRODUCE, "baseline",
                    artifact_root=f"{OUTSIDE}/elsewhere"),
         "reports artifact_root"),
        ("a baseline reference naming another invocation directory",
         referenced(dd.ROUTE_CANNOT_REPRODUCE, "baseline",
                    invocation_dir=f"{OUTSIDE}/artifacts/"
                                   f"{PROBE}-20260821T130000Z-4712-beefcafe"),
         "reports invocation_dir"),
        ("a baseline reference naming another instant",
         referenced(dd.ROUTE_CANNOT_REPRODUCE, "baseline",
                    timestamp_utc="2026-08-22T09:30:00Z"),
         "reports timestamp_utc"),
        ("a baseline naming another batch's retained artifacts",
         outcome_handoff(dd.ROUTE_NO_CONFIDENT_FIX, rebind=False,
                         measurements=[
                             {"role": do.ROLE_BASELINE,
                              "exit_code": probe_flake.EXIT_OK,
                              "result": result_document(
                                  runs=failing_runs(3))}]),
         "retained_artifacts"),
        ("a no-confident-fix carrying a verification",
         outcome_handoff(dd.ROUTE_NO_CONFIDENT_FIX, measurements=[
             {"role": do.ROLE_BASELINE, "exit_code": probe_flake.EXIT_OK,
              "result": produced(dd.ROUTE_NO_CONFIDENT_FIX)[0]["baseline"][
                  "result"]},
             {"role": do.ROLE_VERIFICATION,
              "exit_code": probe_flake.EXIT_OK,
              "result": verification_result(
                  artifact_root=VERIFY_ARTIFACTS)}]),
         "runs no verification batch"),
    )
    for label, document, fragment in cases:
        with census_file() as path:
            before = path.read_bytes()
            publisher = Publisher()
            expect_handoff_rejected(
                lambda d=document, p=publisher: record_outcome(
                    d, path, publisher=p),
                fragment, label)
            expect_nothing_recorded(path, before, publisher, label)

    # A producer record that nulled a reference for a batch its own
    # route DID run. The route-level policy above cannot see it, so the
    # binding is what refuses a measurement of work no reference names.
    with census_file() as path:
        before = path.read_bytes()
        _diagnosis, record = produced(dd.ROUTE_CANNOT_REPRODUCE)
        record["baseline"] = None
        document = outcome_handoff(diagnosis_outcome=record)
        publisher = Publisher()
        expect_handoff_rejected(
            lambda: record_outcome(document, path, publisher=publisher),
            "records no baseline batch",
            "a producer record with no reference for the batch it judged")
        expect_nothing_recorded(path, before, publisher,
                                "a nulled batch reference")

    # The stored `baseline_sha` and the per-batch reference are two
    # independent statements about one commit, so the pre-fix roles are
    # held to BOTH. Moving `baseline_sha` and the handoff identity
    # together — which the identity reconciliation requires — leaves the
    # reference and the measurement agreeing with each other and
    # disagreeing with the commit the row is about to record.
    with census_file() as path:
        before = path.read_bytes()
        _diagnosis, record = produced(dd.ROUTE_CANNOT_REPRODUCE)
        record["baseline_sha"] = other_commit
        record["handoff"]["commit_sha"] = other_commit
        document = outcome_handoff(diagnosis_outcome=record)
        publisher = Publisher()
        expect_handoff_rejected(
            lambda: record_outcome(document, path, publisher=publisher),
            "not at the diagnosis outcome's baseline commit",
            "a producer record whose baseline_sha contradicts the batch it "
            "references")
        expect_nothing_recorded(path, before, publisher,
                                "a contradictory baseline_sha")

    # The input identity #1437 states twice, disagreeing with itself.
    for label, mutate, fragment in (
        ("probe", lambda r: r["handoff"].__setitem__("probe", OTHER),
         "one record cannot be about two probes"),
        ("baseline commit",
         lambda r: r["handoff"].__setitem__("commit_sha", other_commit),
         "identifies no baseline at all"),
        ("acceptable-failure ceiling",
         lambda r: r["handoff"].__setitem__("acceptable_failures", 3),
         "the two cannot differ"),
        ("target list",
         lambda r: r["handoff"].__setitem__("targets", ["beta"]),
         "names two sets names neither"),
    ):
        with census_file() as path:
            before = path.read_bytes()
            _diagnosis, record = produced(dd.ROUTE_CANNOT_REPRODUCE)
            mutate(record)
            document = outcome_handoff(diagnosis_outcome=record)
            publisher = Publisher()
            expect_handoff_rejected(
                lambda d=document, p=publisher: record_outcome(
                    d, path, publisher=p),
                fragment,
                f"a producer record whose two statements of the {label} "
                f"disagree")
            expect_nothing_recorded(path, before, publisher, label)


def test_a_reproduced_failure_with_no_bounded_repair_is_no_confident_fix()\
        -> None:
    with census_file() as path:
        recorded, publisher = record_outcome(
            outcome_handoff(dd.ROUTE_NO_CONFIDENT_FIX), path)
        record = recorded.record
        expect(record["outcome"] == do.OUTCOME_NO_CONFIDENT_FIX,
               f"a reproduced failure nobody can attribute is "
               f"no-confident-fix; got {record['outcome']}")
        expect(record["recommendation"] is None
               and record["comparison"] is None,
               "carrying neither a de-list recommendation nor a comparison")
        expect(publisher.calls == [], "and opening no pull request")

        measurement = record["measurements"][0]
        expect(measurement["role"] == do.ROLE_BASELINE
               and measurement["failure_count"] == 4
               and measurement["completed_runs"] == dd.RUN_COUNT
               and measurement["check_counts"]["beta"][FAIL] == 4
               and measurement["retained_artifacts"],
               f"with the baseline evidence it rests on; got {measurement}")
        expect(record["summary"] == OUTCOME_SUMMARY,
               "and the diagnostic summary that explains it")

    # Evidence that reproduced NOTHING is the other outcome, not this one.
    with census_file() as path:
        before = path.read_bytes()
        handoff = outcome_handoff(dd.ROUTE_NO_CONFIDENT_FIX, measurements=[
            {"role": do.ROLE_BASELINE, "exit_code": probe_flake.EXIT_OK,
             "result": spotless_result()}])
        publisher = Publisher()
        expect_non_success(
            lambda: record_outcome(handoff, path, publisher=publisher),
            "reproduced nothing to attribute",
            "a spotless baseline recorded as no-confident-fix")
        expect_nothing_recorded(path, before, publisher,
                                "a spotless no-confident-fix")


def test_a_lower_rate_that_still_fails_the_gate_is_partial_improvement()\
        -> None:
    with census_file() as path:
        recorded, publisher = record_outcome(
            outcome_handoff(dd.ROUTE_PARTIAL_IMPROVEMENT), path)
        record = recorded.record
        expect(record["outcome"] == do.OUTCOME_PARTIAL_IMPROVEMENT,
               f"a measured improvement that still fails the gate is "
               f"partial-improvement; got {record['outcome']}")
        expect(publisher.calls == [],
               "a lower failure rate is not success, so it opens no PR")
        comparison = record["comparison"]
        expect(comparison == {"baseline_failure_count": 4,
                              "verification_failure_count": 2,
                              "acceptable_failures": 0,
                              "requested_runs": dd.RUN_COUNT,
                              "unmet_condition":
                                  dd.REASON_VERIFICATION_OVER_TOLERANCE},
               f"with the before-and-after counts, the X-out-of-N ceiling "
               f"and the unmet acceptance condition; got {comparison}")
        expect([m["role"] for m in record["measurements"]]
               == [do.ROLE_BASELINE, do.ROLE_VERIFICATION],
               "and both measurements it compared")
        expect(record["recommendation"] is None,
               "and no de-list recommendation, which only cannot-reproduce "
               "carries")


def test_the_acceptance_gate_is_1437s_own_missing_rule() -> None:
    """Not just "no target went MISSING".

    #1437's scoped MISSING rule has four clauses, and only one of them
    is about targets. A verification that reached zero failures while
    losing a NON-target check fails that rule, so
    `deflake_diagnosis.evaluate` routes it here — and a consumer that
    checked only the targets would call it a passing verification and
    record nothing at all.
    """
    verification = non_target_missing_verification()
    expect(bool(dd.missing_problems(
        verification, targets={"beta", "gamma"}, what="the verification")),
        "the fixture must actually violate #1437's MISSING rule")
    expect(not dd.missing_targets(verification, ["beta", "gamma"]),
           "...while leaving every TARGET emitted, or it would be refused "
           "by the clause this case exists to look past")

    diagnosis = route_diagnosis(dd.ROUTE_PARTIAL_IMPROVEMENT)
    diagnosis["verification"]["result"] = verification
    record = evaluate(diagnosis).to_document()
    expect(record["reason"] == dd.REASON_VERIFICATION_MISSING_RULE,
           f"#1437 routes it here for the MISSING rule; got "
           f"{record['reason']}")

    with census_file() as path:
        recorded, publisher = record_outcome(outcome_handoff(
            dd.ROUTE_PARTIAL_IMPROVEMENT, diagnosis_outcome=record,
            measurements=[
                {"role": do.ROLE_BASELINE, "exit_code": probe_flake.EXIT_OK,
                 "result": diagnosis["baseline"]["result"]},
                {"role": do.ROLE_VERIFICATION,
                 "exit_code": probe_flake.EXIT_OK,
                 "result": verification}]), path)
        expect(recorded.record["outcome"] == do.OUTCOME_PARTIAL_IMPROVEMENT,
               f"a 4 -> 0 verification that lost a non-target check is still "
               f"a partial improvement; got {recorded.record['outcome']}")
        expect(recorded.record["comparison"]["verification_failure_count"]
               == 0 and recorded.record["comparison"]["unmet_condition"]
               == dd.REASON_VERIFICATION_MISSING_RULE,
               "recorded with the zero failures it really observed and the "
               "condition that actually stayed unmet")
        expect(publisher.calls == [], "and opening no pull request")


def test_two_concurrent_invocations_of_one_attempt_append_once() -> None:
    """The stamp is chosen inside the lock, so a race is still a resume.

    Two processes starting on the same NEW attempt both build a record
    before either has committed. Whichever loses the census lock finds
    the winner's record already stored and rebuilds against it, so the
    pair ends in one append and two successes rather than a conflict.
    """
    tool = str(Path(__file__).resolve().parent / "deflake_outcome.py")
    with census_file() as path:
        document = Path(path).parent / "concurrent.json"
        document.write_text(json.dumps(outcome_handoff()), encoding="utf-8")
        command = [sys.executable, tool, "--handoff", str(document),
                   "--census", str(path)]
        running = [subprocess.Popen(command, stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE, text=True)
                   for _ in range(2)]
        finished = [process.communicate(timeout=180) for process in running]
        codes = [process.returncode for process in running]
        expect(codes == [do.EXIT_OK, do.EXIT_OK],
               f"both concurrent invocations succeed; got {codes} "
               f"({[err.strip()[:160] for _out, err in finished]})")
        stored = stored_outcomes(path)
        expect(len(stored) == 1,
               f"and exactly one outcome is appended; got {len(stored)}")


def test_a_malformed_stored_census_is_an_actionable_non_success() -> None:
    """No traceback, whatever is sitting in the file.

    The stored census is read and validated inside the locked
    transaction, so a valid-JSON document that is not a census reaches a
    controlled refusal rather than an attribute error on the way to one.
    """
    tool = str(Path(__file__).resolve().parent / "deflake_outcome.py")
    for label, build in (
            ("a census that is a JSON array", lambda _document: []),
            ("a row whose census record is a list",
             lambda document: _row_census_as_list(document)),
            ("a census on an older schema", lambda document: dict(
                document, schema=probe_census.CLAIM_SCHEMA)),
    ):
        with census_file() as path:
            stored = json.loads(Path(path).read_text(encoding="utf-8"))
            Path(path).write_text(json.dumps(build(stored)), encoding="utf-8")
            before = Path(path).read_bytes()
            handoff = Path(path).parent / "handoff.json"
            handoff.write_text(json.dumps(outcome_handoff()), encoding="utf-8")
            done = subprocess.run(
                [sys.executable, tool, "--handoff", str(handoff),
                 "--census", str(path)],
                capture_output=True, text=True, timeout=120)
            expect(done.returncode == do.EXIT_NON_SUCCESS,
                   f"{label}: exits with an actionable non-success; got "
                   f"{done.returncode} ({done.stderr.strip()[:200]})")
            expect("Traceback" not in done.stderr,
                   f"{label}: printed a traceback\n{done.stderr[:400]}")
            expect(Path(path).read_bytes() == before,
                   f"{label}: the refusal changed the census bytes")


def _row_census_as_list(document: dict) -> dict:
    document = copy.deepcopy(document)
    for row in document["probes"]:
        if row["key"] == PROBE:
            row["census"] = []
    return document


def test_partial_improvement_is_numeric_on_both_halves() -> None:
    """An improvement, and a gate it still fails. Neither is an adjective."""
    diagnosis, _record = produced(dd.ROUTE_PARTIAL_IMPROVEMENT)
    baseline = diagnosis["baseline"]["result"]

    def handoff(verification, **kwargs):
        return outcome_handoff(dd.ROUTE_PARTIAL_IMPROVEMENT, measurements=[
            {"role": do.ROLE_BASELINE, "exit_code": probe_flake.EXIT_OK,
             "result": baseline},
            {"role": do.ROLE_VERIFICATION, "exit_code": probe_flake.EXIT_OK,
             "result": verification}], **kwargs)

    cases = (
        ("a verification no better than the baseline",
         handoff(verification_result(runs=failing_runs(4, abort=False),
                                     artifact_root=VERIFY_ARTIFACTS)),
         "which is no improvement"),
        ("a verification that measurably passes the gate",
         handoff(verification_result(artifact_root=VERIFY_ARTIFACTS)),
         "the verification observed 0 failure(s)"),
        ("a verification at another run count",
         handoff(verification_result(
             requested=dd.RUN_COUNT - 4, artifact_root=VERIFY_ARTIFACTS,
             runs=failing_runs(2, abort=False)[:dd.RUN_COUNT - 4])),
         "requested"),
        ("a verification at another capability count",
         handoff(verification_result(rts_caps=dd.RTS_CAPABILITIES + 1,
                                     runs=failing_runs(2, abort=False),
                                     artifact_root=VERIFY_ARTIFACTS)),
         "RTS capabilities"),
        ("an invalid verification",
         handoff(verification_result(runs=failing_runs(2, abort=False),
                                     harness_error=True,
                                     artifact_root=VERIFY_ARTIFACTS),
                 ),
         "exited 0 while its document reports status"),
    )
    for label, document, fragment in cases:
        with census_file() as path:
            before = path.read_bytes()
            publisher = Publisher()
            try:
                do.require_handoff(copy.deepcopy(document),
                                   worktrees=WORKTREES, primary=PRIMARY_WT)
            except do.HandoffError as error:
                expect(fragment in str(error),
                       f"{label}: rejected, but for {str(error)!r} rather "
                       f"than {fragment!r}")
                expect_nothing_recorded(path, before, publisher, label)
                continue
            expect_non_success(
                lambda d=document, p=publisher: record_outcome(
                    d, path, publisher=p),
                fragment, f"{label} recorded as partial-improvement")
            expect_nothing_recorded(path, before, publisher, label)


def test_an_untrustworthy_verification_is_not_an_improvement() -> None:
    """A batch that became invalid improved nothing measurable.

    It is the operational-error route, and specifically NOT a partial
    improvement claimed over evidence that cannot establish one.
    """
    diagnosis, _record = produced(dd.ROUTE_PARTIAL_IMPROVEMENT)
    document = outcome_handoff(dd.ROUTE_PARTIAL_IMPROVEMENT, measurements=[
        {"role": do.ROLE_BASELINE, "exit_code": probe_flake.EXIT_OK,
         "result": diagnosis["baseline"]["result"]},
        {"role": do.ROLE_VERIFICATION,
         "exit_code": probe_flake.EXIT_HARNESS_ERROR,
         "result": verification_result(runs=failing_runs(2, abort=False),
                                       harness_error=True,
                                       artifact_root=VERIFY_ARTIFACTS)}])
    with census_file() as path:
        before = path.read_bytes()
        publisher = Publisher()
        expect_non_success(
            lambda: record_outcome(document, path, publisher=publisher),
            "exited 4",
            "an invalid verification recorded as partial-improvement")
        expect_nothing_recorded(path, before, publisher,
                                "an invalid verification")


def test_a_production_defect_is_an_actionable_non_success_naming_1438()\
        -> None:
    """#1438 is a sibling, not a prerequisite: nothing here stubs it."""
    with census_file() as path:
        before = path.read_bytes()
        publisher = Publisher()
        expect_non_success(
            lambda: record_outcome(
                outcome_handoff(dd.ROUTE_PRODUCTION_DEFECT), path,
                publisher=publisher),
            "#1438", "a production defect recorded as a stable outcome")
        expect_nothing_recorded(path, before, publisher, "a production defect")


def test_no_stable_outcome_reaches_the_pull_request_publisher() -> None:
    """The boundary, from both sides.

    The table is consulted on every route, so the silence below is a
    branch that really ran — flipping an entry makes the injected
    publisher fire, which is what stops this assertion being vacuous.
    """
    expect(set(do.OPENS_PULL_REQUEST) == set(do.STABLE_OUTCOMES)
           and not any(do.OPENS_PULL_REQUEST.values()),
           f"every stable outcome forbids a repair PR; got "
           f"{do.OPENS_PULL_REQUEST}")
    routes = (dd.ROUTE_NO_TARGET, dd.ROUTE_CANNOT_REPRODUCE,
              dd.ROUTE_NO_CONFIDENT_FIX, dd.ROUTE_PARTIAL_IMPROVEMENT)
    for route in routes:
        with census_file() as path:
            recorded, publisher = record_outcome(outcome_handoff(route), path)
            expect(publisher.calls == [],
                   f"the {route!r} route called the publisher")
            expect(recorded.record["outcome"] in do.STABLE_OUTCOMES
                   and recorded.to_document()["opened_pull_request"] is False,
                   f"the {route!r} route recorded "
                   f"{recorded.record['outcome']!r}, not a stable outcome "
                   f"reported as opening nothing")

    # The branch is live: with the policy flipped, the publisher fires.
    saved = dict(do.OPENS_PULL_REQUEST)
    do.OPENS_PULL_REQUEST[do.OUTCOME_CANNOT_REPRODUCE] = True
    try:
        with census_file() as path:
            recorded, publisher = record_outcome(outcome_handoff(), path)
            expect(len(publisher.calls) == 1
                   and recorded.to_document()["opened_pull_request"] is True,
                   "the publisher boundary is consulted rather than absent, "
                   "so `never called` is an observed fact — and the report "
                   "says what the boundary DID, not what the policy says")
    finally:
        do.OPENS_PULL_REQUEST.clear()
        do.OPENS_PULL_REQUEST.update(saved)

    # And the default publisher refuses rather than quietly succeeding.
    try:
        do.forbidden_publisher(do.OUTCOME_CANNOT_REPRODUCE)
    except do.NonSuccess:
        pass
    else:
        FAILURES.append("the default publisher accepted a call")


def test_a_retry_under_a_different_clock_is_still_a_resume() -> None:
    """The one field a rebuilt record cannot derive from the handoff.

    Every other field comes from the handoff, but `timestamp_utc` comes
    from a clock — and the command line reads it afresh on every
    invocation. Idempotency is the WHOLE record, so a retry that stamped
    itself anew would present one attempt identity carrying two
    different records and be refused as a conflict instead of
    recognized as the resume it is.
    """
    with census_file() as path:
        first, _publisher = record_outcome(outcome_handoff(), path,
                                           now=OUTCOME_NOW)
        after_first = path.read_bytes()
        later = "2026-08-29T04:05:06Z"
        expect(later != OUTCOME_NOW, "the retry must read a different clock")
        second, publisher = record_outcome(outcome_handoff(), path, now=later)
        expect(second.resumed and second.record == first.record,
               f"a retry under a later clock is a resume of the stored "
               f"record; got resumed={second.resumed}, stamp="
               f"{second.record['timestamp_utc']}")
        expect(second.record["timestamp_utc"] == OUTCOME_NOW,
               "and it keeps the instant the attempt was first stamped with")
        expect(path.read_bytes() == after_first and
               len(stored_outcomes(path)) == 1,
               "so the census is untouched and holds one outcome")
        expect(publisher.calls == [], "and nothing was published")

    # The shipped entry point, which reads the real clock twice.
    with census_file() as path:
        tool = str(Path(__file__).resolve().parent / "deflake_outcome.py")
        document = Path(path).parent / "retry.json"
        document.write_text(json.dumps(outcome_handoff()), encoding="utf-8")
        codes = []
        for _attempt in range(2):
            done = subprocess.run(
                [sys.executable, tool, "--handoff", str(document),
                 "--census", str(path)],
                capture_output=True, text=True, timeout=120)
            codes.append(done.returncode)
        expect(codes == [do.EXIT_OK, do.EXIT_OK],
               f"running the command twice over one handoff succeeds both "
               f"times; got {codes}")
        expect(len(stored_outcomes(path)) == 1,
               f"and appends exactly one outcome; got "
               f"{len(stored_outcomes(path))}")


def test_resuming_a_recorded_attempt_appends_nothing() -> None:
    with census_file() as path:
        first, _publisher = record_outcome(outcome_handoff(), path)
        after_first = path.read_bytes()
        stamp = path.stat()
        second, publisher = record_outcome(outcome_handoff(), path)
        expect(path.read_bytes() == after_first,
               "resuming a recorded attempt rewrote the census")
        expect(len(stored_outcomes(path)) == 1,
               f"resuming appended a duplicate; got "
               f"{len(stored_outcomes(path))} outcomes")
        expect(second.resumed and second.record == first.record,
               "and the resume reports the record already stored")
        expect(publisher.calls == [], "and publishes nothing on the way")
        after = path.stat()
        expect((after.st_ino, after.st_mtime_ns)
               == (stamp.st_ino, stamp.st_mtime_ns),
               "a resume does not even rewrite the file")

        # The same identity carrying different evidence is not a resume.
        conflicting = outcome_handoff(dd.ROUTE_NO_CONFIDENT_FIX)
        publisher = Publisher()
        expect_non_success(
            lambda: record_outcome(conflicting, path, publisher=publisher),
            "already recorded with different evidence",
            "one attempt identity carrying two outcomes")
        expect(path.read_bytes() == after_first,
               "a conflicting resume changed the census bytes")
        expect(publisher.calls == [],
               "a conflicting resume reached the publisher")


def test_a_census_write_failure_leaves_the_attempt_resumable() -> None:
    """Byte-identical afterwards, nothing recorded, nothing published."""
    with census_file() as path:
        before = path.read_bytes()
        publisher = Publisher()
        original = census_storage._atomic_replace

        def refuse(*args, **kwargs):
            raise probe_census.CensusError("injected: the disk is full")

        census_storage._atomic_replace = refuse
        try:
            expect_non_success(
                lambda: record_outcome(outcome_handoff(), path,
                                       publisher=publisher),
                "the census refused",
                "a census write failure recorded an outcome anyway")
        finally:
            census_storage._atomic_replace = original
        expect_nothing_recorded(path, before, publisher,
                                "a refused census write")

        # Resumable: the same attempt records once the cause is gone.
        recorded, publisher = record_outcome(outcome_handoff(), path)
        expect(recorded.record["outcome"] == do.OUTCOME_CANNOT_REPRODUCE
               and len(stored_outcomes(path)) == 1,
               "the attempt could not be resumed after the write failure")
        expect(not recorded.resumed,
               "and the retry is a real append, not a reported resume")


def test_every_declared_measurement_has_to_be_trustworthy() -> None:
    """Not only the one the route is judged on.

    A stable outcome is stored beside every measurement the attempt
    declared and rests on all of them, so one untrustworthy batch makes
    the whole attempt an operational error rather than a stable outcome
    with a hole in its evidence.
    """
    diagnosis, _record = produced(dd.ROUTE_NO_CONFIDENT_FIX)
    baseline = {"role": do.ROLE_BASELINE, "exit_code": probe_flake.EXIT_OK,
                "result": diagnosis["baseline"]["result"]}
    handoff_measurement = diagnosis["handoff"]["result"]

    with census_file() as path:
        before = path.read_bytes()
        publisher = Publisher()
        expect_non_success(
            lambda: record_outcome(outcome_handoff(
                dd.ROUTE_NO_CONFIDENT_FIX,
                measurements=[baseline,
                              {"role": do.ROLE_HANDOFF,
                               "exit_code": probe_flake.EXIT_NO_PORT,
                               "result": None}]),
                path, publisher=publisher),
            "exited 3",
            "an untrustworthy measurement beside a usable baseline")
        expect_nothing_recorded(path, before, publisher,
                                "an untrustworthy extra measurement")

    # A trustworthy extra measurement is recorded, in role order.
    with census_file() as path:
        recorded, _publisher = record_outcome(outcome_handoff(
            dd.ROUTE_NO_CONFIDENT_FIX,
            measurements=[baseline,
                          {"role": do.ROLE_HANDOFF,
                           "exit_code": probe_flake.EXIT_OK,
                           "result": handoff_measurement}]),
            path)
        expect([m["role"] for m in recorded.record["measurements"]]
               == [do.ROLE_HANDOFF, do.ROLE_BASELINE],
               f"every declared measurement is stored, in role order; got "
               f"{[m['role'] for m in recorded.record['measurements']]}")


def test_a_durability_warning_is_not_a_refusal() -> None:
    """The one failure that happens AFTER the record is installed.

    `probe_census.update` raises it only after the replacement, so the
    outcome is already what a later reader parses; treating it as a
    refusal and re-recording would be the duplicate the attempt identity
    exists to prevent.
    """
    with census_file() as path:
        original = census_storage._atomic_replace

        def unconfirmed(target, payload, **kwargs):
            original(target, payload, **kwargs)
            raise probe_census.CensusDurabilityUnconfirmed(
                "injected: the directory fsync failed", target=target,
                error=OSError("injected"))

        census_storage._atomic_replace = unconfirmed
        try:
            recorded, publisher = record_outcome(outcome_handoff(), path)
        finally:
            census_storage._atomic_replace = original
        expect(recorded.record["outcome"] == do.OUTCOME_CANNOT_REPRODUCE
               and not recorded.durable,
               "an unconfirmed durability is reported, not raised as a "
               "refusal")
        expect(len(stored_outcomes(path)) == 1,
               "the record is installed exactly once despite the warning")
        expect(publisher.calls == [], "and nothing was published")

        again, _publisher = record_outcome(outcome_handoff(), path)
        expect(again.resumed and len(stored_outcomes(path)) == 1,
               "and a later resume finds it rather than appending a second")


def test_an_outcome_append_preserves_every_prior_record() -> None:
    """Measurement history, claim log, policy and unrelated rows."""
    with census_file() as path:
        # Real accumulated state on the row about to be written, and on
        # one that must not move at all.
        probe_census.record_result(path, result_document(
            runs=failing_runs(3), commit=BASE_COMMIT))
        probe_census.record_claim(path, PROBE, {
            "token": "claim-1", "timestamp_utc": "2026-08-27T09:00:00Z",
            "commit_sha": BASE_COMMIT, "owner": "deflake", "host": "here",
            "pid": 4711, "lease_seconds": 3600.0,
            "requested_runs": dd.RUN_COUNT})
        probe_census.record_policy(path, OTHER, acceptable_failures=1,
                                   justification="a known race")
        before = json.loads(path.read_text(encoding="utf-8"))

        record_outcome(outcome_handoff(), path)
        after = json.loads(path.read_text(encoding="utf-8"))

        expect([row["key"] for row in after["probes"]]
               == [row["key"] for row in before["probes"]],
               "an outcome append changed the inventory order or membership")
        for was, now in zip(before["probes"], after["probes"]):
            if was["key"] == PROBE:
                continue
            expect(was == now,
                   f"an outcome append modified the unrelated row "
                   f"{was['key']!r}")
        was = probe_census.find_entry(before, PROBE)["census"]
        now = probe_census.find_entry(after, PROBE)["census"]
        for field in ("current", "history", "attempts", "claims",
                      "acceptable_failures",
                      "acceptable_failures_justification",
                      "estimated_worst_case_seconds"):
            expect(was[field] == now[field],
                   f"an outcome append changed `{field}`, which it may not "
                   f"touch")
        expect(now["current"]["samples"] and now["attempts"] and now["claims"],
               "the fixture must carry real prior state for that to mean "
               "anything")

        # A second, different attempt appends after the first.
        record_outcome(outcome_handoff(dd.ROUTE_NO_CONFIDENT_FIX, attempt=
                                       "role-20260828T130000Z-4711-beefcafe"),
                       path)
        stored = stored_outcomes(path)
        expect([entry["attempt"] for entry in stored]
               == [ATTEMPT, "role-20260828T130000Z-4711-beefcafe"],
               f"outcome history is append-only and ordered; got "
               f"{[entry['attempt'] for entry in stored]}")


def test_recording_an_outcome_changes_no_registry_classification() -> None:
    """The de-list recommendation is advisory, and only that.

    `MANUAL_ONLY_REASONS` models a probe's grounds as several
    INDEPENDENT `Reason` records, so a comparison that looked only at
    the set of keys would miss exactly the regression the advisory-only
    rule exists to prevent: one non-flaky reason quietly dropped.
    """
    def snapshot():
        return (sorted(ci_probes.CI_ELIGIBLE),
                {key: tuple((reason.category, reason.explanation)
                            for reason in reasons)
                 for key, reasons in ci_probes.MANUAL_ONLY_REASONS.items()})

    before = snapshot()
    expect(any(len(reasons) > 1
               for reasons in ci_probes.MANUAL_ONLY_REASONS.values()),
           "the registry must carry a multi-reason probe for this "
           "comparison to be able to see a dropped one")
    with census_file() as path:
        row_before = probe_census.find_entry(
            json.loads(path.read_text(encoding="utf-8")), PROBE)
        record_outcome(outcome_handoff(), path)
        row_after = probe_census.find_entry(
            json.loads(path.read_text(encoding="utf-8")), PROBE)
        for field in ("key", "script", "classification", "protocol"):
            expect(row_before[field] == row_after[field],
                   f"an outcome append changed the inventory field "
                   f"`{field}`")
    expect(snapshot() == before,
           "recording an outcome changed tools/ci_probes.py's classification "
           "or a probe's independent manual-only reasons")


def test_the_census_stores_references_and_never_raw_evidence() -> None:
    """Paths out, streams left where the harness put them."""
    with census_file() as path:
        recorded, _publisher = record_outcome(
            outcome_handoff(dd.ROUTE_PARTIAL_IMPROVEMENT), path)
        record = recorded.record
        trees = [Path(tree) for tree in
                 (CLEAN_WT, REPAIR_WT, PRIMARY_WT, str(Path(path).parent))]
        expect(record["retained_artifacts"],
               "the outcome names the artifacts the attempt retained")
        for entry in record["retained_artifacts"]:
            candidate = Path(entry)
            expect(candidate.is_absolute(),
                   f"a retained artifact reference is relative: {entry}")
            for tree in trees:
                expect(not (candidate == tree or tree in candidate.parents),
                       f"the retained artifact {entry} sits inside {tree}")
        for measurement in record["measurements"]:
            expect(set(measurement) == {
                "role", "exit_code", "status", "commit_sha", "timestamp_utc",
                "requested_runs", "completed_runs", "runs", "check_counts",
                "failure_count", "failure_rate", "timeout_count",
                "rts_capabilities", "error", "error_run_index",
                "retained_artifacts", "census_reference"},
                f"a stored measurement summary carries "
                f"{sorted(measurement)}, which is not the declared set")
            expect(all(set(run) == {"index", "outcome"}
                       for run in measurement["runs"]),
                   "a stored run keeps its outcome, not its check map")
            expect(measurement["census_reference"] == {
                "cohort_commit_sha": measurement["commit_sha"],
                "sample_timestamp_utc": measurement["timestamp_utc"]},
                "a measurement is addressed in the census by its cohort "
                "commit and sample timestamp")


def test_a_failure_whose_logs_are_gone_is_not_diagnostic_evidence() -> None:
    """The retention pairing, which the census validator does not check.

    `probe_flake.measure` deletes a passing run's directory and retains
    every unsuccessful one, so a non-PASS run with a null `artifact_dir`
    and an empty `retained_artifacts` is a shape no harness wrote — and
    `probe_census.validate_result` permits it. A `no-confident-fix`
    recorded from one stores a diagnosis whose evidence nobody can open.
    """
    for label, route in (("no-confident-fix", dd.ROUTE_NO_CONFIDENT_FIX),
                         ("cannot-reproduce", dd.ROUTE_CANNOT_REPRODUCE)):
        with census_file() as path:
            before = path.read_bytes()
            publisher = Publisher()
            expect_handoff_rejected(
                lambda r=route, p=publisher: record_outcome(
                    outcome_handoff(r, measurements=[
                        {"role": do.ROLE_BASELINE,
                         "exit_code": probe_flake.EXIT_OK,
                         "result": discarded_failure_result()}]),
                    path, publisher=p),
                "did not pass",
                f"a {label} whose failing runs kept no logs")
            expect_nothing_recorded(path, before, publisher, label)


def test_failures_somewhere_else_are_not_a_reproduction() -> None:
    """#1437 asks for a TARGET hit, and asks it on every route past the fork.

    A batch over X whose failures are confined to unrelated checks
    demonstrates nothing about the checks under diagnosis, and
    `deflake_diagnosis.evaluate` routes it to `cannot-reproduce` for
    exactly that reason. A consumer that asked only the aggregate would
    persist a `no-confident-fix` its own producer refuses.
    """
    elsewhere = elsewhere_failure_result()
    expect(not [cid for cid in ("beta", "gamma")
                if cid in dd.non_pass_ids(elsewhere)],
           "the fixture must miss every target, or it proves nothing")
    expect(dd.failure_count(elsewhere) > 0,
           "...while still being over an X of zero")

    with census_file() as path:
        before = path.read_bytes()
        publisher = Publisher()
        expect_non_success(
            lambda: record_outcome(outcome_handoff(
                dd.ROUTE_NO_CONFIDENT_FIX, measurements=[
                    {"role": do.ROLE_BASELINE,
                     "exit_code": probe_flake.EXIT_OK,
                     "result": elsewhere}]), path, publisher=publisher),
            "did not reproduce the pattern",
            "failures confined to unrelated checks as no-confident-fix")
        expect_nothing_recorded(path, before, publisher,
                                "a baseline that missed every target")

    # The same qualification guards the comparison route.
    diagnosis, _record = produced(dd.ROUTE_PARTIAL_IMPROVEMENT)
    with census_file() as path:
        before = path.read_bytes()
        publisher = Publisher()
        expect_non_success(
            lambda: record_outcome(outcome_handoff(
                dd.ROUTE_PARTIAL_IMPROVEMENT, measurements=[
                    {"role": do.ROLE_BASELINE,
                     "exit_code": probe_flake.EXIT_OK,
                     "result": elsewhere},
                    {"role": do.ROLE_VERIFICATION,
                     "exit_code": probe_flake.EXIT_OK,
                     "result": diagnosis["verification"]["result"]}]),
                path, publisher=publisher),
            "did not reproduce the pattern",
            "a partial improvement over a baseline that missed every target")
        expect_nothing_recorded(path, before, publisher,
                                "an improvement on nothing")


def test_a_path_no_filesystem_can_name_is_never_stored() -> None:
    """Absolute is not usable.

    An embedded NUL makes `Path.resolve()` raise `ValueError` from
    `lstat` rather than `OSError`, so such a string sails past an
    absoluteness test, names no location for the worktree-containment
    check to find, and would be stored in the census as an artifact
    reference or an invocation directory. Every recorded path this
    module keeps goes through #1437's own `require_path`, including the
    ones it never resolves — a command token and a manifest entry are
    evidence too, and evidence naming nothing is not evidence.
    """
    nul = "/tmp/synarchy-deflake-evidence/artifacts\x00/run-001"
    cases = (
        ("a retained artifact", lambda d: d["diagnosis_outcome"].__setitem__(
            "retained_artifacts", [nul])),
        ("a batch reference's artifact root",
         lambda d: d["diagnosis_outcome"]["baseline"].__setitem__(
             "artifact_root", nul)),
        ("a batch reference's invocation directory",
         lambda d: d["diagnosis_outcome"]["baseline"].__setitem__(
             "invocation_dir", nul)),
        ("a measurement's own artifact root",
         lambda d: d["measurements"][0]["result"].__setitem__(
             "artifact_root", nul)),
        ("the /deflake invocation's directory",
         lambda d: d["diagnosis_outcome"]["handoff"].__setitem__(
             "directory", nul)),
        ("a /deflake command token",
         lambda d: d["diagnosis_outcome"]["handoff"]["command"].__setitem__(
             1, nul)),
        ("a declared comparison worktree",
         lambda d: d["diagnosis_outcome"]["baseline"].__setitem__(
             "worktree", nul)),
        ("a configuration entry's path",
         lambda d: d["diagnosis_outcome"]["configuration"]["entries"].append(
             {"path": "config/save\x00.local.yaml", "sha256": "c" * 64})),
    )
    for label, mutate in cases:
        with census_file() as path:
            before = path.read_bytes()
            document = outcome_handoff()
            mutate(document)
            publisher = Publisher()
            expect_handoff_rejected(
                lambda d=document, p=publisher: record_outcome(
                    d, path, publisher=p),
                "which contains a NUL",
                f"{label} that no filesystem can name")
            expect_nothing_recorded(path, before, publisher, label)

    # And through the shipped entry point, which is where the traceback
    # this refuses would actually have been printed.
    tool = str(Path(__file__).resolve().parent / "deflake_outcome.py")
    with census_file() as path:
        document = outcome_handoff()
        document["diagnosis_outcome"]["retained_artifacts"] = [nul]
        handoff = Path(path).parent / "nul.json"
        handoff.write_text(json.dumps(document), encoding="utf-8")
        before = Path(path).read_bytes()
        done = subprocess.run(
            [sys.executable, tool, "--handoff", str(handoff),
             "--census", str(path)],
            capture_output=True, text=True, timeout=120)
        expect(done.returncode == do.EXIT_REJECTED
               and "Traceback" not in done.stderr,
               f"the CLI rejects it rather than crashing; got "
               f"{done.returncode}\n{done.stderr[:300]}")
        expect(Path(path).read_bytes() == before,
               "and writes nothing on the way out")


def test_one_attempt_reports_against_one_declared_contract() -> None:
    """The descriptor `_bind_to_producer` cannot see.

    A result can keep its probe, its targets, its commit, its instant
    and every artifact path while swapping or relabelling an unrelated
    declared check — nothing the identity binding compares moves — and
    #1437 REJECTS that drift rather than routing it anywhere. The
    producer record restates only the identifiers and their order, so
    labels are held between the declared measurements instead.
    """
    for label, result, fragment in (
        ("a renamed check", redeclared_result(rename="delta"),
         "identifiers and their order are the stable contract"),
        ("a dropped check", redeclared_result(drop=True),
         "identifiers and their order are the stable contract"),
        ("a reordered descriptor", redeclared_result(reorder=True),
         "identifiers and their order are the stable contract"),
    ):
        with census_file() as path:
            before = path.read_bytes()
            publisher = Publisher()
            expect_handoff_rejected(
                lambda r=result, p=publisher: record_outcome(
                    outcome_handoff(dd.ROUTE_CANNOT_REPRODUCE, measurements=[
                        {"role": do.ROLE_BASELINE,
                         "exit_code": probe_flake.EXIT_OK, "result": r}]),
                    path, publisher=p),
                fragment, label)
            expect_nothing_recorded(path, before, publisher, label)

    # A relabel keeps every identifier, so nothing the identity binding
    # compares moves. It is held to the RECORD's own descriptor and not
    # to a sibling measurement, because the routes that carry ONE
    # baseline have no sibling — and those are the routes that record a
    # de-list recommendation.
    relabelled = redeclared_result(relabel="a different assertion entirely")
    for label, route, measurements in (
        ("a single-baseline cannot-reproduce", dd.ROUTE_CANNOT_REPRODUCE,
         [{"role": do.ROLE_BASELINE, "exit_code": probe_flake.EXIT_OK,
           "result": relabelled}]),
        ("a single-baseline no-confident-fix", dd.ROUTE_NO_CONFIDENT_FIX,
         [{"role": do.ROLE_BASELINE, "exit_code": probe_flake.EXIT_OK,
           "result": relabelled}]),
        ("a relabelled verification", dd.ROUTE_PARTIAL_IMPROVEMENT,
         [{"role": do.ROLE_BASELINE, "exit_code": probe_flake.EXIT_OK,
           "result": produced(dd.ROUTE_PARTIAL_IMPROVEMENT)[0]["baseline"][
               "result"]},
          {"role": do.ROLE_VERIFICATION, "exit_code": probe_flake.EXIT_OK,
           "result": redeclared_result(
               relabel="a different assertion entirely",
               commit=REPAIR_COMMIT, artifact_root=VERIFY_ARTIFACTS)}]),
    ):
        with census_file() as path:
            before = path.read_bytes()
            publisher = Publisher()
            expect_handoff_rejected(
                lambda r=route, m=measurements, p=publisher: record_outcome(
                    outcome_handoff(r, measurements=m), path, publisher=p),
                "relabels", f"{label} whose check was relabelled")
            expect_nothing_recorded(path, before, publisher, label)

    # The record states the descriptor twice; the two must agree.
    with census_file() as path:
        before = path.read_bytes()
        _diagnosis, record = produced(dd.ROUTE_CANNOT_REPRODUCE)
        record["handoff"]["expected_descriptor"][-1]["id"] = "delta"
        publisher = Publisher()
        expect_handoff_rejected(
            lambda: record_outcome(
                outcome_handoff(dd.ROUTE_CANNOT_REPRODUCE,
                                diagnosis_outcome=record),
                path, publisher=publisher),
            "one descriptor stated twice cannot be two descriptors",
            "a record whose two descriptors disagree")
        expect_nothing_recorded(path, before, publisher,
                                "two disagreeing descriptors")

    # And a target the record's own descriptor never declared.
    with census_file() as path:
        before = path.read_bytes()
        _diagnosis, record = produced(dd.ROUTE_NO_CONFIDENT_FIX)
        record["targets"] = record["targets"] + ["delta"]
        record["handoff"]["targets"] = list(record["targets"])
        publisher = Publisher()
        expect_handoff_rejected(
            lambda: record_outcome(
                outcome_handoff(dd.ROUTE_NO_CONFIDENT_FIX,
                                diagnosis_outcome=record),
                path, publisher=publisher),
            "does not declare",
            "a target the expected descriptor never declared")
        expect_nothing_recorded(path, before, publisher, "an undeclared target")


def test_the_declared_boundary_is_a_place_not_a_label() -> None:
    """A boundary compared in one spelling bounds nothing in another.

    The declared worktrees are what the containment check is made
    AGAINST once the real ones have been removed, so a relative,
    `..`-bearing or otherwise non-canonical spelling would compare as
    somewhere other than the place it names and the boundary would
    quietly stop covering it. A section that declared none at all would
    contribute no boundary while looking like a record that had one.
    """
    cases = (
        ("a batch that declares no worktree",
         lambda d: d["diagnosis_outcome"]["baseline"].pop("worktree"),
         "must be a non-empty string"),
        ("a relative boundary",
         lambda d: d["diagnosis_outcome"]["baseline"].__setitem__(
             "worktree", "deflake-clean-role"),
         "is the relative path"),
        ("a boundary spelled through a traversal",
         lambda d: d["diagnosis_outcome"]["baseline"].__setitem__(
             "worktree", f"{CLEAN_WT}/../{Path(CLEAN_WT).name}"),
         "not the spelling `Path.resolve` produces"),
        ("two labels for one state",
         lambda d: d["diagnosis_outcome"]["verification"].__setitem__(
             "worktree", CLEAN_WT),
         "not two separate states"),
        ("a boundary nested inside the other",
         lambda d: d["diagnosis_outcome"]["verification"].__setitem__(
             "worktree", f"{CLEAN_WT}/repair"),
         "not two separate states"),
    )
    for label, mutate, fragment in cases:
        route = (dd.ROUTE_PARTIAL_IMPROVEMENT
                 if "other" in label or "one state" in label
                 else dd.ROUTE_CANNOT_REPRODUCE)
        with census_file() as path:
            before = path.read_bytes()
            document = outcome_handoff(route)
            mutate(document)
            publisher = Publisher()
            expect_handoff_rejected(
                lambda d=document, p=publisher: record_outcome(
                    d, path, publisher=p),
                fragment, label)
            expect_nothing_recorded(path, before, publisher, label)


def test_a_removed_comparison_worktree_still_bounds_the_artifacts() -> None:
    """The paths a record names outlive the worktrees they were inside.

    `/deflake` removes or hands off both comparison worktrees when it
    finishes, so by the time an outcome is recorded neither is
    REGISTERED any more — and an artifact that sat inside one was still
    inside a worktree when it was written. The record's own declared
    worktrees are therefore collected beside the live ones, which is the
    only thing standing between a removed worktree's artifacts and the
    census.
    """
    document = outcome_handoff()
    document["diagnosis_outcome"]["retained_artifacts"] = [
        f"{CLEAN_WT}/artifacts/run-001"]
    expect(document["diagnosis_outcome"]["baseline"]["worktree"] == CLEAN_WT,
           "the record must declare the worktree, or this proves nothing")
    try:
        do.require_handoff(copy.deepcopy(document), worktrees=(),
                           primary=None)
    except do.HandoffError as error:
        expect("inside the worktree" in str(error),
               f"a removed worktree's artifacts are still refused; got "
               f"{error}")
    else:
        FAILURES.append("an artifact inside a no-longer-registered "
                        "comparison worktree was accepted")


def test_the_record_keeps_the_condition_and_the_command() -> None:
    """Two inputs no census field has ever held.

    `probe_census.ingest_result` drops the exact command and the
    invocation directory, and nothing in the census has ever stored the
    configuration manifest at all — so a record that did not keep them
    could not say what was run, or under what state, which is the first
    thing anyone resuming an attempt needs.
    """
    entries = [("config/save.local.yaml", "a" * 64),
               ("config/video.local.yaml", "b" * 64)]
    handoff = handoff_document(config=config_entries(entries))
    diagnosis = diagnosis_document(handoff=handoff)
    diagnosis["route"] = dd.ROUTE_CANNOT_REPRODUCE
    diagnosis.pop("attestations", None)
    diagnosis.pop("repair", None)
    diagnosis.pop("verification", None)
    diagnosis["baseline"]["result"] = spotless_result()
    diagnosis["baseline"]["configuration"] = manifest(entries)
    record = evaluate(diagnosis).to_document()

    with census_file() as path:
        recorded, _publisher = record_outcome(outcome_handoff(
            dd.ROUTE_CANNOT_REPRODUCE, diagnosis_outcome=record,
            measurements=[
                {"role": do.ROLE_BASELINE, "exit_code": probe_flake.EXIT_OK,
                 "result": diagnosis["baseline"]["result"]}]), path)
        stored = stored_outcomes(path)[0]
        expect(stored["configuration"] == [
            {"path": path_, "sha256": digest} for path_, digest in entries],
            f"the manifest the batches read is stored entry for entry; got "
            f"{stored['configuration']}")
        expect(stored["invocation"]["command"]
               == list(record["handoff"]["command"])
               and stored["invocation"]["directory"] == PRIMARY_WT,
               f"and the exact command and directory of the /deflake "
               f"invocation consumed; got {stored['invocation']}")
        expect(stored == recorded.record,
               "and the census holds exactly the record that was built")


def test_the_run_count_is_the_measurements_own_not_a_literal() -> None:
    """X out of the handoff's own run count, never X out of ten.

    `probe_flake.py` accepts any positive `--runs`, and the handoff
    carries the count as an input this issue may not change. A ceiling
    hard-coded at ten would misclassify every measurement taken at
    another one.
    """
    short = dd.RUN_COUNT - 4
    ids = [c for c, _label in CHECKS]
    with census_file() as path:
        handoff = outcome_handoff(measurements=[
            {"role": do.ROLE_BASELINE, "exit_code": probe_flake.EXIT_OK,
             "result": result_document(
                 requested=short,
                 runs=[{name: PASS for name in ids} for _ in range(short)])}])
        recorded, _publisher = record_outcome(handoff, path)
        measurement = recorded.record["measurements"][0]
        expect(recorded.record["outcome"] == do.OUTCOME_CANNOT_REPRODUCE
               and measurement["requested_runs"] == short
               and measurement["completed_runs"] == short,
               f"a complete measurement at {short} runs is still complete; "
               f"got {recorded.record['outcome']} over {measurement}")


def test_a_malformed_outcome_handoff_is_rejected_without_recording() -> None:
    """Rejected, not classified: a malformed input reached no diagnosis."""
    def broken(mutate, route=dd.ROUTE_CANNOT_REPRODUCE):
        document = outcome_handoff(route)
        mutate(document)
        return document

    def repair_record():
        document = outcome_handoff()
        _diagnosis, produced_record = produced(dd.ROUTE_REPAIR)
        document["diagnosis_outcome"] = produced_record
        return document

    cases = (
        ("a document that is not a handoff", lambda: {"schema": "other"},
         "expected 'deflake-outcome-handoff/v1'"),
        ("a handoff with no attempt identity",
         lambda: broken(lambda d: d.__setitem__("attempt", "")),
         "`attempt` identity"),
        ("an attempt identity carrying a newline",
         lambda: broken(lambda d: d.__setitem__("attempt", "a\nb")),
         "single unpadded line"),
        ("a handoff with no diagnostic summary",
         lambda: broken(lambda d: d.__setitem__("summary", "   ")),
         "`summary`"),
        ("a handoff carrying #1437's repair ending", repair_record,
         "opens a pull request"),
        ("a handoff naming an unregistered probe",
         lambda: broken(lambda d: d["diagnosis_outcome"].__setitem__(
             "probe", "not_a_probe")),
         "not registered in probe_runner_registry.PROBES"),
        ("a handoff with no measurement at all",
         lambda: broken(lambda d: d.__setitem__("measurements", [])),
         "at least one measurement"),
        ("a handoff declaring one role twice",
         lambda: broken(lambda d: d["measurements"].append(
             copy.deepcopy(d["measurements"][0]))),
         "twice"),
        ("a cannot-reproduce handoff carrying a verification",
         lambda: broken(lambda d: d["measurements"].append({
             "role": do.ROLE_VERIFICATION,
             "exit_code": probe_flake.EXIT_OK,
             "result": verification_result(artifact_root=VERIFY_ARTIFACTS)})),
         "runs no verification batch"),
        ("a partial improvement with no verification",
         lambda: broken(lambda d: d.__setitem__(
             "measurements", d["measurements"][:1]),
             dd.ROUTE_PARTIAL_IMPROVEMENT),
         "rests on its verification measurement"),
        ("a handoff supplying its own unmet condition",
         lambda: broken(lambda d: d.__setitem__("unmet_condition", UNMET),
                        dd.ROUTE_PARTIAL_IMPROVEMENT),
         "is DERIVED from the diagnosis outcome's own `reason`"),
        ("a no-target record that names a target",
         lambda: broken(lambda d: (
             d["diagnosis_outcome"].__setitem__("targets", ["beta"]),
             d["diagnosis_outcome"]["handoff"].__setitem__(
                 "targets", ["beta"])), dd.ROUTE_NO_TARGET),
         "names no target"),
        ("a diagnosing route that names none",
         lambda: broken(lambda d: (
             d["diagnosis_outcome"].__setitem__("targets", []),
             d["diagnosis_outcome"]["handoff"].__setitem__("targets", [])),
             dd.ROUTE_NO_CONFIDENT_FIX),
         "every other route diagnoses at least one"),
        ("a diagnosis outcome with no reason at all",
         lambda: broken(lambda d: d["diagnosis_outcome"].pop("reason")),
         "cannot be reached for"),
        ("a reason its own route cannot be reached for",
         lambda: broken(lambda d: d["diagnosis_outcome"].__setitem__(
             "reason", dd.REASON_VERIFICATION_ACCEPTED)),
         "cannot be reached for"),
        ("an exit this harness never returns",
         lambda: broken(lambda d: d["measurements"][0].__setitem__(
             "exit_code", 5)),
         "not one of tools/probe_flake.py's exits"),
        ("a rejection that somehow wrote a document",
         lambda: broken(lambda d: d["measurements"][0].__setitem__(
             "exit_code", probe_flake.EXIT_REJECTED)),
         "wrote no result document"),
        ("a harness error with no document",
         lambda: broken(lambda d: d["measurements"][0].update(
             {"exit_code": probe_flake.EXIT_HARNESS_ERROR, "result": None})),
         "writes a result document, but the handoff carries none"),
        ("a measurement of another probe",
         lambda: broken(lambda d: d["measurements"][0].__setitem__(
             "result", result_document(probe=OTHER))),
         f"measured {OTHER!r}"),
        ("a retained artifact named by a relative path",
         lambda: broken(lambda d: d["diagnosis_outcome"].__setitem__(
             "retained_artifacts", ["artifacts/run-001"])),
         "must be an absolute path"),
        ("an artifact list with an entry no batch retained",
         lambda: broken(lambda d: d["diagnosis_outcome"].__setitem__(
             "retained_artifacts",
             list(d["diagnosis_outcome"]["retained_artifacts"])
             + [f"{OUTSIDE}/unrelated/run-001"]),
             dd.ROUTE_NO_CONFIDENT_FIX),
         "names evidence this attempt does not have"),
        ("an artifact list that hides one a batch retained",
         lambda: broken(lambda d: d["diagnosis_outcome"].__setitem__(
             "retained_artifacts",
             list(d["diagnosis_outcome"]["retained_artifacts"])[:-1]),
             dd.ROUTE_NO_CONFIDENT_FIX),
         "hides evidence it does"),
        ("an artifact list in an order no producer wrote",
         lambda: broken(lambda d: d["diagnosis_outcome"].__setitem__(
             "retained_artifacts",
             list(reversed(d["diagnosis_outcome"]["retained_artifacts"]))),
             dd.ROUTE_NO_CONFIDENT_FIX),
         "ordered, deduplicated union"),
        ("a retained artifact inside the primary checkout",
         lambda: broken(lambda d: d["diagnosis_outcome"].__setitem__(
             "retained_artifacts", [f"{PRIMARY_WT}/artifacts/run-001"])),
         "inside the worktree"),
        ("a retained artifact inside a comparison worktree",
         lambda: broken(lambda d: d["diagnosis_outcome"].__setitem__(
             "retained_artifacts", [f"{CLEAN_WT}/artifacts/run-001"])),
         "inside the worktree"),
        ("a batch reference whose artifact root is inside a worktree",
         lambda: broken(lambda d: d["diagnosis_outcome"]["baseline"]
                        .__setitem__("artifact_root",
                                     f"{REPAIR_WT}/artifacts")),
         "inside the worktree"),
        ("a batch reference reaching a worktree through a traversal",
         lambda: broken(lambda d: d["diagnosis_outcome"]["baseline"]
                        .__setitem__(
                            "invocation_dir",
                            f"{OUTSIDE}/../{Path(CLEAN_WT).name}/inv")),
         "inside the worktree"),
        ("a diagnosis outcome with no configuration manifest",
         lambda: broken(lambda d: d["diagnosis_outcome"].pop(
             "configuration")),
         "`configuration` must be a JSON object"),
        ("a configuration entry outside the gitignored family",
         lambda: broken(lambda d: d["diagnosis_outcome"]["configuration"]
                        ["entries"].append({"path": "../outside.local.yaml",
                                            "sha256": "0" * 64})),
         "not a member of the gitignored"),
        ("a diagnosis outcome that dropped its invocation command",
         lambda: broken(lambda d: d["diagnosis_outcome"]["handoff"].pop(
             "command")),
         "`handoff`.command must be a list"),
        ("an invocation directory that is not a path",
         lambda: broken(lambda d: d["diagnosis_outcome"]["handoff"]
                        .__setitem__("directory", "relative/checkout")),
         "not an absolute path"),
        ("a record that dropped its target list",
         lambda: broken(lambda d: d["diagnosis_outcome"].pop("targets"),
                        dd.ROUTE_NO_CONFIDENT_FIX),
         "`targets` must be a list"),
        ("a record that dropped its retained-artifact list",
         lambda: broken(lambda d: d["diagnosis_outcome"].pop(
             "retained_artifacts")),
         "`retained_artifacts` must be a list"),
    )
    for label, build, fragment in cases:
        with census_file() as path:
            before = path.read_bytes()
            publisher = Publisher()
            expect_handoff_rejected(
                lambda b=build, p=publisher: record_outcome(
                    b(), path, publisher=p),
                fragment, label)
            expect_nothing_recorded(path, before, publisher, label)


def test_the_exit_contract_is_the_harnesss_own() -> None:
    expect(set(do.EXIT_CONTRACT) == {
        probe_flake.EXIT_OK, probe_flake.EXIT_REJECTED,
        probe_flake.EXIT_NO_PORT, probe_flake.EXIT_HARNESS_ERROR},
        f"the exit contract is probe_flake's four exits; got "
        f"{sorted(do.EXIT_CONTRACT)}")
    expect(do.EXIT_CONTRACT[probe_flake.EXIT_OK]
           == probe_census.ACCEPTED_STATUS,
           "only exit 0 supplies a valid measurement")
    expect(do.EXIT_CONTRACT[probe_flake.EXIT_REJECTED] is None
           and do.EXIT_CONTRACT[probe_flake.EXIT_NO_PORT] is None,
           "exits 2 and 3 are caught before any document is rendered")
    expect(do.EXIT_CONTRACT[probe_flake.EXIT_HARNESS_ERROR]
           == "harness-error",
           "exit 4 DOES write a document, and that document says so itself")


def test_the_command_line_reports_each_ending_with_its_own_exit() -> None:
    """The three endings, through the shipped entry point."""
    tool = str(Path(__file__).resolve().parent / "deflake_outcome.py")
    with census_file() as path:
        root = Path(path).parent
        accepted = root / "accepted.json"
        accepted.write_text(json.dumps(outcome_handoff()), encoding="utf-8")
        done = subprocess.run(
            [sys.executable, tool, "--handoff", str(accepted),
             "--census", str(path), "--json"],
            capture_output=True, text=True, timeout=120)
        expect(done.returncode == do.EXIT_OK,
               f"a recordable outcome exits 0; got {done.returncode} "
               f"({done.stderr.strip()})")
        try:
            reported = json.loads(done.stdout)
        except json.JSONDecodeError:
            reported = {}
        expect(reported.get("outcome") == do.OUTCOME_CANNOT_REPRODUCE
               and reported.get("opened_pull_request") is False,
               f"and reports the outcome it recorded; got {done.stdout[:200]}")
        expect(len(stored_outcomes(path)) == 1,
               "and the census holds it afterwards")

        before = path.read_bytes()
        defect = root / "defect.json"
        defect.write_text(
            json.dumps(outcome_handoff(dd.ROUTE_PRODUCTION_DEFECT)),
            encoding="utf-8")
        done = subprocess.run(
            [sys.executable, tool, "--handoff", str(defect),
             "--census", str(path)],
            capture_output=True, text=True, timeout=120)
        expect(done.returncode == do.EXIT_NON_SUCCESS
               and "#1438" in done.stderr,
               f"an actionable non-success exits 3 naming its owner; got "
               f"{done.returncode} ({done.stderr.strip()[:200]})")

        malformed = root / "malformed.json"
        malformed.write_text(json.dumps({"schema": "nope"}), encoding="utf-8")
        done = subprocess.run(
            [sys.executable, tool, "--handoff", str(malformed),
             "--census", str(path)],
            capture_output=True, text=True, timeout=120)
        expect(done.returncode == do.EXIT_REJECTED,
               f"a malformed handoff exits 2; got {done.returncode}")
        expect(path.read_bytes() == before,
               "and neither ending touched the census")


def _collect() -> tuple:
    """This owner's tests, in source order, out of its own namespace.

    Derived rather than hand-listed, so a case added below joins the
    registry by existing: a hand-maintained roster is exactly the
    fourth list that could silently drop a case while the run still
    exited zero.

    `__module__` is checked because the names imported from the shared
    support module are in these globals too, and a helper that ever
    started with `test_` would otherwise be claimed by whichever owner
    imported it.
    """
    return tuple(value for name, value in globals().items()
                 if name.startswith("test_") and callable(value)
                 and getattr(value, "__module__", None) == __name__)


#: This owner's registry. The facade collects from exactly this, for
#: both the focused invocation and the aggregate one, so the two can
#: never disagree about what this owner declares.
TESTS = _collect()
