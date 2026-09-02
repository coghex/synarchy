#!/usr/bin/env python3
"""Select/claim/measure/record orchestration cases (#1436), split out by #2093.

The 31 cases covering the fixed measurement contract, selection
outcomes, the claim, resources, the commit cohort, recorder outcomes
against the REAL recorder, and the CLI and low-level boundaries --
interruption and cleanup included.

Three things here are deliberately real rather than faked, because
faking them would move the assertion off the thing being tested: the
shipped `probe_census` recorder, whose pre- versus post-replacement
failure distinction is a property of its own `_atomic_replace`; a
foreign exclusive resource holder in a REAL second process, which is
the `/deflake`-versus-`run_probes` conflict direction
(`tools/test_run_probes.py` owns the other); and the real claim module,
for the claimed-set case.

Not a gate of its own. Run through the aggregate:

  python3 tools/test_deflake.py --only orchestration
"""
from __future__ import annotations

import copy
import errno
import json
import os
import stat
import subprocess
import sys
import textwrap
import time
import uuid
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import deflake  # type: ignore  # noqa: E402
import probe_census  # type: ignore  # noqa: E402
import probe_claim  # type: ignore  # noqa: E402
import probe_flake  # type: ignore  # noqa: E402
import probe_resource_lock  # type: ignore  # noqa: E402
import probe_select  # type: ignore  # noqa: E402
from deflake_selftest_support import (  # noqa: E402
    COMMIT, OTHER, OTHER_COMMIT, PROBE, TOOLS_DIR, FakeClaim, Recorder,
    Scratch, expect, held_resources, installed_census, measurement, run)


# --------------------------------------------------------------------------
# The fixed measurement contract
# --------------------------------------------------------------------------
def test_the_harness_is_told_ten_runs_and_four_capabilities() -> None:
    print("\n-- the harness receives runs=10 and rts_capabilities=4, "
          "explicitly")
    scratch = Scratch()
    try:
        expect(deflake.CENSUS_RUN_COUNT == 10,
               f"one census measurement is ten runs "
               f"(got {deflake.CENSUS_RUN_COUNT})")
        expect(deflake.CENSUS_RUN_COUNT == probe_census.POLICY_RUN_COUNT,
               "and it is the same N the selection ladder judges a cohort "
               "against, not a private copy")
        expect(deflake.RTS_CAPABILITIES == 4,
               f"every run uses four RTS capabilities "
               f"(got {deflake.RTS_CAPABILITIES})")
        measure = Recorder(measurement(scratch))
        result = run(scratch, measure=measure,
                     record_result=probe_census.record_result_installed)
        expect(result.outcome == deflake.OUTCOME_RECORDED,
               f"the measurement is recorded ({result.outcome}: {result.detail})")
        expect(len(measure.calls) == 1,
               f"the harness was invoked exactly once ({measure.calls})")
        call = measure.calls[0] if measure.calls else {}
        expect(call.get("runs") == 10,
               f"with runs=10 supplied by the orchestrator (got "
               f"{call.get('runs')!r})")
        expect(call.get("rts_caps") == 4,
               f"and rts_caps=4 supplied EXPLICITLY, not left to the "
               f"harness default (got {call.get('rts_caps')!r})")
        sample = (scratch.census_of().get("current") or {}).get("samples")
        recorded = (sample or [{}])[-1]
        expect(recorded.get("rts_capabilities") == 4,
               f"and the durable census record reports four effective "
               f"capabilities ({recorded.get('rts_capabilities')!r})")
        expect(recorded.get("requested_runs") == 10,
               f"and ten requested runs ({recorded.get('requested_runs')!r})")
    finally:
        scratch.cleanup()


def test_pass_fail_and_timeout_are_all_valid_observations() -> None:
    print("\n-- PASS, FAIL and TIMEOUT are all recorded, and TIMEOUT counts "
          "toward the failure rate")
    scratch = Scratch()
    try:
        outcomes = ([probe_flake.RUN_PASS] * 7 + [probe_flake.RUN_FAIL] * 2
                    + [probe_flake.RUN_TIMEOUT])
        result = run(scratch, measure=Recorder(measurement(
                        scratch, outcomes=outcomes)),
                     record_result=probe_census.record_result_installed)
        expect(result.outcome == deflake.OUTCOME_RECORDED,
               f"a cohort holding failures and a timeout is still a valid "
               f"measurement ({result.outcome}: {result.detail})")
        expect(result.exit_code == 0, f"and exits 0 (got {result.exit_code})")
        cohort = scratch.census_of().get("current") or {}
        sample = (cohort.get("samples") or [{}])[-1]
        expect(sample.get("failure_count") == 3,
               f"the recorded failure count includes the timeout "
               f"({sample.get('failure_count')!r})")
        expect(sample.get("timeout_count") == 1,
               f"and the timeout is reported in its own right "
               f"({sample.get('timeout_count')!r})")
        expect(result.measurement is not None
               and result.measurement.failure_rate == 0.3,
               f"the aggregate rate is 3/10 "
               f"({getattr(result.measurement, 'failure_rate', None)!r})")
    finally:
        scratch.cleanup()


# --------------------------------------------------------------------------
# Selection outcomes
# --------------------------------------------------------------------------
def test_no_qualifying_probe_is_a_successful_no_work_outcome() -> None:
    print("\n-- a valid census with nothing to measure is a success that "
          "says why")
    scratch = Scratch()
    try:
        # Every eligible probe excluded, so the selector returns
        # no-candidate for the "everything is claimed" reason rather than
        # the "roster is healthy" one -- and the reasons must survive.
        measure = Recorder(measurement(scratch))
        result = run(scratch, measure=measure, claimed={PROBE, OTHER})
        expect(result.outcome == deflake.OUTCOME_NO_QUALIFYING_PROBE,
               f"the outcome is no-qualifying-probe ({result.outcome})")
        expect(result.exit_code == 0, f"exit 0 (got {result.exit_code})")
        expect(measure.calls == [], "and nothing was measured")
        reasons = {entry["probe"]: entry["reasons"] for entry in result.skipped}
        expect(reasons.get(PROBE) == [probe_select.REASON_CLAIMED],
               f"the selector's exclusion reasons are reported "
               f"({reasons})")
    finally:
        scratch.cleanup()


def test_deferred_probes_are_never_claimed_or_measured() -> None:
    print("\n-- deferred probes are excluded before claim or measurement")
    scratch = Scratch()
    try:
        for probe in (PROBE, OTHER):
            probe_census.record_deferral(
                scratch.census, probe,
                reason="the scenario's content is intentionally incomplete",
                resume_when="the planned content assets merge")
        measure = Recorder(measurement(scratch))
        claims: list[str] = []

        def claim_it(probe, **_kwargs):
            claims.append(probe)
            return FakeClaim(probe)

        result = run(scratch, acquire_claim=claim_it, measure=measure)
        expect(result.outcome == deflake.OUTCOME_NO_QUALIFYING_PROBE,
               f"the lab reports no qualifying probe ({result.outcome})")
        expect(claims == [], "no deferred probe is claimed")
        expect(measure.calls == [], "no deferred probe is measured")
        reasons = {entry["probe"]: entry["reasons"]
                   for entry in result.skipped}
        expect(reasons == {
            PROBE: [probe_select.REASON_DEFERRED],
            OTHER: [probe_select.REASON_DEFERRED]},
            f"the exclusion is explicitly reported as deferred ({reasons})")
    finally:
        scratch.cleanup()


def test_a_malformed_census_fails_before_anything_is_claimed() -> None:
    print("\n-- census data that cannot be ranked fails before any claim, "
          "resource, measurement or write")
    scratch = Scratch()
    try:
        document = json.loads(scratch.census.read_text(encoding="utf-8"))
        for entry in document["probes"]:
            if entry["key"] == PROBE:
                entry["census"]["current"] = {"nonsense": True}
        scratch.census.write_text(json.dumps(document), encoding="utf-8")
        before = scratch.bytes_now()
        claims: list = []
        measure = Recorder(measurement(scratch))

        def claim_it(probe, **kw):
            claims.append(probe)
            return FakeClaim(probe)

        result = run(scratch, acquire_claim=claim_it, measure=measure,
                     record_result=probe_census.record_result_installed)
        expect(result.outcome == deflake.OUTCOME_SELECTOR_ERROR,
               f"the outcome is selector-error ({result.outcome}: "
               f"{result.detail})")
        expect(result.exit_code != 0,
               f"and it is nonzero (got {result.exit_code})")
        expect(claims == [], "nothing was claimed")
        expect(measure.calls == [], "nothing was measured")
        expect(scratch.bytes_now() == before, "and the census is untouched")
    finally:
        scratch.cleanup()


def test_an_unreachable_census_at_selection_time_is_a_selector_error() -> None:
    print("\n-- an absent docs-wip census at SELECTION time is selector-error, "
          "the parallel of claim-audit-failed after acquisition")
    scratch = Scratch()
    try:
        def missing(_path):
            raise probe_census.DocsWorktreeMissing(
                "no worktree is on branch docs-wip")

        claims: list = []
        result = run(scratch, load_census=missing,
                     acquire_claim=lambda probe, **kw: claims.append(probe))
        expect(result.outcome == deflake.OUTCOME_SELECTOR_ERROR,
               f"the outcome is selector-error ({result.outcome})")
        expect(result.exit_code == deflake.EXIT_CODES[
                   deflake.OUTCOME_SELECTOR_ERROR] != 0,
               "with its own nonzero status")
        expect(claims == [], "and nothing was claimed")
        expect("docs-wip" in result.detail,
               f"the diagnostic names the missing worktree ({result.detail})")
    finally:
        scratch.cleanup()


def test_exactly_one_probe_is_selected_per_invocation() -> None:
    print("\n-- one invocation measures at most ONE probe")
    scratch = Scratch()
    try:
        measure = Recorder(measurement(scratch))
        claimed_keys: list = []

        def claim_it(probe, **kw):
            claimed_keys.append(probe)
            return FakeClaim(probe)

        result = run(scratch, acquire_claim=claim_it, measure=measure,
                     record_result=probe_census.record_result_installed)
        expect(result.outcome == deflake.OUTCOME_RECORDED,
               f"the invocation records one measurement ({result.outcome})")
        expect(len(claimed_keys) == 1,
               f"exactly one probe was claimed ({claimed_keys})")
        expect(len(measure.calls) == 1,
               f"and exactly one measured ({measure.calls})")
        expect(result.probe == claimed_keys[0] == measure.calls[0]["probe"],
               "and they are the same probe")
    finally:
        scratch.cleanup()


# --------------------------------------------------------------------------
# The claim
# --------------------------------------------------------------------------
def test_a_lost_selection_to_claim_race_does_nothing_at_all() -> None:
    print("\n-- losing the selection-to-claim race is a clean no-work "
          "success")
    scratch = Scratch()
    try:
        before = scratch.bytes_now()
        measure = Recorder(measurement(scratch))
        resources: list = []

        def denied(probe, **kw):
            raise probe_claim.ClaimDenied(probe, owner="another agent",
                                          age_seconds=12.0)

        def take(probe, **kw):
            resources.append(probe)
            return held_resources(probe)

        result = run(scratch, acquire_claim=denied, measure=measure,
                     acquire_resources=take,
                     record_claim=probe_census.record_claim,
                     record_result=probe_census.record_result_installed)
        expect(result.outcome == deflake.OUTCOME_CLAIM_BUSY,
               f"the outcome is claim-busy ({result.outcome})")
        expect(result.exit_code == 0,
               f"and it is a SUCCESS ({result.exit_code})")
        expect(measure.calls == [], "the probe was not executed")
        expect(resources == [], "no resource interest was acquired")
        expect(scratch.bytes_now() == before,
               "no competing acquisition or measurement record was written")
        expect(result.ownership == deflake.OWNERSHIP_NONE,
               f"and nothing is owned ({result.ownership})")
        expect("another agent" in result.detail,
               f"the winner is named rather than the loss being silent "
               f"({result.detail})")
    finally:
        scratch.cleanup()


def test_a_claim_audit_failure_releases_the_claim_and_runs_nothing() -> None:
    print("\n-- an unrecordable acquisition releases the claim and refuses "
          "to run")
    scratch = Scratch()
    try:
        claim = FakeClaim()
        measure = Recorder(measurement(scratch))

        def refuse(*_a, **_kw):
            raise probe_census.CensusError("the census refuses this claim")

        result = run(scratch, acquire_claim=lambda probe, **kw: claim,
                     record_claim=refuse, measure=measure)
        expect(result.outcome == deflake.OUTCOME_CLAIM_AUDIT_FAILED,
               f"the outcome is claim-audit-failed ({result.outcome})")
        expect(result.exit_code != 0,
               f"and it is nonzero ({result.exit_code})")
        expect(claim.released, "the owned claim was released")
        expect(result.ownership == deflake.OWNERSHIP_NONE,
               f"and the result says nothing is still owned "
               f"({result.ownership})")
        expect(measure.calls == [], "the probe was not run")
    finally:
        scratch.cleanup()


def test_the_acquisition_token_is_retained_and_reported() -> None:
    print("\n-- the acquisition token is retained for every "
          "ownership-sensitive step")
    scratch = Scratch()
    try:
        claim = FakeClaim(token="token-retained")
        result = run(scratch, acquire_claim=lambda probe, **kw: claim,
                     measure=Recorder(measurement(scratch)),
                     record_result=probe_census.record_result_installed)
        expect(result.token == "token-retained",
               f"the result carries the acquisition token ({result.token})")
        expect(claim.commits == ["audit", "record"],
               f"both census mutations went through the claim's own "
               f"token-checked commit ({claim.commits})")
        expect(claim.reasserted == 1,
               f"and ownership was reasserted immediately before the "
               f"measurement ({claim.reasserted})")
        expect(claim.released, "the claim was released at the end")
    finally:
        scratch.cleanup()


def test_a_claim_lost_before_the_measurement_runs_nothing() -> None:
    print("\n-- a claim lost before the measurement starts runs nothing")
    scratch = Scratch()
    try:
        claim = FakeClaim(lose_on="reassert")
        measure = Recorder(measurement(scratch))
        result = run(scratch, acquire_claim=lambda probe, **kw: claim,
                     measure=measure)
        expect(result.outcome == deflake.OUTCOME_CLAIM_BUSY,
               f"the outcome is claim-busy ({result.outcome}: {result.detail})")
        expect(measure.calls == [],
               "and the probe was NOT started, because starting a "
               "measurement this run no longer owns is the duplicate work "
               "the claim exists to prevent")
    finally:
        scratch.cleanup()


def test_the_claimed_set_keeps_a_held_probe_out_of_selection() -> None:
    print("\n-- live claims are read from the claim namespace and passed to "
          "the selector")
    scratch = Scratch()
    try:
        # A real claim file, written by the real claim module.
        claim = probe_claim.acquire(PROBE, root=scratch.claims,
                                    lease_seconds=600.0)
        try:
            held = deflake.claimed_probe_keys(
                registry={PROBE: "a", OTHER: "b"}, root=scratch.claims,
                now=probe_claim.utc_now())
            expect(held == {PROBE},
                   f"the held probe is reported as claimed ({held})")
        finally:
            claim.release()
        held = deflake.claimed_probe_keys(
            registry={PROBE: "a", OTHER: "b"}, root=scratch.claims,
            now=probe_claim.utc_now())
        expect(held == set(),
               f"and nothing is claimed once it is released ({held})")
        # An EXPIRED claim is not a live one: the lease is what decides.
        expired = probe_claim.acquire(OTHER, root=scratch.claims,
                                      lease_seconds=0.01)
        time.sleep(0.05)
        held = deflake.claimed_probe_keys(
            registry={PROBE: "a", OTHER: "b"}, root=scratch.claims,
            now=probe_claim.utc_now())
        expect(held == set(),
               f"a lapsed claim does not keep a probe out of selection "
               f"({held})")
        expired.release()
    finally:
        scratch.cleanup()


# --------------------------------------------------------------------------
# Resources
# --------------------------------------------------------------------------
def test_resource_busy_releases_only_the_owned_claim() -> None:
    print("\n-- a resource conflict is a no-work success that gives the "
          "claim back and creates nothing")
    scratch = Scratch()
    try:
        claim = FakeClaim()
        measure = Recorder(measurement(scratch))

        def busy(probe, **kw):
            raise probe_resource_lock.ResourceBusy(
                "repo-config", probe_resource_lock.SHARED,
                namespace="selftest",
                holders=[{"owner": "another sweep",
                          "interest": probe_resource_lock.EXCLUSIVE}])

        result = run(scratch, acquire_claim=lambda probe, **kw: claim,
                     acquire_resources=busy, measure=measure,
                     record_result=probe_census.record_result_installed)
        expect(result.outcome == deflake.OUTCOME_RESOURCE_BUSY,
               f"the outcome is resource-busy ({result.outcome})")
        expect(result.exit_code == 0,
               f"and it is a SUCCESS ({result.exit_code})")
        expect(claim.released, "the OWNED claim was released")
        expect(measure.calls == [], "no measurement artifact was created")
        expect(result.artifacts == [] and result.result_path is None,
               f"and no result document ({result.result_path})")
        conflict = result.conflict or {}
        expect(conflict.get("resource") == "repo-config",
               f"the conflicting resource is reported ({conflict})")
        expect(any(entry.get("owner") == "another sweep"
                   for entry in conflict.get("holders") or ()),
               f"with the available owner information ({conflict})")
    finally:
        scratch.cleanup()


def test_a_no_work_outcome_never_reports_retained_ownership() -> None:
    print("\n-- a success-shaped outcome that could not give the claim back "
          "stops being a success")
    scratch = Scratch()
    try:
        def busy(probe, **kw):
            raise probe_resource_lock.ResourceBusy(
                "repo-config", probe_resource_lock.SHARED,
                namespace="selftest",
                holders=[{"owner": "another sweep",
                          "interest": probe_resource_lock.EXCLUSIVE}])

        # exit 0 is read by a surrounding workflow as "nothing happened,
        # move on". A claim this process is still holding is not nothing:
        # the probe stays out of every other agent's reach until the lease
        # expires, and reporting that under the one status nobody
        # investigates would hide it.
        stuck = FakeClaim(release_error="the claim file is unwritable")
        result = run(scratch, acquire_claim=lambda probe, **kw: stuck,
                     acquire_resources=busy,
                     measure=Recorder(measurement(scratch)))
        expect(result.outcome == deflake.OUTCOME_MANAGED_ERROR,
               f"a resource conflict whose claim release FAILS is a managed "
               f"error, not resource-busy ({result.outcome}: {result.detail})")
        expect(result.exit_code != 0,
               f"and it is nonzero ({result.exit_code})")
        expect(result.ownership == deflake.OWNERSHIP_CLAIM_HELD,
               f"naming the ownership that remains ({result.ownership})")
        expect(stuck.token in result.detail
               and "the claim file is unwritable" in result.detail,
               f"with the token and the release diagnostic ({result.detail})")
        expect((result.conflict or {}).get("resource") == "repo-config",
               f"and the conflict is still reported ({result.conflict})")

        # The same rule on the other exit-0 path that has ever owned a
        # claim: a claim lost before the measurement started.
        stuck = FakeClaim(lose_on="reassert",
                          release_error="the claim file is unwritable")
        result = run(scratch, acquire_claim=lambda probe, **kw: stuck,
                     measure=Recorder(measurement(scratch)))
        expect(result.outcome == deflake.OUTCOME_MANAGED_ERROR,
               f"a lost claim whose release also fails is a managed error "
               f"({result.outcome})")
        expect(result.ownership == deflake.OWNERSHIP_CLAIM_HELD,
               f"reporting the retained ownership ({result.ownership})")

        # And the invariant itself: every documented exit-0 outcome the
        # orchestrator can reach owns nothing.
        for outcome, releasing in (
                (deflake.OUTCOME_RESOURCE_BUSY,
                 dict(acquire_resources=busy)),
                (deflake.OUTCOME_CLAIM_BUSY,
                 dict(acquire_claim=lambda probe, **kw:
                      FakeClaim(lose_on="reassert"))),
                (deflake.OUTCOME_NO_QUALIFYING_PROBE,
                 dict(claimed={PROBE, OTHER})),
                (deflake.OUTCOME_RECORDED,
                 dict(record_result=probe_census.record_result_installed))):
            got = run(scratch, measure=Recorder(measurement(scratch)),
                      **releasing)
            expect(got.outcome == outcome
                   and got.exit_code == 0
                   and got.ownership == deflake.OWNERSHIP_NONE,
                   f"{outcome} exits 0 and owns nothing "
                   f"(got {got.outcome}/{got.exit_code}/{got.ownership})")
    finally:
        scratch.cleanup()


FOREIGN_HOLDER_SRC = textwrap.dedent("""\
    import sys, time
    from pathlib import Path
    sys.path.insert(0, sys.argv[1])
    import probe_resource_lock as lock
    namespace, ready, release = sys.argv[2:5]
    hold = lock.acquire(exclusive={"repo-config"}, namespace=namespace,
                        purpose="a foreign run_probes sweep")
    Path(ready).write_text("held")
    deadline = time.time() + 120
    while not Path(release).exists() and time.time() < deadline:
        time.sleep(0.02)
    hold.release()
""")


def test_a_real_foreign_holder_makes_deflake_report_resource_busy() -> None:
    print("\n-- against a REAL foreign exclusive holder in a REAL second "
          "process, /deflake reports resource-busy")
    scratch = Scratch()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    script = scratch.root / "holder.py"
    script.write_text(FOREIGN_HOLDER_SRC)
    ready = scratch.root / "ready"
    release_flag = scratch.root / "release"
    proc = subprocess.Popen([sys.executable, str(script), TOOLS_DIR,
                             namespace, str(ready), str(release_flag)])
    try:
        deadline = time.monotonic() + 30
        while not ready.exists() and time.monotonic() < deadline:
            time.sleep(0.02)
        expect(ready.exists(), "the foreign process holds repo-config "
                               "exclusively")
        claim = FakeClaim()
        measure = Recorder(measurement(scratch))
        # The REAL resource adapter, against the real lock files. Every
        # registered probe holds `repo-config` shared, so this is the
        # shared-measurement-versus-exclusive-holder conflict.
        result = run(scratch, acquire_claim=lambda probe, **kw: claim,
                     acquire_resources=deflake._acquire_probe_resources,
                     namespace=namespace, measure=measure,
                     record_result=probe_census.record_result_installed)
        expect(result.outcome == deflake.OUTCOME_RESOURCE_BUSY,
               f"/deflake reports resource-busy ({result.outcome}: "
               f"{result.detail})")
        expect(measure.calls == [], "and measured nothing")
        expect(claim.released, "and gave its own claim back")
        release_flag.write_text("go")
        proc.wait(timeout=30)
        # Once the foreign holder is gone the same acquisition succeeds,
        # so the refusal was the conflict and not a broken adapter.
        claim = FakeClaim()
        result = run(scratch, acquire_claim=lambda probe, **kw: claim,
                     acquire_resources=deflake._acquire_probe_resources,
                     namespace=namespace,
                     measure=Recorder(measurement(scratch)),
                     record_result=probe_census.record_result_installed)
        expect(result.outcome == deflake.OUTCOME_RECORDED,
               f"and the measurement runs once the holder is gone "
               f"({result.outcome}: {result.detail})")
    finally:
        try:
            release_flag.write_text("go")
            proc.wait(timeout=30)
        except (OSError, subprocess.TimeoutExpired):
            proc.kill()
        for entry in probe_resource_lock.LOCK_ROOT.glob(
                f"{probe_resource_lock.SHARED_PREFIX}-{namespace}-*"):
            try:
                entry.unlink()
            except OSError:
                pass
        scratch.cleanup()


# --------------------------------------------------------------------------
# The commit cohort
# --------------------------------------------------------------------------
def test_a_checkout_that_moved_refuses_to_record() -> None:
    print("\n-- a HEAD that moved during the measurement refuses the cohort")
    scratch = Scratch()
    try:
        before = scratch.bytes_now()
        claim = FakeClaim()
        heads = iter([COMMIT, OTHER_COMMIT])
        result = run(scratch, acquire_claim=lambda probe, **kw: claim,
                     head_commit=lambda: next(heads),
                     measure=Recorder(measurement(scratch)),
                     record_result=probe_census.record_result_installed,
                     record_claim=lambda *a, **kw: PROBE)
        expect(result.outcome == deflake.OUTCOME_COMMIT_CHANGED,
               f"the outcome is commit-changed ({result.outcome})")
        expect(result.exit_code != 0, f"nonzero ({result.exit_code})")
        expect(COMMIT in result.detail and OTHER_COMMIT in result.detail,
               f"and BOTH commits are named ({result.detail})")
        expect(scratch.bytes_now() == before,
               "nothing was recorded into either cohort")
        expect(claim.released, "managed ownership was released")
        expect(result.result_path is not None
               and Path(result.result_path).is_file(),
               f"and the result document is retained OUTSIDE every worktree "
               f"({result.result_path})")
        expect(result.artifacts, "with the diagnostic artifacts kept")
    finally:
        scratch.cleanup()


def test_a_result_document_naming_another_commit_refuses_the_cohort() -> None:
    print("\n-- a result document whose own commit differs is refused too")
    scratch = Scratch()
    try:
        before = scratch.bytes_now()
        result = run(scratch, head_commit=lambda: COMMIT,
                     measure=Recorder(measurement(scratch,
                                                  commit=OTHER_COMMIT)),
                     record_result=probe_census.record_result_installed,
                     record_claim=lambda *a, **kw: PROBE)
        expect(result.outcome == deflake.OUTCOME_COMMIT_CHANGED,
               f"the outcome is commit-changed ({result.outcome}: "
               f"{result.detail})")
        expect(scratch.bytes_now() == before, "and nothing was recorded")
    finally:
        scratch.cleanup()


def test_an_unresolved_commit_never_becomes_a_cohort() -> None:
    print("\n-- an unresolved commit is refused before the probe runs, not "
          "recorded into a fabricated cohort")
    scratch = Scratch()
    try:
        claim = FakeClaim()
        measure = Recorder(measurement(scratch))
        result = run(scratch, acquire_claim=lambda probe, **kw: claim,
                     head_commit=lambda: probe_census.PLACEHOLDER_COMMIT,
                     measure=measure)
        expect(result.outcome == deflake.OUTCOME_COMMIT_CHANGED,
               f"the outcome is commit-changed ({result.outcome})")
        expect(measure.calls == [],
               "and the probe was never run, so no engine time is spent on "
               "a measurement that could not be attributed")
        expect(claim.released, "the claim was released")
    finally:
        scratch.cleanup()


def test_the_cohort_commit_is_captured_once_before_the_runs() -> None:
    print("\n-- the commit is captured once, after ownership and before the "
          "first run, and the recorder is not handed a second one")
    scratch = Scratch()
    try:
        claim = FakeClaim()
        ingested: list = []
        result = run(scratch, acquire_claim=lambda probe, **kw: claim,
                     measure=Recorder(measurement(scratch)),
                     record_result=lambda path, document: (
                         ingested.append(document),
                         (PROBE, installed_census()))[1])
        expect(result.commit == COMMIT,
               f"the captured reference is reported ({result.commit})")
        expect(claim.audits == [(COMMIT, 10)],
               f"the acquisition audit carries it, once ({claim.audits})")
        expect(len(ingested) == 1
               and ingested[0]["commit_sha"] == COMMIT,
               "and the value ingested is the RESULT DOCUMENT's own "
               "commit_sha, not a second value pushed at the recorder")
        expect(result.outcome == deflake.OUTCOME_RECORDED,
               f"({result.outcome})")
    finally:
        scratch.cleanup()


# --------------------------------------------------------------------------
# Recorder outcomes, against the real recorder
# --------------------------------------------------------------------------
def test_a_harness_error_appends_one_attempt_and_changes_nothing_else() -> None:
    print("\n-- a well-formed harness error appends exactly one non-accepted "
          "attempt and changes no cohort, sample or aggregate")
    scratch = Scratch()
    try:
        first = run(scratch, measure=Recorder(measurement(scratch)),
                    record_result=probe_census.record_result_installed)
        expect(first.outcome == deflake.OUTCOME_RECORDED,
               f"a good measurement lands first ({first.outcome})")
        cohort_before = copy.deepcopy(scratch.census_of().get("current"))
        history_before = copy.deepcopy(scratch.census_of().get("history"))
        attempts_before = len(scratch.census_of().get("attempts") or [])

        result = run(scratch, measure=Recorder(measurement(
                        scratch, harness_error=True)),
                     record_result=probe_census.record_result_installed)
        expect(result.outcome == deflake.OUTCOME_HARNESS_ERROR,
               f"the outcome is harness-error ({result.outcome})")
        expect(result.exit_code != 0,
               f"and it is nonzero ({result.exit_code})")
        attempts = scratch.census_of().get("attempts") or []
        expect(len(attempts) == attempts_before + 1,
               f"exactly one attempt was appended "
               f"({attempts_before} -> {len(attempts)})")
        expect(attempts[-1].get("accepted") is False,
               f"and it is not accepted ({attempts[-1].get('accepted')!r})")
        expect(scratch.census_of().get("current") == cohort_before,
               "the current cohort is unchanged")
        expect(scratch.census_of().get("history") == history_before,
               "and so is the history")
    finally:
        scratch.cleanup()


def test_a_pre_replacement_failure_leaves_the_census_bytes_unchanged() -> None:
    print("\n-- a recorder failure BEFORE the atomic replacement is "
          "record-failed, with the authoritative bytes untouched")
    scratch = Scratch()
    saved = probe_census.tempfile.mkstemp
    try:
        before = scratch.bytes_now()
        claim = FakeClaim()

        def refuse(*_a, **_kw):
            raise OSError(errno.EIO, "synthetic staging-write failure")

        probe_census.tempfile.mkstemp = refuse
        result = run(scratch, acquire_claim=lambda probe, **kw: claim,
                     measure=Recorder(measurement(scratch)),
                     record_result=probe_census.record_result_installed)
        probe_census.tempfile.mkstemp = saved
        expect(result.outcome == deflake.OUTCOME_RECORD_FAILED,
               f"the outcome is record-failed ({result.outcome}: "
               f"{result.detail})")
        expect(result.exit_code != 0, f"nonzero ({result.exit_code})")
        expect(scratch.bytes_now() == before,
               "the authoritative census bytes are unchanged")
        expect(claim.released, "managed ownership was released")
        expect(result.ownership == deflake.OWNERSHIP_NONE,
               f"and the result says so ({result.ownership})")
        expect(result.result_path is not None
               and Path(result.result_path).is_file(),
               f"the measurement is retained rather than lost "
               f"({result.result_path})")
        expect("--record" in result.detail,
               f"and the diagnostic names the recovery ({result.detail})")
    finally:
        probe_census.tempfile.mkstemp = saved
        scratch.cleanup()


def test_a_post_replacement_failure_is_indeterminate_and_keeps_the_claim() -> None:
    print("\n-- a durability failure AFTER the replacement is "
          "record-indeterminate: no retry, no compensating record, claim "
          "left for TTL recovery")
    scratch = Scratch()
    saved = probe_census.os.fsync
    try:
        before = scratch.bytes_now()
        claim = FakeClaim()

        def files_only(fd):
            # The FILE fsync inside the staging write still has to work;
            # only the DIRECTORY fsync -- the one step that runs after
            # `os.replace` -- fails. That is the exact state the outcome
            # describes, and the only honest way to reach it.
            if stat.S_ISDIR(os.fstat(fd).st_mode):
                raise OSError(errno.EIO, "synthetic durability failure")
            return saved(fd)

        probe_census.os.fsync = files_only
        result = run(scratch, acquire_claim=lambda probe, **kw: claim,
                     measure=Recorder(measurement(scratch)),
                     record_result=probe_census.record_result_installed)
        probe_census.os.fsync = saved
        expect(result.outcome == deflake.OUTCOME_RECORD_INDETERMINATE,
               f"the outcome is record-indeterminate ({result.outcome}: "
               f"{result.detail})")
        expect(result.exit_code != 0, f"nonzero ({result.exit_code})")
        expect(scratch.bytes_now() != before,
               "the replacement really is already visible, which is what "
               "makes 'unchanged' the wrong thing to report")
        expect(not claim.released,
               "the claim is LEFT for token-aware diagnostics and TTL "
               "recovery rather than released")
        expect(result.ownership == deflake.OWNERSHIP_CLAIM_HELD,
               f"and the result says which ownership remains "
               f"({result.ownership})")
        expect(claim.token in result.detail,
               f"reporting the acquisition token for reconciliation "
               f"({result.detail})")
        expect(claim.commits == ["audit", "record"],
               f"ingestion was attempted exactly once and NOT retried "
               f"({claim.commits})")
        samples = (scratch.census_of().get("current") or {}).get("samples") or []
        expect(len(samples) == 1,
               f"so exactly one sample exists, never a duplicate "
               f"({len(samples)})")
    finally:
        probe_census.os.fsync = saved
        scratch.cleanup()


def test_a_release_failure_after_a_commit_reports_both_facts() -> None:
    print("\n-- a claim release that fails after the census committed is "
          "recorded-release-failed, with no rollback and no duplicate")
    scratch = Scratch()
    try:
        claim = FakeClaim(release_error="the claim file is unwritable")
        result = run(scratch, acquire_claim=lambda probe, **kw: claim,
                     measure=Recorder(measurement(scratch)),
                     record_result=probe_census.record_result_installed)
        expect(result.outcome == deflake.OUTCOME_RECORDED_RELEASE_FAILED,
               f"the outcome is recorded-release-failed ({result.outcome})")
        expect(result.exit_code != 0,
               f"and it is NOT reported as ordinary success "
               f"({result.exit_code})")
        expect("WAS recorded" in result.detail
               and "the claim file is unwritable" in result.detail,
               f"both facts are reported ({result.detail})")
        expect(claim.token in result.detail,
               "with the acquisition token for TTL recovery")
        expect(result.ownership == deflake.OWNERSHIP_CLAIM_HELD,
               f"and the claim is left ({result.ownership})")
        samples = (scratch.census_of().get("current") or {}).get("samples") or []
        expect(len(samples) == 1,
               f"the committed append-only update was neither rolled back "
               f"nor repeated ({len(samples)} sample(s))")
    finally:
        scratch.cleanup()


def test_only_a_valid_measurement_reaches_an_accepted_ingestion() -> None:
    print("\n-- only a valid measurement's references reach an ACCEPTED "
          "ingestion")
    scratch = Scratch()
    try:
        run(scratch, measure=Recorder(measurement(scratch, harness_error=True)),
            record_result=probe_census.record_result_installed)
        cohort = scratch.census_of().get("current")
        expect(cohort is None,
               f"a harness error creates no cohort at all ({cohort})")
        attempts = scratch.census_of().get("attempts") or []
        expect(len(attempts) == 1 and attempts[-1].get("accepted") is False,
               f"only a non-accepted attempt ({attempts})")
        run(scratch, measure=Recorder(measurement(scratch)),
            record_result=probe_census.record_result_installed)
        cohort = scratch.census_of().get("current") or {}
        expect(len(cohort.get("samples") or []) == 1,
               f"and the valid measurement is the only accepted sample "
               f"({cohort.get('samples')})")
    finally:
        scratch.cleanup()


# --------------------------------------------------------------------------
# Boundaries
# --------------------------------------------------------------------------
def test_the_orchestrator_never_manipulates_raw_artifacts() -> None:
    print("\n-- the orchestrator consumes artifact references and never "
          "deletes, relocates or reimplements the harness's lifecycle")
    scratch = Scratch()
    try:
        result_measurement = measurement(scratch)
        directories = [Path(entry)
                       for entry in result_measurement.retained_artifacts()]
        for directory in directories:
            (directory / "engine.log").write_text("engine output\n")
        stamps = {directory: sorted(p.name for p in directory.iterdir())
                  for directory in directories}
        result = run(scratch, measure=Recorder(result_measurement),
                     record_result=probe_census.record_result_installed)
        expect(result.outcome == deflake.OUTCOME_RECORDED,
               f"the measurement is recorded ({result.outcome})")
        expect(all(directory.is_dir() for directory in directories),
               "every retained artifact directory still exists")
        expect(all(sorted(p.name for p in directory.iterdir()) == stamps[directory]
                   for directory in directories),
               "with its contents untouched")
        expect(result.artifacts == [str(directory) for directory in directories],
               f"and the result carries the harness's own references "
               f"({result.artifacts})")
        # Nothing RAW ever lands outside the harness's artifact tree. The
        # census's own JSON and its lock file are not raw artifacts and are
        # the census module's to write, so the assertion is about stdout,
        # protocol streams and engine logs specifically.
        raw = sorted(str(entry) for entry in scratch.root.rglob("*")
                     if entry.is_file()
                     and entry.suffix in {".txt", ".jsonl", ".log"}
                     and scratch.artifacts not in entry.parents)
        expect(raw == [],
               f"no stdout, protocol stream or engine log is written outside "
               f"the harness's own artifact tree ({raw})")
    finally:
        scratch.cleanup()


def test_outcome_identifiers_and_exit_statuses_are_stable() -> None:
    print("\n-- the outcome vocabulary and its exit statuses are the "
          "documented contract")
    documented = {
        "recorded", "no-qualifying-probe", "claim-busy", "resource-busy",
        "selector-error", "claim-audit-failed", "commit-changed",
        "harness-error", "record-failed", "record-indeterminate",
        "recorded-release-failed", "managed-error", "interrupted",
    }
    expect(set(deflake.EXIT_CODES) == documented,
           f"every documented identifier has a status and no others exist "
           f"({sorted(set(deflake.EXIT_CODES) ^ documented)})")
    expect(deflake.SUCCESS_OUTCOMES == {
               "recorded", "no-qualifying-probe", "claim-busy",
               "resource-busy"},
           f"the four success-shaped outcomes are exactly the documented "
           f"ones ({sorted(deflake.SUCCESS_OUTCOMES)})")
    zero = {name for name, code in deflake.EXIT_CODES.items() if code == 0}
    expect(zero == deflake.SUCCESS_OUTCOMES,
           f"and they are precisely the ones that exit 0 ({sorted(zero)})")
    nonzero = {code for name, code in deflake.EXIT_CODES.items()
               if name not in deflake.SUCCESS_OUTCOMES}
    expect(len(nonzero) == len(deflake.EXIT_CODES) - len(zero),
           f"every failing outcome has its OWN status, so a workflow can "
           f"branch on the number as well as the name ({sorted(nonzero)})")
    expect(deflake.EXIT_CODES["interrupted"] == 130,
           "and an interrupt keeps the conventional 130")


def test_the_low_level_harness_needs_no_docs_worktree() -> None:
    print("\n-- the standalone low-level harness stays usable with no docs "
          "worktree and no census claim")
    # The mandatory claim-and-census boundary is /deflake's, not
    # probe_flake's: #1425's own contract is that the harness behaves
    # identically on a checkout with no docs worktree, and putting the
    # boundary here is what preserves it.
    source = Path(TOOLS_DIR, "probe_flake.py").read_text(encoding="utf-8")
    expect("import probe_census" not in source,
           "probe_flake does not import the census")
    expect("import probe_claim" not in source,
           "nor the claim module")
    expect("resolve_docs_worktree" not in source,
           "and never resolves a docs worktree")
    expect(callable(probe_flake.measure) and callable(probe_flake.resolve_probe),
           "its measurement API is importable and callable on its own")


def test_an_interrupt_is_its_own_outcome() -> None:
    print("\n-- an interrupt during the measurement releases ownership and "
          "reports itself")
    scratch = Scratch()
    try:
        claim = FakeClaim()
        result = run(scratch, acquire_claim=lambda probe, **kw: claim,
                     measure=Recorder(raises=KeyboardInterrupt()),
                     record_result=probe_census.record_result_installed)
        expect(result.outcome == deflake.OUTCOME_INTERRUPTED,
               f"the outcome is interrupted ({result.outcome})")
        expect(result.exit_code == 130, f"exit 130 ({result.exit_code})")
        expect(claim.released, "and the claim was released")
    finally:
        scratch.cleanup()


def test_an_unexpected_failure_still_gives_the_claim_back() -> None:
    print("\n-- an unexpected failure releases the claim instead of leaving "
          "the probe unreachable until its lease expires")
    scratch = Scratch()
    try:
        claim = FakeClaim()

        class Boom(Exception):
            pass

        raised = None
        try:
            run(scratch, acquire_claim=lambda probe, **kw: claim,
                measure=Recorder(raises=Boom("a programming error")))
        except Boom as error:
            raised = error
        expect(raised is not None,
               "the unexpected failure is re-raised rather than swallowed "
               "into a documented outcome it is not")
        expect(claim.released,
               "and the claim was given back on the way out, so no other "
               "agent has to wait out a lease for a bug")
    finally:
        scratch.cleanup()


def test_an_interrupt_after_the_claim_reports_what_it_gave_back() -> None:
    print("\n-- an interrupt while the probe is owned reports the ownership "
          "it released")
    scratch = Scratch()
    try:
        claim = FakeClaim()

        def interrupt(*_a, **_kw):
            raise KeyboardInterrupt()

        result = run(scratch, acquire_claim=lambda probe, **kw: claim,
                     record_claim=interrupt,
                     measure=Recorder(measurement(scratch)))
        expect(result.outcome == deflake.OUTCOME_INTERRUPTED,
               f"the outcome is interrupted ({result.outcome}: "
               f"{result.detail})")
        expect(claim.released, "the claim was released")
        expect(result.ownership == deflake.OWNERSHIP_NONE,
               f"and the result says nothing is still owned "
               f"({result.ownership})")
    finally:
        scratch.cleanup()


def test_the_cli_takes_no_probe_or_run_overrides() -> None:
    print("\n-- the command has no forced-probe or run-count override")
    import argparse
    # Read the parser's own registered options rather than its help text:
    # the help text is prose and would pass this test by accident.
    saved = argparse.ArgumentParser.parse_args
    captured: list = []

    def capture(self, args=None, namespace=None):
        captured.append([action.option_strings for action in self._actions])
        raise SystemExit(0)

    argparse.ArgumentParser.parse_args = capture
    try:
        deflake.main([])
    except SystemExit:
        pass
    finally:
        argparse.ArgumentParser.parse_args = saved
    options = {flag for group in (captured[0] if captured else [])
               for flag in group}
    expect("--probe" not in options and "--only" not in options,
           f"no probe can be forced, so a `recorded` outcome always proves "
           f"the selection it came from ({sorted(options)})")
    expect("--runs" not in options and "--rts-caps" not in options,
           f"and the fixed measurement contract cannot be overridden "
           f"({sorted(options)})")



#: This owner's inventory, in the order the aggregate has always run it.
CASES = (
    test_the_harness_is_told_ten_runs_and_four_capabilities,
    test_pass_fail_and_timeout_are_all_valid_observations,
    test_no_qualifying_probe_is_a_successful_no_work_outcome,
    test_deferred_probes_are_never_claimed_or_measured,
    test_a_malformed_census_fails_before_anything_is_claimed,
    test_an_unreachable_census_at_selection_time_is_a_selector_error,
    test_exactly_one_probe_is_selected_per_invocation,
    test_a_lost_selection_to_claim_race_does_nothing_at_all,
    test_a_claim_audit_failure_releases_the_claim_and_runs_nothing,
    test_the_acquisition_token_is_retained_and_reported,
    test_a_claim_lost_before_the_measurement_runs_nothing,
    test_the_claimed_set_keeps_a_held_probe_out_of_selection,
    test_resource_busy_releases_only_the_owned_claim,
    test_a_no_work_outcome_never_reports_retained_ownership,
    test_a_real_foreign_holder_makes_deflake_report_resource_busy,
    test_a_checkout_that_moved_refuses_to_record,
    test_a_result_document_naming_another_commit_refuses_the_cohort,
    test_an_unresolved_commit_never_becomes_a_cohort,
    test_the_cohort_commit_is_captured_once_before_the_runs,
    test_a_harness_error_appends_one_attempt_and_changes_nothing_else,
    test_a_pre_replacement_failure_leaves_the_census_bytes_unchanged,
    test_a_post_replacement_failure_is_indeterminate_and_keeps_the_claim,
    test_a_release_failure_after_a_commit_reports_both_facts,
    test_only_a_valid_measurement_reaches_an_accepted_ingestion,
    test_the_orchestrator_never_manipulates_raw_artifacts,
    test_outcome_identifiers_and_exit_statuses_are_stable,
    test_the_low_level_harness_needs_no_docs_worktree,
    test_an_interrupt_is_its_own_outcome,
    test_an_unexpected_failure_still_gives_the_claim_back,
    test_an_interrupt_after_the_claim_reports_what_it_gave_back,
    test_the_cli_takes_no_probe_or_run_overrides,
)
