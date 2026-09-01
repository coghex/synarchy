#!/usr/bin/env python3
"""Unit tests for the `/deflake` orchestrator (#1436).

Deterministic, engine-free and GPU-free. Every collaborator `deflake`
depends on is a keyword seam, so the whole ordering is asserted with
injected adapters and synthetic state: no probe is ever run, no engine
booted, no port opened, and the developer's own `docs-wip` census is
never touched — every census here is a throwaway file in a temporary
directory.

Two things are deliberately REAL rather than faked, because faking them
would move the assertion off the thing being tested:

* `probe_census` itself. The census claims — a harness error appends one
  non-accepted attempt and changes no cohort, sample or aggregate; a
  pre-replacement failure leaves the authoritative bytes untouched; a
  post-replacement one leaves them CHANGED — are properties of the
  shipped recorder, so they are driven through it against a real
  synthetic census file. The post-replacement case fails the real
  directory fsync inside `_atomic_replace`, which is the only way to
  reach the state it describes.
* `probe_flake.Measurement`. Result documents are built by constructing
  a real measurement out of real `RunRecord`s, so what reaches the
  recorder is what the harness would actually emit and is schema-valid
  for the same reason.

The cross-process resource combinations themselves belong to
`tools/test_probe_resource_lock.py`; this file covers `/deflake`'s
handling of them, plus one end-to-end case against a REAL foreign
holder in a REAL second process, which is the `/deflake`-versus-
`run_probes` conflict direction (`tools/test_run_probes.py` owns the
other).

Usage:
  python3 tools/test_deflake.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import copy
import datetime
import errno
import hashlib
import json
import os
import shutil
import stat
import subprocess
import sys
import tempfile
import textwrap
import time
import uuid
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import deflake  # type: ignore
import probe_census  # type: ignore
import probe_claim  # type: ignore
import probe_engine  # type: ignore
import probe_flake  # type: ignore
import probe_protocol  # type: ignore
import probe_resource_lock  # type: ignore
import probe_select  # type: ignore
import probe_runner_lifecycle  # type: ignore
import probe_runner_resources  # type: ignore

import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402

TOOLS_DIR = str(Path(__file__).resolve().parent)

# Two REGISTERED, manual-only, protocol-compatible probe keys. Real keys
# rather than invented ones because the recorder validates against the
# live registry and refuses a CI-eligible probe -- but only their KEYS
# are used, and nothing here runs either of them.
PROBE = "position_hold"
OTHER = "role"

COMMIT = "a" * 40
OTHER_COMMIT = "b" * 40
NOW = datetime.datetime(2026, 8, 21, 12, 0, tzinfo=datetime.timezone.utc)


# --------------------------------------------------------------------------
# Synthetic state
# --------------------------------------------------------------------------
def selector_inputs(*, keys=(PROBE, OTHER), protocol=None) -> dict:
    """A synthetic registry and both classifications.

    Supplied rather than read from `ci_probes`, for the reason
    `test_probe_select` gives: CLAUDE.md's promotion procedure moves keys
    between the two classifications, and a gate pinned to live
    membership would redden on unrelated registry work.
    """
    statuses = protocol or {key: probe_protocol.PROTOCOL_VERSION for key in keys}
    return {
        "registry": {key: f"{key}_probe.py" for key in keys},
        "ci_eligible": frozenset(),
        "manual_only": frozenset(keys),
        "protocol": statuses,
    }


class Scratch:
    """A temporary census, claim root and artifact tree."""

    def __init__(self) -> None:
        self.root = Path(tempfile.mkdtemp(prefix="test_deflake_"))
        self.census = self.root / "probe_census.json"
        self.claims = self.root / "probe-claims"
        self.claims.mkdir()
        self.artifacts = self.root / "artifacts"
        self.artifacts.mkdir()
        probe_census.ensure_document(self.census)

    def bytes_now(self) -> bytes:
        return self.census.read_bytes()

    def entry(self, probe: str = PROBE) -> dict:
        document = json.loads(self.census.read_text(encoding="utf-8"))
        return probe_census.find_entry(document, probe) or {}

    def census_of(self, probe: str = PROBE) -> dict:
        return (self.entry(probe) or {}).get("census") or {}

    def cleanup(self) -> None:
        shutil.rmtree(self.root, ignore_errors=True)


def measurement(scratch: Scratch, *, probe: str = PROBE, outcomes=None,
                runs: int = deflake.CENSUS_RUN_COUNT,
                rts_caps: int = deflake.RTS_CAPABILITIES,
                commit: str = COMMIT, harness_error: bool = False):
    """A REAL `probe_flake.Measurement`, so its document is a real one."""
    descriptor = probe_protocol.build_descriptor(
        probe, [("alpha", "the only check")])
    invocation = scratch.artifacts / f"invocation-{uuid.uuid4().hex[:8]}"
    invocation.mkdir(parents=True, exist_ok=True)
    result = probe_flake.Measurement(probe, descriptor, runs, rts_caps,
                                     scratch.artifacts, invocation)
    result.commit_sha = commit
    result.timestamp = "2026-08-21T12:00:00Z"
    completed = list(outcomes if outcomes is not None
                     else [probe_flake.RUN_PASS] * runs)
    if harness_error:
        completed = completed[:runs - 1]
    for index, outcome in enumerate(completed, 1):
        run_dir = invocation / f"run-{index:03d}"
        run_dir.mkdir(parents=True, exist_ok=True)
        checks = {"alpha": {probe_flake.RUN_PASS: probe_protocol.PASS,
                            probe_flake.RUN_FAIL: probe_protocol.FAIL,
                            probe_flake.RUN_TIMEOUT: probe_protocol.MISSING,
                            }[outcome]}
        result.runs.append(probe_flake.RunRecord(
            index, 9100 + index, outcome, 1.5, checks, run_dir))
    if harness_error:
        error_dir = invocation / f"run-{runs:03d}"
        error_dir.mkdir(parents=True, exist_ok=True)
        result.status = "harness-error"
        result.error = f"run {runs}: the event stream could not be trusted"
        result.error_run = probe_flake.RunRecord(
            runs, 9100 + runs, "HARNESS_ERROR", 0.5, {}, error_dir)
    return result


class FakeClaim:
    """A `probe_claim.Claim` stand-in with the same ownership surface."""

    def __init__(self, probe: str = PROBE, *, token: str = "token-1",
                 lose_on: str = "", release_error: str = "") -> None:
        self.probe = probe
        self.token = token
        # Read by the REAL `probe_claim.Renewer`, which the orchestrator
        # runs around the measurement: the renewer is not faked, so a
        # fake claim has to answer the same lease question a real one
        # does. Long enough that its first tick never fires inside a test.
        self.lease_seconds = probe_claim.MIN_ORCHESTRATION_LEASE_SECONDS
        self.lose_on = lose_on
        self.release_error = release_error
        self.released = False
        self.commits: list = []
        self.reasserted = 0
        self.audits: list = []

    def census_record(self, *, commit_sha: str, requested_runs: int) -> dict:
        self.audits.append((commit_sha, requested_runs))
        return {"token": self.token, "timestamp_utc": "2026-08-21T12:00:00Z",
                "commit_sha": commit_sha, "owner": "selftest",
                "host": "selftest", "pid": os.getpid(),
                "lease_seconds": 1800.0, "requested_runs": requested_runs}

    def commit_while_held(self, commit, now=None):
        stage = "audit" if not self.commits else "record"
        self.commits.append(stage)
        if self.lose_on == stage:
            raise probe_claim.ClaimLost(
                f"the claim on {self.probe!r} is no longer ours")
        return commit()

    def reassert(self, now=None) -> None:
        self.reasserted += 1
        if self.lose_on == "reassert":
            raise probe_claim.ClaimLost(
                f"the claim on {self.probe!r} is no longer ours")

    def release(self) -> bool:
        if self.release_error:
            raise probe_claim.ClaimError(self.release_error)
        self.released = True
        return True


class Recorder:
    """Records what the measure seam was handed, and returns a measurement."""

    def __init__(self, result=None, raises=None) -> None:
        self.result = result
        self.raises = raises
        self.calls: list = []

    def __call__(self, probe, runs, *, artifact_root=None, rts_caps=None,
                 timeout=None, start_port=None, announce=None):
        self.calls.append({"probe": probe, "runs": runs,
                           "artifact_root": artifact_root,
                           "rts_caps": rts_caps, "timeout": timeout,
                           "start_port": start_port})
        if self.raises is not None:
            raise self.raises
        return self.result


ARGV = ["python3", "tools/deflake.py", "--json"]
CWD = "/private/tmp/synarchy-selftest-checkout"


def installed_census(*, probe: str = PROBE, acceptable: int = 0) -> dict:
    """A census document shaped like the one `update` installs.

    Only the fields the handoff reads: the row for the probe and its
    acceptable-failure count. `record_result_installed` hands the whole
    installed candidate back, and this is the part of it that matters
    here.
    """
    return {"schema": probe_census.CENSUS_SCHEMA,
            "probes": [{"key": probe, "classification": "manual-only",
                        "script": f"{probe}_probe.py", "protocol": "legacy",
                        "census": {"acceptable_failures": acceptable}}]}


def held_resources(probe, *, namespace=None, repo_root=None):
    """An injected resource hold that owns nothing and records nothing."""
    return probe_resource_lock.ResourceHold("selftest", frozenset(),
                                            frozenset(), {}, None)


#: What the preparation seam answers with when a case does not care.
#: Every case needs one: the real seam shells out to Cabal, and a suite
#: that is engine-free and toolchain-free must never reach it by
#: forgetting to substitute.
PREPARED_ENGINE = "/private/tmp/synarchy-selftest-checkout/synarchy"


class Preparer:
    """Records how the engine preparation seam was called."""

    def __init__(self, executable: str = PREPARED_ENGINE, raises=None,
                 observe=None) -> None:
        self.executable = executable
        self.raises = raises
        self.observe = observe
        self.calls: list = []

    def __call__(self, *, namespace, repo_root=None, announce=None):
        self.calls.append({"namespace": namespace, "repo_root": repo_root})
        if self.observe is not None:
            self.observe()
        if self.raises is not None:
            raise self.raises
        return self.executable


class saved_runner_executable:
    """Restore `probe_runner_resources.ENGINE_EXECUTABLE`, which `deflake` installs.

    It is a module global the runner reads when it hands a child probe
    its executable, so a case that leaves it set would decide what a
    later case observes.
    """

    def __enter__(self):
        self._saved = probe_runner_resources.ENGINE_EXECUTABLE
        return self

    def __exit__(self, *exc):
        probe_runner_resources.ENGINE_EXECUTABLE = self._saved
        return False


def run(scratch: Scratch, **overrides):
    """`measure_next_probe` with every seam defaulted to a safe fake."""
    settings = {
        "census_path": scratch.census,
        "claim_root": scratch.claims,
        "artifact_root": scratch.artifacts,
        "now": NOW,
        "inputs": selector_inputs(),
        "acquire_claim": lambda probe, **kw: FakeClaim(probe),
        "acquire_resources": held_resources,
        "prepare_engine": Preparer(),
        "measure": Recorder(),
        "record_claim": lambda *a, **kw: PROBE,
        # The recording seam now answers with the census row the
        # transaction INSTALLED as well as the probe (#1659), because
        # the handoff's acceptable-failure count has to come from that
        # document rather than from a reread after the lock is gone.
        "record_result": lambda *a, **kw: (PROBE, installed_census()),
        "head_commit": lambda: COMMIT,
        "argv": ARGV,
        "cwd": CWD,
        "read_configuration": lambda root: [],
    }
    settings.update(overrides)
    return deflake.measure_next_probe(**settings)


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


# --------------------------------------------------------------------------
# The handoff (#1659)
# --------------------------------------------------------------------------
class Raiser:
    """A recording seam that fails the way one real failure fails."""

    def __init__(self, error) -> None:
        self.error = error

    def __call__(self, *args, **kwargs):
        raise self.error


def written_handoff(scratch: Scratch, **overrides):
    """Run to `recorded` and return `(result, handoff_document_or_None)`."""
    kept: dict = {}

    def save(path, document):
        kept[str(path)] = document
        return None

    settings = {"measure": Recorder(measurement(scratch)),
                "save_handoff": save}
    settings.update(overrides)
    result = run(scratch, **settings)
    if result.handoff_path is None:
        return result, None
    return result, kept.get(str(result.handoff_path))


def test_a_recorded_measurement_writes_its_handoff_beside_the_result() -> None:
    print("\n-- a recorded measurement writes a handoff beside its result")
    scratch = Scratch()
    try:
        result, document = written_handoff(scratch)
        expect(result.outcome == deflake.OUTCOME_RECORDED,
               f"({result.outcome})")
        expect(document is not None, "a handoff was written")
        expect(result.handoff_path is not None
               and Path(result.handoff_path).parent
               == Path(result.result_path).parent,
               f"beside the retained result ({result.handoff_path})")
        expect(str(result.handoff_path)
               == f"{result.result_path}{deflake.HANDOFF_SUFFIX}",
               f"named after it, so two results in one directory cannot "
               f"collide ({result.handoff_path})")
        expect(result.to_document()["handoff_document"]
               == str(result.handoff_path),
               "and the machine-readable outcome reports the path")
        expect(set(document) == {"schema", "probe", "acceptable_failures",
                                 "targets", "result", "invocation",
                                 "configuration", "artifacts"},
               f"the document has exactly the declared keys ({sorted(document)})")
        expect(document["schema"] == deflake.HANDOFF_SCHEMA,
               f"({document['schema']})")
    finally:
        scratch.cleanup()


def test_two_results_in_one_directory_get_two_handoffs() -> None:
    print("\n-- the handoff name is derived injectively from the result's")
    directory = Path("/tmp/one-directory")
    # `Path.stem` would map all three of these to `census`, so the later
    # measurement's handoff would overwrite the earlier one's.
    names = ["census.json", "census.txt", "census", "census.json.bak"]
    produced = [deflake.handoff_path_for(directory / name) for name in names]
    expect(len(set(produced)) == len(names),
           f"distinct results give distinct handoffs ({produced})")
    expect(all(path.parent == directory for path in produced),
           f"all beside their own result ({produced})")
    expect(all(path.name.endswith(deflake.HANDOFF_SUFFIX)
               for path in produced),
           f"and all recognisable as handoffs ({produced})")
    expect(deflake.handoff_path_for(directory / "census.json").name
           == f"census.json{deflake.HANDOFF_SUFFIX}",
           "derived from the WHOLE filename, not its stem")


def test_the_embedded_result_is_the_measurements_own_document() -> None:
    print("\n-- the result is embedded unchanged, not summarised")
    scratch = Scratch()
    try:
        real = measurement(scratch)
        result, document = written_handoff(scratch, measure=Recorder(real))
        expect(document["result"] == real.to_document(),
               "the embedded result is byte-for-byte the harness's own")
        expect(set(document["result"]) == set(real.to_document()),
               "and gains no field, because `probe-flake-result/v1` "
               "rejects additional properties")
        expect(document["probe"] == PROBE, f"({document['probe']})")
        expect(document["artifacts"] == real.retained_artifacts(),
               f"every retained path is named ({document['artifacts']})")
    finally:
        scratch.cleanup()


def test_the_invocation_records_what_the_process_observed() -> None:
    print("\n-- the invocation records argv, cwd, ports and both defaults")
    scratch = Scratch()
    try:
        real = measurement(scratch)
        result, document = written_handoff(scratch, measure=Recorder(real))
        invocation = document["invocation"]
        expect(set(invocation) == {"argv", "cwd", "retries", "ports",
                                   "timeout", "start_port"},
               f"exactly the declared keys ({sorted(invocation)})")
        expect(invocation["argv"] == ARGV,
               f"the observed argv, argv[0] included ({invocation['argv']})")
        expect(invocation["cwd"] == CWD, f"({invocation['cwd']})")
        expect(invocation["retries"] == 0,
               "the retry policy is 0 and this lab has no other")
        expect(invocation["ports"]
               == [run["port"] for run in real.to_document()["runs"]],
               f"the ordered BASE port of each completed run "
               f"({invocation['ports']})")
        expect(invocation["timeout"] == deflake.TIMEOUT
               and invocation["start_port"] == deflake.START_PORT,
               f"and the two settings passed to the adapter "
               f"({invocation['timeout']}, {invocation['start_port']})")
    finally:
        scratch.cleanup()


def test_the_adapter_is_told_the_timeout_and_starting_port() -> None:
    print("\n-- both are passed EXPLICITLY, like the run and capability counts")
    scratch = Scratch()
    try:
        recorder = Recorder(measurement(scratch))
        run(scratch, measure=recorder, save_handoff=lambda p, d: None)
        expect(len(recorder.calls) == 1, f"({recorder.calls})")
        call = recorder.calls[0]
        expect(call["timeout"] == deflake.TIMEOUT,
               f"the timeout is supplied, not defaulted ({call['timeout']})")
        expect(call["start_port"] == deflake.START_PORT,
               f"and so is the starting port ({call['start_port']})")
        expect(deflake.TIMEOUT == probe_flake.DEFAULT_TIMEOUT
               and deflake.START_PORT == probe_flake.PORT_MIN,
               "each equal to the harness's own value, supplied rather "
               "than relied on")
    finally:
        scratch.cleanup()


def test_the_targets_are_the_non_pass_identifiers_in_descriptor_order() -> None:
    print("\n-- targets: once each, FAIL or MISSING, in the declared order")
    scratch = Scratch()
    try:
        # Declared gamma, alpha, beta — deliberately not alphabetical, so
        # sorting the identifiers and reading the descriptor give
        # DIFFERENT answers and only one of them is the contract.
        descriptor = probe_protocol.build_descriptor(
            PROBE, [("gamma", "first"), ("alpha", "second"),
                    ("beta", "third")])
        invocation = scratch.artifacts / "multi"
        invocation.mkdir(parents=True, exist_ok=True)
        real = probe_flake.Measurement(
            PROBE, descriptor, deflake.CENSUS_RUN_COUNT,
            deflake.RTS_CAPABILITIES, scratch.artifacts, invocation)
        real.commit_sha = COMMIT
        real.timestamp = "2026-08-21T12:00:00Z"
        # gamma fails twice and beta goes MISSING once; alpha never
        # fails. Declared order gives [gamma, beta]; sorting would give
        # [beta, gamma], and repeating per run would give gamma twice.
        maps = [{"gamma": probe_protocol.FAIL, "alpha": probe_protocol.PASS,
                 "beta": probe_protocol.PASS},
                {"gamma": probe_protocol.MISSING, "alpha": probe_protocol.PASS,
                 "beta": probe_protocol.MISSING},
                {"gamma": probe_protocol.FAIL, "alpha": probe_protocol.PASS,
                 "beta": probe_protocol.PASS}]
        maps += [{"gamma": probe_protocol.PASS, "alpha": probe_protocol.PASS,
                  "beta": probe_protocol.PASS}] * 7
        for index, checks in enumerate(maps, 1):
            outcome = (probe_flake.RUN_PASS
                       if all(v == probe_protocol.PASS for v in checks.values())
                       else probe_flake.RUN_FAIL)
            run_dir = invocation / f"run-{index:03d}"
            run_dir.mkdir(parents=True, exist_ok=True)
            real.runs.append(probe_flake.RunRecord(
                index, 9100 + index, outcome, 1.5, checks,
                run_dir if outcome != probe_flake.RUN_PASS else None))
        _result, document = written_handoff(scratch, measure=Recorder(real))
        expect(document["targets"] == ["gamma", "beta"],
               f"once each, in the DECLARED order — sorting would answer "
               f"['beta', 'gamma'] ({document['targets']})")
        expect(deflake.handoff_targets(real.to_document()) == ["gamma", "beta"],
               "and the helper answers the same alone")
        expect(deflake.handoff_targets(measurement(scratch).to_document()) == [],
               "a batch that passed everywhere targets nothing")
    finally:
        scratch.cleanup()


def test_the_configuration_manifest_records_contents_and_absence() -> None:
    print("\n-- the config manifest: every family member, or an empty list")
    root = Path(tempfile.mkdtemp(prefix="test_deflake_config_"))
    try:
        (root / "config").mkdir()
        expect(deflake.configuration_manifest(root) == [],
               "an absent family is an EMPTY LIST, stated positively")

        # Created in reverse-alphabetical order, so directory order and
        # sorted order are different answers.
        for name in ("video.local.yaml", "save.local.yaml",
                     "notifications.local.yaml", "keybinds.local.yaml"):
            (root / "config" / name).write_text(f"{name}: true\n")
        (root / "config" / "ignored.yaml").write_text("not in the family\n")
        (root / "config" / "nested").mkdir()
        entries = deflake.configuration_manifest(root)
        paths = [entry["path"] for entry in entries]
        expect(paths == ["config/keybinds.local.yaml",
                         "config/notifications.local.yaml",
                         "config/save.local.yaml",
                         "config/video.local.yaml"],
               f"only the gitignored family, SORTED by path ({paths})")
        expect(paths == sorted(paths), f"explicitly ({paths})")
        expect(all(set(entry) == {"path", "sha256"} for entry in entries),
               f"exactly two keys per entry ({entries})")
        digest = hashlib.sha256(
            (root / "config" / "keybinds.local.yaml").read_bytes()).hexdigest()
        expect(entries[0]["sha256"] == digest and digest == digest.lower(),
               "the lowercase SHA-256 of the bytes actually read")
    finally:
        shutil.rmtree(root, ignore_errors=True)


def test_the_configuration_is_read_under_the_hold_before_the_runs() -> None:
    print("\n-- the manifest describes what the runs read, not what followed")
    scratch = Scratch()
    try:
        order: list = []
        recorder = Recorder(measurement(scratch))

        def measure(*args, **kwargs):
            order.append("measure")
            return recorder(*args, **kwargs)

        def read_configuration(root):
            order.append("configuration")
            return [{"path": "config/save.local.yaml", "sha256": "a" * 64}]

        _result, document = written_handoff(
            scratch, measure=measure, read_configuration=read_configuration)
        expect(order == ["configuration", "measure"],
               f"captured before the first engine ({order})")
        expect(document["configuration"]
               == [{"path": "config/save.local.yaml", "sha256": "a" * 64}],
               f"and carried verbatim ({document['configuration']})")
    finally:
        scratch.cleanup()


def test_a_configuration_that_cannot_be_read_is_a_managed_failure() -> None:
    print("\n-- capturing the configuration OPENS files, and that can fail")
    scratch = Scratch()
    try:
        recorder = Recorder(measurement(scratch))
        claim = FakeClaim()
        attempts: list = []

        def unreadable(root):
            raise PermissionError(13, "Permission denied",
                                  "config/save.local.yaml")

        result = run(scratch, measure=recorder,
                     acquire_claim=lambda probe, **kw: claim,
                     read_configuration=unreadable,
                     save_handoff=lambda path, document: attempts.append(path))
        expect(result.outcome == deflake.OUTCOME_MANAGED_ERROR,
               f"it is a managed outcome, not a traceback ({result.outcome})")
        expect(result.exit_code != 0, f"and nonzero ({result.exit_code})")
        expect("could not be captured" in (result.detail or ""),
               f"naming what failed ({result.detail})")
        expect(recorder.calls == [],
               f"no engine was started ({recorder.calls})")
        expect(attempts == [] and result.handoff_path is None
               and result.to_document()["handoff_document"] is None,
               "and there is no handoff, because nothing was measured")
        expect(claim.released and result.ownership == deflake.OWNERSHIP_NONE,
               f"the claim went back ({result.ownership})")

        # And when the release ALSO fails, the retained ownership is the
        # result rather than a footnote on it — the same rule every
        # other pre-measurement failure follows.
        held = FakeClaim(release_error="the claim file is unwritable")
        result = run(scratch, measure=Recorder(measurement(scratch)),
                     acquire_claim=lambda probe, **kw: held,
                     read_configuration=unreadable)
        expect(result.outcome == deflake.OUTCOME_MANAGED_ERROR
               and result.ownership == deflake.OWNERSHIP_CLAIM_HELD,
               f"({result.outcome}, {result.ownership})")

        # The shipped reader OPENS each file and swallows nothing, which
        # is what makes the injected one a fair stand-in. Asserted by
        # reading the source rather than by denying permission: CI runs
        # as root, where a mode of 0 stops nothing, and a test that
        # quietly skipped there would be coverage only on a laptop.
        source = Path(deflake.__file__).read_text(encoding="utf-8")
        body = source[source.index("def configuration_manifest"):
                      source.index("def handoff_targets")]
        expect("open(path, \"rb\")" in body,
               "the reader opens each file, so it can fail like one")
        expect("except" not in body,
               f"and catches nothing, so the caller sees the OSError")
    finally:
        scratch.cleanup()


def test_the_acceptable_failure_count_comes_from_the_installed_row() -> None:
    print("\n-- X comes from the row the transaction wrote, not a reread")
    scratch = Scratch()
    try:
        _result, document = written_handoff(
            scratch,
            record_result=lambda *a, **kw: (PROBE,
                                            installed_census(acceptable=3)))
        expect(document["acceptable_failures"] == 3,
               f"({document['acceptable_failures']})")

        # The lock is released when the recorder returns, so a row that
        # names no X is a refusal rather than a reread.
        result = run(scratch, measure=Recorder(measurement(scratch)),
                     record_result=lambda *a, **kw: (PROBE, {"probes": []}))
        expect(result.outcome == deflake.OUTCOME_MANAGED_ERROR,
               f"a row with no acceptable-failure count refuses "
               f"({result.outcome})")
        expect(result.handoff_path is None
               and result.to_document()["handoff_document"] is None,
               "and writes no handoff")
    finally:
        scratch.cleanup()


def test_a_recorder_that_answers_the_wrong_shape_is_refused() -> None:
    print("\n-- the recording seam answers (probe, installed census)")
    scratch = Scratch()
    try:
        for label, answer in (("only the probe key", PROBE),
                              ("a one-element tuple", (PROBE,)),
                              ("a probe and a string", (PROBE, "census")),
                              ("nothing at all", None)):
            result = run(scratch, measure=Recorder(measurement(scratch)),
                         record_result=lambda *a, _v=answer, **kw: _v)
            expect(result.outcome == deflake.OUTCOME_MANAGED_ERROR,
                   f"a seam answering {label} is refused ({result.outcome})")
            expect("not the probe and the census document"
                   in (result.detail or ""),
                   f"by name, rather than raising from inside a committed "
                   f"transaction ({result.detail})")
            expect(result.handoff_path is None, "and no handoff is written")
    finally:
        scratch.cleanup()


def test_only_the_recorded_outcome_has_a_handoff() -> None:
    print("\n-- every other post-measurement outcome writes none")
    scratch = Scratch()
    try:
        attempts: list = []

        def save(path, document):
            attempts.append(str(path))
            return None

        cases = {
            deflake.OUTCOME_HARNESS_ERROR: dict(
                measure=Recorder(measurement(scratch, harness_error=True))),
            deflake.OUTCOME_COMMIT_CHANGED: dict(
                measure=Recorder(measurement(scratch)),
                head_commit=lambda: OTHER_COMMIT),
            deflake.OUTCOME_RECORD_FAILED: dict(
                measure=Recorder(measurement(scratch)),
                record_result=Raiser(probe_census.CensusError("no"))),
            deflake.OUTCOME_RECORD_INDETERMINATE: dict(
                measure=Recorder(measurement(scratch)),
                record_result=Raiser(
                    probe_census.CensusDurabilityUnconfirmed(
                    "the directory fsync failed", target="census",
                    error=OSError("fsync")))),
            deflake.OUTCOME_RECORDED_RELEASE_FAILED: dict(
                measure=Recorder(measurement(scratch)),
                acquire_claim=lambda probe, **kw: FakeClaim(
                    probe, release_error="the claim file is unwritable")),
        }
        for expected, overrides in cases.items():
            attempts.clear()
            result = run(scratch, save_handoff=save, **overrides)
            expect(result.outcome == expected,
                   f"{expected} reached ({result.outcome})")
            expect(result.handoff_path is None,
                   f"{expected} reports no handoff ({result.handoff_path})")
            expect(result.to_document()["handoff_document"] is None,
                   f"{expected} nulls the field rather than omitting it")
            expect(attempts == [],
                   f"{expected} did not even attempt a write ({attempts})")
    finally:
        scratch.cleanup()


def test_a_recorded_measurement_with_nothing_retained_cannot_hand_off() -> None:
    print("\n-- `recorded` does not guarantee a retained result to sit beside")
    scratch = Scratch()
    try:
        real = measurement(scratch)
        real.invocation_dir = None
        # An ancestor that is a regular FILE, so `mkdir(parents=True)`
        # raises `NotADirectoryError` for every user. A path under a
        # nonexistent root would not do: CI runs as root in a container,
        # where `/nonexistent/dir/...` is perfectly creatable, and the
        # measurement would be retained after all.
        blocker = scratch.root / "a-regular-file"
        blocker.write_text("not a directory\n", encoding="utf-8")
        result = run(scratch, measure=Recorder(real),
                     result_path=str(blocker / "sub" / "r.json"))
        expect(result.outcome == deflake.OUTCOME_MANAGED_ERROR,
               f"({result.outcome})")
        expect(result.result_path is None,
               f"nothing was retained ({result.result_path})")
        expect(result.handoff_path is None
               and result.to_document()["handoff_document"] is None,
               "so there is no handoff")
        expect("WAS recorded in the census" in (result.detail or ""),
               f"and the detail says the census update stands ({result.detail})")
        expect(result.ownership == deflake.OWNERSHIP_NONE,
               f"the claim was already released ({result.ownership})")
    finally:
        scratch.cleanup()


def test_a_handoff_that_cannot_be_written_is_a_managed_error() -> None:
    print("\n-- the census update stands, and is neither retried nor rolled back")
    scratch = Scratch()
    try:
        result = run(scratch, measure=Recorder(measurement(scratch)),
                     record_result=probe_census.record_result_installed,
                     save_handoff=lambda path, document: "the disk is full")
        expect(result.outcome == deflake.OUTCOME_MANAGED_ERROR,
               f"({result.outcome})")
        expect(result.exit_code != 0, f"and nonzero ({result.exit_code})")
        expect(result.handoff_path is None
               and result.to_document()["handoff_document"] is None,
               "the handoff field is null")
        expect("the disk is full" in (result.detail or "")
               and "WAS recorded in the census" in (result.detail or ""),
               f"both facts are reported ({result.detail})")
        expect(result.ownership == deflake.OWNERSHIP_NONE,
               f"and nothing is still owned ({result.ownership})")
        cohort = scratch.census_of()
        expect(cohort.get("current") is not None,
               "the committed census update was left exactly as it was")
    finally:
        scratch.cleanup()


def test_the_real_writer_produces_a_readable_document() -> None:
    print("\n-- the shipped writer, against a real directory")
    scratch = Scratch()
    try:
        target = scratch.root / "beside" / "result-handoff.json"
        document = {"schema": deflake.HANDOFF_SCHEMA, "probe": PROBE}
        expect(deflake.write_handoff(target, document) is None,
               "it writes, creating the directory")
        expect(json.loads(target.read_text(encoding="utf-8")) == document,
               "and the bytes read back as the document")
        problem = deflake.write_handoff(scratch.root / "beside", document)
        expect(problem is not None and "could not write" in problem,
               f"an unwritable target is a reported problem, not a "
               f"traceback ({problem})")
        # Here the staging file WAS created and the rename is what
        # failed, so this is the case that proves it gets cleaned up.
        expect(not (scratch.root / "beside.partial").exists(),
               f"and the staging file it created is removed "
               f"({sorted(path.name for path in scratch.root.iterdir())})")

        # A failed write leaves NO handoff: only `recorded` may leave
        # one, and a partial file beside the result would tell a later
        # consumer it had a complete measurement when it does not.
        unserialisable = scratch.root / "beside" / "bad-handoff.json"
        unserialisable.parent.mkdir(parents=True, exist_ok=True)
        unserialisable.write_text("PRIOR CONTENT\n", encoding="utf-8")
        problem = deflake.write_handoff(unserialisable, {"runs": {1, 2}})
        expect(problem is not None and "could not serialize" in problem,
               f"a document json cannot encode is reported ({problem})")
        expect(unserialisable.read_text(encoding="utf-8") == "PRIOR CONTENT\n",
               "and the target was never even opened, let alone truncated")

        # A write whose own I/O fails, with an existing handoff in place:
        # an ancestor that is a regular FILE refuses `mkdir` for every
        # user, so this holds in a root CI container too — a chmod would
        # not, which is exactly how the first version of this suite
        # passed locally and failed in CI.
        blocker = scratch.root / "blocking-file"
        blocker.write_text("not a directory\n", encoding="utf-8")
        problem = deflake.write_handoff(blocker / "deep" / "x-handoff.json",
                                        document)
        expect(problem is not None and "could not write" in problem,
               f"an uncreatable target is reported ({problem})")
        expect(blocker.read_text(encoding="utf-8") == "not a directory\n",
               "and nothing on the way to it was disturbed")
    finally:
        scratch.cleanup()


# --------------------------------------------------------------------------
# The engine is prepared BEFORE the measurement's hold (#1913)
#
# A probe that was handed no executable prepares its own, and that
# preparation takes `cabal-build` EXCLUSIVELY. A measurement holds the
# same resource — shared for an ordinary probe, exclusive for the three
# that drive Cabal themselves — for the whole of its runs, and
# `probe_runner_lifecycle.run_one` strips the inherited runner variables on the way
# down, so a child could neither see that hold nor upgrade past it. It
# would wait out its whole allowance for a holder blocked waiting on it.
#
# The fix is ordering, and these cases pin the ordering rather than the
# absence of a hang.
# --------------------------------------------------------------------------
FAKE_CABAL_SRC = """\
#!/usr/bin/env python3
import json, sys
from pathlib import Path
config = json.loads(Path(__file__).with_name("cabal.json").read_text())
with open(config["calls"], "a") as fh:
    fh.write(" ".join(sys.argv[1:]) + "\\n")
step = sys.argv[1] if len(sys.argv) > 1 else ""
if step == "build":
    sys.exit(0)
if step == "list-bin":
    print(config["engine"])
    sys.exit(0)
sys.exit(9)
"""


def test_the_engine_is_prepared_before_the_resource_hold() -> None:
    print("\n-- the engine is prepared before the measurement takes its hold")
    scratch = Scratch()
    order: list[str] = []
    try:
        with saved_runner_executable():
            probe_runner_resources.ENGINE_EXECUTABLE = None
            prepare = Preparer(observe=lambda: order.append("prepare"))

            def take(probe, *, namespace=None, repo_root=None):
                order.append("resources")
                return held_resources(probe, namespace=namespace,
                                      repo_root=repo_root)

            class Watching(Recorder):
                def __call__(self, *args, **kwargs):
                    order.append("measure")
                    self.seen_executable = probe_runner_resources.ENGINE_EXECUTABLE
                    return super().__call__(*args, **kwargs)

            measure = Watching(measurement(scratch))
            result = run(scratch, prepare_engine=prepare, acquire_resources=take,
                         measure=measure,
                         record_result=probe_census.record_result_installed)
            expect(result.outcome == deflake.OUTCOME_RECORDED,
                   f"the measurement is recorded ({result.outcome}: "
                   f"{result.detail})")
            expect(order == ["prepare", "resources", "measure"],
                   f"and preparation precedes the hold, which precedes the "
                   f"runs (got {order})")
            expect(len(prepare.calls) == 1,
                   f"one preparation for the whole measurement, not one per "
                   f"run (got {prepare.calls})")
            expect(getattr(measure, "seen_executable", None)
                   == PREPARED_ENGINE,
                   f"and the resolved path is already installed as the "
                   f"runner's executable when the runs start, so no child "
                   f"prepares its own (got "
                   f"{getattr(measure, 'seen_executable', None)!r})")
    finally:
        scratch.cleanup()


def test_preparation_and_the_hold_resolve_one_namespace() -> None:
    print("\n-- both name the same namespace, so they cannot lock past "
          "each other")
    scratch = Scratch()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    seen: list = []
    try:
        with saved_runner_executable():
            prepare = Preparer()

            def take(probe, *, namespace=None, repo_root=None):
                seen.append(namespace)
                return held_resources(probe, namespace=namespace)

            run(scratch, prepare_engine=prepare, acquire_resources=take,
                namespace=namespace, measure=Recorder(measurement(scratch)),
                record_result=probe_census.record_result_installed)
            expect(prepare.calls
                   and prepare.calls[0]["namespace"] == namespace,
                   f"preparation is told the measurement's own namespace "
                   f"(got {prepare.calls})")
            expect(seen == [namespace],
                   f"which is the one the hold is taken in (got {seen})")
            expect(deflake._probe_resource_namespace(namespace=namespace)
                   == namespace,
                   "resolved through one helper, so a future edit cannot "
                   "make the two disagree")
    finally:
        scratch.cleanup()


def test_a_preparation_failure_runs_nothing_and_gives_the_claim_back() -> None:
    print("\n-- a preparation that fails releases the claim and measures "
          "nothing")
    scratch = Scratch()
    try:
        with saved_runner_executable():
            claim = FakeClaim()
            measure = Recorder(measurement(scratch))
            held: list = []
            result = run(scratch, acquire_claim=lambda probe, **kw: claim,
                         prepare_engine=Preparer(raises=(
                             probe_engine.EnginePreparationError(
                                 "the engine executable could not be "
                                 "prepared: `cabal build exe:synarchy` "
                                 "failed with exit status 1"))),
                         acquire_resources=lambda *a, **kw: held.append(1),
                         measure=measure)
            expect(result.outcome == deflake.OUTCOME_MANAGED_ERROR,
                   f"the outcome is a managed error ({result.outcome}: "
                   f"{result.detail})")
            expect("could not be prepared" in result.detail,
                   f"naming preparation and carrying Cabal's reason "
                   f"(got {result.detail!r})")
            expect(held == [], "no resource hold was taken")
            expect(measure.calls == [], "and nothing was measured")
            expect(claim.released, "while the claim went back")
            expect(result.ownership == deflake.OWNERSHIP_NONE,
                   f"so the invocation owns nothing (got {result.ownership})")
    finally:
        scratch.cleanup()


def test_the_real_preparation_runs_outside_the_real_hold() -> None:
    print("\n-- the REAL preparation and the REAL hold, in one namespace, "
          "without deadlocking")
    scratch = Scratch()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    engine = scratch.root / "fake-synarchy"
    engine.write_text("#!/bin/sh\nexit 0\n")
    engine.chmod(0o755)
    calls = scratch.root / "cabal-calls.txt"
    (scratch.root / "cabal.json").write_text(
        json.dumps({"calls": str(calls), "engine": str(engine)}))
    cabal = scratch.root / "cabal"
    cabal.write_text(FAKE_CABAL_SRC)
    cabal.chmod(0o755)
    saved_path = os.environ.get("PATH", "")
    conflicts: list = []
    try:
        os.environ["PATH"] = f"{scratch.root}{os.pathsep}{saved_path}"
        with saved_runner_executable():
            probe_runner_resources.ENGINE_EXECUTABLE = None

            class Watching(Recorder):
                def __call__(self, *args, **kwargs):
                    # Inside the measurement's hold: an exclusive
                    # `cabal-build` interest — which is exactly what a
                    # child probe's own preparation would take — is
                    # refused. That is the deadlock this ordering
                    # avoids, observed rather than assumed.
                    self.seen_executable = probe_runner_resources.ENGINE_EXECUTABLE
                    try:
                        probe_resource_lock.acquire(
                            exclusive={probe_engine.BUILD_RESOURCE},
                            namespace=namespace,
                            purpose="test_deflake would-be child").release()
                    except probe_resource_lock.ResourceBusy as busy:
                        conflicts.append(busy.resource)
                    return super().__call__(*args, **kwargs)

            measure = Watching(measurement(scratch))
            started = time.monotonic()
            result = run(scratch, acquire_resources=deflake._acquire_probe_resources,
                         prepare_engine=deflake._prepare_probe_engine,
                         namespace=namespace, measure=measure,
                         record_result=probe_census.record_result_installed)
            elapsed = time.monotonic() - started
            expect(result.outcome == deflake.OUTCOME_RECORDED,
                   f"the measurement completes ({result.outcome}: "
                   f"{result.detail})")
            expect(elapsed < 60.0,
                   f"promptly, rather than sitting out a preparation "
                   f"allowance (took {elapsed:.1f} s)")
            recorded = [line for line in
                        (calls.read_text().splitlines() if calls.exists()
                         else []) if line]
            expect(recorded == ["build exe:synarchy", "list-bin exe:synarchy"],
                   f"having really built, exactly once, through the real "
                   f"preparation (got {recorded})")
            expect(conflicts == [probe_engine.BUILD_RESOURCE],
                   f"and the hold covering the runs really does exclude a "
                   f"would-be child preparation, so the ordering is what "
                   f"makes this work (got {conflicts})")
            expect(getattr(measure, "seen_executable", None) == str(engine),
                   f"which is why the child needs none: it is handed the "
                   f"prepared path (got "
                   f"{getattr(measure, 'seen_executable', None)!r})")
    finally:
        os.environ["PATH"] = saved_path
        for entry in probe_resource_lock.LOCK_ROOT.glob(
                f"{probe_resource_lock.SHARED_PREFIX}-{namespace}-*"):
            try:
                entry.unlink()
            except OSError:
                pass
        scratch.cleanup()


def main() -> int:
    selftestlib.parse_verbose()
    test_the_harness_is_told_ten_runs_and_four_capabilities()
    test_pass_fail_and_timeout_are_all_valid_observations()
    test_no_qualifying_probe_is_a_successful_no_work_outcome()
    test_deferred_probes_are_never_claimed_or_measured()
    test_a_malformed_census_fails_before_anything_is_claimed()
    test_an_unreachable_census_at_selection_time_is_a_selector_error()
    test_exactly_one_probe_is_selected_per_invocation()
    test_a_lost_selection_to_claim_race_does_nothing_at_all()
    test_a_claim_audit_failure_releases_the_claim_and_runs_nothing()
    test_the_acquisition_token_is_retained_and_reported()
    test_a_claim_lost_before_the_measurement_runs_nothing()
    test_the_claimed_set_keeps_a_held_probe_out_of_selection()
    test_resource_busy_releases_only_the_owned_claim()
    test_a_no_work_outcome_never_reports_retained_ownership()
    test_a_real_foreign_holder_makes_deflake_report_resource_busy()
    test_a_checkout_that_moved_refuses_to_record()
    test_a_result_document_naming_another_commit_refuses_the_cohort()
    test_an_unresolved_commit_never_becomes_a_cohort()
    test_the_cohort_commit_is_captured_once_before_the_runs()
    test_a_harness_error_appends_one_attempt_and_changes_nothing_else()
    test_a_pre_replacement_failure_leaves_the_census_bytes_unchanged()
    test_a_post_replacement_failure_is_indeterminate_and_keeps_the_claim()
    test_a_release_failure_after_a_commit_reports_both_facts()
    test_only_a_valid_measurement_reaches_an_accepted_ingestion()
    test_the_orchestrator_never_manipulates_raw_artifacts()
    test_outcome_identifiers_and_exit_statuses_are_stable()
    test_the_low_level_harness_needs_no_docs_worktree()
    test_an_interrupt_is_its_own_outcome()
    test_an_unexpected_failure_still_gives_the_claim_back()
    test_an_interrupt_after_the_claim_reports_what_it_gave_back()
    test_the_cli_takes_no_probe_or_run_overrides()
    # The handoff (#1659)
    test_a_recorded_measurement_writes_its_handoff_beside_the_result()
    test_two_results_in_one_directory_get_two_handoffs()
    test_the_embedded_result_is_the_measurements_own_document()
    test_the_invocation_records_what_the_process_observed()
    test_the_adapter_is_told_the_timeout_and_starting_port()
    test_the_targets_are_the_non_pass_identifiers_in_descriptor_order()
    test_the_configuration_manifest_records_contents_and_absence()
    test_the_configuration_is_read_under_the_hold_before_the_runs()
    test_a_configuration_that_cannot_be_read_is_a_managed_failure()
    test_the_acceptable_failure_count_comes_from_the_installed_row()
    test_a_recorder_that_answers_the_wrong_shape_is_refused()
    test_only_the_recorded_outcome_has_a_handoff()
    test_a_recorded_measurement_with_nothing_retained_cannot_hand_off()
    test_a_handoff_that_cannot_be_written_is_a_managed_error()
    test_the_real_writer_produces_a_readable_document()
    test_the_engine_is_prepared_before_the_resource_hold()
    test_preparation_and_the_hold_resolve_one_namespace()
    test_a_preparation_failure_runs_nothing_and_gives_the_claim_back()
    test_the_real_preparation_runs_outside_the_real_hold()

    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(0, "\nAll deflake orchestration tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
