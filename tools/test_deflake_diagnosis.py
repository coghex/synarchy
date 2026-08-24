#!/usr/bin/env python3
"""Unit tests for the `/deflake` diagnosis mechanics (#1437).

Deterministic, engine-free, GPU-free and network-free. No probe is run,
no port opened, no worktree created, no census touched: every fixture is
a document in memory or a file in a temporary directory.

Two things are deliberately REAL rather than faked, for the same reason
`tools/test_deflake.py` keeps them real — faking either would move the
assertion off the thing under test:

* `probe_flake.Measurement`, so every result document a case feeds the
  evaluator is one the harness would actually have written, built out of
  real `RunRecord`s against a real `probe_protocol` descriptor; and
* `probe_census`'s X arithmetic, so "at or below X out of ten" is the
  shipped policy's answer rather than a second implementation of it that
  could drift from the census the same numbers are recorded in.

What is NOT covered here, deliberately: whether a diagnosis is
convincing, whether a repair is minimal, and whether a surviving
assertion was quietly broadened. Those are reviewer judgements. The
module refuses a route whose machine-checkable evidence is missing, and
these tests hold it to exactly that.

Usage:
  python3 tools/test_deflake_diagnosis.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import copy
import json
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import deflake_diagnosis as dd  # type: ignore
import probe_census  # type: ignore
import deflake  # type: ignore
import probe_flake  # type: ignore
import probe_protocol  # type: ignore
import run_probes  # type: ignore

TOOL = str(Path(__file__).resolve().parent / "deflake_diagnosis.py")
FAILURES: list[str] = []

# A REGISTERED, manual-only, `probe-result/v1` probe key. A real key
# because the entry gate validates against the live registry — but only
# its KEY is used, and nothing here runs it.
PROBE = "role"
OTHER = "position_hold"

BASE_COMMIT = "a" * 40
REPAIR_COMMIT = "b" * 40

# Three ordered checks, so "a contiguous suffix of the declared order"
# is a claim with something to be false about.
CHECKS = [("alpha", "the first check"), ("beta", "the second check"),
          ("gamma", "the third check")]

PASS = probe_protocol.PASS
FAIL = probe_protocol.FAIL
MISSING = probe_protocol.MISSING

# RESOLVED, because `check_artifact_root` resolves and a real document
# therefore carries no unresolved symlink — on macOS `/tmp` is a link to
# `/private/tmp`, so a fixture spelling it `/tmp/...` would be asserting
# against a path the harness could not have serialized.
OUTSIDE = str(Path("/tmp/synarchy-deflake-evidence").resolve())

# The two comparison worktrees. `evaluate` requires each declared
# worktree to be a REGISTERED one, so every case supplies these unless it
# is deliberately testing that rule.
CLEAN_WT = str(Path("/tmp/deflake-clean-role").resolve())
REPAIR_WT = str(Path("/tmp/deflake-role").resolve())
WORKTREES = (CLEAN_WT, REPAIR_WT)
# `/deflake` runs in the primary checkout, not in either comparison
# worktree: it is the step BEFORE this workflow creates them.
PRIMARY_WT = str(Path("/tmp/synarchy-primary").resolve())
VERIFY_ARTIFACTS = f"{OUTSIDE}/verify-artifacts"


def expect(cond: bool, msg: str) -> None:
    if not cond:
        FAILURES.append(msg)


def expect_rejected(thunk, fragment: str, msg: str) -> None:
    """`thunk` refuses at the entry gate, naming `fragment`."""
    try:
        thunk()
    except dd.HandoffError as error:
        expect(fragment in str(error),
               f"{msg}: rejected, but for {str(error)!r} rather than "
               f"{fragment!r}")
        return
    except dd.RouteRefused as error:
        FAILURES.append(f"{msg}: refused the ROUTE ({error}) where the entry "
                        f"gate should have rejected the input")
        return
    FAILURES.append(f"{msg}: accepted")


def expect_refused(thunk, fragment: str, msg: str) -> None:
    """`thunk` is well-formed but its declared route is denied."""
    try:
        thunk()
    except dd.RouteRefused as error:
        expect(fragment in str(error),
               f"{msg}: refused, but for {str(error)!r} rather than "
               f"{fragment!r}")
        return
    except dd.HandoffError as error:
        FAILURES.append(f"{msg}: rejected the INPUT ({error}) where the "
                        f"route should have been refused")
        return
    FAILURES.append(f"{msg}: accepted")


# --------------------------------------------------------------------------
# Fixtures
# --------------------------------------------------------------------------
def result_document(*, probe: str = PROBE, commit: str = BASE_COMMIT,
                    checks=None, runs=None, rts_caps=None,
                    requested=None, harness_error: bool = False,
                    artifact_root=None, command_runs=None) -> dict:
    """A REAL `probe-flake-result/v1` document.

    `runs` is a list of per-run check maps; the run outcome is derived
    the way `probe_flake.reconcile` derives it — a timeout is stated
    explicitly by the caller, a FAIL check makes the run FAIL, and
    everything else passes.
    """
    declared = CHECKS if checks is None else checks
    descriptor = probe_protocol.build_descriptor(probe, declared)
    # `command_runs` exists only to make a fixture's intent explicit at
    # the call site: a result document is bound to the command that
    # produced it, so a batch of another size needs both to say so.
    requested = dd.RUN_COUNT if requested is None else requested
    if command_runs is not None and command_runs != requested:
        raise AssertionError("a fixture's command and result must agree")
    caps = dd.RTS_CAPABILITIES if rts_caps is None else rts_caps
    root = Path(artifact_root if artifact_root is not None
                else f"{OUTSIDE}/artifacts")
    # `new_invocation_dir` names this `{probe}-{stamp}-{pid}-{uuid8}` and
    # creates it directly under the root, and the fixture reproduces that
    # rather than inventing a tidier layout the harness never makes.
    invocation_dir = root / f"{probe}-20260821T120000Z-4711-abcdef12"
    measurement = probe_flake.Measurement(
        probe, descriptor, requested, caps, root, invocation_dir)
    measurement.commit_sha = commit
    measurement.timestamp = "2026-08-21T12:00:00Z"
    if runs is None:
        runs = [{cid: PASS for cid, _label in declared}] * requested
    for index, checks_map in enumerate(runs, 1):
        # Copied, because `[{...}] * n` shares ONE dict across every run
        # and a caller's fixture must not change under it.
        checks_map = dict(checks_map)
        timed_out = checks_map.pop("__timeout__", False)
        if timed_out:
            outcome = probe_flake.RUN_TIMEOUT
        elif any(v == FAIL for v in checks_map.values()):
            outcome = probe_flake.RUN_FAIL
        else:
            outcome = probe_flake.RUN_PASS
        keep = outcome != probe_flake.RUN_PASS
        measurement.runs.append(probe_flake.RunRecord(
            index, 9100 + index, outcome, 1.5, dict(checks_map),
            (invocation_dir / f"run-{index:03d}") if keep else None))
    if harness_error:
        # The run that broke the stream is never one of the completed
        # ones, so a real harness error always leaves one uncompleted.
        broken = measurement.runs.pop()
        measurement.status = "harness-error"
        measurement.error = (f"run {broken.index}: the event stream could "
                             f"not be trusted")
        measurement.error_run = probe_flake.RunRecord(
            broken.index, broken.port, probe_flake.RUN_HARNESS_ERROR, 0.5,
            {}, invocation_dir / f"run-{broken.index:03d}")
    return measurement.to_document()


def verification_result(**kwargs) -> dict:
    """The repair batch's result: its own commit, its own artifact root.

    `_require_batch` binds a batch's declared `--artifact-root` to the
    root its result document reports, so the two halves of a fixture
    cannot drift apart the way a hand-written pair would.
    """
    kwargs.setdefault("commit", REPAIR_COMMIT)
    kwargs.setdefault("artifact_root", VERIFY_ARTIFACTS)
    return result_document(**kwargs)


def failing_runs(count: int, *, cid: str = "beta", declared=None,
                 abort: bool = True) -> list:
    """`count` runs that FAIL at `cid`, then clean runs to fill the batch.

    `abort=True` is the common shape: the probe stops there and every
    later check is MISSING. `abort=False` is a probe that reports a
    failed check and keeps going, which is the only way a measurement's
    non-PASS set is NOT suffix-closed — and therefore the only way two
    measurements can fail at genuinely disjoint checks.
    """
    declared = CHECKS if declared is None else declared
    ids = [c for c, _l in declared]
    position = ids.index(cid)
    runs = []
    for _ in range(count):
        run = {}
        for index, name in enumerate(ids):
            if index == position:
                run[name] = FAIL
            elif index < position or not abort:
                run[name] = PASS
            else:
                run[name] = MISSING
        runs.append(run)
    while len(runs) < dd.RUN_COUNT:
        runs.append({name: PASS for name in ids})
    return runs


def command(*, probe: str = PROBE, runs=None, rts_caps=None,
            result: str = f"{OUTSIDE}/baseline.json",
            artifacts: str = f"{OUTSIDE}/artifacts",
            worktree: str = CLEAN_WT) -> list:
    runs = dd.RUN_COUNT if runs is None else runs
    caps = dd.RTS_CAPABILITIES if rts_caps is None else rts_caps
    return [
        "python3", f"{worktree}/tools/probe_flake.py",
        "--probe", probe, "--runs", str(runs), "--rts-caps", str(caps),
        "--result", result, "--artifact-root", artifacts,
    ]


def invocation(*, cmd=None, directory: str = CLEAN_WT,
               retries: int = 0, ports=None, timeout=None,
               start_port=None) -> dict:
    """One recorded harness invocation.

    `timeout` and `start_port` are `probe_flake.measure`'s own defaults,
    which neither CLI exposes — recorded because they are
    behavior-affecting and invisible to the command, so without them two
    identical argv strings could describe two different measurements.
    """
    return {
        "command": command() if cmd is None else cmd,
        "directory": directory,
        "retries": retries,
        "ports": [9101, 9102] if ports is None else ports,
        "timeout_seconds": (probe_flake.DEFAULT_TIMEOUT if timeout is None
                            else timeout),
        "start_port": (probe_flake.PORT_MIN if start_port is None
                       else start_port),
    }


def deflake_invocation(*, cmd=None, directory: str = PRIMARY_WT,
                       retries: int = 0, ports=None) -> dict:
    """The #1436 handoff's own invocation: `python3 tools/deflake.py`.

    `/deflake` does NOT shell out to the harness — it calls
    `probe_flake.measure` in process — and its CLI has no `--probe`, no
    `--runs` and no RTS override at all. A fixture claiming a
    `probe_flake.py` argv for the handoff would be asserting against a
    command that never ran.
    """
    return {
        "command": cmd if cmd is not None else [
            "python3", f"{PRIMARY_WT}/tools/deflake.py", "--json",
            "--result", f"{OUTSIDE}/handoff.json"],
        "directory": directory,
        "retries": retries,
        "ports": [9101, 9102] if ports is None else ports,
        "timeout_seconds": probe_flake.DEFAULT_TIMEOUT,
        "start_port": probe_flake.PORT_MIN,
    }


def manifest(entries=()) -> dict:
    return {"schema": dd.MANIFEST_SCHEMA, "root": "/tmp/whatever",
            "entries": [{"path": path, "sha256": digest}
                        for path, digest in entries]}


def handoff_document(*, probe: str = PROBE, acceptable: int = 0,
                     targets=None, result=None, inv=None,
                     config=None) -> dict:
    """A handoff whose targets DERIVE from its own measurement.

    Deriving rather than declaring is the point: the entry gate requires
    the target list to equal the measurement's ordered non-PASS
    identifiers exactly, so a fixture that hard-coded one would be
    asserting against itself. A case that wants a wrong target passes
    `targets=` explicitly.
    """
    result = result if result is not None else result_document(
        probe=probe, runs=failing_runs(3))
    return {
        "schema": dd.HANDOFF_SCHEMA,
        "probe": probe,
        "acceptable_failures": acceptable,
        "targets": (list(targets) if targets is not None
                    else dd.non_pass_ids(result)),
        "result": result,
        "invocation": deflake_invocation() if inv is None else inv,
        "configuration": manifest() if config is None else config,
        "artifacts": [],
    }


def diagnosis_document(*, route: str = dd.ROUTE_REPAIR, handoff=None,
                       baseline=None, verification=None, diagnosis=None,
                       attestations=None, repair=None) -> dict:
    handoff = handoff_document() if handoff is None else handoff
    document = {
        "schema": dd.DIAGNOSIS_SCHEMA,
        "handoff": handoff,
        "route": route,
        "diagnosis": diagnosis if diagnosis is not None else {
            "category": "setup-precondition",
            "summary": "the fixture is placed on an unloaded chunk",
            "evidence": ["run 1's engine log shows the chunk was never "
                         "loaded before the fixture was placed"],
        },
    }
    if baseline is not None:
        document["baseline"] = baseline
    else:
        document["baseline"] = {
            "worktree": CLEAN_WT,
            "source_clean": True,
            "result": result_document(runs=failing_runs(4)),
            "invocation": invocation(),
            "configuration": manifest(),
        }
    document["baseline"].setdefault("result", result_document())
    if verification is not None:
        document["verification"] = verification
    elif route in (dd.ROUTE_REPAIR, dd.ROUTE_PARTIAL_IMPROVEMENT):
        document["verification"] = {
            "worktree": REPAIR_WT,
            "source_clean": True,
            "result": verification_result(artifact_root=VERIFY_ARTIFACTS),
            "invocation": invocation(
                cmd=command(result=f"{OUTSIDE}/verify.json",
                            artifacts=VERIFY_ARTIFACTS,
                            worktree=REPAIR_WT),
                directory=REPAIR_WT, ports=[9201, 9202]),
            "configuration": manifest(),
        }
    if route == dd.ROUTE_REPAIR:
        document["attestations"] = attestations if attestations is not None else {
            name: True for name in dd.ATTESTATIONS}
        document["repair"] = repair if repair is not None else {
            "commit_sha": REPAIR_COMMIT,
            "base_sha": BASE_COMMIT,
            "changed_paths": ["tools/role_probe.py"],
        }
    else:
        if attestations is not None:
            document["attestations"] = attestations
        if repair is not None:
            document["repair"] = repair
    return document


def relocate_section(section, tree, *, result=f"{OUTSIDE}/verify.json",
                     artifacts=None) -> None:
    """Move a section to `tree`, command and all.

    A recorded command names the tool inside the checkout it ran in, so a
    fixture that moved only the declaration would be refused for the
    script-path binding rather than for whatever it meant to test.
    """
    section["worktree"] = tree
    section["invocation"] = invocation(
        cmd=command(result=result,
                    artifacts=artifacts or VERIFY_ARTIFACTS, worktree=tree),
        directory=tree, ports=[9201])


def evaluate(document, **kwargs):
    return dd.evaluate(document,
                       worktrees=kwargs.pop("worktrees", WORKTREES),
                       primary=kwargs.pop("primary", PRIMARY_WT))


# ==========================================================================
# The entry gate: complete and malformed handoffs
# ==========================================================================
def test_a_complete_handoff_is_accepted() -> None:
    handoff = dd.require_handoff(handoff_document())
    expect(handoff.probe == PROBE, "the accepted handoff names its probe")
    expect(handoff.acceptable_failures == 0, "X survives the gate")
    expect(handoff.expected_checks == [cid for cid, _l in CHECKS],
           "the ordered descriptor comes from the result document itself")
    expect(handoff.commit_sha == BASE_COMMIT,
           "the baseline commit is the result document's own")


def test_a_handoff_naming_several_probes_is_refused() -> None:
    document = handoff_document()
    document["probe"] = [PROBE, OTHER]
    expect_rejected(lambda: dd.require_handoff(document), "names 2 probes",
                    "a handoff naming two probes")


def test_an_unregistered_probe_is_refused() -> None:
    document = handoff_document()
    document["probe"] = "not_a_real_probe"
    document["result"]["probe"] = "not_a_real_probe"
    expect_rejected(lambda: dd.require_handoff(document),
                    "not a registered probe key",
                    "a handoff naming an unregistered probe")


def test_a_probe_with_no_descriptor_is_refused() -> None:
    """A legacy probe has no per-check evidence to diagnose."""
    legacy = next(key for key, _script, _purpose in run_probes.PROBES
                  if key not in probe_flake.PROTOCOL_PROBES)
    document = handoff_document(probe=legacy)
    document["result"]["probe"] = legacy
    expect_rejected(lambda: dd.require_handoff(document),
                    "does not implement", "a legacy probe handoff")


def test_the_wrong_schema_is_refused() -> None:
    document = handoff_document()
    document["schema"] = "deflake-handoff/v0"
    expect_rejected(lambda: dd.require_handoff(document), "expected",
                    "a handoff with the wrong schema")


def test_a_handoff_rebuilt_from_the_census_row_is_refused() -> None:
    """The durable census row is not a substitute for the result document.

    `probe_census.ingest_result` deliberately drops the ports, the
    per-run check maps, the descriptor labels, the artifact root, the
    invocation directory and the exact command — so a handoff carrying
    only what survived ingestion cannot identify the baseline
    invocation, and this is the shape that arrives when someone tries.
    """
    document = handoff_document()
    document["result"] = None
    expect_rejected(lambda: dd.require_handoff(document),
                    "durable census row is not a substitute",
                    "a handoff with no embedded result document")


def test_a_handoff_whose_result_measured_another_probe_is_refused() -> None:
    document = handoff_document()
    document["result"] = result_document(probe=OTHER, runs=failing_runs(3))
    expect_rejected(lambda: dd.require_handoff(document),
                    "its result document measured",
                    "a handoff whose result is another probe's")


def test_the_targets_are_every_non_pass_identifier_in_order() -> None:
    """Not a selection FROM the measurement — they are it."""
    accepted = dd.require_handoff(handoff_document())
    expect(list(accepted.targets) == ["beta", "gamma"],
           f"an abort at beta implicates gamma too; got {accepted.targets}")

    expect_rejected(lambda: dd.require_handoff(handoff_document(
        targets=("delta",))),
        "identifiers the descriptor never declared",
        "a target that is not a declared check")
    expect_rejected(lambda: dd.require_handoff(handoff_document(
        targets=("beta",))),
        "naming a subset would let a repair be verified",
        "a target list that omits an observed failure")
    expect_rejected(lambda: dd.require_handoff(handoff_document(
        targets=("alpha", "beta", "gamma"))),
        "targets something this measurement did not see",
        "a target that never went non-PASS")
    expect_rejected(lambda: dd.require_handoff(handoff_document(
        targets=("gamma", "beta"))),
        "in that order",
        "targets in an order the descriptor does not declare")
    expect_rejected(lambda: dd.require_handoff(handoff_document(
        targets=("beta", "beta", "gamma"))),
        "repeats an identifier",
        "a repeated target")


def test_a_handoff_with_no_targets_is_refused() -> None:
    document = handoff_document(targets=())
    expect_rejected(lambda: dd.require_handoff(document),
                    "names no target check identifiers",
                    "a handoff naming no target")


def test_an_expected_check_list_that_contradicts_the_descriptor_is_refused() -> None:
    document = handoff_document()
    document["expected_checks"] = ["alpha", "gamma", "beta"]
    expect_rejected(lambda: dd.require_handoff(document),
                    "the descriptor is the ordered contract",
                    "a reordered expected-check list")


def test_the_retained_list_is_exactly_what_the_runs_kept() -> None:
    """So "failures with no evidence" is unrepresentable, not just refused."""
    document = handoff_document()
    document["result"]["retained_artifacts"] = []
    expect_rejected(lambda: dd.require_handoff(document),
                    "naming evidence it does not have",
                    "a document that dropped its retained list")

    document = handoff_document()
    document["result"]["retained_artifacts"].append(f"{OUTSIDE}/invented")
    expect_rejected(lambda: dd.require_handoff(document),
                    "naming evidence it does not have",
                    "a document that invented a retained path")


def test_a_passing_run_keeps_no_raw_artifacts() -> None:
    """One of verification's own success conditions, and a harness fact.

    `probe_flake.measure` deletes a run's directory the moment it passes
    and records `artifact_dir: null`, so a PASS run naming one was not
    written by the harness — and leaving successful-run artifacts behind
    is exactly what a verification batch may not do.
    """
    document = handoff_document()
    passing = next(run for run in document["result"]["runs"]
                   if run["outcome"] == probe_flake.RUN_PASS)
    passing["artifact_dir"] = f"{OUTSIDE}/artifacts/invocation/kept"
    document["result"]["retained_artifacts"].append(passing["artifact_dir"])
    expect_rejected(lambda: dd.require_handoff(document),
                    "passed and still names the artifact directory",
                    "a passing run that kept its directory")

    end_to_end = diagnosis_document()
    kept = end_to_end["verification"]["result"]["runs"][0]
    kept["artifact_dir"] = f"{VERIFY_ARTIFACTS}/invocation/kept"
    end_to_end["verification"]["result"]["retained_artifacts"] = [
        kept["artifact_dir"]]
    expect_rejected(lambda: evaluate(end_to_end),
                    "passed and still names the artifact directory",
                    "a verification batch that kept a passing run")


def test_an_unsuccessful_run_must_still_have_its_logs() -> None:
    document = handoff_document()
    failing = next(run for run in document["result"]["runs"]
                   if run["outcome"] != probe_flake.RUN_PASS)
    document["result"]["retained_artifacts"].remove(failing["artifact_dir"])
    failing["artifact_dir"] = None
    expect_rejected(lambda: dd.require_handoff(document),
                    "a failure whose logs are gone",
                    "a failing run whose artifacts were discarded")


# ==========================================================================
# X: the numeric ceiling out of ten
# ==========================================================================
def test_x_must_be_a_validated_integer() -> None:
    for value in (None, "1", 1.0, True, -1, dd.RUN_COUNT):
        document = handoff_document(acceptable=0)
        document["acceptable_failures"] = value
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        "acceptable_failures",
                        f"an X of {value!r}")


def test_x_is_the_census_policys_own_arithmetic() -> None:
    """At or below X passes, above it does not — X=1 accepts 1, rejects 2.

    The accepted failing run is NON-aborting (`abort=False`): a target
    has zero MISSING across all ten runs, so a run that aborted before
    one would be refused for that instead, and this case is about the
    arithmetic.
    """
    handoff = handoff_document(acceptable=1)
    accepted = diagnosis_document(handoff=handoff)
    accepted["verification"]["result"] = verification_result(
        runs=failing_runs(1, abort=False))
    outcome = evaluate(accepted)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"one failure against X=1 is a repair, got {outcome.route}")
    expect(outcome.verification_failures == 1, "the count is reported")

    over = diagnosis_document(handoff=handoff_document(acceptable=1))
    over["verification"]["result"] = verification_result(
        runs=failing_runs(2, abort=False))
    expect_refused(lambda: evaluate(over), "partial-improvement",
                   "two failures against X=1")


def test_x_zero_requires_a_spotless_batch() -> None:
    document = diagnosis_document()
    document["verification"]["result"] = verification_result(runs=failing_runs(1))
    expect_refused(lambda: evaluate(document), "partial-improvement",
                   "one failure against X=0")


def test_a_batch_of_the_wrong_size_is_not_a_measurement() -> None:
    short = result_document(runs=failing_runs(3)[:5], requested=5)
    document = handoff_document(result=short)
    expect_rejected(lambda: dd.require_handoff(document),
                    "measurement contract is exactly",
                    "a five-run batch")


def test_an_incomplete_batch_is_not_a_measurement() -> None:
    document = handoff_document()
    document["result"]["completed_runs"] = dd.RUN_COUNT - 1
    expect_rejected(lambda: dd.require_handoff(document),
                    "an incomplete batch is not a measurement",
                    "an incomplete batch")


def test_a_harness_error_is_not_a_comparison_side() -> None:
    document = handoff_document(result=result_document(
        runs=failing_runs(3), harness_error=True))
    expect_rejected(lambda: dd.require_handoff(document),
                    "no trustworthy failure rate",
                    "a harness-error batch")


def test_the_capability_count_is_fixed() -> None:
    document = handoff_document(result=result_document(
        runs=failing_runs(3), rts_caps=2))
    expect_rejected(lambda: dd.require_handoff(document),
                    "RTS capabilities", "a batch at two capabilities")


def test_an_overlapping_harness_invalidates_the_control() -> None:
    document = handoff_document()
    document["result"]["peak_concurrency"] = 2
    expect_rejected(lambda: dd.require_handoff(document),
                    "peak concurrency", "a contended batch")


def test_a_pass_run_carrying_a_failed_check_is_refused() -> None:
    """Delegation to `probe_census.validate_result`, and why it matters.

    `failure_count` counts RUNS by their outcome, so a document whose
    runs all claim PASS while their check maps carry FAIL would read as a
    spotless batch and could be admitted as a verified repair. The
    canonical validator's `_rule_pass_run_has_no_failed_check` is what
    refuses it, which is why every result document goes through the
    shipped validator before a single field is read.
    """
    document = handoff_document()
    result = document["result"]
    for run in result["runs"]:
        run["outcome"] = probe_flake.RUN_PASS
        run["checks"]["beta"] = FAIL
        run["checks"]["gamma"] = PASS
    result["check_counts"]["beta"] = {PASS: 0, FAIL: dd.RUN_COUNT, MISSING: 0}
    result["check_counts"]["gamma"] = {PASS: dd.RUN_COUNT, FAIL: 0,
                                       MISSING: 0}
    result["failure_count"] = 0
    result["failure_rate"] = 0.0
    result["timeout_count"] = 0
    expect(dd.failure_count(result) == 0,
           "the run-outcome count really would read this as spotless")
    expect_rejected(lambda: dd.require_handoff(document),
                    "internally inconsistent",
                    "runs claiming PASS while carrying a FAIL check")


def test_an_unresolved_commit_is_not_evidence() -> None:
    document = handoff_document(result=result_document(
        runs=failing_runs(3), commit="unknown"))
    expect_rejected(lambda: dd.require_handoff(document),
                    "does not name a resolved commit",
                    "a result document with no commit")


# ==========================================================================
# No retries
# ==========================================================================
def test_a_retry_policy_is_refused() -> None:
    document = handoff_document(inv=deflake_invocation(retries=1))
    expect_rejected(lambda: dd.require_handoff(document),
                    "retry policy", "a handoff measured with retries")


def test_an_absent_retry_policy_is_refused() -> None:
    inv = deflake_invocation()
    del inv["retries"]
    document = handoff_document(inv=inv)
    expect_rejected(lambda: dd.require_handoff(document),
                    "retry policy", "a handoff that states no retry policy")


def test_the_handoff_comes_from_deflake_and_the_batches_from_the_harness() -> None:
    """The three batches do not come from one command.

    `/deflake` calls `probe_flake.measure` IN PROCESS and its CLI has no
    `--probe`, `--runs` or RTS override at all, so requiring a
    `probe_flake.py` argv for the handoff would make a truthful #1436
    record impossible to submit while accepting an argv nobody ran.
    """
    expect(dd.DEFLAKE_LAUNCHER.fixed == {"runs": dd.RUN_COUNT,
                                         "rts_caps": dd.RTS_CAPABILITIES},
           f"/deflake supplies both counts itself: {dd.DEFLAKE_LAUNCHER.fixed}")
    expect(dd.DEFLAKE_LAUNCHER.probe_from_result,
           "and the probe comes from the document its selector produced")

    accepted = dd.require_handoff(handoff_document())
    expect(accepted.probe == PROBE, "a real /deflake record is admitted")

    swapped = handoff_document(inv=invocation())
    expect_rejected(lambda: dd.require_handoff(swapped),
                    "come from deflake.py",
                    "a handoff claiming a probe_flake.py argv")

    document = diagnosis_document()
    document["baseline"]["invocation"] = deflake_invocation(
        cmd=["python3", f"{CLEAN_WT}/tools/deflake.py", "--json"],
        directory=CLEAN_WT)
    expect_rejected(lambda: evaluate(document),
                    "come from probe_flake.py",
                    "a controlled batch claiming a /deflake argv")

    counterfeit = handoff_document(inv=deflake_invocation(cmd=[
        "python3", "/tmp/counterfeit/deflake.py", "--json"]))
    expect_rejected(lambda: dd.require_handoff(counterfeit),
                    "the checkout it declares keeps that tool at",
                    "a handoff claiming a counterfeit /deflake")


def test_a_deflake_command_takes_only_its_own_two_options() -> None:
    for extra in (["--probe", PROBE], ["--runs", "10"],
                  ["--rts-caps", "4"], ["--artifact-root", OUTSIDE]):
        document = handoff_document(inv=deflake_invocation(cmd=[
            "python3", f"{PRIMARY_WT}/tools/deflake.py", "--json"] + extra))
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        "does not accept",
                        f"a /deflake command carrying {extra[0]}")

    # `--json` is a flag, so it must not swallow the next argument.
    document = handoff_document(inv=deflake_invocation(cmd=[
        "python3", f"{PRIMARY_WT}/tools/deflake.py",
        "--json", "--result", f"{OUTSIDE}/handoff.json"]))
    dd.require_handoff(document)
    document = handoff_document(inv=deflake_invocation(cmd=[
        "python3", f"{PRIMARY_WT}/tools/deflake.py",
        "--json=true", "--result", f"{OUTSIDE}/handoff.json"]))
    expect_rejected(lambda: dd.require_handoff(document),
                    "which is a flag", "a value passed to --json")

    # `--result` is OPTIONAL there: /deflake retains the document beside
    # its artifacts whether or not it is also copied out.
    document = handoff_document(inv=deflake_invocation(cmd=[
        "python3", f"{PRIMARY_WT}/tools/deflake.py", "--json"]))
    dd.require_handoff(document)


def test_a_batch_invocation_must_match_the_measurement_it_describes() -> None:
    for label, cmd, fragment in (
            ("another probe", command(probe=OTHER), "describe one measurement"),
            ("five runs", command(runs=5), "describe one measurement"),
            ("eight capabilities", command(rts_caps=8),
             "describe one measurement")):
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(cmd=cmd)
        expect_rejected(lambda d=document: evaluate(d), fragment,
                        f"a baseline command claiming {label}")


# ==========================================================================
# Configuration: contents AND absence
# ==========================================================================
def test_an_empty_manifest_is_an_explicit_statement() -> None:
    root = Path(tempfile.mkdtemp(prefix="test_deflake_diagnosis_"))
    try:
        (root / "config").mkdir()
        document = dd.config_manifest(root)
        expect(document["entries"] == [],
               "an absent configuration family is an empty entry list")
        dd.require_manifest(document, "manifest")
    finally:
        shutil.rmtree(root, ignore_errors=True)


def test_a_manifest_records_a_digest_per_file() -> None:
    root = Path(tempfile.mkdtemp(prefix="test_deflake_diagnosis_"))
    try:
        (root / "config").mkdir()
        (root / "config" / "save.local.yaml").write_text("autosave: true\n")
        (root / "config" / "video.local.yaml").write_text("vsync: false\n")
        (root / "config" / "ignored.yaml").write_text("not in the family\n")
        document = dd.config_manifest(root)
        paths = [entry["path"] for entry in document["entries"]]
        expect(paths == ["config/save.local.yaml", "config/video.local.yaml"],
               f"only the gitignored family is recorded, sorted; got {paths}")
        expect(all(dd.SHA256_RE.match(entry["sha256"])
                   for entry in document["entries"]),
               "every entry carries a SHA-256 digest")
    finally:
        shutil.rmtree(root, ignore_errors=True)


def test_a_manifest_entry_must_name_the_gitignored_family() -> None:
    """Otherwise "identical manifests" can be identical about anything.

    Two documents both listing `../outside-config.local.yaml` agree
    perfectly and establish nothing about the `config/*.local.yaml` state
    the probes actually symlink into their isolated resource roots.
    """
    for relative in ("../outside-config.local.yaml",
                     "/etc/config/save.local.yaml",
                     "config/nested/save.local.yaml",
                     "config/save.yaml",
                     "save.local.yaml",
                     "config/../config/save.local.yaml"):
        expect_rejected(lambda r=relative: dd.require_manifest(
            manifest([(r, "c" * 64)]), "manifest"),
            "gitignored", f"a manifest entry naming {relative!r}")

    for relative in ("config/save.local.yaml", "config/video.local.yaml",
                     "config/keybinds.local.yaml",
                     "config/notifications.local.yaml"):
        dd.require_manifest(manifest([(relative, "c" * 64)]), "manifest")

    # The real generator only ever produces members of the family.
    root = Path(tempfile.mkdtemp(prefix="test_deflake_diagnosis_"))
    try:
        (root / "config").mkdir()
        (root / "config" / "save.local.yaml").write_text("autosave: true\n")
        dd.require_manifest(dd.config_manifest(root), "generated manifest")
    finally:
        shutil.rmtree(root, ignore_errors=True)


def test_both_comparison_worktrees_are_source_clean() -> None:
    """The clean side needs the attestation as much as the repair side.

    Its recorded SHA cannot reveal an uncommitted change, and "the clean
    comparison worktree must remain unmodified" is a contract about its
    SOURCE — the gitignored configuration state it must also reproduce is
    recorded separately in its own manifest.
    """
    for section in ("baseline", "verification"):
        for value in (None, False, "yes"):
            document = diagnosis_document()
            if value is None:
                del document[section]["source_clean"]
            else:
                document[section]["source_clean"] = value
            expect_rejected(lambda d=document: evaluate(d),
                            "not recorded as source-clean",
                            f"a {section} recorded as {value!r}")


def test_a_batch_may_not_write_into_the_other_declared_worktree() -> None:
    """Both declarations are collected BEFORE either batch is validated.

    That is what still holds once the comparison worktrees have been
    removed — which the workflow requires — and neither appears in
    `worktree_paths()` any more. Checked here with NO registered
    worktrees at all, which is exactly the post-cleanup state.
    """
    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(
        cmd=command(result=f"{REPAIR_WT}/baseline.json",
                    artifacts=f"{OUTSIDE}/artifacts", worktree=CLEAN_WT))
    expect_rejected(lambda: evaluate(document, worktrees=()),
                    "inside the working tree",
                    "a baseline writing into the repair worktree")

    # The layout moves as a whole, because topology pins every derived
    # path to the artifact root — so the reachable case is a root inside
    # the OTHER comparison state.
    other = diagnosis_document(handoff=handoff_document(acceptable=1))
    root = f"{CLEAN_WT}/artifacts"
    other["verification"]["result"] = verification_result(
        runs=failing_runs(1), artifact_root=root)
    other["verification"]["invocation"] = invocation(
        cmd=command(result=f"{OUTSIDE}/verify.json", artifacts=root,
                    worktree=REPAIR_WT),
        directory=REPAIR_WT, ports=[9201])
    expect_rejected(lambda: evaluate(other, worktrees=()),
                    "inside the working tree",
                    "a verification retaining logs in the clean worktree")


def test_a_command_must_agree_with_the_result_it_produced() -> None:
    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(
        cmd=command(probe=OTHER))
    expect_rejected(lambda: evaluate(document),
                    "describe one measurement",
                    "a command naming a probe its result did not measure")


def test_a_manifest_with_no_entries_key_is_refused() -> None:
    """Absence has to be ASSERTED, not inferred from an omitted key."""
    expect_rejected(lambda: dd.require_manifest(
        {"schema": dd.MANIFEST_SCHEMA}, "manifest"),
        "record an empty list", "a manifest that omits `entries`")


def test_absence_on_one_side_is_a_difference() -> None:
    empty = manifest()
    present = manifest([("config/save.local.yaml", "c" * 64)])
    problems = dd.manifest_differences(empty, present, left_name="left",
                                       right_name="right")
    expect(len(problems) == 1 and "absent from left" in problems[0],
           f"an extra file on one side is a difference; got {problems}")
    problems = dd.manifest_differences(present, empty, left_name="left",
                                       right_name="right")
    expect(len(problems) == 1 and "absent from right" in problems[0],
           f"and so is a missing one; got {problems}")
    expect(dd.manifest_differences(empty, manifest(), left_name="left",
                                   right_name="right") == [],
           "two confirmed-absent manifests agree")


def test_a_digest_difference_is_a_difference() -> None:
    a = manifest([("config/save.local.yaml", "c" * 64)])
    b = manifest([("config/save.local.yaml", "d" * 64)])
    problems = dd.manifest_differences(a, b, left_name="left",
                                       right_name="right")
    expect(len(problems) == 1 and "differs" in problems[0],
           f"different contents are a difference; got {problems}")


def test_a_baseline_under_another_configuration_is_not_the_condition() -> None:
    document = diagnosis_document()
    document["baseline"]["configuration"] = manifest(
        [("config/save.local.yaml", "e" * 64)])
    expect_refused(lambda: evaluate(document),
                   "did not reproduce the handoff's configuration state",
                   "a baseline under another configuration")


def test_the_two_comparison_worktrees_must_agree() -> None:
    document = diagnosis_document()
    document["verification"]["configuration"] = manifest(
        [("config/keybinds.local.yaml", "f" * 64)])
    expect_refused(lambda: evaluate(document),
                   "do not hold the same configuration state",
                   "comparison worktrees that disagree")


# ==========================================================================
# Controlled reproduction
# ==========================================================================
def test_a_diagnosis_without_a_controlled_baseline_is_refused() -> None:
    document = diagnosis_document()
    del document["baseline"]
    expect_rejected(lambda: evaluate(document),
                    "carries no controlled pre-fix baseline",
                    "a diagnosis with no baseline")


def test_a_baseline_at_or_below_x_cannot_support_a_repair() -> None:
    """A baseline that reproduced the target but stayed within tolerance.

    The target IS observed here, so the only rule left to refuse this is
    "the controlled baseline must exceed X" — a spotless baseline would
    be refused by the target rule first and would prove nothing about
    the arithmetic.
    """
    document = diagnosis_document(handoff=handoff_document(acceptable=1))
    document["baseline"]["result"] = result_document(runs=failing_runs(1))
    expect_refused(lambda: evaluate(document), "cannot-reproduce",
                   "a baseline within tolerance")

    spotless = diagnosis_document()
    spotless["baseline"]["result"] = result_document()
    expect_refused(lambda: evaluate(spotless), "cannot-reproduce",
                   "a spotless baseline")


def test_a_baseline_that_never_hits_the_target_cannot_support_a_repair() -> None:
    """Over tolerance, but failing somewhere else entirely.

    The target is the FIRST declared check and the baseline aborts at the
    second, so the target is PASS in every run — an abort at an earlier
    check would have made it MISSING, which is a non-PASS observation and
    would legitimately count as reproducing the pattern.
    """
    handoff = handoff_document(result=result_document(
        runs=failing_runs(3, cid="alpha", abort=False)))
    expect(handoff["targets"] == ["alpha"],
           f"a non-aborting failure implicates only itself; got "
           f"{handoff['targets']}")
    document = diagnosis_document(handoff=handoff)
    document["baseline"]["result"] = result_document(
        runs=failing_runs(4, cid="gamma", abort=False))
    expect_refused(lambda: evaluate(document), "cannot-reproduce",
                   "a baseline that reproduced another failure")


def test_the_baseline_must_be_measured_at_the_handoffs_own_commit() -> None:
    """One common SHA, or the two states are not a comparison."""
    document = diagnosis_document()
    document["baseline"]["result"] = result_document(
        commit="d" * 40, runs=failing_runs(4))
    expect_rejected(lambda: evaluate(document),
                    "recreate BOTH states on one new common SHA",
                    "a baseline measured at another commit")


def test_the_repair_must_be_cut_from_that_same_commit() -> None:
    document = diagnosis_document()
    del document["repair"]["base_sha"]
    expect_rejected(lambda: evaluate(document),
                    "names no resolved base commit",
                    "a repair whose lineage is unstated")

    document = diagnosis_document()
    document["repair"]["base_sha"] = "e" * 40
    expect_rejected(lambda: evaluate(document),
                    "share one common SHA or they are not a comparison",
                    "a repair cut from another commit")


def test_a_relative_destination_is_resolved_before_it_is_judged() -> None:
    """`probe_flake.write_result` opens `--result` relative to its cwd.

    So `results/verification.json` from inside the repair worktree lands
    IN that worktree while matching no absolute registered path at all.
    The recorded invocation directory is what makes a relative
    destination mean something, and it is joined on before containment
    is decided.
    """
    trees = [REPAIR_WT]
    for relative in ("results/verification.json",
                     "./results/verification.json",
                     "../deflake-role/verification.json"):
        document = diagnosis_document()
        document["verification"]["invocation"] = invocation(
            cmd=command(result=relative, artifacts=VERIFY_ARTIFACTS,
                        worktree=REPAIR_WT),
            directory=REPAIR_WT, ports=[9201])
        expect_rejected(lambda d=document: evaluate(d, worktrees=trees),
                        "inside the working tree",
                        f"a --result of {relative!r}")

    outside = diagnosis_document()
    outside["verification"]["invocation"] = invocation(
        cmd=command(result="../evidence/verification.json",
                    artifacts=VERIFY_ARTIFACTS,
                    worktree=REPAIR_WT),
        directory=REPAIR_WT, ports=[9201])
    outcome = evaluate(outside, worktrees=trees)
    expect(outcome.route == dd.ROUTE_REPAIR,
           "a relative path that really does leave the worktree is fine")


def test_both_batches_must_stay_outside_every_worktree() -> None:
    trees = [REPAIR_WT]
    document = diagnosis_document()
    document["verification"]["invocation"] = invocation(
        cmd=command(result="/tmp/deflake-role/verify.json",
                    artifacts=VERIFY_ARTIFACTS,
                    worktree=REPAIR_WT),
        directory=REPAIR_WT, ports=[9201])
    expect_rejected(lambda: evaluate(document, worktrees=trees),
                    "inside the working tree",
                    "a result document written into a worktree")


def test_the_two_batches_may_not_share_a_destination() -> None:
    document = diagnosis_document()
    document["verification"]["invocation"] = invocation(
        cmd=command(worktree=REPAIR_WT), directory=REPAIR_WT, ports=[9201])
    document["verification"]["result"] = verification_result(
        artifact_root=f"{OUTSIDE}/artifacts")
    expect_refused(lambda: evaluate(document), "both batches wrote to",
                   "two batches sharing one result path")


def test_a_repair_without_a_verification_batch_is_refused() -> None:
    """A repair is accepted only against a fresh batch, never on the baseline."""
    for route in (dd.ROUTE_REPAIR, dd.ROUTE_PARTIAL_IMPROVEMENT):
        document = diagnosis_document(route=route)
        document.pop("verification")
        expect_rejected(lambda d=document: evaluate(d),
                        "requires a verification batch",
                        f"a {route} route with no verification")


def test_the_two_batches_may_share_a_root_but_not_an_invocation() -> None:
    """`--artifact-root` is optional, so a shared ROOT is legitimate.

    What no two invocations can share is the directory beneath it:
    `new_invocation_dir` creates a fresh collision-free one per
    invocation, stamped with the time, the pid and a uuid. Checking only
    the COMMAND's destinations let both batches omit `--artifact-root`,
    point at one invocation directory, and keep distinct `--result`
    paths — every per-batch rule passing while the verification reported
    the baseline's artifacts as its own.
    """
    def defaulted(section, tree, result):
        section["invocation"] = invocation(
            cmd=["python3", f"{tree}/tools/probe_flake.py", "--probe", PROBE,
                 "--runs", str(dd.RUN_COUNT), "--rts-caps",
                 str(dd.RTS_CAPABILITIES), "--result", result],
            directory=tree)

    document = diagnosis_document()
    defaulted(document["baseline"], CLEAN_WT, f"{OUTSIDE}/baseline.json")
    defaulted(document["verification"], REPAIR_WT, f"{OUTSIDE}/verify.json")
    shared_root = f"{OUTSIDE}/defaulted"
    document["baseline"]["result"] = result_document(
        runs=failing_runs(4), artifact_root=shared_root)
    document["verification"]["result"] = verification_result(
        artifact_root=shared_root)
    expect(document["baseline"]["result"]["invocation_dir"]
           == document["verification"]["result"]["invocation_dir"],
           "the fixture really does reuse one invocation directory")
    expect_refused(lambda: evaluate(document),
                   "creates a fresh one per invocation",
                   "two batches reporting one invocation directory")

    # The same shared root with distinct invocation directories is fine.
    document["verification"]["result"]["invocation_dir"] = (
        f"{shared_root}/{PROBE}-20260822T090000Z-5150-beefcafe")
    document["verification"]["result"]["runs"] = [
        dict(run) for run in document["verification"]["result"]["runs"]]
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"a shared artifact root is legitimate; got {outcome.route}")


def test_neither_batch_may_write_into_the_others_artifacts() -> None:
    """Distinct paths are not isolation if one sits inside the other.

    A verification `--result` pointing at the baseline's retained
    `run-001/events.jsonl` is a different path from anything the baseline
    wrote to, and it overwrites the very evidence the comparison is made
    of.
    """
    document = diagnosis_document()
    inside = (f"{document['baseline']['result']['invocation_dir']}"
              f"/run-001/events.jsonl")
    document["verification"]["invocation"] = invocation(
        cmd=command(result=inside, artifacts=VERIFY_ARTIFACTS,
                    worktree=REPAIR_WT),
        directory=REPAIR_WT)
    expect_refused(lambda: evaluate(document),
                   "invocation directory",
                   "a verification writing into the baseline's artifacts")

    # And the other direction: the baseline reporting a retained artifact
    # inside the verification's invocation directory.
    other = diagnosis_document(handoff=handoff_document(acceptable=1))
    other["verification"]["result"] = verification_result(
        runs=failing_runs(1, abort=False))
    victim = other["verification"]["result"]["invocation_dir"]
    failing = next(run for run in other["baseline"]["result"]["runs"]
                   if run["outcome"] != probe_flake.RUN_PASS)
    moved = f"{victim}/run-{failing['index']:03d}"
    other["baseline"]["result"]["retained_artifacts"] = [
        moved if path == failing["artifact_dir"] else path
        for path in other["baseline"]["result"]["retained_artifacts"]]
    failing["artifact_dir"] = moved
    expect_rejected(lambda: evaluate(other),
                    "evidence from somewhere other than this measurement",
                    "a baseline retaining artifacts in the verification's tree")


def test_a_run_sequence_is_the_one_the_loop_emits() -> None:
    """`measure` runs `range(1, runs + 1)`, one record per index.

    Ten records all numbered `1` is one run replayed ten times — and
    every other rule reads a run's index, so leaving the sequence
    unchecked let a forged layout satisfy them all against one number.
    """
    for label, renumber in (
            ("all the same", lambda runs: [1] * len(runs)),
            ("a repeat", lambda runs: [1, 1] + list(range(3, len(runs) + 1))),
            ("a skip", lambda runs: [1] + list(range(3, len(runs) + 2))),
            ("reordered", lambda runs: list(range(len(runs), 0, -1))),
            ("zero-based", lambda runs: list(range(0, len(runs)))),
    ):
        document = handoff_document()
        runs = document["result"]["runs"]
        for run, index in zip(runs, renumber(runs)):
            run["index"] = index
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        "numbers its runs",
                        f"a batch whose indices are {label}")

    broken = handoff_document(result=result_document(
        runs=failing_runs(2), harness_error=True))
    broken["result"]["error_run"]["index"] = 1
    expect_rejected(lambda: dd.require_handoff(broken),
                    "the one after the last completed one",
                    "a harness-error run numbered before the completed ones")


def test_the_two_batches_may_not_share_a_worktree() -> None:
    document = diagnosis_document()
    relocate_section(document["verification"], CLEAN_WT)
    expect_refused(lambda: evaluate(document), "not two separate states",
                   "a verification run in the clean comparison worktree")

    for label, declared in (("a trailing dot", f"{CLEAN_WT}/."),
                            ("a redundant step", f"{CLEAN_WT}/sub/..")):
        document = diagnosis_document()
        relocate_section(document["verification"], declared)
        expect_refused(lambda d=document: evaluate(d),
                       "not two separate states",
                       f"the same worktree spelled with {label}")

    nested = diagnosis_document()
    relocate_section(nested["verification"], f"{CLEAN_WT}/nested")
    expect_refused(lambda: evaluate(nested), "not two separate states",
                   "a repair worktree nested inside the clean one")


def test_a_section_must_measure_in_the_worktree_it_declares() -> None:
    document = diagnosis_document()
    document["verification"]["invocation"]["directory"] = "/tmp/somewhere-else"
    expect_rejected(lambda: evaluate(document),
                    "measures somewhere other than the worktree it names",
                    "a section whose invocation ran elsewhere")


# ==========================================================================
# Same-environment verification
# ==========================================================================
def test_destinations_and_ports_may_differ_and_nothing_else() -> None:
    outcome = evaluate(diagnosis_document())
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"different destinations and ports are fine; got {outcome.route}")

    # Changing only the COMMAND makes the record incoherent — the batch
    # says one thing and the document it produced says another — so it is
    # rejected before comparability is even asked about.
    for label, cmd in (
            ("run count", command(runs=20, result=f"{OUTSIDE}/verify.json",
                                  artifacts=VERIFY_ARTIFACTS,
                                  worktree=REPAIR_WT)),
            ("capabilities", command(rts_caps=8,
                                     result=f"{OUTSIDE}/verify.json",
                                     artifacts=VERIFY_ARTIFACTS,
                                     worktree=REPAIR_WT)),
    ):
        document = diagnosis_document()
        document["verification"]["invocation"] = invocation(
            cmd=cmd, directory=REPAIR_WT, ports=[9201])
        expect_rejected(lambda d=document: evaluate(d),
                        "describe one measurement",
                        f"a verification whose command changed the {label}")


def test_two_commands_that_agree_with_each_other_are_not_the_contract() -> None:
    """The comparison is to the contract, not only to the other batch.

    Both commands claiming twenty runs at eight capabilities compare
    EQUAL to each other. Each is bound to its own result document, so
    matching them needs result documents saying twenty and eight too —
    and those are not measurements this lab's policy is stated on, which
    routes them to #1439 instead of a pull request.
    """
    document = diagnosis_document(route=dd.ROUTE_PARTIAL_IMPROVEMENT)
    document["route"] = dd.ROUTE_CANNOT_REPRODUCE
    for key, dest, tree in (("baseline", f"{OUTSIDE}/baseline.json", CLEAN_WT),
                            ("verification", f"{OUTSIDE}/verify.json",
                             REPAIR_WT)):
        artifacts = (f"{OUTSIDE}/artifacts" if key == "baseline"
                     else VERIFY_ARTIFACTS)
        document[key]["invocation"] = invocation(
            cmd=command(runs=20, rts_caps=8, result=dest,
                        artifacts=artifacts, worktree=tree),
            directory=tree, ports=[9101])
    document["baseline"]["result"] = result_document(
        runs=failing_runs(4), requested=20, rts_caps=8, command_runs=20)
    document["verification"]["result"] = verification_result(
        requested=20, rts_caps=8, command_runs=20)
    expect(dd.invocation_differences(document["baseline"]["invocation"],
                                     document["verification"]["invocation"])
           == [], "the two commands really do compare equal to each other")
    # And that is not enough: the conditions travel the whole chain, so
    # both are measured against the HANDOFF's contract, not each other's.
    expect_refused(lambda: evaluate(document),
                   "did not replay the conditions the handoff was measured",
                   "two batches agreeing with each other but not the handoff")

    repair = copy.deepcopy(document)
    repair["route"] = dd.ROUTE_REPAIR
    expect_refused(lambda: evaluate(repair),
                   "did not replay the conditions the handoff was measured",
                   "a repair declared over two agreeing non-measurements")


def test_only_the_real_harness_options_are_accepted() -> None:
    """A plausible option the shipped CLI does not have is not a condition.

    `probe_flake.main` exposes no `--timeout`, so a pair of commands both
    carrying `--timeout 60` would compare EQUAL and pass same-environment
    validation while describing a measurement neither batch could have
    run. Every option is checked against the real surface instead.
    """
    for extra in (["--timeout", "60"], ["--start-port", "9500"],
                  ["--retries", "2"], ["--jobs", "4"]):
        document = diagnosis_document()
        document["verification"]["invocation"] = invocation(
            cmd=command(result=f"{OUTSIDE}/verify.json",
                        artifacts=VERIFY_ARTIFACTS,
                        worktree=REPAIR_WT) + extra,
            directory=REPAIR_WT, ports=[9201])
        expect_rejected(lambda d=document: evaluate(d),
                        "does not accept",
                        f"a command carrying {extra[0]}")


def test_an_integer_option_uses_the_harnesss_own_grammar() -> None:
    """`--runs 10.0` is numerically ten and argparse would refuse it.

    `probe_flake.main` declares both as `type=int`, so a float spelling
    exits before the harness measures anything — while a comparison that
    parsed it as a number would let the fabricated command compare equal
    to a real one.
    """
    for token in ("10.0", "1e1", " 10.5", "ten", "", "0x0a"):
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(
            cmd=command() [:4] + ["--runs", token] + [
                "--rts-caps", str(dd.RTS_CAPABILITIES),
                "--result", f"{OUTSIDE}/baseline.json",
                "--artifact-root", f"{OUTSIDE}/artifacts"])
        expect_rejected(lambda d=document: evaluate(d),
                        "must be an integer",
                        f"a --runs of {token!r}")

    for token in ("4.0", "four"):
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(
            cmd=command() [:6] + ["--rts-caps", token] + [
                "--result", f"{OUTSIDE}/baseline.json",
                "--artifact-root", f"{OUTSIDE}/artifacts"])
        expect_rejected(lambda d=document: evaluate(d),
                        "must be an integer",
                        f"a --rts-caps of {token!r}")

    # What `int()` does accept, this accepts, because that is argparse's
    # own grammar and nothing narrower.
    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(
        cmd=command() [:4] + ["--runs", f" {dd.RUN_COUNT} "] + [
            "--rts-caps", str(dd.RTS_CAPABILITIES),
            "--result", f"{OUTSIDE}/baseline.json",
            "--artifact-root", f"{OUTSIDE}/artifacts"])
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"a spelling argparse accepts is accepted; got {outcome.route}")


def test_a_command_that_wrote_no_result_document_produced_no_evidence() -> None:
    """`probe_flake.main` writes the document only `if args.result`."""
    for section in ("baseline", "verification"):
        document = diagnosis_document()
        cmd = [token for token in command()]
        index = cmd.index("--result")
        del cmd[index:index + 2]
        tree = CLEAN_WT if section == "baseline" else REPAIR_WT
        cmd = [f"{tree}/tools/probe_flake.py" if token.endswith(
            "/tools/probe_flake.py") else token for token in cmd]
        document[section]["invocation"] = invocation(
            cmd=cmd, directory=tree)
        expect_rejected(lambda d=document: evaluate(d), "names no --result",
                        f"a {section} command with no result destination")

    # Not the handoff: `/deflake` retains the document beside its
    # artifacts either way, so its `--result` is genuinely optional.


def test_the_artifact_layout_is_the_one_the_harness_creates() -> None:
    """Three recorded values determine the whole layout, and nothing else.

    `new_invocation_dir` puts the invocation directory directly under the
    artifact root and names it after the probe; every run directory is
    `invocation_dir / f"run-{index:03d}"`. Containment alone let a batch
    swap a failed run's directory for an unrelated external path and keep
    `repair-pr`.
    """
    nested = handoff_document()
    result = nested["result"]
    result["invocation_dir"] = f"{OUTSIDE}/artifacts/deeper/{PROBE}-x-1-a"
    expect_rejected(lambda: dd.require_handoff(nested),
                    "DIRECT child of the root",
                    "an invocation directory two levels under the root")

    for label, name in (
            ("not named after the probe", f"{OTHER}-20260821T120000Z-1-abcdef12"),
            ("a name the harness never generates", "invocation"),
            ("a name with no uuid", f"{PROBE}-20260821T120000Z-1"),
            ("a name with a bad stamp", f"{PROBE}-2026-08-21-1-abcdef12"),
            ("a name with non-hex", f"{PROBE}-20260821T120000Z-1-abcdefgh")):
        misnamed = handoff_document()
        misnamed["result"]["invocation_dir"] = f"{OUTSIDE}/artifacts/{name}"
        expect_rejected(lambda d=misnamed: dd.require_handoff(d),
                        "not a directory this measurement created",
                        f"an invocation directory {label}")

    for label, key in (("an artifact root", "artifact_root"),
                       ("an invocation directory", "invocation_dir")):
        relative = handoff_document()
        relative["result"][key] = "artifacts/relative"
        expect_rejected(lambda d=relative: dd.require_handoff(d),
                        "every path a real result document carries is "
                        "absolute",
                        f"{label} recorded as a relative path")

    for label, replacement in (
            ("an unrelated external path", f"{OUTSIDE}/elsewhere/run-001"),
            ("another run's directory", None),
            ("a sibling of the invocation directory", None)):
        document = handoff_document()
        result = document["result"]
        failing = next(run for run in result["runs"]
                       if run["outcome"] != probe_flake.RUN_PASS)
        if replacement is None:
            other = next(run for run in result["runs"]
                         if run["outcome"] != probe_flake.RUN_PASS
                         and run["index"] != failing["index"])
            replacement = (other["artifact_dir"] if label.startswith("another")
                           else str(Path(result["invocation_dir"]).parent
                                    / f"run-{failing['index']:03d}"))
        result["retained_artifacts"] = [
            replacement if path == failing["artifact_dir"] else path
            for path in result["retained_artifacts"]]
        failing["artifact_dir"] = replacement
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        "evidence from somewhere other than this measurement",
                        f"a run directory replaced by {label}")

    broken = handoff_document(result=result_document(
        runs=failing_runs(2), harness_error=True))
    broken["result"]["error_run"]["artifact_dir"] = f"{OUTSIDE}/elsewhere/run"
    broken["result"]["retained_artifacts"][-1] = f"{OUTSIDE}/elsewhere/run"
    expect_rejected(lambda: dd.require_handoff(broken),
                    "evidence from somewhere other than this measurement",
                    "a harness-error run directory somewhere else")


def test_only_a_python_three_interpreter_is_accepted() -> None:
    """These are Python 3 programs; `python2` is a SyntaxError, not a run.

    Bare `python` is refused for a different reason: it is whichever of
    the two that machine happens to mean, which a document cannot settle.
    """
    for program in ("python", "python2", "python2.7", "pypy", "python4x"):
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(
            cmd=[program] + command()[1:])
        expect_rejected(lambda d=document: evaluate(d),
                        "not a Python interpreter",
                        f"a command run by {program!r}")

    for program in ("python3", "python3.12", "python3.14.2"):
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(
            cmd=[program] + command()[1:])
        outcome = evaluate(document)
        expect(outcome.route == dd.ROUTE_REPAIR,
               f"{program!r} is a supported spelling; got {outcome.route}")


def test_the_handoff_comes_from_the_primary_checkout() -> None:
    """A path cannot assert that it is a checkout, so one must be named.

    `/deflake` runs in the primary checkout — it is the step BEFORE this
    workflow creates its comparison worktrees, and it claims a probe and
    writes the census from there.
    """
    for label, elsewhere in (
            ("an invented root", "/tmp/not-a-synarchy-checkout"),
            ("the clean comparison worktree", CLEAN_WT),
            ("the repair worktree", REPAIR_WT)):
        document = diagnosis_document()
        document["handoff"]["invocation"] = deflake_invocation(
            cmd=["python3", f"{elsewhere}/tools/deflake.py", "--json",
                 "--result", f"{OUTSIDE}/handoff.json"],
            directory=elsewhere)
        expect_rejected(lambda d=document: evaluate(d),
                        "is not the primary checkout",
                        f"a handoff claiming to have run in {label}")

    # Spelled differently, the same checkout is still the same checkout.
    document = diagnosis_document()
    document["handoff"]["invocation"] = deflake_invocation(
        cmd=["python3", f"{PRIMARY_WT}/./tools/deflake.py", "--json",
             "--result", f"{OUTSIDE}/handoff.json"],
        directory=f"{PRIMARY_WT}/.")
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"a canonically equal spelling is accepted; got {outcome.route}")

    expect(dd.primary_checkout() == dd.worktree_paths()[0],
           "and the primary checkout is the head of the registered list")


def test_a_result_path_must_be_spelled_the_way_resolve_spells_it() -> None:
    """`check_artifact_root` resolves, so a real path has no `.` or `..`.

    Normalising a supplied path before comparing it would accept
    `/tmp/evidence/forged/../artifacts/…`, which the harness could not
    have written and which points somewhere else entirely if any
    component is a symlink.
    """
    # Written straight onto the document: `Path` collapses `.`, doubled
    # separators and a trailing slash as it is CONSTRUCTED, so a fixture
    # that went through it could not produce these spellings — which is
    # also why no real result document carries one.
    for label, root in (
            ("a parent step", f"{OUTSIDE}/forged/../artifacts"),
            ("a self step", f"{OUTSIDE}/./artifacts"),
            ("a doubled separator", f"{OUTSIDE}//artifacts"),
            ("a trailing slash", f"{OUTSIDE}/artifacts/")):
        document = handoff_document()
        document["result"]["artifact_root"] = root
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        "not the spelling `Path.resolve` produces",
                        f"an artifact root with {label}")

    document = handoff_document()
    failing = next(run for run in document["result"]["runs"]
                   if run["outcome"] != probe_flake.RUN_PASS)
    forged = failing["artifact_dir"].replace("/run-", "/./run-")
    document["result"]["retained_artifacts"] = [
        forged if path == failing["artifact_dir"] else path
        for path in document["result"]["retained_artifacts"]]
    failing["artifact_dir"] = forged
    expect_rejected(lambda: dd.require_handoff(document),
                    "not the spelling `Path.resolve` produces",
                    "a run directory with a self step")


def test_an_unresolved_symlink_is_not_the_serialized_path() -> None:
    """`check_artifact_root` RESOLVES, so a real path has none left.

    Driven against a real symlink rather than a hard-coded platform
    quirk, so it means the same thing on a host where `/tmp` is a real
    directory as on one where it is a link to `/private/tmp`.
    """
    root = Path(tempfile.mkdtemp(prefix="test_deflake_diagnosis_")).resolve()
    try:
        (root / "real").mkdir()
        (root / "link").symlink_to(root / "real")
        document = handoff_document()
        document["result"]["artifact_root"] = str(root / "link" / "artifacts")
        expect_rejected(lambda: dd.require_handoff(document),
                        "not the spelling `Path.resolve` produces",
                        "an artifact root reached through a symlink")

        resolved = handoff_document(result=result_document(
            runs=failing_runs(3),
            artifact_root=str(root / "real" / "artifacts")))
        dd.require_handoff(resolved)
    finally:
        shutil.rmtree(root, ignore_errors=True)


def test_a_relative_script_resolves_from_the_directory_it_ran_in() -> None:
    """Python resolves a relative script path from the CWD, not the checkout.

    So an invocation in a SUBDIRECTORY of the declared worktree can write
    `tools/probe_flake.py` and mean a counterfeit nested beside it —
    which resolving against the checkout would have compared to the real
    tool and accepted.
    """
    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(
        cmd=["python3", "tools/probe_flake.py", "--probe", PROBE,
             "--runs", str(dd.RUN_COUNT), "--rts-caps",
             str(dd.RTS_CAPABILITIES), "--result", f"{OUTSIDE}/baseline.json",
             "--artifact-root", f"{OUTSIDE}/artifacts"],
        directory=f"{CLEAN_WT}/nested")
    expect_rejected(lambda: evaluate(document),
                    "the checkout it declares keeps that tool at",
                    "a relative script naming a counterfeit nested tool")

    # The same relative spelling from the checkout ROOT is the real tool.
    fine = diagnosis_document()
    fine["baseline"]["invocation"] = invocation(
        cmd=["python3", "tools/probe_flake.py", "--probe", PROBE,
             "--runs", str(dd.RUN_COUNT), "--rts-caps",
             str(dd.RTS_CAPABILITIES), "--result", f"{OUTSIDE}/baseline.json",
             "--artifact-root", f"{OUTSIDE}/artifacts"],
        directory=CLEAN_WT)
    outcome = evaluate(fine)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"a relative script from the checkout root is fine; got "
           f"{outcome.route}")


def test_a_relabelled_check_has_changed_what_it_measures() -> None:
    """A label is the check's stated MEANING, not decoration."""
    relabelled = [("alpha", "the first check"),
                  ("beta", "an entirely different assertion"),
                  ("gamma", "the third check")]
    document = diagnosis_document()
    document["verification"]["result"] = verification_result(
        checks=relabelled,
        runs=[{cid: PASS for cid, _l in relabelled}] * dd.RUN_COUNT)
    expect_rejected(lambda: evaluate(document), "relabels",
                    "a verification that relabelled a check")

    baseline = diagnosis_document()
    baseline["baseline"]["result"] = result_document(
        checks=relabelled, runs=failing_runs(4, declared=relabelled))
    expect_rejected(lambda: evaluate(baseline), "relabels",
                    "a baseline that relabelled a check")

    expect(dd.descriptor_of(result_document()) ==
           [{"id": cid, "label": label} for cid, label in CHECKS],
           "the descriptor is compared as identifiers AND labels")


def test_a_repair_may_not_change_the_measurement_apparatus() -> None:
    """The probe is under diagnosis; the harness that measures it is not.

    `probe_flake.measure`'s timeout and starting port are module
    constants neither CLI exposes, so a repair that lengthened
    `DEFAULT_TIMEOUT` would produce a calmer verification while both
    command records still compared equal — the two batches would have
    been run by different harnesses.
    """
    for module in dd.HARNESS_MODULES:
        document = diagnosis_document(repair={
            "commit_sha": REPAIR_COMMIT, "base_sha": BASE_COMMIT,
            "changed_paths": ["tools/role_probe.py", module]})
        expect_rejected(lambda d=document: evaluate(d),
                        "measurement apparatus",
                        f"a repair that changed {module}")

    expect("tools/probe_flake.py" in dd.HARNESS_MODULES
           and "tools/probe_protocol.py" in dd.HARNESS_MODULES
           and "tools/deflake.py" in dd.HARNESS_MODULES,
           f"the apparatus names the tools that decide what a run IS: "
           f"{dd.HARNESS_MODULES}")
    expect("tools/role_probe.py" not in dd.HARNESS_MODULES,
           "and not the probes it runs")


def test_the_defaults_no_command_line_names_are_pinned() -> None:
    """Neither CLI exposes them, so the default is the only value there is.

    A record naming another one describes a run that did not happen —
    which is what makes this stronger than comparing the three records to
    each other: setting all three to the same arbitrary value would agree
    perfectly and still be fiction.
    """
    for field, wrong in (("timeout_seconds", probe_flake.DEFAULT_TIMEOUT * 3),
                         ("start_port", probe_flake.PORT_MIN + 200)):
        for section in ("baseline", "verification"):
            absent = diagnosis_document()
            del absent[section]["invocation"][field]
            expect_rejected(lambda d=absent: evaluate(d),
                            f"records no `{field}`",
                            f"a {section} that recorded no {field}")

        # All three altered together, which no comparison between them
        # could catch.
        document = diagnosis_document()
        document["handoff"]["invocation"][field] = wrong
        for section in ("baseline", "verification"):
            document[section]["invocation"][field] = wrong
        expect_rejected(lambda d=document: evaluate(d),
                        "the only value a real measurement can have used",
                        f"every record altered to another {field}")

        handoff = handoff_document()
        del handoff["invocation"][field]
        expect_rejected(lambda d=handoff: dd.require_handoff(d),
                        f"records no `{field}`",
                        f"a handoff that recorded no {field}")


def test_the_conditions_a_measurement_ran_under_include_the_defaults() -> None:
    """`effective_settings` carries them, even though pinning hides it.

    `require_invocation` pins `timeout_seconds` and `start_port` to the
    harness's own values, so two well-formed records can never differ
    here and no end-to-end case can reach this comparison. It is asserted
    directly instead: the settings are what a measurement RAN UNDER, and
    a future `--timeout` flag would make the comparison load-bearing
    without anyone having to remember to add it.
    """
    settings = dd.effective_settings(invocation(), "invocation",
                                     result=result_document())
    expect(settings["timeout_seconds"] == probe_flake.DEFAULT_TIMEOUT,
           f"the timeout is a condition: {settings}")
    expect(settings["start_port"] == probe_flake.PORT_MIN,
           f"and so is the starting port: {settings}")

    altered = invocation(timeout=probe_flake.DEFAULT_TIMEOUT * 2,
                         start_port=probe_flake.PORT_MIN + 1)
    differences = dd.invocation_differences(
        invocation(), altered,
        results=(result_document(), result_document()))
    expect(any("timeout_seconds" in d for d in differences)
           and any("start_port" in d for d in differences),
           f"and both are compared when they differ: {differences}")


def test_the_baseline_replays_the_handoffs_own_conditions() -> None:
    """The chain is handoff -> baseline -> verification, not a pair.

    Comparing only the last pair let BOTH controlled batches agree on
    some arbitrary condition while the handoff sat at the defaults — and
    an agreement between two batches is not the measurement the handoff
    was taken under.

    Driven with the RUN COUNT and CAPABILITY COUNT, which are the
    conditions a command line can actually carry. `timeout_seconds` and
    `start_port` are pinned to the harness's own values by
    `require_invocation` before any comparison, so they cannot differ
    between two well-formed records at all.
    """
    document = diagnosis_document()
    for section in ("baseline", "verification"):
        tree = CLEAN_WT if section == "baseline" else REPAIR_WT
        artifacts = (f"{OUTSIDE}/artifacts" if section == "baseline"
                     else VERIFY_ARTIFACTS)
        document[section]["invocation"] = invocation(
            cmd=command(rts_caps=8, worktree=tree, artifacts=artifacts,
                        result=(f"{OUTSIDE}/baseline.json"
                                if section == "baseline"
                                else f"{OUTSIDE}/verify.json")),
            directory=tree)
        document[section]["result"] = (
            result_document(runs=failing_runs(4), rts_caps=8)
            if section == "baseline"
            else verification_result(rts_caps=8))
    expect(dd.invocation_differences(document["baseline"]["invocation"],
                                     document["verification"]["invocation"],
                                     results=(document["baseline"]["result"],
                                              document["verification"]["result"]))
           == [], "the two controlled batches agree with each other")
    expect_refused(lambda: evaluate(document),
                   "did not replay the conditions the handoff",
                   "two batches at a capability count /deflake never used")

    # The refusal names BOTH sides, so a reader can tell which value came
    # from where rather than seeing one label twice.
    try:
        evaluate(document)
    except dd.RouteRefused as error:
        message = str(error)
        expect("handoff 4" in message and "baseline 8" in message,
               f"the refusal names each side's own value: {message}")
    else:
        FAILURES.append("a baseline that did not replay was accepted")


def test_a_changed_path_is_repository_relative_and_traversal_free() -> None:
    """`tools/../src/…` begins with `tools/` and changes production code."""
    for label, path in (
            ("a traversal out of tools", "tools/../src/Engine/Core/Init.hs"),
            ("a traversal to the root", "tools/../../etc/passwd"),
            ("a self step", "tools/./role_probe.py"),
            ("a doubled separator", "tools//role_probe.py")):
        document = diagnosis_document(repair={
            "commit_sha": REPAIR_COMMIT, "base_sha": BASE_COMMIT,
            "changed_paths": [path]})
        expect_rejected(lambda d=document: evaluate(d),
                        "normalised repository-relative form",
                        f"a changed path with {label}")

    document = diagnosis_document(repair={
        "commit_sha": REPAIR_COMMIT, "base_sha": BASE_COMMIT,
        "changed_paths": [f"{CLEAN_WT}/tools/role_probe.py"]})
    expect_rejected(lambda: evaluate(document), "absolute path",
                    "a changed path given absolutely")


def test_a_generated_directory_name_names_a_real_instant_and_process() -> None:
    """`\\d{8}T\\d{6}Z` matches `99999999T999999Z`; no clock produced that."""
    for label, name in (
            ("an impossible date", f"{PROBE}-99999999T999999Z-4711-abcdef12"),
            ("an impossible month", f"{PROBE}-20261321T120000Z-4711-abcdef12"),
            ("an impossible hour", f"{PROBE}-20260821T250000Z-4711-abcdef12"),
            ("a process id of zero", f"{PROBE}-20260821T120000Z-0-abcdef12")):
        document = handoff_document()
        document["result"]["invocation_dir"] = f"{OUTSIDE}/artifacts/{name}"
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        "not a directory this measurement created",
                        f"an invocation directory with {label}")


def test_a_measurements_timestamp_is_an_instant() -> None:
    """Delegated to `probe_census.parse_timestamp`, the shipped reader."""
    for label, stamp in (("an impossible date", "2026-99-99T99:99:99Z"),
                         ("no timezone marker", "2026-08-21T12:00:00"),
                         ("a date alone", "2026-08-21"),
                         ("nothing at all", None),
                         ("a number", 20260821)):
        document = handoff_document()
        document["result"]["timestamp_utc"] = stamp
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        "timestamp", f"a measurement stamped with {label}")

    for section in ("baseline", "verification"):
        document = diagnosis_document()
        document[section]["result"]["timestamp_utc"] = "2026-99-99T99:99:99Z"
        expect_rejected(lambda d=document: evaluate(d), "timestamp",
                        f"a {section} stamped with an impossible date")


def test_a_malformed_list_is_a_refusal_not_a_traceback() -> None:
    """`list(42)` raises `TypeError`; a document must never do that."""
    for field in ("expected_checks", "targets"):
        for value in (42, "beta", {"beta": True}, [42], [""], [None]):
            document = handoff_document()
            document[field] = value
            expect_rejected(lambda d=document: dd.require_handoff(d),
                            "must be a list of identifiers",
                            f"a {field} of {value!r}")


def test_a_fabricated_argv_is_not_a_harness_invocation() -> None:
    for label, fragment, cmd in (
            ("another script", "the programs that produce",
             ["python3", f"{CLEAN_WT}/tools/run_probes.py",
              "--probe", PROBE, "--runs", "10"]),
            ("an extra positional", "positional token",
             ["python3", f"{CLEAN_WT}/tools/probe_flake.py", "extra",
              "--probe", PROBE, "--runs", "10",
              "--result", f"{OUTSIDE}/b.json"]),
            ("no script at all", "not a Python interpreter",
             ["--probe", PROBE, "--runs", "10"]),
            # The right SHAPE, running something that measures nothing.
            ("a program that is not an interpreter",
             "runs the interpreter by path",
             ["/bin/echo", f"{CLEAN_WT}/tools/probe_flake.py",
              "--probe", PROBE, "--runs", "10"]),
            ("a counterfeit interpreter", "runs the interpreter by path",
             ["/tmp/counterfeit/python3", f"{CLEAN_WT}/tools/probe_flake.py",
              "--probe", PROBE, "--runs", "10",
              "--result", f"{OUTSIDE}/b.json"]),
            ("a counterfeit script", "the checkout it declares keeps that tool at",
             ["python3", "/tmp/counterfeit/probe_flake.py",
              "--probe", PROBE, "--runs", "10",
              "--result", f"{OUTSIDE}/b.json"]),
            ("a shell", "not a Python interpreter",
             ["sh", f"{CLEAN_WT}/tools/probe_flake.py",
              "--probe", PROBE, "--runs", "10"]),
            # Order is part of the grammar: Python rejects an option it
            # does not know BEFORE it runs the script.
            ("an option before the script", "before the script it ran",
             ["python3", "--probe", PROBE,
              f"{CLEAN_WT}/tools/probe_flake.py", "--runs", "10",
              "--result", f"{OUTSIDE}/b.json"]),
    ):
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(cmd=cmd)
        expect_rejected(lambda d=document: evaluate(d), fragment,
                        f"a command with {label}")


def test_an_absent_option_compares_as_its_effective_default() -> None:
    """"The caller declined to override a default" is not a difference."""
    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(cmd=[
        "python3", "/tmp/deflake-clean-role/tools/probe_flake.py",
        "--probe", PROBE, "--runs", str(dd.RUN_COUNT),
        "--result", f"{OUTSIDE}/baseline.json",
        "--artifact-root", f"{OUTSIDE}/artifacts"])
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_REPAIR,
           "an omitted --rts-caps equals an explicit one at the default")


def _shipped_options(script: str) -> set:
    done = subprocess.run(
        [sys.executable, str(Path(TOOL).parent / script), "--help"],
        capture_output=True, text=True, timeout=120)
    expect(done.returncode == 0, f"{script} --help exits 0: {done.stderr}")
    real = set(re.findall(r"(?<![\w-])(--[a-z][a-z-]*)", done.stdout))
    real.discard("--help")
    return real


def test_the_option_tables_match_the_shipped_tools() -> None:
    """Drift guard: the real `--help` is the authority on each surface.

    Both tools build their parsers inside `main`, so the tables here are
    hard-coded — and this reads each shipped CLI's own help output, so
    adding, removing or renaming an option fails here instead of
    silently widening what this module will accept.
    """
    for launcher in (dd.HARNESS_LAUNCHER, dd.DEFLAKE_LAUNCHER):
        real = _shipped_options(launcher.script)
        expect(real == set(launcher.options),
               f"{launcher.script}'s classified options "
               f"{sorted(launcher.options)} are exactly the shipped ones "
               f"{sorted(real)}")
        expect(set(launcher.required) <= set(launcher.options),
               f"and {launcher.script}'s required options are ones it has")
        expect(set(launcher.destinations) <= set(launcher.options),
               f"and so are its destinations")
        expect(launcher.values.isdisjoint(launcher.flags),
               f"and no {launcher.script} option both takes a value and does not")

    real = _shipped_options(dd.HARNESS_LAUNCHER.script)
    expect(real == set(dd.HARNESS_OPTIONS),
           f"the classified options {sorted(dd.HARNESS_OPTIONS)} are exactly "
           f"the shipped ones {sorted(real)}")
    expect(dd.DEFLAKE_LAUNCHER.fixed["runs"] == deflake.CENSUS_RUN_COUNT
           and dd.DEFLAKE_LAUNCHER.fixed["rts_caps"] == deflake.RTS_CAPABILITIES,
           "and /deflake's fixed contract is its own module's constants")
    expect(set(dd.CONDITION_OPTIONS).isdisjoint(dd.DESTINATION_OPTIONS),
           "and no option is both a condition and a destination")
    expect(set(dd.HARNESS_OPTIONS) ==
           set(dd.CONDITION_OPTIONS) | set(dd.DESTINATION_OPTIONS),
           "every classified option is a condition or a destination")
    expect(set(dd.REQUIRED_OPTIONS) <= set(dd.HARNESS_OPTIONS),
           "and every required option is one the harness has")


# ==========================================================================
# Stable check identity
# ==========================================================================
def test_a_renamed_identifier_is_refused() -> None:
    document = diagnosis_document()
    renamed = [("alpha", "the first check"), ("beta_two", "renamed"),
               ("gamma", "the third check")]
    document["verification"]["result"] = verification_result(checks=renamed,
        runs=[{cid: PASS for cid, _l in renamed}] * dd.RUN_COUNT)
    expect_rejected(lambda: evaluate(document),
                    "separately approved protocol change",
                    "a verification that renamed a check")


def test_a_removed_identifier_is_refused() -> None:
    document = diagnosis_document()
    fewer = [("alpha", "the first check"), ("beta", "the second check")]
    document["verification"]["result"] = verification_result(checks=fewer,
        runs=[{cid: PASS for cid, _l in fewer}] * dd.RUN_COUNT)
    expect_rejected(lambda: evaluate(document),
                    "separately approved protocol change",
                    "a verification that dropped a check")


def test_a_reordered_descriptor_is_refused() -> None:
    document = diagnosis_document()
    swapped = [CHECKS[1], CHECKS[0], CHECKS[2]]
    document["verification"]["result"] = verification_result(checks=swapped,
        runs=[{cid: PASS for cid, _l in swapped}] * dd.RUN_COUNT)
    expect_rejected(lambda: evaluate(document),
                    "separately approved protocol change",
                    "a verification that reordered the descriptor")


def test_an_identifier_carrying_a_runtime_value_is_malformed() -> None:
    """`probe-result/v1` identifiers are static; a digit is how a value gets in.

    The descriptor and every run agree on the name, so the identifier's
    SHAPE is the only rule this fixture breaks — a document that merely
    disagreed with itself would be caught by the undeclared-identifier
    rule instead and prove nothing about this one.
    """
    valued = [("alpha", "the first check"), ("beta_two", "the second check"),
              ("gamma", "the third check")]
    document = handoff_document(result=result_document(
        checks=valued,
        runs=failing_runs(3, cid="beta_two", declared=valued)))
    accepted = dd.require_handoff(copy.deepcopy(document))
    expect(accepted.probe == PROBE,
           "a spelled-out number is a legitimate identifier")
    for entry in document["result"]["checks"]:
        if entry["id"] == "beta_two":
            entry["id"] = "beta_2"
    for run in document["result"]["runs"]:
        run["checks"]["beta_2"] = run["checks"].pop("beta_two")
    document["result"]["check_counts"]["beta_2"] = (
        document["result"]["check_counts"].pop("beta_two"))
    document["targets"] = [("beta_2" if cid == "beta_two" else cid)
                           for cid in document["targets"]]
    expect_rejected(lambda: dd.require_handoff(document),
                    "identifiers are static",
                    "an identifier carrying a measured value")


def test_a_run_reporting_an_undeclared_identifier_is_malformed() -> None:
    """Delegated: `probe_census.validate_result` owns the tally rules."""
    document = handoff_document()
    document["result"]["runs"][0]["checks"]["delta"] = PASS
    expect_rejected(lambda: dd.require_handoff(document),
                    "internally inconsistent",
                    "a run reporting an undeclared check")


def test_a_run_that_simply_omits_a_declared_identifier_is_malformed() -> None:
    """A key the harness always writes, absent with a tally that agrees.

    Kept as a rule of this module's own because it is exactly the shape
    the canonical validator cannot see: `check_counts` is derived from
    `runs`, so a document whose tally was lowered to match the omission
    is internally consistent and still was not written by the harness.
    """
    document = handoff_document()
    dropped = document["result"]["runs"][0]["checks"].pop("gamma")
    document["result"]["check_counts"]["gamma"][dropped] -= 1
    expect_rejected(lambda: dd.require_handoff(document),
                    "was not written by the harness",
                    "a run whose check map lost a key")


# ==========================================================================
# MISSING
# ==========================================================================
def test_a_target_has_zero_missing_however_many_runs_may_fail() -> None:
    """The approved rule, isolated, and independent of X.

    Asserted against `missing_problems` directly, because inside
    `evaluate` the failure COUNT also refuses a batch with three aborted
    runs — driving it end to end would prove the count rule and say
    nothing about this one.
    """
    document = result_document(commit=REPAIR_COMMIT,
                               runs=failing_runs(3, cid="beta"))
    problems = dd.missing_problems(document, targets={"gamma"},
                                   what="the verification batch")
    expect(any("zero MISSING" in problem for problem in problems),
           f"gamma is MISSING in the aborted runs, so it is refused: "
           f"{problems}")

    expect(dd.missing_problems(document, targets={"beta"},
                               what="the verification batch") == [],
           "beta is emitted in every run, so it is not")

    spotless = result_document(commit=REPAIR_COMMIT)
    expect(dd.missing_problems(spotless, targets={"gamma"}, what="x") == [],
           "and a batch that aborted nowhere satisfies it outright")


def test_a_target_that_stops_being_emitted_is_refused_end_to_end() -> None:
    """One accepted failing run may lose it; more than X may not."""
    handoff = handoff_document(acceptable=1)
    document = diagnosis_document(handoff=handoff)
    document["verification"]["result"] = verification_result(runs=failing_runs(2))
    expect_refused(lambda: evaluate(document), "partial-improvement",
                   "a target lost in more runs than X allows")


def test_a_passing_run_may_not_omit_a_check() -> None:
    document = diagnosis_document()
    runs = [{"alpha": PASS, "beta": PASS, "gamma": MISSING}]
    runs += [{cid: PASS for cid, _l in CHECKS}] * (dd.RUN_COUNT - 1)
    document["verification"]["result"] = verification_result(runs=runs)
    expect_refused(lambda: evaluate(document),
                   "passed while omitting",
                   "a passing run that omitted a check")


def test_an_accepted_failing_run_may_abort_after_the_targets() -> None:
    """The suffix allowance is for the checks that are NOT targets.

    A target has zero MISSING across all ten runs, so an accepted failing
    run may abort — but only AFTER every target. Here the handoff fails
    at `alpha` without aborting, so `alpha` alone is the target, and the
    verification's one accepted failing run aborts at `beta`, losing only
    non-targets.
    """
    handoff = handoff_document(acceptable=1, result=result_document(
        runs=failing_runs(3, cid="alpha", abort=False)))
    expect(list(handoff["targets"]) == ["alpha"],
           f"a non-aborting failure implicates only itself; got "
           f"{handoff['targets']}")
    document = diagnosis_document(handoff=handoff)
    document["baseline"]["result"] = result_document(
        runs=failing_runs(4, cid="alpha", abort=False))
    document["verification"]["result"] = verification_result(
        runs=failing_runs(1, cid="beta"))
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"an abort after every target is accepted; got {outcome.route}")

    # A run that aborted BEFORE the target is refused, however few such
    # runs the batch has: a run that never reached the target did not
    # demonstrate the target was fixed.
    losing = diagnosis_document(handoff=handoff)
    losing["baseline"]["result"] = result_document(
        runs=failing_runs(4, cid="alpha", abort=False))
    runs = [{"__timeout__": True, "alpha": MISSING, "beta": MISSING,
             "gamma": MISSING}]
    runs += [{cid: PASS for cid, _l in CHECKS}] * (dd.RUN_COUNT - 1)
    losing["verification"]["result"] = verification_result(runs=runs)
    expect_refused(lambda: evaluate(losing),
                   "a target has zero MISSING across all",
                   "an accepted failing run that lost a target")


def test_a_non_contiguous_gap_is_malformed_rather_than_an_abort() -> None:
    document = diagnosis_document()
    document["handoff"]["acceptable_failures"] = 1
    runs = [{"alpha": MISSING, "beta": FAIL, "gamma": MISSING}]
    runs += [{cid: PASS for cid, _l in CHECKS}] * (dd.RUN_COUNT - 1)
    document["verification"]["result"] = verification_result(runs=runs)
    expect_refused(lambda: evaluate(document), "contiguous suffix",
                   "a run with a hole in the middle of its results")


def test_an_identifier_that_vanishes_from_the_batch_is_refused() -> None:
    document = diagnosis_document()
    document["handoff"]["acceptable_failures"] = 3
    runs = [{"alpha": PASS, "beta": FAIL, "gamma": MISSING}] * dd.RUN_COUNT
    document["verification"]["result"] = verification_result(runs=runs)
    expect_refused(lambda: evaluate(document),
                   "never emitted gamma",
                   "a check that was never emitted in the whole batch")


def test_a_missing_violation_is_the_partial_improvement_route() -> None:
    """At or below X but with a MISSING result is #1439, not a PR."""
    document = diagnosis_document(route=dd.ROUTE_PARTIAL_IMPROVEMENT)
    runs = [{"alpha": PASS, "beta": PASS, "gamma": MISSING}]
    runs += [{cid: PASS for cid, _l in CHECKS}] * (dd.RUN_COUNT - 1)
    document["verification"]["result"] = verification_result(runs=runs)
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_PARTIAL_IMPROVEMENT,
           f"a MISSING violation routes to #1439; got {outcome.route}")
    expect(outcome.owner_issue == 1439, "and names its owner")


# ==========================================================================
# Every route
# ==========================================================================
def test_the_repair_route_is_the_only_one_that_opens_a_pull_request() -> None:
    outcome = evaluate(diagnosis_document())
    expect(outcome.route == dd.ROUTE_REPAIR, "a verified repair is a repair")
    expect(outcome.opens_pull_request, "and it opens the one pull request")
    expect(outcome.owner_issue is None, "with no downstream owner")


def test_cannot_reproduce_hands_off_to_1439() -> None:
    document = diagnosis_document(route=dd.ROUTE_CANNOT_REPRODUCE)
    document["baseline"]["result"] = result_document()
    document.pop("verification", None)
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_CANNOT_REPRODUCE, "the route holds")
    expect(outcome.owner_issue == 1439, "and hands off to #1439")
    expect(not outcome.opens_pull_request, "opening no pull request")


def test_every_non_repair_route_preserves_its_evidence() -> None:
    """#1439 and #1438 receive the evidence, so a route without it is refused."""
    for route in (dd.ROUTE_CANNOT_REPRODUCE, dd.ROUTE_PRODUCTION_DEFECT,
                  dd.ROUTE_NO_CONFIDENT_FIX, dd.ROUTE_PARTIAL_IMPROVEMENT):
        document = diagnosis_document(route=route)
        if route == dd.ROUTE_CANNOT_REPRODUCE:
            document["baseline"]["result"] = result_document()
            document.pop("verification", None)
        if route == dd.ROUTE_PARTIAL_IMPROVEMENT:
            document["verification"]["result"] = verification_result(runs=failing_runs(2))
        del document["diagnosis"]
        expect_rejected(lambda d=document: evaluate(d),
                        "states no diagnosis",
                        f"a {route} route with no diagnosis")


def test_cannot_reproduce_is_refused_when_it_did_reproduce() -> None:
    document = diagnosis_document(route=dd.ROUTE_CANNOT_REPRODUCE)
    expect_refused(lambda: evaluate(document), "DID reproduce",
                   "cannot-reproduce declared over a reproducing baseline")


def test_the_production_defect_route_hands_off_to_1438() -> None:
    document = diagnosis_document(route=dd.ROUTE_PRODUCTION_DEFECT,
                                  diagnosis={
                                      "summary": "the engine really does "
                                                 "drop the order",
                                      "evidence": ["the engine log shows the "
                                                   "order accepted and never "
                                                   "executed"]})
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_PRODUCTION_DEFECT, "the route holds")
    expect(outcome.owner_issue == 1438, "and hands off to #1438")
    expect(not outcome.opens_pull_request, "opening no pull request")


def test_a_non_repair_route_may_not_carry_a_verification_batch() -> None:
    """A verification means a repair was attempted, so the route is wrong."""
    document = diagnosis_document(route=dd.ROUTE_PRODUCTION_DEFECT,
                                  diagnosis={"summary": "the product is wrong",
                                             "evidence": ["engine log"]})
    document["verification"] = {
        "worktree": REPAIR_WT, "source_clean": True,
        "result": verification_result(),
        "invocation": invocation(directory=REPAIR_WT),
        "configuration": manifest(),
    }
    expect_refused(lambda: evaluate(document), "runs no verification batch",
                   "a production-defect route carrying a verification")


def test_the_no_confident_fix_route_hands_off_to_1439() -> None:
    document = diagnosis_document(route=dd.ROUTE_NO_CONFIDENT_FIX,
                                  diagnosis={
                                      "summary": "three failures, three "
                                                 "unrelated candidates",
                                      "evidence": ["no single change moved "
                                                   "all three"]})
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_NO_CONFIDENT_FIX, "the route holds")
    expect(outcome.owner_issue == 1439, "and hands off to #1439")


def test_partial_improvement_hands_off_to_1439() -> None:
    document = diagnosis_document(route=dd.ROUTE_PARTIAL_IMPROVEMENT)
    document["verification"]["result"] = verification_result(runs=failing_runs(2))
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_PARTIAL_IMPROVEMENT, "the route holds")
    expect(outcome.owner_issue == 1439, "and hands off to #1439")
    expect(outcome.baseline_failures == 4 and outcome.verification_failures == 2,
           "and reports both counts")


def test_an_invalid_verification_batch_is_1439_not_a_rejection() -> None:
    """The issue lists "becomes invalid" beside "remains above X".

    Both are #1439 outcomes with the evidence preserved, so a harness
    error in the verification batch must reach `partial-improvement` —
    reporting it as a rejected handoff would lose the retained artifacts
    and describe an invocation that never got past the gate.
    """
    for label, result, runs in (
            ("a harness error", verification_result(
                runs=failing_runs(1), harness_error=True), None),
            ("a short batch", verification_result(
                runs=failing_runs(1)[:5], requested=5, command_runs=5), 5),
            ("a contended machine", None, None),
    ):
        document = diagnosis_document(route=dd.ROUTE_PARTIAL_IMPROVEMENT)
        if result is None:
            document["verification"]["result"]["peak_concurrency"] = 2
        else:
            document["verification"]["result"] = result
        if runs is not None:
            # The command is bound to its own result, so a batch of
            # another size has to have ASKED for that size.
            document["verification"]["invocation"] = invocation(
                cmd=command(runs=runs, result=f"{OUTSIDE}/verify.json",
                            artifacts=VERIFY_ARTIFACTS, worktree=REPAIR_WT),
                directory=REPAIR_WT, ports=[9201])
        outcome = evaluate(document)
        expect(outcome.route == dd.ROUTE_PARTIAL_IMPROVEMENT,
               f"{label} routes to #1439; got {outcome.route}")
        expect(outcome.owner_issue == 1439, f"{label} names its owner")

    repair = diagnosis_document()
    repair["verification"]["result"]["peak_concurrency"] = 2
    expect_refused(lambda: evaluate(repair), "partial-improvement",
                   "a repair declared over a contended verification")


def test_an_invalid_baseline_is_cannot_reproduce_not_a_rejection() -> None:
    document = diagnosis_document(route=dd.ROUTE_CANNOT_REPRODUCE)
    document["baseline"]["result"] = result_document(
        runs=failing_runs(4), harness_error=True)
    document.pop("verification", None)
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_CANNOT_REPRODUCE,
           f"an aborted baseline established nothing; got {outcome.route}")
    expect(outcome.owner_issue == 1439, "and hands off to #1439")

    repair = diagnosis_document()
    repair["baseline"]["result"] = result_document(
        runs=failing_runs(4), harness_error=True)
    expect_refused(lambda: evaluate(repair), "cannot-reproduce",
                   "a repair declared over an aborted baseline")


def test_an_over_tolerance_baseline_that_is_still_invalid_is_refused() -> None:
    """An invalid baseline is never a repair, whatever its failure count.

    A harness error also leaves the batch incomparable on run count, so
    the tolerance rule would refuse it anyway. This one is over
    tolerance, hits the target, and is unusable ONLY because the machine
    was contended — which makes the "not a usable measurement" rule the
    single thing standing between it and a pull request.
    """
    document = diagnosis_document()
    document["baseline"]["result"]["peak_concurrency"] = 2
    expect_refused(lambda: evaluate(document),
                   "established nothing to repair from",
                   "a repair declared over a contended baseline")


def test_a_command_missing_a_required_option_never_ran() -> None:
    for option in dd.REQUIRED_OPTIONS:
        cmd = [token for token in command()]
        index = cmd.index(option)
        del cmd[index:index + 2]
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(cmd=cmd)
        expect_rejected(lambda d=document: evaluate(d),
                        f"names no {option}",
                        f"a command with no {option}")


def test_a_section_with_no_worktree_at_all_is_refused() -> None:
    for value in (None, "", 42):
        document = diagnosis_document()
        if value is None:
            del document["verification"]["worktree"]
        else:
            document["verification"]["worktree"] = value
        expect_rejected(lambda d=document: evaluate(d), "names no worktree",
                        f"a section whose worktree is {value!r}")


def test_an_artifact_layout_inside_a_worktree_is_refused() -> None:
    """Raw artifacts never land in a repository worktree.

    The whole layout moves together, because `require_topology` pins the
    invocation directory and every run directory to the artifact root —
    so a batch cannot put one of them in a worktree while leaving the
    others outside, and the honest case is a root that is itself inside
    one.
    """
    for label, root in (("the repair worktree", f"{REPAIR_WT}/artifacts"),
                        ("the clean worktree", f"{CLEAN_WT}/artifacts")):
        document = diagnosis_document(handoff=handoff_document(acceptable=1))
        document["verification"]["result"] = verification_result(
            runs=failing_runs(1, abort=False), artifact_root=root)
        document["verification"]["invocation"] = invocation(
            cmd=command(result=f"{OUTSIDE}/verify.json", artifacts=root,
                        worktree=REPAIR_WT),
            directory=REPAIR_WT, ports=[9201])
        expect_rejected(lambda d=document: evaluate(d, worktrees=()),
                        "inside the working tree",
                        f"a verification whose artifacts live in {label}")


def test_the_handoffs_own_evidence_may_not_live_in_a_worktree() -> None:
    """The handoff is held to the batches' containment rule, not exempt.

    Checked with NO registered worktrees, because the comparison
    worktrees the diagnosis DECLARES are collected before the handoff is
    admitted — which is what still holds after they are removed.
    """
    document = diagnosis_document()
    document["handoff"]["result"] = result_document(
        runs=failing_runs(3), artifact_root=f"{CLEAN_WT}/artifacts")
    expect_rejected(lambda: evaluate(document, worktrees=()),
                    "inside the working tree",
                    "a handoff whose artifact tree is in a comparison worktree")

    moved = diagnosis_document()
    moved["handoff"]["invocation"] = deflake_invocation(cmd=[
        "python3", f"{PRIMARY_WT}/tools/deflake.py", "--json",
        "--result", f"{REPAIR_WT}/handoff.json"])
    expect_rejected(lambda: evaluate(moved, worktrees=()),
                    "inside the working tree",
                    "a handoff result document written into a worktree")

    extra = diagnosis_document()
    extra["handoff"]["artifacts"] = [f"{REPAIR_WT}/kept"]
    expect_rejected(lambda: evaluate(extra, worktrees=()),
                    "inside the working tree",
                    "a handoff naming a retained artifact in a worktree")


def test_a_default_artifact_root_inside_a_worktree_is_still_refused() -> None:
    """`--artifact-root` is optional, and that is where this rule earns its keep.

    With the option present, the agreement rule ties the reported root to
    a destination that is already containment-checked. Omitted — which is
    legitimate, since `probe_flake.default_artifact_root` supplies a
    temporary directory — nothing else constrains the root the document
    reports, so the sweep over the paths a result NAMES is the only thing
    standing between a worktree-resident layout and `repair-pr`.
    """
    document = diagnosis_document(handoff=handoff_document(acceptable=1))
    document["verification"]["result"] = verification_result(
        runs=failing_runs(1, abort=False),
        artifact_root=f"{REPAIR_WT}/artifacts")
    document["verification"]["invocation"] = invocation(
        cmd=["python3", f"{REPAIR_WT}/tools/probe_flake.py",
             "--probe", PROBE, "--runs", str(dd.RUN_COUNT),
             "--rts-caps", str(dd.RTS_CAPABILITIES),
             "--result", f"{OUTSIDE}/verify.json"],
        directory=REPAIR_WT, ports=[9201])
    expect_rejected(lambda: evaluate(document, worktrees=()),
                    "inside the working tree",
                    "a default artifact root inside the repair worktree")

    outside = diagnosis_document(handoff=handoff_document(acceptable=1))
    outside["verification"]["result"] = verification_result(
        runs=failing_runs(1, abort=False),
        artifact_root=f"{OUTSIDE}/defaulted")
    outside["verification"]["invocation"] = invocation(
        cmd=["python3", f"{REPAIR_WT}/tools/probe_flake.py",
             "--probe", PROBE, "--runs", str(dd.RUN_COUNT),
             "--rts-caps", str(dd.RTS_CAPABILITIES),
             "--result", f"{OUTSIDE}/verify.json"],
        directory=REPAIR_WT, ports=[9201])
    outcome = evaluate(outside, worktrees=())
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"and omitting the option is otherwise fine; got {outcome.route}")


def test_the_result_paths_a_document_names_are_all_of_them() -> None:
    """What the containment sweep covers, pinned.

    With `--artifact-root` supplied, topology derives every other path
    from a root the agreement rule has already tied to a checked
    destination — so this list is what the sweep still owns when the
    option is omitted, and it must not silently stop covering any of it.
    """
    document = result_document(runs=failing_runs(2), harness_error=True)
    labels = {label for label, _path in dd.result_paths(document)}
    expect("artifact_root" in labels and "invocation_dir" in labels,
           f"the root and the invocation directory are swept: {labels}")
    expect(any(label.startswith("runs[") for label in labels),
           f"and every run's directory: {labels}")
    expect("error_run.artifact_dir" in labels,
           f"and the run that broke the stream: {labels}")
    expect(any(label.startswith("retained_artifacts[") for label in labels),
           f"and every retained entry: {labels}")

    inside = [path for _label, path in dd.result_paths(document)
              if dd.inside_any_worktree(path, [REPAIR_WT]) is not None]
    expect(inside == [], "an external layout is outside every worktree")
    moved = result_document(runs=failing_runs(2), harness_error=True,
                            artifact_root=f"{REPAIR_WT}/artifacts")
    inside = [path for _label, path in dd.result_paths(moved)
              if dd.inside_any_worktree(path, [REPAIR_WT]) is not None]
    expect(len(inside) == len(dd.result_paths(moved)),
           f"and a layout rooted in one is entirely inside it: {inside}")


def test_the_command_and_its_result_must_describe_one_measurement() -> None:
    document = diagnosis_document()
    document["verification"]["result"] = verification_result(
        artifact_root=f"{OUTSIDE}/somewhere-else")
    expect_rejected(lambda: evaluate(document),
                    "have to describe one measurement",
                    "a result reporting a root its command never named")


def test_every_route_hands_on_every_batchs_retained_artifacts() -> None:
    """#1439 and #1438 are handed the evidence, so it has to be named.

    The batch that went wrong is usually the VERIFICATION, whose logs an
    outcome built from the handoff alone would never mention at all.
    """
    document = diagnosis_document(route=dd.ROUTE_PARTIAL_IMPROVEMENT,
                                  handoff=handoff_document(acceptable=1))
    document["verification"]["result"] = verification_result(
        runs=failing_runs(2), harness_error=True)
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_PARTIAL_IMPROVEMENT,
           f"an aborted verification is #1439; got {outcome.route}")
    for label, expected in (
            ("handoff", document["handoff"]["result"]["retained_artifacts"]),
            ("baseline", document["baseline"]["result"]["retained_artifacts"]),
            ("verification",
             document["verification"]["result"]["retained_artifacts"])):
        missing = [path for path in expected
                   if path not in outcome.artifacts]
        expect(not missing,
               f"the outcome names the {label} batch's retained artifacts; "
               f"missing {missing} from {outcome.artifacts}")
    expect(len(outcome.artifacts) == len(set(outcome.artifacts)),
           f"and names each one once: {outcome.artifacts}")
    expect(outcome.to_document()["retained_artifacts"] == outcome.artifacts,
           "and the emitted document carries the same list")


def test_a_cannot_reproduce_outcome_names_the_baseline_it_ran() -> None:
    document = diagnosis_document(route=dd.ROUTE_CANNOT_REPRODUCE,
                                  handoff=handoff_document(acceptable=1))
    document["baseline"]["result"] = result_document(runs=failing_runs(1))
    document.pop("verification", None)
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_CANNOT_REPRODUCE, "the route holds")
    for path in document["baseline"]["result"]["retained_artifacts"]:
        expect(path in outcome.artifacts,
               f"the baseline's {path} is handed on; got {outcome.artifacts}")


def test_partial_improvement_is_refused_when_the_batch_was_accepted() -> None:
    document = diagnosis_document(route=dd.ROUTE_PARTIAL_IMPROVEMENT)
    expect_refused(lambda: evaluate(document), "an accepted verification",
                   "partial-improvement declared over a clean batch")


def test_handoff_rejected_is_never_declared_after_the_gate_passed() -> None:
    document = diagnosis_document(route=dd.ROUTE_HANDOFF_REJECTED)
    expect_rejected(lambda: evaluate(document), "never a conclusion drawn",
                   "handoff-rejected declared over an accepted handoff")


def test_an_unknown_route_is_refused() -> None:
    document = diagnosis_document()
    document["route"] = "probe-is-fine"
    expect_rejected(lambda: evaluate(document), "the declared routes are",
                    "an invented route")


def test_every_route_has_a_declared_owner() -> None:
    expect(set(dd.ROUTE_OWNER) == set(dd.ROUTES),
           "every route names its owning issue, or explicitly none")
    expect(dd.ROUTES_THAT_CHANGE_CODE == frozenset({dd.ROUTE_REPAIR}),
           "exactly one route may touch the probe's source")


# ==========================================================================
# Assertion weakening and the required evidence
# ==========================================================================
def test_a_repair_without_evidence_is_refused() -> None:
    for diagnosis, fragment in (
            (None, "states no diagnosis"),
            ({}, "records no diagnosis evidence"),
            ({"category": "observation"}, "records no diagnosis evidence"),
            ({"category": "observation", "evidence": []},
             "records no diagnosis evidence"),
            ({"category": "observation", "evidence": ["  "]},
             "records no diagnosis evidence"),
            ({"category": "observation", "evidence": [42]},
             "records no diagnosis evidence"),
    ):
        document = diagnosis_document()
        if diagnosis is None:
            del document["diagnosis"]
        else:
            document["diagnosis"] = diagnosis
        expect_rejected(lambda d=document: evaluate(d), fragment,
                        f"a repair whose diagnosis is {diagnosis!r}")


def test_a_repair_names_one_probe_side_cause_from_the_boundary() -> None:
    for category in (None, "the probe is racy", "production-code"):
        document = diagnosis_document()
        document["diagnosis"] = {"category": category,
                                 "evidence": ["the engine log"]}
        expect_rejected(lambda d=document: evaluate(d),
                        "one probe-side cause",
                        f"a repair whose cause is {category!r}")


def test_every_preservation_attestation_is_required() -> None:
    for name in dd.ATTESTATIONS:
        document = diagnosis_document()
        document["attestations"][name] = False
        expect_rejected(lambda d=document: evaluate(d), name,
                        f"a repair that did not attest {name}")
        document = diagnosis_document()
        del document["attestations"][name]
        expect_rejected(lambda d=document: evaluate(d), name,
                        f"a repair that omitted {name}")


def test_a_repair_may_not_change_production_code() -> None:
    document = diagnosis_document(repair={
        "commit_sha": REPAIR_COMMIT, "base_sha": BASE_COMMIT,
        "changed_paths": ["tools/role_probe.py", "src/Unit/Thread.hs"]})
    expect_rejected(lambda: evaluate(document),
                    "outside this workflow's repair scope",
                    "a repair that touched the engine")


def test_a_repair_may_extend_the_headless_suite() -> None:
    """Focused regression coverage is required, so it must be allowed."""
    document = diagnosis_document(repair={
        "commit_sha": REPAIR_COMMIT, "base_sha": BASE_COMMIT,
        "changed_paths": ["tools/role_probe.py",
                          "test-headless/Test/Headless/Role.hs"]})
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_REPAIR,
           "a probe repair may add deterministic coverage beside itself")


# ==========================================================================
# The repair is frozen before it is verified
# ==========================================================================
def test_a_dirty_repair_worktree_invalidates_the_verification() -> None:
    document = diagnosis_document()
    document["verification"]["source_clean"] = False
    expect_rejected(lambda: evaluate(document), "source-clean",
                    "a verification run against uncommitted source")


def test_the_verification_must_measure_the_proposed_commit() -> None:
    document = diagnosis_document()
    document["repair"]["commit_sha"] = "c" * 40
    expect_rejected(lambda: evaluate(document),
                    "measures something this pull request does not contain",
                    "a verification of another commit")


def test_a_repair_with_no_resolved_commit_is_refused() -> None:
    document = diagnosis_document(repair={"commit_sha": "HEAD",
                                          "base_sha": BASE_COMMIT,
                                          "changed_paths": ["tools/x.py"]})
    expect_rejected(lambda: evaluate(document), "names no resolved commit",
                    "a repair with no commit")


def test_a_repair_with_no_changed_paths_is_refused() -> None:
    document = diagnosis_document(repair={"commit_sha": REPAIR_COMMIT,
                                          "base_sha": BASE_COMMIT,
                                          "changed_paths": []})
    expect_rejected(lambda: evaluate(document), "records no changed paths",
                    "a repair that changed nothing")


# ==========================================================================
# The one-PR limit
# ==========================================================================
def test_one_invocation_opens_at_most_one_pull_request() -> None:
    outcome = evaluate(diagnosis_document())
    session = dd.Diagnosis(outcome.handoff)
    expect(session.open_pull_request(outcome) == 1, "the first PR is allowed")
    try:
        session.open_pull_request(outcome)
    except dd.RouteRefused as error:
        expect("already opened a pull request" in str(error),
               f"the second PR is refused, got {error}")
    else:
        FAILURES.append("a second pull request was allowed")


def test_a_non_repair_route_opens_no_pull_request() -> None:
    document = diagnosis_document(route=dd.ROUTE_CANNOT_REPRODUCE)
    document["baseline"]["result"] = result_document()
    document.pop("verification", None)
    outcome = evaluate(document)
    session = dd.Diagnosis(outcome.handoff)
    try:
        session.open_pull_request(outcome)
    except dd.RouteRefused as error:
        expect("opens no pull request" in str(error),
               f"a non-repair route is refused a PR, got {error}")
    else:
        FAILURES.append("a cannot-reproduce route opened a pull request")


# ==========================================================================
# The CLI
# ==========================================================================
def _run_cli(*args) -> subprocess.CompletedProcess:
    return subprocess.run([sys.executable, TOOL, *args], text=True,
                          capture_output=True, timeout=120)


def _live_document(**kwargs) -> dict:
    """A diagnosis the CLI's OWN worktree resolution will accept.

    The CLI derives the primary checkout from `git worktree list`, so a
    fixture that invented one would exercise nothing but the refusal.
    Everything else — the comparison worktrees, the evidence paths — stays
    synthetic and outside every real worktree.
    """
    document = diagnosis_document(**kwargs)
    live = dd.primary_checkout()
    document["handoff"]["invocation"] = deflake_invocation(
        cmd=["python3", f"{live}/tools/deflake.py", "--json",
             "--result", f"{OUTSIDE}/handoff.json"],
        directory=str(live))
    return document


def test_the_cli_reports_the_route_and_its_exit_status() -> None:
    root = Path(tempfile.mkdtemp(prefix="test_deflake_diagnosis_"))
    try:
        path = root / "diagnosis.json"
        path.write_text(json.dumps(_live_document()), encoding="utf-8")
        done = _run_cli("--diagnosis", str(path), "--json")
        expect(done.returncode == dd.EXIT_OK,
               f"an accepted repair exits {dd.EXIT_OK}: {done.stderr}")
        document = json.loads(done.stdout)
        expect(document["route"] == dd.ROUTE_REPAIR, "and names the route")
        expect(document["schema"] == dd.OUTCOME_SCHEMA,
               "in the outcome schema")
        expect(document["opens_pull_request"] is True,
               "declaring that it opens the pull request")

        broken = root / "broken.json"
        document = _live_document()
        document["handoff"]["probe"] = [PROBE, OTHER]
        broken.write_text(json.dumps(document), encoding="utf-8")
        done = _run_cli("--diagnosis", str(broken))
        expect(done.returncode == dd.EXIT_REJECTED,
               f"a rejected handoff exits {dd.EXIT_REJECTED}")
        expect(dd.ROUTE_HANDOFF_REJECTED in done.stderr,
               f"naming the route: {done.stderr}")

        refused = root / "refused.json"
        document = _live_document()
        document["baseline"]["result"] = result_document()
        refused.write_text(json.dumps(document), encoding="utf-8")
        done = _run_cli("--diagnosis", str(refused))
        expect(done.returncode == dd.EXIT_REFUSED,
               f"a denied route exits {dd.EXIT_REFUSED}")

        gate = root / "handoff.json"
        gate.write_text(json.dumps(_live_document()["handoff"]),
                        encoding="utf-8")
        done = _run_cli("--handoff", str(gate))
        expect(done.returncode == dd.EXIT_OK,
               f"the entry gate alone exits 0: {done.stderr}")
        expect(PROBE in done.stdout, "naming the probe it accepted")

        done = _run_cli("--manifest", str(root))
        expect(done.returncode == dd.EXIT_OK, "a manifest run exits 0")
        expect(json.loads(done.stdout)["entries"] == [],
               "and states an absent configuration family explicitly")
    finally:
        shutil.rmtree(root, ignore_errors=True)


def test_the_cli_needs_exactly_one_mode() -> None:
    done = _run_cli()
    expect(done.returncode != 0, "no mode at all is an error")
    done = _run_cli("--diagnosis", "a.json", "--handoff", "b.json")
    expect(done.returncode != 0, "two modes at once is an error")


def test_a_malformed_document_reaches_the_cli_as_a_rejection() -> None:
    root = Path(tempfile.mkdtemp(prefix="test_deflake_diagnosis_"))
    try:
        for label, mutate in (
                ("expected_checks", lambda d: d["handoff"].update(
                    {"expected_checks": 42})),
                ("targets", lambda d: d["handoff"].update({"targets": 42})),
                ("timestamp_utc", lambda d: d["handoff"]["result"].update(
                    {"timestamp_utc": "2026-99-99T99:99:99Z"})),
                ("artifact_root", lambda d: d["handoff"]["result"].update(
                    {"artifact_root": f"{OUTSIDE}/a/../b"})),
        ):
            document = _live_document()
            mutate(document)
            path = root / f"malformed-{label}.json"
            path.write_text(json.dumps(document), encoding="utf-8")
            done = _run_cli("--diagnosis", str(path))
            expect(done.returncode == dd.EXIT_REJECTED,
                   f"a malformed {label} exits {dd.EXIT_REJECTED}, got "
                   f"{done.returncode}: {done.stderr}")
            expect("Traceback" not in done.stderr,
                   f"without a traceback: {done.stderr}")
    finally:
        shutil.rmtree(root, ignore_errors=True)


def test_an_unreadable_document_is_a_rejection_not_a_traceback() -> None:
    done = _run_cli("--diagnosis", "/nonexistent/diagnosis.json")
    expect(done.returncode == dd.EXIT_REJECTED,
           f"a missing document exits {dd.EXIT_REJECTED}")
    expect("Traceback" not in done.stderr,
           f"without a traceback: {done.stderr}")


# ==========================================================================
# Constants that must not drift
# ==========================================================================
def test_the_measurement_contract_comes_from_its_owners() -> None:
    expect(dd.RUN_COUNT == probe_census.POLICY_RUN_COUNT,
           "the run count is the census policy's own N")
    expect(dd.RTS_CAPABILITIES == probe_flake.DEFAULT_RTS_CAPS,
           "the capability count is the harness's own default")
    expect(dd.RUN_COUNT == 10 and dd.RTS_CAPABILITIES == 4,
           "and both are what #1436 measured under")


def test_x_arithmetic_is_delegated_rather_than_reimplemented() -> None:
    """Guards against a second copy of `failures <= X` drifting from the census."""
    source = Path(dd.__file__).read_text(encoding="utf-8")
    expect("probe_census.tolerance_state" in source,
           "the tolerance comparison is the census policy's own")
    expect("probe_census.require_acceptable_failures" in source,
           "and so is X's validation")


def main() -> int:
    tests = [value for name, value in sorted(globals().items())
             if name.startswith("test_") and callable(value)]
    for test in tests:
        try:
            test()
        except Exception as error:  # noqa: BLE001 - a crash is a failure
            FAILURES.append(f"{test.__name__} raised "
                            f"{type(error).__name__}: {error}")
    if FAILURES:
        print(f"FAILED ({len(FAILURES)}):")
        for failure in FAILURES:
            print(f"  - {failure}")
        return 1
    print(f"ok - {len(tests)} deflake-diagnosis tests passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
