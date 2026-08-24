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
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import deflake_diagnosis as dd  # type: ignore
import probe_census  # type: ignore
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

OUTSIDE = "/tmp/synarchy-deflake-evidence"


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
                    requested=None) -> dict:
    """A REAL `probe-flake-result/v1` document.

    `runs` is a list of per-run check maps; the run outcome is derived
    the way `probe_flake.reconcile` derives it — a timeout is stated
    explicitly by the caller, a FAIL check makes the run FAIL, and
    everything else passes.
    """
    declared = CHECKS if checks is None else checks
    descriptor = probe_protocol.build_descriptor(probe, declared)
    requested = dd.RUN_COUNT if requested is None else requested
    caps = dd.RTS_CAPABILITIES if rts_caps is None else rts_caps
    root = Path(OUTSIDE) / "artifacts"
    measurement = probe_flake.Measurement(
        probe, descriptor, requested, caps, root, root / "invocation")
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
            (root / "invocation" / f"run-{index:03d}") if keep else None))
    return measurement.to_document()


def failing_runs(count: int, *, cid: str = "beta", declared=None) -> list:
    """`count` runs that FAIL at `cid` and abort, and clean runs after."""
    declared = CHECKS if declared is None else declared
    ids = [c for c, _l in declared]
    position = ids.index(cid)
    runs = []
    for _ in range(count):
        run = {}
        for index, name in enumerate(ids):
            if index < position:
                run[name] = PASS
            elif index == position:
                run[name] = FAIL
            else:
                run[name] = MISSING
        runs.append(run)
    while len(runs) < dd.RUN_COUNT:
        runs.append({name: PASS for name in ids})
    return runs


def command(*, probe: str = PROBE, runs=None, rts_caps=None,
            result: str = f"{OUTSIDE}/baseline.json",
            artifacts: str = f"{OUTSIDE}/artifacts",
            worktree: str = "/tmp/deflake-clean-role") -> list:
    runs = dd.RUN_COUNT if runs is None else runs
    caps = dd.RTS_CAPABILITIES if rts_caps is None else rts_caps
    return [
        "python3", f"{worktree}/tools/probe_flake.py",
        "--probe", probe, "--runs", str(runs), "--rts-caps", str(caps),
        "--result", result, "--artifact-root", artifacts,
    ]


def invocation(*, cmd=None, directory: str = "/tmp/deflake-clean-role",
               retries: int = 0, ports=None) -> dict:
    return {
        "command": command() if cmd is None else cmd,
        "directory": directory,
        "retries": retries,
        "ports": [9101, 9102] if ports is None else ports,
    }


def manifest(entries=()) -> dict:
    return {"schema": dd.MANIFEST_SCHEMA, "root": "/tmp/whatever",
            "entries": [{"path": path, "sha256": digest}
                        for path, digest in entries]}


def handoff_document(*, probe: str = PROBE, acceptable: int = 0,
                     targets=("beta",), result=None, inv=None,
                     config=None) -> dict:
    document = {
        "schema": dd.HANDOFF_SCHEMA,
        "probe": probe,
        "acceptable_failures": acceptable,
        "targets": list(targets),
        "result": result if result is not None else result_document(
            probe=probe, runs=failing_runs(3)),
        "invocation": invocation() if inv is None else inv,
        "configuration": manifest() if config is None else config,
        "artifacts": [f"{OUTSIDE}/artifacts/run-001"],
    }
    return document


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
            "worktree": "/tmp/deflake-clean-role",
            "result": result_document(runs=failing_runs(4)),
            "invocation": invocation(),
            "configuration": manifest(),
        }
    if verification is not None:
        document["verification"] = verification
    elif route in (dd.ROUTE_REPAIR, dd.ROUTE_PARTIAL_IMPROVEMENT):
        document["verification"] = {
            "worktree": "/tmp/deflake-role",
            "source_clean": True,
            "result": result_document(commit=REPAIR_COMMIT),
            "invocation": invocation(
                cmd=command(result=f"{OUTSIDE}/verify.json",
                            artifacts=f"{OUTSIDE}/verify-artifacts",
                            worktree="/tmp/deflake-role"),
                directory="/tmp/deflake-role", ports=[9201, 9202]),
            "configuration": manifest(),
        }
    if route == dd.ROUTE_REPAIR:
        document["attestations"] = attestations if attestations is not None else {
            name: True for name in dd.ATTESTATIONS}
        document["repair"] = repair if repair is not None else {
            "commit_sha": REPAIR_COMMIT,
            "changed_paths": ["tools/role_probe.py"],
        }
    else:
        if attestations is not None:
            document["attestations"] = attestations
        if repair is not None:
            document["repair"] = repair
    return document


def evaluate(document, **kwargs):
    return dd.evaluate(document, worktrees=kwargs.pop("worktrees", ()))


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


def test_targets_must_be_declared_and_observed() -> None:
    undeclared = handoff_document(targets=("delta",))
    expect_rejected(lambda: dd.require_handoff(undeclared),
                    "identifiers the descriptor never declared",
                    "a target that is not a declared check")
    unobserved = handoff_document(targets=("gamma",),
                                  result=result_document(runs=failing_runs(3)))
    # `gamma` is MISSING in the aborted runs, so it IS observed; use a
    # check that passed in every run instead.
    unobserved["targets"] = ["alpha"]
    expect_rejected(lambda: dd.require_handoff(unobserved),
                    "never failed or went missing",
                    "a target that never went non-PASS")


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


def test_unsuccessful_runs_without_retained_artifacts_are_refused() -> None:
    document = handoff_document()
    document["artifacts"] = []
    document["result"]["retained_artifacts"] = []
    expect_rejected(lambda: dd.require_handoff(document),
                    "records no retained artifact",
                    "failing runs with no retained evidence")


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
    """At or below X passes, above it does not — X=1 accepts 1, rejects 2."""
    handoff = handoff_document(acceptable=1)
    accepted = diagnosis_document(handoff=handoff)
    accepted["verification"]["result"] = result_document(
        commit=REPAIR_COMMIT, runs=failing_runs(1))
    outcome = evaluate(accepted)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"one failure against X=1 is a repair, got {outcome.route}")
    expect(outcome.verification_failures == 1, "the count is reported")

    over = diagnosis_document(handoff=handoff_document(acceptable=1))
    over["verification"]["result"] = result_document(
        commit=REPAIR_COMMIT, runs=failing_runs(2))
    expect_refused(lambda: evaluate(over), "partial-improvement",
                   "two failures against X=1")


def test_x_zero_requires_a_spotless_batch() -> None:
    document = diagnosis_document()
    document["verification"]["result"] = result_document(
        commit=REPAIR_COMMIT, runs=failing_runs(1))
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
    document = handoff_document()
    document["result"]["status"] = "harness-error"
    expect_rejected(lambda: dd.require_handoff(document),
                    "only a valid measurement", "a harness-error batch")


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
    document = handoff_document(inv=invocation(retries=1))
    expect_rejected(lambda: dd.require_handoff(document),
                    "retry policy", "a handoff measured with retries")


def test_an_absent_retry_policy_is_refused() -> None:
    inv = invocation()
    del inv["retries"]
    document = handoff_document(inv=inv)
    expect_rejected(lambda: dd.require_handoff(document),
                    "retry policy", "a handoff that states no retry policy")


def test_the_invocation_must_match_the_measurement_it_describes() -> None:
    wrong_probe = handoff_document(inv=invocation(cmd=command(probe=OTHER)))
    expect_rejected(lambda: dd.require_handoff(wrong_probe), "where the "
                    "handoff names", "an invocation measuring another probe")
    wrong_runs = handoff_document(inv=invocation(cmd=command(runs=5)))
    expect_rejected(lambda: dd.require_handoff(wrong_runs),
                    "measurement contract is exactly",
                    "an invocation requesting five runs")
    wrong_caps = handoff_document(inv=invocation(cmd=command(rts_caps=8)))
    expect_rejected(lambda: dd.require_handoff(wrong_caps),
                    "RTS capabilities", "an invocation at eight capabilities")


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
    document = diagnosis_document(handoff=handoff_document(
        targets=("alpha",), result=result_document(
            runs=failing_runs(3, cid="alpha"))))
    document["baseline"]["result"] = result_document(
        runs=failing_runs(4, cid="beta"))
    expect_refused(lambda: evaluate(document), "cannot-reproduce",
                   "a baseline that reproduced another failure")


def test_both_batches_must_stay_outside_every_worktree() -> None:
    trees = ["/tmp/deflake-role"]
    document = diagnosis_document()
    document["verification"]["invocation"] = invocation(
        cmd=command(result="/tmp/deflake-role/verify.json",
                    artifacts=f"{OUTSIDE}/verify-artifacts",
                    worktree="/tmp/deflake-role"),
        directory="/tmp/deflake-role", ports=[9201])
    expect_rejected(lambda: evaluate(document, worktrees=trees),
                    "inside the working tree",
                    "a result document written into a worktree")


def test_the_two_batches_may_not_share_a_destination() -> None:
    document = diagnosis_document()
    document["verification"]["invocation"] = invocation(
        cmd=command(worktree="/tmp/deflake-role"),
        directory="/tmp/deflake-role", ports=[9201])
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


def test_the_two_batches_may_not_share_a_worktree() -> None:
    document = diagnosis_document()
    document["verification"]["worktree"] = document["baseline"]["worktree"]
    expect_refused(lambda: evaluate(document), "same worktree",
                   "a verification run in the clean comparison worktree")


# ==========================================================================
# Same-environment verification
# ==========================================================================
def test_destinations_and_ports_may_differ_and_nothing_else() -> None:
    outcome = evaluate(diagnosis_document())
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"different destinations and ports are fine; got {outcome.route}")

    for label, fragment, cmd in (
            ("run count", "runs", command(runs=20, result=f"{OUTSIDE}/verify.json",
                                  artifacts=f"{OUTSIDE}/v",
                                  worktree="/tmp/deflake-role")),
            ("capabilities", "rts_caps", command(rts_caps=8,
                                     result=f"{OUTSIDE}/verify.json",
                                     artifacts=f"{OUTSIDE}/v",
                                     worktree="/tmp/deflake-role")),
    ):
        document = diagnosis_document()
        document["verification"]["invocation"] = invocation(
            cmd=cmd, directory="/tmp/deflake-role", ports=[9201])
        expect_refused(lambda d=document: evaluate(d), fragment,
                       f"a verification that changed the {label}")


def test_a_timeout_difference_is_a_different_condition() -> None:
    document = diagnosis_document()
    document["verification"]["invocation"] = invocation(
        cmd=command(result=f"{OUTSIDE}/verify.json",
                    artifacts=f"{OUTSIDE}/v",
                    worktree="/tmp/deflake-role") + ["--timeout", "60"],
        directory="/tmp/deflake-role", ports=[9201])
    expect_refused(lambda: evaluate(document), "timeout",
                   "a verification under another timeout")


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


def test_an_unclassifiable_option_is_refused_rather_than_ignored() -> None:
    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(
        cmd=command() + ["--start-port", "9500"])
    expect_rejected(lambda: evaluate(document),
                    "does not know how to classify",
                    "an option that is neither a condition nor a destination")


# ==========================================================================
# Stable check identity
# ==========================================================================
def test_a_renamed_identifier_is_refused() -> None:
    document = diagnosis_document()
    renamed = [("alpha", "the first check"), ("beta_two", "renamed"),
               ("gamma", "the third check")]
    document["verification"]["result"] = result_document(
        commit=REPAIR_COMMIT, checks=renamed,
        runs=[{cid: PASS for cid, _l in renamed}] * dd.RUN_COUNT)
    expect_rejected(lambda: evaluate(document),
                    "separately approved protocol change",
                    "a verification that renamed a check")


def test_a_removed_identifier_is_refused() -> None:
    document = diagnosis_document()
    fewer = [("alpha", "the first check"), ("beta", "the second check")]
    document["verification"]["result"] = result_document(
        commit=REPAIR_COMMIT, checks=fewer,
        runs=[{cid: PASS for cid, _l in fewer}] * dd.RUN_COUNT)
    expect_rejected(lambda: evaluate(document),
                    "separately approved protocol change",
                    "a verification that dropped a check")


def test_a_reordered_descriptor_is_refused() -> None:
    document = diagnosis_document()
    swapped = [CHECKS[1], CHECKS[0], CHECKS[2]]
    document["verification"]["result"] = result_document(
        commit=REPAIR_COMMIT, checks=swapped,
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
    document = handoff_document(
        targets=("beta_two",),
        result=result_document(checks=valued,
                               runs=failing_runs(3, cid="beta_two",
                                                 declared=valued)))
    accepted = dd.require_handoff(copy.deepcopy(document))
    expect(accepted.probe == PROBE,
           "a spelled-out number is a legitimate identifier")
    for entry in document["result"]["checks"]:
        if entry["id"] == "beta_two":
            entry["id"] = "beta_2"
    for run in document["result"]["runs"]:
        run["checks"]["beta_2"] = run["checks"].pop("beta_two")
    document["targets"] = ["beta_2"]
    expect_rejected(lambda: dd.require_handoff(document),
                    "identifiers are static",
                    "an identifier carrying a measured value")


def test_a_run_reporting_an_undeclared_identifier_is_malformed() -> None:
    document = handoff_document()
    document["result"]["runs"][0]["checks"]["delta"] = PASS
    expect_rejected(lambda: dd.require_handoff(document),
                    "identifiers the descriptor never declared",
                    "a run reporting an undeclared check")


def test_a_run_that_simply_omits_a_declared_identifier_is_malformed() -> None:
    document = handoff_document()
    del document["result"]["runs"][0]["checks"]["gamma"]
    expect_rejected(lambda: dd.require_handoff(document),
                    "was not written by the harness",
                    "a run whose check map lost a key")


# ==========================================================================
# MISSING
# ==========================================================================
def test_a_target_that_goes_missing_is_never_a_fix() -> None:
    document = diagnosis_document()
    runs = [{"alpha": PASS, "beta": MISSING, "gamma": MISSING,
             "__timeout__": True}]
    runs += [{cid: PASS for cid, _l in CHECKS}] * (dd.RUN_COUNT - 1)
    document["handoff"]["acceptable_failures"] = 1
    document["verification"]["result"] = result_document(
        commit=REPAIR_COMMIT, runs=runs)
    expect_refused(lambda: evaluate(document),
                   "has not been fixed, it has stopped being measured",
                   "a target check that became MISSING")


def test_a_passing_run_may_not_omit_a_check() -> None:
    document = diagnosis_document()
    runs = [{"alpha": PASS, "beta": PASS, "gamma": MISSING}]
    runs += [{cid: PASS for cid, _l in CHECKS}] * (dd.RUN_COUNT - 1)
    document["verification"]["result"] = result_document(
        commit=REPAIR_COMMIT, runs=runs)
    expect_refused(lambda: evaluate(document),
                   "passed while omitting",
                   "a passing run that omitted a check")


def test_an_accepted_failing_run_may_lose_the_suffix_after_its_abort() -> None:
    """The scoped rule: X>0 stays satisfiable, which strict MISSING is not."""
    handoff = handoff_document(acceptable=1, targets=("alpha",),
                               result=result_document(
                                   runs=failing_runs(3, cid="alpha")))
    document = diagnosis_document(handoff=handoff)
    document["baseline"]["result"] = result_document(
        runs=failing_runs(4, cid="alpha"))
    document["verification"]["result"] = result_document(
        commit=REPAIR_COMMIT, runs=failing_runs(1, cid="alpha"))
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"one aborted run within X is accepted; got {outcome.route}")


def test_a_non_contiguous_gap_is_malformed_rather_than_an_abort() -> None:
    document = diagnosis_document()
    document["handoff"]["acceptable_failures"] = 1
    runs = [{"alpha": MISSING, "beta": FAIL, "gamma": MISSING}]
    runs += [{cid: PASS for cid, _l in CHECKS}] * (dd.RUN_COUNT - 1)
    document["verification"]["result"] = result_document(
        commit=REPAIR_COMMIT, runs=runs)
    expect_refused(lambda: evaluate(document), "contiguous suffix",
                   "a run with a hole in the middle of its results")


def test_an_identifier_that_vanishes_from_the_batch_is_refused() -> None:
    document = diagnosis_document()
    document["handoff"]["acceptable_failures"] = 3
    runs = [{"alpha": PASS, "beta": FAIL, "gamma": MISSING}] * dd.RUN_COUNT
    document["verification"]["result"] = result_document(
        commit=REPAIR_COMMIT, runs=runs)
    document["handoff"]["targets"] = ["beta"]
    expect_refused(lambda: evaluate(document),
                   "never emitted gamma",
                   "a check that was never emitted in the whole batch")


def test_a_missing_violation_is_the_partial_improvement_route() -> None:
    """At or below X but with a MISSING result is #1439, not a PR."""
    document = diagnosis_document(route=dd.ROUTE_PARTIAL_IMPROVEMENT)
    runs = [{"alpha": PASS, "beta": PASS, "gamma": MISSING}]
    runs += [{cid: PASS for cid, _l in CHECKS}] * (dd.RUN_COUNT - 1)
    document["verification"]["result"] = result_document(
        commit=REPAIR_COMMIT, runs=runs)
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
            document["verification"]["result"] = result_document(
                commit=REPAIR_COMMIT, runs=failing_runs(2))
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
        "worktree": "/tmp/deflake-role", "source_clean": True,
        "result": result_document(commit=REPAIR_COMMIT),
        "invocation": invocation(directory="/tmp/deflake-role"),
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
    document["verification"]["result"] = result_document(
        commit=REPAIR_COMMIT, runs=failing_runs(2))
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_PARTIAL_IMPROVEMENT, "the route holds")
    expect(outcome.owner_issue == 1439, "and hands off to #1439")
    expect(outcome.baseline_failures == 4 and outcome.verification_failures == 2,
           "and reports both counts")


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
        "commit_sha": REPAIR_COMMIT,
        "changed_paths": ["tools/role_probe.py", "src/Unit/Thread.hs"]})
    expect_rejected(lambda: evaluate(document),
                    "outside this workflow's repair scope",
                    "a repair that touched the engine")


def test_a_repair_may_extend_the_headless_suite() -> None:
    """Focused regression coverage is required, so it must be allowed."""
    document = diagnosis_document(repair={
        "commit_sha": REPAIR_COMMIT,
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
                                          "changed_paths": ["tools/x.py"]})
    expect_rejected(lambda: evaluate(document), "names no resolved commit",
                    "a repair with no commit")


def test_a_repair_with_no_changed_paths_is_refused() -> None:
    document = diagnosis_document(repair={"commit_sha": REPAIR_COMMIT,
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


def test_the_cli_reports_the_route_and_its_exit_status() -> None:
    root = Path(tempfile.mkdtemp(prefix="test_deflake_diagnosis_"))
    try:
        path = root / "diagnosis.json"
        path.write_text(json.dumps(diagnosis_document()), encoding="utf-8")
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
        document = diagnosis_document()
        document["handoff"]["probe"] = [PROBE, OTHER]
        broken.write_text(json.dumps(document), encoding="utf-8")
        done = _run_cli("--diagnosis", str(broken))
        expect(done.returncode == dd.EXIT_REJECTED,
               f"a rejected handoff exits {dd.EXIT_REJECTED}")
        expect(dd.ROUTE_HANDOFF_REJECTED in done.stderr,
               f"naming the route: {done.stderr}")

        refused = root / "refused.json"
        document = diagnosis_document()
        document["baseline"]["result"] = result_document()
        refused.write_text(json.dumps(document), encoding="utf-8")
        done = _run_cli("--diagnosis", str(refused))
        expect(done.returncode == dd.EXIT_REFUSED,
               f"a denied route exits {dd.EXIT_REFUSED}")

        gate = root / "handoff.json"
        gate.write_text(json.dumps(handoff_document()), encoding="utf-8")
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
