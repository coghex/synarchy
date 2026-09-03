#!/usr/bin/env python3
"""Unit tests for the `/deflake` diagnosis mechanics (#1437, #1439, #1438).

Deterministic, engine-free, GPU-free and network-free. No probe is run,
no port opened, no worktree created, no census touched: every fixture is
a document in memory or a file in a temporary directory. #1438's section
is the one exception to "no file outside a temporary directory": it
stages the retained artifact tree a failing batch would have left, under
a fixture-owned path in `/tmp`, because that workflow READS those
artifacts and a filed issue whose only evidence is a pathname is the
thing it exists to prevent. The tracker itself stays a fake at the
publication boundary — no `gh`, no network, no issue.

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

import ast
import contextlib
import copy
import importlib
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import types
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import ci_probes  # type: ignore
import deflake_diagnosis as dd  # type: ignore
import deflake_handoff  # type: ignore
import deflake_issue as di  # type: ignore
# The unfittable-body case below substitutes `MAX_BODY_CHARS`, and
# `issue_body` reads it out of the DOCUMENT owner's globals (#2157), not
# the facade's — so that is the object these cases name.
import deflake_issue_document as did  # type: ignore
import deflake_issue_evidence as die  # type: ignore
import deflake_outcome as do  # type: ignore
import probe_census  # type: ignore
# The census write these cases intercept resolves `_atomic_replace` in
# the STORAGE owner's globals, not the facade's (#2131), so that is
# the object they patch.
import probe_census_storage as census_storage  # type: ignore
import deflake  # type: ignore
import probe_flake  # type: ignore
import probe_protocol  # type: ignore
import probe_runner_registry  # type: ignore
import probe_runner_resources  # type: ignore

TOOL = str(Path(__file__).resolve().parent / "deflake_diagnosis.py")

# `deflake.build_handoff` and this module's own controlled records spell
# one setting differently; the entry gate adapts at the boundary and the
# fixtures have to know which side they are on.
PRODUCER_FIELD = {"timeout_seconds": "timeout"}

# A directory with no `config/*.local.yaml` family at all, so
# `deflake.configuration_manifest` answers with the empty list that is
# this repository's expected default.
PRIMARY_CONFIG_ROOT = tempfile.mkdtemp(prefix="deflake_diag_config_")
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


def deflake_argv(*, worktree: str = PRIMARY_WT,
                 result: str = f"{OUTSIDE}/handoff.json") -> list:
    """`/deflake`'s own argv, in the form the PRODUCER records.

    `sys.argv`, whose [0] is the SCRIPT: `deflake.main` passes
    `list(sys.argv)` (`tools/deflake.py:1108`) and Python never puts the
    interpreter there. Writing `["python3", ...]` here is the assumption
    that let this suite certify a gate no real handoff could pass —
    `real_cli_argv` proves the form against an actual subprocess rather
    than restating it.

    Deliberately no `--probe`, no `--runs` and no RTS override — that CLI
    exposes none of them, so a fixture naming one would be asserting
    against a command that cannot exist.
    """
    return [f"{worktree}/tools/deflake.py", "--json", "--result", result]


def deflake_invocation(*, cmd=None, directory: str = PRIMARY_WT,
                       retries: int = 0, ports=None, timeout=None,
                       start_port=None, runs: int = dd.RUN_COUNT) -> dict:
    """#1659's OWN invocation record, in the PRODUCER's spelling.

    `argv`/`cwd`/`timeout`, not `command`/`directory`/`timeout_seconds`:
    `deflake.build_handoff` writes these names (`tools/deflake.py:446-453`)
    and the entry gate reads what the producer writes. `ports` defaults to
    the ordered base ports `result_document` gives a full batch, because
    the envelope requires it to equal the result's own run ports.
    """
    return {
        "argv": deflake_argv() if cmd is None else cmd,
        "cwd": directory,
        "retries": retries,
        "ports": [9100 + index for index in range(1, runs + 1)]
                 if ports is None else ports,
        "timeout": deflake.TIMEOUT if timeout is None else timeout,
        "start_port": (deflake.START_PORT if start_port is None
                       else start_port),
    }


def config_entries(entries=()) -> list:
    """The producer's `configuration`: a bare sorted list of entries."""
    return [{"path": path, "sha256": digest} for path, digest in entries]


def manifest(entries=()) -> dict:
    """This module's OWN manifest document, which also names its root.

    The controlled batches record one of these; the handoff records
    `config_entries`' bare list, because that is what
    `deflake.configuration_manifest` returns.
    """
    return {"schema": dd.MANIFEST_SCHEMA, "root": "/tmp/whatever",
            "entries": list(config_entries(entries))}


def handoff_document(*, probe: str = PROBE, acceptable: int = 0,
                     targets=None, result=None, inv=None,
                     config=None, artifacts=None) -> dict:
    """A handoff built by the PRODUCER, not hand-assembled to match us.

    `deflake.build_handoff` is the function #1659 ships and the only
    thing that writes a real `deflake-handoff/v1`. Calling it here is
    what makes this suite able to fail when the entry gate and the
    producer disagree — a hand-written envelope agrees with whatever the
    validator happens to require, which is exactly how a validator that
    could not consume a single real handoff kept a green suite.

    Consequently `targets`, `invocation.ports` and `artifacts` all DERIVE
    from the embedded measurement. A case that wants one of them wrong
    overrides it afterwards, so the fixture breaks one relationship and
    not three.
    """
    result = result if result is not None else result_document(
        probe=probe, runs=failing_runs(3))
    document = deflake.build_handoff(
        result=result,
        acceptable_failures=acceptable,
        argv=deflake_argv(),
        cwd=PRIMARY_WT,
        configuration=[] if config is None else config,
        artifacts=list(result.get("retained_artifacts") or []))
    if targets is not None:
        document["targets"] = list(targets)
    if inv is not None:
        document["invocation"] = inv
    if artifacts is not None:
        document["artifacts"] = list(artifacts)
    return document


def resource_hold(*, probe: str = PROBE, held: bool = True,
                  exclusive=None, shared=None,
                  covers: bool = True, detail=None) -> dict:
    """The batch's cross-process hold on the probe's DECLARED interests.

    Taken from `probe_runner_resources` rather than spelled out, because
    they are the
    probe's own and a fixture that listed them would drift from the
    registry it is supposed to be reproducing.
    """
    record = {
        "held": held,
        "exclusive": (sorted(probe_runner_resources.exclusive_resources(probe))
                      if exclusive is None else list(exclusive)),
        "shared": (sorted(probe_runner_resources.shared_resources(probe))
                   if shared is None else list(shared)),
        "covers_configuration_install": covers,
    }
    if detail is not None:
        record["detail"] = detail
    return record


def batch_section(**overrides) -> dict:
    """One controlled batch, in the shape this module defines itself."""
    section = {
        "worktree": CLEAN_WT,
        "source_clean": True,
        "result": result_document(runs=failing_runs(4)),
        "invocation": invocation(),
        "configuration": manifest(),
        "resource_hold": resource_hold(),
    }
    section.update(overrides)
    return section


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
    # `False` OMITS the section, which `None` cannot express: the
    # no-target route has to be able to carry no batch at all.
    if baseline is False:
        pass
    elif baseline is not None:
        document["baseline"] = baseline
    else:
        document["baseline"] = batch_section()
    if "baseline" in document:
        document["baseline"].setdefault("result", result_document())
    if verification is False:
        pass
    elif verification is not None:
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
            "resource_hold": resource_hold(),
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
    legacy = next(key for key, _script, _purpose in probe_runner_registry.PROBES
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


def test_an_emptied_target_list_still_has_to_match_the_measurement() -> None:
    """Emptying the list under a FAILING measurement is still a lie.

    An empty `targets` is only legitimate when the measurement itself
    observed nothing non-PASS; here it contradicts two observed failures,
    and the equality rule says so.
    """
    document = handoff_document(targets=())
    expect_rejected(lambda: dd.require_handoff(document),
                    "All of them are diagnosis inputs",
                    "an emptied target list over a failing measurement")

    document = handoff_document()
    del document["targets"]
    expect_rejected(lambda: dd.require_handoff(document),
                    "absence has to be asserted",
                    "a handoff with no `targets` key at all")


def test_an_all_pass_handoff_is_the_no_target_outcome() -> None:
    """`/deflake` writes one, so the gate may not call it malformed.

    `deflake.handoff_targets` returns `[]` for an all-PASS measurement
    and `tools/test_deflake.py` pins that case, so this is a legitimate
    input with nothing to diagnose. The approved correction routes it to
    #1439 instead of rejecting it or inventing a target.
    """
    passing = handoff_document(result=result_document())
    expect(passing["targets"] == [],
           f"the producer derives no target from an all-PASS measurement; "
           f"got {passing['targets']}")
    accepted = dd.require_handoff(copy.deepcopy(passing))
    expect(accepted.targets == (),
           "and the entry gate admits it rather than refusing it")

    document = diagnosis_document(route=dd.ROUTE_NO_TARGET, handoff=passing,
                                  baseline=False, verification=False)
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_NO_TARGET,
           f"an all-PASS handoff is the no-target outcome; got {outcome.route}")
    expect(outcome.owner_issue == 1439,
           f"owned by #1439; got {outcome.owner_issue}")
    expect(not outcome.opens_pull_request,
           "and it opens no pull request")
    expect(outcome.targets == [], "with no targets to report")

    # Any other route over the same handoff is mislabelled.
    for route in (dd.ROUTE_REPAIR, dd.ROUTE_CANNOT_REPRODUCE,
                  dd.ROUTE_PRODUCTION_DEFECT):
        mislabelled = diagnosis_document(route=route, handoff=passing,
                                         baseline=False, verification=False)
        expect_refused(lambda d=mislabelled: evaluate(d),
                       "no target to diagnose",
                       f"an all-PASS handoff declared as {route!r}")

    # And the no-target route over a handoff that DOES name targets.
    inverted = diagnosis_document(route=dd.ROUTE_NO_TARGET)
    expect_refused(lambda: evaluate(inverted),
                   "what an all-PASS measurement produces",
                   "the no-target route over a handoff naming targets")


def test_the_no_target_route_runs_no_controlled_batch() -> None:
    """It stops before creating repair work, so a batch is work it forbids."""
    passing = handoff_document(result=result_document())
    for section in ("baseline", "verification"):
        document = diagnosis_document(route=dd.ROUTE_NO_TARGET,
                                      handoff=passing, baseline=False,
                                      verification=False)
        document[section] = batch_section()
        expect_refused(lambda d=document: evaluate(d),
                       f"runs no {section} batch",
                       f"a no-target diagnosis carrying a {section}")


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
    """`probe_flake` writes the literal `unknown` when git was unreachable.

    That is a well-formed result FIELD — the declared schema accepts it,
    and `probe_census.require_commit_identity` refuses it BY NAME rather
    than by failing a hex test. Diagnosis evidence needs the identity,
    so the placeholder is malformed input here; a `/deflake`-produced
    record can never carry one, because `deflake._require_commit`
    already refuses to record such a measurement.
    """
    document = handoff_document(result=result_document(
        runs=failing_runs(3), commit="unknown"))
    expect_rejected(lambda: dd.require_handoff(document),
                    "is the placeholder 'unknown'",
                    "a result document carrying the placeholder commit")

    for field in ("commit_sha", "base_sha"):
        document = diagnosis_document()
        document["repair"][field] = "unknown"
        expect_rejected(lambda d=document: evaluate(d),
                        "is the placeholder 'unknown'",
                        f"a repair whose {field} is the placeholder")


def test_commit_identity_is_delegated_rather_than_reimplemented() -> None:
    """One grammar, `probe_census`'s, exactly as `timestamp_utc` is.

    A second local copy could drift into accepting a batch the producer
    and the census both reject. The delegation is asserted directly —
    the helper is called and its `CensusError` is converted — because a
    reimplementation that happened to agree today would satisfy every
    behavioural case above while still being a second grammar.
    """
    calls = []
    original = probe_census.require_commit_identity

    def recording(value, what):
        calls.append((value, what))
        return original(value, what)

    probe_census.require_commit_identity = recording
    try:
        evaluate(diagnosis_document())
    finally:
        probe_census.require_commit_identity = original
    expect(calls, "every commit identity goes through probe_census")
    expect(any(what.endswith("commit_sha") for _v, what in calls),
           f"including the result documents' own: {[w for _v, w in calls]}")

    # And the census's own refusal reaches the caller as a controlled
    # malformed-input rejection, never as an escaping `CensusError`.
    try:
        dd.require_commit("nope", "a field", because="because")
    except dd.HandoffError as error:
        expect("because" in str(error),
               f"the refusal says why the field matters: {error}")
    except probe_census.CensusError:  # pragma: no cover - the bug this pins
        expect(False, "a CensusError escaped instead of a HandoffError")
    else:
        expect(False, "an invalid identity was accepted")


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

    # Each record keeps its OWN shape and borrows only the other's argv,
    # so what fails is the launcher rule rather than a missing key.
    swapped = handoff_document()
    swapped["invocation"]["argv"] = command(worktree=PRIMARY_WT)[1:]
    expect_rejected(lambda: dd.require_handoff(swapped),
                    "come from deflake.py",
                    "a handoff claiming a probe_flake.py argv")

    document = diagnosis_document()
    document["baseline"]["invocation"]["command"] = [
        "python3", f"{CLEAN_WT}/tools/deflake.py", "--json"]
    expect_rejected(lambda: evaluate(document),
                    "come from probe_flake.py",
                    "a controlled batch claiming a /deflake argv")

    counterfeit = handoff_document()
    counterfeit["invocation"]["argv"] = [
        "/tmp/counterfeit/deflake.py", "--json"]
    expect_rejected(lambda: dd.require_handoff(counterfeit),
                    "the checkout it declares keeps that tool at",
                    "a handoff claiming a counterfeit /deflake")


def test_a_deflake_command_takes_only_its_own_two_options() -> None:
    for extra in (["--probe", PROBE], ["--runs", "10"],
                  ["--rts-caps", "4"], ["--artifact-root", OUTSIDE]):
        document = handoff_document(inv=deflake_invocation(cmd=[
            f"{PRIMARY_WT}/tools/deflake.py", "--json"] + extra))
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        "does not accept",
                        f"a /deflake command carrying {extra[0]}")

    # `--json` is a flag, so it must not swallow the next argument.
    document = handoff_document(inv=deflake_invocation(cmd=[
        f"{PRIMARY_WT}/tools/deflake.py",
        "--json", "--result", f"{OUTSIDE}/handoff.json"]))
    dd.require_handoff(document)
    document = handoff_document(inv=deflake_invocation(cmd=[
        f"{PRIMARY_WT}/tools/deflake.py",
        "--json=true", "--result", f"{OUTSIDE}/handoff.json"]))
    expect_rejected(lambda: dd.require_handoff(document),
                    "which is a flag", "a value passed to --json")

    # `--result` is OPTIONAL there: /deflake retains the document beside
    # its artifacts whether or not it is also copied out.
    document = handoff_document(inv=deflake_invocation(cmd=[
        f"{PRIMARY_WT}/tools/deflake.py", "--json"]))
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

    `abort=False` is load-bearing: an aborting run leaves the later
    target MISSING, which since the 2026-08-24 correction qualifies for
    repair on its own and would refuse this for the OTHER reason. A
    non-aborting run FAILs its target and emits the rest, so the
    aggregate rule is genuinely the only one left.
    """
    document = diagnosis_document(handoff=handoff_document(acceptable=1))
    document["baseline"]["result"] = result_document(
        runs=failing_runs(1, abort=False))
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
                    "must be a commit hash string",
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
    A token that is not a version-qualified `python3` at all — `pypy`,
    `python4x`, a bare `sh` — never names the interpreter these
    documents quote.
    """
    for program in ("python", "python2", "python2.7", "pypy", "python4x"):
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(
            cmd=[program] + command()[1:])
        expect_rejected(lambda d=document: evaluate(d),
                        "is not a Python 3 interpreter token",
                        f"a command run by {program!r}")

    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(
        cmd=["python3"] + command()[1:])
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"'python3' is the recorded spelling; got {outcome.route}")


def test_a_version_qualified_interpreter_names_the_same_program() -> None:
    """`python3.12` is `python3` said precisely, not a different tool.

    A machine with several Python 3 installations spells the one it
    means with its version, and that command runs exactly the program a
    correctly-pointed `python3` would have run. Refusing the spelling
    outright would reject a truthful record for naming its interpreter
    more exactly than the bare form does.

    Both controlled commands are changed together, because a single
    altered record is already caught by the same-environment comparison
    — which would make this rule look enforced when only that one was.
    """
    for program in ("python3.10", "python3.12", "python3.10.4",
                    f"python3.{dd.INTERPRETER_MINOR_FLOOR}"):
        document = diagnosis_document()
        for batch in ("baseline", "verification"):
            recorded = document[batch]["invocation"]
            recorded["command"] = [program] + recorded["command"][1:]
        outcome = evaluate(document)
        expect(outcome.route == dd.ROUTE_REPAIR,
               f"{program!r} is at or above the floor; got {outcome.route}")


def test_an_interpreter_below_the_syntax_floor_could_not_have_run_it() -> None:
    """`python3.9` names a version that cannot run the program quoting it.

    The floor is DERIVED, not invented: the shipped tools annotate with
    `X | None`, and while `from __future__ import annotations` defers the
    ones in signatures, nothing defers a type evaluated at runtime — so
    3.10 is where these sources stop being runnable rather than merely
    parseable. A record naming an older interpreter describes a run that
    could not have produced the document.
    """
    for program in ("python3.0", "python3.6", "python3.9", "python3.9.18"):
        document = diagnosis_document()
        for batch in ("baseline", "verification"):
            recorded = document[batch]["invocation"]
            recorded["command"] = [program] + recorded["command"][1:]
        expect_rejected(lambda d=document: evaluate(d),
                        "below this lab's 3.10 syntax floor",
                        f"every controlled command run by {program!r}")

    expect(dd.INTERPRETER_MINOR_FLOOR == 10,
           f"the floor is 3.10, the version `X | None` needs; got "
           f"3.{dd.INTERPRETER_MINOR_FLOOR}")


def test_a_malformed_version_token_is_not_an_interpreter() -> None:
    """A version is a dotted run of digits, with no second spelling.

    `python3.010` and `python3.10` would name one interpreter two ways,
    and diagnosis evidence gets one canonical spelling per interpreter
    for the same reason a duplicated option is refused. `python3.` and
    `python3.x` name no version at all.
    """
    for program in ("python3.", "python3.x", "python3..10", "python3.010",
                    "python3.10.", "python3.10.04", "python3.-1"):
        document = diagnosis_document()
        for batch in ("baseline", "verification"):
            recorded = document[batch]["invocation"]
            recorded["command"] = [program] + recorded["command"][1:]
        expect_rejected(lambda d=document: evaluate(d),
                        "is not a Python 3 interpreter token",
                        f"every controlled command run by {program!r}")


def test_a_path_qualified_interpreter_is_refused_whatever_it_names() -> None:
    """The token is a bare name resolved through `PATH`.

    A document cannot show which binary sits at an arbitrary path, so
    the rejection is about the SHAPE of the token and does not depend on
    the version it appears to name — `/usr/bin/python3.12` is refused
    exactly as `/tmp/counterfeit/python3` is.
    """
    for program in ("/usr/bin/python3", "/usr/bin/python3.12",
                    "/tmp/counterfeit/python3", "./python3",
                    "../bin/python3.11"):
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(
            cmd=[program] + command()[1:])
        expect_rejected(lambda d=document: evaluate(d),
                        "runs the interpreter by path",
                        f"a command run by {program!r}")


def test_a_handoff_argv_carries_no_interpreter_token_at_all() -> None:
    """`sys.argv[0]` is the SCRIPT, whatever the interpreter was called.

    Putting a token there is refused as the wrong FORM, which is a
    stronger statement than refusing its version — and it holds for an
    accepted spelling exactly as for a below-floor one.
    """
    for program in ("python3", "python3.12", "python3.9"):
        document = handoff_document()
        document["invocation"]["argv"] = (
            [program] + document["invocation"]["argv"])
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        "Python never puts the interpreter there",
                        f"a handoff argv prefixed with {program!r}")


def test_an_identity_with_a_trailing_newline_is_not_an_identity() -> None:
    """`re.match` with `$` is not full-string validation.

    `$` matches immediately before a final newline, so a 40-character
    hash spelled `"<sha>\n"` satisfies a `re.match` anchored with `$`
    while being no Git SHA at all — which is why
    `probe_census.require_commit_identity` compares the whole string.
    Every identity in the document is mutated together,
    because a document that spells one that way spells all of them that
    way — and a single altered field would be caught by the equality
    comparisons instead, hiding whether this rule is enforced.
    """
    document = diagnosis_document()
    document["handoff"]["result"]["commit_sha"] += "\n"
    document["baseline"]["result"]["commit_sha"] += "\n"
    expect_rejected(lambda d=document: evaluate(d),
                    "must be 40 lowercase hex characters",
                    "a measurement commit with a trailing newline")

    document = diagnosis_document()
    document["repair"]["commit_sha"] += "\n"
    expect_rejected(lambda d=document: evaluate(d),
                    "must be 40 lowercase hex characters",
                    "a repair commit with a trailing newline")

    document = diagnosis_document()
    document["repair"]["base_sha"] += "\n"
    expect_rejected(lambda d=document: evaluate(d),
                    "must be 40 lowercase hex characters",
                    "a repair base commit with a trailing newline")


def test_a_config_digest_with_a_trailing_newline_is_refused() -> None:
    """The same `$`-before-newline hole, on the manifest's digests.

    Driven through `require_manifest` directly, the way every sibling
    manifest rule here is: the default fixture manifest is EMPTY, which
    is this lab's expected default rather than an edge case.
    """
    expect_rejected(
        lambda: dd.require_manifest(
            manifest([("config/video.local.yaml", "c" * 64 + "\n")]),
            "manifest"),
        "SHA-256 digest",
        "a configuration digest with a trailing newline")

    # The same digest without it is the accepted spelling.
    dd.require_manifest(
        manifest([("config/video.local.yaml", "c" * 64)]), "manifest")


def test_a_check_identifier_with_a_trailing_newline_is_refused() -> None:
    """And on the protocol identifiers, for the same reason.

    Renamed in EVERY place the identifier appears — the declaration, each
    run's map, the tally, and the target list — because mutating one
    alone is caught first by the census's own `check_counts` consistency
    rule, which would make this rule look enforced when it was not.
    """
    document = handoff_document()
    original = document["result"]["checks"][0]["id"]
    spelled = original + "\n"
    for entry in document["result"]["checks"]:
        if entry["id"] == original:
            entry["id"] = spelled
    for run in document["result"]["runs"]:
        if original in run["checks"]:
            run["checks"][spelled] = run["checks"].pop(original)
    counts = document["result"]["check_counts"]
    if original in counts:
        counts[spelled] = counts.pop(original)
    document["targets"] = [(spelled if cid == original else cid)
                           for cid in document["targets"]]
    expect_rejected(lambda: dd.require_handoff(document),
                    "no stable identifier",
                    "a check identifier with a trailing newline")


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
            cmd=[f"{elsewhere}/tools/deflake.py", "--json",
                 "--result", f"{OUTSIDE}/handoff.json"],
            directory=elsewhere)
        expect_rejected(lambda d=document: evaluate(d),
                        "is not the primary checkout",
                        f"a handoff claiming to have run in {label}")

    # Spelled differently, the same checkout is still the same checkout.
    document = diagnosis_document()
    document["handoff"]["invocation"] = deflake_invocation(
        cmd=[f"{PRIMARY_WT}/./tools/deflake.py", "--json",
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

    # The inventory is CLOSED and pinned exactly, not spot-checked: a
    # spot check passes while a module quietly leaves the list, and
    # leaving one out is precisely how a repair would reach the
    # apparatus. Every path owns probe selection, launch, port or
    # resource leasing, protocol reconciliation, measurement timing and
    # construction, result recording or census intake, or diagnosis
    # semantics.
    expect(dd.HARNESS_MODULES == (
        "tools/probe_flake.py",
        "tools/probe_protocol.py",
        "tools/probe_census.py",
        "tools/probe_census_contract.py",
        "tools/probe_census_records.py",
        "tools/probe_census_summary.py",
        "tools/probe_census_storage.py",
        "tools/probe_claim.py",
        "tools/probe_resource_lock.py",
        "tools/probe_select.py",
        "tools/probe_engine.py",
        "tools/probelib.py",
        "tools/run_probes.py",
        "tools/probe_runner_registry.py",
        "tools/probe_runner_diagnostics.py",
        "tools/probe_runner_resources.py",
        "tools/probe_runner_lifecycle.py",
        "tools/probe_runner_scheduler.py",
        "tools/deflake.py",
        "tools/deflake_diagnosis.py",
    ), f"the measurement apparatus is exactly this inventory: "
       f"{dd.HARNESS_MODULES}")
    expect("tools/role_probe.py" not in dd.HARNESS_MODULES,
           "and not the probes it runs")

    # Every named path ships, so the inventory cannot drift into
    # excluding a module that no longer exists while a renamed
    # replacement goes unguarded.
    tools = Path(TOOL).parent
    for module in dd.HARNESS_MODULES:
        expect((tools.parent / module).is_file(),
               f"{module} is a real tracked module")


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

        # The producer spells this one `timeout`; the controlled records,
        # which this module defines, spell it `timeout_seconds`.
        producer_field = PRODUCER_FIELD.get(field, field)

        # All three altered together, which no comparison between them
        # could catch.
        document = diagnosis_document()
        document["handoff"]["invocation"][producer_field] = wrong
        for section in ("baseline", "verification"):
            document[section]["invocation"][field] = wrong
        expect_rejected(lambda d=document: evaluate(d),
                        "the only value a real measurement can have used",
                        f"every record altered to another {field}")

        handoff = handoff_document()
        del handoff["invocation"][producer_field]
        expect_rejected(lambda d=handoff: dd.require_handoff(d),
                        f"records no `{field}`",
                        f"a handoff that recorded no {producer_field}")


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


def test_a_malformed_record_is_refused_rather_than_crashing() -> None:
    """A validator that raises on its own input has refused nothing.

    `int(digits)` reads naturally and is a liveness bug on both paths
    that once used it: CPython caps integer-from-string conversion at
    4,300 digits and raises `ValueError` past it, so a version component
    or a pid of five thousand digits escaped as a traceback out of the
    very code that exists to refuse it. Both are compared without
    converting now, so an absurd digit run gets the controlled answer.
    """
    absurd = "9" * 6000
    problem = dd.interpreter_problem(f"python3.{absurd}")
    expect(problem is None,
           f"an absurdly long version is above the floor, not a crash: "
           f"{problem}")
    problem = dd.interpreter_problem(f"python3.{'0' * 5999}9")
    expect(problem is not None and "is not a Python 3 interpreter token"
           in problem,
           f"and a leading-zero one is malformed, not a crash: {problem}")

    name = f"{PROBE}-20260821T120000Z-{absurd}-abcdef12"
    expect(dd.invocation_name_problem(name, PROBE) is None,
           "an absurdly long pid is a positive one, not a crash")
    name = f"{PROBE}-20260821T120000Z-{'0' * 6000}-abcdef12"
    problem = dd.invocation_name_problem(name, PROBE)
    expect(problem is not None and "is not a process id" in problem,
           f"and an all-zero one is refused, not a crash: {problem}")

    # End to end, because the value of this is that `evaluate` answers.
    document = diagnosis_document()
    token = f"python3.{'0' * 20}1"
    for batch in ("baseline", "verification"):
        recorded = document[batch]["invocation"]
        recorded["command"] = [token] + recorded["command"][1:]
    expect_rejected(lambda d=document: evaluate(d),
                    "is not a Python 3 interpreter token",
                    "a controlled command with an absurd version token")


def test_only_ascii_digits_spell_a_generated_directory() -> None:
    """`\\d` in a `str` pattern matches every Unicode decimal digit.

    The harness writes this name from `strftime` and an f-string over
    `os.getpid()`, both of which emit ASCII and nothing else, so a name
    carrying Arabic-Indic or fullwidth digits was not written by the
    measurement it claims to describe — and `\\d+` accepted one while
    `int()` happily read it as a number.
    """
    for label, name in (
            ("an Arabic-Indic pid",
             f"{PROBE}-20260821T120000Z-\u0664\u0667-abcdef12"),
            ("a fullwidth pid",
             f"{PROBE}-20260821T120000Z-\uff14\uff17-abcdef12"),
            ("an Arabic-Indic stamp",
             f"{PROBE}-\u0662\u0660\u0662\u0666\u0660\u0668\u0662\u0661"
             f"T120000Z-4711-abcdef12")):
        expect(dd.invocation_name_problem(name, PROBE) is not None,
               f"{label} is not a generated name: {name!r}")

    expect(dd.INVOCATION_PID_RE.pattern == "[0-9]+"
           and dd.INVOCATION_STAMP_RE.pattern == "[0-9]{8}T[0-9]{6}Z",
           f"both patterns are ASCII-only: {dd.INVOCATION_PID_RE.pattern}, "
           f"{dd.INVOCATION_STAMP_RE.pattern}")


def test_the_generated_name_is_split_from_the_right() -> None:
    """The three generated fields come off the END; the probe stays whole.

    Every probe key registered today is lowercase `[a-z0-9_]` with no
    hyphen, so a left-to-right split happens to agree — but it would
    misattribute part of a hyphenated key to the stamp field the day one
    were registered, and then compare that fragment to the document's
    probe. Right-anchoring is unambiguous whatever the key contains.

    Driven through `invocation_name_problem` directly, because the
    behaviour only DIFFERS for a probe key this repository does not
    register, which no end-to-end fixture could carry.
    """
    stamp, pid, unique = "20260821T120000Z", "4711", "abcdef12"
    for key in ("role", "blood_gpu_lifecycle", "a-hyphenated-key", "a-b"):
        name = f"{key}-{stamp}-{pid}-{unique}"
        expect(dd.invocation_name_problem(name, key) is None,
               f"{name!r} is {key!r}'s own generated directory")
        # A left-to-right split would have read `a` as the probe here and
        # accepted it for a document whose probe is `a`.
        head = key.split("-", 1)[0]
        if head != key:
            expect(dd.invocation_name_problem(name, head) is not None,
                   f"{name!r} does not belong to the probe {head!r}")

    for label, name in (
            ("too few fields", f"{PROBE}-{stamp}-{pid}"),
            ("an empty probe segment", f"-{stamp}-{pid}-{unique}"),
            ("a uuid that is not hex", f"{PROBE}-{stamp}-{pid}-abcdefgh"),
            ("a uuid of the wrong length", f"{PROBE}-{stamp}-{pid}-abcdef1"),
            ("a pid that is not a number", f"{PROBE}-{stamp}-pid-{unique}"),
            ("a stamp of the wrong shape", f"{PROBE}-2026-08-21-{unique}")):
        expect(dd.invocation_name_problem(name, PROBE) is not None,
               f"{label} is not a generated name: {name!r}")


def test_both_spellings_of_a_value_taking_option_are_read() -> None:
    """`--runs 10` and `--runs=10` are one option to argparse, so to this.

    Read as a bare flag instead, `--runs=10` would leave the command
    naming no run count at all and be refused for a requiredness it
    satisfies. The VALUE has to survive the spelling too, which is why
    the accepted case is driven all the way through the binding that
    compares it to the result document.
    """
    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(cmd=[
        "python3", f"{CLEAN_WT}/tools/probe_flake.py",
        f"--probe={PROBE}", f"--runs={dd.RUN_COUNT}",
        f"--rts-caps={dd.RTS_CAPABILITIES}",
        f"--result={OUTSIDE}/baseline.json",
        f"--artifact-root={OUTSIDE}/artifacts"])
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"the inline spelling is the same command; got {outcome.route}")

    # And the value really is read, rather than the option merely being
    # seen: a wrong one is bound to the result document and refused.
    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(cmd=[
        "python3", f"{CLEAN_WT}/tools/probe_flake.py",
        f"--probe={PROBE}", f"--runs={dd.RUN_COUNT + 1}",
        f"--rts-caps={dd.RTS_CAPABILITIES}",
        f"--result={OUTSIDE}/baseline.json",
        f"--artifact-root={OUTSIDE}/artifacts"])
    expect_rejected(lambda d=document: evaluate(d),
                    "where its own result document reports",
                    "an inline --runs that disagrees with its own result")


def _zero_run_batch(token: str = "0", requested: int = 0) -> dict:
    """A baseline whose command and result agree on a count `measure` refuses.

    They have to agree: a `--runs 0` beside a normal ten-run document is
    already refused by the command-to-result binding, which would make
    the positivity rule look enforced when it was not.
    """
    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(cmd=[
        "python3", f"{CLEAN_WT}/tools/probe_flake.py", "--probe", PROBE,
        "--runs", token, "--rts-caps", str(dd.RTS_CAPABILITIES),
        "--result", f"{OUTSIDE}/baseline.json",
        "--artifact-root", f"{OUTSIDE}/artifacts"])
    document["baseline"]["result"] = result_document(runs=[],
                                                     requested=requested)
    return document


def test_a_recorded_count_carries_the_producers_positive_constraint()\
        -> None:
    """`type=int` accepts `0` and `-3`; `measure` refuses both.

    `probe_flake.measure` raises before it resolves a probe or opens a
    port, so a recorded non-positive count describes a command that
    measured nothing. Left to the comparison downstream it would be a
    diagnosis ROUTE — "the baseline did not replay the handoff's
    conditions" — which reports a disagreement between two measurements
    where only one of them exists.
    """
    expect_rejected(lambda: evaluate(_zero_run_batch()),
                    "must be a positive count",
                    "a baseline command with --runs 0")

    # Zero is the only non-positive count reachable end to end: the
    # declared schema already floors `requested_runs` at zero, so a
    # NEGATIVE command value can never agree with a schema-valid result
    # document, and pairing it with a zero one would be refused by the
    # command-to-result binding instead. Driven through the parse
    # itself, which is where the constraint lives.
    for token in ("0", "-1", "-10"):
        expect_rejected(
            lambda t=token: dd._integer(t, "a count", positive=True),
            "must be a positive count",
            f"a parsed count of {token}")
        expect(dd._integer(token, "a count") == int(token),
               f"and {token} is still a perfectly good integer otherwise")

    for token in ("0", "-4"):
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(
            cmd=[*_command_without("--rts-caps"), "--rts-caps", token])
        expect_rejected(lambda d=document: evaluate(d),
                        "must be a positive count",
                        f"a baseline command with --rts-caps {token}")

    expect(dd.POSITIVE_OPTIONS == ("--runs", "--rts-caps"),
           f"both of the harness's integer options carry it: "
           f"{dd.POSITIVE_OPTIONS}")
    expect(dd.HARNESS_LAUNCHER.positive == frozenset(dd.POSITIVE_OPTIONS),
           "and the launcher declares them")
    # `/deflake` exposes no integer option at all, so it declares none.
    expect(dd.DEFLAKE_LAUNCHER.positive == frozenset(),
           f"`/deflake` constrains no command-line integer: "
           f"{dd.DEFLAKE_LAUNCHER.positive}")


def test_an_option_may_not_be_repeated() -> None:
    """Argparse would keep the last value; evidence gets one spelling.

    `--runs 10 --runs 3` reads as three runs to the shipped tool and as
    ten to anyone reading the record left to right, so a duplicate is
    refused rather than resolved. It holds for both grammars, and for a
    flag as well as for a value-taking option.
    """
    for label, extra in (
            ("a repeated --runs", ["--runs", str(dd.RUN_COUNT)]),
            ("a repeated --probe", ["--probe", PROBE]),
            ("a repeated --result", ["--result", f"{OUTSIDE}/baseline.json"]),
            ("a repeated --rts-caps",
             ["--rts-caps", str(dd.RTS_CAPABILITIES)]),
            ("a repeated --artifact-root",
             ["--artifact-root", f"{OUTSIDE}/artifacts"]),
            # The inline spelling is the same option, not a second one.
            ("--runs twice, spelled two ways", [f"--runs={dd.RUN_COUNT}"])):
        document = diagnosis_document()
        document["baseline"]["invocation"] = invocation(
            cmd=command() + extra)
        expect_rejected(lambda d=document: evaluate(d), "repeats",
                        f"a baseline command with {label}")

    for label, extra in (("a repeated --json", ["--json"]),
                         ("a repeated --result",
                          ["--result", f"{OUTSIDE}/handoff.json"])):
        document = handoff_document()
        document["invocation"]["argv"] = (
            list(document["invocation"]["argv"]) + extra)
        expect_rejected(lambda d=document: dd.require_handoff(d), "repeats",
                        f"a handoff argv with {label}")


def test_a_harness_error_run_must_still_have_its_logs() -> None:
    """`stop_with_harness_error` is its only constructor and always passes one.

    `RunRecord.to_document` makes `artifact_dir` nullable because a
    PASSING run has none, so a null on the error run is a shape the
    schema permits and the harness never wrote — and it is the one run
    whose logs say why the stream broke.
    """
    result = result_document(runs=failing_runs(3), harness_error=True)
    retained = result["error_run"]["artifact_dir"]
    expect(retained, "the fixture's error run retains its directory")
    # A harness-error batch is not a usable comparison side, so this is
    # driven through the retention rule itself rather than through the
    # gate, which would refuse the fixture for the batch's status first
    # and hide whether the null case is checked at all.
    dd.require_result(copy.deepcopy(result), "a harness-error result")

    stripped = copy.deepcopy(result)
    stripped["error_run"]["artifact_dir"] = None
    stripped["retained_artifacts"] = [entry for entry
                                      in stripped["retained_artifacts"]
                                      if entry != retained]
    expect_rejected(lambda d=stripped: dd.require_result(d, "a result"),
                    "a failure whose logs are gone",
                    "a harness-error run that kept no artifacts")


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
            ("no script at all", "is not a Python 3 interpreter token",
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
            ("a shell", "is not a Python 3 interpreter token",
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
    # Built THROUGH the producer from that result, so `artifacts` and
    # `invocation.ports` follow it: swapping the result alone would break
    # the envelope's equality rules instead of the containment rule.
    document = diagnosis_document(handoff=handoff_document(
        result=result_document(runs=failing_runs(3),
                               artifact_root=f"{CLEAN_WT}/artifacts")))
    expect_rejected(lambda: evaluate(document, worktrees=()),
                    "inside the working tree",
                    "a handoff whose artifact tree is in a comparison worktree")

    moved = diagnosis_document()
    moved["handoff"]["invocation"]["argv"] = [
        f"{PRIMARY_WT}/tools/deflake.py", "--json",
        "--result", f"{REPAIR_WT}/handoff.json"]
    expect_rejected(lambda: evaluate(moved, worktrees=()),
                    "inside the working tree",
                    "a handoff result document written into a worktree")

    # A handoff can no longer name an EXTRA kept path at all: the approved
    # envelope rule makes `artifacts` equal `result.retained_artifacts`
    # exactly, and that list is `_require_retention`'s derived view of the
    # runs' own directories. So the stricter rule answers first, and a
    # path inside a worktree can only arrive through the artifact ROOT —
    # which is the first case above.
    extra = diagnosis_document()
    extra["handoff"]["artifacts"] = [f"{REPAIR_WT}/kept"]
    expect_rejected(lambda: evaluate(extra, worktrees=()),
                    "cannot disagree",
                    "a handoff naming a kept path its result never retained")


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


DIGEST_A = "a" * 64
DIGEST_B = "b" * 64
LOCAL_YAML = "config/video.local.yaml"


def real_cli_argv(*extra: str) -> list:
    """The argv a REAL `python3 tools/deflake.py ...` process observes.

    Captured from an actual subprocess rather than written out, because
    writing it out is the mistake this exists to stop: both this suite
    and `tools/test_deflake.py` spelled it `["python3", "tools/deflake.py",
    ...]`, and `deflake.main` passes `list(sys.argv)`, whose [0] is the
    SCRIPT. An interpreter-first fixture therefore certified an entry
    gate that no handoff from the real CLI could pass.

    The subprocess runs the launcher with `-c`-free argv and prints what
    `sys.argv` actually is, so the FORM comes from Python rather than
    from this file's belief about it.
    """
    captured = subprocess.run(
        [sys.executable, "-c",
         "import json, sys; print(json.dumps(sys.argv))",
         *extra],
        capture_output=True, text=True, check=True)
    observed = json.loads(captured.stdout)
    # `-c` occupies argv[0] the way a script path would; the invariant
    # under test is that argv[0] is NOT the interpreter and the options
    # follow it directly.
    expect(observed[0] != sys.executable and observed[0] != "python3",
           f"argv[0] is never the interpreter; got {observed[0]!r}")
    expect(observed[1:] == list(extra),
           f"and every remaining token is the script's own: {observed}")
    # The FORM is what the subprocess established; the PATH is the
    # checkout this fixture declares, since that is what the gate
    # resolves the script against.
    return [f"{PRIMARY_WT}/tools/deflake.py", *extra]


def test_a_handoff_from_the_real_cli_path_is_admitted() -> None:
    """End to end across the seam that broke: producer -> entry gate.

    `deflake.measure_next_probe` is the real producer, driven here the
    way `tools/test_deflake.py` drives it — every collaborator injected,
    no probe executed — with the argv form taken from a real subprocess.
    The handoff it WRITES is then handed to the entry gate unmodified.

    This is the case a hand-assembled fixture cannot be: the envelope's
    keys, its argv form, its ports, its targets and its artifacts all
    come from the shipped producer, so a gate that disagrees with #1659
    about any of them fails here rather than in production.
    """
    argv = real_cli_argv("--json", "--result", f"{OUTSIDE}/handoff.json")
    result = result_document(runs=failing_runs(3))
    document = deflake.build_handoff(
        result=result,
        acceptable_failures=1,
        argv=argv,
        cwd=PRIMARY_WT,
        configuration=deflake.configuration_manifest(PRIMARY_CONFIG_ROOT),
        artifacts=list(result["retained_artifacts"]))

    expect(document["invocation"]["argv"][0].endswith("deflake.py"),
           f"the producer records the script at argv[0]; got "
           f"{document['invocation']['argv'][0]!r}")
    expect("python3" not in document["invocation"]["argv"],
           f"and no interpreter token at all: {document['invocation']['argv']}")

    accepted = dd.require_handoff(copy.deepcopy(document), primary=PRIMARY_WT)
    expect(accepted.probe == PROBE,
           "the entry gate admits what the real CLI path produces")
    expect(accepted.targets == tuple(document["targets"]),
           f"with the producer's own targets: {accepted.targets}")
    expect(list(accepted.invocation["ports"])
           == [run["port"] for run in result["runs"]],
           "and the producer's own ports")

    # And it survives the whole diagnosis, not merely the gate.
    diagnosis = diagnosis_document(handoff=document)
    expect(evaluate(diagnosis).route == dd.ROUTE_REPAIR,
           "and a diagnosis built on it reaches its route")


def test_the_entry_gate_reads_the_producers_own_spelling() -> None:
    """`argv`/`cwd`/`timeout`, because that is what #1659 writes.

    This is the regression that mattered: requiring `command`/`directory`
    — the names the CONTROLLED batches use, which this module defines
    itself — rejected every real handoff for "no `directory`", so the
    workflow could not consume its own prerequisite. The messages are
    pinned because a producer document diagnosed against the internal
    spelling is exactly the confusion that hid the bug.
    """
    for field, fragment in (("cwd", "records the directory it ran in"),
                            ("argv", "records the command it ran")):
        document = handoff_document()
        del document["invocation"][field]
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        fragment,
                        f"a handoff invocation with no `{field}`")

    # And the internal spelling is NOT a second accepted form: a document
    # naming `command`/`directory` is not one the producer wrote.
    document = handoff_document()
    document["invocation"] = invocation()
    expect_rejected(lambda: dd.require_handoff(document),
                    "records the directory it ran in",
                    "a handoff invocation in the controlled batches' shape")

    # The producer's own output goes straight through.
    built = deflake.build_handoff(
        result=result_document(runs=failing_runs(3)),
        acceptable_failures=0, argv=deflake_argv(), cwd=PRIMARY_WT,
        configuration=[],
        artifacts=list(result_document(
            runs=failing_runs(3))["retained_artifacts"]))
    accepted = dd.require_handoff(built, primary=PRIMARY_WT)
    expect(accepted.probe == PROBE,
           "a handoff `deflake.build_handoff` actually produced is admitted")
    expect(accepted.invocation["directory"] == PRIMARY_WT,
           "and its `cwd` reaches the internal record as `directory`")
    expect(accepted.invocation["timeout_seconds"] == deflake.TIMEOUT,
           "and its `timeout` as `timeout_seconds`")


def test_the_envelope_redundancies_are_enforced() -> None:
    """Values the producer DERIVED from the result cannot disagree with it.

    `deflake.build_handoff` reads `artifacts` from
    `measurement.retained_artifacts()` and `ports` from the result's own
    runs, so each is one list recorded twice. `probe_census.validate_result`
    checks neither.
    """
    document = handoff_document()
    document["artifacts"] = list(document["artifacts"]) + ["/tmp/extra"]
    expect_rejected(lambda d=document: dd.require_handoff(d),
                    "cannot disagree",
                    "a handoff naming an artifact its result never retained")

    document = handoff_document()
    document["artifacts"] = []
    expect_rejected(lambda d=document: dd.require_handoff(d),
                    "cannot disagree",
                    "a handoff dropping the artifacts its result retained")

    document = handoff_document()
    document["invocation"]["ports"] = [
        port + 1 for port in document["invocation"]["ports"]]
    expect_rejected(lambda d=document: dd.require_handoff(d),
                    "describes runs that did not happen",
                    "a handoff whose ports are not its runs' own")

    document = handoff_document()
    document["invocation"]["ports"] = list(
        reversed(document["invocation"]["ports"]))
    expect_rejected(lambda d=document: dd.require_handoff(d),
                    "in that order",
                    "a handoff whose ports are its runs' in another order")


def test_the_handoff_manifest_defines_both_batches_configuration() -> None:
    """Not the incidental `config/` state when the diagnosis started.

    `Engine.Core.Init.migrateLegacyConfig` can materialize an absent local
    file during a first boot, so "what is there now" and "what the
    measurement read" are different questions. The handoff's manifest is
    the authority, and a batch that diverges from it did not reproduce
    the condition.
    """
    # Present in the handoff, absent from the batch.
    document = diagnosis_document(
        handoff=handoff_document(config=config_entries([(LOCAL_YAML,
                                                         DIGEST_A)])))
    expect_refused(lambda d=document: evaluate(d), "is absent from",
                   "a baseline missing a file the handoff recorded")

    # Absent from the handoff, present in the batch — absence matches as
    # rigorously as contents, or the extra file is an unrecorded condition.
    document = diagnosis_document()
    document["baseline"]["configuration"] = manifest([(LOCAL_YAML, DIGEST_A)])
    expect_refused(lambda d=document: evaluate(d), "is absent from",
                   "a baseline carrying a file the handoff never recorded")

    # Present in both, different bytes.
    document = diagnosis_document(
        handoff=handoff_document(config=config_entries([(LOCAL_YAML,
                                                         DIGEST_A)])))
    document["baseline"]["configuration"] = manifest([(LOCAL_YAML, DIGEST_B)])
    document["verification"]["configuration"] = manifest([(LOCAL_YAML,
                                                           DIGEST_B)])
    expect_refused(lambda d=document: evaluate(d), "differs",
                   "a baseline whose recorded bytes do not match")

    # An empty manifest on both sides is the expected default, not a gap.
    agreeing = diagnosis_document()
    expect(evaluate(agreeing).route == dd.ROUTE_REPAIR,
           "two confirmed-empty manifests are the same condition")


def test_unrecoverable_configuration_bytes_are_the_cannot_reproduce_route()\
        -> None:
    """The condition could not be established, which is a RESULT.

    The approved correction routes it to #1439 with its evidence rather
    than rejecting it: the invocation ran and found it could not recreate
    the bytes, which is exactly what that route reports.
    """
    document = diagnosis_document(
        route=dd.ROUTE_CANNOT_REPRODUCE,
        handoff=handoff_document(config=config_entries([(LOCAL_YAML,
                                                         DIGEST_A)])))
    document.pop("verification", None)
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_CANNOT_REPRODUCE,
           f"an unrecoverable configuration is that outcome; got "
           f"{outcome.route}")
    expect(outcome.owner_issue == 1439,
           f"owned by #1439; got {outcome.owner_issue}")
    expect("configuration state" in outcome.detail,
           f"and it says why: {outcome.detail}")

    # Declared as a repair, the same evidence names the route it should be.
    repair = diagnosis_document(
        handoff=handoff_document(config=config_entries([(LOCAL_YAML,
                                                         DIGEST_A)])))
    expect_refused(lambda: evaluate(repair), "cannot-reproduce",
                   "a repair over a configuration that could not be recreated")


def test_a_batch_must_hold_the_probes_declared_resource_interests() -> None:
    """`peak_concurrency: 1` cannot prove cross-process isolation.

    It counts other flake-harness invocations only; an independent
    `run_probes.py` sweep holding the same repository-relative resource
    never appears in it. `probe_resource_lock` is what coordinates across
    processes, so the batch has to have held the probe's own declared
    interests.
    """
    for section in ("baseline", "verification"):
        absent = diagnosis_document()
        del absent[section]["resource_hold"]
        expect_rejected(lambda d=absent: evaluate(d),
                        "records no `resource_hold`",
                        f"a {section} that recorded no resource hold")

        # The interests are the PROBE's, not the batch's to choose.
        narrowed = diagnosis_document()
        narrowed[section]["resource_hold"] = resource_hold(shared=[])
        expect_rejected(lambda d=narrowed: evaluate(d),
                        "the probe's own, not this batch's to choose",
                        f"a {section} declaring fewer interests than {PROBE}")

        invented = diagnosis_document()
        invented[section]["resource_hold"] = resource_hold(
            exclusive=["a-resource-nobody-declared"])
        expect_rejected(lambda d=invented: evaluate(d),
                        "the probe's own, not this batch's to choose",
                        f"a {section} inventing an exclusive interest")

        # A hold taken AFTER the configuration was installed leaves the
        # manifest describing a state the runs never saw.
        late = diagnosis_document()
        late[section]["resource_hold"] = resource_hold(covers=False)
        expect_rejected(lambda d=late: evaluate(d),
                        "covered the configuration install",
                        f"a {section} hold that started after the install")


def test_a_busy_resource_hold_is_a_measurement_that_did_not_happen() -> None:
    """Another process owned it, so the batch was never controlled.

    Reported as a batch problem and routed to #1439, not raised: the
    documents are well-formed and the invocation really ran — it simply
    did not run under the conditions the comparison assumes.
    """
    document = diagnosis_document(route=dd.ROUTE_CANNOT_REPRODUCE)
    document["baseline"]["resource_hold"] = resource_hold(
        held=False, detail="held by an independent run_probes.py sweep")
    document.pop("verification", None)
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_CANNOT_REPRODUCE,
           f"a contended baseline is cannot-reproduce; got {outcome.route}")
    expect("run_probes.py sweep" in outcome.detail,
           f"and the reason is carried, not summarised away: {outcome.detail}")

    repair = diagnosis_document()
    repair["baseline"]["resource_hold"] = resource_hold(
        held=False, detail="held by an independent run_probes.py sweep")
    expect_refused(lambda: evaluate(repair), "not a usable measurement",
                   "a repair built on a contended baseline")


def test_the_emitted_outcome_carries_every_declared_field() -> None:
    """#1437 owns the PRODUCER record; #1438/#1439 own consumption.

    The approved spec addition enumerates what the one versioned
    diagnosis-result artifact must carry. Asserted field by field against
    that list rather than against a snapshot, so a field that silently
    stopped being populated fails here instead of downstream.
    """
    document = diagnosis_document()
    emitted = evaluate(document).to_document()

    expect(emitted["schema"] == dd.OUTCOME_SCHEMA,
           f"a stable schema: {emitted['schema']}")
    expect(emitted["route"] == dd.ROUTE_REPAIR,
           f"a stable route identifier: {emitted['route']}")

    handoff = document["handoff"]
    identity = emitted["handoff"]
    expect(identity is not None, "the input handoff is identified")
    # The census row cannot answer these, which is why they are here.
    for field, expected in (
            ("probe", handoff["probe"]),
            ("commit_sha", handoff["result"]["commit_sha"]),
            ("acceptable_failures", handoff["acceptable_failures"]),
            ("targets", handoff["targets"]),
            ("timestamp_utc", handoff["result"]["timestamp_utc"]),
            ("artifact_root", handoff["result"]["artifact_root"]),
            ("invocation_dir", handoff["result"]["invocation_dir"]),
            ("command", handoff["invocation"]["argv"]),
            ("directory", handoff["invocation"]["cwd"]),
            ("retained_artifacts", handoff["artifacts"])):
        expect(identity[field] == expected,
               f"the handoff identity carries {field}: "
               f"{identity[field]!r} vs {expected!r}")

    expect(emitted["baseline_sha"] == handoff["result"]["commit_sha"],
           f"the baseline SHA: {emitted['baseline_sha']}")
    expect(emitted["acceptable_failures"] == handoff["acceptable_failures"],
           f"X: {emitted['acceptable_failures']}")
    expect(emitted["configuration"]["entries"]
           == handoff["configuration"],
           f"the configuration manifest: {emitted['configuration']}")

    for label in ("baseline", "verification"):
        reference = emitted[label]
        expect(reference is not None, f"the {label} is referenced")
        result = document[label]["result"]
        for field in ("commit_sha", "artifact_root", "invocation_dir",
                      "retained_artifacts"):
            expect(reference[field] == result[field],
                   f"the {label} reference carries {field}: "
                   f"{reference[field]!r} vs {result[field]!r}")
        expect(reference["worktree"] == document[label]["worktree"],
               f"and the {label} worktree it ran in")

    expect(emitted["diagnosis"] == document["diagnosis"],
           "the diagnosis evidence rides along")
    expect(emitted["attestations"] == document["attestations"],
           "so do the preservation attestations")
    expect(emitted["repair"] == document["repair"],
           "and the repair's commit evidence")

    # A route with no batches leaves the optional halves explicitly null
    # rather than dropping the keys, so a consumer reads one shape.
    passing = handoff_document(result=result_document())
    quiet = evaluate(diagnosis_document(
        route=dd.ROUTE_NO_TARGET, handoff=passing, baseline=False,
        verification=False)).to_document()
    for field in ("baseline", "verification", "repair", "attestations"):
        expect(field in quiet and quiet[field] is None,
               f"the no-target outcome states {field} as null; got "
               f"{quiet.get(field)!r}")
    expect(quiet["handoff"] is not None and quiet["baseline_sha"],
           "while still identifying the handoff it consumed")


def test_a_missing_target_qualifies_for_repair_below_x() -> None:
    """A batch can be clean by the numbers and still have lost a check.

    `probe_protocol.parse_event_stream` represents a declared check that
    was never emitted as MISSING, while `probe_flake.reconcile` classifies
    a zero-exit run carrying no FAIL event as PASS. So the run outcome is
    PASS, the aggregate failure count is 0, and the target check was not
    observed at all — which the approved correction says qualifies for
    repair independently of the aggregate arithmetic.
    """
    # Every run PASSes as a RUN while `gamma` is never emitted.
    ids = [cid for cid, _label in CHECKS]
    lost = [{cid: (MISSING if cid == "gamma" else PASS) for cid in ids}
            for _ in range(dd.RUN_COUNT)]
    baseline = result_document(runs=lost)
    expect(dd.failure_count(baseline) == 0,
           f"the batch is clean by the numbers; got "
           f"{dd.failure_count(baseline)} failures")
    expect(dd.missing_targets(baseline, ("gamma",)) == ["gamma"],
           "and yet the target was never emitted")

    handoff = handoff_document(result=baseline, acceptable=1)
    expect(handoff["targets"] == ["gamma"],
           f"the producer derives the lost check as the target; got "
           f"{handoff['targets']}")

    document = diagnosis_document(handoff=handoff)
    document["baseline"]["result"] = baseline
    outcome = evaluate(document)
    expect(outcome.route == dd.ROUTE_REPAIR,
           f"a reproducibly MISSING target supports a repair below X; got "
           f"{outcome.route}")

    # And the same evidence refuses `cannot-reproduce`, which would be
    # claiming the batch showed nothing.
    denied = diagnosis_document(route=dd.ROUTE_CANNOT_REPRODUCE,
                                handoff=handoff)
    denied["baseline"]["result"] = baseline
    denied.pop("verification", None)
    expect_refused(lambda: evaluate(denied), "as MISSING",
                   "cannot-reproduce over a reproducibly MISSING target")


def test_verification_is_not_relaxed_by_the_missing_qualification() -> None:
    """Repair may START from a MISSING target; it may not END with one.

    The correction widened the pre-fix qualification only. Verification
    still has to come in at or below X AND satisfy the MISSING rules, so
    a repair whose verification still loses the target is not a repair.
    """
    ids = [cid for cid, _label in CHECKS]
    lost = [{cid: (MISSING if cid == "gamma" else PASS) for cid in ids}
            for _ in range(dd.RUN_COUNT)]
    handoff = handoff_document(result=result_document(runs=lost), acceptable=1)
    document = diagnosis_document(handoff=handoff)
    document["baseline"]["result"] = result_document(runs=lost)
    document["verification"]["result"] = verification_result(
        runs=lost, artifact_root=VERIFY_ARTIFACTS)
    expect_refused(lambda: evaluate(document), "MISSING",
                   "a verification that still loses the target")


def test_a_cannot_reproduce_outcome_names_the_baseline_it_ran() -> None:
    document = diagnosis_document(route=dd.ROUTE_CANNOT_REPRODUCE,
                                  handoff=handoff_document(acceptable=1))
    # Non-aborting, so no target is left MISSING: a MISSING target is a
    # reproduced defect and would refuse `cannot-reproduce` outright.
    document["baseline"]["result"] = result_document(
        runs=failing_runs(1, abort=False))
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
    expect_rejected(lambda: evaluate(document),
                    "must be 40 lowercase hex characters",
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
        cmd=[f"{live}/tools/deflake.py", "--json",
             "--result", f"{OUTSIDE}/handoff.json"],
        directory=str(live))
    return document


def test_a_path_no_filesystem_can_name_is_refused_not_a_traceback() -> None:
    """`probe-flake-result/v1` says "string"; the filesystem says more.

    A path carrying an embedded NUL is schema-valid and makes
    `Path.resolve()` raise `ValueError` out of `lstat` — not `OSError`,
    which the containment helper already tolerated, and not this module's
    own exception, which is all `main` catches. So the CLI printed a
    traceback where `handoff-rejected` was the required answer.

    NB the NUL only ever arrives inside a DOCUMENT: `subprocess` refuses
    an argv token containing one, so `--manifest` cannot be handed such a
    root by any real caller.
    """
    for field, place in (("artifact_root", "the result's artifact root"),
                         ("invocation_dir", "the result's invocation dir")):
        document = handoff_document()
        document["result"][field] = "/tmp/\x00"
        expect_rejected(lambda d=document: dd.require_handoff(d),
                        "contains a NUL", f"{place} carrying a NUL")

    # A run's own directory, kept consistent with the retention rule so
    # the NUL is what fails rather than the envelope's equality.
    document = handoff_document()
    unnameable = "/tmp/a\x00b"
    document["result"]["runs"][0]["artifact_dir"] = unnameable
    document["result"]["retained_artifacts"][0] = unnameable
    document["artifacts"][0] = unnameable
    expect_rejected(lambda d=document: dd.require_handoff(d),
                    "contains a NUL", "a run's artifact dir carrying a NUL")

    # Through the real CLI, because "does not raise" and "exits the way
    # this tool is specified to exit" are different claims.
    root = Path(tempfile.mkdtemp(prefix="test_deflake_diagnosis_nul_"))
    try:
        document = handoff_document()
        document["result"]["artifact_root"] = "/tmp/\x00"
        path = root / "handoff.json"
        path.write_text(json.dumps(document), encoding="utf-8")
        done = _run_cli("--handoff", str(path))
        expect(done.returncode == dd.EXIT_REJECTED,
               f"an unnameable path exits {dd.EXIT_REJECTED}; got "
               f"{done.returncode}")
        expect("Traceback" not in done.stderr,
               f"without a traceback: {done.stderr[:200]}")
        expect(dd.ROUTE_HANDOFF_REJECTED in done.stderr,
               f"naming the route: {done.stderr[:200]}")

        # The same through the diagnosis entry point.
        document = _live_document()
        document["baseline"]["result"]["artifact_root"] = "/tmp/\x00"
        path = root / "diagnosis.json"
        path.write_text(json.dumps(document), encoding="utf-8")
        done = _run_cli("--diagnosis", str(path))
        expect(done.returncode == dd.EXIT_REJECTED,
               f"and so does a diagnosis carrying one; got {done.returncode}")
        expect("Traceback" not in done.stderr,
               f"without a traceback: {done.stderr[:200]}")
    finally:
        shutil.rmtree(root, ignore_errors=True)

    # A NUL in a recorded COMMAND destination reaches the containment
    # helper instead, which `_require_canonical` never sees — so
    # `_path_forms` has to stay total for it. Without that, this is an
    # uncaught ValueError rather than a refusal.
    document = diagnosis_document()
    command_tokens = list(document["baseline"]["invocation"]["command"])
    command_tokens[command_tokens.index("--artifact-root") + 1] = "/tmp/\x00a"
    document["baseline"]["invocation"]["command"] = command_tokens
    raised = None
    try:
        evaluate(document)
    except (dd.HandoffError, dd.RouteRefused) as error:
        raised = error
    except Exception as error:                      # noqa: BLE001
        expect(False, f"an unnameable destination escaped as "
                      f"{type(error).__name__}: {error}")
    expect(raised is not None,
           "a command destination carrying a NUL is refused")

    # And the helper is total for it directly.
    forms = dd._path_forms("/tmp/\x00a")
    expect(bool(forms),
           f"_path_forms answers for an unnameable path: {forms}")

    # The helper itself, on the two shapes a document can carry.
    for value in (None, 42, "", "/tmp/\x00"):
        raised = False
        try:
            dd.require_path(value, "a path")
        except dd.HandoffError:
            raised = True
        expect(raised, f"require_path refuses {value!r}")
    expect(dd.require_path("/tmp/fine", "a path") == "/tmp/fine",
           "and returns a usable one unchanged")


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


# ==========================================================================
# Mutation evidence
# ==========================================================================
# Every provenance invariant below is asserted twice: once by a rejection
# test above, and once here by NEUTRALISING exactly that invariant and
# proving the same fixture is then ACCEPTED.
#
# The second half is what makes the first half evidence. A rejection test
# passes just as happily when a DIFFERENT rule is what did the rejecting
# — the fixture that violates an interpreter floor also, quite often,
# violates a path binding — and then the invariant it claims to cover
# could be deleted without a single test turning red. Bypassing one rule
# at a time is the only way to show which rule each case actually holds.
#
# The bypass is a TEXTUAL edit to a private copy of the module source,
# compiled into a throwaway module. Nothing in the shipped module exists
# to support it: a production hook a test could flip would be a second
# code path in the thing under test, which is exactly what this suite is
# for catching.
def mutant(anchor: str, replacement: str):
    """`deflake_diagnosis` with one invariant neutralised, as a module.

    The anchor must appear EXACTLY once. A silently-missed replacement
    would produce an unmodified module, whose faithful rejection would
    then read as "the bypass changed nothing" — evidence for the
    invariant where none was gathered — so a drifted anchor is a loud
    failure rather than a quiet pass.
    """
    source = Path(dd.__file__).read_text(encoding="utf-8")
    found = source.count(anchor)
    if found != 1:
        raise AssertionError(
            f"the mutation anchor appears {found} times, not once: "
            f"{anchor!r}. It has drifted from the module and this case is "
            f"gathering no evidence.")
    module = types.ModuleType(f"deflake_diagnosis_mutant_{abs(hash(anchor))}")
    module.__file__ = dd.__file__
    exec(compile(source.replace(anchor, replacement), dd.__file__, "exec"),
         module.__dict__)
    return module


def _through_evaluate(module, document):
    return module.evaluate(document, worktrees=WORKTREES, primary=PRIMARY_WT)


def _through_gate(module, document):
    return module.require_handoff(document)


def _refusal(module, document, run):
    """The message `module` refuses `document` with, or `None`."""
    try:
        run(module, document)
    except (module.HandoffError, module.RouteRefused) as error:
        return str(error)
    return None


def check_mutation(label, fragment, anchor, replacement, build,
                   run=_through_evaluate):
    """One invariant, held to both halves of the mutation contract.

    `build` returns a fresh document violating this invariant, and
    `fragment` is what the rejection test above asserts. The shipped
    module must refuse it naming that fragment; the module with this one
    rule bypassed must NOT — which is precisely "bypassing only this
    invariant makes its rejection test fail".

    Usually the bypassed module accepts the document outright. Where one
    invariant is nested inside a broader one — two batches that share an
    invocation directory are also, necessarily, each writing inside the
    other's — the bypassed module refuses for the OTHER reason, and the
    rejection test still fails because it is asserting on the message.
    Both outcomes are reported distinctly so a reader can tell which
    happened.
    """
    document = build()
    refusal = _refusal(dd, document, run)
    if refusal is None:
        FAILURES.append(f"{label}: the shipped module ACCEPTED the fixture, "
                        f"so this case proves nothing")
        return
    if fragment not in refusal:
        FAILURES.append(f"{label}: the shipped module refused the fixture for "
                        f"{refusal!r} rather than {fragment!r}, so the "
                        f"fixture does not isolate this invariant")
        return
    try:
        bypassed = mutant(anchor, replacement)
    except AssertionError as error:
        FAILURES.append(f"{label}: {error}")
        return
    after = _refusal(bypassed, build(), run)
    if after is not None and fragment in after:
        FAILURES.append(
            f"{label}: with this invariant bypassed the fixture is still "
            f"refused for {after!r}, so the rejection test above is held up "
            f"by some OTHER rule and this invariant is unevidenced")


def _relaunched(program: str) -> dict:
    """Both controlled commands relaunched under `program`."""
    document = diagnosis_document()
    for batch in ("baseline", "verification"):
        recorded = document[batch]["invocation"]
        recorded["command"] = [program] + recorded["command"][1:]
    return document


def _baseline_command(*tokens) -> dict:
    document = diagnosis_document()
    document["baseline"]["invocation"] = invocation(cmd=list(tokens))
    return document


def _relocate_artifacts(result, old: str, new: str) -> dict:
    """Respell every artifact-derived path in `result`, consistently.

    A fixture that changed one of them would violate the topology rule
    as well as whatever it meant to test, and the mutation harness would
    correctly report that it proves nothing.
    """
    result["artifact_root"] = result["artifact_root"].replace(old, new)
    result["invocation_dir"] = result["invocation_dir"].replace(old, new)
    for run in result["runs"] + ([result["error_run"]]
                                 if result.get("error_run") else []):
        if run.get("artifact_dir"):
            run["artifact_dir"] = run["artifact_dir"].replace(old, new)
    result["retained_artifacts"] = [entry.replace(old, new)
                                    for entry in result["retained_artifacts"]]
    return result


def _command_without(option: str) -> list:
    tokens = command()
    index = tokens.index(option)
    return tokens[:index] + tokens[index + 2:]


def _require_result(module, document):
    return module.require_result(document, "a result")


def _deflake_argv_without_json(*extra) -> dict:
    document = handoff_document()
    document["invocation"]["argv"] = [
        token for token in document["invocation"]["argv"]
        if token != "--json"] + list(extra)
    return document


def test_the_interpreter_floor_is_what_refuses_an_old_interpreter() -> None:
    check_mutation(
        "the 3.10 syntax floor",
        "below this lab's 3.10 syntax floor",
        "    if minor is not None and _below(minor, "
        "INTERPRETER_MINOR_FLOOR):",
        "    if False:",
        lambda: _relaunched("python3.9"))


def test_the_interpreter_grammar_is_what_refuses_another_program() -> None:
    check_mutation(
        "the interpreter token grammar",
        "is not a Python 3 interpreter token",
        "    matched = INTERPRETER_RE.fullmatch(program)",
        '    matched = INTERPRETER_RE.fullmatch("python3")',
        lambda: _relaunched("pypy"))


def test_the_bare_name_rule_is_what_refuses_a_path_qualified_interpreter()\
        -> None:
    """Bypassing it means reading the token as the bare name it ends with.

    The two interpreter rules cannot be separated by choosing a cleverer
    fixture — no path-qualified token satisfies the grammar, because the
    grammar admits no separator — so the bypass is what isolates them:
    with the path rule gone, `/tmp/counterfeit/python3` is `python3`.
    """
    check_mutation(
        "the bare-interpreter-name rule",
        "runs the interpreter by path",
        "    if os.sep in program or (os.altsep and os.altsep in program):",
        "    program = os.path.basename(program)\n    if False:",
        lambda: _relaunched("/tmp/counterfeit/python3"))


def test_the_script_binding_is_what_refuses_a_counterfeit_tool() -> None:
    check_mutation(
        "the script-to-checkout binding",
        "the checkout it declares keeps that tool at",
        "        if not (_path_forms(script, base if base is not None "
        "else root)\n                & _path_forms(expected)):",
        "        if False:",
        lambda: _baseline_command(
            "python3", "/tmp/counterfeit/probe_flake.py", *command()[2:]))


def test_the_option_surface_is_what_refuses_an_invented_option() -> None:
    check_mutation(
        "the closed option surface",
        "does not accept",
        "        if name not in found.options:",
        "        if False:",
        lambda: _baseline_command(*command(), "--timeout", "600"))


def test_the_positional_rule_is_what_refuses_a_bare_token() -> None:
    """Bypassing it means silently ignoring the token instead.

    Reading it as an option name would only move the refusal to the
    closed option surface, which is a different invariant with its own
    case above.
    """
    check_mutation(
        "the no-positionals rule",
        "positional token",
        '        if not token.startswith("--"):\n'
        '            raise HandoffError(',
        '        if not token.startswith("--"):\n'
        '            index += 1\n'
        '            continue\n'
        '        if False:\n'
        '            raise HandoffError(',
        lambda: _baseline_command(*command(), "extra"))


def test_the_arity_rule_is_what_refuses_a_value_on_a_flag() -> None:
    check_mutation(
        "flag arity",
        "which is a flag",
        '            if inline:\n'
        '                raise HandoffError(\n'
        '                    f"{what} passes a value to {name}, which is a '
        'flag")',
        '            if inline:\n'
        '                pass',
        lambda: _deflake_argv_without_json("--json=yes"),
        run=_through_gate)


def test_the_duplicate_rule_is_what_refuses_a_repeated_option() -> None:
    check_mutation(
        "duplicate-option rejection",
        "repeats",
        '            raise HandoffError(f"{what} repeats {name}")',
        "            pass",
        lambda: _baseline_command(*command(), "--runs", str(dd.RUN_COUNT)))


def test_the_missing_value_rule_is_what_refuses_a_dangling_option() -> None:
    """The dangling option is one the command does not already carry.

    Repeating an option it does have would be refused as a duplicate — a
    different invariant, with its own case above.
    """
    check_mutation(
        "a value-taking option with no value",
        "has no value",
        '                raise HandoffError(f"{what}: {token} has no value")',
        "                break",
        lambda: _baseline_command(*_command_without("--artifact-root"),
                                  "--artifact-root"))


def test_the_required_rule_is_what_refuses_a_command_with_no_result() -> None:
    check_mutation(
        "required options",
        "names no --result",
        "    for option in found.required:",
        "    for option in ():",
        lambda: _baseline_command(*_command_without("--result")))


def test_the_positive_rule_is_what_refuses_a_zero_run_count() -> None:
    """Bypassed, the same record surfaces as a condition disagreement.

    That is the exact failure the rule exists to prevent: a route
    reporting that two measurements disagreed, when one of them was
    never run.
    """
    check_mutation(
        "the producer's positive-value constraint",
        "must be a positive count",
        "    if positive and number < 1:",
        "    if False:",
        _zero_run_batch)


def test_the_integer_grammar_is_what_refuses_a_float_run_count() -> None:
    check_mutation(
        "argparse's own `int()` grammar",
        "must be an integer",
        "        number = int(value)",
        "        number = int(float(value))",
        lambda: _baseline_command(*_command_without("--runs"),
                                  "--runs", f"{dd.RUN_COUNT}.0"))


def test_canonical_spelling_is_what_refuses_a_traversal_path() -> None:
    """Every artifact path is respelled together, so only spelling differs.

    Changing one of them alone would move the invocation directory out
    from under its own root, and the topology rule — not this one —
    would be what refused the fixture.
    """
    check_mutation(
        "resolve-canonical artifact paths",
        "which is not the spelling",
        "    if resolved != path:",
        "    if False:",
        lambda: handoff_document(result=_relocate_artifacts(
            result_document(runs=failing_runs(3)),
            f"{OUTSIDE}/artifacts", f"{OUTSIDE}/forged/../artifacts")),
        run=_through_gate)


def test_the_direct_child_rule_is_what_refuses_a_nested_invocation() -> None:
    check_mutation(
        "invocation_dir is a direct child of artifact_root",
        "DIRECT child of the root",
        "    if invocation.parent != root:",
        "    if False:",
        lambda: handoff_document(result=_relocate_artifacts(
            result_document(runs=failing_runs(3)),
            f"{OUTSIDE}/artifacts/{PROBE}",
            f"{OUTSIDE}/artifacts/deeper/{PROBE}")),
        run=_through_gate)


def test_the_generated_name_rule_is_what_refuses_a_hand_made_directory()\
        -> None:
    check_mutation(
        "the generated invocation-directory name",
        "not a directory this measurement created",
        "    problem = invocation_name_problem(invocation.name, "
        'document["probe"])',
        "    problem = None",
        lambda: handoff_document(result=_relocate_artifacts(
            result_document(runs=failing_runs(3)),
            f"{PROBE}-20260821T120000Z-4711-abcdef12", f"{PROBE}-evidence")),
        run=_through_gate)


def test_the_run_index_rule_is_what_refuses_a_reordered_batch() -> None:
    """The records are SWAPPED whole, so each keeps its own `run-NNN`.

    Swapping only the index fields would put run 2's record in run 1's
    directory, and the topology rule would refuse it first.
    """
    def build():
        result = result_document(runs=failing_runs(3))
        result["runs"][0], result["runs"][1] = (result["runs"][1],
                                                result["runs"][0])
        result["retained_artifacts"] = [run["artifact_dir"]
                                        for run in result["runs"]
                                        if run["artifact_dir"]]
        return result

    check_mutation("run indices are 1..len(runs)",
        "numbers its runs",
                   "    if indices != expected:", "    if False:",
                   build, run=_require_result)


def test_the_error_index_rule_is_what_refuses_a_stray_error_run() -> None:
    """The error run's own directory moves with its index.

    Leaving the directory behind would collide with a completed run's,
    and the topology rule would be what refused the fixture.
    """
    def build():
        result = result_document(runs=failing_runs(3), harness_error=True)
        broken = result["error_run"]
        stale = broken["artifact_dir"]
        broken["index"] = len(result["runs"]) + 2
        broken["artifact_dir"] = str(Path(result["invocation_dir"])
                                     / f"run-{broken['index']:03d}")
        result["retained_artifacts"] = [
            broken["artifact_dir"] if entry == stale else entry
            for entry in result["retained_artifacts"]]
        return result

    check_mutation(
        "the harness-error run's index",
        "numbers its harness-error run",
        '    if isinstance(broken, dict) and broken["index"] != len(expected) '
        '+ 1:',
        "    if False:",
        build, run=_require_result)


def test_the_run_directory_rule_is_what_refuses_a_foreign_artifact_dir()\
        -> None:
    def build():
        result = result_document(runs=failing_runs(3))
        run = result["runs"][0]
        elsewhere = f"{OUTSIDE}/artifacts/elsewhere/run-001"
        result["retained_artifacts"] = [
            elsewhere if entry == run["artifact_dir"] else entry
            for entry in result["retained_artifacts"]]
        run["artifact_dir"] = elsewhere
        return result

    check_mutation("every run directory is `invocation_dir/run-NNN`",
        "every run directory is",
                   "        if Path(directory) != expected:",
                   "        if False:",
                   build, run=_require_result)


def test_the_retention_rule_is_what_refuses_a_kept_passing_run() -> None:
    """The kept directory is NOT declared, so only this rule is violated.

    Declaring it too would break the retained-list equality, which has
    its own case below.
    """
    def build():
        result = result_document(runs=failing_runs(3))
        for run in result["runs"]:
            if run["outcome"] == probe_flake.RUN_PASS:
                run["artifact_dir"] = str(Path(result["invocation_dir"])
                                          / f"run-{run['index']:03d}")
                break
        return result

    check_mutation(
        "a passing run keeps nothing",
        "passed and still names the artifact directory",
        "            if directory is not None:", "            if False:",
        build, run=_require_result)


def test_the_retention_rule_is_what_refuses_a_discarded_failure() -> None:
    def build():
        result = result_document(runs=failing_runs(3))
        for run in result["runs"]:
            if run["artifact_dir"]:
                result["retained_artifacts"] = [
                    entry for entry in result["retained_artifacts"]
                    if entry != run["artifact_dir"]]
                run["artifact_dir"] = None
                break
        return result

    check_mutation(
        "an unsuccessful run keeps everything",
        "a failure whose logs are gone",
        '        if directory is None:\n'
        '            raise HandoffError(\n'
        '                f"{where} did not pass',
        '        if directory is None:\n'
        '            continue\n'
        '        if False:\n'
        '            raise HandoffError(\n'
        '                f"{where} did not pass',
        build, run=_require_result)


def test_the_retained_list_rule_is_what_refuses_a_shuffled_list() -> None:
    """Ordered equality: a shuffled list names the same set, in no order.

    The bypass compares the two as SETS, which is exactly the weakening
    this rule exists to refuse — `Measurement.retained_artifacts` builds
    the list in run order and the error run comes last.
    """
    def build():
        result = result_document(runs=failing_runs(3))
        result["retained_artifacts"] = list(
            reversed(result["retained_artifacts"]))
        return result

    check_mutation(
        "retained_artifacts is the ORDERED list of kept directories",
        "naming evidence it does not have",
        "    if list(declared) != retained:",
        "    if sorted(declared) != sorted(retained):",
        build, run=_require_result)


def test_the_commit_delegation_is_what_refuses_a_placeholder_identity()\
        -> None:
    check_mutation(
        "commit identity via probe_census",
        "is the placeholder 'unknown'",
        "        return probe_census.require_commit_identity(value, what)",
        "        return value",
        lambda: result_document(runs=failing_runs(3), commit="unknown"),
        run=_require_result)


def test_the_timestamp_delegation_is_what_refuses_a_shaped_non_instant()\
        -> None:
    def build():
        result = result_document(runs=failing_runs(3))
        result["timestamp_utc"] = "2026-99-99T99:99:99Z"
        return result

    check_mutation(
        "timestamp via probe_census.parse_timestamp",
        "records WHEN its baseline was measured",
        '        probe_census.parse_timestamp(document.get("timestamp_utc"),',
        '        _ = 0 and probe_census.parse_timestamp('
        'document.get("timestamp_utc"),',
        build, run=_require_result)


def test_the_schema_delegation_is_what_refuses_an_impossible_result() -> None:
    """A run that says PASS while its own check map carries a FAIL.

    `probe_census.validate_result`'s rule and nothing local — which is
    what makes the delegation load-bearing rather than decorative.
    """
    def build():
        result = result_document(runs=failing_runs(3))
        for run in result["runs"]:
            if run["outcome"] == probe_flake.RUN_FAIL:
                run["outcome"] = probe_flake.RUN_PASS
                result["retained_artifacts"] = [
                    entry for entry in result["retained_artifacts"]
                    if entry != run["artifact_dir"]]
                run["artifact_dir"] = None
                break
        return result

    check_mutation(
        "probe_census.validate_result",
        "PASS",
        "        probe_census.validate_result(document)",
        "        pass",
        build, run=_require_result)


def test_the_containment_rule_is_what_refuses_a_shared_invocation_dir()\
        -> None:
    def build():
        """Both batches default their root, so only the sharing is wrong.

        Pointing the verification at the baseline's invocation directory
        while it declared its own root would put that directory outside
        the root it reports, and the direct-child rule would be what
        refused the fixture.
        """
        def defaulted(section, tree, result):
            section["invocation"] = invocation(
                cmd=["python3", f"{tree}/tools/probe_flake.py",
                     "--probe", PROBE, "--runs", str(dd.RUN_COUNT),
                     "--rts-caps", str(dd.RTS_CAPABILITIES),
                     "--result", result],
                directory=tree)

        document = diagnosis_document()
        defaulted(document["baseline"], CLEAN_WT, f"{OUTSIDE}/baseline.json")
        defaulted(document["verification"], REPAIR_WT,
                  f"{OUTSIDE}/verify.json")
        shared_root = f"{OUTSIDE}/defaulted"
        document["baseline"]["result"] = result_document(
            runs=failing_runs(4), artifact_root=shared_root)
        document["verification"]["result"] = verification_result(
            artifact_root=shared_root)
        return document

    check_mutation(
        "the two batches hold distinct invocation directories",
        "creates a fresh one per invocation",
        "    if shared:", "    if False:", build)


def test_the_apparatus_inventory_is_what_refuses_a_harness_repair() -> None:
    check_mutation(
        "the measurement-apparatus inventory",
        "measurement apparatus",
        "    apparatus = [path for path in changed if path in "
        "HARNESS_MODULES]",
        "    apparatus = []",
        lambda: diagnosis_document(repair={
            "commit_sha": REPAIR_COMMIT, "base_sha": BASE_COMMIT,
            "changed_paths": ["tools/role_probe.py", "tools/probe_flake.py"]}))


# ==========================================================================
# #1439: the non-success outcomes of a de-flake attempt
# ==========================================================================
#
# `tools/deflake_outcome.py` consumes the record #1437 emits and decides
# whether the evidence supports a STABLE non-success outcome. The rules
# below are the ones a test can hold it to: what may become
# `cannot-reproduce`, what an improvement has to be to be one, that no
# route reaches a publisher, and that a census append is durable,
# idempotent and destroys nothing.
#
# The diagnosis records these cases feed it are PRODUCED by #1437's own
# evaluator rather than hand-assembled, for the reason `handoff_document`
# already gives: a hand-written envelope agrees with whatever the
# consumer happens to require, which is exactly how a consumer that
# could never read a real producer record keeps a green suite.
ATTEMPT = "role-20260828T120000Z-4711-abcdef12"
OUTCOME_NOW = "2026-08-28T12:00:00Z"
OUTCOME_SUMMARY = ("ten controlled runs at the handoff's own commit, with "
                   "the configuration recreated from its manifest")
UNMET = ("the verification stayed above the acceptable-failure ceiling the "
         "handoff carries")

_DEFAULT = object()


def spotless_result(**kwargs) -> dict:
    """A complete measurement in which nothing at all went wrong."""
    return result_document(**kwargs)



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


def short_result() -> dict:
    """Nine runs of a ten-run batch, and a document that says `ok`.

    `probe_census.validate_result` accepts this — only a NON-accepted
    status is required to leave a run uncompleted — so the incomplete
    run set really does have to be caught by the classifier rather than
    by the schema in front of it.
    """
    ids = [c for c, _label in CHECKS]
    return result_document(
        runs=[{name: PASS for name in ids} for _ in range(dd.RUN_COUNT - 1)])


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


def forged_aggregate_result(shape: str = "silent-timeout") -> dict:
    """A batch whose aggregates disagree with its own run list.

    Nothing in the declared schema binds `failure_count` or
    `timeout_count` to `runs`, so "an inconsistent aggregate" is a shape
    the classifier has to refuse for itself — and each counter is an
    INDEPENDENT account, so each gets a fixture only its own clause can
    see:

    * `silent-timeout` — the last run timed out AFTER emitting every
      declared check, so the per-check tallies are spotless and
      consistent and only `runs` says anything went wrong, with both
      counters zeroed.
    * `phantom-failures` — a spotless run list under a non-zero failure
      count.
    * `phantom-timeouts` — a spotless run list, a zero failure count,
      and a non-zero timeout count. `probe_flake` counts a timeout as a
      failure too, so a batch claiming timeouts and no failures is one
      no harness wrote.
    * `phantom-rate` — honest counts under a rate neither of them
      implies. `failure_rate` is the third account, and it is the one
      every summary and every threshold comparison downstream reads.
    """
    ids = [c for c, _label in CHECKS]
    timing_out = shape == "silent-timeout"
    runs = [{name: PASS for name in ids}
            for _ in range(dd.RUN_COUNT - (1 if timing_out else 0))]
    if timing_out:
        runs.append({**{name: PASS for name in ids}, "__timeout__": True})
    document = result_document(runs=runs)
    if timing_out:
        document["failure_count"] = 0
        document["failure_rate"] = 0.0
        document["timeout_count"] = 0
    elif shape == "phantom-failures":
        document["failure_count"] = 3
        document["failure_rate"] = 0.3
    elif shape == "phantom-timeouts":
        document["timeout_count"] = 2
    elif shape == "phantom-rate":
        document["failure_rate"] = 0.4
    else:
        raise AssertionError(f"unknown forged shape {shape!r}")
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


def elsewhere_failure_result(count: int = 4) -> dict:
    """Over X, and never on a target check.

    The default targets are `beta` and `gamma`; this fails `alpha` and
    keeps going, so every run emits everything and the batch is over
    tolerance while demonstrating nothing about the checks under
    diagnosis. `deflake_diagnosis.evaluate` calls that
    `cannot-reproduce`.
    """
    return result_document(runs=failing_runs(count, cid="alpha",
                                             abort=False))


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


def route_diagnosis(route: str) -> dict:
    """A diagnosis document `dd.evaluate` really does take down `route`."""
    if route == dd.ROUTE_NO_TARGET:
        return diagnosis_document(
            route=route, handoff=handoff_document(result=result_document()),
            baseline=False, verification=False)
    document = diagnosis_document(route=route)
    if route == dd.ROUTE_CANNOT_REPRODUCE:
        # The controlled baseline reproduced nothing: no failure over X
        # and no target left MISSING.
        document["baseline"]["result"] = spotless_result()
    elif route == dd.ROUTE_PARTIAL_IMPROVEMENT:
        # Better than the baseline's four failures and still over an X
        # of zero. `abort=False` keeps every declared check emitted, so
        # the MISSING half of the acceptance gate is satisfied and the
        # failure COUNT is the half that is not.
        document["verification"]["result"] = verification_result(
            runs=failing_runs(2, abort=False), artifact_root=VERIFY_ARTIFACTS)
    return document


def produced(route: str) -> tuple:
    """`(diagnosis document, the deflake-diagnosis-outcome/v1 it emits)`."""
    document = route_diagnosis(route)
    return document, evaluate(document).to_document()


def measurement_entries(route: str, diagnosis: dict) -> list:
    """The measurements the route rests on, exactly as the harness left them.

    Pulled out of the diagnosis document rather than rebuilt, so the
    evidence #1439 classifies is literally the evidence #1437 judged.
    """
    if route == dd.ROUTE_NO_TARGET:
        return [{"role": do.ROLE_HANDOFF, "exit_code": probe_flake.EXIT_OK,
                 "result": diagnosis["handoff"]["result"]}]
    entries = [{"role": do.ROLE_BASELINE, "exit_code": probe_flake.EXIT_OK,
                "result": diagnosis["baseline"]["result"]}]
    if route == dd.ROUTE_PARTIAL_IMPROVEMENT:
        entries.append({"role": do.ROLE_VERIFICATION,
                        "exit_code": probe_flake.EXIT_OK,
                        "result": diagnosis["verification"]["result"]})
    return entries


def rebind_references(record: dict, measurements) -> dict:
    """Refresh the producer record's own references from `measurements`.

    A case that substitutes a measurement is asking a question about the
    CLASSIFIER, and #1439 binds every declared measurement to the
    reference #1437 recorded for it — so a fixture that swapped a result
    without moving the reference would be refused for the substitution
    rather than answering the question it asked. This keeps the envelope
    internally consistent in every dimension except the one the case is
    actually about; the binding tests pass `rebind=False` and supply the
    mismatch deliberately.
    """
    record = copy.deepcopy(record)
    for entry in measurements:
        section = record.get(entry["role"])
        result = entry.get("result")
        if not isinstance(section, dict) or not isinstance(result, dict):
            continue
        for field in do.REFERENCE_FIELDS:
            section[field] = copy.deepcopy(result[field])
    # The top-level list is #1437's ordered, deduplicated union of every
    # batch it ran, and the entry gate rebuilds it from those same
    # references — so a fixture that moved a reference has to move this
    # with it or the substitution is what gets refused.
    union: list = []
    for role in do.ROLES:
        section = record.get(role)
        if not isinstance(section, dict):
            continue
        for path in section.get("retained_artifacts") or []:
            if path not in union:
                union.append(path)
    record["retained_artifacts"] = union
    return record


def outcome_handoff(route: str = dd.ROUTE_CANNOT_REPRODUCE, *,
                    attempt: str = ATTEMPT, summary: str = OUTCOME_SUMMARY,
                    measurements=None, unmet=None, rebind: bool = True,
                    diagnosis_outcome=_DEFAULT) -> dict:
    document_diagnosis, record = produced(route)
    if diagnosis_outcome is not _DEFAULT:
        record = diagnosis_outcome
    entries = (measurement_entries(route, document_diagnosis)
               if measurements is None else measurements)
    if measurements is not None and rebind:
        record = rebind_references(record, entries)
    envelope = {
        "schema": do.HANDOFF_SCHEMA,
        "attempt": attempt,
        "summary": summary,
        "diagnosis_outcome": record,
        "measurements": entries,
    }
    if unmet is not None:
        envelope["unmet_condition"] = unmet
    return copy.deepcopy(envelope)


class Publisher:
    """The pull-request publisher, injected so its silence is provable."""

    def __init__(self) -> None:
        self.calls: list = []

    def __call__(self, record) -> str:
        self.calls.append(record)
        return "https://example.invalid/pull/1"


@contextlib.contextmanager
def census_file():
    """A real seeded census on disk, in a directory of its own."""
    root = Path(tempfile.mkdtemp(prefix="deflake_outcome_census_"))
    try:
        path = root / "docs" / "probe_census.json"
        path.parent.mkdir(parents=True)
        probe_census.ensure_document(path)
        yield path
    finally:
        shutil.rmtree(root, ignore_errors=True)


def record_outcome(handoff_document_, path, *, publisher=None, now=OUTCOME_NOW):
    """Run the whole workflow over one handoff, with a spy publisher."""
    publisher = Publisher() if publisher is None else publisher
    accepted = do.require_handoff(handoff_document_, worktrees=WORKTREES,
                                  primary=PRIMARY_WT)
    return do.record(accepted, census_path=path, now=now,
                     publisher=publisher), publisher


def stored_outcomes(path, probe: str = PROBE) -> list:
    document = json.loads(Path(path).read_text(encoding="utf-8"))
    row = probe_census.find_entry(document, probe)
    return list((row or {}).get("census", {}).get("outcomes") or [])


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


# ==========================================================================
# #1438: filing an issue when the bug is in the engine
# ==========================================================================
#
# `tools/deflake_issue.py` consumes the SAME `deflake-outcome-handoff/v1`
# envelope #1439 does, on the one route #1439 refuses: #1437's
# `production-defect`. It files one review-ready tracker issue carrying
# the measured evidence, records that issue in the probe's census row,
# and stops — the probe is not touched and no pull request is opened.
#
# The tracker is a fake at the publication boundary, so "exactly one
# issue" is a counted fact rather than a hope; the census, its schema,
# and #1437's own evaluator are real.
DEFECT_ATTEMPT = "role-20260829T090000Z-4711-beefcafe"
DEFECT_NOW = "2026-08-29T09:00:00Z"
DEFECT_SUMMARY = ("ten controlled runs at the handoff's own commit "
                  "reproduced the ordering in four of them")
# A diagnosis of PRODUCTION behaviour, which is what this route is for.
# The shared fixture's default names a probe-side setup precondition —
# #1437's repair route — and says nothing about the engine.
DEFECT_DIAGNOSIS = {
    "category": None,
    "summary": ("World.Thread publishes a chunk before its tile map is "
                "installed, so a query issued immediately after "
                "loadChunksInRegion reads a flat column"),
    "evidence": [
        "run 1's engine log records the publish ahead of the install",
        "runs 2 and 3 show the same ordering, and the passing runs do not",
    ],
}
DEFECT_EVENTS = ('{"schema":"probe-event/v1","check":"beta",'
                 '"result":"FAIL","detail":"tile z was 0"}\n')
DEFECT_STDOUT = "probe: beta failed: expected a loaded column\n"
DEFECT_ENGINE_LOG = ("[World] chunk 3,4 published\n"
                     "[World] chunk 3,4 tile map installed\n")


def defect_handoff(*, attempt: str = DEFECT_ATTEMPT,
                   summary: str = DEFECT_SUMMARY, diagnosis=_DEFAULT,
                   document=None, measurements=None,
                   rebind: bool = True) -> dict:
    """One `deflake-outcome-handoff/v1` on #1438's own route.

    The producer record is PRODUCED by #1437's evaluator rather than
    hand-assembled, for the reason every other fixture here is: a
    hand-written envelope agrees with whatever the consumer happens to
    require, which is how a consumer that could never read a real
    producer record keeps a green suite.
    """
    block = DEFECT_DIAGNOSIS if diagnosis is _DEFAULT else diagnosis
    if document is None:
        document = diagnosis_document(route=dd.ROUTE_PRODUCTION_DEFECT,
                                      diagnosis=block)
    record = evaluate(document).to_document()
    entries = (measurement_entries(dd.ROUTE_PRODUCTION_DEFECT, document)
               if measurements is None else measurements)
    if measurements is not None and rebind:
        record = rebind_references(record, entries)
    return copy.deepcopy({
        "schema": di.HANDOFF_SCHEMA,
        "attempt": attempt,
        "summary": summary,
        "diagnosis_outcome": record,
        "measurements": entries,
    })


@contextlib.contextmanager
def staged_evidence(document, *, events=DEFECT_EVENTS,
                    stdout=DEFECT_STDOUT, engine=DEFECT_ENGINE_LOG,
                    only=None):
    """The retained artifacts a real failing batch would have left.

    Every other fixture here treats an artifact path as a STRING,
    because #1439 stores references and never opens one. This workflow
    reads them, so the tree has to exist. `only` stages a prefix of the
    run list, which is how "the artifacts were pruned" is expressed.
    """
    paths = [Path(entry) for entry
             in document["diagnosis_outcome"]["retained_artifacts"]]
    staged = paths if only is None else paths[:only]
    try:
        for directory in staged:
            (directory / "engine").mkdir(parents=True, exist_ok=True)
            (directory / "events.jsonl").write_text(events, encoding="utf-8")
            (directory / "stdout.txt").write_text(stdout, encoding="utf-8")
            (directory / "engine" / "engine-9101.log").write_text(
                engine, encoding="utf-8")
        yield staged
    finally:
        for directory in staged:
            shutil.rmtree(directory, ignore_errors=True)
        # Up to and including the fixture root, so a run of this suite
        # leaves nothing behind in `/tmp`. `rmdir` refuses a non-empty
        # directory, so this can only remove what the fixture made.
        for directory in staged:
            for parent in directory.parents:
                if not str(parent).startswith(OUTSIDE):
                    break
                with contextlib.suppress(OSError):
                    parent.rmdir()


class FakePublication(di.Publication):
    """The tracker, faked at the only boundary that reaches it.

    It counts, so "exactly one issue" is observed rather than assumed;
    it stores what it was given under the publication key the BODY
    carries, so a reconcile finds an issue only if the publisher really
    wrote the marker; and it can be told to fail either operation, which
    is how a publication failure is exercised without a network.
    """

    def __init__(self, *, find_error=None, create_error=None, issues=None,
                 answer=None):
        self.finds: list = []
        self.creates: list = []
        # `key -> {"number", "url", "body"}`, because the real boundary
        # answers with the body: it is what proves the match is the
        # marker LINE rather than a quotation, and where the issue's own
        # origin brand is read from.
        self.issues = dict(issues or {})
        self.find_error = find_error
        self.create_error = create_error
        self.answer = answer
        self.next_number = 901

    @staticmethod
    def filed_key(body: str):
        head = f"<!-- {di.PUBLICATION_MARKER}: "
        for line in body.splitlines():
            if line.startswith(head) and line.endswith(" -->"):
                return line[len(head):-len(" -->")]
        return None

    def find(self, key: str):
        self.finds.append(key)
        if self.find_error is not None:
            raise self.find_error
        return copy.deepcopy(self.issues.get(key))

    def create(self, *, title: str, body: str):
        self.creates.append({"title": title, "body": body})
        if self.create_error is not None:
            raise self.create_error
        if self.answer is not None:
            return copy.deepcopy(self.answer)
        number = self.next_number
        self.next_number += 1
        issue = {"number": number,
                 "url": f"https://github.com/coghex/synarchy/issues/{number}"}
        key = self.filed_key(body)
        if key is not None:
            self.issues[key] = dict(issue, body=body)
        return copy.deepcopy(issue)


class Spy:
    """A forbidden boundary, injected so its silence is provable."""

    def __init__(self) -> None:
        self.calls: list = []

    def __call__(self, record) -> None:
        self.calls.append(record)


def defect_key(document) -> str:
    return di.publication_key(
        di.accept(document, worktrees=WORKTREES, primary=PRIMARY_WT))


def file_defect(document, path, *, publication=None, origin: str = "claude",
                now: str = DEFECT_NOW, probe_spy=None, pr_spy=None):
    """Run the whole workflow over one handoff, with every boundary faked."""
    publication = FakePublication() if publication is None else publication
    probe_spy = Spy() if probe_spy is None else probe_spy
    pr_spy = Spy() if pr_spy is None else pr_spy
    defect = di.accept(document, worktrees=WORKTREES, primary=PRIMARY_WT)
    published = di.publish(defect, census_path=path, now=now,
                           publication=publication, origin=origin,
                           probe_publisher=probe_spy,
                           pull_request_publisher=pr_spy)
    return published, publication, probe_spy, pr_spy


def expect_not_filed(thunk, fragment: str, msg: str) -> None:
    """`thunk` is a well-formed handoff whose ending files nothing."""
    try:
        thunk()
    except di.NonSuccess as error:
        expect(fragment in str(error),
               f"{msg}: refused, but for {str(error)!r} rather than "
               f"{fragment!r}")
        return
    except di.HandoffError as error:
        FAILURES.append(f"{msg}: rejected the INPUT ({error}) where the "
                        f"EVIDENCE should have been refused")
        return
    FAILURES.append(f"{msg}: filed an issue")


def expect_nothing_published(path, before: bytes, publication, msg: str,
                             *, searched=None) -> None:
    """The assertion every non-success owes: no issue, no record, no trace."""
    expect(publication.creates == [],
           f"{msg}: an issue was created anyway")
    expect(Path(path).read_bytes() == before,
           f"{msg}: the census bytes changed")
    expect(not stored_outcomes(path),
           f"{msg}: an outcome was recorded anyway")
    if searched is not None:
        expect(len(publication.finds) == searched,
               f"{msg}: the tracker was searched {len(publication.finds)} "
               f"time(s), not {searched}")


def test_1438_owns_exactly_the_route_1437_hands_it() -> None:
    """Parity with the producer, from both sides of the sibling split."""
    owned = {route for route, owner in dd.ROUTE_OWNER.items()
             if owner == di.OWNER_ISSUE}
    expect(owned == {dd.ROUTE_PRODUCTION_DEFECT},
           f"#1437 hands #1438 exactly the production-defect route; got "
           f"{sorted(owned)}")
    expect(set(di.OWNED.roles) == owned,
           f"and this workflow claims exactly it; got "
           f"{sorted(di.OWNED.roles)}")
    expect(di.OWNED.outcomes == (di.OUTCOME_PRODUCTION_DEFECT,),
           f"reaching one outcome; got {di.OWNED.outcomes}")
    expect(di.OUTCOME_PRODUCTION_DEFECT not in do.STABLE_OUTCOMES,
           "and it is not one of #1439's stable outcomes")
    roles = di.OWNED.roles[dd.ROUTE_PRODUCTION_DEFECT]
    expect(roles["designated"] in roles["required"],
           f"the route is judged on a measurement it requires; got {roles}")
    expect(not (set(roles["required"]) & set(roles["forbidden"])),
           f"and does not both require and forbid a role; got {roles}")
    expect(do.ROLE_VERIFICATION in roles["forbidden"],
           "and runs no verification batch: #1437 refuses that route a "
           "verification outright, because one would mean a repair was "
           "attempted")
    # Constructing this workflow's ownership leaves the sibling's alone.
    expect(do.OWNED.issue == 1439
           and set(do.OWNED.roles) == set(do.ROUTE_TO_OUTCOME),
           f"#1439's own ownership is untouched; got {do.OWNED.issue} over "
           f"{sorted(do.OWNED.roles)}")


def test_a_production_defect_files_one_issue_carrying_its_evidence() -> None:
    """The whole acceptance case, in one pass.

    Exactly one issue; every measurement fact the approved amendment
    names; log evidence a reviewer can READ rather than a path only the
    measuring machine can open; the returned identity recorded; and the
    ending terminal.
    """
    document = defect_handoff()
    key = defect_key(document)
    with staged_evidence(document), census_file() as path:
        published, publication, probe_spy, pr_spy = file_defect(
            document, path)
        expect(len(publication.creates) == 1,
               f"exactly one issue is created; got "
               f"{len(publication.creates)}")
        expect(publication.finds == [key],
               f"reconciled against the publication key BEFORE creating; "
               f"got {publication.finds}")
        body = publication.creates[0]["body"]
        title = publication.creates[0]["title"]
        expect(PROBE in title and "production defect" in title,
               f"the title names the probe and the diagnosis; got {title!r}")

        result = [entry for entry in document["measurements"]
                  if entry["role"] == do.ROLE_BASELINE][0]["result"]
        required = {
            "the probe": f"`{PROBE}`",
            "the failure numerator and denominator":
                f"{result['failure_count']}/{result['requested_runs']}",
            "the failure rate": f"rate {result['failure_rate']}",
            "the timeout count": f"Timeouts: {result['timeout_count']}",
            "the measured commit": f"`{result['commit_sha']}`",
            "the completed run count":
                f"{result['completed_runs']} completed of "
                f"{result['requested_runs']} requested",
            "the RTS capability setting":
                f"+RTS -N{result['rts_capabilities']}",
            "the diagnosed production behaviour":
                DEFECT_DIAGNOSIS["summary"],
            "the attempt identity": DEFECT_ATTEMPT,
            "the acceptable-failure ceiling": "Acceptable failures (X): 0",
        }
        for what, fragment in required.items():
            expect(fragment in body,
                   f"the filed body states {what} ({fragment!r})")
        for cid, tally in sorted(result["check_counts"].items()):
            row = (f"| `{cid}` | {tally[PASS]} | {tally[FAIL]} "
                   f"| {tally[MISSING]} |")
            expect(row in body,
                   f"and every declared check's PASS/FAIL/MISSING tally "
                   f"({row!r})")
        expect(DEFECT_STDOUT.strip() in body
               and DEFECT_ENGINE_LOG.splitlines()[0] in body,
               "and bounded excerpts of the retained failure artifacts, not "
               "their pathnames alone")
        expect(result["retained_artifacts"][0] in body,
               "beside the artifact path the full log can be found at")

        issue = published.record["issue"]
        expect(issue["number"] == 901
               and issue["url"].endswith("/issues/901"),
               f"the returned issue identity is recorded; got {issue}")
        expect(issue["publication_key"] == key
               and issue["origin"] == "claude",
               f"under the key and brand it was filed with; got {issue}")
        stored = stored_outcomes(path)
        expect(len(stored) == 1
               and stored[0]["outcome"] == di.OUTCOME_PRODUCTION_DEFECT
               and stored[0]["issue"] == issue,
               f"and the census holds exactly that one outcome; got "
               f"{stored}")
        expect(stored[0]["recommendation"] is None
               and stored[0]["comparison"] is None,
               "with the two route-specific fields this ending has nothing "
               "to say in stored as an explicit null")
        report = published.to_document()
        expect(report["terminal"] is True
               and report["created_issue"] is True
               and report["reconciled_issue"] is False
               and report["resumed"] is False,
               f"the report says what actually happened; got {report}")
        expect(probe_spy.calls == [] and pr_spy.calls == [],
               "and neither forbidden boundary was reached")


def test_filing_reaches_neither_the_probe_nor_the_pull_request() -> None:
    """The two boundaries, from both sides.

    Both tables are consulted on the one route this workflow owns, so
    the silence below is a branch that really ran — flipping an entry
    makes the injected spy fire, which is what stops the assertion being
    vacuous.
    """
    expect(set(di.CHANGES_THE_PROBE) == {di.OUTCOME_PRODUCTION_DEFECT}
           and not any(di.CHANGES_THE_PROBE.values()),
           f"the filed defect changes no probe; got {di.CHANGES_THE_PROBE}")
    expect(set(di.OPENS_PULL_REQUEST) == {di.OUTCOME_PRODUCTION_DEFECT}
           and not any(di.OPENS_PULL_REQUEST.values()),
           f"and opens no pull request; got {di.OPENS_PULL_REQUEST}")

    document = defect_handoff()
    with staged_evidence(document), census_file() as path:
        published, _publication, probe_spy, pr_spy = file_defect(
            document, path)
        expect(probe_spy.calls == [] and pr_spy.calls == [],
               "neither publisher was called on the shipped policy")
        expect(published.changed_probe is False
               and published.opened_pull_request is False,
               "and the report says what the boundaries DID, not what the "
               "policy says")

    for table, attribute in ((di.CHANGES_THE_PROBE, "changed_probe"),
                             (di.OPENS_PULL_REQUEST, "opened_pull_request")):
        saved = dict(table)
        table[di.OUTCOME_PRODUCTION_DEFECT] = True
        try:
            with staged_evidence(document), census_file() as path:
                published, _pub, probe_spy, pr_spy = file_defect(
                    document, path)
                fired = (probe_spy.calls if attribute == "changed_probe"
                         else pr_spy.calls)
                expect(len(fired) == 1
                       and getattr(published, attribute) is True,
                       f"the {attribute} boundary is consulted rather than "
                       f"absent, so `never called` is an observed fact")
        finally:
            table.clear()
            table.update(saved)

    # And both defaults refuse rather than quietly succeeding.
    for publisher in (di.forbidden_probe_change, di.forbidden_pull_request):
        try:
            publisher({"outcome": di.OUTCOME_PRODUCTION_DEFECT})
        except di.NonSuccess:
            continue
        FAILURES.append(f"{publisher.__name__} accepted a call")


def test_resuming_a_filed_defect_touches_the_tracker_not_at_all() -> None:
    """A recorded outcome is the completion marker.

    Not merely "creates no second issue": a completed attempt must not
    reach the tracker at all, so a resume is free of network traffic and
    of whatever the tracker happens to answer that day. The clock and
    the origin differ deliberately — both are reused from the stored
    record, so the rebuilt record is a replay rather than a conflict.
    """
    document = defect_handoff()
    with staged_evidence(document), census_file() as path:
        first, _publication, _probe, _pr = file_defect(document, path)
        before = Path(path).read_bytes()
        again, publication, probe_spy, pr_spy = file_defect(
            document, path, origin="codex", now="2026-08-30T10:00:00Z")
        expect(publication.finds == [] and publication.creates == [],
               f"the tracker was not consulted at all; got "
               f"{publication.finds} / {len(publication.creates)}")
        expect(again.resumed is True and again.created is False
               and again.reconciled is False,
               f"the resume says so; got {again.to_document()}")
        expect(again.record == first.record,
               "and installs the identical record, stamp, issue and origin "
               "included")
        expect(Path(path).read_bytes() == before,
               "leaving the census byte-identical")
        expect(len(stored_outcomes(path)) == 1,
               "with one outcome recorded, not two")
        expect(probe_spy.calls == [] and pr_spy.calls == [],
               "and no forbidden boundary reached on the resume either")


def test_an_issue_created_before_a_crash_is_reconciled_not_duplicated()\
        -> None:
    """The window a completion marker alone cannot close.

    Creation takes effect and its identity is never durably recorded —
    a timeout, a crash, or a census write that refuses in between. The
    resume must find the issue the first attempt filed, record it once,
    and create nothing.
    """
    document = defect_handoff()
    with staged_evidence(document), census_file() as path:
        publication = FakePublication()
        healthy = json.loads(Path(path).read_text(encoding="utf-8"))
        # A census on a schema this writer refuses: the issue is
        # created, and then nothing is recorded. That is the crash
        # window, made deterministic.
        Path(path).write_text(
            json.dumps(dict(healthy, schema=probe_census.SEED_SCHEMA)),
            encoding="utf-8")
        before = Path(path).read_bytes()
        expect_not_filed(
            lambda: file_defect(document, path, publication=publication),
            "exists", "a census that refused after the issue was created")
        expect(len(publication.creates) == 1,
               f"the issue was created before the census refused; got "
               f"{len(publication.creates)}")
        expect(Path(path).read_bytes() == before,
               "and the refusal left the census byte-identical")

        # The resume, against a census that works again.
        Path(path).write_text(json.dumps(healthy), encoding="utf-8")
        published, _pub, probe_spy, pr_spy = file_defect(
            document, path, publication=publication)
        expect(len(publication.creates) == 1,
               f"the resume creates nothing; got "
               f"{len(publication.creates)} creation(s) in total")
        expect(published.reconciled is True and published.created is False
               and published.resumed is False,
               f"it reconciles the existing issue; got "
               f"{published.to_document()}")
        expect(published.record["issue"]["number"] == 901,
               f"recording the issue that already existed; got "
               f"{published.record['issue']}")
        recorded = stored_outcomes(path)
        expect(len(recorded) == 1
               and recorded[0]["issue"]["number"] == 901,
               f"exactly once; got {recorded}")
        expect(probe_spy.calls == [] and pr_spy.calls == [],
               "and still neither forbidden boundary")

        # A third invocation is an ordinary resume again.
        third, publication, _probe, _pr = file_defect(document, path)
        expect(third.resumed is True and publication.creates == []
               and len(stored_outcomes(path)) == 1,
               "and the attempt stays settled afterwards")


def test_an_attempt_identity_already_used_elsewhere_files_nothing() -> None:
    """One attempt identity identifies one attempt, across both siblings.

    The completion marker this workflow reads is "the census holds this
    attempt". A record the SIBLING wrote under the same identity is not
    that, and reusing its issue is unrepresentable — it has none — so it
    is refused before anything reaches the tracker rather than
    discovered as an attribute error on the way to one.
    """
    sibling = outcome_handoff(attempt=DEFECT_ATTEMPT)
    document = defect_handoff()
    with staged_evidence(document), census_file() as path:
        record_outcome(sibling, path)
        before = Path(path).read_bytes()
        publication = FakePublication()
        expect_not_filed(
            lambda: file_defect(document, path, publication=publication),
            "already recorded",
            "an attempt identity the sibling workflow already used")
        expect(publication.creates == [] and publication.finds == [],
               "the tracker was not consulted")
        expect(Path(path).read_bytes() == before,
               "and the census was left byte-identical")


def test_a_reconcile_answering_with_an_unusable_identity_files_nothing()\
        -> None:
    """The tracker's answer is validated, not taken on trust."""
    document = defect_handoff()
    key = defect_key(document)
    expect(not di.carries_key(None, key),
           "a missing body matches no publication key")
    with staged_evidence(document), census_file() as path:
        before = Path(path).read_bytes()
        publication = FakePublication(issues={key: {"number": 0,
                                                    "url": "not-a-url"}})
        expect_not_filed(
            lambda: file_defect(document, path, publication=publication),
            "not a positive integer",
            "a reconcile answering with an unusable issue number")
        expect_nothing_published(path, before, publication,
                                 "an unusable reconcile")


def test_the_publication_key_is_derived_from_the_attempt() -> None:
    """Stable across invocations, and different for a different attempt."""
    document = defect_handoff()
    first = defect_key(document)
    again = defect_key(copy.deepcopy(document))
    expect(first == again and re.fullmatch(r"[0-9a-f]{64}", first),
           f"the key is a stable sha256 of the attempt; got {first!r} and "
           f"{again!r}")
    other = defect_handoff(attempt="role-20260829T090000Z-4711-0000face")
    expect(defect_key(other) != first,
           "and another attempt of the same probe files under its own")


def test_the_filed_issue_enters_the_canonical_review_gate() -> None:
    """The routing metadata, spelled the way the gate reads it."""
    document = defect_handoff()
    with staged_evidence(document), census_file() as path:
        _published, publication, _probe, _pr = file_defect(
            document, path, origin="codex")
        body = publication.creates[0]["body"]
        marker = re.compile(r"<!--\s*issue-origin:(claude|codex)\s*-->")
        found = marker.search(body)
        expect(found is not None and found.group(1) == "codex",
               "the filed body carries the issue-origin marker the review "
               "gate routes on, spelling the brand it was filed by")
        expect(body.rstrip().endswith(di.origin_marker("codex")),
               "as its last line, where an origin marker belongs")
    with staged_evidence(document), census_file() as path:
        before = Path(path).read_bytes()
        publication = FakePublication()
        try:
            file_defect(document, path, publication=publication,
                        origin="nobody")
        except di.HandoffError as error:
            expect("issue-origin" in str(error),
                   f"an unknown brand is rejected naming the marker; got "
                   f"{error}")
        else:
            FAILURES.append("an unknown issue origin was accepted")
        expect_nothing_published(path, before, publication,
                                 "an unknown issue origin", searched=0)


def test_the_origin_vocabulary_is_one_vocabulary() -> None:
    """The module and the schema spell the review gate's brands alike.

    `origin` is the one enum the census schema declares that also lives
    in a module constant, so the two are held to each other rather than
    left to drift the day a third brand appears.
    """
    declared = probe_census.load_schema()["$defs"]["outcome_issue"]
    expect(tuple(declared["properties"]["origin"]["enum"]) == di.ORIGINS,
           f"the schema enumerates {declared['properties']['origin']['enum']} "
           f"where the module knows {list(di.ORIGINS)}")
    expect(set(declared["required"])
           == {"number", "url", "publication_key", "origin"},
           f"and the stored issue identity is exactly what "
           f"require_issue_identity builds; got {declared['required']}")


def test_an_issue_with_no_readable_evidence_is_not_filed() -> None:
    """A machine-local pathname alone is not reviewable log evidence."""
    document = defect_handoff()
    with census_file() as path:
        before = Path(path).read_bytes()
        publication = FakePublication()
        expect_not_filed(
            lambda: file_defect(document, path, publication=publication),
            "no retained artifact",
            "an attempt whose artifacts have all been pruned")
        # Searched, and correctly so: the reconcile runs BEFORE anything
        # is rendered, so a retry whose issue already exists recovers
        # even with its artifacts gone. Only the case with no issue to
        # find reaches the evidence, and only then is there something to
        # file at all.
        expect_nothing_published(path, before, publication,
                                 "pruned artifacts", searched=1)
    # One readable run is enough, and only what was read is quoted.
    with staged_evidence(document, only=1), census_file() as path:
        _published, publication, _probe, _pr = file_defect(document, path)
        body = publication.creates[0]["body"]
        expect(body.count("#### baseline run ") == 1,
               "one readable run is evidence enough, and only it is quoted")


def test_a_symlinked_artifact_component_reaches_nothing() -> None:
    """This module QUOTES what it finds into a published issue.

    So a symlink under the declared artifact root is not a layout to
    read through: `engine -> elsewhere` would otherwise have every
    listing and open below it land there and publish whatever regular
    files live there as this probe's failure evidence. #1437's own
    canonical-path rule catches a run directory that was ALREADY a
    symlink when the handoff was validated; every component is opened
    `O_NOFOLLOW` anyway, which covers the rest of the tree and the race
    between that validation and this read.
    """
    document = defect_handoff()
    elsewhere = Path(tempfile.mkdtemp(prefix="deflake_elsewhere_"))
    try:
        (elsewhere / "secret.log").write_text("PRIVATE HOST STATE\n",
                                              encoding="utf-8")
        # Named the way a run directory's own files are named, so a
        # substituted directory would really be READ by an
        # implementation that followed the link rather than merely
        # finding nothing there.
        (elsewhere / "stdout.txt").write_text("PRIVATE HOST STATE\n",
                                              encoding="utf-8")
        runs = [Path(entry) for entry
                in document["diagnosis_outcome"]["retained_artifacts"]]

        # (a) `engine` is a symlink to somewhere else entirely.
        with staged_evidence(document), census_file() as path:
            shutil.rmtree(runs[0] / "engine")
            (runs[0] / "engine").symlink_to(elsewhere)
            _published, publication, _probe, _pr = file_defect(document, path)
            body = publication.creates[0]["body"]
            expect("PRIVATE HOST STATE" not in body,
                   "a symlinked engine directory is not descended")
            expect(DEFECT_STDOUT.strip() in body,
                   "while the run's own real files are still quoted")

        # (b) a symlinked artifact FILE, and a non-regular one. An open
        # that blocked on the FIFO would hang the workflow, not merely
        # read the wrong bytes.
        with staged_evidence(document, only=1), census_file() as path:
            (runs[0] / "stdout.txt").unlink()
            (runs[0] / "stdout.txt").symlink_to(elsewhere / "secret.log")
            log = runs[0] / "engine" / "engine-9101.log"
            log.unlink()
            if hasattr(os, "mkfifo"):
                os.mkfifo(log)
            _published, publication, _probe, _pr = file_defect(document, path)
            body = publication.creates[0]["body"]
            expect("PRIVATE HOST STATE" not in body,
                   "a symlinked artifact file is not read")
            expect(DEFECT_EVENTS.strip() in body,
                   "and the real protocol stream beside it still is")

        # (c) the run directory ITSELF substituted after validation —
        # the race #1437's gate cannot see, since it validated the real
        # path. Read directly, because a handoff declaring one would be
        # refused at the gate instead of reaching this.
        with staged_evidence(document, only=1):
            root = str(runs[0].parent.parent)
            substitute = runs[0].parent / "run-009"
            substitute.symlink_to(elsewhere)
            try:
                expect(di.run_excerpts(root, str(substitute)) == [],
                       "a substituted run directory yields no excerpt")
                expect(di.open_run_directory(root, str(elsewhere)) is None,
                       "and a run directory outside the declared root is "
                       "not walked at all")
                expect(len(di.run_excerpts(root, str(runs[0]))) >= 2,
                       "while the genuine directory still reads")
            finally:
                substitute.unlink()
    finally:
        shutil.rmtree(elsewhere, ignore_errors=True)


def test_the_quoted_evidence_is_bounded() -> None:
    """A whole engine log is not a review surface."""
    noisy = "".join(f"[World] line {index}\n" for index in range(5000))
    document = defect_handoff()
    with staged_evidence(document, engine=noisy), census_file() as path:
        _published, publication, _probe, _pr = file_defect(document, path)
        body = publication.creates[0]["body"]
        expect(len(body) <= did.MAX_BODY_CHARS,
               f"the body fits the tracker's limit; got {len(body)}")
        expect("[World] line 4999" in body and "[World] line 0\n" not in body,
               "and the TAIL of the log is what is quoted, which is where a "
               "failing run stops")
        expect(body.count("#### baseline run ") <= di.MAX_EVIDENCE_RUNS,
               f"over at most {di.MAX_EVIDENCE_RUNS} runs")
    # An engine log is arbitrary bytes and may contain a fence of its
    # own; quoting it in a three-backtick block would end the block early
    # and render the rest of the log as markdown.
    fenced = "before\n```\nstill the log\n````\nend of log\n"
    with staged_evidence(document, engine=fenced), census_file() as path:
        _published, publication, _probe, _pr = file_defect(document, path)
        body = publication.creates[0]["body"]
        expect("end of log" in body and "`````" in body,
               "a log carrying its own fence is quoted inside a longer one")


def test_a_route_this_workflow_does_not_own_files_nothing() -> None:
    """The sibling split, from #1438's side."""
    for route in (dd.ROUTE_CANNOT_REPRODUCE, dd.ROUTE_NO_CONFIDENT_FIX,
                  dd.ROUTE_PARTIAL_IMPROVEMENT, dd.ROUTE_NO_TARGET):
        document = outcome_handoff(route)
        with census_file() as path:
            before = Path(path).read_bytes()
            publication = FakePublication()
            expect_not_filed(
                lambda d=document, p=path, pub=publication: file_defect(
                    d, p, publication=pub),
                "#1439", f"the {route!r} route filed as a production defect")
            expect_nothing_published(path, before, publication,
                                     f"the {route!r} route", searched=0)


def test_untrustworthy_or_unreproduced_evidence_is_never_filed() -> None:
    """The evidence is judged BEFORE anything reaches the tracker."""
    def only_baseline(result, exit_code=probe_flake.EXIT_OK) -> list:
        return [{"role": do.ROLE_BASELINE, "exit_code": exit_code,
                 "result": result}]

    cases = (
        ("an incomplete run set",
         lambda: defect_handoff(measurements=only_baseline(short_result())),
         "completed 9 of 10"),
        ("an aggregate that contradicts its own run list",
         lambda: defect_handoff(
             measurements=only_baseline(forged_aggregate_result())),
         "measurement reports"),
        ("failures confined to checks nobody targeted",
         lambda: defect_handoff(
             measurements=only_baseline(elsewhere_failure_result())),
         "did not reproduce the pattern"),
        ("a baseline that reproduced nothing at all",
         lambda: defect_handoff(
             measurements=only_baseline(spotless_result())),
         "reproduced nothing to attribute"),
    )
    for label, build, fragment in cases:
        document = build()
        with staged_evidence(document), census_file() as path:
            before = Path(path).read_bytes()
            publication = FakePublication()
            expect_not_filed(
                lambda d=document, p=path, pub=publication: file_defect(
                    d, p, publication=pub),
                fragment, label)
            expect_nothing_published(path, before, publication, label,
                                     searched=0)


def test_a_publication_failure_leaves_the_attempt_pending() -> None:
    """Neither boundary failure records anything, and neither falls through."""
    document = defect_handoff()
    for label, publication, fragment in (
            ("a reconcile that failed",
             FakePublication(find_error=di.PublicationFailed(
                 "gh issue list exited 1")), "gh issue list"),
            ("a creation that failed",
             FakePublication(create_error=di.PublicationFailed(
                 "gh issue create exited 1")), "gh issue create"),
            ("a creation that answered with no issue number",
             FakePublication(answer={"url": "https://example.com/issues/7"}),
             "not a positive integer"),
    ):
        with staged_evidence(document), census_file() as path:
            before = Path(path).read_bytes()
            probe_spy, pr_spy = Spy(), Spy()
            expect_not_filed(
                lambda p=path, pub=publication: file_defect(
                    document, p, publication=pub, probe_spy=probe_spy,
                    pr_spy=pr_spy),
                fragment, label)
            expect(Path(path).read_bytes() == before
                   and not stored_outcomes(path),
                   f"{label}: something was recorded anyway")
            expect(probe_spy.calls == [] and pr_spy.calls == [],
                   f"{label}: a failure fell through to a forbidden "
                   f"boundary")


def test_a_malformed_defect_handoff_is_rejected_without_filing() -> None:
    """The shared entry gate, reached through this workflow's own route."""
    def broken(mutate):
        document = defect_handoff()
        mutate(document)
        return document

    cases = (
        ("a handoff on another schema",
         lambda: broken(lambda d: d.__setitem__("schema", "nope")),
         f"expected {di.HANDOFF_SCHEMA!r}"),
        ("a handoff with no attempt identity",
         lambda: broken(lambda d: d.pop("attempt")),
         "`attempt` identity"),
        ("a producer record with no diagnosis block",
         lambda: broken(lambda d: d["diagnosis_outcome"].pop("diagnosis")),
         "states no `diagnosis` block"),
        ("a diagnosis with no evidence",
         lambda: broken(lambda d: d["diagnosis_outcome"]["diagnosis"]
                        .__setitem__("evidence", [])),
         "records no evidence"),
        ("a diagnosis with no summary",
         lambda: broken(lambda d: d["diagnosis_outcome"]["diagnosis"]
                        .__setitem__("summary", "  ")),
         "states no `summary`"),
        ("a retained artifact inside a comparison worktree",
         lambda: broken(lambda d: d["diagnosis_outcome"].__setitem__(
             "retained_artifacts", [f"{CLEAN_WT}/artifacts/run-001"])),
         "inside the worktree"),
        ("a measurement taken at another instant",
         lambda: broken(lambda d: d["measurements"][0]["result"]
                        .__setitem__("timestamp_utc",
                                     "2026-08-22T09:30:00Z")),
         "timestamp_utc"),
        ("a verification batch this route never runs",
         lambda: broken(lambda d: d["measurements"].append(
             {"role": do.ROLE_VERIFICATION,
              "exit_code": probe_flake.EXIT_OK,
              "result": verification_result()})),
         "runs no verification batch"),
    )
    for label, build, fragment in cases:
        document = build()
        with census_file() as path:
            before = Path(path).read_bytes()
            publication = FakePublication()
            try:
                file_defect(document, path, publication=publication)
            except di.HandoffError as error:
                expect(fragment in str(error),
                       f"{label}: rejected, but for {str(error)!r} rather "
                       f"than {fragment!r}")
            except di.NonSuccess as error:
                FAILURES.append(f"{label}: refused the EVIDENCE ({error}) "
                                f"where the input should have been rejected")
            else:
                FAILURES.append(f"{label}: accepted")
            expect_nothing_published(path, before, publication, label,
                                     searched=0)


def test_the_census_schema_pairs_the_outcome_with_its_issue() -> None:
    """Declared, so neither half can be recorded without the other."""
    document = defect_handoff()
    with staged_evidence(document), census_file() as path:
        published, _pub, _probe, _pr = file_defect(document, path)
        record = copy.deepcopy(published.record)

    with census_file() as path:
        without = {key: value for key, value in record.items()
                   if key != "issue"}
        try:
            probe_census.record_outcome(path, PROBE, without)
        except probe_census.CensusError:
            pass
        else:
            FAILURES.append("a production defect was recorded with no issue")
        expect(not stored_outcomes(path),
               "and nothing was stored by the refusal")

    with census_file() as path:
        stable = copy.deepcopy(record)
        stable["outcome"] = do.OUTCOME_CANNOT_REPRODUCE
        stable["recommendation"] = {"action": "de-list", "advisory": True,
                                    "detail": "nothing reproduced"}
        try:
            probe_census.record_outcome(path, PROBE, stable)
        except probe_census.CensusError:
            pass
        else:
            FAILURES.append("a stable outcome was recorded carrying an issue")
        expect(not stored_outcomes(path),
               "and nothing was stored by that refusal either")


def test_a_recorded_defect_resumes_after_its_artifacts_are_pruned() -> None:
    """The durable record outlives the evidence it was built from.

    Retained artifacts live in the harness's tree outside every worktree
    and are swept like any other scratch. A resume that re-collected
    evidence would fail on exactly the thing the census record exists to
    make unnecessary, so completion is checked before anything is
    rendered at all.
    """
    document = defect_handoff()
    with census_file() as path:
        with staged_evidence(document):
            first, _publication, _probe, _pr = file_defect(document, path)
        # The artifact tree is gone now, and `collect_evidence` would
        # refuse over it. The completed attempt must not care.
        before = Path(path).read_bytes()
        again, publication, probe_spy, pr_spy = file_defect(
            document, path, origin="codex", now="2026-09-01T08:00:00Z")
        expect(again.resumed is True and again.record == first.record,
               f"a recorded attempt resumes on the record alone; got "
               f"{again.to_document()}")
        expect(publication.finds == [] and publication.creates == [],
               "without reaching the tracker")
        expect(Path(path).read_bytes() == before
               and len(stored_outcomes(path)) == 1,
               "and without touching the census")
        expect(probe_spy.calls == [] and pr_spy.calls == [],
               "or any forbidden boundary")


def test_a_crash_window_retry_recovers_after_the_artifacts_are_swept()\
        -> None:
    """The recovery path must not depend on the evidence either.

    Issue creation took effect, the census refused, and the artifact
    tree was swept before anyone retried. The issue is durable and the
    publication key is on it, so the retry has everything it needs — but
    only if the reconcile runs BEFORE the body is rendered. Rendering
    first would refuse for want of evidence and strand an issue that
    already exists.
    """
    document = defect_handoff()
    publication = FakePublication()
    with census_file() as path:
        with staged_evidence(document):
            healthy = json.loads(Path(path).read_text(encoding="utf-8"))
            Path(path).write_text(
                json.dumps(dict(healthy, schema=probe_census.SEED_SCHEMA)),
                encoding="utf-8")
            expect_not_filed(
                lambda: file_defect(document, path,
                                    publication=publication),
                "exists", "a census that refused after the issue was created")
            expect(len(publication.creates) == 1,
                   "the issue was created before the census refused")

        # The artifacts are gone now, and the census works again.
        Path(path).write_text(json.dumps(healthy), encoding="utf-8")
        published, _pub, probe_spy, pr_spy = file_defect(
            document, path, publication=publication)
        expect(published.reconciled is True and published.created is False,
               f"the retry reconciles the issue that already exists; got "
               f"{published.to_document()}")
        expect(len(publication.creates) == 1,
               f"creating nothing; got {len(publication.creates)} creation(s)")
        recorded = stored_outcomes(path)
        expect(len(recorded) == 1
               and recorded[0]["issue"]["number"] == 901,
               f"and records it exactly once; got {recorded}")
        expect(probe_spy.calls == [] and pr_spy.calls == [],
               "with neither forbidden boundary reached")


def test_a_reconciled_issue_supplies_its_own_review_brand() -> None:
    """The brand is the ISSUE's, not the resuming invocation's.

    A Claude-origin creation whose census write failed, resumed under a
    Codex invocation, still routes to Claude's opposite brand. Recording
    the retry's own brand would put a second, false answer in the
    durable history — and it is the answer the review gate acts on.
    """
    document = defect_handoff()
    key = defect_key(document)
    with staged_evidence(document), census_file() as path:
        publication = FakePublication()
        healthy = json.loads(Path(path).read_text(encoding="utf-8"))
        Path(path).write_text(
            json.dumps(dict(healthy, schema=probe_census.SEED_SCHEMA)),
            encoding="utf-8")
        expect_not_filed(
            lambda: file_defect(document, path, publication=publication,
                                origin="claude"),
            "exists", "a census that refused after the issue was created")

        Path(path).write_text(json.dumps(healthy), encoding="utf-8")
        published, _pub, _probe, _pr = file_defect(
            document, path, publication=publication, origin="codex")
        expect(published.reconciled is True
               and published.record["issue"]["origin"] == "claude",
               f"the reconciled issue's own brand is recorded; got "
               f"{published.record['issue']}")

    # An issue carrying the key but no readable origin marker is not one
    # this workflow filed, so it is a publication failure rather than
    # something to record under the caller's guess.
    with staged_evidence(document), census_file() as path:
        before = Path(path).read_bytes()
        publication = FakePublication(issues={key: {
            "number": 77,
            "url": "https://github.com/coghex/synarchy/issues/77",
            "body": f"someone else's issue\n{di.key_marker(key)}\n"}})
        expect_not_filed(
            lambda: file_defect(document, path, publication=publication),
            di.ORIGIN_MARKER,
            "a reconciled issue with no origin marker")
        expect_nothing_published(path, before, publication,
                                 "an unbranded reconcile")


def test_a_key_quoted_inside_a_code_fence_is_not_a_reconcile() -> None:
    """A filed issue QUOTES engine logs, and a log can say anything.

    So the marker has to be a standalone line outside every fence: a
    duplicate report that pasted this body into a code block would
    otherwise be reconciled as the publication, and the real defect
    would never be filed.
    """
    document = defect_handoff()
    key = defect_key(document)
    marker = di.key_marker(key)
    expect(di.carries_key(f"prose\n{marker}\nmore", key),
           "a standalone marker line is what a filed issue carries")
    expect(not di.carries_key(f"```\n{marker}\n```\n", key),
           "one inside a fence is a quotation of some other issue")
    expect(not di.carries_key(f"see {marker} above", key),
           "and one embedded in a sentence is not a marker line at all")
    expect(di.carries_key(f"````\nlog\n````\n{marker}\n", key),
           "a longer fence closes, so what follows it is read again")
    expect(not di.carries_key(f"```\nlog\n{marker}\n", key),
           "while an unterminated fence swallows the rest, which is the "
           "safe direction")
    expect(di.body_origin(f"```\n{di.origin_marker('codex')}\n```\n") is None
           and di.body_origin(di.origin_marker("codex")) == "codex",
           "and the origin marker is read under the same rule")

    with staged_evidence(document), census_file() as path:
        before = Path(path).read_bytes()
        quoted = FakePublication(issues={key: {
            "number": 88,
            "url": "https://github.com/coghex/synarchy/issues/88",
            "body": f"a duplicate report:\n\n```\n{marker}\n```\n"}})
        expect_not_filed(
            lambda: file_defect(document, path, publication=quoted),
            "carries no", "an issue that only quotes the key in a fence")
        expect_nothing_published(path, before, quoted,
                                 "a quoted-key reconcile")


def test_the_diagnosis_prose_is_bounded_at_the_gate() -> None:
    """#1437 bounds neither the summary nor the evidence list; this does.

    Refused rather than trimmed: the summary is the issue's own claim
    and the evidence is what makes it reviewable, so a body that cut
    either down would publish a defect report whose claim had been
    edited by the publisher.
    """
    cases = (
        ("a summary longer than a body",
         {"summary": "z" * (di.MAX_DIAGNOSIS_SUMMARY + 1),
          "evidence": ["run 1's log"], "category": None},
         "`summary` is"),
        ("more evidence lines than a body carries",
         {"summary": "the world thread raced",
          "evidence": ["line"] * (di.MAX_DIAGNOSIS_EVIDENCE + 1),
          "category": None},
         "evidence lines, over the"),
        ("one evidence line longer than a body carries",
         {"summary": "the world thread raced",
          "evidence": ["z" * (di.MAX_DIAGNOSIS_EVIDENCE_ITEM + 1)],
          "category": None},
         "evidence line 1 is"),
    )
    for label, block, fragment in cases:
        document = defect_handoff(diagnosis=block)
        with census_file() as path:
            before = Path(path).read_bytes()
            publication = FakePublication()
            try:
                file_defect(document, path, publication=publication)
            except di.HandoffError as error:
                expect(fragment in str(error),
                       f"{label}: rejected, but for {str(error)!r} rather "
                       f"than {fragment!r}")
            except di.NonSuccess as error:
                FAILURES.append(f"{label}: refused the EVIDENCE ({error}) "
                                f"where the input should have been rejected")
            else:
                FAILURES.append(f"{label}: accepted")
            expect_nothing_published(path, before, publication, label,
                                     searched=0)
    # And a body that still cannot fit refuses rather than publishing one
    # with its measurements or its log evidence sliced away.
    document = defect_handoff()
    saved = did.MAX_BODY_CHARS
    did.MAX_BODY_CHARS = 400
    try:
        with staged_evidence(document), census_file() as path:
            before = Path(path).read_bytes()
            publication = FakePublication()
            expect_not_filed(
                lambda: file_defect(document, path, publication=publication),
                "every part of it that is left is required",
                "a body no trimming can fit")
            expect_nothing_published(path, before, publication,
                                     "an unfittable body", searched=1)
    finally:
        did.MAX_BODY_CHARS = saved


def test_the_tail_read_is_whole_lines_and_tolerates_junk_bytes() -> None:
    """The two properties of the bounded tail read (#1438, #2157).

    Only `MAX_READ_BYTES` is read off the end of an engine log, so the
    first line of that window is whatever straddled the boundary — a
    FRAGMENT. Publishing it would quote half a line as this probe's
    failure evidence, so it is dropped and every quoted line is a whole
    one. And a macOS engine log carries GLFW's junk, so the window is
    decoded with `errors="replace"` rather than raising: evidence that
    exists must not be discarded because one byte is not UTF-8.

    Driven through `run_excerpts` on a log LARGER than the read window,
    because neither property is reachable otherwise. `MAX_EXCERPT_LINES`
    and `MAX_EXCERPT_CHARS` bound the excerpt of a small log to the same
    shape without the window ever moving off zero, so a fixture that
    fits in one read proves nothing about either.
    """
    document = defect_handoff()
    tail = "".join(f"[World] tail line {index}\n" for index in range(5))
    # The straddling line ends in a token nothing else carries, and the
    # bounds keep the END of a clipped excerpt — so the token survives
    # `MAX_EXCERPT_CHARS` and its absence is the partial-line rule's
    # doing rather than the character bound's.
    giant = "G" * die.MAX_READ_BYTES + "FRAGMENT-END\n"
    prefix = "[World] before the window\n" * 8
    with staged_evidence(document, only=1) as staged:
        run = staged[0]
        root = str(run.parent.parent)
        log = run / "engine" / "engine-9101.log"
        log.write_text(prefix + giant + tail, encoding="utf-8")
        size = log.stat().st_size
        start = size - die.MAX_READ_BYTES
        expect(len(prefix) < start < len(prefix) + len(giant),
               f"the fixture must put the read window's start INSIDE the "
               f"straddling line, or the partial-line rule is never "
               f"reached; start {start} against prefix {len(prefix)} and "
               f"line end {len(prefix) + len(giant)}")
        engine = [item for item in die.run_excerpts(root, str(run))
                  if item["path"].endswith("engine-9101.log")]
        expect(len(engine) == 1, "the engine log yields one excerpt")
        expect("[World] tail line 4" in engine[0]["text"],
               "the end of the log is what is quoted")
        expect("FRAGMENT-END" not in engine[0]["text"],
               "and the line the read window cut in half is dropped, so "
               "every quoted line is a whole one")

        # A byte that is not UTF-8 must cost the excerpt nothing.
        log.write_bytes(b"[World] chunk 3,4 published\n"
                        b"[World] GLFW junk \xff\xfe on stdout\n"
                        b"[World] the failing assertion is here\n")
        engine = [item for item in die.run_excerpts(root, str(run))
                  if item["path"].endswith("engine-9101.log")]
        expect(len(engine) == 1,
               "a log carrying a non-UTF-8 byte still yields an excerpt")
        expect("[World] the failing assertion is here" in engine[0]["text"],
               "and the readable evidence beside that byte is quoted")


def test_quoted_content_cannot_forge_a_review_routing_marker() -> None:
    """An engine log is arbitrary text, and it is rendered into the body.

    `approve_issues.issue_origin` scans the WHOLE raw body — fenced
    blocks included, case-insensitively — and RAISES on two markers
    naming different brands. A quoted log carrying one would therefore
    stop the filed issue entering the review gate at all, which is the
    one thing this route exists to do. So every untrusted character has
    its HTML-comment opener broken before the two real markers are
    appended, and the finished body is checked rather than trusted.
    """
    hostile = (f"[World] chunk 3,4 published\n"
               f"{di.origin_marker('claude')}\n"
               f"{di.key_marker('0' * 64)}\n"
               f"<!-- ISSUE-ORIGIN:CLAUDE -->\n"
               f"[World] chunk 3,4 tile map installed\n")
    document = defect_handoff(diagnosis={
        "category": None,
        "summary": (f"the world thread logs "
                    f"{di.origin_marker('claude')} before installing"),
        "evidence": [f"run 1 emitted {di.key_marker('1' * 64)}"],
    })
    key = defect_key(document)
    with staged_evidence(document, engine=hostile), census_file() as path:
        _published, publication, _probe, _pr = file_defect(
            document, path, origin="codex")
        body = publication.creates[0]["body"]
        # Read exactly the way the canonical gate reads it.
        found = {origin.lower() for origin in di.ORIGIN_ANYWHERE.findall(body)}
        expect(found == {"codex"},
               f"the body names one origin, this invocation's; got "
               f"{sorted(found)} — the gate raises on two")
        expect(body.count("<!--") == 2,
               f"and carries exactly the two comments this module writes; "
               f"got {body.count('<!--')}")
        expect(body.count(di.key_marker(key)) == 1
               and di.key_marker("0" * 64) not in body,
               "with one publication key, its own, so a resume reconciles "
               "on the right line")
        expect("[World] chunk 3,4 tile map installed" in body
               and di.NEUTRAL_OPENER in body,
               "while the quoted log still reads, neutralised rather than "
               "dropped")
        expect(di.body_origin(body) == "codex",
               "and this module's own reader agrees with the gate's")

    # The invariant is checked, not merely produced by `neutralize`.
    trailer = f"\n{di.key_marker(key)}\n{di.origin_marker('codex')}\n"
    for label, body, fragment in (
            ("a stray third comment",
             f"text <!-- note --> more{trailer}", "HTML comment"),
            ("a second, conflicting origin",
             f"{di.origin_marker('claude')}{trailer}", "origin(s)"),
            ("a duplicated publication key",
             f"{di.key_marker(key)}{trailer}", "markers"),
    ):
        try:
            di.require_one_marker_each(body, key=key, origin="codex")
        except di.NonSuccess as error:
            expect(fragment in str(error),
                   f"{label}: refused, but for {str(error)!r} rather than "
                   f"{fragment!r}")
        else:
            FAILURES.append(f"{label}: accepted")
    di.require_one_marker_each(f"clean body{trailer}", key=key,
                               origin="codex")


def test_the_defect_command_line_reports_each_ending() -> None:
    """The endings this workflow has, through the shipped entry point."""
    tool = str(Path(__file__).resolve().parent / "deflake_issue.py")
    document = defect_handoff()
    with staged_evidence(document), census_file() as path:
        root = Path(path).parent
        accepted = root / "defect.json"
        accepted.write_text(json.dumps(document), encoding="utf-8")

        before = Path(path).read_bytes()
        done = subprocess.run(
            [sys.executable, tool, "--handoff", str(accepted),
             "--census", str(path), "--dry-run", "--json",
             "--origin", "claude"],
            capture_output=True, text=True, timeout=120)
        expect(done.returncode == di.EXIT_OK,
               f"a dry run exits 0; got {done.returncode} "
               f"({done.stderr.strip()[:200]})")
        try:
            rendered = json.loads(done.stdout)
        except json.JSONDecodeError:
            rendered = {}
        expect(rendered.get("published") is False
               and DEFECT_DIAGNOSIS["summary"] in rendered.get("body", ""),
               f"rendering the issue without filing it; got "
               f"{done.stdout[:200]}")
        expect(Path(path).read_bytes() == before
               and not stored_outcomes(path),
               "and recording nothing")

        for extra in ([], ["--dry-run"]):
            done = subprocess.run(
                [sys.executable, tool, "--handoff", str(accepted),
                 "--census", str(path), *extra],
                capture_output=True, text=True, timeout=120)
            expect(done.returncode == di.EXIT_REJECTED
                   and "issue-origin" in done.stderr,
                   f"a run with no origin{' (dry)' if extra else ''} is "
                   f"rejected naming the marker; got {done.returncode} "
                   f"({done.stderr.strip()[:200]})")

        sibling = root / "sibling.json"
        sibling.write_text(json.dumps(outcome_handoff()), encoding="utf-8")
        done = subprocess.run(
            [sys.executable, tool, "--handoff", str(sibling),
             "--census", str(path), "--origin", "claude"],
            capture_output=True, text=True, timeout=120)
        expect(done.returncode == di.EXIT_NON_SUCCESS
               and "#1439" in done.stderr,
               f"a sibling route exits 3 naming its owner; got "
               f"{done.returncode} ({done.stderr.strip()[:200]})")

        malformed = root / "malformed.json"
        malformed.write_text(json.dumps({"schema": "nope"}), encoding="utf-8")
        done = subprocess.run(
            [sys.executable, tool, "--handoff", str(malformed),
             "--census", str(path), "--origin", "claude"],
            capture_output=True, text=True, timeout=120)
        expect(done.returncode == di.EXIT_REJECTED,
               f"a malformed handoff exits 2; got {done.returncode}")
        expect("Traceback" not in done.stderr,
               f"and never as a traceback\n{done.stderr[:400]}")
        expect(Path(path).read_bytes() == before,
               "and none of those endings touched the census")


def test_the_handoff_facade_exports_the_canonical_objects() -> None:
    """#2180: the façade binds its owners' objects, it does not copy them.

    `tools/deflake_handoff.py` is a re-export façade over four internal
    owners, so every name a consumer reads through it has to be the ONE
    object its owner defines. A copied alias would be a second
    definition free to drift: `except deflake_outcome.HandoffError`
    would stop catching what `deflake_handoff` raises, and an
    `isinstance` against either `Measurement` would answer differently
    depending on which module the caller imported.

    Asserted here rather than left to inspection because #2097's
    compatibility bindings in `deflake_outcome.py` are what the rest of
    the repository imports, and nothing else executes the claim.
    """
    for name in ("HandoffError", "NonSuccess", "Measurement", "Handoff",
                 "RouteOwnership"):
        expect(getattr(do, name) is getattr(deflake_handoff, name),
               f"deflake_outcome.{name} must BE deflake_handoff.{name}, "
               f"not a copy")
    for name in deflake_handoff.__all__:
        expect(hasattr(deflake_handoff, name),
               f"the façade declares {name} in __all__ but does not bind it")
    owners = ("deflake_handoff_grammar", "deflake_handoff_measurement",
              "deflake_handoff_producer", "deflake_handoff_assembly")
    modules = {name: importlib.import_module(name) for name in owners}
    for name in deflake_handoff.__all__:
        bound = getattr(deflake_handoff, name)
        defining = [module for module in modules.values()
                    if getattr(module, name, None) is bound]
        expect(defining,
               f"{name} is on the façade but no internal owner defines it")


def test_the_handoff_owners_stay_one_way() -> None:
    """#2180: the four owners form an acyclic chain, and nothing above it.

    The whole point of extracting them is that grammar, measurement,
    producer binding and assembly can change independently. A back-edge
    would restore exactly the entanglement #2097 removed: an owner that
    imported the façade would be importing its own siblings through a
    module whose only job is to re-export them, and an owner that
    imported either consumer would make the two consumers each other's
    prerequisite again.

    The reverse reference `require_reproduced` needs — its `Handoff`
    annotation — is pinned rather than excused: it must sit inside a
    `TYPE_CHECKING` guard, where it is evaluated by a type checker and
    never at run time, so the runtime graph stays one-way.
    """
    order = ["deflake_handoff_grammar", "deflake_handoff_measurement",
             "deflake_handoff_producer", "deflake_handoff_assembly"]
    forbidden = {"deflake_handoff", "deflake_outcome", "deflake_issue"}
    directory = Path(dd.__file__).resolve().parent
    for position, owner in enumerate(order):
        source = (directory / f"{owner}.py").read_text(encoding="utf-8")
        tree = ast.parse(source)
        guarded, runtime = set(), set()
        for node in ast.walk(tree):
            if not isinstance(node, (ast.Import, ast.ImportFrom)):
                continue
            names = ({alias.name for alias in node.names}
                     if isinstance(node, ast.Import) else {node.module or ""})
            runtime |= names
        for node in ast.walk(tree):
            if not isinstance(node, ast.If):
                continue
            test = node.test
            if not (isinstance(test, ast.Name) and test.id == "TYPE_CHECKING"):
                continue
            for inner in ast.walk(node):
                if isinstance(inner, ast.Import):
                    guarded |= {alias.name for alias in inner.names}
                elif isinstance(inner, ast.ImportFrom):
                    guarded.add(inner.module or "")
        runtime -= guarded
        for name in sorted(runtime & forbidden):
            expect(False,
                   f"{owner} imports {name}; an internal owner depends on "
                   f"neither the façade nor either consumer")
        for name in sorted(guarded & forbidden):
            expect(False,
                   f"{owner} type-imports {name}; the façade and the "
                   f"consumers are off-limits even under TYPE_CHECKING")
        later = set(order[position + 1:])
        for name in sorted(runtime & later):
            expect(False,
                   f"{owner} imports {name} at run time, which is later in "
                   f"the one-way order {' -> '.join(order)}")
        for name in sorted(guarded & later):
            expect(name == "deflake_handoff_assembly"
                   and owner == "deflake_handoff_measurement",
                   f"{owner} type-imports {name}; the only permitted "
                   f"reverse reference is require_reproduced's Handoff "
                   f"annotation")


def test_the_handoff_family_imports_as_repository_modules() -> None:
    """#2180: every owner resolves under the `tools.` package spelling too.

    `tools/` carries no `__init__.py`, so it is an implicit namespace
    package: `import tools.deflake_handoff` from the repository root is
    a supported spelling, and under it the directory holding these
    modules is NOT on `sys.path`. Sibling imports by bare name resolve
    anyway only because each module inserts its own directory first —
    which the pre-split `deflake_handoff.py` did before importing
    `deflake_diagnosis`, and which the façade must keep doing before the
    first of its re-exports, since those run at import time.

    Asserted for the whole family rather than the façade alone because
    the same bootstrap is what makes each owner importable on its own,
    and a new owner added without one would fail the same way.
    """
    root = Path(dd.__file__).resolve().parent.parent
    family = ("tools.deflake_handoff", "tools.deflake_handoff_grammar",
              "tools.deflake_handoff_measurement",
              "tools.deflake_handoff_producer",
              "tools.deflake_handoff_assembly",
              "tools.deflake_outcome", "tools.deflake_issue")
    environment = dict(os.environ)
    environment.pop("PYTHONPATH", None)
    for module in family:
        done = subprocess.run(
            [sys.executable, "-c", f"import {module}"],
            cwd=str(root), capture_output=True, text=True, timeout=120,
            env=environment)
        expect(done.returncode == 0,
               f"`import {module}` from the repository root must resolve; "
               f"exited {done.returncode}\n{done.stderr[-400:]}")


def issue_family_dependencies(source: str) -> set:
    """Every `tools/` module one owner of the issue family depends on.

    An import-node scan is not enough here. The family resolves each
    dependency with `_sibling("<name>")` so that the `tools.` and bare
    spellings of a module are the SAME object (#2157), and a scan that
    only walked `ast.Import` would see no edges at all — passing the
    acyclicity and no-implementation cases vacuously while a back-edge
    sat in plain sight. So the literal argument of every `_sibling` call
    counts as a dependency, exactly as an import would.
    """
    found = set()
    for node in ast.walk(ast.parse(source)):
        if isinstance(node, ast.Import):
            found |= {alias.name for alias in node.names}
        elif isinstance(node, ast.ImportFrom):
            found.add(node.module or "")
        elif (isinstance(node, ast.Call)
              and isinstance(node.func, ast.Name)
              and node.func.id == "_sibling"
              and node.args
              and isinstance(node.args[0], ast.Constant)
              and isinstance(node.args[0].value, str)):
            found.add(node.args[0].value)
    return found


def test_the_issue_facade_exports_the_canonical_objects() -> None:
    """#2157: the façade binds its owners' objects, it does not copy them.

    `tools/deflake_issue.py` is the route's public import façade over
    four owners, so every name a consumer reads through it has to be the
    ONE object its owner defines. Two of them are load-bearing by name:
    `PublicationFailed` is caught with `except` against the façade
    spelling while the tracker raises the owner's, so a second class
    definition would silently stop matching; and `Publication` is
    SUBCLASSED by this file's own `FakePublication`, so the façade name
    must be the interface `GitHubPublication` implements.

    `MAX_BODY_CHARS` is asserted ABSENT for the opposite reason. It is
    the one constant of the family a caller substitutes, `issue_body`
    reads it out of the document owner's globals, and a façade binding
    would take the assignment and change nothing — leaving the
    unfittable-body refusal unexercised while the case appeared to drive
    it.
    """
    owners = ("deflake_issue_evidence", "deflake_issue_document",
              "deflake_issue_tracker", "deflake_issue_record")
    modules = {name: importlib.import_module(name) for name in owners}
    expect(di.PublicationFailed is modules["deflake_issue_tracker"]
           .PublicationFailed,
           "deflake_issue.PublicationFailed must BE the tracker owner's "
           "class, not a copy; `except` matches by identity")
    expect(di.Publication is modules["deflake_issue_tracker"].Publication,
           "deflake_issue.Publication must BE the tracker owner's "
           "interface; FakePublication subclasses it")
    expect(issubclass(modules["deflake_issue_tracker"].GitHubPublication,
                      di.Publication),
           "the gh-backed publisher must implement the interface the "
           "façade exports")
    defined_here = {"CHANGES_THE_PROBE", "Defect", "EXIT_NON_SUCCESS",
                    "EXIT_OK", "EXIT_REJECTED", "HANDOFF_SCHEMA",
                    "HandoffError", "NonSuccess", "OPENS_PULL_REQUEST",
                    "OUTCOME_PRODUCTION_DEFECT", "OWNED", "OWNER_ISSUE",
                    "ROLES", "ROLE_BASELINE", "ROLE_HANDOFF",
                    "ROLE_VERIFICATION", "ROUTE", "accept",
                    "forbidden_probe_change", "forbidden_pull_request",
                    "main", "publish", "render", "require_defect_diagnosis",
                    "require_handoff", "require_origin"}
    for name in di.__all__:
        expect(hasattr(di, name),
               f"the façade declares {name} in __all__ but does not bind it")
        if name in defined_here:
            continue
        bound = getattr(di, name)
        defining = [owner for owner, module in modules.items()
                    if getattr(module, name, None) is bound]
        expect(defining,
               f"{name} is re-exported by the façade but no owner defines "
               f"that exact object; a compatibility export must be the "
               f"canonical one")
    expect(not hasattr(di, "MAX_BODY_CHARS"),
           "MAX_BODY_CHARS must NOT be bound on the façade: it is the "
           "substituted constant, and an inert alias would swallow the "
           "assignment that exercises the unfittable-body refusal")
    for name in sorted(defined_here):
        expect(name in di.__all__,
               f"{name} is the façade's own and belongs in __all__")


def test_the_issue_owners_stay_one_way() -> None:
    """#2157: the four owners are acyclic, with one permitted sibling edge.

    The whole point of extracting them is that artifact traversal,
    issue rendering, the tracker boundary and the durable record can
    change independently. An owner that imported the façade would be
    importing its own siblings through a module whose other job is to
    orchestrate them, and an owner that imported either sibling
    consumer would make the two consumers each other's prerequisite.

    Exactly one edge between two extracted owners is permitted, and it
    is required rather than tolerated: the tracker CALLS the document
    owner's `carries_key` and `body_origin` instead of restating them,
    because a second spelling of the standalone-marker rule would let a
    search-index match be recorded as this attempt's publication.
    """
    family = {"deflake_issue_evidence", "deflake_issue_document",
              "deflake_issue_tracker", "deflake_issue_record"}
    permitted = {("deflake_issue_tracker", "deflake_issue_document")}
    forbidden = {"deflake_issue", "deflake_outcome"}
    directory = Path(dd.__file__).resolve().parent
    for owner in sorted(family):
        source = (directory / f"{owner}.py").read_text(encoding="utf-8")
        imported = issue_family_dependencies(source)
        expect(imported,
               f"{owner} reads as depending on nothing at all, which means "
               f"the dependency scan has stopped seeing this family's "
               f"edges rather than that the file has none")
        for name in sorted(imported & forbidden):
            expect(False,
                   f"{owner} imports {name}; an extracted owner depends on "
                   f"neither the façade nor the sibling consumer")
        for name in sorted((imported & family) - {owner}):
            expect((owner, name) in permitted,
                   f"{owner} imports {name}; the only permitted edge "
                   f"between two owners is tracker -> document")
    tracker = importlib.import_module("deflake_issue_tracker")
    document = importlib.import_module("deflake_issue_document")
    for name in ("carries_key", "body_origin"):
        expect(getattr(tracker, name) is getattr(document, name),
               f"the tracker must CALL the document owner's {name}, not "
               f"carry a second copy of the standalone-marker rule")


def test_the_issue_facade_keeps_only_what_it_composes() -> None:
    """#2157: the extracted implementations have one home each.

    The façade keeps route admission, `render`'s composition across
    three owners, `publish`'s exact statement order and the command
    line. What it must NOT still carry is a second copy of anything
    extracted — a walker, a body renderer, a `gh` adapter or a census
    record builder left behind would be a definition free to drift from
    the one its owner exports, and the compatibility bindings above
    would go on resolving to the owner while the façade's own callers
    used the stale twin.
    """
    directory = Path(dd.__file__).resolve().parent
    facade = ast.parse((directory / "deflake_issue.py")
                       .read_text(encoding="utf-8"))
    defined = {node.name for node in facade.body
               if isinstance(node, (ast.FunctionDef, ast.ClassDef))}
    moved = {
        "open_run_directory": "deflake_issue_evidence",
        "run_excerpts": "deflake_issue_evidence",
        "collect_evidence": "deflake_issue_evidence",
        "failing_runs": "deflake_issue_evidence",
        "excerpt": "deflake_issue_evidence",
        "issue_body": "deflake_issue_document",
        "issue_title": "deflake_issue_document",
        "publication_key": "deflake_issue_document",
        "neutralize": "deflake_issue_document",
        "require_one_marker_each": "deflake_issue_document",
        "carries_key": "deflake_issue_document",
        "body_origin": "deflake_issue_document",
        "prose_lines": "deflake_issue_document",
        "GitHubPublication": "deflake_issue_tracker",
        "Publication": "deflake_issue_tracker",
        "PublicationFailed": "deflake_issue_tracker",
        "require_issue_identity": "deflake_issue_tracker",
        "require_reconciled_issue": "deflake_issue_tracker",
        "outcome_record": "deflake_issue_record",
        "stored_record": "deflake_issue_record",
        "reuse_stored_publication": "deflake_issue_record",
        "require_supported": "deflake_issue_record",
        "Published": "deflake_issue_record",
    }
    for name, owner in sorted(moved.items()):
        expect(name not in defined,
               f"the façade still defines {name}; it belongs to {owner} and "
               f"a second definition is free to drift from it")
    for name in ("require_defect_diagnosis", "require_origin",
                 "require_handoff", "accept", "render", "publish", "main",
                 "forbidden_probe_change", "forbidden_pull_request"):
        expect(name in defined,
               f"the façade must still define {name}: route admission, "
               f"composition, ordering and the CLI stay here")
    # `subprocess` and `tempfile` are the `gh` adapter's, `stat` the
    # walker's, and `hashlib` the publication key's. None of the four
    # has a caller left on the façade, and an import of one is the first
    # sign an implementation came back.
    imported = issue_family_dependencies(
        (directory / "deflake_issue.py").read_text(encoding="utf-8"))
    for name in ("deflake_issue_document", "deflake_issue_evidence",
                 "deflake_issue_record", "deflake_issue_tracker"):
        expect(name in imported,
               f"the façade must compose {name}; a scan that cannot see "
               f"that edge cannot see a stale implementation either")
    for name in ("subprocess", "tempfile", "stat", "hashlib", "probe_flake",
                 "probe_protocol", "probe_runner_registry"):
        expect(name not in imported,
               f"the façade imports {name}, which only an extracted "
               f"implementation needs")


def test_the_issue_family_is_one_module_under_either_spelling() -> None:
    """#2157: `tools.<name>` and the bare name must not be two modules.

    `tools/` is an implicit namespace package, so every file in it has
    two import spellings and Python treats them as different modules. A
    façade loaded as `tools.deflake_issue` that resolved its owners by
    BARE name would therefore load a second copy of each, and every
    guarantee this split rests on would be false in that process:
    `tools.deflake_issue.issue_body is not
    tools.deflake_issue_document.issue_body`, `except
    tools.deflake_issue.PublicationFailed` would stop catching what
    `tools.deflake_issue_tracker` raises, and lowering
    `tools.deflake_issue_document.MAX_BODY_CHARS` would leave the module
    that actually renders untouched.

    Asserted in ONE fresh interpreter per spelling, because that is the
    only place the defect exists — each module imports fine on its own,
    and the compatibility cases above run under the bare spelling where
    a bare-name resolution looks correct.
    """
    root = Path(dd.__file__).resolve().parent.parent
    environment = dict(os.environ)
    environment.pop("PYTHONPATH", None)
    programs = {
        "the tools. spelling": """
import sys
import tools.deflake_issue_document as document
import tools.deflake_issue_evidence as evidence
import tools.deflake_issue_record as record
import tools.deflake_issue_tracker as tracker
import tools.deflake_issue as facade
assert facade.issue_body is document.issue_body, "issue_body"
assert facade.publication_key is document.publication_key, "publication_key"
assert facade.PublicationFailed is tracker.PublicationFailed, "PublicationFailed"
assert facade.Publication is tracker.Publication, "Publication"
assert facade.run_excerpts is evidence.run_excerpts, "run_excerpts"
assert facade.Published is record.Published, "Published"
assert tracker.carries_key is document.carries_key, "tracker->document"
stray = sorted(name for name in sys.modules
               if name.startswith("deflake_issue"))
assert not stray, f"bare copies loaded beside the package ones: {stray}"
""",
        "the bare spelling": """
import sys
sys.path.insert(0, "tools")
import deflake_issue_document as document
import deflake_issue_tracker as tracker
import deflake_issue as facade
assert facade.issue_body is document.issue_body, "issue_body"
assert facade.PublicationFailed is tracker.PublicationFailed, "PublicationFailed"
stray = sorted(name for name in sys.modules
               if name.startswith("tools.deflake_issue"))
assert not stray, f"package copies loaded beside the bare ones: {stray}"
""",
    }
    for label, program in programs.items():
        done = subprocess.run([sys.executable, "-c", program], cwd=str(root),
                              capture_output=True, text=True, timeout=120,
                              env=environment)
        expect(done.returncode == 0,
               f"under {label} the façade and its owners must be ONE set of "
               f"modules; exited {done.returncode}\n{done.stderr[-400:]}")

    # The substituted constant is the case that matters most, since a
    # duplicated document owner would leave the gate's own mutation seam
    # pointing at a module nothing renders through.
    seam = """
import tools.deflake_issue_document as document
import tools.deflake_issue as facade
document.MAX_BODY_CHARS = 400
source = facade.issue_body.__globals__["MAX_BODY_CHARS"]
assert source == 400, f"the renderer still reads {source}"
"""
    done = subprocess.run([sys.executable, "-c", seam], cwd=str(root),
                          capture_output=True, text=True, timeout=120,
                          env=environment)
    expect(done.returncode == 0,
           f"lowering `tools.deflake_issue_document.MAX_BODY_CHARS` must "
           f"reach the `issue_body` the façade composes; exited "
           f"{done.returncode}\n{done.stderr[-400:]}")


def test_the_issue_family_imports_as_repository_modules() -> None:
    """#2157: every owner resolves under the `tools.` package spelling too.

    `tools/` carries no `__init__.py`, so it is an implicit namespace
    package: `import tools.deflake_issue` from the repository root is a
    supported spelling, and under it the directory holding these modules
    is NOT on `sys.path`. Sibling imports by bare name resolve anyway
    only because each module inserts its own directory first — which the
    pre-split `deflake_issue.py` did, and which the façade must keep
    doing before the first of its re-exports, since those run at import
    time.
    """
    root = Path(dd.__file__).resolve().parent.parent
    family = ("tools.deflake_issue", "tools.deflake_issue_evidence",
              "tools.deflake_issue_document", "tools.deflake_issue_tracker",
              "tools.deflake_issue_record")
    environment = dict(os.environ)
    environment.pop("PYTHONPATH", None)
    for module in family:
        done = subprocess.run(
            [sys.executable, "-c", f"import {module}"],
            cwd=str(root), capture_output=True, text=True, timeout=120,
            env=environment)
        expect(done.returncode == 0,
               f"`import {module}` from the repository root must resolve; "
               f"exited {done.returncode}\n{done.stderr[-400:]}")


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
