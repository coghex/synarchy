#!/usr/bin/env python3
"""Shared fixtures for the `/deflake` deterministic gate (#2031).

The single source of everything the three gate owners --
`deflake_diagnosis_selftest_diagnosis`,
`deflake_diagnosis_selftest_outcome` and
`deflake_diagnosis_selftest_issue` -- hold in common: the assertion
helpers and the ONE failure accumulator behind them, the
probe/commit/worktree constants every fixture is spelled in, the real
`probe_flake.Measurement` builders that make a result document one the
harness would actually have written, and the handoff, batch and
diagnosis documents the routes are evaluated from.

Nothing here belongs to one workflow. A helper used by exactly one
owner stays with that owner — `check_mutation` and its private
`mutant` compiler stay in the diagnosis unit, and the outcome unit
keeps the forgery fixtures only its own cases read — because promoting
a single-owner helper here would make this module the place unrelated
changes converge on, which is the shape #2031 exists to undo.

The second half is the outcome-shaped support the #1438 publication
cases also read: an outcome record is what a defect handoff is built
out of, so `outcome_handoff`, `record_outcome`, `census_file` and the
identity constants around them are genuinely shared rather than
borrowed across a seam. They live here once instead of being imported
sideways from the outcome unit or copied into the issue one.

`FAILURES` is ONE list rather than per-owner state, and since #1922 it
is `tools/selftestlib.py`'s own: `expect` appends to it, the facade
reads exactly it, and one focused run and one aggregate run report
through the same accumulator. Re-exported from here so the owners
import the helper -- and the quiet-by-default `expect` behind it --
from the single place they already import everything else shared from.
Every refusal that used to append to that list directly now goes
through `selftestlib.record_fail`, so nothing registers a failure
behind the assertion tally `concluded` refuses a vacuous run on.

Not a gate of its own. Run through the facade:

  python3 tools/test_deflake_diagnosis.py
"""
from __future__ import annotations

import contextlib
import copy
import json
import shutil
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import deflake  # type: ignore  # noqa: E402
import deflake_diagnosis as dd  # type: ignore  # noqa: E402
import deflake_outcome as do  # type: ignore  # noqa: E402
import probe_census  # type: ignore  # noqa: E402
import probe_flake  # type: ignore  # noqa: E402
import probe_protocol  # type: ignore  # noqa: E402
import probe_runner_resources  # type: ignore  # noqa: E402
import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402

TOOL = str(Path(__file__).resolve().parent / "deflake_diagnosis.py")


# `deflake.build_handoff` and this module's own controlled records spell
# one setting differently; the entry gate adapts at the boundary and the
# fixtures have to know which side they are on.
PRODUCER_FIELD = {"timeout_seconds": "timeout"}


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
        selftestlib.record_fail(
            f"{msg}: refused the ROUTE ({error}) where the entry "
            f"gate should have rejected the input")
        return
    selftestlib.record_fail(f"{msg}: accepted")


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
        selftestlib.record_fail(
            f"{msg}: rejected the INPUT ({error}) where the "
            f"route should have been refused")
        return
    selftestlib.record_fail(f"{msg}: accepted")


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
# Outcome-shaped support, shared by #1439's own cases and #1438's
# ==========================================================================
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


_DEFAULT = object()


def spotless_result(**kwargs) -> dict:
    """A complete measurement in which nothing at all went wrong."""
    return result_document(**kwargs)


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
