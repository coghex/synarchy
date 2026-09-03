#!/usr/bin/env python3
"""What ONE measurement is, and whether it may be believed.

One of four internal owners behind `tools/deflake_handoff.py`, which
stays the stable public import façade for epic #1426's two consumers
(`tools/deflake_outcome.py` #1439 and `tools/deflake_issue.py` #1438).
Read that module's docstring for the contract narrative; the sections
"A document that exists establishes nothing", "The run count is the
document's, not a literal" and "The shared reproduction predicate" are
the rationale for what is below.

This owns the `Measurement` representation, the declared-measurement
gate `require_measurement`, and the shared reproduced-failure predicate
`require_reproduced`: trustworthiness from the harness exit, the
document's own status, the run/aggregate reconciliation, the per-check
and target-hit reading, the durable per-measurement summary, and the
binding of a declared measurement to its own exit contract.

It consumes the grammar owner and #1437's canonical result validators,
and NOTHING downstream of itself. `require_reproduced` keeps the exact
two-argument signature both consumers call it with — the `Handoff` it
takes is the assembly owner's class — and it reads only that object's
`targets` and `acceptable_failures`, so the reference is confined to a
deferred annotation under `from __future__ import annotations` behind a
`TYPE_CHECKING` guard. That guard is evaluated by type checkers and
never at run time, so the runtime import graph stays one-way:
assembly imports measurement, and measurement imports no owner but
grammar.

    python3 tools/test_deflake_diagnosis.py       # the deterministic gate
"""
from __future__ import annotations

import copy
import os
import sys
from typing import TYPE_CHECKING

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import deflake_diagnosis  # noqa: E402
import probe_census  # noqa: E402
import probe_flake  # noqa: E402
import probe_protocol  # noqa: E402
from deflake_handoff_grammar import (  # noqa: E402
    EXIT_CONTRACT,
    HandoffError,
    NonSuccess,
    ROLES,
    _require_object,
    require_artifact_reference,
)

if TYPE_CHECKING:  # pragma: no cover - resolved by type checkers only
    from deflake_handoff_assembly import Handoff


class Measurement:
    """One measurement the handoff hands on, with its harness exit.

    The exit code and the document are held TOGETHER because neither
    answers the question alone: exits 2 and 3 write no document, and
    exit 4 writes one that says so itself.
    """

    def __init__(self, role: str, exit_code: int, result):
        self.role = role
        self.exit_code = exit_code
        self.result = result

    # -- the document's own account of itself ------------------------------
    @property
    def requested_runs(self) -> int:
        return self.result["requested_runs"]

    @property
    def completed_runs(self) -> int:
        return self.result["completed_runs"]

    @property
    def failure_count(self) -> int:
        return self.result["failure_count"]

    @property
    def timeout_count(self) -> int:
        return self.result["timeout_count"]

    @property
    def check_counts(self) -> dict:
        return self.result["check_counts"]

    def trustworthiness_problems(self) -> list[str]:
        """Every reason this measurement cannot support a conclusion.

        The complete list rather than the first, because an operator
        reading a non-success needs to know what to fix, and a batch
        that both aborted early and lost a run has two problems.
        """
        if EXIT_CONTRACT[self.exit_code] != probe_census.ACCEPTED_STATUS:
            return [f"the {self.role} measurement exited {self.exit_code}, "
                    f"which is not a valid measurement"]
        problems = []
        if self.result.get("error_run") is not None:
            problems.append(
                f"the {self.role} measurement kept an error run, so one of "
                f"its runs produced a stream nobody can trust")
        if self.result.get("error") is not None:
            problems.append(
                f"the {self.role} measurement reports the harness error "
                f"{self.result['error']!r}")
        if self.completed_runs != self.requested_runs:
            problems.append(
                f"the {self.role} measurement completed "
                f"{self.completed_runs} of {self.requested_runs} requested "
                f"runs")
        problems += self.aggregate_problems()
        return problems

    def aggregate_problems(self) -> list[str]:
        """Every way this document's totals contradict its own run list.

        A document that disagrees with itself is one nobody can believe,
        so this belongs to trustworthiness rather than to any one
        route's predicate: an inconsistent aggregate must be refused
        before ANY stable outcome is reached, not only before
        `cannot-reproduce`.

        Nothing upstream establishes it. `probe_census.validate_result`'s
        cross-field rules bind `check_counts` to `runs` and refuse a
        PASS run carrying a FAIL check, but they say nothing about
        `failure_count`, `timeout_count` or `failure_rate` — so an
        all-PASS run list under a forged failure count is schema-valid,
        passes the census's own invariants, and would otherwise read as
        a reproduced failure.

        `completed_runs` is checked here for the same reason and is the
        one that makes the rest mean anything: the producer writes it as
        `len(runs)`, nothing on the way back in re-establishes that, and
        a nine-run batch claiming ten completed passes a completeness
        test of `completed_runs == requested_runs` and is then STORED as
        ten of ten.

        The arithmetic is `probe_flake.Measurement`'s own, not a second
        opinion: a failure is a FAIL or a TIMEOUT run, a timeout is a
        TIMEOUT run, and the rate is failures over REQUESTED runs
        rounded the way the producer rounds it.
        """
        problems: list[str] = []
        runs = self.result["runs"]
        if len(runs) != self.completed_runs:
            problems.append(
                f"the {self.role} measurement reports "
                f"{self.completed_runs} completed run(s) while its run list "
                f"holds {len(runs)}")
        failures = sum(1 for run in runs if run["outcome"] in
                       (probe_flake.RUN_FAIL, probe_flake.RUN_TIMEOUT))
        timeouts = sum(1 for run in runs
                       if run["outcome"] == probe_flake.RUN_TIMEOUT)
        if self.failure_count != failures:
            problems.append(
                f"the {self.role} measurement reports "
                f"{self.failure_count} failing run(s) while its run list "
                f"shows {failures}")
        if self.timeout_count != timeouts:
            problems.append(
                f"the {self.role} measurement reports "
                f"{self.timeout_count} timed-out run(s) while its run list "
                f"shows {timeouts}")
        rate = self.result["failure_rate"]
        expected = (round(self.failure_count / self.requested_runs, 6)
                    if self.requested_runs else None)
        if rate != expected:
            problems.append(
                f"the {self.role} measurement reports the failure rate "
                f"{rate!r}, not the {expected!r} its own counts imply")
        return problems

    def defect_problems(self) -> list[str]:
        """Everything a TRUSTWORTHY measurement observed going wrong.

        Its counterpart above answers "can this batch be believed"; this
        answers "did it show anything", and it asks the RUN LIST and the
        PER-CHECK TALLIES rather than the aggregates. Those two are
        genuinely independent — a run can time out after emitting every
        declared check, and a check can go MISSING across a batch whose
        every run PASSED — while the totals are the same fact counted,
        and `aggregate_problems` has already bound them to the run list.
        Reading a total here would be asking the derived value instead
        of its source, and would leave a clause no fixture can isolate.
        """
        problems: list[str] = []
        for run in self.result["runs"]:
            if run["outcome"] != probe_flake.RUN_PASS:
                problems.append(
                    f"the {self.role} measurement's run {run['index']} is "
                    f"{run['outcome']}, not {probe_flake.RUN_PASS}")
        for cid, tally in sorted(self.check_counts.items()):
            for state in (probe_protocol.FAIL, probe_protocol.MISSING):
                if tally.get(state):
                    problems.append(
                        f"the {self.role} measurement's check {cid!r} is "
                        f"{state} in {tally[state]} run(s)")
        return problems

    def missing_targets(self, targets) -> list[str]:
        """The target checks this measurement never observed at all.

        A reproducibly MISSING target is #1437's second qualification
        for a repair, so it is also the second way a batch can fail an
        acceptance gate while sitting at or below X.
        """
        return [cid for cid in targets
                if self.check_counts.get(cid, {}).get(probe_protocol.MISSING)]

    def summary(self) -> dict:
        """The durable summary this measurement contributes to the census.

        References, never copies. The per-run CHECK MAP, the ports, the
        artifact root and the invocation directory stay in the result
        document beside the retained artifacts; what is stored is the
        tally the outcome was judged on and the paths that hold the
        rest.
        """
        result = self.result
        error_run = result.get("error_run")
        return {
            "role": self.role,
            "exit_code": self.exit_code,
            "status": result["status"],
            "commit_sha": result["commit_sha"],
            "timestamp_utc": result["timestamp_utc"],
            "requested_runs": result["requested_runs"],
            "completed_runs": result["completed_runs"],
            "runs": [{"index": run["index"], "outcome": run["outcome"]}
                     for run in result["runs"]],
            "check_counts": copy.deepcopy(result["check_counts"]),
            "failure_count": result["failure_count"],
            "failure_rate": result["failure_rate"],
            "timeout_count": result["timeout_count"],
            "rts_capabilities": result["rts_capabilities"],
            "error": result["error"],
            "error_run_index": (None if error_run is None
                                else error_run["index"]),
            "retained_artifacts": list(result["retained_artifacts"]),
            "census_reference": {
                "cohort_commit_sha": result["commit_sha"],
                "sample_timestamp_utc": result["timestamp_utc"],
            },
        }


def require_measurement(entry, *, probe: str, seen: set,
                        worktrees=()) -> Measurement:
    """One declared measurement, bound to its own exit code."""
    section = _require_object(entry, "a declared measurement")
    role = section.get("role")
    if role not in ROLES:
        raise HandoffError(
            f"a declared measurement's role is {role!r}; the roles are "
            f"{', '.join(ROLES)}")
    if role in seen:
        raise HandoffError(
            f"the handoff declares the {role!r} measurement twice; one "
            f"attempt has one of each")
    exit_code = section.get("exit_code")
    if isinstance(exit_code, bool) or not isinstance(exit_code, int):
        raise HandoffError(
            f"the {role} measurement's `exit_code` must be an integer, got "
            f"{exit_code!r}")
    if exit_code not in EXIT_CONTRACT:
        raise HandoffError(
            f"the {role} measurement exited {exit_code}, which is not one of "
            f"tools/probe_flake.py's exits "
            f"({', '.join(str(code) for code in sorted(EXIT_CONTRACT))})")
    expected_status = EXIT_CONTRACT[exit_code]
    result = section.get("result")
    if expected_status is None:
        if result is not None:
            raise HandoffError(
                f"the {role} measurement exited {exit_code}, which is caught "
                f"before any document is rendered, so it wrote no result "
                f"document and one here did not come from that run")
        return Measurement(role, exit_code, None)
    if result is None:
        raise HandoffError(
            f"the {role} measurement exited {exit_code}, which writes a "
            f"result document, but the handoff carries none")
    # #1437's CANONICAL result gate, not just the census validator it
    # starts with. `probe_census.validate_result` owns declared shape
    # and #1493's cross-field invariants; `require_result` adds the
    # rules that make a document one `probe_flake.measure` could have
    # written — run indices, the artifact TOPOLOGY, and the retention
    # pairing in particular. `measure` deletes a run's directory the
    # moment it passes and keeps every unsuccessful one, so a non-PASS
    # run with a null `artifact_dir` is producer-impossible, and a
    # `no-confident-fix` recorded from one would be a failure nobody can
    # diagnose stored as the evidence for a diagnosis.
    try:
        deflake_diagnosis.require_result(
            result, f"the {role} measurement's result document")
    except deflake_diagnosis.HandoffError as error:
        raise HandoffError(str(error)) from None
    if result["probe"] != probe:
        raise HandoffError(
            f"the {role} measurement measured {result['probe']!r}, not the "
            f"diagnosed probe {probe!r}")
    if result["status"] != expected_status:
        raise HandoffError(
            f"the {role} measurement exited {exit_code} while its document "
            f"reports status {result['status']!r}, which that exit cannot "
            f"produce (expected {expected_status!r})")
    for path in result["retained_artifacts"]:
        require_artifact_reference(
            path, f"a retained artifact of the {role} measurement",
            worktrees=worktrees)
    for field in ("artifact_root", "invocation_dir"):
        require_artifact_reference(
            result[field], f"the {role} measurement's `{field}`",
            worktrees=worktrees)
    return Measurement(role, exit_code, result)


# ==========================================================================
# The shared reproduction predicate
# ==========================================================================

def require_reproduced(handoff: Handoff, baseline: Measurement) -> None:
    """The baseline reproduced the pattern the targets were named from.

    Shared because BOTH of epic #1426's consumers ask it: #1437 asks it
    of every route past the `cannot-reproduce` fork, and neither the
    `production-defect` route nor the two stable outcomes reached past
    that fork are exempt. A private copy in either consumer would be a
    second reading of one producer rule.

    #1437 asks it in two independent parts, and a consumer that asked
    only the first would persist a diagnosis its own producer refuses:

    * the batch is over X or lost a target, which is what makes it a
      reproduction at all; and
    * at least one TARGET was actually non-PASS. Failures confined to
      unrelated checks satisfy the aggregate while demonstrating nothing
      about the checks under diagnosis — `evaluate` calls that
      `cannot-reproduce` precisely because the pattern was not
      reproduced.

    Both are re-derived from the baseline's own document, the second
    through #1437's `non_pass_ids` so "non-PASS" keeps one definition.
    """
    missing = baseline.missing_targets(handoff.targets)
    if (baseline.failure_count <= handoff.acceptable_failures
            and not missing):
        raise NonSuccess(
            f"the baseline observed {baseline.failure_count} failure(s) "
            f"against an acceptable ceiling of "
            f"{handoff.acceptable_failures} out of "
            f"{baseline.requested_runs} and left no target check MISSING, "
            f"so it reproduced nothing to attribute; that is the "
            f"{deflake_diagnosis.ROUTE_CANNOT_REPRODUCE!r} evidence")
    observed = set(deflake_diagnosis.non_pass_ids(baseline.result))
    hit = [cid for cid in handoff.targets if cid in observed]
    if not hit:
        raise NonSuccess(
            f"the baseline never observed the target check(s) "
            f"{', '.join(handoff.targets)} as FAIL or MISSING, so it did "
            f"not reproduce the pattern the targets were identified from; "
            f"failures somewhere else are the "
            f"{deflake_diagnosis.ROUTE_CANNOT_REPRODUCE!r} evidence for these targets")
