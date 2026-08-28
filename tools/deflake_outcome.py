#!/usr/bin/env python3
"""What a de-flake attempt does when it does NOT produce a repair (#1439).

`tools/deflake_diagnosis.py` (#1437) decides mechanically whether one
measurement handoff supports a probe-side repair, and emits a
`deflake-diagnosis-outcome/v1` record naming the ROUTE it took. Only one
of those routes opens a pull request. This module owns every other
ending it hands to #1439: it consumes one outcome handoff for one exact
registered probe and one de-flake attempt, decides whether the evidence
supports a STABLE non-success outcome, and — when it does — appends that
outcome durably to the probe's census row.

    python3 tools/deflake_outcome.py --handoff <document.json>
    python3 tools/test_deflake_diagnosis.py       # the deterministic gate

Nothing here boots an engine, runs a probe, opens a port, edits a file
in a worktree, or talks to GitHub. It reads documents, answers questions
about them, and performs exactly one write: `probe_census.update`'s
locked read-modify-write of `docs/probe_census.json` in the `docs-wip`
worktree. There is no second state store, no second write path and no
second lock — #1428's writer owns that file, and a private one here
would fork its ownership.

Three stable outcomes, and everything else
------------------------------------------
* `cannot-reproduce` — the controlled pre-fix measurement is complete
  and trustworthy and shows nothing wrong at all. Appends the outcome
  plus an ADVISORY de-list recommendation.
* `no-confident-fix` — the failure reproduced, but the evidence
  establishes no one probe-side cause and no one bounded repair.
* `partial-improvement` — a repair candidate measurably improved the
  failure count and still failed #1437's acceptance gate.

None of the three opens a pull request. Only #1437's confidently
diagnosed repair that passes its complete success gate may reach that
publisher, and this module is where that boundary is enforced from the
other side: `record` takes a `publisher` and never calls it. The
parameter exists so the gate can inject a spy and prove the absence,
rather than the absence resting on nobody having written the call yet.

Everything else is an ACTIONABLE NON-SUCCESS: it records none of the
three, opens no PR, and exits non-zero naming what to do next. That
covers a production-code or shipped-script defect (which is #1438's
route, a sibling in epic #1426 that this workflow must not require, stub
or import), every operational error below, and a census write that
refused.

A lower failure rate is not success
-----------------------------------
`partial-improvement` is a NUMERIC claim, not an adjective. Recording it
requires a baseline and a verification that are both complete and
trustworthy, were taken at the SAME run count and the SAME RTS
capability count, and whose verification failure count is strictly lower
than the baseline's while STILL failing the acceptance gate — over X, or
leaving a target check MISSING. Without the same-conditions clause the
comparison is not a comparison; without the still-failing clause the
route contradicts its own evidence. A verification that merely became
INVALID improved nothing measurable, so it is an operational error here
rather than a partial improvement claimed over evidence that cannot
support it.

Operational errors are not "cannot reproduce"
---------------------------------------------
`tools/probe_flake.py`'s exit contract is preserved rather than
paraphrased, and the handoff must state each measurement's exit code so
this can be checked instead of inferred:

* exit 0 — a VALID measurement, whatever failure rate it observed. It
  writes a document whose own `status` is `ok`.
* exit 2 — a rejection before execution. It writes NO document.
* exit 3 — port exhaustion. It writes NO document.
* exit 4 — an untrustworthy harness result. It DOES write a document:
  `probe_flake` renders and writes one whenever it reached the
  measurement loop, and returns `EXIT_HARNESS_ERROR` only from
  `return EXIT_OK if measurement.valid else EXIT_HARNESS_ERROR`. That
  document carries `status: "harness-error"`, a populated `error` and
  `error_run`, and a null `failure_rate`.

So a document that merely EXISTS and PARSES establishes nothing. Every
classification here reads the document's own `status`, `error_run`,
`completed_runs` and per-check tallies, and any of exit 2, 3 or 4, an
incomplete run set, malformed measurement data or an inconsistent
aggregate returns an actionable non-success. None of them can become a
`cannot-reproduce`, which is the load-bearing distinction of this whole
module: "we could not make it fail" and "we could not measure it" are
opposite conclusions that a careless reading collapses into one.

`error_run` gets its own clause because `probe_flake` deliberately keeps
a harness-error run OUT of `runs`. A classifier that inspected only
`runs` would see an all-PASS list for a measurement nobody can trust.

The run count is the document's, not a literal
----------------------------------------------
Completeness is `completed_runs == requested_runs` and the ceiling is X
out of the measurement's OWN requested run count. Ten is the standard
configured N (`probe_census.POLICY_RUN_COUNT`) and this module changes
no measurement semantics, but hard-coding it in the classifier would
silently misclassify a measurement taken at any other run count — which
the handoff carries as an input.

The de-list recommendation is advisory, and only that
-----------------------------------------------------
A `cannot-reproduce` outcome records a recommendation to de-list the
probe. Nothing here reads it back. This module never edits
`tools/ci_probes.py`, never removes a manual-only reason, never changes
a classification and never promotes a probe to CI: `MANUAL_ONLY_REASONS`
models a probe's grounds as several INDEPENDENT `Reason` records, so
"it turned out not to be flaky" is one of them and the rest still stand.
Acting on the recommendation is a person's decision, taken with all of
them in view.

Durability and resume
---------------------
The outcome record is a RESUME point: it carries the attempt identity,
the probe, the timestamp, the baseline commit, X, the targets, one
summary per measurement — commit, timestamp, run counts, failure and
timeout counts, rate, RTS capabilities, per-run outcomes and per-check
PASS/FAIL/MISSING tallies — the retained artifact REFERENCES, the
diagnostic or attempted-fix summary, and the route-specific evidence
(the de-list recommendation, or the baseline comparison with its ceiling
and unmet condition). A later reader never has to re-run the attempt to
know what it established.

Recording is idempotent on the attempt identity: resuming an attempt the
census already holds installs the identical bytes and appends nothing.
A write that refuses leaves the census file byte-identical, records no
outcome, releases the lock and returns an actionable non-success — the
attempt stays incomplete and resumable, and never falls through to a
publisher.

Raw stdout, protocol streams and engine logs stay in the harness's
artifact tree outside every worktree; only their path references are
stored, exactly as `probe_census.summarize_sample` already does for a
measurement.
"""
from __future__ import annotations

import argparse
import copy
import json
import os
import sys
from datetime import datetime, timezone
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import deflake_diagnosis  # noqa: E402
import probe_census  # noqa: E402
import probe_flake  # noqa: E402
import probe_protocol  # noqa: E402
import run_probes  # noqa: E402

HANDOFF_SCHEMA = "deflake-outcome-handoff/v1"
# The producer record this consumes, named from the producer rather than
# restated: a second copy that drifted would accept a document #1437
# never writes.
DIAGNOSIS_OUTCOME_SCHEMA = deflake_diagnosis.OUTCOME_SCHEMA

# The three stable outcomes. Spelled as #1437's own route identifiers,
# because a stable outcome IS the route it was handed, and two
# vocabularies for one concept is how a census row stops being
# greppable against the diagnosis that produced it.
OUTCOME_CANNOT_REPRODUCE = deflake_diagnosis.ROUTE_CANNOT_REPRODUCE
OUTCOME_NO_CONFIDENT_FIX = deflake_diagnosis.ROUTE_NO_CONFIDENT_FIX
OUTCOME_PARTIAL_IMPROVEMENT = deflake_diagnosis.ROUTE_PARTIAL_IMPROVEMENT
STABLE_OUTCOMES = (OUTCOME_CANNOT_REPRODUCE, OUTCOME_NO_CONFIDENT_FIX,
                   OUTCOME_PARTIAL_IMPROVEMENT)

# The publisher boundary, spelled as a decision rather than an omission.
# Under the selected policy all three stable outcomes forbid a repair
# pull request, so every value here is False — and `record` consults
# this table and calls the publisher only on a True, so the rule is a
# branch a gate can exercise instead of a call nobody happened to
# write. Flipping an entry makes the injected publisher fire, which is
# what makes the gate's "never called" assertion mean something.
OPENS_PULL_REQUEST = {
    OUTCOME_CANNOT_REPRODUCE: False,
    OUTCOME_NO_CONFIDENT_FIX: False,
    OUTCOME_PARTIAL_IMPROVEMENT: False,
}

# The measurement roles a handoff may declare.
ROLE_HANDOFF = "handoff"
ROLE_BASELINE = "baseline"
ROLE_VERIFICATION = "verification"
ROLES = (ROLE_HANDOFF, ROLE_BASELINE, ROLE_VERIFICATION)

# Which #1437 route this module answers, and how. A route absent from
# this table is one this workflow does not own.
#
# `no-target` is #1437's all-PASS ending: a schema-valid #1436 handoff
# whose own measurement observed no non-PASS check, so it runs no
# controlled batch at all. It is owned by this issue and means exactly
# what `cannot-reproduce` means — the evidence shows nothing wrong — so
# it becomes that stable outcome over the measurement it DOES have,
# which is the #1436 one. The predicate is identical either way, so an
# all-PASS ending cannot be recorded on weaker evidence than a
# controlled one.
ROUTE_TO_OUTCOME = {
    deflake_diagnosis.ROUTE_NO_TARGET: OUTCOME_CANNOT_REPRODUCE,
    deflake_diagnosis.ROUTE_CANNOT_REPRODUCE: OUTCOME_CANNOT_REPRODUCE,
    deflake_diagnosis.ROUTE_NO_CONFIDENT_FIX: OUTCOME_NO_CONFIDENT_FIX,
    deflake_diagnosis.ROUTE_PARTIAL_IMPROVEMENT: OUTCOME_PARTIAL_IMPROVEMENT,
}

# The measurement whose evidence each route's stable outcome is judged
# on, and the roles the handoff must and must not carry for it.
#
# `no-target` is designated on the #1436 measurement because it ran no
# controlled batch; every other route is designated on the controlled
# pre-fix baseline, which is what "#1437's controlled pre-fix
# measurement" names.
ROUTE_ROLES = {
    deflake_diagnosis.ROUTE_NO_TARGET: {
        "designated": ROLE_HANDOFF,
        "required": (ROLE_HANDOFF,),
        "forbidden": (ROLE_BASELINE, ROLE_VERIFICATION),
    },
    deflake_diagnosis.ROUTE_CANNOT_REPRODUCE: {
        "designated": ROLE_BASELINE,
        "required": (ROLE_BASELINE,),
        "forbidden": (ROLE_VERIFICATION,),
    },
    deflake_diagnosis.ROUTE_NO_CONFIDENT_FIX: {
        "designated": ROLE_BASELINE,
        "required": (ROLE_BASELINE,),
        "forbidden": (),
    },
    deflake_diagnosis.ROUTE_PARTIAL_IMPROVEMENT: {
        "designated": ROLE_BASELINE,
        "required": (ROLE_BASELINE, ROLE_VERIFICATION),
        "forbidden": (),
    },
}

# `tools/probe_flake.py`'s exit contract, taken from the module that
# owns it. Each exit says whether a result document exists at all and,
# when one does, what its own status must be — so "the document exists"
# is never mistaken for "the measurement is trustworthy".
EXIT_CONTRACT = {
    probe_flake.EXIT_OK: probe_census.ACCEPTED_STATUS,
    probe_flake.EXIT_REJECTED: None,
    probe_flake.EXIT_NO_PORT: None,
    probe_flake.EXIT_HARNESS_ERROR: "harness-error",
}

MAX_IDENTITY = 128
MAX_SUMMARY = 8000
MAX_CONDITION = 4000

EXIT_OK = 0
EXIT_REJECTED = 2
EXIT_NON_SUCCESS = 3


class HandoffError(Exception):
    """An outcome handoff this workflow refuses to read.

    Distinct from a non-success on purpose: a malformed input never
    reached a classification, so it is not a `cannot-reproduce` or a
    `no-confident-fix` and must not be recorded as one. It publishes no
    pull request and records no completed stable outcome.
    """


class NonSuccess(Exception):
    """A well-formed handoff whose ending is not a stable outcome.

    Actionable: the message names what the evidence actually showed and
    which route owns it. Nothing is recorded, nothing is published, and
    the attempt stays resumable.
    """


def forbidden_publisher(outcome) -> None:
    """The default `publisher`, which exists to refuse being called.

    Every route this module reaches forbids a pull request, so a call
    here is a bug rather than a policy decision, and it raises instead
    of being a silently unused parameter.
    """
    raise NonSuccess(
        f"the {outcome!r} outcome opens no pull request; only #1437's "
        f"confidently diagnosed repair that passes its complete success "
        f"gate may reach a publisher")


# ==========================================================================
# The entry gate
# ==========================================================================
def _require_object(value, what: str) -> dict:
    if not isinstance(value, dict):
        raise HandoffError(
            f"{what} must be a JSON object, got {type(value).__name__}")
    return value


def _require_text(value, what: str, *, limit: int) -> str:
    if not isinstance(value, str) or not value.strip():
        raise HandoffError(f"{what} must be a non-empty string, got {value!r}")
    if len(value) > limit:
        raise HandoffError(
            f"{what} is {len(value)} characters, above the {limit} a census "
            f"record stores")
    return value


def _require_identity(value, what: str) -> str:
    """An attempt identity: one line, no surrounding blanks, bounded.

    It is the IDEMPOTENCY key, so it has to be something a resuming
    workflow can reproduce exactly. A value carrying a newline or
    padding is one that two spellings of the same attempt would differ
    in, which is how a resume silently becomes a second append.
    """
    text = _require_text(value, what, limit=MAX_IDENTITY)
    if text != text.strip() or any(ch.isspace() and ch != " " for ch in text):
        raise HandoffError(
            f"{what} must be a single unpadded line, got {value!r}")
    return text


def _require_string_list(value, what: str) -> list:
    if not isinstance(value, list) or not all(
            isinstance(item, str) and item for item in value):
        raise HandoffError(f"{what} must be a list of non-empty strings")
    return list(value)


def _delegate(call, what: str):
    """A census grammar applied here, reported as this module's refusal.

    Commit identities and timestamps are the census's own vocabulary and
    are matched full-string by it, the placeholder `unknown` included. A
    `CensusError` is that grammar refusing the input, so it is reported
    as a malformed handoff rather than escaping as a traceback from a
    module the caller never named.
    """
    try:
        return call()
    except probe_census.CensusError as error:
        raise HandoffError(f"{what}: {error}") from None


def require_artifact_reference(value, what: str) -> str:
    """One retained-artifact path, stored as a REFERENCE and nothing else.

    Absolute because `probe_flake.check_artifact_root` resolves its root
    before a run begins and every path below it is built from that, so a
    relative entry names nothing a later reader can find. The census
    stores this string; it never opens the directory and never copies a
    byte of what is inside it.
    """
    text = _require_text(value, what, limit=4096)
    if not Path(text).is_absolute():
        raise HandoffError(
            f"{what} must be an absolute path, got {text!r}; the census "
            f"stores artifact references, and a relative one names nothing")
    return text


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
        return problems

    def defect_problems(self) -> list[str]:
        """Everything a TRUSTWORTHY measurement observed going wrong.

        Its counterpart above answers "can this batch be believed"; this
        answers "did it show anything". Every clause is stated
        separately even where two overlap, because a run outcome, an
        aggregate and a per-check tally are three INDEPENDENT accounts
        of one batch — nothing in the declared schema binds
        `failure_count` or `timeout_count` to the run list — and
        `cannot-reproduce` is the one conclusion that must not be
        reachable by satisfying only some of them.
        """
        problems: list[str] = []
        if self.failure_count:
            problems.append(
                f"the {self.role} measurement observed "
                f"{self.failure_count} failing run(s)")
        if self.timeout_count:
            problems.append(
                f"the {self.role} measurement observed "
                f"{self.timeout_count} timed-out run(s)")
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


class Handoff:
    """One accepted outcome handoff: one probe, one de-flake attempt."""

    def __init__(self, *, attempt: str, summary: str, unmet_condition,
                 diagnosis, measurements: dict):
        self.attempt = attempt
        self.summary = summary
        self.unmet_condition = unmet_condition
        self.diagnosis = diagnosis
        self.measurements = measurements

    @property
    def route(self) -> str:
        return self.diagnosis["route"]

    @property
    def probe(self) -> str:
        return self.diagnosis["probe"]

    @property
    def targets(self) -> list:
        return list(self.diagnosis["targets"])

    @property
    def acceptable_failures(self) -> int:
        return self.diagnosis["acceptable_failures"]

    @property
    def baseline_sha(self) -> str:
        return self.diagnosis["baseline_sha"]

    def measurement(self, role: str):
        return self.measurements.get(role)

    def artifacts(self) -> list:
        """Every retained artifact this attempt has, in a stable order.

        Deduplicated, not concatenated: #1437's outcome document already
        names every batch's retained directories, and a measurement
        names its own, so a plain `+` would store each of them twice.
        """
        seen: list[str] = []
        sources = [self.diagnosis.get("retained_artifacts") or []]
        sources += [self.measurements[role].result["retained_artifacts"]
                    for role in ROLES if role in self.measurements]
        for group in sources:
            for path in group:
                if path not in seen:
                    seen.append(path)
        return seen


def _registered_probes() -> set:
    return {key for key, _script, _purpose in run_probes.PROBES}


def require_diagnosis_outcome(document) -> dict:
    """#1437's producer record, held to the fields this consumer reads.

    Deliberately not a re-validation of the whole
    `deflake-diagnosis-outcome/v1` document: #1437 owns that record and
    this module does not second-guess the parts it does not use. What is
    checked is that every field this classification rests on is present
    and means what it says.
    """
    outcome = _require_object(document, "the diagnosis outcome")
    schema = outcome.get("schema")
    if schema != DIAGNOSIS_OUTCOME_SCHEMA:
        raise HandoffError(
            f"the diagnosis outcome is {schema!r}, expected "
            f"{DIAGNOSIS_OUTCOME_SCHEMA!r}")
    route = outcome.get("route")
    if route not in deflake_diagnosis.ROUTES:
        raise HandoffError(
            f"the diagnosis outcome declares the route {route!r}; the "
            f"declared routes are {', '.join(deflake_diagnosis.ROUTES)}")
    if outcome.get("opens_pull_request"):
        raise HandoffError(
            f"the diagnosis outcome opens a pull request, so it is #1437's "
            f"repair ending and not a non-success this workflow records")
    if route not in ROUTE_TO_OUTCOME:
        owner = deflake_diagnosis.ROUTE_OWNER.get(route)
        if owner == 1438:
            # A well-formed handoff for a route this workflow does not
            # own. It is a non-success rather than a malformed input,
            # and it names the owner rather than stubbing it: #1438 is a
            # sibling in epic #1426, not a prerequisite, so ordering
            # between the two must not be able to break this one.
            raise NonSuccess(
                f"the diagnosis identifies a production-code or "
                f"shipped-script defect, which is #{owner}'s route: this "
                f"attempt records none of the stable outcomes "
                f"({', '.join(STABLE_OUTCOMES)}) and opens no pull request")
        raise HandoffError(
            f"the route {route!r} is not one this workflow records; it hands "
            f"off to {'#%d' % owner if owner else 'nobody'}, and the routes "
            f"this workflow owns are "
            f"{', '.join(sorted(ROUTE_TO_OUTCOME))}")
    probe = outcome.get("probe")
    if probe not in _registered_probes():
        raise HandoffError(
            f"the diagnosis outcome names the probe {probe!r}, which is not "
            f"registered in tools/run_probes.py")
    # Required rather than defaulted to empty. `targets` drives the
    # MISSING half of the acceptance gate, so a record that lost it
    # would quietly weaken the `no-confident-fix` and
    # `partial-improvement` predicates instead of failing. #1437 always
    # writes the key, empty list and all.
    targets = _require_string_list(outcome.get("targets"),
                                   "the diagnosis outcome's `targets`")
    acceptable = outcome.get("acceptable_failures")
    acceptable = _delegate(
        lambda: probe_census.require_acceptable_failures(
            acceptable, "the diagnosis outcome's `acceptable_failures`"),
        "the acceptable-failure ceiling")
    baseline_sha = _delegate(
        lambda: probe_census.require_commit_identity(
            outcome.get("baseline_sha"),
            "the diagnosis outcome's `baseline_sha`"),
        "the baseline commit")
    artifacts = _require_string_list(
        outcome.get("retained_artifacts"),
        "the diagnosis outcome's `retained_artifacts`")
    for path in artifacts:
        require_artifact_reference(
            path, "a retained artifact of the diagnosis outcome")
    return {
        "route": route,
        "probe": probe,
        "targets": targets,
        "acceptable_failures": acceptable,
        "baseline_sha": baseline_sha,
        "retained_artifacts": artifacts,
    }


def require_measurement(entry, *, probe: str, seen: set) -> Measurement:
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
    _delegate(lambda: probe_census.validate_result(result),
              f"the {role} measurement's result document")
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
            path, f"a retained artifact of the {role} measurement")
    return Measurement(role, exit_code, result)


def require_handoff(document) -> Handoff:
    """One `deflake-outcome-handoff/v1`, or the refusal that names why."""
    envelope = _require_object(document, "the outcome handoff")
    schema = envelope.get("schema")
    if schema != HANDOFF_SCHEMA:
        raise HandoffError(
            f"the outcome handoff is {schema!r}, expected {HANDOFF_SCHEMA!r}")
    attempt = _require_identity(envelope.get("attempt"),
                                "the handoff's `attempt` identity")
    summary = _require_text(envelope.get("summary"),
                            "the handoff's diagnostic or attempted-fix "
                            "`summary`", limit=MAX_SUMMARY)
    diagnosis = require_diagnosis_outcome(envelope.get("diagnosis_outcome"))
    entries = envelope.get("measurements")
    if not isinstance(entries, list) or not entries:
        raise HandoffError(
            "the handoff must declare at least one measurement, each with "
            "the exit code its harness invocation returned")
    measurements: dict = {}
    for entry in entries:
        measurement = require_measurement(entry, probe=diagnosis["probe"],
                                          seen=set(measurements))
        measurements[measurement.role] = measurement
    roles = ROUTE_ROLES[diagnosis["route"]]
    for role in roles["required"]:
        if role not in measurements:
            raise HandoffError(
                f"the {diagnosis['route']!r} route rests on its {role} "
                f"measurement, which the handoff does not declare")
    for role in roles["forbidden"]:
        if role in measurements:
            raise HandoffError(
                f"the {diagnosis['route']!r} route runs no {role} batch, so "
                f"a {role} measurement here describes work the route did not "
                f"do")
    unmet = envelope.get("unmet_condition")
    if diagnosis["route"] == deflake_diagnosis.ROUTE_PARTIAL_IMPROVEMENT:
        unmet = _require_text(
            unmet, "the handoff's `unmet_condition`", limit=MAX_CONDITION)
    elif unmet is not None:
        raise HandoffError(
            f"`unmet_condition` states which #1437 acceptance condition a "
            f"partial improvement failed, so the {diagnosis['route']!r} "
            f"route may not carry one")
    return Handoff(attempt=attempt, summary=summary, unmet_condition=unmet,
                   diagnosis=diagnosis, measurements=measurements)


# ==========================================================================
# Classification
# ==========================================================================
def _refuse(problems: list[str], because: str) -> None:
    raise NonSuccess(f"{because}: {'; '.join(problems)}")


def _classify_cannot_reproduce(handoff: Handoff) -> dict:
    """The one predicate that may conclude "nothing is wrong here".

    Stated over the designated measurement's own fields so no partial
    reading of the document can satisfy it, and refused for every
    operational error rather than being reached by one.
    """
    role = ROUTE_ROLES[handoff.route]["designated"]
    measurement = handoff.measurement(role)
    problems = measurement.defect_problems()
    if problems:
        _refuse(problems,
                f"{OUTCOME_CANNOT_REPRODUCE!r} requires a measurement that "
                f"observed nothing wrong at all, and this one is not that")
    return {
        "recommendation": {
            "action": "de-list",
            "advisory": True,
            "detail": (
                f"{measurement.requested_runs} of "
                f"{measurement.requested_runs} runs passed at "
                f"{measurement.result['commit_sha']} with every declared "
                f"check present, so consider de-listing {handoff.probe!r} as "
                f"flaky. Advisory only: tools/ci_probes.py is unchanged, and "
                f"a probe's other independent manual-only reasons still "
                f"stand."),
        },
        "comparison": None,
    }


def _classify_no_confident_fix(handoff: Handoff) -> dict:
    """A reproduced failure the evidence cannot attribute confidently.

    The predicate is not "the agent gave up": it is that the baseline
    genuinely REPRODUCED something to diagnose. Evidence showing nothing
    wrong is `cannot-reproduce`, and recording it here would bury a
    clean measurement under a conclusion about causality it never
    supported.
    """
    measurement = handoff.measurement(ROLE_BASELINE)
    missing = measurement.missing_targets(handoff.targets)
    if (measurement.failure_count <= handoff.acceptable_failures
            and not missing):
        raise NonSuccess(
            f"the baseline observed {measurement.failure_count} failure(s) "
            f"against an acceptable ceiling of "
            f"{handoff.acceptable_failures} out of "
            f"{measurement.requested_runs} and left no target check MISSING, "
            f"so it reproduced nothing to attribute; that is the "
            f"{OUTCOME_CANNOT_REPRODUCE!r} evidence, not "
            f"{OUTCOME_NO_CONFIDENT_FIX!r}")
    return {"recommendation": None, "comparison": None}


def _classify_partial_improvement(handoff: Handoff) -> dict:
    """A measured improvement that still failed the acceptance gate.

    Numeric on both halves. The improvement is a strictly lower failure
    count between two batches taken under the SAME conditions, and the
    failure is the acceptance gate's own measurement half — over X, or a
    target left MISSING. A verification that measurably PASSES that half
    contradicts the route it was handed under, and this refuses rather
    than recording a claim its own evidence denies.
    """
    baseline = handoff.measurement(ROLE_BASELINE)
    verification = handoff.measurement(ROLE_VERIFICATION)
    conditions = []
    if baseline.requested_runs != verification.requested_runs:
        conditions.append(
            f"the baseline requested {baseline.requested_runs} runs and the "
            f"verification {verification.requested_runs}")
    if (baseline.result["rts_capabilities"]
            != verification.result["rts_capabilities"]):
        conditions.append(
            f"the baseline ran at "
            f"{baseline.result['rts_capabilities']} RTS capabilities and the "
            f"verification at {verification.result['rts_capabilities']}")
    if conditions:
        _refuse(conditions,
                "two batches taken under different conditions are not a "
                "before-and-after comparison")
    if verification.failure_count >= baseline.failure_count:
        raise NonSuccess(
            f"the verification observed {verification.failure_count} "
            f"failure(s) against the baseline's {baseline.failure_count}, "
            f"which is no improvement; a lower failure RATE is the only "
            f"thing {OUTCOME_PARTIAL_IMPROVEMENT!r} claims, and this "
            f"evidence does not establish one")
    missing = verification.missing_targets(handoff.targets)
    if (verification.failure_count <= handoff.acceptable_failures
            and not missing):
        raise NonSuccess(
            f"the verification observed {verification.failure_count} "
            f"failure(s) against an acceptable ceiling of "
            f"{handoff.acceptable_failures} out of "
            f"{verification.requested_runs} and left no target check "
            f"MISSING, so it measurably PASSES the acceptance gate this "
            f"outcome exists to record a failure of")
    return {
        "recommendation": None,
        "comparison": {
            "baseline_failure_count": baseline.failure_count,
            "verification_failure_count": verification.failure_count,
            "acceptable_failures": handoff.acceptable_failures,
            "requested_runs": verification.requested_runs,
            "unmet_condition": handoff.unmet_condition,
        },
    }


CLASSIFIERS = {
    OUTCOME_CANNOT_REPRODUCE: _classify_cannot_reproduce,
    OUTCOME_NO_CONFIDENT_FIX: _classify_no_confident_fix,
    OUTCOME_PARTIAL_IMPROVEMENT: _classify_partial_improvement,
}


def utc_now() -> str:
    return datetime.now(timezone.utc).strftime(probe_census.TIMESTAMP_FORMAT)


def outcome_record(handoff: Handoff, *, now: str) -> dict:
    """The durable census record one accepted handoff produces.

    Raises `NonSuccess` when the evidence does not support the stable
    outcome its route was handed under. Nothing is written here: this
    builds the record, and `record` is what installs it.
    """
    outcome = ROUTE_TO_OUTCOME[handoff.route]
    # The census's own timestamp grammar, applied to the value this
    # module is about to store under it, and applied FIRST: a caller
    # supplies `now` so a test pins a clock instead of racing one, and a
    # supplied value that is not an instant is the caller's error rather
    # than something to discover after classifying.
    _delegate(lambda: probe_census.parse_timestamp(
        now, "the outcome timestamp"), "the outcome timestamp")
    # Trustworthiness next, over EVERY measurement the handoff declared
    # rather than only the one the route is judged on. A stable outcome
    # is stored beside all of them and rests on all of them, so one
    # untrustworthy batch makes the whole attempt an operational error —
    # which is also what keeps the summaries below reading a document
    # that exists. The classifiers then own only their own semantic
    # question, which is why none of them re-asks this one.
    problems = [problem
                for role in ROLES if role in handoff.measurements
                for problem in
                handoff.measurements[role].trustworthiness_problems()]
    if problems:
        _refuse(problems,
                "a stable outcome rests on complete, trustworthy "
                "measurements, and this attempt's are not that")
    extra = CLASSIFIERS[outcome](handoff)
    return {
        "attempt": handoff.attempt,
        "outcome": outcome,
        "probe": handoff.probe,
        "timestamp_utc": now,
        "baseline_sha": handoff.baseline_sha,
        "acceptable_failures": handoff.acceptable_failures,
        "targets": handoff.targets,
        "measurements": [handoff.measurements[role].summary()
                         for role in ROLES if role in handoff.measurements],
        "retained_artifacts": handoff.artifacts(),
        "summary": handoff.summary,
        "recommendation": extra["recommendation"],
        "comparison": extra["comparison"],
    }


# ==========================================================================
# Recording
# ==========================================================================
class Recorded:
    """What one accepted attempt did, once the census answered."""

    def __init__(self, record: dict, *, resumed: bool, durable: bool,
                 published: bool):
        self.record = record
        self.resumed = resumed
        self.durable = durable
        # What the publisher boundary actually did, not what the policy
        # says it should have: a report that hard-coded `false` would
        # keep saying so if the policy ever changed underneath it.
        self.published = published

    def to_document(self) -> dict:
        return {"outcome": self.record["outcome"],
                "attempt": self.record["attempt"],
                "probe": self.record["probe"],
                "resumed": self.resumed,
                "durable": self.durable,
                "opened_pull_request": self.published,
                "record": self.record}


def record(handoff: Handoff, *, census_path: Path, now: str,
           publisher=forbidden_publisher) -> Recorded:
    """Append one stable outcome to the probe's census row.

    `publisher` is consulted through `OPENS_PULL_REQUEST` and, under the
    selected policy, never called: every stable outcome forbids a repair
    pull request. It is a live branch rather than a missing call, so a
    gate can inject a spy and prove the absence instead of the absence
    resting on nobody having written the call.

    A refusal from the census leaves its bytes exactly as they were,
    records nothing and returns an actionable non-success — the attempt
    stays incomplete and resumable. A durability warning is the one
    thing that is NOT a refusal: `probe_census.update` raises it only
    after the replacement, so the record is already what a later reader
    parses and re-appending it would be the duplicate this whole path
    exists to prevent.
    """
    document = outcome_record(handoff, now=now)
    path = Path(census_path)
    durable = True
    resumed = False
    try:
        _probe, resumed = probe_census.record_outcome_installed(
            path, handoff.probe, document)
    except probe_census.CensusDurabilityUnconfirmed as error:
        durable = False
        print(f"deflake_outcome: warning: the census append is installed but "
              f"its durability is unconfirmed ({error}); do not re-record "
              f"this attempt", file=sys.stderr)
    except probe_census.CensusError as error:
        raise NonSuccess(
            f"the census refused the {document['outcome']!r} outcome for "
            f"attempt {handoff.attempt!r} ({error}); nothing was recorded "
            f"and nothing was published, so the attempt stays resumable"
        ) from None
    # The publisher boundary, consulted AFTER the durable append rather
    # than instead of it: a route that opened a pull request would still
    # have had to record what it did first. No stable outcome sets this,
    # so nothing here reaches the publisher.
    published = False
    if OPENS_PULL_REQUEST[document["outcome"]]:
        publisher(document)
        published = True
    return Recorded(document, resumed=resumed, durable=durable,
                    published=published)


def _load(path: str, what: str):
    try:
        return json.loads(Path(path).read_text(encoding="utf-8"))
    except OSError as error:
        raise HandoffError(
            f"{what} at {path} is unreadable ({error})") from None
    except json.JSONDecodeError as error:
        raise HandoffError(f"{what} at {path} is not JSON ({error})") from None


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--handoff", metavar="PATH", required=True,
                    help=f"record the outcome of one {HANDOFF_SCHEMA}")
    ap.add_argument("--census", metavar="PATH", default=None,
                    help="the census to append to (default: the docs-wip "
                         "worktree's docs/probe_census.json)")
    ap.add_argument("--json", action="store_true",
                    help="print the recorded outcome instead of prose")
    args = ap.parse_args(argv)

    try:
        handoff = require_handoff(_load(args.handoff, "outcome handoff"))
        census_path = (Path(args.census) if args.census
                       else probe_census.manifest_path())
        recorded = record(handoff, census_path=census_path, now=utc_now())
    except HandoffError as error:
        print(f"deflake_outcome: handoff rejected: {error}", file=sys.stderr)
        return EXIT_REJECTED
    except NonSuccess as error:
        print(f"deflake_outcome: no stable outcome: {error}", file=sys.stderr)
        return EXIT_NON_SUCCESS
    except probe_census.DocsWorktreeMissing as error:
        print(f"deflake_outcome: no stable outcome: {error}", file=sys.stderr)
        return EXIT_NON_SUCCESS

    if args.json:
        print(json.dumps(recorded.to_document(), indent=2, sort_keys=True))
    else:
        verb = "already recorded" if recorded.resumed else "recorded"
        print(f"{recorded.record['outcome']}: {verb} for "
              f"{recorded.record['probe']} attempt "
              f"{recorded.record['attempt']}")
        if recorded.record["recommendation"]:
            print(f"advisory: {recorded.record['recommendation']['detail']}")
    return EXIT_OK


if __name__ == "__main__":
    sys.exit(main())
