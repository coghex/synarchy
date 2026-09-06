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
    python3 tools/test_deflake_diagnosis.py --only outcome    # this module's

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
  "Reproduced" is #1437's own two-part qualification, asked of every
  route past the `cannot-reproduce` fork: the batch is over X or lost a
  target, AND at least one TARGET was actually non-PASS. Failures
  confined to unrelated checks satisfy the first while demonstrating
  nothing about the checks under diagnosis.
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
than the baseline's while STILL failing #1437's acceptance gate for the
reason #1437 named. `verification-over-tolerance` is re-derived from the
verification's own failure count, and `verification-missing-rule` by
CALLING `deflake_contract.missing_problems` — whose scoped rule has
four clauses of which only one is about targets, so a PASSING run that
omits a NON-target check fails it too and a paraphrase of "no target
went MISSING" would call such a verification passing. The other two
reasons are the producer's to make. Without the same-conditions clause
the comparison is not a comparison; without the still-failing clause the
route contradicts its own evidence. A verification that merely became
INVALID improved nothing measurable, so it is an operational error here
rather than a partial improvement claimed over evidence that cannot
support it.

The shared contract lives next door
-----------------------------------
Everything from the `deflake-outcome-handoff/v1` envelope to a validated
`Handoff` is `tools/deflake_handoff.py`'s, not this module's: the schema
and the measurement roles, route ownership and what each of #1437's
endings IS, the two exception classifications, the `Measurement` and
`Handoff` representations, every producer-record, identity, manifest,
invocation, path, worktree, descriptor, artifact and producer-binding
rule, the shared reproduced-failure predicate, and the clock and retry
reconciler a durable record is stamped with. `tools/deflake_issue.py`
(#1438) reads the SAME envelope for the `production-defect` ending, and
it reads it from that contract rather than from here — the two siblings
in epic #1426 are not each other's prerequisites, and a shared rule
owned by one of them is a rule the other cannot rely on.

What is left here is #1439's own: which route becomes which stable
outcome and on what evidence, the records and recommendations those
outcomes produce, the census append that makes them durable and
resumable, the no-pull-request boundary, and this module's CLI. The
names the contract owns are re-exported below rather than redefined, so
`deflake_outcome.HandoffError` IS `deflake_handoff.HandoffError` and an
`except` written against either matches a refusal raised through the
other.

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
Idempotency is the WHOLE record, and exactly one of its fields is not
derived from the handoff — `timestamp_utc` comes from a clock, which
reads differently on a retry — so a retry reuses the instant the stored
attempt was first stamped with instead of restamping itself into a
conflict. That lookup happens INSIDE the census transaction, under the
same lock the append is made under, so two concurrent invocations of one
new attempt serialize: the loser rebuilds against what the winner
actually committed rather than against a snapshot taken before it. A
write that refuses leaves the census file byte-identical,
records no outcome, releases the lock and returns an actionable
non-success — the attempt stays incomplete and resumable, and never
falls through to a publisher.
"""
from __future__ import annotations

import argparse
import json
import os
import sys
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import deflake_contract  # noqa: E402
import deflake_handoff  # noqa: E402
import probe_census  # noqa: E402

# ==========================================================================
# The shared contract, re-exported
# ==========================================================================
# Bound rather than redefined. These are the contract's OWN objects, so
# `deflake_outcome.HandoffError` and `deflake_handoff.HandoffError` are
# one class and an `except` written against either matches a refusal
# raised through the other — which two same-named classes would silently
# stop doing. Every name a repository consumer or the deterministic gate
# resolves through this module keeps resolving here while those callers
# move to the contract; retiring the facade is separate work, and
# nothing here schedules it.
HANDOFF_SCHEMA = deflake_handoff.HANDOFF_SCHEMA
DIAGNOSIS_OUTCOME_SCHEMA = deflake_handoff.DIAGNOSIS_OUTCOME_SCHEMA

ROLE_HANDOFF = deflake_handoff.ROLE_HANDOFF
ROLE_BASELINE = deflake_handoff.ROLE_BASELINE
ROLE_VERIFICATION = deflake_handoff.ROLE_VERIFICATION
ROLES = deflake_handoff.ROLES
PRE_FIX_ROLES = deflake_handoff.PRE_FIX_ROLES

ROUTE_ENDING = deflake_handoff.ROUTE_ENDING
RouteOwnership = deflake_handoff.RouteOwnership

EXIT_CONTRACT = deflake_handoff.EXIT_CONTRACT
MAX_IDENTITY = deflake_handoff.MAX_IDENTITY
MAX_SUMMARY = deflake_handoff.MAX_SUMMARY
REFERENCE_FIELDS = deflake_handoff.REFERENCE_FIELDS

HandoffError = deflake_handoff.HandoffError
NonSuccess = deflake_handoff.NonSuccess

Measurement = deflake_handoff.Measurement
Handoff = deflake_handoff.Handoff

require_artifact_reference = deflake_handoff.require_artifact_reference
require_configuration = deflake_handoff.require_configuration
require_input_identity = deflake_handoff.require_input_identity
require_invocation_identity = deflake_handoff.require_invocation_identity
require_measurement = deflake_handoff.require_measurement
require_worktree_boundary = deflake_handoff.require_worktree_boundary
declared_worktrees = deflake_handoff.declared_worktrees
require_reproduced = deflake_handoff.require_reproduced
utc_now = deflake_handoff.utc_now
reuse_stored_timestamp = deflake_handoff.reuse_stored_timestamp


# The three stable outcomes. Spelled as #1437's own route identifiers,
# because a stable outcome IS the route it was handed, and two
# vocabularies for one concept is how a census row stops being
# greppable against the diagnosis that produced it.
OUTCOME_CANNOT_REPRODUCE = deflake_contract.ROUTE_CANNOT_REPRODUCE
OUTCOME_NO_CONFIDENT_FIX = deflake_contract.ROUTE_NO_CONFIDENT_FIX
OUTCOME_PARTIAL_IMPROVEMENT = deflake_contract.ROUTE_PARTIAL_IMPROVEMENT
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
# controlled one. Its record therefore names NO target, and every other
# route names at least one — the two directions `evaluate` refuses, and
# a consumer that let either through would classify against a premise
# its own producer had rejected.
ROUTE_TO_OUTCOME = {
    deflake_contract.ROUTE_NO_TARGET: OUTCOME_CANNOT_REPRODUCE,
    deflake_contract.ROUTE_CANNOT_REPRODUCE: OUTCOME_CANNOT_REPRODUCE,
    deflake_contract.ROUTE_NO_CONFIDENT_FIX: OUTCOME_NO_CONFIDENT_FIX,
    deflake_contract.ROUTE_PARTIAL_IMPROVEMENT: OUTCOME_PARTIAL_IMPROVEMENT,
}

# The measurement whose evidence each route's stable outcome is judged
# on, and the roles the handoff must and must not carry for it.
#
# `no-target` is designated on the #1436 measurement because it ran no
# controlled batch; every other route is designated on the controlled
# pre-fix baseline, which is what "#1437's controlled pre-fix
# measurement" names.
ROUTE_ROLES = {
    deflake_contract.ROUTE_NO_TARGET: {
        "designated": ROLE_HANDOFF,
        "required": (ROLE_HANDOFF,),
        "forbidden": (ROLE_BASELINE, ROLE_VERIFICATION),
    },
    deflake_contract.ROUTE_CANNOT_REPRODUCE: {
        "designated": ROLE_BASELINE,
        "required": (ROLE_BASELINE,),
        "forbidden": (ROLE_VERIFICATION,),
    },
    deflake_contract.ROUTE_NO_CONFIDENT_FIX: {
        "designated": ROLE_BASELINE,
        "required": (ROLE_BASELINE,),
        # #1437 refuses a `no-confident-fix` diagnosis that carries a
        # verification section at all — that route opens no pull request
        # and changes no probe, so a verification means a repair was
        # attempted and the route is mislabelled. Forbidding it here
        # keeps the two ends of the handoff describing one invocation.
        "forbidden": (ROLE_VERIFICATION,),
    },
    deflake_contract.ROUTE_PARTIAL_IMPROVEMENT: {
        "designated": ROLE_BASELINE,
        "required": (ROLE_BASELINE, ROLE_VERIFICATION),
        "forbidden": (),
    },
}

# This module's own ownership: the three routes that become a stable
# outcome, judged on the roles each rests on.
OWNED = RouteOwnership(issue=1439, outcomes=STABLE_OUTCOMES,
                       roles=ROUTE_ROLES)

EXIT_OK = 0
EXIT_REJECTED = 2
EXIT_NON_SUCCESS = 3


def require_diagnosis_outcome(document, *, worktrees=(),
                              owned: RouteOwnership = OWNED) -> dict:
    """The shared gate's producer-record check, on #1439's routes.

    The contract takes `owned` explicitly, because a shared rule that
    defaulted to one consumer's routes would answer for that consumer
    whenever a caller forgot to say which routes it owns. This module
    does have a default — its own three — and that is what every call
    inside this file passes.
    """
    return deflake_handoff.require_diagnosis_outcome(
        document, worktrees=worktrees, owned=owned)


def require_handoff(document, *, worktrees=(), primary=None,
                    owned: RouteOwnership = OWNED) -> Handoff:
    """One `deflake-outcome-handoff/v1` on this workflow's own routes."""
    return deflake_handoff.require_handoff(
        document, worktrees=worktrees, primary=primary, owned=owned)


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
# Classification
# ==========================================================================
def _refuse(problems: list[str], because: str) -> None:
    raise NonSuccess(f"{because}: {'; '.join(problems)}")


def _classify_cannot_reproduce(handoff: Handoff) -> dict:
    """"Nothing is wrong here" — and only when the condition was real.

    #1437 reaches `cannot-reproduce` three ways, and only one of them
    says anything about the PROBE: the controlled batch ran under the
    handoff's own recorded condition and observed nothing. The other two
    say the invocation could not establish that condition at all — the
    configuration could not be recreated from the manifest, or the batch
    never became a controlled measurement — and a batch that passed
    somewhere else is no evidence about this probe.

    Both are recorded, because the evidence is what #1439 exists to
    keep. Only the first carries the de-list recommendation, and only
    the first is held to "observed nothing wrong at all": a batch that
    ran out of control may well have failed, and demanding a spotless
    one would throw the evidence away rather than record it.
    """
    reason = handoff.reason
    role = ROUTE_ROLES[handoff.route]["designated"]
    measurement = handoff.measurement(role)
    if reason not in deflake_contract.CONTROLLED_REASONS:
        return {"recommendation": None, "comparison": None}
    problems = measurement.defect_problems()
    if problems:
        _refuse(problems,
                f"{OUTCOME_CANNOT_REPRODUCE!r} for the reason {reason!r} "
                f"requires a measurement that observed nothing wrong at all, "
                f"and this one is not that")
    return {
        "recommendation": {
            "action": "de-list",
            "advisory": True,
            "detail": (
                f"{measurement.requested_runs} of "
                f"{measurement.requested_runs} runs passed at "
                f"{measurement.result['commit_sha']} under the handoff's own "
                f"recorded condition, with every declared check present, so "
                f"consider de-listing {handoff.probe!r} as flaky. Advisory "
                f"only: tools/ci_probes.py is unchanged, and a probe's other "
                f"independent manual-only reasons still stand."),
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
    require_reproduced(handoff, handoff.measurement(ROLE_BASELINE))
    return {"recommendation": None, "comparison": None}


def _classify_partial_improvement(handoff: Handoff) -> dict:
    """A measured improvement that still failed the acceptance gate.

    Numeric on both halves. The improvement is a strictly lower failure
    count between two batches taken under the SAME conditions, and the
    failure is #1437's acceptance gate — over X, or any violation of its
    scoped MISSING rule, which is CALLED here rather than paraphrased.
    A verification that measurably PASSES that gate contradicts the
    route it was handed under, and this refuses rather than recording a
    claim its own evidence denies.
    """
    baseline = handoff.measurement(ROLE_BASELINE)
    verification = handoff.measurement(ROLE_VERIFICATION)
    # The same qualification #1437 makes before either route: a repair
    # is only ever verified against a baseline that reproduced the
    # pattern, so "improved" has something to be an improvement ON.
    require_reproduced(handoff, baseline)
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
    # WHICH half of #1437's acceptance gate the verification failed is
    # the producer's own finding, and two of the four reasons are not
    # derivable here at all: whether the two comparison worktrees held
    # the same configuration, and whether the batch ran under control,
    # are facts about the INVOCATION rather than about either result
    # document. So the reason is taken from the record and CROSS-CHECKED
    # against the documents wherever it can be — a producer that named a
    # measurement-visible reason its own evidence denies has contradicted
    # itself, and that is not a stable outcome to record.
    _require_gate_failure(handoff, verification)
    return {
        "recommendation": None,
        "comparison": {
            "baseline_failure_count": baseline.failure_count,
            "verification_failure_count": verification.failure_count,
            "acceptable_failures": handoff.acceptable_failures,
            "requested_runs": verification.requested_runs,
            # DERIVED from the producer's reason, never free text a
            # caller supplies: a stored "unmet condition" nobody checked
            # would be the one field of this record that could say
            # anything at all.
            "unmet_condition": handoff.reason,
        },
    }


def _require_gate_failure(handoff: Handoff, verification: Measurement) -> None:
    """The named gate failure is real, wherever the documents can say.

    `verification-over-tolerance` and `verification-missing-rule` are
    both visible in the verification's own document, so each is
    re-derived — the second by CALLING #1437's scoped MISSING rule,
    whose four clauses include a PASSING run omitting a NON-target
    check, which a consumer paraphrasing "no target went MISSING" would
    have called a passing verification.

    The other two reasons are taken on the producer's word, and stated
    here rather than left implicit: a consumer handed only the result
    documents cannot see a configuration difference between two
    worktrees or a resource hold that was never obtained.
    """
    reason = handoff.reason
    over = verification.failure_count > handoff.acceptable_failures
    missing = deflake_contract.missing_problems(
        verification.result, targets=set(handoff.targets),
        what="the verification batch")
    if reason == deflake_contract.REASON_VERIFICATION_OVER_TOLERANCE:
        if not over:
            raise NonSuccess(
                f"the diagnosis outcome names {reason!r}, but the "
                f"verification observed {verification.failure_count} "
                f"failure(s) against an acceptable ceiling of "
                f"{handoff.acceptable_failures} out of "
                f"{verification.requested_runs}; a record whose own evidence "
                f"denies the condition it names is not a stable outcome")
        return
    if reason == deflake_contract.REASON_VERIFICATION_MISSING_RULE:
        if not missing:
            raise NonSuccess(
                f"the diagnosis outcome names {reason!r}, but the "
                f"verification satisfies #1437's scoped MISSING rule; a "
                f"record whose own evidence denies the condition it names is "
                f"not a stable outcome")
        return
    # `verification-not-controlled` and `verification-not-comparable`
    # are facts about the invocation, not about either result document,
    # so nothing here can confirm or deny them and the producer's word
    # stands. A verification that ALSO fails a measurement-visible half
    # of the gate is no contradiction: the reason names the FIRST thing
    # that made the batch unacceptable, not the only one.
    del over, missing


CLASSIFIERS = {
    OUTCOME_CANNOT_REPRODUCE: _classify_cannot_reproduce,
    OUTCOME_NO_CONFIDENT_FIX: _classify_no_confident_fix,
    OUTCOME_PARTIAL_IMPROVEMENT: _classify_partial_improvement,
}


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
    deflake_handoff.delegate_census_grammar(
        lambda: probe_census.parse_timestamp(
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
        # WHY #1437 took the route this outcome answers. `outcome` says
        # what was concluded; this says what the invocation actually
        # established, and the two are not the same question — a
        # `cannot-reproduce` whose condition could not be recreated
        # carries no de-list recommendation precisely because of it.
        "reason": handoff.reason,
        "probe": handoff.probe,
        "timestamp_utc": now,
        "baseline_sha": handoff.baseline_sha,
        "acceptable_failures": handoff.acceptable_failures,
        "targets": handoff.targets,
        # The condition the numbers were measured under, and the
        # invocation that produced them. Neither is derivable from a
        # census row: `probe_census.ingest_result` drops the command and
        # the invocation directory, and no census field has ever held
        # the configuration manifest at all.
        "configuration": handoff.configuration,
        "invocation": handoff.invocation,
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
    path = Path(census_path)
    document = outcome_record(handoff, now=now)
    durable = True
    resumed = False
    try:
        # A retry rebuilds the record the census already holds, stamp
        # included, so `ingest_outcome` sees a replay rather than a
        # conflict — and the stored record is read under the same lock
        # the append is made under, so no concurrent first writer can
        # slip between the two.
        _probe, resumed, document = probe_census.record_outcome_installed(
            path, handoff.probe, document,
            reconcile=reuse_stored_timestamp)
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
        handoff = require_handoff(
            _load(args.handoff, "outcome handoff"),
            worktrees=deflake_contract.worktree_paths(),
            primary=deflake_contract.primary_checkout())
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
