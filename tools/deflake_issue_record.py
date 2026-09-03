#!/usr/bin/env python3
"""The durable record one published production defect leaves (#1438).

`tools/deflake_issue.py` files ONE tracker issue for a diagnosed
production defect and RECORDS it, and the record is the completion
marker: an attempt the census already holds reuses its stored issue,
touches the tracker not at all, and appends nothing. This module owns
that record.

    python3 tools/test_deflake_diagnosis.py       # the deterministic gate

Its ownership runs from a validated issue identity to a stored outcome:
the production-defect census document, the stored-record lookup, the
retry reconciler, the `Published` result the CLI prints, and the
qualification a record cannot be written without. It performs no tracker
I/O and renders no issue — it consumes the identity orchestration
selected — so it imports neither the façade nor the document, tracker or
evidence owners.

The route's qualification lives beside the record it qualifies
--------------------------------------------------------------
`require_supported` is what `outcome_record` refuses on, so the two sit
together rather than the record owner importing the façade for it — and
it composes only `deflake_handoff` primitives, so nothing is duplicated
by placing it here. `publish` calls it EARLIER still, before the tracker
is touched at all, which is what lets both of its recovery paths run
after the artifact tree has been swept: an unsupported or untrustworthy
handoff is refused without so much as a search.

Trustworthiness is read over EVERY declared measurement, not only the
one the route is judged on: a filed defect is stated over all of them,
so one untrustworthy batch makes the whole attempt an operational error.
Then #1437's own reproduction qualification, CALLED rather than
paraphrased — a batch that stayed at or below X with every target
present, or that failed only checks nobody targeted, reproduced nothing
to attribute to the engine.
"""
from __future__ import annotations

import copy
import importlib
import json
import os
import sys
from pathlib import Path

# `tools/` carries no `__init__.py`, so it is an implicit namespace
# package, and every module in it has TWO import spellings: the
# `tools.<name>` one used from the repository root, and the bare one a
# caller who put `tools/` on `sys.path` uses. Python treats those as
# DIFFERENT modules, so resolving a sibling by bare name from a file
# that was itself loaded as `tools.<name>` loads a second copy of it —
# and then `tools.deflake_issue.issue_body is not
# tools.deflake_issue_document.issue_body`, `except
# tools.deflake_issue.PublicationFailed` stops catching what
# `tools.deflake_issue_tracker` raises, and a substituted
# `MAX_BODY_CHARS` lands on a module nothing renders through.
#
# So every dependency is resolved under the spelling that loaded THIS
# module, and the path insertion below remains for the bare spelling and
# for running this file directly as a script.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))


def _sibling(name: str):
    """One `tools/` module, under the spelling that loaded this one."""
    return importlib.import_module(
        f"{__package__}.{name}" if __package__ else name)


deflake_diagnosis = _sibling("deflake_diagnosis")
deflake_handoff = _sibling("deflake_handoff")
probe_census = _sibling("probe_census")

# Named from the diagnosis that produced it rather than restated, and
# spelled the way the façade spells it: a second spelling of the outcome
# is how a census row stops being greppable against its own diagnosis.
OUTCOME_PRODUCTION_DEFECT = deflake_diagnosis.ROUTE_PRODUCTION_DEFECT

ROLES = deflake_handoff.ROLES
ROLE_BASELINE = deflake_handoff.ROLE_BASELINE
HandoffError = deflake_handoff.HandoffError
NonSuccess = deflake_handoff.NonSuccess


def require_supported(handoff) -> None:
    """Everything that must hold before this attempt reaches the tracker.

    Trustworthiness over EVERY declared measurement, not only the one
    the route is judged on: a filed defect is stated over all of them,
    so one untrustworthy batch makes the whole attempt an operational
    error. Then #1437's own reproduction qualification, CALLED rather
    than paraphrased — a batch that stayed at or below X with every
    target present, or that failed only checks nobody targeted,
    reproduced nothing to attribute to the engine and is #1439's
    `cannot-reproduce`.
    """
    problems = [problem
                for role in ROLES if role in handoff.measurements
                for problem in
                handoff.measurements[role].trustworthiness_problems()]
    if problems:
        raise NonSuccess(
            "a filed production defect rests on complete, trustworthy "
            "measurements, and this attempt's are not that: "
            + "; ".join(problems))
    deflake_handoff.require_reproduced(
        handoff, handoff.measurement(ROLE_BASELINE))


def utc_now() -> str:
    return deflake_handoff.utc_now()


def outcome_record(handoff, *, now: str, issue: dict) -> dict:
    """The census record one published production defect produces.

    Deliberately the SAME shape `deflake_outcome.outcome_record` writes,
    plus the issue identity: one `outcomes` collection holds every
    ending of a de-flake attempt, and a second shape would make the row
    unreadable without knowing which workflow wrote each entry.
    `recommendation` and `comparison` are the two route-specific fields
    this ending has nothing to say in, and they are stored as an
    explicit null rather than dropped.

    Raises `NonSuccess` when the evidence does not support the route.
    Nothing is written here; `publish` is what installs it.
    """
    require_outcome_timestamp(now)
    require_supported(handoff)
    return {
        "attempt": handoff.attempt,
        "outcome": OUTCOME_PRODUCTION_DEFECT,
        "reason": handoff.reason,
        "probe": handoff.probe,
        "timestamp_utc": now,
        "baseline_sha": handoff.baseline_sha,
        "acceptable_failures": handoff.acceptable_failures,
        "targets": handoff.targets,
        "configuration": handoff.configuration,
        "invocation": handoff.invocation,
        "measurements": [handoff.measurements[role].summary()
                         for role in ROLES if role in handoff.measurements],
        "retained_artifacts": handoff.artifacts(),
        "summary": handoff.summary,
        "recommendation": None,
        "comparison": None,
        "issue": copy.deepcopy(issue),
    }


def require_outcome_timestamp(now: str) -> None:
    try:
        probe_census.parse_timestamp(now, "the outcome timestamp")
    except probe_census.CensusError as error:
        raise HandoffError(f"the outcome timestamp: {error}") from None


def stored_record(census_path: Path, handoff):
    """This attempt's already-recorded outcome, or None.

    Read OUTSIDE the census lock and used for exactly one decision: not
    to touch the tracker at all when the attempt is already complete.
    Correctness does not rest on it — `probe_census.ingest_outcome`
    re-reads under the lock and refuses a conflicting record — so a
    stale read costs a redundant reconcile, never a duplicate.
    """
    try:
        document = json.loads(Path(census_path).read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None
    if not isinstance(document, dict):
        # Not a census at all. The locked transaction is what refuses
        # that, with a message naming the file; this read answers only
        # "is the attempt already complete", and it is not.
        return None
    row = probe_census.find_entry(document, handoff.probe)
    found = probe_census.find_outcome((row or {}).get("census"),
                                      handoff.attempt)
    if found is None:
        return None
    if found.get("outcome") != OUTCOME_PRODUCTION_DEFECT:
        raise NonSuccess(
            f"attempt {handoff.attempt!r} is already recorded for "
            f"{handoff.probe!r} as the {found.get('outcome')!r} outcome, so "
            f"this is not a resume of that attempt; an attempt identity "
            f"must identify one attempt")
    issue = found.get("issue")
    if not isinstance(issue, dict):
        raise NonSuccess(
            f"attempt {handoff.attempt!r} is already recorded as a "
            f"{OUTCOME_PRODUCTION_DEFECT!r} outcome carrying no issue "
            f"identity; that record could not have been written by this "
            f"workflow and is not one to resume")
    return found


def reuse_stored_publication(candidate: dict, stored: dict) -> dict:
    """The fields of a rebuilt record that cannot be derived again.

    Idempotency is the WHOLE record, and this ending has two fields a
    retry cannot reproduce from the handoff: `timestamp_utc`, which
    comes from a clock, and `issue`, which came from the tracker and
    carries the review-routing brand the resuming invocation may have
    spelled differently. Both are copied from what the census actually
    holds, INSIDE its transaction, so a replay is a replay rather than a
    conflict — and two genuinely different outcomes still cannot be made
    to agree, because nothing else is copied across.
    """
    candidate = deflake_handoff.reuse_stored_timestamp(candidate, stored)
    issue = stored.get("issue")
    if isinstance(issue, dict):
        candidate = dict(candidate, issue=copy.deepcopy(issue))
    return candidate


class Published:
    """What one accepted attempt did, once the tracker and census answered."""

    def __init__(self, record: dict, *, resumed: bool, durable: bool,
                 created: bool, reconciled: bool, trimmed: bool,
                 changed_probe: bool, opened_pull_request: bool):
        self.record = record
        self.resumed = resumed
        self.durable = durable
        # Whether THIS invocation created the issue, whether it found
        # one already filed under the publication key, or neither
        # because the census already held the completed outcome. Three
        # distinct histories that all end with one issue.
        self.created = created
        self.reconciled = reconciled
        self.trimmed = trimmed
        self.changed_probe = changed_probe
        self.opened_pull_request = opened_pull_request

    def to_document(self) -> dict:
        return {"outcome": self.record["outcome"],
                "attempt": self.record["attempt"],
                "probe": self.record["probe"],
                "issue": copy.deepcopy(self.record["issue"]),
                "resumed": self.resumed,
                "durable": self.durable,
                "created_issue": self.created,
                "reconciled_issue": self.reconciled,
                "evidence_trimmed": self.trimmed,
                "changed_the_probe": self.changed_probe,
                "opened_pull_request": self.opened_pull_request,
                "terminal": True,
                "record": self.record}
