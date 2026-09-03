#!/usr/bin/env python3
"""The whole handoff, assembled once and stamped once.

One of four internal owners behind `tools/deflake_handoff.py`, which
stays the stable public import façade for epic #1426's two consumers
(`tools/deflake_outcome.py` #1439 and `tools/deflake_issue.py` #1438).
Read that module's docstring for the contract narrative; the sections
"A document that exists establishes nothing" and "The clock is shared
because the resume rule is" are the rationale for what is below.

This owns the `Handoff` representation and `require_handoff`, the one
entry gate both consumers call: the envelope schema, the attempt
identity and summary, the worktree set every path below is bounded by,
the role inventory each route requires and forbids, the cross-role
invocation of the producer owner's binding and descriptor rules, the
refusal of a supplied `unmet_condition`, and the clock —
`utc_now` and the `reuse_stored_timestamp` reconciler that makes a
retry a resume rather than a conflict.

It is the last owner in the family's one-way order: it consumes the
grammar, measurement and producer-binding owners, and nothing consumes
it but the façade. The descriptor and target rules it applies are
DEFINED by the producer-binding owner; calling them here is use, not a
second definition.

    python3 tools/test_deflake_diagnosis.py       # the deterministic gate
"""
from __future__ import annotations

import copy
import os
import sys
from datetime import datetime, timezone

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_census  # noqa: E402
from deflake_handoff_grammar import (  # noqa: E402
    HANDOFF_SCHEMA,
    HandoffError,
    MAX_SUMMARY,
    RouteOwnership,
    _require_identity,
    _require_object,
    _require_text,
)
from deflake_handoff_measurement import require_measurement  # noqa: E402
from deflake_handoff_producer import (  # noqa: E402
    _bind_to_producer,
    _require_one_descriptor,
    declared_worktrees,
    require_diagnosis_outcome,
)


class Handoff:
    """One accepted outcome handoff: one probe, one de-flake attempt."""

    def __init__(self, *, attempt: str, summary: str, diagnosis,
                 measurements: dict):
        self.attempt = attempt
        self.summary = summary
        self.diagnosis = diagnosis
        self.measurements = measurements

    @property
    def route(self) -> str:
        return self.diagnosis["route"]

    @property
    def reason(self) -> str:
        return self.diagnosis["reason"]

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

    @property
    def configuration(self) -> list:
        return copy.deepcopy(self.diagnosis["configuration"])

    @property
    def invocation(self) -> dict:
        return copy.deepcopy(self.diagnosis["invocation"])

    def measurement(self, role: str):
        return self.measurements.get(role)

    def artifacts(self) -> list:
        """Every retained artifact this attempt has, in a stable order.

        Taken from the producer record alone, because the entry gate has
        already REBUILT that field from the per-batch references and
        required the two to be equal — and every declared measurement is
        bound to its reference's retained list exactly. Unioning the
        measurements in again here would add nothing and would quietly
        re-admit whatever the equality check exists to refuse.
        """
        return list(self.diagnosis["retained_artifacts"])


def require_handoff(document, *, worktrees=(), primary=None,
                    owned: RouteOwnership) -> Handoff:
    """One `deflake-outcome-handoff/v1`, or the refusal that names why.

    `owned` selects which of #1437's routes the caller answers; every
    other rule below is the same for both of epic #1426's consumers.
    """
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
    trees = [tree for tree in
             list(worktrees) + declared_worktrees(
                 envelope.get("diagnosis_outcome"))
             + ([primary] if primary else [])
             if tree]
    diagnosis = require_diagnosis_outcome(envelope.get("diagnosis_outcome"),
                                          worktrees=trees, owned=owned)
    entries = envelope.get("measurements")
    if not isinstance(entries, list) or not entries:
        raise HandoffError(
            "the handoff must declare at least one measurement, each with "
            "the exit code its harness invocation returned")
    measurements: dict = {}
    for entry in entries:
        measurement = require_measurement(entry, probe=diagnosis["probe"],
                                          seen=set(measurements),
                                          worktrees=trees)
        measurements[measurement.role] = measurement
    roles = owned.roles[diagnosis["route"]]
    for role in roles["required"]:
        if role not in measurements:
            raise HandoffError(
                f"the {diagnosis['route']!r} route rests on its {role} "
                f"measurement, which the handoff does not declare")
    # The route's own policy first, then the identity of what it was
    # given: "this route runs no verification batch" is the more
    # actionable sentence, and it is true of the ROUTE whatever the
    # producer record happens to reference.
    for role in roles["forbidden"]:
        if role in measurements:
            raise HandoffError(
                f"the {diagnosis['route']!r} route runs no {role} batch, so "
                f"a {role} measurement here describes work the route did not "
                f"do")
    for measurement in measurements.values():
        _bind_to_producer(measurement, diagnosis)
    _require_one_descriptor(measurements, diagnosis)
    if envelope.get("unmet_condition") is not None:
        raise HandoffError(
            "which #1437 acceptance condition a partial improvement failed "
            "is DERIVED from the diagnosis outcome's own `reason`, so the "
            "handoff may not supply one; a stored condition nobody checked "
            "would be the one field of the record that could say anything "
            "at all")
    return Handoff(attempt=attempt, summary=summary,
                   diagnosis=diagnosis, measurements=measurements)


# ==========================================================================
# The clock, and the one field a retry may reuse
# ==========================================================================

def utc_now() -> str:
    return datetime.now(timezone.utc).strftime(probe_census.TIMESTAMP_FORMAT)


def reuse_stored_timestamp(candidate: dict, stored: dict) -> dict:
    """The one field of a rebuilt record that cannot be derived again.

    Every field of an outcome record comes from the handoff except
    `timestamp_utc`, which comes from a clock — and a clock reads
    differently on a retry. Idempotency is the WHOLE record, so a retry
    that stamped itself anew would present one attempt identity carrying
    two different records and be refused as a conflict instead of
    recognized as the resume it is.

    `probe_census.record_outcome_installed` calls this INSIDE its locked
    transaction, and only when the attempt is already stored. That
    placement is the point: two concurrent invocations of one new
    attempt serialize on the census lock, so the second builds its
    record against what the first actually committed rather than against
    a snapshot taken before it. Nothing else is copied across — every
    other difference is a real one, and `ingest_outcome` still refuses
    it.
    """
    instant = stored.get("timestamp_utc")
    if not isinstance(instant, str):
        return candidate
    return dict(candidate, timestamp_utc=instant)
