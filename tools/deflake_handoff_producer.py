#!/usr/bin/env python3
"""#1437's producer record, and the measurements it says it judged.

One of four internal owners behind `tools/deflake_handoff.py`, which
stays the stable public import façade for epic #1426's two consumers
(`tools/deflake_outcome.py` #1439 and `tools/deflake_issue.py` #1438).
Read that module's docstring for the contract narrative; the sections
"The record has to agree with itself first", "One attempt reports
against one declared contract", "The measurement is the one that
diagnosis judged" and "Paths are references, and they stay outside every
worktree" are the rationale for what is below.

This owns everything that binds a `deflake-diagnosis-outcome/v1` record
to the batches it references: the registered-probe check, the
route/route-owner agreement `require_diagnosis_outcome` reaches through
the caller's `RouteOwnership`, the per-batch `REFERENCE_FIELDS` and
`_batch_reference` parsing, the measurement-to-producer binding, the
declared comparison-worktree boundaries, the artifact-list rebuild, and
BOTH descriptor rules — `_require_descriptor_record` for the record's own
twice-stated descriptor and `_require_one_descriptor` for every declared
measurement held to it. The assembly owner INVOKES the second; it does
not define one.

It consumes the grammar and measurement owners and nothing downstream of
itself, and imports `deflake_contract` directly for the upstream names
it uses rather than through any wrapper — the document contract, not
the diagnosis evaluator that used to re-export it (#2041).

    python3 tools/test_deflake_diagnosis.py       # the deterministic gate
"""
from __future__ import annotations

import os
import sys
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import deflake_contract  # noqa: E402
import probe_census  # noqa: E402
import probe_runner_registry  # noqa: E402
from deflake_handoff_grammar import (  # noqa: E402
    DIAGNOSIS_OUTCOME_SCHEMA,
    HandoffError,
    NonSuccess,
    PRE_FIX_ROLES,
    ROLES,
    ROLE_BASELINE,
    ROLE_HANDOFF,
    ROLE_VERIFICATION,
    ROUTE_ENDING,
    RouteOwnership,
    _require_object,
    _require_string_list,
    _require_text,
    _require_usable_path,
    delegate_census_grammar,
    require_artifact_reference,
    require_configuration,
    require_input_identity,
    require_invocation_identity,
)
from deflake_handoff_measurement import Measurement  # noqa: E402


def _registered_probes() -> set:
    return {key for key, _script, _purpose in probe_runner_registry.PROBES}


def require_diagnosis_outcome(document, *, worktrees=(),
                              owned: RouteOwnership) -> dict:
    """#1437's producer record, held to the fields this consumer reads.

    Deliberately not a re-validation of the whole
    `deflake-diagnosis-outcome/v1` document: #1437 owns that record and
    this module does not second-guess the parts it does not use. What is
    checked is that every field this classification rests on is present
    and means what it says.

    `owned` is the ONE part of the gate that differs between epic
    #1426's two sibling consumers, so it is a parameter rather than a
    second copy of everything above it. It is required rather than
    defaulted: a shared contract that carried one consumer's routes as
    its default would answer for that consumer whenever a caller forgot
    to say which routes it owns.
    """
    outcome = _require_object(document, "the diagnosis outcome")
    schema = outcome.get("schema")
    if schema != DIAGNOSIS_OUTCOME_SCHEMA:
        raise HandoffError(
            f"the diagnosis outcome is {schema!r}, expected "
            f"{DIAGNOSIS_OUTCOME_SCHEMA!r}")
    route = outcome.get("route")
    if route not in deflake_contract.ROUTES:
        raise HandoffError(
            f"the diagnosis outcome declares the route {route!r}; the "
            f"declared routes are {', '.join(deflake_contract.ROUTES)}")
    if outcome.get("opens_pull_request"):
        raise HandoffError(
            f"the diagnosis outcome opens a pull request, so it is #1437's "
            f"repair ending and not a non-success this workflow records")
    if not owned.owns(route):
        owner = deflake_contract.ROUTE_OWNER.get(route)
        ending = ROUTE_ENDING.get(route, f"declares the route {route!r}")
        if owner is not None and owner != owned.issue:
            # A well-formed handoff for a route this workflow does not
            # own but a SIBLING does. It is a non-success rather than a
            # malformed input, and it names the owner rather than
            # stubbing it: the siblings in epic #1426 are not each
            # other's prerequisites, so ordering between them must not
            # be able to break either one.
            raise NonSuccess(
                f"the diagnosis {ending}, which is #{owner}'s route: this "
                f"attempt records none of the outcomes this workflow owns "
                f"({', '.join(owned.outcomes)}) and opens no pull request")
        raise HandoffError(
            f"the route {route!r} is not one this workflow records; it hands "
            f"off to {'#%d' % owner if owner else 'nobody'}, and the routes "
            f"this workflow owns are "
            f"{', '.join(sorted(owned.roles))}")
    reason = outcome.get("reason")
    if reason not in deflake_contract.ROUTE_REASONS.get(route, ()):
        raise HandoffError(
            f"the diagnosis outcome declares the reason {reason!r}, which "
            f"the {route!r} route cannot be reached for; its reasons are "
            f"{', '.join(deflake_contract.ROUTE_REASONS.get(route, ()))}")
    probe = outcome.get("probe")
    if probe not in _registered_probes():
        raise HandoffError(
            f"the diagnosis outcome names the probe {probe!r}, which is not "
            f"registered in probe_runner_registry.PROBES")
    # Required rather than defaulted to empty. `targets` drives the
    # MISSING half of the acceptance gate, so a record that lost it
    # would quietly weaken the `no-confident-fix` and
    # `partial-improvement` predicates instead of failing. #1437 always
    # writes the key, empty list and all.
    targets = _require_string_list(outcome.get("targets"),
                                   "the diagnosis outcome's `targets`")
    # The target list and the route are two statements of one fact, and
    # #1437 makes them agree in BOTH directions: `no-target` is what an
    # all-PASS measurement produces, and it refuses that route over a
    # handoff naming targets — while every other route is refused over a
    # handoff naming NONE, because there is then nothing to diagnose. A
    # consumer that let either through would classify against a premise
    # its own producer had rejected: a `no-target` record naming `beta`
    # would earn an advisory de-list from a measurement that observed
    # `beta` going wrong.
    if route == deflake_contract.ROUTE_NO_TARGET and targets:
        raise HandoffError(
            f"the {route!r} route is what an all-PASS measurement produces, "
            f"so its record names no target; this one names "
            f"{', '.join(targets)}")
    if route != deflake_contract.ROUTE_NO_TARGET and not targets:
        raise HandoffError(
            f"the diagnosis outcome names no target check, which is the "
            f"{deflake_contract.ROUTE_NO_TARGET!r} ending and not "
            f"{route!r}; every other route diagnoses at least one observed "
            f"non-PASS check")
    acceptable = outcome.get("acceptable_failures")
    acceptable = delegate_census_grammar(
        lambda: probe_census.require_acceptable_failures(
            acceptable, "the diagnosis outcome's `acceptable_failures`"),
        "the acceptable-failure ceiling")
    baseline_sha = delegate_census_grammar(
        lambda: probe_census.require_commit_identity(
            outcome.get("baseline_sha"),
            "the diagnosis outcome's `baseline_sha`"),
        "the baseline commit")
    artifacts = _require_string_list(
        outcome.get("retained_artifacts"),
        "the diagnosis outcome's `retained_artifacts`")
    for path in artifacts:
        require_artifact_reference(
            path, "a retained artifact of the diagnosis outcome",
            worktrees=worktrees)
    expected_checks = _require_string_list(
        outcome.get("handoff", {}).get("expected_checks")
        if isinstance(outcome.get("handoff"), dict) else None,
        "the diagnosis outcome's `handoff`.expected_checks")
    if not expected_checks:
        raise HandoffError(
            "the diagnosis outcome's `handoff`.expected_checks is empty; a "
            "measurement that declared no check reported nothing")
    descriptor = _require_descriptor_record(
        outcome["handoff"].get("expected_descriptor"),
        "the diagnosis outcome's `handoff`.expected_descriptor",
        expected_checks)
    stray = [cid for cid in targets if cid not in expected_checks]
    if stray:
        raise HandoffError(
            f"the diagnosis outcome targets {', '.join(stray)}, which its "
            f"own expected descriptor {expected_checks} does not declare; "
            f"the targets are the measurement's own non-PASS identifiers, "
            f"so one it never declared is not among them")
    # The input identity #1437 states twice, reconciled once.
    require_input_identity(
        outcome.get("handoff"), "the diagnosis outcome's `handoff`",
        probe=probe, baseline_sha=baseline_sha,
        acceptable_failures=acceptable, targets=targets)
    # WHICH measurement #1437 judged for each batch it ran. A route that
    # ran no such batch states `null` rather than dropping the key, so an
    # absent reference is a fact about the invocation and not a gap.
    references = {
        ROLE_HANDOFF: _batch_reference(
            outcome.get("handoff"), "the diagnosis outcome's `handoff`",
            worktrees=worktrees),
        ROLE_BASELINE: _batch_reference(
            outcome.get("baseline"), "the diagnosis outcome's `baseline`",
            worktrees=worktrees),
        ROLE_VERIFICATION: _batch_reference(
            outcome.get("verification"),
            "the diagnosis outcome's `verification`", worktrees=worktrees),
    }
    # #1437 builds its top-level `retained_artifacts` by accumulating
    # every batch it ran, in role order, deduplicated — so the field is
    # DERIVED and the consumer can rebuild it instead of believing it.
    # Validating each path individually only asks whether each names a
    # legal place; it never asks whether the list is the one the
    # invocation actually produced, so an unrelated directory appended
    # here alone would be stored as this attempt's evidence.
    union: list = []
    for role in ROLES:
        reference = references.get(role)
        if reference is None:
            continue
        for path in reference["retained_artifacts"]:
            if path not in union:
                union.append(path)
    if artifacts != union:
        raise HandoffError(
            f"the diagnosis outcome's `retained_artifacts` is {artifacts} "
            f"where the batches it references retained {union}; that list "
            f"is the ordered, deduplicated union of every batch the "
            f"invocation ran, so a set that is not it names evidence this "
            f"attempt does not have, or hides evidence it does")
    return {
        "route": route,
        "reason": reason,
        "probe": probe,
        "targets": targets,
        "acceptable_failures": acceptable,
        "baseline_sha": baseline_sha,
        "retained_artifacts": artifacts,
        "expected_checks": expected_checks,
        "expected_descriptor": descriptor,
        "references": references,
        "configuration": require_configuration(
            outcome.get("configuration"),
            "the diagnosis outcome's `configuration`"),
        "invocation": require_invocation_identity(
            outcome.get("handoff"), "the diagnosis outcome's `handoff`"),
    }


# Every field of #1437's per-batch reference that a result document also
# reports, so a declared measurement can be held to ALL of them. The
# commit and the instant alone are not an identity: two batches of one
# probe at one commit differ in where they wrote, and
# `probe_flake.new_invocation_dir` stamps a fresh directory per
# invocation precisely so that they do.
REFERENCE_FIELDS = ("commit_sha", "timestamp_utc", "artifact_root",
                    "invocation_dir", "retained_artifacts")


def _batch_reference(section, what: str, *, worktrees=()):
    """The identity #1437 recorded for one batch, or None if it ran none.

    #1437's outcome document carries a REFERENCE per batch rather than
    the document itself, so this is what says WHICH measurement it
    judged. A malformed reference is a malformed producer record.
    """
    if section is None:
        return None
    reference = _require_object(section, what)
    commit = delegate_census_grammar(
        lambda: probe_census.require_commit_identity(
            reference.get("commit_sha"), f"{what}'s `commit_sha`"),
        f"{what}'s commit")
    stamp = reference.get("timestamp_utc")
    delegate_census_grammar(lambda: probe_census.parse_timestamp(
        stamp, f"{what}'s `timestamp_utc`"), f"{what}'s timestamp")
    artifacts = _require_string_list(
        reference.get("retained_artifacts"),
        f"{what}'s `retained_artifacts`")
    for path in artifacts:
        require_artifact_reference(path, f"a retained artifact of {what}",
                                   worktrees=worktrees)
    return {
        "commit_sha": commit,
        "timestamp_utc": stamp,
        "artifact_root": require_artifact_reference(
            reference.get("artifact_root"), f"{what}'s `artifact_root`",
            worktrees=worktrees),
        "invocation_dir": require_artifact_reference(
            reference.get("invocation_dir"), f"{what}'s `invocation_dir`",
            worktrees=worktrees),
        "retained_artifacts": artifacts,
    }


def _bind_to_producer(measurement: Measurement, diagnosis: dict) -> None:
    """The declared measurement IS the one #1437 judged, and no other.

    Binding on the probe alone would admit any well-formed batch of the
    same probe: a result taken at another commit, or another instant,
    could be supplied under a diagnosis that judged a different one, and
    the census would then store two conflicting accounts of one attempt.
    So each measurement is held to EVERY field of the producer record's
    reference for its role that a result document also reports — the
    commit, the instant, the artifact root, the invocation directory and
    the ordered retained artifacts. The commit and the instant alone are
    not an identity: two batches of one probe at one commit differ in
    where they wrote, which is exactly why
    `probe_flake.new_invocation_dir` stamps a fresh directory per
    invocation, and a substitute agreeing on the first two would still
    hand the census another batch's artifacts as this attempt's
    evidence.

    The pre-fix roles are additionally held to the `baseline_sha` the
    census row is about to record — an independent statement, because a
    producer record whose reference and `baseline_sha` disagreed would
    satisfy either one alone.

    A role the producer ran no batch for has a `null` reference, and a
    measurement supplied for it describes work the invocation did not
    do.
    """
    if measurement.result is None:
        # Exits 2 and 3 wrote no document, so there is nothing to bind.
        # `trustworthiness_problems` is what refuses them, as an
        # operational error rather than a malformed handoff.
        return
    reference = diagnosis["references"].get(measurement.role)
    if reference is None:
        raise HandoffError(
            f"the diagnosis outcome records no {measurement.role} batch, so "
            f"a {measurement.role} measurement here describes work the "
            f"invocation did not do")
    result = measurement.result
    for field in REFERENCE_FIELDS:
        if result[field] != reference[field]:
            raise HandoffError(
                f"the {measurement.role} measurement reports {field} "
                f"{result[field]!r} while the diagnosis outcome's "
                f"{measurement.role} reference names "
                f"{reference[field]!r}, so it is not the measurement that "
                f"diagnosis judged")
    if measurement.role in PRE_FIX_ROLES:
        if result["commit_sha"] != diagnosis["baseline_sha"]:
            raise HandoffError(
                f"the {measurement.role} measurement was taken at "
                f"{result['commit_sha']!r}, not at the diagnosis outcome's "
                f"baseline commit {diagnosis['baseline_sha']!r}; a pre-fix "
                f"measurement of another commit is not this attempt's "
                f"evidence")


def require_worktree_boundary(value, what: str) -> str:
    """One declared comparison worktree, as a boundary rather than a label.

    #1437 canonicalises both declared worktrees and requires each batch
    to have run inside the one its section names, so a real record
    carries the spelling `Path.resolve` produces. That rule is restated
    here — the producer exposes it publicly only through
    result-document validation — because this is the value the
    containment check is made AGAINST: a relative, `..`-bearing or
    symlinked spelling would compare as somewhere other than the place
    it names, and the boundary would quietly stop covering it.

    What this cannot establish is that the named directory ever WAS a
    worktree. Nothing else in the record identifies those directories,
    and by the time an outcome is recorded both have usually been
    removed, so a plausible substitute is indistinguishable from a
    truthful declaration. The declared worktrees are therefore an
    ADDITIONAL boundary the record supplies about itself: they can only
    ever add refusals, never lift one, and the live registered worktrees
    and the primary checkout — which no document can edit — are what
    the guarantee actually rests on.
    """
    text = _require_usable_path(
        _require_text(value, what, limit=4096), what)
    if not Path(text).is_absolute():
        raise HandoffError(
            f"{what} is the relative path {text!r}; a containment boundary "
            f"has to name one place, and a relative one names a different "
            f"place from every directory it is read in")
    resolved = str(Path(text).resolve())
    if resolved != text:
        raise HandoffError(
            f"{what} is {text!r}, which is not the spelling `Path.resolve` "
            f"produces ({resolved!r}); #1437 canonicalises both declared "
            f"worktrees, and a boundary compared in one spelling while the "
            f"paths it bounds are written in another covers nothing")
    return text


def declared_worktrees(document) -> list:
    """The comparison worktrees the producer record names.

    Collected BEFORE anything is admitted, and kept beside the live
    registered ones, because the workflow removes or hands off both
    comparison worktrees when it finishes: by the time an outcome is
    recorded the paths it correctly names may no longer be registered,
    and an artifact that sat inside one was still inside a worktree when
    it was written.

    REQUIRED of every batch the record says it ran, for the same reason:
    a section that declared no worktree would silently contribute no
    boundary, and the artifacts of a removed worktree would then be
    stored unbounded. Both are held to #1437's own rule that neither may
    contain the other, since two labels for one place are not two
    states.
    """
    trees = []
    for section in ("baseline", "verification"):
        record = document.get(section) if isinstance(document, dict) else None
        if not isinstance(record, dict):
            continue
        trees.append(require_worktree_boundary(
            record.get("worktree"),
            f"the diagnosis outcome's {section} `worktree`"))
    if len(trees) == 2:
        first, second = (Path(tree) for tree in trees)
        if (first == second or first in second.parents
                or second in first.parents):
            raise HandoffError(
                f"the diagnosis outcome declares the comparison worktrees "
                f"{trees[0]} and {trees[1]}, which are not two separate "
                f"states; the clean comparison worktree stays at the "
                f"baseline commit and the repair lives in its own")
    return trees


def _require_descriptor_record(value, what: str, expected_checks) -> list:
    """#1437's WHOLE expected descriptor, identifiers and labels.

    Required, and required to agree with the identifier list beside it:
    the record states the descriptor twice, and two statements free to
    disagree establish neither.
    """
    if not isinstance(value, list) or not value:
        raise HandoffError(
            f"{what} must be the handoff's ordered descriptor, got "
            f"{type(value).__name__}")
    descriptor = []
    for position, entry in enumerate(value):
        where = f"{what}[{position}]"
        record = _require_object(entry, where)
        if set(record) != {"id", "label"}:
            raise HandoffError(
                f"{where} carries {sorted(record)}, not exactly `id` and "
                f"`label`")
        descriptor.append({
            "id": _require_text(record["id"], f"{where}.id", limit=128),
            "label": _require_text(record["label"], f"{where}.label",
                                   limit=4096),
        })
    ids = [entry["id"] for entry in descriptor]
    if ids != list(expected_checks):
        raise HandoffError(
            f"{what} declares {ids} where the record's own "
            f"`expected_checks` is {list(expected_checks)}; one descriptor "
            f"stated twice cannot be two descriptors")
    return descriptor


def _require_one_descriptor(measurements: dict, diagnosis: dict) -> None:
    """Every declared measurement reports the contract the record names.

    A batch's descriptor is what it reports against, and
    `_bind_to_producer` cannot see it: a result can keep its probe, its
    targets, its commit, its instant and every artifact path while
    swapping or relabelling an unrelated declared check, and #1437
    rejects exactly that drift rather than routing it anywhere.

    Held to the RECORD's own ordered descriptor rather than to the other
    supplied measurements, through #1437's `require_descriptor` so
    identifiers, order AND labels are compared by one definition. The
    difference matters: `cannot-reproduce` and `no-confident-fix`
    normally supply a single baseline, so a comparison between
    measurements has nothing to compare it against, and a self-
    consistent handoff that relabelled a check would record a stable
    outcome — a de-list recommendation among them — for a different
    asserted check.
    """
    expected = diagnosis["expected_descriptor"]
    for role in ROLES:
        measurement = measurements.get(role)
        if measurement is None or measurement.result is None:
            continue
        try:
            deflake_contract.require_descriptor(
                measurement.result, expected,
                f"the {role} measurement's result document")
        except deflake_contract.HandoffError as error:
            raise HandoffError(str(error)) from None
