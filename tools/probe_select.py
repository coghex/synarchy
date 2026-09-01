#!/usr/bin/env python3
"""Which probe does `/deflake` measure next? (#1435)

The census (#1428/#1429), the acceptable-failure policy (#1430), the
in-flight check (#1433) and the atomic claim (#1434) each answer one
question about ONE probe. This answers the question that consumes all
four: given the whole roster, which single probe should the next
measurement spend its hour on?

It is a PURE DECISION over supplied inputs. Nothing here boots an
engine, runs a probe, reads a wall clock, resolves a docs worktree,
opens a socket, invokes `gh`, or touches the census file or the claim
lockfiles the prerequisites own. Every input — the registry, the two
classifications, protocol status, the census document, the in-flight
set, the claim set, the evaluation time and the age horizon — is passed
in, which is what makes the whole ladder assertable in milliseconds by
`tools/test_probe_select.py` and what keeps this component out of the
orchestration that #1436 owns: selection does not claim, launch,
record or release.

Borrowed vocabulary is consumed as-is
-------------------------------------
What a valid run is, what a failure is, which cohort is current, and
what makes a measurement stale are all #1429's definitions, reached
through `probe_census.summarize_entry` rather than restated here. N is
`probe_census.POLICY_RUN_COUNT` and each probe's X is the validated
`acceptable_failures` #1430 stores in its census record. This module
redefines none of them. Its only arithmetic is the failure-rate
comparison against X/N and the rung ranking below.

Eligibility, applied before any ranking
---------------------------------------
A probe is eligible only when it is registered, classified manual-only,
implements `probe-result/v1`, is not deferred in the census, is not in
flight, and is not claimed.
Every exclusion is applied before ranking, so an excluded probe cannot
win a rung however bad its numbers are.

Manual-only is decided by KEY MEMBERSHIP of the two classifications and
by nothing else. `ci_probes.MANUAL_ONLY_REASONS` maps a key to a tuple
of `Reason` records and #1440 made that tuple plural; the reasons are
documentation for humans and the census page, and no selection decision
may depend on their count, category, explanation or shape. A key in
neither classification, or in both, is excluded rather than adjudicated:
`ci_probes._self_test` owns that consistency, and excluding is the safe
direction for a component that would otherwise measure a probe CI
already covers.

Protocol status is likewise consumed, never inferred. A legacy probe is
skipped with a recorded `requires protocol migration` reason and is
never treated as merely unmeasured, because `probe_flake.resolve_probe`
would reject it before execution — ranking it first would select a
probe the harness refuses to run.

The priority ladder
-------------------
Every eligible probe falls into exactly one rung, or into none:

1. INCOMPLETE MEASUREMENT — no cohort at all, or fewer than N valid
   runs in it. Ranked fewest valid runs first, so an unmeasured probe
   (which ranks as zero valid runs) precedes every partial cohort.
2. OVER TOLERANCE — at least N valid runs and a failure rate strictly
   above X/N. Ranked highest failure rate first, then oldest
   measurement.
3. STALE WITHIN TOLERANCE — at least N valid runs, a failure rate at or
   below X/N, and stale against the supplied evaluation time and age
   horizon. Ranked oldest measurement first, then highest failure rate.

A fresh probe within tolerance is in no rung. That is the successful
terminal state — the roster is healthy — rather than a fourth,
lowest-priority rung, and it is recorded as a skip reason so a caller
can tell it apart from a roster where everything happened to be claimed.

Every remaining tie breaks lexicographically on the exact registered
key. Registration order, display purpose, script filename and census
array order affect nothing: `probe_runner_registry.PROBES` registers
`blood_gpu_lifecycle` ahead of `bleeding_trail` while the two sort the
other way round, so an implementation that iterated the registry would
pass a census-order-only test and still be wrong.

Rate comparison is EXACT
------------------------
Tolerance is decided as `failures * N > X * valid_runs`, in integers,
so a cohort sitting exactly on X/N is within tolerance whatever its
size: 3 failures in 30 valid runs against X=1 is at the threshold, not
over it, and no float rounding decides it. The rate itself is carried
as a `fractions.Fraction` for the same reason. A cohort is only ever
divided once it has at least N valid runs, so a zero-run cohort is
classified into rung 1 without any rate arithmetic at all.

Three outcomes, and the reasons beside them
-------------------------------------------
`select_next_probe` returns exactly one of:

* `selected` — one registered probe key, with the rung it won;
* `no-candidate` — a valid census in which nothing qualifies;
* `malformed-census` — census data that cannot be classified or ranked.

The reason channel never changes WHICH outcome comes back. `no-candidate`
is returned both when every eligible probe is fresh within tolerance and
when every probe was excluded; `Selection.skipped` carries the recorded
reasons that distinguish "the roster is healthy" from "everything is
claimed", which is the difference between stopping and waiting.

Malformed versus ignored
------------------------
A structurally broken census container is always the error outcome. A
present record for a REGISTERED, MANUAL-ONLY probe that is missing a
field classification or ranking needs is also the error outcome —
regardless of whether that probe is legacy, in flight or claimed,
because transient external state must never hide corrupt persistent
data. A record keyed to a probe outside that domain, unregistered or
currently CI-eligible, is ignored rather than an error: reconciling the
census against the live registry is `probe_census.validate_manifest`
and the census page's job, not the selector's.

An ABSENT record is not malformed at all. A registered manual-only
probe with no census row has never been measured, which is rung 1's
first and most common case.
"""
from __future__ import annotations

import datetime
import os
import sys
from dataclasses import dataclass
from fractions import Fraction

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_census  # noqa: E402
import probe_protocol  # noqa: E402

# N, and X's bounds, come from #1430's policy module. They are imported
# rather than restated because a selector that carried its own copy
# could silently disagree with the census that stores the numbers.
POLICY_RUN_COUNT = probe_census.POLICY_RUN_COUNT

# The protocol a measurable probe must implement. Taken from #1425's
# own module, so this cannot drift from what `probe_flake.protocol_status`
# reports and the supplied status map is compared against.
PROTOCOL_VERSION = probe_protocol.PROTOCOL_VERSION

OUTCOME_SELECTED = "selected"
OUTCOME_NO_CANDIDATE = "no-candidate"
OUTCOME_MALFORMED = "malformed-census"

RUNG_INCOMPLETE = 1
RUNG_OVER_TOLERANCE = 2
RUNG_STALE = 3

# Every recorded reason a registered probe is not the selection. The
# first six are exclusions applied before ranking; the last is the
# healthy terminal state, and is the one that makes `no-candidate`
# readable.
REASON_CI_ELIGIBLE = "ci-eligible"
REASON_UNCLASSIFIED = "not classified manual-only"
REASON_LEGACY = "requires protocol migration"
REASON_DEFERRED = "deferred"
REASON_IN_FLIGHT = "work already in flight"
REASON_CLAIMED = "claimed by another agent"
REASON_FRESH = "fresh within tolerance"

# The order exclusions are recorded in when a probe earns several. Fixed
# so the reason list is deterministic; it carries no precedence meaning,
# because every one of them excludes on its own.
EXCLUSION_ORDER = (REASON_CI_ELIGIBLE, REASON_UNCLASSIFIED, REASON_LEGACY,
                   REASON_DEFERRED, REASON_IN_FLIGHT, REASON_CLAIMED)
_EXCLUSION_POSITION = {reason: position
                       for position, reason in enumerate(EXCLUSION_ORDER)}


class SelectionError(Exception):
    """A supplied input that is the CALLER's own, and is unusable.

    Deliberately not one of the three outcomes: those classify the
    CENSUS, and a naive evaluation time or a negative horizon is a
    programming error in the consumer rather than a fact about the
    stored data. `probe_census.census_summary` refuses the same two the
    same way.
    """


@dataclass(frozen=True)
class Candidate:
    """One eligible probe, its rung, and the numbers that ranked it.

    `valid_runs` and `failure_count` are #1429's pooled totals for the
    authoritative cohort; `failure_rate` is their exact quotient, and is
    None exactly when the cohort has no runs to divide by. `measured_at`
    is the cohort's own freshness anchor in `probe_census`'s fixed-width
    UTC spelling, so ordering it as a string is ordering it in time.
    """

    key: str
    rung: int
    valid_runs: int
    failure_count: int
    acceptable_failures: int | None
    failure_rate: Fraction | None
    measured_at: str | None
    age_seconds: float | None
    stale: bool | None


@dataclass(frozen=True)
class Skip:
    """One registered probe that is not the selection, and why.

    `reasons` holds every ground that applied, not merely the first, so
    a caller can see that a probe is both legacy AND claimed.
    """

    key: str
    reasons: tuple[str, ...]


@dataclass(frozen=True)
class Selection:
    """The whole decision: one outcome, and everything that produced it."""

    outcome: str
    probe: str | None = None
    rung: int | None = None
    candidates: tuple[Candidate, ...] = ()
    skipped: tuple[Skip, ...] = ()
    error: str | None = None


def require_evaluation_time(now):
    """An aware evaluation instant, or a controlled refusal.

    Staleness is a function of a supplied time, never of a clock read
    here: a component that sampled `utcnow()` could not be asserted
    against, only raced.
    """
    if not isinstance(now, datetime.datetime) or now.tzinfo is None:
        raise SelectionError(
            "the evaluation time must be a timezone-aware datetime, got "
            f"{now!r}")
    return now


def require_horizon(stale_after_seconds):
    """#1429's age horizon, refused by #1429's own rule."""
    try:
        return probe_census.require_horizon(stale_after_seconds)
    except probe_census.CensusError as error:
        raise SelectionError(str(error)) from None


def _keys(container, what: str) -> frozenset:
    """A membership set from a mapping or any iterable of keys.

    Values are never read. `MANUAL_ONLY_REASONS` maps each key to a
    tuple of `Reason` records and #1440 made that tuple plural; taking
    only the keys is what keeps eligibility independent of the reason
    count, category, explanation and shape.
    """
    if container is None:
        return frozenset()
    if isinstance(container, (str, bytes)):
        # `frozenset("role")` is four one-character keys, which would
        # exclude nothing and match nothing while looking like a set.
        raise SelectionError(
            f"{what} must be a mapping or an iterable of probe keys, not a "
            f"single string: {container!r}")
    if isinstance(container, dict):
        return frozenset(container.keys())
    try:
        return frozenset(container)
    except TypeError:
        raise SelectionError(
            f"{what} must be a mapping or an iterable of probe keys, got "
            f"{type(container).__name__}") from None


def _registry_keys(registry) -> tuple[str, ...]:
    """Every registered key, in ascending key order.

    Registration order is discarded HERE rather than relied on not to
    matter later, so no downstream ordering can inherit it.
    """
    keys = _keys(registry, "the probe registry")
    for key in keys:
        if not isinstance(key, str):
            raise SelectionError(
                f"the probe registry holds a non-string key: {key!r}")
    return tuple(sorted(keys))


def _protocol_map(protocol) -> dict:
    """The supplied protocol classification, checked once.

    Nothing here inspects a script, a probe's output, or the census's
    own protocol column: the classification `probe_flake.protocol_status`
    reports is consumed, never re-derived.
    """
    if protocol is None:
        return {}
    if not isinstance(protocol, dict):
        raise SelectionError(
            "the protocol classification must be a mapping of probe key to "
            f"status, got {type(protocol).__name__}")
    return protocol


def _protocol_of(protocol: dict, key: str) -> str:
    """`key`'s protocol status; an absent entry reads as `legacy`.

    Exactly what `probe_flake.protocol_status` reports for a probe no
    migration has reached.
    """
    return protocol.get(key, probe_census.LEGACY)


def _exclusions(key: str, *, manual_only, ci_eligible, protocol, in_flight,
                claimed, deferred) -> tuple[str, ...]:
    """Every ground on which `key` is excluded before ranking."""
    reasons = []
    if key in ci_eligible:
        reasons.append(REASON_CI_ELIGIBLE)
    elif key not in manual_only:
        # In neither classification. `ci_probes._self_test` guarantees
        # this cannot happen live; a selector that measured it anyway
        # would be acting on a roster nobody classified.
        reasons.append(REASON_UNCLASSIFIED)
    if _protocol_of(protocol, key) != PROTOCOL_VERSION:
        reasons.append(REASON_LEGACY)
    if key in deferred:
        reasons.append(REASON_DEFERRED)
    if key in in_flight:
        reasons.append(REASON_IN_FLIGHT)
    if key in claimed:
        reasons.append(REASON_CLAIMED)
    return tuple(sorted(reasons, key=_EXCLUSION_POSITION.__getitem__))


def _census_rows(census) -> list:
    """The census document's entry list, or a controlled refusal.

    This is the whole of the container check. It deliberately does NOT
    schema-validate the document: full validation would also reject a
    malformed record for an unregistered or CI-eligible probe, which is
    the census page audit's finding to report and not a reason to refuse
    a selection that never reads it.
    """
    if not isinstance(census, dict):
        raise probe_census.CensusError(
            f"the census must be a JSON object, got {type(census).__name__}")
    rows = census.get("probes")
    if not isinstance(rows, list):
        raise probe_census.CensusError(
            "the census `probes` must be a list, got "
            f"{type(rows).__name__}")
    return rows


def _in_domain_records(census, domain: frozenset) -> dict:
    """Each in-domain probe's census row, keyed by probe.

    `domain` is the registered, manual-only key set — the only records
    this selector reads at all. A duplicate row for one of those keys is
    malformed rather than first-wins: two records for one probe leave
    ranking data ambiguous, and silently preferring one is guessing.
    """
    rows = _census_rows(census)
    records: dict = {}
    duplicates = set()
    for position, row in enumerate(rows):
        if not isinstance(row, dict):
            raise probe_census.CensusError(
                f"census entry {position} is not an object, got "
                f"{type(row).__name__}")
        key = row.get("key")
        if not isinstance(key, str):
            raise probe_census.CensusError(
                f"census entry {position} has no string `key` "
                f"({row.get('key')!r})")
        if key not in domain:
            # Unregistered, or currently CI-eligible: outside what this
            # selector may choose from, so its record is not read.
            continue
        if key in records:
            duplicates.add(key)
        records[key] = row
    if duplicates:
        raise probe_census.CensusError(
            "the census holds more than one record for "
            f"{', '.join(repr(key) for key in sorted(duplicates))}, so its "
            f"ranking data is ambiguous")
    return records


def _summarize(records: dict, *, now, horizon) -> dict:
    """#1429's selection-facing view of every in-domain record.

    Summarizing happens for every in-domain record BEFORE any exclusion
    is consulted, so a corrupt record still reports as corrupt when the
    probe it belongs to happens to be legacy, in flight or claimed.
    Records are visited in ascending key order, so which corruption is
    reported first does not depend on census array order either.
    """
    summaries = {}
    for key in sorted(records):
        summary = probe_census.summarize_entry(
            records[key], now=now, stale_after_seconds=horizon)
        runs = summary["requested_runs"] or 0
        if runs >= POLICY_RUN_COUNT:
            # A cohort large enough to be judged against X needs a
            # usable X. Missing or out-of-range here is corrupt stored
            # policy, not a probe to quietly pass over.
            probe_census.require_acceptable_failures(
                summary["acceptable_failures"],
                f"probe {key!r} `acceptable_failures`")
        summaries[key] = summary
    return summaries


def _candidate(key: str, summary) -> Candidate | None:
    """`key`'s rung and ranking numbers, or None when it does not qualify.

    An absent summary is an unmeasured probe: rung 1 at zero valid runs,
    ahead of every partial cohort. That is the first census run's
    commonest state, so it needs a defined sort position rather than
    only a defined membership.
    """
    if summary is None or not summary["measured"]:
        return Candidate(key=key, rung=RUNG_INCOMPLETE, valid_runs=0,
                         failure_count=0, acceptable_failures=None,
                         failure_rate=None, measured_at=None,
                         age_seconds=None, stale=None)
    runs = summary["requested_runs"] or 0
    failures = summary["failure_count"] or 0
    acceptable = summary["acceptable_failures"]
    common = {
        "key": key,
        "valid_runs": runs,
        "failure_count": failures,
        "acceptable_failures": acceptable,
        "measured_at": summary["measured_at"],
        "age_seconds": summary["age_seconds"],
        "stale": summary["stale"],
    }
    if runs < POLICY_RUN_COUNT:
        # Includes a cohort of zero valid runs, which is why no rate is
        # computed before this branch: there is nothing to divide by.
        return Candidate(rung=RUNG_INCOMPLETE, failure_rate=None, **common)
    rate = Fraction(failures, runs)
    if failures * POLICY_RUN_COUNT > acceptable * runs:
        return Candidate(rung=RUNG_OVER_TOLERANCE, failure_rate=rate, **common)
    if summary["stale"]:
        return Candidate(rung=RUNG_STALE, failure_rate=rate, **common)
    return None


def _rank(candidates: list) -> tuple:
    """Every candidate in selection order, best first.

    Each rung is sorted by its OWN rule and the rungs are concatenated,
    so no lower-rung comparison can ever outrank a higher rung. Every
    rule ends in the registered key, which is total, so the order is
    complete and deterministic.
    """
    def incomplete(candidate):
        return (candidate.valid_runs, candidate.key)

    def over_tolerance(candidate):
        return (-candidate.failure_rate, candidate.measured_at, candidate.key)

    def stale(candidate):
        return (candidate.measured_at, -candidate.failure_rate, candidate.key)

    ranked = []
    for rung, rule in ((RUNG_INCOMPLETE, incomplete),
                       (RUNG_OVER_TOLERANCE, over_tolerance),
                       (RUNG_STALE, stale)):
        ranked.extend(sorted(
            (candidate for candidate in candidates if candidate.rung == rung),
            key=rule))
    return tuple(ranked)


def select_next_probe(*, registry, ci_eligible, manual_only, protocol, census,
                      in_flight=(), claimed=(), now,
                      stale_after_seconds=probe_census.DEFAULT_STALE_AFTER_SECONDS
                      ) -> Selection:
    """The one probe `/deflake` should measure next, or why there is none.

    `registry` is the probe registry — a mapping of registered key to
    script, as `probe_flake.registered_scripts()` returns, or any
    iterable of keys. Only the keys are read: script filenames and
    display purposes name nothing this selector decides.

    `ci_eligible` and `manual_only` are the two classifications, read as
    key sets. `protocol` maps a registered key to the protocol status
    `probe_flake.protocol_status` reports for it; an absent key reads as
    legacy.

    `census` is the census document. `in_flight` and `claimed` are the
    key sets #1433 and #1434 supply. Membership in `in_flight` means "do
    not select this probe": #1433's fail-closed rule, that a
    `source-error` is not a `clear`, is applied by the CALLER when it
    builds the set, because re-adjudicating another component's verdict
    is exactly the redefinition this selector refuses. Keys in either
    set that are not registered are simply irrelevant.

    `now` and `stale_after_seconds` are #1429's evaluation time and age
    horizon, supplied rather than sampled.
    """
    moment = require_evaluation_time(now)
    horizon = require_horizon(stale_after_seconds)
    ci_keys = _keys(ci_eligible, "the CI-eligible classification")
    manual_keys = _keys(manual_only, "the manual-only classification")
    in_flight_keys = _keys(in_flight, "the in-flight set")
    claimed_keys = _keys(claimed, "the claim set")
    protocol_map = _protocol_map(protocol)
    keys = _registry_keys(registry)

    domain = frozenset(
        key for key in keys if key in manual_keys and key not in ci_keys)
    try:
        summaries = _summarize(_in_domain_records(census, domain),
                               now=moment, horizon=horizon)
    except probe_census.CensusError as error:
        return Selection(outcome=OUTCOME_MALFORMED, error=str(error))

    deferred_keys = frozenset(
        key for key, summary in summaries.items()
        if summary.get("deferred") is not None)
    candidates = []
    skipped = []
    for key in keys:
        reasons = _exclusions(key, manual_only=manual_keys,
                              ci_eligible=ci_keys, protocol=protocol_map,
                              in_flight=in_flight_keys, claimed=claimed_keys,
                              deferred=deferred_keys)
        if reasons:
            skipped.append(Skip(key=key, reasons=reasons))
            continue
        candidate = _candidate(key, summaries.get(key))
        if candidate is None:
            skipped.append(Skip(key=key, reasons=(REASON_FRESH,)))
            continue
        candidates.append(candidate)

    ranked = _rank(candidates)
    if not ranked:
        return Selection(outcome=OUTCOME_NO_CANDIDATE,
                         skipped=tuple(skipped))
    winner = ranked[0]
    return Selection(outcome=OUTCOME_SELECTED, probe=winner.key,
                     rung=winner.rung, candidates=ranked,
                     skipped=tuple(skipped))


def live_inputs() -> dict:
    """The registry-derived inputs, from the modules that own them.

    A thin adapter and nothing more: it names which component supplies
    each classification so a consumer cannot invent a second source for
    one. The census, the in-flight set, the claim set and the evaluation
    time stay the caller's to supply — they are state, not registry
    facts, and #1436 owns reading them.

    Imported here rather than at module scope so the decision above
    stays importable, and testable, without the probe registry.
    """
    import ci_probes
    import probe_flake

    registry = probe_flake.registered_scripts()
    return {
        "registry": registry,
        "ci_eligible": frozenset(ci_probes.CI_ELIGIBLE),
        "manual_only": frozenset(ci_probes.MANUAL_ONLY_REASONS),
        "protocol": {key: probe_flake.protocol_status(key)
                     for key in registry},
    }
