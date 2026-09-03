#!/usr/bin/env python3
"""CI-promotion candidate assessment and reporting (#1441, extracted #2034).

`docs/probe_census.json` is the de-flake lab's durable record and
`tools/probe_census.py` owns it: the schema and its migrations, the
validators, the reconciliation against the live registry, the lock, the
atomic write, the docs-worktree resolution, and every policy update. All
of that MUTATES the census.

This module mutates nothing. It is the read-only half: it reads a census
document that is already loaded and validated, decides which manual-only
probes their own measurements qualify for CI promotion, and renders that
decision as a report — a `probe-promotion-report/v1` object for `--json`
and a fixed-width table for a human. It never opens the manifest, never
resolves a worktree, never takes the lock and never writes a byte, so
`--promotion-candidates` is byte-preserving by construction rather than
by discipline.

Nothing here edits `tools/ci_probes.py` either -- see the comment on
`PROMOTION_SCHEMA` below for which half of a promotion this reports on
and which half stays a person's.

The dependency runs ONE WAY, and since #2131 it runs to the census
OWNERS rather than to their facade: this module imports
`probe_census_contract`, `probe_census_records` and
`probe_census_summary` at module scope for the symbols it reads, and
imports `tools/probe_census.py` not at all. That is what keeps the split
acyclic -- the facade imports every owner, so an owner importing the
facade would close the loop. `probe_census.main`'s
`--promotion-candidates` dispatch imports THIS module at its point of
use, following that file's own `import jsonschema  # noqa: PLC0415`
convention.

`probe_census.promotion_report` and `probe_census.render_promotion_report`
therefore no longer exist. Both in-repo callers -- that CLI dispatch and
the self-test -- name this module instead; a third consumer would import
it the same way.

This module has no CLI and is not a gate of its own. The one command is
still `python3 tools/probe_census.py --promotion-candidates`, whose
argument surface, exit codes, human rendering and `--json` structure are
unchanged by the extraction.
"""
from __future__ import annotations

import datetime
import math
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ci_probes  # noqa: E402
import probe_census_contract as census_contract  # noqa: E402
import probe_census_records as census_records  # noqa: E402
import probe_census_summary as census_summary  # noqa: E402
import probe_flake  # noqa: E402
import probe_protocol  # noqa: E402
# The one contract scalar this module names bare rather than through
# `census_contract.`: it reads as a local requirement at both call
# sites, and #2131 moved the definition down beside `require_count`
# rather than leaving a second scalar requirement up here.
from probe_census_contract import require_seconds  # noqa: E402

# Promotion has two halves, and this reports on exactly one of them.
# RELIABILITY is measurable, and measuring it is what the census is for.
# BREADTH, COST and runner support are judgements: whether a probe
# covers enough to be worth a slot on every matching PR, whether its
# wall time fits the gate's budget, and whether the CI runner can host
# it at all. Nothing here decides any of those, and nothing here edits
# `CI_ELIGIBLE` or `MANUAL_ONLY_REASONS` — a promotion stays a person's
# reviewed edit to `tools/ci_probes.py`, and this report is the evidence
# they read before making it.
PROMOTION_SCHEMA = "probe-promotion-report/v1"

# The buckets the report separates its rows into.
BUCKET_READY = "ready"
BUCKET_BLOCKED = "mechanically-blocked"

# The two manual-only grounds a MEASUREMENT can answer. `flaky` is the
# claim a clean cohort directly contradicts, and `unclassified` is the
# absence of any stated ground at all. Every other category names
# something the census cannot observe — a GPU the runner does not have,
# a worldgen the gate cannot afford, a scenario nobody wants on every
# PR, a probe deliberately aimed at one narrow question — so a probe
# carrying one is reported as mechanically blocked however clean its
# runs are.
#
# This is an ALLOWLIST, and that is load-bearing: a category added to
# `ci_probes.KNOWN_REASON_CATEGORIES` later, or one this file has never
# heard of, is absent from it and therefore BLOCKS. Failing closed is
# the only safe default for a report whose ready list is read as
# "nothing measurable stands in the way of promoting this".
READY_REASON_CATEGORIES = frozenset({ci_probes.FLAKY, ci_probes.UNCLASSIFIED})


def declared_reasons(probe: str, what: str) -> tuple:
    """The probe's manual-only reason records, read LIVE.

    `tools/ci_probes.py` is the authority here exactly as it is for
    `classification`: a census not yet reconciled by `--seed` still
    carries the column it was written with, and a report that trusted a
    stored copy would answer about the registry as it used to be.

    A probe with no entry yields an empty tuple rather than a refusal.
    `ci_probes._self_test` already requires every manual-only probe to
    have one, so this is the shape a registry mid-edit has — and an
    undeclared probe is BLOCKED below, never ready, so the empty tuple
    can only ever cost a promotion candidacy it has no grounds for.
    """
    reasons = ci_probes.MANUAL_ONLY_REASONS.get(probe)
    if reasons is None:
        return ()
    if (isinstance(reasons, (str, bytes))
            or not isinstance(reasons, (list, tuple))):
        raise census_contract.CensusError(
            f"{what} must be an ordered collection of reason records, got "
            f"{type(reasons).__name__}")
    return tuple(reasons)


def reason_rows(reasons, what: str) -> list[dict]:
    """Every declared reason, in DECLARED order, as JSON.

    Every one of them, never just the first: `MANUAL_ONLY_REASONS`
    records one entry per INDEPENDENT ground (#1440), and a probe held
    out of CI on both a `flaky` count and a `needs-gpu` one is only
    honestly described by both. Declared order is the stable per-probe
    order `ci_probes --status` renders in, so the two agree.
    """
    rows: list[dict] = []
    for position, record in enumerate(reasons):
        category = getattr(record, "category", None)
        if not isinstance(category, str) or not category:
            raise census_contract.CensusError(
                f"{what}[{position}] has no category string, so the report "
                f"cannot say what it blocks on")
        explanation = getattr(record, "explanation", None)
        rows.append({
            "category": category,
            "explanation": explanation if isinstance(explanation, str)
            else None,
        })
    return rows


def blocking_reason_categories(rows) -> list[str]:
    """The declared categories no measurement can clear, sorted.

    Sorted rather than declared-ordered because this is a SET of
    grounds: two probes blocked on the same ones should read
    identically whatever order their entries happen to be written in.
    The declared order is preserved in `reasons` beside it, which is
    where render order belongs.
    """
    return sorted({row["category"] for row in rows
                   if row["category"] not in READY_REASON_CATEGORIES})


def cohort_evidence(cohort, what: str) -> tuple:
    """`cohort_statistic` plus the fields a promotion decision needs.

    Four additions, each of which a pooled failure rate alone hides:

    - the pooled COMPLETED runs, so a reader can see how much of what
      was scheduled actually ran. It is REPORTING only: whether every
      sample finished its own runs is `incomplete_samples`' question,
      because pooled totals cancel a shortfall against an overrun;
    - the pooled TIMEOUT count, which `probe_flake` counts SEPARATELY
      from failures, so a cohort with zero failures can still have lost
      runs to the clock;
    - the MAXIMUM observed `worst_elapsed_seconds` across the cohort,
      which is an observation and is reported as one — never mixed
      with, or substituted for, the record's stored estimate;
    - `opened_at`, the EARLIEST contributing timestamp, which is what
      bounds attempt attribution below. `measured_at` is the latest and
      stays the freshness anchor.

    Both ends of that window come back as `datetime`s beside the
    statistic — `(evidence, anchor, opened_at)` — because both are
    compared against, and re-parsing a stamp this just formatted would
    be a second place for the two spellings to disagree.

    Everything `cohort_statistic` refuses, this refuses first: it is
    called for its validation as much as for its counts.
    """
    statistic, anchor = census_contract.cohort_statistic(cohort, what)
    completed = 0
    timeouts = 0
    worst = None
    earliest = None
    for position, sample in enumerate(cohort["samples"]):
        where = f"{what} sample {position}"
        completed += census_contract.require_count(
            sample.get("completed_runs"), f"{where} `completed_runs`")
        timeouts += census_contract.require_count(
            sample.get("timeout_count"), f"{where} `timeout_count`")
        observed = require_seconds(sample.get("worst_elapsed_seconds"),
                                   f"{where} `worst_elapsed_seconds`")
        if worst is None or observed > worst:
            worst = observed
        stamp = census_contract.parse_timestamp(
            sample.get("timestamp_utc"), f"{where} `timestamp_utc`")
        if earliest is None or stamp < earliest:
            earliest = stamp
    evidence = dict(statistic)
    evidence.update({
        "completed_runs": completed,
        "timeout_count": timeouts,
        "observed_worst_elapsed_seconds": worst,
        "opened_at": earliest.strftime(census_contract.TIMESTAMP_FORMAT),
    })
    return evidence, anchor, earliest


def incomplete_samples(cohort, what: str) -> list[int]:
    """The positions of samples that did not finish what they scheduled.

    PER SAMPLE, and that is the whole point: pooled totals cannot answer
    this question, because one sample's shortfall is cancelled by
    another's overrun. A cohort of a 9-of-10 and an 11-of-10 pools to a
    perfect 20 of 20 while containing a measurement that lost a run, and
    "every sample finished every run it scheduled" is the condition a
    promotion actually rests on.

    The comparison is inequality in BOTH directions. A sample reporting
    MORE completions than it requested is not a bonus, it is a count
    nothing could have produced — the intake path constrains neither
    against the other — so it disqualifies exactly as a shortfall does.
    """
    if not isinstance(cohort, dict):
        raise census_contract.CensusError(
            f"{what} must be an object, got {type(cohort).__name__}")
    samples = cohort.get("samples")
    if not isinstance(samples, list):
        raise census_contract.CensusError(f"{what} `samples` must be a list")
    positions: list[int] = []
    for position, sample in enumerate(samples):
        where = f"{what} sample {position}"
        if not isinstance(sample, dict):
            raise census_contract.CensusError(
                f"{where} is not an object, got {type(sample).__name__}")
        requested = census_contract.require_count(
            sample.get("requested_runs"), f"{where} `requested_runs`")
        completed = census_contract.require_count(
            sample.get("completed_runs"), f"{where} `completed_runs`")
        if completed != requested:
            positions.append(position)
    return positions


def unresolved_attempts(census, probe: str, cohort_commit: str,
                        opened_at) -> int:
    """Scheduled measurements this cohort cannot account for.

    A run that ended in a harness error appends an ATTEMPT and no
    sample, so the cohort's own counts cannot show it: ten scheduled
    runs of which one never reported look exactly like nine clean ones.
    A nonzero count here is what makes an otherwise spotless cohort
    incomplete, and an incomplete cohort is not promotion evidence.

    Attribution FAILS CLOSED. An attempt is excluded only when it is
    PROVABLY outside this cohort — a usable commit identity that is not
    the cohort's, or a usable timestamp strictly before the cohort
    opened. Anything unattributable counts: the `unknown` provenance a
    harness error legitimately carries, a malformed record, an
    unreadable stamp. "We cannot tell which cohort lost a run" is not
    evidence that this one did not.
    """
    attempts = census.get("attempts")
    if attempts is None:
        attempts = []
    if not isinstance(attempts, list):
        raise census_contract.CensusError(
            f"probe {probe!r}: `attempts` must be a list to read, got "
            f"{type(attempts).__name__}")
    counted = 0
    for attempt in attempts:
        if not isinstance(attempt, dict):
            counted += 1
            continue
        # `is True`, not truthiness: a malformed `accepted` is not a
        # statement that the measurement landed.
        if attempt.get("accepted") is True:
            continue
        try:
            commit = census_contract.require_commit_identity(
                attempt.get("commit_sha"), "attempt `commit_sha`")
        except census_contract.CensusError:
            commit = None
        if commit is not None and commit != cohort_commit:
            continue
        try:
            stamp = census_contract.parse_timestamp(
                attempt.get("timestamp_utc"), "attempt `timestamp_utc`")
        except census_contract.CensusError:
            stamp = None
        if stamp is not None and stamp < opened_at:
            continue
        counted += 1
    return counted


def promotion_row(entry, *, now, stale_after_seconds) -> dict | None:
    """One RELIABILITY-QUALIFIED row, or None when the probe is not one.

    Qualification is every measurable precondition the issue names, and
    each of them is a hard gate rather than a column a reader has to
    notice:

    - the probe is registered and LIVE-classified manual-only (a probe
      already in CI is not a promotion candidate, and one that has left
      the registry is not promotable at all);
    - its protocol is `probe-result/v1` — a legacy probe emits no
      structured result, so there is nothing to have measured;
    - its stored X is integer ZERO. Not "non-positive" and not "unset":
      X is the failures a complete ten-run measurement may show, and
      only a zero says the probe is expected to pass every run. A null
      X states no expectation at all and disqualifies exactly as an X
      above zero does;
    - it has a CURRENT cohort — an archived one is the newest statistic
      a promoted probe keeps, not evidence about the code as it stands;
    - that cohort is FRESH against the caller's horizon;
    - that cohort is COMPLETE: it reaches the policy's run count, every
      sample finished EXACTLY the runs it scheduled — checked per
      sample, since pooled totals cancel a shortfall against an
      overrun — and no unattributable or same-commit harness error sits
      in the attempt log for it;
    - and it shows ZERO failures and ZERO timeouts.

    A probe failing any of these is not reported at all, in either
    bucket. The two lists answer "what could a human promote?", and a
    row with nothing measured is not an answer to it.
    """
    if not isinstance(entry, dict):
        raise census_contract.CensusError(
            f"census entry must be an object, got {type(entry).__name__}")
    probe = entry.get("key")
    if not isinstance(probe, str):
        raise census_contract.CensusError(
            f"census entry has no string `key` ({entry.get('key')!r})")
    if probe not in census_records._registered_keys():
        return None
    live = census_records.classification(probe)
    if live != census_contract.MANUAL_ONLY:
        return None
    protocol = probe_flake.protocol_status(probe)
    if protocol != probe_protocol.PROTOCOL_VERSION:
        return None
    census = entry.get("census")
    if not isinstance(census, dict):
        raise census_contract.CensusError(
            f"probe {probe!r} has no census record to report on")
    acceptable = census.get("acceptable_failures")
    if (not census_records._is_x(acceptable)
            or acceptable != census_contract.MIN_ACCEPTABLE_FAILURES):
        return None
    cohort = census.get("current")
    if cohort is None:
        return None
    evidence, anchor, opened_at = cohort_evidence(
        cohort, f"probe {probe!r} current cohort")
    # Clamped at zero for `summarize_entry`'s reason: a cohort anchored
    # in the future is the freshest thing there is, never negatively old.
    age = max(0.0, (now - anchor).total_seconds())
    if age >= census_summary.require_horizon(stale_after_seconds):
        return None
    if evidence["requested_runs"] < census_contract.POLICY_RUN_COUNT:
        return None
    # PER SAMPLE, never the pooled totals: a 9-of-10 beside an 11-of-10
    # pools to a flawless 20 of 20 while holding a measurement that lost
    # a run. Per-sample equality implies the pooled equality, so this
    # replaces that comparison rather than joining it.
    if incomplete_samples(cohort, f"probe {probe!r} current cohort"):
        return None
    if unresolved_attempts(census, probe, evidence["commit_sha"], opened_at):
        return None
    if evidence["failure_count"] or evidence["timeout_count"]:
        return None
    estimate = census.get("estimated_worst_case_seconds")
    if estimate is not None:
        estimate = require_seconds(
            estimate, f"probe {probe!r} `estimated_worst_case_seconds`")
    reasons = reason_rows(
        declared_reasons(probe, f"probe {probe!r} manual-only reasons"),
        f"probe {probe!r} manual-only reasons")
    blocking = blocking_reason_categories(reasons)
    # An undeclared probe is blocked, not ready. Every category being
    # clearable is vacuously true of NO categories, and a probe whose
    # grounds nobody has written down is precisely the one a "nothing
    # stands in the way" list must not contain.
    ready = bool(reasons) and not blocking
    row = {
        "key": probe,
        "script": entry.get("script"),
        "classification": live,
        "protocol": protocol,
        "acceptable_failures": acceptable,
        "age_seconds": age,
        "estimated_worst_case_seconds": estimate,
        "reasons": reasons,
        "blocking_categories": blocking,
        "bucket": BUCKET_READY if ready else BUCKET_BLOCKED,
    }
    row.update(evidence)
    return row


def promotion_report(document, *, now, stale_after_seconds) -> dict:
    """The two lists a human reads before editing `tools/ci_probes.py`.

    `candidates` are reliability-qualified probes whose every declared
    ground a measurement CAN answer; `blocked` are reliability-qualified
    probes carrying at least one ground it cannot. The split is the
    whole point: a clean GPU probe is not a disappointment, it is a
    probe whose obstacle was never flakiness, and mixing the two lists
    would bury the handful of rows a person can actually act on.

    Every cardinality is DERIVED from the live registry — there is no
    frozen probe total anywhere here, so the report stays correct as
    probes are registered and promoted.

    `now` is supplied by the caller and never read from a clock here,
    for the same reason `census_summary` requires one.
    """
    if not isinstance(now, datetime.datetime) or now.tzinfo is None:
        raise census_contract.CensusError(
            "the evaluation time must be a timezone-aware datetime")
    horizon = census_summary.require_horizon(stale_after_seconds)
    rows = [promotion_row(entry, now=now, stale_after_seconds=horizon)
            for entry in census_records._rows(document, "census")]
    qualified = [row for row in rows if row is not None]
    registered = census_records._registered_keys()
    eligible = sorted(registered & set(ci_probes.CI_ELIGIBLE))
    return {
        "schema": PROMOTION_SCHEMA,
        "evaluated_at": now.strftime(census_contract.TIMESTAMP_FORMAT),
        "stale_after_seconds": horizon,
        "registered_probes": len(registered),
        "ci_eligible": len(eligible),
        "manual_only": len(registered) - len(eligible),
        "reliability_qualified": len(qualified),
        "candidates": [row for row in qualified
                       if row["bucket"] == BUCKET_READY],
        "blocked": [row for row in qualified
                    if row["bucket"] == BUCKET_BLOCKED],
    }


def _promotion_number(value, spec: str) -> str:
    """One optional number for the table, or the mark that it is unset.

    An absent estimate reads as `unset`, never as a zero and never as
    the observed duration beside it: the two answer different
    questions, and substituting one for the other is how a probe gets
    promoted against a budget nobody measured it for.
    """
    return "unset" if value is None else format(value, spec)


PROMOTION_HEADER = (f"{'probe':<34}{'runs':>6}{'fail':>6}{'t/o':>5}"
                    f"{'rate':>8}{'X':>4}{'age':>8}{'worst(s)':>11}"
                    f"{'est(s)':>11}  commit")


def _promotion_lines(rows: list[dict]) -> list[str]:
    """One row per probe, with every declared reason beneath it."""
    if not rows:
        return ["  (none)"]
    lines: list[str] = []
    for row in rows:
        rate = ("n/a" if row["failure_rate"] is None
                else f"{row['failure_rate'] * 100:.1f}%")
        observed = _promotion_number(
            row["observed_worst_elapsed_seconds"], ".1f")
        estimate = _promotion_number(
            row["estimated_worst_case_seconds"], ".1f")
        lines.append(
            f"{row['key']:<34}{row['requested_runs']:>6}"
            f"{row['failure_count']:>6}{row['timeout_count']:>5}{rate:>8}"
            f"{row['acceptable_failures']:>4}"
            f"{row['age_seconds'] / census_contract.SECONDS_PER_DAY:>7.1f}d"
            f"{observed:>11}{estimate:>11}  {row['commit_sha']}")
        for reason in row["reasons"] or [{"category": "none declared",
                                          "explanation": None}]:
            lines.append(f"    [{reason['category']}]  "
                         f"{reason['explanation'] or ''}".rstrip())
    return lines


def render_promotion_report(report: dict) -> str:
    """The human report. `--json` is the machine-readable form.

    Both lists are printed even when empty, and the counts above them
    are printed even when zero: "no candidates" is a result a person
    needs to see, and a section that vanishes reads as a report that
    did not run.
    """
    horizon = report["stale_after_seconds"] / census_contract.SECONDS_PER_DAY
    lines = [
        f"CI-promotion candidates as of {report['evaluated_at']} "
        f"(fresh within {horizon:.1f}d)",
        f"{report['registered_probes']} registered probes: "
        f"{report['ci_eligible']} {census_contract.CI_ELIGIBLE}, "
        f"{report['manual_only']} {census_contract.MANUAL_ONLY}; "
        f"{report['reliability_qualified']} reliability-qualified.",
        "",
        f"ready for breadth/cost review ({len(report['candidates'])}) — "
        f"a person decides breadth, cost, runner support and the actual "
        f"promotion",
        PROMOTION_HEADER,
        "-" * len(PROMOTION_HEADER),
    ]
    lines += _promotion_lines(report["candidates"])
    lines += [
        "",
        f"clean, but mechanically blocked ({len(report['blocked'])}) — "
        f"held out on a ground no measurement can clear",
        PROMOTION_HEADER,
        "-" * len(PROMOTION_HEADER),
    ]
    lines += _promotion_lines(report["blocked"])
    lines.append("")
    lines.append("This report edits nothing. Promotion is a reviewed edit "
                 "to tools/ci_probes.py.")
    return "\n".join(lines) + "\n"
