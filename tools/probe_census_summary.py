#!/usr/bin/env python3
"""The census reader: which cohort counts, how old it is, what it says (#2131).

`probe_census_contract.py` owns one cohort's arithmetic and
`probe_census_records.py` owns every transformation that produces a
cohort. This module is the layer above both, and it MUTATES NOTHING: it
ranks a record's retained cohorts, decides which one is authoritative,
measures its age against a supplied horizon, and assembles the per-entry
and whole-census summaries `--summary`, `tools/probe_select.py` and the
promotion report all read.

Staleness is purely age-based, and both of its inputs are arguments
rather than ambient state. The evaluation moment is the caller's `now`
and the horizon is the caller's `stale_after_seconds`, so a report is
reproducible and a test pins a clock instead of racing one. Nothing here
reads the wall clock; `tools/test_probe_select.py`'s tripwire holds this
module to that.

The authoritative cohort is `current` when the record carries one and
the final `history` entry otherwise. That second case is real rather
than defensive: `reconcile_inventory` archives `current` when a probe is
promoted to CI eligibility and deliberately does not restore it on a
later downgrade, so a promoted probe's newest measured statistic lives
in `history[-1]`. Only a record with neither is UNMEASURED, and an
unmeasured probe reports null measurements — never a zero failure rate,
which is a real and very different observation.

This module has no CLI and is not a gate of its own. Every command is
still `python3 tools/probe_census.py`.
"""
from __future__ import annotations

import datetime
import math
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probe_census_contract import (  # noqa: E402
    COHORT_CURRENT, COHORT_HISTORY, CensusError, TOLERANCE_NOT_COMPARABLE,
    cohort_statistic,
)
from probe_census_records import (  # noqa: E402
    _rows, policy_sample, require_deferral, target_row, tolerance_state,
)


def authoritative_cohort(census, probe: str):
    """The newest retained cohort and where it lives, or None.

    `current` when the record carries one; otherwise the final
    append-ordered `history` entry, which is where a probe promoted to
    CI eligibility keeps its newest measured statistic. None means
    UNMEASURED — neither a current cohort nor a single archived one.
    """
    if not isinstance(census, dict):
        raise CensusError(
            f"probe {probe!r} has no census record to summarize")
    current = census.get("current")
    if current is not None:
        return current, COHORT_CURRENT
    history = census.get("history")
    if history is None:
        history = []
    if not isinstance(history, list):
        raise CensusError(
            f"probe {probe!r}: `history` must be a list, got "
            f"{type(history).__name__}")
    if not history:
        return None
    return history[-1], COHORT_HISTORY


def require_horizon(stale_after_seconds):
    """A usable nonnegative age horizon, or a controlled refusal."""
    if isinstance(stale_after_seconds, bool) or not isinstance(
            stale_after_seconds, (int, float)):
        raise CensusError(
            f"the staleness horizon must be a number of seconds, got "
            f"{type(stale_after_seconds).__name__}")
    if not math.isfinite(stale_after_seconds) or stale_after_seconds < 0:
        raise CensusError(
            f"the staleness horizon must be a finite nonnegative number of "
            f"seconds, got {stale_after_seconds!r}")
    return stale_after_seconds


def summarize_entry(entry, *, now, stale_after_seconds) -> dict:
    """The selection-facing view of ONE census row.

    `measured` is the field that separates "this probe has never been
    measured" from "this probe was measured and never failed"; every
    measurement field of an unmeasured probe is null, so a zero rate
    can only ever mean an observed zero. `stale` is a property of a
    cohort, so it is null — not True — when there is no cohort: absent
    data is a different selection input from old data.

    `age_seconds` is clamped at zero. A cohort anchored in the future
    (a skewed clock, an injected evaluation time) is the freshest thing
    there is, never negative, and a negative age would sort ahead of
    every real measurement.

    `tolerance` reports the record's own X against ONE measurement
    (#1430) — the authoritative cohort's last-appended complete
    `POLICY_RUN_COUNT`-run sample, never its pooled totals, which are
    the right basis for `failure_rate` and the wrong one for a
    fixed-N threshold. It is `not-comparable` when the record has no
    usable X or that cohort holds no such measurement, an unmeasured
    probe included.
    """
    if not isinstance(entry, dict):
        raise CensusError(
            f"census entry must be an object, got {type(entry).__name__}")
    probe = entry.get("key")
    if not isinstance(probe, str):
        raise CensusError(
            f"census entry has no string `key` ({entry.get('key')!r})")
    horizon = require_horizon(stale_after_seconds)
    census = entry.get("census")
    acceptable = (census.get("acceptable_failures")
                  if isinstance(census, dict) else None)
    deferred = require_deferral(
        census.get("deferred") if isinstance(census, dict) else None,
        f"probe {probe!r} `deferred`")
    summary = {
        "key": probe,
        "script": entry.get("script"),
        "classification": entry.get("classification"),
        "acceptable_failures": acceptable,
        "deferred": deferred,
        # A record with no measurement is compared against nothing, so
        # the policy neither passes nor breaches: it does not apply yet.
        "tolerance": TOLERANCE_NOT_COMPARABLE,
        "measured": False,
        "cohort": None,
        "commit_sha": None,
        "measured_at": None,
        "age_seconds": None,
        "stale": None,
        "sample_count": None,
        "requested_runs": None,
        "failure_count": None,
        "failure_rate": None,
    }
    located = authoritative_cohort(census, probe)
    if located is None:
        return summary
    cohort, source = located
    statistic, anchor = cohort_statistic(
        cohort, f"probe {probe!r} {source} cohort")
    age = max(0.0, (now - anchor).total_seconds())
    summary.update(statistic)
    summary["measured"] = True
    summary["cohort"] = source
    summary["age_seconds"] = age
    summary["stale"] = age >= horizon
    # ONE measurement, never the cohort's pooled totals: X is stated
    # against a complete ten-run run, so `statistic`'s combined counts
    # are the wrong basis for it even though they are the right one for
    # the rate beside it.
    measurement = policy_sample(cohort)
    if measurement is not None:
        summary["tolerance"] = tolerance_state(
            acceptable, measurement["requested_runs"],
            measurement["completed_runs"], measurement["failure_count"])
    return summary


def census_summary(document, *, now, stale_after_seconds,
                   probe: str | None = None) -> list[dict]:
    """The selection-facing view of the whole census, or of one probe.

    `now` is supplied by the caller and never read from the wall clock
    here: staleness is a function of an evaluation time, and a test
    that injects one is testing the classification rather than racing
    it.
    """
    if not isinstance(now, datetime.datetime) or now.tzinfo is None:
        raise CensusError(
            "the evaluation time must be a timezone-aware datetime")
    horizon = require_horizon(stale_after_seconds)
    entries = _rows(document, "census")
    if probe is not None:
        entries = [target_row(document, probe, "--summary")]
    return [summarize_entry(entry, now=now, stale_after_seconds=horizon)
            for entry in entries]
