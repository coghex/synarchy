#!/usr/bin/env python3
"""Focused self-test for the next-probe priority ladder (#1435).

Deterministic, engine-free, GPU-free, offline and instantaneous. Every
case drives the shipped `tools/probe_select.py` against SYNTHETIC
registries, classifications, protocol maps and census documents held
entirely in memory. Nothing here boots an engine, runs a registered
probe, opens a socket, invokes `gh`, resolves a docs worktree, or reads
the real census file or the real claim lockfiles.

THE FIXTURES ARE THE POINT. Not one case asserts against the live
contents of `probe_runner_registry.PROBES`, `ci_probes.CI_ELIGIBLE` or
`ci_probes.MANUAL_ONLY_REASONS`. CLAUDE.md's CI-promotion procedure is
exactly "move its key from MANUAL_ONLY_REASONS to CI_ELIGIBLE", and
`ci_probes._self_test` already requires every newly registered probe to
join one of the two, so a gate pinned to live membership would redden on
unrelated registry work. The selector takes all of it as supplied
inputs, and these fixtures supply it.

Two contracts are proved MECHANICALLY rather than inferred:

* PURITY. `subprocess.run`/`Popen`, `socket.socket`,
  `socket.create_connection`, `builtins.open`, `os.replace`,
  `time.time`/`time.monotonic` and `datetime.datetime.now`/`utcnow` are
  all tripwires for the whole run. The clock pair is installed by
  swapping the `datetime` module object inside `probe_select` and
  `probe_census` for a namespace whose `datetime` refuses `now`, keeps
  `strptime`, and delegates `isinstance` to the real class — so a
  selector that sampled a clock instead of using the supplied evaluation
  time FAILS rather than passing on whatever today happens to be.

* DETERMINISM. Registration order, script filenames and census array
  order are all varied while the numbers stay fixed, and the selection
  must not move. The tie fixture registers `blood_gpu_lifecycle` ahead
  of `bleeding_trail` because the two orders genuinely disagree there —
  lexicographically `bleeding_trail` sorts first — so an implementation
  that iterated the registry would pass a census-order-only test and
  still be wrong. The two keys are used as FIXTURE strings; nothing
  reads the live registry to find them.

Usage:
  python3 tools/test_probe_select.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import builtins
import datetime
import os
import socket
import subprocess
import sys
import time
import types
from dataclasses import dataclass
from fractions import Fraction

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_census  # noqa: E402
import probe_census_contract  # noqa: E402
import probe_census_records  # noqa: E402
import probe_census_storage  # noqa: E402
import probe_census_summary  # noqa: E402
import probe_select as select  # noqa: E402

FAILURES: list[str] = []
PASSED = 0

NOW = datetime.datetime(2026, 8, 22, 12, 0, 0, tzinfo=datetime.timezone.utc)
HORIZON = probe_census.DEFAULT_STALE_AFTER_SECONDS
N = probe_census.POLICY_RUN_COUNT

COMMIT_A = "a" * 40
COMMIT_B = "b" * 40

# Fixed-width UTC stamps, oldest first. `probe_census.TIMESTAMP_FORMAT`
# sorts lexicographically in time, which is what the ranking relies on.
LONG_AGO = "2026-01-01T00:00:00Z"
OLD = "2026-06-01T00:00:00Z"
RECENT = "2026-08-01T00:00:00Z"
JUST_NOW = "2026-08-22T11:00:00Z"


# --------------------------------------------------------------------------
# Harness
# --------------------------------------------------------------------------

def check(condition: bool, label: str, detail: str = "") -> None:
    global PASSED
    if condition:
        PASSED += 1
        return
    FAILURES.append(f"{label}{': ' + detail if detail else ''}")


def check_equal(actual: object, expected: object, label: str) -> None:
    check(actual == expected, label, f"expected {expected!r}, got {actual!r}")


class Tripwire(AssertionError):
    """Raised when the selector reaches a forbidden interaction."""


class _DatetimeMeta(type):
    """`isinstance(x, shim)` still means `isinstance(x, datetime.datetime)`.

    Without this the shim would break every aware-datetime check in
    `probe_census` and the tripwire would prove nothing about clocks.
    """

    def __instancecheck__(cls, instance) -> bool:
        return isinstance(instance, datetime.datetime)


class NoClockDatetime(metaclass=_DatetimeMeta):
    """Every constructor the selection path legitimately uses, minus the clock."""

    strptime = staticmethod(datetime.datetime.strptime)

    @staticmethod
    def now(*args: object, **kwargs: object):
        raise Tripwire("the selector read the wall clock via datetime.now")

    @staticmethod
    def utcnow(*args: object, **kwargs: object):
        raise Tripwire("the selector read the wall clock via datetime.utcnow")


NO_CLOCK = types.SimpleNamespace(datetime=NoClockDatetime,
                                 timezone=datetime.timezone,
                                 timedelta=datetime.timedelta)

#: Every module the census was split into by #2131, plus the facade. The
#: tripwire has to reach whichever of them the selection path's census
#: calls resolve `datetime` in -- `summarize_entry` and `require_horizon`
#: are the summary owner's, and the `parse_timestamp` underneath them is
#: the contract owner's -- and patching the facade alone would silently
#: stop covering the census path the moment a function moved. Listing the
#: whole family and patching every member that HAS a clock keeps that
#: from being a thing anyone has to remember.
CENSUS_MODULES = (probe_census, probe_census_contract, probe_census_records,
                  probe_census_storage, probe_census_summary)


class Pure:
    """Forbid processes, sockets, files and clocks for the whole run."""

    def __enter__(self) -> "Pure":
        def forbidden(what: str):
            def guard(*args: object, **kwargs: object):
                raise Tripwire(f"the selector {what}: {args!r}")
            return guard

        self._saved = {
            "run": subprocess.run,
            "popen": subprocess.Popen,
            "socket": socket.socket,
            "connect": socket.create_connection,
            "open": builtins.open,
            "replace": os.replace,
            "time": time.time,
            "monotonic": time.monotonic,
            "select_datetime": select.datetime,
            "census_datetimes": [(module, module.datetime)
                                 for module in CENSUS_MODULES
                                 if hasattr(module, "datetime")],
        }
        subprocess.run = forbidden("started a subprocess")           # type: ignore[assignment]
        subprocess.Popen = forbidden("started a subprocess")         # type: ignore[assignment]
        socket.socket = forbidden("opened a socket")                 # type: ignore[assignment]
        socket.create_connection = forbidden("opened a socket")      # type: ignore[assignment]
        builtins.open = forbidden("opened a file")                   # type: ignore[assignment]
        os.replace = forbidden("replaced a file")                    # type: ignore[assignment]
        time.time = forbidden("read the wall clock")                 # type: ignore[assignment]
        time.monotonic = forbidden("read a clock")                   # type: ignore[assignment]
        select.datetime = NO_CLOCK                                   # type: ignore[assignment]
        # Non-vacuous by construction: a family that patched nothing
        # would be a tripwire covering no census module at all.
        assert self._saved["census_datetimes"], (
            "no census module exposes a clock to blind")
        for module, _clock in self._saved["census_datetimes"]:
            module.datetime = NO_CLOCK                           # type: ignore[assignment]
        return self

    def __exit__(self, *exc_info: object) -> bool:
        subprocess.run = self._saved["run"]                          # type: ignore[assignment]
        subprocess.Popen = self._saved["popen"]                      # type: ignore[assignment]
        socket.socket = self._saved["socket"]                        # type: ignore[assignment]
        socket.create_connection = self._saved["connect"]            # type: ignore[assignment]
        builtins.open = self._saved["open"]                          # type: ignore[assignment]
        os.replace = self._saved["replace"]                          # type: ignore[assignment]
        time.time = self._saved["time"]                              # type: ignore[assignment]
        time.monotonic = self._saved["monotonic"]                    # type: ignore[assignment]
        select.datetime = self._saved["select_datetime"]             # type: ignore[assignment]
        for module, clock in self._saved["census_datetimes"]:
            module.datetime = clock                              # type: ignore[assignment]
        return False


# --------------------------------------------------------------------------
# Fixtures
# --------------------------------------------------------------------------

@dataclass(frozen=True)
class FakeReason:
    """Shaped like `ci_probes.Reason`, and deliberately never read.

    #1440 made a manual-only value a TUPLE of these. Eligibility is key
    membership only, so a fixture carrying two of them, or none, must
    select exactly as a bare key set does.
    """

    category: str
    explanation: str


def registry(*keys: str) -> dict:
    """A registry mapping in the ORDER given, so registration order is testable."""
    return {key: f"{key}_probe.py" for key in keys}


def sample(*, runs: int, failures: int, at: str, commit: str = COMMIT_A) -> dict:
    return {"commit_sha": commit, "timestamp_utc": at,
            "requested_runs": runs, "completed_runs": runs,
            "failure_count": failures}


def cohort(samples: list, commit: str = COMMIT_A) -> dict:
    return {"commit_sha": commit, "samples": samples}


def measured(*, runs: int, failures: int, at: str, commit: str = COMMIT_A) -> dict:
    """A one-sample cohort — the ordinary shape of a single measurement."""
    return cohort([sample(runs=runs, failures=failures, at=at, commit=commit)],
                  commit)


def entry(key: str, *, x: int | None = 0, current=None, history=(),
          script: str | None = None, deferred=None) -> dict:
    """One census row, shaped exactly as `probe_census.empty_census` writes it."""
    record = {
        "acceptable_failures": x,
        "acceptable_failures_justification": None,
        "estimated_worst_case_seconds": None,
        "current": current,
        "history": list(history),
        "attempts": [],
        "claims": [],
        "outcomes": [],
        "deferred": deferred,
    }
    return {"key": key, "script": script or f"{key}_probe.py",
            "classification": probe_census.MANUAL_ONLY,
            "protocol": select.PROTOCOL_VERSION,
            "census": record}


def census(*entries: dict) -> dict:
    return {"schema": probe_census.CENSUS_SCHEMA, "probes": list(entries)}


def decide(*, keys, census_document, ci_eligible=(), manual_only=None,
           protocol=None, in_flight=(), claimed=(), now=NOW,
           stale_after_seconds=HORIZON, scripts=None):
    """Drive the shipped selector with every input supplied.

    `keys` may be a registry mapping (to fix registration order) or a
    plain sequence of keys. By default every key is manual-only and on
    the protocol, so a case only states the classification it is about.
    """
    if isinstance(keys, dict):
        table = keys
    else:
        table = registry(*keys)
    if scripts is not None:
        table = {key: scripts.get(key, script) for key, script in table.items()}
    every = list(table)
    if manual_only is None:
        manual_only = {key: (FakeReason("flaky", "fixture"),)
                       for key in every if key not in ci_eligible}
    if protocol is None:
        protocol = {key: select.PROTOCOL_VERSION for key in every}
    return select.select_next_probe(
        registry=table, ci_eligible=ci_eligible, manual_only=manual_only,
        protocol=protocol, census=census_document, in_flight=in_flight,
        claimed=claimed, now=now, stale_after_seconds=stale_after_seconds)


def reasons_for(selection, key: str) -> tuple:
    for skip in selection.skipped:
        if skip.key == key:
            return skip.reasons
    return ()


def ranked_keys(selection) -> list:
    return [candidate.key for candidate in selection.candidates]


# --------------------------------------------------------------------------
# Rung 1 — incomplete measurement
# --------------------------------------------------------------------------

def test_rung_one_in_isolation() -> None:
    """Fewest valid runs first, and an absent record ranks as zero."""
    document = census(
        entry("partial_three", current=measured(runs=3, failures=0, at=RECENT)),
        entry("partial_one", current=measured(runs=1, failures=0, at=RECENT)),
        entry("present_unmeasured"),
    )
    selection = decide(
        keys=("absent_record", "partial_one", "partial_three",
              "present_unmeasured"),
        census_document=document)
    check_equal(selection.outcome, select.OUTCOME_SELECTED,
                "an incomplete roster selects")
    check_equal(selection.rung, select.RUNG_INCOMPLETE,
                "every candidate is rung 1")
    check_equal(ranked_keys(selection),
                ["absent_record", "present_unmeasured", "partial_one",
                 "partial_three"],
                "rung 1 ranks fewest valid runs first, zero-run probes ahead "
                "of every partial cohort, ties broken by key")
    check_equal(selection.probe, "absent_record",
                "the absent record is the selection")
    check(all(candidate.rung == select.RUNG_INCOMPLETE
              for candidate in selection.candidates),
          "no rung-1 case leaked into another rung")


def test_absent_and_present_unmeasured_records_agree() -> None:
    """A row with no cohort is the same selection input as no row at all."""
    absent = decide(keys=("solo",), census_document=census())
    present = decide(keys=("solo",), census_document=census(entry("solo")))
    for label, selection in (("absent", absent), ("present", present)):
        check_equal(selection.probe, "solo", f"the {label} record selects")
        check_equal(selection.candidates[0].valid_runs, 0,
                    f"the {label} record ranks as zero valid runs")
        check_equal(selection.candidates[0].failure_rate, None,
                    f"the {label} record has no rate to report")


def test_a_zero_run_cohort_never_divides() -> None:
    """A cohort whose samples requested no runs is rung 1, not a rate of zero."""
    document = census(entry("empty_runs",
                            current=measured(runs=0, failures=0, at=RECENT)))
    selection = decide(keys=("empty_runs",), census_document=document)
    check_equal(selection.rung, select.RUNG_INCOMPLETE,
                "a zero-run cohort is incomplete measurement")
    check_equal(selection.candidates[0].failure_rate, None,
                "a zero-run cohort reports no failure rate at all")


def test_a_history_only_cohort_is_measured() -> None:
    """#1429's authoritative cohort is `history[-1]` when `current` is null."""
    document = census(entry(
        "demoted", x=0,
        history=[measured(runs=N, failures=0, at=RECENT, commit=COMMIT_B)]))
    selection = decide(keys=("demoted",), census_document=document,
                       now=NOW, stale_after_seconds=HORIZON)
    # 21 days old against the 14-day default horizon: stale, within
    # tolerance, so rung 3 rather than rung 1's "never measured".
    check_equal(selection.rung, select.RUNG_STALE,
                "an archived cohort is still a measurement")
    check_equal(selection.candidates[0].valid_runs, N,
                "its pooled run count is read from the archived cohort")


# --------------------------------------------------------------------------
# Rung 2 — over tolerance
# --------------------------------------------------------------------------

def test_rung_two_in_isolation() -> None:
    """Highest failure rate first, then oldest measurement, then key."""
    document = census(
        entry("mild", x=0, current=measured(runs=N, failures=1, at=RECENT)),
        entry("severe", x=0, current=measured(runs=N, failures=5, at=RECENT)),
        entry("mild_but_older", x=0,
              current=measured(runs=N, failures=1, at=OLD)),
    )
    selection = decide(keys=("mild", "mild_but_older", "severe"),
                       census_document=document)
    check_equal(selection.rung, select.RUNG_OVER_TOLERANCE,
                "every candidate is over tolerance")
    check_equal(ranked_keys(selection), ["severe", "mild_but_older", "mild"],
                "rate first, then the older measurement")
    check_equal(selection.candidates[0].failure_rate, Fraction(5, N),
                "the rate is carried exactly")


def test_nine_versus_ten_valid_runs() -> None:
    """N-1 valid runs is incomplete; N is measured and judged against X."""
    document = census(
        entry("nine", x=0, current=measured(runs=N - 1, failures=9, at=RECENT)),
        entry("ten", x=0, current=measured(runs=N, failures=1, at=RECENT)),
    )
    selection = decide(keys=("nine", "ten"), census_document=document)
    check_equal(selection.probe, "nine",
                "N-1 runs is incomplete measurement and outranks a breach")
    check_equal(selection.rung, select.RUNG_INCOMPLETE,
                "the N-1 cohort is rung 1")
    check_equal([candidate.rung for candidate in selection.candidates],
                [select.RUNG_INCOMPLETE, select.RUNG_OVER_TOLERANCE],
                "the N-run cohort is rung 2 behind it")
    lonely = decide(keys=("nine",),
                    census_document=census(entry(
                        "nine", x=0,
                        current=measured(runs=N - 1, failures=9, at=RECENT))))
    check_equal(lonely.candidates[0].failure_rate, None,
                "a sub-N cohort is never divided, however many failures it has")


def test_the_tolerance_boundary_is_exact() -> None:
    """Below, exactly at, and above X/N — at N runs and at accumulated sizes.

    The comparison is `failures * N > X * valid_runs` in integers, so a
    cohort sitting exactly on the threshold is WITHIN tolerance by
    construction rather than by however a division happened to round.
    """
    cases = [
        # (label, X, runs, failures, over tolerance?)
        ("below at N", 2, N, 1, False),
        ("equal at N", 2, N, 2, False),
        ("above at N", 2, N, 3, True),
        ("X=0 accepts only a clean sweep", 0, N, 0, False),
        ("X=0 breached by one failure", 0, N, 1, True),
        ("equal in an accumulated 30-run cohort", 1, 30, 3, False),
        ("below in an accumulated 30-run cohort", 1, 30, 2, False),
        ("above in an accumulated 30-run cohort", 1, 30, 4, True),
        ("equal in an accumulated 70-run cohort", 3, 70, 21, False),
        ("above in an accumulated 70-run cohort", 3, 70, 22, True),
    ]
    for label, x, runs, failures, over in cases:
        # Fresh, so a within-tolerance cohort qualifies for nothing and
        # only the breach can select: the outcome IS the classification.
        document = census(entry("subject", x=x,
                                current=measured(runs=runs, failures=failures,
                                                 at=JUST_NOW)))
        selection = decide(keys=("subject",), census_document=document)
        if over:
            check_equal(selection.rung, select.RUNG_OVER_TOLERANCE,
                        f"{label}: over tolerance")
        else:
            check_equal(selection.outcome, select.OUTCOME_NO_CANDIDATE,
                        f"{label}: within tolerance and fresh")
            check_equal(reasons_for(selection, "subject"),
                        (select.REASON_FRESH,),
                        f"{label}: recorded as fresh within tolerance")


def test_an_accumulated_cohort_pools_its_samples() -> None:
    """#1429 pools the cohort's samples; the selector never averages batches."""
    document = census(entry("pooled", x=1, current=cohort([
        sample(runs=N, failures=0, at=OLD),
        sample(runs=N, failures=0, at=RECENT),
        sample(runs=N, failures=4, at=JUST_NOW),
    ])))
    selection = decide(keys=("pooled",), census_document=document)
    candidate = selection.candidates[0]
    check_equal(candidate.valid_runs, 3 * N, "runs pool across the cohort")
    check_equal(candidate.failure_count, 4, "failures pool across the cohort")
    check_equal(candidate.failure_rate, Fraction(4, 3 * N),
                "the rate is the combined quotient, not a batch average")
    # 4/30 > 1/10, so pooled it breaches; the last batch alone (4/10)
    # would too, but the mean of the three batch rates would not.
    check_equal(selection.rung, select.RUNG_OVER_TOLERANCE,
                "the pooled rate decides tolerance")


# --------------------------------------------------------------------------
# Rung 3 — stale within tolerance
# --------------------------------------------------------------------------

def test_rung_three_in_isolation() -> None:
    """Oldest measurement first, then highest rate, then key."""
    document = census(
        entry("older_clean", x=2,
              current=measured(runs=N, failures=0, at=LONG_AGO)),
        entry("newer_worse", x=2,
              current=measured(runs=N, failures=2, at=OLD)),
        entry("older_worse", x=2,
              current=measured(runs=N, failures=1, at=LONG_AGO)),
    )
    selection = decide(keys=("newer_worse", "older_clean", "older_worse"),
                       census_document=document)
    check_equal(selection.rung, select.RUNG_STALE, "every candidate is stale")
    check_equal(ranked_keys(selection),
                ["older_worse", "older_clean", "newer_worse"],
                "oldest first, and the worse rate breaks an age tie")


def test_staleness_follows_the_supplied_evaluation_time() -> None:
    """The same census is fresh or stale purely by the injected inputs."""
    document = census(entry("subject", x=0,
                            current=measured(runs=N, failures=0, at=RECENT)))
    fresh = decide(keys=("subject",), census_document=document,
                   now=datetime.datetime(2026, 8, 5, tzinfo=datetime.timezone.utc))
    check_equal(fresh.outcome, select.OUTCOME_NO_CANDIDATE,
                "four days old against a fortnight horizon is fresh")
    stale = decide(keys=("subject",), census_document=document, now=NOW)
    check_equal(stale.rung, select.RUNG_STALE,
                "three weeks old against the same horizon is stale")
    tightened = decide(keys=("subject",), census_document=document,
                       now=datetime.datetime(2026, 8, 5,
                                             tzinfo=datetime.timezone.utc),
                       stale_after_seconds=probe_census.SECONDS_PER_DAY)
    check_equal(tightened.rung, select.RUNG_STALE,
                "the horizon is the caller's too")


# --------------------------------------------------------------------------
# The ladder: every higher rung defeats every lower one
# --------------------------------------------------------------------------

def test_every_higher_rung_defeats_every_lower_rung() -> None:
    """Mixed candidate sets, with each loser given its rung's best numbers."""
    # Rung 1's WEAKEST member (a nearly complete cohort) against rung 2's
    # STRONGEST (a total breach) and rung 3's oldest.
    incomplete = entry("zz_incomplete", x=0,
                       current=measured(runs=N - 1, failures=0, at=JUST_NOW))
    breach = entry("aa_breach", x=0,
                   current=measured(runs=N, failures=N, at=LONG_AGO))
    stale = entry("aa_stale", x=N - 1,
                  current=measured(runs=N, failures=0, at=LONG_AGO))

    pairs = [
        ("rung 1 beats rung 2", (incomplete, breach), "zz_incomplete",
         select.RUNG_INCOMPLETE),
        ("rung 1 beats rung 3", (incomplete, stale), "zz_incomplete",
         select.RUNG_INCOMPLETE),
        ("rung 2 beats rung 3", (breach, stale), "aa_breach",
         select.RUNG_OVER_TOLERANCE),
    ]
    for label, rows, winner, rung in pairs:
        keys = tuple(sorted(row["key"] for row in rows))
        selection = decide(keys=keys, census_document=census(*rows))
        check_equal(selection.probe, winner, label)
        check_equal(selection.rung, rung, f"{label}: winning rung")

    everything = census(incomplete, breach, stale)
    keys = ("aa_breach", "aa_stale", "zz_incomplete")
    selection = decide(keys=keys, census_document=everything)
    check_equal(ranked_keys(selection),
                ["zz_incomplete", "aa_breach", "aa_stale"],
                "all three rungs together rank strictly by rung, with the "
                "lexicographically later key still winning from rung 1")
    check_equal([candidate.rung for candidate in selection.candidates],
                [select.RUNG_INCOMPLETE, select.RUNG_OVER_TOLERANCE,
                 select.RUNG_STALE],
                "the concatenated order is rung order")


def test_a_fresh_probe_within_tolerance_is_the_terminal_state() -> None:
    """Fresh and within tolerance is no rung at all, not a fourth one."""
    document = census(
        entry("healthy_one", x=1,
              current=measured(runs=N, failures=1, at=JUST_NOW)),
        entry("healthy_two", x=0,
              current=measured(runs=3 * N, failures=0, at=JUST_NOW)),
    )
    selection = decide(keys=("healthy_one", "healthy_two"),
                       census_document=document)
    check_equal(selection.outcome, select.OUTCOME_NO_CANDIDATE,
                "a healthy roster selects nothing")
    check_equal(selection.probe, None, "and names no probe")
    check_equal(selection.candidates, (), "and ranks nothing")


# --------------------------------------------------------------------------
# Exclusions, applied before ranking
# --------------------------------------------------------------------------

def test_every_exclusion_beats_the_best_possible_rank() -> None:
    """An excluded probe cannot win even as the strongest rung-1 candidate."""
    fallback = entry("fallback", x=0,
                     current=measured(runs=N - 1, failures=0, at=JUST_NOW))
    cases = [
        ("ci-eligible", {"ci_eligible": ("aaa_excluded",)},
         select.REASON_CI_ELIGIBLE),
        ("legacy", {"protocol": {"aaa_excluded": probe_census.LEGACY,
                                 "fallback": select.PROTOCOL_VERSION}},
         select.REASON_LEGACY),
        ("in flight", {"in_flight": ("aaa_excluded",)},
         select.REASON_IN_FLIGHT),
        ("claimed", {"claimed": ("aaa_excluded",)}, select.REASON_CLAIMED),
        ("deferred", {}, select.REASON_DEFERRED),
        ("unclassified", {"manual_only": ("fallback",)},
         select.REASON_UNCLASSIFIED),
    ]
    for label, extra, reason in cases:
        # The excluded probe is unmeasured AND lexicographically first,
        # so it would win outright if the exclusion came after ranking.
        excluded = entry("aaa_excluded", deferred={
            "reason": "blocked on assets",
            "resume_when": "the assets merge",
        }) if label == "deferred" else None
        rows = ((excluded,) if excluded is not None else ()) + (fallback,)
        selection = decide(keys=("aaa_excluded", "fallback"),
                           census_document=census(*rows), **extra)
        check_equal(selection.probe, "fallback",
                    f"an {label} probe never wins")
        check_equal(reasons_for(selection, "aaa_excluded"), (reason,),
                    f"an {label} probe records exactly its own reason")
        check("aaa_excluded" not in ranked_keys(selection),
              f"an {label} probe is not even ranked")


def test_the_legacy_reason_is_recorded_verbatim() -> None:
    """A legacy probe is skipped, never treated as merely unmeasured."""
    selection = decide(
        keys=("legacy_probe",), census_document=census(),
        protocol={"legacy_probe": probe_census.LEGACY})
    check_equal(selection.outcome, select.OUTCOME_NO_CANDIDATE,
                "a roster of one legacy probe selects nothing")
    check_equal(reasons_for(selection, "legacy_probe"),
                ("requires protocol migration",),
                "the recorded reason is the spec's own wording")
    unknown_status = decide(keys=("mystery",), census_document=census(),
                            protocol={})
    check_equal(reasons_for(selection, "legacy_probe"),
                reasons_for(unknown_status, "mystery"),
                "an unclassified protocol status reads as legacy, exactly as "
                "probe_flake.protocol_status does")


def test_several_exclusions_are_all_recorded() -> None:
    """One probe may be excluded on more than one ground."""
    selection = decide(keys=("stuck",), census_document=census(entry(
        "stuck", deferred={"reason": "waiting", "resume_when": "ready"})),
                       protocol={"stuck": probe_census.LEGACY},
                       in_flight=("stuck",), claimed=("stuck",))
    check_equal(reasons_for(selection, "stuck"),
                (select.REASON_LEGACY, select.REASON_DEFERRED,
                 select.REASON_IN_FLIGHT,
                 select.REASON_CLAIMED),
                "every applicable exclusion is recorded, in the fixed order")


def test_a_malformed_deferral_is_not_silently_skipped() -> None:
    """Persistent eligibility data must be trustworthy before exclusion."""
    document = census(entry(
        "subject", deferred={"reason": "waiting", "resume_when": "   "}))
    selection = decide(keys=("subject",), census_document=document,
                       claimed=("subject",))
    check_equal(selection.outcome, select.OUTCOME_MALFORMED,
                "a malformed deferral is a census error even while claimed")
    check(selection.error and "resume_when" in selection.error,
          "the malformed field is named", repr(selection.error))


def test_manual_only_is_key_membership_only() -> None:
    """#1440's plural reason tuples change no selection.

    The payload is `dict[str, tuple[Reason, ...]]` and may hold several
    records, one, or none. A selector that unpacked a `(category,
    reason)` pair would break on the first two shapes.
    """
    document = census(entry("subject"))
    shapes = {
        "two reasons": {"subject": (FakeReason("flaky", "one"),
                                    FakeReason("scenario-heavy", "two"))},
        "one reason": {"subject": (FakeReason("flaky", "one"),)},
        "no reasons": {"subject": ()},
        "a bare key set": {"subject"},
        "a key list": ["subject"],
    }
    for label, manual_only in shapes.items():
        selection = decide(keys=("subject",), census_document=document,
                           manual_only=manual_only)
        check_equal(selection.probe, "subject",
                    f"manual-only as {label} selects identically")


def test_a_key_in_both_classifications_is_excluded() -> None:
    """Classification consistency is `ci_probes`' to enforce, not to guess."""
    document = census(entry("contested"), entry("clean"))
    selection = decide(keys=("clean", "contested"), census_document=document,
                       ci_eligible=("contested",),
                       manual_only={"clean": (), "contested": ()})
    check_equal(selection.probe, "clean",
                "a doubly-classified probe is not selected")
    check_equal(reasons_for(selection, "contested"),
                (select.REASON_CI_ELIGIBLE,),
                "and is excluded on the safe side")


def test_unregistered_in_flight_and_claim_keys_are_irrelevant() -> None:
    """Neither set may exclude a probe it does not name."""
    selection = decide(keys=("subject",), census_document=census(),
                       in_flight=("some_other_probe",),
                       claimed=("retired_probe", "subject_probe"))
    check_equal(selection.probe, "subject",
                "a near-miss key in either set excludes nothing")


# --------------------------------------------------------------------------
# Determinism
# --------------------------------------------------------------------------

def test_ties_break_on_the_registered_key_not_registration_order() -> None:
    """The one fixture where registration order and key order disagree.

    `probe_runner_registry.PROBES` registers `blood_gpu_lifecycle` before
    `bleeding_trail` while `bleeding_trail` sorts first, so iterating the
    registry would pass a census-order-only test and still be wrong.
    Both keys are supplied as fixture strings; nothing reads the live
    registry.
    """
    # EVERY rung is tied here, not just the first: each rule ends in the
    # key, and a rule that leaned on Python's stable sort instead would
    # inherit whichever order it was handed.
    rungs = [
        ("rung 1", select.RUNG_INCOMPLETE, {}),
        ("rung 2", select.RUNG_OVER_TOLERANCE,
         {"x": 0, "current": measured(runs=N, failures=3, at=OLD)}),
        ("rung 3", select.RUNG_STALE,
         {"x": 3, "current": measured(runs=N, failures=3, at=OLD)}),
    ]
    for label, rung, shape in rungs:
        rows = (entry("blood_gpu_lifecycle", **shape),
                entry("bleeding_trail", **shape))
        registered_first = decide(
            keys=registry("blood_gpu_lifecycle", "bleeding_trail"),
            census_document=census(*rows))
        check_equal(registered_first.rung, rung, f"{label}: the tie is in it")
        check_equal(registered_first.probe, "bleeding_trail",
                    f"{label}: the lexicographically first key wins the tie")
        reversed_registry = decide(
            keys=registry("bleeding_trail", "blood_gpu_lifecycle"),
            census_document=census(*rows))
        check_equal(ranked_keys(reversed_registry),
                    ranked_keys(registered_first),
                    f"{label}: reversing registration order changes nothing")
        reversed_census = decide(
            keys=registry("blood_gpu_lifecycle", "bleeding_trail"),
            census_document=census(*reversed(rows)))
        check_equal(ranked_keys(reversed_census),
                    ranked_keys(registered_first),
                    f"{label}: reversing census array order changes nothing")


def test_the_registry_payload_and_census_columns_name_nothing() -> None:
    """Only the registered KEY decides anything.

    A registry value may be a script filename, the `(script, purpose)`
    pair `probe_runner_registry.PROBES` carries, or nothing at all, and a census
    row's own `script` and `classification` columns may disagree with
    the live registry — none of it may move a selection. Display
    purposes are not even an input: the registry is consulted for its
    keys alone.
    """
    rows = (entry("alpha", script="zzz_probe.py"),
            entry("beta", script="aaa_probe.py"))
    plain = decide(keys=("alpha", "beta"), census_document=census(*rows))
    renamed = decide(keys=("alpha", "beta"), census_document=census(*rows),
                     scripts={"alpha": "zzzzzz_probe.py",
                              "beta": "aaaaaa_probe.py"})
    check_equal(plain.probe, "alpha", "the key decides the tie")
    check_equal(ranked_keys(renamed), ranked_keys(plain),
                "script filenames decide nothing")
    # A census row whose `script` column disagrees with the registry is
    # the census page audit's finding, and must not move a selection.
    disagreeing = census(entry("alpha", script="not_even_close.py"),
                         entry("beta", script="also_wrong.py"))
    check_equal(ranked_keys(decide(keys=("alpha", "beta"),
                                   census_document=disagreeing)),
                ranked_keys(plain),
                "a stale census `script` column decides nothing either")
    payloads = select.select_next_probe(
        registry={"alpha": ("zzz_probe.py", "a display purpose"),
                  "beta": None},
        ci_eligible=(), manual_only={"alpha": (), "beta": ()},
        protocol={"alpha": select.PROTOCOL_VERSION,
                  "beta": select.PROTOCOL_VERSION},
        census=census(*rows), now=NOW, stale_after_seconds=HORIZON)
    check_equal(ranked_keys(payloads), ranked_keys(plain),
                "the registry's value payload is never read at all")


def test_the_same_inputs_always_give_the_same_answer() -> None:
    """Repetition is identical, which a clock-reading selector could not be."""
    document = census(
        entry("one", x=0, current=measured(runs=N, failures=2, at=OLD)),
        entry("two", x=0, current=measured(runs=N, failures=2, at=OLD)),
        entry("three", x=1, current=measured(runs=N, failures=0, at=OLD)),
    )
    first = decide(keys=("one", "three", "two"), census_document=document)
    second = decide(keys=("two", "one", "three"), census_document=document)
    check_equal(ranked_keys(second), ranked_keys(first),
                "two runs of the same inputs rank identically")
    check_equal(second.probe, first.probe, "and select identically")


# --------------------------------------------------------------------------
# The three outcomes
# --------------------------------------------------------------------------

def test_all_three_outcomes_are_distinct() -> None:
    selected = decide(keys=("subject",), census_document=census())
    check_equal(selected.outcome, select.OUTCOME_SELECTED, "a selection")
    check_equal(selected.error, None, "carries no error")

    healthy = decide(keys=("subject",), census_document=census(entry(
        "subject", x=0, current=measured(runs=N, failures=0, at=JUST_NOW))))
    check_equal(healthy.outcome, select.OUTCOME_NO_CANDIDATE, "a valid empty")
    check_equal(healthy.error, None, "carries no error either")

    broken = decide(keys=("subject",), census_document=census(entry(
        "subject", x=0, current=cohort([{"commit_sha": COMMIT_A,
                                         "requested_runs": N,
                                         "failure_count": 0}]))))
    check_equal(broken.outcome, select.OUTCOME_MALFORMED,
                "a sample with no timestamp is malformed data")
    check(broken.error and "timestamp_utc" in broken.error,
          "the error names the missing field", repr(broken.error))
    check_equal(broken.probe, None, "a malformed census selects nothing")
    check_equal(broken.candidates, (), "and ranks nothing")

    check_equal(len({selected.outcome, healthy.outcome, broken.outcome}), 3,
                "the three outcomes are distinct values")


def test_the_valid_empty_outcome_says_which_empty_it_is() -> None:
    """Healthy and blocked are the same outcome and different reasons."""
    healthy = decide(keys=("subject",), census_document=census(entry(
        "subject", x=0, current=measured(runs=N, failures=0, at=JUST_NOW))))
    blocked = decide(keys=("subject",), census_document=census(),
                     claimed=("subject",))
    check_equal(healthy.outcome, blocked.outcome,
                "both are the valid empty outcome")
    check_equal(reasons_for(healthy, "subject"), (select.REASON_FRESH,),
                "a healthy roster records fresh-within-tolerance")
    check_equal(reasons_for(blocked, "subject"), (select.REASON_CLAIMED,),
                "a blocked roster records the exclusion")
    check(reasons_for(healthy, "subject") != reasons_for(blocked, "subject"),
          "so a caller can tell stopping from waiting apart")


def test_a_recorded_reason_never_changes_the_outcome() -> None:
    """Skips ride alongside a selection, they do not suppress one."""
    document = census(entry("winner"))
    selection = decide(keys=("blocked", "legacy", "winner"),
                       census_document=document, claimed=("blocked",),
                       protocol={"legacy": probe_census.LEGACY,
                                 "blocked": select.PROTOCOL_VERSION,
                                 "winner": select.PROTOCOL_VERSION})
    check_equal(selection.outcome, select.OUTCOME_SELECTED,
                "recorded skips do not turn a selection into an empty")
    check_equal(selection.probe, "winner", "and the winner is unaffected")
    check_equal([skip.key for skip in selection.skipped],
                ["blocked", "legacy"],
                "both skips are recorded, in ascending key order")
    shuffled = decide(keys=registry("winner", "legacy", "blocked"),
                      census_document=document, claimed=("blocked",),
                      protocol={"legacy": probe_census.LEGACY,
                                "blocked": select.PROTOCOL_VERSION,
                                "winner": select.PROTOCOL_VERSION})
    check_equal([skip.key for skip in shuffled.skipped],
                [skip.key for skip in selection.skipped],
                "which registration order does not change")


# --------------------------------------------------------------------------
# Malformed versus ignored
# --------------------------------------------------------------------------

def test_a_broken_container_is_always_the_error_outcome() -> None:
    cases = [
        ("not an object", ["probes"]),
        ("no probes list", {"schema": probe_census.CENSUS_SCHEMA}),
        ("probes is not a list",
         {"schema": probe_census.CENSUS_SCHEMA, "probes": {}}),
        ("an entry is not an object",
         {"schema": probe_census.CENSUS_SCHEMA, "probes": ["subject"]}),
        ("an entry has no string key",
         {"schema": probe_census.CENSUS_SCHEMA, "probes": [{"key": 7}]}),
    ]
    for label, document in cases:
        selection = decide(keys=("subject",), census_document=document)
        check_equal(selection.outcome, select.OUTCOME_MALFORMED,
                    f"a census that is {label} is malformed")
        check(bool(selection.error), f"{label} carries a diagnostic")


def test_a_duplicate_in_domain_record_is_malformed() -> None:
    """Two records for one probe leave ranking data ambiguous."""
    duplicated = census(entry("subject", x=0,
                              current=measured(runs=N, failures=0, at=OLD)),
                        entry("subject", x=0,
                              current=measured(runs=N, failures=9, at=OLD)))
    selection = decide(keys=("subject",), census_document=duplicated)
    check_equal(selection.outcome, select.OUTCOME_MALFORMED,
                "a duplicated record is not first-wins")
    check(selection.error and "subject" in selection.error,
          "the error names the duplicated probe", repr(selection.error))
    # A duplicate OUTSIDE the domain is the census audit's finding.
    outside = census(entry("elsewhere"), entry("elsewhere"), entry("subject"))
    check_equal(decide(keys=("subject",), census_document=outside).probe,
                "subject",
                "a duplicate for an unregistered probe is not read at all")


def test_corrupt_data_beats_transient_external_state() -> None:
    """In-flight, claimed and legacy must not hide a corrupt record."""
    corrupt = census(entry("subject", x=0,
                           current=cohort([{"commit_sha": COMMIT_A,
                                            "timestamp_utc": OLD,
                                            "failure_count": 0}])))
    for label, extra in (("in flight", {"in_flight": ("subject",)}),
                         ("claimed", {"claimed": ("subject",)}),
                         ("legacy", {"protocol": {
                             "subject": probe_census.LEGACY}})):
        selection = decide(keys=("subject",), census_document=corrupt, **extra)
        check_equal(selection.outcome, select.OUTCOME_MALFORMED,
                    f"a corrupt record still errors while {label}")


def test_records_outside_the_domain_are_ignored_not_errors() -> None:
    """Census-versus-registry reconciliation is not the selector's job."""
    broken_row = {"key": "stranger", "census": "not even an object"}
    document = census(broken_row, entry("subject"))
    selection = decide(keys=("subject",), census_document=document)
    check_equal(selection.probe, "subject",
                "a corrupt record for an unregistered probe is ignored")

    promoted = {"key": "promoted", "census": {"current": "garbage"}}
    with_ci = census(promoted, entry("subject"))
    selection = decide(keys=("promoted", "subject"), census_document=with_ci,
                       ci_eligible=("promoted",))
    check_equal(selection.probe, "subject",
                "a corrupt record for a CI-eligible probe is ignored too")
    check_equal(reasons_for(selection, "promoted"),
                (select.REASON_CI_ELIGIBLE,),
                "and the probe is still recorded as CI-eligible")


def test_a_measured_cohort_needs_a_validated_x() -> None:
    """At N runs or more, a missing or unusable X is malformed input."""
    for label, x in (("null", None), ("out of range", N),
                     ("negative", -1), ("a bool", True), ("a string", "0")):
        document = census(entry("subject", x=x,
                                current=measured(runs=N, failures=0, at=OLD)))
        selection = decide(keys=("subject",), census_document=document)
        check_equal(selection.outcome, select.OUTCOME_MALFORMED,
                    f"an X that is {label} is malformed at N runs")
        check(selection.error and "acceptable_failures" in selection.error,
              f"an X that is {label} names the field", repr(selection.error))
    # Below N there is nothing to compare against, so the same X is not
    # yet required: rung 1 does no tolerance arithmetic.
    partial = census(entry("subject", x=None,
                           current=measured(runs=N - 1, failures=0, at=OLD)))
    selection = decide(keys=("subject",), census_document=partial)
    check_equal(selection.rung, select.RUNG_INCOMPLETE,
                "a partial cohort with no X is still rung 1")
    unmeasured = decide(keys=("subject",),
                        census_document=census(entry("subject", x=None)))
    check_equal(unmeasured.rung, select.RUNG_INCOMPLETE,
                "an unmeasured probe with no X is still rung 1")


def test_the_first_reported_corruption_is_census_order_independent() -> None:
    """Which corruption is named must not depend on array order."""
    first = entry("aaa", x=0, current=cohort([{"commit_sha": COMMIT_A}]))
    second = entry("zzz", x=0, current=cohort([{"commit_sha": COMMIT_B}]))
    forward = decide(keys=("aaa", "zzz"), census_document=census(first, second))
    backward = decide(keys=("aaa", "zzz"),
                      census_document=census(second, first))
    check_equal(forward.outcome, select.OUTCOME_MALFORMED, "both are errors")
    check_equal(backward.error, forward.error,
                "and both name the same record first")
    check(forward.error and "aaa" in forward.error,
          "the ascending-key-order record is the one reported",
          repr(forward.error))


# --------------------------------------------------------------------------
# Caller inputs
# --------------------------------------------------------------------------

def test_unusable_caller_inputs_are_refused_not_reported_as_census_errors() -> None:
    """A bad evaluation time or horizon is the consumer's own bug."""
    cases = [
        ("a naive datetime", {"now": datetime.datetime(2026, 8, 22)}),
        ("a date string", {"now": "2026-08-22T12:00:00Z"}),
        ("a negative horizon", {"stale_after_seconds": -1}),
        ("a non-numeric horizon", {"stale_after_seconds": "14"}),
    ]
    for label, extra in cases:
        try:
            decide(keys=("subject",), census_document=census(), **extra)
        except select.SelectionError:
            check(True, f"{label} is refused")
        except Exception as error:                            # noqa: BLE001
            check(False, f"{label} is refused",
                  f"raised {type(error).__name__}: {error}")
        else:
            check(False, f"{label} is refused", "no refusal at all")


def test_unusable_input_shapes_are_refused() -> None:
    """A single string is not a set of keys, however set-like it looks."""
    cases = [
        ("a bare string in-flight set", {"in_flight": "subject"}),
        ("a bare string claim set", {"claimed": "subject"}),
        ("a bare string manual-only classification",
         {"manual_only": "subject"}),
        ("a non-mapping protocol classification", {"protocol": ["subject"]}),
        ("a non-string registered key", {"keys": {7: "seven_probe.py"}}),
        # Passed straight through, because `decide` would otherwise
        # expand a bare string into one registry key per character.
        ("a bare string registry", {"registry": "subject"}),
    ]
    for label, extra in cases:
        arguments = {"keys": ("subject",), "census_document": census()}
        arguments.update(extra)
        try:
            if "registry" in arguments:
                select.select_next_probe(
                    registry=arguments["registry"], ci_eligible=(),
                    manual_only=(), protocol={}, census=census(), now=NOW,
                    stale_after_seconds=HORIZON)
            else:
                decide(**arguments)
        except select.SelectionError:
            check(True, f"{label} is refused")
        except Exception as error:                            # noqa: BLE001
            check(False, f"{label} is refused",
                  f"raised {type(error).__name__}: {error}")
        else:
            check(False, f"{label} is refused", "no refusal at all")


def test_an_empty_roster_is_a_valid_empty() -> None:
    selection = decide(keys=(), census_document=census())
    check_equal(selection.outcome, select.OUTCOME_NO_CANDIDATE,
                "no registered probes selects nothing, and does not error")
    check_equal(selection.skipped, (), "and records no skips")


# --------------------------------------------------------------------------
# The live adapter
# --------------------------------------------------------------------------

def test_the_live_adapter_supplies_every_input_shape() -> None:
    """Structural only — never a pin on which probes are in which set.

    Runs outside the purity guard: the adapter imports the registry
    modules, which is exactly the coupling the decision function does
    not have.
    """
    inputs = select.live_inputs()
    check_equal(sorted(inputs), ["ci_eligible", "manual_only", "protocol",
                                 "registry"],
                "the adapter names all four registry-derived inputs")
    check(isinstance(inputs["registry"], dict) and inputs["registry"],
          "the registry is a non-empty mapping")
    check_equal(set(inputs["protocol"]), set(inputs["registry"]),
                "every registered probe has a supplied protocol status")
    check(set(inputs["protocol"].values())
          <= {select.PROTOCOL_VERSION, probe_census.LEGACY},
          "every protocol status is one of the two probe_flake reports",
          repr(sorted(set(inputs["protocol"].values()))))
    check(all(isinstance(key, str) for key in inputs["ci_eligible"]),
          "the CI-eligible classification is a set of keys")
    check(all(isinstance(key, str) for key in inputs["manual_only"]),
          "the manual-only classification is a set of keys")


# --------------------------------------------------------------------------

def main() -> int:
    cases = [
        test_rung_one_in_isolation,
        test_absent_and_present_unmeasured_records_agree,
        test_a_zero_run_cohort_never_divides,
        test_a_history_only_cohort_is_measured,
        test_rung_two_in_isolation,
        test_nine_versus_ten_valid_runs,
        test_the_tolerance_boundary_is_exact,
        test_an_accumulated_cohort_pools_its_samples,
        test_rung_three_in_isolation,
        test_staleness_follows_the_supplied_evaluation_time,
        test_every_higher_rung_defeats_every_lower_rung,
        test_a_fresh_probe_within_tolerance_is_the_terminal_state,
        test_every_exclusion_beats_the_best_possible_rank,
        test_the_legacy_reason_is_recorded_verbatim,
        test_several_exclusions_are_all_recorded,
        test_a_malformed_deferral_is_not_silently_skipped,
        test_manual_only_is_key_membership_only,
        test_a_key_in_both_classifications_is_excluded,
        test_unregistered_in_flight_and_claim_keys_are_irrelevant,
        test_ties_break_on_the_registered_key_not_registration_order,
        test_the_registry_payload_and_census_columns_name_nothing,
        test_the_same_inputs_always_give_the_same_answer,
        test_all_three_outcomes_are_distinct,
        test_the_valid_empty_outcome_says_which_empty_it_is,
        test_a_recorded_reason_never_changes_the_outcome,
        test_a_broken_container_is_always_the_error_outcome,
        test_a_duplicate_in_domain_record_is_malformed,
        test_corrupt_data_beats_transient_external_state,
        test_records_outside_the_domain_are_ignored_not_errors,
        test_a_measured_cohort_needs_a_validated_x,
        test_the_first_reported_corruption_is_census_order_independent,
        test_unusable_caller_inputs_are_refused_not_reported_as_census_errors,
        test_unusable_input_shapes_are_refused,
        test_an_empty_roster_is_a_valid_empty,
    ]
    with Pure():
        for case in cases:
            try:
                case()
            except Exception as error:                        # noqa: BLE001
                FAILURES.append(
                    f"{case.__name__} raised {type(error).__name__}: {error}")
    # Outside the guard: the adapter is the one function that reaches the
    # registry modules, which is what it exists to do.
    try:
        test_the_live_adapter_supplies_every_input_shape()
    except Exception as error:                                # noqa: BLE001
        FAILURES.append(
            "test_the_live_adapter_supplies_every_input_shape raised "
            f"{type(error).__name__}: {error}")

    print(f"probe_select self-test: {PASSED} checks passed, "
          f"{len(FAILURES)} failed")
    for failure in FAILURES:
        print(f"  FAIL {failure}")
    return 1 if FAILURES else 0


if __name__ == "__main__":
    sys.exit(main())
