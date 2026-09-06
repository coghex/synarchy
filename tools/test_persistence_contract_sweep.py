#!/usr/bin/env python3
"""Unit tests for persistence_contract_sweep.py's registry-drift guard
(issue #1321) and its durable phase records (issue #1768).

`SELECTABLE_CROSS_REFERENCED_PROBE_KEYS` is a hand-maintained copy of a
subset of `probe_runner_registry.PROBES`. Nothing previously
checked the two agree: a key renamed or removed in `PROBES` would leave
`persistence_contract_sweep.py --cross-probe-keys ...` handing a dead key
to `run_probes.py --exact`, which silently drops it, after which the
sweep reports "cross-referenced probes (...) all passed" while having run
fewer than it named -- the exact false-green requirement 11/13 coverage
exists to prevent.

`unregistered_selectable_probe_keys` is the pure, parameterized check
`persistence_contract_sweep.main` runs against the real lists before
booting anything. These tests exercise it directly -- no engine, no
probe, no subprocess -- proving both that today's real pairing is clean
AND (the round-2 review's point) that a deliberately introduced
disagreement is actually caught rather than merely asserting the
current state happens to be fine.

The #1768 half is the same shape: `SWEEP_PHASE_IDENTITIES` and
`announce_phase` are the sweep's declared phase contract, and every
record they emit has to be recognizable to `probe_runner_diagnostics`'s
timeout attribution -- the consumer at the other end of the pipe. These
tests hold both halves against each other without booting anything.

The #2060 half is the failing counterpart. `Checks.ok` used to retain a
COUNT and nothing else, so a failed check that enough sweep output
followed was truncated out of `run_probes.py`'s bounded `--tail 25`
presentation and its identity was simply gone -- which is what the
2026-08-31 artifact lost. `Checks` now emits one durable
`#probe-failure#` record per failed check, and these tests drive the
REAL `Checks` through the REAL `FailureEmitter` print path, bury the
records under far more than a tail's worth of ordinary output, and
require `probe_runner_diagnostics.failure_attribution` to name every one
of them back -- the same reader the runner applies to the complete
capture in both its sequential and its `--jobs N` mode, so neither mode
needs a real 900 s sweep run to be covered.

Usage:
  python3 tools/test_persistence_contract_sweep.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import contextlib
import io
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from persistence_contract_sweep import (  # type: ignore
    SELECTABLE_CROSS_REFERENCED_PROBE_KEYS,
    SWEEP_CYCLE_LETTERS,
    SWEEP_FAILURE_PRODUCER,
    SWEEP_PHASE_COMPARISON,
    SWEEP_PHASE_CROSS_PROBES,
    SWEEP_PHASE_ENGINE_A,
    SWEEP_PHASE_IDENTITIES,
    Checks,
    announce_phase,
    engine_cycle_phase,
    unregistered_selectable_probe_keys,
)
import probe_runner_diagnostics  # type: ignore
from probe_runner_registry import PROBES  # type: ignore

import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402


def test_todays_selectable_keys_are_all_registered() -> None:
    print("\n-- every SELECTABLE_CROSS_REFERENCED_PROBE_KEYS entry names a "
          "real run_probes.py probe today")
    registered = {p[0] for p in PROBES}
    stale = unregistered_selectable_probe_keys(
        SELECTABLE_CROSS_REFERENCED_PROBE_KEYS, registered)
    expect(stale == [],
           f"the real registry and SELECTABLE_CROSS_REFERENCED_PROBE_KEYS "
           f"disagree on {stale!r}")


def test_a_key_the_registry_drops_is_caught() -> None:
    print("\n-- a selectable key that vanishes from the registry is reported, "
          "not silently accepted (round-2 review's negative regression)")
    registered = {p[0] for p in PROBES} - {"chop"}
    stale = unregistered_selectable_probe_keys(
        SELECTABLE_CROSS_REFERENCED_PROBE_KEYS, registered)
    expect(stale == ["chop"],
           f"removing 'chop' from the registry should surface it alone, "
           f"got {stale!r}")


def test_every_stale_key_is_named_at_once() -> None:
    print("\n-- every stale key is identified, not just the first")
    stale = unregistered_selectable_probe_keys(
        ["chop", "not_a_probe", "till", "also_not_a_probe"],
        {"chop", "till"})
    expect(stale == ["not_a_probe", "also_not_a_probe"],
           f"expected both unknown keys in request order, got {stale!r}")


def test_empty_selectable_list_has_nothing_stale() -> None:
    print("\n-- an empty selectable list trivially has no stale entries")
    stale = unregistered_selectable_probe_keys([], {"chop", "till"})
    expect(stale == [], f"expected no stale keys, got {stale!r}")


# --------------------------------------------------------------------------
# Durable phase records (#1768)
# --------------------------------------------------------------------------
class CapturedEmitter(probe_runner_diagnostics.ProgressEmitter):
    """The real emitter, with its flushed line kept instead of printed.

    Subclassing rather than redirecting stdout keeps the formatting under
    test the shipped one: only the delivery is replaced.
    """

    def __init__(self) -> None:
        super().__init__()
        self.lines: list[str] = []

    def emit(self, kind: str, identity: str, detail: str) -> str:
        line = probe_runner_diagnostics.format_progress(kind, identity, detail,
                                          elapsed=0.0, now=0.0)
        self.lines.append(line)
        return line


def test_every_required_sweep_phase_is_declared() -> None:
    print("\n-- the four phases #1768 requires are all declared identities")
    expect(SWEEP_PHASE_ENGINE_A in SWEEP_PHASE_IDENTITIES,
           f"engine A's phase is declared (identities: "
           f"{SWEEP_PHASE_IDENTITIES!r})")
    cycles = [engine_cycle_phase(letter) for letter in SWEEP_CYCLE_LETTERS]
    expect(len(cycles) == 3,
           f"requirement 9's three fresh-process cycles are all named "
           f"(got {cycles!r})")
    expect(all(cycle in SWEEP_PHASE_IDENTITIES for cycle in cycles),
           f"and each is a declared phase identity (got {cycles!r})")
    expect(SWEEP_PHASE_COMPARISON in SWEEP_PHASE_IDENTITIES,
           "the structural comparison's phase is declared")
    expect(SWEEP_PHASE_CROSS_PROBES in SWEEP_PHASE_IDENTITIES,
           "the cross-referenced probe phase is declared")
    expect(len(set(SWEEP_PHASE_IDENTITIES)) == len(SWEEP_PHASE_IDENTITIES),
           f"no identity is declared twice, so begin/end pairing and "
           f"'latest phase' stay unambiguous ({SWEEP_PHASE_IDENTITIES!r})")


def test_an_undeclared_phase_is_refused_rather_than_emitted() -> None:
    print("\n-- a phase missing from the declared list is refused, so the "
          "list cannot silently go stale")
    emitter = CapturedEmitter()
    try:
        announce_phase(emitter, "engine Z", "a phase nobody declared")
    except ValueError:
        expect(True, "an undeclared phase identity raises")
    else:
        expect(False, "an undeclared phase identity should have raised")
    expect(emitter.lines == [],
           f"and emits nothing at all (got {emitter.lines!r})")


def test_every_declared_phase_emits_a_record_the_runner_recognizes() -> None:
    print("\n-- each phase record parses back through the diagnostics "
          "reader, with the right kind and identity")
    for identity in SWEEP_PHASE_IDENTITIES:
        emitter = CapturedEmitter()
        line = announce_phase(emitter, identity, "some detail")
        expect(emitter.lines == [line],
               f"{identity!r} emitted exactly one record (got "
               f"{emitter.lines!r})")
        record = probe_runner_diagnostics.parse_progress(line)
        expect(record is not None,
               f"{identity!r}'s record parses as a progress record "
               f"({line!r})")
        expect(record is not None and record.kind == "phase",
               f"{identity!r}'s record is a phase record (got {record!r})")
        expect(record is not None and record.identity == identity,
               f"{identity!r}'s record carries its identity (got {record!r})")
        expect(record is not None and record.detail == "some detail",
               f"{identity!r}'s record carries its detail (got {record!r})")


def test_the_latest_sweep_phase_survives_a_long_tail() -> None:
    print("\n-- the runner's attribution names the sweep's LATEST phase even "
          "when far more than --tail 25 lines followed it")
    emitter = CapturedEmitter()
    for identity in SWEEP_PHASE_IDENTITIES:
        announce_phase(emitter, identity, f"detail for {identity}")
    capture = "\n".join(
        emitter.lines + [f"ordinary sweep output {i}" for i in range(60)])
    got = probe_runner_diagnostics.progress_attribution(capture)
    text = "\n".join(got)
    latest = SWEEP_PHASE_IDENTITIES[-1]
    expect(latest in text,
           f"the last phase entered ({latest!r}) is named (got {got!r})")
    expect(SWEEP_PHASE_ENGINE_A not in text,
           f"and the phases it superseded are not (got {got!r})")
    expect("ordinary sweep output 0" not in text,
           f"the attribution does not dump the capture (got {got!r})")


# --------------------------------------------------------------------------
# Durable failed-check records (#2060)
#
# The runner's default presentation prints a bounded tail of the capture,
# so what is retained about a failure is whatever `failure_attribution`
# recovers from the COMPLETE capture plus the last `--tail` ORDINARY
# lines. Every case below therefore drives the real `Checks` through the
# real `FailureEmitter` print path and then buries its records under more
# ordinary output than that tail can hold: a `Checks` that only counted
# would leave nothing for the reader to find.
# --------------------------------------------------------------------------
def runner_default_tail() -> int:
    """`tools/run_probes.py`'s OWN default `--tail`, read from its parser.

    The burial below has to be measured against the number the runner
    really uses, not a copy of it: a default raised past the burial size
    would leave every case here passing while proving nothing, because
    the printed `[FAIL]` lines would still be sitting in the tail. The
    runner builds its parser inside `main` and exposes it nowhere, so the
    default is taken at the moment it would be used and the parse is
    abandoned -- nothing is selected, launched, or run.
    """
    import argparse
    import run_probes  # type: ignore

    captured: list[int] = []

    def intercept(self, *args, **kwargs):
        captured.append(self.get_default("tail"))
        raise SystemExit(0)

    real = argparse.ArgumentParser.parse_args
    argparse.ArgumentParser.parse_args = intercept   # type: ignore[method-assign]
    try:
        argv, sys.argv = sys.argv, ["run_probes.py", "--list"]
        try:
            run_probes.main()
        except SystemExit:
            pass
        finally:
            sys.argv = argv
    finally:
        argparse.ArgumentParser.parse_args = real   # type: ignore[method-assign]
    if len(captured) != 1 or not isinstance(captured[0], int):
        raise AssertionError(
            f"could not read run_probes.py's default --tail (got "
            f"{captured!r}); these cases cannot claim a record is outside "
            f"the retained tail without it")
    return captured[0]


#: `tools/run_probes.py`'s default `--tail`, so a case can say "outside the
#: retained tail" in the units the runner actually uses.
DEFAULT_TAIL = runner_default_tail()

#: One realistic failing sweep assertion per long phase, deliberately more
#: than one so "the SECOND failure was unrecoverable" -- the loss #2060 is
#: about -- is what these cases reproduce.
SAMPLE_FAILURES = [
    "gen1 saved page 'contract_sweep_page' back with its own visibility",
    "the craft bill survived the second load->save cycle intact",
    "cross-referenced probes (chop, till, crop) all passed",
]
SAMPLE_PASSES = [
    "the mine designation round-tripped",
    "the camera position is not the default",
]


def run_checks_capturing(failures, passes=()) -> tuple[Checks, str]:
    """Drive the REAL `Checks` and keep everything it wrote.

    `Checks()` builds its own real `FailureEmitter`, which prints, so the
    capture here is byte-for-byte what the sweep would push into the
    runner's pipe -- the emitter's flushing and formatting are under test
    rather than stood in for.
    """
    checks = Checks()
    buffer = io.StringIO()
    with contextlib.redirect_stdout(buffer):
        for label in passes:
            checks.ok(True, label)
        for label in failures:
            checks.ok(False, label)
    return checks, buffer.getvalue()


def bury(capture: str, lines: int | None = None) -> str:
    """`capture` followed by more ordinary output than the tail holds.

    Sized from the runner's real default rather than a fixed number, so
    the burial stays a burial if that default ever moves.
    """
    lines = DEFAULT_TAIL * 2 + 10 if lines is None else lines
    return capture + "".join(
        f"  [PASS] later sweep assertion {i}\n" for i in range(lines))


def retained_tail(capture: str) -> list[str]:
    """The ordinary lines the runner would actually print beside the block."""
    ordinary = probe_runner_diagnostics.without_failure_records(capture)
    return ordinary.splitlines()[-DEFAULT_TAIL:]


def test_the_producer_identity_names_this_script() -> None:
    print("\n-- the producer a sweep record carries is this script's own "
          "name, not some other spelling of it")

    # Asserted against the filename rather than a copy of the constant:
    # comparing the records below to the constant alone would hold only
    # that the sweep agrees with itself, and an operator reading
    # "N recorded failure(s) from <producer>" needs the name to be the
    # tool they would rerun. It is also the name that keeps a sweep-own
    # assertion apart from a cross-referenced probe's own records.
    import persistence_contract_sweep  # type: ignore
    expect(SWEEP_FAILURE_PRODUCER == "persistence_contract_sweep",
           f"the sweep records itself under the name #2060 names (got "
           f"{SWEEP_FAILURE_PRODUCER!r})")
    expect(SWEEP_FAILURE_PRODUCER
           == Path(persistence_contract_sweep.__file__).stem,
           f"which is the script's own filename (got "
           f"{SWEEP_FAILURE_PRODUCER!r})")
    expect(SWEEP_FAILURE_PRODUCER
           not in SELECTABLE_CROSS_REFERENCED_PROBE_KEYS,
           f"and is not one of the cross-referenced probe keys it would "
           f"be confused with (got {SWEEP_FAILURE_PRODUCER!r})")


def test_a_failed_sweep_check_records_its_own_label() -> None:
    print("\n-- one failed check produces exactly one durable record, "
          "carrying the whole label and naming the sweep as its producer")
    checks, capture = run_checks_capturing(SAMPLE_FAILURES[:1])
    records = probe_runner_diagnostics.failure_records(capture)
    expect(len(records) == 1,
           f"exactly one record, so the runner names it once (got "
           f"{records!r})")
    expect(records[0].kind == "check",
           f"in the 'check' vocabulary, not 'setup' -- a sweep assertion "
           f"is a product failure, not a fixture one (got {records[0]!r})")
    expect(records[0].identity == SWEEP_FAILURE_PRODUCER,
           f"naming {SWEEP_FAILURE_PRODUCER!r} as its producer, so a "
           f"sweep-own assertion stays distinguishable from a nested "
           f"probe's (got {records[0].identity!r})")
    expect(records[0].detail == SAMPLE_FAILURES[0],
           f"and carrying the COMPLETE label, which is the only thing "
           f"identifying which check this was (got {records[0].detail!r})")
    expect(checks.failed == 1,
           f"the numeric counter the terminal summary reports is unchanged "
           f"(got {checks.failed})")


def test_a_passing_sweep_check_emits_no_failure_marker() -> None:
    print("\n-- a passing check adds no record and no marker, so a green "
          "run is exactly as quiet as it was")
    checks, capture = run_checks_capturing([], SAMPLE_PASSES)
    expect(probe_runner_diagnostics.FAILURE_MARKER not in capture,
           f"no failure marker appears anywhere in a passing run (got "
           f"{capture!r})")
    expect(probe_runner_diagnostics.failure_records(capture) == [],
           "and therefore no records at all")
    expect(probe_runner_diagnostics.failure_attribution(capture) == [],
           "so the runner's presentation gains nothing for a passing run")
    expect(capture.splitlines()
           == [f"  [PASS] {label}" for label in SAMPLE_PASSES],
           f"the printed verdict lines keep their exact shape, one per "
           f"check and nothing else (got {capture.splitlines()!r})")
    expect(checks.failed == 0, f"and nothing is counted (got {checks.failed})")


def test_a_passing_check_beside_failing_ones_stays_unrecorded() -> None:
    print("\n-- among failures, only the FAILED checks are recorded")
    checks, capture = run_checks_capturing(SAMPLE_FAILURES, SAMPLE_PASSES)
    details = [record.detail
               for record in probe_runner_diagnostics.failure_records(capture)]
    expect(details == SAMPLE_FAILURES,
           f"every failed label, in order, and no passing one (got "
           f"{details!r})")
    for label in SAMPLE_PASSES:
        expect(label not in "\n".join(
                   probe_runner_diagnostics.failure_attribution(capture)),
               f"the passing check {label!r} is named nowhere in the "
               f"failure presentation")
    expect(checks.failed == len(SAMPLE_FAILURES),
           f"the counter still counts only failures (got {checks.failed})")


def test_every_failed_check_survives_outside_the_retained_tail() -> None:
    print("\n-- every failed check is named even when the runner's default "
          "--tail 25 has long since scrolled past all of them")
    _, capture = run_checks_capturing(SAMPLE_FAILURES, SAMPLE_PASSES)
    buried = bury(capture)

    # The precondition: without it this case would pass on a `Checks` that
    # records nothing, purely because the tail still happened to hold the
    # printed [FAIL] lines.
    tail = "\n".join(retained_tail(buried))
    for label in SAMPLE_FAILURES:
        expect(label not in tail,
               f"{label!r} really is outside the retained tail, so only a "
               f"durable record can bring it back")

    got = probe_runner_diagnostics.failure_attribution(buried)
    text = "\n".join(got)
    for label in SAMPLE_FAILURES:
        expect(text.count(label) == 1,
               f"{label!r} is named exactly once by the attribution (got "
               f"{got!r})")
    expect(f"{len(SAMPLE_FAILURES)} recorded failure(s)" in text,
           f"the block counts every one of them (got {got!r})")
    expect(SWEEP_FAILURE_PRODUCER in text,
           f"and attributes them to the sweep (got {got!r})")
    expect("later sweep assertion 0" not in text,
           f"without dumping the capture it read (got {got!r})")


def test_sweep_failure_records_are_not_consumed_by_phase_attribution() -> None:
    print("\n-- the failed-check records and #1768's phase records stay on "
          "separate channels, and neither reader eats the other's")
    emitter = CapturedEmitter()
    for identity in SWEEP_PHASE_IDENTITIES:
        announce_phase(emitter, identity, f"detail for {identity}")
    _, failures = run_checks_capturing(SAMPLE_FAILURES)
    combined = bury("\n".join(emitter.lines) + "\n" + failures)

    progress = probe_runner_diagnostics.progress_attribution(combined)
    progress_text = "\n".join(progress)
    for label in SAMPLE_FAILURES:
        expect(label not in progress_text,
               f"phase attribution does not report {label!r}: 'where was "
               f"it' is not 'what failed' (got {progress!r})")
    expect(SWEEP_PHASE_IDENTITIES[-1] in progress_text,
           f"while still naming the latest phase (got {progress!r})")

    failure_text = "\n".join(
        probe_runner_diagnostics.failure_attribution(combined))
    for identity in SWEEP_PHASE_IDENTITIES:
        expect(f"detail for {identity}" not in failure_text,
               f"and the failure block does not report the {identity!r} "
               f"phase record (got {failure_text!r})")

    # Records-only captures, so neither result can be carried by the other
    # convention's lines happening to be present.
    expect(probe_runner_diagnostics.failure_attribution(
               "\n".join(emitter.lines) + "\n") == [],
           "phase records alone yield no failure attribution")
    expect(probe_runner_diagnostics.progress_attribution(failures) == [],
           "and failed-check records alone yield no phase attribution")


def test_the_failure_records_are_removed_from_the_ordinary_tail() -> None:
    print("\n-- the records are presented in the failure block and taken "
          "out of the tail beside it, never printed twice")
    _, capture = run_checks_capturing(SAMPLE_FAILURES)
    ordinary = probe_runner_diagnostics.without_failure_records(capture)
    expect(probe_runner_diagnostics.FAILURE_MARKER not in ordinary,
           f"no raw record reaches the ordinary tail (got {ordinary!r})")
    expect(ordinary.splitlines()
           == [f"  [FAIL] {label}" for label in SAMPLE_FAILURES],
           f"which keeps exactly the printed verdict lines the sweep "
           f"always had (got {ordinary.splitlines()!r})")


def main() -> int:
    selftestlib.parse_verbose()
    test_todays_selectable_keys_are_all_registered()
    test_a_key_the_registry_drops_is_caught()
    test_every_stale_key_is_named_at_once()
    test_empty_selectable_list_has_nothing_stale()
    test_every_required_sweep_phase_is_declared()
    test_an_undeclared_phase_is_refused_rather_than_emitted()
    test_every_declared_phase_emits_a_record_the_runner_recognizes()
    test_the_latest_sweep_phase_survives_a_long_tail()
    test_the_producer_identity_names_this_script()
    test_a_failed_sweep_check_records_its_own_label()
    test_a_passing_sweep_check_emits_no_failure_marker()
    test_a_passing_check_beside_failing_ones_stays_unrecorded()
    test_every_failed_check_survives_outside_the_retained_tail()
    test_sweep_failure_records_are_not_consumed_by_phase_attribution()
    test_the_failure_records_are_removed_from_the_ordinary_tail()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(
        0, "\nAll persistence_contract_sweep registry-drift, phase-record "
        "and failed-check-record tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
