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

Usage:
  python3 tools/test_persistence_contract_sweep.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from persistence_contract_sweep import (  # type: ignore
    SELECTABLE_CROSS_REFERENCED_PROBE_KEYS,
    SWEEP_CYCLE_LETTERS,
    SWEEP_PHASE_COMPARISON,
    SWEEP_PHASE_CROSS_PROBES,
    SWEEP_PHASE_ENGINE_A,
    SWEEP_PHASE_IDENTITIES,
    announce_phase,
    engine_cycle_phase,
    unregistered_selectable_probe_keys,
)
import probe_runner_diagnostics  # type: ignore
from probe_runner_registry import PROBES  # type: ignore

import selftest  # noqa: E402
from selftest import FAILURES, expect  # noqa: E402


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


def main() -> int:
    selftest.parse_verbose()
    test_todays_selectable_keys_are_all_registered()
    test_a_key_the_registry_drops_is_caught()
    test_every_stale_key_is_named_at_once()
    test_empty_selectable_list_has_nothing_stale()
    test_every_required_sweep_phase_is_declared()
    test_an_undeclared_phase_is_refused_rather_than_emitted()
    test_every_declared_phase_emits_a_record_the_runner_recognizes()
    test_the_latest_sweep_phase_survives_a_long_tail()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftest.concluded(1)
    return selftest.concluded(
        0, "\nAll persistence_contract_sweep registry-drift and "
        "phase-record tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
