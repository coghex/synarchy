#!/usr/bin/env python3
"""Unit tests for persistence_contract_sweep.py's registry-drift guard
(issue #1321).

`SELECTABLE_CROSS_REFERENCED_PROBE_KEYS` is a hand-maintained copy of a
subset of `tools/run_probes.py`'s `PROBES` registry. Nothing previously
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
    unregistered_selectable_probe_keys,
)
from run_probes import PROBES  # type: ignore

FAILURES: list[str] = []


def expect(cond: bool, msg: str) -> None:
    if not cond:
        FAILURES.append(msg)
        print(f"  FAIL: {msg}")
    else:
        print(f"  OK:   {msg}")


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


def main() -> int:
    test_todays_selectable_keys_are_all_registered()
    test_a_key_the_registry_drops_is_caught()
    test_every_stale_key_is_named_at_once()
    test_empty_selectable_list_has_nothing_stale()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return 1
    print("\nAll persistence_contract_sweep registry-drift tests passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
