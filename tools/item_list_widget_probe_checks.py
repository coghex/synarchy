#!/usr/bin/env python3
"""Aggregate check and failure accounting for
`tools/item_list_widget_probe.py` (#2046).

The lowest layer of the split: it imports no sibling module and no
engine surface, and owns the one piece of state the whole run shares —
the failure accumulator every scenario writes through `check` and the
facade reads back through `probe_result`.

**The counter is module state, never a value to import.** `failures` is
an `int` rebound by `check`'s `global failures`, so
`from item_list_widget_probe_checks import failures` in the facade or in
any scenario module would bind a stale `0` that never moves: every
failed check would still print `[FAIL]` and the run would still exit 0,
which is the exact inversion of the contract `probe_result` exists to
hold. Read it through this module (`checks.failures`) or through
`failure_count()`, and never by name.

`check_no_duplicate_rows` lives here rather than with either scenario
module because it has consumers in both: the cargo, unit-endpoint and
item-contents scenarios (Endpoints) and the unit-inventory scenario
(Inventory) all call it, so it is shared support rather than one
module's implementation detail.
"""
from __future__ import annotations

failures = 0


def probe_result() -> int:
    """The run's exit status: non-zero whenever ANY check failed.

    A SETUP failure is one of those (#1911): a scenario that cannot
    establish its fixture reports it and the run is red, rather than the
    probe grading the fixture anyway and exiting green."""
    if failures:
        print(f"\nitem_list_widget_probe: {failures} check(s) FAILED")
        return 1
    print("\nitem_list_widget_probe: all checks passed")
    return 0


def check(name: str, ok: bool, detail: str = "") -> bool:
    global failures
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f" — {detail}" if detail and not ok else ""))
    failures += not ok
    return ok


def check_no_duplicate_rows(port: int, scenario: str, rows: list) -> None:
    """A rebuild that failed to tear its previous elements down would
    leave two live rows carrying the same widget id."""
    ids = [r.get("id") for r in rows]
    check(f"{scenario}: no duplicate row records after rebuilds",
          len(ids) == len(set(ids)), f"got {ids!r}")


def failure_count() -> int:
    """How many checks have failed so far.

    An accessor for callers that want the count without reaching for the
    module attribute; either is correct, `from ... import failures` is
    not (see the module docstring)."""
    return failures
