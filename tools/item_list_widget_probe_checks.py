#!/usr/bin/env python3
"""Aggregate check and failure accounting for
`tools/item_list_widget_probe.py` (#2046).

The lowest layer of the split: it imports no sibling module and no
engine surface (only the runner's leaf failure-record protocol), and owns the one piece of state the whole run shares —
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

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probe_runner_diagnostics import FailureEmitter

failures = 0

# Every failed check is ALSO a durable failure record (#2802), so the
# runner's default `--tail` presentation names it however much output
# followed. Constructed at module scope: the elapsed offset each record
# carries is measured from the probe's own start.
FAILURE = FailureEmitter("item_list_widget_probe")

# A scenario that cannot establish its fixture labels its check with this
# prefix (#1911). The record keeps the full label in its detail AND uses
# the `setup` kind, so a fixture failure stays distinguishable from a
# graded one in the retained output.
SETUP_LABEL_PREFIX = "setup:"


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
    if not ok:
        # Only a FAILED check emits: a passing run gains no output.
        record = FAILURE.setup if name.startswith(SETUP_LABEL_PREFIX) \
            else FAILURE.check
        record(name + (f" — {detail}" if detail else ""))
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
