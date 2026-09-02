#!/usr/bin/env python3
"""Unit tests for the `/deflake` orchestrator (#1436).

Deterministic, engine-free and GPU-free. Every collaborator `deflake`
depends on is a keyword seam, so the whole ordering is asserted with
injected adapters and synthetic state: no probe is ever run, no engine
booted, no port opened, and the developer's own `docs-wip` census is
never touched — every census here is a throwaway file in a temporary
directory.

Two things are deliberately REAL rather than faked, because faking them
would move the assertion off the thing being tested:

* `probe_census` itself. The census claims — a harness error appends one
  non-accepted attempt and changes no cohort, sample or aggregate; a
  pre-replacement failure leaves the authoritative bytes untouched; a
  post-replacement one leaves them CHANGED — are properties of the
  shipped recorder, so they are driven through it against a real
  synthetic census file. The post-replacement case fails the real
  directory fsync inside `_atomic_replace`, which is the only way to
  reach the state it describes.
* `probe_flake.Measurement`. Result documents are built by constructing
  a real measurement out of real `RunRecord`s, so what reaches the
  recorder is what the harness would actually emit and is schema-valid
  for the same reason.

The cross-process resource combinations themselves belong to
`tools/test_probe_resource_lock.py`; this gate covers `/deflake`'s
handling of them, plus one end-to-end case against a REAL foreign
holder in a REAL second process, which is the `/deflake`-versus-
`run_probes` conflict direction (`tools/test_run_probes.py` owns the
other).

Composition (#2093)
-------------------
This module is composition and selection only -- it holds no case body.
The 50 cases live with three independently delivered contract owners,
each of which keeps its own inventory in `CASES`:

  `deflake_selftest_orchestration`  the #1436 select/claim/measure/record
                                    orchestration: the fixed measurement
                                    settings, selection outcomes, claims,
                                    resources, commit cohorts, recorder
                                    outcomes, the CLI and low-level
                                    boundaries, interruption and cleanup
                                    (31 cases);
  `deflake_selftest_handoff`        the #1659 diagnosis handoff: naming,
                                    the embedded result, observed
                                    invocation data, targets, the
                                    configuration manifest, the installed
                                    census row, outcome eligibility,
                                    writer failures and the real writer
                                    (15 cases);
  `deflake_selftest_preparation`    #1913's engine preparation BEFORE the
                                    measurement's hold: ordering,
                                    namespace agreement, preparation
                                    failure, and the real preparation
                                    against the real hold (4 cases).

`deflake_selftest_support` is the single source of everything they
share: the assertion helper and the ONE failure accumulator behind it,
the temporary census, claim and artifact tree, the real
`probe_flake.Measurement` builder, the fake claim, and the recording,
resource and engine-preparation adapters behind `run`.

The aggregate runs the owners in `AGGREGATE_OWNER_ORDER`, each owner's
cases in its own declared order, which is the sequence this gate has
always run in: the orchestration cases, then the handoff's, then the
preparation's.

Three checks run before any case does, on every invocation including a
focused one, so no arrangement of owners can report a pass having run
less than it claims. `inventories()` refuses an owner that has stopped
declaring `CASES` and one that declares an empty inventory; `_resolve`
refuses a case two owners both claim and one an owner lists twice; and
the aggregate order is required to name every owner exactly once, so an
owner cannot drop out of the aggregate while still answering to
`--only`. One check runs after: the selected cases run inside
`deflake_selftest_support.seams_restored`, which asserts that every
module global, environment variable and patched function a case reaches
is back where it started, so a focused owner cannot leak a seam into
whatever runs next in the same interpreter.

Usage:
  python3 tools/test_deflake.py                       every case, in order
  python3 tools/test_deflake.py --only orchestration  one owner's cases
  python3 tools/test_deflake.py --only handoff
  python3 tools/test_deflake.py --only preparation
The bare form is the gate: CI and `tools/ci-local.sh` invoke that and
nothing else. The focused forms are for iteration and each runs its own
owner's cases only, in their declared order, in a fresh process. Every
invocation reports how many cases it ran; an unrecognized argument or an
unknown selector is a usage error, never a fall-through to the aggregate.
Exit codes: 0 = all selected tests passed, 1 = one or more failed,
2 = usage error.
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import deflake_selftest_handoff as handoff_owner  # type: ignore  # noqa: E402
import deflake_selftest_orchestration as orchestration_owner  # type: ignore  # noqa: E402
import deflake_selftest_preparation as preparation_owner  # type: ignore  # noqa: E402
import deflake_selftest_support as support  # type: ignore  # noqa: E402
import selftestlib  # noqa: E402

#: The three contract owners, by selector name.
OWNERS = {
    "orchestration": orchestration_owner,
    "handoff": handoff_owner,
    "preparation": preparation_owner,
}

#: The aggregate's run sequence: every owner, in this order, each
#: contributing its `CASES` in the order it declares them. This is the
#: order the gate has always run in -- the split landed on the section
#: boundaries of one file whose call list ran the sections back to back.
AGGREGATE_OWNER_ORDER = ("orchestration", "handoff", "preparation")


class CompositionError(Exception):
    """The owners and the run sequence no longer agree."""


def inventories() -> dict:
    """Every owner's declared cases.

    Both degenerate shapes are composition failures rather than owners
    with nothing to run, and they are checked here -- before anything
    is selected -- so that an owner losing its inventory is reported as
    itself. An ABSENT owner has stopped declaring `CASES` at all; an
    EMPTY one still declares it, which is the shape that would
    otherwise let a focused command collect nothing and exit 0.
    """
    found = {}
    for owner_key in AGGREGATE_OWNER_ORDER:
        module = OWNERS[owner_key]
        cases = getattr(module, "CASES", None)
        if cases is None:
            raise CompositionError(
                f"owner {owner_key!r} ({module.__name__}) declares no CASES "
                f"inventory")
        if not cases:
            raise CompositionError(
                f"owner {owner_key!r} ({module.__name__}) declares an EMPTY "
                f"inventory -- refusing to report a vacuous pass")
        found[owner_key] = list(cases)
    return found


def _resolve(by_owner=None) -> list:
    """The aggregate sequence as `(owner, case)` pairs.

    Refuses the two ways the "exactly once" rule can silently break: a
    case declared by two owners, and a case an owner lists twice. The
    order itself is checked first -- every owner named exactly once --
    because an owner missing from it would never be consulted at all.

    The parameter defaults at CALL time, not at import, so a test that
    substitutes an inventory to prove one of these checks bites reaches
    the substituted value.
    """
    if sorted(AGGREGATE_OWNER_ORDER) != sorted(OWNERS):
        raise CompositionError(
            f"the aggregate order names {sorted(AGGREGATE_OWNER_ORDER)} "
            f"but the owners are {sorted(OWNERS)}")
    by_owner = inventories() if by_owner is None else by_owner

    seen: dict[str, str] = {}
    resolved = []
    for owner_key in AGGREGATE_OWNER_ORDER:
        for case in by_owner[owner_key]:
            name = getattr(case, "__name__", repr(case))
            if not callable(case):
                raise CompositionError(
                    f"owner {owner_key!r} lists {name!r}, which is not a "
                    f"case")
            if name in seen:
                other = seen[name]
                raise CompositionError(
                    f"case {name!r} is declared twice by {owner_key!r}"
                    if other == owner_key else
                    f"case {name!r} is declared by both {other!r} and "
                    f"{owner_key!r}")
            seen[name] = owner_key
            resolved.append((owner_key, case))
    return resolved


def selected_cases(only: str | None) -> list:
    """The cases one invocation runs.

    With no selector that is the whole sequence; with one it is that
    owner's cases alone, in the same order they hold in the full run.
    Either way the composition check above has already run, so a
    focused command cannot pass while a sibling owner has gone missing.
    """
    resolved = _resolve()
    if only is None:
        return [case for _owner, case in resolved]
    return [case for owner_key, case in resolved if owner_key == only]


def case_count_line(only: str | None, count: int) -> str:
    """What an invocation reports having run, in a form a check can read."""
    if only is not None:
        return f"ran {count} case(s) [--only {only}]"
    by_owner = inventories()
    breakdown = ", ".join(f"{len(by_owner[key])} {key}"
                          for key in AGGREGATE_OWNER_ORDER)
    return f"ran {count} case(s): {breakdown}"


def main(argv: list[str] | None = None) -> int:
    # This parser is the self-test's own. `deflake.main`'s parser is a
    # different object that one orchestration case introspects to prove
    # it offers no `--only`, `--probe`, `--runs` or `--rts-caps`; sharing
    # or deriving one from the other would fail that case.
    parser = argparse.ArgumentParser(
        description="Self-test for tools/deflake.py (#1436).",
        epilog="With no selector, every case runs in the full sequence.")
    parser.add_argument(
        "--only", choices=list(AGGREGATE_OWNER_ORDER),
        help="run one contract owner's cases only.")
    # This module owns its own command line, so the shared verbosity
    # flag joins that parser rather than being consumed behind its
    # back; `begin` then starts this invocation's own count (#1922).
    selftestlib.add_verbose_option(parser)
    args = parser.parse_args(argv)
    selftestlib.begin(args.verbose)

    try:
        cases = selected_cases(args.only)
    except CompositionError as error:
        print(f"deflake self-test composition error: {error}")
        return 1

    if not cases:
        # Unreachable through the checks above -- `inventories()` has
        # already refused an absent or empty owner. Kept as the last
        # word on the invariant the whole selection exists to hold:
        # this gate never reports a pass having run nothing.
        target = "the aggregate" if args.only is None else f"--only {args.only}"
        print(f"no cases collected for {target} -- refusing to report a "
              f"vacuous pass")
        return 1

    with support.seams_restored():
        for case in cases:
            case()
    print(f"\n{case_count_line(args.only, len(cases))}")

    if support.FAILURES:
        print(f"\n{len(support.FAILURES)} test(s) failed:")
        for failure in support.FAILURES:
            print(f"  {failure}")
        return selftestlib.concluded(1)
    if args.only is None:
        return selftestlib.concluded(
            0, "\nAll deflake orchestration tests passed")
    return selftestlib.concluded(
        0, f"\nAll deflake tests passed [--only {args.only}]")


if __name__ == "__main__":
    raise SystemExit(main())
