#!/usr/bin/env python3
"""The one assertion helper the ``tools/test_*.py`` self-tests share (#1922).

Thirty of those scripts each carried a copy of the same six-line
``expect``, and every copy printed a line for each **passing**
assertion. ``make ci`` and CI run most of them, so that narration was a
fixed per-run cost measured in thousands of lines:
``tools/test_audit.py`` alone printed 250 ``OK:`` lines above a summary
that already said ``All 62 test groups passed``, and the
probe-orchestration self-tests printed hundreds more each.

This module owns the helper once and inverts the default. A satisfied
assertion is silent; a failed one always prints and always registers;
per-assertion detail is one ``-v``/``--verbose`` away. Nothing about
what is asserted changes -- the conditions, the messages, and each
script's own exit status are exactly what they were.

``tools/probelib.py`` (#529) is the in-repo precedent for a shared
``tools/`` module, and the source of this one's name. ``selftest`` was
the obvious name and is not available: ``tools/playtest/selftest.py``
(#2040) owns it, and ``tools/playtest/engine.py``, ``critic.py`` and
``personas.py`` each put ``tools/`` ahead of ``tools/playtest/`` on
``sys.path``, so a ``tools/selftest.py`` shadows theirs. Anything run
as ``python3 tools/<name>.py`` has ``tools/`` on ``sys.path`` already,
so importing this needs no path manipulation.

Converting a self-test is three edits::

    import selftestlib
    from selftestlib import FAILURES, expect   # replaces the local pair

    def main() -> int:
        selftestlib.parse_verbose()            # or add_verbose_option()
        ...
        if FAILURES:
            ...unchanged failure reporting...
            return selftestlib.concluded(1)
        return selftestlib.concluded(0, "...unchanged passing summary...")

``FAILURES`` is deliberately this module's own list rather than a
per-script one. ``from selftestlib import FAILURES`` binds the same
list object, so every script's existing ``if FAILURES`` /
``len(FAILURES)`` reporting keeps working untouched, and a helper
defined here can append to it.

**One consequence: this state is per-INTERPRETER, not per-invocation**,
and a converted ``main`` is importable and callable more than once --
two of them take an explicit ``argv`` precisely so they can be. So
every entry point begins with `begin`, which forgets the previous run
before counting this one. Without it a second ``main()`` in one process
would add its assertions to its predecessor's, inherit its failures,
and stay verbose because the FIRST run was passed ``-v``.
`parse_verbose` calls `begin` for the scripts that take no other
options; a script owning its own parser calls it directly with the
parsed flag.

**Silence needs a vacuity guard.** With the per-assertion lines gone, a
self-test whose case registry was silently emptied would print its
passing summary and exit 0 with nothing to reveal that it had checked
nothing. `concluded` therefore refuses a run that executed no assertion
at all, and states the tally on every run that did.
``tools/test_pack_atlas.py``'s registry-truncation guard is the same
idea, for a suite that never had per-assertion lines to lose.
"""
from __future__ import annotations

import sys

__all__ = [
    "FAILURES",
    "add_verbose_option",
    "assertions",
    "begin",
    "concluded",
    "expect",
    "parse_verbose",
    "record_fail",
    "record_pass",
    "set_verbose",
    "verbose",
]

# Both spellings, everywhere -- a flag that means one thing in one
# self-test and nothing in the next is worse than no flag at all.
VERBOSE_FLAGS = ("-v", "--verbose")
VERBOSE_HELP = "print a line per passing assertion, not just the failures"

#: Every failed assertion's registered text, in the order it failed.
FAILURES: list[str] = []

_verbose = False
_assertions = 0


def set_verbose(flag: bool) -> None:
    """Turn per-assertion success narration on or off."""
    global _verbose
    _verbose = bool(flag)


def verbose() -> bool:
    """Whether passing assertions are currently being narrated."""
    return _verbose


def begin(verbose: bool = False) -> None:
    """Start one invocation: forget the last one, set this one's verbosity.

    Every converted entry point calls this before its first assertion,
    which is what makes an invocation's assertion count, failure list
    and verbosity its own rather than the interpreter's. Calling
    ``main()`` twice in one process is supported -- `test_probe_claim`
    and `test_save_compat_audit` both accept an explicit ``argv`` for
    exactly that -- and without this the second call would report the
    first's assertions and failures alongside its own.
    """
    global _assertions
    _assertions = 0
    FAILURES.clear()
    set_verbose(verbose)


def parse_verbose(argv: list[str] | None = None) -> list[str]:
    """Begin an invocation, taking its verbosity from ``argv``.

    Consumes ``-v``/``--verbose`` and returns what is left. Deliberately
    permissive about the rest: the self-tests that call this took no
    options at all and ignored whatever they were handed, so rejecting
    an unrecognized argument here would be a CLI change rather than the
    presentation change this module exists for. A script that already
    parses its own arguments calls `add_verbose_option` instead, so its
    parser keeps deciding what is and is not valid -- and then calls
    `begin` itself with the parsed flag.
    """
    args = list(sys.argv[1:] if argv is None else argv)
    kept = [a for a in args if a not in VERBOSE_FLAGS]
    begin(len(kept) != len(args))
    return kept


def add_verbose_option(parser) -> None:
    """Give an existing ``argparse`` parser the same two spellings."""
    parser.add_argument(*VERBOSE_FLAGS, action="store_true", help=VERBOSE_HELP)


def assertions() -> int:
    """How many assertions this invocation has executed, pass or fail."""
    return _assertions


def record_pass(msg: str) -> None:
    """Count a satisfied assertion, narrating it only under ``--verbose``."""
    global _assertions
    _assertions += 1
    if _verbose:
        print(f"  OK:   {msg}")


def record_fail(entry: str, shown: str | None = None) -> None:
    """Count a failed assertion, register it, and always report it.

    ``shown`` is for the helpers whose printed detail differs from the
    text they register -- ``expect_raises`` registers a summary and
    prints the exception it actually got. When it is absent the printed
    and registered strings are the same one.
    """
    global _assertions
    _assertions += 1
    FAILURES.append(entry)
    print(f"  FAIL: {shown if shown is not None else entry}")


def expect(cond: bool, msg: str) -> bool:
    """The shared assertion: quiet when satisfied, loud and recorded when not.

    Returns the condition, so a caller may branch on it; the twenty-nine
    converted scripts inherited a helper returning ``None`` and none of
    them does.
    """
    if cond:
        record_pass(msg)
    else:
        record_fail(msg)
    return bool(cond)


def concluded(status: int, summary: str | None = None) -> int:
    """Close a self-test: state the tally, refuse a run that asserted nothing.

    ``status`` is the caller's own verdict and ``summary`` its own
    passing summary, printed verbatim with the tally the per-assertion
    lines used to imply appended to it. The failing path passes no
    summary, having already reported each failure in its own words.

    A run that executed no assertion at all is overridden to a failure
    whatever it believed, and its summary is never printed -- a silently
    emptied case registry has no other tell once the narration is gone,
    and "all tests passed" is the one thing it must not be able to say.
    """
    if _assertions == 0:
        print("no assertion executed -- an emptied case registry must not "
              "read as a pass", file=sys.stderr)
        return 1
    plural = "" if _assertions == 1 else "s"
    tally = f"({_assertions} assertion{plural} executed)"
    print(tally if summary is None else f"{summary} {tally}")
    return status
