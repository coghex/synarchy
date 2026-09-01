#!/usr/bin/env python3
"""Self-test for tools/selftestlib.py, the shared assertion helper (#1922).

Two halves, and both are load-bearing.

**Behaviour**, proved by running generated fixture scripts in fresh
interpreters: quiet by default, per-assertion detail under either
verbose spelling, a failure that always prints and always registers, an
assertion tally that counts pass and fail alike, and the vacuity guard
that refuses a run which executed no assertion at all. A fixture is a
real process, so each observes an invocation's own count from zero --
the property `selftestlib.concluded` depends on and the property an
in-process check could accidentally satisfy by sharing this script's
state.

**Conversion**, proved statically over every ``tools/`` file that
imports the module: none defines a local assertion helper or registers a
failure behind the count, and each one that is a self-test in its own
right routes both verdicts through `selftestlib.concluded` and offers
the verbose flag. Plus the tree-wide search requirement 1 states -- the
narrating body survives in the shared module and nowhere else.

The static half is deliberately not "run all thirty and diff": CI
already runs most of them, several take minutes, and one drives
``cabal repl``. What CI cannot notice is a script that quietly stopped
importing the shared helper, which is what these checks are for.

Usage:
  python3 tools/test_selftestlib.py [-v]
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import ast
import re
import subprocess
import sys
import tempfile
from pathlib import Path

import selftestlib
from selftestlib import FAILURES, expect

TOOLS = Path(__file__).resolve().parent

IMPORT = re.compile(
    r"^(import selftestlib\b|from selftestlib import )", re.M)

#: Everything under `tools/` that imports the shared helper, in either
#: spelling. Deriving the roster instead of freezing it means a newly
#: converted file joins these checks the day it lands.
IMPORTERS = sorted(
    path for path in TOOLS.glob("*.py")
    if path.name not in {"selftestlib.py", Path(__file__).name}
    and IMPORT.search(path.read_text(encoding="utf-8")))

#: The subset that is a self-test in its own right, and so has a verdict
#: to route and a command line to carry the flag. The rest are shared
#: modules a self-test is composed from -- since #2100 the probe-claim
#: gate keeps its assertion helper in `probe_claim_selftest_support`,
#: which owns no `main` and offers no CLI.
SCRIPTS = [path for path in IMPORTERS if path.name.startswith("test_")]

#: #1922 converted thirty self-tests, one of them (`test_probe_claim.py`)
#: through a shared support module. A roster that shrinks below either
#: figure is a file that stopped importing the helper, which is exactly
#: the regression the static half exists to catch -- and, like every
#: check here, one an emptied glob would otherwise report as green.
MINIMUM_SCRIPTS = 30
MINIMUM_IMPORTERS = 31

#: The two narrating bodies #1922 removed. Requirement 1: a tree-wide
#: search finds them only in the shared module.
NARRATION = re.compile(r'print\(f"  (OK:|\{.ok  .)')

FIXTURE = '''\
import sys
sys.path.insert(0, {tools!r})
import selftestlib
from selftestlib import FAILURES, expect

def main() -> int:
    selftestlib.parse_verbose()
{body}
    if FAILURES:
        print(f"{{len(FAILURES)}} failed:")
        for message in FAILURES:
            print(f"  {{message}}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(0, "fixture passed")

raise SystemExit(main())
'''


def run_fixture(body: str, *argv: str) -> subprocess.CompletedProcess:
    """One fixture script, in its own interpreter, with its own state."""
    with tempfile.TemporaryDirectory() as tmp:
        script = Path(tmp) / "fixture.py"
        script.write_text(
            FIXTURE.format(tools=str(TOOLS),
                           body="\n".join(f"    {line}" for line in body)),
            encoding="utf-8")
        return subprocess.run([sys.executable, str(script), *argv],
                              capture_output=True, text=True)


TWO_PASSES = ['expect(1 == 1, "one holds")', 'expect(2 == 2, "two holds")']

#: Two in-process runs of a real converted self-test, the first verbose.
#: `test_determinism` is the cheapest one that touches nothing outside
#: itself: five pure cases, nine assertions, no subprocess and no disk.
SEQUENTIAL = """\
import contextlib, io, sys
sys.path.insert(0, {tools!r})
import selftestlib
import test_determinism

def run(argv):
    sys.argv = ["test_determinism.py", *argv]
    out = io.StringIO()
    with contextlib.redirect_stdout(out):
        test_determinism.main()
    return selftestlib.assertions(), len(selftestlib.FAILURES), \
        out.getvalue().count("  OK:   ")

first = run(["-v"])
second = run([])
print(f"counts {{first[0]}} then {{second[0]}}")
print(f"failures {{first[1]}} then {{second[1]}}")
print(f"narration {{first[2]}} then {{second[2]}}")
"""


def run_program(source: str, *argv: str) -> subprocess.CompletedProcess:
    """A whole generated program, in its own interpreter."""
    with tempfile.TemporaryDirectory() as tmp:
        script = Path(tmp) / "program.py"
        script.write_text(source.format(tools=str(TOOLS)), encoding="utf-8")
        return subprocess.run([sys.executable, str(script), *argv],
                              capture_output=True, text=True)


# ----- Behaviour -----------------------------------------------------------

def test_a_passing_run_narrates_nothing() -> None:
    result = run_fixture(TWO_PASSES)
    expect(result.returncode == 0,
           f"a passing fixture exits 0 (got {result.returncode}: "
           f"{result.stderr.strip()})")
    expect("OK:" not in result.stdout,
           f"and prints no per-assertion success line ({result.stdout!r})")
    expect("fixture passed" in result.stdout,
           "while still printing its own summary")


def test_the_summary_carries_the_tally() -> None:
    result = run_fixture(TWO_PASSES)
    expect("fixture passed (2 assertions executed)" in result.stdout,
           f"the tally rides on the script's own summary line "
           f"({result.stdout!r})")


def test_one_assertion_is_singular() -> None:
    result = run_fixture(['expect(True, "the only one")'])
    expect("(1 assertion executed)" in result.stdout,
           f"a single assertion is not pluralized ({result.stdout!r})")


def test_both_verbose_spellings_restore_the_detail() -> None:
    for flag in ("-v", "--verbose"):
        result = run_fixture(TWO_PASSES, flag)
        expect(result.returncode == 0,
               f"{flag} still exits 0 (got {result.returncode})")
        expect(result.stdout.count("  OK:   ") == 2,
               f"{flag} narrates every passing assertion ({result.stdout!r})")
        expect("one holds" in result.stdout and "two holds" in result.stdout,
               f"{flag} narrates each assertion's own message")


def test_an_unrelated_argument_is_left_alone() -> None:
    # These scripts took no options and ignored whatever they were
    # handed; adding the flag must not turn that into a usage error.
    result = run_fixture(TWO_PASSES, "--not-a-flag-this-script-knows")
    expect(result.returncode == 0,
           f"an unknown argument is still ignored (got {result.returncode}: "
           f"{result.stderr.strip()})")
    expect("OK:" not in result.stdout,
           "and does not accidentally enable narration")


def test_a_failure_prints_without_the_flag() -> None:
    result = run_fixture(['expect(False, "this one does not hold")'])
    expect(result.returncode == 1,
           f"a failing fixture exits 1 (got {result.returncode})")
    expect("  FAIL: this one does not hold" in result.stdout,
           f"the failure prints in the default quiet mode ({result.stdout!r})")
    expect("1 failed:" in result.stdout,
           "and registers, so the script's own reporting sees it")
    expect("fixture passed" not in result.stdout,
           "and the passing summary is not printed")


def test_a_failure_is_counted_like_a_pass() -> None:
    result = run_fixture(['expect(True, "holds")', 'expect(False, "does not")'])
    expect("(2 assertions executed)" in result.stdout,
           f"the tally counts both outcomes ({result.stdout!r})")


def test_the_failing_verdict_still_states_its_tally() -> None:
    result = run_fixture(['expect(False, "does not hold")'])
    expect("(1 assertion executed)" in result.stdout,
           f"a failing run reports what it ran too ({result.stdout!r})")


def test_a_run_that_asserts_nothing_is_a_failure() -> None:
    # The whole point of the guard: with the narration gone, an emptied
    # case registry has no other tell.
    result = run_fixture(['pass'])
    expect(result.returncode == 1,
           f"a fixture with no assertion exits nonzero (got "
           f"{result.returncode})")
    expect("no assertion executed" in result.stderr,
           f"and says why, on stderr ({result.stderr!r})")
    expect("fixture passed" not in result.stdout,
           f"and never claims to have passed ({result.stdout!r})")


def test_record_fail_can_show_more_than_it_registers() -> None:
    # `expect_raises` registers a summary and prints the exception it
    # actually saw; both halves have to survive.
    result = run_fixture(
        ['selftestlib.record_fail("registered text", "shown detail")'])
    expect("  FAIL: shown detail" in result.stdout,
           f"the shown text is what prints ({result.stdout!r})")
    expect("  registered text" in result.stdout,
           "while the registered text is what the failure list carries")
    expect(result.returncode == 1, "and the run fails")


def test_record_pass_obeys_the_same_default() -> None:
    result = run_fixture(['selftestlib.record_pass("a bare pass")'])
    expect("a bare pass" not in result.stdout,
           f"record_pass is quiet by default ({result.stdout!r})")
    verbose = run_fixture(['selftestlib.record_pass("a bare pass")'], "-v")
    expect("  OK:   a bare pass" in verbose.stdout,
           f"and narrates under --verbose ({verbose.stdout!r})")


def test_each_invocation_counts_from_zero() -> None:
    one = run_fixture(['expect(True, "a")'])
    three = run_fixture(['expect(True, "a")', 'expect(True, "b")',
                         'expect(True, "c")'])
    expect("(1 assertion executed)" in one.stdout
           and "(3 assertions executed)" in three.stdout,
           f"two invocations count independently ({one.stdout!r} / "
           f"{three.stdout!r})")


def test_a_second_invocation_in_one_process_counts_only_itself() -> None:
    # A converted `main` is importable and callable again, and two of
    # them take an explicit argv precisely so it can be. Without
    # `begin`, the second call reports the first's assertions too.
    result = run_program(SEQUENTIAL)
    expect(result.returncode == 0,
           f"the sequential driver runs (got {result.returncode}: "
           f"{result.stderr.strip()})")
    expect("counts 9 then 9" in result.stdout,
           f"a second main() in one process counts only its own "
           f"assertions ({result.stdout!r})")
    expect("failures 0 then 0" in result.stdout,
           "and starts from an empty failure list rather than inheriting one")


def test_verbosity_does_not_leak_into_the_next_invocation() -> None:
    result = run_program(SEQUENTIAL)
    expect("narration 9 then 0" in result.stdout,
           f"a -v run does not leave the next quiet run narrating "
           f"({result.stdout!r})")


# ----- Conversion ----------------------------------------------------------

def test_the_roster_is_not_truncated() -> None:
    expect(len(SCRIPTS) >= MINIMUM_SCRIPTS,
           f"at least {MINIMUM_SCRIPTS} self-tests import the shared helper "
           f"(found {len(SCRIPTS)}: {sorted(p.name for p in SCRIPTS)})")
    expect(len(IMPORTERS) >= MINIMUM_IMPORTERS,
           f"at least {MINIMUM_IMPORTERS} tools/ files import it in total "
           f"(found {len(IMPORTERS)}: {sorted(p.name for p in IMPORTERS)})")


def test_no_importer_keeps_a_local_helper() -> None:
    for path in IMPORTERS:
        tree = ast.parse(path.read_text(encoding="utf-8"))
        local = [node.name for node in tree.body
                 if isinstance(node, ast.FunctionDef) and node.name == "expect"]
        expect(not local,
               f"{path.name} defines no local expect (found {local})")


def test_no_importer_registers_a_failure_behind_the_count() -> None:
    # A direct `FAILURES.append` would report a failure the tally never
    # saw, which is the one way a converted file can still miscount.
    for path in IMPORTERS:
        text = path.read_text(encoding="utf-8")
        expect("FAILURES.append" not in text,
               f"{path.name} registers failures through the helper, not by "
               f"appending to FAILURES directly")


def test_every_converted_script_routes_both_verdicts() -> None:
    for path in SCRIPTS:
        text = path.read_text(encoding="utf-8")
        expect("return selftestlib.concluded(1)" in text,
               f"{path.name}'s failing verdict goes through concluded()")
        expect(re.search(r"return selftestlib\.concluded\(\s*0", text) is not None,
               f"{path.name}'s passing verdict goes through concluded()")


def test_every_converted_script_offers_the_flag() -> None:
    for path in SCRIPTS:
        text = path.read_text(encoding="utf-8")
        expect("selftestlib.parse_verbose()" in text
               or "selftestlib.add_verbose_option(" in text,
               f"{path.name} accepts -v/--verbose")


def test_the_narrating_body_survives_only_in_the_module() -> None:
    narrating = sorted(
        path.name for path in TOOLS.glob("*.py")
        if NARRATION.search(path.read_text(encoding="utf-8")))
    expect(narrating == ["selftestlib.py"],
           f"only the shared module narrates a passing assertion "
           f"(found {narrating})")


TESTS = [
    test_a_passing_run_narrates_nothing,
    test_the_summary_carries_the_tally,
    test_one_assertion_is_singular,
    test_both_verbose_spellings_restore_the_detail,
    test_an_unrelated_argument_is_left_alone,
    test_a_failure_prints_without_the_flag,
    test_a_failure_is_counted_like_a_pass,
    test_the_failing_verdict_still_states_its_tally,
    test_a_run_that_asserts_nothing_is_a_failure,
    test_record_fail_can_show_more_than_it_registers,
    test_record_pass_obeys_the_same_default,
    test_each_invocation_counts_from_zero,
    test_a_second_invocation_in_one_process_counts_only_itself,
    test_verbosity_does_not_leak_into_the_next_invocation,
    test_the_roster_is_not_truncated,
    test_no_importer_keeps_a_local_helper,
    test_no_importer_registers_a_failure_behind_the_count,
    test_every_converted_script_routes_both_verdicts,
    test_every_converted_script_offers_the_flag,
    test_the_narrating_body_survives_only_in_the_module,
]


def main() -> int:
    selftestlib.parse_verbose()
    for test in TESTS:
        print(f"{test.__name__}:")
        test()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(0, f"\nAll {len(TESTS)} selftestlib tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
