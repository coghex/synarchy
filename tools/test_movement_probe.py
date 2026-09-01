#!/usr/bin/env python3
"""Unit tests for the movement probe's no-boot course listing (issue #1586).

`tools/movement_probe.py` is manual-only: its real acceptance is a long
engine run, so the half that must not regress silently is the cheap one.
`--list` is a metadata query and answers from `scripts/movement_arena.lua`
before any `boot()`, and the derived view is held to the runtime authority
`M.listCourses()` by every real course run.

What is pinned here, because each is a way the listing has drifted or
would drift again:

  * `--list` boots NOTHING. The engine launcher is replaced by one that
    raises, so a reintroduced `boot()` on the listing path fails loudly
    instead of merely costing a minute.
  * `--list` is honoured for every `--mode`. The mode dispatch used to
    return first, so `--list --mode stamina` ran the stamina probe.
  * The inventory is DERIVED from the Lua module, not a hand-kept Python
    table. `wander_ledge` is runnable and appears in neither `VALIDATORS`
    nor `GOAL_Z`, so it is asserted by name: it is exactly the course a
    hand-kept list loses.
  * An unreadable, non-UTF-8, structurally broken, empty or ambiguous
    source is a non-zero exit naming the file, never a silently empty
    list and never an uncaught traceback.
  * The drift guard reports the two directions of difference separately,
    so its message never depends on set iteration order.

No engine, no world, no GPU: every test here runs against temporary
files in well under a second.

Usage:
  python3 tools/test_movement_probe.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import contextlib
import io
import sys
import tempfile
from pathlib import Path

TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))
import movement_probe as probe  # type: ignore  # noqa: E402

import selftest  # noqa: E402
from selftest import FAILURES, expect  # noqa: E402


@contextlib.contextmanager
def lua_source(text: str):
    """Point the probe's course source at a throwaway Lua module."""
    with tempfile.TemporaryDirectory() as tmp:
        path = Path(tmp) / "movement_arena.lua"
        path.write_text(text, encoding="utf-8")
        original = probe.COURSE_SOURCE
        probe.COURSE_SOURCE = path
        try:
            yield path
        finally:
            probe.COURSE_SOURCE = original


@contextlib.contextmanager
def no_engine():
    """Fail the test if anything on the exercised path launches an engine."""
    original_boot = probe.boot
    original_bootstrap = probe.bootstrap
    original_send_json = probe.send_json

    def forbidden(*_args, **_kwargs):
        raise AssertionError("the listing path launched an engine")

    probe.boot = forbidden
    probe.bootstrap = forbidden
    probe.send_json = forbidden
    try:
        yield
    finally:
        probe.boot = original_boot
        probe.bootstrap = original_bootstrap
        probe.send_json = original_send_json


def run_main(argv: list[str]) -> tuple[int, str, str]:
    out, err = io.StringIO(), io.StringIO()
    original_argv = sys.argv
    sys.argv = ["movement_probe.py", *argv]
    try:
        with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
            code = probe.main()
    finally:
        sys.argv = original_argv
    return code, out.getvalue(), err.getvalue()


# --------------------------------------------------------------------------
# The shipped module
# --------------------------------------------------------------------------
def test_the_shipped_inventory_is_complete_and_sorted() -> None:
    print("\nthe shipped scripts/movement_arena.lua")
    names = probe.derive_courses()
    expect(names == sorted(names), "derived names come back sorted")
    expect(len(names) == len(set(names)), "derived names are unique")
    expect(len(names) >= 11, f"at least the 11 shipped courses ({len(names)})")
    # The course that proves the listing is derived rather than hand-kept:
    # runnable through the shared arena module, present in neither of the
    # probe's Python tables.
    expect("wander_ledge" in names, "wander_ledge is listed")
    expect("wander_ledge" not in probe.VALIDATORS
           and "wander_ledge" not in probe.GOAL_Z,
           "wander_ledge is still absent from the probe's Python tables, so "
           "the listing cannot be coming from them")
    for label, table in (("VALIDATORS", probe.VALIDATORS),
                         ("GOAL_Z", probe.GOAL_Z)):
        missing = sorted(set(table) - set(names))
        expect(not missing,
               f"every {label} course is a real registration ({missing})")


def test_list_prints_the_inventory_without_booting() -> None:
    print("\n--list against the shipped module")
    with no_engine():
        code, out, err = run_main(["--list"])
    expect(code == 0, f"--list exits 0 (got {code})")
    expect(err == "", f"--list writes nothing to stderr ({err!r})")
    expect(out.startswith("courses: "), f"--list prints an inventory ({out!r})")
    listed = [n.strip() for n in out[len("courses: "):].strip().split(",")]
    expect(listed == probe.derive_courses(),
           "--list prints exactly the derived names, in order")


def test_list_is_honoured_for_every_mode() -> None:
    print("\n--list under every --mode")
    with no_engine():
        baseline = run_main(["--list"])
        for mode in ("move", "stamina", "pacing"):
            code, out, err = run_main(["--list", "--mode", mode])
            expect(code == 0, f"--list --mode {mode} exits 0 (got {code})")
            expect((code, out, err) == baseline,
                   f"--list --mode {mode} prints the same inventory")


# --------------------------------------------------------------------------
# Deriving from an arbitrary module
# --------------------------------------------------------------------------
STUB = """
M.courses = {}

M.courses.alpha = function()
    return { name = "alpha" }
end

-- M.courses.commented_out = function()
M.courses.beta = function()
    return { name = "beta" }
end
"""


def test_a_new_course_appears_without_any_python_edit() -> None:
    print("\na stub module")
    with lua_source(STUB):
        expect(probe.derive_courses() == ["alpha", "beta"],
               "both registrations are derived, sorted")
        with no_engine():
            code, out, _ = run_main(["--list"])
        expect((code, out) == (0, "courses: alpha, beta\n"),
               f"--list reflects the stub source ({code}, {out!r})")
    with lua_source(STUB + "\nM.courses.gamma = function() return {} end\n"):
        expect(probe.derive_courses() == ["alpha", "beta", "gamma"],
               "adding a course to the Lua module alone extends the listing")


def test_a_commented_or_quoted_mention_is_not_a_registration() -> None:
    print("\nnon-registrations")
    with lua_source(STUB):
        expect("commented_out" not in probe.derive_courses(),
               "a commented-out declaration is not counted")
    with lua_source(STUB + '\nlocal s = "M.courses.quoted = function("\n'):
        expect("quoted" not in probe.derive_courses(),
               "a declaration inside a string is not counted")
    # A long-bracket comment spans lines, so the line-anchored pattern cannot
    # see it on its own -- and a `--list` that advertised a course the engine
    # never registers is the drift this listing exists to close.
    for opener, closer in (("--[[", "]]"), ("--[==[", "]==]")):
        blocked = (f"{opener}\nM.courses.ghost = function()\n"
                   f"    return {{ name = \"ghost\" }}\nend\n{closer}\n")
        with lua_source(STUB + blocked):
            expect(probe.derive_courses() == ["alpha", "beta"],
                   f"a registration inside a {opener} comment is not counted")
        with lua_source(blocked + STUB):
            expect(probe.derive_courses() == ["alpha", "beta"],
                   f"a {opener} comment does not swallow later registrations")
    with lua_source(STUB + "\nlocal s = [[ M.courses.long = function( ]]\n"):
        expect(probe.derive_courses() == ["alpha", "beta"],
               "a declaration inside a long string is not counted")


# --------------------------------------------------------------------------
# Setup failures are non-zero and name the file
# --------------------------------------------------------------------------
def _list_failure(source_text: str | None) -> tuple[int, str]:
    if source_text is None:
        with tempfile.TemporaryDirectory() as tmp:
            missing = Path(tmp) / "movement_arena.lua"
            original = probe.COURSE_SOURCE
            probe.COURSE_SOURCE = missing
            try:
                with no_engine():
                    code, _, err = run_main(["--list"])
            finally:
                probe.COURSE_SOURCE = original
        return code, err
    with lua_source(source_text):
        with no_engine():
            code, _, err = run_main(["--list"])
    return code, err


# Every case below is one `luac -p` also rejects, and the shipped module plus
# all 218 tracked .lua files are ones it also accepts; `luac` is NOT a runtime
# dependency of the probe, which is why the check is structural.
BROKEN = {
    "a truncated function":
        "M.courses.a = function()\n    return { name = 'a' }\n",
    "a stray 'end'":
        "M.courses.a = function() return {} end\nend\n",
    "an unterminated string":
        'M.courses.a = function() return { name = "a }\nend\n',
    "an unterminated long comment":
        "--[[ oops\nM.courses.a = function() return {} end\n",
    "an unclosed brace":
        "M.courses.a = function() return { name = 'a' end\n",
    "a 'repeat' with no 'until'":
        "M.courses.a = function() repeat x = 1 end\n",
}

INTACT = {
    "a numeric for loop":
        "M.courses.a = function() for i=1,3 do x=i end return {} end\n",
    "an if/elseif/else chain":
        "M.courses.a = function() if x then y=1 elseif z then y=2 "
        "else y=3 end return {} end\n",
    "a repeat/until loop":
        "M.courses.a = function() repeat x=1 until x>0 return {} end\n",
    "an escaped quote in a string":
        'M.courses.a = function() return { note = "a \\" b" } end\n',
}


def test_a_structurally_broken_source_is_a_named_non_zero_exit() -> None:
    print("\na structurally broken source")
    for label, source in BROKEN.items():
        code, err = _list_failure(source)
        expect(code != 0, f"{label}: --list exits non-zero (got {code})")
        expect("movement_arena.lua" in err, f"{label}: the file is named ({err!r})")
        expect("courses: " not in err, f"{label}: no inventory is printed")


def test_an_intact_source_is_still_accepted() -> None:
    print("\nan intact source using every block form")
    for label, source in INTACT.items():
        with lua_source(source):
            try:
                names = probe.derive_courses()
            except probe.CourseSourceError as exc:
                names = f"rejected: {exc}"
        expect(names == ["a"], f"{label} is accepted ({names})")


def test_a_non_utf8_source_is_a_named_non_zero_exit() -> None:
    print("\na source that is not UTF-8")
    with tempfile.TemporaryDirectory() as tmp:
        path = Path(tmp) / "movement_arena.lua"
        path.write_bytes(b"M.courses.a = function() return { n = '\xff\xfe' } end\n")
        original = probe.COURSE_SOURCE
        probe.COURSE_SOURCE = path
        try:
            with no_engine():
                code, out, err = run_main(["--list"])
        finally:
            probe.COURSE_SOURCE = original
    expect(code != 0, f"--list exits non-zero rather than raising (got {code})")
    expect("movement_arena.lua" in err, f"the file is named ({err!r})")
    expect(out == "", f"no inventory is printed ({out!r})")


def test_an_unreadable_source_is_a_named_non_zero_exit() -> None:
    print("\nan unreadable source")
    code, err = _list_failure(None)
    expect(code != 0, f"--list exits non-zero (got {code})")
    expect("movement_arena.lua" in err, f"the file is named ({err!r})")
    expect("courses:" not in err, "no inventory is printed")


def test_a_source_declaring_nothing_is_a_named_non_zero_exit() -> None:
    print("\na source declaring no courses")
    code, err = _list_failure("M.courses = {}\nreturn M\n")
    expect(code != 0, f"--list exits non-zero (got {code})")
    expect("movement_arena.lua" in err, f"the file is named ({err!r})")
    expect("no 'M.courses" in err, f"the reason is stated ({err!r})")


def test_a_duplicate_registration_is_a_named_non_zero_exit() -> None:
    print("\nan ambiguous source")
    code, err = _list_failure(STUB + "\nM.courses.alpha = function() return {} end\n")
    expect(code != 0, f"--list exits non-zero (got {code})")
    expect("movement_arena.lua" in err, f"the file is named ({err!r})")
    expect("alpha" in err and "duplicate" in err,
           f"the ambiguous course is named ({err!r})")


# --------------------------------------------------------------------------
# The drift guard a real course run performs
# --------------------------------------------------------------------------
@contextlib.contextmanager
def runtime_courses(names):
    original = probe.send_json
    probe.send_json = lambda *_args, **_kwargs: names
    try:
        yield
    finally:
        probe.send_json = original


def _check(runtime) -> tuple[int, str]:
    err = io.StringIO()
    with runtime_courses(runtime), contextlib.redirect_stderr(err):
        code = probe.check_course_inventory(0)
    return code, err.getvalue()


def test_the_guard_passes_when_the_two_views_agree() -> None:
    print("\ndrift guard: agreement")
    with lua_source(STUB):
        code, err = _check(["beta", "alpha"])   # runtime order is irrelevant
    expect(code == 0, f"an agreeing inventory passes (got {code})")
    expect(err == "", f"nothing is reported ({err!r})")


def test_the_guard_names_both_directions_of_drift() -> None:
    print("\ndrift guard: disagreement")
    with lua_source(STUB):
        code, err = _check(["alpha", "gamma"])
    expect(code != 0, f"drift fails the run (got {code})")
    expect("movement_arena.lua" in err, f"the source is named ({err!r})")
    expect("absent at runtime: beta" in err,
           f"the derived-only course is named on its own line ({err!r})")
    expect("not derived from the Lua source: gamma" in err,
           f"the runtime-only course is named on its own line ({err!r})")


def test_an_empty_runtime_answer_is_a_failure_not_a_match() -> None:
    print("\ndrift guard: an engine that answered nothing")
    for runtime in (None, [], "nil"):
        with lua_source(STUB):
            code, err = _check(runtime)
        expect(code != 0, f"{runtime!r} fails the run (got {code})")
        expect("listCourses" in err, f"the query is named ({err!r})")


def test_an_unreadable_source_fails_the_guard_too() -> None:
    print("\ndrift guard: an unreadable source")
    with tempfile.TemporaryDirectory() as tmp:
        original = probe.COURSE_SOURCE
        probe.COURSE_SOURCE = Path(tmp) / "movement_arena.lua"
        try:
            code, err = _check(["alpha"])
        finally:
            probe.COURSE_SOURCE = original
    expect(code != 0, f"the run fails (got {code})")
    expect("movement_arena.lua" in err, f"the file is named ({err!r})")


def main() -> int:
    selftest.parse_verbose()
    test_the_shipped_inventory_is_complete_and_sorted()
    test_list_prints_the_inventory_without_booting()
    test_list_is_honoured_for_every_mode()
    test_a_new_course_appears_without_any_python_edit()
    test_a_commented_or_quoted_mention_is_not_a_registration()
    test_a_structurally_broken_source_is_a_named_non_zero_exit()
    test_an_intact_source_is_still_accepted()
    test_a_non_utf8_source_is_a_named_non_zero_exit()
    test_an_unreadable_source_is_a_named_non_zero_exit()
    test_a_source_declaring_nothing_is_a_named_non_zero_exit()
    test_a_duplicate_registration_is_a_named_non_zero_exit()
    test_the_guard_passes_when_the_two_views_agree()
    test_the_guard_names_both_directions_of_drift()
    test_an_empty_runtime_answer_is_a_failure_not_a_match()
    test_an_unreadable_source_fails_the_guard_too()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftest.concluded(1)
    return selftest.concluded(
        0, "\nAll movement_probe course-listing tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
