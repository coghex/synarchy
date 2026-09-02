#!/usr/bin/env python3
"""Unit tests for lua_duplicate_function_audit.py (issue #1324: the guard
must fail loud when its scan scope or module grammar goes unmatched,
instead of reporting the same clean success it reports for a real scan).

Mirrors tools/test_haskell_module_budget.py's approach: import the real
guard's `main()` and drive it over synthetic temporary roots, so these
tests exercise the audit's actual discovery, declaration-recognition and
duplicate-checking path. The two defects being fixed are both
false-GREENS -- a scan that reports success while having analyzed less
than it claims -- so a test carrying its own private copy of the scanner
would happily pass while the real guard stayed blind.

Every rule the audit enforces is pinned in BOTH directions: a positive
fixture that must pass, and a negative fixture that must exit nonzero
AND print a diagnostic naming the real problem. A checker that simply
failed everything, or failed with an unattributed message, cannot pass
this suite.

Every fixture is built in its own temporary directory and passed to
`main()` as an explicit root; nothing here reads or writes the shipped
scripts/ tree.

Usage:
  python3 tools/test_lua_duplicate_function_audit.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import contextlib
import io
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from lua_duplicate_function_audit import main  # type: ignore

import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402


CLEAN_SUMMARY = "No duplicate exported function definitions"


def _write(root: Path, rel: str, body: str) -> None:
    path = root / rel
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(body, encoding="utf-8")


def _run(root: Path) -> tuple[int, str]:
    buf = io.StringIO()
    with contextlib.redirect_stdout(buf):
        code = main(repo_root=root)
    return code, buf.getvalue()


def _expect_clean(code: int, output: str, analyzed: int, what: str) -> None:
    expect(code == 0, f"{what} must exit 0, got {code} with output: {output!r}")
    expect(f"{analyzed} analyzed files" in output,
           f"{what} must report {analyzed} analyzed file(s), got: "
           f"{output!r}")


def _expect_failure(code: int, output: str, needles: list[str],
                    what: str) -> None:
    expect(code == 1, f"{what} must exit 1, got {code} with output: {output!r}")
    for needle in needles:
        expect(needle in output,
               f"{what} must print a diagnostic naming {needle!r}, got: "
               f"{output!r}")
    expect(CLEAN_SUMMARY not in output,
           f"{what} must not also claim a clean scan, got: {output!r}")


PLAIN_DUPLICATE = """\
local widget = {}

function widget.f(a)
    return a
end

function widget.f(a, b)
    return a, b
end
"""

PLAIN_CLEAN = """\
local widget = {}

function widget.f(a)
    return a
end

function widget.g(a)
    return a
end
"""

SINGLETON_DUPLICATE = """\
local M = package.loaded["scripts.ui.widget"] or {}
package.loaded["scripts.ui.widget"] = M

function M.f(a)
    return a
end

function M.f(a, b)
    return a, b
end
"""

SINGLETON_CLEAN = """\
local M = package.loaded["scripts.ui.widget"] or {}
package.loaded["scripts.ui.widget"] = M

function M.f(a)
    return a
end

function M.g(a)
    return a
end
"""


# --- duplicate detection, both directions ---------------------------------

def test_duplicate_in_plain_table_module_is_detected():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/widget.lua", PLAIN_DUPLICATE)
        code, output = _run(root)
        _expect_failure(
            code, output,
            ["scripts/ui/widget.lua", "widget.f", "first at line 3"],
            "a second top-level definition of an already-defined export")


def test_distinct_exports_in_plain_table_module_pass():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/widget.lua", PLAIN_CLEAN)
        code, output = _run(root)
        _expect_clean(code, output, 1,
                      "a module whose top-level exports are all distinct")


# --- the two accepted declaration forms, both directions ------------------

def test_singleton_reload_declaration_is_analyzed():
    # The live coverage loss #1324 was filed for: before this change the
    # whole file was skipped in silence and this duplicate went unseen.
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/focusish.lua", SINGLETON_DUPLICATE)
        code, output = _run(root)
        _expect_failure(
            code, output,
            ["scripts/ui/focusish.lua", "M.f", "first at line 4"],
            "a duplicate inside a `package.loaded[...] or {}` module")


def test_singleton_reload_declaration_counts_as_analyzed_when_clean():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/focusish.lua", SINGLETON_CLEAN)
        code, output = _run(root)
        _expect_clean(code, output, 1,
                      "a clean `package.loaded[...] or {}` module")


def test_single_quoted_singleton_key_is_analyzed():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/widget.lua",
               SINGLETON_DUPLICATE.replace('"', "'"))
        code, output = _run(root)
        _expect_failure(
            code, output, ["scripts/ui/widget.lua", "M.f"],
            "a duplicate inside a single-quoted singleton-reload module")


def test_trailing_comment_after_declaration_is_analyzed():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/a.lua",
               PLAIN_DUPLICATE.replace("local widget = {}",
                                       "local widget = {}  -- the module"))
        _write(root, "scripts/ui/b.lua",
               SINGLETON_DUPLICATE.replace(
                   'or {}', 'or {}  -- reload-safe singleton', 1))
        code, output = _run(root)
        _expect_failure(
            code, output,
            ["scripts/ui/a.lua", "widget.f", "scripts/ui/b.lua", "M.f"],
            "a trailing `--` comment after either declaration form")


def test_first_declaration_wins_over_later_private_tables():
    # Every real widget module declares its own table first and private
    # state tables after it; the export table must be the first match.
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/widget.lua", """\
local widget = {}
local instances = {}

function instances.f()
end

function instances.f()
end

function widget.f()
end
""")
        code, output = _run(root)
        _expect_clean(
            code, output, 1,
            "duplicates on a private table declared after the module table")


# --- unrecognized declarations are attributed failures --------------------

def test_table_with_initializer_is_an_attributed_failure():
    # The issue's reproduction: an initializer with contents is NOT a
    # third accepted grammar, it is a named analysis failure.
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/widget.lua",
               PLAIN_DUPLICATE.replace("local widget = {}",
                                       "local widget = { value = 1 }"))
        code, output = _run(root)
        _expect_failure(
            code, output,
            ["scripts/ui/widget.lua", "unrecognized module-table declaration"],
            "a module declared with a non-empty table initializer")


def test_singleton_near_misses_are_attributed_failures():
    for label, decl in [
        ("without the `or {}` fallback",
         'local M = package.loaded["scripts.ui.widget"]'),
        ("built from require rather than package.loaded",
         'local M = require("scripts.ui.widget") or {}'),
        ("with a long-bracket module key",
         'local M = package.loaded[[[scripts.ui.widget]]] or {}'),
        ("keyed by a variable rather than a string literal",
         'local M = package.loaded[MODULE_NAME] or {}'),
        ("keyed by a concatenation rather than a string literal",
         'local M = package.loaded["scripts.ui." .. NAME] or {}'),
    ]:
        with tempfile.TemporaryDirectory() as d:
            root = Path(d)
            _write(root, "scripts/ui/widget.lua",
                   SINGLETON_DUPLICATE.replace(
                       'local M = package.loaded["scripts.ui.widget"] or {}',
                       decl, 1))
            code, output = _run(root)
            _expect_failure(
                code, output,
                ["scripts/ui/widget.lua",
                 "unrecognized module-table declaration"],
                f"a declaration {label}")


MASKED_BY_PRIVATE_TABLE = """\
local widget = { value = 1 }
local instances = {}

function widget.f(a)
    return a
end

function widget.f(a, b)
    return a, b
end
"""


def test_unrecognized_declaration_is_not_masked_by_a_later_private_table():
    # PR #1351 round-1 review: identifying the module table as the first
    # ACCEPTED declaration let an unrecognized one fall through to the
    # private table below it, so the file was reported as a clean
    # analysis of `instances` while both `widget.f` definitions went
    # unseen. Nearly every shipped widget module declares such a private
    # table, so this is the reachable form of the skip #1324 fixed.
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/widget.lua", MASKED_BY_PRIVATE_TABLE)
        code, output = _run(root)
        _expect_failure(
            code, output,
            ["scripts/ui/widget.lua", "widget", "not one the grammar"],
            "an unrecognized declaration followed by a private table")
        # The file must not be counted as analyzed on the strength of
        # the private table it fell through to.
        expect("1 files" in output and "0 analyzed" in output,
               f"a file whose exports were not attributed must not count "
               f"as analyzed, got: {output!r}")


def test_the_same_file_with_a_recognized_declaration_reports_its_duplicate():
    # The positive counterpart: repairing only the declaration turns the
    # attribution failure into the duplicate it was hiding, which proves
    # the new rule did not simply make the shape fail unconditionally.
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/widget.lua",
               MASKED_BY_PRIVATE_TABLE.replace("local widget = { value = 1 }",
                                               "local widget = {}", 1))
        code, output = _run(root)
        _expect_failure(
            code, output,
            ["scripts/ui/widget.lua", "widget.f", "first at line 4"],
            "the same file once its module table is recognized")


def test_export_attached_to_a_foreign_table_stays_out_of_scope():
    # PR #1351 round-2 review: the attribution rule must not classify an
    # unrelated foreign-table definition as an unanalyzable module
    # export. `other` is never declared locally here, so it is a foreign
    # or global table and its definitions are out of scope -- including
    # duplicated ones -- exactly as before this issue.
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/widget.lua", """\
local widget = {}

function widget.f()
end

function other.g()
end

function other.g(extra)
end
""")
        code, output = _run(root)
        _expect_clean(code, output, 1,
                      "a valid module carrying foreign-table definitions")


def test_unrecognized_declaration_is_caught_even_when_it_also_exports():
    # The residual of the round-1 case: the private table it fell
    # through to carries exports of its own, so "the module table has no
    # exports" would not have caught it. Keying on the declaration does.
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/widget.lua", """\
local widget = { value = 1 }
local instances = {}

function instances.track()
end

function widget.f(a)
    return a
end

function widget.f(a, b)
    return a, b
end
""")
        code, output = _run(root)
        _expect_failure(
            code, output,
            ["scripts/ui/widget.lua", "widget", "not one the grammar"],
            "an unrecognized declaration whose private table also exports")


def test_export_attached_to_a_declared_private_table_is_attributed():
    # The positive counterpart of the case above: declaring the table by
    # an accepted form attributes its exports, so they are recognized
    # (though still not duplicate-tracked -- that scope is unchanged).
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/widget.lua", """\
local widget = {}
local other = {}

function widget.f()
end

function other.g()
end
""")
        code, output = _run(root)
        _expect_clean(code, output, 1,
                      "an export attached to a declared private table")


def test_trailing_code_after_a_declaration_is_an_attributed_failure():
    # Only a `--` comment may follow either declaration. Trailing code is
    # unparsed, so it is reported rather than absorbed -- the positive
    # counterpart is test_trailing_comment_after_declaration_is_analyzed.
    for label, decl in [
        ("a statement after the plain form",
         "local widget = {} widget.version = 1"),
        ("a semicolon-separated statement after the plain form",
         "local widget = {}; init()"),
        ("a statement after the singleton form",
         'local M = package.loaded["scripts.ui.widget"] or {} init()'),
    ]:
        with tempfile.TemporaryDirectory() as d:
            root = Path(d)
            source = PLAIN_DUPLICATE if "plain form" in label \
                else SINGLETON_DUPLICATE
            first = source.splitlines()[0]
            _write(root, "scripts/ui/widget.lua",
                   source.replace(first, decl, 1))
            code, output = _run(root)
            _expect_failure(
                code, output,
                ["scripts/ui/widget.lua",
                 "unrecognized module-table declaration"],
                f"a declaration with {label}")


def test_recognized_declaration_passes_where_the_near_miss_failed():
    # The positive counterpart of the two cases above, on the same body:
    # repairing only the declaration turns the failure into a clean scan.
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/widget.lua",
               SINGLETON_CLEAN.replace("M.f", "M.first").replace(
                   "M.g", "M.second"))
        code, output = _run(root)
        _expect_clean(code, output, 1,
                      "the same module body under a recognized declaration")


def test_unreadable_file_is_an_attributed_failure():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        path = root / "scripts/ui/widget.lua"
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(b"local widget = {}\n-- \xff\xfe not utf-8\n")
        code, output = _run(root)
        _expect_failure(
            code, output,
            ["scripts/ui/widget.lua", "could not be read as UTF-8"],
            "a file in scope that cannot be decoded")


def test_non_ascii_utf8_file_is_analyzed():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/widget.lua",
               "-- éè accented comment\n" + PLAIN_CLEAN)
        code, output = _run(root)
        _expect_clean(code, output, 1,
                      "a valid UTF-8 module containing non-ASCII text")


def test_unrecognized_file_fails_even_when_every_other_file_is_clean():
    # A clean summary may only be emitted once every matched file has
    # been analyzed -- one skipped file must not hide behind the rest.
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/clean_a.lua", PLAIN_CLEAN)
        _write(root, "scripts/ui/clean_b.lua", SINGLETON_CLEAN)
        _write(root, "scripts/ui/odd.lua",
               "local widget = setmetatable({}, {})\n\nfunction widget.f()\nend\n")
        code, output = _run(root)
        _expect_failure(
            code, output,
            ["scripts/ui/odd.lua", "unrecognized module-table declaration"],
            "one unanalyzable file among otherwise clean ones")
        expect("clean_a.lua" not in output and "clean_b.lua" not in output,
               f"the clean files must not be reported as problems, got: "
               f"{output!r}")
        # The reported figure is recognized-and-analyzed files, not the
        # matched-path count: three files matched, two were analyzed.
        expect("3 files" in output and "2 analyzed" in output,
               f"the failure summary must distinguish the 3 matched paths "
               f"from the 2 files actually analyzed, got: {output!r}")


def test_reported_count_is_analyzed_files_not_matched_paths():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/a.lua", PLAIN_CLEAN)
        _write(root, "scripts/ui/b.lua", SINGLETON_CLEAN)
        _write(root, "scripts/ui/c.lua", "local c = {}\n")
        code, output = _run(root)
        _expect_clean(code, output, 3, "a three-module clean corpus")


# --- the corpus itself must exist ----------------------------------------

def test_empty_corpus_fails():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        (root / "scripts/ui").mkdir(parents=True)
        code, output = _run(root)
        _expect_failure(code, output, ["scripts/ui/*.lua"],
                        "a scope directory containing no modules at all")


def test_missing_scope_directory_fails_and_never_sees_the_real_tree():
    # Doubles as the isolation check: an empty temporary root must not
    # discover any of this repository's 30-odd real widget modules.
    with tempfile.TemporaryDirectory() as d:
        code, output = _run(Path(d))
        _expect_failure(code, output, ["scripts/ui/*.lua"],
                        "a root with no scripts/ui directory at all")


def test_populated_corpus_passes():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/widget.lua", PLAIN_CLEAN)
        code, output = _run(root)
        _expect_clean(code, output, 1,
                      "a corpus with at least one clean module in it")


# --- preserved scope, both directions ------------------------------------

def test_out_of_scope_definitions_are_not_tracked():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/widget.lua", """\
local widget = {}

local function helper(a)
    return a
end

local function helper(a, b)
    return a, b
end

function widget:method()
end

function widget:method(extra)
end

function other.f()
end

function other.f()
end

do
    function widget.f()
    end
end

    function widget.f()
    end

widget.assigned = function() end
widget.assigned = function() end
""")
        code, output = _run(root)
        _expect_clean(
            code, output, 1,
            "duplicated local helpers, colon methods, foreign-table "
            "functions, nested and indented definitions, and assigned "
            "anonymous functions")


def test_nested_directories_and_non_lua_files_are_out_of_scope():
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        _write(root, "scripts/ui/widget.lua", PLAIN_CLEAN)
        _write(root, "scripts/ui/nested/deep.lua", PLAIN_DUPLICATE)
        _write(root, "scripts/ui/notes.txt", PLAIN_DUPLICATE)
        _write(root, "scripts/other.lua", PLAIN_DUPLICATE)
        code, output = _run(root)
        _expect_clean(
            code, output, 1,
            "a non-recursive scan that ignores subdirectories, non-Lua "
            "files, and Lua modules outside scripts/ui")


TESTS = [
    test_duplicate_in_plain_table_module_is_detected,
    test_distinct_exports_in_plain_table_module_pass,
    test_singleton_reload_declaration_is_analyzed,
    test_singleton_reload_declaration_counts_as_analyzed_when_clean,
    test_single_quoted_singleton_key_is_analyzed,
    test_trailing_comment_after_declaration_is_analyzed,
    test_first_declaration_wins_over_later_private_tables,
    test_table_with_initializer_is_an_attributed_failure,
    test_singleton_near_misses_are_attributed_failures,
    test_unrecognized_declaration_is_not_masked_by_a_later_private_table,
    test_the_same_file_with_a_recognized_declaration_reports_its_duplicate,
    test_export_attached_to_a_foreign_table_stays_out_of_scope,
    test_unrecognized_declaration_is_caught_even_when_it_also_exports,
    test_export_attached_to_a_declared_private_table_is_attributed,
    test_trailing_code_after_a_declaration_is_an_attributed_failure,
    test_recognized_declaration_passes_where_the_near_miss_failed,
    test_unreadable_file_is_an_attributed_failure,
    test_non_ascii_utf8_file_is_analyzed,
    test_unrecognized_file_fails_even_when_every_other_file_is_clean,
    test_reported_count_is_analyzed_files_not_matched_paths,
    test_empty_corpus_fails,
    test_missing_scope_directory_fails_and_never_sees_the_real_tree,
    test_populated_corpus_passes,
    test_out_of_scope_definitions_are_not_tracked,
    test_nested_directories_and_non_lua_files_are_out_of_scope,
]


def main_() -> int:
    selftestlib.parse_verbose()
    for test in TESTS:
        print(f"{test.__name__}:")
        test()
    if FAILURES:
        print(f"\n{len(FAILURES)} test failure(s)")
        return selftestlib.concluded(1)
    return selftestlib.concluded(0, f"\nAll {len(TESTS)} tests passed")


if __name__ == "__main__":
    raise SystemExit(main_())
