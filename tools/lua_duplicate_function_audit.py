#!/usr/bin/env python3
"""Duplicate top-level exported function guard for scripts/ui/*.lua.

Issue #814: PR #259 silently introduced a second
`function slider.findByElementHandle(...)` definition later in
scripts/ui/slider.lua, which overwrote the first in the module table
and narrowed its contract with no error or warning of any kind. This is
the cheap, no-engine static guard against that regression recurring —
here or in any other scripts/ui/*.lua widget module.

Each scripts/ui/*.lua widget module declares exactly one module table
near the top of the file and attaches its public API as
`function <name>.<method>(...)` definitions at the top level. A later
top-level definition of the same `<name>.<method>` silently replaces the
earlier one in that table -- this script flags that.

Deliberately narrow in scope: only top-level (column-0) `function
<module>.<name>(` definitions are tracked, matching the exported-API
shape these widget modules actually use. Local helper functions,
methods defined via `function <name>:<method>`, and nested/anonymous
functions are out of scope.

Issue #1324 closed the two ways this guard used to report success while
covering less than it claimed:

  * A file whose module-table declaration left the recognized grammar
    was skipped in silence, so every exported function in it went
    unchecked. scripts/ui/focus_indicator.lua really was in that state:
    its singleton-reload declaration was unrecognized, so it counted
    towards the reported total while being analyzed not at all. An
    unrecognized in-scope file is now an attributed failure, and the
    singleton-reload form is recognized.
  * An empty glob result reported the same clean success as a populated
    scan, so a moved directory or an edited glob would have disabled the
    guard with CI still green. An empty corpus is now a failure.

A third, subtler form of the first one is closed with it: identifying
the module table as the first ACCEPTED declaration means a file whose
real module table is unrecognized can fall through to a later PRIVATE
table (`local widget = { value = 1 }` followed by `local instances =
{}`) and report a clean analysis of the wrong table, hiding duplicate
`widget.*` exports. So an export attached to a table no accepted
declaration covers is reported too -- recognition is tied to the
exports, not merely to the first declaration that happens to parse.

The supported module-table grammar stays deliberately CLOSED -- exactly
the two forms below. Anything else is reported as an unrecognized
declaration rather than absorbed by widening the pattern: this
repository has twice paid long review cycles for coverage checkers that
stacked regex special cases (PR #704, PR #1128), and a loud failure
naming the file is both cheaper and safer than a quiet guess.

The count in the clean summary is the number of files actually
recognized and analyzed, and that summary is printed only when every
matched file reached it.

Usage:
  python3 tools/lua_duplicate_function_audit.py
Exit codes: 0 = every in-scope module analyzed with no duplicate
definitions, 1 = a duplicate, an unanalyzable in-scope file, or a
missing corpus.
"""
from __future__ import annotations

import re
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent

MODULES_GLOB = "scripts/ui/*.lua"

# The two accepted module-table declaration forms, both anchored at
# column zero and allowing only a trailing `--` comment after them:
#
#   local widget = {}
#   local M = package.loaded["scripts.ui.focus_indicator"] or {}
#
# The second is the singleton-reload idiom this repository uses
# deliberately (the same `package.loaded[...]` pattern CLAUDE.md
# documents for the unit_ai submodules). An initializer with contents
# (`local widget = { value = 1 }`) is NOT a third accepted form -- it is
# an unrecognized declaration, reported as such.
MODULE_TABLE_RE = re.compile(r'^local\s+(\w+)\s*=\s*\{\}\s*(?:--.*)?$')
SINGLETON_TABLE_RE = re.compile(
    r'^local\s+(\w+)\s*=\s*package\.loaded\[\s*'
    r'(?:"[^"\n\\]*"|\'[^\'\n\\]*\')'
    r'\s*\]\s*or\s*\{\}\s*(?:--.*)?$')

DECLARATION_FORMS = ('local <name> = {}',
                     'local <name> = package.loaded["<module>"] or {}')

FUNC_DEF_RE = re.compile(r'^function\s+(\w+)\.(\w+)\s*\(')


def declared_tables(lines: list[str]) -> list[str]:
    """Every table declared by an accepted form, in source order.

    The first is the module table -- these modules declare their own
    table before the private tables that follow it. The rest are those
    private tables, which are not duplicate-tracked but ARE recognized,
    so an export attached to one is attributed rather than orphaned.
    """
    names: list[str] = []
    for line in lines:
        m = MODULE_TABLE_RE.match(line) or SINGLETON_TABLE_RE.match(line)
        if m and m.group(1) not in names:
            names.append(m.group(1))
    return names


def check_file(path: Path, repo_root: Path) -> tuple[bool, list[str]]:
    """Analyze one in-scope module.

    Returns (analyzed, failures). `analyzed` is False when the file
    could not be read or its module-table declaration was not
    recognized -- in which case `failures` says so, naming the file and
    what was not recognized. A file in scope is never skipped silently.
    """
    rel = path.relative_to(repo_root)
    try:
        text = path.read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError) as err:
        return False, [
            f"{rel}: could not be read as UTF-8 text ({err}) -- this file "
            f"is in scope but was not analyzed"]

    lines = text.splitlines()
    declared = declared_tables(lines)
    if not declared:
        return False, [
            f"{rel}: unrecognized module-table declaration -- no "
            f"column-zero `{DECLARATION_FORMS[0]}` or "
            f"`{DECLARATION_FORMS[1]}` line was found, so none of this "
            f"file's exported functions were analyzed. Fix the "
            f"declaration rather than widening the audit's grammar."]

    name = declared[0]
    seen: dict[str, int] = {}
    orphans: dict[str, int] = {}
    failures: list[str] = []
    for lineno, line in enumerate(lines, start=1):
        m = FUNC_DEF_RE.match(line)
        if not m:
            continue
        table = m.group(1)
        if table not in declared:
            # An export attached to a table no accepted declaration
            # covers. Identifying the module table by "first accepted
            # declaration" alone would let this file fall through to a
            # later PRIVATE table and report a clean analysis of the
            # wrong one, which is the same false-green class this guard
            # exists to close -- so it is reported, not guessed at.
            orphans.setdefault(table, lineno)
            continue
        if table != name:
            continue
        fn = m.group(2)
        if fn in seen:
            failures.append(
                f"{rel}:{lineno}: duplicate definition of {name}.{fn} "
                f"(first at line {seen[fn]}) -- the later one silently "
                f"overwrites the earlier one in the module table")
        else:
            seen[fn] = lineno

    for table, lineno in orphans.items():
        failures.append(
            f"{rel}:{lineno}: `function {table}.` definitions attach to "
            f"`{table}`, which has no recognized declaration in this file "
            f"(recognized here: {', '.join(declared)}) -- so they were not "
            f"analyzed. A declaration the grammar does not accept is "
            f"reported rather than silently replaced by another table; fix "
            f"the declaration rather than widening the audit's grammar.")

    return not orphans, failures


def main(repo_root: Path = REPO_ROOT) -> int:
    paths = sorted(repo_root.glob(MODULES_GLOB))
    if not paths:
        print(f"No files matched {MODULES_GLOB} under {repo_root} -- the "
              f"widget-module corpus this guard exists to cover is missing, "
              f"so nothing was analyzed. Either the modules moved or the "
              f"glob is wrong; an empty scan is not a passing scan.")
        return 1

    analyzed = 0
    failures: list[str] = []
    for path in paths:
        ok, file_failures = check_file(path, repo_root)
        if ok:
            analyzed += 1
        failures.extend(file_failures)

    if failures:
        print(f"{len(failures)} problem(s) across {len(paths)} files "
              f"matching {MODULES_GLOB} ({analyzed} analyzed):")
        for f in failures:
            print(f"  {f}")
        return 1

    # The summary count is the ANALYZED count, never the matched-path
    # count that used to be printed here. Reaching this line at all
    # requires the two to be equal -- every file that was not analyzed
    # appended a failure above -- which is precisely the property that
    # makes a clean summary trustworthy.
    print(f"No duplicate exported function definitions across "
          f"{analyzed} analyzed files matching {MODULES_GLOB}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
