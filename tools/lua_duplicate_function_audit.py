#!/usr/bin/env python3
"""Duplicate top-level exported function guard for scripts/ui/*.lua.

Issue #814: PR #259 silently introduced a second
`function slider.findByElementHandle(...)` definition later in
scripts/ui/slider.lua, which overwrote the first in the module table
and narrowed its contract with no error or warning of any kind. This is
the cheap, no-engine static guard against that regression recurring —
here or in any other scripts/ui/*.lua widget module.

Each scripts/ui/*.lua widget module declares exactly one module table
near the top of the file (`local <name> = {}`) and attaches its public
API as `function <name>.<method>(...)` definitions at the top level.
A later top-level definition of the same `<name>.<method>` silently
replaces the earlier one in that table -- this script flags that.

Deliberately narrow in scope: only top-level (column-0) `function
<module>.<name>(` definitions are tracked, matching the exported-API
shape these widget modules actually use. Local helper functions,
methods defined via `function <name>:<method>`, and nested/anonymous
functions are out of scope.

Usage:
  python3 tools/lua_duplicate_function_audit.py
Exit codes: 0 = no duplicate definitions found, 1 = a regression.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent

MODULES_GLOB = "scripts/ui/*.lua"

MODULE_TABLE_RE = re.compile(r'^local\s+(\w+)\s*=\s*\{\}\s*(?:--.*)?$')
FUNC_DEF_RE = re.compile(r'^function\s+(\w+)\.(\w+)\s*\(')


def module_table_name(lines: list[str]) -> str | None:
    for line in lines:
        m = MODULE_TABLE_RE.match(line)
        if m:
            return m.group(1)
    return None


def check_file(path: Path) -> list[str]:
    lines = path.read_text(encoding="utf-8").splitlines()
    name = module_table_name(lines)
    if name is None:
        return []

    rel = path.relative_to(REPO_ROOT)
    seen: dict[str, int] = {}
    failures: list[str] = []
    for lineno, line in enumerate(lines, start=1):
        m = FUNC_DEF_RE.match(line)
        if not m or m.group(1) != name:
            continue
        fn = m.group(2)
        if fn in seen:
            failures.append(
                f"{rel}:{lineno}: duplicate definition of {name}.{fn} "
                f"(first at line {seen[fn]}) -- the later one silently "
                f"overwrites the earlier one in the module table")
        else:
            seen[fn] = lineno
    return failures


def main() -> int:
    failures: list[str] = []
    paths = sorted(REPO_ROOT.glob(MODULES_GLOB))
    for path in paths:
        failures.extend(check_file(path))

    if failures:
        print(f"{len(failures)} duplicate exported function definition(s):")
        for f in failures:
            print(f"  {f}")
        return 1
    print(f"No duplicate exported function definitions across "
          f"{len(paths)} files matching {MODULES_GLOB}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
