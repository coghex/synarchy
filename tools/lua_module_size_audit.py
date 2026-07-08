#!/usr/bin/env python3
"""Line-budget guard for the unit-resource/physiology Lua modules (#541).

scripts/unit_resources.lua was split into an entry/orchestration module
plus sibling scripts/unit_resource_*.lua domain modules so no single file
mixes config, alerts, energy/starvation, per-resource ticking, injury
consequences, and failure meters. This script is the cheap guard against
that regressing back into one monolith: it fails if
scripts/unit_resources.lua or any scripts/unit_resource_*.lua module
exceeds the agreed 500-physical-line budget.

Exit codes:
  0 = every unit-resource/physiology module is within budget
  1 = at least one module exceeds the budget (or none were found at all,
      which would silently defeat the guard if the naming convention
      ever changed)
"""
from __future__ import annotations

import sys
from pathlib import Path

LINE_BUDGET = 500
REPO_ROOT = Path(__file__).resolve().parent.parent
SCRIPTS_DIR = REPO_ROOT / "scripts"


def find_modules() -> list[Path]:
    return sorted(SCRIPTS_DIR.glob("unit_resource*.lua"))


def main() -> int:
    modules = find_modules()
    if not modules:
        print(f"FAIL: no scripts/unit_resource*.lua modules found under {SCRIPTS_DIR}")
        return 1

    failures = []
    for path in modules:
        line_count = sum(1 for _ in path.open(encoding="utf-8"))
        status = "OK" if line_count <= LINE_BUDGET else "FAIL"
        print(f"{status}: {path.relative_to(REPO_ROOT)}: {line_count} lines "
              f"(budget {LINE_BUDGET})")
        if line_count > LINE_BUDGET:
            failures.append((path, line_count))

    if failures:
        print(f"\n{len(failures)} module(s) exceed the {LINE_BUDGET}-line budget.")
        return 1

    print(f"\nAll {len(modules)} unit-resource/physiology module(s) within budget.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
