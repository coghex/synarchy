#!/usr/bin/env python3
"""Line-budget guard for Lua modules split out to stay reviewable.

Some large Lua files get split into a shell/coordinator plus small
per-domain modules, with an explicit agreement that every file involved
stays under a physical-line budget. This script is the cheap,
no-engine-needed guard that catches a regression back past that budget.

Add an entry to BUDGETS whenever a future split takes on the same
guarantee rather than writing a new one-off script — the pattern
generalizes across all of them.

Usage:
  python3 tools/lua_module_budget.py
Exit codes: 0 = every budgeted file is within its budget, 1 = a regression.
"""
from __future__ import annotations

import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent

# (label, glob patterns relative to the repo root, max physical lines)
BUDGETS: list[tuple[str, list[str], int]] = [
    ("debug overlay split (#545)",
     ["scripts/debug.lua", "scripts/debug/*.lua"], 500),
    ("unit-resource physiology split (#541)",
     ["scripts/unit_resources.lua", "scripts/unit_resource*.lua"], 500),
    ("unit-AI split (#538)",
     ["scripts/unit_ai.lua", "scripts/unit_ai_*.lua"], 500),
]


def check() -> list[str]:
    failures: list[str] = []
    for label, patterns, budget in BUDGETS:
        matched: set[Path] = set()
        for pattern in patterns:
            matched.update(REPO_ROOT.glob(pattern))
        if not matched:
            failures.append(f"{label}: no files matched {patterns!r}")
            continue
        for path in sorted(matched):
            n = sum(1 for _ in path.open("r", encoding="utf-8"))
            rel = path.relative_to(REPO_ROOT)
            status = "OK" if n <= budget else "FAIL"
            print(f"  {status}: {rel} -- {n}/{budget} lines")
            if n > budget:
                failures.append(
                    f"{rel} is {n} lines, over the {budget}-line budget "
                    f"for {label}")
    return failures


def main() -> int:
    failures = check()
    if failures:
        print(f"\n{len(failures)} budget violation(s):")
        for f in failures:
            print(f"  {f}")
        return 1
    print("\nAll budgeted Lua modules are within their line budget")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
