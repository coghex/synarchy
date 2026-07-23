#!/usr/bin/env python3
"""Line-budget guard for Haskell modules split out to stay reviewable.

Mirrors tools/lua_module_budget.py's (label, glob-patterns, max-lines)
BUDGETS pattern for Haskell facade/submodule splits with the same
explicit agreement: every file involved stays under a physical-line
budget, so a regression back past it fails fast without needing an
engine build.

Add an entry to BUDGETS whenever a future Haskell split takes on the
same guarantee rather than writing a new one-off script.

Usage:
  python3 tools/haskell_module_budget.py
Exit codes: 0 = every budgeted file is within its budget, 1 = a regression.
"""
from __future__ import annotations

import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent

# (label, glob patterns relative to the repo root, max physical lines)
BUDGETS: list[tuple[str, list[str], int]] = [
    ("input-thread split (#787)",
     ["src/Engine/Input/Thread.hs", "src/Engine/Input/Thread/*.hs"], 500),
    ("timeline split (#588)",
     ["src/World/Generate/Timeline.hs", "src/World/Generate/Timeline/*.hs"], 500),
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
    print("\nAll budgeted Haskell modules are within their line budget")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
