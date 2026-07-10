#!/usr/bin/env python3
"""F4 (#646) action-outcome coverage self-audit.

The action-outcome oracle (`debug.recordOutcome` / `debug.drainActionOutcomes`)
only has value where somebody actually wired it up at a commit boundary.
Rather than trust a hand-maintained "is X done" list that silently drifts
as new commit-boundary verbs are added, this greps each registered verb's
own source for its instrumentation call site and reports yes/no per verb —
mirrors `tools/ci_probes.py --status`'s "make the gap visible" self-audit,
in the no-engine-needed style of `tools/lua_module_budget.py`.

Growing coverage is a two-step: add the real `debug.recordOutcome` /
`pushActionOutcome` call at the commit boundary, then register the verb
here (file + pattern) so the audit stops flagging it as a gap.

Usage:
  python3 tools/action_outcome_coverage.py
Exit code is always 0 — this is a visibility report, not a blocking gate.
Tier 2/3 gaps are deliberate fast-follows (see issue #646), not
regressions; only Tier 1 (this PR's scope) is expected to read 100%.
"""
from __future__ import annotations

import re
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent

# (tier, verb, file relative to repo root, regex confirming instrumentation)
VERBS: list[tuple[str, str, str, str]] = [
    # --- Layer A: input routing, "complete" per the issue's scope note ---
    ("A", "input click -> UI widget consumption",
     "src/Engine/Scripting/Lua/Thread/Dispatch.hs", r"recordWidgetClickOutcome"),
    ("A", "input click -> game-world tool/select/deadclick chain",
     "scripts/init_mouse.lua", r"recordClick\("),

    # --- Layer B Tier 1: onboarding + highest naive-frequency (this PR) ---
    ("B1", "createWorld.generate (proceed commit)",
     "scripts/create_world/generation.lua", r"debug\.recordOutcome"),
    ("B1", "buildTool.commitPlacement",
     "scripts/build_tool.lua", r"debug\.recordOutcome"),
    ("B1", "wire.place",
     "scripts/wire.lua", r"debug\.recordOutcome"),
    ("B1", "till.designate (partial-drop counts)",
     "src/World/Thread/Command/Cursor.hs",
     r'recordDesignationOutcome env "till\.designate"'),
    ("B1", "chop.designate (partial-drop counts)",
     "src/World/Thread/Command/Cursor.hs",
     r'recordDesignationOutcome env "chop\.designate"'),
    ("B1", "world.designateMine (partial-drop counts)",
     "src/World/Thread/Command/Cursor.hs",
     r'recordDesignationOutcome env "world\.designateMine"'),
    ("B1", "plant.designate (accept/reject)",
     "src/World/Thread/Command/Cursor.hs",
     r'aoKind\s*=\s*"plant\.designate"'),

    # --- Layer B Tier 2: common mid-game — fast-follow, not this PR ---
    ("B2", "unitAi.commandMove",
     "scripts/unit_ai_core.lua", r"debug\.recordOutcome"),
    ("B2", "unitAi.commandAttack",
     "scripts/unit_ai_core.lua", r"debug\.recordOutcome"),
    ("B2", "craft.execute",
     "src/Engine/Scripting/Lua/API/Craft/Execute.hs", r"pushActionOutcome"),
    ("B2", "craft.executeAt",
     "src/Engine/Scripting/Lua/API/Craft/Execute.hs", r"pushActionOutcome"),
    ("B2", "craft.addBill",
     "src/Engine/Scripting/Lua/API/Craft/Bill.hs", r"pushActionOutcome"),

    # --- Layer B Tier 3: everything else, added as those paths get
    # touched. construction.designate is structurally identical to
    # till/chop/mine but wasn't named in the issue's Tier 1 list. ---
    ("B3", "construction.designate (building/structure)",
     "src/World/Thread/Command/Cursor.hs",
     r'recordDesignationOutcome env "construction\.designate"'),
]


def check() -> list[tuple[str, str, str, bool]]:
    results = []
    for tier, verb, relpath, pattern in VERBS:
        path = REPO_ROOT / relpath
        instrumented = path.exists() and re.search(pattern, path.read_text(
            encoding="utf-8")) is not None
        results.append((tier, verb, relpath, instrumented))
    return results


def main() -> int:
    results = check()
    done = sum(1 for *_r, ok in results if ok)
    print(f"F4 action-outcome coverage: {done}/{len(results)} registered "
          f"commit-boundary verbs instrumented\n")
    for tier, verb, relpath, ok in results:
        mark = "DONE" if ok else "gap "
        print(f"  [{mark}] tier {tier:<2}  {verb:<45} ({relpath})")

    gaps = [(tier, verb) for tier, verb, _r, ok in results if not ok]
    if gaps:
        print(f"\n{len(gaps)} gap(s) — expected for Tier 2/3 fast-follows, "
              f"not for Tier 1:")
        for tier, verb in gaps:
            print(f"  tier {tier}: {verb}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
