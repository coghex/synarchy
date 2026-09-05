#!/usr/bin/env python3
"""F4 (#646) action-outcome coverage self-audit.

The action-outcome oracle (`debug.recordOutcome` / `debug.drainActionOutcomes`)
only has value where somebody actually wired it up at a commit boundary.
Rather than trust a hand-maintained "is X done" list that silently drifts
as new commit-boundary verbs are added, this greps each registered verb's
own source for its instrumentation call site and reports yes/no per verb —
mirrors `tools/ci_probes.py --status`'s "make the gap visible" self-audit,
in the no-engine-needed style of `tools/lua_module_budget.py`.

A file-wide substring search is not precise enough on its own: several
verbs share a file (e.g. unitAi.commandMove/commandAttack both live in
scripts/unit_ai_core.lua; craft.execute/executeAt share
Craft/Execute.hs), so a naive per-file pattern would mark BOTH complete
the moment either one is instrumented. Each registered verb is checked within
its OWN function body — the span from its definition to the next
top-level definition in the same file — using a pattern that only
matches a real call site, not the instrumentation helper's own
definition line. `--self-test` proves this discriminates.

Growing coverage is a two-step: add the real `debug.recordOutcome` /
`pushActionOutcome` call at the commit boundary, then register the verb
in the core module's registry (or extend its function-scope patterns)
so the audit stops flagging it as a gap.

A `gap` is only honest if the checker is reading the file the producers
actually live in. #1704: it was not. #787 split the input thread into
`Engine.Input.Thread.{Dispatch,Keyboard,Char,Mouse,Scroll}` and left
`Engine.Input.Thread` a 98-line lifecycle facade, and every one of the
five Layer A input checks kept reading the facade — so five fully
instrumented areas reported as gaps while the report still exited 0 and
nothing failed. `--verify-tier1` is the answer to that class of drift:
it is the CI-invoked gate over the Tier 1 areas ONLY, and it fails when
a mapped source file is absent (a producer renamed or moved out from
under the mapping) or when a mapped file is present but a required
producer pattern is missing (instrumentation actually deleted). Whether
a Tier 1 area is uninstrumented or merely unmapped, the answer under
that flag is the same — non-zero — because the report cannot tell those
apart and must not be trusted to.

Usage:
  python3 tools/action_outcome_coverage.py
  python3 tools/action_outcome_coverage.py --self-test
  python3 tools/action_outcome_coverage.py --verify-tier1
Exit code is always 0 for the coverage report — this is a visibility
report, not a blocking gate. Tier 2/3 gaps are deliberate fast-follows
(see issue #646), not regressions; only Tier 1 (this PR's scope) is
expected to read 100%. --self-test exits 1 on a self-test failure;
--verify-tier1 exits 1 on a stranded or uninstrumented Tier 1 area.

Layout (#2149): this file is the sole public command — mode dispatch,
report and Tier 1 diagnostic formatting, self-test invocation, and the
process exit — and owns no audit logic of its own.
`tools/action_outcome_coverage_core.py` owns every producer path,
regex contract, predicate, registry entry and the Tier 1 policy;
`tools/action_outcome_coverage_selftest.py` owns the synthetic
mutation corpus and imports the core's predicates rather than copying
them. Dependencies run one way — the self-test imports the core, this
facade imports both, nothing imports back — and no callback is
registered in either direction. Importing either module runs no check,
prints nothing and reads no file.
"""

from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import action_outcome_coverage_core as core  # noqa: E402
import action_outcome_coverage_selftest as selftest  # noqa: E402


def main_verify_tier1() -> int:
    total, problems = core.verify_tier1(core.build_verbs())
    if problems:
        print(f"{len(problems)} of {total} Tier 1 (Layer A) area(s) failed "
              f"the mapping/instrumentation gate:")
        for problem in problems:
            print(f"  FAIL: {problem}")
        print("\nTier 1 is expected to read 100%. The plain coverage report "
              "cannot tell a stranded mapping from deleted instrumentation "
              "and exits 0 either way — this gate exists so neither is "
              "silent (#1704).")
        return 1
    print(f"action_outcome_coverage.py --verify-tier1: all {total} Tier 1 "
          f"area(s) mapped and instrumented")
    return 0


def main() -> int:
    results = core.evaluate_coverage(core.build_verbs())
    done = sum(1 for *_r, ok in results if ok)
    print(f"F4 action-outcome coverage: {done}/{len(results)} registered "
          f"commit-boundary verbs instrumented\n")
    for tier, verb, ok in results:
        mark = "DONE" if ok else "gap "
        print(f"  [{mark}] tier {tier:<2}  {verb:<58}")

    gaps = [(tier, verb) for tier, verb, ok in results if not ok]
    if gaps:
        print(f"\n{len(gaps)} gap(s) — expected for Tier 2/3 fast-follows, "
              f"not for Tier 1:")
        for tier, verb in gaps:
            print(f"  tier {tier}: {verb}")
    return 0


def main_self_test() -> int:
    failures = selftest.run_self_test()
    if failures:
        print(f"{len(failures)} self-test failure(s):")
        for f in failures:
            print(f"  FAIL: {f}")
        return 1
    print("action_outcome_coverage.py self-test: all checks passed")
    return 0


if __name__ == "__main__":
    if "--self-test" in sys.argv:
        raise SystemExit(main_self_test())
    if "--verify-tier1" in sys.argv:
        raise SystemExit(main_verify_tier1())
    raise SystemExit(main())
