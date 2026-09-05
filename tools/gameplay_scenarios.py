#!/usr/bin/env python3
"""Manual first-expedition gameplay scenarios (#925).

A deliberately small, ON-DEMAND scenario runner for watching real
gameplay behavior in the first-expedition arc. Two scenarios, both
one-shot:

  expedition  Five acolytes + one technomule (the real starting party)
              on a repeatable fixed-seed world. Two acolytes are
              provisioned off the STATIONARY mule and walk a fixed
              out-and-back route; body/inventory/injury checkpoints are
              recorded at every waypoint.
  first-aid   The same real starting roster on a repeatable arena. The
              mule's pre-stocked first-aid kit is moved onto the selected
              expedition acolyte, who then takes a real fall; the injury,
              the kit state and the final unit state are reported. Its
              roster setup, provisioning and pre-fall baseline all run
              with the simulation STOPPED, so ambient AI cannot move or
              injure the scout between the kit issue and the fall
              (#1218); the fall itself and everything after it stay live
              and observational. Treatment is then FOLLOWED to a named
              terminal condition (#1221): the runner administers none of
              it, the real medic AI claims/fetches/dresses on its own,
              and every sampling interval records the patient's blood,
              aggregate bleed rate, dressing state, remaining bandages
              and each treatment result until bleeding is controlled,
              the bandages run out, the patient collapses or dies, or a
              bounded budget expires.

THIS IS NOT A BEHAVIOR PROBE AND NOT A CI GATE. It is deliberately
absent from ``tools/run_probes.py`` and ``tools/ci_probes.py``, is never
selected by CI, and is not named ``*_probe.py`` — the probe registry and
its classification self-test key off registered probe names only.

**Exit status means setup/runtime failure only, never a gameplay
verdict.** 0 = the scenario finished its setup and printed its report;
1 = the engine/setup/console broke before the report could be produced;
2 = a bad command line. A unit dying, starving, failing to reach a
waypoint or going untreated is a reported OBSERVATION, not a failure —
survival-pressure tuning is #919's job, not this script's.

Usage:
  python3 tools/gameplay_scenarios.py --list
  python3 tools/gameplay_scenarios.py --test expedition
  python3 tools/gameplay_scenarios.py --test first-aid
  python3 tools/gameplay_scenarios.py --test expedition --port 9926

Engine hygiene (same conventions as tools/*_probe.py, via probelib):
the default port is 9925 — never 8008, the user's GUI — ``--port``
overrides it, and every scenario shuts its own engine down through
``engine.quit()`` (hard-killing its own tracked PID as a fallback) in a
``finally``. Nothing is saved, so the repository's runtime state is
untouched apart from the engine's normal boot-time materialization of
gitignored ``config/*.local.yaml``.

Ownership (#2151): this file is the executable FAÇADE and nothing more —
argument parsing, the missing- and unknown-scenario diagnostics,
selecting one scenario, anchoring cwd-relative resources to the
repository root, translating ``ScenarioError`` into the setup/runtime-
failure diagnostic, and the public exit contract. Everything the two
scenarios share (engine bootstrap, the simulation hold, the roster,
snapshots and checkpoints, report formatting, movement, the
capacity-gated transfer) lives in ``tools/gameplay_scenarios_support.py``;
each scenario lives with its own constants, steps and report in
``tools/gameplay_scenarios_expedition.py`` and
``tools/gameplay_scenarios_first_aid.py``. Dependencies run one way:
this façade imports both scenario owners, each owner imports shared
support only, and shared support imports no scenario and not this file.
None of the extracted modules is a probe either — none is named
``*_probe.py``, registered in ``tools/run_probes.py`` or classified in
``tools/ci_probes.py``, and none has a command line of its own.
"""
from __future__ import annotations

import argparse
import os
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from gameplay_scenarios_support import DEFAULT_PORT, LOG, ScenarioError  # noqa: E402
from gameplay_scenarios_expedition import run_expedition  # noqa: E402
from gameplay_scenarios_first_aid import run_first_aid  # noqa: E402

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


# ---------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------
SCENARIOS = {
    "expedition": (
        "Five acolytes + one technomule on a fixed-seed world: two "
        "acolytes are provisioned off the stationary mule and walk a "
        "fixed out-and-back route, reporting inventory, carrying state, "
        "hunger/hydration/exhaustion, injuries, treatment activity and "
        "position at every waypoint.",
        run_expedition),
    "first-aid": (
        "The same starting roster on a repeatable arena: the mule's "
        "stocked first-aid kit is moved onto the expedition acolyte, who "
        "takes a real fall. The REAL medic AI is then followed to a named "
        "terminal condition (controlled / supplies exhausted / collapsed "
        "/ died / timeout), reporting per-interval blood, bleed rate, "
        "dressed-vs-undressed wounds, remaining bandages and every "
        "treatment result, plus the kit's contents and the final state.",
        run_first_aid),
}


def main() -> int:
    ap = argparse.ArgumentParser(
        description="Manual first-expedition gameplay scenarios (#925). "
                    "Diagnostics only — deliberately outside CI, and the "
                    "exit status reports setup/runtime failure, never a "
                    "gameplay-balance verdict.")
    ap.add_argument("--list", action="store_true",
                    help="list the available scenarios and exit")
    ap.add_argument("--test", metavar="NAME",
                    help="scenario to run (see --list)")
    ap.add_argument("--port", type=int, default=DEFAULT_PORT,
                    help=f"debug-console port (default {DEFAULT_PORT}; "
                         f"never use 8008, the GUI's port)")
    args = ap.parse_args()

    if args.list:
        for name in sorted(SCENARIOS):
            print(f"{name}\n    {SCENARIOS[name][0]}")
        return 0
    if not args.test:
        print("error: no test selected — pass --test <name> or --list "
              f"(known tests: {', '.join(sorted(SCENARIOS))})",
              file=sys.stderr)
        return 2
    if args.test not in SCENARIOS:
        print(f"error: unknown test {args.test!r} "
              f"(known tests: {', '.join(sorted(SCENARIOS))}; use --list)",
              file=sys.stderr)
        return 2

    # Every runtime resource family is loaded by cwd-relative path, and
    # `cabal run` needs the project root, so anchor both to the checkout
    # this script lives in rather than wherever it was invoked from.
    os.chdir(REPO_ROOT)
    try:
        return SCENARIOS[args.test][1](args.port)
    except ScenarioError as exc:
        print(f"\nSETUP/RUNTIME FAILURE: {exc}", file=sys.stderr)
        print(f"engine log: {LOG}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
