#!/usr/bin/env python3
"""First-session tutorial integration gate (#922, phase 5 of epic #956).

The four tutorial slices each shipped their own gate: #957's tree
loading, #958's progress model, #959's evaluator predicates and #960's
HUD are covered by pure hspec groups ("Tutorial definitions",
"Tutorial progress", "Tutorial evaluation", "Tutorial HUD") plus the
GPU offscreen surface check in tools/tutorial_hud_probe.py. Every one of
those runs against stubs, injected trees, or a synthetic view model.

This probe is the missing joint: the SHIPPED data/tutorials/first_session.yaml
branch driven from end to end by real gameplay state in a real engine —

    Place portal
      -> Secure water source
        -> Prepare an expedition
             - Prepare water
             - Prepare food

— and then carried through a real save/load round trip in a SECOND
process. Nothing here injects a tree, stubs a predicate, or writes the
progress tables directly; every transition below is produced by placing
a real building, teleporting a real acolyte next to real generated
water, and moving real items in and out of a real inventory.

What it proves (issue #922 requirements 2 and 3):

  1. A session with no portal and no units starts with nothing completed
     and only the root objective revealed.
  2. Placing an acolyte_portal completes the portal objective and
     reveals "Secure water source".
  3. One acolyte DISCOVERING generated water (its own FOV scan) and
     SHARING it over the radio with a second acolyte completes the
     water-source objective. Both legs are asserted, because the share
     is the notify_allies fan-out and a single-acolyte run would never
     execute it. The share leg is made conclusive rather than
     circumstantial: the recipient is immobilized in a camp with no
     water anywhere in its own field of view (asserted against the
     engine's real `unit.getVisibleTiles`, before and after), and the
     source it ends up holding must be one of the FINDER's — so it
     cannot have discovered anything itself.
  4. Carrying >= 2 L of water checks the live water subobjective on its
     own, WITHOUT completing the composite.
  5. Carrying >= 2 L of water AND a ration on the SAME acolyte checks
     the food subobjective and permanently completes "Prepare an
     expedition".
  6. Stripping those supplies afterwards unchecks both live
     subobjectives and leaves the completed composite latched.
  7. A save/load round trip through a fresh process preserves every
     completed full objective, brings the HUD back collapsed, and
     recomputes the live subobjectives from the LOADED world's state
     (both unchecked, because the supplies were removed before saving).
  8. (#996, a separate boot) An UNSTRIPPED acolyte's default spawn kit
     latches "Prepare an expedition" and checks both its subobjectives
     before that branch is ever revealed -- reveal is gated on
     "Secure water source", which has nothing to do with carried
     supplies. The moment secure_water_source finally completes (the
     same real FOV discovery check 3 drives, but with its sight
     conditions established rather than hoped for -- see
     `phase_pre_latched_reveal`), the already-latched branch must become
     observable in the checklist rather than latching and hiding in the
     same instant.
  9. (#1941/#2056, the same boot) That suppression is a LOAN, not a
     second hide rule. The gameplay HUD is booted for real and the
     checklist opened for real: a visible HUD over a COLLAPSED panel
     still presents nothing, and opening it renders the whole branch.
     What this GPU-less probe then proves is #2056's negative half --
     an open panel on a visible HUD retires NOTHING while no frame is
     drawn, because acknowledgement is gated on a completed renderer
     snapshot and `--headless` has no renderer. The transition is then
     made explicitly, through #958's own acknowledgePresented, and the
     ordinary hide rule empties the checklist while the supplies are
     still carried and every latch is intact. Removing the supplies
     brings the RETIRED branch back (requirement 3, now under the
     ordinary rule rather than a suppression). The positive proof --
     that the rows really do reach a rendered frame -- belongs to
     tools/tutorial_hud_probe.py, which runs `--offscreen`.
 10. (#1941, a FOURTH boot) A save taken with that branch finished and
     retired reloads in a fresh process without returning any
     already-retired ancestor to the active checklist -- across the
     evaluation tick that re-checks both subobjectives against the same
     loaded world, which is the exact tick that used to resurrect all
     five rows.

Two deliberate departures from "just play the game", both forced by the
shipped content and both documented rather than worked around:

  * The portal's automatic starting roster is suppressed
    (`building.setSpawnRemaining(bid, 0)`) and the probe spawns its own
    two-acolyte party with the SAME faction-tagged call
    scripts/building_spawn.lua makes. Every acolyte spawns with a full
    2 L canteen and two rations (data/units/acolyte.yaml
    starting_inventory), so the roster would satisfy both prepare
    subobjectives the instant it appeared and latch the composite before
    any transition could be observed — scripts/tutorial_eval.lua documents
    that as intended behavior. Shedding the spawn kit first and
    restoring it stepwise is the only way checks 4-6 above exist as
    distinct, observable events.
  * Setup runs PAUSED. scripts/tutorial_eval.lua is deliberately not
    pause-gated (evaluating is observation, not simulation) while
    scripts/unit_ai.lua is, so a paused window lets the probe change one
    gameplay fact at a time and read the tutorial's answer without the
    AI moving, drinking, or foraging underneath it. The unpaused windows
    are exactly the two that need simulation: giving the party its AI
    state, and the water discovery/share leg.

Slow (TWO real worldSize-64 generations across FOUR engine boots, with
TWO real save/load transactions) and manual-only — never a CI gate, per
#922's own requirement 6.

MODULE OWNERSHIP (#2145). This file is the probe: the sole executable,
the only CLI, the only place an engine is booted or quit, the only place
a save slot is removed, and the only place the aggregate failure report
and exit status are produced. It holds no scenario-phase body. The two
scenario owners it sequences, and the support they share, live in
sibling LIBRARY modules — libraries, not probes, which is why none of
them is named `*_probe.py` and why `tools/run_probes.py` still registers
exactly one `tutorial` entry:

  tutorial_probe_contracts.py  the page and slot names, the objective,
                               subobjective, item and building ids,
                               `ProbeError`, the `Checks` recorder, the
                               `Progress` snapshot with its polling and
                               settlement, and `hud_open`
  tutorial_probe_setup.py      YAML loader and script declarations,
                               content and script loading, pause
                               control, generated-world initialization,
                               and the shared save/load barrier
  tutorial_probe_harness.py    generated-water, shore and camp
                               selection, the roster-suppressed portal,
                               acolyte spawning, spawn-kit stripping and
                               restoration, carried/known-water
                               inspection, pinned daylight, facing,
                               freezing, teleport confirmation, and the
                               `SightSnapshot` evidence
  tutorial_probe_ordinary.py   stages 1-8: the ordinary first-session
                               progression and its reload
  tutorial_probe_sticky.py     stages 9-15: the pre-latched
                               sticky-presentation flow and its reload

Dependencies run one way and the graph is acyclic: contracts imports no
sibling; setup imports contracts; the harness imports contracts and
setup; each stage owner imports the shared three and NEVER the other
stage owner or this facade.

Shared state is passed EXPLICITLY. In particular the `Checks` recorder
is constructed here, ONCE, and handed to every stage — a stage that
built its own would report failures into a list this file never prints,
and the run would exit 0 having failed.

Usage:
  python3 tools/tutorial_probe.py
  python3 tools/tutorial_probe.py --seed 42 --size 64 --port 9424

Exit code 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import os
import shutil
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import boot, quit_engine
import tutorial_probe_ordinary as ordinary
import tutorial_probe_sticky as sticky
from tutorial_probe_contracts import (REPO_ROOT, SLOT, STICKY_SLOT, Checks,
                                      ProbeError)

LOG_A = "/tmp/tutorial_probe_engine_a.log"
LOG_B = "/tmp/tutorial_probe_engine_b.log"
LOG_C = "/tmp/tutorial_probe_engine_c.log"
LOG_D = "/tmp/tutorial_probe_engine_d.log"


# --------------------------------------------------------------------------
# Save-slot hygiene
# --------------------------------------------------------------------------
def remove_probe_slot(slot: str = SLOT) -> None:
    """Delete only one of this probe's own save slots, under the repo's
    saves/. The name is checked against the probe's own two rather than
    interpolated blind, so this can never reach a player slot."""
    if slot not in (SLOT, STICKY_SLOT):
        raise ProbeError(f"refusing to remove a slot this probe does not own: {slot!r}")
    saves = os.path.join(REPO_ROOT, "saves")
    target = os.path.join(saves, slot)
    if (os.path.basename(target) == slot
            and os.path.dirname(target) == saves
            and os.path.isdir(target)
            and not os.path.islink(target)):
        shutil.rmtree(target)


def remove_probe_slots() -> None:
    remove_probe_slot(SLOT)
    remove_probe_slot(STICKY_SLOT)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--port", type=int, default=9424)
    args = ap.parse_args()

    remove_probe_slots()
    started = time.time()
    checks = Checks()

    print(f"== boot 1: build the session (port {args.port}) ==")
    proc = boot(args.port, log=LOG_A, label="tutorial engine")
    completed: list[str] = []
    try:
        completed = ordinary.run_session(args.port, checks, args.seed, args.size)
    except ProbeError as e:
        # A first-leg failure short-circuits the whole run: boots 2-4
        # never start, because the reload leg would otherwise run against
        # a save that was never taken.
        print(f"\nSETUP FAILED: {e}")
        checks.fail(f"setup: {e}")
        return 1
    finally:
        quit_engine(args.port, proc)

    print(f"== boot 2: load the save in a FRESH process (port {args.port}) ==")
    proc = boot(args.port, log=LOG_B, label="tutorial reload engine")
    try:
        ordinary.run_reload(args.port, checks, completed)
    except ProbeError as e:
        print(f"\nRELOAD FAILED: {e}")
        checks.fail(f"reload: {e}")
    finally:
        quit_engine(args.port, proc)
        remove_probe_slot()

    print(f"== boot 3: a branch that latches before it is ever revealed "
          f"(#996, port {args.port}) ==")
    proc = boot(args.port, log=LOG_C, label="tutorial pre-latch engine")
    sticky_completed: list[str] = []
    try:
        sticky_completed = sticky.run_session(args.port, checks, args.seed,
                                              args.size)
    except ProbeError as e:
        print(f"\nPRE-LATCHED-BRANCH LEG FAILED: {e}")
        checks.fail(f"pre-latched branch: {e}")
    finally:
        quit_engine(args.port, proc)

    if sticky_completed:
        print(f"== boot 4: reload the retired branch in a FRESH process "
              f"(#1941, port {args.port}) ==")
        proc = boot(args.port, log=LOG_D, label="tutorial retire reload engine")
        try:
            sticky.run_reload(args.port, checks, sticky_completed)
        except ProbeError as e:
            print(f"\nRETIRED-BRANCH RELOAD FAILED: {e}")
            checks.fail(f"retired-branch reload: {e}")
        finally:
            quit_engine(args.port, proc)
            remove_probe_slot(STICKY_SLOT)
    else:
        checks.fail("retired-branch reload: the pre-latched leg never "
                    "produced a save to reload")

    print(f"\n({time.time() - started:.0f}s)")
    if checks.failures:
        print(f"FAILED ({len(checks.failures)}):")
        for f in checks.failures:
            print(f"  - {f}")
        return 1
    print("ALL CHECKS PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
