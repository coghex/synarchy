#!/usr/bin/env python3
"""The first expedition, end to end — the arc's final integrated gate (#923).

`docs/expedition_gameplay_loop.md` step 9 asks for one scenario proving
the whole loop holds together. The ruin encounter ships in #916, but this
survival-control scenario deliberately selects a zero-occupant ruin, so
the loop is proved without hostile combat confounding the food control.
Since #917 that choice also makes the ruin's GUARANTEED significant item
the only outstanding half of its clearance predicate — the sharpest
available test of the conjunction, because nothing but recovering that
item can be what clears the place. What this gate proves is the
**man-versus-nature** loop:

    prepare -> travel -> discover -> extract -> return -> invest

with survival as the risk and supplies as the payoff.

Every component of that chain already had its own gate. What had never
been run is the chain AS ONE SESSION, from an empty world to a reloaded
save: `tools/location_embark_probe.py` stops at discovery,
`tools/expedition_retrieval_probe.py` starts from a staged item and its
own hand-built storage, `tools/tutorial_probe.py` covers preparation
only, and `tools/gameplay_scenarios.py` is an observation log with no
verdict at all.

STAGES
------
Failure output names the stage, because "the expedition failed" is not
diagnosable. The eight stages are independent and each can break on its
own:

  setup     a real world, a real placed ruin, a portal-eligible colony
            site, the portal, its own spawned roster, colony storage
  prepare   water secured by a real acolyte's own FOV scan; the
            traveller provisioned off the technomule through the normal
            inventory-transfer surface; the shipped first_session
            objective set at its expected value
  travel    both travellers walk ONE identical move leg to the ruin (no
            teleport) and are measured together at the ruin, and the
            ruin is DISCOVERED by sight (#1230) — lifecycle, player event,
            and per-unit knowledge
  extract   the retrieval order is issued at the ruin and the ruin's own
            seed-stable loot-table output is picked up through the real
            pickup_ground action
  return    the carrier walks home and deposits into colony storage from
            an adjacent tile
  save      the session is captured through the real save barrier
  load      a FRESH PROCESS loads it and every durable identity is
            re-checked: location instance, per-unit knowledge, objective
            latches, and the recovered item down to its instance id
  control   the unprepared control party, run identically, is measurably
            worse off at the same point in the journey

WHY A CONTROL RUN
-----------------
"The party walked there and back" is not evidence that preparation
matters; it is evidence that walking works. So the scenario runs TWO
travellers side by side, from the same colony, ordered to the SAME
destination tile in the same paused window, sampled at the same two
observation points — differing ONLY in what they carried out of the
colony:

  prepared   its spawn kit (a full 2 L canteen, rations) plus rations
             transferred off the technomule
  control    the same acolyte def, the same full canteen, no rations

The canteen stays full on BOTH, and that is the tighter experiment
rather than the softer one. A dry canteen puts `refill_canteen`'s
urgency ramp at its 7.5 peak, above `follow_command`'s 7.0, so an
unwatered traveller correctly abandons its orders and walks to the
nearest known water — which the scout's radio broadcast has already
told the whole colony about. Observed: a control that left the muster
and was found 10.7 tiles off-route beside the lake. That is a
difference in BEHAVIOUR, not in the supply being measured, so water is
held equal and food is the single variable.

Both set out with the SAME deficit — stomach at 0.20 of each unit's own
max, seeded inside one PAUSED window so neither AI can react to it
before both orders are issued. The prepared traveller then eats en route
through the ordinary AI (`eat_from_inventory`'s utility outranks both a
move order and a pickup order below `eat_max_fraction` — the documented
#306 ladder); the control has nothing to eat, and the gate asserts a
predetermined adverse delta in stomach fraction at the shared arrival
observation point.

The gated metric is FOOD, not water, and that is a deliberate choice
grounded in this repository's own calibration record rather than a
convenience. `docs/history/expedition_survival_calibration_2026-07.md`
measured a real 48-tile round trip and found that hunger pressure
genuinely becomes live on it — both acolytes crossed `eat_max_fraction`
and ate on the return leg, one finishing with no rations at all — while
hydration pressure never did: a unit at ~85% never reaches
`drink_min_thirst`, so its
canteen is never touched. Forcing thirst live would mean seeding a
dehydration deep enough that a *different* mechanism takes over:
`scripts/salts.lua` derives blood salt concentration as
saltFrac / hydrationFrac and `scripts/brain.lua` folds that straight
into consciousness, so a traveller dehydrated far enough to prefer
drinking over its orders is knocked unconscious by the electrolyte
imbalance — and if the sodium pool is scaled down to compensate, the
first meal's salt bolus knocks it out instead (both observed while
building this probe). That is a real interaction and arguably worth its
own issue, but "measure it" is not the same as "manufacture it", and
tuning survival constants here is explicitly out of scope. Water is
therefore held EQUAL between the two travellers and reported as evidence
at both observation points (litres carried, hydration fraction) rather
than gated.

Two scenario conditions are applied to BOTH travellers so that supplies
are genuinely the only variable:

  * the standing `find_water` goal is retired, so neither can walk off
    to a lake instead of travelling. This is the condition every probe
    in `tools/` applies, for the same reason.
  * foraging is neutralised for the whole session (`forage_max_fraction`
    on the live tunables table). This one is specific to this probe:
    #94's emergency inventory -> flora -> ground ladder is a real rescue
    path with its own gate (`tools/foraging_probe.py`), but left live
    here it would measure how generous the ground cover happens to be
    beside one particular ruin, not what preparation is worth.

The two travellers share ONE leg, end to end. They are first MUSTERED to one staging
tile and held there BY THE PAUSE, because a shared destination is not a
shared journey — hunger drains with time on the road, so departing from
36.4 and 31.5 tiles out, as an early run of this probe did, is a ~16%
difference in leg length sitting inside the measurement. A shared
DISTANCE is not enough either: a radial band is satisfied anywhere on a
circle, so the muster gathers them at a place, and the departure check
asserts how far apart they stand as well as how far each has to walk.

The hold is `engine.setPaused` and not `unit.setFrozen`, which is the
trap here: `uiFrozen` only makes the unit thread's `publishToRender`
skip the sim-derived update (`src/Unit/Thread.hs`), so a "frozen" unit
keeps walking under the simulation while `unit.getInfo` keeps reporting
where it was when the flag went up. A muster built on that reads stale
coordinates and then releases two travellers from wherever the sim has
actually carried them. The departure positions here are re-read and
re-checked with the simulation stopped, and everything up to the paired
orders happens inside that same paused window. They then travel under the same verb (`commandMove`), to
the same tile (the ruin's anchor), ordered in the same paused window.
The measurement is taken when BOTH are at the ruin in ONE
COHERENT SNAPSHOT — not "each has been there at some point", because the
two arrive at different times and the first one's own physiology can
still carry it back out (a completed player move order holds position
since #1216, but that hold yields to the same survival ladder the order
did) while the other is still walking; and not two `unit.getInfo` calls
either, because those are two round trips with the simulation running
in between, so a pair that was never inside together can satisfy them.
The candidate is a single paired read, and it is then revalidated with
the simulation STOPPED, with the control's metrics taken inside that
same stopped window. The prepared traveller's retrieval
order is issued only AFTERWARDS, in the extract stage.

What CANNOT also be equalised is elapsed time, and that is a deliberate
choice rather than an oversight. Acolyte walking speed varies with body
mass by roughly 1.5x
(`docs/history/expedition_survival_calibration_2026-07.md`), so two
travellers covering the same distance necessarily take different
amounts of it; equalising place and equalising time are mutually
exclusive. Place is the one that is fixed, because it is the one the
scenario is about — "the party got to the ruin" — and because the
residual is neutralised twice over: both metrics are fractions of each
unit's OWN maximum, and the delta's cause is asserted as an observed
`eat_from_inventory` action rather than inferred from its size. Time on
the road only changes how far the control's stomach falls; it cannot
make the prepared traveller's RISE.

Both of those are load-bearing. `commandMove` walks at
`movement_speed.ordered` while `pickup_ground` walks at `comfort`, and
ordered is comfort * 1.15 — so ordering one traveller to fetch and the
other merely to walk would bury a 15% speed difference inside a
comparison that is supposed to isolate supplies. And the control is
given NO retrieval target of its own: handing it the ruin's second loot
roll would make the loot TABLE part of the experiment, because a ruin
can roll food (instance 3 on the default seed rolls `rations`) and a
control that eats what it finds destroys the very measurement it exists
to provide. The control is a control for the JOURNEY; extraction is
#920's probe's job.

Encumbrance is levelled too: both travellers are shed to inside their
carrying capacity before departure.
`docs/history/expedition_survival_calibration_2026-07.md`
observation E1 recorded a small acolyte walking a whole route at 121% of
capacity at roughly half speed, and a traveller that slow makes no new
closest approach inside `pickup_timeout`, so its order is correctly
retired and it never arrives at all.

The control's degradation is a MEASUREMENT, not a scripted death, and
the gate does not infer the mechanism from the number: it counts what
was CONSUMED. The provisioned traveller must arrive with fewer rations
than it left with, and the control must have had none to eat and eaten
none. Consumption rather than a caught `eat_from_inventory` action,
because `eatExecute` finishes a whole meal inside one AI tick — at a
~1 s poll, catching the action is a coin flip (a run whose stomach
demonstrably went 0.20 -> 0.82 recorded the action as unseen), whereas
`unit.feed` removes a discrete ration outright and the pack still shows
it minutes later. The action sighting is reported alongside, as
corroboration. So the delta is attributed to eating, not to the two
acolytes' separately rolled body masses — which is also why both
metrics are fractions of each unit's OWN maximum rather than absolute
litres or kcal.

WHAT IS DELIBERATELY NOT DONE
-----------------------------
  * No lifecycle is manufactured. `world.setLocationLifecycle` is never
    called. The selected ruin's persisted zero-occupant encounter starts
    clear internally but stays `unknown` until sight, then becomes
    `discovered` — and only reaches `cleared` once its #917 guaranteed
    significant item has actually been carried out, which this run does
    through the real pickup boundary. WHO carries it is not asserted:
    `processing_unit` is a Materials def, so a colonist standing in the
    ruin may recover it of its own accord before the player's gesture,
    which clears the location just as legitimately. An occupied ruin
    would first become `discovered`,
    then `active` on autonomous aggression, and remain so until every
    assigned nomad is dead AND that item is taken.
  * No item is staged in the ruin. The measured extraction target is
    whichever def the ruin's own two `ruin_common` rolls produced (#921
    removed the fixed entries; #948 made the draw seed-stable per
    instance), chosen by a deterministic preference rule and reported in
    the fingerprint. #917's guaranteed item is recovered too, on the
    same trip and through the same player gesture, but it is deliberately
    NOT the measured target: it is a Materials def whose `store_materials`
    AI would bank it autonomously mid-return, and the journey
    measurements have to be about a carried item nothing else moves.
  * No progression project is completed. "Invest" means the recovered
    loot is banked in colony storage and is afterwards indistinguishable
    from a locally produced item — a different colonist withdraws that
    exact instance and holds it with every property intact. #917's
    reward earns its place by being worth the trip and by gating the
    ruin's cleared state (D-6, as amended by D-17), not by unlocking a
    capability; a technology tree remains out of scope.
  * Nothing is teleported, and no state is written to satisfy a loop
    stage. Direct mutation appears only in clearly separated fixture
    setup (finishing the storage building, seeding the shared hunger
    deficit, retiring find_water) — never in place of a stage.

DETERMINISM AND REPEATABILITY
-----------------------------
Fixed seed, fixed size, fixed plate count; the ruin and colony site are
chosen by a total order over the world's own deterministic placement
list. The run prints a single `FINGERPRINT` line carrying the selected
ruin instance, its anchor, its rolled loot, the extraction target, the
guaranteed significant item's def and physical instance id, the
colony and water tiles, the completed objective set, and the per-stage
outcomes — so two consecutive invocations can be diffed as one line for
identity AND result, not merely compared on exit status. Sampled
measurements (the control's stomach delta) are printed separately and
deliberately kept OUT of the fingerprint: two honest runs of the same
seed must agree on what happened, not on a physiological reading's last
digits. Ground-item SCATTER COORDINATES are excluded for the same
reason — they remain `math.random`-driven by design (#948 pins the
selected item sequence, not where in the room each lands).

Runs against a throwaway isolated resource root on a non-default port,
so it never reads or writes the developer's real `saves/` and never
touches a graphical session on 8008.

OWNERS
------
This file is the facade, and the whole scenario body lives in
`tools/expedition_loop/` (#2092): CLI parsing, the ordered two-engine
lifecycle, stage dispatch, the fingerprint line and the aggregate exit
are here, and nothing else is. `tools/expedition_loop/__init__.py` is
the ownership map; the short version is `harness` (checks, isolation,
bootstrap, the shared state record and the one fingerprint accumulator),
`readers` (the shared engine queries and geometry), `constants`, and one
module per stage group — `setup`, `prepare`, `travel` (which owns the
`control` measurement scored from its own paired samples), `extract`
(which owns the `return` leg the same recovered instance makes), and
`persistence` (`save` and `load`).

Those are LIBRARIES, not probes: there is still exactly one
`expedition_loop` registration in `tools/probe_runner_registry.py`, one
`tools/ci_probes.py` classification, one `docs/probe_census.json` row,
and one executable — this one.

Usage:
  python3 tools/expedition_loop_probe.py
  python3 tools/expedition_loop_probe.py --seed 42 --size 64 --port 9923

Exit code 0 = all checks passed; 1 = a check failed; 2 = a stage refused
to go on, so the run could not reach the state it tests.
"""
from __future__ import annotations

import argparse
import sys
import tempfile
import traceback

from probelib import quit_engine

# Imported at MODULE scope, every one of them, deliberately: the cheap
# GPU-free acceptance gate for this probe is
# `python3 -c 'import expedition_loop_probe'`, and it can only catch a
# syntax or import error in an owner that the facade actually imports.
from expedition_loop import extract, persistence, prepare, setup, travel
from expedition_loop.constants import LOG_A, LOG_B
from expedition_loop.harness import (Checks, ExpeditionState, Fingerprint,
                                     SetupError, StageAbort, boot_probe,
                                     bootstrap, make_isolated_root,
                                     remove_root)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    ap.add_argument("--port", type=int, default=9923)
    ap.add_argument("--keep-root", action="store_true",
                    help="don't delete the throwaway resource root on exit")
    args = ap.parse_args()

    chk = Checks()
    base = tempfile.mkdtemp(prefix="synarchy_expedition_loop_")
    root = make_isolated_root(base)
    print(f"isolated resource root: {root}", flush=True)

    fingerprint = Fingerprint(args.seed, args.size, args.plates)
    port = args.port
    st = ExpeditionState(port=port, fp=fingerprint, seed=args.seed,
                         size=args.size, plates=args.plates)
    # An early refusal exits 2, distinct from a failed check's 1. It was
    # eleven `return 2`s inside this function before the stage bodies
    # had owners; it is one exception the owners raise and this function
    # propagates now.
    refused = False

    try:
        # ============ engine A: the whole expedition ===================
        proc = boot_probe(root, port, LOG_A, "engine A")
        try:
            bootstrap(port)
            setup.run(chk, st)
            prepare.run(chk, st)
            travel.run(chk, st)
            extract.run(chk, st)
            extract.deliver(chk, st)
            persistence.save(chk, st)
            # Measured HERE, in engine A, from the observations `travel`
            # retained — before the fresh process, though it is reported
            # last.
            travel.measure_control(chk, st)
        finally:
            quit_engine(port, proc)

        # ============ engine B: a genuinely fresh process ==============
        # Entered before the boot, so an engine that dies before READY
        # is attributed to `load` rather than to `control`.
        persistence.enter_load(chk)
        proc = boot_probe(root, port, LOG_B, "engine B")
        try:
            bootstrap(port)
            persistence.load(chk, st)
        finally:
            quit_engine(port, proc)
    except StageAbort:
        # A stage could not go on and has ALREADY recorded its own
        # failing check. Caught before the clause below, which would
        # otherwise record a second, spurious operational failure over
        # the top of it, and deliberately silent for the same reason.
        refused = True
    except SetupError as exc:
        chk.ok(False, f"the scenario could not reach the state it tests: {exc}")
    except SystemExit as exc:
        # `probelib.boot` reports an engine that died before READY, or
        # never printed it, by calling sys.exit() with a message — and
        # SystemExit derives from BaseException, NOT Exception, so the
        # clause below does not see it. Left uncaught it would unwind
        # straight through the finally, which prints the stage summary
        # first: a stage entered but not yet asserted in (engine B's
        # `load`, most obviously) would be reported as passing on the
        # way out. Recorded here for the same reason, and with the same
        # shape, as any other operational failure.
        chk.ok(False, f"the engine could not be started, or died, during stage "
                      f"'{chk.stage}' (SystemExit: {exc.code})")
    except Exception as exc:  # noqa: BLE001
        # An operational failure — a dead engine, a socket timeout, a
        # malformed console response — is a real probe failure and must
        # name its stage like any other. Left to propagate it would exit
        # non-zero with a traceback but NO recorded failing check, and
        # the summary below would then print PASS over the top of it.
        # KeyboardInterrupt is deliberately still allowed to propagate:
        # that one really is the operator, not the run.
        chk.ok(False, f"unexpected {type(exc).__name__} while running stage "
                      f"'{chk.stage}': {exc}")
        traceback.print_exc()
    finally:
        remove_root(base, args.keep_root)
        fingerprint["stages"] = chk.outcomes()
        print(f"\n{fingerprint.line()}", flush=True)
        chk.report()

    if refused:
        return 2
    return 0 if chk.failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
