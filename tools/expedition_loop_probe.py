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
convenience. `docs/expedition_survival_calibration.md` measured a real
48-tile round trip and found that hunger pressure genuinely becomes live
on it — both acolytes crossed `eat_max_fraction` and ate on the return
leg, one finishing with no rations at all — while hydration pressure
never did: a unit at ~85% never reaches `drink_min_thirst`, so its
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
mass by roughly 1.5x (`docs/expedition_survival_calibration.md`), so
two travellers covering the same distance necessarily take different
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
carrying capacity before departure. `docs/expedition_survival_calibration.md`
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
    `cleared` — and since #917 only once its guaranteed significant item
    has actually been carried out, which this run does through the real
    pickup boundary. An occupied ruin would first become `discovered`,
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

Usage:
  python3 tools/expedition_loop_probe.py
  python3 tools/expedition_loop_probe.py --seed 42 --size 64 --port 9923

Exit code 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import glob
import json
import math
import os
import shutil
import sys
import tempfile
import time
import traceback

from probelib import (boot, quit_engine, send, send_json, poll_until,
                      capture_request_id, wait_save_complete,
                      wait_load_published)

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

PAGE = "expedition"
SLOT = "expedition_loop_probe"
LOG_A = "/tmp/expedition_loop_probe_a.log"
LOG_B = "/tmp/expedition_loop_probe_b.log"

PORTAL_DEF = "acolyte_portal"
STORAGE_DEF = "cargo_hold_S"
RATIONS_DEF = "rations"
ACOLYTE_DEF = "acolyte"
MULE_DEF = "technomule"

#: The widest a unit's night-aware sight radius can reach (#1230):
#: perception * Unit.LineOfSight.awareRangeTiles (6.0), with the
#: page-local night factor only ever shrinking it. No shipped unit
#: carries a perception above 2.0, so 12 tiles bounds every sightline
#: this probe can produce. It replaces the def's removed 6-tile
#: discovery halo as the "far enough not to reveal it" boundary.
MAX_SIGHT_TILES = 12

#: The colony sits this far from the ruin anchor: comfortably beyond
#: MAX_SIGHT_TILES (so the ruin is not revealed the moment the colony
#: is planted), and far enough that the trip is a real journey rather
#: than a stroll across the camp.
HOME_MIN_DIST = 26
HOME_MAX_DIST = 30

#: A colony needs fresh water within this many tiles, so the
#: "Secure water source" objective is earned by a real short walk
#: rather than by a cross-map expedition of its own.
WATER_MAX_DIST = 20

#: Largest surface-Z step tolerated along the straight colony->ruin
#: line. Placement legality says nothing about whether a party can WALK
#: a route (tools/location_embark_probe.py learned the same lesson about
#: cliffs), and a route no path can follow would fail the journey for
#: reasons that have nothing to do with the loop under test.
MAX_CORRIDOR_STEP = 2.0

#: A single position sample may not jump further than this. A sample is
#: about a second of walking at the acolyte's ~0.2-0.7 tiles/s regime;
#: 4.0 leaves headroom for a slow console round trip while still ruling
#: out a teleport.
MAX_STEP_TILES = 4.0

#: The shared departure deficit, as a fraction of each traveller's own
#: max_hunger, applied identically to BOTH travellers while PAUSED.
#: 0.20 sits below `eat_max_fraction` (0.25), so eat_from_inventory is
#: live from the first step; its utility is (1 - 0.20) * eat_weight 10.0
#: = 8.0, above both follow_command (7.0) and pickup (7.5), which is the
#: documented #306 ladder — a hungry unit interrupts its orders to eat.
#: Not lower: the control cannot eat at all, and starting it at 0.10 ran
#: its stomach to empty well before the observation point and left it
#: collapsing on arrival. An empty control is a valid outcome, but one
#: that collapses MID-leg never reaches the shared observation point at
#: all, which would make the gate flaky rather than strict.
DEPART_STOMACH_FRAC = 0.20

#: The departure-origin contract. Hunger drains with time on the road,
#: so an unequal origin — in distance OR in bearing, since route shape
#: is also time — would be a second variable beside supplies. An early
#: run departed from 36.4 and 31.5 tiles out: a ~5-tile (16%) spread on
#: a ~30-tile leg.
#:
#: Both travellers are gathered within STAGING_RADIUS of ONE staging
#: tile and verified there with the simulation stopped (see
#: muster_travellers), which bounds all three quantities: how far apart
#: they stand, how far each has to walk, and — because both are within a
#: couple of tiles of a point ~32 tiles from the ruin — their bearing to
#: it, to within a few degrees.
STAGING_RADIUS = 2.0
MAX_START_SEPARATION = 3.0
MAX_START_SPREAD = 2.0

#: The predetermined adverse delta the control must show at the shared
#: arrival observation point. `eatExecute` feeds until the stomach is
#: >= 99% full or the pack is empty, so a provisioned traveller lands
#: near 1.0 and an unprovisioned one keeps draining from 0.10 — this bar
#: tests that the mechanism ran at all, not its exact yield.
MIN_STOMACH_DELTA = 0.40

#: Objective ids, from data/tutorials/first_session.yaml.
OBJ_PORTAL = "first_session_place_portal"
OBJ_WATER = "first_session_secure_water"
OBJ_EXPEDITION = "first_session_prepare_expedition"
SUB_WATER = "first_session_prepare_water"
SUB_FOOD = "first_session_prepare_food"

#: What this scenario is designed to leave latched, exactly. All three
#: are FULL objectives, so they latch permanently and must survive the
#: reload; the two subobjectives are live checks and are asserted
#: separately (they are recomputed from the loaded world, not restored).
EXPECTED_COMPLETED = {OBJ_PORTAL, OBJ_WATER, OBJ_EXPEDITION}

STAGES = ["setup", "prepare", "travel", "extract", "return",
          "save", "load", "control"]


# --------------------------------------------------------------------------
# Stage-aware check recorder
# --------------------------------------------------------------------------
class Checks:
    """Every check is attributed to one of the eight STAGES, so a failure
    says WHICH part of the loop broke instead of only that it did."""

    def __init__(self) -> None:
        self.failed = 0
        self.stage = STAGES[0]
        self.by_stage: dict[str, list[int]] = {s: [0, 0] for s in STAGES}
        self.reached: list[str] = []

    def enter(self, stage: str, title: str) -> None:
        assert stage in STAGES, stage
        self.stage = stage
        if stage not in self.reached:
            self.reached.append(stage)
        print(f"\n=== [{stage}] {title} ===", flush=True)

    def ok(self, cond: bool, label: str) -> bool:
        cond = bool(cond)
        # Recording against a stage counts as reaching it, so a failure
        # attributed here reports as FAIL rather than NOT REACHED — which
        # matters for an operational failure raised before its own
        # enter().
        if self.stage not in self.reached:
            self.reached.append(self.stage)
        slot = self.by_stage[self.stage]
        slot[0 if cond else 1] += 1
        print(f"  [{'PASS' if cond else 'FAIL'}][{self.stage}] {label}", flush=True)
        if not cond:
            self.failed += 1
        return cond

    def fail_setup(self, label: str) -> None:
        """A stage could not even be reached — reported the same way a
        failing check is, so the summary still names a stage."""
        self.ok(False, label)

    def outcomes(self) -> dict[str, str]:
        """Per-stage pass/fail, for the run fingerprint. Deliberately
        outcomes only and no measurements: two runs of the same seed must
        agree on WHAT happened, while a sampled physiological delta is a
        measurement and will differ in its last digits."""
        out = {}
        for stage in STAGES:
            passed, failed = self.by_stage[stage]
            if stage not in self.reached:
                out[stage] = "not-reached"
            else:
                out[stage] = "fail" if failed else "pass"
        return out

    def report(self) -> None:
        print("\n--- stage summary ---", flush=True)
        broken, failing = [], []
        for stage in STAGES:
            passed, failed = self.by_stage[stage]
            if failed:
                status = f"FAIL ({failed} of {passed + failed} checks)"
                broken.append(stage)
                failing.append(stage)
            elif stage not in self.reached:
                status = "NOT REACHED"
                broken.append(stage)
            else:
                status = f"pass ({passed} checks)"
            print(f"  {stage:<9} {status}", flush=True)
        if broken:
            # Name the first stage that actually FAILED a check; a stage
            # that was never reached is a consequence, not the cause.
            culprit = failing[0] if failing else broken[0]
            print(f"\n--- FAIL: the expedition loop broke at stage "
                  f"'{culprit}' (stages affected: {', '.join(broken)}) ---",
                  flush=True)
        else:
            print("\n--- PASS: the first expedition runs end to end "
                  "(prepare, travel, discover, extract, return, invest) ---",
                  flush=True)


class SetupError(RuntimeError):
    """The scenario could not reach the state it tests."""


# --------------------------------------------------------------------------
# Boot / bootstrap
# --------------------------------------------------------------------------
YAML_LOADERS = [
    ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
    ("data/items/*.yaml", "engine.loadItemYaml"),
    ("data/equipment/*.yaml", "engine.loadEquipmentYaml"),
    ("data/materials/*.yaml", "engine.loadMaterialYaml"),
    ("data/flora/*.yaml", "engine.loadFloraYaml"),
    ("data/units/*.yaml", "engine.loadUnitYaml"),
    ("data/buildings/*.yaml", "engine.loadBuildingYaml"),
    ("data/loot_tables/*.yaml", "engine.loadLootTableYaml"),
]

#: The AI stack, the portal's own spawn sequencer, and the tutorial
#: runtime — the same modules scripts/init_loader.lua loads in a real
#: session, at the same z-orders. Headless has no loading screen, so
#: both engines do it by hand.
SCRIPTS = [
    ("scripts/unit_stats.lua", 0.1),
    ("scripts/unit_resources.lua", 0.2),
    ("scripts/unit_ai.lua", 0.1),
    ("scripts/building_spawn.lua", 0.3),
    ("scripts/tutorial_progress.lua", 1.0),
    ("scripts/tutorial_eval.lua", 1.0),
]


def make_isolated_root(base: str) -> str:
    """A throwaway resource root: the real read-only content families
    symlinked, plus its OWN empty saves/."""
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def bootstrap(port: int) -> None:
    for pattern, fn in YAML_LOADERS:
        for path in sorted(glob.glob(os.path.join(REPO, pattern))):
            send(port, f"{fn}('{os.path.relpath(path, REPO)}'); return 'ok'",
                 timeout=20.0)
    send(port, "engine.loadLocationYaml('data/locations/ruin_small.yaml'); "
               "return 'ok'", timeout=20.0)
    got = send(port, "return engine.loadTutorialDir('data/tutorials')",
               timeout=20.0)
    if got.strip() in ("", "nil", "false"):
        raise SetupError(f"engine.loadTutorialDir failed: {got!r}")
    for script, z in SCRIPTS:
        send(port, f"engine.loadScript('{script}', {z}); return 'ok'", timeout=20.0)
    tree = send(port, "local t = require('scripts.tutorial_progress').ensureTree(); "
                      "return t and t.id or 'nil'", timeout=15.0)
    if tree != "first_session":
        raise SetupError(f"expected the first_session tutorial tree, got {tree!r}")
    # Scenario condition, applied to the whole session and therefore to
    # BOTH travellers symmetrically (see the module docstring): retire
    # the #94 forage ladder so an unprovisioned traveller cannot eat the
    # landscape instead of its pack.
    send(port, "require('scripts.unit_ai_tunables').acolyte.forage_max_fraction "
               "= -1; return 'ok'", timeout=15.0)


def boot_probe(root: str, port: int, log: str, label: str):
    return boot(port, log=log, label=label, ready_timeout=240,
                args=["--resource-root", root])


# --------------------------------------------------------------------------
# Small readers
# --------------------------------------------------------------------------
def _as_float(raw):
    try:
        return float(raw)
    except (TypeError, ValueError):
        return None


def dist(a, b) -> float:
    return math.hypot(a[0] - b[0], a[1] - b[1])


def bearing_gap(a, b, target) -> float:
    """Angle between the two bearings from `a` and `b` to `target`, in
    degrees, wrapped into [0, 180].

    The wrap is the point: a raw difference of two atan2 results runs
    over the +/-pi branch cut, which reports two travellers standing
    three tiles apart as 354.6 degrees rather than 5.4."""
    d = (math.atan2(target[1] - a[1], target[0] - a[0])
         - math.atan2(target[1] - b[1], target[0] - b[0]))
    return abs(math.degrees(math.atan2(math.sin(d), math.cos(d))))


def surface_z(port: int, gx: int, gy: int):
    """world.getSurfaceAt returns several values; the console tab-joins
    them, so wrap the call in parens to keep only the first."""
    return _as_float(send(port, f"return (world.getSurfaceAt({gx},{gy}))"))


def load_region(port: int, cx: int, cy: int, pad: int = 4) -> None:
    send(port, f"return world.loadChunksInRegion({cx-pad},{cy-pad},"
               f"{cx+pad},{cy+pad})", timeout=60.0)
    send(port, "return world.waitForChunks(180)", timeout=190.0)


def ground_items(port: int) -> list:
    data = send_json(port, "return item.listGround()")
    return data if isinstance(data, list) else []


def unit_pos(port: int, uid: int):
    r = send_json(port, f"local i=unit.getInfo({uid}); "
                        f"return i and {{x=i.gridX,y=i.gridY}} or nil")
    if isinstance(r, dict) and "x" in r:
        return float(r["x"]), float(r["y"])
    return None


def paired_positions(port: int, a: int, b: int) -> dict:
    """Both travellers' positions in ONE console round trip.

    Two `unit_pos` calls are two round trips with the simulation running
    in between, so a condition evaluated over them combines coordinates
    from different moments — "both at the ruin" could pass for a
    pair that was never inside it together. One request reads both from
    the same unit-manager state instead."""
    lua = (f"local function f(u) local i=unit.getInfo(u); "
           f"return i and (i.gridX..','..i.gridY) or 'nil' end; "
           f"return f({a})..';'..f({b})")
    raw = send(port, lua).strip().strip('"')
    out: dict = {a: None, b: None}
    parts = raw.split(";")
    for uid, part in zip((a, b), parts):
        if "," not in part:
            continue
        try:
            x, y = part.split(",")
            out[uid] = (float(x), float(y))
        except ValueError:
            continue
    return out


def current_action(port: int, uid: int) -> str:
    return send(port, f"local s=require('scripts.unit_ai').getState({uid}); "
                      f"return s and s.currentAction or 'nil'")


def pose(port: int, uid: int) -> str:
    return send(port, f"return unit.getPose({uid})")


def event_log(port: int) -> list:
    data = send_json(port, "return engine.getEventLog()")
    return data if isinstance(data, list) else []


def inventory(port: int, uid: int) -> list:
    """A unit's loose inventory with every stable per-instance property.

    `temp` is deliberately excluded: it is present only while an item is
    off ambient, so it can legitimately appear or vanish on its own."""
    lua = (f"local out={{}}; for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
           f"out[#out+1]={{defName=it.defName,instanceId=it.instanceId,"
           f"displayName=it.displayName,weight=it.weight,fill=it.currentFill,"
           f"sharpness=it.sharpness,broken=it.broken,contentsKey=it.contentsKey,"
           f"quality=it.quality,condition=it.condition}} end; "
           f"return out")
    data = send_json(port, lua)
    return data if isinstance(data, list) else []


#: The per-instance properties compared across the round trip.
#:
#: An explicit shared key set, not "every field the surface returned":
#: `building.getStorage` joins the item DEF into each row (category,
#: material, kind, iconTex, the weapon block, ...) while
#: `unit.getInventory` returns a narrower per-instance view, and the two
#: spell the container level differently (`currentFill` vs `fill`).
#: Comparing raw dicts would therefore fail on shape rather than on
#: substance. `broken` is deliberately absent: it is an inventory-only
#: field, so a storage row cannot be asked about it, and including it
#: would compare a real False against a missing key. `temp` is excluded
#: too — it is present only while an item is off ambient, so it can
#: legitimately appear or vanish on its own.
PROP_KEYS = ("defName", "displayName", "weight", "sharpness",
             "contentsKey", "quality", "condition")


def properties(item: dict) -> dict:
    """The instance's identity-independent properties, normalised so an
    inventory row and a colony-storage row are comparable."""
    item = item or {}
    out = {k: item.get(k) for k in PROP_KEYS}
    fill = item.get("fill")
    out["fill"] = item.get("currentFill") if fill is None else fill
    return out


def num(value, default: float = 0.0) -> float:
    """`value or default` is wrong for these metrics: a stomach that has
    genuinely drained to 0.0 is falsy, and substituting a default there
    silently inverts the control comparison."""
    return default if value is None else float(value)


def find_instance(items: list, inst_id):
    for it in items:
        if it.get("instanceId") == inst_id:
            return it
    return None


def stat_fraction(port: int, uid: int, stat: str, max_stat: str):
    cur = _as_float(send(port, f"return unit.getStat({uid},'{stat}')"))
    mx = _as_float(send(port, f"return unit.getStat({uid},'{max_stat}')"))
    if cur is None or not mx or mx <= 0:
        return None
    return cur / mx


def vitals(port: int, uid: int) -> dict:
    """The two survival metrics the control run is scored on, plus the
    pose that would explain a zero."""
    return {
        "hydration": stat_fraction(port, uid, "hydration", "max_hydration"),
        "stomach": stat_fraction(port, uid, "hunger", "max_hunger"),
        "load": load_fraction(port, uid),
        "pose": pose(port, uid),
    }


def fmt_vitals(v: dict) -> str:
    def pct(x):
        return "n/a" if x is None else f"{x * 100:.0f}%"
    return (f"hydration {pct(v['hydration'])}, stomach {pct(v['stomach'])}, "
            f"load {pct(v.get('load'))} of capacity, pose {v['pose']}")


def known_locations(port: int, uid: int) -> set[str]:
    """(#915) The per-unit location memories `uid` holds, as a set of
    "<page>#<instance id>" keys, read through the same public query
    surface the AI candidates use. Flattened to a string because an
    empty Lua table serializes identically to an empty object."""
    lua = (f"local ai=require('scripts.unit_ai'); local out={{}}; "
           f"for _,k in ipairs(ai.getKnownLocations({uid})) do "
           f"out[#out+1]=k.page..'#'..tostring(k.id) end; "
           f"return table.concat(out, ',')")
    raw = send(port, lua).strip().strip('"')
    return {p for p in raw.split(",") if p}


def placed(port: int, page: str) -> list:
    data = send_json(port, f"return world.listPlacedLocations('{page}')")
    return data if isinstance(data, list) else []


def instance_by_id(port: int, page: str, inst_id: int):
    data = send_json(port, f"return world.getLocationInstance({inst_id}, '{page}')")
    return data if isinstance(data, dict) else None


def check_ai_tick_clean(chk: Checks, log_path: str, label: str) -> None:
    """No `Lua error in update()` in the engine log.

    A raise out of the unit_ai update tick kills EVERY unit's AI for that
    tick, not just the action that raised — so it fails silently as
    "nothing moved" rather than as an error anyone is looking at."""
    try:
        with open(log_path) as fh:
            bad = [ln.strip() for ln in fh if "Lua error in update()" in ln]
    except OSError as exc:
        chk.ok(False, f"{label}: could not read the engine log ({exc})")
        return
    uniq = sorted(set(bad))
    chk.ok(not uniq,
           f"{label}: the unit_ai update tick raised nothing "
           f"({len(bad)} error line(s){': ' + uniq[0] if uniq else ''})")


# --------------------------------------------------------------------------
# Tutorial progress, read through the module's own public surface
# --------------------------------------------------------------------------
PROGRESS_LUA = (
    "local tp = require('scripts.tutorial_progress'); "
    "local ck = {}; "
    "for _, id in ipairs(tp.index and tp.index.order or {}) do "
    "if tp.isSubobjectiveChecked(id) then ck[#ck+1] = id end end; "
    "return table.concat(tp.completedIds(), ',') .. '#' "
    ".. table.concat(ck, ',')"
)


def progress(port: int) -> tuple[set[str], set[str]]:
    """(completed full objectives, checked subobjectives). Read as a
    delimited string rather than a JSON table so an empty set and an
    empty object stay distinguishable."""
    raw = send(port, PROGRESS_LUA, timeout=15.0)
    completed, _, checked = raw.partition("#")
    return ({c for c in completed.split(",") if c},
            {c for c in checked.split(",") if c})


# --------------------------------------------------------------------------
# [setup] site selection
# --------------------------------------------------------------------------
def corridor_roughness(port: int, x0: int, y0: int, x1: int, y1: int,
                       samples: int = 24):
    """Largest surface-Z step between consecutive samples along the
    straight line between two tiles, or None if any sample is unresolved
    or wet."""
    lua = (f"local worst=0; local prev=nil; "
           f"for i=0,{samples} do local t=i/{samples}; "
           f"local x=math.floor({x0}+({x1}-{x0})*t); "
           f"local y=math.floor({y0}+({y1}-{y0})*t); "
           f"local sz=(world.getSurfaceAt(x,y)); local f=world.getFluidAt(x,y); "
           f"if not sz or f then return -1 end; "
           f"if prev then local d=math.abs(sz-prev); if d>worst then worst=d end end; "
           f"prev=sz end; return worst")
    v = _as_float(send(port, lua, timeout=30.0))
    if v is None:
        return None
    return None if v < 0 else v


def site_candidates(port: int, gx: int, gy: int, rz: float) -> list:
    """Every colony candidate around one ruin, scanned SERVER-side in a
    single console round trip.

    A candidate is a dry, z-compatible, portal-eligible tile on the
    HOME_MIN_DIST..HOME_MAX_DIST ring whose straight corridor to the ruin
    is walkable and which has lake or river water within WATER_MAX_DIST.
    Portal eligibility is asked of `building.canPlaceAt` — the same
    validator `building.spawn` runs, and the one that refuses a starting
    building inside a placed location's bounds (#777).

    Returned as dicts sorted into a TOTAL order (water distance, then
    corridor roughness, then the tile itself), so the selection is a
    function of the seed alone."""
    lua = (
        "local function rough(x0,y0,x1,y1) "
        " local worst=0; local prev=nil; "
        " for i=0,24 do local t=i/24; "
        "  local x=math.floor(x0+(x1-x0)*t); local y=math.floor(y0+(y1-y0)*t); "
        "  local sz=(world.getSurfaceAt(x,y)); local f=world.getFluidAt(x,y); "
        "  if not sz or f then return -1 end; "
        "  if prev then local d=math.abs(sz-prev); if d>worst then worst=d end end; "
        "  prev=sz end; return worst end; "
        "local function water(x,y) "
        f" for d=1,{WATER_MAX_DIST} do for a=0,31 do local ang=a*math.pi/16; "
        "  local px=math.floor(x+d*math.cos(ang)); "
        "  local py=math.floor(y+d*math.sin(ang)); "
        "  local f=world.getFluidAt(px,py); "
        "  if f=='lake' or f=='river' then return d,px,py end end end; "
        " return -1,0,0 end; "
        f"local out={{}}; local seen={{}}; "
        f"for rr={HOME_MIN_DIST},{HOME_MAX_DIST} do for a=0,143 do "
        " local ang=a*math.pi/72; "
        f" local x=math.floor({gx}+rr*math.cos(ang)); "
        f" local y=math.floor({gy}+rr*math.sin(ang)); "
        " local k=x..','..y; if not seen[k] then seen[k]=true; "
        "  local sz=(world.getSurfaceAt(x,y)); local f=world.getFluidAt(x,y); "
        f"  if sz and not f and math.abs(sz-{rz})<=2 "
        f"     and building.canPlaceAt('{PORTAL_DEF}',x,y) then "
        f"   local g=rough(x,y,{gx},{gy}); "
        f"   if g>=0 and g<={MAX_CORRIDOR_STEP} then "
        "     local wd,wx,wy=water(x,y); if wd>0 then "
        "      out[#out+1]=x..','..y..','..sz..','..g..','..wd..','..wx..','..wy "
        "     end end end end end end; return table.concat(out,';')")
    raw = send(port, lua, timeout=900.0)
    rows = []
    for part in raw.split(";"):
        if not part:
            continue
        try:
            x, y, z, g, wd, wx, wy = part.split(",")
        except ValueError:
            continue
        rows.append({"x": int(x), "y": int(y), "z": float(z),
                     "rough": float(g), "water_dist": int(wd),
                     "wx": int(wx), "wy": int(wy)})
    rows.sort(key=lambda r: (r["water_dist"], r["rough"], r["x"], r["y"]))
    return rows


def shore_tile(port: int, wx: int, wy: int):
    """A dry, resolvable tile adjacent to the water tile — where a scout
    can actually stand while the lake comes into its field of view."""
    lua = (f"for _,o in ipairs({{{{1,0}},{{-1,0}},{{0,1}},{{0,-1}},"
           f"{{2,0}},{{-2,0}},{{0,2}},{{0,-2}},{{2,2}},{{-2,-2}}}}) do "
           f"local x={wx}+o[1]; local y={wy}+o[2]; "
           f"local sz=(world.getSurfaceAt(x,y)); local f=world.getFluidAt(x,y); "
           f"if sz and not f then return x..','..y end end; return 'none'")
    raw = send(port, lua, timeout=30.0)
    if raw == "none" or "," not in raw:
        return None
    x, y = raw.split(",")
    return int(x), int(y)


def pick_site(chk: Checks, port: int):
    """Choose the ruin and the colony site.

    Zero-occupant ruins are considered in `world.listPlacedLocations`' own order —
    which is the deterministic overlay order that allocated their
    instance ids (#911) — and the first one that yields a candidate wins.
    Returns (ruin, site) or None."""
    all_ruins = poll_until(60.0, lambda: [
        e for e in placed(port, PAGE)
        if isinstance(e, dict) and e.get("id") == "ruin_small"])
    if not all_ruins:
        chk.fail_setup("the world places at least one ruin_small")
        return None
    ruins = [e for e in all_ruins
             if int((e.get("encounter") or {}).get("rolled_count", -1)) == 0]
    print(f"  {len(all_ruins)} ruin_small placed: "
          f"{[(e['instance_id'], e['gx'], e['gy'], (e.get('encounter') or {}).get('rolled_count')) for e in all_ruins]}", flush=True)
    if not ruins:
        chk.fail_setup("the world places a zero-occupant ruin_small so hostile "
                       "combat cannot confound the food-control journey")
        return None

    for ruin in ruins:
        gx, gy = int(ruin["gx"]), int(ruin["gy"])
        load_region(port, int(ruin["cx"]), int(ruin["cy"]))
        rz = surface_z(port, gx, gy)
        if rz is None:
            print(f"  ruin {ruin['instance_id']} ({gx},{gy}): anchor unresolved, "
                  f"trying next", flush=True)
            continue
        cands = site_candidates(port, gx, gy, rz)
        if not cands:
            print(f"  ruin {ruin['instance_id']} ({gx},{gy}) z={rz}: no colony "
                  f"site with walkable corridor and water within "
                  f"{WATER_MAX_DIST} tiles, trying next", flush=True)
            continue
        site = cands[0]
        shore = shore_tile(port, site["wx"], site["wy"])
        if shore is None:
            print(f"  ruin {ruin['instance_id']}: no dry shore beside "
                  f"({site['wx']},{site['wy']}), trying next", flush=True)
            continue
        site["shore"] = shore
        print(f"  site: ruin instance {ruin['instance_id']} at ({gx},{gy}) z={rz}; "
              f"colony ({site['x']},{site['y']}) z={site['z']} at "
              f"{dist((site['x'], site['y']), (gx, gy)):.1f} tiles "
              f"(corridor roughness {site['rough']}); water "
              f"({site['wx']},{site['wy']}) {site['water_dist']} tiles away, "
              f"shore {shore}", flush=True)
        return ruin, site
    chk.fail_setup(f"a ruin with a portal-eligible colony site "
                   f"{HOME_MIN_DIST}..{HOME_MAX_DIST} tiles away across walkable "
                   f"ground, with water within {WATER_MAX_DIST} tiles, exists")
    return None


# --------------------------------------------------------------------------
# [setup] the colony
# --------------------------------------------------------------------------
def place_portal(chk: Checks, port: int, gx: int, gy: int) -> int:
    valid = send(port, f"return tostring(building.canPlaceAt('{PORTAL_DEF}',{gx},{gy}))",
                 timeout=20.0)
    if not chk.ok(valid == "true",
                  f"the chosen colony tile ({gx},{gy}) accepts the acolyte portal "
                  f"through the real placement validator (canPlaceAt -> {valid!r})"):
        return -1
    raw = send(port, f"return tostring(building.spawn('{PORTAL_DEF}',{gx},{gy}))",
               timeout=20.0)
    try:
        bid = int(float(raw))
    except (TypeError, ValueError):
        chk.ok(False, f"building.spawn('{PORTAL_DEF}') at ({gx},{gy}) -> {raw!r}")
        return -1
    chk.ok(bid > 0, f"the acolyte portal is placed at ({gx},{gy}) (bid {bid})")
    return bid


def roster(port: int) -> dict[str, list[int]]:
    """Live player-faction units by def, from the AI state table — the
    page-agnostic enumeration surface, keyed by uid."""
    lua = ("local ai=require('scripts.unit_ai'); local out={}; "
           "for uid,_ in pairs(ai.aiState or {}) do "
           "if unit.exists(uid) and unit.getFaction(uid)=='player' then "
           "local i=unit.getInfo(uid); "
           "if i then out[#out+1]=uid..':'..tostring(i.defName) end end end; "
           "return table.concat(out, ',')")
    raw = send(port, lua, timeout=20.0)
    out: dict[str, list[int]] = {}
    for part in raw.split(","):
        if ":" not in part:
            continue
        uid, defname = part.split(":", 1)
        try:
            out.setdefault(defname, []).append(int(uid))
        except ValueError:
            continue
    for v in out.values():
        v.sort()
    return out


def await_roster(chk: Checks, port: int, bid: int):
    """Wait for the portal's OWN spawn sequencer to deliver its roster
    (scripts/building_spawn.lua: five acolytes then the technomule that
    hauls the colony's stock). Nothing here spawns a unit."""
    remaining = _as_float(send(port, f"return building.getSpawnRemaining({bid})"))
    got = poll_until(240.0, lambda: (
        lambda r: r if (len(r.get(ACOLYTE_DEF, [])) >= 5
                        and len(r.get(MULE_DEF, [])) >= 1) else None)(roster(port)),
        interval=2.0)
    if got is None:
        got = roster(port)
        chk.fail_setup(
            f"the portal's own spawn roster delivers five acolytes and the "
            f"technomule (got {[(k, len(v)) for k, v in sorted(got.items())]}, "
            f"spawnRemaining {send(port, f'return building.getSpawnRemaining({bid})')!r}, "
            f"seeded from {remaining})")
        return None
    chk.ok(True, f"the portal spawns its own roster: "
                 f"{len(got[ACOLYTE_DEF])} acolytes + "
                 f"{len(got[MULE_DEF])} technomule (uids "
                 f"{sorted(got[ACOLYTE_DEF])} / {sorted(got[MULE_DEF])})")
    # The standing find_water goal is retired on every acolyte — the
    # same scenario condition every probe in tools/ applies, and applied
    # here to the whole colony so the two travellers are treated alike.
    for uid in got[ACOLYTE_DEF]:
        poll_until(10.0, lambda u=uid: send(
            port, f"local ai=require('scripts.unit_ai'); local s=ai.getState({u}); "
                  f"if not s then return false end; "
                  f"ai.markGoalAccomplished(s,'find_water'); return true") == "true")
    return got


def build_storage(chk: Checks, port: int, hx: int, hy: int) -> int:
    """The colony's storage: a real cargo_hold_S beside the portal.

    Finished immediately on purpose. A cargo_hold_S spawns Appearing
    (build_work 240) and an unfinished building is a CONSTRUCTION SITE:
    build_nearby and deliver_to_build_site (utility 6.0, with a lock-in)
    would pull the whole colony — travellers included — off to finish it.
    Fixture setup, clearly separated: the colony's store is meant to be
    a building that already exists."""
    spot = None
    for dx, dy in ((2, 0), (-2, 0), (0, 2), (0, -2), (2, 2), (-2, -2)):
        x, y = hx + dx, hy + dy
        if send(port, f"return tostring(building.canPlaceAt('{STORAGE_DEF}',{x},{y}))",
                timeout=20.0) == "true":
            spot = (x, y)
            break
    if spot is None:
        chk.fail_setup(f"a tile beside the portal accepts {STORAGE_DEF}")
        return -1
    raw = send(port, f"return building.spawn('{STORAGE_DEF}',{spot[0]},{spot[1]})")
    try:
        bid = int(float(raw))
    except (TypeError, ValueError):
        chk.fail_setup(f"{STORAGE_DEF} spawned beside the portal (got {raw!r})")
        return -1
    required = _as_float(send(port, f"return building.getBuildRequired({bid})")) or 240.0
    send(port, f"building.addBuildProgress({bid}, {required + 1.0}); return 'ok'")
    built = poll_until(30.0, lambda: send(
        port, f"return building.getActivity({bid})") == "built")
    cap = _as_float(send(port, f"return building.getStorageCapacity({bid})"))
    if not chk.ok(bool(built) and (cap or 0) > 0,
                  f"the colony has finished storage at {spot} "
                  f"(bid {bid}, activity "
                  f"{send(port, f'return building.getActivity({bid})')!r}, "
                  f"capacity {cap})"):
        return -1
    return bid


def adjacent_tile(port: int, bid: int):
    """A tile satisfying the storage menu's own adjacency rule
    (Chebyshev <= 1 from the building's footprint)."""
    info = send_json(port, f"return building.getInfo({bid})")
    if not isinstance(info, dict):
        return None, None
    bx, by = int(info.get("gridX", 0)), int(info.get("gridY", 0))
    tw, th = int(info.get("tileW", 1) or 1), int(info.get("tileH", 1) or 1)
    return (bx + tw, by + th - 1), (bx, by, tw, th)


def is_adjacent(pos, foot) -> bool:
    bx, by, tw, th = foot
    ux, uy = int(math.floor(pos[0])), int(math.floor(pos[1]))
    dx = bx - ux if ux < bx else (ux - (bx + tw - 1) if ux >= bx + tw else 0)
    dy = by - uy if uy < by else (uy - (by + th - 1) if uy >= by + th else 0)
    return max(dx, dy) <= 1


# --------------------------------------------------------------------------
# [prepare]
# --------------------------------------------------------------------------
def secure_water(chk: Checks, port: int, scout: int, shore) -> bool:
    """One acolyte walks to the water and finds it with its OWN FOV scan.

    Nothing writes knownWaterSources here: scripts/unit_ai_water.lua's
    scan runs every tick from the unit's real `unit.getVisibleTiles`, so
    the memory (and therefore the tutorial's water objective) is earned
    by the walk."""
    sx, sy = shore
    before = _as_float(send(port, f"local s=require('scripts.unit_ai').getState({scout}); "
                                  f"return s and #(s.knownWaterSources or {{}}) or -1"))
    chk.ok((before or 0) == 0,
           f"the scout starts with no remembered water source (count {before})")
    send(port, f"require('scripts.unit_ai').commandMove({scout},{sx},{sy}); "
               f"return 'ok'")
    found = poll_until(240.0, lambda: (_as_float(send(
        port, f"local s=require('scripts.unit_ai').getState({scout}); "
              f"return s and #(s.knownWaterSources or {{}}) or 0")) or 0) > 0,
        interval=1.0)
    return chk.ok(bool(found),
                  f"the scout reaches the water and registers it through its own "
                  f"FOV scan (at {unit_pos(port, scout)}, target shore {shore}, "
                  f"action {current_action(port, scout)})")


def carried(port: int, uid: int) -> tuple[float, int]:
    """(litres of water, ration count) — measured the way
    scripts/tutorial_eval.lua measures it."""
    raw = send(port,
               f"local l,r=0,0; "
               f"for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
               f"if it.holds=='water' then l=l+(tonumber(it.currentFill) or 0) end; "
               f"if it.defName=='{RATIONS_DEF}' then r=r+1 end end; "
               f"return l..','..r", timeout=20.0)
    try:
        litres, rations = raw.split(",")
        return float(litres), int(float(rations))
    except ValueError:
        return 0.0, 0


def provision(chk: Checks, port: int, mule: int, traveller: int) -> bool:
    """Top the traveller up off the technomule — the colony's supply
    point — through `unit.transferItemToUnit`, the atomic engine path
    the fetch AI itself uses. Bounded by the receiver's real carrying
    capacity, exactly as the Lua callers gate it."""
    before = carried(port, traveller)
    moved = _as_float(send(
        port, f"local n=0; for _=1,2 do "
              f"local w=unit.getCarryingWeight({traveller}) or 0; "
              f"local cap=unit.getStat({traveller},'carrying_capacity'); "
              f"if cap and w>=cap then break end; "
              f"if unit.transferItemToUnit({mule},{traveller},'{RATIONS_DEF}') "
              f"then n=n+1 else break end end; return n", timeout=30.0)) or 0
    after = carried(port, traveller)
    chk.ok(moved > 0 and after[1] > before[1],
           f"the traveller is provisioned off the technomule through the normal "
           f"inventory-transfer surface ({int(moved)}x {RATIONS_DEF} moved; "
           f"rations {before[1]} -> {after[1]})")
    return chk.ok(after[0] >= 2.0 and after[1] >= 1,
                  f"the prepared traveller leaves with real supplies "
                  f"({after[0]:.2f} L of water, {after[1]} rations)")


#: Personal tools an acolyte spawns with, in the order a departing
#: traveller sheds them. Deliberately the reverse of what the expedition
#: needs, and the same order `data/units/acolyte.yaml`'s own
#: `drop_priority` uses for its spawn-time shed.
SHEDDABLE = ("pick_steel", "axe_steel", "shovel_steel")


def load_fraction(port: int, uid: int):
    """Carried weight as a fraction of this unit's carrying capacity."""
    carried = _as_float(send(port, f"return unit.getCarryingWeight({uid})"))
    cap = _as_float(send(port, f"return unit.getStat({uid},'carrying_capacity')"))
    if carried is None or not cap or cap <= 0:
        return None
    return carried / cap


def shed_to_capacity(port: int, uid: int, headroom: float = 0.0) -> int:
    """Shed personal tools until the traveller is inside its carrying
    capacity WITH `headroom` kg to spare. Returns how many items were
    shed.

    The headroom is the extraction target's own weight, and it is not
    optional: `unitAi.commandPickup` refuses at command time when
    `getCarryingWeight + the instance's weight` exceeds capacity (#920),
    so a carrier that merely fits itself can still be turned away at the
    ruin. Observed: a traveller that departed at 91% of capacity had its
    retrieval order refused outright on arrival — the loop's own
    capacity contract working exactly as #920 specifies, against a
    fixture that had not left room for the loot.

    This is what a player does before an expedition, and skipping it is
    a real flake rather than a nicety: `docs/expedition_survival_calibration.md`
    observation E1 recorded a small acolyte walking a whole route at
    121% of capacity, where the encumbrance penalty roughly halved its
    speed and pinned its stamina at ~2/8.5. A traveller that slow makes
    no new closest approach inside `pickup_timeout`, so the stall timer
    correctly retires its order and it reverts to `wander` — observed
    here as an outbound leg that covered 15 tiles in 420 s and never
    reached the ruin. Applied to BOTH travellers, so encumbrance is not
    a second variable alongside supplies."""
    shed = 0
    for defname in SHEDDABLE:
        carried_kg = _as_float(send(port, f"return unit.getCarryingWeight({uid})"))
        cap = _as_float(send(port, f"return unit.getStat({uid},"
                                   f"'carrying_capacity')"))
        if carried_kg is None or not cap or cap <= 0:
            break
        if carried_kg + headroom <= cap:
            break
        # `unit.removeItem` reports success truthily; compare against the
        # falsy spellings rather than assuming a boolean.
        got = send(port, f"return tostring(unit.removeItem({uid},'{defname}'))"
                   ).strip().strip('"')
        if got not in ("nil", "false", ""):
            shed += 1
    return shed


def strip_rations(port: int, uid: int) -> None:
    """The control's ONLY difference: every ration removed. Its canteen
    is deliberately left FULL.

    Emptying the canteen too would look like a stronger control and is
    actually a second variable. A dry canteen puts `refill_canteen`'s
    quadratic urgency ramp at its 7.5 peak, above `follow_command`'s
    7.0, so an unwatered traveller correctly abandons its orders and
    walks to the nearest known water — and the scout's `notify_allies`
    radio broadcast has already told the whole colony where that is.
    Observed exactly that: a control that left the muster and was found
    10.7 tiles off-route beside the lake, 43.6 tiles from the ruin.
    That is the game working, but it is a difference in BEHAVIOUR rather
    than in supplies, and the gated metric is food.

    Leaving both canteens full is therefore the tighter experiment, not
    the softer one: the single difference between the two travellers is
    the thing being measured."""
    send(port, f"while unit.removeItem({uid},'{RATIONS_DEF}') do end; "
               f"return 'ok'", timeout=20.0)


def origin_ok(pos, uids, staging, ruin_xy):
    """(ok, separation, distance-spread, bearing-gap) for a candidate
    departure pair. The shared origin is a PLACE, so separation is the
    binding term: a radial distance band is satisfied anywhere on a
    circle, and two travellers on opposite bearings score a spread of
    0.0 while standing 64 tiles apart."""
    a, b = uids
    if not (pos.get(a) and pos.get(b)):
        return False, -1.0, -1.0, -1.0
    sep = dist(pos[a], pos[b])
    spread = abs(dist(pos[a], ruin_xy) - dist(pos[b], ruin_xy))
    gap = bearing_gap(pos[a], pos[b], ruin_xy)
    ok = (sep <= MAX_START_SEPARATION and spread <= MAX_START_SPREAD
          and all(dist(pos[u], staging) <= STAGING_RADIUS for u in uids))
    return ok, sep, spread, gap


def muster_travellers(port: int, uids, staging, ruin_xy, seconds: float = 420.0):
    """Gather both travellers at ONE staging position and HOLD them there
    with the pause, so the paired legs share an origin — and therefore a
    route, not merely a length.

    Returns (positions, sep, spread, gap) with the session left PAUSED on
    success, or (None, ...) with it left running.

    The hold is `engine.setPaused`, and it has to be. `unit.setFrozen`
    looks like the obvious tool and is the wrong one: `uiFrozen` only
    makes the unit thread's `publishToRender` skip the sim-derived
    update (`src/Unit/Thread.hs`), so the AI and the simulation keep
    moving the unit while `unit.getInfo` keeps reporting the position it
    had when the flag went up. Mustering on those coordinates reads
    stale numbers and then releases two travellers from wherever the sim
    has actually carried them. Pausing stops the simulation itself,
    which is what "hold still" has to mean here.

    A completed PLAYER move order does hold position since #1216
    (SURV-4), which retires observation E3 and makes the muster far more
    likely to converge — but it is not a substitute for the pause, and
    the driven shape below stays. The hold sits at `follow_command`'s own
    utility, so every interrupt that outranked the order still carries a
    held unit off its anchor, and the muster's own history is what a
    coincidence hunt costs when that happens: observed twice, once with
    both units 40+ tiles out and 3.4 tiles apart after a 300 s wait, and
    once with the muster expiring on a pair 10.0 tiles apart.
    Hence the shape below: re-order anyone who has stopped following and
    drifted outside the radius (convergence is driven, not awaited),
    poll for a sample satisfying the origin contract, pause the instant
    it appears, then RE-READ and re-check on the now-stable positions. A
    sample that goes stale in the round trip between the two just
    unpauses and keeps looking."""
    for uid in uids:
        send(port, f"require('scripts.unit_ai').commandMove({uid},"
                   f"{staging[0]},{staging[1]}); return 'ok'")
    deadline = time.time() + seconds
    while time.time() < deadline:
        live = paired_positions(port, uids[0], uids[1])
        # Convergence has to be ACTIVE, not awaited. An arrived unit
        # holds its destination since #1216, but a survival interrupt
        # still carries it off and its return is its own to schedule, so
        # polling for a moment when both happen to be at the tile is a
        # coincidence hunt that can time out — observed: a muster that
        # expired with the two 10.0 tiles apart, one of them 9.4 tiles
        # further from the ruin than the other. Re-order anyone who has
        # stopped following and is outside the radius; that is also
        # exactly what a player does when a colonist wanders off.
        for uid in uids:
            p = live.get(uid)
            if (p is None or dist(p, staging) > STAGING_RADIUS) \
                    and current_action(port, uid) != "follow_command":
                send(port, f"require('scripts.unit_ai').commandMove({uid},"
                           f"{staging[0]},{staging[1]}); return 'ok'")
        if origin_ok(live, uids, staging, ruin_xy)[0]:
            send(port, "engine.setPaused(true); return 'ok'")
            # Re-read while the simulation is stopped: these are the
            # positions the measured leg actually starts from.
            held = {u: unit_pos(port, u) for u in uids}
            ok, sep, spread, gap = origin_ok(held, uids, staging, ruin_xy)
            if ok:
                return held, sep, spread, gap
            send(port, "engine.setPaused(false); return 'ok'")
        time.sleep(0.5)
    live = {u: unit_pos(port, u) for u in uids}
    _ok, sep, spread, gap = origin_ok(live, uids, staging, ruin_xy)
    return None, sep, spread, gap


def seed_departure_deficit(port: int, uids) -> bool:
    """The shared, symmetric fixture: both travellers set out equally
    hungry, as a fraction of their OWN max_hunger (body mass varies
    ~3.6x across acolytes, so an absolute stomach value would not be
    comparable between them).

    Applied while PAUSED. `scripts/unit_ai.lua` is pause-gated, so this
    window changes the one gameplay fact under test without the AI
    reacting to it first — otherwise the provisioned traveller eats
    within a second and the two would no longer be departing from the
    same state.

    Returns True once both units read back the seeded value."""
    for uid in uids:
        maxn = _as_float(send(port, f"return unit.getStat({uid},'max_hunger')"))
        if not maxn:
            return False
        send(port, f"unit.setStat({uid},'hunger',"
                   f"{maxn * DEPART_STOMACH_FRAC:.4f}); return 'ok'")

    def settled():
        for uid in uids:
            frac = stat_fraction(port, uid, "hunger", "max_hunger")
            if frac is None or abs(frac - DEPART_STOMACH_FRAC) > 0.02:
                return None
        return True

    return bool(poll_until(20.0, settled, interval=0.5))


# --------------------------------------------------------------------------
# [travel] / [extract]
# --------------------------------------------------------------------------
def loot_in(port: int, ruin: dict) -> list:
    """The INCIDENTAL ground items lying inside one ruin's own absolute
    bounds (#777) — its `ruin_common` rolls, and nothing else.
    ruin_small declares min_spacing 5 chunks, so no two ruin footprints
    can overlap and the attribution is unambiguous.

    #917's guaranteed significant item is EXCLUDED, by the physical ids
    the instance's own `significant` rows report rather than by def
    name, so the exclusion follows provenance rather than a guess about
    what the reward happens to be. Two things depend on it:

      * the count below is a real check on the loot table. Folding the
        guaranteed item in would let a missing incidental roll hide
        behind a constant.
      * `choose_target` must never pick it. It skips Materials and food
        so the carrier's own AI cannot move the target mid-return — but
        it FALLS BACK to the first entry when both rolls are excluded,
        and `processing_unit` sorts first among this ruin's contents.
        Extracting it there would latch it and clear the ruin long
        before the extract stage asserts it is still untaken."""
    b = ruin.get("bounds") or {}
    if not b:
        return []
    # Read LIVE: `ruin` is the placement-time row from `pick_site`, taken
    # before any content spawned, so its obligations carry no ids yet.
    reserved = {r.get("item_instance_id")
                for r in significant_rows(port, int(ruin["instance_id"]))
                if r.get("item_instance_id") is not None}
    inside = [g for g in ground_items(port)
              if g.get("instanceId") not in reserved
              and b["min_x"] <= g.get("x", 1e9) <= b["max_x"]
              and b["min_y"] <= g.get("y", 1e9) <= b["max_y"]]
    return sorted(inside, key=lambda g: (g.get("defName", ""), g.get("id", 0)))


def def_traits(port: int, def_name: str) -> tuple[str, bool]:
    """(category, edible) for one item def.

    Edibility is asked of `item.getFood` — the SAME predicate
    scripts/unit_ai_needs.lua's `isFoodDef` uses — rather than of a
    field on `item.listDefs()`, which carries only name / displayName /
    category / weight."""
    lua = (f"local cat='?'; "
           f"for _,x in ipairs(item.listDefs() or {{}}) do "
           f"if x.name=='{def_name}' then cat=tostring(x.category) end end; "
           f"local f=item.getFood and item.getFood('{def_name}'); "
           f"local edible=(f~=nil) and (((f.calories or 0)>0) "
           f"or ((f.caloriesPerKg or 0)>0)); "
           f"return cat..'/'..tostring(edible)")
    raw = send(port, lua, timeout=20.0)
    cat, _, edible = raw.partition("/")
    return cat, edible == "true"


def choose_target(port: int, loot: list):
    """Which of the ruin's own rolls is carried home.

    The ruin guarantees no particular item (#921), so the target is
    whatever it rolled — but the CHOICE between its rolls is
    deterministic, and skips two def kinds whose own AI would move the
    item before the player's order does:
      * Materials — `store_materials` fires on any Materials in
        inventory (utility 3.0, and the colony's cargo is right there),
        so the carrier would bank it autonomously mid-return;
      * food — `eat_from_inventory` would consume it outright.
    Falls back to the first roll if both are excluded: the loop still
    runs, and the fingerprint records what was chosen."""
    for g in loot:
        cat, edible = def_traits(port, g.get("defName", ""))
        if cat != "Materials" and not edible:
            return g
    return loot[0] if loot else None


def significant_rows(port: int, instance_id: int) -> list:
    """#917's guaranteed significant obligations for one placed ruin, in
    slot order — read straight from the engine's own reported field, not
    re-derived here."""
    inst = instance_by_id(port, PAGE, instance_id)
    rows = (inst or {}).get("significant") or []
    return sorted(rows, key=lambda r: r.get("slot", 0))


def clearance_events(port: int) -> list:
    """Every player-facing clearance notice on the log so far. #917
    promises exactly ONE per location, so the interesting assertion is
    always a count."""
    return [e for e in event_log(port)
            if e.get("category") == "location_clearance"]


def walk_until_adjacent(port: int, uid: int, foot, seconds: float,
                        samples: list):
    """Poll a walking unit until it stands adjacent to `foot`, recording
    every position sample on the way. Returns True if it arrived."""
    deadline = time.time() + seconds
    while time.time() < deadline:
        p = unit_pos(port, uid)
        if p:
            samples.append(p)
            if is_adjacent(p, foot):
                return True
        time.sleep(1.0)
    return False


def assert_real_travel(chk: Checks, samples: list, goal, label: str,
                       min_samples: int, min_closed: float) -> None:
    """The journey happened over many ticks and closed on its
    destination — not a teleport, not a stalled unit."""
    if not samples:
        chk.ok(False, f"{label} produced no position samples")
        return
    steps = [dist(samples[i], samples[i + 1]) for i in range(len(samples) - 1)]
    biggest = max(steps) if steps else 0.0
    chk.ok(len(samples) >= min_samples and 0.05 < biggest <= MAX_STEP_TILES,
           f"{label} is real multi-tick travel — moving, and not a teleport "
           f"({len(samples)} samples, largest single step {biggest:.2f} tiles)")
    chk.ok(dist(samples[-1], goal) < dist(samples[0], goal) - min_closed,
           f"{label} closed on its destination "
           f"({dist(samples[0], goal):.1f} -> {dist(samples[-1], goal):.1f} tiles)")


def arrival_box(ruin: dict):
    """The region this probe counts as "at the ruin", as inclusive
    absolute tile bounds: its stored bounds (#777) grown by
    MAX_SIGHT_TILES.

    #1230 removed the discovery_margin this used to expand by. The
    replacement is deliberately the SIGHT bound rather than a rewritten
    constant, because that is what the measurement below needs: the
    unprepared-control comparison is taken once BOTH travellers are at
    the ruin, and a traveller is only meaningfully "there" once it is
    close enough to have revealed it. A box smaller than the sight
    radius would let a unit reveal the ruin from outside the box the
    probe waits on, and the paired snapshot would never be taken."""
    b = ruin.get("bounds") or {}
    m = MAX_SIGHT_TILES
    return (int(b["min_x"]) - m, int(b["min_y"]) - m,
            int(b["max_x"]) + m, int(b["max_y"]) + m)


def in_arrival_box(pos, box) -> bool:
    x0, y0, x1, y1 = box
    return x0 <= pos[0] <= x1 and y0 <= pos[1] <= y1


def find_instance_by_def(items: list, def_name: str, exclude: set):
    for it in items:
        if it.get("defName") == def_name and it.get("instanceId") not in exclude:
            return it
    return None


# --------------------------------------------------------------------------
# main
# --------------------------------------------------------------------------
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

    fingerprint: dict = {"seed": args.seed, "size": args.size,
                         "plates": args.plates}
    port = args.port
    prepared = control = mule = scout = -1
    storage_bid = -1
    recovered = None
    ruin = None
    home = None

    try:
        # ============ engine A: the whole expedition ===================
        proc = boot_probe(root, port, LOG_A, "engine A")
        try:
            bootstrap(port)
            send(port, f"world.init('{PAGE}', {args.seed}, {args.size}, "
                       f"{args.plates}, 'First Expedition'); return 'ok'")
            send(port, "return world.waitForInit(400)", timeout=420.0)
            send(port, f"world.show('{PAGE}'); return 'ok'")

            # ---------------------------------------------------- setup
            chk.enter("setup", "a real world, a real ruin, and a real colony")
            picked = pick_site(chk, port)
            if not picked:
                return 2
            ruin, site = picked
            ruin_id = int(ruin["instance_id"])
            ruin_xy = (float(ruin["gx"]), float(ruin["gy"]))
            home = (int(site["x"]), int(site["y"]))
            fingerprint.update(ruin_instance=ruin_id,
                               ruin_anchor=[int(ruin["gx"]), int(ruin["gy"])],
                               colony=list(home),
                               water=[site["wx"], site["wy"]])

            inst = instance_by_id(port, PAGE, ruin_id)
            chk.ok(isinstance(inst, dict) and inst.get("lifecycle") == "unknown",
                   f"the ruin starts undiscovered — lifecycle "
                   f"{(inst or {}).get('lifecycle')!r} (nothing has approached it)")

            portal_bid = place_portal(chk, port, home[0], home[1])
            if portal_bid < 0:
                return 2
            party = await_roster(chk, port, portal_bid)
            if not party:
                return 2
            acolytes = party[ACOLYTE_DEF]
            mule = party[MULE_DEF][0]
            # Deterministic role assignment by uid order: the portal
            # spawns its roster in a fixed sequence, so these are stable.
            scout, prepared, control = acolytes[0], acolytes[1], acolytes[2]
            # The remaining acolytes are the never-went-there control for
            # the per-unit KNOWLEDGE layer (#915). They are NOT held:
            # `unit.setFrozen` freezes only the render publish, so a
            # "held" colonist keeps walking under the sim while reporting
            # the position it had when the flag went up — the check would
            # be reading a stale coordinate. Instead the assertion below
            # is made eligibility-based: a colonist qualifies only if it
            # was never observed at the ruin during the leg and is
            # outside it when the check runs. A colonist that genuinely
            # wanders to the ruin has genuinely learned it, and is
            # excluded rather than counted as a leak.
            stay_home = [u for u in acolytes
                         if u not in (scout, prepared, control)]
            print(f"  scout {scout}, prepared traveller {prepared}, "
                  f"unprepared control {control}, technomule {mule}, "
                  f"stay-at-home colonists {stay_home}", flush=True)

            storage_bid = build_storage(chk, port, home[0], home[1])
            if storage_bid < 0:
                return 2
            deposit_spot, foot = adjacent_tile(port, storage_bid)

            loot = loot_in(port, ruin)
            # EXACTLY the two `ruin_common` rolls
            # (data/locations/ruin_small.yaml). Exact, not a floor:
            # `loot_in` now excludes #917's guaranteed item by its own
            # physical id, so a third entry here would mean an
            # unaccounted-for ground item and a missing one would mean a
            # lost roll — neither of which a `>=` could see.
            chk.ok(len(loot) == 2,
                   f"the ruin spawned its own two loot-table rolls: "
                   f"{[g['defName'] for g in loot]} "
                   f"({len(loot)} incidental ground items inside its bounds, "
                   f"the guaranteed item excluded)")
            if len(loot) != 2:
                return 2
            target = choose_target(port, loot)
            fingerprint.update(loot=sorted(g["defName"] for g in loot),
                               target=target["defName"],
                               target_gid=int(target["id"]))
            print(f"  extraction target: {target['defName']} "
                  f"(ground id {target['id']}) at "
                  f"({target['x']:.0f},{target['y']:.0f})", flush=True)

            # -------------------------------------------------- prepare
            chk.enter("prepare", "secure water, provision the party, "
                                 "read the objective state")
            if not secure_water(chk, port, scout, site["shore"]):
                return 2
            if not provision(chk, port, mule, prepared):
                return 2
            strip_rations(port, control)
            c_water, c_rations = carried(port, control)
            p_water, p_rations = carried(port, prepared)
            chk.ok(c_rations == 0 and p_rations > 0
                   and abs(c_water - p_water) < 0.01,
                   f"the control party leaves with NO FOOD and the SAME water "
                   f"as the prepared one — food is the single difference "
                   f"between the two travellers (prepared {p_water:.2f} L / "
                   f"{p_rations} rations, control {c_water:.2f} L / "
                   f"{c_rations} rations)")
            # Headroom for the loot: commandPickup refuses at command
            # time when the instance would not fit (#920), so a carrier
            # that merely fits ITSELF is turned away at the ruin.
            headroom = float(target.get("weight") or 0.0)
            shed = {u: shed_to_capacity(port, u, headroom)
                    for u in (prepared, control)}
            room = {}
            for u in (prepared, control):
                carried_kg = _as_float(send(
                    port, f"return unit.getCarryingWeight({u})")) or 0.0
                cap = _as_float(send(
                    port, f"return unit.getStat({u},'carrying_capacity')")) or 0.0
                room[u] = (carried_kg, cap)
            chk.ok(all(room[u][0] + headroom <= room[u][1]
                       for u in (prepared, control)),
                   f"both travellers set out INSIDE their carrying capacity with "
                   f"room for the {target['defName']} ({headroom:.2f} kg) — an "
                   f"over-encumbered acolyte walks at a fraction of comfort "
                   f"speed and has its orders stall-timed-out, and one with no "
                   f"headroom has its retrieval order refused outright (prepared "
                   f"{room[prepared][0]:.1f}+{headroom:.2f} of "
                   f"{room[prepared][1]:.1f} kg after shedding {shed[prepared]} "
                   f"tool(s), control {room[control][0]:.1f}+{headroom:.2f} of "
                   f"{room[control][1]:.1f} kg after {shed[control]})")

            completed, checked = poll_until(
                45.0, lambda: (lambda p: p if EXPECTED_COMPLETED <= p[0] else None)(
                    progress(port)), interval=1.0) or progress(port)
            chk.ok(completed == EXPECTED_COMPLETED,
                   f"the shipped first_session tree stands at exactly its expected "
                   f"completed set {sorted(EXPECTED_COMPLETED)} (got "
                   f"{sorted(completed)})")
            chk.ok({SUB_WATER, SUB_FOOD} <= checked,
                   f"both live preparation subobjectives are checked while a "
                   f"provisioned traveller is standing in the colony "
                   f"({sorted(checked)})")
            fingerprint["objectives"] = sorted(completed)

            # --------------------------------------------------- travel
            chk.enter("travel", "both travellers walk the same route; "
                                "the ruin is discovered by sight")
            # Muster both travellers at a common departure point first.
            # A shared DESTINATION is not a shared journey: hunger drains
            # with time on the road, so two travellers setting out from
            # different distances would be running different-length legs
            # (36.4 vs 31.5 tiles, in an earlier run of this probe) and
            # the difference would land in the measured delta alongside
            # supplies.
            # Leaves the session PAUSED on success, holding the verified
            # departure positions still: everything from here to the
            # paired orders happens inside that one window, so nothing
            # moves between the check and the departure.
            staged, sep, spread, bearing = muster_travellers(
                port, (prepared, control), deposit_spot, ruin_xy)
            at_start = staged or {u: unit_pos(port, u)
                                  for u in (prepared, control)}
            if not chk.ok(staged is not None,
                          f"both travellers depart from the SAME PLACE, not just "
                          f"the same distance — so they walk one route, and "
                          f"route shape is time on the road, which is hunger "
                          f"(prepared at {at_start[prepared]}, control at "
                          f"{at_start[control]}; {sep:.2f} tiles apart, bar "
                          f"{MAX_START_SEPARATION}; "
                          f"{dist(at_start[prepared], ruin_xy):.1f} vs "
                          f"{dist(at_start[control], ruin_xy):.1f} tiles out, "
                          f"spread {spread:.2f}, bar {MAX_START_SPREAD}; "
                          f"bearings to the ruin {bearing:.1f} deg apart; "
                          f"verified with the simulation stopped)"):
                send(port, "engine.setPaused(false); return 'ok'")
                return 2

            # Already paused by the muster. Seed the shared deficit and
            # issue both orders inside that same window, so the two
            # travellers genuinely leave from the same place, in the same
            # state, under the same command.
            seeded = seed_departure_deficit(port, (prepared, control))
            depart = {u: vitals(port, u) for u in (prepared, control)}
            # Rations in the pack at departure. Consumption is the
            # DURABLE record that a meal happened: `unit.feed` removes a
            # discrete ration outright, so a count that has gone down
            # cannot be missed by a sampling loop the way the
            # `eat_from_inventory` action itself can.
            depart_food = {u: carried(port, u)[1] for u in (prepared, control)}
            print(f"  departure  prepared {prepared}: "
                  f"{fmt_vitals(depart[prepared])}, "
                  f"{dist(at_start[prepared], ruin_xy):.1f} tiles to go",
                  flush=True)
            print(f"  departure  control  {control}: "
                  f"{fmt_vitals(depart[control])}, "
                  f"{dist(at_start[control], ruin_xy):.1f} tiles to go",
                  flush=True)
            if not chk.ok(seeded and all(
                    depart[u]["pose"] not in ("collapsed", "dead")
                    for u in (prepared, control)),
                    f"precondition: both travellers set out on their feet from "
                    f"the same seeded hunger (seeded={seeded}; prepared "
                    f"{depart[prepared]['pose']!r}, control "
                    f"{depart[control]['pose']!r})"):
                return 2

            already = {it["instanceId"] for it in inventory(port, prepared)
                       if it.get("defName") == target["defName"]}
            # ONE identical leg for both: the same verb, to the same
            # tile, issued in the same paused window.
            #
            # The verb has to match, not just the destination.
            # `commandMove` walks at `movement_speed.ordered` while
            # `pickup_ground` walks at `comfort`, and ordered is
            # comfort * 1.15 — so ordering one traveller to fetch and
            # the other to walk would put a 15% speed difference inside
            # the comparison. The prepared traveller's retrieval order
            # is therefore issued LATER, in the extract stage, once the
            # control measurement has already been taken.
            #
            # The control is given no retrieval target at all: handing
            # it the ruin's other loot roll would put the loot TABLE
            # inside the experiment, since a ruin can roll food and a
            # control that eats what it finds destroys the measurement.
            tx, ty = int(ruin["gx"]), int(ruin["gy"])
            for uid in (prepared, control):
                send(port, f"require('scripts.unit_ai').commandMove({uid},"
                           f"{tx},{ty}); return 'ok'")
            tasks = {}
            for uid in (prepared, control):
                tasks[uid] = send(
                    port, f"local s=require('scripts.unit_ai').getState({uid}); "
                          f"local t=s and s.commandedTask; "
                          f"return t and (math.floor(t.x)..','..math.floor(t.y)) "
                          f"or 'none'").strip().strip('"')
            chk.ok(all(tasks[u] == f"{tx},{ty}" for u in (prepared, control)),
                   f"both travellers leave under the IDENTICAL pending move "
                   f"order to the same tile ({tx},{ty}) — prepared "
                   f"{tasks[prepared]!r}, control {tasks[control]!r}")
            send(port, "engine.setPaused(false); return 'ok'")

            box = arrival_box(ruin)
            p_samples: list = []
            c_samples: list = []
            arrived_at: dict[int, float] = {}
            # Watch each traveller's CHOSEN ACTION, so the control-stage
            # delta can be attributed to a mechanism that was observed
            # running rather than inferred from the number it left
            # behind.
            ate: dict[int, bool] = {prepared: False, control: False}
            # Any stay-at-home colonist seen at the ruin during the
            # leg has been there, and is therefore not evidence about
            # units that have NOT been there.
            visited_ruin: set[int] = set()
            start = time.time()
            deadline = start + 480.0
            together = None
            arrive = None
            arrive_food = None
            while time.time() < deadline:
                # ONE round trip for both, so the simultaneity test below
                # is over coordinates from a single read.
                live = paired_positions(port, prepared, control)
                for uid, samples in ((prepared, p_samples), (control, c_samples)):
                    p = live[uid]
                    if p:
                        samples.append(p)
                        if uid not in arrived_at and in_arrival_box(p, box):
                            arrived_at[uid] = time.time() - start
                    # Corroboration only — see `depart_food`. A meal is
                    # over in a tick or two and this poll runs about once
                    # a second, so catching the action is luck; catching
                    # the missing ration is not.
                    if (current_action(port, uid) == "eat_from_inventory"
                            or send(port, f"return unit.getActivity({uid})")
                            == "eating"):
                        ate[uid] = True
                for uid in stay_home:
                    q = unit_pos(port, uid)
                    if q and in_arrival_box(q, box):
                        visited_ruin.add(uid)
                # The shared observation point is both travellers inside
                # the ruin IN THE SAME SAMPLE — not "each has been there
                # at some point". The two arrive at different times, and
                # although an arrived unit holds its destination (#1216)
                # its own physiology can still carry it back out while
                # the other is still walking; latching first-entry would
                # score that as a shared observation point.
                if all(live[u] and in_arrival_box(live[u], box)
                       for u in (prepared, control)):
                    # A candidate. STOP the simulation and revalidate,
                    # so the snapshot the control is scored from is
                    # coherent rather than merely closely-spaced: the
                    # metrics below are several console round trips, and
                    # a running sim would let the pair drift apart
                    # between them.
                    send(port, "engine.setPaused(true); return 'ok'")
                    held = paired_positions(port, prepared, control)
                    if all(held[u] and in_arrival_box(held[u], box)
                           for u in (prepared, control)):
                        together = held
                        arrive = {u: vitals(port, u)
                                  for u in (prepared, control)}
                        arrive_food = {u: carried(port, u)[1]
                                       for u in (prepared, control)}
                        send(port, "engine.setPaused(false); return 'ok'")
                        break
                    send(port, "engine.setPaused(false); return 'ok'")
                time.sleep(1.0)

            chk.ok(together is not None,
                   f"BOTH travellers are at the ruin {box} "
                   f"in ONE COHERENT SNAPSHOT — a single paired read, "
                   f"revalidated with the simulation STOPPED, and the control's "
                   f"metrics taken from that same stopped window — so it is "
                   f"measured where the prepared one is, not part-way behind it "
                   f"and not from two positions sampled moments apart "
                   f"(snapshot {together}; first entered after "
                   f"{arrived_at.get(prepared, -1):.0f}s / "
                   f"{arrived_at.get(control, -1):.0f}s)")
            if arrive is None:
                arrive = {u: vitals(port, u) for u in (prepared, control)}
            if arrive_food is None:
                arrive_food = {u: carried(port, u)[1]
                               for u in (prepared, control)}
            for uid, samples, label in ((prepared, p_samples, "prepared"),
                                        (control, c_samples, "control")):
                assert_real_travel(chk, samples, ruin_xy,
                                   f"the {label} traveller's outbound leg",
                                   min_samples=10, min_closed=10.0)
            if together:
                print(f"  observation point (one shared sample): prepared "
                      f"{dist(together[prepared], ruin_xy):.1f} tiles from the "
                      f"ruin anchor, control "
                      f"{dist(together[control], ruin_xy):.1f} tiles", flush=True)

            # -- the control observation point: one identical leg from a
            #    common departure point, both at the ruin in the same
            #    sample, only the packs differ. `arrive` was captured at
            #    that sample and is deliberately NOT re-read here.
            print(f"  arrival    prepared {prepared}: "
                  f"{fmt_vitals(arrive[prepared])}", flush=True)
            print(f"  arrival    control  {control}: "
                  f"{fmt_vitals(arrive[control])}", flush=True)
            # #999: both travellers arriving collapsed used to be silently
            # tolerated here — the pose was recorded above but only ever
            # printed, so the location/travel checks below could still
            # pass around it. A traveller should reach the ruin on
            # its feet; a real ordinary leg collapsing is exactly the
            # run/faint/run bug this gate now has to catch.
            chk.ok(all(arrive[u]["pose"] not in ("collapsed", "dead")
                       for u in (prepared, control)),
                   f"both travellers are standing at the arrival snapshot, "
                   f"not collapsed/dead from the ordinary leg (prepared "
                   f"{arrive[prepared]['pose']!r}, control "
                   f"{arrive[control]['pose']!r})")

            # -- discovery: lifecycle, player event, per-unit knowledge
            inst = poll_until(60.0, lambda: (
                lambda i: i if isinstance(i, dict)
                and i.get("lifecycle") in ("active", "cleared") else None)(
                    instance_by_id(port, PAGE, ruin_id)), interval=1.0)
            chk.ok(inst is not None,
                   f"approaching the ruin promotes its encounter lifecycle to "
                   f"'active' or 'cleared' "
                   f"({(instance_by_id(port, PAGE, ruin_id) or {}).get('lifecycle')!r})")
            # The WHOLE log, deliberately not a slice from a mark taken
            # before departure. `Engine.PlayerEvent.Emit.pushBounded`
            # keeps a bounded ring buffer and drops the oldest rows past
            # `eventStoreCap`, so an index captured earlier does not
            # survive a busy session: once the buffer saturates, the
            # slice silently skips real entries, and if the mark is past
            # the new length it yields nothing at all (observed — a run
            # whose lifecycle had demonstrably reached its visible encounter state
            # reported no event). Scanning the whole log is also exactly
            # as strict: the promotion is one-way, so an instance can
            # emit its discovery event only once per session.
            label = (ruin.get("name") or "").strip()
            hits = [e for e in event_log(port)
                    if e.get("category") == "location_discovery"
                    and label and label in (e.get("text") or "")]
            chk.ok(len(hits) == 1,
                   f"discovery emits exactly one player-facing event naming the "
                   f"location ({[h.get('text') for h in hits]})")
            key = f"{PAGE}#{ruin_id}"
            knew = poll_until(30.0, lambda: key in known_locations(port, prepared),
                              interval=1.0)
            chk.ok(bool(knew),
                   f"the traveller that walked there personally KNOWS the location "
                   f"({key} in {sorted(known_locations(port, prepared))})")
            # The premise is asserted, not assumed: each held colonist
            # must genuinely still be away from the ruin, and its position
            # and memory are both reported so a failure says which.
            home_state = []
            never_went = []
            for u in stay_home:
                p = unit_pos(port, u)
                known = known_locations(port, u)
                inside = bool(p) and in_arrival_box(p, box)
                been = u in visited_ruin or inside
                home_state.append(
                    f"uid {u} at {p} "
                    f"({'HAS BEEN at' if been else 'never reached'} the ruin) "
                    f"knows {sorted(known) or 'nothing'}")
                if not been:
                    never_went.append((u, known))
            chk.ok(bool(never_went),
                   f"precondition: at least one colonist never entered the "
                   f"ruin, so there is something to test the knowledge "
                   f"layer against ({'; '.join(home_state) or 'none'})")
            chk.ok(bool(never_went) and all(key not in k for _u, k in never_went),
                   f"the colonists who never went learned nothing — per-unit "
                   f"knowledge is experiential, not broadcast "
                   f"({'; '.join(home_state)})")

            # -------------------------------------------------- extract
            chk.enter("extract", "recover the ruin's own loot-table output")
            # Issued only now, so the shared travel leg above was the
            # same verb at the same speed for both travellers. The
            # carrier is already standing at the ruin; this is
            # the "Pick up" the player clicks once the party has
            # arrived.
            acc_p = send(port, f"return require('scripts.unit_ai').commandPickup("
                               f"{prepared},{int(target['id'])})")
            chk.ok(acc_p.strip() == "true",
                   f"the retrieval order is accepted at the ruin "
                   f"(commandPickup -> {acc_p!r})")
            saw_pickup = False
            picked = None
            deadline = time.time() + 180.0
            while time.time() < deadline:
                if current_action(port, prepared) == "pickup_ground":
                    saw_pickup = True
                picked = find_instance_by_def(inventory(port, prepared),
                                              target["defName"], already)
                if picked:
                    break
                time.sleep(1.0)
            chk.ok(saw_pickup,
                   f"the carrier acts on the order through the real "
                   f"pickup_ground AI action (last action "
                   f"{current_action(port, prepared)})")
            if not chk.ok(picked is not None,
                          f"the carrier picks up the {target['defName']} the ruin "
                          f"itself rolled (action "
                          f"{current_action(port, prepared)}, pose "
                          f"{pose(port, prepared)})"):
                return 2
            recovered = picked
            instance_id = recovered["instanceId"]
            chk.ok(not any(g.get("id") == int(target["id"])
                           for g in ground_items(port)),
                   f"the ruin's ground item (gid {target['id']}) is gone from the "
                   f"world — it MOVED into the carrier, it was not copied")
            name = send(port, f"local i=unit.getInfo({prepared}); "
                              f"return i and i.name or ''")
            disp = recovered.get("displayName") or target["defName"]
            hits = [e for e in event_log(port)
                    if e.get("category") == "unit_event"
                    and e.get("uid") == prepared
                    and disp in (e.get("text") or "")
                    and name and name in (e.get("text") or "")]
            chk.ok(bool(hits),
                   f"the recovery is reported on a player-facing surface naming "
                   f"the item and its carrier: "
                   f"{hits[-1]['text'] if hits else '(no event)'}")
            print(f"  recovered instance {instance_id}: {properties(recovered)}",
                  flush=True)
            fingerprint["recovered_def"] = recovered.get("defName")

            # --- #917: the ruin's GUARANTEED significant item, which is
            # what its cleared state actually waits on. Recovered by the
            # same carrier on the same trip, through the same player
            # gesture, right after the incidental target above — so the
            # survival control's measured journey is untouched and this
            # is purely additive.
            #
            # This ruin is the zero-occupant one the scenario selects,
            # so its ENCOUNTER half has been satisfied since placement.
            # That makes it the sharpest possible test of the
            # conjunction: nothing but this pickup can be what clears
            # it.
            sig_before = significant_rows(port, ruin_id)
            chk.ok(len(sig_before) == 1
                   and sig_before[0].get("item_instance_id") is not None
                   and not sig_before[0].get("taken"),
                   f"the ruin owes exactly one guaranteed significant item, "
                   f"spawned and not yet taken ({sig_before})")
            pre_clear = len(clearance_events(port))
            inst_pre = instance_by_id(port, PAGE, ruin_id)
            chk.ok(isinstance(inst_pre, dict)
                   and inst_pre.get("authors_clearance") is True
                   and inst_pre.get("clearance_satisfied") is False
                   and inst_pre.get("lifecycle") != "cleared",
                   f"and the ruin is NOT cleared while it is still lying there, "
                   f"even though its zero-nomad encounter half is already "
                   f"complete ({(inst_pre or {}).get('lifecycle')!r}, "
                   f"satisfied={(inst_pre or {}).get('clearance_satisfied')!r})")

            sig_phys = (sig_before[0].get("item_instance_id")
                        if sig_before else None)
            sig_gid = next((int(g["id"]) for g in ground_items(port)
                            if int(g.get("instanceId", -1)) == sig_phys), None)
            if not chk.ok(sig_gid is not None,
                          f"the guaranteed item is on the ground under a "
                          f"resolvable ground id (physical {sig_phys})"):
                return 2
            acc_s = send(port, f"return require('scripts.unit_ai').commandPickup("
                               f"{prepared},{sig_gid})")
            chk.ok(acc_s.strip() == "true",
                   f"the retrieval order for it is accepted (commandPickup -> "
                   f"{acc_s!r})")
            sig_item = poll_until(
                180.0,
                lambda: find_instance(inventory(port, prepared), sig_phys),
                interval=1.0)
            if not chk.ok(sig_item is not None,
                          f"the carrier picks up the guaranteed item (action "
                          f"{current_action(port, prepared)}, pose "
                          f"{pose(port, prepared)})"):
                return 2
            sig_after = poll_until(
                60.0,
                lambda: (significant_rows(port, ruin_id)
                         if all(r.get("taken")
                                for r in significant_rows(port, ruin_id))
                         else None),
                interval=1.0)
            chk.ok(sig_after is not None
                   and sig_after[0].get("item_instance_id") == sig_phys,
                   f"the pickup latches THAT physical item as taken, keeping its "
                   f"provenance ({sig_after})")
            cleared_inst = poll_until(
                60.0,
                lambda: (lambda i: i if isinstance(i, dict)
                         and i.get("lifecycle") == "cleared" else None)(
                             instance_by_id(port, PAGE, ruin_id)),
                interval=1.0)
            chk.ok(cleared_inst is not None
                   and cleared_inst.get("clearance_satisfied") is True,
                   f"and THAT is what clears the ruin — the last outstanding "
                   f"condition ({(cleared_inst or {}).get('lifecycle')!r})")
            clear_evs = clearance_events(port)
            chk.ok(len(clear_evs) == pre_clear + 1,
                   f"exactly one clearance notice is emitted, not zero and not "
                   f"two ({len(clear_evs) - pre_clear} new: "
                   f"{[e.get('text') for e in clear_evs[pre_clear:]]})")
            fingerprint.update(significant_def=sig_after[0].get("item")
                               if sig_after else None,
                               significant_instance=sig_phys)

            # --------------------------------------------------- return
            chk.enter("return", "walk home and bank it in colony storage")
            send(port, f"require('scripts.unit_ai').commandMove({prepared},"
                       f"{deposit_spot[0]},{deposit_spot[1]}); return 'ok'")
            r_samples: list = []
            arrived = walk_until_adjacent(port, prepared, foot, 420.0, r_samples)
            chk.ok(bool(arrived),
                   f"the carrier walks the whole way home and arrives adjacent to "
                   f"colony storage (at {unit_pos(port, prepared)}, footprint "
                   f"{foot}, action {current_action(port, prepared)}, "
                   f"{fmt_vitals(vitals(port, prepared))})")
            assert_real_travel(chk, r_samples, deposit_spot, "the return leg",
                               min_samples=10, min_closed=10.0)
            chk.ok(find_instance(inventory(port, prepared), instance_id) is not None,
                   "the recovered item is still carried at the end of the return leg")

            # A lax AI verb (D-7) with no adjacency gate of its own, so
            # the adjacency asserted beside it is this probe's own rule.
            # It used to be the call the "Store in <cargo>" menu entry
            # made; #1249 retired that entry for a queued order, and this
            # step stays direct so "invest" does not wait on the transfer
            # executor's own timing.
            at_deposit = unit_pos(port, prepared)
            adj = bool(at_deposit) and is_adjacent(at_deposit, foot)
            ok = send(port, f"return unit.depositToCargo({prepared},{storage_bid},"
                            f"'{recovered['defName']}',{instance_id})")
            chk.ok(adj and ok.strip() == "true",
                   f"the carrier banks it in colony storage from an adjacent tile "
                   f"(adjacent={adj} at {at_deposit}, returned {ok!r})")
            stored = send_json(port, f"return building.getStorage({storage_bid})")
            chk.ok(find_instance(stored if isinstance(stored, list) else [],
                                 instance_id) is not None,
                   f"the exact recovered instance is in colony storage "
                   f"(bid {storage_bid})")

            # #917: the guaranteed item makes the same trip. It may
            # already have been banked autonomously — `processing_unit`
            # is a Materials def, and `store_materials` fires on any
            # Materials in inventory with the colony's cargo right there
            # — so the deposit is issued only if it is still carried,
            # and the assertion is on the OUTCOME either way: that exact
            # physical instance ends up in colony storage.
            if find_instance(inventory(port, prepared), sig_phys) is not None:
                send(port, f"return unit.depositToCargo({prepared},"
                           f"{storage_bid},'{sig_item['defName']}',{sig_phys})")
            banked = poll_until(
                60.0,
                lambda: find_instance(
                    (lambda v: v if isinstance(v, list) else [])(
                        send_json(port,
                                  f"return building.getStorage({storage_bid})")),
                    sig_phys),
                interval=1.0)
            chk.ok(banked is not None,
                   f"the guaranteed item is banked in colony storage as that "
                   f"exact physical instance ({sig_phys})")
            # Taking it out of the ruin and moving it around cannot undo
            # the latch: the ruin was looted, and that does not become
            # untrue.
            chk.ok(all(r.get("taken") for r in significant_rows(port, ruin_id)),
                   f"and the taken latch is unmoved by the return, the deposit "
                   f"and every transfer in between "
                   f"({significant_rows(port, ruin_id)})")

            # ----------------------------------------------------- save
            chk.enter("save", "capture the finished expedition")
            saved = send(port, f"return engine.saveWorld('{PAGE}', '{SLOT}')")
            chk.ok(saved.strip() == "true", f"engine.saveWorld accepted ({saved!r})")
            rid = capture_request_id(port, "return engine.getSaveStatus()")
            done, status = wait_save_complete(port, rid)
            chk.ok(done, f"save {rid} reached SaveCaptureComplete ({status})")
            check_ai_tick_clean(chk, LOG_A, "engine A")

            # -------------------------------------------------- control
            chk.enter("control", "the unprepared party is measurably worse off")
            dp, dc = depart[prepared], depart[control]
            ap_, ac = arrive[prepared], arrive[control]
            chk.ok(all(v is not None for v in (dp["stomach"], dc["stomach"],
                                               ap_["stomach"], ac["stomach"])),
                   "both travellers reported a stomach fraction at both "
                   "observation points")
            chk.ok(abs(num(dp["stomach"]) - num(dc["stomach"])) < 0.02,
                   f"the two travellers set out from the SAME deficit "
                   f"(stomach {num(dp['stomach']):.3f} vs "
                   f"{num(dc['stomach']):.3f}) — the packs are the only "
                   f"difference between them")
            s_delta = num(ap_["stomach"]) - num(ac["stomach"])
            # The MECHANISM, evidenced by what it CONSUMED — not inferred
            # from the numbers below, which two differently-massed
            # acolytes could in principle reach by other means.
            #
            # Rations eaten, rather than the eat_from_inventory action
            # being caught in the act: `eatExecute` finishes a whole meal
            # inside one AI tick, so at a ~1s poll the action is a
            # coin-flip to observe (a run where the stomach demonstrably
            # went 0.20 -> 0.82 recorded ate=False). `unit.feed` removes
            # a discrete ration outright, so the pack is a durable
            # record. The action sighting is still reported, as
            # corroboration.
            eaten = depart_food[prepared] - arrive_food[prepared]
            chk.ok(eaten > 0
                   and depart_food[control] == 0 and arrive_food[control] == 0,
                   f"the delta comes from EATING: the provisioned traveller "
                   f"consumed {eaten} ration(s) en route "
                   f"({depart_food[prepared]} -> {arrive_food[prepared]}) while "
                   f"the control had none to consume and consumed nothing "
                   f"({depart_food[control]} -> {arrive_food[control]}); "
                   f"eat_from_inventory/eating also seen live: "
                   f"prepared={ate[prepared]}, control={ate[control]}")
            chk.ok(num(ap_["stomach"]) > num(dp["stomach"])
                   and num(ac["stomach"]) <= num(dc["stomach"]),
                   f"only the traveller carrying food ate: prepared "
                   f"{num(dp['stomach']):.3f} -> {num(ap_['stomach']):.3f}, "
                   f"control {num(dc['stomach']):.3f} -> "
                   f"{num(ac['stomach']):.3f}")
            chk.ok(s_delta >= MIN_STOMACH_DELTA,
                   f"at the same point in the same journey the unprepared party is "
                   f"{s_delta * 100:.0f} points of its own stomach worse off "
                   f"(bar {MIN_STOMACH_DELTA * 100:.0f})")
            # Reported, not gated — see the module docstring on why water
            # is evidence here rather than a threshold.
            p_water, p_rations = carried(port, prepared)
            c_water, c_rations = carried(port, control)
            print(f"  water/food evidence (not gated): prepared carries "
                  f"{p_water:.2f} L + {p_rations} rations at hydration "
                  f"{num(ap_['hydration']):.3f}; control carries "
                  f"{c_water:.2f} L + {c_rations} rations at hydration "
                  f"{num(ac['hydration']):.3f}", flush=True)
            print(f"  control stomach delta {s_delta:.3f} "
                  f"(measurement, not part of the fingerprint)", flush=True)
        finally:
            quit_engine(port, proc)

        # ============ engine B: a genuinely fresh process ==============
        chk.enter("load", "a fresh process reloads the finished expedition")
        proc = boot_probe(root, port, LOG_B, "engine B")
        try:
            bootstrap(port)
            send(port, f"engine.loadSave('{SLOT}'); return 'queued'")
            published, status = wait_load_published(port, 240)
            if not chk.ok(published, f"the save loads and publishes ({status})"):
                return 2
            send(port, f"world.show('{PAGE}'); return 'ok'")
            # Loads come up paused by design. scripts/tutorial_eval.lua
            # is deliberately not pause-gated, but scripts/unit_ai.lua
            # is — and the withdrawal below is a real unit action, so the
            # session has to be running for it, exactly as it would be
            # for a player resuming a save.
            send(port, "engine.setPaused(false); return 'ok'")

            inst = instance_by_id(port, PAGE, int(ruin["instance_id"]))
            chk.ok(isinstance(inst, dict)
                   and inst.get("lifecycle") in ("active", "cleared"),
                   f"the SAME page and location-instance id retains its visible "
                   f"encounter lifecycle "
                   f"after the restart ({PAGE}#{ruin['instance_id']} -> "
                   f"{(inst or {}).get('lifecycle')!r})")
            chk.ok(isinstance(inst, dict) and inst.get("contents_spawned") is True,
                   f"and its contents are still recorded as spawned exactly once "
                   f"(contents_spawned={(inst or {}).get('contents_spawned')!r})")

            # #917: the whole durable half of the significant-contents
            # contract, re-checked in a FRESH PROCESS — identity,
            # provenance, the taken latch, the compound predicate, and
            # the one-shot notice. Nothing here was written by this
            # engine: it all came off the disk.
            rows_after = significant_rows(port, ruin_id)
            chk.ok(len(rows_after) == 1
                   and rows_after[0].get("item_instance_id") == sig_phys
                   and rows_after[0].get("taken") is True,
                   f"the guaranteed item's identity, provenance and taken latch "
                   f"survive the restart ({rows_after})")
            chk.ok(isinstance(inst, dict)
                   and inst.get("lifecycle") == "cleared"
                   and inst.get("clearance_satisfied") is True,
                   f"the ruin is still CLEARED, with its compound predicate still "
                   f"satisfied ({(inst or {}).get('lifecycle')!r})")
            # The notice is a spent one-shot, and player events are
            # per-session and never saved — so a reloaded, already-cleared
            # ruin must announce nothing at all, however long the
            # discovery tick polls it.
            chk.ok(isinstance(inst, dict)
                   and inst.get("clear_event_emitted") is True,
                   f"its one clearance notice is recorded as already spent "
                   f"(clear_event_emitted="
                   f"{(inst or {}).get('clear_event_emitted')!r})")
            time.sleep(5.0)
            repeat = clearance_events(port)
            chk.ok(not repeat,
                   f"and the reload re-announces nothing "
                   f"({[e.get('text') for e in repeat]})")
            # The item itself is somewhere else entirely now, which is
            # explicitly allowed: the latch records that the ruin was
            # looted, not where the loot went.
            stored_now = send_json(port,
                                   f"return building.getStorage({storage_bid})")
            chk.ok(find_instance(
                       stored_now if isinstance(stored_now, list) else [],
                       sig_phys) is not None,
                   f"the guaranteed item is still in colony storage as that same "
                   f"physical instance ({sig_phys})")
            chk.ok(isinstance(inst, dict)
                   and int(inst.get("gx", 0)) == int(ruin["gx"])
                   and int(inst.get("gy", 0)) == int(ruin["gy"])
                   and inst.get("id") == ruin.get("id"),
                   f"with its definition and anchor unchanged "
                   f"({(inst or {}).get('id')!r} at "
                   f"({(inst or {}).get('gx')},{(inst or {}).get('gy')}))")

            key = f"{PAGE}#{ruin['instance_id']}"
            knew = poll_until(30.0, lambda: key in known_locations(port, prepared),
                              interval=1.0)
            chk.ok(bool(knew),
                   f"the expedition unit still knows that exact (page, instance) "
                   f"pair after the restart ({key} in "
                   f"{sorted(known_locations(port, prepared))})")

            completed, _checked = poll_until(
                45.0, lambda: (lambda p: p if p[0] else None)(progress(port)),
                interval=1.0) or progress(port)
            chk.ok(completed == EXPECTED_COMPLETED,
                   f"the completed objective set survives the reload exactly "
                   f"({sorted(completed)})")

            stored = send_json(port, f"return building.getStorage({storage_bid})")
            stored = stored if isinstance(stored, list) else []
            match = find_instance(stored, recovered["instanceId"])
            chk.ok(match is not None,
                   f"the recovered item is still owned by colony storage "
                   f"(bid {storage_bid}, instance {recovered['instanceId']})")
            chk.ok(match is not None
                   and match.get("defName") == recovered.get("defName"),
                   f"with its definition intact "
                   f"({(match or {}).get('defName')!r})")
            chk.ok(match is not None
                   and properties(match) == properties(recovered),
                   f"and every mutable property intact "
                   f"({properties(match)} vs {properties(recovered)})")

            # "invest", for this deferred-reward slice: the recovered
            # loot is a first-class colony asset a DIFFERENT colonist can
            # draw on, indistinguishable from a locally produced one.
            party = roster(port)
            others = [u for u in party.get(ACOLYTE_DEF, []) if u != prepared]
            user = others[0] if others else -1
            ok = send(port, f"return unit.withdrawFromCargo({user},{storage_bid},"
                            f"'{recovered['defName']}',{recovered['instanceId']})")
            held = find_instance(inventory(port, user), recovered["instanceId"])
            chk.ok(ok.strip() == "true" and held is not None
                   and properties(held) == properties(recovered),
                   f"a different colonist ({user}) draws that exact instance back "
                   f"out of colony storage and holds it unchanged — the recovered "
                   f"item is usable colony stock (returned {ok!r}, {properties(held)})")
            check_ai_tick_clean(chk, LOG_B, "engine B")
        finally:
            quit_engine(port, proc)
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
        if args.keep_root:
            print(f"kept resource root: {base}", flush=True)
        else:
            shutil.rmtree(base, ignore_errors=True)
        fingerprint["stages"] = chk.outcomes()
        print(f"\nFINGERPRINT {json.dumps(fingerprint, sort_keys=True)}", flush=True)
        chk.report()

    return 0 if chk.failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
