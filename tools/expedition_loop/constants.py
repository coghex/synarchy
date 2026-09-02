#!/usr/bin/env python3
"""Every stable value the expedition scenario is defined by (#2092).

One place, so requirement 5's "single-sourced" is structural rather than
asserted: the page and slot names, the def names, the site-selection
bounds, the departure contract, the control's bar, and the tutorial
objective ids each exist exactly once and every owner reads them from
here.

What is deliberately NOT here: a table that belongs to exactly one
function and is documented as part of it — `harness.YAML_LOADERS` and
`harness.SCRIPTS` (the bootstrap sequence), `readers.PROP_KEYS` (the
round-trip comparison), `readers.PROGRESS_LUA` (the tutorial read) and
`prepare.SHEDDABLE` (the shed order). Those are one function's
implementation, not scenario policy, and hoisting them here would
separate each from the comment that explains it.
"""
from __future__ import annotations

import os

#: The repository root. THREE dirnames, not the facade's two: this
#: module sits one directory deeper, in `tools/expedition_loop/`.
REPO = os.path.dirname(os.path.dirname(
    os.path.dirname(os.path.abspath(__file__))))

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
