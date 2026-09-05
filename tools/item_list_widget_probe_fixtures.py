#!/usr/bin/env python3
"""Authored fixture content and the staged fixture state for
`tools/item_list_widget_probe.py` (#2046).

Every throwaway def the run registers is defined here ONCE — the three
storage buildings, the deep-kit and empty-box items, the kit-carrier
unit, the two stock lists and the debug-console expressions naming each
host's live widget instance — so no scenario module carries a second
copy of a YAML body, an id or a level-addressing expression.

`stage_fixture_defs` is the staging half that belongs with the content
it writes: the three authored YAML files and the real loader verbs that
register them. The world-side staging that follows it — the dry anchor
sites, the spawns, the deposits and the late nesting stock — stays in
the facade, because the ORDER in which it mutates shared state is
exactly what a maintainer reads `_run` to see.

`Fixtures` is that staged state, represented once and handed to the
scenarios explicitly. Scenario modules take the ids they need as
parameters; none of them reconstructs one.
"""
from __future__ import annotations

import os
import sys
import tempfile
from dataclasses import dataclass

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from item_list_widget_probe_checks import check
from probelib import send

SPROOT = tempfile.gettempdir()
TEST_BUILDING_YAML = os.path.join(SPROOT, "item_list_widget_probe_buildings.yaml")
DEF_CARGO = "probe_item_list_cargo"
DEF_EMPTY = "probe_item_list_empty"
DEF_UNSEEN = "probe_item_list_unseen"

# DEF_UNSEEN is deliberately WORKER-BUILT (`build_work > 0`). A
# `building.spawn`ed instance of it is created at zero progress and never
# reaches Built, so A3's `SeedAtBuildCompletion` trigger never fires and
# its knowledge record genuinely does not exist — the never-inspected
# state, obtained without calling a single knowledge-mutating verb. Its
# capacity is still def-declared and therefore still LIVE, which is what
# requirement 1 needs a fixture for.
TEST_BUILDINGS = f"""\
buildings:
  - name: "{DEF_CARGO}"
    display_name: "Probe Item List Cargo"
    category: "Test"
    description: "Throwaway #1088 test fixture — not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    visual_class: "freestanding_installation"
    tile_size: {{ x: 1, y: 1 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 0.0
    storage_capacity: 400.0
  - name: "{DEF_EMPTY}"
    display_name: "Probe Item List Empty Cargo"
    category: "Test"
    description: "Throwaway #1237 known-empty fixture — not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    visual_class: "freestanding_installation"
    tile_size: {{ x: 1, y: 1 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 0.0
    storage_capacity: 250.0
  - name: "{DEF_UNSEEN}"
    display_name: "Probe Item List Unseen Cargo"
    category: "Test"
    description: "Throwaway #1237 never-inspected fixture — not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    visual_class: "freestanding_installation"
    tile_size: {{ x: 1, y: 1 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 240.0
    storage_capacity: 300.0
"""

# Deliberately spans several categories so the tab strip has something
# to compute, and repeats defs so grouping has something to merge.
# (defName, copies)
CARGO_STOCK = [
    ("steel_bar", 4),
    ("wood_log", 3),
    ("bandage", 2),
    ("quinoa_sack", 1),
]

# #1238 fixtures. A throwaway item-container whose default contents are
# 15 DISTINCT defs plus a real `first_aid_kit` — so a level rendering it
# has more rows than its own 12-row cap (something to scroll) AND a
# container row of its own (somewhere to descend). Registered through
# the real item-YAML loader, the same throwaway-def technique the
# building fixtures above use.
TEST_ITEM_YAML = os.path.join(SPROOT, "item_list_widget_probe_items.yaml")
DEF_DEEP_KIT = "probe_deep_kit"
DEEP_KIT_CONTENTS = [
    "bandage", "gauze", "elastic_wrap", "tweezers", "scissors",
    "steel_bar", "wood_log", "quinoa_sack", "wiring", "whetstone",
    "tomato", "wheat_grain", "granite_chunk", "steel_plate",
    "steel_hardware",
]
# Since #1418 EVERY creation path materializes a container def's
# declared `contents:` -- `unit.addItem` included -- so the deep kit
# would arrive stocked either way. It still comes in through a spawned
# carrier's starting inventory, deliberately: that is the path the
# nesting fixture was BUILT on, and re-routing it would be changing the
# fixture rather than following the behaviour change.
#
# What the change does take away is the free empty container the
# item-contents scenario used for its "(empty)" render state. That state
# is real UI behaviour and keeps its coverage, so the probe now AUTHORS
# an empty container: a throwaway def with no `contents:` key at all
# decodes to an empty list and materializes empty, however it is created.
DEF_EMPTY_BOX = "probe_empty_box"
TEST_EMPTY_BOX = f"""\
  - name: "{DEF_EMPTY_BOX}"
    display_name: "Probe Empty Box"
    sprite: "assets/textures/items/medical/first_aid_kit.png"
    weight: 0.5
    bulk: 4.0
    kind: container
    category: Medical
"""

TEST_ITEMS = "items:\n" + f"""\
  - name: "{DEF_DEEP_KIT}"
    display_name: "Probe Deep Kit"
    sprite: "assets/textures/items/medical/first_aid_kit.png"
    weight: 0.5
    bulk: 4.0
    kind: container
    category: Medical
    contents:
""" + "".join(f"      - {{ item: {d}, count: 1 }}\n"
              for d in DEEP_KIT_CONTENTS) + \
    "      - { item: first_aid_kit, count: 1 }\n" + TEST_EMPTY_BOX

TEST_UNIT_YAML = os.path.join(SPROOT, "item_list_widget_probe_units.yaml")
DEF_CARRIER = "probe_kit_carrier"
TEST_UNITS = f"""\
units:
  - name: {DEF_CARRIER}
    display_name: "Probe Kit Carrier"
    sprite: "assets/textures/units/tiller/animations/idle/south/frame_000.png"
    starting_inventory:
      - {{ item: "{DEF_DEEP_KIT}", count: 1 }}
"""

# Enough DISTINCT defs in the cargo that the base level (10-row cap) has
# somewhere to scroll to as well. Deposited BEFORE the two containers, so
# both of those stay inside the first rendered rows: a level renders its
# rows in the remembered list's own order, and `unit.depositToCargo`
# PREPENDS (`biStorage = item : biStorage`), so the newest deposit is the
# first row.
CARGO_BULK_STOCK = [
    "bronze_bar", "granite_chunk", "wiring", "whetstone", "tomato",
    "wheat_grain", "steel_plate", "steel_hardware", "rations", "radio",
    "solar_panel",
]

# Debug-console expressions naming each host's live widget instance.
# Since #1238 the container window owns a STACK of levels, and the
# item-contents popup is one of them rather than a second panel:
# `getLevel(i)` names one (default the deepest), and every read below
# goes through it because a level may not exist.
LEVEL = "require('scripts.cargo_inventory_panel').getLevel"
BASE_LEVEL = f"({LEVEL}(1) or {{src={{}}}})"
DEEP_LEVEL = f"({LEVEL}() or {{src={{}}}})"
CARGO_LIST_ID = f"{BASE_LEVEL}.listId"
ITEM_CONTENTS_LIST_ID = f"{DEEP_LEVEL}.listId"
UNIT_INV_LIST_ID = "require('scripts.unit_info_v2').invListId"


@dataclass
class Fixtures:
    """The staged world state, built once by the facade and passed to
    the scenarios explicitly (requirement 8).

    Site coordinates are kept beside the ids they belong to because two
    scenarios need them for their own camera and spawn work — the cargo
    building's anchor for the hit test, and the acolyte's for the
    unit-to-unit escort's spawn pair."""
    bid: int
    empty_bid: int
    unseen_bid: int
    uid: int
    mule_uid: int
    wild_uid: int
    building_site: tuple[int, int]
    acolyte_site: tuple[int, int]
    carrier_site: tuple[int, int]
    bpixel: object


def stage_fixture_defs(port: int) -> None:
    """Write the three authored YAML files and register them through the
    real loader verbs.

    Each load is checked for the count its own file declares, so a
    fixture that silently failed to register is a red run rather than a
    scenario failing later for an unrelated-looking reason."""
    with open(TEST_BUILDING_YAML, "w") as f:
        f.write(TEST_BUILDINGS)
    n = send(port, f"return engine.loadBuildingYaml('{TEST_BUILDING_YAML}')")
    check("probe building defs loaded", float(n) == 3.0, f"got {n!r}")

    with open(TEST_ITEM_YAML, "w") as f:
        f.write(TEST_ITEMS)
    ni = send(port, f"return engine.loadItemYaml('{TEST_ITEM_YAML}')")
    check("probe deep-kit item def loaded", float(ni) >= 1.0, f"got {ni!r}")
    with open(TEST_UNIT_YAML, "w") as f:
        f.write(TEST_UNITS)
    nu = send(port, f"return engine.loadUnitYaml('{TEST_UNIT_YAML}')")
    check("probe kit-carrier unit def loaded", float(nu) >= 1.0, f"got {nu!r}")
