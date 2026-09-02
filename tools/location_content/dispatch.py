#!/usr/bin/env python3
"""Content dispatch and rejection: what `spawnContents` does with each
kind of entry, valid or not (#2095).

One owner for the unknown unit id and the loot table rolling an
unregistered item (both warned about, neither fatal), the two
warning-log assertions those produce, the fixed-position `kind: item`
branch #921 left no shipped location using, and the valid unit and
building content that dispatches onto a hidden, non-active page.

It also owns all five inline YAML fixtures, because it is the scenario
that consumes them (#2095 requirement 7). Their bodies are load-bearing
content -- which ids they name is what the unknown-id checks read back,
the single-entry loot tables are what make a specific item spawn
whatever the draw selects, and the fixed `position` is what is asserted
to the exact tile -- so `tools/test_location_content_probe.py` pins
these bytes by digest and pins the order the calls below register them
in.
"""
from __future__ import annotations

import time

from probelib import load_fixture_yaml, send

from .engine_queries import (ground_items, loc_at, registered_item_names,
                             spawn_counts, unit_count, unregistered_item_ids,
                             wait_floor)
from .invocation import RunArtifacts

#: The probe's own inline YAML fixtures, as the exact bytes that reach
#: disk (#1884). They were inline `fh.write(...)` calls at the phases
#: that use them; only WHERE they are written moved, never WHAT they
#: say. Placement and loot draws are order- and content-sensitive, so
#: `tools/test_location_content_probe.py` pins these bodies by digest
#: and pins the registration order of the calls that load them.

#: Phase 3's unknown-content-id fixture. Deliberately full of unknown
#: IDS, but no unknown KIND: #1708 closed that vocabulary at the YAML
#: boundary, so an entry naming one would fail the whole file's decode
#: and leave `bogus_ruin` unregistered.
BOGUS_LOCATION_YAML = (
    "locations:\n"
    "  - id: bogus_ruin\n"
    "    label: Bogus Ruin\n"
    "    type: ruin\n"
    "    builder: room_small\n"
    "    anchor: []\n"
    "    max_count: 0\n"
    "    bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }\n"
    "    naming: { heads: [KEEP], modifiers: [ASH] }\n"
    "    contents:\n"
    "      - { kind: unit, id: does_not_exist, count: 1 }\n"
    "      - { kind: loot_table, id: bogus_table, rolls: 1 }\n"
)

#: …and the loot table it rolls, whose only entry is an unregistered
#: item id.
BOGUS_LOOT_YAML = (
    "id: bogus_table\n"
    "entries:\n"
    "  - id: item_that_does_not_exist\n"
    "    weight: 1\n"
)

#: The fixed-position `kind: item` content phase 3 asserts to the exact
#: tile. It keeps the `spawnItemContent` dispatch branch
#: (scripts/locations.lua) under test: #921 removed the last SHIPPED use
#: of it, and an untested branch is one edit from silently breaking for
#: the loot-container work that will want it back. `position` is the
#: part with no other coverage — a scattered entry lands anywhere in
#: bounds, so only a fixed one can be asserted to the exact tile.
FIXED_DEF, FIXED_OX, FIXED_OY = "radio", -1, 2

#: A single-entry loot table forces quinoa_sack to spawn through the
#: real content-spawn path (locations.spawnContents -> loot.rollFor ->
#: item.spawnGround) whatever the roll context, rather than depending on
#: whether ruin_common's 2/13-weight entry happens to be the one this
#: instance's draw selects (#800). #948 made that draw seed-stable
#: rather than random, but it is still weight-dependent — which entry a
#: given instance lands on is not something to assert on here.
QUINOA_LOCATION_YAML = (
    "locations:\n"
    "  - id: probe_quinoa_ruin\n"
    "    label: Quinoa Probe Ruin\n"
    "    type: ruin\n"
    "    builder: room_small\n"
    "    anchor: []\n"
    "    max_count: 0\n"
    "    bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }\n"
    "    naming: { heads: [KEEP], modifiers: [ASH] }\n"
    "    contents:\n"
    "      - { kind: loot_table, id: probe_quinoa_table, rolls: 1 }\n"
    f"      - {{ kind: item, id: {FIXED_DEF}, count: 1, "
    f"position: {{x: {FIXED_OX}, y: {FIXED_OY}}} }}\n"
)

QUINOA_LOOT_YAML = (
    "id: probe_quinoa_table\n"
    "entries:\n"
    "  - id: quinoa_sack\n"
    "    weight: 1\n"
)

#: Phase 4's DENSE location (one per land chunk, like
#: tools/location_overlay_probe.py's DENSE_YAML), which guarantees
#: content at the SYNCHRONOUS centre chunk (0,0).
DENSE_LOCATION_YAML = (
    "locations:\n"
    "  - id: dense_ruin\n"
    "    label: Dense Ruin\n"
    "    type: ruin\n"
    "    builder: room_small\n"
    "    anchor: [waterside]\n"
    "    max_count: 100000\n"
    "    min_spacing: 1\n"
    "    bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }\n"
    "    naming: { heads: [KEEP], modifiers: [ASH] }\n"
    "    contents:\n"
    "      - { kind: building, id: cargo_hold_S, count: 1, position: {x: 0, y: 0} }\n"
    "      - { kind: unit, id: acolyte, count: 1, faction: hostile, position: {x: 1, y: 1} }\n"
)


def write_rejection_fixtures(art: RunArtifacts) -> tuple[str, str, str, str]:
    """Stage phase 3's four fixtures into this invocation's own
    fixtures directory, and answer with their paths in registration
    order."""
    bogus_yaml = art.fixture("bogus")
    with open(bogus_yaml, "w") as fh:
        fh.write(BOGUS_LOCATION_YAML)
    bogus_loot_yaml = art.fixture("bogus_loot")
    with open(bogus_loot_yaml, "w") as fh:
        fh.write(BOGUS_LOOT_YAML)
    # Why this fixture's single-entry loot table and fixed-position item
    # are what they are: see QUINOA_LOCATION_YAML above.
    quinoa_yaml = art.fixture("quinoa")
    with open(quinoa_yaml, "w") as fh:
        fh.write(QUINOA_LOCATION_YAML)
    quinoa_loot_yaml = art.fixture("quinoa_loot")
    with open(quinoa_loot_yaml, "w") as fh:
        fh.write(QUINOA_LOOT_YAML)
    return bogus_yaml, bogus_loot_yaml, quinoa_yaml, quinoa_loot_yaml


def register_rejection_fixtures(port: int, bogus_yaml: str,
                                bogus_loot_yaml: str, quinoa_yaml: str,
                                quinoa_loot_yaml: str) -> None:
    """Register those four, in the unchanged order, each through
    `load_fixture_yaml` so one that registers nothing stops the probe at
    SETUP rather than surfacing as downstream behavioural failures
    (#1342)."""
    # These four fixtures are DELIBERATELY full of unknown IDS, but the
    # files themselves must still register: phase 3 is about what
    # spawnContents does with an unresolvable content id, which it can
    # only reach once the location and loot-table defs exist. That is
    # exactly why no entry here names a bogus KIND — since #1708 the
    # kind vocabulary is closed at load, so one would make
    # load_fixture_yaml's zero-count rejection fire here instead.
    load_fixture_yaml(port, "engine.loadLocationYaml", bogus_yaml)
    load_fixture_yaml(port, "engine.loadLootTableYaml", bogus_loot_yaml)
    load_fixture_yaml(port, "engine.loadLocationYaml", quinoa_yaml)
    load_fixture_yaml(port, "engine.loadLootTableYaml", quinoa_loot_yaml)


def check_unknown_content(args, art: RunArtifacts,
                          failures: list[str]) -> None:
    """Phase 3: an unresolvable content id warns and is skipped rather
    than crashing, the registry check accepts a real spawn and rejects a
    synthetic unregistered id, and the fixed-position `kind: item` entry
    lands on exactly its declared tile."""
    # Stamp directly (bogus_ruin has max_count 0, so it never places via
    # the overlay) — content-spawning is the concern here, not overlay
    # placement. spawnContents dispatches to unit/kind lookups directly.
    r = send(args.port,
              "local locations = require('scripts.locations'); "
              "locations.spawnContents('bogus_ruin', 40, 40, 'wc'); "
              "return 'ok'")
    alive = send(args.port, "return engine.getFPS() ~= nil and 'alive' or 'dead'")
    if r.strip('"') == "ok" and "alive" in alive:
        print("PASS: unknown unit id + unknown loot roll did not crash "
              "the engine")
    else:
        failures.append(f"spawnContents with bogus content misbehaved: {r!r} / {alive!r}")
    log_text = open(art.engine_log, errors="replace").read()
    if ("unknown unit content" in log_text
            and "rolled unknown item id" in log_text):
        print("PASS: the unknown unit id AND the "
              "loot-table-rolled-unknown-item-id both logged a warning")
    else:
        failures.append(
            "expected warnings for unknown unit id AND unknown loot "
            f"roll not both found in {art.engine_log}")

    # #800: the registry-based validation replacing the old hardcoded
    # loot_names allowlist. First, force quinoa_sack through the real
    # content-spawn path via the single-entry loot table above.
    # world.hasSpawnedLocationContents/markLocationContentsSpawned track
    # a one-time flag per CHUNK (chunkSize=16 tiles), not per exact tile
    # — this anchor must land in a different chunk than bogus_ruin's
    # (40,40) (chunk 2,2), or it would see that chunk already marked
    # spawned and silently no-op.
    send(args.port,
         "local locations = require('scripts.locations'); "
         "locations.spawnContents('probe_quinoa_ruin', 400, 400, 'wc'); "
         "return 'ok'")
    registered = registered_item_names(args.port)
    counts3 = spawn_counts(args.port)

    # The fixed-position `kind: item` branch: exactly one instance,
    # on the anchor + declared offset tile and no other. Checked by
    # coordinate, so a scatter regression (ignoring `position`) fails
    # here even though the item count would still be right.
    fixed_at = [g for g in ground_items(args.port)
                if g.get("defName") == FIXED_DEF]
    want_xy = (400 + FIXED_OX, 400 + FIXED_OY)
    got_xy = [(round(g["x"]), round(g["y"])) for g in fixed_at]
    if got_xy == [want_xy]:
        print(f"PASS: the fixed-position 'kind: item' entry spawned one "
              f"{FIXED_DEF} at exactly {want_xy} (anchor + declared "
              f"offset), the branch #921 left no shipped location using")
    else:
        failures.append(
            f"fixed-position item content wrong: expected one {FIXED_DEF} "
            f"at {want_xy}, got {got_xy}")

    got_quinoa = counts3["ground_by_name"].get("quinoa_sack", 0)
    if got_quinoa >= 1:
        print(f"PASS: a forced single-entry loot table deterministically "
              f"spawned quinoa_sack ({got_quinoa}), independent of "
              f"ruin_common's 2/13-weight entry")
    else:
        failures.append(
            f"probe_quinoa_ruin's loot table did not spawn quinoa_sack: {counts3}")
    accepted = unregistered_item_ids(set(counts3["ground_by_name"]), registered)
    if not accepted:
        print("PASS: the registry check accepts the deterministically "
              "forced quinoa_sack (data/items/quinoa_sack.yaml is a "
              "registered def)")
    else:
        failures.append(
            f"registry check rejected valid spawned item(s): {accepted}")

    # The engine already skips + warns an unregistered loot roll before
    # it becomes a ground item (asserted above), so a real spawn can
    # never surface one for the new registry check to reject — drive
    # the check function directly with a synthetic unregistered id
    # instead (issue #800 review amendment).
    bogus_name = "item_that_does_not_exist"
    rejected = unregistered_item_ids({bogus_name}, registered)
    if rejected == {bogus_name}:
        print(f"PASS: the registry check rejects a synthetic "
              f"unregistered item id ({bogus_name!r})")
    else:
        failures.append(
            f"registry check did not reject synthetic unregistered id "
            f"{bogus_name!r}: got {rejected}")


def write_dense_fixture(art: RunArtifacts) -> str:
    """Stage phase 4's DENSE fixture (one location per land chunk, like
    tools/location_overlay_probe.py's own), which guarantees content at
    the SYNCHRONOUS centre chunk (0,0)."""
    dense_yaml = art.fixture("dense")
    with open(dense_yaml, "w") as fh:
        fh.write(DENSE_LOCATION_YAML)
    return dense_yaml


def register_dense_fixture(port: int, dense_yaml: str) -> None:
    """Register it -- alone, through the same checking helper."""
    load_fixture_yaml(port, "engine.loadLocationYaml", dense_yaml)


def check_hidden_page_dispatch(args, failures: list[str]):
    """Phase 4: a building AND a unit content entry spawn correctly on a
    HIDDEN, non-active page (#90 review fix -- building.spawn takes an
    explicit pageId, mirroring unit.spawn/item.spawnGround, and its
    occupancy/terrain-Z check is scoped to THAT page rather than a
    snapshot of the visible worlds).

    Answers with the (gx, gy) the centre-chunk location stamped at, or
    None when a step before that could not be established. The facade
    hands that on to the cross-page knowledge check, so #915 runs on
    exactly the site this owner proved real -- the same nesting the
    single `run` expressed as an `else` chain.
    """
    send(args.port, "world.initArena('arena'); world.initArenaDone('arena'); "
                    "world.show('arena'); return 'ok'")
    arena_ok = False
    for _ in range(40):
        r = send(args.port, "local i=world.getChunkInfo(0,0); return i and i.loaded and 'y' or 'n'").strip('"')
        if r == "y":
            arena_ok = True
            break
        time.sleep(0.25)
    if not arena_ok:
        failures.append("phase 4: arena never became ready")
        return None
    # Generate 'sw2' but NEVER show it — arena stays active throughout.
    # NB world.waitForInit always polls the ACTIVE world (arena,
    # already done) — it can't wait for a hidden page, so loc_at's
    # own retry loop is what actually waits for 'sw2' to be ready.
    send(args.port, f"world.init('sw2', {args.seed}, {args.size}, 3); return 'ok'")
    active = send(args.port, "return world.getActiveWorldId()").strip('"')
    if active != "arena":
        failures.append(f"phase 4: expected 'arena' active throughout, got '{active}'")
        return None
    gxgy = loc_at(args.port, 0, 0, "sw2")
    if gxgy is None:
        failures.append(
            "phase 4: no location on centre chunk (0,0) of hidden page 'sw2'")
        return None
    gx, gy = gxgy
    if not wait_floor(args.port, gx, gy, page="sw2"):
        failures.append(
            f"phase 4: centre chunk (0,0)/({gx},{gy}) on 'sw2' never stamped")
        return None
    blist = send(args.port, "return building.list()")
    if f"({gx}, {gy}," in blist:
        print(f"PASS: building content spawned at ({gx},{gy}) on hidden "
              f"page 'sw2' while 'arena' stayed active (multiworld fix)")
    else:
        failures.append(
            f"phase 4: no cargo_hold_S building at ({gx},{gy}) on "
            f"hidden page 'sw2' — building.list() returned: {blist!r}")
    # unit content (a KNOWN id) spawns too — the
    # unit-kind dispatch path, moved here now that
    # This fixture's fixed acolyte exercises the ordinary
    # unit-kind dispatch in addition to ruin_small's ranged
    # nomad entries. The spawn happened while 'sw2' was hidden;
    # unit.list is
    # active-world-only (#377), so show sw2 to observe
    # it — the hidden-spawn property is already proven.
    send(args.port, "world.show('sw2'); return 'ok'")
    n_units = 0
    for _ in range(20):
        n_units = unit_count(args.port, "acolyte")
        if n_units >= 1:
            break
        time.sleep(0.5)
    if n_units >= 1:
        print(f"PASS: unit content spawned on hidden page 'sw2' "
              f"({n_units} acolyte)")
    else:
        failures.append(
            "phase 4: no acolyte unit spawned from dense_ruin "
            "unit content on hidden page 'sw2'")
    return gx, gy
