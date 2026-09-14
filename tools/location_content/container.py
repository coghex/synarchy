#!/usr/bin/env python3
"""Pending container shells: the end-to-end half of #2505 (epic #1231,
PLC-14) that no hspec group can reach.

`Test.Headless.Location.ContainerShells` already pins the pure rules, the
spawn verb and the pickup refusal against hand-built pages. What it
cannot see is the ONE-TIME content lifecycle running for real: a shell
minted the first time a chunk actually loads, NOT minted again when that
chunk is revisited, and the pending slot surviving a save, a process
exit and a load in a fresh engine. That is what this owner covers.

Its three YAML fixtures are the reason the scenario exists at all: no
SHIPPED location authors a `kind: container` entry yet (PLC-10 owns the
wooden crate and the `ruin_small` entry that will carry it), so the
probe supplies its own crate item, its own loot profile, and a DENSE
location that guarantees one at the synchronous centre chunk.
"""
from __future__ import annotations

import json
import time

from probelib import load_fixture_yaml, send

from .invocation import RunArtifacts

#: The pending shell's own item definition. A real portable container —
#: it declares a `storage` block AND one authored default content — so
#: the fixture can tell design D-22's two outcomes apart: "unrolled"
#: means no PROFILE draw has happened, NOT an empty tree, and a shell
#: that came out with nothing inside would be the wrong one.
CONTAINER_ITEM_YAML = (
    "items:\n"
    "  - name: \"probe_pending_crate\"\n"
    "    display_name: \"Probe Pending Crate\"\n"
    "    sprite: \"assets/textures/items/tool/toolbox.png\"\n"
    "    weight: 6.0\n"
    "    bulk: 60.0\n"
    "    kind: container\n"
    "    category: Tools\n"
    "    make: factory\n"
    "    material: steel\n"
    "    storage:\n"
    "      weight_capacity: 40.0\n"
    "      bulk_capacity: 50.0\n"
    "    contents:\n"
    "      - { item: rations, count: 1 }\n"
)

#: The profile the slot names. Never DRAWN in this slice — PLC-15 owns
#: realization — but it must resolve against the live registry at
#: location load AND at save load, which is exactly what this fixture
#: makes real rather than hypothetical.
CONTAINER_PROFILE_YAML = (
    "id: probe_crate_salvage\n"
    "quantity_multiplier:\n"
    "  min: 1\n"
    "  max: 2\n"
    "entries:\n"
    "  - item: steel_bar\n"
    "    chance: 0.5\n"
    "    quantity_factor: 2\n"
)

#: One per land chunk (the `dense_ruin` pattern this probe already uses
#: for the hidden-page dispatch phase), so a container entry is
#: guaranteed at the synchronous centre chunk (0,0). A fixed `position`
#: keeps the shell on a known tile, which is what lets the pickup
#: refusal below address it without searching.
CONTAINER_LOCATION_YAML = (
    "locations:\n"
    "  - id: crate_ruin\n"
    "    label: Crate Ruin\n"
    "    type: ruin\n"
    "    builder: room_small\n"
    "    anchor: [waterside]\n"
    "    max_count: 100000\n"
    "    min_spacing: 1\n"
    "    bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }\n"
    "    naming: { heads: [KEEP], modifiers: [ASH] }\n"
    "    contents:\n"
    "      - { kind: container, id: probe_pending_crate, "
    "profile: probe_crate_salvage, count: 1, position: {x: 0, y: 0} }\n"
)

#: The crate definition's own empty weight, and the mass its ONE
#: authored default content adds — both read straight off the fixture
#: bodies above, so editing either there and not here fails loudly
#: rather than silently weakening the D-22 assertion.
EMPTY_CRATE_KG = 6.0
RATIONS_KG = 0.1

#: The page every phase of this scenario generates onto. Its own id, so
#: nothing here can read or be read by the ruin_small pages the other
#: owners work on.
CRATE_PAGE = "wk"


def write_container_fixtures(art: RunArtifacts) -> tuple[str, str, str]:
    """Stage this scenario's three fixtures into the invocation's own
    fixtures directory, and answer their paths in REGISTRATION order —
    items, then the profile, then the location.

    That order is the engine's own (`scripts/startup_loader.lua`) and it
    is load-bearing here, not cosmetic: the location loader resolves a
    container entry's item id AND its profile id against the live
    registries and rejects the whole file on either, so a location
    registered first would register nothing at all.
    """
    item_yaml = art.fixture("crate_item")
    with open(item_yaml, "w") as fh:
        fh.write(CONTAINER_ITEM_YAML)
    profile_yaml = art.fixture("crate_profile")
    with open(profile_yaml, "w") as fh:
        fh.write(CONTAINER_PROFILE_YAML)
    location_yaml = art.fixture("crate_location")
    with open(location_yaml, "w") as fh:
        fh.write(CONTAINER_LOCATION_YAML)
    return item_yaml, profile_yaml, location_yaml


def register_container_fixtures(port: int, item_yaml: str,
                                profile_yaml: str,
                                location_yaml: str) -> None:
    """Register those three, in that order, each through
    `load_fixture_yaml` so one that registers nothing stops the probe at
    SETUP rather than surfacing as a downstream behavioural failure."""
    load_fixture_yaml(port, "engine.loadItemYaml", item_yaml)
    load_fixture_yaml(port, "engine.loadLootProfileYaml", profile_yaml)
    load_fixture_yaml(port, "engine.loadLocationYaml", location_yaml)


def _decode_rows(reply: str) -> list[dict]:
    """The console JSON-encodes a returned Lua table, exactly as
    `engine_queries.ground_items` already relies on. A reply that is not
    a list answers an empty one — the caller's own assertion then reports
    the discrepancy, rather than this helper raising inside a probe
    phase."""
    raw = reply.strip()
    if not raw or raw in ("nil", "null", "{}", "[]"):
        return []
    try:
        data = json.loads(raw)
    except json.JSONDecodeError:
        return []
    return data if isinstance(data, list) else []


def _crate_slots(port: int, page: str) -> list[dict]:
    """Every container slot on every placed location of `page`, each
    carrying its owning instance id — read through the REAL
    `world.listPlacedLocations`, which is the surface #2505 added the
    array to.

    Reduced SERVER-SIDE, like `engine_queries.loc_at`: crate_ruin is a
    DENSE definition (one per land chunk), so shipping the whole placed
    list to Python would be thousands of entries to find a handful of
    slots in."""
    return _decode_rows(send(
        port,
        "local out = {} "
        f"for _, e in ipairs(world.listPlacedLocations('{page}') or {{}}) do "
        "  for _, c in ipairs(e.containers or {}) do "
        "    out[#out + 1] = { instance = e.instance_id, "
        "      slot = c.slot, item = c.item, profile = c.profile, "
        "      realized = c.realized, "
        "      bound = c.item_instance_id or -1 } "
        "  end "
        "end "
        "return out", timeout=20.0))


def _shell_rows(port: int, item_id: str) -> list[dict]:
    """Every ground shell of definition `item_id`, as
    `{gid, instance, weight}` rows.

    `item.listGround()` is ACTIVE-page scoped with no page argument, and
    every phase here shows CRATE_PAGE before reading — which is also
    what makes the ground ids it returns addressable by
    `item.pickupGround`."""
    return _decode_rows(send(
        port,
        "local out = {} "
        "for _, g in ipairs(item.listGround() or {}) do "
        f"  if g.defName == '{item_id}' then "
        "    out[#out + 1] = { gid = g.id, instance = g.instanceId, "
        "                      weight = g.weight } "
        "  end "
        "end "
        "return out", timeout=20.0))


def observe_initial_shell(args, state, failures: list[str]) -> None:
    """The first chunk load mints exactly ONE shell per crate ruin whose
    chunk actually loaded, and binds each to that ruin's own slot.

    Scoped to BOUND slots on purpose. `crate_ruin` is a DENSE definition
    (one per land chunk, the pattern that guarantees content at the
    synchronous centre chunk), so the world derives a slot for every land
    chunk in it while only the loaded region ever spawns anything — a
    total-slot assertion would be asserting how much of the world the
    probe happened to page in, which is not what this scenario is about.
    What IS asserted is the pairing: every bound slot has exactly one
    shell, no shell is unowned, and none of it is realized.
    """
    slots = _crate_slots(args.port, CRATE_PAGE)
    if not slots:
        failures.append(
            "no container slots were derived at placement — the crate_ruin "
            "fixture registered but placed nothing, so nothing below is "
            "testable")
        return
    print(f"PASS: placement derived {len(slots)} container slot(s)")

    bound = [s for s in slots if s.get("bound", -1) > 0]
    if not bound:
        failures.append(
            f"none of the {len(slots)} derived container slot(s) bound a "
            "shell after the first chunk load — the incidental dispatch "
            "never reached world.spawnLocationContainer")
        return
    state.crate_slots = len(bound)
    print(f"PASS: {len(bound)} slot(s) in the loaded region bound a shell on "
          "first chunk load")

    if any(s.get("realized") for s in slots):
        failures.append("a slot came back REALIZED — this slice never "
                        "transitions one, PLC-15 does")
    else:
        print("PASS: every slot is still PENDING (realized = false)")

    if any(s.get("profile") != "probe_crate_salvage" for s in slots):
        failures.append("a derived slot carries the wrong profile id")
    elif any(s.get("item") != "probe_pending_crate" for s in slots):
        failures.append("a derived slot carries the wrong container def name")
    else:
        print("PASS: every slot names the authored crate AND the authored "
              "profile")

    shells = _shell_rows(args.port, "probe_pending_crate")
    state.crate_shells = len(shells)
    if len(shells) != len(bound):
        failures.append(
            f"{len(bound)} bound slot(s) but {len(shells)} shell(s) on the "
            "ground — a shell is unowned, or a binding named an item that "
            "is not there")
    elif sorted(s["bound"] for s in bound) != sorted(s["instance"]
                                                     for s in shells):
        failures.append(
            "the bound slot ids and the ground shell ids are different sets "
            "— a slot is bound to something other than the shell it minted")
    else:
        print(f"PASS: each of the {len(shells)} shell(s) is the OUTER GROUND "
              "item its own slot names")

    # D-22: the shell mints through the materializer unchanged, authored
    # default contents included. An empty tree here would be the WRONG
    # outcome, and is exactly what a container-specific spawn path would
    # have produced.
    #
    # Read off `weight`, which item.listGround reports as the live
    # RECURSIVE mass (itemTotalWeight: empty weight + fill + nested
    # contents) rather than the static def weight. The crate's own empty
    # weight is EMPTY_CRATE_KG and its one authored `rations` adds
    # RATIONS_KG, so an empty shell and a stocked one are distinguishable
    # by a value the engine computed rather than by a count this probe
    # would have to ask a second verb for.
    lightest = min(s.get("weight", 0.0) for s in shells)
    if lightest > EMPTY_CRATE_KG + RATIONS_KG / 2:
        print("PASS: every shell's recursive weight includes its "
              "definition's authored default content (unrolled is not "
              "empty, D-22)")
    else:
        failures.append(
            f"a spawned shell weighs {lightest} kg, at or below the "
            f"{EMPTY_CRATE_KG} kg empty crate — its authored default "
            "content did not materialize")


def check_no_respawn_and_pickup(args, state, failures: list[str]) -> None:
    """Revisiting the same chunks mints no second shell, and an ordinary
    pickup of a pending one is REFUSED with the shell left where it is."""
    send(args.port, "return world.loadChunksInRegion(-1,-1,1,1)")
    time.sleep(1.0)
    shells = _shell_rows(args.port, "probe_pending_crate")
    if len(shells) != state.crate_shells:
        failures.append(
            f"revisiting respawned shells: {state.crate_shells} before, "
            f"{len(shells)} after")
        return
    print("PASS: revisiting the crate ruins respawned no shell")

    target = shells[0]
    uid = send(args.port,
               "return unit.spawn('acolyte', 0, 0, nil, 'player', "
               f"'{CRATE_PAGE}')").strip().strip('"')
    try:
        uid = int(float(uid))
    except (TypeError, ValueError):
        failures.append(f"could not spawn a pickup unit: {uid!r}")
        return
    if uid < 0:
        failures.append(f"could not spawn a pickup unit: {uid}")
        return
    # An acolyte spawns holding its starting equipment, so the inventory
    # is compared BEFORE and AFTER rather than to zero — the claim is
    # "nothing moved", not "the unit is empty".
    before_held = _inventory_size(args.port, uid)
    # Requirement 7: refused BEFORE anything moves. The shell must still
    # be on the ground afterwards, under the SAME ground id — a rollback
    # would have given it a new one.
    picked = send(args.port,
                  f"return item.pickupGround({uid}, {target['gid']})")
    after = _shell_rows(args.port, "probe_pending_crate")
    still_there = [s for s in after if s["gid"] == target["gid"]]
    after_held = _inventory_size(args.port, uid)
    if (picked.strip() == "false" and still_there
            and after_held == before_held
            and len(after) == state.crate_shells):
        print("PASS: picking up a pending shell was REFUSED, the shell kept "
              "its ground id, and the unit's inventory is untouched")
    else:
        failures.append(
            "a pending shell's pickup should be refused with nothing moved; "
            f"pickupGround={picked!r}, still on ground={bool(still_there)}, "
            f"inventory {before_held} -> {after_held}, shells "
            f"{state.crate_shells} -> {len(after)}")

    bound = [s for s in _crate_slots(args.port, CRATE_PAGE)
             if s.get("bound", -1) > 0]
    if any(s.get("realized") for s in bound):
        failures.append("a refused pickup realized a slot")
    elif len(bound) == state.crate_slots:
        print("PASS: the refused pickup left every slot bound and pending")
    else:
        failures.append(
            f"the refused pickup changed the bound-slot count: "
            f"{state.crate_slots} before, {len(bound)} after")


def _inventory_size(port: int, uid: int) -> int:
    reply = send(port, f"return #(unit.getInventory({uid}) or {{}})")
    try:
        return int(float(reply.strip().strip('"')))
    except (TypeError, ValueError):
        return -1


def check_shell_survived_reload(args, state, failures: list[str]) -> None:
    """The pending slot and its shell come back from save -> quit ->
    fresh process -> load, still bound and still unrealized."""
    slots = _crate_slots(args.port, CRATE_PAGE)
    bound = [s for s in slots if s.get("bound", -1) > 0]
    if len(bound) != state.crate_slots:
        failures.append(
            f"the load restored {len(bound)} bound container slot(s), "
            f"expected {state.crate_slots}")
        return
    if any(s.get("realized") for s in slots):
        failures.append("a restored slot came back REALIZED")
        return
    if any(s.get("profile") != "probe_crate_salvage" for s in bound):
        failures.append("a restored slot lost or changed its profile id")
        return
    print("PASS: every pending slot round-tripped save/quit/load with its "
          "shell id, its profile and its unrealized latch intact")

    shells = _shell_rows(args.port, "probe_pending_crate")
    if len(shells) != state.crate_shells:
        failures.append(
            f"the load restored {len(shells)} shell(s) on the ground, "
            f"expected {state.crate_shells}")
        return
    if sorted(s["bound"] for s in bound) == sorted(s["instance"]
                                                   for s in shells):
        print("PASS: every restored shell is still the OUTER GROUND item its "
              "own slot names — the pending-provenance rule the load "
              "boundary enforces")
    else:
        failures.append(
            "the restored slots and ground shells name different item "
            "instances")
