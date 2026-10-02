#!/usr/bin/env python3
"""Pending container shells and their realization: the end-to-end half
of #2505 (epic #1231, PLC-14) and #2510 (PLC-15) that no hspec group can
reach.

`Test.Headless.Location.ContainerShells` and
`Test.Headless.Location.ContainerRealization` already pin the pure rules,
the spawn verb and the realization boundary against hand-built pages.
What they cannot see is the ONE-TIME lifecycle running for real: a shell
minted the first time a chunk actually loads, NOT minted again when that
chunk is revisited, realized exactly once -- in place through
`item.realizeGround`, or by being picked up -- and every state of it
surviving a save, a process exit and a load in a fresh engine; and two
further fresh processes, each loading the same PRISTINE save (taken
after the shells spawned, before anything was realized) and visiting its
chunks and realizing its shells in opposite orders, realizing every shell
into the same complete tree that the first process and the reloaded one
realized for the same location slot. That is what this owner covers.

Its four YAML fixtures are the reason the scenario exists at all: no
SHIPPED location authors a `kind: container` entry yet (PLC-10 owns the
wooden crate and the `ruin_small` entry that will carry it), so the
probe supplies its own crate item, its own loot profile, a DENSE
location pairing them that guarantees one at the synchronous centre
chunk, and a container-free twin of that location -- the fourth, used by
the last phase alone, and the only way to reach an engine that knows the
location def but has never heard of the profile.
"""
from __future__ import annotations

import json
import time

from probelib import load_fixture_yaml, send, send_json

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

#: The profile the slot names. It must resolve against the live registry
#: at location load AND at save load, and (#2510) it is what every shell
#: is realized from. The `rations` entry always appears, so a realized
#: tree is always observably different from the pending one; the
#: `steel_bar` coin flip is what makes two slots' trees differ, so the
#: cross-process comparison is about each slot's OWN context rather than
#: a constant.
CONTAINER_PROFILE_YAML = (
    "id: probe_crate_salvage\n"
    "quantity_multiplier:\n"
    "  min: 1\n"
    "  max: 2\n"
    "entries:\n"
    "  - item: steel_bar\n"
    "    chance: 0.5\n"
    "    quantity_factor: 2\n"
    "  - item: rations\n"
    "    chance: 1.0\n"
    "    quantity_factor: 1\n"
)

#: One per land chunk (the `dense_ruin` pattern this probe already uses
#: for the hidden-page dispatch phase), so a container entry is
#: guaranteed at the synchronous centre chunk (0,0). THREE shells per
#: location, so even a world whose loaded region holds a single crate
#: ruin has one shell to realize in place, one to pick up, and one left
#: pending across the save. A fixed `position` keeps them on a known
#: tile.
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
    "profile: probe_crate_salvage, count: 3, position: {x: 0, y: 0} }\n"
)

#: The SAME location id over an EMPTY contents list. Registered by the
#: load-refusal phase in place of the real one, which is the only way to
#: reach the state that phase is about: a save carrying a pending slot
#: whose profile this build no longer registers.
#:
#: The real fixture cannot do it. Its container entry resolves its
#: profile against the live registry at LOAD (Engine.Asset.YamlLocations'
#: containerContentErrors), so registering it without the profile
#: registers nothing at all — and then the load would fail on the
#: location DEF being unknown, which is a different rejection masking the
#: one under test. The def id is what missingLocationDefReferences
#: resolves; the pending slot rides on the saved INSTANCE, not on today's
#: contents list. So this models exactly the real case: a world
#: materialized when the profile existed, loaded against a build whose
#: content set has moved on.
CONTAINER_LOCATION_NOPROFILE_YAML = (
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
    "    contents: []\n"
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


def write_container_fixtures(art: RunArtifacts) -> tuple[str, str, str, str]:
    """Stage this scenario's four fixtures into the invocation's own
    fixtures directory, and answer their paths with the first three in
    REGISTRATION order — items, then the profile, then the location —
    followed by the container-free twin, which a LATER phase registers in
    place of that location rather than beside it.

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
    noprofile_yaml = art.fixture("crate_location_noprofile")
    with open(noprofile_yaml, "w") as fh:
        fh.write(CONTAINER_LOCATION_NOPROFILE_YAML)
    return item_yaml, profile_yaml, location_yaml, noprofile_yaml


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
    `{gid, instance, weight}` rows. Trees are compared through
    `_ground_tree`, never through these.

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
        failures.append("a slot came back REALIZED before anything "
                        "realized it")
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


def check_no_respawn(args, state, failures: list[str]) -> None:
    """Revisiting the same chunks mints no second shell -- and every
    shell's COMPLETE pending tree is recorded, keyed by its location
    slot, just before the façade takes the pristine save the
    opposite-order processes load.

    Every comparison in this owner is of EXACT trees
    (`item.debugGroundTree` / `item.debugHeldTree`: ordered, every
    physical field, ids masked), never of `contentsKey`, which is a
    sorted grouping key."""
    send(args.port, "return world.loadChunksInRegion(-1,-1,1,1)")
    time.sleep(1.0)
    shells = _shell_rows(args.port, "probe_pending_crate")
    if len(shells) != state.crate_shells:
        failures.append(
            f"revisiting respawned shells: {state.crate_shells} before, "
            f"{len(shells)} after")
        return
    print("PASS: revisiting the crate ruins respawned no shell")

    slots = _slots_by_shell(args.port)
    if len(shells) < 3 or any(s["instance"] not in slots for s in shells):
        failures.append(
            f"need at least three bound shells to realize one in place, pick "
            f"one up and keep one pending; found {len(shells)}")
        return
    pending = {_slot_key(slots[s["instance"]]): _ground_tree(args.port, s["gid"])
               for s in shells}
    if any(t is None for t in pending.values()):
        failures.append("item.debugGroundTree could not describe a pending "
                        "shell")
        return
    state.crate_pending_trees = pending


def check_realize(args, state, failures: list[str]) -> None:
    """(#2510) One shell is realized IN PLACE through `item.realizeGround`,
    a second is realized by being picked up, and the rest are left
    pending for the save. Each realized tree is recorded COMPLETE, keyed
    by its location slot, for the reload and the opposite-order
    processes to reproduce exactly."""
    slots = _slots_by_shell(args.port)
    shells = sorted(_shell_rows(args.port, "probe_pending_crate"),
                    key=lambda s: s["gid"])
    ground, held = shells[0], shells[1]
    pending = {s["gid"]: state.crate_pending_trees[_slot_key(slots[s["instance"]])]
               for s in (ground, held)}

    # Explicit realization: in place, exactly once.
    before = pending[ground["gid"]]
    first = _realize(args.port, ground["gid"])
    after = _ground_tree(args.port, ground["gid"])
    row = [s for s in _shell_rows(args.port, "probe_pending_crate")
           if s["gid"] == ground["gid"]]
    if (first == "realized" and after and row
            and row[0]["instance"] == ground["instance"]
            and _own(after) == _own(before)
            and after["contents"] != before["contents"]
            and row[0]["weight"] > ground["weight"]):
        print("PASS: item.realizeGround realized a pending shell IN PLACE -- "
              "same ground id and instance id, the shell's own values exactly "
              "as they were, its cargo visible at once in its tree and its "
              "recursive weight")
    else:
        failures.append(
            "item.realizeGround should realize a pending shell in place; "
            f"answered {first!r}, tree before {before}, after {after}, "
            f"row {row}")
        return
    second = _realize(args.port, ground["gid"])
    again = _ground_tree(args.port, ground["gid"])
    if second == "already-realized" and again == after:
        print("PASS: a second item.realizeGround answered already-realized "
              "and left the exact tree untouched")
    else:
        failures.append(
            f"a repeat realization answered {second!r} with tree {again}, "
            f"expected already-realized and {after}")
    ground_slot = _slot_key(slots[ground["instance"]])
    state.crate_ground_shell = ground["instance"]
    state.crate_realized_trees[ground_slot] = after

    # The pickup backstop: a still-pending shell arrives in the
    # inventory already realized.
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
    before = pending[held["gid"]]
    picked = send(args.port,
                  f"return item.pickupGround({uid}, {held['gid']})").strip()
    carried = _held_tree(args.port, uid, held["instance"])
    if (picked == "true" and carried
            and _own(carried) == _own(before)
            and carried["contents"] != before["contents"]):
        print("PASS: picking up a PENDING shell realized it first -- it "
              "arrived in the inventory carrying its cargo, its own values "
              "untouched")
    else:
        failures.append(
            "a pending shell's pickup should realize it and move it; "
            f"pickupGround={picked!r}, carried={carried}, pending tree "
            f"{before}")
        return
    held_slot = _slot_key(slots[held["instance"]])
    state.crate_held_shell = held["instance"]
    state.crate_holder_uid = uid
    state.crate_realized_trees[held_slot] = carried

    _check_slot_states(args.port, state, failures, "after realizing two")


def _check_slot_states(port: int, state, failures: list[str],
                       when: str) -> None:
    """Every bound slot is in the state the scenario left it: the two
    realized shells latched with their profile DISCARDED, every other
    one still pending and still naming the authored profile."""
    realized_ids = {state.crate_ground_shell, state.crate_held_shell}
    wrong = []
    for s in _crate_slots(port, CRATE_PAGE):
        if s.get("bound", -1) <= 0:
            continue
        want_realized = s["bound"] in realized_ids
        want_profile = None if want_realized else "probe_crate_salvage"
        if bool(s.get("realized")) != want_realized or s.get("profile") != want_profile:
            wrong.append(s)
    if wrong:
        failures.append(f"slot states are wrong {when}: {wrong}")
    else:
        print(f"PASS: {when}, exactly the realized slots are latched with "
              "their profile discarded, and every other one is pending")


def _slots_by_shell(port: int) -> dict[int, dict]:
    """Bound slot rows keyed by the shell instance id each one names."""
    return {s["bound"]: s for s in _crate_slots(port, CRATE_PAGE)
            if s.get("bound", -1) > 0}


def _slot_key(slot: dict) -> str:
    """The realization context's location half, `<instance>:<slot>` --
    stable across processes, where the shell's own instance id is not."""
    return f"{slot['instance']}:{slot['slot']}"


def _realize(port: int, gid: int) -> str:
    """`item.realizeGround` on the crate page, its answer unquoted."""
    return send(port, f"return item.realizeGround({gid}, '{CRATE_PAGE}')"
                ).strip().strip('"')


def _ground_tree(port: int, gid: int) -> dict | None:
    """The EXACT tree of ground item `gid` on the crate page."""
    t = send_json(port, f"return item.debugGroundTree({gid}, '{CRATE_PAGE}')")
    return t if isinstance(t, dict) else None


def _held_tree(port: int, uid: int, instance: int) -> dict | None:
    """The EXACT tree of the item `uid` carries with id `instance`."""
    t = send_json(port, f"return item.debugHeldTree({uid}, {instance})")
    return t if isinstance(t, dict) else None


def _own(tree: dict) -> dict:
    """A tree's ROOT values: every field but its contents."""
    return {k: v for k, v in tree.items() if k != "contents"}


def check_shell_survived_reload(args, state, failures: list[str]) -> None:
    """Every slot comes back from save -> quit -> fresh process -> load in
    the state it was saved in, both realized trees come back EXACTLY, and
    (#2510) realization stays exactly-once across the round trip."""
    slots = _crate_slots(args.port, CRATE_PAGE)
    bound = [s for s in slots if s.get("bound", -1) > 0]
    if len(bound) != state.crate_slots:
        failures.append(
            f"the load restored {len(bound)} bound container slot(s), "
            f"expected {state.crate_slots}")
        return
    _check_slot_states(args.port, state, failures, "after the reload")

    shells = _shell_rows(args.port, "probe_pending_crate")
    if len(shells) != state.crate_shells - 1:
        failures.append(
            f"the load restored {len(shells)} shell(s) on the ground, "
            f"expected {state.crate_shells - 1} (one was picked up)")
        return
    slot_of = _slots_by_shell(args.port)
    if sorted(s["instance"] for s in shells) == sorted(
            b for b in slot_of if b != state.crate_held_shell):
        print("PASS: every restored ground shell is still the OUTER GROUND "
              "item its own slot names")
    else:
        failures.append("the restored slots and ground shells name "
                        "different item instances")
        return

    ground = [s for s in shells if s["instance"] == state.crate_ground_shell]
    ground_tree = _ground_tree(args.port, ground[0]["gid"]) if ground else None
    carried = _held_tree(args.port, state.crate_holder_uid,
                         state.crate_held_shell)
    ground_slot = _slot_key(slot_of[state.crate_ground_shell])
    held_slot = _slot_key(slot_of[state.crate_held_shell])
    if (ground_tree == state.crate_realized_trees.get(ground_slot)
            and carried == state.crate_realized_trees.get(held_slot)):
        print("PASS: both realized trees -- one on the ground, one in an "
              "inventory -- round-tripped EXACTLY, order and every field")
    else:
        failures.append(
            f"a realized tree changed across save/load: ground "
            f"{ground_tree}, carried {carried}, saved "
            f"{state.crate_realized_trees}")
        return

    # Exactly once ACROSS the round trip: the restored latch is honoured.
    answer = _realize(args.port, ground[0]["gid"])
    again = _ground_tree(args.port, ground[0]["gid"])
    if answer == "already-realized" and again == ground_tree:
        print("PASS: after the reload a realized shell answers "
              "already-realized and its exact tree is untouched")
    else:
        failures.append(
            f"after the reload a realized shell answered {answer!r} with "
            f"tree {again}, expected already-realized and {ground_tree}")

    # …and a shell still pending after the load realizes now, into the
    # tree its slot's context determines -- which the opposite-order
    # processes must reproduce.
    pending = [s for s in shells
               if s["instance"] not in (state.crate_ground_shell,
                                        state.crate_held_shell)]
    target = pending[0]
    before = _ground_tree(args.port, target["gid"])
    answer = _realize(args.port, target["gid"])
    after = _ground_tree(args.port, target["gid"])
    if (answer == "realized" and before and after
            and _own(after) == _own(before)
            and after["contents"] != before["contents"]):
        state.crate_realized_trees[_slot_key(slot_of[target["instance"]])] = \
            after
        print("PASS: a shell still pending after the reload realized now, "
              "its own values untouched")
    else:
        failures.append(
            f"a pending shell after the reload answered {answer!r} with "
            f"tree {after} (was {before}), expected a fresh realization")


def visit_crate_chunks(port: int, reverse: bool) -> None:
    """Visit the restored crate world's 3x3 region ONE CHUNK AT A TIME,
    in row-major order or its exact reverse."""
    chunks = [(cx, cy) for cy in (-1, 0, 1) for cx in (-1, 0, 1)]
    for cx, cy in (reversed(chunks) if reverse else chunks):
        send(port, f"return world.loadChunksInRegion({cx},{cy},{cx},{cy})")
        send(port, "return world.waitForChunks(30)", timeout=35)


def check_realization_order(args, state, failures: list[str],
                            label: str, reverse: bool) -> None:
    """#2510 requirement 8, in a fresh process that LOADED the pristine
    save -- taken after the shells spawned and before anything was
    realized -- and then visited its chunks in `label` order.

    Like for like first: every shell's COMPLETE pending tree (its own
    rolled values included) must equal the one the crate world recorded
    for the same slot, which the shared save is what guarantees. Then,
    realized here in that order too, every slot's COMPLETE realized tree
    must equal the one every earlier process realized for it -- in place,
    by pickup, and after a reload -- with the shell's own values
    untouched, and the first visit order's trees must be reproduced
    exactly by the second."""
    slot_of = _slots_by_shell(args.port)
    shells = sorted(_shell_rows(args.port, "probe_pending_crate"),
                    key=lambda s: s["gid"], reverse=reverse)
    if len(slot_of) != state.crate_slots or len(shells) != state.crate_shells:
        failures.append(
            f"#2510 ({label}): the pristine save restored {len(slot_of)} "
            f"bound slot(s) and {len(shells)} shell(s), the crate world had "
            f"{state.crate_slots} and {state.crate_shells}")
        return
    before = {s["gid"]: _ground_tree(args.port, s["gid"]) for s in shells}
    pending = {_slot_key(slot_of[s["instance"]]): before[s["gid"]]
               for s in shells}
    if pending != state.crate_pending_trees:
        failures.append(
            f"#2510 ({label}): the restored pending trees differ from the "
            f"crate world's, so the comparison below would not be like for "
            f"like: {pending} vs {state.crate_pending_trees}")
        return
    print(f"PASS: #2510 fresh process, {label} -- every shell's COMPLETE "
          "pending tree matches the crate world's slot for slot")

    realized = {}
    for s in shells:
        if _realize(args.port, s["gid"]) != "realized":
            failures.append(f"#2510 ({label}): shell {s} did not realize")
            return
        after = _ground_tree(args.port, s["gid"])
        if after is None or _own(after) != _own(before[s["gid"]]):
            failures.append(
                f"#2510 ({label}): realizing shell {s} changed its own "
                f"values: {before[s['gid']]} -> {after}")
            return
        realized[_slot_key(slot_of[s["instance"]])] = after

    mismatched = {k: (realized.get(k), v)
                  for k, v in state.crate_realized_trees.items()
                  if realized.get(k) != v}
    if mismatched:
        failures.append(
            f"#2510 ({label}): a slot realized differently from the crate "
            f"world and its reload: {mismatched}")
    else:
        print(f"PASS: #2510 fresh process, {label} -- the "
              f"{len(state.crate_realized_trees)} slot(s) realized before "
              "(in place, by pickup, and after a reload) realized into "
              "exactly the same COMPLETE trees, every shell's own values "
              "untouched")
    if not state.crate_order_trees:
        state.crate_order_trees = realized
    elif realized == state.crate_order_trees:
        print(f"PASS: #2510 fresh process, {label} -- all {len(realized)} "
              "shells realized into exactly the complete trees of the "
              "opposite visit order")
    else:
        failures.append(
            f"#2510 ({label}): the realized trees differ from the opposite "
            f"visit order's: {realized} vs {state.crate_order_trees}")


def register_without_profile(port: int, item_yaml: str,
                             noprofile_yaml: str) -> None:
    """The crate item and a container-free `crate_ruin`, and NOTHING
    else: this build knows the item and the location def, but has never
    heard of `probe_crate_salvage`."""
    load_fixture_yaml(port, "engine.loadItemYaml", item_yaml)
    load_fixture_yaml(port, "engine.loadLocationYaml", noprofile_yaml)


def check_missing_profile_refuses_load(args, state, art,
                                       failures: list[str]) -> None:
    """A save whose PENDING slot names a profile this build no longer
    registers is refused BEFORE the replacement session is staged, and
    the old session is left exactly as it was.

    Requirement 8's other half, and the one no hspec group can reach: the
    check is wired into `continueLoad`'s `allMissing` gate, so only a real
    `engine.loadSave` against a real envelope can show that the gate runs
    at all, aborts, and leaves the live session alone.

    The refusal is SYNCHRONOUS — `engine.loadSave` itself answers false.
    #763's asynchrony begins at staging and publication, which this gate
    sits in front of, so there is no request to wait on and nothing was
    ever staged.
    """
    before_page = send(args.port, "return world.getActiveWorldId()").strip()
    before_locations = send(
        args.port,
        f"return #(world.listPlacedLocations('{CRATE_PAGE}') or {{}})").strip()
    accepted = send(args.port,
                    f"return engine.loadSave('{state.crate_slot_name}')").strip()
    if accepted == "false":
        print("PASS: the load was refused outright — nothing was staged and "
              "no replacement session was published")
    else:
        failures.append(
            "a save whose pending container slot names an unregistered "
            f"profile should be refused; engine.loadSave returned {accepted!r}")

    # The diagnostic must be actionable: page, instance, slot and the
    # profile id, so an operator can tell WHICH crate lost its profile.
    log_text = open(art.engine_log, errors="replace").read()
    attributed = [line for line in log_text.splitlines()
                  if "loadSave rejected" in line
                  and "probe_crate_salvage" in line]
    if attributed and all(
            token in attributed[-1]
            for token in ("pending container shell", "slot",
                          "unknown loot profile", f"page '{CRATE_PAGE}'")):
        print("PASS: the rejection names the page, the location, the slot "
              "and the unresolved profile id")
    else:
        failures.append(
            "the rejection should name the page, instance, slot and profile "
            f"id; found {attributed[-1][:200] if attributed else '<no such line>'}")

    # "nothing changed" is the message's own claim, so check it: the
    # active page is the one this process generated, and the saved crate
    # page never became live.
    after_page = send(args.port, "return world.getActiveWorldId()").strip()
    after_locations = send(
        args.port,
        f"return #(world.listPlacedLocations('{CRATE_PAGE}') or {{}})").strip()
    if after_page == before_page and after_locations == before_locations:
        print("PASS: the refused load left the old session live and "
              "unchanged — the saved crate page never became live")
    else:
        failures.append(
            "the refused load changed the live session: active page "
            f"{before_page!r} -> {after_page!r}, crate-page locations "
            f"{before_locations!r} -> {after_locations!r}")

    # …and the failure is recorded against the content-validation phase,
    # not against an earlier one — which is what tells an operator the
    # save decoded fine and was refused on its CONTENT.
    status = send_json(args.port, "return engine.getLoadStatus()")
    phase = status.get("failedAtPhase") if isinstance(status, dict) else None
    if phase == "LoadContentValidated":
        print("PASS: the failure is recorded at the content-validation "
              "phase")
    else:
        failures.append(
            "the refusal should be recorded at LoadContentValidated "
            f"(got {phase!r})")
