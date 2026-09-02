#!/usr/bin/env python3
"""Content and persistence: what a ruin spawns, and that it stays put
(#2095).

One owner for ruin geometry, content and encounter spawning,
registered-item validation, per-instance loot stability under same and
reversed visit order (#948), and the fresh-process no-respawn round trip
(#90/#91). The assertions are `tools/location_content_probe.py`'s own,
moved rather than rewritten; what is new is that they arrive through an
explicit `ScenarioState` instead of a shared 980-line local scope.

Nothing here boots an engine: the facade opens each process and hands
these functions the live port.
"""
from __future__ import annotations

import time

from probelib import send

from .engine_queries import (GROUND_PER_RUIN, floor_tex, ground_items,
                             has_floor, load_chunk, placed, placed_ready,
                             registered_item_names, ruin_geometry,
                             spawn_counts, unregistered_item_ids)
from .invocation import ScenarioState


def loot_by_instance(port: int, page: str) -> dict[int, list[str]]:
    """Ground-item defName multiset per placed-location INSTANCE (#948),
    attributed by the instance's own absolute bounds (#777).

    Attribution by bounds is unambiguous here: ruin_small declares
    min_spacing 5 chunks (data/locations/ruin_small.yaml), so no two
    ruin footprints can overlap. Multiset, not sequence: ground items
    carry no roll order, and their scatter COORDINATES are still
    math.random-driven by design — this issue pins the selected
    item-definition sequence, which the hspec fixed vectors cover
    directly. Keyed by the stable instance id so the comparison is
    immune to placement/query ORDER."""
    items = ground_items(port)
    out: dict[int, list[str]] = {}
    for e in placed(port, page):
        b = e.get("bounds") or {}
        if not b or "instance_id" not in e:
            continue
        out[e["instance_id"]] = sorted(
            it.get("defName", "?") for it in items
            if b["min_x"] <= it.get("x", 1e9) <= b["max_x"]
            and b["min_y"] <= it.get("y", 1e9) <= b["max_y"])
    return out


def stamp_ruins(port: int, ruins: list[dict], reverse: bool = False) -> None:
    """Load every ruin's chunk and wait for its geometry + contents."""
    order = list(reversed(ruins)) if reverse else list(ruins)
    for e in order:
        load_chunk(port, e["cx"], e["cy"])
    for _ in range(60):
        if all(has_floor(port, e["gx"], e["gy"]) for e in order):
            break
        time.sleep(0.5)
    want = GROUND_PER_RUIN * len(order)
    for _ in range(20):
        if spawn_counts(port)["ground_total"] >= want:
            break
        time.sleep(0.5)


def observe_initial_content(args, state: ScenarioState,
                            failures: list[str]) -> None:
    """The initial process's content half (#90/#91): what the placed
    ruins are, what they spawned, and what shape they are in.

    Records the ruins, spawn counts, per-instance loot and geometry the
    later processes compare against. Returns with `state.ruins` empty
    when the world placed none, which is what makes every dependent
    phase skip.
    """
    la = placed_ready(args.port)
    ruins = [e for e in la if e["id"] == "ruin_small"]
    state.placed_all = la
    state.ruins = ruins
    print(f"world (seed {args.seed}): {len(ruins)} ruin_small placed")
    if not ruins:
        failures.append("no ruin_small placed — cannot test content spawning")
        return

    for e in ruins:
        load_chunk(args.port, e["cx"], e["cy"])
    n = 0
    for _ in range(60):
        n = sum(1 for e in ruins if has_floor(args.port, e["gx"], e["gy"]))
        if n == len(ruins):
            break
        time.sleep(0.5)
    if n != len(ruins):
        failures.append(f"only {n}/{len(ruins)} ruin(s) stamped")

    # Content spawning has its own settle time — poll briefly
    # for the expected ground-item count.
    # Each ruin (#91, #921, #916): 2 loot-table ground items and
    # its one persisted uniform 0..3 nomad roll; no fixed items or
    # buildings.
    want_ground = GROUND_PER_RUIN * len(ruins)
    want_nomads = sum(int((e.get("encounter") or {}).get(
        "rolled_count", 0)) for e in ruins)
    counts1 = {}
    for _ in range(20):
        counts1 = spawn_counts(args.port)
        current = {int(e["instance_id"]): e for e in placed(args.port, "wa")}
        rosters_ready = all(
            bool((current.get(int(e["instance_id"]), {}).get("encounter")
                  or {}).get("roster_complete")) for e in ruins)
        if (counts1["ground_total"] >= want_ground
                and counts1["nomad_primitive"] >= want_nomads
                and rosters_ready):
            break
        time.sleep(0.5)
    print(f"  spawned: {counts1}")

    if counts1["ground_total"] == want_ground:
        print(f"PASS: {want_ground} ground item(s) spawned "
              f"({GROUND_PER_RUIN} loot_table roll(s) per ruin, "
              f"no guaranteed item)")
    else:
        failures.append(
            f"expected {want_ground} ground item(s), got "
            f"{counts1['ground_total']} ({counts1['ground_by_name']})")

    current = {int(e["instance_id"]): e for e in placed(args.port, "wa")}
    roster_errors = []
    for ruin in ruins:
        iid = int(ruin["instance_id"])
        encounter = (current.get(iid, {}).get("encounter") or {})
        rolled = int(encounter.get("rolled_count", -1))
        occupants = encounter.get("occupants") or []
        homes = {(o.get("home_x"), o.get("home_y"))
                 for o in occupants}
        bounds = current.get(iid, {}).get("bounds") or {}
        homes_in_bounds = all(
            bounds.get("min_x") <= o.get("home_x") <= bounds.get("max_x")
            and bounds.get("min_y") <= o.get("home_y") <= bounds.get("max_y")
            for o in occupants)
        if (not encounter.get("roster_complete")
                or len(occupants) != rolled
                or len(homes) != rolled
                or not homes_in_bounds):
            roster_errors.append((iid, rolled, len(occupants),
                                  len(homes), homes_in_bounds,
                                  encounter.get("roster_complete")))
    if (counts1["acolyte"] == 0
            and counts1["nomad_primitive"] == want_nomads
            and counts1["cargo_hold_S"] == 0
            and not roster_errors):
        print(f"PASS: persisted encounter rolls spawned exactly "
              f"{want_nomads} nomad(s), with complete per-ruin rosters "
              f"on distinct in-bounds home tiles and no unrelated "
              f"units/buildings")
    else:
        failures.append(
            f"ruin_small encounter mismatch: expected {want_nomads} nomads "
            f"and complete rosters, got counts={counts1}, "
            f"roster_errors={roster_errors}")

    action_policy = send(
        args.port,
        "local A=require('scripts.unit_ai_actions'); return "
        "tostring(A.has('nomad_primitive','ruin_engage')) .. ',' .. "
        "tostring(A.has('nomad_primitive','engage')) .. ',' .. "
        "tostring(A.has('nomad_primitive','attack_target'))")
    if action_policy.strip('"') == "true,false,true":
        print("PASS: nomads acquire targets only through ruin_engage "
              "while retaining universal attack execution")
    else:
        failures.append(
            "nomad action inventory bypasses encounter acquisition: "
            f"{action_policy!r}")

    # #921: the ruin guarantees NOTHING specific. `radio` and
    # `canteen_steel_2l` (spawn-only starting equipment) were the
    # two entries removed, and they are absent from ruin_common
    # too — so no ruin content on this page may be either. This
    # is the direct inverse of the assertion that used to REQUIRE
    # one of each per ruin; it fails if they are reinstated as
    # fixed entries or quietly added to the loot table.
    spawn_only = {d: counts1["ground_by_name"][d]
                  for d in ("radio", "canteen_steel_2l")
                  if counts1["ground_by_name"].get(d)}
    if not spawn_only:
        print("PASS: no spawn-only equipment (radio, canteen_steel_2l) "
              "in ruin content — nothing is guaranteed")
    else:
        failures.append(
            f"spawn-only equipment appeared in ruin content: {spawn_only}")

    # #948 baseline: which loot each STABLE ruin instance owns.
    # Captured before the discovery owner runs on this same page and
    # spawns its synthetic units (which are units, not ground items),
    # so the two loot-stability processes compare a pure content-spawn
    # result.
    loot1 = loot_by_instance(args.port, "wa")
    print(f"  loot by instance: {loot1}")
    if len(loot1) == len(ruins) and all(loot1.values()):
        print(f"PASS: every ruin instance owns an attributable "
              f"loot multiset ({len(loot1)} instance(s))")
    else:
        failures.append(
            f"could not attribute loot to every ruin instance: {loot1}")

    registered = registered_item_names(args.port)
    unexpected = unregistered_item_ids(set(counts1["ground_by_name"]), registered)
    if not unexpected:
        print("PASS: all spawned ground items resolve to registered "
              "item definitions (item.listDefs(), every loot-table "
              "roll)")
    else:
        failures.append(
            f"unexpected ground item id(s) not in the item registry: {unexpected}")

    # #91 geometry: a ruin is a BREACHED room — all 25 floors,
    # some but not all of the 20 perimeter wall segments, and
    # exactly 3 of the 4 corner posts.
    geoms1 = {}
    for e in ruins:
        f, w, p = ruin_geometry(args.port, e["gx"], e["gy"])
        geoms1[(e["gx"], e["gy"])] = (f, w, p)
        if f == 25 and 1 <= w <= 18 and p == 3:
            print(f"PASS: ruin at ({e['gx']},{e['gy']}) is breached "
                  f"(floors {f}/25, walls {w}/20, posts {p}/4)")
        else:
            failures.append(
                f"ruin at ({e['gx']},{e['gy']}) geometry wrong: "
                f"floors {f}/25 (want 25), walls {w}/20 (want 1..18), "
                f"posts {p}/4 (want 3)")

    # #91 variant: the pieces persist the damaged texture path.
    tex = floor_tex(args.port, ruins[0]["gx"], ruins[0]["gy"])
    if "/damaged/" in tex:
        print(f"PASS: ruin floor carries the damaged variant art ({tex})")
    else:
        failures.append(f"ruin floor texture is not the damaged variant: {tex}")

    state.counts1 = counts1
    state.loot1 = loot1
    state.geoms1 = geoms1


def check_loot_stability(args, state: ScenarioState, failures: list[str],
                         label: str, reverse: bool) -> None:
    """#948, in a fresh process on the same seed: every stable ruin
    instance owns the loot multiset it owned initially, whether the
    ruins are visited in the initial order or the exact reverse.

    Reversing must not swap or shift which ruin owns which reward, which
    is what proves the draw is per-instance rather than a shared stream.
    """
    again = [e for e in placed_ready(args.port) if e["id"] == "ruin_small"]
    if len(again) != len(state.ruins):
        failures.append(
            f"#948 ({label}): fresh process placed {len(again)} "
            f"ruin(s), phase 1 placed {len(state.ruins)}")
        return
    stamp_ruins(args.port, again, reverse=reverse)
    loot_n = loot_by_instance(args.port, "wa")
    if loot_n == state.loot1:
        print(f"PASS: #948 fresh process, same seed, {label} — every "
              f"ruin instance owns the same loot ({loot_n})")
    else:
        failures.append(
            f"#948 ({label}): per-instance loot differs from phase 1: "
            f"phase1={state.loot1} now={loot_n}")


def check_reload_counts_and_loot(args, state: ScenarioState,
                                 failures: list[str]) -> None:
    """Phase 2's content half: revisiting the reloaded chunks does not
    respawn contents, and nothing was re-rolled."""
    for e in state.ruins:
        load_chunk(args.port, e["cx"], e["cy"])
    # No settle-time poll needed here: a respawn would be immediate
    # and permanent, unlike the initial spawn's queue latency.
    time.sleep(2.0)
    counts2 = spawn_counts(args.port)
    print(f"  after reload: {counts2}")
    if counts2 == state.counts1:
        print("PASS: reload does not respawn contents (counts unchanged)")
    else:
        failures.append(
            f"contents respawned on reload: before={state.counts1} after={counts2}")

    # #948 + #90: the one-time flag means nothing is re-rolled,
    # so each instance keeps the EXACT loot it was first given —
    # through save -> quit -> fresh process -> load -> chunk
    # reload. (The chunks above were evicted with the process and
    # re-loaded after the transaction published.)
    loot2 = loot_by_instance(args.port, "wa")
    if loot2 == state.loot1:
        print("PASS: per-instance loot survived save -> quit -> restart "
              "-> load -> chunk reload unchanged (never re-rolled)")
    else:
        failures.append(
            f"per-instance loot changed across save/load: "
            f"before={state.loot1} after={loot2}")


def check_geometry_replay(args, state: ScenarioState,
                          failures: list[str]) -> None:
    """Phase 2's #91 half: the breach pattern replays exactly from the
    edit log, and the pieces still resolve to the damaged variant art."""
    # #91: the damaged geometry replays identically from the edit
    # log (same breach pattern — the builder did NOT re-run and
    # re-roll), and the pieces still resolve to the damaged
    # variant art (texture identity rides the structure palette).
    for e in state.ruins:
        g2 = ruin_geometry(args.port, e["gx"], e["gy"])
        g1 = state.geoms1.get((e["gx"], e["gy"]))
        if g2 == g1:
            print(f"PASS: ruin at ({e['gx']},{e['gy']}) replayed its "
                  f"breach pattern exactly (floors/walls/posts {g2})")
        else:
            failures.append(
                f"ruin at ({e['gx']},{e['gy']}) changed shape on "
                f"reload: before={g1} after={g2}")
    tex = floor_tex(args.port, state.ruins[0]["gx"], state.ruins[0]["gy"])
    if "/damaged/" in tex:
        print(f"PASS: damaged variant survived save/load ({tex})")
    else:
        failures.append(
            f"ruin floor texture lost the damaged variant on reload: {tex}")
