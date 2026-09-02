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

from .engine_queries import (GROUND_PER_RUIN, LOOT_ROLLS_PER_RUIN,
                             SIGNIFICANT_ITEM, SIGNIFICANT_PER_RUIN,
                             clearance_state, floor_tex, ground_id_of_instance,
                             ground_items, has_floor, load_chunk, placed,
                             placed_ready, registered_item_names,
                             ruin_geometry, significant_state, spawn_counts,
                             spawn_unit, unregistered_item_ids)
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
    immune to placement/query ORDER.

    #917's guaranteed significant item is EXCLUDED. It is not loot: it
    is authored, identical for every ruin, and its presence or absence
    says nothing about the loot table's draw — including it would let a
    lost roll hide behind a constant. It is checked on its own terms by
    `check_significant_contents` below."""
    items = ground_items(port)
    out: dict[int, list[str]] = {}
    for e in placed(port, page):
        b = e.get("bounds") or {}
        if not b or "instance_id" not in e:
            continue
        out[e["instance_id"]] = sorted(
            it.get("defName", "?") for it in items
            if it.get("defName") != SIGNIFICANT_ITEM
            and b["min_x"] <= it.get("x", 1e9) <= b["max_x"]
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


def check_significant_contents(args, ruins: list[dict],
                              failures: list[str]) -> None:
    """#917: guaranteed significant contents and the COMPOUND clearance
    predicate, on the initial process's freshly spawned ruins.

    Everything here reads the engine's own reported fields
    (world.listPlacedLocations' `significant` array and its three
    clearance flags) rather than recomputing the predicate probe-side,
    so a divergence between what the engine decided and what this
    asserts is impossible to hide.

    Leaves one ruin's guaranteed item picked up by a HOSTILE unit, which
    is deliberate: the durable `taken` latch is what the later reload
    phases then find unchanged.
    """
    sig1 = significant_state(args.port, "wa")
    print(f"  significant obligations: {sig1}")

    # Every placed ruin owes EXACTLY one, bound to a real physical item
    # id, and none is taken yet. The bound id is the proof the content
    # spawn registered provenance rather than merely dropping an item on
    # the floor.
    sig_errors = []
    for ruin in ruins:
        iid = int(ruin["instance_id"])
        rows = sig1.get(iid) or []
        if (len(rows) != SIGNIFICANT_PER_RUIN
                or any(r.get("item") != SIGNIFICANT_ITEM for r in rows)
                or any(r.get("item_instance_id") is None for r in rows)
                or any(r.get("taken") for r in rows)):
            sig_errors.append((iid, rows))
    if not sig_errors:
        print(f"PASS: every ruin owes exactly {SIGNIFICANT_PER_RUIN} "
              f"guaranteed '{SIGNIFICANT_ITEM}', each bound to its own "
              f"spawned item instance and none taken")
    else:
        failures.append(f"significant obligation mismatch: {sig_errors}")

    # One physical identity per obligation, across the page. Two
    # obligations naming one item would let a single pickup clear a
    # location whose own item was never taken — the session-wide rule
    # World.Save.Integrity enforces at the save boundary, checked here
    # on the live session.
    bound_ids = [r.get("item_instance_id") for rows in sig1.values()
                 for r in rows if r.get("item_instance_id") is not None]
    if len(bound_ids) == len(set(bound_ids)):
        print(f"PASS: each obligation names a DISTINCT physical item "
              f"({len(bound_ids)} bound)")
    else:
        failures.append(
            f"one physical item is owed by several obligations: {bound_ids}")

    # A ruin with a live nomad roster is NOT cleared however its item is
    # handled, and a zero-roll ruin is not cleared either while its item
    # is untaken — requirement 5's conjunction, read straight off the
    # engine.
    clr1 = clearance_state(args.port, "wa")
    unsatisfied = [iid for iid, (_, authors, satisfied, _)
                   in clr1.items() if authors and satisfied]
    if not unsatisfied:
        print("PASS: no ruin's clearance predicate is satisfied while its "
              "guaranteed item is still on the floor — including any "
              "zero-nomad roll, whose encounter half is already complete")
    else:
        failures.append(
            f"clearance satisfied with an untaken guaranteed item: "
            f"{[(i, clr1[i]) for i in unsatisfied]}")

    # (Requirement 4 — that incidental salvage participates in nothing —
    # is proved at two cheaper tiers instead: the pure "Location
    # significant contents" spec and the IO-level "compound clearance
    # with significant contents" one both pick up an unowned item
    # through the real boundary and assert nothing moves. Repeating it
    # HERE would need a player-faction looter standing in a ruin, which
    # would discover it and invalidate the sight-based discovery checks
    # the knowledge owner runs on this same page.)

    # A NON-PLAYER faction's pickup latches the same durable state
    # (requirement 3): the ruin was looted whoever did it. Deliberately
    # a hostile unit, and deliberately a ruin with a LIVE roster where
    # one exists, so the latch is observed without the location
    # clearing — the two halves stay independent.
    occupied = next(
        (r for r in ruins
         if int((r.get("encounter") or {}).get("rolled_count", 0)) > 0),
        None)
    target = occupied or (ruins[0] if ruins else None)
    if target is None:
        failures.append("no ruin to test the taken latch against")
        return

    tiid = int(target["instance_id"])
    rows = sig1.get(tiid) or []
    phys = rows[0].get("item_instance_id") if rows else None
    gid = (ground_id_of_instance(args.port, phys)
           if phys is not None else None)
    if gid is None:
        failures.append(
            f"ruin {tiid}'s guaranteed item is not on the ground: phys={phys}")
        return

    thief = spawn_unit(args.port, "nomad_primitive",
                       int(target["gx"]), int(target["gy"]), "hostile", "wa")
    took = send(args.port,
                f"return item.pickupGround({thief}, {gid}) "
                f"and 'yes' or 'no'").strip('"')
    time.sleep(1.5)
    sig2 = significant_state(args.port, "wa")
    clr2 = clearance_state(args.port, "wa")
    latched = all(r.get("taken") for r in sig2.get(tiid, []))
    others_untouched = all(sig2.get(i) == sig1.get(i)
                           for i in sig1 if i != tiid)
    if took == "yes" and latched and others_untouched:
        print(f"PASS: a hostile unit's pickup latched ruin {tiid}'s "
              f"guaranteed item as taken, and no other ruin's obligations "
              f"moved")
    else:
        failures.append(
            f"non-player pickup did not latch as expected: "
            f"took={took!r} sig={sig2}")

    # …and with a live roster the location is STILL not cleared: the
    # encounter half is outstanding.
    if occupied is not None:
        if not clr2.get(tiid, (None, False, False, False))[2]:
            print(f"PASS: ruin {tiid} stays uncleared with its item taken "
                  f"but its roster alive — both halves are required")
        else:
            failures.append(
                f"ruin {tiid} cleared on the item alone while its roster is "
                f"alive: {clr2.get(tiid)}")

    # A second pickup attempt cannot reset the latch: the item is now in
    # an inventory, so there is no ground id to take, and the durable
    # state is unchanged either way.
    if significant_state(args.port, "wa") != sig2:
        failures.append("the taken latch moved after the pickup settled")
    else:
        print("PASS: the taken latch is stable once set")


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
    # Each ruin (#91, #921, #916, #917): 2 loot-table ground items,
    # ONE guaranteed significant item, and its persisted uniform 0..3
    # nomad roll; no buildings.
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
              f"({LOOT_ROLLS_PER_RUIN} loot_table roll(s) plus "
              f"{SIGNIFICANT_PER_RUIN} guaranteed significant item "
              f"per ruin)")
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

    # #921/#917: the ruin's INCIDENTAL half still guarantees nothing
    # specific. `radio` and `canteen_steel_2l` (spawn-only starting
    # equipment) were the two entries #921 removed, and they are absent
    # from ruin_common too — so no ruin content on this page may be
    # either. #917 added a guaranteed item deliberately distinct from
    # `radio` (D-6 in docs/expedition_gameplay_loop.md reserves it for
    # unit communication), so this assertion is unchanged by it and
    # still fails if either is reinstated as a fixed entry or quietly
    # added to the loot table.
    spawn_only = {d: counts1["ground_by_name"][d]
                  for d in ("radio", "canteen_steel_2l")
                  if counts1["ground_by_name"].get(d)}
    if not spawn_only:
        print("PASS: no spawn-only equipment (radio, canteen_steel_2l) "
              "in ruin content — the guaranteed item is neither")
    else:
        failures.append(
            f"spawn-only equipment appeared in ruin content: {spawn_only}")

    check_significant_contents(args, ruins, failures)

    # The pickup that check just made moved one ground item into a unit,
    # so the spawn census the reload phase compares against has to be
    # re-taken here. That phase's claim is that a RELOAD respawns
    # nothing — a delta across the reload — not that the original
    # numbers are any particular value, and those were already asserted
    # above against want_ground/want_nomads.
    counts1 = spawn_counts(args.port)
    print(f"  census after the significant pickup: {counts1}")

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
