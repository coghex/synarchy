#!/usr/bin/env python3
"""Discovery and per-unit knowledge: who has seen a location, and who
remembers it (#2095).

One owner for the CARTOGRAPHIC layer (#780/#1230 sight-based player
discovery, and the exactly-once `location_discovery` event) and the
EXPERIENTIAL layer beside it (#915 per-unit memories, the dangling
reference the load reconciles away, their save/load survival, and the
same-instance-id isolation two same-seed pages produce). Neither derives
from the other, which is why they share one owner: every check here
reads both, and asserts that acting on one did not disturb the other.

The assertions are `tools/location_content_probe.py`'s own, moved rather
than rewritten. Nothing here boots an engine.
"""
from __future__ import annotations

import json
import time

from probelib import load_ai_stack, send

from .engine_queries import (loc_at, load_chunk, placed, spawn_counts,
                             spawn_unit, wait_floor)
from .invocation import ScenarioState

#: The `ldLabel` every `ruin_small` discovery event is titled with
#: (data/locations/ruin_small.yaml).
RUIN_LABEL = "Small Ruin"

#: A location-instance id no page will ever have allocated (#915) —
#: used to stage a memory whose (page, id) cannot resolve after a load.
DANGLING_ID = 99999

def discovered_flags(port: int, page: str) -> dict[tuple[int, int], bool]:
    """(cx, cy) -> discovered, for every placed location on `page` (#780)."""
    return {(e["cx"], e["cy"]): bool(e.get("discovered")) for e in placed(port, page)}


def event_log(port: int) -> list[dict]:
    raw = send(port, "return engine.getEventLog()").strip()
    if not raw or raw in ("nil", "null", "{}", "[]"):
        return []
    try:
        data = json.loads(raw)
    except json.JSONDecodeError:
        return []
    return data if isinstance(data, list) else []


def discovery_events(port: int, label: str) -> list[dict]:
    """Every logged `location_discovery` event naming `label` (#780)."""
    text = f"Discovered: {label}"
    return [e for e in event_log(port)
            if e.get("category") == "location_discovery" and e.get("text") == text]


def loc_instance_at(port: int, cx: int, cy: int, page: str) -> int:
    """The stable instance id (#911) of the location at chunk (cx, cy) on
    `page`, or -1. Scanned SERVER-side like `loc_at`, for the same
    reason: on a dense page the full list is thousands of entries and
    JSON round-tripping it is what this file deliberately avoids."""
    lua = (f"local t = world.listPlacedLocations('{page}'); "
           f"for _, e in ipairs(t) do if e.cx == {cx} and e.cy == {cy} then "
           f"return e.instance_id end end; return -1")
    try:
        return int(float(send(port, lua, timeout=20.0).strip('"')))
    except ValueError:
        return -1


def known_locations(port: int, uid: int) -> set[str]:
    """(#915) The per-unit location memories `uid` holds, as a set of
    "<page>#<instance id>" keys. Read through unitAi.getKnownLocations —
    the public query surface AI candidates use — and flattened to a
    string so an empty result is unambiguous (an empty Lua table would
    serialize identically to an empty object)."""
    lua = (f"local ai = require('scripts.unit_ai'); "
           f"local out = {{}}; "
           f"for _, k in ipairs(ai.getKnownLocations({uid})) do "
           f"out[#out+1] = k.page .. '#' .. tostring(k.id) end; "
           f"return table.concat(out, ',')")
    raw = send(port, lua).strip().strip('"')
    return {p for p in raw.split(",") if p}


# The widest a unit's night-aware sight radius can reach (#1230). A
# unit sees at most perception * awareRangeTiles tiles
# (Unit.LineOfSight.awareRangeTiles = 6.0), and the page-local night
# factor only ever SHRINKS that. No shipped unit carries a perception
# above 2.0, so 12 tiles bounds every sightline this probe can produce;
# the slack below then puts an "ignorant" unit comfortably past it.
#
# This replaces the removed 6-tile discovery halo. Re-deriving it from
# the sight radius rather than rewriting the old constant is the point:
# a unit that must NOT reveal a location has to be outside the RADIUS,
# outside the facing cone, or behind blocking terrain, and a 6-tile box
# no longer describes that boundary at all.
MAX_SIGHT_TILES = 12


def sight_box(e: dict) -> tuple[int, int, int, int]:
    """The region from which a placed location could be revealed: its
    stored bounds (#777) — the footprint Location.Discovery tests sight
    against since #1230 — grown by the widest reachable sight radius, so
    a tile outside this box cannot see any tile of the location."""
    b = e.get("bounds") or {}
    m = MAX_SIGHT_TILES
    return (int(b.get("min_x", e["gx"])) - m, int(b.get("min_y", e["gy"])) - m,
            int(b.get("max_x", e["gx"])) + m, int(b.get("max_y", e["gy"])) + m)


def pick_far_tile(la: list[dict], origin: tuple[int, int],
                  slack: int = 6) -> tuple[int, int] | None:
    """A tile comfortably outside EVERY placed location's sight box —
    where a second unit can stand and stay ignorant (#915)."""
    ox, oy = origin
    for d in range(24, 400, 8):
        for cand in ((ox + d, oy), (ox, oy + d), (ox + d, oy + d),
                     (ox - d, oy), (ox, oy - d)):
            if all(not (x0 - slack <= cand[0] <= x1 + slack
                        and y0 - slack <= cand[1] <= y1 + slack)
                   for x0, y0, x1, y1 in map(sight_box, la)):
                return cand
    return None


def wait_knows(port: int, uid: int, key: str, tries: int = 40) -> bool:
    for _ in range(tries):
        if key in known_locations(port, uid):
            return True
        time.sleep(0.25)
    return False


def observe_initial_discovery(args, state: ScenarioState,
                              failures: list[str]) -> None:
    """The initial process's discovery and knowledge half.

    Runs after the content owner's checks, on the same live page: the
    synthetic units below are units rather than ground items, so the
    loot baseline was already captured before any of them existed.
    Records the memory key, the units holding it, and the staged
    dangling-memory case for the fresh-process phase to re-check -- and
    refreshes the spawn counts, because the units it spawns persist like
    any other and the no-respawn comparison must account for them.
    """
    # ---- Discovery (#780): the stamping and content-spawning the
    #      content owner just observed on this page did NOT discover
    #      the ruin; a hostile unit standing on it
    #      doesn't either; a player-faction unit that SEES it
    #      does (#1230 — standing on the anchor is the strongest
    #      case, since a unit's own tile is always in its visible
    #      set), exactly once, flipping
    #      world.listPlacedLocations()'s `discovered` field. ----
    ruin0 = state.ruins[0]
    r0key = (ruin0["cx"], ruin0["cy"])

    disc0 = discovered_flags(args.port, "wa")
    if disc0.get(r0key) is False:
        print("PASS: stamping + content-spawning did not discover the ruin")
    else:
        failures.append(
            f"expected discovered:false after stamping, got {disc0.get(r0key)!r}")

    hostile_uid = spawn_unit(args.port, "acolyte", ruin0["gx"], ruin0["gy"],
                              "hostile", "wa")
    time.sleep(0.5)
    disc_hostile = discovered_flags(args.port, "wa")
    if hostile_uid >= 0 and disc_hostile.get(r0key) is False:
        print("PASS: a hostile unit standing on the ruin did not discover it")
    else:
        failures.append(
            f"hostile presence discovery check failed: uid={hostile_uid} "
            f"discovered={disc_hostile.get(r0key)!r}")

    player_uid = spawn_unit(args.port, "acolyte", ruin0["gx"], ruin0["gy"],
                             "player", "wa")
    discovered_ok = False
    for _ in range(20):
        if discovered_flags(args.port, "wa").get(r0key):
            discovered_ok = True
            break
        time.sleep(0.25)
    if player_uid >= 0 and discovered_ok:
        print(f"PASS: a player-faction unit ({player_uid}) that can see "
              f"the ruin flips world.listPlacedLocations() to discovered:true")
    else:
        failures.append(
            f"player presence did not discover the ruin: uid={player_uid}")

    evs = discovery_events(args.port, RUIN_LABEL)
    if len(evs) == 1 and evs[0].get("uid") == player_uid and evs[0].get("page") == "wa":
        print(f"PASS: exactly one location_discovery event, attributed to "
              f"unit {player_uid} on page 'wa'")
    else:
        failures.append(
            f"expected exactly one attributed discovery event, got {evs}")

    # Leaving (teleport away, well out of sight of it) and
    # returning must not emit a second event.
    send(args.port,
         f"unit.setPos({player_uid}, "
         f"{ruin0['gx'] + MAX_SIGHT_TILES + 8}, {ruin0['gy']}); return 'ok'")
    time.sleep(0.5)
    send(args.port,
         f"unit.setPos({player_uid}, {ruin0['gx']}, {ruin0['gy']}); return 'ok'")
    time.sleep(0.5)
    evs_again = discovery_events(args.port, RUIN_LABEL)
    if len(evs_again) == 1:
        print("PASS: leaving and returning emits no duplicate discovery event")
    else:
        failures.append(
            f"expected still exactly one event after leave+return, got {evs_again}")

    # ---- Per-unit location knowledge (#915): the EXPERIENTIAL
    #      layer beside the player-wide CARTOGRAPHIC state above.
    #      The unit AI stack owns that memory, so load it here —
    #      after every check this process has made, and with the
    #      sim PAUSED so
    #      the AI's own decisions (wander, forage, water-seeking)
    #      can never move a unit or pick up one of the ground
    #      items phase 2 re-counts. Pausing is not a workaround
    #      here, it is part of the contract under test: awareness
    #      is ingested BEFORE unitAi.update's pause guard,
    #      mirroring World.Thread.Discovery's own pause
    #      independence, so a paused session still learns. ----
    send(args.port, "engine.setPaused(true); return 'ok'")
    load_ai_stack(args.port)
    r0inst = next((e for e in placed(args.port, "wa")
                   if (e["cx"], e["cy"]) == r0key), None)
    far = pick_far_tile(state.placed_all, (ruin0["gx"], ruin0["gy"]))
    if r0inst is None or far is None:
        failures.append(
            f"#915 setup failed: instance={r0inst!r} far_tile={far!r}")
    else:
        r0mem = f"wa#{r0inst['instance_id']}"
        if wait_knows(args.port, player_uid, r0mem):
            print(f"PASS: the unit that can see the ruin gained its "
                  f"own memory of it ({r0mem}) — while PAUSED")
        else:
            failures.append(
                f"unit {player_uid} that can see the ruin never learned "
                f"{r0mem}: {known_locations(args.port, player_uid)}")

        load_chunk(args.port, far[0] // 16, far[1] // 16)
        far_uid = spawn_unit(args.port, "acolyte", far[0], far[1],
                             "player", "wa")
        time.sleep(1.5)
        if far_uid >= 0 and r0mem not in known_locations(args.port, far_uid):
            print(f"PASS: a second player unit ({far_uid}) elsewhere did "
                  f"NOT learn the ruin — knowledge is not shared for free")
        else:
            failures.append(
                f"remote unit {far_uid} learned {r0mem} without going "
                f"there: {known_locations(args.port, far_uid)}")

        # The player-wide layer is untouched by any of this: still
        # discovered, still exactly one event.
        evs_915 = discovery_events(args.port, RUIN_LABEL)
        if discovered_flags(args.port, "wa").get(r0key) is True \
                and len(evs_915) == 1:
            print("PASS: per-unit memory changed neither the "
                  "discovered lifecycle nor the event count")
        else:
            failures.append(
                f"#915 disturbed the player-wide layer: "
                f"discovered={discovered_flags(args.port, 'wa').get(r0key)!r} "
                f"events={evs_915}")

        # …and a unit arriving at an ALREADY-discovered location
        # still learns it: acquisition is not gated on the
        # one-time lifecycle promotion or its event.
        send(args.port, f"unit.setPos({far_uid}, {ruin0['gx']}, "
                        f"{ruin0['gy']}); return 'ok'")
        if wait_knows(args.port, far_uid, r0mem):
            print(f"PASS: unit {far_uid} arriving at an already-"
                  f"discovered ruin still learned it")
        else:
            failures.append(
                f"unit {far_uid} seeing an already-discovered location "
                f"never learned {r0mem}: "
                f"{known_locations(args.port, far_uid)}")
        evs_late = discovery_events(args.port, RUIN_LABEL)
        if len(evs_late) == 1:
            print("PASS: that later arrival emitted no second "
                  "location_discovery event")
        else:
            failures.append(
                f"a later arrival re-emitted discovery event(s): {evs_late}")
        state.mem_uids = (player_uid, far_uid)
        state.r0mem_key = r0mem

        # Stage the dangling-memory scenario for phase 2: the
        # discoverer walks to a SECOND ruin (so it holds two
        # genuinely-learned, resolving memories), then gets one
        # more naming an instance id that does not exist. A
        # never-allocated id cannot be produced by walking
        # anywhere, so it is injected through the module's own
        # public helper — the same call the ingest path makes.
        ruin1 = next((e for e in state.ruins
                      if (e["cx"], e["cy"]) != r0key), None)
        r1inst = next((e for e in placed(args.port, "wa")
                       if (e["cx"], e["cy"])
                       == (ruin1["cx"], ruin1["cy"])), None) \
            if ruin1 else None
        if r1inst is None:
            failures.append(
                "#915: need a SECOND ruin to stage two resolving "
                "sibling memories")
        else:
            send(args.port, f"unit.setPos({player_uid}, "
                            f"{ruin1['gx']}, {ruin1['gy']}); return 'ok'")
            r1mem = f"wa#{r1inst['instance_id']}"
            if not wait_knows(args.port, player_uid, r1mem):
                failures.append(
                    f"unit {player_uid} never learned the second ruin "
                    f"{r1mem}: {known_locations(args.port, player_uid)}")
            send(args.port,
                 f"local L = require('scripts.unit_ai_locations'); "
                 f"local ai = require('scripts.unit_ai'); "
                 f"L.addKnownLocation(ai.getState({player_uid}), 'wa', "
                 f"{DANGLING_ID}, {ruin0['gx']}, {ruin0['gy']}); "
                 f"return 'ok'")
            staged = known_locations(args.port, player_uid)
            want = {r0mem, r1mem, f"wa#{DANGLING_ID}"}
            if want <= staged:
                print(f"PASS: staged two resolving memories plus one "
                      f"naming a nonexistent instance ({sorted(want)})")
                state.dangling_uid = player_uid
                state.sibling_keys = (r0mem, r1mem)
            else:
                failures.append(
                    f"#915 could not stage the dangling-memory case: "
                    f"want {sorted(want)}, got {sorted(staged)}")

    # Deliberately still PAUSED when this returns, and through the
    # save the facade then takes: a load comes up paused anyway
    # (#763), so this keeps the initial and reloaded sessions in the
    # same sim state, and keeps the AI from moving units or picking up
    # the ground items the reloaded session re-counts.

    # The synthetic units above are now part of 'wa' — refresh
    # counts1 so phase 2's "reload does not respawn contents"
    # comparison accounts for them too (they persist like any
    # other unit, unrelated to the ruin's one-time content flag).
    state.counts1 = spawn_counts(args.port)


def check_discovery_survived(args, state: ScenarioState,
                             failures: list[str]) -> None:
    """The reloaded process's #780 half: the discovered lifecycle rides
    the save, and the per-session event does not."""
    r0key = (state.ruins[0]["cx"], state.ruins[0]["cy"])
    # #780: discovered state survives save -> quit -> restart ->
    # load; the event itself does NOT (player events are
    # per-session, never saved), so a fresh process reloading an
    # already-discovered location must emit zero events for it.
    disc_reload = discovered_flags(args.port, "wa")
    if disc_reload.get(r0key) is True:
        print("PASS: discovered state survived save -> quit -> restart -> load")
    else:
        failures.append(
            f"discovered state lost on reload: {disc_reload.get(r0key)!r}")
    evs_reload = discovery_events(args.port, RUIN_LABEL)
    if not evs_reload:
        print("PASS: reloading an already-discovered location re-emits no event")
    else:
        failures.append(
            f"reload incorrectly re-emitted discovery event(s): {evs_reload}")


def check_memory_survived(args, art, state: ScenarioState,
                          failures: list[str]) -> None:
    """The reloaded process's #915 half: a resolving memory rides
    `lua.unit_ai` through the round trip, the one deliberately
    unresolvable memory produces exactly one integrity diagnostic in
    THIS invocation's log and is the only entry the reconcile drops, and
    neither act touches the player-wide lifecycle."""
    # #915: per-unit location memory rides the lua.unit_ai
    # component (now v4) through the same round trip, and its
    # (page, instance id) reference still resolves — so the
    # reconcile pass keeps it rather than scrubbing it.
    if state.r0mem_key and state.mem_uids:
        still = {uid: known_locations(args.port, uid) for uid in state.mem_uids}
        if all(state.r0mem_key in ks for ks in still.values()):
            print(f"PASS: per-unit location memory ({state.r0mem_key}) survived "
                  f"save -> quit -> restart -> load for units {state.mem_uids}")
        else:
            failures.append(
                f"per-unit location memory lost on reload: {still}")
        # The engine-side integrity graph must report EXACTLY the
        # one memory phase 1 made unresolvable, and no other: a
        # VALID memory is only ever resolvable if its page
        # survives every hop from the references() hook to
        # World.Save.Integrity (the save_modules flatteners
        # rebuild each edge field by field, and an id alone
        # resolves against nothing for a per-page kind). The
        # log is this INVOCATION's (#1884) and probelib.boot
        # truncates it per boot, so this names only this load —
        # and no concurrent run can interleave into it.
        try:
            with open(art.engine_log, encoding="utf-8",
                      errors="replace") as fh:
                diags = [ln.strip() for ln in fh
                         if "integrity diagnostic" in ln
                         and "location_instance" in ln]
        except OSError as e:
            diags = [f"could not read {art.engine_log}: {e}"]
        want_bits = ("lua.unit_ai", f"page=wa,id={DANGLING_ID}",
                     "knownLocations", "location_instance")
        if len(diags) == 1 and all(b in diags[0] for b in want_bits):
            print(f"PASS: exactly one location_instance diagnostic, "
                  f"naming lua.unit_ai + the knownLocations field + "
                  f"page=wa,id={DANGLING_ID} — every VALID memory "
                  f"resolved")
        else:
            failures.append(
                f"expected exactly one dangling diagnostic naming "
                f"{want_bits}, got {diags}")

        # …and the load SUCCEEDED anyway (the facade already asserted
        # that through wait_load_published before calling any owner
        # here), with the real
        # apply/onSaveLoaded reconcile dropping ONLY the
        # unresolvable entry — its resolving siblings intact.
        if state.dangling_uid >= 0:
            after = known_locations(args.port, state.dangling_uid)
            if f"wa#{DANGLING_ID}" not in after \
                    and all(k in after for k in state.sibling_keys):
                print(f"PASS: onSaveLoaded dropped ONLY the "
                      f"unresolvable memory; unit {state.dangling_uid} kept "
                      f"{sorted(state.sibling_keys)}")
            else:
                failures.append(
                    f"dangling-memory scrub wrong for unit "
                    f"{state.dangling_uid}: kept {sorted(after)}, expected "
                    f"{sorted(state.sibling_keys)} without wa#{DANGLING_ID}")
            # Dropping a memory is a per-unit act: it must not
            # touch the player-wide layer either of its siblings
            # names.
            lifecycles = discovered_flags(args.port, "wa")
            sib_keys = {tuple(int(n) for n in k.split("#")[1:])
                        for k in state.sibling_keys}
            undiscovered = [e for e in placed(args.port, "wa")
                            if (e["instance_id"],) in sib_keys
                            and not e.get("discovered")]
            if not undiscovered and any(lifecycles.values()):
                print("PASS: scrubbing a memory left every remembered "
                      "location's player-wide lifecycle untouched")
            else:
                failures.append(
                    f"lifecycle changed while scrubbing a memory: "
                    f"{undiscovered}")
        else:
            failures.append(
                "phase 2 could not re-check the dangling-memory case: "
                "phase 1 never staged one")
    else:
        failures.append(
            "phase 2 could not re-check per-unit location memory: "
            "phase 1 never established one")


def check_cross_page_instance_isolation(args, site: tuple[int, int],
                                        failures: list[str]) -> None:
    """#915's cross-page half, on the dispatch scenario's own two
    same-seed pages: the SAME instance id names a different real
    location on each, and a unit learns only its own page's.

    `site` is the centre-chunk tile the dispatch owner established and
    handed back through the facade, so this assumes nothing about where
    that page put its location.
    """
    gx, gy = site
    # #915 multi-page COLLISION: instance ids are
    # allocated PER PAGE, so the SAME number names
    # different real locations on different worlds.
    # 'sw3' is generated from the same seed/size as
    # 'sw2', so its dense placement is identical and
    # the centre-chunk location gets the identical
    # id — the case a page-blind memory would get
    # wrong. Both pages stay loaded throughout.
    send(args.port, "engine.setPaused(true); return 'ok'")
    load_ai_stack(args.port)
    send(args.port, f"world.init('sw3', {args.seed}, "
                    f"{args.size}, 3); return 'ok'")
    sw3xy = loc_at(args.port, 0, 0, "sw3")
    iid2 = wait_floor(args.port, gx, gy, page="sw3") \
        and loc_instance_at(args.port, 0, 0, "sw3")
    iid = loc_instance_at(args.port, 0, 0, "sw2")
    if sw3xy != (gx, gy) or iid < 0 or iid2 != iid:
        failures.append(
            f"phase 4 (#915) setup failed: same-seed pages "
            f"did not collide — sw2 ({gx},{gy})#{iid} vs "
            f"sw3 {sw3xy}#{iid2}")
    else:
        print(f"PASS: 'sw2' and 'sw3' both carry instance "
              f"id {iid} at ({gx},{gy}) — a genuine "
              f"cross-page id collision to test against")
        u2 = spawn_unit(args.port, "acolyte", gx, gy,
                        "player", "sw2")
        u3 = spawn_unit(args.port, "acolyte", gx, gy,
                        "player", "sw3")
        ok2 = u2 >= 0 and wait_knows(args.port, u2, f"sw2#{iid}")
        ok3 = u3 >= 0 and wait_knows(args.port, u3, f"sw3#{iid}")
        k2 = known_locations(args.port, u2)
        k3 = known_locations(args.port, u3)
        if ok2 and ok3 and f"sw3#{iid}" not in k2 \
                and f"sw2#{iid}" not in k3:
            print(f"PASS: each unit learned ONLY its own "
                  f"page's instance {iid} — equal ids on "
                  f"two pages did not alias")
        else:
            failures.append(
                f"phase 4 (#915): cross-page aliasing — "
                f"unit {u2} on sw2 knows {sorted(k2)}, "
                f"unit {u3} on sw3 knows {sorted(k3)}")
        resolves = {
            p: send(args.port,
                    f"return world.getLocationInstance("
                    f"{iid}, '{p}') and 'y' or 'n'").strip('"')
            for p in ("sw2", "sw3")}
        if resolves == {"sw2": "y", "sw3": "y"}:
            print(f"PASS: instance id {iid} resolves on BOTH "
                  f"pages — the memories stayed distinct "
                  f"because each carries its own page, not "
                  f"because only one page had that id")
        else:
            failures.append(
                f"phase 4 (#915): expected instance {iid} on "
                f"both pages, got {resolves}")
