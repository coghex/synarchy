#!/usr/bin/env python3
"""Headless location content-spawning probe (#90) + ruin probe (#91).

Issues #88/#89 give locations a definition and a place in the world;
this checks the `contents` list actually spawns things when a
location's chunk loads, end to end:

  1. Visiting a `ruin_small` (#91: a partially-collapsed room) spawns
     its contents — two fixed-position ground items (`radio`,
     `canteen_steel_2l`) and two `ruin_common` loot-table rolls (also
     ground items) — per ruin, and NO units or buildings (units in
     ruins are deferred by design). The geometry is a damaged
     `room_small`: all 25 floors present, a breached perimeter (some
     but not all of the 20 wall segments), exactly 3 corner posts, and
     every piece carrying the pack's "damaged" variant texture path.
  2. The one-time content-spawn flag AND the damaged geometry survive a
     save -> quit -> fresh restart -> load: revisiting the same chunk
     does NOT respawn (counts stay exactly the same, not doubled), the
     breach pattern replays identically, and the pieces still resolve
     to the damaged variant art (the #91 variant round-trip).
  3. An unknown content `kind` and an unknown content `id` both log a
     warning and are skipped rather than crashing the engine.

Headless skips the GUI data-loading step, so defs are registered by
hand here (items/units/buildings/loot_tables/locations), same as
tools/location_overlay_probe.py does for locations alone.

Usage:
  python3 tools/location_content_probe.py
  python3 tools/location_content_probe.py --seed 42 --size 64 --port 9190

Exit code 0 = all checks passed.
"""
from __future__ import annotations

import argparse
import json
import re
import socket
import subprocess
import sys
import time
from probelib import quit_engine, boot, send

LOG = "/tmp/location_content_engine.log"


def load_yaml_dir(port: int, directory: str, loader: str) -> None:
    lua = (f"local fs = engine.listFiles('{directory}', '.yaml') or {{}}; "
           f"for _, f in ipairs(fs) do {loader}('{directory}/' .. f) end; "
           f"return #fs")
    send(port, lua, timeout=20.0)


def load_registries(port: int) -> None:
    load_yaml_dir(port, "data/items", "engine.loadItemYaml")
    load_yaml_dir(port, "data/units", "engine.loadUnitYaml")
    load_yaml_dir(port, "data/buildings", "engine.loadBuildingYaml")
    load_yaml_dir(port, "data/loot_tables", "engine.loadLootTableYaml")


def load_defs(port: int) -> None:
    load_registries(port)
    send(port, "engine.loadLocationYaml('data/locations/ruin_small.yaml'); return 'ok'")


def gen_world(port: int, page: str, seed: int, size: int) -> None:
    send(port, f"world.init('{page}', {seed}, {size}, 3); return 'ok'")
    send(port, "return world.waitForInit(240)", timeout=250)
    send(port, f"world.show('{page}'); return 'ok'")
    send(port, "return world.loadChunksInRegion(-1,-1,1,1)")
    send(port, "return world.waitForChunks(60)", timeout=65)


def placed(port: int, page: str | None = None) -> list[dict]:
    arg = f"'{page}'" if page else ""
    raw = send(port, f"return world.listPlacedLocations({arg})").strip()
    if not raw or raw in ("nil", "null", "{}", "[]"):
        return []
    try:
        data = json.loads(raw)
    except json.JSONDecodeError:
        return []
    return data if isinstance(data, list) else []


def loc_at(port: int, cx: int, cy: int, page: str, tries: int = 120) -> tuple[int, int] | None:
    """(gx, gy) of the location placed at chunk (cx, cy) on `page`, or
    None. Server-side scan, never ships the full list to Python — needed
    for a DENSE def (one location per land chunk; #90 phase 4), where the
    full list is thousands of entries and JSON round-tripping it is the
    kind of thing tools/location_overlay_probe.py deliberately avoids.

    Polls: world.waitForInit always reads the ACTIVE world's load phase
    (Engine/Scripting/Lua/API/World.hs worldWaitForInitFn), so it cannot
    be used to wait for a HIDDEN page's init to finish — the caller can't
    know when `page`'s gen params (and thus its overlay) become readable
    other than by retrying this query."""
    lua = (f"local t = world.listPlacedLocations('{page}'); "
           f"for _, e in ipairs(t) do if e.cx == {cx} and e.cy == {cy} then "
           f"return e.gx .. ',' .. e.gy end end; return 'none'")
    r = "none"
    for _ in range(tries):
        r = send(port, lua, timeout=20.0).strip('"')
        if r != "none":
            break
        time.sleep(0.5)
    if r == "none" or "," not in r:
        return None
    gx_s, gy_s = r.split(",", 1)
    return int(gx_s), int(gy_s)


def placed_ready(port: int, tries: int = 30) -> list[dict]:
    last: list[dict] = []
    for _ in range(tries):
        last = placed(port)
        if last:
            return last
        time.sleep(0.5)
    return last


def load_chunk(port: int, cx: int, cy: int) -> None:
    send(port, f"return world.loadChunksInRegion({cx},{cy},{cx},{cy})")
    send(port, "return world.waitForChunks(30)", timeout=35)


def has_floor(port: int, gx: int, gy: int, page: str | None = None) -> bool:
    arg = f",'{page}'" if page else ""
    r = send(port, f"return structure.hasAt({gx},{gy},'floor'{arg}) and 'yes' or 'no'")
    return r.strip('"') == "yes"


def wait_floor(port: int, gx: int, gy: int, page: str | None = None, tries: int = 40) -> bool:
    for _ in range(tries):
        if has_floor(port, gx, gy, page):
            return True
        time.sleep(0.5)
    return False


def ruin_geometry(port: int, gx: int, gy: int, page: str | None = None) -> tuple[int, int, int]:
    """(floors, walls, posts) of the 5x5 ruin anchored at (gx, gy).
    Counted server-side over the room footprint: 25 floor tiles, the 20
    perimeter wall segments (nw/se run along x0/x1, ne/sw along y0/y1),
    and the 4 corner posts."""
    arg = f",'{page}'" if page else ""
    lua = (
        f"local f,w,p=0,0,0; "
        f"for x={gx-2},{gx+2} do for y={gy-2},{gy+2} do "
        f"if structure.hasAt(x,y,'floor'{arg}) then f=f+1 end end end; "
        f"for y={gy-2},{gy+2} do "
        f"if structure.hasAt({gx-2},y,'wall_nw'{arg}) then w=w+1 end "
        f"if structure.hasAt({gx+2},y,'wall_se'{arg}) then w=w+1 end end; "
        f"for x={gx-2},{gx+2} do "
        f"if structure.hasAt(x,{gy-2},'wall_ne'{arg}) then w=w+1 end "
        f"if structure.hasAt(x,{gy+2},'wall_sw'{arg}) then w=w+1 end end; "
        f"for _,c in ipairs({{{{{gx-2},{gy-2},'post_n'}},{{{gx+2},{gy-2},'post_e'}},"
        f"{{{gx+2},{gy+2},'post_s'}},{{{gx-2},{gy+2},'post_w'}}}}) do "
        f"if structure.hasAt(c[1],c[2],c[3]{arg}) then p=p+1 end end; "
        f"return f .. ',' .. w .. ',' .. p")
    r = send(port, lua).strip('"')
    try:
        f, w, p = (int(v) for v in r.split(","))
        return f, w, p
    except ValueError:
        return -1, -1, -1


def floor_tex(port: int, gx: int, gy: int, page: str | None = None) -> str:
    """Texture path of the floor piece at (gx, gy) — the persisted
    variant identity (#91)."""
    arg = f",'{page}'" if page else ""
    r = send(port, f"local t=structure.getAt({gx},{gy},'floor'{arg}); "
                   f"return t and t.tex or 'none'")
    return r.strip('"')


def unit_count(port: int, def_name: str) -> int:
    r = send(port, "return unit.list()")
    return len(re.findall(re.escape(def_name), r))


def building_count(port: int, def_name: str) -> int:
    r = send(port, "return building.list()")
    return len(re.findall(re.escape(def_name), r))


def ground_items(port: int) -> list[dict]:
    raw = send(port, "return item.listGround()").strip()
    if not raw or raw in ("nil", "null", "{}", "[]"):
        return []
    try:
        data = json.loads(raw)
    except json.JSONDecodeError:
        return []
    return data if isinstance(data, list) else []


def spawn_counts(port: int) -> dict:
    items = ground_items(port)
    counts: dict[str, int] = {}
    for it in items:
        name = it.get("defName", "?")
        counts[name] = counts.get(name, 0) + 1
    return {
        "acolyte": unit_count(port, "acolyte"),
        "cargo_hold_S": building_count(port, "cargo_hold_S"),
        "ground_total": len(items),
        "ground_by_name": counts,
    }


def registered_item_names(port: int) -> set[str]:
    """The live item registry (item.listDefs()) — #800 replaces the stale
    hardcoded loot_names allowlist with this as the authoritative source,
    so a valid new loot entry (e.g. quinoa_sack, #458) is accepted without
    the probe needing to be updated by hand."""
    raw = send(port, "return item.listDefs()").strip()
    if not raw or raw in ("nil", "null", "{}", "[]"):
        return set()
    try:
        data = json.loads(raw)
    except json.JSONDecodeError:
        return set()
    return {d["name"] for d in data if isinstance(d, dict) and "name" in d}


def unregistered_item_ids(names: set[str], registered: set[str]) -> set[str]:
    """Pure check: which of `names` aren't in the live item registry.
    Kept as a standalone function so it can be exercised directly against
    a synthetic id, independent of whatever a real spawn happens to
    produce (#800)."""
    return set(names) - registered


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--port", type=int, default=9190)
    args = ap.parse_args()

    failures: list[str] = []
    ruins: list[dict] = []
    counts1: dict = {}
    geoms1: dict = {}

    # ---- Phase 1: content spawns when a ruin's chunk loads. ----
    proc = boot(args.port, log=LOG)
    try:
        load_defs(args.port)
        gen_world(args.port, "wa", args.seed, args.size)
        la = placed_ready(args.port)
        ruins = [e for e in la if e["id"] == "ruin_small"]
        print(f"world (seed {args.seed}): {len(ruins)} ruin_small placed")
        if not ruins:
            failures.append("no ruin_small placed — cannot test content spawning")
        else:
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
            # Each ruin (#91): 2 fixed items (radio + canteen) + 2
            # loot_table rolls, all ground items; NO units or buildings.
            want_ground = 4 * len(ruins)
            counts1 = {}
            for _ in range(20):
                counts1 = spawn_counts(args.port)
                if counts1["ground_total"] >= want_ground:
                    break
                time.sleep(0.5)
            print(f"  spawned: {counts1}")

            if counts1["ground_total"] == want_ground:
                print(f"PASS: {want_ground} ground item(s) spawned "
                      f"(fixed radio + canteen + 2 loot_table rolls per ruin)")
            else:
                failures.append(
                    f"expected {want_ground} ground item(s), got "
                    f"{counts1['ground_total']} ({counts1['ground_by_name']})")

            if counts1["acolyte"] == 0 and counts1["cargo_hold_S"] == 0:
                print("PASS: no units or buildings spawned (out of scope for #91 ruins)")
            else:
                failures.append(
                    f"ruin_small spawned units/buildings it shouldn't: {counts1}")

            for fixed in ("radio", "canteen_steel_2l"):
                n = counts1["ground_by_name"].get(fixed, 0)
                if n >= len(ruins):
                    print(f"PASS: fixed-position '{fixed}' item present ({n} >= {len(ruins)})")
                else:
                    failures.append(f"expected >= {len(ruins)} '{fixed}', got {n}")

            registered = registered_item_names(args.port)
            unexpected = unregistered_item_ids(set(counts1["ground_by_name"]), registered)
            if not unexpected:
                print("PASS: all spawned ground items resolve to registered "
                      "item definitions (item.listDefs(), fixed items + loot "
                      "table entries)")
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

            send(args.port, "engine.saveWorld('wa', 'loc_content_probe'); return 'saved'")
            time.sleep(1.0)
    finally:
        quit_engine(args.port, proc)

    # ---- Phase 2: save -> quit -> fresh restart -> load -> revisit does
    #      NOT respawn (one-time flag persisted, independent of the
    #      structure.hasAt geometry check). ----
    if ruins and not failures:
        proc = boot(args.port, log=LOG)
        try:
            load_defs(args.port)
            send(args.port, "engine.loadSave('loc_content_probe'); return 'queued'")
            time.sleep(6.0)
            send(args.port, "world.show('main_world'); return 'ok'")
            time.sleep(1.0)
            for e in ruins:
                load_chunk(args.port, e["cx"], e["cy"])
            # No settle-time poll needed here: a respawn would be immediate
            # and permanent, unlike the initial spawn's queue latency.
            time.sleep(2.0)
            counts2 = spawn_counts(args.port)
            print(f"  after reload: {counts2}")
            if counts2 == counts1:
                print("PASS: reload does not respawn contents (counts unchanged)")
            else:
                failures.append(
                    f"contents respawned on reload: before={counts1} after={counts2}")

            # #91: the damaged geometry replays identically from the edit
            # log (same breach pattern — the builder did NOT re-run and
            # re-roll), and the pieces still resolve to the damaged
            # variant art (texture identity rides the structure palette).
            for e in ruins:
                g2 = ruin_geometry(args.port, e["gx"], e["gy"])
                g1 = geoms1.get((e["gx"], e["gy"]))
                if g2 == g1:
                    print(f"PASS: ruin at ({e['gx']},{e['gy']}) replayed its "
                          f"breach pattern exactly (floors/walls/posts {g2})")
                else:
                    failures.append(
                        f"ruin at ({e['gx']},{e['gy']}) changed shape on "
                        f"reload: before={g1} after={g2}")
            tex = floor_tex(args.port, ruins[0]["gx"], ruins[0]["gy"])
            if "/damaged/" in tex:
                print(f"PASS: damaged variant survived save/load ({tex})")
            else:
                failures.append(
                    f"ruin floor texture lost the damaged variant on reload: {tex}")
        finally:
            quit_engine(args.port, proc)
    elif not ruins:
        failures.append("phase 2 skipped: no ruins from phase 1")

    # ---- Phase 3: unknown content kind / id logs a warning and is
    #      skipped, not a crash. Also covers a loot_table rolling an
    #      item id that isn't registered. ----
    bogus_yaml = "/tmp/loc_content_probe_bogus.yaml"
    with open(bogus_yaml, "w") as fh:
        fh.write(
            "locations:\n"
            "  - id: bogus_ruin\n"
            "    label: Bogus Ruin\n"
            "    type: ruin\n"
            "    builder: room_small\n"
            "    anchor: []\n"
            "    max_count: 0\n"
            "    bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }\n"
            "    discovery_margin: 6\n"
            "    contents:\n"
            "      - { kind: unit, id: does_not_exist, count: 1 }\n"
            "      - { kind: not_a_real_kind, id: whatever, count: 1 }\n"
            "      - { kind: loot_table, id: bogus_table, rolls: 1 }\n"
        )
    bogus_loot_yaml = "/tmp/loc_content_probe_bogus_loot.yaml"
    with open(bogus_loot_yaml, "w") as fh:
        fh.write(
            "id: bogus_table\n"
            "entries:\n"
            "  - id: item_that_does_not_exist\n"
            "    weight: 1\n"
        )
    # A single-entry loot table forces quinoa_sack to spawn deterministically
    # through the real content-spawn path (locations.spawnContents ->
    # loot.roll -> item.spawnGround), rather than depending on whether
    # ruin_common's random 2/12-weight roll happens to select it (#800).
    quinoa_yaml = "/tmp/loc_content_probe_quinoa.yaml"
    with open(quinoa_yaml, "w") as fh:
        fh.write(
            "locations:\n"
            "  - id: probe_quinoa_ruin\n"
            "    label: Quinoa Probe Ruin\n"
            "    type: ruin\n"
            "    builder: room_small\n"
            "    anchor: []\n"
            "    max_count: 0\n"
            "    bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }\n"
            "    discovery_margin: 6\n"
            "    contents:\n"
            "      - { kind: loot_table, id: probe_quinoa_table, rolls: 1 }\n"
        )
    quinoa_loot_yaml = "/tmp/loc_content_probe_quinoa_loot.yaml"
    with open(quinoa_loot_yaml, "w") as fh:
        fh.write(
            "id: probe_quinoa_table\n"
            "entries:\n"
            "  - id: quinoa_sack\n"
            "    weight: 1\n"
        )
    proc = boot(args.port, log=LOG)
    try:
        load_defs(args.port)
        send(args.port, f"engine.loadLocationYaml('{bogus_yaml}'); return 'ok'")
        send(args.port, f"engine.loadLootTableYaml('{bogus_loot_yaml}'); return 'ok'")
        send(args.port, f"engine.loadLocationYaml('{quinoa_yaml}'); return 'ok'")
        send(args.port, f"engine.loadLootTableYaml('{quinoa_loot_yaml}'); return 'ok'")
        gen_world(args.port, "wc", args.seed, args.size)
        # Stamp directly (bogus_ruin has max_count 0, so it never places via
        # the overlay) — content-spawning is the concern here, not overlay
        # placement. spawnContents dispatches to unit/kind lookups directly.
        r = send(args.port,
                  "local locations = require('scripts.locations'); "
                  "locations.spawnContents('bogus_ruin', 40, 40, 'wc'); "
                  "return 'ok'")
        alive = send(args.port, "return engine.getFPS() ~= nil and 'alive' or 'dead'")
        if r.strip('"') == "ok" and "alive" in alive:
            print("PASS: unknown unit id + unknown content kind + unknown "
                  "loot roll did not crash the engine")
        else:
            failures.append(f"spawnContents with bogus content misbehaved: {r!r} / {alive!r}")
        log_text = open(LOG, errors="replace").read()
        if ("unknown unit content" in log_text
                and "unknown content kind" in log_text
                and "rolled unknown item id" in log_text):
            print("PASS: the unknown unit id, unknown content kind, AND "
                  "loot-table-rolled-unknown-item-id all logged a warning")
        else:
            failures.append(
                "expected warnings for unknown unit id, unknown content "
                f"kind, AND unknown loot roll not all found in {LOG}")

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
        got_quinoa = counts3["ground_by_name"].get("quinoa_sack", 0)
        if got_quinoa >= 1:
            print(f"PASS: a forced single-entry loot table deterministically "
                  f"spawned quinoa_sack ({got_quinoa}), independent of "
                  f"ruin_common's random 2/12-weight roll")
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
    finally:
        quit_engine(args.port, proc)

    # ---- Phase 4: a building AND a unit content entry spawn correctly
    #      on a HIDDEN,
    #      non-active page (#90 review fix — building.spawn now takes an
    #      explicit pageId, mirroring unit.spawn/item.spawnGround, and its
    #      occupancy/terrain-Z check is scoped to THAT page, not a snapshot
    #      of the visible worlds). A DENSE location (one per land chunk,
    #      like tools/location_overlay_probe.py's DENSE_YAML) guarantees
    #      content at the SYNCHRONOUS centre chunk (0,0), which stamps at
    #      world.init time via Init.hs's centre-chunk hook regardless of
    #      active/visible status — so this needs no chunk loading on the
    #      hidden page (world.loadChunksInRegion only targets the active
    #      world, so a hidden page can't otherwise be force-loaded here). ----
    dense_yaml = "/tmp/loc_content_probe_dense.yaml"
    with open(dense_yaml, "w") as fh:
        fh.write(
            "locations:\n"
            "  - id: dense_ruin\n"
            "    label: Dense Ruin\n"
            "    type: ruin\n"
            "    builder: room_small\n"
            "    anchor: [waterside]\n"
            "    max_count: 100000\n"
            "    min_spacing: 1\n"
            "    bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }\n"
            "    discovery_margin: 6\n"
            "    contents:\n"
            "      - { kind: building, id: cargo_hold_S, count: 1, position: {x: 0, y: 0} }\n"
            "      - { kind: unit, id: acolyte, count: 1, faction: hostile, position: {x: 1, y: 1} }\n"
        )
    proc = boot(args.port, log=LOG)
    try:
        # Registries only — NOT ruin_small.yaml, which would contend with
        # dense_ruin for chunk (0,0) and make the placement non-deterministic
        # (mirrors tools/location_overlay_probe.py's isolated DENSE_YAML use).
        load_registries(args.port)
        send(args.port, f"engine.loadLocationYaml('{dense_yaml}'); return 'ok'")
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
        else:
            # Generate 'sw2' but NEVER show it — arena stays active throughout.
            # NB world.waitForInit always polls the ACTIVE world (arena,
            # already done) — it can't wait for a hidden page, so loc_at's
            # own retry loop is what actually waits for 'sw2' to be ready.
            send(args.port, f"world.init('sw2', {args.seed}, {args.size}, 3); return 'ok'")
            active = send(args.port, "return world.getActiveWorldId()").strip('"')
            if active != "arena":
                failures.append(f"phase 4: expected 'arena' active throughout, got '{active}'")
            else:
                gxgy = loc_at(args.port, 0, 0, "sw2")
                if gxgy is None:
                    failures.append(
                        "phase 4: no location on centre chunk (0,0) of hidden page 'sw2'")
                else:
                    gx, gy = gxgy
                    if not wait_floor(args.port, gx, gy, page="sw2"):
                        failures.append(
                            f"phase 4: centre chunk (0,0)/({gx},{gy}) on 'sw2' never stamped")
                    else:
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
                        # ruin_small itself is unit-free (#91). The spawn
                        # happened while 'sw2' was hidden; unit.list is
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
    finally:
        quit_engine(args.port, proc)

    print("-" * 56)
    if failures:
        for f in failures:
            print(f"FAIL: {f}", file=sys.stderr)
        return 1
    print("ALL CHECKS PASSED")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
