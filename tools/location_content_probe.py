#!/usr/bin/env python3
"""Headless location content-spawning probe (#90).

Issues #88/#89 give locations a definition and a place in the world;
this checks the `contents` list actually spawns things when a
location's chunk loads, end to end:

  1. Visiting a `ruin_small` (contents: building, unit, item, loot_table)
     spawns all of them: one `cargo_hold_S` building, one hostile
     `acolyte` unit, one fixed-position `rations` ground item, and two
     loot-table rolls (also ground items) — per ruin.
  2. The one-time content-spawn flag survives a save -> quit -> fresh
     restart -> load: revisiting the same chunk does NOT respawn
     (counts stay exactly the same, not doubled).
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

LOG = "/tmp/location_content_engine.log"


def send(port: int, lua: str, timeout: float = 10.0) -> str:
    """Run one line of Lua in the debug console, return the result text."""
    with socket.create_connection(("localhost", port), timeout=timeout) as s:
        s.sendall((lua + "\n").encode())
        chunks: list[bytes] = []
        s.settimeout(0.3)
        try:
            while True:
                b = s.recv(4096)
                if not b:
                    break
                chunks.append(b)
        except socket.timeout:
            pass
    out = b"".join(chunks).decode(errors="replace")
    results = [ln[2:].strip() for ln in out.splitlines() if ln.startswith("> ")]
    results = [r for r in results if r]
    return results[-1] if results else out.strip()


def boot(port: int) -> subprocess.Popen:
    log = open(LOG, "w")
    proc = subprocess.Popen(
        ["cabal", "run", "-v0", "exe:synarchy", "--",
         "--headless", "--port", str(port)],
        stdout=log, stderr=subprocess.STDOUT,
    )
    deadline = time.time() + 240
    while time.time() < deadline:
        try:
            if "READY" in open(LOG).read():
                return proc
        except FileNotFoundError:
            pass
        if proc.poll() is not None:
            sys.exit(f"engine exited before READY; see {LOG}")
        time.sleep(0.4)
    proc.kill()
    sys.exit("engine never printed READY")


def shutdown(port: int, proc: subprocess.Popen) -> None:
    try:
        send(port, "engine.quit(); return 'bye'", timeout=3.0)
    except OSError:
        pass
    try:
        proc.wait(timeout=15)
    except subprocess.TimeoutExpired:
        proc.kill()


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


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--port", type=int, default=9190)
    args = ap.parse_args()

    failures: list[str] = []
    ruins: list[dict] = []
    counts1: dict = {}

    # ---- Phase 1: content spawns when a ruin's chunk loads. ----
    proc = boot(args.port)
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

            # Content spawning has its own settle time (unit spawn queues to
            # the unit thread) — poll briefly for the expected counts.
            counts1 = {}
            for _ in range(20):
                counts1 = spawn_counts(args.port)
                if (counts1["acolyte"] >= len(ruins)
                        and counts1["cargo_hold_S"] >= len(ruins)):
                    break
                time.sleep(0.5)
            print(f"  spawned: {counts1}")

            want_units = len(ruins)
            want_buildings = len(ruins)
            # Each ruin: 1 fixed `rations` + 2 loot_table rolls (from
            # ruin_common: rations/first_aid_kit/steel_dagger).
            want_ground = 3 * len(ruins)

            if counts1["acolyte"] == want_units:
                print(f"PASS: {want_units} 'acolyte' unit(s) spawned (1 per ruin, faction hostile)")
            else:
                failures.append(
                    f"expected {want_units} acolyte unit(s), got {counts1['acolyte']}")

            if counts1["cargo_hold_S"] == want_buildings:
                print(f"PASS: {want_buildings} 'cargo_hold_S' building(s) spawned")
            else:
                failures.append(
                    f"expected {want_buildings} cargo_hold_S building(s), "
                    f"got {counts1['cargo_hold_S']}")

            if counts1["ground_total"] == want_ground:
                print(f"PASS: {want_ground} ground item(s) spawned "
                      f"(fixed 'rations' + 2 loot_table rolls per ruin)")
            else:
                failures.append(
                    f"expected {want_ground} ground item(s), got "
                    f"{counts1['ground_total']} ({counts1['ground_by_name']})")

            rations = counts1["ground_by_name"].get("rations", 0)
            if rations >= len(ruins):
                print(f"PASS: fixed-position 'rations' item present ({rations} >= {len(ruins)})")
            else:
                failures.append(f"expected >= {len(ruins)} 'rations', got {rations}")

            loot_names = {"rations", "first_aid_kit", "steel_dagger"}
            unexpected = set(counts1["ground_by_name"]) - loot_names
            if not unexpected:
                print("PASS: all spawned ground items resolve to known ids "
                      "(fixed item + loot table entries)")
            else:
                failures.append(f"unexpected ground item id(s): {unexpected}")

            send(args.port, "engine.saveWorld('wa', 'loc_content_probe'); return 'saved'")
            time.sleep(1.0)
    finally:
        shutdown(args.port, proc)

    # ---- Phase 2: save -> quit -> fresh restart -> load -> revisit does
    #      NOT respawn (one-time flag persisted, independent of the
    #      structure.hasAt geometry check). ----
    if ruins and not failures:
        proc = boot(args.port)
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
        finally:
            shutdown(args.port, proc)
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
    proc = boot(args.port)
    try:
        load_defs(args.port)
        send(args.port, f"engine.loadLocationYaml('{bogus_yaml}'); return 'ok'")
        send(args.port, f"engine.loadLootTableYaml('{bogus_loot_yaml}'); return 'ok'")
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
    finally:
        shutdown(args.port, proc)

    # ---- Phase 4: a building content entry spawns correctly on a HIDDEN,
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
            "    contents:\n"
            "      - { kind: building, id: cargo_hold_S, count: 1, position: {x: 0, y: 0} }\n"
        )
    proc = boot(args.port)
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
    finally:
        shutdown(args.port, proc)

    print("-" * 56)
    if failures:
        for f in failures:
            print(f"FAIL: {f}", file=sys.stderr)
        return 1
    print("ALL CHECKS PASSED")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
