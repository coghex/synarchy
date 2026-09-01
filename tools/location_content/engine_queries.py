#!/usr/bin/env python3
"""Engine reads and world setup shared by more than one scenario owner
(#2095).

Split out of `tools/location_content_probe.py` unchanged. A helper used
by exactly one scenario lives with that scenario instead; what is here
is what two or more of them need, plus the nine names other probes
import through the facade.
"""
from __future__ import annotations

import json
import re
import time

from probelib import send

#: Ground items one ruin_small spawns: its `ruin_common` loot_table
#: entry's 2 rolls, and nothing else. #921 removed the two fixed-position
#: items that used to make this 4 — the count is now purely the roll
#: count in data/locations/ruin_small.yaml.
GROUND_PER_RUIN = 2


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
        "nomad_primitive": unit_count(port, "nomad_primitive"),
        "cargo_hold_S": building_count(port, "cargo_hold_S"),
        "ground_total": len(items),
        "ground_by_name": counts,
    }


def spawn_unit(port: int, def_name: str, gx: int, gy: int, faction: str, page: str) -> int:
    """unit.spawn(...) returns the new unit's numeric id, or -1 on failure."""
    r = send(port, f"return unit.spawn('{def_name}', {gx}, {gy}, nil, '{faction}', '{page}')")
    try:
        return int(float(r.strip('"')))
    except ValueError:
        return -1


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
