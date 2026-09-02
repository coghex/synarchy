#!/usr/bin/env python3
"""Shared readers and deterministic geometry (#2092).

Every question this scenario asks a live engine that more than one stage
owner needs the answer to, plus the geometry that is a pure function of
those answers. One reader per fact, so a stage owner never reaches into
a sibling's implementation to learn something and no two owners spell
the same query differently.

Nothing here asserts, boots, mutates the world, or knows what stage is
running: a reader takes a live port and returns what the engine said.
The one exception to "no mutation" is `load_region`, which asks the
engine to page chunks in — a read that has to make its own subject
exist.

Deliberately expedition-LOCAL rather than hoisted into `probelib`.
`docs/code_health_findings.md` CH-136 separately proposes consolidating
the duplicated probe helpers across `tools/`, with the explicit warning
that the `jget`/`send_json` semantic differences have to be reconciled
first; routing this extraction through that would touch every other
probe.
"""
from __future__ import annotations

import math

from probelib import send, send_json

from .constants import MAX_SIGHT_TILES, PAGE, RATIONS_DEF


def _as_float(raw):
    try:
        return float(raw)
    except (TypeError, ValueError):
        return None


def dist(a, b) -> float:
    return math.hypot(a[0] - b[0], a[1] - b[1])


def bearing_gap(a, b, target) -> float:
    """Angle between the two bearings from `a` and `b` to `target`, in
    degrees, wrapped into [0, 180].

    The wrap is the point: a raw difference of two atan2 results runs
    over the +/-pi branch cut, which reports two travellers standing
    three tiles apart as 354.6 degrees rather than 5.4."""
    d = (math.atan2(target[1] - a[1], target[0] - a[0])
         - math.atan2(target[1] - b[1], target[0] - b[0]))
    return abs(math.degrees(math.atan2(math.sin(d), math.cos(d))))


# --------------------------------------------------------------------------
# The world: terrain, chunks, ground items
# --------------------------------------------------------------------------
def surface_z(port: int, gx: int, gy: int):
    """world.getSurfaceAt returns several values; the console tab-joins
    them, so wrap the call in parens to keep only the first."""
    return _as_float(send(port, f"return (world.getSurfaceAt({gx},{gy}))"))


def load_region(port: int, cx: int, cy: int, pad: int = 4) -> None:
    send(port, f"return world.loadChunksInRegion({cx-pad},{cy-pad},"
               f"{cx+pad},{cy+pad})", timeout=60.0)
    send(port, "return world.waitForChunks(180)", timeout=190.0)


def ground_items(port: int) -> list:
    data = send_json(port, "return item.listGround()")
    return data if isinstance(data, list) else []


# --------------------------------------------------------------------------
# Units: where they are, what they are doing, what they carry
# --------------------------------------------------------------------------
def unit_pos(port: int, uid: int):
    r = send_json(port, f"local i=unit.getInfo({uid}); "
                        f"return i and {{x=i.gridX,y=i.gridY}} or nil")
    if isinstance(r, dict) and "x" in r:
        return float(r["x"]), float(r["y"])
    return None


def paired_positions(port: int, a: int, b: int) -> dict:
    """Both travellers' positions in ONE console round trip.

    Two `unit_pos` calls are two round trips with the simulation running
    in between, so a condition evaluated over them combines coordinates
    from different moments — "both at the ruin" could pass for a
    pair that was never inside it together. One request reads both from
    the same unit-manager state instead."""
    lua = (f"local function f(u) local i=unit.getInfo(u); "
           f"return i and (i.gridX..','..i.gridY) or 'nil' end; "
           f"return f({a})..';'..f({b})")
    raw = send(port, lua).strip().strip('"')
    out: dict = {a: None, b: None}
    parts = raw.split(";")
    for uid, part in zip((a, b), parts):
        if "," not in part:
            continue
        try:
            x, y = part.split(",")
            out[uid] = (float(x), float(y))
        except ValueError:
            continue
    return out


def current_action(port: int, uid: int) -> str:
    return send(port, f"local s=require('scripts.unit_ai').getState({uid}); "
                      f"return s and s.currentAction or 'nil'")


def pose(port: int, uid: int) -> str:
    return send(port, f"return unit.getPose({uid})")


def event_log(port: int) -> list:
    data = send_json(port, "return engine.getEventLog()")
    return data if isinstance(data, list) else []


def inventory(port: int, uid: int) -> list:
    """A unit's loose inventory with every stable per-instance property.

    `temp` is deliberately excluded: it is present only while an item is
    off ambient, so it can legitimately appear or vanish on its own."""
    lua = (f"local out={{}}; for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
           f"out[#out+1]={{defName=it.defName,instanceId=it.instanceId,"
           f"displayName=it.displayName,weight=it.weight,fill=it.currentFill,"
           f"sharpness=it.sharpness,broken=it.broken,contentsKey=it.contentsKey,"
           f"quality=it.quality,condition=it.condition}} end; "
           f"return out")
    data = send_json(port, lua)
    return data if isinstance(data, list) else []


#: The per-instance properties compared across the round trip.
#:
#: An explicit shared key set, not "every field the surface returned":
#: `building.getStorage` joins the item DEF into each row (category,
#: material, kind, iconTex, the weapon block, ...) while
#: `unit.getInventory` returns a narrower per-instance view, and the two
#: spell the container level differently (`currentFill` vs `fill`).
#: Comparing raw dicts would therefore fail on shape rather than on
#: substance. `broken` is deliberately absent: it is an inventory-only
#: field, so a storage row cannot be asked about it, and including it
#: would compare a real False against a missing key. `temp` is excluded
#: too — it is present only while an item is off ambient, so it can
#: legitimately appear or vanish on its own.
PROP_KEYS = ("defName", "displayName", "weight", "sharpness",
             "contentsKey", "quality", "condition")


def properties(item: dict) -> dict:
    """The instance's identity-independent properties, normalised so an
    inventory row and a colony-storage row are comparable."""
    item = item or {}
    out = {k: item.get(k) for k in PROP_KEYS}
    fill = item.get("fill")
    out["fill"] = item.get("currentFill") if fill is None else fill
    return out


def num(value, default: float = 0.0) -> float:
    """`value or default` is wrong for these metrics: a stomach that has
    genuinely drained to 0.0 is falsy, and substituting a default there
    silently inverts the control comparison."""
    return default if value is None else float(value)


def find_instance(items: list, inst_id):
    for it in items:
        if it.get("instanceId") == inst_id:
            return it
    return None


def stat_fraction(port: int, uid: int, stat: str, max_stat: str):
    cur = _as_float(send(port, f"return unit.getStat({uid},'{stat}')"))
    mx = _as_float(send(port, f"return unit.getStat({uid},'{max_stat}')"))
    if cur is None or not mx or mx <= 0:
        return None
    return cur / mx


def vitals(port: int, uid: int) -> dict:
    """The two survival metrics the control run is scored on, plus the
    pose that would explain a zero."""
    return {
        "hydration": stat_fraction(port, uid, "hydration", "max_hydration"),
        "stomach": stat_fraction(port, uid, "hunger", "max_hunger"),
        "load": load_fraction(port, uid),
        "pose": pose(port, uid),
    }


def fmt_vitals(v: dict) -> str:
    def pct(x):
        return "n/a" if x is None else f"{x * 100:.0f}%"
    return (f"hydration {pct(v['hydration'])}, stomach {pct(v['stomach'])}, "
            f"load {pct(v.get('load'))} of capacity, pose {v['pose']}")


# --------------------------------------------------------------------------
# Locations, per-unit knowledge, and the clearance surfaces
# --------------------------------------------------------------------------
def known_locations(port: int, uid: int) -> set[str]:
    """(#915) The per-unit location memories `uid` holds, as a set of
    "<page>#<instance id>" keys, read through the same public query
    surface the AI candidates use. Flattened to a string because an
    empty Lua table serializes identically to an empty object."""
    lua = (f"local ai=require('scripts.unit_ai'); local out={{}}; "
           f"for _,k in ipairs(ai.getKnownLocations({uid})) do "
           f"out[#out+1]=k.page..'#'..tostring(k.id) end; "
           f"return table.concat(out, ',')")
    raw = send(port, lua).strip().strip('"')
    return {p for p in raw.split(",") if p}


def placed(port: int, page: str) -> list:
    data = send_json(port, f"return world.listPlacedLocations('{page}')")
    return data if isinstance(data, list) else []


def instance_by_id(port: int, page: str, inst_id: int):
    data = send_json(port, f"return world.getLocationInstance({inst_id}, '{page}')")
    return data if isinstance(data, dict) else None


# --------------------------------------------------------------------------
# Tutorial progress, read through the module's own public surface
# --------------------------------------------------------------------------
PROGRESS_LUA = (
    "local tp = require('scripts.tutorial_progress'); "
    "local ck = {}; "
    "for _, id in ipairs(tp.index and tp.index.order or {}) do "
    "if tp.isSubobjectiveChecked(id) then ck[#ck+1] = id end end; "
    "return table.concat(tp.completedIds(), ',') .. '#' "
    ".. table.concat(ck, ',')"
)


def progress(port: int) -> tuple[set[str], set[str]]:
    """(completed full objectives, checked subobjectives). Read as a
    delimited string rather than a JSON table so an empty set and an
    empty object stay distinguishable."""
    raw = send(port, PROGRESS_LUA, timeout=15.0)
    completed, _, checked = raw.partition("#")
    return ({c for c in completed.split(",") if c},
            {c for c in checked.split(",") if c})


def roster(port: int) -> dict[str, list[int]]:
    """Live player-faction units by def, from the AI state table — the
    page-agnostic enumeration surface, keyed by uid."""
    lua = ("local ai=require('scripts.unit_ai'); local out={}; "
           "for uid,_ in pairs(ai.aiState or {}) do "
           "if unit.exists(uid) and unit.getFaction(uid)=='player' then "
           "local i=unit.getInfo(uid); "
           "if i then out[#out+1]=uid..':'..tostring(i.defName) end end end; "
           "return table.concat(out, ',')")
    raw = send(port, lua, timeout=20.0)
    out: dict[str, list[int]] = {}
    for part in raw.split(","):
        if ":" not in part:
            continue
        uid, defname = part.split(":", 1)
        try:
            out.setdefault(defname, []).append(int(uid))
        except ValueError:
            continue
    for v in out.values():
        v.sort()
    return out


# --------------------------------------------------------------------------
# Deterministic geometry over what the readers above returned
# --------------------------------------------------------------------------
def is_adjacent(pos, foot) -> bool:
    bx, by, tw, th = foot
    ux, uy = int(math.floor(pos[0])), int(math.floor(pos[1]))
    dx = bx - ux if ux < bx else (ux - (bx + tw - 1) if ux >= bx + tw else 0)
    dy = by - uy if uy < by else (uy - (by + th - 1) if uy >= by + th else 0)
    return max(dx, dy) <= 1


def carried(port: int, uid: int) -> tuple[float, int]:
    """(litres of water, ration count) — measured the way
    scripts/tutorial_eval.lua measures it."""
    raw = send(port,
               f"local l,r=0,0; "
               f"for _,it in ipairs(unit.getInventory({uid}) or {{}}) do "
               f"if it.holds=='water' then l=l+(tonumber(it.currentFill) or 0) end; "
               f"if it.defName=='{RATIONS_DEF}' then r=r+1 end end; "
               f"return l..','..r", timeout=20.0)
    try:
        litres, rations = raw.split(",")
        return float(litres), int(float(rations))
    except ValueError:
        return 0.0, 0


def load_fraction(port: int, uid: int):
    """Carried weight as a fraction of this unit's carrying capacity."""
    carried = _as_float(send(port, f"return unit.getCarryingWeight({uid})"))
    cap = _as_float(send(port, f"return unit.getStat({uid},'carrying_capacity')"))
    if carried is None or not cap or cap <= 0:
        return None
    return carried / cap


def significant_rows(port: int, instance_id: int) -> list:
    """#917's guaranteed significant obligations for one placed ruin, in
    slot order — read straight from the engine's own reported field, not
    re-derived here."""
    inst = instance_by_id(port, PAGE, instance_id)
    rows = (inst or {}).get("significant") or []
    return sorted(rows, key=lambda r: r.get("slot", 0))


def clearance_events(port: int) -> list:
    """Every player-facing clearance notice on the log so far. #917
    promises exactly ONE per location, so the interesting assertion is
    always a count."""
    return [e for e in event_log(port)
            if e.get("category") == "location_clearance"]


def arrival_box(ruin: dict):
    """The region this probe counts as "at the ruin", as inclusive
    absolute tile bounds: its stored bounds (#777) grown by
    MAX_SIGHT_TILES.

    #1230 removed the discovery_margin this used to expand by. The
    replacement is deliberately the SIGHT bound rather than a rewritten
    constant, because that is what the measurement below needs: the
    unprepared-control comparison is taken once BOTH travellers are at
    the ruin, and a traveller is only meaningfully "there" once it is
    close enough to have revealed it. A box smaller than the sight
    radius would let a unit reveal the ruin from outside the box the
    probe waits on, and the paired snapshot would never be taken."""
    b = ruin.get("bounds") or {}
    m = MAX_SIGHT_TILES
    return (int(b["min_x"]) - m, int(b["min_y"]) - m,
            int(b["max_x"]) + m, int(b["max_y"]) + m)


def in_arrival_box(pos, box) -> bool:
    x0, y0, x1, y1 = box
    return x0 <= pos[0] <= x1 and y0 <= pos[1] <= y1


def find_instance_by_def(items: list, def_name: str, exclude: set):
    for it in items:
        if it.get("defName") == def_name and it.get("instanceId") not in exclude:
            return it
    return None
