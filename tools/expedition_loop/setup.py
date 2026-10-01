#!/usr/bin/env python3
"""[setup] — a real world, a real ruin, and a real colony (#2092).

The stage that creates everything the rest of the run measures, and the
only one whose fixture mutation is allowed to be direct: the world
itself, the seed-stable site and ruin choice, the portal and the roster
IT spawns, colony storage, and the ruin's own loot rolls.

Site selection is a total order over the world's own deterministic
placement list, so the ruin and the colony are a function of the seed
alone. Nothing here spawns a unit, stages an item, or writes a
lifecycle: the portal's own sequencer delivers the party
(`scripts/building_spawn.lua`) and the ruin's own `ruin_common` rolls
are the loot. The one test switch it throws is the sequencer's
standard roster (#2756), BEFORE the portal exists: every unit the portal
delivers then takes its definition's base/mean values instead of a
gameplay roll, so the two travellers carry the same capacity every run
rather than, now and then, one too weak to carry its rations.

LOOT SELECTION lives here rather than with the extraction owner because
requirement 9 puts it here: "setup selects and creates the real world,
ruin, colony, portal roster, storage, and seed-stable loot". `loot_in`
and `choose_target` have exactly one caller, `run` below, and placing
them with the extraction stage would make the setup owner import a LATER
stage's module for a decision setup makes.
"""
from __future__ import annotations

import math

from probelib import poll_until, send, send_json

from .constants import (ACOLYTE_DEF, HOME_MAX_DIST, HOME_MIN_DIST,
                        MAX_CORRIDOR_STEP, MULE_DEF, OCCUPIED_MAX_DIST,
                        OCCUPIED_MAX_STEP, OCCUPIED_MAX_WET_TILES, PAGE,
                        PORTAL_DEF, STANDARD_ACOLYTE_CAPACITY,
                        STANDARD_LEAN_FRACTION, STORAGE_DEF, WATER_MAX_DIST)
from .day_budget import safe
from .harness import Checks, ExpeditionState, StageAbort
from .readers import (_as_float, arrival_box, dist, ground_items,
                      instance_by_id, load_region, placed, roster,
                      significant_rows, surface_z)


# --------------------------------------------------------------------------
# Site selection
# --------------------------------------------------------------------------
#: Tiles per chunk side, for the chunk arithmetic `pick_occupied` does
#: without asking the engine.
CHUNK_TILES = 16


def line_tiles(x0: int, y0: int, x1: int, y1: int) -> list:
    """The tiles `corridor_profile` samples between two tiles, in order —
    the same rounding, so a truncation computed here lands on a tile the
    engine-side walk also visits."""
    n = int(max(abs(x1 - x0), abs(y1 - y0), 1))
    return [(int(math.floor(x0 + (x1 - x0) * i / n + 0.5)),
             int(math.floor(y0 + (y1 - y0) * i / n + 0.5)))
            for i in range(n + 1)]


def chunk_gap(a, b) -> int:
    """Chebyshev distance between two chunk coordinates."""
    return max(abs(a[0] - b[0]), abs(a[1] - b[1]))


def corridor_profile(port: int, x0: int, y0: int, x1: int, y1: int):
    """(largest surface-Z step between consecutive DRY tiles, wet tiles)
    along the straight line between two tiles, sampled at every tile — or
    None if any tile is unresolved.

    A wet tile is skipped rather than refused: a stream a party wades is
    not a wall, and the step is measured across it from the last dry
    tile, so a gorge with a river in it still reads as the gorge."""
    samples = int(max(abs(x1 - x0), abs(y1 - y0), 1))
    # Every coordinate is parenthesised: a negative one spliced after a
    # minus sign would otherwise read `x - -8` as `x --8`, a Lua comment
    # that swallows the rest of the line.
    lua = (f"local x0,y0,x1,y1=({x0}),({y0}),({x1}),({y1}); "
           f"local worst=0; local prev=nil; local wet=0; "
           f"for i=0,{samples} do local t=i/{samples}; "
           f"local x=math.floor(x0+(x1-x0)*t+0.5); "
           f"local y=math.floor(y0+(y1-y0)*t+0.5); "
           f"local sz=(world.getSurfaceAt(x,y)); local f=world.getFluidAt(x,y); "
           f"if not sz then return 'unresolved' end; "
           f"if f then wet=wet+1 else "
           f"if prev then local d=math.abs(sz-prev); if d>worst then worst=d end end; "
           f"prev=sz end end; return worst..','..wet")
    raw = send(port, lua, timeout=60.0)
    step, _, wet = raw.partition(",")
    step_v, wet_v = _as_float(step), _as_float(wet)
    if step_v is None or wet_v is None:
        return None
    return step_v, int(wet_v)


def site_candidates(port: int, gx: int, gy: int, rz: float) -> list:
    """Every colony candidate around one ruin, scanned SERVER-side in a
    single console round trip.

    A candidate is a dry, z-compatible, portal-eligible tile on the
    HOME_MIN_DIST..HOME_MAX_DIST ring whose straight corridor to the ruin
    is walkable and which has lake or river water within WATER_MAX_DIST.
    Portal eligibility is asked of `building.canPlaceAt` — the same
    validator `building.spawn` runs, and the one that refuses a starting
    building inside a placed location's bounds (#777).

    Returned as dicts sorted into a TOTAL order (water distance, then
    corridor roughness, then the tile itself), so the selection is a
    function of the seed alone."""
    lua = (
        "local function rough(x0,y0,x1,y1) "
        " local worst=0; local prev=nil; "
        " for i=0,24 do local t=i/24; "
        "  local x=math.floor(x0+(x1-x0)*t); local y=math.floor(y0+(y1-y0)*t); "
        "  local sz=(world.getSurfaceAt(x,y)); local f=world.getFluidAt(x,y); "
        "  if not sz or f then return -1 end; "
        "  if prev then local d=math.abs(sz-prev); if d>worst then worst=d end end; "
        "  prev=sz end; return worst end; "
        "local function water(x,y) "
        f" for d=1,{WATER_MAX_DIST} do for a=0,31 do local ang=a*math.pi/16; "
        "  local px=math.floor(x+d*math.cos(ang)); "
        "  local py=math.floor(y+d*math.sin(ang)); "
        "  local f=world.getFluidAt(px,py); "
        "  if f=='lake' or f=='river' then return d,px,py end end end; "
        " return -1,0,0 end; "
        f"local out={{}}; local seen={{}}; "
        f"for rr={HOME_MIN_DIST},{HOME_MAX_DIST} do for a=0,143 do "
        " local ang=a*math.pi/72; "
        f" local x=math.floor({gx}+rr*math.cos(ang)); "
        f" local y=math.floor({gy}+rr*math.sin(ang)); "
        " local k=x..','..y; if not seen[k] then seen[k]=true; "
        "  local sz=(world.getSurfaceAt(x,y)); local f=world.getFluidAt(x,y); "
        f"  if sz and not f and math.abs(sz-{rz})<=2 "
        f"     and building.canPlaceAt('{PORTAL_DEF}',x,y) then "
        f"   local g=rough(x,y,{gx},{gy}); "
        f"   if g>=0 and g<={MAX_CORRIDOR_STEP} then "
        "     local wd,wx,wy=water(x,y); if wd>0 then "
        "      out[#out+1]=x..','..y..','..sz..','..g..','..wd..','..wx..','..wy "
        "     end end end end end end; return table.concat(out,';')")
    raw = send(port, lua, timeout=900.0)
    rows = []
    for part in raw.split(";"):
        if not part:
            continue
        try:
            x, y, z, g, wd, wx, wy = part.split(",")
        except ValueError:
            continue
        rows.append({"x": int(x), "y": int(y), "z": float(z),
                     "rough": float(g), "water_dist": int(wd),
                     "wx": int(wx), "wy": int(wy)})
    rows.sort(key=lambda r: (r["water_dist"], r["rough"], r["x"], r["y"]))
    return rows


def shore_tile(port: int, wx: int, wy: int):
    """A dry, resolvable tile adjacent to the water tile — where a scout
    can actually stand while the lake comes into its field of view."""
    lua = (f"for _,o in ipairs({{{{1,0}},{{-1,0}},{{0,1}},{{0,-1}},"
           f"{{2,0}},{{-2,0}},{{0,2}},{{0,-2}},{{2,2}},{{-2,-2}}}}) do "
           f"local x={wx}+o[1]; local y={wy}+o[2]; "
           f"local sz=(world.getSurfaceAt(x,y)); local f=world.getFluidAt(x,y); "
           f"if sz and not f then return x..','..y end end; return 'none'")
    raw = send(port, lua, timeout=30.0)
    if raw == "none" or "," not in raw:
        return None
    x, y = raw.split(",")
    return int(x), int(y)


def pick_site(chk: Checks, port: int, day=None):
    """Choose the ruin and the colony site.

    Zero-occupant ruins are considered in `world.listPlacedLocations`' own order —
    which is the deterministic overlay order that allocated their
    instance ids (#911) — and the first one that yields a candidate wins.
    Returns (ruin, site) or None."""
    all_ruins = poll_until(60.0, lambda: [
        e for e in placed(port, PAGE)
        if isinstance(e, dict) and e.get("id") == "ruin_small"])
    if not all_ruins:
        chk.fail_setup("the world places at least one ruin_small")
        return None
    ruins = [e for e in all_ruins
             if int((e.get("encounter") or {}).get("rolled_count", -1)) == 0]
    print(f"  {len(all_ruins)} ruin_small placed: "
          f"{[(e['instance_id'], e['gx'], e['gy'], (e.get('encounter') or {}).get('rolled_count')) for e in all_ruins]}", flush=True)
    if not ruins:
        chk.fail_setup("the world places a zero-occupant ruin_small so hostile "
                       "combat cannot confound the food-control journey")
        return None

    for ruin in ruins:
        gx, gy = int(ruin["gx"]), int(ruin["gy"])
        load_region(port, int(ruin["cx"]), int(ruin["cy"]), day=day)
        rz = surface_z(port, gx, gy)
        if rz is None:
            print(f"  ruin {ruin['instance_id']} ({gx},{gy}): anchor unresolved, "
                  f"trying next", flush=True)
            continue
        cands = site_candidates(port, gx, gy, rz)
        if not cands:
            print(f"  ruin {ruin['instance_id']} ({gx},{gy}) z={rz}: no colony "
                  f"site with walkable corridor and water within "
                  f"{WATER_MAX_DIST} tiles, trying next", flush=True)
            continue
        site = cands[0]
        shore = shore_tile(port, site["wx"], site["wy"])
        if shore is None:
            print(f"  ruin {ruin['instance_id']}: no dry shore beside "
                  f"({site['wx']},{site['wy']}), trying next", flush=True)
            continue
        site["shore"] = shore
        print(f"  site: ruin instance {ruin['instance_id']} at ({gx},{gy}) z={rz}; "
              f"colony ({site['x']},{site['y']}) z={site['z']} at "
              f"{dist((site['x'], site['y']), (gx, gy)):.1f} tiles "
              f"(corridor roughness {site['rough']}); water "
              f"({site['wx']},{site['wy']}) {site['water_dist']} tiles away, "
              f"shore {shore}", flush=True)
        return ruin, site
    chk.fail_setup(f"a ruin with a portal-eligible colony site "
                   f"{HOME_MIN_DIST}..{HOME_MAX_DIST} tiles away across walkable "
                   f"ground, with water within {WATER_MAX_DIST} tiles, exists")
    return None


def pick_occupied(chk: Checks, port: int, ruin: dict, site: dict,
                  day=None):
    """Choose the second, OCCUPIED ruin the confrontation leg goes to.

    Candidates are the placed `ruin_small` instances whose persisted
    encounter roll is at least one, in a total order — distance from the
    colony, then instance id — so the choice is a function of the seed
    alone. The first one within OCCUPIED_MAX_DIST whose straight
    corridors from the colony AND from the zero-occupant ruin are
    resolvable, cross no more than OCCUPIED_MAX_WET_TILES of water and
    step no higher than OCCUPIED_MAX_STEP between dry tiles wins,
    sampled at every tile. Placement legality says nothing about whether
    a party can walk there, and the pathfinder will happily route a
    party up a cliff it does not survive (observed on seed 42's nearest
    occupied ruin, behind a 12-to-30-level gorge: both acolytes sent to
    it died on the way).

    Each corridor is read only up to the edge of the occupied ruin's own
    CHUNK, and nothing here pages that chunk in. A location's contents
    spawn the first time its chunk loads, and a neglected
    `nomad_primitive` turns delirious and dies of its own physiology
    within about twenty game-minutes (see docs/engine_contracts.md §The
    expedition loop), so its contents are left to spawn as the
    confrontation party sets out (`encounter.run` pages the ruin in).
    The unread tail is the ruin's own chunk, which its `anchor: [flat]`
    placement constraint already holds flat.

    Returns the placement row, or None."""
    home = (int(site["x"]), int(site["y"]))
    rx, ry = int(ruin["gx"]), int(ruin["gy"])
    cands = sorted(
        (e for e in placed(port, PAGE)
         if isinstance(e, dict) and e.get("id") == "ruin_small"
         and int((e.get("encounter") or {}).get("rolled_count", 0)) >= 1),
        key=lambda e: (dist(home, (int(e["gx"]), int(e["gy"]))),
                       int(e["instance_id"])))
    for occ in cands:
        gx, gy = int(occ["gx"]), int(occ["gy"])
        far = dist(home, (gx, gy))
        if far > OCCUPIED_MAX_DIST:
            break
        ruin_chunk = (gx // CHUNK_TILES, gy // CHUNK_TILES)
        profiles = []
        for (x0, y0) in (home, (rx, ry)):
            tiles = line_tiles(x0, y0, gx, gy)
            outside = []
            for x, y in tiles:
                if chunk_gap((x // CHUNK_TILES, y // CHUNK_TILES),
                             ruin_chunk) == 0:
                    break
                outside.append((x, y))
            # Page in the chunks this stretch crosses, each with its
            # neighbours — but only from chunks two or more away from the
            # ruin's, so no neighbourhood reaches it.
            for c in dict.fromkeys((x // CHUNK_TILES, y // CHUNK_TILES)
                                   for x, y in outside):
                if chunk_gap(c, ruin_chunk) >= 2:
                    load_region(port, c[0], c[1], pad=1, day=day)
            end = outside[-1] if outside else (x0, y0)
            profiles.append(corridor_profile(port, x0, y0, end[0], end[1]))
        ok = all(p is not None and p[0] <= OCCUPIED_MAX_STEP
                 and p[1] <= OCCUPIED_MAX_WET_TILES for p in profiles)
        print(f"  occupied ruin {occ['instance_id']} at ({gx},{gy}), rolled "
              f"{occ['encounter']['rolled_count']}: {far:.0f} tiles from the "
              f"colony; (largest step, wet tiles) from colony / first ruin "
              f"{profiles} -> {'usable' if ok else 'not walkable'}", flush=True)
        if ok:
            return occ
    chk.fail_setup(f"an OCCUPIED ruin_small (encounter roll >= 1) lies within "
                   f"{OCCUPIED_MAX_DIST} tiles of the colony across walkable "
                   f"ground from both the colony and the zero-occupant ruin "
                   f"(steps <= {OCCUPIED_MAX_STEP}, <= "
                   f"{OCCUPIED_MAX_WET_TILES} wet tiles)")
    return None


# --------------------------------------------------------------------------
# The colony
# --------------------------------------------------------------------------
def enable_standard_roster(chk: Checks, port: int) -> bool:
    """Switch the portal sequencer's TEST-ONLY standard roster on
    (#2756) before the portal is placed, so every roster spawn asks
    `unit.spawn` for the standard profile. Read back rather than
    assumed: a silently-off switch would quietly restore rolled units."""
    got = send(port, "local BS=require('scripts.building_spawn'); "
                     "BS.setTestStandardProfile(true); "
                     "return tostring(BS.standardProfile)", timeout=20.0)
    return chk.ok(got == "true",
                  f"the portal sequencer's standard roster is switched on "
                  f"before the portal exists (standardProfile -> {got!r})")


#: The body stats `snapshot_spawn_body` reads in ONE Lua round trip:
#: carrying_capacity is derived from the other three plus height
#: (Unit.Thread.Command.Body.recomputeBodyDerivedStats), so while they
#: still stand at their spawn seed the capacity is still the spawn one.
SPAWN_BODY_STATS = ("carrying_capacity", "lean_mass", "frame_mass",
                    "strength_base")

#: How far (kg) a first-sight lean_mass may sit from its spawn seed and
#: still count as pre-physiological — see `record_traveller_capacities`.
PRISTINE_LEAN_TOLERANCE = 1e-4


def snapshot_spawn_body(port: int, uid: int) -> dict | None:
    """One unit's capacity and the body stats it derives from, read in a
    single call so they describe one instant. None until the unit's
    stats exist (its spawn command has not been drained yet)."""
    raw = send(port, "local u=" + str(uid) + "; local out={}; "
               "for _,k in ipairs({" + ",".join(f"'{k}'" for k in SPAWN_BODY_STATS)
               + "}) do local v=unit.getStat(u,k); if v==nil then return 'nil' end; "
               "out[#out+1]=string.format('%.6f',v) end; "
               "return table.concat(out,'|')")
    parts = (raw or "").split("|")
    if len(parts) != len(SPAWN_BODY_STATS):
        return None
    try:
        return dict(zip(SPAWN_BODY_STATS, (float(p) for p in parts)))
    except ValueError:
        return None


def record_traveller_capacities(chk: Checks, st: ExpeditionState,
                                spawn_bodies: dict, travellers: list) -> None:
    """Both travellers' INITIAL carrying capacity (#2756), from the
    snapshot `await_roster` took the first time each acolyte was seen —
    not a read after the whole roster wait, while the simulation ran on.

    The snapshot is proved to predate any physiological change rather
    than assumed to: capacity is re-derived only from lean_mass and
    strength_base (with height), and a standard acolyte spawns with
    strength_base at its definition base and lean_mass at
    STANDARD_LEAN_FRACTION of its frame. Both still standing there means
    no catabolism or regrowth has touched the body, so the capacity read
    in the same call is the spawn value.

    "Still standing there" is a tolerance, not equality: physiology
    starts on the first tick, and a live run's first-sight lean_mass is
    already ~1e-5 kg off its seed. PRISTINE_LEAN_TOLERANCE (10x that)
    bounds the capacity to within ~1e-4 kg of its spawn value, so the
    FINGERPRINT rounds to 2 decimals — nowhere near a rounding boundary
    for the standard 23.887 kg — and two runs must print the same pair.
    Three decimals would sit 0.0002 kg from one, and a late read of the
    same unit already rounds differently."""
    snaps = [spawn_bodies.get(u) for u in travellers]
    pristine = [s is not None
                and abs(s["strength_base"] - 1.0) < 1e-4
                and abs(s["lean_mass"] - STANDARD_LEAN_FRACTION * s["frame_mass"])
                    < PRISTINE_LEAN_TOLERANCE
                for s in snaps]
    caps = [None if s is None else s["carrying_capacity"] for s in snaps]
    rounded = [None if c is None else round(c, 2) for c in caps]
    st.fp["traveller_capacity"] = rounded
    chk.ok(all(pristine),
           f"both travellers' capacities were captured at first sight, "
           f"before any physiological change: lean_mass still at its "
           f"spawn seed and strength_base at its base "
           f"(snapshots {snaps})")
    chk.ok(all(c is not None and abs(c - STANDARD_ACOLYTE_CAPACITY) < 0.01
               for c in caps),
           f"both travellers spawn as STANDARD acolytes with the base "
           f"capacity {STANDARD_ACOLYTE_CAPACITY} kg (prepared, control -> "
           f"{rounded})")


def place_portal(chk: Checks, port: int, gx: int, gy: int) -> int:
    valid = send(port, f"return tostring(building.canPlaceAt('{PORTAL_DEF}',{gx},{gy}))",
                 timeout=20.0)
    if not chk.ok(valid == "true",
                  f"the chosen colony tile ({gx},{gy}) accepts the acolyte portal "
                  f"through the real placement validator (canPlaceAt -> {valid!r})"):
        return -1
    raw = send(port, f"return tostring(building.spawn('{PORTAL_DEF}',{gx},{gy}))",
               timeout=20.0)
    try:
        bid = int(float(raw))
    except (TypeError, ValueError):
        chk.ok(False, f"building.spawn('{PORTAL_DEF}') at ({gx},{gy}) -> {raw!r}")
        return -1
    chk.ok(bid > 0, f"the acolyte portal is placed at ({gx},{gy}) (bid {bid})")
    return bid


def await_roster(chk: Checks, port: int, bid: int, spawn_bodies: dict):
    """Wait for the portal's OWN spawn sequencer to deliver its roster
    (scripts/building_spawn.lua: five acolytes then the technomule that
    hauls the colony's stock). Nothing here spawns a unit.

    Every acolyte is snapshotted into `spawn_bodies` the first poll it
    is seen (`snapshot_spawn_body`), so its spawn-time capacity is not
    left to whenever the LAST roster entry happens to walk out."""
    remaining = _as_float(send(port, f"return building.getSpawnRemaining({bid})"))

    def poll():
        r = roster(port)
        for uid in r.get(ACOLYTE_DEF, []):
            if uid not in spawn_bodies:
                snap = snapshot_spawn_body(port, uid)
                if snap is not None:
                    spawn_bodies[uid] = snap
        return r if (len(r.get(ACOLYTE_DEF, [])) >= 5
                     and len(r.get(MULE_DEF, [])) >= 1) else None
    got = poll_until(240.0, poll, interval=0.5)
    if got is None:
        got = safe(lambda: roster(port), {})
        chk.fail_setup(
            f"the portal's own spawn roster delivers five acolytes and the "
            f"technomule (got {[(k, len(v)) for k, v in sorted(got.items())]}, "
            f"spawnRemaining "
            f"{safe(lambda: send(port, f'return building.getSpawnRemaining({bid})'))!r}, "
            f"seeded from {remaining})")
        return None
    chk.ok(True, f"the portal spawns its own roster: "
                 f"{len(got[ACOLYTE_DEF])} acolytes + "
                 f"{len(got[MULE_DEF])} technomule (uids "
                 f"{sorted(got[ACOLYTE_DEF])} / {sorted(got[MULE_DEF])})")
    return got


def retire_find_water(chk: Checks, st: ExpeditionState, uids) -> bool:
    """Retire the standing find_water goal on every acolyte — the same
    scenario condition every probe in tools/ applies, and applied here
    to the whole colony so the two travellers are treated alike.

    Each unit's wait is for its AI state to exist. One that never does
    is a failing check with its own timeout diagnostics (#2755), not a
    silently skipped scenario condition."""
    port = st.port
    unretired = []
    for uid in uids:
        t0 = st.day.now()
        done = poll_until(10.0, lambda u=uid: send(
            port, f"local ai=require('scripts.unit_ai'); local s=ai.getState({u}); "
                  f"if not s then return false end; "
                  f"ai.markGoalAccomplished(s,'find_water'); return true") == "true")
        if not done:
            unretired.append((uid, t0))
    ok = chk.ok(not unretired,
                f"the standing find_water goal is retired on every acolyte "
                f"{sorted(uids)} (no AI state within 10 s: "
                f"{[u for u, _t in unretired] or 'none'})")
    for uid, t0 in unretired:
        st.day.timed_out(f"acolyte {uid}'s AI state, to retire its "
                         f"find_water goal", [uid], t0)
    return ok


def build_storage(chk: Checks, port: int, hx: int, hy: int) -> int:
    """The colony's storage: a real cargo_hold_S beside the portal.

    Finished immediately on purpose. A cargo_hold_S spawns Constructing
    (build_work 240) and an unfinished building is a CONSTRUCTION SITE:
    build_nearby and deliver_to_build_site (utility 6.0, with a lock-in)
    would pull the whole colony — travellers included — off to finish it.
    Fixture setup, clearly separated: the colony's store is meant to be
    a building that already exists."""
    spot = None
    for dx, dy in ((2, 0), (-2, 0), (0, 2), (0, -2), (2, 2), (-2, -2)):
        x, y = hx + dx, hy + dy
        if send(port, f"return tostring(building.canPlaceAt('{STORAGE_DEF}',{x},{y}))",
                timeout=20.0) == "true":
            spot = (x, y)
            break
    if spot is None:
        chk.fail_setup(f"a tile beside the portal accepts {STORAGE_DEF}")
        return -1
    # Spawned AND finished inside one PAUSED window. The AI tick is
    # pause-gated, so no colonist ever sees the building's brief
    # `constructing` ghost: a deliver job planned against it in that
    # instant goes on fetching the hold's materials — on seed 14 that
    # includes the first ruin's guaranteed `processing_unit` off the
    # ground, which the hold's material list then consumes (observed,
    # #2640). Same fixture, the race closed.
    send(port, "engine.setPaused(true); return 'ok'")
    raw = send(port, f"return building.spawn('{STORAGE_DEF}',{spot[0]},{spot[1]})")
    try:
        bid = int(float(raw))
    except (TypeError, ValueError):
        send(port, "engine.setPaused(false); return 'ok'")
        chk.fail_setup(f"{STORAGE_DEF} spawned beside the portal (got {raw!r})")
        return -1
    required = _as_float(send(port, f"return building.getBuildRequired({bid})")) or 240.0
    send(port, f"building.addBuildProgress({bid}, {required + 1.0}); return 'ok'")
    built = poll_until(30.0, lambda: send(
        port, f"return building.getActivity({bid})") == "built")
    send(port, "engine.setPaused(false); return 'ok'")
    cap = safe(lambda: _as_float(send(
        port, f"return building.getStorageCapacity({bid})")), None)
    if not chk.ok(bool(built) and (cap or 0) > 0,
                  f"the colony has finished storage at {spot} "
                  f"(bid {bid}, activity "
                  f"{safe(lambda: send(port, f'return building.getActivity({bid})'))!r}, "
                  f"capacity {cap})"):
        return -1
    return bid


def adjacent_tile(port: int, bid: int):
    """A tile satisfying the storage menu's own adjacency rule
    (Chebyshev <= 1 from the building's footprint)."""
    info = send_json(port, f"return building.getInfo({bid})")
    if not isinstance(info, dict):
        return None, None
    bx, by = int(info.get("gridX", 0)), int(info.get("gridY", 0))
    tw, th = int(info.get("tileW", 1) or 1), int(info.get("tileH", 1) or 1)
    return (bx + tw, by + th - 1), (bx, by, tw, th)


# --------------------------------------------------------------------------
# The ruin's own seed-stable loot, and which roll is carried home
# --------------------------------------------------------------------------
def loot_in(port: int, ruin: dict) -> list:
    """The INCIDENTAL ground items lying inside one ruin's own absolute
    bounds (#777) — its `ruin_common` rolls, and nothing else.
    ruin_small declares min_spacing 5 chunks, so no two ruin footprints
    can overlap and the attribution is unambiguous.

    #917's guaranteed significant item is EXCLUDED, by the physical ids
    the instance's own `significant` rows report rather than by def
    name, so the exclusion follows provenance rather than a guess about
    what the reward happens to be. Two things depend on it:

      * the count below is a real check on the loot table. Folding the
        guaranteed item in would let a missing incidental roll hide
        behind a constant.
      * `choose_target` must never pick it. It skips Materials and food
        so the carrier's own AI cannot move the target mid-return — but
        it FALLS BACK to the first entry when both rolls are excluded,
        and `processing_unit` sorts first among this ruin's contents.
        Extracting it there would latch it and clear the ruin long
        before the extract stage asserts it is still untaken."""
    b = ruin.get("bounds") or {}
    if not b:
        return []
    # Read LIVE: `ruin` is the placement-time row from `pick_site`, taken
    # before any content spawned, so its obligations carry no ids yet.
    reserved = {r.get("item_instance_id")
                for r in significant_rows(port, int(ruin["instance_id"]))
                if r.get("item_instance_id") is not None}
    inside = [g for g in ground_items(port)
              if g.get("instanceId") not in reserved
              and b["min_x"] <= g.get("x", 1e9) <= b["max_x"]
              and b["min_y"] <= g.get("y", 1e9) <= b["max_y"]]
    return sorted(inside, key=lambda g: (g.get("defName", ""), g.get("id", 0)))


def def_traits(port: int, def_name: str) -> tuple[str, bool]:
    """(category, edible) for one item def.

    Edibility is asked of `item.getFood` — the SAME predicate
    scripts/unit_ai_needs.lua's `isFoodDef` uses — rather than of a
    field on `item.listDefs()`, which carries only name / displayName /
    category / weight."""
    lua = (f"local cat='?'; "
           f"for _,x in ipairs(item.listDefs() or {{}}) do "
           f"if x.name=='{def_name}' then cat=tostring(x.category) end end; "
           f"local f=item.getFood and item.getFood('{def_name}'); "
           f"local edible=(f~=nil) and (((f.calories or 0)>0) "
           f"or ((f.caloriesPerKg or 0)>0)); "
           f"return cat..'/'..tostring(edible)")
    raw = send(port, lua, timeout=20.0)
    cat, _, edible = raw.partition("/")
    return cat, edible == "true"


def choose_target(port: int, loot: list):
    """Which of the ruin's own rolls is carried home.

    The ruin guarantees no particular item (#921), so the target is
    whatever it rolled — but the CHOICE between its rolls is
    deterministic, and skips two def kinds whose own AI would move the
    item before the player's order does:
      * Materials — `store_materials` fires on any Materials in
        inventory (utility 3.0, and the colony's cargo is right there),
        so the carrier would bank it autonomously mid-return;
      * food — `eat_from_inventory` would consume it outright.
    Falls back to the first roll if both are excluded: the loop still
    runs, and the fingerprint records what was chosen."""
    for g in loot:
        cat, edible = def_traits(port, g.get("defName", ""))
        if cat != "Materials" and not edible:
            return g
    return loot[0] if loot else None


# --------------------------------------------------------------------------
# The stage
# --------------------------------------------------------------------------
def await_world_init(chk: Checks, st: ExpeditionState) -> None:
    """Wait for world generation, and refuse to go on if it never ends.

    `world.waitForInit` returns `world.getInitProgress`'s four values
    rather than failing when its timeout passes; the parens keep only
    the first, the load phase, which is 3 once generation is done. Its
    expiry used to pass unnoticed (#2755)."""
    t0 = st.day.now()
    phase = send(st.port, "return (world.waitForInit(400))", timeout=420.0)
    if not chk.ok(phase.strip() == "3",
                  f"world generation finishes within 400 s (load phase "
                  f"{phase!r}; 3 is done)"):
        st.day.timed_out("world generation (the world thread, not a unit, "
                         "is what is awaited)", None, t0)
        raise StageAbort("world generation did not finish")


def run(chk: Checks, st: ExpeditionState) -> None:
    """Generate the world, choose the site, plant the colony, read the
    ruin's loot.

    World generation sits inside this stage rather than in the facade
    because it IS the stage's first act, and because `Checks` starts on
    `STAGES[0]` — so a failure between the boot and the `enter` below is
    already attributed to `setup`, exactly as it was pre-split.
    """
    port = st.port
    send(port, f"world.init('{PAGE}', {st.seed}, {st.size}, "
               f"{st.plates}, 'First Expedition'); return 'ok'")
    await_world_init(chk, st)
    send(port, f"world.show('{PAGE}'); return 'ok'")

    chk.enter("setup", "a real world, a real ruin, and a real colony")
    t0 = st.day.now()
    picked = pick_site(chk, port, st.day)
    if not picked:
        st.day.timed_out("the site search (world placement and chunk "
                         "loading, not a unit, are what is awaited)", None, t0)
        raise StageAbort("no ruin with a usable colony site")
    st.ruin, st.site = picked
    ruin, site = st.ruin, st.site
    st.ruin_id = ruin_id = int(ruin["instance_id"])
    st.ruin_xy = (float(ruin["gx"]), float(ruin["gy"]))
    st.home = home = (int(site["x"]), int(site["y"]))
    st.fp.update(ruin_instance=ruin_id,
                 ruin_anchor=[int(ruin["gx"]), int(ruin["gy"])],
                 colony=list(home),
                 water=[site["wx"], site["wy"]])

    # The confrontation leg's ruin (#2640), chosen before anything moves
    # so a world that cannot host it refuses here, naming which ruin it
    # lacks, rather than half an hour later. Its chunk is deliberately
    # NOT paged in yet (see `pick_occupied`); its roster is spawned and
    # checked by `encounter.run`.
    occ = pick_occupied(chk, port, ruin, site, st.day)
    if occ is None:
        raise StageAbort("no reachable occupied ruin")
    st.occ = occ
    st.occ_id = occ_id = int(occ["instance_id"])
    st.occ_xy = (float(occ["gx"]), float(occ["gy"]))
    st.occ_box = arrival_box(occ)
    st.occ_rolled = int((occ.get("encounter") or {}).get("rolled_count", -1))
    occ_inst = instance_by_id(port, PAGE, occ_id) or {}
    chk.ok(occ_inst.get("lifecycle") == "unknown" and st.occ_rolled >= 1,
           f"the occupied ruin {occ_id} starts undiscovered with a persisted "
           f"encounter roll of {st.occ_rolled} (lifecycle "
           f"{occ_inst.get('lifecycle')!r}; contents_spawned "
           f"{occ_inst.get('contents_spawned')!r} — its chunk is paged in "
           f"when its leg begins)")
    st.fp.update(occupied_instance=occ_id,
                 occupied_anchor=[int(occ["gx"]), int(occ["gy"])],
                 occupied_rolled=st.occ_rolled)

    inst = instance_by_id(port, PAGE, ruin_id)
    chk.ok(isinstance(inst, dict) and inst.get("lifecycle") == "unknown",
           f"the ruin starts undiscovered — lifecycle "
           f"{(inst or {}).get('lifecycle')!r} (nothing has approached it)")
    # #917's conjunction, proved HERE because this is the only
    # moment it is guaranteed observable: nothing has been near
    # the ruin, so nothing can have recovered its guaranteed
    # item yet. The selected ruin is zero-occupant, so its
    # ENCOUNTER half has been satisfied since placement — and it
    # is still not clearance-satisfied, which is exactly what
    # the guaranteed item is authored to hold back.
    chk.ok(isinstance(inst, dict)
           and inst.get("authors_clearance") is True
           and inst.get("clearance_satisfied") is False,
           f"…and is NOT clearance-satisfied even though its "
           f"zero-nomad encounter half already is — the guaranteed "
           f"item is what holds it back "
           f"(authors={(inst or {}).get('authors_clearance')!r}, "
           f"satisfied={(inst or {}).get('clearance_satisfied')!r})")
    enc0 = (inst or {}).get("encounter") or {}
    chk.ok(int(enc0.get("rolled_count", -1)) == 0
           and enc0.get("cleared") is True,
           f"the encounter half really is complete already (rolled "
           f"{enc0.get('rolled_count')!r}, cleared "
           f"{enc0.get('cleared')!r}), so the item is the ONLY "
           f"outstanding condition")

    if not enable_standard_roster(chk, port):
        raise StageAbort("the standard roster did not switch on")
    # The day budget's reference (#2755): read at the colony immediately
    # BEFORE the portal, so it is no later than any party member's spawn.
    st.day.establish(chk, home)
    st.portal_bid = portal_bid = place_portal(chk, port, home[0], home[1])
    if portal_bid < 0:
        raise StageAbort("the colony tile refused the acolyte portal")
    spawn_bodies: dict = {}
    t0 = st.day.now()
    party = await_roster(chk, port, portal_bid, spawn_bodies)
    if not party:
        st.day.timed_out("the portal's spawn roster (its sequencer, not "
                         "a unit, is what is awaited)", None, t0)
        raise StageAbort("the portal did not deliver its roster")
    acolytes = party[ACOLYTE_DEF]
    retire_find_water(chk, st, acolytes)
    st.mule = mule = party[MULE_DEF][0]
    # Deterministic role assignment by uid order: the portal
    # spawns its roster in a fixed sequence, so these are stable.
    st.scout, st.prepared, st.control = acolytes[0], acolytes[1], acolytes[2]
    scout, prepared, control = st.scout, st.prepared, st.control
    record_traveller_capacities(chk, st, spawn_bodies, [prepared, control])
    # The remaining acolytes are the never-went-there control for
    # the per-unit KNOWLEDGE layer (#915). They are NOT held:
    # `unit.setFrozen` freezes only the render publish, so a
    # "held" colonist keeps walking under the sim while reporting
    # the position it had when the flag went up — the check would
    # be reading a stale coordinate. Instead the assertion below
    # is made eligibility-based: a colonist qualifies only if it
    # was never observed at the ruin during the leg and is
    # outside it when the check runs. A colonist that genuinely
    # wanders to the ruin has genuinely learned it, and is
    # excluded rather than counted as a leak.
    st.stay_home = stay_home = [u for u in acolytes
                                if u not in (scout, prepared, control)]
    print(f"  scout {scout}, prepared traveller {prepared}, "
          f"unprepared control {control}, technomule {mule}, "
          f"stay-at-home colonists {stay_home}", flush=True)

    t0 = st.day.now()
    st.storage_bid = storage_bid = build_storage(chk, port, home[0], home[1])
    if storage_bid < 0:
        st.day.timed_out("colony storage's construction (the building, not "
                         "a unit, is what is awaited)", None, t0)
        raise StageAbort("the colony has no finished storage")
    st.deposit_spot, st.foot = adjacent_tile(port, storage_bid)

    st.loot = loot = loot_in(port, ruin)
    # EXACTLY the two `ruin_common` rolls
    # (data/locations/ruin_small.yaml). Exact, not a floor:
    # `loot_in` now excludes #917's guaranteed item by its own
    # physical id, so a third entry here would mean an
    # unaccounted-for ground item and a missing one would mean a
    # lost roll — neither of which a `>=` could see.
    chk.ok(len(loot) == 2,
           f"the ruin spawned its own two loot-table rolls: "
           f"{[g['defName'] for g in loot]} "
           f"({len(loot)} incidental ground items inside its bounds, "
           f"the guaranteed item excluded)")
    if len(loot) != 2:
        raise StageAbort("the ruin's loot-table rolls are not accounted for")
    st.target = target = choose_target(port, loot)
    st.fp.update(loot=sorted(g["defName"] for g in loot),
                 target=target["defName"],
                 target_gid=int(target["id"]))
    print(f"  extraction target: {target['defName']} "
          f"(ground id {target['id']}) at "
          f"({target['x']:.0f},{target['y']:.0f})", flush=True)
