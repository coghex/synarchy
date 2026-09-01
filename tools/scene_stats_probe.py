#!/usr/bin/env python3
"""Scene-assembly telemetry probe — the #1921 gate.

Boots a REAL engine with ``--offscreen`` (full Vulkan render, no
window), builds a world with a deliberately created population of
units, ground items and buildings, and reads ``debug.getSceneStats()``
back through the debug console.

``--offscreen`` rather than ``--headless`` is the whole point of this
probe. The GPU-free path leaves ``rvTextureSystemRef`` empty, and
``Unit.Render`` / ``Building.Render`` / ``Structure.Render`` all return
no quads without it — so a headless run can only prove that scanned
counts and publication work, which is exactly what the hspec group
"scene assembly telemetry" already does against a synthetic fixture.
Non-zero EMITTED counts for units, ground items and buildings are only
observable with a live texture system, and that is what this gate adds.

What it checks:

1. The query answers at all, with the complete ten-row shape in the
   contract's order and identifiers.
2. The sequence advances across completed passes, and a later snapshot
   is a whole replacement rather than an accumulation.
3. Scanned counts move by exactly the population deliberately created:
   spawning N units raises ``units`` scanned by N, K buildings raise
   ``buildings`` by K, and M ground items raise ``ground_items`` by M —
   measured as a DELTA against a baseline taken before the spawns, so
   nothing has to assume the world started empty.
4. Units, ground items and buildings all EMIT quads, which is the claim
   only an offscreen run can make.
5. Every duration is a present, non-negative integer. No threshold is
   asserted: that would be a scheduler-dependent claim.
6. A Lua caller mutating the returned table cannot change what the
   engine reports next.
7. The engine exits cleanly.

Needs a GPU (Vulkan device) — manual-only, never CI-gated.

Usage: python3 tools/scene_stats_probe.py [--port 9521] [--size 1280x720]
       [--seed 42] [--world-size 64] [--keep-open]
"""
from __future__ import annotations

import argparse
import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import (boot, init_world, pin_camera_to_tile, poll_until,
                      quit_engine, send, send_json, viewport)

failures = 0

# The published contract: ten rows, this order, these identifiers.
EXPECTED_IDS = [
    "tiles", "cursor", "ground_items", "spoil", "blood",
    "units", "buildings", "structures", "ghost", "zoom_map",
]

# The deliberately created population.
N_UNITS = 3
N_ITEMS = 4
N_BUILDINGS = 2


def check(ok: bool, label: str, detail: str = "") -> bool:
    global failures
    if ok:
        print(f"  PASS  {label}")
    else:
        failures += 1
        print(f"  FAIL  {label}{(' — ' + detail) if detail else ''}")
    return bool(ok)


# --------------------------------------------------------------------------
# Telemetry access
# --------------------------------------------------------------------------
def scene_stats(port: int, timeout: float = 15.0):
    """One ``debug.getSceneStats()`` read, or None when unreadable."""
    got = send_json(port, "return debug.getSceneStats()", timeout=timeout)
    return got if isinstance(got, dict) else None


def rows_by_id(stats: dict) -> dict:
    return {r.get("id"): r for r in (stats.get("categories") or [])
            if isinstance(r, dict)}


def wait_for_later_sequence(port: int, after: int, seconds: float = 30.0):
    """The next snapshot published STRICTLY after sequence ``after``.

    Every assertion about a freshly created population has to wait for a
    pass that ran after the creation landed; reading the currently
    published snapshot would race the world thread's 16.6 ms tick.
    """
    return poll_until(seconds, lambda: (
        lambda s: s if isinstance(s, dict)
                       and s.get("available") is True
                       and int(s.get("sequence") or 0) > after
                    else None)(scene_stats(port)))


def wait_content_loaded(port: int, seconds: float = 120.0) -> bool:
    """READY precedes the startup loader: unit/building defs are EMPTY
    for the first ~10-20 s while the menu phase loads content."""
    def defs():
        got = send_json(port, "return {units = #unit.listDefs(),"
                              " buildings = #building.listDefs()}")
        return got if isinstance(got, dict) else {}
    return poll_until(seconds, lambda: (
        defs().get("units", 0) > 0 and defs().get("buildings", 0) > 0) or None
    ) is not None


# --------------------------------------------------------------------------
# Scene construction
# --------------------------------------------------------------------------
def find_placeable(port: int, cx: int, cy: int, radius: int = 16):
    """A (building def, tile) pair the ENGINE says goes together.

    Located through ``building.canPlaceAt`` against the live def
    registry, never guessed: this is a real seeded world, so a hardcoded
    coordinate — or a hardcoded def name — would turn the gate into a
    seed test. The scan walks outward in Chebyshev rings so the answer
    is the nearest fit to the requested centre.
    """
    lua = (
        "local defs = building.listDefs() or {} "
        "local cx, cy, r = %d, %d, %d "
        "for d = 0, r do "
        "  for dx = -d, d do for dy = -d, d do "
        "    if math.max(math.abs(dx), math.abs(dy)) == d then "
        "      for _, bd in ipairs(defs) do "
        "        if building.canPlaceAt(bd.name, cx + dx, cy + dy) then "
        "          return {name = bd.name, gx = cx + dx, gy = cy + dy} "
        "        end "
        "      end "
        "    end "
        "  end end "
        "end return nil" % (cx, cy, radius)
    )
    return send_json(port, lua, timeout=120.0)


def flat_tiles(port: int, cx: int, cy: int, z: int, want: int,
               radius: int = 10):
    """Up to ``want`` tiles within ``radius`` of (cx, cy) whose terrain
    surface sits at exactly ``z``.

    Everything this probe creates goes on such a tile, and the camera's
    z-slice is pinned to the same ``z``. That is not decoration: units,
    buildings and ground items are all culled against the slice band, so
    a population scattered over a generated world's real relief would
    make the EMITTED counts depend on the seed's terrain rather than on
    the telemetry.
    """
    lua = (
        "local out = {} local cx, cy, z, r = %d, %d, %d, %d "
        "for d = 0, r do "
        "  for dx = -d, d do for dy = -d, d do "
        "    if math.max(math.abs(dx), math.abs(dy)) == d then "
        "      if (world.getTerrainAt(cx + dx, cy + dy)) == z then "
        "        out[#out + 1] = {gx = cx + dx, gy = cy + dy} "
        "        if #out >= %d then return out end "
        "      end "
        "    end "
        "  end end "
        "end return out" % (cx, cy, z, radius, want)
    )
    got = send_json(port, lua, timeout=120.0)
    return got if isinstance(got, list) else []


def one_item_def(port: int):
    """The first item def name the live registry offers."""
    got = send_json(port,
                    "local d = item.listDefs() or {}; "
                    "return d[1] and {name = d[1].name} or nil", timeout=20.0)
    return got.get("name") if isinstance(got, dict) else None


def counts(port: int) -> dict:
    got = send_json(port,
                    "return {units = #(unit.getAllIds() or {}),"
                    " buildings = #(building.getActiveIds() or {}),"
                    " ground = item.groundCount()}", timeout=20.0)
    return got if isinstance(got, dict) else {}


# --------------------------------------------------------------------------
def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9521)
    ap.add_argument("--size", default="1280x720")
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--world-size", type=int, default=64)
    ap.add_argument("--keep-open", action="store_true")
    args = ap.parse_args()

    proc = boot(args.port, args=["--size", args.size], label="offscreen",
                mode=("--offscreen",))
    print(f"        engine log: {getattr(proc, '_probe_log', '?')}")
    try:
        print("phase 1: content load")
        if not check(wait_content_loaded(args.port),
                     "unit and building defs populated"):
            return 1

        print("phase 2: a real world, initialized and shown")
        # `world.waitForInit` answers with SEVERAL values (phase, chunk
        # progress, ...), so its raw text is not a boolean to compare
        # against; the active page below is the real completion check.
        init_world(args.port, name="scene_stats", seed=args.seed,
                   size=args.world_size, plates=3, show=True, timeout=300.0)
        page = poll_until(60.0, lambda: (
            lambda g: (g or {}).get("page") or None)(send_json(
                args.port, "local id = world.getActiveWorldId();"
                           " return id and {page = tostring(id)} or nil")))
        if not check(bool(page) and page != "nil", "a world page is active",
                     f"world.getActiveWorldId() = {page!r}"):
            return 1

        print("phase 3: the query answers with the contract's shape")
        stats = poll_until(60.0, lambda: (
            lambda s: s if isinstance(s, dict) and s.get("available") is True
                      else None)(scene_stats(args.port)))
        if not check(bool(stats), "debug.getSceneStats() reports available"):
            return 1
        ids = [r.get("id") for r in (stats.get("categories") or [])]
        check(ids == EXPECTED_IDS,
              "ten rows, in the contract's order, with its identifiers",
              f"got {ids!r}")
        check(int(stats.get("sequence") or 0) >= 1,
              "the sequence has advanced past the unavailable state",
              f"sequence = {stats.get('sequence')!r}")

        print("phase 4: place the camera on a placeable tile")
        # The tile under the camera, from the engine's own pick — never a
        # computed world centre. Chunks stream in around wherever the
        # freshly shown world put the camera, and an unloaded chunk reads
        # as "no terrain", which every placement check below would then
        # refuse for the wrong reason.
        vp = viewport(args.port, fallback=(1280, 720))
        cx = int(vp.get("win_w", 1280)) // 2
        cy = int(vp.get("win_h", 720)) // 2
        pick = (f"local gx, gy = world.pickTile({cx}, {cy});"
                " return gx and {gx = gx, gy = gy} or nil")
        centre_tile = poll_until(90.0, lambda: send_json(args.port, pick))
        if not check(isinstance(centre_tile, dict),
                     "the engine picked the tile under the camera",
                     f"world.pickTile({cx}, {cy}) never resolved"):
            return 1
        centre = (int(centre_tile["gx"]), int(centre_tile["gy"]))
        print(f"        camera tile {centre}")
        anchor = find_placeable(args.port, centre[0], centre[1])
        if not check(isinstance(anchor, dict),
                     "the engine named a placeable building def and tile",
                     f"building.canPlaceAt found none near {centre}"):
            return 1
        def_name = str(anchor["name"])
        ax, ay = int(anchor["gx"]), int(anchor["gy"])
        az = send_json(args.port,
                       f"return {{z = (world.getTerrainAt({ax}, {ay}))}}")
        z = int((az or {}).get("z") or 0)
        print(f"        '{def_name}' fits at ({ax}, {ay}) z {z} on page '{page}'")
        check(pin_camera_to_tile(args.port, ax, ay, z),
              "camera pinned on the anchor (gameplay zoom, tracking off)")

        item_name = one_item_def(args.port)
        if not check(bool(item_name), "found an item def to drop"):
            return 1
        spots = flat_tiles(args.port, ax, ay, z, N_UNITS + N_ITEMS + 1)
        if not check(len(spots) >= N_UNITS + N_ITEMS,
                     f"found {N_UNITS + N_ITEMS} tiles at the anchor's own z",
                     f"got {len(spots)}"):
            return 1

        print("phase 5: baseline, then the deliberate population")
        base = wait_for_later_sequence(args.port, 0)
        if not check(bool(base), "a baseline snapshot was published"):
            return 1
        base_rows = rows_by_id(base)

        for spot in spots[:N_UNITS]:
            send(args.port, "return unit.spawn('acolyte', %d, %d)"
                            % (int(spot["gx"]), int(spot["gy"])))
        for spot in spots[N_UNITS:N_UNITS + N_ITEMS]:
            send(args.port,
                 "item.spawnGround('%s', %d.5, %d.5); return 'ok'"
                 % (item_name, int(spot["gx"]), int(spot["gy"])))
        placed = []
        for i in range(N_BUILDINGS):
            spot = find_placeable(args.port, ax + 4 * (i + 1), ay, radius=8)
            if isinstance(spot, dict):
                raw = send(args.port,
                           "return building.spawn('%s', %d, %d)"
                           % (str(spot["name"]), int(spot["gx"]),
                              int(spot["gy"])))
                try:
                    placed.append(int(float(raw)))
                except (TypeError, ValueError):
                    pass
        check(len(placed) == N_BUILDINGS,
              f"placed {N_BUILDINGS} buildings through building.spawn",
              f"placed {placed!r}")

        # The unit and building threads publish asynchronously: wait for
        # the managers to actually expose what was created before any
        # telemetry is compared against it.
        settled = poll_until(60.0, lambda: (
            lambda c: c if c.get("units", 0) >= N_UNITS
                           and c.get("buildings", 0) >= N_BUILDINGS
                           and c.get("ground", 0) >= N_ITEMS
                      else None)(counts(args.port)))
        if not check(bool(settled),
                     "the unit/building threads expose the created instances",
                     f"counts = {counts(args.port)!r}"):
            return 1

        print("phase 6: the counters moved by exactly that population")
        after = wait_for_later_sequence(args.port,
                                        int(base.get("sequence") or 0))
        if not check(bool(after), "a later snapshot was published"):
            return 1
        after_rows = rows_by_id(after)

        def delta(cat: str) -> int:
            return (int(after_rows[cat]["scanned"])
                    - int(base_rows[cat]["scanned"]))

        check(delta("units") == N_UNITS,
              f"units scanned rose by exactly {N_UNITS}",
              f"{base_rows['units']['scanned']} -> "
              f"{after_rows['units']['scanned']}")
        check(delta("buildings") == N_BUILDINGS,
              f"buildings scanned rose by exactly {N_BUILDINGS}",
              f"{base_rows['buildings']['scanned']} -> "
              f"{after_rows['buildings']['scanned']}")
        check(delta("ground_items") == N_ITEMS,
              f"ground_items scanned rose by exactly {N_ITEMS}",
              f"{base_rows['ground_items']['scanned']} -> "
              f"{after_rows['ground_items']['scanned']}")

        print("phase 7: the offscreen texture system makes them emit")
        emitting = poll_until(60.0, lambda: (
            lambda s: s if s and all(
                int(rows_by_id(s)[c]["emitted"]) > 0
                for c in ("units", "buildings", "ground_items"))
                      else None)(scene_stats(args.port)))
        emitted = rows_by_id(emitting or after)
        for cat in ("units", "buildings", "ground_items"):
            check(int(emitted[cat]["emitted"]) > 0,
                  f"{cat} emitted quads with a live texture system",
                  f"emitted = {emitted[cat]['emitted']}")

        print("phase 8: durations, sequence advance, and immutability")
        durations = [r.get("durationNs") for r in (after.get("categories") or [])]
        check(all(isinstance(d, int) and d >= 0 for d in durations),
              "every row carries a present, non-negative integer durationNs",
              f"durations = {durations!r}")

        first = scene_stats(args.port)
        later = wait_for_later_sequence(args.port,
                                        int((first or {}).get("sequence") or 0))
        check(bool(later)
              and int(later["sequence"]) > int(first["sequence"]),
              "the sequence advances across completed passes",
              f"{(first or {}).get('sequence')!r} -> "
              f"{(later or {}).get('sequence')!r}")
        check(bool(later) and len(later.get("categories") or []) == 10,
              "a later snapshot is a whole ten-row replacement")

        mutated = send_json(
            args.port,
            "local s = debug.getSceneStats(); "
            "s.sequence = -1; s.available = false; "
            "s.categories[1].id = 'clobbered'; "
            "s.categories[1].scanned = -99; "
            "local t = debug.getSceneStats(); "
            "return {id = t.categories[1].id, scanned = t.categories[1].scanned,"
            " available = t.available}",
            timeout=20.0)
        check(isinstance(mutated, dict)
              and mutated.get("id") == "tiles"
              and mutated.get("available") is True
              and int(mutated.get("scanned", -1)) >= 0,
              "a Lua caller cannot mutate the engine's stored snapshot",
              f"re-read = {mutated!r}")

        return 1 if failures else 0
    finally:
        if not args.keep_open:
            quit_engine(args.port, proc)
            code = proc.poll()
            check(code == 0, "the engine exited cleanly",
                  f"exit code {code!r}")
        print(f"\n{'FAILED' if failures else 'PASSED'} — {failures} failure(s)")


if __name__ == "__main__":
    sys.exit(main() or (1 if failures else 0))
