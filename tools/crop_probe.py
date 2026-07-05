#!/usr/bin/env python3
"""Crop content + two growth forms probe (#334).

Boots a headless engine and checks BOTH growth forms the farming epic
(#331) calls for, sharing the #332 growth runtime like wild flora:

  1. Row crop: an ordinary FloraInstance laid out at intervals within
     the tile (World.Flora.Placement's "row_crop" category — see
     rowOffset) via NATURAL worldgen placement, exactly like wild
     flora. The real shipped `tomato_plant` ships with worldGen
     density 0.0 (no wild spawn; #335/#336 own player-driven planting),
     so it can never place in an ordinary generated world — to exercise
     it headless anyway, this probe loads a SECOND copy of its exact
     YAML entry (parsed straight out of data/flora/crops.yaml, not
     hand-duplicated) with only the worldGen tolerances relaxed for
     guaranteed placement, mirroring flora_growth_probe.py's
     probe_berry pattern but deriving it from the real content so the
     phases/annualCycle/harvestable data under test is byte-identical
     to what ships.
  2. Groundcover crop: NOT a FloraInstance at all — planted via the new
     world.plantCropAt primitive into a World.Flora.CropPlot, and
     rendered as the tile's veg-fill rather than a floating sprite.
     Tests the REAL shipped `wheat` species + `wheat_grain` item
     directly (planting doesn't depend on worldGen density). Also
     checks world.plantCropAt REFUSES a row_crop species (tomato_plant)
     — only a groundcover_crop can become a CropPlot.

Checks per form: growth is derived and visibly advances (age/phase),
a harvest yields the species' item, and (groundcover only) the planted
state survives save -> load.

Usage: python3 tools/crop_probe.py [--port 9195] [--seed 42]
       [--size 64] [--plates 3]
"""
import argparse, copy, glob, json, socket, subprocess, sys, time
import yaml

SPROOT = "/tmp"


def send(port, lua, timeout=10.0):
    with socket.create_connection(("localhost", port), timeout=timeout) as s:
        s.settimeout(timeout)
        f = s.makefile("rw")
        f.readline()              # banner
        f.write(lua + "\n")
        f.flush()
        return f.readline().strip().lstrip("> ").strip()


def jget(port, lua, timeout=10.0):
    raw = send(port, lua, timeout)
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return raw.strip('"')


def boot(port, log_path):
    log = open(log_path, "w")
    proc = subprocess.Popen(
        ["cabal", "run", "-v0", "exe:synarchy", "--",
         "--headless", "--port", str(port)],
        stdout=log, stderr=subprocess.STDOUT)
    for _ in range(300):
        time.sleep(0.2)
        try:
            if "READY" in open(log_path).read():
                return proc
        except FileNotFoundError:
            pass
    sys.exit("engine never printed READY")


# Throwaway row-crop fixture (mirrors flora_growth_probe.py's
# Max-tolerance worldGen override for the placement-tolerant tomato_plant
# double below — same numbers flora_growth_probe.py's probe_berry uses,
# just relaxed so it places on any seed's geography regardless of the
# shipped species' real (narrower) climate gate.
RELAXED_WORLDGEN = {
    "minTemp": -60, "maxTemp": 60, "idealTemp": 15,
    "minPrecip": 0.0, "maxPrecip": 5.0, "idealPrecip": 0.8,
    "minAlt": -100, "maxAlt": 3000, "idealAlt": 50,
    "minHumidity": 0.0, "maxHumidity": 1.0, "idealHumidity": 0.5,
    "maxSlope": 7, "density": 1.0,
}


def bootstrap(port):
    checked = {}
    for pattern, fn in [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/flora/*.yaml",      "engine.loadFloraYaml"),
    ]:
        for path in sorted(glob.glob(pattern)):
            r = jget(port, f"return {fn}('{path}')")
            if path.endswith("crops.yaml") or path.endswith(
                    ("tomato.yaml", "wheat_grain.yaml")):
                checked[path] = r

    # tomato_plant ships with worldGen density 0.0 (#334: farmed crops
    # shouldn't wild-spawn), so it can never place in an ordinary
    # generated world. To exercise the REAL shipped row-crop content
    # (not a hand-written proxy), parse its exact entry back out of
    # data/flora/crops.yaml and reload it under the SAME name with only
    # worldGen relaxed — phases/annualCycle/cycleOverrides/harvestable
    # (and so the "tomato" yield item) are byte-identical to what ships.
    # Appended after the real flora so THEIR placement hashes (indexed
    # by registration order) are untouched; the two same-named
    # registrations don't collide (World.Flora.Placement keys placement
    # by FloraId, not name — see Engine.Scripting.Lua.API.Forage's
    # findSpeciesByName docstring for the one place name matters, which
    # this doesn't affect since both copies share the same category).
    with open("data/flora/crops.yaml") as f:
        crops = yaml.safe_load(f)
    tomato = copy.deepcopy(
        next(e for e in crops["flora"] if e["name"] == "tomato_plant"))
    tomato["worldGen"] = {**tomato["worldGen"], **RELAXED_WORLDGEN}
    path = f"{SPROOT}/probe_tomato_plant.yaml"
    with open(path, "w") as f:
        yaml.safe_dump({"flora": [tomato]}, f)
    send(port, f"engine.loadFloraYaml('{path}'); return 'ok'")
    return checked


def set_date(port, page, y, mo, d):
    """setDate is a queued world command — send, then wait until
    getDate reflects it."""
    send(port, f"world.setDate('{page}', {y}, {mo}, {d}); return 'ok'")
    for _ in range(20):
        time.sleep(0.2)
        got = jget(port, f"return world.getDate('{page}')")
        if isinstance(got, dict) and got.get("year") == y \
           and got.get("month") == mo and got.get("day") == d:
            return got
    sys.exit(f"setDate({y},{mo},{d}) never landed")


def find_species_tile(port, species, harvestable=None, lo=-64, hi=64):
    """Scan the loaded region for the first tile carrying an instance of
    the given species (optionally requiring its harvestable flag).
    Returns (gx, gy) or None."""
    cond = f"e.id=='{species}'"
    if harvestable is not None:
        cond += f" and e.harvestable=={'true' if harvestable else 'false'}"
    r = send(
        port,
        f"for gx={lo},{hi} do for gy={lo},{hi} do "
        f"local t=world.getFloraGrowthAt(gx,gy); "
        f"if t then for _,e in ipairs(t) do "
        f"if {cond} then return gx..','..gy end end end "
        f"end end return 'none'",
        timeout=60.0)
    r = r.strip('"')
    if r == "none":
        return None
    gx, gy = r.split(",")
    return int(gx), int(gy)


def find_dry_tile(port, cx, cy, radius=12):
    """Nearest tile to (cx, cy) with a real surface and no fluid on it —
    world.getSurfaceAt returns MULTIPLE Lua values (surfaceZ, terrainZ,
    fluidType, fluidSurface), not a table, so wrap it into one value
    per call. Returns (gx, gy, surfaceZ) or None."""
    r = send(
        port,
        f"for r=0,{radius} do for dx=-r,r do for dy=-r,r do "
        f"local gx,gy={cx}+dx,{cy}+dy; "
        f"local sz,tz,ft=world.getSurfaceAt(gx,gy); "
        f"if sz and not ft then return gx..','..gy..','..sz end "
        f"end end end return 'none'",
        timeout=30.0)
    r = r.strip('"')
    if r == "none":
        return None
    gx, gy, sz = r.split(",")
    return int(gx), int(gy), int(sz)


def till_and_wait(port, page, gx, gy, z):
    """world.setVegAt is a queued world command, like world.setDate —
    send, then poll isPlantable until it lands before planting."""
    send(port, f"world.setVegAt('{page}', {gx}, {gy}, {z}, 77); return 'ok'")
    for _ in range(20):
        if jget(port, f"return world.isPlantable({gx},{gy})") is True:
            return True
        time.sleep(0.2)
    sys.exit(f"setVegAt({gx},{gy}) never landed")


def growth_entries(port, gx, gy, species):
    t = jget(port, f"return world.getFloraGrowthAt({gx},{gy})")
    if isinstance(t, list):
        return [e for e in t if e.get("id") == species]
    return []


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9195)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    args = ap.parse_args()
    port = args.port
    passed = True

    proc = boot(port, f"{SPROOT}/crop_probe_engine.log")
    try:
        checked = bootstrap(port)
        ok0 = all(v not in (0, None, "") for v in checked.values()) \
            and len(checked) >= 3
        passed &= ok0
        print(f"  [{'PASS' if ok0 else 'FAIL'}] shipped crop content loads "
              f"cleanly: {checked}")

        send(port, f"world.init('probe', {args.seed}, {args.size}, {args.plates}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, "world.show('probe'); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)

        # ============== 1. Row crop (natural placement) ==============
        set_date(port, "probe", 2, 1, 5)  # dormant/budding season baseline
        tile = find_species_tile(port, "tomato_plant")
        if not tile:
            print("  [FAIL] tomato_plant not found in region — try another seed")
            return 1

        es = growth_entries(port, *tile, "tomato_plant")
        ok1a = len(es) == 3
        passed &= ok1a
        print(f"  [{'PASS' if ok1a else 'FAIL'}] row-crop category places "
              f"exactly 3 instances per tile (rowOffset): found {len(es)}")

        ok1b = all(0.0 <= e["health"] <= 1.0 and e["age"] >= 0.0 for e in es)
        passed &= ok1b
        print(f"  [{'PASS' if ok1b else 'FAIL'}] row-crop instances report "
              f"derived growth state: {es}")

        # Season window (tomato_plant's real annualCycle: dormant@0 /
        # budding@30 / flowering@60 / fruiting@90 / senescing@240):
        # dormant/budding now, fruiting once the date moves into its window.
        ok1c = all(e.get("stage") in ("dormant", "budding")
                   and not e.get("harvestable") for e in es)
        passed &= ok1c
        print(f"  [{'PASS' if ok1c else 'FAIL'}] row crop not harvestable "
              f"before its fruiting window: {es}")
        set_date(port, "probe", 2, 7, 21)  # day-of-year ~202, in [90,240)
        es2 = growth_entries(port, *tile, "tomato_plant")
        ok1d = any(e.get("stage") == "fruiting" and e.get("harvestable")
                   for e in es2)
        passed &= ok1d
        print(f"  [{'PASS' if ok1d else 'FAIL'}] row crop harvestable in its "
              f"fruiting window: {es2}")

        y1 = jget(port, f"return world.harvestFlora({tile[0]},{tile[1]})")
        ok1e = isinstance(y1, list) and len(y1) >= 1 \
            and all(it.get("id") == "tomato" for it in y1)
        passed &= ok1e
        print(f"  [{'PASS' if ok1e else 'FAIL'}] row-crop harvest yields "
              f"tomato: {y1}")

        # ======= 2. Groundcover crop (planted via world.plantCropAt) =======
        found = find_dry_tile(port, tile[0] + 3, tile[1] + 3)
        if not found:
            print("  [FAIL] no dry tile found near the row-crop site")
            return 1
        gx0, gy0, z0 = found

        pre = jget(port, f"return world.isPlantable({gx0},{gy0})")
        ok2a = pre is False
        passed &= ok2a
        print(f"  [{'PASS' if ok2a else 'FAIL'}] untilled tile refuses "
              f"plantCropAt's gate before tilling: isPlantable={pre}")
        refused = jget(port,
            f"return world.plantCropAt({gx0},{gy0},'wheat')")
        ok2b = refused in (None, False)
        passed &= ok2b
        print(f"  [{'PASS' if ok2b else 'FAIL'}] plantCropAt refuses on "
              f"untilled soil: {refused}")

        ok2c = till_and_wait(port, "probe", gx0, gy0, z0)
        passed &= ok2c
        print(f"  [{'PASS' if ok2c else 'FAIL'}] tile plantable after tilling "
              f"(vegTilledSoil): {ok2c}")

        # plantCropAt is a CropPlot-only primitive: a row_crop species
        # (tomato_plant is an ordinary FloraInstance, not a CropPlot) must
        # be refused even on tilled, otherwise-plantable soil.
        row_refused = jget(port,
            f"return world.plantCropAt({gx0},{gy0},'tomato_plant')")
        ok2r = row_refused in (None, False)
        passed &= ok2r
        print(f"  [{'PASS' if ok2r else 'FAIL'}] plantCropAt refuses a "
              f"row_crop species (tomato_plant) on tilled soil: {row_refused}")
        cleared_row = jget(port, f"return world.getCropPlotAt({gx0},{gy0})")
        ok2s = cleared_row is None
        passed &= ok2s
        print(f"  [{'PASS' if ok2s else 'FAIL'}] the refused tomato_plant "
              f"plant left no crop plot behind: {cleared_row}")

        planted = jget(port, f"return world.plantCropAt({gx0},{gy0},'wheat')")
        ok2d = planted is True
        passed &= ok2d
        print(f"  [{'PASS' if ok2d else 'FAIL'}] plantCropAt plants the real "
              f"'wheat' species: {planted}")

        p0 = jget(port, f"return world.getCropPlotAt({gx0},{gy0})")
        ok2e = isinstance(p0, dict) and p0.get("id") == "wheat" \
            and p0.get("phase") == "sprout" and p0.get("age") < 5.0
        passed &= ok2e
        print(f"  [{'PASS' if ok2e else 'FAIL'}] freshly planted crop plot "
              f"starts at age ~0, sprout phase: {p0}")

        # Advance the REAL game clock (not a calendar jump — CropPlot age
        # is measured relative to its OWN planted day, see
        # World.Flora.CropPlot) far enough to clear the 30-day vegetating
        # threshold: ~50000 game-min/real-sec for 4 real-sec ~= 139 game-days.
        send(port, "world.setTimeScale('probe', 50000); return 'ok'")
        time.sleep(4.0)
        send(port, "world.setTimeScale('probe', 1); return 'ok'")
        p1 = jget(port, f"return world.getCropPlotAt({gx0},{gy0})")
        ok2f = isinstance(p1, dict) and p1.get("age", 0) > p0.get("age", 0) \
            and p1.get("phase") != "sprout" and p1.get("harvestable") is True
        passed &= ok2f
        print(f"  [{'PASS' if ok2f else 'FAIL'}] groundcover crop visibly "
              f"advances under the game clock and becomes harvestable: "
              f"{p0} -> {p1}")

        y2 = jget(port, f"return world.harvestFlora({gx0},{gy0})")
        ok2g = isinstance(y2, list) and len(y2) >= 1 \
            and all(it.get("id") == "wheat_grain" for it in y2)
        passed &= ok2g
        print(f"  [{'PASS' if ok2g else 'FAIL'}] groundcover-crop harvest "
              f"yields wheat_grain: {y2}")

        cleared = jget(port, f"return world.getCropPlotAt({gx0},{gy0})")
        ok2h = cleared is None
        passed &= ok2h
        print(f"  [{'PASS' if ok2h else 'FAIL'}] harvest clears the plot "
              f"(annual, one-shot): {cleared}")

        # ============== 3. Groundcover plot survives save/load ==============
        found2 = find_dry_tile(port, gx0 + 2, gy0 + 2)
        if not found2:
            print("  [FAIL] no dry tile found for the save/load plot")
            return 1
        gx1, gy1, z1 = found2
        till_and_wait(port, "probe", gx1, gy1, z1)
        jget(port, f"return world.plantCropAt({gx1},{gy1},'wheat')")
        before = jget(port, f"return world.getCropPlotAt({gx1},{gy1})")

        send(port, "engine.saveWorld('probe', 'crop_plot_check'); return 'ok'")
        time.sleep(3.0)
        send(port, "engine.loadSave('crop_plot_check'); return 'ok'")
        time.sleep(15.0)
        send(port, "world.show('main_world'); return 'ok'")
        after = jget(port, f"return world.getCropPlotAt({gx1},{gy1})")
        ok3 = isinstance(before, dict) and isinstance(after, dict) \
            and before.get("id") == after.get("id") == "wheat" \
            and after.get("age", -1) >= 0.0
        passed &= ok3
        print(f"  [{'PASS' if ok3 else 'FAIL'}] planted crop plot survives "
              f"save/load: {before} -> {after}")

        print("\n" + ("ALL CROP CHECKS PASSED" if passed else "SOME FAILED"))
        return 0 if passed else 1
    finally:
        try:
            send(port, "engine.quit()")
        except Exception:
            pass
        time.sleep(1.0)
        proc.kill()


if __name__ == "__main__":
    sys.exit(main())
