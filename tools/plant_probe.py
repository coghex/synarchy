#!/usr/bin/env python3
"""Planting tool + suitability probe (#335).

Boots a headless engine on a real generated world (natural ground cover
+ real climate/slope data are needed for the suitability query), then
checks:

  1. Suitability: world.getPlantSuitability(gx,gy) returns every
     registered plantable-crop species (row_crop `tomato_plant` +
     groundcover_crop `wheat`), sorted best-first, each with
     id/name/category/score/factors — and each `factors` entry has the
     6 expected labels (temperature/precipitation/humidity/altitude/
     slope/soil) with a fit in [0,1], the per-factor breakdown #335's
     planting screen shows as the "why is this good/bad here" read-out.
  2. Soil actually gates suitability: forcing the tile's surface
     material to a species-preferred soil (loam) vs a non-preferred one
     (granite) via world.setCell flips the "soil" factor's fit between
     1.0 and 0.0 and the overall score between nonzero and exactly 0.0
     — proving data/flora/crops.yaml's new `soils:` list + the
     name->id resolution in registerFloraSpecies actually take effect,
     not just parse.
  3. Designation refusal: plant.designate on an UNTILLED tile is
     refused (plant.getDesignationAt stays nil) — mirrors till's
     untillable-exclusion check.
  4. Designation refusal: an unregistered crop name is refused even on
     a tilled tile.
  5. Designation success: plant.designate on a TILLED tile for a
     registered crop (both row_crop and groundcover_crop names accepted
     symmetrically — designating doesn't execute planting, so it isn't
     gated on which primitive #336 will later use to place it) records
     {x,y,z,crop}; plant.cancelDesignation clears it;
     plant.getDesignationCount / nearestDesignation agree.
  6. Replace semantics: designating a second crop on an
     already-designated tile overwrites the first (HashMap insert, not
     a re-sweep skip like till's rectangle sweep — there's no
     "idempotent" concept for a single explicit designate call).
  7. Save/load: a designation (with its crop) survives
     save -> loadSave (WorldPageSave wpsPlantDesignations).

The farm AI that actually claims/walks/plants a designation is #336,
not this issue — this probe stops at "designation recorded and
queryable", the same scope boundary till_probe.py draws around the
till AI it doesn't test (that AI already existed; here the AI doesn't
exist yet).

Usage: python3 tools/plant_probe.py [--port 9179] [--seed 42]
       [--size 64] [--plates 3]
"""
import argparse, glob, json, socket, subprocess, sys, time
from probelib import boot, send

SPROOT = "/tmp"

# data/materials/soils_mineral.yaml `loam` (in both crops.yaml `soils:`
# lists) and data/materials/igneous_intrusive.yaml `granite` (in
# neither) — used to prove soil actually gates suitability, not just
# parses. world.setCell resolves either a material NAME or numeric id
# (World.Material.materialIdByName) — names are used here so this
# probe doesn't need to track raw ids.
LOAM_NAME = "loam"
GRANITE_NAME = "granite"

FACTOR_NAMES = {"temperature", "precipitation", "humidity", "altitude",
                "slope", "soil"}


def jget(port, lua, timeout=10.0):
    raw = send(port, lua, timeout)
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return raw.strip('"')


def bootstrap(port):
    for pattern, fn in [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/infections/*.yaml", "engine.loadInfectionYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/flora/*.yaml",      "engine.loadFloraYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
    ]:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")


def find_tillable(port, span=4):
    """Scan sample points across the loaded region for a flat, dry,
    flora-free tile; returns (gx, gy) or None."""
    for sx in range(-span * 16, span * 16 + 1, 4):
        for sy in range(-span * 16, span * 16 + 1, 4):
            slope = jget(port, f"return world.getSlopeAt({sx},{sy})")
            if slope != 0:
                continue
            fluid = jget(port, f"return world.getFluidAt({sx},{sy})")
            if isinstance(fluid, dict) and fluid.get("type"):
                continue
            flora = jget(port, f"return world.getFloraAt({sx},{sy})")
            if isinstance(flora, dict):
                continue
            return sx, sy
    return None


def till_and_wait(port, page, gx, gy, z):
    """world.setVegAt is a queued world command, like world.setDate —
    send, then poll isPlantable until it lands before designating."""
    send(port, f"world.setVegAt('{page}', {gx}, {gy}, {z}, 77); return 'ok'")
    for _ in range(20):
        if jget(port, f"return world.isPlantable({gx},{gy})") is True:
            return True
        time.sleep(0.2)
    sys.exit(f"setVegAt({gx},{gy}) never landed")


def suitability_row(port, page, gx, gy, species):
    """world.getPlantSuitability(gx,gy) → the row for `species`, or None."""
    rows = jget(port, f"return world.getPlantSuitability({gx},{gy})")
    if not isinstance(rows, list):
        return None
    return next((r for r in rows if r.get("name") == species), None)


def soil_fit(port, page, gx, gy, species):
    row = suitability_row(port, page, gx, gy, species)
    if not row:
        return None
    f = next((f for f in row["factors"] if f["factor"] == "soil"), None)
    return f["fit"] if f else None


def set_material_and_wait(port, page, gx, gy, z, mat_name, species, expect_fit):
    """world.setCell is a queued world command — send, then poll
    getPlantSuitability's "soil" factor for `species` until it actually
    REACHES expect_fit (no world.getMaterialAt primitive exists to poll
    the raw cell directly, and a bare presence check would return
    immediately on the stale pre-edit value)."""
    send(port, f"world.setCell('{page}', {gx}, {gy}, {z}, '{mat_name}'); "
               f"return 'ok'")
    for _ in range(30):
        if soil_fit(port, page, gx, gy, species) == expect_fit:
            return True
        time.sleep(0.2)
    return False


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9179)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    args = ap.parse_args()
    port = args.port
    passed = True

    proc = boot(port, f"{SPROOT}/plant_probe_engine.log")
    try:
        bootstrap(port)
        send(port, f"world.init('probe', {args.seed}, {args.size}, "
                   f"{args.plates}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, "world.show('probe'); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)

        found = find_tillable(port)
        if not found:
            print("  [FAIL] no flat, dry, flora-free tile in the loaded "
                  "region (try another seed)")
            return 1
        tx, ty = found
        # world.getSurfaceAt returns MULTIPLE Lua values (surfaceZ,
        # terrainZ, fluidType, fluidSurface), not a table — capture just
        # the first via a local (same quirk crop_probe.py's
        # find_dry_tile documents).
        z = jget(port, f"local sz=world.getSurfaceAt({tx},{ty}); return sz")
        print(f"  candidate tile at ({tx},{ty}), surfaceZ={z}")

        # Force a known, species-preferred soil BEFORE any suitability
        # assertions, so they're deterministic regardless of whatever
        # material this seed's worldgen happened to put here (loam is
        # in both tomato_plant's and wheat's crops.yaml `soils:` list).
        okL = set_material_and_wait(port, "probe", tx, ty, z,
                                     LOAM_NAME, "wheat", 1.0)
        passed &= okL
        print(f"  [{'PASS' if okL else 'FAIL'}] forcing loam soil: soil "
              f"fit={soil_fit(port, 'probe', tx, ty, 'wheat')}")

        # --- 1. Suitability query lists every registered crop, with a
        #     6-factor breakdown per crop ---
        rows = jget(port, f"return world.getPlantSuitability({tx},{ty})")
        by_name = {r["name"]: r for r in rows} if isinstance(rows, list) else {}
        ok1 = ("tomato_plant" in by_name and "wheat" in by_name
               and by_name["tomato_plant"]["category"] == "row_crop"
               and by_name["wheat"]["category"] == "groundcover_crop"
               and all(0.0 <= r["score"] <= 1.0 for r in rows)
               and rows == sorted(rows, key=lambda r: -r["score"]))
        passed &= ok1
        print(f"  [{'PASS' if ok1 else 'FAIL'}] getPlantSuitability lists "
              f"both shipped crops, sorted best-first: {rows}")

        ok1b = all(
            isinstance(r.get("factors"), list)
            and {f["factor"] for f in r["factors"]} == FACTOR_NAMES
            and all(0.0 <= f["fit"] <= 1.0 for f in r["factors"])
            for r in rows
        ) if isinstance(rows, list) else False
        passed &= ok1b
        print(f"  [{'PASS' if ok1b else 'FAIL'}] each row's factors cover "
              f"all 6 labels with fit in [0,1]")

        # --- 2. Soil actually gates suitability (not just parses) ---
        okG = set_material_and_wait(port, "probe", tx, ty, z,
                                     GRANITE_NAME, "wheat", 0.0)
        row_bad_wheat = suitability_row(port, "probe", tx, ty, "wheat")
        row_bad_tomato = suitability_row(port, "probe", tx, ty, "tomato_plant")
        ok2 = (okG
               and row_bad_wheat is not None and row_bad_wheat["score"] == 0.0
               and row_bad_tomato is not None and row_bad_tomato["score"] == 0.0)
        passed &= ok2
        print(f"  [{'PASS' if ok2 else 'FAIL'}] granite (non-preferred soil) "
              f"zeroes both crops' scores: wheat={row_bad_wheat}, "
              f"tomato={row_bad_tomato}")

        # Restore loam so the rest of this probe (designation checks
        # below) runs against a species-preferred soil, matching the
        # deterministic setup at the top.
        set_material_and_wait(port, "probe", tx, ty, z, LOAM_NAME, "wheat", 1.0)

        # --- 3. Designation refused on an untilled tile ---
        pre = jget(port, f"return world.isPlantable({tx},{ty})")
        ok2a = pre is False
        passed &= ok2a
        print(f"  [{'PASS' if ok2a else 'FAIL'}] isPlantable is false before "
              f"tilling: {pre}")

        send(port, f"plant.designate('probe',{tx},{ty},'wheat'); "
                   f"return 'ok'")
        time.sleep(0.5)
        d0 = jget(port, f"return plant.getDesignationAt('probe',{tx},{ty})")
        ok2 = not isinstance(d0, dict)
        passed &= ok2
        print(f"  [{'PASS' if ok2 else 'FAIL'}] designate refused on an "
              f"untilled tile: {d0}")

        till_and_wait(port, "probe", tx, ty, z)

        # --- 4. Designation refused for an unregistered crop name ---
        send(port, f"plant.designate('probe',{tx},{ty},'not_a_real_crop'); "
                   f"return 'ok'")
        time.sleep(0.5)
        d1 = jget(port, f"return plant.getDesignationAt('probe',{tx},{ty})")
        ok3 = not isinstance(d1, dict)
        passed &= ok3
        print(f"  [{'PASS' if ok3 else 'FAIL'}] designate refused for an "
              f"unregistered crop name: {d1}")

        # --- 5. Designation succeeds on a tilled tile ---
        send(port, f"plant.designate('probe',{tx},{ty},'wheat'); "
                   f"return 'ok'")
        time.sleep(0.5)
        n = jget(port, "return plant.getDesignationCount('probe')")
        d2 = jget(port, f"return plant.getDesignationAt('probe',{tx},{ty})")
        ok4 = (isinstance(n, (int, float)) and n >= 1
               and isinstance(d2, dict) and d2.get("crop") == "wheat"
               and isinstance(d2.get("z"), (int, float)))
        passed &= ok4
        print(f"  [{'PASS' if ok4 else 'FAIL'}] designate marks a tilled "
              f"tile: count={n} at-tile={d2}")

        # plant.nearestDesignation returns MULTIPLE Lua values (gx, gy,
        # dist), not a table — the debug console prints them
        # tab-separated on one line, so parse that directly instead of
        # jget's JSON path (same multi-return quirk world.getSurfaceAt
        # has, per crop_probe.py's find_dry_tile).
        near_raw = send(port,
            f"return plant.nearestDesignation('probe',{tx},{ty})")
        near_parts = near_raw.split()
        ok4b = (len(near_parts) >= 2
                and int(float(near_parts[0])) == tx
                and int(float(near_parts[1])) == ty)
        passed &= ok4b
        print(f"  [{'PASS' if ok4b else 'FAIL'}] nearestDesignation finds "
              f"it: {near_raw!r}")

        send(port, f"plant.cancelDesignation({tx},{ty}); return 'ok'")
        time.sleep(0.5)
        d3 = jget(port, f"return plant.getDesignationAt('probe',{tx},{ty})")
        ok4c = not isinstance(d3, dict)
        passed &= ok4c
        print(f"  [{'PASS' if ok4c else 'FAIL'}] cancelDesignation clears "
              f"it: {d3}")

        # --- 6. Designating a row_crop works too (designation is
        #     category-symmetric; only execution is #336's asymmetry) ---
        send(port, f"plant.designate('probe',{tx},{ty},'tomato_plant'); "
                   f"return 'ok'")
        time.sleep(0.5)
        d4 = jget(port, f"return plant.getDesignationAt('probe',{tx},{ty})")
        ok5 = isinstance(d4, dict) and d4.get("crop") == "tomato_plant"
        passed &= ok5
        print(f"  [{'PASS' if ok5 else 'FAIL'}] designate accepts a "
              f"row_crop name too: {d4}")

        # --- 7. Replace semantics: designating again overwrites ---
        send(port, f"plant.designate('probe',{tx},{ty},'wheat'); "
                   f"return 'ok'")
        time.sleep(0.5)
        d5 = jget(port, f"return plant.getDesignationAt('probe',{tx},{ty})")
        ok6 = isinstance(d5, dict) and d5.get("crop") == "wheat"
        passed &= ok6
        print(f"  [{'PASS' if ok6 else 'FAIL'}] re-designating the same "
              f"tile replaces the crop: {d5}")

        # --- 8. Save/load round-trip ---
        send(port, "engine.saveWorld('probe', 'plant_v78_check'); "
                   "return 'ok'")
        time.sleep(3.0)
        send(port, "engine.loadSave('plant_v78_check'); return 'ok'")
        time.sleep(15.0)
        send(port, "world.show('main_world'); return 'ok'")
        send(port, "engine.setPaused(false); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)
        d6 = jget(port,
                  f"return plant.getDesignationAt('main_world',{tx},{ty})")
        ok7 = isinstance(d6, dict) and d6.get("crop") == "wheat"
        passed &= ok7
        print(f"  [{'PASS' if ok7 else 'FAIL'}] designation (with crop) "
              f"survives save/load: {d6}")

        print("\n" + ("ALL PLANT CHECKS PASSED" if passed else "SOME FAILED"))
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
