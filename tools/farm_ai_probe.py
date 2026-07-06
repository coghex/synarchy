#!/usr/bin/env python3
"""Farm AI probe (#336): plant + skill-gated auto-harvest + rot.

Boots a headless engine on a real generated world and drives the
capstone of the farming epic (#331) — till → plant → grow → auto-harvest
— end to end through the real acolyte AI stack, plus the two new
primitives this issue adds:

  - world.plantRowCropAt: runtime placement of a row-crop FloraInstance
    (the WePlaceFlora edit path, save v79) — #334 only ever placed row
    crops at worldgen time; #336 needs to place one when a unit finishes
    planting a row-crop designation.
  - world.findHarvestableFlora's new CropPlot scan — so auto-harvest can
    discover a ripe PLANTED groundcover crop, not just wild FloraInstances.

Checks:

  1. plantRowCropAt refused on untilled soil (no instance appears).
  2. plantRowCropAt refused for a groundcover_crop name (wheat) — mirrors
     plantCropAt's reciprocal refusal of a row_crop name.
  3. plantRowCropAt places a real row-crop instance (tomato_plant) on
     tilled soil, at full health, age ~0. A tile already carrying an
     instance then refuses a second planting (no duplicate/overlapping
     instances stacking from a re-plant), and the reciprocal cross-form
     guard holds too: plantCropAt (groundcover) refuses a tile already
     carrying a row-crop instance.
  4. Rot: a freshly-planted row crop (its own tile) becomes harvestable
     in its fruiting window and NOT harvestable once the calendar rolls
     into senescing without being picked — the #332 mechanic, exercised
     through THIS issue's own planting primitive. The rotten-but-still-
     standing plant then makes plant.designate itself refuse that tile
     (the designation path is the occupancy gate, not just the two
     planting primitives — a farm AI should never walk a full
     claim-and-work cycle toward a designation that was always going to
     fail the primitive's own occupancy guard).
  5. plant.getDesignationAt's new "category" field reports row_crop /
     groundcover_crop correctly (the farm AI's dispatch key).
  6. Full loop, tile A (groundcover): till.designate → AI tills
     autonomously (farming-skill-scaled, #265) → plant.designate(wheat)
     → AI plants autonomously (world.plantCropAt) → getCropPlotAt shows
     it planted, at full health.
  7. Full loop, tile B (row crop): pre-tilled, plant.designate
     (tomato_plant) → AI plants autonomously (world.plantRowCropAt) →
     getFloraGrowthAt shows the new instance.
  8. Farming skill (#265) grows from these actions.
  9. Auto-harvest: fast-forward tile A's wheat past its 30-day
     vegetating threshold, then the same (now idle) acolyte
     autonomously finds and harvests it — NOT hunger-gated, reuses
     #94's yield path — clearing the plot and dropping wheat_grain on
     the ground, growing farming XP further.
 10. Re-designating a tile the AI has already claimed with a DIFFERENT
     crop makes it plant the NEW crop, not the stale one it originally
     walked over for (plant.designate replaces in place, HM.insert).
 11. Save/load: the row-crop instance (WePlaceFlora, save v79) survives
     save → loadSave.

Usage: python3 tools/farm_ai_probe.py [--port 9336] [--seed 42]
       [--size 64] [--plates 3]
"""
import argparse, glob, json, socket, subprocess, sys, time

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


def find_tillable(port, cx=0, cy=0, span=4, exclude=None):
    """Scan sample points around (cx,cy) for a flat, dry, flora-free
    tile not already in `exclude`; returns (gx, gy) or None."""
    exclude = exclude or set()
    for sx in range(cx - span * 16, cx + span * 16 + 1, 4):
        for sy in range(cy - span * 16, cy + span * 16 + 1, 4):
            if (sx, sy) in exclude:
                continue
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
    """world.setVegAt is a queued world command — send, then poll
    isPlantable until it lands before designating/planting."""
    send(port, f"world.setVegAt('{page}', {gx}, {gy}, {z}, 77); return 'ok'")
    for _ in range(20):
        if jget(port, f"return world.isPlantable({gx},{gy})") is True:
            return True
        time.sleep(0.2)
    sys.exit(f"setVegAt({gx},{gy}) never landed")


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


def growth_entries(port, gx, gy, species):
    t = jget(port, f"return world.getFloraGrowthAt({gx},{gy})")
    if isinstance(t, list):
        return [e for e in t if e.get("id") == species]
    return []


def spawn_worker(port, x, y):
    """Spawn an acolyte and quiet its find_water goal (the water-search
    spiral outranks menial work and can walk a fresh spawn off cliffs) —
    the till_probe.py / role_probe.py convention."""
    uid_s = send(port, f"local u=unit.spawn('acolyte',{x},{y}); return u")
    try:
        uid = int(float(uid_s.strip('"')))
    except ValueError:
        return -1
    time.sleep(2.0)
    quiet = send(port,
                 f"local ai=require('scripts.unit_ai'); "
                 f"local s=ai.getState({uid}); "
                 f"if s then ai.markGoalAccomplished(s,'find_water'); "
                 f"unit.stop({uid}); return 'ok' "
                 f"else return 'nostate' end").strip('"')
    if quiet != "ok":
        return -1
    return uid


def clear_wild_forage(port, cx, cy, radius=30, keep=None):
    """Harvest away any currently-ripe wild flora within `radius` of
    (cx,cy) so the auto-harvest AI check below can't get distracted by
    incidental wild forage racing it to a nearer bush instead of the
    planted crop plot under test. `keep` (gx,gy) is the crop plot under
    test itself — findHarvestableFlora covers crop plots too now, so
    without this the direct harvestFlora call here would harvest the
    plot itself before the unit ever gets a turn."""
    cleared = []
    for _ in range(50):
        spot = jget(port, f"return world.findHarvestableFlora({cx},{cy},{radius})")
        if not isinstance(spot, dict):
            return cleared
        if keep and (spot.get("gx"), spot.get("gy")) == tuple(keep):
            return cleared
        jget(port, f"return world.harvestFlora({spot['gx']},{spot['gy']})")
        cleared.append((spot.get("gx"), spot.get("gy"), spot.get("id")))
    return cleared


def get_skill(port, uid, name):
    v = jget(port, f"return unit.getSkill({uid},'{name}') or -1")
    return float(v) if isinstance(v, (int, float)) else -1.0


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9336)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    args = ap.parse_args()
    port = args.port
    passed = True

    proc = boot(port, f"{SPROOT}/farm_ai_probe_engine.log")
    try:
        bootstrap(port)
        send(port, f"world.init('probe', {args.seed}, {args.size}, "
                   f"{args.plates}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, "world.show('probe'); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)

        # Four distinct tiles: A (groundcover, full till+plant+harvest AI
        # loop) and B (row crop, AI-planted) near the origin; C and D
        # (direct-primitive checks: refusal + a single real plant, and
        # the rot timeline, respectively — a planted tile now refuses a
        # second planting, so the rot check needs its OWN fresh tile
        # rather than replanting C) far enough away (>24 tiles, past
        # harvest_scan_range) that the acolyte working A/B can never
        # stumble onto them and confound the "ignored crop rots" check.
        used = set()
        tA = find_tillable(port)
        if not tA:
            print("  [FAIL] no tillable tile found for site A (try another seed)")
            return 1
        used.add(tA)
        tB = find_tillable(port, exclude=used)
        if not tB:
            print("  [FAIL] no tillable tile found for site B (try another seed)")
            return 1
        used.add(tB)
        tC = find_tillable(port, cx=-60, cy=-60, exclude=used)
        if not tC:
            print("  [FAIL] no tillable tile found for site C (try another seed)")
            return 1
        used.add(tC)
        tD = find_tillable(port, cx=-60, cy=-60, exclude=used)
        if not tD:
            print("  [FAIL] no tillable tile found for site D (try another seed)")
            return 1
        used.add(tD)
        tF = find_tillable(port, cx=-60, cy=-60, exclude=used)
        if not tF:
            print("  [FAIL] no tillable tile found for site F (try another seed)")
            return 1
        ax, ay = tA
        bx, by = tB
        cx, cy = tC
        dx, dy = tD
        fx, fy = tF
        print(f"  site A={tA} (groundcover) B={tB} (row, AI) C={tC} (row, direct) "
              f"D={tD} (row, rot) F={tF} (category field)")

        # --- 1/2/3. Direct-primitive plantRowCropAt checks (tile C) ---
        refused0 = growth_entries(port, cx, cy, "tomato_plant")
        send(port, f"world.plantRowCropAt('probe',{cx},{cy},'tomato_plant'); "
                   f"return 'ok'")
        time.sleep(0.5)
        after_refused = growth_entries(port, cx, cy, "tomato_plant")
        ok1 = not refused0 and not after_refused
        passed &= ok1
        print(f"  [{'PASS' if ok1 else 'FAIL'}] plantRowCropAt refused on "
              f"untilled soil: {after_refused}")

        cz = jget(port, f"local sz=world.getSurfaceAt({cx},{cy}); return sz")
        till_and_wait(port, "probe", cx, cy, cz)

        send(port, f"world.plantRowCropAt('probe',{cx},{cy},'wheat'); "
                   f"return 'ok'")
        time.sleep(0.5)
        wheat_refused = growth_entries(port, cx, cy, "wheat")
        ok2 = not wheat_refused
        passed &= ok2
        print(f"  [{'PASS' if ok2 else 'FAIL'}] plantRowCropAt refuses a "
              f"groundcover_crop name (wheat): {wheat_refused}")

        send(port, f"world.plantRowCropAt('probe',{cx},{cy},'tomato_plant'); "
                   f"return 'ok'")
        time.sleep(0.5)
        es = growth_entries(port, cx, cy, "tomato_plant")
        ok3 = (len(es) == 1 and es[0].get("health") == 1.0
               and es[0].get("age", 99) < 2.0)
        passed &= ok3
        print(f"  [{'PASS' if ok3 else 'FAIL'}] plantRowCropAt places a real "
              f"row-crop instance at full health, age~0: {es}")

        # --- 4. Rot: fruiting window then senescing, back-to-back with
        #     no sleep so the (still-elsewhere) acolyte gets no wall-clock
        #     window to auto-harvest it first. Recipe: land the SECOND
        #     setDate ~197 days after whatever moment this was planted
        #     at (mirrors crop_probe.py's proven day5->day202 jump for
        #     tomato_plant's real annualCycle: fruiting@90..240). Its OWN
        #     tile (D) — a planted tile now refuses a second planting
        #     (see check 3b below), so this can't reuse C. ---
        dz = jget(port, f"local sz=world.getSurfaceAt({dx},{dy}); return sz")
        till_and_wait(port, "probe", dx, dy, dz)
        set_date(port, "probe", 2, 1, 5)
        send(port, f"world.plantRowCropAt('probe',{dx},{dy},'tomato_plant'); "
                   f"return 'ok'")
        set_date(port, "probe", 2, 7, 21)
        ripe = growth_entries(port, dx, dy, "tomato_plant")
        ok4a = any(e.get("stage") == "fruiting" and e.get("harvestable")
                   for e in ripe)
        passed &= ok4a
        print(f"  [{'PASS' if ok4a else 'FAIL'}] planted row crop reaches "
              f"its fruiting window: {ripe}")
        set_date(port, "probe", 2, 9, 15)
        rotten = growth_entries(port, dx, dy, "tomato_plant")
        ok4b = ok4a and not any(e.get("harvestable") for e in rotten)
        passed &= ok4b
        print(f"  [{'PASS' if ok4b else 'FAIL'}] ignored ripe crop rots past "
              f"senescing: {rotten}")

        # --- 4b. plant.designate itself refuses an occupied tile (the
        #     rotten-but-still-standing tomato at D) — the designation
        #     path is the actual gate now, not just the two planting
        #     primitives, so a farm AI never spends a full walk-and-work
        #     cycle on a designation that was always going to fail. ---
        send(port, f"plant.designate('probe',{dx},{dy},'wheat'); return 'ok'")
        time.sleep(0.5)
        d4c = jget(port, f"return plant.getDesignationAt('probe',{dx},{dy})")
        ok4c = not isinstance(d4c, dict)
        passed &= ok4c
        print(f"  [{'PASS' if ok4c else 'FAIL'}] plant.designate refuses an "
              f"already-occupied tile: {d4c}")

        # --- 3b. A tile that's already been planted refuses a second
        #     planting (guards against overlapping/duplicate instances
        #     stacking from a re-designate or a repeated direct call). ---
        send(port, f"world.plantRowCropAt('probe',{cx},{cy},'tomato_plant'); "
                   f"return 'ok'")
        time.sleep(0.5)
        es_dup = growth_entries(port, cx, cy, "tomato_plant")
        ok3b = len(es_dup) == 1
        passed &= ok3b
        print(f"  [{'PASS' if ok3b else 'FAIL'}] plantRowCropAt refuses an "
              f"already-planted tile (no duplicate instance): {es_dup}")

        # --- 3c. The reciprocal cross-form guard: plantCropAt (the
        #     groundcover primitive) refuses a tile that already has a
        #     row-crop FloraInstance on it (tile C, above) — otherwise a
        #     CropPlot lands underneath the existing plant since
        #     isPlantable is tilled-soil-only and stays true either way. ---
        cross_plant = jget(port, f"return world.plantCropAt({cx},{cy},'wheat')")
        cross_plot = jget(port, f"return world.getCropPlotAt({cx},{cy})")
        ok3c = cross_plant in (None, False) and cross_plot is None
        passed &= ok3c
        print(f"  [{'PASS' if ok3c else 'FAIL'}] plantCropAt refuses a tile "
              f"already carrying a row-crop instance: planted={cross_plant} "
              f"plot={cross_plot}")

        # --- 5. plant.getDesignationAt's category field (its own fresh,
        #     unoccupied tile F — C is already planted by this point,
        #     and an occupied tile now refuses designation, check 4b). ---
        fz = jget(port, f"local sz=world.getSurfaceAt({fx},{fy}); return sz")
        till_and_wait(port, "probe", fx, fy, fz)
        send(port, f"plant.designate('probe',{fx},{fy},'tomato_plant'); "
                   f"return 'ok'")
        time.sleep(0.5)
        dcat_row = jget(port, f"return plant.getDesignationAt('probe',{fx},{fy})")
        send(port, f"plant.designate('probe',{fx},{fy},'wheat'); return 'ok'")
        time.sleep(0.5)
        dcat_ground = jget(port, f"return plant.getDesignationAt('probe',{fx},{fy})")
        ok5 = (isinstance(dcat_row, dict) and dcat_row.get("category") == "row_crop"
               and isinstance(dcat_ground, dict)
               and dcat_ground.get("category") == "groundcover_crop")
        passed &= ok5
        print(f"  [{'PASS' if ok5 else 'FAIL'}] getDesignationAt reports "
              f"category: row={dcat_row} ground={dcat_ground}")
        send(port, f"plant.cancelDesignation({fx},{fy}); return 'ok'")

        # --- 6/7/8. Full AI loop: till (A) -> plant (A, wheat) ->
        #     plant (B, tomato_plant, pre-tilled) ---
        send(port, "engine.loadScript('scripts/unit_stats.lua', 0.1); "
                   "return 'ok'")
        send(port, "engine.loadScript('scripts/unit_resources.lua', 0.2); "
                   "return 'ok'")
        send(port, "engine.loadScript('scripts/unit_ai.lua', 0.1); "
                   "return 'ok'")
        bz = jget(port, f"local sz=world.getSurfaceAt({bx},{by}); return sz")
        till_and_wait(port, "probe", bx, by, bz)

        uid = spawn_worker(port, ax + 2, ay)
        if uid < 0:
            print(f"  [FAIL] could not spawn farm worker")
            return 1
        farming_before = get_skill(port, uid, "farming")

        send(port, f"till.designate('probe',{ax},{ay},{ax},{ay}); return 'ok'")
        send(port, f"plant.designate('probe',{bx},{by},'tomato_plant'); "
                   f"return 'ok'")

        deadline = time.time() + 90.0
        tilled = False
        while time.time() < deadline:
            time.sleep(2.0)
            if jget(port, f"return world.isPlantable({ax},{ay})") is True:
                tilled = True
                break
        ok6a = tilled
        passed &= ok6a
        print(f"  [{'PASS' if ok6a else 'FAIL'}] acolyte tills site A "
              f"autonomously: {tilled}")
        if not ok6a:
            print("\nSOME FAILED")
            return 1

        send(port, f"plant.designate('probe',{ax},{ay},'wheat'); return 'ok'")

        deadline = time.time() + 120.0
        planted_a = planted_b = False
        while time.time() < deadline:
            time.sleep(2.0)
            if not planted_a:
                pa = jget(port, f"return world.getCropPlotAt({ax},{ay})")
                if isinstance(pa, dict) and pa.get("id") == "wheat":
                    planted_a = True
            if not planted_b:
                eb = growth_entries(port, bx, by, "tomato_plant")
                if eb:
                    planted_b = True
            if planted_a and planted_b:
                break
        ok6 = planted_a
        passed &= ok6
        print(f"  [{'PASS' if ok6 else 'FAIL'}] acolyte plants wheat at site A "
              f"(world.plantCropAt): planted={planted_a}")
        ok7 = planted_b
        passed &= ok7
        print(f"  [{'PASS' if ok7 else 'FAIL'}] acolyte plants tomato_plant at "
              f"site B (world.plantRowCropAt): planted={planted_b}")

        da = jget(port, f"return plant.getDesignationAt('probe',{ax},{ay})")
        db = jget(port, f"return plant.getDesignationAt('probe',{bx},{by})")
        ok7b = not isinstance(da, dict) and not isinstance(db, dict)
        passed &= ok7b
        print(f"  [{'PASS' if ok7b else 'FAIL'}] both plant designations "
              f"cleared on completion: A={da} B={db}")

        farming_after_plant = get_skill(port, uid, "farming")
        ok8 = farming_after_plant > farming_before
        passed &= ok8
        print(f"  [{'PASS' if ok8 else 'FAIL'}] farming skill grows from "
              f"till+plant: {farming_before} -> {farming_after_plant}")

        # --- 9. Auto-harvest: fast-forward wheat past its 30-day
        #     vegetating threshold, then let the same idle acolyte find
        #     and harvest it on its own (not hunger-gated). A real
        #     generated world has plenty of ambient wild forage, which
        #     the same world.findHarvestableFlora search also covers —
        #     left unchecked the unit happily wanders off picking wild
        #     bushes instead of the plot under test, forever finding a
        #     nearer distraction before it ever arrives. Two mitigations,
        #     confirmed sufficient by an isolated repro: station the
        #     unit adjacent to the plot first (a raw moveTo, bypassing
        #     the AI decision this once), and sweep wild forage away
        #     from the site frequently.
        clear_wild_forage(port, ax, ay)
        send(port, "world.setTimeScale('probe', 50000); return 'ok'")
        deadline = time.time() + 15.0
        ripe_wheat = False
        while time.time() < deadline:
            time.sleep(1.0)
            pw = jget(port, f"return world.getCropPlotAt({ax},{ay})")
            if isinstance(pw, dict) and pw.get("harvestable") is True:
                ripe_wheat = True
                break
        send(port, "world.setTimeScale('probe', 1); return 'ok'")
        ok9a = ripe_wheat
        passed &= ok9a
        print(f"  [{'PASS' if ok9a else 'FAIL'}] wheat becomes harvestable "
              f"under the game clock: {ripe_wheat}")

        send(port, f"unit.moveTo({uid}, {ax + 0.5}, {ay + 1.5}, 1.0); return 'ok'")
        deadline = time.time() + 20.0
        while time.time() < deadline:
            time.sleep(1.0)
            info = jget(port, f"return unit.getInfo({uid})")
            if isinstance(info, dict):
                dx = info.get("gridX", 0) - ax
                dy = info.get("gridY", 0) - ay
                if dx * dx + dy * dy <= 4.0:
                    break
        clear_wild_forage(port, ax, ay, radius=60, keep=(ax, ay))

        # The big time-jump above can ripen a lot of ambient wild flora
        # at once, not just the wheat plot — keep sweeping it away each
        # poll, tightly, so the plot stays the ONLY harvestable thing in
        # range and the now-nearby unit can't get waylaid en route by a
        # regrowing wild bush.
        deadline = time.time() + 60.0
        harvested = False
        while time.time() < deadline:
            time.sleep(0.5)
            clear_wild_forage(port, ax, ay, radius=60, keep=(ax, ay))
            pw2 = jget(port, f"return world.getCropPlotAt({ax},{ay})")
            if pw2 is None:
                harvested = True
                break
        ground = jget(port, "return item.listGround()")
        has_grain = isinstance(ground, list) and any(
            g.get("defName") == "wheat_grain" for g in ground)
        ok9 = harvested and has_grain
        passed &= ok9
        print(f"  [{'PASS' if ok9 else 'FAIL'}] acolyte auto-harvests the ripe "
              f"wheat: plot_cleared={harvested} grain_on_ground={has_grain}")

        farming_after_harvest = get_skill(port, uid, "farming")
        ok9b = farming_after_harvest > farming_after_plant
        passed &= ok9b
        print(f"  [{'PASS' if ok9b else 'FAIL'}] farming skill grows further "
              f"from auto-harvest: {farming_after_plant} -> "
              f"{farming_after_harvest}")

        # --- 10. Re-designating a claimed tile with a DIFFERENT crop
        #     must not plant the stale crop the unit originally claimed.
        #     plant.designate replaces in place (HM.insert), so a player
        #     can swap the crop mid-job; the AI must notice and plant
        #     whatever's there NOW, not what it walked over for. ---
        tE = find_tillable(port, exclude=used)
        if not tE:
            print("  [FAIL] no tillable tile found for site E (try another seed)")
            return 1
        used.add(tE)
        ex, ey = tE
        ez = jget(port, f"local sz=world.getSurfaceAt({ex},{ey}); return sz")
        till_and_wait(port, "probe", ex, ey, ez)
        send(port, f"plant.designate('probe',{ex},{ey},'wheat'); return 'ok'")

        deadline = time.time() + 30.0
        claimed = False
        while time.time() < deadline:
            time.sleep(0.5)
            job = send(port,
                       f"local ai=require('scripts.unit_ai'); "
                       f"local s=ai.getState({uid}); "
                       f"if s and s.plantJob then return s.plantJob.x..','.."
                       f"s.plantJob.y else return 'none' end").strip('"')
            if job == f"{ex},{ey}":
                claimed = True
                break
        if not claimed:
            print("  [FAIL] acolyte never claimed the re-designation test job")
            return 1
        send(port, f"plant.designate('probe',{ex},{ey},'tomato_plant'); "
                   f"return 'ok'")

        deadline = time.time() + 90.0
        redesignated_done = False
        while time.time() < deadline:
            time.sleep(2.0)
            de = jget(port, f"return plant.getDesignationAt('probe',{ex},{ey})")
            if not isinstance(de, dict):
                redesignated_done = True
                break
        planted_new = growth_entries(port, ex, ey, "tomato_plant")
        planted_stale = jget(port, f"return world.getCropPlotAt({ex},{ey})")
        ok10 = (redesignated_done and len(planted_new) >= 1
                and planted_stale is None)
        passed &= ok10
        print(f"  [{'PASS' if ok10 else 'FAIL'}] re-designating a claimed tile "
              f"plants the NEW crop, not the stale one: "
              f"new={planted_new} stale_plot={planted_stale}")

        # --- 11. Save/load: the AI-planted row crop (WePlaceFlora, v79)
        #     survives save -> loadSave. ---
        send(port, "engine.saveWorld('probe', 'farm_ai_v79_check'); "
                   "return 'ok'")
        time.sleep(3.0)
        send(port, "engine.loadSave('farm_ai_v79_check'); return 'ok'")
        time.sleep(15.0)
        send(port, "world.show('main_world'); return 'ok'")
        send(port, "engine.setPaused(false); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)
        eb2 = growth_entries(port, bx, by, "tomato_plant")
        ok10 = len(eb2) >= 1
        passed &= ok10
        print(f"  [{'PASS' if ok10 else 'FAIL'}] AI-planted row crop survives "
              f"save/load: {eb2}")

        print("\n" + ("ALL FARM AI CHECKS PASSED" if passed else "SOME FAILED"))
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
