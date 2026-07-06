#!/usr/bin/env python3
"""Flora growth runtime probe (#332).

Boots a headless engine on a real generated world (flora placement
needs worldgen) and checks the DERIVED growth runtime end-to-end:

  1. Clock: the calendar date advances on its own when the world clock
     runs (midnight rollover in tickWorldTime — world.getDate moves
     under a cranked time scale).
  2. Inspection: world.getFloraGrowthAt reports per-instance derived
     state (age / health / phase / stage / generation).
  3. Season window: a fruiting species is harvestable only inside its
     fruiting window; a leaves species (white_clover) stays open in the
     dormant season. Poked via world.setDate. The fruiting species is a
     probe-registered `probe_berry` (raspberry-shaped, max-tolerance
     worldGen so it places on any seed's geography — real raspberries
     are climate-gated and absent from many spawn regions); it is
     appended AFTER the data/flora species so their placement rolls
     stay untouched.
  4. Aging + reseed: jumping the date years ahead grows ages; far
     enough out a perennial has wrapped to generation >= 1 (the old
     plant died through the dead window and reseeded).
  5. Persistence: the date (the growth clock) survives save -> load,
     so growth state does too — it derives from date + deterministic
     placement.

Usage: python3 tools/flora_growth_probe.py [--port 9186] [--seed 42]
       [--size 64] [--plates 3]
"""
import argparse, glob, json, socket, subprocess, sys, time
from probelib import boot, send

SPROOT = "/tmp"


def jget(port, lua, timeout=10.0):
    raw = send(port, lua, timeout)
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return raw.strip('"')


PROBE_BERRY_YAML = """flora:
  - name: probe_berry
    type: deciduous_shrub
    texDir: "assets/textures/flora/red_raspberry"
    lifecycle: perennial
    minLife: 1080
    maxLife: 3600
    deathChance: 0.1
    phases:
      - {tag: sprout, texture: "sprout.png", age: 0}
      - {tag: matured, texture: "matured.png", age: 360}
      - {tag: dead, texture: "dead.png", age: 3600}
    annualCycle:
      - {tag: dormant, startDay: 0, texture: "matured_dormant.png"}
      - {tag: fruiting, startDay: 180, texture: "matured_fruiting.png"}
      - {tag: senescing, startDay: 270, texture: "matured_senescing.png"}
    harvestable:
      tags: [fruit]
      yield:
        - id: wild_berries
          count: [1, 3]
      regrowth_time: 86400
      harvested_texture: "matured_senescing.png"
    worldGen:
      category: bush
      minTemp: -60
      maxTemp: 60
      idealTemp: 15
      minPrecip: 0.0
      maxPrecip: 5.0
      idealPrecip: 0.8
      minAlt: -100
      maxAlt: 3000
      idealAlt: 50
      minHumidity: 0.0
      maxHumidity: 1.0
      idealHumidity: 0.5
      maxSlope: 7
      density: 1.0
      footprint: 0
"""


def bootstrap(port):
    for pattern, fn in [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/flora/*.yaml",      "engine.loadFloraYaml"),
    ]:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")
    # The probe's own fruiting species — appended after the real flora
    # so their placement hashes (indexed by registration order) are
    # untouched. Max-tolerance worldGen: places on any seed.
    berry_path = f"{SPROOT}/probe_berry.yaml"
    with open(berry_path, "w") as f:
        f.write(PROBE_BERRY_YAML)
    send(port, f"engine.loadFloraYaml('{berry_path}'); return 'ok'")


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


def growth_entry(port, gx, gy, species):
    t = jget(port, f"return world.getFloraGrowthAt({gx},{gy})")
    if isinstance(t, list):
        for e in t:
            if e.get("id") == species:
                return e
    return None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9186)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    args = ap.parse_args()
    port = args.port
    passed = True

    proc = boot(port, f"{SPROOT}/flora_growth_probe_engine.log")
    try:
        bootstrap(port)
        send(port, f"world.init('probe', {args.seed}, {args.size}, {args.plates}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, "world.show('probe'); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)

        # --- 1. The clock ticks: date advances under a cranked scale ---
        d0 = jget(port, "return world.getDate('probe')")
        ok = isinstance(d0, dict) and d0.get("absoluteDay") is not None
        passed &= ok
        print(f"  [{'PASS' if ok else 'FAIL'}] getDate reads the calendar: {d0}")
        # 3000 game-min/real-sec for ~3 real-sec ≈ 6 game-days
        send(port, "world.setTimeScale('probe', 3000); return 'ok'")
        time.sleep(3.0)
        send(port, "world.setTimeScale('probe', 1); return 'ok'")
        d1 = jget(port, "return world.getDate('probe')")
        ok1 = isinstance(d1, dict) and isinstance(d0, dict) \
            and d1["absoluteDay"] >= d0["absoluteDay"] + 3
        passed &= ok1
        print(f"  [{'PASS' if ok1 else 'FAIL'}] date advances on the game clock: "
              f"day {d0.get('absoluteDay')} -> {d1.get('absoluteDay')}")

        # --- 2. Growth inspection ---
        # Move into raspberry's fruiting window FIRST (day-of-year 200 =
        # month 7 day 21; window is 180–269), then find an instance the
        # window is actually open for — a random raspberry could
        # legitimately be a sprout or inside its dead window.
        set_date(port, "probe", 2, 7, 21)
        rasp = find_species_tile(port, "probe_berry", harvestable=True)
        clov = find_species_tile(port, "white_clover")
        if not rasp or not clov:
            print(f"  [FAIL] species not found in region (probe_berry={rasp}, "
                  f"clover={clov}) — try another seed")
            return 1
        ef = growth_entry(port, *rasp, "probe_berry")
        ok2 = ef is not None and all(
            k in ef for k in ("age", "health", "generation", "stage",
                              "harvestable", "regrowthRemaining")) \
            and 0.0 <= ef["health"] <= 1.0 and ef["age"] >= 0.0
        passed &= ok2
        print(f"  [{'PASS' if ok2 else 'FAIL'}] getFloraGrowthAt reports derived "
              f"state: {ef}")

        # --- 3. Seasonal harvest window (poked via setDate) ---
        ok3a = ef is not None and ef.get("stage") == "fruiting" \
            and ef.get("harvestable") is True
        passed &= ok3a
        print(f"  [{'PASS' if ok3a else 'FAIL'}] raspberry harvestable in its "
              f"fruiting window: {ef}")
        # THE seasonal assertion: the same plant, only the date changed.
        set_date(port, "probe", 2, 1, 5)
        ed = growth_entry(port, *rasp, "probe_berry")
        ok3b = ed is not None and ed.get("stage") == "dormant" \
            and ed.get("harvestable") is False
        passed &= ok3b
        print(f"  [{'PASS' if ok3b else 'FAIL'}] the same raspberry NOT "
              f"harvestable in the dormant season: {ed}")
        ec = growth_entry(port, *clov, "white_clover")
        ok3c = ec is not None and ec.get("harvestable") is True
        passed &= ok3c
        print(f"  [{'PASS' if ok3c else 'FAIL'}] clover (no fruiting stage) "
              f"still open in the dormant season: {ec}")
        # And the harvest itself respects the window on a fruiting date.
        set_date(port, "probe", 2, 7, 21)
        y = jget(port, f"return world.harvestFlora({rasp[0]},{rasp[1]})")
        ok3d = isinstance(y, list) and len(y) >= 1
        passed &= ok3d
        print(f"  [{'PASS' if ok3d else 'FAIL'}] harvest yields in season: {y}")

        # --- 4. Aging + generational reseed ---
        # +4 years: the plant aged (or, if its lifespan fell in between,
        # wrapped to the next generation — either proves the clock moved).
        age_now = ef["age"] if ef else 0.0
        set_date(port, "probe", 6, 7, 21)
        e4 = growth_entry(port, *rasp, "probe_berry")
        ok4a = e4 is not None and (e4["age"] > age_now
                                   or e4["generation"] >= 1)
        passed &= ok4a
        print(f"  [{'PASS' if ok4a else 'FAIL'}] age grows with the date: "
              f"{age_now:.1f} -> {e4['age'] if e4 else '?'} "
              f"(gen {e4['generation'] if e4 else '?'})")
        # Far out: any perennial must have wrapped at least once — even
        # at the minimum growth rate (0.25), year 80 ≈ 28.6k days is past
        # max lifespan 3600 + dead window 60.
        set_date(port, "probe", 80, 7, 21)
        e5 = growth_entry(port, *rasp, "probe_berry")
        ok4b = e5 is not None and e5["generation"] >= 1
        passed &= ok4b
        print(f"  [{'PASS' if ok4b else 'FAIL'}] perennial reseeded (generation "
              f">= 1) decades out: {e5}")

        # --- 5. The growth clock survives save/load ---
        set_date(port, "probe", 3, 2, 10)
        send(port, "engine.saveWorld('probe', 'flora_growth_check'); return 'ok'")
        time.sleep(3.0)
        send(port, "engine.loadSave('flora_growth_check'); return 'ok'")
        time.sleep(15.0)
        send(port, "world.show('main_world'); return 'ok'")
        d5 = jget(port, "return world.getDate('main_world')")
        ok5 = isinstance(d5, dict) and d5.get("year") == 3 \
            and d5.get("month") == 2 and d5.get("day") == 10
        passed &= ok5
        print(f"  [{'PASS' if ok5 else 'FAIL'}] growth clock survives "
              f"save/load: {d5}")

        print("\n" + ("ALL FLORA GROWTH CHECKS PASSED" if passed else "SOME FAILED"))
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
