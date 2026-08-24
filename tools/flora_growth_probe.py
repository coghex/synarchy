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
     fruiting window; a leaves species with no fruiting stage stays open
     in the dormant season. Poked via world.setDate. Both species under
     test are probe-registered, max-tolerance worldGen fixtures so they
     place reliably on any seed's geography: `probe_berry` (raspberry-
     shaped, fruiting) and `probe_clover` (white-clover-shaped, no
     fruiting stage — mirrors the real white_clover's phases/annual
     cycle rather than depending on natural white_clover placement,
     which isn't guaranteed inside the probe's fixed scan region). Both
     are appended AFTER the data/flora species, in that order, so the
     real species' placement rolls AND probe_berry's own index stay
     untouched.
  4. Aging + reseed: jumping the date years ahead grows ages; far
     enough out a perennial has wrapped to generation >= 1 (the old
     plant died through the dead window and reseeded).
  5. Persistence: the date (the growth clock) survives save -> load,
     so growth state does too — it derives from date + deterministic
     placement. The engine runs on a throwaway resource root, so that
     slot lands in this run's own saves/ and is deleted with it — the
     developer's saves/ is never read, written or rotated (#1616).

Usage: python3 tools/flora_growth_probe.py [--port 9186] [--seed 42]
       [--size 64] [--plates 3]
"""
import argparse, glob, os, shutil, socket, subprocess, sys, tempfile, time, uuid
from probelib import (FixtureNotRegistered, boot, capture_request_id,
                      load_fixture_yaml, quit_engine, send, send_json,
                      wait_load_published, wait_save_complete)

SPROOT = "/tmp"
REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def make_isolated_root(base: str) -> str:
    """A throwaway resource root for one invocation (#1616): the
    read-only content families symlinked, `config/` COPIED without the
    developer's `*.local.yaml` overrides, and its OWN empty `saves/`.

    `app/App/ResourceRoot.hs` chdirs the engine into this directory and
    `World.Save.Serialize` resolves `saves` relative to it, so the round
    trip below writes here instead of the developer's live `saves/` —
    which is gitignored and therefore accumulates abandoned slots
    silently. Copying `config/` rather than symlinking it keeps a
    personal `config/save.local.yaml` out of the run: `scripts/init.lua`
    loads the autosave scheduler, so a local autosave interval could
    otherwise fire a competing save while this probe is winding the
    calendar around and rotate slots underneath it.

    Only the SAVE slot moves here; the probe's own fixture YAML stays
    where it already was, under `/tmp`.
    """
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    config_dst = os.path.join(root, "config")
    if not os.path.exists(config_dst):
        shutil.copytree(os.path.join(REPO, "config"), config_dst,
                        ignore=shutil.ignore_patterns("*.local.yaml"))
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def remove_run_root(base: str) -> bool:
    """Delete this invocation's own throwaway tree, save artifacts and
    all, and say whether it is really gone.

    Only ever removes the directory THIS process made with
    `tempfile.mkdtemp`, so nothing pre-existing is at risk; `rmtree`
    unlinks the symlinked content families rather than recursing into
    them, so the real `scripts/`, `assets/` and `data/` are never
    followed. A survivor makes the run non-zero: a green result sitting
    beside leftover saves is precisely the outcome this isolation
    exists to prevent, so it must not be reported as a pass.
    """
    try:
        shutil.rmtree(base)
    except OSError as exc:
        print(f"  [FAIL] could not remove this run's resource root "
              f"{base}: {exc}")
        return False
    if os.path.exists(base):
        print(f"  [FAIL] this run's resource root survived removal: {base}")
        return False
    return True


def save_and_reload(port, page, slot):
    """The persistence round trip, tied at every step to THIS run's own
    requests (#1616).

    `engine.saveWorld` and `engine.loadSave` only ACCEPT synchronously
    (`src/Engine/Scripting/Lua/API/Save.hs`), so neither return value
    means the work finished and no fixed sleep can stand in for one.
    Each half therefore asserts acceptance, captures that request's own
    id, and waits for a terminal status carrying it. A missing id is
    itself a failure rather than something to wait past: without one the
    wait falls back to accepting whichever terminal status it sees
    first, which is the stale-status hole the request ids exist to
    close.

    Returns None on success, or a message naming the step that broke.
    """
    saved = send(port, f"return engine.saveWorld('{page}', '{slot}')")
    if saved.strip() != "true":
        return f"engine.saveWorld('{slot}') was not accepted (got {saved!r})"
    save_id = capture_request_id(port, "return engine.getSaveStatus()")
    if save_id is None:
        return (f"engine.getSaveStatus() never reported a request id for "
                f"saveWorld('{slot}')")
    ok, save_status = wait_save_complete(port, save_id)
    print(f"  save '{slot}' request {save_id} -> {save_status}")
    if not ok:
        return (f"save '{slot}' (request {save_id}) did not reach "
                f"SaveCaptureComplete: {save_status}")
    if not isinstance(save_status, dict) or save_status.get("id") != save_id:
        return (f"save '{slot}' reported terminal status {save_status!r}, "
                f"which does not carry this run's request id {save_id}")

    loaded = send(port, f"return engine.loadSave('{slot}')")
    if loaded.strip() != "true":
        return f"engine.loadSave('{slot}') was not accepted (got {loaded!r})"
    load_id = capture_request_id(port, "return engine.getLoadStatus()")
    if load_id is None:
        return (f"engine.getLoadStatus() never reported a request id for "
                f"loadSave('{slot}')")
    published, load_status = wait_load_published(port, 200, request_id=load_id)
    print(f"  load '{slot}' request {load_id} -> {load_status}")
    if not published:
        return f"load transaction {load_id} did not publish: {load_status}"
    if not isinstance(load_status, dict) or load_status.get("id") != load_id:
        return (f"load '{slot}' reported terminal status {load_status!r}, "
                f"which does not carry this run's request id {load_id}")
    return None


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

# The probe's own no-fruiting-stage species, standing in for natural
# white_clover so the year-round-harvest assertion doesn't depend on it
# being placed inside the fixed scan region. Phases/annualCycle mirror
# real white_clover (data/flora/temperate_wildflowers.yaml) — no
# `fruiting` stage anywhere in annualCycle. Same wide min/max tolerance
# as probe_berry (still places reliably anywhere), but a DIFFERENT ideal
# niche (cold/wet/humid/highland vs. probe_berry's temperate lowland) —
# a shared ideal point would make World.Flora.Placement.speciesFitness
# score both species near-identically at every tile, so on a
# climate-uniform region (some seeds' loaded chunks) BOTH would place on
# EVERY eligible tile with near-certainty, leaving no tile with a
# harvestable raspberry and no co-located clover for the harvest-action
# test (below) to target unambiguously.
PROBE_CLOVER_YAML = """flora:
  - name: probe_clover
    type: perennial_flower
    texDir: "assets/textures/flora/white_clover"
    lifecycle: perennial
    minLife: 1080
    maxLife: 3600
    deathChance: 0.1
    phases:
      - {tag: sprout, texture: "sprout.png", age: 0}
      - {tag: matured, texture: "budding.png", age: 30}
      - {tag: dead, texture: "dead.png", age: 3600}
    annualCycle:
      - {tag: dormant, startDay: 0, texture: "dormant.png"}
      - {tag: budding, startDay: 60, texture: "budding.png"}
      - {tag: flowering, startDay: 100, texture: "flowering.png"}
      - {tag: senescing, startDay: 200, texture: "senescing.png"}
    harvestable:
      tags: [leaves]
      yield:
        - id: wild_greens
          count: [1, 2]
      regrowth_time: 43200
      harvested_texture: "senescing.png"
    worldGen:
      category: wildflower
      minTemp: -60
      maxTemp: 60
      idealTemp: -20
      minPrecip: 0.0
      maxPrecip: 5.0
      idealPrecip: 3.0
      minAlt: -100
      maxAlt: 3000
      idealAlt: 1200
      minHumidity: 0.0
      maxHumidity: 1.0
      idealHumidity: 0.85
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
    load_fixture_yaml(port, "engine.loadFloraYaml", berry_path)
    # The probe's own no-fruiting-stage species — appended AFTER
    # probe_berry so both the real flora's indices and probe_berry's
    # own index stay untouched. Max-tolerance worldGen: places on any
    # seed, same as probe_berry.
    clover_path = f"{SPROOT}/probe_clover.yaml"
    with open(clover_path, "w") as f:
        f.write(PROBE_CLOVER_YAML)
    load_fixture_yaml(port, "engine.loadFloraYaml", clover_path)


def set_date(port, page, y, mo, d):
    """setDate is a queued world command — send, then wait until
    getDate reflects it."""
    send(port, f"world.setDate('{page}', {y}, {mo}, {d}); return 'ok'")
    for _ in range(20):
        time.sleep(0.2)
        got = send_json(port, f"return world.getDate('{page}')")
        if isinstance(got, dict) and got.get("year") == y \
           and got.get("month") == mo and got.get("day") == d:
            return got
    sys.exit(f"setDate({y},{mo},{d}) never landed")


def find_species_tile(port, species, harvestable=None, exclude=None,
                       extra_cond=None, lo=-64, hi=64):
    """Scan the loaded region for the first tile whose FIRST-listed
    instance of `species` (array order — the same instance
    growth_entry's plain species-id lookup below reads back, so search
    and read always agree on which individual they mean even when a
    placement rolls more than one onto a tile) satisfies the given
    condition: `harvestable` flag and/or an arbitrary extra Lua boolean
    expression over that instance (`e`).

    `exclude` additionally requires NO instance anywhere on the tile
    (not just the first-listed one) carry the named species. Matters
    for a species pair whose max-tolerance worldGen makes them commonly
    share a tile: world.harvestFlora resolves a shared tile's "first
    harvestable" pick by internal list order, not registration order,
    so a harvest-action test on one owned fixture must land on a tile
    the other owned fixture isn't also standing on. Returns (gx, gy) or
    None."""
    cond = f"e.id=='{species}'"
    if harvestable is not None:
        cond += f" and e.harvestable=={'true' if harvestable else 'false'}"
    if extra_cond is not None:
        cond += f" and ({extra_cond})"
    bad_cond = f"x.id=='{exclude}'" if exclude is not None else "false"
    r = send(
        port,
        f"for gx={lo},{hi} do for gy={lo},{hi} do "
        f"local t=world.getFloraGrowthAt(gx,gy); "
        f"if t then local e,bad=nil,false; for _,x in ipairs(t) do "
        f"if e==nil and x.id=='{species}' then e=x end; "
        f"if {bad_cond} then bad=true end end; "
        f"if e and ({cond}) and not bad then return gx..','..gy end end "
        f"end end return 'none'",
        timeout=60.0)
    r = r.strip('"')
    if r == "none":
        return None
    gx, gy = r.split(",")
    return int(gx), int(gy)


def growth_entry(port, gx, gy, species):
    """Read the tile's FIRST-listed instance of `species` — matches
    find_species_tile's own selection above, so a caller tracks the
    same individual across both."""
    t = send_json(port, f"return world.getFloraGrowthAt({gx},{gy})")
    if not isinstance(t, list):
        return None
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

    # This invocation owns its resource root and therefore its saves/
    # (#1616): the round trip at the end writes a slot that no ordinary
    # `cabal run` can reach, and the whole tree goes away below.
    base = tempfile.mkdtemp(prefix="synarchy_flora_growth_")
    root = make_isolated_root(base)
    # Unique per invocation as well as per root, so the slot NAME alone
    # identifies this run even in a log shared with another.
    slot = f"flora_growth_check_{uuid.uuid4().hex[:8]}"
    print(f"isolated resource root: {root}", flush=True)
    print(f"save slot: {slot}", flush=True)

    # `boot` exits the probe outright when the engine dies before READY
    # or never prints it, so it belongs INSIDE the try: otherwise that
    # failure path leaves this run's root — the one thing the cleanup
    # below exists to remove — sitting in the temp directory. A None
    # handle is what `quit_engine` already expects when there is no live
    # process to shut down.
    proc = None
    try:
        proc = boot(port, f"{SPROOT}/flora_growth_probe_engine.log",
                    args=["--resource-root", root])
        bootstrap(port)
        send(port, f"world.init('probe', {args.seed}, {args.size}, {args.plates}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, "world.show('probe'); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)

        # --- 1. The clock ticks: date advances under a cranked scale ---
        d0 = send_json(port, "return world.getDate('probe')")
        ok = isinstance(d0, dict) and d0.get("absoluteDay") is not None
        passed &= ok
        print(f"  [{'PASS' if ok else 'FAIL'}] getDate reads the calendar: {d0}")
        # 3000 game-min/real-sec for ~3 real-sec ≈ 6 game-days
        send(port, "world.setTimeScale('probe', 3000); return 'ok'")
        time.sleep(3.0)
        send(port, "world.setTimeScale('probe', 1); return 'ok'")
        d1 = send_json(port, "return world.getDate('probe')")
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
        # Excludes probe_clover from the raspberry tile: both are
        # max-tolerance fixtures that commonly share a tile, and a
        # shared tile's harvestFlora pick (test 3d below) must
        # unambiguously resolve to the raspberry under test.
        rasp = find_species_tile(port, "probe_berry", harvestable=True,
                                  exclude="probe_clover")
        if not rasp:
            print(f"  [FAIL] probe_berry fixture not found in scan region "
                  f"— this is a fixture-placement regression, not a "
                  f"seed issue")
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
        # probe_clover: searched fresh AT this dormant date, not reused
        # from the earlier fruiting-date scan — age is a pure function
        # of the current absolute day, and this date is well before the
        # earlier one within year 2, so an instance matured there isn't
        # guaranteed still matured here.
        clov = find_species_tile(
            port, "probe_clover",
            extra_cond="e.dead==false and e.phase=='matured'")
        ec = clov and growth_entry(port, *clov, "probe_clover")
        ok3c = clov is not None and ec is not None \
            and ec.get("harvestable") is True \
            and ec.get("dead") is False \
            and ec.get("phase") == "matured"
        passed &= ok3c
        print(f"  [{'PASS' if ok3c else 'FAIL'}] probe_clover (no fruiting "
              f"stage, alive, matured) still open in the dormant season: {ec}")
        # And the harvest itself respects the window on a fruiting date —
        # specifically the raspberry's own yield (wild_berries), not
        # whatever else might be on the tile (rasp was found excluding
        # probe_clover above precisely so this is unambiguous).
        set_date(port, "probe", 2, 7, 21)
        y = send_json(port, f"return world.harvestFlora({rasp[0]},{rasp[1]})")
        ok3d = isinstance(y, list) and len(y) >= 1 \
            and all(item.get("id") == "wild_berries" for item in y)
        passed &= ok3d
        print(f"  [{'PASS' if ok3d else 'FAIL'}] harvest yields raspberry's "
              f"fruit in season: {y}")

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
        failure = save_and_reload(port, "probe", slot)
        if failure:
            print(f"  [FAIL] {failure}")
            return 1
        send(port, "world.show('probe'); return 'ok'")
        d5 = send_json(port, "return world.getDate('probe')")
        ok5 = isinstance(d5, dict) and d5.get("year") == 3 \
            and d5.get("month") == 2 and d5.get("day") == 10
        passed &= ok5
        print(f"  [{'PASS' if ok5 else 'FAIL'}] growth clock survives "
              f"save/load: {d5}")

        print("\n" + ("ALL FLORA GROWTH CHECKS PASSED" if passed else "SOME FAILED"))
        rc = 0 if passed else 1
    finally:
        # Orderly shutdown FIRST: the root must still exist while the
        # engine is closing its own files, and only then does this run's
        # tree (with every save artifact it created) go away — on the
        # failing path exactly as on the passing one.
        #
        # Shut down ONLY an engine this run actually launched. `boot`
        # already disposes of the process it started on either of its own
        # failure paths, and leaves `proc` None — so a None here means
        # the port belongs to somebody else (an instance that was already
        # listening is exactly why a boot fails on a busy port), and
        # `engine.quit()` would be aimed at their engine. Cleanup of the
        # root stays unconditional: that directory is ours either way.
        if proc is not None:
            quit_engine(port, proc)
        cleaned = remove_run_root(base)
    return rc if cleaned else 1


if __name__ == "__main__":
    try:
        sys.exit(main())
    except FixtureNotRegistered as exc:
        print(f"\n{exc}")
        sys.exit(1)
