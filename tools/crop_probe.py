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
     it headless anyway, this probe loads a copy of its exact YAML
     entry (parsed straight out of data/flora/crops.yaml, not
     hand-duplicated) under the DISTINCT name `probe_row_crop`, with
     only the worldGen tolerances relaxed for guaranteed placement.
     Everything else — phases, annualCycle, cycleOverrides, the
     harvestable block and so the `tomato` yield item — is
     byte-identical to what ships, which is the point of deriving it
     from the real content rather than hand-writing a proxy. The name
     has to differ because #2241 made a duplicate authored flora name
     a whole-file refusal: the authored name is a species' stable key.
  2. Groundcover crop: NOT a FloraInstance at all — planted via the new
     world.plantCropAt primitive into a World.Flora.CropPlot, and
     rendered as the tile's veg-fill rather than a floating sprite.
     Tests the REAL shipped `wheat` species + `wheat_grain` item
     directly (planting doesn't depend on worldGen density). Also
     checks world.plantCropAt REFUSES a row_crop species — the SHIPPED
     `tomato_plant`, not the relaxed copy, so the refusal is asserted
     against real content — because only a groundcover_crop can become
     a CropPlot.

Checks per form: growth is derived and visibly advances (age/phase),
a harvest yields the species' item, and (groundcover only) the planted
state survives save -> load.

Usage: python3 tools/crop_probe.py [--port 9195] [--seed 42]
       [--size 64] [--plates 3]
"""
import argparse, copy, glob, os, shutil, socket, subprocess, sys, tempfile, time
import yaml
from pathlib import Path
import probe_protocol
from probelib import (FixtureNotRegistered, quit_engine, boot,
                      load_fixture_yaml, send, send_json, wait_load_published)

SPROOT = "/tmp"
REPO = Path(__file__).resolve().parent.parent


def make_isolated_root(base: str) -> str:
    """A throwaway resource root: real scripts/assets/data/config
    (symlinked -- read-only content, safe to share) plus its OWN empty
    saves/ directory, so this probe never touches a real player's saves
    (round-6 review, issue #767 requirement 15's cross-referenced-probe
    isolation gap)."""
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


# Max-tolerance worldGen override for the placement-tolerant row-crop
# copy below — same numbers flora_growth_probe.py's probe_berry uses,
# just relaxed so it places on any seed's geography regardless of the
# shipped species' real (narrower) climate gate.
# The probe's own row-crop species name. Distinct from the shipped
# `tomato_plant` it is copied from, because a duplicate authored flora
# name is refused whole-file since #2241.
PROBE_ROW_CROP = "probe_row_crop"

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
            r = send_json(port, f"return {fn}('{path}')")
            if path.endswith("crops.yaml") or path.endswith(
                    ("tomato.yaml", "wheat_grain.yaml")):
                checked[path] = r

    # tomato_plant ships with worldGen density 0.0 (#334: farmed crops
    # shouldn't wild-spawn), so it can never place in an ordinary
    # generated world. To exercise the REAL shipped row-crop content
    # (not a hand-written proxy), parse its exact entry back out of
    # data/flora/crops.yaml and reload it with only worldGen relaxed —
    # phases/annualCycle/cycleOverrides/harvestable (and so the "tomato"
    # yield item) stay byte-identical to what ships.
    #
    # Under a DIFFERENT authored name. #2241 made the authored name a
    # species' stable key: it salts the placement roll, names the plant
    # in its instance identity, and a second definition claiming it is
    # now a whole-file refusal. Re-registering the shipped tomato_plant
    # is therefore no longer an option, and reusing the shipped entry
    # as-is is not either — its density is 0.0 by design. Registering
    # after the shipped flora, before world.init, remains what makes it
    # visible to worldgen.
    with open("data/flora/crops.yaml") as f:
        crops = yaml.safe_load(f)
    tomato = copy.deepcopy(
        next(e for e in crops["flora"] if e["name"] == "tomato_plant"))
    tomato["name"] = PROBE_ROW_CROP
    tomato["worldGen"] = {**tomato["worldGen"], **RELAXED_WORLDGEN}
    path = f"{SPROOT}/{PROBE_ROW_CROP}.yaml"
    with open(path, "w") as f:
        yaml.safe_dump({"flora": [tomato]}, f)
    load_fixture_yaml(port, "engine.loadFloraYaml", path)
    return checked


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
    """Nearest tile to (cx, cy) with a real surface, no fluid, and no
    existing flora on it — world.getSurfaceAt returns MULTIPLE Lua
    values (surfaceZ, terrainZ, fluidType, fluidSurface), not a table,
    so wrap it into one value per call. The flora exclusion matters
    since #336: world.plantCropAt now refuses a tile that already
    carries a flora instance (no planting underneath/on top of an
    existing plant), so a wild bush here would make the subsequent
    plantCropAt call spuriously refuse. Returns (gx, gy, surfaceZ) or
    None."""
    r = send(
        port,
        f"for r=0,{radius} do for dx=-r,r do for dy=-r,r do "
        f"local gx,gy={cx}+dx,{cy}+dy; "
        f"local sz,tz,ft=world.getSurfaceAt(gx,gy); "
        f"if sz and not ft and not world.getFloraAt(gx,gy) then "
        f"return gx..','..gy..','..sz end "
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
        if send_json(port, f"return world.isPlantable({gx},{gy})") is True:
            return True
        time.sleep(0.2)
    sys.exit(f"setVegAt({gx},{gy}) never landed")


def growth_entries(port, gx, gy, species):
    t = send_json(port, f"return world.getFloraGrowthAt({gx},{gy})")
    if isinstance(t, list):
        return [e for e in t if e.get("id") == species]
    return []


PROBE_CHECKS = [
    ('content_loads', 'shipped crop content loads cleanly'),
    ('row_count', 'row-crop category places exactly 3 instances per tile (rowOffset)'),
    ('row_growth', 'row-crop instances report derived growth state'),
    ('row_unripe', 'row crop not harvestable before its fruiting window'),
    ('row_ripe', 'row crop harvestable in its fruiting window'),
    ('row_yield', 'row-crop harvest yields tomato'),
    ('untilled_gate', "untilled tile refuses plantCropAt's gate before tilling"),
    ('untilled_refused', 'plantCropAt refuses on untilled soil'),
    ('tilled_gate', 'tile plantable after tilling (vegTilledSoil)'),
    ('row_refused', 'plantCropAt refuses a row_crop species (tomato_plant) on tilled soil'),
    ('row_plot_absent', 'the refused tomato_plant plant left no crop plot behind'),
    ('wheat_planted', "plantCropAt plants the real 'wheat' species"),
    ('wheat_sprout', 'freshly planted crop plot starts at age ~0, sprout phase'),
    ('wheat_growth', 'groundcover crop visibly advances under the game clock and becomes harvestable'),
    ('wheat_yield', 'groundcover-crop harvest yields wheat_grain'),
    ('harvest_clears', 'harvest clears the plot (annual, one-shot)'),
    ('plot_restored', 'planted crop plot survives save/load'),
]
DESCRIPTOR = probe_protocol.build_descriptor('crop', PROBE_CHECKS)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9195)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    ap.add_argument("--describe", action="store_true",
                    help="print the probe-result/v1 descriptor without booting")
    args = ap.parse_args()
    if args.describe:
        print(DESCRIPTOR.to_json())
        return 0
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    try:
        return _run(args, rep)
    except Exception as exc:
        rep.abort(str(exc))
        raise
    finally:
        rep.close()


def _run(args, rep):
    port = args.port
    passed = True

    tmpdir = tempfile.mkdtemp(prefix="crop_probe_")
    try:
        root = make_isolated_root(tmpdir)
        proc = boot(port, rep.engine_log_path("crop_engine.log", f"{SPROOT}/crop_probe_engine.log"),
                    args=["--resource-root", root] + rep.engine_args())
        return _exercise(port, proc, args, passed, rep)
    finally:
        shutil.rmtree(tmpdir, ignore_errors=True)


def _exercise(port, proc, args, passed, rep):
    try:
        checked = bootstrap(port)
        ok0 = all(v not in (0, None, "") for v in checked.values()) \
            and len(checked) >= 3
        passed &= ok0
        rep.check('content_loads', ok0, f'shipped crop content loads cleanly: {checked}')

        send(port, f"world.init('probe', {args.seed}, {args.size}, {args.plates}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, "world.show('probe'); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)

        # ============== 1. Row crop (natural placement) ==============
        set_date(port, "probe", 2, 1, 5)  # dormant/budding season baseline
        tile = find_species_tile(port, PROBE_ROW_CROP)
        if not tile:
            rep.abort(f'{PROBE_ROW_CROP} not found in region — try another seed')
            return 1

        es = growth_entries(port, *tile, PROBE_ROW_CROP)
        ok1a = len(es) == 3
        passed &= ok1a
        rep.check(
            'row_count',
            ok1a,
            f'row-crop category places exactly 3 instances per tile (rowOffset): found {len(es)}'
        )

        ok1b = all(0.0 <= e["health"] <= 1.0 and e["age"] >= 0.0 for e in es)
        passed &= ok1b
        rep.check('row_growth', ok1b, f'row-crop instances report derived growth state: {es}')

        # Season window (the shipped tomato_plant annualCycle this copy
        # carries verbatim: dormant@0 /
        # budding@30 / flowering@60 / fruiting@90 / senescing@240):
        # dormant/budding now, fruiting once the date moves into its window.
        ok1c = all(e.get("stage") in ("dormant", "budding")
                   and not e.get("harvestable") for e in es)
        passed &= ok1c
        rep.check('row_unripe', ok1c, f'row crop not harvestable before its fruiting window: {es}')
        set_date(port, "probe", 2, 7, 21)  # day-of-year ~202, in [90,240)
        es2 = growth_entries(port, *tile, PROBE_ROW_CROP)
        ok1d = any(e.get("stage") == "fruiting" and e.get("harvestable")
                   for e in es2)
        passed &= ok1d
        rep.check('row_ripe', ok1d, f'row crop harvestable in its fruiting window: {es2}')

        y1 = send_json(port, f"return world.harvestFlora({tile[0]},{tile[1]})")
        ok1e = isinstance(y1, list) and len(y1) >= 1 \
            and all(it.get("id") == "tomato" for it in y1)
        passed &= ok1e
        rep.check('row_yield', ok1e, f'row-crop harvest yields tomato: {y1}')

        # ======= 2. Groundcover crop (planted via world.plantCropAt) =======
        found = find_dry_tile(port, tile[0] + 3, tile[1] + 3)
        if not found:
            rep.abort('no dry tile found near the row-crop site')
            return 1
        gx0, gy0, z0 = found

        pre = send_json(port, f"return world.isPlantable({gx0},{gy0})")
        ok2a = pre is False
        passed &= ok2a
        rep.check(
            'untilled_gate',
            ok2a,
            f"untilled tile refuses plantCropAt's gate before tilling: isPlantable={pre}"
        )
        refused = send_json(port,
            f"return world.plantCropAt({gx0},{gy0},'wheat')")
        ok2b = refused in (None, False)
        passed &= ok2b
        rep.check('untilled_refused', ok2b, f'plantCropAt refuses on untilled soil: {refused}')

        ok2c = till_and_wait(port, "probe", gx0, gy0, z0)
        passed &= ok2c
        rep.check('tilled_gate', ok2c, f'tile plantable after tilling (vegTilledSoil): {ok2c}')

        # plantCropAt is a CropPlot-only primitive: a row_crop species
        # (tomato_plant is an ordinary FloraInstance, not a CropPlot) must
        # be refused even on tilled, otherwise-plantable soil.
        row_refused = send_json(port,
            f"return world.plantCropAt({gx0},{gy0},'tomato_plant')")
        ok2r = row_refused in (None, False)
        passed &= ok2r
        rep.check(
            'row_refused',
            ok2r,
            f'plantCropAt refuses a row_crop species (tomato_plant) on tilled soil: {row_refused}'
        )
        cleared_row = send_json(port, f"return world.getCropPlotAt({gx0},{gy0})")
        ok2s = cleared_row is None
        passed &= ok2s
        rep.check(
            'row_plot_absent',
            ok2s,
            f'the refused tomato_plant plant left no crop plot behind: {cleared_row}'
        )

        planted = send_json(port, f"return world.plantCropAt({gx0},{gy0},'wheat')")
        ok2d = planted is True
        passed &= ok2d
        rep.check('wheat_planted', ok2d, f"plantCropAt plants the real 'wheat' species: {planted}")

        p0 = send_json(port, f"return world.getCropPlotAt({gx0},{gy0})")
        ok2e = isinstance(p0, dict) and p0.get("id") == "wheat" \
            and p0.get("phase") == "sprout" and p0.get("age") < 5.0
        passed &= ok2e
        rep.check('wheat_sprout', ok2e, f'freshly planted crop plot starts at age ~0, sprout phase: {p0}')

        # Advance the REAL game clock (not a calendar jump — CropPlot age
        # is measured relative to its OWN planted day, see
        # World.Flora.CropPlot) far enough to clear the 30-day vegetating
        # threshold: ~50000 game-min/real-sec for 4 real-sec ~= 139 game-days.
        send(port, "world.setTimeScale('probe', 50000); return 'ok'")
        time.sleep(4.0)
        send(port, "world.setTimeScale('probe', 1); return 'ok'")
        p1 = send_json(port, f"return world.getCropPlotAt({gx0},{gy0})")
        ok2f = isinstance(p1, dict) and p1.get("age", 0) > p0.get("age", 0) \
            and p1.get("phase") != "sprout" and p1.get("harvestable") is True
        passed &= ok2f
        rep.check(
            'wheat_growth',
            ok2f,
            f'groundcover crop visibly advances under the game clock and becomes harvestable: {p0} -> {p1}'
        )

        y2 = send_json(port, f"return world.harvestFlora({gx0},{gy0})")
        ok2g = isinstance(y2, list) and len(y2) >= 1 \
            and all(it.get("id") == "wheat_grain" for it in y2)
        passed &= ok2g
        rep.check('wheat_yield', ok2g, f'groundcover-crop harvest yields wheat_grain: {y2}')

        cleared = send_json(port, f"return world.getCropPlotAt({gx0},{gy0})")
        ok2h = cleared is None
        passed &= ok2h
        rep.check('harvest_clears', ok2h, f'harvest clears the plot (annual, one-shot): {cleared}')

        # ============== 3. Groundcover plot survives save/load ==============
        found2 = find_dry_tile(port, gx0 + 2, gy0 + 2)
        if not found2:
            rep.abort('no dry tile found for the save/load plot')
            return 1
        gx1, gy1, z1 = found2
        till_and_wait(port, "probe", gx1, gy1, z1)
        send_json(port, f"return world.plantCropAt({gx1},{gy1},'wheat')")
        before = send_json(port, f"return world.getCropPlotAt({gx1},{gy1})")

        send(port, "engine.saveWorld('probe', 'crop_plot_check'); return 'ok'")
        time.sleep(3.0)
        send(port, "engine.loadSave('crop_plot_check'); return 'ok'")
        published, load_status = wait_load_published(port, 200)
        if not published:
            rep.abort(f'load transaction did not publish: {load_status}')
            return 1
        send(port, "world.show('probe'); return 'ok'")
        after = send_json(port, f"return world.getCropPlotAt({gx1},{gy1})")
        ok3 = isinstance(before, dict) and isinstance(after, dict) \
            and before.get("id") == after.get("id") == "wheat" \
            and after.get("age", -1) >= 0.0
        passed &= ok3
        rep.check('plot_restored', ok3, f'planted crop plot survives save/load: {before} -> {after}')

        rep.note('\n' + ('ALL CROP CHECKS PASSED' if passed else 'SOME FAILED'))
        return 0 if passed else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    try:
        sys.exit(main())
    except FixtureNotRegistered as exc:
        print(f"\n{exc}")
        sys.exit(1)
