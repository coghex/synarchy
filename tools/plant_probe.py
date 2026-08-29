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
  2. Soil actually gates suitability, in BOTH directions and for ONE
     crop: forcing the tile's surface material to a species-preferred
     soil (loam) vs a non-preferred one (granite) via world.setCell
     flips that crop's "soil" factor between 1.0 and 0.0 AND its
     overall score between strictly positive and exactly 0.0 — proving
     data/flora/crops.yaml's `soils:` list + the name->id resolution in
     registerFloraSpecies actually take effect, not just parse.

     Both halves are load-bearing (#1762). `speciesFitnessDetail`
     short-circuits to 0.0 for an excessive slope, a non-preferred
     soil, OR any zeroed climate/altitude factor, and the soil test
     comes FIRST — so a granite zero on its own is evidence of soil
     gating only against a positive score for the SAME crop on loam.
     Asserting only the granite zero (what this probe did before #1762)
     stays green even if every preferred-soil score regressed to zero,
     and that vacuous case was live: at the pinned seed, wheat scored
     zero on loam with its soil fit at 1.0, so the wheat half of the
     granite assertion re-asserted a value that was already zero.

     The crop and tile are therefore SELECTED for the property the
     contrast needs (`select_positive_fixture`) rather than taken from
     whichever tile the scan reaches first: the fixture is a tillable
     tile carrying a crop whose non-soil factors are all positive, with
     the resulting positive loam score confirmed against the live
     engine before any assertion runs.
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
import argparse, glob, os, shutil, socket, subprocess, sys, tempfile, time
from pathlib import Path
from probelib import quit_engine, boot, send, send_json, wait_load_published

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

# Every factor except "soil". `speciesFitnessDetail` zeroes the overall
# score when the slope gate fails, when the soil is non-preferred, or
# when any of temperature/precipitation/humidity/altitude reaches 0.0 —
# so a crop whose NON-soil factors are all positive is exactly a crop
# that will score positive once its tile is forced to a preferred soil.
# Soil is excluded because forcing loam is what the contrast then does.
NON_SOIL_FACTORS = FACTOR_NAMES - {"soil"}

# How many flat/dry/flora-free candidate tiles `select_positive_fixture`
# will examine before giving up. The scan is one debug-console round
# trip per factor read, and the pinned seed's first candidate already
# carries a usable crop, so this only bounds the pathological case.
MAX_FIXTURE_CANDIDATES = 40


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


def iter_tillable(port, span=4):
    """Scan sample points across the loaded region, yielding every
    flat, dry, flora-free tile in scan order.

    Yields rather than returning the first hit so `select_positive_fixture`
    can keep walking when a candidate turns out to be climatically
    unusable (#1762) — every tile it yields still carries the flat, dry,
    flora-free preconditions the designation, cancellation, replacement
    and save/load phases below rely on, so the selected fixture stays
    usable for all of them.

    ``world.getFluidAt`` is a MULTI-RETURN query whose ARITY is the
    contract (`Engine.Scripting.Lua.API.WorldQuery.Fluid`): a fluid tile
    pushes TWO values — the type string and the fluid surface z — while a
    dry tile, and one whose chunk is not loaded, pushes a single nil. The
    debug console joins several return values with tabs, so asking for
    both back yields text like ``river\t12``, never anything JSON-shaped.
    Bind the first return alone: a nonempty first value IS the fluid type,
    and means WET. ``getFloraAt`` really is table-or-nil
    (`Engine.Scripting.Lua.API.Forage.Query`), so its dict test below is
    correct as written.
    """
    for sx in range(-span * 16, span * 16 + 1, 4):
        for sy in range(-span * 16, span * 16 + 1, 4):
            slope = send_json(port, f"return world.getSlopeAt({sx},{sy})")
            if slope != 0:
                continue
            fluid = send_json(port, f"local t = world.getFluidAt({sx},{sy}); "
                                    f"return t")
            if isinstance(fluid, str) and fluid:
                continue
            flora = send_json(port, f"return world.getFloraAt({sx},{sy})")
            if isinstance(flora, dict):
                continue
            yield sx, sy


def till_and_wait(port, page, gx, gy, z):
    """world.setVegAt is a queued world command, like world.setDate —
    send, then poll isPlantable until it lands before designating."""
    send(port, f"world.setVegAt('{page}', {gx}, {gy}, {z}, 77); return 'ok'")
    for _ in range(20):
        if send_json(port, f"return world.isPlantable({gx},{gy})") is True:
            return True
        time.sleep(0.2)
    sys.exit(f"setVegAt({gx},{gy}) never landed")


def suitability_row(port, page, gx, gy, species):
    """world.getPlantSuitability(gx,gy) → the row for `species`, or None."""
    rows = send_json(port, f"return world.getPlantSuitability({gx},{gy})")
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


def factor_fits(row):
    """{factor label: fit} for one `getPlantSuitability` row."""
    if not isinstance(row, dict) or not isinstance(row.get("factors"), list):
        return {}
    return {f["factor"]: f["fit"] for f in row["factors"]
            if isinstance(f, dict) and "factor" in f and "fit" in f}


def non_soil_fits_positive(row):
    """True when this row reports every NON-soil factor at a positive
    fit — i.e. nothing but the soil gate can be holding its score at
    zero. Material-independent, so it is read WITHOUT mutating the tile:
    only the "soil" factor depends on the surface material."""
    fits = factor_fits(row)
    if not NON_SOIL_FACTORS <= fits.keys():
        return False
    return all(fits[name] > 0.0 for name in NON_SOIL_FACTORS)


def select_positive_fixture(port, page):
    """Choose the (tile, crop) pair the soil-gating contrast needs (#1762).

    Walks `iter_tillable`'s candidates and, for each, asks
    getPlantSuitability which crops have all their non-soil factors
    positive. The first such tile is forced to loam and the candidate
    crops' overall scores are re-read from the live engine; the first
    crop that really does score above zero on the preferred soil becomes
    the fixture. Nothing is asserted on a tile where no crop could grow —
    requirement 5 — and nothing is inferred from the pre-mutation read:
    the positive score is CONFIRMED before it is asserted against.

    Returns ``(fixture, notes)``: ``fixture`` is
    ``(gx, gy, z, crop, rows)`` on success and ``None`` when no
    candidate could supply one, and ``notes`` records why each rejected
    candidate was rejected so a failure names the reason rather than
    just reporting that nothing was found.
    """
    notes = []
    for seen, (gx, gy) in enumerate(iter_tillable(port), start=1):
        if seen > MAX_FIXTURE_CANDIDATES:
            notes.append(f"gave up after examining {MAX_FIXTURE_CANDIDATES} "
                         f"tillable candidates")
            break
        # world.getSurfaceAt returns MULTIPLE Lua values (surfaceZ,
        # terrainZ, fluidType, fluidSurface), not a table — capture just
        # the first via a local (same quirk crop_probe.py's
        # find_dry_tile documents).
        z = send_json(port,
                      f"local sz=world.getSurfaceAt({gx},{gy}); return sz")
        rows = send_json(port, f"return world.getPlantSuitability({gx},{gy})")
        if not isinstance(rows, list) or not rows:
            notes.append(f"({gx},{gy}): getPlantSuitability returned no rows "
                         f"({rows!r})")
            continue
        candidates = [r for r in rows if non_soil_fits_positive(r)]
        if not candidates:
            notes.append(f"({gx},{gy}): no crop has every non-soil factor "
                         f"positive: "
                         + "; ".join(f"{r.get('name')}={factor_fits(r)}"
                                     for r in rows))
            continue
        # One forcing per tile: the material is shared by every crop, so
        # poll it in on the first candidate and then re-read each one.
        first = candidates[0]["name"]
        if not set_material_and_wait(port, page, gx, gy, z,
                                     LOAM_NAME, first, 1.0):
            notes.append(f"({gx},{gy}): forcing loam never reached soil "
                         f"fit 1.0 for {first}")
            continue
        for candidate in candidates:
            crop = candidate["name"]
            row = suitability_row(port, page, gx, gy, crop)
            if row is not None and row["score"] > 0.0:
                confirmed = send_json(
                    port, f"return world.getPlantSuitability({gx},{gy})")
                return (gx, gy, z, crop, confirmed), notes
            notes.append(f"({gx},{gy}): {crop} still scores "
                         f"{row['score'] if row else None} on loam: {row}")
    return None, notes


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9179)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    args = ap.parse_args()
    port = args.port
    passed = True

    tmpdir = tempfile.mkdtemp(prefix="plant_probe_")
    try:
        root = make_isolated_root(tmpdir)
        proc = boot(port, f"{SPROOT}/plant_probe_engine.log",
                    args=["--resource-root", root])
        return _run(port, proc, args, passed)
    finally:
        shutil.rmtree(tmpdir, ignore_errors=True)


def _run(port, proc, args, passed):
    try:
        bootstrap(port)
        send(port, f"world.init('probe', {args.seed}, {args.size}, "
                   f"{args.plates}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, "world.show('probe'); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)

        # Select the fixture DELIBERATELY (#1762): a flat, dry,
        # flora-free tile carrying a crop whose non-soil factors are all
        # positive, forced to loam (in both shipped crops' crops.yaml
        # `soils:` lists) with its positive score confirmed against the
        # live engine. Requirement 5: never assert the granite zero
        # against a tile where nothing could grow in the first place.
        fixture, notes = select_positive_fixture(port, "probe")
        for note in notes:
            print(f"  (skipped candidate) {note}")
        if fixture is None:
            print("  [FAIL] no tillable tile in the loaded region carries a "
                  "crop that scores above zero on preferred soil, so the "
                  "granite contrast would prove nothing. Either every "
                  "preferred-soil score regressed to zero, or this seed's "
                  "tillable tiles are all climatically unsuitable — the "
                  "skipped-candidate lines above name which.")
            return 1
        tx, ty, z, crop, rows = fixture
        print(f"  fixture tile ({tx},{ty}), surfaceZ={z}, crop={crop!r} "
              f"(non-soil factors all positive, loam forced)")

        okL_row = suitability_row(port, "probe", tx, ty, crop)
        okL_fits = factor_fits(okL_row)
        okL = (okL_row is not None
               and okL_row["score"] > 0.0
               and okL_fits.get("soil") == 1.0)
        passed &= okL
        print(f"  [{'PASS' if okL else 'FAIL'}] on loam, {crop} scores "
              f"{okL_row['score'] if okL_row else None} > 0 with soil fit "
              f"{okL_fits.get('soil')}")
        if not okL:
            # Requirement 4: the breakdown is what separates "soil
            # gating regressed" from "this tile is climatically
            # unsuitable for this crop" — a zero here with every
            # non-soil fit still positive means the former.
            print(f"      full factor breakdown for {crop}: {okL_row}")

        # --- 1. Suitability query lists every registered crop, with a
        #     6-factor breakdown per crop ---
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
        #     The SAME crop that just scored positive on loam must fall
        #     to zero on granite, soil fit included — one contrast on
        #     one tile, not two independent observations. Both shipped
        #     crops are still required to zero, preserving the coverage
        #     this check had before #1762.
        okG = set_material_and_wait(port, "probe", tx, ty, z,
                                     GRANITE_NAME, crop, 0.0)
        rows_bad = send_json(
            port, f"return world.getPlantSuitability({tx},{ty})")
        by_bad = ({r["name"]: r for r in rows_bad}
                  if isinstance(rows_bad, list) else {})
        row_bad = by_bad.get(crop)
        ok2 = (okG
               and row_bad is not None
               and row_bad["score"] == 0.0
               and factor_fits(row_bad).get("soil") == 0.0
               and all(by_bad.get(n) is not None and by_bad[n]["score"] == 0.0
                       for n in ("wheat", "tomato_plant")))
        passed &= ok2
        print(f"  [{'PASS' if ok2 else 'FAIL'}] granite (non-preferred soil) "
              f"drops {crop} from a positive score to 0.0 with soil fit 0.0, "
              f"and zeroes both shipped crops: {rows_bad}")

        # Restore loam so the rest of this probe (designation checks
        # below) runs against a species-preferred soil, matching the
        # deterministic setup at the top.
        set_material_and_wait(port, "probe", tx, ty, z, LOAM_NAME, crop, 1.0)

        # --- 3. Designation refused on an untilled tile ---
        pre = send_json(port, f"return world.isPlantable({tx},{ty})")
        ok2a = pre is False
        passed &= ok2a
        print(f"  [{'PASS' if ok2a else 'FAIL'}] isPlantable is false before "
              f"tilling: {pre}")

        send(port, f"plant.designate('probe',{tx},{ty},'wheat'); "
                   f"return 'ok'")
        time.sleep(0.5)
        d0 = send_json(port, f"return plant.getDesignationAt('probe',{tx},{ty})")
        ok2 = not isinstance(d0, dict)
        passed &= ok2
        print(f"  [{'PASS' if ok2 else 'FAIL'}] designate refused on an "
              f"untilled tile: {d0}")

        till_and_wait(port, "probe", tx, ty, z)

        # --- 4. Designation refused for an unregistered crop name ---
        send(port, f"plant.designate('probe',{tx},{ty},'not_a_real_crop'); "
                   f"return 'ok'")
        time.sleep(0.5)
        d1 = send_json(port, f"return plant.getDesignationAt('probe',{tx},{ty})")
        ok3 = not isinstance(d1, dict)
        passed &= ok3
        print(f"  [{'PASS' if ok3 else 'FAIL'}] designate refused for an "
              f"unregistered crop name: {d1}")

        # --- 5. Designation succeeds on a tilled tile ---
        send(port, f"plant.designate('probe',{tx},{ty},'wheat'); "
                   f"return 'ok'")
        time.sleep(0.5)
        n = send_json(port, "return plant.getDesignationCount('probe')")
        d2 = send_json(port, f"return plant.getDesignationAt('probe',{tx},{ty})")
        ok4 = (isinstance(n, (int, float)) and n >= 1
               and isinstance(d2, dict) and d2.get("crop") == "wheat"
               and isinstance(d2.get("z"), (int, float)))
        passed &= ok4
        print(f"  [{'PASS' if ok4 else 'FAIL'}] designate marks a tilled "
              f"tile: count={n} at-tile={d2}")

        # plant.nearestDesignation returns MULTIPLE Lua values (gx, gy,
        # dist), not a table — the debug console prints them
        # tab-separated on one line, so parse that directly instead of
        # send_json's JSON path (same multi-return quirk
        # world.getSurfaceAt has, per crop_probe.py's find_dry_tile).
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
        d3 = send_json(port, f"return plant.getDesignationAt('probe',{tx},{ty})")
        ok4c = not isinstance(d3, dict)
        passed &= ok4c
        print(f"  [{'PASS' if ok4c else 'FAIL'}] cancelDesignation clears "
              f"it: {d3}")

        # --- 6. Designating a row_crop works too (designation is
        #     category-symmetric; only execution is #336's asymmetry) ---
        send(port, f"plant.designate('probe',{tx},{ty},'tomato_plant'); "
                   f"return 'ok'")
        time.sleep(0.5)
        d4 = send_json(port, f"return plant.getDesignationAt('probe',{tx},{ty})")
        ok5 = isinstance(d4, dict) and d4.get("crop") == "tomato_plant"
        passed &= ok5
        print(f"  [{'PASS' if ok5 else 'FAIL'}] designate accepts a "
              f"row_crop name too: {d4}")

        # --- 7. Replace semantics: designating again overwrites ---
        send(port, f"plant.designate('probe',{tx},{ty},'wheat'); "
                   f"return 'ok'")
        time.sleep(0.5)
        d5 = send_json(port, f"return plant.getDesignationAt('probe',{tx},{ty})")
        ok6 = isinstance(d5, dict) and d5.get("crop") == "wheat"
        passed &= ok6
        print(f"  [{'PASS' if ok6 else 'FAIL'}] re-designating the same "
              f"tile replaces the crop: {d5}")

        # --- 8. Save/load round-trip ---
        send(port, "engine.saveWorld('probe', 'plant_v78_check'); "
                   "return 'ok'")
        time.sleep(3.0)
        send(port, "engine.loadSave('plant_v78_check'); return 'ok'")
        published, load_status = wait_load_published(port, 200)
        if not published:
            print(f"  [FAIL] load transaction did not publish: {load_status}")
            return 1
        send(port, "world.show('probe'); return 'ok'")
        send(port, "engine.setPaused(false); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)
        d6 = send_json(port,
                       f"return plant.getDesignationAt('probe',{tx},{ty})")
        ok7 = isinstance(d6, dict) and d6.get("crop") == "wheat"
        passed &= ok7
        print(f"  [{'PASS' if ok7 else 'FAIL'}] designation (with crop) "
              f"survives save/load: {d6}")

        print("\n" + ("ALL PLANT CHECKS PASSED" if passed else "SOME FAILED"))
        return 0 if passed else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    sys.exit(main())
