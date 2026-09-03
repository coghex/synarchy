#!/usr/bin/env python3
"""Foraging / interactive-flora probe (#94).

Boots a headless engine on a real generated world (flora placement needs
worldgen; the arena has no plants), then checks:

  0. Fixture: the probe registers its OWN harvestable food species,
     `probe_forage_berry`, before world.init (#1766), so every stage
     below has a subject on ANY seed instead of waiting for worldgen to
     happen to drop a raspberry or a clover inside the scanned region.
     See PROBE_FORAGE_YAML for why its placement is certain rather than
     merely likely.
  1. API: world.findHarvestableFlora locates a harvestable tile in the
     loaded region; world.getFloraAt reports it harvestable.
  2. Harvest: world.harvestFlora spawns yield ground items, flips the
     tile to harvestable=false with a live regrowthRemaining timer, and
     a second harvest is refused (nil).
  3. Regrowth: at a cranked time scale the timer counts down and the
     tile returns to harvestable.
  4. Save/load: a fresh harvest's regrowth timer survives
     save → loadSave (world-page map wpsFloraHarvests, v66). The engine
     runs on a throwaway resource root under a slot named for this
     invocation, and both halves of the round trip are tied to their own
     request ids, so the reloaded tile can only be this run's save and
     the developer's saves/ is never read, written or rotated (#1618).
  5. AI: an acolyte with an empty stomach and no carried food forages a
     nearby plant autonomously (real unit_ai stack, not neutralised)
     and ends up with food eaten or in hand.

Stages 1-4 all work one deterministic fixture tile; stage 5 works a
SECOND one, far enough away that stage 4's still-running regrowth timer
is nowhere near it (the reloaded world cannot re-harvest the tile it
just persisted a live timer for).

Usage: python3 tools/foraging_probe.py [--port 9173] [--seed 42]
       [--size 64] [--plates 3]
"""
import argparse, glob, os, shutil, socket, stat, subprocess, sys
import tempfile, time, uuid
from probelib import (FixtureNotRegistered, boot, capture_request_id,
                      load_fixture_yaml, quit_engine, send, send_json,
                      wait_load_published, wait_save_complete)

SPROOT = "/tmp"
REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def _make_owner_writable(top: str) -> None:
    """Add owner write (and directory search) permission throughout a
    freshly copied tree.

    `shutil.copytree` reproduces the SOURCE's mode bits, so a checkout
    whose `config/` is read-only -- a CI cache restored read-only, a
    read-only mount, an archive unpacked without write bits -- yields a
    private `config/` this run cannot use and cannot delete: unlinking a
    child needs owner write+search on its parent directory, so
    `remove_run_root` would report residue and leave the invocation's
    whole tree, engine log and save slot behind after a run that did
    nothing wrong (#1912). The copy is THIS invocation's, so it is made
    writable regardless of what the source happened to be; the source
    itself is never touched, and a symlink is skipped rather than
    followed, so the content families it names keep their own modes.
    Same treatment `tools/flora_growth_probe.py` and the four location
    probes give their own copies.
    """
    for path, dirs, files in os.walk(top):
        for name in [None, *dirs, *files]:
            target = path if name is None else os.path.join(path, name)
            try:
                mode = os.lstat(target).st_mode
                if stat.S_ISLNK(mode):
                    continue
                extra = stat.S_IRWXU if stat.S_ISDIR(mode) \
                    else stat.S_IRUSR | stat.S_IWUSR
                os.chmod(target, stat.S_IMODE(mode) | extra)
            except OSError:
                # Best effort: a mode this process cannot change is
                # reported by the cleanup that actually trips over it,
                # with the path it failed on, rather than here.
                pass


def make_isolated_root(base: str) -> str:
    """A throwaway resource root for one invocation (#1618): the
    read-only content families symlinked, `config/` COPIED without the
    developer's `*.local.yaml` overrides, and its OWN empty `saves/`.

    `app/App/ResourceRoot.hs` chdirs the engine into this directory and
    `World.Save.Serialize` resolves `saves` relative to it, so stage 4's
    round trip writes here instead of the developer's live `saves/` —
    which is gitignored and therefore accumulates abandoned slots
    silently. Copying `config/` rather than symlinking it keeps a
    personal `config/save.local.yaml` out of the run: `scripts/init.lua`
    loads the autosave scheduler, so a local autosave interval could
    otherwise fire a competing save while stage 3 has the clock cranked
    to 3000x and rotate slots underneath the probe.

    Only the SAVE slot moves here; `bootstrap` still feeds the engine
    repo-relative `data/**.yaml` paths, which resolve through the
    symlinked `data/` exactly as before, and the engine log stays under
    `SPROOT`.
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
        _make_owner_writable(config_dst)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def remove_run_root(base: str) -> bool:
    """Delete this invocation's own throwaway tree, save artifacts and
    all, and say whether it is really gone.

    Only ever removes the directory THIS process made with
    `tempfile.mkdtemp`, so nothing pre-existing is at risk — in
    particular the abandoned slots already sitting in the developer's
    `saves/` are neither read nor touched. `rmtree` unlinks the
    symlinked content families rather than recursing into them, so the
    real `scripts/`, `assets/` and `data/` are never followed. A
    survivor makes the run non-zero: a green result sitting beside
    leftover saves is precisely the outcome this isolation exists to
    prevent, so it must not be reported as a pass.
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
    requests (#1618).

    `engine.saveWorld` and `engine.loadSave` only ACCEPT synchronously
    (`src/Engine/Scripting/Lua/API/Save.hs`), so neither return value
    means the work finished and no fixed sleep can stand in for one.
    Each half therefore asserts acceptance, captures that request's own
    id, and waits for a terminal status carrying it. A missing id is
    itself a failure rather than something to wait past: without one the
    wait falls back to accepting whichever terminal status it sees
    first, which is the stale-status hole the request ids exist to
    close. A terminal FAILED save returns here without issuing the load,
    so a rejected or broken save is never mistaken for a round trip.

    Returns None on success, or a message naming the step that broke.
    """
    saved = send(port, f"return engine.saveWorld('{page}', '{slot}')")
    if saved.strip() != "true":
        return (f"engine.saveWorld('{page}', '{slot}') was not accepted "
                f"(got {saved!r}); the engine log carries the validation "
                f"reason, which the Boolean itself does not")
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


# The probe's OWN harvestable food species (#1766). Registered after the
# shipped data/flora and before world.init, which is what makes it
# visible to worldgen at all. Since #2241 a placement roll is salted
# from the species' own authored NAME, so adding this fixture cannot
# move a shipped species' roll whatever order it registers in — what it
# CAN still do is occupy a tile a shipped species would otherwise have
# taken, which is deliberate cross-species competition, not a defect.
#
# Its whole job is to make "is there a harvestable food plant in the
# loaded region?" a property of the PROBE rather than of the seed. Every
# field below that differs from a shipped species is doing exactly that:
#
#   * `lifecycle: evergreen` — World.Flora.Growth.instanceLifespan
#     returns Nothing for an evergreen, so no instance ever wraps into
#     the 60-day dead window. `harvestOpen` can never see fgDead.
#   * one `matured` phase at age 0 — findActivePhase picks the
#     highest-age phase whose lpAge <= age, and this is the only one, so
#     every instance presents `matured` at every age. That keeps
#     `harvestOpen`'s phaseBlocked (sprout/seedling/withering/dead)
#     unreachable, which a sprout-at-0 / matured-at-N species like
#     white_clover cannot promise for a freshly-rolled placement age.
#   * NO annualCycle — `harvestOpen` gates on a `fruiting` stage only
#     when the species authors one, so a species with no cycle at all is
#     open year-round on any calendar day. The probe therefore never
#     has to poke world.setDate to reach its own subject. (Shipped
#     precedent: scots_pine, white_spruce, coconut_palm, red_mangrove.)
#   * ranges far outside anything terrain produces, and `maxSlope: 15`
#     (the whole slope bitmask) — speciesFitnessDetail hard-kills a
#     species when ANY factor's asymBell hits 0, which happens at or
#     past a range endpoint. Endpoints this far out keep every factor
#     strictly positive on every land tile.
#   * `density: 1000.0` — placement is `roll < density * fitness` with
#     roll in [0,1). This is the field that turns "likely" into
#     "certain": with fitness held well above 0.001 by the ranges above,
#     the product exceeds 1 and the strict `<` can never fail. A
#     density of 1.0 would leave placement at the mercy of the roll,
#     which is the seed dependency this fixture exists to remove.
#   * `category: shrub` — instanceCount is exactly 1 for a shrub, so the
#     guaranteed-everywhere placement costs one instance per tile rather
#     than the 2-3 a wildflower would add.
#   * `yield: wild_berries` — a SHIPPED edible item
#     (data/items/foraged_food.yaml). world.findHarvestableFlora's bare
#     (food) call only counts species whose yield contains an edible
#     item, so an inedible yield would make the fixture invisible to
#     both the query under test and the foraging AI in stage 5.
#   * `regrowth_time: 43200` — half a game-day, the same figure real
#     white_clover uses. Long enough that stage 4's timer is still
#     running across the save/load round trip at time scale 1, short
#     enough that stage 3's cranked clock finishes it in the 3 real
#     seconds it waits.
#
# Placement stays subject to the tile being land at all (worldgen skips
# fluid, barren material and unset columns), which no species can opt
# out of; a region with no eligible tile is reported as the fixture
# failure it is, never as advice to try another seed.
PROBE_FORAGE_YAML = """flora:
  - name: probe_forage_berry
    type: deciduous_shrub
    texDir: "assets/textures/flora/red_raspberry"
    lifecycle: evergreen
    phases:
      - {tag: matured, texture: "matured.png", age: 0}
    harvestable:
      tags: [fruit]
      yield:
        - id: wild_berries
          count: [1, 3]
      regrowth_time: 43200
      harvested_texture: "matured_senescing.png"
    worldGen:
      category: shrub
      minTemp: -200
      maxTemp: 200
      idealTemp: 0
      minPrecip: -10.0
      maxPrecip: 20.0
      idealPrecip: 5.0
      minAlt: -20000
      maxAlt: 20000
      idealAlt: 0
      minHumidity: -10.0
      maxHumidity: 10.0
      idealHumidity: 0.0
      maxSlope: 15
      density: 1000.0
      footprint: 0
"""

PROBE_SPECIES = "probe_forage_berry"
PROBE_YIELD = "wild_berries"

# The tile span the probe's own `loadChunksInRegion(-4,-4,4,4)` covers:
# chunkSize is 16, so chunks -4..4 are global tiles -64..79 on both axes.
SCAN_LO, SCAN_HI = -64, 79

# Stage 5 watches a box this many tiles either side of its target, and
# keeps that target at least STAGE5_SEP away from stage 1-4's tile. The
# separation is deliberately larger than the radius, so the timer stage 4
# persisted sits outside the box no matter which corner of it the forager
# works.
STAGE5_RADIUS = 8
STAGE5_SEP = 20


def bootstrap(port, fixture_path):
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
    # The probe's own species, LAST and before world.init: worldgen reads
    # the catalog when it generates a chunk, so a fixture registered
    # afterwards would place nothing. `load_fixture_yaml` reads the
    # loader's count and raises FixtureNotRegistered on a rejection
    # (#1342), which is the difference between "the engine refused this
    # YAML" and "the YAML registered but nothing placed" — the two
    # failures below are reported separately for exactly that reason.
    with open(fixture_path, "w") as f:
        f.write(PROBE_FORAGE_YAML)
    load_fixture_yaml(port, "engine.loadFloraYaml", fixture_path)


def find_fixture_tiles(port, want=1, sep=0, avoid=None, inset=0):
    """Locate up to `want` tiles in the loaded region whose bare harvest
    resolves to the probe's own species.

    `world.getFloraAt` reports the instance a BARE `world.harvestFlora`
    would take: both walk the tile's instances in `floraAt` order and
    stop at the first one with a harvest block whose growth window is
    open (Forage/Query.hs's `filter open harvestables`, Forage/
    Harvest.hs's `listToMaybe`). So requiring the reported `id` to be
    the fixture is what establishes that a harvest here yields the
    fixture's food rather than an unrelated natural plant that happens
    to share the tile — a tree standing on the same tile is harvestable
    year-round for `wood` and would otherwise win the pick.

    Tiles are returned at least `sep` apart (Chebyshev) from each other
    and from every tile in `avoid`, so a caller can ask for a target
    that no earlier stage's regrowth timer reaches. `inset` shrinks the
    searched span on every side, which is how stage 5 guarantees its
    whole watch box is inside the chunks the probe actually loaded — a
    box hanging off the edge would read an unloaded tile as "nothing
    regrowing here" and silently narrow the assertion. Returns a list of
    (gx, gy).
    """
    lo, hi = SCAN_LO + inset, SCAN_HI - inset
    skip = " and ".join(
        f"math.max(math.abs({ax}-gx),math.abs({ay}-gy))>={sep}"
        for ax, ay in (avoid or [])) or "true"
    lua = (
        f"local function scan() local out={{}} "
        f"for gx={lo},{hi} do for gy={lo},{hi} do "
        f"local f=world.getFloraAt(gx,gy) "
        f"if f and f.id=='{PROBE_SPECIES}' and f.harvestable==true "
        f"and (f.regrowthRemaining or 0)<=0 and ({skip}) then "
        f"local ok=true for _,p in ipairs(out) do "
        f"if math.max(math.abs(p[1]-gx),math.abs(p[2]-gy))<{sep} "
        f"then ok=false end end "
        f"if ok then out[#out+1]={{gx,gy}} "
        f"if #out>={want} then return out end end end "
        f"end end return out end "
        f"local o=scan() local t={{}} "
        f"for _,p in ipairs(o) do t[#t+1]=p[1]..','..p[2] end "
        f"return table.concat(t,';')")
    raw = send(port, lua, timeout=120.0).strip('"')
    if not raw:
        return []
    return [tuple(int(v) for v in pair.split(",")) for pair in raw.split(";")]


def regrowing_tiles(port, gx, gy, radius):
    """How many tiles in the (2*radius+1)^2 box around (gx,gy) carry a
    live regrowth timer — i.e. how many have been harvested recently.

    Stage 5 watches this rather than one nominated tile: the fixture
    places on every eligible tile, so a forager standing beside its
    target legitimately picks whichever neighbour its own search ranks
    first. The assertion is that an autonomous harvest HAPPENED near the
    unit, which is the behaviour under test; pinning it to one tile
    would only be asserting the AI's tie-break."""
    raw = send(
        port,
        f"local n=0 for x={gx - radius},{gx + radius} do "
        f"for y={gy - radius},{gy + radius} do "
        f"local f=world.getFloraAt(x,y) "
        f"if f and (f.regrowthRemaining or 0)>0 then n=n+1 end "
        f"end end return n",
        timeout=60.0)
    try:
        return int(float(raw.strip('"')))
    except (TypeError, ValueError):
        return -1


def fixture_missing(where):
    """The one report for "the fixture registered but placed nothing
    here". Never advice to re-roll the seed: placement is certain on
    every eligible land tile (see PROBE_FORAGE_YAML), so reaching this
    means either the region holds no eligible land at all or the
    placement/growth contract the fixture is built on has changed."""
    print(f"  [FAIL] no harvestable '{PROBE_SPECIES}' tile {where} — the "
          f"probe's own fixture registered but did not place a "
          f"harvest-open instance in the searched span (chunks -4..4 = "
          f"global tiles {SCAN_LO}..{SCAN_HI}, inset by "
          f"{STAGE5_RADIUS}); this is a fixture-placement regression in "
          f"the probe, not a property of --seed")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9173)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    args = ap.parse_args()
    port = args.port
    passed = True

    # This invocation owns its resource root and therefore its saves/
    # (#1618): stage 4 writes a slot that no ordinary `cabal run` can
    # reach, and the whole tree goes away below.
    base = tempfile.mkdtemp(prefix="synarchy_foraging_")

    # The guard starts HERE, one statement after that directory exists
    # (#1791), because everything between this point and the cleanup
    # below can fail with invocation-owned state already on disk.
    # `make_isolated_root` stages incrementally — the root, three
    # symlinks, a copied `config/`, `saves/` — so a permission, source
    # or disk-space failure part-way through leaves a partial tree that
    # nothing outside this guard would remove. `boot` is inside for the
    # same reason: it exits the probe outright when the engine dies
    # before READY or never prints it, and that failure path would
    # otherwise leave this run's root — the one thing the cleanup below
    # exists to remove — sitting in the temp directory.
    #
    # A None handle is what `quit_engine` already expects when there is
    # no live process to shut down, and it is initialised BEFORE the
    # try, so a staging failure — which happens before any boot — sends
    # no `engine.quit()` at an engine that is somebody else's.
    proc = None
    rc = 1
    try:
        root = make_isolated_root(base)
        # Unique per invocation as well as per root, so the slot NAME alone
        # identifies this run even in a log shared with another.
        slot = f"foraging_v66_check_{uuid.uuid4().hex[:8]}"
        print(f"isolated resource root: {root}", flush=True)
        print(f"save slot: {slot}", flush=True)

        # The fixture YAML lives in THIS invocation's own tree (beside
        # the resource root rather than inside it — the engine only ever
        # reads it through the absolute path passed to the loader), so
        # `remove_run_root` below takes it away on every exit path,
        # including a FixtureNotRegistered raised out of `bootstrap`.
        fixture_path = os.path.join(base, "probe_forage_flora.yaml")

        proc = boot(port, f"{SPROOT}/foraging_probe_engine.log",
                    args=["--resource-root", root])
        bootstrap(port, fixture_path)
        send(port, f"world.init('probe', {args.seed}, {args.size}, {args.plates}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, "world.show('probe'); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)

        # --- 1. Find + query ---
        # Two targets, far enough apart that stage 4's persisted timer
        # (on the first) is nowhere near stage 5's forager (beside the
        # second). Asking for both HERE is also the check that the
        # fixture really did place more than one harvest-open instance.
        targets = find_fixture_tiles(port, want=2, sep=STAGE5_SEP,
                                     inset=STAGE5_RADIUS)
        if len(targets) < 2:
            fixture_missing(f"in the loaded region (wanted 2 at least "
                            f"{STAGE5_SEP} tiles apart, found "
                            f"{len(targets)})")
            return 1
        (gx, gy) = targets[0]
        print(f"  fixture targets: {targets[0]} (stages 1-4) and "
              f"{targets[1]} (a second, >= {STAGE5_SEP} tiles away)")
        # The query API under test, anchored on the fixture tile: radius
        # 1 leaves the tile itself the only candidate at distance 0, so
        # a correct findHarvestableFlora can only answer with it.
        near = send_json(port, f"return world.findHarvestableFlora({gx},{gy},1)")
        ok1a = isinstance(near, dict) and near.get("gx") == gx \
               and near.get("gy") == gy and near.get("id") == PROBE_SPECIES
        passed &= ok1a
        print(f"  [{'PASS' if ok1a else 'FAIL'}] findHarvestableFlora finds the "
              f"fixture at ({gx},{gy}): {near}")
        fl = send_json(port, f"return world.getFloraAt({gx},{gy})")
        # `id` is asserted, not just reported: getFloraAt names the
        # instance a bare harvestFlora would take, so this is what makes
        # stage 2 a harvest OF THE FIXTURE rather than of whatever else
        # shares the tile.
        ok1 = isinstance(fl, dict) and fl.get("id") == PROBE_SPECIES \
              and fl.get("harvestable") is True \
              and fl.get("regrowthRemaining", -1) == 0
        passed &= ok1
        print(f"  [{'PASS' if ok1 else 'FAIL'}] getFloraAt reports harvestable: "
              f"{PROBE_SPECIES} at ({gx},{gy}) → {fl}")

        # --- 2. Harvest: yields spawn, tile flips, re-harvest refused ---
        yields = send_json(port, f"return world.harvestFlora({gx},{gy})")
        # Every spawned item is the one the fixture declares. Paired
        # with the species assertion above (which pins WHICH instance a
        # bare harvest takes), a yield of anything else means the
        # harvest did not come from the plant this stage nominated —
        # which without the check would read as a pass.
        ok2 = isinstance(yields, list) and len(yields) >= 1 \
              and all("gid" in y and y.get("id") == PROBE_YIELD
                      for y in yields)
        fl2 = send_json(port, f"return world.getFloraAt({gx},{gy})")
        ok2b = isinstance(fl2, dict) and fl2.get("harvestable") is False \
               and fl2.get("regrowthRemaining", 0) > 0
        again = send(port, f"return world.harvestFlora({gx},{gy}) and 'yes' or 'nil'")
        ok2c = again.strip('"') == "nil"
        passed &= ok2 and ok2b and ok2c
        print(f"  [{'PASS' if ok2 else 'FAIL'}] harvest spawns ground yields: {yields}")
        print(f"  [{'PASS' if ok2b else 'FAIL'}] tile regrowing after harvest: {fl2}")
        print(f"  [{'PASS' if ok2c else 'FAIL'}] double-harvest refused: {again}")

        # --- 3. Regrowth under cranked clock ---
        # The fixture regrows in 43200 game-seconds (half a game-day);
        # at timeScale 3000 (game-min/real-sec) that's 0.24
        # real-seconds, well inside the 3 waited below.
        send(port, "world.setTimeScale('probe', 3000); return 'ok'")
        time.sleep(3.0)
        send(port, "world.setTimeScale('probe', 1); return 'ok'")
        fl3 = send_json(port, f"return world.getFloraAt({gx},{gy})")
        ok3 = isinstance(fl3, dict) and fl3.get("harvestable") is True
        passed &= ok3
        print(f"  [{'PASS' if ok3 else 'FAIL'}] regrowth completes on the game clock: {fl3}")

        # --- 4. Save/load round-trip of a live timer ---
        yields2 = send_json(port, f"return world.harvestFlora({gx},{gy})")
        if not isinstance(yields2, list):
            print("  [FAIL] re-harvest for the save test failed")
            passed = False
        problem = save_and_reload(port, "probe", slot)
        if problem:
            print(f"  [FAIL] {problem}")
            return 1
        send(port, "world.show('probe'); return 'ok'")
        # A loaded world comes up PAUSED (auto-pause-on-save) with only
        # the center chunk generated — resume the clock and pull in the
        # region around the harvested tile so getFloraAt can see it.
        send(port, "engine.setPaused(false); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)
        fl4 = send_json(port, f"return world.getFloraAt({gx},{gy})")
        ok4 = isinstance(fl4, dict) and fl4.get("harvestable") is False \
              and fl4.get("regrowthRemaining", 0) > 0
        passed &= ok4
        print(f"  [{'PASS' if ok4 else 'FAIL'}] harvest timer survives save/load: {fl4}")

        # --- 5. Autonomous foraging (real AI stack) ---
        send(port, "engine.loadScript('scripts/unit_stats.lua', 0.1); return 'ok'")
        send(port, "engine.loadScript('scripts/unit_resources.lua', 0.2); return 'ok'")
        send(port, "engine.loadScript('scripts/unit_ai.lua', 0.1); return 'ok'")
        # A fresh target (the save-test tile is regrowing): find one on
        # the RELOADED world and spawn a hungry acolyte two tiles away.
        # Re-searched on the reloaded session rather than reusing
        # stage 1's second target, so this stage proves the fixture
        # survived the round trip as a placement and not just as a
        # registration. `avoid` keeps it STAGE5_SEP away from the tile
        # whose live timer the save carried, which is > the watch radius
        # below, so that timer can never be mistaken for this stage's
        # own harvest.
        found2 = find_fixture_tiles(port, want=1, sep=STAGE5_SEP,
                                    avoid=[(gx, gy)], inset=STAGE5_RADIUS)
        if not found2:
            fixture_missing("on the reloaded world")
            return 1
        (fgx, fgy) = found2[0]
        # The watch box must start clean: anything already regrowing
        # inside it would make the after-reading below unattributable.
        before = regrowing_tiles(port, fgx, fgy, STAGE5_RADIUS)
        if before != 0:
            print(f"  [FAIL] {(2 * STAGE5_RADIUS + 1) ** 2}-tile watch box "
                  f"around ({fgx},{fgy}) already had {before} regrowing "
                  f"tile(s) before the forager spawned")
            return 1
        uid_s = send(port, f"local u=unit.spawn('acolyte',{fgx + 2},{fgy}); return u")
        uid = int(float(uid_s.strip('"')))
        if uid < 0:
            print("  [FAIL] could not spawn forager")
            return 1
        time.sleep(2.0)
        # Hungry, carrying nothing edible: strip rations, empty the
        # stomach, halve the store (need ≈ 0.6 → forage well above
        # wander; nothing else competes on a fresh spawn).
        send(port, f"local u={uid}; unit.removeItem(u,'rations'); "
                   f"unit.removeItem(u,'rations'); "
                   f"unit.setStat(u,'hunger',0); "
                   f"unit.setStat(u,'calories',unit.getStat(u,'max_calories')*0.5); "
                   f"return 'ok'")
        deadline = time.time() + 45.0
        foraged = eaten = False
        harvested = 0
        while time.time() < deadline:
            time.sleep(2.0)
            # Kept as the high-water mark, not the latest reading: a
            # poll that failed to parse (-1) must not overwrite an
            # observation the report is about to print.
            harvested = max(harvested,
                            regrowing_tiles(port, fgx, fgy, STAGE5_RADIUS))
            if harvested > 0:
                foraged = True
            st = float(send(port, f"return unit.getStat({uid},'hunger') or -1"))
            inv_food = send(port,
                f"local inv=unit.getInventory({uid}) or {{}}; "
                f"for _,it in ipairs(inv) do if it.food then return 'yes' end end; "
                f"return 'no'").strip('"')
            if st > 10 or inv_food == "yes":
                eaten = True
            if foraged and eaten:
                break
        ok5 = foraged and eaten
        passed &= ok5
        print(f"  [{'PASS' if ok5 else 'FAIL'}] hungry acolyte forages autonomously: "
              f"tiles_harvested={harvested} food_acquired={eaten}")

        print("\n" + ("ALL FORAGING CHECKS PASSED" if passed else "SOME FAILED"))
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
    # A fixture the engine loader REFUSED is a setup failure, distinct
    # from a registered fixture that placed nothing (`fixture_missing`).
    # `main`'s own `finally` has already shut the engine down and removed
    # this run's tree by the time it lands here (#1342).
    try:
        sys.exit(main())
    except FixtureNotRegistered as exc:
        print(f"\n{exc}")
        sys.exit(1)
