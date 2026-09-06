#!/usr/bin/env python3
"""Tree-felling / chop-designation probe (#97).

Boots a headless engine on a real generated world (tree placement needs
worldgen; the arena has no flora), then checks:

  0. Fixture: the probe registers its OWN wood-tagged tree species,
     `probe_chop_pine`, after the shipped data/flora and before
     world.init (#2058), so every stage below has a choppable subject
     on ANY seed instead of waiting for worldgen to happen to drop an
     oak, birch or maple inside the scanned region — which seed 42's
     default fixture does not. See PROBE_CHOP_YAML for why its
     placement is certain rather than merely likely, and why it still
     leaves the bare construction tile stage 5 needs. Setup then
     positively verifies REGISTRATION (the loader's own count, via
     probelib.load_fixture_yaml) and PLACEMENT (a tile whose sole
     plant is the fixture, with a live build spot beside it) before
     anything is graded; either one missing is a loud setup failure
     that grades nothing (#1342), never a chop-pipeline verdict.
  1. API: world.findHarvestableFlora(..., 'wood') locates the fixture;
     world.getFloraAt reports it with a 'wood' tag and no live
     regrowth timer (its 'harvestable' flag is the FORAGE signal,
     gated on the #332 growth window — the chop path keys on
     tags + the species' authored `ungated_tags:` exemption (#2212)
     + regrowthRemaining instead); a BARE findHarvestableFlora
     (the foraging AI's food search) does NOT return the tree tile —
     wood yields are inedible, so the #97 tag split must keep hungry
     units from felling oaks for dinner.
  2. Designation: chop.designateInstances commits an exact set of
     plant identities (chop.getDesignationAt / getDesignationCount);
     chop.cancelDesignation removes exactly one BY ID, leaving a
     co-tenant of the same tile standing. #1856 made the PLAYER's
     gesture a screen-space press-drag, which needs a camera this
     probe has not got — the exact-ID authority underneath it is
     the same one the gesture reaches, and is what is proved here.
  3. Save/load: designations survive save → loadSave with their z
     (WorldPageSave wpsChopDesignations, v67).
  4. AI: an acolyte with an axe autonomously claims the designated
     tree (real unit_ai stack), walks over, chops it, wood_log ground
     items appear, the designation clears, and the tile flips to
     regrowing (long timer).
  5. Construction: the chopped logs satisfy a build job — a dungeon_1
     POST (materials: wood_log, #96 build: block) designated beside
     the stump gets built by the same acolyte, consuming a log off
     the ground (the #97 acceptance that wood is a usable build
     material). Posts need a floor under them (findConstructJob's
     floorZAt guard), so a steel floor goes down first. The tile is
     the one stage 0 reserved, inside the same box this stage has
     always searched.

Usage: python3 tools/chop_probe.py [--port 9177] [--seed 42]
       [--size 64] [--plates 3]

       python3 tools/chop_probe.py --fail-fixture register|place
         The NEGATIVE CONTROL for stage 0 (#2058): deliberately breaks
         the fixture's registration (an undeclared ungated tag the
         loader must reject) or its placement (a temperature window no
         terrain reaches), and must exit nonzero having printed a
         SETUP FAILURE and NO [PASS]/[FAIL] grade at all. Requirement 2
         is otherwise unobservable from a passing run.
"""
import argparse, glob, os, shutil, socket, subprocess, sys, tempfile, time
from pathlib import Path
from probelib import (FixtureNotRegistered, clear_find_water, quit_engine,
                      boot, load_fixture_yaml, send, send_json,
                      wait_load_published)

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


# --------------------------------------------------------------------
# The probe's OWN choppable tree (#2058)
# --------------------------------------------------------------------
# Registered after the shipped data/flora and before world.init, which
# is what makes it visible to worldgen at all. Since #2241 a placement
# roll is salted from the species' own authored NAME, so adding this
# fixture cannot move a shipped species' roll whatever order it
# registers in — what it CAN still do is occupy a tile a shipped
# species would otherwise have taken, which is deliberate cross-species
# competition, not a defect.
#
# Its whole job is to make "is there a choppable tree in the loaded
# region?" a property of the PROBE rather than of the seed: the default
# seed 42 / size 64 / 3 plates fixture supplies none, which is why
# every stage below used to be unreachable. Field by field:
#
#   * `tags: [wood]` + `ungated_tags: [wood]` — the #97 chop key and
#     the #2212 growth-window exemption the three shipped tree species
#     author. `floraHarvestAdmits` takes the tagged path when either
#     the tag is ungated OR the plant is in its harvest window, so this
#     tree is findable by world.findHarvestableFlora(..., 'wood') and
#     fellable by world.harvestFloraInstance(..., 'wood') on any
#     calendar day, which is precisely the authority stage 1 asserts.
#   * `yield: wood_log` — a SHIPPED item (data/items/wood_log.yaml) and an
#     INEDIBLE one, which is load-bearing twice over. Stage 1's bare
#     (food) findHarvestableFlora only counts species whose yield holds
#     an edible item, so an edible yield would break the #97 tag split
#     this probe exists to prove; and stage 5 builds a dungeon_1 post
#     out of exactly this item, so any other yield would leave the
#     construction phase with nothing to consume.
#   * `lifecycle: evergreen` — World.Flora.Growth.instanceLifespan
#     returns Nothing for an evergreen, so no instance ever wraps into
#     the 60-day dead window and stage 4's felled/regrowing assertions
#     read one plant's own timer rather than a generational wrap.
#   * one `matured` phase at age 0 — findActivePhase picks the
#     highest-age phase whose lpAge <= age, and this is the only one,
#     so every instance presents `matured` at every age and inherits
#     the block's own yield roll. A shipped tree authors
#     `phase_yield: {sprout: []}` because a freshly-rolled placement age
#     can land on its sprout phase and a felled sprout must drop
#     nothing; with no sprout phase to reach, this fixture cannot roll
#     a log-less fell into stage 4.
#   * NO annualCycle — nothing to poke world.setDate for.
#   * ranges far outside anything terrain produces, and `maxSlope: 15`
#     (the whole slope bitmask) — speciesFitnessDetail hard-kills a
#     species when ANY factor's asymBell hits 0, which happens at or
#     past a range endpoint. Endpoints this far out keep every factor
#     strictly positive on every land tile.
#   * `density: 1000.0` — placement is `roll < density * fitness` with
#     roll in [0,1). This is the field that turns "likely" into
#     "certain": with fitness held well above 0.001 by the ranges
#     above, the product exceeds 1 and the strict `<` can never fail.
#   * `footprint: 64` — and this is the field that keeps that certainty
#     from paving the region. World.Flora.Placement marks a placed
#     tile's neighbourhood occupied out to
#     `ceiling(footprint/32) - 1` tiles, and an occupied tile is skipped
#     before any species rolls on it, so footprint 64 reserves a
#     one-tile apron of GUARANTEED flora-free ground around every
#     fixture tree. Copying the foraging fixture's footprint 0 would
#     place on every eligible tile and leave stage 5 — which rejects
#     any tile where world.getFloraAt succeeds — with nowhere to build.
#   * `category: tree` — instanceCount is exactly 1 for a tree, so a
#     fixture tile holds ONE plant. Stage 2's cancel-by-id assertion
#     ("the tile has no designation left") reads a tile with a single
#     designated plant, and stage 2's co-tenant assertion gets its
#     second plant from a neighbouring fixture tree instead.
#   * `regrowth_time: 345600` — four game-days, what white_oak uses.
#     Stage 4 requires the felled tile to carry a LIVE timer at time
#     scale 1, and stage 5 then keeps building on a stump that has not
#     grown back underneath it.
#
# Placement stays subject to the tile being land at all (worldgen skips
# fluid, barren material and unset columns), which no species can opt
# out of; a region with no eligible tile is reported as the fixture
# failure it is, never as advice to try another seed.
PROBE_CHOP_YAML = """flora:
  - name: probe_chop_pine
    type: evergreen_tree
    texDir: "assets/textures/flora/white_oak"
    lifecycle: evergreen
    phases:
      - {tag: matured, texture: "matured.png", age: 0}
    harvestable:
      tags: [wood]
      ungated_tags: [wood]
      yield:
        - id: wood_log
          count: [3, 6]
      regrowth_time: 345600
      harvested_texture: "dead.png"
    worldGen:
      category: tree
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
      footprint: 64
"""

# The two negative controls (#2058 requirement 2). Each breaks ONE of
# the two things stage 0 verifies, so a run under --fail-fixture must
# stop at setup with a nonzero exit and no grade printed. Without them
# a green default run says nothing about the fail-loud path.
#
#   register: `ungated_tags` names a tag `tags:` does not declare, which
#     Engine.Asset.YamlFlora's requireUngatedTags rejects — the whole
#     file fails to parse, engine.loadFloraYaml registers 0, and
#     probelib.load_fixture_yaml raises FixtureNotRegistered.
#   place: a temperature window no terrain reaches. The YAML is valid
#     and registers, so registration passes and PLACEMENT is what
#     fails — speciesFitnessDetail hard-kills the species on every
#     tile, and the region holds no fixture tree to find.
PROBE_CHOP_UNREGISTERABLE_YAML = PROBE_CHOP_YAML.replace(
    "ungated_tags: [wood]", "ungated_tags: [wodo]")
PROBE_CHOP_UNPLACEABLE_YAML = (PROBE_CHOP_YAML
                               .replace("minTemp: -200", "minTemp: 900")
                               .replace("maxTemp: 200", "maxTemp: 901")
                               .replace("idealTemp: 0", "idealTemp: 900"))

FIXTURE_YAML = {
    None: PROBE_CHOP_YAML,
    "register": PROBE_CHOP_UNREGISTERABLE_YAML,
    "place": PROBE_CHOP_UNPLACEABLE_YAML,
}

PROBE_SPECIES = "probe_chop_pine"
PROBE_YIELD = "wood_log"

# The tile span the probe's own `loadChunksInRegion(-4,-4,4,4)` covers:
# chunkSize is 16, so chunks -4..4 are global tiles -64..79 on both
# axes. The inset keeps every tile stages 2-5 reach — the co-tenant
# box, the acolyte's spawn, stage 5's search — inside those loaded
# chunks, so an unloaded tile can never read as "nothing here".
SCAN_LO, SCAN_HI = -64, 79
SCAN_INSET = 8

# Stage 5 searches r=2..5 around the stump; stage 0 reserves its build
# tile inside the innermost of those boxes, so the tile it verified is
# one this stage was always entitled to use.
BUILD_RADIUS = 2
# Where the woodcutter is spawned, relative to the tree. Kept out of the
# build-tile search so the two never name the same tile.
SPAWN_DX = 2
# How far stage 2 will look for a SECOND fixture tree to leave standing.
# The footprint apron above puts the nearest one at Chebyshev 2 or more,
# so the 3x3 the cancel-by-id assertion uses can no longer supply it.
CO_TENANT_RADIUS = 4


def bootstrap(port, fixture_path, fixture_mode=None):
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
    # failures are reported separately for exactly that reason.
    with open(fixture_path, "w") as f:
        f.write(FIXTURE_YAML[fixture_mode])
    load_fixture_yaml(port, "engine.loadFloraYaml", fixture_path)


def setup_failure(detail):
    """The one report for a stage-0 failure, and the only exit that is
    allowed to be silent about the chop pipeline.

    Nothing downstream of a fixture that did not place can pass, so the
    probe stops HERE and grades nothing (#1342) rather than letting a
    fixture gap arrive as a chop-pipeline verdict — which is exactly
    how #2058's missing tree used to read. Never advice to re-roll the
    seed: placement is certain on every eligible land tile (see
    PROBE_CHOP_YAML), so reaching this means the region holds no
    eligible land at all, or the placement/harvest contract the fixture
    is built on has changed."""
    print(f"\nSETUP FAILURE: {detail}")
    return 1


def find_fixture_target(port, inset=SCAN_INSET):
    """Locate the one tile every stage below works, and the build tile
    stage 5 needs, in a single engine-side scan.

    A tile qualifies only when ALL of this holds, which is what makes
    the target the PROBE's own fixture rather than whatever worldgen
    happened to leave nearby:

      * `world.getFloraAt` reports the fixture species, a 'wood' tag and
        no live regrowth timer. getFloraAt names the instance a bare
        harvest would take, so requiring the fixture here is what stops
        an unrelated co-located plant answering for the tile.
      * `world.getFloraGrowthAt` reports EXACTLY ONE plant on it. Stage
        2 cancels one designation by id and then requires the tile to
        hold none, which a second designated co-tenant would defeat;
        and stage 1's bare (food) search must not resolve to this tile,
        which an edible plant sharing it would defeat.
      * a build tile exists within BUILD_RADIUS that is flat, dry and
        flora-free — the same three conditions stage 5 tests, checked
        while there is still a setup failure to report it as. The
        fixture's footprint apron is what guarantees the flora-free
        part; slope and fluid belong to the terrain.

    A second fixture tree within CO_TENANT_RADIUS is reported when one
    is there (stage 2's co-tenant assertion designates it) and is
    optional: its absence narrows that one assertion, it does not make
    the fixture unusable.

    Returns a dict of ints, or None when the region holds no such tile.
    """
    lo, hi = SCAN_LO + inset, SCAN_HI - inset
    lua = (
        "local S='" + PROBE_SPECIES + "' "
        "local function tagged(f) for _,t in ipairs(f.tags or {}) do "
        "if t=='wood' then return true end end return false end "
        "local function sole(x,y) local g=world.getFloraGrowthAt(x,y) "
        "if not g or #g~=1 then return false end return g[1].id==S end "
        "local function spot(x,y) "
        "for dx=-" + str(BUILD_RADIUS) + "," + str(BUILD_RADIUS) + " do "
        "for dy=-" + str(BUILD_RADIUS) + "," + str(BUILD_RADIUS) + " do "
        "if not (dx==0 and dy==0) and not (dx==" + str(SPAWN_DX)
        + " and dy==0) then local bx,by=x+dx,y+dy "
        "if world.getSlopeAt(bx,by)==0 and not world.getFluidAt(bx,by) "
        "and not world.getFloraAt(bx,by) then return bx,by end end "
        "end end return nil end "
        "local function mate(x,y) "
        "for r=2," + str(CO_TENANT_RADIUS) + " do for dx=-r,r do "
        "for dy=-r,r do "
        "if math.max(math.abs(dx),math.abs(dy))==r then "
        "local f=world.getFloraAt(x+dx,y+dy) "
        "if f and f.id==S and f.instanceId then return x+dx,y+dy end "
        "end end end end return nil end "
        "for gx=" + str(lo) + "," + str(hi) + " do "
        "for gy=" + str(lo) + "," + str(hi) + " do "
        "local f=world.getFloraAt(gx,gy) "
        "if f and f.id==S and (f.regrowthRemaining or 0)<=0 and tagged(f) "
        "and sole(gx,gy) then local bx,by=spot(gx,gy) "
        "if bx then local mx,my=mate(gx,gy) "
        "return {gx=gx,gy=gy,bx=bx,by=by,mx=mx,my=my} end end "
        "end end return nil")
    r = send_json(port, lua, timeout=180.0)
    if not isinstance(r, dict) or "gx" not in r:
        return None
    out = {k: int(r[k]) for k in ("gx", "gy", "bx", "by")}
    if "mx" in r and "my" in r:
        out["mx"], out["my"] = int(r["mx"]), int(r["my"])
    return out


def count_logs_near(port, gx, gy, radius=4):
    ground = send_json(port, "return item.listGround()")
    if not isinstance(ground, list):
        return 0
    return sum(1 for g in ground
               if g.get("defName") == PROBE_YIELD
               and abs(g.get("x", 1e9) - gx) <= radius
               and abs(g.get("y", 1e9) - gy) <= radius)



# #1856 retired the tile-rectangle `chop.designate`: the player's gesture
# is a screen-space press-drag, so designation crosses the queue as an
# EXACT list of plant identities. A headless probe has no camera to
# project through, so it builds that list the way any exact-ID caller
# does — `world.getFloraAt` reports each tile's plant id — and hands it
# to the same authority the gesture reaches. This is deliberately not a
# tile-keyed runtime path: the ids are what is designated, and the tile
# walk is only how this probe chooses them.
# The Lua table constructor is written `{{}}` because this template is
# consumed through `str.format`, which reads a bare `{}` as a positional
# field. Nothing reached the call before #2058 gave the probe a fixture,
# so the `IndexError` that shape raises had never been thrown.
_COLLECT_IDS = (
    "local ids = {{}} "
    "for x = {x1}, {x2} do for y = {y1}, {y2} do "
    "  local f = world.getFloraAt(x, y) "
    "  if f and f.instanceId then ids[#ids+1] = f.instanceId end "
    "end end "
)


def designate_rect(port, page, x1, y1, x2, y2, send_fn):
    """Designate every plant the given tile span holds, by exact id."""
    send_fn(port,
            _COLLECT_IDS.format(x1=x1, y1=y1, x2=x2, y2=y2)
            + f"return chop.designateInstances('{page}', ids)")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9177)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--size", type=int, default=64)
    ap.add_argument("--plates", type=int, default=3)
    ap.add_argument("--fail-fixture", choices=("register", "place"),
                    default=None,
                    help="negative control (#2058): break the fixture's "
                         "registration or its placement and require a "
                         "setup-classified nonzero exit that grades nothing")
    args = ap.parse_args()
    port = args.port
    passed = True

    tmpdir = tempfile.mkdtemp(prefix="chop_probe_")
    try:
        root = make_isolated_root(tmpdir)
        # The fixture YAML lives in THIS invocation's own tree (beside
        # the resource root rather than inside it — the engine only ever
        # reads it through the absolute path passed to the loader), so
        # the rmtree below takes it away on every exit path, including a
        # FixtureNotRegistered raised out of `bootstrap`.
        fixture_path = os.path.join(tmpdir, "probe_chop_flora.yaml")
        proc = boot(port, f"{SPROOT}/chop_probe_engine.log",
                    args=["--resource-root", root])
        return _run(port, proc, args, passed, fixture_path)
    finally:
        shutil.rmtree(tmpdir, ignore_errors=True)


def _run(port, proc, args, passed, fixture_path):
    try:
        bootstrap(port, fixture_path, args.fail_fixture)
        send(port, f"world.init('probe', {args.seed}, {args.size}, "
                   f"{args.plates}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, "world.show('probe'); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)

        # --- 0. The fixture placed, and stage 5 has somewhere to build ---
        target = find_fixture_target(port)
        if target is None:
            return setup_failure(
                f"no usable '{PROBE_SPECIES}' tile in the loaded region "
                f"(chunks -4..4 = global tiles {SCAN_LO}..{SCAN_HI}, inset "
                f"by {SCAN_INSET}). The probe's own fixture registered but "
                f"did not place a sole, wood-tagged, un-regrowing instance "
                f"with a flat, dry, flora-free tile within {BUILD_RADIUS} "
                f"of it, so nothing below can be graded.")
        tx, ty = target["gx"], target["gy"]
        bx, by = target["bx"], target["by"]
        mate = ((target["mx"], target["my"])
                if "mx" in target and "my" in target else None)
        print(f"  fixture target: {PROBE_SPECIES} at ({tx},{ty}); build "
              f"tile ({bx},{by}); co-tenant tree "
              f"{mate if mate else 'none within ' + str(CO_TENANT_RADIUS)}")

        # --- 1. Tag reporting + forage-tag guard ---
        found = send_json(port,
                          f"return world.findHarvestableFlora({tx},{ty},1,'wood')")
        ok0 = isinstance(found, dict) and found.get("gx") == tx \
              and found.get("gy") == ty and found.get("id") == PROBE_SPECIES
        passed &= ok0
        print(f"  [{'PASS' if ok0 else 'FAIL'}] the wood search finds the "
              f"fixture at ({tx},{ty}): {found}")

        fl = send_json(port, f"return world.getFloraAt({tx},{ty})")
        ok1 = isinstance(fl, dict) and fl.get("id") == PROBE_SPECIES \
              and "wood" in (fl.get("tags") or []) \
              and fl.get("regrowthRemaining", -1) == 0
        passed &= ok1
        print(f"  [{'PASS' if ok1 else 'FAIL'}] getFloraAt reports a "
              f"choppable tree (wood tag, no regrowth timer): "
              f"{PROBE_SPECIES} at ({tx},{ty}) → {fl}")

        bare = send_json(port,
                         f"return world.findHarvestableFlora({tx},{ty},2)")
        ok1b = not (isinstance(bare, dict)
                    and bare.get("gx") == tx and bare.get("gy") == ty)
        passed &= ok1b
        print(f"  [{'PASS' if ok1b else 'FAIL'}] bare (food) search skips "
              f"the tree tile: {bare}")

        # --- 2. Designate / query / cancel ---
        designate_rect(port, "probe", tx - 1, ty - 1, tx + 1, ty + 1, send)
        time.sleep(0.5)
        n = send_json(port, "return chop.getDesignationCount('probe')")
        d = send_json(port, f"return chop.getDesignationAt('probe',{tx},{ty})")
        ok2 = isinstance(n, (int, float)) and n >= 1 \
              and isinstance(d, dict) and isinstance(d.get("z"), (int, float))
        passed &= ok2
        print(f"  [{'PASS' if ok2 else 'FAIL'}] designate marks the tree: "
              f"count={n} at-tile={d}")

        # #1856: cancel the EXACT plant, by the id getDesignationAt just
        # reported. Calling cancelDesignation WITHOUT an id reaches the
        # tile-wide fallback that exists for restored jobs and legacy
        # migration, which would clear every designation standing there
        # and so could not tell a working exact-identity cancel from a
        # working tile sweep. Stage 0 established that this tile carries
        # exactly one plant, so "the tile has no designation left" is a
        # statement about the cancel rather than about the tile's census.
        cancel_iid = d.get("instanceId") if isinstance(d, dict) else None
        ok2b = isinstance(cancel_iid, (int, float)) \
               and not isinstance(cancel_iid, bool)
        if ok2b:
            send(port, f"chop.cancelDesignation({tx},{ty},{int(cancel_iid)}); "
                       f"return 'ok'")
            time.sleep(0.5)
            d2 = send_json(port,
                           f"return chop.getDesignationAt('probe',{tx},{ty})")
            ok2b = not isinstance(d2, dict)
        else:
            d2 = f"no instanceId to cancel by: {d}"
        passed &= ok2b
        print(f"  [{'PASS' if ok2b else 'FAIL'}] cancelDesignation clears "
              f"exactly the named plant: {d2}")

        # And it clears ONLY that plant. Designate two plants — a tile
        # key alone cannot tell two designations apart (#1854) — cancel
        # one by id, and require the other to survive. The fixture's
        # footprint apron (see PROBE_CHOP_YAML) ordinarily leaves the
        # 3x3 above holding one tree — the occupancy map is per-chunk,
        # so a chunk edge can still stand two side by side — which is
        # why the second plant comes from the neighbouring fixture tree
        # stage 0 found rather than from that box. Widening the 3x3 to
        # reach it keeps this box a SUPERSET of the one the cancel-by-id
        # assertion used, so the erase below has nothing outside it to
        # miss.
        cx1, cy1, cx2, cy2 = tx - 1, ty - 1, tx + 1, ty + 1
        if mate:
            cx1, cy1 = min(cx1, mate[0]), min(cy1, mate[1])
            cx2, cy2 = max(cx2, mate[0]), max(cy2, mate[1])
        pair = send_json(port,
                         _COLLECT_IDS.format(x1=cx1, y1=cy1, x2=cx2, y2=cy2)
                         + "chop.designateInstances('probe', ids) return ids")
        time.sleep(0.5)
        ids = [int(i) for i in pair if isinstance(i, (int, float))] \
              if isinstance(pair, list) else []
        if len(ids) >= 2:
            before = send_json(port, "return chop.getDesignationCount('probe')")
            send(port, f"chop.cancelDesignation(0,0,{ids[0]}); return 'ok'")
            time.sleep(0.5)
            after = send_json(port, "return chop.getDesignationCount('probe')")
            ok2c = (isinstance(before, (int, float))
                    and isinstance(after, (int, float))
                    and after == before - 1)
            detail = f"{before} -> {after}"
        else:
            # Not a contract failure: the fixture's own spacing left no
            # second plant inside the widened box to leave standing.
            ok2c = True
            detail = f"skipped, only {len(ids)} designatable plant(s) nearby"
        passed &= ok2c
        print(f"  [{'PASS' if ok2c else 'FAIL'}] an exact-id cancel leaves "
              f"co-tenants designated: {detail}")
        # Leave the page as this stage found it: the save and AI phases
        # below re-designate deliberately and count what they get.
        if ids:
            send(port, "chop.eraseInstances('probe', {"
                       + ",".join(str(i) for i in ids) + "}); return 'ok'")
            time.sleep(0.5)
        left = send_json(port, "return chop.getDesignationCount('probe')")
        ok2d = left == 0
        passed &= ok2d
        print(f"  [{'PASS' if ok2d else 'FAIL'}] eraseInstances clears the "
              f"rest: count={left}")

        # Re-designate for the save + AI phases.
        designate_rect(port, "probe", tx, ty, tx, ty, send)
        time.sleep(0.5)

        # --- 3. Save/load round-trip ---
        send(port, "engine.saveWorld('probe', 'chop_v67_check'); "
                   "return 'ok'")
        time.sleep(3.0)
        send(port, "engine.loadSave('chop_v67_check'); return 'ok'")
        published, load_status = wait_load_published(port, 200)
        if not published:
            print(f"  [FAIL] load transaction did not publish: {load_status}")
            return 1
        send(port, "world.show('probe'); return 'ok'")
        send(port, "engine.setPaused(false); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)
        d3 = send_json(port, f"return chop.getDesignationAt('probe',{tx},{ty})")
        ok3 = isinstance(d3, dict) and isinstance(d3.get("z"), (int, float))
        passed &= ok3
        print(f"  [{'PASS' if ok3 else 'FAIL'}] designation survives "
              f"save/load: {d3}")

        # --- 4. Autonomous felling (real AI stack) ---
        send(port, "engine.loadScript('scripts/unit_stats.lua', 0.1); "
                   "return 'ok'")
        send(port, "engine.loadScript('scripts/unit_resources.lua', 0.2); "
                   "return 'ok'")
        send(port, "engine.loadScript('scripts/unit_ai.lua', 0.1); "
                   "return 'ok'")
        uid_s = send(port, f"local u=unit.spawn('acolyte',{tx + SPAWN_DX},"
                           f"{ty}); return u")
        try:
            uid = int(float(uid_s.strip('"')))
        except ValueError:
            uid = -1
        if uid < 0:
            print(f"  [FAIL] could not spawn woodcutter: {uid_s}")
            return 1
        time.sleep(2.0)
        # Acolytes spawn at/over carrying capacity with a full toolkit
        # (pick, shovel, axe, radio, rations, canteen) plus worn kit
        # that unit.getCarryingWeight counts too. Shed everything but
        # the axe so hauling a wood_log for the phase-5 build job can't
        # brush carrying_capacity — the capacity STAT is a per-spawn
        # roll (observed 17.9–24.2 kg), and the fetch capacity gate
        # refusing the log reads as a stuck job (same shedding the
        # construction probe does).
        for it in ("pick_steel", "shovel_steel", "rations", "rations",
                   "canteen_steel_2l", "radio"):
            send(port, f"unit.removeItem({uid},'{it}'); return 'ok'")
        # carrying_capacity is a per-spawn body-composition roll
        # (observed 12.3–24.2 kg); on a weak roll even a bare unit in
        # worn kit can't shoulder an 8 kg log and the phase-5 haul
        # stalls at the fetch capacity gate. Pin the fixture: bulk the
        # woodcutter up and recompute the derived stats — the probe
        # tests chopping/hauling/building, not the physique lottery.
        cap = send(port,
                   f"local lean=unit.getStat({uid},'lean_mass'); "
                   f"local body=unit.getStat({uid},'body_mass'); "
                   f"if lean and body then "
                   f"unit.setStat({uid},'lean_mass', lean*1.6); "
                   f"unit.setStat({uid},'body_mass', body+lean*0.6); "
                   f"unit.recomputeBody({uid}) end; "
                   f"return unit.getStat({uid},'carrying_capacity') or -1")
        try:
            cap_ok = float(cap) >= 18.0
        except ValueError:
            cap_ok = False
        if not cap_ok:
            print(f"  [FAIL] could not pin carrying capacity "
                  f"(got {cap})")
            passed = False
        has_axe = send(port,
                       f"for _,it in ipairs(unit.getInventory({uid}) or {{}})"
                       f" do if it.defName=='axe_steel' then return 'yes' end"
                       f" end; return 'no'").strip('"')
        print(f"  [{'PASS' if has_axe == 'yes' else 'FAIL'}] woodcutter "
              f"carries an axe: {has_axe}")
        passed &= has_axe == "yes"
        # Fresh acolytes spawn with the standing "find_water" goal
        # (DEFAULT_GOALS), whose search floor (~3.0) outranks menial
        # work (#306 bands) — and the water-search spiral happily walks
        # scouts off cliffs on unlucky seeds. The probe tests CHOPPING,
        # not hydration scouting, so mark the goal accomplished through
        # the canonical goal API before the spiral leads it anywhere.
        if clear_find_water(port, uid):
            send(port, f"unit.stop({uid})", expect_result=False)
        else:
            print("  [FAIL] could not quiet find_water goal")
            passed = False

        deadline = time.time() + 90.0
        felled = logs = regrowing = False
        while time.time() < deadline:
            time.sleep(2.0)
            d4 = send_json(port,
                           f"return chop.getDesignationAt('probe',{tx},{ty})")
            if not isinstance(d4, dict):
                felled = True
            if count_logs_near(port, tx, ty) >= 1:
                logs = True
            fl4 = send_json(port, f"return world.getFloraAt({tx},{ty})")
            if isinstance(fl4, dict) and not fl4.get("harvestable") \
               and fl4.get("regrowthRemaining", 0) > 0:
                regrowing = True
            if felled and logs and regrowing:
                break
        ok4 = felled and logs and regrowing
        passed &= ok4
        print(f"  [{'PASS' if ok4 else 'FAIL'}] acolyte fells the tree "
              f"autonomously: designation_cleared={felled} "
              f"logs_on_ground={logs} tile_regrowing={regrowing}")
        if not ok4:
            print("\nSOME FAILED")
            return 1

        # --- 5. Chopped logs satisfy construction (#96 wiring) ---
        # A dungeon_1 POST costs 1 wood_log (its build: block). The
        # flat, dry, flora-free tile is the one stage 0 reserved inside
        # this stage's own r=2 box — the fixture's footprint apron is
        # what guarantees the region has one at all (#2058). It is
        # re-read here rather than trusted: nothing should have changed
        # it, and a changed one is a real finding rather than a reason
        # to go looking for another tile.
        px, py = bx, by
        still = send(port,
                     f"if world.getSlopeAt({px},{py})==0 "
                     f"and not world.getFluidAt({px},{py}) "
                     f"and not world.getFloraAt({px},{py}) "
                     f"and not chop.getDesignationAt('probe',{px},{py}) "
                     f"then return 'yes' end return 'no'").strip('"')
        if still != "yes":
            print(f"  [FAIL] the build tile ({px},{py}) stage 0 reserved is "
                  f"no longer flat, dry, flora-free and undesignated")
            print("\nSOME FAILED")
            return 1
        # Posts need a floor under them (findConstructJob skips floorless
        # posts), so the acolyte first lays a steel floor from inventory,
        # then builds the post from a felled log it hauls off the ground.
        send(port, f"unit.addItem({uid},'steel_plate',0); return 'ok'")
        send(port, f"construction.designate('probe',{px},{py},{px},{py},"
                   f"'structure','dungeon_1','floor'); return 'ok'")
        deadline = time.time() + 90.0
        floored = False
        while time.time() < deadline:
            time.sleep(2.0)
            if send(port,
                    f"return structure.hasAt({px},{py},'floor')") == "true":
                floored = True
                break
        if not floored:
            print(f"  [FAIL] prerequisite floor never built at ({px},{py})")
            print("\nSOME FAILED")
            return 1

        logs_before = count_logs_near(port, tx, ty, radius=8)
        send(port, f"construction.designate('probe',{px},{py},{px},{py},"
                   f"'structure','dungeon_1','post'); return 'ok'")
        time.sleep(0.5)
        deadline = time.time() + 120.0
        built = cleared = False
        while time.time() < deadline:
            time.sleep(2.0)
            # The AI-placed post defaults to corner "n" and is stored
            # under the composite kind (like wall_ne).
            built = send(port, f"return structure.hasAt({px},{py},"
                               f"'post_n')") == "true"
            cleared = not isinstance(
                send_json(port, f"return construction.getDesignationAt("
                                f"'probe',{px},{py})"), dict)
            if built and cleared:
                break
        logs_after = count_logs_near(port, tx, ty, radius=8)
        consumed = logs_after < logs_before
        ok5 = built and cleared and consumed
        passed &= ok5
        print(f"  [{'PASS' if ok5 else 'FAIL'}] post built from chopped "
              f"logs at ({px},{py}): built={built} designation_cleared="
              f"{cleared} log_consumed={consumed} "
              f"(ground logs {logs_before}→{logs_after})")

        print("\n" + ("ALL CHOP CHECKS PASSED" if passed else "SOME FAILED"))
        return 0 if passed else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    # A fixture the engine loader REFUSED is a setup failure, distinct
    # from a registered fixture that placed nothing (`setup_failure`).
    # `main`'s own `finally` has already shut the engine down and removed
    # this run's tree by the time it lands here (#1342).
    try:
        sys.exit(main())
    except FixtureNotRegistered as exc:
        print(f"\n{exc}")
        sys.exit(1)
