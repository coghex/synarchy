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
     #94's yield path — clearing the plot and producing wheat_grain,
     growing farming XP further. The yield is judged by IDENTITY, not
     by where it lies at one instant (#1760): `YieldTrail` below is
     armed before the clock jumps and follows the exact yield off the
     ground and into whoever picked it up, so a harvest whose own
     collecting phase (or the needs ladder) removed the grain still
     passes. Reaching the harvestable state is satisfied either by
     sampling it or by the AI having already harvested the plot, on
     causally specific evidence.
 10. Re-designating a tile the AI has already claimed with a DIFFERENT
     crop makes it plant the NEW crop, not the stale one it originally
     walked over for (plant.designate replaces in place, HM.insert).
     The simulation is paused the moment the worker's claim is seen, the
     swap is made and read back as COMMITTED while the wheat job is still
     unfinished, and the check completes on the tomato INSTANCE appearing
     after the resume — never on the designation vanishing (#2937).
 11. Save/load: the AI-planted row-crop instances (WePlaceFlora, save
     v79) survive save → loadSave as the SAME instance. With the
     simulation paused, each is read right before the save (by instanceId);
     after the load the session resumes, its chunks are admitted and each is
     read again. A failed query is reported as a failure,
     never as an absent plant (#2937). The engine runs on a throwaway
     resource root,
     so that slot lands in this run's own saves/ and is deleted with
     it — the developer's saves/ is never read, written or rotated
     (#1616).

The six sites (A, B, E near the origin; C, D, F far away) are a FIXED,
deterministic fixture, not a search over natural terrain (#2938): before
the first check the probe raises two small plateaus with ordinary
`world.addTile` edits (see `SITES`, `NEAR_PATCH`, `FAR_PATCH`) and
verifies, by reading them back, that every patch tile is flat, dry and
flora-free at one uniform height. The till, plant, grow and auto-harvest
chain below is still the real acolyte AI; only the ground it works on is
authored. Elapsed seconds are printed per phase (`PhaseClock`).

Usage: python3 tools/farm_ai_probe.py [--port 9336] [--seed 42]
       [--size 64] [--plates 3]
"""
import argparse, glob, json, os, shutil, socket, stat, subprocess, sys, tempfile, \
       time, uuid
import probe_engine
import probe_evidence
from probelib import (boot, capture_request_id, clear_find_water, quit_engine,
                      send, send_json, wait_load_published, wait_save_complete)

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
    otherwise fire a competing save partway through this probe's
    long run and rotate slots underneath it.
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
    `tempfile.mkdtemp`, so nothing pre-existing is at risk; `rmtree`
    unlinks the symlinked content families rather than recursing into
    them, so the real `scripts/`, `assets/` and `data/` are never
    followed. A survivor makes the run non-zero: a green result sitting
    beside leftover saves is precisely the outcome this isolation
    exists to prevent, so it must not be reported as a pass.
    """
    try:
        probe_evidence.retire(base)
    except OSError as exc:
        print(f"  [FAIL] could not remove this run's resource root "
              f"{base}: {exc}")
        return False
    if os.path.exists(base):
        print(f"  [FAIL] this run's resource root survived removal: {base}")
        return False
    return True


def save_and_reload(port, page, slot, mark=None):
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
    if mark:
        mark("save")

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
    if mark:
        mark("load_publish")
    return None


def bootstrap(port):
    for pattern, fn in [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/infections/*.yaml", "engine.loadInfectionYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/flora/*.yaml",      "engine.loadFloraYaml"),
        ("data/factions/*.yaml", "engine.loadFactionYaml"),
        ("data/physiology/*.yaml", "engine.loadPhysiologyYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
    ]:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")


# --- The fixed tillable fixture (#2938) -------------------------------
#
# This probe used to FIND six tillable sites by scanning natural terrain
# over the console (step 4 over a 33 x 33 window, up to three queries per
# point), which cost about 546 of a 660 s run on one measured seed and
# scaled with however few flat dry tiles the world happened to place near
# the origin. The sites are now authored: two plateaus raised with the
# ordinary player edit `world.addTile` (`WeAddTile`), whose replay
# persists them through save/load exactly like the AI's plantings.
#
# `WeAddTile` raises a column's terrain top by ONE z, gives the new top
# cell slope 0 (flat), removes any flora rooted on the tile and displaces
# any fluid the fill reaches (`World.Edit.Apply`). Raising every tile of a
# patch to ONE height above the highest surface under it (fluid
# included) therefore yields flat, dry, flora-free ground that is also
# one contiguous walkable level. There is no fallback to a natural-terrain
# search: a patch that does not read back as specified fails the run with
# a diagnostic, because a silently different fixture would change what
# the checks mean.
PAGE = "probe"
PATCH_MATERIAL = "loam"
# Extra z above the highest existing surface under a patch; one is enough
# to touch every tile, the second clears a fluid ceiling that sits above
# the rendered surface.
PATCH_MARGIN = 2

#: (x0, y0, x1, y1), inclusive tile bounds. Both lie inside the chunks
#: `world.loadChunksInRegion(-4, -4, 4, 4)` loads (tiles -64..79).
NEAR_PATCH = (0, 0, 10, 10)
FAR_PATCH = (-62, -62, -52, -52)

#: Site -> tile. A (groundcover, full AI loop), B (row crop, AI-planted)
#: and E (mid-job re-designation) are on the near patch where the worker
#: operates; C (direct-primitive checks), D (rot timeline) and F (category
#: field) are on the far patch, beyond the worker's harvest scan range, so
#: the acolyte working A/B/E can never find them and confound the
#: "ignored crop rots" check.
SITES = {
    "A": (2, 2), "B": (8, 2), "E": (2, 8),
    "C": (-60, -60), "D": (-54, -60), "F": (-60, -54),
}
NEAR_SITES = ("A", "B", "E")
FAR_SITES = ("C", "D", "F")

#: Where the worker is spawned: `spawn_worker(ax + 2, ay)`.
WORKER_SPAWN = (SITES["A"][0] + 2, SITES["A"][1])

#: `till_scan_range` / `plant_scan_range` and `harvest_scan_range` in
#: scripts/unit_ai_tunables.lua.
WORK_SCAN_RANGE = 30.0
HARVEST_SCAN_RANGE = 24.0
LOADED_TILES = (-64, 79)


def _in_rect(rect, xy):
    x0, y0, x1, y1 = rect
    return x0 <= xy[0] <= x1 and y0 <= xy[1] <= y1


def _dist_to_rect(rect, xy):
    x0, y0, x1, y1 = rect
    dx = max(x0 - xy[0], 0, xy[0] - x1)
    dy = max(y0 - xy[1], 0, xy[1] - y1)
    return (dx * dx + dy * dy) ** 0.5


def validate_layout(sites=None, near=NEAR_PATCH, far=FAR_PATCH,
                    spawn=WORKER_SPAWN):
    """Geometry the fixture promises, checked without an engine.

    Returns a list of problems (empty when sound): six distinct sites;
    near sites on the near patch and within the worker's work-search range
    of its spawn; far sites on the far patch and farther than the harvest
    scan range from every tile of the near patch and from the spawn; both
    patches inside the tiles the probe loads.
    """
    sites = SITES if sites is None else sites
    problems = []
    if len(set(sites.values())) != len(sites):
        problems.append("sites are not all distinct tiles")
    lo, hi = LOADED_TILES
    for name, rect in (("near", near), ("far", far)):
        if min(rect) < lo or max(rect) > hi:
            problems.append(f"{name} patch {rect} leaves the loaded "
                            f"tiles {lo}..{hi}")
    if not _in_rect(near, spawn):
        problems.append(f"worker spawn {spawn} is off the near patch")
    for name in NEAR_SITES:
        xy = sites[name]
        if not _in_rect(near, xy):
            problems.append(f"site {name} {xy} is off the near patch")
        d = ((xy[0] - spawn[0]) ** 2 + (xy[1] - spawn[1]) ** 2) ** 0.5
        if d >= WORK_SCAN_RANGE:
            problems.append(f"site {name} is {d:.1f} tiles from the worker, "
                            f"outside its {WORK_SCAN_RANGE:.0f}-tile work "
                            f"search")
    for name in FAR_SITES:
        xy = sites[name]
        if not _in_rect(far, xy):
            problems.append(f"site {name} {xy} is off the far patch")
        for what, d in (("near patch", _dist_to_rect(near, xy)),
                        ("worker spawn",
                         ((xy[0] - spawn[0]) ** 2
                          + (xy[1] - spawn[1]) ** 2) ** 0.5)):
            if d <= HARVEST_SCAN_RANGE:
                problems.append(f"site {name} is {d:.1f} tiles from the "
                                f"{what}, inside the {HARVEST_SCAN_RANGE:.0f}"
                                f"-tile harvest scan")
    return problems


def build_flat_patch(port, rect, timeout=90.0):
    """Raise every tile of `rect` to one height and wait for it to land.

    Returns `(target_z, None)` on success, or `(None, diagnostic)`.
    `world.addTile` only ENQUEUES its edit, so success is judged by
    reading every tile back (terrain top and rendered surface equal to the
    target, slope 0, no fluid, no flora), never by the call returning.
    """
    x0, y0, x1, y1 = rect
    plan = send_json(port, (
        f"local top=nil local bad=0 "
        f"for x={x0},{x1} do for y={y0},{y1} do "
        f"local s=world.getTerrainAt(x,y,'{PAGE}') "
        f"if s==nil then return {{unloaded=x..','..y}} end "
        f"if top==nil or s>top then top=s end end end "
        f"local target=top+{PATCH_MARGIN} local n=0 "
        f"for x={x0},{x1} do for y={y0},{y1} do "
        f"local s,t=world.getTerrainAt(x,y,'{PAGE}') "
        f"for i=t+1,target do "
        f"if world.addTile('{PAGE}',x,y,'{PATCH_MATERIAL}') then n=n+1 "
        f"else bad=bad+1 end end end end "
        f"return {{target=target,edits=n,refused=bad}}"), timeout=60.0)
    if not isinstance(plan, dict) or "target" not in plan:
        return None, f"patch {rect} could not be planned: {plan!r}"
    if plan.get("refused"):
        return None, (f"patch {rect}: world.addTile refused "
                      f"{plan['refused']} edit(s) for material "
                      f"'{PATCH_MATERIAL}'")
    target = plan["target"]
    total = (x1 - x0 + 1) * (y1 - y0 + 1)
    check = (f"local ok=0 local first=nil "
             f"for x={x0},{x1} do for y={y0},{y1} do "
             f"local s,t=world.getTerrainAt(x,y,'{PAGE}') "
             f"local sl=world.getSlopeAt(x,y) local f=world.getFluidAt(x,y) "
             f"local fl=world.getFloraAt(x,y) "
             f"if t=={target} and s=={target} and sl==0 and f==nil "
             f"and fl==nil then ok=ok+1 elseif first==nil then "
             f"first={{x=x,y=y,surface=s,terrain=t,slope=sl,fluid=f,"
             f"flora=(fl~=nil)}} end end end "
             f"return {{ok=ok,first_bad=first}}")
    deadline = time.monotonic() + timeout
    state = None
    while time.monotonic() < deadline:
        state = send_json(port, check, timeout=30.0)
        if isinstance(state, dict) and state.get("ok") == total:
            return target, None
        time.sleep(0.5)
    return None, (f"patch {rect} did not settle to a flat, dry, flora-free "
                  f"level at z={target} within {timeout:.0f}s "
                  f"({plan['edits']} edits queued): {state!r}")


def build_fixture(port):
    """Build both plateaus and verify all six sites; `(ok, message)`."""
    problems = validate_layout()
    if problems:
        return False, "fixture layout is unsound: " + "; ".join(problems)
    heights = {}
    for label, rect in (("near", NEAR_PATCH), ("far", FAR_PATCH)):
        z, why = build_flat_patch(port, rect)
        if z is None:
            return False, why
        heights[label] = z
        print(f"  fixture: {label} patch {rect} flat, dry and flora-free "
              f"at z={z}")
    for name, (gx, gy) in SITES.items():
        want = heights["near" if name in NEAR_SITES else "far"]
        st = send_json(port, f"local s,t=world.getTerrainAt({gx},{gy},"
                             f"'{PAGE}') return {{s=s,t=t}}")
        s = st.get("s") if isinstance(st, dict) else None
        t = st.get("t") if isinstance(st, dict) else None
        slope = send_json(port, f"return world.getSlopeAt({gx},{gy})")
        fluid = send_json(port, f"local t=world.getFluidAt({gx},{gy}) "
                                f"return t")
        flora = send_json(port, f"return world.getFloraAt({gx},{gy})")
        tilled = send_json(port, f"return world.isPlantable({gx},{gy})")
        if not (s == want and t == want and slope == 0
                and not fluid and not isinstance(flora, dict)
                and tilled is not True):
            return False, (f"site {name} ({gx},{gy}) is not a flat, dry, "
                           f"flora-free, untilled tile at z={want}: "
                           f"surface={s} terrain={t} slope={slope!r} "
                           f"fluid={fluid!r} flora={flora!r} "
                           f"plantable={tilled!r}")
    return True, "all six sites verified"


class PhaseClock:
    """Monotonic per-phase elapsed seconds (#2938).

    `mark(name)` closes the phase that began at the previous mark (or at
    construction) and prints it, so a long run reports as it goes and the
    table at the end needs no stamping wrapper. `time.monotonic` never
    jumps with the wall clock.
    """

    def __init__(self, now=time.monotonic):
        self._now = now
        self.start = self.last = now()
        self.rows = []

    def mark(self, name):
        t = self._now()
        self.rows.append((name, t - self.last))
        self.last = t
        print(f"  [phase] {name}: {self.rows[-1][1]:.1f}s "
              f"(t+{t - self.start:.1f}s)", flush=True)

    def table(self):
        total = self._now() - self.start
        width = max([len(n) for n, _ in self.rows] + [len("total")])
        lines = ["", "PHASE TIMES (seconds)"]
        lines += [f"  {n:<{width}}  {d:8.1f}" for n, d in self.rows]
        lines.append(f"  {'total':<{width}}  {total:8.1f}")
        return "\n".join(lines)


def till_and_wait(port, page, gx, gy, z):
    """world.setVegAt is a queued world command — send, then poll
    isPlantable until it lands before designating/planting."""
    send(port, f"world.setVegAt('{page}', {gx}, {gy}, {z}, 77); return 'ok'")
    for _ in range(20):
        if send_json(port, f"return world.isPlantable({gx},{gy})") is True:
            return True
        time.sleep(0.2)
    sys.exit(f"setVegAt({gx},{gy}) never landed")


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


def growth_entries(port, gx, gy, species):
    t = send_json(port, f"return world.getFloraGrowthAt({gx},{gy})")
    if isinstance(t, list):
        return [e for e in t if e.get("id") == species]
    return []


def growth_read(port, gx, gy, species):
    """`growth_entries` that keeps a failed query apart from an absent plant.

    `world.getFloraGrowthAt` answers nil when the tile has no flora (or its
    chunk is not loaded) and a list otherwise; anything else is the query
    itself failing (a console error string, a closed connection). Returns
    `(entries, raw_failure)`: `raw_failure` is None for a real answer (a list,
    or nil meaning "no flora here") and the unexpected raw value otherwise, so
    a check can report "the read failed" instead of "the crop is gone" (#2937).
    """
    raw = send(port, f"return world.getFloraGrowthAt({gx},{gy})")
    try:
        t = json.loads(raw) if raw else raw
    except (ValueError, TypeError):
        return [], raw
    if isinstance(t, list):
        return [e for e in t if e.get("id") == species], None
    if raw and t is None:
        return [], None          # the literal `null`: nothing grows here
    return [], raw               # empty reply, or text that is not an answer


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
    if not clear_find_water(port, uid):
        return -1
    send(port, f"unit.stop({uid})", expect_result=False)
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
        spot = send_json(port, f"return world.findHarvestableFlora({cx},{cy},{radius})")
        if not isinstance(spot, dict):
            return cleared
        if keep and (spot.get("gx"), spot.get("gy")) == tuple(keep):
            return cleared
        send_json(port, f"return world.harvestFlora({spot['gx']},{spot['gy']})")
        cleared.append((spot.get("gx"), spot.get("gy"), spot.get("id")))
    return cleared


# --- The harvested yield's observation trail (#1760) -----------------
#
# The auto-harvest capstone used to be scored by catching `wheat_grain`
# lying on the ground at one instant. That instant is an INTERMEDIATE of
# the very action the check waits for: `scripts/unit_ai_harvest.lua`
# runs the harvest as TWO phases of one action -- `world.harvestFlora`
# spawns the yield and clears the plot, and the `collecting` phase then
# pulls it in one item per tick -- so the probe broke its wait on the
# FIRST phase's own effect and then sampled a ground the SECOND phase
# was actively emptying. Since #1743 made that collecting phase
# reachable under ordinary arbitration the emptying reliably completes,
# and the needs ladder can eat the grain besides (`data/items/
# wheat_grain.yaml` declares it as 80-calorie food), so the old oracle
# failed precisely on the runs where everything worked.
#
# What is durable here is not a LOCATION but an observation trail the
# probe owns. `world.harvestFlora` reports page-local ground ids
# (`gid`), not the item's process-unique `instanceId` (#67), and a gid
# stops resolving the moment the item leaves the ground -- so the trail
# is built by WATCHING, not by one lookup after the fact:
#
#   * armed BEFORE the time scale goes up, so a harvest completed inside
#     the first polling interval is still recorded, and so nothing
#     already lying about (or already carried) can be credited to it;
#   * scoped to the crop plot's OWN tile, which `spawnYields`
#     (`Engine.Scripting.Lua.API.Forage.Harvest`) drops yields onto at
#     `gx + 0.5 +/- 0.3`, so `clear_wild_forage`'s sweep yields -- which
#     land on the wild plants' tiles -- can never satisfy it;
#   * every candidate id resolved through `item.getGroundForUnit(uid,
#     gid)`, the worker-OWNING-page contract (#1666). The active-page
#     `item.listGround()` is used only to DISCOVER candidate ids, never
#     as evidence: identical numeric gids name different items on
#     different pages, so a row only enters the trail once the
#     owning-page query has confirmed it;
#   * followed off the ground and into the carrier by `instanceId`,
#     which `item.getGroundForUnit` and `unit.getInventory` spell
#     identically.
#
# Only "never produced" fails the check. A yield that was picked up,
# carried, hauled or eaten was still harvested (#1760 requirement 1).
HARVEST_YIELD_DEF = "wheat_grain"

# Where the yield got to, MOST-ADVANCED state first: a carried instance
# outranks one that merely left the ground, which outranks one still
# lying there. Only YIELD_NOT_PRODUCED is a failure.
YIELD_CARRIED = "carried"
YIELD_MOVED = "moved-after-pickup"
YIELD_ON_GROUND = "on-ground"
YIELD_NOT_PRODUCED = "not-produced"


def _rows(value):
    """A Lua array field as a Python list.

    `Engine.Scripting.Lua.API.Shell.luaTableToJson` emits an EMPTY Lua
    table as the JSON OBJECT `{}` -- its array test needs at least one
    key -- so every list-shaped field arrives as either a list or an
    empty dict. A caller that only tested `isinstance(v, list)` would
    read "nothing observed yet" as a decode failure.
    """
    return value if isinstance(value, list) else []


def _ids(value):
    """The integer ids in a Lua array field, ignoring anything else."""
    out = []
    for entry in _rows(value):
        if isinstance(entry, bool) or not isinstance(entry, (int, float)):
            continue
        out.append(int(entry))
    return out


class YieldTrail:
    """Follows ONE crop plot's harvest yield by identity (#1760).

    `arm` takes the baseline, `observe` takes one sample (a console
    round trip plus `ingest`), and `classify` says how far the yield
    got. `ingest` is deliberately separate from the console read so the
    classification oracle can be exercised deterministically without an
    engine (`tools/test_farm_ai_probe.py`).
    """

    def __init__(self, gx, gy, def_name=HARVEST_YIELD_DEF):
        self.gx = gx
        self.gy = gy
        self.def_name = def_name
        self.armed = False
        self.armed_gids = set()
        self.armed_instances = set()
        # Ids the unit AI itself recorded from world.harvestFlora's
        # return. Diagnostic only: a gid alone does not say WHICH plant
        # was picked, so it never carries the check on its own.
        self.loot_gids = []
        # gid -> the owning-page row that confirmed it at (gx, gy).
        self.ground = {}
        # Candidate ids the owning page answered for and does not have.
        self.retired = set()
        # instanceId -> {"poll": n, "linked": bool}
        self.carried = {}
        self.log = []
        self.last_phase = None
        self.last_action = None
        self.polls = 0

    # -- the console query --------------------------------------------

    def tracked(self):
        """Candidate gids worth re-resolving on the next sample."""
        return (set(self.loot_gids) | set(self.ground)) - self.retired

    def query(self, uid, known=()):
        """The single-line Lua this trail samples with.

        One round trip returns the unit AI's harvest phase/action, the
        gids it recorded from `world.harvestFlora`, the owning-page
        resolution of every candidate id, and the worker's carried
        instances of `def_name`.
        """
        known_list = ",".join(str(int(g)) for g in sorted(known))
        return (
            f"local ai=require('scripts.unit_ai'); "
            f"local s=ai.getState({uid}) or {{}}; "
            f"local o={{loot={{}},ground={{}},gone={{}},inv={{}}}}; "
            f"o.phase=s.harvestPhase or ''; o.action=s.currentAction or ''; "
            f"local seen={{}}; local cand={{}}; "
            f"local function add(g) if g and not seen[g] then "
            f"seen[g]=true; cand[#cand+1]=g end end; "
            f"for _,g in ipairs(s.harvestLoot or {{}}) do "
            f"o.loot[#o.loot+1]=g; add(g) end; "
            f"for _,g in ipairs({{{known_list}}}) do add(g) end; "
            # item.listGround() is ACTIVE-page scoped, so it only ever
            # nominates candidates; the owning-page query below is what
            # admits one.
            f"for _,r in ipairs(item.listGround() or {{}}) do "
            f"if r.defName=='{self.def_name}' "
            f"and math.floor(r.x)=={self.gx} and math.floor(r.y)=={self.gy} "
            f"then add(r.id) end end; "
            f"for _,g in ipairs(cand) do "
            f"local e,p=item.getGroundForUnit({uid},g); "
            f"if e and e.defName=='{self.def_name}' "
            f"and math.floor(e.x)=={self.gx} and math.floor(e.y)=={self.gy} "
            f"then o.ground[#o.ground+1]="
            f"{{gid=g,instanceId=e.instanceId,x=e.x,y=e.y}} "
            # nil WITH a resolved page means that page really has no
            # such id; nil with p==false determined nothing at all and
            # must never be read as a disappearance.
            f"elseif (not e) and p then o.gone[#o.gone+1]=g end end; "
            f"for _,r in ipairs(unit.getInventory({uid}) or {{}}) do "
            f"if r.defName=='{self.def_name}' "
            f"then o.inv[#o.inv+1]=r.instanceId end end; "
            f"return o")

    def _read(self, port, uid, known):
        got = send_json(port, self.query(uid, known))
        return got if isinstance(got, dict) else {}

    # -- sampling ------------------------------------------------------

    def arm(self, port, uid):
        """Baseline, taken BEFORE the time scale goes up."""
        return self.arm_from(self._read(port, uid, ()))

    def arm_from(self, obs):
        self.armed_gids = {int(row["gid"]) for row in _rows(obs.get("ground"))
                           if isinstance(row, dict)
                           and isinstance(row.get("gid"), (int, float))}
        self.armed_instances = set(_ids(obs.get("inv")))
        self.armed = True
        self.log.append(
            f"armed: ground gids {sorted(self.armed_gids)} already at "
            f"({self.gx},{self.gy}), carried {self.def_name} instanceIds "
            f"{sorted(self.armed_instances)}")
        return obs

    def observe(self, port, uid):
        return self.ingest(self._read(port, uid, tuple(sorted(self.tracked()))))

    def ingest(self, obs):
        """Fold one decoded sample into the trail."""
        if not isinstance(obs, dict):
            obs = {}
        self.polls += 1
        phase = obs.get("phase") or None
        action = obs.get("action") or None
        if phase != self.last_phase:
            self._note(f"unit_ai harvestPhase -> {phase!r}")
            self.last_phase = phase
        if action != self.last_action:
            self._note(f"unit_ai currentAction -> {action!r}")
            self.last_action = action
        for gid in _ids(obs.get("loot")):
            if gid in self.armed_gids or gid in self.loot_gids:
                continue
            self.loot_gids.append(gid)
            self._note(f"unit_ai recorded harvest yield gid {gid}")
        for row in _rows(obs.get("ground")):
            if not isinstance(row, dict):
                continue
            gid = row.get("gid")
            if not isinstance(gid, (int, float)) or isinstance(gid, bool):
                continue
            gid = int(gid)
            if gid in self.armed_gids:
                continue
            if gid not in self.ground:
                self._note(
                    f"gid {gid} confirmed on the ground at "
                    f"({row.get('x')},{row.get('y')}) as instanceId "
                    f"{row.get('instanceId')}")
            self.ground[gid] = row
            self.retired.discard(gid)
        for gid in _ids(obs.get("gone")):
            if gid in self.armed_gids or gid in self.retired:
                continue
            self.retired.add(gid)
            if gid in self.ground:
                self._note(f"gid {gid} left its owner's page")
        ground_instances = {row.get("instanceId") for row in self.ground.values()}
        for inst in _ids(obs.get("inv")):
            if inst in self.armed_instances or inst in self.carried:
                continue
            linked = inst in ground_instances
            self.carried[inst] = {"poll": self.polls, "linked": linked}
            self._note(
                f"instanceId {inst} carried by the worker"
                f"{' (the same instance seen on the ground)' if linked else ''}")
        return obs

    def _note(self, text):
        self.log.append(f"poll {self.polls}: {text}")

    # -- the oracle ----------------------------------------------------

    def gone(self):
        """Confirmed at the plot tile, then confirmed no longer there."""
        return {gid for gid in self.ground if gid in self.retired}

    def on_ground_now(self):
        return {gid: row for gid, row in self.ground.items()
                if gid not in self.retired}

    def classify(self):
        if self.carried:
            return YIELD_CARRIED
        if self.gone():
            return YIELD_MOVED
        if self.ground:
            return YIELD_ON_GROUND
        return YIELD_NOT_PRODUCED

    def produced(self):
        return self.classify() != YIELD_NOT_PRODUCED

    def report(self):
        """Where the yield was found -- printed on every run, pass or
        fail, as the acceptance evidence that the oracle no longer
        depends on one transient location."""
        where = []
        still = self.on_ground_now()
        if still:
            where.append(
                "on the ground as gid(s) "
                + ",".join(str(g) for g in sorted(still)))
        left = self.gone()
        if left:
            where.append(
                "left the ground as gid(s) "
                + ",".join(str(g) for g in sorted(left)))
        if self.carried:
            linked = sum(1 for v in self.carried.values() if v["linked"])
            where.append(
                "carried as instanceId(s) "
                + ",".join(str(i) for i in sorted(self.carried))
                + f" ({linked} of {len(self.carried)} matched to a ground "
                  f"observation)")
        if not where:
            where.append("nowhere -- never observed")
        return f"{self.classify()}: " + "; ".join(where)

    def diagnostics(self):
        """Everything a reader needs to tell "no harvest happened" from
        "the harvest happened and the yield moved" (#1760 requirement
        4)."""
        lines = [
            f"    yield trail for {self.def_name} at ({self.gx},{self.gy}) "
            f"over {self.polls} sample(s): {self.report()}",
            f"    worker's last unit_ai currentAction={self.last_action!r} "
            f"harvestPhase={self.last_phase!r}",
            f"    gids unit_ai recorded from world.harvestFlora: "
            f"{self.loot_gids or 'none'}",
            f"    owning-page rows confirmed at the plot tile: "
            f"{sorted(self.ground.values(), key=lambda r: r.get('gid', 0)) or 'none'}",
            f"    carried instances: {self.carried or 'none'}",
        ]
        lines += [f"      {entry}" for entry in self.log]
        return "\n".join(lines)


def get_skill(port, uid, name):
    v = send_json(port, f"return unit.getSkill({uid},'{name}') or -1")
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

    # This invocation owns its resource root and therefore its saves/
    # (#1616): the round trip at the end writes a slot that no ordinary
    # `cabal run` can reach, and the whole tree goes away below.
    base = tempfile.mkdtemp(prefix="synarchy_farm_ai_")

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
    clock = None
    try:
        root = make_isolated_root(base)
        # Unique per invocation as well as per root, so the slot NAME alone
        # identifies this run even in a log shared with another.
        slot = f"farm_ai_v79_check_{uuid.uuid4().hex[:8]}"
        print(f"isolated resource root: {root}", flush=True)
        print(f"save slot: {slot}", flush=True)

        clock = PhaseClock()
        prepared = os.environ.get(probe_engine.ENV_ENGINE_EXE, "").strip()
        print("  boot phase "
              + ("excludes the engine build (runner-prepared "
                 f"${probe_engine.ENV_ENGINE_EXE})" if prepared else
                 "INCLUDES any engine build preparation (no prebuilt "
                 "executable supplied; a warm build is a no-op)"),
              flush=True)
        proc = boot(port, f"{SPROOT}/farm_ai_probe_engine.log",
                    args=["--resource-root", root])
        bootstrap(port)
        clock.mark("boot_and_content")
        send(port, f"world.init('probe', {args.seed}, {args.size}, "
                   f"{args.plates}); return 'ok'")
        send(port, "return world.waitForInit(300)", timeout=310)
        send(port, "world.show('probe'); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)
        clock.mark("world_init_and_chunks")

        # Six fixed sites on two authored plateaus (#2938): A (groundcover,
        # full till+plant+harvest AI loop), B (row crop, AI-planted) and E
        # (mid-job re-designation) near the origin; C (direct-primitive
        # checks: refusal + a single real plant), D (the rot timeline) and
        # F (category field) far enough away (>24 tiles, past
        # harvest_scan_range) that the acolyte working A/B/E can never
        # stumble onto them and confound the "ignored crop rots" check. A
        # planted tile refuses a second planting, so the rot check needs
        # its OWN fresh tile rather than replanting C.
        built, why = build_fixture(port)
        if not built:
            print(f"  [FAIL] fixture setup failed: {why}")
            return 1
        clock.mark("fixture")
        tA, tB, tE = (SITES[k] for k in NEAR_SITES)
        tC, tD, tF = (SITES[k] for k in FAR_SITES)
        ax, ay = tA
        bx, by = tB
        cx, cy = tC
        dx, dy = tD
        fx, fy = tF
        print(f"  site A={tA} (groundcover) B={tB} (row, AI) C={tC} (row, direct) "
              f"D={tD} (row, rot) F={tF} (category field) E={tE} (re-designation)")

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

        cz = send_json(port, f"local sz=world.getSurfaceAt({cx},{cy}); return sz")
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
        dz = send_json(port, f"local sz=world.getSurfaceAt({dx},{dy}); return sz")
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
        d4c = send_json(port, f"return plant.getDesignationAt('probe',{dx},{dy})")
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
        cross_plant = send_json(port, f"return world.plantCropAt({cx},{cy},'wheat')")
        cross_plot = send_json(port, f"return world.getCropPlotAt({cx},{cy})")
        ok3c = cross_plant in (None, False) and cross_plot is None
        passed &= ok3c
        print(f"  [{'PASS' if ok3c else 'FAIL'}] plantCropAt refuses a tile "
              f"already carrying a row-crop instance: planted={cross_plant} "
              f"plot={cross_plot}")

        # --- 5. plant.getDesignationAt's category field (its own fresh,
        #     unoccupied tile F — C is already planted by this point,
        #     and an occupied tile now refuses designation, check 4b). ---
        fz = send_json(port, f"local sz=world.getSurfaceAt({fx},{fy}); return sz")
        till_and_wait(port, "probe", fx, fy, fz)
        send(port, f"plant.designate('probe',{fx},{fy},'tomato_plant'); "
                   f"return 'ok'")
        time.sleep(0.5)
        dcat_row = send_json(port, f"return plant.getDesignationAt('probe',{fx},{fy})")
        send(port, f"plant.designate('probe',{fx},{fy},'wheat'); return 'ok'")
        time.sleep(0.5)
        dcat_ground = send_json(port, f"return plant.getDesignationAt('probe',{fx},{fy})")
        ok5 = (isinstance(dcat_row, dict) and dcat_row.get("category") == "row_crop"
               and isinstance(dcat_ground, dict)
               and dcat_ground.get("category") == "groundcover_crop")
        passed &= ok5
        print(f"  [{'PASS' if ok5 else 'FAIL'}] getDesignationAt reports "
              f"category: row={dcat_row} ground={dcat_ground}")
        send(port, f"plant.cancelDesignation({fx},{fy}); return 'ok'")

        clock.mark("direct_primitive_checks")

        # --- 6/7/8. Full AI loop: till (A) -> plant (A, wheat) ->
        #     plant (B, tomato_plant, pre-tilled) ---
        send(port, "engine.loadScript('scripts/unit_stats.lua', 0.1); "
                   "return 'ok'")
        send(port, "engine.loadScript('scripts/unit_resources.lua', 0.2); "
                   "return 'ok'")
        send(port, "engine.loadScript('scripts/unit_ai.lua', 0.1); "
                   "return 'ok'")
        bz = send_json(port, f"local sz=world.getSurfaceAt({bx},{by}); return sz")
        till_and_wait(port, "probe", bx, by, bz)

        uid = spawn_worker(port, ax + 2, ay)
        if uid < 0:
            print(f"  [FAIL] could not spawn farm worker")
            return 1
        farming_before = get_skill(port, uid, "farming")

        send(port, f"till.designate('probe',{ax},{ay},{ax},{ay}); return 'ok'")
        send(port, f"plant.designate('probe',{bx},{by},'tomato_plant'); "
                   f"return 'ok'")

        clock.mark("ai_setup")
        deadline = time.time() + 90.0
        tilled = False
        while time.time() < deadline:
            time.sleep(2.0)
            if send_json(port, f"return world.isPlantable({ax},{ay})") is True:
                tilled = True
                break
        clock.mark("ai_till_wait")
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
        b_iid = None
        while time.time() < deadline:
            time.sleep(2.0)
            if not planted_a:
                pa = send_json(port, f"return world.getCropPlotAt({ax},{ay})")
                if isinstance(pa, dict) and pa.get("id") == "wheat":
                    planted_a = True
            if not planted_b:
                eb = growth_entries(port, bx, by, "tomato_plant")
                if eb:
                    planted_b = True
                    b_iid = eb[0].get("instanceId")
            if planted_a and planted_b:
                break
        clock.mark("ai_plant_wait")
        ok6 = planted_a
        passed &= ok6
        print(f"  [{'PASS' if ok6 else 'FAIL'}] acolyte plants wheat at site A "
              f"(world.plantCropAt): planted={planted_a}")
        ok7 = planted_b
        passed &= ok7
        print(f"  [{'PASS' if ok7 else 'FAIL'}] acolyte plants tomato_plant at "
              f"site B (world.plantRowCropAt): planted={planted_b}")

        da = send_json(port, f"return plant.getDesignationAt('probe',{ax},{ay})")
        db = send_json(port, f"return plant.getDesignationAt('probe',{bx},{by})")
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

        # Arm the yield trail BEFORE the clock jumps (#1760). Everything
        # after this point is watched, so a harvest completed inside the
        # very first polling interval is still recorded, and nothing
        # lying about beforehand can be credited to it.
        trail = YieldTrail(ax, ay)
        trail.arm(port, uid)

        send(port, "world.setTimeScale('probe', 50000); return 'ok'")
        deadline = time.time() + 15.0
        ripe_wheat = False
        cleared_while_ripening = False
        plot_sample = send_json(port, f"return world.getCropPlotAt({ax},{ay})")
        while time.time() < deadline:
            time.sleep(1.0)
            trail.observe(port, uid)
            pw = send_json(port, f"return world.getCropPlotAt({ax},{ay})")
            plot_sample = pw
            if isinstance(pw, dict) and pw.get("harvestable") is True:
                ripe_wheat = True
                break
            if pw is None:
                # world.setTimeScale is game-MINUTES per real second, so
                # scale 50,000 buys roughly 34.7 game days per one-second
                # poll: the acolyte can find the plot and harvest it
                # inside a single sample. That is the outcome this phase
                # exists to produce, not a missing ripe state — resolved
                # below against the yield trail and the phase-local
                # farming-XP delta, never against a bare nil crop sample.
                cleared_while_ripening = True
                break
        send(port, "world.setTimeScale('probe', 1); return 'ok'")

        send(port, f"unit.moveTo({uid}, {ax + 0.5}, {ay + 1.5}, 1.0); return 'ok'")
        deadline = time.time() + 20.0
        while time.time() < deadline:
            time.sleep(1.0)
            trail.observe(port, uid)
            info = send_json(port, f"return unit.getInfo({uid})")
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
        # regrowing wild bush. Sample the trail immediately after the
        # sweep and immediately before the plot read, so the three
        # observations describe the same moment.
        deadline = time.time() + 60.0
        harvested = cleared_while_ripening
        while not harvested and time.time() < deadline:
            time.sleep(0.5)
            clear_wild_forage(port, ax, ay, radius=60, keep=(ax, ay))
            trail.observe(port, uid)
            pw2 = send_json(port, f"return world.getCropPlotAt({ax},{ay})")
            plot_sample = pw2
            if pw2 is None:
                harvested = True
                break

        # The pick and the collection are two phases of ONE action
        # (scripts/unit_ai_harvest.lua): the plot clears in the first,
        # and the second pulls the yield off the ground one item per
        # tick. Keep watching PAST the clear so the trail records that
        # transition rather than sampling one instant of it. Nothing is
        # driven here — no sweep, no time scale, no command — so the
        # scenario under test is unchanged.
        settle = time.time() + 15.0
        while time.time() < settle:
            trail.observe(port, uid)
            if trail.classify() == YIELD_CARRIED:
                break
            time.sleep(0.5)

        clock.mark("auto_harvest")
        farming_after_harvest = get_skill(port, uid, "farming")
        xp_grew = farming_after_harvest > farming_after_plant

        # Ripeness: a sampled harvestable state, OR the AI having
        # already harvested the plot — the same precondition reached by
        # its own effect. The fallback needs causally specific evidence
        # that THIS plot was harvested (a recorded yield, or the plot
        # clear plus the phase-local farming-XP delta), so a plot that
        # merely vanished never satisfies it.
        ok9a = ripe_wheat or (harvested and (trail.produced() or xp_grew))
        passed &= ok9a
        print(f"  [{'PASS' if ok9a else 'FAIL'}] wheat reaches the harvestable "
              f"state: sampled_ripe={ripe_wheat} plot_cleared={harvested} "
              f"yield={trail.classify()} farming_xp_grew={xp_grew}")

        # The yield is judged by identity, not by where it happens to be
        # at one instant: picked up, carried or eaten all still count,
        # and only a yield that was never produced fails (#1760).
        ok9 = harvested and trail.produced()
        passed &= ok9
        print(f"  [{'PASS' if ok9 else 'FAIL'}] acolyte auto-harvests the ripe "
              f"wheat: plot_cleared={harvested} yield found {trail.report()}")
        if not (ok9a and ok9):
            print(f"    last crop-plot sample at ({ax},{ay}): {plot_sample!r}")
            print(trail.diagnostics())

        ok9b = xp_grew
        passed &= ok9b
        print(f"  [{'PASS' if ok9b else 'FAIL'}] farming skill grows further "
              f"from auto-harvest: {farming_after_plant} -> "
              f"{farming_after_harvest}")

        # --- 10. Re-designating a claimed tile with a DIFFERENT crop
        #     must not plant the stale crop the unit originally claimed.
        #     plant.designate replaces in place (HM.insert), so a player
        #     can swap the crop mid-job; the AI must notice and plant
        #     whatever's there NOW, not what it walked over for. ---
        ex, ey = tE
        ez = send_json(port, f"local sz=world.getSurfaceAt({ex},{ey}); return sz")
        till_and_wait(port, "probe", ex, ey, ez)
        send(port, f"plant.designate('probe',{ex},{ey},'wheat'); return 'ok'")

        # Hold the worker, do not race it (#2937). Three things must be true
        # when the crop is swapped, and each is READ, none assumed: the worker
        # holds the wheat job, that job is unfinished (nothing planted on E,
        # progress below 1), and the tomato designation has been COMMITTED by
        # the world thread (plant.designate only queues it, so a single read
        # right after the call can still see wheat). Pausing the simulation
        # the moment the matching claim is seen freezes the worker's AI and
        # clock while the world thread still services commands, so the state
        # read after the pause is the state the swap lands in.
        plant_job_lua = (f"local ai=require('scripts.unit_ai'); "
                         f"local s=ai.getState({uid}); "
                         f"if s and s.plantJob then return s.plantJob.x..','.."
                         f"s.plantJob.y..','..tostring(s.plantJob.crop)..','.."
                         f"tostring(s.plantProgress or 0) "
                         f"else return 'none' end")
        deadline = time.time() + 30.0
        job = "none"
        while time.time() < deadline:
            time.sleep(0.2)
            job = send(port, plant_job_lua).strip('"')
            if job.startswith(f"{ex},{ey},wheat,"):
                break
        else:
            print(f"  [FAIL] acolyte never claimed the re-designation test "
                  f"wheat job (last plantJob={job!r})")
            return 1
        clock.mark("redesignation_setup_and_claim")
        send(port, "engine.setPaused(true); return 'ok'")
        try:
            held = send(port, plant_job_lua).strip('"')
            held_plot = send_json(port, f"return world.getCropPlotAt({ex},{ey})")
            held_flora, held_fail = growth_read(port, ex, ey, "tomato_plant")
            try:
                held_progress = float(held.split(",")[3])
            except (IndexError, ValueError):
                held_progress = None
            if not (held.startswith(f"{ex},{ey},wheat,")
                    and held_progress is not None and held_progress < 1.0
                    and held_plot is None and not held_flora
                    and held_fail is None):
                print(f"  [FAIL] re-designation test is invalid: the wheat "
                      f"job was not held and unfinished when the swap was "
                      f"made (plantJob={held!r} plot={held_plot!r} "
                      f"flora={held_flora} read_failure={held_fail!r})")
                return 1
            send(port, f"plant.designate('probe',{ex},{ey},'tomato_plant'); "
                       f"return 'ok'")
            swapped = None
            commit_deadline = time.time() + 10.0
            while time.time() < commit_deadline:
                swapped = send_json(
                    port, f"return plant.getDesignationAt('probe',{ex},{ey})")
                if isinstance(swapped, dict) \
                   and swapped.get("crop") == "tomato_plant":
                    break
                time.sleep(0.2)
            after_swap = send(port, plant_job_lua).strip('"')
            swap_plot = send_json(port, f"return world.getCropPlotAt({ex},{ey})")
            swap_flora, swap_fail = growth_read(port, ex, ey, "tomato_plant")
        finally:
            send(port, "engine.setPaused(false); return 'ok'")
        ok10a = (isinstance(swapped, dict)
                 and swapped.get("crop") == "tomato_plant"
                 and after_swap.startswith(f"{ex},{ey},wheat,")
                 and swap_plot is None and not swap_flora
                 and swap_fail is None)
        passed &= ok10a
        print(f"  [{'PASS' if ok10a else 'FAIL'}] the tomato re-designation "
              f"was committed while the worker held the unfinished wheat "
              f"job: designation={swapped} plantJob={after_swap!r} "
              f"plot={swap_plot!r} flora={swap_flora} "
              f"read_failure={swap_fail!r}")

        # Completion is the tomato INSTANCE itself, never the designation
        # vanishing: the designation clears the moment the plant command is
        # queued, and a vanished one also reads the same when a query fails.
        deadline = time.time() + 90.0
        planted_new = []
        new_fail = None
        while time.time() < deadline:
            time.sleep(1.0)
            planted_new, new_fail = growth_read(port, ex, ey, "tomato_plant")
            if planted_new:
                break
        clock.mark("redesignation_swap_and_plant")
        time.sleep(1.0)
        de = send_json(port, f"return plant.getDesignationAt('probe',{ex},{ey})")
        planted_stale = send_json(port, f"return world.getCropPlotAt({ex},{ey})")
        ok10 = (len(planted_new) >= 1 and planted_stale is None
                and not isinstance(de, dict))
        passed &= ok10
        print(f"  [{'PASS' if ok10 else 'FAIL'}] re-designating a claimed tile "
              f"plants the NEW crop, not the stale one: "
              f"new={planted_new} stale_plot={planted_stale} "
              f"designation={de} read_failure={new_fail!r}")

        # --- 11. Save/load: the AI-planted row crops (WePlaceFlora, v79)
        #     survive save -> loadSave, by identity. Two AI plantings are
        #     followed: site B (planted early) and site E (planted by the
        #     re-designation check just above). ---
        e_iid = planted_new[0].get("instanceId") if planted_new else None
        # Freeze the simulation so nothing — the calendar, the worker's AI,
        # a harvest — can change the plants between the pre-save read and
        # the capture (#2937). A load leaves the session paused again.
        send(port, "engine.setPaused(true); return 'ok'")
        followed = {"B": (bx, by, b_iid), "E": (ex, ey, e_iid)}
        for tag, (fx, fy, fiid) in followed.items():
            now, fail = growth_read(port, fx, fy, "tomato_plant")
            alive = fiid is not None and any(
                e.get("instanceId") == fiid for e in now)
            ok11a = alive and fail is None
            passed &= ok11a
            print(f"  [{'PASS' if ok11a else 'FAIL'}] AI-planted row crop at "
                  f"site {tag} still exists right before the save "
                  f"(instanceId {fiid}): {now} read_failure={fail!r}")

        clock.mark("pre_save_reads")
        failure = save_and_reload(port, "probe", slot, mark=clock.mark)
        if failure:
            print(f"  [FAIL] {failure}")
            return 1
        send(port, "world.show('probe'); return 'ok'")
        # Resume BEFORE observing, as a player's load does: a chunk's flora
        # is reconciled against the calendar when the world thread admits it,
        # and that pass is what #2937 found killing every planted row crop.
        # A read taken while the load left the session paused runs before it
        # and sees a plant that is about to die.
        send(port, "engine.setPaused(false); return 'ok'")
        send(port, "return world.loadChunksInRegion(-4, -4, 4, 4)", timeout=30)
        send(port, "return world.waitForChunks(120)", timeout=125)
        time.sleep(3.0)
        clock.mark("reload_chunks_and_settle")
        for tag, (fx, fy, fiid) in followed.items():
            after, fail = growth_read(port, fx, fy, "tomato_plant")
            ok11 = fail is None and any(
                e.get("instanceId") == fiid for e in after)
            passed &= ok11
            print(f"  [{'PASS' if ok11 else 'FAIL'}] AI-planted row crop at "
                  f"site {tag} survives save/load as the same instance "
                  f"({fiid}): {after} read_failure={fail!r}")

        print("\n" + ("ALL FARM AI CHECKS PASSED" if passed else "SOME FAILED"))
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
        if clock is not None:
            print(clock.table(), flush=True)
        if proc is not None:
            quit_engine(port, proc)
        cleaned = remove_run_root(base)
    return rc if cleaned else 1


if __name__ == "__main__":
    sys.exit(main())
